;;; call-graph.el --- Library to generate call graph for cpp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Version: 0.0.2
;; Keywords: programming, convenience
;; Created: 2018-01-07
;; Package-Requires: ((emacs "25.1") (hierarchy "0.7.0") (tree-mode "1.0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Library to generate call graph for cpp functions.

;;; Install:

;; Put this file into load-path directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'call-graph)
;;     (global-set-key (kbd "C-c g") 'call-graph)
;;
;;; Usage:

;; "C-c g" => (call-graph) => buffer <*call-graph*> will be generated

;;; Code:

(require 'hierarchy)
(require 'tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup call-graph nil
  "Customization support for the `call-graph'."
  :version "0.0.2"
  :group 'applications)

(defcustom call-graph-max-depth 2
  "The maximum depth of call graph."
  :type 'integer
  :group 'call-graph)

(defconst call-graph-key-to-depth "*current-depth*"
  "The key to get current depth of call graph.")

(defconst call-graph-key-to-caller-location "*caller-location*"
  "The key to get caller location.")

;; use hash-table as the building blocks for tree
(defun call-graph--make-node ()
  "Serve as tree node."
  (make-hash-table :test 'equal))

(defvar call-graph-internal-cache (call-graph--make-node)
  "The internal cache of call graph.")

(defcustom call-graph-termination-list '("main")
  "Call-graph stops when seeing symbols from this list."
  :type 'list
  :group 'call-graph)

(defcustom call-graph-unique-buffer t
  "Non-nil means only one buffer will be used for `call-graph'."
  :type 'boolean
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; simplicistic queue implementation
;;; thanks to wasamasa, @see http://ix.io/DoE

;; (defun list-to-queue (list)
;;   (cons list (if (consp list) (last list) '())))

;; (defun queue-to-list (queue)
;;   (car queue))

(defun call-graph--make-queue ()
  (cons '() '()))

(defun call-graph--queue-empty-p (queue)
  (null (car queue)))

(defun call-graph--queue-get (queue)
  (if (null (car queue))
      (error "Queue is empty")
    (let ((x (caar queue)))
      (setcar queue (cdar queue))
      (if (null (car queue)) (setcdr queue '()))
      x)))

(defun call-graph--queue-put (queue x)
  (let ((entry (cons x '())))
    (if (null (car queue))
        (setcar queue entry)
      (setcdr (cdr queue) entry))
    (setcdr queue entry)
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--get-buffer ()
  "Generate ‘*call-graph*’ buffer."
  (let ((buffer-name "*call-graph*"))
    (if call-graph-unique-buffer
        (get-buffer-create buffer-name)
      (generate-new-buffer buffer-name))))

(defun call-graph--find-caller (reference)
  "Given a REFERENCE, return the caller of this reference."
  (when-let ((tmpVal (split-string reference ":"))
             (fileName (seq-elt tmpVal 0))
             (lineNbStr (seq-elt tmpVal 1))
             (lineNb (string-to-number lineNbStr))
             (is-valid-file (file-exists-p fileName))
             (is-valid-Nb (integerp lineNb)))
    (let ((location (concat fileName ":" lineNbStr))
          caller)
      (with-temp-buffer
        (insert-file-contents-literally fileName)
        ;; TODO: leave only hooks on which 'which-function-mode depends
        ;; (set (make-local-variable 'c++-mode-hook) nil)
        (c++-mode)
        (which-function-mode t)
        (forward-line lineNb)
        (setq caller (which-function)))
      (setq tmpVal (split-string caller "::"))
      (if (> (seq-length tmpVal) 1)
          (cons (seq-elt tmpVal 1) location)
        (cons (seq-elt tmpVal 0) location)))))

(defun call-graph--find-references (function)
  "Given a FUNCTION, return all references of this function."
  (let* ((command
          (concat (format "global -a --result=grep -r %s" function) " | grep -E \"\\.(cpp|cc):\""))
         (command-out-put (shell-command-to-string command)))
    (split-string command-out-put "\n" t)))

(defun call-graph--walk-tree-in-bfs-order (item node func)
  "Wallk tree in BFS order, apply FUNC for each (item . node).
ITEM is parent of NODE, NODE should be a hash-table."
  (let ((queue (call-graph--make-queue))
        queue-elt current-item current-node)
    (call-graph--queue-put queue (cons item node))
    (while (not (call-graph--queue-empty-p queue))
      (setq queue-elt (call-graph--queue-get queue)
            current-item (car queue-elt)
            current-node (cdr queue-elt))
      (funcall func current-item current-node)
      (seq-doseq (map-pair (map-pairs current-node))
        (when (hash-table-p (cdr map-pair))
          (call-graph--queue-put queue map-pair))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--create (item root)
  "Construct `call-graph' tree.
ITEM is parent of root, ROOT should be a hash-table."
  (when (and item root)
    (let ((caller-visited call-graph-termination-list))
      (push (symbol-name item) caller-visited)
      (map-put root call-graph-key-to-depth 0)
      ;; (map-put root call-graph-key-to-caller-location location)
      (map-put call-graph-internal-cache (symbol-name item) root)
      (catch 'exceed-max-depth
        (call-graph--walk-tree-in-bfs-order
         item root
         (lambda (parent node)
           (when (hash-table-p node)
             (let ((depth (map-elt node call-graph-key-to-depth 0))
                   caller location sub-node)
               (when (> depth call-graph-max-depth) (throw 'exceed-max-depth t))
               (when (hash-table-p node)
                 (seq-doseq (reference (call-graph--find-references parent))
                   (unless (member (setq caller (call-graph--find-caller reference)
                                         location (cdr caller)
                                         caller (car caller)) caller-visited)
                     (message caller)
                     (push caller caller-visited)
                     (setq sub-node (call-graph--make-node))
                     (map-put sub-node call-graph-key-to-depth (1+ depth))
                     (map-put sub-node call-graph-key-to-caller-location location)
                     ;; save to cache for fast data retrival
                     (map-put call-graph-internal-cache caller sub-node)
                     (map-put node (intern caller) sub-node))))))))))))

(defun call-graph--display (item root)
  "Prepare data for display.
ITEM is parent of root, ROOT should be a hash-table."
  (let ((first-time t) (log (list))
        (hierarchy (hierarchy-new)))
    (call-graph--walk-tree-in-bfs-order
     item root
     (lambda (parent node)
       (when (hash-table-p node)
         (seq-doseq (child (map-keys node))
           (when first-time (setq first-time nil)
                 (hierarchy--add-relation hierarchy parent nil 'identity))
           (unless (member child (list call-graph-key-to-depth call-graph-key-to-caller-location))
             (hierarchy--add-relation hierarchy child parent 'identity)
             (push
              (concat "insert childe " (symbol-name child)
                      " under parent " (symbol-name parent)) log))))))
    (call-graph--hierarchy-display hierarchy)
    (seq-doseq (rec (reverse log)) (message rec))))

(defun call-graph--hierarchy-display (hierarchy)
  "Display call graph with HIERARCHY."
  (switch-to-buffer-other-window
   (hierarchy-tree-display
    hierarchy
    (lambda (tree-item _)
      (let* ((caller (symbol-name tree-item))
             (location (map-elt (map-elt call-graph-internal-cache caller)
                                call-graph-key-to-caller-location)))
        ;; use propertize to avoid this error => Attempt to modify read-only object
        ;; @see https://stackoverflow.com/questions/24565068/emacs-text-is-read-only
        (insert (propertize caller 'caller-location location))))
    (call-graph--get-buffer)))
  (call-graph-mode)
  (call-graph-widget-expand-all))

(defun call-graph ()
  "Generate a function `call-graph' for the function at point."
  (interactive)
  (save-excursion
    (when-let ((target (symbol-at-point))
               (root (call-graph--make-node)))
      (call-graph--create target root)
      (call-graph--display target root))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-widget-expand-all ()
  "Iterate all widgets in buffer and expand em."
  (interactive)
  (tree-mode-expand-level 0))

(defun call-graph-widget-collapse-all (&optional level)
  "Iterate all widgets in buffer and close em at LEVEL."
  (interactive)
  (goto-char (point-min))
  (tree-mode-expand-level (or level 1)))

(defun call-graph-forward (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (widget-forward arg)
  (save-excursion
    (forward-char 4)
    (call-graph-display-file-at-point)))

(defun call-graph-backward (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (widget-backward arg)
  (save-excursion
    (forward-char 4)
    (call-graph-display-file-at-point)))

(defun call-graph-display-file-at-point ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (save-selected-window
    (call-graph-visit-file-at-point)))

(defun call-graph-visit-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (when-let ((location (get-text-property (point) 'caller-location))
             (tmpVal (split-string location ":"))
             (fileName (seq-elt tmpVal 0))
             (lineNbStr (seq-elt tmpVal 1))
             (lineNb (string-to-number lineNbStr))
             (is-valid-file (file-exists-p fileName))
             (is-valid-Nb (integerp lineNb)))
    (find-file-read-only-other-window fileName)
    (with-no-warnings (goto-line lineNb))))

(defun call-graph-goto-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (if (get-char-property (point) 'button)
      (save-excursion
        (forward-char 4)
        (call-graph-visit-file-at-point))
    (call-graph-visit-file-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "e") 'call-graph-widget-expand-all)
    (define-key map (kbd "c") 'call-graph-widget-collapse-all)
    (define-key map (kbd "p") 'call-graph-backward)
    (define-key map (kbd "n") 'call-graph-forward)
    (define-key map (kbd "q") 'kill-this-buffer)
    (define-key map (kbd "o") 'call-graph-goto-file-at-point)
    (define-key map (kbd "g")  nil) ; nothing to revert
    (define-key map (kbd "<RET>") 'call-graph-visit-file-at-point)
    map)
  "Keymap for `call-graph' major mode.")

;;;###autoload
(define-derived-mode call-graph-mode special-mode "call-graph"
  "Major mode for viewing function's `call graph'.

\\{call-graph-mode-map}"
  :group 'call-graph
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil
        list-buffers-directory (abbreviate-file-name default-directory)
        mode-name "call-graph")
  (setq-local line-move-visual t)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (run-mode-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "C-c g") 'call-graph)


(provide 'call-graph)
;;; call-graph.el ends here
