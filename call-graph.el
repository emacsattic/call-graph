;;; call-graph.el --- Library to generate call graph for cpp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Version: 0.0.4
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
  :version "0.0.4"
  :group 'applications)

(defcustom call-graph-initial-max-depth 2
  "The maximum initial depth of call graph."
  :type 'integer
  :group 'call-graph)

(defcustom call-graph-filters nil
  "The filters used by `call-graph' when searching caller."
  :type 'list
  :group 'call-graph)

(defcustom call-graph-display-file t
  "Non-nil means display file in another window while moving from one field to another in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defvar call-graph--current-depth 0
  "The current depth of call graph.")

(defvar call-graph--switch-window-p t
  "Non-nil means switch to `call-graph' window.")

;; use hash-table as the building blocks for tree
(defun call-graph--make-node ()
  "Serve as tree node."
  (make-hash-table :test 'equal))

;; Refactor with cl-defstruct later on.
(defvar call-graph--internal-cache (call-graph--make-node)
  "The internal cache of call graph.")

(defvar call-graph--location-cache (call-graph--make-node)
  "The caller locations map.")

(defvar call-graph--hierarchy (hierarchy-new)
  "The hierarchy used by call-graph.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--get-buffer ()
  "Generate ‘*call-graph*’ buffer."
  (let ((buffer-name "*call-graph*"))
    (get-buffer-create buffer-name)))

(defun call-graph--find-caller (reference)
  "Given a REFERENCE, return the caller as (caller . location)."
  (when-let ((tmp-val (split-string reference ":"))
             (file-name (seq-elt tmp-val 0))
             (line-nb-str (seq-elt tmp-val 1))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (let ((location (concat file-name ":" line-nb-str))
          caller)
      (with-temp-buffer
        (insert-file-contents-literally file-name)
        ;; TODO: leave only hooks on which 'which-function-mode depends
        ;; (set (make-local-variable 'c++-mode-hook) nil)
        (c++-mode)
        (which-function-mode t)
        (forward-line line-nb)
        (setq caller (which-function)))
      (when (and caller (setq tmp-val (split-string caller "::")))
        (if (> (seq-length tmp-val) 1)
            (cons (intern (seq-elt tmp-val 1)) location)
          (cons (intern (seq-elt tmp-val 0)) location))))))

(defun call-graph--find-references (func)
  "Given a FUNC, return all references as a list."
  (let ((command
         (format "global -a --result=grep -r %s | grep -E \"\\.(cpp|cc):\""
                 (shell-quote-argument (symbol-name func))))
        (filter-separator " | ")
        command-filter command-out-put)
    (when (and (> (length call-graph-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil call-graph-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-out-put (shell-command-to-string command))
      (split-string command-out-put "\n" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--hierarchy-display (hierarchy)
  "Display call graph in HIERARCHY."
  (let (hierarchy-buffer)
    (setq hierarchy-buffer
          (hierarchy-tree-display
           hierarchy
           (lambda (tree-item _)
             (let* ((caller (symbol-name tree-item))
                    (location (map-elt call-graph--location-cache tree-item)))
               ;; use propertize to avoid this error => Attempt to modify read-only object
               ;; @see https://stackoverflow.com/questions/24565068/emacs-text-is-read-only
               (insert (propertize caller 'caller-location location))))
           (call-graph--get-buffer)))
    (when call-graph--switch-window-p
      (switch-to-buffer-other-window hierarchy-buffer))
    (call-graph-mode)
    (call-graph-widget-expand-all)))

(defun call-graph--display-caller-map (hierarchy func caller-map &optional actual-depth)
  "Given HIERARCHY and FUNC, display its CALLER-MAP, calculate ACTUAL-DEPTH."
  (when (and hierarchy func caller-map)
    (when-let ((actual-depth (or actual-depth 1))
               (next-depth (1+ actual-depth))
               (callers (map-keys caller-map))
               (is-not-empty (not (map-empty-p callers))))

      ;; populate hierarchy data.
      (seq-doseq (caller callers)
        (hierarchy-add-tree hierarchy caller (lambda (item) (when (eq item caller) func)))
        (message "insert child %s under parent %s" (symbol-name caller) (symbol-name func)))

      ;; calculate the actual depth.
      (when (> actual-depth call-graph--current-depth)
        (setq call-graph--current-depth actual-depth))

      ;; recursively populate callers.
      (seq-doseq (caller callers)
        (call-graph--display-caller-map hierarchy caller
                                        (map-elt call-graph--internal-cache caller) next-depth)))))

(defun call-graph--display (hierarchy item caller-map)
  "Prepare data and display `call-graph' in HIERARCHY.
ITEM is the root, CALLER-MAP should be a hash-table."
  (call-graph--display-caller-map hierarchy item caller-map)
  (call-graph--hierarchy-display hierarchy))

(defun call-graph--find-caller-map (func depth)
  "Given a FUNC, return its caller-map.
DEPTH is the depth of caller-map, SEEN-CALLERS prevent infinite loop."
  (when-let ((next-depth (and (> depth 0) (1- depth))))
    (let ((caller-map (map-elt call-graph--internal-cache func)))

      ;; search in internal-cache.
      (unless caller-map
        (setq caller-map (call-graph--make-node))
        (map-put call-graph--internal-cache func caller-map))

      ;; callers not found.
      (when (map-empty-p (map-keys caller-map))
        (seq-doseq (reference (call-graph--find-references func))
          (when-let ((caller-pair
                      (and reference (call-graph--find-caller reference)))
                     (caller (car caller-pair))
                     (location (cdr caller-pair)))
            (message (format "Search returns: %s" (symbol-name caller)))

            ;; prevent infinite loop
            (unless (map-elt call-graph--internal-cache caller)
              (let ((sub-caller-map (call-graph--make-node)))
                (map-put caller-map caller sub-caller-map)
                (map-put call-graph--internal-cache caller sub-caller-map)
                (map-put call-graph--location-cache caller location))))))

      ;; recursively find callers.
      (seq-doseq (caller (map-keys caller-map))
        (call-graph--find-caller-map caller next-depth))

      ;; return top-level caller-map.
      caller-map)))

(defun call-graph--create (func depth)
  "Generate `call-graph' for FUNC.
DEPTH is the depth of caller-map."
  (when-let ((hierarchy (hierarchy-new))
             (caller-map
              (and func depth (call-graph--find-caller-map func depth))))
    (setq call-graph--current-depth 0)
    (call-graph--display hierarchy func caller-map)
    (setq call-graph--hierarchy hierarchy)))

;;;###autoload
(defun call-graph (&optional depth)
  "Generate `call-graph' for function at point.
DEPTH is the depth of caller-map."
  (interactive)
  (save-excursion
    (when-let ((func (symbol-at-point))
               (depth (or depth call-graph-initial-max-depth)))
      (setq call-graph--internal-cache nil)
      (if (eq major-mode 'call-graph-mode)
          (setq call-graph--switch-window-p nil)
        (setq call-graph--switch-window-p t))
      (if (> depth call-graph-initial-max-depth)
          (call-graph--create func depth)
        (call-graph--create func call-graph-initial-max-depth)))))

(defun call-graph-expand (&optional level)
  "Expand `call-graph' by LEVEL."
  (interactive "p")
  (when-let ((depth (+ call-graph--current-depth level))
             (func (and call-graph--hierarchy
                        (car (hierarchy-roots call-graph--hierarchy)))))
    (setq call-graph--switch-window-p nil)
    (call-graph--create func depth)))

(defun call-graph-collapse (&optional level)
  "Collapse `call-graph' by LEVEL."
  (interactive "p")
  (let ((level (- call-graph--current-depth level)))
    (goto-char (point-min))
    (cond
     ((>= level call-graph--current-depth)
      (tree-mode-expand-level call-graph--current-depth))
     ((> level 0)
      (tree-mode-expand-level level)
      (setq call-graph--current-depth level))
     ((<= level 0)
      (tree-mode-expand-level 1)
      (setq call-graph--current-depth 1)))))

(defun call-graph-quit ()
  "Quit `call-graph'."
  (interactive)
  (kill-this-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-widget-expand-all ()
  "Iterate all widgets in buffer and expand em."
  (interactive)
  (tree-mode-expand-level 0))

(defun call-graph-widget-collapse-all ()
  "Iterate all widgets in buffer and close em."
  (interactive)
  (goto-char (point-min))
  (tree-mode-expand-level 1))

(defun call-graph-visit-file-at-point ()
  "Visit occurrence on the current line."
  (when-let ((location (get-text-property (point) 'caller-location))
             (tmp-val (split-string location ":"))
             (file-name (seq-elt tmp-val 0))
             (line-nb-str (seq-elt tmp-val 1))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (find-file-read-only-other-window file-name)
    (with-no-warnings (goto-line line-nb))))

(defun call-graph-goto-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (save-excursion
    (when (get-char-property (point) 'button)
      (forward-char 4))
    (call-graph-visit-file-at-point)))

(defun call-graph-display-file-at-point ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (save-selected-window
    (call-graph-goto-file-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "e") 'call-graph-widget-expand-all)
    (define-key map (kbd "c") 'call-graph-widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'call-graph-quit)
    (define-key map (kbd "d") 'call-graph-display-file-at-point)
    (define-key map (kbd "o") 'call-graph-goto-file-at-point)
    (define-key map (kbd "+") 'call-graph-expand)
    (define-key map (kbd "_") 'call-graph-collapse)
    (define-key map (kbd "g") 'call-graph)
    (define-key map (kbd "<RET>") 'call-graph-goto-file-at-point)
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
        show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (when call-graph-display-file
    (add-hook 'widget-move-hook (lambda () (call-graph-display-file-at-point))))
  (run-mode-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "C-c g") 'call-graph)


(provide 'call-graph)
;;; call-graph.el ends here
