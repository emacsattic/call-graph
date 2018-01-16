;;; call-graph.el --- Library to generate call graph for cpp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Package-Version: 20180107.1540
;; Keywords: tools, convenience
;; Version: 0.0.1
;; Created: 2018-01-07
;; Package-Requires: ((emacs "25.1") (hierarchy "0.7.0"))

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

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'call-graph)

;;; Usage:

;; (call-graph function)
;; (test-it)
;; hierarchy will be  generated.

;;; Code:

(require 'seq)
(require 'map)

(require 'hierarchy)
;; (require 'call-graph-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom call-graph-max-depth 2
  "The maximum depth of call graph."
  :type 'integer)

(defconst call-graph-key-to-depth "*current-depth*"
  "The key to get current depth of call graph.")

;; use hash-table as the building blocks for tree
(defun make-new-hash-table ()
  (make-hash-table :test 'equal))

(defvar call-graph-internal-tree (make-new-hash-table)
  "The internal data of call graph.")

(defcustom call-graph-termination-list '("main")
  "Call-graph stops when seeing symbols from this list."
  :type 'list)

(defvar call-graph--current-buffer (current-buffer)
  "The current buffer on which call-graph operate."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--find-caller (reference)
  "Given a REFERENCE, return the caller of this reference."
  (when-let ((tmpVal (split-string reference ":"))
             (fileName (seq-elt tmpVal 0))
             (is-valid-file (file-exists-p fileName))
             (lineNb (string-to-number (seq-elt tmpVal 1)))
             (is-valid-Nb (integerp lineNb)))
    (let (caller)
      (find-file fileName)
      (goto-line lineNb)
      (setq caller (which-function))
      (unless (eq (current-buffer)
                  call-graph--current-buffer)
        (kill-this-buffer))
      (setq tmpVal (split-string caller "::"))
      (if (> (seq-length tmpVal) 1)
          (seq-elt tmpVal 1)
        (seq-elt tmpVal 0)))))

(defun call-graph--find-references (function)
  "Given a FUNCTION, return all references of this function."
  (let* ((command
          (concat (format "global -a --result=grep -r %s" function) " | grep -E \"\\.(cpp|cc):\""))
         (command-out-put (shell-command-to-string command)))
    (split-string command-out-put "\n" t)))

(defun call-graph--walk-tree-in-bfs-order (item node func)
  "Wallk tree in BFS order, apply FUNC for each (item . node).
ITEM is parent of NODE, NODE should be a hash-table."
  (let ((queue (make-queue))
        queue-elt current-item current-node)
    (queue-put queue (cons item node))
    (while (not (queue-empty-p queue))
      (setq queue-elt (queue-get queue)
            current-item (car queue-elt)
            current-node (cdr queue-elt))
      (funcall func current-item current-node)
      (seq-doseq (map-pair (map-pairs current-node))
        (when (mapp (cdr map-pair))
          (queue-put queue map-pair))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--create (item root)
  "Construct call-graph tree.
ITEM is parent of root, ROOT should be a hash-table."
  (when (and item root)
    (let ((caller-visited call-graph-termination-list))
      (setq call-graph--current-buffer (current-buffer))
      (push (symbol-name item) caller-visited)
      (map-put root call-graph-key-to-depth 0)
      (catch 'exceed-max-depth
        (call-graph--walk-tree-in-bfs-order
         item root
         (λ (parent node)
           (let (caller sub-node (depth (map-elt node call-graph-key-to-depth 0)))
             (when (> depth call-graph-max-depth) (throw 'exceed-max-depth t))
             (when (mapp node)
               (seq-doseq (reference (call-graph--find-references parent))
                 (when (not (member (setq caller (call-graph--find-caller reference)) caller-visited))
                   (message caller)
                   (push caller caller-visited)
                   (setq sub-node (make-new-hash-table))
                   (map-put sub-node call-graph-key-to-depth (1+ depth))
                   (map-put node (intern caller) sub-node)))))))))))

(defun call-graph--display (item root)
  "Prepare data for display.
ITEM is parent of root, ROOT should be a hash-table."
  (let ((first-time t) (log (list))
        (hierarchy (hierarchy-new)))
    (call-graph--walk-tree-in-bfs-order
     item root
     (λ (parent node)
       (when (mapp node)
         (map-delete node call-graph-key-to-depth)
         (seq-doseq (child (map-keys node))
           (when first-time (setq first-time nil)
                 (hierarchy--add-relation hierarchy parent nil 'identity))
           (hierarchy--add-relation hierarchy child parent 'identity)
           (push
            (concat "insert childe " (symbol-name child)
                    " under parent " (symbol-name parent)) log)))))
    (switch-to-buffer
     (hierarchy-tree-display
      hierarchy
      (λ (tree-item _) (insert (symbol-name tree-item)))))
    (seq-doseq (rec (reverse log)) (message rec))))

;;;###autoload
(defun call-graph ()
  "Construct call-graph tree."
  (interactive)
  (save-excursion
    (when-let ((target (symbol-at-point))
               (root (make-new-hash-table)))
      (call-graph--create target root)
      (call-graph--display target root))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-it ()
  (setq tmp (hierarchy-new)
        log '())
  (call-graph-display)
  (switch-to-buffer
   (hierarchy-tree-display
    tmp
    (lambda (item _) (insert (symbol-name item)))))
  (seq-doseq (rec (reverse log))
    (message rec)))


(provide 'call-graph)
;;; call-graph.el ends here
