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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--find-caller (reference)
  "Given a REFERENCE, return the caller of this reference."
  (let* ((tmpVal (split-string reference ":"))
         (fileName (seq-elt tmpVal 0))
         (lineNb (seq-elt tmpVal 1))
         caller)
    (find-file fileName)
    (goto-line (string-to-number lineNb))
    (setq caller (which-function))
    (kill-this-buffer)
    (setq tmpVal (split-string caller "::"))
    (if (> (seq-length tmpVal) 1)
        (seq-elt tmpVal 1)
      (seq-elt tmpVal 0))))

(defun call-graph--find-references (function)
  "Given a FUNCTION, return all references of this function."
  (let* ((command
          (format "global -a --result=grep -r %s | grep .cpp:" function))
         (command-out-put (shell-command-to-string command)))
    (split-string command-out-put "\n" t)))

(defun call-graph--walk-tree-in-bfs-order (root function)
  "Wallk tree in BFS order, apply FUNCTION for each node.
ROOT should be a hash-table with its values as hash-table too."
  (let ((queue (make-queue))
        (current-node nil))
    (queue-put queue root)
    (while (not (queue-empty-p queue))
      (setq current-node (queue-get queue))
      (seq-doseq (map-pair (map-pairs current-node))
        (let ((key (car map-pair))
              (value (cdr map-pair)))
          (funcall function key value current-node)
          (when (mapp value)
            (queue-put queue value)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun call-graph ()
  "Generate call-graph for function at point."
  (interactive)
  (save-excursion
    (when-let ((function (symbol-at-point)))
      (let ((caller-visited call-graph-termination-list)
            (root call-graph-internal-tree)
            (current-node nil))
        (clrhash root)    ;; clear history data
        (push (symbol-name function) caller-visited)
        (map-put root function (make-new-hash-table))
        (map-put root call-graph-key-to-depth 0)
        (catch 'exceed-max-depth
          (call-graph--walk-tree-in-bfs-order
           root
           (lambda (key value current-node)
             (let ((caller nil)
                   (sub-node nil))
               (when (> (map-elt current-node call-graph-key-to-depth 0) call-graph-max-depth)
                 (throw 'exceed-max-depth t))
               (when (mapp value)
                 (map-put value call-graph-key-to-depth (1+ (map-elt current-node call-graph-key-to-depth)))
                 (seq-doseq (reference (call-graph--find-references key))
                   (when (not (member (setq caller (call-graph--find-caller reference)) caller-visited))
                     (message caller)
                     (push caller caller-visited)
                     (setq sub-node (make-new-hash-table))
                     (map-put value (intern caller) sub-node))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-display ()
  (let ((first-time t))
    (call-graph--walk-tree-in-bfs-order
     call-graph-internal-tree
     (lambda (key value _)
       (let ((parent key)
             (children nil))
         (when (mapp value)
           (map-delete value call-graph-key-to-depth)
           (setq children  (map-keys value))
           (seq-doseq (child children)
             (when first-time
               (setq first-time nil)
               (hierarchy--add-relation tmp parent nil 'identity))
             (hierarchy--add-relation tmp child parent 'identity)
             (push
              (concat "insert childe " (symbol-name child)
                      " under parent " (symbol-name parent)) log))))))))

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
