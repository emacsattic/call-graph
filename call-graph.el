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

;; (call-graph func)
;; (test-it)
;; hierarchy will be  generated.

;;; Code:

(require 'seq)
(require 'map)

(require 'hierarchy)
;; (require 'cg-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom cg-max-depth 2
  "The maximum depth of call graph."
  :type 'integer)


(defconst cg-key-to-depth "*current-depth*"
  "The key to get current depth of call graph.")


;; use hash-table as the building blocks for tree
(defun make-new-hash-table ()
  (make-hash-table :test 'equal))


(defvar cg-internal-tree (make-new-hash-table)
  "The data of call graph all stored in this.")


(defun cg-find-caller (reference)
  (let* ((tmpVal (split-string reference ":"))
         (fileName (nth 0 tmpVal))
         (lineNb (nth 1 tmpVal))
         caller)

    (find-file fileName)
    (goto-line (string-to-number lineNb))
    (setq caller (which-function))
    (kill-this-buffer)
    (setq tmpVal (split-string caller "::"))
    (if (> (length tmpVal) 1)
        (nth 1 tmpVal)
      (nth 0 tmpVal))))

(defun cg-find-references (func)
  (interactive "S")
  (let* ((command
          (format "global -a --result=grep -r %s | grep .cpp:" func))
         (command-out-put (shell-command-to-string command)))
    (split-string command-out-put "\n" t)))


(defun cg-walk-tree-in-bfs-order (root func)
  "wallk tree in breadth first search order."
  (let ((queue (make-queue))
        (current-node nil))

    (queue-put queue root)
    (while (not (queue-empty-p queue))
      (setq current-node (queue-get queue))
      (dolist (map-pair (map-pairs current-node))
        (let ((key (car map-pair))
              (value (cdr map-pair)))

          (funcall func key value)
          (when (mapp value)
            (queue-put queue value)))))))


;;;###autoload
(defun call-graph (func)
  "Generate a call-graph for the function func."
  (interactive "S")
  (let* ((queue (make-queue))
         (root cg-internal-tree)
         (current-node nil)
         (caller-visited '()))

    ;; clear history data
    (clrhash root)

    (map-put root (symbol-name func) (make-new-hash-table))
    (map-put root cg-key-to-depth 0)
    (queue-put queue root)

    (catch 'exceed-max-depth

      (while (not (queue-empty-p queue))
        (setq current-node (queue-get queue))

        (when (> (map-elt current-node cg-key-to-depth 0) cg-max-depth)
          (throw 'exceed-max-depth t))

        (dolist (map-pair (map-pairs current-node))
          (let ((key (car map-pair))
                (sub-node (cdr map-pair))
                (caller nil)
                (new-sub-node nil))

            (when (mapp sub-node)
              (map-put sub-node cg-key-to-depth (1+ (map-elt current-node cg-key-to-depth)))
              (queue-put queue sub-node)

              (dolist (reference (cg-find-references key))
                (when (not
                       (member (setq caller (cg-find-caller reference)) caller-visited))
                  (message caller)
                  (push caller caller-visited)
                  (setq new-sub-node (make-new-hash-table))
                  (map-put sub-node caller new-sub-node)
                  (queue-put queue new-sub-node))))))))))




(defun call-graph-display ()
  (let ((first-time t))
    (cg-walk-tree-in-bfs-order
     cg-internal-tree
     (lambda (key value)
       (let ((parent (intern key))
             (children nil))
         (when (mapp value)
           (map-delete value cg-key-to-depth)
           (setq children (seq-map (lambda (elt) (intern elt)) (map-keys value)))
           (seq-doseq (child children)
             (when first-time
               (setq first-time nil)
               (hierarchy--add-relation tmp parent nil 'identity))
             (hierarchy--add-relation tmp child parent 'identity)
             (push
              (concat "insert childe " (symbol-name child)
                      " under parent " (symbol-name parent)) log))))))))

(defun test-it ()
  (setq tmp (hierarchy-new))
  (setq log '())

  (call-graph-display)
  (switch-to-buffer
   (hierarchy-tree-display
    tmp
    (lambda (item _) (insert (symbol-name item))))))


(provide 'call-graph)
;;; call-graph.el ends here
