;;; cg-lib.el --- call-graph routines. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph

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

;; All functions that don't require specific call-graph code should go here.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pattern-replace-alist
  '(("\"[^\"]*\""   " quoted-string ") ;; get rid of quoted-string first
    ("([^()]*)"     " parens ")
    ("<[^<>]*>"     " brackets ")
    ("{[^{}]*}"     " curly-brace ")
    ("\\[[^][]*\\]" " brackets-2 ")
    ("void"         ""))
  "Replace PATTERN with REPLACE for better C++ function argument parsing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; steal from ag/dwim-at-point
(defun smart/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; improved version, based on ag/read-from-minibuffer
(defun smart/read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, use it instead of prompt."
  (let* ((suggested (smart/dwim-at-point))
         (final-prompt
          (if suggested (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt))))
    (if (or current-prefix-arg (string= "" suggested) (not suggested))
        (read-from-minibuffer final-prompt nil nil nil nil suggested)
      suggested)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-of-args(func-with-args)
  "Count number of C++ function arguments of FUNC-WITH-ARGS."
  (condition-case nil
      (with-temp-buffer
        (insert func-with-args)
        (check-parens) ;; check parentheses balance
        (delete-region (point-min) (with-no-warnings (goto-char (point-min)) (search-forward "(" nil t) (point)))
        (delete-region (with-no-warnings (goto-char (point-max)) (search-backward ")" nil t) (point)) (point-max))
        ;; (message (buffer-string))
        (save-match-data ;; save previous match-data and restore later
          ;; Map over the elements of pattern-replace-alist
          ;; (pattern, replace)
          (dolist (pair pattern-replace-alist)
            (let ((pattern (car pair))
                  (replace (cadr pair)))
              (goto-char (point-min))
              (while (re-search-forward pattern nil t) ;; patttern exists
                (goto-char (point-min)) ;; start from begining
                (while (re-search-forward pattern nil t) ;; start replacing
                  (replace-match replace t nil))
                (goto-char (point-min))))) ;; go over and do match-replace again
          ;; all noise cleared, count number of args
          (let ((args-string (trim-string (buffer-string))))
            (cond ((string= "" args-string) 0)
                  ((not (string= "" args-string)) (length (split-string args-string ",")))))))
    (error nil)))

(defun get-number-of-args(func-with-args)
  "Interactively get number of arguments of FUNC-WITH-ARGS."
  (interactive (list (smart/read-from-minibuffer "Input C++ function with args")))
  (deactivate-mark)
  (message "number of args is: %d" (number-of-args func-with-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-assert (= (number-of-args "func(template<p1,p2>(a),[a,b](a,b){a,b,c;},(a,b))") 3))

(provide 'cg-lib)
;;; cg-lib.el ends here
