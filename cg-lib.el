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
(require 'cc-defs)
(require 'cc-menus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pattern-replace-alist
  '(("\"[^\"]*\""   " quoted-string ") ;; get rid of quoted-string first
    ("([^()]*)"     " parens ")
    ("<[^<>]*>"     " angle-bracket ")
    ("{[^{}]*}"     " curly-bracket ")
    ("\\[[^][]*\\]" " square-bracket ")
    ("void"         ""))
  "Replace PATTERN with REPLACE for better C++ function argument parsing.")

(defconst pattern-to-func-left-parens
  (concat
   "\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)" ;; match function name
   "\\([ \t\n]\\|\\\\\n\\)*(") ;; match left-parens
  "Regexp to match function til its left parens.")

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

;;; enable imenu to display both function name and its arg-list
(defun enable-imenu-func-args()
  "For current buffer, enable imenu to extract both function name and its arg-list."
  (make-local-variable 'cc-imenu-c-generic-expression)
  (make-local-variable 'cc-imenu-c++-generic-expression)
  (setf (nth 2 cc-imenu-c++-generic-expression)
        ;; General function name regexp
        `(nil
          ,(concat
            "^\\<"                                 ; line MUST start with word char
            ;; \n added to prevent overflow in regexp matcher.
            ;; https://lists.gnu.org/r/emacs-pretest-bug/2007-02/msg00021.html
            "[^()\n]*"                             ; no parentheses before
            "[^" c-alnum "_:<>~]"                  ; match any non-identifier char
            "\\(?2:\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)" ; 2ND-GROUP MATCH FUNCTION AND ITS ARGS WHILE 1ST-GROUP MATCH FUNCTION NAME
            "\\([ \t\n]\\|\\\\\n\\)*("            ; see above, BUT the arg list
            "\\([ \t\n]\\|\\\\\n\\)*"             ; must not start
            "\\([^ \t\n(*]"                       ; with an asterisk or parentheses
            "[^()]*\\(([^()]*)[^()]*\\)*"         ; Maybe function pointer arguments
            "\\)?)\\)"                            ; END OF 2ND-GROUP
            "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n;(]"
            ) 2)                                  ; USE 2ND-GROUP AS IMENU ITEM
        cc-imenu-c-generic-expression cc-imenu-c++-generic-expression))

;;; borrowed from somewhere else
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string
    "[ \t\n]*\\'" "" string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-of-args(func-with-args)
  "Count number of C++ function arguments of FUNC-WITH-ARGS."
  (condition-case nil
      (with-temp-buffer
        (insert func-with-args)
        (check-parens) ;; check parentheses balance
        (goto-char (point-min))
        (unless (re-search-forward pattern-to-func-left-parens nil t)
          (error "Failed to find left-parens"))
        (delete-region (point-min) (point))
        (goto-char (point-max))
        (delete-region (search-backward ")" nil t) (point-max))
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
                  ((not (string= "" args-string))
                   (length (split-string args-string ",")))))))
    (error nil)))

(defun get-number-of-args(&optional func-with-args)
  "Interactively get number of arguments of FUNC-WITH-ARGS."
  (interactive (list (smart/read-from-minibuffer "Input C++ function with args")))
  (deactivate-mark)
  (let ((nb-args (number-of-args func-with-args)))
    (if nb-args
        (message "Number of args is: %d" nb-args)
      (message "Failed to get argument."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-assert (= (number-of-args "func(template<p1,p2>(a),[a,b](a,b){a,b,c;},(a,b))") 3))


(provide 'cg-lib)
;;; cg-lib.el ends here
