;;; cg-global.el --- C++ support -*- lexical-binding: t -*-

;; Copyright (C) 2019-2023 Huming Chen

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

;; C++ support code for call-graph should go here.

;;; Code:

(require 'cc-mode)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'which-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom cg-ignore-invalid-reference nil
  "Non-nil means reference with function name but no `(...)' will be ignored."
  :type 'boolean
  :group 'call-graph)

(defcustom cg-display-func-args nil
  "Display function together with its args in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defcustom cg-search-filters '("grep -E \"\\.(cpp|cc|c):\"")
  "The filters used by `call-graph' when searching caller."
  :type 'list
  :group 'call-graph)

(defcustom cg-path-to-global nil
  "Directory to search Gnu global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'call-graph)

(defcustom cg-search-backend nil
  "Backend for `call-graph' to search with."
  :type '(choice (const :tag "Global")
                 (const :tag "Git"))
  :risky t
  :group 'call-graph)

(defcustom cg-path-to-git-repo nil
  "Directory to git repo."
  :type 'string
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst cg--pattern-replace-alist
  '(("\"[^\"]*\""   " quoted-string ") ;; get rid of quoted-string first
    ("([^()]*)"     " parens ")
    ("<[^<>]*>"     " angle-bracket ")
    ("{[^{}]*}"     " curly-bracket ")
    ("\\[[^][]*\\]" " square-bracket ")
    ("void"         ""))
  "Replace PATTERN with REPLACE for better C++ function argument parsing.")

(defconst cg--pattern-to-func-left-parens
  (concat
   "\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)" ;; match function name
   "\\([ \t\n]\\|\\\\\n\\)*(") ;; match left-parens
  "Regexp to match function til its left parens.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--extract-method-name (full-func)
  "Given FULL-FUNC, return a SHORT-FUNC.
e.g: class::method(arg1, arg2) => method."
  (when-let ((full-func-str (symbol-name full-func))
             (temp-split (split-string full-func-str "("))
             (short-func-with-namespace (car temp-split))
             (short-func (intern (car (last (split-string short-func-with-namespace "::"))))))
    short-func))

(defun cg--which-function ()
  "Return current function name and args based on point."
  (when-let ((func (which-function)))
    (catch 'found
      (dolist (alist imenu--index-alist)
        (when-let ((full-func (car alist))
                   (match? (string-match func full-func))
                   (found? (zerop match?)))
          (throw 'found full-func))))))

(defun cg--find-caller (reference func data-mode)
  "Given a REFERENCE of FUNC for mode DATA-MODE.
Return the caller as (caller . location).
When FUNC with args, match number of args as well."
  (when-let ((tmp-split (split-string reference ":"))
             (file-name (car tmp-split))
             (line-nb-str (cadr tmp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb))
             func
             (short-func (cg--extract-method-name func)))
    (let ((location (concat file-name ":" line-nb-str))
          (caller nil)
          (nb-of-func-args (cg--number-of-args (symbol-name func)))
          (nb-of-reference-args nil)
          (short-fun-str (symbol-name short-func))
          (is-valid-reference t))
      (with-temp-buffer
        (cg--show-function-args)
        (insert-file-contents-literally file-name)
        (goto-char (point-min))
        (while (re-search-forward "__attribute__[ \t\n]*(([[:alpha:]]+))" nil t)
          ; imenu failed to parse function with __attribute__ ((...)) as args
          (replace-match "__attribute__" t nil))
        (goto-char (point-min))
        (forward-line (1- line-nb))
        (cg--setq-local-mode-hook-nil data-mode)
        (setq imenu--index-alist nil)
        (funcall data-mode)
        (setq-local which-func-cleanup-function nil)
        (which-function-mode t)
        ;; make sure reference contains a function call
        (when cg-ignore-invalid-reference
          (save-excursion
            (end-of-line)
            (let ((end-of-line-pos (point)))
              (beginning-of-line)
              (if (not (re-search-forward (concat short-fun-str "\\([ \t\n]\\|\\\\\n\\)*(") nil t))
                  (setq is-valid-reference nil)
                (when (> (match-beginning 0) end-of-line-pos)
                  (setq is-valid-reference nil))))))
        (when is-valid-reference
          (setq nb-of-reference-args (cg--scan-func-args short-fun-str))
          (if (and nb-of-func-args nb-of-reference-args)
              ;; TODO: check if func has args with default value
              ;; if not, we should use exact match here.
              (when (= nb-of-reference-args nb-of-func-args) ; check func-args matches references-args
                (setq caller (cg--which-function)))
            (setq caller (cg--which-function)))
          (unless cg-display-func-args
            (setq caller (cg--extract-namespace-and-method caller)))))
      (when caller
        (cons (intern caller) location)))))

(defun cg--git-find-references (func root-location)
  "Given a FUNC and ROOT-LOCATION, return all references as a list."
  (let* ((git-repo-path (string-trim-right cg-path-to-git-repo "[ \t\n\r/]+"))
         (command
          (format "git -C %s --no-pager grep --full-name --no-color -ne %s"
                  (shell-quote-argument git-repo-path)
                  (shell-quote-argument (symbol-name func))))
         (filter-separator " | ")
         command-filter command-output command-result)
    (when (and (> (length cg-search-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil cg-search-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-output
                (with-temp-message "Searching ..."
                  (shell-command-to-string command)))
      (seq-doseq (reference (split-string command-output "\n" t))
        (when-let* ((full-location (concat git-repo-path "/" reference))
                    ;; TODO: enable this when call-graph--root-location works well
                    ;; (ignore (not (string-match root-location full-location)))
                    )
          (cl-pushnew full-location command-result))))
    command-result))

(defun cg--global-find-references (func)
  "Given a FUNC, return all references as a list."
  (let ((command
         (format "%s -a --result=grep -r %s"
                 (cg--get-path-to-global)
                 (shell-quote-argument (symbol-name func))))
        (filter-separator " | ")
        command-filter command-output)
    (when (and (> (length cg-search-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil cg-search-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-output (shell-command-to-string command))
      (split-string command-output "\n" t))))

(defun cg--handle-root-function (call-graph)
  "Save location of root function in CALL-GRAPH."
  (when-let ((file-name (buffer-file-name))
             (line-nb (line-number-at-pos))
             (location (concat file-name ":" (number-to-string line-nb))))
    ;; save root function location
    (setf (map-elt (call-graph--locations call-graph) 'root-function) (list location))
    ;; TODO: this line causes void-function error, fix it later
    ;; (setf (call-graph--root-location call-graph) location)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; steal from ag/dwim-at-point
(defun cg--dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;;; improved version, based on ag/read-from-minibuffer
(defun cg--read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, use it instead of prompt."
  (let* ((suggested (cg--dwim-at-point))
         (final-prompt
          (if suggested (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt))))
    (if (or current-prefix-arg (string= "" suggested) (not suggested))
        (read-from-minibuffer final-prompt nil nil nil nil suggested)
      suggested)))

(defun cg--trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string
    "[ \t\n]*\\'" "" string)))

(defun cg--extract-namespace-and-method (full-func)
  "Given FULL-FUNC, return a namespace and method.
e.g: class::method(arg1, arg2) => class::method."
  (when-let ((full-func-str full-func)
             (temp-split (split-string full-func-str "("))
             (short-func-with-namespace (car temp-split)))
    short-func-with-namespace))

(defun cg--get-path-to-global ()
  "Return path to program GNU GLOBAL."
  (let ((absolute-path
         (or (executable-find "global")
             (expand-file-name "global" cg-path-to-global))))
    (unless (file-exists-p absolute-path)
      (error "Failed to find \"GNU GLOBAL\" in path: %s" absolute-path))
    absolute-path))

(defun cg--show-function-args ()
  "Customize c++-generic-expression to support function args."
  (make-local-variable 'cc-imenu-c++-generic-expression)
  (make-local-variable 'cc-imenu-c-generic-expression)
  ;; enable imenu to display both function name and its arg-list
  (setf (nth 2 cc-imenu-c++-generic-expression)
        ;; General function name regexp
        `(nil
          ,(concat
            "^\\<"                                 ; line MUST start with word char
            ;; \n added to prevent overflow in regexp matcher.
            ;; https://lists.gnu.org/r/emacs-pretest-bug/2007-02/msg00021.html
            "[^()\n]*"                             ; no parentheses before
            "[^" c-alnum "_:<>~]"                  ; match any non-identifier char
            ; 2ND-GROUP MATCH FUNCTION AND ITS ARGS WHILE 1ST-GROUP MATCH FUNCTION NAME
            "\\(?2:\\(?1:[" c-alpha "_][" c-alnum "_:<>~]*\\)"
            "\\([ \t\n]\\|\\\\\n\\)*("            ; see above, BUT the arg list
            "\\([ \t\n]\\|\\\\\n\\)*"             ; must not start
            "\\([^ \t\n(*]"                       ; with an asterisk or parentheses
            "[^()]*\\(([^()]*)[^()]*\\)*"         ; Maybe function pointer arguments
            "\\)?)\\)"                            ; END OF 2ND-GROUP
            "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n;(]"
            ) 2)                                  ; USE 2ND-GROUP AS IMENU ITEM
        cc-imenu-c-generic-expression cc-imenu-c++-generic-expression))

(defun cg--number-of-args (func-with-args)
  "Count number of C++ function arguments of FUNC-WITH-ARGS."
  (ignore-errors
    (with-temp-buffer
      (insert func-with-args)
      (check-parens) ;; check parentheses balance
      (goto-char (point-min))
      (unless (re-search-forward cg--pattern-to-func-left-parens nil t)
        (error "Failed to find left-parens"))
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (delete-region (search-backward ")" nil t) (point-max))
      ;; (message (buffer-string))
      (save-match-data ;; save previous match-data and restore later
        ;; Map over the elements of cg--pattern-replace-alist
        ;; (pattern, replace)
        (dolist (pair cg--pattern-replace-alist)
          (let ((pattern (car pair))
                (replace (cadr pair)))
            (goto-char (point-min))
            (while (re-search-forward pattern nil t) ;; patttern exists
              (goto-char (point-min)) ;; start from begining
              (while (re-search-forward pattern nil t) ;; start replacing
                (replace-match replace t nil))
              (goto-char (point-min))))) ;; go over and do match-replace again
        ;; all noise cleared, count number of args
        (let ((args-string (cg--trim-string (buffer-string))))
          (cond ((string= "" args-string) 0)
                ((not (string= "" args-string))
                 (length (split-string args-string ",")))))))))

(defun cg--scan-func-args (func)
  "Scan FUNC and its args from current position, and return number of args."
  (save-excursion
    (save-match-data
      (ignore-errors
        (let (func-beginning
              func-with-args-str)
          (search-forward func)
          (setq func-beginning (match-beginning 0))
          (forward-sexp)
          (setq func-with-args-str
                (buffer-substring-no-properties func-beginning (point)))
          (when func-with-args-str
            (cg--number-of-args func-with-args-str)))))))

(defun cg-get-number-of-args (&optional func-with-args)
  "Interactively get number of arguments of FUNC-WITH-ARGS."
  (interactive (list (cg--read-from-minibuffer "Input C++ function with args")))
  (deactivate-mark)
  (let ((nb-args (cg--number-of-args func-with-args)))
    (if nb-args
        (message "Number of args is: %d" nb-args)
      (message "Failed to get argument."))))

(defun cg--setq-local-mode-hook-nil (mode)
  "Clear mode hooks for MODE."
  (cond ((eql mode 'c++-mode)
         (setq-local c++-mode-hook nil))
        ((eql mode 'c-mode)
         (setq-local c-mode-hook nil))))


(provide 'cg-support)
;;; cg-support.el ends here
