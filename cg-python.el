;;; cg-python.el --- call-graph python support. -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Huming Chen

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

;; Python support code for call-graph should go here.

;;; Code:

(require 'python)
(require 'anaconda-mode)
(require 'cg-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cg--internal-references nil
  "Used by internal functions.")

(defvar cg--internal-source nil
  "Used by internal functions.")

(defvar cg--internal-line nil
  "Used by internal functions.")

(defvar cg--internal-column nil
  "Used by internal functions.")

(defvar cg--internal-path nil
  "Used by internal functions.")

(defvar cg--map-path-to-source (make-hash-table :test #'equal)
  "Map path to source.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--python-extract-method-name (full-func)
  "Given FULL-FUNC, return a SHORT-FUNC.
e.g: class.method => method."
  (when-let ((full-func-str (symbol-name full-func))
             (short-func (intern (car (last (split-string full-func-str "\\."))))))
    short-func))

(defun cg--python-find-caller (reference func &optional data-mode)
  "Given a REFERENCE of FUNC for mode DATA-MODE.
Return the caller as (caller . location).
When FUNC with args, match number of args as well."
  (when-let ((is-valid-func func)
             (tmp-split (split-string reference ":"))
             (file-name (car tmp-split))
             (line-nb-str (cadr tmp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (let ((location (concat file-name ":" line-nb-str))
          (caller nil))
      (with-temp-buffer
      ;; (with-current-buffer "TEST"
        ;; (erase-buffer)
        (insert-file-contents-literally file-name)
        (goto-char (point-min))
        (forward-line (1- line-nb))
        (setq-local python-mode-hook nil)
        (setq imenu--index-alist nil)
        (python-mode)
        (setq-local which-func-cleanup-function nil)
        (which-function-mode t)
        (setq caller (which-function))
        (when-let ((is-valid-caller (not (equal caller (symbol-name func))))
                   (short-caller (cg--extract-method-name (intern caller)))
                   (short-caller-str (symbol-name short-caller)))
          (beginning-of-defun)
          (search-forward short-caller-str)
          (cg--save-param-in-symbol (intern short-caller-str)
                                    (buffer-substring-no-properties (point-min) (point-max))
                                    (line-number-at-pos)
                                    (current-column)
                                    file-name)
          (cons (intern short-caller-str) location))))))

(defun cg--python-find-references (func)
  "Given FUNC, return all references as a list."
  (cg--read-param-from-symbol func)
  (cg--anaconda-mode-call
   "usages"
   (lambda (result)
     (setq cg--internal-references (cg--references-to-list result))))
  cg--internal-references)

(defun cg--python-handle-root-function (call-graph func)
  "Save location of root function FUNC in CALL-GRAPH."
  (when-let ((file-name (buffer-file-name))
             (line-nb (line-number-at-pos))
             (location (concat file-name ":" (number-to-string line-nb))))
    ;; save root function location
    (setf (map-elt (call-graph--locations call-graph) 'root-function) (list location))
    ;; save root function param
    (cg--save-param-in-symbol func
                              (buffer-substring-no-properties (point-min) (point-max))
                              (line-number-at-pos)
                              (current-column)
                              file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--save-param-in-symbol (symbol source line column path)
  "Save param SOURCE, LINE, COLUMN, PATH in SYMBOL."
  (setf (map-elt cg--map-path-to-source path) source)
  (put symbol 'param-line line)
  (put symbol 'param-column column)
  (put symbol 'param-path path))

(defun cg--read-param-from-symbol (symbol)
  "Read param from SYMBOL."
  (setq cg--internal-source (map-elt cg--map-path-to-source (get symbol 'param-path))
        cg--internal-line (get symbol 'param-line)
        cg--internal-column (get symbol 'param-column)
        cg--internal-path (pythonic-python-readable-file-name (get symbol 'param-path))))

(defun cg--references-to-list (result)
  "Return refererces as a list from RESULT."
  (when result
    (let ((references (list)))
      (if (stringp result)
          (message result)
        (seq-doseq (item result)
          (push
           (concat (seq-elt item 0) ":" (int-to-string (seq-elt item 1))  ": " (seq-elt item 3))
           references)))
      references)))

(defun cg--anaconda-mode-call (command callback)
  "Make remote procedure call for COMMAND.
Apply CALLBACK to it result."
  (anaconda-mode-start
   (lambda () (cg--anaconda-mode-jsonrpc command callback))))

(defun cg--anaconda-mode-jsonrpc (command callback)
  "Perform JSONRPC call for COMMAND.
Apply CALLBACK to the call result when retrieve it.  Remote
COMMAND must expect four arguments: python buffer content, line
number position, column number position and file path."
  (let ((url-request-method "POST")
        (url-request-data (cg--anaconda-mode-jsonrpc-request command)))
    (url-retrieve
     (format "http://%s:%s" (anaconda-mode-host) (anaconda-mode-port))
     (cg--anaconda-mode-create-response-handler callback)
     nil
     t)))

(defun cg--anaconda-mode-jsonrpc-request (command)
  "Prepare JSON encoded buffer data for COMMAND call."
  (encode-coding-string (json-encode (cg--prepare-jsonrpc-request-data command)) 'utf-8))

(defun cg--prepare-jsonrpc-request-data (command)
  "Prepare buffer data for COMMAND call."
  `((jsonrpc . "2.0")
    (id . 1)
    (method . ,command)
    (params . ((source . ,cg--internal-source)
               (line . ,cg--internal-line)
               (column . ,cg--internal-column)
               (path . ,cg--internal-path)))))

(defun cg--anaconda-mode-create-response-handler (callback)
  "Create server response handler based on CALLBACK function."
  (let ((anaconda-mode-request-buffer (current-buffer)))
    (lambda (status)
      (let ((http-buffer (current-buffer)))
        (unwind-protect
            (progn
              (search-forward-regexp "\r?\n\r?\n" nil t)
              (let ((response
                     (condition-case nil
                         (json-read)
                       ((json-readtable-error json-end-of-file end-of-file)
                        (let ((response (concat (format "# status: %s\n# point: %s\n" status (point))
                                                (buffer-string))))
                          (with-current-buffer (get-buffer-create anaconda-mode-response-buffer)
                            (erase-buffer)
                            (insert response)
                            (goto-char (point-min)))
                          nil)))))
                (if (null response)
                    (message "Cannot read anaconda-mode server response")
                  (if (assoc 'error response)
                      (let* ((error-structure (cdr (assoc 'error response)))
                             (error-message (cdr (assoc 'message error-structure)))
                             (error-data (cdr (assoc 'data error-structure)))
                             (error-template (if error-data "%s: %s" "%s")))
                        (apply 'message error-template (delq nil (list error-message error-data))))
                    (with-current-buffer anaconda-mode-request-buffer
                      (let ((result (cdr (assoc 'result response))))
                        ;; Terminate `apply' call with empty list so response
                        ;; will be treated as single argument.
                        (apply callback result nil)))))))
          (kill-buffer http-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @see cg-test.el


(provide 'cg-python)
;;; cg-python.el ends here
