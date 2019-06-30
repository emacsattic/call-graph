;;; cg-python.el --- call-graph python support. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Huming Chen

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
(require 'which-func)
(require 'seq)
(require 'subr-x)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--extract-method-name (full-func)
  "Given FULL-FUNC, return a SHORT-FUNC.
e.g: class.method => method."
  (when-let ((full-func-str (symbol-name full-func))
             (short-func (intern (car (last (split-string full-func-str "\\."))))))
    short-func))

(defun cg--find-caller (reference &optional func)
  "Given a REFERENCE of FUNC, return the caller as (caller . location).
When FUNC with args, match number of args as well."
  (when-let ((tmp-split (split-string reference ":"))
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
        (when-let (caller
                   (short-caller (cg--extract-method-name (intern caller)))
                   (short-caller-str (symbol-name short-caller)))
          (beginning-of-defun)
          (search-forward short-caller-str)
          (setq cg--internal-source (buffer-substring-no-properties (point-min) (point-max))
                cg--internal-line (line-number-at-pos)
                cg--internal-column (current-column)
                cg--internal-path (pythonic-python-readable-file-name file-name))
          (cons (intern short-caller-str) location))))))

(defun cg--find-references (func)
  "Given FUNC, return all references as a list."
  (cg--anaconda-mode-call
   "usages"
   (lambda (result)
     (setq cg--internal-references (cg--references-to-list result))))
  cg--internal-references)

(defun cg--handle-root-function (call-graph)
  "Save root function in CALL-GRAPH."
  (when-let ((file-name (buffer-file-name))
             (line-nb (line-number-at-pos))
             (location (concat file-name ":" (number-to-string line-nb))))
    ;; save root function location
    (setf (map-elt (call-graph--locations call-graph) 'root-function) (list location))
    (setq cg--internal-source (buffer-substring-no-properties (point-min) (point-max))
          cg--internal-line (line-number-at-pos)
          cg--internal-column (current-column)
          cg--internal-path (pythonic-python-readable-file-name (buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     (anaconda-mode-create-response-handler callback)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @see cg-test.el


(provide 'cg-python)
;;; cg-python.el ends here
