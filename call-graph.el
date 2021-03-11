;;; call-graph.el --- Library to generate call graph for c/c++ functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021 by Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Version: 0.1.0
;; Keywords: programming, convenience
;; Created: 2018-01-07
;; Package-Requires: ((emacs "25") (cl-lib "0.6.1") (hierarchy "0.7.0") (tree-mode "1.0.0") (ivy "0.10.0") (anaconda-mode "0.1.13"))

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

;; Library to generate call graph for c/c++ functions.

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

(require 'cg-global)
(require 'cg-lib)
(require 'cg-python)
(require 'hierarchy)
(require 'ivy)
(require 'tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup call-graph nil
  "Customization support for the `call-graph'."
  :version "0.1.0"
  :group 'applications)

(defcustom cg-initial-max-depth 2
  "The maximum initial depth of call graph."
  :type 'integer
  :group 'call-graph)

(defcustom cg-search-filters '("grep -E \"\\.(cpp|cc|c):\"")
  "The filters used by `call-graph' when searching caller."
  :type 'list
  :group 'call-graph)

(defcustom cg-display-file t
  "Non-nil means display file in another window while moving from one field to another in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defcustom cg-path-to-global nil
  "If non-nil the directory to search global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'call-graph)

(defcustom cg-display-func-args nil
  "Non-nil means display function together with its args in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defcustom cg-ignore-invalid-reference nil
  "Non-nil means reference with function name but no `(...)' will be ignored."
  :type 'boolean
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cg-persist-caller-cache nil
  "The alist form of `cg--caller-cache'.")

(defvar cg--caller-cache nil
  "The cached caller-map.")

(defvar cg--default-instance nil
  "Default CALL-GRAPH instance.")

(defvar cg--default-hierarchy nil
  "Hierarchy to display `call-graph'.")

(defvar cg--window-configuration nil
  "The window configuration to be restored upon closing the buffer.")

(defvar cg--selected-window nil
  "The currently selected window.")

(defvar cg--created-buffers ()
  "List of buffers created by `call-graph'.")

(defvar cg--previous-buffers ()
  "List of buffers before opening `call-graph'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (call-graph
               (:constructor nil)
               (:constructor call-graph--make)
               (:conc-name call-graph--))
  (callers (make-hash-table :test #'equal)) ; map func to its callers
  (locations (make-hash-table :test #'equal)) ; map func <- caller to its locations
  (data-mode nil)) ; to which mode call-graph data belongs

(defun cg-new ()
  "Create a `call-graph' and return it."
  (call-graph--make))

(defun cg--add-callers (call-graph func callers)
  "In CALL-GRAPH, given FUNC, add CALLERS."
  (when (and call-graph func callers)
    (let* ((short-func (cg--extract-method-name func))) ; method only
      (unless (map-elt (call-graph--callers call-graph) short-func)
        (seq-doseq (caller callers)
          (let* ((full-caller (car caller)) ; class::method
                 (location (cdr caller)) ; location
                 (func-caller-key
                  (intern (concat (symbol-name short-func) " <- " (symbol-name full-caller))))) ; "callee <- class::caller" as key

            ;; populate caller data
            (cl-pushnew full-caller (map-elt (call-graph--callers call-graph) short-func (list)))

            ;; populate location data
            (cl-pushnew location (map-elt (call-graph--locations call-graph) func-caller-key (list))
                        :test #'equal)))))))

(defun cg--is-valid (call-graph)
  "Check if CALL-GRAPH is valid."
  (when (and call-graph
             (not (zerop (map-length (call-graph--callers call-graph))))
             (not (zerop (map-length (call-graph--locations call-graph)))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persitence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--save-caller-cache ()
  "Save caller cache by saving cg-persist-caller-cache in .emacs.desktop file."
  (when cg--caller-cache
    (setq cg-persist-caller-cache
          (map-into cg--caller-cache 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--get-func-caller-location (call-graph func caller)
  "In CALL-GRAPH, given FUNC and CALLER, return the caller postion."
  (when (and call-graph func caller)
    (let ((locations (call-graph--locations call-graph))
          (func-caller-key
           (if (eq 'root-function func) 'root-function ; special treatment for root-function
             (intern (concat (symbol-name (cg--extract-method-name func)) " <- " (symbol-name caller))))))
      (map-elt locations func-caller-key))))

(defun cg--get-buffer ()
  "Generate buffer <*call-graph*>."
  (let ((buffer-name "*call-graph*"))
    (get-buffer-create buffer-name)))

(defun cg--visit-function (func-location)
  "Visit function location FUNC-LOCATION."
  (when-let ((temp-split (split-string func-location ":"))
             (file-name (car temp-split))
             (line-nb-str (cadr temp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (find-file-read-only-other-window file-name)
    (with-no-warnings (goto-line line-nb))
    (unless (member
             (buffer-name (window-buffer))
             (mapcar (function buffer-name) cg--previous-buffers))
      (add-to-list 'cg--created-buffers (window-buffer)))))

(defun cg--widget-root ()
  "Return current tree root."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((me (tree-mode-icon-current-line)))
      (when (and (not (tree-widget-leaf-node-icon-p me))
                 (tree-widget-p (widget-get me :parent)))
        (setq me (widget-get me :parent))
        (intern (widget-get (tree-widget-node me) :tag))))))

(defun cg--widget-depth ()
  "Return current tree depth."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((me (tree-mode-icon-current-line))
          (depth 0))
      (if (or (tree-widget-leaf-node-icon-p me)
              (not (tree-widget-p (widget-get me :parent))))
          (message "Not a tree under point!")
        (prog1 (setq me (widget-get me :parent)
                     depth (cg--widget-depth-imp me))
          (message "Depth of tree is %d" depth))))))

(defun cg--widget-depth-imp (tree &optional depth)
  "Return `DEPTH' of `TREE'."
  (if-let ((depth (or depth 0))
           (is-valid-tree (tree-widget-p tree))
           (is-tree-open (widget-get tree :open)))
      (progn
        ;; (message "Depth of %s is %d" (widget-get (tree-widget-node tree) :tag) depth)
        (seq-max
         (seq-map (lambda (child) (cg--widget-depth-imp child (1+ depth)))
                  (widget-get tree :children))))
    (if is-valid-tree depth (1- depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--search-callers (call-graph func depth)
  "In CALL-GRAPH, given FUNC, search callers deep to level DEPTH."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (short-func (cg--extract-method-name func))
             (data-mode (call-graph--data-mode call-graph)))
    (let ((caller-list (list))
          (callers (map-elt (call-graph--callers call-graph) short-func (list))))

      ;; callers not found.
      (unless callers
        (seq-doseq (reference (cg--find-references short-func))
          (when-let ((caller-info
                      (and reference (cg--find-caller reference func data-mode))))
            (message (format "Search returns: %s" (symbol-name (car caller-info))))
            (push caller-info caller-list)))
        (cg--add-callers call-graph func caller-list)
        (setq callers (map-elt (call-graph--callers call-graph) short-func (list))))

      ;; recursively search callers.
      (seq-doseq (caller callers)
        (cg--search-callers call-graph caller next-depth)))))

(defun cg--build-hierarchy (call-graph func depth)
  "In CALL-GRAPH, given FUNC, build hierarchy deep to level DEPTH.
CALCULATE-DEPTH is used to calculate actual depth."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (hierarchy cg--default-hierarchy)
             (short-func (cg--extract-method-name func))
             (callers
              (or (map-elt cg--caller-cache func (list)) ; load callers from cache
                  (map-elt (call-graph--callers call-graph) short-func (list)))))

    ;; populate hierarchy data.
    (seq-doseq (caller callers)
      ;; avoid recursive caller
      (unless (eq caller func)
        (hierarchy-add-tree hierarchy caller (lambda (item) (when (eq item caller) func)))
        (message "Insert child %s under parent %s" (symbol-name caller) (symbol-name func))))

    ;; recursively populate callers.
    (seq-doseq (caller callers)
      (cg--build-hierarchy call-graph caller next-depth))))

(defun cg--display-hierarchy ()
  "Display `call-graph' in hierarchy."
  (let ((switch-buffer (not (eq major-mode 'call-graph-mode)))
        hierarchy-buffer)
    (setq hierarchy-buffer
          (hierarchy-tree-display
           cg--default-hierarchy
           (lambda (tree-item _)
             (let ((caller (symbol-name tree-item))
                   (parent (or (hierarchy-parent cg--default-hierarchy tree-item) 'root-function)))

               ;; use propertize to avoid this error => Attempt to modify read-only object
               ;; @see https://stackoverflow.com/questions/24565068/emacs-text-is-read-only
               (insert (propertize caller 'caller-name tree-item 'callee-name parent))))
           (cg--get-buffer)))
    (when switch-buffer
      (switch-to-buffer-other-window hierarchy-buffer))
    (call-graph-mode)
    (cg-widget-expand-all)))

(defun cg--create (call-graph func depth)
  "Generate CALL-GRAPH for FUNC, DEPTH is the depth of caller-map."
  (when (and call-graph func depth)
    (setq cg--default-hierarchy (hierarchy-new))
    (cg--search-callers call-graph func depth)
    (cg--build-hierarchy call-graph func depth)
    (cg--display-hierarchy)))

(defun cg--initialize ()
  "Initialize data for `call-graph'."
  (when (or current-prefix-arg
            (not cg--default-instance)
            (not (cg--is-valid cg--default-instance)))
    (setq cg--default-instance (cg-new))) ; clear cached reference

  (when (not cg--caller-cache)
    (if cg-persist-caller-cache ; load cache from saved session
        (setq cg--caller-cache (map-into cg-persist-caller-cache 'hash-table)
              cg-persist-caller-cache nil)
      (setq cg--caller-cache (make-hash-table :test #'equal))))

  (unless (eq major-mode 'call-graph-mode) ; set mode of data
    (setf (call-graph--data-mode cg--default-instance) major-mode)))

(defun cg--dispatch-interface()
  "Dispatch interface for different language."
  (cond
   ((member major-mode '(c-mode c++-mode))
    (progn
      (defalias 'cg--extract-method-name 'cg--global-extract-method-name)
      (defalias 'cg--find-caller 'cg--global-find-caller)
      (defalias 'cg--find-references 'cg--global-find-references)
      (defalias 'cg--handle-root-function 'cg--global-handle-root-function)))
   ((equal major-mode 'python-mode)
    (progn
      (defalias 'cg--extract-method-name 'cg--python-extract-method-name)
      (defalias 'cg--find-caller 'cg--python-find-caller)
      (defalias 'cg--find-references 'cg--python-find-references)
      (defalias 'cg--handle-root-function 'cg--python-handle-root-function)))
   ((equal major-mode 'emacs-lisp-mode)
    (progn
      (defalias 'cg--extract-method-name 'cg--global-extract-method-name)
      (defalias 'cg--find-caller 'cg--global-find-caller)
      (defalias 'cg--find-references 'cg--global-find-references)
      (defalias 'cg--handle-root-function 'cg--global-handle-root-function)))))

;;;###autoload
(defun call-graph (&optional func)
  "Generate `call-graph' for FUNC / func-at-point / func-in-active-rigion.
With prefix argument, discard cached data and re-generate reference data."
  (interactive (list (and (cg--dwim-at-point) (intern (cg--dwim-at-point)))))
  (deactivate-mark)
  (when func
    (cg--initialize)
    (let ((call-graph cg--default-instance)
          (window-configuration (current-window-configuration))
          (selected-window (frame-selected-window)))

      (setq cg--previous-buffers (buffer-list))
      (cg--dispatch-interface)
      (cg--handle-root-function call-graph func)

      (save-mark-and-excursion
        (cg--create call-graph func cg-initial-max-depth)
        (setq cg--window-configuration window-configuration
              cg--selected-window selected-window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Graph Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg-visit-file-at-point ()
  "Visit occurrence on the current line."
  (when-let ((call-graph cg--default-instance)
             (callee (get-text-property (point) 'callee-name))
             (caller (get-text-property (point) 'caller-name))
             (locations (cg--get-func-caller-location call-graph callee caller))
             (location (car locations)))
    (cg--visit-function location)
    (setq cg--window-configuration (current-window-configuration)
          cg--selected-window (frame-selected-window)) ; update window configuration
    (when (> (seq-length locations) 1)
      (message "Multiple locations for this function, select with `cg-select-caller-location'"))))

(defun cg-goto-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (save-mark-and-excursion
    (when (get-char-property (point) 'button)
      (forward-char 4))
    (cg-visit-file-at-point)))

(defun cg-display-file-at-point ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (save-selected-window
    (cg-goto-file-at-point)))

(defun cg-at-point ()
  "Within buffer <*call-graph*>, generate new `call-graph' for symbol at point."
  (interactive)
  (save-mark-and-excursion
    (when (get-char-property (point) 'button)
      (forward-char 4))
    (when-let ((caller (get-text-property (point) 'caller-name)))
      (call-graph caller))))

(defun cg-select-caller-location ()
  "Select caller location as default location for function at point."
  (interactive)
  (save-mark-and-excursion
    (when (get-char-property (point) 'button)
      (forward-char 4))
    (when-let ((call-graph cg--default-instance)
               (callee (get-text-property (point) 'callee-name))
               (caller (get-text-property (point) 'caller-name))
               (func-caller-key
                (intern (concat (symbol-name (cg--extract-method-name callee)) " <- " (symbol-name caller))))
               (locations (cg--get-func-caller-location call-graph callee caller))
               (has-many (> (seq-length locations) 1)))
      (ivy-read "Caller Locations:" locations
                :action (lambda (func-location)
                          (while (not (equal func-location (car locations)))
                            (setq locations
                                  (nconc (cdr locations) (cons (car locations) ())))) ; put selected location upfront
                          (setf (map-elt (call-graph--locations call-graph) func-caller-key) locations)
                          (cg--visit-function func-location))))))

(defun cg-remove-single-caller ()
  "Within buffer <*call-graph*>, remove single caller at point."
  (when (get-char-property (point) 'button)
    (forward-char 4))
  (when-let ((call-graph cg--default-instance)
             (callee (get-text-property (point) 'callee-name))
             (caller (get-text-property (point) 'caller-name))
             (short-func (cg--extract-method-name callee))
             (callers (map-elt (call-graph--callers call-graph) short-func (list)))
             (deep-copy-of-callers (seq-map #'identity callers))
             (filters
              (or (map-elt cg--caller-cache callee deep-copy-of-callers)
                  (setf (map-elt cg--caller-cache callee) deep-copy-of-callers))))
    (unwind-protect
        (progn
          (when cg-display-file (remove-hook 'widget-move-hook 'cg-display-file-at-point)) ; disable display-file temporarly
          (tree-mode-delete-match (symbol-name caller)))
      (when cg-display-file (add-hook 'widget-move-hook 'cg-display-file-at-point))) ; restore display-file
    (setf (map-elt cg--caller-cache callee)
          (remove caller filters))))

(defun cg-remove-region-callers ()
  "Within buffer <*call-graph*>, remove callers within active region."
  (when (region-active-p)
    (deactivate-mark)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           rbeg-line rend-line line-iterator)
      (goto-char rbeg) (setq rbeg-line (line-number-at-pos))
      (goto-char rend) (setq rend-line (line-number-at-pos))
      (setq line-iterator (min rbeg-line rend-line))
      (while (<= line-iterator (max rbeg-line rend-line))
        (goto-char (point-min)) (forward-line (1- (min rbeg-line rend-line)))
        (beginning-of-line) (while (not (get-char-property (point) 'button)) (forward-char))
        (cg-remove-single-caller)
        (setq line-iterator (1+ line-iterator))))))

(defun cg-remove-caller ()
  "Within buffer <*call-graph*>, remove caller."
  (interactive)
  (if (region-active-p)
      (cg-remove-region-callers)
    (cg-remove-single-caller)))

(defun cg-reset-caller-cache ()
  "Within buffer <*call-graph*>, reset caller cache for symbol at point.
With prefix argument, discard whole caller cache."
  (interactive)
  (if current-prefix-arg
      (when (yes-or-no-p "Reset whole caller cache ?")
        (setf cg--caller-cache nil)
        (message "Reset whole caller cache done"))
    (save-mark-and-excursion
      (when (get-char-property (point) 'button)
        (forward-char 4))
      (when-let ((caller (get-text-property (point) 'caller-name)))
        (setf (map-elt cg--caller-cache caller) nil)
        (message (format "Reset caller cache for %s done" caller))))))

(defun cg-quit ()
  "Quit `call-graph'."
  (interactive)
  (when (eq major-mode 'call-graph-mode)
    (setq major-mode nil)
    (let ((configuration cg--window-configuration)
          (selected-window cg--selected-window))
      (kill-this-buffer)
      (set-window-configuration configuration)
      (select-window selected-window)
      (mapc 'kill-buffer-if-not-modified cg--created-buffers)
      (setq cg--created-buffers ()))))

(defun cg-toggle-show-func-args ()
  "Toggle show func-args for current `call-graph'."
  (interactive)
  (setq cg-display-func-args (not cg-display-func-args)
        cg--default-instance nil)
  (goto-char (point-min))
  (cg-at-point))

(defun cg-toggle-invalid-reference ()
  "Toggle ignore-invalid-reference for current `call-graph'."
  (interactive)
  (setq cg-ignore-invalid-reference (not cg-ignore-invalid-reference)
        cg--default-instance nil)
  (goto-char (point-min))
  (cg-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg-widget-expand-all ()
  "Iterate all widgets in buffer and expand em."
  (interactive)
  (tree-mode-expand-level 0))

(defun cg-widget-collapse-all ()
  "Iterate all widgets in buffer and close em."
  (interactive)
  (goto-char (point-min))
  (tree-mode-expand-level 1))

(defun cg-expand (&optional level)
  "Expand `call-graph' by LEVEL."
  (interactive "p")
  (when-let ((call-graph cg--default-instance)
             (depth (+ (cg--widget-depth) level))
             (func (cg--widget-root)))
    (cg--create call-graph func depth)))

(defun cg-collapse (&optional level)
  "Collapse `call-graph' by LEVEL."
  (interactive "p")
  (let ((level (- (cg--widget-depth) level)))
    (goto-char (point-min))
    (cond
     ((> level 0)
      (tree-mode-expand-level level))
     ((<= level 0)
      (tree-mode-expand-level 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "e") 'cg-widget-expand-all)
    (define-key map (kbd "c") 'cg-widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'cg-quit)
    (define-key map (kbd "+") 'cg-expand)
    (define-key map (kbd "_") 'cg-collapse)
    (define-key map (kbd "o") 'cg-goto-file-at-point)
    (define-key map (kbd "g") 'cg-at-point)
    (define-key map (kbd "d") 'cg-remove-caller)
    (define-key map (kbd "l") 'cg-select-caller-location)
    (define-key map (kbd "r") 'cg-reset-caller-cache)
    (define-key map (kbd "t") 'cg-toggle-show-func-args)
    (define-key map (kbd "f") 'cg-toggle-invalid-reference)
    (define-key map (kbd "<RET>") 'cg-goto-file-at-point)
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
  (when cg-display-file
    (add-hook 'widget-move-hook 'cg-display-file-at-point))
  (add-hook 'kill-emacs-hook 'cg--save-caller-cache)
  (run-mode-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @see cg-test.el


(provide 'call-graph)
;;; call-graph.el ends here
