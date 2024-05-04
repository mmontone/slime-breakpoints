;;; slime-breakpoints.el --- Setup breaks on functions from Emacs/SLIME      -*- lexical-binding: t -*-

;; Copyright (C) 2022 Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/slime-breakpoints
;; Keywords: help, lisp, slime, common-lisp
;; Version: 0.1
;; Package-Requires: ((emacs "25"))

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

;; slime-breakpoints is a SLIME extension that lets you setup breaks on functions and manage them.

;;; Code:

(require 'slime)

(defgroup slime-breakpoints nil
  "SLIME breakpoints."
  :group 'slime)

(defcustom slime-breakpoints-display-fringe-indicators nil
  "When enabled, display indicators for break on the Emacs fringe."
  :type 'boolean
  :group 'slime-breakpoints)

(defvar slime-breakpoints--fringe-indicators (make-hash-table :test 'equal))

(defun slime-breakpoints--display-break-indicator (function-name)
  "Display indicator on the fringe for break on FUNCTION-NAME."
  (let ((break-indicator (make-overlay (point) (point))))
    (overlay-put break-indicator
                 'before-string
                 (propertize "x" 'display
                             (list 'left-fringe
                                   'filled-square 'error)))
    (puthash function-name break-indicator slime-breakpoints--fringe-indicators)))

(defun slime-breakpoints--delete-break-indicator (function-name)
  "Remove display of break indicator on FUNCTION-NAME."
  (let ((break-indicator (gethash function-name slime-breakpoints--fringe-indicators)))
    (when break-indicator
      (remhash function-name slime-breakpoints--fringe-indicators)
      (delete-overlay break-indicator))))

(defun slime-breakpoints--set-fringe-indicator (function-name enable)
  "Toggle fringe indicator on FUNCTION-NAME following ENABLE."
  (when slime-breakpoints-display-fringe-indicators
    (slime-edit-definition function-name)
    (if enable
        (slime-breakpoints--display-break-indicator function-name)
      (slime-breakpoints--delete-break-indicator function-name))))

(defun slime-breakpoints--delete-all-fringe-indicators ()
  "Delete all fringe indicators."
  (dolist (overlay (hash-table-values slime-breakpoints--fringe-indicators))
    (delete-overlay overlay))
  (setq slime-breakpoints--fringe-indicators (make-hash-table :test 'equal)))

(defun slime-break-on-entry (function-name)
  "Install breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Break on entry: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-remove-breakpoint function-name)
  (slime-breakpoints--set-fringe-indicator function-name t)
  (slime-eval `(breakpoints:break-on-entry (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint installed on %s entry" function-name)
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-step-on-entry (function-name)
  "Start stepping when FUNCTION-NAME is called."
  (interactive (list (slime-read-symbol-name "Step on entry: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-remove-breakpoint function-name)
  (slime-breakpoints--set-fringe-indicator function-name t)
  (slime-eval `(breakpoints:step-on-entry (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint installed on %s entry" function-name)
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-toggle-breakpoint (function-name)
  "Toggle breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Toggle breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (let ((installed (slime-eval `(breakpoints:breakpoint-installed-p (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))))
    (when (not installed)
      (slime-breakpoints--set-fringe-indicator function-name t))
    (slime-eval `(breakpoints:toggle-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
    (when installed
      (slime-breakpoints--set-fringe-indicator function-name nil))
    (if installed
        (message "Breakpoint removed from %s" function-name)
      (message "Breakpoint installed on %s entry" function-name))
    (slime-breakpoints--refresh-breakpoints-buffer)))

(defun slime-remove-breakpoint (function-name)
  "Remove breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Remove breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:remove-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint removed")
  (slime-breakpoints--delete-break-indicator function-name)
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-remove-all-breakpoints ()
  "Remove all breakpoints."
  (interactive)
  (slime-eval '(breakpoints:remove-all-breakpoints))
  (slime-breakpoints--delete-all-fringe-indicators)
  (message "All breakpoints removed.")
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-disable-breakpoint (function-name)
  "Disable breakpoint on FUNCTION-NAME.
The breakpoint remains in the list of breakpoints."
  (interactive (list (slime-read-symbol-name "Disable breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:disable-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint disabled")
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-disable-all-breakpoints ()
  "Disable all breakpoints."
  (interactive)
  (slime-eval '(breakpoints:disable-all-breakpoints))
  (message "All breakpoints disabled.")
  (slime-breakpoints--delete-all-fringe-indicators)
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-reinstall-breakpoint (function-name)
  "Reinstall breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Reinstall breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:reinstall-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint reinstalled")
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-reinstall-all-breakpoints ()
  "Reinstall all breakpoints."
  (interactive)
  (slime-eval '(breakpoints:reinstall-all-breakpoints))
  (message "All breakpoints reinstalled.")
  (slime-breakpoints--refresh-breakpoints-buffer))

(defface slime-breakpoints-button
  '((t (:box (:line-width 2 :color "dark grey") :background "light grey" :foreground "black")))
  "Face for slime-breakpoints buttons"
  :group 'slime-breakpoints-faces)

(defun slime-breakpoints--breakpoints-list ()
  (slime-eval `(slime-breakpoints:list-of-breakpoints)))

(cl-defun slime-breakpoints--update-breakpoints-buffer-contents ()
  (let ((breakpoints-list (slime-breakpoints--breakpoints-list)))
    (when (zerop (length breakpoints-list))
      (insert "There are no breakpoints installed.")
      (cl-return-from slime-breakpoints--update-breakpoints-buffer-contents))
    (dolist (breakpoint breakpoints-list)
      (insert-button
       (symbol-name (cl-getf breakpoint :name))
       'action (lambda (_)
                 (slime-edit-definition (slime-qualify-cl-symbol-name (cl-getf breakpoint :name))))
       'follow-link t)
      (indent-to-column 60)
      (widget-create
       'toggle
       :on "[Enabled]"
       :off "[Disabled]"
       :notify (lambda (_ &rest _)
                 (slime-toggle-breakpoint (slime-qualify-cl-symbol-name (cl-getf breakpoint :name))))
       (cl-getf breakpoint :enabled)))
    (newline 2)
    (insert-button
     "Remove all"
     'face 'slime-breakpoints-button
     'action (lambda (_)
               (slime-remove-all-breakpoints))
     'follow-link t)
    (insert " ")
    (insert-button
     "Reinstall all"
     'face 'slime-breakpoints-button
     'action (lambda (_)
               (slime-reinstall-all-breakpoints))
     'follow-link t)
    (widget-setup)
    ))

(defun slime-breakpoints--refresh-breakpoints-buffer ()
  (let ((buffer (get-buffer "*slime-breakpoints*")))
    (when buffer
      (with-current-buffer buffer
        (let ((buffer-read-only nil))
          (erase-buffer)
          (slime-breakpoints--update-breakpoints-buffer-contents))))))

(defun slime-list-breakpoints ()
  "Open a buffer that list the current installed breakpoints."
  (interactive)
  (if (get-buffer "*slime-breakpoints*")
      (switch-to-buffer "*slime-breakpoints*")
    (let ((buffer (get-buffer-create "*slime-breakpoints*")))
      (with-current-buffer buffer
        (slime-breakpoints--update-breakpoints-buffer-contents)
        (setq buffer-read-only t)
        (use-local-map widget-keymap)
        (local-set-key (kbd "q") 'kill-buffer)
        (display-buffer buffer)))))

;; -- Navigation

;; This advices slime-list-definition function so that original source locations of breakpointed functions are followed correctly.

(defun slime-breakpoints--find-definitions (function-name)
  (slime-eval `(cl:getf (breakpoints:find-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name)))
                        :definitions)))


(defun slime-breakpoints--find-definitions-advice (find-defs name)
  (or (slime-breakpoints--find-definitions name)
      (funcall find-defs name)))

(advice-add 'slime-find-definitions :around 'slime-breakpoints--find-definitions-advice)

;; -- Additional useful debugging commands

(defun slime-last-expression-with-positions ()
  "Return the last expression and start and end positions, in a list."
  (let (start
        (end (point)))
    (save-excursion
      (backward-sexp)
      (setq start (point)))
    (list
     (buffer-substring-no-properties start end)
     start end)))

(defun slime-region-for-last-expression ()
  "Return the region for last expression."
  (let (start
        (end (point)))
    (save-excursion
      (backward-sexp)
      (setq start (point)))
    (list start end)))

(defun slime-expression-next-expression ()
  "Return the next Lisp expression at point."
  (buffer-substring-no-properties
   (point)
   (save-excursion (forward-sexp) (point))))

(defun slime-region-for-next-expression ()
  "Return the region for next expression."
  (let ((start (point))
        end)
    (save-excursion
      (forward-sexp)
      (setq end (point)))
    (list start end)))

(defun slime-next-expression-with-positions ()
  "Return the next expression and start and end positions, in a list."
  (let ((start (point))
        end)
    (save-excursion
      (forward-sexp)
      (setq end (point)))
    (list
     (buffer-substring-no-properties start end)
     start end)))

(defun slime--wrap-last-expression (wrapper)
  "Wrap the expression at point.
WRAPPER is a function that takes the expression at point string,
and is expected to return a wrapped version."
  (cl-destructuring-bind (last-expression exp-start exp-end)
      (slime-last-expression-with-positions)
    (cl-destructuring-bind (defun-start defun-end)
        (slime-region-for-defun-at-point)
      ;; Remove the expression from the function source
      (let* ((start-region (buffer-substring-no-properties defun-start exp-start))
             (end-region (buffer-substring-no-properties exp-end defun-end))
             (wrapped-expression (funcall wrapper last-expression))
             (wrapped-defun-source (concat start-region wrapped-expression end-region)))
        wrapped-defun-source))))

(defun slime--async-compile-string (string start-offset &optional cont)
  (let* ((line (save-excursion
                 (goto-char start-offset)
                 (list (line-number-at-pos) (1+ (current-column)))))
         (position `((:position ,start-offset) (:line ,@line))))
    (slime-eval-async
        `(swank:compile-string-for-emacs
          ,string
          ,(buffer-name)
          ',position
          ,(if (buffer-file-name) (slime-to-lisp-filename (buffer-file-name)))
          ',slime-compilation-policy)
      (or cont #'slime-compilation-finished))))

(defun slime-wrap-next-expression (wrapper)
  "Wrap the expression at point.
WRAPPER is a function that takes the expression at point string,
and is expected to return a wrapped version."
  (cl-destructuring-bind (next-expression exp-start exp-end)
      (slime-next-expression-with-positions)
    (cl-destructuring-bind (defun-start defun-end)
        (slime-region-for-defun-at-point)
      ;; Remove the expression from the function source
      (let* ((start-region (buffer-substring-no-properties defun-start exp-start))
             (end-region (buffer-substring-no-properties exp-end defun-end))
             (wrapped-expression (funcall wrapper next-expression))
             (wrapped-defun-source (concat start-region wrapped-expression end-region)))
        wrapped-defun-source))))

(defun slime-break-with-last-expression (datum)
  "Compile function at point with a BREAK at last expression position.
DATUM is the string passed as first argument to CL:BREAK function.
The CL:BREAK function is invoked with last expression value as second argument.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff."
  (interactive (list (read-string "Datum: " "~s")))
  (let ((source-with-break
         (slime--wrap-last-expression
          (lambda (exp)
            (let* ((inner-break-exp (format "(cl:break \"%s\" %s)" datum exp))
                   (break-exp (format "(cl:prog1 %s %s)" exp inner-break-exp)))
              (display-message-or-buffer (format "Break installed with: %s" inner-break-exp))
              (view-echo-area-messages)
              break-exp)))))
    (slime-compile-string source-with-break 0)))

(defun slime-insert-break-at-point ()
  "Compile function at point with a BREAK at cursor position.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff."
  (interactive)
  (cl-destructuring-bind (defun-start defun-end)
      (slime-region-for-defun-at-point)
    (let* ((start-region (buffer-substring-no-properties defun-start (point)))
           (end-region (buffer-substring-no-properties (point) defun-end))
           (wrapped-defun-source (concat start-region " (CL:BREAK) " end-region)))
      (display-message-or-buffer (format "Break inserted: %s" wrapped-defun-source))
      (view-echo-area-messages)
      (slime-compile-string wrapped-defun-source 0))))

(defun slime-trace-last-expression (datum)
  "Compile function at point with a 'trace' expression at last expression position.
DATUM is the string passed to CL:FORMAT as control-string.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff."
  (interactive (list (read-string "Datum: " "~s~%")))
  (let ((source-with-trace
         (slime--wrap-last-expression
          (lambda (exp)
            (format "(cl:prog1 %s (cl:format cl:t \"%s\" %s))" exp datum exp)))))
    (slime-compile-string source-with-trace 0)))

(defun slime-step-in-last-expression ()
  "Compile function at point with a 'step' expression at last expression position.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff."
  (interactive)
  (let ((source-with-step
         (slime--wrap-last-expression
          (lambda (exp)
            (format "(cl:step %s)" exp)))))
    (slime-compile-string source-with-step 0)))

(defun slime-step-in-next-expression ()
  "Compile function at point with a 'step' expression at next expression position.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff."
  (interactive)
  (let ((source-with-step
         (slime-wrap-next-expression
          (lambda (exp)
            (format "(cl:step %s)" exp)))))
    (slime-compile-string source-with-step 0)))

(defun slime-debug-print-next-expression ()
  "Instrument next expression to be debug printed when evaluated.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff.
Requires cl-debug-print."
  (interactive)
  (slime-eval '(cl:require :cl-debug-print))
  (let ((source-with-print
         (slime-wrap-next-expression
          (lambda (exp)
            (display-message-or-buffer (format "Instrumented for debug printing: %s" exp))
            (view-echo-area-messages)
            (format "(debug-print:debug-print '%s %s)" exp exp)))))
    (slime-compile-string source-with-print 0)))

(defun slime-debug-print-last-expression ()
  "Instrument last expression to be debug printed when evaluated.
The function at point is compiled with the extra debugging code.
Use `slime-compile-defun' on the function source code to recompile without the debugging stuff.
Requires cl-debug-print."
  (interactive)
  (slime-eval '(cl:require :cl-debug-print))
  (let ((source-with-print
         (slime--wrap-last-expression
          (lambda (exp)
            (display-message-or-buffer (format "Instrumented for debug printing: %s" exp))
            (view-echo-area-messages)
            (format "(debug-print:debug-print '%s %s)" exp exp)))))
    (slime-compile-string source-with-print 0)))

(defvar slime-breakpoints-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slime-break-on-entry)
    (define-key map (kbd "SPC") #'slime-toggle-breakpoint)
    (define-key map (kbd "<deletechar>") #'slime-remove-breakpoint)
    (define-key map (kbd "s") #'slime-step-on-entry)
    (define-key map (kbd "q") #'slime-remove-all-breakpoints)
    (define-key map (kbd "l") #'slime-list-breakpoints)
    map))

(fset 'slime-breakpoints-command-map slime-breakpoints-command-map)

(defun slime-breakpoints-setup-key-bindings ()
  "Setup key bindings for `slime-breakpoints' package."
  (add-hook 'lisp-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c b") 'slime-breakpoints-command-map))))

(defun slime-breakpoints--extend-slime-menu ()
  "Extend slime debugging menu."
  (easy-menu-add-item 'menubar-slime '("Debugging") "---")
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Break on entry..." slime-break-on-entry])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Break with last expression" slime-break-with-last-expression])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Insert break at point" slime-insert-break-at-point])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Toggle breakpoint at point" slime-toggle-breakpoint])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Remove breakpoint at point" slime-remove-breakpoint])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Disable breakpoint at point" slime-disable-breakpoint])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Disable all breakpoints" slime-disable-all-breakpoints])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Remove all breakpoints" slime-remove-all-breakpoints])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["List breakpoints" slime-list-breakpoints])
  (easy-menu-add-item 'menubar-slime '("Debugging") "---")
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Trace last expression" slime-trace-last-expression])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Step on entry..." slime-step-on-entry])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Step in last expression..." slime-step-in-last-expression])
  (easy-menu-add-item 'menubar-slime '("Debugging") "---")
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Debug print last expression" slime-debug-print-last-expression])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Debug print next expression" slime-debug-print-next-expression]))

(define-slime-contrib slime-breakpoints
  "Breakpoints management extension for SLIME."
  (:authors "Mariano Montone")
  (:license "GPL")
  (:swank-dependencies slime-breakpoints)
  (:on-load
   ;; setup key bindings
   (slime-breakpoints-setup-key-bindings)
   ;; add menu items
   (slime-breakpoints--extend-slime-menu)))


(provide 'slime-breakpoints)

;;; slime-breakpoints.el ends here
