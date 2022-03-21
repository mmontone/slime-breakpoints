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

(defun slime-break-on-entry (function-name)
  "Install breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Break on entry: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:break-on-entry (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint installed on %s entry" function-name)
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-toggle-breakpoint (function-name)
  "Toggle breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Toggle breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (let ((enabled (slime-eval `(breakpoints:toggle-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))))
    (if enabled
        (message "Breakpoint installed on %s entry" function-name)
      (message "Breakpoint removed from %s" function-name)))
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-remove-breakpoint (function-name)
  "Remove breakpoint on FUNCTION-NAME."
  (interactive (list (slime-read-symbol-name "Remove breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:remove-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint removed")
  (slime-breakpoints--refresh-breakpoints-buffer))

(defun slime-remove-all-breakpoints ()
  "Remove all breakpoints."
  (interactive)
  (slime-eval '(breakpoints:remove-all-breakpoints))
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
       'action (lambda (button)
		 (slime-edit-definition (cl-getf breakpoint :name)))
       'follow-link t)
      (indent-to-column 60)
      (widget-create
       'toggle
       :on "[Enabled]"
       :off "[Disabled]"
       :notify (lambda (wid &rest ignore)
		 (if (widget-value wid)
                     (slime-break-on-entry (cl-getf breakpoint :name))
                   (slime-disable-breakpoint (cl-getf breakpoint :name))))
       (cl-getf breakpoint :enabled)))
    (newline 2)
    (insert-button
     "Remove all"
     'face 'slime-breakpoints-button
     'action (lambda (button)
               (slime-remove-all-breakpoints))
     'follow-link t)
    (insert " ")
    (insert-button
     "Reinstall all"
     'face 'slime-breakpoints-button
     'action (lambda (button)
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

(defvar slime-breakpoints-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slime-break-on-entry)
    (define-key map (kbd "SPC") #'slime-toggle-breakpoint)
    (define-key map (kbd "<deletechar>") #'slime-remove-breakpoint)
    (define-key map (kbd "q") #'slime-remove-all-breakpoints)
    (define-key map (kbd "l") #'slime-list-breakpoints)
    map))

(fset 'slime-breakpoints-command-map slime-breakpoints-command-map)

(defun slime-breakpoints-setup-key-bindings ()
  (add-hook 'lisp-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-c b") 'slime-breakpoints-command-map))))

(defun slime-breakpoints--extend-slime-menu ()
  (easy-menu-add-item 'menubar-slime '("Debugging") "---")
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Break on entry..." slime-break-on-entry])
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
                      ["List breakpoints" slime-list-breakpoints]))

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
