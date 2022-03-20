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
  (interactive (list (slime-read-symbol-name "Break on entry: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:break-on-entry (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint installed on %s entry" function-name))

(defun slime-toggle-breakpoint (function-name)
  (interactive (list (slime-read-symbol-name "Toggle breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (let ((enabled (slime-eval `(breakpoints:toggle-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))))
    (if enabled
        (message "Breakpoint installed on %s entry" function-name)
      (message "Breakpoint removed from %s" function-name))))

(defun slime-remove-breakpoint (function-name)
  (interactive (list (slime-read-symbol-name "Toggle breakpoint: ")))
  (when (not function-name)
    (error "No function name given"))
  (slime-eval `(breakpoints:remove-breakpoint (cl:read-from-string ',(slime-qualify-cl-symbol-name function-name))))
  (message "Breakpoint removed"))

(defun slime-remove-all-breakpoints ()
  (interactive)
  (slime-eval '(breakpoints:remove-all-breakpoints))
  (message "All breakpoints removed."))

(defface slime-breakpoints-button
  '((t (:box (:line-width 2 :color "dark grey") :background "light grey" :foreground "black")))
  "Face for slime-breakpoints buttons"
  :group 'slime-breakpoints-faces)

(defvar slime-breakpoints--breakpoints
  (list
   (list :name "FOO" :type :break-on-entry :enabled t)
   (list :name "BAR" :type :break-on-entry :enabled nil)
   (list :name "BAZ" :type :break-on-entry :enabled t)))

(cl-defun slime-breakpoints--update-breakpoints-buffer-contents ()
  (when (zerop (length slime-breakpoints--breakpoints))
    (insert "There are no breakpoints installed.")
    (cl-return-from slime-breakpoints--update-breakpoints-buffer-contents))
  (dolist (breakpoint slime-breakpoints--breakpoints)
    (insert-button
     (cl-getf breakpoint :name)
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
                   ;;(slime-break-on-entry (cl-getf breakpoint :name))
                   (message "enable")
                 ;;(slime-remove-breakpoint (cl-getf breakpoint :name))
                 (message "disable")))
     (cl-getf breakpoint :enabled)))
  (newline 2)
  (insert-button
   "Remove all"
   'face 'slime-breakpoints-button
   'action (lambda (button)
             (slime-remove-all-breakpoints)
             (setf slime-breakpoints--breakpoints nil)
             (slime-breakpoints--refresh-breakpoints-buffer))
   'follow-link t))

(defun slime-breakpoints--refresh-breakpoints-buffer ()
  (let ((buffer (get-buffer "*slime-breakpoints*")))
    (when buffer
      (with-current-buffer buffer
        (kill-all-local-variables)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (remove-overlays)
        (slime-breakpoints--update-breakpoints-buffer-contents)))))

(defun slime-list-breakpoints ()
  (interactive)
  (let ((buffer (get-buffer-create "*slime-breakpoints*")))
    (with-current-buffer buffer
      (slime-breakpoints--update-breakpoints-buffer-contents)
      (setq buffer-read-only t)
      (use-local-map widget-keymap)
      (widget-setup)
      (display-buffer buffer))))


(defun slime-breakpoints-setup-key-bindings ()
  (add-hook 'lisp-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c b") 'slime-break-on-entry)
              (local-set-key (kbd "C-c C-b k") 'slime-remove-breakpoint)
              (local-set-key (kbd "C-c C-b K") 'slime-remove-all-breakpoints))))

(defun slime-breakpoints--extend-slime-menu ()
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Break on entry..." slime-break-on-entry])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Remove all breakpoints" slime-remove-all-breakpoints])
  (easy-menu-add-item 'menubar-slime '("Debugging")
                      ["Toggle breakpoint at point" slime-toggle-breakpoint]))

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
