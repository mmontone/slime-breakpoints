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

(defun slime-remove-all-breakpoints ()
  (interactive)
  (slime-eval '(breakpoints:remove-all-breakpoints))
  (message "All breakpoints removed."))

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
  (:on-load
   ;; setup key bindings
   (slime-breakpoints-setup-key-bindings)
   ;; add submenu to SLIME menu
   ;;(slime-help--add-menu-to-slime)
   (slime-breakpoints--extend-slime-menu)))


(provide 'slime-breakpoints)

;;; slime-breakpoints.el ends here
