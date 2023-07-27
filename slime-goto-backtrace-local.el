;;; slime-goto-backtrace-local.el --- Navigation to backtrace locals from source code in SLIME.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Navigation to backtrace locals from source code in SLIME.

;;; Code:

(require 'slime)

(defun slime-goto-backtrace-local (symbol)
  "Go to backtrace local for SYMBOL."
  (interactive (list (slime-read-symbol-name "Navigate to backtrace local: ")))
  (print symbol))

(defun sldb-hide-all-frame-details ()
  "Hide details of all frames."
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-beginning-of-backtrace)
    (while (get-text-property (point) 'frame)
      (sldb-hide-frame-details)
      (sldb-forward-frame))))

(defun sldb-show-all-frames-details ()
  "Show details of all frames."
  (interactive)
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (sldb-beginning-of-backtrace)
    (while (get-text-property (point) 'frame)
      (sldb-show-frame-details)
      (sldb-forward-frame))))

(provide 'slime-goto-backtrace-local)
;;; slime-goto-backtrace-local.el ends here
