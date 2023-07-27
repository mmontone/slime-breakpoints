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
(require 'hl-line)

(defun slime-maybe-select-sldb-buffer ()
  "Select the SLDB buffer to work with."
  (let ((sldb-buffers (sldb-buffers)))
    (case (length sldb-buffers)
      (0 nil)
      (1 (car sldb-buffers))
      (t (completing-read "SLDB buffer: " sldb-buffers)))))

(defun slime-qualified-symbol-at-point ()
  (interactive)
  (let ((symbol-at-point (slime-symbol-at-point)))
    (when symbol-at-point
      (slime-qualify-cl-symbol-name symbol-at-point))))

(defun slime-symbol-package (symbol)
  )

(defun slime-goto-backtrace-local (symbol)
  "Go to backtrace local for SYMBOL."
  (interactive (list (slime-read-symbol-name "Navigate to backtrace local: ")))
  (let ((current-buffer (current-buffer))
        (sldb-buffer (slime-maybe-select-sldb-buffer)))
    (when sldb-buffer
      (switch-to-buffer-other-window sldb-buffer)
      (hl-line-mode)
      (sldb-hide-all-frame-details)
      (sldb-beginning-of-backtrace)
      (cl-block loop
        (while (get-text-property (point) 'frame)
          ;; frame-details has format: (START END FRAME LOCALS CATCHES)
          (cl-destructuring-bind (_start _end _frame locals _catches)
              (sldb-frame-details)
            (let ((lines 2))
              (dolist (local locals)
                (when (string= (cl-getf local :name) symbol)
                  (let ((inhibit-read-only t)
                        (inhibit-point-motion-hooks t))
                    (sldb-show-frame-details)
                    (forward-line lines)
                    (hl-line-highlight)
                    (cl-return-from loop)))
                (incf lines))))
          (sldb-forward-frame)))
      (switch-to-buffer-other-window current-buffer))))

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
