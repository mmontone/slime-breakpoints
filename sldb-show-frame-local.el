;;; slime-show-frame-local.el --- Navigation to backtrace locals from source code in SLIME.  -*- lexical-binding: t; -*-

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
    (cl-case (length sldb-buffers)
      (0 nil)
      (1 (car sldb-buffers))
      (t (completing-read "SLDB buffer: " sldb-buffers)))))

(defun slime-qualified-symbol-at-point ()
  (interactive)
  (let ((symbol-at-point (slime-symbol-at-point)))
    (when symbol-at-point
      (slime-qualify-cl-symbol-name symbol-at-point))))

(defun sldb-show-frame-local (symbol)
  "Show backtrace local for SYMBOL.

Look for the local in sldb error buffer backtrace frames.
Then show the frame details and highlight the line with the local."
  (interactive (list (slime-read-symbol-name "Navigate to backtrace local: ")))
  (let ((current-buffer (current-buffer))
        (sldb-buffer (slime-maybe-select-sldb-buffer))
        (original-pos nil)
        (local-found-p nil))
    (when sldb-buffer
      (switch-to-buffer-other-window sldb-buffer)
      (setq original-pos (point))
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
                (when (cl-equalp (cl-getf local :name) symbol)
                  ;; Local found
                  (setq local-found-p t)
                  (let ((inhibit-read-only t)
                        (inhibit-point-motion-hooks t))
                    ;; Show the frame details and goto the line of the local
                    (sldb-show-frame-details)
                    (forward-line lines)
                    ;; Highlight the line
                    (hl-line-highlight)
                    ;; Echo the value of the local
                    (message (cl-getf local :value))
                    ;; Try to prevent the message being overwritten by other
                    ;; processes (eldoc) for 2 seconds.
                    (sit-for 2)
                    ;; Exit the loop
                    (cl-return-from loop)))
                (cl-incf lines))))
          ;; Try with next frame
          (sldb-forward-frame)))
      ;; Restore position in buffer unless local was found
      (unless local-found-p
        (goto-char original-pos))
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

;; Hook for navigating to current symbol-at-point

(defvar sldb-show-frame-local--last-symbol nil
  "The last symbol visited.")

(defun sldb-show-frame-local--post-command ()
  "Function to run as post-command, that shows local-at-point in sldb error buffer."
  (when (and slime-mode
             (slime-connected-p)
             (sldb-buffers))
    (let ((symbol-at-point (slime-symbol-at-point)))
      (when (and symbol-at-point
                 (not (string= symbol-at-point sldb-show-frame-local--last-symbol)))
        (setq sldb-show-frame-local--last-symbol symbol-at-point)
        (sldb-show-frame-local symbol-at-point)))))

(defun sldb-show-frame-local-on-cursor-move ()
  "Setup sldb-show-frame-local when cursor moves."
  (add-to-list 'slime-mode-hook
               (lambda ()
                 (add-to-list 'post-command-hook #'sldb-show-frame-local--post-command))))

(provide 'sldb-show-frame-local)
;;; sldb-show-frame-local.el ends here
