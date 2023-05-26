;;; sldb-source-eval.el --- SLIME debugger (SLDB) extension that adds debugger context based evaluation directly from Lisp source buffers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

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

;; SLIME debugger (SLDB) extension that adds debugger context based evaluation
;; directly from Lisp source buffers.

;; Usage:

;; When SLIME debugger (SLDB) opens, move cursor to a backtrace frame,
;; and press letter `v' for navigating to the frame source.
;; Use C-x C-e to evaluate expressions in the source buffer using the backtrace frame as context.

;;; Code:

(require 'slime)

(defvar-local sldb-source-eval-frame-number nil
  "The SLDB frame number attached to the current buffer.")
(defvar-local sldb-source-eval-mode-enabled nil
  "Use internally to indicate that SLDB-SOURCE-EVAL-MODE is enabled in the local buffer.")

(defun sldb-source-eval-show-frame-source (frame-number)
  "Show source for FRAME-NUMBER."
  ;; keep slime-current-thread for later assignment
  (let ((current-thread slime-current-thread))
    (slime-eval-async
        `(swank:frame-source-location ,frame-number)
      (lambda (source-location)
        (slime-dcase source-location
          ((:error message)
           (message "%s" message)
           (ding))
          (t
           (slime-show-source-location source-location t nil)
           ;; slime-show-source-location sets the buffer with the source to current-buffer
           (setq-local sldb-source-eval-frame-number frame-number)
           ;; assign the current-thread to new buffer
           (setq-local slime-current-thread current-thread)
           ;; enable sldb-source-eval-mode
           (sldb-source-eval-mode 1)))))))

(defun sldb-source-eval-goto-frame-source (frame-number)
  "Show source for FRAME-NUMBER."
  ;; keep slime-current-thread for later assignment
  (let ((current-thread slime-current-thread))
    (slime-eval-async
        `(swank:frame-source-location ,frame-number)
      (lambda (source-location)
        (slime-dcase source-location
          ((:error message)
           (message "%s" message)
           (ding))
          (t
           (slime-show-source-location source-location t nil)
           ;; slime-show-source-location sets the buffer with the source to current-buffer
           (setq-local sldb-source-eval-frame-number frame-number)
           ;; assign the current-thread to new buffer
           (setq-local slime-current-thread current-thread)
           ;; enabl sldb-source-eval-mode
           (sldb-source-eval-mode 1)
           ;; navigate to the buffer
           (switch-to-buffer-other-window (current-buffer))
           ))))))

(defun sldb-source-eval-eval-last-expression ()
  "Prompt for an expression and evaluate it in the selected frame."
  (interactive)
  (let ((package (slime-current-package))
        (expr (slime-last-expression))
        (frame sldb-source-eval-frame-number))
    (slime-eval-async `(swank:eval-string-in-frame ,expr ,frame ,(upcase package))
      (if current-prefix-arg
          'slime-write-string
        'slime-display-eval-result))))

(defun sldb-source-eval-inspect (string)
  "Prompt for an expression and inspect it in the current debugger frame."
  (interactive (list (slime-read-from-minibuffer
                      "Inspect in frame (evaluated): "
                      (slime-sexp-at-point))))
  (slime-eval-async `(swank:inspect-in-frame ,string ,sldb-source-eval-frame-number)
    'slime-open-inspector))

(defun sldb-source-eval-show-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-source-eval-show-frame-source (sldb-frame-number-at-point)))

(defun sldb-source-eval-goto-source ()
  "Highlight the frame at point's expression in a source code buffer."
  (interactive)
  (sldb-source-eval-goto-frame-source (sldb-frame-number-at-point)))

(defvar sldb-source-eval-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-x C-e") 'sldb-source-eval-eval-last-expression)
    (define-key map (kbd "C-c I") 'sldb-source-eval-inspect)
    (define-key map (kbd "C-c C-r") 'sldb-source-eval-eval-region)
    map))

(define-minor-mode sldb-source-eval-mode
  "SLDB evaluation minor mode."
  :lighter " sldb-source-eval"
  :keymap sldb-source-eval-mode-map
  (setq-local sldb-source-eval-mode-enabled t))

(defun sldb-source-eval-quit ()
  "Disable sldb-source-eval mode in all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when sldb-source-eval-mode-enabled
        (sldb-source-eval-mode -1)
        (setq-local sldb-source-eval-mode-enabled nil)
        (setq-local slime-current-thread t)))))

;; When user quits sldb buffer, disable sldb-source-eval mode from all buffers.
(advice-add 'sldb-quit :after #'sldb-source-eval-quit)
(advice-add 'sldb-invoke-restart :after #'sldb-source-eval-quit)
(advice-add 'sldb-abort :after #'sldb-source-eval-quit)
(advice-add 'sldb-continue :after #'sldb-source-eval-quit)

;; Replace default sldb-mode show-source behaviour.
;; Redirect to a buffer with sldb-source-eval mode enabled.
(advice-add 'sldb-show-source :override 'sldb-source-eval-show-source)

(provide 'sldb-source-eval)
;;; sldb-source-eval.el ends here
