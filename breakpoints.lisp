(defpackage :breakpoints
  (:use :cl)
  (:export
   #:break-on-entry
   #:remove-breakpoint
   #:remove-all-breakpoints))

(in-package :breakpoints)

(defvar *installed-breakpoints* (make-hash-table))

(defun break-on-entry (function-name)
  (check-type function-name symbol)
  (let ((breakpoint (gethash function-name *installed-breakpoints*)))
    (when breakpoint
      (destructuring-bind (original with-breakpoint) breakpoint
	(when (eq (symbol-function function-name)
		  with-breakpoint)
	  (return-from break-on-entry nil)))))
  (let* ((original-function (symbol-function function-name))
	 (function-with-breakpoint
	   (lambda (&rest args)
	   (break)
	   (apply original-function args))))
    (setf (symbol-function function-name)
	  function-with-breakpoint)
    (setf (gethash function-name *installed-breakpoints*)
	  (list original-function function-with-breakpoint)))
  t)

(defun remove-breakpoint (function-name)
  (let ((breakpoint (gethash function-name *installed-breakpoints*)))
    (when breakpoint
      (destructuring-bind (original with-breakpoint) breakpoint
	(when (eq (symbol-function function-name)
		  with-breakpoint)
	  (setf (symbol-function function-name) original)))
      (remhash function-name *installed-breakpoints*)
      t)))

(defun remove-all-breakpoints ()
  (loop for k being each hash-key of *installed-breakpoints*
	do (remove-breakpoint k))
  (setf *installed-breakpoints* (make-hash-table))
  t)

(defun foo (x)
  (print x))

(foo 22)

(break-on-entry 'foo)

(foo 22)

(remove-all-breakpoints)
