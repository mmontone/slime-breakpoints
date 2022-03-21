(require :breakpoints (merge-pathnames "breakpoints.lisp" *load-pathname*))

(defpackage :slime-breakpoints
  (:use :cl :breakpoints)
  (:export #:list-of-breakpoints))

(in-package :slime-breakpoints)

(defun list-of-breakpoints ()
  (loop for breakpoint-name being the hash-keys of breakpoints:*breakpoints*
	using (hash-value breakpoint)
	collect (list :name breakpoint-name
		      :type (getf breakpoint :type)
		      :enabled (breakpoints:breakpoint-installed-p breakpoint-name))))

(provide :slime-breakpoints)
