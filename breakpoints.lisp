(defpackage :breakpoints
  (:use :cl)
  (:export
   #:break-on-entry
   #:toggle-breakpoint
   #:remove-breakpoint
   #:remove-all-breakpoints
   #:reinstall-breakpoint
   #:reinstall-all-breakpoints
   #:*breakpoints*))

(in-package :breakpoints)

(defvar *breakpoints* (make-hash-table))

(defun installed-breakpoint-p (function-name)
  (let ((breakpoint (gethash function-name *breakpoints*)))
    (when breakpoint
      (destructuring-bind (&key type replaced break) breakpoint
        (declare (ignore type replaced))
        (when (eq (symbol-function function-name) break)
	  (return-from installed-breakpoint-p t))))))

(defun break-on-entry (function-name)
  (check-type function-name symbol)
  (when (installed-breakpoint-p function-name)
    (return-from break-on-entry nil))
  (let* ((original-function (symbol-function function-name))
         (function-with-break
           (lambda (&rest args)
           (break)
           (apply original-function args))))
    (setf (symbol-function function-name) function-with-break)
    (setf (gethash function-name *breakpoints*)
          (list :type :break-on-entry
                :replaced original-function
                :break function-with-break)))
  t)

(defun remove-breakpoint (function-name)
  (check-type function-name symbol)
  
  (when (not (installed-breakpoint-p function-name))
    (return-from remove-breakpoint nil))
	     
  (let ((breakpoint (gethash function-name *breakpoints*)))
    (destructuring-bind (&key type replaced break) breakpoint
      (declare (ignore type))
      (when (eq (symbol-function function-name) break)
        (setf (symbol-function function-name) replaced)))
    (remhash function-name *breakpoints*)
    t))

(defun toggle-breakpoint (function-name)
  (check-type function-name symbol)
  (if (installed-breakpoint-p function-name)
      (progn
	(remove-breakpoint function-name)
	nil)
      (progn
	(break-on-entry function-name)
	t)))

(defun remove-all-breakpoints ()
  (loop for k being each hash-key of *breakpoints*
        do (remove-breakpoint k))
  (setf *breakpoints* (make-hash-table))
  t)

(defun reinstall-breakpoint (function-name)
  (let ((breakpoint (gethash function-name *breakpoints*)))
    (when breakpoint
      (let ((break (getf breakpoint :break)))
        (when (not (eq (symbol-function function-name) break))
          (break-on-entry function-name))))))

(defun reinstall-all-breakpoints ()
  (loop for k being each hash-key of *breakpoints*
        do (reinstall-breakpoint k)))

(provide 'breakpoints)
