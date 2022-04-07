(defpackage :breakpoints
  (:use :cl)
  (:export
   #:break-on-entry
   #:step-on-entry
   #:toggle-breakpoint
   #:remove-breakpoint
   #:remove-all-breakpoints
   #:reinstall-breakpoint
   #:reinstall-all-breakpoints
   #:disable-breakpoint
   #:disable-all-breakpoints
   #:breakpoint-installed-p
   #:*breakpoints*))

(in-package :breakpoints)

(defvar *breakpoints* (make-hash-table))

(defun breakpoint-installed-p (function-name)
  "Wether a breakpoint is installed on FUNCTION-NAME."
  (let ((breakpoint (gethash function-name *breakpoints*)))
    (when breakpoint
      (destructuring-bind (&key type replaced break) breakpoint
        (declare (ignore type replaced))
        (when (eq (symbol-function function-name) break)
          (return-from breakpoint-installed-p t))))))

(defun break-on-entry (function-name)
  "Setup a breakpoint on entry on FUNCTION-NAME."
  (check-type function-name symbol)

  ;; First remove any breakpoints installed on FUNCTION-NAME, if any
  (remove-breakpoint function-name)
  
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

(defun step-on-entry (function-name)
  "Start stepping when function named FUNCTION-NAME is invoked."
  (check-type function-name symbol)
  
  ;; First remove any breakpoints installed on FUNCTION-NAME, if any
  (remove-breakpoint function-name)
  
  (let* ((original-function (symbol-function function-name))
         (function-with-step
           (lambda (&rest args)
           (step
            (apply original-function args)))))
    (setf (symbol-function function-name) function-with-step)
    (setf (gethash function-name *breakpoints*)
          (list :type :step-on-entry
                :replaced original-function
                :break function-with-step)))
  t)

(defun remove-breakpoint (function-name)
  "Remove breakpoint on FUNCTION-NAME."
  (check-type function-name symbol)

  (when (not (breakpoint-installed-p function-name))
    (return-from remove-breakpoint nil))

  (let ((breakpoint (gethash function-name *breakpoints*)))
    (destructuring-bind (&key type replaced break) breakpoint
      (declare (ignore type))
      (when (eq (symbol-function function-name) break)
        (setf (symbol-function function-name) replaced)))
    (remhash function-name *breakpoints*)
    t))

(defun disable-breakpoint (function-name)
  "Disable breakpoint on FUNCTION-NAME.
The breakpoint remains in the list of breakpoints."
  (check-type function-name symbol)

  (when (not (breakpoint-installed-p function-name))
    (return-from disable-breakpoint nil))

  (let ((breakpoint (gethash function-name *breakpoints*)))
    (destructuring-bind (&key type replaced break) breakpoint
      (declare (ignore type))
      (when (eq (symbol-function function-name) break)
        (setf (symbol-function function-name) replaced)))
    t))

(defun toggle-breakpoint (function-name)
  "Toggle breakpoint on FUNCTION-NAME."
  (check-type function-name symbol)
  (if (breakpoint-installed-p function-name)
      (progn
        (remove-breakpoint function-name)
        nil)
      (progn
        (break-on-entry function-name)
        t)))

(defun remove-all-breakpoints ()
  "Remove all installed breakpoints."
  (loop for k being each hash-key of *breakpoints*
        do (remove-breakpoint k))
  (setf *breakpoints* (make-hash-table))
  t)

(defun disable-all-breakpoints ()
  "Disable all installed breakpoints."
  (loop for k being each hash-key of *breakpoints*
        do (disable-breakpoint k))
  t)

(defun reinstall-breakpoint (function-name)
  "Reinstall breakpoint on FUNCTION-NAME.

When a function is recompiled, the breakpoint is lost. A call to this function reinstalls the breakpoint."
  (let ((breakpoint (gethash function-name *breakpoints*)))
    (when breakpoint
      (let ((break (getf breakpoint :break)))
        (when (not (eq (symbol-function function-name) break))
          (break-on-entry function-name))))))

(defun reinstall-all-breakpoints ()
  "Reinstall all breakpoints.

When a function is recompiled, the breakpoint is lost. A call to this function reintalls all breakpoints."
  (loop for k being each hash-key of *breakpoints*
        do (reinstall-breakpoint k)))

(provide :breakpoints)
