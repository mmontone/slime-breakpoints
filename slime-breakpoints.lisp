(require :breakpoints (merge-pathnames "breakpoints.lisp" *load-pathname*))

(defpackage :slime-breakpoints
  (:use :cl :breakpoints))

(in-package :slime-breakpoints)

(provide :slime-breakpoints)
