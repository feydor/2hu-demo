;;;; common.lisp - Common functions

(in-package :cl-user)
(defpackage :common-functions
  (:use :cl)
  (:export :print-elements-of-list
           :make-dynamic-array))
(in-package :common-functions)

(defun make-dynamic-array ()
  (make-array 1 :fill-pointer 0 :adjustable t))

(defun print-elements-of-list (list print-func)
  "Print each element of a list on a single line."
  (loop for x across list
        do (funcall print-func x))
  (format t "~%"))
