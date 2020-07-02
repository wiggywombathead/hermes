;;;; File to host all Javascript and AJAX functions for the server

(mapcar #'ql:quickload '(:parenscript :smackjack))

(defpackage :js
  (:use :cl :parenscript :smackjack)
  (:export :nonempty-fields))

(in-package :js)

(defun make-nonempty-check (field)
  `(equal (getprop ,field 'value) ""))

(defun make-nonempty-list (fields)
  (loop while fields
		collecting (make-nonempty-check (pop fields))))

(defmacro nonempty-fields (msg &rest fields)
  `(ps-inline
	 (when (or ,@(make-nonempty-list fields))
	   (if (equal ,msg "")
		 (alert "Please fill in all required fields")
		 (alert ,msg))
	   (return false))))
