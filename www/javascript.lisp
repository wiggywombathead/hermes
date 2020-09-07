;;;; File to host all Javascript and AJAX functions for the server

(mapcar #'ql:quickload '(:parenscript :smackjack))

(defpackage :js
  (:use :cl :parenscript :smackjack)
  (:export :main-script
		   :nonempty-fields))

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

(defun main-script ()
  " main javascript script "
  (ps
	(set-timer)

	(defun set-timer ()
	  " set the market closing function to execute when next market expires "
	  (chain smackjack
			 (ajax-set-timer #'(lambda (response)
								 (set-timeout (lambda ()
												(chain location (reload)))
											  (* (@ response seconds) 1000))))))

	(defun display-projected-cost (response)
	  " asynchronously update projected transaction cost "
	  (setf (chain document (get-element-by-id :projected) inner-h-t-m-l)
			(@ response cost)))

	(defun ajax-transaction-cost-create ()
	  " calculate transaction cost when creating market "
	  (let ((quantity (chain document (get-element-by-id :quantity) value)))
		(chain smackjack (ajax-transaction-cost-quantity
						   quantity
						   0
						   T
						   display-projected-cost))))

	(defun ajax-transaction-cost-trade ()
	  " calculate transaction cost when trading in market "
	  (let ((quantity (chain document (get-element-by-id :quantity) value))
			(q (chain document (get-element-by-id :old-shares) value))
			(radios (chain document (get-elements-by-name "buying")))
			checked
			buying-p)

		;; find which radio button is checked
		(loop for option in radios do
			  (if (@ option checked)
				(setf checked (@ option value))))

		;; checked is equal to 1 if "buy" is selected
		(setf buying-p (equal checked 1))

		(chain smackjack (ajax-transaction-cost-quantity
						   quantity
						   q
						   buying-p
						   display-projected-cost))))))
