;;;; Collection of utility functions for various operations on lists

(defpackage :util
  (:use :cl)
  (:export :random-pairing))

(in-package :util)

(defun random-range (high &optional (low 0))
  " returns a random number in the range [low,high) "
  (+ low (random (- high low))))

(defun generate-derangement (lst)
  " generates random permutation from LST where no element appears in its
  original position (a derangement) "
  (let* ((n (length lst))
		 (arr (make-array n :initial-contents lst))
		 (mark (make-array n :initial-element NIL))
		 (D (make-array (1+ n)))
		 (i (1- n))
		 (u n)
		 j p)

	;; initialise D using recurrence relation
	(setf (aref D 0) 1)
	(setf (aref D 1) 0)
	(loop for i from 2 to n do
		  (setf (aref D i) (* (1- i) (+ (aref D (- i 1)) (aref D (- i 2))))))

	(loop while (>= u 2) do
		  (if (not (aref mark i))
			(progn
			  (loop do
					(setf j (random i))
					until (not (aref mark j)))
			  (rotatef (aref arr i) (aref arr j))
			  (setf p (random 1.0))
			  (if (< p (/ (* (1- u) (aref D (- u 2))) (aref D u)))
				(progn
				  (setf (aref mark j) T)
				  (decf u)))
			  (decf u)))
		  (decf i))

	;; return the derangement as a list
	(coerce arr 'list)))

(defun shuffle (lst)
  " shuffle LST randomly (without modifying it) "
  (let ((lst (copy-list lst)))
	(loop for i from (length lst) downto 2 do
		  (rotatef (elt lst (random i))
				   (elt lst (1- i))))
	lst))

(defun random-pairing (lst)
  " randomly pair items from LST together if length(lst) is even "
  (if (evenp (length lst))
	(loop for (a b) on (shuffle lst) by #'cddr while b
		  collect (list a b))))
