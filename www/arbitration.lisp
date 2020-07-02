;;;; Arbitration
;;; Implements the one-over-prior and one-over-prior-with-midpoint mechanism
;;; for paying arbiters of expired markets

(load "util.lisp")

(defpackage :arb
  (:use :cl :util)
  (:export :one-over-prior
		   :one-over-prior-midpoint
		   :i-posterior))

(in-package :arb)

(defconstant +k+ 10)

(defun pair-arbiters (arbiters)
  (random-pairing arbiters))

;; one-over-prior:
;;	- k * mu		if xi = xj = 0
;;	- k * (1 - mu)	if xi = xj = 1
;;	- 0				otherwise
(defun one-over-prior (report-i report-j mu)
  " calculate payment for agent I and J according to their reports and the
  prior probability MU of receiving a positive signal "
  (if (= report-i report-j)
	(if (= report-i 0)
	  (* +k+ mu)
	  (* +k+ (- 1 mu)))
	0))

(defun one-over-prior-midpoint (report-i report-j mu-1 mu-0)
  " calculate payment for agent I and J according to their reports using the
  one-over-prior-with-midpoint mechanism
  MU-1: probability that, given agent i receives positive signal, another
  randomly chosen agent receives negative signal
  MU-2: probability that, given agent i receives negative signal, another
  randomly chosen agent receives positive signal "
  (one-over-prior report-i
				  report-j
				  (/ (+ mu-1 mu-0) 2)))

(defun i-posterior
  (prior positive-belief-i negative-belief-i positive-belief-j negative-belief-j)
  " calculate the probability according to i's belief model that, given that
  user i has received a positive signal, another randomly chosen user j also
  received a positive signal "
  (let ((i1x0 negative-belief-i)	; Pr[Si=1|X=0]
		(i1x1 positive-belief-i)	; Pr[Si=1|X=1]
		(j1x0 negative-belief-j)	; Pr[Sj=1|X=0]
		(j1x1 positive-belief-j)	; Pr[Sj=1|X=1]
		i1		; Pr[Si=1]
		x0i1	; Pr[X=0|Si=1]
		x1i1)	; Pr[X=1|Si=1] = 1 - Pr[X=0|Si=1]

	;; Pr[s=1]
	(setf i1 (+ (* i1x1 prior) (* i1x0 (- 1 prior))))

	;; Pr[X=1|s=1]
	(setf x1i1 (/ (* i1x1 prior) i1))

	;; Pr[X=0|s=1]
	(setf x0i1 (- 1 x1i1))

	(+ (* j1x0 x0i1) (* j1x1 x1i1))))
