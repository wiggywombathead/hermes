;;;; Arbitration
;;; Implements the one-over-prior and one-over-prior-with-midpoint mechanism
;;; for paying arbiters of expired markets

(load "util.lisp")

(defpackage :arb
  (:use :cl :util)
  (:export :one-over-prior
		   :one-over-prior-midpoint
		   :calculate-positive-belief
		   :calculate-negative-belief
		   :signal-positive-posterior-i
		   :signal-negative-posterior-i
		   :calculate-k
		   :calculate-mu1
		   :calculate-mu0))

(in-package :arb)

(defun pair-arbiters (arbiters)
  (random-pairing arbiters))

(defun calculate-k (n-i m delta)
  " compute one-over-prior parameter K for arbiter with N-I shares of given
  security for which there are M arbiters and update strength (mu1-mu0) is
  DELTA "
  (/ (* 2 (abs n-i)) (* m delta)))

;; one-over-prior:
;;	- k * mu		if xi = xj = 0
;;	- k * (1 - mu)	if xi = xj = 1
;;	- 0				otherwise
(defun one-over-prior (report-i report-j k mu)
  " calculate payment for agent I and J according to their reports and the
  prior probability MU of receiving a positive signal "
  (if (= report-i report-j)
	(if (= report-i 0)
	  (* k mu)
	  (* k (- 1 mu)))
	0))

(defun one-over-prior-midpoint (report-i report-j k mu1 mu0)
  " calculate payment for agent I and J according to their reports using the
  one-over-prior-with-midpoint mechanism
  MU-1: probability that, given agent i receives positive signal, another
  randomly chosen agent receives negative signal
  MU-2: probability that, given agent i receives negative signal, another
  randomly chosen agent receives positive signal "
  (one-over-prior report-i
				  report-j
				  k
				  (/ (+ mu1 mu0) 2)))

(defun calculate-positive-belief (reporting-history)
  " calculate Pr[Si=1|X=1] give the list REPORTING-HISTORY of the form
  ((security report outcome) ...) "

  ;; first only get the reports where the outcome was positive
  (let ((positive-outcomes (remove-if-not #'(lambda (x) (>= 0.5 (third x)))
										  reporting-history))
		positive-reports)

	;; now count the number of times the user reported a positive outcome
	(setf positive-reports (count T (mapcar #'(lambda (x) (equal 1 (second x)))
											positive-outcomes)))
	(if positive-outcomes
	  (/ positive-reports (length positive-outcomes))
	  1)))

(defun calculate-negative-belief (reporting-history)
  " calculate Pr[Si=1|X=0] given the list REPORTING-HISTORY of the form
  ((security report outcome) ...)" 

  ;; first only get the reports where the outcome was negative
  (let ((negative-outcomes (remove-if-not #'(lambda (x) (< 0.5 (third x)))
										  reporting-history))
		positive-reports)

	;; now count the number of times the user reported a positive outcome
	(setf positive-reports (count T (mapcar #'(lambda (x) (equal 1 (second x)))
											negative-outcomes)))
	(if negative-outcomes
	  (/ positive-reports (length negative-outcomes))
	  0)))

(defun signal-positive-posterior-i
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

	;; Pr[Si=1]
	(setf i1 (+ (* i1x1 prior) (* i1x0 (- 1 prior))))

	;; Pr[X=1|Si=1]
	(setf x1i1 (/ (* i1x1 prior) i1))

	;; Pr[X=0|Si=1]
	(setf x0i1 (- 1 x1i1))

	;; Pr[Sj=1|Si=1] = Pr[Sj=1|X=0] Pr[X=0|Si=1] + Pr[Sj=1|X=1] Pr[X=1|Si=1]
	(+ (* j1x0 x0i1) (* j1x1 x1i1))))

(defun signal-negative-posterior-i
  (prior positive-belief-i negative-belief-i positive-belief-j negative-belief-j)
  " calculate the probability according to i's belief model that, given that
  user i has received a positive signal, another randomly chosen user j also
  received a positive signal "
  (let ((i1x0 negative-belief-i)	; Pr[Si=1|X=0]
		(i1x1 positive-belief-i)	; Pr[Si=1|X=1]
		(j1x0 negative-belief-j)	; Pr[Sj=1|X=0]
		(j1x1 positive-belief-j)	; Pr[Sj=1|X=1]
		i0x0	; Pr[Si=0|X=0] = 1 - Pr[Si=1|X=0]
		i0x1	; Pr[Si=0|X=1] = 1 - Pr[Si=1|X=1]
		i0		; Pr[Si=0]
		x0		; Pr[X=0] = 1 - Pr[X=1]
		x0i0	; Pr[X=0|Si=0]
		x1i0)	; Pr[X=1|Si=0] = 1 - Pr[X=0|Si=1]

	(setf x0 (- 1 prior))

	(setf i0x0 (- 1 i1x0))
	(setf i0x1 (- 1 i1x1))

	;; Pr[Si=0] = Pr[Si=0|X=0] Pr[X=0] + Pr[Si=0|X=1] Pr[X=1]
	(setf i0 (+ (* i0x0 x0) (* i0x1 prior)))

	;; Pr[X=0|Si=0]
	(setf x0i0 (/ (* i0x0 x0) i0))

	;; Pr[X=1|Si=0]
	(setf x1i0 (- 1 x0i0))

	;; Pr[Sj=1|Si=0] = Pr[Sj=1|X=0] Pr[X=0|Si=0] + Pr[Sj=1|X=1] Pr[X=1|Si=0]
	(+ (* j1x0 x0i0) (* j1x1 x1i0))))

(defun calculate-mu1 (positive-posteriors)
  " the signal posterior for the group of arbiters is the minimum signal
  posterior within the list of POSITIVE-POSTERIORS "
  (apply #'min positive-posteriors))

(defun calculate-mu0 (negative-posteriors)
  " the signal posterior for the group of arbiters is the minimum signal
  posterior within the list of POSITIVE-POSTERIORS "
  (apply #'max negative-posteriors))
