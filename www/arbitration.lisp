;;;; Arbitration
;;; Implements the one-over-prior and one-over-prior-with-midpoint mechanism
;;; for paying arbiters of expired markets

(load "util.lisp")

(defpackage :arb
  (:use :cl :util)
  (:export :one-over-prior))

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
