;;;; Arbitration
;;; Implements the one-over-prior and one-over-prior-with-midpoint mechanism
;;; for paying arbiters of expired markets

(load "util.lisp")

(defpackage :arb
  (:use :cl :util)
  (:export :one-over-prior))

(in-package :arb)

(defun pair-arbiters (arbiters)
  (random-pairing arbiters))

(defun one-over-prior (report-i report-j mu k)
  " pay agent I and J according to the 1/prior mechanism "
  (if (= report-i report-j)
	(if (= report-i 0)
	  (* k mu)
	  (* k (- 1 mu)))
	0))
