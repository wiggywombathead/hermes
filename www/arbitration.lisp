
(load "database.lisp")
(load "util.lisp")

(defpackage :arb
  (:use :cl :util)
  (:export :one-over-prior))

(in-package :arb)

(defun pair-arbiters (arbiters)
  (random-pairing arbiters))

(defun one-over-prior (user-i report-i user-j report-j mu k)
  " pay agent I and J according to the 1/prior mechanism "
  (let ((paid (if (= report-i report-j)
				(if (= report-i 0)
				  (* k mu)
				  (* k (- 1 mu)))
				0)))
	(db:bank-pay user-i paid)
	(db:bank-pay user-j paid)))
