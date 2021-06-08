;;;; Implementation of market scoring rules

;;; Logarithmic Market Scoring Rule (LMSR)
;;; An agent will pay C(q')-C(q) for obtaining q'-q shares of a security,
;;; where:
;;;		C(q) = b * log(1+e^(q/b)) for some quantity q and parameter b
;;;		q  = number of outstanding shares of security before transaction
;;;		q' = number of outstanding shares of security after transaction
;;;
;;; We can quote the price of a security using:
;;;		p(q) = e^(q/b) / (1 + e^(q/b))
;;; 
;;; We do not use this value to calculate the price an agent must pay to obtain
;;; the security -- this is only the price to buy an infinitesimal amount

(defpackage :msr
  (:use :cl)
  (:export :share-price
           :transaction-cost))

(in-package :msr)

(defconstant +e+ 2.71828182)
(defconstant +liquidity+ 10)	; TODO: tune this parameter

(defun share-price (q)
  "computes share price p(q)"
  (/ (exp (/ q +liquidity+)) (1+ (exp (/ q +liquidity+)))))

(defun q-price (q)
  "computes transaction cost C(Q)"
  (* +liquidity+ (log (1+ (exp (/ q +liquidity+))))))

(defun transaction-cost (q* q)
  "compute the price to pay for (Q* - Q) shares of security"
  (- (q-price q*) (q-price q)))
