;;;; Implementation of market scoring rules

;;; Logarithmic Market Scoring Rule (LMSR)
;;; An agent will pay C(q')-C(q) for attaining q'-q shares of a security,
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

(defconstant +e+ 2.71828182)
(defconstant +liquidity+ 10)	; TODO: tune this parameter

(defun price (q)
  " compute quote price of stock with Q outstanding shares -- not to be used to
  calculate the price an agent must pay "
  (/ (exp (/ q +liquidity+)) (1+ (exp (/ q +liquidity+)))))

(defun cost (q)
  " computes cost for Q shares of security "
  (* +liquidity+ (log (1+ (exp (/ q +liquidity+))))))

(defun transaction-cost (q_ q)
  " compute the price to pay for (Q_ - Q) shares of security "
  (- (cost q_) (cost q)))
