;;;; Implements market/trading side of the mechanism
;;; charge an extra:
;;;	- fp for buys
;;; - f(1-p) for sells
;;;	- 0 for liquidation transaction

(defpackage :mkt
  (:use :cl)
  (:export :calculate-fee))

(in-package :mkt)

(defconstant +trading-fee+ 0.05)

(defun calculate-fee (current-position shares share-price)
  " calculates the additional trading fee for transactions increasing risk
  (buy, sell) and liquidity transactions, where:
  CURRENT-POSITION is the number of shares currently held
  SHARES is the number of shares being bought (negative if we are selling)
  SHARE-PRICE is the price to which the share has been moved as a result of the transaction "

  (let* ((buying-p (> shares 0))
		 (selling-p (not buying-p)))

	(if (zerop current-position)

	  ;; charge on all transactions where investor increases their risk
	  (if buying-p
		(* +trading-fee+ share-price)
		(* +trading-fee+ (- 1 share-price)))

	  ;; TODO: take into account the number of shares we are liquidating
	  (if (or (and selling-p (> current-position 0) (<= (abs shares) current-position))
			  (and buying-p (< current-position 0) (<= shares (abs current-position))))

		;; liquidation transaction
		0

		;; transaction fee on increasing risk
		(if buying-p
		  (* +trading-fee+ share-price)
		  (* +trading-fee+ (- 1 share-price)))))))
