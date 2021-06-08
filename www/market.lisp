;;;; Implements market/trading side of the mechanism
;;; charge an extra:
;;;	- fp for buys
;;; - f(1-p) for sells
;;;	- 0 for liquidation transaction

(defpackage :mkt
  (:use :cl)
  (:export :calculate-fee
           :sufficient-funds?))

(in-package :mkt)

(defconstant +trading-fee+ 0.05)

(defun calculate-fee (current-position shares share-price)
  "calculates the additional trading fee for transactions increasing risk
   (buy, sell) and liquidity transactions, where:
   CURRENT-POSITION is the number of shares currently held
   SHARES is the number of shares being bought (negative if we are selling)
   SHARE-PRICE is the price to which the share has been moved as a result of the transaction"

  (let* ((buying-p (plusp shares))
         (selling-p (not buying-p)))

    (if (zerop current-position)

        ;; charge on all transactions where investor increases their risk
        (if buying-p
            (* +trading-fee+ share-price)
            (* +trading-fee+ (- 1 share-price)))

        (if (or (and selling-p (plusp current-position) (<= (abs shares) current-position))
                (and buying-p (minusp current-position) (<= shares (abs current-position))))

            ;; liquidation transaction
            0

            ;; transaction fee on increasing risk
            (if buying-p
                (* +trading-fee+ share-price)
                (* +trading-fee+ (- 1 share-price)))))))

(defun sufficient-funds? (budget amount shares fee)
  "returns T if there are sufficient funds to make transaction"
  ;; returns:
  ;; - T : if we are buying shares and BUDGET > AMOUNT
  ;; - T : if we are selling shares we own (FEE == 0)
  ;; - T : if we are shorting shares and BUDGET + AMOUNT - SHARES > 0
  ;; - NIL : otherwise

  (if (plusp shares)

      ;; buy shares only if we have enough to cover cost and fee
      (> budget amount)

      ;; short shares only if we can cover if they lose i.e. we must buy them
      ;; back at a price of $1 per share
      (or (plusp (+ budget (abs amount) shares)) (zerop fee))))
