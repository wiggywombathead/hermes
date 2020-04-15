;;;; Implementation of crowdsourced outcome determination in prediction markets

(load "msr.lisp")
(load "util.lisp")

(defstruct security
  buy-price
  sell-price
  query-str			; the event as a string
  deadline			; the date/time when the event will be realised
  (arbiters NIL)	; players who hold a stake in the security
  (shares 0))		; number of outstanding shares of security

(defstruct agent
  name
  budget
  (portfolio (make-hash-table)))

;;; Market Stage
;;;  set up the prediction market for an event X using a market scoring rule
;;;  agents trade in the market:
;;;	 	- for a security bought at price p, charge fp
;;;		- for a security sold at price p, charge f(1-p)
;;;	 the market closes and trading stops
(defun trade-security (agent security quantity)
  " buy or sell QUANTITY shares of SECURITY. If QUANTITY > 0 then we are buying
  shares, otherwise we are selling shares "
  (let* ((budget (agent-budget agent))
		 (portfolio (agent-portfolio agent))
		 (outstanding (security-shares security))
		 (cost (transaction-cost (+ quantity outstanding) outstanding)))

	(if (< budget cost)
	  (return-from trade-security NIL))

	(incf (security-shares security) quantity)	; update number of outstanding

	;; TODO: where is this money going from/to?
	(decf (agent-budget agent) cost)			; update agent budget

	(if (gethash security portfolio)
	  (incf (gethash security portfolio) quantity)
	  (setf (gethash security portfolio) quantity))))

;;; Arbitration Stage
;;;  each arbiter receives signal and reports the outcome of X
;;;  each arbiter is randomly assigned to another arbiter and paid according to
;;;   1/prior mechanism
;;;	 the outcome of the market and payoff of each share sold is the fraction of
;;;	 arbiter

(defun pair-arbiters (arbiters)
  (random-pairing arbiters))

(defparameter s (make-security
				  :buy-price (random 10)
				  :sell-price (random 10)
				  :query-str "biden will win presidency"
				  :deadline "2020/04/20"))

(defparameter a (make-agent :name "alice" :budget 100))
(defparameter b (make-agent :name "bob" :budget 100))
(defparameter c (make-agent :name "carol" :budget 100))
(defparameter e (make-agent :name "eve" :budget 100))

(defparameter *players* (list a b c e))
