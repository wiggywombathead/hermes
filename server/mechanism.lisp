;;;; Implementation of crowdsourced outcome determination in prediction markets

(load "msr.lisp")

(defstruct security
  buy-price
  sell-price
  query-str
  deadline
  (shares 0))

(defstruct agent
  name
  budget
  (portfolio (make-hash-table)))

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

(defparameter s (make-security
				  :buy-price (random 10)
				  :sell-price (random 10)
				  :query-str "biden will win presidency"
				  :deadline "2020/04/20"))

(defparameter a (make-agent :name "alice" :budget 100))
