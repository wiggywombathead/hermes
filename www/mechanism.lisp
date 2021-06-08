;;;; Demonstration of trading and peer prediction mechanism

(load "msr.lisp")
(load "util.lisp")

(defstruct security
  bet-str			; the event as a string
  deadline			; the date/time when the event will be realised
  (stakers NIL)		; agents who hold shares of this security
  (arbiters NIL)	; players who hold a stake in the security
  (shares 0)		; number of outstanding shares of security
  (payout 0))		; payout price per share

(defstruct agent
  name
  budget
  (portfolio (make-hash-table))	; stores shares of securities
  (reports (make-hash-table)))	; stores reported outcome for security

(defun holds-shares? (agent security)
  "return T if AGENT holds non-zero shares of SECURITY"
  (and (gethash security (agent-portfolio agent))
       (not (eq 0 (gethash security (agent-portfolio agent))))))

;; TODO: don't treat prices as floats!

;;; Market Stage
;;;  set up the prediction market for an event X using a market scoring rule
;;;  agents trade in the market:
;;;	 	- for a security bought at price p, charge fp
;;;		- for a security sold at price p, charge f(1-p)
;;;	 the market closes and trading stops

(defconstant +trading-fee+ 0.05)	; TODO: tune this parameter?

(defun create-market (bet-str deadline)
  (make-security :bet-str bet-str :deadline deadline))

(defun trade-security (agent security quantity)
  "buy or sell QUANTITY shares of SECURITY. If QUANTITY > 0 then we are buying
   shares, otherwise we are selling shares"
  (let* ((budget (agent-budget agent))
         (portfolio (agent-portfolio agent))
         (outstanding (security-shares security))
         (stakers (security-stakers security))

         (buying-p (> quantity 0))
         (selling-p (not buying-p))

         (cost (msr:transaction-cost (+ quantity outstanding) outstanding))
         (price (msr:share-price outstanding))
         fee
         total-fee
         total-price)

    ;; can't have a "nothing" trade
    (if (eq quantity 0)
        (return-from trade-security NIL))

    (if (holds-shares? agent security)
        ;; if we are selling owned shares or buying ones already sold
        (if (or (and selling-p (> (gethash security portfolio) 0))
                (and buying-p (< (gethash security portfolio) 0)))
            (setf fee 0)
            (setf fee +trading-fee+))
        (setf fee +trading-fee+))

    (setf total-fee (* fee (if buying-p
                               price 			; charge fp
                               (- 1 price))))	; charge f(1-p)

    (setf total-price (+ cost total-fee))

    ;; can't afford the transaction
    (if (> total-price budget)
        (return-from trade-security NIL))

    (incf (security-shares security) quantity)	; update outstanding shares

    ;; TODO: where is this money going from/to?
    (decf (agent-budget agent) total-price)		; update agent budget

    (format T "~D shares traded for ~$+~$=~$"
            quantity cost total-fee total-price)

    ;; update agent's portfolio
    (if (gethash security portfolio)
        (incf (gethash security portfolio) quantity)
        (setf (gethash security portfolio) quantity))

    ;; update security's stakers field
    (if (holds-shares? agent security)
        (unless (member agent stakers)
          (setf (security-stakers security) (cons agent stakers)))
        (setf (security-stakers security) (remove agent stakers)))))

;;; Arbitration Stage
;;;  each arbiter receives signal and reports the outcome of X
;;;  each arbiter is randomly assigned to another arbiter and paid according to
;;;   1/prior mechanism
;;;	 the outcome of the market and payoff of each share sold is the fraction of
;;;	 arbiter

(defun pair-arbiters (arbiters)
  (random-pairing arbiters))

(defun one-over-prior (i j security mu k)
  "pay agent I and J according to the one over prior rule"
  (let ((i-report (gethash security (agent-reports i)))
        (j-report (gethash security (agent-reports j)))
        paid)

    ;; determine amount to pay based on 1/prior mechanism
    (setf paid (if (eq i-report j-report)
                   (if (= i-report 0)
                       (* k mu)			; x_i = x_j = 0
                       (* k (- 1 mu)))	; x_i = x_j = 1
                   0))

    ;; pay the agents
    (incf (agent-budget i) paid)
    (incf (agent-budget j) paid)))

(defparameter s
  (create-market
    "the weather will be nice on 21/07/2020"
    "2020/07/22-12:00:00"))

(defparameter r
  (create-market
    "the next president will be a democrat"
    "2020/11/22-12:00:00"))

(defparameter a (make-agent :name "alice" :budget 100))
(defparameter b (make-agent :name "bob" :budget 100))
(defparameter c (make-agent :name "carol" :budget 100))
(defparameter e (make-agent :name "eve" :budget 100))

(defparameter *players* (list a b c e))
