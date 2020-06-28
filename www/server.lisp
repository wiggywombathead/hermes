;;;; Webserver on which to host the prediction market

;; load the required packages
(mapcar #'ql:quickload '(:cl-who :hunchentoot :parenscript :smackjack))

(defpackage :srv
  (:use :cl :cl-who :hunchentoot :parenscript :smackjack)
  (:export :start-server
		   :stop-server))

(in-package :srv)

(load "database.lisp")
(load "msr.lisp")
(load "market.lisp")
(load "arbitration.lisp")

(defparameter *web-server* NIL)
(defparameter *server-port* 8080)
(defparameter *dispatch-table* NIL)

(defparameter *ajax-processor* NIL)

;;; Server functions

(defun init-server ()

  ;; initialise the database (set the BANKER)
  (db:init-database)

  (setf *web-server*
		(make-instance 'easy-acceptor
					   :name 'cassie
					   :port *server-port*
					   :document-root #p"/home/tom/compsci/masters/cs907/www/"))

  (setf *ajax-processor*
		(make-instance 'ajax-processor :server-uri "/ajax")))

  ;(push (create-ajax-dispatcher *ajax-processor*) *dispatch-table*)) 

(defun start-server ()
  (start *web-server*))

(defun stop-server ()
  (stop *web-server*))

;;; Webpage functions

(defmacro standard-page ((&key title) &body body)
  " template for a standard webpage "
  `(with-html-output-to-string
	 (*standard-output* nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml"
			:xml\:lang "en"
			:lang "en"

			(:head
			  (:title ,title)

			  (:meta :http-equiv "Content-Type"
					 :content "text/html;charset=utf-8")

			  (:link :type "text/css"
					 :rel "stylesheet"
					 :href "/style.css"))

			(:body
			  ; (:div :id "header" (:img :src "img/kappa.png" :alt "K") (:span
			  ; :class "strapline" "Predict the future!"))

			  (:div :id "navbar"
					(:ul
					  (:li (:img :src "img/kappa.png" :class "logo"))
					  (:li (:a :href "index" "home"))
					  (:li (:a :href "about" "about"))
					  (if (session-value 'session-user)
						(htm
						  (:li (:a :href "portfolio" "portfolio"))
						  (:li (:a :href "logout-user" "logout")))
						(htm
						  (:li (:a :href "login" "login"))))))

			  ,@body))))

(defmacro define-url-fn ((name) &body body)
  " creates handler NAME and pushes it to *DISPATCH-TABLE* "
  `(progn
	 ;; define the handler
	 (defun ,name ()
	   ,@body)

	 ;; add the handler to the dispatch table
	 (push (create-prefix-dispatcher
			 ,(format NIL "/~(~A~)" name) ',name)
		   *dispatch-table*)))

;; TODO: point / to /index

(defun make-nonempty-check (field)
  `(equal (getprop ,field 'value) ""))

(defun make-nonempty-list (fields)
  (loop while fields
		collecting (make-nonempty-check (pop fields))))

(defmacro js-ensure-nonempty (msg &rest fields)
  `(ps-inline
	 (when (or ,@(make-nonempty-list fields))
	   (if (equal ,msg "")
		 (alert "Please fill in all required fields")
		 (alert ,msg))
	   (return false))))

;;; Start the server
(init-server)

(defun pretty-datetime (datetime)
  (local-time:format-timestring T datetime
								:format '((:hour 2)":"(:min 2)" "(:day 2)"-"(:month 2)"-":year)))

(define-url-fn
  (index)
  (start-session)
  (standard-page
	(:title "Cassie")

	(:h1 "Welcome")
	(:h2 (format T "~A"
				 (if (session-value 'session-user)
				   (format NIL "Hello ~A! Funds: ~4$"
						   (db:user-name (session-value 'session-user))
						   (db:user-budget (session-value 'session-user)))
				   "")))

	(:h2 "Active Markets")
	(:table :class "markets"
			(:tr
			  (:th "Bet")
			  (:th "Deadline")
			  (:th :class "number" "Price ($)"))

			(dolist (s (db:get-active-markets (local-time:now)))
			  (htm
				(:tr
				  (:td (format T "~S" (db:security-bet s)))
				  (:td (pretty-datetime (db:security-deadline s)))
				  (:td :class "number" (format T "~4$" (msr:share-price (db:security-shares s))))
				  (if (session-value 'session-user)
					(htm
					  (:td (:form :action "trade-security" :method "POST"
								  (:input :type :hidden :name "bet-id" :value (db:security-id s))
								  (:input :type :submit :value "Trade")))))))))

	(:h2 "Unresolved Markets")
	(:table :class "markets"
			(:tr
			  (:th "Bet")
			  (:th "Expired on")
			  (:th "Closing price"))

			(dolist (s (db:get-unresolved-markets (local-time:now)))
			  (htm
				(:tr
				  (:td (format T "~S" (db:security-bet s)))
				  (:td (pretty-datetime (db:security-deadline s)))
				  (:td :class "number" (format T "~4$" (msr:share-price (db:security-shares s))))

				  (if (session-value 'session-user)
					(htm
					  (:td (:form :action "resolve-market" :method "POST"
								  (:input :type :hidden :name "bet-id" :value (db:security-id s))
								  (:input :type :submit :value "Report Outcome")))
					  (:td (:form :action "close-market" :method "POST"
								  (:input :type :hidden :name "bet-id" :value (db:security-id s))
								  (:input :type :submit :value "Close Market")))))))))

	;; create a new market
	(if (session-value 'session-user)
	  (htm
		(:h2 "Create a Market")
		(:div :id "market-maker"
			  (:form :action "create-market" :method "POST"
					 :onsubmit (js-ensure-nonempty "" bet deadline_date)
					 (:table
					   (:tr
						 (:td "Bet")
						 (:td :colspan 2 (:input :type "text" :name "bet")))
					   (:tr
						 (:td "Deadline")
						 (:td (:input :type "date" :name "deadline_date"))
						 (:td (:input :type "time" :name "deadline_time")))
					   (:tr
						 (:td (:input :type "submit" :value "Create market"))))))))))

(define-url-fn
  (about)
  (standard-page
	(:title "About")
	(:h1 "About")

	(:h2 "What is this?")
	(:p "This is a peer prediction market where you can bet on the (binary)
		outcome of anything you want, such as the winner of the presidential
		election, or who will win the next football match between Arsenal and
		Tottenham Hotspur. It uses a peer-prediction mechanism to collect bets
		and decide on their outcome based on reports from its userbase. This
		opens up the types of bets that can be made since they do not have to
		be specifically provided by a central moderator, and it is left to the
		users to decide on their outcome.")
	
	(:h2 "How do I use it?")
	(:p "After registering, the dashboard will show all of the current markets.
		Active markets are those where shares can still be bought and sold,
		while unresolved markets are those whose deadlines have passed but
		still require users to report on the outcome of the bet.")

	(:p "Any user is able to create their own market by simply providing a bet
		and the deadline by which you believe the outcome will be realised. For
		example, if betting on the outcome of a single football match, the bet
		would be who wins and the deadline will be the time at which the match
		finishes. Since other people will be reporting on what the outcome is,
		you should try to make your bet as unambiguous as possible. After
		creating the market it will then be possible to trade shares in it up
		until the deadline.")

	(:p "When there are markets listed under \"Unresolved Markets\", you can
		report its outcome based on what you observed \(e.g. by reading the
		news, watching the match, or otherwise hearing about it\). Since
		reporting on a market helps the mechanism to work, you will be given a
		small monetary reward for doing so. This incentivises users to act
		truthfully, meaning users will be worse off if they lie in an attempt
		to swing the outcome in their favour.")

	(:p "Once the outcome of the market has been peer-determined you will
		receive a payout for any shares you hold in it. The payout-per-share is
		the fraction of reporters that said that the outcome indeed occurred.
		For example, if eight uers report a market as 'Yes' and two reporters
		report a 'No', then for each share you own you will be paid 40p. This
		works for short-selling shares as well, in which case you will need to
		buy back that amount of shares at their payout price \(meaning ideally
		0 reporters report a 'Yes' and hence all report 'No'\).")))

(define-url-fn
  (portfolio)
  (start-session)
  (standard-page
	(:title "Portfolio")
	(:h1 "My Portfolio")
	(:table
	  (:tr
		(:th "Bet")
		(:th "Deadline")
		(:th "Current Price")
		;; TODO: bought-at price
		(:th "Shares"))
	  (dolist (security (db:get-portfolio-securities (session-value 'session-user)))
		(htm
		  (:tr
			(:td (fmt "~S" (db:security-bet security)))
			(:td (pretty-datetime (db:security-deadline security)))
			(:td :class "number"
				 (format T "~4$" (msr:share-price (db:security-shares security))))
			(:td :class "number"
				 (fmt "~D" (db:get-current-position (session-value 'session-user)
													security)))
			(:td (:form :action "trade-security" :method :POST
						(:input :type :hidden :name "bet-id" :value (db:security-id security))
						(:input :type :submit :value "Trade")))))))))

(define-url-fn
  (login)
  (standard-page
	(:title "Login")

	(:h1 "Existing User")
	(:form :action "login-user" :method "POST"
		   (:table
			 (:tr
			   (:td "Username")
			   (:td (:input :type "text" :name "username")))
			 (:tr
			   (:td :colspan 2 (:input :type "submit" :value "login")))))

	(:h1 "New User")
	(:form :action "register-user" :method "POST"
		   (:table
			 (:tr
			   (:td "Username")
			   (:td (:input :type "text" :name "username")))
			 (:tr
			   (:td :colspan 2 (:input :type "submit" :value "register")))))))

(define-url-fn
  (register-user)
  (start-session)
  (let ((username (parameter "username")))
	(unless (db:user-exists? username)
	  (setf (session-value 'session-user) (db:insert-user username))))
  (redirect "/index"))

(define-url-fn
  (login-user)
  (start-session)
  (let ((username (parameter "username")))
	(if (db:user-exists? username)
	  (setf (session-value 'session-user) (db:get-user-by-name username))
	  (redirect "/login")))
  (redirect "/index"))

(define-url-fn
  (logout-user)
  (start-session)
  (delete-session-value 'session-user)
  (redirect "/index"))

(define-url-fn
  (create-market)
  (start-session)
  (let ((bet (parameter "bet"))
		(deadline (format NIL "~A ~A"
						  (parameter "deadline_date")
						  (parameter "deadline_time")))
		(share-price (msr:share-price 0)))

	(standard-page
	  (:title "Create Market")
	  (:h1 "Create Position")
	  (:form :action "first-dibs" :method "POST"
			 :onsubmit (js-ensure-nonempty "Quantity cannot be empty" shares)
			 (:table
			   (:tr
				 (:td "Market")
				 (:td (fmt "~A" bet)
					  (:input :type :hidden :name "bet" :value bet)))
			   (:tr
				 (:td "Deadline")
				 (:td (fmt "~A" deadline)
					  (:input :type :hidden :name "deadline" :value deadline)))
			   (:tr
				 (:td "Share price")
				 (:td (format T "~4$" share-price)))
			   (:tr
				 (:td "Initial Position")
				 (:td (:input :type "number" :min 1 :value 1 :name "shares"))
				 (:td "shares"))
			   (:tr
				 (:td "Exposure")
				 (:td (:b "TODO (update asynchronously)")))
			   (:tr
				 (:td :colspan 3
					  (:input :type "submit" :value "Buy shares"))))))))

(define-url-fn
  (first-dibs)
  (start-session)
  (standard-page
	(:title "Transaction")

	(:h1 "Transaction successful")

	(let ((bet (parameter "bet"))
		  (deadline (parameter "deadline"))
		  (shares (parse-integer (parameter "shares")))
		  (session-user (session-value 'session-user))
		  budget
		  total-price
		  new-share-price
		  fee
		  paid
		  new-budget)

	  (setf budget (db:user-budget session-user))

	  (setf total-price (msr:transaction-cost shares 0))
	  (setf new-share-price (msr:share-price shares))

	  (setf fee (mkt:calculate-fee 0 shares new-share-price))

	  (setf paid (+ total-price fee))

	  (setf new-budget (- budget paid))

	  ;; update the current user's budget and transfer to the bank
	  (db:pay-bank session-user paid)

	  ;; insert the new security and record that USER now owns SECURITY
	  (let ((inserted-security (db:insert-security bet deadline shares)))
		(db:insert-user-security
		  session-user
		  inserted-security
		  shares))

	  (htm
		(:h2 "Summary")
		(:table
		  (:tr
			(:td "Market")
			(:td (fmt "~S" bet)))
		  (:tr
			(:td "Expires")
			(:td (fmt "~A" deadline)))
		  (:tr
			(:td "Shares bought")
			(:td (fmt "~D" shares)))
		  (:tr
			(:td "Total price")
			(:td (format T "~4$" total-price)))
		  (:tr
			(:td "Fee")
			(:td (format T "~4$" fee)))
		  (:tr
			(:td "Remaining budget")
			(:td (format T "~4$" new-budget))))
		(:a :href "/index" :class "button" "Return to dashboard")))))

(define-url-fn
  (trade-security)
  (start-session)
  (standard-page
	(:title "Trade Position")

	(let* ((id (parameter "bet-id"))
		   (security (db:get-security-by-id id))
		   (outstanding-shares (db:security-shares security))
		   (share-price (msr:share-price outstanding-shares))
		   (session-user (session-value 'session-user))
		   current-position)

	  (setf current-position (db:get-current-position session-user security))

	  (htm
		(:h1 "Trade")
		(:form :action "buy-or-sell-security" :method "POST"
			   :onsubmit (js-ensure-nonempty "Quantity cannot be empty" shares buying)
			   (:table
				 (:input :type :hidden :name "bet-id" :value id)

				 ;; previous quantity of outstanding shares to calculate price
				 (:input :type :hidden
						 :name "previous-outstanding"
						 :value outstanding-shares)
				 (:tr
				   (:td "Market")
				   (:td (fmt "~A" (db:security-bet security)))
				 (:tr
				   (:td "Deadline")
				   (:td (pretty-datetime (db:security-deadline security))))
				 (:tr
				   (:td "Share price")
				   (:td (format T "~4$" share-price)))
				 (:tr
				   (:td "Current Position")
				   (:td (fmt "~D shares" current-position)))
				 (:tr
				   (:td "Quantity")
				   (:td (:input :type :number :min 1 :value 1 :name "shares"))
				   (:td "shares"))
				 (:tr
				   (:td "Buy/Sell")
				   (:td (:input :type :radio :value 1 :name "buying" :checked "checked") "Buy" (:br)
						(:input :type :radio :value 0 :name "buying") "Sell"))
				 (:tr
				   (:td :colspan 3
						(:input :type "submit" :value "Trade"))))))))))

;; TODO
;(defun transaction-summary (bet deadline shares paid fee budget-remaining))

(define-url-fn
  (buy-or-sell-security)
  (start-session)
  (standard-page
	(:title "Transaction")
	(:h1 "Transaction successful")

	;; TODO: move most of this to separate file/interface?
	(let ((id (parameter "bet-id"))
		  (buying-p (= 1 (parse-integer (parameter "buying"))))
		  (shares (parse-integer (parameter "shares")))
		  (previous-outstanding (parse-integer (parameter "previous-outstanding")))
		  (session-user (session-value 'session-user))
		  budget
		  security
		  new-outstanding
		  new-share-price
		  total-price
		  current-position
		  fee
		  paid
		  new-budget)

	  (setf budget (db:user-budget session-user))

	  ;; get the security object
	  (setf security (db:get-security-by-id id))

	  ;; is the user buying or selling shares?
	  (if (not buying-p)
		(setf shares (- shares)))

	  (setf new-outstanding (+ previous-outstanding shares))

	  ;; total price to move number of shares from previous-outstanding to
	  ;; new-outstanding
	  (setf total-price
			(msr:transaction-cost new-outstanding previous-outstanding))

	  ;; buying this number of shares moves the price to new-share-price
	  (setf new-share-price (msr:share-price new-outstanding))

	  (setf current-position
			(db:get-current-position session-user security))

	  ;; set fee to 0 if liquidation transaction, otherwise fp or f(1-p)
	  (setf fee (mkt:calculate-fee current-position shares new-share-price))

	  (setf paid (+ total-price fee))
	  (setf new-budget (- budget paid))

	  ;; either add new portfolio entry, or update existing one
	  (if (db:user-security-exists? session-user security)
		(db:update-portfolio session-user security shares)
		(db:add-portfolio-entry session-user security shares))

	  ;; update the current user's budget and transfer to the bank
	  (db:pay-bank session-user paid)

	  (db:update-security-shares security new-outstanding)

	  (htm
		(:h2 "Summary")
		(:table
		  (:tr
			(:td "Market")
			(:td (fmt "~S" (db:security-bet security))))
		  (:tr
			(:td "Expires")
			(:td (pretty-datetime (db:security-deadline security))))
		  (:tr
			(:td (fmt "Shares ~A" (if (> shares 0) "bought" "sold")))
			(:td (fmt "~D" (abs shares))))
		  (:tr
			(:td (fmt "Paid~A" (if (< shares 0) " (to you)" "")))
			(:td (format T "~4$" (abs total-price))))
		  (:tr
			(:td "Transaction fee")
			(:td (format T "~4$" fee)))
		  (:tr
			(:td "Remaining budget")
			(:td (format T "~4$" new-budget))))
		(:a :href "/index" :class "button" "Return to dashboard")))))

(define-url-fn
  (resolve-market)
  (start-session)
  (standard-page
	(:title "Resolve Security")
	(:h1 "Resolve Security")

	(let ((id (parameter "bet-id"))
		  security)

	  (setf security (db:get-security-by-id id))

	  (htm
		(:form :action "report-security" :method "POST"
			   (:input :type :hidden :name "id" :value id)
			   (:table
				 (:tr
				   (:th "Bet")
				   (:td (fmt "~S" (db:security-bet security))))
				 (:tr
				   (:th "Expired on")
				   (:td (pretty-datetime (db:security-deadline security))))
				 (:tr
				   (:th "Market Outcome")
				   (:td (:input :type :radio :name "report" :value 1) "Yes"
						(:input :type :radio :name "report" :value 0) "No"))
				 (:tr
				   (:td :colspan 2
						(:input :type :submit :value "Submit")))))))))

(define-url-fn
  (report-security)
  (start-session)
  (let ((id (parameter "id"))
		(report (parameter "report"))
		(session-user (session-value 'session-user))
		security)

	(setf security (db:get-security-by-id id))

	(db:report-market-outcome session-user security report))
  (redirect "/index"))

(define-url-fn
  (close-market)
  (start-session)

  (let ((id (parameter "bet-id"))
		security	
		mu			; prior probability that an agent receives +ve signal
		arbiter-reports
		(reports-table (make-hash-table))
		arbiters	; just the list of arbiters
		reports		; just the list of reports
		pairs
		outcome)

	(setf security (db:get-security-by-id id))
	(setf mu (msr:share-price (db:security-shares security)))

	(setf arbiter-reports (db:get-arbiter-reports security))

	;; if there are an odd number of reports, randomly remove one
	(if (oddp (length arbiter-reports))
	  (setf arbiter-reports (util:remove-nth (random (length arbiter-reports))
											 arbiter-reports)))

	;; if there are zero reports, redirect
	;; TODO: make it obvious why the redirect occurred/nothing happened
	(if (zerop (length arbiter-reports))
	  (redirect "/index"))

	;; convert list of tuples ((user report) ...) into hashmap
	(dolist (arbiter-report arbiter-reports)
	  (setf (gethash (first arbiter-report) reports-table) (second arbiter-report)))

	(setf arbiters (mapcar #'first arbiter-reports))
	(setf reports (mapcar #'second arbiter-reports))

	;; the payoff of each share held is the fraction of arbiters reporting 1
	(setf outcome (float (/ (count 1 reports) (length reports))))

	;; set the payoff for each share held in the database
	(db:set-security-outcome security outcome)

	;; pay the shareholders
	(let ((shareholder-shares (db:get-shareholder-shares security))
		  shareholder
		  shares)
	  (dolist (shareholder-share shareholder-shares)
		(setf shareholder (first shareholder-share))
		(setf shares (second shareholder-share))

		(format T "Paying ~A ~D*~4$=~4$ for ~D shares of ~A"
				(db:user-name shareholder)
				shares
				outcome
				(* outcome shares)
				shares
				(db:security-bet security))

		(db:bank-pay shareholder (* outcome shares))))

	;; pair arbiters randomly
	(setf pairs (util:random-pairing arbiters))

	(standard-page
	  (:title "One-Over-Prior Payment")

	  (:p (format T "Closing share price: ~4$" mu))

	  (dolist (pair pairs)
		(let ((i (first pair))
			  (j (second pair))
			  report-i
			  report-j
			  payment)

		  (setf report-i (gethash i reports-table))
		  (setf report-j (gethash j reports-table))

		  ;; TODO: finish/verify
		  (setf payment (arb:one-over-prior report-i report-j mu))

		  ;; pay the arbiters
		  (db:bank-pay i payment)
		  (db:bank-pay j payment)

		  (htm
			(:p (format T "~A reported ~D, ~A reported ~D"
						(db:user-name i)
						report-i
						(db:user-name j)
						report-j))
			(:p (format T "Paying arbiters: ~4$" payment)))))
	  
	  (:a :href "/index" :class "button" "Return to dashboard"))))
