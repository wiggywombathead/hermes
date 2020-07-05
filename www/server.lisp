;;;; Webserver on which to host the prediction market

;; load the required packages
(mapcar #'ql:quickload '(:cl-who :hunchentoot))

(defpackage :srv
  (:use :cl :cl-who :hunchentoot)
  (:export :start-server
		   :stop-server))

(in-package :srv)

(load "database.lisp")
(load "javascript.lisp")

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
					   :document-root #p"/home/tom/compsci/masters/cs907/www/")))

  ;; TODO: add this back when I figure out how
;  (setf *ajax-processor*
;		(make-instance 'ajax-processor :server-uri "/ajax")))
;
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

			  ;; fonts
			  (:link :rel "stylesheet"
					 :href "https://fonts.googleapis.com/css?family=Merriweather")

			  ;; style
			  ;(:link :rel "stylesheet" :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css" :integrity="sha384-9aIt2nRpC12Uk9gS9baDl411NQApFmC26EwAOH8WgZl5MYYxFfc+NcPb1dKGj7Sk" :crossorigin="anonymous"))

			  (:link :type "text/css"
					 :rel "stylesheet"
					 :href "/style.css")

			  (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js")
			  (:script :src "/ajax.js"))

			(:body
			  (:ul :id "navbar"
				   (:li (:img :src "img/kappa.png" :class "logo"))
				   (:li (:a :href "index" "home"))
				   (:li (:a :href "about" "about"))
				   (if (session-value 'session-user)
					 (htm
					   (:li (:a :href "portfolio" "portfolio"))
					   (:li (:a :href "logout-user" "logout")))
					 (htm
					   (:li (:a :href "login" "login")))))

			  (:div :class "container"
					(if (session-value 'session-user)
					  (htm
						(:div :id "toolbar"
							  (format T "User: ~A Funds: ~4$"
									  (db:user-name (session-value 'session-user))
									  (db:user-budget (session-value 'session-user))))))

					(:div :class "content"
						  ,@body))))))

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

;;; Start the server
(init-server)

(defun pretty-datetime (datetime)
  (local-time:format-timestring
	T datetime :format '((:hour 2)":"(:min 2)" "(:day 2)"-"(:month 2)"-":year)))

(define-url-fn
  (index)
  (start-session)
  (standard-page
	(:title "Cassie")

	(:h2 "Active Markets")
	(:table :class "striped"
			(:tr
			  (:th "Bet")
			  (:th "Deadline")
			  (:th :class "number" "Price ($)"))

			(dolist (s (db:get-active-markets (local-time:now)))
			  (htm
				(:tr
				  (:td (format T "~S" (db:security-bet s)))
				  (:td (pretty-datetime (db:security-deadline s)))
				  (:td :class "number"
					   (format T "~4$" (msr:share-price (db:security-shares s))))
				  (if (session-value 'session-user)
					(htm
					  (:td (:form :action "trade-security" :method "POST"
								  (:input :type :hidden :name "bet-id" :value (db:security-id s))
								  (:input :type :submit :value "Trade")))))))))

	(:h2 "Unresolved Markets")
	(:table :class "striped"
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
					 ;:onsubmit (js:nonempty-fields "" bet deadline_date)
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
	(:p "This is a peer prediction market. You can bet on the outcome of any
		\(binary\) event you want, such as the winner of some sports event, or
		whether or not the next US president will be a Democrat.")

;;	(:p "This is a peer prediction market where you can bet on the (binary)
;;		outcome of anything you want, such as the winner of the presidential
;;		election, or the winner of some sports event. It uses a peer-prediction
;;		mechanism to collect bets and decide on their outcome based on reports
;;		from its userbase. This opens up the types of bets that can be made
;;		since they do not have to be specifically provided by a central
;;		moderator, and it is left to the users to decide on their outcome.")
	
	(:h2 "How do I use it?")
	(:p "First you need to register. Then, your dashboard will show all of the
		current markets. Active markets are those whose deadlines have not
		passed and shares can still be bought and sold. Unresolved markets are
		those whose deadlines have passed and require users to report the
		outcome of the event.")

	(:h3 "Creating markets")
	(:p "Create a market by entering a bet and a deadline by which the outcome
		will be known. Other users will need to report their observation of the
		outcome, so bets should be made as unambiguous as possible to minimise
		the opportunity for users to interpret it differently. You are free to
		make bets as unambiguous as you like, though this may affect the final
		payout. The deadline should also be the earliest date and time you
		expect the event to have occurred, to prevent other users trading in a
		market whose outcome is known.")

	(:h3 "Trading in markets")
	(:p "Shares can be bought and sold in any of the markets listed under
		\"Active Markets\" up until the market closes, with the only
		restriction being that shares can only be bought when creating the
		market \(otherwise you should just create a market for the opposite
		outcome\). When the market expires and the outcome has been determined,
		you will be paid for any shares you own in the market and you will need
		to pay for any shares you are short. The payout of one share is the
		proportion of users who reported a positive \('Yes'\) outcome for the
		event.")
	(:p "If you buy shares in a market, you are saying you believe the event
		will have a positive outcome, since you expect people to report a
		positive outcome and hence push the payout per share towards $1, and
		hence you will receive $1 for each share you own. By similar reasoning,
		when you sell shares you are predicting the opposite.")
	(:p "As in any other market money can be made by buying low and selling
		high -- you don't have to hold onto your shares right up until the
		event occurs and risk a small payoff.")

	(:h3 "Resolving Markets")
	(:p "Markets whose deadlines have passed are listed under \"Unresolved
		Markets\". You can report its outcome based on your observation \(e.g.
		reading the news, watching the match\) and you will be rewarded for
		doing so.")
	(:p "Once enough reports on the outcome have been received, the outcome of
		the event and payoff for each share held will be the proportion of
		reporters who reported a positive outcome.")))

(define-url-fn
  (portfolio)
  (start-session)
  (standard-page
	(:title "Portfolio")
	(:h1 "My Portfolio")

	(:h2 "Active Markets")
	(:table :class "striped"
	  (:tr
		(:th "Bet")
		(:th "Deadline")
		(:th "Position")
		;; TODO: bought-at price
		(:th "Current Price"))
	  (dolist (security (db:get-portfolio-active-securities (session-value 'session-user)
															(local-time:now)))
		(htm
		  (:tr
			(:td (fmt "~S" (db:security-bet security)))
			(:td (pretty-datetime (db:security-deadline security)))
			(:td :class "number" (fmt "~D" (db:get-current-position
											 (session-value 'session-user)
											 security)))
			(:td :class "number"
				 (format T "~4$" (msr:share-price (db:security-shares security))))

			(:td (:form :action "trade-security" :method :POST
						(:input :type :hidden :name "bet-id" :value (db:security-id security))
						(:input :type :submit :value "Trade")))))))

	(:h2 "History")
	(:table :class "striped"
	  (:tr
		(:th "Bet")
		(:th "Expired On")
		(:th "Position")
		(:th "Closing Price")
		;; TODO: bought-at price
		(:th "Payout per share"))
	  (dolist (security (db:get-portfolio-expired-securities (session-value 'session-user)
															 (local-time:now)))
		(htm
		  (:tr
			(:td (fmt "~S" (db:security-bet security)))
			(:td (pretty-datetime (db:security-deadline security)))
			(:td :class "number" (fmt "~D" (db:get-current-position
											 (session-value 'session-user)
											 security)))
			(:td :class "number"
				 (format T "~4$" (msr:share-price (db:security-shares security))))
			(:td :class "number"
				 (format T "~4$" (db:security-outcome security)))))))))

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
			 ;:onsubmit (js:nonempty-fields "Quantity cannot be empty" shares)
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
				 (:td "Projected Cost")
				 (:td (:b "TODO (update asynchronously)")))
			   (:tr
				 (:td :colspan 3
					  (:input :type "submit" :value "Buy shares"))))))))

(define-url-fn
  (first-dibs)
  (start-session)
  (standard-page
	(:title "Transaction")

	(:h1 "Transaction summary")

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
			   ;:onsubmit (js:nonempty-fields "Quantity cannot be empty" shares buying)
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
		  closing-price
		  mu1-min
		  mu0-max
		  security)

	  (setf security (db:get-security-by-id id))
	  (setf closing-price (msr:share-price (db:security-shares security)))
	  (setf mu1-min (ceiling (* 100 closing-price)))
	  (setf mu0-max (1- mu1-min))

	  (htm
		(:form :action "report-security" :method "POST"
			   ;:onsubmit (js:nonempty-fields "" positive_belief negative_belief)
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
				   (:td (:input :type :radio :value 1 :checked "checked"
								:name "report") "Yes"
						(:input :type :radio :value 0
								:name "report") "No"))
				 (:tr
				   (:th "Positive Belief")
				   (:td (:input :type :number
								:step 1
								:min mu1-min
								:max 100
								:value mu1-min
								:name "positive_belief") "%"))
				 (:tr
				   (:th "Negative Belief")
				   (:td (:input :type :number
								:step 1
								:min 0
								:max mu0-max
								:value mu0-max
								:name "negative_belief") "%"))
				 (:tr
				   (:td :colspan 2
						(:input :type :submit :value "Submit")))))))))

(define-url-fn
  (report-security)
  (start-session)
  (let ((id (parameter "id"))
		(report (parameter "report"))
		(positive-belief (/ (parse-integer (parameter "positive_belief")) 100))
		(negative-belief (/ (parse-integer (parameter "negative_belief")) 100))
		(session-user (session-value 'session-user))
		security)

	(setf security (db:get-security-by-id id))
	(db:report-market-outcome session-user security report positive-belief negative-belief))

  (redirect "/index"))

(define-url-fn
  (close-market)
  (start-session)

  (let ((id (parameter "bet-id"))
		security	
		mu			; prior probability that an agent receives +ve signal
		mu1
		mu0
		arbiter-reports
		(reports-table (make-hash-table))
		arbiter-beliefs
		(beliefs-table (make-hash-table))
		positive-posteriors	; list of posteriors Pr[Sj=1|Si=1]
		negative-posteriors	; list of posteriors Pr[Sj=1|Si=0]
		arbiters			; just the list of arbiters
		pairs
		outcome)

	(setf security (db:get-security-by-id id))
	(setf mu (msr:share-price (db:security-shares security)))

	(setf arbiter-reports (db:get-arbiter-reports security))
	(setf arbiter-beliefs (db:get-arbiter-beliefs security))

	;; if there are an odd number of reports, randomly remove one
	(if (oddp (length arbiter-reports))
	  (setf arbiter-reports (util:remove-nth (random (length arbiter-reports))
											 arbiter-reports)))

	;; if there are zero reports, redirect
	;; TODO: make it obvious why the redirect occurred/nothing happened
	(if (zerop (length arbiter-reports))
	  (redirect "/index"))

	;; convert list of tuples ((user report) ...) into hashmap
	;; reports-table[user-name] = report
	(dolist (arbiter-report arbiter-reports)
	  (setf (gethash (db:user-id (first arbiter-report)) reports-table)
			(second arbiter-report)))

	;; convert list ((user positive negative) ...) into hashmap
	;; beliefs-table[user-name] = (positive negative)
	(dolist (arbiter-belief arbiter-beliefs)
	  (setf (gethash (db:user-id (first arbiter-belief)) beliefs-table)
			(rest arbiter-belief)))

	(setf arbiters (mapcar #'first arbiter-reports))

	(let ((reports (mapcar #'second arbiter-reports)))
	  ;; the payoff of each share held is the fraction of arbiters reporting 1
	  (setf outcome (float (/ (count 1 reports) (length reports)))))

	;; set the payoff for each share held in the database
	(db:set-security-outcome security outcome)

	;; pay the shareholders
	(let ((shareholder-shares (db:get-shareholder-shares security))
		  shareholder
		  shares)
	  (dolist (shareholder-share shareholder-shares)
		(setf shareholder (first shareholder-share))
		(setf shares (second shareholder-share))

		(format T "Paying ~A ~D*~4$=~4$ for ~D shares of ~A~%"
				(db:user-name shareholder)
				shares
				outcome
				(* outcome shares)
				shares
				(db:security-bet security))

		(db:bank-pay shareholder (* outcome shares))))

	;; pair arbiters randomly
	(setf pairs (util:random-pairing arbiters))

	;; for each pair:
	;;	- retrieve their report
	;;  - compute their signal (positive/negtive) posterior beliefs
	;; compute signal positive posterior mu1 and signal negative posterior mu0
	;;  - mu1: Pr[Sj=1|Si=1]
	;;	- mu0: Pr[Sj=1|Si=0]

	;; TODO: move this to separate function/interface!!
	(dolist (pair pairs)
	  (let ((i (first pair))
			(j (second pair))
			id-i
			id-j
			report-i
			report-j
			positive-belief-i
			negative-belief-i
			positive-belief-j
			negative-belief-j)

		(setf id-i (db:user-id i))
		(setf id-j (db:user-id j))

		(setf report-i (gethash id-i reports-table))
		(setf report-j (gethash id-j reports-table))

		(setf positive-belief-i (first (gethash id-i beliefs-table)))
		(setf negative-belief-i (second (gethash id-i beliefs-table)))
		(setf positive-belief-j (first (gethash id-j beliefs-table)))
		(setf negative-belief-j (second (gethash id-j beliefs-table)))

		;; signal positive posterior for player i
		(push (arb:signal-positive-posterior-i
				mu
				positive-belief-i negative-belief-i
				positive-belief-j negative-belief-j)
			  positive-posteriors)

		;; signal positive posterior for player j
		(push (arb:signal-positive-posterior-i
				mu
				positive-belief-j negative-belief-j
				positive-belief-i negative-belief-i)
			  positive-posteriors)

		;; signal negative posterior for player i
		(push (arb:signal-negative-posterior-i
				mu
				positive-belief-i negative-belief-i
				positive-belief-j negative-belief-j)
			  negative-posteriors)

		;; signal negative posterior for player j
		(push (arb:signal-negative-posterior-i
				mu
				positive-belief-j negative-belief-j
				positive-belief-i negative-belief-i)
			  negative-posteriors)))

	(setf mu1 (arb:calculate-mu1 positive-posteriors))
	(setf mu0 (arb:calculate-mu0 negative-posteriors))

	(standard-page
	  (:title "One-Over-Prior Payment")

	  (dolist (pair pairs)
		(let ((i (first pair))
			  (j (second pair))
			  id-i
			  id-j
			  report-i
			  report-j
			  arbiter-payment)

		  (setf id-i (db:user-id i))
		  (setf id-j (db:user-id j))

		  (setf report-i (gethash id-i reports-table))
		  (setf report-j (gethash id-j reports-table))

		  (setf arbiter-payment (arb:one-over-prior-midpoint
								  report-i
								  report-j
								  mu1
								  mu0))

		  ;; pay the arbiters
		  (db:bank-pay i arbiter-payment)
		  (db:bank-pay j arbiter-payment)

		  (htm
			(:p (format T "Closing share price: ~4$" mu))
			(:p (format T "mu1: ~D, mu0: ~D" mu1 mu0))
			(:p (format T "~A reported ~D, ~A reported ~D"
						(db:user-name i)
						report-i
						(db:user-name j)
						report-j))

			(:p (format T "Paying arbiters: ~4$" arbiter-payment)))))

	  (:a :href "/index" :class "button" "Return to dashboard"))))
