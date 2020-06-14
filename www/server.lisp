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

(defparameter *web-server* NIL)
(defparameter *server-port* 8080)
(defparameter *dispatch-table* NIL)

(defparameter *session-user* NIL)

(defparameter *ajax-processor* NIL)

;; charge an extra:
;;	- fp for buys
;;  - f(1-p) for sells
;;	- 0 for liquidity/
(defconstant +trading-fee+ 0.05)

;;; Server functions

(defun init-server ()
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
					  (:li (:a :href "login" "login"))
					  (if *session-user*
						(htm
						  (:li
							(:a :href "logout-user" "logout"))))))

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

(define-url-fn
  (index)
  (standard-page
	(:title "Cassie")
	(:h1 "Welcome")

	(:h2 (format T "~A"
				 (if *session-user*
				   (format NIL "Hello ~A! Funds: ~$"
						   (db:user-name *session-user*)
						   (db:user-budget *session-user*))
				   "")))

	(:h2 "Active Markets")
	(:table :class "markets"
			(:tr
			  (:th "Bet")
			  (:th "Deadline")
			  (:th :class "price" "Price ($)"))
			(dolist (s (db:get-securities))
			  (htm
				(:tr
				  (:td (format T "~S" (db:security-bet s)))
				  (:td (format T "~A" (db:security-deadline s)))
				  (:td :class "price" (format T "~$" (msr:share-price (db:security-shares s))))
				  (if *session-user*
					(htm
					  (:td (:form :action "trade-security" :method "POST"
								  (:input :type :hidden :name "bet-id" :value (db:security-id s))
								  (:input :type :submit :value "Trade")))))))))

	;; create a new market
	(if *session-user*
	  (htm
		(:h2 "Create a Market")
		(:div :id "market-maker"
			  (:form :action "create-market" :method "POST"
					 :onsubmit (js-ensure-nonempty "" bet deadline_date)
					 (:table
					   (:tr
						 (:td "Bet")
						 (:td (:input :type "text" :name "bet")))
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
	(:p "This is an about section")))

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
  (let ((username (parameter "username")))
	(unless (db:user-exists? username)
	  (setf *session-user* (db:insert-user username))))
  (redirect "/index"))

(define-url-fn
  (login-user)
  (let ((username (parameter "username")))
	(if (db:user-exists? username)
	  (setf *session-user* (db:get-user-by-name username))
	  (redirect "/login")))
  (redirect "/index"))

(define-url-fn
  (logout-user)
  (setf *session-user* NIL)
  (redirect "/index"))

(define-url-fn
  (create-market)
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
				 (:td (format T "~$" share-price)))
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
  (standard-page
	(:title "Transaction")
	(:h1 "Transaction successful")

	(let ((bet (parameter "bet"))
		  (deadline (parameter "deadline"))
		  (shares (parse-integer (parameter "shares")))
		  (budget (db:user-budget *session-user*))
		  total-price
		  new-share-price
		  fee
		  paid
		  new-budget)

	  (setf total-price (msr:transaction-cost shares 0))
	  (setf new-share-price (msr:share-price shares))

	  (setf fee (* +trading-fee+ new-share-price))
	  (setf paid (+ total-price fee))

	  (setf new-budget (- budget paid))

	  (db:update-budget *session-user* (- paid))
	  (db:pay-bank paid)
	  
	  ;; TODO: update the bank budget

	  ;; insert the new security and record that USER now owns SECURITY
	  (let ((inserted-security (db:insert-security bet deadline shares)))
		(db:insert-user-security *session-user* inserted-security shares))

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
			(:td (format T "~$" total-price)))
		  (:tr
			(:td "Fee")
			(:td (format T "~$" fee)))
		  (:tr
			(:td "Remaining budget")
			(:td (format T "~$" new-budget))))
		(:a :href "/index" :class "button" "Return to dashboard")))))

(define-url-fn
  (trade-security)
  (standard-page
	(:title "Trade Position")

	(let* ((id (parameter "bet-id"))
		   (security (db:get-security-by-id id))
		   (outstanding-shares (db:security-shares security))
		   (share-price (msr:share-price outstanding-shares))
		   current-position)

	  (setf current-position (db:get-current-position *session-user* security))

	  (htm
		(:h1 "Trade")
		(:form :action "buy-or-sell-security" :method "POST"
			   :onsubmit (js-ensure-nonempty "Quantity cannot be empty" shares)
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
				   (:td (fmt "~A" (db:security-deadline security))))
				 (:tr
				   (:td "Share price")
				   (:td (format T "~$" share-price)))
				 (:tr
				   (:td "Current Position")
				   (:td (fmt "~D shares" current-position)))
				 (:tr
				   (:td "Quantity")
				   (:td (:input :type :number :name "shares"))
				   (:td "shares"))
				 (:tr
				   (:td :colspan 3
						(:input :type "submit" :value "Trade"))))))))))

;; TODO
;(defun transaction-summary (bet deadline shares paid fee budget-remaining))

(define-url-fn
  (buy-or-sell-security)
  (standard-page
	(:title "Transaction")
	(:h1 "Transaction successful")

	(let ((id (parameter "bet-id"))
		  (shares (parse-integer (parameter "shares")))
		  (previous-outstanding (parse-integer (parameter "previous-outstanding")))
		  (budget (db:user-budget *session-user*))
		  security
		  buying-p
		  selling-p
		  new-outstanding
		  new-share-price
		  total-price
		  current-position
		  fee
		  paid
		  new-budget)

	  ;; get the security object
	  (setf security (db:get-security-by-id id))

	  ;; is the user buying or selling shares?
	  (setf buying-p (> shares 0))
	  (setf selling-p (not buying-p))

	  (setf new-outstanding (+ previous-outstanding shares))

	  ;; total price to move number of shares from previous-outstanding to
	  ;; new-outstanding
	  (setf total-price
			(msr:transaction-cost new-outstanding previous-outstanding))

	  ;; buying this number of shares moves the price to new-share-price
	  (setf new-share-price (msr:share-price new-outstanding))

	  (setf current-position
			(db:get-current-position *session-user* security))

	  ;; set fee to 0 if liquidation transaction, otherwise fp or f(1-p)
	  (setf fee (if (not (= 0 current-position))

				  ;; if it is a liquidation transaction, set fee to 0
				  (if (or (and selling-p
							   (> current-position 0))
						  (and buying-p
							   (< current-position 0)))

					;; liquidation transaction fee
					0

					;; risk transaction fee
					(if buying-p
					  (* +trading-fee+ new-share-price)
					  (* +trading-fee+ (- 1 new-share-price))))

				  ;; we have no shares, so must be risk transaction
				  (if buying-p
					(* +trading-fee+ new-share-price)
					(* +trading-fee+ (- 1 new-share-price)))))

	  (setf paid (+ total-price fee))
	  (setf new-budget (- budget paid))

	  ;; either add new portfolio entry, or update existing one
	  (if (db:user-security-exists? *session-user* security)
		(db:update-portfolio *session-user* security shares)
		(db:add-portfolio-entry *session-user* security shares))

	  (db:update-budget *session-user* (- paid))
	  (db:update-security-shares security new-outstanding)

	  (htm
		(:h2 "Summary")
		(:table
		  (:tr
			(:td "Market")
			(:td (fmt "~S" (db:security-bet security))))
		  (:tr
			(:td "Expires")
			(:td (fmt "~A" (db:security-deadline security))))
		  (:tr
			(:td (fmt "Shares ~A" (if (> shares 0) "bought" "sold")))
			(:td (fmt "~D" (abs shares))))
		  (:tr
			(:td (fmt "Paid~A" (if (< shares 0) " (to you)" "")))
			(:td (format T "~$" (abs total-price))))
		  (:tr
			(:td "Transaction fee")
			(:td (format T "~$" fee)))
		  (:tr
			(:td "Remaining budget")
			(:td (format T "~$" new-budget))))
		(:a :href "/index" :class "button" "Return to dashboard")))))
