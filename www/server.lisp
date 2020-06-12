;;;; Webserver on which to host the prediction market

;; load the required packages
(mapcar #'ql:quickload '(:hunchentoot :cl-who :parenscript :smackjack))

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
			  (:div :id "header"
					(:img :src "/kappa.png" :alt "K")
					(:span :class "strapline" "Predict the future!"))

			  (:div :id "navbar"
					(:ul
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
			 ,(format NIL "/~(~a~)" name) ',name)
		   *dispatch-table*)))

;;; Start the server
(init-server)

(define-url-fn
  (index)
  (standard-page
	(:title "Cassie")
	(:h1 "Welcome to cassie")
	(:p "cassie is a flexible prediction market where you can bet on the
		outcome of future events!")

	(:h2 (format T "~a"
				 (if *session-user*
				   (format NIL "Hello ~a! Funds: ~$"
						   (db:user-name *session-user*)
						   (db:user-budget *session-user*))
				   "")))
	(:h2 "Markets")
	;; TODO: display active markets here

	;; create a new market
	(if *session-user*
	  (htm
		(:div :id "market-maker"
			  (:form :action "create-market" :method "POST"
					 :onsubmit (ps-inline
								 (when (or (= (getprop bet_str 'value) "")
										   (= (getprop deadline_date 'value) ""))
								   (alert "Please fill in all fields")
								   (return false)))
					 (:table
					   (:tr
						 (:td "Bet")
						 (:td (:input :type "text" :name "bet_str")))
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
	(unless (db:user-exists username)
	  (setf *session-user* (db:insert-user username))))
  (redirect "/index"))

(define-url-fn
  (login-user)
  (let ((username (parameter "username")))
	(if (db:user-exists username)
	  (setf *session-user* (db:get-user-by-name username))
	  (redirect "/login")))
  (redirect "/index"))

(define-url-fn
  (logout-user)
  (setf *session-user* NIL)
  (redirect "/index"))

(define-url-fn
  (create-market)
  (let ((bet-str (parameter "bet_str"))
		(deadline (format NIL "~a ~a"
						  (parameter "deadline_date")
						  (parameter "deadline_time")))
		(share-price (msr:share-price 0)))
	(standard-page
	  (:title "Create Market")
	  (:h1 "Create Position")
	  (:form :action "first-dibs" :method "POST"
			 (:table
			   (:tr
				 (:td "Market")
				 (:td (fmt "~A" bet-str)
					  (:input :type :hidden :name "bet_str" :value bet-str)))
			   (:tr
				 (:td "Deadline")
				 (:td (fmt "~A" deadline)
					  (:input :type :hidden :name "deadline" :value deadline)))
			   (:tr
				 (:td "Share price")
				 (:td (format T "~$" share-price)))
			   (:tr
				 (:td "Initial Position:")
				 (:td (:input :type "number" :min 1 :name "shares"))
				 (:td "shares"))
			   (:tr
				 (:td "Exposure")
				 (:td (:em "TODO")))
			   (:tr
				 (:td :colspan 3
					  (:input :type "submit" :value "Buy shares"))))))))

(define-url-fn
  (first-dibs)
  (standard-page
	(:title "Transaction")
	(:h1 "Transaction successful")
	(let* ((bet-str (parameter "bet_str"))
		   (deadline (parameter "deadline"))
		   (shares (parse-integer (parameter "shares")))
		   (budget (db:user-budget *session-user*))
		   (paid (msr:transaction-cost shares 0)))
	  (htm
		(:p (format T "You have paid ~$ for ~D shares of: \"~A\", which expires
					on ~A. Your remaining budget is ~$"
					paid shares bet-str deadline (- budget paid))))
		(db:insert-security bet-str deadline))))
