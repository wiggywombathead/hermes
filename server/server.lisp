;;;; Webserver on which to host the prediction market

;; load the required packages
(mapcar #'ql:quickload '(:hunchentoot :cl-who :parenscript))

(defpackage :cassie
  (:use :cl :cl-who :hunchentoot :parenscript))

;(in-package :cassie)

(load "database.lisp")

(defparameter *web-server* NIL)
(defparameter *server-port* 8080)
(defparameter hunchentoot:*dispatch-table* NIL)

(defparameter *session-user* NIL)

;;; Server functions

(defun init-server ()
  (setf *web-server*
	  (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))

(defun start-server ()
  (hunchentoot:start *web-server*))

(defun stop-server ()
  (hunchentoot:stop *web-server*))

;;; Webpage functions

(defparameter *pages*
  (list "home" "about" "login"))

(defmacro standard-page ((&key title) &body body)
  " template for a standard webpage "
  `(cl-who:with-html-output-to-string
	 (*standard-output* nil :prologue t :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml"
			:xml\:lang "en"
			:lang "en"

			(:head
			  (:meta :http-equiv "Content-Type"
					 :content "text/html;charset=utf-8")
			  (:title ,title)
			  (:link :type "text/css"
					 :rel "stylesheet"
					 :href "/style.css"))

			(:body
			  (:div :id "header"
					(:img :src "/lambda.jpg"
						  :alt "Lambda"
						  :class "logo")
					(:span :class "strapline" "Predict the future!"))

			  (:div :id "navbar"
					(:ul
					  (dolist (page *pages*)
						(cl-who:htm
						  (:li
							(:a :href page (format T "~a" page)))))
					  (if *session-user*
						(cl-who:htm
						  (:li
							(:a :href "logout-user" "logout"))))))

			  ,@body))))

(defun index ()
  (standard-page (:title "Cassie")
				 (:h1 "Welcome to cassie")
				 (:p "cassie is a flexible prediction market where you can bet
					 on the outcome of future events!")
				 (:p (format T "~a"
							 (if *session-user*
							   (format NIL "Hello ~a! Funds: ~$"
									   (user-name *session-user*)
									   (user-budget *session-user*))
							   "")))))


;;; Start the server
(init-server)

;;; Add the index page under URI "/"
(push (hunchentoot:create-prefix-dispatcher (format NIL "/") 'index)
	  hunchentoot:*dispatch-table*)

(defmacro define-url-fn ((name) &body body)
  " creates handler NAME and pushes it to *DISPATCH-TABLE* "
  `(progn
	 ;; define the handler
	 (defun ,name ()
	   ,@body)

	 ;; add the handler to the dispatch table
	 (push (hunchentoot:create-prefix-dispatcher
			 ,(format NIL "/~(~a~)" name) ',name)
		   hunchentoot:*dispatch-table*)))

(define-url-fn (login)
			   (standard-page (:title "Login")
							  (:h1 "Existing User")
							  (:form :action "login-user" :method "POST"
									 (:p "Username"
										 (:input :type "text"
												 :name "username"))
									 (:input :type "submit"
											 :value "login"))
							  (:h1 "New User")
							  (:form :action "register-user" :method "POST"
									 (:p "Username"
										 (:input :type "text"
												 :name "username"))
									 (:input :type "submit"
											 :value "register"))))

(define-url-fn (register-user)
			   (let ((username (hunchentoot:parameter "username")))
				 (unless (user-exists username)
				   (setf *session-user* (insert-user username))))
			   (hunchentoot:redirect "/"))

(define-url-fn (login-user)
			   (let ((username (hunchentoot:parameter "username")))
				 (if (user-exists username)
				   (setf *session-user* (get-user-by-name username))
				   (hunchentoot:redirect "/login"))
				 (hunchentoot:redirect "/")))

(define-url-fn (logout-user)
			   (setf *session-user* NIL)
			   (hunchentoot:redirect "/"))

(define-url-fn (about)
			   (standard-page (:title "About")
							  (:h1 "About")
							  (:p "This is an about section")))

