;;;; Webserver on which to host the prediction market

;; load the required packages
(mapcar #'ql:quickload '(:hunchentoot :cl-who :parenscript))

(defpackage :cassie
  (:use :cl :cl-who :hunchentoot :parenscript))

;(in-package :cassie)

;; use custom database
(load "database.lisp")

(defparameter *web-server* NIL)
(defparameter *server-port* 8080)
(defparameter hunchentoot:*dispatch-table* NIL)

(defparameter
  *database* (make-database :name "cassie"
							:tables (list (make-table :name "users")
										  (make-table :name "securities"))))

(defparameter *pages*
  (list "home" "about" "login"))

(setf *web-server*
	  (make-instance 'hunchentoot:easy-acceptor :port *server-port*))

(defmacro standard-page ((&key title) &body body)
  " template for a standard webpage "
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
									  (:html :xmlns "http://www.w3.org/1999/xhtml"
											 :xml\:lang "en"
											 :lang "en"
											 (:head
											   (:meta :http-equiv "Content-Type"
													  :content    "text/html;charset=utf-8")
											   (:title ,title)
											   (:link :type "text/css"
													  :rel "stylesheet"
													  :href "/style.css"))
											 (:body
											   (:div :id "header"
													 (:img :src "/lambda.jpg"
														   :alt "Lambda"
														   :class "logo")
													 (:span :class "strapline"
															"Predict the future!"))
											   (:div :id "navbar"
													 (dolist (page *pages*)
													   (cl-who:htm
														 (:ul
														   (:li (:a :href (format NIL "/~a" page) (format T "~a" page)))))))
											   ,@body))))
(defun index ()
  (standard-page (:title "Cassie")
				 (:h1 "Welcome to cassie")
				 (:p "cassie is a flexible prediction market where you can bet on the outcome of future events!")))

;;; add the index page under URI "/"
(push (hunchentoot:create-prefix-dispatcher
		(format NIL "/") 'index)
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
									 (:p :input :type "submit"
											 :value "register"))))

(define-url-fn (register-user)
			   ;; TODO: add user to database
			   (hunchentoot:redirect "/"))

(define-url-fn (login-user)
			   (let ((username (hunchentoot:parameter "username")))
				 (hunchentoot:redirect "/")
				 (format NIL "Hello ~a" username)))

(define-url-fn (about)
			   (standard-page (:title "About")
							  (:h1 "About")
							  (:p "This is an about section")))

(defun start ()
  (hunchentoot:start *web-server*))

(defun stop ()
  (hunchentoot:stop *web-server*))
