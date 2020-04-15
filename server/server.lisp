;;;; Webserver on which to host the prediction market

;; load the required packages
(mapcar #'ql:quickload '(:hunchentoot :cl-who :parenscript))

; (defpackage :hermes
;   (:use :cl :cl-who :hunchentoot :parenscript))
; (in-package :hermes)

(defparameter *web-server* NIL)
(defparameter *server-port* 8080)

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
										,@body))))

(defmacro define-url-fn ((name) &body body)
  " creates handler NAME and pushes it to *DISPATCH-TABLE* "
  `(progn
	 ;; define the handler
	 (defun ,name ()
	   ,@body)

	 ;; add the handler to the dispatch table
	 (push (hunchentoot:create-prefix-dispatcher
			 ,(format NIL "/~(~a~).htm" name) ',name)
		   hunchentoot:*dispatch-table*)))

(define-url-fn (hermes)
			   (standard-page (:title "Hermes")
							  (:h1 "Welcome to Hermes")
							  (:p "Hermes is a flexible prediction market where you can bet on the outcome of future events!")))

(define-url-fn (about)
			   (standard-page (:title "About")
							  (:h1 "About")
							  (:p "This is an about section")))

; (hunchentoot:start *web-server*)
; (hunchentoot:stop *web-server*)

