(defvar *games* '())

(defclass game ()
  ((name :initarg :name
		 :reader name)
   (votes :initform 0
		  :accessor votes)))

(defmethod vote-for (game)
  (incf (votes game)))

(defmethod add-game (name)
  (unless (is-game-stored name)
	(push (make-instance 'game :name name) *games*)))

(defun find-game (name)
  (find name *games*
		:test #'string-equal	; compare strings
		:key #'name))			; compare the name slot

(defun get-game-names ()
  (mapcar #'name (games)))

(defun is-game-stored (game)
  (find-game game))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defvar tetris (make-instance 'game :name "tetris"))
(defvar pacman (make-instance 'game :name "pacman"))

;; Load the libraries
(mapcar #'ql:quickload '(:cl-who :cl-fad :hunchentoot :parenscript :smackjack))

(defpackage :server
  (:use "COMMON-LISP" "HUNCHENTOOT" "CL-WHO" "PARENSCRIPT" "SMACKJACK"))

(in-package :server)

(defparameter *server*
  (make-instance 'easy-acceptor :address "localhost" :port 8080))

;; tell parenscript how to escape strings so it works with cl-who
(setf *js-string-delimiter* #\")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defun-ajax echo (data) (*ajax-processor* :callback-data :response-text)
  (concatenate 'string "echo: " data))

;; access remote fns through Htoot by integrating AJAX handler with Htoot's 
;; dispatch table
(setq *dispatch-table*
	  (list 'dispatch-easy-handlers (create-ajax-dispatcher *ajax-processor*)))

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
	 (*standard-output* nil :prologue T :indent t)
	 (:html :xmlns "http://www.w3.org/1999/xhtml"
			:xml\:lang "en" 
			:lang "en"
			(:head
			  (:meta :http-equiv "Content-Type"
					 :content    "text/html; charset=utf-8")
			  (:title ,title)
			  (:link :type "text/css"
					 :rel "stylesheet"
					 :href "/style.css"))
			(:body
			  (:div :id "header"
					(:img :src "lambda.jpg"
						  :alt "Hermes"
						  :class "logo")
					(:span :class "strapline"
						   "Welcome to my site"))
			  ,@body))))

(defun markets ()
  (standard-page (:title "Hermes")
				 (:h1 "Welcome to Hermes")
				 (:p "This is where I will host my dissertation")))

(push (create-prefix-dispatcher "/markets.htm" 'markets) *dispatch-table*)

(define-easy-handler (test :uri "/test") ()
					 (standard-page (:title "This is a test")
									(:h1 "Welcome to Mercury")
									(:p "This is a site on which I will host my dissertation -- a two-sided combinatorial prediction market")))

(define-easy-handler (repl :uri "/repl") ()
  (with-html-output-to-string (s)
    (:html
      (:head
        (:title "Jank REPL")
        (str (generate-prologue *ajax-processor*))
        (:script :type "text/javascript"
          (str
            (ps
              (defun callback (response)
                (alert response))
              (defun on-click ()
                (chain smackjack (echo (chain document
                                              (get-element-by-id "data")
                                              value)
                                       callback)))))))
      (:body
        (:p
          (:input :id "data" :type "text"))
        (:p
          (:button :type "button"
                   :onclick (ps-inline (on-click))
                   "Submit!"))))))

(define-easy-handler (example1 :uri "/example1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#" :onclick (ps (alert "Hello World"))
                "Hello World")))))

(define-easy-handler (example2 :uri "/example2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial: 2nd example")
      (:script :type "text/javascript"
               (str (ps
                      (defun greeting-callback ()
                        (alert "Hello World"))))))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World")))))

(define-easy-handler (example3 :uri "/example3.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (defun greeting-callback ()
      (alert "Hello World"))))

#|
(ql:quickload :hunchentoot)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

(hunchentoot:define-easy-handler (say-hello :uri "/hello") (name)
 (setf (hunchentoot:content-type*) "text/plain")
 (format nil "Hello, ~a! I am Tom~%I build a website with Lisp!" name))
|#
