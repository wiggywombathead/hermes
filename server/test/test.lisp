(ql:quickload :cl-who)
(ql:quickload :hunchentoot)
(ql:quickload :parenscript)

(defpackage :server
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :server)

(defclass game ()
  ;; :reader auto-creates read function
  ((name :reader name
		 :initarg :name)
   ;; :accessor auto-creates read and write functions
   (votes :accessor votes
		  :initform 0)))

(defmethod vote-for (game)
  (incf (votes game)))

(setf tetris (make-instance 'game :name "tetris"))
(setf pacman (make-instance 'game :name "pacman"))

(name tetris)
(votes tetris)
(incf (votes tetris))

(defvar *games* '())

(defclass point ()
  ;; initarg - name of slot
  ;; initform - defalut value
  ((x :initarg :x
	  :initform "error - no value for x"
	  :accessor x)

   (y :initarg :y
	  :initform 0
	  :accessor y)

   (z :initarg :z
	  :initform 10
	  :accessor z)))

(defun make-point (x y z)
  (make-instance 'point :x x :y y :z z))

(setf p1 (make-point 4 5 6))
;; (inspect p1)

(slot-value p1 'x)

;; (hunchentoot:define-easy-handler (say-hello :uri "/hello") (name)
;;  (setf (hunchentoot:content-type*) "text/plain")
;;  (format nil "Hello, ~a! I am Tom~%I build a website with Lisp!" name))
;; 
;; (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
