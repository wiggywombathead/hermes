;;;; Implementation of database

(ql:quickload :mito)
(ql:quickload :sxql)

(defpackage :db
  (:use :cl :mito :sxql)
  (:export :insert-user
		   :insert-security
		   :user-exists
		   :get-user-by-name

		   :user-name
		   :user-budget
		   :security-bet-str
		   :security-shares
		   :security-deadline
		   :security-closing-price))

(in-package :db)

(defun connect-database ()
  " connect to the database "
  (connect-toplevel
	:mysql
	:database-name "cassie" :username "tom" :password ""))

(defun disconnect-database ()
  (disconnect-toplevel))

;;; Database table definitions

(deftable user ()
		  ((name :col-type (:varchar 64))
		   (budget :initform 100
				   :col-type :float)))

(deftable security ()
		  ((bet-str :col-type :text)
		   (shares :initform 0
				   :col-type :integer)
		   (deadline :col-type :datetime)
		   (closing-price :col-type (or :float :null))))

(deftable users-securities ()
		  ((user :col-type user)
		   (security :col-type security)
		   (quantity :col-type :integer)
		   (report :col-type (or :bit :null))))

(defmacro with-open-database (&body code)
  " execute CODE without worrying about the connection "
  `(progn
	 (connect-database)
	 (let ((result (progn ,@code)))
	   (disconnect-database)
	   result)))

(defun create-tables ()
  " create tables for USER, SECURITY, and USERS-SECURITIES "
  (with-open-database
	(mapcar #'ensure-table-exists '(user security users-securities))))

(defun update-table-definition (table)
  " update the table defined by class/struct TABLE "
  (with-open-database
	; (migration-expression 'user) ; print the generated expression
	(migrate-table table)))

(defun insert-user (name)
  (with-open-database
	(create-dao 'user :name name)))

(defun insert-security (bet-str deadline)
  (with-open-database
	;(format T "inserting security [~A,~A]~%" bet-str deadline)
	(create-dao 'security :bet-str bet-str :deadline deadline)))

(defun user-exists (name)
  " return T if user with username NAME exists, else NIL "
  (with-open-database
	(not (eq NIL (find-dao 'user :name name)))))

(defun get-user-by-name (name)
  " return the user struct associated with NAME "
  (with-open-database
	(find-dao 'user :name name)))
