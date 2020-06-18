;;;; Implementation of database

(ql:quickload :mito)
(ql:quickload :sxql)

(defpackage :db
  (:use :cl :mito :sxql)
  (:export :init-database
		   :create-tables
		   :insert-user
		   :insert-security
		   :insert-user-security

		   ;; user stuff
		   :get-users
		   :user-exists?
		   :holds-shares?
		   :get-user-by-name

		   ;; security stuff
		   :get-securities
		   :get-security-by-id

		   ;; portfolio stuff
		   :add-portfolio-entry
		   :update-portfolio
		   :get-current-position
		   :user-security-exists?

		   ;; class accessors
		   :user-id
		   :user-name
		   :user-budget

		   :security-id
		   :security-bet
		   :security-shares
		   :security-deadline
		   :security-closing-price

		   :user-security-shares

		   ;; update functions
		   :update-budget
		   :update-budget-by-name
		   :update-security-shares
		   :update-portfolio

		   :pay-bank

		   :get-active-markets
		   :get-unresolved-markets
		   :report-market-outcome
		   :get-arbiter-reports))

(in-package :db)

(defparameter +banker-name+ "bank")	; TODO: make this a constant
(defparameter *banker* NIL)

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
				   :col-type :double)))

(deftable security ()
		  ((bet :col-type :text)
		   (shares :initform 0
				   :col-type :integer)
		   (deadline :col-type :datetime)
		   (closing-price :col-type (or :double :null))))

(deftable user-security ()
		  ((user :col-type user)
		   (security :col-type security)
		   (shares :col-type :integer
				   :initform 0)
		   (report :col-type (or :char :null))))

(defun user-id (user)
  (object-id user))

(defun security-id (security)
  (object-id security))

(defmacro with-open-database (&body code)
  " execute CODE without worrying about the connection "
  `(progn
	 (connect-database)
	 (let ((result (progn ,@code)))
	   (disconnect-database)
	   result)))

(defun insert-user (name &optional budget)
  (with-open-database
	(if budget
	  (create-dao 'user :name name :budget budget)
	  (create-dao 'user :name name))))

(defun insert-security (bet deadline shares)
  (with-open-database
	;(format T "inserting security [~A,~A]~%" bet deadline)
	(create-dao 'security :bet bet :deadline deadline :shares shares)))

(defun get-user-by-name (name)
  " return the user struct associated with NAME "
  (with-open-database
	(find-dao 'user :name name)))

(defun create-tables ()
  " create tables for USER, SECURITY, and USER-SECURITY "
  (with-open-database
	(mapcar #'ensure-table-exists '(user security user-security))))

(defun init-database ()
  (create-tables)
  (setf *banker* (if (get-user-by-name +banker-name+)
				   (get-user-by-name +banker-name+)
				   (insert-user +banker-name+))))

(defun clear-tables ()
  (with-open-database
	(mapcar #'delete-by-values '(user security user-security))))

(defun delete-table (table)
  (with-open-database
	(execute-sql (drop-table table))))

;; TODO: fix this
;(defun drop-tables
;  (with-open-database
;	(mapcar
;	  #'(lambda (x) (drop-table x)) '(security))))

(defun update-table-definition (table)
  " update the table defined by class/struct TABLE "
  (with-open-database
	; (migration-expression 'user) ; print the generated expression
	(migrate-table table)))

(defun user-exists? (name)
  " return T if user with username NAME exists, else NIL "
  (with-open-database
	(not (eq NIL (find-dao 'user :name name)))))

(defun get-current-position (user security)
  " returns the number of shares USER currently holds of SECURITY "
  (with-open-database
	(let ((position (find-dao 'user-security :user user :security security)))
	  (if position
		(user-security-shares position)
		0))))

(defun holds-shares? (user security)
  " return T if user-security entry referring to USER and SECURITY exists and
  the quantity is not equal to 0"
  (not (= 0 (get-current-position user security))))

(defun get-users ()
  " return all users in the database "
  (with-open-database
	(select-dao 'user)))

(defun update-budget (user amount)
  " increase budget of USER by AMOUNT "
  (let ((current-budget (user-budget user)))
	;; TODO: better way to remove `d0` suffix for storing in database?
	(setf (slot-value user 'budget) (float (rational (+ current-budget amount))))
	(with-open-database
	  (save-dao user))))

(defun update-budget-by-name (name new-budget)
  (update-budget (get-user-by-name name) new-budget))

(defun get-securities ()
  " return all securities in the database "
  (with-open-database
	(select-dao 'security)))

(defun get-active-markets (date)
  " return all securities whose deadline has not yet passed "
  (with-open-database
	(select-dao 'security (where (:> :deadline date)))))

(defun get-unresolved-markets (date)
  " return all securities whose deadline has passed but outcomes have not been
  settled -- this amounts to retrieving all securities whose closing-price is
  null "
  (with-open-database
	(select-dao 'security (where (:and (:is-null :closing-price)
									   (:< :deadline date))))))

(defun get-security-by-id (id)
  (with-open-database
	(find-dao 'security :id id)))

(defun update-security-shares (security new-quantity)
  (setf (slot-value security 'shares) new-quantity)
  (with-open-database
	(save-dao security)))

(defun insert-user-security (user security &optional shares report)
  (with-open-database
	(create-dao 'user-security :user user :security security :shares shares :report report)))

(defun user-security-exists? (user security)
  (with-open-database
	(find-dao 'user-security :user user :security security)))

(defun add-portfolio-entry (user security shares)
  (insert-user-security user security shares))

(defun update-portfolio (user security shares)
  " sets the number of shares USER holds of SECURITY to SHARES "
  (with-open-database
	(let* ((portfolio-entry (find-dao 'user-security :user user :security security))
		   (old-quantity (user-security-shares portfolio-entry)))
	  (setf (slot-value portfolio-entry 'shares) (+ shares old-quantity))
	  (save-dao portfolio-entry))))

(defun pay-bank (user amount)
  " transfer AMOUNT from USER's account to the bank "
  (update-budget user (- amount))
  (update-budget *banker* amount))

(defun report-market-outcome (user security report)
  " allow USER to submit a REPORT on the outcome of SECURITY "
  (with-open-database
	(let ((portfolio-entry (find-dao 'user-security :user user :security security)))
	  (if portfolio-entry
		(progn
		  (setf (slot-value portfolio-entry 'report) report)
		  (save-dao portfolio-entry))
		(create-dao 'user-security
					:user user
					:security security
					:shares 0
					:report report)))))

(defun get-arbiters (security)
  " get all users who reported an outcome of SECURITY "
  (with-open-database
	(select-dao 'user (inner-join 'user-security :on (:= :user.id :user-security.user-id))
				(where (:and (:= :user-security.security-id (security-id security))
							 (:not-null :user-security.report))))))

(defun get-arbiter-reports (security)
  " return a list ((arbiter report) ...) of the arbiters and their report on
  the outcome of SECURITY"
  (let ((arbiters (get-arbiters security))
		reports)

	(dolist (arbiter arbiters)
	  (with-open-database
		(let ((report (parse-integer (user-security-report
									   (find-dao 'user-security
												 :user arbiter
												 :security security)))))
		  (push (list arbiter report) reports))))
	reports))
