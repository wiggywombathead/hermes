;;;; Implementation of database

(ql:quickload :mito)

(defun connect-database ()
  " connect to the database "
  (mito:connect-toplevel
	:mysql
	:database-name "cassie" :username "tom" :password ""))

(defun disconnect-database ()
  (mito:disconnect-toplevel))

;;; Database table definitions

(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (budget :initform 100
		   :col-type :float)))

(mito:deftable security ()
  ((query-str :col-type :text)
   (shares :initform 0
		   :col-type :integer)
   (deadline :col-type :datetime)
   (closing-price :col-type (or :float :null))))

(mito:deftable users-securities ()
  ((user :col-type user)
   (security :col-type security)
   (quantity :col-type :integer)
   (report :col-type (or :bit :null))))

(defun create-table (table)
  (mito:ensure-table-exists table))

(defmacro with-open-database (&body code)
  " execute CODE without worrying about the connection "
  `(progn
	 (connect-database)
	 (let ((result ,@code))
	   (disconnect-database)
	   result)))

(defun create-tables ()
  " create tables for USER, SECURITY, and USERS-SECURITIES "
  (with-open-database
	(mapcar #'mito:ensure-table-exists '(user security users-securities))))

(defun alter-table (table)
  " alter the table defined by TABLE object "
  (with-open-database
	; (mito:migration-expression 'user) ; print the generated expression
	(mito:migrate-table table)))

(defun insert-user (name)
  (with-open-database
	(mito:create-dao 'user :name name)))

(defun insert-security (query deadline)
  (with-open-database
	(mito:create-dao 'security :query query :deadline deadline)))

(defun user-exists (name)
  " return T if user with username NAME exists, else NIL "
  (with-open-database
	(not (eq NIL (mito:find-dao 'user :name name)))))

(defun get-user-by-name (name)
  " return the user struct associated with NAME "
  (with-open-database
	(mito:find-dao 'user :name name)))
