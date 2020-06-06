(defstruct database
  name
  tables)

(defstruct table
  name
  entries)

(defun insert (entry table)
  (push entry table))

(defun dump-table (table)
  " dump contents of table "
  (dolist (entry table)
	(format T "~{~a:~10t~a~%~}~%" entry)))

(defun save-database (database)
  " save database to a file "
  (with-open-file (out (format NIL "~a.db" (database-name database))
					   :direction :output		; write out
					   :if-exists :supersede) 	; overwrite if exists
	(with-standard-io-syntax
	  (print database out))))

(defun load-database (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  (read in))))

(defun make-comparison-expr (field value)
  `(equal (getf album ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (x) (and ,@(make-comparisons-list clauses))))

(defun select (selector-fn database)
  (remove-if-not selector-fn database))

;; (ql:quickload :mito)
;; 
;; (defun connect ()
;;   " connect to the database "
;;   (mito:connect-toplevel
;; 	:mysql
;; 	:database-name "cassie" :username "tom" :password ""))
;; 
;; (defun disconnect ()
;;   (mito:disconnect-toplevel))
;; 
;; (defclass user ()
;;   ((username :col-type (:varchar 64)
;; 			 :init-arg :name
;; 			 :accessor user-username)
;;    (funds :col-type :decimal)))
