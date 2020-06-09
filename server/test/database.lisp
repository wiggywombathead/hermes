;;; Mito OOP style

;; (defclass user ()
;;   ((name :initarg :name
;; 		 :accessor user-name
;; 		 :col-type (:varchar 64))
;;    (budget :initarg :budget
;; 		   :initform 100
;; 		   :accessor user-budget
;; 		   :col-type :float))
;;   (:metaclass mito:dao-table-class))
;; 
;; (defclass security ()
;;   ((query-str :initarg :query
;; 			  :accessor security-query-str
;; 			  :col-type :text)
;;    (shares :initarg :shares
;; 		   :initform 0
;; 		   :accessor :security-shares
;; 		   :col-type :integer)
;;    (deadline :initarg :deadline
;; 			 :accessor security-deadline
;; 			 :col-type :datetime)
;;    (closing-price :initarg :closing-price
;; 				  :accessor security-closing-price
;; 				  :col-type (or :float :null)))
;;   (:metaclass mito:dao-table-class))
;; 
;; (defclass users-securities ()
;;   ((user :initarg :user
;; 		 :col-type user)
;;    (security :initarg :security
;; 			 :col-type security)
;;    (quantity :initarg :quantity
;; 			 :accessor quantity
;; 			 :col-type :integer)
;;    (report :initarg :report
;; 		   :accessor outcome-report
;; 		   :col-type (or :bit :null)))
;;   (:metaclass mito:dao-table-class))

(defvar *db* nil)

(defun make-album (title artist year)
  (list :title title :artist artist :year year :rating 0))

(defun add-album (album)
  (push album *db*))

(defun dump-db ()
  (dolist (album *db*)
	(format t "~{~a:~10t~a~%~}~%" album)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-album ()
  (make-album 
	(prompt-read "title")
	(prompt-read "artist")
	(or (parse-integer (prompt-read "year") :junk-allowed t) 0)))

(defun add-albums ()
  (loop (add-album (prompt-for-album))
		(if (not (y-or-n-p "Another? [y/n]: "))
		  (return))))

(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output		; write out
					   :if-exists :supersede)	; overwrite if file exists
	(with-standard-io-syntax
	  (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  (setf *db* (read in)))))

;; tick -> item is not evaluated
;; back tick -> only items preceded by , are evaluated
(defun make-comparison-expr (field value)
  `(equal (getf album ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))

;; ,@ -> splice the resulting list into enclosing list
;; ` -> expand no expression apart from those preceded by ,
(defmacro where (&rest clauses)
  `#'(lambda (album) (and ,@(make-comparisons-list clauses))))

(defmacro where-or (&rest clauses)
  `#'(lambda (album) (or ,@(make-comparisons-list clauses))))

;; (defun where (&key title artist year)
;;   #'(lambda (album) (and
;; 					  (if title  (equal (getf album :title) title) t)
;; 					  (if artist (equal (getf album :artist) artist) t)
;; 					  (if year   (equal (getf album :year) year) t))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist year rating)
  (setf *db* (mapcar
			   #'(lambda (row)
				   (when (funcall selector-fn row)
					 (if title  (setf (getf row :title) title))
					 (if artist (setf (getf row :artist) artist))
					 (if year   (setf (getf row :year) year))
					 (if rating (setf (getf row :rating) rating)))
				   row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun select-by-artist (artist)
  (select (where :artist artist)))

(defun rate-album (title rating)
  (update (where :title title) :rating rating))

(defmacro backwards (expr)
  (reverse expr))

(setf *db* nil)
(add-album (make-album "Dark Side of the Moon" "Pink Floyd" 1973))
(add-album (make-album "In Utero" "Nirvana" 1993))
(add-album (make-album "Abbey Road" "The Beatles" 1969))
(add-album (make-album "Blood Sugar Sex Magik" "Red Hot Chili Peppers" 1991))
(add-album (make-album "Ummagumma" "Pink Floyd" 1969))
