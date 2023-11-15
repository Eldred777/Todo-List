(defpackage :todo-database)

(require :uiop)

(defparameter *database* nil)
(defvar *current-database-name* "")
(defparameter *db-path* (concatenate 'string
                          (namestring *default-pathname-defaults*)
                          "db/"))

(defmacro coerce-to-string (name)
  (unless (typep name 'string)
    (setf name (format nil "~s" name))))

(defun load-database (database-name)
  (coerce-to-string database-name)
  (with-open-file (file (concatenate 'string *db-path* database-name))
    :direction :input
    :if-does-not-exist :file-error
    (setf *database* (read-line file))
    (setf *current-database-name* database-name)))

(defun save-database (&optional (database-name *current-database-name*))
  (coerce-to-string database-name)
  (ensure-directories-exist *db-path*)
  (with-open-file (file (concatenate 'string *db-path* database-name)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~s" *database*)))

(defun clear-database ()
  (setf *database* nil))
