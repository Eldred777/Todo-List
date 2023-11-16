(defpackage :database
  (:use :cl)
  (:export :load-database :save-database
           :push-to-database :remove-from-database
           :clear-current-database
           :display-database)
  (:import-from :todo-entry #:display-todo-entry))

(require :uiop)

(in-package :database)

(defparameter *database* nil)
(defvar *current-database-name* "")
(defparameter *db-path* (concatenate 'string (namestring *default-pathname-defaults*) "db/"))

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

(defun display-database ()
  (loop for entry in *database*
        do (progn
            (display-todo-entry entry)
            (format t "~%"))))

(defun clear-database ()
  (setf *database* nil))

(defun push-to-database (entry)
  (push entry *database*))

(defun remove-from-database (id)
  (todo-entry:mutable-remove-if-id *database* id))

;;; TODO: clear and push to subitems 
