(defpackage :todo-list)

(require :uiop)

(defvar *database* ())
(defvar *current-database* "")

(defparameter *db-path* (concatenate 'string
                          (namestring *default-pathname-defaults*)
                          "db/"))


(defun load-database (database-name)
  (with-open-file (file (concatenate 'string *db-path* database-name)
                        :direction :input
                        :if-does-not-exist nil)
    (setf *database* (read-line file))
    (setf *current-database* database-name)))

(defun save-database (&optional (database-name *current-database*))
  ; Ensure `database-name` is a string
  (unless (typep database-name 'string)
    (setf database-name (format nil "~a" database-name)))

  (ensure-directories-exist *db-path*)

  (with-open-file (file (concatenate 'string *db-path* database-name)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~a" *database*)))

(defun clear-database ()
  (setf *database* nil))

(defun add-todo (name &optional (description "(unspecified)"))
  (setf *database*
    (append (list (list name description)) *database*)))

(defun done-todo (name)
  (setf *database*
    (remove-if (lambda (x) (eql (car x) name))
        *database*)))

(defun show-todo ()
  (format t "~{~{~a: ~a~}~%~}" *database*))
