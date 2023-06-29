(defpackage :todo-list)

(require :uiop)

(defvar *database* ())
(defvar *current-database-name* "")

(defparameter *db-path* (concatenate 'string
                          (namestring *default-pathname-defaults*)
                          "db/"))

(defmacro coerce-to-string (name)
  (unless (typep name 'string)
    (setf name (format nil "~a" name))))

(defun load-database (database-name)
  (coerce-to-string database-name)

  (with-open-file (file (concatenate 'string *db-path* database-name))
    :direction :input
    :if-does-not-exist :file-error
    (setf *database* (read-line file))
    (setf *current-database-name* database-name)))

(defun save-database (&optional (database-name *current-database-name*))
  ; Ensure `database-name` is a string
  (coerce-to-string database-name)

  (ensure-directories-exist *db-path*)

  (with-open-file (file (concatenate 'string *db-path* database-name)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~a" *database*)))

(defun clear-database ()
  (setf *database* nil))

;; Add a new todo entry to `*database*`. 
;; `name`: a 
(defun add-todo (name &optional (description (symbol-name name)))
  "Adds a new todo entry to `*database*`. 
  `name` may be a symbol, or a list of symbols.
  "
  ; entries consist of a name, an optional description, and a list of sub-entries
  (typecase name
    (symbol
     (setf *database*
       (push (list name description nil) *database*)))
    (list
     ; TODO: implement. traverse down the list `name`, constructing new todos as needed.
     )))

(defun done-todo (name)
  (setf *database*
    (remove-if (lambda (x) (eql (car x) name))
        *database*)))

(defun show-todo ()
  (format t "~{~{~a: ~a~}~%~}" *database*))
