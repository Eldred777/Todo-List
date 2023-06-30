(defpackage :todo-list)

(require :uiop)

(defun not-null (l)
  (not (null l)))

;; Entries to `*database*` are 
;; entry = (name: symbol, description: string, sublist: list[entry])
(defvar *database* ())
(defun get-todo-sublist (entry)
  (caddr entry))

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
  ; Ensure `database-name` is a string
  (coerce-to-string database-name)

  (ensure-directories-exist *db-path*)

  (with-open-file (file (concatenate 'string *db-path* database-name)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~s" *database*)))

(defun clear-database ()
  (setf *database* nil))

(defun position-of-todo (name target-list)
  (position (car name) (map 'list #'first target-list)))

(defun construct-todo (target-list name &optional (description "_"))
  ; TODO: implement. constructs a todo recursively in a functional style,
  ; returning a copy of `target-list` with a new todo in `name` with
  ; `description` and `sublist`=`nil`. 

  (coerce-to-string description)

  (typecase name
    (symbol (push (list name description nil) target-list)) ; [x] test
    (list ; [ ] test
         ; TODO: verify non-null, else error 
         ; TODO: verification that all entries of list are symbols, else error
         ; TODO: below still not working. 
         (if (> (length name) 1)
             (let ((index (position (car name) (map 'list #'first *database*))))
               (unless (numberp index)
                 (progn (add-todo (car name))
                        (setf index 0)))
               (setf *database* (get-todo-sublist (nth index *database*)))
               (add-todo (cdr name) description))
             ; else, one symbol left so use the implementation of this function on symbols
             (add-todo (car name) description)))))

(defun remove-todo (target-list name)
  ; TODO: implement. returns a copy of `target-list` with `name` removed
  )

;; TODO: test 
(defun add-todo (name &optional (description "_"))
  "Adds a new todo entry to `*database*`. Defaults to a blank description if none is provided.
  `name` may be a symbol, or a list of symbols.
  `description` should be a string that describes the todo entry 
  "

  *database*)

(defun done-todo (name)
  (setf *database*
    (remove-if (lambda (x) (eql (car x) name))
        *database*)))

(defun rename-todo (name new-name)
  "Rename the symbol used for a todo."
  ; TODO: implement 
  name
  new-name)

(defun describe-todo (name description)
  "Change the description used for a todo."
  ; TODO: implement 
  ; TODO: option to only update if blank 
  name
  description)

(defun show-todo ()
  (format t "~{~{~a: ~a~}~%~}" *database*))
