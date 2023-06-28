(defpackage :todo-list
  )

(require :uiop)

(defvar *database* ())

(defun create-new-database (database-name)
  ; Check type of database-name. Should be a string. 
  ; Check if database already exists first
  (uiop:probe-file* database-name)
  ; TODO: implement 
  )

(defun load-database (database-name)
  ; TODO: implement 
  ; Check if database already exists, else fail gracefully and return nil.
  )

(defun save-database (database-name)
  ; TODO: implement 
  )

(defun add-todo (name)
  ; TODO: implement 
  )

(defun done-todo (name)
  ; TODO: implement 
  )

(defun show-todo ()
  ; TODO: implement
  )
