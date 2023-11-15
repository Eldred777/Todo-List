(defpackage :todo-entry
  (:export #:make-todo-entry))


(defclass todo-entry ()
    ((entry-id
       :initarg :entry-id
       :initform (error "Must supply an ID for the entry.")
       :accessor entry-id)
     (description
       :initarg :description
       :initform (error "Must supply a description.")
       :accessor description)
     (children
       :initarg :children
       :initform nil
       :accessor children)))

(defun make-todo-entry (entry-id &optional (description "_") (children nil))
  (make-instance 'todo-entry
    :entry-id entry-id
    :description description
    :children children))

(defun add-child (parent child)
  (push child (children parent)))

(defun has-child-id (parent child-id)
  ; TODO: implement 
  t)

(defun display-todo-entry (entry &optional (indent-level 0))
  (loop for x from 1 to indent-level do (format t "  "))
  (format t "~s ~s~%" (entry-id entry) (description entry))
  (loop for x in (children entry) do (display-todo-entry x (1+ indent-level))))
