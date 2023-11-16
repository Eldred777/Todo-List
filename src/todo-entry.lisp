(defpackage :todo-entry
  (:use :cl)
  (:export :make-todo-entry
           :add-child :add-children
           :display-todo-entry
           :mutable-remove-if-id))

(in-package :todo-entry)

;;; TODO: remove subitems at arbitrary depth, specify via path?

(defmacro mutable-remove-if-id (target-list id-to-remove)
  `(setf
     ,target-list
     (delete-if
         (lambda (x) (eq (entry-id x) ,id-to-remove))
         ,target-list)))

(defclass todo-entry-class ()
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
  (make-instance 'todo-entry-class
    :entry-id entry-id
    :description description
    :children children))

(defun add-child (parent child)
  (push child (children parent)))

(defun add-children (parent children)
  (loop for child in children do (push child (children parent))))

(defun remove-child (parent child-id)
  (mutable-remove-if-id (children parent) child-id))

(defun display-todo-entry (entry &optional (indent-level 0))
  (loop for x from 1 to indent-level do (format t "  "))
  (format t "~s ~s~%" (entry-id entry) (description entry))
  (loop for x in (children entry) do (display-todo-entry x (1+ indent-level))))
