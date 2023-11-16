(defpackage :todo-list
  (:use :cl :todo-entry :todo-database))

(in-package :todo-list)

(defun not-null-p (l)
  (not (null l)))

(defun position-of-todo (id target-list)
  (position id (map 'list #'first target-list)))

(defun helper-description-on-last-entry (entry-id description)
  "Helper function to `construct-todo` that returns `description` when 
  `entry-id` is a singleton list, and \"_\" otherwise.

  Precondition: 
    `entry-id` is a list."
  (if (null (cdr entry-id))
      description
      "_"))

(defun construct-todo (target-list entry-id &optional (description "_"))
  ; TODO: implement. constructs a todo recursively in a functional style,
  ; returning a copy of `target-list` with a new todo in `entry-id` with
  ; `description` and `sublist`=`nil`. 

  (coerce-to-string description)

  (etypecase entry-id
    (symbol (if (null entry-id)
                nil ; null case handled below 
                (cons (list entry-id description nil) target-list)))
    ; TODO: verification that all entries of list are symbols, else error
    ; TODO: below still not working. 
    (list
     (if (null target-list)
         (setf (car target-list)
           (list (car entry-id)
                 (helper-description-on-last-entry entry-id description)
                 (let ((sublist (construct-todo nil (cdr entry-id) description)))
                   (if (null sublist)
                       nil
                       sublist))))
         target-list)
     ; TODO: remove repeated code, possibly via lazy evaluation? 
     ; else, we need to find the appropriate entry to traverse into,
     ; if any such entry exists. 
     (let ((index (position (get-todo-id entry-id) (map 'list #'first target-list))))
       (if (numberp index)
           (let ((list (nth index target-list))
                 (sublist (get-todo-sublist (nth index target-list))))
             (cons (list (get-todo-id list)
                         (get-todo-description list)
                         (construct-todo sublist (cdr entry-id) description))
                   (remove list target-list)))
           ; TODO: is there a better way to do this `remove list` (see below)
           ; TODO: is there a way to do the above let block via mutation instead? 
           (cons (list (car entry-id)
                       (helper-description-on-last-entry entry-id description)
                       (let ((sublist (construct-todo nil (cdr entry-id) description)))
                         (if (null sublist)
                             nil
                             sublist)))
                 target-list))))))

;; TODO: test 
(defun add-todo (entry &optional (description "_"))
  "Adds a new todo entry to `*database*`. 
  Defaults to a blank description if none is provided.
  `entry` may be a symbol, or a list of symbols.
  `description` should be a string that describes the todo entry 
  "
  (setf *database* (construct-todo *database* entry description)))

(defun remove-todo (target-list entry)
  ; TODO: implement. returns a copy of `target-list` with `entry` removed
  entry
  target-list
  ; shut up compiler
  )


(defun mark-done (entry)
  (setf *database* (remove-todo *database* entry)))

(defun mark-undone (entry)
  (setf *database* (remove-todo *database* entry)))

(defun rename-todo-symbol (entry new-name)
  "Rename the symbol used for a todo."
  ; TODO: implement 
  entry
  new-name)

(defun redescribe-todo (entry description)
  "Change the description used for a todo."
  ; TODO: implement 
  ; TODO: option to only update if blank 
  entry
  description)

(defun show-todo ()
  (format t "~{~{~a: ~a~}~%~}" *database*))
