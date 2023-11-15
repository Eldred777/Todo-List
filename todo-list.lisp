(defpackage :todo-list)

(in-package :todo-list)

(require :uiop)

(defun not-null (l)
       (not (null l)))

;; Entries to `*database*` are 
;; item = (id: symbol, description: string, sublist: list[entry])
(defvar *database* ())
(defun get-todo-id (entry) (car entry))
(defun get-todo-description (entry) (cadr entry))
(defun get-todo-sublist (entry) (caddr entry))

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

(defun helper-description-on-last-entry (entry-id description)
       "Helper function to `construct-todo` that returns `description` when 
  `entry-id` is a singleton list, and \"_\" otherwise.

  Precondition: 
    `entry-id` is a list."
       (if (null (cdr entry-id))
           description
           " _"))

(defun construct-todo (target-list entry-id &optional (description "_"))
       ; TODO: implement. constructs a todo recursively in a functional style,
       ; returning a copy of `target-list` with a new todo in `entry-id` with
       ; `description` and `sublist`=`nil`. 

       (coerce-to-string description)

       (etypecase entry-id
                  ; [x] test
                  (symbol (if (null entry-id)
                              ; if nil, just return nil which is handled explicitly below
                              nil
                              ; verify that this is not in the list 
                              (cons (list entry-id description nil) target-list)))
                  ; [ ] test
                  (list
                   ; TODO: verification that all entries of list are symbols, else error
                   ; TODO: below still not working. 
                   (if (null target-list)
                       ; if nil then we have created a new list in the previous step
                       ; so just create the next entry. 
                       (cons
                        (list (car entry-id)
                              (helper-description-on-last-entry entry-id description)
                              ; Handle terminal case, we don't want a '(())
                              (let ((sublist (construct-todo nil (cdr entry-id) description)))
                                   (if (null sublist)
                                       nil
                                       sublist)))
                        target-list)
                       ; TODO: remove repeated code, possibly via lazy evaluation? 
                       ; else, we need to find the appropriate entry to traverse into,
                       ; if any such entry exists. 
                       (let ((index (position (get-todo-id entry-id) (map 'list #'first target-list))))
                            (if (numberp index)
                                ; If exists, simply traverse down and run again
                                (let ((list (nth index target-list))
                                      (sublist (get-todo-sublist (nth index target-list))))
                                     (cons (list (get-todo-id list)
                                                 (get-todo-description list)
                                                 (construct-todo sublist (cdr entry-id) description))
                                           (remove list target-list)))
                                ; TODO: is there a better way to do this `remove list` (see below)
                                ; TODO: is there a way to do the above let block via mutation instead? 
                                ; Otherwise we need to add a new entry
                                (cons (list (car entry-id)
                                            (helper-description-on-last-entry entry-id description)
                                            ; Handle terminal case, we don't want a '(())
                                            (let ((sublist (construct-todo nil (cdr entry-id) description)))
                                                 (if (null sublist)
                                                     nil
                                                     sublist)))
                                      target-list)))))))

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

(defun done-todo (entry)
       (setf *database* (remove-todo *database* entry)))

(defun rename-todo (entry new-name)
       "Rename the symbol used for a todo."
       ; TODO: implement 
       entry
       new-name)

(defun describe-todo (entry description)
       "Change the description used for a todo."
       ; TODO: implement 
       ; TODO: option to only update if blank 
       entry
       description)

(defun show-todo ()
       (format t "~{~{~a: ~a~}~%~}" *database*))
