(use-package '(:todo-entry :todo-database :todo-list))

(defparameter w (make-todo-entry 'w))
(defparameter x (make-todo-entry 'x))
(defparameter y (make-todo-entry 'y))
(defparameter z (make-todo-entry 'z))

(add-child x y)
(add-child x z)
(todo-entry:display-todo-entry x)

(todo-database:push-to-database w)
(todo-database:push-to-database x)

(todo-database:display-database)
