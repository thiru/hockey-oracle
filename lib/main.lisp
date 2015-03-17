(ql:quickload '(cl-who hunchentoot parenscript))

(defpackage :hockey-oracle
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :hockey-oracle)

(defclass player ()
  ((first-name :reader first-name
               :initarg :first-name)
   (last-name :reader last-name
              :initarg :last-name)
   (pposition :accessor pposition
             :initarg :pposition)
   (active? :accessor active?
              :initarg :active?)))

(defmethod print-object ((object player) stream)
  (print-unreadable-object (object stream :type T)
    (with-slots (first-name last-name pposition active?) object
      (format stream "~s ~s ~s (active? ~s)" first-name last-name pposition active?))))

(defmethod activate-player (p)
  "Activate the given player."
  (setf (active? p) T))

(defmethod deactivate-player (p)
  "Deactivate the given player."
  (setf (active? p) NIL))

(defvar *players* '())

(defun players ()
  "Get a sorted list of all players."
  (sort (copy-list *players*) #'string< :key #'first-name))

(defun add-player (fname lname pos active?)
  "Add a player to the global list."
  (push
    (make-instance
      'player
      :first-name fname
      :last-name lname
      :pposition pos
      :active? active?)
    *players*))

(add-player "Touraj" "Nikou" "C" T)
(add-player "Osama" "Raza" "RW" T)
(add-player "Aiyaz" "Ahmed" "LW" NIL)
