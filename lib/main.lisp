(in-package :hockey-oracle)

(defvar app-version 0.2)
(defvar app-updated "Apr 14 2015")

(defclass player ()
  ((player-id :reader player-id
              :initarg :player-id)
   (first-name :reader first-name
               :initarg :first-name)
   (last-name :reader last-name
              :initarg :last-name)
   (pposition :accessor pposition
             :initarg :pposition)
   (active? :accessor active?
            :initarg :active?)))

(defmethod print-object ((object player) stream)
  (print-unreadable-object (object stream :type T)
    (with-slots (player-id first-name last-name pposition active?) object
      (format stream "~s ~s ~s ~s (active? ~s)" player-id first-name last-name pposition active?))))

(defmethod activate-player (p)
  "Activate the given player."
  (setf (active? p) T))

(defmethod deactivate-player (p)
  "Deactivate the given player."
  (setf (active? p) NIL))

(defvar players '())
(defvar player-id-seed 0)

(defun players ()
  "Get a sorted list of all players."
  (sort (copy-list players) #'string< :key #'first-name))

(defun add-player (fname lname pos active?)
  "Add a player to the global list."
  (push
    (make-instance
      'player
      :player-id (incf player-id-seed)
      :first-name fname
      :last-name lname
      :pposition pos
      :active? active?)
    players))

(add-player "Aiyaz" "" "LW" NIL)
(add-player "Anish" "" "RW" T)
(add-player "Bryan" "T." "D" T)
(add-player "Brian" "K." "D" T)
(add-player "Carmen" "" "C" NIL)
(add-player "Elroy" "" "G" T)
(add-player "Kup" "" "C" T)
(add-player "Mark" "M." "RW" T)
(add-player "Mark" "S." "G" T)
(add-player "Mauz" "" "RW" T)
(add-player "Osama" "" "LW" T)
(add-player "Raj" "" "D" NIL)
(add-player "Robin" "" "D" T)
(add-player "Steve" "" "LW" NIL)
(add-player "Saif" "" "LW" T)
(add-player "Taran" "" "RW" T)
(add-player "Thiru" "" "D" T)
(add-player "Touraj" "" "C" T)
