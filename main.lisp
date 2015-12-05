(in-package :hockey-oracle)

(defvar app-version (uiop:read-file-form "version.lisp-expr"))
(defvar app-updated (uiop:read-file-form "date-updated.lisp-expr"))

(defstruct player
  (id 0)
  (first-name "")
  (last-name "")
  (position "")
  (active? t))

(defmethod player-add ((p player))
  "Add the given player to the list of all players."
  (when (eql 0 (player-id p))
    (setf (player-id p) (incf player-id-seed)))
  (push p players))

(defmethod player-activate ((p player))
  "Activate the given player."
  (setf (player-active? p) T))

(defmethod player-deactivate ((p player))
  "Deactivate the given player."
  (setf (player-active? p) NIL))

(defvar players '())
(defvar player-id-seed 0)

(defun sorted-players ()
  "Get a sorted list of all players."
  (sort (copy-list players) #'string< :key #'player-first-name))

(player-add (make-player :first-name "Anish" :position "RW"))
(player-add (make-player :first-name "Brian"  :last-name"K." :position "D"))
(player-add (make-player :first-name "Bryan" :last-name "T." :position "D" :active? nil))
(player-add (make-player :first-name "Carmen" :position "C"))
(player-add (make-player :first-name "Daniel" :position "D"))
(player-add (make-player :first-name "Elroy" :position "G"))
(player-add (make-player :first-name "Kup" :position "C"))
(player-add (make-player :first-name "Mark" :last-name "M." :position "RW"))
(player-add (make-player :first-name "Mark" :last-name "S." :position "G"))
(player-add (make-player :first-name "Osama" :position "LW"))
(player-add (make-player :first-name "Raj" :position "D"))
(player-add (make-player :first-name "Robin" :position "D"))
(player-add (make-player :first-name "Saif" :position "LW"))
(player-add (make-player :first-name "Steve" :position "LW" :active? nil))
(player-add (make-player :first-name "Taran" :position "RW"))
(player-add (make-player :first-name "Thiru" :position "D"))
(player-add (make-player :first-name "Touraj" :position "C"))
