(in-package :hockey-oracle)

(defvar app-version (uiop:read-file-form "version.lisp-expr"))
(defvar app-updated (uiop:read-file-form "date-updated.lisp-expr"))

;;; Player struct
(defstruct player
  (id 0)
  (first-name "")
  (last-name "")
  (position "")
  (active? t))

(defmethod player-activate ((p player))
  "Activate the given player."
  (setf (player-active? p) T))

(defmethod player-deactivate ((p player))
  "Deactivate the given player."
  (setf (player-active? p) NIL))
;;; Player struct ----------------------------------------------------------- END

(defun players-get-all ()
  "Gets a list of all players sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((player-keys (red-keys "player:*"))
           (players nil))
      (dolist (player-key player-keys)
        (push (create-player-from-db player-key) players))
      (sort players #'string< :key #'player-first-name))))

(defun create-player-from-db (player-key)
  "Create a player struct from a database record."
  (let ((id (parse-integer player-key :start 7)))
    (make-player :id id
                 :first-name (red-hget player-key "first-name")
                 :last-name (red-hget player-key "last-name")
                 :position (red-hget player-key "position")
                 :active? (red-sismember "players:active" id))))
