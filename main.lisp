(in-package :hockey-oracle)

(defvar app-version (uiop:read-file-form "version.lisp-expr"))
(defvar app-updated (uiop:read-file-form "date-updated.lisp-expr"))

;;; Utils
(defun get-secure-key (key)
  "Gets a secure key by calling the 'pass' program."
  (uiop:run-program (sf "pass ~A" key) :output '(:string :stripped t)))

(defun to-bool (redis-val)
  "Converts the given redis value to bool."
  (plusp (parse-integer redis-val)))
;;; Utils ------------------------------------------------------------------- END

;;; Leagues
(defstruct league
  (id 0)
  (name "")
  (created "")
  (active? t))

(defun get-all-leagues ()
  "Gets a list of all leagues sorted by name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((league-keys (red-keys "league:*"))
           (leagues nil))
      (dolist (league-key league-keys)
        (push (create-league-from-db league-key) leagues))
      (sort leagues #'string< :key #'league-name))))

(defun create-league-from-db (league-key)
  "Create a league struct from a database record."
  (let ((id (parse-integer league-key :start 7)))
    (make-league :id id
                 :name (red-hget league-key "name")
                 :created (red-hget league-key "created")
                 :active? (to-bool (red-hget league-key "active?")))))
;;; Leagues ----------------------------------------------------------------- END

;;; Players
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

(defvar players-positions '("C" "D" "G" "LW" "RW"))

(defun get-all-players ()
  "Gets a list of all players sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((player-keys (red-keys "player:*"))
           (players nil))
      (dolist (player-key player-keys)
        (push (create-player-from-db player-key) players))
      (sort players #'string< :key #'player-first-name))))

(defun get-players (&key league-name)
  "Gets a list of all players sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (let* ((leagues (get-all-leagues))
         (league (find league-name leagues :test #'string-equal
                                           :key #'league-name)))
    (redis:with-persistent-connection ()
      (let* ((player-ids '())
             (players '()))
        (setf player-ids
              (red-smembers (sf "leagues:~A:players" (league-id league))))
        (dolist (player-id player-ids)
          (push (create-player-from-db (sf "player:~A" player-id)) players))
        (sort players #'string< :key #'player-first-name)))))

(defun create-player-from-db (player-key)
  "Create a player struct from a database record."
  (let ((id (parse-integer player-key :start 7)))
    (make-player :id id
                 :first-name (red-hget player-key "first-name")
                 :last-name (red-hget player-key "last-name")
                 :position (red-hget player-key "position")
                 :active? (red-sismember "players:active" id))))
;;; Players ----------------------------------------------------------------- END
