(in-package :hockey-oracle)

(defvar app-version (uiop:read-file-form "version.lisp-expr"))
(defvar app-updated (uiop:read-file-form "date-updated.lisp-expr"))

;;; Utils
(defun empty? (val)
  "Determine whether 'val' is essentially empty. I.e. is nil, an empty sequence
  or empty string."
  (or (null val)
      (= 0 (length val))))

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

(defun get-league (&key id name)
  "Get a league by id or name."
  (if (and (null id) (empty? name))
      nil
      (let* ((leagues (get-all-leagues)))
        (if (not (empty? name))
            (find name leagues :test #'string-equal :key #'league-name)
            (find id leagues :test #'= :key #'league-id)))))

(defun create-league-from-db (league-key)
  "Create a league struct from a database record."
  (let ((id (parse-integer league-key :start 7)))
    (make-league :id id
                 :name (red-hget league-key "name")
                 :created (red-hget league-key "created")
                 :active? (to-bool (red-hget league-key "active?")))))
;;; Leagues ----------------------------------------------------------------- END

;;; Seasons
(defstruct season
  (id 0)
  (name "")
  (start-date "")
  (end-date ""))

(defun get-seasons (league)
  "Gets all seasons for the specified league."
  (if league
      (redis:with-persistent-connection ()
        (let* ((season-ids (red-smembers (sf "leagues:~A:seasons" (league-id league))))
               (seasons '()))
          (dolist (season-id season-ids)
            (push (create-season-from-db (sf "season:~A" season-id)) seasons))
          (sort seasons #'string< :key #'season-start-date)))))

(defun create-season-from-db (season-key)
  "Create a season struct from a database record."
  (let ((id (parse-integer season-key :start 7)))
    (make-season :id id
                 :name (red-hget season-key "name")
                 :start-date (red-hget season-key "start-date")
                 :end-date (red-hget season-key "end-date"))))
;;; Seasons ----------------------------------------------------------------- END

;;; Games
(defstruct game
  (id 0)
  (date-time "")
  (progress ""))

(defun get-games (seasons)
  "Get all games belonging to the given seasons."
  (if seasons
      (redis:with-persistent-connection ()
        (let* ((game-keys '())
               (game-ids '())
               (games '()))
          (dolist (season seasons)
            (push (sf "seasons:~A:games" (season-id season)) game-keys))
          (dolist (game-key game-keys)
            (setf game-ids (append game-ids (red-smembers game-key))))
          (dolist (game-id game-ids)
            (push (create-game-from-db (sf "game:~A" game-id)) games))
          (sort games #'string< :key #'game-date-time)))))

(defun create-game-from-db (game-key)
  "Create a game struct from a database record."
  (let ((id (parse-integer game-key :start 5)))
    (make-game :id id
               :date-time (red-hget game-key "date-time")
               :progress (red-hget game-key "progress"))))
;;; Games ------------------------------------------------------------------- END

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

(defun get-players (league)
  "Gets a list of all players sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (if league
      (redis:with-persistent-connection ()
        (let* ((player-ids (red-smembers (sf "leagues:~A:players"
                                             (league-id league))))
               (players '()))
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
