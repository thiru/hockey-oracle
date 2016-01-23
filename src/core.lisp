;;;; Core domain.

(in-package :hockey-oracle.core)

;;; Utils
(defun get-secure-key (key)
  "Gets a secure key by calling the 'pass' program."
  (uiop:run-program (sf "pass ~A" key) :output '(:string :stripped t)))

(defun parse-id (key)
  "Parses an integer id from a redis key. The key is expected to end with a
   colon and a series of numbers, specifying the key."
  (let* ((last-idx (position #\: key :from-end t)))
    (if (null last-idx) (error "'key' does not have a colon."))
    (parse-integer key :start (1+ last-idx))))

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
        (push (new-league-from-db league-key) leagues))
      (sort leagues #'string< :key #'league-name))))

(defun get-league (&key id name)
  "Get a league by id or name."
  (if (and (null id) (empty? name))
      nil
      (let* ((leagues (get-all-leagues)))
        (if (not (empty? name))
            (find name leagues :test #'string-equal :key #'league-name)
            (find id leagues :test #'= :key #'league-id)))))

(defun new-league-from-db (league-key)
  "Create a league struct from a database record."
  (let ((id (parse-id league-key)))
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
        (let* ((season-ids (red-smembers (sf "leagues:seasons:~A"
                                             (league-id league))))
               (seasons '()))
          (dolist (season-id season-ids)
            (push (new-season-from-db (sf "season:~A" season-id)) seasons))
          (sort seasons #'string< :key #'season-start-date)))))

(defun new-season-from-db (season-key)
  "Create a season struct from a database record."
  (let ((id (parse-id season-key)))
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
            (push (sf "seasons:games:~A" (season-id season)) game-keys))
          (dolist (game-key game-keys)
            (setf game-ids (append game-ids (red-smembers game-key))))
          (dolist (game-id game-ids)
            (push (new-game-from-db (sf "game:~A" game-id)) games))
          (sort games #'string< :key #'game-date-time)))))

(defun new-game-from-db (game-key)
  "Create a game struct from a database record."
  (let ((id (parse-id game-key)))
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

(defparameter players-positions '("C" "D" "G" "LW" "RW"))

(defun get-all-players ()
  "Gets a list of all players sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((player-keys (red-keys "player:*"))
           (players nil))
      (dolist (player-key player-keys)
        (push (new-player-from-db player-key) players))
      (sort players #'string< :key #'player-first-name))))

(defun get-players (league)
  "Gets a list of all players sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (if league
      (redis:with-persistent-connection ()
        (let* ((player-ids (red-smembers (sf "leagues:players:~A"
                                             (league-id league))))
               (players '()))
          (dolist (player-id player-ids)
            (push (new-player-from-db (sf "player:~A" player-id)) players))
          (sort players #'string< :key #'player-first-name)))))

(defun new-player-from-db (player-key)
  "Create a player struct from a database record."
  (let ((id (parse-id player-key)))
    (make-player :id id
                 :first-name (red-hget player-key "first-name")
                 :last-name (red-hget player-key "last-name")
                 :position (red-hget player-key "position")
                 :active? (red-sismember "players:active" id))))
;;; Players ----------------------------------------------------------------- END
