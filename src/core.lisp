;;;; Core domain.

(in-package :hockey-oracle.core)

;;; Utils
(defun get-secure-key (key)
  "Gets a secure key by calling the 'pass' program."
  (uiop:run-program (sf "pass ~A" key) :output '(:string :stripped t)))

(defun parse-id (key &key idx)
  "Parses an integer id from a redis key. If 'idx' is non-null it specifies the
   index to retrieve the id from, after the key has been split between colons.
   If 'idx' is null the last segment of the key is retrieved."
  (let* ((key-segs (split-sequence #\: key)))
    (if (null idx)
        (parse-integer (last1 key-segs))
        (parse-integer (nth idx key-segs)))))

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

;;; Games
(defstruct game
  (date-time nil)
  (progress nil)
  (home-score 0)
  (away-score 0)
  (confirms '()))

(defun get-games (league)
  "Get all games belonging to the given league."
  (if league
      (redis:with-persistent-connection ()
        (let* ((game-keys (red-keys (sf "leagues:~A:games:*" (-> league id))))
               (games '()))
          (dolist (game-key game-keys)
            (push (new-game-from-db game-key) games))
          (sort games #'string< :key #'game-date-time)
          ))))

(defun new-game-from-db (game-key)
  "Create a game struct from a database record."
  (let ((date-time (last1 (split-sequence #\: game-key))))
    (make-game :date-time date-time
               :progress (red-hget game-key "progress")
               :home-score (parse-integer (red-hget game-key "home-score"))
               :away-score (parse-integer (red-hget game-key "away-score"))
               :confirms (red-hget game-key "confirms"))))
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
