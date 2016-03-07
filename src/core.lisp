;;;; Core domain.

(in-package :hockey-oracle.core)

;;; Utils
(defparameter levels '(:success 2 :info 1 :debug 0 :warning -1 :error -2 :fatal -3))

(defstruct r
  "Encapsulates a 'result' indicating the success/failure stated of a function
   or operation. An optional DATA object includes the 'natural' return type of
   the function."
  (level :info)
  (message "")
  (data nil))

(defun new-r (level &optional msg data)
  "Creates a new R."
  (make-r :level (find level levels) :message msg :data data))

(defun succeeded? (obj)
  "Determine whether OBJ represents a 'successful' object."
  (typecase obj
    (r (>= (or (getf levels (r-level obj)) -1) 0))
    (t obj)))

(defun failed? (obj)
  "Determine whether OBJ represents a 'failed' object."
  (not (succeeded? obj)))

(defun first1 (obj)
  "Gets the first item in OBJ if it's a list, otherwise OBJ is simply returned."
  (if (listp obj) (first obj) obj))

(defun second1 (obj &optional fallback)
  "Gets the second item in OBJ if it's a list of at least two items, otherwise
   FALLBACK."
  (cond ((atom obj)
         fallback)
        ((and (listp obj) (> (length obj) 1))
         (second obj))
        (t fallback)))

(defun third1 (obj &optional fallback)
  "Gets the third item in OBJ if it's a list of at least three items, otherwise
   FALLBACK."
  (cond ((atom obj)
         fallback)
        ((and (listp obj) (> (length obj) 2))
         (third obj))
        (t fallback)))

(defun get-secure-key (key)
  "Gets a secure key by calling the 'pass' program."
  (uiop:run-program (sf "pass ~A" key) :output '(:string :stripped t)))

(defun parse-id (key &key idx)
  "Parses an integer id from a redis key. If IDX is non-null it specifies the
   index to retrieve the id from, after the key has been split by colons. If
   IDX is null the last segment of the key is returned."
  (let* ((key-segs (split-sequence #\: key)))
    (if (null idx)
        (parse-integer (last1 key-segs))
        (parse-integer (nth idx key-segs)))))

(defun read-code (string)
  "Read in STRING as source code, without complaining about empty/nil strings."
  (if (empty? string)
      nil
      (read-from-string string)))

(defun to-bool (redis-val)
  "Converts the given redis value to a bool."
  (plusp (parse-integer redis-val)))

(defun random-string (&optional (n 16))
  "Return a random hex string with N digits."
  (ironclad:byte-array-to-hex-string (ironclad:make-random-salt n)))

(defun gen-hash (str &optional salt)
  "Generate a hash of STR."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha512
                             (ironclad:ascii-string-to-byte-array
                              (sf "~A~A" str (or salt ""))))))
;;; Utils ------------------------------------------------------------------- END

;;; Leagues
(defstruct league
  "Defines a hockey league.
   * ID: unique identifier
   * NAME: unique name
   * CREATED: date/time created
   * ACTIVE?: whether it is active/visible"
  (id 0)
  (name "")
  (created "")
  (active? t))

(defun get-all-leagues ()
  "Gets a list of all LEAGUE's sorted by name (A -> Z)."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((league-keys (red-keys "league:*"))
           (leagues nil))
      (dolist (league-key league-keys)
        (push (new-league-from-db league-key) leagues))
      (sort leagues #'string< :key #'league-name))))

(defun get-league (&key id name)
  "Get a LEAGUE by id or name."
  (if (and (null id) (empty? name))
      nil
      (let* ((leagues (get-all-leagues)))
        (if (not (empty? name))
            (find name leagues :test #'string-equal :key #'league-name)
            (find id leagues :test #'= :key #'league-id)))))

(defun new-league-from-db (league-key)
  "Create a LEAGUE struct from the given redis key."
  (let ((id (parse-id league-key)))
    (make-league :id id
                 :name (red-hget league-key "name")
                 :created (red-hget league-key "created")
                 :active? (to-bool (red-hget league-key "active?")))))
;;; Leagues ----------------------------------------------------------------- END

;;; Teams
(defstruct team
  "Defines a hockey team."
  (id 0)
  (name "")
  (logo ""))

(defun get-teams (league)
  "Gets teams belonging to the given LEAGUE."
  (check-type league LEAGUE)
  (if league
      (redis:with-persistent-connection ()
        (let* ((team-ids (red-smembers (sf "leagues:~A:teams" (-> league id))))
               (teams '()))
          (dolist (team-id team-ids)
            (push (new-team-from-db (sf "team:~A" team-id)) teams))
          (sort teams #'string< :key #'team-name)))))

(defun get-team (id)
  "Get TEAM with the given id."
  (if id
      (let ((key (sf "team:~A" id)))
        (redis:with-persistent-connection ()
          (if (red-exists key)
              (new-team-from-db (sf "team:~A" id)))))))

(defun new-team-from-db (team-key)
  "Create a TEAM struct from the given redis key."
  (let ((id (parse-id team-key)))
    (make-team :id id
               :name (red-hget team-key "name")
               :logo (red-hget team-key "logo"))))
;;; Teams ------------------------------------------------------------------- END

;;; Games
(defstruct game
  "Describes a hockey game.
   * TIME: date/time of the game
     * This also acts as a unique identifier within the scope of a league
   * PROGRESS: is the state of the game, and one of:
     * NIL (not yet started)
     * IN-PROGRESS
     * FINAL
   * CONFIRMS: list of GAME-CONFIRM structs"
  (id 0)
  (league nil)
  (time nil)
  (progress nil)
  (home-team nil)
  (away-team nil)
  (home-score 0)
  (away-score 0)
  (confirms '()))

(defun game-confirm-for (game player)
  "Get GAME-CONFIRM (if any) for PLAYER for the game GAME."
  (if game
      (find (player-id player)
            (game-confirms game)
            :key (lambda (x) (player-id (game-confirm-player x))))))

(defun confirmed-players (game)
  "Gets PLAYER's confirmed to play for the given GAME as a list of GAME-CONFIRM
   structs."
  (sort (remove-if (complement (lambda (x) (string-equal :playing
                                                         (-> x confirm-type))))
                   (-> game confirms))
        #'string<
        :key (lambda (x) (player-name (game-confirm-player x)))))

(defun unconfirmed-players (game)
  "Gets PLAYER's that have not confirmed, are not able to play, or are unsure of
   being able to player for the given game, as a list of GAME-CONFIRM structs."
  (let* ((unconfirmed (remove-if (lambda (x) (string-equal :playing
                                                           (-> x confirm-type)))
                                 (-> game confirms))))
    (dolist (p (get-players (game-league game)))
      (unless (find (player-id p) (-> game confirms)
                    :key (lambda (x) (player-id (game-confirm-player x))))
        (push (make-game-confirm :player p
                                 :confirm-type :no-response)
              unconfirmed)))
    (sort unconfirmed
          #'string<
          :key (lambda (x) (player-name (game-confirm-player x))))))

(defstruct game-confirm
  "Describes the confirmation state of a player for a game.
   * PLAYER: the respective PLAYER
   * TIME: when the PLAYER made a response
   * CONFIRM-TYPE: a key value of an item in the plist CONFIRM-TYPES
   * REASON: typically a description of why a player is unable/unsure of
     playing"
  (player nil)
  (time "")
  (confirm-type nil)
  (reason ""))

(defparameter game-confirm-reason-max-length 500)
(defparameter confirm-types '(:no-response "No response"
                              :maybe "Maybe"
                              :cant-play "Can't play"
                              :playing "Playing"))

(defun get-games (league &key exclude-started exclude-unstarted)
  "Get all GAME's belonging to the given LEAGUE."
  (if league
      (redis:with-persistent-connection ()
        (let* ((game-keys (red-keys (sf "leagues:~A:games:*" (-> league id))))
               (games '()))
          (dolist (game-key game-keys)
            (let ((game (new-game-from-db game-key league)))
              (cond (exclude-started
                     (if (empty? (game-progress game))
                         (push game games)))
                    (exclude-unstarted
                     (if (not (empty? (game-progress game)))
                         (push game games)))
                    (t (push game games)))))
          (sort games #'string< :key #'game-time)))))

(defun get-game (league game-id)
  "Get the GAME with the given LEAGUE and GAME-ID."
  (if (or (empty? league) (null game-id))
      nil
      (redis:with-persistent-connection ()
        (let* ((game-key (sf "leagues:~A:games:~A"
                             (-> league id)
                             game-id)))
          (if (redis:red-exists game-key)
              (new-game-from-db game-key league))))))

;; TODO: transactify
(defun save-game-confirm (game player confirm-type &optional reason)
  "Save the game confirm details for the game GAME and player PLAYER. If REASON
   is NIL it is not updated, therefore keeping any previous value.
   Returns an R, with an updated GAME object if successful."
  (check-type game GAME)
  (check-type player PLAYER)
  (if (empty? confirm-type)
      (return-from save-game-confirm
        (new-r :error "No confirm-type provided.")))
  (if (null (find confirm-type confirm-types :test #'string-equal))
      (return-from save-game-confirm
        (new-r :error (sf "Invalid confirm-type, '~A'." confirm-type))))
  (if (string-equal "final" (game-progress game))
      (return-from save-game-confirm
        (new-r :error
               (sf "Can't update confirmation status for completed game."))))
  (if (not (null reason))
      (setf reason (subseq reason
                           0
                           (min game-confirm-reason-max-length
                                (length reason)))))
  (let* ((new-gcs nil))
    (setf game (get-game (game-league game) (game-id game))) ; Get latest game info
    (dolist (gc (game-confirms game))
      (let* ((p-id (player-id (game-confirm-player gc)))
             (p-to-update? (= (player-id player)
                              (player-id (game-confirm-player gc)))))
        (setf (getf new-gcs p-id)
              ;; NOTE: Seem to have to surround each string value in quotes in order
              ;; to be able to read it back in
              (list
               (if p-to-update?
                   confirm-type
                   (game-confirm-confirm-type gc))
               (sf "\"~A\""
                   (if p-to-update?
                       (local-time:now)
                       (game-confirm-time gc)))
               (sf "\"~A\""
                   (if (and p-to-update? (not (null reason)))
                       reason
                       (game-confirm-reason gc)))))))
    (dolist (p (get-players (game-league game)))
      (if (and (not (getf new-gcs (player-id p)))
               (= (player-id p) (player-id player)))
          (setf (getf new-gcs (player-id p))
                (list
                 confirm-type
                 (sf "\"~A\"" (local-time:now))
                 (sf "\"~A\"" (or reason ""))))))
    (redis:with-persistent-connection ()
      (red-hset (sf "leagues:~A:games:~A"
                    (league-id (game-league game))
                    (game-id game))
                "confirms"
                new-gcs))
    (new-r :success "" (get-game (game-league game) (game-id game)))))

(defun new-game-from-db (game-key league)
  "Create a GAME struct based on the given redis key."
  (let ((id (parse-id game-key)))
    (make-game :id id
               :league league
               :time (red-hget game-key "time")
               :progress (red-hget game-key "progress")
               :home-team (new-team-from-db (sf "team:~A"
                                                (red-hget game-key
                                                          "home-team")))
               :away-team (new-team-from-db (sf "team:~A"
                                                (red-hget game-key
                                                          "away-team")))
               :home-score (parse-integer (red-hget game-key "home-score"))
               :away-score (parse-integer (red-hget game-key "away-score"))
               :confirms (new-game-confirm
                          (read-code (red-hget game-key "confirms"))))))

(defun new-game-confirm (plist)
  "The key of PLIST is expected to be a player id and the value a list of the
   form: (CONFIRM-TYPE TIME REASON). REASON is optional."
  (let ((game-confirms '()))
    (doplist (player-id confirm-info plist)
             (push (make-game-confirm :player (get-player player-id)
                                      :confirm-type (find (first1 confirm-info)
                                                          confirm-types
                                                          :test #'string-equal)
                                      :time (second1 confirm-info "")
                                      :reason (third1 confirm-info ""))
                   game-confirms))
    game-confirms))
;;; Games ------------------------------------------------------------------- END

;;; Players
(defstruct player
  "Describes a player/user.
   * ID: unique identifier (across all leagues)
   * NAME: his/her name
   * AUTH: current authentication (hashed and salted password)
   * TEMP-AUTH: a random short-lived authentication token
   * PERM-AUTH: a random longer-lived authentication token
   * SALT: unique salt used in AUTH
   * POSITION: default position
   * ACTIVE?: whether active/able to play"
  (id 0)
  (name "")
  (auth "")
  (perm-auth "")
  (temp-auth "")
  (salt "")
  (position "")
  (active? t))

(defparameter players-positions '("C" "D" "G" "LW" "RW"))

(defun get-all-players ()
  "Gets a list of all PLAYER's sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((player-keys (red-keys "player:*"))
           (players nil))
      (dolist (player-key player-keys)
        (push (new-player-from-db player-key) players))
      (sort players #'string< :key #'player-name))))

(defun get-players (league)
  "Gets a list of all PLAYER's belonging the given LEAGUE, sorted by first name."
  ;; TODO: Use (red-scan) instead of (red-keys) to get player keys
  ;; TODO: Use pipelines to send multiple commands at once
  (if league
      (redis:with-persistent-connection ()
        (let* ((player-ids (red-smembers (sf "leagues:players:~A"
                                             (league-id league))))
               (players '()))
          (dolist (player-id player-ids)
            (push (new-player-from-db (sf "player:~A" player-id)) players))
          (sort players #'string< :key #'player-name)))))

(defun get-player (id &key pwd perm-auth temp-auth)
  "Gets the PLAYER with the given ID. If PWD is non-null it is compared against
   PLAYER-AUTH, and only returned if they match. If PERM-AUTH is non-null it is
   compared against PLAYER-PERM-AUTH, and only returned if they match. If
   TEMP-AUTH is non-null it is compared against PLAYER-TEMP-AUTH, and only
   returned if non-null."
  (if id
      (let* ((player-key (sf "player:~A" id))
             (player nil))
        (redis:with-persistent-connection ()
          (when (red-exists player-key)
            (setf player (new-player-from-db player-key))
            (cond (pwd
                   (if (string= (player-auth player)
                                (gen-hash pwd (player-salt player)))
                       player))
                  (perm-auth
                   (if (string= (player-perm-auth player) perm-auth)
                       player))
                  (temp-auth
                   (if (string= (player-temp-auth player) temp-auth)
                       player))
                  (t player)))))))

;; TODO: Wrap DB updates in transaction
(defun change-player-temp-auth (player &optional new-auth)
  "Change the temporary authentication token of PLAYER to NEW-AUTH if non-null,
   or a random hex string otherwise. The new temporary authentication token is
   returned if successful, otherwise NIL."
  (check-type player PLAYER)
  (if player
      (let* ((player-key (sf "player:~A" (player-id player)))
             (new-auth (or new-auth (random-string))))
        (redis:with-persistent-connection ()
          (when (red-exists player-key)
            (red-hset player-key "temp-auth" new-auth)
            new-auth)))))

;; TODO: Wrap DB updates in transaction
(defun change-player-pwd (player pwd)
  "Change the password of PLAYER to PWD. The hashed and salted password is
   returned if successful, otherwise NIL."
  (check-type player PLAYER)
  (if (and player pwd)
      ;; Generate a new salt whenever password is changed
      (let* ((salt (random-string))
             (new-auth (gen-hash pwd salt))
             (player-key (sf "player:~A" (player-id player))))
        (redis:with-persistent-connection ()
          (when (red-exists player-key)
            (red-hset player-key "auth" new-auth)
            (red-hset player-key "salt" salt)
            (red-hset player-key "perm-auth" (random-string 128))
            new-auth)))))

(defun new-player-from-db (player-key)
  "Create a PLAYER struct from the given redis key."
  (let ((id (parse-id player-key)))
    (make-player :id id
                 :name (red-hget player-key "name")
                 :auth (red-hget player-key "auth")
                 :perm-auth (red-hget player-key "perm-auth")
                 :temp-auth (red-hget player-key "temp-auth")
                 :salt (red-hget player-key "salt")
                 :position (red-hget player-key "position")
                 :active? (red-sismember "players:active" id))))
;;; Players ----------------------------------------------------------------- END
