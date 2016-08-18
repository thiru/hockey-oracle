;;;; Core domain.

(in-package :hockey-oracle.core)

;;; Utils
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
  (if (empty? redis-val)
      nil
      (plusp (parse-integer redis-val))))

(defun random-string (&optional (n 16))
  "Return a random hex string with N digits."
  (ironclad:byte-array-to-hex-string (ironclad:make-random-salt n)))

(defun gen-hash (str &optional salt)
  "Generate a hash of STR."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha512
                             (ironclad:ascii-string-to-byte-array
                              (sf "~A~A" str (or salt ""))))))

(defun pretty-time (time-str)
  "Formats a date/time to a user-friendly form. 'time-str' is expected to be a
   timestamp readable by LOCAL-TIME."
  (if (empty? time-str)
      ""
      (let* ((format-desc '())
             (timestamp (parse-timestring time-str)))

        ;; NOTE: This format needs to be in sync with the front-end date format
        (setf format-desc '(:short-weekday " " :short-month " " :day
                            ", " :hour12 ":" (:min 2) " " :ampm))

        (format-timestring nil timestamp :format format-desc))))
;;; Utils ------------------------------------------------------------------- END

;;; Leagues
(defstruct league
  "Defines a hockey league.
   * ID: unique identifier
   * NAME: unique short name (e.g. NHL)
   * FULL-NAME: full-length name (e.g. National Hockey League)
   * CREATED: date/time created
   * ACTIVE?: whether it is active/visible
   * COMMISSIONER-IDS: a list of players ids considered commissioners of this
     league
   * GAME-REMINDER-DAY-OFFSET: the number of days ahead of game to send an email
     reminder to players
   * GAME-REMINDER-TIME: the time of day to send the game email reminder
   * SEND-AUTOMATED-EMAILS?: whether to send automated email reminders,
     notifications, etc. to players"
  (id 0)
  (name "")
  (full-name "")
  (created "")
  (active? t)
  (commissioner-ids '())
  (game-reminder-day-offset 0)
  (game-reminder-time "")
  (send-automated-emails? t))

(defun get-all-leagues ()
  "Gets a list of all LEAGUE's sorted by name (A -> Z)."
  ;; TODO: Use pipelines to send multiple commands at once
  (redis:with-persistent-connection ()
    (let* ((league-ids (red-smembers "leagues"))
           (leagues nil))
      (dolist (league-id league-ids)
        (push (new-league-from-db (sf "league:~A" league-id)) leagues))
      (sort leagues #'string< :key #'league-name))))

(defun get-league (&key id name)
  "Get a LEAGUE by id or name."
  (if (and (null id) (empty? name))
      nil
      (let* ((leagues (get-all-leagues)))
        (if (not (empty? name))
            (find name leagues :test #'string-equal :key #'league-name)
            (find id leagues :test #'= :key #'league-id)))))

(defun update-league (league)
  "Update editable league details.
   Returns an R."
  (if (empty? league)
      (return-from update-league
        (new-r :error "No league provided." league)))
  (check-type league LEAGUE)
  (if (minusp (league-game-reminder-day-offset league))
      (return-from update-league
        (new-r :error (sf '("Day offset for game email reminder must be a "
                            "positive integer but was ~A.")
                          (league-game-reminder-day-offset league)))))
  (if (empty? (league-game-reminder-time league))
      (return-from update-league
        (new-r :error "Game email reminder time was not specified.")))
  (let ((league-key (sf "league:~A" (league-id league))))
    (redis:with-persistent-connection ()
      (when (red-exists league-key)
        (red-hset league-key
                  "send-automated-emails?"
                  (if (league-send-automated-emails? league) 1 0))
        (red-hset league-key
                  "game-reminder-day-offset"
                  (league-game-reminder-day-offset league))
        (red-hset league-key
                  "game-reminder-time"
                  (league-game-reminder-time league)))))
  (new-r :success "Update successful!" league))

(defun new-league-from-db (league-key)
  "Create a LEAGUE struct from the given redis key."
  (let* ((id (parse-id league-key))
         (commish-key (sf "leagues:~A:commissioners" id)))
    (make-league :id id
                 :name (red-hget league-key "name")
                 :full-name (red-hget league-key "full-name")
                 :created (red-hget league-key "created")
                 :active? (to-bool (red-hget league-key "active?"))
                 :commissioner-ids (map 'list
                                        #'parse-integer
                                        (red-smembers commish-key))
                 :game-reminder-day-offset
                 (parse-integer
                  (or (red-hget league-key "game-reminder-day-offset") "0"))
                 :game-reminder-time (red-hget league-key "game-reminder-time")
                 :send-automated-emails?
                 (to-bool (red-hget league-key "send-automated-emails?")))))
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
   * PROGRESS: is the state of the game, and defined in GAME-PROGRESS-STATES
   * CONFIRMS: list of GAME-CONFIRM structs
   * EMAIL-REMINDER: the time to send an email reminder to players about this
     game"
  (id 0)
  (created-at "")
  (created-by "")
  (league nil)
  (time nil)
  (progress nil)
  (home-team nil)
  (away-team nil)
  (home-score 0)
  (away-score 0)
  (email-reminder "")
  (confirms '()))

(defparameter game-progress-states '(:new :underway :final))

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

(defun get-games (&key league exclude-started exclude-unstarted)
  "Get all GAME's belonging with the specified criteria."
  (if (non-empty? league)
      (check-type league LEAGUE))
  (redis:with-persistent-connection ()
    (let* ((league-ids '())
           (games '()))
      (setf league-ids
            (if league
                (list (league-id league))
                (map 'list #'parse-integer (red-smembers "leagues"))))
      (if (empty? league-ids)
          (return-from get-games))
      (dolist (league-id league-ids)
        (dolist (game-id (red-smembers (sf "league:~A:games" league-id)))
          (let* ((game-key (sf "game:~A" game-id))
                 (game (if (red-exists game-key) (new-game-from-db game-key))))
            (if game
                (cond (exclude-started
                       (if (or (empty? (game-progress game))
                               (string-equal "new" (game-progress game)))
                           (push game games)))
                      (exclude-unstarted
                       (if (or (string-equal "underway" (game-progress game))
                               (string-equal "final" (game-progress game)))
                           (push game games)))
                      (t (push game games)))))))
      (sort games #'string< :key #'game-time))))

(defun get-game (id)
  "Get the GAME with the specified id."
  (if id
      (redis:with-persistent-connection ()
        (let* ((game-key (sf "game:~A" id)))
          (if (redis:red-exists game-key)
              (new-game-from-db game-key))))))

(defun get-upcoming-games (league count)
  "Get the next COUNT games that haven't yet started."
  (let* ((new-games (get-games :league league :exclude-started t))
         (compare-time (adjust-timestamp (now) (offset :hour -6)))
         (upcoming-games '()))
    (setf upcoming-games
          (remove-if-not (lambda (x)
                           (timestamp>=
                            (parse-timestring (game-time x))
                            compare-time))
                         new-games))
    (subseq upcoming-games 0 (min (length upcoming-games) count))))

;; TODO: transactify
(defun save-new-game (league time user)
  "Save a new game for LEAGUE at TIME by USER.
   Returns an R, containing the new game if successful."
  (if (null league)
      (return-from save-new-game
        (new-r :error "No league specified.")))
  (check-type league LEAGUE)
  (if (empty? time)
      (return-from save-new-game
        (new-r :error "No game time specified.")))
  (if (null (parse-timestring time :fail-on-error nil))
      (return-from save-new-game
        (new-r :error (sf "Game date/time '~A' invalid." time))))
  (if (null user)
      (return-from save-new-game
        (new-r :error "No user/player specified.")))
  (check-type user PLAYER)
  (if (not (is-commissioner? user league))
      (return-from save-new-game
        (new-r :error
               (sf "You do not have permission to create new games in ~A."
                   (league-name league)))))
  (redis:with-persistent-connection ()
    (let* ((id-seed-key "games:id-seed")
           (game-id (red-get id-seed-key))
           (game-key (sf "game:~A" game-id))
           (email-time (adjust-timestamp (parse-timestring time)
                         (offset :day
                                 (- (league-game-reminder-day-offset league)))
                         (set :hour
                              (parse-integer
                               (first1 (split-sequence
                                        #\:
                                        (league-game-reminder-time league)))))
                         (set :minute
                              (parse-integer
                               (second1
                                (split-sequence
                                 #\:
                                 (league-game-reminder-time league)))))))
           (new-game))
      (red-hset game-key "created-at" (now))
      (red-hset game-key "created-by" (player-id user))
      (red-hset game-key "time" time)
      (red-hset game-key "league" (league-id league))
      (red-hset game-key "home-team" 1) ; TODO: remove hard-coding
      (red-hset game-key "away-team" 2) ; TODO: remove hard-coding
      (red-hset game-key "home-score" 0)
      (red-hset game-key "away-score" 0)
      (red-hset game-key "email-reminder" (to-string email-time))
      (red-sadd (sf "league:~A:games" (league-id league)) game-id)
      (red-incr id-seed-key)
      (red-zadd "emails:game-reminders"
                (timestamp-to-universal email-time)
                game-id)
      (setf new-game (get-game game-id))
      (new-r :success
             (sf "Created new game on ~A!" (pretty-time time))
             new-game))))

;; TODO: transactify
(defun update-game-info (game user time progress)
  "Update the game time and progress.
   Returns an R, with the updated game if successful."
  (if (null game)
      (return-from update-game-info
        (new-r :error "No game specified." game)))
  (if (null user)
      (return-from update-game-info
        (new-r :error "No user/player specified." game)))
  (if (empty? progress)
      (return-from update-game-info
        (new-r :error "No game progress specified." game)))
  (if (not (is-commissioner? user (game-league game)))
      (return-from update-game-info
        (new-r :error
               (sf "You do not have permission to update games in ~A."
                   (league-name league)))))
  (redis:with-persistent-connection ()
    (let* ((league (game-league game))
           (game-key (sf "game:~A" (game-id game)))
           (email-time (adjust-timestamp (parse-timestring time)
                         (offset :day
                                 (- (league-game-reminder-day-offset league)))
                         (set :hour
                              (parse-integer
                               (first1 (split-sequence
                                        #\:
                                        (league-game-reminder-time league)))))
                         (set :minute
                              (parse-integer
                               (second1
                                (split-sequence
                                 #\:
                                 (league-game-reminder-time league)))))))
           (updated-game nil))
      (red-hset game-key "updated-at" (now))
      (red-hset game-key "updated-by" (player-id user))
      (red-hset game-key "time" time)
      (red-hset game-key "progress" progress)
      (red-hset game-key "email-reminder" (to-string email-time))
      (red-zrem "emails:game-reminders" (game-id game))
      (red-zadd "emails:game-reminders"
                (timestamp-to-universal email-time)
                (game-id game))
      (setf updated-game (get-game (game-id game)))
      (new-r :success "Updated game!" updated-game))))

(defun delete-game (game user)
  "Delete GAME.
   Returns an R."
  (if (null game)
      (return-from delete-game
        (new-r :error "No game specified." game)))
  (check-type game GAME)
  (if (null user)
      (return-from delete-game
        (new-r :error "No user/player specified." game)))
  (check-type user PLAYER)
  (if (not (is-commissioner? user (game-league game)))
      (return-from delete-game
        (new-r :error
               (sf "You do not have permission to update games in ~A."
                   (league-name league)))))
  (redis:with-persistent-connection ()
    (let* ((league-id (league-id (game-league game)))
           (game-key (sf "game:~A" (game-id game))))
      (red-srem (sf "league:~A:games" league-id) (game-id game))
      (red-del game-key)
      (red-zrem "emails:game-reminders" (game-id game))
      (new-r :success "Deleted game!" game))))

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
    ;; Get latest game info
    (setf game (get-game (game-id game)))
    (dolist (gc (game-confirms game))
      (let* ((p-id (player-id (game-confirm-player gc)))
             (p-to-update? (= (player-id player)
                              (player-id (game-confirm-player gc)))))
        (setf (getf new-gcs p-id)
              (list
               (if p-to-update?
                   confirm-type
                   (game-confirm-confirm-type gc))
               (if p-to-update?
                   (to-string (now))
                   (game-confirm-time gc))
               (if (and p-to-update? (not (null reason)))
                   reason
                   (game-confirm-reason gc))))))
    (dolist (p (get-players (game-league game)))
      (if (and (not (getf new-gcs (player-id p)))
               (= (player-id p) (player-id player)))
          (setf (getf new-gcs (player-id p))
                (list confirm-type
                      (to-string (now))
                      (or reason "")))))
    (redis:with-persistent-connection ()
      (red-hset (sf "game:~A" (game-id game)) "confirms" new-gcs))
    (new-r :success "" (get-game (game-id game)))))

(defun new-game-from-db (game-key)
  "Create a GAME struct based on the given redis key."
  (let ((id (parse-id game-key)))
    (make-game :id id
               :created-at (red-hget game-key "created-at")
               :created-by (get-player :id (red-hget game-key "created-by"))
               :league
               (get-league :id (parse-integer (red-hget game-key "league")))
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
               :email-reminder (red-hget game-key "email-reminder")
               :confirms (new-game-confirm
                          (read-code (red-hget game-key "confirms"))))))

(defun new-game-confirm (plist)
  "The key of PLIST is expected to be a player id and the value a list of the
   form: (CONFIRM-TYPE TIME REASON). REASON is optional."
  (let ((game-confirms '()))
    (doplist (player-id confirm-info plist)
             (push (make-game-confirm :player (get-player :id player-id)
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
   * ADMIN?: whether a site-wide administrator
   * EMAIL: email address
   * NOTIFY-IMMEDIATELY?: notify immediately if upcoming game state changes
   * AUTH: current authentication (hashed and salted password)
   * TEMP-AUTH: a random short-lived authentication token
   * PERM-AUTH: a random longer-lived authentication token
   * SALT: unique salt used in AUTH
   * POSITION: default position
   * ACTIVE?: whether active/able to play"
  (id 0)
  (name "")
  (admin? nil)
  (email "")
  (notify-immediately? nil)
  (auth "")
  (perm-auth "")
  (temp-auth "")
  (salt "")
  (position "")
  (active? t))

(defparameter players-positions '("C" "D" "G" "LW" "RW"))
(defparameter player-name-max-length 100)
(defparameter player-email-max-length 254)

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
        (let* ((player-ids (red-smembers (sf "leagues:~A:players"
                                             (league-id league))))
               (players '()))
          (dolist (player-id player-ids)
            (push (new-player-from-db (sf "player:~A" player-id)) players))
          (sort players #'string< :key #'player-name)))))

(defun get-player (&key
                     (id 0 id-given?)
                     name
                     email
                     league
                     (pwd "" pwd-given?)
                     (perm-auth "" perm-auth-given?)
                     (temp-auth "" temp-auth-given?))
  "Gets the PLAYER with the given ID or matching one or more of NAME, EMAIL and
   LEAUGE. If more than one PLAYER is found NIL is returned.
   If PWD is non-null it is compared against PLAYER-AUTH, and only returned if
   they match. If PERM-AUTH is non-null it is compared against PLAYER-PERM-AUTH,
   and only returned if they match. If TEMP-AUTH is non-null it is compared
   against PLAYER-TEMP-AUTH, and only returned if non-null."
  (let ((player nil))
    (if id-given?
        (let* ((player-key (sf "player:~A" id)))
          (redis:with-persistent-connection ()
            (when (red-exists player-key)
              (setf player (new-player-from-db player-key)))))
        (let* ((players (if (null league)
                            (get-all-players)
                            (get-players league)))
               (players (remove-if-not
                         (lambda (p)
                           (and
                            (or (empty? name)
                                (string-equal name (player-name p)))
                            (or (empty? email)
                                (string-equal email (player-email p)))))
                         players)))
          (if (= 1 (length players))
              (setf player (first players)))))
    (if player
        (cond (pwd-given?
               (if (string= (player-auth player)
                            (gen-hash pwd (player-salt player)))
                   player))
              (perm-auth-given?
               (if (string= (player-perm-auth player) perm-auth)
                   player))
              (temp-auth-given?
               (if (string= (player-temp-auth player) temp-auth)
                   player))
              (t player)))))

(defun get-emailable-players (league &key immediate-notify-only?)
  "Get email addresses of all players in LEAGUE.
   If IMMEDIATE-NOTIFY-ONLY? is T, only players who requested immediate email
   notification will be returned."
  (remove-if (complement
              (lambda (p)
                (and (or (not immediate-notify-only?)
                         (player-notify-immediately? p))
                     (not (empty? (player-email p))))))
             (get-players league)))

(defun is-commissioner? (player league)
  "Check whether PLAYER is a commission of LEAGUE."
  (or (and player (player-admin? player))
      (and player
           league
           (find (player-id player)
                 (league-commissioner-ids league)))))

(defun reset-pwd-get-token (player)
  "Get the password reset request key.
   Returns the key if found, otherwise NIL."
  (check-type player PLAYER)
  (redis:with-persistent-connection ()
    (red-get (sf "pwd-reset:player:~A" (player-id player)))))

(defun reset-pwd-set-token (player)
  "Initiate a password reset for PLAYER.
   Returns the password reset request token."
  (check-type player PLAYER)
  (let* ((key (sf "pwd-reset:player:~A" (player-id player)))
         (val (sf "~A-~A" (player-id player) (random-string))))
    (redis:with-persistent-connection ()
      (red-set key val)
      (red-expire key (* 60 60 24)) ; Expire key within a day
      val)))

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
  "Change the password of PLAYER to PWD. An R object is returned including the
   updated player, if successful."
  (if (null player)
      (return-from change-player-pwd
        (new-r :error "No player provided." player)))
  (if (empty? pwd)
      (return-from change-player-pwd
        (new-r :error "New password can't be empty." player)))
  (check-type player PLAYER)
  ;; Generate a new salt whenever password is changed
  (let* ((salt (random-string))
         (new-auth (gen-hash pwd salt))
         (player-key (sf "player:~A" (player-id player)))
         (pwd-reset-key (sf "pwd-reset:player:~A" (player-id player))))
    (redis:with-persistent-connection ()
      (if (not (red-exists player-key))
          (return-from change-player-pwd
            (new-r :error (sf "Player with id ~A not found."
                              (player-id player))
                   player)))
      (red-hset player-key "auth" new-auth)
      (red-hset player-key "salt" salt)
      (red-hset player-key "perm-auth" (random-string 128))
      (if (red-exists pwd-reset-key)
          (red-del pwd-reset-key)))
    (new-r :success "Password updated!" (get-player :id (player-id player)))))

;; TODO: transactify
(defun update-player (player)
  "Update player details such as name, email, etc.
   Returns an R."
  (check-type player PLAYER)
  (if (empty? player)
      (return-from update-player
        (new-r :error "No player provided." (player-id player))))
  (if (> (length (player-name player)) player-name-max-length)
      (return-from update-player
        (new-r :error
               (sf '("Name is too long (~A characters). "
                     "The maximum length is ~A characters.")
                   (length (player-name player))
                   player-name-max-length)
               player)))
  (if (> (length (player-email player)) player-email-max-length)
      (return-from update-player
        (new-r :error
               (sf '("Email address is too long (~A characters). "
                     "The maximum length is ~A characters.")
                   (length (player-email player))
                   player-email-max-length)
               player)))
  (if (null (find (player-position player)
                  players-positions
                  :test #'string-equal))
      (return-from update-player
        (new-r :error
               (sf "Invalid player position '~A'. Position must be one of: ~A."
                   (player-position player) players-positions)
               player)))
  (if (and (not (empty? (player-email player)))
           (find-if (lambda (p)
                      (and
                       (/= (player-id player) (player-id p))
                       (string-equal (player-email player) (player-email p))))
                    (get-all-players)))
      (return-from update-player
        (new-r :error
               (sf "Sorry, this email address is taken by another player."))))
  (let ((player-key (sf "player:~A" (player-id player))))
    (redis:with-persistent-connection ()
      (when (red-exists player-key)
        (red-hset player-key "name" (player-name player))
        (red-hset player-key "email" (player-email player))
        (red-hset player-key "notify-immediately?"
                  (if (player-notify-immediately? player) 1 0))
        (red-hset player-key "position" (player-position player)))))
  (new-r :success "Update successful!" player))

(defun new-player-from-db (player-key)
  "Create a PLAYER struct from the given redis key."
  (let ((id (parse-id player-key)))
    (make-player :id id
                 :name (red-hget player-key "name")
                 :admin? (to-bool (find (sf "~A" id)
                                        (red-smembers "admin:users")
                                        :test #'string-equal))
                 :email (red-hget player-key "email")
                 :notify-immediately?
                 (to-bool (red-hget player-key "notify-immediately?"))
                 :auth (red-hget player-key "auth")
                 :perm-auth (red-hget player-key "perm-auth")
                 :temp-auth (red-hget player-key "temp-auth")
                 :salt (red-hget player-key "salt")
                 :position (red-hget player-key "position")
                 :active? (red-sismember "players:active" id))))
;;; Players ----------------------------------------------------------------- END

;;; Web Server
(defstruct server-info
  "Describes web server details."
  (host ""))

(defun get-server-info ()
  "Get web server details."
  (let* ((key "web"))
    (redis:with-persistent-connection ()
      (make-server-info :host
                        (red-hget key "host")))))
;;; Web Server -------------------------------------------------------------- END

;;; Email
(defun send-email (subject message &optional to)
  "Sends an HTML email with the specified parameters. If TO is not specified the
   email is sent to the administrator. The email is sent in a background thread."
  (if (empty? subject)
      (return-from send-email
        (new-r :error "No subject provided.")))
  (if (empty? message)
      (return-from send-email
        (new-r :error "No message provided.")))
  (redis:with-persistent-connection ()
    (let ((username (red-hget "admin:email" "username"))
          (pwd (red-hget "admin:email" "pwd"))
          (server (red-hget "admin:email" "server"))
          (reply-to (red-hget "admin:email" "reply-to")))
      (if (empty? to)
          (setf to (red-hget "admin:email" "admin")))
      (bt:make-thread (lambda ()
                        (cl-smtp:send-email server reply-to to subject ""
                                            :display-name "Hockey Oracle"
                                            :html-message message
                                            :ssl :tls
                                            :authentication `(,username ,pwd)))))))

(defun send-email-to-players (subject get-message league &key immediate-notify-only?)
  "Sends an HTML email to certain players belonging to LEAGUE.
   GET-MESSAGE is a func that takes a PLAYER and returns the body of the email.
   If IMMEDIATE-NOTIFY-ONLY? is T, only players who requested immediate email
   notification will be included."
  (if (empty? subject)
      (return-from send-email-to-players
        (new-r :error "No subject provided.")))
  (if (null get-message)
      (return-from send-email-to-players
        (new-r :error "No func provided to get message.")))
  (if (null league)
      (return-from send-email-to-players
        (new-r :error "No league provided.")))
  (if (not (league-send-automated-emails? league))
      (return-from send-email-to-players
        (new-r :info (sf "Automated emails currently disabled for league, '~A'."
                         (league-name league)))))
  (let* ((players (get-emailable-players
                   league
                   :immediate-notify-only? immediate-notify-only?)))
    (if (empty? players)
        (return-from send-email-to-players
          (new-r :info
                 (sf "No players have email addresses in the league, '~A'."
                     (league-name league)))))
    (dolist (player players)
      (send-email subject (funcall get-message player) (player-email player)))
    (new-r :success (sf "Sent email to ~A recipients." (length players)))))
;;; Email ------------------------------------------------------------------- END
