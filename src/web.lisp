;;;; Web-specific functionality.

(in-package :hockey-oracle.web)

;;; General
(defvar *debug* t)
(defvar main-acceptor nil "The global web-server instance.")
(defvar static-files-dir (merge-pathnames "www/" base-dir))

(setf (html-mode) :HTML5)

(defun create-acceptor (&key (port 9090) debug)
  "Creates an 'easy-acceptor' which will listen on the specified port."
  (make-instance 'easy-acceptor
                 :port port
                 :document-root static-files-dir
                 :access-log-destination (if debug
                                             *standard-output*
                                             "~/tbnl-access.log")
                 :message-log-destination (if debug
                                              *standard-output*
                                              "~/tbnl-message.log")))

(defun start-server! (&key (port 9090) debug)
  "Starts the web server.
   @param port:
     Specifies the port for the web server.
   @param debug:
     If T, the server is started with access and message logs sent to standard
     out, and the following hunchentoot special variable settings:
     * *CATCH-ERRORS-P* => NIL
     * *SHOW-LISP-ERRORS-P* => T
   Side-effects: sets the special variable MAIN-ACCEPTOR to the created
   acceptor, and *DEBUG* to the value of DEBUG."
  (setf *debug* debug)
  (setf main-acceptor (create-acceptor :port port :debug debug))
  (when debug
    (setf *catch-errors-p* nil)
    (setf *show-lisp-errors-p* t))
  (start main-acceptor))

(defun stop-server ()
  "Stops the web server referenced by the special variable main-acceptor."
  (if main-acceptor
      (stop main-acceptor :soft t)))

(defun send-error-email (message)
  "Sends an email indicating a server error occurred."
  (let ((auth '()))
    (push (get-secure-key "ho/email/pwd") auth)
    (push (get-secure-key "ho/email/username") auth)
    (cl-smtp:send-email (get-secure-key "ho/email/server")
                        (get-secure-key "ho/email/reply-to")
                        (get-secure-key "ho/email/admins")
                        "Server Error" ""
                        :display-name "Hockey Oracle"
                        :html-message message :ssl :tls
                        :port (parse-integer
                               (get-secure-key "ho/email/ssl-port"))
                        :authentication auth)))
;;; General ----------------------------------------------------------------- END

;;; Utils
(defmacro html-snippet (root-tag)
  "Generate HTML given a single root HTML tag."
  `(with-html-output-to-string (*standard-output* nil :indent t)
     ,root-tag))

(defmacro safe-parse-int (str &key (fallback 0))
  "Lenient parsing of 'str'."
  `(if (empty? ,str)
       ,fallback
       (or (parse-integer ,str :junk-allowed t) ,fallback)))

(defun based-on-path? (path base-path)
  "Determine whether 'path' is based on 'base-path'."
  (let ((path-segs (split-sequence #\/
                                   path
                                   :remove-empty-subseqs t)))
    (or (string-equal (first path-segs) base-path)
        (string-equal (second path-segs) base-path))))

(defun path-segments (req)
  "Gets a list of path segments, excluding query parameters."
  (split-sequence #\/ (script-name* req) :remove-empty-subseqs t))

(defun pretty-time (time-str &optional mode)
  "Formats a date/time to a user-friendly form. 'time-str' is expected to be a
   timestamp readable by LOCAL-TIME. MODE can be FULL or SHORT."
  (if (empty? time-str)
      ""
      (let* ((format-desc '())
             (timestamp (parse-timestring time-str)))

        (if (eq 'short mode)
            (setf format-desc '(:short-weekday " " :short-month " " :day " "
                                :hour12 ":" (:min 2) :ampm))
            (setf format-desc '(:long-weekday " " :short-month " " :day " "
                                :year " @ " :hour12 ":" (:min 2) :ampm)))

        (format-timestring nil timestamp :format format-desc))))

(defun parse-league (req)
  "Parses the request path to obtain the league defined as the first segment.
   The league is returned."
  (let* ((path-segs (path-segments req))
         (league-name (first path-segs)))
    (if (not (empty? league-name))
        (get-league :name league-name))))
;;; Utils ------------------------------------------------------------------- END

;;; Routes
(setf *dispatch-table*
      (list (create-folder-dispatcher-and-handler "/deps/"
                                                  (merge-pathnames
                                                   "deps/"
                                                   static-files-dir))
            (create-folder-dispatcher-and-handler "/images/"
                                                  (merge-pathnames
                                                   "images/"
                                                   static-files-dir))
            (create-folder-dispatcher-and-handler "/scripts/"
                                                  (merge-pathnames
                                                   "scripts/"
                                                   static-files-dir))
            (create-folder-dispatcher-and-handler "/styles/"
                                                  (merge-pathnames
                                                   "styles/"
                                                   static-files-dir))
            (create-regex-dispatcher "^/$" 'www-home-page)
            (create-regex-dispatcher "^/about$"
                                     (lambda ()
                                       (base-league-page 'www-about-page
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/[a-zA-Z0-9-]+/about$"
                                     (lambda ()
                                       (base-league-page 'www-about-page)))
            (create-regex-dispatcher "^/leagues$"
                                     (lambda ()
                                       (base-league-page 'www-league-list-page
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/[a-zA-Z0-9-]+/games$"
                                     (lambda ()
                                       (base-league-page 'www-game-list-page)))
            (create-regex-dispatcher "^/[a-zA-Z0-9-]+/games/[0-9-]+$"
                                     (lambda ()
                                       (base-league-page 'www-game-detail-page)))
            (create-regex-dispatcher "^/[a-zA-Z0-9-]+/players$"
                                     (lambda ()
                                       (base-league-page 'www-player-list-page)))
            (create-regex-dispatcher "^/test-server-error$"
                                     (lambda ()
                                       (base-league-page
                                        'www-test-server-error
                                        :require-league? nil)))
            (create-regex-dispatcher "^/test-not-found$"
                                     (lambda ()
                                       (base-league-page
                                        'www-not-found-page
                                        :require-league? nil)))
            (create-regex-dispatcher "^/[a-zA-Z-]+$"
                                     (lambda ()
                                       (base-league-page
                                        'www-league-detail-page)))))
;;; Routes ------------------------------------------------------------------ END

;;; Template Page
(defmacro standard-page ((&key title page-id league player) &body body)
  "Creates a standard page layout.
   @param title
     Specifies the title of a page.
   @param page-id
     Specifies an id for the root element of the page. This is primarily
     intended to be used for CSS rules.
   @param body
     Contains the page body."
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:meta :http-equiv "X-UA-Compatible"
                    :content "IE=edge")
             (:meta :name "viewport"
                    :content "width=device-width, initial-scale=1")
             (:title (fmt "~A - Hockey Oracle" ,title))
             (:link :rel "shortcut icon"
                    :href "/images/favicon.ico")
             (:link
              :href "/deps/fira/fira.css"
              :rel "stylesheet"
              :type "text/css")
             (:link
              :href "/deps/font-awesome/css/font-awesome.min.css"
              :rel "stylesheet"
              :type "text/css")
             (:link
              :href "/styles/main.css"
              :rel "stylesheet"
              :type "text/css")
             (:script :src "/deps/jquery/jquery-2.1.3.min.js")
             (:script :src "/deps/lodash/lodash.min.js")
             (:script :src "/scripts/utils.js")
             (:script :src "/scripts/main.js"))
            (:body
             (:div :id "overlay" "&nbsp;")
             (:div :id "top-shade")
             (:header :id "top-heading"
                      (:div :id "league-name-header"
                            (if ,player
                                (htm
                                 (:a :href (sf "/players/~A"
                                               (player-id ,player))
                                     (esc (player-name ,player)))))
                            (if ,league
                                (htm
                                 (:span " - ")
                                 (:span (esc (league-name ,league))))))
                      (:a :href "/"
                          (:img
                           :alt "logo"
                           :class "logo"
                           :src "/images/banner.jpg")
                          (:span :class "title" "Hockey Oracle")))
             (let ((path (script-name* *request*)))
               (htm
                (:nav
                 (:ul :class "nav-items"
                      (:li
                       (:a :href "/" (:i :class "fa fa-bars")))
                      (:li
                       (:a :class (if (based-on-path? path "leagues")
                                      "active"
                                      nil)
                           :href "/leagues" "Leagues"))
                      (if ,league
                          (htm
                           (:li
                            (:a :class (if (based-on-path? path "games")
                                           "active"
                                           nil)
                                :href (sf "/~A/games"
                                          (string-downcase(league-name ,league)))
                                "Games"))
                           (:li
                            (:a :class (if (based-on-path? path "players")
                                           "active"
                                           nil)
                                :href (sf "/~A/players"
                                          (string-downcase (league-name
                                                            ,league)))
                                "Players"))
                           (:li
                            (:a :class (if (based-on-path? path "about")
                                           "active"
                                           nil)
                                :href (sf "/~A/about"
                                          (string-downcase (league-name
                                                            ,league)))
                                "About"))
                           ))
                      (if (null ,league)
                          (htm (:li (:a :class (if (based-on-path? path
                                                                   "about")
                                                   "active"
                                                   nil)
                                        :href "/about" "About"))))))))
             (:main :id ,page-id
                    ,@body)))))
;;; Template Page ----------------------------------------------------------- END

;;; Base Page
(defun base-league-page (actual-page &key (require-league? t))
  (let* ((league (parse-league *request*))
         (me-query (get-parameter "me"))
         (user-cookie (cookie-in "user"))
         (me-query-hash (if (not (empty? me-query)) (gen-hash me-query)))
         (me-query-hash2 (if (not (empty? me-query)) (gen-hash me-query-hash)))
         (user-cookie-hash (if (not (empty? user-cookie)) (gen-hash user-cookie)))
         (player (get-auth-player (or me-query-hash2 user-cookie-hash))))
    (log-message* :debug
                  "=== ME-QUERY ~A === ME-QUERY-HASH ~A === ME-QUERY-HASH2 ~A === COOKIE ~A === COOKIE-HASH ~A ===~%"
                  me-query
                  me-query-hash
                  me-query-hash2
                  user-cookie
                  user-cookie-hash)
    (if (and me-query player)
        (set-cookie "user" :value me-query-hash
                           ;; Expire a month from now
                           :max-age (* 60 60 24 30)
                           :path "/"
                           :secure (not *debug*)
                           :http-only t))
    ;; Load player from cookie incase an invalid 'me' query was given
    (if (and (null player) user-cookie)
        (setf player (get-auth-player user-cookie-hash)))
    (cond ((and require-league? (null league))
           (www-not-found-page :player player))
          ((and require-league? (null player))
           (www-not-authorised-page))
          (t (funcall actual-page :player player :league league)))))
;;; Base Page --------------------------------------------------------------- END

;;; Error Pages
(defun www-not-found-page (&key player league)
  (setf (return-code*) +http-not-found+)
  (standard-page
      (:title "Not Found"
       :player player
       :league league
       :page-id "not-found-page")
    (:h2 "Not Found")
    (:p "The page or resource you requested could not be found.")
    (:a :href "/" "Go back to the home page")))

(defun www-not-authorised-page (&key player league)
  (setf (return-code*) +http-forbidden+)
  (standard-page
      (:title "Not Authorised"
       :player player
       :league league
       :page-id "not-authorised-page")
    (:h2 "Not Authorised")
    (:p "Sorry but you do not have permission to view the page or resource you requested.")
    (:a :href "/" "Go back to the home page")))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (base-league-page #'www-not-found-page :require-league? nil))

(defun www-server-error-page (&key player league)
  (standard-page
   (:title "Server Error"
    :player player
    :league league
    :page-id "server-error-page")
   (:h2 "Server Error")
   (:p "Sorry, it looks like something unexpected happened on the server.")
   (:p "An administrator has been notified of the error.")
   (:a :href "/" "Go back to the home page")))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 500)) &key)
  (bt:make-thread (lambda () (send-error-email "A <b>server</b> error occurred.")))
  (www-server-error-page nil))

(defun www-test-server-error (&key player league)
  (log-message* :error "Test error page \(error log level).")
  (log-message* :warning "Test error page \(warning log level).")
  (log-message* :info "Test error page \(info log level).")
  (error "This is an intentional error for testing purposes.")
  ;; The following should never be displayed
  (standard-page
      (:title "Test Server Error"
       :player player
       :league league)
   (:h2 "Test Server Error")))
;;; Error Pages ------------------------------------------------------------- END

;;; Home Page
(defun www-home-page (&key player league)
  (redirect "/leagues"))
;;; Home Page --------------------------------------------------------------- END

;;; About Page
(defun www-about-page (&key player league)
  (standard-page
   (:title "About"
    :player player
    :league league
    :page-id "about-page")
   (:p
    "The Hockey Oracle is a simple app that generates teams by randomly "
    "selecting from a pool of active players.")
   (:p
    (:span "Please note this is an")
    (:b "alpha")
    (:span "version of the website with very limited functionality."))
   (:table :class "brief-table"
           (:tr
            (:td "Version")
            (:td (fmt "~a" version)))
           (:tr
            (:td "Last Updated")
            (:td (fmt "~a" (pretty-time updated))))
           (:tr
            (:td "License")
            (:td
             (:a :href "https://www.gnu.org/licenses/gpl-2.0.html" "GPL v2")))
           (:tr
            (:td "Copyright")
            (:td "2014-2015 Thirushanth Thirunavukarasu")))))
;;; About Page -------------------------------------------------------------- END

;;; League List Page
(defun www-league-list-page (&key player league)
  (standard-page
   (:title "Leagues"
    :player player
    :league league
    :page-id "league-list-page")
   (:h2 "Choose your league:")
   (:ul :class "simple-list"
        (dolist (league (get-all-leagues))
          (htm
           (:li
            (:a :class "button wide-button"
                :href (string-downcase (league-name league))
                (esc (league-name league)))))))))
;;; League List Page -------------------------------------------------------- END

;;; League Detail Page
(defun www-league-detail-page (&key player league)
  (redirect (sf "/~A/games~A"
                (string-downcase (league-name league))
                (if (query-string*)
                    (sf "?~A" (query-string*)) ""))))
;;; League Detail Page ------------------------------------------------------ END

;;; Game List Page
(defun www-game-list-page (&key player league)
  (let* ((started-games (get-games league :exclude-unstarted t))
         (unstarted-games (get-games league :exclude-started t)))
    (standard-page
        (:title "Games"
         :player player
         :league league
         :page-id "game-list-page")
      (if (and (empty? started-games) (empty? unstarted-games))
          (htm (:div "No games have been created for this league."))
          (htm
           (:h2 :class "blue-heading" "Schedule")
           (:ul :class "data-list"
                (dolist (game unstarted-games)
                  (htm
                   (:li
                    (:a :class "game-date"
                        :href (sf "/~A/games/~A"
                                  (string-downcase (league-name league))
                                  (game-id game))
                        (esc (pretty-time (game-time game))))
                    (:span :class "game-state" "")
                    (:span :class "clear-fix")))))
           (:h2 :class "blue-heading" "Scores")
           (:ul :class "data-list"
                (dolist (game (reverse started-games))
                  (htm
                   (:li
                    (:div :class "game-date"
                          (:a
                           :href (sf "/~A/games/~A"
                                     (string-downcase (league-name league))
                                     (game-id game))
                           (esc (pretty-time (game-time game)))))
                    (:div :class "game-score"
                          (:div
                           (:img :class "team-logo"
                                 :src (sf "/images/team-logos/~A"
                                          (team-logo (game-away-team game))))
                           (:span :class "team-name"
                                  (esc (sf "~A"
                                           (team-name (game-away-team game)))))
                           (:span :class "score"
                                  (esc (sf "~A" (game-away-score game)))))
                          (:div
                           (:img :class "team-logo"
                                 :src (sf "/images/team-logos/~A"
                                          (team-logo (game-home-team game))))
                           (:span :class "team-name"
                                  (esc (sf "~A"
                                           (team-name (game-home-team game)))))
                           (:span :class "score"
                                  (esc (sf "~A" (game-home-score game))))))
                    (:div :class "clear-fix"))))))))))
;;; Game List Page ---------------------------------------------------------- END

;;; Game Detail Page
(defun www-game-detail-page (&key player league)
  (let* ((game-id (last1 (path-segments *request*)))
         (game (get-game league game-id))
         (show-confirm-inputs
           (and game
                (string-equal "in-progress" (game-progress game)))))
    (if (null game)
        (www-not-found-page :player player :league league)
        (standard-page
            (:title (fmt "Game on ~a" (pretty-time (game-time game)))
             :player player
             :league league
             :page-id "game-detail-page")
          (:h1 :id "time-status"
               (:span (esc (pretty-time (game-time game))))
               (if (not (empty? (game-progress game)))
                   (htm
                    (:span " - ")
                    (:span :class "uppercase"
                           (esc (game-progress game))))))
          (:section
           :id "confirm-inputs"
           (:p "You answered YES/NO/MAYBE (TODO)"))
          (:div :id "edit-dialog" :class "dialog"
                (:header "Editing Player")
                (:section :class "content"
                          (:table
                           (:tr :class "input-row"
                                (:td :class "label-col"
                                     (:label :for "player-name-edit" "Name: "))
                                (:td :class "input-col"
                                     (:input :id "player-name-edit" :type "text")))
                           (:tr
                            (:td
                             (:label :for "player-pos-edit" "Position: "))
                            (:td
                             (:select :id "player-pos-edit"
                                      (dolist (pos players-positions)
                                        (htm
                                         (:option :value pos (esc pos)))))))
                           (:tr
                            (:td
                             (:label :for "player-active-edit" "Is Active: "))
                            (:td
                             (:input :id "player-active-edit" :type "checkbox"))))
                          (:div :class "actions"
                                (:button
                                 :class "button save-btn"
                                 :data-player-id "0"
                                 :onclick "savePlayer()"
                                 "Save")
                                (:button
                                 :class "button cancel-btn"
                                 :onclick "closeDialog()"
                                 "Cancel"))))
          (:section
           :id "confirmed-players-section"
           (:h2 :id "confirmed-heading"
                :class (if (confirmed-players game) "blue-heading" "grey-heading")
                (:span :class (if (confirmed-players game) "true" "true hidden")
                       "Confirmed to play")
                (:span :class (if (confirmed-players game) "false hidden" "false")
                       "No players confirmed to play"))
           (:ul :class "template-player-item"
                (:li :class "player-item"
                     (:span :class "player-name" "")
                     (:span :class "confirm-type" "&nbsp;")
                     (:span :class "confirm-btn-toggle"
                            (:button :class "button"
                                     :onclick "unconfirmPlayer(this)"
                                     :title "Move to \"Not playing...\" section"
                                     (:i :class "fa fa-thumbs-down")))
                     (:select :class "player-position"
                              :onchange "positionChanged(this)"
                              (dolist (pos players-positions)
                                (htm
                                 (:option :value pos
                                          :selected nil
                                          (esc pos)))))
                     (:span :class "confirm-info"
                            (:span :class "confirm-reason" "")
                            (:span :class "confirm-time" :title "Date confirmed" ""))
                     (:span :class "clear-fix")))
           (:ul :id "confirmed-players"
                :class (if (confirmed-players game)
                           "data-list"
                           "data-list hidden")
                (dolist (pc (confirmed-players game))
                  (htm
                   (:li :class "player-item"
                        :data-id (player-id (-> pc player))
                        :data-name (player-name (-> pc player))
                        :data-position (player-position (-> pc player))
                        :data-confirm-type (esc (sf "(~A)"
                                                    (game-confirm-confirm-type pc)))
                        :data-reason (esc (game-confirm-reason pc))
                        :data-response-time (pretty-time
                                             (game-confirm-time pc)
                                             'short)
                        (:span :class "player-name"
                               (esc (player-name (-> pc player))))
                        (:span :class "confirm-type" "&nbsp;")
                        (:span :class "confirm-btn-toggle"
                               (:button :class "button"
                                        :onclick "unconfirmPlayer(this)"
                                        :title "Move to \"Not playing...\" section"
                                        (:i :class "fa fa-thumbs-down")))
                        (:select :class "player-position"
                                 :onchange "positionChanged(this)"
                                 (dolist (pos players-positions)
                                   (htm
                                    (:option
                                     :value pos
                                     :selected (if (string-equal
                                                    pos
                                                    (player-position
                                                     (-> pc player)))
                                                   ""
                                                   nil)
                                     (esc pos)))))
                        (:span :class "confirm-info"
                               (:span :class "confirm-reason" "")
                               (:span :class "confirm-time"
                                      :title "Date confirmed"
                                      (esc (pretty-time
                                            (game-confirm-time pc)
                                            'short))))
                        (:span :class "clear-fix"))))))
          (:section :id "random-teams"
                    (:ul :class "template-player-item"
                         (:li :class "player-item"
                              (:span :class "player-name")
                              (:span :class "player-position")
                              (:span :class "clear-fix")))
                    (:div :id "team1" :class "team"
                          (:img :class "team-logo" :src "/images/team-logos/cripplers.png")
                          (:h2 :class "team-heading" "Cripplers")
                          (:ul :class "team-players data-list"))
                    (:div :id "team2" :class "team"
                          (:img :class "team-logo" :src "/images/team-logos/panthers.png")
                          (:h2 :class "team-heading" "Panthers")
                          (:ul :class "team-players data-list")))
          (:section
           :id "unconfirmed-players-section"
           (:h2 :id "unconfirmed-heading" :class "blue-heading"
                "Not playing or undecided")
           (:ul :class "template-player-item"
                (:li :class "player-item"
                     (:span :class "player-name" "")
                     (:span :class "confirm-type" "&nbsp;")
                     (:span :class "confirm-btn-toggle"
                            (:button :class "button"
                                     :onclick "confirmPlayer(this)"
                                     :title "Move to \"Confirmed...\" section"
                                     (:i :class "fa fa-thumbs-up")))
                     (:span :class "player-position" "&nbsp;")
                     (:span :class "confirm-info"
                            (:span :class "confirm-reason" "")
                            (:span :class "confirm-time" :title "Date confirmed" ""))
                     (:span :class "clear-fix")))
           (:ul :id "unconfirmed-players"
                :class (if (unconfirmed-players game)
                           "data-list"
                           "data-list hidden")
                (dolist (pc (unconfirmed-players game))
                  (htm
                   (:li :class "player-item"
                        :data-id (player-id (-> pc player))
                        :data-name (player-name (-> pc player))
                        :data-position (player-position (-> pc player))
                        :data-confirm-type (esc (sf "(~A)"
                                                    (game-confirm-confirm-type pc)))
                        :data-reason (esc (game-confirm-reason pc))
                        :data-response-time (pretty-time
                                             (game-confirm-time pc) 'short)
                        (:span :class "player-name" (esc (player-name (-> pc player))))
                        (:span :class "confirm-type"
                               (esc (sf "(~A)" (game-confirm-confirm-type pc))))
                        (:span :class "confirm-btn-toggle"
                               (:button :class "button"
                                        :onclick "confirmPlayer(this)"
                                        :title "Move to \"Confirmed...\" section"
                                        (:i :class "fa fa-thumbs-up")))
                        (:span :class "player-position" "&nbsp;")
                        (:span :class "confirm-reason"
                               (esc (game-confirm-reason pc)))
                        (:span :class "confirm-time"
                               :title "Date confirmed"
                               (esc (pretty-time
                                     (game-confirm-time pc)
                                     'short)))
                        (:span :class "clear-fix"))))))
          (:button :id "make-teams"
                   :class (if (confirmed-players game)
                              "button wide-button"
                              "button wide-button hidden")
                   :onclick "makeTeams()"
                   :title "Generate random teams"
                   (:i :class "fa fa-random")
                   (:span :class "button-text" "Make Teams"))
          (:button :id "add-player"
                   :class "button wide-button"
                   :onclick "addPlayer()"
                   (:i :class "fa fa-user-plus")
                   (:span :class "button-text" "Add Player"))
          (:button :id "pick-players"
                   :class "button wide-button"
                   :onclick "pickPlayers()"
                   :title "Choose players"
                   (:i :class "fa fa-check-circle-o")
                   (:span :class "button-text" "Pick Players"))))))
;;; Game Detail Page -------------------------------------------------------- END

;;; Player List Page
(defun www-player-list-page (&key player league)
  (standard-page
   (:title "Players"
    :player player
    :league league
    :page-id "player-list-page")
   (:div :id "edit-dialog" :class "dialog"
         (:header "Editing Player")
         (:section :class "content"
                   (:table
                    (:tr :class "input-row"
                         (:td :class "label-col"
                              (:label :for "player-name-edit" "Name: "))
                         (:td :class "input-col"
                              (:input :id "player-name-edit" :type "text")))
                    (:tr
                     (:td
                      (:label :for "player-pos-edit" "Position: "))
                     (:td
                      (:select :id "player-pos-edit"
                               (dolist (pos players-positions)
                                 (htm
                                  (:option :value pos (esc pos)))))))
                    (:tr
                     (:td
                      (:label :for "player-active-edit" "Is Active: "))
                     (:td
                      (:input :id "player-active-edit" :type "checkbox"))))
                   (:div :class "actions"
                         (:button
                          :class "button save-btn"
                          :data-player-id "0"
                          :onclick "savePlayer()"
                          "Save")
                         (:button
                          :class "button cancel-btn"
                          :onclick "closeDialog()"
                          "Cancel"))))
   (:table :id "player-list" :class "data-table"
           (:thead
            (:tr
             (:th :class "player-name" "Player")
             (:th :class "player-position" :title "Position" "Pos")
             (:th :class "actions-col" "")))
           (:tbody
            (dolist (p (get-players league))
              (htm
               (:tr
                :class (if (player-active? p)
                           "player-item selected"
                           "player-item")
                :data-player-id (player-id p)
                (:td
                 :class "player-name"
                 :onclick "togglePlayerActive(this)"
                 (:i
                  :class (if (player-active? p)
                             "player-check fa fa-check-circle-o"
                             "player-check fa fa-circle-o")
                  :title "When checked the player is considered active/able to play")
                 (:span :class "player-name"
                        (esc (player-name p))))
                (:td
                 (:select :class "player-position"
                          (dolist (pos players-positions)
                            (htm
                             (:option
                              :value pos
                              :selected (if (string-equal
                                             pos
                                             (player-position p))
                                            ""
                                            nil)
                              (esc pos))))))
                (:td :class "action-buttons"
                     (:button
                      :class "button"
                      :href "javascript:void(0)"
                      :onclick "editPlayer(this)"
                      (:i :class "fa fa-pencil-square-o"))))))))
   (:section :id "random-teams"
             (:table :id "team1" :class "team data-table"
                     (:thead
                      (:tr :class "team-heading"
                           (:th :class "team-name"
                                "Cripplers")
                           (:th
                            (:img :class "team-logo"
                                  :src "/images/team-logos/cripplers.png"))))
                     (:tbody :class "team-players"))
             (:table :id "team2" :class "team data-table"
                     (:thead
                      (:tr :class "team-heading"
                           (:th :class "team-name"
                                "Panthers")
                           (:th
                            (:img :class "team-logo"
                                  :src "/images/team-logos/panthers.png"))))
                     (:tbody :class "team-players")))
   (:button :id "make-teams"
            :class "button wide-button"
            :href "javascript:void(0)"
            :onclick "makeTeams()"
            :title "Select to generate random teams"
            (:i :class "fa fa-random")
            (:span :class "button-text" "Make Teams"))
   (:button :id "add-player"
            :class "button wide-button"
            :href "javascript:void(0)"
            :onclick "addPlayer()"
            (:i :class "fa fa-user-plus")
            (:span :class "button-text" "Add Player"))
   (:button :id "pick-players"
            :class "button wide-button"
            :href "javascript:void(0)"
            :onclick "pickPlayers()"
            :title "Select to choose active players"
            (:i :class "fa fa-check-circle-o")
            (:span :class "button-text" "Pick Players"))
    ))
;;; Player List Page -------------------------------------------------------- END
