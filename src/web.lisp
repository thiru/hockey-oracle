;;;; Web-specific functionality.

(in-package :hockey-oracle.web)

;;; General
(defvar *debug* t)
(defvar main-acceptor nil "The global web-server instance.")
(defvar static-files-dir (merge-pathnames "www/" (app-base-dir *app*)))

(defparameter server-info (get-server-info))

(defparameter reset-pwd-msg
  (concatenate 'string
               "<p>A request was made to reset your password. If you would like "
               "to continue please follow the link below. Please note, this "
               "request will expire within 24 hours.</p>"
               "<p><a href='~Areset-password?token=~A'>"
               "Reset my password</a></p>"))

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
;;; General ----------------------------------------------------------------- END

;;; Email
(defun email-game-reminder (game)
  "Sends an email reminder of an upcoming game."
  (check-type game GAME)
  (let* ((league (game-league game)))
    (send-email-to-players
     (sf "Upcoming game in ~A" (league-name league))
     (lambda (player)
       (let* ((player-confirm (game-confirm-for game player))
              (confirm-type (if player-confirm
                                (game-confirm-confirm-type player-confirm)
                                :no-response))
              (all-confirmed (confirmed-players game))
              (all-unconfirmed (unconfirmed-players game))
              (all-maybes (remove-if-not
                           (lambda (gc) (equal :maybe
                                               (game-confirm-confirm-type gc)))
                           all-unconfirmed))
              (all-cant-play (remove-if-not
                              (lambda (gc) (equal :cant-play
                                                  (game-confirm-confirm-type
                                                   gc)))
                              all-unconfirmed))
              (all-no-response (remove-if-not
                                (lambda (gc) (equal :no-response
                                                    (game-confirm-confirm-type
                                                     gc)))
                                all-unconfirmed)))
         (mkstr
          (sf '("<p>This is a reminder of an <a href='~(~A~)'>upcoming game</a> "
                "in the <a href='~(~A~)' title='~A'>~A</a> on ~A.</p>")
              (build-url (sf "~A/games/~A"
                             (league-name league)
                             (game-id game))
                         player)
              (build-url (league-name league) player)
              (league-full-name league)
              (league-name league)
              (pretty-time (game-time game)))
          (if (or (null player-confirm)
                  (equal :no-response confirm-type))
              (sf '("<p>Please update your <a href='~(~A~)'>game status"
                    "</a>.</p>")
                  (build-url (sf "~A/games/~A"
                                 (league-name league)
                                 (game-id game))
                             player))
              (sf '("<p>Your current status for this game is "
                    "<strong>~(~A~)</strong>.</p>")
                  (getf confirm-types confirm-type)))
          (if (non-empty? (game-notes game))
              (sf "<p><strong>~A</strong></p>"
                  (escape-string (game-notes game)))
              "")
          (if (empty? all-confirmed)
              "<p><u>No players have confirmed to play as yet.</u></p>"
              (sf '("<strong>Confirmed to play (~A):</strong>"
                    "<ul>~{<li>~A</li>~}</ul>")
                  (length all-confirmed)
                  (map 'list
                       (lambda (gc)
                         (sf "~A - <i>~A</i>"
                             (player-name (game-confirm-player gc))
                             (player-position (game-confirm-player gc))))
                       all-confirmed)))
          (if (non-empty? all-maybes)
              (sf '("<strong>Might play (~A):</strong>"
                    "<ul>~{<li>~A</li>~}</ul>")
                  (length all-maybes)
                  (map 'list
                       (lambda (gc)
                         (sf "~A - <i>~A</i>"
                             (player-name (game-confirm-player gc))
                             (player-position (game-confirm-player gc))))
                       all-maybes))
              "")
          (if (non-empty? all-cant-play)
              (sf '("<strong>Not playing (~A):</strong>"
                    "<ul>~{<li>~A</li>~}</ul>")
                  (length all-cant-play)
                  (map 'list
                       (lambda (gc)
                         (sf "~A - <i>~A</i>"
                             (player-name (game-confirm-player gc))
                             (player-position (game-confirm-player gc))))
                       all-cant-play))
              "")
          (if (non-empty? all-no-response)
              (sf '("<strong>No response (~A):</strong>"
                    "<ul>~{<li>~A</li>~}</ul>")
                  (length all-no-response)
                  (map 'list
                       (lambda (gc)
                         (sf "~A - <i>~A</i>"
                             (player-name (game-confirm-player gc))
                             (player-position (game-confirm-player gc))))
                       all-no-response))
              ""))))
     league)))
;;; Email ------------------------------------------------------------------- END

;;; Utils
(defun build-url (path &optional player)
  "Builds an absolute URL to this website at the specified path. The player is
   used to include a temporary authentication token in case they have not set a
   permanent password."
  (if player
      (check-type player PLAYER))
  (sf "~A/~(~A~)~(~A~)"
      (server-info-host server-info)
      path
      (if (and player (empty? (player-perm-auth player)))
          (sf "?me=~A-~A" (player-id player) (player-temp-auth player))
          "")))

;;; TODO: Following is not being used
(defmacro html-snippet (root-tag)
  "Generate HTML given a single root HTML tag."
  `(with-html-output-to-string (*standard-output* nil :indent t)
     ,root-tag))

(defun clean-uri-segment (input)
  "Replaces non-alphanumeric chars in INPUT for a cleaner URI segment."
  (setf input (cl-ppcre:regex-replace-all "[ ]+" input "-"))
  (cl-ppcre:regex-replace-all "[^A-Za-z0-9-]" input ""))

(defun based-on-path? (current-path test-path &optional league)
  "Determine whether CURRENT-PATH is rooted in TEST-PATH.
   If LEAGUE is given, the league's name is expected to be at the root of
   CURRENT-PATH.
   All paths are expected to be relative and only contain the 'file' path. I.e.
   no query parameters or hash segments."
  (if league
      (cl-ppcre:scan (sf "^/~(~A~)/~(~A~)" (league-name league) test-path)
                     current-path)
      (cl-ppcre:scan (sf "^/~(~A~)" test-path) current-path)))

(defun path-segments (req)
  "Gets a list of path segments, excluding query parameters."
  (split-sequence #\/ (script-name* req) :remove-empty-subseqs t))

(defun json-result (result &optional data)
  "Converts the given R instance to a JSON string."
  (json:encode-json-plist-to-string
   `(level ,(r-level result)
           message ,(r-message result)
           data ,data)))

(defun parse-league (req)
  "Parses the request path to obtain the league defined as the first segment.
   The league is returned."
  (let* ((path-segs (path-segments req))
         (league-name (first path-segs)))
    (if (not (empty? league-name))
        (get-league :name league-name))))

(defun set-auth-cookie (player &key perm?)
  "Set the temporary or permanent authorisation cookie."
  (if player
      (set-cookie (if perm? "puser" "tuser")
                  :value (sf "~A-~A"
                             (player-id player)
                             (if perm?
                                 (player-perm-auth player)
                                 (player-temp-auth player)))
                  ;; Expire a month from now
                  :max-age (* 60 60 24 30)
                  :path "/"
                  :secure (not *debug*)
                  :http-only t)))

(defun remove-cookie (id)
  "Removes the cookie with the specified ID."
  (set-cookie id
              :value ""
              :max-age 0
              :path "/"
              :secure (not *debug*)
              :http-only t))

(defun remove-auth-cookies ()
  "Invalidates all authorisation cookies."
  (remove-cookie "puser")
  (remove-cookie "tuser"))
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
            (create-regex-dispatcher "^/about/?$"
                                     (lambda ()
                                       (base-league-page 'www-about-page
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/[\\w-]+/about/?$"
                                     (lambda ()
                                       (base-league-page 'www-about-page)))
            (create-regex-dispatcher "^/logout/?$"
                                     (lambda ()
                                       (base-league-page 'www-user-logout-page
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/api/login/?$"
                                     (lambda ()
                                       (base-league-page 'api-login
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/api/forgot-password/?$"
                                     (lambda ()
                                       (base-league-page 'api-forgot-pwd
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/reset-password/?$"
                                     (lambda ()
                                       (base-league-page 'www-reset-pwd
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/api/reset-password/?$"
                                     (lambda ()
                                       (base-league-page 'api-reset-pwd
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/users/me/?$"
                                     (lambda ()
                                       (base-league-page 'www-user-detail-page
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/[\\w-]+/users/me/?$"
                                     (lambda ()
                                       (base-league-page 'www-user-detail-page)))
            (create-regex-dispatcher "^/[\\w-]+/users/[\\w-]+/[0-9]+/?$"
                                     (lambda ()
                                       (base-league-page 'www-user-detail-page)))
            (create-regex-dispatcher "^/api/users/[0-9]+/?$"
                                     (lambda ()
                                       (base-league-page 'api-user-save
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/leagues/?$"
                                     (lambda ()
                                       (base-league-page 'www-league-list-page
                                                         :require-league? nil)))
            (create-regex-dispatcher "^/[\\w-]+/games/schedule/?$"
                                     (lambda ()
                                       (base-league-page 'www-schedule-page)))
            (create-regex-dispatcher "^/[\\w-]+/games/scores/?$"
                                     (lambda ()
                                       (base-league-page 'www-scores-page)))
            (create-regex-dispatcher "^/[\\w-]+/games/[0-9-]+/?$"
                                     (lambda ()
                                       (base-league-page 'www-game-detail-page)))
            (create-regex-dispatcher "^/[\\w-]+/api/games/new/?$"
                                     (lambda ()
                                       (base-league-page 'api-new-game)))
            (create-regex-dispatcher "^/[\\w-]+/api/games/[0-9-]+/?$"
                                     (lambda ()
                                       (base-league-page 'api-game-update)))
            (create-regex-dispatcher "^/[\\w-]+/players/?$"
                                     (lambda ()
                                       (base-league-page 'www-player-list-page)))
            (create-regex-dispatcher "^/[\\w-]+/players/[\\w-]+/\[\\w-]+/?$"
                                     (lambda ()
                                       (base-league-page 'www-player-detail-page)))
            (create-regex-dispatcher "^/[\\w-]+/manage/?$"
                                     (lambda ()
                                       (base-league-page 'www-manage-league-page)))
            (create-regex-dispatcher "^/[\\w-]+/api/leagues/save/?$"
                                     (lambda ()
                                       (base-league-page 'api-league-save)))
            (create-regex-dispatcher "^/test-server-error/?$"
                                     (lambda ()
                                       (base-league-page
                                        'www-test-server-error
                                        :require-league? nil)))
            (create-regex-dispatcher "^/test-not-found/?$"
                                     (lambda ()
                                       (base-league-page
                                        'www-not-found-page
                                        :require-league? nil)))
            (create-regex-dispatcher "^/[\\w-]+/?$"
                                     (lambda ()
                                       (base-league-page
                                        'www-league-detail-page)))))
;;; Routes ------------------------------------------------------------------ END

;;; Base Page
(defun base-league-page (actual-page &key (require-league? t))
  (let* ((league (parse-league *request*))
         (me-query (get-parameter "me"))
         (perm-user-cookie (cookie-in "puser"))
         (temp-user-cookie (cookie-in "tuser"))
         (player-id 0)
         (given-auth "")
         (player nil))
    (when (not (empty? me-query))
      (setf player-id
            (loose-parse-int (subseq me-query 0 (position #\- me-query))))
      (setf given-auth (subseq me-query (1+ (or (position #\- me-query) 0))))
      (setf player (get-player :id player-id :temp-auth given-auth))
      (set-auth-cookie player))
    ;; Try to load player from long-lived cookie if player not yet found
    (when (and (null player) perm-user-cookie)
      (setf player-id
            (loose-parse-int (subseq perm-user-cookie
                                     0
                                     (position #\- perm-user-cookie))))
      (setf given-auth
            (subseq perm-user-cookie (1+ (or (position #\- perm-user-cookie)
                                             0))))
      (setf player (get-player :id player-id :perm-auth given-auth)))
    ;; Try to load player from short-lived cookie if player not yet found
    (when (and (null player) temp-user-cookie)
      (setf player-id
            (loose-parse-int (subseq temp-user-cookie
                                     0
                                     (position #\- temp-user-cookie))))
      (setf given-auth
            (subseq temp-user-cookie (1+ (or (position #\- temp-user-cookie)
                                             0))))
      (setf player (get-player :id player-id :temp-auth given-auth)))
    (cond ((and require-league? (null league))
           (www-not-found-page :player player))
          ((and require-league? (or (null player)
                                    (null (get-player :id player-id
                                                      :league league))))
           (www-not-authorised-page
            :player player
            :message "Sorry, you must be a member of this league to access it."))
          (t (funcall actual-page :player player :league league)))))
;;; Base Page --------------------------------------------------------------- END

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
            :id "root"
            :data-user (if ,player (player-id player))
            :data-league (if ,league (league-name league))
            (:head
             (:meta :charset "utf-8")
             (:meta :http-equiv "X-UA-Compatible"
                    :content "IE=edge")
             (:meta :name "viewport"
                    :content "width=device-width, initial-scale=1")
             (:meta :name "theme-color" :content "#0F83BC")
             (:title (fmt "~A - Hockey Oracle" ,title))
             (:link :rel "manifest" :href "/manifest.json")
             (:link :rel "shortcut icon"
                    :href "/images/favicon.ico")
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
             (:script :src "/deps/momentjs/moment.min.js")
             (:script :src "/deps/rxjs/rx.all.min.js")
             (:script :src "/scripts/utils.js")
             (:script :src "/scripts/main.js"))
            (:body
             (:div :id "overlay" "&nbsp;")
             (:div :id "top-shade")
             (:header :id "top-heading"
                      (:div :id "top-right-heading"
                            (if ,league
                                (htm
                                 (:a :href (sf "/~(~A~)" (league-name ,league))
                                     :title (esc (league-full-name ,league))
                                     (esc (league-name ,league)))
                                 (:span " - ")))
                            (if ,player
                                (htm
                                 (:a :href (if ,league
                                               (sf "/~(~A~)/users/me"
                                                   (league-name ,league))
                                               "/users/me")
                                     (esc (player-name ,player)))
                                 (:a :href "/logout"
                                     :title "Log out"
                                     (:i :class "fa fa-sign-out")))
                                (htm
                                 (:a :href "javascript:void(0)"
                                     :onclick "page.showLogin()"
                                     "Log in"))))
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
                       (:button :id "main-menu"
                                :class "clear-button"
                                :onclick "page.toggleMainMenu()"
                                (:i :class "fa fa-bars")))
                      (if ,league
                          (htm
                           (:li
                            (:a :class (if (based-on-path? path
                                                           "games/schedule"
                                                           ,league)
                                           "active"
                                           nil)
                                :href (sf "/~(~A~)/games/schedule"
                                          (league-name ,league))
                                "Schedule"))
                           (:li
                            (:a :class (if (based-on-path? path
                                                           "games/scores"
                                                           ,league)
                                           "active"
                                           nil)
                                :href (sf "/~(~A~)/games/scores"
                                          (league-name ,league))
                                "Scores"))
                           (:li
                            (:a :class (if (based-on-path? path
                                                           "players"
                                                           ,league)
                                           "active"
                                           nil)
                                :href (sf "/~(~A~)/players"
                                          (league-name ,league))
                                "Players")))))
                 (:br)
                 (:div :id "ham-menu-group"
                       :class "hidden"
                       (:ul :class "simple-list"
                            (:li
                             (:a :class (if (based-on-path? path "leagues")
                                            "active"
                                            nil)
                                 :href "/leagues"
                                 (:i :class "fa fa-fw fa-users")
                                 (:span "Leagues")))
                            (if (and ,league
                                     ,player
                                     (is-commissioner? ,player ,league))
                                (htm
                                 (:li
                                  (:a :href (sf "/~(~A~)/manage"
                                                (league-name ,league))
                                      (:i :class "fa fa-fw fa-pencil-square")
                                      (:span "Manage")))))
                            (if (null ,league)
                                (htm (:li (:a :class (if (based-on-path? path
                                                                         "about")
                                                         "big-screen active"
                                                         "big-screen")
                                              :href "/about"
                                              (:i :class "fa fa-fw fa-info-circle")
                                              (:span "About"))))
                                (htm (:li
                                      (:a :class (if (based-on-path? path
                                                                     "about"
                                                                     ,league)
                                                     "active"
                                                     nil)
                                          :href (sf "/~A/about"
                                                    (league-name ,league))
                                          (:i :class "fa fa-fw fa-info-circle")
                                          (:span "About")))))
                            (:li
                             (:a :onclick "page.toggleMainMenu()"
                                 :href "javascript:void(0)"
                                 (:i :class "fa fa-fw fa-times-circle")
                                 (:span "Close"))))))))
             (:section :id "login-dialog"
                       :class "dialog"
                       (:div :class "dialog-content"
                             (:h2 "Welcome!")
                             (:p
                              (:input :id "login-email-address"
                                      :class "full-width"
                                      :onkeyup "onEnter(event, page.login)"
                                      :placeholder "Email Address"
                                      :title "Email Address"
                                      :type "text"))
                             (:p
                              (:input :id "login-pwd"
                                      :class "full-width"
                                      :onkeyup "onEnter(event, page.login)"
                                      :placeholder "Password"
                                      :title "Password"
                                      :type "password"))
                             (:p :id "login-result")
                             (:p
                              (:button :id "login-btn"
                                       :class "button wide-button"
                                       :onclick "page.login()"
                                       "Log In"))
                             (:p
                              (:a :id "forgot-pwd"
                                  :href "javascript:void(0)"
                                  :onclick "page.forgotPwd()"
                                  :style "float:left"
                                  "Forgot password")
                              (:a :href "javascript:void(0)"
                                  :onclick "page.closeLogin()"
                                  :style "float:right"
                                  "Close"))))
             (:main :id ,page-id
                    ,@body)))))
;;; Template Page ----------------------------------------------------------- END

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

(defun www-not-authorised-page (&key player league message)
  (setf (return-code*) +http-forbidden+)
  (standard-page
      (:title "Not Authorised"
       :player player
       :league league
       :page-id "not-authorised-page")
    (:h2 "Not Authorised")
    (:p
     (if (non-empty? message)
         (esc message)
         (esc (sf '("Sorry, you do not have permission to view the page or "
                    "resource you requested.")))))
    (:a :href "/" "Go back to the home page")
    (if (null player)
        (htm
         (:p "Or")
         (:button :class "button wide-button"
                  :onclick "page.showLogin()"
                  "Log in")))))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (base-league-page #'www-not-found-page :require-league? nil))

(defun www-server-error-page (&key player league)
  (setf (return-code*) +http-internal-server-error+)
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
  (send-email "Server Error" "A <b>server error</b> occurred.")
  (base-league-page #'www-server-error-page :require-league? nil))

(defun www-test-server-error (&key player league)
  (log-message* :error "Test error page \(error log level).")
  (log-message* :warning "Test error page \(warning log level).")
  (log-message* :info "Test error page \(info log level).")
  ;; The following page should not be displayed. Instead we should be redirected
  ;; to the standard server error page.
  (standard-page
      (:title "Test Server Error"
       :player player
       :league league)
    (:h2 "Test Server Error"))
  (error "This is an intentional error for testing purposes."))
;;; Error Pages ------------------------------------------------------------- END

;;; Home Page
(defun www-home-page (&key player league)
  (declare (ignorable player league))
  (redirect "/leagues"))
;;; Home Page --------------------------------------------------------------- END

;;; About Page
(defun www-about-page (&key player league)
  (standard-page
      (:title "About"
       :player player
       :league league
       :page-id "about-page")
    (:p "The Hockey Oracle is a simple app intended to ease management of "
        "amateur hockey leagues.")
    (:p "Current features:")
    (:ul
     (:li "Generate random teams from a pool of" (:em "active") "players")
     (:li "Manage a schedule of games including")
     (:ul
      (:li "Sending out email reminders of upcoming games")
      (:li "Keeping track of player status per game, e.g.:")
      (:ul
       (:li "confirmed to play")
       (:li "unable to play")
       (:li "undecided"))))
    (:p
     (:span "Please note that this is an")
     (:strong "alpha")
     (:span "version of the website with limited functionality."))
    (:table :class "brief-table"
            (:tr
             (:td "Version")
             (:td (fmt "~a" (app-version *app*))))
            (:tr
             (:td "Last Updated")
             (:td (fmt "~a" (pretty-time (app-updated *app*)))))
            (:tr
             (:td "Source Code")
             (:td (:a :href "https://github.com/thiru/hockey-oracle"
                      "Hosted at Github")))
            (:tr
             (:td "License")
             (:td
              (:a :href "https://www.gnu.org/licenses/gpl-2.0.html" "GPL v2")))
            (:tr
             (:td "Copyright")
             (:td "2014-2016 Thirushanth Thirunavukarasu")))))
;;; About Page -------------------------------------------------------------- END

;;; Login API
(defun api-login (&key player league)
  (declare (ignorable player league))
  (setf (content-type*) "application/json")
  (let* ((email (post-parameter "email"))
         (pwd (post-parameter "pwd"))
         (player nil))
    ;; Verify password provided by user is correct
    (setf player (get-player :email email :pwd pwd))
    (when (null player) ; Password incorrect
      (sleep 2) ; Deliberate sleep to help against DOS attack
      (return-from api-login
        (json:encode-json-plist-to-string
         `(level :error message "Incorrect login..."))))
    (set-auth-cookie player :perm? t)
    (json:encode-json-plist-to-string
     `(level :success message "Login successful!"))))
;;; Login API --------------------------------------------------------------- END

;;; Reset Password Page
(defun www-reset-pwd (&key player league)
  (declare (ignorable player))
  (let* ((token (get-parameter "token"))
         (player-id (loose-parse-int (subseq token 0 (position #\- token))))
         (player (get-player :id player-id))
         (verified-token (if player (reset-pwd-get-token player))))
    (if (not (and player
                  (not (empty? verified-token))
                  (string-equal token verified-token)))
        (return-from www-reset-pwd
          (www-not-found-page :player player :league league)))
    (standard-page
        (:title "Reset Password"
         :player player
         :league league
         :page-id "reset-password-page")
      (:h2 "Please enter your new password")
      (:p
       (:input :id "pwd-new"
               :class "full-width"
               :onkeyup "onEnter(event, page.resetPwd)"
               :placeholder "New password"
               :title "New password"
               :type "password"))
      (:p
       (:input :id "pwd-new-repeat"
               :class "full-width"
               :onkeyup "onEnter(event, page.resetPwd)"
               :placeholder "Repeat new password"
               :title "Repeat new password"
               :type "password"))
      (:p :id "save-result")
      (:p
       (:button :id "save-btn"
                :class "button wide-button"
                :data-player-id player-id
                :data-reset-token (escape-string verified-token)
                :onclick "page.resetPwd()"
                "Save")))))
;;; Reset Password Page ----------------------------------------------------- END

;;; Reset Password API
(defun api-reset-pwd (&key player league)
  (declare (ignorable player league))
  (sleep 2)
  (setf (content-type*) "application/json")
  (let* ((player-id (loose-parse-int (post-parameter "id")))
         (reset-token (post-parameter "resetToken"))
         (new-pwd (post-parameter "pwd"))
         (player (get-player :id player-id))
         (verified-token (if player (reset-pwd-get-token player)))
         (save-res nil))
    ;; Player not found:
    (if (null player)
        (return-from api-reset-pwd
          (json:encode-json-plist-to-string
           `(level :error message "Account not found."))))
    ;; Verify reset password token is still valid
    (if (or (empty? verified-token)
            (not (string-equal reset-token verified-token)))
        (return-from api-reset-pwd
          (json:encode-json-plist-to-string
           `(level :error message "Password reset period expired."))))
    (setf save-res (change-player-pwd player new-pwd))
    ;; Password update failed:
    (if (failed? save-res)
        (return-from api-reset-pwd
          (json:encode-json-plist-to-string
           `(level :error message "Failed to reset password."))))
    (setf player (r-data save-res))
    (set-auth-cookie player :perm? t)
    (json:encode-json-plist-to-string
     `(level :success
             message "Password succesfully updated!"))))
;;; Reset Password API ------------------------------------------------------ END

;;; Forgot Password API
(defun api-forgot-pwd (&key player league)
  (declare (ignorable player league))
  (sleep 2)
  (setf (content-type*) "application/json")
  (let* ((email (post-parameter "email"))
         (player (get-player :email email))
         (reset-token ""))
    ;; User email not found:
    (if (null player)
        (return-from api-forgot-pwd
          (json:encode-json-plist-to-string
           `(level :error message "Account not found."))))
    (setf reset-token (reset-pwd-set-token player))
    ;; Reset attempt failed:
    (if (empty? reset-token)
        (return-from api-forgot-pwd
          (json:encode-json-plist-to-string
           `(level :error message "Failed to reset password."))))
    ;; Send email to reset password
    (send-email "Reset Password"
                (sf reset-pwd-msg (build-url "") reset-token)
                (player-email player))
    ;; Report success
    (json:encode-json-plist-to-string
     `(level :success
             message "A link to reset your password was emailed to you."))))
;;; Forgot Password API ----------------------------------------------------- END

;;; User Detail Page
(defun www-user-detail-page (&key player league)
  (let* ((path-segs (path-segments *request*))
         (target-player-id (loose-parse-int (last1 path-segs)))
         (target-player (if (plusp target-player-id)
                            (get-player :id target-player-id))))
    ;; Abort if player id specified in URL but not found
    (when (and (plusp target-player-id) (null target-player))
      (return-from www-user-detail-page
        (www-not-found-page :player player :league league)))
    ;; Abort if implicit player not provided
    (if (null player)
        (return-from www-user-detail-page
          (www-not-found-page :player player :league league)))
    ;; Abort if attempting to view a different player is not an admin
    (if (and target-player
             (/= target-player-id (player-id player))
             (not (player-admin? player)))
        (return-from www-user-detail-page
          (www-not-authorised-page :player player :league league)))
    (if (null target-player)
        (setf target-player player))
    (let ((leagues (get-all-leagues))
          (commissions '()))
      (dolist (l leagues)
        (if (find (player-id target-player)
                  (league-commissioner-ids l))
            (push l commissions)))
      (standard-page
          (:title "User"
           :player player
           :league league
           :page-id "user-detail-page")
        (:section :id "left-col" :class "col"
                  (:p
                   (:img :id "user-img"
                         :class "full-width"
                         :src "/images/user.png")))
        (:section :id "right-col" :class "col"
                  (if (eq player target-player)
                      (htm
                       (:p
                        (:a :class "button"
                            :href "/logout"
                            :style "float:right" "Log out")
                        (:div :class "clear-fix"))
                       ))
                  (:p
                   (:input :id "player-name-edit"
                           :class "full-width"
                           :data-orig-val (escape-string (player-name target-player))
                           :data-player-id (player-id target-player)
                           :placeholder "Name"
                           :title "Name"
                           :type "text"
                           :value (escape-string (player-name target-player))))
                  (:p
                   (:input :id "player-email-edit"
                           :class "full-width"
                           :data-orig-val (escape-string (player-email target-player))
                           :placeholder "Email address"
                           :title "Email address"
                           :type "email"
                           :value (escape-string (player-email target-player))))
                  (:section :id "change-pwd"
                            (:p
                             (:a :id "change-pwd-btn"
                                 :href "javascript:void(0)"
                                 :onclick "page.changePwd()"
                                 "Change Password"))
                            (:div :id "pwd-group"
                                  :style "display:none"
                                  (if (non-empty? (player-perm-auth target-player))
                                      (htm
                                       (:p
                                        (:input :id "pwd-curr"
                                                :class "full-width"
                                                :type "password"
                                                :placeholder "Current password"
                                                :title "Current password"))))
                                  (:p
                                   (:input :id "pwd-new"
                                           :class "full-width"
                                           :type "password"
                                           :placeholder "New password"
                                           :title "New password"))
                                  (:p
                                   (:input :id "pwd-new-repeat"
                                           :class "full-width"
                                           :type "password"
                                           :placeholder "Repeat new password"
                                           :title "Repeat new password"))))
                  (if (player-admin? target-player)
                      (htm
                       (:p :id "admin"
                           :title "You have site-wide adminstrator privileges"
                           (:i :class "fa fa-star")
                           (:span "Administrator"))))
                  (if commissions
                      (htm
                       (:p :id "commissioner"
                           :title "You are a commissioner of these leagues"
                           (:i :class "fa fa-star")
                           (:span "Commissioner: ")
                           (dolist (l commissions)
                             (htm
                              (:a :href (sf "/~(~A~)" (league-name l))
                                  (esc (league-name l)))
                              (:span :class "comma" ","))))))
                  (:p
                   (:label
                    :title (sf '("Uncheck to deactive yourself from this league."
                                 "When inactive you will not be part of the "
                                 "regular lineup, and you will not receive "
                                 "email reminders of upcoming games."))
                    (:input :id "player-active-edit"
                            :checked (player-active-in? target-player league)
                            :data-orig-val
                            (if (player-active-in? target-player league)
                                "true"
                                "false")
                            :type "checkbox")
                    (:span "Active")))
                  (:p
                   (:label
                    :title (sf '("Notify me immediately when the state of the "
                                 "upcoming game changes. E.g. when a player "
                                 "changes their status."))
                    (:input :id "player-immediate-notify-edit"
                            :checked (player-notify-immediately? target-player)
                            :data-orig-val
                            (if (player-notify-immediately? target-player)
                                "true"
                                "false")
                            :type "checkbox")
                    (:span "Immediate email notifications")))
                  (:p
                   (:label
                    (:span "Default Position: ")
                    (:select :id "player-pos-edit"
                             :data-orig-val
                             (escape-string (player-position target-player))
                             (dolist (pos players-positions)
                               (htm
                                (:option :selected
                                         (string-equal pos
                                                       (player-position
                                                        target-player))
                                         :value pos (esc pos)))))))
                  (:p
                   (:button :id "save-btn"
                            :class "button wide-button"
                            :onclick "page.saveUser()"
                            :style "display:none"
                            "Save"))
                  (:p :id "save-result"))))))
;;; User Detail Page -------------------------------------------------------- END

;;; User Save API
(defun api-user-save (&key player league)
  (declare (ignorable league))
  (setf (content-type*) "application/json")
  (let* ((league (get-league :name (post-parameter "leagueName")))
         (curr-player-id (player-id player))
         (id (loose-parse-int (post-parameter "id")))
         (target-player (get-player :id id))
         (name (post-parameter "name"))
         (email (post-parameter "email"))
         (active? (string-equal "true" (post-parameter "active")))
         (notify-immediately?
           (string-equal "true" (post-parameter "notifyImmediately")))
         (pos (post-parameter "position"))
         (curr-pwd (post-parameter "currentPwd"))
         (new-pwd (post-parameter "newPwd"))
         (save-res nil))
    ;; Verify target player exists
    (when (null target-player)
      (setf (return-code*) +http-not-found+)
      (return-from api-user-save
        (json-result (new-r :error
                            "This player no longer exists."))))
    ;; Verify target player is same as current player or is admin
    (when (not (or (= id (player-id player)) (player-admin? player)))
      (setf (return-code*) +http-forbidden+)
      (return-from api-user-save
        (json-result (new-r :error
                            "You do not have permission to make this change."))))
    ;; Update simple player info
    (setf (player-name target-player) name)
    (setf (player-email target-player) email)
    (setf (player-notify-immediately? target-player) notify-immediately?)
    (setf (player-position target-player) pos)
    ;; Save simple player info
    (setf save-res (update-player target-player))
    ;; Abort if basic player update failed
    (when (failed? save-res)
      (setf (return-code*) +http-internal-server-error+)
      (return-from api-user-save (json-result save-res)))
    ;; If user has changed their active status for this league..
    (log-message* :ERROR "=== ACTIVE = ~A; PAI = ~A ===" active? (player-active-in? target-player league))
    (when (not (eq active? (player-active-in? target-player league)))
      (log-message* :WARNING "====== INSIDE ======")
      (update-player-active target-player league active?))
    ;; If user is attempting to change their password..
    (when (non-empty? new-pwd)
      ;; Abort if current password provided by user is incorrect
      (when (and (non-empty? (player-perm-auth target-player))
                 (null (get-player :id (player-id target-player) :pwd curr-pwd)))
        (setf (return-code*) +http-bad-request+)
        (return-from api-user-save
          (json-result (new-r :error "Current password is incorrect."))))
      ;; Save new password
      (setf save-res (change-player-pwd target-player new-pwd))
      (when (failed? save-res) ; Save new password failed
        (setf (return-code*) +http-internal-server-error+)
        (return-from api-user-save (json-result save-res)))
      ;; If the current user is changing their own password update their
      ;; authorisation cookie
      (if (= curr-player-id (player-id player))
          (set-auth-cookie (r-data save-res) :perm? t)))
    (json-result (new-r :success "Update successful!"))))
;;; User Save API ----------------------------------------------------------- END

;;; User Logout Page
(defun www-user-logout-page (&key player league)
  (declare (ignorable player))
  (remove-auth-cookies)
  (standard-page
      (:title "Log Out"
       :player nil
       :league league
       :page-id "user-logout-page")
    (:h2 "Thank you, come again!")
    (:p
     (:a :class "wide-button"
         :href "/"
         "Go back to Home page"))))
;;; User Logout Page -------------------------------------------------------- END

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
                :href (sf "/~(~A~)" (league-name league))
                :title (esc (league-name league))
                (esc (league-full-name league)))))))))
;;; League List Page -------------------------------------------------------- END

;;; League Detail Page
;;; NOTE: Disabling this page for now as it doesn't add much value over the
;;;       schedule/score pages.
(defun www-league-detail-page (&key player league)
  (declare (ignorable player))
  (redirect (sf "/~(~A~)/games/schedule" (league-name league))))
#||
  (let* ((upcoming-games (get-upcoming-games league 3)))
    (standard-page
        (:title (league-name league)
         :player player
         :league league
         :page-id "league-detail-page")
      (:h1 (fmt "Welcome to the ~A!" (league-name league)))
      (:h2 :class "underlined-heading" "Upcoming Games")
      (if (empty? upcoming-games)
          (htm
           (:p "No upcoming games scheduled"))
          (dolist (game upcoming-games)
            (htm
             (:p :class "upcoming-game-item"
              (:a :class "upcoming-game-time"
                  :href (sf "/~(~A~)/games/~(~A~)"
                            (league-name league)
                            (game-id game))
                  (esc (pretty-time (game-time game))))
              (:span :class "upcoming-game-rel-time" "")))))
      (:h2 :class "underlined-heading" "All Games")
      (:p
       (:a :href (sf "/~(~A~)/games#schedule" (league-name league))
           "Schedule"))
      (:p
       (:a :href (sf "/~(~A~)/games#scores" (league-name league))
           "Scores")))))
||#
;;; League Detail Page ------------------------------------------------------ END

;;; Schedule Page
(defun www-schedule-page (&key player league)
  (let* ((games (get-games :league league :exclude-started t)))
    (standard-page
        (:title "Schedule"
         :player player
         :league league
         :page-id "schedule-page")
      ;; Template Items
      (:div :class "template-items"
            (:ul
             (:li :id "template-game-item"
                  (:a :class "game-time"
                      :href (sf "/~(~A~)/games/<GAME-ID>" (league-name league)))
                  (:span :class "game-state" "")
                  (:span :class "clear-fix"))))
      (if (is-commissioner? player league)
          (htm
           ;; Edit Game Dialog
           (:section :id "new-game-dialog"
                     :class "dialog"
                     (:div :class "dialog-content"
                           (:h2 "New Game")
                           (:p
                            (:label :for "date-picker" "Date:")
                            (:input :id "date-picker"
                                    :class "full-width"
                                    :onchange "page.updateRelTime()"
                                    :onkeyup "page.gameTimeKeyUp(event)"
                                    :placeholder "YYYY-MM-DD"
                                    :type "date"))
                           (:p
                            (:label :for "time-picker" "Time:")
                            (:input :id "time-picker"
                                    :class "full-width"
                                    :onchange "page.updateRelTime()"
                                    :onkeyup "page.gameTimeKeyUp(event)"
                                    :placeholder "HH:MM AM/PM"
                                    :type "text"))
                           (:p :class "actions"
                               (:button :id "save-game-btn"
                                        :class "button"
                                        :onclick "page.saveGame()"
                                        "Save")
                               (:button :class "button"
                                        :onclick "page.closeGameEditor()"
                                        "Close"))
                           (:p :id "save-result")))
           ;; New Game Button
           (:section :id "edit-controls"
                     (:button :class "button"
                              :onclick "page.openGameEditor()"
                              "New Game"))))
      ;; New Games Section
      (:section :id "new-games-section" :style "display:none"
                (:h2 :class "blue-heading" "New Games")
                (:ul :id "new-games-list" :class "data-list"))
      (if (empty? games)
          ;; No Games Notice
          (htm
           (:h2 :id "no-games" "No games scheduled."))
          (htm
           ;; List of Games
           (:h2 :id "schedule" :class "blue-heading" "Schedule")
           (:ul :id "schedule-list"
                :class "data-list"
                (dolist (game games)
                  (htm
                   (:li :class "game-item"
                        :title (if (string-equal "cancelled"
                                                 (game-progress game))
                                   "Game cancelled")
                        (:span :class
                               (if (string-equal "cancelled"
                                                 (game-progress game))
                                   "cancelled"
                                   nil)
                               (:a :class "game-time"
                                   :href (sf "/~(~A~)/games/~(~A~)"
                                             (league-name league)
                                             (game-id game))
                                   (esc (pretty-time (game-time game))))
                               (:span :class "game-rel-time")
                               (:span :class "clear-fix")))))))))))
;;; Schedule Page ----------------------------------------------------------- END

;;; Scores Page
(defun www-scores-page (&key player league)
  (let* ((games (reverse (get-games :league league :exclude-unstarted t))))
    (standard-page
        (:title "Scores"
         :player player
         :league league
         :page-id "scores-page")
      (if (empty? games)
          ;; No Games Notice
          (htm (:h2 :id "no-games" "No scores available yet."))
          (htm
           ;; List of Games
           (:h2 :id "scores" :class "blue-heading" "Scores")
           (:ul :class "data-list"
                (dolist (game games)
                  (htm
                   (:li
                    :title
                    (if (string-equal "cancelled" (game-progress game))
                        "Game cancelled")
                    (:div :class
                          (if (string-equal "cancelled" (game-progress game))
                              "game-time cancelled"
                              "game-time")
                          (:a
                           :href (sf "/~(~A~)/games/~(~A~)"
                                     (league-name league)
                                     (game-id game))
                           (esc (pretty-time (game-time game)))))
                    (:div :class "game-score"
                          (:div
                           (:img :class "team-logo"
                                 :src (sf "/images/leagues/~(~A~)/teams/~(~A~)"
                                          (league-name league)
                                          (team-logo (game-away-team game))))
                           (:span :class "team-name"
                                  (esc (sf "~A"
                                           (team-name (game-away-team game)))))
                           (:span :class "score"
                                  (esc (sf "~A" (game-away-score game)))))
                          (:div
                           (:img :class "team-logo"
                                 :src (sf "/images/leagues/~(~A~)/teams/~(~A~)"
                                          (league-name league)
                                          (team-logo (game-home-team game))))
                           (:span :class "team-name"
                                  (esc (sf "~A"
                                           (team-name (game-home-team game)))))
                           (:span :class "score"
                                  (esc (sf "~A" (game-home-score game))))))
                    (:div :class "clear-fix"))))))))))
;;; Scores Page ------------------------------------------------------------- END

;;; New Game API
(defun api-new-game (&key player league)
  (setf (content-type*) "application/json")
  (let* ((game-time (post-parameter "date")))
    (if (empty? game-time)
        (json-result (new-r :error "Game date not provided."))
        (let ((save-res (save-new-game league game-time player))
              (data '()))
          (if (and (succeeded? save-res)
                   ;; New game time within next 7 days
                   (timestamp<=
                    (now)
                    (parse-timestring game-time)
                    (adjust-timestamp (now) (offset :day 7))))
              (send-email-to-players
               (sf "New game in ~A" (league-name league))
               (lambda (player)
                 (sf '("<p>A new <a href='~(~A~)'>game</a> was added in the "
                       "<strong title='~A'>~A</strong> on ~A.</p>")
                     (build-url (sf "~A/games/~A"
                                    (league-name league)
                                    (game-id (r-data save-res)))
                                player)
                     (league-full-name league)
                     (league-name league)
                     (pretty-time game-time)))
               league))
          (push (pretty-time (game-time (r-data save-res))) data)
          (push (game-id (r-data save-res)) data)
          (json-result save-res data)))))
;;; New Game API ------------------------------------------------------------ END

;;; Game Detail Page
(defun www-game-detail-page (&key player league)
  (let* ((game-id (last1 (path-segments *request*)))
         (game (get-game game-id))
         (player-gc (if game (or (game-confirm-for game player)
                                 (make-game-confirm
                                  :player player
                                  :confirm-type :no-response))))
         (confirm-qp (get-parameter "confirm"))
         (confirm-save-res (new-r :info "Confirmation not updated.."))
         (show-confirm-inputs
           (and game
                (not (string-equal "cancelled" (game-progress game)))
                (not (string-equal "final" (game-progress game))))))
    (if (null game)
        (www-not-found-page :player player :league league)
        (progn
          ;; Update player's confirmation status and reload game object, unless
          ;; game is in final state
          (when (and confirm-qp
                     (not (string-equal "final" (game-progress game))))
            (setf confirm-save-res
                  (save-game-confirm game player confirm-qp))
            (when (succeeded? confirm-save-res)
              (setf game (r-data confirm-save-res))
              (setf player-gc (game-confirm-for game player))))
          (standard-page
              (:title (fmt "Game on ~A" (pretty-time (game-time game)))
               :player player
               :league league
               :page-id "game-detail-page")
            ;; Edit/Delete/Email buttons
            (if (is-commissioner? player league)
                (htm
                 (:section :id "quick-crud-btns"
                           (if (not (string-equal "cancelled"
                                                  (game-progress game)))
                               (htm
                                (:button :id "edit-btn"
                                         :class "button crud-btn"
                                         :onclick "page.editGame()"
                                         :title "Edit"
                                         (:i :class "fa fa-pencil"))))
                           (:button :id "email-reminder-btn"
                                    :class "button crud-btn"
                                    :onclick "page.sendEmailReminder()"
                                    :title
                                    (sf '("Send email reminder to all active "
                                          "players of this game now"))
                                    (:i :class "fa fa-envelope"))
                           (:button :id "delete-btn"
                                    :class "button crud-btn"
                                    :data-delete-msg
                                    (if (string-equal "cancelled"
                                                      (game-progress game))
                                        "Are you sure you want to delete this game?"
                                        "Are you sure you want to cancel this game?")
                                    :onclick "page.deleteGame()"
                                    :title "Delete"
                                    (:i :class "fa fa-trash")))))
            ;; Quick CRUD button result
            (:div :id "quick-crud-res")
            ;; Game Time Display (read only)
            (:h1 :id "time-status-ro"
                 (:span :id "game-time-ro"
                        (esc (pretty-time (game-time game))))
                 (if (and (non-empty? (game-progress game))
                          (not (string-equal "new" (game-progress game))))
                     (htm
                      (:span :id "game-state-ro"
                             :class "uppercase"
                             (fmt " - ~A" (game-progress game))))))
            ;; Game Time/Status/Emails (editable)
            (:div :id "game-info-edit"
                  :class "background-highlight"
                  :data-game game-id
                  :style "display:none"
                  ;; Game Time/Status Edit
                  (:div :id "time-status-rw"
                        (:h2 "Game Time")
                        (:div :class "col col-3"
                              (:input :id "game-date-rw"
                                      :class "full-width"
                                      :onchange "page.updateGameTime()"
                                      :onkeyup "page.updateGameTime()"
                                      :placeholder "Game date (e.g. Dec 12 2016)"
                                      :title "Game date (e.g. Dec 12 2016)"
                                      :value (format-timestring
                                              nil
                                              (parse-timestring (game-time game))
                                              :format
                                              '(:short-month " " :day " " :year))))
                        (:div :class "col col-3"
                              (:input :id "game-time-rw"
                                      :class "full-width"
                                      :onchange "page.updateGameTime()"
                                      :onkeyup "page.updateGameTime()"
                                      :placeholder "Game time (e.g. 7:00 pm)"
                                      :title "Game time (e.g. 7:00 pm)"
                                      :value (format-timestring
                                              nil
                                              (parse-timestring (game-time game))
                                              :format '(:hour12
                                                        ":"
                                                        (:min 2)
                                                        " "
                                                        :ampm))))
                        (:div :class "col col-3"
                              (:select :id "game-status-ddl"
                                       :class "full-width"
                                       :onchange "page.updateGameState()"
                                       :onkeyup "page.updateGameState()"
                                       :title "Game state"
                                       (dolist (gps game-progress-states)
                                         (htm
                                          (:option :selected
                                                   (string-equal
                                                    gps
                                                    (game-progress game))
                                                   :value gps
                                                   (fmt "~A" gps))))))
                        (:div :class "clear-fix"))
                  (:br)
                  (:h2 "Game Notes")
                  (:textarea :id "game-notes-input"
                             :placeholder "Special notices, alerts, etc."
                             (esc (game-notes game)))
                  (:br)
                  ;; Email Reminders
                  (if (is-commissioner? player league)
                      (htm
                       (:button :id "email-reminders-toggle"
                                :class "clear-button"
                                :onclick "page.showEmailRemindersSection()"
                                (:h2
                                 "Email Reminders"
                                 (:i :class "fa fa-chevron-circle-down")))
                       (:div :id "email-reminders-content"
                             :style "display:none"
                             (:ul
                              (:li (fmt "~A"
                                        (pretty-time (game-email-reminder game))))))
                       (:br)))
                  ;; Save button/result
                  (if (is-commissioner? player league)
                      (htm
                       (:div
                        (:button :id "save-game-info-btn"
                                 :class "button wide-button crud-btn"
                                 :onclick "page.saveGame()"
                                 (:span :class "button-text" "Save"))
                        (:div :id "save-res")))))
            ;; Player-Specific Game Confirmation
            (when show-confirm-inputs
              (htm
               (:section
                :id "confirm-inputs"
                (:b "Your current status for this game is:&nbsp;")
                (:select :id "game-confirm-opts"
                         :onchange "page.confirmTypeChanged(this)"
                         (doplist (ct-id ct-name confirm-types)
                           (htm
                            (:option :selected
                                     (string-equal ct-id
                                                   (game-confirm-confirm-type
                                                    player-gc))
                                     :value ct-id (esc ct-name)))))
                (:span :id "confirm-type-status"
                       (cond ((eq :success (r-level confirm-save-res))
                              (htm
                               (:i :class
                                   (sf "fa fa-check ~(~A~)"
                                       (r-level confirm-save-res))
                                   :title
                                   (esc (r-message confirm-save-res)))))
                             ((failed? confirm-save-res)
                              (htm
                               (:i :class
                                   (sf "fa fa-exclamation-circle ~(~A~)"
                                        (r-level confirm-save-res))
                                   :title
                                   (esc (r-message confirm-save-res)))))))
                (:div :id "reason-input-group"
                      (:textarea :id "reason-input"
                                 :maxlength game-confirm-reason-max-length
                                 :onchange "page.reasonTextChanged(this)"
                                 :onkeyup "page.reasonTextChanged(this)"
                                 :placeholder
                                 (sf '("Notes on your status (e.g. reason for "
                                       "not playing)"))
                                 (esc (game-confirm-reason player-gc)))
                 (:div :id "save-confirm-group"
                       (:div :id "reason-input-info"
                             (fmt "~A chars left"
                                  (- game-confirm-reason-max-length
                                      (length (game-confirm-reason
                                               player-gc)))))
                       (:button :class "button crud-btn"
                                :onclick "page.saveConfirmInfo()"
                                "Update")
                       (:div :class "clear-fix"))))))
            ;; Game Notes
            (:section
             :id "game-notes"
             :class (if (empty? (game-notes game)) "hidden" "")
             (:h2 "Game Notes")
             (:p :id "game-notes-ro" (esc (game-notes game))))
            ;; Players Confirmed To Play
            (:section
             :id "confirmed-players-section"
             (:h2 :id "confirmed-heading-many"
                  :class "blue-heading"
                  :style (if (empty? (confirmed-players game)) "display:none")
                  (:span "Confirmed to play")
                  (:span :id "confirmed-count"
                          (fmt "(~A)" (length (confirmed-players game)))))
             (:h2 :id "confirmed-heading-zero"
                  :class "grey-heading"
                  :style (if (confirmed-players game) "display:none")
                         "No players confirmed to play")
             (:ul :class "template-player-item"
                  (:li :class "player-item"
                       (:span :class "player-name" "")
                       (:span :class "confirm-type" "&nbsp;")
                       (:span :class "confirm-btn-toggle"
                              (:button :class "button"
                                       :onclick "page.unconfirmPlayer(this)"
                                       :title "Move to \"Not playing\" section"
                                       (:i :class "fa fa-chevron-circle-down")))
                       (:select :class "player-position"
                                :onchange "page.positionChanged(this)"
                                (dolist (pos players-positions)
                                  (htm
                                   (:option :value pos
                                            :selected nil
                                            (esc pos)))))
                       (:span :class "confirm-info"
                              (:span :class "confirm-reason" "")
                              (:span :class "confirm-time"
                                     :title "Date confirmed"
                                     ""))
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
                                                      (getf
                                                       confirm-types
                                                       (game-confirm-confirm-type
                                                        pc))))
                          :data-reason (esc (game-confirm-reason pc))
                          :data-response-time (pretty-time
                                               (game-confirm-time pc))
                          (:span :class "player-name"
                                 (esc (player-name (-> pc player))))
                          (:span :class "confirm-type" "&nbsp;")
                          (:span :class "confirm-btn-toggle"
                                 (:button :class "button"
                                          :onclick "page.unconfirmPlayer(this)"
                                          :title
                                          "Move to \"Not playing\" section"
                                          (:i :class
                                              "fa fa-chevron-circle-down")))
                          (:select :class "player-position"
                                   :onchange "page.positionChanged(this)"
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
                                 (:span :class "confirm-reason"
                                        (esc (game-confirm-reason pc)))
                                 (:span :class "confirm-time"
                                        :title "Date confirmed"
                                        (esc (pretty-time
                                              (game-confirm-time pc)))))
                          (:span :class "clear-fix"))))))
            ;; Players Not Playing Or Unsure
            (:section
             :id "unconfirmed-players-section"
             (:h2 :id "unconfirmed-heading" :class "blue-heading"
                  "Not playing or undecided"
                  (:span :id "unconfirmed-count"
                         (fmt "(~A)" (length (unconfirmed-players game)))))
             (:ul :class "template-player-item"
                  (:li :class "player-item"
                       (:span :class "player-name" "")
                       (:span :class "confirm-type" "&nbsp;")
                       (:span :class "confirm-btn-toggle"
                              (:button :class "button"
                                       :onclick "page.confirmPlayer(this)"
                                       :title "Move to \"Confirmed\" section"
                                       (:i :class "fa fa-chevron-circle-up")))
                       (:span :class "player-position" "&nbsp;")
                       (:span :class "confirm-info"
                              (:span :class "confirm-reason" "")
                              (:span :class "confirm-time"
                                     :title "Date confirmed"
                                     ""))
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
                                                      (getf
                                                       confirm-types
                                                       (game-confirm-confirm-type
                                                        pc))))
                          :data-reason (esc (game-confirm-reason pc))
                          :data-response-time (pretty-time
                                               (game-confirm-time pc))
                          (:span :class "player-name"
                                 (esc (player-name (-> pc player))))
                          (:span :class "confirm-type"
                                 (esc (sf "(~A)"
                                          (getf confirm-types
                                                (game-confirm-confirm-type pc)))))
                          (:span :class "confirm-btn-toggle"
                                 (:button :class "button"
                                          :onclick "page.confirmPlayer(this)"
                                          :title "Move to \"Confirmed\" section"
                                          (:i :class "fa fa-chevron-circle-up")))
                          (:span :class "player-position"
                                 (esc (player-position (-> pc player))))
                          (:span :class "confirm-reason"
                                 (esc (game-confirm-reason pc)))
                          (:span :class "confirm-time"
                                 :title "Date confirmed"
                                 (esc (pretty-time
                                       (game-confirm-time pc))))
                          (:span :class "clear-fix"))))))
            ;; Random Teams
            (:section :id "random-teams"
                      (:ul :class "template-player-item"
                           (:li :class "player-item"
                                (:span :class "player-name")
                                (:span :class "player-position")
                                (:span :class "clear-fix")))
                      ; TODO: Remove hard-coded team logos
                      (:div :id "team1" :class "team"
                            (:img :class "team-logo"
                                  :src
                                  "/images/leagues/phl/teams/cripplers.png")
                            (:h2 :class "team-heading" "Cripplers")
                            (:ul :class "team-players data-list"))
                      (:div :id "team2" :class "team"
                            (:img :class "team-logo"
                                  :src
                                  "/images/leagues/phl/teams/panthers.png")
                            (:h2 :class "team-heading" "Panthers")
                            (:ul :class "team-players data-list")))
            ;; Edit Player Dialog
            (:div :id "edit-dialog" :class "dialog"
                  (:div :class "dialog-content"
                        (:header "Editing Player")
                        (:section :class "content"
                                  (:table
                                   (:tr :class "input-row"
                                        (:td :class "label-col"
                                             (:label :for "player-name-edit"
                                                     "Name: "))
                                        (:td :class "input-col"
                                             (:input :id "player-name-edit"
                                                     :type "text")))
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
                                     (:label :for "player-active-edit"
                                             "Is Active: "))
                                    (:td
                                     (:input :id "player-active-edit"
                                             :type "checkbox"))))
                                  (:div :class "actions"
                                        (:button
                                         :class "button save-btn"
                                         :data-player-id "0"
                                         :onclick "page.savePlayer()"
                                         "Save")
                                        (:button
                                         :class "button cancel-btn"
                                         :onclick
                                         "page.closeDialog(\"#edit-dialog\")"
                                         "Cancel")))))
            (:br)
            (:button :id "make-teams"
                     :class "button wide-button"
                     :onclick "page.makeTeams()"
                     :title "Generate random teams"
                     (:i :class "fa fa-random")
                     (:span :class "button-text" "Make Teams"))
            (:button :id "add-player"
                     :class "button wide-button"
                     :onclick "page.addPlayer()"
                     (:i :class "fa fa-user-plus")
                     (:span :class "button-text" "Add Player"))
            (:button :id "pick-players"
                     :class "button wide-button"
                     :onclick "page.pickPlayers()"
                     :title "Choose players"
                     (:i :class "fa fa-check-circle-o")
                     (:span :class "button-text" "Pick Players")))))))
;;; Game Detail Page -------------------------------------------------------- END

;;; Game Update API
(defun api-game-update (&key player league)
  (setf (content-type*) "application/json")
  (let* ((game-id (last1 (path-segments *request*)))
         (game (get-game game-id))
         (delete-game? (string-equal "true" (post-parameter "deleteGame")))
         (send-email-reminder? (string-equal "true"
                                             (post-parameter
                                              "sendEmailReminder")))
         (game-time (post-parameter "gameTime"))
         (game-progress (post-parameter "gameProgress"))
         (game-notes (post-parameter "gameNotes"))
         (confirm-type (post-parameter "confirmType"))
         (reason (post-parameter "reason"))
         (save-res (new-r :info "Nothing updated.")))
    (when (null game)
      (setf (return-code*) +http-not-found+)
      (return-from api-game-update
        (json-result (new-r :error "Game not found."))))
    (cond
      ;; Delete/cancel game
      (delete-game?
       (setf save-res
             (if (string-equal "cancelled" (game-progress game))
                 (delete-game game player)
                 (cancel-game game player)))
       (if (and (succeeded? save-res)
                (string-equal "new" (game-progress game))
                ;; New game time within next 7 days
                (timestamp<=
                 (now)
                 (parse-timestring (game-time game))
                 (adjust-timestamp (now) (offset :day 7))))
           (send-email-to-players
            (sf "Game cancelled in ~A" (league-name league))
            (lambda (player-to-email)
              (sf '("<p>An upcoming game in the <a href='~A' title='~A'>~A</a> "
                    "on ~A was cancelled.</p>"
                    "<p><strong>Game Notes:</strong></p>"
                    "<p>~A</p>")
                  (build-url (sf "~A/games/schedule" (league-name league))
                             player-to-email)
                  (league-full-name league)
                  (league-name league)
                  (pretty-time (game-time game))
                  (escape-string (game-notes game))))
            league))
       (json-result save-res))
      ;; Update game info (e.g. time, progress, notes, etc.)
      (game-time
       (if (string-equal "cancelled" (game-progress game))
           (return-from api-game-update
             (json-result (new-r :warning "Can't edit cancelled games."))))
       (setf save-res (update-game-info game player game-time game-progress game-notes))
       (if (and (succeeded? save-res)
                (not (string-equal game-time (game-time game)))
                (string-equal "new" (game-progress game))
                ;; New game time within next 7 days
                (timestamp<=
                 (now)
                 (parse-timestring game-time)
                 (adjust-timestamp (now) (offset :day 7))))
           (send-email-to-players
            (sf "Game time changed in ~A" (league-name league))
            (lambda (player-to-email)
              (sf '("<p>An upcoming <a href='~(~A~)'>game's</a> time changed in "
                    "the <strong title'~A'>~A</strong> from ~A to ~A.</p>")
                  (build-url (sf "~A/games/~A"
                                 (league-name league)
                                 (game-id game))
                             player-to-email)
                  (league-full-name league)
                  (league-name league)
                  (pretty-time (game-time game))
                  (pretty-time game-time)))
            league))
       (json-result save-res))
      (send-email-reminder?
       (if (or (string-equal "final" (game-progress game))
               (string-equal "cancelled" (game-progress game)))
           (return-from api-game-update
             (json-result
              (new-r :warning
                     (sf '("Can't send email reminders for cancelled or final "
                           "games."))))))
       (email-game-reminder game)
       (json-result (new-r :success "Emails sent!")))
      ;; Update player's confirmation status
      (confirm-type
       (setf save-res (save-game-confirm game player confirm-type reason))
       (if (and (succeeded? save-res)
                (not (string-equal "cancelled" (game-progress game)))
                (not (string-equal "final" (game-progress game)))
                ;; Game time within next 3 days
                (timestamp<=
                 (now)
                 (parse-timestring (game-time game))
                 (adjust-timestamp (now) (offset :day 3))))
           (send-email-to-players
            (sf "Game confirmation change in ~A" (league-name league))
            (lambda (player-to-email)
              (sf '("<p><a href='~(~A~)'>~A</a> updated their confirmation "
                    "status for the upcoming <a href='~(~A~)'>game</a> in the "
                    "<strong title='~A'>~A</strong> on ~A.</p>"
                    "<p>Status: <b>~(~A~)</b></p>"
                    "~A")
                  (build-url (sf "~A/players/~A/~A"
                                 (league-name league)
                                 (player-name player)
                                 (player-id player))
                             player-to-email)
                  (player-name player)
                  (build-url (sf "~A/games/~A"
                                 (league-name league)
                                 (game-id game))
                             player-to-email)
                  (league-full-name league)
                  (league-name league)
                  (pretty-time (game-time game))
                  (getf confirm-types
                        (find confirm-type confirm-types :test #'string-equal))
                  (if (empty? reason)
                      ""
                      (sf '("<p>Notes:</p>"
                            "<blockquote>~A</blockquote>")
                          reason))))
            league
            :immediate-notify-only? t))
       (json:encode-json-plist-to-string
        (if (succeeded? save-res)
            `(level :success
                    message "Confirmation updated!"
                    data ,(game-confirm-reason
                           (game-confirm-for (r-data save-res) player)))
            `(level ,(r-level save-res)
                    message ,(r-message save-res)
                    data ""))))
      (t (json-result (new-r :error "Unable to determine save type."))))))
;;; Game Update API --------------------------------------------------------- END

;;; Player List Page
(defun www-player-list-page (&key player league)
  (standard-page
      (:title "Players"
       :player player
       :league league
       :page-id "player-list-page")
    (:h2 :class "blue-heading"
         "Players")
    (:ul :id "all-players" :class "data-list"
         (dolist (p (get-players :league league))
           (htm
            (:li :class "player-item"
                 :data-id (player-id p)
                 :data-name (player-name p)
                 :data-position (player-position p)
                 (:a :class "player-name"
                     :href (sf "/~(~A~)/players/~(~A~)/~A"
                               (league-name league)
                               (clean-uri-segment (player-name p))
                               (player-id p))
                     (esc (player-name p)))
                 (if (not (player-active-in? p league))
                     (htm
                      (:i :title "Currently unavailable to play"
                          "(inactive)")))
                 (:span :class "action-buttons"
                        (:button :class "button"
                                 :onclick "page.editPlayer(this, \"#all-players\")"
                                 (:i :class "fa fa-pencil-square-o")))
                 (:span :class "player-position" (esc (player-position p)))
                 (:span :class "clear-fix")))))
    (:div :id "edit-dialog" :class "dialog"
          (:div :class "dialog-content"
                (:header "Editing Player")
                (:section :class "content"
                          (:table
                           (:tr :class "input-row"
                                (:td :class "label-col"
                                     (:label :for "player-name-edit" "Name: "))
                                (:td :class "input-col"
                                     (:input :id "player-name-edit"
                                             :type "text")))
                           (:tr
                            (:td
                             (:label :for "player-pos-edit" "Position: "))
                            (:td
                             (:select :id "player-pos-edit"
                                      (dolist (pos players-positions)
                                        (htm
                                         (:option :selected t
                                                  :value pos (esc pos)))))))
                           (:tr
                            (:td
                             (:label :for "player-active-edit" "Is Active: "))
                            (:td
                             (:input :id "player-active-edit"
                                     :checked t
                                     :type "checkbox"))))
                          (:div :class "actions"
                                (:button
                                 :class "button save-btn"
                                 :data-player-id "0"
                                 :onclick "page.savePlayer(\"#all-players\", \".template-player-item .player-item\")"
                                 "Save")
                                (:button
                                 :class "button cancel-btn"
                                 :onclick
                                 "page.closeDialog(\"#edit-dialog\")"
                                 "Cancel")))))
    (:br)
    (:button :id "add-player"
             :class "button wide-button"
             :onclick "page.addPlayer()"
             (:i :class "fa fa-user-plus")
             (:span :class "button-text" "Add Player"))
    (:div :class "template-items"
          (:ul :class "template-player-item"
               (:li :class "player-item"
                    (:span :class "player-name" "")
                    (:span :class "action-buttons"
                           (:button :class "button"
                                    :onclick "page.editPlayer(this, \"#all-players\")"
                                    (:i :class "fa fa-pencil-square-o")))
                    (:span :class "player-position" "&nbsp;")
                    (:span :class "clear-fix"))))))
;;; Player List Page -------------------------------------------------------- END

;;; Player Detail Page
(defun www-player-detail-page (&key player league)
  (let* ((path-segs (path-segments *request*))
         (target-player-id (loose-parse-int (last1 path-segs)))
         (target-player (get-player :id target-player-id)))
    (if (null target-player)
        (return-from www-player-detail-page
          (www-not-found-page :player player :league league)))
    (let ((leagues (get-all-leagues))
          (commissions '()))
      (dolist (l leagues)
        (if (find (player-id target-player) (league-commissioner-ids l))
            (push l commissions)))
      (standard-page
          (:title "Player"
           :player player
           :league league
           :page-id "player-detail-page")
        ;; Player Picture
        (:section :id "left-col" :class "col"
                  (:p
                   (:img :id "user-img"
                         :class "full-width"
                         :src "/images/user.png")))
        ;; Editable Section
        (:section :id "right-col" :class "col"
                  ;; Edit Button
                  (if (or (player-admin? player)
                          (= target-player-id (player-id player)))
                      (htm
                       (:a :class "button wide-button"
                           :href (sf "/~(~A~)/users/~(~A~)/~A"
                                     (league-name league)
                                     (clean-uri-segment
                                      (player-name target-player))
                                     (player-id target-player))
                           "Edit")))
                  ;; Name/active status
                  (:h1
                   (:span (esc (player-name target-player)))
                   (if (not (player-active-in? target-player league))
                       (htm
                        (:i :title "Currently unavailable to play"
                            "(inactive)"))))
                  ;; Admin?
                  (if (player-admin? target-player)
                      (htm
                       (:p :id "admin"
                           (:i :class "fa fa-star")
                           (:span "Administrator"))))
                  ;; Commissioned Leagues
                  (if commissions
                      (htm
                       (:p :id "commissioner"
                           (:i :class "fa fa-star")
                           (:span "Commissioner: ")
                           (dolist (l commissions)
                             (htm
                              (:a :href (sf "/~(~A~)" (league-name l))
                                  (esc (league-name l)))
                              (:span :class "comma" ","))))))
                  (:p
                   (:span "Default Position: ")
                   (:b (fmt "~A" (player-position target-player)))))))))
;;; Player Detail Page ------------------------------------------------------ END

;;; Manage League Page
(defun www-manage-league-page (&key player league)
  (standard-page
      (:title "Manage"
       :player player
       :league league
       :page-id "manage-league-page")
    (:h2 (fmt "~A Management" (league-name league)))
    (:p :style "font-weight:bold"
        "Commissioners: "
        (dolist (commish
                 (map 'list
                      (lambda (commish-id) (get-player :id commish-id))
                      (league-commissioner-ids league)))
          (htm
           (:a :href (sf "/~(~A~)/players/~(~A~)/~A"
                         (league-name league)
                         (clean-uri-segment (player-name commish))
                         (player-id commish))
               (esc (player-name commish)))
           (:span :class "comma" ","))))
    (:p
     (:span "Send email reminder")
     (:input :id "email-reminder-day-offset"
             :type "text"
             :value (league-game-reminder-day-offset league))
     (:span "day(s) ahead of game at")
     (:input :id "email-reminder-time"
             :type "text"
             :value (league-game-reminder-time league)))
    (:p
     (:label
      (:input :id "send-automated-emails"
              :checked (league-send-automated-emails? league)
              :type "checkbox")
      (:span " Send automated emails")))
    (:button :id "save-btn"
             :class "button wide-button"
             :onclick "page.save()"
             "Save")
    (:p :id "save-result")))
;;; Manage League Page ------------------------------------------------------ END

;;; League Save API
(defun api-league-save (&key player league)
  (setf (content-type*) "application/json")
  (when (not (is-commissioner? player league))
    (setf (return-code*) +http-forbidden+)
    (return-from api-league-save
      (json-result
       (new-r :error "Sorry, you don't have permission to make this change."))))
  (setf (league-send-automated-emails? league)
        (string-equal "true" (post-parameter "sendAutomatedEmails")))
  (setf (league-game-reminder-day-offset league)
        (loose-parse-int (post-parameter "gameReminderDayOffset")))
  (setf (league-game-reminder-time league) (post-parameter "gameReminderTime"))
  (json-result (update-league league)))
;;; League Save API --------------------------------------------------------- END
