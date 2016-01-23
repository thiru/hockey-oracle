;;;; Web-specific functionality.

(in-package :hockey-oracle.web)

;;; General
(defvar main-acceptor nil "The global web-server instance.")
(defvar static-files-dir (merge-pathnames "www/" base-dir))

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
   Side-effects: sets the special variable main-acceptor to the created
   acceptor."
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

(defun to-nice-date-time (date-time-str)
  "Formats a date/time to a user-friendly form. 'date-time-str' is expected to
   be a string of the form 'year4-month2-day2-hour2-min2'."
  (let* ((time-segs (split-sequence #\- date-time-str :remove-empty-subseqs t))
         (timestamp (encode-timestamp 0 ; Nanoseconds
                                      0 ; Seconds
                                      ;; Minute
                                      (safe-parse-int (nth 4 time-segs)
                                                      :fallback 0)
                                      ;; Hour
                                      (safe-parse-int (nth 3 time-segs)
                                                      :fallback 0)
                                      ;; Day
                                      (safe-parse-int (nth 2 time-segs)
                                                      :fallback 1)
                                      ;; Month
                                      (safe-parse-int (nth 1 time-segs)
                                                      :fallback 1)
                                      ;; Year
                                      (safe-parse-int (nth 0 time-segs)
                                                      :fallback 1))))
    (format-timestring nil
                       timestamp
                       :format '(:long-weekday " " :short-month " " :day " "
                                 :year " " :hour12 ":" (:min 2) :ampm))))

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
            (create-regex-dispatcher "^/leagues$" 'www-league-list-page)
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
                                     'www-test-server-error)
            (create-regex-dispatcher "^/test-not-found$"
                                     'www-not-found-page)
            (create-regex-dispatcher "^/[a-zA-Z-]+$"
                                     (lambda ()
                                       (base-league-page 'www-league-detail-page)))
            ))
;;; Routes ------------------------------------------------------------------ END

;;; Template Page
(defmacro standard-page ((&key title page-id league) &body body)
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
             (:title ,(sf "~a - Hockey Oracle" title))
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
                      (if ,league
                          (htm (:div :id "league-name-header"
                                     (esc (league-name ,league)))))
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
  (let ((league (parse-league *request*)))
    (if (and require-league? (null league))
        (www-not-found-page)
        (funcall actual-page league))))
;;; Base Page --------------------------------------------------------------- END

;;; Error Pages
(defun www-not-found-page ()
  (setf (return-code*) +http-not-found+)
  (standard-page
   (:title "Not Found"
    :page-id "not-found-page")
   (:h2 "Not Found")
   (:p "The page or resource you requested could not be found.")
   (:a :href "/" "Go back to the home page")))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 404)) &key)
  (www-not-found-page))

(defun www-server-error-page (league)
  (standard-page
   (:title "Server Error"
    :league league
    :page-id "server-error-page")
   (:h2 "Server Error")
   (:p "Sorry, it looks like something unexpected happened on the server.")
   (:p "An administrator has been notified of the error.")
   (:a :href "/" "Go back to the home page")))

(defmethod acceptor-status-message (acceptor (http-status-code (eql 500)) &key)
  (bt:make-thread (lambda () (send-error-email "A <b>server</b> error occurred.")))
  (www-server-error-page nil))

(defun www-test-server-error ()
  (log-message* :error "Test error page \(error log level).")
  (log-message* :warning "Test error page \(warning log level).")
  (log-message* :info "Test error page \(info log level).")
  (error "This is an intentional error for testing purposes.")
  ;; The following should never be displayed
  (standard-page
   (:title "Test Server Error")
   (:h2 "Test Server Error")))
;;; Error Pages ------------------------------------------------------------- END

;;; Home Page
(defun www-home-page ()
  (redirect "/leagues"))
;;; Home Page --------------------------------------------------------------- END

;;; About Page
(defun www-about-page (league)
  (standard-page
   (:title "About"
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
            (:td (fmt "~a" (to-nice-date-time updated))))
           (:tr
            (:td "License")
            (:td
             (:a :href "https://www.gnu.org/licenses/gpl-2.0.html" "GPL v2")))
           (:tr
            (:td "Copyright")
            (:td "2014-2015 Thirushanth Thirunavukarasu")))))
;;; About Page -------------------------------------------------------------- END

;;; League List Page
(defun www-league-list-page ()
  (standard-page
   (:title "Leagues"
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
(defun www-league-detail-page (league)
  (redirect (sf "/~A/games" (string-downcase (league-name league)))))
;;; League Detail Page ------------------------------------------------------ END

;;; Game List Page
(defun www-game-list-page (league)
  (let* ((games (get-games league)))
    (standard-page
        (:title "Games"
         :league league
         :page-id "game-list-page")
      (if (null games)
          (htm (:div "No games have been created for this league."))
          (htm
           (:table :class "data-table"
                   (:tbody
                    (dolist (game games)
                      (htm
                       (:tr
                        (:td
                         (:a :href (sf "/~A/games/~A"
                                       (string-downcase (league-name
                                                         league))
                                       (game-date-time game))
                             (esc (to-nice-date-time
                                   (game-date-time game)))))
                        (:td ""))))))))))) ; TODO: game-state
;;; Game List Page ---------------------------------------------------------- END

;;; Game Detail Page
(defun www-game-detail-page (league)
  (standard-page
   (:title "Game on TODO"
    :league league
    :page-id "game-detail-page")
   (:h2 "TODO: Game detail page")))
;;; Game Detail Page -------------------------------------------------------- END

;;; Player List Page
(defun www-player-list-page (league)
  (standard-page
   (:title "Players"
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
             (:th :class "name-col" "Player")
             (:th :class "position-col" :title "Position" "Pos")
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
                 :class "player-name-col"
                 :onclick "togglePlayerActive(this)"
                 (:i
                  :class (if (player-active? p)
                             "player-check fa fa-check-circle-o"
                             "player-check fa fa-circle-o")
                  :title "When checked the player is considered active/able to play")
                 (:span :class "player-name"
                        (esc (fmt "~a ~a"
                                  (player-first-name p)
                                  (player-last-name p)))))
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
            (:span :class "button-text" "Pick Players"))))
;;; Player List Page -------------------------------------------------------- END
