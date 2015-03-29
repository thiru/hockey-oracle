(ql:quickload '(cl-who hunchentoot parenscript))

(defpackage :hockey-oracle
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :hockey-oracle)

(defvar app-version 0.2)
(defvar app-updated "Mar 28 2015")

(defclass player ()
  ((player-id :reader player-id
              :initarg :player-id)
   (first-name :reader first-name
               :initarg :first-name)
   (last-name :reader last-name
              :initarg :last-name)
   (pposition :accessor pposition
             :initarg :pposition)
   (active? :accessor active?
            :initarg :active?)))

(defmethod print-object ((object player) stream)
  (print-unreadable-object (object stream :type T)
    (with-slots (player-id first-name last-name pposition active?) object
      (format stream "~s ~s ~s ~s (active? ~s)" player-id first-name last-name pposition active?))))

(defmethod activate-player (p)
  "Activate the given player."
  (setf (active? p) T))

(defmethod deactivate-player (p)
  "Deactivate the given player."
  (setf (active? p) NIL))

(defvar *players* '())
(defvar *player-id-seed* 0)

(defun players ()
  "Get a sorted list of all players."
  (sort (copy-list *players*) #'string< :key #'first-name))


(defun add-player (fname lname pos active?)
  "Add a player to the global list."
  (push
    (make-instance
      'player
      :player-id (incf *player-id-seed*)
      :first-name fname
      :last-name lname
      :pposition pos
      :active? active?)
    *players*))

(add-player "Aiyaz" "Ahmed" "LW" NIL)
(add-player "Anish" "Patel" "RW" T)
(add-player "Brian" "Tse" "D" T)
(add-player "Brian" "Kwan" "D" T)
(add-player "Carmen" "Francese" "C" NIL)
(add-player "Elroy" "Toney" "G" T)
(add-player "Kup" "Santhirasivam" "C" T)
(add-player "Mark" "MacDonald" "RW" T)
(add-player "Mark" "Solis" "G" T)
(add-player "Mauz" "Syed" "RW" T)
(add-player "Osama" "Raza" "LW" T)
(add-player "Robin" "Pinto" "D" T)
(add-player "Steve" "Hall" "LW" NIL)
(add-player "Saif" "Ansari" "LW" T)
(add-player "Taran" "Anderson" "RW" T)
(add-player "Thiru" "Thirunavukarasu" "D" T)
(add-player "Touraj" "Nikou" "C" T)
(add-player "Extra 1" "" "D" NIL)
(add-player "Extra 2" "" "D" NIL)
(add-player "Extra 3" "" "D" NIL)
(add-player "Extra 4" "" "D" NIL)

;;; Web-related code:

(defmacro standard-page ((&key title page-id) &body body)
  "Creates a standard page layout."
  `(with-html-output-to-string
     (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
      (:head
        (:meta :charset "utf-8")
        (:meta :http-equiv "X-UA-Compatible"
               :content "IE=edge")
        (:meta :name "viewport"
               :content "width=device-width, initial-scale=1")
        (:title ,(format nil "~a - Hockey Oracle" title))
        (:link :rel "shortcut icon"
               :href "/images/favicon.ico")
        (:link :type "text/css"
               :rel "stylesheet"
               :href "/deps/font-awesome/css/font-awesome.min.css")
        (:link :type "text/css"
               :rel "stylesheet"
               :href "/styles/base.css"))
        (:script :src "/deps/jquery/jquery-2.1.3.min.js")
        (:script :src "/deps/lodash/lodash.min.js")
        (:script :src "/scripts/main.js")
      (:body
        (:header
          (:a :href "/"
            (:img
              :alt "logo (magic ball)"
              :class "logo"
              :src "/images/banner.jpg")
            (:span :class "title" "Hockey Oracle")))
        (:nav
          (:ul :class "nav-items"
            (:li
              (:a :href "/players" "Players"))
            (:li
              (:a :href "/about" "About"))
            ))
        (:main :id ,page-id
          ,@body)))))

(define-easy-handler (www-home :uri "/") ()
  (redirect "/players"))

(define-easy-handler (www-players :uri "/players") ()
  (standard-page
    (:title "Players"
     :page-id "player-list-page")
    (:table :id "player-list" :class "data-table"
      (:thead
        (:tr
          (:th "Player")
          (:th "Position")))
      (:tbody
        (dolist (p (players))
          (htm
            (:tr :class "player-item"
              ; This indicates the player's initial active status. I.e. it may
              ; change on the client.
              :data-player-active (active? p)
              :onclick "togglePlayerActive(this)"
              (:td
                (:label
                  (:i :class "player-check fa fa-circle-o")
                  (:span :class "label-text player-name"
                    (esc (fmt "~a ~a" (first-name p) (last-name p))))))
              (:td :class "player-position" (esc (pposition p))))))))
    (:section :id "random-teams"
      (:div :id "team1" :class "team"
        (:h3 :class "team-heading"
          (:img :class "team-logo" :src "/images/team-logos/cripplers.png")
          (:span :class "team-name" "Cripplers"))
        (:ul :class "team-players simple-list"))
      (:div :id "team2" :class "team"
        (:h3 :class "team-heading"
          (:img :class "team-logo" :src "/images/team-logos/panthers.png")
          (:span :class "team-name" "Panthers"))
        (:ul :class "team-players simple-list")))
    (:button :id "make-teams"
      :class "wide-button"
      :onclick "makeTeams()"
      :title "Select to generate random teams"
      :type "button"
      (:i :class "fa fa-random")
      "Make Teams")))

(define-easy-handler (www-about :uri "/about") ()
  (standard-page
    (:title "About"
     :page-id "about-page")
    (:p
      "The Hockey Oracle is a simple app that generates teams by randomly "
      "selecting from a pool of active players."
      )
    (:br)
    (:table :class "brief-table"
      (:tr
        (:td "Version")
        (:td (fmt "~a" app-version)))
      (:tr
        (:td "Last Updated")
        (:td (fmt "~a" app-updated)))
      (:tr
        (:td "License")
        (:td
          (:a :href "https://www.gnu.org/licenses/gpl-2.0.html" "GPL v2")))
      (:tr
        (:td "Copyright")
        (:td "2014-2015 Thirushanth Thirunavukarasu")))
    )
  )

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port :document-root #p"public/")))

(start-server 9090)
