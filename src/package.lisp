;;;; Hockey Oracle package definitions

(in-package :cl-user)

(defpackage :hockey-oracle.app
  (:use :asdf :cl :uiop)
  (:documentation "Hockey Oracle build configuration.")
  (:export
   :base-dir
   :updated
   :version))

(defpackage :hockey-oracle.core
  (:use :cl :glu :redis)
  (:documentation "Hockey Oracle core domain.")
  (:export
   :league
   :league-id
   :league-name
   :league-created
   :league-active?
   :get-all-leagues
   :get-league
   :season
   :season-id
   :season-name
   :season-start-date
   :season-end-date
   :get-seasons
   :game
   :game-id
   :game-date-time
   :game-progress
   :get-games
   :player
   :player-id
   :player-first-name
   :player-last-name
   :player-position
   :player-active?
   :player-activate
   :player-deactivate
   :players-positions
   :get-all-players
   :get-players
   :get-secure-key))

(defpackage :hockey-oracle.web
  (:use :cl :cl-who :glu :hockey-oracle.app :hockey-oracle.core :hunchentoot
        :local-time :split-sequence)
  (:documentation "Hockey Oracle web interface.")
  (:export
   :web-app
   :start-server!
   :stop-server))
