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
  (:use :alexandria :cl :glu :redis :split-sequence)
  (:documentation "Hockey Oracle core domain.")
  (:export
   :levels
   :r
   :r-level
   :r-message
   :r-data
   :new-r
   :succeeded?
   :failed?
   :random-string
   :gen-hash
   :league
   :make-league
   :league-id
   :league-name
   :league-created
   :league-active?
   :get-all-leagues
   :get-league
   :season
   :make-season
   :season-id
   :season-name
   :season-start-date
   :season-end-date
   :get-seasons
   :game
   :game-id
   :game-time
   :game-progress
   :game-home-team
   :game-away-team
   :game-home-score
   :game-away-score
   :game-confirms
   :get-game
   :get-games
   :confirm-types
   :game-confirm
   :game-confirm-reason-max-length
   :make-game-confirm
   :game-confirm-player
   :game-confirm-time
   :game-confirm-confirm-type
   :game-confirm-reason
   :game-confirm-for
   :confirmed-players
   :unconfirmed-players
   :save-game-confirm
   :player
   :make-player
   :player-id
   :player-name
   :player-auth
   :player-perm-auth
   :player-temp-auth
   :player-salt
   :player-position
   :player-active?
   :player-activate
   :player-deactivate
   :players-positions
   :get-all-players
   :get-players
   :get-player
   :team
   :make-team
   :team-id
   :team-name
   :team-logo
   :get-teams
   :get-team
   :get-secure-key))

(defpackage :hockey-oracle.web
  (:use :alexandria :cl :cl-who :glu :hockey-oracle.app :hockey-oracle.core
        :hunchentoot :local-time :split-sequence)
  (:shadowing-import-from :glu :str)
  (:documentation "Hockey Oracle web interface.")
  (:export
   :web-app
   :start-server!
   :stop-server))
