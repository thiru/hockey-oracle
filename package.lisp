;;;; Hockey Oracle package definitions

(defpackage :hockey-oracle
  (:use :cl :glu :asdf :redis)
  (:documentation "Hockey Oracle core domain/API")
  (:export
    :app-version
    :app-updated
    :empty?
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
  (:use :hockey-oracle :cl :asdf :glu :cl-who :hunchentoot :local-time
   :split-sequence)
  (:documentation "Hockey Oracle web interface")
  (:export
    :web-app
    :start-server!
    :stop-server))

