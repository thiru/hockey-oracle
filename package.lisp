;;;; Hockey Oracle package definitions

(defpackage :hockey-oracle
  (:use :cl :glu :asdf :redis)
  (:documentation "Hockey Oracle core domain/API")
  (:export
    :app-version
    :app-updated
    :league
    :league-id
    :league-name
    :league-created
    :league-active?
    :get-all-leagues
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
  (:use :hockey-oracle :cl :asdf :glu :cl-who :hunchentoot)
  (:documentation "Hockey Oracle web interface")
  (:export
    :web-app
    :start-server!
    :stop-server))

