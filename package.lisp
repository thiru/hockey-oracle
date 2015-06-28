;;;; Hockey Oracle package definitions

(defpackage :hockey-oracle
  (:use :cl :glu :asdf)
  (:documentation "Hockey Oracle core domain/API")
  (:export
    :app-version
    :app-updated
    :player
    :sorted-players
    :player-add
    :player-id
    :player-first-name
    :player-last-name
    :player-position
    :player-active?
    :player-activate
    :player-deactivate))

(defpackage :hockey-oracle.web
  (:use :hockey-oracle :cl :asdf :glu :cl-who :hunchentoot)
  (:documentation "Hockey Oracle web interface")
  (:export 
    :web-app
    :start-server!
    :stop-server))

