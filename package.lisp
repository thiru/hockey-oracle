;;;; Hockey Oracle package definitions

(defpackage :hockey-oracle
  (:use :cl :glu :asdf)
  (:documentation "Hockey Oracle core domain/API")
  (:export
    :app-version
    :app-updated
    :players
    :player-id
    :first-name
    :last-name
    :pposition
    :active?
    :add-player
    :activate-player
    :deactivate-player))

(defpackage :hockey-oracle.web
  (:use :hockey-oracle :cl :asdf :glu :cl-who :hunchentoot)
  (:documentation "Hockey Oracle web interface")
  (:export 
    :web-app
    :start-server!
    :stop-server))

