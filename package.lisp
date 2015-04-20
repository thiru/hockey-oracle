;;;; Hockey Oracle package definitions

(defpackage :hockey-oracle
  (:use :cl :asdf :glu :cl-who :hunchentoot)
  (:documentation "Main package - currently contains everything!")
  (:export 
    :app-version
    :app-updated
    :players
    :add-player
    :activate-player
    :deactivate-player
    :web-app
    :start-server!
    :stop-server))

