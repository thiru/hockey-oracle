;;;; Hockey Oracle package definitions

(in-package :cl-user)

(defpackage :hockey-oracle
  (:use :alexandria :cl :cl-who :glu :hunchentoot :local-time :redis
   :split-sequence)
  (:documentation "Sole package of this app.")
  (:export
   :web-app
   :start-server!
   :stop-server))
