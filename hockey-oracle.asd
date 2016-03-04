;;;; Hockey Oracle build configuration

(in-package :cl-user)

(asdf:defsystem :hockey-oracle
  :version (:read-file-form "version.lisp-expr")
  :description "An aid for pick-up hockey enthusiasts"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :depends-on (:alexandria :bordeaux-threads :cl-json :cl-redis :cl-smtp :cl-who
               :glu :hunchentoot :ironclad :local-time :split-sequence)
  :components ((:file "src/package")
               (:file "src/app")
               (:file "src/core")
               (:file "src/web")))
