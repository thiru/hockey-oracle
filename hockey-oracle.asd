;;;; Hockey Oracle build configuration

(asdf:defsystem :hockey-oracle
  :version (:read-file-form "version.lisp-expr")
  :description "An aid for pick-up hockey enthusiasts"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :depends-on (:bordeaux-threads :cl-redis :cl-smtp :cl-who :glu :hunchentoot)
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "EULA")
               (:file "package")
               (:file "main")
               (:file "web")))

