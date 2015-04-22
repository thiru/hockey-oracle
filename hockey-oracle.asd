;;;; Hockey Oracle build configuration

(asdf:defsystem :hockey-oracle
  :version "0.2"
  :description "An aid for pick-up hockey enthusiasts"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :depends-on (:glu :cl-who :hunchentoot)
  :components ((:static-file "README.md")
               (:static-file "LICENSE")
               (:static-file "EULA")
               (:file "package")
               (:file "lib/main")
               (:file "lib/web")))

