;;;; Hockey Oracle build configuration

(in-package :cl-user)

(defpackage :hockey-oracle.system
  (:use :asdf :cl :uiop)
  (:documentation "Hockey Oracle build configuration.")
  (:export
    :base-dir
    :updated
    :version))

(in-package :hockey-oracle.system)

(defsystem :hockey-oracle
  :version (:read-file-form "version.lisp-expr")
  :description "An aid for pick-up hockey enthusiasts"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv2"
  :serial t
  :depends-on (:bordeaux-threads :cl-redis :cl-smtp :cl-who :glu :hunchentoot
               :local-time :split-sequence)
  :components ((:file "src/package")
               (:file "src/main")
               (:file "src/web")))

(defvar base-dir *default-pathname-defaults*)
(defvar version
  (read-file-form (system-relative-pathname :hockey-oracle
                                            "version.lisp-expr")))
(defvar updated
  (read-file-form (system-relative-pathname :hockey-oracle
                                            "date-updated.lisp-expr")))
