;;;; Contains app metadata and configuration.

(in-package :hockey-oracle.app)

(defparameter base-dir (system-relative-pathname :hockey-oracle ""))
(defparameter version
  (read-file-form (system-relative-pathname :hockey-oracle
                                            "version.lisp-expr")))
(defparameter updated
  (read-file-form (system-relative-pathname :hockey-oracle
                                            "date-updated.lisp-expr")))
