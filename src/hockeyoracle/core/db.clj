;; ## Summary
;;
;; Database access functions.
;;
(ns hockeyoracle.core.db
  (:require
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]

            [thiru.logging :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]))

(def pg-db
  "Database connection spec."
  (:db-spec @app/config))

;;; The following two protocol extensions support converting Postgresql array
;;; columns to and from Clojure vectors.
;;;
;;; This was taken from: https://stackoverflow.com/a/25786990.
;;;
;;; It might be worth just using the author's library at some point:
;;; https://github.com/remodoy/clj-postgresql.

(extend-protocol clojure.java.jdbc/ISQLParameter
  clojure.lang.IPersistentVector
  (set-parameter [v ^java.sql.PreparedStatement stmt ^long i]
    (let [conn (.getConnection stmt)
          meta (.getParameterMetaData stmt)
          type-name (.getParameterTypeName meta i)]
      (if-let [elem-type (when (= (first type-name) \_)
                           (apply str (rest type-name)))]
        (.setObject stmt i (.createArrayOf conn elem-type (to-array v)))
        (.setObject stmt i v)))))

(extend-protocol clojure.java.jdbc/IResultSetReadColumn
  java.sql.Array
  (result-set-read-column [val _ _]
    (into [] (.getArray val))))

(defn get-users
  "Gets all users."
  []
  (jdbc/query pg-db ["SELECT * FROM users"]))

(defn get-user
  "Get the user with the specified criteria.
  
  TODO"
  [criteria]
  (first
    (cond
      (:id criteria)
      (jdbc/query pg-db ["SELECT * FROM users where id = ? LIMIT 1"
                         (:id criteria)])
      
      (:name criteria)
      (jdbc/query pg-db ["SELECT * FROM users where lower(name) = ? LIMIT 1"
                         (string/lower-case (:name criteria))])

      (:email criteria)
      (jdbc/query pg-db ["SELECT * FROM users where lower(email) = ? LIMIT 1"
                         (string/lower-case (:email criteria))]))))
