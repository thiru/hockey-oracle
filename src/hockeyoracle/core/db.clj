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
