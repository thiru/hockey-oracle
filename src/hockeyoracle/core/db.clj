;; ## Summary
;;
;; Database access functions.
;;
(ns hockeyoracle.core.db
  (:require
            [clj-postgresql.core :as pg]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]

            [thiru.logging :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]))

;; ## General Database Stuff

(def conn-pool
  "Database connection pool.

  We delay the connection pool intialization to prevent this from happening at
  compile-time (as the *clj-postgresql* library recommends)."
  (delay (pg/pool :host (-> @app/config :db-spec :host)
                  :dbname (-> @app/config :db-spec :dbname)
                  :user (-> @app/config :db-spec :user)
                  :password (-> @app/config :db-spec :password))))

;; Read timestamps as plain strings.
;; I'm going this route because JDBC seems to double "correct" the UTC time
;; given by Postgres. So for e.g. if 13:00 (UTC) is stored in the database,
;; JDBC returns a `java.sql.Timstamp` with 19:00. At least, this way I have
;; full control on how the time is interpreted (local, UTC, etc.).
(extend-protocol clojure.java.jdbc/IResultSetReadColumn
  java.sql.Timestamp
  (result-set-read-column [val _ _]
    (str val)))

;; ## Leagues

(defn get-leagues
  "Gets leagues.
  
  * `ids`
    * A list of league ids
    * If this parameter is not specified, all leagues are retrieved"
  ([]
   (jdbc/query @conn-pool ["SELECT * FROM leagues"]))
  ;; TODO: spec to ensure this is a list of integers
  ([ids]
   (if (< 0 (count ids))
     (jdbc/query @conn-pool
                 [(str "SELECT * FROM leagues "
                       "WHERE id IN ("
                       (string/join ", " ids)
                       ")")]))))

(defn get-league
  "Get the league with the specified column criteria.
  
  * `columns`
    * A map of column names and values
    * Only one column is currently supported
  
  Name and tricode columns are matched without case sensitivity."
  [columns]
  (first
    (cond
      (:id columns)
      (jdbc/query @conn-pool
                  ["SELECT * FROM leagues WHERE id = ? LIMIT 1"
                   (:id columns)])
      
      (:name columns)
      (jdbc/query @conn-pool
                  [(str "SELECT * FROM leagues "
                        "WHERE lower(name) = ? LIMIT 1")
                   (string/lower-case (:name columns))])
      
      (:tricode columns)
      (jdbc/query @conn-pool
                  [(str "SELECT * FROM leagues "
                        "WHERE lower(tricode) = ? LIMIT 1")
                   (string/lower-case (:tricode columns))]))))

;; ## Games

(defn get-games
  "Get games with the specified criteria.
  
  * `league-id`
    * The id of the league the games belong to
  * `progress`
    * An optional list of progress states to filter by
    * These must be one of: new, underway, final, cancelled"
  [league-id & {:keys [progress]}]
  (if (empty? progress)
    (jdbc/query @conn-pool
                ["SELECT * FROM games WHERE league_id = ?"
                 league-id]
                {:identifiers #(.replace % \_ \-)})
    (jdbc/query @conn-pool
                [(str "SELECT * FROM games "
                      "WHERE league_id = ? "
                      "  AND progress = ANY(?)")
                 league-id
                 progress]
                {:identifiers #(.replace % \_ \-)})))

;; ## Users

(defn get-users
  "Gets all users."
  []
  (jdbc/query @conn-pool ["SELECT * FROM users"]))

(defn get-user
  "Get the user with the specified criteria.
  
  * `columns`
    * A map of column names and values
    * Only one column is currently supported
  
  Name and email are matched without case sensitivity."
  [columns]
  (first
    (cond
      (:id columns)
      (jdbc/query @conn-pool
                  ["SELECT * FROM users WHERE id = ? LIMIT 1"
                   (:id columns)])
      
      (:name columns)
      (jdbc/query @conn-pool
                  ["SELECT * FROM users WHERE lower(name) = ? LIMIT 1"
                   (string/lower-case (:name columns))])

      (:email columns)
      (jdbc/query @conn-pool
                  ["SELECT * FROM users WHERE lower(email) = ? LIMIT 1"
                   (string/lower-case (:email columns))]))))
