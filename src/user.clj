;; ## Summary
;;
;; Initial namespace loaded when using a REPL (e.g. using `clj`).
;;
(ns user
  (:require
            [cheshire.core :as json]
            [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]
            [clojure.string :as string]

            [thiru.fsreload :as reload]
            [thiru.logging :refer :all]
            [thiru.repl :as repl]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.main :as cli]
            [hockeyoracle.core.db :as db]
            [hockeyoracle.web.server :as server]))

;; We only want to start the watcher once.
(defonce watch-started?
  (reload/start-watch!))
