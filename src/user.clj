;; ## Summary
;;
;; Initial namespace loaded when using a REPL (e.g. using `clj`).
;;
(ns user
 (:require
           [clojure.string :as string]

           [thiru.logging :refer :all]
           [thiru.repl :as repl]
           [thiru.reporting :refer :all]
           [thiru.utils :refer :all]

           [hockeyoracle.app :as app]
           [hockeyoracle.main :refer :all]
           [hockeyoracle.web.server :as server]))
