;; ## Summary
;;
;; High-level app details and configuration, used in places like the CLI, web
;; server defaults, build definition, etc.
;;
(ns hockeyoracle.app
  (:require
            [thiru.logging :refer :all]
            [thiru.utils :refer :all]))

(def config-defaults
  "Default configuration values.
 
  This will be used if a user supplied config can't be found."
  {:name "Hockey Oracle"
   :version "0.0.1"
   :description "A management tool for amateur hockey leagues"
   :app-mode :dev
   :log-level :debug
   :web-server-port 8023
   :nrepl-port 8024})

(def config
  "Contains the global app configuration map.
  
  We store it in an atom so it can be live-reloaded."
  (atom
    (let [config-res (load-config "config.edn" config-defaults)]
      (log nil config-res)
      (:data config-res))))

(defn reload-config
  "Reload config from one of the predefined user directories.
  
  * `file-name`
    * If specified, it is will be used as the config file name
    * Otherwise 'config.edn' is used"
  ([] (reload-config "config.edn"))
  ([file-name]
   (reset! config (load-config file-name config-defaults))))
