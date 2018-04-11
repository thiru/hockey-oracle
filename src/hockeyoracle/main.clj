;; ## Summary
;;
;; Command-line interface and entry-point into the application.
;;
;; Command-line arguments are parsed and displayed with the help of
;; [clojure.tools.cli](https://github.com/clojure/tools.cli).
;;
(ns hockeyoracle.main
  (:require
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]

            [thiru.logging :refer :all]
            [thiru.repl :as repl]
            [thiru.reporting :refer :all]
            [thiru.utils :refer :all]

            [hockeyoracle.app :as app]
            [hockeyoracle.web.server :as server])
  (:gen-class))

(def cli-commands
  "A map of CLI (sub-)commands with a short description of each."
  {:start "Start web server"
   :version "Print app version"})

(def cli-options
  "A vector of CLI options.
 
  Each item follows the spec of a CLI option as defined by
  `clojure.tools.cli`."
  [["-p" "--port PORT" "Web server listen port"
    :default (:web-server-port @app/config)
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Port must be a number between 0 and 65536"]]

   ["-r" "--nrepl-port PORT" "nREPL server listen port"
    :default (:nrepl-port @app/config)
    :parse-fn #(Integer/parseInt %)]

   ["-l" "--log-level LEVEL"
    (str "Log verbosity level (" (level-names) ")")
    :default (:log-level @app/config)
    :default-desc (name (:log-level @app/config))
    :parse-fn #(first (find levels (keyword %)))
    :validate [#(get levels %)
               (str "Log verbosity level must be one of: " (level-names))]]

   ["-v" "--version" "Show app version"]

   ["-h" "--help" "Show help"]])

(defn usage
  "User-friendly CLI usage description.

  * `options-summary`
    *  A user-friendly summary of CLI options to be injected into the full
       summary string returned
    *  We generate the options summary with `clojure.tools.cli/parsed-opts`"
  [options-summary]
  (->> [(str "hockeyoracle " (:version @app/config))
        ""
        (:description @app/config)
        ""
        "Usage: hockeyoracle [options] command"
        ""
        "Options:"
        options-summary
        ""
        "Commands:"
        (apply str
               (map (fn [kvp] (str "  " (name (key kvp)) " - " (val kvp) "\n"))
                    cli-commands))]
       (string/join \newline)
       (string/trim)))

(defn error-msg
  "Common error message format.
 
  * `errors`
    * A vector of error messages"
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate the given command-line arguments.
  A map is returned of the following form:

  * `:result`
    * A result map indicating successful/error validation
    * This could contain error messages and/or the usage summary
  * `:command`
    * A keyword specifying the command to run the app with
    * See `cli-commands`"
  [args]
  (let [parsed-opts (cli/parse-opts args cli-options)
        {:keys [options arguments errors summary]} parsed-opts]
    (cond
      ;; No arguments given
      (empty? args)
      {:result (r :warning (usage summary))}
      
      ;; Help option was specified
      (:help options)
      {:result (r :info (usage summary))}

      ;; Version option was specified
      (:version options)
      {:result (r :info (:version @app/config))}

      ;; Errors were found while parsing
      errors
      {:result (r :error (error-msg errors))}

      ;; Ensure only one command is given
      (>= (count arguments) 2)
      {:result (r :warning "Only one command at a time is supported")}

      ;; Ensure the given command is valid
      (and (= 1 (count arguments))
           (not-empty (sets/difference
                        (set (map keyword arguments))
                        (set (keys cli-commands)))))
      {:result (r :error (str "Unrecognised command: " (first arguments)))}

      ;; Handle commands
      (and (= 1 (count arguments))
           (get cli-commands (keyword (first arguments))))
      {:result (r :success)
       :command (keyword (first arguments))
       :options options}

      ;; Failed custom validation. Exit with usage summary.
      :else
      {:result (r :error (usage summary))})))

(defn exit
  "Exit app with the given result map, printing the message to standard out."
  [result]
  (if (success? result)
    (println (:message result))
    (log nil result :print-level-prefix false))
  (System/exit
    (cond
      (failed? result) 1
      :else 0)))

(defn start
  "Start the application (nREPL server and web server)."
  ([] (start @app/config))
  ([config]
   (repl/start {:port (:nrepl-port config)})
   (server/start false (:web-server-port config))))

(defn stop
  "Stop the application (nREPL server and web server)."
  []
  (server/stop)
  (repl/stop))

(defn restart
  "Restart the application (nREPL server and web server)."
  []
  (server/restart)
  (repl/restart))

(defn print-version
  "Print out the current version of the app."
  []
  (log :info (:version @app/config) :print-level-prefix false))

(defn -main
  "This function is called when the **app first starts up**.
  
  * `args`
    * A vector of command-line arguments"
  [& args]
  (let [{:keys [result command options]} (validate-args args)]
    (if (or (failed? result) (nil? command))
      (exit result)
      (with-redefs [log-level (:log-level options)]
        (case command
          :start (start options)
          :version (print-version))))))
