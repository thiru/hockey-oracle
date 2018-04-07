;; ## Boot Build Definition

;; See https://github.com/boot-clj/boot for details on using Boot.

;; ### Dependencies

;; Library dependencies are defined in it's own var since it'll be used in at
;; least two places. One for defining the Boot environment, and for dynamically
;; generating a (Leiningen compatible) project.clj file (see below).
;;
(def deps
  "Clojure library dependencies used by this project."
  '[;; #### Core Domain

    ;; Clojure on JVM:
    [org.clojure/clojure "1.9.0"]

    ;; Date/time library:
    [clojure.java-time "0.3.1"]

    ;; Database access:
    [org.clojure/java.jdbc "0.7.5"]
    [org.postgresql/postgresql "42.2.2"]

    ;; #### Web Server

    ;; Web server abstraction based on middleware:
    [ring/ring-core "1.6.3"]
    ;; Development middleware (e.g. middleware to auto-reload handlers):
    [ring/ring-devel "1.6.3"]
    ;; In-process web server:
    [ring/ring-jetty-adapter "1.6.3"]
    ;; Sensible middleware defaults for Ring:
    [ring/ring-defaults "0.3.1"]
    ;; Simpler HTTP response handling (replaces `ring.util.response`):
    [metosin/ring-http-response "0.9.0"]
    ;; Routing library for Ring
    [compojure "1.6.0"]
    ;; Generate HTML from clojure data structures:
    [hiccup "1.0.5"]
    ;; Better exception reporting middleware:
    [prone "1.1.4"]
    ;; Security library:
    [buddy "2.0.0"]

    ;; #### Logging & Debugging

    ;; Tracing tools during development:
    [org.clojure/tools.trace "0.7.9"]

    ;; The following libraries revolve around SLF4J (Simple Logging Facade for
    ;; Java). SLF4J serves as a simple facade or abstraction for various
    ;; logging frameworks (e.g. log4j, java.util.logging, logback, etc.),
    ;; allowing the user to plug in the desired logging framework at
    ;; *deployment* time.
    ;;
    ;; These are needed in order to get logs from Jetty.

    ;; SLF4J binding for Jakarta Commons Logging:
    [org.slf4j/jcl-over-slf4j "1.7.25"]
    ;; SLF4J binding for java.util.logging:
    [org.slf4j/jul-to-slf4j "1.7.25"]
    ;; SLF4J binding for log4j:
    [org.slf4j/log4j-over-slf4j "1.7.25"]
    ;; SLF4J native binding:
    [ch.qos.logback/logback-classic "1.2.3" :exclusions [org.slf4j/slf4j-api]]

    ;; #### Tooling

    ;; Clojure *network* REPL server/client:
    [org.clojure/tools.nrepl "0.2.13"]
    ;; nREPL middleware for CIDER. This is used by Vim's Fireplace plugin:
    [cider/cider-nrepl "0.16.0"]
    ;; Command-line interface helper:
    [org.clojure/tools.cli "0.3.5"]
    ;; ANSI colours for terminal:
    [clansi "1.0.0"]
    ;; Literate programming-ish documentation
    [lein-marginalia "0.9.1"]])

;; ### Boot Environment
;;
(set-env!
  :source-paths #{"src" "resources"}
  :dependencies deps)

;; Require namespaces needed before calling `task-options!`
(require '[hockeyoracle.app :as app]
         '[clojure.string :as string]
         '[clojure.tools.trace :as t])

;; Define project metadata, etc.
;;
(task-options!
  pom {:project 'hockeyoracle
       :version (:version @app/config)
       :description (:description @app/config)}
  aot {:namespace '#{hockeyoracle.main}}
  jar {:main 'hockeyoracle.main :file "hockeyoracle.jar"}
  sift {:include #{#"\.jar$"}})

;; Require namespaces that'll be used in the REPL and needed by some of the
;; following Boot tasks
(require
         '[clojure.java.shell :as shell]
         '[clojure.pprint :refer :all]
         '[java-time :as jt]
         '[thiru.logging :refer :all]
         '[thiru.reporting :refer :all]
         '[thiru.utils :refer :all]
         '[hockeyoracle.main :as cli]
         '[hockeyoracle.core.db :as db]
         '[hockeyoracle.web.server :as server])

;; ### Boot Tasks

(deftask docs
  "Generate (Literate Programming) documentation using Marginalia.
  The existing Boot plugin for Marginalia doesn't seem to work so what we're
  doing here is dynamically generating a Leiningen project.clj file which the
  original Marginalia plugin then uses (via `lein marg`)."
  []
  (log :info "Creating Leiningen project.clj...")
  (spit "project.clj"
        (format "(defproject hockeyoracle \"%s\"
                :description \"%s\"
                :dependencies %s
                :plugins [[lein-marginalia \"0.9.1\"]]
                :main ^:skip-aot hockeyoracle.main
                :target-path \"target/%%s\"
                :profiles {:uberjar {:aot :all}})\n"
                (:version @app/config)
                (:description @app/config)
                deps))
  (let [cmd "lein marg --file index.html"]
    (log :info (str "Running '" cmd "'..."))
    (shell/sh "sh" "-c" cmd)
    (log :success "Doc generation complete!")))

(deftask run
  "Run the project with default settings."
  []
  (comp
    (with-pass-thru _
      (cli/-main))))

(deftask build
  "Build project and generate stand-alone jar."
  []
  (comp
    (aot)
    (pom)
    (uber)
    (jar)
    (sift)
    (target)))
