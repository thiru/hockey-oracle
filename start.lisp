;;;; Launches website with swank

(ql:quickload :swank)

; Should I use ":style :spawn"?
(swank:create-server :port 4005 :dont-close t)

(ql:quickload :hockey-oracle)

(hockey-oracle.web:start-server! :port 9090)
