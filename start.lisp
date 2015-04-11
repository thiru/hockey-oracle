;;;; Launches website with swank

(require :swank)

; Should I use ":style :spawn"?
(swank:create-server :port 4005 :dont-close t)

(ql:quickload :hockey-oracle)
