;;;; Launches website with swank

(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)

(ql:quickload :hockey-oracle)
(hockey-oracle:start-server! :port 9091)
