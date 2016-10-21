;;;; Launches website

(ql:quickload :hockey-oracle)
(hockey-oracle:start-server! :port 9090)
