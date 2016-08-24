;;;; Launches website

(ql:quickload :hockey-oracle)
(hockey-oracle.web:start-server! :port 9090)
