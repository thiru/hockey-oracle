#!/bin/sh

# Restart already running Hockey Oracle within screen session

screen -S howeb -X stuff '(hockey-oracle.web:stop-server)^M'
screen -S howeb -X stuff '(exit)^M'
screen -S howeb -X stuff 'sbcl --load restart.lisp^M'
