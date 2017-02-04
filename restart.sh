#!/bin/sh

# Restart already running Hockey Oracle within screen session

screen -S howeb -X stuff '(hockey-oracle:stop-server)^M'
screen -S howeb -X stuff '(exit)^M'
screen -S howeb -X stuff 'ros run --load restart.lisp^M'
