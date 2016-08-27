#!/bin/sh

# Stops the Hockey Oracle web server

screen -S howeb -X stuff '(hockey-oracle.web:stop-server)^M'
screen -S howeb -X stuff '(exit)^M'
screen -S howeb -X stuff 'exit^M'
