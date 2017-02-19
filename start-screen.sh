#!/bin/sh

# Starts the Hockey Oracle web server in a screen session:
# * detached by default
# * name of session is "howeb"
# * session will not die even if lisp process ends

screen -dmS howeb sh -c 'ros run --load start.lisp; exec zsh'
