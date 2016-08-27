#!/bin/sh

# Starts the Hockey Oracle web server in a screen session:
# * detached by default
# * name of session is "howeb"
# * session will not die even if sbcl process ends

screen -dmS howeb sh -c 'sbcl --load start.lisp; exec zsh'
