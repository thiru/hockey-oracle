#!/bin/sh

# Starts Hockey Oracle in a screen session with the following properties:
# * detached by default
# * name of session is "howeb"
# * session will not die even if sbcl process ends

screen -dmS howeb sh -c 'sbcl --load start.lisp; exec zsh'
