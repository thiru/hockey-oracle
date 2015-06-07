#!/bin/bash

screen -d -m -S howeb sbcl --load start.lisp

screen -list
