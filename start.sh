#!/bin/bash

screen -d -m -S lispweb sbcl --load start.lisp

screen -list
