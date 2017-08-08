#!/bin/sh

cd f:/ozone

SWANKSTART="(swank:start-server (symbol-name '|$1|))"
#echo $SWANKSTART
c:/Program\ Files/acl70/mlisp.exe +B +cm -L c:/msys/1.0/home/mrd/slime/swank-loader.lisp -e "$SWANKSTART"

