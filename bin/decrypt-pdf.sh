#!/bin/bash
if [ "x$PWD" -eq "x" ]; then
    echo -n Password:
    read -s PWD
    echo
fi
qpdf --password=$PWD --decrypt $*
