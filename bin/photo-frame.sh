#!/bin/bash
f=$1
shift
fname=${f/.*/}
convert $f -bordercolor white -border 10x10 $fname.png
mogrify -bordercolor "#000000" -border 10 $fname.png 
