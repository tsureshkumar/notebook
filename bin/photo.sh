#!/usr/bin/env /bin/bash
#usage $0 <command> <infile> [outfile]

# add shadow
# source: http://blog.bemoko.com/2009/07/01/add-shadow-and-border-to-images-with-imagemagick/

function merge_shadow () {
  in=$file
  out=$outfile
  # this just creats a shadow without border
  #convert $in \( +clone -background black -shadow 60x5+10+10 \) \
  #  +swap -background white -layers merge +repage $out
  #convert $in -bordercolor white -border 13 \( +clone -background black -shadow 80x3+2+2 \)  \
  convert $in -bordercolor white -border 15 \( +clone -background black -shadow 80x3+4+4 \)  \
   +swap -background white -layers merge +repage $outfile
}

function double_border () {
   convert $in -bordercolor white -border 10x10 $outfile.png
   mogrify -bordercolor "#000000" -border 10 $outfile.png
}

command=$1
file=$2
outfile=$3
if [ -z $outfile ] ; then
   outfile=${file%.*}-$command.${file#*.}
fi

case $command in
   shadow*)
      merge_shadow
      ;;
   double-border*)
      double_border
      ;;
esac
echo "Converted file : $out"
