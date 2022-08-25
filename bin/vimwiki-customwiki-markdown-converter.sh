# converter for markdown to html using pandoc.
# To be used with vimwiki as personal wiki

# customize vimviki with
# g:vimwiki_customwiki2html=$HOME.'/my/noteboom/bin/vimwiki-customwiki-markdown-converter.sh'
# requires pandoc. brew install pandoc

MARKDOWN=markdown
MKD2HTML=pandoc


FORCE="$1"
SYNTAX="$2"
EXTENSION="$3"
OUTPUTDIR="$4"
INPUT="$5"
CSSFILE="$6"

FORCEFLAG=

[ $FORCE -eq 0 ] || { FORCEFLAG="-f"; };

OUTPUT="$OUTPUTDIR"/$(basename "$INPUT" .$EXTENSION).html

# # Method 1:
# # markdown [-d] [-T] [-V] [-b url-base] [-C prefix] [-F bitmap] [-f flags] [-o file] [-s text] [-t text] [textfile]
#
# URLBASE=http://example.com
# $MARKDOWN -T -b $URLBASE -o $OUTPUT  $INPUT


# Method 2:
# mkd2html [-css file] [-header string] [-footer string] [file]

# $MKD2HTML -css "$CSSFILE" "$INPUT"

OUTPUTTMP=$(dirname "$INPUT")/$(basename "$INPUT" ."$EXTENSION").html
pandoc -o "$OUTPUTTMP" "$INPUT"
mv -f "$OUTPUTTMP" "$OUTPUT"
