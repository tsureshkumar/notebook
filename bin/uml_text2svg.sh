#!/usr/bin/env bash

# cpan
# force install UML::Sequence

TMP=$(mktemp)

genericseq.pl UML::Sequence::SimpleSeq $1 > $TMP 2> /dev/null
seq2svg.pl $TMP > $(basename $1).svg
echo written svg file $(basename $1).svg

rm $TMP
