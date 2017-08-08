#!/bin/sh

# sometimes the default passfile I supply doesn't work
# in that case: rm slime.passfile ; touch slime.passfile ; sh get-slime.sh
# and enter 'anonymous' as the password
PASSFILE=slime.passfile
ROOT=:pserver:anonymous@common-lisp.net:/project/slime/cvsroot

CVSROOT=$ROOT CVS_PASSFILE=$PASSFILE cvs co slime

