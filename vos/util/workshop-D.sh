#!/bin/sh
#
# Script to run the Workshop debugger (workshop -D) from within TAE.  Workshop
# is the Sun 5.0 compiler's debugger.  It requires a -D option which TAE cannot
# handle, thus this intermediary script.  The "exec" is required so that the
# open socket connections to TAE are passed to the child.
#
# If "workshop" is not installed or you use a different compiler suite, change
# the DEBUGGER environment variable to the name of your favorite debugger
# ( `which dbx` should always work, note the backquotes).  DEBUGGER is set
# in $V2TOP/vicset1.csh.

exec workshop -D $*

