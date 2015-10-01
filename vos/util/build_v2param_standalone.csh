#!/bin/csh
#    N  O  T  E
# This builds a STANDALONE VERSION of rtl/v2param.com, for use ONLY when
# the program is needed but the RTL is not available.  It is identical
# code-wise to the RTL version, with the exception of including
# v2param_subs in this .com file instead of getting them from the
# shell_vicar subsystem, and a few include file differences.  If the
# RTL (really, shell_vicar) is being used, the v2param_subs routines
# are referenced in there too; thus the nominal version shares the same source.

# Check to see if the RTL is available.  Exit if so.

if (-d $V2TOP/rtl) then
   exit
endif

# Build the standalone v2param

source $V2TOP/vicset2.csh
cd $VICCPU
vunpack ../v2param_standalone.com -q system
cp ../xvmaininc.h .
vimake v2param
make -f v2param.make system
rm -f xvmaininc.h

