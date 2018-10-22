#!/bin/csh

# proc to build 'tlgeom' before running tstlgeom1.pdf, which
# requires this (both called from tstlgeom.pdf)

# first, remove any previous version of tlgeom, which may
# have been made on a different system:
unalias rm
rm -f tlgeom
rm -f tlgeom.o
rm -f tlgeom.make

#select core-t
# remove dependence on specific system, instead do:
source $V2TOP/vicset2.csh

vimake tlgeom
make -f tlgeom.make

# remove this step, run it from tstlgeom.pdf instead:
#taetm -s "tstlgeom1"
