#!/bin/csh
# This utility will run vids.imake through vimake, then execute the generated
# makefile.  It must be in csh since vimake will not (yet) run in sh.

source $V2TOP/vicset2.csh		# get alias for vimake
cp vids.imake $VICCPU
cd $VICCPU
vimake vids				# uses vids.imake
make -f vids.make

