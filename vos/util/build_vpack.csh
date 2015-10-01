#!/bin/csh
#
# This script builds the packer/unpacker programs
# for the platform on which the script is run.
#
cd $V2UTIL
#
mkdir $VICCPU
make -f vpack.make
