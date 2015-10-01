#!/bin/csh
# This script creates the binary data directories and populates them from
# the canonical storage area (/project/cm4/binaries).
# VERY IMPORTANT: It assumes the current directory is the top level of the
# VICAR tree!

mkdir data
mkdir data/gui
mkdir data/isis
cp /project/cm/binaries/gui/* data/gui
cp /project/cm/binaries/isis/* data/isis
