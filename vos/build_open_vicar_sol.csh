#!/bin/csh
#
# Build the Open Source version of VICAR
#
# PREREQUISITE:
#   setenv V2TOP /directory/containing/vicar
#
# V2TOP should point to the directory containing "vicset1.source" among
# other things.  The parent directory should contain the externals (vos-ext*
# directory).  You should avoid upper-case letters in the path if possible.
# If needed you can create a softlink alias to vicar, e.g.:
#
# sudo ln -s /Users/username/vicar /usr/local/vicar
# setenv V2TOP /usr/local/vicar/vos
#

echo "**********************************************************"
echo "*** VICAR OPEN SOURCE BUILD LOG"
echo "**********************************************************"

unalias rm
unalias mv
unalias cp
unset noclobber

# Create vicset1.csh

cd $V2TOP
echo "creating vicset1.csh"
util/process_project_file.csh vicset1.source PROJ_OS > vicset1.csh
source $V2TOP/vicset1.csh
source $V2TOP/vicset2.csh

# Prep the build

echo "util/prep.csh"
util/prep.csh
echo "util/fetch_tae.csh"
util/fetch_tae.csh >& log1

source $V2TOP/vicset1.csh
make -f imakefile.vicar Makefile

echo "**********************************************************"
echo "*** BUILDING TAE"
echo "**********************************************************"

source $V2TOP/vicset1.csh
make -f Makefile.$VICCPU tae >& build_tae_$VICCPU.log
util/setup-indep.csh

# Build first part of VICAR

source $V2TOP/vicset1.csh	
make -f Makefile.$VICCPU opens1 >& build_opens1_$VICCPU.log

echo "**********************************************************"
echo "*** BUILDING JAVA"
echo "**********************************************************"

source $V2TOP/vicset1.csh
util/java_build.csh >& build_java.log

echo "**********************************************************"
echo "*** BUILDING VICAR PART 2"
echo "**********************************************************"

source $V2TOP/vicset1.csh
make -f Makefile.$VICCPU opens2 >& build_opens2_$VICCPU.log


make -f Makefile.$VICCPU tae52 |& tee build_tae52_$VICCPU.log

echo "Done with VICAR build!  Check log for errors."


