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

# Create "external" softlink

cd $V2TOP
cd ..
ln -s vos-ext-$VICCPU external

# Create vicset1 file
cd $V2TOP
util/process_project_file.csh vicset1.source PROJ_OS >vicset1.csh
source $V2TOP/vicset1.csh
source $V2TOP/vicset2.csh

# Prep the build, build imake on mac

util/prep.csh
util/fetch_tae.csh |& tee fetch_tae.log
echo "*** Ignore errors related to tae52 above"

if ("$VICCPU" == "x86-macosx") then
  cd util/imake-x86-macosx
  make -f Makefile
  cd ..
  mkdir x86-macosx
  cp imake-x86-macosx/imake x86-macosx/
  cd $V2TOP
  source $V2TOP/vicset1.csh
endif

make -f imakefile.vicar Makefile
source $V2TOP/vicset1.csh

echo "**********************************************************"
echo "*** BUILDING TAE"
echo "**********************************************************"

make -f Makefile.$VICCPU tae53 |& tee build_tae53_$VICCPU.log

echo "**********************************************************"
echo "*** BUILDING VICAR PART 1"
echo "**********************************************************"

util/setup-indep.csh
source $V2TOP/vicset1.csh

# Build first part of VICAR

make -f Makefile.$VICCPU opens1 |& tee build_opens1_$VICCPU.log

echo "**********************************************************"
echo "*** BUILDING JAVA"
echo "**********************************************************"

util/java_build.csh |& tee build_java .log

echo "**********************************************************"
echo "*** BUILDING VICAR PART 2"
echo "**********************************************************"

make -f Makefile.$VICCPU opens2 |& tee build_opens2_$VICCPU.log

echo "Done with VICAR build!  Check log for errors."


