#!/bin/csh
#
# Copy externals from the given platform from one place to another.
# The vicset1 env vars are used to determine the current version number
# directories for the external automatically (by stripping the value of
# $V2EXT from the front).  If the env var does not exist, that external
# is skipped.  The source and dest directories are as given; neither
# need match $V2EXT or the env vars.
#
# The platform is specified by the third parameter, NOT using $VICCPU.
# This means you do not need to be running on the target platform.
# It does however mean there could be some difficulties when the
# external env var is defined differently on different platforms.
#
# Note that README.MIPL, if it exists, is also copied over.  If installing
# into the main tree, make sure this will not overwrite anything.
#
# Java externals are NOT copied, not even those with native code.
#
# Usage:
#  copy_platform_externals.csh src-dir dest-dir viccpu-value
#

if ($#argv != 3) then
   head -22 $0
   exit
endif

setenv SRC $1
setenv DST $2
setenv PLAT $3

if ($?GEOTRANSLIB != 0) then
   echo "######"
   echo "# GEOTRANS"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $GEOTRANSLIB
endif

if ($?PDS_LIBRARY != 0) then
   echo "######"
   echo "# PDS"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $PDS_LIBRARY
endif

if ($?CASSISS/$PLAT != 0) then
   echo "######"
   echo "# cassini_iss"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $CASSISS
endif

if ($?PDS_LABEL_LIB != 0) then
   echo "######"
   echo "# PDS Label Lib"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $PDS_LABEL_LIB
endif

if ($?TIFFLIB != 0) then
   echo "######"
   echo "# TIFF"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $TIFFLIB
endif

if ($?SIMBADLIB != 0) then
   echo "######"
   echo "# SIMBAD"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $SIMBADLIB
endif

# MATRACOMP skipped because it's prop, not ext

if ($?PVM_ROOT != 0) then
   echo "######"
   echo "# PVM"
   echo "######"
printenv PVM_ROOT
   setenv VER `echo $PVM_ROOT | sed -e s%$V2EXT%% -e s%$VICCPU%%`
   echo $VER
   if (-d $SRC/$VER/lib/$PLAT) then
      pushd $SRC/$VER/
      mkdir -p $DST/$VER/
      tar cf - M* R* conf console doc examples gexamples hoster hostfile include libfpvm man misc pvmgs rm shmd src tasker tracer xdr xep | (cd $DST/$VER; tar xBfp -)
      cd lib
      mkdir -p $DST/$VER/lib/$PLAT
      cp * $DST/$VER/lib/
      cd $PLAT
      tar cf - . | (cd $DST/$VER/lib/$PLAT; tar xBfp -)
      cd ../../bin/$PLAT
      mkdir -p $DST/$VER/bin/$PLAT
      tar cf - . | (cd $DST/$VER/bin/$PLAT; tar xBfp -)
      popd
   endif
endif

if ($?PGPLOT_DIR != 0) then
   echo "######"
   echo "# PGPLOT"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $PGPLOT_DIR
endif

if ($?SPICEROOT != 0) then
   echo "######"
   echo "# SPICE"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $SPICEROOT/c/
   copy_one_platform_external.csh $SRC $DST $PLAT $SPICEROOT/fortran/
endif

if ($?ACE_ROOT != 0) then
   echo "######"
   echo "# ACE Wrappers"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $ACE_ROOT
endif

if ($?XERCES_C_ROOT != 0) then
   echo "######"
   echo "# Xerces-C++"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $XERCES_C_ROOT
endif

if ($?MATH77LIB != 0) then
   echo "######"
   echo "# Math77"
   echo "######"
   setenv VER `echo $MATH77LIB | sed -e s%$V2EXT%% -e s%$VICCPU%% -e s%lib/%%`
   echo $VER
   if (-d $SRC/$VER/lib/$PLAT) then
      pushd $SRC/$VER/
      mkdir -p $DST/$VER/
      tar cf - R* a* demo docs.ps docs.tex fortran m* sftran test | (cd $DST/$VER; tar xBfp -)
      cd lib/$PLAT
      mkdir -p $DST/$VER/lib/$PLAT
      tar cf - . | (cd $DST/$VER/lib/$PLAT; tar xBfp -)
      popd
   endif
   if (-f $SRC/$VER/README.mipl) then
      cp $SRC/$VER/README.mipl $DST/$VER
   endif
endif

if ($?MPILIB != 0) then
   echo "######"
   echo "# MPI"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $MPILIB
endif

if ($?HDFLIB != 0) then
   echo "######"
   echo "# HDF"
   echo "######"
   setenv EXT `echo $HDFLIB | sed -e s%$VICCPU/.\*%%`
   copy_one_platform_external.csh $SRC $DST $PLAT $EXT
endif

if ($?HDFEOSLIB != 0) then
   echo "######"
   echo "# HDF-EOS"
   echo "######"
   setenv EXT `echo $HDFEOSLIB | sed -e s%$VICCPU/.\*%%`
   copy_one_platform_external.csh $SRC $DST $PLAT $EXT
endif

if ($?MERIDDLIB != 0) then
   echo "######"
   echo "# MER-IDD"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $MERIDDLIB
endif

if ($?SIRTF_PHE_LIB != 0) then
   echo "######"
   echo "# SIRTF-PHE"
   echo "######"
   copy_one_platform_external.csh $SRC $DST $PLAT $SIRTF_PHE_LIB
endif

echo "######"
echo "# DONE"
echo "######"

