#!/bin/csh
#
#  This script builds all of the p2/programs which
#  use the XRT library
#
cd $P2SOURCE
$V2UTIL/makeapp.sys ccdnoise $R2LIB
$V2UTIL/makeapp.sys ccdrecip $R2LIB
$V2UTIL/makeapp.sys ccdslope $R2LIB
$V2UTIL/makeapp.sys mosplot $R2LIB
$V2UTIL/makeapp.sys otf1 $R2LIB
$V2UTIL/makeapp.sys plot3d $R2LIB
$V2UTIL/makeapp.sys plotint $R2LIB
$V2UTIL/makeapp.sys pltgraf $R2LIB
$V2UTIL/makeapp.sys power $R2LIB
$V2UTIL/makeapp.sys qplot2 $R2LIB
$V2UTIL/makeapp.sys statplt $R2LIB
$V2UTIL/makeapp.sys tieplot $R2LIB
cd $V2TOP
