#!/bin/csh
# 
# This utility builds all *.com files in p1, p2, p3 and gui subsystems.
# The first argument is the destination directory for the build.  The 
# second argument, if present, is the target to use for the make instead
# of system. The ".COM" file must be in the current directory.
#
foreach icom_file (*.com)
  if (${#argv} == 2) then
      $V2UTIL/makeapp.sys ${icom_file:r} $1 $2
  else
      $V2UTIL/makeapp.sys ${icom_file:r} $1
  endif
end

