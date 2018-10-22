#!/bin/csh
# 
# This utility builds all subdirectories files in p1, p2, p3, gui and other subsystems.
# The first argument is the destination directory for the build.  The 
# second argument, if present, is the target to use for the make instead
# of system. The subdirectory file must be in the current directory.
#
foreach icom_file (*)
  if (${#argv} == 2) then
      $V2UTIL/makeapp_nounpack.sys ${icom_file:r} $1 $2
  else
      $V2UTIL/makeapp_nounpack.sys ${icom_file:r} $1
  endif
end

