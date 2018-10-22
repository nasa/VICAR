#!/bin/csh
# This script fixes up the fetch from AccuRev.  
# VERY IMPORTANT:  It assumes the current directory is the top level of the
# VICAR tree!
#
# Rename tae changes directory (because "tae" as subsys is so much easier to
# deal with.  If this script is run twice, this will error, but that can be
# ignored.

if (-d $V2TOP/tae) then
  mkdir tae/help/msg
  rm -f tae52_changes
  ln -s tae tae52_changes
  chmod -R +rw tae52_changes

# Copy duplicated files

  cp tae52_changes/help/tm/taefac.msg tae52_changes/help/msg/taefac.msg
endif

# Create $V2UTIL/x86-macosx and mac64-osx directories for 32 & 64-bit Mac Builds.
#if ($VICCPU == "x86-macosx") then
  mkdir util/x86-macosx
#else if ($VICCPU == "mac64-osx") then
  mkdir util/mac64-osx
#endif
