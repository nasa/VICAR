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

# Create $V2UTIL/x86-macosx directory for Mac Build.
if ($VICCPU == "x86-macosx") then
  mkdir util/x86-macosx
endif

#
# Make various build scripts executable
#

if (-d $V2TOP/tae) then
  chmod +x util/fetch_tae53.csh
  chmod +x util/fetch_tae52.csh
  chmod +x util/fetch_tae.csh
endif
#
chmod +x util/remove_element.csh
chmod +x util/fetch_binaries.csh
chmod +x util/bldcomfiles.csh
chmod +x util/setup-indep.csh
chmod +x util/java_build.csh
chmod +x util/java_build_dir.csh
chmod +x util/java_build_javadoc.csh
chmod +x util/java_fetch_externals.csh
chmod +x util/copy_one_platform_external.csh
chmod +x util/copy_platform_externals.csh
chmod +x util/chklog_unix.perl

# Give all of the files read access.
#
chmod -R ugo+r $V2TOP

