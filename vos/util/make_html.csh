#!/bin/csh
# Creates the html for one directory full of PDF's.  Intended to be called
# by build scripts only, and is just a convenience wrapper around pdf2html.
# Since html needs to be generated only once, we pick sun-solr as the most
# common machine, and build the html only on that platform.  Since this script
# should be used only by the builds, the destination directory is automatically
# set to $V2TOP/html/vichelp.
#
# arg 1 = the directory to find PDF's in (e.g. $R2LIB)
# arg 2 = the name of the index file to create (e.g. index_p2.html)

if ("$VICCPU" == "x86-linux") then
   echo "*********************"
   echo "* Building HTML files for $1"
   echo "*********************"
   mkdir -p $V2TOP/html/vichelp
   cd $V2TOP/html/vichelp
   $V2UTIL/pdf2html.perl $1 $2
else
   echo "HTML files are built on linux 32-bit only"
endif

