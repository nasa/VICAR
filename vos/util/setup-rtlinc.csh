#!/bin/csh
#Makes sure the Unix-style names are linked to the VMS-style names that
# come across from the VAX and that the Unix-specific versions are used.
#
cd $V2TOP/rtl/inc
ln -fs xviodefs_unix.h xviodefs.h
ln -fs vicmain_c.h vicmain_c
ln -fs vicmain_for_unix.fin vicmain_for.fin
cd $V2TOP
