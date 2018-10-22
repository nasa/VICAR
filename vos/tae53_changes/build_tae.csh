#!/bin/csh
#
# Rebuild everything necessary after the VICAR patches have been installed.
# It is assumed that the unmodified TAE tree is functional, i.e. install it
# independently if necessary.
#
# Make sure the environment variables are set.  $TAE must already be set.
#
source $TAE/bin/csh/taesetup
#
# Rebuild the imake files that have changed.  This rebuilds all of them,
# but it won't hurt.
# We do it twice because the first time sets up the V2_FORCE_32 stuff in
# the makefile and the second time uses it.
#

make Makefiles
make Makefiles

#
# Make sure all relevant object code gets recompiled due to include changes
#
rm $TAETM/*.o
rm $TAESRC/lib/vicar/*.o
rm $TAETAELIB/*.o
#
# run the installation.
# ***********************************************************************
# *									*
# *	Note that the installation will be done with the parameters as	*
# * listed in $TAE/install.opt  If you want different values for these,	*
# * edit install.opt and set the appropriate yes and no flags.  An	*
# * example of this file follows.					*
# *									*
# ***********************************************************************
# example>#
# example># /software/vicar/taetest/dev/tae52/install.opt.
# example>#
# example># Record of choices for most recent TAE installation.
# example># DO NOT DELETE THIS FILE.
# example>#
# example>	WINDOWS=yes
# example>	DEMOS=yes
# example>	ADABIND=no
# example>	CLFORTBIND=yes
#
# If you are not running with Windows, don't do the following InterViews
# build
#
#cd $TAEIV
#setenv CPU $TAEPLAT
#if ($CPU != 'sgi') then
#  make bootstrap
#endif
#make Makefile
#make Makefiles
#make
#make install
#
# end of Interviews build
#
# Now build the rest of TAE
#
cd $TAE
$TAE/installtae.nointer
#
# Don't know why this isn't done in the real procedures...
#
cd $TAEHELPINX
cp ../taefac.msg .
$TAEBIN/taetm "msgbld taefac.msg"
cd $TAE
#
# done
#
