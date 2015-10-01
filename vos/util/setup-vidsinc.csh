#!/bin/csh
#
# vids include
#
cd $V2TOP/vids/source
# OS X can't handle filenames differing only in case... so we copy instead
# of softlink.  That way, no matter which one it picks up, it'll be fine
# (otherwise a tar transfer can end up with the file symlinked to itself).
# ln -fs vidsdefs.h VIDSdefs.h
cp vidsdefs.h VIDSdefs.h
cd $V2TOP


