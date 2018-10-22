#!/bin/csh
#
# Not to be used directly.  See copy_platform_external.csh.
#
setenv SRC $1
setenv DST $2
setenv PLAT $3
setenv EXT $4

setenv VER `echo $EXT | sed -e s%$V2EXT%% -e s%$VICCPU%%`
echo $VER
if (-d $SRC/$VER/$PLAT) then
   pushd $SRC/$VER/$PLAT
   mkdir -p $DST/$VER/$PLAT
   tar cpf - . | (cd $DST/$VER/$PLAT; tar xBfp -)
   popd
endif
if (-d $SRC/$VER/src) then
   pushd $SRC/$VER/src
   mkdir -p $DST/$VER/src
   tar cpf - . | (cd $DST/$VER/src; tar xBfp -)
   popd
endif
if (-f $SRC/$VER/README.mipl) then
   cp -p $SRC/$VER/README.mipl $DST/$VER
endif

exit

