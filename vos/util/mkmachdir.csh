#!/bin/csh
#
# Create the machine-dependent object code and executable directories for
# the current machine.  $V2TOP and $VICCPU must already be defined.
#
mkdir -m go-w  $V2TOP/rtl/source/$VICCPU
mkdir -m go-w -p $V2TOP/olb/$VICCPU
mkdir -m go-w -p $V2TOP/lib/$VICCPU
mkdir -m go-w -p $V2TOP/util/$VICCPU
if (-d $V2TOP/cas) mkdir -m go-w -p $V2TOP/cas/lib/$VICCPU
if (-d $V2TOP/gui) mkdir -m go-w -p $V2TOP/gui/lib/$VICCPU
if (-d $V2TOP/mars) mkdir -m go-w -p $V2TOP/mars/lib/$VICCPU
if (-d $V2TOP/mdms) mkdir -m go-w -p $V2TOP/mdms/lib/$VICCPU
if (-d $V2TOP/mdms) mkdir -m go-w -p $V2TOP/mdms/olb/$VICCPU
mkdir -m go-w  $V2TOP/mdms/source/dbview/$VICCPU
mkdir -m go-w  $V2TOP/mdms/source/dbq/$VICCPU
mkdir -m go-w  $V2TOP/mdms/source/fei/$VICCPU
mkdir -m go-w  $V2TOP/mdms/source/glblib/$VICCPU
mkdir -m go-w  $V2TOP/mdms/source/lcllib/$VICCPU
if (-d $V2TOP/p1) mkdir -m go-w -p $V2TOP/p1/lib/$VICCPU
if (-d $V2TOP/p2) mkdir -m go-w -p $V2TOP/p2/lib/$VICCPU
mkdir -m go-w  $V2TOP/p2/inc/$VICCPU
if (-d $V2TOP/p3) mkdir -m go-w -p $V2TOP/p3/lib/$VICCPU
if (-d $V2TOP/tlm) mkdir -m go-w -p $V2TOP/tlm//lib/$VICCPU
mkdir -m go-w  $V2TOP/tlm/inc/$VICCPU
if (-d $V2TOP/uplinktour) mkdir -m go-w -p $V2TOP/uplinktour/lib/$VICCPU
if (-d $V2TOP/vrdi) mkdir -m go-w -p $V2TOP/vrdi/lib/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/adage/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/common/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/dummy/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/ip85hi/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/ip85lo/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/ivas/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/jup/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/ramtek/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/tek/$VICCPU
mkdir -m go-w  $V2TOP/vrdi/source/xdisplay/$VICCPU
if (-d $V2TOP/vids) mkdir -m go-w -p $V2TOP/vids/lib/$VICCPU
mkdir -m go-w  $V2TOP/vids/source/$VICCPU
mkdir -m go-w -p $V2TOP/html/javadoc
if (-d $V2TOP/neat) mkdir -m go-w -p $V2TOP/neat/lib/$VICCPU
if (-d $V2TOP/sirtf) mkdir -m go-w -p $V2TOP/sirtf/lib/$VICCPU
if (-d $V2TOP/ssv) mkdir -m go-w -p $V2TOP/ssv/lib/$VICCPU
if (-d $V2TOP/div) mkdir -m go-w -p $V2TOP/div/lib/$VICCPU
if (-d $V2TOP/fei5) mkdir -m go-w -p $V2TOP/fei5/lib/$VICCPU
if (-d $V2TOP/js) mkdir -m go-w -p $V2TOP/js/lib
