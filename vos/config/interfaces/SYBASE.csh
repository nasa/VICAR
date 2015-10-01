#!/bin/csh

#setenv SYBASE /usr/local/sybase

setenv SYBASE_SYSAM SYSAM-1_0
setenv SYBASE_ASE ASE-12_5
setenv SYBASE_OCS OCS-12_5
setenv SYBASE_FTS EFTS-12_5
setenv SYBASE_JRE ${SYBASE}/shared-1_0/jre1.2.2

if ( $?LM_LICENSE_FILE == "0" ) then
        setenv LM_LICENSE_FILE ""
endif
setenv LM_LICENSE_FILE $SYBASE/$SYBASE_SYSAM/licenses/license.dat:$LM_LICENSE_FILE
if ( $?PATH == "0" ) then
        setenv PATH ""
endif
setenv PATH $SYBASE/$SYBASE_ASE/bin:$SYBASE/$SYBASE_OCS/bin:$SYBASE/CFG-1_0/bin:$PATH

if ( $?LD_LIBRARY_PATH == "0" ) then
        setenv LD_LIBRARY_PATH ""
endif
setenv LD_LIBRARY_PATH $SYBASE/$SYBASE_ASE/lib:/usr/openwin/lib:$SYBASE/$SYBASE_FTS/lib:$SYBASE/$SYBASE_OCS/lib:$SYBASE/SQLRemote/lib:$LD_LIBRARY_PATH
