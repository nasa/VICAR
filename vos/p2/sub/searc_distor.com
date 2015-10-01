$!****************************************************************************
$!
$! Build proc for MIPL module searc_distor
$! VPACK Version 1.9, Monday, December 07, 2009, 16:35:43
$!
$! Execute by entering:		$ @searc_distor
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module searc_distor ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to searc_distor.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("searc_distor.imake") .nes. ""
$   then
$      vimake searc_distor
$      purge searc_distor.bld
$   else
$      if F$SEARCH("searc_distor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake searc_distor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @searc_distor.bld "STD"
$   else
$      @searc_distor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create searc_distor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack searc_distor.com -mixed -
	-s searc_distor.f zsearc_distor.c -
	-i searc_distor.imake -
	-t tsearc_distor.f tsearc_distor.imake tsearc_distor.pdf -
	   tstsearc_distor.pdf -
	-o searc_distor.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create searc_distor.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE SEARC_DISTOR( UNIT, IND)

c  This routine searches the entire label of a VICAR image in order to
c  determine whether it is "Object space" (perspective projection) or
c  "Image space" (contains camera-specific distortion).  Both cases
c  imply that no map labels are present, so it should be called only
c  after verifying that the image does not contain map labels (using
c  MP_LABEL_READ or SEARCV2).  Note that SEARCV2 (on the unported
c  system) performs the same function, but the current version does
c  not work for certain kinds of (very old) VICAR labels.

c  Currently, the criteria for determining that an unlabelled image
c  is Obect space are that it has been processed by a GEOM task or by
c  FARENC.  Thus, all that SEARC_DISTOR does is to search the entire
c  label buffer for these strings.  (This code is borrowed from the
c  old version of SEARCV2.)

c  25jul94 -lwk- initial version

c  Arguments:

c  UNIT (input, integer) = unit number assigned by XVUNIT

c  IND (output, integer) = indicator flag:  
c	IND=0 means Image space
c	IND=1 means Object space
c	IND=-1: label exceeds buffer size
c	IND=-2: error reading label

      INTEGER UNIT
      INTEGER TEMP(6000),BUFSIZE
      CHARACTER*24000 WORK
      equivalence (temp,work)

      CALL ZIA(TEMP,6000)
      BUFSIZE=0
      CALL XLGETLABEL(UNIT,WORK,BUFSIZE,ISTAT)
      IF (BUFSIZE.GT.24000) THEN
	CALL PRNT(4,1,BUFSIZE,'SEARCV2: BYTES IN LABEL = .')
	ind = -1
	return
      ENDIF
      CALL XLGETLABEL(UNIT,WORK,BUFSIZE,ISTAT)
      CALL CHKSTAT( ISTAT,' ERROR IN XLGETLABEL,SEARC_DISTOR', 0, i, 0)
      if (istat.lt.0) then
	ind = -2
	return
      endif

      IF (INDEX(WORK,'GEOM').NE.0 .OR. INDEX(WORK,'FARENC').NE.0) THEN
	ind = 1		! object space
      ELSE
	ind = 0		! image space
      ENDIF

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zsearc_distor.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of searc_distor */
/************************************************************************/

void zsearc_distor(int unit, int *ind)
{
FTN_NAME2_(searc_distor, SEARC_DISTOR) (&unit,ind);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create searc_distor.imake
/* Imake file for VICAR subroutine searc_distor */

#define SUBROUTINE searc_distor

#define MODULE_LIST searc_distor.f zsearc_distor.c

#define P2_SUBLIB

#define LIB_LOCAL
#if UNIX_OS
#define LOCAL_LIBRARY sublib.a
#else
#define LOCAL_LIBRARY sublib.olb
#endif

#define USES_FORTRAN
#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tsearc_distor.f
      INCLUDE 'VICMAIN_FOR'
C       PROGRAM TSEARC_DISTOR TO TEST SEARC_DISTOR
      SUBROUTINE MAIN44
      INTEGER*4 UNIT

      CALL XVUNIT( unit,'INP',1,ISTAT,' ')
      CALL CHKSTAT( ISTAT,' ERROR IN XVUNIT, ISTAT=', 1, ISTAT, 1)
      CALL XVOPEN(unit,ISTAT,' ')
      CALL CHKSTAT( ISTAT,' ERROR IN XVOPEN, ISTAT=', 1, ISTAT, 1)
      CALL SEARC_DISTOR(unit,ind)
      if (ind.eq.0) then
	call xvmessage(' input image is Image Space',' ')
      elseif (ind.eq.1) then
	call xvmessage(' input image is Object Space',' ')
      else
	call xvmessage(' ** error in searc_distor! **',' ')
      endif

      return
      end
$!-----------------------------------------------------------------------------
$ create tsearc_distor.imake
#define  PROGRAM   tsearc_distor

#define MODULE_LIST tsearc_distor.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN

#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tsearc_distor.pdf
PROCESS
PARM INP TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tstsearc_distor.pdf
procedure help=*
refgbl $echo
refgbl $syschar
refgbl $autousage
local geo      type=string            !...F1636832.geo
local raw      type=string            !...F1636832.raw
body
let  _onfail="continue"
let  $echo="yes"
let  $autousage="none"

if ($syschar(1) = "UNIX")
  let geo = "/project/test_work/testdata/mipl/vgr/f1636832.geo"
  let raw = "/project/test_work/testdata/mipl/vgr/f1636832.raw"
else ! VAX assumed
  let geo = "wms_test_work:[testdata.mipl.vgr]f1636832.geo"
  let raw = "wms_test_work:[testdata.mipl.vgr]f1636832.raw"
end-if

write "Test of module SEARCV_DISTOR"

write " this image should be Object Space:"
tsearc_distor @geo
label-list @geo

write " this image should be Image Space:"
tsearc_distor @raw
label-list @raw

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create searc_distor.hlp
1 SEARC_DISTOR

 This routine searches the entire label of a VICAR image in order to
 determine whether it is "Object space" (perspective projection) or
 "Image space" (contains camera-specific distortion).  Both cases
 imply that no map labels are present, so it should be called only
 after verifying that the image does not contain map labels (using
 MP_LABEL_READ or SEARCV2).  Note that SEARCV2 (on the unported
 system) performs the same function, but the current version does
 not work for certain kinds of (very old) VICAR labels.

2 Calling Sequence

 CALL SEARC_DISTOR( UNIT, IND)

 UNIT (input, integer) = unit number assigned by XVUNIT

 IND (output, integer) = indicator flag:
      IND=0 means Image space
      IND=1 means Object space
      IND=-1: label exceeds buffer size
      IND=-2: error reading label
 
2 History

  Original Programmer: L.W.Kamp,  25 JULY 1994
  Current Cognizant Programmer: L.W.Kamp
  Source Language: FORTRAN
  Revisions:
   01 Mar 03  GMY  Replace TEMP by WORK in call to XLGETLABEL (unix error)

2 Operation

 Currently, the criteria for determining that an unlabelled image
 is Obect space are that it has been processed by a GEOM task or by
 FARENC.  Thus, all that SEARC_DISTOR does is to search the entire
 label buffer for these strings.  
$ Return
$!#############################################################################
