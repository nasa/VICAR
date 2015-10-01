$!****************************************************************************
$!
$! Build proc for MIPL module testos
$! VPACK Version 1.9, Monday, December 07, 2009, 16:38:05
$!
$! Execute by entering:		$ @testos
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
$ write sys$output "*** module testos ***"
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
$ write sys$output "Invalid argument given to testos.com file -- ", primary
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
$   if F$SEARCH("testos.imake") .nes. ""
$   then
$      vimake testos
$      purge testos.bld
$   else
$      if F$SEARCH("testos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake testos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @testos.bld "STD"
$   else
$      @testos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create testos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack testos.com -mixed -
	-s testos.c -
	-i testos.imake -
	-t ttestos.f ttestos.imake ttestos.pdf tsttestos.pdf -
	-o testos.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create testos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Fortran-Callable (no C-version needed -- use #if                     */
/************************************************************************/

void FTN_NAME2(testos, TESTOS) ( ios )
  int *ios;	
{
#if VMS_OS
    *ios = 0;       /*  VMS  */
#else
#if UNIX_OS
    *ios = 1;       /*  UNIX  */
#else
    *ios = 2;       /*  other  */
#endif
#endif
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create testos.imake
/* Imake file for VICAR subroutine TESTOS */

#define SUBROUTINE testos
#define MODULE_LIST testos.c 
#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create ttestos.f
C TEST SUBROUTINE testos
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE testos   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL TESTOS(IOS)
      IF (IOS .EQ. 0) CALL XVMESSAGE('THE OS IS VMS',' ')
      IF (IOS .EQ. 1) CALL XVMESSAGE('THE OS IS UNIX',' ')
      IF (IOS .EQ. 2) CALL XVMESSAGE('THE OS IS other',' ')

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create ttestos.imake
/* Imake file for test of VICAR subroutine testos */

#define PROGRAM ttestos

#define MODULE_LIST ttestos.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create ttestos.pdf
! pdf for test pgm for subroutine TESTOS
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tsttestos.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage = "no"
! test for subroutine TESTOS
! ON VMS THIS SHOULD SAY THIS IS VMS, ON UNIX IT SHOULD SAY UNIX:
!  THUS THE TEST LOGS SHOULD BE DIFFERENT ON VMS AND UNIX.
ttestos
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create testos.hlp
1 TESTOS

PURPOSE:   To tell the calling routine whether the operating system executing
           is VMS, UNIX, or other.

Fortran Call:   CALL TESTOS(IOS)
C Call not needed because of #if.

PARAMETERS:

     IOS =  Is a fullword integer.  TESTOS will set it to
              0  for VMS
              1  for UNIX
              2  for other

2 NOTES

DESCRIPTION

This routine allows a FORTRAN program to determine the operating system
being run.

HISTORY

  Original Programmer: S. Pohorsky   1994-3-14
  Current Cog Progr:   S. Pohorsky   1994-3-14
  Ported to UNIX:      S. Pohorsky   1994-3-14

$ Return
$!#############################################################################
