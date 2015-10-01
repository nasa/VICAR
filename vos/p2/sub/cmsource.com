$!****************************************************************************
$!
$! Build proc for MIPL module cmsource
$! VPACK Version 1.9, Monday, December 07, 2009, 16:08:52
$!
$! Execute by entering:		$ @cmsource
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
$ write sys$output "*** module cmsource ***"
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
$ write sys$output "Invalid argument given to cmsource.com file -- ", primary
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
$   if F$SEARCH("cmsource.imake") .nes. ""
$   then
$      vimake cmsource
$      purge cmsource.bld
$   else
$      if F$SEARCH("cmsource.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cmsource
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cmsource.bld "STD"
$   else
$      @cmsource.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cmsource.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cmsource.com -mixed -
	-s cmsource.f zcmsource.c -
	-i cmsource.imake -
	-t tstcmsource.pdf -
	-o cmsource.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cmsource.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Print the source of the C matrix in following format:
C       CKNAME=FARE  SPKID=NO11  PROGRAM=IBISUPD  GMY059  MIPL  09/22/96
C
      SUBROUTINE CMSOURCE(SBUF,isource)
      INTEGER*4 SBUF(200)	!getspice95 buffer (input)
      INTEGER ISOURCE

      CHARACTER*4 CKID,SPKID,INSTITUTE,CKNAME
      CHARACTER*6 USER,PROGRAM
      CHARACTER*8 DATE
      LOGICAL*1 LBUF(4)
      CHARACTER*80 MSG

      CKNAME='NAIF'     ! default to NAIF
      ISOURCE=7
      CALL MVLC(SBUF(172),ckid,4)
      IF (CKID.EQ.'M901') THEN
        CKNAME='AMOS'
        ISOURCE=6
      END IF
      IF (CKID.EQ.'M902') THEN
        CKNAME='NEAR'
        ISOURCE=5
      END IF
      IF (CKID.EQ.'M903') THEN
        CKNAME='NAV2'
        ISOURCE=4
      END IF
      IF (CKID.EQ.'M904') THEN
        CKNAME='FARE'
        ISOURCE=2
      END IF
      IF (CKID.EQ.'M905') THEN
        CKNAME='NAV'
        ISOURCE=3
      END IF
      IF (CKID.EQ.'M906') THEN
        CKNAME='DAVI'
        ISOURCE=1
      END IF
      CALL MVLC(SBUF(14),spkid,4)
      CALL MVLC(SBUF(174),program,6)
      CALL MVLC(SBUF(177),user,6)
      CALL MVLC(SBUF(189),institute,4)
      CALL MVE(1,4,SBUF(170),lbuf,1,1)
      CALL MVLC(LBUF,date(1:2),2)		!Insert month
      DATE(3:3) = '/'
      CALL MVLC(LBUF(3),date(4:5),2)		!Insert day
      DATE(6:6) = '/'
      CALL MVE(1,4,SBUF(169),lbuf,1,1)		!Insert year
      CALL MVLC(LBUF(3),date(7:8),2)
      WRITE(MSG,110) CKNAME,SPKID,PROGRAM,USER,INSTITUTE,DATE
  110 FORMAT('CKNAME=',A4,'  SPKID=',A4,'  PROGRAM=',A6,2X,A6,2X,
     &  A4,2X,A8)
      CALL XVMESSAGE(MSG,' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zcmsource.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcmsource - determine the source of the C-matrix...*/
/************************************************************************/


void zcmsource(sedr,isource)
void *sedr;			/* buffer of SEDR/SPICE data from GETSPICE*/
int  *isource;			/* integer code of C-matrix source      */

{
FTN_NAME2(cmsource, CMSOURCE) ( sedr, isource);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cmsource.imake
/* Imake file for VICAR subroutine CMSOURCE */

#define SUBROUTINE cmsource

#define MODULE_LIST cmsource.f zcmsource.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tstcmsource.pdf
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! Test script for the subroutine CMSOURCE
! Voyager test - CMSOURCE prints the following text:
!      CKNAME=NAV   SPKID=N003  PROGRAM=NAV     MPB
gspice spacecraft=vgr-1 target=jupiter scet=(1979,63,19,23,0,0) camera=7
! Galileo test - CMSOURCE prints the following text:
!      CKNAME=FARE  SPKID=N011  PROGRAM=FARENC  TXH PI        08/30/96
gspice spacecraft=gll target=venus scet=(1990,44,5,58,16,962) ckname=fare
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create cmsource.hlp
1 CMSOURCE

Given a SPICE buffer, as returned by subroutine GETSPICE95, CMSOURCE
will print the provenance information in the following format:

      CKNAME=FARE  SPKID=NO11  PROGRAM=IBISUPD  GMY059  MIPL  09/22/96


 Fortran Calling Sequence:  CALL CMSOURCE(SBUF,isource)
 C Calling Sequence:        zcmsource(SBUF,&isource);

         REAL*4 SBUF(200)      Input SBUF buffer (returned by GETSPICE)
      OR REAL*8 SBUF(100)      (From C, see HELP for GETSPICE for declaration
                                and for SBUF buffer layout.)
         INTEGER*4 ISOURCE     Output source number. (From C isource is an int.)

  ISOURCE=1  for Mert Davies' C-matrix
         =2  for FARENC
         =3  for NAV
         =4  for NAV2
         =5  for NEARENC
         =6  for AMOS
         =7  for NAIF

NOTE:
  The way source is determined is the so called "hard coded".  At this point it
checks CKID to fall within the range of M901~M906 and output the source.
However, since all the CKID-CKSOURCE corresponding should be according to the
file pointed by the environment variable KERNELDB, this is not a good way for
determination.  Meaning when KERNELDB changes, CMSOURCE.F has to be changed
to reflect.

2 History

  Original Programmer: Gary Yagi, 22 January 1989
  Current Cognizant Programmer: Gary Yagi
  Source Language: FORTRAN
  Ported to UNIX: Steve Pohorsky     9-16-94
  Revisions:
    When       Who  What
    --------   ---  -----------------------------------------------------------
    13 Jun 00  GMY  Update for VGR SPICE kernels
    14 Nov 96  SMC  Fixed to default ISOURCE to 7 (NAIF) for GLL, thus for
                      those CKID not in range M901~M906 default is returned
    12 Nov 96  SMC  Fixed to output correct ISOURCE when processing GLL (DFR)
    10 Oct 96  GMY  Added capability to print provenance data from GETSPICE95
                    buffer.
    16-Sep 94  SP   Corrected message from CMSOURCE to display the last 2 
                    digits of year (SCET) instead of first 2.
    12 Dec 92  gmy  Increase # char in frame number (fr 70977).
    16 May 90  GMY  Modify to work for GETSPICE instead of SEDR79V2.
$ Return
$!#############################################################################
