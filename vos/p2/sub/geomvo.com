$!****************************************************************************
$!
$! Build proc for MIPL module geomvo
$! VPACK Version 1.9, Monday, December 07, 2009, 16:19:07
$!
$! Execute by entering:		$ @geomvo
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
$ write sys$output "*** module geomvo ***"
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
$ write sys$output "Invalid argument given to geomvo.com file -- ", primary
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
$   if F$SEARCH("geomvo.imake") .nes. ""
$   then
$      vimake geomvo
$      purge geomvo.bld
$   else
$      if F$SEARCH("geomvo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geomvo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geomvo.bld "STD"
$   else
$      @geomvo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geomvo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geomvo.com -mixed -
	-s geomvo.f zgeomvo.c -
	-i geomvo.imake -
	-t tgeomvo.f tzgeomvo.c tgeomvo.imake tgeomvo.pdf tstgeomvo.pdf -
	-o geomvo.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create geomvo.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c******************************************************************
C 
c Subroutine   GEOMVO(CONV,ICAM,LOCS)
c 
c Viking Orbiter routine to generate GEOMA parameters.
c
c ARGUMENTS
c
c  Input:   
c
c    ICAM  - Viking Orbiter camera serial number.  Valid are:
c        	7=VO-1A		8 = VO-2A
c	        4=VO-1B		6 = VO-2B
c    RES   - Image space line-sample coordinates for the 103 reseau
c	    marks of a Viking Orbiter frame.
c  Output:
c
c    CONV  - GEOMA parameters in the following format:
c
c		 CONV(1) = 'NAH '
c		 CONV(2) = '    '
c		 CONV(3) = 21		(INTEGER*4, int)
c		 CONV(4) = 'NAV '
c		 CONV(5) = '    '
c		 CONV(6) = 8		(INTEGER*4, int)
c		 CONV(7) = 'TIEP'
c		 CONV(8) = '    '
c		 CONV(9) = beginning of tiepoints in REAL*4, float format
c
c HISTORY
c
c  Original Programmer: Gary Yagi, 1 June 1990
c  Current Cognizant Programmer: G. Yagi
c  Source Language: Fortran
c  Revisions: 
c		25-Aug-93  ...TLT...  Ported to Unix
C
c******************************************************************
c
      SUBROUTINE GEOMVO(CONV,ICAM,LOCS)
c
      IMPLICIT INTEGER (A-Z)
      REAL*4 LOCS(2,103)	!Input image-space reseau locations
      REAL*4 CONV(*)		!Output GEOMA parameters
      REAL*4 OSLOCS(2,103)	!Object-space reseau locations
      CHARACTER*4 NAH,NAV,TIEP,BLANK
      DATA NAH/'NAH '/
      DATA NAV/'NAV '/
      DATA TIEP/'TIEP'/
      DATA BLANK/'    '/

      T21 = 21
      T8 = 8
      CALL MVCL(NAH,CONV(1),4)
      CALL MVCL(BLANK,CONV(2),4)
      CALL MVE(-4,1,T21,CONV(3),1,1)
      CALL MVCL(NAV,CONV(4),4)
      CALL MVCL(BLANK,CONV(5),4)
      CALL MVE(-4,1,T8,CONV(6),1,1)
      CALL MVCL(TIEP,CONV(7),4)
      CALL MVCL(BLANK,CONV(8),4)
      CALL VOOS(ICAM,OSLOCS)	!Get OS reseau locations
      J = 8
C
      DO 100 N1=1,103,23
      N2 = N1 + 10
      M1 = N1 + 11
      M2 = N1 +21
C
      DO 10 I=N1,N2
      CONV(J+1)=OSLOCS(1,I)
      CONV(J+2)=OSLOCS(2,I)
      CONV(J+3)=LOCS(1,I)
      CONV(J+4)=LOCS(2,I)
      CONV(J+5)=OSLOCS(1,I)
      CONV(J+6)=OSLOCS(2,I)
      CONV(J+7)=LOCS(1,I)
      CONV(J+8)=LOCS(2,I)
   10 J = J + 8
C
      DO 20 I=M1,M2
      CONV(J+1)=OSLOCS(1,I)
      CONV(J+2)=OSLOCS(2,I)
      CONV(J+3)=LOCS(1,I)
      CONV(J+4)=LOCS(2,I)
      CONV(J+5)=OSLOCS(1,I+1)
      CONV(J+6)=OSLOCS(2,I+1)
      CONV(J+7)=LOCS(1,I+1)
      CONV(J+8)=LOCS(2,I+1)
   20 J = J + 8
C
  100 CONTINUE
C
      CALL PRNT(4,1,J,' CONV size=.')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zgeomvo.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of GEOMVO                                         */
/************************************************************************/

void zgeomvo(conv,icam,res)
	int icam;	/* Viking Orbiter camera serial number (input) */
	void *res;      /* image-space reseau locations (input) */
	void *conv;     /* GEOMA parameters (output) */

{
FTN_NAME2(geomvo, GEOMVO) (conv,&icam,res);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geomvo.imake
/* Imake file for VICAR subroutine GEOMVO */

#define SUBROUTINE geomvo

#define MODULE_LIST geomvo.f zgeomvo.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tgeomvo.f
C--------------------------------------------------------------
C THIS IS A TEST OF MODULE GEOMVO
C 
C PORTED TO UNIX 8/25/93
C--------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c      
      INTEGER ICAM
      REAL*4 RLOC(404)
      REAL*4 CONV(2216)

      call xvmessage('**************fortran callable***********',' ')
c
c  test 1
c      
      ICAM = 4
      CALL XVMESSAGE(' ',' ')
      CALL  PRNT(4,1,ICAM,'----------GEOMA parameters for ICAM=   .')
      CALL GETRES(RLOC,ICAM)         !to generate input test values only
      CALL GEOMVO(CONV,ICAM,RLOC)
      CALL PRNT(99,4,CONV(1),'CONV(1)=    .')
      CALL PRNT(99,4,CONV(2),'CONV(2)=    .')
      CALL PRNT(4,1,CONV(3),'CONV(3)=    .')
      CALL PRNT(99,4,CONV(4),'CONV(4)=    .')
      CALL PRNT(99,4,CONV(5),'CONV(5)=    .')
      CALL PRNT(4,1,CONV(6),'CONV(6)=    .')
      CALL PRNT(99,4,CONV(7),'CONV(7)=    .')
      CALL PRNT(99,4,CONV(8),'CONV(8)=    .')
      CALL XVMESSAGE('1st 10 of tiepoints=',' ')
      CALL PRNT(7,5,CONV(9),' ')
      CALL PRNT(7,5,CONV(14),' ')
c
c  tests 2-4
c
      DO 100 ICAM=6,8
C
      CALL XVMESSAGE(' ',' ')
      CALL  PRNT(4,1,ICAM,'----------GEOMA parameters for ICAM=   .')
      CALL GETRES(RLOC,ICAM)
      CALL GEOMVO(CONV,ICAM,RLOC)
      CALL PRNT(99,4,CONV(1),'CONV(1)=    .')
      CALL PRNT(99,4,CONV(2),'CONV(2)=    .')
      CALL PRNT(4,1,CONV(3),'CONV(3)=    .')
      CALL PRNT(99,4,CONV(4),'CONV(4)=    .')
      CALL PRNT(99,4,CONV(5),'CONV(5)=    .')
      CALL PRNT(4,1,CONV(6),'CONV(6)=    .')
      CALL PRNT(99,4,CONV(7),'CONV(7)=    .')
      CALL PRNT(99,4,CONV(8),'CONV(8)=    .')
      CALL XVMESSAGE('1st 10 of tiepoints=',' ')
      CALL PRNT(7,5,CONV(9),' ')
      CALL PRNT(7,5,CONV(14),' ')
c
100   CONTINUE
C
      call xvmessage('**************C callable***********',' ')
c
c  test 5
c
      ICAM=8
      CALL XVMESSAGE(' ',' ')
      CALL  PRNT(4,1,ICAM,'----------GEOMA parameters for ICAM=   .')
      CALL GETRES(RLOC,ICAM)
      CALL tzGEOMVO(CONV,ICAM,RLOC)
      CALL PRNT(99,4,CONV(1),'CONV(1)=    .')
      CALL PRNT(99,4,CONV(2),'CONV(2)=    .')
      CALL PRNT(4,1,CONV(3),'CONV(3)=    .')
      CALL PRNT(99,4,CONV(4),'CONV(4)=    .')
      CALL PRNT(99,4,CONV(5),'CONV(5)=    .')
      CALL PRNT(4,1,CONV(6),'CONV(6)=    .')
      CALL PRNT(99,4,CONV(7),'CONV(7)=    .')
      CALL PRNT(99,4,CONV(8),'CONV(8)=    .')
      CALL XVMESSAGE('1st 10 of tiepoints=',' ')
      CALL PRNT(7,5,CONV(9),' ')
      CALL PRNT(7,5,CONV(14),' ')
      return
      end
$!-----------------------------------------------------------------------------
$ create tzgeomvo.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TGEOMVO.F */
/************************************************************************/

void FTN_NAME(tzgeomvo)(conv,icam,res)
	int *icam;	/* Viking Orbiter camera serial number (input) */
	void *res;      /* image-space reseau locations (input) */
	void *conv;     /* GEOMA parameters (output) */
{
       zgeomvo(conv,*icam,res);
}

$!-----------------------------------------------------------------------------
$ create tgeomvo.imake
/* Imake file for Test of VICAR subroutine geomvo */

#define PROGRAM tgeomvo

#define MODULE_LIST tgeomvo.f tzgeomvo.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
$!-----------------------------------------------------------------------------
$ create tgeomvo.pdf
!*****************************************************************************
! TGEOMVO.PDF - pdf for test program TGEOMVO.F for the subroutine GEOMVO
!*****************************************************************************
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgeomvo.pdf
!****************************************************************************
! TSTGEOMVO.PDF, unit test procedure for subroutine GEOMVO.F
!
!THIS IS A TEST OF MODULE GEOMVO
!****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

tgeomvo

end-proc
.title TSTGEOMVO.PDF - unit test for subroutine GEOMVO
.end
$ Return
$!#############################################################################
$Other_File:
$ create geomvo.hlp
1  GEOMVO

   Given the VO camera serial number and the image-space reseau locations 
   for a Viking Orbiter image, GEOMVO returns the geometric distortion 
   correction parameters.  The resulting parameters may then be input 
   into subroutine TRITRA or CONVISOS for converting between image-space 
   and object-space line-sample coordinates.  The parameters are also 
   suitable for input to geometric transformation programs LGEOM, MGEOM, 
   and MGEOM.
 
   Fortran calling sequence:  

	INTEGER*4 ICAM	
	REAL*4 RES(2,103)
	REAL*4 CONV(2216) 

	CALL GEOMVO(CONV,ICAM,RES)

   C calling sequence:

	int icam;	
	float res[102][1];
	float conv[2215]; 

	zgeomvo(conv,icam,res);

2 ARGUMENTS

  Input:   

    ICAM  - Viking Orbiter camera serial number.  Valid are:
        	7=VO-1A		8 = VO-2A
	        4=VO-1B		6 = VO-2B
    RES   - Image space line-sample coordinates for the 103 reseau
	    marks of a Viking Orbiter frame.
  Output:

    CONV  - GEOMA parameters in the following format:

		 CONV(1) = 'NAH '
		 CONV(2) = '    '
		 CONV(3) = 21		(INTEGER*4, int)
		 CONV(4) = 'NAV '
		 CONV(5) = '    '
		 CONV(6) = 8		(INTEGER*4, int)
		 CONV(7) = 'TIEP'
		 CONV(8) = '    '
		 CONV(9) = beginning of tiepoints in REAL*4, float format

2 HISTORY

  Original Programmer: Gary Yagi, 1 June 1990
  Current Cognizant Programmer: G. Yagi
  Source Language: Fortran
  Revisions: 
		25-Aug-93  ...TLT...  Ported to Unix
$ Return
$!#############################################################################
