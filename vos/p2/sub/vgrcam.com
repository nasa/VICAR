$!****************************************************************************
$!
$! Build proc for MIPL module vgrcam
$! VPACK Version 1.7, Monday, July 12, 1993, 14:00:49
$!
$! Execute by entering:		$ @vgrcam
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
$ write sys$output "*** module vgrcam ***"
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
$ write sys$output "Invalid argument given to vgrcam.com file -- ", primary
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
$   if F$SEARCH("vgrcam.imake") .nes. ""
$   then
$      vimake vgrcam
$      purge vgrcam.bld
$   else
$      if F$SEARCH("vgrcam.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vgrcam
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vgrcam.bld "STD"
$   else
$      @vgrcam.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vgrcam.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vgrcam.com -
	-s vgrcam.f zvgrcam.c -
	-i vgrcam.imake -
	-t tvgrcam.f tzvgrcam.c tvgrcam.imake tvgrcam.pdf tstvgrcam.pdf -
	-o vgrcam.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vgrcam.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C*********************************************************
C
C     SUBROUTINE VGRCAM
C
C     ROUTINE TO RETURN VOYAGER CAMERA CONSTANTS
C      
C     ICAM    = SERIAL NO.
C     OBUF(1) = FOCAL LENGTH (MM.)
C     OBUF(2) = OPTICAL AXISLINE (O.S. PIXELS)
C     OBUF(3) = OPTICAL AXIS SAMPLE (O.S. PIXELS)
C     OBUF(4) = SCALE (PIXELS/MM.)
C
C    12 JUL 93  T. L. TRUONG  PORTED TO UNIX
C    2 FEB 83   ...CCA...     INITIAL RELEASE
C*********************************************************
      SUBROUTINE VGRCAM(ICAM,OBUF)
      REAL*4 OBUF(*)
      REAL*4 FOC(4,8)
      DATA FOC /
     *1499.12,500.,500.,84.821428,
     *201.568,500.,500.,84.821428,
     *1499.95,500.,500.,84.821428,
     *200.770,500.,500.,84.821428,
     *1503.49,500.,500.,84.821428,
     *200.465,500.,500.,84.821428,
     *1500.19,500.,500.,84.821428,
     *200.293,500.,500.,84.821428/
CCC  *201.640,500.,500.,84.821428    ,
CCC  *1501.68,500.,500.,84.821428     ,
CCC  *201.498,500.,500.,84.821428     ,
CCC  *1499.96,500.,500.,84.821428     ,
C
      CALL MVE(7,4,FOC(1,ICAM),OBUF,1,1)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zvgrcam.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of RFT2CH                                         */
/************************************************************************/

void zvgrcam(sln,buffer)
int   sln;       /* Voyager camera serial number (input) */
void  *buffer;  /* four-word buffer (output) */

{
FTN_NAME(vgrcam)(&sln,buffer);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vgrcam.imake
/* Imake file for VICAR subroutine VGRCAM */

#define SUBROUTINE vgrcam

#define MODULE_LIST vgrcam.f zvgrcam.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN


$ Return
$!#############################################################################
$Test_File:
$ create tvgrcam.f
C----------------------------------------------------------
C-----THIS PROGRAM WILL TEST SUBROUTINE VGRCAM
C-----PORTED TO UNIX 07/12/93
C----------------------------------------------------------
        INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	REAL*4 BUF(4)
C---------------------------------
C FORTRAN - CALLABLE
C---------------------------------

        CALL XVMESSAGE('*******FORTRAN-CALLABLE RFT2CH******',' ')
	CALL VGRCAM(4,BUF)
	CALL PRNT(7,4,BUF,' SN 4 VALUES.')
	CALL VGRCAM(5,BUF)
	CALL PRNT(7,4,BUF,' SN 5 VALUES.')
	CALL VGRCAM(6,BUF)
	CALL PRNT(7,4,BUF,' SN 6 VALUES.')
	CALL VGRCAM(7,BUF)
	CALL PRNT(7,4,BUF,' SN 7 VALUES.')
C
C--------------------------------------------------------------
C ----C-CALLABLE
C--------------------------------------------------------------
C
        CALL XVMESSAGE('**********C-CALLABLE RFT2CH*******',' ')
	CALL TZVGRCAM(4,BUF)
	CALL PRNT(7,4,BUF,' SN 4 VALUES.')
	CALL TZVGRCAM(5,BUF)
	CALL PRNT(7,4,BUF,' SN 5 VALUES.')
	CALL TZVGRCAM(6,BUF)
	CALL PRNT(7,4,BUF,' SN 6 VALUES.')
	CALL TZVGRCAM(7,BUF)
	CALL PRNT(7,4,BUF,' SN 7 VALUES.')
	CALL EXIT
	END
$!-----------------------------------------------------------------------------
$ create tzvgrcam.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TVGRCAM.F */
/************************************************************************/

void FTN_NAME(tzvgrcam) (sln,buffer)
int   *sln;     /* Voyager camera serial number (input) */
void  *buffer;  /* four-word buffer (output) */


{
       zvgrcam(*sln,buffer);
}
$!-----------------------------------------------------------------------------
$ create tvgrcam.imake
/* Imake file for Test of VICAR subroutine vgrcam */

#define PROGRAM tvgrcam

#define MODULE_LIST tvgrcam.f tzvgrcam.c

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
$ create tvgrcam.pdf
!*****************************************************************************
! TVGRCAM.PDF - pdf for test program TVGRCAM.F for the subroutine VGRCAM
!*****************************************************************************
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstvgrcam.pdf
!****************************************************************************
! TSTVGRCAM.PDF, unit test procedure for subroutine VGRCAM.F
!
!THIS IS A TEST OF MODULE VGRCAM
!VGRCAM WILL RETURN A 4 WORD BUFFER OF FLOATING POINT
!NUMBERS FOR AN INPUT SERIAL NUMBER OF 4 TO 7.
!THESE NUMBERS REPRESENT  1) CAMERA FOCAL LENGTH
!                         2) LINE OF OPTICAL AXIS
!                         3) SAMP OF OPTICAL AXIS
!                         4) IMAGE SCALE
!****************************************************************************
procedure help=*
refgbl $echo

body

let _onfail="continue"
let $echo="yes"
tvgrcam

end-proc
.title TSTVGRCAM.PDF - unit test for subroutine VGRCAM
.end
$ Return
$!#############################################################################
$Other_File:
$ create vgrcam.hlp
1 VGRCAM

       VGRCAM will return Voyager camera constants in a four-word buffer.

2  CALLING SEQUENCE

	FORTRAN calling sequence:
	
	       CALL VGRCAM(SN,BUF)

	C calling sequence:

		vgrcam(sn,buf)

2  ARGUMENTS

       Sn      is the Voyager caamera serial number, I*4.

       Buf     is the returned four-word buffer,     R*4
               
               BUF(1) = Focal Length in mm.
               BUF(2) = Optical Axis Line (Object Space)
               BUF(3) = Optical Axis Sample (Object Space)
               BUF(4) = Camera Scale (pixels/mm)

2  HISTORY

      Original Programmer: Gary Yagi
      Current Cognizant Programmer: Charlie Avis
      Source Language: Fortran
      Latest Revision: New, 15 December 1982
      Ported to UNIX: T. L. Truong, 12 July 1993

2 OPERATION

       REAL*4 MAP(40) [the standard 40-word geometry buffer defined in
       subroutine MAP]
       CALL VGRCAM(SN,MAP(27))

       SUBROUTINES CALLED:  MVE


$ Return
$!#############################################################################
