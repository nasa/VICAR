$!****************************************************************************
$!
$! Build proc for MIPL module fftt
$! VPACK Version 1.9, Monday, December 07, 2009, 16:17:20
$!
$! Execute by entering:		$ @fftt
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
$ write sys$output "*** module fftt ***"
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
$ write sys$output "Invalid argument given to fftt.com file -- ", primary
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
$   if F$SEARCH("fftt.imake") .nes. ""
$   then
$      vimake fftt
$      purge fftt.bld
$   else
$      if F$SEARCH("fftt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftt.bld "STD"
$   else
$      @fftt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftt.com -mixed -
	-s fftt.f zfftt.c -
	-i fftt.imake -
	-t tfftt.f tzfftt.c tfftt.imake tfftt.pdf tstfftt.pdf -
	-o fftt.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C*****************************************************************
C
C     Subroutine FFTT
C     
C     Purpose:  To compute 1-D ffts using RFT2CH instead of
C     FFTT.   It will accept and return the assumed formats.
C     MD comes in with  -1 = direct  +1 = inverse.
C
C        1 JULY 1993   PORTED TO UNIX  (T. L. TRUONG)
C        9-88  SP      MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C*****************************************************************
C
	SUBROUTINE FFTT(IPOW,MD,COM)
	REAL*4 BUF(1026),COM(2050)
C
C ...Where    IPOW is the input size = 2**power elements
C             MD   is +1 for inverse transform
C                     -1 for direct transform
C             COM  is a complex*8 data array
C             BUF  is a buffer containing data to be processed
C
        M=1
	N= 2**IPOW
	MODE = -MD          !CHANGE TO -1 = INVERSE
	IF(MODE .EQ. -1) GO TO 10
C
C------------------------------------
C    FORWARD
C------------------------------------
	CALL MVE(7,N,COM,BUF,2,1)    !STRIP REALS OUT OF COMPLEX
	CALL RFT2CH(BUF,M,N,MODE)    !RETURNED AS COMPLEX
        CALL MVE(7,N+2,buf,com,1,1)
        RETURN
C
C------------------------------------
C    INVERSE
C------------------------------------
10	CALL MVE(7,N+2,COM,BUF,1,1)
	CALL RFT2CH(BUF,M,N,MODE)    !PROCESS COMPLEX DATA
	CALL ZIA(COM,2*N)
	S = 2 * N                    !SCALING FACTOR
	CALL DIVV(7,N,S,BUF,0,1)      !GET VALUES TO PROPER SCALE
	CALL MVE(7,N,BUF,COM,1,2)    !STICK REALS INTO COMPLEX C
        RETURN
C
        END

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zfftt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of FFTT                                         */
/************************************************************************/

void zfftt(power,mode,buffer)
int  power;     /* input size = 2**power elements  (input)*/
int  mode;      /* +1 for inverse transform        (input)
                  -1 for direct transform*/
void *buffer;   /* a complex*8 data array          (i/o)*/

{
FTN_NAME2(fftt, FFTT) (&power,&mode,buffer);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftt.imake
/* Imake file for VICAR subroutine FFTT */

#define SUBROUTINE fftt

#define MODULE_LIST fftt.f zfftt.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tfftt.f
C--------------------------------------------------------------
C THIS IS A TEST OF MODULE RFT2CH
C 
C PORTED TO UNIX 6/25/93
C--------------------------------------------------------------
	INCLUDE 'VICMAIN_FOR'
	subroutine main44
	complex*8 cc(16)
	integer*2 in(16)
c
C---------------------------------
C FORTRAN - CALLABLE
C---------------------------------
c
        CALL XVMESSAGE('*******FORTRAN-CALLABLE RFT2CH******',' ')
        CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
        CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
	ipow=4
	num=16
        CALL XVREAD(IUNIT,in,STAT,'LINE',1,'nsamps',num,' ')
	do 11 l=1,num
11	cc(l) = cmplx(in(l)+0.0,0.0)
c
	call xvmessage(' print the input array',' ')
	call prnt(7,2*num,cc,' input.')
	call fftt(ipow,-1,cc)
	call xvmessage(' print the direct result',' ')
	call prnt(7,2*num,cc,' direct.')
c
	call fftt(ipow,+1,cc)
	call xvmessage(' print the inverse of the direct result',' ')
	call prnt(7,2*num,cc,' inverse.')
c
        CALL XVCLOSE(IUNIT,STAT,' ')
C
C--------------------------------------------------------------
C ----C-CALLABLE
C--------------------------------------------------------------
C
        CALL XVMESSAGE('**********C-CALLABLE RFT2CH*******',' ')
        CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
        CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
	ipow=4
	num=16
        CALL XVREAD(IUNIT,in,STAT,'LINE',1,'nsamps',num,' ')
	do 111 l=1,num
111	cc(l) = cmplx(in(l)+0.0,0.0)
c
	call xvmessage(' print the input array',' ')
	call prnt(7,2*num,cc,' input.')
	call tzfftt(ipow,-1,cc)
	call xvmessage(' print the direct result',' ')
	call prnt(7,2*num,cc,' direct.')
c
	call tzfftt(ipow,+1,cc)
	call xvmessage(' print the inverse of the direct result',' ')
	call prnt(7,2*num,cc,' inverse.')
c
        CALL XVCLOSE(IUNIT,STAT,' ')
	return
	end
$!-----------------------------------------------------------------------------
$ create tzfftt.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* Unit test C-bridge for TFFTT.F                                         */
/************************************************************************/

void FTN_NAME(tzfftt) (power,mode,buffer)
int  *power;     /* input size = 2**power elements  (input)*/
int  *mode;      /* +1 for inverse transform        (input)
                  -1 for direct transform*/
void *buffer;   /* a complex*8 data array          (i/o)*/

{
      zfftt(*power,*mode,buffer);
}

$!-----------------------------------------------------------------------------
$ create tfftt.imake
/* Imake file for Test of VICAR subroutine fftt */

#define PROGRAM tfftt

#define MODULE_LIST tfftt.f tzfftt.c

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
$ create tfftt.pdf
!*****************************************************************************
! TFFTT.PDF - pdf for test program TFFTT.F for the subroutine FFTT
!*****************************************************************************
process
parm inp string
end-proc
$!-----------------------------------------------------------------------------
$ create tstfftt.pdf
!*****************************************************************************
! TSTFFTT.PDF -  unit test procedure for subroutine FFTT.F
!
! A test array will be generated and it will be transformed 
! both direct and inverse and the results will be printed.
!*****************************************************************************
procedure  help=*
refgbl $echo
refgbl $syschar
parm inp string count=0:1 default=--
body
let _onfail="continue"
let $echo="yes"

if ($count(inp)=0) let inp = "testimage.fftt;"
gen out=&inp nl=1 ns=16 format=real

tfftt inp=&inp

if ($syschar(1)="UNIX")
  ush rm &inp
else
  dcl delete/log &inp
end-if
end-proc
.title TSTFFTT.PDF - unit test for subroutine FFTT
.end

$ Return
$!#############################################################################
$Other_File:
$ create fftt.hlp
1 FFTT

	One-dimensional FFT.  Complex input and output.

  FORTRAN Calling Sequence and arguments:  
        
        CALL FFTT(power,mode,buffer)

        integer*4 power....input size = 2**power elements (input)
        integer*4 mode.....+1 for inverse transform (input)
                           -1 for direct transform
        real*4 buffer......a complex*8 data array (input/output)

  C Calling Sequence and arguments:  
        
        fftt(power,mode,buffer)

        int power......input size = 2**power elements (input)
        int mode.......+1 for inverse transform (input)
                       -1 for direct transform
        float *buffer..a complex*8 data array (input/output)

2 History

  Ported to UNIX: T. L. Truong,  7/1/93
  Original Programmer: T. Rindfleisch, 10/16/1967
  Current Cognizant Programmer: C. Avis
  Source Language: Fortran

2 Operation

  FFTT has been rewritten to use the subroutine RFT2CH.
  The complex data array is formatted as RFT2CH expects, and 
  is reformatted upon return from RFT2CH.  It is always 
  returned as a complex*8 array (as the old FFTT did).

  The number of elements is limited by the routine's buffer
  size of 1024 elements.  pow <= 10.
2 Arguments

  power...FFTT can only transform arrays whose size is a power
          of 2.  The power argument defines the size in elements
          by defining that power.

  mode....FFTT can do direct or inverse transforms.  The mode 
          argument determines the direction.  
                mode = -1  means direct
                mode = +1  means inverse.

  buffer..This is both the input data array (complex*8) and the
          output data array (complex*8) of size 2**power elements.

$ Return
$!#############################################################################
