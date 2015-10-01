$!****************************************************************************
$!
$! Build proc for MIPL module rft2ch
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:34:22
$!
$! Execute by entering:		$ @rft2ch
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
$ write sys$output "*** module rft2ch ***"
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
$ write sys$output "Invalid argument given to rft2ch.com file -- ", primary
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
$   if F$SEARCH("rft2ch.imake") .nes. ""
$   then
$      vimake rft2ch
$      purge rft2ch.bld
$   else
$      if F$SEARCH("rft2ch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rft2ch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rft2ch.bld "STD"
$   else
$      @rft2ch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rft2ch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rft2ch.com -mixed -
	-s rft2ch.f zrft2ch.c -
	-i rft2ch.imake -
	-t trft2ch.f tzrft2ch.c trft2ch.imake trft2ch.pdf tstrft2ch.pdf -
	   tstrft2ch.log -
	-o rft2ch.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rft2ch.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C************************************************************************
C
C     Subroutine RFT2CH
C
C     Purpose:  To compute 1-D and 2-D real FFT's in core.
C     RFT2CH calls RFT2, DFFT, and REALTR.
C
C     25 JUN 93   T. L. Truong   PORTED TO UNIX
C     10 OCT 83    ...CCA...     CONVERT TO VAX
C     5 MAR 79    ...JJL...      INITIAL RELEASE
c 15-Jan-2013  ...lwk... fixed continued CHARACTER constant for new compiler flag on Solaris
C
C************************************************************************

      SUBROUTINE RFT2CH (BUF,M,N,MODE)
      REAL*4 BUF(*)
C
C ...Where   M     is the number of lines in the data matrix to be transformed
C            N     is the number of samples in data matrix to be transformed
C            MODE  is the direction of the transform, 1(direct) or -1(inverse)
C            BUF   is a buffer containing the data to be transformed
C
      IF(M.GT.1) GO TO 10
      K=N/2
      IF(N.NE.K*2) GO TO 1
      IF(MODE.LT.0) GO TO 11
C****************************
C  DIRECT REAL 1-D FFT
C****************************
      CALL DFFT(BUF(1),BUF(2),K,K,K,2,*1,*2)
      CALL REALTR(BUF(1),BUF(2),K,2)
      RETURN
C****************************
C  INVERSE REAL 1-D FFT
C****************************
11    CONTINUE
      CALL REALTR(BUF(1),BUF(2),K,-2)
      CALL DFFT(BUF(1),BUF(2),K,K,K,-2,*1,*2)
      RETURN
10    CONTINUE
C****************************
C  REAL 2-D FFT DIRECT AND INVERSE
C****************************
      CALL RFT2(BUF,M,N,MODE,ISTATUS)

      IF (ISTATUS.NE.1) THEN
       IF (ISTATUS.EQ.1) GOTO 1
       IF (ISTATUS.EQ.2) GOTO 2
       IF (ISTATUS.EQ.3) GOTO 3
      ENDIF

      RETURN
C****************************
C  PRINT ERROR MESSAGES
C****************************
1     CALL QPRINT(
     +  '0A PRIME FACTOR OF M OR N EXCEEDS 23, RFT2 ABEND',48)
      CALL ABEND
2     CALL QPRINT('0THE PRODUCT OF THE SQUARE-FREE FACTORS OF M OR N IS TOO LARGE, RFT2 ABEND',75)
      CALL ABEND
3     CALL QPRINT('0M IS ODD, RFT2 ABEND',21)
      CALL ABEND
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zrft2ch.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of RFT2CH                                         */
/************************************************************************/

void zrft2ch(buffer,nlines,nsampl,tmode)
void  *buffer;   /* buffer containing the data to be transformed (i/o) */
int   nlines;   /* number of lines in the matrix (M)        (input) */
int   nsampl;   /* number of samples in the matrix (N)      (input) */
int   tmode;     /* flag to do forward/reverse transform (input) */

{
FTN_NAME(rft2ch)(buffer,&nlines,&nsampl,&tmode);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rft2ch.imake
/* Imake file for VICAR subroutine RFT2CH */

#define SUBROUTINE rft2ch

#define MODULE_LIST rft2ch.f zrft2ch.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

/*#define LIB_LOCAL /* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create trft2ch.f
C--------------------------------------------------------------
C THIS IS A TEST OF MODULE RFT2CH
C 
C PORTED TO UNIX 6/25/93
C--------------------------------------------------------------
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	REAL*4 BUF(20),BUF2(20,22)
C---------------------------------
C FORTRAN - CALLABLE
C---------------------------------

        CALL XVMESSAGE('*******FORTRAN-CALLABLE RFT2CH******',' ')
	CALL QPRINT('01-D CASE',9)
C
C-----CONSTRUCT INPUT VECTOR
	N=20	
	DO 10 I=1,N
10	BUF(I) = I
	CALL PRNT(7,N,BUF,' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL RFT2CH(BUF,1,N,1)
C
	CALL PRNT(7,N+2,BUF,' CMPLX XFORM.')
C-----INVERT TRANSFORM
	CALL RFT2CH(BUF,1,N,-1)
C
C-----RESCALE
	A=2*N
	CALL DIVV(7,N,A,BUF,0,1)
	CALL PRNT(7,N,BUF,' INVRS XFORM.')
C
C
C---------------------------------------------------------
	CALL QPRINT('02-D CASE',9)
	N=20
	M=20
C-----CONSTRUCT INPUT ARRAY
	DO 20 J=1,M
	DO 150 I=1,N
150	BUF2(I,J) = I + J
20	CALL PRNT(7,N,BUF2(1,J),' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL RFT2CH(BUF2,M,N,1)
	DO 30 J=1,M+2
30	CALL PRNT(7,N,BUF2(1,J),' CMPLX XFORM.')
C
C-----DO INVERSE
	CALL RFT2CH(BUF2,M,N,-1)
C
	A=2*M*N
	DO 40 J=1,M
	CALL DIVV(7,N,A,BUF2(1,J),0,1)
40	CALL  PRNT(7,N,BUF2(1,J),' INVRS XFORM.')
C
C--------------------------------------------------------------
C ----C-CALLABLE
C--------------------------------------------------------------
        CALL XVMESSAGE('**********C-CALLABLE RFT2CH*******',' ')
	CALL QPRINT('01-D CASE',9)
C
C-----CONSTRUCT INPUT VECTOR
	N=20	
	DO 210 I=1,N
210	BUF(I) = I
	CALL PRNT(7,N,BUF,' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL TZRFT2CH(BUF,1,N,1)
C
	CALL PRNT(7,N+2,BUF,' CMPLX XFORM.')
C-----INVERT TRANSFORM
	CALL TZRFT2CH(BUF,1,N,-1)
C
C-----RESCALE
	A=2*N
	CALL DIVV(7,N,A,BUF,0,1)
	CALL PRNT(7,N,BUF,' INVRS XFORM.')
C
C
C---------------------------------------------------------
	CALL QPRINT('02-D CASE',9)
	N=20
	M=20
C-----CONSTRUCT INPUT ARRAY
	DO 220 J=1,M
	DO 350 I=1,N
350	BUF2(I,J) = I + J
220	CALL PRNT(7,N,BUF2(1,J),' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL TZRFT2CH(BUF2,M,N,1)
	DO 230 J=1,M+2
230	CALL PRNT(7,N,BUF2(1,J),' CMPLX XFORM.')
C
C-----DO INVERSE
	CALL TZRFT2CH(BUF2,M,N,-1)
C
	A=2*M*N
	DO 240 J=1,M
	CALL DIVV(7,N,A,BUF2(1,J),0,1)
240	CALL  PRNT(7,N,BUF2(1,J),' INVRS XFORM.')
C
	RETURN
	END

$!-----------------------------------------------------------------------------
$ create tzrft2ch.c
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TRFT2CH.F */
/************************************************************************/

void FTN_NAME(tzrft2ch) (buffer,nlines,nsampl,tmode)
void  *buffer;   /* buffer containing the data to be transformed (i/o) */
int   *nlines;   /* number of lines in the matrix (M)        (input) */
int   *nsampl;   /* number of samples in the matrix (N)      (input) */
int   *tmode;     /* flag to do forward/reverse transform (input) */

{
       zrft2ch(buffer,*nlines,*nsampl,*tmode);
}

$!-----------------------------------------------------------------------------
$ create trft2ch.imake
/* Imake file for Test of VICAR subroutine rft2ch */

#define PROGRAM trft2ch

#define MODULE_LIST trft2ch.f tzrft2ch.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define LIB_LOCAL /* remove before delivery */
$!-----------------------------------------------------------------------------
$ create trft2ch.pdf
!*****************************************************************************
! TRFT2CH.PDF - pdf for test program TRFT2CH.F for the subroutine RFT2CH
!*****************************************************************************
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstrft2ch.pdf
!****************************************************************************
! TSTRFT2CH.PDF, unit test procedure for subroutine RFT2CH.F
!
!THIS IS A TEST OF MODULE RFT2CH
!TWO TESTS WILL BE RUN,  THE FIRST WILL BE A 1-D TEST
!THE SECOND WILL BE A 2-D CASE.  BOTH WILL HAVE THE INPUT,
!THE TRANSFORM , AND THE INVERSE TRANSFORM PRINTED OUT.
!THE INVERSE TRANSFORM SHOULD BE IDENTICAL WITH THE ORIGINAL.
!****************************************************************************
procedure help=*
refgbl $echo

body

let _onfail="continue"
let $echo="yes"

trft2ch

end-proc
.title TSTRFT2CH.PDF - unit test for subroutine RFT2CH
.end
$!-----------------------------------------------------------------------------
$ create tstrft2ch.log
tstrft2ch
trft2ch
Beginning VICAR task trft2ch
*******FORTRAN-CALLABLE RFT2CH******

1-D CASE
 INPUT R*4
            1.000E+00  2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01
 CMPLX XFORM
            4.200E+02  0.000E+00 -2.000E+01 -1.263E+02 -2.000E+01 -6.155E+01 -2.000E+01 -3.925E+01 -2.000E+01 -2.753E+01
           -2.000E+01 -2.000E+01 -2.000E+01 -1.453E+01 -2.000E+01 -1.019E+01 -2.000E+01 -6.498E+00 -2.000E+01 -3.168E+00
           -2.000E+01  0.000E+00
 INVRS XFORM
            1.000E+00  2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01

2-D CASE
 INPUT R*4
            2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
 INPUT R*4
            3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
 INPUT R*4
            4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
 INPUT R*4
            5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
 INPUT R*4
            6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
 INPUT R*4
            7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
 INPUT R*4
            8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
 INPUT R*4
            9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
 INPUT R*4
            1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
 INPUT R*4
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
 INPUT R*4
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
            2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01
 INPUT R*4
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
            2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01
 INPUT R*4
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
            2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01
 INPUT R*4
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
            2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01
 INPUT R*4
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
            2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01
 INPUT R*4
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
            2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01
 INPUT R*4
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
            2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01
 INPUT R*4
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
            2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01
 INPUT R*4
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
            3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01
 INPUT R*4
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
            3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01  4.000E+01
 CMPLX XFORM
            1.680E+04 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02
           -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02
 CMPLX XFORM
            0.000E+00 -2.526E+03 -1.231E+03 -7.850E+02 -5.506E+02 -4.000E+02 -2.906E+02 -2.038E+02 -1.300E+02 -6.335E+01
            0.000E+00  6.335E+01  1.300E+02  2.038E+02  2.906E+02  4.000E+02  5.506E+02  7.850E+02  1.231E+03  2.526E+03
 CMPLX XFORM
           -4.000E+02  0.000E+00 -4.136E-06  0.000E+00 -1.904E-05  0.000E+00 -2.375E-05  0.000E+00 -1.648E-05  0.000E+00
            0.000E+00  0.000E+00  1.939E-05  0.000E+00  3.430E-05  0.000E+00  3.901E-05  0.000E+00  3.174E-05  0.000E+00
 CMPLX XFORM
           -2.526E+03  0.000E+00  5.969E-05  0.000E+00  4.720E-05  0.000E+00  2.834E-05  0.000E+00  1.031E-05  0.000E+00
            0.000E+00  0.000E+00  1.344E-06  0.000E+00  1.383E-05  0.000E+00  3.269E-05  0.000E+00  5.072E-05  0.000E+00
 CMPLX XFORM
           -4.000E+02  6.389E-06 -5.138E-05  1.998E-05 -4.034E-05  4.196E-05 -1.876E-05  3.225E-05 -1.029E-05  2.070E-05
            3.052E-05  2.195E-05  4.524E-05  2.137E-05  2.241E-05  3.433E-05  3.670E-05  5.263E-05  6.220E-05  3.454E-05
 CMPLX XFORM
           -1.231E+03  3.601E-05  1.197E-04  4.985E-05  9.696E-05  2.861E-05  4.548E-05  2.169E-05  9.889E-06  2.308E-05
            6.104E-05  1.323E-05 -5.903E-07  3.330E-06 -4.792E-08 -1.335E-05 -2.032E-05 -3.381E-05 -6.902E-06 -1.420E-05
 CMPLX XFORM
           -4.000E+02  0.000E+00  9.317E-06  0.000E+00  2.731E-06  0.000E+00 -1.984E-06  0.000E+00 -3.027E-06  0.000E+00
            0.000E+00  0.000E+00  5.942E-06  0.000E+00  1.253E-05  0.000E+00  1.724E-05  0.000E+00  1.829E-05  0.000E+00
 CMPLX XFORM
           -7.850E+02  0.000E+00  1.829E-05  0.000E+00  1.724E-05  0.000E+00  1.253E-05  0.000E+00  5.942E-06  0.000E+00
            0.000E+00  0.000E+00 -3.027E-06  0.000E+00 -1.984E-06  0.000E+00  2.731E-06  0.000E+00  9.317E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  9.930E-06 -2.791E-05  9.289E-06 -4.451E-05  3.242E-05 -2.388E-05  2.796E-05 -3.331E-06  1.026E-05
            1.526E-05  1.386E-05  4.429E-05  2.325E-05  4.632E-05  3.242E-05  5.259E-05  4.812E-05  6.325E-05  4.044E-05
 CMPLX XFORM
           -5.506E+02  1.945E-05  5.389E-05  5.192E-05  5.339E-05  3.815E-05  1.234E-05  2.461E-05  4.177E-06  2.948E-05
            0.000E+00  1.835E-05  4.909E-06  2.722E-06 -1.122E-05  0.000E+00 -2.398E-05 -1.350E-05 -3.245E-05 -1.859E-05
 CMPLX XFORM
           -4.000E+02  0.000E+00  9.317E-06  0.000E+00  2.731E-06  0.000E+00 -1.984E-06  0.000E+00 -3.027E-06  0.000E+00
            0.000E+00  0.000E+00  5.942E-06  0.000E+00  1.253E-05  0.000E+00  1.724E-05  0.000E+00  1.829E-05  0.000E+00
 CMPLX XFORM
           -4.000E+02  0.000E+00  1.829E-05  0.000E+00  1.724E-05  0.000E+00  1.253E-05  0.000E+00  5.942E-06  0.000E+00
            0.000E+00  0.000E+00 -3.027E-06  0.000E+00 -1.984E-06  0.000E+00  2.731E-06  0.000E+00  9.317E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  4.356E-06 -3.133E-05 -3.541E-06 -4.524E-05  2.098E-05 -2.752E-05  1.790E-05 -5.296E-06 -7.057E-06
            1.526E-05 -3.451E-06  4.008E-05  1.319E-05  3.179E-05  2.098E-05  4.098E-05  3.529E-05  5.758E-05  3.487E-05
 CMPLX XFORM
           -2.906E+02  1.070E-05  3.006E-05  4.040E-05  2.708E-05  3.815E-05 -1.044E-05  2.760E-05 -1.656E-05  2.970E-05
            0.000E+00  1.813E-05  2.110E-05 -2.692E-07  1.509E-05  0.000E+00 -1.212E-06 -1.982E-06 -1.935E-05 -9.837E-06
 CMPLX XFORM
           -4.000E+02  0.000E+00  4.659E-06  0.000E+00  1.366E-06  0.000E+00 -9.921E-07  0.000E+00 -1.514E-06  0.000E+00
            0.000E+00  0.000E+00  2.971E-06  0.000E+00  6.264E-06  0.000E+00  8.621E-06  0.000E+00  9.143E-06  0.000E+00
 CMPLX XFORM
           -2.038E+02  0.000E+00  9.143E-06  0.000E+00  8.621E-06  0.000E+00  6.264E-06  0.000E+00  2.971E-06  0.000E+00
            0.000E+00  0.000E+00 -1.514E-06  0.000E+00 -9.921E-07  0.000E+00  1.366E-06  0.000E+00  4.659E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  1.474E-05 -1.249E-05 -8.030E-07 -2.113E-05  1.907E-05 -1.120E-05  1.253E-05 -4.059E-06 -1.963E-05
            0.000E+00 -9.408E-06  2.813E-05  1.616E-05  1.664E-05  1.144E-05  1.568E-05  1.734E-05  3.420E-05  3.393E-05
 CMPLX XFORM
           -1.300E+02  8.913E-06  6.704E-06  2.245E-05  6.039E-06  3.242E-05 -1.117E-05  3.057E-05 -1.960E-05  1.841E-05
            7.629E-06  5.560E-06  3.376E-05 -8.366E-07  2.906E-05 -1.907E-06  1.040E-05 -1.693E-06  2.018E-06  5.464E-07
 CMPLX XFORM
           -4.000E+02  0.000E+00  1.296E-05  0.000E+00  8.627E-06  0.000E+00  3.911E-06  0.000E+00  6.163E-07  0.000E+00
            0.000E+00  0.000E+00  2.298E-06  0.000E+00  6.632E-06  0.000E+00  1.135E-05  0.000E+00  1.464E-05  0.000E+00
 CMPLX XFORM
           -6.335E+01  0.000E+00  7.072E-06  0.000E+00  9.129E-06  0.000E+00  8.244E-06  0.000E+00  4.758E-06  0.000E+00
            0.000E+00  0.000E+00 -4.211E-06  0.000E+00 -6.268E-06  0.000E+00 -5.383E-06  0.000E+00 -1.897E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  0.000E+00  1.380E-05  0.000E+00  9.987E-06  0.000E+00  5.272E-06  0.000E+00  1.457E-06  0.000E+00
            0.000E+00  0.000E+00  1.457E-06  0.000E+00  5.272E-06  0.000E+00  9.987E-06  0.000E+00  1.380E-05  0.000E+00
 CMPLX XFORM
            0.000E+00  0.000E+00  4.484E-06  0.000E+00  7.256E-06  0.000E+00  7.256E-06  0.000E+00  4.484E-06  0.000E+00
            0.000E+00  0.000E+00 -4.484E-06  0.000E+00 -7.256E-06  0.000E+00 -7.256E-06  0.000E+00 -4.484E-06  0.000E+00
 INVRS XFORM
            2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
 INVRS XFORM
            3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
 INVRS XFORM
            4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
 INVRS XFORM
            5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
 INVRS XFORM
            6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
 INVRS XFORM
            7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
 INVRS XFORM
            8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
 INVRS XFORM
            9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
 INVRS XFORM
            1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
 INVRS XFORM
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
 INVRS XFORM
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
            2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01
 INVRS XFORM
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
            2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01
 INVRS XFORM
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
            2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01
 INVRS XFORM
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
            2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01
 INVRS XFORM
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
            2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01
 INVRS XFORM
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
            2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01
 INVRS XFORM
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
            2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01
 INVRS XFORM
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
            2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01
 INVRS XFORM
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
            3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01
 INVRS XFORM
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
            3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01  4.000E+01
**********C-CALLABLE RFT2CH*******

1-D CASE
 INPUT R*4
            1.000E+00  2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01
 CMPLX XFORM
            4.200E+02  0.000E+00 -2.000E+01 -1.263E+02 -2.000E+01 -6.155E+01 -2.000E+01 -3.925E+01 -2.000E+01 -2.753E+01
           -2.000E+01 -2.000E+01 -2.000E+01 -1.453E+01 -2.000E+01 -1.019E+01 -2.000E+01 -6.498E+00 -2.000E+01 -3.168E+00
           -2.000E+01  0.000E+00
 INVRS XFORM
            1.000E+00  2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01

2-D CASE
 INPUT R*4
            2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
 INPUT R*4
            3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
 INPUT R*4
            4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
 INPUT R*4
            5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
 INPUT R*4
            6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
 INPUT R*4
            7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
 INPUT R*4
            8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
 INPUT R*4
            9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
 INPUT R*4
            1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
 INPUT R*4
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
 INPUT R*4
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
            2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01
 INPUT R*4
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
            2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01
 INPUT R*4
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
            2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01
 INPUT R*4
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
            2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01
 INPUT R*4
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
            2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01
 INPUT R*4
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
            2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01
 INPUT R*4
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
            2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01
 INPUT R*4
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
            2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01
 INPUT R*4
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
            3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01
 INPUT R*4
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
            3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01  4.000E+01
 CMPLX XFORM
            1.680E+04 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02
           -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02 -4.000E+02
 CMPLX XFORM
            0.000E+00 -2.526E+03 -1.231E+03 -7.850E+02 -5.506E+02 -4.000E+02 -2.906E+02 -2.038E+02 -1.300E+02 -6.335E+01
            0.000E+00  6.335E+01  1.300E+02  2.038E+02  2.906E+02  4.000E+02  5.506E+02  7.850E+02  1.231E+03  2.526E+03
 CMPLX XFORM
           -4.000E+02  0.000E+00 -4.136E-06  0.000E+00 -1.904E-05  0.000E+00 -2.375E-05  0.000E+00 -1.648E-05  0.000E+00
            0.000E+00  0.000E+00  1.939E-05  0.000E+00  3.430E-05  0.000E+00  3.901E-05  0.000E+00  3.174E-05  0.000E+00
 CMPLX XFORM
           -2.526E+03  0.000E+00  5.969E-05  0.000E+00  4.720E-05  0.000E+00  2.834E-05  0.000E+00  1.031E-05  0.000E+00
            0.000E+00  0.000E+00  1.344E-06  0.000E+00  1.383E-05  0.000E+00  3.269E-05  0.000E+00  5.072E-05  0.000E+00
 CMPLX XFORM
           -4.000E+02  6.389E-06 -5.138E-05  1.998E-05 -4.034E-05  4.196E-05 -1.876E-05  3.225E-05 -1.029E-05  2.070E-05
            3.052E-05  2.195E-05  4.524E-05  2.137E-05  2.241E-05  3.433E-05  3.670E-05  5.263E-05  6.220E-05  3.454E-05
 CMPLX XFORM
           -1.231E+03  3.601E-05  1.197E-04  4.985E-05  9.696E-05  2.861E-05  4.548E-05  2.169E-05  9.889E-06  2.308E-05
            6.104E-05  1.323E-05 -5.903E-07  3.330E-06 -4.792E-08 -1.335E-05 -2.032E-05 -3.381E-05 -6.902E-06 -1.420E-05
 CMPLX XFORM
           -4.000E+02  0.000E+00  9.317E-06  0.000E+00  2.731E-06  0.000E+00 -1.984E-06  0.000E+00 -3.027E-06  0.000E+00
            0.000E+00  0.000E+00  5.942E-06  0.000E+00  1.253E-05  0.000E+00  1.724E-05  0.000E+00  1.829E-05  0.000E+00
 CMPLX XFORM
           -7.850E+02  0.000E+00  1.829E-05  0.000E+00  1.724E-05  0.000E+00  1.253E-05  0.000E+00  5.942E-06  0.000E+00
            0.000E+00  0.000E+00 -3.027E-06  0.000E+00 -1.984E-06  0.000E+00  2.731E-06  0.000E+00  9.317E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  9.930E-06 -2.791E-05  9.289E-06 -4.451E-05  3.242E-05 -2.388E-05  2.796E-05 -3.331E-06  1.026E-05
            1.526E-05  1.386E-05  4.429E-05  2.325E-05  4.632E-05  3.242E-05  5.259E-05  4.812E-05  6.325E-05  4.044E-05
 CMPLX XFORM
           -5.506E+02  1.945E-05  5.389E-05  5.192E-05  5.339E-05  3.815E-05  1.234E-05  2.461E-05  4.177E-06  2.948E-05
            0.000E+00  1.835E-05  4.909E-06  2.722E-06 -1.122E-05  0.000E+00 -2.398E-05 -1.350E-05 -3.245E-05 -1.859E-05
 CMPLX XFORM
           -4.000E+02  0.000E+00  9.317E-06  0.000E+00  2.731E-06  0.000E+00 -1.984E-06  0.000E+00 -3.027E-06  0.000E+00
            0.000E+00  0.000E+00  5.942E-06  0.000E+00  1.253E-05  0.000E+00  1.724E-05  0.000E+00  1.829E-05  0.000E+00
 CMPLX XFORM
           -4.000E+02  0.000E+00  1.829E-05  0.000E+00  1.724E-05  0.000E+00  1.253E-05  0.000E+00  5.942E-06  0.000E+00
            0.000E+00  0.000E+00 -3.027E-06  0.000E+00 -1.984E-06  0.000E+00  2.731E-06  0.000E+00  9.317E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  4.356E-06 -3.133E-05 -3.541E-06 -4.524E-05  2.098E-05 -2.752E-05  1.790E-05 -5.296E-06 -7.057E-06
            1.526E-05 -3.451E-06  4.008E-05  1.319E-05  3.179E-05  2.098E-05  4.098E-05  3.529E-05  5.758E-05  3.487E-05
 CMPLX XFORM
           -2.906E+02  1.070E-05  3.006E-05  4.040E-05  2.708E-05  3.815E-05 -1.044E-05  2.760E-05 -1.656E-05  2.970E-05
            0.000E+00  1.813E-05  2.110E-05 -2.692E-07  1.509E-05  0.000E+00 -1.212E-06 -1.982E-06 -1.935E-05 -9.837E-06
 CMPLX XFORM
           -4.000E+02  0.000E+00  4.659E-06  0.000E+00  1.366E-06  0.000E+00 -9.921E-07  0.000E+00 -1.514E-06  0.000E+00
            0.000E+00  0.000E+00  2.971E-06  0.000E+00  6.264E-06  0.000E+00  8.621E-06  0.000E+00  9.143E-06  0.000E+00
 CMPLX XFORM
           -2.038E+02  0.000E+00  9.143E-06  0.000E+00  8.621E-06  0.000E+00  6.264E-06  0.000E+00  2.971E-06  0.000E+00
            0.000E+00  0.000E+00 -1.514E-06  0.000E+00 -9.921E-07  0.000E+00  1.366E-06  0.000E+00  4.659E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  1.474E-05 -1.249E-05 -8.030E-07 -2.113E-05  1.907E-05 -1.120E-05  1.253E-05 -4.059E-06 -1.963E-05
            0.000E+00 -9.408E-06  2.813E-05  1.616E-05  1.664E-05  1.144E-05  1.568E-05  1.734E-05  3.420E-05  3.393E-05
 CMPLX XFORM
           -1.300E+02  8.913E-06  6.704E-06  2.245E-05  6.039E-06  3.242E-05 -1.117E-05  3.057E-05 -1.960E-05  1.841E-05
            7.629E-06  5.560E-06  3.376E-05 -8.366E-07  2.906E-05 -1.907E-06  1.040E-05 -1.693E-06  2.018E-06  5.464E-07
 CMPLX XFORM
           -4.000E+02  0.000E+00  1.296E-05  0.000E+00  8.627E-06  0.000E+00  3.911E-06  0.000E+00  6.163E-07  0.000E+00
            0.000E+00  0.000E+00  2.298E-06  0.000E+00  6.632E-06  0.000E+00  1.135E-05  0.000E+00  1.464E-05  0.000E+00
 CMPLX XFORM
           -6.335E+01  0.000E+00  7.072E-06  0.000E+00  9.129E-06  0.000E+00  8.244E-06  0.000E+00  4.758E-06  0.000E+00
            0.000E+00  0.000E+00 -4.211E-06  0.000E+00 -6.268E-06  0.000E+00 -5.383E-06  0.000E+00 -1.897E-06  0.000E+00
 CMPLX XFORM
           -4.000E+02  0.000E+00  1.380E-05  0.000E+00  9.987E-06  0.000E+00  5.272E-06  0.000E+00  1.457E-06  0.000E+00
            0.000E+00  0.000E+00  1.457E-06  0.000E+00  5.272E-06  0.000E+00  9.987E-06  0.000E+00  1.380E-05  0.000E+00
 CMPLX XFORM
            0.000E+00  0.000E+00  4.484E-06  0.000E+00  7.256E-06  0.000E+00  7.256E-06  0.000E+00  4.484E-06  0.000E+00
            0.000E+00  0.000E+00 -4.484E-06  0.000E+00 -7.256E-06  0.000E+00 -7.256E-06  0.000E+00 -4.484E-06  0.000E+00
 INVRS XFORM
            2.000E+00  3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
 INVRS XFORM
            3.000E+00  4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
 INVRS XFORM
            4.000E+00  5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
 INVRS XFORM
            5.000E+00  6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
 INVRS XFORM
            6.000E+00  7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
 INVRS XFORM
            7.000E+00  8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
 INVRS XFORM
            8.000E+00  9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
 INVRS XFORM
            9.000E+00  1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
 INVRS XFORM
            1.000E+01  1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
 INVRS XFORM
            1.100E+01  1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
 INVRS XFORM
            1.200E+01  1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01
            2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01
 INVRS XFORM
            1.300E+01  1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01
            2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01
 INVRS XFORM
            1.400E+01  1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01
            2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01
 INVRS XFORM
            1.500E+01  1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01
            2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01
 INVRS XFORM
            1.600E+01  1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01
            2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01
 INVRS XFORM
            1.700E+01  1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01
            2.700E+01  2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01
 INVRS XFORM
            1.800E+01  1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01
            2.800E+01  2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01
 INVRS XFORM
            1.900E+01  2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01
            2.900E+01  3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01
 INVRS XFORM
            2.000E+01  2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01
            3.000E+01  3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01
 INVRS XFORM
            2.100E+01  2.200E+01  2.300E+01  2.400E+01  2.500E+01  2.600E+01  2.700E+01  2.800E+01  2.900E+01  3.000E+01
            3.100E+01  3.200E+01  3.300E+01  3.400E+01  3.500E+01  3.600E+01  3.700E+01  3.800E+01  3.900E+01  4.000E+01
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
$Other_File:
$ create rft2ch.hlp
1 RFT2CH

	Ported to UNIX:  T. L. Truong			25 June 1993
	Original programmer:  J. J. Lorre		1 April 1979
	Current cognizant programmer:  C. C. Avis	1 Jan. 1984

2 PURPOSE

	To compute 1-D and 2-D real FFT's in core.  RFT2CH calls RFT2,
	DFFT, and REALTR.

2 CALL SEQUENCE
	
  FORTRAN calling sequence and parameters:

	call rft2ch(BUF,M,N,MODE)
 
   where:
   integer*4 M	- no. of lines in the data matrix to be transformed
   integer*4 N	- no. of samples in the data matrix to be transformed
   integer*4 MODE- direction of the transform:  1 = direct
					       -1 = inverse
   real*4 BUF	- contains data to be transformed.  The data should be formated
		as REAL*4 data.  The transform will be organized differently 
		for 1-D and 2-D arrays as follows:

		M = 1 (1-D):  BUF must be of length N/2 + 1 complex values.
			The transform is organized as pairs of floating point 
			values, (real,imaginary).
		M > 1 (2-D):  BUF must be of dimension (N,M+2), in the 
			FORTRAN sense, of real data values.  The odd lines 
			contain the real values and the even lines the 
			imaginery values.

  C calling sequence and parameters:

	rft2ch(BUF,M,N,MODE)

   where:
   int M	- no. of lines in the data matrix to be transformed
   int N	- no. of samples in the data matrix to be transformed
   int MODE	- direction of the transform:  1 = direct
					      -1 = inverse
   float *BUF	-  contains data to be transformed.  The data should be 
		formated as floating point data.  The transform will be 
		organized differently for 1-D and 2-D rays as follows:

                M = 1 (1-D):  BUF must be of length N/2 + 1 complex values.
                        The transform is organized as pairs of floating point 
                        values, (real,imaginary).
		M > 1 (2-D):  BUF must be of dimension [M+2] [N], in the 
                        C sense, of real data values.

2 METHOD

	The data to be transformed as well as the inverse-transformed data
	comprises the first N fullword in the 1-D case, and the first NxM
	fullwords in the 2-D case.  When the inverse transform is computed,
	the data will be scaled slightly differently.  In order to return the
	data to its original scale, one must divide it by the following values:
 	  1-D :  2*N
	  2-D :  2*N*M
	N and M need not be powers of 2.  For more details, see RFT2 help.

$ Return
$!#############################################################################
