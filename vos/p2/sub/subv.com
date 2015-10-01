$!****************************************************************************
$!
$! Build proc for MIPL module subv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:37:13
$!
$! Execute by entering:		$ @subv
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
$ write sys$output "*** module subv ***"
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
$ write sys$output "Invalid argument given to subv.com file -- ", primary
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
$   if F$SEARCH("subv.imake") .nes. ""
$   then
$      vimake subv
$      purge subv.bld
$   else
$      if F$SEARCH("subv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake subv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @subv.bld "STD"
$   else
$      @subv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create subv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack subv.com -mixed -
	-s subv.c -
	-i subv.imake -
	-t tsubv.f tzsubv.c tsubv.imake tsubv.pdf tstsubv.pdf -
	-o subv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create subv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            SUBV

	General routine for subtracting arrays.  Array B is replaced with the
	product of subtracting B from A.  A and B can be of different data 
        types as indicated

	DCODE......Data types
	           =1,   A is byte         B is byte
	           =2,   A is halfword     B is halfword
	           =3,   A is byte         B is halfword
                   =4,   A is fullword     B is fullword
	           =5,   A is byte         B is fullword
                   =6,   A is halfword     B is fullword
	           =7,   A is real(single) B is real
	           =8,   A is double       B is double
                   =9,   A is real         B is double
                  negative values -1 to -9 reverse of above
*/    
/*                     ADAPTED FROM ASU VERSION  */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdint.h>


/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(subv, SUBV) (dcode, n, avec, bvec, inca, incb)
     int *dcode, *n, *inca, *incb;
     void *avec, *bvec;
{
   zsubv( *dcode, *n, avec, bvec, *inca, *incb);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zsubv(dcode, n, avec, bvec, inca, incb)
   int dcode, n, inca, incb;
   void *avec, *bvec;
{
  int i;
  
  /* vectors */
  uint8_t *bytein,   *byteout;
  int16_t         *halfin,   *halfout;
  int32_t          *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = *byteout - *bytein;
    }
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb){
      *halfout = *halfout - *halfin;
    }
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb){
      *byteout = *byteout - *halfin ;
    }
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = *halfout - *bytein ;
    }
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullout - *fullin;
    }
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = *byteout - *fullin;
    }
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *fullout - *bytein;
    }
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = (int16_t *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = *halfout - *fullin;
    }
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *fullout - *halfin; 
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< n; i++, realin+=inca,realout+=incb){
      *realout = *realout - *realin ;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,doubleout+=incb){
      *doubleout = *doubleout - *doublein ;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,realout+=incb){
      *realout = *realout - *doublein;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++,realin+=inca,doubleout+=incb){
      *doubleout = *doubleout - *realin; 
    }
    break;
  default:    
    zvmessage("*** SUBV - Illegal DCODE","");
    zabend();
    break;
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create subv.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY subv

   To Create the build file give the command:

	$ vimake subv                     (VMS)
   or
	% vimake subv                     (Unix)


*************************************************************************/

#define SUBROUTINE subv

#define MODULE_LIST subv.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tsubv.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE SUBV                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL XVMESSAGE(' ',' ')
      CALL PARTA    !FOR DCODE > 0

      CALL XVMESSAGE(' ',' ')
      CALL PARTB    !FOR DCODE < 0

      RETURN
      END

      SUBROUTINE PARTA    !DCODE > 0

      include  'fortport'  ! DEFINES INT2BYTE

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B1(5), B2(5)        !BYTE ARRAYS
      INTEGER*2  H1(5), H2(5)        !HALFWORD ARRAYS
      INTEGER*4  F1(5), F2(5)        !FULLWORD ARRAYS
      REAL*4     R1(5), R2(5)        !REAL ARRAYS
      REAL*8     D1(5), D2(5)        !DOUBLE WORD ARRAYS
      INTEGER    DCODE(9) /1,2,3,4,5,6,7,8,9/
C--LABEL CONSTANTS
      CHARACTER  R*4  /'REAL'/
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/
      CHARACTER  F*4  /'FULL'/
      CHARACTER  D*4  /'DOUB'/ 

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 2
         B1(I) = INT2BYTE(K1)
         B2(I) = INT2BYTE(K2)
         H1(I) = K1
         H2(I) = K2
         F1(I) = K1
         F2(I) = K2
         R1(I) = K1
         R2(I) = K2
         D1(I) = R1(I)
         D2(I) = R2(I)
      ENDDO

      CALL SUBV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL SUBV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD,' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL SUBV(DCODE(3), 5, B1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(3), B, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL SUBV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL SUBV(DCODE(5), 5, B1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(5), B, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL SUBV(DCODE(6), 5, H1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(6), H, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL SUBV(DCODE(7), 5, R1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(7), R, R, R2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES R2
            R2(I) = 2
         ENDDO

      CALL SUBV(DCODE(8), 5, D1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(8), D, D, D2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES D2
            D2(I) = 2
         ENDDO

      CALL SUBV(DCODE(9), 5, R1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(9), R, D, D2
      CALL XVMESSAGE(CARD, ' ')

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
 110  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5F5.1)
      END

      SUBROUTINE PARTB    !DCODE < 0

      include  'fortport'  ! DEFINES INT2BYTE

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B1(5), B2(5)        !BYTE ARRAYS
      INTEGER*2  H1(5), H2(5)        !HALFWORD ARRAYS
      INTEGER*4  F1(5), F2(5)        !FULLWORD ARRAYS
      REAL*4     R1(5), R2(5)        !REAL ARRAYS
      REAL*8     D1(5), D2(5)        !DOUBLE WORD ARRAYS
      INTEGER    DCODE(9)   /-1,-2,-3,-4,-5,-6,-7,-8,-9/
C--LABEL CONSTANTS
      CHARACTER  R*4  /'REAL'/
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/
      CHARACTER  F*4  /'FULL'/
      CHARACTER  D*4  /'DOUB'/ 

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 2
         B1(I) = INT2BYTE(K1)
         B2(I) = INT2BYTE(K2)
         H1(I) = K1
         H2(I) = K2
         F1(I) = K1
         F2(I) = K2
         R1(I) = K1
         R2(I) = K2
         D1(I) = R1(I)
         D2(I) = R2(I)
      ENDDO

      CALL SUBV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL SUBV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL SUBV(DCODE(3), 5, H1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(3), H, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B1
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL SUBV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL SUBV(DCODE(5), 5, F1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(5), F, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL SUBV(DCODE(6), 5, F1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(6), F, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL SUBV(DCODE(7), 5, R1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(7), R, R, R2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES R2
            R2(I) = 2
         ENDDO

      CALL SUBV(DCODE(8), 5, D1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(8), D, D, D2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES D2
            D2(I) = 2
         ENDDO

      CALL SUBV(DCODE(9), 5, D1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(9), D, R, R2
      CALL XVMESSAGE(CARD, ' ')


      CALL TZSUBV      ! TEST C INTERFACE

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
 110  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5F5.1)
      END
$!-----------------------------------------------------------------------------
$ create tzsubv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzsubv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,64}, 
                  c[2] = {10,3};

  static float bf[2] = {5.0,64.0}, cf[2] = {10.0,3.0};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zsubv( 1,2, b, c, 1,1);   /*  subtract b from c */

      sprintf( pbuf, "Output from zsubv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   5    195","");

      zsubv( 7,2, bf, cf, 1,1);   /*  subtract bf from cf */

      sprintf( pbuf, "Output from zsubv = %f   %f", cf[0], cf[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   5.000000    -61.000000","");
}
$!-----------------------------------------------------------------------------
$ create tsubv.imake
/* Imake file for Test of VICAR subroutine subv */

#define PROGRAM tsubv

#define MODULE_LIST tsubv.f tzsubv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define FTNINC_LIST fortport
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tsubv.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstsubv.pdf


procedure
refgbl $autousage
refgbl $echo
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
! This is a test of the subroutine SUBV which subtracts
! one array from another which may be of different types
! (byte, halfword, fullword, real, or double
! precision). The product the two arrays is placed in 
! the second array at that point.
tsubv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create subv.hlp
1 SUBV

PURPOSE: Subtracts the elements of two arrays

USAGE:   CALL SUBV(DCODE, N, A, B [,AINC, BINC])

PARAMETERS:

      DCODE= Data types for A and B (See table next page)
      A    = Input array
      B    = Input array. 
             On output B(i) = B(i)-A(i)   [i=1,..,N]
      N    = The number of elements in A and B
      AINC = Sorce vector address increment
      BINC = Destination increment



 
         TABLE OF AVAILABLE DATA TYPES FOR A AND B 
       ====================================================
       DCODE   A     B                DCODE   A     B 
       ====================================================
        1     byte  byte               6     half  full
        2     half  half               7     real  real
        3     byte  half               8     doub  double
        4     full  full               9     real  double
        5     byte  full
       ====================================================         
       Negative values  of DCODE (-1,-9) reverse data types
       for A and B

2 NOTES

HISTORY

  Origional Programmer: G.M. Yagi  02/04/75
  Converted to Vax by:  L.W. Kamp  12/15/82
  Current Cog Progr:    L.W. Kamp
  Ported to Unix:       D.D. Knight 9/15/93

DESCRIPTION

  This MACRO routine provides the FORTRAN programmer with a function to
  subtract the elements of two given arrays A and B. On output B(i) =
  B(i)-A(i). The two arrays may be any of the data types listed in the
  table for DCODE, which controls the assumed combination of data types.




  The calling sequence:

       CALL SUB(DCODE, N, A, B, AINC, BINC)

  Is equivalent to the following code:


       I = 1
       J = 1
       DO K=1,N
          B(J) = B(J)-A(I)
          I    = I + AINC
          J    = J + BINC
       ENDDO

  Where all arguements are INTEGER*4 except for the arrays whose
  data types are controlled by the data code DCODE

$ Return
$!#############################################################################
