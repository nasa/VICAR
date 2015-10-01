$!****************************************************************************
$!
$! Build proc for MIPL module xorv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:42:51
$!
$! Execute by entering:		$ @xorv
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
$ write sys$output "*** module xorv ***"
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
$ write sys$output "Invalid argument given to xorv.com file -- ", primary
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
$   if F$SEARCH("xorv.imake") .nes. ""
$   then
$      vimake xorv
$      purge xorv.bld
$   else
$      if F$SEARCH("xorv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xorv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xorv.bld "STD"
$   else
$      @xorv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xorv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xorv.com -mixed -
	-s xorv.c -
	-i xorv.imake -
	-t txorv.f tzxorv.c txorv.imake txorv.pdf tstxorv.pdf -
	-o xorv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xorv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            XORV

	General routine for XOR-ing two arrays.  Array B is replaced with the
	exclusive-OR of A and B.  A and B can be of different data types as 
        indicated.

	DCODE......Data types
	           =1,   A is byte         B is byte
	           =2,   A is halfword     B is halfword
	           =3,   A is byte         B is halfword
                   =4,   A is fullword     B is fullword
	           =5,   A is byte         B is fullword
                   =6,   A is halfword     B is fullword
                  negative values -1 to -6 reverse of above
*/    
/*                     ADAPTED FROM MULV */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdint.h>


/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(xorv, XORV) (dcode, n, avec, bvec, inca, incb)
     int *dcode, *n, *inca, *incb;
     void *avec, *bvec;
{
   zxorv( *dcode, *n, avec, bvec, *inca, *incb);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zxorv(dcode, n, avec, bvec, inca, incb)
   int dcode, n, inca, incb;
   void *avec, *bvec;
{
  int i;
  
  /* vectors */
  uint8_t *bytein,   *byteout;
  int16_t         *halfin,   *halfout;
  int32_t          *fullin,   *fullout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = *bytein ^ *byteout;
    }
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb){
      *halfout = *halfin ^ *halfout;
    }
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb){
      *byteout = *halfin ^ *byteout ;
    }
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = *bytein ^ *halfout ;
    }
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullin ^ *fullout;
    }
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = *fullin ^ *byteout;
    }
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *bytein ^ *fullout;
    }
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = (int16_t *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = *fullin ^ *halfout;
    }
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *halfin ^ *fullout;
    }
    break;
  default:    
    zvmessage("*** XORV - Illegal DCODE","");
    zabend();
    break;
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xorv.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY xorv

   To Create the build file give the command:

	$ vimake xorv                     (VMS)
   or
	% vimake xorv                     (Unix)


*************************************************************************/

#define SUBROUTINE xorv

#define MODULE_LIST xorv.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create txorv.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE XORV                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL XVMESSAGE('Output values for this test should be 128',' ')
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
      INTEGER    DCODE(6) /1,2,3,4,5,6/
C--LABEL CONSTANTS
      CHARACTER  F*4  /'FULL'/
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 128+K1
         B1(I) = INT2BYTE(K1)
         B2(I) = INT2BYTE(K2)
         H1(I) = K1
         H2(I) = K2
         F1(I) = K1
         F2(I) = K2
      ENDDO

      CALL XORV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD,' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(3), 5, B1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(3), B, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(5), 5, B1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(5), B, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(6), 5, H1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(6), H, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO


      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
      END

      SUBROUTINE PARTB    !DCODE < 0

      include  'fortport'  ! DEFINES INT2BYTE

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B1(5), B2(5)        !BYTE ARRAYS
      INTEGER*2  H1(5), H2(5)        !HALFWORD ARRAYS
      INTEGER*4  F1(5), F2(5)        !FULLWORD ARRAYS
      REAL*4     R1(5), R2(5)        !REAL ARRAYS
      REAL*8     D1(5), D2(5)        !DOUBLE WORD ARRAYS
      INTEGER    DCODE(6)   /-1,-2,-3,-4,-5,-6/
C--LABEL CONSTANTS
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/
      CHARACTER  F*4  /'FULL'/

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 128+I
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

      CALL XORV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(3), 5, H1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(3), H, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B1
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(5), 5, F1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(5), F, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(6), 5, F1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(6), F, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO


      CALL TZXORV      ! TEST C INTERFACE

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
      END
$!-----------------------------------------------------------------------------
$ create tzxorv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzxorv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,255}, 
                  c[2] = {10,3};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zxorv( 1,2, b, c, 1,1);   /*  XOR b and c*/

      sprintf( pbuf, "Output from zxorv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   15    252","");
}
$!-----------------------------------------------------------------------------
$ create txorv.imake
/* Imake file for Test of VICAR subroutine xorv */

#define PROGRAM txorv

#define MODULE_LIST txorv.f tzxorv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define FTNINC_LIST fortport
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create txorv.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstxorv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!this is a test of module XORV
!An 'exclusive or' will be done on the listed constants A & B.
!Various data type combinations will be tested.
txorv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create xorv.hlp
1  XORV

2  PURPOSE

	General routine for "xor"-ing arrays.

2  CALLING SEQUENCE

 Fortran calling sequence:  CALL XORV(DCODE, N, A, B, INCA, INCB)
 C Calling Sequence:        zxorv(dcode, n, a, b, inca, incb);
 
  INCA and INCB are REQUIRED arguments. (dcode, n, inca, and incb are
  passed by value for zxorv.)


  Arguments:

    DCODE  (input, integer)           data format code (See below.)
    N      (input, integer)           number of elements
    A      (input, DCODE data type)   first input array
    B      (in/out, DCODE data type)  second input array and output array
             On output B(i) = XOR(B(i),A(i))   [i=1,..,N] for case of 
                                                          INCA=1=INCB.
    INCA   (input, integer)           A array index increment
    INCB   (input, integer)           B array index increment

	DCODE      Data types

                   ..INPUT..         ..OUTPUT..

                   =1,   A is byte         B is byte
                   =2,   A is halfword     B is halfword
                   =3,   A is byte         B is halfword
                   =4,   A is fullword     B is fullword
                   =5,   A is byte         B is fullword
                   =6,   A is halfword     B is fullword
                   negative values -1 to -6 reverse of above

2  HISTORY

        Original Programmer: Lucas Kamp, 83-7-4
        Current Cognizant Programmer: Steve Pohorsky
        Source Language: C
        Ported to UNIX:       Steve Pohorsky 3-30-93

 REVISION HISTORY:                                          
   3-30-93  ..SP....  Made portable for UNIX - Adapted from mulv.
                      Added zxorv for calls from C.  Changed name from
                      xor to xorv (added v for most of the vector routines).
                      ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.
                      Replaced test program for portability.
2 OPERATION

        Array B is replaced with XOR(A,B). A and B can be of 
        different data types as indicated.

  The calling sequence:

       CALL XORV(DCODE, N, A, B, INCA, INCB)

  Is equivalent to the following code:


       I = 1
       J = 1
       DO K=1,N
          B(J) = ieor( B(J),A(I))
          I    = I + INCA
          J    = J + INCB
       ENDDO

  Where all arguments are INTEGER, except for the arrays whose
  data types are controlled by the data code DCODE.

$ Return
$!#############################################################################
