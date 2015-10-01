$!****************************************************************************
$!
$! Build proc for MIPL module divv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:11:31
$!
$! Execute by entering:		$ @divv
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
$ write sys$output "*** module divv ***"
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
$ write sys$output "Invalid argument given to divv.com file -- ", primary
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
$   if F$SEARCH("divv.imake") .nes. ""
$   then
$      vimake divv
$      purge divv.bld
$   else
$      if F$SEARCH("divv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake divv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @divv.bld "STD"
$   else
$      @divv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create divv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack divv.com -mixed -
	-s divv.c -
	-i divv.imake -
	-t tdivv.f tzdivv.c tdivv.imake tdivv.pdf tstdivv.pdf -
	-o divv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create divv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            DIVV

	General routine for dividing arrays. B(J) is replaced with 
	B(J)/A(I). A and B can be of different data types as indicated

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
#include <zvproto.h>
#include <stdint.h>


/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zdivv(dcode, n, avec, bvec, inca, incb)
   int dcode, n, inca, incb;
   void *avec, *bvec;
{
  int i;
  
  /* vectors */
  uint8_t *bytein,   *byteout;
  int16_t         *halfin,   *halfout;
  int32_t         *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = *byteout / *bytein;
    }
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb){
      *halfout = *halfout / *halfin;
    }
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb){
      *byteout = (int)(*byteout) / *halfin;
    }
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = *halfout / (int)(*bytein);
    }
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullout / *fullin;
    }
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = (int)(*byteout) / *fullin;
    }
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *fullout / (int)(*bytein);
    }
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = (int16_t *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = *halfout / *fullin;
    }
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *fullout / *halfin;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< n; i++, realin+=inca,realout+=incb){
      *realout = *realout / *realin;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,doubleout+=incb){
      *doubleout = *doubleout / *doublein;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,realout+=incb){
      *realout = *realout / *doublein;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++,realin+=inca,doubleout+=incb){
      *doubleout = *doubleout / *realin;
    }
    break;
  default:    
    zvmessage("*** DIVV - Illegal DCODE","");
    zabend();
    break;
  }
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(divv, DIVV) (dcode, n, avec, bvec, inca, incb)
     int *dcode, *n, *inca, *incb;
     void *avec, *bvec;
{
   zdivv( *dcode, *n, avec, bvec, *inca, *incb);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create divv.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY divv

   To Create the build file give the command:

	$ vimake divv                     (VMS)
   or
	% vimake divv                     (Unix)


*************************************************************************/

#define SUBROUTINE divv

#define MODULE_LIST divv.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tdivv.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE DIVV                                  C
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
         K1    = 2
         K2    = 2*I
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

      CALL DIVV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2*I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL DIVV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD,' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(3), 5, B1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(3), B, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(5), 5, B1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(5), B, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(6), 5, H1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(6), H, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(7), 5, R1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(7), R, R, R2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES R2
            R2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(8), 5, D1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(8), D, D, D2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES D2
            D2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(9), 5, R1, D2, 1, 1)
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
         K1    = 2
         K2    = 2*I
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

      CALL DIVV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2*I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL DIVV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(3), 5, H1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(3), H, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B1
            K2    = 2*I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL DIVV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(5), 5, F1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(5), F, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2*I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL DIVV(DCODE(6), 5, F1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(6), F, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(7), 5, R1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(7), R, R, R2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES R2
            R2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(8), 5, D1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(8), D, D, D2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES D2
            D2(I) = 2*I
         ENDDO

      CALL DIVV(DCODE(9), 5, D1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(9), D, R, R2
      CALL XVMESSAGE(CARD, ' ')


      CALL TZDIVV      ! TEST C INTERFACE

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
 110  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5F5.1)
      END
$!-----------------------------------------------------------------------------
$ create tzdivv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzdivv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {10,1}, 
                  c[2] = {5,255};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zdivv( 1,2, b, c, 1,1);   /*  divide c by b*/

      sprintf( pbuf, "Output from zdivv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   0    255","");
}
$!-----------------------------------------------------------------------------
$ create tdivv.imake
/* Imake file for Test of VICAR subroutine divv */

#define PROGRAM tdivv

#define MODULE_LIST tdivv.f tzdivv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tdivv.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstdivv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "THIS IS A TEST OF MODULE DIVV"
tdivv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create divv.hlp
1 DIVV

PURPOSE: Divides the elements of two arrays

 Fortran calling sequence:  CALL DIVV(DCODE, N, A, B, INCA, INCB)
 C Calling Sequence:        zdivv(dcode, n, a, b, inca, incb);
 
  INCA and INCB are REQUIRED arguments. (dcode, n, inca, and incb are
  passed by value for zdivv.)

PARAMETERS:

      DCODE= Data types for A and B (See table next page)
      A    = Input array
      B    = Input and output array. 
             On output B(i) = B(i)/A(i)   [i=1,..,N]
      N    = The number of elements in A and B
      INCA   (input, integer)           A array index increment
      INCB   (input, integer)           B array index increment



 
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
  Original Programmer:  G.M. Yagi  02/04/75
  Converted to Vax by:  L.W. Kamp  12/15/82
  Current Cog Progr:    L.W. Kamp
  Ported to UNIX:       Steve Pohorsky

 REVISION HISTORY:                                          
   3-26-93  ..SP....  Made portable for UNIX - Adapted from ASU version.
                      Added zdivv for calls from C.  
                      Changed OPTIONAL PARAMETERS to REQUIRED PARAMETERS
                      FOR PORTABILITY.
   9-9-93   ..SP....  Casted unsigned char to int in a few places where
                      ANSI C compiler was giving a warning. (See SPARC
                      Compiler C Transition Guide.)

DESCRIPTION

  This routine divides the elements of two given arrays A and B.  On output  
  B(i) = B(i)/A(i). The two arrays may be any of the data types listed in the
  table for DCODE, which controls the assumed combination of data types.
  NO checking for division by zero is done.


  The calling sequence:

       CALL DIVV(DCODE, N, A, B, INCA, INCB)

  Is equivalent to the following code:

       I = 1
       J = 1
       DO K=1,N
          B(J) = B(J)/A(I)
          I    = I + INCA
          J    = J + INCB
       ENDDO

  Where all arguments are INTEGER*4 except for the arrays whose
  data types are controlled by the data code DCODE.
$ Return
$!#############################################################################
