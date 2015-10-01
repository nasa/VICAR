$!****************************************************************************
$!
$! Build proc for MIPL module shfv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:36:13
$!
$! Execute by entering:		$ @shfv
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
$ write sys$output "*** module shfv ***"
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
$ write sys$output "Invalid argument given to shfv.com file -- ", primary
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
$   if F$SEARCH("shfv.imake") .nes. ""
$   then
$      vimake shfv
$      purge shfv.bld
$   else
$      if F$SEARCH("shfv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake shfv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @shfv.bld "STD"
$   else
$      @shfv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create shfv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack shfv.com -mixed -
	-s shfv.c -
	-i shfv.imake -
	-t tshfv.f tzshfv.c tshfv.imake tshfv.pdf tstshfv.pdf -
	-o shfv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create shfv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*---------------------------  shfv     ------------------------
 * SHFV  (SHiFt Vector)
 *
 * Performs an arithmetic shift on each element of an array
 *
 * dcode   - Data type code. (See below.)
 * n       - number of data elements to transfer
 * shift   - number of bits to shift left(pos.) or right(neg.)
 * buf     - input vector of original (source) data;
 *           On output contains shifted bits.
 * inc     - source vector index increment
 *

 The data type (dcode) of array a may  be one of the following:

        1=BYTE        2=INTEGER*2     4=INTEGER*4

 REVISION HISTORY:                                          
   92-5-18  ..SP....  Made portable for UNIX - Adapted version from ASU.
   92-5-21  ..SP....  Added extra code to handle shifts of +/-32... since
                      these are undefined in ANSI C.  Unfortunately this
                      makes the code dependent on the word size, but I
                      guess that is what the above DCODE types are.
--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"

/*  Values of dcode - format of data in BUF buffer.  */

#define BYTE   1
#define HALF   2
#define FULL   4

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(shfv, SHFV) (dcode, n, shift, buf, inc)
   int *dcode,*n,*shift,*buf,*inc;
{
   zshfv( *dcode, *n, *shift, buf, *inc);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zshfv(dcode, n, shift, buf, inc)
   int dcode,n,shift,*buf, inc;
{

  switch (dcode) {
    case BYTE:	byte_shift(n,shift,inc, (unsigned char *)buf);
		break;
    case HALF:	half_shift(n,shift,inc, (short *)buf);
		break;                  
    case FULL:	full_shift(n,shift,inc, (int *)buf);
		break;
    default:    zvmessage("*** SHFV - Illegal DCODE","");
                zabend();
                break;
  }
  return;
}
/*  ##################################################################  */

byte_shift(n,shift,inc,bbuf)
     int n, shift, inc;
     unsigned char *bbuf;
{
  register int i;

/*  =================================  */
  if (shift < -7){
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = 0;
  }
  else if (shift < 0){
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = *bbuf >> (-shift);
  }
  else if (shift > 7){
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = 0;
  }
  else if (shift > 0) {
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = *bbuf << (shift);
  }
}       /*  0 shift means zero work.  */


/*  #################################################################  */

half_shift(n,shift,inc,hbuf)
     int n, shift, inc;
     short int *hbuf;
{
  register int i;
/*  =================================  */

  if (shift < -15){
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = *hbuf >> (15);  /* The sign bit gets dragged all the way thru*/
  }
  else if (shift < 0){
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = *hbuf >> (-shift);
  }
  else if (shift > 15) {
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = 0;
  }
  else if (shift > 0) {
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = *hbuf << (shift);
  }
}
/*  ##################################################################  */

full_shift(n,shift,inc,fbuf)
     int n, shift, inc;
     int *fbuf;
{
  register int i;
/*  =================================  */

  if (shift < -31){
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = *fbuf >> (31);
  }
  else if (shift < 0){
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = *fbuf >> (-shift);
  }
  else if (shift > 31) {
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = 0;
  }
  else if (shift > 0) {
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = *fbuf << (shift);
  }
}

  
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create shfv.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY shfv

   To Create the build file give the command:

	$ vimake shfv                     (VMS)
   or
	% vimake shfv                     (Unix)


*************************************************************************/

#define SUBROUTINE shfv

#define MODULE_LIST shfv.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tshfv.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE SHFV      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**THIS IS A TEST OF MODULE SHFV**',' ')
      CALL PARTA
      CALL PARTB       
      CALL PARTC       
      CALL TZSHFV      ! TEST C INTERFACE
      RETURN
      END

      SUBROUTINE PARTA
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  RUN THROUGH ALL OF THE DATA TYPES  C
C  I.E. BYTE, HALF AND FULL WORD,     C
C  SHIFTING LEFT ALL THE WAY          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**PARTA:  ARITHMETIC SHIFT LEFT TEST',' ')

      CALL XVMESSAGE('**SHIFT A BYTE VALUE 0 THROUGH 8 BITS',' ')
      CALL SHFVX(1, 15, 1)    ! 15 = '0000000F'X

      CALL XVMESSAGE('**SHIFT A HALFWORD VALUE 0 THROUGH 16 BITS',' ')
      CALL SHFVX(2, 15, 1)    ! 15 = '0000000F'X

      CALL XVMESSAGE('**SHIFT A FULLWORD VALUE 0 THROUGH 32 BITS',' ')
      CALL SHFVX(4, 15, 1)    ! 15 = '0000000F'X

      RETURN
      END

      SUBROUTINE PARTB
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  RUN THROUGH ALL OF THE DATA TYPES  C
C  I.E. BYTE, HALF AND FULL WORD,     C
C  SHIFTING RIGHT ALL THE WAY         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**PARTB:  ARITHMETIC SHIFT RIGHT TEST',' ')

      CALL XVMESSAGE('**SHIFT A BYTE VALUE 0 THROUGH 8 BITS',' ')
      CALL SHFVX(1, 128, -1)         ! 128 = '00000080'X

      CALL XVMESSAGE('**SHIFT A HALFWORD VALUE 0 THROUGH 16 BITS',' ')
      CALL SHFVX(2, -32768, -1)      ! -32768 = 'FFFF8000'X

      CALL XVMESSAGE('**SHIFT A FULLWORD VALUE 0 THROUGH 32 BITS',' ')
      CALL SHFVX(4, -2147483648, -1) ! -2147483648 = '80000000'X

      RETURN
      END

      SUBROUTINE PARTC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  SHIFT AN ENTIRE ARRAY OF EACH DATA TYPE AND PRINT THE RESULTS    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BYTE       B(0:15)             !BYTE VARIABLES
      INTEGER*2  H(0:15)             !HALFWORD VARIABLE
      INTEGER*4  F(0:15)             !FULLWORD VARIABLE
      CHARACTER  CARD*80            !PRINT BUFFER

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(
     .    '**PARTC: ARITHMETIC SHIFT ARRAYS OF EACH DATA TYPE',' ')

C--CREATE THE ARRAYS
      N  = 15
      DO I=0,N
         K    = I+1
         B(I) = K
         H(I) = K
         F(I) = K
      ENDDO

C--PRINT OUT ORIGIONAL VALUES TO BE SHIFTED
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**FULLWORD VERSION OF VALUES TO BE SHIFTED',' ')
 
      NBLK = N/4 + 1 
      J1 = 0
      DO J=1,NBLK            !FULLWORD VERSION DATA
         J2 = MIN(N,J1+3)
         WRITE(CARD, 120) (F(I),I=J1,J2)
         CALL XVMESSAGE(CARD, ' ')
         J1 = J1 + 4
      ENDDO

C--SHIFT BY FOUR BITS
      CALL SHFV(1, N+1, 4, B,1)   !BYTE
      CALL SHFV(2, N+1, 4, H,1)   !HALF
      CALL SHFV(4, N+1, 4, F,1)   !FULL

C--PRINT OUT THE ARRAYS IN BLOCKS OF FOUR ELEMENTS PER LINE
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**SHIFT BYTE ARRAY BY 4-BITS',' ')

      J1 = 0
      DO J=1,NBLK         !BYTE DATA
         J2 = MIN(N,J1+3)
         WRITE(CARD, 100) (B(I),I=J1,J2)
         CALL XVMESSAGE(CARD, ' ')
         J1 = J1 + 4
      ENDDO

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**SHIFT HALFWORD ARRAY BY 4-BITS',' ')

      J1 = 0
      DO J=1,NBLK           !HALFWORD DATA
         J2 = MIN(N,J1+3)
         WRITE(CARD, 110) (H(I),I=J1,J2)
         CALL XVMESSAGE(CARD, ' ')
         J1 = J1 + 4
      ENDDO

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('**SHIFT FULLWORD ARRAY BY 4-BITS',' ')

      J1 = 0
      DO J=1,NBLK            !FULLWORD DATA
         J2 = MIN(N,J1+3)
         WRITE(CARD, 120) (F(I),I=J1,J2)
         CALL XVMESSAGE(CARD, ' ')
         J1 = J1 + 4
      ENDDO

      RETURN
 100  FORMAT(4Z10.2)
 110  FORMAT(4Z10.4)
 120  FORMAT(4Z10.8)
      END

      SUBROUTINE SHFVX(ICODE, IVAL, ISHIFT)

      include  'fortport'  ! DEFINES INT2BYTE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  TAKE AN INITIAL VALUE (IVAL) AND SHIFT IT ARITHMETICALLY THROUGH C
C  A RANGE OF BIT COUNTS USING ONE OF THE GIVEN FORMATS CONTROLLED  C
C  BY (ICODE). SHIFT LEFT WHEN  ISHIFT = +1 AND SHIFT RIGHT WHEN    C
C  ISHIFT = -1. PRINT THE RESULTS. TEST INCLUDES THE PATHOLOGICAL   C
C  CASE WHERE THE BIT COUNT = 0.                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BYTE       B(0:8)               !BYTE VARIABLES
      INTEGER*2  H(0:16)              !HALFWORD VARIABLE
      INTEGER*4  F(0:32)              !FULLWORD VARIABLE
      CHARACTER  CARD*80            !PRINT BUFFER

      CALL XVMESSAGE(' ',' ')
      WRITE(CARD, 125) ICODE, IVAL, ISHIFT
      CALL XVMESSAGE(CARD, ' ')

      K     = IVAL
      N     = ICODE*8
      DO I=0,N
         IF(ICODE .EQ. 1) THEN   
            B(I) = INT2BYTE(K)
            CALL SHFV(ICODE, 1, ISHIFT*I, B(I),1) 
         ENDIF
         IF(ICODE .EQ. 2) THEN   
            H(I) = K
            CALL SHFV(ICODE, 1, ISHIFT*I, H(I),1) 
         ENDIF
         IF(ICODE .EQ. 4) THEN   
            F(I) = K
            CALL SHFV(ICODE, 1, ISHIFT*I, F(I),1) 
         ENDIF
      ENDDO

      NBLK = N/4 + 1
      J1 = 0
      DO J=1,NBLK
         J2 = MIN(N,J1+3)
         IF(ICODE .EQ. 1) WRITE(CARD, 100) (B(I),I=J1,J2)
         IF(ICODE .EQ. 2) WRITE(CARD, 110) (H(I),I=J1,J2)
         IF(ICODE .EQ. 4) WRITE(CARD, 120) (F(I),I=J1,J2)
         CALL XVMESSAGE(CARD, ' ')
         J1 = J1 + 4
      ENDDO

      RETURN
 100  FORMAT(4Z10.2)
 110  FORMAT(4Z10.4)
 120  FORMAT(4Z10.8)
 125  FORMAT('DCODE =', I3,
     :       2X, 'IVAL  =', Z10.8,
     :       2X, 'ISHIFT=', I5)
      END
$!-----------------------------------------------------------------------------
$ create tzshfv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzshfv)() 

{
  char pbuf[81];
  int b[1];

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      b[0] = -16;
      zshfv( 4, 1, -2, b, 1);   /*  shift b to the right by 2 bits.  */

      sprintf( pbuf, "Output from zshfv = %d", b[0]);
      zvmessage(pbuf, "");
      zvmessage("Correct value is -4","");
}
$!-----------------------------------------------------------------------------
$ create tshfv.imake
/* Imake file for Test of VICAR subroutine shfv */

#define PROGRAM tshfv

#define MODULE_LIST tshfv.f tzshfv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tshfv.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstshfv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tshfv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create shfv.hlp
1 SHFV

 PURPOSE: Performs an arithmetic shift on elements of an array

 FORTRAN Calling Sequence:  CALL SHFV( DCODE, N, NSHIFT, A, INC )
 C Calling Sequence:        zshfv( dcode, n, nshift, a, inc );

 PARAMETERS:

    DCODE  = Data type code for A
    N      = Number of data elements to perform shift on.
    NSHIFT = Number of bits to shift left (pos.) or right (neg.)
    A      = Input vector of original bits.
             On output contains shifted bits.
    INC    = Input vector index increment

  INC is a REQUIRED argument. (dcode, n, nshift, and inc are
  passed by value for zshfv.)


    DATA TYPES 
    =================
    DCODE     A
    =================
      1      BYTE    (unsigned interpretation)
      2      HALF    (signed interpretation)
      4      FULL    (signed interpretation)
    =================

2 NOTES

DESCRIPTION

 This routine performs an arithmetic shift (not a circulate) with
 no attention paid to bits shifted off the end.  Since DCODE 2 and 4
 involve signed interpretation, the arithmetic shift to the right brings
 copies of the most significant (sign) bit of the input value into the
 most significant bits of the output value.  Thus, if NSHIFT = -2, an
 INTEGER*4 value of 'FFFFFFF8'X (i.e. -8) would be changed to
 'FFFFFFFE'X (i.e. -2).  The arithmetic shift to the left always brings
 0s into the least significant bits of the output value.  

HISTORY

  Original Programmer: G.M. Yagi
  Converted to Vax by: L.W. Kamp
  Current Cog Progr:   S. Pohorsky
  Source Language:       C


 REVISION HISTORY:                                          
   92-5-18  ..SP....  Made portable for UNIX - Adapted version from ASU.
                      Added zshfv for calls from C.  Changed name from
                      shf to shfv (added v for most of the vector routines).
                      ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.

$ Return
$!#############################################################################
