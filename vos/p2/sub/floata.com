$!****************************************************************************
$!
$! Build proc for MIPL module floata
$! VPACK Version 1.9, Monday, December 07, 2009, 16:18:06
$!
$! Execute by entering:		$ @floata
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
$ write sys$output "*** module floata ***"
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
$ write sys$output "Invalid argument given to floata.com file -- ", primary
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
$   if F$SEARCH("floata.imake") .nes. ""
$   then
$      vimake floata
$      purge floata.bld
$   else
$      if F$SEARCH("floata.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake floata
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @floata.bld "STD"
$   else
$      @floata.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create floata.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack floata.com -mixed -
	-s floata.c -
	-i floata.imake -
	-t tfloata.f tzfloata.c tfloata.imake tfloata.pdf tstfloata.pdf -
	-o floata.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create floata.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                      FLOATA

	General routine for converting a byte or integer array
        to REAL*4. Note the complementary routine CONVRT.
        Byte, halfword or longword data can be converted.

	Fortran format of call:

	CALL FLOATA(DCODE,N,IBUF,RBUF)

	Parameters:

	DCODE ... Input data type.
                  1 = Byte data.
	          2 = Integer*2
                  4 = Integer*4
	N     ... Number of elements to be converted
	IBUF  ... Input array containing data.
	RBUF  ... REAL*4 array for result.
*/    
#include "xvmaininc.h"
#include "applic.h"
#include <zvproto.h>
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zfloata(dcode, n, ibuf, rbuf)
   int dcode, n;
   void *ibuf;
   float *rbuf;
{
  static int first = 1;
  static int dcodesav;
  static int tbuf[12];
         int stat;

/*  ==================================================================  */

 if ( first || dcode != dcodesav) {
  first = 0;         /*  call zvtrans_set first time or if dcode changes  */
  dcodesav = dcode;

  switch (dcode) {
  case 1:
    stat = zvtrans_set(tbuf, "BYTE", "REAL");
    if (stat != SUCCESS)   zmabend("FLOATA -Programming error");
    break;
  case 2:
    stat = zvtrans_set(tbuf, "HALF", "REAL");
    if (stat != SUCCESS)   zmabend("FLOATA -Programming error");
    break;
  case  4:
    stat = zvtrans_set(tbuf, "FULL", "REAL");
    if (stat != SUCCESS)   zmabend("FLOATA -Programming error");
    break;
  default:    
    zvmessage("*** FLOATA - Illegal DCODE","");
    zabend();
    break;
  }
 }
 zvtrans(tbuf, ibuf, rbuf, n);   /*  convert from int to float  */
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(floata, FLOATA) (dcode, n, ibuf, rbuf)
     int *dcode, *n;
     void *ibuf;
     float *rbuf;
{
   zfloata( *dcode, *n, ibuf, rbuf);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create floata.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY floata

   To Create the build file give the command:

	$ vimake floata                     (VMS)
   or
	% vimake floata                     (Unix)


*************************************************************************/

#define SUBROUTINE floata

#define MODULE_LIST floata.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tfloata.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      include  'fortport'  ! DEFINES INT2BYTE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE FLOATA                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B(5)                !BYTE ARRAY
      INTEGER*2  H(5)                !HALFWORD ARRAY
      INTEGER*4  F(5)                !FULLWORD ARRAY
      REAL*4     R(5)                !REAL ARRAYS

      CALL XVMESSAGE('**BEGIN TEST FOR MODULE FLOATA',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(
     . 'FLOATA should generate 0.0 50.0 100.0 150.0 200.0',' ')
      CALL XVMESSAGE(' ',' ')
      DO I=1,5          !INITIALIZE THE INPUT ARRAY
         K    = (I-1)*50
         B(I) = INT2BYTE(K)
         H(I) = K
         F(I) = K
      ENDDO

      CALL XVMESSAGE(' ',' ')
      CALL FLOATA(1, 5, B, R)
      WRITE( CARD, 100) (R(I),I=1,5)
      CALL XVMESSAGE(CARD, ' ')

      CALL XVMESSAGE(' ',' ')
      CALL FLOATA(2, 5, H, R)
      WRITE( CARD, 100) (R(I),I=1,5)
      CALL XVMESSAGE(CARD, ' ')

      CALL XVMESSAGE(' ',' ')
      CALL FLOATA(4, 5, F, R)
      WRITE( CARD, 100) (R(I),I=1,5)
      CALL XVMESSAGE(CARD, ' ')

      CALL TZFLOATA      ! TEST C INTERFACE

      CALL XVMESSAGE('**END   TEST FOR MODULE FLOATA',' ')
      RETURN
 100  FORMAT(5F6.1)
      END
$!-----------------------------------------------------------------------------
$ create tzfloata.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzfloata)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,255};
  float c[2];

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zfloata( 1,2, b, c);   /*  convert b to float, storing in c*/

      sprintf( pbuf, "Output from zfloata = %f   %f", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   5.0   255.0","");
}
$!-----------------------------------------------------------------------------
$ create tfloata.imake
/* Imake file for Test of VICAR subroutine floata */

#define PROGRAM tfloata

#define MODULE_LIST tfloata.f tzfloata.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define FTNINC_LIST fortport
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tfloata.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstfloata.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
WRITE "TEST MODULE FLOATA"
tfloata
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create floata.hlp
1 FLOATA

PURPOSE: Converts byte, INTEGER*2 or INTEGER*4 arrays to REAL*4. This
         is a complementary function to CONVRT.

2  CALLING SEQUENCE

 Fortran calling sequence:  CALL FLOATA(DCODE, N, IBUF, RBUF)
 C Calling Sequence:        zfloata(dcode, n, ibuf, rbuf);
 
  (dcode, and n are passed by value for zfloata.)

 PARAMETERS:

      DCODE = Data type for IBUF (See below.)
      N     = Number of elements to be converted
      IBUF  = Input array containing byte or integer data
              to be converted.
      RBUF  = Output array containing REAL*4 result.

       DATA TYPES 
       ============
       DCODE  IBUF
       ============
        1     BYTE
        2     HALF
        4     FULL
       ============

2 NOTES

HISTORY

  Original Programmer:  G.M. Yagi  01/17/77
  Converted to Vax by:  L.W. Kamp  12/15/82
  Current Cognizant Programmer: Steve Pohorsky
  Source Language: C
  Ported to UNIX:       Steve Pohorsky 4-1-93

 REVISION HISTORY:                                          
   4-1-93  ..SP....   Made portable for UNIX; Added zfloata for calls from C.
                      Source code is all new - based on zvtrans ala Porting
                      Guide.
DESCRIPTION

  This routine provides the programmer with a function to
  convert a BYTE, INTEGER*2 or INTEGER*4 array to equivalent REAL*4 data.
  The input data type is controlled by the DCODE parameter.  FLOATA calls
  the VICAR run-time library routine zvtrans to perform the conversion.
  (FLOATA calls zvtrans_set the first time it is called or when the value
  of dcode changes.)


  The calling sequence:

       CALL FLOATA(DCODE, N, IBUF, RBUF)

  Is similar to the following code:


       DO K=1,N
          RBUF(K) = FLOAT(IBUF(K))
       ENDDO

  Where DCODE and N are INTEGER*4, RBUF is a REAL*4 array, and the data type 
  of the IBUF array is controlled by the data code DCODE.

$ Return
$!#############################################################################
