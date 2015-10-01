$!****************************************************************************
$!
$! Build proc for MIPL module addv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:07:10
$!
$! Execute by entering:		$ @addv
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
$ write sys$output "*** module addv ***"
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
$ write sys$output "Invalid argument given to addv.com file -- ", primary
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
$   if F$SEARCH("addv.imake") .nes. ""
$   then
$      vimake addv
$      purge addv.bld
$   else
$      if F$SEARCH("addv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake addv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @addv.bld "STD"
$   else
$      @addv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create addv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack addv.com -mixed -
	-s addv.c -
	-i addv.imake -
	-t taddv.f tzaddv.c taddv.imake taddv.pdf tstaddv.pdf -
	-o addv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create addv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            ADDV

	General routine for adding arrays.Array B is replaced with the
	sum of A and B.A and B can be of different data types as indicated

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

void zaddv(int dcode, int n, void* avec, void *bvec, int inca, int incb);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(addv, ADDV) (int *dcode, int *n, void *avec, void *bvec,
			int *inca, int *incb)
{
   zaddv( *dcode, *n, avec, bvec, *inca, *incb);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zaddv(int dcode, int n, void* avec, void *bvec, int inca, int incb)
{
  int i;
  
  /* vectors */
  uint8_t  *bytein,   *byteout;
  int16_t  *halfin,   *halfout;
  int32_t  *fullin,   *fullout;
  float    *realin,   *realout;
  double   *doublein, *doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = *bytein + *byteout;
    }
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb){
      *halfout = *halfin + *halfout;
    }
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb){
      *byteout = *halfin + *byteout ;
    }
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = *bytein + *halfout ;
    }
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullin + *fullout;
    }
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = *fullin + *byteout;
    }
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *bytein + *fullout;
    }
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = (int16_t *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = *fullin + *halfout;
    }
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *halfin + *fullout;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< n; i++, realin+=inca,realout+=incb){
      *realout = *realin + *realout;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,doubleout+=incb){
      *doubleout = *doublein + *doubleout;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,realout+=incb){
      *realout = *doublein + *realout;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++,realin+=inca,doubleout+=incb){
      *doubleout = *realin + *doubleout;
    }
    break;
  default:    
    zvmessage("*** ADDV - Illegal DCODE","");
    zabend();
    break;
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create addv.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY addv

   To Create the build file give the command:

	$ vimake addv                     (VMS)
   or
	% vimake addv                     (Unix)


*************************************************************************/

#define SUBROUTINE addv

#define MODULE_LIST addv.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create taddv.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C TADD IS A TEST PROGRAM CONSTRUCTED TO RUN THE
C SUBROUTINE ADD THROUGH ITS PACES.
	BYTE A1(8),B1(8)
	INTEGER*2 A2(8), B2(8)
	INTEGER*4 A3(8), B3(8)
	REAL A4(8), B4(8)
	DOUBLE PRECISION A5(8), B5(8)
	DATA A1 /-1,64,-3,4,-5,6,-7,8/
	DATA B1 /8,64,6,5,4,3,2,1/
	DATA A2 /1111,2222,3333,4444,5555,6666,
     .  7777,8888/
	DATA B2 /8888,7777,6666,-5555,4444,
     .  -3333,2222,-1111/
	DATA A3 /11111111,22222222,33333333,44444444,
     .  55555555,66666666,77777777,88888888/
        DATA B3 /-88888888,77777777,-66666666,55555555,
     .  -44444444,33333333,-22222222,11111111/
	DATA A4 /1.1,2.22,3,4.4444,5.55555,6.666666,
     .  7.77777,8.888/
	DATA B4 /8.8,7.77,6.666,5.5555,4.444,3.33,2.2,1.0/
	DATA A5 /111111.D8,222222.D7,333333.D6,444444.D5,
     .  555555.D4,666666.D3,777777.D2,888888.D1/
	DATA B5 /88888888.D8,77777777.D7,66666666.D6,
     .  55555555.D5,44444444.D4,33333333.D3,
     .  22222222.D2,11111111.D1/


C ADD BYTE TO BYTE
	CALL ADDV(1,8,A1,B1,1,1)
	CALL PRNT(1,8,B1,' BYTE TO BYTE.')
C ADD HALFWORD TO HALFWORD
	CALL ADDV(2,8,A2,B2,1,1)
	CALL PRNT(2,8,B2,' HALFWORD TO HALFWORD.')
C ADD BYTE TO HALFWORD
	CALL ADDV(3,8,A1,B2,1,1)
	CALL PRNT(2,8,B2,' BYTE TO HALFWORD.')

C ADD FULLWORD TO FULLWORD
	CALL ADDV(4,8,A3,B3,1,1)
	CALL PRNT(4,8,B3,' FULLWORD TO FULLWORD.')
C ADD BYTE TO FULLWORD
	CALL ADDV(5,8,A1,B3,1,1)
	CALL PRNT(4,8,B3,' BYTE TO FULLWORD.')
C ADD HALFWORD TO FULLWORD
	CALL ADDV(6,8,A2,B3,1,1)
	CALL PRNT(4,8,B3,' HALFWORD TO FULLWORD.')

C ADD REAL TO REAL
	CALL ADDV(7,8,A4,B4,1,1)
	CALL PRNT(7,8,B4,' REAL TO REAL.')


C ADD DOUBLE TO DOUBLE 
	CALL ADDV(8,8,A5,B5,1,1)
	CALL PRNT(8,8,B5,' DOUBLE TO DOUBLE.')
C ADD REAL TO DOUBLE
	CALL ADDV(9,8,A4,B5,1,1)
	CALL PRNT(8,8,B5,' REAL TO DOUBLE.')
C ADD DOUBLE + REAL
	CALL ADDV(-9,8,A5,B4,1,1)
	CALL PRNT(7,8,B4,' DOUBLE TO REAL.')

        CALL TZADDV      ! TEST C INTERFACE
        RETURN
	END
$!-----------------------------------------------------------------------------
$ create tzaddv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzaddv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,64}, 
                  c[2] = {10,100};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zaddv( 1,2, b, c, 1,1);   /*  add b to c*/

      sprintf( pbuf, "Output from zaddv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   15    164","");
}
$!-----------------------------------------------------------------------------
$ create taddv.imake
/* Imake file for Test of VICAR subroutine addv */

#define PROGRAM taddv

#define MODULE_LIST taddv.f tzaddv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create taddv.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstaddv.pdf
procedure
refgbl $autousage
refgbl $echo
body
let _onfail="continue"
let $autousage="none"
let $echo="yes"
! This is a test of the subroutine ADDV which adds 
! together two arrays which may be of different types
! (byte, halfword, fullword, real, or double
! precision). The sum of each index point of the 
! two arrays is placed in the second array at that point.
taddv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create addv.hlp
1 ADDV

  ADDV is a general routine for adding arrays.  

 Fortran calling sequence:  CALL ADDV(DCODE, N, A, B, INCA, INCB)
 C Calling Sequence:        zaddv(dcode, n, a, b, inca, incb);
 
  INCA and INCB are REQUIRED arguments. (dcode, n, inca, and incb are
  passed by value for zaddv.)

  Arguments:

    DCODE  (input, integer)           data format code
    N      (input, integer)           number of elements
    A      (input, DCODE data type)   first input array
    B      (in/out, DCODE data type)  second input/output array
    INCA   (input, integer)           A increment
    INCB   (input, integer)           B increment

2 Operation

  ADDV is a general routine for adding arrays.  Array B is 
  replaced with the sum of A and B.  Arrays A and B can be
  of different data types as indicated by DCODE.
  
2 Arguments 

    DCODE   data types    =1    A is byte           B is byte
                          =2    A is halfword       B is halfword
                          =3    A is byte           B is halfword
                          =4    A is fullword       B is fullword
                          =5    A is byte           B is fullword
                          =6    A is halfword       B is fullword
                          =7    A is real (single)  B is real
                          =8    A is double         B is double
                          =9    A is real           B is double

                          negative values -1 to -9 are reverse of above

    N       Number of elements on which operation will be performed
    A       First input array
    B       Second input array; also contains the output (sum of A and B)
    INCA    Address increment in array A
    INCB    Address increment in array B

2 History

  Original Programmer: Gary M. Yagi      2/4/75
  Current Cognizant Programmer:  Lucas W. Kamp   3/15/83
  Source Language: C

 REVISION HISTORY:                                          
   11-92    ..SP....  Made portable for UNIX - Adapted from ASU version.
                      Added zaddv for calls from C.  Changed name from
                      add to addv (added v for most of the vector routines).
                      ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.

$ Return
$!#############################################################################
