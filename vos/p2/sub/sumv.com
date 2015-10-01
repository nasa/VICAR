$!****************************************************************************
$!
$! Build proc for MIPL module sumv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:37:34
$!
$! Execute by entering:		$ @sumv
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
$ write sys$output "*** module sumv ***"
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
$ write sys$output "Invalid argument given to sumv.com file -- ", primary
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
$   if F$SEARCH("sumv.imake") .nes. ""
$   then
$      vimake sumv
$      purge sumv.bld
$   else
$      if F$SEARCH("sumv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sumv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sumv.bld "STD"
$   else
$      @sumv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sumv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sumv.com -mixed -
	-s sumv.c -
	-i sumv.imake -
	-t tsumv.f tzsumv.c tsumv.imake tsumv.pdf tstsumv.pdf -
	-o sumv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sumv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*---------------------------  sumv     ------------------------
 * SUMV  (SUM Vector)
 *
   REVISION HISTORY
      6-87   SP  CORRECTED PROBLEM WITH DCODE 6 FOR NEGATIVE DNS BY CHANGING
                 MOVZWL TO CVTWL
     11-92   SP  Made portable for UNIX, changed to use C - Adapted from ADDV

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	VICAR SUBROUTINE                                            sumv

	General routine for summing elements in array A and returning sum 
	in B (B is a scalar). B = SUM A(i). A and B can be of different
       data types as indicated.

	Fortran format of call:

	CALL SUMV(DCODE, N, A, B, INCA)

	Parameters:-

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
	N..........Number of elements to sum.
	A..........Source vector
        B..........Sum (B is an output scalar variable)
        INCA     - Source vector index increment
--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdint.h>

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(sumv, SUMV) (dcode, n, avec, b, inca)
     int *dcode, *n, *inca;
     void *avec, *b;
{
   zsumv( *dcode, *n, avec, b, *inca);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zsumv(dcode, n, avec, b, inca)
   int dcode, n, inca;
   void *avec, *b;
{
  int i;
  
  /* vector in, scalar out */
  uint8_t *bytein,   byteout;
  int16_t     *halfin,   halfout;
  int32_t          *fullin,   fullout;
  float         *realin,   realout;
  double        *doublein, doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = 0;
    for (i=0; i < n; i++, bytein+=inca) {
      byteout += *bytein;
    }
    *(uint8_t *)b = byteout; 
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = 0;
    for (i = 0; i <n;  i++, halfin+=inca){
      halfout += *halfin;
    }
    *(int16_t *)b = halfout;
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = 0;
    for (i = 0; i<n; i++,halfin+=inca){
      byteout += *halfin;
    }
    *(uint8_t *)b = byteout;
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = 0;
    for (i = 0; i< n; i++, bytein+=inca){
      halfout += *bytein;
    }
    *(int16_t *)b = halfout;
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = 0;
    for (i = 0; i<n; i++,fullin+=inca){
      fullout += *fullin;
    }
    *(int32_t *)b = fullout;
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = 0;
    for (i = 0; i< n; i++,fullin+=inca){
      byteout += *fullin;
    }
    *(uint8_t *)b = byteout;
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = 0;
    for (i = 0; i< n; i++, bytein+=inca){
      fullout += *bytein;
    }
    *(int32_t *)b = fullout;
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = 0;
    for (i = 0;i< n; i++, fullin+=inca){
        halfout += *fullin;
    }
    *(int16_t *)b = halfout;
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = 0;
    for (i = 0; i< n; i++, halfin+=inca){
      fullout += *halfin;
    }
    *(int32_t*)b = fullout;
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = 0.0;
    for (i = 0;i< n; i++, realin+=inca){
      realout += *realin;
    }
    *(float *)b = realout;
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = 0.0;
    for (i = 0; i< n; i++, doublein+=inca){
      doubleout += *doublein;
    }
    *(double *)b = doubleout;
    break;
  case -9:
    doublein = (double *) avec;
    realout = 0.0;
    for (i = 0; i< n; i++, doublein+=inca){
      realout += *doublein;
    }
    *(float *)b = realout;
    break;
  case 9:
    realin = (float *) avec;
    doubleout = 0.0;
    for (i = 0; i< n; i++,realin+=inca){
      doubleout += *realin;
    }
    *(double *)b = doubleout;
    break;
  default:    
    zvmessage("*** SUMV - Illegal DCODE","");
    zabend();
    break;
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sumv.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY sumv

   To Create the build file give the command:

	$ vimake sumv                     (VMS)
   or
	% vimake sumv                     (Unix)


*************************************************************************/

#define SUBROUTINE sumv

#define MODULE_LIST sumv.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tsumv.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      implicit integer (a - z)
C*******************************************
C     Data for dcode=1
      byte b
      byte a(5)
      data a/2,3,100,-2,-3/
C******************************************
C     Data for dcode=2
      integer*2 d
      integer*2 c(10)
      data c/-999,1000,999,-1000,31000,255,-31000,-100,100,255/
C******************************************
C     Data for dcode=4
      integer f
      integer e(10)
      data e/11111111,1,-11111111,-1,22222222,-22222222,1,
     .       -999,999,22222222/
C******************************************
C     Data for dcode=7
      real h,g(16)
      data g /1.1,2.22,3.333,4.4444,5.55555,6.666666,
     .        7.77777,8.888,8.8,7.77,6.666,5.5555,
     . 	      4.444,3.33,2.2,1.0/
C******************************************
C     Data for dcode=8
      double precision j,i(7)
      data i /111111.d8,222222.d8,333333.d8,444444.d5,
     .        222222.d8,-444444.d5,111111.d8/
C******************************************
C     Data for dcode=3
      integer*2 l
      byte k(11)
      data k /-127,126,127,112,-1,-112,1,-126,
     .        -1,-1,100/
C******************************************
C     Data for dcode=-3
      byte n
      integer*2 m(6)
      data m /250,500,-210,-310,210,310/
C******************************************
C     Data for dcode=6
      integer*2 o(11)
      data o/-999,1000,999,-1000,31000,255,31000,-100,100,-255,31000/
C*******************************************
C     Data for dcode=-6
      integer*4 p(7)
      data p/66666,-66666,-999,1000,999,-1000,-31000/
C*******************************************
C     Test for byte to byte
      call sumv(1,5,a,b, 1)
      call XVMESSAGE('B should be equal to 100 ',' ')
      call prnt (1,1,b,'b=.')
      call sumv(1,3,a,b,2)
      call XVMESSAGE('B should be equal to 99 ',' ')
      call prnt (1,1,b,'b=.')
      call sumv(1,2,a,b,3)
      call XVMESSAGE('B should be equal to 0 ',' ')
      call prnt (1,1,b,'b=.')
C*********************************************
C     Test for half to half
      call sumv(2,10,c,d, 1)
      call XVMESSAGE('D should be equal to 510 ',' ')
      call prnt (2,1,d,'d=.')
      call sumv(2,5,c,d,2)
      call XVMESSAGE('D should be equal to 100 ',' ')
      call prnt (2,1,d,'d=.')
C*********************************************
C     Test for full to full
      call sumv(4,10,e,f, 1)
      call XVMESSAGE('F should be equal to 22222223 ',' ')
      call prnt (4,1,f,'f=.')
C*********************************************
C     Test for real to real
      call sumv(7,16,g,h, 1)
      call XVMESSAGE('H should be approximately equal to 79.750886 ',
     .                ' ')
      call prnt (7,1,h,'h=.')
      call sumv(7,3,g,h,7)
      call XVMESSAGE('H should be approximately equal to 12.188 ',' ')
      call prnt (7,1,h,'h=.')
C*********************************************
C     Test for double to double
      call sumv(8,7,i,j, 1)
      call XVMESSAGE('J should be approximately equal to 999999.d8 ',
     .               ' ')
      call prnt (8,1,j,'j=.')
      call sumv(8,2,i,j,5)
      call XVMESSAGE('J should be approximately equal to 110666.d8 ',
     .                ' ')
      call prnt (8,1,j,'j=.')
C*********************************************
C     Test for byte fo halfword
      call sumv(3,11,k,l, 1)
      call XVMESSAGE('L should be equal to 1634 ',' ')
      call prnt (2,1,l,'l=.')
      call sumv(3,6,k,l,2)
      call XVMESSAGE('L should be equal to 867 ',' ')
      call prnt (2,1,l,'l=.')
C*********************************************
C     Test for halfword to byte
      call sumv(-3,6,m,n, 1)
      call XVMESSAGE('N should be equal to 238 ',' ')
      call prnt (1,1,n,'N=.')
      call sumv(-3,3,m,n,2)
      call XVMESSAGE('N should be equal to 250 ',' ')
      call prnt (1,1,n,'N=.')
C*********************************************
C     Test for byte to fullword
      call sumv(5,11,k,f, 1)
      call XVMESSAGE('F should be equal to 1634 ',' ')
      call prnt (4,1,f,'f=.')
C*********************************************
C     Test for fullword to byte
      call sumv(-5,7,e,b, 1)
      call XVMESSAGE('B should be equal to 1 ',' ')
      call prnt (1,1,b,'b=.')
C*********************************************
C     Test for halfword to fullword
      call sumv(6,11,o,f, 1)
      call XVMESSAGE('F should be equal to 93000 ',' ')
      call prnt (4,1,f,'f=.')
      call sumv(6,6,o,f,2)
      call XVMESSAGE('F should be equal to 93100 ',' ')
      call prnt (4,1,f,'f=.')
C*********************************************
C     Test for fullword to halfword
      call sumv(-6,7,p,d, 1)
      call XVMESSAGE('D should be equal to -31000 ',' ')
      call prnt (2,1,d,'d=.')
C*********************************************
C     Test for real to double
      call sumv(9,16,g,j, 1)
      call XVMESSAGE('J should be approximately equal to 79.750886 ',
     .                ' ')
      call prnt (8,1,j,'j=.')
C*********************************************
C     Test for double to real
      call sumv(-9,7,i,h, 1)
      call XVMESSAGE('H should be approximately equal to 999999.d8 ',
     .               ' ')
      call prnt (7,1,h,'h=.')
C******************************************************************
      CALL TZSUMV      ! TEST C INTERFACE

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzsumv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzsumv)() 

{
  char pbuf[81];
  int b[2], c;

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      b[0] = -16;
      b[1] =  640116;
      zsumv( 4, 2, b, &c, 1);   /*  sum b[0] and b[1] */

      sprintf( pbuf, "Output from zsumv = %d", c);
      zvmessage(pbuf, "");
      zvmessage("Correct value is 640100","");
}
$!-----------------------------------------------------------------------------
$ create tsumv.imake
/* Imake file for Test of VICAR subroutine sumv */

#define PROGRAM tsumv

#define MODULE_LIST tsumv.f tzsumv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tsumv.pdf
process help=*
END-PROC
$!-----------------------------------------------------------------------------
$ create tstsumv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tsumv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create sumv.hlp
1 SUMV
 
    PURPOSE
 
         Subroutine to sum elements in array A and return sum in B.
	 B = SUM A(i) for i = 1 to 1 + INCA*(N-1). 
         A and B can be of different data types as indicated.
 
 Fortran calling sequence:  CALL SUMV(DCODE, N, A, B, INCA)
 C Calling Sequence:        zsumv( dcode, n, a, b, inca );
 
  INCA is a REQUIRED argument. (dcode, n, and inca are
  passed by value for zsumv.)
   	 

    Arguments:

    DCODE  (input, integer)           data format code
    N      (input, integer)           number of elements
    A      (input, DCODE data type)   input array
    B      (out,   DCODE data type)   sum of elements from input array.
    INCA   (input, integer)           A increment
 
2  ARGUMENTS
 
 	DCODE      Data types
 
                          ..INPUT..         ..OUTPUT..
 
                    =1,   A is byte         B is byte
                    =2,   A is halfword     B is halfword
                    =3,   A is byte         B is halfword
                    =4,   A is fullword     B is fullword
                    =5,   A is byte         B is fullword
                    =6,   A is halfword     B is fullword
                    =7,   A is REAL*4       B is REAL*4
                    =8,   A is REAL*8       B is REAL*8
                    =9,   A is REAL*4       B is REAL*8
                    negative values -1 to -9 reverse of above
 
         N          Number of elements to sum.
 
         A          Source vector
 
         B          Sum of elements from A.
  
         INCA       Source vector address increment.
 
2  HISTORY
 
         Original Programmer: Gary Yagi, 4 Feb 1975
         Current Cognizant Programmer: Steve Pohorsky
         Source Language: C

 REVISION HISTORY:                                          
   11-92    ..SP....  Made portable for UNIX - Adapted from ADDV.
                      Added zsumv for calls from C.  Changed name from
                      sum to sumv (added v for most of the vector routines).
                      ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.
$ Return
$!#############################################################################
