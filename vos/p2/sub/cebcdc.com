$!****************************************************************************
$!
$! Build proc for MIPL module cebcdc
$! VPACK Version 1.9, Monday, December 07, 2009, 16:08:37
$!
$! Execute by entering:		$ @cebcdc
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
$ write sys$output "*** module cebcdc ***"
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
$ write sys$output "Invalid argument given to cebcdc.com file -- ", primary
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
$   if F$SEARCH("cebcdc.imake") .nes. ""
$   then
$      vimake cebcdc
$      purge cebcdc.bld
$   else
$      if F$SEARCH("cebcdc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cebcdc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cebcdc.bld "STD"
$   else
$      @cebcdc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cebcdc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cebcdc.com -mixed -
	-s cebcdc.c -
	-i cebcdc.imake -
	-t tcebcdc.f tzcebcdc.c tcebcdc.imake tcebcdc.pdf tstcebcdc.pdf -
	-o cebcdc.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cebcdc.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*---------------------------  cebcdc     ------------------------
 * CEBCDC  (EBCDIC to ASCII)
 *
 *  REVISION HISTORY
 *    10-94 CRI MSTP S/W Conversion (VICAR Porting), changed to use C
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	VICAR SUBROUTINE                                           nibble

	General routine for converting an array of EBCDIC characters
          into an array of ASCII characters

	Fortran format of call:

          CALL CEBCDC( BUF,NCHAR)

        "C" format of call:

          zcebcdc(buf, nchar);
 
	Parameters:-

	BUF - character declared array containing the EBCDIC
        NCHAR - is the number of characters

--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"

void zcebcdc(unsigned char *buf, int nchar);
/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(cebcdc, CEBCDC) (buf, nchar)
     unsigned char *buf;
     int *nchar;
{
   zcebcdc( buf, *nchar);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zcebcdc(unsigned char *buf, int nchar)
{
  unsigned char ebcdic_table[256] = {
/*                          ROW    Conversion results are in decimal
   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   A,  B,  C,  D,  E,   F
                                                                            */
  00,  01,  02,  03,  92,   9,  92, 127,  92,  92,  92, 11, 12, 13, 14,  15,
  16,  17,  18,  19,  92,  92,   8,  92,  24,  25,  92, 92, 28, 29, 30,  31,
  92,  92,  92,  92,  92,  10,  23,  27,  92,  92,  92, 92, 92, 05, 06,  07,
  92,  92,  22,  92,  92,  92,  92,  04,  92,  92,  92, 92, 20, 21, 92,  26,
  32,  92,  92,  92,  92,  92,  92,  92,  92,  92,  91, 46, 60, 40, 43,  33,
  38,  92,  92,  92,  92,  92,  92,  92,  92,  92,  93, 36, 42, 41, 59,  94,
  45,  47,  92,  92,  92,  92,  92,  92,  92,  92, 124, 44, 37, 95, 62,  63,
  92,  92,  92,  92,  92,  92,  92,  92,  92,  96,  58, 35, 64, 39, 61,  34,
  92,  97,  98,  99, 100, 101, 102, 103, 104, 105,  92, 92, 92, 92, 92,  92,
  92, 106, 107, 108, 109, 110, 111, 112, 113, 114,  92, 92, 92, 92, 92,  92,
  92, 126, 115, 116, 117, 118, 119, 120, 121, 122,  92, 92, 92, 92, 92,  92,
  92,  92,  92,  92,  92,  92,  92,  92,  92,  92,  92, 92, 92, 92, 92,  92,
 123,  65,  66,  67,  68,  69,  70,  71,  72,  73,  92, 92, 92, 92, 92,  92,
 125,  74,  75,  76,  77,  78,  79,  80,  81,  82,  92, 92, 92, 92, 92,  92,
  92,  92,  83,  84,  85,  86,  87,  88,  89,  90,  92, 92, 92, 92, 92,  92,
  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  92, 92, 92, 92, 92, 255};

  register int i;

  for (i=0; i < nchar; i++)         /* for each character */
    {
      buf[i] = ebcdic_table[buf[i]];   /* get conversion and replace */
    }
  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cebcdc.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY cebcdc

   To Create the build file give the command:

	$ vimake cebcdc                     (VMS)
   or
	% vimake cebcdc                     (Unix)


*************************************************************************/

#define SUBROUTINE cebcdc

#define MODULE_LIST cebcdc.c

#define P2_SUBLIB
#define DEBUG
#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tcebcdc.f
C   
C   This is the test program for the subroutine CEBCDC
C
C
      include 'VICMAIN_FOR'
      subroutine main44
      BYTE BUF(11)
      INTEGER N
      N = 11
      DATA BUF /'E3'X,'C5'X,'E2'X,'E3'X,'7A'X,'E2'X,'E3'X,
     +'D9'X,'C9'X,'D5'X,'C7'X/
      CALL PRNT (0,N,BUF,'  START =  .')
      CALL CEBCDC(BUF,N)
      CALL PRNT (0,N,BUF,'  RESULT = .')
      call tzcebcdc  ! test the "C" interface
      return
      end
C       
$!-----------------------------------------------------------------------------
$ create tzcebcdc.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/* "C" test routine to test the cebcdc subroutine.  This routine builds
 *  an array of EBCDIC characters and then calls the zcebcdc subroutine
 *  to parse the array of characters and replace them with ASCII
 *  characters. 
 */ 



void FTN_NAME(tzcebcdc)() 

{
  unsigned char buf[256] = {0xE3,0xC5,0xE2,0xE3,0x7A,0xE2,0xE3,
                            0xD9,0xC9,0xD5,0xC7};
  int nchar;

/*  ==================================================================  */

      zvmessage("Test the C interface","");
        nchar = 11; 
	zprnt(0,nchar,buf,"start =  ");
	zcebcdc(buf,nchar);
	zprnt(0,nchar,buf,"result =  ");
}
$!-----------------------------------------------------------------------------
$ create tcebcdc.imake
/* Imake file for Test of VICAR subroutine cebcdc */

#define PROGRAM tcebcdc

#define MODULE_LIST tcebcdc.f tzcebcdc.c

#define MAIN_LANG_FORTRAN

#define USES_FORTRAN
#define USES_ANSI_C
/*#define LIB_LOCAL*/

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tcebcdc.pdf
   process
   end-proc
$!-----------------------------------------------------------------------------
$ create tstcebcdc.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
tcebcdc
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create cebcdc.hlp
1 CEBCDC

    PURPOSE
 
         Subroutine to convert EBCDIC to ASCII.
 
  Fortran calling sequence:  CALL CEBCDC(BUF, NCHAR)
  C Calling Sequence:        zcebcdc(buf, nchar);

  Note: If conversion from ASCII to EBCDIC is needed in the future, Copy
        CEBCDC procedures to new names and change the table in CEBCDC.C.
  
2 History

  Original Programmer: M. Girard, 12 April 1982
  Additional Programmer: LWK Barkan (April 18, 1983)
  Current Cognizant Programmer: M. Girard
  Documentation Author: M. Girard
  Source Language: "C"
  Made portable for UNIX RNR(CRI) (Oct 31, 1994) Rewrote in "C"

2 Operation

  Converts input buffer of EBCDIC characters to ASCII characters
  in the same buffer.

2 Arguments

  BUF is a character declared array containing the EBCDIC characters
  NCHAR is the number of EBCDIC characters
$ Return
$!#############################################################################
