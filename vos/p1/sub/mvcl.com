$!****************************************************************************
$!
$! Build proc for MIPL module mvcl
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:17
$!
$! Execute by entering:		$ @mvcl
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
$ write sys$output "*** module mvcl ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mvcl.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   if F$SEARCH("mvcl.imake") .nes. ""
$   then
$      vimake mvcl
$      purge mvcl.bld
$   else
$      if F$SEARCH("mvcl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mvcl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mvcl.bld "STD"
$   else
$      @mvcl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mvcl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mvcl.com -mixed -
	-s mvcl.c -
	-i mvcl.imake -
	-t tstmvcl.f tstmvcl.imake tstmvcl.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mvcl.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>

/* Moves len bytes of data from Fortran character*n variable "from" to	*/
/* non-character*n buffer "to".  No C-callable version is necessary.	*/
/* No error checking is performed on the length of the Fortran string;	*/
/* it's assumed the "len" parameter is valid.				*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(mvcl, MVCL)
(
  char *from,			/* Fortran CHARACTER*n variable */
  char *to,			/* output buffer, NOT a CHARACTER*n */
  int *len,			/* Length in bytes to move */
  ZFORSTR_PARAM
)
{
   ZFORSTR_BLOCK

   zmove(zsfor2ptr(from), to, *len);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mvcl.imake
/* Imake file for VICAR subroutine MVCL */

#define SUBROUTINE mvcl

#define MODULE_LIST mvcl.c

#define P1_SUBLIB

#define USES_ANSI_C
#define FTN_STRING

$ Return
$!#############################################################################
$Test_File:
$ create tstmvcl.f
	include 'VICMAIN_FOR'
	subroutine main44
	byte byt(100)
	character*100 ch, outchar

	ch = 'This is a test. ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

	call mvcl(ch, byt, 100)

	do i=1,100
	   outchar(i:i) = char(byt(i))
	end do

	call xvmessage('The output is:',' ')
	call xvmessage(outchar, ' ')

	return
	end
$!-----------------------------------------------------------------------------
$ create tstmvcl.imake
/* Imake file for Test of VICAR subroutine MVCL */

#define PROGRAM tstmvcl

#define MODULE_LIST tstmvcl.f

#define MAIN_LANG_FORTRAN
#define TEST
#define LIB_P1SUB

#define USES_FORTRAN

#define LIB_RTL

$!-----------------------------------------------------------------------------
$ create tstmvcl.pdf
process
end-proc
$ Return
$!#############################################################################
