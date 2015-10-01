$!****************************************************************************
$!
$! Build proc for MIPL module zia
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:19
$!
$! Execute by entering:		$ @zia
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
$ write sys$output "*** module zia ***"
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
$ write sys$output "Invalid argument given to zia.com file -- ", primary
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
$   if F$SEARCH("zia.imake") .nes. ""
$   then
$      vimake zia
$      purge zia.bld
$   else
$      if F$SEARCH("zia.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zia
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zia.bld "STD"
$   else
$      @zia.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zia.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zia.com -mixed -
	-s zia.c -
	-i zia.imake -
	-t tzia.f tzia.imake tzia.pdf tstzia.pdf -
	-o zia.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zia.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************
 * zia.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>

/************************************************************************/
/* Fortran-Callable Version (no C-version needed -- use memset)         */
/************************************************************************/

void FTN_NAME2(zia, ZIA) (char *cbuf,int *n)
{
  memset(cbuf,0,4*(*n));
  return;
}



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zia.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY zia

   To Create the build file give the command:

	$ vimake zia                     (VMS)
   or
	% vimake zia                     (Unix)


*************************************************************************/

#define SUBROUTINE zia

#define MODULE_LIST zia.c

#define P1_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tzia.f
        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 ot(6)
        character*80 message

        do i=1,6
        ot(i)=-3+i
        end do
        write (message,'(A,6I4,A)')
     +     '    A =(',ot(1),ot(2),ot(3),ot(4),ot(5),ot(6),'  )'
        call xvmessage(message,' ')
	call ZIA(ot,6)
	write (message,'(A,6I4,A)')
     +     'ZIA(A)=(',ot(1),ot(2),ot(3),ot(4),ot(5),ot(6),'  )'
        call xvmessage(message,' ')
	return
	end
$!-----------------------------------------------------------------------------
$ create tzia.imake
/***********************************************************************

                     IMAKE FILE FOR TEST PROGRAM tzia

   To Create the build file give the command:

	$ vimake tzia                     (VMS)
   or
	% vimake tzia                     (Unix)

*************************************************************************/

#define PROGRAM tzia

#define MODULE_LIST tzia.f

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_LOCAL

#define USES_FORTRAN
#define MAIN_LANG_FORTRAN
$!-----------------------------------------------------------------------------
$ create tzia.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstzia.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
TZIA
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create zia.hlp
1 ZIA

  zeroes a fullword array

  FORTRAN Calling Sequence:  ZIA( OUT, NWORDS)

  Arguments:  OUT is the array to be cleared
              NWORDS is word (4-byte!) count to be cleared
2 Operation

  ZIA calls the C-subroutine memset to zero the array.

2 History

  Original Programmer: Budak Barkan (Dec 12, 1984)
  Current Cognizant Programmer: Niles D Ritter (6 Feb, 1992)
  Source Language: C

  1. Original Version 				bb	Dec 12, 1984
  2  Conversion to C for UNIX Compatibility	ndr	Feb  6, 1992

$ Return
$!#############################################################################
