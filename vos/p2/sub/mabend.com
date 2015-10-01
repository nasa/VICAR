$!****************************************************************************
$!
$! Build proc for MIPL module mabend
$! VPACK Version 1.9, Monday, December 07, 2009, 16:26:45
$!
$! Execute by entering:		$ @mabend
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
$ write sys$output "*** module mabend ***"
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
$ write sys$output "Invalid argument given to mabend.com file -- ", primary
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
$   if F$SEARCH("mabend.imake") .nes. ""
$   then
$      vimake mabend
$      purge mabend.bld
$   else
$      if F$SEARCH("mabend.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mabend
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mabend.bld "STD"
$   else
$      @mabend.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mabend.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mabend.com -mixed -
	-s mabend.f zmabend.c -
	-i mabend.imake -
	-t tmabend.f tzmabend.c tmabend.imake tmabend.pdf tstmabend.pdf -
	-o mabend.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mabend.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	subroutine MABEND( msg)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c  85-5-5  ...lwk...
c  92-9-18    sp    MADE PORTABLE FOR UNIX.  MADE MSG A REQUIRED PARAMETER.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	implicit integer (a-z)
        CHARACTER*(*) msg
C==================================================================
        call xvmessage( msg, ' ')
	call abend
	end
C##################################################################

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zmabend.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zmabend(char* message)
{
   zvmessage(message,"");
   zabend();
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mabend.imake
/* Imake file for VICAR subroutine MABEND */

#define SUBROUTINE mabend

#define MODULE_LIST mabend.f zmabend.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create tmabend.f
c  test subroutine MABEND
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer (a-z)
      character*17 msg/'This is a message'/
      logical xvptst

      if (xvptst('CHAR')) then
        call MABEND( msg)
      else if (xvptst('C') .OR. xvptst('CCHAR')) then
        call tzmabend
      else
        call MABEND( 'This is also a message')
      endif


      return
      end
$!-----------------------------------------------------------------------------
$ create tzmabend.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzmabend)() 
{
      static char msg[] = "This is a C message";
/*  ============================================  */

      if (zvptst("CCHAR"))   zmabend( msg);
      else                   zmabend("This is also a C message");
}
$!-----------------------------------------------------------------------------
$ create tmabend.imake
/* Imake file for Test of VICAR subroutine mabend */

#define PROGRAM tmabend

#define MODULE_LIST tmabend.f tzmabend.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tmabend.pdf
process
parm char keyword valid=(CHAR,C,CCHAR) count=0:1 default=--
end-proc

$!-----------------------------------------------------------------------------
$ create tstmabend.pdf
procedure
refgbl $autousage
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
let $autousage = "no"
tmabend 'char
tmabend
tmabend 'cchar
tmabend 'c
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mabend.hlp
1 MABEND

  Print a message and abort the current program.

  FORTRAN Calling Sequence:  call mabend( msg )
  C Calling Sequence:        zmabend( msg );

  The argument msg is REQUIRED. 
  In Fortran, it must be a quoted string or CHARACTER type.
  In C, it must be a quoted string or any other null-terminated string.

  MABEND calls XVMESSAGE to print the message and then calls ABEND.
  It is a programming convenience that often allows error checking
  to be coded as 1-line "if" statements.

2 History

 92-09-25 ...SP.... Made portable for UNIX conversion. 
                    Added zmabend for calls from C.

  Original Programmer: L. W. Kamp, 5 May 1985.
  Current Cognizant Programmer: L. W. Kamp
  Source Language: Fortran (or C for zmabend)

$ Return
$!#############################################################################
