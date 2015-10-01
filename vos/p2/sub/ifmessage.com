$!****************************************************************************
$!
$! Build proc for MIPL module ifmessage
$! VPACK Version 1.9, Monday, December 07, 2009, 16:23:30
$!
$! Execute by entering:		$ @ifmessage
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
$ write sys$output "*** module ifmessage ***"
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
$ write sys$output "Invalid argument given to ifmessage.com file -- ", primary
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
$   if F$SEARCH("ifmessage.imake") .nes. ""
$   then
$      vimake ifmessage
$      purge ifmessage.bld
$   else
$      if F$SEARCH("ifmessage.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ifmessage
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ifmessage.bld "STD"
$   else
$      @ifmessage.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ifmessage.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ifmessage.com -mixed -
	-s ifmessage.f zifmessage.c -
	-i ifmessage.imake -
	-t tifmessage.f tzifmessage.c tifmessage.imake tifmessage.pdf -
	   tstifmessage.pdf -
	-o ifmessage.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ifmessage.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	subroutine IFMESSAGE( msg)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	implicit integer (a-z)
        PARAMETER (NOMESSAGE_PAR = 512)  !SEE RTL ROUTINE XVZINIT.
        CHARACTER*(*) msg
C==================================================================
        CALL XVP('$switch', SWTCH, N)
        SWTCH = SWTCH/NOMESSAGE_PAR     !TEST THE 512 BIT OF $SWITCH.

        IF ( MOD(SWTCH,2) .EQ. 0)  call xvmessage( msg, ' ')
	RETURN
	end
C##################################################################

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zifmessage.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#define S_NO_MESSAGE 512  /*NOMESSAGE bit in $SWITCH. See RTL routine xvzinit*/
#include "zvproto.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zifmessage(char* message)
{
   int n, swtch;
/*  ==================================================================  */
   zvp("$switch", &swtch, &n);	   /* Get $switch to check NOMESSAGE flag. */
   if ((swtch & S_NO_MESSAGE) == 0)   
      zvmessage(message,"");
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ifmessage.imake
/* Imake file for VICAR subroutine IFMESSAGE */

#define SUBROUTINE ifmessage

#define MODULE_LIST ifmessage.f zifmessage.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create tifmessage.f
c  test subroutine IFMESSAGE
      INCLUDE 'VICMAIN_FOR'
      subroutine main44

        call IFMESSAGE( 'A message from FORTRAN')
        call tzifmessage
      return
      end
$!-----------------------------------------------------------------------------
$ create tzifmessage.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzifmessage)() 
{
/*  ============================================  */
  zifmessage("This is a C message");
}
$!-----------------------------------------------------------------------------
$ create tifmessage.imake
/* Imake file for Test of VICAR subroutine ifmessage */

#define PROGRAM tifmessage

#define MODULE_LIST tifmessage.f tzifmessage.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tifmessage.pdf
process
end-proc

$!-----------------------------------------------------------------------------
$ create tstifmessage.pdf
procedure
refgbl $autousage
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
let $autousage = "no"
FLAG-ADD NOMESSAGE
WRITE "NO MESSAGES THE FIRST TIME"
tifmessage 
FLAG-DELETE NOMESSAGE
WRITE "MESSAGES FROM FORTRAN AND C THE SECOND TIME."
tifmessage
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create ifmessage.hlp
1 IFMESSAGE

  Display a message unless the the TAE NOMESSAGE flag was set (with
  FLAG-ADD NOMESSAGE).  This may be done, for example, to display the
  program version unless NOMESSAGE is set.

  FORTRAN Calling Sequence:  call ifmessage( msg )
  C Calling Sequence:        zifmessage( msg );

  The argument msg is REQUIRED. 
  In Fortran, it must be a quoted string or CHARACTER type.
  In C, it must be a quoted string or any other null-terminated string.

  IFMESSAGE calls XVP to check the bit of the TAE global variable $switch
  that is the NOMESSAGE flag.  If the bit is not set, it calls XVMESSAGE to 
  print the message.  (See the VICAR online help for FLAG for info on the flag.)

2 History

 93-7-16  ...SP.... INITIAL VERSION

  Original Programmer: STEVE POHORSKY  July 16, 1993
  Current Cognizant Programmer: Steve Pohorsky
  Source Language: Fortran (or C for zifmessage)
$ Return
$!#############################################################################
