$!****************************************************************************
$!
$! Build proc for MIPL module gtprcs
$! VPACK Version 1.9, Monday, December 07, 2009, 16:22:23
$!
$! Execute by entering:		$ @gtprcs
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
$ write sys$output "*** module gtprcs ***"
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
$ write sys$output "Invalid argument given to gtprcs.com file -- ", primary
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
$   if F$SEARCH("gtprcs.imake") .nes. ""
$   then
$      vimake gtprcs
$      purge gtprcs.bld
$   else
$      if F$SEARCH("gtprcs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gtprcs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gtprcs.bld "STD"
$   else
$      @gtprcs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gtprcs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gtprcs.com -mixed -
	-s gtprcs.c -
	-i gtprcs.imake -
	-t tgtprcs.f tzgtprcs.c tgtprcs.imake tgtprcs.pdf tstgtprcs.pdf -
	-o gtprcs.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gtprcs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  This is the gtprcs subroutine for all Operating Systems.
    This subroutine returns a pointer to the current user's real 
    name.  If the user's real name cannot be found then a blank
    character string is returned.                                 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "vmachdep.h"
#include <string.h>

/*    Fortran Callable Subroutine    */

void zgtprcs(char s[8]);

char FTN_NAME2(gtprcs, GTPRCS) (char s[8], ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     char c_string[8];
     zgtprcs(c_string);
     zsc2for(c_string,8,s,&s,1,1,1, s);
}

/*    C-Callable Subroutine     */

void zgtprcs(s)
char s[8];
{
     char *cuserid_p2();

     {
          strcpy(s,cuserid_p2());
     }

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gtprcs.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY gtprcs

   To Create the build file give the command:

	$ vimake gtprcs                     (VMS)
   or
	% vimake gtprcs                     (Unix)


*************************************************************************/

#define SUBROUTINE gtprcs

#define MODULE_LIST gtprcs.c 

#define USES_ANSI_C

#define P2_SUBLIB

$ Return
$!#############################################################################
$Test_File:
$ create tgtprcs.f
C  THIS IS A TEST PROGRAM FOR THE FORTRAN CALLABLE PORTION OF THE
C  GTPRCS SUBROUTINE

        SUBROUTINE TGTPRCS()

	CHARACTER*8 B
        CHARACTER*30 MSG

	CALL GTPRCS(B)
	WRITE(MSG,100) B
 100    FORMAT(A8)
        CALL XVMESSAGE(MSG,' ')
        CALL XVMESSAGE(' ',' ')
	RETURN
	END
$!-----------------------------------------------------------------------------
$ create tzgtprcs.c
/*  This is a test program for the FORTRAN Callable portion of the
    gtprcs subroutine.                                               */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     char b[8], msg[8];

     zvmessage(" "," ");
     zvmessage("Test the C Interface"," ");
     zvmessage(" "," ");

     zgtprcs(b);
     sprintf(msg,"%s",b);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zvmessage(" "," ");
     zvmessage("Test the FORTRAN Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tgtprcs)();

}
$!-----------------------------------------------------------------------------
$ create tgtprcs.imake
/* Imake file for Test of VICAR subroutine gtprcs */

#define PROGRAM tgtprcs

#define MODULE_LIST tgtprcs.f tzgtprcs.c

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define LIB_LOCAL         /* Comment out upon delivery */
$!-----------------------------------------------------------------------------
$ create tgtprcs.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstgtprcs.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
write "Results may differ from one Operating System to another."
tgtprcs
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create gtprcs.hlp
1 GTPRCS

  PURPOSE - GTPRCS will return the process name or user id. 

  FORTRAN CALLING SEQUENCE  - CALL GTPRCS(B)
                              CHARACTER*8 B

         C CALLING SEQUENCE - zgtprcs(b)
                              char b[8]

2  ARGUMENTS

      B(b) is a character string of size 8.  Upon return this array 
      will contain the user ID or process name in ASCII. 

2  HISTORY

      Original Programmer: Gary Yagi, 16 January 1977
      Current Cognizant Programmer: Damon Knight,  5 October 1993
      Source Language: C 
      Ported to Unix:  Damon Knight,  5 October 1993

      Aug. 30, 2000 AXC  Added Macros to Fortran calllable subroutine.
                         (AR-104622)

2  OPERATION

      GETPRCS gets the process or user ID name and returns it in a
      character string.  If the subroutine is not capable of finding
      the process name or user ID, a blank character string is returned.

$ Return
$!#############################################################################
