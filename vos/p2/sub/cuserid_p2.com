$!****************************************************************************
$!
$! Build proc for MIPL module cuserid_p2
$! VPACK Version 1.9, Wednesday, November 20, 2002, 18:42:24
$!
$! Execute by entering:		$ @cuserid_p2
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
$ write sys$output "*** module cuserid_p2 ***"
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
$ write sys$output "Invalid argument given to cuserid_p2.com file -- ", primary
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
$   if F$SEARCH("cuserid_p2.imake") .nes. ""
$   then
$      vimake cuserid_p2
$      purge cuserid_p2.bld
$   else
$      if F$SEARCH("cuserid_p2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cuserid_p2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cuserid_p2.bld "STD"
$   else
$      @cuserid_p2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cuserid_p2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cuserid_p2.com -mixed -
	-s cuserid_p2.c -
	-i cuserid_p2.imake -
	-t tstcuserid_p2.c tstcuserid_p2.imake tstcuserid_p2.pdf -
	-o cuserid_p2.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cuserid_p2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**
 * cuserid_p2() - simple wrapper to make cuserid() work on all platforms.
 * Needed because some don't implement (or have deprecated) cuserid.
 *
 * Returns the userid of the current process as a string.  The string must
 * be copied or used before calling again - this is NOT reentrant.
 */

#include "xvmaininc.h"
#include <stdio.h>

#if CUSERID_AVAIL_OS == 0
#include <pwd.h>
#endif

char *cuserid_p2()
{
#if CUSERID_AVAIL_OS
    char *cuserid();
    return cuserid(NULL);
#else
    struct passwd *pw;

    pw = getpwuid(getuid());
    return pw->pw_name;
#endif

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cuserid_p2.imake
#define SUBROUTINE cuserid_p2

#define MODULE_LIST cuserid_p2.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tstcuserid_p2.c
/**
 * Routine to test cuserid_p2.  All it does it print the current user - no
 * other test is possible or useful.
 */
#include "vicmain_c"
#include <stdio.h>

main44()
{
    printf("current user is: '%s'\n", cuserid_p2());
}

$!-----------------------------------------------------------------------------
$ create tstcuserid_p2.imake
/* Imake file for Test of VICAR subroutine cuserid_p2 */

#define PROGRAM tstcuserid_p2

#define MODULE_LIST tstcuserid_p2.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tstcuserid_p2.pdf
process help=*
END-PROC
$ Return
$!#############################################################################
$Other_File:
$ create cuserid_p2.hlp
1 CUSERID_P2

  Routine to return the current process' user ID as a string.  Just like
  cuserid() but portable to all systems (some don't implement cuserid()).

  C Calling Sequence:  strcpy(buf, cuserid_p2());

  The returned string must be copied or used before calling this routine
  again... it is NOT reentrant.

  FORTRAN Calling Sequence: N/A

  Fortran bridge not implemented yet.

2 History

  Original Programmer: Bob Deen, Novermber 20, 2002
  Current Cognizant Programmer: Bob Deen
  Source Language: C

$ Return
$!#############################################################################
