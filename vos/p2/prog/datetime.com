$!****************************************************************************
$!
$! Build proc for MIPL module datetime
$! VPACK Version 1.9, Friday, July 10, 1998, 17:49:53
$!
$! Execute by entering:		$ @datetime
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
$!   PDF         Only the PDF file is created.
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
$ write sys$output "*** module datetime ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to datetime.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("datetime.imake") .nes. ""
$   then
$      vimake datetime
$      purge datetime.bld
$   else
$      if F$SEARCH("datetime.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake datetime
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @datetime.bld "STD"
$   else
$      @datetime.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create datetime.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack datetime.com -
	-s datetime.c -
	-i datetime.imake -
	-p datetime.pdf -
	-t tstdatetime.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create datetime.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c"
#include  <math.h>
#include <string.h>

/**********************************************************************/
/* PROGRAM: datetime                                                  */
/**********************************************************************/
/*                                                                    */
/*   Program "datetime" is a VICAR applications program which types   */
/*   current local date and time.                                     */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/* HISTORY:                                                           */
/*                                                                    */
/*   Converted to 'C' and ported to UNIX  by CRI          March 94    */
/*   Prepared in Fortran-77 for MIPL by Steve Pohorsky    Sept  84    */
/*                                                                    */
/*   Jul. 9, 1998 ..T.Huang .. Modified to output a 4-digit year.     */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*                                                                    */
/*   Input args : None                                                */
/*                                                                    */
/*   Output args: None                                                */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/* MAIN PROGRAM                                                       */
/*                                                                    */
/**********************************************************************/


void main44()
{
    long time(), t;

    char  *date_time, *ctime();
    char  aday[3], amonth[4], ayear[5], adate[8];
    char  line[80];

    zifmessage("DATETIME version 09-Jul-1998");

    /* Get current time */
    t = time(0);

    /* Convert to Local in ASCII String  */
    date_time = ctime(&t); 

    /* Gather date and time pieces */
    strncpy(ayear, (date_time + 20), 4);
    strncpy(amonth, (date_time + 4), 3);
    strncpy(aday, (date_time + 8), 2);
    strncpy(adate, (date_time +11), 8);

    sprintf(line, "\nThe date and time are: %.2s-%.3s-%.4s %.8s\0\n",
            aday,amonth,ayear,adate);

    zvmessage(line, "");
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create datetime.imake
#define PROGRAM  datetime

#define MODULE_LIST datetime.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create datetime.pdf
process help=*
!  PDF FILE FOR "datetime"
!

!# annot function="VICAR Utilities"
!# annot keywords=(day,time)
END-PROC
.title
Prints current date and time: dd-mmm-yyyy hh:mm:ss
.help
PURPOSE: "datetime" types the day and time.  No parameters are needed.

EXECUTION: TAE COMMAND LINE FORMAT     "datetime".

Results in "The date and time are: dd-mmm-yyyy  hh:mm:ss" typed at terminal.

 WRITTEN BY:             Steve Pohorsky              19 Sep 1984

 COGNIZANT PROGRAMMER:   Steve Pohorsky              19 Sep 1984

 REVISION:               1                           19 Sep 1984

 Made portable for UNIX  Ralph Richardson (CRI)      01 Apr 1994
 (Rewrote in "C")

 T.Huang  Converted to output 4-digit year date      09 Jul 1998

.LEVEL1
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstdatetime.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM "datetime"
!
datetime
end-proc
$ Return
$!#############################################################################
