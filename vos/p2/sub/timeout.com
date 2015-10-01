$!****************************************************************************
$!
$! Build proc for MIPL module timeout
$! VPACK Version 1.9, Friday, November 13, 1998, 16:04:34
$!
$! Execute by entering:		$ @timeout
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
$ write sys$output "*** module timeout ***"
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
$ write sys$output "Invalid argument given to timeout.com file -- ", primary
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
$   if F$SEARCH("timeout.imake") .nes. ""
$   then
$      vimake timeout
$      purge timeout.bld
$   else
$      if F$SEARCH("timeout.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timeout
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timeout.bld "STD"
$   else
$      @timeout.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timeout.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timeout.com -
	-s timeout.c -
	-i timeout.imake -
	-t ttimeout.c ttimeout.imake ttimeout.pdf tsttimeout.pdf -
	-o timeout.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timeout.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 | TIMEOUT.C  -- Routine to generate an ASCII string of the form:	     |
 |			"YYYY.DAY HH:MM:SS.MSC"				     |
 |		 given year, day, hour, minutes, seconds, and milliseconds   |
 |		 as inputs.						     |
 *===========================================================================*/

#include <stdio.h>
#include <string.h>

timeout(date_buf,string)
int date_buf[6];	/* input buffer containing year,day,hour,etc..*/
char string[25];	/* output ASCII string */
{
	int year,day,hour,minute,second,msec;

	day = year = date_buf[0];
        if(!(year/1000)) { /* year not four digit */
          if(year<=20) year+=2000;
          else year+=1900;
          sprintf(string,"Year=%d changed to %d.",day,year);
          zvmessage(string,0);
        }
	day = date_buf[1];
	hour = date_buf[2];
	minute = date_buf[3];
	second = date_buf[4];
	msec = date_buf[5];

	sprintf(string,"%04d.%03d %02d:%02d:%02d.%03d",year,day,hour,minute,second,msec);

	strcat(string,"\n");
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create timeout.imake
/* Imake file for VICAR subroutine TIMEOUT */

#define SUBROUTINE  timeout 

#define MODULE_LIST timeout.c

#define USES_ANSI_C
#define P2_SUBLIB

$ Return
$!#############################################################################
$Test_File:
$ create ttimeout.c
/*
 *	ttimeout.c
 *
*/
#include "vicmain_c"

void main44()
{
	int count, def;
	int date[6];
	char string[25];


	zvparm("DATE", date, &count, &def, 0, 0);

	timeout(date,string);
	zvmessage(string,0);
}
$!-----------------------------------------------------------------------------
$ create ttimeout.imake
/* Imake file for VICAR program ttimeout */

#define PROGRAM  ttimeout 

#define MODULE_LIST ttimeout.c

#define MAIN_LANG_C
#define TEST

#define USES_ANSI_C
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

#define LIB_LOCAL

$!-----------------------------------------------------------------------------
$ create ttimeout.pdf
process
Parm DATE type=Integer count=6 default=(1994,310,10,40,50,10)
end-proc
$!-----------------------------------------------------------------------------
$ create tsttimeout.pdf
procedure  help=*
refgbl $echo
body
let _onfail="continue"
let $echo="no"
Write " "
Write " The Test Data can be modified to suit the test. "
Write " To Change the test data, simply modify the"
Write " Default parameters in the ttimeout.pdf file"
Write " "
Write " "
Write " *************TEST ON TIMEOUT ROUTINE************"
Write " "

ttimeout date=(2000,121,10,20,30,40)
ttimeout date=(12,121,10,20,30,40)
ttimeout date=(1980,198,35,42,16,53)
ttimeout date=(80,198,35,42,16,53)

Write " "
Write " *************TEST COMPLETED************"

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create timeout.hlp
1 TIMEOUT

	timeout generates an ASCII string of the form:
		     "YYYY.DAY HH:MM:SS.MSC"
	given year, day, hour, minutes, seconds, and milliseconds as
	inputs.

        NOTE : The year should be 4-digit. If a two digit year is supplied
        then year=2000+year if year<=20, otherwise year=1900+year.

	Calling sequence:   int date_buf[6];
			    char  string[25];

			    timeout(date_buf,string)

	Input:	date_buf[6]	- integer array of size 6
		string[25]	- string caharacters of size 25
		

	Output:	Converted ASCII string of the form:
		"YYYY.DAY HH:MM:SS.MSC"

	Status returns:	none

2 History

Original programmer:	Raymond Lin
Cognizant programmer:	Steve Pohorsky
Source language:	C

Ported to UNIX by:	Raymond Lin,	Oct-1994
Y2K Complient           Rajesh Patel,   Nov-1998
$ Return
$!#############################################################################
