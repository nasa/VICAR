$!****************************************************************************
$!
$! Build proc for MIPL module get_seconds
$! VPACK Version 1.9, Monday, December 07, 2009, 16:19:26
$!
$! Execute by entering:		$ @get_seconds
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
$ write sys$output "*** module get_seconds ***"
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
$ write sys$output "Invalid argument given to get_seconds.com file -- ", primary
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
$   if F$SEARCH("get_seconds.imake") .nes. ""
$   then
$      vimake get_seconds
$      purge get_seconds.bld
$   else
$      if F$SEARCH("get_seconds.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake get_seconds
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @get_seconds.bld "STD"
$   else
$      @get_seconds.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create get_seconds.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack get_seconds.com -mixed -
	-s get_seconds.c -
	-i get_seconds.imake -
	-t tget_seconds.f tget_seconds.imake tget_seconds.pdf -
	   tstget_seconds.pdf -
	-o get_seconds.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create get_seconds.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************
 * get_seconds.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>
#include <stdio.h>
#include <time.h>

#define Y2K_CORRECTION 0X7FFFFFFF

/* Y2K_CORRECTION will turn off the first bit when time returned by
   time() function will be greater then 2**31 - 1, thus forcing the
   value to be positive in 2's complement for the calling fortran
   program. */

#define TEST_PURPOSE   0X00000000

/* Only used for purpose of testing. Set the flag to some hex value such
   as 0XC0000000 to forward time in future. */
/************************************************************************/
/* Fortran-Callable Version (no C-version needed)                       */
/************************************************************************/


void FTN_NAME2_(get_seconds, GET_SECONDS) (sec_out)
long *sec_out;
{
  time_t thetime;
  char msg[100];
  thetime = time(NULL);

  if(TEST_PURPOSE) {
     zvmessage("*************************************************","");
     zvmessage("*** This is test of get_seconds for Y2K only. ***","");
     zvmessage("*** This is simulation of how program will    ***","");
     zvmessage("*** behave once the time function starts      ***","");
     zvmessage("*** returning values greater then 2**31-1     ***","");
     zvmessage("*************************************************","");
     sprintf(msg,"Time returned by time function is %u",(int)thetime);
     zvmessage(msg,"");
     thetime+=TEST_PURPOSE;
  }

  *sec_out = (long) (thetime & Y2K_CORRECTION);

  if(TEST_PURPOSE) {
    sprintf(msg,"Time after offseted by get_seconds is %u",(int)thetime);
     zvmessage(msg,"");
     sprintf(msg,"Time after corrected by get seconds is %d",(int)*sec_out);
     zvmessage(msg,"");
  }

  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create get_seconds.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY get_seconds

   To Create the build file give the command:

	$ vimake get_seconds                     (VMS)
   or
	% vimake get_seconds                     (Unix)


*************************************************************************/

#define SUBROUTINE get_seconds

#define MODULE_LIST get_seconds.c

#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tget_seconds.f
        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 time_sec
        character*100 msg
       
        call get_seconds( time_sec )
        write(msg,100) time_sec
        call xvmessage(msg,' ') 

 100    format(' The GMT time in seconds since 1-JAN-1970 is: ', I)

	return
	end
$!-----------------------------------------------------------------------------
$ create tget_seconds.imake
/***********************************************************************

                     IMAKE FILE FOR TEST PROGRAM tget_seconds

   To Create the build file give the command:

	$ vimake tget_seconds                     (VMS)
   or
	% vimake tget_seconds                     (Unix)

*************************************************************************/

#define PROGRAM tget_seconds

#define MODULE_LIST tget_seconds.f

#define TEST

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

#define USES_FORTRAN
#define MAIN_LANG_FORTRAN
/* #define LIB_LOCAL DISABLE DURING DELIVERY. ONLY FOR Y2K TEST PURPOSE */
$!-----------------------------------------------------------------------------
$ create tget_seconds.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstget_seconds.pdf
procedure
refgbl $echo
body
write ""
write "Results will vary depending on what system you are using"
write ""
write "*******************************************************"
write "For Y2K testing, you need to modify the source code for"
write "get_seconds by setting TEST_PURPOSE to 0XC000000 or higher"
write "value and including LIB_LOCAL in tget_seconds.imake."
write "*******************************************************"
write ""

let _onfail="continue"
let $echo="yes"
tget_seconds
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create get_seconds.hlp
1 get_seconds

  Return current date and time expressed as the number of seconds elapsed since
  GMT 00:00:00 on January 1, 1970, as long as number of seconds are less then
  2**31. If number of seconds exceeds 2**31-1, get_seconds will return number
  of seconds minus 2**31. This change is legitimate, since the sole usage of
  this function is to obtain a random seed and not time, this change should
  not effect functionality of progrmas using it.  get_seconds was developed
  to provide a portable Fortran interface to obtain the current time as a
  random seed for programs like fracgen which use current time to randomize
  the random number generator.
  This subroutine is callable only from Fortran.

  FORTRAN Calling Sequence:  CALL get_seconds( SEC_OF_TIME )

  Arguments: INTEGER*4 SEC_OF_TIME   ouput integer.
             Seconds of elapsed time since GMT 00:00:00 on 1-JAN-1970. 
              

2 Operation

  get_seconds calls the C time function to obtain the current dat and time 
  expressed as the number of seconds elapsed since GMT 00:00:00 on 1-JAN-1970.
  

2 History

  Original Programmer: C. Randy Schenk (CRI) (1-July-1994)
  Current Cognizant Programmer:
  Source Language: C
  Revisions:
             Removed LIB_LOCAL as per FR85769   (CRI) 7-Mar-1995
  06-18-98   RRP Made it y2k compatible.
$ Return
$!#############################################################################
