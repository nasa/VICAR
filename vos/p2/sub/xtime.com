$!****************************************************************************
$!
$! Build proc for MIPL module xtime
$! VPACK Version 1.9, Monday, December 07, 2009, 16:43:12
$!
$! Execute by entering:		$ @xtime
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
$ write sys$output "*** module xtime ***"
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
$ write sys$output "Invalid argument given to xtime.com file -- ", primary
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
$   if F$SEARCH("xtime.imake") .nes. ""
$   then
$      vimake xtime
$      purge xtime.bld
$   else
$      if F$SEARCH("xtime.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xtime
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xtime.bld "STD"
$   else
$      @xtime.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xtime.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xtime.com -mixed -
	-s xtime.c -
	-i xtime.imake -
	-t txtime.f tzxtime.c txtime.imake txtime.pdf tstxtime.pdf -
	-o xtime.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xtime.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <time.h>

/**************************************************************************

This routine returns the current time in the format "HHMMSS".

     Input args: NONE

     Output args: time_now - Pointer to the variable to hold the time string

     Returned status: none

     Modifications:   3-6-95 AMS(CRI) MSTP S/W CONVERSION (VICAR PORTING)

***************************************************************************/

/**************************************************************************/
/*    FORTRAN-Callable Subroutine                                         */
/**************************************************************************/

void FTN_NAME2(xtime, XTIME) (char *time_now, ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     unsigned char c_string[6];

     zxtime(c_string);
     zsc2for((char *)c_string,6,time_now,&time_now,1,1,1, time_now);
}

/**************************************************************************/
/*    C-Callable Subroutine                                               */
/**************************************************************************/

zxtime(time_now)

char *time_now;

{
     time_t time(), t;
     char *temp_time, *ctime();

     t=time(0);
     temp_time=ctime(&t);
     strncpy(time_now, (temp_time + 11), 2);
     strncpy((time_now + 2), (temp_time + 14), 2);
     strncpy((time_now + 4), (temp_time + 17), 2);
     *(time_now + 6) = '\0';

     return;

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xtime.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY xtime

   To Create the build file give the command:

	$ vimake xtime                     (VMS)
   or
	% vimake xtime                     (Unix)


*************************************************************************/

#define SUBROUTINE xtime

#define MODULE_LIST xtime.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create txtime.f
        SUBROUTINE TXTIME()

	CHARACTER*6 TIME_NOW
	CALL XTIME(TIME_NOW)
	CALL XVMESSAGE('The time in HHMMSS format is:',' ')
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(TIME_NOW,' ')
        RETURN
	END
$!-----------------------------------------------------------------------------
$ create tzxtime.c
/* Test for subroutine zxtime for C callable         */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     char msg[7];
     unsigned char time_now[7];

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");
     zvmessage("The time in HHMMSS format is:"," ");
     zvmessage(" "," ");

     zxtime(time_now);
     
     sprintf(msg,"%s", time_now);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zvmessage("Test the Fortran interface"," ");
     zvmessage(" "," ");

     FTN_NAME(txtime)();

}
$!-----------------------------------------------------------------------------
$ create txtime.imake
/* Imake file for Test of VICAR subroutine xtime */

#define PROGRAM txtime

#define MODULE_LIST tzxtime.c txtime.f

#define MAIN_LANG_C
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create txtime.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstxtime.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MODULE XTIME  ** NAME CHANGE **
!THIS USED TO BE IBM ROUTINE TIME WITH A DIFFERENT
!CALLING SEQUENCE AND IS NOW A C SUBROUTINE.
!THE SYSTEM TIME IS RETURNED IN A 6 BYTE ASCII BUFFER AS
!   HHMMSS   (24 HOUR CLOCK)
txtime
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create xtime.hlp
1  XTIME

2  PURPOSE

   Returns the current time as an ascii string of the form "HHMMSS".

2  CALLING SEQUENCE

   FORTRAN CALL

      CHARACTER*6 B
      CALL XTIME(B)

   C CALL

      char B
      ZXTIME(B)

2  ARGUMENTS

      B    is the returned time in the format: (HHMMSS).

2  HISTORY

      Original Programmer: C. Avis, 6 July 1983
      Current Cognizant Programmer: C. Avis
      Source Language: Fortran
      Made portable for UNIX and changed to C Source Language AMS (CRI) 3/6/95

2 OPERATION

       THE NAME HAS BEEN CHANGED 'CAUSE THERE IS A
       VAX FORTRAN LIBRARY SUBROUTINE CALLED TIME.

       CALL C ROUTINE FOR TIME
       CONVERT CHARACTER ARRAY (HH:MM:SS) TO
       CHARACTER ARRAY (HHMMSS)

$ Return
$!#############################################################################
