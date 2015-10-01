$!****************************************************************************
$!
$! Build proc for MIPL module get_ran
$! VPACK Version 1.9, Monday, December 07, 2009, 16:19:17
$!
$! Execute by entering:		$ @get_ran
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
$ write sys$output "*** module get_ran ***"
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
$ write sys$output "Invalid argument given to get_ran.com file -- ", primary
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
$   if F$SEARCH("get_ran.imake") .nes. ""
$   then
$      vimake get_ran
$      purge get_ran.bld
$   else
$      if F$SEARCH("get_ran.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake get_ran
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @get_ran.bld "STD"
$   else
$      @get_ran.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create get_ran.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack get_ran.com -mixed -
	-s get_ran.c -
	-i get_ran.imake -
	-t tget_ran.f tget_ran.imake tget_ran.pdf tstget_ran.pdf -
	-o get_ran.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create get_ran.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************
 * get_ran.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* Fortran-Callable Version (no C-version needed -- use memset)         */
/************************************************************************/

void FTN_NAME2_(get_ran, GET_RAN) (seed1,randout)
int *seed1;
float *randout;
{
  int i;
  srand((unsigned) *seed1);  
  i = ((unsigned int) rand());
  if (i >= 32767) *randout = (float) (i%32768)/32767.0;
  else *randout = (float) (i/32767.0);
  *seed1 = ((*seed1)*1103515245+12345);
  *seed1 = (unsigned int) (*seed1/65536)%32768;
  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create get_ran.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY get_ran

   To Create the build file give the command:

	$ vimake get_ran                     (VMS)
   or
	% vimake get_ran                     (Unix)


*************************************************************************/

#define SUBROUTINE get_ran

#define MODULE_LIST get_ran.c

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tget_ran.f
        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 seed1
        character*200 msg
        real randout
        seed1 = 15289

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

 100    format(' The input seed value: ', I)
 101    format(' The output seed value: ', F12.6)

	return
	end
$!-----------------------------------------------------------------------------
$ create tget_ran.imake
/***********************************************************************

                     IMAKE FILE FOR TEST PROGRAM tget_ran

   To Create the build file give the command:

	$ vimake tget_ran                     (VMS)
   or
	% vimake tget_ran                     (Unix)

*************************************************************************/

#define PROGRAM tget_ran

#define MODULE_LIST tget_ran.f

#define TEST

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

#define USES_FORTRAN
#define MAIN_LANG_FORTRAN
$!-----------------------------------------------------------------------------
$ create tget_ran.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstget_ran.pdf
procedure
refgbl $echo
body
write "Results will vary depending on what system you are using"
let _onfail="continue"
let $echo="yes"
tget_ran
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create get_ran.hlp
1 GET_RAN

  Return a random number with a value between 0.0 and 1.0.  This subroutine
  was developed to replace the VAX/VMS Fortran function, RAN.  This subroutine
  is callable only from Fortran.

  FORTRAN Calling Sequence:  CALL GET_RAN( SEED, RANDOUT)

  Arguments: INTEGER*2 SEED is used for a new sequence of random numbers. 
             REAL RANDOUT a random number between 0.0 and 1.0. 

2 Operation

  GET_RAN calls the C srand function to intialize the C rand function with 
  its seed value and then generate a random number.

2 History

  Original Programmer: Damon D. Knight (Dec 1, 1993)
  Current Cognizant Programmer: Damon D. Knight (Dec 1, 1993)
  Source Language: C

$ Return
$!#############################################################################
