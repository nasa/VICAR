$!****************************************************************************
$!
$! Build proc for MIPL module usedisp
$! VPACK Version 1.9, Tuesday, May 27, 2014, 15:06:34
$!
$! Execute by entering:		$ @usedisp
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
$ write sys$output "*** module usedisp ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to usedisp.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("usedisp.imake") .nes. ""
$   then
$      vimake usedisp
$      purge usedisp.bld
$   else
$      if F$SEARCH("usedisp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake usedisp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @usedisp.bld "STD"
$   else
$      @usedisp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create usedisp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack usedisp.com -mixed -
	-s usedisp.c -
	-i usedisp.imake -
	-p showdisp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create usedisp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*	USEDISP.C - Program to allocate a MIPL display device
 *
 *	Description:
 *
 *		USEDISP allocates and deallocates display devices for
 *	the VRDI.  Along with taking care of the allocation tasks, USEDISP 
 *	also must assign logical names to point to the physical device.
 *	The information that this program needs for each device
 *	on the system is kept in the file VRDI$LIB:XDDEVICE.DIB (on VMS)
 *	or $VRDIDIB/xddevice.dib (on Unix).
 *
 *	History:
 *
 *		Sep 30, 1984 - Original code by Tom Wolfe (in FORTRAN)
 *		Jul  2, 1986 - Rewritten in "C"
 *			     - Changed CMKRNL to CMEXEC
 *			     - Removed need for TERMINAL symbol
 *			     - Removed need for MIPLVAX symbol
 *				(It is still used if there however.)
 *		May  4, 1987 - Rewritten to call XDDAllocate, XDDFree, etc
 *			       Renamed USEDISP
 *
 *	Notes:
 *
 *	USEDISP should be installed for whoever will use it as 
 *		four foreign commands.
 *
 *		$ use*disp	:== $vrdi$lib:usedisp a
 *		$ free*disp	:== $vrdi$lib:usedisp d
 *		$ showdisp	:== $vrdi$lib:usedisp s
 *		$ helpdisp	:== $vrdi$lib:usedisp h
 *
 *	Routines:
 *
 *		main		 - Parses command line and executes
 *				   the specified command.
 *		ShowDevices	 - Shows information about the devices
 *				   on the current system.
 *		ShowHelp	 - Displays a brief summary of valid
 *				   USEDISP commands.
 *		UpperString	 - Converts a string to uppercase.
 *		UseError	 - Prints out a message and exits.
 *		Information	 - Prints out an informational message.
 */

#include "xvmaininc.h"

#include "xdexterns.h"
#include "xdalloc.h"
#include "xderrors.h"

#define	HELP		1
#define	SHOW		2
#define	ALLOCATE	3
#define	DEALLOCATE	4

#define	SUCCESS		1

char Message[81];
#if UNIX_OS
static char display[256];	/* used in putenv call for DISPLAY */
#endif

void UpperString();
void UseError();
void Information();
void ShowDevices();
void ShowHelp();

/*	The main routine parses the command line and calls a routine
 *	to perform the requested function.
 */
int main( Argc, Argv )
int	Argc;				/* Arguement count		*/
char	*Argv[];			/* Arguement values (strings)	*/
{
  int	Command;
  int	ArgPtr;				/* Arguement being worked on	*/
  int	i;
  int	status;

/*  Parse the command line.						*/

  ArgPtr = 1;				/* check for DEBUG flag		*/

  for (i = 0; i < Argc; i++) {		/* Convert Args to uppercase	*/
    UpperString( Argv[i] );
  }

/*	Try to "MATCH" the possible commands				*/

  if (Argc < ArgPtr+1) {		/* No Args -> Command is HELP	*/
    Command = HELP;
  }
  else if ( MATCH(Argv[ArgPtr],"?",1) || MATCH(Argv[ArgPtr],"SHOW",1) ) {
    Command = SHOW;
  }
  else if ( MATCH(Argv[ArgPtr],"HELP",1) ) {
    Command = HELP;
  }
  else if ( MATCH(Argv[ArgPtr],"ALLOCATE",1) ) {
    Command = ALLOCATE;
  }
  else if ( MATCH(Argv[ArgPtr],"DEALLOCATE",1) ) {
    Command = DEALLOCATE;
  }
  else {				/* No command found		*/
    UseError( "Unknown command.  Use 'usedisp help' for help." );
  }

/*  Check for valid number of parameters.				*/

  if ( ((Command == ALLOCATE) || (Command == DEALLOCATE))
         && (Argc < 3) ) {
    UseError( "Invalid number of parameters.  Use 'usedisp help' for help." );
  }

/*  Execute Command.							*/

  switch( Command ) {
    case ALLOCATE: {
      status = zdeaction ( 2, 2, 2 );
#if UNIX_OS			/* allow X display name under Unix */
      if (Argc == 4) {
        sprintf(display, "DISPLAY=%s", Argv[3]);
        putenv(display);
      }
#endif
      if ( status == SUCCESS ) {
        status = zddallocate( Argv[ArgPtr+1] );
        if (status == SUCCESS) {
	  printf( "   %s allocated\n", DIB[DeviceNumber]->DeviceName );
	}
	else if (status == CANNOT_ALLOC_DEVICE) {
	  status = GetDeviceInfo();
	  if (status == SUCCESS) {
	    sprintf ( Message, "%s is allocated to %s at %s.",
    		    Argv[ArgPtr+1], DEVICE.UserName, DEVICE.UserTerminal );
	    Information( Message );
	  }
	  else {
	    zdesignal ( status );
	  }
        }
      }
      break;
    }
    case DEALLOCATE: {
      status = zdeaction ( 2, 2, 2 );
      if ( status == SUCCESS ) {
	status = zddfree( Argv[ArgPtr+1] );
	if ( status == SUCCESS ) {
	  printf( "   %s deallocated\n", DIB[DeviceNumber]->DeviceName );
	}
      }
      break;
    }
    case SHOW: {
      status = zdeaction ( 2, 2, 2 );
      if ( status == SUCCESS ) {
	status = GetMasterPID( 0, &UserPID );
	if ( status == SUCCESS ) {
	  status = GetUserVAX( &UserVAX );
	  if ( status == SUCCESS ) {
	    status = XD_Read_DIBs();
	    if ( status == SUCCESS ) {
	      status = GetDeviceInfo();
	      if ( status == SUCCESS ) {
		ShowDevices();
		break;
	      }	
	    }
	  }
	}
      }
      zdesignal ( status );
      break;
    }
    case HELP: {
      ShowHelp();
      break;
    }
  }
  exit(0);
}

/*	Displays information about each display device on the system.
 *	Only devices connected to the computer that this is running on are
 *	showed.  The information that is displayed includes the
 *	terminal and user name of the person that owns the device (if
 *	any).
 */
void ShowDevices()
{
  int DevNumber;

/*	Print a nice header.						*/
  printf( "\n" );
  printf( "Device        User                     Device            \n" );
  printf( " Name   Term  Term    Username          Type       Res.  \n" );
  printf( "------  ----  ----  ------------  ---------------  ----  \n" );
   
/*	For each device in the device table...				*/

  /* Note: this was: (DIB[DevNumber]->DeviceName==" ")  which is	*/
  /* undefined.  I assume the strcmp is the same.  rgd 2014/05/27	*/
  for ( DevNumber = 0; ((DevNumber < TotalDevices) || 
        (strcmp(DIB[DevNumber]->DeviceName," ") != 0));
        DevNumber++ ) {
    if ((DIB[DevNumber]->SystemNumber == UserVAX) ||
        (DIB[DevNumber]->SystemNumber == -1)) {/* if on this computer */
      printf( "%-6.6s  ", DIB[DevNumber]->DeviceName );
      printf( "%-4.4s  ", DIB[DevNumber]->Terminal );
      printf( "%-4.4s  ", Devices[DevNumber].UserTerminal );
      printf( "%-12.12s  ", Devices[DevNumber].UserName );
      printf( "%-15.15s  ", DIB[DevNumber]->Make );

/*	Check resolution.						*/

      switch ( DIB[DevNumber]->Resolution ) {
	case LOW_RESOLUTION: {
	  printf( "Low   " );
	  break;
	}
	case HIGH_RESOLUTION: {
	  printf( "High  " );
	  break;
	}
	case BOTH_RESOLUTIONS: {
	  printf( "Both  " );
	  break;
	}
      }
      printf( "\n" );
    }
  }
  printf( "\n" );
}

/*	Displays a brief summary of the commands available in USEDISP.
 */
void ShowHelp()
{
  printf( "\n" );
  printf( "Valid usedisp commands are:\n" );
  printf( "\n" );
  printf( "     use device    - allocates a device\n" );
#if UNIX_OS
  printf( " or  use device display - allocates device on a given X display\n");
#endif
  printf( "     free device   - deallocates a device\n" );
  printf( "     showdisp      - outputs device information\n" );
  printf( "     helpdisp      - displays this message\n" );
  printf( "\n" );
  printf( "     where device  = a physical device name (i.e. EPA1)\n" );
  printf( "                   = a generic device name (i.e. EP)\n" );
  printf( "                   = or DEFAULT, to get device associated\n" );
  printf( "                     with your terminal\n" );
#if UNIX_OS
  printf( " and where display = an X-windows display name to use\n" );
#endif
  printf( "\n" );
}

/*************************** UTILITIY ROUTINES **************************/

void UpperString( String )
char	*String;
{
  int	i, Length;

  Length = strlen( String );
  for ( i = 0; i < Length; i++ ) {
    if (islower(String[i]))
      String[i] = toupper(String[i]);
  }
}

void UseError( Message )
char	*Message;
{
  Information( Message );
  exit(1);
}

void Information( Message )
char	*Message;
{
  printf( "USEDISP: %s\n", Message );
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create usedisp.imake
#define PROGRAM usedisp
#define MODULE_LIST usedisp.c

#define MAIN_LANG_C
#define USES_C

#if VMS_OS
#define C_OPTIONS /define=VRDI
#define LINK_OPTIONS /notraceback
#else
#define C_OPTIONS -DVRDI
#endif

#define LIB_VRDI_NOSHR
#define LIB_RTL_NOSHR
#define LIB_TAE_NOSHR
$ Return
$!#############################################################################
$PDF_File:
$ create showdisp.pdf
!------------------------------------------------------------------------
!   SHOWDISP.PDF
!
!   Allocate/Deallocate a display device to a user
!
!   The program VRDI$LIB:USEDISP must be installed with WORLD and CMEXEC
!   privileges in order to run on VMS.
!
!   This has been renamed from USEDISP.PDF to avoid conflicts with the
!   TAE intrinsic commands USEDISP and FREEDISP on VMS.  The USE and FREE
!   functionality is retained in this program for when it is used at
!   the DCL level, or from Unix.  All USE/FREE commands inside VMS VICAR
!   should go through the TAE intrinsics.
!
!------------------------------------------------------------------------
procedure help=*
refgbl $syschar
SUBCMD SHOW
END-SUBCMD
SUBCMD HELP
END-SUBCMD
body
if (_subcmd = "SHOW")
  if ($syschar(1) = "UNIX")
    ush $VRDILIB/usedisp s
  else
    dcl showdisp 
  end-if
else
  if ($syschar(1) = "UNIX")
    ush $VRDILIB/usedisp h
  else
    dcl helpdisp 
  end-if
end-if
end-proc
.title
VICAR program SHOWDISP
.help
SHOWDISP is a program that displays information about the image
display devices.

Invocation:   SHOWDISP
              HELPDISP

where:        SHOWDISP    outputs device information
              HELPDISP    outputs a brief summary of USEDISP,
                          FREEDISP, and SHOWDISP.
.level1
.SUBCMD SHOW
SHOWDISP outputs device
information
.SUBCMD HELP
HELPDISP outputs a summary of
USEDISP, FREEDISP, and SHOWDISP
.level2
.SUBCMD SHOW
The SHOWDISP command outputs display device status information
for the machine you are currectly using.
.SUBCMD HELP
The HELPDISP command outputs a brief summary of the display 
allocation/deallocation programs USEDISP, FREEDISP, and
SHOWDISP.
.end
$ Return
$!#############################################################################
