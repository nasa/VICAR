$!****************************************************************************
$!
$! Build proc for MIPL module vwait
$! VPACK Version 1.9, Monday, December 07, 2009, 16:42:41
$!
$! Execute by entering:		$ @vwait
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
$ write sys$output "*** module vwait ***"
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
$ write sys$output "Invalid argument given to vwait.com file -- ", primary
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
$   if F$SEARCH("vwait.imake") .nes. ""
$   then
$      vimake vwait
$      purge vwait.bld
$   else
$      if F$SEARCH("vwait.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vwait
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vwait.bld "STD"
$   else
$      @vwait.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vwait.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vwait.com -mixed -
	-s vwait.mar zvwait.c vwait.c -
	-i vwait.imake -
	-t tvwait.c tvwaitf.f tvwait.imake tvwait.pdf tstvwait.pdf -
	-o vwait.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vwait.mar
$ DECK/DOLLARS="$ VOKAGLEVE"
	.psect vwait
	.title vwait
;
;	CALL VWAIT(N)
;	INTEGER*4 N
;
;  wait for N x 0.01 seconds.
;  (max. time = # 10**-4-sec-units per longword = 100 hours)
;
;  83-11-22  ...LWK...
;
	.entry vwait,^m<>
;
	movl	@4(ap),r0		; store N
	clrl	r1			; clear hi bits
	mull2	#100,r0			; convert to 10**-4 sec units
	ashq	#10,r0,r0		; convert to 100-nanosec in [r1,r0]
	mcoml	r1,r1			; complement hi 32 bits
	mcoml	r0,r0			; complement lo 32 bits
	incl	r0			; add 1 for negate
	bcc	10$			; branch if no carry
	incl	r1			; else carry to hi part
10$:	movq	r0,deltim		; store result
	$setimr_s #0,deltim,,		; set timer
	$waitfr_s #0			; wait
	movl	#ss$_normal,r0		; normal return code
	ret
;
	.PSECT	VWAIT_DATA,NOEXE,WRT,QUAD
deltim:	.blkq	1
;
	.end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zvwait.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


void zvwait( hundredths )
int hundredths;	
{
FTN_NAME(vwait)( &hundredths);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vwait.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*    suspend execution for HUNDREDTHS x 0.01 (wall clock) seconds.  */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/time.h>

#include "xvmaininc.h"
#include "vmachdep.h"
#include "ftnbridge.h"

#ifndef NULL
#define NULL ((void *)0)
#endif

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/
void zvwait(int hundredths );


void FTN_NAME2(vwait, VWAIT) ( hundredths )
  int *hundredths;	
{
   zvwait( *hundredths );
}

/************************************************************************/
/* C-Callable Version       The method using select was proposed by Alan Mazer
            as the most universal under UNIX.  usleep is an alternative where
            available, but neither usleep or select appear to be in ANSI C.*/
/************************************************************************/

void zvwait(int hundredths )
{
#if SELECT_AVAIL_OS
    struct timeval timeout;

    timeout.tv_sec  = hundredths/100;   /*  run a timer on a NULL pipe  */
    timeout.tv_usec = (hundredths*10000) % 1000000;
    (void)select(64,(fd_set*)NULL,(fd_set *)NULL,(fd_set *)NULL,
	&timeout); /*64 used instead of getdtablesize which is not a universal*/
#else
    sleep( (hundredths+50)/100 );     /*  perhaps this is good enough.
           				  Resolution of 1 second.  */
#endif
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vwait.imake
/* Imake file for VICAR subroutine VWAIT */

#define SUBROUTINE vwait

#if VMS_OS
#define MODULE_LIST vwait.mar zvwait.c
#define CLEAN_OTHER_LIST vwait.c 
#else
#define MODULE_LIST vwait.c 
#define CLEAN_OTHER_LIST vwait.mar zvwait.c 
#endif

#define P2_SUBLIB

#define USES_C
#if VMS_OS
#define USES_MACRO
#endif
$ Return
$!#############################################################################
$Test_File:
$ create tvwait.c
#include "vicmain_c"
#include "ftnbridge.h"
#include <time.h>

/* Program to test subroutine vwait().                                       */
main44()
{
   char msg[80];
   time_t time1, time2;
   double delta;
/*  ==================================================================  */
   zvmessage("\nTesting VWAIT subroutine C interface\n", "");
   zvmessage("**suspend execution for approximately 10-seconds", "");
   time(&time1);
   zvwait(1000);   /*  10 times 100  */
   time(&time2);
   delta = difftime(time2,time1);
   sprintf(msg, "Time interval was approximately %3.0f seconds", delta);
   zvmessage("It generally should be 10 or 11 seconds", "");
   zvmessage(msg, "");

   zvmessage("\nTesting VWAIT subroutine Fortran interface\n", "");
   zvmessage("**suspend execution for approximately 10-seconds", "");
   time(&time1);
   FTN_NAME(tvwaitf)();
   time(&time2);
   delta = difftime(time2,time1);
   sprintf(msg, "Time interval was approximately %3.0f seconds", delta);
   zvmessage(msg, "");

   exit(0);
}

$!-----------------------------------------------------------------------------
$ create tvwaitf.f
c  test subroutine VWAIT
      SUBROUTINE TVWAITF
C==================================================================
      CALL vwait(1000)    ! 10 seconds * 100 ticks/sec
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tvwait.imake
/* Imake file for test of VICAR subroutine vwait */

#define PROGRAM tvwait

#define MODULE_LIST tvwait.c tvwaitf.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tvwait.pdf
! pdf for test pgm for subroutine VWAIT
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstvwait.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage = "no"
! test for subroutine VWAIT
tvwait
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create vwait.hlp
1 VWAIT

PURPOSE:   Suspends execution of the calling program for a
           specified interval of time.

Fortran Call:   CALL VWAIT(N)
C Call:         zvwait(n);

PARAMETERS:

     N   =  Is a fullword integer expression containing the number of
            0.01 second intervals to suspend program execution.
            Maximum time = 100 hours.  For zvwait, n is passed by value.

2 NOTES

DESCRIPTION

This routine provides a function
to suspend execution of a program for a specified time interval. The
argument N is in units of hundredths of a second of wall clock time.

HISTORY

  Original Programmer: G.M. Yagi     1977-01-17
  Converted to Vax:    L.W. Kamp     1983-11-22
  Current Cog Progr:   L.W.Kamp
  Ported to UNIX:      S. Pohorsky   1992-12-21

$ Return
$!#############################################################################
