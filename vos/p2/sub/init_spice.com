$!****************************************************************************
$!
$! Build proc for MIPL module init_spice
$! VPACK Version 1.9, Monday, December 07, 2009, 16:23:48
$!
$! Execute by entering:		$ @init_spice
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
$ write sys$output "*** module init_spice ***"
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
$ write sys$output "Invalid argument given to init_spice.com file -- ", primary
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
$   if F$SEARCH("init_spice.imake") .nes. ""
$   then
$      vimake init_spice
$      purge init_spice.bld
$   else
$      if F$SEARCH("init_spice.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake init_spice
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @init_spice.bld "STD"
$   else
$      @init_spice.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create init_spice.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack init_spice.com -mixed -
	-s gll_spice_env.c init_spice.f initspice.c reset1.f zreset1.c -
	-i init_spice.imake -
	-t tinit_spice.f tzinit_spice.c tinit_spice.imake tinit_spice.pdf -
	   tstinit_spice.pdf -
	-o init_spice.hlp gll_spice_env.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gll_spice_env.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "string.h"
#include "ftnbridge.h"
#include "ms_defines.h"

#define MAX_GLL_STR 132

int zgll_getenv ();

/* FORTRAN bridge for getting GLL-specific SPICE environment variables. */ 
void FTN_NAME2_ (gll_spice_env,GLL_SPICE_ENV) (char *kdb, char *spiceker,
	char *mipsker,
	char *sclk, char *consts, char *bodyids, char *leapsec, int *ind,
	ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char tkdb[MAX_GLL_STR], tspiceker[MAX_GLL_STR], tmipsker[MAX_GLL_STR],
        tsclk[MAX_GLL_STR], tconsts[MAX_GLL_STR], tbodyids[MAX_GLL_STR],
        tleapsec[MAX_GLL_STR];

   *ind = zgll_spice_env (tkdb, tspiceker, tmipsker, tsclk, tconsts, tbodyids, 
                          tleapsec);
   /* failed when *ind < 0 */
   if (*ind >= 0)
   {
      zsc2for (tkdb, 0, kdb, &kdb, 8, 1, 1, ind);
      zsc2for (tspiceker, 0, spiceker, &kdb, 8, 2, 2, ind);
      zsc2for (tmipsker, 0, mipsker, &kdb, 8, 3, 3, ind);
      zsc2for (tsclk, 0, sclk, &kdb, 8, 4, 4, ind);
      zsc2for (tconsts, 0, consts, &kdb, 8, 5, 5, ind);
      zsc2for (tbodyids, 0, bodyids, &kdb, 8, 6, 6, ind);
      zsc2for (tleapsec, 0, leapsec, &kdb, 8, 7, 7, ind);
   }
} 


/* subroutine to retrieve GLL-spceific SPICE environment variables. */
int zgll_spice_env (kdb, spiceker, mipsker, sclk, consts, bodyids,
                    leapsec)
   char *kdb;
   char *spiceker;
   char *mipsker;
   char *sclk;
   char *consts;
   char *bodyids;
   char *leapsec;
{
   msEnvStruct env;   /* structure to hold returned environment values */
   int len, ind;

   /* invoke MSPICE subroutine to return environment variables. */
   ind = mslcl_getgllenv (&env);

   if (ind >= 0)
   {
      strcpy (kdb, env.kdb);
      strcpy (spiceker, env.spiceker);
      len = (int) strlen (spiceker);

      /* remove the '/' from the SPICEKER environment to be consistent with 
         old SPICE environment. */
      if (spiceker[len-1] == '/') spiceker[len-1] = spiceker[len];

      strcpy (mipsker, env.mipsker);
      len = (int) strlen (mipsker);
 
      /* remove the '/' from the MIPSKER environment to be consistent with
         old SPICE environment. */ 
      if (mipsker[len-1] == '/') mipsker[len-1] = mipsker[len];
      strcpy (sclk, env.sclk);
      strcpy (consts, env.consts);
      strcpy (bodyids, env.bodyids);
      strcpy (leapsec, env.leapsec);
   }

   return ind;
}



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create init_spice.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Load the CONSTANTS, SCLK, and LEAPSECONDS kernels.
C The logical names CONSTANTS, SCLK, and LEAPSECONDS must be defined.
C
      SUBROUTINE INIT_SPICE
      IMPLICIT NONE

      CHARACTER*132 KDB, SPICEKER, MIPSKER, SCLK, CONSTS
      CHARACTER*132 BODYIDS, LEAPSEC

      INTEGER*4 STATUS
      LOGICAL FAILED

      KDB = ' '
      SPICEKER = ' '
      MIPSKER = ' '
      SCLK = ' '
      CONSTS = ' '
      BODYIDS = ' '
      LEAPSEC = ' '

      CALL GLL_SPICE_ENV (KDB, SPICEKER, MIPSKER, SCLK, CONSTS,
     &                    BODYIDS, LEAPSEC, STATUS)

      IF (STATUS .LT. 0) 
     &   CALL MABEND('Undefine GLL SPICE environment variables.')

      CALL ERRPRT('SET','NONE')
      CALL ERRACT('SET','RETURN')

      CALL CLPOOL

      CALL LDPOOL(CONSTS)
      IF (FAILED()) CALL XVMESSAGE
     &		('***Error loading CONSTANTS kernel',' ')

      CALL LDPOOL(SCLK)
      IF (FAILED()) CALL XVMESSAGE
     &		('***Error loading SCLK kernel',' ')

      CALL LDPOOL(LEAPSEC)
      IF (FAILED()) CALL XVMESSAGE
     &		('***Error loading LEAPSECONDS kernel',' ')
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create initspice.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* C-bridge for FORTRAN routines init_spice */
#include "xvmaininc.h"
#include "ftnbridge.h"

initspice()
{
  FTN_NAME2_(init_spice, INIT_SPICE) ();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create reset1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C$NAIF routine RESET ( Reset Error Status )
c
C Renamed RESET1 and comment out ACCEPT to avoid naming conflict.
c 
C      Reset the SPICELIB error status to a value of
C      "no error."   As a result, the status routine, FAILED,
C      will return a value of .FALSE.
C 
      SUBROUTINE RESET1
      LOGICAL               SETERR
c      LOGICAL               ACCEPT
 
      LOGICAL               STAT

      STAT = SETERR ( .FALSE. )
C           Wipe out the short and long error messages:
      CALL PUTSMS ( ' ' )
      CALL PUTLMS ( ' ' )
C           Allow long error message to be updated:
c      STAT = ACCEPT ( .TRUE. )
C           Reset the frozen traceback:
      CALL FREEZE
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zreset1.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/*
Bridge for RESET1, called from C
*/
void zreset1()

{
  FTN_NAME(reset1) ();
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create init_spice.imake
/* Imake file for VICAR subroutine init_spice */
#define SUBROUTINE init_spice
#define MODULE_LIST  gll_spice_env.c init_spice.f initspice.c reset1.f zreset1.c
#define FTN_STRING
#define USES_FORTRAN
#define USES_ANSI_C
#define P2_SUBLIB
#define LIB_NETWORK

$ Return
$!#############################################################################
$Test_File:
$ create tinit_spice.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      CHARACTER*132 KDB, SPICEKER, MIPSKER, SCLK, CONSTS
      CHARACTER*132 BODYIDS, LEAPSEC
      CHARACTER*256 MSG
      INTEGER*4 STATUS

      KDB = ' '
      SPICEKER = ' '
      MIPSKER = ' '
      SCLK = ' '
      CONSTS = ' '
      BODYIDS = ' '
      LEAPSEC = ' '
      MSG = '  '

      CALL GLL_SPICE_ENV (KDB, SPICEKER, MIPSKER, SCLK, CONSTS,
     &                    BODYIDS, LEAPSEC, STATUS)

      IF (STATUS .LT. 0) 
     &   CALL MABEND('Undefine GLL SPICE environment variables.')

      CALL XVMESSAGE ('**** Testing FORTRAN Interface ****', ' ')

10    FORMAT (A19,A132) 

      WRITE (MSG,10) 'GLL_KDB ---------->', KDB
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_SPICEKER ----->', SPICEKER
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_MIPSKER ------>', MIPSKER
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_SCLK --------->', SCLK
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_CONSTANTS ---->', CONSTS
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_BODY_IDS ----->', BODYIDS
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_LEAPSECONDS -->', LEAPSEC
      CALL XVMESSAGE (MSG, ' ')
      CALL XVMESSAGE (' ', ' ')
      CALL XVMESSAGE ('Calling INIT_SPICE', ' ')
      CALL INIT_SPICE ()
      CALL XVMESSAGE ('Return from INIT_SPICE', ' ')

      CALL XVMESSAGE (' ', ' ')
      CALL XVMESSAGE (' ', ' ')

      CALL TZINIT_SPICE ()

      RETURN
      END


$!-----------------------------------------------------------------------------
$ create tzinit_spice.c
#include "xvmaininc.h"
#include "ftnbridge.h"

#define TINIT_STR_LEN 132

void FTN_NAME (tzinit_spice) ()
{
   char kdb[TINIT_STR_LEN];
   char spiceker[TINIT_STR_LEN];
   char mipsker[TINIT_STR_LEN];
   char sclk[TINIT_STR_LEN];
   char consts[TINIT_STR_LEN];
   char bodyids[TINIT_STR_LEN];
   char leapsec[TINIT_STR_LEN];

   char msg[256];

   int ind;

   zvmessage ("**** Testing C Interface ****", "");

   ind = zgll_spice_env(kdb, spiceker, mipsker, sclk, consts, bodyids, leapsec);

   if (ind < 0)
      zmabend ("Undefined GLL SPICE environment variables.");

   sprintf (msg, "GLL_KDB ---------->%s", kdb);
   zvmessage (msg, "");

   sprintf (msg, "GLL_SPICEKER ----->%s", spiceker);
   zvmessage (msg, "");

   sprintf (msg, "GLL_MIPSKER ------>%s", mipsker);
   zvmessage (msg, "");

   sprintf (msg, "GLL_SCLK --------->%s", sclk);
   zvmessage (msg, "");

   sprintf (msg, "GLL_CONSTANTS ---->%s", consts);
   zvmessage (msg, "");

   sprintf (msg, "GLL_BODY_IDS ----->%s", bodyids);
   zvmessage (msg, "");

   sprintf (msg, "GLL_LEAPSECONDS -->%s", leapsec);
   zvmessage (msg, "");
   zvmessage ("", "");
   zvmessage ("Calling INITSPICE", "");
   initspice();
   zvmessage ("Return from INITSPICE", "");

   zvmessage ("", "");

   return;
}





$!-----------------------------------------------------------------------------
$ create tinit_spice.imake
#define PROGRAM tinit_spice
 
#define MODULE_LIST tinit_spice.f tzinit_spice.c
 
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_ANSI_C
 
#define FTN_STRING
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE

#define LIB_NETWORK

/*
#define LIB_LOCAL
*/

$!-----------------------------------------------------------------------------
$ create tinit_spice.pdf
process
end-proc

$!-----------------------------------------------------------------------------
$ create tstinit_spice.pdf
! Test script for subroutine INIT_SPICE
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

tinit_spice

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create init_spice.hlp
1 INIT_SPICE
 
VICAR subroutine INIT_SPICE initializes the call to GETSPICE95 by setting the
error flags, clearing the pool, and loading the CONSTANTS, LEAPSECONDS, and
SCLK kernels.

FORTRAN calling sequences:    CALL INIT_SPICE

C calling sequences:   initspice();

Original programmer: Gary Yagi October 28, 1996
Current cognizant programmer:  Gary Yagi
Revisions: New

   Jul. 10, 1998  T.Huang  Created subroutine GLL_SPICE_ENV to invoke a MSPICE
                           subroutine for retrieving GLL-specific SPICE 
                           environment variables.

                           Modified INIT_SPICE to call GLL_SPICE_ENV.


$!-----------------------------------------------------------------------------
$ create gll_spice_env.hlp
1 GLL_SPICE_ENV
 
VICAR subroutine GLL_SPICE_ENV invokes MSPICE subroutine to retrieve 
GLL-specific environment variables from SPICE_CONFIG_FILE.  The environment 
variables include: KDB, SPICEKER, MIPSKER, SCLK, CONSTANTS, BODY_IDS, and 
LEAPSECONDS.  

FORTRAN calling sequences:  (STATUS .LT. 0 when fail)

      CHARACTER*132 KDB, SPICEKER, MIPSKER, SCLK, CONSTS
      CHARACTER*132 BODYIDS, LEAPSEC
      INTEGER*4 STATUS  
      ...
      KDB = ' '
      SPICEKER = ' '
      MIPSKER = ' '
      SCLK = ' '
      CONSTS = ' '
      BODYIDS = ' '
      LEAPSEC = ' '
      ...
      CALL GLL_SPICE_ENV (KDB, SPICEKER, MIPSKER, SCLK, CONSTS,
     &                    BODYIDS, LEAPSEC, STATUS)
      ...

C calling sequences:  (ind < 0 when fail)

      #define TINIT_STR_LEN 132
      ...
      char kdb[TINIT_STR_LEN];
      char spiceker[TINIT_STR_LEN];
      char mipsker[TINIT_STR_LEN];
      char sclk[TINIT_STR_LEN];
      char consts[TINIT_STR_LEN];
      char bodyids[TINIT_STR_LEN];
      char leapsec[TINIT_STR_LEN];
      int ind;
      ...
      ind = zgll_spice_env (kdb, spiceker, mipsker, sclk, consts, 
                            bodyids, leapsec);
      ...

Original programmer: Thomas Huang, July 10, 1998
Revisions: New

$ Return
$!#############################################################################
