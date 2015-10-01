$!****************************************************************************
$!
$! Build proc for MIPL module msserver
$! VPACK Version 1.9, Friday, December 17, 1999, 14:10:35
$!
$! Execute by entering:		$ @msserver
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
$ write sys$output "*** module msserver ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to msserver.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("msserver.imake") .nes. ""
$   then
$      vimake msserver
$      purge msserver.bld
$   else
$      if F$SEARCH("msserver.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake msserver
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @msserver.bld "STD"
$   else
$      @msserver.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create msserver.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack msserver.com -
	-s msserver.c -
	-i msserver.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create msserver.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include "ms_defines.h"
#include "cltsub.h"
#include "lclsub.h"
#include "ms_bridge.h"


int main (int argc, char **argv)
{
   int  ld,
        sd;

   pid_t  childpid;

   const char *cm = "SpiceServer";

   msclt_log (cm, "**** Starting SpiceServer v.01-27-1999 *****");
   if (mssvr_initAcceptor (&ld)) 
   {
      msclt_log (cm, "init FAILED !!!!");
      msclt_log (cm, "Server Terminating !!!!");
   }
   else msclt_log (cm, "init SUCCESS !!!!");

   zms_erract ("set", "return");


   /* Ignore the SIGCHLD signal, which is sent when a child process dies.  
    * When SIGCHLD's disposition is set to ignore,
    * children never create zombies.
    */
   signal(SIGCHLD, SIG_IGN);


   for (;;) 
   {
      if ((sd = mssvr_getNewClient(ld)) == (-1)) 
      {
         msclt_log (cm, "Cannot get new client");
         msclt_log (cm, "Server Terminating !!!!");
         exit (1);
      }

      switch (childpid = fork())
      {
         case -1 : 
            msclt_log (cm, "fork() FAILED");
            msclt_log (cm, "Server Terminating !!!!");
            exit (0);
            break;

         case 0 : 
            close (ld);  /* close listener descriptor */
            mssvr_handleRequest (sd);
            close (sd);  /* close socket descriptor */
            exit (0);
            break;

         default :
            close (sd); 
      }
   }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create msserver.imake
#define  PROGRAM   msserver
#define  MAIN_LANG_C
#define  USES_ANSI_C

#define MODULE_LIST msserver.c

#define FTN_STRING
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_MATH77
#define LIB_NETWORK

/*************
#define LIB_LOCAL
*************/
$ Return
$!#############################################################################
