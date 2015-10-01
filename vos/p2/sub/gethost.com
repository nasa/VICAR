$!****************************************************************************
$!
$! Build proc for MIPL module gethost
$! VPACK Version 1.9, Monday, December 07, 2009, 16:20:05
$!
$! Execute by entering:		$ @gethost
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
$ write sys$output "*** module gethost ***"
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
$ write sys$output "Invalid argument given to gethost.com file -- ", primary
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
$   if F$SEARCH("gethost.imake") .nes. ""
$   then
$      vimake gethost
$      purge gethost.bld
$   else
$      if F$SEARCH("gethost.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gethost
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gethost.bld "STD"
$   else
$      @gethost.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gethost.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gethost.com -mixed -
	-s gethost.c -
	-i gethost.imake -
	-t tgethost.c tgethostf.f tgethost.imake tgethost.pdf tstgethost.pdf -
	-o gethost.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gethost.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************/
/*                                                                            */
/*  FORTRAN:                                                                  */
/*  integer status, gethost                                                   */
/*  char*20 hostname                                                          */
/*  status = gethost(hostname)                                                */
/*                                                                            */
/*  C:                                                                        */
/*  char hostname[21];                                                        */
/*  int  status, length=20, zgethost();                                       */
/*  status = zgethost(hostname, length);                                      */
/*                                                                            */
/*  This function will return the name of the current host in the 'hostname'  */
/*  parameter.  For C calls, you must include the length of the string, for   */
/*  Fortran, this is unnecessary.  The returned name will always be lower-    */
/*  case.  If the function fails for any reason, it will return 0; otherwise, */
/*  it returns 1.  If the function fails, the hostname string will be set to  */
/*  null.  If the string is too small to contain the entire host name, this   */
/*  function will return as many characters as will fit in the string you     */
/*                                                                            */
/*  Cognizant Programmer:  Paul Bartholomew                                   */
/*                                                                            */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  05-25-93  81831  PDB - Added LIB_P2SUB to tgethost.imake (no code change).*/
/*  02-22-93   N/A   PDB - Initial release.                                   */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#if VMS_OS
#include syidef
#include ssdef
#endif

#ifndef SUCCESS
#define SUCCESS		1
#define FAILURE		0
#endif

FTN_NAME2(gethost, GETHOST) (char *hostname, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char *c_string;
   int  length, status;

   zsfor2len(length, hostname, &hostname, 1, 1, 1, hostname);
   c_string = (char *) malloc(sizeof(char) * (length+1));
   status = zgethost(c_string, length);
   zsc2for(c_string, length, hostname, &hostname, 1, 1, 1, hostname);

   free(c_string);
   return (status);
}

int zgethost(hostname, namelength)
char *hostname;
int  namelength;
{
   int  status, i;

#if VMS_OS
   char temp[16];
   long retlen;
   struct itmlst {
      short length;
      short code;
      long *bufadr;
      long *retadr;
   } itemlist[] = {15, SYI$_NODENAME, temp, &retlen, 0, 0, 0, 0};

   status = sys$getsyiw(0, 0, 0, itemlist, 0, 0, 0);
   if (status != SS$_NORMAL) {
      hostname[0] = '\0';
      status = FAILURE;
   }
   else {
      status = SUCCESS;
      if (strlen(itemlist[0].bufadr) > namelength) {
         strncpy(hostname, itemlist[0].bufadr, namelength);
         hostname[namelength] = '\0';
      }
      else
         strcpy(hostname, itemlist[0].bufadr);
      for (i = 0; i < strlen(hostname); i++)
         hostname[i] = (char) tolower((char) hostname[i]);
   }
#else
   status = gethostname(hostname, namelength);
   if (status != 0) {
      hostname[0] = '\0';
      status = FAILURE;
   }
   else {
      hostname[namelength] = '\0';
      status = SUCCESS;
      for (i = 0; i < strlen(hostname); i++)
         hostname[i] = (char) tolower((char) hostname[i]);
   }
#endif

   return status;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gethost.imake
#define SUBROUTINE gethost

#define MODULE_LIST gethost.c

#define FTN_STRING

#define USES_ANSI_C
#define USES_FORTRAN

#define P2_SUBLIB
$ Return
$!#############################################################################
$Test_File:
$ create tgethost.c
#include "vicmain_c"
#include "ftnbridge.h"

/*  Program to test the GETHOST subroutine.  */

main44()
{
   char hostname[41], msg[81];
   int  length, status;

   length = 40;
   zvmessage("Testing GETHOST subroutine C interface", "");

   status = zgethost(hostname, length);
   if (status) {
      sprintf(msg, "   Full Host Name = %s", hostname);
      zvmessage(msg, "");
   }
   else
      zvmessage("   GETHOST cannot determine host name.", "");

   length = 4;
   status = zgethost(hostname, length);
   if (status) {
      sprintf(msg, "   Truncated Host Name = %s", hostname);
      zvmessage(msg, "");
   }
   else
      zvmessage("   GETHOST cannot determine host name.", "");

   FTN_NAME(tgethostf)();
   exit(0);
}
$!-----------------------------------------------------------------------------
$ create tgethostf.f
C FORTRAN ROUTINE TO TEST FORTRAN BRIDGE TO SUBROUTINE GETHOST

       SUBROUTINE tgethostf()
       character*40 hostname
       character*4  trunchost
       integer status, gethost

       call xvmessage('Testing GETHOST FORTRAN interface', ' ')
       status = gethost(hostname)
       if (status .ne. 1) then
          call xvmessage('   GETHOST cannot determine host name.', ' ')
       else
          call xvmessage('   Full Host Name = '//hostname, ' ')
       endif
       status = gethost(trunchost)
       if (status .ne. 1) then
          call xvmessage('   GETHOST cannot determine host name.', ' ')
       else
          call xvmessage('   Truncated Host Name = '//trunchost, ' ')
       endif

       return
       end
$!-----------------------------------------------------------------------------
$ create tgethost.imake
#define PROGRAM tgethost

#define MODULE_LIST tgethost.c tgethostf.f

#define MAIN_LANG_C
#define TEST

#define USES_C
#define USES_FORTRAN

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$!-----------------------------------------------------------------------------
$ create tgethost.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstgethost.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tgethost
let $echo="no"
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create gethost.hlp
1 GETHOST

  GETHOST will return the name of the current host machine in the 'hostname'
  parameter.  For C calls, you must include the length of the string in the
  parameter list; for Fortran calls, this is unecessary.  The return name of
  the host machine will always be lower-case.  If the function fails for any
  reason, the hostname parameter will be set to null and the function will
  return 0.  Otherwise, it returns 1 to indicate success.  If the string is
  too small to contain the entire host name, this function will return as
  many characters as will fit in the string you provide.

  FORTRAN:
  integer status, gethost
  char *20 hostname
  status = gethost(hostname)

  C:
  char hostname[21];
  int  status, length, zgethost();
  status = zgethost(hostname, length);

2 History

  Original Programmer:  Paul Bartholomew  02-22-93
  Current Cognizant Programmer:  Paul Bartholomew  02-22-93
  Source Language:  C

  Revision History:
    Date    FR #   Description
  --------  -----  -------------------------------------------------------
  05/25/93  81831  PDB - Added LIB_P2SUB to tgethost.imake (no code change).
  02-22-93   N/A   PDB - Initial release.
$ Return
$!#############################################################################
