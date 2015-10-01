$!****************************************************************************
$!
$! Build proc for MIPL module file_no_path
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:15
$!
$! Execute by entering:		$ @file_no_path
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
$ write sys$output "*** module file_no_path ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to file_no_path.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("file_no_path.imake") .nes. ""
$   then
$      vimake file_no_path
$      purge file_no_path.bld
$   else
$      if F$SEARCH("file_no_path.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake file_no_path
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @file_no_path.bld "STD"
$   else
$      @file_no_path.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create file_no_path.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack file_no_path.com -mixed -
	-s file_no_path.c -
	-i file_no_path.imake -
	-t tstfile_no_path.c tstfile_no_path.imake tstfile_no_path.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create file_no_path.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*

  Written by Thomas Roatsch, DLR             2-Sep-1993
  Prototypes added by Vadim Parizher, JPL   27-Jun-1997

*/

/* Function file_no_path returns the filename without path */

#include "xvmaininc.h"
#include <stdio.h>
#include <string.h>

void file_no_path(char *filename)
{
   char *value;
   
#ifdef __VMS
   value = strrchr(filename,':');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
   value = strrchr(filename,']');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
#else
   value = strrchr(filename,'/');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
#endif   
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create file_no_path.imake
#define SUBROUTINE file_no_path
#define MODULE_LIST file_no_path.c 

#define P1_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tstfile_no_path.c
#include "vicmain_c"

/* Testprogramm for Function FILE_NO_PATH.C   */

void main44()

{

char filename[120];
int count;

zvp("filename", filename, &count);

zvmessage("","");
zvmessage("filename from TAE","");
zvmessage(filename,"");

file_no_path(filename);

zvmessage("","");
zvmessage("filename without path","");
zvmessage(filename,"");

zvmessage("","");
zvmessage("TSTFILE_NO_PATH succesfully completed", "");

} 
$!-----------------------------------------------------------------------------
$ create tstfile_no_path.imake
#define PROGRAM tstfile_no_path
#define MODULE_LIST tstfile_no_path.c

#define TEST

#define MAIN_LANG_C
#define USES_ANSI_C

#define LIB_P1SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN

/* #define LIB_LOCAL */

$!-----------------------------------------------------------------------------
$ create tstfile_no_path.pdf
Process help=*
 PARM      FILENAME TYPE=(STRING,80) COUNT=1         DEFAULT=HWSPICE_BSP
END-PROC
.Title
 Test Programm for FILE_NO_PATH
.HELP

WRITTEN BY: Thomas Roatsch, DLR     7-Mar-1994

.LEVEL1
.VARI FILENAME
Filename 
.End
 
$ Return
$!#############################################################################
