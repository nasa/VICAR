$!****************************************************************************
$!
$! Build proc for MIPL module read_info1
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:56
$!
$! Execute by entering:		$ @read_info1
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
$ write sys$output "*** module read_info1 ***"
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
$   if F$SEARCH("read_info1.imake") .nes. ""
$   then
$      vimake read_info1
$      purge read_info1.bld
$   else
$      if F$SEARCH("read_info1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake read_info1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @read_info1.bld "STD"
$   else
$      @read_info1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create read_info1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack read_info1.com -
	-s read_info1.c -
	-i read_info1.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create read_info1.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>


int read_info1 (label)
   char  *label;
{
   int length, day, month, year, count, date;

   if ((length = strlen(label)) >= 20) {
      if (label[5] == 'D')
         count = sscanf(&label[14],"%d/%d/%d", &month, &day, &year);
      else
         count = sscanf(&label[17],"%d/%d/%d", &month, &day, &year);

      if (count == 3)
         date = 10000 * year + 100 * month + day;
      else {
         l_message (" read_info1 error:");
         l_message ("  Unable to read date from the label string.");
         date = 0;
       }
   }
   else {
      l_message (" read_info1 error:");
      l_message ("  Label string too short to have date in INFO1 format");
      l_message ("  date (mm/dd/yy) must start on or past char 15 or 18");
      date = 0;
   }

   return date;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create read_info1.imake

#define SUBROUTINE read_info1

#define MODULE_LIST read_info1.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
