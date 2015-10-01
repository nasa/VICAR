$!****************************************************************************
$!
$! Build proc for MIPL module addtofil
$! VPACK Version 1.9, Thursday, May 31, 2007, 14:01:05
$!
$! Execute by entering:		$ @addtofil
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
$ write sys$output "*** module addtofil ***"
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
$ write sys$output "Invalid argument given to addtofil.com file -- ", primary
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
$   if F$SEARCH("addtofil.imake") .nes. ""
$   then
$      vimake addtofil
$      purge addtofil.bld
$   else
$      if F$SEARCH("addtofil.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake addtofil
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @addtofil.bld "STD"
$   else
$      @addtofil.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create addtofil.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack addtofil.com -mixed -
	-s addtofil.c -
	-i addtofil.imake -
	-p addtofil.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create addtofil.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>
#include "vicmain_c"

/*  Revision History                                             */
/*    11-96  SXP   Converted from DCL to C for portability.	 */
/*     1-97  SXP   Programmer note:  This version crashes mysteriously
                   on our current version of VMS.  I noticed in RESET.COM,
                   SVH could not get VMS to behave.  Maybe this is a
                   similar problem.  I left this module using DCL for VMS.  */
void main44(void)
{
#define MAXR1    256            /* buffer length for file name.     */
#define MAXR2    2*MAXR1        /* buffer length for full file name */   

    FILE *unit;	                /* C I/O control block	            */
    int stat;	                /* status return	            */
    int count;			/* COUNT from zvp	       	    */
    char str[MAXR1];            /* buffer for string.               */
    char file_name[MAXR1];	/* name of file to be appended to.  */
    char full_name[MAXR2];	/* full file path name		    */
/*=========================================================================*/

    zvp("STRING1",str,&count);             /* get string to be appended */ 

    zvp("INPUT",file_name,&count);	   /* get name of input		*/
    zvfilename(file_name,full_name,MAXR2);

    unit = fopen(full_name,"a");
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }

    stat = fprintf (unit, "%s\n", str); 
    if (stat <= 0)
    {
 	zvmessage(" Write (update) error on input","");
	zabend();
    }
    fclose(unit);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create addtofil.imake
#define PROGRAM addtofil

#define MODULE_LIST addtofil.c

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*  This IMAKE file is patterned after that for module RATIO.
    THIS module is supposed to generate 2 PDF and an executable.  Maybe this
    could be simplified one day, but currently VMS dies in fclose if
    the C code is run.  */
$ Return
$!#############################################################################
$PDF_File:
$ create addtofil.pdf
PROCESS HELP=*
  PARM INPUT    TYPE=STRING
  PARM STRING1  TYPE=STRING
!# annot function="VICAR Utilities"
!# annot keywords=(append, "ASCII file", CREATEFILE)
END-PROC
.Title
Append a string to an ASCII file
.help
PURPOSE:

ADDTOFILE appends a string to a specified file.

EXECUTION:
 This utility can be called from a VICAR procedure using the syntax:

 ADDTOFILE filename string

 where filename is an existing ASCII file produced by CREATEFILE or any other
 means.
 
REVISION HISTORY:
   06-1-89  HBM   Initial release.
   11-5-96  SP    Converted from DCL to C for portability.
   01-3-97  SP    Since the C version crashed on VMS, renamed the C version
                  addtofil, and wrote a procedure to call DCL on VMS and to
                  call addtofil on UNIX.
.level1
.vari input
Input file name
.vari STRING1
string to append.
.level2
.vari input
INPUT may be specified with a directory and may use the ~username notation.
Alternately it may be specified without a directory (pathname), in which 
case the current working directory is used.  
.vari STRING1
STRING1 is appended on a new line to INPUT.
.end
$ Return
$!#############################################################################
