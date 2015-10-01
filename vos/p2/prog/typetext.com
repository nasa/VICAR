$!****************************************************************************
$!
$! Build proc for MIPL module typetext
$! VPACK Version 1.8, Thursday, April 10, 1997, 09:48:24
$!
$! Execute by entering:		$ @typetext
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
$ write sys$output "*** module typetext ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to typetext.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("typetext.imake") .nes. ""
$   then
$      vimake typetext
$      purge typetext.bld
$   else
$      if F$SEARCH("typetext.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake typetext
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @typetext.bld "STD"
$   else
$      @typetext.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create typetext.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack typetext.com -
	-s typetext.c -
	-i typetext.imake -
	-p typetext.pdf -
	-t tsttypetext.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create typetext.c
$ DECK/DOLLARS="$ VOKAGLEVE"

/* Revision History */
/* 3-97 SXP Initial release. Simple, but still for the glory of God.*/
/*          Corrected by Thomas Huang.  Added call to zvfilename with
            enlarged full_name per Bob Deen.*/

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    250            /* buffer length for file name.     */
#define MAXR2    2*MAXR1        /* buffer length for full file name */   

/*    also used for zvmessage max message length.  */
   int   count;                  /* used by zvp to get parameters    */
   int   len, status;
   FILE  *textfile;              /* file pointer to text file        */
   char  buffer[MAXR1];          /* buffer for text.                 */
   char  file_name[MAXR1];       /* name of file to be TYPEd.        */
   char full_name[MAXR2+1];	 /* full file path name		    */
/*  ==================================================================  */

/* Retrieve the user specified filename.*/

   zvp("FILE", file_name, &count);
   zvfilename(file_name,full_name,MAXR2);

/* Open the text file for read-only. */

   textfile = fopen(full_name, "r");
   if (textfile == NULL) {
       sprintf(buffer, "ERROR OPENING text FILE '%s'", file_name);
       zmabend(buffer);
   }
   while (fgets(buffer, sizeof(buffer), textfile) != NULL) {
      len = strlen(buffer);
      if ( len>0 && buffer[len-1] == '\n')
         buffer[len-1]  =  '\0';
      zvmessage(buffer,"");
   }
   status= fclose(textfile);
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create typetext.imake
#define PROGRAM typetext

#define MODULE_LIST typetext.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create typetext.pdf
PROCESS HELP=*
  PARM FILE     TYPE=STRING
END-PROC
.help
PURPOSE:

TYPETEXT types the specified text (ASCII) file.  It includes the output
in the VICAR session log.  It is like DCL TYPE or UNIX cat, but it includes
the output in session log.

EXECUTION:
 This utility can be called from a VICAR procedure using the syntax:

 typetext filename 

 where filename is an existing ASCII file.
 
REVISION HISTORY:
   03-18-97 SP    Initial release.
.level1
.vari FILE
Text file name
.level2
.vari FILE
INPUT may be specified with a directory and may use the ~username notation.
Alternately it may be specified without a directory (pathname), in which 
case the current working directory is used.  (See under zvfilename in
VICAR Porting Guide.)
.end
$ Return
$!#############################################################################
$Test_File:
$ create tsttypetext.pdf
procedure
!
!
refgbl $echo
body
let _onfail="continue"
let $echo=("yes","no","no") ! echo only top level.
!
createfile add1.dat
typetext   add1.dat
addtofile  add1.dat "NEXT FILE = 00001"
typetext   add1.dat
addtofile  add1.dat "/home/xyz/VENUS.IMG"
typetext add1.dat
!
end-proc
$ Return
$!#############################################################################
