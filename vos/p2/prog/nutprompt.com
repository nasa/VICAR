$!****************************************************************************
$!
$! Build proc for MIPL module nutprompt
$! VPACK Version 1.9, Monday, December 07, 2009, 16:48:23
$!
$! Execute by entering:		$ @nutprompt
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
$ write sys$output "*** module nutprompt ***"
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
$ write sys$output "Invalid argument given to nutprompt.com file -- ", primary
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
$   if F$SEARCH("nutprompt.imake") .nes. ""
$   then
$      vimake nutprompt
$      purge nutprompt.bld
$   else
$      if F$SEARCH("nutprompt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake nutprompt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @nutprompt.bld "STD"
$   else
$      @nutprompt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create nutprompt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack nutprompt.com -mixed -
	-s nutprompt.c -
	-i nutprompt.imake -
	-p nutprompt.pdf -
	-t tstnutprompt.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create nutprompt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include        "vicmain_c"
#include	"taeconf.inp"
#define prompt "NUT>"

    void main44 (void)
    {
    COUNT	line_num;		/* IN: input line number	     */
    COUNT	col_num;		/* IN: input column number	     */
    TEXT	loc_prompt[STRINGSIZ+1];
    int         count, lfcr;
    char         msg[5];

     zvp ("line",&line_num,&count);
     zvp ("col",&col_num,&count);
     zvp ("lfcr",&lfcr,&count);
     s_copy (prompt, loc_prompt);
     t_highlight (loc_prompt);
     t_output( line_num, col_num, loc_prompt);	/* write prompt              */
     if (lfcr) {
       line_num++;
       t_output( line_num, 1, "");
     }
     return;
    }
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create nutprompt.imake
#define PROGRAM nutprompt
#define MODULE_LIST nutprompt.c  

#define MAIN_LANG_C
#define R2LIB

#define USES_C

#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$PDF_File:
$ create nutprompt.pdf
process help=* 
PARM LINE INTEGER
PARM COL INTEGER
PARM LFCR INTEGER
end-proc

$ Return
$!#############################################################################
$Test_File:
$ create tstnutprompt.pdf
procedure
refgbl $syschar
body
!  
write " " 
write  "This is a dummy test pdf; it is used with NUT."
if ($syschar(1) = "VAX_VMS")
write  "To run NUT type NUT at the VICAR prompt."
else
write  "To run NUT type NUT at the VICAR prompt.  Use lower case."
end-if
end-proc
$ Return
$!#############################################################################
