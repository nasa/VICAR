$!****************************************************************************
$!
$! Build proc for MIPL module tempname
$! VPACK Version 1.8, Friday, August 08, 1997, 15:55:41
$!
$! Execute by entering:		$ @tempname
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module tempname ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tempname.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tempname.imake") .nes. ""
$   then
$      vimake tempname
$      purge tempname.bld
$   else
$      if F$SEARCH("tempname.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tempname
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tempname.bld "STD"
$   else
$      @tempname.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tempname.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tempname.com -
	-i tempname.imake -
	-p tempname.pdf -
	-t tsttempname.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tempname.imake
#define PROCEDURE tempname
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create tempname.pdf
PROCEDURE TEMPNAME HELP=*

SUBCMD DEL
PARM DIR	STRING	COUNT=0:1	DEFAULT=--
REFGBL	$SYSCHAR
END-SUBCMD

SUBCMD-DEFAULT CREATE
PARM INN STRING
PARM OUT NAME
END-SUBCMD

PROCEDURE NAME=DEL
LOCAL PID STRING
BODY
LET PID = $SESSION
IF ($SYSCHAR(1) = "UNIX")
  IF ($COUNT(DIR) = 0)
    ush /bin/rm *.ZZZ&PID
  ELSE
    ush /bin/rm &DIR/*.ZZZ&PID
  END-IF
ELSE
  IF ($COUNT(DIR) = 0)  
    DCL DELETE *.ZZZ&PID;
  ELSE
    DCL DELETE &DIR*.ZZZ&PID;
  END-IF
END-IF
END-PROC

PROCEDURE NAME=CREATE
LOCAL FULL STRING
BODY
LET OUT="&INN"//".ZZZ"//$SESSION
END-PROC

BODY
IF (_SUBCMD = "CREATE")
  CREATE
ELSE
  DEL
END-IF
END-PROC

!# annot function="VICAR Utilities"
!# annot keywords=(add,ZZZ,ID,delete,OUT)

.TITLE
Appends ZZZ extension to filename (making it a temporary file)
.HELP
PURPOSE:

Tempname is used to add a process-specific extension onto a filename.
The extension starts with "ZZZ" followed by the process ID.  Tempname
can also be used to delete temporary files with matching extensions
from a directory.

EXECUTION:

Tempname has two subcommands: create and del.  The default is create
so you can just type tempname instead of tempname-create to access it.
The value of the OUT parameter must be declared as a local variable
first.  The form of this subcommand is:

	tempname INN OUT 

The form of the del subcommand is as follows:

	tempname-del [DIR]

The DIR parameter is the directory from which you want to delete the
temporary files.  It is optional and defaults to your current working
directory.  It deletes files with extensions starting with "ZZZ"
followed by the current process ID.  In UNIX leave off the trailing
'/' after the directory name because tempname puts that in.

REVISIONS:

	8-97 ...RRD... Wrote original version.

.LEVEL1
!
.SUBCMD CREATE
Create temporary filename. 
.VARIABLE INN -CREATE
STRING
Filename to add
extension to.
.VARIABLE OUT -CREATE
STRING
Completed 
filename.extension
!
.SUBCMD DEL
Delete temporary files.
.VARIABLE DIR -DEL
STRING -OPTIONAL
Directory of
temporary files.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tsttempname.pdf
procedure help=*
refgbl $echo
local trans	string
refgbl $syschar

body
let _onfail="continue"
let $echo=("yes","no","no")
tempname tsttempname trans
disp trans
if ($syschar(1) = "UNIX")
  ush /bin/ls >&trans
else
  dire/out=&trans
end-if
typetext &trans
tempname-del
end-proc

.help 
This is the test procedure for tempname.  It creates a temporary file
and then does a directory listing to make sure the file is there.
Then it deletes the temporary file using tempname-del.  The user
should verify that the temporary file was deleted.
.end
$ Return
$!#############################################################################
