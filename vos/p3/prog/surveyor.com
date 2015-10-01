$!****************************************************************************
$!
$! Build proc for MIPL module surveyor
$! VPACK Version 1.7, Thursday, July 21, 1994, 16:57:14
$!
$! Execute by entering:		$ @surveyor
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
$ write sys$output "*** module surveyor ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to surveyor.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("surveyor.imake") .nes. ""
$   then
$      vimake surveyor
$      purge surveyor.bld
$   else
$      if F$SEARCH("surveyor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake surveyor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @surveyor.bld "STD"
$   else
$      @surveyor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create surveyor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack surveyor.com -
	-p surveyor.pdf -
	-i surveyor.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create surveyor.pdf
PROCESS HELP=*
END-PROC
.TITLE
 SURVEYOR
.HELP
     Surveyor is an interactive, pull down menu based
program used to create terrain fly by animations. As 
input it takes two registered images (an image of the 
region from the top and an elevation image). The user 
then creates a flight path around the data interactively. 
Once the path is defined Surveyor can create the frames 
for the animation which can then be put on video tape, 
film etc. Surveyor is run outside of VICAR on Sun 
computers.

     Make sure SURVHOME/bin/sun4 is in your path before
running Surveyor. By doing a 'select d' the SURVHOME
environment variable will be automatically defined.

.PAGE
     To run Surveyor do the following at the command prompt:
> survd
> surveyor
     This will run Surveyor. If for some reason you exit from
Surveyor abnormally, make sure you kill the survd process.

     To find the process id of the survd process do:
> ps -auxww | grep survd

     And you will get something like:

> YourUserName  747  0.0  0.3 2272  152 ?  I    11:37   0:00 survd

     Then use the kill command to kill the stray survd process:

> kill -9 747

     For more information please consult the user guide.
Your system administrator should have a copy.
     
.END
$ Return
$!#############################################################################
$Imake_File:
$ create surveyor.imake
#define PROCEDURE surveyor
#define R3LIB	
$ Return
$!#############################################################################
