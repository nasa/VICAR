$!****************************************************************************
$!
$! Build proc for MIPL module qb
$! VPACK Version 1.8, Friday, July 25, 1997, 14:15:43
$!
$! Execute by entering:		$ @qb
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
$ write sys$output "*** module qb ***"
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
$ write sys$output "Invalid argument given to qb.com file -- ", primary
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
$   if F$SEARCH("qb.imake") .nes. ""
$   then
$      vimake qb
$      purge qb.bld
$   else
$      if F$SEARCH("qb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake qb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @qb.bld "STD"
$   else
$      @qb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create qb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack qb.com -
	-i qb.imake -
	-p qb.pdf -
	-t tstqb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create qb.imake
#define PROCEDURE qb
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create qb.pdf
Procedure HELP=*
REFGBL $VIDSFLG
parm range integer count=2 default=(0,4095)
parm input string 
parm scale keyword valid=(NORMAL,NONE,FULL,NOSCALE) COUNT=0:1 default=--
body
local (s,n,f,Return) string
local (i,t,x,r1,r2) integer
IF ($VIDSFLG <> "READY")
	WRITE "---VIDS MUST BE RUNNING FOR THIS PROC TO WORK"
	GOTO ERR
END-IF
let r1=range(1)
let r2=range(2)
let s="FIT"
let Return=""
if ($count(scale) = 1) let s ="NONE"
flag-add nomessage
WRITE " "
WRITE "---SET YOUR STRETCH BEFORE ENTERING THIS PROC (EG, JSTRETCH-LINEAR 0 40)"
WRITE "---FOR HALFWORD DATA, DON'T FORGET RANGE PARAMETER (EG, QB (0,7000) )"
WRITE " "
WRITE "TO QUIT, ENTER QUIT"
WRITE " "
!RESET &INPUT
jset-range &r1 &r2
loop
	nxt &input n i t x
	if ( n = "END_OF_FILE") BREAK
	jdisp &n scale=&s
        write "[7mFOR NEXT IMAGE[0m"
	getpar Return 
	if ( Return = "quit" ) Break
end-loop
ERR>
end-proc
.TITLE
    QB	(QUICK BROWSE) VICAR/VIDS PROCEDURE TO BROWSE FILES FROM A LIST
.HELP
Qb will sequentially display (using VIDS) files listed in an input text file.
Qb pauses after each display until the user enters a CR.  If QUIT is entered,
the proc will terminate.  It works for byte or halfword images.  It will 
zoom the image down to fit the screen or show as much as the screen can hold.

The input list is in SRCH format.  That is, the first record says :
NEXT FILE=0001.   Each subsequent record is a file spec.  However, SRCH 
need not be run to create the file.  A DCL DIRE/NOHEAD/NOTRAIL  file_spec
will make the list of file specs, but the first record must be added by 
editor.

Qb should be given its RANGE parameter if the data is halfword.  It will 
default to RANGE=(0,4095).

Example:

	qb input=A.LIS 
		images from text file A.LIS will be displayed zoomed to fit
                screen if necessary.

	qb input=A.LIS 'FULL
		no zooming done

	qb range=(0,4000) input=A.LIS 
		halfword images from file A.LIS will be compressed from
		0 to 4000 into 0 to 255 and zoomed if necessary.

REVISIONS:

	7-97  ...RRD... Ported to UNIX.
.level1
.vari range
halfword compression limits
.vari input
text file of file names
.vari scale
fit on screen or not
.level2
.vari range
integer
For halfword data, this is the range of dn values that will displayed as
0 to 255.  default= (0,4095).   No effect for byte data.
.vari input
String
Name of text file in SRCH format which contains the file specs of the images
to be displayed.
.vari scale
keyword
Any valid entry for scale will display images at full resolution.  
Default is to zoom to fit the entire image on the display screen.
VALID: NONE, FULL, NORMAL, NOSCALE.
$ Return
$!#############################################################################
$Test_File:
$ create tstqb.pdf
procedure help=*
refgbl $echo

body

let _onfail="continue"
let $echo=("yes","no","no")	! only echo top level
createfile a.lis
addtofile a.lis "NEXT FILE =     1"
addtofile a.lis "a"
addtofile a.lis "b"
typetext a.lis
gen a 100 100 linc=0 sinc=0 ival=100
gen b 100 100 format=half linc=0 sinc=0 ival=3000

qb range=(0,4000) input=a.lis

end-proc

.help

	This is the test procedure for qb.pdf.  You must
	be running VIDS with an active display window and
	you must hit ENTER when prompted to view the second
	image and ENTER again to quit.
.end
$ Return
$!#############################################################################
