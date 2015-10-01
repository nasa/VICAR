$!****************************************************************************
$!
$! Build proc for MIPL module stripper
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:08
$!
$! Execute by entering:		$ @stripper
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
$ write sys$output "*** module stripper ***"
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
$   if F$SEARCH("stripper.imake") .nes. ""
$   then
$      vimake stripper
$      purge stripper.bld
$   else
$      if F$SEARCH("stripper.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stripper
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stripper.bld "STD"
$   else
$      @stripper.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stripper.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stripper.com -
	-p stripper.pdf -
	-i stripper.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create stripper.pdf
Procedure help=*

parm inp	string
parm out	string
parm sl		integer default=1
parm nl		integer default=0
parm striplen	integer	default=3500
parm gapwidth	integer	default=47
local inpv	string
local i		integer	initial=1
local nstrips	integer
local nli       integer
refgbl $echo

Body

let $echo="no"
if (nl = 0) 
  form &inp nl=nli
else
  let nli = nl
end-if
write "nl=&nli"
let nstrips = (nli+striplen-1)/striplen
let inpv = inp

if (nstrips = 1)
  let striplen = nli
end-if

loop
 if (i = nstrips) break
 let inpv = inpv//","//inp
 let i = i + 1
end-loop

let inpv = "("//inpv//")"

STRIPPIT &inpv &out SL=&sl NL=&nl STRIPLEN=&striplen GAPWIDTH=&gapwidth

let $echo="yes"

End-proc

.TITLE
TAE PROCEDURE STRIPPER
.HELP
PURPOSE:
 
   STRIPPER is a TAE procedure which takes an image and puts it into strips
of STRIPLEN lines with a spacing of GAPWIDTH number of samples between the
strips.  It was written to facilitate the formatting of long and skinny
flight lines for playback.
 
EXECUTION:
 
   The following is the execution statement format for STRIPPIT:
 
		STRIPPIT INP OUT PARAMS
 
   where INP, OUT, and PARAMS are parameters discussed in their respective
parameter sections. 
.PAGE
OPERATION:
 
   STRIPPER does the seemingly simple task of cutting and pasting long slender
flight line images into parallel strips in order to format the images for
playback.  It uses a slightly more complex algorithm than would seem necessary
because of the inefficiency of non-sequential I/O (especially XVREADing) which
would slow down the operation considerably.  Instead, a maximum size buffer is
allocated for the output, and the reading and writing is done on a block by
block basis, one block being the maximum number of lines by the number of
strips necessary in the output.

   The two unique parameters (other than SIZE) are STRIPLEN and GAPWIDTH.
STRIPLEN lets the user select the number of lines of the strips, and GAPWIDTH
sets the number of samples spacing between the image strips.  The defaults
are set up for standard TIMS processing.

   This program was written to facilitate the processing of TIMS data, which
has come in lengths of over 22,000 lines in a single flight line.  It can just
as well be used by anyone working with long flight lines of AIS, NS001 or other
airborne sensors, however.
 
 
RESTRICTIONS:
 
   As it is currently designed, the output can be in no wider than 10,000
samples, and must be byte format.

.PAGE 
EXAMPLES:
 
1) STRIPPIT INP OUT
 
2) STRIPPIT INP OUT STRIPLEN=2000 GAPWIDTH=20
 
 
TIMING:
 
WRITTEN BY:  R. E. Walker     16DEC85
 
COGNIZANT PROGRAMMER:  same
 
REVISION: NEW
.LEVEL1
.VARIABLE INP
input dataset name
.VARIABLE OUT
output dataset name
.VARIABLE SL
first line of input to be used
.VARIABLE NL
number of lines of input to be used
.VARIABLE STRIPLEN
number of lines in output image
.VARIABLE GAPWIDTH
number of pixels separating each strip
.LEVEL2
.VARIABLE INP
The name of the input dataset.
.VARIABLE OUT
The name of the output dataset.
.VARIABLE SL
The first line of input image to be output.
.VARIABLE NL
The number of lines of input image to be output.
.VARIABLE STRIPLEN
The number of lines in the output image.  That is, the number of lines in
each strip.  The default is 3500 pixels.
.VARIABLE GAPWIDTH
The number of pixels separating each strip.  The default is 47 pixels.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create stripper.imake
#define  PROCEDURE stripper

#define R2LIB 
$ Return
$!#############################################################################
