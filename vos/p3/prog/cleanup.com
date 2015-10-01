$!****************************************************************************
$!
$! Build proc for MIPL module cleanup
$! VPACK Version 1.8, Friday, July 06, 2001, 18:14:29
$!
$! Execute by entering:		$ @cleanup
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
$ write sys$output "*** module cleanup ***"
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
$ write sys$output "Invalid argument given to cleanup.com file -- ", primary
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
$   if F$SEARCH("cleanup.imake") .nes. ""
$   then
$      vimake cleanup
$      purge cleanup.bld
$   else
$      if F$SEARCH("cleanup.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cleanup
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cleanup.bld "STD"
$   else
$      @cleanup.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cleanup.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cleanup.com -
	-p cleanup.pdf -
	-i cleanup.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create cleanup.pdf
procedure help=*

parm inp (string,60) count=1:10 
parm out (string,60) count=1:10 
parm wts type=integer default=25
parm direct type=keyword default=horiz valid=(horiz,vert) 
local icnt integer   initial=1
local xx (string,60) initial=" "
local yy (string,60) initial=" "
local zz type=string  initial="'horiz"
local fmt type=string
local nl type=integer
local ns type=integer
local nb type=integer

body

if (direct="vert") let zz="'vert "
loop
   if (icnt>$count(inp)) break
   let xx=inp(&icnt)
   let yy=out(&icnt)
   write "   "
   write "CLEANING FILE &icnt"
   form &xx fmt nl ns nb
   if (fmt="BYTE")
      let fmt="128"
   else
      let fmt="0"
   end-if
   lave &xx CLEANUPSCR &zz 'high 'image 'noprint filter=&wts
   F2 (&xx,CLEANUPSCR) &yy func="IN1-IN2+&fmt"
   let icnt=icnt+1
end-loop
ush \rm CLEANUPSCR
end-proc

.title VICAR2 Procedure CLEANUP
.HELP
CLEANUP is a VICAR procedure designed to clean line noise from
scanner data. The procedure will clean up to ten single-band
images in a single run. 

CLEANUP works by adjusting each line (in the VERT mode) or each column (in
the HORIZ mode) by adding a constant to that line/sample, so that its
average matches that of the surrounding WTS lines/samples.

CLEANUP can be run in either a horizontal (cleaning rows) or vertical (cleaning
columns) mode.  It may be run on BYTE, HALF, FULLWORD, or REAL data formats,
but it will only run on files containing a single channel.

.LEVEL1
.variable inp
names of the 
images to be cleaned.
.variable out
names for the cleaned
versions of input images.
.variable wts
Number of weights for
the high-pass filter
(must be an odd integer)
.variable direct
Direction of averaging.
Valid: horiz, vert
.level2
.variable inp
The user may input up to ten files. BYTE, HALF, FULL, and REAL pixel formats
are accepted, but each file must contain only a single channel.
.variable out
The name(s) of the output (cleaned) files.  There should be one output file
for each input file.1
.variable wts
The user may specify the number of
weights for the high-pass filter that
is run over the averaged image (see the
general help). The spatial frequency passed
by the filter is inversely proportional to 
the length of the filter. 
.variable direct
This variable is the direction of averaging. Use HORIZ to remove horizontal
striping, and use VERT to remove vertical striping.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create cleanup.imake
#define  PROCEDURE cleanup

#define R3LIB 
$ Return
$!#############################################################################
