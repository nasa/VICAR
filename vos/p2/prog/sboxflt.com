$!****************************************************************************
$!
$! Build proc for MIPL module sboxflt
$! VPACK Version 1.8, Monday, January 22, 1996, 16:28:12
$!
$! Execute by entering:		$ @sboxflt
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
$ write sys$output "*** module sboxflt ***"
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
$ write sys$output "Invalid argument given to sboxflt.com file -- ", primary
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
$   if F$SEARCH("sboxflt.imake") .nes. ""
$   then
$      vimake sboxflt
$      purge sboxflt.bld
$   else
$      if F$SEARCH("sboxflt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sboxflt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sboxflt.bld "STD"
$   else
$      @sboxflt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sboxflt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sboxflt.com -
	-p sboxflt.pdf -
	-i sboxflt.imake -
	-t tstsboxflt.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create sboxflt.pdf
procedure help=*
parm infile type=(string,100)
parm outfile type=(string,100)
parm width type=integer count=1 default=3
parm length type=integer count=1 default=3
parm passfilter type=keyword valid=(highpass,lowpass) default=highpass
body
write "SBOXFLT version 31-OCT-94"
boxflt2 &infile &outfile nsw=&width nlw=&length filter=&passfilter
end-proc
.TITLE
SBOXFLT
.HELP
PURPOSE:
SBOXFLT applies a 2-d convolutional high-pass box filter to an input image
by taking the local mean of all pixels contained within a prescribed window 
centered at each pixel of the input image, with reflection performed at image 
boundaries.  Each input value is replaced with the difference between the 
input and the local mean, plus a constant DC-level offset of 128. The filter 
weights are equal to one within the box.  The degree of high-pass filtering 
applied is controlled by the box width and length which may be specified.
A lowpass option is available which replaces each input value with the
calculated local mean.

EXECUTION:
SBOXFLT invokes BOXFLT2 with the specified parameters.  See help for BOXFLT2
for the complete list of defaulted parameters used by BOXFLT2.

.page
Examples:
	sboxfltx  INP  OUT  NLW=21  NSW=451

	This example performs a high-pass filter of size 451 samples by 21
	lines on the input. Reflection is performed at image boundaries.

	sboxfltx  INP  OUT  NLW=21  NSW=451 'lowpass

	This example performs a low-pass filter of size 451 samples by 21
	lines on the input. Reflection is performed at image boundaries.

.page	
Mode of handling boundaries:
		a = pixel (1,1)		b = pixel (1,NS)
		c = pixel (NL,1)	d = pixel (NL,NS)
			+-------+-------+-------+
			| d   c | c   d | d   c |
			|       |       |       |
			| b   a | a   b | b   a |
			|-------|-------|-------|
			| b   a | a   b | b   a |
			|       |       |       |
			| d   c | c   d | d   c |
			|-------|-------|-------|
			| d   c | c   d | d   c |
			|       |       |       |
			| b   a | a   b | b   a |
			+-------+-------+-------+
				RELECTION
.page
OPERATION:
SBOXFLT applies a 2-d convolutional high-pass box filter to an input image
by taking the local mean of all pixels contained within a prescribed window 
centered at each pixel of the input image, with reflection performed at image 
boundaries.  Each input value is replaced with the difference between the 
input and the local mean, plus a constant DC-level offset of 128. The filter 
weights are equal to one within the box.  The degree of high-pass filtering 
applied is controlled by the box width and length which may be specified.
A lowpass option is available which replaces each input value with the
calculated local mean.

WRITTEN BY:  
COGNIZANT PROGRAMMER:
REVISION:  Made Portable for UNIX   Richardson(CRI)  31-Oct-94 

.LEVEL1
.VARIABLE INFILE
STRING - Input dataset
.VARIABLE OUTFILE
STRING - Output dataset
.VARIABLE WIDTH
INTEGER - Filter width in pixels
.VARIABLE LENGTH
INTEGER - Filter length in pixels
.VARIABLE PASSFILTER
KEYWORD - Selects type of filtering (LOWPASS, HIGHPASS)
.LEVEL2
.VARIABLE INFILE
INFILE is the input dataset
.VARIABLE OUTFILE
OUTFILE is the input dataset
.VARIABLE WIDTH
WIDTH is the width in pixels of the box filter.  It must be less than
twice the image width in pixels and defaults to 3.
.VARIABLE LENGTH
LENGTH is the length in lines of the box filter.  It must be less than
twice the image length in pixels and defaults to 3.
.VARIABLE PASSFILTER
FILTER=HIGHPASS specifies that the output is to be the high-pass, rather than
the low-pass, version of the input, i.e., OUT = IN - LOW + DCLEVEL.
The default is high-pass filtering.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create sboxflt.imake
#define PROCEDURE sboxflt
#define R2LIB 
$ Return
$!#############################################################################
$Test_File:
$ create tstsboxflt.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  TEST WITH BYTE IMAGES
!
gen +BOXFA 20 20 LINC=10 SINC=4 IVAL=0
list +BOXFA
sboxflt +BOXFA +BOXFB
list +BOXFB
sboxflt +BOXFA +BOXFC length=11 width=11
list +BOXFC
sboxflt +BOXFA +BOXFD length=11 width=11 'lowpass
list +BOXFD
!
!  TEST WITH HALFWORD IMAGES
!
gen +BOXFA 20 20 LINC=10 SINC=4 IVAL=-100 'HALF
list +BOXFA
sboxflt +BOXFA +BOXFB
list +BOXFB
sboxflt +BOXFA +BOXFC length=11 width=11
list +BOXFC
sboxflt +BOXFA +BOXFD length=11 width=11 'lowpass
list +BOXFD
end-proc
$ Return
$!#############################################################################
