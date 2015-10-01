$!****************************************************************************
$!
$! Build proc for MIPL module hyperfix
$! VPACK Version 1.8, Friday, February 08, 2002, 13:44:58
$!
$! Execute by entering:		$ @hyperfix
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
$ write sys$output "*** module hyperfix ***"
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
$ write sys$output "Invalid argument given to hyperfix.com file -- ", primary
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
$   if F$SEARCH("hyperfix.imake") .nes. ""
$   then
$      vimake hyperfix
$      purge hyperfix.bld
$   else
$      if F$SEARCH("hyperfix.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hyperfix
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hyperfix.bld "STD"
$   else
$      @hyperfix.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hyperfix.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hyperfix.com -
	-p hyperfix.pdf -
	-i hyperfix.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create hyperfix.pdf
Procedure help=*

parm inp	(string,80)
parm out	(string,80)
parm sl		integer default=1
parm nl		integer default=0
local i		integer	initial=71
local j		integer
local nli       integer
local offset	real
refgbl $echo

Body

let $echo="yes"

if (nl = 0) 
  FORM &inp NL=nli
else
  let nli=nl
end-if

COPY &inp &out NS=255 NL=&nli SL=&sl
GEN SCR990229SAMP NL=&nli NS=255 'REAL LINC=0.0 SINC=1.0 IVAL=2.0

let offset = $FLOAT(sl) + 0.5
loop
  COPY &inp SCR990229RAW SB=&i NB=1
  GEN SCR990229LINE NL=&nli NS=255 'REAL LINC=1.0 SINC=0.0 IVAL=&offset
  TRAN (SCR990229LINE,SCR990229SAMP) SCR990229MAP 'BIL
  TGEOM2 (SCR990229MAP,SCR990229RAW) SCR990229FIX
  INSERT3D (&out,SCR990229FIX) BAND=&i 'UPDATE 'OVER
  if (i = 242) break
  let offset = offset - 0.00877193
  let i = i + 1
end-loop
ush rm -f SCR990229*


End-proc

.TITLE
TAE PROCEDURE HYPERFIX
.HELP
PURPOSE:
 
   HYPERFIX is a VICAR procedure to correct for the misregistration of
the SWIR bands in HYPERION image data. It assumes that all SWIR channels
(Channels 71 - 242) are misregistered 1.0 pixel to the right, and the up-down
misregistration varies in evenly spaced increments from 0.5 pixels down for
Channel 71, to 1.0 pixels up for Channel 242.
 
EXECUTION:
 
   The following is the execution statement format for HYPERFIX:
 
		HYPERFIX INP OUT PARAMS
 
   where INP, OUT, and PARAMS are parameters discussed in their respective
parameter sections. 
.PAGE
OPERATION:
 
   HYPERFIX copies the input file into the output file, then for each of
Bands 71 through 242, it creates a rectification image using GEN and TRAN,
performs a registration using TGEOM2, and replaces that band into the output 
file, using INSERT3D.

   The user may process a subset of the image in the line direction, by
using the SL and NL parameters.  The last sample (256) is removed from
the output image, since there are no corresponding pixels to this sample in
the SWIR bands.  Otherwise, no subsetting is permitted in the sample direction.

   Since INSERT3D requires the input file to be in BSQ format, the input
to HYPERFIX must also be in BSQ format.
.PAGE 
EXAMPLES:
 
1) HYPERFIX INP OUT
 
2) HYPERFIX INP OUT SL=101 NL=500
 
 
WRITTEN BY:  Ron Alley     6 February 2002
 
COGNIZANT PROGRAMMER:  same
 
REVISION: NEW
.LEVEL1
.VARIABLE INP
input dataset name (BSQ format)
.VARIABLE OUT
output dataset name
.VARIABLE SL
first line of input to be used
.VARIABLE NL
# of lines of input to be used
(Default is entire input image)
.LEVEL2
.VARIABLE INP
The name of the input dataset. The input dataset must be in band sequential
(BSQ) format.
.VARIABLE OUT
The name of the output dataset.
.VARIABLE SL
The first line of input image to be output.
.VARIABLE NL
The number of lines of input image to be output.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create hyperfix.imake
#define  PROCEDURE hyperfix

#define R2LIB 
$ Return
$!#############################################################################
