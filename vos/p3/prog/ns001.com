$!****************************************************************************
$!
$! Build proc for MIPL module ns001
$! VPACK Version 1.8, Tuesday, March 14, 2000, 14:40:18
$!
$! Execute by entering:		$ @ns001
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
$ write sys$output "*** module ns001 ***"
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
$ write sys$output "Invalid argument given to ns001.com file -- ", primary
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
$   if F$SEARCH("ns001.imake") .nes. ""
$   then
$      vimake ns001
$      purge ns001.bld
$   else
$      if F$SEARCH("ns001.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ns001
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ns001.bld "STD"
$   else
$      @ns001.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ns001.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ns001.com -
	-p ns001.pdf -
	-i ns001.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ns001.pdf
PROCEDURE	HELP=*

PARM	inp	TYPE=(STRING,40)		! Input dataset name
PARM	out	TYPE=(STRING,40)  COUNT=(1:9)	! Output dataset name(s)
PARM	chan	TYPE=INTEGER COUNT=(1:9) DEFAULT=(1,2,3,4,5,6,7,8,9)
						! Channels to be logged
PARM	date	TYPE=(STRING,15)		! Date of flight
PARM	site	TYPE=(STRING,40)		! Site name
PARM	flight	TYPE=(STRING,20)		! Flight number
PARM	line	TYPE=(STRING,10)		! Flight line number
PARM    run     TYPE=(STRING,10)  DEFAULT=1	! Flight run number
PARM	text	TYPE=STRING  COUNT=(1:9)+
		DEFAULT=("NS001  CHANNEL  1 OF  8   0.45 TO 0.52 MICRONS",+
			 "NS001  CHANNEL  2 OF  8   0.52 TO 0.60 MICRONS",+
			 "NS001  CHANNEL  3 OF  8   0.63 TO 0.69 MICRONS",+
			 "NS001  CHANNEL  4 OF  8   0.76 TO 0.90 MICRONS",+
			 "NS001  CHANNEL  5 OF  8   1.00 TO 1.30 MICRONS",+
			 "NS001  CHANNEL  6 OF  8   1.55 TO 1.75 MICRONS",+
			 "NS001  CHANNEL  7 OF  8   2.08 TO 2.35 MICRONS",+
			 "NS001  CHANNEL  8 OF  8   10.4 TO 12.5 MICRONS",+
                         "NS001  PHANTOM BONUS CHANNEL")
PARM	start	TYPE=INTEGER DEFAULT=0		! GRADREM PARM START
PARM	length	TYPE=INTEGER DEFAULT=1		! GRADREM PARM LENGTH
PARM	linc	TYPE=INTEGER DEFAULT=1		! GRADREM PARM LINC
PARM	filt	TYPE=INTEGER DEFAULT=101	! GRADREM PARM FILT
PARM	gain	TYPE=REAL    DEFAULT=100.0	! GRADREM PARM GAIN
PARM	hist	TYPE=STRING  DEFAULT=NONE VALID=(NONE,NORMAL,WIDE)
						! If NONE, no histogram;
						! else, HIST PARM WIDTH
PARM    scale   TYPE=REAL    DEFAULT=1.0	! C130RECT PARM SCALE
LOCAL	i	TYPE=INTEGER			! Channel number
LOCAL	j	TYPE=INTEGER			! Starting byte of input
LOCAL	n	TYPE=INTEGER INITIAL=1		! Index
LOCAL	nmax	TYPE=INTEGER			! # of channels to be logged
LOCAL	txt	TYPE=STRING
LOCAL   outds	TYPE=STRING
LOCAL	dsname	TYPE=STRING INITIAL=NS001A

BODY

LET nmax=$COUNT(out)
IF (start=0) LET dsname="NS001B"

LOOP						! Log each requested channel
   LET outds=out(n)
   LET i=chan(n)
   LET j=750*i-699
   LET txt=text(i)
   COPY &inp &dsname SS=&j NS=699
   LABEL-ADD &dsname ITEMS="BANDPASS='&txt' DATE='&date' SITE='&site'"
   LABEL-ADD &dsname ITEMS="FLIGHT='&flight' LINE='&line' RUN='&run'"
   IF (start<>0) GRADREM NS001A NS001B START=&start LENGTH=&length+
     LINC=&linc FILT=&filt GAIN=&gain
   IF (hist<>"NONE") HIST NS001B WIDTH=&hist
   C130RECT NS001B &outds DEFL=50.0 SCALE=&scale
   IF (n=nmax) BREAK
   LET n=n+1
END-LOOP

END-PROC
.TITLE 
VICAR2 Procedure NS001
.HELP
     NS001 is the logging procedure for data acquired from the NS001
multispectral scanner. The NS001 scanner is an 8 channel instrument with 7
bands in visible and near-IR and a single thermal-IR band. Its bandpasses are
similar to the LANDSAT-TM system, with an added band at 1.0 to 1.3 microns. The
NS001 scanner was designed to be operated from an aircraft platform, and scenes
are typically acquired from a C-130 operated by NASA AMES. 
.PAGE
     This TAE procedure separates the input data file into separate files for
each band logged, adds the standard annotation for NS001 data, (optionally) 
performs GRADREM for brightness gradient correction, (optionally) prints the 
histograms, and makes aspect and panorama corrections by means of the C130RECT
program. The user may choose to log all bands, or only a subset of the bands.
     The user is required to specify the following information when running
NS001:
	the input dataset name
	the output dataset(s)
	the flight number, line/run number, and date of the data acquisition

Cognizant Programmer: Ron Alley		12/10/85

.LEVEL1
.VARIABLE INP
Input dataset name
.VARIABLE OUT
Output dataset name(s)
.VARIABLE CHAN
Channels to be logged
.VARIABLE DATE
Date of data acquisition
ddmmmyy
.VARIABLE SITE
Site name
.VARIABLE FLIGHT
NASA/AMES flight number
.VARIABLE LINE
Line number for this flight
.VARIABLE RUN
Run number for this flight line
.VARIABLE TEXT
Channel descriptions to be
entered into the VICAR label
.VARIABLE START
First line of the gradient
estimaion region
(USED ONLY IF GRADREM INVOKED)
.VARIABLE LENGTH
Number of lines in the
gradient estimation region
(USED ONLY IF GRADREM INVOKED)
.VARIABLE LINC
Separation between lines used
in the gradient estimation
(USED ONLY IF GRADREM INVOKED)
.VARIABLE FILT
Size of filter window used to
smooth the gradient estimation
function
(USED ONLY IF GRADREM INVOKED)
.VARIABLE GAIN
Scaling factor appied to each
pixel after dividing by the
gradient estimation function
(USED ONLY IF GRADREM INVOKED)
.VARIABLE HIST
Histogram display format.
Valid values: WIDE, NORMAL, NONE
.VARIABLE SCALE
Aspect ratio (expansion factor)
used in panorama correction by
C130RECT
.LEVEL2
.VARIABLE INP
The name of the input dataset
.VARIABLE OUT
The name(s) of the output dataset(s).
.VARIABLE CHAN
Channels to be logged. The default is to log all channels, in order. You
may select any subset to be logged; they will be output IN THE ORDER LISTED.
.VARIABLE DATE
Date of data acquisition. This is the date that the mission was flown. The
date is usually written on the tape label, or it can be retrieved from the
flight log. The preferred format is ddmmmyy, e.g. 01FEB85.
.VARIABLE SITE
This is the user specified site name.
.VARIABLE FLIGHT
NASA/AMES flight number. This is usually written on the tape label; otherwise
it can be obtained from the flight log.
.VARIABLE LINE
Line number. This is usually written on the external tape label.
.VARIABLE RUN
Run number for this flight line. This is usually written on the external
label of the tape.
.VARIABLE TEXT
Channel descriptions to be entered into the VICAR label. If you wish to add
annotation to individual channels, you may do so by entering the default string
followed by the desired additional information. You are urged to always include
the default string, unless the text is incorrect for this data.
.VARIABLE START
This parameter is passed to the program GRADREM. It designates the first line 
of the gradient estimaion region. GRADREM will not be run on the images if this
parameter is defaulted.
.VARIABLE LENGTH
This parameter is passed to the program GRADREM. It designates the number of 
lines in the region selected for gradient estimation. 
.VARIABLE LINC
This parameter is passed to the program GRADREM. Every LINC-th line is used in
the region specified by the START and LENGTH parameters to obtain the gradient
function estimate. The default is 1; that is, every line is used.
.VARIABLE FILT
This parameter is passed to the program GRADREM. After the gradient estimation
function is computed, a FILT element (lowpass) box filter is used to smooth the
function. This value must be an odd integer. The default value is 101. (The
default when running the GRADREM program itself is that no filtering is 
performed. The NS001 procedure requests FILT=101 unless the user overrides this
value. To get no filtering, specify FILT=1.)
.VARIABLE GAIN
This parameter is passed to the program GRADREM. The DN for a pixel output from
GRADREM is computed by the formula

		DNout = GAIN * (DNin/gradient function)

GAIN specifies the factor to be used in the computation. Usually, GAIN should
be about the same as a typical gradient function value (which is the average DN
for pixels in the estimation region of the image). If GAIN is too large, pixels
may be saturated to DN=255. If GAIN is too small, some of the dynamic range of
the data will be lost. The default value of 100.0 is usually appropiate.
.VARIABLE HIST
If the value of this parameter is WIDE or NORMAL, it is passed to the program
HIST. If the value is NONE, HIST is not executed. The NORMAL value leads to an
output format limited to 80 characters in width and is convenient for terminal
output. The WIDE value leads to an output format that is up to 132 characters
and is often used for hardcopy output.
.VARIABLE SCALE
This parameter is passed to the program C130RECT. It specifies the aspect ratio
(expansion factor) used during panorama correction. The default value of 1.0 is
the correct value if, at nadir (along the column of pixels in the center of the
input image), the inter-pixel distances are the same for vertical and horizonal
lines. The larger the value of SCALE, the wider the output image becomes. The 
panorama correction for NS001 data yields an image 954*SCALE pixels wide.
$ Return
$!#############################################################################
$Imake_File:
$ create ns001.imake
#define  PROCEDURE ns001

#define R2LIB 
$ Return
$!#############################################################################
