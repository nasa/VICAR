$!****************************************************************************
$!
$! Build proc for MIPL module ratio
$! VPACK Version 1.9, Thursday, May 31, 2007, 14:09:52
$!
$! Execute by entering:		$ @ratio
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
$ write sys$output "*** module ratio ***"
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
$ write sys$output "Invalid argument given to ratio.com file -- ", primary
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
$   if F$SEARCH("ratio.imake") .nes. ""
$   then
$      vimake ratio
$      purge ratio.bld
$   else
$      if F$SEARCH("ratio.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ratio
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ratio.bld "STD"
$   else
$      @ratio.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ratio.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ratio.com -mixed -
	-i ratio.imake -
	-p ratio.pdf -
	-t tstratio.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ratio.imake
#define  PROCEDURE ratio 
#define R2LIB 

$ Return
$!#############################################################################
$PDF_File:
$ create ratio.pdf
PROCEDURE help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING DEFAULT=""
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM CENTER TYPE=KEYWORD VALID=(CENTER,NOCENTER) DEFAULT=CENTER
PARM MODE TYPE=KEYWORD VALID=(RATIO,DIFFEREN) DEFAULT=RATIO
PARM MODE2 TYPE=KEYWORD VALID=(NOLOG,LOG) DEFAULT=NOLOG
PARM AREA TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SAMPLE TYPE=REAL DEFAULT=5.0
PARM LINC TYPE=INTEGER DEFAULT=20
PARM INCLUDE TYPE=REAL COUNT=2 DEFAULT=(0.0,5.0)
PARM THRESHOL TYPE=REAL DEFAULT=0.0
PARM MODE3 TYPE=KEYWORD VALID=(FILTER,NOFILTER) DEFAULT=FILTER
PARM PERCENT TYPE=REAL DEFAULT=2.0
PARM MODE4 TYPE=KEYWORD VALID=(DISPLAY,NODISPLA) DEFAULT=DISPLAY
PARM ATM1 TYPE=REAL DEFAULT=0.0
PARM ATM2 TYPE=REAL DEFAULT=0.0
LOCAL LFUNC TYPE=STRING 
LOCAL SIZF TYPE=STRING
LOCAL AREF TYPE=STRING
LOCAL FLD1 TYPE=STRING
LOCAL FLD2 TYPE=STRING
BODY
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&&ATM2"
RATIO0 &INP &OUT &SIZF &AREF &FLD1 &FLD2 CENTER=&CENTER MODE=&MODE +
 MODE2=&MODE2 MODE3=&MODE3 MODE4=&MODE4 FUNC=LFUNC
IF (OUT<>"") F2 &INP &OUT &SIZF FUNC=&LFUNC
END-PROC
.TITLE
Vicar Procedure ratio
.HELP
PURPOSE:
ratio operates on two input pictures to generate a third (comparison) picture.
There are four modes of operation:

	1. RATIO            OUT = GAIN*(IN1/IN2)+OFFSET
	2. LOG RATIO        OUT = GAIN*LN(IN1/IN2)+OFFSET
	3. DIFFERENCE       OUT = GAIN*(IN1-IN2)+OFFSET
	4. LOG DIFFERENCE   OUT = GAIN*LN(IN1-IN2+256)+OFFSET

The GAIN and OFFSET values are calculated by RATIO so that either (centering
option, default) the mean DN of the output picture will be 128, and the 
saturation on each tail will be at most at the user-specified level, or 
(nocentering option) the saturation on each tail will be at the user-specified
level. The program F2 is used by RATIO to form the output picture.

.PAGE
EXECUTION:

Examples

Basic Form

ratio INP=(PIC1,PIC2) OUT=RATIO

This is the simplest call to ratio.  Output picture ratio will be made using
the ratio of the two pictures, as in mode 1 above.  Ratioing, then, is the
default mode.  To use differencing, instead, specify 'DIFFEREN.  To get a
natural log output of either the ratio or the difference, use 'LOG or
MODE2=LOG.  (Note: It is possible to not give an output data set name.  In
this case, the program will print the gain and offset, but won't write an
output data set.)

.PAGE
Sampling Parameters

ratio INP=(PIC1,PIC2) OUT=RATIO AREA=(1,1,1024,1024) LINC=5

In this example, only the upper-left corner will be used for sampling, and
further, only every fifth line within this area.

ratio INP=(PIC1,PIC2) OUT=RATIO SAMPLE=20.0

Here, the SAMPLE parameter is used to specify that 20% of the lines in the
image are to be used for sampling.  The default value for SAMPLE is 5.0, so
normally, sampling is done using only 5% of the image lines.

.PAGE
Offset and Gain Parameters

ratio INP=(PIC1,PIC2) OUT=DIFFPIC 'DIFF INCLUDE=(-200.0,200.0)

The INCLUDE option is used here to specify that only difference values between
-200.0 and 200.0 are to be used for statistics.  The default for this range
when calculating differences is [-249.5,250.5].  This option can also be
used to exclude certain ratio values; the default range for ratios is [0.,5.].
If the LOG mode is used, INCLUDE applies to the value before the logarithm
is calculated.

ratio INP=(PIC1,PIC2) OUT=DIFFPIC 'DIFF THRESHOL=5.0

Here, the THRESHOLd option is used to toss out any difference values which
occur less than 5.0 percent as much as the most-frequent value; these are then
excluded from the calculations.  This option can be used for ratios, as well.
The default value is 0.0, that is, to never exclude values only on the basis
of frequency.  

ratio INP=(PIC1,PIC2) OUT=RATIO 'NOFILTER PERCENT=2.0

Normally, a five-element box filter is used to smooth the histogram of ratio
and difference values.  'NOFILTER prevents filtering of the histogram.
    The PERCENT keyword specifies the desired saturation level (in percent).
Since the mean is pegged at DN=128, and the stretch is linear, only one end
of the histogram will be saturated at the specified percentage.  The saturation
at the other end will be less.  The default is 2.0 percent, as used in the
example.

.PAGE
Miscellaneous Parameters

ratio INP=(PIC1,PIC2) OUT=LOGRAT 'LOG 'NODISPLA ATM1=-0.5 ATM2=-0.5

This last example illustrates the remaining commands.  The output picture
will be calculated from the logs of the ratios using the whole image (but
only five percent of the lines) as the sampling area.  Since we are calculating
a ratio, values outside of the range [0.0,5.0] will be excluded, but no
values will be excluded solely on the basis of frequency.  The box filtering
will be done as normal to smooth the histogram, and the program will saturate
the output image to two percent.
    Normally, the ratio (or difference) histogram is printed. 'NODISPLA
prevents this.  Finally, ATM1 and ATM2 are real numbers which will be added to
IN1 and IN2, respectively, before any ratios or differences are calculated. 
These values are generally used as atmospheric correction terms and are usually
negative.  The default is ATM1 = ATM2 = 0.0. 

.PAGE
OPERATION:
The region between the two INCLUDE values is divided into 500 equal parts.
These become the permissible histogram values.  The input pictures are sampled
according to the AREA, SAMPLE, and LINC parameters to form the histogram.
Values beyond the INCLUDE range are ignored.
    The histogram is filtered, then levels populated below the THRESHOLD are
set to zero.  The mean of this histogram is calculated .  Gain and offset 
values are chosen so that the mean is transformed to 128 and the higher of
the dark and bright saturation levels is equal to the requested level of
saturation.
    The histogram is displayed, and the gain and offset passed to the program 
F2, which generates the output picture.

WRITTEN BY:  A.R. Gillespie, 4 January 1974
COGNIZANT PROGRAMMER:  Ron Alley
REVISION:  4		4 June 1986
Made Portable for UNIX      CRI      10-JUL-95

.LEVEL1
.VARIABLE INP
Input files
.VARIABLE OUT
Output data set, if any
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE CENTER
Force output mean=128?
(CENTER,NOCENTER) 
.VARIABLE MODE
Calculation type 
(RATIO,DIFFEREN)
.VARIABLE MODE2
Log mode selection 
(NOLOG,LOG)
.VARIABLE AREA
Sampling area
.VARIABLE SAMPLE
Sampling percentage
.VARIABLE LINC
Line-increment
.VARIABLE INCLUDE
Range of values for 
statistical inclusion
.VARIABLE THRESHOLD
Value-frequency exclusion 
threshold
.VARIABLE MODE3
Filtering selection 
(FILTER,NOFILTER)
.VARIABLE PERCENT
Desired saturation level
.VARIABLE MODE4
Display control 
(DISPLAY,NODISPLA)
.VARIABLE ATM1
IN1 correction
.VARIABLE ATM2
IN2 correction
.LEVEL2
.VARIABLE INP
INP specifies the input files, of which there must be two.
.VARIABLE OUT
OUT specifies the output data set, if any.  If none is specified, the gain and
offset and computed and printed, but F2 is not called and no output data set
is written.
.VARIABLE CENTER
Specifying NOCENTER forces the saturation level of the output image to be at
the requested level on both ends. Specifying CENTER (the default) forces the
output mean to be at 128 DN, and the saturation level to be at the requested
level at one end, and less at the other.
.VARIABLE MODE
MODE may be used to specify whether calculations are to be based on differences
or ratios between the two input images.  If the user asserts MODE=DIFF or 'DIFF
the differences mode will be invoked.  Otherwise the program will use ratioing.
.VARIABLE MODE2
MODE2=LOG and 'LOG both specify that the natural log of the difference or ratio
should be calculated.
.VARIABLE AREA
AREA causes the program to sample only within the region specified.  The
default is to use the whole image for sampling.
.VARIABLE SAMPLE
SAMPLE indicates the percentage of lines within the sampling area which will be
sampled.  All samples on a chosen line are sampled.  The default is 5 percent.
.VARIABLE LINC
LINC specifies the line increment to use in sampling lines.
.VARIABLE INCLUDE
The INCLUDE option may be used to include only a certain range of values for
statistical calculations.  The default range for ratios is [0.0,5.0], and for
differences, [-249.5,250.5].  If the LOG option is used, INCLUDE applies
to the value before the logarithm is calculated.
.VARIABLE THRESHOL
Ratio (and difference) values occuring less than THRESHOLd percent as much
as the most frequent value will be excluded from the calculations.  The
default is THRESHOLd = 0.0, that is, to not exclude any values based only on
frequency.
.VARIABLE MODE3
MODE3 may be used to disable filtering.  Typing 'NOFILTER or MODE3=NOFILTER
will prevent the program from performing its normal 5-element, smoothing
box-filter on the histogram.
.VARIABLE PERCENT
PERCENT is a real number specifying the desired saturation level (in percent).
Since the mean is pegged at DN=128, and the stretch is linear, only one end
of the histogram will be saturated at this percentage.  The saturation at the
other end will be less.  The default is saturation of 2.0 percent.
.VARIABLE MODE4
MODE4 may be used to disable the normal histogram output.  Set it equal to 
NODISPLA or usr 'NODISPLA.
.VARIABLE ATM1
ATM1 specifies a real number to be added to IN1 before computing ratios or
differences.  This value is normally used as an atmospheric correction term,
and is usually negative.  The default is 0.0.
.VARIABLE ATM2
ATM2 specifies a real number to be added to IN1 before computing ratios or
differences.  This value is normally used as an atmospheric correction term,
and is usually negative.  The default is 0.0.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstratio.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! RATIO TEST SCRIPT
! SET TERMINAL WIDTH=132 BEFORE RUNNING THIS SCRIPT
!
! CREATE 4 TEST IMAGES
gen 1 20 20
gen 2 20 20 IVAL=1
genthis tmp1 5 5 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,+
                     18,19,20,21,22,23,24,25)
size tmp1 3 zoom=4 'noin
genthis tmp2 10 10 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,+
                       18,19,20,21,22,23,24,25,26,27,28,29,30,31,+
                       32,33,34,35,36,37,38,39,40,41,42,43,44,45,+
                       46,47,48,49,50,51,52,53,54,55,56,57,58,59,+
                       60,61,62,63,64,65,66,67,68,69,70,71,72,73,+
                       74,75,76,77,78,79,80,81,82,83,84,85,86,87,+
                       88,89,90,91,92,93,94,95,96,97,98,99,100)
size tmp2 4 zoom=2 'noin
!
! RATIO OF 1 AND 2 WITH HISTOGRAM DISPLAY
ratio (1,2) C 
list C
!
! LOG RATIO OF 1 AND 2 WITH HISTOGRAM
ratio (1,2) C 'LOG
list C
!
! DIFFERENCE OF 2 AND 3 WITH HISTOGRAM
ratio (2,3) C 'DIFF
list C
!
! LOG DIFFERENCE OF 1 AND 4 WITH HISTOGRAM
ratio (1,4) C 'LOG 'DIFF
list C
!
! COMPUTATION ONLY (NO OUTPUT SET OR HISTOGRAM)
ratio (3,4) 'NODISP
! 
! COMPUTATION ONLY AND NO FILTERING
ratio (3,4) 'NODISP 'NOFILTER
!
! SAMPLING AREA SPECIFIED, ALONG WITH ATM CORRECTIONS
ratio (3,4) C AREA=(1,1,4,4) SAMPLE=100.0 ATM1=0.5 ATM2=0.5 'NODISP
label-list C
!
! ABOVE, WITH ALL LINES SAMPLES
ratio (3,4) C LINC=1 ATM1=0.5 ATM2=0.5 'NODISP 
label-list C
!
! RATIO BETWEEN 1 AND 2 WITH BOTTOM 10 PERCENT EXCLUDED
ratio (1,2) C THRESH=10.0
list C
!
! AS ABOVE, BUT INCLUDING ONLY THOSE RATIO'S ABOVE .90
ratio (1,2) C INCLUDE=(.90,5.0)
list C
!
! AS ABOVE, BUT WITH 10 PERCENT SATURATION
ratio (1,2) C INCLUDE=(.90,5.0) PERCENT=10.0
list C
! 
! END-SCRIPT
end-proc
$ Return
$!#############################################################################
