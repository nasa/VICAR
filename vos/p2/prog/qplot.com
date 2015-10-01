$!****************************************************************************
$!
$! Build proc for MIPL module qplot
$! VPACK Version 1.9, Tuesday, July 01, 2014, 18:10:32
$!
$! Execute by entering:		$ @qplot
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
$ write sys$output "*** module qplot ***"
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
$ write sys$output "Invalid argument given to qplot.com file -- ", primary
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
$   if F$SEARCH("qplot.imake") .nes. ""
$   then
$      vimake qplot
$      purge qplot.bld
$   else
$      if F$SEARCH("qplot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake qplot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @qplot.bld "STD"
$   else
$      @qplot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create qplot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack qplot.com -mixed -
	-i qplot.imake -
	-p qplot.pdf -
	-t tstqplot.pdf tstqplot.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create qplot.imake
#define PROCEDURE qplot
#define R2LIB
$ Return
$!#############################################################################
$PDF_File:
$ create qplot.pdf
procedure help=*
 PARM INP        TYPE=STRING   COUNT=(1:10)
 PARM PLOTOUT    TYPE=STRING   COUNT=(0:1)     DEFAULT="NONE"
 PARM PLOTFMT    TYPE=STRING                   DEFAULT="GNUPLOT"
 PARM NCHAN      TYPE=INTEGER  COUNT=1         DEFAULT=1
 PARM PROCESS    TYPE=INTEGER  COUNT=(0,5:50)  DEFAULT=--
 PARM SPROCESS   TYPE=INTEGER  COUNT=(0,2:20)  DEFAULT=--
 PARM TITLE      TYPE=STRING                   DEFAULT="IPL LINE PLOT"
 PARM LOTITLE    TYPE=KEYWORD  VALID=(LOTITLE,HITITLE)  DEFAULT=HITITLE
 PARM XTITLE     TYPE=STRING   DEFAULT="RELATIVE SAMPLE NUMBER"
 PARM YTITLE     TYPE=STRING                   DEFAULT="DN VALUE"
 PARM LABELSIZ   TYPE=INTEGER                  DEFAULT=12
 PARM LOLABEL    TYPE=KEYWORD  VALID=(LOLABEL,HILABEL)  DEFAULT=HILABEL
 PARM TICS       TYPE=KEYWORD  VALID=(TICS,NOTICS)      DEFAULT=NOTICS
 PARM NORM       TYPE=KEYWORD  VALID=(NORM,NONORM)      DEFAULT=NONORM
 PARM RDS        TYPE=REAL                     DEFAULT=0.0
 PARM DISPLACE   TYPE=REAL                     DEFAULT=0.0
 PARM XLENGTH    TYPE=REAL     COUNT=1         DEFAULT=9.0
 PARM YLENGTH    TYPE=REAL     COUNT=1         DEFAULT=7.0
 PARM XSCALE     TYPE=REAL     COUNT=(0,2)     DEFAULT=--
 PARM YSCALE     TYPE=REAL     COUNT=(0,2)     DEFAULT=--
 PARM XVALUES    TYPE=REAL     COUNT=(0,2:20)  DEFAULT=--
 PARM YVALUES    TYPE=REAL     COUNT=(0,2:20)  DEFAULT=--
 PARM DEVTYPE    TYPE=STRING                   DEFAULT="lpr"
 BODY
! refgbl $syschar

if (PLOTOUT = "YES")
  let PLOTFMT = "eps"
end-if


QPLOT2 &INP TITLE=@TITLE LOTITLE=@LOTITLE XTITLE=@XTITLE +
       YTITLE=@YTITLE LABELSIZ=@LABELSIZ LOLABEL=@LOLABEL +
       NORM=@NORM RDS=@RDS DISPLACE=@DISPLACE XLENGTH=@XLENGTH +
       YLENGTH=@YLENGTH XSCALE=@XSCALE YSCALE=@YSCALE +
       NCHAN=@NCHAN PROCESS=@PROCESS SPROCESS=@SPROCESS +
       XVALUES=@XVALUES YVALUES=@YVALUES TICS=@TICS +
       PLOTOUT=@PLOTOUT PLOTFMT=&PLOTFMT

if (PLOTOUT = "NONE")
  ! display plot on screen
  ush gnuplot qplot.gpi
else-if (PLOTOUT = "YES" or PLOTOUT = "yes")
  ! print eps file
  ush gnuplot qplot.eps.gpi
  write "ush &DEVTYPE qplot.eps"
end-if

!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=("DN values",spectral,plot,file,PostScript)

END-PROC

.TITLE
TAE procedure which calls QPLOT2
.HELP
Purpose:  QPLOT calls QPLOT2 to plot the DN values along a specified straight 
  line through an image. It also does spectral plots for multi-channel data. 
  The plot will be displayed on the terminal unless PLOTOUT = YES, in
  which case QPLOT2 generates a PostScipt (EPS) output file which will be printed
  on the device specified by DEVTYPE.

Parameters:  There are two types of parameters: 

      (1) Parameters with a single specification:
                INP             OUT             NCHAN
		TITLE		LOTITLE		XTITLE
		YTITLE		LABELSIZ	LOLABEL
		NORM		RDS		DISPLACE
		XLENGTH		YLENGTH         TICS
		XSCALE		YSCALE
      (2) Parameters which have a specification for each line plotted:
		PROCESS		SPROCESS
		XVALUES         YVALUES

Restrictions:
  (1) Maximum number of lines plotted is 10.
  (2) Maximum x axis length is 6 feet. (on PRINTRONIX)
  (3) Maximum y axis length is 12 inches. (on PRINTRONIX)
  (4) Spectral plots require a single input in mss format.


Vicar Plotting

The Vicar Plotting system can produce plots on either a display device or
in a postscript file which can be sent to a printer.  This is controlled
by the parameter PLOTOUT. If PLOTOUT is "NONE", the plot is displayed
interactively by gnuplot. If PLOTOUT is "YES", the plot is written as a
Postscript file and printed. If PLOTOUT is something other than "NONE" or
"YES", the value is used as a filename for the output, which will be
either Postscript or a gnuplot script, depending on the value of PLOTFMT.

Original Programmer:  John H. Reimer
Cognizant Programmer:  John H. Reimer
Revision: One - 15 April 1984
                14 November 1988
Made portable: 7-Nov 1995 Florance Moss
 01jul14 -wlb- qplot2 Replaced NODISP with PLOTOUT to work with the new
        qplot2 interface, which changed as part of an update switching
        from XRTPS to Gnuplot.
 23aug06 -lwk- since default for Unix is now different from VMS, disabled
	latter option (VMS no longer being supported);  replaced NOPRINT 
	with NODISP (apparently the behaviour of the XRTPS software has 
	changed!)

.level1
.variable inp
STRING - Input data set
.variable plotout
STRING - Output plot mode or file
.variable plotfmt
STRING - Format of output file
.variable process
INTEGER - DSN1,SL1,SS1,EL1,ES1, DSN2,SL2,SS2,EL2,ES2, ... 
.variable nchan
INTEGER - Number of channels in mss formatted input data set.
.variable sprocess
INTEGER - LINE1,SAMPLE1, LINE2,SAMPLE2, ... 
.variable title
STRING - Title of plots.
.variable lotitle
KEYWORD - Lower position for title.
.variable xtitle
STRING - X axis title
.variable ytitle
STRING - Y axis title
.variable labelsiz
REAL - Height of label (inches).
.variable lolabel
KEYWORD - Lower position for label.
.variable tics
KEYWORD - Put 10 tic marks per inch.
.variable norm
KEYWORD - Normalizes data to 1.
.variable rds
REAL - DN scaling factor.
.variable displace
REAL - Displacement for subsequent lines.
.variable xlength
REAL - Length of X axis (inches).
.variable ylength
REAL - Length of Y axis (inches).
.variable xscale
REAL - Min & Max of X axis.
.variable yscale
REAL - MIN & Max of Y axis.
.variable xvalues
REAL - Rescaling factors.
.variable yvalues
REAL - Rescaling factors.
.variable devtype
KEYWORD - printer command for hardcopy
.level2
.variable inp
Input data set; maximum number is 10.
.variable plotout
Output plot mode or file. If PLOTOUT is "NONE", the plot is displayed
interactively by gnuplot. If PLOTOUT is "YES", the plot is written as
a Postscript file and printed. If PLOTOUT is something other than
"NONE" or "YES", the value is used as a filename for the output, which
will be either Postscript or a gnuplot script, depending on the value
of PLOTFMT.
.variable plotfmt
STRING - Format of output file, either "gnuplot" or "eps".
.variable process
Specifies one or more lines to be plotted. Following the keyword,
each plot is specified by a set of five numbers. The first value 
in each set specifies the Input Data Set Number and  the remaining four
values specify the Starting Line, Starting Sample, Ending Line, and 
Ending Sample. (No Default).  NOTE:  The line plotted will cover
SQRT((EL-SL)**2+(ES-SS)**2) "relative samples" along the horizontal axis and
will start at "relative sample" 1.
.variable nchan
Specifies the number of spectral channels in an input data set in mss
format. This keyword is used in conjunction with the SPROCESS keyword.
(Default is 1)
.variable sprocess
Specifies one or more spectral plots. This keyword requires that the input 
be in mss format and that NCHAN is specified. Following the keyword,
each spectral plot is specified by a set of two numbers. The first value
in each set specifies the Line and the second value the Sample of the
point within the first channel through which the spectral plot is to be
done. (No Default)
.variable title
Used to specify the title of plots (Max length of 52 characters).
(Default is 'IPL LINE PLOT', or for spectral plots, 'IPL SPECTRAL PLOT')
.variable lotitle
Specifies that the title will be written within the 8.5 x 11 area
(if the y axis length is less than or equal to 7 inches). (Default
is to place the title at the top of the page)
.variable xtitle
Specifies the title for the X axis (Max length of 52 characters). (Default is
'Relative Sample Number')
.variable ytitle
Specifies the title for the Y axis (Max length of 52 characters). (Default is
'DN Value')
.variable labelsiz
Specifies the height in inches of the label characters. For a value
of 0.0, no labels are printed. (Default is 0.12)
.variable lolabel
Specifies that labels will be written within the 8.5 x 11 space.
(Default is to place the labels at the top of the page.)
.variable norm
Causes DN values to be scaled linearly so that the largest value
becomes 1.  The length of the y axis is set to 5 inches.
(Default is that this is not done.)
.variable rds
Causes DN values to be scaled by the following equation:
OUT=SQRT(IN**2-RDS**2). (Default is that this is not done)
.variable displace
Specifies that subsequent lines on the same plot will be displaced
by the given amount.  This is specified in terms of the final plotted
vertical values, rather than input DN, in the cases where the input
values are scaled. (Default is 0.0)
.variable xlength
Specifies the length in inches of the X axis. (Default is 9.0; Max is 72.0)
.variable ylength
Specifies the length in inches of the Y axis. (Default is 7.0; max is 12.0)
.variable xscale
Specifies the scale used along the X axis.  The X axis will be drawn going
from a minimum of the first value to a maximum of the second.  The defaults
for these values are obtained by determining the minimum and maximum X 
values to be plotted on the axis and then passing these values to the
subroutine SCALE.
SCALE determines the scaling so that 1 inch along the axis will always be
an interval of 1,2,3,4,5,6 or 8*(10**n) units.  This usually results in 
having the plotted lines occupy only a portion of the axis.  By using the 
XSCALE keyword the user can force plots to occupy a greater portion of the 
X axis.  Axis values are printed every inch, however, and if it is desired
that these values be nice round numbers the quantity:
                  (XSCALEMAX-XSCALEMIN)/XLENGTH 
should be a nice round number.
.variable yscale
Specifies the scale used along the Y axis.  The Y axis will be drawn going
from a minimum of the first value to a maximum of the second.  The defaults
for these values are obtained by determining the minimum and maximum Y 
values to be plotted on the axis and then passing these values to the
subroutine SCALE.
SCALE determines the scaling so that 1 inch along the axis will always be
an interval of 1,2,3,4,5,6 or 8*(10**n) units.  This usually results in 
having the plotted lines occupy only a portion of the axis.  By using the 
YSCALE keyword the user can force plots to occupy a greater portion of the 
Y axis.  Axis values are printed every inch, however, and if it is desired
that these values be nice round numbers the quantity:
                  (YSCALEMAX-YSCALEMIN)/YLENGTH 
should be a nice round number.
.variable xvalues
Allows the user to rescale the actual x (sample) data values of the 
lines to be plotted. XVALUES is followed by sets of two real values, one
set for each line to be plotted. The two values in each set specify the
minimum and maximum x values. The first value (the minimum) is the point
along the X axis where the first data point will be plotted.  The second
value (the maximum) is the point along the X axis where the last data
point will be plotted.
(DEFAULTS: XMIN=1.0, XMAX=SQRT[(EL-SL)**2+(ES-SS)**2]+1
.variable yvalues
Allows the user to rescale the actual y (DN) data values of the 
lines to be plotted. YVALUES is followed by sets of two real values, one
set for each line to be plotted. The two values in each set specify the
minimum and maximum y values. The first value (the minimum) is the point
along the Y axis where a DN of zero will be plotted.  The second value
(the maximum) is the point along the Y axis where a DN of 255 (32767 for
halfword data) will be plotted.
(DEFAULT: YMIN=0.0, YMAX=255 (byte) YMAX=32767 (halfword)   )
.variable devtype
The command which causes the hardcopy to be printed. Default command is 
lpr for Unix platforms.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstqplot.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

! Generate datasets to test with

gen inp1 nl=20 ns=20
gen inp2 nl=20 ns=20 linc=10 sinc=10
gen inp3 nl=128 ns=128 linc=5 sinc=5

! Just defaults
qplot (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70)

! Multi-axes  Minor default change from now on

qplot (inp1,inp2,inp3) 'norm proc=(1,1,1,20,20, 2,1,1,20,20, 3,1,1,128,128)

! Change some labels

qplot (inp1,inp2,inp3) title="MIPL test run for QPLOT" +
      xtitle="This is the X axis" ytitle="This is the Y axis" +
      proc=(3,1,1,64,64)

! Save plot to qplot.eps and print

qplot inp1 yscale=(0,70) proc=(1,1,1,20,20) plotout="YES"

! Change output file name

qplot inp1 yscale=(0,70) proc=(1,1,1,20,20) plotout="qplot1" plotfmt="eps"

end-proc
$!-----------------------------------------------------------------------------
$ create tstqplot.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

gen inp1 nl=20 ns=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp2 nl=20 ns=20 linc=10 sinc=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp3 nl=128 ns=128 linc=5 sinc=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
qplot (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70)
if (PLOTOUT = "YES")
end-if
QPLOT2 (inp1,inp2,inp3) TITLE=@TITLE LOTITLE=@LOTITLE XTIT+
LE=@XTITLE         YTITLE=@YTITLE LABELSIZ=@LABELSIZ LOLABE+
L=@LOLABEL         NORM=@NORM RDS=@RDS DISPLACE=@DISPLACE XLENGT+
H=@XLENGTH         YLENGTH=@YLENGTH XSCALE=@XSCALE YSCA+
LE=@YSCALE         NCHAN=@NCHAN PROCESS=@PROCESS SPROCESS+
=@SPROCESS         XVALUES=@XVALUES YVALUES=@YVALUES +
TICS=@TICS         PLOTOUT=@PLOTOUT PLOTFMT=GNUPLOT
Beginning VICAR task QPLOT2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (PLOTOUT = "NONE")
  ush gnuplot qplot.gpi
else-if (PLOTOUT = "YES" or PLOTOUT = "yes")
end-if
END-PROC
qplot (inp1,inp2,inp3) 'norm proc=(1,1,1,20,20, 2,1,1,20,20, 3,1,1,128,128)
if (PLOTOUT = "YES")
end-if
QPLOT2 (inp1,inp2,inp3) TITLE=@TITLE LOTITLE=@LOTITLE XTIT+
LE=@XTITLE         YTITLE=@YTITLE LABELSIZ=@LABELSIZ LOLABE+
L=@LOLABEL         NORM=@NORM RDS=@RDS DISPLACE=@DISPLACE XLENGT+
H=@XLENGTH         YLENGTH=@YLENGTH XSCALE=@XSCALE YSCA+
LE=@YSCALE         NCHAN=@NCHAN PROCESS=@PROCESS SPROCESS+
=@SPROCESS         XVALUES=@XVALUES YVALUES=@YVALUES +
TICS=@TICS         PLOTOUT=@PLOTOUT PLOTFMT=GNUPLOT
Beginning VICAR task QPLOT2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (PLOTOUT = "NONE")
  ush gnuplot qplot.gpi
else-if (PLOTOUT = "YES" or PLOTOUT = "yes")
end-if
END-PROC
qplot (inp1,inp2,inp3) title="MIPL test run for QPLOT"  +
      xtitle="This is the X axis" ytitle="This is the Y axis"  +
      proc=(3,1,1,64,64)
if (PLOTOUT = "YES")
end-if
QPLOT2 (inp1,inp2,inp3) TITLE=@TITLE LOTITLE=@LOTITLE XTIT+
LE=@XTITLE         YTITLE=@YTITLE LABELSIZ=@LABELSIZ LOLABE+
L=@LOLABEL         NORM=@NORM RDS=@RDS DISPLACE=@DISPLACE XLENGT+
H=@XLENGTH         YLENGTH=@YLENGTH XSCALE=@XSCALE YSCA+
LE=@YSCALE         NCHAN=@NCHAN PROCESS=@PROCESS SPROCESS+
=@SPROCESS         XVALUES=@XVALUES YVALUES=@YVALUES +
TICS=@TICS         PLOTOUT=@PLOTOUT PLOTFMT=GNUPLOT
Beginning VICAR task QPLOT2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (PLOTOUT = "NONE")
  ush gnuplot qplot.gpi
else-if (PLOTOUT = "YES" or PLOTOUT = "yes")
end-if
END-PROC
qplot inp1 yscale=(0,70) proc=(1,1,1,20,20) plotout="YES"
if (PLOTOUT = "YES")
  let PLOTFMT = "eps"
end-if
QPLOT2 inp1 TITLE=@TITLE LOTITLE=@LOTITLE XTITLE=@XTITLE  +
       YTITLE=@YTITLE LABELSIZ=@LABELSIZ LOLABEL=@LOLABEL  +
       NORM=@NORM RDS=@RDS DISPLACE=@DISPLACE XLENGTH=@XLENGTH  +
       YLENGTH=@YLENGTH XSCALE=@XSCALE YSCALE=@YSCALE  +
       NCHAN=@NCHAN PROCESS=@PROCESS SPROCESS=@SPROCESS  +
       XVALUES=@XVALUES YVALUES=@YVALUES TICS=@TICS  +
       PLOTOUT=@PLOTOUT PLOTFMT=eps
Beginning VICAR task QPLOT2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (PLOTOUT = "NONE")
else-if (PLOTOUT = "YES" or PLOTOUT = "yes")
  ush gnuplot qplot.eps.gpi
  ush lpr qplot.eps
end-if
END-PROC
qplot inp1 yscale=(0,70) proc=(1,1,1,20,20) plotout="qplot1" plotfmt="eps"
if (PLOTOUT = "YES")
end-if
QPLOT2 inp1 TITLE=@TITLE LOTITLE=@LOTITLE XTITLE=@XTITLE  +
       YTITLE=@YTITLE LABELSIZ=@LABELSIZ LOLABEL=@LOLABEL  +
       NORM=@NORM RDS=@RDS DISPLACE=@DISPLACE XLENGTH=@XLENGTH  +
       YLENGTH=@YLENGTH XSCALE=@XSCALE YSCALE=@YSCALE  +
       NCHAN=@NCHAN PROCESS=@PROCESS SPROCESS=@SPROCESS  +
       XVALUES=@XVALUES YVALUES=@YVALUES TICS=@TICS  +
       PLOTOUT=@PLOTOUT PLOTFMT=eps
Beginning VICAR task QPLOT2
qplot2 version 12-Jul-2013 (64-bit gnuplot) - rjb
if (PLOTOUT = "NONE")
else-if (PLOTOUT = "YES" or PLOTOUT = "yes")
end-if
END-PROC
end-proc
$ Return
$!#############################################################################
