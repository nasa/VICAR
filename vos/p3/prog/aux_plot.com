$!****************************************************************************
$!
$! Build proc for MIPL module aux_plot
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:06
$!
$! Execute by entering:		$ @aux_plot
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
$ write sys$output "*** module aux_plot ***"
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
$   if F$SEARCH("aux_plot.imake") .nes. ""
$   then
$      vimake aux_plot
$      purge aux_plot.bld
$   else
$      if F$SEARCH("aux_plot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake aux_plot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @aux_plot.bld "STD"
$   else
$      @aux_plot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create aux_plot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack aux_plot.com -
	-p aux_plot.pdf -
	-i aux_plot.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create aux_plot.pdf
procedure help=*
parm in       type=(string,40)
parm bbtemps  type=integer    count=2  default=(0,0)
parm ambtemps type=integer    count=2  default=(0,0)
parm dnvalues type=integer    count=2  default=(0,260)
parm title    type=(string,20)         default=" "
local bb1     type=integer
local bb2     type=integer
local amb1    type=integer
local amb2    type=integer
local dn1     type=integer
local dn2     type=integer
body
let bb1=bbtemps(1)
let bb2=bbtemps(2)
let amb1=ambtemps(1)
let amb2=ambtemps(2)
let dn1=dnvalues(1)
let dn2=dnvalues(2)
ush echo aux_plot,\'&in\',&bb1,&bb2,&amb1,&amb2,&dn1,&dn2,\'&title'' > aux_plot.inp
ush idl aux_plot.inp
ush \rm aux_plot.inp
end-proc
.TITLE
VICAR/IDL Procedure AUX_PLOT
.HELP
PURPOSE:
AUX_PLOT uses IDL to produce plots of TIMS aux file data. The blackbody
temperatures, the six channel responses (in DNs), and the ambient
temperatures are plotted at the screen, and, optionally, on the printer.
The plots are printed 4 to a window/page.
.LEVEL1
.VARIABLE IN
input TIMS AUX dataset
.VARIABLE BBTEMPS
temperature range of blackbodies
(deg C)
.VARIABLE AMBTEMPS
ambient temperature range 
(deg C)
.VARIABLE DNVALUES
DN range for all bands
.VARIABLE TITLE
for labels on plots
.LEVEL2
.VARIABLE IN
This is a TIMS AUX file. It may be either the default (16 columns) form,
or the ALL form (41 columns), but it must be in real format. Files
produced by TIMSLOG meet this criterion.
.VARIABLE BBTEMPS
BBTEMPS specifies the extent of the temperature scale for the plot of
blackbody temperatures. If the Blackbody temperatures do not fall in this
range, this and all following plots may not be produced.  The default lets
the program select a scale, after looking at the minimum and maximum.
.VARIABLE AMBTEMPS
AMBTEMPS specifies the extent of the temperature scale for the plot of 
ambient temperatures, in degrees Celsius.  If the temperatures fall outside
this range, this plot may not be produced.  The default lets the program 
select a scale, after looking at the minimum and maximum.

.VARIABLE DNVALUES
DNVALUES specifies the range of DNs spanned in the plots of each channel's
response.  If the actual DNs do not fall in this range for any channel,
the plot for that channel and all following plots may not be produced.
.VARIABLE TITLE
With TITLE, the user may provide a name of up to 20 characters to identify
the individual dataset being plotted.
.END
parm dnvalues type=integer    count=2  default=(0,260)
parm title    type=(string,20)         default=" "
$ Return
$!#############################################################################
$Imake_File:
$ create aux_plot.imake
#define  PROCEDURE aux_plot

#define R3LIB 
$ Return
$!#############################################################################
