$!****************************************************************************
$!
$! Build proc for MIPL module confill2
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:06
$!
$! Execute by entering:		$ @confill2
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
$ write sys$output "*** module confill2 ***"
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
$   if F$SEARCH("confill2.imake") .nes. ""
$   then
$      vimake confill2
$      purge confill2.bld
$   else
$      if F$SEARCH("confill2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake confill2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @confill2.bld "STD"
$   else
$      @confill2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create confill2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack confill2.com -
	-p confill2.pdf -
	-i confill2.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create confill2.pdf
PROCEDURE HELP=*
 PARM INP         TYPE=(STRING,60)
 PARM OUT         TYPE=(STRING,60)
 PARM BACKGRND    TYPE=REAL				DEFAULT=0.0
 PARM MODE	  TYPE=KEYWORD	VALID=(LINEAR,SPLINE)	DEFAULT=SPLINE
 PARM IDS         TYPE=(STRING,60)   COUNT=4 +
       DEFAULT=(confillscr1,confillscr2,confillscr3,confillscr4)
 LOCAL IDS1       TYPE=STRING
 LOCAL IDS2       TYPE=STRING
 LOCAL IDS3       TYPE=STRING
 LOCAL IDS4       TYPE=STRING

BODY
let ids1=ids(1)
let ids2=ids(2)
let ids3=ids(3)
let ids4=ids(4)
flot &inp &ids1 'clock
confill &ids1 (&ids2,&ids3) backgrnd=&backgrnd mode=&mode
flot &ids2 &ids1 'counter
flot &ids3 &ids2 'counter
confill &inp (&ids3,&ids4) backgrnd=&backgrnd mode=&mode
f2 (&ids1,&ids3,&ids2,&ids4) &out +
              func="((in3+0.0001)*in2+(in4+0.0001)*in1)/(in3+in4+0.0002)"
ush \rm &ids1
ush \rm &ids2
ush \rm &ids3
ush \rm &ids4
END-PROC
.TITLE
 CONFILL2 --  2-dimensional CONtour line image FILL-in procedure
.HELP
      CONFILL2 is a VICAR procedure that fills by interpolation an image of
 contour lines.  The input image must consist of contour lines, each of whose
 DN is equal to the value of the contour.  BYTE, HALF, FULL, and REAL data
 formats are all accepted as input.  The background pixels must all have a
 single DN value, given by the BACKGRND parameter.  Interpolation is performed 
 in the horizontal and vertical directions in separate steps, and the final 
 result at each pixel is a linear interpolation of the horizontal and vertical 
 values.
      The user may choose either a linear or cubic spline interpolation for the
 horizontal and vertical interpolations, but the final combination is always by
 linear interpolation.  The values of the outermost contours are extended to 
 the edges of the image.
      This procedure uses four intermediate datasets, each the size of the
 input image.  The user is advised to override the default intermediate 
 dataset names with ones specifying scratch disks if space is likely to be
 a problem in the default directory.  The user is also warned that these
 intermediate datasets are all deleted upon completion of CONFILL2.
.PAGE
 ORIGINAL PROGRAMMER:  Ron Alley   13 March 1992
 
 CURRENT COGNIZANT PROGRAMMER: Ron Alley
.LEVEL1
.VARIABLE INP
 input contour image
.VARIABLE OUT
 output filled image
.VARIABLE IDS
 four intermediate dataset
 names.  Each will be as
 big as the input (twice as
 large for BYTE data)
.VARIABLE BACKGRND
 DN value of background pixels
.VARIABLE MODE
 interpoloation mode
 Valid: LINEAR, SPLINE
.LEVEL2
.VARIABLE INP
 input data set
.VARIABLE OUT
 the output image, filled by a 2-dimensional interpolation of contour lines.
.VARIABLE BACKGRND
 CONFILL2 expects all background pixels to be of a single DN value, specified
 by this parameter (Default value is 0).  All pixels with different values
 are assumed to be part of a contour line.
.VARIABLE MODE
 Two interpolation modes are currently available in CONFILL: linear and cubic
 spline.  Extrapolation, when needed, is done by extension of the outermost
 contour. The valid keywords are: LINEAR, and SPLINE. SPLINE is the default.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create confill2.imake
#define  PROCEDURE confill2

#define R3LIB 
$ Return
$!#############################################################################
