$!****************************************************************************
$!
$! Build proc for MIPL module timspix3
$! VPACK Version 1.8, Friday, June 21, 1996, 16:34:13
$!
$! Execute by entering:		$ @timspix3
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
$ write sys$output "*** module timspix3 ***"
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
$ write sys$output "Invalid argument given to timspix3.com file -- ", primary
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
$   if F$SEARCH("timspix3.imake") .nes. ""
$   then
$      vimake timspix3
$      purge timspix3.bld
$   else
$      if F$SEARCH("timspix3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timspix3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timspix3.bld "STD"
$   else
$      @timspix3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timspix3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timspix3.com -
	-p timspix3.pdf -
	-i timspix3.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timspix3.pdf
PROCEDURE      HELP=*
PARM NAME (STRING,30)
PARM CALMODE KEYWORD DEFAULT=CAL VALID=(CAL,NOCAL)
PARM FITAREA INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM DSTAREA INTEGER COUNT=4:40 DEFAULT=(1,1,0,0)
PARM DATE    INTEGER DEFAULT=-1
PARM LENGTH  INTEGER DEFAULT=0
PARM FLIP    KEYWORD DEFAULT=NOFLIP VALID=(FLIP,NOFLIP)
LOCAL BREAD (STRING,132) INITIAL=""
LOCAL AREAD (STRING,132) INITIAL=""
LOCAL INPIX (STRING,40)
LOCAL INAUX (STRING,40)
LOCAL IRAD  (STRING,40)
LOCAL OUT   (STRING,40)
LOCAL IMAGE (STRING,40)
LOCAL FORMAT KEYWORD
LOCAL NL     INTEGER
LOCAL NS     INTEGER
LOCAL NB     INTEGER
LOCAL ZOOM   INTEGER
LOCAL EXPAND INTEGER

BODY
LET INPIX = NAME // ".pix"
LET INAUX = NAME // ".aux"
LET IRAD  = NAME // ".irad"
LET IMAGE = NAME // ".TIMSimage"

IF (FITAREA(3) <> 0) LET BREAD="AREA=&&FITAREA"
IF (DSTAREA(3) <> 0) LET AREAD="AREA=&&DSTAREA"

IF (FLIP="FLIP")
   IF (CALMODE="CAL")
      TIMSCAL (&INPIX,&INAUX) &IMAGE 'IRAD DATE=&DATE
      TO2D &IMAGE
      FLOT &IMAGE &IRAD 'HORI
      TO3D &IRAD
      TRAN &IRAD (SCRTCHD,SCRTCHE,SCRTCHF) BANDS=(1,3,5) 'BSQ
      FIT SCRTCHD  SCRTCHA 'BYTE PERC=3.0 &BREAD
      FIT SCRTCHE  SCRTCHB 'BYTE PERC=3.0 &BREAD
      FIT SCRTCHF  SCRTCHC 'BYTE PERC=3.0 &BREAD
   ELSE
      TRAN &INPIX (SCRTCHD,SCRTCHE,SCRTCHF) BANDS=(1,3,5) 'BSQ
      FLOT SCRTCHD SCRTCHA 'HORI
      FLOT SCRTCHE SCRTCHB 'HORI
      FLOT SCRTCHF SCRTCHC 'HORI
   END-IF
ELSE
   IF (CALMODE="CAL")
      TIMSCAL (&INPIX,&INAUX) &IRAD 'IRAD DATE=&DATE
      TRAN &IRAD (SCRTCHD,SCRTCHE,SCRTCHF) BANDS=(1,3,5) 'BSQ
      FIT SCRTCHD  SCRTCHA 'BYTE PERC=3.0 &BREAD
      FIT SCRTCHE  SCRTCHB 'BYTE PERC=3.0 &BREAD
      FIT SCRTCHF  SCRTCHC 'BYTE PERC=3.0 &BREAD
   ELSE
      TRAN &INPIX (SCRTCHA,SCRTCHB,SCRTCHC) BANDS=(1,3,5) 'BSQ
   END-IF
END-IF

EIGEN (SCRTCHA,SCRTCHB,SCRTCHC) (&IMAGE,SCRTCHE,SCRTCHF) 'DSTR 'CORR +
      DSCA=(1.,1.,0.5) &AREAD EXCLUDE=0. PERC=3.0
ush \rm EMATRIX
ush \rm TMATRIX
LABEL-ADD &IMAGE ITEM="NOTE1='D-STRETCH TRAINED ON ABOVE AREA(S)'"
LET OUT = NAME // ".label"
LABEL-LIST|STDOUT=&OUT| &IMAGE

FORM &IMAGE FORMAT NL NS NB
IF (&LENGTH = 0)
   IF (&NL > 20000)
      LET LENGTH = 5000
   ELSE-IF (&NL > 8000)
      LET LENGTH = 4000
   ELSE
      LET LENGTH = 2000
   END-IF
END-IF
LET EXPAND = 1 + ((&NL-1)/&LENGTH)

STRIPPER &IMAGE SCRTCHA STRIPLEN=&LENGTH GAPWIDTH=62
MASKV SCRTCHA SCRTCHD 'NOTASK 'NOHIST 'NOSYSTEM EXPAND=&EXPAND
STRIPPER SCRTCHE SCRTCHA STRIPLEN=&LENGTH GAPWIDTH=62
MASKV SCRTCHA SCRTCHE 'NOTASK 'NOHIST 'NOSYSTEM EXPAND=&EXPAND
STRIPPER SCRTCHF SCRTCHA STRIPLEN=&LENGTH GAPWIDTH=62
MASKV SCRTCHA SCRTCHF 'NOTASK 'NOHIST 'NOSYSTEM EXPAND=&EXPAND

LET OUT = NAME // ".tif"
VTIFF-FROMVIC (SCRTCHF,SCRTCHE,SCRTCHD) &OUT 'LZW 

FORM SCRTCHD FORMAT NL NS NB
LET ZOOM = &NL * &NS
IF (&ZOOM < 2000000)
   LET ZOOM = 1
ELSE-IF (&ZOOM < 8000000)
   LET ZOOM = -2
ELSE-IF (&ZOOM < 18000000)
   LET ZOOM = -3
ELSE-IF (&ZOOM < 32000000)
   LET ZOOM = -4
ELSE
   LET ZOOM = -5
END-IF

SIZE SCRTCHD SCRTCHA ZOOM=&ZOOM
ush \rm SCRTCHD
SIZE SCRTCHE SCRTCHB ZOOM=&ZOOM
ush \rm SCRTCHE
SIZE SCRTCHF &IMAGE ZOOM=&ZOOM
ush \rm SCRTCHF
LET OUT = NAME // ".ps"
IF (&NS > &NL)
   PSCRIPT (&IMAGE,SCRTCHB,SCRTCHA) &OUT 'TITLE POINT=12 'NOPRINT 'WIDE
ELSE
   PSCRIPT (&IMAGE,SCRTCHB,SCRTCHA) &OUT 'TITLE POINT=12 'NOPRINT
END-IF

ush \rm SCRTCHA
ush \rm SCRTCHB
ush \rm SCRTCHC
ush \rm &IMAGE
END-PROC
.TITLE
VICAR2 Procedure TIMSPIX3
.HELP
     TIMSPIX3 is a procedure designed to process TIMS picture products
in a throughput manner.  There are three possible outputs from 
this procedure: a calibrated, band-interleaved (BIL), halfword-format
image file, a PostScript file, and a TIFF file.

     The user must submit the name (NAME parameter) that is the root of the
PIX and AUX files.  That is, if the starting datasets are MONO3.pix and 
MONO3.aux, the NAME parameter must be "MONO3". If the user

.PAGE
     The user has the option of calibrating the TIMS imagery or performing
the D-stretch processing on the raw data.  The default is to perform the
calibration: this produces the BIL .IRAD file for storage in the TIMSBSQ
tape library. The calibrated, halfword data is fit to byte format prior
to the D-stretch; the user may specify the region of the image used to 
calculate the statistics for the stretch. The default is to use the 
entire image.

     The user may specify up to 10 subareas for use as training sites for 
the D-stretch procedure. The default is to use the enitre image as the 
source for the statistics. 

.PAGE
Cognizant Programmer: Ron Alley              March 20, 1996

.LEVEL1
.VARIABLE NAME
name of input PIX file,
excluding the ".PIX",
i.e. MONO3 for MONO3.PIX
and MONO3.AUX
.VARIABLE CALMODE
calibration option
(CAL or NOCAL)
.VARIABLE FITAREA
optional subarea for 
calc. of stats used in
halfword to byte fit
.VARIABLE DSTAREA
optional training areas
for the D-stretch 
(max. of 10 subareas)
.VARIABLE DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.VARIABLE LENGTH
Length of each strip in
playback image.
(Default is to let the
 procedure compute strip length)
.VARIABLE FLIP
Flip the image left-to-right?
Valid: FLIP, NOFLIP
.LEVEL2
.VARIABLE NAME
The user must provide the base name for the datasets used. For example,
to process "HAWAII32.PIX" and "HAWAII32.AUX" the name "HAWAII32" must be
given.  This procedure will then generate output datasets with the names
"HAWAII32.ps", "HAWAII32.tif", "HAWAII32.irad", and "HAWAII32.label" as
needed.
.VARIABLE CALMODE
User's choice of a calibration option. The default
is to calibrate the data. When this option is in effect,
an IRAD file with the name, xxxxx.irad is written to disk.
.VARIABLE FITAREA
Optional specification of the subarea of the image that will
be used to calculate statistics for the fitting of halfword
data to byte data. The byte imagery thus created is input
to the D-stretch algorithm. The specification of this area 
is only required when the calibrated (CAL) mode is selected.
The default in CAL mode is to use the entire image to calculate
the statistics. 
.VARIABLE DSTAREA
Optional subareas for the training of the D-STRETCH.
If subareas are specified, the data from these areas 
are used to calculate the correlation matrix. The default
is to use the entire image to calculate the correlation 
matrix.
.VARIABLE DATE
TIMSCAL uses the date of data acquisition to determine the proper
calibration coefficients.  If defaulted, the date in the VICAR 
label is used. This parameter is needed only if the VICAR label
is incorrect, or if an abnormal calibration set is to be used.
.VARIABLE LENGTH
Optional length for the stripping of the playback image. 
If no stripping is desired, the user should enter the number 
lines in the image for LENGTH.
.VARIABLE FLIP
Specifying the 'FLIP option will cause a left-to-right mirror
image to be generated.  This is necessary to get the proper
viewing geometry for TIMS data acquired in the Cessna aircraft.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create timspix3.imake
#define  PROCEDURE timspix3

#define R3LIB 
$ Return
$!#############################################################################
