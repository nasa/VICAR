$!****************************************************************************
$!
$! Build proc for MIPL module timspix2
$! VPACK Version 1.8, Friday, June 21, 1996, 16:34:08
$!
$! Execute by entering:		$ @timspix2
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
$ write sys$output "*** module timspix2 ***"
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
$ write sys$output "Invalid argument given to timspix2.com file -- ", primary
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
$   if F$SEARCH("timspix2.imake") .nes. ""
$   then
$      vimake timspix2
$      purge timspix2.bld
$   else
$      if F$SEARCH("timspix2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timspix2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timspix2.bld "STD"
$   else
$      @timspix2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timspix2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timspix2.com -
	-p timspix2.pdf -
	-i timspix2.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timspix2.pdf
PROCEDURE      HELP=*

PARM NAME (STRING,30)
PARM CALMODE (STRING) DEFAULT=CAL VALID=(CAL,NOCAL)
PARM FITAREA INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM DSTAREA INTEGER COUNT=4:40 DEFAULT=(1,1,0,0)
PARM ASPECT  REAL    DEFAULT=1.0
PARM LENGTH  INTEGER COUNT=1 DEFAULT=3500
PARM DATE    INTEGER DEFAULT=-1
PARM FLIP    KEYWORD DEFAULT=NOFLIP VALID=(FLIP,NOFLIP)
LOCAL BREAD (STRING,132) INITIAL=""
LOCAL AREAD (STRING,132) INITIAL=""
LOCAL INPIX (STRING,30)
LOCAL INAUX (STRING,30)
LOCAL DSTR1 (STRING,30)
LOCAL DSTR3 (STRING,30)
LOCAL DSTR5 (STRING,30)
LOCAL OUTB (STRING,30)
LOCAL OUTG (STRING,30)
LOCAL OUTR (STRING,30)
LOCAL IRAD (STRING,30) 

BODY
LET INPIX = NAME // ".pix"
LET INAUX = NAME // ".aux"
LET IRAD = NAME // ".irad"
LET DSTR1 = NAME // ".ds1"
LET DSTR3 = NAME // ".ds3"
LET DSTR5 = NAME // ".ds5"
LET OUTB = NAME // ".blu"
LET OUTG = NAME // ".grn"
LET OUTR = NAME // ".red"

IF (FITAREA(3) <> 0) LET BREAD="AREA=&&FITAREA"
IF (DSTAREA(3) <> 0) LET AREAD="AREA=&&DSTAREA"

IF (FLIP="FLIP")
   IF (CALMODE="CAL")
      TIMSCAL (&INPIX,&INAUX) SCRTCHA 'IRAD DATE=&DATE
      TO2D SCRTCHA
      FLOT SCRTCHA &IRAD 'HORI
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

EIGEN (SCRTCHA,SCRTCHB,SCRTCHC) (SCRTCHD,SCRTCHE,SCRTCHF) 'DSTR 'CORR +
      DSCA=(1.,1.,0.5) &AREAD EXCLUDE=0. PERC=3.0
ush \rm SCRTCHB
ush \rm SCRTCHC
LABEL-ADD SCRTCHD ITEM="INFO1='D-STRETCH TRAINED ON ABOVE AREA(S)'"
LABEL-ADD SCRTCHE ITEM="INFO1='D-STRETCH TRAINED ON ABOVE AREA(S)'"
LABEL-ADD SCRTCHF ITEM="INFO1='D-STRETCH TRAINED ON ABOVE AREA(S)'"
C130RECT SCRTCHD &DSTR1 DEFL=38.28 SCALE=&ASPECT
ush \rm SCRTCHD
C130RECT SCRTCHE &DSTR3 DEFL=38.28 SCALE=&ASPECT
ush \rm SCRTCHE
C130RECT SCRTCHF &DSTR5 DEFL=38.28 SCALE=&ASPECT
ush \rm SCRTCHF
STRIPPER &DSTR1 SCRTCHA STRIPLEN=&LENGTH GAPWIDTH=47
MASKV SCRTCHA &OUTB 
STRIPPER &DSTR3 SCRTCHA STRIPLEN=&LENGTH GAPWIDTH=47
MASKV SCRTCHA &OUTG
STRIPPER &DSTR5 SCRTCHA STRIPLEN=&LENGTH GAPWIDTH=47
MASKV SCRTCHA &OUTR
ush \rm SCRTCHA
END-PROC
.TITLE
VICAR2 Procedure TIMSPIX
.HELP
     TIMSPIX is a procedure designed to process TIMS picture products
in a throughput manner.  There are seven possible outputs from 
this procedure: a calibrated, band-interleaved (BIL), halfword-format
image file, 3 D-stretched, panorama-corrected byte-format image files,
and 3 playback versions of the D-stretched files. 

     The user must submit the name (NAME parameter) that is the root of the
PIX and AUX files.  That is, if the starting datasets are MONO3.PIX and 
MONO3.AUX, the NAME parameter must be "MONO3". If the user

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

    The user may also specify the strip length used to create the playback
image. If no stripping is desired, the strip length should be set to the
number of scanlines in the flightline. The default is set at 3500 lines
per strip.
.PAGE
Cognizant Programmer: Vince Realmuto                   9 OCT 1990

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
.VARIABLE ASPECT
aspect (H/V) ratio 
used in the panorama
correction
.VARIABLE LENGTH
length of the strip
used in playback
.VARIABLE DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.VARIABLE FLIP
Flip the image left-to-right?
Valid: FLIP, NOFLIP
.LEVEL2
.VARIABLE NAME
The user must provide the base name for the datasets used. For example,
to process "HAWAII32.PIX" and "HAWAII32.AUX" the name "HAWAII32" must be
given.  This procedure will then generate output datasets with the names
"HAWAII32.IRAD", "HAWAII32.DS1", "HAWAII32.DS3", "HAWAII32.DS5", 
"HAWAII32.RED", "HAWAII32.GRN", and/or "HAWAII32.BLU" as needed.
.VARIABLE CALMODE
User's choice of a calibration option. The default
is to calibrate the data. When this option is in effect,
six band-sequential(BSQ) halfword-format files are
witten to disk. 
.VARIABLE FITAREA
Optional specification of the subarea of the image that will
be used to calculate statistics for the fitting of halfword
data to byte data. The byte imagery thus created is input
to the D-stretch algorithm. The specification of this area 
is only required when the calibrated (CAL) mode is selected.
The default in CAL mode is to use the entire image to calculate
the statistics. 
.VARIABLE ASPECT
Aspect (or horizontal to vertical) ratio used in the 
panorama correction (C130RECT). The default is 1.0,
which implies that the two dimensions are equal.
With ASPECT=1, the corrected TIMS image will have 
753 samples (or pixels) per line.
.VARIABLE DSTAREA
Optional subareas for the training of the D-STRETCH.
If subareas are specified, the data from these areas 
are used to calculate the correlation matrix. The default
is to use the entire image to calculate the correlation 
matrix.
.VARIABLE LENGTH
Optional length for the stripping of the playback image. 
If no stripping is desired, the user should enter the number 
lines in the image for LENGTH. The default is to use a strip
length of 3500 lines to create the playback image.
.VARIABLE DATE
TIMSCAL uses the date of data acquisition to determine the proper
calibration coefficients.  If defaulted, the date in the VICAR 
label is used. This parameter is needed only if the VICAR label
is incorrect, or if an abnormal calibration set is to be used.
.VARIABLE FLIP
Specifying the 'FLIP option will cause a left-to-right mirror
image to be generated.  This is necessary to get the proper
viewing geometry for TIMS data acquired in the Cessna aircraft.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create timspix2.imake
#define  PROCEDURE timspix2

#define R3LIB 
$ Return
$!#############################################################################
