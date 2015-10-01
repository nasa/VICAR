$!****************************************************************************
$!
$! Build proc for MIPL module fit3d
$! VPACK Version 1.5, Wednesday, March 31, 1993, 15:36:07
$!
$! Execute by entering:		$ @fit3d
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
$ write sys$output "*** module fit3d ***"
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
$   if F$SEARCH("fit3d.imake") .nes. ""
$   then
$      vimake fit3d
$      purge fit3d.bld
$   else
$      if F$SEARCH("fit3d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fit3d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fit3d.bld "STD"
$   else
$      @fit3d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fit3d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fit3d.com -
	-p fit3d.pdf -
	-i fit3d.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create fit3d.pdf
PROCEDURE HELP=*
PARM INP     TYPE=(STRING,40)
PARM OUT     TYPE=(STRING,40) COUNT=1:32
PARM MODE    TYPE=KEYWORD                   DEFAULT=BYCHAN    VALID=(ALL,BYCHAN)
PARM STDEV   TYPE=REAL                      DEFAULT=0.0
PARM SB      TYPE=INTEGER                   DEFAULT=1
PARM INC     TYPE=INTEGER                   DEFAULT=1
PARM LINC    TYPE=INTEGER                   DEFAULT=1
PARM SINC    TYPE=INTEGER                   DEFAULT=1
PARM EXCLUDE TYPE=KEYWORD      COUNT=(0:1)  DEFAULT=--        VALID=EXCLUDE 

LOCAL HPARMS  TYPE=STRING +
   INITIAL="'NOHIST MEAN=MEAN SIGMA=SIGMA MAX=MAX MIN=MIN  "
LOCAL MEAN    TYPE=REAL
LOCAL SIGMA   TYPE=REAL
LOCAL MIN     TYPE=REAL
LOCAL MAX     TYPE=REAL
LOCAL NB      TYPE=INTEGER
LOCAL I       TYPE=INTEGER
LOCAL J       TYPE=INTEGER
LOCAL OUTFILE TYPE=STRING



BODY

IF (INC<>1)       LET HPARMS= HPARMS//" INC=&&INC"       ! Build the HIST
IF (LINC<>1)      LET HPARMS= HPARMS//" LINC=&&LINC"     ! parameter string
IF (SINC<>1)      LET HPARMS= HPARMS//" SINC=&&SINC"
IF (EXCLUDE="EXCLUDE") LET HPARMS= HPARMS//" 'EXCLUDE"

IF (MODE="ALL")                                          ! DO ALL BANDS AT ONCE
  HIST &INP &HPARMS
  write "mean = &mean"
  write "sigma = &sigma"
  write "min = &min"
  write "max = &max"
  TO2D &INP
  IF (STDEV<>0.0)                                        ! If rescaling by 
    LET MIN = MEAN - STDEV*SIGMA                         ! std dev, compute 
    LET MAX = MEAN + STDEV*SIGMA                         ! limiting values
  END-IF
  C &INP &OUT 'BYTE ORANGE=(0,255) IRANGE=(&MIN,&MAX)    ! rescale to byte
  TO3D &INP
  TO3D &OUT                                              !-------------------
ELSE                                                     ! DO BANDS SEPARATELY
  LET NB = $COUNT(OUT)
  LET I = 1
  LET J = SB                                             ! Loop over the range
  LOOP                                                   ! of bands
    IF (I>NB) BREAK
    TRAN &INP SCRFILXX BAND=&J                           ! Put band in a scratch
    HIST SCRFILXX &HPARMS                                ! dataset.
    IF (STDEV<>0.0)                                      ! If rescaling by  
      LET MIN = MEAN - STDEV*SIGMA                       ! std dev, compute
      LET MAX = MEAN + STDEV*SIGMA                       ! limiting values
    END-IF
    LET OUTFILE=OUT(I)
    C SCRFILXX &OUTFILE 'BYTE ORANGE=(0,255) IRANGE=(&MIN,&MAX)   ! do it
    LET I = I+1
    LET J = J+1
  END-LOOP
END-IF

END-PROC

.HELP
PURPOSE:
        FIT3D is a procedure to rescale multiple bands of a 3-D image into
byte format.  The input may be of half, real, integer, or byte format; and 
BIL, BSQ, or BIP organization.  If the keyword 'ALL is used, all bands are 
scaled to the same limits and the output is a single 3-D dataset.  Otherwise,
individual scaling limits are computed for each band and the output bands are
placed in separate datasets.
        By default, the rescaling is done to map the minimum and maximum input
values to 0 and 255, respectively.  Alternatively, the user may choose to make 
the rescaling limits a set number of standard deviations from the mean by 
setting the STDEV parameter to the desired value. 
.PAGE
EXAMPLES:

FIT3D IMG.HALF IMG.BYTE 'ALL 

In this example, the minimum and maximum values in the dataset IMG.HALF are used
to rescale the data output into IMG.BYTE.



FIT3D IMG.HALF (B.1,B.2,B.3,B.4,B.5,B.6) STDEV=2.5 INC=3

In this case, the rescaling limits are computed to be 2.5 standard deviations
from the mean value in each channel.  INC=3 directs HIST to sample every third
line and pixel to gather statistics.  Since there are 6 outputs, only the first
6 bands will be rescaled, if there are more than 6 bands in the input.




FIT3D IMG.HALF (B.101,B.102,B.103,B.104,B.105) SB=101 STDEV=3.0

In this example, 5 bands starting with Band 101 are rescaled individually to
3.0 standard deviation limits.
.LEVEL1
.VARIABLE INP
Input data set
.VARIABLE OUT
Output data set(s),
One dataset if one set
of scaling limits for all
bands.
One dataset for each band
if different scaling limits
for each band.
.VARIABLE MODE
Rescaling mode
ALL if all at once
BYCHAN if by individual band
.VARIABLE STDEV
Number of std devs to limits
(The default is to not use
std devs, but limit to
minimum and maximum pixel values
.VARIABLE SB
Starting band
(Not used with ALL keyword)
.VARIABLE INC
Gather stats from every
nth line and nth sample
.VARIABLE LINC
Gather stats from every
nth line.
.VARIABLE SINC
Gather stats from every
nth sample.
.VARIABLE EXCLUDE
Exclude 0 values from statistics
.LEVEL2
.VARIABLE INP
The input data set. It may be byte, half, integer, or real; it may be BIL,
BSQ, or BIP.
.VARIABLE OUT
The output data set or sets.  If the keyword ALL is used, then the output is a
single dataset rescaled into byte format, containing all the bands of the input
dataset.  If the BYCHAN mode (the default) is used, each band processed is 
placed into its own dataset.  The number of output datasets determines the 
number of bands that will be processed.  The maximum number of output datasets
is 32.
.VARIABLE MODE
Rescaling mode. Use ALL if all bands are to have a single set of scaling 
limits.  Do not use ALL if you wish each band to be scaled individually.
.VARIABLE STDEV
If specified, this parameter sets the number of standard deviations from the 
mean spanned by the output scaling limits.  If this parameter is defaulted,
the statistics are ignored, and the scaling limits are set to the minimum and
maximum pixel values.
.VARIABLE SB
This parameter sets the starting band to be processed, if not all bands are to
be used.  SB is incompatible with the ALL mode, and if both are specified SB is
ignored.
.VARIABLE INC
Statistics are gathered using only every n'th line and n'th sample of the
image. If different line and sample increments are desired, use LINC and
SINC parameters.
.VARIABLE LINC
Statistics are gathered using only every n'th line of the image. 
.VARIABLE SINC
Statistics are gathered using only every n'th sample of the image. 
.VARIABLE EXCLUDE
A pixel is excluded from use in determining the mean and standard deviation
if the EXCLUDE keyword is used.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create fit3d.imake
#define  PROCEDURE fit3d

#define R2LIB 
$ Return
$!#############################################################################
