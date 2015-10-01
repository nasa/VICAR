$!****************************************************************************
$!
$! Build proc for MIPL module stretch2b
$! VPACK Version 1.8, Wednesday, March 25, 1998, 17:10:54
$!
$! Execute by entering:		$ @stretch2b
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
$ write sys$output "*** module stretch2b ***"
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
$ write sys$output "Invalid argument given to stretch2b.com file -- ", primary
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
$   if F$SEARCH("stretch2b.imake") .nes. ""
$   then
$      vimake stretch2b
$      purge stretch2b.bld
$   else
$      if F$SEARCH("stretch2b.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stretch2b
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stretch2b.bld "STD"
$   else
$      @stretch2b.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretch2b.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretch2b.com -
	-p stretch2b.pdf -
	-i stretch2b.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create stretch2b.pdf
PROCEDURE HELP=*
PARM INP TYPE=(STRING,80)
PARM OUT TYPE=(STRING,80) COUNT=(1:200)
!
PARM SIZE    TYPE=INTEGER   DEFAULT=--       COUNT=(0:4)
PARM SL      TYPE=INTEGER   DEFAULT=--       COUNT=(0:1)
PARM SS      TYPE=INTEGER   DEFAULT=--       COUNT=(0:1)
PARM NL      TYPE=INTEGER   DEFAULT=--       COUNT=(0:1)
PARM NS      TYPE=INTEGER   DEFAULT=--       COUNT=(0:1)
!
PARM DOALL      TYPE=KEYWORD  COUNT=(0:1)    VALID=ALL      DEFAULT=--
PARM BANDS      TYPE=INTEGER  COUNT=(1:200)  DEFAULT=1
!
PARM MEAN       TYPE=REAL     DEFAULT=127.5
PARM SIGMA      TYPE=REAL     DEFAULT=2.5
!
LOCAL PARSTRING TYPE=STRING   INITIAL=" "
LOCAL OUTFILE   TYPE=STRING
LOCAL COUNTER   TYPE=INTEGER
LOCAL BAND      TYPE=INTEGER
LOCAL LMEAN     TYPE=REAL
LOCAL LSIGMA    TYPE=REAL
LOCAL LOW       TYPE=REAL
LOCAL HIGH      TYPE=REAL
LOCAL LOW2      TYPE=REAL
LOCAL HIGH2     TYPE=REAL
LOCAL NOUT      TYPE=INTEGER
LOCAL NBANDS    TYPE=INTEGER
LOCAL NPAR      TYPE=INTEGER
!
BODY
LET NPAR = $COUNT(SIZE)
IF (NPAR=4) LET PARSTRING = "SIZE=&&SIZE"
LET NPAR = $COUNT(SL)
IF (NPAR=1) LET PARSTRING = PARSTRING // " SL=&&SL"
LET NPAR = $COUNT(SS)
IF (NPAR=1) LET PARSTRING = PARSTRING // " SS=&&SS"
LET NPAR = $COUNT(NL)
IF (NPAR=1) LET PARSTRING = PARSTRING // " NL=&&NL"
LET NPAR = $COUNT(NS)
IF (NPAR=1) LET PARSTRING = PARSTRING // " NS=&&NS"
!
LET NPAR = $COUNT(DOALL)
IF (NPAR=0)
   LET NOUT = $COUNT(OUT)
   LET NBANDS = $COUNT(BANDS)
   IF (NOUT=NBANDS)
      LET COUNTER = 1
      LOOP
         LET BAND=BANDS(COUNTER)
         LET OUTFILE=OUT(COUNTER)
         COPY &INP SCRstretch2b &PARSTRING SB=&BAND NB=1
         HIST SCRstretch2b 'NOHIST MEAN=LMEAN SIGMA=LSIGMA
         LET LOW =  LMEAN - &SIGMA*LSIGMA
         LET HIGH = LMEAN + &SIGMA*LSIGMA
         LET LOW2 = &MEAN - 127.5
         LET HIGH2 = &MEAN + 127.5
         C SCRstretch2b &OUTFILE 'BYTE IRANGE=(&LOW,&HIGH) ORANGE=(&LOW2,&HIGH2)
         LET COUNTER = COUNTER + 1
         IF (COUNTER > $COUNT(OUT)) BREAK
      END-LOOP
      FILE_DELETE SCRstretch2b
   ELSE
      WRITE "The number of output files must equal the number of BANDS listed"
   END-IF
ELSE
   FORM &INP NB=NBANDS
   LET BAND = 1
   LOOP
      LET OUTFILE=OUT(1) // "." // "&BAND"
      COPY &INP SCRstretch2b &PARSTRING SB=&BAND NB=1
      HIST SCRstretch2b 'NOHIST MEAN=LMEAN SIGMA=LSIGMA
      LET LOW =  LMEAN - &SIGMA*LSIGMA
      LET HIGH = LMEAN + &SIGMA*LSIGMA
      LET LOW2 = &MEAN - 127.5
      LET HIGH2 = &MEAN + 127.5
      C SCRstretch2b &OUTFILE 'BYTE IRANGE=(&LOW,&HIGH) ORANGE=(&LOW2,&HIGH2)
      LET BAND = BAND + 1
      IF (BAND > NBANDS) BREAK
   END-LOOP
   FILE_DELETE SCRstretch2b
END-IF
!
END-PROC
.TITLE
STRETCH2B
.HELP
 PURPOSE:
Procedure STRETCH2B (STRetch TO BYTE) is a VICAR procedure that converts an
image (either single or multichannel) to byte, by means of an automatic
contrast stretch, based upon the input mean and standard deviation. The input
may be of any format excluding COMPLEX (but including BYTE itself), and 
contain any number of channels.  For multichannel inputs, the user may list
the desired band(s) to process, or request that all bands be processed,
without enumeration.
.PAGE
 EXECUTION:
     STRETCH2B uses the VICAR programs COPY, HIST, and C to extract the
requested band and subarea, compute the input mean and standard deviation,
the rescale to byte by forcing the input mean to map to the value or the
MEAN parameter (default = 127.5), and forcing the range of SIGMA (default = 2.5)
standard deviations to map to the 255 DN range about the MEAN.
     If the input is multichannel, each requested channel is processed
one at a time, and each channel is output to a different file.
 EXAMPLES:
     STRETCH2B yuma.half yuma.byte

         In this example, if the input contains one band, that band is
         stretched, using the defaults, then output to yuma.byte.  If the
         input is multichannel, the first input band is stretched.

     STRETCH2B dv12 (dv12.r,dv12.g,dv12.b) bands=(5,3,1) sigma=5

         In this example, Band 5 is stretched to dv12.r
                          Band 3 is stretched to dv12.g and
                          Band 1 is stretched to dv12.b
         with 0 to 255 spanning 5 standard deviations from the mean.

     STRETCH2B sebass sebass 'all

         In this example, all bands of the input are stretched. The outputs
         will have the names sebass.1, sebass.2, sebass.3, ...
.PAGE
 WRITTEN BY:             Ron Alley                   18 Feb 1998

 COGNIZANT PROGRAMMER:   Ron Alley                   18 Feb 1998

 REVISION:               New                         18 Feb 1988
.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name(s).  Use
one name for the root output
name if keyword ALL is used;
otherwise one filename for 
each band to be stretched.
.VARIABLE SIZE
Vicar size field for input:
Same as  (SL,SS,NL,NS)
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line of input
.VARIABLE SS
Starting sample of input
.VARIABLE NL
Number of lines from input
.VARIABLE NS
Number of samples from input
.VARIABLE DOALL
Use KEYWORD "ALL" if all
channels are desired.
.VARIABLE BANDS
The band(s) to be stretched.
The number of bands must match
the number of output files.
.VARIABLE MEAN
The desired mean of each output
.VARIABLE SIGMA
The desired number of standard
deviations from mean in output
data range.
.LEVEL2
.VARIABLE INP
Input file name. The input file may be of any image data format, except 
COMPLEX. It may be a single channel or multichannel.
.VARIABLE OUT
If the keyword ALL is not specified (this is the default case), this is a
list of the output file names, one for each band to be stretched. There should 
be one output file for each value listed for the BANDS parameter. Each output
file will contain one channel of data in BYTE format, suitably stretched.
     If the keyword ALL has been specified, OUT should contain just one value,
the root name used for all output filenames.  In this case, the channel 
number is appended to the name given by OUT, to produce each output file name.
For example, if OUT=cuprite9 and the input contains 6 bands, the following six
output files are produced:
             cuprite9.1      cuprite9.2         cuprite9.3
             cuprite9.4      cuprite9.5         cuprite9.6
.VARIABLE SIZE
Vicar size field for input:
Same as  (SL,SS,NL,NS)
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line of input
.VARIABLE SS
Starting sample of input
.VARIABLE NL
Number of lines from input
.VARIABLE NS
Number of samples from input
.VARIABLE DOALL
Under default conditions, if the input file is multichannel, the user must
list the bands to be processed, using the BANDS parameter, and there must be
one output file listed for each band to be processed.
     If the user wishes to process all bands, (s)he may specify the 'ALL
keyword.  Under these conditions, only one value of OUT is needed, and the 
band numbers need not be listed.  Each band is processed, with the output
going into a file called by the name given by "OUT", plus the band number as
an extension to the file name.  For example, if OUT=dv and the input has
3 bands, the files dv.1, dv.2, and dv.3 are produced.
.VARIABLE BANDS
The band(s) to be stretched. If the input is a single channel dataset, this
parameter is defaulted. If the input is a multichannel file, the number of
bands listed by this parameter must match the number of output files. If the
ALL keyword is used, all bands are processed, and this parameter is ignored.
.VARIABLE MEAN
The desired mean of each output. The default is 127.5.  If a value other
than 127.5 is used, the number of standard deviations to the high and low
saturation points (0 and 255) will not be equal.  Caution should be used
when specifying a MEAN value not close to 127.5.
.VARIABLE SIGMA
SIGMA is the desired number of standard deviations from the mean to be used 
to span the BYTE data range of 0 to 255.  For example, if SIGMA=2.0, then
a pixel two standard deviations below the mean will be mapped to 0, and a
pixel two standard deviations above the mean will be mapped to 255. All pixel
values within this range are scaled linearly between these two endpoints. 
.END
$ Return
$!#############################################################################
$Imake_File:
$ create stretch2b.imake
#define  PROCEDURE stretch2b

#define R3LIB 
$ Return
$!#############################################################################
