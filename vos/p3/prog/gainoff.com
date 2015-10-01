$!****************************************************************************
$!
$! Build proc for MIPL module gainoff
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:13:12
$!
$! Execute by entering:		$ @gainoff
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module gainoff ***"
$!
$ Create_Source = ""
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
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to gainoff.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("gainoff.imake") .nes. ""
$   then
$      vimake gainoff
$      purge gainoff.bld
$   else
$      if F$SEARCH("gainoff.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gainoff
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gainoff.bld "STD"
$   else
$      @gainoff.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gainoff.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gainoff.com -mixed -
	-s gainoff.f -
	-p gainoff.pdf -
	-i gainoff.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gainoff.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL GAIN(500),OFFSET(500),BUF(10000)
	REAL ZMIN(4)/  0.0, -32768.0, 0.0, -1.5E38/
	REAL ZMAX(4)/255.0,  32767.0, 0.0,  1.5E38/
	CHARACTER*40 FILNAM(2)
	CHARACTER*8 FORMAT
C						open input, get size and org
	CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBI)
	CALL XVGET(INUNIT1,ISTAT,'PIX_SIZE',NBPP,'FORMAT',FORMAT,' ')
C			       					open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		  'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','REAL','U_ORG','BIL','O_FORMAT',FORMAT,' ')
C							get remaining parameters
	CALL XVPARM('INGAIN',GAININ,ICNT,IDEF,0)
	CALL XVPARM('OUTGAIN',GAINOUT,ICNT,IDEF,0)
	CALL XVPARM('INP',FILNAM,ICNT,IDEF,0)
C							read in gains & offsets
	CALL READDS(FILNAM(2),GAIN,OFFSET,NBX)
	IF (NB.NE.NBX) THEN
	    CALL XVMESSAGE(
     +      ' Number of bands in image does not match gain/offset file',
     +      ' ')
	    CALL ABEND
	ENDIF
C
	DO I=1,NB
	    GAIN(I) = GAININ*GAINOUT*GAIN(I)
	    OFFSET(I) = GAINOUT*OFFSET(I)
	    IF (FORMAT .NE. 'REAL') OFFSET(I)=OFFSET(I)+0.5
	END DO
C
	IEB = ISB+NB-1
	IEL = ISL+NL-1
	XLO = ZMIN(NBPP)
	XHI = ZMAX(NBPP)
	DO LINE=ISL,IEL
	    DO IBAND=ISB,IEB
		CALL XVREAD(INUNIT1,BUF,ISTAT,'LINE',LINE,'BAND',IBAND,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO N=1,NS
		    BUF(N) = GAIN(IBAND)*BUF(N) + OFFSET(IBAND)
		    BUF(N) = MIN(MAX(BUF(N),XLO),XHI)
		END DO
		CALL XVWRIT(IOUTUNIT,BUF,ISTAT,' ')
	    END DO
	END DO
	RETURN
	END
C**************************************************************************
      SUBROUTINE READDS(FILENAME,GAIN,OFFSET,NRECS)
C
C      This subroutine reads all records from the file FILENAME, and puts
C      the second numeric value that it finds in the GAIN array, and the
C      third numeric value in the OFFSET array. The delimiters may be space, 
C      tab, or comma. If something other than a numeric or delimiter is
C      encountered prior to finding three numeric fields, the entire line is 
C      discarded.
C
      CHARACTER*40 FILENAME
      REAL GAIN(*),OFFSET(*)
C
      OPEN (51,FILE=FILENAME,STATUS='OLD')
      NRECS = 1
  100 CONTINUE
          READ (51,*,END=900,ERR=100) X,GAIN(NRECS),OFFSET(NRECS)
	  NRECS = NRECS+1
          GO TO 100
  900 CONTINUE
      NRECS = NRECS-1
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create gainoff.pdf
process help=*
  PARM INP  	(STRING,40)		COUNT=2
  PARM OUT  	(STRING,40)
  PARM SIZE 	INTEGER DEFAULT=--	COUNT=0:4 
  PARM BANDS 	INTEGER DEFAULT=--	COUNT=0:2 
  PARM SL 	INTEGER DEFAULT=1
  PARM SS 	INTEGER DEFAULT=1
  PARM SB 	INTEGER DEFAULT=1
  PARM NL 	INTEGER DEFAULT=0
  PARM NS 	INTEGER DEFAULT=0
  PARM NB 	INTEGER DEFAULT=0
  PARM INGAIN   REAL	DEFAULT=1.0
  PARM OUTGAIN	REAL	DEFAULT=1.0
END-PROC
.HELP
      GAINOFF is a program to apply differing gains and offsets to the various
channels of multispectral data.  It accepts as input images of BSQ or BIL 
format, and data types BYTE, HALF, FULL, or REAL.  Regardless of input format,
the output will be in BIL format.
      The gains and offsets for each channel are supplied through a second 
input dataset, which is an ASCII (non-VICAR) file.  This ASCII file should be a
table of numbers at least 3 columns wide, and have one row of numbers for each
band to be processed. The gain values are taken from column 2 and the offset
values are taken from column 3.  All other columns and any headers or annotation
in the file is ignored. Row 1 of the file provides the gain and offset for the
first band processed, Row 2 provides for the second band and so on.
      In addition, the parameters INGAIN and OUTGAIN may be used to adjust the 
values of all pixels, regardless of band.  INGAIN is multiplied to all input
pixels prior to the spectral gain, and may be used to compensate for mismatched
units.  OUTGAIN is multiplied to all pixels just prior to output, and may be
used to rescale the data over the desired output range.
.LEVEL1
.VARIABLE INP
Input image, gain/offset file
.VARIABLE OUT
Output image file name
.VARIABLE SIZE
Window into input
.VARIABLE BANDS
Window into input
in band dimension
.VARIABLE SL
Starting line
= size(1)
.VARIABLE SS
Starting sample
= size(2)
.VARIABLE SB
Starting band
= bands(1)
.VARIABLE NL
Number of lines
= size(3)
.VARIABLE NS
Number of samples
= size(4)
.VARIABLE NB
Number of bands
= bands(2)
.VARIABLE INGAIN
Gain applied prior to
spectral gains/offsets
.VARIABLE OUTGAIN
Gain applied after
spectral gains/offsets
.LEVEL2
.VARIABLE INP
The first file is the image file, in VICAR format. It may be either in BSQ or
BIL organization, and BYTE, HALF, FULL, or REAL format.

The second file must be an ASCII file of three columns of numbers. The second
column will be used as the spectral gain, and the third column used as the
spectral offset. The first column is ignored. The ASCII file may contain headers
and annotation, and must have the same number of rows of numbers as the number
of bands to be processed.
.VARIABLE OUT
Name of the output image file. The output file will be in BIL format, 
regardless of the format of the input image.
.VARIABLE SIZE
The size parameter determines the boundaries in the input
file from which the computation is to take place.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.VARIABLE BANDS
The bands parameter determines the bands in the input
file from which the computation is to take place.  It is specified
as (SB,NB), where
	SB is the starting band
	NB is the number of bands to be copied
.VARIABLE INGAIN
INGAIN is a value multiplied to all input pixels prior to the application of
individual gains.  It may be used to get the units of the image to match the
units of the gains.
.VARIABLE OUTGAIN
OUTGAIN is a value multiplied to all pixels immediately prior to output. It
may be used to rescale the data into a reasonable range for byte or halfword 
output.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create gainoff.imake
#define  PROGRAM   gainoff

#define MODULE_LIST gainoff.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
