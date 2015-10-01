$!****************************************************************************
$!
$! Build proc for MIPL module timscal
$! VPACK Version 1.8, Thursday, October 05, 2000, 16:50:00
$!
$! Execute by entering:		$ @timscal
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
$ write sys$output "*** module timscal ***"
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
$ write sys$output "Invalid argument given to timscal.com file -- ", primary
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
$   if F$SEARCH("timscal.imake") .nes. ""
$   then
$      vimake timscal
$      purge timscal.bld
$   else
$      if F$SEARCH("timscal.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timscal
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timscal.bld "STD"
$   else
$      @timscal.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timscal.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timscal.com -
	-s timscal.f -
	-p timscal.pdf -
	-i timscal.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timscal.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	COMMON /IOPRM/ SL,SS,NLO,NSO,EL,ES,NS
	COMMON /LUTS/ RAD_LUT,TEMP_LUT
C
	REAL BBEMIS(6)
	INTEGER SL,SS,EL,ES,PIX,OUT
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	INTEGER*2 BUF(638)/638*0/
	CHARACTER*100 LABEL
	LOGICAL XVPTST,QTEM
C
C		Open input image data set
C
	CALL XVUNIT(PIX,'INP',1,ISTAT,' ')
	CALL XVOPEN(PIX,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		    'U_FORMAT','HALF',' ')
C
C		Parameter processing
C
	CALL XVSIZE(SL,SS,NLO,NS,NLI,NSI)
	CALL XVPARM('FILTER',NWTS,NUM,IDEF,1)
	CALL XVPARM('SHIFT',SHIFT,NUM,IDEF,1)
	SHIFT = SHIFT / 1000.0
	CALL XVPARM('INST_T',TEMP_INST,NUM,IDEF,1)
	TEMP_INST = TEMP_INST + 273.15
C						convert shift parameter from
C						nanometers to micrometers.
C
C		Check to see if the date is already in the label
C
	CALL XLGET(PIX,'HISTORY','INFO1',LABEL,ISTAT,
     &		   'HIST','TIMSLOG','FORMAT','STRING',' ')
	IF (ISTAT.LT.0) CALL XLGET(PIX,'HISTORY','INFO1',LABEL,ISTAT,
     &			'HIST','VTIMSLOG','FORMAT','STRING',' ')
	IF (ISTAT.LT.0) CALL XLGET(PIX,'HISTORY','LAB1',LABEL,ISTAT,
     &			'HIST','VTIMSLOG','FORMAT','STRING',' ')
	IDATE = -1
	IF (LABEL(6:6).EQ.'D') THEN
	    READ (LABEL,80,err=95) MONTH,IDAY,IYEAR
   80	    FORMAT (14X,I2,1X,I2,1X,I2)
	ELSE
	    READ (LABEL,90,err=95) MONTH,IDAY,IYEAR
   90	    FORMAT (17X,I2,1X,I2,1X,I2)
	END IF
	IDATE = 10000*IYEAR+100*MONTH+IDAY
   95	CONTINUE
C
C		Get and process DATE parameter
C
	CALL XVPARM('DATE',JDATE,NUM,IDEF,1)
	IF (JDATE.GT.800000) IDATE=JDATE
	IF (IDATE.LT.0) THEN
	    CALL XVMESSAGE(' Unable to read date in VICAR label.',' ')
	    CALL XVMESSAGE(' Please specify the date as a parameter.'
     +				,' ')
	    CALL ABEND
	END IF

C
C		Set the BB Emissivities, depending on the date
C
	IF (IDATE .GT. 970513) THEN
	    BBEMIS(1) = 0.9481
	    BBEMIS(2) = 0.9359
	    BBEMIS(3) = 0.9379
	    BBEMIS(4) = 0.9383
	    BBEMIS(5) = 0.9409
	    BBEMIS(6) = 0.9419
	ELSE
	    DO I=1,6
		BBEMIS(I) = 1.0
	    END DO
	END IF
C
C		Set some pointers
C
	IF(NS.GT.638) NS=638
	EL=SL+NLO-1
	ES=SS+NS-1
	IF(EL.GT.NLI.OR.ES.GT.NSI) THEN
	    CALL XVMESSAGE(
     &             ' Size field specified overruns file dimensions',' ')
	    CALL ABEND
	ENDIF
C
C
C		Open output data set
C
	CALL XVUNIT(OUT,'OUT',1,ISTAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(OUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','WRITE','U_ORG','BSQ','U_NL',NLO,'U_NS',NS,
     &		   'U_FORMAT','HALF','O_FORMAT','HALF',' ')
	    DO I=1,6
		DO J=1,NLO
		    CALL XVWRIT(OUT,BUF,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(OUT,ISTAT,' ')
	    CALL XVOPEN(OUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','UPDATE',' ')
	ELSE
	    CALL XVOPEN(OUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &			'U_NL',NLO,'U_NS',NS,'U_ORG','BIL','OP','WRITE',
     &			'U_FORMAT','HALF','O_FORMAT','HALF',' ')
	END IF
C
	QTEM = XVPTST('ITEM')
	IF(QTEM) THEN							! ITEM
	    CALL XLADD(OUT,'HISTORY','LBL1',
     &		'Instrument Perceived Temperature Image',STAT,
     &		'FORMAT','STRING',' ')
	    CALL XLADD(OUT,'HISTORY','LBL2',
     &		'DN = 100*Degrees Celsius',
     &		STAT,'FORMAT','STRING',' ')
	ELSE								! IRAD
	    CALL XLADD(OUT,'HISTORY','LBL1',
     &		'Instrument Perceived Radiance Image',STAT,
     &		'FORMAT','STRING',' ')
	    CALL XLADD(OUT,'HISTORY','LBL2',
     &		    'DN = milliwatts/m*m*sr*micrometer',
     &		    STAT,'FORMAT','STRING',' ')
	END IF
C
	CALL READCAL(NWTS,SL,NLO)
	CALL GET_TIMS_RAD_LUT(IDATE,SHIFT,RAD_LUT)
	IF (QTEM) CALL GET_TIMS_TEMP_LUT(IDATE,SHIFT,TEMP_LUT)
	CALL CALIBRATE(PIX,OUT,QTEM,BBEMIS,TEMP_INST)
C
C		Close all datasets
C
	CALL XVCLOSE(PIX,STAT,' ')
	CALL XVCLOSE(OUT,STAT,' ')
C
	RETURN
	END
C***********************************************************************
	SUBROUTINE READCAL(NWTS,ISL,NL)
C
	COMMON /BBDAT/ X(30000,15)
	REAL BUF(14)
	CHARACTER*80 PRT

C							Open the AUX dataset
	CALL XVUNIT(IUNIT,'INP',2,ISTAT,' ')
	CALL XVOPEN(IUNIT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	LINE = ISL
C						    Read each line of cal data,
	DO I=1,NL
	    CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'NSAMPS',14,' ')
	    LINE = LINE+1
	    DO J=1,14
		X(I,J+1) = BUF(J)
	    END DO
	END DO
C						    Check for reasonable values
	NBAD = 0
	IF (X(1,2).LT.-25.0 .OR. X(1,2).GT.65.0) THEN
	    X(1,2) = X(2,2)
	    NBAD = NBAD + 1
	END IF
	IF (X(1,3).LT.-25.0 .OR. X(1,3).GT.65.0) THEN
	    X(1,3) = X(2,3)
	    NBAD = NBAD + 1
	END IF
	DO I=2,NL
	    IF (X(I,2).LT.-25.0 .OR. X(I,2).GT.65.0) THEN
		X(I,2) = X(I-1,2)
		NBAD = NBAD + 1
	    END IF
	    IF (X(I,3).LT.-25.0 .OR. X(I,3).GT.65.0) THEN
		X(I,3) = X(I-1,3)
		NBAD = NBAD + 1
	    END IF
	END DO
	DO J=4,15
	    IF (X(1,J).LT.0.0 .OR. X(1,J).GT.255.0) THEN
		X(1,J) = X(2,J)
		NBAD = NBAD + 1
	    END IF
	    DO I=2,NL
		IF (X(I,J).LT.0.0 .OR. X(I,J).GT.255.0) THEN
		    X(I,J) = X(I-1,J)
		    NBAD = NBAD + 1
		END IF
	    END DO
	END DO
	IF (NBAD .NE. 0) THEN
	    WRITE (PRT,100) NBAD
  100	    FORMAT('***WARNING:',I6,' aux file values have been ',
     +		   'rejected and reset.')
	    CALL XVMESSAGE(PRT,' ')
	END IF
C							Filter all arrays
	DO J=1,14
	    CALL UNIFLT(7,NL,X(1,J+1),X(1,J),NWTS)
	END DO
	CALL XVCLOSE(IUNIT,ISTAT,' ')
	RETURN
	END
C*******************************************************************************
	SUBROUTINE CALIBRATE(PIX,OUT,QTEM,BBEMIS,TEMP_INST)
C
	COMMON /BBDAT/BBLO(30000),BBHI(30000),BBDN(30000,2,6),DMY(30000)
	COMMON /RAWFIL/ FILTER,WVLEN,ICALDATE
	COMMON /LUTS/ RAD_LUT,TEMP_LUT
	COMMON /IOPRM/ SL,SS,NLO,NSO,EL,ES,NS
	COMMON /IOBLK/ OUTBUF,INBUF
C
	REAL FILTER(158,6),WVLEN(158),DNLO_LAST(6),DNHI_LAST(6)
	REAL SLOPE(6),YINT(6),RADLO(6),RADHI(6),BBEMIS(6),REFLRAD(6)
	INTEGER SL,SS,EL,ES,PIX,OUT
	INTEGER*2 OUTBUF(3828),INBUF(3828)
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	LOGICAL QRECALC,QTEM
C
C					 compute radiance reflected off the BB's
	DO ICHAN=1,6
	    INDEX = 100.0*TEMP_INST
	    X = 100.0*TEMP_INST - INDEX
	    REFLRAD(ICHAN) = (1.0 - BBEMIS(ICHAN)) *
     +				((1.0-X)*RAD_LUT(INDEX,ICHAN) + 
     +				     X*RAD_LUT(INDEX+1,ICHAN))
	END DO
C								      initialize
	IAUX = 0
	BBTLO = 0.0
	BBTHI = 0.0
	DO ICHAN=1,6
	    DNLO_LAST(ICHAN) = -1.0
	    DNHI_LAST(ICHAN) = -1.0
	END DO
C
	DO ILINE=SL,EL
	    IAUX = IAUX + 1
C					   store previous line's BB temperatures
	    BBTLO_LAST = BBTLO
	    BBTHI_LAST = BBTHI
C						 get this line's BB temperatures
	    BBTLO = BBLO(IAUX)+273.15
	    BBTHI = BBHI(IAUX)+273.15
C
	    QRECALC = (BBTLO.NE.BBTLO_LAST) .OR. (BBTHI.NE.BBTHI_LAST)
C
	    DO ICHAN=1,6
		DNLO  = BBDN(IAUX,1,ICHAN)
		DNHI  = BBDN(IAUX,2,ICHAN)
C						if BB temps have changed, we
C					        need to recalculate BB rads
		IF (QRECALC) THEN
		    INDEX = 100.0*BBTLO
		    X = 100.0*BBTLO - INDEX
		    RADLO(ICHAN) = (1.0-X)*RAD_LUT(INDEX,ICHAN) +
     +					 X*RAD_LUT(INDEX+1,ICHAN)
		    INDEX = 100.0*BBTHI
		    X = 100.0*BBTHI - INDEX
		    RADHI(ICHAN) = (1.0-X)*RAD_LUT(INDEX,ICHAN) +
     +					 X*RAD_LUT(INDEX+1,ICHAN)
C						correct for the non-black nature
C						of the blackbody paint
		    RADLO(ICHAN) = BBEMIS(ICHAN)*RADLO(ICHAN) +
     +				       REFLRAD(ICHAN)
		    RADHI(ICHAN) = BBEMIS(ICHAN)*RADHI(ICHAN) +
     +				       REFLRAD(ICHAN)
		END IF
C					   if BB temps or DN's have changed,
C					   we need to recalculate SLOPE and YINT
		IF (QRECALC .OR. (DNLO .NE. DNLO_LAST(ICHAN)) .OR.
     +				(DNHI .NE. DNHI_LAST(ICHAN))) THEN
		    SLOPE(ICHAN) = (RADHI(ICHAN)-RADLO(ICHAN)) /
     +					(DNHI-DNLO)
		    YINT(ICHAN)  = RADLO(ICHAN)-SLOPE(ICHAN)*DNLO + 0.5
		END IF
C							read & write a line
		CALL XVREAD(PIX,INBUF,STAT,'LINE',ILINE,'SAMP',SS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    RAD = SLOPE(ICHAN)*INBUF(ISAMP) + YINT(ICHAN)
		    OUTBUF(ISAMP) = MIN(32767.0,MAX(0.0,RAD))
		END DO
C						if necessary, convert to temps
		IF (QTEM) THEN
		    DO ISAMP = 1,NS
			IF (OUTBUF(ISAMP) .GT. 0) THEN
			    OUTBUF(ISAMP)=TEMP_LUT(OUTBUF(ISAMP),ICHAN)
			ELSE
			    OUTBUF(ISAMP) = -32768
			END IF
		    END DO
		END IF
C
		CALL XVWRIT(OUT,OUTBUF,ISTAT,'LINE',IAUX,'BAND',ICHAN,
     &			    ' ')
		DNLO_LAST(ICHAN) = DNLO
		DNHI_LAST(ICHAN) = DNHI
	    END DO
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timscal.pdf
Process help=*
parm  INP      (string,40)	count=2
parm  OUT      (string,40)
parm  SIZE     integer		default=(1,1,0,0)	count=4
parm  SL       integer		default=1
parm  SS       integer		default=1
parm  NL       integer		default=0
parm  NS       integer		default=0
parm  CALMODE  keyword		valid=(IRAD,ITEM)	default=IRAD
parm  FILTER   integer 		default=75
parm  DATE     integer 		default=-1
!parm  BBEMIS   real count=6 default=(0.9481,0.9359,0.9379,0.9383,0.9409,0.9419)
parm  INST_T   real    		default=0.0
parm  ORG      keyword 		valid=(BIL,BSQ)  default=BIL
parm  SHIFT    real             default=0.0
End-proc

.TITLE
TAE PROCESS TIMSCAL
.HELP
PURPOSE:

   TIMSCAL is a program which calibrates TIMS data which have already been 
logged with the program TIMSLOG.  It can output two types of data, instrument 
perceived radiance (IRAD), or instrument perceived temperature (ITEM). The
output is a data set of all six bands in two byte integer.

EXECUTION:

   The following is the execution statement format for TIMSCAL:

	TIMSCAL INP=(PIX,AUX) OUT=CALPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 



OPERATION:

   TIMSCAL takes two input data sets - the first is a TIMS image file and the 
second an auxiliary file for calibration, both of which are output from TAE 
program TIMSLOG.  Based upon the auxiliary file, it calibrates a TIMS image 
(6 bands), according to the type of data specified with keyword CALMODE.

   The units output for radiance images are  milliwatts/m**2/um/sr. The units 
output for temperature images are in hundredths of degrees Celsius. Output 
is in halfword (two byte signed integer) format. 


WRITTEN BY:  J. H. REIMER    1982

COGNIZANT PROGRAMMER:   Ron Alley
			(Logic changed to structured by Rich Walker)
			(Logic reworked by Ron Alley Spring 1986)
			(Atmospheric corrections implemented by Rich Walker
				and Ron Alley, Fall 1986)
			(Additional changes made Sept.,1988 - Ron Alley)
                        (LUTs added, central wavelengths removed - Ron Alley)
                        (Logic added for non-black blackbody paint - Ron Alley)
REVISION: 4; August, 1997

.LEVEL1
.VARI INP
TIMS image file,
auxiliary calibration file.
.VARI OUT
Output data set of all six bands
of calibrated TIMS data.
.VARI SIZE
The standard VICAR output size
field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI CALMODE
This keyword selects the type
of decalibatrion to be performed
Valid: IRAD, ITEM
.VARI FILTER
Filter width for calibration
data smoothing.
.VARI DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.VARI  BBEMIS
The emissivities for the 
blackbody paint, in each of
the six channels
.VARI INST_T
Instrument temperature
(degrees Celsius)
.VARI ORG
Organization of output dataset
.VARI SHIFT
Spectral shift of filter 
functions (Nanometers)
.LEVEL2
.VARI INP
(1)	The file containing all six bands of a TIMS image in MSS format,
	which has been output from TIMSLOG.
		Example: INP(1) = location.pix
(2)	The file containing the auxiliary calibration data set, the second
	output for a given flight line from TIMSLOG.
		Example: INP(2) = location.aux
.VARI OUT
	Output data set in INTEGER*2 (HALFWORD) format of all six bands
	of calibrated TIMS data.  The exact type of data output (e.g. tem-
	perature, radiance) is determined by the MODE parameter.
		Example: OUT = location.irad
.VARI SIZE
	The standard VICAR2 output size field.   Default will calibrate
	the entire data set.
		Example: SIZE = (1,1,200,638)
.VARI SL
	Starting line (same as SIZE(1)).
.VARI SS
	Starting sample (same as SIZE(2)).
.VARI NL
	Number of lines (same as SIZE(3)).
.VARI NS
	Number of samples (same as SIZE(4)).
.VARI CALMODE
	This keyword selects the type of decalibatrion to be performed.
	There is a choice of two possibilities:
		IRAD - (Instrument perceived RADiance)
			Radiance values of scene with no atmospheric
			corrections.
		ITEM - (Instrument perceived TEMperature)
			Temperature values of the radiance observed
			entering the instrument, without atmospheric
			corrections.
.VARI FILTER
	This selects the width of the filter used to smooth the calibration
	file data.  Its value must be an odd integer.
.VARI DATE
	TIMSCAL uses the date of data acquisition to determine the proper
	calibration coefficients.  If defaulted, the date in the VICAR 
	label is used. This parameter is needed only if the VICAR label
	is incorrect, or if an abnormal calibration set is to be used.
.VARI INST_T
INST_T is the temperature of the portion of the TIMS instrument that is in
field of view of the blackbodies, in degrees Celsius.  This value is used to
compensate for the deviations from unity in the emissivity of the paint on the
blackbodies.  The default value of 0.0 degrees Celsius is nearly always 
acceptible.
.VARI ORG
ORG specifies the organizational format of the output dataset. BIL (the default)
and BSQ are supported.  
.VARI SHIFT
   This parameter is used to adjust the wavelength calibration of the TIMS
   filter functions.  This value (in nanometers) is added to the wavelengths
   in the table of spectral response functions.
$ Return
$!#############################################################################
$Imake_File:
$ create timscal.imake
#define  PROGRAM   timscal

#define MODULE_LIST timscal.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
