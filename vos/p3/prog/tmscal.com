$!****************************************************************************
$!
$! Build proc for MIPL module tmscal
$! VPACK Version 1.8, Tuesday, April 10, 2001, 15:19:54
$!
$! Execute by entering:		$ @tmscal
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
$ write sys$output "*** module tmscal ***"
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
$ write sys$output "Invalid argument given to tmscal.com file -- ", primary
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
$   if F$SEARCH("tmscal.imake") .nes. ""
$   then
$      vimake tmscal
$      purge tmscal.bld
$   else
$      if F$SEARCH("tmscal.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tmscal
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tmscal.bld "STD"
$   else
$      @tmscal.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tmscal.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tmscal.com -
	-s tmscal.f -
	-p tmscal.pdf -
	-i tmscal.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tmscal.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL GAIN(10),OFFSET(30000,27)
	INTEGER*2 INBUF(766),OUTBUF(766)
	LOGICAL XVPTST,QTEMP
C
C		Open input image data set
C
	CALL XVUNIT(IPIX,'INP',1,ISTAT,' ')
	CALL XVOPEN(IPIX,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		    'U_FORMAT','HALF',' ')
C
C		Parameter processing
C
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVPARM('FILTER',NWTS,NUM,IDEF,' ')
	QTEMP = XVPTST('TEMP') .OR. XVPTST('ITEM')
C
C		Set some pointers
C
	IEL = ISL + NL - 1
	IES = ISS + NS - 1
	IF(IEL.GT.NLIN.OR.IES.GT.NSIN) THEN
	    CALL XVMESSAGE
     &		(' Size field specified overruns file dimensions',' ')
	    CALL ABEND
	ENDIF
C
C		Open output data set, add labels
C
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA','O_FORMAT',
     &		    'HALF','OP','WRITE','U_NS',NS,'U_FORMAT','HALF',
     &		    'U_NL',NL,' ')
	CALL XLADD(IOUT,'HISTORY','LBL1',
     &		   'Instrument Perceived Radiance Image',ISTAT,
     &		   'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','DN',
     &		   'microwatts/cm*cm*sr*micrometer',
     &		   ISTAT,'FORMAT','STRING',' ')
	IF (QTEMP) CALL XLADD(IOUT,'HISTORY','AND_DN',
     &		   'hundredths degrees Celsius for Channels 11 & 12',
     &		   ISTAT,'FORMAT','STRING',' ')
C
C						Generate buffers of AUX data
	CALL READCAL(NWTS,ISL,NL,GAIN,OFFSET)
C
	WRITE (6,*)
	IAUX = 0
	DO ILINE=ISL,IEL
	    IAUX = IAUX + 1
	    DO ICHAN=1,10
		CALL XVREAD(IPIX,INBUF,STAT,'LINE',ILINE,'SAMP',ISS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    X = GAIN(ICHAN)*INBUF(ISAMP) + OFFSET(IAUX,ICHAN)
		    OUTBUF(ISAMP) = MIN(32767.0, MAX(-32768.0,X)) + 0.5
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,' ')
	    END DO
	    DO ICHAN=11,12
		CALL XVREAD(IPIX,INBUF,STAT,'LINE',ILINE,'SAMP',ISS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    X = OFFSET(IAUX,ICHAN+2)*INBUF(ISAMP) +
     &			OFFSET(IAUX,ICHAN)
		    IF (QTEMP) X = 100.0*(PLKINV(11.0,X/1000.0)-273.15)
		    OUTBUF(ISAMP) = MIN(32767.0, MAX(-32768.0,X)) + 0.5
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,' ')
	    END DO
	END DO
C
C		Close all datasets
C
	CALL XVCLOSE(IPIX,STAT,' ')
	CALL XVCLOSE(IOUT,STAT,' ')
C
	RETURN
	END
C
C***********************************************************************
	SUBROUTINE READCAL(NWTS,ISL,NL,GAIN,X)
C
	REAL GAIN(10),X(30000,27),BUF(42)
	INTEGER LOC(26)/31,32,33,34,35,36,37,38,39,40,
     +			 3, 5, 7, 9,11,13,15,17,19,21,23,24,25,26,1,2/
C							Set the gain vlaues
	GAIN(1) = 77.2
	GAIN(2) = 50.4
	GAIN(3) = 62.3
	GAIN(4) = 62.8
	GAIN(5) = 89.9
	GAIN(6) = 100.2
	GAIN(7) = 82.2
	GAIN(8) = 33.9
	GAIN(9) = 10.0
	GAIN(10) = 4.1
C							Open the AUX dataset
	CALL XVUNIT(IUNIT,'INP',2,ISTAT,' ')
	CALL XVOPEN(IUNIT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
C						    Read each line of cal data,
	LINE = ISL
	DO I=1,NL
	    CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'NSAMPS',42,' ')
	    LINE = LINE+1
	    DO J=1,26
		X(I,J) = BUF(LOC(J))
	    END DO
	END DO
C						    Check for reasonable values
	DO J=11,24
	    IF (X(1,J).LT.0.0 .OR. X(1,J).GT.255.0) X(1,J)=X(2,J)
	    DO I=2,NL
		IF (ABS(X(I,J)-X(I-1,J)) .GT. 10.0) X(I,J)=X(I-1,J)
	    END DO
	END DO
	DO J=25,26
	    IF (X(1,J).LT.-20.0 .OR. X(1,J).GT.75.0) X(1,J)=X(2,J)
	    DO I=2,NL
		IF (ABS(X(I,J)-X(I-1,J)) .GT. 0.8) X(I,J)=X(I-1,J)
	    END DO
	END DO
C							Filter arrays
	DO J=26,11,-1
	    CALL UNIFLT(7,NL,X(1,J),X(1,J+1),NWTS)
	END DO
	DO J=10,1,-1
	    CALL MEDFLT(X(1,J),X(1,J+1),NL,NWTS)
	END DO
C							Build offset arrays
	DO J=1,10
	    GN = GAIN(J)
	    L1 = J + 1
	    L2 = L1 + 10
	    DO I=1,NL
		X(I,J) = GN*X(I,L1)*X(I,L2) 
	    END DO
	END DO
	DO I=1,NL
	    RADLO = 1000.0*PLANCK(11.0, X(I,26)+273.15)
	    RADHI = 1000.0*PLANCK(11.0, X(I,27)+273.15)
	    X(I,13) = (RADHI-RADLO)/(X(I,23)-X(I,22))
	    X(I,14) = (RADHI-RADLO)/(X(I,25)-X(I,24))
	    X(I,11) = RADLO - X(I,13)*X(I,22)
	    X(I,12) = RADLO - X(I,14)*X(I,24)
	END DO
C
	CALL XVCLOSE(IUNIT,ISTAT,' ')
	RETURN
	END
C**********************************************************
	SUBROUTINE MEDFLT(BUFIN,BUFOUT,NUM,NWTS)
C
	REAL BUFIN(NUM),BUFOUT(NUM),WINDOW(75)
C
	NWT = MIN(75,NWTS)		! filter width (max width = 75)
	NWT2 = NWT/2			! half of filter width
	MED = NWT2 + 1			! median location in window
C
C	                                                initialize window
	DO I=1,NWT2
	    WINDOW(I) = BUFIN(1)
	    WINDOW(I+NWT2) = BUFIN(I)
	END DO
	WINDOW(NWT) = BUFIN(MED)
	CALL SORTM(WINDOW,NWT)
	BUFOUT(1) = WINDOW(MED)
C						    compute value for each pixel
	DO II=2,NUM
C							add right pixel,
C						 remove left pixel from window
	    N = MAX(II-MED,1)
	    XREM = BUFIN(N)
	    N = MIN(II+NWT2,NUM)
	    XADD = BUFIN(N)
	    IF (XADD .GT. XREM) THEN
		I = 1
		DO WHILE (WINDOW(I) .NE. XREM)
		    I = I+1
		END DO
		DO WHILE (I.LT.NWT .AND. WINDOW(I+1).LT.XADD)
		    WINDOW(I) = WINDOW(I+1)
		    I = I+1
		END DO
		WINDOW(I) = XADD
	    ELSE IF (XADD .LT. XREM) THEN
		I = NWT
		DO WHILE (WINDOW(I) .NE. XREM)
		    I = I-1
		END DO
		DO WHILE (I.GT.1 .AND. WINDOW(I-1).GT.XADD)
		    WINDOW(I) = WINDOW(I-1)
		    I = I-1
		END DO
		WINDOW(I) = XADD
	    END IF
C
	    BUFOUT(II) = WINDOW(MED)
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SORTM(ARR,NUM)
C
C	This is an NlogN sort,  of NUM values in the real array ARR,
C	low to high
C
	REAL ARR(NUM)
C
	ISPAN = NUM/2
	DO WHILE (ISPAN .GT. 0)
	    LAST = NUM - ISPAN
	    LOC = 1
	    DO WHILE (LOC .LE. LAST)
		I1 = LOC
		DO WHILE (I1 .GE. 1)
		    I2 = I1 + ISPAN
		    IF (ARR(I1) .LE. ARR(I2)) GO TO 100
		    HOLD = ARR(I1)
		    ARR(I1) = ARR(I2)
		    ARR(I2) = HOLD
		    I1 = I1 - ISPAN
		END DO
  100		CONTINUE
		LOC = LOC + 1
	    END DO
	    ISPAN = ISPAN/2
	END DO
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tmscal.pdf
Process help=*
parm  INP	(string,40)	count=2
parm  OUT	(string,40)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  MODE	keyword default=RAD valid=(RAD,TEMP,ITEM,IRAD)
parm  FILTER	integer default=301
End-proc

.TITLE
TAE PROCESS TMSCAL
.HELP
PURPOSE:

   TMSCAL is a program which decalibrates Daedalus 12 Channel TMS data which
have already been logged with the program TMSLOG.  By default, it outputs all 
channels in radiance units (microWatts per square centimeter per steradian
per micrometer).  Optionally (if 'TEMP or 'ITEM is specified), the thermal IR
channels may be output in units of hundredths of degrees Celsius. The 
output is a band interleaved by line (BIL) data set of all twelve bands in 
two byte integer (HALF) format.

.PAGE
WRITTEN BY:  Ron Alley      February, 1992

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: New

.LEVEL1
.VARI INP
TMS image file,
auxiliary calibration file.
.VARI OUT
output data set of all 12 bands
of calibrated TMS data.
.VARI SIZE
The standard  VICAR output size
field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI MODE
This keyword selects the type
of decalibatrion to be performed
on the thermal channels
Valid:
   TEMP or ITEM for temperature
   RAD or IRAD for radiance
.VARI FILTER
Filter width for calibration
data smoothing.
.LEVEL2
.VARI INP
(1) The file containing all 12 bands of a TMS image in BIL format,
        which has been output from TMSLOG.
                Example: INP(1) = TMS.PIX
(2) The file containing the auxiliary calibration data set, the second
        output for a given flight line from TMSLOG.
                Example: INP(2) = TMS.AUX
.VARI OUT
  THE output data set in INTEGER*2 (HALFWORD) format of all 12 channels
  of calibrated TMS data.  The exact type of data output (i.e. temperature
  or radiance) is determined by the MODE parameter.
                Example: OUT = TMS.CAL
.VARI SIZE
  The standard VICAR2 output size field.   Default will calibrate the entire 
  dataset.
                Example: SIZE = (1,1,200,500)
.VARI SL
  Starting line (same as SIZE(1)).
.VARI SS
  Starting sample (same as SIZE(2)).
.VARI NL
  Number of lines (same as SIZE(3)).
.VARI NS
  Number of samples (same as SIZE(4)).
.VARI MODE
  This keyword selects the type of decalibatrion to be performed on
  Channels 11 and 12, the thermal IR channels.
  There is a choice of two possibilities:
       RAD - for decalibration to radiance units
             IRAD is synonymous with RAD, and is permitted for compatibility
             with the operation of TIMSCAL
      TEMP - for decalibration to temperature units (Celsius)
             ITEM is synonymous with TEMP, and is permitted for compatibility
             with the operation of TIMSCAL
.VARI FILTER
	This selects the width of the filter used to smooth the calibration
	file data.  Its value must be an odd integer.
$ Return
$!#############################################################################
$Imake_File:
$ create tmscal.imake
#define  PROGRAM   tmscal

#define MODULE_LIST tmscal.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
