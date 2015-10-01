$!****************************************************************************
$!
$! Build proc for MIPL module timsconv
$! VPACK Version 1.8, Thursday, July 11, 1996, 18:29:44
$!
$! Execute by entering:		$ @timsconv
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
$ write sys$output "*** module timsconv ***"
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
$ write sys$output "Invalid argument given to timsconv.com file -- ", primary
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
$   if F$SEARCH("timsconv.imake") .nes. ""
$   then
$      vimake timsconv
$      purge timsconv.bld
$   else
$      if F$SEARCH("timsconv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timsconv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timsconv.bld "STD"
$   else
$      @timsconv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timsconv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timsconv.com -
	-s timsconv.f -
	-p timsconv.pdf -
	-i timsconv.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timsconv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIT(6),TEMP(10000),RAD(10000)
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	CHARACTER*80 PRT
	CHARACTER*4 OFMT
	LOGICAL XVPTST,QRAD
C						Open input, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'U_FORMAT','REAL','IO_ACT','SA',
     +		    'OPEN_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C						Get and process DATE parameter
	CALL XVPARM('DATE',IDATE,NUM,IDEF,0)
	IF (IDEF.NE.0) THEN
C				Check to see if the date is already in the label
C
	    CALL XLGET(IN,'HISTORY','INFO1',PRT,ISTAT,'HIST','TIMSLOG',
     &			'FORMAT','STRING',' ')
	    IF (ISTAT.LT.0) CALL XLGET(IN,'HISTORY','INFO1',PRT,ISTAT,
     &				 'HIST','VTIMSLOG','FORMAT','STRING',' ')
	    IF (ISTAT.LT.0) CALL XLGET(IN,'HISTORY','LAB1',PRT,ISTAT,
     &				'HIST','VTIMSLOG','FORMAT','STRING',' ')
C
	    IF (PRT(6:6).EQ.'D') THEN
		READ(PRT,90,err=95) MONTH,IDAY,IYEAR
   90		FORMAT (14X,I2,1X,I2,1X,I2)
	    ELSE
		READ(PRT,92,err=95) MONTH,IDAY,IYEAR
   92		FORMAT (17X,I2,1X,I2,1X,I2)
	    END IF
	    IDATE = 10000*IYEAR+100*MONTH+IDAY
   95	    CONTINUE
	    IF (IDATE.LT.0) THEN
		CALL XVMESSAGE(' Unable to read date in VICAR label.',
     +				' ')
		CALL XVMESSAGE(' Please specify the date as a parameter.',' ')
		CALL ABEND
	    END IF
	END IF
C							Get the other parameters
	QRAD = XVPTST('RAD')
	CALL XVPARM('TEMPFAC',TEMPFAC,NUM,IDEF,0)
	CALL XVPARM('RADFAC',RADFAC,NUM,IDEF,0)
	CALL XVPARM('EMIT',EMIT,NUM,IDEF,6)
	CALL XVPARM('FORMAT',OFMT,NUM,IDEF,0)
	IF (IDEF.NE.0) CALL XVGET(IN,ISTAT,'FORMAT',OFMT,' ')
C								open output
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		   'U_FORMAT','REAL','O_FORMAT',OFMT,'OP','WRITE',
     &		   'U_NL',NL,'U_NS',NS,'U_NB',6,'U_ORG','BIL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL2',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL2',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCONV',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCONV',' ')
C							Set minimum and maximum 
	IF (OFMT .EQ. 'BYTE') THEN
	    ROUND = 0.5
	    XMIN = 0.0
	    XMAX = 255.0
	ELSE IF (OFMT .EQ. 'HALF') THEN
	    ROUND = 0.5
	    XMIN = -32768.0
	    XMAX = 32767.0
	ELSE IF (OFMT .EQ. 'FULL') THEN
	    ROUND = 0.5
	    XMIN = -2147483600.0
	    XMAX =  2147483600.0
	ELSE
	    ROUND = 0.0
	    XMIN = -1.0E30
	    XMAX =  1.0E30
	END IF
C							Temperature to radiance
	IF (QRAD) THEN
	    CALL GET_TIMS_RAD_LUT(IDATE,0.0,RAD_LUT)
	    CALL XVMESSAGE(' Converting from Temperature to Radiance',
     &			   ' ')
	    CALL XLADD(IOUT,'HISTORY','LBL1','Radiance Image',
     &		       ISTAT,'FORMAT','STRING',' ')
	    WRITE(PRT,200) RADFAC + 0.00005, CHAR(0)
  200	    FORMAT('DN =',F9.4,' * milliwatts/(m*m*sr*micrometer)',A1)
	    CALL XLADD(IOUT,'HISTORY','LBL2',PRT,ISTAT,
     &			'FORMAT','STRING',' ')
C								Loop thru image
	    TEMPFAC = 100.0*TEMPFAC
	    DO ILINE=ISL,ISL+NL-1
		DO ICHAN=1,6
		    CALL XVREAD(IN,TEMP,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &				'SAMP',ISS,'NSAMPS',NS,' ')
		    DO J=1,NS
			ITEMP = NINT(TEMPFAC*TEMP(J) + 27315.0)
			ITEMP = MIN(40000,MAX(1,ITEMP))
			XRAD = EMIT(ICHAN)*RAD_LUT(ITEMP,ICHAN)
			RAD(J) = MIN(XMAX, 
     &				 MAX(XMIN, XRAD/RADFAC + ROUND))
		    END DO
		    CALL XVWRIT(IOUT,RAD,ISTAT,' ')
		END DO
	    END DO
C							Radiance to temperature
	ELSE 
	    CALL GET_TIMS_TEMP_LUT(IDATE,0.0,TEMP_LUT)
	    CALL XVMESSAGE(' Converting from Radiance to Temperature',
     &			   ' ')
	    CALL XLADD(IOUT,'HISTORY','LBL1','Temperature Image',
     &			ISTAT,'FORMAT','STRING',' ')
	    WRITE(PRT,300) TEMPFAC + 0.00005, CHAR(0)
  300	    FORMAT('DN =',F9.4,' Degrees Celsius',A1)
	    CALL XLADD(IOUT,'HISTORY','LBL2',PRT,ISTAT,
     &			'FORMAT','STRING',' ')
C								Loop thru image
	    DO ILINE=ISL,ISL+NL-1
		DO ICHAN=1,6
		    CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &				'SAMP',ISS,'NSAMPS',NS,' ')
		    DO J=1,NS
			IRAD = NINT(RADFAC*RAD(J)/EMIT(ICHAN))
			IRAD = MIN(32767,MAX(1,IRAD))
			XTEMP = TEMP_LUT(IRAD,ICHAN) / 100.0
			TEMP(J) = MIN(XMAX, 
     &				  MAX(XMIN, XTEMP/TEMPFAC + ROUND))
		    END DO
		    CALL XVWRIT(IOUT,TEMP,ISTAT,' ')
		END DO
	    END DO
	ENDIF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timsconv.pdf
Process help=*
parm  INP	(string,80)
parm  OUT	(string,80)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  MODE	keyword	valid=(TEMP,RAD) default=TEMP
parm  EMIT	real	default=(0.93,0.93,0.93,0.93,0.93,0.93)	count=6
parm  FORMAT	keyword valid=(byte,half,full,real) count=0:1 default=--
parm  TEMPFAC   real    default=0.01
parm  RADFAC    real    default=1.0
parm  DATE	integer default=-1
End-proc

.TITLE
TAE PROCESS TIMSCONV
.HELP
PURPOSE:

   TIMSCONV is a program which converts TIMS data from radiance to temperature,
or vice versa.  The input must be a 3-D file of all 6 bands, and the
output will also be a 3-D file of all 6 bands, though the user may choose
the output format (byte, half, full, or real). 

EXECUTION:

   The following is the execution statement format for TIMSCONV:

	TIMSCONV INP=PIX OUT=CALPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 



WRITTEN BY:  Ron Alley    April, 1991

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: 1.0  Conversion to remove central wavelengths    July, 1996

.LEVEL1
.VARI INP
TIMS radiance or temperature
image.
.VARI OUT
TIMS temperature or radiance
image; opposite of INP
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
.VARI MODE
Conversion is to these units
Valid: TEMP, RAD
.VARI TEMPFAC
Rescaling factor to get image
data into degrees Celsius
.VARI RADFAC
Rescaling factor to get 
image data into 
milliwatts/(m*m*sr*micrometer)
.VARI FORMAT
Output data format
Default is same as input format
Valid: BYTE, HALF, FULL, REAL
.VARI EMIT
The emittance assumed for
each band.
.VARI DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.LEVEL2
.VARI INP
The file containing all six bands of a calibrated TIMS image, in either
Temperature or Radiance units.  If the units are not the ones used by 
TIMSCAL/TIMSCAL2, then the rescaling factor TEMPFAC or RADFAC must be used.
.VARI OUT
The file containing all six bands of a calibrated TIMS image, in either
Radiance or Temperature units (opposite units from INP).  If the units to be
output are not the ones used by TIMSCAL/TIMSCAL2, then the rescaling factor 
TEMPFAC or RADFAC must be used.  The output data format (byte, half, full, or
real) may be selected by the user via keyword.  If defaulted, the output
format will be the same as the input format.
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
.VARI MODE
This keyword selects the type of decalibatrion to be performed.
There is a choice of two possibilities:
       TEMP - Conversion from radiance to temperature
       RAD  - Conversion from temperature to radiance
.VARI DATE
	TIMSCONV uses the date of data acquisition to determine the proper
	conversion coefficients.  If defaulted, the date in the VICAR 
	label is used. This parameter is needed only if the VICAR label
	is incorrect, or if an abnormal calibration set is to be used.
.VARI TEMPFAC
TEMPFAC is the rescaling factor to convert image data (either input or output)
into degrees Celsius.  The dafault, 0.01, is the factor appropriate for
TIMSCAL/TIMSCAL2 generated images.
.VARI RADFAC
TEMPFAC is the rescaling factor to convert image data (either input or output)
into milliwatts/(m*m*sr*micrometer).  The dafault, 1.0, is the factor 
appropriate for TIMSCAL/TIMSCAL2 generated images.
.VARI FORMAT
FORMAT allows the user to specify the output data format.  The default is for
the output image to be the same as the input format.  BYTE, HALF, FULL, and
REAL formats are valid
.VARI EMIT
The six values of EMIT are the emittance values assumed for each TIMS band in
performing the Temperature/Radiance conversion.
$ Return
$!#############################################################################
$Imake_File:
$ create timsconv.imake
#define  PROGRAM   timsconv

#define MODULE_LIST timsconv.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
