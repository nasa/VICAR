$!****************************************************************************
$!
$! Build proc for MIPL module timsemis
$! VPACK Version 1.8, Tuesday, July 16, 1996, 17:04:42
$!
$! Execute by entering:		$ @timsemis
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
$ write sys$output "*** module timsemis ***"
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
$ write sys$output "Invalid argument given to timsemis.com file -- ", primary
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
$   if F$SEARCH("timsemis.imake") .nes. ""
$   then
$      vimake timsemis
$      purge timsemis.bld
$   else
$      if F$SEARCH("timsemis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timsemis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timsemis.bld "STD"
$   else
$      @timsemis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timsemis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timsemis.com -
	-s timsemis.f -
	-p timsemis.pdf -
	-i timsemis.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timsemis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	INTEGER*2 BUF(638)/638*0/
	LOGICAL XVPTST,QBBFIT,QREFCHAN
	CHARACTER*100 LABEL
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT2,'OUT',2,ISTAT,' ')
	CALL XVOPEN(IOUT2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA','OP',
     &	     'WRITE','U_NL',NL,'U_NS',NS,'U_NB',1,'U_ORG','BSQ',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','WRITE','U_ORG','BSQ','U_NL',NL,'U_NS',NS,' ')
	    DO I=1,6
		DO J=1,NL
		    CALL XVWRIT(IOUT,BUF,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(IOUT,ISTAT,' ')
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','UPDATE',' ')
	ELSE
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','WRITE','U_ORG','BIL','U_NL',NL,'U_NS',NS,' ')
	END IF
C						Get and process DATE parameter
	CALL XVPARM('DATE',IDATE,NUM,IDEF,0)
	IF (IDATE.LT.0) THEN
C					Check to see if the date is in the label
C
	    CALL XLGET(IN,'HISTORY','INFO1',LABEL,ISTAT,'HIST',
     &			'TIMSLOG','FORMAT','STRING',' ')
	    IF (ISTAT.LT.0) CALL XLGET(IN,'HISTORY','INFO1',LABEL,ISTAT,
     &				'HIST','VTIMSLOG','FORMAT','STRING',' ')
	    IF (ISTAT.LT.0) CALL XLGET(IN,'HISTORY','LAB1',LABEL,ISTAT,
     &				'HIST','VTIMSLOG','FORMAT','STRING',' ')
C
	    IF (LABEL(6:6).EQ.'D') THEN
		READ (LABEL,90,err=95) MONTH,IDAY,IYEAR
   90		FORMAT (14X,I2,1X,I2,1X,I2)
	    ELSE
		READ (LABEL,92,err=95) MONTH,IDAY,IYEAR
   92		FORMAT (17X,I2,1X,I2,1X,I2)
	    END IF
	    IDATE = 10000*IYEAR+100*MONTH+IDAY
   95	    CONTINUE
	END IF
	IF (IDATE .LT. 0) THEN
	    CALL XVMESSAGE(' Unable to read date in VICAR label.',' ')
	    CALL XVMESSAGE(
     +		' Please specify the date as a parameter.',' ')
	    CALL ABEND
	END IF
C							Get the other parameters
	QBBFIT = XVPTST('BBFIT')
	QREFCHAN = XVPTST('REFCHAN')
	CALL XVPARM('E',E,NUM,IDEF,0)
	CALL XVPARM('REFCHAN',IREF,NUM,IDEF,0)
C							       Get lookup tables
	CALL GET_TIMS_RAD_LUT(IDATE,0.0,RAD_LUT)
	CALL GET_TIMS_TEMP_LUT(IDATE,0.0,TEMP_LUT)
C					    Remove obsolete VICAR history labels
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL2',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL2',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL2',' ')
	CALL XLDEL(IOUT2,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL2',' ')
C						    Add new VICAR history labels
	CALL XLADD(IOUT,'HISTORY','LBL1','Emissivity Image',
     &			ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = 10,000*Emissivity',
     &			ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT2,'HISTORY','LBL1','Ground Temperature Image',
     &			ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT2,'HISTORY','LBL2',
     &	      'DN = 100*Degrees Celsius',ISTAT,'FORMAT','STRING',' ')
C
C									refchan
	IF (QREFCHAN) THEN
     	    WRITE (LABEL,100) E,IREF
  100	    FORMAT(F5.3,' emittance fit to reference channel',I2)
   	    CALL XLADD(IOUT,'HISTORY','METHOD',LABEL,ISTAT,
     &		       'FORMAT','STRING',' ')
  	    CALL XLADD(IOUT2,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL EMISCAL(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IREF,E,RAD_LUT,
     &			 TEMP_LUT)
C									bbfit
	ELSE IF (QBBFIT) THEN
	    WRITE (LABEL,200) E
  200	    FORMAT(F5.3,' Emittance graybody fit')
	    CALL XLADD(IOUT,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL XLADD(IOUT2,'HISTORY','METHOD',LABEL,ISTAT,
     &			'FORMAT','STRING',' ')
	    CALL EMISCAL2(IN,IOUT,IOUT2,ISL,ISS,NL,NS,E,RAD_LUT,
     &			  TEMP_LUT)
C									2-Chan
	ELSE
	    CALL XLADD(IOUT,'HISTORY','METHOD','Two channel fit',
     &			ISTAT,'FORMAT','STRING',' ')
	    CALL XLADD(IOUT2,'HISTORY','METHOD','Two channel fit',
     &			ISTAT,'FORMAT','STRING',' ')
	    CALL EMISCAL3(IN,IOUT,IOUT2,ISL,ISS,NL,NS,E,RAD_LUT,
     &			  TEMP_LUT)
	END IF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	CALL XVCLOSE(IOUT2,ISTAT,' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE EMISCAL(IN,IOUT,IOUT2,ISL,ISS,NL,NS,IREF,E,RAD_LUT,
     +			   TEMP_LUT)
C
	REAL BBRAD(5000,6)
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	INTEGER*2 OUTBUF(5000),INBUF(5000)
C
	ILINE = ISL
	DO I=1,NL
	    CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'BAND',IREF,' ')
	    ISAMP = ISS
	    DO J=1,NS
		IRAD = NINT(INBUF(ISAMP)/E)
		IRAD = MIN(32767,MAX(1,IRAD))
		ITEMP = TEMP_LUT(IRAD,IREF)
		OUTBUF(J) = ITEMP
		DO ICHAN=1,6
		    BBRAD(J,ICHAN) = RAD_LUT(ITEMP+27315,ICHAN)
C                                               BBRAD(J,ICHAN) is the blackbody
C                                               radiance of channel ICHAN at
C                                               temperature ITEMP.
		END DO
		ISAMP = ISAMP+1
	    END DO
	    CALL XVWRIT(IOUT2,OUTBUF,ISTAT,' ')
	    DO ICHAN=1,6
		CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,
     +			    'BAND',ICHAN,' ')
		ISAMP = ISS
		DO J=1,NS
		    OUTBUF(J) = 10000.0*INBUF(ISAMP)/BBRAD(J,ICHAN) + .5
		    ISAMP = ISAMP+1
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,'BAND',ICHAN,' ')
	    END DO
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE EMISCAL2(IN,IOUT,IOUT2,ISL,ISS,NL,NS,E,RAD_LUT,
     +			    TEMP_LUT)
C
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	INTEGER*2 OUTBUF(5000,6),INBUF(5000,6),TBUF(5000)
C
	IEL = ISL + NL - 1
	DO ILINE=ISL,IEL
	    DO ICHAN=1,6
		CALL XVREAD(IN,INBUF(1,ICHAN),ISTAT,'LINE',ILINE,
     +			    'SAMP',ISS,'NSAMPS',NS,'BAND',ICHAN,' ')
	    END DO
	    DO ISAMP=1,NS
C						Find the max temperature that is
C						consistent with the given max e
		ITEMP = -32768
		DO ICHAN=1,6
		    IRAD = NINT(INBUF(ISAMP,ICHAN)/E)
		    IRAD = MIN(32767,MAX(1,IRAD))
		    ITEMP = MAX(ITEMP,TEMP_LUT(IRAD,ICHAN))
		END DO
		TBUF(ISAMP) = ITEMP
		DO ICHAN=1,6
		    OUTBUF(ISAMP,ICHAN) = NINT(10000.0 * FLOAT(
     +				INBUF(ISAMP,ICHAN)) / FLOAT(
     +				RAD_LUT(ITEMP+27315,ICHAN)))
		END DO
	    END DO
	    CALL XVWRIT(IOUT2,TBUF,ISTAT,' ')
	    DO ICHAN=1,6
		CALL XVWRIT(IOUT,OUTBUF(1,ICHAN),ISTAT,
     +			    'LINE',ILINE-ISL+1,'BAND',ICHAN,' ')
	    END DO
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE EMISCAL3(IN,IOUT,IOUT2,ISL,ISS,NL,NS,E,RAD_LUT,
     +			    TEMP_LUT)
C
	REAL EMIS(6)
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	INTEGER*2 OUTBUF(5000,6),INBUF(5000,6),TBUF(5000),IRAD(6)
C
	IEL = ISL + NL - 1
	DO ILINE=ISL,IEL
	    DO ICHAN=1,6
		CALL XVREAD(IN,INBUF(1,ICHAN),ISTAT,'LINE',ILINE,
     +			    'SAMP',ISS,'NSAMPS',NS,'BAND',ICHAN,' ')
	    END DO
	    DO ISAMP=1,NS
		ITEMP = -32768
		DO ICHAN=1,6
		    IRAD(ICHAN) = NINT(INBUF(ISAMP,ICHAN)/E)
		    IRAD(ICHAN) = MIN(32767,MAX(1,IRAD(ICHAN)))
		    ITEMP = MAX(ITEMP,TEMP_LUT(IRAD(ICHAN),ICHAN))
		END DO
		ITEMP = MIN(ITEMP+27315,40000)
C						calculate bbfit emittances
		DO ICHAN=1,6
		    BB = RAD_LUT(ITEMP,ICHAN)
		    IF (BB .NE. 0.0) THEN
			EMIS(ICHAN) = IRAD(ICHAN)/BB
		    ELSE
			EMIS(ICHAN) = -10.0*ICHAN
		    END IF
		END DO
C				find the 2 emittances in closest agreement
		DELTA = 10000000.0
		DO I=2,6
		    DO J=1,I-1
			ETEST = ABS(EMIS(I)-EMIS(J))
			IF (ETEST .LT. DELTA) THEN
			    DELTA = ETEST
			    II = I
			    JJ = J
			END IF
		    END DO
		END DO
C							     compute temperature
		RATIO = FLOAT(IRAD(JJ)) / FLOAT(IRAD(II))
		ITEMP = NEWTEMP(RATIO,JJ,II,ITEMP,RAD_LUT)
		TBUF(ISAMP) = ITEMP - 27315
C
C				compute emittances based upon output temperature
		DO ICHAN=1,6
		    OUTBUF(ISAMP,ICHAN)=NINT(10000.0*FLOAT(IRAD(ICHAN))/
     +					    FLOAT(RAD_LUT(ITEMP,ICHAN)))
		END DO
	    END DO
C							       write out results
	    CALL XVWRIT(IOUT2,TBUF,ISTAT,' ')
	    DO ICHAN=1,6
		CALL XVWRIT(IOUT,OUTBUF(1,ICHAN),ISTAT,
     +			    'LINE',ILINE-ISL+1,'BAND',ICHAN,' ')
	    END DO
	END DO
	RETURN
	END
C****************************************************************************
	FUNCTION NEWTEMP(RATIO,ICHAN,JCHAN,ITEMP,RAD_LUT)
C
C	Use the bisection method to solve for temperature, given the ratio of
C	the radiances (RATIO) at two given wavelengths (WVL1 and WVL2). 
C	TEMPIN is a first trial value for the solution.
C
	INTEGER*2 RAD_LUT(40000,6)
C
	LOWTEMP = ITEMP - 4096
	IHITEMP = ITEMP + 4096
	VLOW = RATIO - (FLOAT(RAD_LUT(LOWTEMP-1,ICHAN) +
     +		RAD_LUT(LOWTEMP,ICHAN) + RAD_LUT(LOWTEMP+1,ICHAN)) /
     +		FLOAT(RAD_LUT(LOWTEMP-1,JCHAN) +
     +	 	RAD_LUT(LOWTEMP,JCHAN) + RAD_LUT(LOWTEMP+1,JCHAN)))
	DO I=1,13
	    MTEMP = (LOWTEMP + IHITEMP) / 2
	    VSOLVE = RATIO - (FLOAT(RAD_LUT(MTEMP-1,ICHAN) +
     +		RAD_LUT(MTEMP,ICHAN) + RAD_LUT(MTEMP+1,ICHAN)) /
     +		FLOAT(RAD_LUT(MTEMP-1,JCHAN) +
     +		RAD_LUT(MTEMP,JCHAN) + RAD_LUT(MTEMP+1,JCHAN)))
	    IF (VSOLVE*VLOW .GT. 0.0) THEN
		LOWTEMP = MTEMP
		VLOW = VSOLVE
	    ELSE
		IHITEMP = MTEMP
	    END IF
	END DO
C
	VHI = RATIO - (FLOAT(RAD_LUT(IHITEMP-1,ICHAN) +
     +		RAD_LUT(IHITEMP,ICHAN) + RAD_LUT(IHITEMP+1,ICHAN)) /
     +		FLOAT(RAD_LUT(IHITEMP-1,JCHAN) +
     +		RAD_LUT(IHITEMP,JCHAN) + RAD_LUT(IHITEMP+1,JCHAN)))
	IF (ABS(VLOW) .LE. ABS(VHI)) THEN
	    NEWTEMP = LOWTEMP
	ELSE
	    NEWTEMP = IHITEMP
	END IF
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timsemis.pdf
Process help=*
parm  INP	(string,40)
parm  OUT	(string,40) 	count=2
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  MODE	keyword default=REFCHAN	valid=(REFCHAN,BBFIT,TWOCHAN)
parm  E		real	default=0.95
parm  REFCHAN	integer	default=6	valid=(1,2,3,4,5,6)
parm  DATE	integer default=-1
parm  ORG       keyword valid=(BIL,BSQ)  default=BIL
End-proc

.TITLE
VICAR PROCESS TIMSEMIS
.HELP
PURPOSE:
     TIMSEMIS is a program to generate spectral emissivity images from
calibrated TIMS data. The input dataset should be the 6 calibrated (IRAD or
GRAD) TIMS channels. The first output will contain the 6 spectral emittance
images, while the second output will contain the temperature image.
     Three algorithms (MODEs) have presently been implemented. The first method,
called REFCHAN, sets a specified reference channel to a specified constant
emittance value. The default sets Channel 6 to 0.95. The temperature is computed
based upon the radiance and given emittance of the reference channel, then the 
other five spectral emittances are computed, using this temperature.
     The second method, called BBFIT, assumes that the highest spectral
emittance is at a user-specified level (default = 0.95). It then computes the
highest temperature consistent with this assumption, and the six spectral
emittances, based upon the computed temperature.
     The third method, TWOCHAN, estimates the temperature by assuming that
the emissivity is exactly equal in the two channels whose blackbody fit
emissivities are most nearly equal.  The kinetic temperature is then
computed to fit the ratio of the radiances of those two channels, then each 
of the emissivities is calculated.
.LEVEL1
.VARI INP
TIMS calibrated image (RAD)
.VARI OUT
(1) Spectral emittance image
(2) Temperature image
.VARI SIZE
The standard  VICAR2 output
size field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI REFCHAN
(For REFCHAN mode only)
The channel to be used as the
reference channel. Valid: (1:6)
.VARI DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.VARI E
The emittance assumed for the
reference channel (REFCHAN or
TWOCHAN mode); or the maximum
emittance (BBFIT mode)
.VARI MODE
REFCHAN, TWOCHAN, or BBFIT
.VARI ORG
Organization of output dataset
.LEVEL2
.VARI INP
The file containing all six bands of a calibrated TIMS image, using the
WATTS units.
.VARI OUT
     The first output dataset is a halfword image of all six bands of TIMS
emittance data, scaled such that an emittance of 1.0 is 10,000 DN.
     The second output dataset is a halfword temperature image, where each DN
corresponds to 0.01 degree Celsius. 0 DN corresponds to 0 degrees C.
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
.VARI REFCHAN
	The channel to which the other bands are to be referenced, in
	the case of REFCHAN mode, is input through this parameter.
	The default is band six is used. In the BBFIT mode, this parameter
        is ignored.
		Example: REFCHAN = 5
.VARI DATE
	TIMSEMIS uses the date of data acquisition to determine the proper
	calibration coefficients.  If defaulted, the date in the VICAR 
	label is used. This parameter is needed only if the VICAR label
	is incorrect, or if an abnormal calibration set is to be used.
.VARI E
     In the REFCHAN mode, the spectral emittance of the reference channel is
set to a constant value, given by this parameter.
     In the BBFIT mode, spectral emittances are computed so that the maximum
of the six emittance values is equal to the value of E.
     In the TWOCHAN mode, the spectral emittance of the two channels with the
most 
.VARI MODE
     Two algorithms (MODEs) have presently been implemented. The first method,
called REFCHAN, sets a specified reference channel to a specified constant
emittance value. The default sets Channel 6 to 0.95. The temperature is computed
based upon the radiance and given emittance of the reference channel, then the 
other five spectral emittances are computed, using this temperature.
     The second method, called BBFIT, assumes that the highest spectral
emittance is at a user-specified level (default = 0.95). It then computes the
highest temperature consistent with this assumption, and the six spectral
emittances, based upon the computed temperature.
.VARI ORG
ORG specifies the organizational format of the first (emissivity) output 
dataset. BIL (the default) and BSQ are supported.  
$ Return
$!#############################################################################
$Imake_File:
$ create timsemis.imake
#define  PROGRAM   timsemis

#define MODULE_LIST timsemis.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
