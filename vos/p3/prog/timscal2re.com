$!****************************************************************************
$!
$! Build proc for MIPL module timscal2re
$! VPACK Version 1.8, Wednesday, March 28, 2001, 13:31:58
$!
$! Execute by entering:		$ @timscal2re
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
$ write sys$output "*** module timscal2re ***"
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
$ write sys$output "Invalid argument given to timscal2re.com file -- ", primary
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
$   if F$SEARCH("timscal2re.imake") .nes. ""
$   then
$      vimake timscal2re
$      purge timscal2re.bld
$   else
$      if F$SEARCH("timscal2re.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timscal2re
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timscal2re.bld "STD"
$   else
$      @timscal2re.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timscal2re.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timscal2re.com -
	-s timscal2re.f -
	-p timscal2re.pdf -
	-i timscal2re.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timscal2re.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C       Jul 2, 2001   ...rea...  Update MODTRAN3 argument list
C
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6)
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	REAL CWAVE(6)
	CHARACTER*2 SLMODEL, SLHUMID, SLOZONE, SLH2O, SLTEMP
	CHARACTER*100 LABEL
	CHARACTER*80 LOWFILE,LOWTAB
	LOGICAL XVPTST,QMOD
C						Open datasets, get size field
	CALL XVUNIT(IN1,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN1,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IN2,'INP',2,ISTAT,' ')
	CALL XVOPEN(IN2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','WRITE','U_ORG','BSQ','U_NL',NL,'U_NS',NS,' ')
	    DO I=1,6
		DO J=1,NL
		    CALL XVWRIT(IOUT,TRAN,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(IOUT,ISTAT,' ')
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','UPDATE',' ')
	ELSE
	    CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','WRITE','U_ORG','BIL','U_NL',NL,'U_NS',NS,' ')
	END IF
C
C				Check to see if the date is already in the label
C
	CALL XLGET(IN,'HISTORY','INFO1',LABEL,ISTAT,'HIST','TIMSLOG',
     &		   'FORMAT','STRING',' ')
	IF (ISTAT.LT.0) CALL XLGET(IN,'HISTORY','INFO1',LABEL,ISTAT,
     &				'HIST','VTIMSLOG','FORMAT','STRING',' ')
	IF (ISTAT.LT.0) CALL XLGET(IN,'HISTORY','LAB1',LABEL,ISTAT,
     &				'HIST','VTIMSLOG','FORMAT','STRING',' ')
	IDATE = -1
	IF (LABEL(6:6).EQ.'D') THEN
	    READ (LABEL,80,err=95) MONTH,IDAY,IYEAR
   80	    FORMAT(14X,I2,1X,I2,1X,I2)
	ELSE
	    READ (LABEL,90,err=95) MONTH,IDAY,IYEAR
   90	    FORMAT(17X,I2,1X,I2,1X,I2)
	END IF
	IDATE = 10000*IYEAR+100*MONTH+IDAY
   95	CONTINUE
C						Get and process DATE parameter
	CALL XVPARM('DATE',JDATE,NUM,IDEF,0)
	IF (JDATE.GT.0) IDATE=JDATE
	IF (IDATE.LT.0) THEN
	    CALL XVMESSAGE(' Unable to read date in VICAR label.',' ')
	    CALL XVMESSAGE(
     +		' Please specify the date as a parameter.',' ')
	    CALL ABEND
	END IF
C							Get the other parameters
	CALL XVPARM('SHIFT',SHIFT,NUM,IDEF,0)
	CALL XVPARM('REFCHAN',IREF,NUM,IDEF,0)
	CALL XVPARM('FILTER',NWTS,NUM,IDEF,0)
	CALL XVPARM('AMODEL',SLMODEL,NUM,IDEF,0)
	CALL XVPARM('AHUMID',SLHUMID,NUM,IDEF,0)
	CALL XVPARM('AOZONE',SLOZONE,NUM,IDEF,0)
	CALL XVPARM('ATEMP',SLTEMP,NUM,IDEF,0)
	CALL XVPARM('HEIGHT',HEIGHT,NUM,IDEF,0)
	CALL XVPARM('DATUM',DATUM,NUM,IDEF,0)
	CALL XVPARM('NLAYERS',ML,NUM,IDEF,0)
	CALL XVPARM('ALTITUDE',ALTS,NUM,IDEF,0)
	CALL XVPARM('PRESSURE',PRESS,NUM,IDEF,0)
	CALL XVPARM('TEMP',TEMPS,NUM,IDEF,0)
	CALL XVPARM('WATER',H2OS,NUM,IDEF,0)
	CALL XVPARM('WTYPE',SLH2O,NUM,IDEF,0)
	CALL XVPARM('O3FAC',O3FAC,NUM,IDEF,0)
	CALL XVPARM('O2FAC',O2FAC,NUM,IDEF,0)
	CALL XVPARM('CO2FAC',CO2FAC,NUM,IDEF,0)
	CALL XVPARM('SO2FAC',SO2FAC,NUM,IDEF,0)
	CALL XVPARM('CH4FAC',CH4FAC,NUM,IDEF,0)
	CALL XVPARM('WATERFAC',WATERFAC,NUM,IDEF,0)
C							LOWTRAN/MODTRAN choice
C							and report files
	QMOD = XVPTST('MODTRAN')
	CALL XVPARM('LOWFILE',LOWFILE,NUM,IDEF,0)
	IF (NUM .EQ. 0) LOWFILE='dummy'
	CALL XVPARM('LOWTAB',LOWTAB,NUM,IDEF,0)
	IF (NUM .EQ. 0) LOWTAB='dummy'
C							Interpret MODEL number
	IF(SLMODEL.EQ.'TR') THEN
	    LMODEL = 1
	ELSE IF(SLMODEL.EQ.'MS') THEN
	    LMODEL = 2
	ELSE IF(SLMODEL.EQ.'MW') THEN
	    LMODEL = 3
	ELSE IF(SLMODEL.EQ.'SS') THEN
	    LMODEL = 4
	ELSE IF(SLMODEL.EQ.'SW') THEN
	    LMODEL = 5
	ELSE IF(SLMODEL.EQ.'ST') THEN
	    LMODEL = 6
	ELSE IF(SLMODEL.EQ.'RS') THEN
	    LMODEL = 7
	END IF
C
	IF(SLMODEL.NE.'RS') THEN
	    IF(SLHUMID.EQ.'  ') THEN
		LHUMID = LMODEL
	    ELSE IF(SLHUMID.EQ.'TR') THEN
		LHUMID = 1
	    ELSE IF(SLHUMID.EQ.'MS') THEN
		LHUMID = 2
	    ELSE IF(SLHUMID.EQ.'MW') THEN
		LHUMID = 3
	    ELSE IF(SLHUMID.EQ.'SS') THEN
		LHUMID = 4
	    ELSE IF(SLHUMID.EQ.'SW') THEN
		LHUMID = 5
	    ELSE IF(SLHUMID.EQ.'ST') THEN
		LHUMID = 6
	    END IF
	END IF
C
	IF(SLOZONE.EQ.'  ') THEN
	    IF (LMODEL.NE.7) THEN
		LOZONE = LMODEL
	    ELSE
		LOZONE = 2
	    END IF
	ELSE IF(SLOZONE.EQ.'TR') THEN
	    LOZONE = 1
	ELSE IF(SLOZONE.EQ.'MS') THEN
	    LOZONE = 2
	ELSE IF(SLOZONE.EQ.'MW') THEN
	    LOZONE = 3
	ELSE IF(SLOZONE.EQ.'SS') THEN
	    LOZONE = 4
	ELSE IF(SLOZONE.EQ.'SW') THEN
	    LOZONE = 5
	ELSE IF(SLOZONE.EQ.'ST') THEN
	    LOZONE = 6
	END IF
C
	IF(SLTEMP.EQ.'  ') THEN
	    LTEMP = 0
	ELSE IF(SLTEMP.EQ.'TR') THEN
	    LTEMP = 1
	ELSE IF(SLTEMP.EQ.'MS') THEN
	    LTEMP = 2
	ELSE IF(SLTEMP.EQ.'MW') THEN
	    LTEMP = 3
	ELSE IF(SLTEMP.EQ.'SS') THEN
	    LTEMP = 4
	ELSE IF(SLTEMP.EQ.'SW') THEN
	    LTEMP = 5
	ELSE IF(SLTEMP.EQ.'ST') THEN
	    LTEMP = 6
	END IF
C
	IF (SLH2O.NE.'  ') THEN
	    IF (SLH2O.EQ.'DP') IH2OTYPE=1
	    IF (SLH2O.EQ.'RH') IH2OTYPE=2
	    IF (SLH2O.EQ.'DE') IH2OTYPE=3
	END IF
C					convert SHIFT parameter from nanometers
C					to micrometers.
	SHIFT = SHIFT / 1000.0
C				      get radiance/temp conversion coefficients,
C				      get atmospheric radiance and transmittance
C				      get sky radiance
	CALL CENTWAV(IDATE,CWAVE)
	DO I=1,6
	    CWAVE(I) = CWAVE(I) + SHIFT
	END DO
C
	CALL ATMOS1(LMODEL,LTEMP,LHUMID,LOZONE,HEIGHT,DATUM,LOWFILE,
     &		   LOWTAB,WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,
     &		   SHIFT,QMOD)
C
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLADD(IOUT,'HISTORY','LBL1','Ground Radiance Image',
     &		       ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2',
     &		       'DN = milliwatts/(m*m*sr*micrometer)',
     &		       ISTAT,'FORMAT','STRING',' ')
	CALL GRADCAL(IN1,IN2,IOUT,ISL,ISS,NL,NS)
C								Close datasets
	CALL XVCLOSE(IN1,ISTAT,' ')
	CALL XVCLOSE(IN2,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE GRADCAL(IN1,IN2,IOUT,ISL,ISS,NL,NS)
C
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6)
C
	INTEGER*2 OUTBUF(638),INBUF(638),EBUF(638)
C
	ILINE = ISL
	DO I=1,NL
	    DO ICHAN=1,6
		CALL XVREAD(IN1,INBUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    ' ')
		CALL XVREAD(IN2,EBUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    ' ')
		ISAMP = ISS
		DO J=1,NS
                    REFL = 1.0 - FLOAT(EBUF(ISAMP))/10000.0
		    NUM = (INBUF(ISAMP)-RADUP(ISAMP,ICHAN))/
     &			   TRAN(ISAMP,ICHAN) - REFL*RADDOWN(ICHAN) + 0.5
		    OUTBUF(J) = MIN(32767,MAX(-32768,NUM))
		    ISAMP = ISAMP+1
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,'BAND',ICHAN,' ')
	    END DO
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE ATMOS1(IMODEL,IM1,IM2,IM3,RH1,RH2,LOWFILE,LOWTAB,
     +		 WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,SHIFT,QMOD)
C
	COMMON /LOWFIL/ LFIL,LWVLEN,LRESPSUM
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL LFIL(525,6), LWVLEN(525), LRESPSUM(6)
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6)
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	REAL ARAD1(525),TRAN1(525),SKYRAD(525),RESP(525,6),SECTH(319)
	REAL FRAD1SUM(6,2),FTRN1SUM(6,2)
	REAL THETA(2)/141.72,180.0/,HC/1.98629E-19/
	LOGICAL QMOD
	CHARACTER*80 MSG,LOWFILE,LOWTAB,LOWFILEX,LOWTABX
	EQUIVALENCE (LFIL,RESP)
C
	IF (QMOD) THEN
	    IV1 = 800
	    IV2 = 1324
	    IDV = 1
	    IFWHM = 1
	ELSE
	    IV1 = 800
	    IV2 = 1325
	    IDV = 5
	    IFWHM = 20
	END IF
C
	DO I=1,319
	    SECTH(I) = 1.0 / COS((38.34 - 0.12*I)*PI/180)
	END DO
C
C	Load response functions for the 6 TIMS bands according to the date
C	of the flightline.  Return response as a function of band and of
C	wavenumber.
C
	CALL GETLOW(IV1,IV2,IDV,NFREQ,SHIFT)
C
	DO ICHAN = 1,6
	    LRESPSUM(ICHAN)  = 0.0
	    DO IWAVE = 1,NFREQ
		LRESPSUM(ICHAN)  = LRESPSUM(ICHAN)+RESP(IWAVE,ICHAN)
	    ENDDO
	ENDDO
C
C	For each of two angles, compute integrated atmospheric radiance
C	and transmittance values which we will then use to make a linear
C	function relating the radiance and transmittance to the look
C	angle THETA (defined in terms of number of samples from nadir.)
C
	LOWFILEX = 'dummy'
	LOWTABX = LOWFILEX
C
	DO IANGLE = 1,2
C
C	Return the atmospheric radiance, transmittance, and sky radiance
C	in ARAD1, TRAN1, and SKYRAD respectively, indexed by wave number.
C
	    CALL MODTRAN_INT(IMODEL,IM1,IM2,IM3,1,RH1,RH2,THETA(IANGLE),
     &		IV1,IV2,IDV,IFWHM,ARAD1,TRAN1,SKYRAD,IANGLE-1,ML,ALTS,
     &		PRESS,TEMPS,H2OS,IH2OTYPE,LOWFILEX,LOWTABX,
     &		WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,QMOD)
	    LOWFILEX = LOWFILE
	    LOWTABX = LOWTAB
C
C	Multiply the six response functions by the atmospheric radiance
C	and transmittance functions, yielding the instrument filtered
C	values.  Let's do a stepwise integration of them at the same
C	time.
C
	    DO ICHAN = 1,6

		FRAD1SUM(ICHAN,IANGLE) = 0.0
		FTRN1SUM(ICHAN,IANGLE) = 0.0
		DO IWAVE = 1,NFREQ
		    FRAD1SUM(ICHAN,IANGLE) = FRAD1SUM(ICHAN,IANGLE)+
     &			(RESP(IWAVE,ICHAN)*ARAD1(IWAVE))
		    FTRN1SUM(ICHAN,IANGLE) = FTRN1SUM(ICHAN,IANGLE)+
     &			(RESP(IWAVE,ICHAN)*TRAN1(IWAVE))
		END DO
C
C	We normalize the sums.  In the current code, FRAD1SUM corresponds 
C	to John's RAD2, containing the atmospheric radiance as a function
C	of THETA, the look angle.  In the case of watts, the units are
C	changed from watts/cm*cm*sr to milliwatts/m*m*sr.
C
		FRAD1SUM(ICHAN,IANGLE)=(FRAD1SUM(ICHAN,IANGLE)
     &					/LRESPSUM(ICHAN))*1.0E+07
		FTRN1SUM(ICHAN,IANGLE)=FTRN1SUM(ICHAN,IANGLE)
     &					/LRESPSUM(ICHAN)
	    END DO
	END DO
C
C	Make the same calculation for Sky Radiance in the nadir-looking
C	case.
C
	DO ICHAN=1,6
	    SUM = 0.0
	    DO IWAVE=1,NFREQ
		SUM = SUM+RESP(IWAVE,ICHAN)*SKYRAD(IWAVE)
	    END DO
	    SUM = 1.0E7*SUM/LRESPSUM(ICHAN)
	    RADDOWN(ICHAN) = SUM / 3.14159
	END DO
C
C	Report the results
C
	IF (SHIFT .NE. 0.0) THEN
	    WRITE (MSG,400) NINT(1000.0*SHIFT)
  400	    FORMAT (' Wavelength Calibration has been shifted by',I5,
     &		    ' Nanometers')
	    CALL XVMESSAGE(MSG,' ')
	END IF
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(
     &'          Transmittance      Path  Radiance      Sky Radiance',
     &' ')
	CALL XVMESSAGE(
     &'                             (mW/m**2/sr/u)       (mW/m**2/u)',
     &' ')
	CALL XVMESSAGE(
     &' Band   Nadir   Off-nadir   Nadir   Off-nadir',' ')
	DO ICHAN=1,6
	    WRITE (MSG,500) ICHAN,FTRN1SUM(ICHAN,2),FTRN1SUM(ICHAN,1),
     &		FRAD1SUM(ICHAN,2),FRAD1SUM(ICHAN,1),RADDOWN(ICHAN)
  500	    FORMAT(I4,F10.5,F9.5,F11.1,F9.1,F15.1)
	    CALL XVMESSAGE(MSG,' ')
	END DO
C
C	Now let's get the slopes and offsets relating atmospheric radiance
C	and transmittance as a function of secant theta.
C
	DELTASEC = SECTH(1) - SECTH(319)
	DO ICHAN = 1,6
	    AR = (FRAD1SUM(ICHAN,1) - FRAD1SUM(ICHAN,2)) / DELTASEC
	    BR = FRAD1SUM(ICHAN,2) - AR
C
	    AT = (FTRN1SUM(ICHAN,1) - FTRN1SUM(ICHAN,2)) / DELTASEC
	    BT = FTRN1SUM(ICHAN,2) - AT
C
C	Load arrays with atmospheric radiance and transmittance values
C	according to its sample number.
C
	    DO ISAMP = 1,319
		RADUP(ISAMP,ICHAN) = AR*SECTH(ISAMP) + BR
		TRAN(ISAMP,ICHAN)  = AT*SECTH(ISAMP) + BT
		RADUP(639-ISAMP,ICHAN) = RADUP(ISAMP,ICHAN)
		TRAN(639-ISAMP,ICHAN)  = TRAN(ISAMP,ICHAN)
	    END DO
	END DO
	RETURN
	END
C****************************************************************************
	SUBROUTINE GETLOW(IV1,IV2,IDV,X2PTS,SHIFT)
C
C	This subroutine loads array LFIL with the band filter weights
C	according to the date on which the data were taken (in the
C	instrument's history).  The array elements coincide with the
C	wave number increments used in the MODTRAN routine, and were
C	derived through interpolation of the original response functions.
C	The latter were received in terms of wavelength.
C
	COMMON /LOWFIL/ LFIL,LWVLEN,LRESPSUM
	COMMON /RAWFIL/ RFIL,RWVLEN,ICALDATE
	INTEGER X1PTS/158/, X2PTS
	REAL*4 YDERIV(158), RMIN/1.0E-05/, TLFIL(525), LWVLEN(525)
	REAL*4 LFIL(525,6), RFIL(158,6), RWVLEN(158), LRESPSUM(6)
C
C		RWVELN and RFIL contain the original points' X's and Y's.
C		LWVLEN contains the X's for which the interpolated Y's are
C		to be found.  All arrays are indexed implicitly by the
C		element number.  In other words the F(X(4)) = Y(4).  The
C
C		Adjust the wavelengths from the response function calibration
C		tables (RWVLEN), then compute the wavelengths in the MODTRAN
C		buffers (LWVLEN).
C
	DO I=1,X1PTS
	    RWVLEN(I) = RWVLEN(I) + SHIFT
	END DO
C
	X2PTS = 1 + (IV2-IV1)/IDV
	DO I=1,X2PTS
	    WAVENUM = IV2 - IDV*(I-1)
	    LWVLEN(I) = 10000.0 / WAVENUM
	END DO
C		Read the original Y's and compute the interpolations on
C		a channel by channel (function by function) basis.
C
	DO ICHAN = 1,6
C
	    CALL SPLINE(RWVLEN,RFIL(1,ICHAN),YDERIV,X1PTS,0.,0.)
C
	    CALL SPLINT(RWVLEN,RFIL(1,ICHAN),X1PTS,YDERIV,LWVLEN,
     &			TLFIL,X2PTS)
C
	    DO I = 1,X2PTS
		IF(TLFIL(I).LT.RMIN) TLFIL(I) = 0.0
	    ENDDO
C
C		Now we flip the array to index it according to LOWTRAN
C		with index by wave number.
C
	    CALL MVE(7,X2PTS,TLFIL,LFIL(X2PTS,ICHAN),1,-1)
C
	ENDDO
C
	RETURN
	END
C
C
C****************************************************************************
	SUBROUTINE SPLINE(X,Y,Y2,N,YP1,YPN)
C
C	This subroutine was taken from Press, et al, "Numerical Recipes -
C	The Art of Scientific Computing" (1986), page 88.
C
C	Given arrays X and Y of length N containing a tabulated function,
C	i.e. Yi = f(Xi), with X1<X2<. . .<XN, and given values YP1 and YPN
C	for the first derivative of the interpolating function at points 1
C	and N, respectively, this routine returns an array Y2 of length N
C	which contains the second derivatives of the interpolating function
C	at the tabulated points Xi.  If YP1 and/or YPN are equal to 10**30
C	or larger, the routine is signalled to set the corresponding boun-
C	dary condition for a natural spline, with zero second derivative on
C	that boundary.
C
	INTEGER N
	REAL*4 X(1000), Y(1000), Y2(1000), U(1000), YP1, YPN
C
C	The lower boundary condition is set either to be "natural"
C	or else to have a specified first derivative.
C
	IF (YP1.GT..99E30) THEN
	    Y2(1)=0.
	    U(1)=0.
	ELSE
	    Y2(1)=-0.5
	    U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
	ENDIF
C
C	This is the decomposition loop of the tridiagonal algorithm.
C	Y2 and U are used for temporary storage of the decomposed
C	factors.
C
	DO I = 2,N-1
	    SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
	    P=SIG*Y2(I-1)+2.
	    Y2(I)=(SIG-1.)/P
	    U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     &		/(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
	ENDDO
C
C	The upper boundary condition is set either to be "natural"
C	or else to have a specified first derivative.
C
	IF (YPN.GT..99E30) THEN
	    QN=0.
	    UN=0.
	ELSE
	    QN=0.5
	    UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
	ENDIF
C
	Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
C
C	This is the back substitution loop of the tridiagonal algorithm.
C
	DO K = N-1,1,-1
	    Y2(K)=Y2(K)*Y2(K+1)+U(K)
	ENDDO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE SPLINT(XA,YA,N,Y2A,X,Y,NPTS)
C
C	This subroutine was taken from Press, et al, "Numerical Recipes -
C	The Art of Scientific Computing" (1986), page 89.
C
C	Given the arrays XA and YA of length N, which tabulate a function
C	(with the XAi's in order), and given the array Y2A, which is the
C	output from SPLINE above, and given a value of X, this routine
C	returns a cubic-spline interpolated value Y.
C
	REAL*4 XA(1000), YA(1000), X(1000), Y(1000), Y2A(1000)
C
C	We will find the right place in the table by means of bisection.
C	This is optimal if sequential calls to this routine are at random
C	values of X.  If sequential calls are in order, and closely spaced
C	one would do better to store previous values of KLO and KHI and
C	test if they remain appropriate on the next call.
C
	DO J = 1,NPTS
C
	    KLO=1
	    KHI=N
C
  1	    IF (KHI-KLO.GT.1) THEN
		K=(KHI+KLO)/2
		IF(XA(K).GT.X(J)) THEN
		    KHI=K
		ELSE
		    KLO=K
		ENDIF
	    GOTO 1
	    ENDIF
C
	    H=XA(KHI)-XA(KLO)
	    IF (H.EQ.0) THEN
		CALL XVMESSAGE(' Check input values for X',' ')
		CALL ABEND
	    ENDIF
C
	    A=(XA(KHI)-X(J))/H
	    B=(X(J)-XA(KLO))/H
	    Y(J)=A*YA(KLO)+B*YA(KHI)+((A**3-A)*Y2A(KLO)+(B**3-B)
     &		*Y2A(KHI))*(H**2)/6.
C
	ENDDO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE MODTRAN_INT(MODEL,M1,M2,M3,IHAZE,H1,H2,ANGLE,IV1,IV2,
     +                         IDV,IFWHM,PATHRAD,TRANSMIT,SKYRAD,IPRINT,
     +                         LAYERS,ALT,PRESS,TEMP,WATER,MTYPE,
     +                         LOWFILE,LOWTAB,WATERFAC,CO2FAC,O3FAC,
     +                         O2FAC,CH4FAC,SO2FAC,QMOD)
C
C	Interface to the LOWTRAN7 subroutine.
C	If IPRINT<>0, a listing of the parameters is also produced.
C
c	INCLUDE 'mod3files'
	REAL WAVELEN(525),TRANSMIT(*),PATHRAD(*),SKYRAD(*)
	REAL ALT(61),PRESS(61),TEMP(61),WATER(61),AHAZE(61)
        REAL EQLWCZ(61),RRATZ(61),AWCCON(4),VX(47),WAVLEN(47)
        REAL EXTC(4,47),ABSC(4,47),ASYM(4,47),ANGF(50),F(4,50)
	REAL TITLE2D(18,4),SPECALB(8000)
	REAL WMOL(12,61)/732*0.0/
	REAL WMOLX(13,61)/793*0.0/
	INTEGER IHA1(61),ICLD1(61),IVUL1(61),ISEA1(61),ICHR1(61)
	INTEGER IREG(4)
	LOGICAL QMOD
	CHARACTER*80 BUF,LOWFILE,LOWTAB
	CHARACTER*80 MODLIB,SUN,DIRAC,UFTAPX
	CHARACTER*4 TITLEX(16)/'User',' Sup','plie','d Mo','del ',
     +                         11*'    '/
	CHARACTER*1 JCHAR(15,61)/915*' '/
	CHARACTER*1 JCHARX(61)/61*' '/
	CHARACTER*1 SYM(3)/'G','H','D'/
	CHARACTER*1 CODE/'T'/
C
	CHARACTER*44 MODEL_NAME(7)
     +      /' TROPICAL TEMPERATURE AND PRESSURE          ',
     +       ' MIDLATITUDE SUMMER TEMPERATURE AND PRESSURE',
     +       ' MIDLATITUDE WINTER TEMPERATURE AND PRESSURE',
     +       ' SUBARCTIC SUMMER TEMPERATURE AND PRESSURE  ',
     +       ' SUBARCTIC WINTER TEMPERATURE AND PRESSURE  ',
     +       ' 1962 U.S. STANDARD ATMOSPHERE              ',
     +       ' USER SPECIFIED ATMOSPHERIC MODEL           '/
C
	CHARACTER*50 M2_NAME(6)
     +      /' TROPICAL WATER VAPOR PROFILE                     ',
     +       ' MIDLATITUDE SUMMER WATER VAPOR PROFILE           ',
     +       ' MIDLATITUDE WINTER WATER VAPOR PROFILE           ',
     +       ' SUBARCTIC SUMMER WATER VAPOR PROFILE             ',
     +       ' SUBARCTIC WINTER WATER VAPOR PROFILE             ',
     +       ' 1962 U.S. STANDARD ATMOSPHERE WATER VAPOR PROFILE'/
C
	CHARACTER*44 M3_NAME(6)
     +      /' TROPICAL OZONE PROFILE                     ',
     +       ' MIDLATITUDE SUMMER OZONE PROFILE           ',
     +       ' MIDLATITUDE WINTER OZONE PROFILE           ',
     +       ' SUBARCTIC SUMMER OZONE PROFILE             ',
     +       ' SUBARCTIC WINTER OZONE PROFILE             ',
     +       ' 1962 US STANDARD ATMOS WATER VAPOR PROFILE '/
C
	CHARACTER*43 IHAZE_NAME(9)
     +	    /' RURAL AEROSOLS, VISIBILITY - 23 KM.       ',
     +       ' RURAL AEROSOLS, VISIBILITY - 5 KM.        ',
     +       ' NAVY MARITIME AEROSOL ATTENUATION         ',
     +       ' MARITIME AEROSOLS, VISIBILITY - 23 KM.    ',
     +       ' URBAN AEROSOLS, VISIBILITY - 5 KM.        ',
     +       ' TROPOSPHERIC AEROSOLS, VISIBILITY - 50 KM.',
     +       ' INVALID AEROSOL OPTION                    ',
     +       ' ADVECTION FOG, VISIBILITY - 0.2 KM.       ',
     +       ' RADIATION FOG, VISIBILITY - 0.5 KM.       '/
C
 	IF (MODEL .EQ. 7) THEN
 	    IF (M3 .EQ. 0) M3=6
 	    M4 = 6
 	    M5 = 6
 	    M6 = 6
 	    MDEF = 1
 	ELSE
 	    IF (M2 .EQ. 0) M2=MODEL
 	    IF (M3 .EQ. 0) M3=MODEL
 	    M4 = MODEL
 	    M5 = MODEL
 	    M6 = MODEL
 	    MDEF = 0
 	END IF
C
	IF (IPRINT.GT.0) THEN
	    CALL XVMESSAGE(' ',' ')
	    IF (QMOD) THEN
		CALL XVMESSAGE('       ***MODTRAN PARAMETERS***',' ')
	    ELSE
		CALL XVMESSAGE('       ***LOWTRAN PARAMETERS***',' ')
	    END IF
	    IF (M1.EQ.0) THEN
		CALL XVMESSAGE(MODEL_NAME(MODEL),' ')
	    ELSE
		CALL XVMESSAGE(MODEL_NAME(M1),' ')
	    END IF
	    IF (MODEL.EQ.7) THEN
		IF (MTYPE.EQ.1) THEN
		    CALL XVMESSAGE(
     +			'   ALTITUDE  PRESSURE    TEMP    DEWPOINT',' ')
		    CALL XVMESSAGE( 
     +			'     (KM.)     (MB.)   (DEG. C)  (DEG. C)',' ')
		ELSE IF (MTYPE.EQ.2) THEN
		    CALL XVMESSAGE(
     +			'   ALTITUDE  PRESSURE    TEMP    HUMIDITY',' ')
		    CALL XVMESSAGE(
     +			'     (KM.)     (MB.)   (DEG. C)     (%)  ',' ')
		ELSE
		    CALL XVMESSAGE(
     +			'   ALTITUDE  PRESSURE    TEMP   VAPOR DEN',' ')
		    CALL XVMESSAGE(
     +			'     (KM.)     (MB.)   (DEG. C) (GM/M**3)',' ')
		END IF
		DO I=1,LAYERS
		    WRITE (BUF,100) ALT(I),PRESS(I),TEMP(I),WATER(I)
  100		    FORMAT(F10.3,3F10.2)
		    CALL XVMESSAGE(BUF,' ')
		END DO
	    ELSE
		CALL XVMESSAGE(M2_NAME(M2),' ')
		CALL XVMESSAGE(M3_NAME(M3),' ')
	    END IF
C
	    WRITE (BUF,110) WATERFAC
  110	    FORMAT(' Water Rescaling Factor =',F6.3)
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,120) O3FAC
  120	    FORMAT(' Ozone Rescaling Factor =',F6.3)
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,130) CO2FAC
  130	    FORMAT(' Carbon Dioxide Rescaling Factor =',F6.3)
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,140) SO2FAC
  140	    FORMAT(' Sulfur Dioxide Rescaling Factor =',F6.3)
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,150) CH4FAC
  150	    FORMAT(' Methane Rescaling Factor =',F6.3)
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,160) O2FAC
  160	    FORMAT(' Diatomic Oxygen Rescaling Factor =',F6.3)
	    CALL XVMESSAGE(BUF,' ')
C
	    IF (IHAZE.GT.0) CALL XVMESSAGE(IHAZE_NAME(IHAZE),' ')
C
	    WRITE (BUF,200) H1
  200	    FORMAT(' Sensor Altitude =',F9.3,' km')
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,210) H2
  210	    FORMAT(' Target Altitude =',F9.3,' km')
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,300) ANGLE
  300	    FORMAT(' Angle =',F7.2,' Degrees')
	    CALL XVMESSAGE(BUF,' ')
C
	    WRITE (BUF,400) IV1,IV2,IDV
  400	    FORMAT(' Spectral Range =',I6,' to',I6,' with',I3,
     +		   ' cm-1 Step Size')
	    CALL XVMESSAGE(BUF,' ')
	END IF
C					JCHAR gives units codes for pressure,
C					temperature, and moisture. WMOL(1,I)
C					is the final home for moisture content.
	DO I=1,LAYERS
	    WMOL(1,I) = WATER(I)
	    JCHAR(1,I) = 'A'
	    JCHAR(2,I) = 'B'
	    JCHAR(3,I) = SYM(MTYPE)
	END DO
C					Make albedo=0; this is probably
C					unnecessary -- but just to be safe.
	DO I=1,IV2
	    SPECALB(I) = 0.0
	END DO
C
	IF (MODEL .EQ. 7) THEN
	    IM = 1
	ELSE
	    IM = 0
	END IF
C
	IF (.NOT.QMOD) CODE = 'F'
	IEMSCT = 1
	ITYPE = 2
	CO2MX = 330.0 * CO2FAC
C						generate correct input filenames
        CALL XGETENV_VIC('VICARMODTRAN35',MODLIB)
        SUN = MODLIB(1:LNBLNK(MODLIB)) // '/sun3'
        DIRAC = MODLIB(1:LNBLNK(MODLIB)) // '/MOLBMP96.BIN'
        UFTAPX = MODLIB(1:LNBLNK(MODLIB)) // '/CFCBMP96.ASC'
C
	CALL MODTRAN3(LOWFILE,LOWTAB,PLTABDEF,SUN,DIRAC,UFTAPX,
     +                WATERFAC,O3FAC,O2FAC,CH4FAC,SO2FAC,0.0,
     +		      CODE,MODEL,ITYPE,IEMSCT,1,M1,M2,M3,M4,M5,M6,MDEF,
     +                    IM,0,0.0,SPECALB,
     +                .FALSE.,0,.TRUE.,5,CO2MX,
     +		      IHAZE,0,0,1,0,0,0.0,0.0,0.0,0.0,H2,
     +		      H1,H2,ANGLE,0.0,0.0,0.0,0,0.0,
     +                IV1,IV2,IDV,IFWHM,' ',' ',' ',
     +                0.0,0.0,0.0,0,0,0,0.0,0.0,0.0,0.0,0.0,0.0,
     +                0.0,0.0,0.0,
     +		      LAYERS,0,0,TITLEX,
     +		      ALT,PRESS,TEMP,WMOL,WMOLX,JCHAR,JCHARX,
     +		      AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR1,
     +		      IREG,AWCCON,TITLE2D,WAVLEN,VX,EXTC,ABSC,ASYM,
     +                ZCLD,CLD,CLDICE,RR,
     +                0,0,0,0,
     +                0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,
     +                ANGF,F,WAVELEN,TRANSMIT,PATHRAD,SKYRAD)
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timscal2re.pdf
Process help=*
parm  INP	(string,40) count=2
parm  OUT	(string,40)
parm  SIZE	integer	default=(1,1,0,0)	count=4
parm  SL	integer	default=1
parm  SS	integer	default=1
parm  NL	integer	default=0
parm  NS	integer	default=0
parm  ORG       keyword valid=(BIL,BSQ)  default=BIL
parm  DATE	integer default=-1
parm  HEIGHT	real	default=10.0
parm  DATUM	real	default=0.001
parm  AMODEL	string	valid=(TR,MS,MW,SS,SW,ST,RS)
parm  ATEMP	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  AHUMID	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  AOZONE	string	default="  "	valid=(TR,MS,MW,SS,SW,ST,"  ")
parm  NLAYERS	integer	default=0
parm  WTYPE	keyword	default=RH	valid=(DP,RH,DE)
parm  ALTITUDE	real	count=0:61	default=0.0
parm  PRESSURE	real	count=0:61	default=0.0
parm  TEMP	real	count=0:61	default=0.0
parm  WATER	real	count=0:61	default=0.0
parm  WATERFAC  real                    default=1.0
parm  O3FAC	real	                default=1.0
parm  CO2FAC	real	                default=1.0
parm  SO2FAC	real	                default=1.0
parm  CH4FAC	real	                default=1.0
parm  O2FAC	real	                default=1.0
parm  LOWFILE   (string,64) count=0:1   default=--
parm  LOWTAB    (string,64) count=0:1   default=--
parm  ATMODEL   keyword default=MODTRAN valid=(LOWTRAN,MODTRAN)
parm  SHIFT     real                    default=0.0
End-proc

.TITLE
TAE PROCESS TIMSCAL2RE
.HELP
PURPOSE:

   TIMSCAL2RE is a program which calibrates TIMS data to ground radiance.  It
is similar to TIMSCAL2, except that it uses an input spectral emissivity
image, rather than a single emissivity spectrum, and that the only output
option is ground radiance.  The input is TIMS data which have already been 
calibrated to instrument radiance (in watts/...) with the program TIMSCAL,
plus a six channel emissivity image in the format produced by the TIMS suite
of programs.  The output is a data set of all six bands in two byte integer.

EXECUTION:

   The following is the execution statement format for TIMSCAL2RE:

	TIMSCAL2RE INP=(PIX,EMIS) OUT=CALPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 



OPERATION:

   TIMSCAL2RE takes as input the output dataset of TIMSCAL (IRAD mode, WATTS
units), plus an emissivity image in the format produced by TIMSEMIS or TIMSCAL2.
It uses LOWTRAN7 or MODTRAN (user selectable) to compute the 
atmospheric path radiance, transmittance, and ground reflected atmospheric 
radiance. These values are convolved with the TIMS instrument functions and 
the instrument perceived radiances to obtain ground radiance.  The formula 
used to compute ground radiance is:

        IRAD = [GRAD + (1.0-e)*L   ]*Trans + L
                                sky           path

   The units output for radiance images are in milliwatts/m**2/um/sr.
Output is in halfword (two byte signed integer) format. 


WRITTEN BY:  Ron Alley   July, 1995
             (based upon TIMSCAL2, origially written by  J. H. REIMER 1982)

COGNIZANT PROGRAMMER:  Ron Alley

REVISION: 1  1/18/96  rea  - Update to use MODTRAN3

.LEVEL1
.VARI INP
(1) TIMS calibrated image (IRAD)
(2) TIMS emissivity image
.VARI OUT
Output data set of all six bands
of calibrated TIMS data.
.VARI SIZE
The standard  VICAR2 output size
field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI ORG
Organization of output dataset
.VARI AMODEL
Selects the model for which the
atmospheric corrections will
be performed via LOWTRAN/MODTRAN
.VARI ATEMP
Selects atmospheric temperature
and pressure profiles, if 
different form that defaulted
with AMODEL.
.VARI AHUMID
Selects the atmospheric water
vapor model, if different from
that defaulted with AMODEL.
.VARI AOZONE
Selects the ozone model, if
different from that defaulted
with AMODEL.
.VARI HEIGHT
Aircraft altitude (km above MSL)
.VARI DATUM
Surface elevation (km above MSL)
.VARI NLAYERS
The number of atmospheric lay-
ers in the radiosonde data
(if used).
.VARI ALTITUDE
The altitudes of each of the
atmospheric layers given in
the radiosonde data. (km.)
.VARI PRESSURE
The atmospheric pressures of
each of the atmospheric lay-
ers given in the radiosonde
data. (mb.)
.VARI TEMP
The temperatures of the at-
mospheric layers given in
the radiosonde data. (deg C)
.VARI WATER
The water profile in terms
of the parameter WTYPE, for
each layer in the radiosonde
data.
.VARI WTYPE
The units in which H2OS are
given.  The three types are
relative humidity (RH), dew-
point (DP), and density (DE)
.VARI WATERFAC
Varies the moisture profile
by the factor specified
.VARI O3FAC
Varies the ozone profile by
the factor specified
.VARI CO2FAC
Varies the carbon dioxide
profile by the factor specified
.VARI SO2FAC
Varies the sulfur dioxide
profile by the factor specified
.VARI CH4FAC
Varies the methane profile by
the factor specified
.VARI O2FAC
Varies the oxygen profile by
the factor specified
.VARI DATE
Date of data acquisition. Used
to override the date in the
VICAR label (yymmdd)
.VARI LOWFILE
Dataset name for LOWTRAN/MODTRAN
output report file.
.VARI LOWTAB
Dataset name for LOWTRAN/MODTRAN
output tabular file.
.VARI ATMODEL
Atmospheric model to be used
(LOWTRAN or MODTRAN)
.VARI SHIFT
Spectral shift of filter 
functions (Nanometers)
.LEVEL2
.VARI INP
The first file is the file containing all six bands of a calibrated TIMS image,
using the IRAD mode and WATTS units.  The second file is the file containing
all six bands of emissivity data, scaled such that the 0 to 1 emissivity range
is mapped to 0 to 10,000.
.VARI OUT
Output data set in 16-bit integer format of all six bands of ground radiance
TIMS data.
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
.VARI ORG
ORG specifies the organizational format of the output dataset. BIL (the default)
and BSQ are supported.  
.VARI AMODEL
	This selects the model atmosphere for which the atmospheric cor-
	rections will be computed.  The program uses a version of LOWTRAN
	(or MODTRAN) as a subroutine, to which AMODEL is passed.  The exact 
        parameters of each model is given in the LOWTRAN handbook.  The 
        options in brief:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
                RS - User supplied (e.g. Radiosonde) model atmosphere
	These same abbreviations are used with the HUMID parameter, with
	the same meanings.
		Example: LMODEL = SW
.VARI ATEMP
The ATEMP parameter is used to modify the AMODEL given, with respect to 
its temperature and pressure profiles.  The choices are the same as those 
given in the AMODEL description and with the same meanings.  The ATEMP 
parameter will only change AMODEL's temperature and pressure profiles and 
nothing else.  Here are the valid strings:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
		Example: ATEMP = MW
.VARI AHUMID
	The AHUMID parameter is used to modify the AMODEL given, with
	respect to its water vapor content.  The choices are the same
	as those given in the AMODEL description and with the same
	meanings.  The AHUMID parameter will only change AMODEL's water
	vapor profile and nothing else.  Here are the valid strings:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
		Example: HUMID = MW
.VARI AOZONE
	The AOZONE parameter is used to modify the AMODEL given, with
	respect to its ozone concentrations. The choices are the same
	as those given in the AMODEL description and with the same
	meanings.  The AOZONE parameter will only change AMODEL's ozone
	concentration profile and nothing else.  Here are the valid strings:
		TR - TRopical model atmosphere
		MS - Midlatitude Summer
		MW - Midlatitude Winter
		SS - Subarctic Summer
		SW - Subarctic Winter
		ST - U.S. STandard
		Example: HUMID = MW
.VARI HEIGHT
	Variable HEIGHT gives the altitude above mean sea level of the
	TIMS aircraft as it scanned the particular flight line.  The
	units of HEIGHT are kilometers.  A default value of 10 kilometers
	is used as a nominal approximation of a typical TIMS overflight.
		Example: HEIGHT = 12.5
.VARI DATUM
	The DATUM is the mean elevation in kilometers of the targeted
	terrain	above mean sea level.  A default value of 1 meter is
	used if the user does not specify the DATUM explicitly.  Again,
	this variable is used only in the MODE = GRAD, GTEM or EMIS cases.
		Example: DATUM = 0.2
.VARI NLAYERS
If the atmospheric model to be used is user-defined (AMODEL=RS), NLAYERS
specifies the number of altitudes for which the atmospheric parameters are
expressed. NLAYERS has a minimum of 2 and a maximum of 61. It is not used
if one of the standard LOWTRAN/MODTRAN models is selected.
.VARI ALTITUDE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the altitudes of each of the NLAYERS of atmospheric 
layers given in the radiosonde data. The proper units are kilometers.
.VARI PRESSURE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the pressure (in millibars) of each of the NLAYERS 
of atmospheric layers given in the radiosonde data. 
.VARI TEMP
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the temperature (in degrees Celsius) of each of the
NLAYERS of atmospheric layers given in the radiosonde data.
.VARI WATER
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the moisture content of each of the NLAYERS of 
atmospheric layers given in the radiosonde data. It may be specified in units
of dewpoint temperature, grams per cubic meter, or relative humidity, as 
indicated by the WTYPE parameter.
.VARI WTYPE
This parameter is used only if the atmospheric model to be used is user-defined
(AMODEL=RS). It specifies the units used by the WATER parameter to denote
moisture content. The valid values are:
                       RH for relative humidity (percent of sauration)
                       DP for dewpoint temperature (degrees Celsius)
                   and DE for density (grams per cubic meter)
.VARI O3FAC
	Used to vary the impact of the ozone model in use. All ozone
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI O2FAC
	Used to vary the impact of the oxygen model in use. All oxygen
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI CO2FAC
	Used to vary the impact of the carbon dioxide model in use. All
        carbon dioxide concentrations are multiplied by this factor prior to 
        atmospheric parameter computation.
.VARI SO2FAC
	Used to vary the impact of the sulfur dioxide model in use. All
        sulfur dioxide concentrations are multiplied by this factor prior to 
        atmospheric parameter computation.
.VARI CH4FAC
	Used to vary the impact of the methane model in use. All methane
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI WATERFAC
	Used to vary the impact of the moisture profile in use. All moisture
        concentrations are multiplied by this factor prior to atmospheric
        parameter computation.
.VARI DATE
	TIMSCAL2RE uses the date of data acquisition to determine the proper
	calibration coefficients.  If defaulted, the date in the VICAR 
	label is used. This parameter is needed only if the VICAR label
	is incorrect, or if an abnormal calibration set is to be used.
.VARI LOWFILE
   Normally, the report file generated by LOWTRAN (or MODTRAN) is discarded 
   at the conclusion of the LOWTRAN/MODTRAN run.  If a dataset name is given
   as the parameter value to LOWFILE, the report for the nadir looking case
   of LOWTRAN/MODTRAN is saved in that dataset.
.VARI LOWTAB
   Normally, the tabular file generated by LOWTRAN (or MODTRAN) is discarded 
   at the conclusion of the LOWTRAN/MODTRAN run.  If a dataset name is given
   as the parameter value to LOWTAB, the report for the nadir looking case
   of LOWTRAN/MODTRAN is saved in that dataset.
.VARI ATMODEL
   This parameter selects the particular atmospheric modelling program to be
   used in the computation.  The choices are LOWTRAN (LOWTRAN7) or MODTRAN
   (MODTRAN3).
.VARI SHIFT
   This parameter is used to adjust the wavelength calibration of the TIMS
   filter functions.  This value (in nanometers) is added to the wavelengths
   in the table of spectral response functions.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create timscal2re.imake
#define  PROGRAM   timscal2re

#define MODULE_LIST timscal2re.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN
/* #define FTNINC_LIST mod3files */

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
#define LIB_MOD35
$ Return
$!#############################################################################
