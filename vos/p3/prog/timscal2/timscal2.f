	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C       Jul 2, 2001   ...rea...  Update MODTRAN3 argument list
C
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN,E
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	COMMON /RAWFIL/ FILTER,WVLEN,ICALDATE
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6),E(6)
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
	REAL FILTER(158,6),WVLEN(158)
C
	INTEGER*2 RAD_LUT(40000,6),TEMP_LUT(32767,6)
	CHARACTER*2 SLMODEL, SLHUMID, SLOZONE, SLH2O, SLTEMP
	CHARACTER*100 LABEL
	CHARACTER*80 LOWFILE,LOWTAB,RSTABLE
	LOGICAL XVPTST,GRAD,GTEM,EMIS,BTEM,QMOD
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
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
	GTEM = XVPTST('GTEM')
	BTEM = XVPTST('BTEM')
	EMIS = XVPTST('EMIS')
	GRAD = .NOT. (GTEM.OR.EMIS.OR.BTEM)
	CALL XVPARM('DEFL',DEFL,NUM,IDEF,0)
	CALL XVPARM('SHIFT',SHIFT,NUM,IDEF,0)
	CALL XVPARM('REFCHAN',IREF,NUM,IDEF,0)
	CALL XVPARM('FILTER',NWTS,NUM,IDEF,0)
	CALL XVPARM('AMODEL',SLMODEL,NUM,IDEF,0)
	CALL XVPARM('AHUMID',SLHUMID,NUM,IDEF,0)
	CALL XVPARM('AOZONE',SLOZONE,NUM,IDEF,0)
	CALL XVPARM('ATEMP',SLTEMP,NUM,IDEF,0)
	CALL XVPARM('HEIGHT',HEIGHT,NUM,IDEF,0)
	CALL XVPARM('DATUM',DATUM,NUM,IDEF,0)
	CALL XVPARM('WTYPE',SLH2O,NUM,IDEF,0)
	CALL XVPARM('O3FAC',O3FAC,NUM,IDEF,0)
	CALL XVPARM('O2FAC',O2FAC,NUM,IDEF,0)
	CALL XVPARM('CO2FAC',CO2FAC,NUM,IDEF,0)
	CALL XVPARM('SO2FAC',SO2FAC,NUM,IDEF,0)
	CALL XVPARM('CH4FAC',CH4FAC,NUM,IDEF,0)
	CALL XVPARM('WATERFAC',WATERFAC,NUM,IDEF,0)
	CALL XVPARM('E',E,NUM,IDEF,0)
C						if radiosonde input, get the
C						values at each layer
	IF (SLMODEL .EQ. 'RS') THEN
	    CALL XVPARM('RSTABLE',RSTABLE,NUM,IDEF,0)
	    IF (NUM .EQ. 0) THEN
		CALL XVPARM('NLAYERS',ML,NUM,IDEF,0)
		CALL XVPARM('ALTITUDE',ALTS,NUM,IDEF,0)
		CALL XVPARM('PRESSURE',PRESS,NUM,IDEF,0)
		CALL XVPARM('TEMP',TEMPS,NUM,IDEF,0)
		CALL XVPARM('WATER',H2OS,NUM,IDEF,0)
	    ELSE
		CALL READ_RSTABLE(RSTABLE)
	    END IF
	END IF
C						if necessary, convert
C						temperature profiles to Celsius
	IF (XVPTST('KELVIN') .AND. ML.GT.0) THEN
	    DO I=1,ML
		TEMPS(I) = TEMPS(I) - 273.15
	    END DO
	END IF
C						emissivities must be 1.0 for
C						brightness temperature
	IF (BTEM) THEN
	    DO I=1,6
		E(I) = 1.0
	    END DO
	END IF
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
C				      	load TIMS filter functions and LUTs
C
	IF (GRAD) THEN
	    CALL GETFIL(IDATE,0)
	    DO I=1,158
		WVLEN(I) =WVLEN(I) + SHIFT
	    END DO
	ELSE
	    CALL GET_TIMS_TEMP_LUT(IDATE,SHIFT,TEMP_LUT)
	END IF
	IF (EMIS) CALL GET_TIMS_RAD_LUT(IDATE,SHIFT,RAD_LUT)
C
C				      get atmospheric radiance and transmittance
C				      get sky radiance
C
	CALL ATMOS1(LMODEL,LTEMP,LHUMID,LOZONE,HEIGHT,DATUM,LOWFILE,
     &		   LOWTAB,WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,
     &		   SHIFT,DEFL,QMOD)
C
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','TIMSCAL',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','TIMSCAL',' ')
C									GRAD
	IF (GRAD) THEN
	    CALL XLADD(IOUT,'HISTORY','LBL1','Ground Radiance Image',
     &		       ISTAT,'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		       'DN = milliwatts/(m*m*sr*micrometer)',
     &		       ISTAT,'FORMAT','STRING',' ')
	    CALL GRADCAL(IN,IOUT,ISL,ISS,NL,NS)
C									GTEM
	ELSE IF(GTEM) THEN
	    CALL XLADD(IOUT,'HISTORY','LBL1','Ground Temperature Image',
     &			ISTAT,'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		  'DN = 100*Degrees Celsius',ISTAT,
     &		  'FORMAT','STRING',' ')
	    CALL GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,TEMP_LUT)
C									BTEM
	ELSE IF(BTEM) THEN
	    CALL XLADD(IOUT,'HISTORY','LBL1',
     &		'Brightness Temperature Image',ISTAT,
     &		'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		 'DN = 100*Degrees Celsius',ISTAT,
     &		 'FORMAT','STRING',' ')
	    DO I=1,6
		E(I) = 1.0
	    END DO
	    CALL GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,TEMP_LUT)
C									EMIS
	ELSE
	    CALL XLADD(IOUT,'HISTORY','LBL1','Emissivity Image',
     &			ISTAT,'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2','DN = 10,000*Emissivity',
     &			ISTAT,'FORMAT','STRING',' ')
	    CALL EMISCAL(IN,IOUT,ISL,ISS,NL,NS,IREF,TEMP_LUT,RAD_LUT)
	ENDIF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE GRADCAL(IN,IOUT,ISL,ISS,NL,NS)
C
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN,E
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6),E(6)
C
	INTEGER*2 OUTBUF(638),INBUF(638)
C
	ILINE = ISL
	DO I=1,NL
	    DO ICHAN=1,6
		CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    ' ')
		ISAMP = ISS
		DO J=1,NS
		    NUM = NINT((INBUF(ISAMP)-RADUP(ISAMP,ICHAN))/
     &				TRAN(ISAMP,ICHAN) - RADDOWN(ICHAN))
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
	SUBROUTINE GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,TEMP_LUT)
C
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN,E
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6),E(6)
C
	INTEGER*2 OUTBUF(638),INBUF(638),TEMP_LUT(32767,6)
C
	ILINE = ISL
	DO I=1,NL
	    DO ICHAN=1,6
		CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    ' ')
		ISAMP = ISS
		DO J=1,NS
		    NUM = NINT(((INBUF(ISAMP)-RADUP(ISAMP,ICHAN))/
     &			  TRAN(ISAMP,ICHAN) - RADDOWN(ICHAN))/E(ICHAN))
		    NUM = MIN(32767,MAX(1,NUM))
		    OUTBUF(J) = TEMP_LUT(NUM,ICHAN)
	            ISAMP = ISAMP+1
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,'BAND',ICHAN,' ')
	    END DO
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE EMISCAL(IN,IOUT,ISL,ISS,NL,NS,IREF,TEMP_LUT,RAD_LUT)
C
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN,E
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6),E(6)
C
	REAL BBRAD(638,6)
	INTEGER*2 OUTBUF(638),INBUF(638)
	INTEGER*2 TEMP_LUT(32767,6),RAD_LUT(40000,6)
C
	ILINE = ISL
	DO I=1,NL
	    CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'BAND',IREF,' ')
	    ISAMP = ISS
	    DO J=1,NS
		NUM = NINT((((INBUF(ISAMP)-RADUP(ISAMP,IREF))
     &			   /TRAN(ISAMP,IREF) - RADDOWN(IREF))/E(IREF)))
		NUM = MIN(32767,MAX(1,NUM))
		ITEMP = TEMP_LUT(NUM,IREF) + 27315
		DO ICHAN=1,6
		    BBRAD(J,ICHAN) = RAD_LUT(ITEMP,ICHAN)
		END DO
		ISAMP = ISAMP+1
	    END DO
	    DO ICHAN=1,6
		CALL XVREAD(IN,INBUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    ' ')
		ISAMP = ISS
		DO J=1,NS
		    X = (INBUF(ISAMP)-RADUP(ISAMP,ICHAN))/
     &			TRAN(ISAMP,ICHAN) - RADDOWN(ICHAN)
		    OUTBUF(J) = NINT(10000.0*X/BBRAD(J,ICHAN))
		    ISAMP = ISAMP+1
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,'LINE',I,'BAND',ICHAN,' ')
	    END DO
	    ILINE = ILINE+1
	END DO
	RETURN
	END
C****************************************************************************
	SUBROUTINE ATMOS1(IMODEL,IM1,IM2,IM3,RH1,RH2,LOWFILE,LOWTAB,
     +	      WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,SHIFT,DEFL,QMOD)
C
	INCLUDE 'pgminc'
	COMMON /PARB/PARB
	INTEGER PARB(xprdim)
C
	COMMON /LOWFIL/ LFIL,LWVLEN,LRESPSUM
	COMMON /ATMPRM/ TRAN,RADUP,RADDOWN,E
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL LFIL(525,6), LWVLEN(525), LRESPSUM(6)
	REAL TRAN(638,6),RADUP(638,6),RADDOWN(6),E(6),IRRAD(6)
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	REAL ARAD1(525),TRAN1(525),SKYRAD(525),RESP(525,6),SECTH(319)
	REAL FRAD1SUM(6,2),FTRN1SUM(6,2)
	REAL THETA(2),HC/1.98629E-19/
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
	THETA(1) = 180.0 - DEFL
	THETA(2) = 180.0
C					DEFL is max deflection of scanning
C					DELTA is angular increment per pixel
C					THETA0 is deflection of 0'th pixel
	DELTA = DEFL / 319.0
	THETA0 = DEFL + (DELTA/2.0)
	DO I=1,319
	    SECTH(I) = 1.0 / COS((THETA0 - DELTA*I)*PI/180)
	END DO
C
C	Load response functions for the 6 TIMS bands according to the date
C	of the flightline.  Return response as a function of band and of
C	wavenumber.
C
	CALL GETLOW(IV1,IV2,IDV,NFREQ)
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
	    IRRAD(ICHAN) = SUM
	    RADDOWN(ICHAN) = SUM*(1.0-E(ICHAN))/3.14159
	END DO
C							send IRRAD values
C							back to PDF
      CALL XQINI(PARB,xprdim,xcont,ISTAT)
      CALL XQREAL(PARB,'SKYRAD',6,IRRAD,xadd,ISTAT)      
      CALL XQOUT(PARB,ISTAT)
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
	CALL XVMESSAGE(     '          Transmittance      Path  Radiance
     &   Reflected Sky Radiance',' ')
	CALL XVMESSAGE(     '                             (mW/m**2/sr/u)
     &       (mW/m**2/sr/u)',' ')
	CALL XVMESSAGE(
     &' Band   Nadir   Off-nadir   Nadir   Off-nadir',' ')
	DO ICHAN=1,6
	    WRITE (MSG,500) ICHAN,FTRN1SUM(ICHAN,2),FTRN1SUM(ICHAN,1),
     &		FRAD1SUM(ICHAN,2),FRAD1SUM(ICHAN,1),RADDOWN(ICHAN)
  500	    FORMAT(I4,F10.5,F9.5,F11.1,F9.1,F17.1)
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
	SUBROUTINE GETLOW(IV1,IV2,IDV,X2PTS)
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
cccc	INCLUDE 'mod3files'
	REAL WAVELEN(525),TRANSMIT(*),PATHRAD(*),SKYRAD(*)
	REAL ALT(61),PRESS(61),TEMP(61),WATER(61),AHAZE(61)
        REAL EQLWCZ(61),RRATZ(61),AWCCON(4),VX(47),WAVLEN(47)
        REAL EXTC(4,47),ABSC(4,47),ASYM(4,47),ANGF(50),F(4,50)
	REAL TITLE2D(18,4),SPECALB(8000)
	REAL ZCLD(16),CLD(16),CLDICE(16),RR(16)
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
     +       ' 1976 U.S. STANDARD ATMOSPHERE              ',
     +       ' USER SPECIFIED ATMOSPHERIC MODEL           '/
C
	CHARACTER*50 M2_NAME(6)
     +      /' TROPICAL WATER VAPOR PROFILE                     ',
     +       ' MIDLATITUDE SUMMER WATER VAPOR PROFILE           ',
     +       ' MIDLATITUDE WINTER WATER VAPOR PROFILE           ',
     +       ' SUBARCTIC SUMMER WATER VAPOR PROFILE             ',
     +       ' SUBARCTIC WINTER WATER VAPOR PROFILE             ',
     +       ' 1976 U.S. STANDARD ATMOSPHERE WATER VAPOR PROFILE'/
C
	CHARACTER*44 M3_NAME(6)
     +      /' TROPICAL OZONE PROFILE                     ',
     +       ' MIDLATITUDE SUMMER OZONE PROFILE           ',
     +       ' MIDLATITUDE WINTER OZONE PROFILE           ',
     +       ' SUBARCTIC SUMMER OZONE PROFILE             ',
     +       ' SUBARCTIC WINTER OZONE PROFILE             ',
     +       ' 1976 U.S. STANDARD ATMOSPHERE OZONE PROFILE'/
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
	    IF (H2 .LT. ALT(1)) THEN
		CALL XVMESSAGE('The lowest atmospheric layer must',' ')
		CALL XVMESSAGE('be at least as low as the DATUM value',' ')
		CALL ABEND
	    END IF
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
     +                IV1,IV2,IDV,IFWHM,' ',' ','        ',
     +		      0.0,0.0,0.0,0,0,0,0.0,0.0,0.0,0.0,0.0,0.0,
     +		      0.0,0.0,0.0,
     +		      LAYERS,0,0,TITLEX,
     +		      ALT,PRESS,TEMP,WMOL,WMOLX,JCHAR,JCHARX,
     +		      AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR1,
     +		      IREG,AWCCON,TITLE2D,WAVLEN,VX,EXTC,ABSC,ASYM,
     +                ZCLD,CLD,CLDICE,RR,
     +		      0,0,0,0,
     +		      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,
     +		      ANGF,F,WAVELEN,TRANSMIT,PATHRAD,SKYRAD)
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READ_RSTABLE(RSTABLE)
C
C      This subroutine reads records from the file RSTABLE, and fills the
C      appropriate radiosonde arrays.  If more than 61 entries are 
C      encountered, the routine aborts.  If something other than a 
C      numeric or delimiter is encountered prior to finding four numeric
C      fields, the entire line is discarded.
C
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
	CHARACTER*64 RSTABLE
C
	OPEN (51,FILE=RSTABLE,STATUS='OLD')
	ML = 1
  100	CONTINUE
	    READ (51,*,END=900,ERR=100) ALTS(ML),PRESS(ML),TEMPS(ML),
     +					H2OS(ML)
	    IF (PRESS(ML) .LT. 0.0) PRESS(ML)=1.0E-10
	    ML = ML+1
	    IF (ML .GT. 62) THEN
		CALL XVMESSAGE('Sonde table has too many layers',' ')
		CALL ABEND
	    ENDIF
	    GO TO 100
  900	CONTINUE
	ML = ML - 1
	CLOSE (51)
	RETURN
	END
