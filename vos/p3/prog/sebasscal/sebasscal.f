	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C       Jul 2, 2001   ...rea...  Update MODTRAN3 argument list
C
	COMMON /ATMPRM/ RAD,TRANS,SKY,EMIS
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL RAD(128),TRANS(128),SKY(128),WVLEN(128)
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	CHARACTER*2 SLMODEL, SLHUMID, SLOZONE, SLH2O, SLTEMP
	CHARACTER*80 MODFILE,MODTAB,RSTABLE
	CHARACTER*10 TASK/' '/
	LOGICAL XVPTST,GRAD,GTEM,BTEM
C						Open datasets, get size field
	CALL XVEACTION('SA',' ')
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'U_FORMAT','HALF','O_FORMAT','HALF',
     &	           'OP','WRITE','U_ORG','BSQ','U_NL',NL,'U_NS',NS,' ')
C
C						Get the other parameters
	GTEM = XVPTST('GTEM')
	BTEM = XVPTST('BTEM')
	GRAD = .NOT. (GTEM.OR.BTEM)
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
	CALL XVPARM('E',EMIS,NUM,IDEF,0)
	CALL XVPARM('DATE',IDATE,NUM,IDEF,0)
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
C							report files
	CALL XVPARM('MODFILE',MODFILE,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODFILE='dummy'
	CALL XVPARM('MODTAB',MODTAB,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODTAB='dummy'
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
C						get central wavelengths
	CALL GET_SEBASS_WAVLEN(IDATE,WVLEN)
C
C				      get atmospheric radiance and transmittance
C				      get sky radiance
C
	CALL ATMOS1(LMODEL,LTEMP,LHUMID,LOZONE,HEIGHT,DATUM,MODFILE,
     &		 MODTAB,WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,WVLEN)
C
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
     &		 'DN = 100*Degrees Celsius',ISTAT,'FORMAT','STRING',' ')
	    CALL GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,WVLEN)
C									BTEM
	ELSE
	    CALL XLADD(IOUT,'HISTORY','LBL1',
     &		'Brightness Temperature Image',ISTAT,
     &		'FORMAT','STRING',' ')
	    CALL XLADD(IOUT,'HISTORY','LBL2',
     &		 'DN = 100*Degrees Celsius',ISTAT,'FORMAT','STRING',' ')
	    EMIS = 1.0
	    CALL GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,WVLEN)
	ENDIF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE GRADCAL(IN,IOUT,ISL,ISS,NL,NS)
C
	COMMON /ATMPRM/ RAD,TRANS,SKY,EMIS
	REAL RAD(128),TRANS(128),SKY(128)
C
	REAL BUF(128)
	INTEGER*2 OBUF(128)
C
	DO ICHAN=1,128
	    ILINE = ISL
	    DO I=1,NL
		CALL XVREAD(IN,BUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO J=1,NS
		    X = ((BUF(J)-RAD(ICHAN))/TRANS(ICHAN)) - SKY(ICHAN)
		    OBUF(J) = MIN(32767,MAX(-32768,NINT(X)))
		END DO
		CALL XVWRIT(IOUT,OBUF,ISTAT,' ')
		ILINE = ILINE+1
	    END DO
	END DO
	RETURN
	END
C***********************************************************************
	SUBROUTINE GTEMCAL(IN,IOUT,ISL,ISS,NL,NS,WVLEN)
C
	COMMON /ATMPRM/ RAD,TRANS,SKY,EMIS
	REAL RAD(128),TRANS(128),SKY(128)
C
	REAL BUF(128),WVLEN(128)
	INTEGER*2 OBUF(128)
C
	DO ICHAN=1,128
	    ILINE = ISL
	    DO I=1,NL
		CALL XVREAD(IN,BUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     &			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO J=1,NS
		    X = ((BUF(J)-RAD(ICHAN))/TRANS(ICHAN)) - SKY(ICHAN)
		    Y = 100.0*(PLKINV(WVLEN(ICHAN),0.001*X/EMIS)-273.15)
		    OBUF(J) = MIN(32767,MAX(-32768,NINT(Y)))
		END DO
		CALL XVWRIT(IOUT,OBUF,ISTAT,' ')
		ILINE = ILINE+1
	    END DO
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE ATMOS1(IMODEL,IM1,IM2,IM3,RH1,RH2,MODFILE,MODTAB,
     +	      WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC,WVLEN)
C
	COMMON /ATMPRM/ RAD,TRANS,SKY,EMIS
	COMMON /RADIO/ ML,ALTS,PRESS,TEMPS,H2OS,IH2OTYPE
	REAL RAD(128),TRANS(128),SKY(128)
	REAL ALTS(61),PRESS(61),TEMPS(61),H2OS(61)
C
	REAL ARAD1(750),TRAN1(750),SKYRAD(750),WVLEN(128),WVNUM(0:129)
	CHARACTER*80 MSG,MODFILE,MODTAB
C
	IV1 = 701
	IV2 = 1450
	IDV = 1
	IFWHM = 1
C
C	Return the atmospheric radiance, transmittance, and sky radiance
C	in ARAD1, TRAN1, and SKYRAD respectively, indexed by wave number.
C
	    CALL MODTRAN_INT(IMODEL,IM1,IM2,IM3,1,RH1,RH2,180.0,
     &		IV1,IV2,IDV,IFWHM,ARAD1,TRAN1,SKYRAD,1,ML,ALTS,
     &		PRESS,TEMPS,H2OS,IH2OTYPE,MODFILE,MODTAB,
     &		WATERFAC,CO2FAC,O3FAC,O2FAC,CH4FAC,SO2FAC)
C
C	Fill in the wavenumber array
C
	DO ICHAN=1,128
	    WVNUM(ICHAN) = 10000.0 / WVLEN(ICHAN)
	END DO
	WVNUM(0) = 2.0*WVNUM(1) - WVNUM(2)
	WVNUM(129) = 2.0*WVNUM(128) - WVNUM(127)
C
C	Compute RAD, TRANS, and SKY for each of the 128 channels
C	by applying a box filter function to RAD1, TRAN1, and SKYRAD
C
	DO ICHAN = 1,128
	    WVNUM_LOW = (WVNUM(ICHAN) + WVNUM(ICHAN+1)) / 2.0
	    WVNUM_HIGH = (WVNUM(ICHAN) + WVNUM(ICHAN-1)) / 2.0
	    IWVNUM = NINT(WVNUM_LOW)
	    INDEX = IWVNUM - 700
C						 low wavenumber fractional part
	    FRACTION = IWVNUM - WVNUM_LOW + 0.5
	    RAD(ICHAN) = FRACTION*ARAD1(INDEX)
            TRANS(ICHAN) = FRACTION*TRAN1(INDEX)
	    SKY(ICHAN) = FRACTION*SKYRAD(INDEX)
	    WEIGHT = FRACTION
C					     wavenumbers that are fully included
	    IWVNUM = IWVNUM + 1
	    INDEX = INDEX + 1
	    DO WHILE (IWVNUM .LE. WVNUM_HIGH-0.5)
		RAD(ICHAN) = RAD(ICHAN) + ARAD1(INDEX)
		TRANS(ICHAN) = TRANS(ICHAN) + TRAN1(INDEX)
		SKY(ICHAN) = SKY(ICHAN) + SKYRAD(INDEX)
		WEIGHT = WEIGHT + 1.0
		IWVNUM = IWVNUM + 1
		INDEX = INDEX + 1
	    END DO
C						high wavenumber fractional part
	    FRACTION = WVNUM_HIGH - IWVNUM + 0.5
	    RAD(ICHAN) = RAD(ICHAN) + FRACTION*ARAD1(INDEX)
            TRANS(ICHAN) = TRANS(ICHAN) + FRACTION*TRAN1(INDEX)
	    SKY(ICHAN) = SKY(ICHAN) + FRACTION*SKYRAD(INDEX)
	    WEIGHT = WEIGHT + FRACTION
C
C	Normalize the sums.  Convert radiance terms from watts/(cm*cm*sr*u)
C	to milliwatts/(m*m*sr*u). For sky radiance term, factor in the
C	surface reflectance and pi terms.
C
	    RAD(ICHAN) = 1.0E+07 * RAD(ICHAN) / WEIGHT
	    TRANS(ICHAN) = TRANS(ICHAN) /WEIGHT
	    SKY(ICHAN) = 1.0E+07 * SKY(ICHAN) / WEIGHT
	    SKY(ICHAN) = SKY(ICHAN) * (1.0-EMIS) / 3.14159
	END DO
C
C	Report the results
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(
     &' Band  Wavelength Transmittance  Path  Radiance  Sky Radiance',
     &' ')
	CALL XVMESSAGE(
     &'          (u)                    (mW/m**2/sr/u) (mW/m**2/sr/u)',
     &' ')
	DO ICHAN=1,128
	    WRITE (MSG,500) ICHAN,WVLEN(ICHAN),TRANS(ICHAN),RAD(ICHAN),
     &			    SKY(ICHAN)
  500	    FORMAT(I4,F11.4,F13.5,F15.1,F13.1)
	    CALL XVMESSAGE(MSG,' ')
	END DO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE MODTRAN_INT(MODEL,M1,M2,M3,IHAZE,H1,H2,ANGLE,IV1,IV2,
     +                         IDV,IFWHM,PATHRAD,TRANSMIT,SKYRAD,IPRINT,
     +                         LAYERS,ALT,PRESS,TEMP,WATER,MTYPE,
     +                         MODFILE,MODTAB,WATERFAC,CO2FAC,O3FAC,
     +                         O2FAC,CH4FAC,SO2FAC)
C
C	Interface to the MODTRAN subroutine.
C	If IPRINT<>0, a listing of the parameters is also produced.
C
ccc	INCLUDE 'mod3files'	! not available.  rgd 3/2010
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
	CHARACTER*80 BUF,MODFILE,MODTAB
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
	    CALL XVMESSAGE('       ***MODTRAN PARAMETERS***',' ')
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
	IEMSCT = 1
	ITYPE = 2
	CO2MX = 330.0 * CO2FAC
C						generate correct input filenames
        CALL XGETENV_VIC('VICARMODTRAN35',MODLIB)
        SUN = MODLIB(1:LNBLNK(MODLIB)) // '/sun3'
        DIRAC = MODLIB(1:LNBLNK(MODLIB)) // '/MOLBMP96.BIN'
        UFTAPX = MODLIB(1:LNBLNK(MODLIB)) // '/CFCBMP96.ASC'
C
	CALL MODTRAN3(MODFILE,MODTAB,PLTABDEF,SUN,DIRAC,UFTAPX,
     +                WATERFAC,O3FAC,O2FAC,CH4FAC,SO2FAC,0.0,
     +		      'T',MODEL,ITYPE,IEMSCT,1,M1,M2,M3,M4,M5,M6,
     +                    MDEF,IM,0,0.0,SPECALB,
     +                .FALSE.,0,.TRUE.,5,CO2MX,
     +		      IHAZE,0,0,1,0,0,0.0,0.0,0.0,0.0,H2,
     +		      H1,H2,ANGLE,0.0,0.0,0.0,0,0.0,
     +                IV1,IV2,IDV,IFWHM,' ',' ',' ',
     +                0.0,0.0,0.0,0,0,0,0.0,0.0,0.0,0.0,0.0,0.0,
     +                0.0,0.0,0.0,
     +		      LAYERS,0,0,TITLEX,
     +		      ALT,PRESS,TEMP,WMOL,WMOLX,JCHAR,JCHARX,
     +		     AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR1,
     +		     IREG,AWCCON,TITLE2D,WAVLEN,VX,EXTC,ABSC,ASYM,
     +               ZCLD,CLD,CLDICE,RR,
     +		     0,0,0,0,
     +		     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     +		     0,ANGF,F,WAVELEN,TRANSMIT,PATHRAD,SKYRAD)
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
