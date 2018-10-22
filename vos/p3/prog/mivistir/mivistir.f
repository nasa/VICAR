C
C	VICAR program MIVISTIR, used to calculate upwelling radiance at
C	surface from radiance at sensor for the MIVIS instrument, thermal IR
C	channels.
C
C	Ron Alley	29 October 1998
C       Jul 2, 2001   ...rea...  Update MODTRAN3 argument list
C
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL RESP(900,10),TRANX(755,10),PATHX(755,10)
	CHARACTER*80 MODINP,MODOUT,MODTAB,INPUT_NAMES(2)
	CHARACTER*3 ORG
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INCAL,'INP',2,ISTAT,' ')
	CALL XVOPEN(INCAL,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(INLUT,'INP',3,ISTAT,' ')
	CALL XVOPEN(INLUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_FORMAT','HALF','O_FORMAT','HALF',
     +		   'U_ORG','BIL','U_NB',10,'U_NL',NL,'U_NS',NS,' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (NS .NE. 755) THEN
	    IF (NSIN .NE. 755) THEN
		CALL XVMESSAGE('Input image must be 755 pixels wide',' ')
		CALL ABEND
	    ENDIF
	    CALL XVMESSAGE('WARNING: NS parameter is ignored',' ')
	    CALL XVMESSAGE('The full image width will be processed',' ')
	END IF
	CALL XVGET(IN,ISTAT,'ORG',ORG,' ')
C							      update VICAR label
	CALL XLADD(IOUT,'HISTORY','LBL1','MIVIS Ground Radiance Image',
     +		   ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2',
     +		       'DN = milliwatts/(m*m*sr*micrometer)',
     +		       ISTAT,'FORMAT','STRING',' ')
	CALL XVPARM('INP',INPUT_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','CAL_FILE',INPUT_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C							Get the other parameters
	CALL XVPARM('MODINP',MODINP,NUM,IDEF,0)
	CALL XVPARM('MODOUT',MODOUT,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODOUT='dummy'
	CALL XVPARM('MODTAB',MODTAB,NUM,IDEF,0)
	IF (NUM .EQ. 0) MODTAB='dummy'
C						Load spectral response functions
	DO ICHAN=1,10
	    CALL XVREAD(INCAL,RESP(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INCAL,ISTAT,' ')
C						Get MODTRAN parameters from
C						MODTRAN input file
	CALL GET_MODTRAN_INPUT(MODINP)
	CALL REPORT_MODTRAN_PARMS(MODOUT,MODTAB)
C						Run MODTRAN
	CALL MODTRAN_INTERFACE(IOUT,RESP,MODOUT,MODTAB,TRANX,PATHX)
C						Do the atmospheric correction
	CALL GRADCAL(IN,INLUT,IOUT,ISL,NL,ORG,TRANX,PATHX)
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GET_MODTRAN_INPUT(MODINP)
	CHARACTER*80 MODINP
C
	PARAMETER(LAYDIM=61,NSPECX=13,NZCLD=16,NAER=7,NWAVLN=47)
        COMMON /MODPARS/H2O,O3,CH4,SO2,TEMPADJ,MODEL,M1,M2,M3,M4,M5,M6,
     +          MDEF,TBOUND,CO2MIX,IHAZE,ISEASN,IVULCN,ICSTL,VIS,WSS,
     +          WHH,H1,H2,ML,IRD1,IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,
     +          JCHARX,AHAZE,EQLWCZ,IHA1,IVUL1,ISEA1,ICHR1,IPARM,IPH,
     +          IDAY,PARM1,PARM2,PARM3,PARM4,TIME,G
	REAL ZMDL(LAYDIM),P(LAYDIM),T(LAYDIM),AHAZE(LAYDIM)
	REAL WMOL(12,LAYDIM),XMOL(NSPECX,LAYDIM),EQLWCZ(LAYDIM)
	INTEGER IHA1(LAYDIM),IVUL1(LAYDIM),ISEA1(LAYDIM),ICHR1(LAYDIM)
	CHARACTER*4 TITLE(16)
	CHARACTER*1 JCHAR(15,LAYDIM),JCHARX(LAYDIM)
C
	REAL HOLD(NSPECX),RRATZ(LAYDIM),AWCCON(4)
	REAL WAVLEN(NWAVLN),VX(NWAVLN),EXTC(NAER,NWAVLN)
	REAL ABSC(NAER,NWAVLN),ASYM(NAER,NWAVLN),ANGF(50),F(4,50)
	REAL ZCLD(NZCLD),CLD(NZCLD),CLDICE(NZCLD),RR(NZCLD)
	INTEGER ICLD1(LAYDIM),IREG(4)
	CHARACTER*8 DLIMIT
	CHARACTER*4 TITLE2D(18,4)
	CHARACTER*1 YFLAG,XFLAG,CODE
	LOGICAL LDISORT,LSUN1
C								    open dataset
	OPEN (UNIT=81,FILE=MODINP,STATUS='OLD')
C                                                                       Card 1
	READ (81,100) CODE,MODEL,ITYPE,IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,
     +		      MDEF,IM,NOPRNT,TBOUND,SALB  
  100	FORMAT(A1,I4,12I5,F8.3,F7.2)
C									Card 1A
	READ (81,120) LDISORT,ISTRM,LSUN1,ISUN,CO2MIX
  120	FORMAT(L1,I4,L1,I4,F10.3)
C                                                                       Card 2
	READ (81,200) IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,
     +		      RAINRT,GNDALT
  200	FORMAT(6I5,5F10.3)
C                                                                       Card 2A
	IF (ICLD.GE.18) READ (81,210) CTHIK,CALT,CEXT,ISEED
  210	FORMAT(3F8.3,2I4,6F8.3)
	IF (ICLD.GE.1 .AND. ICLD.LE.10) READ (81,210) CTHIK,CALT,CEXT,
     +		NCRALT,NCRSPC,CWAVLN,CCOLWD,CCOLIP,CHUMID,ASYMWD,ASYMIP
C                                                                       Card 2B
	IF (IVSA.EQ.1) READ (81,220) ZCVSA,ZTVSA,ZINVSA
  220	FORMAT(3F10.3)
C                                                                       Card 2C
	IF (IM.EQ.1 .AND. (MODEL.EQ.0 .OR. MODEL.EQ.7)) THEN
	    READ (81,230) ML,IRD1,IRD2,TITLE
  230	    FORMAT(3I5,18A4)
	    DO I=1,ML
C                                                                       Card 2C1
		READ (81,240) ZMDL(I),P(I),T(I),WMOL(1,I),WMOL(2,I),
     +                    WMOL(3,I),JCHAR(1,I),JCHAR(2,I),JCHAR(3,I),
     +                    JCHAR(4,I),JCHAR(5,I),JCHAR(6,I),JCHAR(7,I),
     +                    JCHAR(8,I),JCHAR(9,I),JCHAR(10,I),JCHAR(11,I),
     +                    JCHAR(12,I),JCHAR(13,I),JCHAR(14,I),
     +                    JCHAR(15,I),JCHARX(I)
  240		FORMAT(F10.3,5E10.3,15A1,A1)
C
		IF (IRD1.EQ.1) THEN
C                                                                       Card 2C2
		    READ (81,250) WMOL(4,I),WMOL(5,I),WMOL(6,I),
     +			 	  WMOL(7,I),WMOL(8,I),WMOL(9,I),
     +				  WMOL(10,I),WMOL(11,I),WMOL(12,I)
  250		    FORMAT(8E10.3)
		    IF (MDEF .EQ. 2) THEN
C									Card 2CX
			READ (81,250) (HOLD(J),J=1,NSPECX)
			DO J=1,NSPECX
			    XMOL(J,I) = HOLD(J)
			END DO
		    END IF
		END IF
C                                                                       Card 2C3
		IF (IRD2.EQ.1) READ (81,260) AHAZE(I),EQLWCZ(I),
     +					     RRATZ(I),IHA1(I),ICLD1(I),
     +					     IVUL1(I),ISEA1(I),ICHR1(I)
  260		FORMAT(10X,3F10.3,5I5)
	    END DO
	END IF
C                                                                       Card 2D
	IF (IHAZE.EQ.7 .OR. ICLD.EQ.11) THEN
	    READ (81,280) IREG
  280	    FORMAT(4I5)
	    DO I=1,4
		IF (IREG(I).NE.0) THEN
C                                                                       Card 2D1
		    READ (81,283) AWCCON(I),(TITLE2D(J,I), J=1,17)
  283		    FORMAT(E10.3,17A4)
		    TITLE2D(18,I) = '    '
C                                                                       Card 2D2
		    READ (81,285) (VX(J),EXTC(I,J),ABSC(I,J),ASYM(I,J),
     +				  J=1,NWAVLN)
  285		    FORMAT(3(F6.2,2F7.5,F6.4))
		END IF
	    END DO
	END IF
C
	IF (ICLD.GE.1 .AND. ICLD.LE.10) THEN
C									Card 2E1
	    IF (NCRALT .GE. 2) THEN
		DO I=1,NCRALT
		    READ (81,292) ZCLD(I),CLD(I),CLDICE(I),RR(I)
  292		    FORMAT(4F10.5)
		END DO
	    END IF
C									Card 2E2
	    IF (NCRSPC .GE. 2) THEN
		DO I=1,NCRSPC
		    READ (81,296) WAVLEN(I),EXTC(6,I),ABSC(6,I),
     +				 ASYM(6,I),EXTC(7,I),ABSC(7,I),ASYM(7,I)
  296		    FORMAT(7F10.5)
		END DO
	    END IF
	END IF
C
	IF (IEMSCT.EQ.3) THEN
C                                                                       Card 3
C                                                                    (Alternate)
	    READ (81,300) H1,H2,ANGLE,IDAY,RO,ISOURC,ANGLEM
  300	    FORMAT(3F10.3,I5,5X,F10.3,I5,F10.3)
	ELSE
C                                                                       Card 3
	    READ (81,310) H1,H2,ANGLE,RANGE,BETA,RO,LEN,PHI
  310	    FORMAT(6F10.5,I5,5X,F10.5)
	    IF (IEMSCT.EQ.2) THEN
C                                                                       Card 3A1
		READ (81,320) IPARM,IPH,IDAY,ISOURC
  320		FORMAT(4I5)
C                                                                       Card 3A2
		READ(81,330) PARM1,PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G
  330		FORMAT(8F10.3)
		IF (IPH.EQ.1) THEN
C                                                                       Card 3B1
		    READ (81,340) NANGLS
  340		    FORMAT(I5)
		    DO I=1,NANGLS
C                                                                       Card 3B2
			READ(81,350) ANGF(I),F(1,I),F(2,I),F(3,I),F(4,I)
  350			FORMAT(5E10.3)
		    END DO
		END IF
	    END IF
	END IF
C                                                                       Card 4
	READ (81,400) IV1,IV2,IDV,IRES,YFLAG,XFLAG,DLIMIT
  400	FORMAT(4I10,2A1,A8)
C                                                                       Card 5
	READ (81,500) IRPT
  500	FORMAT(I5)
C                                                                 Rescaling Card
	H2O = 1.0
	CO2 = 1.0
	O3 = 1.0
	O2 = 1.0
	CH4 = 1.0
	SO2 = 1.0
	TEMPADJ = 0.0
	READ (81,600,END=690) H2O,CO2,O3,O2,CH4,SO2,TEMPADJ
  600	FORMAT(7F10.3)
  690	CONTINUE
	IF (CO2 .LE. 0.0) CO2 = 1.0E-20
	IF (CO2MIX .LE. 0.0) CO2MIX = 330.0
	CO2MIX = CO2MIX * CO2
C
	CLOSE(81)
	RETURN
	END
C****************************************************************************
	SUBROUTINE REPORT_MODTRAN_PARMS(MODOUT,MODTAB)
C
	CHARACTER*80 MODOUT,MODTAB
C
	PARAMETER(LAYDIM=61,NSPECX=13,NZCLD=16,NAER=7,NWAVLN=47)
        COMMON /MODPARS/H2O,O3,CH4,SO2,TEMPADJ,MODEL,M1,M2,M3,M4,M5,M6,
     +          MDEF,TBOUND,CO2MIX,IHAZE,ISEASN,IVULCN,ICSTL,VIS,WSS,
     +          WHH,H1,H2,ML,IRD1,IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,
     +          JCHARX,AHAZE,EQLWCZ,IHA1,IVUL1,ISEA1,ICHR1,IPARM,IPH,
     +          IDAY,PARM1,PARM2,PARM3,PARM4,TIME,G
	REAL ZMDL(LAYDIM),P(LAYDIM),T(LAYDIM),AHAZE(LAYDIM)
	REAL WMOL(12,LAYDIM),XMOL(NSPECX,LAYDIM),EQLWCZ(LAYDIM)
	INTEGER IHA1(LAYDIM),IVUL1(LAYDIM),ISEA1(LAYDIM),ICHR1(LAYDIM)
	CHARACTER*4 TITLE(16)
	CHARACTER*1 JCHAR(15,LAYDIM),JCHARX(LAYDIM)
C
	CHARACTER*132 BUF
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
 	IF (MODEL.EQ.7 .AND. H2.LT.ZMDL(1)) THEN
	    CALL XVMESSAGE('The lowest atmospheric layer must be',' ')
	    CALL XVMESSAGE(
     +		'at least as low as the ground elevation value')
	    CALL ABEND
 	END IF
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('       ***MODTRAN PARAMETERS***',' ')
	IF (M1.EQ.0) THEN
	    CALL XVMESSAGE(MODEL_NAME(MODEL),' ')
	ELSE
	    CALL XVMESSAGE(MODEL_NAME(M1),' ')
	END IF
	IF (MODEL.EQ.7) THEN
	    CALL XVMESSAGE('   ALTITUDE  PRESSURE    TEMP    WATER',' ')
	    DO I=1,ML
		WRITE (BUF,100) ZMDL(I),P(I),T(I),WMOL(1,I),
     +				(JCHAR(J,I),J=1,15),JCHARX(I)
  100		FORMAT(F10.3,3F10.2,5X,15A1,2X,A1)
		CALL XVMESSAGE(BUF,' ')
	    END DO
	ELSE
	    IF (M2 .EQ. 0) THEN
		CALL XVMESSAGE(M2_NAME(MODEL),' ')
	    ELSE
		CALL XVMESSAGE(M2_NAME(M2),' ')
	    END IF
	    IF (M3 .EQ. 0) THEN
		CALL XVMESSAGE(M3_NAME(MODEL),' ')
	    ELSE
		CALL XVMESSAGE(M3_NAME(M3),' ')
	    END IF
	END IF
C
	WRITE (BUF,110) H2O
  110	FORMAT(' Water Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,120) O3
  120	FORMAT(' Ozone Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,130) CH4
  130	FORMAT(' Methane Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,140) SO2
  140	FORMAT(' Sulfur Dioxide Rescaling Factor =',F6.3)
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,150) CO2MIX
  150	FORMAT(' Carbon Dioxide Mixing Ratio =',F8.2,' ppm')
	CALL XVMESSAGE(BUF,' ')
C
        IF (TEMPADJ .NE. 0.0) THEN
            WRITE (BUF,155) TEMPADJ
  155       FORMAT(' Temperature Adjustment =',F6.2,' degrees Celsius')
            CALL XVMESSAGE(BUF,' ')
        END IF
C
	IF (IHAZE.GT.0) THEN
	    CALL XVMESSAGE(IHAZE_NAME(IHAZE),' ')
	    IF (VIS .NE. 0.0) THEN
		WRITE (BUF,160) VIS
  160		FORMAT(' Visibility set to ',F7.2,' km.')
		CALL XVMESSAGE(BUF,' ')
	    END IF
	ELSE
	    CALL XVMESSAGE(' No boundary layer aerosol attenuation',' ')
	END IF
C
	WRITE (BUF,200) H1
  200	FORMAT(' Sensor Altitude =',F9.3,' km')
	CALL XVMESSAGE(BUF,' ')
C
	WRITE (BUF,210) H2
  210	FORMAT(' Target Altitude =',F9.3,' km')
	CALL XVMESSAGE(BUF,' ')
C
	IF (MODOUT .NE. 'dummy') THEN
	    WRITE (BUF,300) MODOUT
  300	    FORMAT(' MODTRAN report file is stored in ',A80)
	    CALL XVMESSAGE(BUF,' ')
	END IF
C
	IF (MODTAB .NE. 'dummy') THEN
	    WRITE (BUF,310) MODTAB
  310	    FORMAT(' MODTRAN tabular file is stored in ',A80)
	    CALL XVMESSAGE(BUF,' ')
	END IF
C
	CALL XVMESSAGE(' ',' ')
	RETURN
	END
C***********************************************************************
	SUBROUTINE MODTRAN_INTERFACE(IOUT,RESP,MODOUT,MODTAB,TRANX,
     +				     PATHX)
C
	REAL RESP(900,10),TRANX(755,10),PATHX(755,10)
	CHARACTER*80 MODOUT,MODTAB
C
	PARAMETER(LAYDIM=61,NSPECX=13,NZCLD=16,NAER=7,NWAVLN=47)
        COMMON /MODPARS/H2O,O3,CH4,SO2,TEMPADJ,MODEL,M1,M2,M3,M4,M5,M6,
     +          MDEF,TBOUND,CO2MIX,IHAZE,ISEASN,IVULCN,ICSTL,VIS,WSS,
     +          WHH,H1,H2,ML,IRD1,IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,
     +          JCHARX,AHAZE,EQLWCZ,IHA1,IVUL1,ISEA1,ICHR1,IPARM,IPH,
     +          IDAY,PARM1,PARM2,PARM3,PARM4,TIME,G
	REAL ZMDL(LAYDIM),P(LAYDIM),T(LAYDIM),AHAZE(LAYDIM)
	REAL WMOL(12,LAYDIM),XMOL(NSPECX,LAYDIM),EQLWCZ(LAYDIM)
	INTEGER IHA1(LAYDIM),IVUL1(LAYDIM),ISEA1(LAYDIM),ICHR1(LAYDIM)
	CHARACTER*4 TITLE(16)
	CHARACTER*1 JCHAR(15,LAYDIM),JCHARX(LAYDIM)
C
	REAL WAVELEN(50000),TRANSMIT(50000),PATHRAD(50000),SKYRAD(50000)
	REAL SPECALB(50000),RRATZ(LAYDIM),AWCCON(4),WAVLEN(47),VX(47)
	REAL EXTC(7,47),ABSC(7,47),ASYM(7,47)
	REAL ZCLD(16),CLD(16),CLDICE(16),RR(16)
	REAL ANGF(50),F(4,50),FRAC(378),TRANS(10,2),PATH(10,2),SKY(10,2)
	INTEGER ICLD1(LAYDIM),IREG(4)
	LOGICAL LDISORT,LSUN1
	CHARACTER*80 MODLIB,PLTABFILE,SUNFILE,DIRACFILE,UFTAPXFILE,MSG
	CHARACTER*8 DLIMIT
	CHARACTER*4 TITLE2D(18,4)
	CHARACTER*1 CODE,YFLAG,XFLAG
C						  set up interpolation fractions
C						  for each sample location
	DEFL = 35.482
	DELTA = DEFL/377.0
	COSFULL = COS(DEFL*PI/180)
	DO I=1,377
	    COSI = COS(DELTA*FLOAT(I-1)*PI/180)
	    FRAC(I) = (COSI-COSFULL) / (COSI*(1.0-COSFULL))
	END DO
	FRAC(378) = 0.0
C						       declare modtran constants
	PLTABFILE = 'null'
	CALL XGETENV_VIC('VICARMODTRAN35',MODLIB)
	SUNFILE = MODLIB(1:LNBLNK(MODLIB)) // '/sun3'
	DIRACFILE = MODLIB(1:LNBLNK(MODLIB)) // '/MOLBMP96.BIN'
	UFTAPXFILE = MODLIB(1:LNBLNK(MODLIB)) // '/CFCBMP96.ASC'
	O2 = 1.0
	CODE = 'T'
	ITYPE = 2
	IEMSCT = 1
	IMULT = 1
	IF (MODEL .EQ. 7) THEN
	    IM = 1
	ELSE
	    IM = 0
	END IF
	NOPRNT = 0
	DO I=1,50000
	    SPECALB(I) = 0.05
	END DO
	LDISORT = .FALSE.
	ISTRM = 0
	LSUN1 = .TRUE.
	ISUN = 5
	ICLD = 0
	IVSA = 0
	RAINRT = 0.0
	GNDALT = H2
	RANGE = 0.0
	BETA = 0.0
	RO = 0.0
	LEN = 0
	PHI = 0.0
	IV1 = 601
	IV2 = 1500
	IDV = 1
	IRES = 2
	YFLAG = ' '
	XFLAG = ' '
	IF (IRD2 .NE. 0) THEN
	    DO I=1,ML
		RRATZ(I) = 0.0
		ICLD1(I) = 0
	    END DO
	END IF
	ANGLE = 180.0
C						       run MODTRAN at two angles
	DO IANGLE=1,2
	    CALL MODTRAN3(MODOUT,MODTAB,PLTABFILE,SUNFILE,DIRACFILE,
     +		UFTAPXFILE,H2O,O3,O2,CH4,SO2,TEMPADJ,CODE,MODEL,ITYPE,
     +		IEMSCT,IMULT,M1,M2,M3,M4,M5,M6,MDEF,IM,NOPRNT,
     +		TBOUND,SPECALB,LDISORT,ISTRM,LSUN1,ISUN,CO2MIX,
     +		IHAZE,ISEASN,IVULCN,ICSTL,ICLD,IVSA,VIS,WSS,WHH,
     +		RAINRT,GNDALT,H1,H2,ANGLE,RANGE,BETA,RO,LEN,PHI,
     +		IV1,IV2,IDV,IRES,YFLAG,XFLAG,DLIMIT,CTHIK,CALT,
     +		CEXT,ISEED,NCRALT,NCRSPC,CWAVLN,CCOLWD,CCOLIP,
     +		CHUMID,ASYMWD,ASYMIP,ZCVSA,ZTVSA,ZINVSA,ML,IRD1,
     +		IRD2,TITLE,ZMDL,P,T,WMOL,XMOL,JCHAR,JCHARX,
     +		AHAZE,EQLWCZ,RRATZ,IHA1,ICLD1,IVUL1,ISEA1,ICHR1,
     +		IREG,AWCCON,TITLE2D,WAVLEN,VX,EXTC,ABSC,ASYM,
     +		ZCLD,CLD,CLDICE,RR,IPARM,IPH,IDAY,ISOURC,PARM1,
     +		PARM2,PARM3,PARM4,TIME,PSIPO,ANGLEM,G,NANGLS,
     +		ANGF,F,WAVELEN,TRANSMIT,PATHRAD,SKYRAD)
C
C					Compute the transmittance, path radiance
C					and sky radiance for each channel by
C					multiplying the per wavenumber values by
C					the 10 normalized response functions.
	    DO ICHAN=1,10
		TRANS(ICHAN,IANGLE) = 0.0
		PATH(ICHAN,IANGLE) = 0.0
		SKY(ICHAN,IANGLE) = 0.0
		DO IWAVE=1,900
		    TRANS(ICHAN,IANGLE) = TRANS(ICHAN,IANGLE) +
     +			 	       RESP(IWAVE,ICHAN)*TRANSMIT(IWAVE)
		    PATH(ICHAN,IANGLE) = PATH(ICHAN,IANGLE) +
     +					RESP(IWAVE,ICHAN)*PATHRAD(IWAVE)
		    SKY(ICHAN,IANGLE) = SKY(ICHAN,IANGLE) +
     +					RESP(IWAVE,ICHAN)*SKYRAD(IWAVE)
		END DO
C					      Change units from W/cm^2 to mW/m^2
C
		PATH(ICHAN,IANGLE) = 1.0E7 * PATH(ICHAN,IANGLE)
		SKY(ICHAN,IANGLE) = 1.0E7 * SKY(ICHAN,IANGLE)
C						      to prevent division by 0.0
		IF (TRANS(ICHAN,IANGLE).LE.0.0) TRANS(ICHAN,IANGLE)=1.0
	    END DO
C					    update params for second modtran run
	    MODOUT = 'dummy'
	    MODTAB = 'dummy'
	    ANGLE = 180.0 - DEFL
	END DO
C					  put sky radiance values in VICAR label
	CALL XLADD(IOUT,'HISTORY','SKY93',SKY(1,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY94',SKY(2,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY95',SKY(3,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY96',SKY(4,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY97',SKY(5,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY98',SKY(6,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY99',SKY(7,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY100',SKY(8,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY101',SKY(9,1),ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUT,'HISTORY','SKY102',SKY(10,1),ISTAT,
     +		   'FORMAT','REAL',' ')
C					     interpolate transmittance and path
C					     radiance to a value for each sample
	DO ICHAN=1,10
	    TRANX(378,ICHAN) = TRANS(ICHAN,1)
	    PATHX(378,ICHAN) = PATH(ICHAN,1)
	    DO I=1,377
		TRANX(378+I,ICHAN) = FRAC(I+1)*TRANS(ICHAN,1) +
     +				     (1.0-FRAC(I+1))*TRANS(ICHAN,2)
		PATHX(378+I,ICHAN) = FRAC(I+1)*PATH(ICHAN,1) +
     +				     (1.0-FRAC(I+1))*PATH(ICHAN,2)
		TRANX(378-I,ICHAN) = TRANX(378+I,ICHAN)
		PATHX(378-I,ICHAN) = PATHX(378+I,ICHAN)
	    END DO
	END DO
C							      report the relults
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(
     +'          Transmittance      Path  Radiance      Sky Radiance',
     +' ')
	CALL XVMESSAGE(
     +'                             (mW/m**2/sr/u)       (mW/m**2/u)',
     +' ')
	CALL XVMESSAGE(
     +' Band   Nadir   Off-nadir   Nadir   Off-nadir',' ')
	DO ICHAN=1,10
	    WRITE (MSG,500) ICHAN,TRANS(ICHAN,1),TRANS(ICHAN,2),
     +			    PATH(ICHAN,1),PATH(ICHAN,2),SKY(ICHAN,1)
  500	    FORMAT(I4,F10.5,F9.5,F11.1,F9.1,F15.1)
	    CALL XVMESSAGE(MSG,' ')
	END DO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE GRADCAL(IN,INLUT,IOUT,ISL,NL,ORG,TRANX,PATHX)
C
	REAL TRANX(755,10),PATHX(755,10),BUF(755)
	INTEGER*2 OUTBUF(755),LUT(40000,10),BIPBUF(755,10)
	CHARACTER*3 ORG
C							       load radiance LUT
	DO I=1,10
	    CALL XVREAD(INLUT,LUT(1,I),ISTAT,' ')
	END DO
C							       process BIP input
	IF (ORG .EQ. 'BIP') THEN
	    DO ILINE=ISL,ISL+NL-1
		DO ISAMP=1,755
		    CALL XVREAD(IN,BUF,ISTAT,'LINE',ILINE,'SAMP',ISAMP,
     +				' ')
		    DO ICHAN=1,10
			ITEMP = NINT(10*BUF(ICHAN)) + 27315
			ITEMP = MIN(40000,MAX(1,ITEMP))
			NUM = NINT((FLOAT(LUT(ITEMP,ICHAN)) -
     +				 PATHX(ISAMP,ICHAN))/TRANX(ISAMP,ICHAN))
			BIPBUF(ISAMP,ICHAN) = MIN(32767,MAX(-32768,NUM))
		    END DO
		END DO
		DO ICHAN=1,10
		    CALL XVWRIT(IOUT,BIPBUF(1,ICHAN),ISTAT,' ')
		END DO
	    END DO
C							  process BIL, BSQ input
	ELSE
	    DO ILINE=ISL,ISL+NL-1
		DO ICHAN=1,10
		    CALL XVREAD(IN,BUF,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +				' ')
		    DO J=1,755
			ITEMP = NINT(10*BUF(J)) + 27315
			ITEMP = MIN(40000,MAX(1,ITEMP))
			NUM = NINT((FLOAT(LUT(ITEMP,ICHAN)) -
     +				    PATHX(J,ICHAN))/TRANX(J,ICHAN))
			OUTBUF(J) = MIN(32767,MAX(-32768,NUM))
		    END DO
		    CALL XVWRIT(IOUT,OUTBUF,ISTAT,' ')
		END DO
	    END DO
	END IF
C
	RETURN
	END
