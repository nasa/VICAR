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
