	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C  27 NOV 91   ...REA...   New Program
C   5 FEB 92   ...REA...   Place gains in AUX, rather than label
C
	INTEGER IP1(12),IP2(12)
	INTEGER SLO,SSO,ELO,PIX,AUX
	REAL*4  OUTBUF(42)
	LOGICAL*1 INBUF(9192)
C
C	  ***  Open input data set ***
C
	CALL XVUNIT(INP,'INP',1,ISTAT,' ')
	CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C	  ***  Parameter processing ***
C
	CALL XVPCNT('OUT',NO)
	CALL XVSIZE(SLO,SSO,NLO,NSO,NLIN,NSIN)
	IF (NSO.EQ.NSIN) NSO=716
C
C	  ***  Set pointers for each channel ***
C
	DO ICHAN = 1,12
	    IP1(ICHAN) = ((ICHAN-1)*766)+1
	    IP2(ICHAN) = IP1(ICHAN)+SSO+49
	END DO
C
C	  ***  Open first output data set ***
C
	CALL XVUNIT(PIX,'OUT',1,STAT,' ')
	CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     &		NLO,'U_NS',NSO,'U_NB',12,'U_ORG','BIL','OP','WRITE',
     &		'O_FORMAT','BYTE','U_FORMAT','BYTE',' ')
C
C	  *** For standard processing, write an auxiliary file ***
C
	IF(NO.GT.1) THEN
	    CALL XVUNIT(AUX,'OUT',2,ISTAT,' ')
	    CALL XVOPEN(AUX,ISTAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     &		NLO,'U_NS',42,'U_ORG','BSQ','OP','WRITE',
     &		'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	ENDIF
C
C	  *** Process data records ***
C
	ELO = SLO + NLO - 1
	DO ILINE = SLO,ELO
	    CALL XVREAD(INP,INBUF,ISTAT,'LINE',ILINE,' ')
C
	    IF (ILINE .EQ. SLO) THEN
		IRUN = I2VAL(INBUF(3))
		IDATE = I4VAL(INBUF(9))
		JDATE = JULDAT(IDATE)
		SCANSPD = I2VAL(INBUF(17))/10.0
		DEMAG = I2VAL(INBUF(25))/100.0
		CALL XLADD(PIX,'HISTORY','DATE',JDATE,ISTAT,
     & 		           'FORMAT','INT',' ')
		CALL XLADD(PIX,'HISTORY','RUN',IRUN,ISTAT,
     & 		           'FORMAT','INT',' ')
		CALL XLADD(PIX,'HISTORY','SCANSPD',SCANSPEED,ISTAT,
     &		           'FORMAT','REAL',' ')
		CALL XLADD(PIX,'HISTORY','DEMAG',DEMAG,ISTAT,
     &			   'FORMAT','REAL',' ')
		IF (NO .GT. 1) THEN
		    CALL XLADD(AUX,'HISTORY','DATE',JDATE,ISTAT,
     &		               'FORMAT','INT',' ')
		    CALL XLADD(PIX,'HISTORY','RUN',IRUN,ISTAT,
     & 				'FORMAT','INT',' ')
	    	    CALL XLADD(AUX,'HISTORY','SCANSPD',SCANSPEED,ISTAT,
     &			       'FORMAT','REAL',' ')
		    CALL XLADD(AUX,'HISTORY','DEMAG',DEMAG,ISTAT,
     &			       'FORMAT','REAL',' ')
		END IF
	    END IF
	    DO I=1,12
		CALL XVWRIT(PIX,INBUF(IP2(I)),ISTAT,' ')
	    END DO
C								write AUX file
	    IF (NO .GT. 1) THEN
		OUTBUF(1) = I2VAL(INBUF(13))/100.0		! BB1 Temp
		OUTBUF(2) = I2VAL(INBUF(15))/100.0		! BB2 Temp
		OUTBUF(27) = I4VAL(INBUF(5))			! Scan Line
		OUTBUF(28) = I4VAL(INBUF(33))/10.0		! Time HHMMSS.T
		OUTBUF(29) = I2VAL(INBUF(41))			! Roll
		OUTBUF(30) = I2VAL(INBUF(1))			! Frame Status
		DO I=1,12
		    OUTBUF(2*I+1) = I2VAL(INBUF(IP1(I)+36))	! BB1 DN
		    OUTBUF(2*I+2) = I2VAL(INBUF(IP1(I)+38))	! BB2 DN
		    OUTBUF(I+30) = I2VAL(INBUF(IP1(I)+28))/1000.0 ! Gain
		END DO
		CALL XVWRIT(AUX,OUTBUF,ISTAT,' ')
	    END IF
	END DO
C
	CALL XVCLOSE(INP,ISTAT,' ')
	CALL XVCLOSE(PIX,ISTAT,' ')
	IF(NO.GT.1) CALL XVCLOSE(AUX,ISTAT,' ')
	RETURN
	END
C******************************************************************************
	INTEGER FUNCTION I2VAL(BUF)
C
	BYTE BUF(2)
C
	IF (BUF(1) .GE. 0) THEN
	    I1 = BUF(1)
	ELSE
	    I1 = BUF(1) + 256
	END IF
	IF (BUF(2) .GE. 0) THEN
	    I2 = BUF(2)
	ELSE
	    I2 = BUF(2) + 256
	END IF
	I2VAL = 256*I1 + I2
	IF (I2VAL .GT. 32767) I2VAL=I2VAL-65536
	RETURN
	END
C******************************************************************************
	INTEGER FUNCTION I4VAL(BUF)
C
	BYTE BUF(4)
C
	IF (BUF(1) .GE. 0) THEN
	    I1 = BUF(1)
	    ISIGN = 0
	ELSE
	    I1 = BUF(1) + 128
	    ISIGN = 1
	END IF
	IF (BUF(2) .GE. 0) THEN
	    I2 = BUF(2)
	ELSE
	    I2 = BUF(2) + 256
	END IF
	IF (BUF(3) .GE. 0) THEN
	    I3 = BUF(3)
	ELSE
	    I3 = BUF(3) + 256
	END IF
	IF (BUF(4) .GE. 0) THEN
	    I4 = BUF(4)
	ELSE
	    I4 = BUF(4) + 256
	END IF
	I4VAL = 16777216*I1 + 65536*I2 + 256*I3 + I4
	IF (ISIGN .EQ. 1) I4VAL = I4VAL - 2**30 - 2**30
	RETURN
	END
C****************************************************************************
	INTEGER FUNCTION JULDAT(NUM)
C
	IYR = NUM/1000000
	JDAY = MOD(NUM,1000)
C
	IF (MOD(IYR,4).NE.0 .AND. JDAY.GT.59) JDAY=JDAY+1
	IF (JDAY .LE. 31) THEN
	    MON = 1
	    IDAY = JDAY
	ELSE IF (JDAY .LE. 60) THEN
	    MON = 2
	    IDAY = JDAY-31
	ELSE IF (JDAY .LE. 91) THEN
	    MON = 3
	    IDAY = JDAY-60
	ELSE IF (JDAY .LE. 121) THEN
	    MON = 4
	    IDAY = JDAY-91
	ELSE IF (JDAY .LE. 152) THEN
	    MON = 5
	    IDAY = JDAY-121
	ELSE IF (JDAY .LE. 182) THEN
	    MON = 6
	    IDAY = JDAY-152
	ELSE IF (JDAY .LE. 213) THEN
	    MON = 7
	    IDAY = JDAY-182
	ELSE IF (JDAY .LE. 244) THEN
	    MON = 8
	    IDAY = JDAY-213
	ELSE IF (JDAY .LE. 274) THEN
	    MON = 9
	    IDAY = JDAY-244
	ELSE IF (JDAY .LE. 305) THEN
	    MON = 10
	    IDAY = JDAY-274
	ELSE IF (JDAY .LE. 335) THEN
	    MON = 11
	    IDAY = JDAY-305
	ELSE
	    MON = 12
	    IDAY = JDAY-335
	END IF
C
	JULDAT = 10000*IYR + 100*MON + IDAY
	RETURN
	END
