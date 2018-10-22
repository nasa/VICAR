      INCLUDE 'VICMAIN_FOR'
C  TIMSNAV
C  PURPOSE: ADD NAV DATA TO TIMS AUX FILE
C
	SUBROUTINE MAIN44
	IMPLICIT INTEGER(A-Z)
	CHARACTER*80 PRT
	REAL*8 SEC
	REAL AUX(41) ! BUFFER FOR AUX DATA
	REAL C2(5000),C8(5000),C9(5000),C10(5000),C11(5000),C12(5000)
	REAL C13(5000),C14(5000),C15(5000),C16(5000),C17(5000),C18(5000)
	REAL C19(5000),C20(5000),C21(5000)
	REAL R1,OSEC,DEL,SSEC,TSEC,TIME
	INTEGER HMS(3)
	LOGICAL QTEST/.TRUE./
C
C---- READ PARAMETERS. OPEN FILES
C
	CALL XVUNIT(UNIT,'INP',1,STAT,' ')
	CALL XVOPEN(UNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XVGET(UNIT,STAT,'NS',NS,'NL',NL,' ')
	WRITE (PRT,10) NL,NS
   10	FORMAT(' TIMS AUX input NL =',I5,'   NS =',I3)
	CALL XVMESSAGE(PRT,' ')
	IF(NS.LT.31) THEN
	    CALL XVMESSAGE(
     +		' TIMS NS LT 31, use ALL option in TIMSLOG',' ')
	    CALL ABEND
	ENDIF
C
	CALL RDFIL(NUNIT,2,CLEN,NUMC,STAT) ! OPEN NAV INTERFACE FILE
	CLEN1 = CLEN-1
	IF(NUMC.NE.21) THEN
	    CALL XVMESSAGE(' Columns in NAV file NE 21',' ')
	    CALL ABEND
	ENDIF
	CALL GETCOL(NUNIT,2,CLEN,C2) ! GET TIMES (SECONDS) IN C2
	CALL GETCOL(NUNIT,8,CLEN,C8) ! DRIFT
	CALL GETCOL(NUNIT,9,CLEN,C9) ! LATITUDE
	CALL GETCOL(NUNIT,10,CLEN,C10) ! LONGITUDE
	CALL GETCOL(NUNIT,11,CLEN,C11) ! GROUND SPEED
	CALL GETCOL(NUNIT,12,CLEN,C12) ! TRUE HEADING
	CALL GETCOL(NUNIT,13,CLEN,C13) ! WIND SPEED
	CALL GETCOL(NUNIT,14,CLEN,C14) ! WIND ANGLE
	CALL GETCOL(NUNIT,15,CLEN,C15) ! PRT-5
	CALL GETCOL(NUNIT,16,CLEN,C16) ! DEW POINT
	CALL GETCOL(NUNIT,17,CLEN,C17) ! TOTAL AIR TEMP
	CALL GETCOL(NUNIT,18,CLEN,C18) ! PRESSURE ALTITUDE
	CALL GETCOL(NUNIT,19,CLEN,C19) ! PITCH
	CALL GETCOL(NUNIT,20,CLEN,C20) ! ROLL
	CALL GETCOL(NUNIT,21,CLEN,C21) ! RADAR ALTITUDE
C
	CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
	CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NS',41,' ')
	CALL XVP('STIME',HMS,CNT)
	SEC = 3600.*HMS(1) + 60.*HMS(2) + HMS(3)
	CALL SETTIME(UNIT,TIME,DEL,NL)
	IF (SEC .EQ. 0.0) THEN
	    SEC = TIME - DEL
	ELSE
	    SEC = SEC - DEL
	END IF
	SSEC = -999.99
	OSEC = SSEC
	IC = 1
	LEND = 0
	LST = NL+1
C
	DO 100 L=1,NL
	    CALL XVREAD(UNIT,AUX,STAT,'LINE',L,' ')
	    AUX(40) = AUX(32)
	    AUX(41) = AUX(33)
	    TSEC=3600.*AUX(18)+60.*AUX(19)+AUX(20)
C
C				If this time is not the same as the last time,
C				but within 0.2 seconds of expected time, accept
C				this time as accurate.  Otherwise, project a new
C				time based upon the previous line's time and the
C				scan speed.
	    IF (TSEC.NE.SSEC .AND. ABS(TSEC-OSEC-DEL).LT.0.2) THEN
		SEC = TSEC
	    ELSE
		SEC = SEC + DEL
	    END IF
	    ISEC = SEC
	    MINS = ISEC/60
	    AUX(18) = MINS/60
	    AUX(19) = MOD(MINS,60)
	    AUX(20) = SEC - 60.0*MINS
C
	    DO 60 II=IC,CLEN1
		IF (SEC .LE. C2(II+1)) GO TO 80
   60	    CONTINUE
	    II = CLEN1
C
   80	    CONTINUE
C							Is extrapolation needed?
	    IF (SEC .LT. C2(1)) LEND=L
	    IF (SEC .GT. C2(CLEN) .AND. QTEST) THEN
		QTEST = .FALSE.
		LST = L
	    END IF
C
	    IC = II
	    R1=(SEC-C2(IC))/(C2(IC+1)-C2(IC))      ! INTERPOLATION FACTOR
	    AUX(25)=C9(IC)+R1*(C9(IC+1)-C9(IC))    ! LATITUDE
	    AUX(26)=C10(IC)+R1*(C10(IC+1)-C10(IC)) ! LONGITUDE
	    AUX(27)=C12(IC)+R1*(C12(IC+1)-C12(IC)) ! TRUE HEADING
	    AUX(28)=C19(IC)+R1*(C19(IC+1)-C19(IC)) ! PITCH
	    AUX(29)=C20(IC)+R1*(C20(IC+1)-C20(IC)) ! ROLL
	    AUX(30)=C11(IC)+R1*(C11(IC+1)-C11(IC)) ! GROUND SPEED
	    AUX(31)=C8(IC)+R1*(C8(IC+1)-C8(IC))    ! DRIFT
	    AUX(32)=C21(IC)+R1*(C21(IC+1)-C21(IC)) ! RADAR ALTITUDE (FT)
	    AUX(32)=AUX(32)/3.281                     ! CONVERT TO METERS
	    AUX(33)=C18(IC)+R1*(C18(IC+1)-C18(IC)) ! PRESSURE ALTITUDE (FT)
	    AUX(33)=AUX(33)/3.281                     ! CONVERT TO METERS
	    AUX(34)=C17(IC)+R1*(C17(IC+1)-C17(IC)) ! TOTAL AIR TEMP
	    AUX(35)=C16(IC)+R1*(C16(IC+1)-C16(IC)) ! DEW POINT
	    AUX(36)=C15(IC)+R1*(C15(IC+1)-C15(IC)) ! PRT-5
	    AUX(37)=C14(IC)+R1*(C14(IC+1)-C14(IC)) ! WIND ANGLE
	    AUX(38)=C13(IC)+R1*(C13(IC+1)-C13(IC)) ! WIND SPEED
	    AUX(39)=AUX(33)-AUX(32)                ! GROUND ELEVATION (METERS)
	    CALL XVWRIT(OUNIT,AUX,STAT,' ')
	    OSEC=SEC                         ! SAVE OLD SEC (TIME THAT WAS USED)
	    IF(TSEC.NE.0.) SSEC=TSEC         ! ALSO SAVE TIMS TIME, IF NON-ZERO
  100	CONTINUE
C
	IF (LEND .GT. 0) THEN
	    WRITE (PRT,150) 1,LEND
	    CALL XVMESSAGE(PRT,' ')
	END IF
	IF (LST .LE. NL) THEN
	    WRITE (PRT,150) LST,NL
	    CALL XVMESSAGE(PRT,' ')
	END IF
  150	FORMAT(' Extrapolation used on Lines',I6,' -',I6)
C
	CALL XVCLOSE(UNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
	CALL XVCLOSE(NUNIT,STAT,' ')
	RETURN
C
	END
C*******************************************************************************
	SUBROUTINE SETTIME(INUNIT,TIME,DEL,NL)
C
C	This routine searches for 3 scan lines of one time followed by 3 scan
C	lines of a different time.  It is assumed that the time on the first
C	scan line with the second time is correct.  The exact time of the first
C	scan line is then computed, using the scan speed to determine the
C	time interval from the first scan line to the first scan line of the 
C	second time.
C
	REAL BUF(41)
	SECS(HR,XMIN,SEC) = 3600.0*HR + 60.0*XMIN + SEC
C
	CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	VAL = SECS(BUF(18),BUF(19),BUF(20))
	IF (BUF(24) .NE. 0.0) DEL = 1.0/BUF(24)
	II = 1
C
  100	CONTINUE
	DO I=II+1,NL			! do we have 3 lines of the first value?
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    X = SECS(BUF(18),BUF(19),BUF(20))
	    IF (X .NE. VAL) THEN
		VAL = X
		IF (I-II .LT. 3) THEN		! NO - reset and try again
		    II = I
		    GO TO 100
		ELSE				! YES - go look for 3 of 2nd val
		    II = I
		    GO TO 200
		END IF
	    END IF
	END DO
	CALL XVMESSAGE(
     +		' Unable to find a reliable sequence of times',' ')
	CALL ABEND
C
  200	CONTINUE
	DO I=1,2
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    X = SECS(BUF(18),BUF(19),BUF(20))
	    IF (X .NE. VAL) THEN		! reset and try again from start
		II = II+I
		VAL = X
		GO TO 100
	    END IF
	END DO
C						! valid pattern of times found
	IF (BUF(24) .NE. 0.0) DEL = 1.0/BUF(24)
	TIME = VAL - (II-1)*DEL
	RETURN
	END
