	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C  1 AUG 85    ...REW...   CONCATENATION OF TIMSLOG (NSTL) AND VAMESLOG
C  23 SEP 88   ...REA...   MODIFIED FROM VTIMSLOG
C  26 MAR 91   ...REA...   ADAPTED TO UNIX FROM GNK VERSION
C  20 JAN 92   ...REA...   AMBIENT TEMPERATURES ADDED TO AUX FILE
C  27 MAR 92   ...REA...   AMBIENT TEMPERATURES ADDED TO SHORT AUX FILE,
C                          BETTER COMPUTING AND CHECKING FOR VALID DATES.
C  17 JUL 96   ...REA...   EXABYTE OPTION ADDED
C
	COMMON /C1/ INBUF,OUTBUF
	LOGICAL*1 INBUF(4200), OUTBUF(4200)
	LOGICAL XVPTST
C
C	    ** One of the two main subroutines is selected at this point **
C
	IF(XVPTST('AMES')) THEN
	    CALL AMESLOG
	ELSE IF (XVPTST('NSTL')) THEN
	    CALL NSTLLOG
	ELSE
	    CALL EXABYTELOG
	END IF
C
	RETURN
	END
C*******************************************************************************
C	** The EXABYTELOG subroutine is used in the case that the data
C	** have not been decommutated.
C
	SUBROUTINE EXABYTELOG
C
	REAL AUXBUF(33)
	INTEGER IORDER(6)/1,2,3,4,5,6/,IOUTORD(6)
	INTEGER PIX,AUX,STAT,ELO,SLO,SSO
	LOGICAL XVPTST
	BYTE INBUF(4200),OUTBUF(4200)
	LOGICAL*1 FIRST
	CHARACTER*80 LBL,MSG
C							  Open input data set
	CALL XVUNIT(INP,'INP',1,STAT,0)
	CALL XVOPEN(INP,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C							  Parameter processing
	CALL XVPCNT('OUT',NO)
	CALL XVPARM('ORDER',IORDER,ICNT,IDEF,6)
	CALL XVPARM('MAXBAD',MAXBAD,ICNT,IDEF,1)
	CALL XVSIZE(SLO,SSO,NLO,NSO,NLIN,NSIN)
	IF (NSO .GT. 638) NSO=638
	IF (NLO .EQ. NLIN) NLO=6*(NLIN-1)
	NCHAN = 6
C						     report # of channels, bands
	WRITE (MSG,100) 'bands', NCHAN
	CALL XVMESSAGE(MSG,' ')
	WRITE (MSG,100) 'lines', NLO
	 CALL XVMESSAGE(MSG,' ')
  100	FORMAT(' Number of ',A5,' =',I5)
C					    re-order, if inputs are out of order
	DO I=1,NCHAN
	    IOUTORD(I) = IORDER(I)
	END DO
	IF (NCHAN.GT.1) CALL SORTIN(IOUTORD,NCHAN)
C								Open output(s)
	CALL XVUNIT(PIX,'OUT',1,STAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &			'U_NL',NLO,'U_NS',NSO,'U_NB',NCHAN,
     &			'U_ORG','BSQ','OP','WRITE',' ')
	    DO I=1,NCHAN
		DO J=1,NLO
		    CALL XVWRIT(PIX,OUTBUF,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(PIX,STAT,' ')
	    CALL XVOPEN(PIX,STAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','UPDATE',' ')
	ELSE
	    CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &			'U_NL',NLO,'U_NS',NSO,'U_NB',NCHAN,
     &			'U_ORG','BIL','OP','WRITE',' ')
	END IF
C	  		For quick-look processing, don't write an auxiliary file
	IF(NO.GT.1) THEN
	    IF(XVPTST('ALL')) THEN
		NSO2 = 33
	    ELSE
		NSO2 = 16
	    END IF
	    CALL XVUNIT(AUX,'OUT',2,STAT,' ')
	    CALL XVOPEN(AUX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &		'U_NL',NLO,'U_NS',NSO2,'OP','WRITE','U_FORMAT','REAL',
     &		'O_FORMAT','REAL',' ')
	END IF
C						    *** Process data records ***
	FIRST = .TRUE.
	NBAD = 0
	LOC = 101 + SSO
	ELO = SLO + NLO - 1
C
	DO ILINE=SLO,ELO
C
	    DO I=1,NCHAN
		CALL GET_REC(INP,ILINE,I,INBUF)
C
C			Use first data line to fill in first and second labels
		IF (FIRST) THEN
		    CALL LABFIL1(INBUF,LBL,1)
		    CALL XLADD(PIX,'HISTORY','INFO1',LBL,STAT,
     &			    'FORMAT','STRING',' ')
		    IF(NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO1',LBL,
     &					   STAT,'FORMAT','STRING',' ')
		    LBL = ' ' // LBL
		    CALL XVMESSAGE(LBL,' ')
C
		    CALL LABFIL1(INBUF,LBL,2)
		    CALL XLADD(PIX,'HISTORY','INFO2',LBL,STAT,
     &			    'FORMAT','STRING',' ')
		    IF(NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO2',LBL,
     &				 	STAT,'FORMAT','STRING',' ')
		    LBL = ' ' // LBL
		    CALL XVMESSAGE(LBL,' ')
C
		    FIRST = .FALSE.
		END IF
C
C	  Get true channel number
		ICHAN = INBUF(100)
		ICHAN = MOD(ICHAN,8)
		IF(ICHAN.NE.IORDER(I)) THEN
		    WRITE (LBL,200) ILINE, IORDER(I), ICHAN
  200		    FORMAT(' *** Error in Channel ID ***, Line',I6,
     +		           ' Channel',I2,' Reads',I2)
		    CALL XVMESSAGE(LBL,' ')
		    NBAD = NBAD+1
		    ICHAN = IORDER(I)
		    IF(NBAD.GE.MAXBAD) THEN
			CALL XVMESSAGE(' MAXBAD EXCEEDED',' ')
			CALL ABEND
		    ENDIF
		END IF
C
C				       Concatenate channels to form output image
		CALL MVL(INBUF(LOC),OUTBUF((ICHAN-1)*NSO+1),NSO)
C								  Auxiliary data
		IF(NO.GT.1) CALL AUXCON1(INBUF,AUXBUF,NSO2,ICHAN)
	    END DO
C							      Write output image
	    DO I=1,NCHAN
		CALL XVWRIT(PIX,OUTBUF(NSO*(IOUTORD(I)-1)+1),STAT,
     +			   'LINE',ILINE-SLO+1,'BAND',I,'NSAMPS',NSO,' ')
	    END DO
C					Write secondary output  (auxiliary data)
	    IF(NO.GT.1) CALL XVWRIT(AUX,AUXBUF,STAT,' ')
	END DO
C
C				      Fill in third label from last data record
	CALL LABFIL1(INBUF,LBL,3)
	CALL XLADD(PIX,'HISTORY','INFO3',LBL,STAT,'FORMAT','STRING',' ')
	IF(NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO3',LBL,STAT,
     &			'FORMAT','STRING',' ')
	LBL = ' ' // LBL
	CALL XVMESSAGE(LBL,' ')
C
C	  Close data sets
	CALL XVCLOSE(INP,STAT,' ')
	CALL XVCLOSE(PIX,STAT,' ')
	IF(NO.GT.1) CALL XVCLOSE(AUX,STAT,' ')
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GET_REC(INP,ILINE,ICHAN,BUF)
C
	SAVE
	INTEGER LAST_REC/-1/
	BYTE BUF(4200),BIGBUF(750,7,7)
C
	INREC = 2 + ((ILINE-1)/6)
	LOC = MOD(ILINE,6)
	IF (LOC .EQ. 0) LOC=6
C
	IF (INREC .NE. LAST_REC)  THEN
	    CALL XVREAD(INP,BIGBUF,ISTAT,'LINE',INREC,' ')
	    LAST_REC = INREC
	END IF
	CALL MVL(BIGBUF(1,ICHAN,LOC),BUF,750)
C
	RETURN
	END
C*******************************************************************************
C	** The NSTLLOG subroutine is used in the case that the data **
C	**  were produced at the NSTL (now Stennis) in Mississippi **
C
	SUBROUTINE NSTLLOG
C
	COMMON /C1/ INBUF,OUTBUF
C
	REAL AUXBUF(33)
	INTEGER IORDER(6)/1,2,3,4,5,6/,IINBUF(500),IOUTORD(6)
	INTEGER PIX,AUX,STAT,ELO,SLO,SSO
	LOGICAL XVPTST
	BYTE INBUF(4200),OUTBUF(4200)
	LOGICAL*1 FIRST
	CHARACTER*80 LBL,MSG
	EQUIVALENCE (IINBUF,INBUF)
C
C	  Open input data set
	CALL XVUNIT(INP,'INP',1,STAT,0)
	CALL XVOPEN(INP,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C	  Parameter processing
	CALL XVPCNT('OUT',NO)
	CALL XVPARM('ORDER',IORDER,ICNT,IDEF,6)
	CALL XVPARM('MAXBAD',MAXBAD,ICNT,IDEF,1)
	CALL XVSIZE(SLO,SSO,NLO,NSO,NLIN,NSIN)
	IF (NSO.EQ.NSIN) NSO=638
C
C	  Read ELAS format label
	CALL XVREAD(INP,INBUF,STAT,'LINE',1,'NSAMPS',1024,' ')
	IF(IINBUF(8).EQ.4321) THEN
	    LRECL=IINBUF(2)
	    NLINE=IINBUF(4)-IINBUF(3)+1
	    IF(NLO.EQ.NLIN) NLO=NLINE
	    NCHAN=IINBUF(7)
	    WRITE (MSG,100) 'bands', NCHAN
	    CALL XVMESSAGE(MSG,' ')
	    WRITE (MSG,100) 'lines', NLINE
	    CALL XVMESSAGE(MSG,' ')
  100	    FORMAT(' Number of ',A5,' =',I5)
	ELSE
C						    check for reverse byte order
	    CALL BSWAP(IINBUF(8),ITEST)
	    IF (ITEST .EQ. 4321) THEN
		CALL BSWAP(IINBUF(2),LRECL)
		CALL BSWAP(IINBUF(3),ISR)
		CALL BSWAP(IINBUF(4),IER)
		NLINE=IER-ISR+1
		IF(NLO.EQ.NLIN) NLO=NLINE
		CALL BSWAP(IINBUF(7),NCHAN)
		WRITE (MSG,100) 'bands', NCHAN
		CALL XVMESSAGE(MSG,' ')
		WRITE (MSG,100) 'lines', NLINE
		CALL XVMESSAGE(MSG,' ')
	    ELSE
		CALL XVMESSAGE(' *** No header record ***',' ')
		LRECL = 736
		CALL XVPARM('NCHAN',NCHAN,ICNT,IDEF,1)
		IF (NLO.EQ.NLIN) NLO=(NLO-1)/NCHAN
	    END IF
	END IF
C
	DO I=1,NCHAN
	    IOUTORD(I) = IORDER(I)
	END DO
	IF (NCHAN.GT.1) CALL SORTIN(IOUTORD,NCHAN)
C
	IF(NCHAN*NSO.GT.4000) THEN
	    CALL XVMESSAGE(' NCHAN*NS must be no more than 4000',' ')
	    CALL ABEND
	ENDIF
C
C		Open output(s)
	CALL XVUNIT(PIX,'OUT',1,STAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &			'U_NL',NLO,'U_NS',NSO,'U_NB',NCHAN,
     &			'U_ORG','BSQ','OP','WRITE',' ')
	    DO I=1,NCHAN
		DO J=1,NLO
		    CALL XVWRIT(PIX,OUTBUF,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(PIX,STAT,' ')
	    CALL XVOPEN(PIX,STAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','UPDATE',' ')
	ELSE
	    CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &			'U_NL',NLO,'U_NS',NSO,'U_NB',NCHAN,
     &			'U_ORG','BIL','OP','WRITE',' ')
	END IF
C
C	  For quick-look processing, don't write an auxiliary file
	IF(NO.GT.1) THEN
	    IF(XVPTST('ALL')) THEN
		NSO2 = 33
	    ELSE
		NSO2 = 16
	    END IF
	    CALL XVUNIT(AUX,'OUT',2,STAT,' ')
	    CALL XVOPEN(AUX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &		'U_NL',NLO,'U_NS',NSO2,'OP','WRITE','U_FORMAT','REAL',
     &		'O_FORMAT','REAL',' ')
	END IF
C
C	  *** Process data records ***
C
	FIRST = .TRUE.
	NBAD = 0
	LOC = 101 + SSO
	ELO = SLO + NLO - 1
	IREC=1+NCHAN*(SLO-1)
C
	DO ILINE=SLO,ELO
C
	    DO I=1,NCHAN
		IREC = IREC+1
		CALL XVREAD(INP,INBUF(5),STAT,'LINE',IREC,
     &			    'NSAMPS',LRECL,' ')
C
C			Use first data line to fill in first and second labels
		IF (FIRST) THEN
		    CALL LABFIL1(INBUF,LBL,1)
		    CALL XLADD(PIX,'HISTORY','INFO1',LBL,STAT,
     &			    'FORMAT','STRING',' ')
		    IF(NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO1',LBL,
     &					   STAT,'FORMAT','STRING',' ')
		    LBL = ' ' // LBL
		    CALL XVMESSAGE(LBL,' ')
C
		    CALL LABFIL1(INBUF,LBL,2)
		    CALL XLADD(PIX,'HISTORY','INFO2',LBL,STAT,
     &			    'FORMAT','STRING',' ')
		    IF(NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO2',LBL,
     &				 	STAT,'FORMAT','STRING',' ')
		    LBL = ' ' // LBL
		    CALL XVMESSAGE(LBL,' ')
C
		    FIRST = .FALSE.
		END IF
C
C	  Get true channel number
		ICHAN = INBUF(100)
		ICHAN = MOD(ICHAN,8)
		IF(ICHAN.NE.IORDER(I)) THEN
		    WRITE (LBL,200) ILINE, IORDER(I), ICHAN
  200		    FORMAT(' *** Error in Channel ID ***, Line',I6,
     +		           ' Channel',I2,' Reads',I2)
		    CALL XVMESSAGE(LBL,' ')
		    NBAD = NBAD+1
		    ICHAN = IORDER(I)
		    IF(NBAD.GE.MAXBAD) THEN
			CALL XVMESSAGE(' MAXBAD EXCEEDED',' ')
			CALL ABEND
		    ENDIF
		END IF
C
C				       Concatenate channels to form output image
		CALL MVL(INBUF(LOC),OUTBUF((ICHAN-1)*NSO+1),NSO)
C								  Auxiliary data
		IF(NO.GT.1) CALL AUXCON1(INBUF,AUXBUF,NSO2,ICHAN)
	    END DO
C							      Write output image
	    DO I=1,NCHAN
		CALL XVWRIT(PIX,OUTBUF(NSO*(IOUTORD(I)-1)+1),STAT,
     +			   'LINE',ILINE-SLO+1,'BAND',I,'NSAMPS',NSO,' ')
	    END DO
C					Write secondary output  (auxiliary data)
	    IF(NO.GT.1) CALL XVWRIT(AUX,AUXBUF,STAT,' ')
	END DO
C
C				      Fill in third label from last data record
	CALL LABFIL1(INBUF,LBL,3)
	CALL XLADD(PIX,'HISTORY','INFO3',LBL,STAT,'FORMAT','STRING',' ')
	IF(NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO3',LBL,STAT,
     &			'FORMAT','STRING',' ')
	LBL = ' ' // LBL
	CALL XVMESSAGE(LBL,' ')
C
C	  Close data sets
	CALL XVCLOSE(INP,STAT,' ')
	CALL XVCLOSE(PIX,STAT,' ')
	IF(NO.GT.1) CALL XVCLOSE(AUX,STAT,' ')
	RETURN
	END
C******************************************************************************
	SUBROUTINE AUXCON1(INBUF,RBUF,NSO2,ICHAN)
C
C	  This subroutine decodes tims auxiliary data
C
	REAL*4 SSPEED(4)/7.301,8.701,12.001,25.001/
	REAL*4 RBUF(33)
	LOGICAL*1 INBUF(2000)
C
C	  Blackbody values
	RBUF(2*ICHAN+1) = IV(INBUF(101))
	RBUF(2*ICHAN+2) = IV(INBUF(740))
C
C	  Reference temperatures
	IF(ICHAN.LE.1) THEN
	    I = IV(INBUF(13))
	    TEMP1 = 10*(I/32)+MOD(I,16)+.1*(IV(INBUF(14))/16)+.000001
	    IF (2*(I/32) .EQ. (I/16)) TEMP1 = -TEMP1
	    RBUF(1) = TEMP1
	    I = IV(INBUF(15))
	    TEMP2 = 10*(I/32)+MOD(I,16)+.1*(IV(INBUF(16))/16)+.000001
	    IF (2*(I/32) .EQ. (I/16)) TEMP2 = -TEMP2
	    RBUF(2)=TEMP2
	END IF
C
C	  Ambient Temperature #1
	I38 = IV(INBUF(38))
	I39 = IV(INBUF(39))
	I40 = IV(INBUF(40))
	RBUF(15) = 10*(I38/32) + MOD(I38,16) + 0.1*(I39/16)
	IF (2*(I38/32) .NE. (I38/16)) RBUF(15) = -RBUF(15)
C
C	  Ambient Temperature #2
	RBUF(16) = 10*(MOD(I39/2,8)) + (I40/16) + 0.1*(MOD(I40,16))
	IF (2*(I39/2) .NE. I39) RBUF(16) = -RBUF(16)
C
	IF(NSO2.NE.16) THEN
C
C	  Line count
	I = IV(INBUF(5))
	J = IV(INBUF(6))
	K = IV(INBUF(7))
	L = IV(INBUF(8))
	LINE = 1000000*(I/16) + 100000*MOD(I,16) + 10000*(J/16) +
     +	       1000*MOD(J,16) + 100*(K/16) + 10*MOD(K,16) + L/16
	IF (L/16 .NE. MOD(L,16)) CALL XVMESSAGE(
     +          ' *** Error in line count ***',' ')
	RBUF(22) = LINE
C
C	  Date
	I = IV(INBUF(9))
	J = IV(INBUF(10))
	K = IV(INBUF(11))
	RBUF(17) = 10*(I/16) + MOD(I,16)
	RBUF(16) = 10*(J/16) + MOD(J,16)
	RBUF(15) = 90 + K/16
C
C	  Sortie
	I = IV(INBUF(12))
	RBUF(21) = 100*MOD(K,16) + 10*(I/16) + MOD(I,16)
C
C	  Scan speed
	I = IV(INBUF(17))/64 + 1
	RBUF(24) = SSPEED(I)
C
C	  Time
	I = IV(INBUF(17))
	J = IV(INBUF(18))
	K = IV(INBUF(19))
	RBUF(18) = 10*MOD(I/16,4) + MOD(I,16)
	RBUF(19) = 10*MOD((J/16),8) + MOD(J,16)
	RBUF(20) = 10*MOD((K/16),8) + MOD(K,16)
C
C	  True heading
	I = IV(INBUF(22))
	J = IV(INBUF(23))
	RBUF(27) = 100*(I/64) + 10*MOD(I,16) + J/16 + 0.1*MOD(J,16)
C
C	  Pitch angle
	I = IV(INBUF(24))
	J = IV(INBUF(25))
	PITCH = 10*(I/16) + MOD(I,16) + 0.1*MOD(J,16) + 0.001
	IF (2*(J/32) .EQ. J/16) PITCH = -PITCH
	RBUF(28)=PITCH
C
C	  Roll angle
	I = IV(INBUF(26))
	J = IV(INBUF(27))
	ROLL = 100*MOD(I,128) + 10*MOD(I,16) + J/16 + .1*MOD(J,16) +.001
	IF (2*(J/32) .EQ. J/16) ROLL = -ROLL
	RBUF(29)=ROLL
C
C	  Latitude
	I = IV(INBUF(28))
	J = IV(INBUF(29))
	LATD = 10*(I/16) + MOD(I,16)
	LATM = 10*(J/32) + MOD(J,16)
	LATS=6*(IV(INBUF(30))/16)
	RLAT=LATD+LATM/60.+LATS/3600.
	IF (2*(J/32) .EQ. J/16) RLAT = -RLAT
	RBUF(25)=RLAT
C
C	  Longitude
	I = IV(INBUF(31))
	J = IV(INBUF(32))
	K = IV(INBUF(33))
	LONGD = 100*(K/128) + 10*(I/16) + MOD(I,16)
	LONGM = 10*(J/32) + MOD(J,16)
	LONGS = 6*MOD(K,16)
	RLONG = LONGD + LONGM/60.0 + LONGS/3600.0
	IF (2*(J/32) .EQ. J/16) RLONG = -RLONG
	RBUF(26) = RLONG
C
C	  Ground speed
	I = IV(INBUF(34))
	J = IV(INBUF(35))
	RBUF(30) = 1000*(I/64) + 100*MOD(I,16) + 10*(J/16) + MOD(J,16)
C
C	  Drift angle
	I = IV(INBUF(36))
	J = IV(INBUF(37))
	DRIFT = 10*(I/64) + MOD(I,16) + 0.1*(J/16) + 0.001
	IF (2*(I/32) .EQ. I/16) DRIFT = -DRIFT
	RBUF(31) = DRIFT
C
C	  Video gain setting
	RBUF(23) = MOD(IV(INBUF(100))/16,8)
C
C	  Ambient Temperature #1
	I38 = IV(INBUF(38))
	I39 = IV(INBUF(39))
	I40 = IV(INBUF(40))
	RBUF(32) = 10*(I38/32) + MOD(I38,16) + 0.1*(I39/16)
	IF (2*(I38/32) .NE. (I38/16)) RBUF(32) = -RBUF(32)
C
C	  Ambient Temperature #2
	RBUF(33) = 10*(MOD(I39/2,8)) + (I40/16) + 0.1*(MOD(I40,16))
	IF (2*(I39/2) .NE. I39) RBUF(33) = -RBUF(33)
C
	END IF
	RETURN
	END
C**************************************************************************
	SUBROUTINE LABFIL1(INBUF,LAB,LABNO)
C
C	  This routine fills in the three added labels
C
	REAL*4 SSPEED(4)/7.301,8.701,12.001,25.001/
	INTEGER IDAT(3)
	LOGICAL QSOUTH,QEAST
	LOGICAL*1 INBUF(2000)
	CHARACTER*80 LAB
	CHARACTER*15 TITLE
	CHARACTER*6 NAME(2)/'Start:','Stop: '/
C
	IF(LABNO.EQ.1) THEN
C
C	  *** First label ***
C
C	  Sortie
	    ISORTI=100*MOD(IV(INBUF(11)),16)+10*(IV(INBUF(12))/16)
     &		   +MOD(IV(INBUF(12)),16)
C
C	  Date
	    CALL IDATE(IDAT)
	    ITODAY = 10000*(IDAT(3)-1900) + 100*IDAT(2) + IDAT(1)
	    IDAY = 10*(IV(INBUF(9))/16) + MOD(IV(INBUF(9)),16)
	    IMONTH = 10*(IV(INBUF(10))/16) + MOD(IV(INBUF(10)),16)
	    IYEAR = 90 + IV(INBUF(11))/16
	    JDATE = 10000*IYEAR + 100*IMONTH + IDAY
	    IF (JDATE .GT. ITODAY) IYEAR = IYEAR - 10
C					if day or month is bad, trash the year
	    IF (IMONTH.LT.1 .OR. IMONTH.GT.12) IYEAR=0
	    IF (IDAY.LT.1 .OR. IDAY.GT.31) IYEAR=0
C
C	  Scan speed
	    ISPEED = IV(INBUF(17))/64 + 1
C
C	  Video gain
	    IGAIN = MOD(IV(INBUF(100))/16, 8)
C
C	  User supplied title
	    CALL XVP('TITLE',TITLE,ICNT)
C
	    WRITE(LAB,100) ISORTI,IMONTH,IDAY,IYEAR,TITLE,
     +                     SSPEED(ISPEED),IGAIN
  100	    FORMAT('TIMS Sortie',I4,I4,'/',I2,'/',I2,'  ',A15,
     +		   '  Scan Speed=',F4.1,'  Gain=',I1)
	ELSE
C
C	  *** Second and third labels ***
C
C	  Time
	    I = IV(INBUF(17))
	    J = IV(INBUF(18))
	    K = IV(INBUF(19))
	    IHOUR = 10*MOD(I/16,4) + MOD(I,16)
	    IMIN = 10*MOD((J/16),8) + MOD(J,16)
	    ISEC = 10*MOD((K/16),8) + MOD(K,16)
C
C	  Line count
	    I = IV(INBUF(5))
	    J = IV(INBUF(6))
	    K = IV(INBUF(7))
	    L = IV(INBUF(8))
	    LINE = 1000000*(I/16) + 100000*MOD(I,16) + 10000*(J/16) +
     +             1000*MOD(J,16) + 100*(K/16) + 10*MOD(K,16) + L/16
C
C	  Latitude
	    I = IV(INBUF(28))
	    J = IV(INBUF(29))
	    K = IV(INBUF(30))
	    LATD = 10*(I/16) + MOD(I,16)
	    LATM = 10*(J/32) + MOD(J,16)
	    LATS = 6*(K/16)
	    QSOUTH = 2*(J/32) .EQ. J/16
C
C	  Longitude
	    I = IV(INBUF(31))
	    J = IV(INBUF(32))
	    K = IV(INBUF(33))
	    LONGD = 100*(K/128) + 10*(I/16) + MOD(I,16)
	    LONGM = 10*(J/16) + MOD(J,16)
	    LONGS = 6*MOD(K,16)
	    QEAST = 2*(J/32) .NE. J/16
C
	    WRITE (LAB,200) NAME(LABNO-1),IHOUR,IMIN,ISEC,LINE,
     +                  LATD,LATM,LATS,LONGD,LONGM,LONGS
  200	    FORMAT (A6,I3,':',I2,':',I2,'  Scan=',I7,'  Lat=',I2,'D',I3,
     +              'M',I3,'S N  Long=',I3,'D',I3,'M',I3,'S W')
	    IF (IMIN.LT.10) LAB(11:11) = '0'
	    IF (ISEC.LT.10) LAB(14:14) = '0'
	    IF (QSOUTH) LAB(48:48) = 'S'
	    IF (QEAST) LAB(69:69) = 'E'
C
	END IF
	RETURN
	END
C***************************************************************************
C
C	    ** The AMESLOG subroutine is called when the data were **
C	    **  decommutated at NASA Ames in Moffett Field **
C
	SUBROUTINE AMESLOG
C
	COMMON /C1/ INBUF,OUTBUF
C
	INTEGER IORDER(6)/1,2,3,4,5,6/
	INTEGER IP1(6),IP2(6),IP3(6),IP4(6),IP5(6),IP6(6),IP7(6)
	INTEGER SLO,SSO,STAT,ELO,PIX,AUX
	REAL*4  AUXBUF(33)
	BYTE INBUF(4200),OUTBUF(4200)
	LOGICAL*1 FIRST
	LOGICAL XVPTST
	CHARACTER*80 LBL
C
C	  ***  Open input data set ***
C
	CALL XVUNIT(INP,'INP',1,STAT,' ')
	CALL XVOPEN(INP,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C	  ***  Parameter processing ***
C
	CALL XVPCNT('OUT',NO)
	CALL XVPARM('NCHAN',NCHAN,ICNT,IDEF,1)
	CALL XVPARM('ORDER',IORDER,ICNT,IDEF,6)
	CALL XVPARM('MAXBAD',MAXBAD,ICNT,IDEF,1)
	CALL XVSIZE(SLO,SSO,NLO,NSO,NLIN,NSIN)
	IF (NSO.EQ.NSIN) NSO=638
C
	IF(NCHAN*NSO.GT.4000) THEN
	    CALL XVMESSAGE(' NCHAN*NS must be no more than 4000',' ')
	    CALL ABEND
	ENDIF
C
C	  ***  Set pointers for each channel for each field  ***
C
	DO ICHAN = 1,NCHAN
	    IP1(ICHAN) = ((ICHAN-1)*698)+32
	    IP2(ICHAN) = 61+(IP1(ICHAN)-32)+SSO-1
	    IP3(ICHAN) = (ICHAN-1)*NSO+1
	    IP4(ICHAN) = IP1(ICHAN)-19
	    IP5(ICHAN) = IP4(ICHAN)+46
	    IP6(ICHAN) = (ICHAN-1)*349+19
	    IP7(ICHAN) = IP1(ICHAN)-31
	END DO
C
C	  ***  Open first output data set ***
C
	CALL XVUNIT(PIX,'OUT',1,STAT,' ')
	IF (XVPTST('BSQ')) THEN
	    CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &			'U_NL',NLO,'U_NS',NSO,'U_NB',NCHAN,
     &			'U_ORG','BSQ','OP','WRITE',' ')
	    DO I=1,NCHAN
		DO J=1,NLO
		    CALL XVWRIT(PIX,OUTBUF,ISTAT,' ')
		END DO
	    END DO
	    CALL XVCLOSE(PIX,STAT,' ')
	    CALL XVOPEN(PIX,STAT,'IO_ACT','SA','OPEN_ACT','SA',
     &	           'OP','UPDATE','O_FORMAT','BYTE',' ')
	ELSE
	    CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &		       'U_NL',NLO,'U_NS',NSO,'U_NB',NCHAN,
     &		       'U_ORG','BIL','OP','WRITE','O_FORMAT','BYTE',' ')
	END IF
C
C	  *** For standard processing, write an auxiliary file ***
C
	IF(NO.GT.1) THEN
	    IF(XVPTST('ALL')) THEN
		NSO2 = 33
	    ELSE
		NSO2 = 16
	    END IF
	    CALL XVUNIT(AUX,'OUT',2,STAT,' ')
	    CALL XVOPEN(AUX,STAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     &		NLO,'U_NS',NSO2,'U_ORG','BSQ','OP','WRITE',
     &		'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	ENDIF
C
C	  *** Process data records ***
C
	FIRST = .TRUE.
	NBAD = 0
	ELO = SLO + NLO - 1
C
	DO ILINE = SLO,ELO
	    CALL XVREAD(INP,INBUF,STAT,'LINE',ILINE,' ')
C
C	  *** Use first good data line to fill in first and second labels ***
C
	    IF(FIRST .AND. (INBUF(1).EQ.0)) THEN
		CALL LABFIL2(LBL,1)
		CALL XLADD(PIX,'HISTORY','INFO1',LBL,STAT,'FORMAT',
     &			   'STRING',' ')
		IF (NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO1',LBL,
     &		       STAT,'FORMAT','STRING',0)
		LBL = ' ' // LBL
		CALL XVMESSAGE(LBL,' ')
C
		CALL LABFIL2(LBL,2)
		CALL XLADD(PIX,'HISTORY','INFO2',LBL,STAT,'FORMAT',
     &			   'STRING',' ')
		IF (NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO2',LBL,
     &		       STAT,'FORMAT','STRING',' ')
		LBL = ' ' // LBL
		CALL XVMESSAGE(LBL,' ')
C
		FIRST = .FALSE.
	    END IF
C
	    DO ICHAN = 1,NCHAN
C
C	  ** Get true channel number, if data are good **
C	  ** Else check for Interpolated, Repeated and Zero fill data **
C
		IF(INBUF(IP7(ICHAN)).EQ.0) THEN
		    IF(INBUF(IP1(ICHAN)).NE.IORDER(ICHAN)) THEN
			WRITE (LBL,100) ILINE,IORDER(ICHAN),INBUF(I)
  100			FORMAT(' *** Error in channel ID ***, Line',
     +			       I6,' Channel',I2,' Reads',I2)
			CALL XVMESSAGE(LBL,' ')
			NBAD = NBAD + 1
			IF(NBAD.GT.MAXBAD) THEN
     			    CALL XVMESSAGE(' MAXBAD exceeded',' ')
			    CALL ABEND
			ENDIF
		    END IF
		ELSE
		   IF(INBUF(IP7(ICHAN)).EQ.10) THEN
			WRITE (LBL,200) ILINE
  200			FORMAT(' Interpolated data on line',I6)
			CALL XVMESSAGE(LBL,' ')
		    ELSE IF(INBUF(IP7(ICHAN)).EQ.20) THEN
			WRITE (LBL,300) ILINE
  300			FORMAT(' Repeated data on line',I6)
			CALL XVMESSAGE(LBL,' ')
		    ELSE IF(INBUF(IP7(ICHAN)).EQ.30) THEN
			WRITE (LBL,400) ILINE
  400			FORMAT(' Zero-filled data on line',I6)
			CALL XVMESSAGE(LBL,' ')
			GO TO 500
		    ELSE
			WRITE (LBL,410) ILINE
  410			FORMAT(' Data Staus Error on line',I6)
			CALL XVMESSAGE(LBL,' ')
		    END IF
		END IF
C
C	  ** Concatenate channels **
C
		CALL MVL(INBUF(IP2(ICHAN)),OUTBUF(IP3(ICHAN)),NSO)
C
	    END DO
C
C	  ** Auxiliary data **
C
	    IF(NO.GT.1) CALL AUXCON2(AUXBUF,NSO2,NCHAN,IP4,IP5,IP6)
C
C	  ** Write output image **
C
	    DO I=1,NCHAN
		CALL XVWRIT(PIX,OUTBUF(IP3(I)),STAT,'LINE',ILINE-SLO+1,
     &			    'BAND',I,' ')
	    END DO
C
C	  ** Write secondary output (auxiliary data) **
C
	    IF(NO.GT.1) CALL XVWRIT(AUX,AUXBUF,STAT,' ')
  500	    CONTINUE
	END DO
C
C	  ** Fill in third label from last data record **
C
	CALL LABFIL2(LBL,3)
	CALL XLADD(PIX,'HISTORY','INFO3',LBL,STAT,'FORMAT','STRING',' ')
	IF (NO.GT.1) CALL XLADD(AUX,'HISTORY','INFO3',LBL,STAT,
     &			'FORMAT','STRING',' ')
	LBL = ' ' // LBL
	CALL XVMESSAGE(LBL,' ')
C
C	  ** Close data sets **
C
	CALL XVCLOSE(INP,STAT,' ')
	CALL XVCLOSE(PIX,STAT,' ')
	IF(NO.GT.1) CALL XVCLOSE(AUX,STAT,' ')
	RETURN
	END
C***************************************************************************
	SUBROUTINE AUXCON2(RBUF,NSO2,NCHAN,IP4,IP5,IP6)
C
C	  This subroutine decodes tims auxiliary data
C
	COMMON /C1/ INBUF,OUTBUF
	REAL*4 RBUF(33), GAIN
	INTEGER IP4(6), IP5(6), IP6(6), IBUF(1050)
	INTEGER*2 HBUF(2100)
	LOGICAL*1 INBUF(4200), OUTBUF(4200)
	EQUIVALENCE (IBUF,HBUF,INBUF)
C
	DO ICHAN = 1,NCHAN
C
C	  ** Blackbody DN values **
C
	    ISTART = IP6(ICHAN)
	    IDNLO = HBUF(ISTART)
	    IDNHI = HBUF(ISTART+1)
	    RBUF(2*ICHAN+1)=IDNLO
	    RBUF(2*ICHAN+2)=IDNHI
	END DO
C
C	  ** Reference temperatures **
C
	TEMP1 = HBUF(7)/100.0
	RBUF(1) = TEMP1
	TEMP2 = HBUF(8)/100.0
	RBUF(2) = TEMP2
C
C	  ** Ambient Temperatures **
C
	RBUF(15) = HBUF(13)/10.0
	RBUF(16) = HBUF(14)/10.0
C
	IF(NSO2.NE.16) THEN
C
C	  ** Line count **
C
	    RBUF(22) = IBUF(2)
C
C	  ** Date **
C     *For date we convert an I*4*
C
	    I = IBUF(3)/1000
	    IYEAR = 90 + MOD(I,10)
	    RBUF(15) = IYEAR
	    I = I/10
	    RBUF(16) = MOD(I,100)
	    RBUF(17) = I/100
C
C
C	  ** Scan speed **
C
	    RBUF(24) = HBUF(9)/10.0
C
C	  ** Time **
C
	    RBUF(18) = HBUF(10)
	    RBUF(19) = HBUF(11)
	    RBUF(20) = HBUF(12)/10.0
C
C	  ** True heading **
C
	    RBUF(27) = HBUF(23)/10.0
C
C	  ** Pitch angle **
C
	    PITCH = HBUF(22)/10.0
	    IF(PITCH.LT.0) PITCH = -PITCH
	    RBUF(28) = PITCH
C
C	  ** Roll angle **
C
	    ROLL = HBUF(21)/10.0
	    IF(ROLL.LT.0) ROLL = -ROLL
	    RBUF(29) = ROLL
C
C	  ** Latitude **
C
	    RLAT = ABS(HBUF(24)) + HBUF(25)/600.0
	    IF (HBUF(24).LT.0) RLAT=-RLAT
	    RBUF(25) = RLAT
C
C	  ** Longitude **
C
	    RLONG = ABS(HBUF(26)) + HBUF(27)/600.0
	    IF (HBUF(26).LT.0) RLONG=-RLONG
	    RBUF(26) = RLONG
C
C	  ** Ground speed **
C
	    IGS = HBUF(28)
	    RBUF(30) = IGS
C
C	  ** Drift angle **
C
	    DRIFT = HBUF(29)/10.0
	    IF(DRIFT.LT.0) DRIFT = -DRIFT
	    RBUF(31) = DRIFT
C
C	  ** Video gain setting **
C
	    GAIN = HBUF(15)/1000.0
	    RBUF(23) = GAIN
C
C	  ** Ambient Temperatures **
C
	    RBUF(32) = HBUF(13)/10.0
	    RBUF(33) = HBUF(14)/10.0
C
	END IF
	RETURN
	END
C****************************************************************************
	SUBROUTINE LABFIL2(LAB,LABNO)
C
C     This routine fills in the three added labels
C
	COMMON /C1/ INBUF,OUTBUF
	INTEGER*4 IBUF(1050),IDAT(3)
	INTEGER*2 HBUF(2100)
	CHARACTER*80 LAB
	CHARACTER*15 TITLE
	CHARACTER*6 NAME(2)/'Start:','Stop: '/
	LOGICAL*1 INBUF(4200), OUTBUF(4200)
	EQUIVALENCE (IBUF,HBUF,INBUF)
C
	IF(LABNO.EQ.1) THEN
C
C	  *** First label ***
C
C	  ** Date **
C	*For date we convert an I*4*
C
	    CALL IDATE(IDAT)
	    ITODAY = 10000*(IDAT(3)-1900) + 100*IDAT(2) + IDAT(1)
	    I = IBUF(3)/1000
	    IYEAR = 90 + MOD(I,10)
	    I = I/10
	    IMONTH=MOD(I,100)
	    IDAY = I/100
	    JDATE = 10000*IYEAR + 100*IMONTH + IDAY
	    IF (JDATE .GT. ITODAY) IYEAR = IYEAR - 10
C					if day or month is bad, trash the year
	    IF (IMONTH.LT.1 .OR. IMONTH.GT.12) IYEAR=0
	    IF (IDAY.LT.1 .OR. IDAY.GT.31) IYEAR=0
C
C	  ** Scan speed **
C
	    SSPEED = HBUF(9)/10 + 0.0001
C
C	  ** Video gain setting **
C
	    GAIN = HBUF(15)/1000.0
C
C	  ** User supplied title **
C
	    CALL XVP('TITLE',TITLE,ICNT)
C
	    WRITE (LAB,100) IMONTH,IDAY,IYEAR,TITLE,SSPEED,GAIN
  100	    FORMAT('TIMS Data',I7,'/',I2,'/',I2,'  ',A15,
     +		   '     Scan Speed=',F4.1,'  Gain=',F3.1)
	ELSE
C
C	  *** Second and third labels ***
C
C	  ** Time **
C
	    IHOUR = HBUF(10)
	    IMIN = HBUF(11)
	    ISEC = HBUF(12)/10
C
C	  ** Line count **
C
	    LINE = IBUF(2)
C
C	  ** Latitude **
C
	    LATD = HBUF(24)
	    LATM = HBUF(25)/10
	    LATS = 6*MOD(HBUF(25),10)
C
C	  *** Longitude ***
C
	    LONGD = HBUF(26)
	    LONGM = HBUF(27)/10
	    LONGS = 6*MOD(HBUF(27),10)
C
	    WRITE (LAB,200) NAME(LABNO-1),IHOUR,IMIN,ISEC,LINE,
     +                      ABS(LATD),LATM,LATS,ABS(LONGD),LONGM,LONGS
  200	    FORMAT (A6,I3,':',I2,':',I2,'  Scan=',I8,' Lat=',I2,'D',I3,
     +              'M',I3,'S N  Long=',I3,'D',I3,'M',I3,'S W')
	    IF (IMIN.LT.10) LAB(12:12) = '0'
	    IF (ISEC.LT.10) LAB(15:15) = '0'
	    IF (LATD.LT.0)  LAB(49:49) = 'S'
	    IF (LONGD.GT.0) LAB(70:70) = 'E'
C
	END IF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE BSWAP(IN,OUT)
C
C	Routine to reverse the byte order of a 4-byte word.
C
	BYTE IN(4),OUT(4)
C
	OUT(1) = IN(4)
	OUT(2) = IN(3)
	OUT(3) = IN(2)
	OUT(4) = IN(1)
	RETURN
	END
