	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL GAIN(10),OFFSET(30000,27)
	INTEGER*2 INBUF(766),OUTBUF(766)
	LOGICAL XVPTST,QTEMP
C
C		Open input image data set
C
	CALL XVUNIT(IPIX,'INP',1,ISTAT,' ')
	CALL XVOPEN(IPIX,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		    'U_FORMAT','HALF',' ')
C
C		Parameter processing
C
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVPARM('FILTER',NWTS,NUM,IDEF,' ')
	QTEMP = XVPTST('TEMP') .OR. XVPTST('ITEM')
C
C		Set some pointers
C
	IEL = ISL + NL - 1
	IES = ISS + NS - 1
	IF(IEL.GT.NLIN.OR.IES.GT.NSIN) THEN
	    CALL XVMESSAGE
     &		(' Size field specified overruns file dimensions',' ')
	    CALL ABEND
	ENDIF
C
C		Open output data set, add labels
C
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA','O_FORMAT',
     &		    'HALF','OP','WRITE','U_NS',NS,'U_FORMAT','HALF',
     &		    'U_NL',NL,' ')
	CALL XLADD(IOUT,'HISTORY','LBL1',
     &		   'Instrument Perceived Radiance Image',ISTAT,
     &		   'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','DN',
     &		   'microwatts/cm*cm*sr*micrometer',
     &		   ISTAT,'FORMAT','STRING',' ')
	IF (QTEMP) CALL XLADD(IOUT,'HISTORY','AND_DN',
     &		   'hundredths degrees Celsius for Channels 11 & 12',
     &		   ISTAT,'FORMAT','STRING',' ')
C
C						Generate buffers of AUX data
	CALL READCAL(NWTS,ISL,NL,GAIN,OFFSET)
C
	WRITE (6,*)
	IAUX = 0
	DO ILINE=ISL,IEL
	    IAUX = IAUX + 1
	    DO ICHAN=1,10
		CALL XVREAD(IPIX,INBUF,STAT,'LINE',ILINE,'SAMP',ISS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    X = GAIN(ICHAN)*INBUF(ISAMP) + OFFSET(IAUX,ICHAN)
		    OUTBUF(ISAMP) = MIN(32767.0, MAX(-32768.0,X)) + 0.5
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,' ')
	    END DO
	    DO ICHAN=11,12
		CALL XVREAD(IPIX,INBUF,STAT,'LINE',ILINE,'SAMP',ISS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    X = OFFSET(IAUX,ICHAN+2)*INBUF(ISAMP) +
     &			OFFSET(IAUX,ICHAN)
		    IF (QTEMP) X = 100.0*(PLKINV(11.0,X/1000.0)-273.15)
		    OUTBUF(ISAMP) = MIN(32767.0, MAX(-32768.0,X)) + 0.5
		END DO
		CALL XVWRIT(IOUT,OUTBUF,ISTAT,' ')
	    END DO
	END DO
C
C		Close all datasets
C
	CALL XVCLOSE(IPIX,STAT,' ')
	CALL XVCLOSE(IOUT,STAT,' ')
C
	RETURN
	END
C
C***********************************************************************
	SUBROUTINE READCAL(NWTS,ISL,NL,GAIN,X)
C
	REAL GAIN(10),X(30000,27),BUF(42)
	INTEGER LOC(26)/31,32,33,34,35,36,37,38,39,40,
     +			 3, 5, 7, 9,11,13,15,17,19,21,23,24,25,26,1,2/
C							Set the gain vlaues
	GAIN(1) = 77.2
	GAIN(2) = 50.4
	GAIN(3) = 62.3
	GAIN(4) = 62.8
	GAIN(5) = 89.9
	GAIN(6) = 100.2
	GAIN(7) = 82.2
	GAIN(8) = 33.9
	GAIN(9) = 10.0
	GAIN(10) = 4.1
C							Open the AUX dataset
	CALL XVUNIT(IUNIT,'INP',2,ISTAT,' ')
	CALL XVOPEN(IUNIT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
C						    Read each line of cal data,
	LINE = ISL
	DO I=1,NL
	    CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'NSAMPS',42,' ')
	    LINE = LINE+1
	    DO J=1,26
		X(I,J) = BUF(LOC(J))
	    END DO
	END DO
C						    Check for reasonable values
	DO J=11,24
	    IF (X(1,J).LT.0.0 .OR. X(1,J).GT.255.0) X(1,J)=X(2,J)
	    DO I=2,NL
		IF (ABS(X(I,J)-X(I-1,J)) .GT. 10.0) X(I,J)=X(I-1,J)
	    END DO
	END DO
	DO J=25,26
	    IF (X(1,J).LT.-20.0 .OR. X(1,J).GT.75.0) X(1,J)=X(2,J)
	    DO I=2,NL
		IF (ABS(X(I,J)-X(I-1,J)) .GT. 0.8) X(I,J)=X(I-1,J)
	    END DO
	END DO
C							Filter arrays
	DO J=26,11,-1
	    CALL UNIFLT(7,NL,X(1,J),X(1,J+1),NWTS)
	END DO
	DO J=10,1,-1
	    CALL MEDFLT(X(1,J),X(1,J+1),NL,NWTS)
	END DO
C							Build offset arrays
	DO J=1,10
	    GN = GAIN(J)
	    L1 = J + 1
	    L2 = L1 + 10
	    DO I=1,NL
		X(I,J) = GN*X(I,L1)*X(I,L2) 
	    END DO
	END DO
	DO I=1,NL
	    RADLO = 1000.0*PLANCK(11.0, X(I,26)+273.15)
	    RADHI = 1000.0*PLANCK(11.0, X(I,27)+273.15)
	    X(I,13) = (RADHI-RADLO)/(X(I,23)-X(I,22))
	    X(I,14) = (RADHI-RADLO)/(X(I,25)-X(I,24))
	    X(I,11) = RADLO - X(I,13)*X(I,22)
	    X(I,12) = RADLO - X(I,14)*X(I,24)
	END DO
C
	CALL XVCLOSE(IUNIT,ISTAT,' ')
	RETURN
	END
C**********************************************************
	SUBROUTINE MEDFLT(BUFIN,BUFOUT,NUM,NWTS)
C
	REAL BUFIN(NUM),BUFOUT(NUM),WINDOW(75)
C
	NWT = MIN(75,NWTS)		! filter width (max width = 75)
	NWT2 = NWT/2			! half of filter width
	MED = NWT2 + 1			! median location in window
C
C	                                                initialize window
	DO I=1,NWT2
	    WINDOW(I) = BUFIN(1)
	    WINDOW(I+NWT2) = BUFIN(I)
	END DO
	WINDOW(NWT) = BUFIN(MED)
	CALL SORTM(WINDOW,NWT)
	BUFOUT(1) = WINDOW(MED)
C						    compute value for each pixel
	DO II=2,NUM
C							add right pixel,
C						 remove left pixel from window
	    N = MAX(II-MED,1)
	    XREM = BUFIN(N)
	    N = MIN(II+NWT2,NUM)
	    XADD = BUFIN(N)
	    IF (XADD .GT. XREM) THEN
		I = 1
		DO WHILE (WINDOW(I) .NE. XREM)
		    I = I+1
		END DO
		DO WHILE (I.LT.NWT .AND. WINDOW(I+1).LT.XADD)
		    WINDOW(I) = WINDOW(I+1)
		    I = I+1
		END DO
		WINDOW(I) = XADD
	    ELSE IF (XADD .LT. XREM) THEN
		I = NWT
		DO WHILE (WINDOW(I) .NE. XREM)
		    I = I-1
		END DO
		DO WHILE (I.GT.1 .AND. WINDOW(I-1).GT.XADD)
		    WINDOW(I) = WINDOW(I-1)
		    I = I-1
		END DO
		WINDOW(I) = XADD
	    END IF
C
	    BUFOUT(II) = WINDOW(MED)
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SORTM(ARR,NUM)
C
C	This is an NlogN sort,  of NUM values in the real array ARR,
C	low to high
C
	REAL ARR(NUM)
C
	ISPAN = NUM/2
	DO WHILE (ISPAN .GT. 0)
	    LAST = NUM - ISPAN
	    LOC = 1
	    DO WHILE (LOC .LE. LAST)
		I1 = LOC
		DO WHILE (I1 .GE. 1)
		    I2 = I1 + ISPAN
		    IF (ARR(I1) .LE. ARR(I2)) GO TO 100
		    HOLD = ARR(I1)
		    ARR(I1) = ARR(I2)
		    ARR(I2) = HOLD
		    I1 = I1 - ISPAN
		END DO
  100		CONTINUE
		LOC = LOC + 1
	    END DO
	    ISPAN = ISPAN/2
	END DO
C
	RETURN
	END
