	INCLUDE 'VICMAIN_FOR'				      
	SUBROUTINE MAIN44
C
	REAL AVRAD(10)/9.97, 17.5, 30.6, 39.2, 46.8,
     +		       54.0, 59.4, 58.4, 25.8, 10.8/
	REAL VCAL(10)/  20.0, 317.0, 688.0, 729.0,1162.0,
     +		      1454.0, 850.0,1516.0,1169.0,1143.0/
	REAL GAIN(5)/ 0.5, 1.0, 2.0, 4.0, 8.0/
	CHARACTER*80 MSG
	REAL CALARR(20000,5),FACTOR(10)
	INTEGER*4 IARR(28)
	INTEGER*2 OBUF(716)
	LOGICAL QTCAL,XVPTST
	BYTE BUF(736)
C
	DO I=1,10
	    FACTOR(I) = 100.0 * (4000.0*AVRAD(I)) / (VCAL(I)*255.0)
	END DO
C						get DN for saturated input
	CALL XVPARM('SAT',ISAT,ICNT,IDEF,1)
C								open input
        CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')
        CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C								get & check size
        CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (NS .NE. 736) THEN
	    CALL XVMESSAGE('Number of samples must be 736',' ')
	    CALL ABEND
	ENDIF
	CALL XVBANDS(ISB,NB,NBI)
	IF (NB .NE. 12) THEN
	    CALL XVMESSAGE('Number of bands must be 12',' ')
	    CALL ABEND
	ENDIF
C								open output
        CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
        CALL XVOPEN(IOUT,STAT,'OP','WRITE','OPEN_ACT','SA',
     +		    'IO_ACT','SA','U_FORMAT','HALF','O_FORMAT','HALF',
     +		    'U_NS',716,'U_NL',NL,'U_NB',12,'U_ORG','BSQ',' ')
C
C						     add date and units to label
C
	CALL XVREAD(IUNIT,BUF,ISTAT,'BAND',1,'LINE',1,' ')
	DO I=1,3
	    CALL PARSE(BUF(I+5),IARR(2*I-1),IARR(2*I))
	END DO
	WRITE (MSG,100) IARR(3),IARR(4),IARR(1),IARR(2),IARR(5),IARR(6)
  100	FORMAT(2I1,'/',2I1,'/',2I1)
	CALL XLADD(IOUT,'HISTORY','DATE',MSG,ISTAT,'FORMAT','STRING',
     +		   ' ')
	CALL XLADD(IOUT,'HISTORY','CAL','Radiance at Sensor',ISTAT,
     +		   'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','1DN',
     +		   '10 milliWatts/square meter/steradian/micrometer',
     +		   ISTAT,'FORMAT','STRING',' ')
C
C					      get thermal calibration parameters
	CALL XVPARM('FILTER',NWTS,ICNT,IDEF,1)
	IF (XVPTST('ITEM')) THEN
	    QTCAL = .TRUE.
	    CALL XLADD(IOUT,'HISTORY','THERMAL',
     +		  '0.01 degrees Celsius',ISTAT,'FORMAT','STRING',' ')
	ELSE
	    QTCAL = .FALSE.
	    CALL XLADD(IOUT,'HISTORY','THERMAL',
     +	 	       'milliWatts/square meter/steradian/micrometer',
     +			ISTAT,'FORMAT','STRING',' ')
	END IF
C
C						loop thru reflectance channels
	DO IBAND=1,10
	    DO ILINE=1,NL
		CALL XVREAD(IUNIT,BUF,ISTAT,'BAND',IBAND,'LINE',ILINE,
     +			    ' ')
		CALL PARSE(BUF(18),IGAIN,ICHAN)
		IF (IGAIN.LT.1 .OR. IGAIN.GT.5) THEN
		    WRITE (MSG,200) IGAIN,IBAND,ILINE
  200		    FORMAT('Bad gain value of',I4,' found at Band ',
     +			   I3,' Line',I7)
		    CALL XVMESSAGE(MSG,' ')
		    IGAIN = MIN(5,MAX(IGAIN,1))
		END IF
		DO N=1,716
		    X = DNFIX(BUF(N+19))
		    IF (X .GE. 255.0) THEN
			IVAL = ISAT
		    ELSE
			IVAL = NINT(X*FACTOR(IBAND)/GAIN(IGAIN))
		    END IF
		    OBUF(N) = MIN(32767,MAX(IVAL,-32768))
		END DO
		CALL XVWRIT(IOUT,OBUF,ISTAT,' ')
	    END DO
	END DO
C						loop thru thermal channels
	DO IBAND=11,12
	    CALL READCAL(IUNIT,IBAND,NL,NWTS,CALARR)
	    IF (QTCAL) THEN
		CALL ITEMCAL(IUNIT,IOUT,IBAND,NL,ISAT,CALARR)
	    ELSE
		CALL IRADCAL(IUNIT,IOUT,IBAND,NL,ISAT,CALARR)
	    END IF
	END DO
C
	CALL XVCLOSE(IUNIT,STAT,' ')
	CALL XVCLOSE(IOUT,STAT,' ')
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE READCAL(IUNIT,IBAND,NL,NWTS,ARR)
C
	REAL ARR(20000,5)
	BYTE BUF(736)
C
C				read in blackbody temperatures and DN values
C				for all scan lines
	DO I=1,NL
	    CALL XVREAD(IUNIT,BUF,ISTAT,'BAND',IBAND,'LINE',I,' ')
	    CALL PARSE(BUF(9),ITENS,IONES)
C							adjust for sign bit
	    IF (ITENS .LT. 8) THEN
		ITENS = 8 - ITENS
	    ELSE
		ITENS = ITENS - 8
	    END IF
C
	    CALL PARSE(BUF(10),ITENTHS,IHUNDREDTHS)
	    ARR(I,2) = 10*ITENS + IONES + 0.1*ITENTHS + 0.01*IHUNDREDTHS
	    CALL PARSE(BUF(11),ITENS,IONES)
C							adjust for sign bit
	    IF (ITENS .LT. 8) THEN
		ITENS = 8 - ITENS
	    ELSE
		ITENS = ITENS - 8
	    END IF
C
	    CALL PARSE(BUF(12),ITENTHS,IHUNDREDTHS)
	    ARR(I,3) = 10*ITENS + IONES + 0.1*ITENTHS + 0.01*IHUNDREDTHS
	    ARR(I,4) = DNFIX(BUF(19))
	    ARR(I,5) = DNFIX(BUF(736))
	END DO
C						Adjust any unreasonable values
C
	IF (ARR(1,2).LT.-30.0 .OR. ARR(1,2).GT. 50.0) ARR(1,2)=ARR(2,2)
	IF (ARR(1,3).LT.-10.0 .OR. ARR(1,3).GT. 80.0) ARR(1,3)=ARR(2,3)
	DO I=2,NL
	    IF (ABS(ARR(I,2)-ARR(I-1,2)) .GT. 0.8) ARR(I,2)=ARR(I-1,2)
	    IF (ABS(ARR(I,3)-ARR(I-1,3)) .GT. 0.8) ARR(I,3)=ARR(I-1,3)
	    IF (ABS(ARR(I,4)-ARR(I-1,4)) .GT. 9.0) ARR(I,4)=ARR(I-1,4)
	    IF (ABS(ARR(I,5)-ARR(I-1,5)) .GT. 9.0) ARR(I,5)=ARR(I-1,5)
	END DO
C							filter the arrays
	DO I=1,4
	    CALL UNIFLT(7,NL,ARR(1,I+1),ARR(1,I),NWTS)
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE IRADCAL(IUNIT,IOUT,IBAND,NL,ISAT,CALARR)
C
	REAL CALARR(20000,5)
	REAL CENTWAV/10.75/
	INTEGER*2 OBUF(716)
	BYTE INBUF(716)
C
	DO ILINE=1,NL
	    BBTLO = CALARR(ILINE,1) + 273.15
	    BBTHI = CALARR(ILINE,2) + 273.15
	    DNLO = CALARR(ILINE,3)
	    DNHI = CALARR(ILINE,4)
	    RADLO = 1000.0*PLANCK(CENTWAV,BBTLO)
	    RADHI = 1000.0*PLANCK(CENTWAV,BBTHI)
	    SLOPE = (RADHI-RADLO)/(DNHI-DNLO)
	    YINT = RADLO - SLOPE*DNLO
	    CALL XVREAD(IUNIT,INBUF,ISTAT,'LINE',ILINE,'BAND',IBAND,
     +			'SAMP',20,'NSAMPS',716,' ')
	    DO ISAMP=1,716
		X = DNFIX(INBUF(ISAMP))
		IF (X .GE. 255.0) THEN
		    OBUF(ISAMP) = ISAT
		ELSE
		    IRAD = NINT(SLOPE*X + YINT)
		    OBUF(ISAMP) = MIN(32767,MAX(0,IRAD))
		END IF
	    END DO
	    CALL XVWRIT(IOUT,OBUF,ISTAT,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE ITEMCAL(IUNIT,IOUT,IBAND,NL,ISAT,CALARR)
C
	REAL CALARR(20000,5)
	REAL CENTWAV/10.75/
	INTEGER*2 OBUF(716),LUT(256)
	BYTE INBUF(716)
C
	DO ILINE=1,NL
	    BBTLO = CALARR(ILINE,1) + 273.15
	    BBTHI = CALARR(ILINE,2) + 273.15
	    DNLO = CALARR(ILINE,3)
	    DNHI = CALARR(ILINE,4)
	    RADLO = PLANCK(CENTWAV,BBTLO)
	    RADHI = PLANCK(CENTWAV,BBTHI)
	    SLOPE = (RADHI-RADLO)/(DNHI-DNLO)
	    YINT = RADLO - SLOPE*DNLO
	    DO IDN=1,255
		X = SLOPE*(IDN-1) + YINT
		ITEMP = NINT(100.0*(PLKINV(CENTWAV,X)-273.15))
		LUT(IDN) = MIN(32767,MAX(ITEMP,-32768))
	    END DO
	    LUT(256) = ISAT
C
	    CALL XVREAD(IUNIT,INBUF,ISTAT,'LINE',ILINE,'BAND',IBAND,
     +			'SAMP',20,'NSAMPS',716,' ')
	    DO ISAMP=1,716
		N = DNFIX(INBUF(ISAMP)) + 1
		OBUF(ISAMP) = LUT(N)
	    END DO
	    CALL XVWRIT(IOUT,OBUF,ISTAT,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PARSE(IN,NOUT1,NOUT2)
C
	BYTE IN
C
	IF (IN .GE. 0) THEN
	    N = IN
	ELSE
	    N = IN + 256
	END IF
	NOUT1 = N/16
	NOUT2 = MOD(N,16)
	RETURN
	END
C*******************************************************************************
	FUNCTION DNFIX(IN)
C
	BYTE IN
C
	IF (IN .GE. 0) THEN
	    DNFIX = IN
	ELSE
	    DNFIX = IN + 256.0
	END IF
	RETURN
	END
