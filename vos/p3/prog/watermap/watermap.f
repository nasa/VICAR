	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL WATER(500),CIBR(500),BUFOUT(10000),VAL(32767)
	INTEGER IBUF(10000)
	CHARACTER*80 PRT
	CHARACTER*40 FILNAM(2)
C						open input, get size and org
	CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','FULL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C			       					open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		    'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL','O_FORMAT','REAL',' ')
C							get remaining parameters
	CALL XVPARM('SCALE',SCALE,ICNT,IDEF,0)
	CALL XVPARM('INP',FILNAM,ICNT,IDEF,0)
C						    read in CIBR and H2O columns
	CALL READDS(FILNAM(2),WATER,CIBR,NVALS)
C						       compute linear regression
	N = 0
	SUMX = 0.0
	SUMY = 0.0
	SUMX2 = 0.0
	SUMXY = 0.0
	DO I=1,NVALS
	    IF (WATER(I).GT.0.0 .AND. CIBR(I).LT.1.0 .AND.
     +		CIBR(I).GT.0.0) THEN
		N = N+1
		X = ALOG(WATER(I))
		Y = ALOG(-1.0*ALOG(CIBR(I)))
		SUMX = SUMX + X
		SUMY = SUMY + Y
		SUMX2 = SUMX2 + X*X
		SUMXY = SUMXY + X*Y
		SUMY2 = SUMY2 + Y*Y
	    END IF
	END DO
	SLOPE = (SUMX*SUMY - N*SUMXY) / (SUMX*SUMX - N*SUMX2)
	OFFSET = (SUMY - SLOPE*SUMX) / N
	R2 = (SLOPE*SLOPE*(SUMX2 - (SUMX*SUMX/N))) / 
     +		(SUMY2 - (SUMY*SUMY/N))
	ALPHA = EXP(OFFSET)
	BETA = SLOPE
C							      update VICAR label
	CALL XLADD(IOUTUNIT,'HISTORY','ALPHA',ALPHA,ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUTUNIT,'HISTORY','BETA',BETA,ISTAT,
     +		   'FORMAT','REAL',' ')
	CALL XLADD(IOUTUNIT,'HISTORY','FIT',R2,ISTAT,
     +		   'FORMAT','REAL',' ')
	WRITE (PRT,100) ALPHA,BETA,R2
  100	FORMAT(' alpha = ',F7.5,'   beta = ',F7.5,'   fit = ',F6.4)
	CALL XVMESSAGE(PRT,' ')
	IF (ALPHA .LE. 0.0) THEN
	    CALL XVMESSAGE(' Invalid alpha value',' ')
	    CALL ABEND
	ENDIF
C								       build LUT
	X = -1.0/ALPHA
	Y = 1.0/BETA
	DO I=1,32767
	    Z = SCALE*I
	    IF (Z.LT.1.0) THEN
		VAL(I) = (X*LOG(Z))**Y
	    ELSE
		VAL(I) = -1.0
	    END IF
	END DO
C							      build output image
	IEL = ISL + NL - 1
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,IBUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',I,' ')
	    DO J=1,NS
		IF (IBUF(J) .GT. 0) THEN
		    BUFOUT(J) = VAL(IBUF(J))
		ELSE
		    BUFOUT(J) = 99999.0
		END IF
	    END DO
	    CALL XVWRIT(IOUTUNIT,BUFOUT,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C**************************************************************************
      SUBROUTINE READDS(FILENAME,COL1,COL2,NRECS)
C
C      This subroutine reads all records from the file FILENAME, and puts
C      the first numeric value that it finds in the COL1 array and the
C      second numeric value in the COL2 array.  If something other than a 
C      numeric or delimiter is encountered prior to finding two numeric fields, 
C      the entire line is discarded.
C
      CHARACTER*40 FILENAME
      REAL COL1(*),COL2(*)
C
      OPEN (51,FILE=FILENAME,STATUS='OLD')
      NRECS = 1
  100 CONTINUE
          READ (51,*,END=900,ERR=100) COL1(NRECS),COL2(NRECS)
	  NRECS = NRECS+1
          GO TO 100
  900 CONTINUE
      NRECS = NRECS-1
      RETURN
      END
