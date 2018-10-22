	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	11 MAR 1992  ...REA...    New Program
C
	LOGICAL XVPTST
	CHARACTER*4 FMT
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','OP','WRITE',' ')
C
	CALL XVPCNT('OUT',NOUT)
	IF (NOUT .EQ. 2) THEN
	    CALL XVUNIT(IOUT2,'OUT',2,ISTAT,' ')
	    CALL XVOPEN(IOUT2,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','HALF','O_FORMAT','HALF',
     +			'OP','WRITE',' ')
	END IF
C								get parameters
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,' ')
	IF (FMT.EQ.'REAL') THEN
	    ROUND = 0.0
	ELSE
	    ROUND = 0.5
	END IF
	CALL XVPARM('BACKGRND',BACKGRND,ICNT,IDEF,0)
C
	IF (XVPTST('LINEAR')) THEN
	    IF (NOUT .NE. 2) THEN
		CALL LININTERP(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +			       ROUND)
	    ELSE
		CALL LININTERP2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +			        BACKGRND,ROUND)
	    END IF
	ELSE
	    IF (NOUT .NE. 2) THEN
		CALL CUBICSPLINE(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +				 ROUND)
	    ELSE
		CALL CUBICSPLINE2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +				  BACKGRND,ROUND)
	    END IF
	END IF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE LININTERP(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +			     ROUND)
C
	REAL BUF(15000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find first contour line
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) GO TO 250
	    END DO
C							no contours found
	    WRITE (PRT,200) LINE
  200	    FORMAT(' No contour lines found on Line',I5)
	    CALL XVMESSAGE(PRT,' ')
	    GO TO 300
C						extend the first contour's value
C						to the start of the line
  250	    CONTINUE
	    II = ISAMP + 1
	    X1 = ISAMP
	    Y1 = BUF(ISAMP)
	    ISTART = II
	    DO IPIX=1,II-2
		BUF(IPIX) = Y1
	    END DO
C						search for the next contour and
C						and interpolate pixels between
	    DO ISAMP=II,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    DELTA = (BUF(ISAMP)-Y1) / (ISAMP-X1)
		    DO IPIX=ISTART,ISAMP-1
			BUF(IPIX) = Y1 + (IPIX-X1)*DELTA + ROUND
		    END DO
		    X1 = ISAMP
		    Y1 = BUF(ISAMP)
		    ISTART = ISAMP + 1
		END IF
	    END DO
C							fill remainder of line
	    DO IPIX=ISTART,NS
		BUF(IPIX) = Y1
	    END DO
C
  300	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE LININTERP2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +			      BACKGRND,ROUND)
C
	REAL BUF(15000)
	INTEGER*2 BUF2(15000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find first contour line
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) GO TO 250
	    END DO
C							no contours found
	    WRITE (PRT,200) LINE
  200	    FORMAT(' No contour lines found on Line',I5)
	    CALL XVMESSAGE(PRT,' ')
	    DO ISAMP=1,NS
		BUF2(ISAMP) = 32767
	    END DO
	    GO TO 300
C						extend the first contour's value
C						to the start of the line
  250	    CONTINUE
	    X1 = ISAMP
	    Y1 = BUF(ISAMP)
	    ISTART = ISAMP + 1
	    DO IPIX=1,ISAMP
		BUF(IPIX) = Y1
		BUF2(IPIX) = ISAMP - IPIX
	    END DO
C						search for the next contour and
C						and interpolate pixels between
	    II = ISTART
	    DO ISAMP=II,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    DELTA = (BUF(ISAMP)-Y1) / (ISAMP-X1)
		    DO IPIX=ISTART,ISAMP-1
			XX = IPIX
			BUF(IPIX) = Y1 + (XX-X1)*DELTA + ROUND
			BUF2(IPIX) = MIN(XX-X1,ISAMP-XX)
		    END DO
		    BUF2(ISAMP) = 0
		    X1 = ISAMP
		    Y1 = BUF(ISAMP)
		    ISTART = ISAMP + 1
		END IF
	    END DO
C							fill remainder of line
	    DO IPIX=ISTART,NS
		BUF(IPIX) = Y1
		BUF2(IPIX) = IPIX - ISTART + 1
	    END DO
C
  300	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    CALL XVWRIT(IOUT2,BUF2,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE CUBICSPLINE(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +				ROUND)
C
	REAL BUF(15000),X(5000),Y(5000),Y2(5000),U(5000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find all contours that
C							cross this line
	    NUM = 1
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    NUM = NUM + 1
		    X(NUM) = ISAMP
		    Y(NUM) = BUF(ISAMP)
		END IF
	    END DO
C
	    IF (NUM .EQ. 1) THEN
		WRITE (PRT,200) LINE
  200		FORMAT(' No contour lines found on Line',I5)
		CALL XVMESSAGE(PRT,' ')
	    ELSE
C						copy the first and last values
C						to beyond the line endpoints
C						to force good behavior at ends
		X(1) = 0.0
		Y(1) = Y(2)
		NUM = NUM + 1
		X(NUM) = NS + 1
		Y(NUM) = Y(NUM-1)
C						fit a spline
		CALL SPLINEA(X,Y,Y2,U,NUM)
		CALL SPLINEB(BUF,X,Y,Y2,NUM,NS,ROUND)
	    END IF
C
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE CUBICSPLINE2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +				BACKGRND,ROUND)
C
	REAL BUF(15000),X(5000),Y(5000),Y2(5000),U(5000)
	INTEGER*2 BUF2(15000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find all contours that
C							cross this line
	    NUM = 1
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    NUM = NUM + 1
		    X(NUM) = ISAMP
		    Y(NUM) = BUF(ISAMP)
		END IF
	    END DO
C
	    IF (NUM .EQ. 1) THEN
		WRITE (PRT,200) LINE
  200		FORMAT(' No contour lines found on Line',I5)
		CALL XVMESSAGE(PRT,' ')
		DO IPIX=1,NS
		    BUF2(IPIX) = 32767
		END DO
	    ELSE
C						copy the first and last values
C						to beyond the line endpoints
C						to force good behavior at ends
		X(1) = 0.0
		Y(1) = Y(2)
		NUM = NUM + 1
		X(NUM) = NS + 1
		Y(NUM) = Y(NUM-1)
C						fit a spline
		CALL SPLINEA(X,Y,Y2,U,NUM)
		CALL SPLINEB(BUF,X,Y,Y2,NUM,NS,ROUND)
C						fill in the distance buffer
		I1 = X(2)
		DO IPIX = 1,I1
		    BUF2(IPIX) = I1 - IPIX
		END DO
C
		LOC = 3
		I2 = X(LOC)
		I3 = X(NUM-1)
		DO IPIX=I1+1,I3
		    BUF2(IPIX) = MIN(IPIX-I1,I2-IPIX)
		    IF (IPIX .EQ. I2) THEN
			LOC = LOC + 1
			I1 = I2
			I2 = X(LOC)
		    END IF
		END DO
C
		DO IPIX=I3+1,NS
		    BUF2(IPIX) = IPIX - I3
		END DO
	    END IF
C
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    CALL XVWRIT(IOUT2,BUF2,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SPLINEA(X,Y,Y2,U,NUM)
C
C	This routine computes the
	REAL X(*),Y(*),Y2(*),U(*)
C
	Y2(1) = 0.0
	U(1) = 0.0
	QN = 0.0
	UN = 0.0
C
	DO I=2,NUM-1
	    SIG   = (X(I)-X(I-1)) / (X(I+1)-X(I-1))
	    P     = SIG*Y2(I-1) + 2.0
	    Y2(I) = (SIG-1.0) / P
	    U(I)  = (6.0*((Y(I+1)-Y(I))/(X(I+1)-X(I)) - (Y(I)-Y(I-1)) /
     .		    (X(I)-X(I-1))) / (X(I+1)-X(I-1)) - SIG*U(I-1)) / P
	END DO
C
	Y2(NUM) = (UN - QN*U(NUM-1))  /  (QN*Y2(NUM-1) + 1.0)
	DO I=NUM-1,1,-1
	    Y2(I) = Y2(I)*Y2(I+1) + U(I)
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SPLINEB(BUF,X,Y,Y2,NUM,NS,ROUND)
C
	REAL BUF(*),X(*),Y(*),Y2(*)
C
	LOC1 = 1
	LOC2 = 2
	WIDTH = X(LOC2) - X(LOC1)
	DO ISAMP=1,NS
	    XX = ISAMP
	    IF (XX .GT. X(LOC2)) THEN
		LOC1 = LOC2
		LOC2 = LOC2 + 1
		WIDTH = X(LOC2) - X(LOC1)
	    END IF
	    A = (X(LOC2) - XX) / WIDTH
	    B = (XX - X(LOC1)) / WIDTH
	    BUF(ISAMP) = A*Y(LOC1) + B*Y(LOC2) + ROUND +
     +		 ((A*A*A-A)*Y2(LOC1)+(B*B*B-B)*Y2(LOC2))*WIDTH*WIDTH/6.0
	END DO
C						
	RETURN
	END
