	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	March 21, 2003     ...rea...   Initial release
C	March 28, 2003     ...rea...   Change background fill value to -1.0
C
	IMPLICIT NONE
	EXTERNAL INVERT_FILE
	INTEGER INUNIT,IOUTUNIT,ISTAT,ISL,ISS,NL,NS,NLIN,NSIN
	INTEGER NLOUT,NSOUT,NUM,IDEF,LEN1,LEN2,LEN3,LEN4
C								      open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C								 get output size
	CALL XVPARM('NLOUT',NLOUT,NUM,IDEF,1)
	CALL XVPARM('NSOUT',NSOUT,NUM,IDEF,1)
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'U_NL',NLOUT,'U_NS',NSOUT,'U_NB',2,
     +		    'U_ORG','BIL','IO_ACT','SA','OPEN_ACT','SA','OP',
     +		    'WRITE','O_FORMAT','REAL','U_FORMAT','REAL', ' ')
C
	LEN1 = 4*NLIN*NSIN
	LEN2 = LEN1
	LEN3 = 4*NLOUT*NSOUT
	LEN4 = LEN3
	CALL STACKA(12,INVERT_FILE,4,LEN1,LEN2,LEN3,LEN4,INUNIT,
     +		    IOUTUNIT,NLIN,NSIN,NLOUT,NSOUT)
	RETURN
	END
C******************************************************************************
	SUBROUTINE INVERT_FILE(XLINE_IN,LEN1,SAMP_IN,LEN2,XLINE_OUT,
     +			       LEN3,SAMP_OUT,LEN4,INUNIT,IOUTUNIT,NLIN,
     +			       NSIN,NLOUT,NSOUT)
C
	IMPLICIT NONE
	REAL*8 X(2,4),Y(4),COEF_L(3),COEF_S(3),ARR1(12),ARR2(9),ARR3(3)
	REAL*8 CHISQ
	REAL*8 SIG(4)/4*1.0/
	REAL XLINE_IN(NSIN,NLIN),SAMP_IN(NSIN,NLIN),POINT(2)
	REAL XLINE_OUT(NSOUT,NLOUT),SAMP_OUT(NSOUT,NLOUT),CORNER(2,4)
	INTEGER LEN1,LEN2,LEN3,LEN4,INUNIT,IOUTUNIT,NLIN,NSIN,NLOUT
	INTEGER NSOUT,IL,IS,JL,JS,ISTAT,MINL,MINS,MAXL,MAXS
	LOGICAL INSIDE,QPOINT,QFIRST
C							      read in input grid
	DO IL=1,NLIN
	    CALL XVREAD(INUNIT,XLINE_IN(1,IL),ISTAT,'LINE',IL,'BAND',1,
     +			' ')
	    CALL XVREAD(INUNIT,SAMP_IN(1,IL),ISTAT,'LINE',IL,'BAND',2,
     +			' ')
	END DO
C							initialize output arrays
	DO IL=1,NLOUT
	    DO IS=1,NSOUT
		XLINE_OUT(IS,IL) = -99999.9
		SAMP_OUT(IS,IL) = -99999.9
	    END DO
	END DO
C					       looping through each 4-pixel cell
	DO IL=2,NLIN
	    DO IS=2,NSIN
		QPOINT = .TRUE.
		CALL FIND_RANGE(XLINE_IN(IS-1,IL-1),XLINE_IN(IS,IL-1),
     +				XLINE_IN(IS-1,IL),XLINE_IN(IS,IL),NLOUT,
     +				MINL,MAXL,QPOINT)
		IF (QPOINT) CALL FIND_RANGE(SAMP_IN(IS-1,IL-1),
     +				SAMP_IN(IS,IL-1),SAMP_IN(IS-1,IL),
     +				SAMP_IN(IS,IL),NSOUT,MINS,MAXS,QPOINT)
C
		IF (QPOINT) THEN
		    CORNER(1,1) =  SAMP_IN(IS-1,IL-1)
		    CORNER(2,1) =  XLINE_IN(IS-1,IL-1)
		    CORNER(1,2) =  SAMP_IN(IS,IL-1)
		    CORNER(2,2) =  XLINE_IN(IS,IL-1)
		    CORNER(1,3) =  SAMP_IN(IS,IL)
		    CORNER(2,3) =  XLINE_IN(IS,IL)
		    CORNER(1,4) =  SAMP_IN(IS-1,IL)
		    CORNER(2,4) =  XLINE_IN(IS-1,IL)
		    QFIRST = .TRUE.
C					  loop through range of inversion points
		    DO JL=MINL,MAXL
			DO JS=MINS,MAXS
			    POINT(1) = FLOAT(JS)
			    POINT(2) = FLOAT(JL)
C					 first check the upper left corner point
			    IF (POINT(1).EQ.CORNER(1,1) .AND.
     +					POINT(2).EQ.CORNER(2,1)) THEN
				XLINE_OUT(JS,JL) =  FLOAT(IL-1)
				SAMP_OUT(JS,JL) = FLOAT(IS-1)
C							     then check interior
			    ELSE IF (INSIDE(POINT,CORNER,4)) THEN
				IF (QFIRST) THEN
C							compute the fitting 
C						       cofficients for this cell
				    CALL MVE(9,8,CORNER,X,1,1)
				    Y(1) = DFLOAT(IL-1)
				    Y(2) = DFLOAT(IL-1)
				    Y(3) = DFLOAT(IL)
				    Y(4) = DFLOAT(IL)
				    CALL SVDFIT(X,Y,SIG,4,2,1,3,COEF_L,
     +						ARR1,ARR2,ARR3,CHISQ)
				    Y(1) = DFLOAT(IS-1)
				    Y(2) = DFLOAT(IS)
				    Y(3) = DFLOAT(IS)
				    Y(4) = DFLOAT(IS-1)
				    CALL SVDFIT(X,Y,SIG,4,2,1,3,COEF_S,
     +						ARR1,ARR2,ARR3,CHISQ)
				    QFIRST = .FALSE.
				END IF
				XLINE_OUT(JS,JL) = COEF_L(1)*FLOAT(JS) +
     +					COEF_L(2)*FLOAT(JL) + COEF_L(3)
				SAMP_OUT(JS,JL) = COEF_S(1)*FLOAT(JS) +
     +					COEF_S(2)*FLOAT(JL) + COEF_S(3)
			    END IF
			END DO
		    END DO
		END IF
	    END DO
	END DO
C							      write output image
	DO IL=1,NLOUT
	    CALL XVWRIT(IOUTUNIT,XLINE_OUT(1,IL),ISTAT,' ')
	    CALL XVWRIT(IOUTUNIT,SAMP_OUT(1,IL),ISTAT,' ')
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FIND_RANGE(X1,X2,X3,X4,IEND,LOW,IHI,QPOINT)
C
C	This routine finds the low (LOW) and high (IHI) integers that are
C	spanned by X1 through X4.  The range is truncated to the range 1 
C	to IEND. If no integers are spanned by X1-X4, then QPOINT is set
C	to FALSE.
C
	IMPLICIT NONE
	REAL X1,X2,X3,X4,X
	INTEGER IEND,LOW,IHI,I
	LOGICAL QPOINT
C
	X = MIN(X1,X2,X3,X4)
	IF (X .LE. 0.0) THEN
	    QPOINT = .FALSE.
	    RETURN
	END IF
	I = INT(X)
	IF (X .EQ. FLOAT(I)) THEN
	    LOW = I
	ELSE
	    LOW = I + 1
	END IF
C
	IHI = MIN(IEND,INT(MAX(X1,X2,X3,X4)))
	IF (LOW .GT. IHI) QPOINT = .FALSE.
C
	RETURN
	END
