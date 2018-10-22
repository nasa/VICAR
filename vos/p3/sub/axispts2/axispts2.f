	SUBROUTINE AXISPTS2(XMIN,XMAX,NTIC)
C
	INTEGER NVAL(100)/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     +			  11,12,13, 7, 5, 8,17, 9, 1,10,
     +			   7,11, 1, 8, 5,13, 9, 7, 1,10,
     +			   1, 8,11, 2, 7, 6, 1, 2,13,10,
     +			   1, 7, 1,11, 9, 2, 1, 8, 7,10,
     +			   3,13, 1, 9,11, 7, 3, 2, 1, 6,
     +			   1, 2, 9, 8,13,11, 1, 4,13, 7,
     +			   1, 9, 1, 2, 5, 4,11,13, 1, 8,
     +			   9, 2, 1, 7, 5, 2, 3,11, 1, 9,
     +			   7, 4, 3, 2, 5, 8, 1, 7,11,10/
C
C								scale axis
	DELTA = XMAX - XMIN
	X = ALOG10(DELTA)
	IF (X.GE.0.0) THEN				! span between tic-marks
	    DIV = 10.0**(INT(X)-1)
	ELSE
	    DIV = 10.0**(INT(X)-2)
	END IF
	N = DELTA/DIV + 0.5
	NTIC = NVAL(N)
	RETURN
	END
