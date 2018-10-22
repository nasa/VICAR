	SUBROUTINE AXISPTS(XMIN,XMAX,BOT,TOP,NTIC)
C
C								scale axis
	X = ALOG10(XMAX-XMIN)
	IF (X.GE.0.0) THEN				! span between tic-marks
	    DIV = 10.0**INT(X)
	ELSE
	    DIV = 10.0**INT(X-1.0)
	END IF
	IF (XMIN.GE.0.0) THEN				! lower limit of graph
	    BOT = DIV*INT(XMIN/DIV)
	ELSE
	    BOT = DIV*INT((XMIN/DIV)-1.0)
	END IF
	NTIC = 1+(XMAX-BOT)/DIV				! # of tic-marks
	TOP = BOT+NTIC*DIV				! upper limit of graph
C
	IF (NTIC.LE.3) THEN				! adjust # of tic-marks
	    NTIC = 5*NTIC
	    DIV = DIV/5.0
	END IF
	IF (NTIC.LE.7) THEN
	    NTIC = 2*NTIC
	    DIV = DIV/2.0
	END IF
	DO WHILE (XMIN .GE. BOT+0.999999*DIV)
	    BOT = BOT+DIV
	    NTIC = NTIC-1
	END DO
	DO WHILE (XMAX .LE. TOP-0.999999*DIV)
	    TOP = TOP-DIV
	    NTIC = NTIC-1
	END DO
	RETURN
	END
