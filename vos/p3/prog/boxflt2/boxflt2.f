	INCLUDE 'VICMAIN_FOR'
C**********************************************************************
	SUBROUTINE MAIN44
C
C        MODIFIED FOR VAX CONVERSION BY ALAN MAZER 28-JUL-83
C        CONVERTED TO VICAR2 BY J. REIMER 14-AUG-85
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C        5-91  REA  REWRITTEN
C	 4-03  REA  EXTEND keyword added
C
	EXTERNAL LOW,HIGH
	CHARACTER*4 FORMAT
	LOGICAL XVPTST,QLCYCLE,QSCYCLE,QEXTEND,QINT
	COMMON /BXFLT2/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,
     +			DC,XMIN,XMAX,QLCYCLE,QSCYCLE,QEXTEND,QINT
C
C						open datasets, check size field
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +		  'IO_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF(ISL+NL-1 .GT. NLIN) THEN
	    CALL XVMESSAGE(
     +              ' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
	    CALL ABEND
	ENDIF
	IF(ISS+NS-1 .GT. NSIN) THEN
	    CALL XVMESSAGE(
     +          ' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
	    CALL ABEND
	ENDIF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &		  'OPEN_ACT','SA','IO_ACT','SA','U_NL',NL,'U_NS',NS,' ')
C
C							      process parameters
 	CALL XVP('DCLEVEL',DC,ICOUNT)
	CALL XVP('NLW',NLW,ICOUNT)
	CALL XVP('NSW',NSW,ICOUNT)
	IF (XVPTST('CYCLE')) THEN
	    QLCYCLE = .TRUE.
	    QSCYCLE = .TRUE.
	ELSE
	    QLCYCLE = XVPTST('LCYCLE')
	    QSCYCLE = XVPTST('SCYCLE')
	END IF
	QEXTEND = XVPTST('EXTEND')
C
	IF(NLW/2*2.EQ.NLW) CALL XVMESSAGE(
     +                            ' WARNING-NLW IS AN EVEN INTEGER',' ')
	IF(NSW/2*2.EQ.NSW) CALL XVMESSAGE(
     +                            ' WARNING-NSW IS AN EVEN INTEGER',' ')
C
C							determine cutoff values
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FORMAT,' ')
	IF (FORMAT .EQ. 'BYTE') THEN
	    XMIN = 0.0
	    XMAX = 255.0
	    QINT = .TRUE.
	ELSE IF (FORMAT .EQ. 'HALF') THEN
	    XMIN = -32768.0
	    XMAX = 32767.0
	    QINT = .TRUE.
	ELSE IF (FORMAT .EQ. 'FULL') THEN
	    XMIN = -2147483000
	    XMAX =  2147483000
	    QINT = .TRUE.
	ELSE
	    XMIN = -1.0E33
	    XMAX =  1.0E33
	    QINT = .FALSE.
	END IF
C
	NSX = NS + NSW
	NLWX = NLW
	NSO = NS
	N1 = 4*NLWX*NSX				! bytes in input array
	N2 = 4*NSO				! bytes in output array
C
	IF (XVPTST('HIGHPASS')) THEN
	    CALL STACKA(7,HIGH,2,N1,N2,NSX,NLWX,NSO)
	ELSE
	    CALL STACKA(7,LOW,2,N1,N2,NSX,NLWX,NSO)
	END IF
C
	CALL XVCLOSE(IUNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
	RETURN
	END
C**********************************************************************
	SUBROUTINE LOW(BUF,N1,OUT,N2,NSX,NLWX,NSO)
C
	REAL BUF(NSX,NLWX),OUT(NSO)
	LOGICAL QLCYCLE,QSCYCLE,QEXTEND,QINT
	COMMON /BXFLT2/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,
     +			DC,XMIN,XMAX,QLCYCLE,QSCYCLE,QEXTEND,QINT
C
	PIXELS = NLW*NSW		! number of pixels in filter window
	NLW2 = NLW/2			! half of the line weights
	NSW2 = NSW/2			! half of the sample weights
	IST = MAX(ISS-NSW2,1)		! first sample to be read in
	LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
	NLEFT = IST - ISS + NSW2	! number of pixels to pad on left
	NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
	NSREAD = LAST - IST + 1		! number of samples to be read
	IEND = NLEFT + NSREAD		! location of final pixel in inp buffer
	LOC = NLW			! line position in input buffer
C
C						Set up input buffer
	DO LINE = ISL+NLW2, ISL-NLW2, -1
	    IF (LINE .GE. 1) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       NLIN+LINE,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE IF (QEXTEND) THEN
			CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW2+1),BUF(1,LOC),1,1)
		ELSE
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC - 1
	END DO
C
	LOC = 1					!location for next line into BUF
	LINE = ISL + NLW2 +1			!line number of next line
C						   looping thru all output lines
	DO L=1,NL
C							   initialize window sum
	    SUM = 0.0
	    DO I=1,NLW
		DO J=1,NSW
		    SUM = SUM + BUF(J,I)
		END DO
	    END DO
C						    compute value for each pixel
	    DO ISAMP=1,NS
		OUT(ISAMP) = SUM/PIXELS
		IF (QINT) OUT(ISAMP) = NINT(OUT(ISAMP))
		DO I=1,NLW
		    SUM = SUM + BUF(ISAMP+NSW,I) - BUF(ISAMP,I)
		END DO
	    END DO
	    CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	    IF (LINE .LE. NLIN) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       LINE-NLIN,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    LOC2 = LOC - 1
		    IF (LOC2 .LT. 1) LOC2 = NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		ELSE
		    LOC2 = LOC - (LINE - NLIN)
		    IF (LOC2 .LT. 1) LOC2=LOC2+NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC + 1
	    IF (LOC .GT. NLW) LOC = 1
	    LINE = LINE + 1
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE HIGH(BUF,N1,OUT,N2,NSX,NLWX,NSO)
C
	REAL BUF(NSX,NLWX),OUT(NSO)
C
	LOGICAL QLCYCLE,QSCYCLE,QEXTEND,QINT
	COMMON /BXFLT2/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,
     +			DC,XMIN,XMAX,QLCYCLE,QSCYCLE,QEXTEND,QINT
C
	PIXELS = NLW*NSW		! number of pixels in filter window
	NLW2 = NLW/2			! half of the line weights
	NSW2 = NSW/2			! half of the sample weights
	IST = MAX(ISS-NSW2,1)		! first sample to be read in
	LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
	NLEFT = IST - ISS + NSW2	! number of pixels to pad on left
	NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
	NSREAD = LAST - IST + 1		! number of samples to be read
	IEND = NLEFT + NSREAD		! location of final pixel in inp buffer
	LOC = NLW			! line position in input buffer
C
C						Set up input buffer
	DO LINE = ISL+NLW2, ISL-NLW2, -1
	    IF (LINE .GE. 1) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       NLIN+LINE,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE IF (QEXTEND) THEN
			CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW2+1),BUF(1,LOC),1,1)
		ELSE
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC - 1
	END DO
C
	LOC = 1					!location for next line into BUF
	LINE = ISL + NLW2 +1			!line number of next line
	INLOC = NLW2 + 1			!location of current line in BUF
C						   looping thru all output lines
	DO L=1,NL
C							   initialize window sum
	    SUM = 0.0
	    DO I=1,NLW
		DO J=1,NSW
		    SUM = SUM + BUF(J,I)
		END DO
	    END DO
C						    compute value for each pixel
	    DO ISAMP=1,NS
		OUT(ISAMP) = BUF(NSW2+ISAMP,INLOC) - SUM/PIXELS + DC
		OUT(ISAMP) = MIN(XMAX, MAX(XMIN, OUT(ISAMP)))
		IF (QINT) OUT(ISAMP) = NINT(OUT(ISAMP))
		DO I=1,NLW
		    SUM = SUM + BUF(ISAMP+NSW,I) - BUF(ISAMP,I)
		END DO
	    END DO
	    CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	    IF (LINE .LE. NLIN) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       LINE-NLIN,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    LOC2 = LOC - 1
		    IF (LOC2 .LT. 1) LOC2 = NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		ELSE
		    LOC2 = LOC - (LINE - NLIN)
		    IF (LOC2 .LT. 1) LOC2=LOC2+NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC + 1
	    IF (LOC .GT. NLW) LOC = 1
	    INLOC = INLOC +1
	    IF (INLOC .GT. NLW) INLOC = 1
	    LINE = LINE + 1
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE CYCLE(BUF,NLEFT,NRIGHT,LEN)
C
	REAL BUF(LEN)
C
	N = LEN - NLEFT - NRIGHT
	DO I=1,NLEFT
	    BUF(I) = BUF(I+N)
	END DO
	DO I=LEN-NRIGHT+1, LEN
	    BUF(I) = BUF(I-N)
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE REFLCT(BUF,NLEFT,NRIGHT,LEN)
C
	REAL BUF(LEN)
C
	DO I=1,NLEFT
	    BUF(I) = BUF(2*NLEFT-I+1)
	END DO
	DO I=1,NRIGHT
	    BUF(LEN-I+1) = BUF(LEN-2*NRIGHT+I)
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE EXTEND(BUF,NLEFT,NRIGHT,LEN)
C
	REAL BUF(LEN)
C
	DO I=1,NLEFT
	    BUF(I) = BUF(NLEFT+1)
	END DO
	DO I=1,NRIGHT
	    BUF(LEN-I+1) = BUF(LEN-NRIGHT)
	END DO
	RETURN
	END
