	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C       10-91  REA  REWRITTEN
C	10-02  REA  Second input file option added. The second input, if used,
C		    holds the filter window, and overrides the NLW, NSW, SYMM,
C		    and WEIGHTS parameters.
C
	PARAMETER (MAXSIZE=90000)
	EXTERNAL FILT
	REAL*4 WTS(MAXSIZE),BUF(5000)
	CHARACTER*4 FORMAT
	LOGICAL XVPTST,QLCYCLE,QSCYCLE,QINT
	COMMON /FLTCOM/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,
     +			OFFSET,GAIN,DIV,XMIN,XMAX,QLCYCLE,QSCYCLE,QINT
C
C						open datasets, check size field
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +		  'IO_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF(ISL+NL-1 .GT. NLIN) THEN
	    CALL XVMESSAGE(
     +             ' Number of lines requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
	IF(ISS+NS-1 .GT. NSIN) THEN
	    CALL XVMESSAGE(
     +             ' Number of samples requested exceeds input size',
     +		   ' ')
	    CALL ABEND
	ENDIF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &		  'OPEN_ACT','SA','IO_ACT','SA','U_NL',NL,'U_NS',NS,' ')
C
C						determine default cutoff values
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
C							  check for second input
C							 and process, if present
	CALL XVPCNT('INP',ICOUNT)
	IF (ICOUNT .EQ. 2) THEN
	    CALL XVUNIT(INUNIT2,'INP',2,ISTAT,' ')
	    CALL XVOPEN(INUNIT2,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +			'IO_ACT','SA',' ')
	    CALL XVGET(INUNIT2,ISTAT,'NL',NLW,'NS',NSW,' ')
	    INDEX = 1
	    DO I=1,NLW
		IF (INDEX+NSW-1 .GT. MAXSIZE) THEN
		    CALL XVMESSAGE(
     +			'The filter window is too large to process',' ')
		    CALL ABEND
		ENDIF
		CALL XVREAD(INUNIT2,WTS(INDEX),ISTAT,' ')
		INDEX = INDEX + NSW
	    END DO
	ELSE
C						process NLW, NSW, SYMM & WEIGHTS

	    CALL XVP('NLW',NLW,ICOUNT)
	    CALL XVP('NSW',NSW,ICOUNT)
C							    build weights matrix
	    CALL XVPARM('WEIGHTS',BUF,ICOUNT,IDEF,0)
	    IF (XVPTST('ALL') .OR. XVPTST('ASYMMETR')) THEN
		IF (ICOUNT .NE. NLW*NSW) THEN
		    CALL XVMESSAGE(
     +			' NLW*NSW weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		CALL MVE(7,ICOUNT,BUF,WTS,1,1)
	    ELSE IF (XVPTST('UPPER').OR.XVPTST('NONSYMME')) THEN
		IF (ICOUNT .NE. NSW*((NLW+1)/2)) THEN
		    CALL XVMESSAGE(
     +		  'NSW*((NLW+1)/2) weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		CALL MVE(7,ICOUNT,BUF,WTS,1,1)
		LOC1 = ICOUNT - 2*NSW + 1
		LOC2 = ICOUNT + 1
		DO I=1,NLW/2
		    CALL MVE(7,NSW,BUF(LOC1),WTS(LOC2),1,1)
		    LOC1 = LOC1 - NSW
		    LOC2 = LOC2 + NSW
		END DO
	    ELSE IF (XVPTST('LEFT')) THEN
		IF (ICOUNT .NE. NLW*((NSW+1)/2)) THEN
		    CALL XVMESSAGE(
     +		    'NLW*((NSW+1)/2) weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		LOC1 = 1
		LOC2 = 1
		NUM = (NSW+1)/2
		DO I=1,NLW
		    CALL MVE(7,NUM,BUF(LOC1),WTS(LOC2),1,1)
		    CALL MVE(7,NUM-1,BUF(LOC1+NUM-2),WTS(LOC2+NUM),-1,1)
		    LOC1 = LOC1 + NUM
		    LOC2 = LOC2 + NSW
		END DO
	    ELSE
		IF (ICOUNT .NE. ((NLW+1)/2)*((NSW+1)/2) ) THEN
		    CALL XVMESSAGE(
     +	' ((NLW+1)/2) * ((NSW+1)/2) weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		LOC1 = 1
		LOC2 = 1
		NUM = (NSW+1)/2
		DO I=1,(NLW+1)/2
		    CALL MVE(7,NUM,BUF(LOC1),WTS(LOC2),1,1)
		    CALL MVE(7,NUM-1,BUF(LOC1+NUM-2),WTS(LOC2+NUM),-1,1)
		    LOC1 = LOC1 + NUM
		    LOC2 = LOC2 + NSW
		END DO
		LOC1 = LOC1 - 2*NUM
		DO I=1,(NLW-1)/2
		    CALL MVE(7,NUM,BUF(LOC1),WTS(LOC2),1,1)
		    CALL MVE(7,NUM-1,BUF(LOC1+NUM-2),WTS(LOC2+NUM),-1,1)
		    LOC1 = LOC1 - NUM
		    LOC2 = LOC2 + NSW
		END DO
	    END IF
	END IF
C
	IF((NLW/2)*2 .EQ. NLW) THEN
	    CALL XVMESSAGE(' NLW cannot be an even integer',' ')
	    CALL ABEND
	ENDIF
	IF((NSW/2)*2 .EQ. NSW) THEN
	    CALL XVMESSAGE(' NSW cannot be an even integer',' ')
	    CALL ABEND
	ENDIF
C
	CALL XVP('SCALE',BUF,ICOUNT)
	OFFSET = BUF(1)
	GAIN = BUF(2)
C
	CALL XVP('RANGE',BUF,ICOUNT)
	IF (ICOUNT .EQ. 2) THEN
	    XMIN = BUF(1)
	    XMAX = BUF(2)
	ELSE
	    CALL XVP('DNMIN',BUF,ICOUNT)
	    IF (ICOUNT .EQ. 1) XMIN = BUF(1)
	    CALL XVP('DNMAX',XMAX,ICOUNT)
	    IF (ICOUNT .EQ. 1) XMAX = BUF(1)
	END IF
C							edge treatment
	IF (XVPTST('CYCLE')) THEN
	    QLCYCLE = .TRUE.
	    QSCYCLE = .TRUE.
	ELSE
	    QLCYCLE = XVPTST('LCYCLE')
	    QSCYCLE = XVPTST('SCYCLE')
	END IF
	CALL XVP('DIVIDE',DIV,ICOUNT)
	IF ( ICOUNT .EQ. 0) THEN
	    DIV = 0.0
	    DO I=1,NLW*NSW
		DIV = DIV + WTS(I)
	    END DO
	END IF
	IF (DIV .EQ. 0.0) DIV = 1.0
C
	NSX = NS + NSW
	NSO = NS
	N1 = 4*NLW*NSX				! bytes in input array
	N2 = 4*NSO				! bytes in output array
C
	CALL STACKA(9,FILT,2,N1,N2,WTS,NSX,NLW,NSW,NSO)
C
	CALL XVCLOSE(IUNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
	RETURN
	END
C**********************************************************************
	SUBROUTINE FILT(BUF,N1,OUT,N2,WTS,NSX,NLW,NSW,NSO)
C
	REAL BUF(NSX,NLW),OUT(NSO),WTS(NSW,NLW)
	LOGICAL QLCYCLE,QSCYCLE,QINT
	COMMON /FLTCOM/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,
     +			OFFSET,GAIN,DIV,XMIN,XMAX,QLCYCLE,QSCYCLE,QINT
C
	NLW2 = NLW/2			! half of the line weights
	NSW2 = NSW/2			! half of the sample weights
	IST = MAX(ISS-NSW2,1)		! first sample to be read in
	LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
	NLEFT = IST - ISS + NSW2	! number of pixels to pad on left
	NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
	NSREAD = LAST - IST + 1		! number of samples to be read
	LOC = NLW			! line position in input buffer
C
C						Set up input buffer
	DO LINE = ISL+NLW2, ISL-NLW2, -1
	    IF (LINE .GE. 1) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       NLIN+LINE,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
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
C						    compute value for each pixel
	    DO ISAMP=1,NS
		SUM = 0.0
		DO I=1,NLW
		    LLOC = MOD(LOC+I-2,NLW) + 1
		    DO J=1,NSW
			SUM = SUM + WTS(J,I)*BUF(ISAMP+J-1,LLOC)
		    END DO
		END DO
		SUM = GAIN*(SUM/DIV) + OFFSET
		OUT(ISAMP) = MIN(MAX(SUM,XMIN), XMAX)
		IF (QINT) OUT(ISAMP) = NINT(OUT(ISAMP))
	    END DO
	    CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	    IF (LINE .LE. NLIN) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
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
