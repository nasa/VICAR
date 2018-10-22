	INCLUDE 'VICMAIN_FOR'
C        MODIFIED FOR VAX CONVERSION BY ASM, 6 SEPT 83
C        CONVERTED TO VICAR2 BY JHR  26 AUG 85
C        CORRECTED DIVIDE BY 0 IN GAIN COMPUTATION   SP 19 MAY 86
C	 ADD MEANSIG OPTION; ALLOW NON-BYTE FORMATS  REA 1 JUNE 87
C	 CONVERTED TO UNIX/VICAR                     REA 30 APRIL 91
C**********************************************************************
	SUBROUTINE MAIN44
	EXTERNAL MODE0,MODE1,MODE4
	REAL XMS(2)
	CHARACTER*8 FMT
	LOGICAL*4 XVPTST
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C							     open input data set
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL',
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
C						  get size information and check
	CALL XVSIZE(ISL,ISS,NLO,NSO,NLI,NSI)
	IF(ISL+NLO-1 .GT. NLI) THEN
	    CALL XVMESSAGE(
     +		 'number of lines requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
	IF(ISS+NSO-1 .GT. NSI) THEN
	    CALL XVMESSAGE(
     +		   'number of samples requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,'PIX_SIZE',IFMT,' ')
	IF (FMT.EQ.'REAL') IFMT=7
C
C        OPEN OUTPUT DATA SET
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &		'OPEN_ACT','SA','IO_ACT','SA','U_NL',NLO,'U_NS',NSO,' ')
C
C							      process parameters
C									START
	CALL XVPARM('START',IPARM,ICOUNT,IDEF,1)
	IF (ICOUNT.NE.0) THEN
	    ISTART = IPARM
	ELSE
	    ISTART = ISL
	END IF
C									LENGTH
	CALL XVPARM('LENGTH',IPARM,ICOUNT,IDEF,1)
	IF (ICOUNT.NE.0) THEN
	    IEND = ISTART+IPARM-1
	ELSE
	    IEND = ISL+NLO-1
	END IF
C									LINC
	CALL XVPARM('LINC',LINC,ICOUNT,IDEF,1)
C									FILTER
	CALL XVPARM('FILTER',IPARM,ICOUNT,IDEF,1)
	IF (ICOUNT.NE.0) THEN
	    NSW = IPARM
	ELSE
	    NSW = 1
	END IF
C									PERCENT
	CALL XVPARM('PERCENT',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.NE.0) THEN
	    MODE = 1
	    SAT = RPARM
	    IF (IFMT.NE.1) THEN
		CALL XVMESSAGE(
     +	      ' Only byte data is permitted with the PERCENT option',' ')
		CALL ABEND
	    ENDIF
	ELSE
	    MODE = 0
	END IF
C									GAIN
	CALL XVPARM('GAIN',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.NE.0) THEN
	    MODE = 2
	    GFAC = RPARM
	ELSE
	    GFAC = 100.0
	END IF
C									OFF
	CALL XVPARM('OFF',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.NE.0) THEN
	    MODE = 2
	    OFF = RPARM
	ELSE
	    OFF = 0.0
	END IF
C									NOSAT
	IF(XVPTST('NOSAT')) MODE=3
C									MEANSIG
	CALL XVPARM('MEANSIG',XMS,ICOUNT,IDEF,2)
	IF (ICOUNT.EQ.2) MODE=4
C									NOPRINT
	QPR = .NOT. XVPTST('NOPRINT')
C					compute lengths of arrays,  call STACKA
	II = 4*NSO
	JJ = II
	KK = II
	IF (MODE.EQ.1) THEN
	    CALL STACKA(6,MODE1,3,II,JJ,KK,QPR)
	ELSE IF (MODE.EQ.4) THEN
	    LL = 8*NSO
	    CALL STACKA(8,MODE4,4,II,JJ,KK,LL,QPR,XMS)
	ELSE
	    CALL STACKA(7,MODE0,3,II,JJ,KK,QPR,MODE)
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE MODE0(SUM,II,BUF,JJ,GAIN,KK,QPR,MODE)
C
	REAL GAIN(NSO),SUM(NSO),BUF(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C					   compute average DN for each sample
	CALL ZIA(SUM,NSO)
	NUM = 0
	DO I=ISTART,IEND,LINC
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     &			'NSAMPS',NSO,' ')
	    NUM = NUM+1
	    DO J=1,NSO
		SUM(J) = SUM(J)+BUF(J)
	    END DO
	END DO
	POP = NUM*NSO
	X = NUM
	GRANDSUM = 0.0
	DO I=1,NSO
	    GRANDSUM = GRANDSUM+SUM(I)
	    GAIN(I) = SUM(I)/X
	END DO
	XMEAN = GRANDSUM/POP
	CALL XVMESSAGE(' ',' ')
	WRITE(OUT,100) XMEAN
  100	FORMAT(' MEAN =',F10.3)
	CALL XVMESSAGE(OUT,' ')
C				   if necessary,  compute gain factor and offset
	IF(MODE.EQ.0) GFAC=XMEAN
	IF(NSW.NE.1) CALL FILTER(GAIN,SUM,NSW,NSO)
	IF(MODE.EQ.3) CALL MINMAX(7,NSO,GAIN,GFAC,XMAX,IMIN,IMAX)
	CALL WORK(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GFAC,OFF,
     &		  GAIN,BUF)
	RETURN
	END
C**********************************************************************
	SUBROUTINE MODE1(ISUM,II,IN,JJ,GAIN,KK,QPR)
C
	REAL GAIN(NSO)
	INTEGER*4 ISUM(NSO),IHIST(256)
	INTEGER*2 IN(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF',
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
C			   compute average DN for each sample and form histogram
	CALL ZIA(IHIST,256)
	CALL ZIA(ISUM,NSO)
	NUM = 0
	DO I=ISTART,IEND,LINC
	    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',I,'SAMP',ISS,
     +			'NSAMPS',NSO,' ')
	    NUM = NUM+1
	    DO J=1,NSO
		N = IN(J)
		ISUM(J) = ISUM(J)+N
		IHIST(N+1) = IHIST(N+1)+1
	    END DO
	END DO
	POP = NUM*NSO
	X = NUM
	DO I=1,NSO
	    GAIN(I) = ISUM(I)/X
	END DO
	ITOT = 0
	DO I=1,256
	    ITOT = ITOT+IHIST(I)*(I-1)
	END DO
	XMEAN = FLOAT(ITOT)/POP
	CALL XVMESSAGE(' ',' ')
	WRITE(OUT,100) XMEAN
  100	FORMAT(' MEAN =',F7.2)
	CALL XVMESSAGE(OUT,' ')
C				   		compute gain factor and offset
	NSAT = POP*SAT/200.0
	N = 0
	I = 1
	DO WHILE (N.LT.NSAT .AND. I.LE.256)
	    N = N+IHIST(I)
	    I = I+1
	END DO
	XLOW = FLOAT(I-1)/XMEAN
	N = 0
	I = 256
	DO WHILE(N.LT.NSAT .AND. I.GE.1)
	    N = N+IHIST(I)
	    I = I-1
	END DO
	HIGH = FLOAT(I+1)/XMEAN
	GFAC = 255.0/(HIGH-XLOW)
	OFF = -GFAC*XLOW
	IF(NSW.NE.1) CALL FILTER(GAIN,ISUM,NSW,NSO)
C					reset the data type for the I/O buffer
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL',
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL WORK(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GFAC,OFF,
     &		  GAIN,IN)
	RETURN
	END
C***********************************************************************
	SUBROUTINE MODE4(SUM,II,BUF,JJ,GAIN,KK,SUMSQ,LL,QPR,XMS)
C
	REAL*8 SUMSQ(NSO),Z
	REAL GAIN(NSO),SUM(NSO),BUF(NSO),XMS(2)
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C					   compute average DN for each sample
	CALL ZIA(SUM,NSO)
	NUM = 0
	DO I=ISTART,IEND,LINC
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     &			'NSAMPS',NSO,' ')
	    NUM = NUM+1
	    DO J=1,NSO
		Z = BUF(J)
		SUM(J) = SUM(J)+Z
		SUMSQ(J) = SUMSQ(J) + Z*Z
	    END DO
	END DO
	POP = NUM*NSO
	X = NUM
	DO I=1,NSO
	    Z = SUM(I)/X
	    BUF(I) = Z				! BUF now contains the means
	    VAR = SUMSQ(I)/X - Z*Z
	    IF (VAR.GT.0.0) THEN
		GAIN(I) = XMS(2)/SQRT(VAR)
	    ELSE
		GAIN(I) = 1.0
	    END IF
	END DO
	IF(NSW.NE.1) CALL FILTER(BUF,SUMSQ,NSW,NSO)
	DO I=1,NSO
	    SUM(I) = XMS(1) - GAIN(I)*BUF(I)	! SUM now contains the offsets
	END DO
	CALL WORK4(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GAIN,
     &		   SUM,BUF)
	RETURN
	END
C**********************************************************************
	SUBROUTINE WORK4(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GAIN,
     &			 OFF,BUF)
C
	REAL GAIN(NSO),OFF(NSO),BUF(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
C				      if necessary, print out all average values
	IF(QPR) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('     SAMPLE     GAIN VALUES',' ')
	    DO I=1,NSO,15
		II = MIN(I+14,NSO)
		WRITE (OUT,100) I,I+14,(GAIN(J),J=I,II)
  100		FORMAT(I5,'-',I5,15F8.2)
		CALL XVMESSAGE(OUT,' ')
	    END DO
C
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('     SAMPLE     OFFSET VALUES',' ')
	    DO I=1,NSO,15
		II = MIN(I+14,NSO)
		WRITE (OUT,100) I,I+14,(OFF(J),J=I,II)
		CALL XVMESSAGE(OUT,' ')
	    END DO
	END IF
C
	IF (IFMT.EQ.7) THEN
	    XMAX = 1.7E38
	    XMIN = -XMAX
	ELSE
	    DO I=1,NSO
		OFF(I) = OFF(I)+0.5
	    END DO
	    IF (IFMT.EQ.1) THEN
		XMIN = 0.0
		XMAX = 255.0
	    ELSE IF (IFMT.EQ.2) THEN
		XMIN = -32768.0
		XMAX = 32767.0
	    ELSE 
		XMIN = -2147483648.0
		XMAX = 2147483647.0
	    END IF
	END IF
C					read in each line, apply gain and offset
C					to each pixel, write each line back out
	IEL = ISL+NLO-1
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     +			'NSAMPS',NSO,' ')
	    DO J=1,NSO
		BUF(J) = MIN(XMAX,MAX(XMIN,GAIN(J)*BUF(J)+OFF(J)))
	    END DO
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NSO,' ')
	END DO
	RETURN
	END
C**********************************************************************
	SUBROUTINE WORK(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GFAC,
     &			OFF,GAIN,BUF)
C
	REAL GAIN(NSO),BUF(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
C				      if necessary, print out all average values
	IF(QPR) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('     SAMPLE     AVERAGE VALUES',' ')
	    DO I=1,NSO,15
		II = MIN(I+14,NSO)
		WRITE (OUT,100) I,I+14,(GAIN(J),J=I,II)
  100		FORMAT(I5,'-',I5,15F8.2)
		CALL XVMESSAGE(OUT,' ')
	    END DO
	END IF
C
C	   make the the gain = gain factor / average dn for that sample location
C
	WRITE (OUT,200) GFAC,OFF
  200	FORMAT(' GAIN',F8.2,'    OFFSET',F7.1)
	CALL XVMESSAGE(OUT,' ')
C
	IF (IFMT.EQ.1) THEN
	    XMIN = 0.0
	    XMAX = 255.0
	    OFF = OFF+0.5
	ELSE IF (IFMT.EQ.2) THEN
	    XMIN = -32768.0
	    XMAX = 32767.0
	    OFF = OFF+0.5
	ELSE IF (IFMT.EQ.4) THEN
	    XMIN = -2147483648.0
	    XMAX = 2147483647.0
	    OFF = OFF+0.5
	ELSE
	    XMAX = 1.7E38
	    XMIN = -XMAX
	END IF
	DO I=1,NSO
	    IF (GAIN(I) .NE. 0)  GAIN(I) = GFAC/GAIN(I)   !  avoid divide by 0.0
	END DO
C					read in each line, apply gain and offset
C					to each pixel, write each line back out
	IEL = ISL+NLO-1
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     +			'NSAMPS',NSO,' ')
	    DO J=1,NSO
		BUF(J) = MIN(XMAX,MAX(XMIN,GAIN(J)*BUF(J)+OFF))
	    END DO
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NSO,' ')
	END DO
	RETURN
	END
C**********************************************************************
	SUBROUTINE FILTER(GAIN,AVG,NSW,NSO)
C
	REAL GAIN(NSO),AVG(NSO)
C
	M = NSW/2
	SUM = 0.0
	DO I=1,M
	    SUM = SUM+GAIN(I)
	END DO
C					    filter window truncated at left edge
	J = 0
	N = M+1
	DO I=N,NSW
	    J = J+1
	    SUM = SUM+GAIN(I)
	    AVG(J) = SUM/FLOAT(I)
	END DO
C					 move filter window across to right edge
	X = NSW
	N = NSW+1
	DO I=N,NSO
	    SUM = SUM+GAIN(I)-GAIN(I-NSW)
	    J = J+1
	    AVG(J) = SUM/X
	END DO
C					   filter window truncated at right edge
	N = J-M
	L = NSO-M-1
	DO I=N,L
	    SUM = SUM-GAIN(I)
	    X = X-1.0
	    J = J+1
	    AVG(J) = SUM/X
	END DO
	DO I=1,NSO
	    GAIN(I) = AVG(I)
	END DO
	RETURN
	END
