C  REVISION HISTORY
C     5-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     7-97  RRD  ADDED NEW LABEL ITEMS FOR GRID SIZE 
c  2013-Jan-10 -lwk- initializations of MSG1/2 fixed for the new compiler flag on
c                    Solaris

      INCLUDE 'VICMAIN_FOR'
C Geometric Calibration Program "gridlocb"
C Locates grid intersections to sub-pixel accuracy given initial positions...
C           gridlocb (f.dat,tf.dat,iloc.dat) oloc NHOR=20 NVER=20
C
C Comment:  GOTO statements are neat!
C
      SUBROUTINE MAIN44
      PARAMETER (NPOINT = 7)
      COMMON/C1/LABEL,PAIRS(2,2500)

      REAL*4 A(2,2),WORK(2,NPOINT)
      INTEGER IUNIT(2),NL(2),NS(2),OUNIT,SAMP
      INTEGER*2 ARAY1(1024,1024), ARAY2(1024,1024),IBUFFER1(1024)
      CHARACTER*4320 LABEL
      CHARACTER*86 MSG1
      CHARACTER*72 MSG2

      CALL IFMESSAGE('GRIDLOCB version 14-JULY-97')
      MSG1 = 'NO ZERO CROSSING FOUND SEARCHING RECORD ****** OF FILTRD DATA SET * NEAR PIXEL ******'
      MSG2 = ' INTERSECTION AT LINE ********** SAMPLE ********** ********** **********'

C          Open the (filtered) input grid target images...
C            IUNIT(1) = filtered data set
C            IUNIT(2) = transposed/filtered data set
      DO I=1,2
          CALL XVUNIT(IUNIT(I),'INP',I,ISTAT,' ')
          CALL XVOPEN(IUNIT(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
          CALL XVGET(IUNIT(I),ISTAT,'NL',NL(I),'NS',NS(I),' ')
          CALL LABPROC(IUNIT(I),LABEL,NLAB)
      ENDDO
      DO 480 L = 1,NL(1)
        CALL XVREAD(IUNIT(1),IBUFFER1,ISTAT,' ')
        DO 470 LL = 1,NS(1)
          ARAY1(LL,L) = IBUFFER1(LL)
470     CONTINUE
480   CONTINUE
      DO 481 L = 1,NL(2)
        CALL XVREAD(IUNIT(2),IBUFFER1,ISTAT,' ')
        DO 471 LL = 1,NS(2)
          ARAY2(LL,L) = IBUFFER1(LL)
471     CONTINUE
481   CONTINUE
C
      I = 3
C            Read input (approximate) grid locations (mark format)
      CALL XVUNIT(INP,'INP',I,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL LABPROC(INP,LABEL,NLAB)
      CALL XVREAD(INP,PAIRS,ISTAT,' ')

	NHOR = 0
	NVER = 0
C---------GET GRID SIZE FROM INTERLOC MARK FILE LABEL
	  CALL XLGET(INP,'HISTORY','GRID_NROW',NHOR,IST,'HIST',
     1                'INTERLOC','INSTANCE',1,' ')
	  IF (IST .NE. 1) CALL XVMESSAGE('GRID_NROW NOT FOUND IN LABEL',
     1                                   ' ')

	  CALL XLGET(INP,'HISTORY','GRID_NCOL',NVER,IST,'HIST',
     1                'INTERLOC','INSTANCE',1,' ')
	  IF (IST .NE. 1) CALL XVMESSAGE('GRID_NCOL NOT FOUND IN LABEL',
     1                                   ' ')
C
C-------OVERRIDE WITH GRID SIZE FROM PARAMETERS
	CALL XVPARM('NHOR',NH,ICNTH,IDEF,0)
	IF (ICNTH .EQ. 1) NHOR=NH
	CALL XVPARM('NVER',NV,ICNTV,IDEF,0)
	IF (ICNTV .EQ. 1) NVER=NV
C
      NUMBER = NHOR*NVER
      IF (NUMBER .EQ. 0) GO TO 991
C
C Do for each intersection coordinate pair in the mark file
C
      DO 100 I=1,NUMBER
      LINE = PAIRS(1,I)
      SAMP = PAIRS(2,I)
      IF (PAIRS(1,I).EQ.-99.) GOTO 94

      DO 90 J=1,10

      DO 80 K=1,2      ! K=1 regular image, K=2 transposed image

      IF (K.EQ.1) THEN
           ITEM = SAMP
           IREC = LINE
      ELSE
           ITEM = LINE
           IREC = SAMP
      ENDIF
C           Find zero crossing for each point...
      IF (K .EQ. 1) THEN
      CALL FINDZERO(ARAY1,NL(K),NS(K),
     &         IREC,ITEM,NPOINT,WORK,*92)
      ENDIF
      IF (K .EQ. 2) THEN
      CALL FINDZERO(ARAY2,NL(K),NS(K),
     &         IREC,ITEM,NPOINT,WORK,*92)
      ENDIF
C           Fit points to straight line and return slope and offset...
      CALL FITLINE(WORK,NPOINT,A(1,K),A(2,K))
   80 CONTINUE
C           Solve the two equations simultaneously to find intersection...
      XA = (A(1,1)*A(2,2) + A(2,1)) /(1. - A(1,1)*A(1,2))
      YA = (A(1,2)*A(2,1) + A(2,2)) /(1. - A(1,1)*A(1,2))
      LX = NINT(XA)
      LY = NINT(YA)
C
      IF (LX.EQ.SAMP.AND.LY.EQ.LINE) GOTO 95
      LINE = LY
      SAMP = LX
      IF (J.EQ.10) CALL XVMESSAGE('*** Over 10 iterations',' ')
   90 CONTINUE

      GOTO 95
C
C            Here if no zero crossing found...
   92 WRITE(MSG1(40:46),'(I6)') LINE
      WRITE(MSG1(67:67),'(I1)') K
      WRITE(MSG1(80:85),'(I6)') SAMP
      CALL XVMESSAGE(MSG1(1:86),' ')
C            Flag intersections as missing...
   94 YA = -99.
      XA = -99.

C            Store refined intersection...
   95 PAIRS(1,I) = YA
      PAIRS(2,I) = XA
      WRITE(MSG2(23:32),'(F10.3)') YA
      WRITE(MSG2(41:50),'(F10.3)') XA
      WRITE(MSG2(52:61),'(I10)') LINE
      WRITE(MSG2(63:72),'(I10)') SAMP
      CALL XVMESSAGE(MSG2,' ')

  100 CONTINUE
C
C          Write refined intersections to output file...
      CALL XVUNIT(OUNIT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(OUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &          'OPEN_ACT','SA','IO_ACT','SA',
     &          'O_FORMAT','REAL','U_NL',1,'U_NS',2*NUMBER,' ')

C-----ADD THE GRID SIZE TO THE VICAR LABEL
      CALL XLADD(OUNIT,'HISTORY','GRID_NROW',NHOR,ISTAT,'FORMAT','INT',
     &           ' ')
      CALL XLADD(OUNIT,'HISTORY','GRID_NCOL',NVER,ISTAT,'FORMAT','INT',
     &           ' ')

C-----Write refined intersections to output file...
      CALL XVWRIT(OUNIT,PAIRS,ISTAT,' ')
      CALL XVCLOSE(OUNIT,ISTAT,' ')
      CALL XVMESSAGE('GRIDLOCB task completed',' ')
      RETURN
C
991	CALL XVMESSAGE('UNKNOWN GRID SIZE',' ')
	CALL ABEND
      END
C Routine to find the zero crossing for each point along the line
C Output: WORK will contain (line,samp) coordinates for NPOINTs.
C
      SUBROUTINE FINDZERO(PIC,NL,NS,IREC,ITEM,NPOINT,WORK,*)
      INTEGER*2 PIC(1024,1024)
      REAL*4 WORK(2,NPOINT)
      INTEGER SAMP,SAMP0

      LINE = IREC
      IF (IREC.LT.1.OR.IREC.GT.NS) RETURN1
      LMAX = MIN0(IREC+NPOINT/2,NL)
      LINE = MAX0(LMAX-NPOINT+1,1)
      SAMP0 = ITEM
C
      DO 70 L=1,NPOINT
      SAMP = SAMP0
C
   60 D1 = PIC(SAMP,LINE)
      D2 = PIC(SAMP+1,LINE)
C
      IF (D1.EQ.0) THEN
           SAMP0 = SAMP			!Exactly on zero crossing
           WORK(1,L) = LINE
           WORK(2,L) = SAMP0
           GOTO 70
      ENDIF

      IF (D1.GT.0.AND.D2.LT.0) THEN
           R = SAMP - D1/(D2-D1)	!Interpolate to find zero crossing...
           WORK(1,L) = LINE
           WORK(2,L) = R
           SAMP0 = R
           GOTO 70
      ENDIF

      IF (D1.GT.0) SAMP=SAMP+1
      IF (D1.LT.0) SAMP=SAMP-1
      IF(IABS(SAMP-SAMP0).LT.6
     &      .AND.SAMP.GT.0.AND.SAMP.LE.NS) GOTO 60
      RETURN1

   70 LINE = LINE + 1

      RETURN
      END
C Routine to fit points to a straight line and return the slope and offset
C 
      SUBROUTINE FITLINE(WORK,NPOINT,SLOPE,OFFSET)
      REAL*4 WORK(2,NPOINT)
      REAL*8 XMEAN,YMEAN,XY,SQ
C         Fit points to a straight line: Y = mX + b...
      XMEAN = 0.
      YMEAN = 0.
      XY = 0.
      SQ = 0.

      DO L=1,NPOINT
          XMEAN = XMEAN + WORK(1,L)
          YMEAN = YMEAN + WORK(2,L)
          XY = XY + WORK(1,L)*WORK(2,L)
          SQ = SQ + WORK(1,L)**2
      ENDDO

      D = NPOINT*SQ - XMEAN**2
      SLOPE = (NPOINT*XY-XMEAN*YMEAN)/D
      OFFSET = (SQ*YMEAN-XMEAN*XY)/D
      RETURN
      END
      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)
      IMPLICIT INTEGER(A-Z)
      INTEGER INSTANCES(20)
      CHARACTER*32 TASKS(20)
      CHARACTER*4320 LABEL
      CHARACTER*132 MSG
      CHARACTER*12 UNAME
      CHARACTER*28 TIME
      CHARACTER*65 HBUF

      HBUF = '----TASK:------------USER:--------------------------------
     &------'
      MSG = ' '
      LABEL = ' '
      CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0)
      CNT=20                             !EXTRACTS VIC*2 LAB
      CALL XLHINFO(IUNI,TASKS,INSTANCES,CNT,STAT,' ')
      DO 801 J=1,CNT
      UNAME = ' '
      TIME = ' '
      CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     *'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
      IF (STAT .NE. 1) CALL MABEND('ERROR:  BAD STAT')
      CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',TASKS(J),
     *'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
      IF (STAT .NE. 1) CALL MABEND('ERROR:  BAD STAT')
      HBUF(10:17) = TASKS(J)
      HBUF(27:38) = UNAME
      HBUF(39:64) = TIME
801   LABEL(1+(NLAB+J-1)*72:1+(NLAB+J-1)*72+64) = HBUF
      NLAB=NLAB+CNT
      DO 800 I=1,NLAB
      MSG = LABEL(1+(I-1)*72:1+(I-1)*72+71)
      CALL XVMESSAGE(MSG,' ')
800   MSG = ' '   
      RETURN
      END
