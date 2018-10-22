      INCLUDE 'VICMAIN_FOR'
C     VAX CONVERSION BY ASM, JAN 1984
C     REVISION TO HANDLE F2 BEING CONVERTED TO VICAR2
C     ADD 'NOCENTER OPTION    -- REA 4JUN86
C     REVISED TO UNIX/VICAR   -- REA 3SEP91
C
C**********************************************************************
C
      SUBROUTINE MAIN44
      INCLUDE 'pgminc'
      EXTERNAL RAT,DIFF,LNRAT,LNDIFF
      REAL RPAR(2)
      INTEGER IPAR(4),IPOP(500),JPOP(500),PARB(xprdim)
      LOGICAL XVPTST
      LOGICAL*1 QLOG,QDIFF,QDISP,QFILT,QCENTER
      CHARACTER*60 FUNCSTR,FUNCSTR2
      COMMON/PARB/PARB
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      VALUE(I) = FLOAT(I-1)/SCALE+BOTTOM

      call xvmessage( '*** p3/RATIO0 version Sep 9 2015 ***',' ')

C
C       OPEN INPUT DATASETS
C
      CALL XVUNIT(INP1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INP1,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA','U_FORMAT',
     +            'REAL',' ')
      CALL XVUNIT(INP2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INP2,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA','U_FORMAT',
     +            'REAL',' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C
C     GET AND PROCESS THE PARAMETERS
C
      QLOG = XVPTST('LOG')
      QDISP = .NOT.XVPTST('NODISPLA') 
      QFILT = .NOT.XVPTST('NOFILTER')
      QDIFF = XVPTST('DIFFEREN')
      QCENTER = XVPTST('CENTER')
C                                                       INCLUDE
      CALL XVPARM('INCLUDE',RPAR,NUM,IND,0)
      IF (QDIFF.AND.IND.NE.0) THEN
          BOTTOM = -249.5
          TOP = 250.5
      ELSE
          BOTTOM = MIN(RPAR(1),RPAR(2))
          TOP = MAX(RPAR(1),RPAR(2))
      END IF
C                                                       ATMOSPHERIC CORRECTIONS
      CALL XVPARM('ATM1',ATM1,NUM,IND,0)
      CALL XVPARM('ATM2',ATM2,NUM,IND,0)
C                                                       PERCENT SATURATION
      CALL XVPARM('PERCENT',SAT,NUM,IND,0)
      SAT = 0.01*SAT
C                                                       THRESHOLD
      CALL XVPARM('THRESHOL',THRESH,NUM,IND,0)
      THRESH = 0.01*THRESH
C                                                       SUBSAMPLING PARAMS
      CALL XVPARM('SAMPLE',SUBSAMP,NUM,IND,0)
      INC = 1.0/(0.01*SUBSAMP)
      CALL XVPARM('LINC',LINC,NUM,IND,0)
      IF (IND.EQ.0) INC=LINC
      IF (INC.LE.0) INC=1
C                                                       AREA
      CALL XVPARM('AREA',IPAR,NUM,IND,0)
      IF (IND.EQ.0) THEN
          ISL = IPAR(1)
          ISS = IPAR(2)
          NL = IPAR(3)
          NS = IPAR(4)
      END IF
C
C       IF GIVEN PARAMETERS ARE UNREASONABLE, ADJUST THEM
C
      IF(QLOG.AND.TOP.LT.0.0) TOP=250.5
      IF(QLOG.AND.BOTTOM.LT.-255.) BOTTOM=-255.0
      IF(QLOG.AND..NOT.QDIFF.AND.BOTTOM.LE.0.0) BOTTOM=0.2
      IF(QLOG.AND.QDIFF) ATM1=ATM1+256.0
C
C
      II = 4*NS
      JJ = II
      CALL ZIA(IPOP,500)
      IF(QLOG) THEN
          IF(.NOT.QDIFF) THEN
C                                                     LOG RAT
              BOTTOM = ALOG(BOTTOM)
              SCALE = 500.0/(ALOG(TOP)-BOTTOM)
              CALL STACKA(4,LNRAT,2,II,JJ)
          ELSE
C                                                     LOG DIFF
              BOTTOM = ALOG(BOTTOM+256.0)
              SCALE = 500.0/(ALOG(TOP+256.0)-BOTTOM)
              CALL STACKA(4,LNDIFF,2,II,JJ)
          END IF
C                                                     RAT & DIFF
      ELSE
          SCALE = 500.0/(TOP-BOTTOM)
          IF(QDIFF) CALL STACKA(4,DIFF,2,II,JJ)
          IF(.NOT.QDIFF) CALL STACKA(4,RAT,2,II,JJ)
      END IF
C
C     FIND MAXIMUM POPULATED BIN; FILTER HISTOGRAM IF REQUESTED
C
      IF(.NOT.QFILT) THEN
          CALL MVE(4,500,IPOP,JPOP,1,1)
          MAXMUM = 0
          DO I=1,500
              MAXMUM = MAX(MAXMUM,IPOP(I))
          END DO
      ELSE
          JPOP(3) = IPOP(1)+IPOP(2)+IPOP(3)+IPOP(4)+IPOP(5)
          JPOP(2) = JPOP(3)-IPOP(5)+IPOP(1)
          JPOP(1) = JPOP(2)-IPOP(4)+IPOP(2)
          MAXMUM = MAX(JPOP(1),JPOP(2),JPOP(3))
          DO  I=4,498
              JPOP(I) = JPOP(I-1)-IPOP(I-3)+IPOP(I+2)
              MAXMUM = MAX(MAXMUM,JPOP(I))
          END DO
          JPOP(499) = JPOP(498)-IPOP(496)+IPOP(500)
          JPOP(500) = JPOP(499)-IPOP(497)+IPOP(499)
          MAXMUM = MAX(MAXMUM,JPOP(499),JPOP(500))
      END IF
C
C     ZERO OUT ALL BINS POPULATED BELOW THRESHOLD LEVEL;
C     COMPUTE THE MEAN
C
      ICUT = FLOAT(MAXMUM)*THRESH+0.5
      ISUM = 0
      IWT = 0
      DO I=1,500
          IF(JPOP(I).LT.ICUT) JPOP(I)=0
          ISUM = ISUM+JPOP(I)
          IWT = IWT+JPOP(I)*I
      END DO
      MEAN = FLOAT(IWT)/FLOAT(ISUM)+0.5
C
C     COMPUTE THE HIGH AND LOW BINS THAT CORRESPOND TO THE REQUESTED
C     SATURATION LEVEL
C
      N = 0
      NSAT = MAX(ISUM*SAT,1.0)
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(I)
          I=I+1
      END DO
      IF (N.LT.NSAT) THEN
         CALL XVMESSAGE(' SATURATION GREATER THAN 100 PERCENT',' ')
         CALL ABEND
      ENDIF
      LOW = I-1
      N = 0
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(501-I)
          I=I+1
      END DO
      IF (N.LT.NSAT) THEN
         CALL XVMESSAGE(' SATURATION GREATER THAN 100 PERCENT',' ')
         CALL ABEND
      ENDIF
      IHI = 502-I
C
C     IF NEEDED, FORCE THE MEAN TO BE CENTERED (OUTPUT DN=128)
C     BY DECREASING THE SATURATION AT ONE END.
C
      IF (QCENTER) THEN
          M = MEAN-LOW
          N = IHI-MEAN
          IF(N.GT.M) LOW=MEAN-N
          IF(M.GT.N) IHI=MEAN+M
      END IF
C
C     COMPUTE THE GAIN AND OFFSET;  IF REQUESTED, CALL DISPLY TO
C     PRINT OUT THE HISTOGRAM
C
      VAL0 = VALUE(LOW)
      VAL255 = VALUE(IHI)
      GAIN = 255.0/(VAL255-VAL0)
      OFF = -GAIN*VAL0
      IF(QDISP) CALL DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,
     +    QDIFF,QLOG)
C
C       GENERATE THE STRING THAT CONTAINS THE FUNCTION TO BE PASSED 
C       TO F2.
C
      IF (ATM1.EQ.0.0 .AND.ATM2.EQ.0.0) THEN
          IF (QLOG) THEN
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,110) GAIN,OFF
  110             FORMAT('"',G13.6,'*ALOG(IN1-IN2)',SP,G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,120) GAIN,OFF
  120             FORMAT('"',G13.6,'*ALOG(IN1/IN2)',SP,G13.6,'"')
              END IF
              N = 42
          ELSE
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,210) GAIN,OFF
  210             FORMAT('"',G13.6,'*(IN1-IN2)',SP,G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,220) GAIN,OFF
  220             FORMAT('"',G13.6,'*(IN1/IN2)',SP,G13.6,'"')
              END IF
              N = 38
          END IF
      ELSE
          IF (QLOG) THEN
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,310) GAIN,ATM1,ATM2,OFF
  310             FORMAT('"',G13.6,'*ALOG((IN1',SP,F6.1,
     +                   ')-(IN2',F6.1,'))',G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,320) GAIN,ATM1,ATM2,OFF
  320             FORMAT('"',G13.6,'*ALOG((IN1',SP,F6.1,
     +                   ')/(IN2',F6.1,'))',G13.6,'"')
              END IF
              N = 58
          ELSE
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,410) GAIN,ATM1,ATM2,OFF
  410             FORMAT('"',G13.6,'*((IN1',SP,F6.1,
     +                   ')-(IN2',F6.1,'))',G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,420) GAIN,ATM1,ATM2,OFF
  420             FORMAT('"',G13.6,'*((IN1',SP,F6.1,
     +                   ')/(IN2',F6.1,'))',G13.6,'"')
              END IF
              N = 54
          END IF
      END IF
      CALL SQUEEZE(FUNCSTR,FUNCSTR2,N)
      CALL XVMESSAGE(FUNCSTR2,' ')
C
C       SEND THE FUNCTION BACK TO THE PARAMETER 'FUNC' FOR F2
C
      CALL XQINI(PARB,xprdim,xcont,ISTATUS)
      CALL XQSTR(PARB,'FUNC',1,FUNCSTR2,xadd,ISTATUS)
      CALL XQOUT(PARB,ISTATUS)
C
      RETURN
      END
C**********************************************************************
C
      SUBROUTINE RAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING RATIO HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      DO I=ISL,ISL+NL-1,INC
          CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          DO J=1,NS
              X = XARR(J)+ATM1
              Y = YARR(J)+ATM2
              IF (Y.NE.0.0) THEN
                  N = SCALE*(X/Y-BOTTOM)+1.0
                  IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
              END IF
          END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE DIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING DIFFERENCE HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      OFFSET = ATM1-ATM2-BOTTOM
      DO I=ISL,ISL+NL-1,INC
          CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          DO J=1,NS
              N = SCALE*(XARR(J)-YARR(J)+OFFSET)+1.0
              IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
          END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE LNRAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG RATIO HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      DO I=ISL,ISL+NL-1,INC
          CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          DO J=1,NS
              X = XARR(J)+ATM1
              Y = YARR(J)+ATM2
              IF (X.GT.0.0.AND.Y.GT.0.0) THEN
                  N = SCALE*(ALOG(X/Y)-BOTTOM)+1.0
                  IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
              END IF
          END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE LNDIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG DIFFERENCE HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      OFFSET = ATM1-ATM2
      DO I=ISL,ISL+NL-1,INC
          CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
          DO J=1,NS
              N = SCALE*(ALOG(XARR(J)-YARR(J)+OFFSET)-BOTTOM)+1.0
              IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
          END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,QDIFF,QLOG
     +    )
C
C     ROUTINE FOR PRINTING HISTOGRAMS
C     IF FILTERING WAS PERFORMED, THIS IS THE FILTERED HISTOGRAM.
C
      CHARACTER*80 PRT
      CHARACTER*5 FILL
      INTEGER JPOP(500)
      LOGICAL*1 QDIFF,QLOG
C
      BOTTOM = BOTTOM+0.5/SCALE
C
C     PRINT THE PROPER HEADING
C
      IF (QDIFF) THEN
          IF (QLOG) THEN
              CALL XVMESSAGE('L O G   D I F F E R E N C E',' ')
          ELSE
              CALL XVMESSAGE('D I F F E R E N C E',' ')
          END IF
      ELSE
          IF (QLOG) THEN
              CALL XVMESSAGE('L O G   R A T I O',' ')
          ELSE
              CALL XVMESSAGE('R A T I O',' ')
          END IF
      END IF
      CALL XVMESSAGE(' ',' ')
C
C     PRINT A LINE FOR EACH POPULATED BIN, NORMALIZED TO THE MAXIMUM
C     POPULATED BIN
C
      DO I=1,500
          IF(JPOP(I).NE.0) THEN
              X = FLOAT(I-1)/SCALE+BOTTOM
C
C     LABEL THE MEAN (OUTPUT=128DN) AND SATURATION LEVEL BINS.  IN SOME
C     CASES, ONE OF '0DN' AND '255DN' WILL BE OUTSIDE THE POPULATED RANG
C     OF THE HISTOGRAM, AND WILL NOT APPEAR.
C
              FILL = '     '
              IF(I.EQ.LOW)  FILL = '  0DN'
              IF(I.EQ.MEAN) FILL = ' MEAN'
              IF(I.EQ.IHI)  FILL = '255DN'
              N = (50*JPOP(I))/MAXMUM + 1
              WRITE (PRT,100) X,FILL,('*',J=1,N)
  100         FORMAT(F12.3,3X,A5,2X,51A1)
              CALL XVMESSAGE(PRT,' ')
            ELSE IF (I.NE.1 .AND. JPOP(I-1).NE.0) THEN
              CALL XVMESSAGE(' ',' ')
          END IF
      END DO
      CALL QPRINT(' ',' ')
      RETURN
      END
