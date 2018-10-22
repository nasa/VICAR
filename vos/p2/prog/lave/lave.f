      INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C -------------------------------------------------------------
C  10 Dec 2010  RJB   Fixes for 64-bit linux, MacOSX
C  31 AUG 94    SVH   PORTED TO UNIX
C  26 AUG 86    SP    MODIFIED FOR FR 19204 TO IMPROVE ACCURACY OF STDEV,
C                     (ESPECIALLY WHEN THE STDEV IS VERY SMALL), AND TO
C                     AVOID SQRT OF NEGATIVE NUMBERS.  BEFORE COMPUTING STDEV
C                     OF A SET OF NUMBERS, THE FIRST ELEMENT OF THE SET IS 
C                     SUBTRACTED FROM EVERY ELEMENT OF THE SET.  THIS SHOULD
C                     REDUCE THE EFFECT OF ROUNDING IN MOST CASES.
C  18 DEC 85    JRH   REMOVED THE FORMAT KEYWORD.  IF THE USER DID
C                     NOT SPECIFY THE FORMAT FOR DATA OTHER THAN BYTE,
C                     HAD POSSIBLE PRECISION ERRORS. 
C   8 JAN 85    JRH   CONVERTED TO VICAR2.  MODIFIED TO HANDLE
C                     FULLWORD AND REAL*4.  INCREASED THE MAXIMUM
C                     TITLE LENGTH TO BE 122.
C  30 OCT 84    SP    REMOVED LENGTH FOR QPRINT CALL FOR TITLE.
C  29 OCT 84    SP    MADE LENGTH OF TITLE ARRAY EQUAL TO 70.
C  29 OCT 84    SP    ADDED XVP CALL FOR PROCESSING TITLE PARAMETER.
C  29 OCT 84    SP    CORRECTED PROBLEM WHERE QSTDEV WAS NOT PASSED TO
C                     SUBROUTINE FILTER AS A PARAMETER.
C  25 OCT 84    SP    MERGED VAX VERSION WITH VERSION FROM CJL WITH STDEV.
C  25 OCT 84    SP    REWROTE PROUT2 TO HANDLE MULTIPLE OF 1024 VALUES.
C  25 OCT 84    SP    CHANGED PDF TO USE SEPARATE PARAMETERS INSTEAD OF
C                     USING MODE WITH COUNT=(1:4).
C   7 AUG 84    CJL   COMPUTE MEAN & STANDARD DEVIATION IN REAL*8
C  25 AUG 83    ASM   MODIFIED FOR VAX CONVERSION 
C  21 JAN 80    REA   FIX BUG IN EXCLUDED VALUE CALCULATION
C   2 AUG 79    REA   EXCLUDE,IMAGE,FILTER KEYWORDS,  STACKA
C   2 FEB 79    REA   LABEL 77  CONVENTION
C  15 MAY 78    MAG   INCREASE BUFFER SIZE
C  27 JUN 75    DAH   CHANGES FOR CONVERSION TO 360/OS
C  29 AUG 74          ROUND MODIFICATION
C  30 JUN 71    RMR61 ALLOWS LAVE TO BE USED UNDER VICAF 4.1
C
	implicit none
      EXTERNAL VERT,HORI
      LOGICAL*4    QVERT, QEXCL, QIMAGE, QSTDEV, QAIS
      LOGICAL*4    QHIGH, QQUIET
      INTEGER*4    ICNT,INUNIT,NEXCL,NL,NS,NODS,NSW,OUTUNIT,STATUS
      INTEGER*4    ISL,ISS
      INTEGER*4    II,JJ,KK,LL
      INTEGER*4    DUM1,DUM2
      INTEGER*8    II8,JJ8,KK8,LL8
      CHARACTER*123 TITLE
      CHARACTER*5  AIS,DATA,IMAGE,STDEV,QUIET
      CHARACTER*8  HIGH,MODE
      CHARACTER*45 LBUF
      DATA    QVERT, QEXCL, QIMAGE, QSTDEV, QAIS /.false.,.false.,.false.,.false.,.false./
      DATA    QHIGH, QQUIET                      /.false.,.false./
      DATA    TITLE                              /'  '/
      DATA    DUM1,DUM2 /1,1/
C
	call xvmessage ('** LAVE version 2016-12-18',' ')
      NSW = 1
C
C     PROCESS THE PARAMETERS
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','REAL','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
      CALL XVP('OUT',LBUF,NODS)
      IF (NODS.NE.0) CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVGET(INUNIT,STATUS,'FORMAT',DATA,' ')
      CALL XVSIZE(ISL,ISS,NL,NS,DUM1,DUM2)
C                                                     VERTICAL
      CALL XVP('MODE',MODE,ICNT)
      IF (MODE.EQ.'VERTICAL') QVERT=.true.
C                                                     EXCLUDE
      CALL XVP('EXCLUDE',NEXCL,ICNT)
      IF (ICNT.NE.0) QEXCL = .true.
C                                                     FILTER
      CALL XVP('FILTER',NSW,ICNT)
C                                                     IMAGE
      CALL XVP('IMAGE',IMAGE,ICNT)
      IF (ICNT.NE.0) QIMAGE=.true.
C                                                     STANDARD DEVIATION
      CALL XVP('STDEV',STDEV,ICNT)
      IF (ICNT.NE.0) QSTDEV = .true.
C                                                     AIS PRINT FORMAT
      CALL XVP('AIS',AIS,ICNT)
      IF (ICNT.NE.0) QAIS = .true.
C                                                     HIGHPASS
      CALL XVP('HIGHPASS',HIGH,ICNT)
      IF (ICNT.NE.0) QHIGH= .true.
C                                                     TITLE
      CALL XVP('TITLE', TITLE,ICNT)
C                                                     QUIET MODE
      CALL XVP('QUIET',QUIET,ICNT)
      IF (ICNT.NE.0) QQUIET = .true.
C
C
      IF ((QIMAGE).AND.(NODS.NE.0))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +                'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',
     +                ' ')
      IF ((.not.QIMAGE).AND.(NODS.NE.0).AND.(MODE.EQ.'VERTICAL'))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',1,'U_NS',NS,
     +                'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',
     +                ' ')
      IF ((.not.QIMAGE).AND.(NODS.NE.0).AND.(MODE.EQ.'HORIZONT'))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',1,'U_NS',NL,
     +                'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',
     +                ' ')
C     POSITION INPUT DS FOR READING, COMPUTE SIZE OF ARRAYS, CALL STACKA 
C
      II = 4*NS
      IF(QVERT) GO TO 200
      JJ = 4*NL
      IF (II.LT.JJ) II=JJ
      II8 = int8(II)
      JJ8 = int8(JJ)
      CALL STACKA(21,HORI,2,II8,JJ8,ISL,ISS,NL,NS,DATA,NODS,NEXCL,QEXCL,
     +            NSW,QIMAGE,QSTDEV,QAIS,TITLE,QHIGH,QQUIET,
     +            INUNIT,OUTUNIT)
	call xvmessage('LAVE PROCESSING COMPLETE',' ')
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN

  200 CONTINUE
      JJ = 4*NS
      KK = 1
      IF (QEXCL) KK=4*NS
      LL = 8*NS
      II8 = int8(II)
      JJ8 = int8(JJ)
      KK8 = int8(KK)
      LL8 = int8(LL)
      CALL STACKA(24,VERT,5,II8,JJ8,KK8,LL8,LL8,ISL,ISS,NL,NS,DATA,NODS,
     +            NEXCL,QEXCL,NSW,QIMAGE,QSTDEV,QAIS,TITLE,QHIGH,QQUIET,
     +            INUNIT,OUTUNIT)
	call xvmessage('LAVE PROCESSING COMPLETE',' ')
      RETURN
c  900 CONTINUE
      call xvmessage ('??E - Insufficient memory for stacka operation',' ')
	call abend
      RETURN
      END
C
C=======================================================================
      SUBROUTINE HORI(IN,II,OUT,JJ,ISL,ISS,NL,NS,DATA,NODS,NEXCL,
     +                QEXCL,NSW,QIMAGE,QSTDEV,QAIS,TITLE,QHIGH,
     +                QQUIET,INUNIT,OUTUNIT)
C
C     THIS SUBROUTINE COMPUTES AVERAGES FOR EACH LINE
C
	implicit none
	INTEGER*4   ISL,ISS,NL,NS,NODS,NEXCL,NSW
      REAL*4      IN(NS),OUT(NL), IN1,XADJ
      REAL*8      SUM,SUM2,XNS, XDIFF
      INTEGER*4   INUNIT,OUTUNIT
      INTEGER*4   I,J,K,NUM
      INTEGER*8   II,JJ,dummy
      INTEGER*4   STATUS
      LOGICAL*4   QEXCL,QIMAGE,QSTDEV,QAIS,QHIGH,QQUIET
      CHARACTER*123 TITLE
      CHARACTER*5 DATA
	dummy = ii		!to prevent message
	in1 = 0
        IF (JJ.NE.4*NL) then
	    call xvmessage ('??E- Insufficient memory for STACKA buffers',' ')
	    call abend
	ENDIF
      CALL ZIA(OUT,NL)
      K=0
      IF (QEXCL) GO TO 400
      IF (QSTDEV) GO TO 340
C
C     PROCESSING FOR NO EXCLUDED VALUE, MEAN COMPUTED
C
      XNS = NS
      DO 300 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          SUM = 0
          DO 200 J=1,NS
              SUM = SUM+IN(J)
  200     CONTINUE
          K = K+1
          OUT(K) = SUM/XNS
  300 CONTINUE
      GO TO 700
C
C   PROCESSING FOR NO EXCLUDED VALUE, STANDARD DEVIATION COMPUTED
C
  340 XNS = NS
      DO 380 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          IN1 = IN(1)
          SUM = 0
          SUM2 = 0
          DO 360 J=1,NS
              XADJ = IN(J) - IN1        ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                        ! OF SET FROM EVERY ELEMENT OF SET.
              SUM = SUM + XADJ
              SUM2 = SUM2 + XADJ*XADJ
  360     CONTINUE
          K = K+1
          XDIFF = SUM2-SUM*SUM/XNS
          IF ( XDIFF .LE. 0.0D0 )  THEN
             OUT(K) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
          ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
             OUT(K) = SNGL(DSQRT(XDIFF/XNS))
          END IF
  380 CONTINUE
      GO TO 700
C
C     EXCLUDED VALUE PROCESSING, MEAN COMPUTED
  400 CONTINUE
      IF (QSTDEV) GO TO 640
      DO 600 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          SUM = 0
          NUM = 0
          DO 500 J=1,NS
              IF (IN(J).EQ.NEXCL) GO TO 500
              SUM = SUM+IN(J)
              NUM = NUM+1
  500     CONTINUE
          IF (NUM.EQ.0) NUM=1
          K = K+1
          OUT(K) = SUM/NUM
  600 CONTINUE
      GO TO 700
C
C   PROCESSING FOR EXCLUDED VALUES, STANDARD DEVIATION COMPUTED
C
  640 DO 680 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          SUM = 0
          SUM2 = 0
          NUM = 0
          DO 660 J=1,NS
              IF (IN(J) .EQ. NEXCL) GO TO 660
              IF ( NUM .EQ. 0 )  IN1 = IN(J)
              XADJ = IN(J) - IN1        ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
              SUM = SUM + XADJ
              SUM2 = SUM2 + XADJ*XADJ
              NUM = NUM+1
  660     CONTINUE
          IF (NUM .EQ. 0) NUM=1
          K = K+1
          XDIFF = SUM2-SUM*SUM/NUM
          IF ( XDIFF .LE. 0.0D0 )  THEN
             OUT(K) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
          ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
             OUT(K) = SNGL(DSQRT(XDIFF/NUM))
          END IF
  680 CONTINUE
C
C     PRINT OUT RESULTS, WRITE OUTPUT DS
C
  700 CONTINUE
      IF (QQUIET .EQV. .FALSE.) THEN
         IF (QAIS) THEN  
            IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
               CALL PROUT2(OUT,NL,QSTDEV,TITLE,DATA,32,32,18,6)
            ELSE
               CALL PROUT2(OUT,NL,QSTDEV,TITLE,DATA,32,32,9,11)
            END IF
         ELSE
            CALL PROUT(OUT,NL,DATA,QSTDEV)
         END IF
      END IF
c  740 continue
	IF (NSW.NE.1) CALL FILTER(OUT,IN,NSW,NL,DATA,QHIGH,QSTDEV)
      IF (NODS.EQ.0) RETURN
      IF (QIMAGE) GO TO 800
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.(DATA.EQ.'FULL')) THEN
         DO 780 I=1,NL
             OUT(I) = ANINT(OUT(I))
  780    CONTINUE
      ENDIF
      CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NL,' ')
      RETURN
  800 CONTINUE
C
C     FULL IMAGE OUTPUT 
C
c  900 CONTINUE
      DO 950 I=1,NL
          DO 920 J=1,NS
              IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.(DATA.EQ.'FULL')) THEN
                  IN(J) = ANINT(OUT(I))
              ELSE   
                  IN(J) = OUT(I)
              ENDIF
  920     CONTINUE
      CALL XVWRIT(OUTUNIT,IN,STATUS,'NSAMPS',NS,' ')
  950 CONTINUE
      RETURN
      END
C
C====================================================================
      SUBROUTINE VERT(IN,II,OUT,JJ,IPOP,KK,TOT,LL,TOT2,L2,ISL,ISS,
     +               NL,NS,DATA,NODS,NEXCL,QEXCL,NSW,QIMAGE,
     +               QSTDEV,QAIS,TITLE,QHIGH,QQUIET,INUNIT,OUTUNIT)
C
C     THIS SUBROUTINE COMPUTES AVERAGES FOR EACH SAMPLE
C
	implicit none
	INTEGER*4   ISL,ISS,NL,NS,NODS,NEXCL,NSW
      REAL*4      IN(NS),OUT(NS), XADJ
      REAL*8      TOT(NS),TOT2(NS),XNL, XDIFF
      INTEGER*4   INUNIT,OUTUNIT
      INTEGER*4   IPOP(NS)
      INTEGER*4   I,J
      INTEGER*8   II,JJ,KK,LL
      INTEGER*8   L2,dummy
      INTEGER*4   STATUS
      LOGICAL*4   QIMAGE,QEXCL,QSTDEV,QAIS,QHIGH,QQUIET
      CHARACTER*123 TITLE
      CHARACTER*5 DATA
	dummy = ii		!to prevent compiler message
	dummy = jj
	dummy = kk
	dummy = ll
      	IF (L2.NE.8*NS) then
	    call xvmessage ('??E - Insufficient core for STACKA buffers',' ')
	    call abend
	ENDIF
      CALL ZIA(TOT,2*NS)
      CALL ZIA(TOT2,2*NS)
      IF (QEXCL) GO TO 500
      IF (QSTDEV) GO TO 420
C
C     NO EXCLUDED VALUE, MEAN COMPUTED
C
      DO 300 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          DO 200 J=1,NS
              TOT(J) = TOT(J)+IN(J)
  200     CONTINUE
  300 CONTINUE
      XNL = NL
      DO 400 I=1,NS
      OUT(I) = TOT(I)/XNL
  400 CONTINUE
      GO TO 800
C
C   NO EXCLUDED VALUE, STANDARD DEVIATION COMPUTED
C
  420 DO 460 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      IF ( I .EQ. ISL )  CALL MVE( 7, NS, IN, OUT,1,1)
      DO 440 J=1,NS

      XADJ = IN(J) - OUT(J)     ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
      TOT(J) = TOT(J) + XADJ
      TOT2(J) = TOT2(J) + XADJ*XADJ
  440 CONTINUE
  460 CONTINUE
      XNL = NL
      DO 480 I=1,NS
          XDIFF = TOT2(I)-TOT(I)*TOT(I)/XNL
          IF ( XDIFF .LE. 0.0D0 )  THEN
             OUT(I) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
          ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
             OUT(I) = SNGL(DSQRT(XDIFF/XNL))
          END IF
  480 CONTINUE
      GO TO 800
C
C     EXCLUDED VALUE PROCESSING, MEAN COMPUTED
C
  500 CONTINUE
      CALL ZIA(IPOP,NS)
      CALL ZIA(OUT,NS)
      IF (QSTDEV) GO TO 760
      DO 700 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          DO 600 J=1,NS
             IF (IN(J).EQ.NEXCL) GO TO 600
             TOT(J) = TOT(J)+IN(J)
             IPOP(J) = IPOP(J)+1
  600     CONTINUE
  700 CONTINUE
      DO 750 I=1,NS
          IF (IPOP(I).NE.0) OUT(I) = TOT(I)/IPOP(I)
  750 CONTINUE
      GO TO 800
C
C   EXCLUDED VALUE PROCESSING, STANDARD DEVIATION COMPUTED
C
  760 CONTINUE
      DO 780 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
          DO 770 J=1,NS
              IF (IN(J).EQ.NEXCL) GO TO 770
              IF ( IPOP(J) .EQ. 0 )  OUT(J) = IN(J)   ! STORE FIRST ELEMENT IN OUT.
              XADJ = IN(J) - OUT(J)     ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
              TOT(J) = TOT(J) + XADJ
              TOT2(J) = TOT2(J) + XADJ*XADJ
              IPOP(J) = IPOP(J)+1
  770     CONTINUE
  780 CONTINUE
      DO 790 I=1,NL
      IF ( IPOP(I) .GT. 0 )   THEN
        XDIFF = TOT2(I)-TOT(I)*TOT(I)/ IPOP(I)
      ELSE
        XDIFF = 0.0D0
      END IF
      IF ( XDIFF .LE. 0.0D0 )  THEN
         OUT(I) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
      ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
         OUT(I) = SNGL(DSQRT(XDIFF/IPOP(I) ))
      END IF
  790 CONTINUE
C
C     PRINT OUT RESULTS, WRITE OUTPUT DS
C
  800 CONTINUE
      IF (QQUIET .EQV. .FALSE.) THEN
         IF (QAIS) THEN  
            IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
               CALL PROUT2(OUT,NS,QSTDEV,TITLE,DATA,32,32,18,6)
            ELSE
               CALL PROUT2(OUT,NS,QSTDEV,TITLE,DATA,32,32,9,11)
            END IF
         ELSE
            CALL PROUT(OUT,NS,DATA,QSTDEV)
         END IF
      END IF
c  840 continue
	IF (NSW.NE.1) CALL FILTER(OUT,IN,NSW,NS,DATA,QHIGH,QSTDEV)
      IF (NODS.EQ.0) RETURN
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.(DATA.EQ.'FULL')) THEN
         DO 860 I=1,NS
             OUT(I) = ANINT(OUT(I))
  860    CONTINUE
      ENDIF
c      IF (QIMAGE.NE.0) GO TO 900
	if (qimage) go to 900
      CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NS,' ')
      RETURN
C
C     FULL IMAGE OUTPUT
C
  900 CONTINUE
      DO 950 I=1,NL
          CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NS,' ')
  950 CONTINUE
      RETURN
      END
C
C====================================================================
      SUBROUTINE FILTER(OUT,IN,NSW,NS,DATA,QHIGH,QSTDEV)
	implicit none
      INTEGER*4   NSW,NS,M
      INTEGER*4   I,J,N,L
      REAL*4      OUT(NS),IN(NS),SUM,X
      LOGICAL*4   QHIGH,QSTDEV
      CHARACTER*5 DATA
      INTEGER*4 DUM1,DUM3,DUM4
      DATA  DUM1,DUM3,DUM4 /7,1,1/
C

      M = NSW/2
      SUM = 0
      DO 100 I=1,M
          SUM = SUM+OUT(I)
  100 CONTINUE
C
C     FILTER WINDOW TRUNCATED AT LEFT EDGE
C
      J = 0
      N = M+1
      DO 200 I=N,NSW
          J = J+1
          SUM = SUM+OUT(I)
          IN(J) = SUM/FLOAT(I)
  200 CONTINUE
C
C     MOVE FILTER WINDOW ACROSS PICTURE TO RIGHT EDGE
C
      X = NSW
      N = NSW+1
      DO 300 I=N,NS
          SUM = SUM+OUT(I)-OUT(I-NSW)
          J = J+1
          IN(J) = SUM/X
  300 CONTINUE
C
C     FILTER WINDOW TRUNCATED AT RIGHT EDGE
C
      N = J-M
      L = NS-M-1
      DO 400 I=N,L
          SUM = SUM-OUT(I)
          X = X-1.0
          J = J+1
          IN(J) = SUM/X
  400 CONTINUE
C
C     HIGHPASS FILTERING
C
      IF (.not.QHIGH) GO TO 700
      N = 128
      IF (DATA.NE.'BYTE') N=0
      DO 600 J=1,NS
          IN(J) = OUT(J)-IN(J)+N
          IF (DATA.NE.'BYTE') GO TO 600
          IF (IN(J).LT.0) IN(J)=0
          IF (IN(J).GT.255) IN(J)=255
  600 CONTINUE
  700 CONTINUE
      CALL MVE(DUM1,NS,IN,OUT,DUM3,DUM4)
      CALL XVMESSAGE('??I - After filtering',' ')

      CALL PROUT(OUT,NS,DATA,QSTDEV)
      RETURN        
      END
C
C=====================================================================
      SUBROUTINE PROUT(OUT,NS,DATA,QSTDEV)
C
C     THIS SUBROUTINE PRINTS OUT THE CONTENTS OF THE 'OUT' ARRAY
C
	implicit none
	INTEGER*4   NS,INC
      REAL*4      OUT(NS)
      INTEGER*2   IVALH
      INTEGER*4   IVALF
      INTEGER*4   I,J
      CHARACTER*132 UNCONFMT
      CHARACTER*132 BUF
      LOGICAL*4 QSTDEV
      CHARACTER*5 DATA
C
      IF (DATA.EQ.'BYTE') THEN
         INC = 4
      ELSE IF (DATA.EQ.'HALF') THEN
         INC = 6
      ELSE 
         INC = 11
      END IF
      IF (QSTDEV) THEN
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('    SAMPLE  STANDARD DEVIATION VALUES',' ')
      ELSE
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('    SAMPLE  AVERAGE VALUES',' ')
      END IF
	do i=1,132
	  BUF(i:i)=' '
        enddo
      BUF(1:6) = '    1-'
      J = 11
      DO 500 I=1,NS
C     Logically, it would make sense to have .LE.132, but that may break in
C     a different spot than the unported version, making test logs that don't
C     compare properly.
      IF ((J+INC).LE.131) GO TO 200
      WRITE (BUF(7:11),'(I5)') I-1
c	BUF(132:132) = ' '
      CALL XVMESSAGE(BUF,' ')
      J = 11
      WRITE (BUF(1:5),'(I5)') I
  200 CONTINUE
      J = J+INC
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
         IVALH = NINT(OUT(I))
         WRITE (UNCONFMT,201) INC
  201    FORMAT('(I',I4.4,')')
         WRITE (BUF(J-INC+1:J),UNCONFMT) IVALH
      ELSE IF (DATA.EQ.'FULL') THEN
         IVALF = NINT(OUT(I))
         WRITE (UNCONFMT,202) INC
  202    FORMAT('(I',I4.4,')')
         WRITE (BUF(J-(INC)+1:J),UNCONFMT) IVALF
      ELSE
         WRITE (UNCONFMT,203) (INC-1), (INC-1)-6
  203    FORMAT('(E',I4.4,'.',I4.4,')')
         WRITE (BUF(J-(INC-1)+1:J),UNCONFMT) OUT(I)
         BUF(J-INC+1:J-INC+1) = ' '
      END IF
  500 CONTINUE
      WRITE (BUF(7:11),'(I5)') NS
c      BUF(132:132) = ' '
      CALL XVMESSAGE(BUF(1:J),' ')
      RETURN
      END
C
C=======================================================================
      SUBROUTINE PROUT2(OUT,NV,QSTDEV,TITLE,DATA,ILB,ISB,LL,ISIZ )
C#######################################################################
C
C  PURPOSE
C   THIS SUBROUTINE PRINTS OUT THE CONTENTS OF THE 'OUT' ARRAY
C      AS ONE OR MORE ILBxISB AIS ARRAYS.  BECAUSE OF THE PAGE WIDTH
C      LIMITATION, EACH ILBxISB ARRAY WILL BE PRINTED IN MORE THAN ONE PIECE
C      IF ISB (the number of values per line of the AIS array) IS GREATER
C      THAN LL (the printed line length measured in terms of values).
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    OCT 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  INPUT PARAMETERS ( all parameters are INTEGER*4 except as otherwise noted )
C      OUT(K) array      - VALUES TO BE PRINTED, FOR K = 1 TO NV.
C      NV                - NUMBER OF VALUES IN OUT. SHOULD BE A MULTIPLE OF
C                          ILB*ISB.
C      QSTDEV            - 1 IF OUT ARRAY CONTAINS STANDARD DEVIATIONS.
C      TITLE             - TITLE SPECIFIED FOR THE USER FOR PRINTOUT.
C      ILB               - LINES PER BLOCK OF NUMBERS TO BE PRINTED.
C      ISB               - SAMPLES PER LINE IN BLOCK OF NUMBERS TO BE PRINTED.
C      LL                - MAXIMUM NUMBER OF VALUES TO FIT ON A PRINT LINE.
C      ISIZ              - NUMBER OF SPACES ALLOCATED FOR EACH VALUE ON A LINE
C                          (INCLUDING THE SPACES BETWEEN VALUES).
C  OUTPUT PARAMETERS
C      THE VALUES ARE PRINTED UNLESS NV IS NOT A MULTIPLE OF ILB*ISB,
C      IN WHICH CASE THE PROGRAM ABORTS.  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

C
C
	implicit none
	INTEGER*4   NV,ILB,ISB,ISIZ
      REAL*4      OUT(ISB,ILB,*)        ! MAKE IT EASIER BY DECLARING AS 3-DIM.
      CHARACTER*132 UNCONFMT
C      BUF               - WORK SPACE FOR PRINTER LINE BUFFER.
      CHARACTER*132 BUF
      LOGICAL*4 QSTDEV
      INTEGER*2   IVALH
      INTEGER*4   IVALF
      INTEGER*4   IBLOCK,NBLOCKS,ISPTR,ISLINE,ISLAST,IPOS
      INTEGER*4   J,L
      INTEGER*4   LL
      CHARACTER*5 DATA
      CHARACTER*123 TITLE

C=============================================================================

      IF ( MOD(NV,ISB*ILB) .NE. 0) then
         CALL xvmessage('??E - Incorrect number of input samples for ais print format',' ' )
	 call abend
      endif
      IF (QSTDEV)   THEN
          CALL XVMESSAGE(' ',' ')
          CALL XVMESSAGE('    SAMPLE  STANDARD DEVIATION VALUES',' ')
      ELSE
          CALL XVMESSAGE(' ',' ')
          CALL XVMESSAGE('    SAMPLE  AVERAGE VALUES',' ')
      END IF

      NBLOCKS = NV / (ISB*ILB)    ! NUMBER OF BLOCKS (SECTIONS OR AIS ARRAYS).

      DO IBLOCK = 1, NBLOCKS      ! PRINT A BLOCK FOR EACH BLOCK OF VALUES.

         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE(TITLE,' ')
         IF (NBLOCKS .GT. 1) THEN     ! IF MORE THAN 1 SECTION, NUMBER THEM.
           CALL XVMESSAGE( ' ' ,' ')
           BUF(1:8) = ' SECTION'
           WRITE (BUF(9:11),'(I3)') IBLOCK
           CALL XVMESSAGE(BUF(2:11),' ')
         END IF

         ISPTR = 1                     ! COLUMN POINTER TO BEGINNING OF PIECE.
         DO WHILE ( ISPTR .LE. ISB )   ! USE AS MANY PIECES PER BLOCK AS NEEDED
            ISLINE = MIN0( LL, ISB-ISPTR+1 )
            ISLAST = ISPTR + ISLINE - 1
            CALL XVMESSAGE( ' ' ,' ')
            CALL XVMESSAGE( ' ' ,' ')
            BUF(1:132) = ' '
            BUF(1:12) = '      SAMP  '
            IPOS = 12 - ISIZ
            DO J = ISPTR, ISLAST, 2    ! PRINT SAMPLE NUMBERS ABOVE THE PIECE.
               IPOS = IPOS + 2*ISIZ
               WRITE (UNCONFMT,501) ISIZ
  501    FORMAT('(I',I4.4,')')
               WRITE (BUF(IPOS-(ISIZ)+1:IPOS),UNCONFMT) J
            END DO
            CALL XVMESSAGE(BUF(2:IPOS),' ')
            CALL XVMESSAGE(' LINE',' ')
      
            DO L = 1, ILB
               BUF(1:12) = ' '
               WRITE (BUF(2:6),'(I5)') L ! PRINT LINE NUMBER
               IPOS = 12 
               DO J = ISPTR, ISLAST
                  IPOS = IPOS + ISIZ               ! AND A LINE OF VALUES.
                  IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
                     IVALH = NINT(OUT(J,L,IBLOCK))
                     WRITE (UNCONFMT,502) ISIZ
  502    FORMAT('(I',I4.4,')')
                     WRITE (BUF(IPOS-(ISIZ)+1:IPOS),UNCONFMT) IVALH
                  ELSE IF (DATA.EQ.'FULL') THEN
                     IVALF = NINT(OUT(J,L,IBLOCK))
                     WRITE (UNCONFMT,503) ISIZ
  503    FORMAT('(I',I4.4,')')
                     WRITE (BUF(IPOS-(ISIZ)+1:IPOS),UNCONFMT) IVALF
                  ELSE
                     WRITE (UNCONFMT,504) (ISIZ-1),(ISIZ-1)-6
  504    FORMAT('(E',I4.4,'.',I4.4,')')
                     WRITE (BUF(IPOS-(ISIZ-1)+1:IPOS),UNCONFMT) OUT(J,
     +L,IBLOCK)
                     BUF(IPOS-(ISIZ-1)+1:IPOS-ISIZ+1) = ' '
                  END IF
               END DO
               CALL XVMESSAGE(BUF(2:IPOS),' ')
            END DO

            ISPTR = ISLAST + 1                     ! POINT TO NEXT PIECE
         END DO
      END DO
      RETURN
      END
C
C=====================================================================
c      SUBROUTINE ABND(BUF,N)
c      BYTE BUF(N)
c      INTEGER N
C
c      CALL XVMESSAGE(BUF,' ')
c      CALL ABEND
c      RETURN
c      END
