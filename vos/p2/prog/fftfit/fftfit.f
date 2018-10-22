      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 30 SEPT 1983
C     MSTP S/W CONVERSION (VICAR PORTING) BY A SCOP, (CRI) 31 OCT 1994
C**********************************************************************
      SUBROUTINE MAIN44
      INTEGER*2 IN(1024),MAX
      COMPLEX*8 C(1024),CDC
      INTEGER*4 IPARM(2),SAMPLE,STAT,OUNIT
      REAL*4 RPARM(2)
      LOGICAL*4 XVPTST
      CHARACTER*8 FMT1,FMT2
      COMMON/C1/ C,IN

      CALL IFMESSAGE('FFTFIT version 31-OCT-94')
      CALL XVEACTION('SA',' ')
C
C ----- OPEN DATA SETS
C
      CALL XVUNIT(IUNIT1,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT1,STAT,' ')

      CALL XVUNIT(IUNIT2,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT2,STAT,'U_FORMAT','HALF',' ')

      CALL XVGET(IUNIT1,STAT,'NL',NL1,'NS',NS1,'FORMAT',FMT1,' ')
      CALL XVGET(IUNIT2,STAT,'NL',NL2,'NS',NS2,'FORMAT',FMT2,' ')
      IF (FMT2.NE.'BYTE' .AND. FMT2.NE.'HALF') THEN
         CALL MABEND('SECOND INPUT MUST BE BYTE OR HALF')
      END IF

      NLI=MAX0(NL1,NL2)
      NSI=MAX0(NS1,NS2)
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NL',NLI,'U_NS',NSI,' ')

C
C ----- SET DEFAULTS
C
      NL=NL1
      NPIX=NS1
      SCALE=1.0
      LINE=NL/2+1
      SAMPLE=NPIX/2+1
      MULT=0
      ISET1=2
      ISET2=3
      IFIND=0
      S=-99.
C
C ----- PARAMETER PROCESSING
C

C        'LINE'
      CALL XVPARM('LINE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) LINE = IPARM(1)

C        'SAMPLE'
      CALL XVPARM('SAMPLE',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) SAMPLE = IPARM(1)

C        'SCALE'
      CALL XVPARM('SCALE',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.EQ.1) S = RPARM(1)

C        'MODE' - 'MULT'
      IF(XVPTST('MULT')) MULT = 1

C        'FIND'
      IF(XVPTST('FIND')) IFIND = 1

C        'SET'
      CALL XVPARM('SET',IPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
         ISET1 = IPARM(1)
         ISET2 = IPARM(2)
      END IF

C
C**********************************************************************
C           FIND MAX DN TO SET CENTER OF MASK PICTURE TO
C**********************************************************************
C
      IF(IFIND.NE.0) THEN
          MAX=-32760
          DO 201 L=1,NL
              CALL XVREAD(IUNIT2,IN,STAT,'LINE',L,'NSAMPS',NPIX,' ')
              DO 202 J=1,NPIX
                  IF(MAX.EQ.IN(J)) THEN
                      K=K+1
                      LINE=LINE+L
                      SAMPLE=SAMPLE+J
                  ELSE IF (MAX.LT.IN(J)) THEN
                      MAX=IN(J)
                      LINE=L
                      SAMPLE=J
                      K=1
                  END IF
202           CONTINUE
201       CONTINUE
          LINE=LINE/FLOAT(K)+0.5
          SAMPLE=SAMPLE/FLOAT(K)+0.5
          CALL PRNT(4,1,LINE,' LINE=.')
          CALL PRNT(4,1,SAMPLE,' SAMPLE=.')
      END IF
C
C**********************************************************************
C              IF SCALE WASN'T SPECIFIED, DETERMINE HERE
C**********************************************************************
C
      IF(S.EQ.-99.) THEN
C
C  OBTAIN MEAN AMPLITUDE OF TRANSFORM IN SET AREA
C
          S=1.0
          I=ISET1
          IF(MULT.NE.1) THEN
              K=0
              AMPL=0.0
              DO 300 L=1,ISET2
                  CALL XVREAD(IUNIT1,C,STAT,'LINE',L,'NSAMPS',NPIX,' ')
                  IF(L.GE.ISET1) ISET1=1
                  J=ISET1
                  DO WHILE (J.EQ.ISET1 .OR. J.LE.ISET2)
                      AMPL=CABS(C(J))+AMPL
                      K=K+1
                      J=J+1
                  END DO
300           CONTINUE
              S=AMPL/K
          END IF
C
C  OBTAIN MEAN DN OF MASK IN SET AREA
C
          ISET1=I
          K=0
          AMPL=0.0
          DO 320 L=1,ISET2
              MASK=LINE+L-1
              IF(MASK.GT.NL) MASK=MASK-NL
              CALL XVREAD(IUNIT2,IN,STAT,'LINE',MASK,'NSAMPS',NPIX,' ')
              IF(L.GE.ISET1) ISET1=1
              J=ISET1
              DO WHILE (J.EQ.ISET1 .OR. J.LE.ISET2)
                  MISK=SAMPLE+J-1
                  IF(MISK.GT.NPIX) MISK=MISK-NPIX
                  AMPL=AMPL+IN(MISK)
                  K=K+1
                  J=J+1
              END DO
320       CONTINUE
          S=S/(AMPL/K)
      END IF
      CALL PRNT(7,1,S,' SCALE=.')
C
C**********************************************************************
C                 SET UP CONSTANTS FOR FFT FORMAT
C**********************************************************************
C
      N1=NPIX-SAMPLE+1
      N2=SAMPLE-1
      N3=N1+1
C
C**********************************************************************
C                          MAIN LINE LOOP
C**********************************************************************
C
      DO 100 L=1,NL
C
C  READ FFT RECORD # L
C
          CALL XVREAD(IUNIT1,C,STAT,'LINE',L,'NSAMPS',NPIX,' ')
          CDC=C(1)
C
C  READ CORRESPONDING MASK RECORD
C
          MASK=LINE+L-1
          IF(MASK.GT.NL) MASK=MASK-NL
          CALL XVREAD(IUNIT2,IN,STAT,'LINE',MASK,'NSAMPS',NPIX,' ')
C
C  SAMPLE LOOP
C
          IF(MULT.NE.1) THEN
C
C  FITTING MODE
C
              DO 110 J=1,N1
                  A=CABS(C(J))
                  IF(A.GE.1.0E-10) C(J)=C(J)*S*IN(N2+J)/A
110           CONTINUE
              J=N3
              K=1
              A=CABS(C(J))
              DO WHILE (A.GE.1.0E-10 .AND. J.LE.NPIX)
                  C(J)=C(J)*S*IN(K)/A
                  K = K+1
                  J = J+1
                  A=CABS(C(J))
              END DO
          ELSE
C
C  MULTIPLICATIVE MODE
C
              DO 120 J=1,N1
                  C(J)=C(J)*S*IN(N2+J)
120           CONTINUE
              K=0
              DO 121 J=N3,NPIX
                  K=K+1
                  C(J)=C(J)*S*IN(K)
121           CONTINUE
          END IF

          IF(L.EQ.1) C(1)=CDC

          CALL XVWRIT(OUNIT,C,STAT,'NSAMPS',NPIX,' ')
100   CONTINUE

      CALL XVCLOSE(IUNIT1,STAT,' ')
      CALL XVCLOSE(IUNIT2,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')

      RETURN
      END
