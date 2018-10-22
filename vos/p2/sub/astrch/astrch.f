CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute low and high stretch limits by saturating lper and hper of histogram.
C
      SUBROUTINE ASTRCH(HIS,LOWDN,HIGHDN,LPER,HPER,N)
      REAL*4 LPER,HPER
      INTEGER HIS(1),LOWDN,HIGHDN,N
      INTEGER LSUM,HSUM,FSUM,SUM,I,ILO,IHI
C     ....Compute number of pixels to be saturated at LPER and HPER
      CALL SUMV(4,N,HIS,FSUM,1)
      IF (FSUM.EQ.0) THEN
         LOWDN = 0
         HIGHDN = 1
         RETURN
      ENDIF
      LSUM = (FSUM*LPER)*.01 + 0.5
      HSUM = (FSUM*HPER)*.01 + 0.5
C     ....Position index at first non-zero entry
      I = 1
    5 IF (HIS(I).GT.0) GOTO 6
      I = I + 1
      IF (I.LT.N) GOTO 5
      LOWDN = N - 2
      HIGHDN = N - 1
      RETURN

    6 SUM0 = HIS(I)
C
   10 SUM = SUM0 + HIS(I+1)
      IF (SUM.GE.LSUM) THEN
         IF (LSUM-SUM0.GT.SUM-LSUM) I=I+1
         GOTO 20
      ENDIF
      SUM0 = SUM
      I = I + 1
      IF (I.LT.N-1) GOTO 10

   20 ILO = I
      I = N
   25 IF (HIS(I).GT.0) GOTO 26
      I = I - 1
      GOTO 25

   26 SUM0 = HIS(I)

   30 SUM = SUM0 + HIS(I-1)
      IF (SUM.GT.HSUM) THEN
         IF (HSUM-SUM0.GT.SUM-HSUM) I=I-1
         GOTO 40
      ENDIF
      SUM0 = SUM
      I = I - 1
      IF (I.GT.2) GOTO 30

   40 IHI = I
C     ....Indices start at 1, DNs start at 0
      LOWDN  = ILO - 1
      HIGHDN = IHI - 1
C     ....Make sure low and high stretch limits are'nt equal
      IF (LOWDN.LT.HIGHDN) RETURN
      IF (LOWDN.GT.0) THEN
         LOWDN = LOWDN - 1
      ELSE
         HIGHDN = HIGHDN + 1
      ENDIF
      RETURN
      END
