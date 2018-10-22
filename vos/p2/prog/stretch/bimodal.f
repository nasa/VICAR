CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute a piecewise linear stretch table for a bimodal histogram.
C
      SUBROUTINE BIMODAL(FORMAT,HIST,lut,prt,nchar,imode)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 NCHAR,IMODE
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IND

      CALL TWOPK(FORMAT,HIST(INMIN),lut,ind)
      IF (IND.EQ.0) THEN
         PRT = 'BIMODAL STRETCH'
         NCHAR = 16
         RETURN
      ELSE
         CALL XVMESSAGE('Bimodal option unsuccessful',' ')
         CALL XVMESSAGE('Reset to default astretch option',' ')
         IMODE=18	!Revert to ASTRETCH mode
      END IF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute stretch table for bimodal histogram
C
      SUBROUTINE TWOPK(FORMAT,HIST,lut,ind)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(65536)
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 IND		!0=OK

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/CW/HISTW
      INTEGER*4 HISTW(65536,2)

      INTEGER*4 ICOUNT,IDEF,IPARM(6),LOC(3)
      INTEGER*4 I,J,K,NLEV,NLEV2,NSW,NSW2,NTOL
      INTEGER*4 SUM,LSUM,HSUM,NPEAK,LOPK,HIPK,IMIN
      INTEGER*4 IDUP,IFREQ,MINFREQ
      INTEGER*4 LPT1,LPT2,HPT1,HPT2
      REAL*4 L1PERC,L2PERC,H1PERC,H2PERC,FF,RPARM(6)

      INTEGER*4 T(0:5)
      INTEGER*4 ITER/5/		!Max number of iterations
      REAL*4 FTOL/10.0/		!Min freq percent tol for a peak
      CHARACTER*80 MSG8

      IND=-1
      NLEV = INMAX - INMIN + 1      
      SUM = 0
C     ....Copy histogram
      DO I=1,NLEV
         HISTW(I,1) = HIST(I)
         SUM = SUM + HIST(I)
      ENDDO
      NTOL = 0.01*FLOAT(SUM)/FLOAT(NLEV)

      NSW = 0.03*NLEV
      NSW = (NSW/2)*2 + 1   !Force filter NSW to be an odd number
      NSW2 = NSW/2
      WRITE (MSG8,1000) NSW
 1000 FORMAT('Filter window NSW=',I4)
      CALL XVMESSAGE(MSG8,' ')

      CALL XVPARM('LPERCENT',RPARM,ICOUNT,IDEF,1)
      L1PERC = RPARM(1)
      H2PERC = RPARM(1)
      CALL XVPARM('HPERCENT',RPARM,ICOUNT,IDEF,1)
      L2PERC = RPARM(1)
      H1PERC = RPARM(1)

      J = 1
C     ....Filter histogram, take derivative, and find candidate peaks
      DO 20 I=1,ITER
      CALL UNIFLT(4,NLEV,HISTW(1,J),histw(1,3-j),NSW)
      CALL HMAX(NLEV,NSW,NTOL,HISTW(1,3-J),histw(1,j),loc,npeak)
      IF (NPEAK.EQ.2) GOTO 30
      J = 3 - J
   20 CONTINUE
      IND=-1			!Unable to find two peaks
      RETURN

C     ....Here if two peaks located
   30 J = 3 - J
      LOPK = MIN0(LOC(1),LOC(2))	!Left peak
      HIPK = MAX0(LOC(1),LOC(2))	!Right peak

C     ....Find the minimum between the peaks.
      NLEV2 = HIPK - LOPK + 1
      DO 80 I=1,ITER
      CALL UNIFLT(4,NLEV2,HISTW(LOPK,J),histw(lopk,3-j),NSW)
      J = 3 - J
      IDUP = 0
      MINFREQ=1000000
      DO K=LOPK,HIPK
         IFREQ = HISTW(K,J)
         IF (IFREQ.EQ.0) GOTO 80
         IF (IFREQ.EQ.MINFREQ) IDUP=1
         IF (IFREQ.LT.MINFREQ) THEN
            IDUP = 0
            MINFREQ = IFREQ
            IMIN = K
         ENDIF
      ENDDO
      IF (IDUP.EQ.0) GOTO 90
   80 CONTINUE
      IND=-2				!Cannot find unique minumum
      RETURN

C     ....Calc area around low and high peaks and chk that each is .gt. ftol
   90 LSUM = 0
      DO I=1,IMIN
         LSUM = LSUM + HIST(I)
      END DO

      HSUM = 0
      DO I=IMIN+1,NLEV
         HSUM = HSUM + HIST(I)
      END DO

      SUM = LSUM + HSUM
      IND=-3
      FF = 100.*FLOAT(LSUM)/FLOAT(SUM)
      IF (FF.GT.100.-FTOL .OR. FF.LT.FTOL) RETURN

C     ....Compute stretch limits around low peak
      CALL ASTRCH(HIST,lpt1,lpt2,L1PERC,H1PERC,IMIN)
      IND=-4
      IF (LPT1.LT.0) LPT1=0
      IF (LPT2.GT.IMIN-1) LPT2=IMIN-1
      IF (LPT1.GE.LPT2) RETURN

C     ....Compute stretch limits around high peak
      CALL ASTRCH(HIST(IMIN+1),hpt1,hpt2,L2PERC,H2PERC,NLEV-IMIN)
      HPT1 = HPT1 + IMIN
      HPT2 = HPT2 + IMIN
      IND=-5
      IF (HPT1.LT.IMIN) HPT1=IMIN
      IF (HPT2.GT.NLEV-1) HPT2=NLEV-1
      IF (HPT1.GE.HPT2) RETURN

C     ....Compute piece-wise linear stretch
      CALL XVMESSAGE('*** BIMODAL OPTIION ***',' ')
      LPT1 = LPT1 + INMIN
      LPT2 = LPT2 + INMIN
      HPT1 = HPT1 + INMIN
      HPT2 = HPT2 + INMIN
      T(0) = DNMIN
      T(1) = DNMIN
      T(2) = (DNMIN+DNMAX)/2
      T(3) = T(2) + 1
      T(4) = DNMAX
      T(5) = DNMAX
      CALL XVPARM('BIMODAL',iparm,icount,idef,1)
      DO I=1,ICOUNT
         T(I-1) = IPARM(I)
      ENDDO
      CALL COMPUTE_LST(INMIN,LPT1,T(0),T(1),lut)
      CALL COMPUTE_LST(LPT1,LPT2,T(1),T(2),lut)
      CALL COMPUTE_LST(LPT2,HPT1,T(2),T(3),lut)
      CALL COMPUTE_LST(HPT1,HPT2,T(3),T(4),lut)
      CALL COMPUTE_LST(HPT2,INMAX,T(4),T(5),lut)
cccc      IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Find all local maxima in histogram and return their locations.
C
      SUBROUTINE HMAX(NLEV,NSW,NTOL,HIST,histo,loc,npeak)
      IMPLICIT NONE
      INTEGER*4 NLEV		!# of DN levels in histogram
      INTEGER*4 NSW		!Width of pre-filter width
      INTEGER*4 NTOL		!Determines whether a peak is significant
      INTEGER*4 HIST(NLEV)	!Input histogram
      INTEGER*4 HISTO(NLEV)	!Ouput filtered histogram
      INTEGER*4 LOC(10)		!DN location of maxima
      INTEGER*4 NPEAK		!# of local maxima in histogram

      INTEGER I,FIRST,LAST

C     ....Compute derivative of histogram
      HISTO(1) = 0
      DO I=2,NLEV
         HISTO(I)=HIST(I)-HIST(I-1)
      END DO

      FIRST = 1    + MAX0(NSW/2,1)
      LAST  = NLEV - MAX0(NSW/2,1)
      NPEAK = 0
C     ....Find all local maxima larger than NTOL
      DO 30 I=FIRST,LAST
      IF (.NOT.(HISTO(I-1).GT.0.AND.HISTO(I+1).LT.0)) GOTO 30
      IF (HISTO(I-1)-HISTO(I+1).LE.NTOL) GOTO 30
      IF (NPEAK.GT.0) THEN
         IF (LOC(NPEAK).EQ.I-1) GOTO 30	!Reject if max is adjacent to last max
      ENDIF
      NPEAK = NPEAK + 1		!New relative max found
      IF (NPEAK.GT.2) RETURN
      LOC(NPEAK) = I
   30 CONTINUE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute linear stretch table so that IMIN maps to OMIN and IMAX maps to OMAX
C
      SUBROUTINE COMPUTE_LST(IMIN,IMAX,OMIN,OMAX,lut)
      IMPLICIT NONE
      INTEGER*4 IMIN,IMAX,OMIN,OMAX
      INTEGER*2 LUT(-32768:32767)

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,ODN
      REAL*4 A,B

      A = FLOAT(OMAX-OMIN)/FLOAT(IMAX-IMIN)
      B = -A*IMIN + OMIN

      DO IDN=IMIN,IMAX
         ODN = NINT(A*IDN + B)
         IF (ODN.LT.DNMIN) ODN=DNMIN
         IF (ODN.GT.DNMAX) ODN=DNMAX
         LUT(IDN) = ODN
      ENDDO
      RETURN
      END
