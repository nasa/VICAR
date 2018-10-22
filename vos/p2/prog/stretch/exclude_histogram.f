CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Exclude DN ranges from histogram, as specified.
C
      SUBROUTINE EXCLUDE_HISTOGRAM(FORMAT,hist,npts)
      IMPLICIT NONE
      CHARACTER*8 FORMAT
      INTEGER*4 HIST(-32768:32767)
      INTEGER*4 NPTS			!Number of pixels in histogram

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 I,IDN,NPAIR,IXL,IXH,ICOUNT,IDEF
      REAL*4 TMEAN,TSIGMA,CUT

      REAL*4 RPARM(1000)
      INTEGER*4 IPARM(1000)
      EQUIVALENCE (RPARM,IPARM)

      CHARACTER*80 PRT
      LOGICAL*4 XVPTST
   20 FORMAT('DN values',I7,' thru',I7,' excluded')
   25 FORMAT('DN values',I7,' and',I7,' excluded')
   30 FORMAT(I7,' Pixels Outside Range',I7,' to',I7)
   40 FORMAT('Histogram after exclusion: Mean=',F11.4,' Sigma=',F11.4)
   50 FORMAT('Histogram after cutting: Mean=',F11.4,' Sigma=',F11.4)

C     ....Default is to exclude 0 and the maximum dn.
      IF (.NOT. XVPTST('INCLUDE'))  THEN
         WRITE (PRT,25) DNMIN,DNMAX
         CALL XVMESSAGE(PRT,' ')
         HIST(DNMIN) = 0
         HIST(DNMAX) = 0
      ENDIF        

      CALL XVPARM('REXCLUDE',IPARM,ICOUNT,IDEF,200)
      IF (ICOUNT.GT.0) THEN
         NPAIR=ICOUNT/2
         IF (2*NPAIR.NE.ICOUNT) GOTO 900
         CALL XVMESSAGE(' ',' ')
         DO I=1,ICOUNT,2
            IXL=IPARM(I)
            IXH=IPARM(I+1)
            IF (IXL.LT.INMIN) IXL=INMIN
            IF (IXH.GT.INMAX) IXH=INMAX
            IF (IXL.GT.IXH) GOTO 902
            WRITE (PRT,20) IXL,IXH
            CALL XVMESSAGE(PRT,' ')
            CALL ZIA(hist(ixl),IXH-IXL+1)	!zero out exluded entries
         END DO
      END IF

      CALL XVPARM('IEXCLUDE',IPARM,ICOUNT,IDEF,100)
      IF (ICOUNT.GT.0) THEN
         CALL XVMESSAGE(' ',' ')
         DO I=1,ICOUNT
            IDN=IPARM(I)
            IF (IDN.LT.INMIN) IDN=INMIN
            IF (IDN.GT.INMAX) IDN=INMAX
            HIST(IDN)=0				!zero out excluded entries
         END DO
      END IF

C     ....Calculate new mean and standard deviation
      CALL STATI(HIST,tmean,tsigma,npts)
      IF (NPTS.LE.0) GOTO 904
      CALL XVMESSAGE(' ',' ')
      WRITE (PRT,40) TMEAN,TSIGMA
      CALL XVMESSAGE(PRT,' ')

C     ....Exclude DNs with less than a specified percent of max DN level
      CALL XVPARM('CUT',RPARM,ICOUNT,IDEF,1)
      IF (ICOUNT.GT.0) THEN
         CUT=RPARM(1)
         IF (CUT.LT.0.0 .OR. CUT.GT.100.0) THEN
            CALL XVMESSAGE('INVALID CUT VALUE - RESET TO 5.0',' ')
            CUT=5.0
         END IF
         CALL FCLIP(HIST,INMIN,INMAX,CUT)
         CALL STATI(HIST,tmean,tsigma,npts)
         IF (NPTS.LE.0) GOTO 908
         CALL XVMESSAGE(' ',' ')
         WRITE (PRT,40) TMEAN,TSIGMA
         CALL XVMESSAGE(PRT,' ')
      END IF

C        Print number OF pixels outside expected range.
      RETURN

  900 CALL XVMESSAGE('***Invalid count for parameter REXCLUDE',' ')
      CALL ABEND
  902 CALL XVMESSAGE('***Error in rexclude range ***',' ')
      CALL ABEND
  904 CALL XVMESSAGE('***No pixels remain after exclusion',' ')
      CALL ABEND
  908 CALL XVMESSAGE('***No pixels remain after cutting',' ')
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Exclude all histogram entries with a frequency less than FCLIPC percent
C from the maximum frequency.
C
      SUBROUTINE FCLIP(HIST,INMIN,INMAX,FCLIPC)
      INTEGER*4 INMIN,INMAX
      INTEGER*4 HIST(-32768:32767)
      REAL      RTOL

C     ....Find max frequency
      MAX=0
      DO I=INMIN,INMAX
         IF (HIST(I).GT.MAX) MAX=HIST(I)
      END DO

C     ....Zero out any frequencies less than FCLIPC percent of max.
      RTOL = FLOAT(MAX)*(FCLIPC/100.0)
      DO I=INMIN,INMAX
         IF (HIST(I).LT.RTOL) HIST(I)=0
      END DO
      RETURN
      END
