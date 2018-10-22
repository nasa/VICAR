CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PEAK and MEAN stretch options
C
      SUBROUTINE HPEAK(IMODE,HIST,NPTS,lut,prt,nchar)
      IMPLICIT NONE
      INTEGER*4 IMODE,NPTS,NCHAR
      INTEGER*4 HIST(-32768:32767)
      INTEGER*2 LUT(-32768:32767)
      CHARACTER*80 PRT

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 ICOUNT,IDEF,IMEAN,IDN
      INTEGER*4 I1,I2,NPKFQ,NPKDN,NRANGE,DNMIDDLE
      INTEGER*4 NMIN,NMAX,NLEV
      REAL*4 A,B,XFACTR,AMEAN,RTRUNC
      CHARACTER*14 PRT3
  275 FORMAT (A14,I7,' to',I7,' and',I7,' to',I7)

      NLEV = INMAX - INMIN + 1
      IF (IMODE.EQ.16) THEN
         CALL XVMESSAGE('*** PEAK OPTION ***',' ')
         NPKFQ=0
         NPKDN=0
         I1 = INMIN + 1
         I2 = INMAX - 1
         DO IDN=I1,I2			!Find IDN with max frequency
            IF (NPKFQ.LT.HIST(IDN)) THEN
               NPKFQ = HIST(IDN)
               NPKDN = IDN
            END IF
         END DO
         PRT3 = 'Peak Stretch:'
      ENDIF

      IF (IMODE.EQ.17) THEN
         CALL XVMESSAGE('*** MEAN OPTION ***',' ')
         AMEAN=0.0
         DO IDN=INMIN,INMAX		!Compute mean DN
            AMEAN = AMEAN + IDN*HIST(IDN)
         ENDDO
         AMEAN = AMEAN/FLOAT(NPTS)
         IMEAN = NINT(AMEAN)
         NPKFQ = HIST(IMEAN)
         NPKDN = IMEAN
         PRT3 = 'Mean Stretch:'
      ENDIF

      CALL XVPARM('RANGE',nrange,icount,idef,1)
      IF (ICOUNT.EQ.1) THEN
         NMIN = NPKDN - NRANGE
         NMAX = NPKDN + NRANGE
      ELSE
         CALL ASTRETCH_LIMITS(HIST,NPTS,nmin,nmax)
      ENDIF

      DNMIDDLE = 0.5*(DNMIN+DNMAX)
      CALL XVPARM('FACTOR',xfactr,icount,idef,1)

C........Compute lower part of stretch table
      A = FLOAT(DNMIDDLE-DNMIN)/FLOAT(NPKDN-NMIN)
      IF (ICOUNT.EQ.1) A=XFACTR
      B = -A*NMIN + DNMIN
      DO IDN=INMIN,NPKDN
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO

C........Compute upper part of stretch table
      A = FLOAT(DNMAX-DNMIDDLE)/FLOAT(NMAX-NPKDN)
      IF (ICOUNT.EQ.1) A=XFACTR
      B = -A*NPKDN + DNMIDDLE
      DO IDN=NPKDN,INMAX
         LUT(IDN) = RTRUNC(A*IDN+B)
      ENDDO

      WRITE (PRT,275) PRT3,NMIN,DNMIN,NMAX,DNMAX
      NCHAR=52
      CALL XVMESSAGE(PRT,' ')
      RETURN
      END
