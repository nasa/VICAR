CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute look-up table for histogram stretches
C
      SUBROUTINE HISTOGRAM_STRETCH(FORMAT,HIST,NPTS,
     &		imode,lut,nchar,prt)
      IMPLICIT NONE
      CHARACTER*8 FORMAT        !'BYTE' or 'HALF'
      INTEGER*4 HIST(-32768:32767)
      INTEGER*4 NPTS		!Number of pixels in histogram
      INTEGER*4 IMODE		!Stretch mode
      INTEGER*2 LUT(-32768:32767)!Computed stretch table
      INTEGER*4 NCHAR           !Number of characters in PRT buffer
      CHARACTER*80 PRT          !String to be inserted in image label

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

      INTEGER*4 NMIN,NMAX,ICOUNT,IDEF
      EXTERNAL NGAUSS,NRAMP,NELLIP,NPOWER
C
  225 FORMAT ('Gaussian Stretch: GSIGMA =',F7.2,'  GMEAN =',F9.2)
  245 FORMAT ('Power Law Stretch: POWER =',F8.2)
  285 FORMAT ('AUTO-STRETCH:',I7,' to',I7,' and',I7,' to',I7)

      IF (IMODE.EQ.11) THEN
         CALL XVMESSAGE('*** SMOOTH OPTION ***',' ')
         CALL HMOD(HIST,LUT,NRAMP,NPTS)
         PRT = 'Ramp CDF Stretch'
         NCHAR = 17
         CALL XVMESSAGE(PRT,' ')
cccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF

      IF (IMODE.EQ.12) THEN
         CALL XVMESSAGE('*** GAUSS OPTION ***',' ')
         CALL XVPARM('GSIGMA',gsigma,icount,idef,1)
         CALL XVPARM('GMEAN',GMEAN,icount,idef,1)
         IF (ICOUNT.EQ.0) GMEAN=(DNMIN+DNMAX)/2.0
         CALL HMOD(HIST,LUT,NGAUSS,NPTS)
         WRITE (PRT,225) GSIGMA,GMEAN
         CALL XVMESSAGE(PRT,' ')
         NCHAR = 52
ccccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF
 
      IF (IMODE.EQ.13) THEN
         CALL XVMESSAGE('*** ELLIPSE OPTION ***',' ')
         CALL HMOD(HIST,LUT,NELLIP,NPTS)
         PRT = 'Elliptical Stretch'
         NCHAR = 19
ccccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF

      IF (IMODE.EQ.14) THEN
         CALL XVMESSAGE('*** POWER LAW OPTION ***',' ')
         CALL HMOD(HIST,LUT,NPOWER,NPTS)
         WRITE (PRT,245) POWER
         NCHAR = 35
         CALL XVMESSAGE(PRT,' ')
ccccc         IF (FORMAT.EQ.'BYTE') CALL PTABLE(LUT,DNMIN,DNMAX)
         RETURN
      ENDIF

      IF (IMODE.EQ.15) THEN
         CALL BIMODAL(FORMAT,HIST,lut,prt,nchar,imode)
         IF (IMODE.EQ.15) RETURN
      ENDIF

      IF (IMODE.EQ.16 .OR. IMODE.EQ.17) THEN
         CALL HPEAK(IMODE,HIST,NPTS,lut,prt,nchar)
         RETURN
      ENDIF

      IF (IMODE.EQ.18) THEN
         CALL XVMESSAGE('*** AUTO-STRETCH OPTION ***',' ')
         CALL ASTRETCH(HIST,NPTS,lut,nmin,nmax)
         WRITE (PRT,285) NMIN,DNMIN,NMAX,DNMAX
         NCHAR=52
         CALL XVMESSAGE(PRT,' ')
         RETURN
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate a stretch table (LUT) to produce a histogram with the specified
C distribution.  The function pointer LCDF is used to call a routine which
C computes CDF(IDN).  I.e., LCDF points to one of the following functions:
C NRAMP, NGAUSS, NELLPI, or NPOWER.
C
      SUBROUTINE HMOD(HIST,lut,LCDF,NPTS)
      EXTERNAL  LCDF			!Function pointer
      INTEGER*4 HIST(-32768:32767)	!Input histogram
      INTEGER*2 LUT(-32768:32767)	!Output look-up table
      INTEGER*4 NPTS			!Number of pixels in HIST

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      INTEGER*4 IDN,ODN,CDF,HCDF,I

      HCDF = 0			!CDF computed by integrating over histogram
      IDN = INMIN

C     ....Compute LUT so that CDF of histogram matches CDF computed by function
      DO 30 ODN=DNMIN,DNMAX
      CDF = LCDF(ODN,NPTS,DNMIN,DNMAX)		!Call function to compute CDF
   20 IF (HCDF+HIST(IDN).GT.CDF) GOTO 30	!Compare the two CDFs
      HCDF = HCDF + HIST(IDN)			!Update histogram computed CDF
      LUT(IDN) = ODN
      IDN = IDN + 1
      IF (IDN.GT.INMAX) RETURN
      GOTO 20
   30 CONTINUE

      DO I=IDN,INMAX
         LUT(I) = DNMAX
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for uniform-distribution histogram
C
      FUNCTION NRAMP(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 DNMIN,DNMAX
      SAVE		!Save SLOPE for subsequent calls
      REAL*4 SLOPE

C     ....On first call, compute scale for normalizing CDF
      IF (ODN.LE.DNMIN) SLOPE=FLOAT(NPTS)/FLOAT(DNMAX-DNMIN)
      NRAMP = FLOAT(ODN-DNMIN)*SLOPE
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for gaussian-distribution histogram
C
      FUNCTION NGAUSS(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 DNMIN,DNMAX
      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

      SAVE	!Save variables for subsequent calls
      REAL*4 ANORM,SUM
      REAL*4 GMEAN2,ST,A,B

C     ....On first call, compute normalization factor ANORM
      IF (ODN.GT.DNMIN) GOTO 100
      GMEAN2 = 2.0*GMEAN
      SIGMA = (DNMAX-DNMIN+1)/(2*GSIGMA)
      A = 1./(SQRT(2.*3.14159)*SIGMA)
      SIGSQ = SIGMA*SIGMA
      B = -1./(2.*SIGSQ)
      SS = FLOAT(DNMIN)**2 - 2.*DNMIN*GMEAN + GMEAN**2
      ST = SS
C     ....Compute normalization constant ANORM
      SUM = 0.
      DO I=DNMIN,DNMAX
         SUM = SUM + A*EXP(B*SS)
         SS = SS - GMEAN2 +2*I + 1.
      ENDDO
      ANORM = A*FLOAT(NPTS)/SUM

      SUM = 0.
C     ....Accumulate CDF of normalized gaussian
  100 SUM = SUM + ANORM*EXP(B*ST)
      ST = ST - GMEAN2 + 2*ODN + 1.
      NGAUSS = NINT(SUM)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for elliptic-distribution histogram
C
      FUNCTION NELLIP(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 NPTS	!Total number of pixels in histogram
      INTEGER*4 DNMIN,DNMAX	!DN range of histogram

      SAVE	!Save variables for subsequent calls

C     ....On first call, compute ellipse parameters
      IF (ODN.GT.DNMIN) GOTO 100
      B = FLOAT(DNMAX-DNMIN+1)/2.
      BSQ = B*B
      NOFFST= NINT(B)
      SUM=0.

C     ....Compute discrete ellipse area
      DO I=DNMIN,DNMAX
         X = I - NOFFST
         BSQXSQ = BSQ - X*X
         IF (BSQXSQ.LT.0.) BSQXSQ=0.
         SUM = SUM + SQRT(BSQXSQ)
      END DO

C     ....Calculate normalization factor
      ANORM=FLOAT(NPTS)/SUM
      SUM=0.

C     ....Accumulate CDF of normalized ellipse
  100 X = ODN - NOFFST
      BSQXSQ = BSQ - X*X
      IF (BSQXSQ.LT.0.) BSQXSQ=0.
      Y = ANORM*SQRT(BSQXSQ)
      SUM = SUM + Y
      NELLIP = NINT(SUM)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return CDF(ODN) for power-distribution histogram
C The power function is of the form:
C      y = 1 -ABS(x)**POWER  on interval (-1,1)
C but is scaled so that the function covers the interval (dnmin,dnmax) and
C the max occurs and mid-range.
C
      FUNCTION NPOWER(ODN,NPTS,DNMIN,DNMAX)
      INTEGER*4 ODN	!Output DN value ( CDF(ODN) is returned )
      INTEGER*4 NPTS	!Total number of pixels in histogram
      INTEGER*4 DNMIN,DNMAX	!DN range of histogram

      SAVE	!Save variables for subsequent calls

      COMMON/C2/GSIGMA,GMEAN,POWER
      REAL*4 GSIGMA,GMEAN,POWER

C     ....On first call, compute function parameters
      IF (ODN.GT.DNMIN) GOTO 100
      HLDN = DNMAX - DNMIN
      SUM = 0.
      DO I=DNMIN,DNMAX
         X = I - DNMIN
         C = ABS(2.*(X/HLDN)-1.)
         C = C**POWER
         SUM = SUM + (1.-C)
      END DO

C     ....Calculate normalization factor
      ANORM = FLOAT(NPTS)/SUM
      SUM = 0.

C     ....Accumulate cdf of normalized power law function
  100 X = ODN - DNMIN
      C = ABS(2.*(X/HLDN)-1.)
      Y = ANORM*(1.-C**POWER)
      SUM = SUM + Y
      NPOWER = NINT(SUM)
      RETURN
      END
