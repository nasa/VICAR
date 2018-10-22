      SUBROUTINE histat(OHIST,NPTS,mean,sigma,maxfreq)
C   Compute mean and standard deviation (for byte inputs only).
C
      INTEGER*4 OHIST(0:255)
      REAL*4 MEAN,SIGMA
      REAL*8 DMEAN,DSIGMA

      DMEAN = 0.0D0		!Mean DN of input image
      DSIGMA = 0.0D0		!Standard deviation of input image
      MAXFREQ = 0

      IF (NPTS.EQ.0) THEN	!Test number of pixels against 0
	MEAN = DMEAN            !Set mean, std. dev. & max. freq.
	SIGMA = DSIGMA		!to 0; then exit routine.
        GOTO 15
      ENDIF 

      DO 10 J=0,255
      NPIXELS = OHIST(J)
      IF (NPIXELS.EQ.0) GOTO 10
      DN = J
      DMEAN = DMEAN + NPIXELS*DN
      DSIGMA = DSIGMA + NPIXELS*DN**2
      IF (J.NE.0.AND.J.NE.255.AND.NPIXELS.GT.MAXFREQ) MAXFREQ=NPIXELS
   10 CONTINUE
    
      DMEAN = DMEAN/NPTS
      MEAN = DMEAN
      SIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
   15 RETURN
      END
C
C   Compute mean and standard deviation (for halfword inputs only).
C
      SUBROUTINE histat2(HIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 MEAN,SIGMA
      REAL*8 DMEAN,DSIGMA

      DN = 0.0			!DN value
      DMEAN = 0.0D0		!Mean DN of input image
      DSIGMA = 0.0D0		!Standard deviation of input image
      MINDN = 32767
      MAXFREQ = 0

      IF (NPTS.EQ.0) THEN	!Test number of pixels against 0
	MEAN = DMEAN		!Set mean, std. dev. & max. freq.
	SIGMA = DSIGMA		!to 0; set max. and min. DN to 
	MAXDN = MINDN		!32767; then exit program.
        GOTO 15
      ENDIF 

      DO 10 J=-32768,32767
      NPIXELS = HIST(J)
      IF (NPIXELS.EQ.0) GOTO 10
      DN = J
      DMEAN = DMEAN + NPIXELS*DN
      DSIGMA = DSIGMA + NPIXELS*DN**2
      IF (J.LT.MINDN) MINDN=J
      IF (J.NE.-32768.AND.J.NE.32767.AND.NPIXELS.GT.MAXFREQ)
     &		 MAXFREQ=NPIXELS
   10 CONTINUE

      MAXDN = DN + 0.5
      DMEAN = DMEAN/NPTS
      MEAN = DMEAN
      SIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
   15 RETURN
      END
