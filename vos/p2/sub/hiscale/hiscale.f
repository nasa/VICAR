C Compute compressed output histogram OHIST:
C   Scale the output levels by SCALE
C   Set all values below 0 DN to 0 DN
C   Set all values above 255 DN to 255 DN
C
      SUBROUTINE HISCALE(HIST,NPTS,SCALE,ohist,lsat,hsat)
      INTEGER*4 HIST(-32768:32767),OHIST(0:255)
      REAL*4 LSAT,HSAT,SCALE
      INTEGER*4 ODN

      NLOW = 0			!Number of pixels below 0 DN
      NHIGH = 0			!Number of pixels above 255 DN
C     ....Compute compressed output histogram OHIST
      CALL ZIA(OHIST,256)

      IF (NPTS.EQ.0) THEN
	LSAT = 0.0D0
	HSAT = 0.0D0
	GOTO 15
      ENDIF
	
      DO J=-32768,-1
         NLOW = NLOW + HIST(J)
      ENDDO

      DO J=0,32767
         NPIXELS = HIST(J)
         IF (NPIXELS.GT.0) THEN
            ODN = (J+.1)*SCALE		!Scale DN value for output histogram
            IF (ODN.GT.255) THEN
               NHIGH = NHIGH + NPIXELS
            ELSE
               OHIST(ODN) = OHIST(ODN) + NPIXELS
            ENDIF
         ENDIF
      ENDDO

      OHIST(0) = OHIST(0) + NLOW
      OHIST(255) = OHIST(255) + NHIGH
      LSAT = 100.0*OHIST(0)/NPTS	!Percent saturation at low DN
      HSAT = 100.0*OHIST(255)/NPTS	!Percent saturation at high DN

   15 RETURN
      END
