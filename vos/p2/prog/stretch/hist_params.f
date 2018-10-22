CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return histogram stretch and print parameters (but not all)
C
      SUBROUTINE HIST_PARAMS(FORMAT,imode,hflag,ihist,ohist,
     &		icdf,ocdf,power,nstretch)
      IMPLICIT NONE
      INTEGER*4 IMODE,HFLAG,IHIST,OHIST,ICDF,OCDF,NSTRETCH
      REAL*4 POWER
      CHARACTER*8 FORMAT
      LOGICAL XVPTST

      INTEGER*4 IPARM,ICOUNT,IDEF

      HFLAG=0		!=1 if histogram is needed
      IHIST=0		!=1 to print input histogram
      OHIST=0		!=1 to print output histogram
      ICDF=0		!=1 to print input CDF
      OCDF=0		!=1 to print output CDF

      IF (XVPTST('IHIST')) THEN		!Print input histogram?
         IF (FORMAT.EQ.'BYTE') THEN
            IHIST = 1
            HFLAG = 1
         ELSE
            CALL XVMESSAGE('Histogram available only for byte data',
     .                     ' ')
         END IF
      END IF

      IF (XVPTST('OHIST')) THEN		!Print output histogram?
         IF (FORMAT.EQ.'BYTE') THEN
            OHIST=1
            HFLAG=1
         ELSE
            CALL XVMESSAGE('Histogram available only for byte data',
     .                     ' ')
         END IF
      END IF

      IF (XVPTST('ICDF')) THEN		!Print input CDF?
         IF (FORMAT.EQ.'BYTE') THEN
            ICDF=1
            HFLAG=1
         ELSE
            CALL XVMESSAGE('CDF available only for byte data',' ')
         END IF
      END IF

      IF (XVPTST('OCDF')) THEN
         IF(FORMAT.EQ.'BYTE') THEN
            OCDF=1
            HFLAG=1
         ELSE
            CALL XVMESSAGE('CDF AVAILABLE ONLY FOR BYTE DATA',' ')
         END IF
      END IF

      IF (XVPTST('SMOOTH')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=11
         HFLAG=1
      END IF

      IF (XVPTST('GAUSS')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=12
         HFLAG=1
      END IF

      IF (XVPTST('ELLIPSE')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=13
         HFLAG=1
      END IF

      CALL XVPARM('POWER',IPARM,ICOUNT,IDEF,1)
      IF (ICOUNT .EQ. 1) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=14
         HFLAG=1
         POWER=IPARM
      END IF

      CALL XVPARM('BIMODAL',IPARM,ICOUNT,IDEF,1)
      IF (ICOUNT.GT.0) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=15
         HFLAG=1
      END IF

      IF (XVPTST('PEAK')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=16
         HFLAG=1
      END IF

      IF (XVPTST('MEAN')) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=17
         HFLAG=1
      END IF

      IF (XVPTST('ASTRETCH') .OR. IMODE.EQ.0) THEN
         CALL CHK_NUM_STRETCH(nstretch)
         IMODE=18
         HFLAG=1
      END IF

      RETURN
      END
