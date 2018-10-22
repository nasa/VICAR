	SUBROUTINE GET_SEBASS_WAVLEN(IDATE,WAVLEN)
C
C	This routine accepts a date, IDATE, (in yyyymmdd format), finds
C	the appropriate SEBASS calibration file, then loads the central
C	wavelengths of the 128 channels into the WAVLEN array.
C
	REAL WAVLEN(128)
	CHARACTER*100 INDEXNAME,P3INC,CALTABNAME
	CHARACTER*80 MSG
C								open index file
	CALL XGETENV_VIC('P3INC',P3INC)
	INDEXNAME = P3INC(1:LNBLNK(P3INC)) // '/sebass.index'
	OPEN (85,FILE=INDEXNAME,ERR=900,STATUS='OLD')
C							       search index file
C							  for proper calibration
	READ (85,*) JDATE
	DO WHILE (IDATE .GT. JDATE)
	    IDATE_OF_CAL = JDATE
	    READ (85,*,END=100) JDATE
	END DO
  100	CONTINUE
	CLOSE(85)
	WRITE (MSG,200) IDATE_OF_CAL
  200	FORMAT(I10,' SEBASS calibration file being used.')
	CALL XVMESSAGE(MSG,' ')
C							   open calibration file
	CALTABNAME = P3INC(1:LNBLNK(P3INC)) // '/sebass.' // MSG(3:10)
	OPEN (85,FILE=CALTABNAME,ERR=920,STATUS='OLD')

	CALL XVUNIT(IUNIT,'CALIB',1,ISTAT,'U_NAME',LUTNAME,' ')
C							   read calibration file
	DO I=1,128
	    READ (85,*,ERR=950) ICHAN,WAVLEN(I)
	END DO
	CLOSE(85)
	RETURN
C								error conditions
  900	CALL XVMESSAGE('SEBASS index file not found',' ')
	CALL ABEND
  920   CALL XVMESSAGE('SEBASS calibration file not found',' ')
	CALL ABEND
  950   CALL XVMESSAGE('SEBASS calibration file read error',' ')
	CALL ABEND
	RETURN
	END
