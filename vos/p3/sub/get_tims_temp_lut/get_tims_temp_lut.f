	SUBROUTINE GET_TIMS_TEMP_LUT(IDATE,SHIFT,TEMP_LUT)
C
C	Modified for Y2K compliance 10/7/1998
C
	COMMON /RAWFIL/ FILTER,WVLEN,ICALDATE
C
	REAL FILTER(158,6),WVLEN(158)
	INTEGER*2 TEMP_LUT(32767,6)
	CHARACTER*100 LUTNAME,P3INC
	CHARACTER*6 CALDATE
C
	CALL GETFIL(IDATE,0)
C
	IF (SHIFT .EQ. 0.0) THEN
C						find and read in the appropriate
C						radiance to temp lookup table
	    CALL XGETENV_VIC('P3INC',P3INC)
	    WRITE (CALDATE,'(I6)') ICALDATE
	    IF (ICALDATE .LT. 10) CALDATE(1:5) = '00000'
	    IF (ICALDATE .LT. 100) CALDATE(1:4) = '0000'
	    IF (ICALDATE .LT. 1000) CALDATE(1:3) = '000'
	    IF (ICALDATE .LT. 10000) CALDATE(1:2) = '00'
	    IF (ICALDATE .LT. 100000) CALDATE(1:1) = '0'
	    LUTNAME = P3INC(1:LNBLNK(P3INC)) // '/tims_temp_lut.'
     +		      // CALDATE
	    CALL XVUNIT(LUTUNIT,'CALIB',2,ISTAT,'U_NAME',LUTNAME,' ')
	    CALL XVOPEN(LUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	    DO ICHAN=1,6
		CALL XVREAD(LUTUNIT,TEMP_LUT(1,ICHAN),ISTAT,' ')
	    END DO
	ELSE
C						LUT must be built, since a
C						wvlen shift screws everything up
	    DO I=1,158
		WVLEN(I) = WVLEN(I) + SHIFT
	    END DO
C
	    CALL XVMESSAGE('Building new temperature LUT',' ')
C				     calculate filter function normalizing terms
	    DO ICHAN=1,6
		FILTER_NORM = 0.0
		DO I=1,158
		    FILTER_NORM = FILTER_NORM + FILTER(I,ICHAN)
		END DO
C								       build LUT
		IRAD_LAST = 0
		RAD_LAST = 0.0
C						temperature range -188 to 195
C						should fill the table
		DO ITEMP=-18800,19500
C						convert to Kelvin, and add 0.005
C						to reach boundary with next step
		    TEMP = FLOAT(ITEMP+27315)/100.0 + 0.005
C						  compute radiance for this temp
		    RAD = 0.0
		    DO I=1,158
			RAD = RAD+FILTER(I,ICHAN)*PLANCK(WVLEN(I),TEMP)
		    END DO
		    RAD = 1000.0*RAD/FILTER_NORM
C						This temp should fill all 
C						previously unfilled bins, up to
C						the position IRAD
		    IRAD = MIN(INT(RAD),32767)
		    IF (IRAD .GT. IRAD_LAST) THEN
			DO LOC=IRAD_LAST+1,IRAD
			    TEMP_LUT(LOC,ICHAN) = ITEMP
			END DO
		    END IF
		    IF (IRAD .EQ. 32767) GO TO 500
		    IRAD_LAST = IRAD
		    RAD_LAST = RAD
		END DO
  500		CONTINUE
	    END DO
	END IF
	RETURN
	END
