	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	This program accepts as input a TIMS image in temperature
C	units, and produces as output a simulation of an ASTER
C	image in radiance units.  It does this by matching each ASTER
C	channel to its closest TIMS channel, and assuming that there
C	is no emissivity change over the change of wavelengths.
C
C	4/11/97 ...rea... Modified to use the PFM calibration of the
C			  ASTER sensor
C
	REAL WAVE(6)/8.2873,
     &		     8.6349,
     &		     9.0793,
     &              99.9999,
     &		    10.6592,
     &		    11.2893/
	INTEGER*2 HBUF(10000)
C
C		Open data sets
C
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		    'U_FORMAT','HALF',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		'U_NL',NL,'U_NS',NS,'U_ORG','BIL','OP','WRITE',
     &		'U_FORMAT','HALF','O_FORMAT','HALF','U_NB',5,' ')
C
	IEL = ISL+NL-1
	IES = ISS+NS-1
C
	IF(IEL.GT.NLIN .OR. IES.GT.NSIN) THEN
	    CALL XVMESSAGE(
     &         ' Size field specified overruns file dimensions',' ')
	    CALL ABEND
	ENDIF
C
	CALL XLADD(IOUT,'HISTORY','LBL1',
     &		'Radiance Image to Simulate ASTER TIR',ISTAT,
     &		'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2',
     &		   'DN = milliwatts/m*m*sr*micrometer',
     &		   ISTAT,'FORMAT','STRING',' ')
C
	DO ILINE=ISL,IEL
C
	    DO ICHAN=1,6
C							read & write a line
		CALL XVREAD(INUNIT,HBUF,ISTAT,'LINE',ILINE,'SAMP',ISS,
     &			    'BAND',ICHAN,'NSAMPS',NS,' ')
		IF (ICHAN .NE. 4) THEN
		    DO ISAMP=1,NS
			X = (FLOAT(HBUF(ISAMP)) / 100.0) + 273.15
			Y = 1000.0 * PLANCK(WAVE(ICHAN),X)
			HBUF(ISAMP) = NINT(Y)
		    END DO
		    CALL XVWRIT(IOUT,HBUF,ISTAT,' ')
		END IF
	    END DO
	END DO
C							Close all datasets
C
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
C
	RETURN
	END
