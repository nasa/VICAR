C
C	VICAR program ASTERTEMP, used to convert from upwelling radiance at
C	surface to spectral kinetic temperature of the surface, assuming a
C	given spectral emissivity and downwelling sky irradiance.  This
C	program works on the thermal IR bands of ASTER (Channels 10-14).
C
C	Ron Alley	11 June 2000
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIS(5),SKY(5),RAD(2000),RLUT(32767,5)
	REAL PI/3.141593/
	INTEGER*2 ITEMP(2000)
	LOGICAL QINTERP,XVPTST
	CHARACTER*80 INP_NAMES(2)
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INLUT,'INP',2,ISTAT,' ')
	CALL XVOPEN(INLUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_FORMAT','HALF','O_FORMAT','HALF',
     +		   'U_NB',5,'U_NL',NL,'U_NS',NS,'U_ORG','BSQ',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							      update VICAR label
	CALL XLADD(IOUT,'HISTORY','LBL1',
     +		 'ASTER Temperature Image',ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = 100*Degrees Celsius',
     +		   ISTAT,'FORMAT','STRING',' ')
	CALL XVPARM('INP',INP_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','TEMP_LUT',INP_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C							      Get the parameters
	CALL XVPARM('SKYRAD',SKY,NUM,IDEF,5)
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,5)
	QINTERP = .NOT. XVPTST('NO')
C
C					    Convert downwelling radiances to the
C						 amount of radiance reflected up
	DO ICHAN=1,5
	    SKY(ICHAN) = (1.0-EMIS(ICHAN))*SKY(ICHAN)/PI
	END DO
C					       Load the temperature lookup table
	DO ICHAN=1,5
	    CALL XVREAD(INLUT,RLUT(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INLUT,ISTAT,' ')
C								   Do conversion
	IF (QINTERP) THEN
	    CALL XVMESSAGE(' Interpolating lookup table values',' ')
	    DO ICHAN=1,5
		DO ILINE=ISL,ISL+NL-1
		    CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		    DO ISAMP=1,NS
			IF (RAD(ISAMP) .LE. 0.0) THEN
			    ITEMP(ISAMP) = -27315
			ELSE
			    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
			    INDEX = MAX(MIN(INT(RADX),32766),1)
			    FRAC = RADX - FLOAT(INDEX)
			    TEMP = (1.0-FRAC)*RLUT(INDEX,ICHAN) + 
     +				   FRAC*RLUT(INDEX+1,ICHAN) - 273.15
			    ITEMP(ISAMP) = MAX(MIN(NINT(100.0*TEMP),
     +						   32767), -32768)
			END IF
		    END DO
		    CALL XVWRIT(IOUT,ITEMP,ISTAT,' ')
		END DO
	    END DO
	ELSE
	    CALL XVMESSAGE(' Using closest lookup table values',' ')
	    DO ICHAN=1,5
		DO ILINE=ISL,ISL+NL-1
		    CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		    DO ISAMP=1,NS
			IF (RAD(ISAMP) .LE. 0.0) THEN
			    ITEMP(ISAMP) = -27315
			ELSE
			    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
			    INDEX = MAX(MIN(NINT(RADX),32767),1)
			    TEMP = RLUT(INDEX,ICHAN) - 273.15
			    ITEMP(ISAMP) = MAX(MIN(NINT(100.0*TEMP),
     +					       32767),-32768)
			END IF
		    END DO
		    CALL XVWRIT(IOUT,ITEMP,ISTAT,' ')
		END DO
	    END DO
	END IF
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
