C
C	VICAR program MASTERTEMP, used to convert from upwelling radiance at
C	surface to spectral kinetic temperature of the surface, assuming a
C	given spectral emissivity.  This program works on the thermal IR bands
C	of MASTER (Channels 41-50).
C
C	Ron Alley	14 August 1998
C	Revision: 1.1   Report temperature lookup table name rea 9/21/98
C	Revision: 1.2   Change inputs and outputs to unscaled real pixels
C								rea 11/12/2004
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIS(10),RAD(2000),TEMP(2000)
	REAL SKY(10)/10*0.0/
	REAL PI/3.141593/
	INTEGER*2 LUT(32767,10)
	CHARACTER*80 INP_NAMES(2)
C						Open datasets, get size field
	CALL XVUNIT(IN,'INP',1,ISTAT,' ')
	CALL XVOPEN(IN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INLUT,'INP',2,ISTAT,' ')
	CALL XVOPEN(INLUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +	           'OP','WRITE','U_FORMAT','REAL','O_FORMAT','REAL',
     +		   'U_NB',10,'U_NL',NL,'U_NS',NS,'U_ORG','BSQ',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							      update VICAR label
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','MASTERTIR',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','MASTERTIR',' ')
	CALL XLADD(IOUT,'HISTORY','LBL1',
     +		 'MASTER Temperature Image',ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = Degrees Celsius',ISTAT,
     +		   'FORMAT','STRING',' ')
	CALL XVPARM('INP',INP_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','TEMP_LUT',INP_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C						   Get the emissivity parameters
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,10)
C						   Get the downwelling radiances
C						   from the VICAR label
        IF (EMIS(1) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY41',SKY(1),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(2) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY42',SKY(2),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(3) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY43',SKY(3),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(4) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY44',SKY(4),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(5) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY45',SKY(5),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(6) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY46',SKY(6),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(7) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY47',SKY(7),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(8) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY48',SKY(8),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(9) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY49',SKY(9),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
        IF (EMIS(10) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY50',SKY(10),
     +                   ISTAT,'FORMAT','REAL','HIST','MASTERTI',' ')
C
C					    Convert downwelling radiances to the
C						 amount of radiance reflected up
	DO ICHAN=1,10
	    SKY(ICHAN) = (1.0-EMIS(ICHAN))*SKY(ICHAN)/(1000.0*PI)
	END DO
C					       Load the temperature lookup table
	DO ICHAN=1,10
	    CALL XVREAD(INLUT,LUT(1,ICHAN),ISTAT,' ')
	END DO
	CALL XVCLOSE(INLUT,ISTAT,' ')
C								   Do conversion
	DO ICHAN=1,10
	    DO ILINE=ISL,ISL+NL-1
		CALL XVREAD(IN,RAD,ISTAT,'LINE',ILINE,'BAND',ICHAN,
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		DO ISAMP=1,NS
		    RADX = (RAD(ISAMP)-SKY(ICHAN)) / EMIS(ICHAN)
		    INDEX = MAX(MIN(NINT(1000.0*RADX),32767),1)
		    TEMP(ISAMP) = LUT(INDEX,ICHAN) / 100.0
		END DO
		CALL XVWRIT(IOUT,TEMP,ISTAT,' ')
	    END DO
	END DO
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
