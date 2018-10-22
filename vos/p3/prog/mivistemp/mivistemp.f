C
C	VICAR program MIVISTEMP, used to convert from upwelling radiance at
C	surface to spectral kinetic temperature of the surface, assuming a
C	given spectral emissivity.  This program works on the thermal IR bands
C	of MIVIS (Channels 93-102).
C
C	Ron Alley	9 February 1999
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL EMIS(10),RAD(2000)
	REAL SKY(10)/10*0.0/
	REAL PI/3.141593/
	INTEGER*2 LUT(32767,10),ITEMP(2000)
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
     +		   'U_NB',10,'U_NL',NL,'U_NS',NS,'U_ORG','BSQ',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C							      update VICAR label
	CALL XLDEL(IOUT,'HISTORY','LBL1',ISTAT,'HIST','MIVISTIR',' ')
	CALL XLDEL(IOUT,'HISTORY','LBL2',ISTAT,'HIST','MIVISTIR',' ')
	CALL XLADD(IOUT,'HISTORY','LBL1',
     +		 'MIVIS Temperature Image',ISTAT,'FORMAT','STRING',' ')
	CALL XLADD(IOUT,'HISTORY','LBL2','DN = 100*Degrees Celsius',
     +		   ISTAT,'FORMAT','STRING',' ')
	CALL XVPARM('INP',INP_NAMES,NUM,IDEF,2)
	CALL XLADD(IOUT,'HISTORY','TEMP_LUT',INP_NAMES(2),ISTAT,
     +		   'FORMAT','STRING',' ')
C						   Get the emissivity parameters
	CALL XVPARM('EMIS',EMIS,NUM,IDEF,10)
C						   Get the downwelling radiances
C						   from the VICAR label
	IF (EMIS(1) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY93',SKY(1),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(2) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY94',SKY(2),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(3) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY95',SKY(3),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(4) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY96',SKY(4),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(5) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY97',SKY(5),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(6) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY98',SKY(6),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(7) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY99',SKY(7),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(8) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY100',SKY(8),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(9) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY101',SKY(9),
     +					 ISTAT,'FORMAT','REAL',' ')
	IF (EMIS(10) .NE. 1.0) CALL XLGET(IN,'HISTORY','SKY102',SKY(10),
     +					  ISTAT,'FORMAT','REAL',' ')
C
C					    Convert downwelling radiances to the
C						 amount of radiance reflected up
	DO ICHAN=1,10
	    SKY(ICHAN) = (1.0-EMIS(ICHAN))*SKY(ICHAN)/PI
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
		    INDEX = MAX(MIN(NINT(RADX),32767),1)
		    ITEMP(ISAMP) = LUT(INDEX,ICHAN)
		END DO
		CALL XVWRIT(IOUT,ITEMP,ISTAT,' ')
	    END DO
	END DO
C								Close datasets
	CALL XVCLOSE(IN,ISTAT,' ')
	CALL XVCLOSE(IOUT,ISTAT,' ')
	RETURN
	END
