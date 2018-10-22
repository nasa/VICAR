	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	This program accepts either (a) the two 11x11 lat/long arrays
C	associated with ASTER scenes, and included in the ASTER hdf files,
C	or (b) the corner point latitude and longitude coordinates input
C	as parameters.  From this, a pair of full size images is created,
C	which give the latitude and longitude for each ASTER scene pixel.
C
C	5/14/01  ...rea... Initial release
C
	IMPLICIT NONE
	REAL*8 CORNER(2,4),XLATIN(11,11),XLONGIN(11,11),CONV
	REAL*8 CONVFAC/0.99330562/
	INTEGER NLO,NSO,NINP,ICOUNT,IDEF,INUNIT1,INUNIT2,ISTAT,NBANDS
	INTEGER ILINE,IOUT
	LOGICAL XVPTST
C
	CALL XVMESSAGE('ASTERGEO Version: May 18, 2001',' ')
C							   determine output size
	IF (XVPTST('VNIR')) THEN
	    NLO = 4200
	    NSO = 4980
	ELSE IF (XVPTST('SWIR')) THEN
	    NLO = 2100
	    NSO = 2490
	ELSE
	    NLO = 700
	    NSO = 830
	END IF
C					     is coordinate conversion requested?
	IF (XVPTST('GEODETIC')) THEN
	    CONV = 1.0 / CONVFAC
	ELSE IF (XVPTST('GEOCENTRIC')) THEN
	    CONV = CONVFAC
	ELSE
	    CONV = 1.0
	END IF
C						   get input files or parameters
	CALL XVPCNT('INP',NINP)
	IF (NINP .EQ. 0) THEN
	    CALL XVPARMD('UL',CORNER(1,1),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two UL values or input file needed',' ')
		CALL ABEND
	    ENDIF
	    CALL XVPARMD('UR',CORNER(1,2),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two UR values needed',' ')
		CALL ABEND
	    ENDIF
	    CALL XVPARMD('LL',CORNER(1,3),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two LL values needed',' ')
		CALL ABEND
	    ENDIF
	    CALL XVPARMD('LR',CORNER(1,4),ICOUNT,IDEF,2)
	    IF (ICOUNT .NE. 2) THEN
		CALL XVMESSAGE('Two LR values needed',' ')
		CALL ABEND
	    ENDIF
	ELSE IF (NINP .EQ. 1) THEN
	    CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	    CALL XVOPEN(INUNIT1,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	    CALL XVGET(INUNIT1,ISTAT,'NB',NBANDS,' ')
	    IF (NBANDS .LT. 2) THEN
		CALL XVMESSAGE(
     +	'If one input file, it must contain both lat and long bands',' ')
		CALL ABEND
	    ENDIF
	    DO ILINE=1,11
		CALL XVREAD(INUNIT1,XLATIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    'BAND',1,' ')
		CALL XVREAD(INUNIT1,XLONGIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    'BAND',2,' ')
	    END DO
	ELSE
	    CALL XVUNIT(INUNIT1,'INP',1,ISTAT,' ')
	    CALL XVOPEN(INUNIT1,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	    CALL XVUNIT(INUNIT2,'INP',2,ISTAT,' ')
	    CALL XVOPEN(INUNIT2,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     +		    'U_FORMAT','DOUB',' ')
	    DO ILINE=1,11
		CALL XVREAD(INUNIT1,XLATIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    ' ')
		CALL XVREAD(INUNIT2,XLONGIN(1,ILINE),ISTAT,'LINE',ILINE,
     +			    ' ')
	    END DO
	END IF
C							        open output file
	CALL XVUNIT(IOUT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUT,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &		'U_NL',NLO,'U_NS',NSO,'U_ORG','BIL','OP','WRITE',
     &		'U_FORMAT','DOUB','O_FORMAT','DOUB','U_NB',2,' ')
C
	IF (NINP .EQ. 0) THEN
	    CALL FILL2(IOUT,NLO,NSO,CORNER,CONV)
	ELSE
	    CALL FILL11(IOUT,NLO,NSO,XLATIN,XLONGIN,CONV)
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FILL2(IOUT,NLO,NSO,CORNER,CONV)
C
C	This routine creates the output dataset, from the corner points
C	that have been input by the user. Bilinear interpolation is used.
C
	IMPLICIT NONE
	REAL*8 CORNER(2,4),XLATOUT(4980),XLONGOUT(4980),CONV
	REAL*8 XNL,XNS,X,FRAC1,FRAC2
	INTEGER IOUT,NLO,NSO,ILINE,ISAMP,ISTAT
	REAL*8 PI/3.141592653589793D0/

C						 if requested, convert latitudes
	IF (CONV .NE. 1.0) THEN
	    DO ILINE=1,4
		X = CONV*TAN(CORNER(1,ILINE)*PI/180.)
		CORNER(1,ILINE) = ATAN(X)*180./PI
	    END DO
	END IF
C
	XNL = NLO
	XNS = NSO
C
	DO ILINE=1,NLO
	    X = ILINE - 1
	    FRAC1 = X/XNL
	    DO ISAMP=1,NSO
		X = ISAMP - 1
		FRAC2 = X/XNS
		XLATOUT(ISAMP) = (1.0-FRAC1)*(1.0-FRAC2)*CORNER(1,1) +
     +				 (1.0-FRAC1)*FRAC2*CORNER(1,2) +
     +				 FRAC1*(1.0-FRAC2)*CORNER(1,3) +
     +				 FRAC1*FRAC2*CORNER(1,4) 
		XLONGOUT(ISAMP) = (1.0-FRAC1)*(1.0-FRAC2)*CORNER(2,1) +
     +				  (1.0-FRAC1)*FRAC2*CORNER(2,2) +
     +				  FRAC1*(1.0-FRAC2)*CORNER(2,3) +
     +				  FRAC1*FRAC2*CORNER(2,4) 
	    END DO
	    CALL XVWRIT(IOUT,XLATOUT,ISTAT,' ')
	    CALL XVWRIT(IOUT,XLONGOUT,ISTAT,' ')
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FILL11(IOUT,NLO,NSO,XLATIN,XLONGIN,CONV)
C
C	This routine creates the output dataset, from the 11x11 latitude
C	and longitude arrays. Bilinear interpolation is used.
C
	IMPLICIT NONE
	REAL*8 XLATIN(11,11),XLONGIN(11,11),XLAT(11),XLONG(11)
	REAL*8 CONV,XNL,XNS,X,FRAC,XLATOUT(4980),XLONGOUT(4980)
	INTEGER IOUT,NLO,NSO,ILINE,ISAMP,INDEX,IX,ISTAT
	REAL*8 PI/3.141592653589793D0/
C
	XNL = NLO
	XNS = NSO
C						 if requested, convert latitudes
	IF (CONV .NE. 1.0) THEN
	    DO ILINE=1,11
		DO ISAMP=1,11
		    X = CONV*TAN(XLATIN(ISAMP,ILINE)*PI/180.)
		    XLATIN(ISAMP,ILINE) = ATAN(X)*180./PI
		END DO
	    END DO
	END IF
C
	DO ILINE=1,NLO
	    X = 10 * (ILINE - 1)
	    FRAC = X/XNL
	    INDEX = INT(FRAC)
	    FRAC = FRAC - INDEX
	    INDEX = INDEX + 1
	    DO IX=1,11
		XLAT(IX) = (1.0-FRAC)*XLATIN(IX,INDEX) + 
     +			   FRAC*XLATIN(IX,INDEX+1)
		XLONG(IX) = (1.0-FRAC)*XLONGIN(IX,INDEX) + 
     +			    FRAC*XLONGIN(IX,INDEX+1)
	    END DO
	    DO ISAMP=1,NSO
		X = 10 * (ISAMP - 1)
		FRAC = X/XNS
		INDEX = INT(FRAC)
		FRAC = FRAC - INDEX
		INDEX = INDEX + 1
		XLATOUT(ISAMP) = (1.0-FRAC)*XLAT(INDEX) + 
     +				 FRAC*XLAT(INDEX+1)
		XLONGOUT(ISAMP) = (1.0-FRAC)*XLONG(INDEX) + 
     +				  FRAC*XLONG(INDEX+1)
	    END DO
	    CALL XVWRIT(IOUT,XLATOUT,ISTAT,' ')
	    CALL XVWRIT(IOUT,XLONGOUT,ISTAT,' ')
	END DO
C
	RETURN
	END
