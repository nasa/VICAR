      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     1  MAY 89   ...REA...    INITIAL RELEASE
C     3  DEC 93   ...REA...    Port to Unix
C
	REAL BUF(30000,3)
	INTEGER ISUBAREA(200),INUNIT(3),IOUT(1001,900)
	CHARACTER*80 MSG
	CHARACTER*4 FMT
C
	SQRT3 = SQRT(3.0)
C								open inputs
	DO I=1,3
	    CALL XVUNIT(INUNIT(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(INUNIT(I),ISTAT,'OPEN_ACT','SA',
     +		        'IO_ACT','SA','U_FORMAT','REAL',' ')
	END DO
C							        get parameters
C								width
	CALL XVPARM('WIDTH',NS,ICNT,IDEF,0)
	NL = NS*SQRT3/2.0 + 0.5
	SAMPOFFSET = (NS+1.0)/2.0 + 0.5
C								format
	CALL XVPARM('FORMAT',FMT,ICNT,IDEF,0)
	IF (FMT.EQ.'BYTE') THEN
	    IBACK = 255
	    MAXVAL = 255
	ELSE
	    IBACK = -1
	    MAXVAL = 32767
	    IF (FMT.EQ.'FULL') MAXVAL=1E9
	END IF
C								area
	CALL XVPARM('AREA',ISUBAREA,N_AREA_PARS,IND,0)
	IF (N_AREA_PARS.EQ.0) THEN
	    N_AREA_PARS = 4
	    ISUBAREA(1) = 1
	    ISUBAREA(2) = 1
	    CALL XVGET(INUNIT(1),ISTAT,'NL',ISUBAREA(3),
     +		       'NS',ISUBAREA(4),' ')
	END IF
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'U_NL',NL,'U_NS',NS,'U_NB',1,
     +		    'U_ORG','BSQ','OP','WRITE','OPEN_ACT','SA',
     +		    'IO_ACT','SA','O_FORMAT',FMT,'U_FORMAT','FULL',' ')
C
C					      zero output array, fill background
	DO I=1,NL
	    LOW = SAMPOFFSET - (I-1)/SQRT3
	    IHI = SAMPOFFSET + (I-1)/SQRT3
	    DO J=1,LOW-1
		IOUT(J,I) = IBACK
	    END DO
	    DO J=LOW,IHI
		IOUT(J,I) = 0
	    END DO
	    DO J=IHI+1,NS
		IOUT(J,I) = IBACK
	    END DO
	END DO
C					             get the areas to process
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' Area(s) Processed:',' ')
	CALL XVMESSAGE(' ',' ')
	DO I=1,N_AREA_PARS,4
	    ISL = ISUBAREA(I)
	    ISS = ISUBAREA(I+1)
	    NLIN = ISUBAREA(I+2)
	    NSIN = ISUBAREA(I+3)
	    WRITE (MSG,100) ISL,ISS,NLIN,NSIN
  100	    FORMAT(I11,3I5)
	    CALL XVMESSAGE(MSG,' ')
C							gather data
	    DO J=1,NLIN
		DO K=1,3
		    CALL XVREAD(INUNIT(K),BUF(1,K),ISTAT,'LINE',J,
     +				'SAMP',ISS,'NSAMPS',NSIN,' ')
		END DO
C
		DO L=1,NSIN
		    X1 = BUF(L,1) + 0.001
		    X2 = BUF(L,2) + 0.001
		    X3 = BUF(L,3) + 0.001
		    ILINE = (1.0-X2/(X1+X2+X3))*(NL-1.0) + 1.5
		    ISAMP = ((X3-X1)*(ILINE-1.0))/((X3+X1)*SQRT3)
     +			    + SAMPOFFSET
		    IOUT(ISAMP,ILINE) = IOUT(ISAMP,ILINE) + 1
		END DO
	    END DO
	END DO
C								write output
	DO I=1,NL
	    DO J=1,NS
		IOUT(J,I) = MIN(IOUT(J,I),MAXVAL)
	    END DO
	    CALL XVWRIT(IOUTUNIT,IOUT(1,I),ISTAT,' ')
	END DO
	RETURN
	END
