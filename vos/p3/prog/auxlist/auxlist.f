	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C     29 APR 91   ...REA...    CONVERT TO UNIX/VICAR
C     30 NOV 89   ...REA...    INITIAL RELEASE
C
	REAL BUF(14)
	INTEGER IDN(12)
	CHARACTER*80 PR
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('  Line Blackbody Temp  Band 1    Band 2    Band 
     +3    Band 4    Band 5    Band 6',' ')
	CALL XVMESSAGE('          Low   High  Low High  Low High  Low Hi
     +gh  Low High  Low High  Low High',' ')
C
	IEL = ISL+NL-1   
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,' ')
	    DO J=1,12
		IDN(J) = BUF(J+2)
	    END DO
	    WRITE (PR,50) I,BUF(1),BUF(2),(IDN(J),J=1,12)
   50	    FORMAT(I6,2F7.1,12I5)
	    CALL XVMESSAGE(PR,' ')
	END DO
C
	CALL XVMESSAGE(' ',' ')
	RETURN
	END
