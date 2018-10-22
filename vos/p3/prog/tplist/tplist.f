	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C     2  MAY 89   ...REA...    INITIAL RELEASE
C    12  AUG 93   ...REA...    PORT TO UNIX
C
	REAL BUF(800)
	CHARACTER*80 PR
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(
     +	'Tiepoint  New Line   New Sample     Old Line   Old Sample',' ')
	CALL XVMESSAGE(
     +	'--------  --- ----   --- ------     --- ----   --- ------',' ')
C
	DO I=1,10
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    DO N=1,200
		J = 4*N-3
C
		IF (BUF(J) .EQ. 0.0 .AND. BUF(J+1) .EQ. 0.0) GO TO 100
		WRITE (PR,50) N,BUF(J),BUF(J+1),BUF(J+2),BUF(J+3)
   50		FORMAT(I6,F11.2,F12.2,F14.2,F12.2)
		CALL XVMESSAGE(PR,' ')
	    END DO
	END DO
C
  100	CONTINUE
	CALL XVMESSAGE(' ',' ')
	RETURN
	END
