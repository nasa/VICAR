	SUBROUTINE ADD0(STRING,LEN,NEWLEN)
C
C		This routine adds a binary 0 (C string terminator) to the
C		end of a FORTRAN character variable, stripping off trailing
C		blanks in the process.  The new length of the string is returned
C		in NEWLEN.
C
	CHARACTER*(*) STRING
C
	DO NEWLEN=LEN,1,-1
	    IF(STRING(NEWLEN:NEWLEN) .NE. ' ') GO TO 100
	END DO
  100	CONTINUE
	STRING(NEWLEN+1:NEWLEN+1) = CHAR(0)
	RETURN
	END
