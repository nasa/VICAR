	SUBROUTINE SQUEEZE(INBUF,OUTBUF,N)
C					This routine moves INBUF to OUTBUF
C					removing all blanks and replacing '#'
C					with a blank. A zero byte is placed at 
C					the new end of the string, and the 
C					length, N, is updated.
	CHARACTER*(*) INBUF,OUTBUF
C
	I = 0
	DO J=1,N
	    IF (INBUF(J:J).NE.' ') THEN
		I=I+1
		IF (INBUF(J:J).NE.'#') THEN
		    OUTBUF(I:I) = INBUF(J:J)
		ELSE
		    OUTBUF(I:I) = ' '
		END IF
	    END IF
	END DO
	OUTBUF(I+1:I+1) = CHAR(0)
	N = I
	RETURN
	END
