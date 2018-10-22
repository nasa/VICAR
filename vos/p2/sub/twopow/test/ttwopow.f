C  This program will test the FORTRAM Callable Subroutine
C  for twopow

       SUBROUTINE TTWOPOW()

       INTEGER*2 I,N,TWOPOW
       CHARACTER*40 MS1, MS2

       DO I= -16,9,5
         IF (TWOPOW(I,N).EQ.1) THEN
           WRITE(MS1, 100) I
           WRITE(MS2, 200) N
           CALL XVMESSAGE(MS1,' ')
           CALL XVMESSAGE(MS2,' ')
         ELSE
           WRITE(MS1, 100) I
           WRITE(MS2, 300) N
           CALL XVMESSAGE(MS1,' ')
           CALL XVMESSAGE(MS2,' ')
         ENDIF
       ENDDO           
 100   FORMAT ('number = ', I5)
 200   FORMAT ('  power of 2 = ', I5)
 300   FORMAT ('  next power of 2 = ', I5)
       RETURN
       END
