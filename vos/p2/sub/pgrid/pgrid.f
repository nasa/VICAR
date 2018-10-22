      SUBROUTINE PGRID(X,NR,NC,TRIX,MODE)

C          ROUTINE TO LIST GRID COORDINATES

      REAL*4 X(2,1)
      INTEGER*2 TRIX(*)
      CHARACTER*132 PBUF,QBUF,RBUF,SBUF
C
C
      JJJ = 0
C          PAGE LOOP
    5 JJ = JJJ
      IF (JJ .NE. 0) CALL XVMESSAGE(' ',' ')
      NCO = MIN0(NC-JJ,20)
      SBUF(1:132) = ' '
      PBUF(1:132) = ' '
      QBUF(1:132) = ' '
      RBUF(1:132) = ' '
C
      DO 20 M = 1, NR
         I = 3
         J = JJ
C
         CALL XVMESSAGE(' ',' ')
         DO 10 N = 1, NCO
            I = I + 6
            J = J + 1
            WRITE (SBUF(I-1-3:I-1),'(I4)') J
            WRITE (PBUF(I-5:I),'(F6.1)') X(1,J)
            WRITE (QBUF(I-5:I),'(F6.1)') X(2,J)
            IF (MODE.EQ.1) WRITE (RBUF(I-1-2:I-1),'(I3)') TRIX(J)
   10    CONTINUE
C
         CALL XVMESSAGE(SBUF,' ')
         CALL XVMESSAGE(PBUF,' ')
         CALL XVMESSAGE(QBUF,' ')
         IF(MODE.EQ.1) CALL XVMESSAGE(RBUF,' ')
   20 JJ = JJ + NC
C
      JJJ = JJJ + 20
      IF (JJJ .LT. NC) GOTO 5
C
      RETURN
      END
