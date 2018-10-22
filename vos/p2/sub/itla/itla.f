      SUBROUTINE ITLA(INT, B, N)
      include  'fortport'  ! defines INT2BYTE.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  83-3-1  ...  LWK                                               C
C  86-3-26 ...  JRS  ADD HELP AND TEST FILES                      C
C  92-9-18      SP   made portable for UNIX.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER      INT,  N
      BYTE         B(N), B1
C==================================================================
      IF (INT .LT. 0 .OR. INT .GT. 255) CALL MABEND('ITLA: PROGRAM ERR')
      B1 = INT2BYTE(INT)
      DO I=1,N
	B(I) = B1
      ENDDO

      RETURN
      END

