
      SUBROUTINE PMJS (U,N)
C--------------------------------------------------------------------------
C
C 83-5-2   ... LWK... taken from UCL RESLOC program.
C
C  Edit History:
C       4-94      CRI   MSTP S/W CONVERSION (VICAR PORTING)
C
C   This prints out 202 pairs of real*4 or real*8 numbers, in a format that
C   simulates the positioning of reseau marks on Voyager pictures.
C--------------------------------------------------------------------------


      REAL*4 U(N,2,202)
      CHARACTER*132 RBUF
      CHARACTER*132 QBUF
      CHARACTER*132 PBUF
c  u is real*4 if n=1, real*8 if n=2
c
      J = 1
c
      DO 10 II=1,12
	    DO JJ = 1, 132
                PBUF(JJ:JJ) = ' '
                QBUF(JJ:JJ) = ' '
                RBUF(JJ:JJ) = ' '
	    ENDDO
            RBUF(1:1) = '0'
            K = 7
            IF(II.GT.2.AND.II.LT.11) GOTO 5
c
            DO 4 I=1,12
                  WRITE (RBUF(K-2-2:K-2),'(I3)') J
                  WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
                  WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
                  J = J + 1
    4             K = K + 11
c
            GOTO 8
c
c
    5       DO 6 I=1,12
                  IF(I.GT.2.AND.I.LT.11) GOTO 6
                  WRITE (RBUF(K-2-2:K-2),'(I3)') J
                  WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
                  WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
                  J = J + 1
    6             K = K + 11
c
            IF(II.NE.3) GOTO 8
            WRITE (RBUF(91:93),'(I3)') 202
            WRITE (PBUF(90:95),'(F6.1)') U(1,1,202)
            WRITE (QBUF(90:95),'(F6.1)') U(1,2,202)
c
8           CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(RBUF(2:132),' ')
            CALL XVMESSAGE(PBUF(2:132),' ')
            CALL XVMESSAGE(QBUF(2:132),' ')
            CALL XVMESSAGE(' ',' ')
            IF(II.EQ.12) GOTO 20
	    DO JJ = 1, 132
                PBUF(JJ:JJ) = ' '
                QBUF(JJ:JJ) = ' '
                RBUF(JJ:JJ) = ' '
	    ENDDO
            RBUF(1:1) = '0'
            K = 13
c
            DO 9 I=1,11
                  WRITE (RBUF(K-2-2:K-2),'(I3)') J
                  WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
                  WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
                  J = J + 1
    9             K = K + 11
c
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(RBUF(2:132),' ')
            CALL XVMESSAGE(PBUF(2:132),' ')
            CALL XVMESSAGE(QBUF(2:132),' ')
  10        CALL XVMESSAGE(' ',' ')
c
   20 CALL XVMESSAGE(' ',' ')
      RETURN
      END
