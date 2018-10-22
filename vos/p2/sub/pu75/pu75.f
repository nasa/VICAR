C------------------------------------------------------------------
C                        VICAR SUBROUTINE pu75
C
C Edit History:
C      4-94        CRI    MSTP S/W CONVERSION (VICAR PORTING)
C------------------------------------------------------------------


      SUBROUTINE PU75(U,N)

      REAL*4 U(N,2,103)
      CHARACTER*132 RBUF
      CHARACTER*132 QBUF
      CHARACTER*132 PBUF

C  U IS REAL*4 IF N=1, REAL*8 IF N=2
C
      J = 1
C
      DO II = 1, 5
         PBUF(1:132) = ' '
         QBUF(1:132) = ' '
         RBUF(1:132) = ' '
         K = 7
C
         DO I = 1, 11
            WRITE (RBUF(K-2-2:K-2),'(I3)') J
            WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
            WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
            J = J + 1
            K = K + 12
         end do
C
         CALL XVMESSAGE(RBUF(2:132),' ')
         CALL XVMESSAGE(PBUF(2:132),' ')
         CALL XVMESSAGE(QBUF(2:132),' ')
         CALL XVMESSAGE(' ',' ')
         IF (II .EQ. 5) RETURN
         PBUF(1:132) = ' '
         QBUF(1:132) = ' '
         RBUF(1:132) = ' '
         K = 7
C
         DO I = 1, 12
            WRITE (RBUF(K-2-2:K-2),'(I3)') J
            WRITE (PBUF(K-5:K),'(F6.1)') U(1,1,J)
            WRITE (QBUF(K-5:K),'(F6.1)') U(1,2,J)
            J = J + 1
            IF (I .EQ. 1) K = K - 6
            IF (I .EQ. 11) K = K - 6
            K = K + 12
         END DO
C
         CALL XVMESSAGE(RBUF(2:132),' ')
         CALL XVMESSAGE(PBUF(2:132),' ')
         CALL XVMESSAGE(QBUF(2:132),' ')
         CALL XVMESSAGE(' ',' ')
      END DO
C
      return
      END
