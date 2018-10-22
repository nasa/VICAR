      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      DOUBLE PRECISION X(2,2), QR(5,3)
      INTEGER I, M, N, IP, J, IFAIL, IPIV(2)
      DOUBLE PRECISION A(3,2) ! Presets: / 1.1,1.2,1.0,  0.9,1.0,1.0 /
      DOUBLE PRECISION B(3,2) ! Presets: / 2.2,2.3,2.1,  4.4,4.6,4.2 /
      CHARACTER*132 TBUF

      DATA A / 1.1,1.2,1.0,  0.9,1.0,1.0/
      DATA B / 2.2,2.3,2.1,  4.4,4.6,4.2  /
 
C  1.1  0.9  2.2
C  1.2  1.0  2.3
C  1.0  1.0  2.1

      M = 3
      N = 2
      IP = 2
      IFAIL = 1


! Call DLLSQ to calculat the least squares solution for test values
      CALL DLLSQ(A,B,M,N,IP,X,IPIV,1.D-15,IFAIL,QR)

! If a failure was detected then print contents of array and terminate
      IF (IFAIL.EQ.0) GO TO 20
      WRITE (TBUF,99996) IFAIL
99996 FORMAT ('ERROR IN DLLSQ IFAIL = ', I2)
      CALL XVMESSAGE (TBUF, ' ') 
      RETURN

! DLLSQ was successful ... print results of least squares computatin 
   20 WRITE (TBUF,90010)
90010 FORMAT ('SOLUTIONS')
      CALL XVMESSAGE (TBUF, ' ') 

      DO 40 J = 1, IP
      DO 40 I = 1, N
      WRITE (TBUF,99920) X(I,J)
99920 FORMAT (F7.4)
      CALL XVMESSAGE (TBUF, ' ') 
40    END DO

C     Test DLLSQ error processing 
      WRITE (TBUF, 90100)
90100 FORMAT ('TEST ERROR HANDLING. SHOULD GET IER=1')
      CALL XVMESSAGE (TBUF, ' ') 
 
C     Set first 24 (6*4) bytes of memory occupied by matrix A to zero
      CALL ZIA(A,6)

      CALL DLLSQ(A,B,M,N,IP,X,IPIV,1.D-15,IFAIL,QR)
      WRITE (TBUF,99996) IFAIL
      CALL XVMESSAGE (TBUF, ' ') 

! Initiate Call to the C bridge for DLLSQ and the DLLSQ bridge test driver
      CALL tzdllsq

      RETURN
      END
