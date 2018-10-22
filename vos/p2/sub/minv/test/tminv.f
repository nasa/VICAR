      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      REAL*4 A(4,4), C(4,4)
      REAL*4 B(4), Z(4), D
      INTEGER*4 N

C     Initialize an array with a determinant of 1

      DATA A/5,7,6,5,  7,10,8,7,  6,8,10,9,  5,7,9,10/

C       INITIALIZE AN ARRAY TO COMPARE WITH THE 'C' VERSION

      C(1,1) = 6
      C(1,2) = 4
      C(1,3) = 6
      C(1,4) = 5

      C(2,1) = 7
      C(2,2) = 2
      C(2,3) = 3
      C(2,4) = 4

      C(3,1) = 4
      C(3,2) = 2
      C(3,3) = 1
      C(3,4) = 5

      C(4,1) = 6
      C(4,2) = 6
      C(4,3) = 6
      C(4,4) = 6
 
      N = 4
      CALL XVMESSAGE('FORTRAN TEST',' ')
      CALL XVMESSAGE('MATRIX WITH DETERMINANT = 1',' ')
      CALL TMINVMAT(A,N)
      CALL MINV(A,N,D,B,Z)
      CALL TMINVPRT (A,N,D)

      CALL XVMESSAGE('NOW INVERT BACK TO ORIGINAL',' ')
      CALL MINV(A,N,D,B,Z)
      CALL TMINVPRT (A,N,D)

      CALL XVMESSAGE('MATRIX FOR C BRIDGE COMPARE',' ')
      CALL TMINVMAT(C,N)
      CALL MINV(C,N,D,B,Z)
      CALL TMINVPRT (C,N,D)

      CALL XVMESSAGE('NOW INVERT BACK TO ORIGINAL',' ')
      CALL MINV(C,N,D,B,Z)
      CALL TMINVPRT (C,N,D)

      CALL XVMESSAGE('TEST THE C INTERFACE zminv',' ')
      CALL TZMINV

      RETURN
      END

C*******************************************************************C
C  THIS SUBROUTINE IS USED TO PRINT THE RESULTS OF THE TESTS.       C
C*******************************************************************C

      SUBROUTINE TMINVPRT (A,N,D)

      REAL*4 A(4,4), D
      INTEGER*4 N
      INTEGER*2 I, J
      CHARACTER*80 MSG

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('LOWER TRIANGLE OF INVERSE',' ')

      DO 340 I=1,N
         WRITE (MSG,99994) (A(I,J),J=1,I)
         CALL XVMESSAGE(MSG,' ')
  340 CONTINUE

      CALL XVMESSAGE(' ',' ')
      WRITE (MSG,99993) D
      CALL XVMESSAGE(MSG,' ')

99994 FORMAT (' ', 4F10.6)
99993 FORMAT ('VALUE OF DETERMINANT = ',F14.8)
      END
C*******************************************************************C
C  THIS SUBROUTINE IS USED TO PRINT THE MATRIX.			    C
C*******************************************************************C

      SUBROUTINE TMINVMAT (A,N)

      REAL*4 A(4,4)
      INTEGER*4 N
      INTEGER*2 I, J
      CHARACTER*80 MSG

      DO 540 I=1,N
         WRITE (MSG,99994) (A(I,J),J=1,N)
         CALL XVMESSAGE(MSG,' ')
  540 CONTINUE
      CALL XVMESSAGE(' ',' ')

99994 FORMAT (' ', 4F10.6)
      END
