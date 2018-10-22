      include 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT NONE
      CHARACTER*80 MSG
      INTEGER STATUS,MODE,I,N,ICAM
      REAL*4 IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R,ERR
      REAL*4 L(4)/1.,  1.,400.,800./
      REAL*4 S(4)/1.,400.,400.,800./
  100 FORMAT('IS to OS: (',F10.6,',',F10.6,
     &       ')   (',F10.6,',',F10.6,')  R=',F10.6)
  101 FORMAT('OS to IS: (',F10.6,',',F10.6,
     &       ')   (',F10.6,',',F10.6,')  ERR=',F10.6)

      CALL XVMESSAGE('Test FORTRAN bridge to GLLGCOR', ' ')

      DO 10 ICAM=1,2
      CALL XVMESSAGE(' ',' ')
      IF (ICAM.EQ.1) CALL XVMESSAGE('Full Frame test',' ')
      IF (ICAM.EQ.2) CALL XVMESSAGE('Summation mode test',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('            IS (LINE,SAMP)          OS (LINE,SAMP)
     & ',' ')
      N = 5 - ICAM

      DO 10 I=1,N
      IS_LINE = L(I)
      IS_SAMP = S(I)
      MODE = 1
      CALL GLLGCOR(status,IS_LINE,IS_SAMP,os_line,os_samp,MODE,ICAM)
      R = SQRT((OS_LINE-IS_LINE)**2 + (OS_SAMP-IS_SAMP)**2)
      WRITE (MSG,100) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R
      CALL XVMESSAGE(MSG,' ')
      MODE = 0
      CALL GLLGCOR(status,is_line,is_samp,OS_LINE,OS_SAMP,MODE,ICAM)
      ERR = SQRT((IS_LINE-L(I))**2 + (IS_SAMP-S(I))**2)
      WRITE (MSG,101) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,ERR
      CALL XVMESSAGE(MSG,' ')
   10 CALL XVMESSAGE(' ',' ')

      CALL XVMESSAGE('Test zgllgcor', ' ')
      CALL TCGLLGCOR
      RETURN
      END
