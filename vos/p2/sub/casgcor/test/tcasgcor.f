      include 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT NONE
      CHARACTER*80 MSG
      INTEGER STATUS,MODE,I,N,ICAM
      REAL*4 IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R,ERR
      REAL*4 L(5)/1.,128.,256.,512.,1024./
      REAL*4 S(5)/1.,256.,256.,512.,1024./
  100 FORMAT('IS to OS: (',F11.6,',',F11.6,
     &       ')   (',F11.6,',',F11.6,')  R=',F10.6)
  101 FORMAT('OS to IS: (',F11.6,',',F11.6,
     &       ')   (',F11.6,',',F11.6,')  ERR=',F10.6)

      CALL XVMESSAGE('Test FORTRAN bridge to CASGCOR', ' ')

      DO 10 ICAM=2,42,20
      CALL XVMESSAGE(' ',' ')
      IF (ICAM.EQ.2) CALL XVMESSAGE('Full Frame test',' ')
      IF (ICAM.EQ.22) CALL XVMESSAGE('2x2 Summation mode test',' ')
      IF (ICAM.EQ.42) CALL XVMESSAGE('4x4 Summation mode test',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('            IS (LINE,SAMP)          OS (LINE,SAMP)
     & ',' ')
      N = 5 - (ICAM-2)/20

      DO 10 I=1,N
      IS_LINE = L(I)
      IS_SAMP = S(I)
      MODE = 1
      CALL CASGCOR(status,IS_LINE,IS_SAMP,os_line,os_samp,MODE,ICAM)
      R = SQRT((OS_LINE-IS_LINE)**2 + (OS_SAMP-IS_SAMP)**2)
      WRITE (MSG,100) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R
      CALL XVMESSAGE(MSG,' ')
      MODE = 0
      CALL CASGCOR(status,is_line,is_samp,OS_LINE,OS_SAMP,MODE,ICAM)
      ERR = SQRT((IS_LINE-L(I))**2 + (IS_SAMP-S(I))**2)
      WRITE (MSG,101) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,ERR
      CALL XVMESSAGE(MSG,' ')
   10 CALL XVMESSAGE(' ',' ')


      CALL XVMESSAGE('NAC test - should have no correction',' ')
      DO 20 ICAM=1,41,20
      CALL XVMESSAGE(' ',' ')
      IF (ICAM.EQ.1) CALL XVMESSAGE('Full Frame test',' ')
      IF (ICAM.EQ.21) CALL XVMESSAGE('2x2 Summation mode test',' ')
      IF (ICAM.EQ.41) CALL XVMESSAGE('4x4 Summation mode test',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('            IS (LINE,SAMP)          OS (LINE,SAMP)
     & ',' ')
      N = 5 - (ICAM-1)/20

      DO 20 I=1,N
      IS_LINE = L(I)
      IS_SAMP = S(I)
      MODE = 1
      CALL CASGCOR(status,IS_LINE,IS_SAMP,os_line,os_samp,MODE,ICAM)
      R = SQRT((OS_LINE-IS_LINE)**2 + (OS_SAMP-IS_SAMP)**2)
      WRITE (MSG,100) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R
      CALL XVMESSAGE(MSG,' ')
      MODE = 0
      CALL CASGCOR(status,is_line,is_samp,OS_LINE,OS_SAMP,MODE,ICAM)
      ERR = SQRT((IS_LINE-L(I))**2 + (IS_SAMP-S(I))**2)
      WRITE (MSG,101) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,ERR
      CALL XVMESSAGE(MSG,' ')
   20 CALL XVMESSAGE(' ',' ')


      CALL XVMESSAGE('Test zcasgcor', ' ')
      CALL TCCASGCOR
      RETURN
      END
