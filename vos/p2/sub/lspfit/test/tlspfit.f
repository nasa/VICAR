      INCLUDE 'VICMAIN_FOR' 
C VICAR PROGRAM TLSPFIT
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER N,IFIT, IMAX, IER
      REAL*4 PTS(4,100)
      REAL*8 COEF(12),MOM(36),UMOM(12)
      REAL*8 PI,DTR,RTD, DMAX, DRMS

      CALL XVMESSAGE ('*** Testing Fortran interfaces ***',' ') 
      PI = 3.141592653589793D0
      DTR = PI/180.D0
      RTD = 180.D0/PI

      PTS(1,1) = 1.
      PTS(2,1) = 1.
      PTS(1,2) = 1.
      PTS(2,2) = 400.
      PTS(1,3) = 1.
      PTS(2,3) = 800.

      PTS(1,4) = 400.
      PTS(2,4) = 1.
      PTS(1,5) = 400.
      PTS(2,5) = 400.
      PTS(1,6) = 400.
      PTS(2,6) = 800.

      PTS(1,7) = 800.
      PTS(2,7) = 1.
      PTS(1,8) = 800.
      PTS(2,8) = 400.
      PTS(1,9) = 800.
      PTS(2,9) = 800.
      N = 9		!number of tiepoints

C     ....Test with offset only
      COEF(1) = 1.
      COEF(2) = 1.
      COEF(3) = 0.
      COEF(4) = 1.
      COEF(5) = 0.
      COEF(6) = 1.
      IFIT = 1
      CALL PTRAN(IFIT,COEF,N,pts)
      CALL LFIT(IFIT,PTS,N,coef,mom,umom,drms,dmax,imax,ier)
      IF (IER .NE. 0) GOTO 999

C     ....Test 2nd order polynomial
      COEF(1) = 6.
      COEF(2) = 5.
      COEF(3) = 4.
      COEF(4) = 3.
      COEF(5) = 2.
      COEF(6) = 1.
      COEF(7) = 12.
      COEF(8) = 11.
      COEF(9) = 10.
      COEF(10) = 9.
      COEF(11) = 8.
      COEF(12) = 7.
   
      IFIT = 9
      CALL PTRAN(IFIT,COEF,N,pts)
      CALL LFIT(IFIT,PTS,N,coef,mom,umom,drms,dmax,imax,ier)
      IF (IER .NE. 0) GOTO 999
      CALL XVMESSAGE('TLSPFIT task completed',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL TZLSPFIT()  ! calling C-bridge test
      RETURN
C
  999 CALL XVMESSAGE('***TLSPFIT task cancelled',' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE PTRAN(IFIT,COEF,N,pts)
      IMPLICIT NONE

      INTEGER IFIT,N
      REAL*4 PTS(4,1)
      REAL*8 COEF(20)
      INTEGER NPOW,NU,I
      REAL*4 X,Y,U,V
      LOGICAL*1 BLANK(132)
      CHARACTER*132 MSG
      DATA BLANK/132*' '/
  100 FORMAT(4(F10.1,A1))

      NPOW = MAX0(IFIT-7,1)
      NU = (NPOW+1)*(NPOW+2)
      CALL PRNT(8,NU,COEF,'INITIAL COEF=.')
      DO 20 I=1,N
      X = PTS(1,I)
      Y = PTS(2,I)
      CALL POLYTRAN(NPOW,COEF,X,Y,U,V)
      CALL MVLC(BLANK,MSG(1:132),132)
      WRITE(MSG,100) X,' ',Y,' ',U,' ',V,' '
      CALL XVMESSAGE(MSG,' ')
      PTS(3,I) = U
   20 PTS(4,I) = V

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE LFIT(IFIT,PTS,N,coef,mom,umom,drms,dmax,imax,ier)
      IMPLICIT NONE

      INTEGER IFIT,N,IMAX,IER
      REAL*4 PTS(4,1)
      REAL*8 COEF(1),MOM(1),UMOM(1),DRMS,DMAX
      INTEGER NPOW,NU
C
      IER = -1
      NPOW = MAX0(IFIT-7,1)
      NU = (NPOW+1)*(NPOW+2)/2
      IF (NU.GT.N) RETURN

      CALL MOMENT(NPOW,PTS,N,MOM,UMOM)
      CALL MVE(8,12,0.D0,COEF,0,1)
      CALL PRNT(4,1,IFIT,'IFIT=.')
      IF (IFIT.LT.8) CALL CLFIT(IFIT,MOM,UMOM,coef,ier)
      IF (IFIT.GE.8) CALL LSPFIT(NPOW,MOM,UMOM,coef,ier)
      CALL RMSFIT (0,NPOW,PTS,N,coef,drms,dmax,imax)
      CALL PRNT(8,2*NU,COEF,'COEF=.')
      CALL PRNT(8,1,DRMS,'RMS=.')
      CALL PRNT(8,1,DMAX,'MAX ERROR=.')
      RETURN
      END
