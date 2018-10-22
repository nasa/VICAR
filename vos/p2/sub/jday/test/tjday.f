      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      INTEGER*4  MONTH, DAY, YEAR, OUT
      INTEGER*4  ICNT, IDEF

      CALL XVMESSAGE('******  Testing FORTRAN version  ******', ' ')

      CALL XVPARM ('YEAR',  YEAR,  ICNT, IDEF, 1)
      CALL XVPARM ('MONTH', MONTH, ICNT, IDEF, 1)
      CALL XVPARM ('DAY',   DAY,   ICNT, IDEF, 1)
      CALL JDAY (MONTH, DAY, YEAR, OUT)
      CALL PRNT (4, 1, OUT, 'Day-of-Year = .')

      CALL tzjday

      RETURN
      END

