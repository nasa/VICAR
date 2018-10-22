      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

C*********************OLDGEOMA2IBIS**************************
C     JUL 95   SP  INITIAL RELEASE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE

      INTEGER COLCOUNT, STATUS, UNIT, PS, NSAMP
      INTEGER NAH, NAV, NTIEP, IP, PARMFIL, PNL, TPTR, STAT,J, NPAR
      INTEGER IPAR (500000)
      CHARACTER*4 cmp

C==================================================================

      CALL IFMESSAGE('OLDGEOMA2IBIS version 24-JUL-95')

C..READ OLD GEOMA FORMAT

      CALL XVUNIT(PARMFIL,'INP',1,STAT,' ')
      CALL XVOPEN(PARMFIL,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(PARMFIL,STAT,'NS',NSAMP, 'PIX_SIZE',PS, ' ')
      IF (PS .NE. 4  .OR.  NSAMP .NE. 900)
     .   CALL MABEND('ERROR: INPUT NOT IN OLD GEOMA FORMAT')

      CALL XVREAD(PARMFIL,IPAR,STAT,' ')
      PNL = (IPAR(1)-11) / 900
      DO J = 1, PNL
        CALL XVREAD(PARMFIL,IPAR(J*900+1),STAT,' ')
      END DO
      CALL XVCLOSE(PARMFIL,STAT,' ')

      NPAR = IPAR(1) + 1
      IP = 2
    1 IF (IP .GT. NPAR) GO TO 20
      CALL MVLC(IPAR(IP), cmp, 4)
      IF (cmp .EQ. 'NAH ') THEN
         NAH = IPAR(IP+2)
         IP = IP + 3
      ELSE IF (cmp .EQ. 'NAV ') THEN
         NAV = IPAR(IP+2)
         IP = IP + 3
      ELSE IF (cmp .EQ. 'HALF') THEN
         IP = IP + 2      !NO LONGER NEEDED - FORMAT IS IN LABEL
      ELSE IF (cmp .EQ. 'TIEP') THEN
         TPTR = IP + 2
         NTIEP = 4 * (NAH+1) * (NAV+1)
         IP = IP + NTIEP + 2
         IF (IP .GT. NPAR+1) GO TO 999
      ELSE
         GO TO 996
      END IF

      GO TO 1

20    CONTINUE

C..WRITE IN IBIS-2 FORMAT

      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
      CALL XVSIGNAL(UNIT, STATUS, 1)     ! ABORT IF ERROR

      COLCOUNT = 4
      CALL IWRITE_TIEPOINTS(UNIT,NAH,NAV,0,IPAR(TPTR),COLCOUNT)  !WRITE FILE.

      RETURN

C     End of parameter processing
  996 CALL XVMESSAGE ('*** PARAMETER ERROR. ABEND.',' ')
      CALL ABEND
  999 CALL XVMESSAGE ('*** ERROR IN NAH, NAV, OR TIEPOINT.',' ')
      CALL XVMESSAGE ('*** ABEND.',' ')
      CALL ABEND
      END
