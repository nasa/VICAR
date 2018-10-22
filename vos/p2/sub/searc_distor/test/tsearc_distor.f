      INCLUDE 'VICMAIN_FOR'
C       PROGRAM TSEARC_DISTOR TO TEST SEARC_DISTOR
      SUBROUTINE MAIN44
      INTEGER*4 UNIT

      CALL XVUNIT( unit,'INP',1,ISTAT,' ')
      CALL CHKSTAT( ISTAT,' ERROR IN XVUNIT, ISTAT=', 1, ISTAT, 1)
      CALL XVOPEN(unit,ISTAT,' ')
      CALL CHKSTAT( ISTAT,' ERROR IN XVOPEN, ISTAT=', 1, ISTAT, 1)
      CALL SEARC_DISTOR(unit,ind)
      if (ind.eq.0) then
	call xvmessage(' input image is Image Space',' ')
      elseif (ind.eq.1) then
	call xvmessage(' input image is Object Space',' ')
      else
	call xvmessage(' ** error in searc_distor! **',' ')
      endif

      return
      end
