      INCLUDE 'VICMAIN_FOR'
c
      Subroutine MAIN44
c
C  PROGRAM TMVM73B
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE MVM73B.
C  MVM73B PROVIDES THE CALLING PROGRAM A BUFFER CONTAINING
C  NOMINAL MVM DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  MVM73B RETURNS DATA FOR THE "B" CAMERA.
c
      REAL*4  BUF(840)
c
      CALL MVM73B(BUF)
c
c      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c      CALL PRNT(0,32,BUF)
c
      Call Prnt(99, 8, BUF(1), ' FIRST 2 BUF = .')
      Call Prnt( 4, 1, BUF(3), ' Value of NAH = .')
      Call Prnt(99, 8, BUF(4), ' NEXT  2 BUF = .')
      Call Prnt( 4, 1, BUF(6), ' Value of NAV = .')
      Call Prnt(99, 8, BUF(7), ' NEXT  2 BUF = .')
c
      CALL QPRINT(' GEOMA PARAMETERS:',18)
      CALL PRNT(7,80,BUF(81),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(161),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(241),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(321),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(401),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(481),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(561),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(641),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(721),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,40,BUF(801),'.')
c
      Return
      End
