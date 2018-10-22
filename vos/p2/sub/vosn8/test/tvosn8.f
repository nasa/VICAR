      INCLUDE 'VICMAIN_FOR'
c
      Subroutine  Main44
c
C  PROGRAM TVOSN8
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE VOSN8.
C  VOSN8 PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C  NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  VOSN8 RETURNS DATA FOR THE CAMERA.
c
      REAL*4  BUF(800)
c
      Call  Vosn8(BUF)
c
      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c
c     CALL PRNT(7,8,BUF)
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
c
      Return
      End
