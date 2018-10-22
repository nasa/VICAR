      INCLUDE 'VICMAIN_FOR'
c
C  PROGRAM TMM71B
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE MM71B.
C  MM71B PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C  NOMINAL MARINER 9 DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  MM71B RETURNS DATA FOR THE "B" CAMERA.
c
      Subroutine  Main44
      REAL*4  BUF(840)

      Call  MM71B(BUF)
      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c     CALL PRNT(0,32,BUF)
      Call Prnt(99, 8, BUF(1), ' First 2 BUFs = .')
      Call Prnt( 4, 1, BUF(3), ' Value of NAH = .')
      Call Prnt(99, 8, BUF(4), ' Next  2 BUFs = .')
      Call Prnt( 4, 1, BUF(6), ' Value of NAV = .')
      Call Prnt(99, 8, BUF(7), ' Next  2 BUFs = .')
     
      CALL QPRINT(' GEOMA PARAMETERS:',18)
      CALL PRNT(7,80,BUF(81), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(161), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(241), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(321), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(401), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(481), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(561), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(641), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(721), '.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,40,BUF(801), '.')

      Return
      End  
