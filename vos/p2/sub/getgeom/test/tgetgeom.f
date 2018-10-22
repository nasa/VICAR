      Include 'VICMAIN_FOR' 
c
      Subroutine main44
      Implicit integer*4 (a-z)
      Character*5  Project 
      Real*4 BUF(2720)
      Real*4 MUF(2720)

      CALL Xvparm ('PROJECT', Project, ICNT, IDEF, 0)
      CALL Xvparm ('CAMERA', CAMERA, ICNT, IDEF, 0)
      Call Xvmessage(Project, ' ')
c 
      Call Prnt (4, 1, camera, 'camera serial number=.')
      Call Xvmessage(' ', ' ')
      Call Xvpcnt('INP', Count)
      If  (Count .EQ. 1) then
         Call Xvunit(unit2,'INP',1,status,' ')
         geomsor=1
       Call Xvmessage('obtaining geom parameters from input file', ' ')
      Else
         geomsor=0
        Call Xvmessage('obtaining geom parameters from nominals', ' ')
      Endif
      Call getgeom(unit2,project,camera,geomsor,BUF,MUF,
     +             nah,nav,ind)
      If(ind.ne.0) Call Xvmessage('GETGEOM: error, ind=1', ' ')

      IF (PROJECT.EQ.'GLL') THEN
         Call Prnt(99, 8, Buf(1), ' Elements 1 & 2 in BUF .')
         CALL PRNT(4,1,BUF(3),' Camera S/N=.')
         RETURN
      ENDIF

      Call Xvmessage(' First 8 Elements in BUF:  ', ' ')
      Call Prnt(4, 1, Buf(3), ' NAH = .')
      Call Prnt(99, 8, Buf(4), ' Elements 4 & 5 in BUF .')
      Call Prnt(4, 1, Buf(6), ' NAV = .')
      Call Prnt(99, 8, Buf(7), ' Elements 7 & 8 in BUF .')

      Call  Xvmessage(' First PART OF GEOMA PARAMETERS:', ' ')
      Call  Prnt (7, 32, BUF(9), '.')
      Call Xvmessage(' ', ' ')
c
      Return
      End
