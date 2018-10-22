      INCLUDE 'VICMAIN_FOR'
c
C Test program for subroutine ABLE86
C
      Subroutine  MAIN44
      Integer  BUF(83),UNIT

      Call  Xvmessage(' ', ' ')
      Call  Xvmessage(' ******  Testing FORTRAN Version  ******',' ')

      Call  XVUNIT(UNIT,'INP',1,ISTAT,' ')
      Call  XVOPEN(UNIT,ISTAT,' ')
      If (ISTAT.NE.1) Then
         Call Xvmessage(' *** Call NOT Open INPUT', ' ')
         Call Abend
      EndIF
c
      CALL XVP('NPAR',NPAR,ICNT)
      BUF(1) = NPAR
      Call  ABLE86(IND,UNIT,BUF)
      Call  PRNT(4,1,BUF(1),' LABEL TYPE=.')
      IF (BUF(1) .EQ. 1) Call Xvmessage(' *** GROUND CALIB DATA', ' ')
      IF (BUF(1) .EQ. 2) Call Xvmessage(' *** PHASE I DATA', ' ')
      IF (BUF(1) .EQ. 3) Call Xvmessage(' *** PHASE II DATA', ' ')
      Call  PRNT(4,1,BUF(2),' FRAME NO=.')
      Call  PRNT(7,1,BUF(3),' EXPOSURE=.')     !  Floating point 
      Call  PRNT(4,1,BUF(4),' FILTER POSITION=.')
      Call  PRNT(4,1,BUF(5),' FRAME RATE=.')
      Call  PRNT(4,1,BUF(6),' FIBE/MOFIBE=.')
      Call  PRNT(4,1,BUF(7), ' BOOM FLAG =.')
      Call  PRNT(4,1,BUF(8), ' GAIN =.')
      Call  PRNT(4,1,BUF(9), ' MOD10 =.')
      Call  PRNT(4,1,BUF(10),' EVENT YEAR =.')
      Call  PRNT(4,1,BUF(11),' EVENT DAY =.')
      Call  PRNT(4,1,BUF(12),' EVENT HOUR =.')
      Call  PRNT(4,1,BUF(13),' EVENT MINUTE =.')
      Call  PRNT(4,1,BUF(14),' EVENT SECOND =.')
      Call  PRNT(4,1,BUF(15),' EVENT MSEC =.')
      Call  PRNT(4,1,BUF(16),' PARTITION =.')
      Call  PRNT(99,12,BUF(17),' TARGET =.')
      Call  PRNT(7,1,BUF(20),' IOF =.')
      Call  PRNT(7,1,BUF(21),' CONV=.')
      Call  PRNT(7,1,BUF(22),' SORANGE =.')
      Call  PRNT(99,20,BUF(23),' DARK CURRENT FILE=.')
      Call  PRNT(99,20,BUF(28),' RADIOMETRIC FILE=.')
      Call  PRNT(99,20,BUF(33),' BLEMISH FILE=.')
      Call  PRNT(99,20,BUF(38),' SHUTTER-OFFSET FILE=.')
      Call  PRNT(99,8,BUF(43),' EDR TAPE =.')
      Call  PRNT(4,1,BUF(45),' EDR FILE =.')
      Call  PRNT(4,1,BUF(46),' UBWC =.')
      Call  PRNT(99,7,BUF(47),' PICNO=.')
      Call  PRNT(4,1,BUF(49),' SEQNO =.')
      Call  PRNT(7,1,BUF(50),' ENTROPY=.')
      IF (NPAR.GT.50) THEN
         Call  PRNT(99,32,BUF(51),' DARK CURRENT DIRECTORY=.')
         Call  PRNT(99,32,BUF(59),' RADIOMETRIC DIRECTORY=.')
         Call  PRNT(99,32,BUF(67),' BLEMISH DIRECTORY=.')
         Call  PRNT(99,32,BUF(75),' SHUTTER-OFFSET DIRECTORY=.')
         Call  PRNT(4,1,BUF(83),' READOUTMODE=.')
      ENDIF
      Call  PRNT(4,1,IND,'IND =.')
c
c    Testing the C-Bridge
c
      Call XVCLOSE(unit, istat, ' ')
      Call tzable86(NPAR)
      Return
      End
