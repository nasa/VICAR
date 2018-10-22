C--------------------------------------------------------------
C THIS IS A TEST OF MODULE VOLABV2
C 
C PORTED TO UNIX 7/29/93
C--------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      character*8 msg
      INTEGER*4 BUF(40)
      call xvmessage('**************fortran callable***********',' ')
c
      CALL XVUNIT(UNIT,'INP',1,STAT,' ')

      CALL CHKSTAT(STAT,' ERR IN XVNIT, STAT=',1,STAT,1)
      CALL XVOPEN(UNIT,STAT,' ')
      CALL CHKSTAT(STAT,' ERR IN XVOPEN, STAT=',1,STAT,1)
c
      CALL VOLABV2(IND,UNIT,BUF)
c
      CALL PRNT(4,1,IND,' IND=.')
      CALL PRNT(4,1,BUF(1), ' DATA FORMAT    = .')
      CALL PRNT(4,1,BUF(2), ' CAMERA SERIAL #= .')
      CALL PRNT(4,1,BUF(3), ' SERIAL #  =      .')
      CALL PRNT(4,1,BUF(4), ' CAMERA   =       .')
      CALL PRNT(4,1,BUF(5), ' FRAME #  =       .')
      CALL PRNT(4,1,BUF(7), ' FSC #    =       .')
      CALL PRNT(4,1,BUF(8), ' FILTER POS  =    .')
      CALL PRNT(4,1,BUF(9), ' EXPOSURE    =    .')
      CALL PRNT(4,1,BUF(10),' FLOOD STATE  =   .')
      CALL PRNT(4,1,BUF(11),' GAIN STATE   =   .')
      CALL PRNT(4,1,BUF(12),' DC OFFSET STATE= .')
      CALL PRNT(4,1,BUF(13),' SCALE IN M/PIXEL=.')
      CALL PRNT(4,1,BUF(14),' FOV HEIGHT   =   .')
      CALL PRNT(4,1,BUF(15),' FOV WIDTH    =   .')
      CALL PRNT(4,1,BUF(16),' RANGE        =   .')
      msg = '        '
      CALL MVLc(BUF(17),MSG,8)
      CALL XVMESSAGE(' PICNO=       ',' ')
      CALL xvmessage(MSG,'                  ')

      CALL PRNT(4,1,BUF(19),' SCET YEAR    =   .')
      CALL PRNT(4,1,BUF(20),' SCET DAY     =   .')
      CALL PRNT(4,1,BUF(21),' SCET HOUR    =   .')
      CALL PRNT(4,1,BUF(22),' SCET MIN     =   .')
      CALL PRNT(4,1,BUF(23),' SCET SEC     =   .')
      CALL PRNT(4,1,BUF(24),' SCET MS      =   .')

      call xvmessage('**************c callable***********',' ')
c

      CALL tzVOLABV2(IND,UNIT,BUF)
c
      CALL PRNT(4,1,IND,' IND=.')
      CALL PRNT(4,1,BUF(1), ' DATA FORMAT    = .')
      CALL PRNT(4,1,BUF(2), ' CAMERA SERIAL #= .')
      CALL PRNT(4,1,BUF(3), ' SERIAL #  =      .')
      CALL PRNT(4,1,BUF(4), ' CAMERA   =       .')
      CALL PRNT(4,1,BUF(5), ' FRAME #  =       .')
      CALL PRNT(4,1,BUF(7), ' FSC #    =       .')
      CALL PRNT(4,1,BUF(8), ' FILTER POS  =    .')
      CALL PRNT(4,1,BUF(9), ' EXPOSURE    =    .')
      CALL PRNT(4,1,BUF(10),' FLOOD STATE  =   .')
      CALL PRNT(4,1,BUF(11),' GAIN STATE   =   .')
      CALL PRNT(4,1,BUF(12),' DC OFFSET STATE= .')
      CALL PRNT(4,1,BUF(13),' SCALE IN M/PIXEL=.')
      CALL PRNT(4,1,BUF(14),' FOV HEIGHT   =   .')
      CALL PRNT(4,1,BUF(15),' FOV WIDTH    =   .')
      CALL PRNT(4,1,BUF(16),' RANGE        =   .')
      msg = '        '
      CALL MVLc(BUF(17),MSG,8)
      CALL XVMESSAGE(' PICNO=       ',' ')
      CALL xvmessage(MSG,'                  ')

      CALL PRNT(4,1,BUF(19),' SCET YEAR    =   .')
      CALL PRNT(4,1,BUF(20),' SCET DAY     =   .')
      CALL PRNT(4,1,BUF(21),' SCET HOUR    =   .')
      CALL PRNT(4,1,BUF(22),' SCET MIN     =   .')
      CALL PRNT(4,1,BUF(23),' SCET SEC     =   .')
      CALL PRNT(4,1,BUF(24),' SCET MS      =   .')

      RETURN
      END
