C--------------------------------------------------------------
C THIS IS A TEST OF MODULE GEOMVO
C 
C PORTED TO UNIX 8/25/93
C--------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c      
      INTEGER ICAM
      REAL*4 RLOC(404)
      REAL*4 CONV(2216)

      call xvmessage('**************fortran callable***********',' ')
c
c  test 1
c      
      ICAM = 4
      CALL XVMESSAGE(' ',' ')
      CALL  PRNT(4,1,ICAM,'----------GEOMA parameters for ICAM=   .')
      CALL GETRES(RLOC,ICAM)         !to generate input test values only
      CALL GEOMVO(CONV,ICAM,RLOC)
      CALL PRNT(99,4,CONV(1),'CONV(1)=    .')
      CALL PRNT(99,4,CONV(2),'CONV(2)=    .')
      CALL PRNT(4,1,CONV(3),'CONV(3)=    .')
      CALL PRNT(99,4,CONV(4),'CONV(4)=    .')
      CALL PRNT(99,4,CONV(5),'CONV(5)=    .')
      CALL PRNT(4,1,CONV(6),'CONV(6)=    .')
      CALL PRNT(99,4,CONV(7),'CONV(7)=    .')
      CALL PRNT(99,4,CONV(8),'CONV(8)=    .')
      CALL XVMESSAGE('1st 10 of tiepoints=',' ')
      CALL PRNT(7,5,CONV(9),' ')
      CALL PRNT(7,5,CONV(14),' ')
c
c  tests 2-4
c
      DO 100 ICAM=6,8
C
      CALL XVMESSAGE(' ',' ')
      CALL  PRNT(4,1,ICAM,'----------GEOMA parameters for ICAM=   .')
      CALL GETRES(RLOC,ICAM)
      CALL GEOMVO(CONV,ICAM,RLOC)
      CALL PRNT(99,4,CONV(1),'CONV(1)=    .')
      CALL PRNT(99,4,CONV(2),'CONV(2)=    .')
      CALL PRNT(4,1,CONV(3),'CONV(3)=    .')
      CALL PRNT(99,4,CONV(4),'CONV(4)=    .')
      CALL PRNT(99,4,CONV(5),'CONV(5)=    .')
      CALL PRNT(4,1,CONV(6),'CONV(6)=    .')
      CALL PRNT(99,4,CONV(7),'CONV(7)=    .')
      CALL PRNT(99,4,CONV(8),'CONV(8)=    .')
      CALL XVMESSAGE('1st 10 of tiepoints=',' ')
      CALL PRNT(7,5,CONV(9),' ')
      CALL PRNT(7,5,CONV(14),' ')
c
100   CONTINUE
C
      call xvmessage('**************C callable***********',' ')
c
c  test 5
c
      ICAM=8
      CALL XVMESSAGE(' ',' ')
      CALL  PRNT(4,1,ICAM,'----------GEOMA parameters for ICAM=   .')
      CALL GETRES(RLOC,ICAM)
      CALL tzGEOMVO(CONV,ICAM,RLOC)
      CALL PRNT(99,4,CONV(1),'CONV(1)=    .')
      CALL PRNT(99,4,CONV(2),'CONV(2)=    .')
      CALL PRNT(4,1,CONV(3),'CONV(3)=    .')
      CALL PRNT(99,4,CONV(4),'CONV(4)=    .')
      CALL PRNT(99,4,CONV(5),'CONV(5)=    .')
      CALL PRNT(4,1,CONV(6),'CONV(6)=    .')
      CALL PRNT(99,4,CONV(7),'CONV(7)=    .')
      CALL PRNT(99,4,CONV(8),'CONV(8)=    .')
      CALL XVMESSAGE('1st 10 of tiepoints=',' ')
      CALL PRNT(7,5,CONV(9),' ')
      CALL PRNT(7,5,CONV(14),' ')
      return
      end
