      PROGRAM TLGEOM
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
      IMPLICIT NONE
      INTEGER UNIT,STAT
      INTEGER IBUF(250)
      REAL RBUF(250)
      EQUIVALENCE (IBUF,RBUF)
      IBUF(1) = 21
      CALL MVCL('SAMPLE  ',IBUF(2), 8)
      IBUF(4) = 1
      IBUF(5) = 20
      CALL MVCL('LINE    ',IBUF(6), 8)
      RBUF(8) = 1.0
      RBUF(9) = 0
      RBUF(10) = 0
      RBUF(11) = -10
      RBUF(12) = 0
      CALL MVCL('LINE    ',IBUF(13), 8)
      RBUF(15) = 20
      RBUF(16) = 0
      RBUF(17) = -10
      RBUF(18) = -10
      RBUF(19) = -10
      CALL MVCL('NOIN    ',IBUF(20), 8)
      CALL XVUNIT(UNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(UNIT,STAT,'OP','WRITE','U_NL',1,'U_NS',4*21,' ')
      CALL XVWRIT(UNIT,IBUF,STAT,' ')
      CALL XVCLOSE(UNIT,STAT,' ')
      RETURN
      END
