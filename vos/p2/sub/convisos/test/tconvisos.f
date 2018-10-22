      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER ICAM,MODE,NPV,NPH,IND,IPTR
      REAL IS_LINE, IS_SAMP, OS_LINE, OS_SAMP
      REAL RES(2,202),CONV(2216)
      CHARACTER*5 PROJECT

C  TEST1 FOR convisos
      CALL XVMESSAGE('TEST1 FOR convisos', ' ')
      PROJECT = 'VGR-2'
      ICAM = 7
      CALL GETRES(res,ICAM)
      CALL GEOMAV(conv,ICAM,RES)
      NPH = 24
      NPV = 23
      IND = 0
      IPTR=0
      MODE = 1
      IS_LINE = 400.
      IS_SAMP = 400.
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')
      CALL CONVISOS(PROJECT,ICAM,is_line,is_samp,os_line,os_samp,
     +                                  MODE,CONV(9),NPH,NPV,ind)
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')

C  TEST2 FOR convisos

      CALL XVMESSAGE('TEST2 FOR convisos', ' ')
      OS_LINE = 498.56
      OS_SAMP = 498.59
      MODE = 0
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(9),nph,npv,ind)
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')

C  TEST3 FOR convisos

      CALL XVMESSAGE('TEST3 FOR convisos', ' ')
      IND = 0
      PROJECT = 'GLL'
      ICAM = 1
      MODE = 1
      IS_LINE = -100.
      IS_SAMP =    1.
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')

      CALL XVMESSAGE('TEST4 FOR convisos', ' ')
      MODE = 0
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')

C  TEST5 FOR convisos

      CALL XVMESSAGE('TEST5 FOR convisos', ' ')
      IND = 0
      PROJECT = 'CAS'
      ICAM = 2 
      MODE = 1
      IS_LINE =    1.
      IS_SAMP =    1.
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')

      CALL XVMESSAGE('TEST6 FOR convisos', ' ')
      MODE = 0
      CALL PRNT(7,1,OS_LINE,' OS_LINE =.')
      CALL PRNT(7,1,OS_SAMP,' OS_SAMP =.')
      CALL convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +                                  mode,conv(10),nph,npv,ind)
      CALL PRNT(7,1,IS_LINE,'IS_LINE =.')
      CALL PRNT(7,1,IS_SAMP,'IS_SAMP =.')


      CALL XVMESSAGE(
     . 'Repeat TEST1 in C to test C interface: zconvisos', ' ')

      call tzconvisos(CONV(9))

      return
      END
