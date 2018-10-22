      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      CHARACTER*20 A
      CHARACTER*45 MS1
      INTEGER*4 I
      INTEGER*2 FLAG

C-------- TEST FOR FIRST FORMAT ----------

      CALL XVMESSAGE ('Test the FORTRAN interface',' ')
      CALL XVMESSAGE (' ',' ')

      CALL XVMESSAGE ('*** Testing for flag = 1',' ')
      FLAG = 1

      CALL DATFMT(FLAG, A, I)

      CALL PRNT(4,1,I,'    -> The integer date is .')

      MS1(1:25) = '    -> The ASCII date is '
      MS1(26:45) = A(1:20)
      CALL XVMESSAGE(MS1 , ' ')

      CALL XVMESSAGE(' ',' ')
C-------- TEST FOR SECOND FORMAT ----------

      CALL XVMESSAGE ('*** Testing for flag = 2',' ') 
      FLAG = 2
      
      CALL DATFMT(FLAG, A, I)

      CALL PRNT(4,1,I,'    -> The integer date is .')
      MS1(1:25) = '    -> The ASCII date is '
      MS1(26:45) = A(1:20)
      CALL XVMESSAGE(MS1 , ' ')
      CALL XVMESSAGE(' ',' ')

      CALL tzdatfmt

      RETURN
      END
