
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'
      REAL*4 LOC(2,1000)

C
      CALL IFMESSAGE('TPU75 version 01-JULY-1994')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(inunit,'INP',1,status,' ')
      CALL XVOPEN(inunit,status,' ')
      CALL XVREAD(inunit,loc,status,' ')
C          SCAN FOR SPECIAL END OF RECORD MARK
      CALL PU75(LOC,1)
      CALL XVCLOSE(inunit,status,' ')
C
      
      CALL XVMESSAGE(
     X 'Repeat test cases in C to test C interface: zpu75',' ')


      call tzpu75  

      RETURN
      END

