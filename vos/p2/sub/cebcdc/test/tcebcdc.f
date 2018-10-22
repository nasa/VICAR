C   
C   This is the test program for the subroutine CEBCDC
C
C
      include 'VICMAIN_FOR'
      subroutine main44
      BYTE BUF(11)
      INTEGER N
      N = 11
      DATA BUF /'E3'X,'C5'X,'E2'X,'E3'X,'7A'X,'E2'X,'E3'X,
     +'D9'X,'C9'X,'D5'X,'C7'X/
      CALL PRNT (0,N,BUF,'  START =  .')
      CALL CEBCDC(BUF,N)
      CALL PRNT (0,N,BUF,'  RESULT = .')
      call tzcebcdc  ! test the "C" interface
      return
      end
C       
