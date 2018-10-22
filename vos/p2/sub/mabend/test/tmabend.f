c  test subroutine MABEND
      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer (a-z)
      character*17 msg/'This is a message'/
      logical xvptst

      if (xvptst('CHAR')) then
        call MABEND( msg)
      else if (xvptst('C') .OR. xvptst('CCHAR')) then
        call tzmabend
      else
        call MABEND( 'This is also a message')
      endif


      return
      end
