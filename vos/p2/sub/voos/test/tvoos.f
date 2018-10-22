C--------------------------------------------------------------
C THIS IS A TEST OF MODULE VOOS
C 
C PORTED TO UNIX 8/24/93
C--------------------------------------------------------------
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
c      
      real*4 loc(2,103)
c
      call xvmessage('**************fortran callable***********',' ')
c
c  test 1
c
      call voos(4,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-1B:')
c
c  test 2
c
      call voos(6,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-2B:')
c
c  test 3
c
      call voos(7,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-1A:')
c
c  test 4
c
      call voos(8,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-2A:')
c
c  test 5 - test C version
c
      call xvmessage('**************C callable***********',' ')
c
      call tzvoos(8,loc)
      call prnt(7,206,loc,'reseau coordinates for VO-2A:')
c
c  test 6 - incorrect ICAM
c
      call xvmessage('Should call ABEND',' ')
      call  tzvoos(2,loc)
c
      return
      end
