C********************************************************
C
C   TEST PROGRAM FOR FORTRAN AND C CALLABLE SUBROUTINE
C                       GETPROJ
C
C********************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44

      implicit integer*4 (a-z)
      character*5 project

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')

      call xvmessage('******fortran callable*******  ',' ')

      call xvmessage('GETPROJ:',' ')
      call getproj(unit,project,camera,fds,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call xvmessage('project=     '//project,' ')
      call prnt(4,1,camera,'camera serial number=.')
      call prnt(4,1,fds,   'frame number=        .')

      call xvmessage('*********c callable**********  ',' ')

      call xvmessage('ZGETPROJ:',' ')
      call tzgetproj(unit)

      return
      end
