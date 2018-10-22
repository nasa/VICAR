c*************************************************************
c
c program to test FORTRAN and C callable versions of GETCAMCON
c
c*************************************************************
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)

      REAL*4      FOCAL,OAL,OAS,SCALE
      CHARACTER*5 Project
      INTEGER     Camera, FileUnit, Ind, Fds

      CALL xvunit(FileUnit, 'INP', 1, Ind,' ')
      CALL xvopen(FileUnit, Ind,'OPEN_ACT','SA',' ')

      call xvmessage('************FORTRAN CALLABLE************',' ')
      CALL getproj(FileUnit, Project, Camera, Fds, Ind)

      call xvmessage(project,' ')
      call getcamcon(project,camera,focal,oal,oas,scale,ind)
      if(ind.ne.0) call prnt(4,1,ind,'fatal indicator=.')
      call prnt(7,1,focal,'focal length=       .')
      call prnt(7,1,oal,  'optical axis line=  .')
      call prnt(7,1,oas,  'optical axis sample=.')
      call prnt(7,1,scale,'image scale=        .')
      call xvmessage('  ',' ')

      call xvmessage('************C- CALLABLE************',' ')
      call tzgetcamcon(FileUnit)

      CALL XVCLOSE(FileUnit, Ind, ' ')
      RETURN
      END
