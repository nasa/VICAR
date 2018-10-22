C****************************************************************
C   TGETSPICE2: Test program for subroutine GETSPICE2
C****************************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      integer*4 unit,status,i,ind
      integer*4 ibuf(200),jbuf(200)
      logical provenance
      character*5 project
      character*12 target
      integer*4 camera_sn,sclk,data(100),scet(6)
      double precision dbuf(100)
      equivalence (dbuf,ibuf)

      do i=1,200
         ibuf(i) = 0
         jbuf(i) = 0
      enddo

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
c     ....Testing getspice2
      provenance = .true.
      call getspice2(unit,provenance,dbuf,status)
      if (status .ne. 1) then
         call xvmessage('***GETSPICE2 FORTRAN test failed',' ')
         goto 100
      endif
c     ....Testing getspice3
      call getproj(unit,project,camera_sn,sclk,ind)
      call getlabcon(unit,project,data,ind)
      call mve(4,6,data(8),scet,1,1)
      call mvlc(data(25),target,12)
      call getspice3(project,target,camera_sn,sclk,scet,provenance,
     &		jbuf,status)
      if (status .ne. 1) then
         call xvmessage('***GETSPICE3 FORTRAN test failed',' ')
         goto 100
      endif
      call xvmessage('Comparing GETSPICE2 and GETSPICE3 output.',' ')
      call compare_bufs(ibuf,jbuf)

C     ....Testing getspice4
      call getspice4(project,provenance,data,jbuf,status)
      if (status .ne. 1) then
         call xvmessage('***GETSPICE4 FORTRAN test failed',' ')
         goto 100
      endif
      call xvmessage('Comparing GETSPICE2 and GETSPICE4 output.',' ')
      call compare_bufs(ibuf,jbuf)


      call xvmessage('GETSPICE2 FORTRAN test succeeded',' ')
      call xvmessage('             ',' ')
      call xvmessage('GETSPICE2 C-bridge test',' ')
  100 call tzgetspice2(unit,jbuf,data)

      call xvmessage('Comparing GETSPICE2 and zgetspice2 output.',' ')
      call compare_bufs(ibuf,jbuf)
      return
      end
ccccccccccccccccccccccccccccccccccccccccc
c Compare buffers.
c
      subroutine compare_bufs(ibuf,jbuf)
      integer*4 ibuf(200),jbuf(200)
      integer*4 i

      do i=1,200
         if (ibuf(i).ne.jbuf(i)) 
     &      call prnt(4,1,i,' ***buffers do not match, i=.')
      enddo
      return
      end
