C****************************************************************
C   TPUTSPICE2: Test program for subroutine PUTSPICE2
C****************************************************************
C
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      integer*4 unit,status,ind
      integer*4 ibuf(200)
      logical provenance
      double precision dbuf(100)
      equivalence (dbuf,ibuf)
C      character*5 source
C      integer*4 count, def

      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'OPEN_ACT','SA',' ')
      provenance = .false.
C      call xvparm ('CKNAME', source, count, def, 0)

      call getspice2(unit,provenance,dbuf,ind)
      if (ind .ne. 1) then
         call prnt(4,1,ind,'***GETSPICE2 failed, IND=.')
         call abend
      endif

      call xvmessage('             ',' ')
      call xvmessage( '..................................',' ')
      call xvmessage('PUTSPICE2 test',' ')
C      call putspice2(source,'TPUTSP',dbuf,ind)
      call putspice2 ('NAV2','TPUTSP',dbuf,ind)
      if (ind.ne.1) then
         call prnt(4,1,ind,'***PUTSPICE2 failed, IND=.')
      else
         call xvmessage('PUTSPICE2 test succeeded',' ')      
      endif
CC      call xvmessage('             ',' ')
CC      call xvmessage( '..................................',' ')
CC      call xvmessage('ZPUTSPICE2 test',' ')
CC      call tzputspice2(unit)
      return
      end
