c**************************************************************
c Get the planet-of-encounter from the Voyager label...

      subroutine get_poe(lab,poe)
      implicit none
c Input...
      character*7200 lab	!Voyager image label
c Output...
      integer*4 poe		!planet of encounter

c     ...local variables
      integer ind
      character*10 picno	!0548J1-001
      logical xvptst

      poe = 0
      if (xvptst('jupiter')) poe=5
      if (xvptst('saturn')) poe=6
      if (xvptst('uranus')) poe=7
      if (xvptst('neptune')) poe=8
      if (poe.gt.0) goto 10

      call vgrlab_picno(lab,picno,ind)
      if (ind.ne.-1) then
         if (picno(5:5).eq.'J') poe=5
         if (picno(5:5).eq.'S') poe=6
         if (picno(5:5).eq.'U') poe=7
         if (picno(5:5).eq.'N') poe=8
      endif
      if (poe.eq.0) poe=8

   10 continue
      if (poe.eq.5) call xvmessage('Planet-of-encounter is Jupiter',0)
      if (poe.eq.6) call xvmessage('Planet-of-encounter is Saturn',0)
      if (poe.eq.7) call xvmessage('Planet-of-encounter is Uranus',0)
      if (poe.eq.8) call xvmessage('Planet-of-encounter is Neptune',0)
      return
      end
