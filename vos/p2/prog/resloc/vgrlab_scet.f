ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract Spacecraft-Event_Time from Voyager flight label
c
      subroutine vgrlab_scet(lab,scet,ind1)
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 scet(5)		!output SCET year,day,hour,minute,second
      integer*4 ind1

      integer*4 i

      ind1 = 0

      do i=1,5
         scet(i) = 0
      enddo

c Note: the year is two digits

      i = index(lab(1:),' SCET ')	!Example: SCET 79.063 19:23:00

      if (i.eq.0) then
         ind1 = -1
         return	!SCET not found
      endif

      if (lab(i+6:i+6).ne.'*') then
         read(lab(i+6:),'(bn,i2)') scet(1)	!year
      else
         ind1 = -1
      endif

      if (lab(i+9:i+9).ne.'*') then
         read(lab(i+9:),'(bn,i3)') scet(2)	!day
      else
         ind1 = -1
      endif

      if (lab(i+13:i+13).ne.'*') then
         read(lab(i+13:),'(bn,i2)') scet(3)	!hr
      else
         ind1 = -1
      endif

      if (lab(i+16:i+16).ne.'*') then
         read(lab(i+16:),'(bn,i2)') scet(4)	!min
      else
         ind1 = -1
      endif

      if (lab(i+19:i+19).ne.'*') then
         read(lab(i+19:),'(bn,i2)') scet(5)	!sec
      else
         ind1 = -1
      endif
      return
      end
