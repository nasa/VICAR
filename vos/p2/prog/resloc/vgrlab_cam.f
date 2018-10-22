cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract camera serial number from Voyager flight label.

      subroutine vgrlab_cam(lab,cam)
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 cam		!output camera serial number (4-7)
      
      integer*4 i
      integer*4 sc_id,camera_id
      integer*4 camera_sn(2,2)/7,6,	!VGR1 NA,WA
     &			       5,4/	!VGR2 NA,WA

      call vgrlab_sc_id(lab,sc_id)
      call vgrlab_camera_id(lab,camera_id)

      if (sc_id.eq.-999 .or. camera_id.eq.-999) then
         cam = -999
      else
         cam = camera_sn(camera_id,sc_id)
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract spacecraft ID from Voyager flight label.

      subroutine vgrlab_sc_id(lab,sc_id)
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 sc_id		! 1=VGR-1, 2=VGR-2
      
      integer*4 i

      sc_id = -999

      i = index(lab(1:),'VGR-1')
      if (i.ne.0) then
         sc_id = 1		!Voyager 1
      else
         i = index(lab(1:),'VGR-2')
         if (i.ne.0) sc_id=2
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract camera id from Voyager flight label.

      subroutine vgrlab_camera_id(lab,camera_id) 
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 camera_id	! 1=NA, 2=WA
      
      integer*4 i

      camera_id = -999

      i = index(lab(1:),'NA CAMERA')
      if (i.ne.0) then
         camera_id = 1		!narrow angle
      else
         i = index(lab(1:),'WA CAMERA')
         if (i.ne.0) camera_id=2 
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract FDS count from Voyager flight label.
c
      subroutine vgrlab_fds(lab,fds)
      implicit none
      character*7200 lab	!Voyager flight label (input)
      integer*4 fds		!FDS count (output)
      
      integer*4 i,mod16,mod60

c note: the FDS is returned as an integer as 100*mod16 + mod60
c Example: FDS 16368.32 is returened as 1636832

       i = index(lab(1:),' FDS ')
       if (i.ne.0 .and. lab(i+5:i+5).ne.'*') then
          read(lab(i+5:),'(bn,i5)') mod16
          read(lab(i+11:),'(bn,i2)') mod60
          fds = 100*mod16 + mod60
       else
          fds = -999
       endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract filter, gain, scan rate, operating mode, and exposure time
c from Voyager flight label.

      subroutine vgrlab_camparams(lab,filter,gain,scan,mode,t)
      implicit none
c     ...input argument
      character*7200 lab	!Voyager flight label
c     ...output arguments
      integer*4 filter		!filter position (0-7)
      integer*4 gain		!0=low gain, 1=high gain
      integer*4 scan		!scan rate: 1, 2, 3, 5, 10
      integer*4 mode		!operating mode
      real*4 t			!exposure time in milliseconds
      
      integer*4 i

      filter = -999
      gain = -999
      scan = -999
      mode = -999
      t = -999.

      i = index(lab(1:),' FILT ')	!example: FILT 0(CLEAR)
      if (i.ne.0 .and. lab(i+6:i+6).ne.'*') 
     &	    read(lab(i+6:),'(bn,i1)') filter

      if (index(lab(1:),' LO GAIN').gt.0) then
         gain = 0
      elseif (index(lab(1:),' HI GAIN ').gt.0) then
         gain = 1
      endif

      i = index(lab(1:),'  SCAN RATE')	!example: SCAN RATE 10:1
      if (i.ne.0 .and. lab(i+12:i+12) .ne. '*') 
     & 	   read(lab(i+12:),'(bn,i2)') scan

      i = index(lab(1:),' MODE')	!example: MODE 4(BOTALT)
      if (i.ne.0 .and. lab(i+6:i+6).ne.'*')
     &	   read (lab(i+6:),'(bn,i1)') mode

      i = index(lab(1:),' EXP ')	!example: EXP 00360.0 MSEC
      if (i.ne.0 .and. lab(i+5:i+5).ne.'*') 
     &	   read(lab(i+5:),'(bn,f7.0)') t
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract PICNO from Voyager flight label.
c
      subroutine vgrlab_picno(lab,picno,ind1)
      implicit none
      character*7200 lab	!Voyager flight label (input)
      character*10 picno	!picno='xxxxPS+DDD' (output)
      integer*4 ind1
      
      integer*4 i

      i = index(lab(1:),' PICNO ')	!Example: PICNO 0548J1-001
      if (i.gt.0 .and. lab(i+7:i+7).ne.'X'
     +          .and. lab(i+7:i+7).ne.'*') then
         picno = lab(i+7:i+16)
      else
         ind1 = -1
      endif
      return
      end
