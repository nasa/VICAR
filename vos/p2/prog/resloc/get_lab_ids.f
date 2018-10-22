c**************************************************************
c Get image identifiers from Voyager label...

      subroutine get_lab_ids(lab,ids)
      implicit none
c Input...
      character*7200 lab	!Voyager image label
c Output...
      integer*4 ids(5)	!fds,cam,filter,scet year,scet day

c     ...local variables

      integer*4 fds		!frame number (VGR FDS)
      integer*4 cam		!camera serial number (4,5,6,7)
      integer*4 filter          !filter position (0-7)
      integer*4 gain            !0=low gain, 1=high gain
      integer*4 scan            !scan rate: 1, 2, 3, 5, 10
      integer*4 mode            !operating mode
      real*4 t                  !exposure time in milliseconds
      integer poe		!planet of encounter
      character*10 picno	!0548J1-001
      integer par(2),count,def,ind
      character*80 msg
  101 format('Frame=',i7,' Camera=',i1)

      call vgrlab_fds(lab,fds)
      call xvparm('frame',par,count,def,1)
      if (def.eq.0) fds=par(1)

      call vgrlab_cam(lab,cam)
      call xvparm('camera',par,count,def,1)
      if (def.eq.0) cam=par(1)

      call vgrlab_camparams(lab,filter,gain,scan,mode,t)
c     ...only filter is used.

      call vgrlab_scet(lab,par,ind)
      if (ind.eq.-1) call xvmessage( '***invalid SCET',0)

      ids(1) = fds	!7-digit VGR FDS
      ids(2) = cam		!camera serial number (4-7)
      ids(3) = filter		!filter position (0-7)
      ids(4) = par(1)		!SCET year
      ids(5) = par(2)		!SCET day

      call vgrlab_picno(lab,picno,ind)
      if (ind.eq.-1) then
         call xvmessage('***PICNO not found',' ')
         poe = 5		!pick the most benign planet
      else
         if (picno(5:5).eq.'J') then
            poe = 5
         elseif (picno(5:5).eq.'S') then
            poe = 6
         elseif (picno(5:5).eq.'U') then
            poe = 7
         elseif (picno(5:5).eq.'N') then
            poe = 8
         else
            call xvmessage('***error determining POE',0)
            poe = 5
         endif
      endif

      write(msg,101) fds,cam
      call xvmessage(msg,' ')
      if (fds.eq.-999) call mabend('***invalid FDS count')
      if (cam.eq.-999) call mabend('***invalid camera')
      return
      end
