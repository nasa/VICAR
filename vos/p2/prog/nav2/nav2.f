      include 'VICMAIN_FOR'

c nav2 - image navigation program, part 2
c this part encompasses all algorithms which require 2 images.
c user's guide is in NAV2.PDF
c the file nav2.doc contains information useful for program maintenance.
c
      subroutine main44
      IMPLICIT NONE

      include 'cmap.fin'
      include 'const.fin'
      include 'cpic.fin'
      include 'cpts.fin'

      common/ipic2/nl(2),ns(2)
      integer nl,ns
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/cp5/debug,ni,no,navmode,ifit
      integer debug, ni, no, navmode, ifit
      common/cp9/icorr,iproject,nlw,nsw,ihpf,iphase,interp,nw,cthresh
      integer icorr,iproject,nlw,nsw,ihpf,iphase,interp,nw
      real*8 cthresh

      character*5 format
      integer i, ind, nl1, ns1, nl2, ns2
      logical xvptst
      external navmain

      call xvmessage(' NAV2 Version 2016-02-18',' ')
      call init_spice
      pi = 3.141592653589793d0
      dtr = pi/180.d0
      rtd = 180.d0/pi
      planet_id = 0
      target_id = 0
c             default parameter values...
      debug = 0
      navmode = 0	!0=no wind adjustment
      ifit = 3		!2=offsets only, 3=solve for north angle
      icorr = 1
      iproject = 1
      ihpf = 1
      iphase = 0
      interp = 1
      cthresh = 0.2
      nlw = 64
      nsw = 64
      nw = 10
      model = 1

      if (xvptst('DEBUG',debug,' ')) debug=1

      call xvpcnt('INP',ni, ' ')		!get number of input files
      call xvpcnt('OUT',no, ' ')		!get number of output files
c           open input pictures...
      do i=1,2
          call xvunit(img(i),'INP',i,ind,' ')
          call xvopen(img(i),ind,'OPEN_ACT','SA','IO_ACT','SA',' ')
          call xvget(img(i),ind,'FORMAT',format,' ')
          if (format.ne.'BYTE') goto 998
          call xvget(img(i),ind,'NL',nl(i),'NS',ns(i),' ')
      enddo
c
      if (ni.ge.3) then
          call getpts(*999)	!read old tiepoints from third input file
      else
          npts = 0
      endif

      do i=1,500
          u(i) = -999.
          v(i) = -999.
      enddo

      nl1 = nl(1)
      ns1 = ns(1)
      nl2 = nl(2)
      ns2 = ns(2)
c     ....initialize display devices
      call device2(nl1,ns1,nl2,ns2,*999)
c     ....navigate the frames
      call stacka(9,navmain,2,nl1*ns1,nl2*ns2,nl1,ns1,nl2,ns2,ind)
      if (ind.lt.0) goto 999
c     ....write tiepoints to output file
      if (no.eq.1) call putpts(*999)
      call xvmessage(' NAV2 task completed',' ')
      return
c
  998 call xvmessage(' ***Input image must be in byte format',' ')
  999 call xvmessage(' ***NAV2 task cancelled',' ')
      call abend
      end

C==============================================================================
c image navigation controlling routine...
c
c inputs: image arrays pic1(ns1,nl1) and pic2(ns2,nl2)
c return indicator <0 on error
C==============================================================================
      subroutine navmain(pic1,npix1,pic2,npix2,nl1,ns1,nl2,ns2,ind)
      IMPLICIT NONE

      integer npix1,npix2,nl1,ns1,nl2,ns2,ind
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'cpic.fin'
      include 'cpts.fin'
      include 'cmap.fin'
      include 'const.fin'
      include 'dev.fin'

      common/sedr/sedr(200,2),lbuf(80,2),isystem,p_source
      character*4 p_source
      real*4 sedr
      integer lbuf, isystem
      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project
      common/cp5/debug,ni,no,navmode,ifit
      integer debug, ni, no, navmode, ifit
      common/ipic2/nl(2),ns(2)
      integer nl, ns
      common/ipic/img(2),sl(2),ss(2)
      integer img, sl,ss

      logical parmtst
      integer i,mode,mpv,n

      do i=1,nl1
         call xvread(img(1),pic1(1,i),ind, ' ')	!read left image
      enddo

      do i=1,nl2
         call xvread(img(2),pic2(1,i),ind, ' ')	!read right image
      enddo

c     ....connect to "home" plane and display it...
      call home(1,pic1,nl1,ns1,pic2,nl2,ns2)

c     ....get navigation data from sedr
      call navsedr(navmode,*999)

c     ....get camera distortion-correction parameters
      mode = 0
      do 12 i=1,2
      if (itype(i).eq.8) goto 12
      call getnav2(i)
      call getgeopar(project_id,camera_id,frame_id,planet_id,
     &		conv(1,i),nph(i),npv(i),mode,*999)
   12 continue

c     ....convert the tiepoints from image-space to object-space
      do 15 i=1,npts
      if (itype(1).eq.7) call convisos(project,camera_id,
     &          lpt(1,i),lpt(2,i),lpt_os(1,i),lpt_os(2,i),
     &          1,conv,nph,npv,ind)
      if (itype(2).eq.7) call convisos(project(2),camera_id,
     &          rpt(1,i),rpt(2,i),rpt_os(1,i),rpt_os(2,i),
     &          1,conv(1,2),nph(2),npv(2),ind)
   15 continue

      call timer(deltat)	!get time separation between frames (seconds).

   20 call xvintract('NAV2','NAV2')
      if (parmtst('HELP', mpv, 1)) call xvintract('NAV2',' ')

      if (parmtst('EXIT', mpv, 1)) then
          call xddclose (idev)
          if (npts.eq.0) return
          call navigate(1,navmode,ifit,deltat,*999)
          n = max0(navmode,1)
          do i=1,n
             call getnav2(i)
             call pnav2(i) 		  !print navigation data
             call updtsedr2(ind,i,sedr(1,i),sedr(1,i)) !update c-matrices
          enddo
          call estimate(deltat)
          return
      endif

      if (parmtst('NAVPTS', mpv, 1)) then
           navmode=0
           goto 20
      endif

      if (parmtst('NAVVEL', mpv, 1)) then
          call vparam(navmode)
          goto 20
      endif

      if (parmtst('FIT',n, 2)) then
          ifit = n
          call navigate(1,navmode,ifit,deltat,*20)
          goto 20
      endif

      call nparam(*20)
      if (parmtst('DELETE',n, 2)) call deletept(n,*20)
      if (parmtst('RESTORE',n, 2)) call restorept(n,*20)
      if (parmtst('GET',n, 2)) call gettp(pic1,nl1,ns1,pic2,nl2,ns2,
     &                                    n,*20)

      if (parmtst('EDIT', mpv, 1)) then
           call xvintract('PIC',' Enter LEFT or RIGHT')
           if (parmtst('EXIT', mpv, 1)) goto 20
           if (parmtst('RIGHT', mpv, 1)) then
                i = 2
           else
                i = 1
           endif
           call editnav(i)
           goto 20
      endif

      call sdsply(pic1,nl1,ns1,pic2,nl2,ns2,*20)
c           check for command to move split-screen window...
      call swindow(pic1,nl1,ns1,pic2,nl2,ns2,*20)
c           if no command, acquire a tiepoint...
      call tac(pic1,nl1,ns1,pic2,nl2,ns2,*20)
      goto 20
  999 ind = -1		! abort task....
      call xddclose (idev)
      return
      end


C==============================================================================
c user specification of correlation parameters...
C==============================================================================
      subroutine nparam(*)
      IMPLICIT NONE

      common/cp5/debug,ni,no,navmode,ifit
      integer debug, ni, no, navmode, ifit
      common/cp9/icorr,iproject,nlw,nsw,ihpf,iphase,interp,nw,cthresh
      integer icorr,iproject,nlw,nsw,ihpf,iphase,interp,nw
      real*8 cthresh

      logical parmtst
      character*80 msg
      integer mpv,n,r

  109 format(' CORR=',i1,'  PROJECT=',i1,'  HPF=',i1,'  IPHASE=',i1,
     &   '  INTERP=',i1)
  110 format(' NLW=',i2,'  NSW=',i2,'  MINCORR=',f5.2,'  ZWIND=',i2)

      if (.not.parmtst('PARAMS',mpv,1)) return

   20 call xvintract('PARAMS',' ')
      if (parmtst('EXIT', mpv, 1)) return1

      if (parmtst('STATUS', mpv, 1)) then
          write(msg,109) icorr,iproject,ihpf,iphase,interp
          call xvmessage(msg,' ')
          write(msg,110) nlw,nsw,cthresh,nw
          call xvmessage(msg,' ')
          goto 20
      endif

      if (parmtst('CORR', mpv, 1)) icorr = 1
      if (parmtst('PROJECT', mpv, 1)) iproject = 1
      if (parmtst('HPF', mpv, 1)) ihpf = 1
      if (parmtst('PHASE', mpv, 1)) iphase = 1
      if (parmtst('INTERP', mpv, 1)) interp = 1
      if (parmtst('NLW',n, 2)) nlw = 2*(n/2)	!nlw must be even
      if (parmtst('NSW',n, 2)) nsw = 2*(n/2)	!nsw must be even
      if (parmtst('MINCORR',r, 2)) cthresh=r
      if (parmtst('ZWIND',n, 2)) nw = n
      if (parmtst('NOCORR', mpv, 1)) icorr = 0
      if (parmtst('NOPROJEC', mpv, 1)) iproject = 0
      if (parmtst('NOHPF', mpv, 1)) ihpf = 0
      if (parmtst('NOPHASE', mpv, 1)) iphase = 0
      if (parmtst('NOINTERP', mpv, 1)) interp = 0
      return1
      end
