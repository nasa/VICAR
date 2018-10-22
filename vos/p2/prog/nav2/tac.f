C==============================================================================
c tiepoint acquisition cycle: acquire a pair of corresponding tiepoints in
c the left and right frames.
c
c inputs: pic1,pic2 = left and right frames
c
c outputs: tiepoints stored in lpt and rpt.  npts is updated.
c          the camera pointing is periodically updated.
C==============================================================================
      subroutine tac(pic1,nl1,ns1,pic2,nl2,ns2,*)
      IMPLICIT NONE
      integer nl1,ns1,nl2,ns2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'cmap.fin'
      include 'cpts.fin'
      include 'const.fin'
      include 'dev.fin'
      include 'cpic.fin'
      include 'xdgclr.fin'

      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project
      common/zvp/nz,zvp(2,1000)
      real*4 zvp
      integer nz
      common/ipic/img(2),sl(2),ss(2)
      integer*4 img, sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns
      common/cp5/debug,ni,no,navmode, ifit
      integer debug, ni, no, navmode, ifit
      common/cp9/icorr,iproject,nlw,nsw,ihpf,iphase,interp,nw,cthresh
      integer icorr,iproject,nlw,nsw,ihpf,iphase,interp,nw
      real*4 cthresh

      logical xst
      character*3  gmsg
      character*80 msg
      integer s1,l1,s2,l2,conv_mode,nch,ind,mapflag,mpv
      real*4 r4rl,r4rs,r4rl_os,r4rs_os,p1(128,130),p2(128,130),rmax
      real*8 rl1,rs1,rl2,rs2,rl1_os,rs1_os,rl2_os,rs2_os,rlat,
     +       rlat1,rlon1,rlat2,rlon2,rline,rsamp

      logical parmtst,xdipixelwrite,xdttext,xdcset,xdtcolor
      real*8 geodet

  110 format(' FN=',i3,'  (L1,S1)=(',f7.2,',',f7.2,
     &   ')  (LAT1,LON1)=(',f6.2,',',f7.2,')')
  111 format(' FN=',i3,'  (L1,S1)=(',f7.2,',',f7.2,
     &   ')  (LAT1,LON1)=(***.**,****.**)')
  120 format(   '         (L2,S2)=(',f7.2,',',f7.2,
     &   ')  (LAT2,LON2)=(',f6.2,',',f7.2,')')
  121 format(   '         (L2,S2)=(',f7.2,',',f7.2,
     &   ')  (LAT2,LON2)=(***.**,****.**)')

      if (npts.eq.500) goto 990
c     ....read cursor to get display coordinates (l1,s1) and image
c     ....coordinates (rl1,rs1) of left tiepoint.
      call cursor(1,l1,s1,rl1,rs1,*999)
      npts = npts + 1
      lpt(1,npts) = rl1
      lpt(2,npts) = rs1

      xst = xdipixelwrite(idev,g,s1,l1,255)	!draw dot at (l1,s1)
      if (npts.lt.10) then
      	  write(gmsg, '(I1)') npts
	  nch = 1
      else if (npts.ge.10) then
      	  write(gmsg, '(I2)') npts
	  nch = 2
      else if (npts.ge.100) then
      	  write(gmsg, '(I3)') npts
	  nch = 3
      endif
      xst = xdtcolor(CYAN,0)
      xst = xdttext(idev,g,s1+2,l1+10,1,nch,gmsg)   !draw feature number

      if (itype(1).eq.7) then
         r4rl = rl1
         r4rs = rs1
         conv_mode = 1
         call convisos(project(1),camera_id,r4rl,r4rs,r4rl_os,
     &		r4rs_os,1,conv,nph,npv,ind)
         rl1_os = r4rl_os
         rs1_os = r4rs_os
      else
         rl1_os = rl1
         rs1_os = rs1
      endif
      lpt_os(1,npts) = rl1_os
      lpt_os(2,npts) = rs1_os

      call latlon1(1,rl1_os,rs1_os,rlat1,rlon1,mapflag)
      if (mapflag.eq.1) then
           rlat = geodet(rlat1)
           write(msg,110) npts,rl1,rs1,rlat*rtd,rlon1*rtd
      else
           write(msg,111) npts,rl1,rs1
      endif
      call xvmessage(msg,' ')
c
c     ....determine tiepoint location in right frame
      rlat2 = rlat1	!initial location is same as for left frame
      rlon2 = rlon1
c     ....get wind speed and predict (lat,lon) position in right frame
      if (navmode.gt.0)
     &	 call getuv(nz,u(npts),v(npts),rlat2,rlon2,deltat,re,rp,*998)
c     ....move cursor to point in right image
      if (mapflag.eq.1) call linsam2(2,rlat2,rlon2,rline,rsamp,mapflag)
      if (mapflag.eq.1) then
           l2 = (rline-sl(2))*zoom + 1.5
           s2 = (rsamp-ss(2))*zoom + nsd/2 + 1.5
           l2 = max0(l2,1)
           s2 = max0(s2,nsd/2)
           l2 = min0(l2,nld)
           s2 = min0(s2,nsd)
      else
           l2 = l1
           s2 = s1 + nsd/2
      endif
      xst = xdcset(idev,tb,s2,l2)

   20 call xvintract('READY',' Position RIGHT cursor')
      if (parmtst('EXIT', mpv, 1)) goto 998
      call cursor(2,l2,s2,rl2,rs2,*998)
c     ....if pointing is off, navigate frames
      if ((rl2-rline)**2+(rs2-rsamp)**2 .gt. 5.d0) then
         if (itype(2).eq.7) then
           r4rl = rl2
           r4rs = rs2
           call convisos(project(2),camera_id,r4rl,r4rs,
     &          r4rl_os,r4rs_os,1,conv(1,2),nph(2),npv(2),ind)
           rl2_os = r4rl_os
           rs2_os = r4rs_os
         else
            rl2_os = rl2
            rs2_os = rs2
         endif
         rpt_os(1,npts) = rl2_os
         rpt_os(2,npts) = rs2_os
         if (nz.gt.0) then
            call latlon1(2,rl2_os,rs2_os,rlat2,rlon2,ind)
            if (ind.eq.1) call zonal(rlat2,u(npts))
         endif
         call navigate(0,navmode,ifit,deltat,*80)
      endif

   80 rmax = 0.d0
	print *, '1.1'
      if (icorr.eq.1) call fcor2(pic1,nl1,ns1,pic2,nl2,ns2,
     &     rl1,rs1,rl2,rs2,rmax,p1,p2,nlw,nsw,iproject,ihpf,
     &     iphase,interp,cthresh,*998)

      l2 = (rl2-sl(2))*zoom + 1.5
      s2 = (rs2-ss(2))*zoom + nsd/2 + 1.5
      xst = xdipixelwrite(idev,g,s2,l2,255)	!draw dot at (l2,s2)
      xst = xdttext(idev,g,s2+2,l2+10,1,nch,gmsg)

      if (itype(2).eq.7) then
        r4rl = rl2
        r4rs = rs2
        call convisos(project(2),camera_id,r4rl,r4rs,r4rl_os,r4rs_os,
     &		1,conv(1,2),nph(2),npv(2),ind)
        rl2_os = r4rl_os
        rs2_os = r4rs_os
      else
           rl2_os = rl2
           rs2_os = rs2
      endif
      call latlon1(2,rl2_os,rs2_os,rlat2,rlon2,ind)
      if (ind.eq.1) then
         rlat = geodet(rlat2)
         write(msg,120) rl2,rs2,rlat*rtd,rlon2*rtd
         if (nz.gt.0) call zonal(rlat2,u(npts))
      else
         write(msg,121) rl2,rs2
      endif
      call xvmessage(msg,' ')

      rpt(1,npts) = rl2
      rpt(2,npts) = rs2
      rpt_os(1,npts) = rl2_os
      rpt_os(2,npts) = rs2_os
      cmax(npts) = rmax
      if (no.ge.1) then
         call getzvl(pic1,nl1,ns1,rl1,rs1,nw,zl(npts),*992)
         call getzvl(pic2,nl2,ns2,rl2,rs2,nw,zr(npts),*992)
      endif
      xst = xdcset(idev,tb,s1,l1)
      return

  990 call xvmessage(' ***Maximum number of points acquired',' ')
      goto 999
  992 call xvmessage(' ***Point lies too close to picture margin',' ')
      goto 999
  998 xst = xdipixelwrite(idev,g,s1,l1,0)		!erase dot
      xst = xdtcolor(TRANSPARENT,0)
      xst = xdttext(idev,g,s1+2,l1+10,1,nch,gmsg)	!erase number
      npts = npts - 1
      xst = xdcset(idev,tb,s1,l1)
  999 call xvmessage(' Feature rejected',' ')
      return1
      end

C==============================================================================
c get wind speed and predict location of point in right frame.
c outputs: u,v = zonal and meridional velocity in m/s.
c updated: rlat,rlon
C==============================================================================
      subroutine getuv(nz,u,v,rlat,rlon,deltat,re,rp,*)
      IMPLICIT NONE
      integer nz
      real*4 u,v,par(2)
      real*8 rlat,rlon,deltat,re,rp

      integer mpv

      logical parmtst

      if (nz.eq.0) then
   10    call xvintract('VELOCITY',
     &            ' Enter velocity components (U,V)')
         if (parmtst('EXIT', mpv, 1)) return1
         if (.not.parmtst('VEL',par, 2)) goto 10
         u = par(1)
         v = par(2)
      else
         call zonal(rlat,u)
         v = 0.0
      endif

      par(1) = rlat
      par(2) = rlon
      call predict(1,u,v,-deltat,re,rp,par)
      rlat = par(1)
      rlon = par(2)
      return
      end

C==============================================================================
c get average dn value for an nwxnw area centered at (fl,fs).
c this routine is from program picmatch.
c output: z
c alternate return if area is not contained in picture boundary.
C==============================================================================
      subroutine getzvl(pic,nl,ns,rline,rsamp,nw,z,*)
      IMPLICIT NONE
      integer nl,ns,nw
      byte pic(ns,nl)
      real*8 rline,rsamp
      real   z

      include 'fortport'

      integer l,s,sl,ss,el,es,ival
      real*8 del,des,dsl,dss,sum

      if (nw.le.0) return
      l = rline
      s = rsamp
c     ....compute starting line,sample and ending line,sample for area
      sl = l - nw/2 - 1
      ss = s - nw/2 - 1
      el = sl + nw
      es = ss + nw
      if (sl.lt.1.or.el.gt.nl) return1
      if (ss.lt.1.or.es.gt.ns) return1

      del = rline - l
      des = rsamp - s
      dsl = 1. - del
      dss = 1. - des

      ival = 0
      sum = 0.
c     ....first, sum up the interior dn values
      do l=sl+1,el-1
         do s=ss+1,es-1
C - old            lval(1) = pic(s,l)
	    ival = byte2int(pic(s,l))      ! - new (dpp)
            sum = sum + ival
         enddo
      enddo
c     ....sum up upper and lower margins
      do s=ss+1,es-1
C - old        lval(1) = pic(s,sl)
	ival = byte2int(pic(s,sl))      ! - new (dpp)
        sum = sum + ival*dsl
C - old        lval(1) = pic(s,el)
	ival = byte2int(pic(s,el))      ! - new (dpp)
        sum = sum + ival*del
      enddo
c     ....sum up left and right margins
      do l=sl+1,el-1
C - old        lval(1) = pic(ss,l)
	ival = byte2int(pic(ss,l))      ! - new (dpp)
        sum = sum + ival*dss
C - old        lval(1) = pic(es,l)
	ival = byte2int(pic(es,l))      ! - new (dpp)
        sum = sum + ival*des
      enddo
c     ....sum up the four corners
C - old      lval(1) = pic(ss,sl)
      ival = byte2int(pic(ss,sl))      ! - new (dpp)
      sum = sum + ival*dss*dsl
C - old      lval(1) = pic(es,sl)
      ival = byte2int(pic(es,sl))      ! - new (dpp)
      sum = sum + ival*des*dsl
C - old      lval(1) = pic(ss,el)
      ival = byte2int(pic(ss,el))      ! - new (dpp)
      sum = sum + ival*dss*del
C - old      lval(1) = pic(es,el)
      ival = byte2int(pic(es,el))      ! - new (dpp)
      sum = sum + ival*des*del
c     ....output the average
      z = sum/float(nw*nw)
      return
      end

C==============================================================================
c locate feature #n in split screen display and report (lat,lon) coordinates.
c move display window if feature is not visible.  return1 if always used.
c
c inputs: feature number n, picture arrays pic1(ns1,nl1) and pic2(ns2,nl2)
c updated: display window sl(2),ss(2)
C==============================================================================
      subroutine gettp(pic1,nl1,ns1,pic2,nl2,ns2,n,*)
      IMPLICIT NONE
      integer ns1,nl1,ns2,nl2,n
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'const.fin'
      include 'cpts.fin'
      include 'dev.fin'


      common/ipic2/nl(2),ns(2)
      integer nl,ns
      common/ipic/img(2),sl(2),ss(2)
      integer*4 img,sl,ss

      real*8 buf(1),rl1,rs1,rl2,rs2,rlat1,rlon1,rlat,rlat2,rlon2
      integer s1,l1,s2,l2,sl1,ss1,sl2,ss2,maxsl,maxss,ind
      logical xst

      logical xdifill,xdcset
      real*8 geodet

      character*80 msg
  110 format(' FN=',i3,'  (L1,S1)=(',f7.2,',',f7.2,
     &   ')  (LAT1,LON1)=(',f6.2,',',f7.2,')')
  111 format(' FN=',i3,'  (L1,S1)=(',f7.2,',',f7.2,
     &   ')  (LAT1,LON1)=(***.**,****.**)')
  120 format(   '         (L2,S2)=(',f7.2,',',f7.2,
     &   ')  (LAT2,LON2)=(',f6.2,',',f7.2,')')
  121 format(   '         (L2,S2)=(',f7.2,',',f7.2,
     &   ')  (LAT2,LON2)=(***.**,****.**)')

      if (n.gt.npts) goto 992
c	display feature in left screen...
      rl1 = lpt(1,n)			! feature #n is at image
      rs1 = lpt(2,n)			! coordinates (rl1,rs1)
      if (rl1.lt.0.0.or.rs1.lt.0.0) goto 990
      l1 = (rl1-sl(1))*zoom + 1.5	! convert to display
      s1 = (rs1-ss(1))*zoom + 1.5	! coordinates (l1,s1)
      if (l1.gt.0.and.s1.gt.0
     &     .and.l1.le.nld.and.s1.le.nsd/2) goto 20
c	if feature is not in display window, move window so that feature
c	lies in center of window...
      sl1 = rl1 - nld/(2.*zoom)		! compute (sl1,ss1) of display window
      ss1 = rs1 - nsd/(4.*zoom)
      sl1 = max0(sl1,1)
      ss1 = max0(ss1,1)
      maxsl = nl1 - nld/zoom + 1
      maxss = ns1 - nsd/(2.*zoom) + 1
      sl(1) = min0(sl1,maxsl)
      ss(1) = min0(ss1,maxss)      
      xst = xdifill(idev,g,0)
      call spic(1,pic1,nl1,ns1,buf)	! redisplay image plane
      call tpdisplay(1)			! and tiepoints.
      l1 = (rl1-sl(1))*zoom + 1.5	! convert to display
      s1 = (rs1-ss(1))*zoom + 1.5	! coordinates (l1,s1)
   20 xst = xdcset(idev,tb,s1,l1)	! position cursor on feature
c
c	display feature in right screen...
      rl2 = rpt(1,n)
      rs2 = rpt(2,n)
      if (rl2.lt.0.0.or.rs2.lt.0.0) goto 990
      l2 = (rl2-sl(2))*zoom + 1.5
      s2 = (rs2-ss(2))*zoom + 1.5
      if (l2.gt.0.and.s2.gt.0
     &     .and.l2.le.nld.and.s2.le.nsd/2) goto 40
      sl2 = rl2 - nld/(2.*zoom)
      ss2 = rs2 - nsd/(4.*zoom)
      sl2 = max0(sl2,1)
      ss2 = max0(ss2,1)
      maxsl = nl2 - nld/zoom + 1
      maxss = ns2 - nsd/(2.*zoom) + 1
      sl(2) = min0(sl2,maxsl)
      ss(2) = min0(ss2,maxss)
      call spic(2,pic2,nl2,ns2,buf)
      call tpdisplay(2)
c
c	print (lat,lon) of feature...
   40 call latlon2(1,rl1,rs1,rlat1,rlon1,ind)
      if (ind.eq.1) then
             rlat = geodet(rlat1)
             write(msg,110) n,rl1,rs1,rlat*rtd,rlon1*rtd
      else
             write(msg,111) n,rl1,rs1
      endif
      call xvmessage(msg,' ')

      call latlon2(2,rl2,rs2,rlat2,rlon2,ind)
      if (ind.eq.1) then
             rlat = geodet(rlat2)
             write(msg,120) rl2,rs2,rlat*rtd,rlon2*rtd
      else
             write(msg,121) rl2,rs2
      endif
      call xvmessage(msg,' ')
      return1

  990 call xvmessage(' ***Point has been deleted',' ')
      goto 999
  992 call xvmessage(' ***Point does not exist',' ')
  999 return1
      end

C==============================================================================
c delete tiepoint #n.  the alternate return is always taken.
C==============================================================================
      subroutine deletept(n,*)
      IMPLICIT NONE
      integer n

      include 'cpts.fin'

      character*80 msg

  110 format(' ***Feature',i3,' already deleted')

      if (n.gt.npts) return1

      if (lpt(1,n).lt.0.) then
         write(msg,110) n
         call xvmessage(msg,' ')
         return1
      endif

      lpt(1,n) = -lpt(1,n)
      lpt(2,n) = -lpt(2,n)
      rpt(1,n) = -rpt(1,n)
      rpt(2,n) = -rpt(2,n)
      lpt_os(1,n) = -lpt_os(1,n)
      lpt_os(2,n) = -lpt_os(2,n)
      rpt_os(1,n) = -rpt_os(1,n)
      rpt_os(2,n) = -rpt_os(2,n)

      call tpdisplay(1)
      call tpdisplay(2)
      return1
      end

C==============================================================================
c restore tiepoint #n.  the alternate return is always taken.
C==============================================================================
      subroutine restorept(n,*)
      IMPLICIT NONE
      integer n

      include 'cpts.fin'

      character*80 msg

  110 format(' ***Feature',i3,'  already restored')

      if (n.gt.npts) return1

      if (lpt(1,n).gt.0.) then
         write(msg,110) n
         call xvmessage(msg,' ')
         return1
      endif

      lpt(1,n) = -lpt(1,n)
      lpt(2,n) = -lpt(2,n)
      rpt(1,n) = -rpt(1,n)
      rpt(2,n) = -rpt(2,n)
      lpt_os(1,n) = -lpt_os(1,n)
      lpt_os(2,n) = -lpt_os(2,n)
      rpt_os(1,n) = -rpt_os(1,n)
      rpt_os(2,n) = -rpt_os(2,n)

      call tpdisplay(1)
      call tpdisplay(2)
      return1
      end

C==============================================================================
c fourier correlation routine...
c
c inputs: pic1,pic2 = left and right (reference) images
c         (rl1,rs1) and (rl2,rs2) = (line,sample) coordinates of centers
c		of left and right areas.
c         the areas are of dimension nlw x nsw pixels
c         p1,p2 = work area of dimension nlw+2 x nsw.  to hold transforms
c		of left and right areas.
c         iproject=1 to project left area, =0 otherwise
c         ihpf=1 to zero out transform axis (high pass filter the areas)
c         iphase=1 to correlate phase information only
c         interp=1 to interpolate to find correlation maximum
c	  cthresh=correlation threshold
c
c output: cmax = max correlation coefficient
c	  (rl2,rs2) are updated
C==============================================================================
      subroutine fcor2(pic1,nl1,ns1,pic2,nl2,ns2,rl1,rs1,rl2,rs2,cmax,
     &     p1,p2,nlw,nsw,iproject,ihpf,iphase,interp,cthresh,*)
      IMPLICIT NONE
      integer ns1,nl1,nsw,nl2,ns2,nlw,iproject,ihpf,iphase,interp
C - old (dpp)      logical*1 pic1(ns1,nl1),pic2(ns2,nl2)
      byte pic1(ns1,nl1),pic2(ns2,nl2)
      REAL*8 rl1, rs1, rl2, rs2
      real*4 p1(nsw,2),p2(nsw,2),cmax,cthresh

      common/cp5/debug,ni,no,navmode,ifit
      integer debug, ni, no, navmode, ifit

      integer l1,s1,l2,s2,sl,ss,nswh,nlwh,nlw2,l,ind,i,ll,ii,npixel,
     +        ix,iy,ipass
      logical*1 buf(132)	!work buffer for svideo and sprnt
      character*80 msg
      real*4 temp,dx,dy,scale,delta
      real*8 x1,y1,rlat,rlon

      nswh = nsw/2
      nlwh = nlw/2
      nlw2 = nlw + 2
      l1 = rl1
      s1 = rs1
      l2 = rl2
      s2 = rs2

      if (iproject.eq.1) then
           call project(pic1,nl1,ns1,rl1,rs1,p1,nlw,nsw,*999)
      else
           sl = l1 - nlwh
           ss = s1 - nswh
           if(sl.lt.1.or.ss.lt.1) goto 994
           if(sl+nlw.gt.nl1) goto 994
           if(ss+nsw.gt.ns1) goto 994
           y1 = sl
           x1 = ss
		print *, 'LL 1'
           call latlon2(1,y1,x1,rlat,rlon,ind)
           if (ind.eq.0) goto 991
		print *, 'LL 2'
           call latlon2(1,y1,x1+nsw-1,rlat,rlon,ind)
           if (ind.eq.0) goto 991
		print *, 'LL 3'
           call latlon2(1,y1+nlw-1,x1,rlat,rlon,ind)
           if (ind.eq.0) goto 991
		print *, 'LL 4'
           call latlon2(1,y1+nlw-1,x1+nsw-1,rlat,rlon,ind)
           if (ind.eq.0) goto 991
           do l=1,nlw
               call floata(1,nsw,pic1(ss,sl+l-1),p1(1,l))
           enddo
      endif
c             transform the left picture area
      call rft2(p1,nlw,nsw,1,ind)
      if (ind.ne.1) goto 998
      p1(1,1) = 0.0		!zero out the dc term
      p1(1,2) = 0.0

      if(ihpf.eq.1) then
          do l=3,nlw2
              p1(1,l) = 0.0	!zero out the y-axis
          enddo
          do i=2,nsw
              p1(i,1) = 0.0	!zero out the x-axis reals
              p1(i,2) = 0.0	!zero out the x-axis imaginaries
          enddo
      endif

      if (iphase.eq.1) call phase(p1,nsw,nlw2)
      ipass = 0
c
    5 ipass = ipass + 1
c           transform the right picture area
      sl = l2 - nlwh
      ss = s2 - nswh
      if(sl.lt.1.or.ss.lt.1) goto 995
      if(sl+nlw.gt.nl2) goto 995
      if(ss+nsw.gt.ns2) goto 995
      y1 = sl
      x1 = ss
      call latlon2(2,y1,x1,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call latlon2(2,y1,x1+nsw-1,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call latlon2(2,y1+nlw-1,x1,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call latlon2(2,y1+nlw-1,x1+nsw-1,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      do l=1,nlw
          call floata(1,nsw,pic2(ss,sl+l-1),p2(1,l))
      enddo
      call rft2(p2,nlw,nsw,1,ind)
      if (ind.ne.1) goto 998
      if (iphase.eq.1) call phase(p2,nsw,nlw2)

      call cmulx(nsw,nlw2,p1,p2)	!multiply the transforms into p2
      call rft2(p2,nlw,nsw,-1,ind)	!take inverse transform
      if (ind.ne.1) goto 998
c
c          rearrange transform (swap quadrants 1&3, 2&4)
      do 32 l=1,nlwh
      ll = l + nlwh
      do 32 i=1,nswh
      ii = i + nswh
      temp = p2(ii,ll)
      p2(ii,ll) = p2(i,l)
      p2(i,l) = temp
      temp = p2(ii,l)
      p2(ii,l) = p2(i,ll)
   32 p2(i,ll) = temp
c
      if(interp.eq.1) then
          call centro(p2,nlw,nsw,dy,dx,cmax,npixel,*998)
      else
          call maxr(p2,nlw*nsw,cmax,i)
          if(cmax.lt.1.0e-10) goto 998
          dy = (i-1)/nsw + 1
          dx = mod(i-1,nsw) + 1
      endif

      scale=255./cmax
      if(debug.eq.1) then
          call svideo(p2,buf,nsw,nlw,scale,dx,dy)
          call xvmessage(' Listing of correlation matrix',30,' ')
          call sprnt(p2,buf,scale,nsw,nlw)
      endif

      ix = dx - nswh - 1		!round offsets to nearest integer
      iy = dy - nlwh - 1
      if (ix.eq.0.and.iy.eq.0) goto 80
      if (ipass.lt.3) then
          s2 = s2 + ix		!move window and try again
          l2 = l2 + iy
          goto 5
      endif

      if (cmax.lt.cthresh) goto 996
      delta = (dx-nlwh-1)**2 + (dy-nswh-1)**2
      if (delta.gt.1.1) goto 998
c
   80 rs2 = rs1 + dx + (s2-s1-nswh-1)
      rl2 = rl1 + dy + (l2-l1-nlwh-1)
      if(debug.ne.1) call svideo(p2,buf,nsw,nlw,scale,dx,dy)
      return
c
  991 call xvmessage(' ***Left point too close to planet edge',' ')
      goto 999
  992 call xvmessage(' ***Right point too close to planet edge',' ')
      goto 999
  994 call xvmessage(' ***Left point too close to picture border',' ')
      goto 999
  995 call xvmessage(' ***Right point too close to picture border',' ')
      goto 999
  996 write(msg,101) cmax
  101 format(' ***Correlation maximum below threshold: CMAX=',f4.1)
      call xvmessage(msg,' ')
      goto 999
  998 call xvmessage(' ***No correlation',' ')
  999 return1
      end

C==============================================================================
c project left area into the right area...
c
c inputs: pic(ns,nl) = left image
c         (y0p,x0p) = (line,sample) center of left area
c 
c output: p = projected area of dimension nlw x nsw
C==============================================================================
      subroutine project(pic,nl,ns,y0p,x0p,p,nlw,nsw,*)
      IMPLICIT NONE
      integer ns,nl,nlw,nsw
      byte pic(ns,nl)
      real*4 p(nsw,1)
      real*8 y0p,x0p

      include 'fortport'

      integer d1,d2,d3,d4,nswh,nlwh,ind,i,l,ix,iy
      real*8 x0,y0,x1,y1,x2,y2,x1p,y1p,x2p,y2p,x3p,y3p,x4p,y4p,
     +       rlat,rlon,dx,dy,dxdy,b0,b1,b2,b3,a0,a1,a2,a3,g0,g1,
     +       h0,h1,xp,yp

      nswh = nsw/2
      nlwh = nlw/2

      call latlon2(1,y0p,x0p,rlat,rlon,ind)
      if (ind.eq.0) goto 991
      call linsam2(2,rlat,rlon,y0,x0,ind)
      if (ind.eq.0) goto 992
c         the (right) reference area is centered at (x0,y0).  compute the
c         corners (x1,y1), (x2,y1), (x1,y2), and (x2,y2)...
      y1 = y0 - nlwh
      x1 = x0 - nswh
      y2 = y0 + nlwh - 1
      x2 = x0 + nswh - 1
      if (x1.lt.1.or.y1.lt.1) goto 997
c          compute the corresponding corners for the left area, (x1p,y1p),
c          (x2p,y2p), (x3p,y3p), (x4p,y4p)...
      call latlon2(2,y1,x1,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call linsam2(1,rlat,rlon,y1p,x1p,ind)
      if (ind.eq.0) goto 991

      call latlon2(2,y1,x2,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call linsam2(1,rlat,rlon,y2p,x2p,ind)
      if (ind.eq.0) goto 991

      call latlon2(2,y2,x1,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call linsam2(1,rlat,rlon,y3p,x3p,ind)
      if (ind.eq.0) goto 991

      call latlon2(2,y2,x2,rlat,rlon,ind)
      if (ind.eq.0) goto 992
      call linsam2(1,rlat,rlon,y4p,x4p,ind)
      if (ind.eq.0) goto 991

      if (x1p.lt.1.or.y1p.lt.1.or.x1p.ge.ns.or.y1p.ge.nl) goto 996
      if (x2p.lt.1.or.y2p.lt.1.or.x2p.ge.ns.or.y2p.ge.nl) goto 996
      if (x3p.lt.1.or.y3p.lt.1.or.x3p.ge.ns.or.y3p.ge.nl) goto 996
      if (x4p.lt.1.or.y4p.lt.1.or.x4p.ge.ns.or.y4p.ge.nl) goto 996
c	   compute linear transformation for projecting left area into
c          right area...
      dx = x1 - x2
      dy = y1 - y2
      dxdy = dx*dy
      a3 = (x1p-x2p-x3p+x4p)/dxdy
      b3 = (y1p-y2p-y3p+y4p)/dxdy
      a2 = (x1p-x3p)/dy - a3*x1
      b2 = (y1p-y3p)/dy - b3*x1
      a1 = (x1p-x2p)/dx - a3*y1
      b1 = (y1p-y2p)/dx - b3*y1
      a0 = x1p - (a1*x1+a2*y1+a3*x1*y1)
      b0 = y1p - (b1*x1+b2*y1+b3*x1*y1)            
      g0 = a0 + a2*y1
      g1 = a1 + a3*y1
      h0 = b0 + b2*y1
      h1 = b1 + b3*y1
c           project the left area...
      do 10 l=1,nlw
      xp = g0 + g1*x1
      yp = h0 + h1*x1

      do 8 i=1,nsw
      ix = xp
      iy = yp
      dx = xp - ix
      dy = yp - iy
C - these are the old way - dpp
C      lval1(1) = pic(ix,iy)
C      lval2(1) = pic(ix+1,iy)
C      lval3(1) = pic(ix,iy+1)
C      lval4(1) = pic(ix+1,iy+1)
C these are the new way - dpp
      d1 = byte2int(pic(ix,iy))
      d2 = byte2int(pic(ix+1,iy))
      d3 = byte2int(pic(ix,iy+1))
      d4 = byte2int(pic(ix+1,iy+1))
      p(i,l) = d1 + (d2-d1)*dx + (d3-d1)*dy + (d1-d2-d3+d4)*dx*dy
      xp = xp + g1
    8 yp = yp + h1

      g0 = g0 + a2
      g1 = g1 + a3
      h0 = h0 + b2
   10 h1 = h1 + b3

      return
c
  991 call xvmessage(' ***Left point too close to planet edge',' ')
      return1
  992 call xvmessage(' ***Right point too close to planet edge',' ')
      return1
  996 call xvmessage(' ***Left point too close to picture border',' ')
      return1
  997 call xvmessage(' ***Right point too close to picture border',' ')
      return1
      end

C==============================================================================
c compute phase term of transform...
C==============================================================================
      subroutine phase(p,nsw,nlw2)
      IMPLICIT NONE
      integer nsw,nlw2
      real*4 p(nsw,nlw2)

      integer i,l
      real*4 rmag

      do l=1,nlw2,2
        do i=1,nsw
          rmag = sqrt(p(i,l)**2 + p(i,l+1)**2)
          if (rmag.eq.0.0) then
              p(i,l) = 0.0
              p(i,l+1) = 0.0
          else
              p(i,l) = p(i,l)/rmag
              p(i,l+1) = p(i,l+1)/rmag
          endif
        enddo
      enddo

      return
      end

C==============================================================================
c this routine will do a complex multiply of the array format resulting
c from an rft2 call.  let x=a+bi, y=c+di.  then upon return,
c         y = (a-bi)*(c+di)
c           = (ac+bd) + i(ad-bc)
C==============================================================================
      subroutine cmulx(n,m,x,y)
      IMPLICIT NONE
      integer n,m
      real x(n,m),y(n,m)

      real c,d
      integer i,l

      do 10 l=1,m,2
      do 10 i=1,n
      c = x(i,l)*y(i,l) + x(i,l+1)*y(i,l+1)
      d = x(i,l)*y(i,l+1) - x(i,l+1)*y(i,l)
      y(i,l) = c
   10 y(i,l+1) = d
c
      return
      end

C==============================================================================
c routine to compute the centroid (dx,dy) of all r(i) gt a threshold.
c the threshold is determined by finding the two largest local maxima,
c rmax & r2.  r2 is then used as the threshold.
c a local maximum r0 satisfies the following conditions...
c   1. r0 lies in the interior of the region r(nsw,nlw)
c   2. r0 is gt each of its 8 neighbors
c   3. r0 is gt any point on the border
c upon return, the program will supply...
c   (dx,dy) = location of centroid
c   rmax = maximum value in region r
c   npixel = number of elements used in computing centroid
c alternate error if maximum occurs on edge or if centroid has correlation
c value less than zero.
C==============================================================================
      subroutine centro(r,nlw,nsw,dy,dx,rmax,npixel,*)
      IMPLICIT NONE
      integer nlw,nsw,npixel
      real*4 r(1),dx,dy,rmax

      integer s,nlw1,nsw1,nsw2,nswt2,i1,i2,i3,i,j,imax,l,k
      real r0,r2,sigdn,sigx,sigy

      rmax = 1.0e-10
      nlw1 = nlw - 1
      nsw1 = nsw - 1
      nsw2 = nsw + 2
      nswt2 = nsw*2
c          find largest border value
      i1 = nlw1*nsw
      i2 = nsw
      i3 = 2*nsw - 1
c
      do i=1,nsw
         if(r(i).gt.rmax) rmax=r(i)
         if(r(i1+i).gt.rmax) rmax=r(i1+i)
      enddo
c
      i = 1
c
      do j=2,nlw1
         if (r(i2+i).gt.rmax) rmax=r(i2+i)
         if (r(i3+i).gt.rmax) rmax=r(i3+i)
         i = i + nsw
      enddo
c
      r2 = rmax
      imax = 0
      i = nsw
c
c          find two largest local maxima
      do 50 l=2,nlw1
         i = i + 2
         do 50 s=2,nsw1
            if(r(i).le.r2) goto 50
            r0 = r(i)
            if (r0.le.r(i-1).or.r0.le.r(i+1)) goto 50
            i1 = i - nsw2
c
            do j=1,2
               do k=1,3
                  if (r0.le.r(i1+k)) goto 50
               enddo
               i1 = i1 + nswt2
            enddo
c
            if (r0.gt.rmax) then
               r2 = rmax
               rmax = r0
               imax = i
            else
               r2 = r0
            endif
   50 i = i + 1
c
      if(imax.eq.0) return1
c
c          compute centroid
      sigdn = 0.
      sigx = 0.
      sigy = 0.
      i = nsw
      npixel = 0
c
      do l=2,nlw1
         i = i + 2
         do s=2,nsw1
            if (r(i) .gt. r2) then
               npixel = npixel + 1
               r0 = r(i)
               sigdn = sigdn + r0
               sigx = sigx + r0*s
               sigy = sigy + r0*l
            endif
            i = i + 1
         enddo
      enddo
c
      if (sigdn .le. 1.e-10) return1
      dx = sigx / sigdn
      dy = sigy / sigdn
      return
      end

C==============================================================================
c routine to find the maximum value in a real*4 array.  in case of ties,
c first occurence is returned.
C==============================================================================
      subroutine maxr(r,n,rmax,i)
      IMPLICIT NONE
      integer n       ! input # of elements in r
      real*4 r(n)       ! input array
      real*4 rmax       ! output maximum value in r
      integer*4 i       ! output index to rmax, i.e. rmax=r(i)

      integer j

      rmax = r(1)
      i = 1

      do j=2,n
           if (r(j).gt.rmax) then
                rmax = r(j)
                i = j
           endif
      enddo

      return
      end

C==============================================================================
c display the correlation matrix on the video plane...
c
c inputs: r = nlw x nsw correlation matrix
c         scale = normalization factor to scale correlation values so
c                 that maximum does not exceed 255 dn.
c         (dy,dx) = location of correlation maximum
c         buf = 129 byte buffer area to hold display line.
C==============================================================================
      subroutine svideo(r,buf,nsw,nlw,scale,dx,dy)
      IMPLICIT NONE
      integer nsw,nlw
      real*4 r(nsw,nlw),scale,dx,dy
      byte buf(1)

      include 'fortport'
      include 'dev.fin'

      byte lval 			! new (dpp)
      integer ival,slds,ssds,s,inc,nsds,l,ll,j,i
      logical xst,xdilinewrite

      call xdiawset(idev,vid,1,1,nsd,nld)
      inc = max0(64/nsw,1)
      nsds = inc*nsw
      slds = 1
      ssds = (nsd-nsds)/2
      ssds = ssds + mod(ssds,2)
      if (inc.eq.1) then
          l = dy + 0.5
          s = dx + 0.5
      else
          l = (dy-0.5)*inc + .5
          s = (dx-0.5)*inc + .5
      endif
c
      do j=1,nlw
          do i=1,nsw
              ival = r(i,j)*scale
              ival = max0(ival,0)
              ival = min0(ival,255)
              buf(i) = int2byte(ival)
          enddo
          if (inc.ne.1) call expand(buf,buf,nsw,inc)

          do ll=1,inc
              if (l.eq.slds) then
                  lval = buf(s)
                  buf(s) = .false.
              endif
              xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
              if (l.eq.slds) buf(s)=lval
              slds = slds + 1
          enddo
      enddo
c
      return
      end

C==============================================================================
c print the correlation matrix...
c
c inputs: r = nlw x nsw correlation matrix
c         scale = normalization factor so maximum does not exceed 255 dn
c         buf = 132 byte print line buffer
C==============================================================================
      subroutine sprnt(r,buf,scale,nsw,nlw)
      IMPLICIT NONE
      integer nsw,nlw
      real r(nsw,nlw),scale
      logical*1 buf(1)

      integer s,si,ss,ns,ii,l
      character*8 lmsg
      data lmsg/'       .'/
      character*133 smsg
      data smsg/'      SAMP'/
C      data smsg/' ',5*' ','S','A','M','P',123*' '/
c
c       list out correlation matrix in vertical strips, 30 elements per line
      do si = 1,nsw,30
         ss = si - 1
         ns = min0(nsw-ss,30)
         if (ns .lt. 30) call itla(32,smsg(11:),123)
c          print sample heading
         do s = 1, ns
	    ii = 14
C            call outcon(ss+s,smsg(4*s+11),2)
	    write(smsg(ii:ii+2), '(I2)') ss+s
	    ii = ii + 4
         end do
         call xvmessage(smsg,133,' ')
c
         do l = 13, nlw
C            call outcon(l,lmsg(7),2)
	    write(lmsg, '(I2)') l
            call sfix(1,ns,r(si,l),buf,buf,buf,scale,.5)
            call xvmessage('0',1,' ')
           call prnt(1,ns,buf,lmsg)
         end do
      end do
c
      return
      end

C==============================================================================
C==============================================================================
      subroutine sfix(dcode,ns,r,bufb,bufh,buf,scale,offset)
      IMPLICIT NONE
      integer dcode,ns,buf(1)
      real r(1),scale,offset
      byte bufb(1)	! - not used kb (dpp)
      integer*2 bufh(1)	! - not used kh (dpp)

      include 'fortport'

      integer k,i

      goto (10,20,40,30) dcode
      return
   10 do i = 1, ns
         k = scale*r(i) + offset
         if (k .lt. 0) k = 0
         if (k .gt. 255) k = 255
C - old (dpp)         bufb(i) = kb
         bufb(i) = int2byte(k)
      end do
      return
   20 do i = 1, ns
         k = scale*r(i) + offset
         if (k .lt. -32768) k = -32768
         if (k .gt. 32767) k = 32767
C - old         bufh(i) = kh
            bufh(i) = k
      end do
      return
   30 do i = 1, ns
         k = scale*r(i) + offset
         if (k .lt. -2147483647) k = -2147483647
         if (k .gt. 2147483647) k = 2147483647
         buf(i) = k
      end do
   40 return
      end
