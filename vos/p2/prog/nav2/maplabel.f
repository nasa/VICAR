C==============================================================================
c get projection type (image-space or object-space) by examining
c input image label and presence of input reseau locations.
C==============================================================================
      subroutine maplabel(img,project_id,itype,*)
      IMPLICIT NONE
      integer img,project_id,itype

      real*8 mp
      integer*4 idata(40)
      character*8  geom/'GEOM    '/,farenc/'FARENC  '/
      character*7200 label
      logical parmtst
      integer icnt,idef,jtype,nchar,stat,mpv

c     ....check if image has been map projected
      call mp_init(mp,stat)
      call mp_label_read(mp,img,stat)
      if (stat.ne.0) goto 10	!skip if map label not present
      call mp_mpo2buf(mp,idata,stat)
      if (stat.ne.0) call mabend('***Err converting MP to data buf')
      if (idata(39).ne.16) 
     +	call mabend('***image has been map projected')

c     ....check if image has been geometrically corrected
   10 nchar = 7200
      call xlgetlabel(img,label,nchar,stat)    !read image label
      call chkstat(stat,' ***Error reading input label',1)
      if (index(label(1:nchar),'GEOM').ne.0
     &		.or. index(label(1:nchar),'FARENC').ne.0) then
	 itype = 8
      else
         itype = 7
      endif
      if (project_id.gt.4) return	!skip rest if not VGR

c     ....consistency check
      call xvpcnt('RES',icnt)
      if (icnt.eq.1) then       !if reseau locations are specified,
          jtype = 7             !then image-space is assumed.
      else
          jtype = 8             !else, object-space is assumed.
      endif
      if (itype.eq.jtype) return

      call xvmessage(' ***WARNING:', ' ')	!warning messages follow...
      if (itype.eq.7) goto 20
      call xvmessage(' ***Label indicates image has been',' ')
      call xvmessage(' ***geometrically corrected.',' ')
      goto 30
   20 call xvmessage(' ***Label indicates images has not been',' ')
      call xvmessage(' ***geometrically corrected',' ')
   30 call xvintract('DISTOR',
     &  ' Specify image type by entering ''IMAGE or ''OBJECT')
      if (parmtst('EXIT', mpv, 1)) return1
      if (parmtst('IMAGE', mpv, 1)) then
         if (itype.eq.7) goto 990
         return
      endif
      if (parmtst('OBJECT', mpv, 1)) then
         itype = 8
         return
      endif
      goto 30

  990 call xvmessage('***Use RES parameter for image-space frames',' ')
      return1
      end

C==============================================================================
c routine to get navigation data from the sedr/spice file.
C==============================================================================
      subroutine spicesub(unit,project,buf,ind)
      IMPLICIT NONE
      character*5 project		!gll or vgr
      integer unit, ind
      real*8 buf(100)			!record returned by getspice

      include 'cmap.fin'
      include 'const.fin'
      include 'cpic.fin'

      common/cp5/debug,ni,no,navmode,ifit
      integer debug,ni,no,navmode,ifit

      real*4 r4fl,r4oal,r4oas,r4pscale
      character*12 blank/'            '/
      integer isource,i,j
  110 format(10x,3f12.1)

c     ....get camera constants
      call getcamcon(project,camera_id,r4fl,r4oal,r4oas,r4pscale,ind)
      fl = r4fl
      oal = r4oal
      oas = r4oas
      pscale = r4pscale
      zscale = fl*pscale		!object space scale (pixels/radian)

c     ....get spice data
      call getspice2(unit,0,buf,ind)
      if (ind.ne.1) return
      call cmsource(buf,isource)		!determine c-matrix source
      call getscet(buf,target_id,idate,itime)	!get spacecraft event time

c     ....get target-body constants
      re = 0.5*(buf(13)+buf(14))
      rp = buf(15)			!polar radius
      epsln = (re/rp)**2

      planet_id = target_id
      if (planet_id.gt.9) planet_id=planet_id/100
c
      call mve(8,9,buf(50),me,1,1)           ! me matrix
      call mve(8,9,buf(41),cm,1,1)           ! c-matrix
      call mve(8,3,buf(19),psc,1,1)	   ! spacecraft vector.
      rsc = dsqrt(psc(1)**2+psc(2)**2+psc(3)**2)    !spacecraft range
      do i=1,3
          psc(i) = -psc(i)/rsc		!make it a unit vector
      enddo

      call fromeme(me,psc,sclat,sclon)  !compute sclat,sclon
c             compute spacecraft vector in (x3,y3,z3) coordinates
      psc3(1) = rsc*dcos(sclat)
      psc3(2) = 0.0d0
      psc3(3) = rsc*dsin(sclat)
      epsln2 = psc3(1)**2 + epsln*psc3(3)**2 - re**2
      ind = 0

      if (debug.eq.0) return
      call xvmessage(' C-Matrix=',' ')
      call printmtx(cm,3)
      call orthot(cm)

      call xvmessage(' ME=',' ')
      call printmtx(me,3)
      call orthot(me)

      call xvmessage(' OM = MET*C',' ')
      do 40 i=1,3
      do 40 j=1,3
   40 om(i,j) = me(1,i)*cm(1,j) + me(2,i)*cm(2,j) + me(3,i)*cm(3,j)
c
      call printmtx(om,3)
      return
      end

C==============================================================================
c update voyager sedr file by storing c and om-matrices, rs-vector, and
c planet center...
c
c input: sedr buffer
c output: return indicator ind=0  successful sedr update
c			      =1  no update performed
c			      <0  error updating sedr
C==============================================================================
      subroutine updtsedr2(ind,ipic,sedr,dsedr)
      IMPLICIT NONE
      integer ind, ipic
      real*4 sedr(200)
      real*8 dsedr(100)

      include 'cmap.fin'
      include 'const.fin'
      include 'cpic.fin'

      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project

      real*8 omp(3,3),scline,scsamp
      real*4 naline,nasamp
      logical parmtst
      character*80 msg
      integer*2 i2camera_id
      integer mpv

   99 format(' NA planet center=(',f9.2,',',f9.2,')')

      ind = 1

    5 call xvintract('QUERY',
     &       ' Do you wish to update the SEDR? (Enter Y or N)')
      if (parmtst('N', mpv, 1)) return
      if (.not.parmtst('Y', mpv, 1)) goto 5

C      call mvl('NAV2',sedr(11),4)	!set nav2 flag...
      call mve(5, 4, 'NAV2',sedr(11), 1, 1)	!set nav2 flag...
      call ommatrix(angln,angla,anglb-sclon,omp)	!compute om' matrix
c     ....c = me*omp
      call mxm(me,omp,dsedr(41))
c     ....transpose to regular convention (camera-to-planet)
      call xpose(omp,omp)
      call mve(8,9,omp,dsedr(59), 1, 1)
c          compute rs vector
      dsedr(22) = rsc*dcos(sclat)*dcos(sclon)
      dsedr(23) = rsc*dcos(sclat)*dsin(sclon)
      dsedr(24) = rsc*dsin(sclat)
c          compute planet center....
      call plainv(ind,sclat,sclon,scline,scsamp,
     &            om,psc3,sclon,re,epsln,epsln2,oal,oas,zscale)
      if (ind.eq.0) then
           call xvmessage(' ***Err calculating planet center',' ')
           ind = -999
           return
      endif
      dsedr(69) = scline
      dsedr(70) = scsamp
      if (project_id.eq.4.and.mod(camera_id,2).eq.0) then
          i2camera_id = camera_id
          call mwatna(i2camera_id,scline,scsamp,naline,nasamp,*999)
          if (itype(ipic).eq.7) call tritra(ind,conv(1,ipic),
     &		nph(ipic),npv(ipic),naline,nasamp,0)
          write(msg,99) naline,nasamp
          call xvmessage(msg,' ')
      endif

      call putspice2('NEAR','NAV2',sedr,ind)
      IF (ind.NE.0) THEN
        if (ipic.eq.1) call xvmessage(
     +              ' New C-matrix stored for left frame',' ')
        if (ipic.eq.2) call xvmessage(
     +              ' New C-matrix stored for right frame',' ')
      ELSE
        call xvmessage('SEDR was NOT updated', ' ')
        call xvmessage('The new C-Matrix would had been:', ' ')
      END IF
  999 return
      end

C==============================================================================
c get target-id and spacecraft event time from ipl sedr buffer.
c
c outputs: target_id = sedr id # for target body
c          idate = scet date in the form yyddd (1000*year + day)
c          itime = scet time in the form hhmmssmmm
C==============================================================================
      subroutine getscet(ibuf,target_id,idate,itime)
      IMPLICIT NONE
      integer ibuf(9),target_id,idate,itime

      idate = 1000*mod(ibuf(3),100) + ibuf(4)	! idate = yyddd
      itime = 10000000*ibuf(5) + 100000*ibuf(6)
     &         + 1000*ibuf(7) + ibuf(8)		! itime = hhmmssmmm
      target_id = ibuf(9)
      return
      end

C==============================================================================
c given a unit vector in eme50 coordinates, compute the latitude-longitude
c coordinates...
c
c inputs: me matrix
c         p = unit vector
c outputs: rlat,rlon
C==============================================================================
      subroutine fromeme(me,p,rlat,rlon)
      IMPLICIT NONE
      real*8 me(3,3),p(3),rlat,rlon

      include 'const.fin'

      real*8 dot

      rlat = dasin(dot(p,me(1,3)))			!rlat = p o n
      rlon = datan2(dot(p,me(1,2)),dot(p,me(1,1)))
      rlon = dmod(rlon+2.0d0*pi,2.0d0*pi)
      return
      end

C==============================================================================
c compute time difference deltat (in seconds) between frames
C==============================================================================
      subroutine timer(deltat)
      IMPLICIT NONE
      real*8 deltat

      include 'cpic.fin'

      integer*4 time(2, 2)
      integer*2 sdrt(3)
      integer ipic,iyear,iday,ihour,msec,imin,isec
      character*80 msg

  110 format(' FRAME=',i9,'  SCET=',i5,i10)
  111 format(' Time separation=',f8.0,' seconds')

      do ipic=1,2
          call getnav2(ipic)
          iyear = idate/1000
          iday = mod(idate,1000)
          ihour = itime/10000000
          msec = mod(itime,10000000)
          imin = msec/100000
          msec = mod(msec,100000)
          isec = msec/1000
          msec = mod(msec,1000)
          sdrt(1) = 24*(iday-1) + ihour
          sdrt(2) = 60*imin + isec
          sdrt(3) = 0
          call sfdudate(time(1, ipic),time(2, ipic), sdrt,iyear)
          write(msg,110) frame_id,idate,itime
          call xvmessage(msg,' ')
      enddo
      deltat = time(1, 1) - time(1, 2)
      write(msg,111) deltat
      call xvmessage(msg,' ')
      return
      end

C==============================================================================
c return the dot product of two 3x3 vectors
C==============================================================================
      function dot(a,b)
      IMPLICIT NONE
      real*8 dot,a(3),b(3)

      dot = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
      return
      end

C==============================================================================
c routine to test orthogonality of a 3x3 rotation matrix
C==============================================================================
      subroutine orthot(a)
      IMPLICIT NONE
      real*8 a(3,3)

      character*80 msg
      data msg /' '/
      integer i, ii, j
      real*8 dij

      do 20 i=1,3
      	ii = 1
      	do 10 j=1,3
      	   dij = a(i,1)*a(j,1) + a(i,2)*a(j,2) + a(i,3)*a(j,3)
      	   write(msg(ii:ii+14), '(f14.10)') dij
   10      ii = ii + 15
   20 call xvmessage(msg,' ')
      return
      end

C==============================================================================
c routine to print out a 3xn matrix
C==============================================================================
      subroutine printmtx(mtx,n)
      IMPLICIT NONE
      integer n
      real*8 mtx(3,n)

      character*80 msg
      integer i,j

  110 format(10x,3f12.7)

      do i=1,n
          write(msg,110) (mtx(i,j),j=1,3)
          call xvmessage(msg,' ')
      enddo
      return
      end

C==============================================================================
c get navigation data from sedr.
c common areas sedr, cpic and cmap are filled.
c return1 on error.
C==============================================================================
      subroutine navsedr(navmode,*)
      IMPLICIT NONE
      integer navmode

      include 'cmap.fin'
      include 'cpic.fin'

      common/ipic2/nl(2),ns(2)
      integer nl, ns
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/sedr/sedr(100,2),lbuf(80,2),isystem
      real*8 sedr
      integer lbuf,isystem
      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project

      real*4 pbuf(20)
      character*12 target_name
      integer ipic,ind,mpv
      logical parmtst
      character*80 msg
      character*6 msg2(2)/' left ',' right'/
  101 format('Processing geometric data for',a6,' picture')

      do 50 ipic=1,2
      write(msg,101) msg2(ipic)
      call xvmessage('msg',' ')
      call frameid(img(ipic),project(ipic),lbuf(1,ipic),frame_id,
     &	        camera_id,project_id,ind)
      call maplabel(img(ipic),project_id,itype(ipic),*999)
      call spicesub(img(ipic), project(ipic), sedr(1,ipic),ind)
      if (ind.ne.0) return1
      call getangles(cm,me(1,3),psc,angln,angla,anglb) !get angln,angla,anglb
      call ommatrix(angln,angla,anglb,om)   !compute om-matrix
      call printmtx(om,3)
      call putnav20(ipic)
      call putnav2(ipic)
   50 continue

      igeo = 0				!planetocentric latitudes
      if (project_id.eq.4) igeo=1	!VGR is planetographic
      isystem = 2			!VGR and earlier is eme50 coordinates
      if (project_id.ge.5) isystem=1	!GLL and later is j2000

c     ....get target rotation rate
      call pbname(target_id,target_name,*999)
      call pbdata(target_name,pbuf,*999)
      rot = pbuf(5)			!rotation period (days)

      navmode = 0

      if (mod(target_id,100).ne.99) return	!skip satellites
      if (target_id.eq.199 .or. target_id.eq.499) return !skip Mercury and Mars
c     ....here if target is a planet with an atmosphere
      call xvmessage(' Do you wish to adjust for wind speed?',' ')
   60 call xvintract('QUERY',' Enter Y or N')
      if (parmtst('N', mpv, 1)) return
      if (.not.parmtst('Y', mpv, 1)) goto 60
      call vparam(navmode)
      return

  999 return1
      end

C==============================================================================
c given right points and sedr pointing, estimate left tiepoints.
c output: lpt0(2,500)
C==============================================================================
      subroutine estimate(deltat)
      IMPLICIT NONE
      real*8 deltat

      include 'cmap.fin'
      include 'cpts.fin'

      common/cp5/debug,ni,no,navmode, ifit
      integer debug, ni, no, navmode, ifit
      common/c1_coords/coords(2,500), rcoords(2, 500)
      real*4 coords, rcoords
      common/zvp/nz,zvp(2,1000)
      integer nz
      real*4 zvp

      integer i, ind
      real*8 rlat,rlon,rline,rsamp

      if (npts.eq.0) return
      call getnav20(1)		!get original sedr pointing
      call putnav2(1)		!and put it in current pointing
c     ....convert from line-sample in right frame to lat-lon
      call getlatlon(2,npts,rpt_os,coords)
c     ....correct for wind speed
      if (navmode.gt.0) then
         if (nz.gt.0) call getzv(npts,coords,u,v)
         call predict(npts,u,v,deltat,re,rp,coords)
      endif
c     ....convert from lat-lon to line-sample in left frame
      do 80 i=1,npts
      lpt0(1,i) = -999.0
      lpt0(2,i) = -999.0
      rlat = coords(1,i)
      rlon = coords(2,i)
      if (rlat.eq.-999.0) goto 80
      call linsam2(1,rlat,rlon,rline,rsamp,ind)
      if (ind.eq.0) goto 80
      lpt0(1,i) = rline
      lpt0(2,i) = rsamp
   80 continue

      return
      end

C==============================================================================
c use the tiepoints to update the om and c-matrices for the left frame.
c inputs: mode=1 results of fit are reported to terminal, =0 otherwise.
c         ifit=2 update angla and anglb
c             =3 update angla, anglb, and angln
c         tiepoints lpt(2,npts) and rpt(2,npts)
c outputs: om,c
c return1 on fit error.
C==============================================================================
      subroutine navpts2(mode,navmode,ifit,npts,lpt,rpt,u,v,deltt,*)
      IMPLICIT NONE
      integer mode,navmode,ifit,npts
      real*4 lpt(2,npts),rpt(2,npts),u(npts),v(npts)
      real*8 deltt

      include 'cmap.fin'
      include 'const.fin'

      common/c1_coords/coords(2,500), rcoords(2, 500)
      real*4 coords, rcoords

      real*8 scline0,scsamp0,dl,ds,dt,scline,scsamp,diff
      integer i,ind

c     ....compute lat-lon coordinates and predict their positions in
c     ....left frame.
      call getlatlon(2,npts,rpt,coords)
      if (navmode.eq.1) call predict(npts,u,v,deltt,re,rp,coords)
      call getnav2(1)
      scline0 = 0.d0
      scsamp0 = 0.d0

      do 50 i=1,10
      if (ifit.eq.2.or.npts.lt.2) then
           call fit2(1,coords,lpt,npts,dl,ds,*999)
      else
           call fit3(1,coords,lpt,npts,oal,oas,dl,ds,dt,*999)
      endif

      call move1(dl,ds,angln,angla,anglb,zscale)!update angla and anglb
      if (ifit.eq.3) angln = angln + dt		!update angln
      call ommatrix(angln,angla,anglb,om)	!compute new om matrix
      call cmatrix(me,angln,angla,anglb,sclon,cm)  !compute c matrix
      call putnav2(1)
      call linsam1(1,sclat,sclon,scline,scsamp,ind) !compute planet center
      diff = (scline-scline0)**2 + (scsamp-scsamp0)**2
      if (diff.lt.0.5) goto 60
      scline0 = scline
   50 scsamp0 = scsamp

      call xvmessage('***Fit does not converge',' ')

   60 if (mode.eq.1) call rmserr(npts,coords,lpt)
      return

c	fit error...
  999 return1
      end

C==============================================================================
c use the tiepoints to update the om and c-matrices of both frames.
c the rms errors are split between the two frames.
c inputs: mode=1 results of fit are reported to terminal, =0 otherwise.
c         ifit=2 update angla and anglb
c             =3 update angla, anglb, and angln
c         tiepoints lpt(2,npts) and rpt(2,npts)
c outputs: om,c
c return1 on fit error.
C==============================================================================
      subroutine navsplit(mode,ifit,npts,lpt,rpt,u,v,deltt,*)
      IMPLICIT NONE
      integer mode,ifit,npts
      real*4 lpt(2,npts),rpt(2,npts),u(npts),v(npts)
      real*8 deltt

      include 'cmap.fin'
      include 'const.fin'

      common/zvp/nz,zvp(2,1000)
      integer nz
      real*4 zvp
      common/c1_coords/lcoords(2,500), rcoords(2,500)
      real*4 lcoords,rcoords

      real*8 scline,scsamp,scline1,scsamp1,scline2,scsamp2,dl1,ds1,
     +       dl2,ds2,dt1,dt2,diff1,diff2
      integer i

      scline1 = 0.d0
      scsamp1 = 0.d0
      scline2 = 0.d0
      scsamp2 = 0.d0

      do 50 i=1,10
c     ....predict coordinates of left points from right points and u,v
      call getlatlon(2,npts,rpt,lcoords)
      if (nz.gt.0) call getzv(npts,lcoords,u,v)
      call predict(npts,u,v,deltt,re,rp,lcoords)
c     ....predict coordinates of right points from left points and u,v
      call getlatlon(1,npts,lpt,rcoords)
      if (nz.gt.0) call getzv(npts,rcoords,u,v)
      call predict(npts,u,v,-deltt,re,rp,rcoords)
c     ....fit the points to their predicted locations
      if (ifit.eq.2.or.npts.lt.2) then
         call fit2(1,lcoords,lpt,npts,dl1,ds1,*999)
         call fit2(2,rcoords,rpt,npts,dl2,ds2,*999)
      else
         call fit3(1,lcoords,lpt,npts,oal,oas,dl1,ds1,dt1,*999)
         call fit3(2,rcoords,rpt,npts,oal,oas,dl2,ds2,dt2,*999)
      endif
c     ....update the pointing and recompute planet-center
      call updatenav(1,ifit,dl1/2.,ds1/2.,dt1/2.,scline,scsamp)
      diff1 = (scline-scline1)**2 + (scsamp-scsamp1)**2
      scline1 = scline
      scsamp1 = scsamp
      call updatenav(2,ifit,dl2/2.,ds2/2.,dt2/2.,scline,scsamp)
      diff2 = (scline-scline2)**2 + (scsamp-scsamp2)**2
      scline2 = scline
      scsamp2 = scsamp
      if (diff1+diff2.lt.0.5) goto 60	!stop if pc hasn't moved
   50 continue

      call xvmessage('***Fit does not converge',' ')

   60 if (mode.eq.0) return
c     ....print rms error between predicted and actual points
      call getlatlon(2,npts,rpt,lcoords)
      if (nz.gt.0) call getzv(npts,lcoords,u,v)
      call predict(npts,u,v,deltt,re,rp,lcoords) !predicted points
      call rmserr(npts,lcoords,lpt)
      return

c-----fit error...
  999 return1
      end

C==============================================================================
c navigate the left (and possibly right) frame.
C==============================================================================
      subroutine navigate(mode,navmode,ifit,deltat,*)
      IMPLICIT NONE
      integer mode,navmode,ifit
      real*8 deltat

      include 'cpts.fin'

      integer ipic

      if (npts.eq.0) return
      if (navmode.eq.2) then		!split the errors btwn both frames
         do ipic=1,2			!restore the original pointing
            call getnav20(ipic)
            call putnav2(ipic)
         enddo
         call navsplit(mode,ifit,npts,lpt_os,rpt_os,u,v,deltat,*999)
      else				!correct left frame only
         call navpts2(mode,navmode,ifit,npts,lpt_os,rpt_os,
     &		u,v,deltat,*999)
      endif
      return
c
  999 return1
      end

C==============================================================================
c given line-sample coordinates pts, compute corresponding lat-lon
c coordinates coords.
C==============================================================================
      subroutine getlatlon(ipic,npts,pts,coords)
      IMPLICIT NONE
      integer ipic,npts
      real*4 pts(2,npts),coords(2,npts)

      character*80 msg
      integer i,ind
      real*8 rline,rsamp,rlat,rlon

  115 format(' ***PT',i4,' is off planet in LEFT frame')
  116 format(' ***PT',i4,' is off planet in RIGHT frame')

      do 5 i=1,npts
      rline = pts(1,i)
      rsamp = pts(2,i)
      coords(1,i) = -999.
      if (rline.lt.0.d0) goto 5
      call latlon1(ipic,rline,rsamp,rlat,rlon,ind)
      if (ind.eq.1) then
         coords(1,i) = rlat
         coords(2,i) = rlon
      else
         if (ipic.eq.1) write(msg,115) i
         if (ipic.eq.2) write(msg,116) i
         call xvmessage(msg,' ')
      endif
    5 continue

      return
      end

C==============================================================================
c for each point in pts, compute zonal velocities (in meters/sec) from
c zonal velocity profile.
C==============================================================================
      subroutine getzv(npts,coords,u,v)
      IMPLICIT NONE
      integer npts
      real*4 coords(2,npts),u(npts),v(npts)

      integer i
      real rlat, vel

      do i=1,npts
         rlat = coords(1,i)
         if (rlat.ne.-999.0) then
            call zonal(rlat,vel)	!compute zonal velocity vel
         else
            vel = 0.0
         endif
         u(i) = vel
         v(i) = 0.0
      enddo

      return
      end

C==============================================================================
c given planetocentric latitude (rlat), compute zonal velocity (u) from zonal
c velocity profile.
C==============================================================================
      subroutine zonal(rlat,u)
      IMPLICIT NONE
      real*8 rlat,u

      common/zvp/nz,zvp(2,1000)
      integer nz
      real zvp

      integer i, Start, End

      IF (rlat.eq.-999.0 .OR. rlat.lt.zvp(1,1) .OR. rlat.gt.zvp(1,nz)) 
     +   THEN
        goto 990
      END IF
c     ....search so that  zvp(i) < rlat < zvp(i+1)
      Start = 1  ! Set the beginning of the range to first index
      End = nz   ! Set the end of the range to the last index

      ! Binary Search for the nearest rlat range
   10 i = Start + (End-Start)/2
        IF (Start .GT. End) THEN
          GOTO 990
        ELSE IF (rlat.ge.zvp(1,i).AND.rlat.le.zvp(1,i+1)) THEN
          GOTO 20
        ELSE IF (rlat.gt.zvp(1,i)) THEN
          Start = i + 1
        ELSE IF (rlat.lt.zvp(1,i)) THEN
          End = i - 1
        ELSE
          CALL xvmessage('Either the ZVP file is bad or', ' ')
          CALL mabend('Binary searching algorithm failed', ' ')
        END IF
      GOTO 10

c     ....interpolate between points
   20 u = zvp(2,i) + (zvp(2,i+1)-zvp(2,i))*(rlat-zvp(1,i))/
     &		(zvp(1,i+1)-zvp(1,i))
      return
c     ....all latitudes outside range of table are set to zero
  990 u = 0.0
      return
      end

C==============================================================================
c given velocities u and v, predict the lat-lon positions of
c each tiepoint after time deltat.
c updated: coords
C==============================================================================
      subroutine predict(npts,u,v,deltat,re,rp,coords)
      IMPLICIT NONE
      integer npts
      real*4 u(npts),v(npts),coords(2,npts)
      real*8 deltat,re,rp

      real*8 e2,epsln,p,rlat1,rlon1,rlat2,rlon2,phi0,phi1,phi2,avglat,
     +       q,r
      integer i,loop

      e2 = 1 - (rp/re)**2		!square of eccentricity
      epsln = (re/rp)**2
      p = 1000.d0*re*(1-e2)
c
      do 20 i=1,npts
      rlat1 = coords(1,i)
      rlon1 = coords(2,i)
      if (rlat1.eq.-999.) goto 20
      phi1 = datan(epsln*dtan(rlat1))		!planetographic latitude
      phi0 = phi1
      loop = 0
c     ....iteratively solve for new latitude
   15 avglat = 0.5d0*(phi0+phi1)
      q = 1 - e2*dsin(avglat)**2
      r = p/q**1.5				!radius of curvature
      phi2 = phi1 + v(i)*deltat/r
      if (dabs(phi2-phi0).lt.1.d-05) goto 16
      if (loop.gt.100) goto 20
      phi0 = phi2
      loop = loop + 1
      goto 15

   16 rlat2 = datan(dtan(phi2)/epsln)		!planetocentric latitude
      rlon2 = rlon1 + (dsqrt(q)/(1000.d0*re*dcos(avglat)))*u(i)*deltat
      coords(1,i) = rlat2
      coords(2,i) = rlon2
   20 continue

      return
      end

C==============================================================================
c compute displacements dl,ds.
C==============================================================================
      subroutine fit2(ipic,coords,pts,npts,dl,ds,*)
      IMPLICIT NONE
      integer ipic,npts
      real*4 coords(2,npts),pts(2,npts)
      real*8 rlat, rlon
      real*8 dl,ds

      integer n,i,ind
      real*8 rline,rsamp

      dl = 0.
      ds = 0.
      n = 0

      do 50 i=1,npts
	print *, 'COORDS(1,',i,'): ', coords(1, i)
        if (coords(1,i).eq.-999.) goto 50
        rlat = coords(1,i)
        rlon = coords(2,i)
        call linsam1(ipic,rlat,rlon,rline,rsamp,ind)
        if (ind.eq.0) goto 50

        write(*,*)'Converted (line,samp) = ',rline,rsamp

        n = n + 1
        dl = dl + pts(1,i) - rline
        ds = ds + pts(2,i) - rsamp
   50 continue   

      if (n.eq.0) then
          call xvmessage(' ***Fit error.  No valid points',' ')
          return1
      endif

      dl = dl/n
      ds = ds/n
      return
      end

C==============================================================================
c given lat-lon coordinates coords and line-samp coords pts
c compute displacements dl,ds, and dt.
c return1 on fit error.
C==============================================================================
      subroutine fit3(ipic,coords,pts,npts,oal,oas,dl,ds,dt,*)
      IMPLICIT NONE
      integer ipic, npts
      real*4 coords(2,npts),pts(2,npts)
      real*8 oal,oas,dl,ds,dt

      integer n, i, ind
      real*8 a13,a23,a33,b1,b2,b3,u,v,det,rlat,rlon,rline,rsamp

      a13 = 0.d0
      a23 = 0.d0
      a33 = 0.d0
      b1 = 0.d0
      b2 = 0.d0
      b3 = 0.d0
      n = 0

      do 50 i=1,npts
        rlat = coords(1,i)
        rlon = coords(2,i)
        if (rlat.eq.-999.) goto 50
        call linsam1(ipic,rlat,rlon,rline,rsamp,ind)
        if (ind.eq.0) goto 50
        n = n + 1
        dl = pts(1,i) - rline
        ds = pts(2,i) - rsamp
        v = oal - pts(1,i)
        u = pts(2,i) - oas
        a13 = a13 + u
        a23 = a23 + v
        a33 = a33 + u**2 + v**2
        b1 = b1 + dl
        b2 = b2 + ds
        b3 = b3 + u*dl + v*ds
   50 continue

      if (n.lt.2) then
          call xvmessage(' ***Fit error.  Insufficient points',' ')
          return1
      endif

      det = n*(n*a33-a13**2-a23**2)
      if (det.eq.0.0) then
          call xvmessage(' ***Fit3 error.',' ')
          return1
      endif

      dl = (n*(b1*a33-b3*a13) + a23*(b2*a13-b1*a23))/det
      ds = (n*(b2*a33-b3*a23) + a13*(b1*a23-b2*a13))/det
      dt = (n*(n*b3-b1*a13-b2*a23))/det
      return
      end

C==============================================================================
c compute rms error between computed points (coords) and actual
c points (pts).
C==============================================================================
      subroutine rmserr(npts,coords,pts)
      IMPLICIT NONE
      integer npts
      real*4 coords(2,npts),pts(2,npts)

      character*80 msg
      integer m, i, ind,imax
      real*8 errmax,rms,rlat,rlon,rline,rsamp,error
  116 format(' ***PT',i4,' is not visible in LEFT frame')
  119 format(' ',i3,' points used in fit')
  120 format(' RMS error=',f5.1,' pixels')
  121 format(' PT',i4,' has maximum error of',f5.1,' pixels')

      errmax = -1.d0
      rms = 0.d0
      m = 0

      do 100 i=1,npts
      rlat = coords(1,i)
      rlon = coords(2,i)
      if (rlat.eq.-999.) goto 100
      call linsam1(1,rlat,rlon,rline,rsamp,ind)
      if (ind.eq.0) then
           write(msg,116) i
           call xvmessage(msg,' ')
           goto 100
      endif
      m = m + 1
      error = (pts(1,i)-rline)**2 + (pts(2,i)-rsamp)**2
      rms = rms + error
      if (error.gt.errmax) then
           errmax=error
           imax = i
      endif
  100 continue

      write(msg,119) m
      call xvmessage(msg,' ')
      if (m.gt.0) rms=dsqrt(rms/m)
      write(msg,120) rms
      call xvmessage(msg,' ')
      errmax = dsqrt(errmax)
      write(msg,121) imax,errmax
      call xvmessage(msg,' ')
      return
      end

C==============================================================================
c given the me-matrix for a target and the spacecraft vector expressed in
c celestial coordinates, express the vector in target centered (x3,y3,z3)
c coordinates.
c
c outputs: psc3,sclat,sclon
C==============================================================================
      subroutine scvector2(me,psc,rsc,psc3,sclat,sclon)
      IMPLICIT NONE
      real*8 me(3,3),psc(3),rsc,psc3(3),sclat,sclon

      real*8 dot

      sclat = dasin(dot(psc,me(1,3)))	!sclat = psc o n
      sclon = datan2(dot(psc,me(1,2)),dot(psc,me(1,1)))
c             change spacecraft vector to (x3,y3,z3) coordinates
      psc3(1) = rsc*dcos(sclat)
      psc3(2) = 0.d0
      psc3(3) = rsc*dsin(sclat)
      return
      end

C==============================================================================
c calculates angln, angla, and anglb from the c-matrix, spin vector,
c and spacecraft vector.
c inputs: cm,n,psc
c outputs: angln,angla,anglb
C==============================================================================
      subroutine getangles(cm,n,psc,angln,angla,anglb)
      IMPLICIT NONE
      real*8 cm(3,3),n(3),psc(3),angln,angla,anglb

      real*8 n0(3),p0(3),p1(3),nx,nz,px,py
      integer i
c
c        rotate spin vector n0 and s/c vector p0 into image space by 
c        multiplying by cm-inverse.
      do i=1,3
      n0(i) = cm(1,i)*n(1) + cm(2,i)*n(2) + cm(3,i)*n(3)
      p0(i) = cm(1,i)*psc(1) + cm(2,i)*psc(2) + cm(3,i)*psc(3)
      enddo
c
c        rotate spin vector thru angln so that north is along x-axis
      angln = datan2(n0(2),n0(1))	! compute angle n
      nx = n0(1)*dcos(angln) + n0(2)*dsin(angln)
      nz = n0(3)
c         rotate s/c vector thru angles n and a
      angla = datan2(nx,nz)             ! compute angle a
      p1(1) =  p0(1)*dcos(angln) + p0(2)*dsin(angln)
      p1(2) = -p0(1)*dsin(angln) + p0(2)*dcos(angln)
      p1(3) =  p0(3)
      px = dcos(angla)*p1(1) - dsin(angla)*p1(3)
      py = p1(2)
      anglb = datan2(py,px)		! compute angle b
      return
      end

C==============================================================================
c routine to calculate the camera-to-planet rotation matrix (om)
c from angles angln, angla, and anglb.
C==============================================================================
      subroutine ommatrix(angln,angla,anglb,om)
      IMPLICIT NONE
      real*8 angln,angla,anglb,om(3,3)

      integer j
      real*8 temp

c        the om-matrix is initially matrix m1 (north angle rotation about
c        z0-axis).
      om(1,1) =  dcos(angln)
      om(2,1) = -dsin(angln)
      om(3,1) =  0.d0
c
      om(1,2) =  dsin(angln)
      om(2,2) =  dcos(angln)
      om(3,2) =  0.d0
c
      om(1,3) =  0.d0
      om(2,3) =  0.d0
      om(3,3) =  1.d0
c        om = m2*m1 (rotate about y1-axis through angle a)
      do 20 j=1,3
      temp    = dcos(angla)*om(1,j) - dsin(angla)*om(3,j)
      om(3,j) = dsin(angla)*om(1,j) + dcos(angla)*om(3,j)
   20 om(1,j) = temp
c        om = m3*m2*m1 (rotate about z2-axis through angle b)
      do 30 j=1,3
      temp    =  dcos(anglb)*om(1,j) + dsin(anglb)*om(2,j)
      om(2,j) = -dsin(anglb)*om(1,j) + dcos(anglb)*om(2,j)
   30 om(1,j) = temp
c
      return
      end

C==============================================================================
c compute the c-matrix from me and om' matrices...
c output: cm
C==============================================================================
      subroutine cmatrix(me,angln,angla,anglb,sclon,cm)
      IMPLICIT NONE
      real*8 me(3,3),angln,angla,anglb,sclon,cm(3,3)

      real*8 omp(3,3)
      integer i, j

      call ommatrix(angln,angla,anglb-sclon,omp)  !compute om' matrix

      do 150 j=1,3
      do 150 i=1,3
  150 cm(i,j) = me(i,1)*omp(1,j)+me(i,2)*omp(2,j)+me(i,3)*omp(3,j)

      return
      end

C==============================================================================
c given displacements dl,ds, and dt, update the c and om matrices.
C==============================================================================
      subroutine updatenav(ipic,ifit,dl,ds,dt,scline,scsamp)
      IMPLICIT NONE
      integer ipic,ifit
      real*8 dl,ds,dt,scline,scsamp

      include 'cmap.fin'

      integer ind

      call getnav2(ipic)
      call move1(dl,ds,angln,angla,anglb,zscale)!update angla and anglb
      if (ifit.eq.3) angln=angln+dt			!update angln
      call ommatrix(angln,angla,anglb,om)		!om matrix
      call cmatrix(me,angln,angla,anglb,sclon,cm)	!c matrix
      call linsam1(ipic,sclat,sclon,scline,scsamp,ind)	!planet center
      call printmtx(om,3)
      call putnav2(ipic)
      return
      end

C==============================================================================
c given a displacement of the optical axis of (dl,ds) in the image plane,
c computes the resulting changes in angla and anglb.
c
c inputs: dl,ds,angln,zscale
c updated: angla,anglb
C==============================================================================
      subroutine move1(dl,ds,angln,angla,anglb,zscale)
      IMPLICIT NONE
      real*8 dl,ds,angln,angla,anglb,zscale

      real*8 scale,dux,duy,da,db

      scale = dsqrt(zscale**2+dl**2+ds**2)
      dux = ds/scale
      duy = dl/scale
      da = dux*dcos(angln) + duy*dsin(angln)
      db = (dux*dsin(angln) - duy*dcos(angln))/dsin(angla)
      angla = angla + da
      anglb = anglb + db
      return
      end

C==============================================================================
c routines to save and restore frame identifiers and navigation data...
c the current data for the left and right frames are saved in pnav, rnav,
c and snav.  the original sedr data is saved in pnav0, rnav0, and snav0.
c
c input: ipic=1 for left image, =2 for right image
C==============================================================================
      subroutine getnav2(ipic)
      IMPLICIT NONE
      integer ipic

      include 'cmap.fin'
      include 'cpic.fin'

      common/cnav_sedr/rnav(20,2)
      real*8 rnav
      common/cnav_fit/rnav0(20,2)
      real*8 rnav0
      common/cnav/snav(25,2),snav0(25,2)
      common/cnav/pnav(7,2),pnav0(7,2)
      common/cnav/qnav(3,2),qnav0(3,2)
      real*8 snav,snav0,qnav,qnav0
      integer*4 pnav, pnav0

c     ....move current data to cmap
      call mve(4,7,pnav(1,ipic),project_id, 1, 1) 
      call mve(8,3,qnav(1,ipic),re, 1, 1)
      call mve(8,20,rnav(1,ipic),fl, 1, 1)
      call mve(8,25,snav(1,ipic),cm, 1, 1)
      return

c     ....move original sedr data to cmap
      entry getnav20(ipic)
      call mve(4,7,pnav0(1,ipic),project_id, 1, 1)
      call mve(8,3,qnav0(1,ipic),re, 1, 1)
      call mve(8,20,rnav0(1,ipic),fl, 1, 1)
      call mve(8,25,snav0(1,ipic),cm, 1, 1)
      return

c     ....replace current data with cmap
      entry putnav2(ipic)
      call mve(4,7,project_id,pnav(1,ipic), 1, 1)
      call mve(8,3,re,qnav(1,ipic), 1, 1)
      call mve(8,20,fl,rnav(1,ipic), 1, 1)
      call mve(8,25,cm,snav(1,ipic), 1, 1)
      return

c     ....replace original sedr data with cmap
      entry putnav20(ipic)
      call mve(4,7,project_id,pnav0(1,ipic), 1, 1)
      call mve(8,3,re,qnav0(1,ipic), 1, 1)
      call mve(8,20,fl,rnav0(1,ipic), 1, 1)
      call mve(8,25,cm,snav0(1,ipic), 1, 1)
      return
      end
