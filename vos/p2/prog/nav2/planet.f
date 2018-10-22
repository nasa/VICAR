C==============================================================================
c transforms from (line,sample) to (latitude,longitude) for a
c perspective projection camera system.  an oblate spheroid model of the
c planet (re,rp) is used.  all angles are in radians.  planetocentric
c latitudes and east-longitudes are used.
c
c inputs:
c     rline,rsamp   :  input image coordinates
c     om	    :  camera to planet transformation matrix
c     psc3          :  spacecraft vector in (x3,y3,z3) coordinates, where
c		    :     psc3(1) = rsc*dcos(sclat)
c		    :     psc3(2) = 0.0d0
c		    :     psc3(3) = rsc*dsin(sclat)
c     sclon         :  spacecraft longitude
c     re            :  planet's equatorial radius (km)
c     epsln	    :  planet eccentricity = (re/rp)**2
c     epsln2        :  constant = psc3(1)**2 + epsln*psc3(3)**2 - re**2
c     oal,oas       :  image coordinates of optical axis intercept point
c     zscale        :  camera constant = fl*pscale
c
c outputs:
c     ind           :	ind=1 if resulting (rlat,rlon) are valid
c		    :	ind=0 if point is off the planet
c     rlat,rlon     :  computed planet coordinates.
C==============================================================================
      subroutine planet(ind,rline,rsamp,rlat,rlon,
     &       om,psc3,sclon,re,epsln,epsln2,oal,oas,zscale)
      IMPLICIT NONE
      integer ind
      real*8 rline,rsamp,rlat,rlon,om(3,3),psc3(3),re,epsln,epsln2,
     +       oal,oas,zscale

      real*8 sclon,ux0,uy0,uz0,ux3,uy3,uz3,a,b,d,r,x,y,z
      real*8 pi/3.141592653589793d0/

c         vector from spacecraft to point on planet in camera coordinate system
      ux0 = rsamp - oas
      uy0 = rline - oal
      uz0 = zscale
c         convert vector to planet coordinate system (x3,y3,z3)
      ux3 = om(1,1)*ux0 + om(1,2)*uy0 + om(1,3)*uz0
      uy3 = om(2,1)*ux0 + om(2,2)*uy0 + om(2,3)*uz0
      uz3 = om(3,1)*ux0 + om(3,2)*uy0 + om(3,3)*uz0
c          find where vector intersects planet
      a = ux3**2      + uy3**2 + epsln*uz3**2
      b = ux3*psc3(1)          + epsln*uz3*psc3(3)
      d = b*b - a*epsln2
      if(d.lt.0.) then
          ind = 0		!point off the planet
          return
      endif
      r = (-b-dsqrt(d))/a	!choose smaller root for point in front
c
      x = r*ux3 + psc3(1)
      y = r*uy3
      z = r*uz3 + psc3(3)
      d = dsqrt(x**2+y**2)
      rlat = datan2(z,d)
      rlon = dmod(datan2(y,x)+sclon+2.d0*pi,2.d0*pi)
      ind = 1
      return
      end

C==============================================================================
c transforms from (latitude,longitude) to (line,sample) in a
c perspective projection camera system.  an oblate spheroid model of the
c planet (re,rp) is used.  all angles are in radians.  planetocentric
c latitudes and east-longitudes are used.
c
c inputs:
c     rlat,rlon     :  input planet coordinates.
c     om	    :  camera to planet transformation matrix
c     psc3          :  spacecraft vector in (x3,y3,z3) coordinates, where
c		    :     psc3(1) = rsc*dcos(sclat)
c		    :     psc3(2) = 0.0d0
c		    :     psc3(3) = rsc*dsin(sclat)
c     sclon         :  spacecraft longitude
c     re            :  planet's equatorial radius (km)
c     epsln	    :  planet eccentricity = (re/rp)**2
c     epsln2        :  constant = psc3(1)**2 + epsln*psc3(3)**2 - re**2
c     oal,oas       :  image coordinates of optical axis intercept point
c     zscale        :  camera constant = fl*pscale
c
c outputs:
c     ind           :  ind=1 if resulting (rline,rsamp) are valid
c                   :  ind=0 if point is behind the planet
c     rline,rsamp   :  computed image coordinates
C==============================================================================
      subroutine plainv(ind,rlat,rlon,rline,rsamp,
     &       om,psc3,sclon,re,epsln,epsln2,oal,oas,zscale)
      IMPLICIT NONE
      integer ind
      real*8 rlat,rlon,rline,rsamp,om(3,3),psc3(3),sclon,re,
     +       epsln,epsln2,oal,oas,zscale

      real*8 clat,r,ux3,uy3,uz3,a,b,d,ux0,uy0,uz0,s

      clat = dcos(rlat)
      r = re/dsqrt((1-epsln)*clat**2+epsln)	!geocentric radius
c          compute vector from camera to point on planet
      ux3 =  r*clat*dcos(rlon-sclon) - psc3(1)
      uy3 =  r*clat*dsin(rlon-sclon)
      uz3 =  r*dsin(rlat)            - psc3(3)
c          back-of-planet test
      a = ux3**2      + uy3**2 + epsln*uz3**2
      b = ux3*psc3(1)          + epsln*uz3*psc3(3)
      d = b*b - a*epsln2
      if(d.gt.0.) then
          r = (-b-dsqrt(d))/a
          if (r.lt.0.99999d0) then
               ind = 0			!point behind the planet
               return
          endif
      endif
c          rotate vector into camera coordinates
      ux0 = om(1,1)*ux3 + om(2,1)*uy3 + om(3,1)*uz3
      uy0 = om(1,2)*ux3 + om(2,2)*uy3 + om(3,2)*uz3
      uz0 = om(1,3)*ux3 + om(2,3)*uy3 + om(3,3)*uz3
c          scale vector into pixels
      s = zscale/uz0
      rline = s*uy0 + oal
      rsamp = s*ux0 + oas
      ind = 1
      return
      end

C==============================================================================
c convert from (line,sample) to (lat,lon) and vice versa.
C==============================================================================
      subroutine latlon1(ipic,rline,rsamp,rlat,rlon,ind)
      IMPLICIT NONE
      integer ipic,ind
      real*8 rline,rsamp,rlat,rlon

      include 'cmap.fin'

      common/cnav_sedr/fl1,oal1,oas1,pscale1,zscale1
      common/cnav_sedr/om1(3,3),psc31(3),sclat1,sclon1,epsln21
      common/cnav_sedr/fl2,oal2,oas2,pscale2,zscale2
      common/cnav_sedr/om2(3,3),psc32(3),sclat2,sclon2,epsln22
      real*8 fl1,oal1,oas1,pscale1,zscale1,om1,psc31,sclat1,sclon1,
     +       epsln21,fl2,oal2,oas2,pscale2,zscale2,om2,psc32,sclat2,
     +       sclon2,epsln22

c           convert from (line,samp) to (lat,lon)
      if (ipic.eq.1) then
           call planet(ind,rline,rsamp,rlat,rlon,
     &        om1,psc31,sclon1,re,epsln,epsln21,oal1,oas1,zscale1)
      else
           call planet(ind,rline,rsamp,rlat,rlon,
     &        om2,psc32,sclon2,re,epsln,epsln22,oal2,oas2,zscale2)
      endif
      return

      entry linsam1(ipic,rlat,rlon,rline,rsamp,ind)
c           convert from (lat,lon) to (line,samp)
      if (ipic.eq.1) then
           call plainv(ind,rlat,rlon,rline,rsamp,
     &            om1,psc31,sclon1,re,epsln,epsln21,oal1,oas1,zscale1)
      else
           call plainv(ind,rlat,rlon,rline,rsamp,
     &            om2,psc32,sclon2,re,epsln,epsln22,oal2,oas2,zscale2)
      endif

      return
      end

C==============================================================================
c convert from (line,sample) to (lat,lon) and vice versa.  this version
c also converts from image-space to object-space and vice versa, as
c required.
C==============================================================================
      subroutine latlon2(ipic,rline,rsamp,rlat,rlon,ind)
      IMPLICIT NONE
      integer ipic,ind
      real*8 rline,rsamp,rlat,rlon

      include 'cmap.fin'
      include 'cpic.fin'

      common/cnav_sedr/fl1,oal1,oas1,pscale1,zscale1
      common/cnav_sedr/om1(3,3),psc31(3),sclat1,sclon1,epsln21
      common/cnav_sedr/fl2,oal2,oas2,pscale2,zscale2
      common/cnav_sedr/om2(3,3),psc32(3),sclat2,sclon2,epsln22
      real*8 fl1,oal1,oas1,pscale1,zscale1,om1,psc31,sclat1,sclon1,
     +       epsln21,fl2,oal2,oas2,pscale2,zscale2,om2,psc32,sclat2,
     +       sclon2,epsln22

      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project

      real*4 rl,rs
      real*8 r8rl,r8rs

      rl = rline
      rs = rsamp
      if (itype(ipic).eq.7) call convisos(project(ipic),camera_id,
     &     rl,rs,rl,rs,1,conv(1,ipic),nph(ipic),npv(ipic),ind)
c           convert from (line,samp) to (lat,lon)
      if (ipic.eq.1) then
           r8rl = rl
           r8rs = rs
           call planet(ind,r8rl,r8rs,rlat,rlon,
     &        om1,psc31,sclon1,re,epsln,epsln21,oal1,oas1,zscale1)
      else
           r8rl = rl
           r8rs = rs
           call planet(ind,r8rl,r8rs,rlat,rlon,
     &        om2,psc32,sclon2,re,epsln,epsln22,oal2,oas2,zscale2)
      endif
      return

      entry linsam2(ipic,rlat,rlon,rline,rsamp,ind)
c           convert from (lat,lon) to (line,samp)
      if (ipic.eq.1) then
           call plainv(ind,rlat,rlon,rline,rsamp,
     &            om1,psc31,sclon1,re,epsln,epsln21,oal1,oas1,zscale1)
      else
           call plainv(ind,rlat,rlon,rline,rsamp,
     &            om2,psc32,sclon2,re,epsln,epsln22,oal2,oas2,zscale2)
      endif

      if (itype(ipic).eq.7) then
        rl=rline
        rs=rsamp
        call convisos(project(ipic),camera_id,rl,rs,rl,rs,0,
     &          conv(1,ipic),nph(ipic),npv(ipic),ind)
        rline = rl
        rsamp = rs
      end if
 
      ind = 1
      return
      end

C==============================================================================
c routine to convert input latitude (rlat) to planetocentric if igeo=1
C==============================================================================
      real*8 function geocen(rlat)
      IMPLICIT NONE
      real*8 rlat

      include 'cmap.fin'
      include 'const.fin'

      if (dabs(rlat).ge.pi/2.d0) then
           geocen = dsign(pi/2.d0,rlat)
           return
      endif

      if (igeo.ne.1) then
         geocen = rlat
      else
         geocen = datan(dtan(rlat)/epsln)
      endif
      return
      end

C==============================================================================
c routine to convert input latitude (rlat) to planetodetic if igeo=1
C==============================================================================
      real*8 function geodet(rlat)
      IMPLICIT NONE
      real*8 rlat

      include 'cmap.fin'
      include 'const.fin'

      if (dabs(rlat).ge.pi/2.d0) then
           geodet = dsign(pi/2.d0,rlat)
           return
      endif

      if (igeo.ne.1) then
         geodet = rlat
      else
         geodet = datan(dtan(rlat)*epsln)
      endif
      return
      end

C==============================================================================
c get camera distortion parameters.
c mode=1 if res file directory has already been read.
C==============================================================================
      subroutine getgeopar(project_id,camera_id,frame_id,planet_id,
     &		conv,nph,npv,mode,*)
      IMPLICIT NONE
      integer*4 project_id,camera_id,frame_id,planet_id,nph,npv,mode
      real*4 conv(2216)			!camera distortion parameters

      save
      character *6 format(409) /5*'FULL',404*'REAL'/
      integer status,nrows,ibis

      integer iunitr,jdata(2)
      real*4 rloc(2,202)

      logical parmtst
      integer icnt,idef,ind,icount,iret,mpv
      character*24 rfnames(4)/
     &		'/project/vgr/resj.fil',	!jupiter
     &		'/project/vgr/ress.fil',	!saturn
     &		'/project/vgr/resu.fil',	!uranus
     &		'/project/vgr/resn.fil'/	!neptune
      character*256 rfname

      integer ibis_file_get

  101 format(' ***Reseau locations for frame',i8,' not found')
      character*80 msg
c
      if (project_id.ne.4) return	!only implemented for vgr
c     ...find and open reseau location file
      if (mode.eq.0) then
         call xvparm('RES',rfname,icnt,idef,1)	!get file name
         if (idef.eq.1) rfname=rfnames(planet_id-4)
         call xvunit(iunitr,'X',1,ind,'U_NAME',rfname,' ')
         call xvsignal(iunitr,ind,.true.)
         call ibis_file_open(iunitr,ibis,'read',409,99999,
     +                        format,0,status)
         if ( status .ne. 1 ) call ibis_signal_u(iunitr,status,1)
         icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
         if ( nrows .lt. 0 ) call ibis_signal(ibis,icount,1)
         mode = 1
      endif
c     ....get reseau locations
      jdata(1) = frame_id
      jdata(2) = camera_id
      call getlocv2(ibis,nrows,jdata,rloc,iret)
      if (iret.eq.0) goto 30	!skip if successful

      write(msg,101) frame_id 
      call xvmessage(msg,' ')
      call xvmessage('Do you wish to use nominal reseau locations?',' ')
   20 call xvintract('QUERY',' Enter Y or N')
      if (parmtst('EXIT', mpv, 1)) goto 990
      if (parmtst('N', mpv, 1)) goto 990
      call xvmessage('Nominal reseau locations used',' ')
      call getres(rloc,camera_id)

c     ....get geom parameters
   30 call geomav(conv,camera_id,rloc)
      nph = 24
      npv = 23
      call mve(7,2208,conv(9),conv,1,1)  !move so buffer begins with tiepoints
      return
c     ...error conditions
  990 call xvmessage(' ***Use program RESLOC to locate the reseau',' ')
      return1
      end

C==============================================================================
c read old tiepoints from third input file...
C==============================================================================
      subroutine getpts(*)
      IMPLICIT NONE

      include 'cpts.fin'

      common/c1_iobuf/buf(500,10)		!scratch space for i/o
      real*4 buf

      integer iunit,status,ncol,count,ibis_in,nrow,icol,i
      character*80 msg

      integer ibis_file_get

      call xvunit(iunit, 'inp', 3, status, ' ')
      call ibis_file_open(iunit, ibis_in, 'read', 0, 0,' ',' ', status)
      if (status .ne. 1) call ibis_signal_u(iunit, status, 1)
      count = ibis_file_get(ibis_in, 'nc', ncol, 1, 1)
      if (count .lt. 0) call ibis_signal(ibis_in, count, 1)
      count = ibis_file_get(ibis_in, 'nr', nrow, 1, 1)
      if (count .lt. 0) call ibis_signal(ibis_in, count, 1)

      if (ncol.ne.10) then
          call xvmessage(' ***Invalid input tiepoint file format',' ')
          return1
      endif

      do icol=1,ncol
         call ibis_column_read(ibis_in, buf(1, icol), icol, 1, nrow, 
     +                         status)
         if (status .ne. 1) call ibis_signal(ibis_in, status, 1)
	 
      enddo

      npts = nrow 
c
c     ....move data to tiepoints area
      do i=1,nrow
         lpt(1,i) = buf(i,1)
         lpt(2,i) = buf(i,2)
         rpt(1,i) = buf(i,3)
         rpt(2,i) = buf(i,4)
         lpt0(1,i) = buf(i,5)
         lpt0(2,i) = buf(i,6)
         zl(i) = buf(i,7)
         zr(i) = buf(i,8)
         cmax(i) = buf(i,9)
      enddo

      call ibis_file_close(ibis_in, ' ', status)
      if (status .ne. 1) call ibis_signal(ibis_in, status, 1)
      write(msg,110) npts
  110 format(i6,' tiepoints read from input tiepoint file')
      call xvmessage(msg,' ')
      return
      end

C==============================================================================
c write tiepoints to output ibis interface file.
C==============================================================================
      subroutine putpts(*)
      IMPLICIT NONE

      include 'cpts.fin'

      common/c1_iobuf/obuf(500,10)		!scratch space for i/o
      real*4 obuf

      integer ounit,status,ncol,ibis_out,nrow,i,n,icol

      if (npts.eq.0) return
      nrow = 0		!column length=number of tiepoints output
      do i=1,npts
         if (lpt(1,i).ge.0.0) nrow=nrow+1
      enddo

      ncol = 10
      call xvunit(ounit, 'out', 1, status, ' ')
      call ibis_file_open(ounit, ibis_out, 'write', ncol, nrow, ' ', 
     +' ', status)
      if (status .ne. 1) call ibis_signal_u(ounit, status, 1)
c
c     ....rearrange data for column sequential output
      n = 0
      DO i=1,npts
        IF (lpt(1,i).ge.0.0) THEN	!reject all flagged tiepoints
          n = n + 1
          obuf(n,1) = lpt(1,i)
          obuf(n,2) = lpt(2,i)
          obuf(n,3) = rpt(1,i)
          obuf(n,4) = rpt(2,i)
          obuf(n,5) = lpt0(1,i)
          obuf(n,6) = lpt0(2,i)
          obuf(n,7) = zl(i)
          obuf(n,8) = zr(i)
          obuf(n,9) = cmax(i)
          obuf(n,10) = i
        END IF
      END DO

      do icol=1,10
         call ibis_column_write(ibis_out, obuf(1, icol), icol, 1, nrow, 
     +status)
         if (status .ne. 1) call ibis_signal(ibis_out, status, 1)
      enddo

      call ibis_file_close(ibis_out, ' ', status)
      return
      end
