C==============================================================================
c routine to edit navigation data
C==============================================================================
      subroutine editnav(ipic)
      IMPLICIT NONE
      integer ipic

      include 'cmap.fin'
      include 'const.fin'
      include 'cpic.fin'

      common/cpar/par(20),par2(20)
      real*4 par,par2
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss

      common/sedr/sedr(200,2),lbuf(80,2),isystem,p_source
      character*4 p_source
      real*4 sedr
      integer lbuf,isystem
      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project

      logical*1 parmtst
      character*12 target_name
      character*80 msg
      real*4 r,r4fl,r4oal,r4oas,r4pscale,r4scline,r4scsamp,
     +       r4waline,r4wasamp
      real*8 scline,scsamp,waline,wasamp,geocen
      integer mpv,ind,ival

  100 format(' O.S. NA CENTER=(',f10.2,',',f10.2,')')

      call getnav2(ipic)

   20 call xvintract('EDIT','EDIT')
      if (parmtst('HELP', mpv, 1)) call xvintract('EDIT',' ')

      if (parmtst('EXIT', mpv, 1)) then
            call putnav2(ipic)
            return
      endif

      if (parmtst('GEOCEN', mpv, 1)) igeo=0
      if (parmtst('GEODET', mpv, 1)) igeo=1

      if (parmtst('PR',r, 2)) then	
         rp = r					! change polar radius
         epsln = (re/rp)**2			! update ellipticity constants
         epsln2 = psc3(1)**2 + epsln*psc3(3)**2 - re**2
      endif

      if (parmtst('ER',r, 2)) then
         re = r					! change equatorial radius
         epsln = (re/rp)**2			! update ellipticity constants
         epsln2 = psc3(1)**2 + epsln*psc3(3)**2 - re**2
      endif

      if (parmtst('FL',r, 2)) then			! change focal length
         fl = r
         zscale=pscale*fl
      endif

      if (parmtst('OAXIS',par, 2)) then		! change optical axis intercept
         oal = par(1)
         oas = par(2)
      endif

      if (parmtst('SC',r, 2)) then
         pscale=r				! change picture scale
         zscale=pscale*fl
      endif

      if (parmtst('RANGE',r, 2)) then
            rsc = r		 		! change spacecraft range
            call scvector2(me,psc,rsc,psc3,sclat,sclon)
            epsln2 = psc3(1)**2 + epsln*psc3(3)**2 - re**2
      endif

      if (parmtst('SSP',par, 2)) then		! change subspacecraft point
            sclat = geocen(par(1)*dtr)
            sclon = par(2)*dtr
c             compute s/c vector in target centered coordinates
            psc3(1) = dcos(sclat)*dcos(sclon)
            psc3(2) = dcos(sclat)*dsin(sclon)
            psc3(3) = dsin(sclat)
c             convert to eme50 coordinates
            psc(1)=me(1,1)*psc3(1)+me(1,2)*psc3(2)+me(1,3)*psc3(3)
            psc(2)=me(2,1)*psc3(1)+me(2,2)*psc3(2)+me(2,3)*psc3(3)
            psc(3)=me(3,1)*psc3(1)+me(3,2)*psc3(2)+me(3,3)*psc3(3)
            call scvector2(me,psc,rsc,psc3,sclat,sclon)
            call getangles(cm,me(1,3),psc,angln,angla,anglb)
            call ommatrix(angln,angla,anglb,om)
            epsln2 = psc3(1)**2 + epsln*psc3(3)**2 - re**2
      endif

      if (parmtst('PC',par, 2)) then		!change planet center
            scline = par(1)
            scsamp = par(2)			!update om and cm
            call farenc(sclat,sclon,scline,scsamp,pscale,fl,oal,oas,
     &              psc,me,cm,om,angln,angla,anglb)
            goto 20
      endif

      if (parmtst('ISPC',par, 2)) then		!change i.s. planet center
            if (itype(ipic).eq.8) then
               call xvmessage(' ***ISPC invalid for object-space frames'
     +                        ,' ')
               goto 20
            endif
            call convisos(project(ipic),camera_id,par(1),par(2),
     &           r4scline,r4scsamp,1,conv(1,ipic),nph(ipic),npv(ipic),
     &           ind)
            scline = r4scline
            scsamp = r4scsamp
            call farenc(sclat,sclon,scline,scsamp,pscale,fl,oal,oas,
     &              psc,me,cm,om,angln,angla,anglb)
            goto 20
      endif

      if (parmtst('WAPC',par, 2)) then	! change wa center to na center
         if (project_id.ne.4) then
            call xvmessage(' ***WAPC only valid for Voyager',' ')
            goto 20
         endif
         waline = par(1)
         wasamp = par(2)
	 call mwatna(camera_id,waline,wasamp,scline,scsamp,*999)
         call farenc(sclat,sclon,scline,scsamp,pscale,fl,oal,oas,
     &              psc,me,cm,om,angln,angla,anglb)
         if (dabs(scline).lt.999999..and.dabs(scsamp).lt.999999.) then
            write(msg,100) scline,scsamp
            call xvmessage(msg,' ')
         else
            call xvmessage(' O.S. NA CENTER=(*******.**,*******.**)',' '
     +                    )
         endif
      endif

      if (parmtst('WAISPC',par, 2)) then	! change wa center to na center
         if (project_id.ne.4) then
            call xvmessage(' ***WAISPC only valid for Voyager',' ')
            goto 20
         endif
         if (itype(ipic).eq.8) then
            call xvmessage(' ***WAISPC invalid for object-space frames',
     +                    ' ')
            goto 20
         endif
         call convisos(project(ipic),camera_id,par(1),par(2),r4waline,
     &          r4wasamp,1,conv(1,ipic),nph(ipic),npv(ipic),ind)
         waline = r4waline
         wasamp = r4wasamp
	 call mwatna(camera_id,waline,wasamp,scline,scsamp,*999)
         call farenc(sclat,sclon,scline,scsamp,pscale,fl,oal,oas,
     &              psc,me,cm,om,angln,angla,anglb)
         if (dabs(scline).lt.999999..and.dabs(scsamp).lt.999999.) then
            write (msg,100) scline,scsamp
            call xvmessage(msg,' ')
         else
            call xvmessage(' O.S. NA CENTER=(*******.**,*******.**)',' '
     +                    )
         endif
      endif

      if (parmtst('ANGLN',r, 2)) then		!change north angle
            angln = r*dtr
            call ommatrix(angln,angla,anglb,om) !update om and cm
            call cmatrix(me,angln,angla,anglb,sclon,cm)
      endif

      if (parmtst('CAMERA',ival, 2)) then    ! change camera s/n
         camera_id = ival
         call getcamcon(project(ipic),camera_id,r4fl,r4oal,r4oas,
     +                  r4pscale,ind)
         fl = r4fl
         oal = r4oal
         oas = r4oas
         pscale = r4pscale
         zscale = pscale*fl
      endif

      if (parmtst('STATUS', mpv, 1)) call pnav2(ipic)
      if (parmtst('SAVE', mpv, 1)) call putnav2(ipic)
      if (parmtst('RESTORE', mpv, 1)) call getnav2(ipic)
      if (parmtst('GETSEDR', mpv, 1)) call getnav20(ipic)

      if (parmtst('CKNAME',par, 2)) then
C         call mvl(par,p_source,4)
         call mve(-5, 1, par,p_source,1, 1)

         call spicesub(img(ipic), project(ipic), sedr(1,ipic),ind)

         if (ind.ne.0) goto 20
         call getangles(cm,me(1,3),psc,angln,angla,anglb)
         call ommatrix(angln,angla,anglb,om)
         call putnav2(ipic)
         call putnav20(ipic)
      endif

      if (parmtst('TARGET',target_name, 2)) then	! change target id
         call uprcase(target_name)
         call bfstr(target_name,12)
         call pbid(target_name,target_id,*20)
         planet_id = target_id
         if (planet_id.gt.9) planet_id=planet_id/100
         call pbdata(target_name,par2,*20)	! get planet radii
         re = par2(1)
         rp = par2(3)
         epsln = (re/rp)**2			! update ellipticity constants
         epsln2 = psc3(1)**2 + epsln*psc3(3)**2 - re**2
         goto 20
      endif

      goto 20

  999 return
      end

C==============================================================================
c given the planet center (scline,scsamp), the ssp (lat,lon), and the
c north angle angln, calculate angla, anglb, cm, and om.
c
c outputs: angla,anglb,cm,om
c note that angln is also recalculated and may differ due to round off errs...
C==============================================================================
      subroutine farenc(sclat,sclon,scline,scsamp,pscale,fl,oal,oas,
     &             psc,me,cm,om,angln,angla,anglb)
      IMPLICIT NONE
      real*8 sclat,sclon,scline,scsamp,pscale,fl,oal,oas,psc(3),
     +       me(3,3),cm(3,3),om(3,3),angln,angla,anglb

      include 'const.fin'

      real*8 omp(3,3),rs(3),scla,sclo,angn
      integer i,j

c            calculate omp transpose...
      scla = sclat*rtd
      sclo = (2.d0*pi-sclon)*rtd
      angn = angln*rtd + 90.d0	!ipl north angle
      call momati(oal,oas,scline,scsamp,pscale,fl,sclo,scla,angn,
     &       0.d0,omp,rs)	!note the rs is ignored
c            calculate c-matrix...
      do 20 j=1,3
      do 20 i=1,3
   20 cm(i,j) = me(i,1)*omp(j,1)+me(i,2)*omp(j,2)+me(i,3)*omp(j,3)
c            compute angles n, a, and b...
      call getangles(cm,me(1,3),psc,angln,angla,anglb)
      call ommatrix(angln,angla,anglb,om)	!compute om-matrix
      return
      end

C==============================================================================
c routine to print summary of navigation data
C==============================================================================
      subroutine pnav2(ipic)
      IMPLICIT NONE
      integer ipic

      include 'cmap.fin'
      include 'const.fin'
      include 'cpic.fin'

      common/sedr/sedr(200,2),lbuf(80,2),isystem,p_source
      character*4 p_source
      real*4 sedr
      integer lbuf,isystem
      common/distor/itype(2),conv(2216,2),nph(2),npv(2),project(2)
      real*4 conv
      integer nph, npv, itype
      character*5 project

      character*12 target_name
      character*80 msg
      integer irange,ind,isource
      real*4 r4scline,r4scsamp
      real*8 rlat,rlon,scline,scsamp
 
      real*8 geodet

  100 format(' NAVIGATION DATA FOR FRAME ',i9)
  101 format(' S/C EVENT TIME (yyddd hhmmssmmm)    SCET  ',i5,i10)
  102 format(' TARGET BODY                         TARG  ',a8)
  103 format(' POLAR RADIUS (km)                   PR    ',f8.1)
  104 format(' EQUATORIAL RADIUS (km)              ER    ',f8.1)
  108 format(' SPACECRAFT RANGE (km)               RANG  ',i10)
  109 format(' SPACECRAFT POSITION (lat,lon)       SSP   ',
     & '(',f6.2,',',f7.2,')')
  110 format(' O.S. planet center (line,sample)    PC    ',
     & '(',f10.2,',',f10.2,')')
  111 format(' O.S. planet center (line,sample)    PC    ',
     & '(******.**,******.**)')
  112 format(' I.S. planet center (line,sample)    ISPC  ',
     & '(',f10.2,',',f10.2,')')
  113 format(' I.S. planet center (line,sample)    ISPC  ',
     & '(******.**,******.**)')
  115 format(' NORTH ANGLE (measured from right)   ANGLN ',f7.2)
  116 format(' CAMERA SERIAL NUMBER                CAM   ',i8)
  117 format(' FOCAL LENGTH (mm)                   FL    ',f8.3)
  118 format(' OPTICAL AXIS (LINE,SAMPLE)          OAXIS ',
     & '(',f6.1,',',f6.1,')')
  120 format(' SCALE (pixels/mm at focal plane)    SC    ',f8.4)

      write(msg,100) frame_id
      call xvmessage(msg,' ')

      write(msg,101) idate,itime
      call xvmessage(msg,' ')

      call pbname(target_id,target_name,*10)
      write(msg,102) target_name
      call xvmessage(msg,' ')

   10 write(msg,103) rp
      call xvmessage(msg,' ')

      write(msg,104) re
      call xvmessage(msg,' ')

      irange = rsc + 0.5
      write(msg,108) irange
      call xvmessage(msg,' ')

      rlat = geodet(sclat)*rtd
      rlon = sclon*rtd
      write(msg,109) rlat,rlon
      call xvmessage(msg,' ')

      call plainv(ind,sclat,sclon,scline,scsamp,om,psc3,sclon,
     &       re,epsln,epsln2,oal,oas,zscale)
      if (dabs(scline).gt.999999.) ind=0
      if (dabs(scsamp).gt.999999.) ind=0
      if (ind.eq.1) then
          write(msg,110) scline,scsamp
          call xvmessage(msg,' ')
          if (itype(ipic).eq.7) then
             r4scline = scline
             r4scsamp = scsamp
     	     call convisos(project(ipic),camera_id,r4scline,r4scsamp,
     &          r4scline,r4scsamp,0,conv(1,ipic),nph(ipic),npv(ipic),
     &          ind)
             write(msg,112) r4scline,r4scsamp
             call xvmessage(msg,' ')
          endif
      else
          write(msg,111)
          call xvmessage(msg,' ')
          if (itype(ipic).eq.7) then
              write(msg,113)
              call xvmessage(msg,' ')
          endif
      endif

      write(msg,115) angln*rtd
      call xvmessage(msg,' ')

      write(msg,116) camera_id
      call xvmessage(msg,' ')

      write(msg,117) fl
      call xvmessage(msg,' ')

      write(msg,118) oal,oas
      call xvmessage(msg,' ')

      pscale = zscale/fl
      write(msg,120) pscale
      call xvmessage(msg,' ')

      if (igeo.eq.0) then
          call xvmessage(' All latitudes are planetocentric',' ')
      else
          call xvmessage(' All latitudes are planetographic',' ')
      endif

      call cmsource(sedr,isource)
      return
      end
