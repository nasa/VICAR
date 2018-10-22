c ********************************************************************
c Get image/object space geometry and store in data buffer.
c
      subroutine os_parms(projx)
      implicit none
      integer projx

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      common/c3/rdata
      real rdata(40)
      real*8 data(20),om(3,3),rs(3)
      real fic,lo,so,pixpmm		!camera focal length, op-axis, scale
      equivalence (data,rdata),(om,data(1)),(rs,data(10)),
     * (rdata(27),fic),(rdata(28),lo),(rdata(29),so),(rdata(30),pixpmm)

      common/konst/rad
      real rad

      integer def,count,ind
      real far(9)
      real*8 ra,rb,rc,lora		!target-body constants
      real*8 sclat,sclon,rmag		!spacecraft lat,long and distance
      real*8 scline,scsamp,angln	!spacecraft line,samp,north angle
      real*8 gcr,v(3)

c get target radii...
      call target_parms(rdata(26),rdata(37),rdata(25),rdata(36))
      ra = rdata(26)
      rb = rdata(37)
      rc = rdata(25)
      lora = rdata(36)

c determine camera parameters...
      call camera_parms(project,projx,icam,fic,lo,so,pixpmm)

c determine position of spacecraft.....
      call rs_parms(rdata(31),rdata(32),rdata(38),rs)
      sclat = rdata(31)
      sclon = rdata(32)
      rmag = rdata(38)

c determine camera pointing (om matrix).....
      call xvparm('OMMATRIX',far,count,def,' ')
      if (def.eq.0) call mve(9,9,far,om,1,1)

c compute om matrix and rs vector using farenc or tiepoints mode.
      call farenc_mode(rdata,om,rs) 
      call tiepoints_mode(rdata,om,rs)

c compute s/c line, sample, and north angle from om and rs.
      call ellipse_radius(sclat,sclon,ra,rb,rc,lora,gcr)
      call latrec(gcr,-sclon/rad,sclat/rad,v)
      call ellipse_proj(om,rs,fic,lo,so,pixpmm,ra,rb,rc,v,
     &		scline,scsamp,ind)
      if (ind.ne.1) call mabend('Err in geometry of input image')
      angln = rad*datan2(om(2,3),om(1,3)) + 90.	!clockwise from up
      if (angln.gt.360.) angln=angln-360.
      rdata(33) = scline
      rdata(34) = scsamp
      rdata(35) = angln
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Return target radii.
c
      subroutine target_parms(ra,rb,rc,lora)
      implicit none
      real ra,rb,rc		!target radii
      real lora			!longitude of radius a

      integer count,def,idnum,ind
      real far(20)
      character*12 target

      if (ra.gt.0.) goto 10	!skip if already set by SPICE

      call xvparm('TARGET',target,count,def,' ')
      if (def.ne.0) goto 10
      call getplacon(target,idnum,far,ind)
      if (ind.eq.0) then         
         ra = far(1)		!equatorial radius long axis
         rb = far(2)		!equatorial radius short axis
         rc = far(3)		!polar radius
         lora = far(4)		!longitude of radius a
      endif

   10 call xvparm('RADII',far,count,def,' ')
      if (def.eq.0) then
         ra = far(1)
         rb = far(2)
         rc = far(3)
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get camera parameters.
c
      subroutine camera_parms(project,projx,icam,fic,lo,so,pixpmm)
      implicit none
      character*5 project
      integer projx,icam
      real fic,lo,so,pixpmm

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      real r
      integer ind,count,def
      real fl,oal,oas,scale	!values returned by getcamcon
      logical xvptst

      lo = nli/2.		!default optical axis intercepts
      so = nsi/2.		!center of image

      if (projx.ne.0) then
         call getcamcon(project,icam,fl,oal,oas,scale,ind)
         if (ind.eq.0) then
            fic = fl		!focal length (mm)
            lo = oal		!optical axis line
            so = oas		!optical axis samp
            pixpmm = scale	!object space scale (pixels/mm)
         else
            call prnt(4,1,ind,'GETCAMCON: bad ind=.')
         endif
      endif

      if (xvptst('SIPS')) then
         fic = 9753.6		!sips 24" at table mountain
         lo = 256.
         so = 256.
         pixpmm = 51.2
      endif

      if (xvptst('QUESTAR')) then
         fic=700.		!sips with questar 700mm optics
         lo=256.
         so=256.
         pixpmm=51.2
      endif

      call xvparm('FOCL',r,count,def,' ')
      if (def.eq.0) fic=r	!camera focal length

      call xvparm('FOCAL',r,count,def,' ')
      if (def.eq.0) fic=r	!camera focal length

      call xvparm('LAXIS',r,count,def,' ')
      if (def.eq.0) lo=r	!line of optical axis

      call xvparm('SAXIS',r,count,def,' ')
      if (def.eq.0) so=r	!sample of optical axis

      call xvparm('PSCALE',r,count,def,' ')
      if (def.eq.0) pixpmm=r	!object space scale in pixels/mm

      if (fic.lt.1.) call mabend('FOCAL parameter not specified.')
      if (pixpmm.lt.1.) call mabend('PSCALE parameter not specified.')
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Determine position of spacecraft (latitude, longitude, range).
c
      subroutine rs_parms(sclat,sclon,rmag,rs)
      implicit none
      real sclat,sclon,rmag
      real*8 rs(3)		!rs vector

      common/konst/rad
      real rad

      integer i,ivabs,count,def
      real r,far(3)

      call xvparm('RSVECTOR',far,count,def,' ')
      if (def.eq.0) then
         do i=1,3
            rs(i) = far(i)
         enddo
         rmag = sqrt(far(1)**2+far(2)**2+far(3)**2)!spacecraft range
         sclat = rad*(asin(far(3)/rmag))	!spacecraft latitude
         sclon = rad*(-atan2(far(2),far(1)))	!spacecraft longitude
      endif

      ivabs = 0		!=1 if RS vector is changed by parameters below
      call xvparm('SPACE',far,count,def,' ')
      if (def.eq.0) then
         sclat = far(1)		!s/c latitude
         sclon = far(2)		!s/c longitude
         rmag = far(3)		!s/c range
         ivabs=1
      endif

      call xvparm('RMAGNITU',r,count,def,' ')
      if (def.eq.0) then
         rmag = r		!range to target center
         ivabs=1
      endif

      call xvparm('SLATITUD',r,count,def,' ')
      if (def.eq.0) then
         sclat = r		!s/c latitude
         ivabs=1
      endif

      call xvparm('SLONGITU',r,count,def,' ')
      if (def.eq.0) then
         sclon = r		!s/c longitude
         ivabs=1
      endif

      if (ivabs.ne.0) then	!Recalculate RS vector if neccessary
         rs(1) = rmag*cos(sclat/rad)*cos(sclon/rad)
         rs(2) = -rmag*cos(sclat/rad)*sin(sclon/rad)
         rs(3) = rmag*sin(sclat/rad)
      endif
      return
      end
