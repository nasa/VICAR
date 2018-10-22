ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Determine image geometry and store it in rdata buffer.
c Routines called:
c   map_label: get data from map3 label (if any)
c   get_mission: determine mission and camera
c   spice_data: get SPICE data
c   geo_distor_parms: get geometric distortion parameters
c   os_parms: get parameters for image and object space images
c   print_os: print object space geometry
c   print_map: print map projection geometry
c
      subroutine image_geometry(geom)
      implicit none
      integer geom		!geometric distortion unit

      common/cmp/mp
      real*8 mp

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      common/c3/rdata
      real rdata(40)
      integer*4 idata(40)
      real*8 data(20),om(9),rs(3)
      equivalence (rdata,data,idata),(data,om),(data(10),rs)

      common/units/iunit,ounit,maxdn,fmt
      integer iunit,ounit,maxdn
      character*5 fmt

      real sdata(40)
      integer projx,ind,itype

c get map projection data from map label (if present) and parameters
      call map_label(iunit,itype,ind)
      if (ind.eq.1) goto 100		!skip SPICE processing

c access SPICE data
      call get_mission(iunit,project,icam,projx)  !get mission and camera ids
      call mve(7,40,rdata,sdata,1,1)	!save map projection data
      call spice_data(iunit,projx,data,rdata)

      if (itype.eq.0) then
         call geo_distor_parms(iunit,geom,itype)
         if (itype.eq.7) then
            idist = 1
            itype = 16
         endif
      endif

      if (itype.eq.16) then	!image or object space
         call os_parms(projx)
      else					!map projection
         call mve(7,9,sdata,rdata,1,1)		!restore map projection geometry
         rdata(25) = sdata(25)	!polar radius
         rdata(26) = sdata(26)	!long equatorial radius
         rdata(37) = sdata(37)	!short equatorial radius
      endif

c print the geometry information
  100 idata(39) = itype
      if (itype.eq.16) then
         call print_os(icam)
      else
         call mp_buf2mpo(rdata,mp,ind)
         if (ind.ne.0) call mabend('***Err converting data buf to mp')
         call print_map(rdata,itype)
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print object space geometry for input image.
c
      subroutine print_os(icam)
      implicit none
      integer icam

      common/c3/rdata
      real rdata(40)
      real*8 data(20),om(3,3),rs(3)
      real fic,lo,so,pixpmm             !camera focal length, op-axis, scale
      real ra,rb,rc                     !target body radii
      real sclat,sclon,rmag             !spacecraft lat,long and distance
      real scline,scsamp,angln          !spacecraft line,samp,north angle
      equivalence (data,rdata),(om,data(1)),(rs,data(10)),
     * (rdata(25),rc),(rdata(26),ra),(rdata(37),rb),
     * (rdata(27),fic),(rdata(28),lo),(rdata(29),so),(rdata(30),pixpmm),
     * (rdata(31),sclat),(rdata(32),sclon),(rdata(38),rmag),
     * (rdata(33),scline),(rdata(34),scsamp),
     * (rdata(35),angln)

      integer i
      character*80 msg

      write(msg,100) icam,fic,pixpmm
  100 format('Camera=',I2,'  Focal length=',F8.2,
     &  ' mm  Picture scale=',f7.4,' pixels/mm')
      call xvmessage(msg,' ')

      write(msg,101) lo,so
  101 format('Optical axis intercept (line,samp)=(',f7.1,',',f7.1,')')
      call xvmessage(msg,' ')

      write(msg,106) ra,rb,rc
  106 format('Long eq radius=',f7.1,' Short eq radius=',f7.1,
     +  ' Polar radius=',f7.1)
      call xvmessage(msg,' ')      

      write(msg,113) rmag,sclat,sclon
  113 format('Spacecraft range=',f10.0,' (lat,lon)=(',f7.3,',',f7.3,')')
      call xvmessage(msg,' ')      

      write(msg,114) angln
  114 format('North angle=',f6.2,' deg clockwise from up')
      call xvmessage(msg,' ')      

      write(msg,115) scline,scsamp
  115 format('Target center (line,samp)=(',f12.2,',',f12.2,')')
      call xvmessage(msg,' ')

      call xvmessage('OM matrix:',' ')
      do i=1,3
         write(msg,120) om(i,1),om(i,2),om(i,3)
  120    format(f12.8,f12.8,f12.8)
      call xvmessage(msg,' ')
      enddo

      call xvmessage('RS vector:',' ')
      write(msg,122) rs(1),rs(2),rs(3)
  122 format(f14.1,f14.1,f14.1)
      call xvmessage(msg,' ')
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print map projection geometry for input image.
c
      subroutine print_map(rdata,itype)
      implicit none
      real rdata(40)
      integer itype

      common/cmp/mp
      real*8 mp

      integer ind,ll_type
      real*8 lat,lon,line,samp
      character*80 msg

      character*23 pname(16)/
     +  'Polar Orthographic','Oblique Orthographic',
     +	'Polar Stereographic','Oblique Stereographic',
     +  'Lambert Conformal','Mercator',
     +  'Image Space','Object Space',
     +  'Normal Cylindrical','Simple Cylindrical',
     +  'Oblique Simple Cyl','Sinusoidal Equal Area',
     +  'Oblique Sinusoidal','Mollweide',
     +  'Transverse Mercator','Perspective'/

      write(msg,90) pname(itype)
   90 format('Input image map projection=',a23)
      call xvmessage(msg,' ')      

      write(msg,100) rdata(26),rdata(25)
  100 format('Equatorial radius=',f7.1,' Polar radius=',f7.1)
      call xvmessage(msg,' ')      

      write(msg,102) rdata(7)
  102 format('Picture scale=',f10.2,' km/pixel')
      call xvmessage(msg,' ')

      if (itype.ne.1 .and. itype.ne.3) then	!if not polar projection
         write(msg,103) rdata(9)		!print north angle.
  103    format('North angle=',f10.2)
         call xvmessage(msg,' ')
      endif

      if (itype.eq.5) then			!if lambert projection
         write(msg,105) rdata(4),rdata(5)	!print special parallels.
  105    format('Special parallels=',f10.2,f12.2)
         call xvmessage(msg,' ')
      endif

      lat = rdata(3)
      lon = rdata(4)
      ll_type = 1
      call mp_ll2xy(mp,line,samp,lat,lon,ll_type,ind)
      if (ind.ne.0) call mabend('Err in input image geometry')
      write(msg,115) line,samp,lat,lon
  115 format('At special pt: (line,samp)=(',f6.0,',',f6.0,
     +  ')   (lat,long)=(',f7.2,',',f7.2,')')
      call xvmessage(msg,' ')
      return
      end
