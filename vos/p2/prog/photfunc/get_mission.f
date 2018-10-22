ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Determine mission ID from label or parameters.
c
      subroutine get_mission(iunit,project,icam,projx)
      implicit none
      integer iunit	!input image unit number
      character*5 project
      integer icam	!camera serial number
      integer projx

      integer ind,n,fdsno,count,def
      character*5 proj

c scan flight label for project and camera identifiers
      call getproj(iunit,project,icam,fdsno,ind)
      if (ind.ne.0) then
         project = 'NONE '
         icam = 0
      endif

c check for parameters
      call xvparm('CAMERA',n,count,def,' ')
      if (def.eq.0) icam=n              !camera serial number

      call xvparm('MISSION',proj,count,def,' ')
      if (def.eq.0) project=proj

      call xvmessage('Flight mission='//project,' ')

c check project for multimission support.....
c projx=1 if mission is supported by getcamcon and getspice2
c projx=-1 if mission is supported by getcamcon, but not by getspice2
c projx=0 otherwise
      projx = 0
      if (project.eq.'VGR-1' .or. project.eq.'VGR-2') projx=1
      if (project.eq.'GLL') projx=1
      if (project.eq.'CASSI') projx=1

      if (project.eq.'WFPC1') projx=-1
      if (project.eq.'WFPC2') projx=-1
      if (project.eq.'MAR10') projx=-1
      if (project.eq.'VIKOR') projx=-1
      if (project.eq.'MAR-9') projx=-1
      if (project.eq.'MAR10') projx=-1

      if (project.eq.'SIPS') projx=0
      if (project.eq.'QUEST') projx=0

      if (projx.ne.0 .and. icam.eq.0) then
         call xvmessage('Unknown camera serial number',' ')
         call mabend('See CAMERA parameter')
      endif
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get SPICE data and return it in data.
c
      subroutine spice_data(iunit,projx,data,rdata)
      implicit none
      integer iunit		!input image unit number
      integer projx		!is .le.0 if no spice data available
      real*8 data(20)		!returned IS/OS geometry
      real*4 rdata(40)		!equivalenced to data

      common/c2/sunlat,sunlon,sunrange
      real*8 sunlat,sunlon,sunrange

      integer ind,isource,idnum,count,def
      real far(20)

      integer*4 buf(200)
      real*8 buf8(100)		!buffer returned by getspice2
      equivalence (buf,buf8)

      character*12 target
      character*80 msg
      logical xvptst

      call mve(7,40,-999.,rdata,0,1)
      rdata(36) = 0.		!default lora

      call xvparm('TARGET',target,count,def,' ')
      if (def.eq.0) then
         call getplacon(target,idnum,far,ind)
         rdata(26) = far(1)		!long equatorial radius (ra)
         rdata(37) = far(2)		!short equatorial radius (rb)
         rdata(25) = far(3)		!polar radius (rc)
         rdata(36) = far(4)		!longitude of radius a
      endif

      if (projx.le.0) return		!skip if no SPICE data for this mission
      if (xvptst('NOSPICE')) return	!skip if no SPICE available

      call init_spice
      call getspice2(iunit,0,buf8,ind)
      if (ind.ne.1) then
         call prnt(4,1,ind,'GETSPICE2: bad indicator.')
         call abend
      endif
      call cmsource(buf8,isource)	!print c-matrix source
      call mve(8,9,buf8(59),data,1,1)     !om
      call mve(8,3,buf8(22),data(10),1,1) !rs
      rdata(26)=buf8(13)		!long equatorial radius (ra)
      rdata(37)=buf8(14)		!short equatorial radius (rb)
      rdata(25)=buf8(15)		!polar radius (rc)
      rdata(35) = buf8(68) + 90.	!north angle (angln)
      if (rdata(35).gt.360.) rdata(35)=rdata(35)-360.
      rdata(31)=buf8(30)		!spacecraft latitude (sclat)
      rdata(32)=buf8(31)		!spacecraft longitude (sclon)
      rdata(38)=buf8(27)		!target range (rmag)
      sunlat = buf8(28)
      sunlon = buf8(29)
      idnum = buf(9)
      call pbname(idnum,target,*999)
      call getplacon(target,idnum,far,ind)
      rdata(36) = far(4)		!longitude of ra
      return

  999 write(msg,101) idnum
  101 format('Invalid target ID returned from SPICE=',I10)
      call mabend(msg)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Determine if input image is image or object space.
c If image space, get geometry correction parameters.
c
      subroutine geo_distor_parms(iunit,geom,itype)
      implicit none
      integer iunit,geom
      integer itype		!7=image space, 16=object space

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      integer ind,mode,nah,nav
      logical xvptst

c Scan label for GEOMA keyword....
      call searc_distor(iunit,ind)
      if (ind.lt.0)
     +	 call xvmessage('SEARC_DISTOR: err reading label',' ')
      if (ind.eq.1) then
         itype = 16		!object space
      else
         itype = 7		!image space
      endif

      if (xvptst('OBJECT')) itype=16
      if (xvptst('IMAGE').or.xvptst('DISTOR')) itype=7

      if (itype.eq.16) then
         call xvmessage('Input image is in Object Space',' ')
         if (geom.ne.0)
     +     call xvmessage('Geometric distortion file not needed',' ')
         return		!skip rest if object space
      endif

c Here if image space.
      call xvmessage('Input image is in Image Space',' ')
      if (geom.ne.0) then
         call xvmessage('Geometric distortion param file used',' ')
         call xvclose(geom,ind,' ')
         mode = 1	!input file present
      else
         call xvmessage('Nominal geometric distortion params used',' ')
         mode = 0	!use nominals
      endif

c Get geometric correction parameters from input file or nominal parameters.
      call getgeom(geom,project,icam,mode,conv,conv,nah,nav,ind)
      if (ind.ne.0) then
         call prnt(4,1,ind,'GETGEOM: bad indicator.')
         call mabend('NO GEOMETRIC CORRECTION DATA FOR IMAGE SPACE')
      endif
      nph = nah + 1
      npv = nav + 1
      return
      end
