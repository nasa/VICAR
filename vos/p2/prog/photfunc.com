$!****************************************************************************
$!
$! Build proc for MIPL module photfunc
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:24:55
$!
$! Execute by entering:		$ @photfunc
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module photfunc ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to photfunc.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("photfunc.imake") .nes. ""
$   then
$      vimake photfunc
$      purge photfunc.bld
$   else
$      if F$SEARCH("photfunc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake photfunc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @photfunc.bld "STD"
$   else
$      @photfunc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create photfunc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack photfunc.com -mixed -
	-s photfunc.f image_geometry.f map_label.f get_mission.f os_parms.f -
	   farenc_mode.f phot_parms.f grid_labels.f photb.f compute_segment.f -
	   setup.f phot_sub.f toplanet.f -
	-p photfunc.pdf -
	-i photfunc.imake -
	-t tstphotfunc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create photfunc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c VICAR program PHOTFUNC: Correct for variation in solar illumination
c
c The major routines are:
c    open_image: open input image and corresponding output image
c                open secondary inputs and determine purpose for these
c    image_geometry: determine projection and lighting geometry
c    phot_parms: get specifications for photometric function
c    phot_label: write photfunc label to output image
c    photb: correct the image using grid (called via stacka) or...

      include 'VICMAIN_FOR'

      subroutine main44
      implicit none

      common/cplanet/vsc,ra,rb,rc
      real*8 vsc(3),ra,rb,rc

      common/csun/vsun,s,ax,bx,cx
      real*8 vsun(3),s(3),ax,bx,cx

      common/c2/sunlat,sunlon,sunrange
      real*8 sunlat,sunlon,sunrange

      common/c3/rdata
      real*4 rdata(40),sclat,sclon,rmag
      real*8 data(20),rs
      integer idata(40),itype
      equivalence (data,rdata,idata),(data(10),rs)
      equivalence (rdata(31),sclat),(rdata(32),sclon),(rdata(38),rmag)
      equivalence (idata(39),itype)

      common/pf0/linc,sinc,grid,term,limb,const,maxcor,nointerp
      integer linc,sinc,grid
      real term,limb,const,maxcor
      logical*1 nointerp

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      common/units/iunit,ounit,maxdn,fmt
      integer iunit,ounit,maxdn
      character*5 fmt

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      common/konst/rad
      real rad

      integer geom,ncol
      integer l1,l2,l3,l4,total
      character*80 msg
      logical xvptst
      external photb

      call xvmessage('PHOTFUNC version 10-Jan-2013',' ')
      rad=57.29578

      call open_image(iunit,ounit,geom,cmap,grid,maxdn,fmt)

      call image_geometry(geom)

c set up call to light_angles
      call mve(8,3,rs,vsc,1,1)
ccc      sunrange = 149597871.D0*0.387098
      sunrange = 1.d0
      call latrec(sunrange,-sunlon/rad,sunlat/rad,vsun)
      call unorm(vsun,s,sunrange)
      ra = rdata(26)
      rb = rdata(37)
      rc = rdata(25)
      ax = 1.
      bx = (ra/rb)**2
      cx = (ra/rc)**2

      write(msg,112) sunlat,sunlon
  112 format('Solar latitude=',f7.3,' Solar longitude=',f7.3)
      call xvmessage(msg,' ')      

c check solar lat,long
      if (sunlat.eq.-999. or. sunlon.eq.-999.) then
         call xvmessage('Position of sun is unknown',' ')
         call mabend('See SOLAR parameter')
      endif

c check spacecraft position (lat,lon,range).
      if (sclat.eq.-999. .or. sclon.eq.-999. .or. rmag.eq.-999.) then
         call xvmessage('Position of spacecraft is unknown',' ')
         call mabend('See parameters RSVECTOR or SPACE')
      endif

c check target radii
      if (ra.lt.1. or. rb.lt.1. .or. rc.lt.1.) then
         call xvmessage('Target radii are unknown',' ')
         call mabend('See RADII parameter')
      endif

      call phot_parms			!get photometric function parameters
      call phot_label(ounit)            !write photfunc label to output image

c set up buffer sizes (in bytes) for call to stacka...
      ncol = (nso-2)/sinc + 2
      l1 = 4*nso		!real*4 buf(nso)      real image line
      l2 = 4*ncol		!real*4 tbrite(ncol)
      l3 = 4*ncol		!real*4 bbrite(ncol)
      l4 = 4
      if (cmap.gt.0) l4=2*nso	!integer*2 cmbuf(nso)

      nointerp = xvptst('NOINTERP')
      if (linc.eq.1 .and. sinc.eq.1) then
         nointerp = .true.
         linc = 10
         sinc = 10
      endif

      total = l1+l2+l3+l4
      call stacka(9,photb,4,l1,l2,l3,l4,total,nso,ncol)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open input and output images.
c
      subroutine open_image(iunit,ounit,geom,cmap,grid,maxdn,fmt)
      implicit none
      integer iunit,ounit	!Returned input and output unit numbers
      integer geom,cmap,grid	!Returned geom, cmap, and grid unit numbers
      integer maxdn		!maximum output DN
      character*5 fmt		!image format=BYTE, WORD, HALF, REAL

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      common/cmax/maxfull	!Maximum 32-bit integer (approximate)
      real maxfull

      common/c9/gdlat,gdlon,gslat,gslon,numlat,numlon,nlrg,nlg,nbg,
     & numsam
      real gdlat,gdlon,gslat,gslon
      integer numlat,numlon,nlrg,nlg,nbg,numsam

      integer i,ind,count,idef,ninp,nout,unit,ibufsize
      integer nl,ns,nlb,nbb,nlc,nsc
      integer index

      character*7 gmsg/'PHOTFIT'/
      character*18 cmsg/'CLASSIFICATION MAP'/
      character*7200 lblbuf
      character*5 cfmt

c except for input image, all input and output files are optional.
c =0 if not present.
      ounit = 0		!output photometrically corrected image
      geom = 0		!geometric distortion parameters
      cmap = 0		!classification map (=input image size)
      grid = 0		!grid file
      maxfull = 1024.**3

      call xvunit(iunit,'INP',1,ind,' ')
      call chkstat(ind,' ERR IN XVUNIT, INPUT FILE 1,STAT=',1,ind)
      call xvopen(iunit,ind,'U_FORMAT','REAL','OPEN_ACT','SA',
     *   'IO_ACT','SA',' ')

      call xvget(iunit,ind,'FORMAT',fmt,' ') !Determine data format
      call xvmessage(' System label says input is '//fmt,' ')
      if (fmt.eq.'BYTE') then
         maxdn = 255
      elseif (fmt.eq.'HALF' .or. fmt.eq.'WORD') then
         maxdn = 32676
      elseif (fmt.eq.'FULL') then
         maxdn = maxfull
      elseif (fmt.eq.'REAL') then
         maxdn = 0		!not used for REAL images
      else
         call mabend('***Unrecognized format')
      endif

      call xvparm('MAXDN',i,count,idef,' ')
      if (idef.eq.0) maxdn=i

c check size field of input image
      call xvsize(sl,ss,nlo,nso,nli,nsi)
      if (nlo.gt.nli-sl+1) then
         nlo = nli - sl + 1
         call xvmessage('***Output lines truncated',' ')
      endif
      if (nso.gt.nsi-ss+1) then
         nso = nsi - ss + 1
         call xvmessage('***Output samples truncated',' ')
      endif

      call xvpcnt('OUT',nout)
      if (nout.gt.0) then
         call xvunit(ounit,'OUT',1,ind,' ')
         call chkstat(ind,' ERR IN XVUNIT FOR OUTPUT FILE',1,ind)
         call xvopen(ounit,ind,'OP','WRITE','OPEN_ACT','SA',
     *     'IO_ACT','SA','U_FORMAT','REAL','O_FORMAT',fmt,' ')
      endif

c check for secondary input files.
      call xvpcnt('INP',ninp)
      if (ninp.lt.2) return

      do 100 i=2,ninp
      call xvunit(unit,'INP',i,ind,' ')
      call chkstat(ind,' ERR IN XVUNIT, INPUT FILE=',1,i,1)
      call xvopen(unit,ind,'U_FORMAT','REAL','OPEN_ACT','SA',
     *   'IO_ACT','SA',' ')

      call xvget(unit,ind,'NL',nl,'NS',ns,'NLB',nlb,'NBB',nbb,' ')
      if (ns.eq.512 .and. nl.eq.0 .and. nlb.gt.0 .and. nbb.eq.0) then
         geom = unit
         call prnt(4,1,geom,' GEOM=.')
         goto 100
      endif

      ibufsize=7200
      call xlgetlabel(unit,lblbuf,ibufsize,ind)
      call chkstat(ind,' ERR XLGETLABEL,STAT=',1,ind)
      if (index(lblbuf(1:ibufsize),cmsg) .gt. 0) then
         cmap = unit
         call prnt(4,1,cmap,' CMAP=.')
         call xvget(cmap,ind,'NL',nlc,'NS',nsc,'FORMAT',cfmt,' ')
         if (cfmt.ne.'BYTE') call mabend(' CMAP MUST BE BYTE IMAGE')
         if (nlc.ne.nli .or. nsc.ne.nsi) call mabend(
     &   ' CLASSIFICATION MAP MUST BE SAME SIZE AS IMAGE')
         call xvclose( cmap, ind,' ')
         call xvopen( cmap, ind, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     &   'U_FORMAT', 'HALF',' ')
         go to 100
      endif

      if (index(lblbuf(1:ibufsize),gmsg) .le. 0) goto 999
      call xvget(unit,ind,'NL',nlg,'NS',numsam,' ')
      if (numsam.ne.120) goto 999	!not grid file after all
      grid = unit
      call prnt(4,1,grid,' GRID=.')
      call mabend('***input PHOTFIT grid files not supported')
  100 continue

      return

  999 call mabend('Unrecognizable secondary input')
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create image_geometry.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create map_label.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get map projection information.
c
c Routines called:
c   mp_label_read: read map3 label
c   mp_mpo2buf: convert mp data to rdata (CONVEV format)
c   map_parms: if no map3 label, check for map projection parameters
c
c The remainder of the routine is an attempt to avoid calling SPICE by using
c parameters instead:
c   target_parms: get target radii
c   rs_parms: get spacecraft vector
c   sun_label: get solar lat-long from label or parameters
c
      subroutine map_label(iunit,itype,ind)
      implicit none
      integer iunit		!input image unit number
      integer itype		!map projection type
      integer ind		!=1 if map data is complete

      common/cmp/mp
      real*8 mp

      common/c2/sunlat,sunlon,sunrange
      real*8 sunlat,sunlon,sunrange

      common/c3/rdata
      real*4 rdata(40)
      integer idata(40)
      real*8 data(20),rs(3)
      real ra,rb,rc,lora	!target body radii, longitude of radius a
      real sclat,sclon,rmag	!spacecraft lat,long and distance
      equivalence (data,rdata,idata),(data(10),rs)
      equivalence (rdata(25),rc),(rdata(26),ra),(rdata(37),rb)
      equivalence (rdata(31),sclat),(rdata(32),sclon),(rdata(38),rmag)
      equivalence (rdata(36),lora)

      integer stat,cnt,def
      real far(2)

c Initialize all projection and lighting geometry as invalid...
      call mve(7,40,-999.,rdata,0,1)
      rdata(9) =0.              !default north angle (map projections)
      rdata(36) = 0.		!default lora

      sunlat = -999.
      sunlon = -999.
      sunrange = -999.
      call xvparm('SOLAR',far,cnt,def,' ')
      if (def.eq.0) then
         sunlat = far(1)
         sunlon = far(2)
      endif

      itype = 0                 !projection type is initially unknown

c Retrieve image geometry from map projection label (if present)
      call mp_init(mp,stat)
      if (stat.ne.0) call mabend('***MP error on init')
      call mp_label_read(mp,iunit,stat)
      if (stat.eq.0) then	!if map label found, copy to data buffer
         call sun_label(iunit,sunlat,sunlon)
         call mp_mpo2buf(mp,rdata,stat)
         if (stat.ne.0) call mabend('***Err converting MP to data buf')
         itype = idata(39)
      endif

      if (itype.eq.0) call map_parms(rdata,itype)
      idata(39) = itype

      ind = 0
      if (itype.eq.0) return		!skip if not a map projection

c For map-projected images, check if the illumination geometry has been
c specified via parameters:

      call target_parms(ra,rb,rc,lora)		!get target radii
      call rs_parms(sclat,sclon,rmag,rs)	!get spacecraft position

      if (ra.eq.-999. .or. rb.eq.-999. .or. rc.eq.-999.) return
      if (sclat.eq.-999. .or. sclon.eq.-999. .or. rmag.eq.-999.) return
      if (sunlat.eq.-999. .or. sunlon.eq.-999.) return

      ind = 1		!information complete, so we are done
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get map projection parameters.  rdata and itype may be updated.
c
      subroutine map_parms(rdata,itype)
      implicit none
      real rdata(40)
      integer itype

      integer cnt,def
      logical xvptst

c determine projection type
      if (xvptst('ORTHOGRA')) then
         itype = 2			!orthographic projection
         if(xvptst('POLE'))itype=1	!polar orthographic projection
      endif

      if (xvptst('STEREOGR')) then
         itype = 4			!stereographic projection
         if(xvptst('POLE'))itype=3	!polar stereographic projection
      endif

      if(xvptst('LAMBERT'))itype=5	!lambert projection
      if(xvptst('MERCATOR'))itype=6	!mercator projection
      if(xvptst('CYLINDRI'))itype=9	!cylindrical projection
      if(xvptst('RECTANGU'))itype=10 !rectangular,lat lon
      if(xvptst('LATLON'))itype=10

      if (itype.eq.0 .or. itype.eq.16) return	!skip if not standard projection

      call prnt(4,1,itype,'Map projection type specified=.')

c get projection parameters
      call xvparm('SAMPLE',rdata(1),cnt,def)	!special sample
      if (def.ne.0) call mabend('SAMPLE parameter not specified')

      call xvparm('LINE',rdata(2),cnt,def)	!special line
      if (def.ne.0) call mabend('LINE parameter not specified')

      call xvparm('LATITUDE',rdata(3),cnt,def)	!special latitude
      if (def.ne.0) call mabend('LATI parameter not specified')

      call xvparm('LONGITUD',rdata(6),cnt,def)	!special longitude 
      if (def.ne.0) call mabend('LONG parameter not specified')

      call xvparm('SCALE',rdata(7),cnt,def)	!scale (km/pixel)
      if (def.ne.0) call mabend('SCALE parameter not specified')

      call xvparm('NORTH',rdata(9),cnt,def)	!north angle (default=0.)

      if (itype.eq.1 .or. itype.eq.3) return	!skip rest if polar projection

      if (itype.eq.6) call mercpatch(rdata)

      if (itype.ne.5) return			!skip rest if not lambert

      call xvparm('PAR1',rdata(4),cnt,def)	!lat of parallel for lambert
      if (def.ne.0) call mabend('PAR1 parameter not specified')

      call xvparm('PAR2',rdata(5),cnt,def)	!lat of parallel for lambert
      if (def.ne.0) call mabend('PAR2 parameter not specified')
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get solar latitude and longitude from PERSLAB label.
c
      subroutine sun_label(iunit,sunlat,sunlon)
      implicit none
      integer iunit		!input image unit number
      real*8 sunlat,sunlon	!returned

      real r
      integer ind

      call xlget(iunit,'HISTORY','SUB_SOLAR_LATITUDE',r,
     +              ind,'FORMAT','REAL','HIST','PERSLAB',' ')
      if (ind.eq.1) sunlat=r

      call xlget(iunit,'HISTORY','SUB_SOLAR_LONGITUDE',r,
     +              ind,'FORMAT','REAL','HIST','PERSLAB',' ')
      if (ind.eq.1) sunlon=r
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_mission.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create os_parms.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create farenc_mode.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Far Encounter mode:  If line and sample of planet center is input via
c parameters, compute OM matrix using FARENC algorithm (momati).
c
      subroutine farenc_mode(rdata,om,rs)
      implicit none
      real rdata(40)
      real*8 om(9),rs(3)

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      integer ind,ifar,count,def
      real r,far(2)
      real*8 lo,so,lssp,sssp,pixpmm,fic,bl,phi,tht,vabs
      logical xvptst

      ifar = 0
      if (xvptst('FARENC')) ifar=1

      call xvparm('ISSCPT',far,count,def,' ')	!if image space line-sample,
      if (def.eq.0) then			!convert to object space.
         call convisos(project,icam,far(1),far(2),rdata(33),rdata(34),
     +               1,conv(9),nph,npv,ind)
         ifar = 1
      endif

      call xvparm('OSSCPT',far,count,def,' ')
      if (def.eq.0) then
         rdata(33) = far(1)	!object space line
         rdata(34) = far(2)	!object space sample
         ifar = 1
      endif

      call xvparm('SSCPT',far,count,def,' ')
      if (def.eq.0) then
         rdata(33) = far(1)	!object space line
         rdata(34) = far(2)	!object space sample
         ifar = 1
      endif

      if (ifar.eq.0) return		!skip if not farenc mode
      call xvmessage('FARENC mode specified',' ')
      call xvparm('NORANGLE',r,count,def,' ')
      if (def.eq.0) rdata(35)=r       !north angle

      fic = rdata(27)		!focal length (mm)
      lo = rdata(28)		!optical axis intercept line
      so = rdata(29)		!optical axis intercept sample
      pixpmm = rdata(30)	!o.s. scale (pixels/mm)
      lssp = rdata(33)		!subspacecraft point line
      sssp = rdata(34)		!subspacecraft point sample
      phi = rdata(31)		!s/c latitude in degrees
      bl = rdata(32)		!s/c longitude in degrees
      tht = rdata(35)		!North angle (90 degrees from up)
      vabs = rdata(38)		!range to target body (km)

      if (tht.eq.-999.) call mabend('NORANGLE parameter not specified')

c compute om matrix and rs vector.
      call momati(lo,so,lssp,sssp,pixpmm,fic,bl,phi,tht,vabs,om,rs)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check for tiepoints mode parameter.
c If present, use tiepoints to compute OM matrix.
c
      subroutine tiepoints_mode(rdata,om,rs)
      implicit none
      real rdata(40)
      real*8 om(9),rs(3)


      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      integer i,j,npoint,ind,count,def
      real is_line,is_samp,os_line,os_samp,cl,cs
      real aa(100),bb(6)

c tiepoints mode allows up to 25 tiepoints of 4 words each
c it returns om matrix and rs vector

      call xvparm('TIEPOINT',aa,count,def,' ')
      if (def.ne.0) return	!skip if tiepoints mode not specified
      npoint=count/4
      call xvmessage('TIEPOINTS mode specified:',' ')
      call prnt(4,1,npoint,'0NUMBER OF TIEPOINTS =.')
      call prnt(7,npoint*4,aa,' TIEPOINTS     =         .')

      if (idist.eq.1) then	!convert tiepoints from is to os
         do i=1,npoint,4
            j=i*4-3
            is_line=aa(j)
            is_samp=aa(j+1)
            call convisos(project,icam,is_line,is_samp,
     +           os_line,os_samp,1,conv(9),nph,npv,ind)
            if (ind.ne.0) call prnt(4,1,ind,'CONVISOS: bad ind=.')
            aa(j)=os_line
            aa(j+1)=os_samp
         enddo
      endif

c set up parameters for fomclv
      bb(1) = rdata(27)*rdata(30)	!fl*scale
      bb(2) = rdata(26) - rdata(25)	!req - rpole
      bb(3) = rdata(38)			!rmag
      bb(4) = rdata(31)			!spacecraft lat
      bb(5) = rdata(32)			!spacecraft long
      bb(6) = rdata(26)			!req
      cl = rdata(28)			!optical axis line
      cs = rdata(29)			!optical axis samp

      call fomclv(ind,npoint,aa,bb,om,rs,cl,cs)		!compute om
      if (ind.ne.0) then
         call prnt(4,1,ind,'0TIEPOINT MODE ERROR,IND=.')
         call abend
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create phot_parms.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c ********************************************************************
c process photometric function parameters.
c
      subroutine phot_parms
      implicit none

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      common/pf4/data
      real data(7)

      common/hapke_norm/bb1  ! the hapke function at zero phase & i & e
      common/buratti_norm/bur_norm  ! the buratti function at zero phase
      real bb1,bur_norm

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      common/konst/rad
      real rad

      real r,far(10)
      integer i,j,n,def,count

c set some defaults
      bb1 = 0.0		!hapke normalizing factor
      bur_norm=0.0	! "

      d(1) = 1.
      do i=2,6		!veverka-squyres phase angle correction
         d(i) = 0.0	! elements 5&6 are for mosher function
      enddo

      call xvp('LINC',linc,count)		!line spacing of grid
      call xvp('SINC',sinc,count)		!sample spacing of grid
      call xvparm('INCR',n,count,def,' ')
      if (def.eq.0) then
         linc = n
         sinc = n
      endif

      minn = 1		!minnaert function is the default
      call xvparm('MINNAERT',k,count,def,' ')
      if (k.eq.1.0) minn = 3	! lambert function

c  1978 hapke fcn.
      ihapke=0
      call xvparm('HAPKE',far,count,def,' ')
      if (count.eq.4) then
        minn=2
        ihapke=1
	w=far(1)
        b=far(2)
        h=far(3)
        c=far(4)

c  cook'S MODIFICATION TO HAPKES FUNCTION
	call xvparm('COOK',far,count,def,' ')
	if (def.eq.0) then
	  icook=1
	  ck=far(1)
	endif
      elseif (count.eq.5) then
c  1984 hapke fcn., henyey-greenstein phase fcn.
        minn=2
        ihapke=2
	w = far(1)
        h = far(2)
        ck = far(3)	! theta-bar
        c = far(4)	! s0
	b = far(5)	! henyey-greenstein
      elseif (count.eq.6) then
c  1984 hapke fcn., legendre phase fcn.
        minn=2
        ihapke=3
	w = far(1)
        h = far(2)
        ck = far(3)	! theta-bar
        c = far(4)	! s0
	b = far(5)	! first legendre coefficient
	xlg2 = far(6)	! second legendre coef.
      endif

c  squyres-veverka function
      call xvparm('VEVERKA',far,count,def,' ')
      if (def.eq.0) then
	minn = 4
        d(1)=far(1)
        d(2)=far(2)
        d(3)=far(3)
        d(4)=far(4)
      endif

c  mosher modification of veverka function
      call xvparm('MOSHER',far,count,def,' ')
      if(def.eq.0)then
        minn=5
        d(1)=far(1)
        d(2)=far(2)
        d(3)=far(3)
        d(4)=far(4)
        d(5)=far(5)
        d(6)=far(6)
      endif

c  buratti-veverka function
      call xvparm('BURATTI',far,count,def,' ')
      if(def.eq.0)then
        minn=6
        d(1)=far(1)
        d(2)=far(2)
        d(3)=far(3)
        d(4)=far(4)
        d(5)=far(5)
        d(6)=far(6)
      endif

c irvine function
      call xvparm('IRVINE',far,count,def,' ')
      if(def.eq.0)then
         minn=7
         d(1)=far(1)  ! k
         d(2)=far(2)  ! a
         d(3)=far(3)  ! b
      endif

      if (cmap.ne.0) call xvparm( 'CLASS', class, i, j,' ')

c this parameter used to zero areas near terminator with low signal
      term=0.
      call xvparm('TERMINAT',r,count,def,' ')
      if (def.eq.0) term=cos((90.-r)/rad)

c this parameter used to zero areas near the limb with foreshortening
      limb=0.
      call xvparm('LIMB',r,count,def,' ')
      if (def.eq.0) limb=cos((90.-r)/rad)

      call xvp('MULTIPLY',const,count)	!multiply corrected dn by constant
      const=1./const

      call xvp('MAXCOR',maxcor,count)
      maxcor = 1.0/maxcor
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c add label to output image specifying photometric function used
c
      subroutine phot_label(ounit)
      implicit none
      integer ounit

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      character stars(4)/'*','*','*','*'/
      integer ind

      character*36 lablam/
     *' LAMBERT PHOTOMETRIC FUNCTION, COS I'/                             
      character*140  labmos/     
     *' MOSHER PHOTOMETRIC FUNCTION(0.00-0.00000a+0.00e0p-0.00a)(ci**(0.00+0.0000a)(ce**(00.000.0000a)) '/
      character*70 labvev/
     *' VEVERKA PHOTOMETRIC FUNCTION(00000+0000000A+ 0000E0P-0000A)(CI/Ci+ce)'/
      character*62 labmin/
     *' MINNAERT PHOTOMETRIC FUNCTION COSI**000000*(COSE**000000    )'/
      character*84 labbur/
     *' BURATTI PHOTOMETRIC FUNCTION  A=000000 B=000000 C=000000 D=000000 e=000000 f=000000'/
      character*70 labhap/
     *' HAPKE PHOTOMETRIC FUNCTION,W=000000,B= 000000,H=000000,C= 000000     '/
      character*75 labhap1/
     *' HAPKE PHOTOMETRIC FUNCTION,W=000000,H=000000,TB=000000,S0=000000,hg=000000'/
      character*83 labhap2/
     *' HAPKE PHOTOMETRIC FUNCTION,W=000000,H=000000,TB=000000,S0=000000,b=000000,c=000000'/
      character*70 labcok/
     *' COOK PHOTOMETRIC FUNCTION,W=00000,B= 00000,H=00000,C= 00000,CK= 0    '/
      character *56 labirv/
     *' IRVINE Photometric function A=0.00000 B=0.00000 K=0.000'/

      if (grid.gt.0) then	!if grid option is used,
         call grid_labels(grid)
ccc         k=stars		!put asterisks in label
ccc         w=stars		!failed to compile on linux
ccc         b=stars
ccc         h=stars
ccc         c=stars
ccc         ck=stars
         call mve(4,6,stars,k,0,1)!put asterisks in label
      endif 

      if (ounit.eq.0) return		!skip if no output image

      if(minn.eq.1)then      
	   write(labmin(39:43), '(f5.4)') k		! new
	   write(labmin(52:58), '(f7.4)') k-1		! new
           call xladd(ounit,'HISTORY','PHOT',labmin,ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
           call xvmessage(labmin,' ')
      endif

      if(minn.eq.2.and.icook.ne.1)then
	if (ihapke.eq.1) then
	   write(labhap(31:36), '(f6.3)') w		! new
	   write(labhap(40:46), '(f7.3)') b		! new
	   write(labhap(50:55), '(f6.3)') h		! new
	   write(labhap(59:65), '(f7.3)') c		! new
           call xladd(ounit,'HISTORY','PHOT',labhap(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
 	   call xvmessage(labhap,' ')
	elseif (ihapke.eq.2) then
	   write(labhap1(31:36), '(f6.3)') w		! new
	   write(labhap1(40:45), '(f6.3)') h		! new
	   write(labhap1(50:55), '(f6.2)') ck		! new
	   write(labhap1(60:65), '(f6.3)') c		! new
	   write(labhap1(65:70), '(f6.3)') b		! new
           call xladd(ounit,'HISTORY','PHOT',labhap1(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
 	   call xvmessage(labhap1,' ')
	elseif (ihapke.eq.3) then
	   write(labhap2(31:36), '(f6.3)') w		! new
	   write(labhap2(40:45), '(f6.3)') h		! new
	   write(labhap2(50:55), '(f6.2)') ck		! new
	   write(labhap2(60:65), '(f6.3)') c		! new
	   write(labhap2(69:74), '(f6.3)') b		! new
	   write(labhap2(78:83), '(f6.3)') xlg2		! new
           call xladd(ounit,'HISTORY','PHOT',labhap2(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
 	   call xvmessage(labhap2,' ')
	endif
      endif

      if(minn.eq.2.and.icook.eq.1)then
	   write(labcok(30:34), '(f5.3)') w		! new
	   write(labcok(38:43), '(f6.3)') b		! new
	   write(labcok(47:51), '(f5.3)') h		! new
	   write(labcok(55:60), '(f6.3)') c		! new
	   write(labcok(64:69), '(f6.3)') ck		! new
           call xladd(ounit,'HISTORY','PHOT',labcok(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
           call xvmessage(labcok,' ')
      endif         

      if(minn.eq.3)then
           call xladd(ounit,'HISTORY','PHOT',lablam(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
	   call xvmessage(lablam,' ')
      endif      

      if(minn.eq.4)then
	   write(labvev(31:35), '(f5.3)') d(1)		! new
	   write(labvev(37:43), '(f7.4)') d(2)		! new
	   write(labvev(47:50), '(f4.3)') d(3)		! new
	   write(labvev(56:58), '(f3.2)') d(4)		! new
           call xladd(ounit,'HISTORY','PHOT',labvev(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
      	   call xvmessage(labvev,' ')
      endif      

      if(minn.eq.5) then   
	   write(labmos(30:33), '(f4.2)') d(1)		! new
	   write(labmos(35:51), '(f7.4)') -d(2)		! new
	   write(labmos(44:47), '(f4.2)') d(3)		! new
	   write(labmos(51:56), '(f5.2)') d(4)		! new
	   write(labmos(64:67), '(f4.2)') d(5)	! new
	   write(labmos(69:74), '(f6.4)') d(6)	! new
	   write(labmos(83:87), '(f5.2)') d(5)-1	! new
	   write(labmos(89:94), '(f6.4)') d(6)	! new
           call xladd(ounit,'HISTORY','PHOT',labmos(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
      	   call xvmessage(labmos,' ')
      endif

      if(minn.eq.6) then   
	   write(labbur(34:39), '(f6.3)') d(1)		! new
	   write(labbur(43:48), '(f6.3)') d(2)		! new
	   write(labbur(52:57), '(f6.4)') d(3)		! new
	   write(labbur(61:66), '(f6.3)') d(4)		! new
	   write(labbur(69:75), '(f6.3)') d(5)		! new
	   write(labbur(79:84), '(f6.3)') d(6)		! new
           call xladd(ounit,'HISTORY','PHOT',labbur(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
      	   call xvmessage(labbur,' ')
      endif

      if(minn.eq.7)then
	   write(labirv(52:56), '(f5.2)') d(1)		! new
	   write(labirv(32:38), '(f7.5)') d(2)		! new
	   write(labirv(42:48), '(f7.5)') d(3)		! new
           call xladd(ounit,'HISTORY','PHOT',labirv(2:),ind,
     *            'FORMAT','STRING',' ')
           call chkstat(ind,' ERR XLADD,STAT=',1,ind)
           call xvmessage(labirv,' ')
      endif

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create grid_labels.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get function and parametrs from grid labels.
c
      subroutine grid_labels(grid)
      implicit none
      integer grid

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/c9/gdlat,gdlon,gslat,gslon,numlat,numlon,nlrg,nlg,nbg,
     & numsam
      real gdlat,gdlon,gslat,gslon
      integer numlat,numlon,nlrg,nlg,nbg,numsam

      integer istat,ifu,minn1
      character*8 func

      integer ibuf(10)		!****never initialized
      real rbuf(10)
      equivalence(ibuf,rbuf)

c these are photfit keywords -- in photfunc, lambert = cosine,
c  oldhapke=hapke, hapke=newhapke, and linear & buratti are not supported.
      character*8 funcn(9)/ 'MINNAERT', 'LAMBERT', 'VEVERKA',
     & 'LINEAR', 'BURATTI', 'MOSHER', 'OLDHAPKE', 'COOK', 'HAPKE'/

      call zia(ibuf,6)

      call xlget(grid,'HISTORY','FUNCTION',func,istat,' ')
      call chkstat(istat, 'ERROR IN GRID LABEL', 1)
      do ifu=1,9
         if (funcn(ifu).eq.func) goto 10
      enddo
      call mabend(' INVALID FUNCTION IN GRID LABEL')

   10 if (ifu.eq.1) then
         minn1=1
      elseif (ifu.eq.2) then
         minn1=3
      elseif (ifu.eq.3) then
         minn1=4
      elseif (ifu.eq.6) then
         minn1=5
      elseif (ifu.ge.7) then
         minn1=2
         ihapke=1
         if (ifu.eq.8) icook=1
         if (ifu.eq.9) ihapke=2
      else
         call mabend(' FUNCTION '//func//' NOT SUPPORTED')
      endif

      if (minn.eq.0 .or. minn.eq.minn1) then
         minn = minn1
      else
         call mabend(
     &     'FUNCTION IN GRID D.S. INCOMPATIBLE WITH USER SPECIFICATION')
      endif
      call prnt(4,1,minn,' MINN=.')

c get grid parameters from label
      call xlget( grid, 'HISTORY', 'DLAT', gdlat, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'DLON', gdlon, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'SLAT', gslat, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'SLON', gslon, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'NUMLAT', numlat, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call xlget( grid, 'HISTORY', 'NUMLON', numlon, istat,' ')
      call chkstat( istat, 'ERROR IN GRID LABEL', 1)
      call prnt(7,4,rbuf,' GRID DLAT,DLON,SLAT,SLON=.')
      call prnt(4,2,ibuf(5),' GRID NUMLAT,NUMLON=.')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create photb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c ********************************************************************
      subroutine photb(buf,l1,tbrite,l2,bbrite,l3,
     +          cmbuf,l4,total,nsox,ncol)
      implicit none
      integer l1,l2,l3,l4,total
      integer nsox,ncol
      real*4 buf(nsox)			!input and output image line
      real*4 tbrite(ncol),bbrite(ncol)	!correction for top and bottom rows
      integer*2 cmbuf(nsox)		!classification map line
      real gbuf(1)		!****this has to be assigned via stacka*****

      common/units/iunit,ounit,maxdn,fmt
      integer iunit,ounit,maxdn
      character*5 fmt

      common/pf0/linc,sinc,grid,term,limb,const,maxcor,nointerp
      integer linc,sinc,grid
      real term,limb,const,maxcor
      logical*1 nointerp

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      integer irow,icol,nrow
      integer jl,line,samp,nline,nsamp
      integer numcor1,numcor2,numoff,ind,j,num,iprnt
      real ul,ur,ll,lr,vall,valr
      logical xvptst

      character*75 msg1/
     &'   LINE    SAMPLE  LATITUDE LONGITUDE  INC    EMI   PHASE   BRIGHT COLUMN'/

c Photfunc computes the normalized brightnesses for an nrow x ncol grid of
c tiepoints.  Each four adjacent tiepoints form the corners of a linc x sinc
c box.  Therefore, the the image is divided into (nrow-1) x (ncol-1) boxes.
c For the pixels within a box, the brightnesses are interpolated from these
c four corners.

c If any of the corners of the box is off the target or unilluminated, the
c brightnesses are computed exactly at each pixel in the box.

      iprnt = xvptst('ALL')             !print all points
      if (xvptst('PRINT')) iprnt=2	!print illuminated pts
      if (iprnt.gt.0) call xvmessage( msg1,' ')

      if (total.ne.l1+l2+l3+l4) call mabend(' STACKA ERROR')

      nrow = (nlo-2)/linc + 2

      if (xvptst('NOCORREC') .or. ounit.eq.0) then	!if no correction,
         line = sl
         do irow=1,nrow			!print out brights for each box
            call setup(iprnt,gbuf,line,ncol,bbrite,numoff)
            line = line + linc
         enddo
         return                         !and exit.
      endif

      numcor1 = 0		!number of pixels interpolated
      numcor2 = 0		!number of pixels computed exactly
      numoff = 0		!number of grid points off limb/term

c compute normalized photometric function for top row of tiepoints.
      call setup(iprnt,gbuf,sl,ncol,bbrite,numoff)

      do 50 irow=1,nrow-1		!loop thru vertical row of boxes
      j = (irow-1)*linc			!line relative to output image
      line = sl + j			!line relative to input image
      nline = min0(nlo-j,linc)		!number of lines in box

      call mve(7,ncol,bbrite,tbrite,1,1)!copy brights from bottom to top
      call setup(iprnt,gbuf,line+nline,ncol,bbrite,numoff)

      do 45 jl=0,nline-1
      call xvread(iunit,buf,ind,'LINE',line,'SAMP',ss,'NSAMPS',nso,' ')
      if (cmap.gt.0) then		!read line from classication map
         call cm_read(cmap,cmbuf,line,class,ind)
         if (ind.eq.0) then		!if no pixels on line in class
            call mve(7,nso,0.,buf,0,1)	!zero out the output line
            goto 42
         endif
      endif

      do 40 icol=1,ncol-1		!loop thru horizontal row of boxes
      j = (icol-1)*sinc + 1		!samp relative to output image
      samp = ss + j - 1			!samp relative to input image
      nsamp = min0(nso-j+1,sinc)	!pixel length of line segment
      ul=tbrite(icol)
      ur=tbrite(icol+1)
      ll=bbrite(icol)
      lr=bbrite(icol+1)

      num=0		!count # of corners on target and within limb and term
      if (ul.gt.0.0) num=num+1
      if (ll.gt.0.0) num=num+1
      if (ur.gt.0.0) num=num+1
      if (lr.gt.0.0) num=num+1
      if (num.eq.0) then			!if all 4 corners are bad
         call mve(7,nsamp,0.,buf(j),0,1)	!zero out segment
         goto 40
      endif
      if (nointerp) num=0	!force pixel-by-pixel computation
      if (num.eq.4) then	!if all 4 pts are illuminated, interpolate
         vall = jl*(ll-ul)/nline + ul	!brightness at left end of segment
         valr = jl*(lr-ur)/nline + ur	!brightness at right end of segment
         call interp_segment(buf(j),cmbuf(j),vall,valr,nsamp,numcor1)
      else
         call compute_segment(buf(j),cmbuf(j),line,samp,nsamp,numcor2)
      endif
   40 continue		!end horizontal loop thru ncol boxes

   42 call xvwrit(ounit,buf,ind,' ')
   45 line = line + 1	!end line loop thru a row of boxes

   50 continue		!end vertical loop thru nrow boxes

      call prnt(4,1,numcor1,' # of pixels interpolated =.')
      call prnt(4,1,numcor2,' # of pixels computed exactly =.')
      call prnt(4,1,numoff,' Number of grid points flagged=.')
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read line from classification map into cmbuf.  Upon return cmbuf(i)=1
c if the pixel should be corrected, =0 otherwise.
c
      subroutine cm_read(cmap,cmbuf,line,class,ind)
      implicit none
      integer*2 cmbuf(1)
      integer cmap,line,class
      integer ind		!=0 if line is empty, =1 otherwise

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      integer i

      call xvread(cmap,cmbuf,ind,'LINE',line,'SAMP',ss,
     +		'NSAMPS',nso,' ')

      if (class.lt.0) goto 10
      ind = 0		!no class members on this line
      do i=1,nso
         if (cmbuf(i).eq.class) then
            cmbuf(i) = 1
            ind = 1	!class member found
         else
            cmbuf(i) = 0
         endif
      enddo
      return

c here for complement operation
   10 continue
      ind = 0

      do i=1,nso
         if (cmbuf(i).ne.-class .and. cmbuf(i).ne.0) then
            cmbuf(i) = 1
            ind = 1
         else
            cmbuf(i) = 0
         endif
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_segment.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Correct a line segment of a box by computing the brightness correction at
c each pixel of the segment.
c
      subroutine compute_segment(buf,cmbuf,line,samp,nsamp,numcor)
      implicit none
      integer nsamp		!pixel length of segment
      real*4 buf(nsamp)		!input and output line segment
      integer*2 cmbuf(nsamp)	!classification map for segment
      integer line,samp		!pixel coords at left end of segment
      integer numcor		!number of pixels corrected

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      integer j,ind
      real value,rdn,truncate
      real*8 v(3)

      do 10 j=1,nsamp		!loop thru samples of a box
      rdn = buf(j)
      buf(j) = 0.
      if (cmap.gt.0 .and. cmbuf(j).eq.0) goto 10	!skip pixel not in class
      call toplanet(line,samp,v,ind)
      if (ind.ne.1) goto 10		!skip if point off the target
      call phot_sub(v,0.,0.,value)	!compute brightness correction
      value = value*const
      if (value.lt.maxcor) goto 10
      buf(j) = truncate(rdn/value)	!correct the sample
      numcor = numcor+1
   10 samp = samp + 1			!move to next pixel of segment

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Correct a line segment of a box by interpolating between the brightness
c corrections for the left and right ends.
c
      subroutine interp_segment(buf,cmbuf,vall,valr,nsamp,numcor)
      implicit none
      integer nsamp		!pixel length of segment
      real*4 buf(nsamp)		!input and output line segment
      integer*2 cmbuf(nsamp)	!classification map for segment
      real vall,valr		!corrections for left and right ends
      integer numcor		!number of pixels corrected

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/cm/cmap,class      !classification map unit number and class
      integer cmap,class

      integer j
      real value,dval,rdn,truncate

      dval=(valr-vall)/sinc     !delta brightness correction per sample
      value = vall		!brightness correction at current pixel

      do 10 j=1,nsamp		!loop thru samples of a box
      rdn = buf(j)
      buf(j) = 0.
      if (cmap.gt.0 .and. cmbuf(j).eq.0) goto 10	!skip pixel not in class
      if (value.lt.maxcor) goto 10	!skip if too large a correction
      buf(j) = truncate(rdn/value)	!correct the sample
      numcor = numcor + 1
   10 value = value + dval	!update correction for next pixel

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Truncate dn value to size of output format.
c
      function truncate(rdn)
      implicit none
      real truncate,rdn

      common/units/iunit,ounit,maxdn,fmt
      integer iunit,ounit,maxdn
      character*5 fmt

      common/cmax/maxfull
      real maxfull        !largest 32-bit integer (approximate)

      integer idn

      if (fmt.eq.'REAL') then
         truncate = rdn
         return
      endif

      if (rdn.gt.maxfull) rdn=maxfull	!make sure we don't overflow
         
      idn = rdn + 0.5
      if (idn.gt.maxdn) idn=maxdn	!and truncate to output range.
      if (idn.lt.0) idn=0
      truncate = idn
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create setup.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c ********************************************************************
c set up the brightness correction buffer for photfunc
c
      subroutine setup(iprnt,gbuf,line,ncol,bbrite,numoff)
      implicit none
      integer iprnt		!print brightness values
      real gbuf(1)		!grid buffer
      integer line		!image line number
      integer ncol		!ncol = nso/sinc
      real bbrite(ncol)		!returned bright values
      integer numoff		!running count of bad points

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      common/konst/rad
      real rad

      integer i,j,icol,samp,nsamp,ind
      real pdata(5)
      real*8 v(3),gcr,dlat,dlon

c bbrite(icol)=0.   means point off target or past terminator.
c bbrite(icol)=-1.  means point off target or past terminator.
c                   and violates limb * term limits.
c bbrite(icol)=>0.   means point ok.

      do 100 icol=1,ncol
      j = (icol-2)*sinc
      nsamp = min0(nso-j,sinc)
      samp = ss + j + nsamp
      call toplanet(line,samp,v,ind)
      if (ind.ne.1) then
         numoff=numoff+1		!point is off the target
         bbrite(icol)=0.0
         goto 100
      endif

      if (iprnt.gt.0 .or. grid.ne.0) then
         call reclat(v,gcr,dlon,dlat)
         dlon = 360.-dlon*rad
         dlat = dlat*rad
      endif

      if (grid.ne.0) then
         call interp(grid,dlat,dlon,gbuf,pdata)
         k=pdata(1)	!note this code is not valid for ihapke > 1
         w=pdata(1)
         b=pdata(2)
         h=pdata(3)
         c=pdata(4)
         ck=pdata(5)
      endif
         
      call phot_sub(v,limb,term,bbrite(icol)) !compute correction
      if (bbrite(icol).eq.0.) numoff=numoff+1

      if (iprnt.gt.0) call xprint(iprnt,line,samp,
     +		dlat,dlon,bbrite(icol),icol)
  100 continue

      do i=1,ncol
         bbrite(i)=bbrite(i)*const	!add in free multiplicative constant
      enddo
      return
      end

c ********************************************************************
      subroutine interp(grid,lat,lon,buf,pdata)
      implicit none
      integer grid
      real lat,lon,buf(1),pdata(5)

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

       common/c9/gdlat,gdlon,gslat,gslon,numlat,numlon,nlrg,nlg,nbg
     &    ,numsam
      real gdlat,gdlon,gslat,gslon
      integer numlat,numlon,nlrg,nlg,nbg,numsam

      integer nwords,numgrp,num,igrp,jgrp,line,samp,ilat,ilon
      integer i,ind
      real dellat,dellon,vall,valr
      real ul(5),ur(5),ll(5),lr(5)

      if (minn.gt.2 .or. minn.lt.1) return	!skip if minnaert or hapke
      if (minn.eq.1) nwords=2
      if (minn.eq.2) then
         nwords=5
         if (ihapke.eq.1 .and. icook.eq.0) nwords=4
      endif

      numgrp = numsam/nwords	!number of groups (lat/lon points) per line
      if (lon.gt.gslon) then
         ilon = (lon-gslon)/gdlat+1
      else
	ilon = (360.+lon-gslon)/gdlat+1
      endif
      ilat = (lat-gslat)/gdlat+1
      igrp = (ilat-1)*numlon+ilon
      jgrp = mod(igrp-1,numgrp)+1
      line = (igrp-jgrp)/numgrp+1
      samp = 1+(igrp-1)*nwords

      call xvread(grid, buf, ind, 'LINE', line,' ')
      call mve(4,nwords,buf(samp),ul,1,1)
      call mve(4,nwords,buf(samp+nwords),ur,1,1)
      call xvread(grid, buf, ind, 'LINE', line+1,' ')
      call mve(4,nwords,buf(samp),ll,1,1)
      call mve(4,nwords,buf(samp+nwords),lr,1,1)

c  interpolate
c  find position in box relative to upper left
      dellat = (lat-gslat+(ilat-1)*gdlat)/gdlat
      dellon = (lon-gslon+(ilon-1)*gdlon)/gdlon

c  compute values on left and right side of box
      num = nwords
      do i=1,num
         vall=(ll(i)-ul(i))*dellat+ul(i)
         valr=(lr(i)-ur(i))*dellat+ur(i)
         pdata(i)=(valr-vall)*dellon+vall
      enddo
      return
      end

c********************************************************************
      subroutine xprint(iprnt,line,samp,lat,long,val,icol)
      implicit none
      integer iprnt,line,samp,icol
      real lat,long,val

      common/pf4/refl,ci,ce,cg,tg,rlat,rlon
      real refl,ci,ce,cg,tg,rlat,rlon

      common/konst/rad
      real rad

      real inc,emis,phase	!incidence, emission, and phase angles

      character*132 msg
      data msg /' '/

      if (iprnt.eq.2 .and. ci.lt.0.) return	!skip unilluminated points

      inc=(acos(ci))*rad
      emis=(acos(ce))*rad
      phase=(acos(cg))*rad

      write(msg(1:7), '(I7)') line		
      write(msg(8:15), '(I8)') samp		
      write(msg(17:25), '(f9.4)') lat		
      write(msg(26:35), '(f10.4)') long		
      write(msg(38:43), '(f6.2)') inc		
      write(msg(45:50), '(f6.2)') emis		
      write(msg(51:57), '(f7.2)') phase		
      write(msg(60:65), '(f6.3)') val		
      write(msg(66:70), '(I4)') icol		
      call xvmessage(msg,' ')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create phot_sub.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute normalized brightness correction for a given lat,lon of an image
c
      subroutine phot_sub(v,limb,term,bbrite)
      implicit none
      real*8 v(3)		!vector from target-center to surface point
      real limb,term		!limb and terminator thresholds
      real bbrite		!output brightness correction

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      common/hapke_norm/bb1             !hapke function at zero phase & i & e
      common/buratti_norm/bur_norm      !buratti function at zero phase
      real bb1,bur_norm

      real bb0,phase,result
      real data1(5)/0.,1.,1.,1.,0./     !normal angles for hapke

      common/konst/rad
      real rad

      common/pf4/refl,ci,ce,cg,tg
      real refl,ci,ce,cg,tg,data(5)
      equivalence (data,refl)

      real fhapke
      external fhapke

c  statement function for several photometric functions:
      real pac,a,dd
      pac(a,b,c,dd,phase)=a+b*phase+c*exp(-dd*phase)/(a+c)

c compute tg,cg,ci,ce
      call light_angles(v,tg,cg,ci,ce)
      bbrite = 0.0
      if (ci.le.0.0.or.ce.le.0.0) goto 580
      if (ci.le.term.or.ce.le.limb) goto 580

c             1   2   3   4   5   6   7
      go to (560,565,570,571,572,573,574),minn

c  minnaert function:
  560 if(ce.ne.0.0)then 
        bbrite = ((ci*ce)**k)/ce	!(ci**k)*ce**(k-1.)
      else
        bbrite=0.0
      endif
      go to 580

c  hapke functions:
  565 bb0 = fhapke(rad,data,w,b,h,ck,c,xlg2,icook,ihapke)
      if (bb1.eq.0.0) then	!compute normalizing factor
        bb1 = fhapke(rad,data1,w,b,h,ck,c,xlg2,icook,ihapke)
      endif
      bbrite = bb0/bb1
      go to 580

c  lambert function:
  570 bbrite = ci
      go to 580

c  veverka function:
  571 phase=abs(acos(cg))*rad
      bbrite = pac(d(1),d(2),d(3),d(4),phase)*2.*ci/(ce+ci)
      go to 580

c  mosher function:
  572 phase=abs(acos(cg))*rad
      bbrite = pac(d(1),d(2),d(3),d(4),phase) * 
     & (ci**(d(5)+d(6)*phase)*(ce**(d(5)+d(6)*phase-1.)))
      go to 580

c  buratti function
c  note the term: 2/(1+d(1)) is the ci=ce=0 normalizing term.
  573 phase=abs(acos(cg))*rad
      call buratti(phase,ci,ce,rad,d,result)
      bbrite=result
      goto 580

c irvine function
574   continue
      bbrite=(((ci*ce)**d(1))/ce)*(1.0-exp(-ci/d(2)))/
     + (1.0-exp(-ce/d(3)))
      goto 580

c  put other functions in here
c  ...

  580 continue
      return
      end

c *******************************************************************
      function fhapke(rad,data,w,b,h,ck,c,xlg2,icook,ihapke)

c  ihapke = 1:  old (1978) hapke fcn.
c               icook=1:  cook modification of above
c  ihapke = 2:  new (1984) hapke fcn. with henyey-greenstein phase fcn.
c  ihapke = 3:  new (1984) hapke fcn. with legendre polynomial phase fcn.

c     w=single scattering albedo 
c     h=porosity (shadowing, angular spread of internally reflected light,
c       larger h = larger width of opposition peak)
c     b,c,ck,xlg2 determine particle phase function and opposition term
c       amplitude in varying ways.

c  data(1)=reflectance
c  data(2)=cos(incidence)
c  data(3)=cos(emission)
c  data(4)=cos(phase)
c  data(5)=tan(phase)

      real*4 data(5),rad,mu

c  statement function definitions:
      hh(w,mu)=(1.+2.*mu)/(1.+2.*mu*sqrt(1.-w))
      cook(a,b)=sqrt(1.0-a*a*(1.0-b*b))

      if (ihapke.ge.2) then
	xi = acos(data(2))*rad
	xe = acos(data(3))*rad
	xp = acos(data(4))*rad
C To call hapke with 8 params; set 1st arg to 8 and put a value in for xlg2
C it simply does not get used int the function
	if (ihapke.eq.2) then
		xlg2 = -1.0
		fhapke = hapke(8,xi,xe,xp,w,h,ck,c,b,xlg2)
	endif
	if (ihapke.eq.3) fhapke = hapke(9,xi,xe,xp,w,h,ck,c,b,xlg2)
	return
      endif

      ci=data(2)
      ce=data(3)
      if(icook.eq.0)go to 20
      ci=cook(ck,ci)
      ce=cook(ck,ce)
   20 continue
      fhapke=data(1)
      x=data(2)+data(3)
      if(x.eq.0.0) return
      x1=data(2)*w/x
      bb=0.0
      if(data(5).le.0.0) go to 10
      x2=exp(-w*w/2.)
      x3=exp(-h/data(5))
      bb=x2*(1.0-(data(5)/(2.*h))*(3.-x3)*(1.-x3))
10    x=data(2)
      hi=hh(w,x)
      x=data(3)
      he=hh(w,x)
      p=1.0+b*data(4)+c*(3.*data(4)*data(4)-1.)/2.
      fhapke=x1*((1.0+bb)*p+hi*he-1.0)
      return
      end

c ********************************************************************
	real function hapke(nargs,i,e,g,w,h,tbar,opmag,aa,bb)
c ------------------------------------------------------------------ 
c fcn.hapke calculates rough-surface reflectance using a new version
c           of hapke'S PHOTOMETRIC FUNCTION.
c this version is a simplified version of that used by paul helfenstein
c in his program 'GENERAL_HAPKE'.   the simplification lies in the
c scattering phase function, which can here only be a 1-parameter
c henyey-greenstein function or a 2-parameter legendre polynomial.
c
c inputs:
C      nargs = number of arguments to use: 8 or 9
c      i = incidence angle in degrees
c      e = emergence angle in degrees
c      g = phase angle in degrees
c      w = single scattering coefficient
c      h = backscatter parameter related to soil porosity
c   tbar = average macaroscopic slope angle (in degrees)
c  opmag = s(0) term in oppostion magnitude coefficient, b0
c  aa,bb = constants in the phase function.
c ** note **: if only aa is specified, then the henyey-greenstein phase
c             function is used.  if both aa and bb are specified, then
c             a two-parameter legendre polynomial is used.
c ------------------------------------------------------------------ 
c
	real mu0,mu01,mu02,mu1,mu11,mu12,i,i0
c
c------> define statement function for hapke'S H-FUNCTION <-------------
c
	hfnctn(x0,x1)=(1.0+2.0*x0)/(1.0+2.0*x0*sqrt(1.0-x1))
c
c       note: x0 = cos(i) or cos(e) and x1 = w
c
c  phase function:
        bb1 = -999.9			! henyey-greenstein
        if (nargs .eq. 9) bb1 = bb	! legendre polynomial
C
C I think nargs() does not exist so passed in nargs above - dpp
C        if (nargs().eq.9) bb1 = bb	! legendre polynomial
c
c------- determine principal variable values -------------------------
c
	pi = 3.141592654
	raddeg = pi/180.
	i0 = i*raddeg
	e0 = e*raddeg
	g0 = g*raddeg
	thebar = tbar*raddeg
c
	cosi = cos(i0)
	cose = cos(e0)
	cosg = cos(g0)
	sini = sin(i0)
	sine = sin(e0)
	sing = sin(g0)
c
c  determine the azimuthal angle psi and related quantities:
	if (i0.eq.0.0 .or. e0.eq.0.0) then
	  cospsi=1.0
	else
	  cospsi = (cosg-cosi*cose)/(sini*sine)
	endif
	if (cospsi.ge.1.0) cospsi=1.0
	if (cospsi.le.-1.0) cospsi=-1.0
	psi = acos(cospsi)
	sinps2 = sin(0.5*psi)
	sinps2 = sinps2*sinps2
	if (abs(psi).gt.3.10) then
	  f = 0.0
	else
	  f = tan(0.5*psi)
	  f = exp(-2.0*f)
	endif
c
c  determine the backscatter function b(g) 
c
	tstmag = w*sppf(0.0,aa,bb1)
	bg = bsf(w,h,opmag,tstmag,g0)
c
c  determine p(g): average single particle scattering function
c
	pg = sppf(g0,aa,bb1)
c
c---- values for delta variables: first non-case dependant terms---
c
c  if average macroscopic slopes=0, then all delta'S=0 
	if (thebar.gt.0.0) go to 105
	mu0  = cosi
	mu01 = cosi
	mu02 = cosi
	mu1  = cose
	mu11 = cose
	mu12 = cose
	y = 1.0
	go to 500
c
  105	if (thebar.gt.1.4) thebar = 1.4
	tant = tan(thebar)
	cott = 1.0/tant
	beta = sqrt(1.0 + pi*tant*tant)
c
c------- exponential terms first --------------------------
c
	if (abs(sine).lt.1.e-04) then
	  ee1 = 0.0
	  ee2 = 0.0
	else
	  cote = cose/sine
	  ee2 = -2.0*cott*cote/pi
	  ee1 = -0.25*pi*ee2*ee2
	  if(ee1.gt.(87.0))ee1=87.0
	  if(ee2.gt.(87.0))ee2=87.0
	  ee1 = exp(ee1)
	  ee2 = exp(ee2)
	  if(ee1.gt.(1.0e+20))ee1=1.0e+20
	  if(ee2.gt.(1.0e+20))ee2=1.0e+20
	endif
c
	if (abs(sini).lt.1.e-04) then
	  ei1 = 0.0
	  ei2 = 0.0
	else
	  coti = cosi/sini
	  ei2 = -2.0*cott*coti/pi
	  ei1 = -0.25*pi*ei2*ei2
	  if(ei1.gt.(87.0))ei1 = 87.0
	  if(ei2.gt.(87.0))ei2 = 87.0
	  ei1 = exp(ei1)
	  ei2 = exp(ei2)
	  if(ei1.gt.(1.0e+20))ei1=1.0e+20
	  if(ei2.gt.(1.0e+20))ei2=1.0e+20
	endif
c
	mu0 = cosi
	mu1 = cose
	mu02 = (cosi + sini*tant*(ei1/(2.0-ei2)))/beta
	mu12 = (cose + sine*tant*(ee1/(2.0-ee2)))/beta
c
c------------------------------------------------------------------
c---- now, determine which case is appropriate for the model ------
c----      case #1: i<=e      case #2: i>=e                  ------
c------------------------------------------------------------------
	if (i.le.e) then
	  xi2 = 2.0 - ee2 - (psi/pi)*ei2
	  xi1 = (cospsi*ee1 + sinps2*ei1)
	  mu01 = (cosi + sini*tant*(xi1/xi2))/beta
	  xi3 = (ee1 - sinps2*ei1)
	  mu11 = (cose + sine*tant*(xi3/xi2))/beta
	  y = beta - f*beta + f*(mu0/mu02)
	else
	  xi2 = 2.0 - ei2 - (psi/pi)*ee2
	  xi1 = (ei1 - sinps2*ee1)
	  mu01 = (cosi + sini*tant*(xi1/xi2))/beta
	  xi3 = (cospsi*ei1 + sinps2*ee1)
	  mu11 = (cose + sine*tant*(xi3/xi2))/beta
	  y = beta - f*beta + f*(mu1/mu12)
	endif
c
c--------- now, calculate the complete photometric reflectance ------
c
  500   if (w.ge.0.99998) w=0.99998
	hapcof = (0.25*w)*(mu01/(mu01+mu11))
	sfnct = (mu0/mu02)*(mu11/mu12)/y
	bgfnct = bg
	pgfnct = pg
	hfncti = hfnctn(mu01,w)
	hfncte = hfnctn(mu11,w)
c
	hapke = hapcof*((1.0+bgfnct)*pgfnct-1.0+hfncti*hfncte)*sfnct
c
	return
	end

c-------------------------------------------------------------------------
c-- function bsf:  computes the regolith backscatter function b(g)     ---
c--                                                                    ---
c-- inputs:  w:       single-scattering albedo                         ---
c--          h:       particle compaction parameter                    ---
c--          opmag:   s0 parameter of hapke'S NEW B(G)                 ---
c--          tstmag:  particle phase function                          ---
c--          g0:      phase angle                                      ---
c-------------------------------------------------------------------------
c
c ***********************************************************************
        function bsf(w,h,opmag,tstmag,g0)

	bsf = 0.0
        if (h.le.0.0) return

	if (tstmag.lt.0.000001) tstmag=0.000001
	b0 = opmag/tstmag
	if (abs(g0).le.0.0001) then
	  bsf = b0*h/(h+abs(0.5*g0))
	else
	  bsf = b0*h/(h+tan(abs(0.5*g0)))
	endif

        return
	end
c---------------------------------------------------------------------
c--- sppf - this routine computes the average single-particle phase function
c   using either a one-parameter henyey-greenstein function or a two-
c   parameter legendre polynomial.
c---------------------------------------------------------------------
c--- input parameters:                                         -
c---  phangl   phase angle in radians.
c---  pt1,pt2  phase function parameters
c---------------------------------------------------------------------
c
c ******************************************************************
	function sppf(phangl,pt1,pt2)
c
	pi = 3.141592654
c
        if (pt2.le.-999.) then		! henyey-greenstein
	  g1 = pt1
	  x = cos(pi-phangl)
	  bottom = 1.5*log(1.0+g1*g1-2.0*g1*x)
	  bottom = exp(bottom)
	  sppf = (1.0-g1*g1)/bottom

	else				! legendre polynomial
	  g1 = cos(phangl)
	  sppf = 1.0+pt1*g1+0.5*pt2*(3.*g1*g1-1.)

	endif
c
	return
	end

c **************************************************************
      subroutine buratti(phase,ci,ce,rad,d,result)
      real*4 d(6)      
      common/buratti_norm/bur_norm  ! the buratti function at zero phase

      pi=3.141592654
      if(bur_norm.eq.0.0)then
         bur_norm=(d(2)+d(4)-1.0)*(1.0-d(1))*(2.0/3.0) +
     +             d(1)*(d(2)+d(4))*d(6) + (1.0-d(1))
      endif
      bur1=d(2)+d(3)*phase+d(4)*exp(-d(5)*phase) ! surface phase fcn
      ph = phase/rad		!convert to radians

      if (phase.ne.0.0) then
         bur2=(d(1)*pi/2.0)*(1.0-sin(ph/2.0)*tan(ph/2.0)*
     +      log(1.0/tan(ph/4.0)))
      else
         bur2=d(1)*pi/2.0
      endif

      bur3=(2.0/3.0)*(1.0-d(1))*(sin(ph)+(pi-ph)*cos(ph))
      bur4=bur1*pi*((2.0/3.0)*(1.0-d(1))+d(1)*d(6))
      burf=(bur4-bur3)/bur2
      bur=d(1)*(ci/(ce+ci))*burf+(1.0-d(1))*ci
      result=bur/bur_norm
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create toplanet.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Convert from (line,sample) to vector from target-center to surface point.
c
      subroutine toplanet(line,samp,v,ind)
      implicit none
      integer*4 line,samp
      real*8 v(3)
      integer ind	!1=normal, other=point off planet

      common/cmp/mp
      real*8 mp

      common/cplanet/vsc,ra,rb,rc
      real*8 vsc(3),ra,rb,rc

      common/c3/rdata
      real rdata(40)
      real*8 data(20),om(3,3),rs(3)
      real fic,lo,so,pixpmm             !camera focal length, op-axis, scale
      integer idata(40),itype
      equivalence (data,rdata,idata),(data(1),om),(data(10),rs),
     * (rdata(27),fic),(rdata(28),lo),(rdata(29),so),(rdata(30),pixpmm),
     * (idata(39),itype)

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      common/konst/rad
      real rad

      real is_line,is_samp,os_line,os_samp
      real*8 rline,rsamp,rlat,rlon,gcr,lora
      integer ll_type

      if (idist.eq.1) then
         is_line = line
         is_samp = samp
         call convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +               1,conv(9),nph,npv,ind)
         rline = os_line
         rsamp = os_samp
      else
         rline = line
         rsamp = samp
      endif

      if (itype.ne.16) then
         ll_type = 1
         call mp_xy2ll(mp,rline,rsamp,rlat,rlon,ll_type,ind)
         if (ind.eq.0) ind=1
         lora = rdata(36)
         call ellipse_radius(rlat,rlon,ra,rb,rc,lora,gcr)
         call latrec(gcr,-rlon/rad,rlat/rad,v)
      else
         call ellipse_inv(om,rs,fic,lo,so,pixpmm,ra,rb,rc,rline,rsamp,
     +		v,ind)
      endif
      return
      end

cccccccccccccccccccccccccccccccc
c Calculate phase, incidence, and emission angles.
c
      subroutine light_angles(v,tp,cp,ci,ce)
      implicit none
      real*8 v(3)		!Point where angles are calculated
      real tp,cp		!Tangent and cosine of phase angle
      real ci,ce		!Cosine of incidence and emission angles

      common/cplanet/vsc,ra,rb,rc
      real*8 vsc(3),ra,rb,rc

      common/csun/vsun,s,ax,bx,cx
      real*8 vsun(3),s(3),ax,bx,cx

      real*8 n(3),c(3)	!unit vectors
      real*8 magnitude,sp,dcp

c compute unit normal at surface point
      n(1) = v(1)*ax
      n(2) = v(2)*bx
      n(3) = v(3)*cx
      call unorm(n,n,magnitude)      

c compute unit vector from surface point to spacecraft
      c(1) = vsc(1) - v(1)
      c(2) = vsc(2) - v(2)
      c(3) = vsc(3) - v(3)
      call unorm(c,c,magnitude)

c The unit vector from surface point to sun is assumed to be constant for the
c entire image.  Tests show this to be valid, even for Mercury.  The vector
c is therefore computed once in the main routine.
ccc      s(1) = vsun(1) - v(1)
ccc      s(2) = vsun(2) - v(2)
ccc      s(3) = vsun(3) - v(3)
ccc      call unorm(s,s,magnitude)

c cosine of phase angle = S o C
      dcp = s(1)*c(1) + s(2)*c(2) + s(3)*c(3)
      if (dcp.gt.1.d0) dcp=1.d0
      if (dcp.lt.-1.d0) dcp=-1.d0
      cp = dcp

c tangent of phase angle:
      sp = dsqrt(1.d0-dcp**2)
      if (dabs(dcp).gt.0.000000001d0) then
         tp = sp/dcp
      else
         tp = 1000.
      endif

c cosine of incidence angle = n o s
      ci = n(1)*s(1)+n(2)*s(2)+n(3)*s(3)
      if (ci.gt.1.d0) ci=1.d0
      if (ci.lt.-1.d0) ci=-1.d0

c cosine of emission angle = n o c
      ce = n(1)*c(1)+n(2)*c(2)+n(3)*c(3)
      if (ce.gt.1.d0) ce=1.d0
      if (ce.lt.-1.d0) ce=-1.d0
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create photfunc.pdf
process help=*
PARM INP          TYPE=STRING       COUNT=(1:4)
PARM OUT          TYPE=STRING       COUNT=0:1      DEFAULT=--
PARM SIZE         TYPE=INTEGER      COUNT=4        DEFAULT=(1,1,0,0)
PARM SL           TYPE=INTEGER                     DEFAULT=1
PARM SS           TYPE=INTEGER                     DEFAULT=1
PARM NL           TYPE=INTEGER                     DEFAULT=0
PARM NS           TYPE=INTEGER                     DEFAULT=0
PARM MINNAERT     TYPE=REAL                        DEFAULT=0.5
PARM HAPKE        TYPE=REAL COUNT=(0,4:6)          DEFAULT=--
PARM COOK         TYPE=REAL COUNT=0:1              DEFAULT=--
PARM VEVERKA      TYPE=REAL COUNT=(0,4)            DEFAULT=--
PARM BURATTI      TYPE=REAL COUNT=(0,6)            DEFAULT=--
PARM MOSHER       TYPE=REAL COUNT=(0,6)            DEFAULT=--
PARM IRVINE       TYPE=REAL COUNT=(0,3)            DEFAULT=--
PARM NOINTERP     KEYWORD   COUNT=0:1  VALID=NOINTERP	DEFAULT=--
PARM INCR         TYPE=INTEGER                     DEFAULT=10
PARM LINC         TYPE=INTEGER                     DEFAULT=10
PARM SINC         TYPE=INTEGER                     DEFAULT=10
PARM LIMB     TYPE=REAL                            DEFAULT=0.
PARM TERMINAT TYPE=REAL                            DEFAULT=0.
PARM MULTIPLY TYPE=REAL                            DEFAULT=1.
PARM MAXCOR   TYPE=REAL COUNT=0:1 VALID=0.1:99999  DEFAULT=5.
PARM MAXDN    TYPE=INTEGER                         DEFAULT=255
PARM CLASS    TYPE=INTEGER   VALID=(-255:255)      DEFAULT=-255
PARM TARGET     TYPE=(STRING,12) COUNT=0:1                      DEFAULT=--
PARM RADII    TYPE=REAL COUNT=3  VALID=(1.:99999.) DEFAULT=(1.0,1.0,1.0)
PARM FARENC       KEYWORD   COUNT=0:1 VALID=FARENC DEFAULT=-- 
PARM NORANGLE     TYPE=REAL COUNT=0:1		   DEFAULT=--
PARM SSCPT        TYPE=REAL COUNT=2          DEFAULT=(0.,0.)
PARM OSSCPT       TYPE=REAL COUNT=2          DEFAULT=(0.,0.)
PARM ISSCPT       TYPE=REAL COUNT=2          DEFAULT=(0.,0.)
PARM TIEPOINT     TYPE=REAL COUNT=1:20       DEFAULT=0.
PARM MISSION      KEYWORD   COUNT=0:1              DEFAULT=-- +
  VALID=(VGR-1,VGR-2,GLL,CASSI,WFPC1,WFPC2,SIPS,QUEST,VIKOR,MAR-9,MAR10)
PARM NOSPICE       KEYWORD   COUNT=0:1 VALID=NOSPICE DEFAULT=--
PARM OBJECT       KEYWORD  COUNT=0:1 VALID=OBJECT  DEFAULT=-- 
PARM IMAGE        KEYWORD  COUNT=0:1 VALID=IMAGE   DEFAULT=-- 
PARM DISTOR       KEYWORD  COUNT=0:1 VALID=DISTOR  DEFAULT=-- 
PARM CAMERA       TYPE=INTEGER                     DEFAULT=0
PARM FOCAL        TYPE=REAL COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
PARM FOCL         TYPE=REAL COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
PARM LAXIS        TYPE=REAL COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
PARM SAXIS        TYPE=REAL COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
PARM PSCALE       TYPE=REAL COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
PARM OMMATRIX     TYPE=REAL      COUNT=9  DEFAULT=(0.,0.,0.,0.,0.,0.,0.,0.,0.)
PARM RSVECTOR     TYPE=REAL      COUNT=3           DEFAULT=(0.,0.,0.)
PARM RMAGNITU TYPE=REAL                            DEFAULT=0.0
PARM SLATITUD TYPE=REAL                            DEFAULT=0.0
PARM SLONGITU TYPE=REAL                            DEFAULT=0.0
PARM SPACE    TYPE=REAL      COUNT=3               DEFAULT=(0.,0.,0.)
PARM SOLAR    TYPE=REAL      COUNT=2               DEFAULT=(0.,0.)
PARM PROJECT      KEYWORD   COUNT=0:1              DEFAULT=-- +
   VALID=(STEREOGR,LAMBERT,MERCATOR,ORTHOGRA,POLE,CYLINDRI,RECTANGU,LATLON)
PARM LINE         TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM SAMPLE       TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM LATITUDE     TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM LONGITUD     TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM SCALE        TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM NORTH        TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM PAR1         TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM PAR2         TYPE=REAL COUNT=0:1              DEFAULT=0.
PARM PRINT    KEYWORD COUNT=0:1 VALID=PRINT DEFAULT=--
PARM ALL      KEYWORD   COUNT=0:1 VALID=ALL        DEFAULT=-- 
PARM NOCORREC KEYWORD   COUNT=0:1 VALID=NOCORREC   DEFAULT=-- 
PARM SCET       TYPE=INTEGER  COUNT=0:6                         DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=0:1 +
VALID=("FARE ", "NEAR ","NAV  ","NAV2 ","DAVI ","NAIF ")	DEFAULT=--
PARM CKID       TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1                      DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1                DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1                        DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1                        DEFAULT=NONE
END-PROC

.TITLE
VICAR2 program PHOTFUNC
.HELP
PURPOSE:

PHOTFUNC corrects an image for spatial variations in apparent brightness caused
by illumination and viewing geometry.  This variation, known as the photometric
function, is removed to enable comparison of albedo differences on the target
surface.  In addition, the brightness correction enables the generation of
mosaics free from brightness differences along the edges of mosaic elements.

EXECUTION:

    PHOTFUNC INP=(INIMG[,GEOMA][,GRID][,CMAP]) [OUT=OUTIMG]  user-parameters...

  where: INIMG and OUTIMG are the input and output images,
         GEOMA is a geometric correction parameter file,
         GRID is a PHOTFIT grid file,
         CMAP is a classification map (see STATS, FASTCLAS),
	 [] brackets denote optional values.

Note: The grid input is currently deactivated (PHOTFIT is obsolete.
      See PHOTFIT2 and PHOTOM)

.page
OPERATION:  PHOTFUNC can be used on virtually any image of a planet or
satellite, provided that the projection and lighting geometry are known.
The image may be a raw flight image (image space), one that has been corrected
for geometric distortions (object space), or one that has been map projected.
The image may be arbitrarily large and of any data format.

The projection and lighting geometry is normally obtained by via the SPICE
server or from the image label.  Alternatively, these may be provided via
parameters.

Because the computations can be fairly expensive, the function is computed
exactly for a grid of points spaced m samples and n lines apart (see SINC,
LINC, and INCR parameters).  For intermediate points within the grid, the
function is computed using bilinear interpolation.  Note that with increasingly
faster computers, the need for the interpolation is rapidly diminishing.  Use
the 'NOINTERP keyword to force exact computations at each pixel.

PHOTFUNC can also operate in a second mode, in which no photometric 
correction is performed, but information relating to the illumination 
geometry and photometry is calculated and printed (see NOCORREC and PRINT
parameters).

Further information is given under the following headings below:

  THE PHOTOMETRIC FUNCTION
  PROJECTION AND LIGHTING GEOMETRY
  GEOMETRIC DISTORTIONS
  COMPUTING THE OM MATRIX
  COMPUTING THE LIGHTING ANGLES
  INTERPOLATION ALGORITHM
  CLASSIFICATION MAP OPTION
  EXAMPLES

.page
THE PHOTOMETRIC FUNCTION:  A model of the intrinsic reflectivity of a surface
is called a Photometric Function F:
		
	F(I,E,G,w,...) = B/J
where:
	B	is the observed reflected surface brightness,
	J	is the brightness incident on the surface,
	I	is the angle of incidence (i.e., of the sun),
	E	is the angle of emission (i.e., of the observer),
	G	is the phase, 
	w	is the albedo of surface particles, and
	...	denotes any other quantities which may contribute to the 
                functional relationship of B/J.
(See also HELP PHOTFIT2.)

PHOTFUNC uses such a model to correct the observed brightness to the
normalized brightness, B0, which is the brightness that would be 
observed with normal (90-degree) illumination and viewing angle
(I = E = G = 0).  Thus:

	B0/J = F(0,0,0,w,...)

and the correction factor f = B/B0 can be computed by dividing the above 
equations.  Then the apparent brightness B in the input picture is divided
by f in order to obtain an estimate of the normalized brightness B0 in the 
output picture. 

Currently, PHOTFUNC supports eight models for F:
  the Minnaert function:  F=B0*(COS(I)**k*COS(E)**(k-1),
    (the Lambert function is equivalent to Minnaert for k=1)
  three Hapke functions (1978 version, Cook modification, and 1984 version),
  the Veverka function, and
  the Mosher function (a variant of the Veverka function).
  the Buratti-Veverka function.
  the Irvine function.
See parameters MINNAERT, HAPKE, COOK, VEVERKA, BURATTI, MOSHER, and IRVINE.

Since the program actually only corrects by the normalized factor f, constant
factors, such as B0 in the first model, drop out and they are not needed
as input to the program.  (Thus Minnaert needs only 1 parameter in PHOTFUNC.)

When the gain introduced by the function is greater than a constant (see
MAXCOR parameter), no correction is applied.  All points which are uncorrected
for any reason (e.g. points off the target) are set to ZERO.

Planets also have a systematic brightness variation with solar phase
angle. Planetary science literature contains the magnitude phase
coefficients for satellites and planets. Burns (1977) "Planetary
Satellites" is one good source. If the geometric effects are removed,
the planet must still be brightened up by a factor obtained from the
magnitude phase coefficient depending on the phase angle. CAUTION:
The coefficients quoted in the literature usually refer to the
integrated disk of the planet. Care must be taken to account for any
phase defect (if the terminator is in the picture) which effectively
reduces the planet's area. By dividing the DNs in the picture by a
factor of (1+COS(G))/2 where G is the phase angle, the phase
defect can be removed. 

Also it may be necessary to determine the magnitude phase coefficient
if the picture was taken at a phase angle not previously seen and/or
different parts of the planet have different coefficients. The
coefficient can change over a range of phase angles. Callisto is a
good example. The leading hemisphere has a different coefficient than
the trailing hemisphere. Callisto's coefficient also changes at about
50 degrees phase. 

.page
PROJECTION AND LIGHTING GEOMETRY

To remove the photometric function from an image, the projection and
lighting geometry for the image must be known.  Under normal circumstances,
this information is automatically retrieved from the image label or by
accessing the SPICE server.  The following description is intended to help
the user to provide any missing information via parameters.

The projection and lighting information is used to:

(1) relate each pixel in the image to a surface point on the target body
(2) compute the incidence, emission, and phase angles of light reflected off
    that surface point.

Both of these problems require a model of the target body.  PHOTFUNC uses a
tri-axial ellipsoid model defined by the three radii of the ellipsoid (See
RADII parameter).

The projection information required depends on whether the input image is a
raw flight image or one that has been transformed to a standard map projection.

For raw images, the following information is required (user parameters in
parenthesis):

  camera constants: focal length, pixel scale, line-samp coordinates of
    optical axis intercept (FOCL, PSCALE, LAXIS, SAXIS)
  geometric distortion parameters: see section on GEOMETRIC DISTORTIONS below.
  spacecraft vector: lat-long coordinates of the spacecraft and it's distance
    from the center of the target body (SLAT, SLON, RMAG, SPACE, RSVECTOR).
  camera pointing direction: transformation matrix from (rotating) target-
    centered coordinates to camera-centered coordinates (OMMATRIX).

For map projected images, the projection geometry is defined by the following:

  type of projection: e.g. orthographic, stereographic, mercator (PROJECT)
  picture scale in pixels/km (SCALE)
  special point line-sample (LINE,SAMPLE)
  special point lat-long (LATITUDE,LONGITUD)
  special parallels (PAR1,PAR2) for Lambert projection only

The special point is different for each projection.  See program MAP3 for a
description of the above parameters.

The lighting geometry for the image is defined by:

  spacecraft vector: same as above
  solar vector: lat-long coordinates of the sun (SOLAR).

The program first determines whether the image has been map projected by
scanning for a map label (see programs MAP3, MAPTRANS).  The required projection
geometry is normally provided in this map label.  However, the map label
currently does not contain the lighting geometry (spacecraft and solar vectors).
The program will attempt to retrieve these vectors from the SPICE server using
the same strategy as for raw flight images (see immediately below).  The user
may avoid SPICE access by supplying these vectors via the SPACE and SOLAR
parameters.

For raw flight images, PHOTFUNC will first scan the image label to determine
the target name, spacecraft ID, camera ID, and Spacecraft-Event-Time (see
parameters TARGET, MISSION, CAMERA, and SCET).  For a "supported" mission (see
parameter MISSION), the camera constants and geometric distortion parameters
are automatically retrieved from built-in tables (via a call to getcamcon).
See also the section on GEOMETRIC DISTORTIONS below.

If SPICE data is available for the mission (e.g. Voyager, Galileo, Cassini),
the projection and lighting geometry is automatically retrieved from the SPICE
server.  If not, that information can be supplied in the image label (see
program PERSLAB) or via parameters.  If the OM matrix is not know, see the
section on COMPUTING THE OM MATRIX below.

.page
GEOMETRIC DISTORTIONS

If the input image is a flight image (i.e. not map projected), the program will
check if it has already been corrected for geometric camera distortions (by
scanning for the GEOMA keyword in the image label).  An uncorrected image is
is said to be in "image space" while a corrected image is said to be in "object
space" (see keywords DIST, IMAGE, OBJECT).  Note that "object space" is
identical to the perspective projection output by program MAP3.

If the image is uncorrected, a model of the geometric distortions must be
available so that it can be accounted for in the projection geometry.  For CCD
cameras (e.g. Galileo, Cassini), the geometric distortions are largely due to
the radial distortions of the optics.  For vidicon cameras (e.g. Voyager and
earlier missions) the distortions are defined by displacements in the reseau
pattern embedded on the camera's face-plate.  For each "supported" mission,
a nominal model of the geometric distortions is retrieved from built-in tables.
For CCD cameras, this model is adequate.  For vidicon cameras, more accurate
results may be obtained by locating the reseau in the image and inputing the
resulting GEOMA parameters (see parameter INP and programs RESLOC and RESLOCVO).

   resloc f1636832.fic (res,geo)
   photfunc (f1636832.fic,geo) a.img HAPKE=(.5,.2,10.,.5,.2) target=IO

A raw Voyager image containing geometric distortions is first input to program
RESLOC to locate the reseau and compute geometric correction parameters (geo).
These parameters are input to PHOTFUNC together with the image (the
image must be first) and are used to account for the geometric distortions.

   resloc f1636832.fic (res,geo)
   farenc (f1636832.fic,geo) (c,p,g) target=IO
   geoma (f1636832.fic,g) b.img
   photfunc b.img a.img HAPKE=(.5,.2,10.,.5,.2) target=IO +
     sscpt=(400.,400.)

This is similar to the previous example, except that program FARENC is used to
correct the camera pointing and output GEOMA parameters (g) which center the
target in the image and correct for geometric distortion.  GEOMA is then used to
create an object-space image.  The result is input into PHOTFUNC, which
recognizes that the image is in object-space from the GEOMA keyword in the
image label.  The SSCPT parameter is required to indicate that the target
center has been translated to the center of the image by FARENC.  Note that
if an object-space output is desired, then running GEOMA before PHOTFUNC is
more effecient since PHOTFUNC runs faster if it does not need to account for
geometric distortions.

Currently, no nominal models are available for Space Telescope or SIPS.

.page
COMPUTING THE OM MATRIX

If the OM matrix is not available, it may be computed via the (1) far-encounter
or (2) tiepoints algorithms.  Both of these methods require that the remaining
projection information be known.

The far encounter algorithm is used in images in which the target limb is
visible in the image.  VICAR programs FARENC or NAV is used to fit the
limb and obtain the line-sample coordinates of the target center.  Parameters
SSCPT, OSSCPT, or ISSCPT may be used to input these value to PHOTFUNC.  This
algorithm also requires the north angle (orientation of the target's spin axis
measured clockwise from up in the image).  See NORANGLE parameter.
The program inputs these values into the far encounter algorithm (see VICAR
subroutine MOMATI) to compute the OM matrix.

The tiepoints algorithm is used in high-resolution images in which the target
limb is not available.  The image is first paired with a reference image whose
camera pointing is known (see programs MANMATCH, NAV2).  Common features
(tiepoints) are located in both images (by determining their line-sample
coordinates) and the corresponding latitude-longitude coordinates are computed.
These (line,sample) and (lat,long) pairs are input to PHOTFUNC via the
TIEPOINTS parameter.

Note that a common use of the far encounter and tiepoints algorithms is to
improve the nominal camera pointing initially available from the SPICE server.
The programs referred to above normally store their results into SPICE C-kernels
from which they are automatically retrieved by PHOTFUNC.  The FARENC and
TIEPOINTS parameters are therefore necessary only if the SPICE server interface
is circumvented or unavailable.

.page
COMPUTING THE LIGHTING ANGLES

To compute the lighting angles at a given pixel, the programs must transform
that pixel to a surface point on the target, and use the spacecraft and solar
vectors to compute vectors that point from that surface point to the sun and
spacecraft.  In addition, the target body model (polar and equatorial radii) is
used to compute the surface normal at that point.

The incidence angle is the angle between the surface normal and the vector
pointing to the sun.

The emission angle is the angle between the surface normal and the vector
pointing to the spacecraft.

The phase angle is the angle between the vectors pointing to the sun and
spacecraft.  Note that since the geometry is three-dimensional, the phase angle
is normally not equal to the sum of the incidence and emission angles.

.page
INTERPOLATION ALGORITHM:  The photometric function is computed exactly for a
grid of points whose spacing is specified by parameters LINC, SINC, or INCR.
For intermediate points within the grid, the function is computed using
bilinear interpolation.  In general, a smaller grid spacing reduces the error
introduced by the interpolation, but increases the execution time.

If one or more of the four grid points surrounding a given pixel is off the
target body, the photometric function will be computed exactly for every pixel
within the box within the four grid points.  This criteria can be extended to
apply to points within LIMB degrees or the limb or TERM degrees of the
terminator (see LIMB and TERM parameters).  This strategy for avoiding large
interpolation errors near the limb or terminator may not be the best, as shall
be clear below.

The keyword 'NOINTERP causes the photometric function to be computed exactly
at every pixel in the image.  In this mode, the grid (LINC, SINC) is used to
skip efficiently over dark sky regions of the image.

It is instructive to compute the error introduced by the interpolation
algorithm by photometrically correcting a full-disk image with and without
interpolation:

	photfunc f1636832.geo a.img MINN=0.7
	photfunc f1636832.geo b.img MINN=0.7 'NOIN
        f2 (a.img,b.img) c.img func="100*in1/in2"
        fit c.img 'EHIST

The error introduced near the limb of the target is significant.  For high
resolution images (no visible limb) the error introduced by interpolation is
negligible.

Keep in mind that the interpolation algorithm was developed in the '70s when
computers were 1000 times slower and 1000 times more expensive.  In an age of
more powerful computers, the interpolation algorithm seems less relevant.  The
following timing results were obtained on a Sun Ultra 5 workstation:

Timing data using interpolation with a 10x10 grid spacing (the default):
					CPU sec		Wall clock sec
Galileo 800x800 Image Space image:	 1.63		 3.52
Cassini 1024x1024 Image Space image:	 1.84		 4.77
Voyager 800x800 Image Space image:	 3.96		 5.35
Voyager 1000x1000 Object Space image:    1.57		 3.44
500x500 Rectilinear projection:		 1.49		 2.70

Corresponding timing data using the 'NOIN keyword:
					CPU sec		Wall clock sec
Galileo 800x800 Image Space image:	 8.50            9.80
Cassini 1024x1024 Image Space image:	15.33		17.00
Voyager 800x800 Image Space image:	25.22		26.63
Voyager 1000x1000 Object Space image:    4.84		 6.09
500x500 Rectilinear projection:		13.44		15.04

Also, some of the photometric functions are computationally expensive if
calculated for every pixel.  The following are timing results using the 'NOIN
keyword on a 1024x1024 Cassini image space image:
						CPU sec		Wall clock sec
Minnaert:					15.33		17.00
BURATTI:					17.13		19.23
VEVERKA:					15.44		17.20
Old HAPKE function:				15.87		17.44
HAPKE function with COOK modification:		15.86		17.39
HAPKE with one term Henyey-Greenstein:		25.72		27.18
HAPKE with two term Legendre polynomial:	24.93		27.20
IRVINE:						16.99		18.95

.page
CLASSIFICATION MAP OPTION:

It is possible to have PHOTFUNC apply its photometric correction only
to certain points in the image, by supplying a classification map (such
as can be generated by program FASTCLAS) as a supplementary input file, 
and specifying a class by the CLASS parameter.  Then only pixels which 
have the specified DN value in the classification map are corrected.  
This can be useful if different types of terrain in an image are to have 
different photometric parameters, as would be the case for the more 
physically realistic functions, such as those of Hapke.  

The following example illustrates the use of the classification map option.
Note that the example fails because of this programmer's ignorance in
using STATS and FASTCLAS.  However, the method should work in principal.

fit io.org a.img (1,1,450,450) 'byte
fit io.blu b.img (1,1,450,450) 'byte
fit io.vio c.img (1,1,450,450) 'byte

stats (a.img,b.img,c.img) stats.img exclude=0 +
  class1=(188,81,20,20) class2=(369,80,20,20)
fastclas (a.img,b.img,c.img,stats.img) class.img sigma=(20.,10.)

copy io.vio c.img (1,1,450,450)
photfunc (c.img,class.img) out.img IRVINE=(1.14,.118,.0019) CLASS=2 +
   'CYLI SCALE=8. NORTH=0. +
   LATI=80. LONG=230. LINE=225. SAMP=1.
   SOLAR=(0.541,171.276) SPACE=(-.032,156.474,806030.) +

A 450x450 area of a color triplet is extracted from an IO global mosaic and
convert to byte data (program STATS will only accept byte inputs).  Programs
STATS and FASTCLAS are run to create a classification map (class.img) which is
then input to PHOTFUNC.  Only pixels from Class 2 of the map will be corrected.
All other pixels are output as 0 DN.  Note that the image input to PHOTFUNC
must represent the same image area as that of the classification map.  This is
accomplished by copying the area from the mosaic BEFORE inputing into PHOTFUNC.

This example also illustrates how to enter the projection and lighting geometry
for map-projected images if these are not included in the image label.

As the program is currently structured, a separate run must be made for 
each class, with the output of one run forming the input for the next, 
until all classes have been processed.  (The option to do all classes
except for a specified one is also supported.)  It should be possible 
for a future modification to process all classes in one run.

.page
EXAMPLES:

!--------------------------Galileo image-------------------------------

  photfunc inp=18494401.1 out=a.img MINN=0.7

A Minnaert function is applied to a Galileo image of Callisto.  PHOTFUNC will
retrieve the mission, camera id, Spacecraft-Event-Time, and target (Callisto)
from the flight label. These image identifiers are used to automatically
retrieve the image and lighting geometry via the SPICE server.  If everything
goes well, running PHOTFUNC can be as easy as pi.

!--------------------------Cassini image-------------------------------

photfunc n1354897340.1 a.img BURATTI=(0.5,.6,-.003,.14,.14,1.0) +
   SSCPT=(512.,512.)

Assume that the input image has been previously processed by program FARENC to
center the target body in the image.  The SSCPT parameter specifies that the
center of the target is at line-sample coordinates (512,512).  This will result
in a re-computation of the OM matrix (see description of far encounter algorithm
above).

!--------------------------VGR image space image-------------------------------

photfunc f1636832.fic a.img HAPKE=(.5,.2,10.,.5,.2) target=IO

A raw Voyager image containing geometric distortions is input to PHOTFUNC.
Since no geometric distortion parameters are provided, nominal values are
retrieved from built-in tables and used to account for the geometric distortions
in the projection geometry.  Note that the TARGET parameter is required since
the target body is not identified in Voyager flight labels.  See the section
on GEOMETRIC DISTORTIONS above.

!------------------What to do when SPICE data is not available-----------------

photfunc f1636832.fic a.img BURATTI=(0.5,.6,-.003,.14,.14,1.0) 'NOSPICE +
    TARGET=IO SOLAR=(0.541,171.276) SPACE=(-.032,156.474,806030.) +
    SSCPT=(539.67,601.21) NORANGLE=15.7

Assume that the input image has a Voyager flight label.  The 'NOSPICE keyword
prevents the program from attempting to retrieve data from a non-existent
SPICE server.  Based on the mission (VGR-1) and camera identifiers in the label,
the camera constants and nominal geometric distortion parameters are retrieved
from built-in tables.  Based on the TARGET parameter, the polar and equatorial
radii are retrieved as well (careful, these may not be the latest or greatest).

Assuming that the image label contains no further useful information, the
solar and spacecraft vectors must be input via parameters.  Finally, the
subspacecraft line-sample coordinates (a.k.a target center) and north angle are
input to enable the program to compute the camera pointing (see description of
far encounter algorithm above).

photfunc f1636832.geo a.img VEVERKA=(0.5,-0.01,0.5,0.01) 'NOSPICE
    RADII=(1824.3,1824.3,1815.7) +
    SOLAR=(0.541,171.276) SPACE=(-.032,156.474,806030.) +
    TIEPOINT=(381.86,382.64,19.35,229.21, +
        382.17,498.94,25.83,190.00, +
        381.98,615.31,32.86,163.58, +
        498.40,498.49,2.539,179.32, +
        498.53,615.11, 8.65,156.13)

The above example demonstrates the use of the tiepoints mode to specify the
camera pointing for a high-resolution Voyager image (see description of
tiepoints algorithm above).

!------------------------Unsupported missions---------------------------------

  photfunc inp=a.img out=b.img VEVERKA=(0.5,-0.01,0.5,0.01) 'OBJECT +
    TARGET=IO FOCL=1500.19 LAXIS=500. SAXIS=500. PSCALE=84.8214 +
    SOLAR=(0.541,171.276) SPACE=(-.032,156.474,806030.) +
    SSCPT=(539.67,601.21) NORANGLE=15.7

If the image is taken from a spacecraft which is not recognized by PHOTFUNC,
all projection and lighting geometry information must be supplied via
parameters.  Here the target constants are specified by identifying the target
body, and the OM matrix is computed from the target center and north angle
(SSCPT and NORANGLE).

!--------------------------Map projected image---------------------------------

map3 &"path1"f1636832.geo b.img NL=500 NS=500 +
   'RECT TARGET=IO SCALE=10. +
   LINE=1. SAMP=1. LATI=80. LONG=230.
photfunc b.img a.img IRVINE=(1.14,.118,.0019) TARGET=IO

A Voyager image of IO is mapped to a rectilinear projection via program MAP3.
The resulting projection is input to PHOTFUNC.  Although the projection
information can be retrieved from the "map label" output by MAP3, this label
does not currently contain the lighting geometry.  PHOTFUNC will attempt to
retrieve that information from SPICE.  Since both MAP3 and PHOTFUNC will
access the SPICE server (in this example), and since the Voyager label does
not include the target name, this must be provided via the TARGET parameter.

.page
RESTRICTIONS:

1. The GRID option is currently disabled.

HISTORY

Written By: Joel Mosher			1 AUGUST 1978
Cognizant Programmer: L.W.Kamp
Revision: 1				9 FEBRUARY 1981
Revision: 2     CONVERT TO VICAR2      16 MAY 1985
Revision: 3     Viking additions       11 MAY 1986
Rev. 4:     Mods for new PHOTFIT.  2 APR 1987  -- LWK.
Rev. 5:     Added classificaion map option,  19 Aug. 1987  -- LWK.
Rev. 6:     Use normalized phot. func., 2 Sept. 1987 -- LWK
Rev. 7:     Add 6-parameter Hapke fcn., bug fixes, 8 Oct. 1987 -- lwk
Rev 8       Many bug fixes. Put in exact photometric function at
            every point for limb & terminator.
23 Aug 89  ..GMY..  Delete fiddling FDS for WA frames of simultaneous exposure
15 Aug 93   jjl ability to read map3 perspective label type.
06 Sep 02  GMY: Major revisions to help and test files.
	   Major revisions to projection and lighting geometry determination
	   Add NAIF keyword and delete obsolete SEDR keyword (AR 107288)
           Updated to read IBIS format geometric distortion files (AR 107368)
           Changed NOSEDR keyword to NOSPICE.
	   Deleted GEOMA, NAH, NAV, and START parameters.
	   Deleted LUMS and THRESH parameters.
           Logic controlling line loop and interpolation redesigned.
           Fix bug in farenc algorithm.  Fix to work on floating point images.
	   Fix computation of surface normal vector.
07 Oct 02  Replaced oblate spheroid model with triaxial ellipsoid.  All calls
           to convev have been replaced by calls to mp routines.
           Added 'NOIN parameter.
10 Jan 13 -lwk- fixed CHARACTER continuation lines for new compiler flag on Solaris

.LEVEL1
.VARI INP
1-4 input files: image,
[GEOMA], [GRID], [CMAP].
.VARI OUT
Output file (optional)
.VARI SIZE
Standard VICAR size field.
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI RADII
Three radii of target
(RA,RB,RC)
.VARI LINC
Line spacing of tiepoints.
.VARI SINC
Sample spacings of tiepoints.
.VARI INCR
Line and sample spacings 
of tiepoints.
.VARI NOINTERP
Keyword: Perform no
interpolation
.VARI MINNAERT
The Minnaert constant K
.VARI MOSHER
Mosher function constants 
.VARI VEVERKA
Veverka function constants
.VARI HAPKE
Constants for Hapke functions
(4 values= OLD, 5/6 values= NEW)
.VARI COOK
Cook modification to OLD HAPKE
.VARI BURATTI
(6 values)
Buratti function.
.VARI IRVINE
Irvine constants K,A,B
.VARI PROJECT
Specifies projection.
VALID: LAMBERT,MERCATOR,
ORTHOGRA,STEREOGR,POLE,
CYLINDRI,RECTANGU,LATLON
.VARI PAR1
Northern most parallel 
of Lambert conformal.
.VARI PAR2
Southern most parallel 
of Lambert conformal
.VARI NORTH
Angle measured clockwise from 
the center of projection.
.VARI SCALE
Projection scale in km/pixel
.VARI LINE
Projection special line
.VARI SAMPLE
Projection special sample
.VARI LATITUDE
Projection special latitude
.VARI LONGITUD
Projection special longitude
.VARI FARENC
Far encounter mode.
.VARI SSCPT
Object space line and sample.
.VARI OSSCPT
Object space line and sample.
.VARI ISSCPT
Image space line and sample.
.VARI NORANGLE
North angle in degrees.
.VARI TIEPOINT
Specifies tiepoint mode.
.VARI TARGET
String specifying the name
of the target body.
.VARI NOSPICE
Disables access to the
SPICE server.
.VARI SOLAR
Subsolar latitude and longitude
in degrees.
.VARI SPACE
Subspacecraft lat., long. and
distance to center of planet.
.VARI SLATITUD
Subspacecraft latitude in degrees
.VARI SLONGITUD
Subspacecraft longitude in degrees
.VARI RMAGNITU
Distance from spacecraft to
planet center in km.
.VARI OMMATRIX
Relationship between planet
coord. system and camera
coord. system.
.VARI RSVECTOR
Planet to spacecraft vector
.VARI OBJECT
Indicates camera distortions
removed.
.VARI IMAGE
System distortion not
geometrically correct.
.VARI DISTOR
System distortion not
geometrically correct.
.VARI FOCAL
Camera's focal length in mm.
.VARI FOCL
Camera's focal length in mm.
.VARI LAXIS
Value of line in object space.
.VARI SAXIS
Value of sample in object space.
.VARI PSCALE
Scale in object 
space focal plane.
.VARI MISSION
Mission.
VALID=(VGR-1,VGR-2,GLL,CASSI,WFPC1,
WFPC2,SIPS,QUEST,VIKOR,MAR-9,MAR10
.VARI SCET
Optional SpaceCraft Event Time
.VARI CAMERA
Camera serial number.
.VARI NOPROJEC
Specifies picture not taken by
any of the mentioned projects.
.VARI PRINT
Information about picture 
is to be printed.
.VARI ALL
Data is to be printed out for
points beyond the terminator.
.VARI NOCORREC
No photometric function
correction is done.
.VARI MAXDN
Maximum data number the
program can output.
.VARI TERMINAT
Restricts interpolation
to TERMINAT degrees 
from terminator.
.VARI LIMB
Restricts interpolation
to LIMB degrees 
from planet limb.
.VARI MULTIPLY
Output DN to be multiplied
by a constant.
.vari CLASS
Class number in classification 
map
.vari MAXCOR
Optional keyword
Maximum permitted intensity
correction.
.vari TARGET
STRING - Target body name
.vari SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.vari CKNAME
Optional 4-char string
C-kernel name
.vari CKID
Optional 4-char string
C-kernel ID
.vari USERID
Optional 3-char string
User who created camera pointing
.vari GROUPID
Optional 3-char string
Group which created camera pointing
.vari INSTITUTE
Optional 4-char string
Facility which created camera pointing
.vari PURPOSE
Optional 4-char string
Purpose for camera pointing
.vari PROGRAM
Optional 6-char string
Program which created camera pointing
.vari SPKID
Optional 4-char string
SP kernel for created camera pointing
.vari REQNUM
Optional 4-char string
IPL request number for created camera pointing
.vari CDATE
Optional 12-char string
Date and time camera pointing was created
.LEVEL2
.VARI INP
 Specifies 1 to 4 input filenames: (IMAGE,GEO,GRID,CMAP)

 IMAGE is the input VICAR image upon which brightness correction is performed.
 IMAGE is required and must always be the first in this list.  The remaining
 inputs (GEO, GRID, and CMAP) are optional and may appear anywhere in the INP
 list after IMAGE.

 IMAGE may be an image from a "supported" or "unsupported" mission.  The
 degree of support varies with the mission and determines the amount of
 information that can be automatically retrieved or which must be supplied by
 the user.  See the MISSION parameter.  IMAGE may be in BYTE, HALF, FULL, or
 REAL data formats.  There are no size restrictions.

 GEO is a geometric correction parameter file as created by programs RESLOC
 and RESLOCVO.  GEO is in IBIS graphics format and is identified solely by its
 record size (512 bytes).

 GRID specifies a file of photometric function parameters in the format
 generated by PHOTFIT.  The program recognizes it by the word "PHOTFIT"
 in the label, and a record size of 120 samples.
 NOTE: Since PHOTFIT is obsolete, this option is currently disabled.

 CMAP specifies a classification map as produced by program FASTCLAS.
 CMAP is used to limit the brightness correction to pixels of a specified
 class (See CLASS parameter).  The program recognizes the file by the words
 "CLASSIFICATION MAP" in the label.  CMAP must be of BYTE format
 and of the same size as the input image.

.VARI OUT
OUT=B where B is the output (brightness corrected) image.  B will be in the
same data format as the input image.  If no output file is specified, then
brightness correction is performed (See parameter NOCORREC.)
.VARI SIZE
SIZE=(sl,ss,nl,ns) is the standard VICAR size field and specifies the image
area of the input image to be operated on.  SIZE specifies the starting
line, starting sample, number of lines, and number of samples of the image
area, so that the area will begin at (sl,ss) of the input image, and the output
image will have dimensions nl x ns.
.VARI CLASS
 CLASS is only used when a classification map has been included among the
 input data (see INP parameter).  It specifies a DN value that will be used to
 select a set of pixels from the input image, and only those pixels will be
 processed by PHOTFUNC.

 E.g., if CLASS=8 is specified, then a pixel in the input image will only
 be corrected if the corresponding pixel in the classification map has a DN
 value of 8.

 If the value specified by CLASS is negative, then the absolute value
 of CLASS is taken to denote a class that is NOT to be processed;  all
 classes will be processed except for that one and the DN=0 (unclassifiable)
 class.
.VARI SOLAR
SOLAR=(R1,R2) where R1 and R2 are floating point numbers specifying the
subsolar latitude and longitude in degrees.
.VARI SPACE
SPACE=(R3,R3,R5) where R3, R4, and R5 are floating point numbers specifying
the subspacecraft latitude and longitude in degrees and the distance to the
center of the target in kilometers respectively.
.VARI SLATITUD
SLAT=R4 where R4 is a floating point number specifying the subspacecraft
latitude in degrees.
.VARI SLONGITUD
SLON=R5 where R5 is a floating point number specifying the subspacecraft
longitude in degrees.
.VARI RMAGNITU
RMAG=R6 where R6 is a floating point number specifying the distance from
subspacecraft to planet center in kilometers.
.VARI RADII
RADII=(RA,RB,RC)
The three radii of the target, in kilometers, where
  RA = long equatorial radius
  RB = short equatorial radius
  RC = polar radius
The default are values retrieved from SPICE.  If SPICE is not available,
the data stored in subroutine PBDATA are used.
.VARI TARGET
TARGET=string
Specifies the name of the target body.  The target body name is one of four
identifiers needed to retrieve the projection and lighting geometry from the
SPICE server.  If no SPICE data is available, the target name is used to
retrieve the target radii via a call to PBDATA.
.VARI OMMATRIX
OMMATRIX=(R1,R2,..,R10) where Rn is a floating point number specifying the
nine elements of the OM matrix used to define the relationship between planet
coordinate system and camera coordiante system. The values are in row major
order.

The OM matrix is normally retrieved automatically by accessing the SPICE
server (when available).
.VARI RSVECTOR
RSVECTOR=(R1,R2,R3) where Rn is a floating point number and R1, R2, R3 are the
three values of the RS vector expressed in the planet coordinate system. The
vector can be alternatively specified in polar coordinates via parameter
SPACE (or SLAT, SLON and RMAG).

The RS vector is normally retrieved automatically by accessing the SPICE
server (when available).
.VARI OBJECT
OBJECT specifies that the input frame is geometrically correct; that is,
all camera systems distortions have been removed. This is the default.
.VARI IMAGE
IMAGE specifies that the input frame is not geometrically correct; that is,
camera system distortions have not been removed. The default is the picture
has been geometrically corrected. If IMAGE is specified, the GEOMA parameters
must be supplied either through the specifiction of a documented flight
project or the user can supply an alternate input file.
.VARI DISTOR
DISTOR specifies that the input frame is not geometrically correct; that
is, camera system distortions have not been removed. The default is the
picture has been geometrically corrected.
.VARI FOCAL
FOCAL=R1 where R1 is a floating point number and specifies the value 
of the camera's focal length in millimeters. The default is 1500 mm.
.VARI FOCL
FOCL=R1 where R1 is a floating point number and specifies the value 
of the camera's focal length in millimeters. The default is 1500 mm.
.VARI LAXIS
LAXIS=R1 where R1 is a floating point number and specifies the value of the
line of the object space optical axis. The default is R1=500.
.VARI SAXIS
SAXIS=R1 where R1 is a floating point number and specifies the value of the
sample of the object space optical axis. The default is 500.
.VARI PSCALE
PSCALE=R2 where R2 is a floating point number and specifies the scale in the
object space focal plane in pixels per millimetrer. The default is 84.821428.
.VARI FARENC
FARENC specifies that the far encounter mode is to be used to determine the
OM matrix. Be sure to specify SSCPT NORA, and the RS vector(or its equivalent).
.VARI SSCPT
SSCPT=(R1,R2) where R1 and R2 are floating point numbers which specify the
object space line and sample respectively or the subspacecraft point in the
input image. The defaults are R1=R2=0.
.VARI OSSCPT
OSSCP=(R1,R2) where R1 and R2 are floating point numbers which specify the
object space line and sample respectively or the subspacecraft point in the
input image. The defaults are R1=R2=0.
.VARI ISSCPT
ISSCP=(R1,R2) where R1 and R2 are floating point numbers which specify the
image space line and sample respectively at the subspacecraft point in the
input image. The default are R1=R2=0.
.VARI NORANGLE
NORA=R1 where R1 is a floating point number specifying the angle of north in
degrees. It is measured in the image plane at the subspacecraft point
clockwise from up. The default is 0.
.VARI TIEPOINT
TIEPOINT=(R1,R2,R3,R4,...,RN) where Rn are floating point numbers and are
lines, samples, latitudes and longitudes of the tiepoints. The numbers
following TIEPOINT are in groups of four; each group specifying the line,
sample, latitude, and west longitude in that order of each tiepoint. At
least three points are needed to determine the OM matrix. No more than 25
points can be used and if more than 25 points are input only the first 25
will be used. If the TIEPOINTS mode is used be sure to specify the RS vector
or its equivalent.
.VARI PROJECT
The following keywords are valid for this parameter:

LAMBERT: specifies that the picture is in a Two-Standard Lambert Conformal
Conic projection.

MERCATOR: specifies that the picture is a Mercator projection.

ORTHOGRA: specifies that the picture is an orthographic projeciton.

STEROGR: specifies that the picture is a stereographic projection.

POLE: specifies that the picture is a polar projection. If the user
specifies POLE, he must also specify either ORTH or STER.

CYLI: specifies that the picture is the normal cylindrical projection.

RECT: specifies that the picture is in the simple cylindrical projection.

LATL: specifies that the picture is in the simple cylindrical projeciton.
.VARI LINE
LINE=R1 where R1 is a floating point number specifying some arbitrary line
in the picture which is some specified latitude and/or longitude. The default
is R1=0.
.VARI SAMPLE
SAMPLE =R1 where R1 is a floating point number specifying some arbitrary sample
in the picture which is some specified latitude and/or longitude. The default
is R1=0.
.VARI LATITUDE
LATITUDE=R1 where R1 is a floating point number specifying an arbitrary
latitude in the picture which is some specified latitude and/or longitude.
.VARI LONGITUD
LONG=R1 where R1 is floating number specifying an arbitrary longitude in the
picture which is some line and/or sample.
.VARI PAR1
PAR1=R1 where R1 is a floating point number specifying the latitude of the
standard parallels if the projection is Lambert conformal. PAR1 is the northern
most parallel.
.VARI PAR2
PAR2=R1 where R1 is a floating point number specifying the latitude of the
standard parallels if the projection is Lambert conformal. PAR2 is the southern
most parallel.
.VARI NORTH
NORTH=R2 where R2 is a floating point number specifying the angle in degrees
of north in the output picture. This angle is measured in the projection plane
at the center of projection clockwise from up.
.VARI SCALE
SCALE=R3 where R3 is a floating point number specifying the number of
kilometers per pixel at the center of projection for stereographic and
orthographic projections, at the standard parallels for Lambert Conformal
and at the equator for simple cylindrical, normal cylindrical and 
Mercator projections.

.VARI MINNAERT
MINNAERT=R1, where R1 is a floating point number specifying the Minnaert 
function k value.  
The photometric function is:

	F = A * COS(i)**k * COS(e)**k-1

The photometric correction factor, f, used for the 
Minnaert function is:

	f = F/F0 = COS(i)**k * COS(e)**k-1

where:	F is the brightness
	F0 is normalized brightness, F0=F(i=0,e=0)
	k  is the geometrical constant
	i  is the incidence angle 
	e  is the emission angle 

Minnaert is the default function, with a default k of 0.5

.vari VEVERKA
VEVERKA=(A,B,C,D) specifies that the Squyres-Veverka phase angle function 
in conjunction the Lommel-Seeliger law is to be used, and also specifies 
its four constants.

The equation for this photometric function is:

	F = ( A + B*g + C*exp(-D*g) ) * COS(i) / (COS(i)+COS(e))

where:	g  is the phase angle in degrees
	i  is the incidence angle
	e  is the emission angle

Therefore the correction factor f performed by photfunc is given by:

        f=F/F0  where F0=F(i=0,e=0,g=0)
	f = (A + B*g + C*exp(-D*g)) * 2/(A+C) * COS(i)/(COS(i)+COS(e))

.vari HAPKE
HAPKE=(from 4 to 6 parameters)
This is the Hapke function. There are three versions of this function 
depending upon the number of parameters specified, as follows:

4-parameters W,B,H,C    This is the 1978 Hapke function.
     W = single scattering albedo
     B = first term legendre polynomial phase function.
     H = porosity
     C = second term legendre polynomial phase function.
     CI=cos(i)
     CE=cos(e)
     TP=tan(g)

      R = (W/4) * (CI/(CI+CE)) * ((1+Q(TP))*PF(TP)+H(CI)*H(CE)-1)

    where:

      H(X)=(1+2*X)/(1+2*X*SQRT(1-W))     (multiple scattering term)
      Q(TP)=EXP(-W**2/2)*(1-TP*(3-EXP(-H/TP))*(1-EXP*(-H/TP)/2H) for TP>0
           =0                                                   for TP<=0
      PF(CP)=1+B*CP+C*(3*CP**2-1)/2  (phase function of a single particle)

5-parameters W,H,CK,C,B    This is the 1984 Hapke function.
     W = single scattering albedo
     H = Backscatter parameter related to soil porosity.
     CK (TBAR) = Average macroscopic slope angle in degrees.
     C (OPMAG) = S(0) term in the opposition magnitude coefficient.
     B = Henyey-Greenstein single particle phase function.

       R = (W/4) * (C01/(C01+C11)) * SF * 
           ( (1+QF(S0,W,H,P,HG)) * PF(CP) + H(C01)*H(C11) -1)
    where:
      C01 = (CI + sinI*tanTB*X1/X2)/BETA
      C11 = (CE + sinE*tanTB*X3/X2)/BETA
      BETA = sqrt(1+pi*(tanTB)**2)
      QF(S0,W,H,P,HG) = S0*H/(W*(H-tan|P/2|)*PF(1)
    X1,X2,X3,SF are complicated functions of I, E, and TB, and the remaining
    functions and symbols are as defined above.

      PF(CP) = (1-HG**2)/(1+HG**2+2*HG*CP)**1.5,

6-parameters W,H,CK,C,B,XLG2    This is the 1984 Hapke function.
     W = single scattering albedo
     H = Backscatter parameter related to soil porosity.
     CK (TBAR) = Average macroscopic slope angle in degrees.
     C (OPMAG) = S(0) term in the opposition magnitude coefficient.
     B = Term#1 in Legendre polynomial particle phase function.
     XLG2 = Term#2 in Legendre polynomial particle phase function.

Note: these functions are also normalized as f=F/F0 
      where F0=F(i=0,e=0,g=0)

.vari MOSHER
MOSHER = (A,B,C,D,E,F) specifies that the MOSHER function is to be used 
to process the input image. 

The first 4 values that follow the keyword have the same function as
the constants for VEVERKA, the last 2 correspond to the values of the 
Minnaert "k" and a phase angle coefficient.  The MOSHER function is a 
combination of the Veverka and Minnaert functions:

              PAC=A+B*g+C*exp(-D*g)
              F = PAC * (cos(i)**(E+F*g)) * (cos(e)**(E+F*g-1))
     where
              g is the phase angle in degrees
              i is the incidence angle
              e is the emission angle

The correction applied by photfunc is:

              f=F/F0, F0=F(i=0,e=0,g=0)
              PAC=A+B*g+C*exp(-D*g)*2/(A+C)
              f = PAC * (cos(i)**(E+F*g)) * (cos(e)**(E+F*g-1))


.VARI COOK
COOK=K where K is a floating point number specifying the COOK parameter. The
Cook function is a modification to the 1978 Hapke function so W, B, H, and C
should also be specified, using parameter HAPKE. The modification to the 
Hapke function is in redefining incidence and emission angles to their
new values:
          cos(i) <-- sqrt(1-K*K*(1-cos(i)*cos(i)))
          cos(e) <-- sqrt(1-K*K*(1-cos(e)*cos(e)))

If HAPKE is not specified, or is specified with 5 values, then this parameter
is ignored.

.VARI BURATTI
Specifies the BURATTI-VEVERKA function. 
The function is of the form:
            ci                
    Q= A*-------*f(a) + (1-A)*ci
          ci+ce               

         p(a)*pi*((2/3)*(1-A)+A*F)-(2/3)*(1-A)*(sin(a)+(pi-a)*cos(a))
    f(a)=------------------------------------------------------------
         (A*pi/2)*(1-sin(a/2)*tan(a/2)*ln(cot(a/4)))

    p(a)=B+C*a+D*exp(-E*a)

    The coefficients A,B,C,D,E,F are input parameters 1-6.

    and where
          a=phase angle
          i=incidence angle
          e=emission angle
          ci=cos(i)
          ce=cos(e)


The correction applied by photfunc is:

    f=Q/q, q=Q(a=0,i=0,e=0)

    q=(B+D-1)*(2/3)*(1-A) + A*(B+D)*F +(1-A)

The coefficients look like: (.5,.6,-.003,.14,.14,1.0)
The following articles discuss the function and parameters.
See Icarus 59 392-405 Buratti
See Icarus 46 137-155 Veverka

.VARI IRVINE
Specifies the Irvine function. There are three parameters corresponding
to the constants k,a,b in the equation:

  B*Fsun         k     (1-exp(-ci/a)
F=------- * (ci*ce)  * ------------
  pi*ce                (1-exp(-ce/b)

    where
          i=incidence angle
          e=emission angle
          ci=cos(i)
          ce=cos(e)

The correction applied by photfunc is:

    f=F/F0, F0=F(i=0,e=0)

    Also the constants k,a,b are typically 0.9, 0.118, and 0.0039 respectively.
    For these values and at i=0 and e=0 the term (1-exp(-1/a))/(1-exp(-1/b))=1
    so it is omitted from the normalization F0.

The implemented function in Photfunc is therefore:

               k
        (ci*ce)   (1-exp(-ci/a)
f=F/F0= ------- * ------------
          ce      (1-exp(-ce/b)


Example:  irvine=(.9,.118,.0039)

.VARI MISSION
Keyword identifying the mission from which the image as acquired.  Valid
missions are:

CASSI: Cassini ISS
GLL: Galileo
VGR-1 and VGR-2: Voyager 1 and 2
VIKOR: Viking Orbiter (1976)
MAR10: Mariner Venus Mercury mission (a.k.a. MVM73).
MAR-9: Mariner 9 (Mars)
WFPC1 and WFPC2:  Wide Field Planetary Camera (Space Telescope) before and
  after camera upgrade.
SIPS: Silicon Imaging Photometers System, Table Mountain Observatory 24"
  telescope in the 512 lines by 512 sample mode at the Cassigrain focus.
QUEST: the SIPS used with a Questar 700 mm lens in the 512 lines by 512 sample
  mode.

For each of these missions, the camera focal length, the line and sample
coordinates of the optical axis intercept point, and the picture scale in
pixels/mm for each specific camera are automatically loaded from built-in
tables.  See the CAMERA parameter for a view of this table.

In addition, for all the JPL missions (Cassini, Galileo, Voyager, Viking
Orbiter, Mariners 9 and 10), a model of the geometric camera distortions
is also retrieved.  For missions using a CCD camera system (Cassini, Galileo),
a radial distortion model of the optics is used.  For vidicon camera systems
(Voyager, Viking Orbiter, Mariners 9 and 10), the distortions are modeled
by the reseau pattern on the face plate of the camera.  Nominal displacements
for this reseau pattern are used for each specific camera.  For more accurate
results, locate the reseau directly from the image using RESLOC or RESLOCVO.
For the other missions, the image is assumed to be free from distortions.

Finally, for Cassini, Galileo, and Voyager, SPICE data containing the
projection and lighting geometry for the image is automatically retrieved via
the MIPS SPICE server.  The 'NOSPICE keyword suppresses this feature.

.VARI SCET
Shutter centered Spacecraft Event Time of the image for which SPICE data is
to be printed.  SCET is only required if INP is not specified.
.VARI CAMERA
CAM=I1 where I1 is an integer specifying the camera serial number. The default
is to obtain I1 from the picture label.  The camera serial number (together
with the mission ID) is used to retrieve the focal length, line and sample
of the optical-axis intercept point, and the picture scale from built-in
tables.  The current values in these tables are:

		      CAMERA	 FOCAL	  LAXIS  SAXIS  PSCALE (pixels/mm)
        CASSI NAC       1       2000.00    512    512   83.333333
	CASSI WAC       2        200.736    "      "      "
        CASSI NAC 2x2  21       2000.00    256    256   41.666665
	CASSI WAC 2x2  22        200.736    "      "      "
        CASSI NAC 4x4  41       2000.00    128    128   20.833333
	CASSI WAC 4x4  42        200.736    "      "      "

        GLL             1       1501.039   400    400   65.6167979 
        GLL 2x2 sum     2	1501.039   200    200   32.8083990

	VGR-2 WA        4	 200.770   500    500   84.821428
	VGR-2 NA        5	1503.49     "      "      "
	VGR-1 WA        6	 200.465    "      "      "
	VGR-1 NA        7	1500.19     "      "      "

        VIKOR 1A        7	474.610    575    625   85.0
        VIKOR 1B        4	474.398     "      "      "
        VIKOR 2A        8	474.101     "      "      "
        VIKOR 2B        6	474.448     "      "      "

        MAR10 A         1      1495.66     400    475   74.78
        MAR10 B         1      1503.69     400    475   74.78

        MAR-9           1	 52.267    400    475   75.0     
        MAR-9		2	500.636     "      "      "

        WFPC1		1       67991.     400    400   66.66667
	WFPC1		2       31168.      "      "      "
        WFPC2		1       67991.      "      "      "
	WFPC2		2       31168.      "      "      "

	SIPS		1	9753.6     256    256   51.2
	QUESTAR		1        700.      256    256   512.

Note: These tables are obtained via a call to VICAR subroutine GETCAMCON.  For
active missions, these values may be updated as they are more accurately
determined.

See also MISSION parameter.

.VARI NOPROJ
NOPROJ: specifies that the picture was not taken by one of the mentioned
projects.
.VARI LINC
LINC=I1 where I1 is an integer specifying the line spacings of the
tiepoints at which the photometric function is computed exactly. The default
is I1=10.
.VARI SINC
SINC=I1 where I1 is an integer specifying the sample spacings of the tiepoints
at which the photometric function is computed exactly. The default is I1 = 10.
.VARI INC
INC=I1 where I1 is an integer specifying the line spacings of the tiepoints at
which the photometric function is computed exactly. The default is I1=10.
.VARI INCR
INCR=I1 where I1 is an integer specitying the sample spacings of the tiepoints
at which the photometric function is computed exactly. The default is I1=10.
.VARI NOINTERP
Keyword: Perform no interpolation.  Instead, compute photometric function
exactly for each pixel on the target.
.VARI PRINT
 PRINT specifies that the program print out the following information
 about the input picture, at the tiepoints, line, sample, latitude, longitude,
 incidence angle, emission angle, phase angle, computed brightness correction,
 from the photometric function, and object space line and sample. These angles
 are in degrees. Only the points that are actually on the visible side of the
 planet and illuminated are printed out. 

 The default is to not print this.
.VARI ALL
ALL specifies that the data from PRINT be printed out for points
beyond the terminator, i.e. points not illuminated.
.VARI NOCORREC
NOCORREC specifies that the program not perform the photometric function
correction. The default is to perform it, unless no output file is specified.
.VARI MAXDN
MAXD=I3 where I3 is an integer specifying the maximum data number the program
can output. The default is 255. If the data is HALF or REAL, I3=32767.
.VARI NOSPICE
NOSPICE disables access the the MIPS SPICE server.  All geometry
information must be provided via the parameter list.
.VARI TERM
TERM=R1 where R1 is a floating point number specifying the closest
distance in degrees that pixels computed by interpolation may
approach the terminator. Points closer than this are computed exactly.
Default is R1=0.
.VARI LIMB
LIMB=R2 Where R2 is a floating point number specifying the closest
distance in degrees that pixels computed by interpolation may
approach the limb. Points closer than this are computed exactly.
Default is R2=0.
the definition of the limb. See section on OPERATION. The default is R2=0.
.VARI MULTIPLY
MULT=R3 where R3 is a floating point number specifying that the output DN be
multiplied by a constant, R3. This is accomplished by dividing the calculated
brightness correction at a tiepoint by this constant. The default is R3=1.
.VARI MAXCOR
Optional keyword.
This is the maximum permitted intensity boost to correct for the limb
and terminator darkening caused by the photometric function. If the
determined correction is greater than this the DN will remain unchanged.
Notice the default is 5.
.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used:

  CKNAME        C KERNEL
  --------      -------------
  DAVI          MIPS_DAVI.CK
  NAV           MIPS_NAV.CK
  FARE          MIPS_FARENC.CK
  NAV2          MIPS_NAV2.CK
  NEAR          MIPS_NEAR.CK
  AMOS          MIPS_AMOS.CK
  NAIF          the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel (see CKNAME
parameter):

  CKID    CKNAME        C KERNEL
  ----    --------      -------------
  M906    DAVI          MIPS_DAVI.CK
  M905    NAV           MIPS_NAV.CK
  M904    FARE          MIPS_FARENC.CK
  M903    NAV2          MIPS_NAV2.CK
  M902    NEAR          MIPS_NEAR.CK
  M901    AMOS          MIPS_AMOS.CK
  varies  NAIF          there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

.VARI USERID
USERID is a three character string which identifies the user who created the
camera pointing.

Ex:  USERID=HBM identifies Helen Mortensen as the creator of the camera
     pointing.

.VARI GROUPID
GROUPID is a three character string which identifies the group which created the
camera pointing.

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which created
the camera pointing.

Ex:  INSTITUTE=MIPS specifies that MIPS created the camera pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI PROGRAM
PROGRAM is the first six characters of the program creating the camera pointing.

Ex:  PROGRAM=FARENC specifies that FARENC created the camera pointing.

.VARI SPKID
SPKID specifies the four character ID which uniquely identifies the
SP kernel used to create the camera pointing.  The SP-kernel IDs are located
in the ASCII file assigned the logical name (or environmental variable)
KERNELDB.

Ex:  SPKID=N015 specifies the SP kernel GLL_LONG_2.BSP

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.

Ex:  REQNUM=3456 identifies (somewhat) request number R123456

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create photfunc.imake
/* Imake file for PHOTFUNC */

#define PROGRAM photfunc
#define R2LIB

#define FTNINC_LIST mp_for_defs

#define MODULE_LIST photfunc.f image_geometry.f map_label.f get_mission.f \
		os_parms.f farenc_mode.f phot_parms.f grid_labels.f \
		photb.f compute_segment.f setup.f phot_sub.f toplanet.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_SPICE
#define LIB_TAE
#define LIB_MATH77
#define LIB_RTL
#define LIB_VRDI
#define LIB_P2SUB
#define LIB_NETWORK
$ Return
$!#############################################################################
$Test_File:
$ create tstphotfunc.pdf
!Test of program PHOTFUNC
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
local path1 type=string init="wms_test_work:[testdata.mipl.vgr]"
local path2 type=string init="wms_test_work:[testdata.mipl.gll]"
local path3 type=string init="wms_test_work:[testdata.cassini.cas$i$ss]"

refgbl $syschar
if ($syschar(1) = "UNIX")
  let path1="/project/test_work/testdata/mipl/vgr/"
  let path2="/project/test_work/testdata/mipl/gll/"
  let path3="/project/test_work/testdata/cassini/casIss/"
end-if

!Note that each test block (separated by ---) can be run independently.
!Note that some of the tests have been commented out to reduce the overall
!length by skipping features which are unlikely to change over time.
!
! The first three tests make use of SPICE data.
! For Galileo, Cassini, and Voyager, using SPICE is the default.

!--------------------------Galileo image-------------------------------
! MINNAERT function
photfunc &"path2"venus.img a.img ckname=fare MINN=0.7 MAXCOR=5.
list a.img ss=100 ns=600 linc=60 sinc=60

!--------------------------Cassini image-------------------------------
! BURATTI function
! Supply parameters to retrieve SPICE data
photfunc &"path3"n1354897340.1 a.img BURATTI=(0.5,.6,-.003,.14,.14,1.0) +
   MISSION=CASSI CAMERA=1 SCET=(2000,342,16,10,56,292) TARGET=JUPITER
list a.img ss=340 linc=50 sinc=50

!--------------------------VGR image space image-------------------------------
! New HAPKE function with one term Henyey-Greenstein function
! Input geometric distortion file
photfunc (&"path1"f1636832.fic,&"path1"f1636832.gpribis) a.img +
    HAPKE=(.5,.2,10.,.5,.2) target=IO
list a.img ss=200 linc=60 sinc=60

! Size field test.  Result should be identical to above except for image offsets
!photfunc (&"path1"f1636832.fic,&"path1"f1636832.gpribis) a.img +
!  (61,200,740,600) HAPKE=(.5,.2,10.,.5,.2) target=IO
!list a.img linc=60 sinc=60

! The remaining tests demonstrate the capability to process an image without
! accessing SPICE ('NOSPICE).  This means that all image geometry information
! must be supplied via parameters.

! New HAPKE function with two term Legendre polynomial fcn.
! Specify camera pointing via OM matrix, use nominal distortions
photfunc &"path1"f1636832.fic a.img HAPKE=(.5,.2,10.,.5,.2,0.0) +
    'NOSPICE MISSION=VGR-1 CAMERA=7 TARGET=IO +
    SOLAR=(0.541,171.276) RSVECTOR=(-739030.7,-321741.5,-456.1) +
    OMMATRIX=(0.225294680,.330341900,.91657871, +
              -.51654703,-.757162930,.39985430, +
              0.82608805,-.563541060,.00005235)
list a.img ss=200 linc=60 sinc=60

!--------------------------VGR object space image-------------------------------
!5: Old HAPKE function
! Specify camera pointing via object-space tiepoints
photfunc &"path1"f1636832.geo a.img HAPKE=(0.8,0.1,0.4,0.0) +
    'NOSPICE MISSION=VGR-1 CAMERA=7 RADII=(1829.4,1819.3,1815.7) +
    SOLAR=(0.541,171.276) SPACE=(-.032,156.474,806030.) +
    TIEPOINT=(381.86,382.64,19.35,229.21, +
        382.17,498.94,25.83,190.00, +
        381.98,615.31,32.86,163.58, +
        498.40,498.49,2.539,179.32, +
        498.53,615.11, 8.65,156.13)
list a.img ss=300 ns=600 linc=60 sinc=60

! Same old function
! Specify camera pointing via image-space subspacecraft point and north angle
!photfunc &"path1"f1636832.geo b.img HAPKE=(0.8,0.1,0.4,0.0) +
!    'NOSPICE MISSION=VGR-1 CAMERA=7 RADII=(1829.4,1819.3,1815.7) +
!    SOLAR=(0.541,171.276) SPACE=(-.032,156.474,806030.) +
!    SSCPT=(539.67,601.21) NORANGLE=15.7
!
!f2 (a.img,b.img) diff.img func="in1-in2"
!hist diff.img		!differences should be small

!-------------------------------Other missions---------------------------------
! Create psuedo data for other missions by stripping the flight label from a
! Voyager object-space image.
gen a.img 1000 1000				!Gen an image with no labels
labswtch (a.img,&"path1"f1636832.geo) b.img	!Remove flight label
label-list b.img				!Check it out

! The program knows that none of these missions have SPICE data, so it does'nt
! even try to access SPICE.  Built-in tables are used to supply the camera
! parameters.  All other information must be supplied via parameters.

!6: HAPKE function with COOK modification
photfunc b.img a.img COOK=0.5 HAPKE=(0.9,-0.1,0.3,.005) 'OBJECT 'NOIN +
    'MAR10 CAMERA=1 RADII=(1829.4,1819.3,1815.7) +
    SOLAR=(0.541,171.276) SLATI=-.032 SLONG=156.474 RMAG=806030. +
    SSCPT=(539.67,601.21) NORANGLE=15.7
list a.img ss=300 ns=600 linc=60 sinc=60

! Try also WFPC1, WFPC2, SIPS, QUEST, VIKOR, MAR-9

!7: VEVERKA function
! The mission is not specified.  Therefore must supply camera parameters.
photfunc b.img a.img VEVERKA=(0.5,-0.01,0.5,0.01) 'OBJECT +
    TARGET=IO FOCL=1500.19 LAXIS=500. SAXIS=500. PSCALE=84.8214 +
    SOLAR=(0.541,171.276) SLATI=-.032 SLONG=156.474 RMAG=806030. +
    SSCPT=(539.67,601.21) NORANGLE=15.7
list a.img ss=300 ns=600 linc=60 sinc=60

!-----------------------------PERSLAB label-----------------------------------
! IRVINE function
perslab &"path1"f1636832.geo b.img target=io	!add perslab label
photfunc b.img a.img IRVINE=(1.14,.118,.0019)	!note that SPICE is not accessed
list a.img ss=300 ns=600 linc=60 sinc=60

!--------------------------Map projected image---------------------------------
! Default Lambert function.  Note that photfunc must access SPICE to determine
! the illumination geometry.  For VGR SPICE, the target must be specified.
map3 &"path1"f1636832.geo b.img NL=500 NS=500 +
   'RECT TARGET=IO SCALE=10. +
   LINE=1. SAMP=1. LATI=80. LONG=230.
photfunc b.img a.img target=io
list a.img linc=50 sinc=50

-------------------------------Data format test---------------------------------
! Start with a byte image....
!fit &"path1"f1636832.fic b.img perc=0. 'byte exclude=(-32768,0)

!photfunc b.img a.img HAPKE=(.5,.2,10.,.5,.2) target=IO
!list a.img ss=200 linc=60 sinc=60

!cform b.img c.img 'half
!photfunc c.img a.img HAPKE=(.5,.2,10.,.5,.2) target=IO
!list a.img ss=200 linc=60 sinc=60

!cform b.img c.img 'full
!photfunc c.img a.img HAPKE=(.5,.2,10.,.5,.2) target=IO
!cform a.img c.img 'half
!list c.img ss=200 linc=60 sinc=60

!cform b.img c.img 'real
!photfunc c.img a.img HAPKE=(.5,.2,10.,.5,.2) target=IO
!cform a.img c.img 'half
!list c.img ss=200 linc=60 sinc=60

!------------------------Classification map option------------------------------
! Get a 450x450 area of a color triplet and convert to byte (for STATS)
!fit &"path1"io.org a.img (1,1,450,450) 'byte
!fit &"path1"io.blu b.img (1,1,450,450) 'byte
!fit &"path1"io.vio c.img (1,1,450,450) 'byte

! Create classification map
!stats (a.img,b.img,c.img) stats.img exclude=0 +
!  class1=(188,81,20,20) class2=(369,80,20,20)
!fastclas (a.img,b.img,c.img,stats.img) class.img sigma=(20.,10.)

! Replace classification map with gen data for testing program
!gen a.img nl=2 ns=2
!size a.img b.img zoom=225 'noin
!labswtch (class.img,b.img) a.img
!copy a.img class.img

! Input map projection info since PHOTFUNC won't handle ancient labels.
!copy  &"path1"io.vio c.img (1,1,450,450)
!photfunc (c.img,class.img) a.img inc=100 'NOSPICE +
!    RADII=(1829.4,1819.3,1815.7) + class=1 +
!    SOLAR=(0.541,171.276) RSVECTOR=(-739030.7,-321741.5,-456.1) +
!    'CYLI SCALE=8. NORTH=0. +
!    LATI=80. LONG=230. LINE=225. SAMP=1.
!label-replace a.img item="TYPE=IMAGE" 'sys
if ($syschar(1) = "VAX_VMS")
   dcl delete a.img;*
   dcl delete b.img;*
else
   ush rm a.img
   ush rm b.img
end-if
end-proc
$ Return
$!#############################################################################
