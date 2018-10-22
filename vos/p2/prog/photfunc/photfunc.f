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
