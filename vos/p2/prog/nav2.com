$!****************************************************************************
$!
$! Build proc for MIPL module nav2
$! VPACK Version 2.1, Thursday, February 18, 2016, 16:04:50
$!
$! Execute by entering:		$ @nav2
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
$ write sys$output "*** module nav2 ***"
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
$ write sys$output "Invalid argument given to nav2.com file -- ", primary
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
$   if F$SEARCH("nav2.imake") .nes. ""
$   then
$      vimake nav2
$      purge nav2.bld
$   else
$      if F$SEARCH("nav2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake nav2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @nav2.bld "STD"
$   else
$      @nav2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create nav2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack nav2.com -mixed -
	-s nav2.f vparam.f maplabel.f editnav.f tac.f planet.f sdsply.f zvp.f -
	   jupiter_zvp.f const.fin cpic.fin cpts.fin dev.fin cmap.fin -
	   xdgclr.fin -
	-p nav2.pdf -
	-i nav2.imake -
	-t tstnav2.pdf tstnav2.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create nav2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vparam.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C==============================================================================
c prompt user for navvel parameters.
c common area zvp is filled.
C==============================================================================
      subroutine vparam(navmode)
      IMPLICIT NONE
      integer navmode	!0=no wind adjustment, 1=yes, 2=split errors

      include 'cpic.fin'
      include 'cpts.fin'

      common/zvp/nz,zvp(2,1000)
      integer nz
      real*4 zvp

      common/c1_coords/coords(2,500), rcoords(2, 500)
      real*4 coords, rcoords

      integer navmode0,id,mpv
      logical parmtst

      navmode0 = navmode
      id = mod(target_id,100)
      if (id.ne.99) goto 990
      if (target_id.ne.599.and.navmode.eq.0.and.npts.gt.0) goto 992
      call xvmessage(' Do you wish to split the pointing error',' ')
      call xvmessage(' between the two frames?',' ')
   10 call xvintract('QUERY',' Enter Y or N')
      if (parmtst('EXIT', mpv, 1)) return
      if (parmtst('Y', mpv, 1)) then
         navmode = 2
      else if (parmtst('N', mpv, 1)) then
         navmode = 1
      else
         goto 10
      endif

C     ....check if zonal velocity profile is to be used, 
C     ....only Voyager on Jupiter is valid
      if (project_id.ne.4 .or. target_id.ne.599) return
      if (nz.gt.0) goto 30		! already being used

      if (navmode0.eq.0.and.npts.gt.0) then
         call xvmessage(' Zonal velocity profile will be used to',' ')
         call xvmessage(' adjust for wind speed.',' ')
         goto 25
      endif

      call xvmessage(' Do you wish to use the zonal velocity profile?',
     +              ' ')
   20 call xvintract('QUERY',' Enter Y or N')
      if (parmtst('N', mpv, 1)) return
      if (.not.parmtst('Y', mpv, 1)) goto 20

   25 call jupiter_zvp(zvp,nz)

c     ....get zonal velocities for all existing tiepoints
   30 if (navmode.eq.1.and.npts.gt.0) then
         call getlatlon(2,npts,rpt_os,coords)
         call getzv(npts,coords,u,v)
      endif
      return

  990 call xvmessage('***Wind speed adjustment valid only for',' ')
      call xvmessage(' ***planets with atmospheres',' ')
      goto 999
  992 call xvmessage('***Wind speed adjustment must be selected',' ')
      call xvmessage('***upon entry to the program.',' ')
  999 return
      end

C==============================================================================
c  get user command:
c  upon return, parmtst=.true. if user has entered the command "parm",
c     =.false. otherwise.
c  if the command is of the form keyword=value, then the values are
c  stored in array nn if count is 2 or 3.
c  all args must be passed but set count to number of args desired 
C==============================================================================
      logical function parmtst(parm,nn,count)
      IMPLICIT NONE
      character*(*) parm
      integer nn(1),count

      integer n(10),cnt,def

      parmtst = .false.
      if (count .eq. 1) then
         call xviparm(parm,n,cnt,def,' ')
      else 
         call xviparm(parm,nn,cnt,def,' ')
      endif
      if (def .eq. 0) parmtst = .true.
      return
      end

C==============================================================================
c sends message to user, prompting for a string of length length.
c the string is returned in upper-case and blank-filled on the right.
c ind=1 for success, =0 if cancelled by user.
C==============================================================================
      subroutine getstring(message,length,string,ind)
      IMPLICIT NONE
      character*(*) message
      integer length
      character*(*) string
      logical parmtst

      integer ind, mpv, icnt, idef

   10 call xvintract('STRING',message)
      if (parmtst('EXIT', mpv, 1)) then
         ind = 0
         return
      endif
      call xviparm('STRNG',string,icnt,idef,' ')
      if (idef.ne.0) goto 10
      call uprcase(string)
      call bfstr(string,length)
      ind = 1
      return
      end

C==============================================================================
c get the project, frame, and camera ids
c   img = input logical unit number for frame
c   lbuf = output buffer returned by getlabcon
C==============================================================================
      subroutine frameid(img,project,lbuf,frame_id,camera_id,
     &                   project_id,ind)
      IMPLICIT NONE
      character*5 project
      integer img,lbuf(80),frame_id,camera_id,project_id,ind,status
      logical parmtst

      call getproj(img,project,camera_id,frame_id,ind)
      if (ind.eq.0) goto 12

   10 call xvmessage(' ***Unknown project ID',' ')
      call getstring('Enter project ID',5,project,ind)
      if (ind.eq.0) return

   12 if (project.eq.'MAR-9') then
         project_id = 1		!mariner 9
      else if (project.eq.'MAR10') then
         project_id = 2		!mariner 10
      else if (project.eq.'VIKOR') then
         project_id = 3		!viking orbitor
      else if (project.eq.'VGR-1' .or. project.eq.'VGR-2') then
	 project_id = 4		!voyager
      else if (project.eq.'GLL') then
         project_id = 5		!galileo
      else if (project.eq.'CASSI') then
         project_id = 6		!cassini
      else
         call xvmessage(' ***Invalid project ID',' ')
         goto 10
      endif

      call getlabcon(img,project,lbuf,ind)
      if (ind.eq.2 .or. lbuf(2).eq.-999) then
         call xvintract('IVALUE','Enter frame number')
         status=parmtst('VALUE',frame_id, 2)
      endif

      if (ind.eq.2 .or. lbuf(6).eq.-999) then
         if (project_id.eq.5) then
            camera_id = 1		!only one camera on galileo
         else
            call xvintract('IVALUE','Enter camera serial number')
            status=parmtst('VALUE',camera_id, 2)
         endif
      endif
      ind = 1
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create maplabel.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create editnav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tac.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create planet.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sdsply.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C==============================================================================
c split screen display controlling routine...
C==============================================================================
      subroutine sdsply(pic1,nl1,ns1,pic2,nl2,ns2,*)
      IMPLICIT NONE
      integer ns1,nl1,ns2,nl2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'dev.fin'

      common/c1_buf/buf(1024)
      integer buf
      common/chist/his(256,2),hflg(2),ihis(256,2),ihflg(2),nspikes
      integer his,hflg,ihis,ihflg,nspikes
      common/ipic2/nl(2),ns(2)
      integer nl,ns
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss

      logical xst
      integer*4 par(2),iflag,mpv,il,ih,n

      logical xdifill,parmtst

      iflag = 0

      if (parmtst('H', mpv, 1)) then
         call home(0,pic1,nl1,ns1,pic2,nl2,ns2)
         iflag = 1
      endif

      if (parmtst('STRETCH',par, 2)) then
           il = par(1)
           ih = par(2) 
           call strech(il,ih,buf)
           call lutwrite(idev,buf)
           iflag = 1
      endif

      if (parmtst('STR1',par, 2)) then
         ilow(1) = par(1)
         ihigh(1) = par(2)
         call strech(ilow,ihigh,buf)
         call mve(-5,256,buf,stbl, 1, 1)
         call spic(1,pic1,nl1,ns1,buf)
         call tpdisplay(1)
         iflag = 1
      endif

      if (parmtst('STR2',par, 2)) then
         ilow(2) = par(1)
         ihigh(2) = par(2)
         call strech(ilow(2),ihigh(2),buf)
         call mve(-5,256,buf,stbl(1,2), 1, 1)
         call spic(2,pic2,nl2,ns2,buf)
         call tpdisplay(2)
         iflag = 1
      endif

      if (parmtst('GERASE', mpv, 1)) then
           xst = xdifill(idev,g,0)
           iflag = 1
      endif

      if (parmtst('SPIKES',n, 2)) then
           nspikes=n
           iflag = 1
      endif

      if (parmtst('SPTS', mpv, 1)) then
           call tpdisplay(1)
           call tpdisplay(2)
           iflag = 1
      endif

      if (parmtst('HIST',mpv,1)) then
         call hisgen(pic1,sl(1),ss(1),nl1,ns1,ihis,ihflg(1))
         call hdisplay(idev,g,ihis,101,101,100,nspikes)
         call hisgen(pic2,sl(2),ss(2),nl2,ns2,ihis(1,2),ihflg(2))
         call hdisplay(idev,g,ihis(1,2),101,nsd/2+101,100,nspikes)
         iflag = 1
      endif

      if (iflag.eq.1) return1
      return
      end

C==============================================================================
c routine to move the window(s) of the split-screen display, acting on
c commands input by the user.  if any of the commands below are detected,
c then video plane v2 is used.  alternate return if any action is taken.
C==============================================================================
      subroutine swindow(pic1,nl1,ns1,pic2,nl2,ns2,*)
      IMPLICIT NONE
      integer nl1, ns1, nl2, ns2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'dev.fin'

      common/c1_buf/buf(1024)
      integer buf
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      integer sl1,ss1,sl2,ss2,n
      logical both,left,right,parmtst

c     ....first, reset all flags
      left = .false.		!display left screen
      right = .false.		!display right screen
      both = .false.		!display both screens

      sl1 = sl(1)
      ss1 = ss(1)
      sl2 = sl(2)
      ss2 = ss(2)

      if (parmtst('CZOOM',n, 2)) then
         izoom1 = n
         zoom1 = n
         if (n .lt. 0) zoom1 = -1./n
         call czoom(zoom1,sl1,ss1,sl2,ss2,*999)
         both = .true.
         goto 100
      endif

      if (parmtst('SL',n, 2)) then
         sl1 = n
         sl2 = n
         both = .true.
      endif

      if (parmtst('SL1',n, 2)) then
         sl1 = n
         left = .true.
      endif

      if (parmtst('SL2',n, 2)) then
         sl2 = n
         right = .true.
      endif

      if (parmtst('SS',n, 2)) then
         ss1 = n
         ss2 = n
         both = .true.
      endif

      if (parmtst('SS1',n, 2)) then
         ss1 = n
         left = .true.
      endif

      if (parmtst('SS2',n, 2)) then
         ss2 = n
         right = .true.
      endif

      if (parmtst('U',n, 2))   then
         sl1 = sl1 - n
         sl2 = sl2 - n
         both = .true.
      endif

      if (parmtst('D',n, 2))   then
         sl1 = sl1 + n
         sl2 = sl2 + n
         both = .true.
      endif

      if (parmtst('U1',n, 2)) then
         sl1 = sl1 - n
         left = .true.
      endif

      if (parmtst('D1',n, 2)) then
         sl1 = sl1 + n
         left = .true.
      endif

      if (parmtst('U2',n, 2)) then
         sl2 = sl2 - n
         right = .true.
      endif

      if (parmtst('D2',n, 2)) then
         sl2 = sl2 + n
         right = .true.
      endif

      if (parmtst('L',n, 2))  then
         ss1 = ss1 - n
         ss2 = ss2 - n
         both = .true.
      endif

      if (parmtst('R',n, 2))  then
         ss1 = ss1 + n
         ss2 = ss2 + n
         both = .true.
      endif

      if (parmtst('L1',n, 2)) then
         ss1 = ss1 - n
         left = .true.
      endif

      if (parmtst('R1',n, 2)) then
         ss1 = ss1 + n
         left = .true.
      endif

      if (parmtst('L2',n, 2)) then
         ss2 = ss2 - n
         right = .true.
      endif

      if (parmtst('R2',n, 2)) then
         ss2 = ss2 + n
         right = .true.
      endif

      if (parmtst('ZOOM',n, 2)) then
         izoom1 = n
         zoom1 = n
         if (n .lt. 0) zoom1 = -1./n
         both = .true.
      endif

  100 if (both) then
          left = .true.
          right = .true.
      endif

      if (.not.left .and. .not.right) return

      if (vid.ne.v2) then
         vid = v2
         call impon(idev,vid)
      endif

      zoom = zoom1
      izoom = izoom1

      if (left) then
         call cwindow(1,sl1,ss1)
         call spic(1,pic1,nl1,ns1,buf)
         call tpdisplay(1)
      endif

      if (right) then
         call cwindow(2,sl2,ss2)
         call spic(2,pic2,nl2,ns2,buf)
         call tpdisplay(2)
      endif

  999 return1
      end

C==============================================================================
c routine to check window movement to keep display within picture boundaries.
c inputs: ipic=1 for left image, =2 for right image
c         (sli,ssi)=picture offsets for display
c outputs: sl(ipic),ss(ipic)
C==============================================================================
      subroutine cwindow(ipic,sli,ssi,ind)
      IMPLICIT NONE
      integer ipic,sli,ssi,ind

      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      character*80 msg
      integer maxsl,maxss

  110 format(' (SL',i1,',SS',i1,')=(',i4,',',i4,')  ZOOM=',i2)

      maxsl = nl(ipic) - nld/zoom + 1
      maxss = ns(ipic) - nsd/(2.*zoom) + 1
      sli = min0(sli,maxsl)
      ssi = min0(ssi,maxss)
      sl(ipic) = max0(sli,1)
      ss(ipic) = max0(ssi,1)
      write(msg,110) ipic,ipic,sl(ipic),ss(ipic),izoom
      call xvmessage(msg,' ')
      return
      end

C==============================================================================
c full-frame split-screen display.  all arguments are inputs.
C==============================================================================
      subroutine home(mode,pic1,nl1,ns1,pic2,nl2,ns2)
      IMPLICIT NONE
      integer mode,ns1,nl1,ns2,nl2
      byte pic1(ns1,nl1),pic2(ns2,nl2)

      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

C      common/c1_buf/buf(1024)		!temporary work area
      byte buf(1024)

      vid = v1
      zoom = zoom2
      izoom = izoom2
      call impon(idev,vid)
      sl(1) = 1
      ss(1) = 1
      sl(2) = 1
      ss(2) = 1
c     ....display if necessary
      if (v1.eq.v2.or.mode.eq.1) then
         call spic(1,pic1,nl1,ns1,buf)	!display left image
         call spic(2,pic2,nl2,ns2,buf)	!display right image
      endif
      call tpdisplay(1)			!display left tiepoints
      call tpdisplay(2)			!display right tiepoints
      return
      end

C==============================================================================
c determine line and sample offsets which center the left and right display
c about a point selected by the user via the cursor.
c outputs: (sl1,ss1) and (sl2,ss2)
C==============================================================================
      subroutine czoom(zoom_1,sl1,ss1,sl2,ss2,*)
      IMPLICIT NONE
      integer*4 sl1,ss1,sl2,ss2
      real zoom_1

      include 'dev.fin'

      integer*4 s1,s2,mpv,l1,l2,ind
      real*8 rl1,rs1,rlat,rlon,rl2,rs2
      logical parmtst,xst,xdcset

c     ....user selects point in left image
   10 call xvintract('READY',' Position LEFT Cursor')
      if (parmtst('EXIT', mpv, 1)) return1
      call cursor(1,l1,s1,rl1,rs1,*10)
c     ....predict corresponding location of point in the right image
      call latlon2(1,rl1,rs1,rlat,rlon,ind)
      if (ind.eq.1) call linsam2(2,rlat,rlon,rl2,rs2,ind)
      if (ind.eq.1) then
         l2 = (rl2-sl2)*zoom
         s2 = (rs2-ss2)*zoom + nsd/2
         l2 = max0(l2,1)
         s2 = max0(s2,nsd/2)
         l2 = min0(l2,nld)
         s2 = min0(s2,nsd)
      else
         l2 = l1
         s2 = s1 + nsd/2
      endif
      xst = xdcset(idev,tb,s2,l2)	!move cursor to point in right image
c     ....user selects point in right image
   20 call xvintract('READY',' Position RIGHT Cursor')
      if (parmtst('EXIT', mpv, 1)) return1
      call cursor(2,l2,s2,rl2,rs2,*20)
c     ....compute line and sample offsets in the display
      sl1 = rl1 - nld/(2.*zoom_1)
      ss1 = rs1 - nsd/(4.*zoom_1)
      sl2 = rl2 - nld/(2.*zoom_1)
      ss2 = rs2 - nsd/(4.*zoom_1)
      xst = xdcset(idev,tb,nsd/4,nld/2)		!move cursor to left image
      return
      end

C==============================================================================
c routine to write the image to split-screen display
C==============================================================================
      subroutine spic(ipic,pic,nli,nsi,buf)
      IMPLICIT NONE
      integer ipic,nsi,nli
      byte pic(nsi,nli),buf(1024)

      include 'dev.fin'

      common/chist/his(256,2),hflg(2),ihis(256,2),ihflg(2),nspikes
      integer his,hflg,ihis,ihflg,nspikes
      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss

      integer sli,ssi,nsds,nlds,ssds,istrech,esds,slds,inc,nlo,nso,
     +        l,i,n
      logical xst

      logical xdiawset,xdifill,xdilinewrite

      hflg(ipic) = 0
      ihflg(ipic) = 0
      sli = sl(ipic)
      ssi = ss(ipic)
      nsds = nsd/2
      nlds = nld
      ssds = 1
      if (ipic.eq.2) ssds = nsds + 1
      esds = ssds + nsds - 1
      xst = xdiawset(idev,vid,ssds,1,esds,nlds)	!set access window
      xst = xdifill(idev,vid,0)			!erase image plane
c     ....determine if a stretch is to be applied before writing to display
      if (ilow(ipic).eq.0.and.ihigh(ipic).eq.255) then
         istrech = 0		!no stretch
      else
         istrech = 1		!apply stretch
      endif
c
      if (izoom.eq.1) then
          nlds = min0(nli-sli+1,nlds)
          if (ipic.eq.1) nsds = nsds - 1
          nsds = min0(nsi-ssi+1,nsds)
          xst = xdiawset(idev,vid,ssds,1,ssds+nsds-1,nlds)

          do slds=1,nlds
             if (istrech.eq.1) then
                call lut(nsds,pic(ssi,sli+slds-1),stbl(1,ipic),buf)
                xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
             else
       write(*,*) 'Not changing LUT'
                xst = xdilinewrite(idev,vid,ssds,slds,nsds,
     &			pic(ssi,sli+slds-1))
             endif
             if (.not.xst) goto 999
          enddo
          xst = xdiawset(idev,vid,1,1,nsd,nld)	!reset window to full-screen
          return
      endif

      if (izoom.lt.1) then
          inc = -izoom
          nlds = min0((nli-sli)/inc+1,nlds)
          n = (nsi-ssi)/inc + 1
          if (n.lt.nsds) then
              nsds = n
              xst = xdiawset(idev,vid,ssds,1,ssds+nsds-1,nlds)
          endif
          if (ipic.eq.1) then
               buf(nsds)=.false.	!put vertical black line
               n = nsds - 1		!between left and right frames
          else
               n = nsds
          endif
          do slds=1,nlds
             call mve(1,n,pic(ssi,sli),buf,inc, 1)    ! check the 1 for binc
             if (istrech.eq.1) call lut(n,buf,stbl(1,ipic),buf)
             xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
             if (.not.xst) goto 999
             sli = sli + inc
          enddo
          xst = xdiawset(idev,vid,1,1,nsd,nld)	!reset window to full-screen
          return
      endif


      if (izoom.gt.1) then
         nlo = min0(nli-sli+1,nlds/izoom)
         nso = min0(nsi-ssi+1,nsds/izoom)
         nsds = nso*izoom
         xst = xdiawset(idev,vid,ssds,1,ssds+nsds-1,nlds)
         slds = 0

         do l=1,nlo
            if (istrech.eq.1) then
                call lut(nsds,pic(ssi,sli+l-1),stbl(1,ipic),buf)
                call expand(buf,buf,nso,izoom)
            else
                call expand(pic(ssi,sli+l-1),buf,nso,izoom)
            endif
            if (ipic.eq.1) buf(nsds)=.false.
            do i=1,izoom
               slds = slds + 1
               xst = xdilinewrite(idev,vid,ssds,slds,nsds,buf)
               if (.not.xst) goto 999
            enddo
         enddo
         xst = xdiawset(idev,vid,1,1,nsd,nld)	!reset window to full-screen
         return
      endif
c
      return
c     ....here if error writing to image display
  999 return
      end

C==============================================================================
c routine to display a histogram on the graphics plane g, starting at
c pixel coordinates (l0,s0).
C==============================================================================
      subroutine hdisplay(idev,g,his,l0,s0,ihght,nspikes)
      IMPLICIT NONE
      integer idev,g
      integer his(1),l0,s0,ihght,nspikes,maxf/268435455/,x,y,dx,tic

      include 'xdgclr.fin'

      integer xx(10),yy(10),n,maxs,max,j,ifreq,i,l,ierr
      real height,zscale
      character*3 msg

      integer xdttext,xdipolyline,xdtcolor

c          linc must be a power of two
      n = nspikes + 1
      maxs = maxf
c          scale histogram to max frequency
      do j=1,n
         max = 0
         do i=2,254
            ifreq = his(i)
            if (ifreq.gt.max .and. ifreq.lt.maxs) max=ifreq
         end do
         maxs = max
      end do
c
      height = ihght
      zscale = 1.
      if (max.gt.1) zscale = height/alog10(float(max))
      x = s0
      y = l0

      ierr=xdtcolor(RED,0)
      do 50 l=0,255
      tic = 0
      if (mod(l,10).eq.0) tic=2
      if (mod(l,50).eq.0) then
C           call outcon(l,msg(3),3)
	   write(msg, '(I3)') l
           ierr = xdttext(idev,g,x-34,y+4,1,3,msg)
           tic = 4
      endif

      ifreq = his(l+1)
      if (ifreq.gt.1) then
           dx = zscale*alog10(float(ifreq))
      else
           dx = 0.0
      endif

      if(dx.le.height) goto 30
      dx = height
      xx(1) = x + dx + 2
      yy(1) = y
      xx(2) = x + dx + 2
      yy(2) = y
      ierr = xdipolyline(idev,g,RED,2,xx,yy)
   30 xx(1) = x - tic
      yy(1) = y
      xx(2) = x + dx
      yy(2) = y
      ierr = xdipolyline(idev,g,RED,2,xx,yy)
      y = y + 1
   50 continue
c
      return
      end

C==============================================================================
c routine to display the tiepoints in graphics...
c ipic=1 to display tiepoints in left picture
c     =2 to display tiepoints in right picture
C==============================================================================
      subroutine tpdisplay(ipic)
      IMPLICIT NONE
      integer ipic

      include 'cpts.fin'
      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      character*3 gmsg
      integer s,l,sld,ssd,nsds,ssds,esds,i,nch
      logical xst
      real rl,rs

      logical xdiawset,xdifill,xdipixelwrite,xdttext

      nsds = nsd/2
      ssds = 1
      if (ipic.eq.2) ssds=nsds+1
      esds = ssds + nsds - 1
      xst = xdiawset(idev,g,ssds,1,esds,nld)	!set access window
      xst = xdifill(idev,g,0)			!erase graphics

      sld = sl(ipic)
      ssd = ss(ipic)

      do 50 i=1,npts
      if (ipic.eq.1) then
          rl = lpt(1,i)
          rs = lpt(2,i)
      else
          rl = rpt(1,i)
          rs = rpt(2,i)
      endif

      if (rl.lt.0.0.or.rs.lt.0.0) goto 50
      l = (rl-sld)*zoom + 1.5
      s = (rs-ssd)*zoom + 1.5
      if (l.lt.0.or.s.lt.0) goto 50
      if (l.gt.nld.or.s.gt.nsds) goto 50
      if (ipic.eq.2) s=s+nsds
      if (.not.xdipixelwrite(idev,g,s,l,255)) goto 999
C      nch = 1
C      if (i.ge.10) nch=2
C      if (i.ge.100) nch=3
C      call outcon(i,gmsg(nch),nch)
      if (i.lt.10) then
      	  write(gmsg, '(I1)') i
	  nch = 1
      else if (i.ge.10) then
      	  write(gmsg, '(I2)') i
	  nch = 2
      else if (i.ge.100) then
      	  write(gmsg, '(I3)') i
	  nch = 3
      endif
      xst = xdttext(idev,g,s+2,l+10,1,nch,gmsg)
   50 continue

      xst = xdiawset(idev,g,1,1,nsd,nld)  !reset window to full-screen
      return
c     ....here if error writing to graphics display
  999 return
      end

C==============================================================================
c read cursor and return display coordinates (l,s) and image
c coordinates (rl,rs) for left or right image.
C==============================================================================
      subroutine cursor(ipic,l,s,rl,rs,*)
      IMPLICIT NONE
      integer ipic,l,s
      real*8 rl,rs

      include 'dev.fin'

      common/ipic/img(2),sl(2),ss(2)
      integer img,sl,ss
      common/ipic2/nl(2),ns(2)
      integer nl,ns

      logical xst

      logical xdclocation

      xst = xdclocation(idev,tb,s,l)
      rl = (l-1)/zoom + sl(ipic)
      if (ipic.eq.1) then
         rs = (s-1)/zoom + ss(ipic)
         if (rl.gt.nl(1).or.rs.gt.ns(1).or.s.gt.nsd/2) goto 995
      else
         rs = (s-nsd/2-1)/zoom + ss(ipic)
         if (rl.gt.nl(2).or.rs.gt.ns(2).or.s.le.nsd/2) goto 996
      endif
      return

  995 call xvmessage(' ***Cursor lies off LEFT picture',' ')
      goto 999
  996 call xvmessage(' ***Cursor lies off RIGHT picture',' ')
  999 return1
      end

C==============================================================================
c generate histogram from area (sl,ss,nl,ns) of input image.
c output: his = 256 grey-level histogram.
c updated: upon return, hflg =1
C==============================================================================
      subroutine hisgen(pic,sl,ss,nl,ns,his,hflg)
      IMPLICIT NONE
      integer sl,ss,ns,nl,his(256),hflg
      byte pic(ns,nl)

      include 'dev.fin'

      integer nlo,nso,el,inc,l

      if (hflg.eq.1) return		!skip if histogram already computed
      nlo = nld/zoom + 0.001
      nso = nsd/(2.*zoom) + 0.001
      nlo = min0(nl-sl+1,nlo) - 1
      nso = min0(ns-ss+1,nso)
      el = sl + nlo - 1
      inc = 1
      if (izoom.lt.0) inc=-izoom
      call zia(his,256)			!zero out the histogram
c     ....accumulate the histogram
      do l=sl,el,inc
         call hsub(1,nso,pic(ss,l),his)
      enddo

      hflg = 1
      return
      end

C==============================================================================
c generate a stretch table (stbl) to linearly stretch an image.
C==============================================================================
      subroutine strech(i1,i2,stbl)
      IMPLICIT NONE
      integer i1,i2,stbl(1)

      integer d,offset,k,i

      d = i2 - i1
      if (d .eq. 0) d = 1
      offset = d/2 - 255*i1
      k = 0

   10 i = (255*k+offset)/d
      if (i .lt. 0) i=0
      if (i .gt. 255) i=255
      stbl(k+1) = i
      k = k + 1
      if (k .le. 255) goto 10
c
      return
      end

C==============================================================================
c expand an image line by and integral factor (izoom) by pixel replication...
c input array: pic    output array: buf
c note the pic and buf may be concurrent...
C==============================================================================
      subroutine expand(pic,buf,ns,izoom)
      IMPLICIT NONE
      byte pic(1),buf(1)
      integer ns,izoom

      integer k,i,j

      k = izoom*ns
      do i=ns,1,-1
           do j=1,izoom
               buf(k) = pic(i)
               k = k - 1
           enddo
      enddo
      return
      end

C==============================================================================
c open and initialize the video plane, graphics plane, and cursor.
c alternate return if unsuccessful.
C==============================================================================
      subroutine device2(nl1,ns1,nl2,ns2,*)
      IMPLICIT NONE
      integer nl1,ns1,nl2,ns2

      include 'dev.fin'
      include 'xdgclr.fin'

      integer buf(256), info(80), con(4),lut,xdsvnl,xdsvns,nl,ns,n,i

      logical xdeaction,xddunit,xddopen,xddactivate,xddinfo,
     +        xddconfigure,xdlramp,xdgconnect,xdglinit,
     +        xdgon,xdtfont,xdtsize,xdtrotate,xdcon,xdcset,
     +        xdcautotrack,xdiawset,xdsgraph
      integer xdgcolor

      if (.not.xdeaction(2,2,3)) return1	!define error action
      if (.not.xddunit(idev)) return1		!get logical unit number
      if (.not.xddopen(idev)) return1		!open device
      if (.not.xddactivate(idev,.true.)) return1 !activate device
      if (.not.xddinfo(idev,1,80,info)) return1
      v1 = 1				!variable field image plane
      v2 = 2				!full-field (home) image plane
      v3 = 3				!unused
      if (info(4).lt.2) v2=v1
      if (info(4).lt.3) v3=v2

      tb = 1				!cursor
c     ....configure the display for b/w mode
      con(1) = 0			!default color display
      con(2) = 0			!default memory plane size
      con(3) = 0			!default video size
      con(4) = 0			!default aspect ratio
      if (.not.xddconfigure(idev,con)) return1
c     ....eliminate stretches in luts
      do lut=1,3
         if (.not.xdlramp(idev,lut,1)) return1
      enddo
      g = xdsgraph(idev)   ! g = graphic plane

c     ....pick plane for g
      if (.not.xdgconnect(idev,g,1,.false.)) return1
      if (.not.xdglinit(idev,1)) return1
      RED = xdgcolor(idev, 'RED')
      CYAN = xdgcolor(idev,'CYAN')
      if (RED*CYAN .EQ. 0) return1
      TRANSPARENT = 0
      
      if (.not.xdgon(idev)) return1		!turn g on
      if (.not.xdtfont(30)) return1		!set font type
      if (.not.xdtsize(8,1.0)) return1		!set text size
      if (.not.xdtrotate(0)) return1
      if (.not.xdcon(idev,tb,1,0)) return1	!turn cursor on
      if (.not.xdcset(idev,tb,256,256)) return1	!center cursor
      if (.not.xdcautotrack(idev,tb,tb,.true.)) return1
c     ....initialize zoom factors.
      nl = max0(nl1,nl2)
      ns = max0(ns1,ns2)
      nld = xdsvnl(idev)
      nsd = xdsvns(idev)
      n = max0((nl-1)/nld,2*(ns-1)/nsd) + 1
      if (n.gt.1) then
         zoom1 = 1.0/n
         izoom1 = -n
      else
         zoom1 = 1
         izoom1 = 1
      endif

      zoom2 = zoom1
      izoom2 = izoom1
c     ....set access window to full-screen
      if (.not.xdiawset(idev,g,1,1,nsd,nld)) return1
      if (.not.xdiawset(idev,v1,1,1,nsd,nld)) return1
      if (.not.xdiawset(idev,v2,1,1,nsd,nld)) return1

c     ....initialize stretch tables
      do i=1,2
         ilow(i) = 0
         ihigh(i) = 255
         call strech(0,255,buf)
         call mve(-5,256,buf,stbl(1,i), 1, 1)
      enddo
      return
      end
c connects all luts to specified image plane (imp)
c
      subroutine impon(idev,imp)

      do lut=1,3
         call xdlconnect(idev,imp,lut,1,.false.)
      end do
      return
      end

      subroutine lutwrite(idev,stbl)
      integer stbl(256)

      do lut=1,3
         call xdlwrite(idev,lut,1,stbl)
      end do
      return
      end

C Detect end of zero-terminated string and blank-fill.
      SUBROUTINE bfstr(buf,nchar)
      IMPLICIT NONE
      integer nchar
      byte buf(nchar)

      integer izero,i

      izero = 0
      do i=1,nchar
         if (buf(i).eq.0) izero=1
         if (izero.eq.1) buf(i)=32
      enddo
      return
      end


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zvp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      include 'VICMAIN_FOR'
      subroutine main44
      common/zvp/nz,zvp(2,1000)
      character*80 msg
  101 format(f6.2,',',f6.2,',')

      rtd = 180.d0/3.141592653589793d0
      call xvunit(iunit,'Z',1,ind,'U_NAME','JUPITER.ZVP')
      call xvsignal(iunit,ind,.true.)
      call xvopen(iunit,ind,'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(iunit,ind,'NL',nl,'NS',ns,' ')
      nz = (ns-2)/2
      call prnt(4,1,nz,'nz=.')
      call xvread(iunit,zvp,ind,' ')
      call xvclose(iunit,ind,' ')
      do i=1,nz
         write(msg,101) rtd*zvp(1,i),zvp(2,i)
         call xvmessage(msg,' ')
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create jupiter_zvp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Return Jupiter zonal velocity profile (based on VGR data)
c
      subroutine jupiter_zvp(zvp,nz)
      implicit none
      real*4 zvp(2,807)
      integer nz,i
      real*4 jzvp(2,807)/-55.26, -3.00,-55.03, -3.50,-54.81, -3.30,
     +	-54.58, -3.20,-54.36, -3.30,-54.14, -3.30,-53.92, -3.30,
     +	-53.71, -3.50,-53.49, -3.40,-53.27, -3.90,-53.06, -4.10,
     +	-52.85, -3.20,-52.64, -2.20,-52.43, -1.90,-52.22, -1.40,
     +	-52.01, -0.10,-51.80,  1.70,-51.60,  3.70,-51.39,  6.00,
     +	-51.19,  8.60,-50.98, 11.40,-50.78, 14.60,-50.58, 17.90,
     +	-50.38, 22.10,-50.18, 27.00,-49.99, 30.80,-49.79, 33.50,
     +	-49.59, 35.60,-49.40, 36.30,-49.21, 35.70,-49.01, 34.30,
     +	-48.82, 32.00,-48.63, 28.80,-48.44, 25.00,-48.25, 20.80,
     +	-48.06, 16.60,-47.87, 12.70,-47.69,  9.60,-47.50,  7.30,
     +	-47.31,  5.40,-47.13,  3.60,-46.95,  2.10,-46.76,  0.50,
     +	-46.58, -1.20,-46.40, -2.80,-46.22, -4.30,-46.04, -5.50,
     +	-45.86, -6.60,-45.68, -7.30,-45.50, -7.70,-45.32, -8.00,
     +	-45.15, -8.00,-44.97, -7.90,-44.80, -7.60,-44.62, -6.90,
     +	-44.45, -6.10,-44.27, -5.00,-44.10, -3.70,-43.93, -2.00,
     +	-43.76, -0.40,-43.58,  1.20,-43.41,  2.60,-43.24,  3.90,
     +	-43.07,  5.10,-42.91,  6.30,-42.74,  7.80,-42.57,  9.20,
     +	-42.40, 10.60,-42.24, 12.10,-42.07, 13.70,-41.91, 15.20,
     +	-41.74, 16.50,-41.58, 17.90,-41.41, 19.40,-41.25, 21.00,
     +	-41.09, 22.60,-40.92, 24.60,-40.76, 26.80,-40.60, 28.90,
     +	-40.44, 31.20,-40.28, 34.00,-40.12, 36.80,-39.96, 39.20,
     +	-39.80, 41.00,-39.64, 42.00,-39.48, 42.10,-39.33, 41.30,
     +	-39.17, 39.60,-39.01, 37.10,-38.86, 33.90,-38.70, 30.40,
     +	-38.55, 26.70,-38.39, 22.80,-38.24, 19.20,-38.08, 15.90,
     +	-37.93, 13.00,-37.77, 10.80,-37.62,  9.20,-37.47,  8.20,
     +	-37.32,  7.50,-37.17,  7.00,-37.01,  6.60,-36.86,  6.20,
     +	-36.71,  5.80,-36.56,  5.40,-36.41,  4.90,-36.26,  4.40,
     +	-36.11,  4.00,-35.97,  3.70,-35.82,  3.50,-35.67,  3.50,
     +	-35.52,  3.70,-35.37,  4.10,-35.23,  4.60,-35.08,  5.30,
     +	-34.93,  6.00,-34.79,  6.90,-34.64,  7.80,-34.50,  8.70,
     +	-34.35,  9.70,-34.21, 10.90,-34.06, 12.40,-33.92, 14.20,
     +	-33.78, 16.40,-33.63, 18.90,-33.49, 21.80,-33.35, 24.70,
     +	-33.20, 27.40,-33.06, 29.40,-32.92, 30.60,-32.78, 31.30,
     +	-32.64, 31.60,-32.50, 31.30,-32.36, 29.60,-32.22, 27.30,
     +	-32.08, 24.40,-31.94, 21.40,-31.80, 18.30,-31.66, 15.90,
     +	-31.52, 13.80,-31.38, 11.80,-31.24,  9.80,-31.10,  7.90,
     +	-30.97,  6.10,-30.83,  4.50,-30.69,  3.00,-30.55,  1.40,
     +	-30.42, -0.20,-30.28, -2.00,-30.14, -3.80,-30.01, -5.70,
     +	-29.87, -8.00,-29.74,-10.40,-29.60,-12.80,-29.47,-15.20,
     +	-29.33,-17.30,-29.20,-19.00,-29.06,-20.20,-28.93,-20.80,
     +	-28.80,-20.50,-28.66,-19.40,-28.53,-17.60,-28.39,-15.20,
     +	-28.26,-12.70,-28.13,-10.20,-28.00, -7.50,-27.86, -4.90,
     +	-27.73, -2.60,-27.60, -0.10,-27.47,  2.40,-27.34,  4.90,
     +	-27.21,  7.50,-27.08, 10.20,-26.94, 13.00,-26.81, 15.90,
     +	-26.68, 19.00,-26.55, 22.70,-26.42, 27.00,-26.29, 31.30,
     +	-26.16, 34.70,-26.03, 37.00,-25.91, 38.80,-25.78, 40.10,
     +	-25.65, 40.90,-25.52, 41.00,-25.39, 40.50,-25.26, 40.00,
     +	-25.13, 39.70,-25.01, 39.70,-24.88, 39.80,-24.75, 40.20,
     +	-24.62, 40.60,-24.50, 41.00,-24.37, 41.50,-24.24, 42.10,
     +	-24.12, 42.80,-23.99, 43.50,-23.86, 44.00,-23.74, 44.30,
     +	-23.61, 44.20,-23.49, 43.50,-23.36, 42.40,-23.23, 40.80,
     +	-23.11, 39.00,-22.98, 37.10,-22.86, 35.20,-22.73, 33.40,
     +	-22.61, 31.50,-22.49, 29.60,-22.36, 27.50,-22.24, 25.40,
     +	-22.11, 23.20,-21.99, 20.80,-21.87, 18.50,-21.74, 16.50,
     +	-21.62, 14.60,-21.50, 12.70,-21.37, 10.60,-21.25,  8.50,
     +	-21.13,  6.40,-21.00,  4.40,-20.88,  2.30,-20.76,  0.10,
     +	-20.64, -2.50,-20.51, -5.50,-20.39, -8.60,-20.27,-11.80,
     +	-20.15,-14.90,-20.03,-17.90,-19.91,-20.80,-19.78,-23.40,
     +	-19.66,-25.80,-19.54,-27.90,-19.42,-29.80,-19.30,-31.70,
     +	-19.18,-33.50,-19.06,-35.40,-18.94,-37.40,-18.82,-39.60,
     +	-18.70,-42.10,-18.58,-44.40,-18.46,-46.70,-18.34,-48.90,
     +	-18.22,-51.00,-18.10,-52.80,-17.98,-54.20,-17.86,-55.40,
     +	-17.74,-56.00,-17.62,-56.40,-17.50,-56.60,-17.38,-56.50,
     +	-17.26,-56.00,-17.15,-55.30,-17.03,-54.60,-16.91,-53.60,
     +	-16.79,-52.50,-16.67,-51.30,-16.55,-50.10,-16.44,-48.90,
     +	-16.32,-47.90,-16.20,-47.00,-16.08,-45.90,-15.96,-44.60,
     +	-15.85,-43.10,-15.73,-41.30,-15.61,-39.20,-15.50,-36.90,
     +	-15.38,-34.30,-15.26,-31.30,-15.14,-28.20,-15.03,-25.30,
     +	-14.91,-22.80,-14.79,-20.30,-14.68,-17.60,-14.56,-14.70,
     +	-14.44,-12.00,-14.33, -9.70,-14.21, -7.40,-14.09, -5.40,
     +	-13.98, -3.30,-13.86, -1.30,-13.75,  0.50,-13.63,  2.30,
     +	-13.52,  4.00,-13.40,  5.70,-13.28,  7.30,-13.17,  8.90,
     +	-13.05, 10.60,-12.94, 12.20,-12.82, 13.70,-12.71, 15.10,
     +	-12.59, 15.90,-12.48, 16.40,-12.36, 16.60,-12.25, 16.70,
     +	-12.13, 16.80,-12.02, 16.90,-11.90, 17.10,-11.79, 17.20,
     +	-11.67, 17.50,-11.56, 17.90,-11.44, 18.30,-11.33, 18.90,
     +	-11.22, 19.40,-11.10, 20.00,-10.99, 20.80,-10.87, 21.90,
     +	-10.76, 23.30,-10.64, 24.70,-10.53, 26.20,-10.42, 27.70,
     +	-10.30, 29.20,-10.19, 30.70,-10.08, 32.20, -9.96, 33.70,
     +	 -9.85, 35.40, -9.74, 37.30, -9.62, 39.30, -9.51, 41.70,
     +	 -9.39, 44.30, -9.28, 47.00, -9.17, 49.60, -9.06, 52.40,
     +	 -8.94, 55.30, -8.83, 58.30, -8.72, 61.40, -8.60, 64.60,
     +	 -8.49, 67.90, -8.38, 71.30, -8.26, 75.10, -8.15, 78.70,
     +	 -8.04, 82.00, -7.93, 85.00, -7.81, 87.70, -7.70, 90.00,
     +	 -7.59, 92.00, -7.48, 93.70, -7.36, 95.40, -7.25, 97.30,
     +	 -7.14, 99.20, -7.03,101.10, -6.91,103.50, -6.80,106.10,
     +	 -6.69,108.80, -6.58,112.00, -6.46,116.30, -6.35,120.60,
     +	 -6.24,124.00, -6.13,126.40, -6.02,128.10, -5.90,128.40,
     +	 -5.79,127.60, -5.68,126.20, -5.57,124.10, -5.46,121.50,
     +	 -5.34,118.40, -5.23,115.30, -5.12,112.60, -5.01,110.20,
     +	 -4.90,108.10, -4.79,106.10, -4.67,104.50, -4.56,102.90,
     +	 -4.45,101.30, -4.34,100.10, -4.23, 99.10, -4.12, 97.60,
     +	 -4.00, 96.30, -3.89, 95.30, -3.78, 94.40, -3.67, 93.50,
     +	 -3.56, 92.90, -3.45, 92.40, -3.34, 91.90, -3.22, 91.30,
     +	 -3.11, 90.60, -3.00, 90.00, -2.89, 89.70, -2.78, 88.90,
     +	 -2.67, 87.80, -2.56, 86.70, -2.45, 86.00, -2.33, 85.50,
     +	 -2.22, 85.20, -2.11, 84.80, -2.00, 84.50, -1.89, 84.80,
     +	 -1.78, 85.60, -1.67, 86.50, -1.56, 86.90, -1.44, 86.30,
     +	 -1.33, 85.70, -1.22, 86.00, -1.11, 86.40, -1.00, 86.70,
     +	 -0.89, 86.80, -0.78, 86.60, -0.67, 86.20, -0.56, 86.00,
     +	 -0.44, 85.80, -0.33, 85.60, -0.22, 85.40, -0.11, 85.10,
     +	  0.00, 84.90,  0.11, 85.20,  0.22, 85.60,  0.33, 85.70,
     +	  0.44, 85.60,  0.56, 85.80,  0.67, 86.10,  0.78, 86.60,
     +	  0.89, 87.30,  1.00, 88.10,  1.11, 88.90,  1.22, 89.70,
     +	  1.33, 90.20,  1.44, 90.70,  1.56, 91.40,  1.67, 92.00,
     +	  1.78, 92.40,  1.89, 92.90,  2.00, 93.30,  2.11, 93.90,
     +	  2.22, 94.50,  2.33, 95.20,  2.45, 95.90,  2.56, 96.40,
     +	  2.67, 96.70,  2.78, 97.00,  2.89, 97.30,  3.00, 97.50,
     +	  3.11, 97.70,  3.22, 97.90,  3.34, 98.50,  3.45, 98.90,
     +	  3.56, 99.30,  3.67, 99.60,  3.78,100.00,  3.89,100.30,
     +	  4.00,100.60,  4.12,100.90,  4.23,101.20,  4.34,101.30,
     +	  4.45,101.40,  4.56,101.70,  4.67,101.90,  4.79,102.20,
     +	  4.90,102.30,  5.01,102.20,  5.12,102.20,  5.23,102.40,
     +	  5.34,102.60,  5.46,102.70,  5.57,102.70,  5.68,102.70,
     +	  5.79,102.80,  5.90,103.00,  6.02,103.20,  6.13,103.20,
     +	  6.24,103.10,  6.35,103.00,  6.46,102.80,  6.58,102.70,
     +	  6.69,102.50,  6.80,102.10,  6.91,101.50,  7.03,100.90,
     +	  7.14,100.20,  7.25, 99.20,  7.36, 98.00,  7.48, 96.80,
     +	  7.59, 95.10,  7.70, 92.80,  7.81, 90.40,  7.93, 87.80,
     +	  8.04, 85.00,  8.15, 82.10,  8.26, 79.30,  8.38, 76.70,
     +	  8.49, 74.50,  8.60, 72.30,  8.72, 69.90,  8.83, 67.50,
     +	  8.94, 65.20,  9.06, 63.00,  9.17, 60.90,  9.28, 59.10,
     +	  9.39, 57.30,  9.51, 55.80,  9.62, 54.40,  9.74, 53.30,
     +	  9.85, 52.40,  9.96, 51.60, 10.08, 50.80, 10.19, 49.90,
     +	 10.30, 49.10, 10.42, 48.30, 10.53, 47.50, 10.64, 46.70,
     +	 10.76, 45.80, 10.87, 44.80, 10.99, 43.70, 11.10, 42.50,
     +	 11.22, 41.30, 11.33, 40.00, 11.44, 38.80, 11.56, 37.50,
     +	 11.67, 36.00, 11.79, 34.30, 11.90, 32.40, 12.02, 30.30,
     +	 12.13, 27.90, 12.25, 25.30, 12.36, 22.50, 12.48, 19.60,
     +	 12.59, 16.80, 12.71, 14.20, 12.82, 11.80, 12.94,  9.60,
     +	 13.05,  7.70, 13.17,  5.90, 13.28,  4.20, 13.40,  2.60,
     +	 13.52,  1.20, 13.63, -0.10, 13.75, -1.40, 13.86, -2.80,
     +	 13.98, -4.20, 14.09, -5.70, 14.21, -7.30, 14.33, -9.10,
     +	 14.44,-11.00, 14.56,-13.20, 14.68,-15.60, 14.79,-17.90,
     +	 14.91,-19.80, 15.03,-21.40, 15.14,-22.80, 15.26,-23.80,
     +	 15.38,-24.30, 15.50,-24.20, 15.61,-23.90, 15.73,-23.20,
     +	 15.85,-22.20, 15.96,-21.00, 16.08,-19.40, 16.20,-17.50,
     +	 16.32,-15.30, 16.44,-12.90, 16.55,-10.60, 16.67, -8.10,
     +	 16.79, -5.60, 16.91, -3.00, 17.03, -0.40, 17.15,  2.10,
     +	 17.26,  4.70, 17.38,  7.60, 17.50, 10.90, 17.62, 14.60,
     +	 17.74, 19.00, 17.86, 23.90, 17.98, 29.00, 18.10, 34.50,
     +	 18.22, 40.60, 18.34, 46.90, 18.46, 53.20, 18.58, 59.60,
     +	 18.70, 66.00, 18.82, 72.20, 18.94, 78.00, 19.06, 83.50,
     +	 19.18, 89.30, 19.30, 95.70, 19.42,102.80, 19.54,110.10,
     +	 19.66,117.60, 19.78,125.30, 19.91,132.60, 20.03,139.10,
     +	 20.15,144.60, 20.27,149.20, 20.39,153.10, 20.51,156.80,
     +	 20.64,160.10, 20.76,162.30, 20.88,163.00, 21.00,161.80,
     +	 21.13,159.20, 21.25,155.30, 21.37,149.50, 21.50,140.10,
     +	 21.62,129.50, 21.74,120.50, 21.87,113.10, 21.99,106.20,
     +	 22.11, 99.90, 22.24, 94.20, 22.36, 89.00, 22.49, 84.00,
     +	 22.61, 79.20, 22.73, 74.80, 22.86, 70.50, 22.98, 66.50,
     +	 23.11, 63.10, 23.23, 60.00, 23.36, 57.10, 23.49, 54.60,
     +	 23.61, 52.10, 23.74, 49.50, 23.86, 46.80, 23.99, 44.10,
     +	 24.12, 41.20, 24.24, 38.30, 24.37, 35.50, 24.50, 32.90,
     +	 24.62, 30.50, 24.75, 28.20, 24.88, 26.20, 25.01, 24.40,
     +	 25.13, 23.00, 25.26, 21.70, 25.39, 20.40, 25.52, 18.90,
     +	 25.65, 17.20, 25.78, 15.00, 25.91, 12.20, 26.03,  9.00,
     +	 26.16,  5.10, 26.29,  0.80, 26.42, -3.30, 26.55, -7.10,
     +	 26.68,-10.50, 26.81,-13.50, 26.94,-16.50, 27.08,-19.40,
     +	 27.21,-21.90, 27.34,-24.40, 27.47,-26.80, 27.60,-29.00,
     +	 27.73,-30.70, 27.86,-31.70, 28.00,-32.00, 28.13,-31.80,
     +	 28.26,-31.20, 28.39,-30.30, 28.53,-29.00, 28.66,-27.30,
     +	 28.80,-24.90, 28.93,-22.10, 29.06,-19.00, 29.20,-15.70,
     +	 29.33,-11.90, 29.47, -7.50, 29.60, -2.50, 29.74,  2.90,
     +	 29.87,  8.00, 30.01, 12.40, 30.14, 16.00, 30.28, 18.90,
     +	 30.42, 21.40, 30.55, 23.60, 30.69, 25.80, 30.83, 27.80,
     +	 30.97, 29.40, 31.10, 30.90, 31.24, 32.10, 31.38, 33.10,
     +	 31.52, 34.00, 31.66, 34.50, 31.80, 34.50, 31.94, 34.30,
     +	 32.08, 33.80, 32.22, 33.10, 32.36, 32.20, 32.50, 31.00,
     +	 32.64, 29.30, 32.78, 26.80, 32.92, 23.90, 33.06, 21.10,
     +	 33.20, 18.40, 33.35, 15.60, 33.49, 12.40, 33.63,  9.20,
     +	 33.78,  6.10, 33.92,  3.20, 34.06,  0.40, 34.21, -2.20,
     +	 34.35, -4.50, 34.50, -6.70, 34.64, -8.60, 34.79,-10.40,
     +	 34.93,-12.00, 35.08,-13.40, 35.23,-14.30, 35.37,-14.80,
     +	 35.52,-14.80, 35.67,-14.60, 35.82,-14.10, 35.97,-13.60,
     +	 36.11,-12.90, 36.26,-11.90, 36.41,-10.70, 36.56, -9.50,
     +	 36.71, -8.00, 36.86, -6.40, 37.01, -4.60, 37.17, -2.80,
     +	 37.32, -0.80, 37.47,  1.30, 37.62,  3.30, 37.77,  5.30,
     +	 37.93,  7.60, 38.08, 10.30, 38.24, 13.50, 38.39, 16.60,
     +	 38.55, 19.10, 38.70, 20.90, 38.86, 21.80, 39.01, 21.30,
     +	 39.17, 19.80, 39.33, 17.60, 39.48, 14.70, 39.64, 11.50,
     +	 39.80,  8.70, 39.96,  6.50, 40.12,  4.80, 40.28,  3.40,
     +	 40.44,  2.00, 40.60,  0.70, 40.76, -0.20, 40.92, -1.10,
     +	 41.09, -1.40, 41.25, -1.40, 41.41, -1.00, 41.58, -0.40,
     +	 41.74,  0.20, 41.91,  1.00, 42.07,  2.10, 42.24,  3.30,
     +	 42.40,  4.60, 42.57,  6.10, 42.74,  8.00, 42.91, 10.60,
     +	 43.07, 13.50, 43.24, 16.70, 43.41, 19.90, 43.58, 22.90,
     +	 43.76, 25.50, 43.93, 27.60, 44.10, 28.50, 44.27, 27.80,
     +	 44.45, 25.40, 44.62, 21.60, 44.80, 17.20, 44.97, 12.70,
     +	 45.15,  8.60, 45.32,  5.20, 45.50,  2.30, 45.68, -0.30,
     +	 45.86, -2.60, 46.04, -4.50, 46.22, -6.20, 46.40, -8.00,
     +	 46.58, -9.50, 46.76,-10.40, 46.95,-11.00, 47.13,-11.90,
     +	 47.31,-12.50, 47.50,-12.60, 47.69,-12.50, 47.87,-11.90,
     +	 48.06,-11.00, 48.25,-10.30, 48.44, -9.80, 48.63, -9.70,
     +	 48.82, -9.60, 49.01, -9.10, 49.21, -8.20, 49.40, -7.40,
     +	 49.59, -6.50, 49.79, -5.50, 49.99, -4.40, 50.18, -3.30,
     +	 50.38, -2.20, 50.58, -1.10, 50.78,  0.40, 50.98,  2.10,
     +	 51.19,  3.60, 51.39,  5.40, 51.60,  7.60, 51.80,  9.70,
     +	 52.01, 11.30, 52.22, 12.20, 52.43, 13.00, 52.64, 14.10,
     +	 52.85, 13.90, 53.06, 13.50, 53.27, 12.50, 53.49, 10.30,
     +	 53.71,  7.70, 53.92,  4.90, 54.14,  3.40, 54.36,  2.30,
     +	 54.58,  0.80, 54.81, -0.90, 55.03, -3.00, 55.26, -4.80/

      nz = 807

      do i=1,807
         zvp(1,i) = jzvp(1,i)
         zvp(2,i) = jzvp(2,i)
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create const.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      common/const/pi,dtr,rtd,deltat
      real*8 pi,dtr,rtd,deltat

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cpic.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      common/cpic/project_id,camera_id,frame_id,planet_id
      common/cpic/target_id,idate,itime
      integer project_id,camera_id,frame_id,planet_id
      integer target_id, idate, itime

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cpts.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      common/cpts/npts,lpt(2,500),rpt(2,500)
      common/cpts/lpt_os(2,500),rpt_os(2,500),lpt0(2,500)
      common/cpts/zl(500),zr(500),cmax(500),u(500),v(500)
      integer npts
      real*4 lpt,rpt,lpt_os,rpt_os,lpt0,zl,zr,cmax,u,v

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dev.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      common/dev/idev,vid,g,tb,nld,nsd,izoom,v1,v2,v3,izoom1,izoom2
      common/dev/ilow(2), ihigh(2)
      common/dev/zoom,zoom1,zoom2
      common/dev/stbl(256,2)
      integer idev,vid,g,tb,nld,nsd,izoom,v1,v2,v3
      integer izoom1, izoom2, ilow, ihigh
      logical*1 stbl
      real*4 zoom,zoom1,zoom2

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cmap.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      integer model, igeo
      real*8 rot, re, rp, epsln, fl, oal, oas, pscale, zscale
      real*8 om(3,3), psc3(3), sclat, sclon, epsln2
      real*8 cm(3,3), me(3,3), psc(3), rsc, angln, angla, anglb
      common/cmap/model,igeo,rot,re,rp,epsln
      common/cmap/fl,oal,oas,pscale,zscale
      common/cmap/om,psc3,sclat,sclon,epsln2
      common/cmap/cm,me,psc,rsc,angln,angla,anglb

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xdgclr.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
      common/xdgclr/TRANSPARENT,RED,CYAN
      byte TRANSPARENT,RED,CYAN
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create nav2.pdf
PROCESS HELP=*
SUBCMD-DEFAULT MAIN
  PARM INP        STRING      COUNT=2:3
  PARM OUT        STRING      COUNT=0:1                      DEFAULT=--
  PARM RES        STRING      COUNT=0:1                      DEFAULT=--
  PARM TARGET     (STRING,12) COUNT=0:1                      DEFAULT=--
  PARM SPICEMODE  KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
  PARM CKNAME     (STRING,4)  COUNT=1 +
	           VALID=(FARE,NEAR,NAV,NAV2,DAVI,NAIF)	     DEFAULT=DAVI
  PARM CKID       (STRING,4)  COUNT=1                        DEFAULT=NONE
  PARM USERID     (STRING,3)  COUNT=0:1                      DEFAULT=--
  PARM GROUPID    (STRING,3)  COUNT=0:1                      DEFAULT=--
  PARM INSTITUTE  (STRING,4)  COUNT=1                        DEFAULT=NONE
  PARM CDATE      (STRING,12) COUNT=1                DEFAULT=000000000000
  PARM REQNUM     (STRING,4)  COUNT=1                        DEFAULT=NONE
  PARM PURPOSE    (STRING,4)  COUNT=1                        DEFAULT=NONE
  PARM PROGRAM    (STRING,6)  COUNT=1                        DEFAULT=*NONE*
  PARM SPKID      (STRING,4)  COUNT=1                        DEFAULT=NONE
  PARM DEBUG      KEYWORD     COUNT=0:1  VALID=DEBUG         DEFAULT=--
END-SUBCMD
SUBCMD NAV2
  PARM EXIT	 TYPE=KEYWORD    COUNT=0:1  VALID=EXIT     DEFAULT=--
  PARM CZOOM     TYPE=INTEGER    COUNT=0:1  VALID=(-4:4)   DEFAULT=--
  PARM H         TYPE=KEYWORD    COUNT=0:1  VALID=H	   DEFAULT=--
  PARM HIST	 TYPE=KEYWORD    COUNT=0:1  VALID=HIST     DEFAULT=--
  PARM SPIKES	 TYPE=INTEGER	 COUNT=0:1  VALID=(0:250)  DEFAULT=--
  PARM STRETCH   TYPE=INTEGER    COUNT=0:2  	           DEFAULT=--
  PARM STR1      TYPE=INTEGER    COUNT=0:2  	           DEFAULT=--
  PARM STR2      TYPE=INTEGER    COUNT=0:2  	           DEFAULT=--
  PARM GERASE    TYPE=KEYWORD    COUNT=0:1  VALID=GERASE   DEFAULT=--
  PARM SPTS	 TYPE=KEYWORD    COUNT=0:1  VALID=SPTS     DEFAULT=--
  PARM GET	 TYPE=INTEGER    COUNT=0:1  VALID=(1:500)  DEFAULT=--
  PARM DELETE    TYPE=INTEGER    COUNT=0:1  VALID=(1:500)  DEFAULT=--
  PARM RESTORE   TYPE=INTEGER    COUNT=0:1  VALID=(1:500)  DEFAULT=--
  PARM FIT	 TYPE=INTEGER    COUNT=0:1  VALID=(2:3)    DEFAULT=--
  PARM NAVPTS	 TYPE=KEYWORD    COUNT=0:1  VALID=NAVPTS   DEFAULT=--
  PARM NAVVEL	 TYPE=KEYWORD    COUNT=0:1  VALID=NAVVEL   DEFAULT=--
  PARM EDIT	 TYPE=KEYWORD    COUNT=0:1  VALID=EDIT     DEFAULT=--
  PARM PARAMS    TYPE=KEYWORD    COUNT=0:1  VALID=PARAMS   DEFAULT=--
  PARM (SL,SL1,SL2) TYPE=INTEGER COUNT=0:1		   DEFAULT=--
  PARM (SS,SS1,SS2) TYPE=INTEGER COUNT=0:1		   DEFAULT=--
  PARM (U,U1,U2) TYPE=INTEGER    COUNT=0:1		   DEFAULT=--
  PARM (D,D1,D2) TYPE=INTEGER    COUNT=0:1		   DEFAULT=--
  PARM (L,L1,L2) TYPE=INTEGER    COUNT=0:1		   DEFAULT=--
  PARM (R,R1,R2) TYPE=INTEGER    COUNT=0:1		   DEFAULT=--
  PARM ZOOM      TYPE=INTEGER    COUNT=0:1  VALID=(-4:4)   DEFAULT=--
  PARM HELP	 TYPE=KEYWORD    COUNT=0:1  VALID=HELP     DEFAULT=--
END-SUBCMD
SUBCMD PARAMS
  PARM EXIT	 TYPE=KEYWORD    COUNT=0:1  VALID=EXIT     DEFAULT=--
  PARM CORR	 TYPE=KEYWORD    COUNT=0:1  VALID=CORR     DEFAULT=--
  PARM PROJECT	 TYPE=KEYWORD    COUNT=0:1  VALID=PROJECT  DEFAULT=--
  PARM HPF	 TYPE=KEYWORD    COUNT=0:1  VALID=HPF      DEFAULT=--
  PARM PHASE	 TYPE=KEYWORD    COUNT=0:1  VALID=PHASE    DEFAULT=--
  PARM INTERP	 TYPE=KEYWORD    COUNT=0:1  VALID=INTERP   DEFAULT=--
  PARM NLW	 TYPE=INTEGER    COUNT=0:1  VALID=(8:128)  DEFAULT=--
  PARM NSW	 TYPE=INTEGER    COUNT=0:1  VALID=(8:128)  DEFAULT=--
  PARM MINCORR	 TYPE=REAL       COUNT=0:1  VALID=(0.:1.0) DEFAULT=--
  PARM ZWIND	 TYPE=INTEGER    COUNT=0:1  VALID=(0:10)   DEFAULT=--
  PARM NOCORR	 TYPE=KEYWORD    COUNT=0:1  VALID=NOCORR   DEFAULT=--
  PARM NOPROJEC  TYPE=KEYWORD    COUNT=0:1  VALID=NOPROJEC DEFAULT=--
  PARM NOHPF	 TYPE=KEYWORD    COUNT=0:1  VALID=NOHPF    DEFAULT=--
  PARM NOPHASE	 TYPE=KEYWORD    COUNT=0:1  VALID=NOPHASE  DEFAULT=--
  PARM NOINTERP	 TYPE=KEYWORD    COUNT=0:1  VALID=NOINTERP DEFAULT=--
  PARM STATUS 	 TYPE=KEYWORD    COUNT=0:1  VALID=STATUS   DEFAULT=--
END-SUBCMD
SUBCMD EDIT
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM GEODET	TYPE=KEYWORD    COUNT=0:1  VALID=GEODET DEFAULT=--
  PARM GEOCEN	TYPE=KEYWORD    COUNT=0:1  VALID=GEOCEN DEFAULT=--
  PARM PR       TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM ER       TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM CAMERA	TYPE=INTEGER	COUNT=0:1 		DEFAULT=--
  PARM FL       TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM OAXIS    TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM SC       TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM SSP      TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM PC       TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM ISPC     TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM WAPC     TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM WAISPC   TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM ANGLN    TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM RANGE    TYPE=REAL       COUNT=0:1 		DEFAULT=--
  PARM TARGET   TYPE=(STRING,12) COUNT=0:1 		DEFAULT=--
  PARM STATUS   TYPE=KEYWORD    COUNT=0:1 VALID=STATUS  DEFAULT=--
  PARM SAVE     TYPE=KEYWORD    COUNT=0:1 VALID=SAVE    DEFAULT=--
  PARM RESTORE  TYPE=KEYWORD    COUNT=0:1 VALID=RESTORE DEFAULT=--
  PARM GETSEDR  TYPE=KEYWORD    COUNT=0:1 VALID=GETSEDR DEFAULT=--
  PARM CKNAME   TYPE=KEYWORD    COUNT=0:1 +
	   VALID=(FARE,NEAR,NAV,NAV2,DAVI,NAIF,SEDR)    DEFAULT=--
END-SUBCMD
SUBCMD RVALUE
  PARM VALUE    TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD IVALUE
  PARM VALUE    TYPE=INTEGER    COUNT=0:1  	        DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD STRING
  PARM STRNG	TYPE=(STRING,60) COUNT=0:1		DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD PIC
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM LEFT	TYPE=KEYWORD    COUNT=0:1  VALID=LEFT   DEFAULT=--
  PARM RIGHT	TYPE=KEYWORD    COUNT=0:1  VALID=RIGHT  DEFAULT=--
END-SUBCMD
SUBCMD READY
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD QUERY
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM Y        TYPE=KEYWORD    COUNT=0:1  VALID=Y      DEFAULT=--
  PARM N        TYPE=KEYWORD    COUNT=0:1  VALID=N      DEFAULT=--
END-SUBCMD
SUBCMD VELOCITY
  PARM VEL      TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD DISTOR
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM IMAGE	TYPE=KEYWORD    COUNT=0:1  VALID=IMAGE  DEFAULT=--
  PARM OBJECT	TYPE=KEYWORD    COUNT=0:1  VALID=OBJECT DEFAULT=--
END-SUBCMD
END-PROC
.TITLE
VICAR Image Navigation Program NAV2
.HELP
PURPOSE

NAV2 is an interactive program which improves the camera pointing of an image
by registering it to a reference image whose camera pointing is accurately
known.  NAV2 is currently specific to Voyager, Galileo, and Cassini flight
images.  

The image to be navigated (primary image) and the reference image must have
overlapping areas in which common features (tiepoints) can be identified.
These tiepoints are used in combination with image projection data retrieved
from the MIPS SPICE server to correct the camera pointing angles (C-matrix) of
the primary image.

In cases where the uncertainties in camera pointing for the primary and
reference frames are equivalent, both images may be navigated simultaneously.
The C-matrices of both frames are modified, with the geometric errors being
split evenly between the two frames.

The algorithms implemented in NAV2 are based on notes provided by
Andy Ingersoll, 3-19-85.  See also VICAR programs NAV, FARENC and MANMATCH.

.page
EXECUTION:

NAV2 is an interactive program and requires the allocation of a display device
with an image plane, graphics plane, and cursor (e.g. trackball, mouse):

      USE  EPA2		!allocate a display device
      NAV2  INP=(PIC,REF)  TARGET=targetname
   or NAV2  INP=(PIC,REF)  OUT=NEWPTS  TARGET=targetname
   or NAV2  INP=(PIC,REF,OLDPTS)  OUT=NEWPTS  TARGET=targetname
where
    PIC is the image to be navigated (primary frame),
    REF is a reference image whose camera pointing is accurately known.
    OLDPTS and NEWPTS are optional input and output tiepoint files.
    TARGET is a required parameter for Voyager.

PIC and REF must be Galileo, Voyager, or Cassini images in byte format.
The input images may be raw images containing geometric camera distortions
or images which have been geometrically corrected.

If NEWPTS is included, NAV2 will output all tiepoints to this file.
Tiepoints saved from a previous session can be input as an optional
third input file (0LDPTS).  OLDPTS and NEWPTS are in IBIS graphics file format.

.page
OPERATION:

NAV2 uses an east-longitude system.  For Voyager, latitudes are input
and output to the user as planetographic.  Planetocentric latitudes are
used for all other missions.  An oblate spheroid target-body model is used.

Initially, the input frames are displayed on a split screen, with the primary
frame on the left and the reference frame on the right (this is the same
convention used by the program PICREG).  The images are displayed at reduced
resolution to display the complete field of view.  If tiepoints are input from
a previous session, these are displayed in graphics (see OLDPTS and NEWPTS in
program execution statement above).

The MIPS SPICE server is accessed to retrieve the image projection geometry
for both the left and right images.  If the input image(s) contain geometric
distortions, the program will attempt to account for this.  Special provisions
must be made for dealing with vidicon camera distortions (e.g. Voyager).  See
the following sections below for details:

	GEOMETRIC CAMERA DISTORTIONS
	ACCESSING SPICE DATA

The primary frame is navigated via the following steps:

   1) Using the cursor and split-screen display, the user identifies one or
      more common features (tiepoints) in both images.

   2) The FIT command is used to recompute the C-matrix so as to minimize
      displacement errors (in pixels) between the left and right images.
      The displacement error for a tiepoint is zero if both left and right
      pixel locations project to the same latitude-longitude coordinates.
      The RMS error, the tiepoint generating the maximum error, and the number
      of points used in the fit are reported.

   3) The user may iteratively delete bad points and redo the fit,
      or go back to acquire more tiepoints.

   4) When the user exits from the program, a final fit is performed.
      The (line,sample) position of the planet center and the north
      angle (ANGLN) are computed and reported.  Finally, the corrected
      camera pointing can be optionally stored in a C-kernel.

All tiepoint selection is peformed interactively, with the user responding to
prompts from the program.  See NAV2 COMMAND PROCESSOR below.

.page
ADJUSTING FOR ATMOSPHERIC FEATURE MOTION:

If the target-body is a planet with an atmosphere, NAV2 will automatically
prompt the user for intructions for dealing with zonal and meridional wind
currents:

	Do you wish to adjust for wind speed?
	Enter Y or N >

If the user responds with 'N', then all tiepoints are assumed to be stationary.
If the user responds with 'Y', the next prompt is issued:

	Do you wish to split the pointing error between the two frames?
	Enter Y or N >

Entering 'Y' will cause both primary and reference frames to be navigated
simultaneously.  The camera pointing angles of both frames are adjusted by
moving each optic axis an equal distance (i.e. it is assumed that the
uncertainties in camera pointing are equivalent for the two frames).

If the target-body is Jupiter, the following prompt is issued:

	Do you wish to use the zonal velocity profile?
	Enter Y or N >

Entering 'Y' will cause the zonal velocity of each tiepoint to be computed
as a function of its latitude using a zonal velocity profile.  The profile is
based on Voyager data.  The meridional velocities are assumed to be zero.

The profile is a table consisting of (latitude, zonal-velocity) pairs, where
the latitudes are stored in monotonically increasing order but at variable
angular spacing.  For latitudes between table values, linear interpolation is
used to compute the zonal velocity.  The table contains data between latitudes
-55.26 and +55.26.  For latitudes outside the range of the table, the zonal
velocities are assumed to be zero.

The SCET is used to determine the time separation between frames. This is used
to convert zonal velocities into pixel displacements (see SCET parameter).

At this point, control is passed to the NAV2 command processor.

.page
NAV2 COMMAND PROCESSOR:

Upon entry, the user will be prompted to input commands.

The user-prompts in NAV2 are nested, so that certain commands will
invoke program modules which issue lower level prompts, and so forth.
All prompts (ideally) will accept the 'EXIT command, which causes
an exit to the next higher level prompt.  In many cases, entering
'HELP will display a list of all commands available at the current prompt.

The controlling program is identified by the prompt:

		NAV2>

Entering 'HELP will result in:

	ENTER EXIT, CZOOM, H, HIST, SPIKES, STRETCH, STR1, STR2,
        GERASE, SPTS, GET, DELETE, FIT, NAVPTS, NAVVEL, EDIT, PARAMS,
        SL, SL1, SL2, SS, SS1, SS2, ZOOM, U, U1, U2, D, D1, D2, L, L1,
        L2, R, R1, R2, HELP>

If the user responds by typing carriage return (without entering any
characters at all) the program will acquire a tiepoint.  See Tiepoint
Acquisition Cycle below.

The CZOOM command is used to center the display about a feature and
change the image resolution of the display.  Entering CZOOM=1 will
cause the split screen to be re-displayed at normal resolution.  The
user will be prompted to center the left and right screens via the
trackball.  If previously acquired features are visible, they are
re-displayed in graphics.

Entering 'H (for Home) will cause the original full-field-of-view
display to appear.

Entering STRETCH=(5,220) will cause a hardware stretch (linear) of
the displayed images.

The STR1 and STR2 commands may be used to stretch the left and right
image areas independently.  Entering STR1=(10,128) will cause the left
image area to be stretched as it is being written to the image-display memory
plane.  Note that these commands are independent of the hardware stretch,
so that following these commands with STRETCH=(5,220) will result in further
stretching of the stretched images.

The 'HIST command displays the histograms of the image areas for both left and
right pictures.  The histograms are displayed in graphics, using a logarithmic
scale in the frequency axis.  The histogram display may be modified by entering
SPIKES=5, which will cause the five highest frequencies to be scaled to the
maximum height of the frequency axis.

Entering 'GERASE will erase the graphics plane.

Entering 'SPTS will re-display the acquired tiepoints.

Entering GET=15 will move the cursor to feature 15 on the left
screen.  If the feature is not visible, the display window will be
moved so that it is.  The (line,sample) and (lat,lon) coordinates
of the feature are reported.

Entering DELETE=15 will delete feature 15.  If you later change
your mind, entering RESTORE=15 will restore the feature.

Entering FIT=2 will cause the camera angular components ANGLA and
ANGLB of the left image to be recomputed, resulting in an updated
transformation from (line,sample) to (lat,lon).  The angles are
recomputed by performing a least squares fit so that points in the
left image occur at the same (lat,lon) coordinates as corresponding
points in the right image.

Entering FIT=3 will cause the camera angular components ANGLA, ANGLB,
and ANGLN (north angle) of the left image to be recomputed.

Entering NAVVEL will cause NAV2 to adjust for wind speed when fitting
the tiepoints.  The user is prompted to specify processing modes for wind
adjustment as before.  This command is valid for atmospheric planets only.

Entering NAVPTS cancels any adjustments made for wind speed.

Entering 'EDIT will enable the user to edit the navigation data.
See EDIT COMMAND below.

Entering 'PARAMS will enable the user to edit the area correlation
parameters.  See PARAMS COMMAND below.

Entering SL=250 SS=80 will move the display windows of both left and
right image so that (line,samp)=(250,80) appears in the upper left
corner.

Entering ZOOM=2 will magnify the displayed area to twice normal resolution
(via pixel replication).

The commands U, D, L, R are for moving the displayed areas up, down,
left, or right.  Entering U=10 will move the displayed areas up 10 pixels.

The left image may be moved independently using SL1, SS1, U1, D1, etc.
Similarly, the right image may be moved using SL2, SS2, U2, D2, etc.

.PAGE
EDIT COMMAND

Typing 'EDIT will permit the user to edit the navigation data.

	Enter LEFT or RIGHT>

Typing 'RIGHT will permit the navigation data for the right image to be
edited.  The EDIT command processor is identified by the prompt:

	EDIT>

The EDIT command processor enables the user to modify values which
effect camera pointing.  These include planet constants, camera
constants, and SEDR data.  Under normal circumstances, the user
will never have to edit the camera pointing data since these are
automatically retrieved from the SEDR file and frame label.

Typing HELP will cause of all the available commands to be listed:

	Enter EXIT,HELP,GEODET,GEOCEN,PR,ER,CAMERA,FL,OAXIS,
	SC,SSP,PC,ANGLN,RANGE,TARGET,WAPC,STATUS,
        SAVE,RESTORE,GETSEDR,CKNAME>

Typing 'EXIT will return control back to the NAV2 command processor.

Typing 'GEODET will cause all latitudes input by the user to be treated as
planetographic, and to report all latitudes to the user as planetographic.
Typing 'GEOCEN will cause all input and output latitudes to be treated as
planetocentric.  Note that the default is GEODET for Voyager and GEOCEN for
all other projects.


Typing PR=54450 ER=60330 will change the values of the planet's
polar and equatorial radii to 54450 and 60330 km respectively.

Typing CAMERA=7 will change the camera-ID to VGR-1 NA (provided the image is
from Voyager).  The focal length, picture scale, and optic axis intercept for
this camera will be automatically set for this camera.

Typing  FL=1200.175 will change the camera focal length to
1200.175 mm.

Typing OAXIS=(400,300) will place the optical axis intercept
at (line,sample)=(400,300).

Typing SC=80 will change the picture scale to 80 pixels
per mm on the focal plane.

Typing SSP=(60,240.3) will change the spacecraft position
to (SCLAT,SCLON)=(60,240.3).

Typing PC=(445.3,558.4) will move the planet center to
(SCLINE,SCSAMP)=(445.3,558.4).  This will cause the camera pointing
to be updated.

Typing ANGLN=190.9 will cause the orientation of the projected
spin axis to be changed to 190.9 degrees, measured clockwise
from right (the positive sample direction).

Typing RANGE=4794909 will cause the distance from spacecraft to
planet center to be changed to 4794909 km.

Typing TARGET=SATURN will change the target-ID to Saturn and
automatically changes the polar and equatorial radii to Saturn's
nominal values.

Typing WAPC=(455.1,376.2) changes the (line,sample) coordinates
of the planet center in the wide-angle frame.  The (line,sample)
coordinates are converted to the narrow-angle field of view and
the result used as the (line,sample) coordinates for the NA planet
center.  The camera pointing is then updated.  Here it is assumed
that the frame being processed is the NA from a WA-NA simultaneous
exposure.

Typing 'STATUS will cause a summary of the navigation data to be
printed.

Typing 'SAVE will store the current navigation data in the array
SNAV.  Typing 'RESTORE will replace the navigation data by
whatever is in SNAV.  Typing 'GETSEDR will replace the navigation
data with the nominal pointing.

Typing CKNAME=FARE specifies that the desired source of C-matrix is FARENC.
Valid source names are:

	DAVI	--Data determined by Mert Davies
	NAV     --Data determined by program NAV
	FARE    --Data determined by program FARENC
	NAV2    --Data determined by program NAV2
	NEAR    --Data determined by program MANMATCH
        NAIF    --Data determined by NAIF

The default is to select the C-matrix in the above order of priority.

.PAGE
PARAMS COMMAND

Typing 'PARAMS will enable the user to modify the area correlation
parameters:

	Enter EXIT, CORR, PROJECT, HPF, PHASE, INTERP, NLW, NSW,
        ZWIND, NOCORR, NOPROJEC, NOHPF, NOPHASE, NOINTERP, STATUS

Typing 'EXIT will return control to the NAV2 command processor.

Entering 'CORR will enable the area correlation algorithm.  The algorithm
consists of multiplying the Fourier transform of the right area by the
complex conjugate of the transform of the left area, computing the inverse
transform and locating the maximum.

Entering 'PROJECT will cause the left area to be projected to the same
perspective as the right area, prior to correlation.  This feature
corrects for large perspective differences between frames.

Entering 'HPF will cause the axes of the transforms to be set to zero.
This is equivalent to applying a high-pass filter to the areas prior
to correlation.

Entering 'PHASE will cause the correlation to be performed using phase
information only.

Entering 'INTERP will cause the correlation maxima to be located to
sub-pixel accuracy by locating the centroid of the correlation peak.

Entering NLW=64 NSW=80 specifies the dimensions of the correlation
areas to be 64x80 pixels.  NLW and NSW must be even integers.

Entering ZWIND=10 will cause a 10x10 window to be used for computing the
average DN value centered on each tiepoint.  This value is stored in the
output tiepoint file (see below).

The commands 'NOCORR, 'NOPROJEC, 'NHPF, 'NOPHASE, and 'NOINTERP disable
the corresponding functions.

Entering 'STATUS displays the current parameter settings.

The default values are 'CORR, 'NOPROJEC, 'HPF, 'NOPHASE, 'INTERP,
NLW=64 and NSW=64.

.PAGE
TIEPOINT ACQUISITION CYCLE

Tiepoints are selected by means of the split screen display.  When a new
tiepoint is to be acquired, its initial location is determined by positioning
the cursor over the feature in the left image.  The user enters a carriage
return (CR), thus sending a blank string to the command processor.  This
signals the command processor to begin a tiepoint acquisition cycle.

The coordinates of the cursor position on the left image are read and
converted into (line,sample) image coordinates.  The (lat,lon) coordinates
of the selected feature are computed, based upon current camera pointing
information, and the results reported:

	FN= 25  (L1,S1)=( 345.23, 673.31)  (LAT1,LON1)=( 22.32,245.32)

If the user has elected to compensate for atmospheric motion, the following
prompt is issued:

	Enter velocity components (U,V) >

The zonal and meridional velocities should be entered in meters/sec.  If
the Jupiter zonal velocity profile has been selected, U is automatically
computed and the prompt is not issued.

A rough estimate of the feature's location in the right image is obtained by
computing the location of the corresponding (lat,lon) coordinates in the
right image, compensating for any feature motion.  The cursor is automatically
moved to this location in the right screen and the following prompt is issued:

	Position RIGHT cursor>

The user may then position the right cursor to center it over the feature.
When the user is satisfied that the cursor has been properly centered,
he responds by entering (CR).  The program then performs an area correlation
to refine the position of the right feature (See PARAMS COMMAND to modify
the correlation parameters).  The resulting position is then reported:

	        (L2,S2)=( 673.34, 124.32)  (LAT2,LON2)=( 25.23,247.31)

The tiepoint is then stored and the program returns with the NAV> prompt.

The tiepoint acquisition cycle may be aborted at any time by entering
'EXIT.  If, for example, the selected feature is not visible in the right
screen, the user should exit the cycle to re-position the right window.
If a tiepoint is accidentally acquired, it may be deleted via the
DELETE command.

Initially, the program may fail badly in predicting the position of the
feature in the right image.  This is because the nominal pointing may
be very inaccurate.  The program periodically updates the camera pointing
of the left image, based upon the tiepoints acquired up to that point.

.PAGE
GENERAL COMMENTS

The general rules regarding TAE parameters apply:

Commands may be abbreviated, as long as enough letters are typed
to insure uniqueness.

When a command is not followed by a value, it must be preceded
by an apostrophe, e.g. 'EDIT.  The exception is that if the first
keyword in a list is entered, the apostrophe may be ommitted,
e.g:
	Enter LEFT or RIGHT> L

If you need help or want to get out of a routine, try typing
HELP or EXIT.  Sometimes it works, sometimes it doesn't....

.page
GEOMETRIC CAMERA DISTORTIONS:

NAV2 assumes that the input images (primary and reference) have been acquired
via a camera system with a shutter and focal plane (e.g. vidicon/CCD sensor),
so that the images are perspective projections of the target body.  The input
images may be raw images containing geometric camera distortions (image-space
frames), or images which have been geometrically corrected (object-space
frames).

NAV2 corrects for Galileo or Cassini camera distortions using a built-in radial
distortion model.

If a Voyager image-space frame is used, then the reseau locations must first
be located and stored into a reseau location file (see RES parameter):

  	RESLOC  (PIC,RFILE)
	RESLOC  (REF,RFILE)
	NAV2  (PIC,REF)  OUT=TPT  RES=RFILE

In this case, both PIC and REF are image-space frames (It is possible for one
or both images to be object-space).  The reseaux are located using RESLOC
and stored in the Reseau Location File.   NAV2 then retrieves the reseau
locations from the file (see RES parameter).

If the RES parameter is not specified, the program automatically accesses the
file for the planet-of-encounter as follows:

		VGR:[CCA314.VGR]RESJ.FIL	(Jupiter)
		VGR:[CCA314.VGR]RESS.FIL	(Saturn)
		VGR:[CCA314.VGR]RESU.FIL	(Uranus)
		VGR:[CCA314.VGR]RESN.FIL	(Neptune)

Note that the RES parameter is not required for object-space frames.

.page
ACCESSING SPICE DATA

NAV2 will retrieve the projection geometry for both images by accessing the
MIPS SPICE server.  Data is retrieved from the server by target name,
spacecraft ID, camera ID, and Spacecraft-Event-Time.  These image identifiers
are usually determined by scanning the image label.  Note, however, that the
Voyager flight label does not include the target name.  Therefore, the target
parameter must be included when running NAV2 on Voyager images.

The following parameters may also be used to control the access of SPICE data:

  SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
  or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
  of the environmental variable DEFAULTSPICE.

  CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
  example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
  When specified, the CKID parameter overrides the CKNAME parameter.  If the
  camera pointing data is not found in the requested C kernel, the other C 
  kernels are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific 
instance of camera pointing based upon this provenance information:

  CDATE specifies the date and time the camera pointing was created.
  REQNUM identifies the request number associated with the camera pointing.
  PURPOSE identifies the purpose for creating the camera pointing.
  PROGRAM identifies the program which created the camera pointing.
  SPKID identifies the SP-kernel used to create the camera pointing.
  USERID identifies the user who created the camera pointing.
  GROUPID identifies the group which created the camera pointing.
  INSTITUTE identifies the facility which created the camera pointing.

  A complete list of CK and SPK IDs are located in the ASCII file assigned the
  logical name (or environmental variable) KERNELDB.

.page
TIEPOINT FILE FORMAT

The tiepoints are written to an IBIS interface (tabular) file with one
row for each tiepoint.  The following column format is used:

     Column       Description			   Format

        1     First input matching line            real*4
        2     First input matching sample          real*4
        3     Second input line                    real*4
        4     Second input sample                  real*4
	5     First input estimated line           real*4
	6     First input estimated sample         real*4
        7     First input Z value                  real*4
        8     Second input Z value                 real*4
        9     Correlation value                    real*4
       10     Sequence number                      real*4

The estimated (line,sample) coordinates of the left image are computed
using the camera pointing information retrieved from the SEDR (prior to
any updates by the program).  The Z values are obtained by averaging
the DN values over an area centered on the tiepoint (see ZWIND and
ZREJECT parameters).  The correlation value is a number between -1 and
+1, where +1 represents perfect positive correlation and -1 represents
perfect negative correlation.  The sequence number is 1 for the first
tiepoint, 2 for the second, etc.

.page
HISTORY

Original Programmer: Gary Yagi, 1 November, 1986
Current Cognizant Programmer: Gary Yagi
Revisions:
 1988-09-23 GMY * Modify for new VRDI interface, multiple versions of camera
		  pointing in SEDR.
		* Implement 1024x1024 display capability.
		* Add code to support systems with only one video plane.
		* Implement capability to process image-space frames.
		* Change tiepoint file format to IBIS interface file.
		* Add independent stretches for left and right images.
		* Implement compensation for atmospheric feature motion.
		* Add Jupiter zonal-velocity profile.
		* Fix FRs 35652 and 37254.
		* Numerous other modifications and minor bug fixes.
 1990-07-28 GMY * Galileo conversion
 1991-08-21 GMY * Fix FRs 66566, 61146.
 1997-05-07 SMC Port to Unix and Alpha
                 * Switch the image memory plane (IMP) of image and tiepoint,
                   so the image can be properly displayed on 8-bit device.
                 * Upgrade all CONVISOS calls to take camera_id parameter.
                 * Replace all implicit declaration with explicit ones.
                 * distinguish between REAL*8 and REAL*4, the old VMS was able
                   to operate between the two types interchangablly, but the
                   new OS has problems with it.
                 * Upgraded to GETSPICE2 and PUTSPICE2
                 * Convert the S/C ID and Planet ID of the VGR SPICE/SEDR
                   buffer to that of the GLL's so that PBNAME and PUTSPICE2
                   can operate and terminate correctly
                 * Replaced the xdglconstant call with xdglinit so that the
                   graphic plane can be displayed accross all kinds of devices
                   and that colors can be used.
                 * Modified maplabel.f/zonal routine to use Binary Searching
                   algorithm to find the proper record.  The previous routine
                   had a problem of going into infinite loop.
 2001-10-10 GMY Fix Linux compile errors (NAV2_LINUX)
                Modify to handle Cassini and VGR image space.
		Add Cassini and VGR image space test cases to unit test.
 2016-02-18 WLB Added enable/disable-log to tstnav2.pdf
.LEVEL1
.VARI INP
STRING - File names for
input and reference images,
and optional input tiepoint
file.
.VARI OUT
STRING - File name for
ouput tiepoint file.
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI CKNAME
Optional 4-char string
C-kernel name
.VARI CKID
Optional 4-char string
C-kernel ID
.VARI USERID
Optional 3-char string
User who created camera pointing
.VARI GROUPID
Optional 3-char string
Group which created camera pointing
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI PROGRAM
Optional 6-char string
Program which created camera pointing
.VARI SPKID
Optional 4-char string
SP kernel for created camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created
.VARI TARGET
STRING - Target body name
.VARI DBUG
KEYWORD - Output diagnostic
information for program
maintenance.
.VARI CZOOM
INTEGER - Center display and zoom
.VARI H
KEYWORD - Display entire image
.VARI HIST
KEYWORD - Histogram of displayed image area
.VARI SPIKES
INTEGER - Number of spikes in histogram.
.VARI STRETCH
INTEGER - Stretch the displayed image
.VARI STR1
INTEGER - Stretch the left display
.VARI STR2
INTEGER - Stretch the right display
.VARI GERASE
Erase graphics plane
.VARI SPTS
KEYWORD - Display tiepoints
.VARI GET
INTEGER - print tiepoint information
.VARI DELETE
INTEGER - Delete tiepoint
.VARI FIT
INTEGER - Fit tiepoints and
compute new OM and C matrices.
.VARI NAVVEL
KEYWORD - Adjust for atmospheric
feature motion.
.VARI NAVPTS
KEYWORD - Cancels adjustment for
atmospheric motion.
.VARI SL
INTEGER - Set starting line
of display window.
.VARI SS
INTEGER - Set starting sample
of display window.
.VARI U
INTEGER - Raise displayed area
.VARI D
INTEGER - Lower displayed area
.VARI L
INTEGER - Move displayed area left
.VARI R
INTEGER - Move displayed area right
.VARI SL1
INTEGER - Set starting line
of display window, left image only
.VARI SS1
INTEGER - Set starting sample
of display window, left image only
.VARI U1
INTEGER - Raise displayed area,
left image only
.VARI D1
INTEGER - Lower displayed area,
left image only
.VARI L1
INTEGER - Move displayed area left,
left image only
.VARI R1
INTEGER - Move displayed area right,
left image only
.VARI SL2
INTEGER - Set starting line
of display window, right image only
.VARI SS2
INTEGER - Set starting sample
of display window, right image only
.VARI U2
INTEGER - Raise displayed area,
right image only
.VARI D2
INTEGER - Lower displayed area,
right image only
.VARI L2
INTEGER - Move displayed area left,
right image only
.VARI R2
INTEGER - Move displayed area right,
right image only
.VARI ZOOM
INTEGER - Magnify or compress displayed
area by an integral factor.
.VARI PARAMS
KEYWORD - Edit processing parameters
.VARI CORR
KEYWORD - Enable correlation
function.
.VARI PROJECT
KEYWORD - Project left area prior
to correlation.
.VARI HPF
KEYWORD - High pass filter areas
prior to correlation.
.VARI PHASE
KEYWORD - Correlate phase
information only.
.VARI INTERP
KEYWORD - Locate centroid
of correlation peak.
.VARI NLW
INTEGER - Length of correlation
window in pixels.
.VARI NSW
INTEGER - Width of correlation
window in pixels.
.VARI ZWIND
INTEGER - Size of area used to
compute average DN (Z) values.
.VARI NOCORR
KEYWORD - Disable correlation
function.
.VARI NOPROJEC
KEYWORD - Disable projection
.VARI NOHPF
KEYWORD - Disable high
pass filter.
.VARI NOPHASE
KEYWORD - Disable phase
correlation.
.VARI NOINTERP
KEYWORD - Disable centroid
location.
.VARI EDIT
Edit navigation data
.VARI GEODET
Use planetographic latitudes
.VARI GEOCEN
Use planetocentric latitudes
.VARI PR
REAL - Polar radius (km)
.VARI ER
REAL - Equatorial radius (km)
.VARI FL
REAL - Camera focal length (mm)
.VARI OAXIS
REAL - Optical axis intercept
 (line,sample) coordinates.
.VARI SC
REAL - Picture scale (pixels/mm)
.VARI SSP
REAL - Spacecraft position (lat,lon)
.VARI PC
REAL - Planet center (line,sample)
.VARI WAPC
REAL - Wide-angle planet center (line,sample)
.VARI ANGLN
REAL - North angle
.VARI RANGE
REAL - Spacecraft range
.VARI SAVE
KEYWORD - Save navigation data
.VARI RESTORE
KEYWORD - Restore navigation data
.VARI GETSEDR
KEYWORD - Retrieve SEDR nav data
.VARI STATUS
KEYWORD - Print navigation data or processing parameters
.VARIABLE RES
STRING--OPTIONAL
Specifies the Voyager Reseau Location File.
.VARI HELP
List available commands
.VARI EXIT
Exit from routine
.LEVEL2
.VARI INP
	INP=(PIC,REF,OLDPTS)
Input images and optional tiepoint file.  These are the image to be navigated
(PIC), the reference image (REF), and any tiepoints previously acquired for
this frame pair (OLDPTS).

The input images may be Voyager or Galileo SSI flight images in byte format.  
Both geometrically corrected (object-space) and uncorrected (image-space)
frames may be input.
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

.VARI NLW
INTEGER - Length of correlation window in pixels.
    The correlation window is of dimension NLW x NSW.
.VARI NAVVEL
Entering NAVVEL will cause NAV2 to adjust for atmospheric motion when fitting
the tiepoints.  The user is prompted to specify the zonal and meridional
components of each tiepoint.  For Jupiter, the user may elect to have the
program automatically compute the zonal velocities from a zonal-velocity
profile (the meridional component is assumed to be zero).  This command is
valid for atmospheric planets only.
.VARI ZWIND
INTEGER - Size of window used to compute average DN values for tiepoints.
Entering ZWIND=10 will cause a 10x10 window to be used for computing the
average DN value centered on each tiepoint.  This value is stored in the
output tiepoint file.
.VARI CZOOM
INTEGER - Center display and zoom
Example: CZOOM=2
    Center the display around the selected cursor position and magnify
    the image to twice normal resolution.
.VARI H
Display entire image.  The image may be displayed at reduced resolution
to accomplish this.
.VARI HIST
KEYWORD - Overlay (in graphics plane) histogram of displayed image area.
  A logrithmic frequency scale is used.
.VARI SPIKES
INTEGER - Ex: SPIKES=n will scale the frequencies so that the n'th largest
  frequency corresponds to maximum scale.
.VARI STRETCH
INTEGER - Stretch the displayed image
Example: STRETCH=(25,100)
    A hardware stretch (linear transformation) is applied to the displayed
    image.
.VARI STR1
The STR1 and STR2 commands may be used to stretch the left and right
image areas independently.  Entering STR1=(10,128) will cause the left
image area to be stretched as it is being written to the display.  Note
that these commands are independent of the hardware stretch, so that
following these commands with STRETCH=(5,220) will result in further
stretching of the stretched images.
.VARI GEODET
Use planetographic latitudes
    All input and output latitudes are reported as planetographic.
    Note that this is the default for Voyager.
.VARI GEOCEN
Use planetocentric latitudes
    All input and output latitudes are reported as planetocentric.
.VARI PC
REAL - Planet center (line,sample)
Example: PC=(433.2,315.3)
    The camera pointing is updated so that the planet center is 
    located at (line,sample)=(433.2,315.3) in the image.
.VARI WAPC
REAL - Planet center of VGR wide-angle frame.  The input frame is
assumed to be the narrow-angle frame of a WA-NA simultaneous
exposure.  WAPC permits input of the planet center (line,sample)
of the corresponding wide-angle frame.  The planet center for the
narrow-angle frame is then derived using the known transformation
between the to camera fields-of-view.
EX: WAPC=(235.3,335.8)
.VARI ANGLN
REAL - North angle
Example: ANGLN=45.3
    The camera pointing is updated so that the planet's projected
    spin axis is oriented at a 45.3 degree angle, measured
    clockwise from up in the image.
.VARI RANGE
REAL - Spacecraft range (km).
    Distance from spacecraft to planet center.
.VARI TARGET
STRING - Target body name
Example: TARGET=SATURN
.VARI SAVE
KEYWORD - Save current navigation or orbital data.
.VARI RESTORE
KEYWORD - Restore previously saved navigation or orbital data.  If no
previous values were saved, initial values are retrieved.
.VARI GETSEDR
KEYWORD - Retrieve navigation data from SEDR.  This restores the
nominal camera pointing.
.VARIABLE RES
STRING--OPTIONAL
Specifies the Voyager Reseau Location File.  If defaulted, the program
automatically accesses the file for the planet-of-encounter as follows:

               /project/vgr/resj.fil        (jupiter)
               /project/vgr/ress.fil        (saturn)
               /project/vgr/resu.fil        (uranus)
               /project/vgr/resn.fil        (neptune)

.VARI ZOOM
Magnify or compress displayed area by an integral factor.
   ZOOM=2 magnifies the area by a factor of two.
   ZOOM=-4 compresses the area by a factor of four.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create nav2.imake
/* Imake file for NAV2 */

#define PROGRAM nav2
#define R2LIB
#define FTNINC_LIST	fortport

#define MODULE_LIST nav2.f vparam.f maplabel.f editnav.f tac.f planet.f \
		sdsply.f jupiter_zvp.f
#define INCLUDE_LIST const.fin cpic.fin cpts.fin dev.fin xdgclr.fin cmap.fin 

#define CLEAN_OTHER_LIST zvp.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_SPICE
#define LIB_TAE
#define LIB_MATH77
#define LIB_RTL
#define LIB_VRDI
#define LIB_P2SUB
#define LIB_NETWORK 
/* End of Imake file */
$ Return
$!#############################################################################
$Test_File:
$ create tstnav2.pdf
!NAV2 unit test.........
!NAV2 is an interactive program.  The user is prompted for inputs.
!Read the help file before testing and make sure you know what you are doing.
!Before running this script you must allocate a display device as follows:
!	  VICAR> use xwa0
!When exiting from NAV2, please do not update the SEDR (i.e. respond 'N to
!the prompt)."
procedure
  RefGbl $echo
  RefGbl $syschar
  PARM TESTPROJECT KEYWORD Valid=(ALL,GLL,VGR,CAS) DEFAULT=ALL
body
let _onfail="continue"
let $echo="yes"

enable-log

use xwa0

LOCAL VGR_INP STRING COUNT=1
LOCAL GLL_INP STRING COUNT=1
LOCAL CAS_INP STRING COUNT=1
let VGR_INP="/project/test_work/testdata/mipl/vgr/"
let GLL_INP="/project/test_work/testdata/gll/"
let CAS_INP="/project/test_work/testdata/cassini/casIss/"

if (TESTPROJECT="ALL" or TESTPROJECT="GLL")
   NAV2 (&"GLL_INP"60.img, &"GLL_INP"57.img) OUT=gll_test.ibis
end-if

if (TESTPROJECT="ALL" or TESTPROJECT="VGR")
   !....test VGR object space
   FIT &"VGR_INP"ptp_vgr_red.img vgrred.img OFORM=BYTE 
   FIT &"VGR_INP"ptp_vgr_blu.img vgrblu.img OFORM=BYTE
   NAV2 (vgrblu.img vgrred.img) OUT=vgr_test.ibis target=jupiter

   !....test VGR image space
   copy &"VGR_INP"f1636832.raw a.img
   nav2 (&"VGR_INP"f1636832.raw,a.img) ibis.img target=io +
	res=&"VGR_INP"reseau.test
   !nav2 (&"VGR_INP"f1636832.raw,a.img,ibis.img) ibis2.img target=io +
   !	res=&"VGR_INP"reseau.test
   !gspice a.img target=io ckname=near
end-if

if (TESTPROJECT="ALL" or TESTPROJECT="CAS")
   copy &"CAS_INP"n1354897340.1 a.img
   nav2 (&"CAS_INP"n1354897340.1,a.img) ibis.img
   !nav2 (&"CAS_INP"n1354897340.1,a.img,ibis.img) ibis2.img
   !gspice a.img ckname=near
end-if

disable-log

end-proc
$!-----------------------------------------------------------------------------
$ create tstnav2.log
ush $VRDILIB/usedisp a xwa0
LOCAL VGR_INP STRING COUNT=1
LOCAL GLL_INP STRING COUNT=1
LOCAL CAS_INP STRING COUNT=1
let VGR_INP="/project/test_work/testdata/mipl/vgr/"
let GLL_INP="/project/test_work/testdata/gll/"
let CAS_INP="/project/test_work/testdata/cassini/casIss/"
if (TESTPROJECT="ALL" or TESTPROJECT="GLL")
   NAV2 (/project/test_work/testdata/gll/60.img, /project/test_work/testdata/gll/57.img) OUT=gll_test.ibis
Beginning VICAR task NAV2
 NAV2 Version 2016-02-18
msg
CKNAME=NAIF  SPKID=N083  PROGRAM=redict  990
            -0.0919477   0.5189606  -0.8498385
            -0.9863231  -0.1647092   0.0061336
            -0.1367932   0.8387793   0.5270074
msg
CKNAME=NAIF  SPKID=N083  PROGRAM=redict  990
            -0.0877228   0.5188153  -0.8503737
            -0.9863254  -0.1648054   0.0011990
            -0.1395241   0.8388503   0.5261779
 Do you wish to adjust for wind speed?
'N
 FRAME= 61056000  SCET=90343  22259466
 FRAME= 61055700  SCET=90343  21957466
 Time separation=    182. seconds
exit
 NAV2 task completed
end-if
if (TESTPROJECT="ALL" or TESTPROJECT="VGR")
   FIT /project/test_work/testdata/mipl/vgr/ptp_vgr_red.img vgrred.img OFORM=BYTE
Beginning VICAR task FIT

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=  2112.281 STANDARD DEVIATION=  1836.205 NUMBER OF ELEMENTS= 1000000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=  2112.281 STANDARD DEVIATION=  1836.205 NUMBER OF ELEMENTS= 1000000

MINIMUM DN OF  -9147   SCALED TO     0

MAXIMUM DN OF   6162   SCALED TO   255
FIT task completed
   FIT /project/test_work/testdata/mipl/vgr/ptp_vgr_blu.img vgrblu.img OFORM=BYTE
Beginning VICAR task FIT

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=  2105.005 STANDARD DEVIATION=  1943.223 NUMBER OF ELEMENTS= 1000000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=  2105.005 STANDARD DEVIATION=  1943.223 NUMBER OF ELEMENTS= 1000000

MINIMUM DN OF  -3508   SCALED TO     0

MAXIMUM DN OF   5636   SCALED TO   255
FIT task completed
   NAV2 (vgrblu.img vgrred.img) OUT=vgr_test.ibis target=jupiter
Beginning VICAR task NAV2
 NAV2 Version 2016-02-18
msg
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
            -0.0338463  -0.0235998  -0.9991484
            -0.6334012   0.7738170   0.0031791
             0.7730830   0.6329694  -0.0411390
msg
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAIF  SPKID=N005  PROGRAM=VGRMCK  VRH059  MIPS  03/02/85
            -0.0247017  -0.0290090  -0.9992739
            -0.6031438   0.7975899  -0.0082446
             0.7972499   0.6025022  -0.0371985
 Do you wish to adjust for wind speed?
'N
 FRAME=  1634146  SCET=79062 215722910
 FRAME=  1634142  SCET=79062 215409080
 Time separation=    193. seconds
exit
 NAV2 task completed
   copy /project/test_work/testdata/mipl/vgr/f1636832.raw a.img
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
   nav2 (/project/test_work/testdata/mipl/vgr/f1636832.raw,+
a.img) ibis.img target=io  	res=/project/test_work/testdata/mipl/vgr/reseau.test
Beginning VICAR task nav2
 NAV2 Version 2016-02-18
msg
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAV   SPKID=N005  PROGRAM=NAV     GMY059  NONE  01/12/01
            -0.0003499  -0.0006059  -0.9999998
             0.5635410   0.8260878  -0.0006977
             0.8260881  -0.5635411   0.0000523
msg
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAV   SPKID=N005  PROGRAM=NAV     GMY059  NONE  01/12/01
            -0.0003499  -0.0006059  -0.9999998
             0.5635410   0.8260878  -0.0006977
             0.8260881  -0.5635411   0.0000523
 Frame  =    1636832
 Camera =          7
 Filter =          0
 Year   =          0
 Day    =          0
 Frame  =    1636832
 Camera =          7
 Filter =          0
 Year   =          0
 Day    =          0
 FRAME=  1636832  SCET=79063 192259820
 FRAME=  1636832  SCET=79063 192259820
 Time separation=      0. seconds
exit
 NAV2 task completed
end-if
if (TESTPROJECT="ALL" or TESTPROJECT="CAS")
   copy /project/test_work/testdata/cassini/casIss/n1354897340.1 a.img
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
   nav2 (/project/test_work/testdata/cassini/casIss/n1354897340.1,a.img) ibis.img
Beginning VICAR task nav2
 NAV2 Version 2016-02-18
msg
CKNAME=NAIF  SPKID=N009  PROGRAM=Y CASS  
            -0.0020650   0.0645526  -0.9979122
             0.9999965  -0.0015065  -0.0021668
            -0.0016432  -0.9979132  -0.0645493
msg
CKNAME=NAIF  SPKID=N009  PROGRAM=Y CASS  
            -0.0020650   0.0645526  -0.9979122
             0.9999965  -0.0015065  -0.0021668
            -0.0016432  -0.9979132  -0.0645493
 Do you wish to adjust for wind speed?
'N
 FRAME=*********  SCET=  342 161056162
 FRAME=*********  SCET=  342 161056162
 Time separation=      0. seconds
exit
 NAV2 task completed
end-if
disable-log
$ Return
$!#############################################################################
