$!****************************************************************************
$!
$! Build proc for MIPL module resloc
$! VPACK Version 1.9, Wednesday, April 17, 2013, 10:06:05
$!
$! Execute by entering:		$ @resloc
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
$ write sys$output "*** module resloc ***"
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
$ write sys$output "Invalid argument given to resloc.com file -- ", primary
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
$   if F$SEARCH("resloc.imake") .nes. ""
$   then
$      vimake resloc
$      purge resloc.bld
$   else
$      if F$SEARCH("resloc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake resloc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @resloc.bld "STD"
$   else
$      @resloc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create resloc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack resloc.com -mixed -
	-s resloc.f open_pic.f get_lab_ids.f get_poe.f vgrlab_cam.f -
	   vgrlab_scet.f open_rdb.f get_res_rdb.f put_res_rdb.f read_pic.f -
	   find_res_d.f get_rtype.f get_neighbors.f get_A_size.f compute_A.f -
	   get_R_size.f get_window.f check_window.f filter_B.f print_B.f -
	   compute_R_detrend.f detrend_window.f compute_R_gap.f print_R.f -
	   find_rmax.f local_max.f interp_max.f get_W.f compute_dn.f -
	   compute_dedge.f compute_offsets.f compute_eps.f compute_dc.f -
	   compute_q.f choose_best.f reject.f check_rho.f check_q.f -
	   check_corners.f fill_res.f interp_3.f interp_4.f compute_geo.f -
	   interg.f triag.f write_res.f write_geo.f ptres1.f ptres5.f -
	   ptres7.f ptres8.f print_window.f print_stats.f p_stats.f -
	   p_avg_stats.f print_res.f pmjs.f print_rows.f display_results.f -
	   overlay.f -
	-i resloc.imake -
	-p resloc.pdf -
	-t tstresloc.pdf tst_defaults.pdf tst_parameters.pdf -
	   tst_calibration.pdf tst_rdb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create resloc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c Voyager image reseau location program resloc.

      include 'VICMAIN_FOR'	!resloc is entered through VICAR

c Nominal usage...
c	resloc inp=D out=res
c	resloc inp=D out=(res,geo)
c
c Used with a reseau database...
c	resloc inp=(D,rdb)
c       resloc inp=(D,rdb) out=res
c       resloc inp=(D,rdb) out=(res,geo)

      subroutine main44		!top level resloc
      implicit none
      integer ni		!number of inputs (1 or 2)
      integer no		!number of outputs (0,1,or 2)

      byte D(800,800)		!D = 800x800 Voyager byte image
      integer pic		!VICAR unit number for D
      character*7200 lab        !Voyager image label
      integer*4 labsize/7200/

      real*4 res(2,202)		!(l,s) coordinates of the marks found in D
      real*4 nom(2,202)		!nominal coordinates for camera sn
      real*4 os_res(2,202)	!object space (l,s) coordinates

      logical valid(202)	!.true. if res coordinates are valid
      logical d_valid		!.true. if image has been read into D.

      integer sn		!Voyager camera serial number (4,5,6,7)
      real geo(4,24,23)		!geometric transformation parameters for sn
      integer ids(5)		!Image IDs: frm,sn,fil,scet year & day
      integer cnt		!cnt=1 if file is specified.
      logical xvptst,success
      character*132 file

      call ifmessage('resloc version April 14, 2013')
      call open_pic(pic,lab)	!open image D and return pic and lab
      call get_lab_ids(lab,ids)	!get pic identifiers from lab
      sn = ids(2)		!Voyager camera serial number (4,5,6,7)
      call getres(nom,sn)	!get nominal coordinates for sn 
      if (xvptst('pnom')) call print_nom(nom)

      d_valid = .false.		!in the beginning, matrix D is empty

c Find the reseau locations in D or get it from the rdb...
      call xvpcnt('inp',ni)			!number of inputs
      if (ni.eq.1) then				!If inp=D, then
         call read_pic(pic,D,d_valid)		!read pic into D
         call find_res_d(D,lab,nom,res)		!and find res.
      else					!else if inp=(D,rdb), then
         call get_res_rdb(ids,res,success)      !retrieve res from rdb.
         if (.not.success) then			!If res not in rdb, then
            call read_pic(pic,D,d_valid)	! read pic into D
            call find_res_d(D,lab,nom,res)	! find res in D
            call put_res_rdb(ids,res)  		! store res in the rdb
         endif
      endif
      if (xvptst('pres')) call print_res(res)

c Output res and geo...
      call xvpcnt('out',no)			!number of outputs=0,1 or 2
      if (no.eq.1) then
         call write_res(res,ids)		!output res
      elseif (no.eq.2) then
         call write_res(res,ids)		!output res
         call vgros(sn,os_res)			!get object space coordinates
         call compute_geo(res,os_res,geo)	!compute geo
         call write_geo(geo)			!output geo
      endif

c Optionally overlay res on image D...
      call xvp('ores',file,cnt)
      if (cnt.eq.1) then
         call read_pic(pic,D,d_valid)
         call overlay(D,res,file)
      endif

c Optionally overlay nom on image D...
      call xvp('onom',file,cnt)
      if (cnt.eq.1) then
         call read_pic(pic,D,d_valid)
         call overlay(D,nom,file)
      endif

      call xvmessage('resloc task completed',0)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create open_pic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open image D and return pic and lab.

      subroutine open_pic(pic,lab)
      implicit none
c Outputs...
      integer pic		!VICAR unit number for D
      character*7200 lab        !Voyager image label

c Local variables...
      integer*4 labsize/7200/
      integer ind		!ignored return status
      integer ns,nl		!pic image size according to VICAR
      character*5 format

      call xvunit(pic,'inp',1,ind,0) 	!inp=pic
      call xvopen(pic,ind,'open_act','sa','io_act','sa',0)

c Make sure the image is valid...
      call xvget(pic,ind,'nl',nl,'ns',ns,' ')
      if (nl.ne.800.or.ns.ne.800) call mabend('***wrong pic size')
      call xvget(pic,ind,'format',format,' ')
      if (format.ne.'BYTE') call mabend('***pic must be byte') 

c Read the Voyager label...
      call xlgetlabel(pic,lab,labsize,ind)
      call chkstat(ind,'***err reading label, ind=',1,ind,1)
      return
      end

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_lab_ids.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c**************************************************************
c Get image identifiers from Voyager label...

      subroutine get_lab_ids(lab,ids)
      implicit none
c Input...
      character*7200 lab	!Voyager image label
c Output...
      integer*4 ids(5)	!fds,cam,filter,scet year,scet day

c     ...local variables

      integer*4 fds		!frame number (VGR FDS)
      integer*4 cam		!camera serial number (4,5,6,7)
      integer*4 filter          !filter position (0-7)
      integer*4 gain            !0=low gain, 1=high gain
      integer*4 scan            !scan rate: 1, 2, 3, 5, 10
      integer*4 mode            !operating mode
      real*4 t                  !exposure time in milliseconds
      integer poe		!planet of encounter
      character*10 picno	!0548J1-001
      integer par(2),count,def,ind
      character*80 msg
  101 format('Frame=',i7,' Camera=',i1)

      call vgrlab_fds(lab,fds)
      call xvparm('frame',par,count,def,1)
      if (def.eq.0) fds=par(1)

      call vgrlab_cam(lab,cam)
      call xvparm('camera',par,count,def,1)
      if (def.eq.0) cam=par(1)

      call vgrlab_camparams(lab,filter,gain,scan,mode,t)
c     ...only filter is used.

      call vgrlab_scet(lab,par,ind)
      if (ind.eq.-1) call xvmessage( '***invalid SCET',0)

      ids(1) = fds	!7-digit VGR FDS
      ids(2) = cam		!camera serial number (4-7)
      ids(3) = filter		!filter position (0-7)
      ids(4) = par(1)		!SCET year
      ids(5) = par(2)		!SCET day

      call vgrlab_picno(lab,picno,ind)
      if (ind.eq.-1) then
         call xvmessage('***PICNO not found',' ')
         poe = 5		!pick the most benign planet
      else
         if (picno(5:5).eq.'J') then
            poe = 5
         elseif (picno(5:5).eq.'S') then
            poe = 6
         elseif (picno(5:5).eq.'U') then
            poe = 7
         elseif (picno(5:5).eq.'N') then
            poe = 8
         else
            call xvmessage('***error determining POE',0)
            poe = 5
         endif
      endif

      write(msg,101) fds,cam
      call xvmessage(msg,' ')
      if (fds.eq.-999) call mabend('***invalid FDS count')
      if (cam.eq.-999) call mabend('***invalid camera')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_poe.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c**************************************************************
c Get the planet-of-encounter from the Voyager label...

      subroutine get_poe(lab,poe)
      implicit none
c Input...
      character*7200 lab	!Voyager image label
c Output...
      integer*4 poe		!planet of encounter

c     ...local variables
      integer ind
      character*10 picno	!0548J1-001
      logical xvptst

      poe = 0
      if (xvptst('jupiter')) poe=5
      if (xvptst('saturn')) poe=6
      if (xvptst('uranus')) poe=7
      if (xvptst('neptune')) poe=8
      if (poe.gt.0) goto 10

      call vgrlab_picno(lab,picno,ind)
      if (ind.ne.-1) then
         if (picno(5:5).eq.'J') poe=5
         if (picno(5:5).eq.'S') poe=6
         if (picno(5:5).eq.'U') poe=7
         if (picno(5:5).eq.'N') poe=8
      endif
      if (poe.eq.0) poe=8

   10 continue
      if (poe.eq.5) call xvmessage('Planet-of-encounter is Jupiter',0)
      if (poe.eq.6) call xvmessage('Planet-of-encounter is Saturn',0)
      if (poe.eq.7) call xvmessage('Planet-of-encounter is Uranus',0)
      if (poe.eq.8) call xvmessage('Planet-of-encounter is Neptune',0)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vgrlab_cam.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract camera serial number from Voyager flight label.

      subroutine vgrlab_cam(lab,cam)
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 cam		!output camera serial number (4-7)
      
      integer*4 i
      integer*4 sc_id,camera_id
      integer*4 camera_sn(2,2)/7,6,	!VGR1 NA,WA
     &			       5,4/	!VGR2 NA,WA

      call vgrlab_sc_id(lab,sc_id)
      call vgrlab_camera_id(lab,camera_id)

      if (sc_id.eq.-999 .or. camera_id.eq.-999) then
         cam = -999
      else
         cam = camera_sn(camera_id,sc_id)
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract spacecraft ID from Voyager flight label.

      subroutine vgrlab_sc_id(lab,sc_id)
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 sc_id		! 1=VGR-1, 2=VGR-2
      
      integer*4 i

      sc_id = -999

      i = index(lab(1:),'VGR-1')
      if (i.ne.0) then
         sc_id = 1		!Voyager 1
      else
         i = index(lab(1:),'VGR-2')
         if (i.ne.0) sc_id=2
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract camera id from Voyager flight label.

      subroutine vgrlab_camera_id(lab,camera_id) 
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 camera_id	! 1=NA, 2=WA
      
      integer*4 i

      camera_id = -999

      i = index(lab(1:),'NA CAMERA')
      if (i.ne.0) then
         camera_id = 1		!narrow angle
      else
         i = index(lab(1:),'WA CAMERA')
         if (i.ne.0) camera_id=2 
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract FDS count from Voyager flight label.
c
      subroutine vgrlab_fds(lab,fds)
      implicit none
      character*7200 lab	!Voyager flight label (input)
      integer*4 fds		!FDS count (output)
      
      integer*4 i,mod16,mod60

c note: the FDS is returned as an integer as 100*mod16 + mod60
c Example: FDS 16368.32 is returened as 1636832

       i = index(lab(1:),' FDS ')
       if (i.ne.0 .and. lab(i+5:i+5).ne.'*') then
          read(lab(i+5:),'(bn,i5)') mod16
          read(lab(i+11:),'(bn,i2)') mod60
          fds = 100*mod16 + mod60
       else
          fds = -999
       endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract filter, gain, scan rate, operating mode, and exposure time
c from Voyager flight label.

      subroutine vgrlab_camparams(lab,filter,gain,scan,mode,t)
      implicit none
c     ...input argument
      character*7200 lab	!Voyager flight label
c     ...output arguments
      integer*4 filter		!filter position (0-7)
      integer*4 gain		!0=low gain, 1=high gain
      integer*4 scan		!scan rate: 1, 2, 3, 5, 10
      integer*4 mode		!operating mode
      real*4 t			!exposure time in milliseconds
      
      integer*4 i

      filter = -999
      gain = -999
      scan = -999
      mode = -999
      t = -999.

      i = index(lab(1:),' FILT ')	!example: FILT 0(CLEAR)
      if (i.ne.0 .and. lab(i+6:i+6).ne.'*') 
     &	    read(lab(i+6:),'(bn,i1)') filter

      if (index(lab(1:),' LO GAIN').gt.0) then
         gain = 0
      elseif (index(lab(1:),' HI GAIN ').gt.0) then
         gain = 1
      endif

      i = index(lab(1:),'  SCAN RATE')	!example: SCAN RATE 10:1
      if (i.ne.0 .and. lab(i+12:i+12) .ne. '*') 
     & 	   read(lab(i+12:),'(bn,i2)') scan

      i = index(lab(1:),' MODE')	!example: MODE 4(BOTALT)
      if (i.ne.0 .and. lab(i+6:i+6).ne.'*')
     &	   read (lab(i+6:),'(bn,i1)') mode

      i = index(lab(1:),' EXP ')	!example: EXP 00360.0 MSEC
      if (i.ne.0 .and. lab(i+5:i+5).ne.'*') 
     &	   read(lab(i+5:),'(bn,f7.0)') t
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract PICNO from Voyager flight label.
c
      subroutine vgrlab_picno(lab,picno,ind1)
      implicit none
      character*7200 lab	!Voyager flight label (input)
      character*10 picno	!picno='xxxxPS+DDD' (output)
      integer*4 ind1
      
      integer*4 i

      i = index(lab(1:),' PICNO ')	!Example: PICNO 0548J1-001
      if (i.gt.0 .and. lab(i+7:i+7).ne.'X'
     +          .and. lab(i+7:i+7).ne.'*') then
         picno = lab(i+7:i+16)
      else
         ind1 = -1
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vgrlab_scet.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Extract Spacecraft-Event_Time from Voyager flight label
c
      subroutine vgrlab_scet(lab,scet,ind1)
      implicit none
      character*7200 lab	!input Voyager flight label
      integer*4 scet(5)		!output SCET year,day,hour,minute,second
      integer*4 ind1

      integer*4 i

      ind1 = 0

      do i=1,5
         scet(i) = 0
      enddo

c Note: the year is two digits

      i = index(lab(1:),' SCET ')	!Example: SCET 79.063 19:23:00

      if (i.eq.0) then
         ind1 = -1
         return	!SCET not found
      endif

      if (lab(i+6:i+6).ne.'*') then
         read(lab(i+6:),'(bn,i2)') scet(1)	!year
      else
         ind1 = -1
      endif

      if (lab(i+9:i+9).ne.'*') then
         read(lab(i+9:),'(bn,i3)') scet(2)	!day
      else
         ind1 = -1
      endif

      if (lab(i+13:i+13).ne.'*') then
         read(lab(i+13:),'(bn,i2)') scet(3)	!hr
      else
         ind1 = -1
      endif

      if (lab(i+16:i+16).ne.'*') then
         read(lab(i+16:),'(bn,i2)') scet(4)	!min
      else
         ind1 = -1
      endif

      if (lab(i+19:i+19).ne.'*') then
         read(lab(i+19:),'(bn,i2)') scet(5)	!sec
      else
         ind1 = -1
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create open_rdb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Open the Reseau Database (RDB)
c All arguments are outputs

      subroutine open_rdb(rdb,ibis,nrows)
      implicit none
      integer rdb,ibis		!VICAR and IBIS unit numbers for RDB
      integer nrows		!number of records in RDB

      integer ind,ibis_file_get
      character*6 format(409)/5*'full',404*'real'/	!RDB record format

      call xvunit(rdb,'inp',2,ind,0)  
      call ibis_file_open(rdb,ibis,'read',409,99999,
     &                        format,0,ind)
      if (ind.ne.1) call ibis_signal_u(rdb,ind,1)
      ind = ibis_file_get(ibis,'nr',nrows,1,1)
      if (nrows.lt.0) call ibis_signal(ibis,ind,1)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_res_rdb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c The database is a table (spreadsheet) where each row contains the reseau
c locations for a Voyager image (res) together with its image identifiers (ids).
c The ids uniquely identifies each image taken during the mission.  

c Try to get res from the rdb.
      subroutine get_res_rdb(ids,res,success)
      implicit none
c Input...
      integer ids(5)		!frm,cam,fil,and scet year and day
c Outputs...
      real res(2,202)		!output locations (line,samp)
      logical success		!.true. if row for res was found in rdb

c VICAR and IBIS interfaces to the database...
      common/cibis/rdb,ibis,nrows
      integer rdb               !VICAR unit number for rdb
      integer ibis              !IBIS unit number for rdb
      integer nrows             !number of rows in the rdb

      integer ind
      logical xvptst
 

c Open the reseau database and set cibis...
      call open_rdb(rdb,ibis,nrows)

c If redo, ignore any rdb entry...
      success = .false.
      if (xvptst('redo')) return

c Retrieve res from the rdb...
      call getlocv2(ibis,nrows,ids,res,ind)
      if (ind.eq.0) then
         success = .true.
         call xvmessage('res was retrieved from RDB',0)
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create put_res_rdb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write res to the RDB.
c
      subroutine put_res_rdb(ids,res)
      implicit none
c Inputs...
      integer ids(5)		!pic IDs: ifrm,icam,ifil,iyear,iday
      real res(2,202)		!output locations (line,samp)

      common/cibis/rdb,ibis,nrows
      integer rdb               !VICAR unit number for rdb
      integer ibis              !IBIS unit number for rdb
      integer nrows             !number of images in rdb table

      integer ind
      character*80 msg

      call putlocv2(rdb,ibis,nrows,ids,res)	!put res in RDB
      call ibis_file_close(ibis,' ',ind)
      write(msg,101) ids(1)		!print frame number
  101 format('Frame ',i7,' stored in reseau database.')
      call xvmessage(msg,' ')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create read_pic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Read input pic into matrix D.

      subroutine read_pic(pic,D,d_valid)
      implicit none
c Input...
      integer pic
      logical d_valid
c Outputs...
      byte D(800,800)		!byte format to reduce memory.
      integer*4 ids(5)		!ids=(ifrm,icam,ifil,iyear,iday)

c Local variables...
      integer l
      integer ind		!ignored return status.

      if (d_valid) return	!return if already done.

      do l=1,800
         call xvread(pic,D(1,l),ind,0)
      enddo
      call xvclose(pic,ind,0)
      d_valid = .true.
      return
      end

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create find_res_d.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find the reseau marks in D.

      subroutine find_res_d(D,lab,nom,res)
      implicit none
c Inputs...
      byte D(800,800)		!Voyager image
      character*7200 lab        !label of image D
      real*4 nom(2,202)		!(lnom,snom)=nominal coordinates for camera

c Output...
      real*4 res(2,202)		!(line,samp) coordinates reseau marks

c Local variables...
      real*4 dc(202)		!estimate DN contribution of the camera
      real*4 lnom(202),snom(202)!lnom(i)=nom(1,i) and snom(i)=nom(2,i)
      integer lk,sk		!nominals truncated to integers
      real*4 e,f		!mean offsets from nominals

c Given the shape template A, we compute the correlation coefficient rho at
c every pixel inside the search area.
      integer*2 A(9,9)		!reseau shape template
      integer*2 W(9,9)		!a window in the search area
      integer ia,ja		!half-window dimensions of A
      real*4 R(25,25)		!rho for each pixel in (max) 25x25 search area.
      integer ir,jr		!half-window dimensions of R 
      integer rsw(9)		!sums of rows of W

c For convenience, we copy all the pixels needed to conduct the search
c into a window B.
      integer*2 B(33,33)	!window centered at truncated nominals (lk,sk)
      integer ib,jb		!half-window dimensions of B
      integer csw(33)		!column sums along the windows of B

c We save the following measurements for the 3 highest peaks in each R:
      real*4 rho(3,202)		!correlation coefficient
      real*4 dn(3,202)		!darkest DN in W (near center of window)
      real*4 dedge(3,202)	!mean DN of pixels along edge of window
      real*4 l(3,202),s(3,202)	!(line,sample) coordinates of the 3 peaks
      real*4 el(3,202),es(3,202)!estimated (line,sample) coordinates of peaks
      real*4 eps(3,202)		!distance from (l,s) to (el,es)
      real*4 g(3,202)		!normalized darkness measure
      real*4 h(3,202)		!normalized distance measure
      real*4 q(3,202)		!composite match quality

c We choose the peak with the highest q and copy its measurements...
      integer choice(202)	!index of chosen candidate (1,2 or 3).
      real*4 crho(202)		!correlation coefficient
      real*4 cdn(202)		!minimum DN
      real*4 cdedge(202)	!mean DN of window's perimeter
      real*4 cel(202),ces(202)	!estimated coordinates
      real*4 cl(202),cs(202)	!located coordinates
      real*4 ceps(202)		!distance from (cel,ces) to (cl,cs)
      real*4 cg(202)		!normalized darkness measure
      real*4 ch(202)		!normalized distance measure
      real*4 cq(202)		!composite match quality

c Miscellaneous variables...
      integer i,k,n,count
      integer*4 di(3),dj(3)	!integer offsets from nominals 
      real*4 dx(3),dy(3)	!fractional offsets from nominals
      real*4 maxeps		!maximum displacement
      integer*4 poe		!planet of encounter (J=5,S=6,U=7,N=8)
      integer tres		!print intermediate values for reseau mark tres
      logical gap		!true if search area contains data gaps
      logical filter		!filter hot edges
      logical noin		!do no interpolation of R
      logical dbug		!print more stuff to aid debugging
      logical xvptst

c Reseau mark data constants...
      integer nbr(8,202),rtype(202)

      call get_neighbors(nbr)	!8 neighbors of each mark
      call get_rtype(rtype)	!type of reseau mark (corner,edge,interior)
      call get_poe(lab,poe)	!data-compression line truncation if poe=6 or 7

c Reformat the nominals to make the problem easier to see....
      do k=1,202
         lnom(k) = nom(1,k)
         snom(k) = nom(2,k)		
      enddo

c Initialize all measurements as invalid...

      do k=1,202
         dc(k) = -999
         do i=1,3
            rho(i,k) = -999
            dn(i,k) = -999
            dedge(i,k) = -999
            l(i,k) = -999
            s(i,k) = -999
            eps(i,k) = -999
            g(i,k) = -999
            h(i,k) = -999
            q(i,k) = -999
         enddo
      enddo

c Set the program parameters...

      filter = .not.xvptst('nofilter')	!hot edge filter
      noin = xvptst('noin')		!no subpixel accuracy
      dbug = xvptst('dbug')		!print diagnostic messages
      call xvp('tres',tres,count)	!print arrays for this test reseau mark
      call get_A_size(ia,ja)		!get half-window dimensions of A
      call compute_A(A,ia,ja)		!compute the shape template
      call get_R_size(ir,jr)		!get half-window dimensions of R
      ib = ir + ia			!compute half-window dimensions of B
      jb = jr + ja

c Main loop...

c For each reseau mark k, k=1,2,...,202, the following is done:
c The correlation coefficient is computed at every pixel in the search area
c and stored in matrix R.  The mark is assumed to be located at one of the
c three highest peaks in R.  For each of these three peaks, we measure its
c darkness and its distance from its expected coordinates.  And from the
c darkness, distance, and our previously computed R values, we select the peak
c where the mark is most likely to be found.

      do 20 k=1,202		
      lk = lnom(k)		!truncate the nominal coordinates
      sk = snom(k)		!into integers (lk,sk).
      call get_window(D,lk,sk,B,ib,jb)	!get window centered at (lk,sk)
      call check_window(B,ib,jb,lk,sk,k,rtype(k),poe,gap) !flag gaps in window
      if (filter) call filter_B(B,ib,jb,lk,sk,rtype(k))	!filter hot edges
      if (k.eq.tres) call print_B(B,ib,jb,k)
      if (gap .or. rtype(k).ne.0) then
         call compute_R_gap(A,ia,ja,B,ib,jb,R,ir,jr)
      else
         call compute_R_detrend(A,ia,ja,B,ib,jb,R,ir,jr,
     &		W,rsw,csw)
      endif
      if (k.eq.tres) call print_R(R,ir,jr)

c     ...Find the 3 best candidates for mark k and compute l, s, dn and
c     ...dedge for these candidates.

      call find_rmax(R,ir,jr,rho(1,k),di,dj)	!return offsets from R(0,0)
      do i=1,3		!loop thru the 3 highest peaks in R
         if (rho(i,k).gt.-1) then	!if rho is a valid measure...
            call interp_max(R,ir,jr,di(i),dj(i),noin,rho(i,k),
     &		dx(i),dy(i))		!compute subpixel offsets
            l(i,k) = lk + dj(i) + dy(i)
            s(i,k) = sk + di(i) + dx(i)
            call get_W(B,ib,jb,W,ia,ja,di(i),dj(i))	!measure the dn
            call compute_dn(W,ia,ja,dn(i,k))		!dn of mark
            call compute_dedge(W,ia,ja,dedge(i,k))	!background dn
            if (k.eq.tres) then
               call ptres5(i,di,dj,dx,dy,k,rho,dn,dedge)
ccc               call detrend_window(B,ib,jb,di(i),dj(i),W,ia,ja,rsw,csw)
               call print_window(W,ia,ja)
            endif
         endif
      enddo
   20 continue

      call compute_offsets(l,s,lnom,snom,rtype,e,f)  !compute e and f
      call compute_eps(l,s,lnom,snom,e,f,tres,eps)
      call compute_dc(l,s,dn,nbr,rtype,dc)

c     ...Choose the candidate with the highest q.
ccc      maxeps = sqrt((ir+abs(e))**2+(jr+abs(f))**2)
      maxeps = ir
      call compute_q(rho,dn,dedge,dc,eps,maxeps,tres,g,h,q)
      call choose_best(q,choice,dbug)

c     ...Simplify things by copying the data for the chosen candidate
      do k=1,202
         i = choice(k)
         crho(k) = rho(i,k)
         cdn(k) = dn(i,k)
         cdedge(k) = dedge(i,k)
         cl(k) = l(i,k) 
         cs(k) = s(i,k)
         ceps(k) = eps(i,k)
         cg(k) = g(i,k)
         ch(k) = h(i,k)
         cq(k) = q(i,k)
      enddo

      if (xvptst('pstats')) call print_stats(cdn,dc,
     &		cdedge,ceps,crho,cg,ch,cq)

c     ...Identify and reject mismatches (by setting cl and cs to -999).
      call reject(cl,cs)	!reject a specific list of reseau marks 
      call check_rho(crho,cl,cs,dbug)	!reject if rho<rthresh
      call check_q(cq,cl,cs,dbug)	!reject if q<qthresh
      call check_corners(ceps,cl,cs)	!reject if wrong mark in corner
      call p_avg_stats(ceps,crho,cq,cl)

c     ...Fill in the (cl,cs) coordinates of all marks rejected or not found
      if (.not.xvptst('nofill'))
     &	 call fill_res(lnom,snom,nbr,rtype,e,f,cl,cs)

      do k=1,202		!copy coordinates to output format
         res(1,k) = cl(k)
         res(2,k) = cs(k)
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_rtype.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the reseau mark type...

      subroutine get_rtype(rtype)
      implicit none
      integer rtype(202)	!reseau mark type

      integer k

      do k=1,202
         rtype(k) = 0		!0=interior mark
      enddo

c Set the four corners...
      rtype(1) = 1
      rtype(12) = 2
      rtype(190) = 3
      rtype(201) = 4

c Set the top edge...
      do k=2,11
         rtype(k) = 5
      enddo

c Set the left edge...
      do k=47,152,15
         rtype(k) = 6
      enddo
      rtype(167) = 6

c Set the bottom edge...
      do k=191,200
         rtype(k) = 7
      enddo

c Set the right edge...
      do k=50,155,15
         rtype(k) = 8
      enddo
      rtype(178) = 8
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_neighbors.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors surrounding each of the 202 reseau marks.
c
      subroutine get_neighbors(nbr)
      implicit none
c Output...
      integer nbr(8,202)	!8 neighbors of each reseau mark

      integer k,i
c      character*80 msg

      call get_nbrs_corners(nbr)
      call get_nbrs_frame(nbr)
      call get_nbrs_interior(nbr)
      call get_nbrs_202(nbr)
c      do k=1,202
c      write(msg,101) k,(nbr(i,k),i=1,8)
c  101 format('k=',i3,' nbrs=',8i4)
c      call xvmessage(msg,0)
c      enddo
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of the reseau marks in the corners.

      subroutine get_nbrs_corners(nbr)
      implicit none
c Filled...
      integer nbr(8,202)

c Local variables...
      integer i

      integer n1(8)/13,2,24,25,14,36,0,0/	!neighbors of res mark 1
      integer n13(8)/1,2,24,25,0,14,36,37/	!neighbors of res mark 13
      integer n25(8)/13,14,36,37,24,26,2,48/
      integer n37(8)/25,26,48,0,36,38,14,52/

      integer n12(8)/23,11,35,34,22,46,0,0/	!0 if neighbor does not exist
      integer n23(8)/11,12,34,35,22,0,46,45/
      integer n34(8)/22,23,45,46,33,35,11,49/
      integer n45(8)/33,34,0,49,44,46,22,60/

      integer n157(8)/153,168,169,156,158,142,180,0/
      integer n168(8)/156,157,179,180,167,169,153,191/
      integer n179(8)/167,168,190,191,156,180,157,0/
      integer n190(8)/179,167,191,168,0,180,156,0/

      integer n165(8)/154,176,177,164,166,150,188,0/
      integer n177(8)/165,166,188,189,176,178,154,200/
      integer n189(8)/177,178,200,201,188,0,166,165/
      integer n201(8)/189,178,200,177,166,188,0,0/

c Upper-left corner...
      do i=1,8
         nbr(i,1) = n1(i)
         nbr(i,13) = n13(i)
         nbr(i,25) = n25(i)
         nbr(i,37) = n37(i)
      enddo

c Upper-right corner...
      do i=1,8
         nbr(i,12) = n12(i)
         nbr(i,23) = n23(i)
         nbr(i,34) = n34(i)
         nbr(i,45) = n45(i)
      enddo

c Lower-left corner...
      do i=1,8
         nbr(i,157) = n157(i)
         nbr(i,168) = n168(i)
         nbr(i,179) = n179(i)
         nbr(i,190) = n190(i)
      enddo

c Lower-right corner...
      do i=1,8
         nbr(i,165) = n165(i)
         nbr(i,177) = n177(i)
         nbr(i,189) = n189(i)
         nbr(i,201) = n201(i)
      enddo 

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors for the reseau marks making up the frame.
c
      subroutine get_nbrs_frame(nbr)
      implicit none
c Ouput...
      integer nbr(8,202)

c Local variables...

c Neighbors of reseau mark 2,14,26,etc...

      integer n2(8)/13,14,1,3,25,0,0,0/		!0 if neighbor does not exist
      integer n14(8)/2,3,25,26,13,15,0,37/
      integer n26(8)/14,15,37,38,25,27,3,0/
      integer n38(8)/26,27,0,0,37,39,15,53/

      integer n158(8)/169,170,157,159,143,181,0,0/
      integer n169(8)/157,158,180,181,168,170,192,0/
      integer n180(8)/168,169,191,192,179,181,157,0/
      integer n191(8)/179,180,190,192,168,0,0,0/

      integer n24(8)/13,36,1,47,25,0,0,0/ 
      integer n36(8)/24,25,47,48,13,51,37,0/
      integer n48(8)/36,37,51,52,25,63,47,0/
      integer n52(8)/48,63,51,53,37,67,0,0/

      integer n60(8)/49,64,59,61,45,75,0,0/
      integer n49(8)/45,46,60,61,34,64,50,0/
      integer n46(8)/34,35,49,50,23,61,45,0/
      integer n35(8)/23,46,12,50,34,0,0,0/

c Get neighbors of top frame...
      call get_nbrs_row(2,11,n2,nbr)	!row starts at 2 and ends at 11
      call get_nbrs_row(14,22,n14,nbr)
      call get_nbrs_row(26,33,n26,nbr)
      call get_nbrs_row(38,44,n38,nbr)
      nbr(4,43) = 202			!202 is a near neighbor of 43 and 44
      nbr(3,44) = 202
      nbr(8,32) = 202			!202 is a distant neighbor of 32

c Get nbrs of bottom frame...
      call get_nbrs_row(158,164,n158,nbr)
      call get_nbrs_row(169,176,n169,nbr)
      call get_nbrs_row(180,188,n180,nbr)
      call get_nbrs_row(191,200,n191,nbr)

c Get neighbors of left frame...
      call get_nbrs_col(24,167,n24,nbr)
      call get_nbrs_col(36,156,n36,nbr)
      call get_nbrs_col(48,153,n48,nbr)
      call get_nbrs_col(52,142,n52,nbr)

c Get neighbors of right frame...
      call get_nbrs_col(60,150,n60,nbr)
      call get_nbrs_col(49,154,n49,nbr)
      call get_nbrs_col(46,166,n46,nbr)
      call get_nbrs_col(35,178,n35,nbr)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of the interior reseau marks.

      subroutine get_nbrs_interior(nbr)
      implicit none
c Output...
      integer nbr(8,202)	!only the nbrs of the interior marks are filled

c Local variables...
      integer i
      integer k		!reseau mark index
      integer nk(8)	!neighbors of k

c The interior consists of the 7x7 grid of reseau marks at the center of the
c image.  To fill in the neighbors of all 49 marks, we need only know the
c neighbor of mark 53:
  
      integer n53(8)/0,0,0,0,52,54,38,68/	!interior has no near neighbors

c Initialize the neighbors of k...

      do i=1,8
         nk(i) = n53(i)
      enddo

c Fill the rows starting at 53,68,83,98,113,128, and 143...

      do k=53,143,15
         call get_nbrs_row(k,k+6,nk,nbr)
         do i=5,8
            nk(i) = nk(i) + 15
         enddo
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of 202.

      subroutine get_nbrs_202(nbr)
      implicit none
c Output...
      integer nbr(8,202)

c Local variables...
      integer i

      integer n202(8)/43,44,58,59,0,0,32,0/

      do i=1,8
         nbr(i,202) = n202(i)
      enddo

c Add 202 to list of its neighbors
      nbr(8,32) = 202                   !distant neighbor...
      nbr(4,43) = 202                   !near neighbors...
      nbr(3,44) = 202
      nbr(2,58) = 202
      nbr(1,59) = 202
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get the neighbors of the row starting with sk and ending with ek.

      subroutine get_nbrs_row(sk,ek,nsk,nbrs)
      implicit none
c Inputs...
      integer sk,ek	!starting k, ending k of row
      integer nsk(8)	!neighbors of sk
c Output...
      integer nbrs(8,202)

c Local variables...
      integer i,j
      integer k		!current mark
      integer nk(8)	!neighbors of k

      do i=1,8
         nk(i) = nsk(i)
      enddo

      do 20 k=sk,ek
      do i=1,8
         nbrs(i,k) = nk(i)
         if (nk(i).ne.0) nk(i)=nk(i)+1
      enddo
   20 continue
       
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get neighbors of column that begins with sk and ends with ek.

      subroutine get_nbrs_col(sk,ek,nsk,nbrs)
      implicit none
c Inputs...
      integer sk,ek	!starting k and ending k of column
      integer nsk(8)	!neighbors of sk
c Output...
      integer nbrs(8,202)

c Local variables...
      integer i
      integer k		!current mark on column
      integer nk(8)	!neighbors of k

      do i=1,8
         nk(i) = nsk(i)
      enddo

      k = sk

   10 continue
      do i=1,8
         nbrs(i,k) = nk(i)
      enddo

      if (k.ge.ek) return

      do i=1,8
         if (nk(i).ne.0) then
            if (nk(i).lt.34 .or. nk(i).gt.153) then
               nk(i) = nk(i) + 23
            else
               nk(i) = nk(i) + 15
            endif
         endif
      enddo
      if (k.lt.34 .or. k.gt.153) then
         k = k + 23
      else
         k = k + 15
      endif
      goto 10

      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_A_size.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get size of correlation template.

      subroutine get_A_size(ia,ja)
      implicit none
c Outputs...
      integer*4 ia,ja		!template half widths

c Local variables...
      integer*4 count
      integer*4 nsw,nlw		!correlation template is nlw x nsw
      character*80 msg
  101 format('nlw='i2,' nsw=',i2)

      call xvp('nlw',nlw,count)		!height of correlation template
      call xvp('nsw',nsw,count)		!width of correlation template
      write(msg,101) nlw,nsw
      call xvmessage(msg,0)

      ja = nlw/2 			!half widths of template
      ia = nsw/2
      if (nlw.eq.2*ja) call mabend('***nlw must be odd')
      if (nsw.eq.2*ia) call mabend('***nsw must be odd')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_A.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute reseau shape function A, where A is an inverted Gaussian:
c	A(i,j) = 255.*(1-exp(s*(i**2+j**2)))
c where
c	s = -1/(2*sig**2)
c i=-ia,...,ia and j=-ja,...,ja.

      subroutine compute_A(A,ia,ja)
      implicit none
c Inputs...
      integer ia,ja			!template half widths
c Output...
      integer*2 A(-ia:ia,-ja:ja)	!shape function

c Routine also sets these values in common...
      common/ca/area,meanA,sigA
      real area,meanA,sigA		!area, mean, and sigma of A

      common/cd/npixels,sii,sjj
      real npixels
      real sii,sjj			!sum of i**2 and j**2

c Local variables...
      real sig,s
      integer sumA,sumA2

      integer i,j,m,n,count
      logical xvptst
      character*80 msg
  100 format('sig=',f6.2)
  101 format(11i4)
  102 format(' meanA=',f6.2,' sigmaA=',f6.2)
  103 format('sii=',f10.2,' sjj=' f10.2)

c Compute the Gaussian constant s...
      call xvp('sigma',sig,count)
      write(msg,100) sig
      call xvmessage(msg,0)
      s = -1/(2*sig**2)	

c Compute A, mean(A) and sigma(A)...
      sumA = 0.
      sumA2 = 0.
      do j=-ja,ja
         do i=-ia,ia
            A(i,j) = 255*(1 - exp(s*(i**2+j**2)))
            sumA = sumA + A(i,j)
            sumA2 = sumA2 + A(i,j)**2
         enddo
      enddo

c Set the values in common/ca/...
      area = (2*ia+1)*(2*ja+1)
      meanA = sumA/area				!mean of A
      sigA = sqrt(sumA2/area - meanA**2)	!sigma of A
      npixels = area
      sii = (ia*(ia+1)*(2*ia+1)*(2*ja+1))/3.
      sjj = (ja*(ja+1)*(2*ja+1)*(2*ia+1))/3.

      if (xvptst('print')) then
         call xvmessage('reseau shape template A...',' ')
         do j=-ja,ja
            write(msg,101) (a(i,j),i=-ia,ia)
            call xvmessage(msg,' ')
         enddo
         write(msg,102) meanA,sigA
         call xvmessage(msg,' ')
ccc        write(msg,103) sii,sjj
ccc        call xvmessage(msg,0)
      endif

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_R_size.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get search area dimensions.

      subroutine get_R_size(hsR,hlR)
      implicit none
c Outputs...
      integer hsR,hlR		!half widths of R

      integer*4 count
      integer nver,nhor		!search area is nver x nhor
      character*80 msg
  101 format('search area is ',i2,' x ',i2)

      call xvp('nhor',nhor,count)	!width of search window
      call xvp('nver',nver,count)	!height of search window
      write(msg,101) nver,nhor
      call xvmessage(msg,0)
      hsR = nhor/2
      hlR = nver/2
      if (nhor.eq.2*hsR) call mabend('***nhor must be odd')
      if (nver.eq.2*hlR) call mabend('***nver must be odd')
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_window.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copy the pixel area centered at D(l,s) into window W.
c Pixels that lie outside the image are set to -999 DN.

      subroutine get_window(D,l,s,W,iw,jw)
      implicit none
c Inputs...
      integer l,s		!center of window is at D(l,s)
      byte D(800,800)	!d is nl x ns
      integer iw,jw		!window half-widths
c Output...
      integer*2 W(-iw:iw,-jw:jw)	!W(i,j) = d(s+i,l+j)

c Local variables...
      integer i,j
      integer nl,ns
      byte tbl(0:255)
      include 'fortport'

      do j=-jw,jw
         if (l+j.ge.1 .and. l+j.le.800) then
            do i=-iw,iw
               if (s+i.ge.1 .and. s+i.le.800) then
               W(i,j) = byte2int(D(s+i,l+j))	!convert from byte to i*2
               else
                  W(i,j) = -999
               endif
            enddo
         else
            do i=-iw,iw
               W(i,j) = -999
            enddo
         endif
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create check_window.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check the window W for data gaps, and replaces these gaps with -998 DN.

c The problem is that data gaps are zero filled and zero is also a
c valid DN value.  Therefore, the meaning of 0 DN is ambiguous.

c Any string of 7 or more consecutive zeroes, either vertical or horizontal, is
c considered a data gap because such an event rarely occurs naturally, and
c is more likely to be due to (1) data dropouts (in the transmission from
c Jupiter to Earth), (2) image data-compression line truncation, or (3)
c image editing.  Therefore, any such string is replaced by -998 DN.
 
c Additionally, windows along the left and right margins are checked for
c data-compression line truncation, no matter how short the gap may be.

      subroutine check_window(W,iw,jw,lk,sk,k,rtype,poe,gap)
      implicit none
c Inputs...
      integer iw,jw
      integer lk,sk		!W(0,0) = D(sk,lk)
      integer k
      integer rtype
      integer poe		!planet-of-encounter (J=5,S=6,U=7,N=8)
c Updated...
      integer*2 W(-iw:iw,-jw:jw)	!gaps set to -998
c Output...
      logical gap			!true if data gap exist
c Local variables...
      integer i,j,ii,jj,loop,dn
      integer n			!running count of strings of 0s
      integer switch		!0=even line, 1=odd line 
      integer line,samp
      integer type		!1=left margin, 2=right margin, 0=neither

      gap = .false.		!initialize the result

c Any horizontal or vertical string of 0s that is at least 7 pixels long is 
c considered a data gap.

c Since images taken at Uranus and Neptune suffer from data-compression
c line truncation, we allow for shorter horizontal strings based on
c the following rules:

c If the mark is on the left margin and the line is even, and if the string of
c 0s starts at sample 1, we assume that this is a result of data compression
c line truncation.

c If the mark is on the right margin and the string ends at sample 800, we
c assume truncation.  (In mode IMK, both odd and even lines may be truncated on
c the right)
 
c This algorithm is not foolproof, but assumes the most probable circumstances.

c To expedite the algorithm, we classify the marks into three types:
c   type=1 if mark is on the left margin
c   type=2 if mark is on the right margin
c   type=0 if neither is true

c type is derived from the more general rtype, where
c rtype=1,2,3,4 for the upper-left, upper-right, lower-left, and lower-
c right corners, =5,6,7,8 for the top, left, bottom, and right margins,
c and 0 everywhere else (the interior).

      type = 0
      if (rtype.eq.1 .or. rtype.eq.3 .or. rtype.eq.6) type=1
      if (rtype.eq.2 .or. rtype.eq.4 .or. rtype.eq.8) type=2

c For pre data compression encounters, we turn the type off
      if (poe.lt.7) type=0


c Search for horizontal strings of 0s...

c Before scanning line-by-line, we set the odd/even switch...
      line = lk - jw		!first line in W
      if (2*(line/2).eq.line) then
         switch = 0		!first line is even
      else
         switch = 1		!first line is odd
      endif


c Scan each line segment of W for strings of 0s... `

      do 30 j=-jw,jw
      n = 0			!reset the string count

      do 20 i=-iw,iw
      if (W(i,j).eq.0) then
         n = n + 1		!add 1 to the string
         goto 20
      endif
      if (n.eq.0) goto 20	!if no string, continue
      if (n.ge.7) goto 16	!if string is long enough, fill in the gap
      if (type.eq.1 .and. switch.eq.0) then
         samp = sk + i - n
         if (samp.eq.1) goto 16		!fill in the gap
      endif
      if (type.eq.2) then 
         samp = sk + i - 1
         if (samp.eq.800) goto 16	!fill in the gap
      endif
      n = 0			!if no gap detected, reset the string
      goto 20			!and continue 
   16 gap = .true.		!here if gap has been detected
      do ii=i-n,i-1
         W(ii,j) = -998		!change the 0s to -998s
      enddo
      n = 0
   20 continue

      if (n.ge.7) then		!flush out the last gap
         gap = .true.
         ii = iw		!the gap ends at the end of W
         do loop=1,n
            W(ii,j) = -998	!change the 0s to -998s
            ii = ii - 1
         enddo
      endif

   30 switch = 1 - switch	!flip the odd/even switch


c Scan every column segment of W for strings of 0s or -998s...

      do 60 i=-iw,iw
      j = -jw
      n = 0

      do 50 j=-jw,jw
      if (W(i,j).eq.0 .or. W(i,j).eq.-998) then
         n = n + 1
         goto 50
      endif
      if (n.eq.0) goto 50
      if (n.ge.7) then
         gap = .true.
         do jj=j-n,j-1
            W(i,jj) = -998
         enddo
      endif
      n = 0
   50 continue
   60 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create filter_B.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Filter B to remove the hot lines along the top edge of the image,
c and the hot columns along the left edge.

      subroutine filter_B(B,ib,jb,lk,sk,rtype)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!B(i,j) = d(sk+i,lk+j)
      integer lk,sk
      integer rtype

c Local variables...
      integer i,j

      if (rtype.eq.1 .or. rtype.eq.2 .or. rtype.eq.5) then
         call filter5(ib,jb,lk,sk,B)
      elseif (rtype.eq.3 .or. rtype.eq.6) then
         call filter6(ib,jb,lk,sk,B)
      endif

c Note: Using filter5 for rtype=1 is an incomplete solution.
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Filter a search area on the top edge.

      subroutine filter5(ib,jb,lk,sk,B)
      implicit none
c Inputs...
      integer ib,jb
      integer lk,sk
c Filtered output...
      integer*2 B(-ib:ib,-jb:jb)        !B(i,j) = d(sk+i,lk+j)
c Local variables...
      integer dn
      real sum,scale
      real*4 mean(4)		        !mean DNs of columns 1,2 and 3
      integer i,j,l,n

c Compute the mean values of lines 1 to 4...

      do 20 l=1,4
      mean(l) = -999
      j = l - lk
      if (j.lt.-jb .or. j.gt.jb) goto 20
      n = 0
      sum = 0

      do 10 i=-ib,ib
      dn = B(i,j)
      if (dn.lt.0) goto 10
      n = n + 1
      sum = sum + dn
   10 continue

      if (n.gt.0) mean(l)=sum/n
   20 continue

c Scale lines 1,2 and 3 so that their new means are the same as mean(4).

      do l=1,3
         if (mean(l).gt.0) then
            j = l - lk
            scale = mean(4)/mean(l)
            do i=-ib,ib
               dn = B(i,j) 
               if (dn.ge.0) B(i,j)=dn*scale
            enddo
         endif
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Filter a search area on the left edge.

      subroutine filter6(ib,jb,lk,sk,B)
      implicit none
c Inputs...
      integer ib,jb
      integer lk,sk
c Filtered output...
      integer*2 B(-ib:ib,-jb:jb)        !B(i,j) = d(sk+i,lk+j)
c Local variables...
      integer dn
      real sum,scale
      real*4 mean(3)		!mean DNs of columns 1,2 and 3
      integer s,i,j,n

c Compute the mean values of columns 1,2 and 3...

      do 20 s=1,3
      mean(s) = -999
      i = s - sk
      if (i.lt.-ib .or. i.gt.ib) goto 20
      n = 0
      sum = 0

      do 10 j=-jb,jb
      dn = B(i,j)
      if (dn.lt.0) goto 10
      n = n + 1
      sum = sum + dn
   10 continue

      if (n.gt.0) mean(s)=sum/n
   20 continue

c Scale columns 1 and 2 so that their new means are the same as mean(3).

      do s=1,2
         if (mean(s).gt.0) then
            i = s - sk
            scale = mean(3)/mean(s)
            do j=-jb,jb
               dn = B(i,j) 
               if (dn.ge.0) B(i,j)=dn*scale
            enddo
         endif
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_B.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print B.

      subroutine print_B(B,ib,jb,k)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!B(i,j) = d(s+i,l+j)
      integer k				!reseau mark number

c Local variables...
      integer i,j
      character*132 msg
  102 format('tres=',i3,' B=...')
  103 format(30i4)

      call xvmessage('.page.',0)
      write(msg,102) k
      call xvmessage(msg,0)

      do j=-jb,jb
         write(msg,103) (B(i,j),i=-ib,ib)
         call xvmessage(msg,0)
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_R_detrend.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the matrix R.
c This version detrends every search window W before computing rho(A,W).

      subroutine compute_R_detrend(A,ia,ja,B,ib,jb,R,ir,jr,
     &		W,rsw,csw)
      implicit none
c Inputs...
      integer ia,ja
      integer*2 A(-ia:ia,-ja:ja)	!nlw x nsw shape function.
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!search window centered at D(lnom,snom).
      integer ir,jr
c Scratch space...
      integer*2 W(-ia:ia,-ja:ja)
      integer rsw(-ja:ja)		!sums along each row of w
      integer csw(-ib:ib)		!sums along each column of B
c Output...
      real*4 R(-ir:ir,-jr:jr)		!rho for nver x nhor search area

c Template A mean and sigma...
      common/ca/area,meanA,sigA
      real area,meanA,sigA

c Local variables...
      integer ii,jj		!search window offsets
      integer i,j		!A(i,j) = B(i+ii,j+jj) 

      integer sw,sw2,saw
      real meanW,varW,covAW,sigW

      do 20 jj=-jr,jr		!row loop thru R
      do 10 ii=-ir,ir		!column loop thru R
      call detrend_window(B,ib,jb,ii,jj,W,ia,ja,rsw,csw)
      sw = 0
      sw2 = 0
      saw = 0
      do j=-ja,ja
         do i=-ia,ia
            sw = sw + w(i,j)
            sw2 = sw2 + w(i,j)**2
            saw = saw + a(i,j)*w(i,j)
         enddo
      enddo
      meanW = sw/area
      varW = sw2/area - meanW**2
      if (varW.gt.0) then	!do this if there is a signal
         covAW = saw/area - meanA*meanW
         sigW = sqrt(varW)
         R(ii,jj) = covAW/(sigA*sigW)
      else
         R(ii,jj) = -999	!no pulse, rho has no meaning.
      endif
   10 continue
   20 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create detrend_window.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Detrend the window centered at B(ii,jj) and output to W.

      subroutine detrend_window(B,ib,jb,ii,jj,W,ia,ja,rsw,csw)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!search area
      integer ia,ja
      integer ii,jj			!center window at B(ii,jj)  
c Output...
      integer*2 W(-ia:ia,-ja:ja)	!detrended window
c Scratch space...
      integer rsw(-ja:ja)	!row sums of w(i,j)
      integer csw(-ia:ia)	!column sums of w(i,j)

      common/cd/npixels,sii,sjj
      real npixels,sii,sjj

c Local variables...
      integer i,j		!w(i,j) = b(i+ii,j+jj)
      integer rsumw
      integer sumw,sumwi,sumwj
      real*8 c1,c2,c3		!plane constants


c Fit window W to a brightness plane D, where

c	d(i,j) = c1*i + c2*j + c3

c solving for the coefficients c1, c2, and c3 that minimize the
c least-squares error.

      sumw = 0
      do i=-ia,ia
         csw(i) = 0
      enddo

      do j=-ja,ja
         rsumw = 0
         do i=-ia,ia
            w(i,j) = b(i+ii,j+jj)	!copy window from B to W 
            rsumw = rsumw + w(i,j)
            csw(i) = csw(i) + w(i,j)
         enddo
         sumw = sumw + rsumw
         rsw(j) = rsumw
      enddo

      sumwi = 0
      do i=-ia,ia
         sumwi = sumwi + csw(i)*i
      enddo

      sumwj = 0
      do j=-ja,ja
         sumwj = sumwj + rsw(j)*j
      enddo

      c1 = sumwi/sii
      c2 = sumwj/sjj
      c3 = sumw/npixels


c Detrend the window...

      do j=-ja,ja
         do i=-ia,ia
            w(i,j) = w(i,j) - (c1*i+c2*j+c3)
         enddo
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_R_gap.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the correlation coefficient rho for every pixel in the search
c area and return in matrix R.

c This version of compute_R ignores pixels in data gaps or off the
c edge in the calculation of rho.

      subroutine compute_R_gap(A,ia,ja,B,ib,jb,R,ir,jr)
      implicit none
c Inputs...
      integer ia,ja
      integer*2 A(-ia:ia,-ja:ja)	!shape template.
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!search window centered at D(snom,lnom)
      integer ir,jr
c Output...
      real*4 R(-ir:ir,-jr:jr)		!rho for every pixel in search area

c Local variables...
      integer ii,jj		!window W is centered over B(ii,jj) 
      integer i,j
      integer aij		!aij = A(i,j)
      integer wij		!wij = B(i+ii,j+jj)
      real sa,sw,sa2,saw,sw2	!sums of A and W
      real meana,meanw		!mean of A and mean of W
      real vara,varw,covaw	!variance of A and W and covariance of A and W
      real*4 fract
      integer narea		!number of pixels in correlation window
      real n			!number of valid pixels in correlation window

      narea = (2*ia+1)*(2*ja+1)	!total number of pixels in template A
      fract = 0.5		!fraction of pixels required for computation


c Compute rho for every pixel in the search area...

      do 20 jj=-jr,jr		!search area row index
      do 10 ii=-ir,ir		!search area column index
      sa = 0
      sw = 0
      sa2 = 0
      saw = 0
      sw2 = 0
      n = 0

c     ...compute rho for window W centered at B(ii,jj)
      do j=-ja,ja		!shape template row index
         do i=-ia,ia		!shape template col index
            aij = A(i,j)	!pixel in A
            wij = B(i+ii,j+jj)	!corresponding pixel in W
            if (wij.ge.0) then	!do if the pixel is valid
               sa = sa + aij
               sw = sw + wij
               sa2 = sa2 + aij**2
               saw = saw +aij*wij
               sw2 = sw2 + wij**2
               n = n + 1
            endif
         enddo
      enddo


      R(ii,jj) = -999		!initialize as invalid
      if (n.lt.fract*narea) goto 10	!reject if too few valid pixels
      meanw = sw/n
      varw = sw2/n - meanw**2	!variance of B
      if (varw.le.0) goto 10	!if no pulse, rho has no meaning.
      meana = sa/n
      vara = sa2/n - meana**2	!variance of A
      covaw = saw/n - meana*meanw
      R(ii,jj) = covaw/sqrt(vara*varw)
   10 continue
   20 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_R.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print correlation matrix.

      subroutine print_R(R,ir,jr)
      implicit none
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)

      integer buf(-12:12)
      integer i,j
      character*100 msg
  101 format(8x,25i4)

      call xvmessage('Search area 100*rho...',' ')
      do j=-jr,jr
         do i=-ir,ir
            if (R(i,j).ge.-1) then
               buf(i) = 100*R(i,j)
            else
               buf(i) = -999
            endif
         enddo
         write(msg,101) (buf(i),i=-ir,ir)
         call xvmessage(msg,' ')
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create find_rmax.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find the 3 highest local maxima of R.
 
      subroutine find_rmax(R,ir,jr,rho,di,dj)
      implicit none
c Inputs...
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)	!correlation coefficient over search area
c Outputs...
      real*4 rho(3)	!First, second and third highest local maximum of R
      integer*4 di(3)	!(i,j) location of 3 maxima
      integer*4 dj(3)

c Local variables...
      integer i,j,k
c Function calls...
      logical local_max		!true if r(i,j) is a local maximum

c We do not allow pixels on the perimeter of the search area to be
c local maxima.  Therefore, we limit our search to pixels away from
c the edges...

      do 50 j=-jr+1,jr-1	!Loop thru interior of search area	
      do 40 i=-ir+1,ir-1
      if (.not.local_max(R,ir,jr,i,j)) goto 40	!skip if not a local max
      if (r(i,j).gt.rho(1)) then	!if r(i,j) is greater than 1,
         rho(3) = rho(2)		!replace 3 with 2
         di(3) = di(2)
         dj(3) = dj(2)
         rho(2) = rho(1)		!replace 2 with 1,
         di(2) = di(1)
         dj(2) = dj(1)
         rho(1) = r(i,j)		!and replace 1 with r(i,j)
         di(1) = i
         dj(1) = j
      elseif (r(i,j).gt.rho(2)) then	!If r(i,j) is greater than 2,
         rho(3) = rho(2)		!replace 3 with 2
         di(3) = di(2)
         dj(3) = dj(2)
         rho(2) = r(i,j)		!and replace 2 with r(i,j).
         di(2) = i
         dj(2) = j
      elseif (r(i,j).gt.rho(3)) then	!elsif r(i,j) is greater than 3,
         rho(3) = r(i,j)		!replace 3 with r(i,j).
         di(3) = i
         dj(3) = j
      endif
   40 continue
   50 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create local_max.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Test for local maxima.

      logical function local_max(R,ir,jr,i,j)
      implicit none
c Inputs...
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)   !correlation coefficient (rho) over search area
      integer i,j       !pixel of interest is R(i,j).

c Local variables
      integer ii,jj     !inner pixel loop
      real rho

c           R(i-1,j-1)  R(i,j-1)  R(i+1,j-1)
c           R(i-1,j)    R(i,j)    R(i+1,j)
c           R(i-1,j+1)  R(i,j+1)  R(i+1,j+1)

      local_max = .false.
      rho = R(i,j)
      do jj=-1,1
         do ii=-1,1
            if (ii.ne.0 .or. jj.ne.0) then
               if (rho.le.R(i+ii,j+jj)) return  !not local max
            endif
         enddo
      enddo

      local_max = .true.
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create interp_max.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fit the surface R to a parabola to obtain sub-pixel offsets (dx,dy)
c for the reseau mark coordinates. 
c
c Reference: "Quadratic interpolation of the surface maximum",
c Gary Yagi, JPL IOM April 10, 2005

c The 3 horizontal pixels R(i-1,j),R(i,j),R(i+1,j) are fit to a parabola
c and then solving for the maximum of the parabola.  This determines dx.

c The 3 vertical pixels R(i,j-1),R(i,j),R(i,j+1) are treated the same way
c to obtain dy.

c Because of the 3x3 window, do not call this routine for (i,j) along
c the margins of the search area.

      subroutine interp_max(R,ir,jr,i,j,noin,rho,dx,dy)
      implicit none
c Inputs...
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)		!corr coef over search area
      integer i,j			!R(i,j) is the integer maximum
      logical noin
c Outputs...
      real*4 rho			!interpolated at (dx,dy)
      real*4 dx,dy			!sub-pixel offsets


c Local variables...
      real a,b,c
      real rx,ry

      c = R(i,j)
      rx = c
      ry = c
      dx = 0
      dy = 0
      if (noin) return

c Fit R(i-1,j), R(i,j), R(i+1,j) to a parabola to solve for dx
      if (R(i-1,j).gt.-1. .and. R(i+1,j).gt.-1.) then
         a = R(i+1,j) - 2*R(i,j) + R(i-1,j)
         b = R(i+1,j)-R(i-1,j)
         if (abs(a).gt.0.0001) then
            dx = -b/(2*a)
            rx = a*dx**2 + b*dx + c
         endif
      endif

c Fit R(i,j-1), R(i,j), R(i,j+1) to a parabola to solve for dy
      if (R(i,j-1).gt.-1. .and. R(i,j+1).gt.-1.) then
         a = R(i,j+1) - 2*R(i,j) + R(i,j-1)
         b = R(i,j+1) - R(i,j-1)
         if (abs(a).gt.0.0001) then
            dy = -b/(2*a)
            ry = a*dy**2 + b*dy + c
         endif
      endif
      rho = (rx + ry)/2
      if (rho.gt.1) rho=1
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create get_W.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccc
c Copy pixels from B to W so that W(0,0)=B(i,j)...

      subroutine get_W(B,ib,jb,W,ia,ja,i,j)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)
      integer*4 i,j
      integer ia,ja
c Output...
      integer*2 W(-ia:ia,-ja:ja)
c Local variables...
      integer ii,jj

      do jj=-ja,ja
         do ii=-ia,ia
             W(ii,jj) = B(i+ii,j+jj)
         enddo
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_dn.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Do a surface interpolation W to compute its minimum value.
c Note: W(0,0) is assumed to be an integral minimum.

      subroutine compute_dn(W,ia,ja,dn)
      implicit none
c Inputs...
      integer*4 ia,ja		!half-window size of shape template
      integer*2 W(-ia:ia,-ja:ja)
c Output...
      real*4 dn			!surface interpolated min value of B

c Local variables...
      integer a,b,c		!because the W(i,j) are integers
      real x,y			!because we divide by 2.
      real dnx,dny		!horizontal and vertical minima
      integer i,j,dnij

c Interpolate mindn to sub-pixel accuracy...
c The calculations are centered at W(0,0) and involves only 5 pixels:
c                  W(0,-1)
c         W(-1,0)  W(0,0)   W(+1,0)
c                  W(0,+1)

c Find the horizontal minimum by fitting W(-1,0), W(0,0), W(+1,0)
c to a parabola...

      if (W(0,0).lt.0) goto 50	!no interpolation is possible

c Horizontal interpolation using W(-1,0), W(0,0) and W(1,0)...
      dnx = -999
      if (W(-1,0).lt.0 .or. W(1,0).lt.0) goto 30
      a = W(1,0) - 2*W(0,0) + W(-1,0)
      if (a.eq.0) goto 30	!skip if straight line
      b = W(1,0) - W(-1,0)
      c = W(0,0)
      x = -b/(2.*a)		!x value of minimum
      dnx = a*x**2 + b*x + c	!interpolated minimum dn

   30 continue

c Vertical interpolation using W(0,-1), W(0,0) and W(0,1)...
      dny = -999
      if (W(0,-1).lt.0 .or. W(0,1).lt.0) goto 40
      a = W(0,1) - 2*W(0,0) + W(0,-1)
      if (a.eq.0) goto 40
      b = W(0,1) - W(0,-1)
      c = W(0,0)
      y = -b/(2.*a)
      dny = a*y**2 + b*y + c

   40 continue
      dn = 256
      if (dnx.ne.-999) then
         if (dny.ne.-999) then
            dn = (dnx+dny)/2
         else
            dn = dnx
         endif
      else
         if (dny.ne.-999) dn=dny
      endif
      if (dn.lt.0) dn=0
      if (dn.eq.256) dn=W(0,0)	!Use center if both interpolations failed
      return


c Here if W(0,0) is invalid.  Set dn equal to minimum of 8 neighbors...

   50 continue
      dn = 256
      do j=-1,1
         do i=-1,1
            dnij = W(i,j)
            if (dnij.ge.0 .and. dnij.lt.dn) dn=dnij
         enddo
      enddo
      if (dn.ne.256) return	!return if dn has been set

c If all else fails, set dn equal to minimum of W...

      do j=-ja,ja
         do i=-ia,ia
            dnij = W(i,j)
            if (dnij.ge.0 .and. dnij.lt.dn) dn=dnij
         enddo
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_dedge.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccc
c Compute the mean dn of the pixels along the edges of the window...

      subroutine compute_dedge(W,ia,ja,dedge)
      implicit none
c Inputs...
      integer ia,ja
      integer*2 W(-ia:ia,-ja:ja)
c Output...
      real*4 dedge	!mean dn of the edge pixels
c Local variables...
      integer i,j
      integer dn,sumdn,n

      sumdn = 0
      n = 0

      do i=-ia,ia	!pixels along the top and bottom edges of W
         dn = W(i,-ja)			!dn along top edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
         dn = W(i,ja)			!dn along bottom edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
      enddo

      do j=-ja+1,ja-1	!pixels along the left and right edges of W
         dn = w(-ia,j)			!dn along left edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
         dn = W(ia,j)			!dn along right edge
         if (dn.ge.0) then
            sumdn = sumdn + dn
            n = n + 1
         endif
      enddo

      if (n.gt.0) then
         dedge = float(sumdn)/n
      else
         dedge = -999
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_offsets.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the mean offsets from the nominals.

      subroutine compute_offsets(l,s,lnom,snom,rtype,e,f)
      implicit none
c Inputs...
      real*4 l(3,202),s(3,202)
      real*4 lnom(202),snom(202)	
      integer rtype(202)

c Outputs...
      real e,f			!mean offsets from nominals

c Local variables...
      integer n
      real dl,ds
      real sumdl,sumds
      real err,maxerr,ethresh
      integer k,kmax
      logical reject(202)
ccc      character*80 msg
ccc  101 format('Reseau',i4,' not used in fit. eps=',
ccc     &	f5.1,' pixels')

      ethresh = 3	!set the distance error threshold to 3 pixels

c First, we reject all marks on the margins or that are not found
c from the computations...

      do k=1,202
         if (rtype(k).ne.0 .or.l(1,k).eq.-999) then
            reject(k) = .true.
         else
            reject(k) = .false.
         endif
      enddo

c Since the actual mark is usually the first candidate, we compute an
c estimate for e and f using the locations of the first candidates only..

      sumds = 0
      sumdl = 0
      n = 0

      do k=1,202
         if (.not.reject(k)) then
            sumdl = sumdl + (l(1,k) - lnom(k))	!line displacements
            sumds = sumds + (s(1,k) - snom(k))	!sample displacements
            n = n + 1
         endif
      enddo

      if (n.eq.0) goto 990		!punt on fourth down

c This is our best estimate for e and f so far...

      e = sumds/n
      f = sumdl/n

c We now iterate thru a loop, rejecting the mark furthest from its
c expected value and obtaining a better esimate for e and f...

c Top of elimination loop...

   20 continue

c Find the mark with the largest residue from the model...
c	s = snom + e
c	l = lnom + f

      maxerr = 0

      do 40 k=1,202
      if (reject(k)) goto 40		!skip rejects
      dl = l(1,k) - lnom(k) - f		!line displacement
      ds = s(1,k) - snom(k) - e		!sample displacement
      err = dl**2 + ds**2
      if (err.gt.maxerr) then
         maxerr = err
         kmax = k
      endif
   40 continue

      maxerr = sqrt(maxerr)

      if (maxerr.lt.ethresh) return	!return if error is less than threshold
      reject(kmax) = .true.		!reject kmax
ccc      write(msg,101) kmax,maxerr
ccc      call xvmessage(msg,0)
      n = n - 1
      if (n.eq.0) goto 990		!this should never happen.
      sumdl = sumdl - (l(1,kmax)-lnom(kmax))
      sumds = sumds - (s(1,kmax)-snom(kmax))
      e = sumds/n
      f = sumdl/n
      goto 20

c Punt onf fourth down...
  990 call xvmessage('Warning: error solving for e and f',0)
      call xvmessage('Values set to 0',0)
      e = 0
      f = 0
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_eps.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the residuals from the model:
c          s = snom + e
c          l = lnom + f

      subroutine compute_eps(l,s,lnom,snom,e,f,tres,eps)
      implicit none
c Inputs...
      real*4 l(3,202),s(3,202)		!3 best locations
      real*4 lnom(202),snom(202)		!nominal locations	
      real e,f
      integer*4 tres
c Outputs...
      real*4 eps(3,202)

c Local variables..
      integer i,k
      real dl,ds
      character*132 msg

      do 50 k=1,202
      do 40 i=1,3
      if (l(i,k).ne.-999) then
         dl = l(i,k) - lnom(k) - f
         ds = s(i,k) - snom(k) - e
         eps(i,k) = sqrt(dl**2+ds**2)
      else
         eps(i,k) = -999
      endif
   40 continue
   50 continue

      if (tres.eq.0) return
      k = tres
      write(msg,101) lnom(k),snom(k),e,f
      call xvmessage(msg,0)
      do i=1,3
         write(msg,102) i,l(i,k),s(i,k),eps(i,k)
         call xvmessage(msg,0)
      enddo
      return

  101 format('(lnom,snom)=(',f5.1,',',f5.1,') e=',f5.2,' f=',f5.2)
  102 format(i1,': (l,s)=(', f6.1,',',f6.1,') eps=',f6.1)
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_dc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccc
c Estimate the dn contribution of the camera at each reseau mark.

      subroutine compute_dc(l,s,dn,nbr,rtype,dc)
      implicit none
c Inputs...
      real*4 l(3,202),s(3,202)	!line-sample coordinates of candidates
      real*4 dn(3,202)		!dn values of candidates
      integer nbr(8,202)	!8 neighbors of each reseau mark
      integer rtype(202)	!mark type (corner,edge,interior)
c Output...
      real*4 dc(202)		!dn contribution from the camera
c Local variables...
      real*4 ldc(202),sdc(202)	!coordinates where dc was measured
      integer k,k1,k2,ik
      integer i,n,imin
      integer vnk(8)		!up to 8 valid neighbors of k
      real dnmin
      real*4 edck		!estimated value of dc at mark k
      real*4 dcthresh		
      character*80 msg

      dcthresh = 20.

c Set dc equal to the lowest DN among the three candidates...

      do k=1,202
         dnmin = 256
         do i=1,3		!find the min of the 3
            if (dn(i,k).ge.0 .and. dn(i,k).lt.dnmin) then
               dnmin = dn(i,k)
               imin = i
            endif
         enddo
         if (dnmin.lt.256) then
            dc(k) = dnmin
            ldc(k) = l(imin,k)
            sdc(k) = s(imin,k)
         endif
      enddo

c
c Detect and reject any bad values of dc(k)...
c
      do 50 k=1,202

c     ...Estimate what dc(k) should be if consistent with its neighbors.
c     ...First, collect all the valid neighbors of reseau mark k.
      n = 0				!number of valid neighbors

      do 40 i=1,8
      ik = nbr(i,k)			!ith neighbor of k
      if (ik.eq.0) goto 40		!reject if ik does not exist
      if (rtype(ik).ne.0) goto 40	!reject if ik is on the margin
      if (dc(ik).eq.-999) goto 40	!reject if dc(ik) is invalid
      n = n + 1	
      vnk(n) = ik			!add ik to list of valid neighbor
   40 continue

ccc      if (n.ne.0) then
ccc      write(msg,101) k,(vnk(i),i=1,n)
ccc  101 format('k=',i3,' vnk=',8i4)
ccc      call xvmessage(msg,0)
ccc      endif

      if (n.eq.0) then
	 edck = -999
      elseif (n.eq.1) then
         k1 = vnk(1)
         edck = dc(k1)			!if 1 valid neighbor, copy the value
      elseif (n.eq.2) then
         k1 = vnk(1)
         k2 = vnk(2)
         edck = 0.5*(dc(k1)+dc(k2))	!if 2 valid neighbors, take the mean
      elseif (n.eq.3) then
         call interp_3(k,vnk,sdc,ldc,dc,edck)	!if 3, use an affine fit
      elseif (n.ge.4) then
         call interp_4(k,vnk,sdc,ldc,dc,edck)	!if 4 or more, use bilinear fit
      endif
      if (edck.lt.0) edck=0		!this should never happen

      if (rtype(k).ne.0) then
         if (edck.lt.dc(k)) dc(k)=edck	!along margin, use lower value
      elseif (dc(k)-edck.gt.dcthresh) then
         dc(k) = edck	!elsewhere, replace if dc(k) much larger than estimate.
      endif
   50 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_q.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccct
c
c Compute composite match quality for all 3x202 candidates.

      subroutine compute_q(rho,dn,dedge,dc,eps,maxeps,tres,g,h,q)
      implicit none
c Inputs...
      real*4 rho(3,202)		!correlation coefficient
      real*4 dn(3,202)		!dn of reseau mark
      real*4 dedge(3,202)	!avg dn of pixels on the perimeter
      real*4 dc(202)		!estimate of dark current
      real*4 eps(3,202)		!distance from expected location
      real*4 maxeps		!maximum displacement	
      integer tres		!test reseau mark number
c Output...
      real*4 g(3,202)		!darkness measure
      real*4 h(3,202)		!closeness measure
      real*4 q(3,202)		!composite match quality
c Local variables...
      integer i,k
      real dnthresh

      dnthresh = 10.		!warning: arbitrary noise floor

      do 202 k=1,202

      do 10 i=1,3		!compute q for candidates 1,2,and 3
      if (rho(i,k).eq.-999) goto 10	!skip invalid values
      g(i,k) = 1 - (dn(i,k)-dc(k))/(dedge(i,k)-dc(k))
      h(i,k) = 1 - eps(i,k)/maxeps 
      if (dedge(i,k)-dc(k).gt.dnthresh) then
         q(i,k) = (rho(i,k)+g(i,k)+h(i,k))/3
      else
         q(i,k) = (rho(i,k)+h(i,k))/2	!ignore value of g
      endif
      if (k.eq.tres) then
         call ptres7(i,dc(k),dn(i,k),dedge(i,k),eps(i,k))
         call ptres8(rho(i,k),g(i,k),h(i,k),q(i,k))
      endif
   10 continue

  202 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create choose_best.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Choose candidate with best match quality...

      subroutine choose_best(q,choice,dbug)
      implicit none
c Input...
      real*4 q(3,202)
      logical dbug
c Output...
      integer choice(202)		!choice=1,2,or 3
c Local variables...
      integer i,k
      real*4 maxq
      integer imax
      character*80 msg
  101 format('Mark',i3,' Candidate chosen=',i1)


      do 20 k=1,202
      maxq = q(1,k)
      imax = 1
      do i=2,3
         if (q(i,k).gt.maxq) then
            maxq = q(i,k)
            imax = i
         endif
      enddo
      choice(k) = imax
      if (dbug .and. imax.ne.1) then
         write(msg,101) k,imax
        call xvmessage(msg,0)
      endif
   20 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create reject.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccc
c Allow user to reject the location of any mark.

      subroutine reject(cl,cs)
      implicit none
c Updated...
      real*4 cl(202),cs(202)
c Local variables...
      integer*4 i,k
      integer*4 ibuf(202),cnt

      call xvp('reject',ibuf,cnt)

      do i=1,cnt
         k = ibuf(i)
         cl(k) = -999
         cs(k) = -999
      enddo
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create check_rho.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccc
c Reject a reseau mark if rho is smaller than rthresh...

      subroutine check_rho(crho,cl,cs,dbug)
      implicit none
c Inputs...
      real*4 crho(202)
      logical dbug
c Updated...
      real*4 cl(202),cs(202)
c Local variables...
      integer k,cnt
      real rthresh
      character*80 msg
  100 format('Rho threshold=',f5.1)
  101 format('Mark',i3,' rho=',f6.3,' is too small')

      call xvp('rthresh',rthresh,cnt)
      write(msg,100) rthresh
      call xvmessage(msg,0)

      do 20 k=1,202
      if (cl(k).eq.-999) goto 20	!already rejected
      if (crho(k).lt.rthresh) then
         cl(k) = -999			!reject the measurement
         cs(k) = -999
         if (dbug) then
            write(msg,101) k,crho(k)
            call xvmessage(msg,0)
         endif
      endif
   20 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create check_q.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccc
c Check for low match quality...

      subroutine check_q(cq,cl,cs,dbug)
      implicit none
c Inputs...
      real*4 cq(202)
      logical dbug
c Updates...
      real*4 cl(202),cs(202)
c Local variables...
      integer k,count
      real qthresh
      character*80 msg
  100 format('q threshold=',f5.2)
  101 format('Mark',i3,' q=',f7.3,' is too small')

      call xvp('qthresh',qthresh,count)
      write(msg,100) qthresh
      call xvmessage(msg,0)

      do 20 k=1,202
      if (cl(k).eq.-999) goto 20
      if (cq(k).lt.qthresh) then
         cl(k) = -999
         cs(k) = -999
         if (dbug) then
            write(msg,101) k,cq(k)
            call xvmessage(msg,0)
         endif
      endif
   20 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create check_corners.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check the corner marks.

      subroutine check_corners(ceps,cl,cs)
      implicit none
c Input...
      real*4 ceps(202)
c Updated...
      real*4 cl(202),cs(202)

      if (cl(1).eq.cl(13) .and. cs(1).eq.cs(13)) then
         call xvmessage('mark 1 same as mark 13',0)
         if (ceps(1).gt.ceps(13)) then
            cl(1) = -999
            cs(1) = -999
            call xvmessage('mark 1 rejected',0)
         else
            cl(13) = -999
            cs(13) = -999
            call xvmessage('mark 13 rejected',0)
         endif
      endif

      if (cl(12).eq.cl(23) .and. cs(12).eq.cs(23)) then
         call xvmessage('mark 12 same as mark 23',0)
         if (ceps(12).gt.ceps(23)) then
            cl(12) = -999
            cs(12) = -999
            call xvmessage('mark 12 rejected',0)
         else
            cl(23) = -999
            cs(23) = -999
            call xvmessage('mark 23 rejected',0)
         endif
      endif

      if (cl(190).eq.cl(179) .and. cs(190).eq.cs(179)) then
         call xvmessage('mark 190 same as mark 179',0)
         if (ceps(179).gt.ceps(190)) then
            cl(179) = -999
            cs(179) = -999
            call xvmessage('mark 179 rejected',0)
         else
            cl(190) = -999
            cs(190) = -999
            call xvmessage('mark 190 rejected',0)
         endif
      endif

      if (cl(201).eq.cl(189) .and. cs(201).eq.cs(189)) then
         call xvmessage('mark 201 same as mark 189',0)
         if (ceps(189).gt.ceps(201)) then
            cl(189) = -999
            cs(189) = -999
            call xvmessage('mark 189 rejected',0)
         else
            cl(201) = -999
            cs(201) = -999
            call xvmessage('mark 201 rejected',0)
         endif
      endif

      return
      end

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create fill_res.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fill in all missing (l,s) coordinates. 
c
      subroutine fill_res(lnom,snom,nbr,rtype,e,f,l,s)
      implicit none
c Inputs...
      real*4 lnom(202),snom(202)  !nominal reseau coordinates
      integer nbr(8,202)        !8 neighbors of each reseau mark
      integer rtype(202)
      real*4 e,f		!global offsets
c Updated...
      real*4 l(202),s(202)
c Local variables...
      logical found(202)	!true if mark k was found
      integer vnk(8)		!valid neighbors of k
      integer i,n
      integer k,k1,k2,kn

c rtype is 1,2,3,4 for the UL,UR,LL and LR corners,
c 5,6,7,8 for the top,left,bottom and right margins,
c and 0 for the interior.

c Remembering which reseau marks were actually found...

      do k=1,202
         found(k) = l(k).ne.-999	!found if not invalid
      enddo

c Fill in the missing marks...

      do 50 k=1,202
      if (l(k).ne.-999) goto 50		!skip if it already exists
      n = 0				!number of valid neighbors

      do 40 i=1,8	!build list of valid neighbors of k...
      kn = nbr(i,k)                     !ith neighbor of k
      if (kn.eq.0) goto 40              !reject if kn does not exist
      if (rtype(kn).ne.0) goto 40       !reject if kn is on margin
      if (.not.found(kn)) goto 40       !reject if kn was not found
      n = n + 1 
      vnk(n) = kn                       !add kn to list of valid neighbor
   40 continue

      if (n.eq.0) then		!if no valid neighbors, use global offsets
         l(k) = lnom(k) + f
         s(k) = snom(k) + e
      elseif (n.eq.1) then	!use the same offsets
         k1 = vnk(1)
         l(k) = lnom(k) + (l(k1)- lnom(k1))
         s(k) = snom(k) + (s(k1)- snom(k1))
      elseif (n.eq.2) then	!use mean offsets
         k1 = vnk(1)
         k2 = vnk(2)
         l(k) = lnom(k) + 0.5*((l(k1)-lnom(k1))+(l(k2)-lnom(k2)))
         s(k) = snom(k) + 0.5*((s(k1)-snom(k1))+(s(k2)-snom(k2)))
      elseif (n.eq.3) then	!interpolate using an affine transformation 
         call interp_3(k,vnk,lnom,snom,l,l(k))
         call interp_3(k,vnk,lnom,snom,s,s(k))
      elseif (n.ge.4) then	!interpolate using a bilinear transformation
         call interp_4(k,vnk,lnom,snom,l,l(k))
         call interp_4(k,vnk,lnom,snom,s,s(k))
      endif
   50 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create interp_3.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccc
c Interpolate over the kth element of z by using its 3 valid neighbors.

c Examples of use:
c         call interp_3(k,vnk,snom,lnom,l)
c         call interp_3(k,vnk,snom,lnom,s)
c         call interp_3(k,vnk,sdc,ldc,dc)

      subroutine interp_3(k,vnk,x,y,z,ezk)
      implicit none
c Inputs...
      integer k 		!mark to be filled
      integer vnk(3)		!valid neighbors of k
      real*4 x(202),y(202),z(202)
c Output...
      real*4 ezk		!estimated value of z(k)

c Local variables...
      real*4 a(3,3),b(3)
      integer i,ki,ind

c Model the behavior of z in the local image region as an affine transformation 
c using its 3 neighbors as data points.

      do i=1,3
         ki = vnk(i)
         a(i,1) = 1
         a(i,2) = x(ki)
         a(i,3) = y(ki)
         b(i) = z(ki)
      enddo

c     ...Solve the simultaneous equation Ac = b
      call simq(a,b,3,ind)	!note: c is returned in b
      if (ind.eq.0) then
         ezk = b(1) + b(2)*x(k) + b(3)*y(k)
      else
         call prnt(2,1,k,'***simq err at mark.')
         ezk = -999
      endif

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create interp_4.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccc
c Interpolate over the kth element of z by by using its 4 valid neighbors.

      subroutine interp_4(k,vnk,x,y,z,ezk)
      implicit none
c Inputs...
      integer k 		!mark to be filled
      integer vnk(4)		!valid neighbors of k
      real*4 x(202),y(202),z(202)
c Output...
      real*4 ezk		!estimated value of z(k)

c Local variables...
      real*4 a(4,4),b(4)
      integer i,ki,ind

c Model the behavior of z in the local image region as a bilinear
c transfomation using its four neighbors as data points.

      do i=1,4
         ki = vnk(i)
         a(i,1) = 1
         a(i,2) = x(ki)
         a(i,3) = y(ki)
         a(i,4) = x(ki)*y(ki)
         b(i) = z(ki)
      enddo

      call simq(a,b,4,ind)
      if (ind.eq.0) then
         ezk = b(1) + b(2)*x(k) + b(3)*y(k) 
     &		+ b(4)*x(k)*y(k)
      else
         call prnt(2,1,k,'***simq err at mark.')
         ezk = -999
      endif

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create compute_geo.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccc
c create geoma parameters...

      subroutine compute_geo(res,os_res,geo)
      implicit none
c inputs...
      real*4 res(2,202)		!image space coordinates
      real*4 os_res(2,202)	!object space coordinates
c output...
      real*4 geo(4,24,23)	!quad grid in geoma format
c local variables..
      real*4 is(2,13,23)	!IS coordinates of triangular grid
      real*4 os(2,13,23)	!OS coordinates of triangular grid
      integer row
      logical xvptst

      do row=1,23
         call interg(res,os_res,row,is(1,1,row),os(1,1,row))
         call triag(row,is(1,1,row),os(1,1,row),geo(1,1,row))
      enddo

      if (xvptst('pgeo')) call print_rows(is,os)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create interg.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccc
c Return a complete row of triangular grid by copying existing values
c and filling in the gaps.
c From Jean Lorre's subroutine interg.

      subroutine interg(res,os_res,row,is,os)
      implicit none
c Inputs...
      real*4 res(2,202),os_res(2,202)
      integer row
c Outputs...
      real*4 is(2,13),os(2,13)	!IS and OS (line,samp) for entire row.
c Local variables...
      real*4 a(16),b(4)
      integer k,k1,k2,k3,k4
      integer i,j,ind
c     ...left-most and right-most reseau mark on each row.
      integer*2 ltabl(23)/1,13,24,36,47,51,62,66,77,81,92,96,107,111,
     *  122,126,137,141,152,156,167,179,190/
      integer*2 rtabl(23)/12,23,35,46,50,61,65,76,80,91,95,106,110,
     *  121,125,136,140,151,155,166,178,189,201/

      if (row.ne.2*(row/2)) goto 20	!branch if row is odd

c Here if row is even...

c Even rows all begin and end with an x, with no gaps in between.

c   1     2     3     4     5     6     7     8     9    10    11    12
c   x  13    14    15    16    17    18    19    20    21    22    23 x
c  24    25    26    27    28    29    30    31    32    33    34    35

c Copy all 11 existing coordinates to outputs (e.g. 13 to 23).

      k = ltabl(row)		!left-most reseau mark on row
      do j=2,12
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo

c Compute pseudo tiepoint x on the left side of the row by fitting
c to the 3 closest neighbors...
c             k1
c             x  k2
c             k3
      k1 = ltabl(row-1)		!left-most mark on row above
      k2 = ltabl(row)		!left-most mark on row
      k3 = ltabl(row+1)		!left-most mark on row below

c In object space, place x midway between k1 and k2...

      os(1,1) = (os_res(1,k1)+os_res(1,k3))/2	!OS line
      os(2,1) = (os_res(2,k1)+os_res(2,k3))/2	!OS samp

c Compute corresponding image space cooordinates is(1,1) and is(2,1) by
c fitting marks k, k1, and k2 to an affine transformation...
      
c   is_line = c0 + c1*os_samp + c2*os_line
c   is_samp = d0 + d1*os_samp + d2*os_line

c Note: matrix A is destroyed by simq, and C is returned in B.

      do 5 i=1,2
      a(1) = 1
      a(2) = 1
      a(3) = 1
      a(4) = os_res(2,k1)	!os samp coordinate of mark k1
      a(5) = os_res(2,k2)	!os samp coordinate of mark k
      a(6) = os_res(2,k3)	!os samp coordinate of mark k2
      a(7) = os_res(1,k1)	!os line coordinate for mark k1
      a(8) = os_res(1,k2)	!os line coordinate for mark k
      a(9) = os_res(1,k3)	!os line coordinate for mark k2
      b(1) = res(i,k1)
      b(2) = res(i,k2)
      b(3) = res(i,k3)
      call simq(a,b,3,ind)
      if (ind.ne.0) call prnt(2,1,k,'***simq err at mark.')
      is(i,1) = b(1) + b(2)*os(2,1) + b(3)*os(1,1)
    5 continue

c Compute pseudo tiepoint on right side of row...
      k1 = rtabl(row-1)		!right-most mark of row above
      k2 = rtabl(row)		!right-most mark on row
      k3 = rtabl(row+1)		!right-most mark on row below

      os(1,13) = (os_res(1,k1)+os_res(1,k3))/2	!os line
      os(2,13) = (os_res(2,k1)+os_res(2,k3))/2	!os samp

      do 8 i=1,2
      a(1) = 1
      a(2) = 1
      a(3) = 1
      a(4) = os_res(2,k1)
      a(5) = os_res(2,k2)
      a(6) = os_res(2,k3)
      a(7) = os_res(1,k1)
      a(8) = os_res(1,k2)
      a(9) = os_res(1,k3)
      b(1) = res(i,k1)
      b(2) = res(i,k2)
      b(3) = res(i,k3)
      call simq(a,b,3,ind)
      if (ind.ne.0) call prnt(2,1,k2,'***simq err at mark.')
      is(i,13) = b(1) + b(2)*os(2,13) + b(3)*os(1,13)
    8 continue
      return

c Here if row is odd...
   20 continue
      if (row.gt.3 .and. row.lt.21) goto 30

c Here if row is 1,3,21 or 23.  For these rows, all 12 coordinates exits:
c   1     2     3     4     5     6     7     8     9    10    11    12

c Copy coordinates to output row...
      k = ltabl(row)		!left-most reseau mark on row
      do j=1,12
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo
      return

   30 continue
c For rows 7,...,19, only the first 2 and last 2 marks exist...
c     51    52    53    54    55    56    57    58    59    60    61
c  62    63    x     x     x     x     x     x     x     x     64    65
c     66    67    68    69    70    71    72    73    74    75    76
c Row 5 is similar, except for the existance of mark 202.

c Copy these four marks to output buffers:
      k = ltabl(row)	!left-most mark
      do j=1,2
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo

      k = rtabl(row) - 1
      do j=11,12
         is(1,j) = res(1,k)
         is(2,j) = res(2,k)
         os(1,j) = os_res(1,k)
         os(2,j) = os_res(2,k)
         k = k + 1
      enddo
         
c Fill each x in the middle of the row by fitting its 4 neighbors to a 
c bilinear transformation: 

c         k1     k2
c             x
c         k3    k4

      k1 = ltabl(row-1) + 1
      k2 = k1 + 1
      k3 = ltabl(row+1) + 1
      k4 = k3 + 1

      do 40 j=3,10
      os(1,j)=(os_res(1,k1)+os_res(1,k2)+os_res(1,k3)+os_res(1,k4))/4. 
      os(2,j)=(os_res(2,k1)+os_res(2,k2)+os_res(2,k3)+os_res(2,k4))/4.
      do 38 i=1,2
      a(1) = 1.0
      a(2) = 1.0
      a(3) = 1.0
      a(4) = 1.0
      a(5) = os_res(2,k1)
      a(6) = os_res(2,k2)
      a(7) = os_res(2,k3)
      a(8) = os_res(2,k4)
      a(9) = os_res(1,k1)
      a(10) = os_res(1,k2)
      a(11) = os_res(1,k3)
      a(12) = os_res(1,k4)
      a(13) = a(5)*a(9)
      a(14) = a(6)*a(10)
      a(15) = a(7)*a(11)
      a(16) = a(8)*a(12)
      b(1) = res(i,k1)
      b(2) = res(i,k2)
      b(3) = res(i,k3)
      b(4) = res(i,k4)
      call simq(a,b,4,ind)
      if (ind.ne.0) call prnt(2,1,k1,'***simq err at mark.')
      is(i,j) = b(1) + b(2)*os(2,j) + b(3)*os(1,j)
     &	  	+ b(4)*os(2,j)*os(1,j)
   38 continue
      k1 = k1 + 1
      k2 = k2 + 1
      k3 = k3 + 1
      k4 = k4 + 1
   40 continue

c Restore reseau mark 202 which is inadvertently filled by this algorithm...

      if (row.eq.24) then
         is(1,9) = res(1,202)
         is(2,9) = res(2,202)
         os(1,9) = os_res(1,202)
         os(2,9) = os_res(2,202)
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create triag.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Assemble complete row of quadrilateral grid and pack in geoma format.
c From Jean Lorre's subroutine triag.
     
      subroutine triag(row,is,os,geo)
      implicit none
c Inputs...
      integer row
      real*4 is(2,13),os(2,13)
c Output...
      real*4 geo(4,24)
c Local variables...
      integer i,j,k,m,n

c The inputs have 13 tiepoints if the row is odd and 12 if even.
c The output always has 24 tiepoints using the follow rules:
c   If the row is odd, all 12 positions are replicated.
c   If the row is even, only the 11 internal positions are replicated. 

      k = 0
      m = 0
      if (row.eq.2*(row/2)) m=1

      do j=1,12
         do i=1,2
            k = k + 1
            m = m + 1
            n = (m + 1)/2
            geo(1,k) = os(1,n)
            geo(2,k) = os(2,n)
            geo(3,k) = is(1,n)
            geo(4,k) = is(2,n)
         enddo
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create write_res.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Write res to IBIS file (out=res) 

      subroutine write_res(res,ids)
      implicit none
c Inputs...
      real res(2,202)	!reseau locations
      integer*4 ids(5)	!frame,camera,filter,year,day

      integer runit,ibis	!VICAR and IBIS	unit numbers for res
      integer nrows/1/		!res is an IBIS file of 1 row
      integer ind
      character*80 msg
      character*6 format(409) /5*'full',404*'real'/

c     ...open runit as a VICAR file to write history label
      call xvunit(runit,'out',1,ind,0)		!out=(res,geo)
      call xvopen(runit,ind,'op','write','open_act','sa',
     &	 'io_act','sa',' ')
      call xladd(runit,'history','title','**RESLOC COORDINATES**',
     .	 ind,'format','string',0)
      call xvclose(runit,ind,0 )

c     ...open res as an IBIS file to write res
      call xvunit(runit,'out',1,ind,0)
      call ibis_file_open(runit,ibis,'write',409,nrows,format,0,ind)
      if (ind.ne.1) call ibis_signal_u(runit,ind,1)
      call putlocv2(runit,ibis,nrows,ids,res)	!write the record
      call ibis_file_close(ibis,' ',ind)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create write_geo.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Output geo...

      subroutine write_geo(geo)
      implicit none
c Inputs...
      real geo(4,24,23)		!geomtric transformation parameters
c Local variables...
      integer*4 icam		!camera serial number
      integer ind,gunit
      character*6 format(409)/5*'full',404*'real'/

c     ...open geo as VICAR file to write history label
      call xvunit(gunit,'out',2,ind,0)	!get VICAR unit number for geo
      call xvopen(gunit,ind,'op','write',0)
      call xladd(gunit,'history', 'title',
     .	 '**RESLOC GEOMA PARAMETERS**',ind,'format','string',0)
      call xvclose(gunit,ind,0)

c     ...open geo as an IBIS file to write record
      call xvunit(gunit,'out',2,ind,0)
      call iwrite_tiepoints(gunit,23,22,0,geo,4)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ptres1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccc
c Print the inputs...
      subroutine ptres1(hm1,h0,h1,vm1,v0,v1)
      implicit none
      integer*2 hm1,h0,h1
      integer*2 vm1,v0,v1
      character*80 msg
  101 format(3i5)

      call xvmessage('horizontal pixels...',0)
      write(msg,101) hm1,h0,h1
      call xvmessage(msg,0)
      
      call xvmessage('vertical pixels...',0)
      write(msg,101) vm1,v0,v1
      call xvmessage(msg,0)

      return
      end
      
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ptres5.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine ptres5(i,di,dj,dx,dy,k,rho,dn,dedge)
      implicit none
c Inputs...
      integer*4 i	!candidate 1,2 or 3
      integer*4 k	!reseau mark index
      integer*4 di(3),dj(3)
      real*4 dx(3),dy(3)
      real*4 rho(3,202),dn(3,202),dedge(3,202)
c Local variables...
      character*80 msg
  102 format(i1,': (di,dj)=(',i2,',',i2,') (dx,dy)=(',f5.1,',',f5.1,
     &	') rho=',f5.3,' dn=',f5.1,' dedge=',f5.1)

      write(msg,102) i,di(i),dj(i),dx(i),dy(i),
     &	rho(i,k),dn(i,k),dedge(i,k)
      call xvmessage(msg,0)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ptres7.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine ptres7(i,dc,dn,dedge,eps)
      implicit none
c Inputs...
      integer*4 i	!candidate 1,2 or 3
      real*4 dc,dn,dedge,eps
c Local variables...
      character*80 msg
  101 format('DN contribution of camera: dc=',f7.2)
  102 format(i1,' dn=',f7.2,' dedge=',f7.2,' eps=',f7.2)

      if (i.eq.1) then
         write(msg,101) dc
         call xvmessage(msg,0)
      endif
      write(msg,102) i,dn,dedge,eps
      call xvmessage(msg,0)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ptres8.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine ptres8(f,g,h,q)
      implicit none
c Inputs...
      real*4 f,g,h,q
c Local variables...
      character*80 msg
  102 format(' rho=',f8.3,' g=',f8.3,' h=',f8.3,' q=',f8.3)

      write(msg,102) f,g,h,q
      call xvmessage(msg,0)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_window.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print window.

      subroutine print_window(W,iw,jw)
      implicit none
      integer iw,jw
      integer*2 W(-iw:iw,-jw:jw)


      integer i,j
      character*132 msg		!room for 33 4-digit integers
  101 format(25i4)

      do j=-jw,jw
         write(msg,101) (W(i,j),i=-iw,iw)
         call xvmessage(msg,0)
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_stats.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccc
c Print the statistics for each reseau mark in reseau grid format....

      subroutine print_stats(cdn,dc,cdedge,ceps,crho,cg,ch,cq)
      implicit none
c Inputs...
      real*4 cdn(202),dc(202),cdedge(202),ceps(202)
      real*4 crho(202),cg(202),ch(202),cq(202)

      call xvmessage('.page',0)
      call xvmessage('dn...',0)
      call p_stats(cdn,1.)

      call xvmessage('.page',0)
      call xvmessage('dc...',0)
      call p_stats(dc,1.)

      call xvmessage('.page',0)
      call xvmessage('dedge...',0)
      call p_stats(cdedge,1.)

      call xvmessage('.page',0)
      call xvmessage('10*eps...',0)
      call p_stats(ceps,10.)

      call xvmessage('.page',0)
      call xvmessage('100*rho...',0)
      call p_stats(crho,100.)

      call xvmessage('.page',0)
      call xvmessage('100*g...',0)
      call p_stats(cg,100.)

      call xvmessage('.page',0)
      call xvmessage('100*h...',0)
      call p_stats(ch,100.)

      call xvmessage('.page',0)
      call xvmessage('100*q...',0)
      call p_stats(cq,100.)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create p_stats.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print the array v(202) in reseau grid format.

      subroutine p_stats(v,scale)
      implicit none
c Inputs...
      real*4 v(202)		!v may be rho,sigma,dn,dc, or eps
      real*4 scale		!1,10,or 100
c Local variables...
      integer k			!mark index
      integer loop,i,ival,ichar
      character*132 msg

c This loop prints two consecutive rows of reseau marks so that after
c completing the 12th loop, all 202 values are printed in reseau grid
c format. Since there are only 23 rows in the grid, only one row is printed
c on the last looop.

c We start at the top...
      k = 1				!reseau mark 1

      do 100 loop=1,12	

c     ...Print the first row.
c     ...This row starts at mark 1,24,47,62,77,92,107,122,137,152,167,or 190

      do ichar=1,132
         msg(ichar:ichar) = ' '		!blank out the message
      enddo

      ichar = 6
      if (loop.le.2 .or. loop.ge.11) then
c					!Rows that start at 1,24,167,190
         do i=1,12			!have 12 marks.
            if (v(k).eq.-999) then
               ival = -99
            else
               ival = scale*v(k) + 0.5
            endif
            write (msg(ichar-5:ichar),'(i6)') ival
            k = k + 1
            ichar = ichar + 6
         enddo
      else				!All other rows have 4 marks.
         do i=1,12
            if (i.le.2 .or. i.ge.11) then	!left and right two marks.
               if (v(k).eq.-999) then
                  ival = -99
               else
                  ival = scale*v(k) + 0.5
               endif
               write (msg(ichar-5:ichar),'(i6)') ival
               k = k + 1
            endif
            ichar = ichar + 6
         enddo
         if (loop.eq.3) then	!mark 202 is on row 3
            if (v(202).eq.-999) then
               ival = -99
            else
               ival = scale*v(202) + 0.5
            endif
            ichar = 54
            write (msg(ichar-5:ichar),'(i6)') ival
         endif
      endif
      call xvmessage(msg(2:132),' ')

c     ...Print the second row.
c     ...This row starts at mark 13,36,51,66,81,96,111,126,141,156,or 179

      if (loop.eq.12) goto 100		!the last loop has no second row.

      do ichar=1,132
         msg(ichar:ichar) = ' '		!blank out the print line
      enddo

      ichar = 9
      do i=1,11
         if (v(k).eq.-999) then
            ival = -99
         else
            ival = scale*v(k) + 0.5
         endif
         write (msg(ichar-5:ichar),'(i6)') ival
         k = k + 1
         ichar = ichar + 6
      enddo
      call xvmessage(msg(2:132),' ')

  100 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create p_avg_stats.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine p_avg_stats(ceps,crho,cq,cl)
      implicit none
c Inputs...
      real*4 ceps(202),crho(202),cq(202),cl(202)
c Local variables...
      integer k,n
      real esum,rsum,qsum
      real eavg,ravg,qavg
      character*80 msg

      n = 0
      esum = 0
      rsum = 0
      qsum = 0
      do k=1,202
         if (cl(k).ne.-999) then
            n = n + 1
            esum = esum + ceps(k)
            rsum = rsum + crho(k)
            qsum = qsum + cq(k)
         endif
      enddo
      if (n.gt.0) then 
         eavg = esum/n
         ravg = rsum/n
         qavg = qsum/n
      else
         eavg = -999
         ravg = -999
         qavg = -999
      endif

      write(msg,101) eavg
  101 format('average eps=',f8.3)
      call xvmessage(msg,0) 

      write(msg,102) ravg
  102 format('average rho=',f8.3)
      call xvmessage(msg,0) 

      write(msg,103) qavg
  103 format('average q=',f8.3)
      call xvmessage(msg,0) 

      write(msg,109) 202-n
  109 format(i3,' reseau marks not found')
      call xvmessage(msg,0)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_res.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccc
c Print output res...

      subroutine print_res(res)
      implicit none
c Inputs...
      real*4 res(2,202)

      call xvmessage('.page',0)
      call xvmessage('Output reseau locations...',0)
      call pmjs(res,1)
      return
      end

ccccccccccccccccccccccccccccccccccccccc
c Print nominals...

      subroutine print_nom(nom)
      implicit none
c Inputs...
      real*4 nom(2,202)

      call xvmessage('.page',0)
      call xvmessage('Nominal locations...',0)
      call pmjs(nom,1)
      return
      end

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pmjs.f
$ DECK/DOLLARS="$ VOKAGLEVE"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print out the Voyager reseau locations for an image in a format that
c  simulates the positioning of the reseau marks in Voyager images.

      subroutine pmjs(u,n)
      implicit none
      integer n			!1 if u is real*4, 2 if u is real*8
      real*4 u(n,2,202)

      integer i,j,k,ii,jj
      character*132 rbuf
      character*132 qbuf
      character*132 pbuf

      j = 1

      do 10 ii=1,12

      do jj=1,132
         pbuf(jj:jj) = ' '
         qbuf(jj:jj) = ' '
         rbuf(jj:jj) = ' '
      enddo

      k = 7

      if (ii.le.2. .or. ii.ge.11) then
         do i=1,12
            write (rbuf(k-2-2:k-2),'(i3)') j
            write (pbuf(k-5:k),'(f6.1)') u(1,1,j)
            write (qbuf(k-5:k),'(f6.1)') u(1,2,j)
            j = j + 1
            k = k + 11
         enddo
      else
         do i=1,12
            if (i.le.2 .or. i.ge.11) then
               write (rbuf(k-2-2:k-2),'(i3)') j
               write (pbuf(k-5:k),'(f6.1)') u(1,1,j)
               write (qbuf(k-5:k),'(f6.1)') u(1,2,j)
               j = j + 1
            endif
            k = k + 11
         enddo
         if (ii.eq.3) then
            write (rbuf(91:93),'(i3)') 202
            write (pbuf(90:95),'(f6.1)') u(1,1,202)
            write (qbuf(90:95),'(f6.1)') u(1,2,202)
         endif
      endif

      call xvmessage(rbuf(2:132),' ')
      call xvmessage(pbuf(2:132),' ')
      call xvmessage(qbuf(2:132),' ')

      if (ii.lt.12) then
         do jj=1,132
            pbuf(jj:jj) = ' '
            qbuf(jj:jj) = ' '
            rbuf(jj:jj) = ' '
	 enddo
         k = 13

         do i=1,11
            write (rbuf(k-2-2:k-2),'(i3)') j
            write (pbuf(k-5:k),'(f6.1)') u(1,1,j)
            write (qbuf(k-5:k),'(f6.1)') u(1,2,j)
            j = j + 1
            k = k + 11
         enddo

         call xvmessage(rbuf(2:132),' ')
         call xvmessage(pbuf(2:132),' ')
         call xvmessage(qbuf(2:132),' ')
      endif
   10 continue

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create print_rows.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccc
c Print the rows output by fill_gaps...

      subroutine print_rows(is,os)
      implicit none
c inputs...
      real*4 is(2,13,23)	!IS coordinates with gaps filled
      real*4 os(2,13,23)	!OS coordinates with gaps filled
c local variables..
      integer row
      integer i,j
      logical odd
      character*132 msg
  101 format(3x,12f6.1)
  102 format(13f6.1)

      do i=1,2
         if (i.eq.1) then
            call xvmessage('Image space line coordinates...',0)
         else
            call xvmessage('Image space sample coordinates...',0)
         endif
         do row=1,23
            odd = .not.row.eq.2*(row/2)
            if (odd) then
               write(msg,101) (is(i,j,row),j=1,12)
               call xvmessage(msg,0)
            else
               write(msg,102) (is(i,j,row),j=1,13)
               call xvmessage(msg,0)
            endif
         enddo
      enddo

      do i=1,2
         if (i.eq.1) then
            call xvmessage('Object space line coordinates...',0)
         else
            call xvmessage('Object space sample coordinates...',0)
         endif
         do row=1,23
            odd = .not.row.eq.2*(row/2)
            if (odd) then
               write(msg,101) (os(i,j,row),j=1,12)
               call xvmessage(msg,0)
            else
               write(msg,102) (os(i,j,row),j=1,13)
               call xvmessage(msg,0)
            endif
         enddo
      enddo

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create display_results.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c Display and print results.

      subroutine display_results(D,res,nom)
      implicit none
c Inputs...
      byte D(800,800)
      real*4 res(2,202)		!reseau coordinates found in D
      real*4 nom(2,202)		!nominal reseau coordinates

c Local variables...
      integer cnt
      real*4 dif(2,202)		!dif = res - nom
      real*4 dif2(2,202)	!dif2 = res - nom - mean
      character*132 file
      logical xvptst

c Overlay res and nom over the image...
      call xvp('ores',file,cnt)
      if (cnt.eq.1) call overlay(D,res,file)
      call xvp('onom',file,cnt)
      if (cnt.eq.1) call overlay(D,nom,file)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create overlay.f
$ DECK/DOLLARS="$ VOKAGLEVE"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copy input image to ounit, overlaying it with reseau locations...
c Overlay resau locations on D, outputting the results to the filename
c specified by param. 

      subroutine overlay(D,res,file)
      implicit none
c Inputs...
      byte D(800,800)
      real*4 res(2,202)
      character*132 file

c Local variables...
      byte buf(825)		!output image line
      integer ounit		!VICAR logical unit number for output
      integer ires
      integer line,l,s
      integer n,dn,ind,cnt
!!!      byte black/0/,white/zff/
      byte black/0/,white/-1/
      character*3 msg
  101 format(i3)

c Given file, get ounit and open it...
      call xvunit(ounit,'scr',1,ind,'u_name',file,' ')
      call xvopen(ounit,ind,'op','write',
     &          'open_act','sa','io_act','sa',' ')

c Create the overlay one line at a time...

      do 50 line=1,800

      do s=1,800
         buf(s) = D(s,line)
      enddo

      do 40 ires=1,202
      l = res(1,ires)
      s = res(2,ires)
      if (line.lt.l .or. line.gt.l+7) goto 40
      if (s.lt.1 .or. s.gt.800) goto 40

      if (l.eq.line) then
         buf(s) = white
      else
         if (ires.lt.10) then
            n = 1
         elseif (ires.lt.100) then
            n = 2
         else
            n = 3
         endif
         write(msg,101) ires
         call text(msg(4-n:3),n,line-l-1,buf(s+2),6,255)
      endif
   40 continue

   50 call xvwrit(ounit,buf,ind,0)

      call xvclose(ounit,ind,0)
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create resloc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM resloc

   To Create the build file give the command:

		$ vimake resloc			(VMS)
   or
		% vimake resloc			(Unix)


************************************************************************/

#define PROGRAM	resloc
#define R2LIB

#define MODULE_LIST resloc.f \
		open_pic.f get_lab_ids.f get_poe.f \
		vgrlab_cam.f vgrlab_scet.f \
		open_rdb.f get_res_rdb.f put_res_rdb.f \
                read_pic.f \
		find_res_d.f \
		get_rtype.f get_neighbors.f \
		get_A_size.f compute_A.f get_R_size.f \
		get_window.f check_window.f \
		filter_B.f print_B.f \
		compute_R_detrend.f detrend_window.f \
		compute_R_gap.f print_R.f \
		find_rmax.f local_max.f interp_max.f \
		get_W.f compute_dn.f compute_dedge.f \
		compute_offsets.f compute_eps.f \
		compute_dc.f \
		compute_q.f choose_best.f \
		reject.f \
		check_rho.f check_q.f check_corners.f \
		fill_res.f interp_3.f interp_4.f \
		compute_geo.f interg.f triag.f \
		write_res.f write_geo.f \
		ptres1.f ptres5.f ptres7.f ptres8.f \
		print_window.f \
		print_stats.f p_stats.f p_avg_stats.f \
		print_res.f pmjs.f print_rows.f \
		display_results.f overlay.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77

/*#define DEBUG		/* disable on delivery */

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create resloc.pdf
process help=*
parm inp      type=string  count=1:2
parm out      type=string  count=1:2
parm camera   type=integer count=0:1   valid=4:7     default=--
parm frame    type=integer count=0:1                 default=--
parm planet   type=keyword count=0:1 	+
	      valid=(jupiter,saturn,uranus,neptune)  default=--
parm nlw      type=integer count=0:1   valid=(5:9)   default=5
parm nsw      type=integer count=0:1   valid=(5:9)   default=5
parm nhor     type=integer count=0:1   valid=(11:25) default=19
parm nver     type=integer count=0:1   valid=(11:25) default=19
parm sigma    type=real    count=0:1                 default=1.
parm rthresh  type=real    count=0:1   valid=(-1:1)  default=0.7
parm qthresh  type=real    count=0:1   valid=(-1:1)  default=0.6
parm redo     type=keyword count=0:1   valid=redo    default=--
parm nofilter type=keyword count=0:1   valid=nofilter default=--
parm noin     type=keyword count=0:1   valid=noin    default=--
parm nofill   type=keyword count=0:1   valid=nofill  default=--
parm dbug     type=keyword count=0:1   valid=dbug    default=--
parm tres     type=integer count=0:1   valid=0:202   default=0
parm reject   type=integer count=0:100 valid=0:202   default=0
parm ores     type=string  count=0:1   default=""
parm onom     type=string  count=0:1   default=""
parm pres     type=keyword count=0:1   valid=pres    default=--
parm pnom     type=keyword count=0:1   valid=pnom    default=--
parm print    type=keyword count=0:1   valid=print   default=--
parm pstats   type=keyword count=0:1   valid=pstats  default=--
parm pgeo     type=keyword count=0:1   valid=pgeo    default=--
end-proc

.TITLE
 "resloc" -- Voyager reseau location program
.HELP
Reference:  618-802 Voyager Imaging Science Subsystem Calibration Report,
Milosh Benesh and Paul Jespen.

Resloc is a VICAR program that locates the reseau marks in Voyager images.

When used together, resloc and geoma will remove the camera's geometric
distortions from the image.

	resloc inp=D out=(Res,Geo)
	geoma inp=(D,Geo) out=Dos size=(1,1,1000,1000)

Resloc finds the reseau marks in image D and stores their (line,sample)
coordinates together with their object-space coordinates in tiepoint file Geo.
Geoma creates an object-space version of D by mapping each reseau mark to its
known coordinates in object space and linearly interpolating elsewhere.  In
this procedure, the file Res plays no role.

Alternatively, the geometric correction can be combined with a map projection
by using program map3.

When used together, resloc and ressar77 will remove the reseau marks from the
image:

        resloc inp=D out=Res
        ressar77 inp=(D,Res) out=E

Resloc stores the (line,sample) coordinates of the reseau marks in file Res.
Ressar77 cosmetically removes the marks by interpolating over neighboring
pixels.  

A detailed description of resloc is presented in the following sections:

   1.The Voyager reseau
   2.Image windows
   3.The shape template
   4.Finding the reseau
   5.Correlation coefficient
   6.Darkness measure
   7.Closeness measure
   8.Detrending
   9.Camera noise
  10.Beam bending
  11.Filling the gaps
  12.Geometric calibration
  13.Geoma parameters
  14.Reseau database
  15.Resloc parameters


.page
1.The Voyager reseau 

The Voyager reseau is a network of fiducial marks spatially distributed over
the field of view of the camera.  They are numbered as follows:

   1     2     3     4     5     6     7     8     9    10    11    12
     13    14    15    16    17    18    19    20    21    22    23
  24    25    26    27    28    29    30    31    32    33    34    35
     36    37    38    39    40    41    42    43    44    45    46
  47    48                                       202          49    50
     51    52    53    54    55    56    57    58    59    60    61
  62    63                                                    64    65
     66    67    68    69    70    71    72    73    74    75    76
  77    78                                                    79    80
     81    82    83    84    85    86    87    88    89    90    91
  92    93                                                    94    95
     96    97    98    99   100   101   102   103   104   105   106
 107   108                                                   109   110
    111   112   113   114   115   116   117   118   119   120   121
 122   123                                                   124   125
    126   127   128   129   130   131   132   133   134   135   136
 137   138                                                   139   140
    141   142   143   144   145   146   147   148   149   150   151
 152   153                                                   154   155
    156   157   158   159   160   161   162   163   164   165   166
 167   168   169   170   171   172   173   174   175   176   177   178
    179   180   181    182  183   184   185   186   187   188   189
 190   191   192   193   194   195   196   197   198   199   200   201

The reseau marks are metal squares embedded on the faceplate of the vidicon.
By measuring the positions of these squares on the faceplate, and
the position of their shadows in the image, we obtain a coarse spatial
sampling of the geometric distortion in the image.

The marks are concentrated around the margins where the distortions are
largest.  The reseau is symmetric about its center (mark 101) except for
reseau mark 202, whose uniqueness establishes the orientation of the image.

Note:  The above numbering scheme differs from that in Benesh Fig. 3-39 in 
that the extra reseau mark is number 49 in Benesh, and 202 here.  Also, there
is a right-to-left reversal of the two grid patterens that is analogous to
what happens when a sign in a window is read from the inside and from the
outside.


.page
2.Image_windows

Let D be a Voyager image, where

	    |   d(1,1)   d(1,2) . . . d(1,800) |
	    |   d(2,1)   d(2,2) . . . d(2,800) |
	D = |      .        .  .         .     |
	    |      .        .    .       .     |
	    |      .        .      .     .     |
	    | d(800,1) d(800,2) ... d(800,800) |

and d(l,s) is the DN value at line-sample coordinates (l,s).

An image window is a rectangular area in the image.  Let W be a window in D,
where

	    | w(-m,-n) .  .  . w(0,-n) . . . w(m,-n) | 
	    |     .     .         .       .     .    |
	    |     .       .       .     .       .    |
	    |     .         .     .   .         .    |
	W = | w(-m,0)  .  .  . w(0,0)  . . . w(m,0)  | 
	    |     .        .      .  .         .     |
	    |     .      .        .     .   .  .     |
	    |     .   .           .        .    .    |
	    | w(-m,n)  .  .  . w(0,n)  . . . w(m,n)  | 


The window is centered at pixel (l,s) if w(0,0)=d(l,s).

Example: The window centered at the pixel (1,1) is

            |  0  0     0      0      0   |
            |  0  0     0      0      0   |
        W = |  0  0  d(1,1) d(1,2) d(1,3) |
            |  0  0  d(2,1) d(2,2) d(2,3) |
            |  0  0  d(3,1) d(3,2) d(3,3) |

As this example illustrates, windows along the corners and edges are allowed
provided we keep track of which pixels are real and which are not.

Integers m and n are the half-window dimensions of W.  The height and width
of the window are 2n+1 and 2m+1, which are always odd integers.

The total number of pixels in W is N=(2m+1)(2n+1).

The mean and standard deviations of W are

	         1
	mu(W) = ---sum(sum(w(i,j)))
	         N

	               1
     sigma(W) = sqrt{ ---sum(sum((w(i,j) - mu(W))**2))}
	               N

where i=-m,...,m and j=-n,...,n.


.page
4.The shape template

The shape template is a Gaussian approximation of a reseau mark.  The template
is represented by the window A, where the default A is

                   |250 234 220 234 250|
                   |234 161 100 161 234|
               A = |220 100   0 100 220|
                   |234 161 100 161 234|
                   |250 234 220 234 250|

What follows describes how this A came to be defined.

Each reseau mark is a metal square 0.04 mm on a side, and creates a shadow
in the image that is roughly 3x3 pixels in area.  Here are examples of
these shadows:
 
           3   3   3   3   3		255 255 255 255 255
           5   3   2   3   2		234 144 131 185 239
           3   1   0   0   2		177  51  48 164 241
           4   2   0   2   2		169  34  20 146 236
           5   5   4   3   4		203 147 166 252 252
 
   12  10  10  14  15    103  89  62  48  35    227 205 162 163 192
   11   7   7   9  13     81  33  16  33  48    199 121  43  81 172
   12   8   3   2  13     65  16   3  27  59    180  99   5  54 167
   12  12   7   7  12     79  49  46  79  92    174 116  48 107 206
   12  12  12  12  12    103 110 105 108 108    189 176 164 186 209

As the first example suggests, the shadow is sometimes too faint to locate
with confidence.

The shadows are never shaped like squares, but have a rounded appearance.
We approximate this shape with an inverted Gaussian of the form

			        1
	f(x,y) = 1 - e**{ - ----------*(x**2+y**2)}
                            2*sigma**2
 
f is called the reseau shape function.  f is actually a vast simplification
of John Kresznar’s shape function used in Mariner 9 and Viking Orbiter.  During
those missions, the shape of each reseau mark was individually measured and
modeled by a Gaussian multiplied by a fifth order polynomial.  For Voyager,
a simple Gaussian represents the shape of all 202 reseau marks.

The shape template is a digitized window of f, where

		    | a(-m,-n) .  .  . a(0,-n) .  .  . a(m,-n) |
		    |     .   .           .           .   .    |
		    |     .       .       .       .       .    |
		    |     .           .   .  .            .    |
		A = | a(-m,0)  .  .  . a(0,0)  .  .  . a(m,-0) |
		    |     .               .               .    |
		    |     .       .       .       .       .    |
		    |     .   .           .            .  .    |
		    | a(-m,n)  .  .  . a(0,n)  .  .  . a(m,n)  | 

and
		a(i,j) = 255*f(i,j).

The pixels are scaled by 255 to match the 0 to 255 DN range output by the
Voyager camera.

The default shape template shown at the top is for sigma=1.  A 5x5 template
appears to be the optimum size.  It is large enough to enclose the 3x3 shadow
plus 16 pixels of the surrounding background.


.page
6.Finding the Reseau

Let W be a window centered at line-sample coordinates (l,s) in image D, and
let A be the shape template.  The correlation coefficient rho measures how
closely A and W match:   
         	          cov(A,W)
        	rho = -----------------
               	      sigma(A)*sigma(W)

To find a reseau mark, a search is made in an area surrounding its nominal
coordinates.  Let (l0,s0) be these nominals.  We compute rho at every pixel
in the search area, creating a matrix R, where

	    | r(-m,-n) .  .  . r(0,-n) .  .  . r(m,-n) |
	    |     .    .          .          .    .    |
	    |     .       .       .       .       .    |
	    |     .          .    .    .          .    | 
	R = | r(-m,0)  .  .  . r(0,0)  .  .  . r(m,0)  |
	    |     .          .    .    .          .    | 
	    |     .       .       .       .       .    |
	    |     .    .          .          .    .    |
	    | r(-m,n)  .  .  . r(0,n)  .  .  . r(m,n)  |

and r(0,0) is the value of rho when W is centered at (l0,s0).

The mark is usually at the maximum value of R.  Occasionally, however
resloc will find another object in the search area that resembles A better than
the actual mark.  Also, as many as half of the marks along the margins of the
image fall off the edge.  In other words, we can never be certain
whether we have found the mark or if the mark exists at all.

To reduce this uncertainty, we take the three highest peaks in R and
ask two further questions:

	1) Is the object dark enough to be a reseau mark?
	2) How close is it to where we expected it to be?

Since only stray light shines on the shadow, its center should be the
darkest point in the search area, if it is there at all.

To predict the position of each mark, we allow for a systematic shift of
the entire reseau from the nominals.  We solve for this shift by minimizing
the residual error between the found coordinates and their nominals.

From these two measurements, we compute normalized darkness and closeness
measures g and h that are roughly on the same scale as rho. 

We may then compute a composite match quality measure q, where

		     1
		q = ---(rho+g+h)
		     3
 
In dark-sky regions of the image, where the DN values are near the noise floor, we ignore the darkness measure and set

		     1
		q = ---(rho+h)
		     2
 
The pixel with the highest q is the most likely place for the reseau mark.

To filter out mismatches, we apply thresholds to rho and q, rejecting all
objects where ρ<rthresh or q<qthresh.  The default values are rthresh=0.7
and qthresh=0.6.  These defaults are high enough to filter out practically all
mismatches but low enough to accept nearly all of our visible marks.

We fill in the places where the mark was not found by interpolating over
the nearest neighbors. This is done so that geoma receives a mapping that is
complete.


.page
5.The correlation coefficient

Let A be the shape template and let  W be a window in the image. 

The covariance between A and W is
	            1
	cov(A,W) = ---sum(sum((a(i,j)-mu(A))*(w(i,j)-mu(W)))
	            N 

where mu(A) and mu(W) are the mean values of A and W, and N is the number
of pixels in A and W.

The correlation coefficient between  A and W is a normalized covariance

	               cov(A,W)
	rho(A,W) = -----------------
	           sigma(A)*sigma(W)


where the normalization factors are the standard deviations of A and W.

Three properties of rho follow from this definition:

	1) rho(A,-W) = -rho(A,W)
	2) -1 < rho(A,W) < 1
	3) rho(A,W)=1 if and only if A=W 


Examples:

The following windows contain reseau marks.  

	| 159 151 131 128 138 |		| 166 172 179 170 163 |
	| 133  61  35  75 130 |		| 163 143  74  69 124 |
	| 116  35   7  58 124 |		| 146  99  27  39 117 |
	| 117  52  28  73 133 |		| 141  93  40  59 139 |
	| 133 123 117 129 139 |		| 147 129 124 137 159 |

In the left window, the reseau mark lies near the center.  In the right window,
the mark lies somewhere between four pixels.

The default shape template is  

                   |250 234 220 234 250|
                   |234 161 100 161 234|
               A = |220 100   0 100 220|
                   |234 161 100 161 234|
                   |250 234 220 234 250|

Since A is centered, a mark that is centered will have a higher rho than
one that is not.  In the above examples, rho=0.962 vs 0.909. To get a more
unbiased measure or rho, we interpolate the center of the mark and compute
rho at that point.  The interpolated values of rho are 0.968 vs 0.974.


Here are examples of marks that are under-exposed and over- exposed:

	|   6   6   6   6   7 |		| 255 255 255 255 255 |
	|   5   2   1   4   4 |		| 234 144 131 185 239 |
	|   5   1   0   1   5 |		| 177  51  48 164 241 | 
	|   3   0   0   2   5 |		| 169  34  20 146 236 |
	|   6   4   3   4   6 |		| 203 147 166 252 252 |

The saturated pixels (0s and 255s) will distort the shape of the mark.  This
effect is small, however, compared to whether or not the mark is centered in
the window.  In fact, the interpolated values at the center of the marks are
rho = 0.909 and 0.935.

Here is a case where a reseau mark straddles the left margin of the image
(indicated by 0s).  This window has 15 valid pixels, barely enough to compute 
a reliable value for rho. 

		|   0   0  14  15  18 |
		|   0   0   6   7  15 |
		|   0   0   4   5  16 |
		|   0   0  13  13  16 |
		|   0   0  18  17  17 |

Here the interpolated value for rho is 0.924.

Note that if the mark were located 1 pixel to the left, its center would
lie off the edge of the image, and its window would contain only 10 real
pixels.  In fact, only a faint signal would remain:


		|   0   0   0  15  18 |
		|   0   0   0   7  15 |
		|   0   0   0   5  16 |
		|   0   0   0  13  16 |
		|   0   0   0  17  17 |


Since random surface terrain can look like this, reducing our sample size
to 10 greatly increases the risk of a false match.  Consequently, resloc
rejects any window if more than half the pixels are missing.

When an image line is compressed by Voyager but is still too large to fit in a
telemetry packet, the line is truncated and the excess pixels are lost.  When
the JPL ground database system reconstructs the line, the truncated pixels are
filled with 0s.

The following window contains two truncated line segments.

		|  46  49  35  41  49 |
		|   0   0   0   0   0 |
		|  38  19  13  31  43 |
		|   0   0   0   0   0 |
		|  46  47  47  47  48 |

			rho = 0.947

As before, 15 pixels seems to be enough to measure a reasonably reliable rho.


.page
6.Darkness measure

Since no light strikes its center, the reseau mark should be the darkest object
in the search area, if it is there at all.

Here is a window containing a reseau mark:
 
                 | 166 172 179 170 163 |
                 | 163 143  74  69 124 |
                 | 146  99  27  39 117 |
                 | 141  93  40  59 139 |
                 | 147 129 124 137 159 |

The darkest point of the shadow lies between four pixels.  Let d be its
interpolated value.  Here, d is 19.2.

Here are the values of d for the entire reseau:

  117   112   113   157   164   138   165   122    73    66    84    90
      96    32    23    14    18    24    34    33    48    65    93
   73    35    19    12     9    19    20    23    37    48    67    68
      44    18     8    18     4    17    15    17    27    45    60
   26    24                                        17          44    90
      20     9    10     5    10    10     9    17    20    34    34
    0    10                                                    30   184
      20     6    10     2     6     4     3     9    14    19    31
  117    16                                                    12   200
      11    13     1     5     0     1     6    11     9    15    18
  129     5                                                    14   194
       7     0     4     0     2     6     6     5     5    14    13
  115     6                                                    11   198
      10     0     2     0     3     6     4     0     5     6    11
  104     0                                                    10   172
       4     4     5     0     1     4     4     0     9     8    15
   15     0                                                    23   140
       9     3     4     0     6     3     1     7     3    17    17
    0     9                                                    23    18
      14     7     5     6     1     5     8     5    14    25    34
    0    14    15    10     8    11     2     8    14    17    21    34
      13    17    14     8     8     2    13    13    19    17    27
   19    17    16    42   145   153   165   142   172   165    34    65
 

Notice how d increases as you move outward from the center.

Also, there are obvious errors along the edges.  These are places where
the mark falls off the edge, and d is simply a dark feature on the planet.
Bogus values like these are rooted out by comparing each d to its four
nearest neighbors as follows:

Let di be the interpolated value that lies in the DN plane as its four
neighbors.

Let dedge be the mean value of the window's edge.  dedge
measures the surrounding background.

We define the darkness measure to be

		          d-di
		g = 1 - ----------   if d>di
		        dedge-di
 
		g = 1  otherwise


From this definition, it follows that 0<=g<=1 and that g=1 when d<=di.


.page
7.Closeness measure

Let (li,si) be the coordinates of mark i, as found by resloc, where
i=1,2,...,202.

Let (li0,si0) be their nominal coordinates.

We assume that the principal difference between the (li,si) and (li0,si0) is
a small constant offset:

                si ~ si0 + e
                li ~ li0 + f

We solve for the e and f that minimize the least-squares differences
between the two data sets:

                    1
                e = -*sum(si-si0)
                    n

                    1
                f = -*sum(li-li0)
                    n

The distance between the coordinates found by resloc and this model is

		delta = sqrt((si-si0-e)**2+(li-li0-f)**2))


The normalized closeness measure is

			 delta
		h = 1 - --------
		        deltamax
 
where deltamax is the maximum possible delta within the search area.

h has a range from 0 to 1, and equals 1 only when the mark is found at
the model's predicted location.


.page
9.Solar shading

Solar shading near the limb or terminator of the planet degrades the shape
of reseau marks.  A process called detrending is used to remove this shading.

Example:

The window below (W) contains a reseau mark that straddles the terminator of
Uranus.  When the window is detrended (W’), the correlation coefficient
improves from 0.712 to 0.988.

        | 69  35  23  24  25 |          |  0   5   9   9  10 |
        | 46   7   3  15  22 |          |  5   8 -11  -9   4 |
        | 39   7   0  11  20 |          |  6  -6 -32 -22   3 |
        | 44  26  18  20  20 |          |  8  -6 -24 -17   7 |
        | 42  31  24  20  21 |          |  8   6   2   8  24 |

                   W                               W’


	    | w(-m,-n) .  .  . w(0,-n) .  .  . w(m,-n) |
	    |     .    .          .          .    .    |
	    |     .       .       .       .       .    |
	    |     .          .    .    .          .    |
Let	W = | w(-m,0)  .  .  . w(0,0)  .  .  . w(m,0)  |
	    |     .          .    .    .          .    |
	    |     .       .       .       .       .    |
	    |     .    .          .          .    .    |
	    | w(-m,n)  .  .  . w(0,n)  .  .  . w(m,n)  |
 

We fit a brightness plane B to W, where

                | b(-m,-n) .  .  . b(0,-n) .  .  . b(m,-n) |
	        |     .    .          .          .    .    |
	        |     .       .       .       .       .    |
		|     .          .    .    .          .    |
  B(c1,c2,c3) = | b(-m,0)  .  .  . b(0,0)  .  .  . b(m,0)  |
		|     .          .    .    .          .    |
	        |     .       .       .       .       .    |
	        |     .    .          .          .    .    |
                | b(-m,n)  .  .  . b(0,n)  .  .  . b(m,n)  |

and
		b(i,j) = c1*i + c2*j + c3
 

We solve for plane constants c1, c2 and c3 that minimize the squared
differences between B and W.   Let 

		r = sum{sum{c1*i+c2*j+c3-w(i,j)}**2}

i=-m,...,m and j=-n,...,n, be the sum of the squared differences.
			 
The function r is at its minimum when the partial derivatives or r with
respect to c1, c2, and c3 are 0.  This leads to the following system of
linear equations:

		c1*sii + c2*sij + c3*si = zi
		c1*sij + c2*sjj + c3*sj = zj
		c1*si  + c2*sj  + c3*N  = z
where
	si = sum(sum(i)) = 0	z = sum(sum(w(i*j)))
	sj = sum(sum(j)) = 0	zi = sum(sum(i*w(i,j)))
	sij = sum(sum(ij) = 0	zj = sum(sum(j*w(i,j)))

			       N
	sii = sum(sum(i**2)) = -*m(m+1)
			       3

			       N
	sjj = sum(sum(j**2)) = -*n(n+1)
			       3

and N=(2m+1)*(2n+1) is the total number of pixels in W.

Since the nondiagonal terms in the above equations are all 0, the
solution follows immediately:

 	     zi		     zj		     z
	c1 = ---	c2 = ---	c3 = -
	     sii	     sjj	     N


To detrend the window, we subtract the brightness plane from it:

		W' = W - B
where
		w'(i,j) = w(i,j) - (c1*i+c2*j+c3)

Why not leave in the c3?


.page
9.Camera noise

Here is a pixel listing of the upper-left corner of the image where the noise
reaches its peak:

     Samp     1       3       5       7       9      11      13      15
   Line
      1      64  46  44  39  35  33  32  28  29  28  28  25  24  27  27
      2      40  29  26  21  18  15  13  12  11  11  13  12  12  13  11
      3      34  24  23  18  16  15  14  13  14  14  13  15  17  15  15
      4      38  27  27  24  22  21  21  19  19  18  18  16  12  19  17
      5      42  28  28  25  21  21  23  22  21  21  19  13   9  17  16
      6      45  27  27  22  20  22  22  21  22  21  21  18  15  15  15
      7      45  25  24  21  20  21  22  22  20  19  19  17  16  15  13
      8      42  25  22  19  19  20  22  21  20  19  16  16  14  13  13
      9      38  24  22  18  19  21  19  20  18  16  15  14  12  13  14
     10      36  24  22  18  19  20  19  18  16  16  13  13  13  13  12
     11      35  22  21  19  19  19  17  16  14  13  12  12  13  13  13
     12      33  22  21  18  19  16  15  14  12  12  13  13  14  14  14
     13      33  22  18  17  17  15  15  12  12  12  13  12  14  12  13
     14      31  20  19  16  16  14  13  11  12  11  13  12  12  13  12
     15      29  17  17  14  13  12  11  11  11  12  12  11  13  12  11

Reseau mark #1 is the 9 at pixel (5,13).  No satisfactory method for removing
this noise was found.  The problem is that any filter will ring off the edges
and the horizontal and vertical fiducial marks in this corner, creating objects
that look like reseau marks.

Here is a printout of the top edge of the image:

     Samp   101     103     105     107     109     111     113     115
   Line
      1      20  21  21  22  22  22  21  21  22  22  22  22  20  21  21
      2       9   8   8   8   9   8   8   9   8   8   8   7   8   8   8
      3      12  11  12  11  11  12  12  12  12  11  12  12  12  12  12
      4      15  15  15  15  16  15  15  14  14  15  14  14  15  14  15
      5      15  17  17  15  16  15  15  15  15  15  15  14  15  14  15

For search areas that straddle the top edge, lines 1, 2 and 3 are scaled so
that they have the same mean value as line 4.  Hot and cold columns along the
left edge are treated the same.


Notes:

1) Each of the four corners contain two long fiducial marks, one vertical and
one horizontal.  They are roughly 50 pixels long and 5 pixels wide.  Despite
their difference in size and shape, radiometrically they behave similar to
reseau marks.  The danger here is that if the reseau mark is not visible, these
fiducial marks could be mistaken for the missing mark.

2) Reseau marks 1 and 13 are placed so close together that they appear in each
other’s search area.  A similar situation exists in the other three corners.
A proximity check is made to ensure that the wrong mark is not chosen.


.page
10.Beam bending

The electron beam that reads the image off the photosensitive surface of the 
vidicon is attracted to bright objects in the image.  This effect, called beam
bending (see Benesh and Jepsen), will cause every bright object to be read out
prematurely.

Consider, for example, an image of Io resembling a bright circular disk.  As
the beam scans from line to line, Io will be read out prematurely, causing the
satellite to bulge upward and outward to the left and right.  Furthermore, once
an image line has been scanned by the readout beam, its charge is erased. 
Therefore, an object can never be distorted by the part of the image that lies
above it, because that part has already been erased.

Beam bending can cause displacements as large as 5 pixels.  Distortions similar
to our Io example above are inadequately measured by our coarse grid of reseau
marks.  Therefore, the brighter an object is, the more you must learn to
distrust its position.


.page
11.Filling the gaps

To provide geoma with a complete set of tiepoints, the line-sample coordinates
of every missing mark is filled by interpolating over its four neighbors.
Neighbors along the edge are excluded from this interpolation because they are
less reliable.

Every reseau mark potentially has 8 neighbors.  Let the neighbors of X be
denoted as near or far neighbors as follows:

				f1
			    n1      n2
			f3      X      f4
			    n3      n4
				f2


Let L=[n1,n2,n3,n4,f1,f2,f3,f4] be a list of these neighbors, roughly ordered
from nearest to farthest.  From this list, we eliminate any neighbors that
are missing or not found or near the edges.

Example:  The neighbors of reseau mark 36 are:

				13
			    24      25
			 0      36      37
			    47      48
				51

Here L=[24,25,47,48,13,51,0,37].  As this example shows, missing neighbors are
assigned a 0.  Suppose that mark 36 was not found.  An interpolation is made
using the first four valid neighbors in the list.  Marks 24 and 47 are excluded
because they lie near the left edge.  The four valid neighbors are 25, 48, 13
and 51.  We compute the binomial transformation that maps these neighbors to
their nominal locations. This transformation then allows us to compute the
coordinates for mark 36 from its nominals.

The cases where the number of valid neighbors are 0,1,2 or 3 are handled as
well.


.page
12.Geometric calibration

The purpose of geometric calibration is to measure and remove the geometric
camera distortions from the images.

The optics of the Voyager imaging subsystem (the ISS) was designed to mimic
the geometry of a pinhole camera.  If the telescope and vidicon tube were
geometrically perfect, light entering the aperture of the telescope would
converge at the focal point (the pinhole) and strike a focal plane mounted
normal to the optic axis.  The resulting image would be a perfect perspective
projection of the 3-dimensional world.  Object space is the coordinate system
of this focal plane.

Alas, the camera is not perfect. There are radial distortions in the Voyager
telescope.  In addition the vidicon is a cathode ray tube and suffers from
geometric distortions that increase from the center.  In addition, a fraction
of the vidicon distortion is caused by beam bending, an image-dependent
phenomenom where bright objects bend the electron beam reading out the pixels.
Image space is the coordinate system of this distorted image.

Constructing object space:  Ideally, object space is a focal plane mounted
normal to the optic axis of the telescope.  The faceplate of the vidicon was
used for this focal plane.  202 metal squares (called reseau marks) were
embedded on its surface.  During bench calibration, the positions of these
marks were measured with a theodolite (Benesh and Jepsen).  The resultin
(x,y) coordinates, in mm, are the object space coordinates of the reseau marks.

Object space is scaled by a factor of 84.821428 pixels per millimeter so that
an 800x800 Voyager image will map into a 1000x100 object-space image.

Finding the reseau marks in image space:  The (line,sample) coordinates of
the 202 reseau marks in the image represent a coarse spatial sampling of image
space.  These coordinates will differ from image to image because of  inherent
randomness in the geometric distortion.  Therefore, the reseau marks must be
found anew in each image output by the camera.  This is the purpose for which
resloc was developed.

Transforming the image into object space:  Program geoma creates an
object space version of the image.  The transformation maps every reseau
mark from the image to its known position in object space.  All other pixels
are mapped using an affine transformation defined by the three nearest marks.


.page
12.Geoma parameters

Geoma is the program that transforms the image into object space.  Geoma
requires that the transformation be defined by a rectangular grid of tiepoints.
The Voyager reseau more naturally lends itself to being divided into a grid of
triangles.  In order to satisfy geoma, we represent our triangles as a
collection of degenerate quadrilaterals, each of which has two identical
vertices.  Geoma is equipped to handle this degenerate case.

The grid of triangles is made spatially more uniform by adding pseudo marks via
interpolation or extrapolation.  X marks the spots where the pseudo marks are
added:


  1      2      3      4      5      6      7      8      9     10     11     12

  x  13     14     15     16     17     18     19     20     21     22     23  x

 24     25     26     27     28     29     30     31     32     33     34     35

  x  36     37     38     39     40     41     42     43     44     45     46  x

 47     48      x      x      x      x      x      x    202      x     49     50

  x 51     52     53     54     55     56     57     58     59     60     61   x

 62     63      x      x      x      x      x      x     x       x     64     65

  x  66     67     68     69     70     71     72     73     74     75     76  x

 77     78      x      x      x      x      x      x     x       x     79     80

  x  81     82     83     84     85     86     87     88     89     90     91  x

 92     93      x      x      x      x      x      x     x       x     94     95

  x  96     97     98     99    100    101    102    103    104    105    106  x

107    108      x      x      x      x      x      x     x       x    109    110

  x 111    112    113    114    115    116    117    118    119    120    121  x

122    123      x      x      x      x      x      x     x       x    124    125

  x 126    127    128    129    130    131    132    133    134    135    136  x

137    138      x      x      x      x      x      x     x       x    139    140

  x 141    142    143    144    145    146    147    148    149    150    151  x

152    153      x      x      x      x      x      x     x       x    154    155

  x 156    157    158    159    160    161    162    163    164    165    166  x

167    168    169    170    171    172    173    174    175    176    177    178

  x 179    180    181    182    183    184    185    186    187    188    189  x

190    191    192    193    194    195    196    197    198    199    200    201

This grid contains 12 tiepoints on every odd-numbered row and 13 tiepoints on
every even-numbered row.

We create a rectangular grid of tiepoints for geoma by repeating every tiepoint
wice,  except for the pseudo marks that begin and end every even-numbered row.
These are entered only once.  Two things are accomplished in this bizarre
procedure.  First, every triangle is converted into a quadrilateral with a
repeated vertex.  Second, each resulting row contains exactly 24 tiepoints,
creating a more or less spatially uniform 23x24 grid:

   1   1   2   2   3   3  ....  12  12
   x  13  13  14  14  15  ....  23   x
  24  24  25  25  26  26  ....  35  35
   etc...

The output geometric transformation parameters (geo) is a FORTRAN array:

      real*4 geo(4,24,23)

Each tiepoint in this array forms a geometric link between image space and
object space:

	geo(1,j,k) = object-space line coordinate
	geo(2,j,k) = object-space sample coordinate
	geo(3,j,k) = image-space line coordinate
	geo(4,j,k) = image-space sample coordinate


.page
14.Reseau database

Resloc can store the reseau locations of an image into a reseau database, and
later retrieve it from there:

	resloc inp=(D,RDB out=res
or
	resloc inp=(D,RDB) out=(res,geo)

In both of the above cases, if the reseau coordinates for D are already
in RDB, they are retrieved from there and output to res.  However, see the
keyword REDO.  If the reseau coordinates for D are not in RDB, resloc finds
them in D and stores them in RDB.

Data in the RDB are identified by camera, frame number, filter, and SCET year
and day.

Is there a limit on the number of reseau locations RDB can hold?


.page
15.Resloc parameters

The resloc parameters are grouped below by common function:

 inp out
 camera frame planet
 nlw nsw nhor never
 sigma
 rthresh qthresh
 redo nofilter noin

 inp and out are the only required parameters.
 camera, frame and planet overide the image IDs found in the label.
 nlw and nsw specify the dimensions of the shape template. 
 nhor and nver specify the dimensions of the search area.
 sigma specifies the sigma in the Gaussian shape function.
 rthresh and qthresh specify the thresholds for rho and q.
 redo forces resloc to redo an entry in the RDB.
 nofilter suppresses filtering of the hot edges.
 noin suppresses sub-pixel interpolation.

 dbug and all the parameters that follow it are for debugging the program.

 dbug
 tres reject nofill
 ores onom
 pres pnom
 pstats
 pgeo

 dbug causes miscellaneous diagnostics to be printed.
 tres is a test mark #.  All intermediate results for # are printed out.
 reject deletes all coordinates in the list:  reject=(2,3,101)
 nofill suppresses interpolation of missing values.
 ores superimposes the reseau coordinates on D.  View using xvd.
 onom superimposes the nominals on D.  View using xvd.
 pres and pnom print the reseau coordinates and their nominals.
 pstats prints a map of d, di, dedge, eps, rho, g, h and q.
 pgeo prints the geoma parameters

Do not use ‘nofill when creating the geoma parameters, as the solution will
be incomplete.

ores creates an image that is useful for assessing the accuracy and reliability
of resloc.


.page
Program history:

written by: Gary Yagi	23 july 1977
converted to vax by:  c. c. avis	27 may 1983
cognizant programmer: Gary Yagi
revisions:
 16-nov-85 - l.w.kamp - converted i/o to vicar2
 24-jan-86 - l.w.kamp - replaced lock management code with vicar2
                         open status check + call wait.
 26-feb-86 - l.w.kamp - modified subr.maxr to check dn<dnthresh (new parm)
 20-jun-86 - l.w.kamp - revised subr.filloc to fill in all locations
 27-jun-86 - f.f.moss - converted param i/o to vicar2
 10-jul-95 - a.scop   - (cri) made portable for unix
 27-mar-96 - b.a.mcguffie   - modified to accept new ibis reseau location
      3-89  sp  prevent divide by zero.  finish off lwk's fix to filloc where
               min set to 0.
      3-10  lwk replaced testos() with xvhost()
 2013 version:  gmy - added search for margin reseau marks.

The original resloc (the one used throughout the mission) did not
attempt to locate the marks along the margins of the image because of
the difficulty of doing this reliably.  The 2013 resloc version was an
attempt to remedy this.

For purposes of testing, a small sample master reseau location file
named RESFIL.TST is in the current MIPL test directory.  Please check
with Integration and Test to determine where this directory is located.
This file should not be randomly modified: copy it to a scratch directory 
before tampering with it.

.LEVEL1
.variable inp
inp=D or inp=(D,RDB)
raw Voyager image D and
reseau database RDB
.variable out
out=RES or out=(RES,GEO)
reseau locations RES and
geometric parameters GEO
.variable camera
Voyager camera serial number
 (4,5,6,7)
.variable frame
mod16 count of the FDS
  frame=1234567
.variable nlw
height of correlation area
  nlw=5
.variable nsw
width of correlation area
  nsw=5
.variable nver
height of the search area
  nver=19
.variable nhor
width of the search area
  nhor=19
.variable sigma
standard deviation of
Gaussian shape function
  sigma=1.0
.variable rthresh
rho threshold
  rthresh=0.7
.variable qthresh
Quality measure threshold.
  qthresh=0.6
.variable redo
  keyword: 'redo
Redo the reseau locations
.variable nofilter
  keyword: 'nofilter
No hot edge filter
.variable noin
  keyword: 'noin
No subpixel interpolation
.variable nofill
  keyword: 'nofill
No filling of missing marks
.variable dbug
  keyword: 'dbug
Switch to debug mode
.variable tres
  tres=29 
Test reseau mark number
.variable reject
  reject=(2,3,202)
Reject this list of marks
.variable ores
  ores=filename
Overlay of reseau locations.
.variable onom
  onom=filename
Overlay of nominal locations.
.variable pres
  keyword: 'pres
Print the reseau locations.
.variable pnom
  keyword: 'pnom
Print the nominal locations.
.variable pstats
  keyword 'pstats
Print maps of d,di,dedge,
  eps,rho,g,h, and q.
.variable pgeo
  keyword: 'pgeo
Print the geoma parameters.


.LEVEL2
.vari inp
Input files...

      inp=D   or  inp=(D,RDB)

where D is the input image and RDB is an optional reseau database.

D must be a raw Voyage image. That is, it must be in 800x800 format and
must neither be radiometrically or geometrically transformed.

If an RDB is included, the reseau locations are stored into or retrieved
from it.

.vari out
Output files...

     out=res
  or out=(res,geo)

res contains the line-sample coordinates of all 202 reseau marks.
geo contains the geometric parameters for transforming image D
to object space.

.vari camera
Voyager camera serial number:

        camera=4 for VGR2 WA
        camera=5 for VGR2 NA
        camera=6 for VGR1 WA
        camera=7 for VGR1 NA

Overrides the camera serial number in the image label.

.vari frame
A 7-digit frame number created by appending the mod60 count to the
mod16 count.

  frame=1234567

This parameter is only useful if you are storing or retrieving reseau
locations from the rdb and the frame number in the label is incorrect.

.vari sigma
Sigma of Gaussian shape template.

  sigma=1.0

.vari rthresh
Correlation coefficient threshold.  

    rthresh=0.7

If rho is less than rthresh, the match is rejected.

.vari qthresh
Composite match quality threshold.

    qthresh=0.6

If q is less than qthresh, the match is rejected.

.vari redo
Forces resloc to ignore any previous RDB entry and redo the location of
the reseau.

.vari noin
Suppresses interpolation of the reseau mark locations to sub-pixel accuracy.

.vari nofill
Suppresses filling in off missing reseau marks.  Do not use this keyword if
you are planning to input the results to geoma.

.vari tres
All intermediate calculations for the test reseau mark are printed.

  tres=n   where n=1 to 202

.vari ores
Create an overlay of the reseau locations.

  ores=filename

where filename is a copy of the input image with the pixel positions of the
reseau marks overlayed in graphics.

.vari onom
  onom=filename
where filename is a copy of the input image with the nominal positions of the
reseau marks overlayed in graphics.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstresloc.pdf
!Test of Voyager program resloc.
!
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let $echo="no"
let _onfail="continue"

!Copy the test image to Io...
if ($syschar(1) = "unix")
  copy /project/test_work/testdata/mipl/vgr/f1636832.raw Io
end-if
if ($syschar(1) = "vax_vms")
  copy wms_test_work:[testdata.mipl.vgr]f1636832.raw Io
end-if

tst_defaults
tst_parameters
tst_calibration
tst_rdb

let $echo="yes"

! clean up:
if ($syschar(1) = "unix")
  ush rm -f Io
  ush rm -f Io_after
  ush rm -f res
  ush rm -f res.overlay
  ush rm -f geo
  ush rm -f G
  ush rm -f RDB
  ush rm -f OS
end-if

end-proc
$!-----------------------------------------------------------------------------
$ create tst_defaults.pdf
!Test resloc in default mode...
!
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let $echo="no"
let _onfail="continue"

resloc Io (res,geo)

write "Contents of res..."
label-list res
ibis-list res nr=1 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5)

write "Contents of geo..."
label-list geo
ibis-list geo nr=10 nc=4 cols=(1,2,3,4) 

!Check res by printing mark 102 before and after running ressar77
ressar77 (Io,res) Io_after
list Io (396,476,11,11)       	!mark 102 before removal
list Io_after (396,476,11,11)   !mark 102 after removal

!Check geo by geometrically transforming the image into object space...
geoma (Io,geo) OS
list OS (496,588,11,11)         !print the same reseau mark in object space

!view Io_after and OS by using xvd...
end-proc
$!-----------------------------------------------------------------------------
$ create tst_parameters.pdf
!Test resloc parameters...
!
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let $echo="no"
let _onfail="continue"

resloc Io res +
  'nofilter 'noin 'nofill +
  frame=1 camera=7 +
  'dbug +
  'pnom +
  'print +
  nlw=7 nsw=7 +
  sigma=1.2 +
  nver=21 nhor=21 +
  tres=1 +
  'pstats +
  rthresh=0.6 +
  qthresh=0.65 +
  'pres +
  ores=res.overlay

end-proc
$!-----------------------------------------------------------------------------
$ create tst_calibration.pdf
!Test resloc as part of a procedure for performing both radiometric and
!geometric calibration...

procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="no"
local DIR  string init="WMS_TEST_WORK:[TESTDATA.VGR]"
if ($syschar(1) = "UNIX")
  let DIR="/project/test_work/testdata/vgr/"
end-if

!  D --> R --> G

!Create radiometrically calibrated image 1134710.fic ...
!ficor77 (&"DIR"1134710.cln,&"DIR"v2_wa_ora.calib,&"DIR"dc.wa_5to1) R +
!       scf=&"DIR"vgrscf.dat
! this step is temporarily removed because ficor77 crashes on 32-bit Linux,
! probably due to a compiler bug.  The file R has been replaced by the
! file 1134710.fic in the test directory  / L.Kamp (16Apr2013)

!Locate the reseau and output geometric tiepoints geo...`
resloc (&"DIR"1134710.cln,&"DIR"cresn.fil) (res,geo)

!Create geometrically calibrated image G...
!geoma (R,geo) G nl=1000 ns=1000 format="HALF"
! above replaced by:
geoma (&"DIR"1134710.fic geo) G nl=1000 ns=1000 format="HALF"
hist G
end-proc
$!-----------------------------------------------------------------------------
$ create tst_rdb.pdf
!Test support for reseau database (RDB)....
!
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="no"
local RDB0 string
if ($syschar(1) = "unix")
  let RDB0="/project/test_work/testdata/mipl/vgr/reseau.test"
end-if
if ($syschar(1) = "vax_vms")
  let RDB0="wms_test_work:[testdata.mipl.vgr]reseau.test"
end-if

!Delete any record of Io frame from RDB"
rowop &RDB0 RDB keycol=1 range=(1636832,1636832) mode=delete

write "Contents of RDB before resloc..."
ibis-list RDB nr=7 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5) sr=19

!Find res and store in RDB...
resloc inp=(Io,RDB) out=res

write "Contents of RDB after resloc..."
label-list RDB
ibis-list RDB nr=8 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5) sr=19

write "Contents of res, as extracted from RDB..."
resloc inp=(Io,RDB) out=res 'pres
end-proc
$ Return
$!#############################################################################
