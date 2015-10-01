$!****************************************************************************
$!
$! Build proc for MIPL module ibisnav
$! VPACK Version 1.9, Friday, November 01, 2002, 15:53:32
$!
$! Execute by entering:		$ @ibisnav
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
$ write sys$output "*** module ibisnav ***"
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
$ write sys$output "Invalid argument given to ibisnav.com file -- ", primary
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
$   if F$SEARCH("ibisnav.imake") .nes. ""
$   then
$      vimake ibisnav
$      purge ibisnav.bld
$   else
$      if F$SEARCH("ibisnav.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisnav
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisnav.bld "STD"
$   else
$      @ibisnav.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisnav.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisnav.com -mixed -
	-s ibisnav.f -
	-p ibisnav.pdf -
	-i ibisnav.imake -
	-t tstibisnav.pdf gll_img_lst.unx vgr_img_lst.unx gll_img_lst.vms -
	   vgr_img_lst.vms cas_img_lst.unx cas_img_lst.vms
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisnav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c-----program ibisnav
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none

C     ....IBIS File Format
      character*6 format(38)/'FULL',2*'A4  ',10*'REAL',
     +  2*'A4  ',16*'REAL',6*'FULL','A4  '/

C     ....Data for IBIS records (max # of records=50)
      integer*4 sclk(50),scet(6,50),scet_ibis(50,6),isn(50)
      real*4 oma(50),omb(50),omc(50)
      real*4 rs1(50),rs2(50),rs3(50)
      real*4 num(50),sn(50)
      real*4 fl(50),ol(50),os(50),sc(50)
      real*4 rp(50),re(50)
      character*4 src(50)

C     .... CMSOURCE isource representation
      character*4 CMSOURCE_ISOURCE(0:7) /'SEDR','DAVI','FARE','NAV ',
     +                                   'NAV2','NEAR','AMOS','NAIF'/

      real*4 buf(200)		!SPICE buffer
      real*8 buf8(100)
      equivalence (buf,buf8)

      real*8 d(3),om(9)
      integer n,unit,nsclk
      integer*4 system,mode,count,def,ind,sc_id
      character*4 source
      character*5 project
      character*5 camera
      character*12 target
      character*4 SPKID_Used(50)
      character*10 usr_info(10)
      integer IBIS      ! IBIS file unit
      logical error

      call xvmessage('IBISNAV version Oct 31, 2002',' ')
      call get_params(project,nsclk,sclk,scet,isn)

C      ....Get camera constants
      do n=1,nsclk
         call getcamcon(project,isn(n),fl(n),ol(n),os(n),sc(n),ind)
         sn(n) = isn(n)
         if (ind.ne.0) goto 903
      enddo

C     ....Set up call to getspice95
      call xvparm('TARGET',target,count,def,0)
      call ucase(target,target)
      call get_source(nsclk,src)
      call get_user_info(usr_info,mode)
      if (project.eq.'CASSI') then		!Cassini
         sc_id = -82
         system = 1
      endif
      if (project.eq.'GLL  ') then		!Galileo
         sc_id = -77
         system = 1
      endif
      if (project.eq.'VGR-1') then		!Voyager 1
         sc_id = -31
         system = 2
      endif
      if (project.eq.'VGR-2') then		!Voyager 2
         sc_id = -32
         system = 2
      endif

C     ....Loop through each image to get SPICE data.
      do 100 n=1,nsclk
      call xvmessage('************************',0)
      call get_camera_name(project,isn(n),camera)
      call prnt(4,1,sclk(n),'sclk=.')
      call prnt(4,6,scet(1,n),'SCET:y,d,h,m,s,msec.')
      source = src(n)
      call getspice95(mode,sc_id,camera,scet(1,n),target,
     &         system,source,usr_info,buf,ind)
      if (ind.ne.1) goto 910
      call cmsource(buf,ind)
      src(n) = cmsource_isource(ind)
      rp(n) = buf8(15)
      re(n) = (buf8(13)+BUF8(14))/2.0
      call prnt(8,3,buf8(22),' RS vector=.')
      rs1(n) = buf8(22)
      rs2(n) = buf8(23)
      rs3(n) = buf8(24)
      call mve(8,9,buf8(59),om,1,1)		!OM matrix
      call toeuler(om,d(1),d(2),d(3),error)	!Convert to euler angles
      if (error) go to 997
      call prnt(8,3,d,' OM angles=.')
      oma(n) = d(1)
      omb(n) = d(2)
      omc(n) = d(3)
      num(n) = n				    !index number
      scet_ibis(n,1)=scet(1,n)
      scet_ibis(n,2)=scet(2,n)
      scet_ibis(n,3)=scet(3,n)
      scet_ibis(n,4)=scet(4,n)
      scet_ibis(n,5)=scet(5,n)
      scet_ibis(n,6)=scet(6,n)
      call mvlc(buf(14),SPKID_Used(N),4)
  100 continue

c     ....Write IBIS output file.
      call xvunit(unit,'OUT',1,ind,' ')
      call ibis_file_open(unit,IBIS,'write',38,nsclk,format,' ',ind)
      if ( ind.ne.1) call ibis_signal_u(unit,ind,1)
      call ibis_column_write(IBIS,sclk,1,1,nsclk,ind)!sclk
      call ibis_column_write(IBIS,SRC,2,1,nsclk,ind)!SEDR SOURCE
      call ibis_column_write(IBIS,SN,4,1,nsclk,ind) !SN
      call ibis_column_write(IBIS,RS1,5,1,nsclk,ind)!RS  COL 5
      call ibis_column_write(IBIS,RS2,6,1,nsclk,ind)
      call ibis_column_write(IBIS,RS3,7,1,nsclk,ind)
      call ibis_column_write(IBIS,OMA,8,1,nsclk,ind)!OM  COL 8
      call ibis_column_write(IBIS,OMB,9,1,nsclk,ind)
      call ibis_column_write(IBIS,OMC,10,1,nsclk,ind)
      call ibis_column_write(IBIS,NUM,20,1,nsclk,ind)!FRAME INDEX
      call ibis_column_write(IBIS,OMA,21,1,nsclk,ind)!new OM  COL 21
      call ibis_column_write(IBIS,OMB,22,1,nsclk,ind)!copys of old
      call ibis_column_write(IBIS,OMC,23,1,nsclk,ind)
      call ibis_column_write(IBIS,RP,26,1,nsclk,ind)	!DUMMY COL 26
      call ibis_column_write(IBIS,RE,27,1,nsclk,ind)	!DUMMY COL 27
      call ibis_column_write(IBIS,FL,28,1,nsclk,ind)	!DUMMY COL 28
      call ibis_column_write(IBIS,OL,29,1,nsclk,ind)	!DUMMY COL 29
      call ibis_column_write(IBIS,OS,30,1,nsclk,ind)	!DUMMY COL 30
      call ibis_column_write(IBIS,SC,31,1,nsclk,ind)	!DUMMY COL 31
      call ibis_column_write(IBIS,scet_ibis(1,1),32,1,nsclk,ind)
      call ibis_column_write(IBIS,scet_ibis(1,2),33,1,nsclk,ind)
      call ibis_column_write(IBIS,scet_ibis(1,3),34,1,nsclk,ind)
      call ibis_column_write(IBIS,scet_ibis(1,4),35,1,nsclk,ind)
      call ibis_column_write(IBIS,scet_ibis(1,5),36,1,nsclk,ind)
      call ibis_column_write(IBIS,scet_ibis(1,6),37,1,nsclk,ind)
      call ibis_column_write(IBIS,SPKID_Used,38,1,nsclk,ind)
      call ibis_file_close(IBIS,' ',ind)
      if (ind.ne.1) call ibis_signal(IBIS,ind,1)
      return

  903 call xvmessage('***Invalid camera serial number',0)
      call abend
  910 call xvmessage('***GETSPICE95 error',0)
      call abend
  997 call xvmessage('***TOEULER error',0)
      call abend
      end	
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get project, sclk, scet, isn
C
      subroutine get_params(project,nsclk,sclk,scet,isn)
      implicit none
      character*5 project	!VGR-1, VGR-2, GLL, CASSI
      integer nsclk		!Number of images
      integer*4 sclk(50)	!Spacecraft clock
      integer*4 scet(6,50)	!Galileo SCET
      integer*4 isn(50)		!Camera serial number

      character*80 filename	!Name if FILENAME file
      character*80 name(50)	!Filenames read from FILENAME file
      byte iname(80,50)
      equivalence (iname,name)

      integer i,j,icnt,idef,ind,n
      integer*4 unit,labdata(80)
  300 format(80A1)

      call xvparm('PROJECT',project,n,idef,' ')
      call ucase(project,project)
      call xvparm('FILENAME',filename,n,idef,' ')
      if (filename.eq.'NOFILE') goto 100

C     ....If filenames are input from file, get the SCLK, SCET, and
C     ....camera serial numbers from the image labels.
C     ....First, read the filenames from file.
      open(unit=3,file=filename,status='OLD',err=910,
     +         access='SEQUENTIAL',form='FORMATTED')
      do i=1,50
         read(3,300,iostat=ind,ERR=911,END=20) (iname(j,i),j=1,80)
      enddo

   20 close(unit=3)
      nsclk = i - 1
      call prnt(4,1,nsclk,'# of input files located =.')

      do i=1,nsclk
         if (project.ne.'CASSI') 
     +		call file_for_host(name(i),name(i),ind)
         call xvunit(unit,'OLD',n,ind,'U_NAME',name(i),' ')
         call xvsignal(unit,ind,.true.,' ')
         call xvopen(unit,ind,'OPEN_ACT','SA',
     +                'IO_ACT','SA','OP','READ',' ')
         call xvsignal(unit,ind,.true.,' ')
         call getlabcon(unit,project,labdata,ind)
         if (ind.gt.1) goto 920
         sclk(i)=labdata(2)
         isn(i)=labdata(6)
         scet(1,i)=labdata(8)
         scet(2,i)=labdata(9)
         scet(3,i)=labdata(10)
         scet(4,i)=labdata(11)
         scet(5,i)=labdata(12)
         scet(6,i)=labdata(13)
         if (scet(1,i).lt.100) scet(1,i)=scet(1,i)+1900
         if (scet(6,i).lt.0) scet(6,i)=0
         call xvclose(unit,ind,' ')
      enddo
      goto 1000

C     ....Here if filenames are not provided.  Get the SCLK, SCET, and
C     ....camera serial numbers from user parameters.
  100 call xvparm('SCET',scet,nsclk,idef,' ')
      nsclk=nsclk/6
      if (nsclk.eq.0) goto 990
      call xvparm('CAMERA',isn,icnt,idef,' ')
      if (icnt.ne.nsclk) goto 992
      do i=1,nsclk
         sclk(i) = i		!dummy sclks
      enddo
      goto 1000

  910 call xvmessage('***Error opening FILENAME file',0)
      call abend
  911 call xvmessage('***Err reading FILENAME file '//filename,0)
      call prnt(4,1,ind,'FORTRAN IOSTAT error # =.')
      call abend
  920 call xvmessage('***Err reading image label',0)
      call abend
  990 call xvmessage('***GLL requires SCET parameter',0)
      call abend
  992 call xvmessage('***Require one CAMERA value per image',0)
      call abend
 1000 do i=1,nsclk
        call chk_scet_date(scet(1,i),idef)
        if (idef .ne. 1) then
           call xvmessage('ERROR: Incorrect format for scet.',' ')
           call abend
        endif
      enddo
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Return the camera name.
c
      subroutine get_camera_name(project,isn,camera)
      implicit none
      integer*4 isn		!input camera serial number
      character*5 project	!input project name
      character*5 camera	!output camera name

      if (project.eq.'CASSI') then
         if (isn.eq.1) camera='ISSN'
         if (isn.eq.21) camera='ISN2'
         if (isn.eq.41) camera='ISN4'
         if (isn.eq.2) camera='ISSW'
         if (isn.eq.22) camera='ISW2'
         if (isn.eq.42) camera='ISW4'
      endif

      if (project.eq.'GLL  ')then
         if (isn.eq.2) then  ! Summation Mode Image
            camera = 'SSI2'
         else                   ! Full Frame Mode Image
            camera = 'SSI1'
         end if
      endif

      if (project.eq.'VGR-1') then		!Voyager 1
         if (isn.ne.6 .and. isn.ne.7) goto 990
         camera = 'ISSW'
         if (isn.eq.7) camera='ISSN'
      endif

      if (project.eq.'VGR-2') then		!Voyager 2
         if (isn.ne.4 .and. isn.ne.5) goto 990
         camera = 'ISSW'
         if (isn.eq.5) camera='ISSN'
      endif
      return

  990 call xvmessage('***Invalid camera serial number',' ')
      call abend
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Get C-kernel source from CKNAME parameter.
c
      subroutine get_source(nsclk,src)
      implicit none
      integer nsclk
      character*4 src(50)

      integer nsrc,j

      call xvpcnt('CKNAME',nsrc)
      if (nsrc.eq.0) then
         do j=1,nsclk
            src(j) = 'DAVI'
         enddo
         call xvmessage(' Using best available C-kernel source',0)
         return
      endif

      if (nsclk.ne.nsrc) goto 990
      do j=1,nsclk
         call xvpone('CKNAME',src(j),j,4)
         call ucase(src(j),src(j))
      enddo
      return

  990 call qprint('***Error in number of CKNAME values',0)
      call qprint('***Enter one for each SCET or none at all',0)
      call abend
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Set up getspice95 usr_info and mode from user parameters.
c
      subroutine get_user_info(usr_info,mode)
      implicit none
      character*10 usr_info(10)
      integer mode

      integer count,def
      character*12 cdate
      character*4 year,month_day,hour_minute
      character*6 userid
      character*3 groupid
      character*11 default
      logical xvptst

      call xvparm('INSTITUTE',usr_info(1),count,def,0)
      call xvparm('PURPOSE',usr_info(2),count,def,0)
      call xvparm('PROGRAM',usr_info(3),count,def,0)
      call xvparm('SPKID',usr_info(4),count,def,0)
      call xvparm('REQNUM',usr_info(5),count,def,0)
      call xvparm('CDATE',cdate,count,def,0)
      year = cdate(1:4)
      month_day = cdate(5:8)
      hour_minute = cdate(9:12)
      usr_info(6) = year
      usr_info(7) = month_day
      usr_info(8) = hour_minute
      usr_info(9) = 'NONE'
      userid = '*NONE*'
      call xvparm('USERID',userid,count,def,0)
      call xvparm('GROUPID',groupid,count,def,0)
      if (count.eq.1) userid(4:6) = groupid(1:3)
      usr_info(10) = userid
 
      if (xvptst('REMOTE')) then
         mode = 1
      else if (xvptst('LOCAL')) then
         mode = 0
      else
         call xgetenv_vic('DEFAULTSPICE',default)
         mode = 0
         if (default.eq.'REMOTESPICE') mode=1 
      endif          

      return
      end

c**********************************************************************
c
	subroutine TOEULER (c, alpha, delta, kappa, error)
	implicit none
	real*8	c(3,3)      ! input - derived rotation matrix 
	real*8	alpha       ! output  - ra of z axis (degrees)
	real*8	delta	    ! output  - declination z axis (degrees)
	real*8	kappa	    ! output  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)

c  this routine performs the functional inverse of routine fromeuler.
c  this routine takes an input rotation matrix, and computes the three euler
c  angles representing the matrix.  (these 3 angles are called alpha, delta,
c  and kappa by mert davies etc.)  if the matrix is not a valid rotation
c  matrix (i.e. if the length of the row and column vectors is not within
c  0.0001 of unity) then error is returned true.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

	real*8 collength,rowlength,rpd,dpr
	integer i, j
	logical	error

	error = .false.
	do i = 1, 3
	    collength = 0.0
	    rowlength = 0.0
	    do j = 1, 3
		collength = collength + c(i,j)**2
		rowlength = rowlength + c(j,i)**2
	    enddo
	    if (abs(collength-1.0) .gt. 0.0001) error = .true.
	    if (abs(rowlength-1.0) .gt. 0.0001) error = .true.
	enddo

	if (.not. error) then
	    delta = asin(c(3,3))*dpr()
	    if (c(3,1) .ne. 0.0) then
		alpha = atan2(c(3,2), c(3,1))*dpr()
	    else if (c(3,2) .eq. 0.0) then
		alpha = 0.0
	    else
		alpha = sign(dble(90.0),c(3,2))
	    endif
	    if (alpha .lt. 0.0) alpha = alpha + 360.0
	    if (c(2,3) .ne. 0.0) then
		kappa = atan2(c(1,3), c(2,3))*dpr()
	    else if (c(1,3) .eq. 0.0) then
		kappa = atan2( -c(1,1), -c(2,1) )*dpr()
	    else
		kappa = sign(dble(90.0),c(1,3))
	    endif
	    if (kappa .lt. 0.0) kappa = kappa + 360.0
	    if (abs(cos(rpd()*delta))*cos(rpd()*kappa)-c(2,3).gt.0.0001) 
     +		kappa = 180.0 - kappa
	endif

	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ibisnav.pdf
process help=*
PARM OUT 	STRING 		 COUNT=1
PARM PROJECT    TYPE=(STRING,5)  COUNT=1 VALID=("VGR-1","VGR-2","GLL  ","CASSI")
PARM TARGET     TYPE=(STRING,12) COUNT=1
PARM FILENAME   TYPE=(STRING,80) COUNT=(0:1)                    DEFAULT="NOFILE"
PARM SCET       INTEGER          COUNT=(0:300)                  DEFAULT=--
PARM CKNAME   	TYPE=(STRING,4)	 COUNT=0:50 	                DEFAULT=--    +
		VALID=("DAVI","NAV ","FARE","AMOS","NEAR","NAIF")
PARM CAMERA 	INTEGER 	 COUNT=(0:50)	VALID=(1:42)     DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
END-PROC
.TITLE
IBISNAV - Get SPICE geometry data for a list of images
.HELP
PURPOSE:  IBISNAV retrieves the SPICE geometry data for a given list of images
and stores the data in an IBIS interface file.  The program is used with
programs MANMATCH, OMCOR, and IBISUPDATE to register images for mosaics and
color reconstruction.

EXECUTION:

  IBISNAV  OUT=ibisfilename  PROJECT=projectname  TARGET=targetname +
        FILENAME=file
where
  OUT is the output IBIS interface file,
  PROJECT is the project name (CASSI, GLL, VGR-1, VGR-2),
  TARGET is the name of the target body (planet, satellite, or asteroid),
  FILENAME is a file which contains a list of images

The label of each image is read to extract the spacecraft-event-time (SCET)
and camera serial number.  Alternatively, these data may be input directly via
parameters as follows:

  IBISNAV  OUT=ibisfilename  PROJECT=projectname  TARGET=targetname +
        SCET=(1998,212,18,54,32,213,+
	      1998,212,19,2,30,214, +
	      1998,212,19,10,28,212)
        CAMERA=(4,4,4)
where
  SCET is a list of spacecraft-event-times entered as groups of 6 integers
     representing year, day, hour, minute, second, and millisecond.
  CAMERA is a list of camera serial numbers, one for each SCET.

.page
OPERATION:

IBISNAV collects all the geometry data for a list of images and stores the
data in an IBIS interface file.  The list of images is usually provided via
an ASCII text file (see FILENAME parameter).  The label of each image is
read to extract the spacecraft-event-time (SCET) and camera serial
number.

Alternatively, the SCET and camera serial number of each image may be input
directly via the SCET and CAMERA parameters.

The SCET and camera serial numbers are required to retrieve the image geometry
information from the SPICE kernels.

In addition, the camera serial numbers are used to retrieve the camera 
constants (via a call to getcamcon).

.page
EXAMPLE:

    IBISNAV  OUT=IBIS.INT  PROJECT=GLL  TARGET=JUPITER +
	FILENAME="/home/gmy/mosaicfiles.dat"

or

    IBISNAV  OUT=IBIS.INT  PROJECT=GLL  TARGET=JUPITER +
         SCET=(1990,44,5,58,16,962, +
               1990,44,7,58,36,296)
         CAMERA=(1,1)


.page
IBIS SPICE FILE FORMAT: The output interface file has 37 "columns" and N "rows", where N is the number
of frames requested.  Thus, the output file has a VICAR label followed by
38 records padded out to 512 bytes.


 Index  Content                                       Data Type
 -----  --------------------------------------------- ----------------------
 1      frame id (fds count)                   		i*4
 2      ckname (NAIF,FARE,NAV,NEAR)			char*4
 3      (not used)
 4      camera s/n                             		r*4
 5-7    rs vector, planetary coords            		3 (r*4)
 8-10   om matrix  angles, plan -> camera	      	3 (r*4)
 11-13  me matrix angles, plan -> EME50  (NOT USED)	3 (r*4)
 14-15  archive tape location    (NOT USED)    		2 (char*4)
 16     lat of p5 point (geodetic)  (NOT USED)  	r*4
 17     lon of p5 point (geodetic)  (NOT USED) 		r*4
 18     range to pb center, km      (NOT USED) 		r*4
 19     filter number        (NOT USED)        		r*4
 20     frame index number                     		r*4
 21-23  updated om angles                      		3 (r*4)
 24     avg pix scale p5 point,km/pix   N/U	    	r*4
 25     DUMMY						r*4
 26     POLAR RADIUS, KM				r*4
 27     EQUAT RADIUS, KM				r*4
 28     CAMERA FOCAL LENGTH, MM				r*4
 29     OPTICAL AXIS LINE, OBJ. SPACE PIXELS		r*4
 30     OPTICAL AXIS SAMP, OBJ. SPACE PIXELS		r*4
 31     SCALE, PIXELS/MM				r*4
 32     scet year                               	i*4
 33     scet day                                	i*4
 34     scet hour                               	i*4
 35     scet min                                	i*4
 36     scet sec                                	i*4
 37     scet msec                               	i*4
 38     SPK kernel id                                 char*4

.page
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels:

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

The CKNAME parameter is used to specify  the C kernel to be used.  For
example, CKNAME=(FARE,NAV,DAVI) specifies that MIPS_FARENC.CK, MIPS_NAV, and
MIPS_DAVI are the preferred sources for images 1, 2, and 3 respectively.
If the camera pointing data is not found in the requested C kernel, the other
C kernels are searched.

Within a given C kernel, there may be a number of different versions of camera
pointing for a given image.  The segment identifier for each version contains
provenance information identifying the creator of the pointing data.  One or
more of the following parameters may be used to retrieve a specific instance of
camera pointing based upon this provenance information:

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

The above parameters are optional.  Note that if specified, the provenance
paramater must apply to ALL images processed.  For example, if CDATE is used
to search for camera pointing, the same CDATE should apply to all images.

If provenance data are defaulted (or if no data is found for the requested
version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Example: 'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

.page
RESTRICTIONS:

IBISNAV is currently limited to 50 frames per execution.  However, IBIS programs
can be used to concatenate several output files, if more than 50 are desired.

File names contained in the file specified by the FILENAME parameter are
limited to 80 characters.

.page
PROGRAM HISTORY:

Written by:         C. AVIS, 2/89
Current Cognizant programmer:  Gary Yagi

Revisions:
  When      Who  What
  --------- ---  --------------------------------------------------------------
  31 Oct 02 GMY  * Fix Linux compile errors.  Update to support Cassini.
  08 Sep 98 RRP  * Added check for scet. (Y2K FRD<901>)
  24 Aug 98 GMY  * Modified to work on VGR SPICE data.
  01 May 97 TXH  * Fixed SPICE source being output to the second (2) column.

  01 May 97 SMC  * Corrected VGRSPICE calling sequence so that the data type of
                   camera matches.                                    (FR90073)
                 * Corrected VGRSPICE so the Target ID of SPICE buffer (index
                   9 of integer*4) is converted to GLL format, this fix
                   affects the program output in no way since it does not use
                   this particular piece of buffer.                   (DFR)
                 * Used standard string assignment to copy SEDR source string
                   instead of the MVLC call which failed in Coda1     (DFR)
  12 Nov 96 SMC  * Modified to add a SPKID (char*4) field to the IBIS output
                   file.  Mainly for the purpose of IBISUPDATE        (FR89225)
                 * Fixed C-Matrix source column.
  20 Aug 96 SMC  * Modified GETSPICE95 call to handle Summation Mode images.
                   This is done by calling GETSPICE95 with camera of 'SSI1'
                   for Full Frame, and 'SSI2' for Summation Mode.     (DFR)
                 * Use CAMERA to signal GLL Summation Mode            (DFR)
  20 Jun 96 SMC  * Allow different CAMERAs to be specified for multiple FDS
                 * Allow VGR processing by calling VGRSPICE instead of
                   GETSPICE95.  This part will not be necessary once Voyager
                   SEDR converts to SPICE.
  23 May 95 GMY  Updated to GETSPICE2 interface
  circa  95 BAM  ported to UNIX and Alpha

.level1

.VAR OUT
Output VICAR IBIS file

.VAR FILENAME
Optional
list of input
filenames

.VAR CKNAME
List of CKNAME sources

.VAR CAMERA
List Camera IDs for each frames specified
Not required if 
FILENAME keyword used.

.VAR SCET
The spacecraft event 
time for each frame 
in the mosaic.
Not required if 
FILENAME keyword used.

.VAR PROJECT
The project ID.
Valid values:
VGR-1  VGR-2  GLL

.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
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

.level2

.VAR OUT
STRING--A VICAR labelled file with IBIS data structure.  See help file
for description of file format.

.VAR FILENAME
The name of an ASCII text file containing a list of filenames:

	/home/gmy/s001245663.img
	/project/gll/s004455667.abc
	/project/gll/s987654123.img

The file may be created via a text editor.

A maximum of 50 images can be included, one per line.  The filename cannot
exceed 80 characters.  The list should contain images of the same target and
be listed in the order of priority for the mosaic.

The FILENAME parameter overrides the SCET and CAMERA parameters.

.VAR CKNAME
STRING - A list of C kernel types, one for each image, indicating the preferred
source of the SPICE camera pointing information.

Valid values are "DAVI","NAV ","FARE","NAV2","NEAR","AMOS", and "SEDR".

If CKNAME is defaulted, or if the preferred source is not available,
the best available camera pointing will be substituted (in the order listed
above).

.VAR CAMERA
INTEGER - List of camera serial numbers, one for each image.
Not required if FILENAME keyword used.

Voyager camera serial numbers:

        4 = VGR-2 WA            6 = VGR-1 WA
        5 = VGR-2 NA            7 = VGR-1 NA

Galileo SSI camera serial numbers:

	1 = full frame
	2 = summation mode

Cassini ISS camera serial numbers:

        1=NAC full frame	2=WAC full frame
	21=NAC 2x2 summation	22=WAC 2x2 summation
	41=NAC 4x4 summation	42=WAC 4x4 summation

.VAR SCET
Integer - The spacecraft event time for each frame in the mosaic.
Not required if FILENAME keyword used.

The SCETs are entered as groups of 6 integers, representing the year, day
hour, minute, second, and millisecond:

        SCET=(1998,212,18,54,32,213,+
	      1998,212,19,2,30,214, +
	      1998,212,19,10,28,212)

.VAR PROJECT
String - The project ID. Valid are:
VGR-1  VGR-2  GLL

.VARI TARGET
REQUIRED PARAMETER

Name of target body (planet, satellite, or asteroid).

Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be retrieved from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be retrieved
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.  Note that if SPICE data is not found in LOCAL or REMOTE mode,
the other mode is attempted.

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
$ create ibisnav.imake
#define PROGRAM ibisnav
#define MODULE_LIST ibisnav.f
#define MAIN_LANG_FORTRAN
#define R2LIB
#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_SPICE
#define LIB_MATH77
#define LIB_NETWORK
$ Return
$!#############################################################################
$Test_File:
$ create tstibisnav.pdf
! Test of program IBISNAV
procedure
  RefGbl $echo
  RefGbl $syschar
body
  local VgrImgList
  local GllImgList
  local CasImgList
  if ($SysChar(1)="VAX_VMS")
   let VgrImgList="vgr_img_lst.vms"
   let GllImgList="gll_img_lst.vms"
   let CasImgList="cas_img_lst.vms"
  else
   let VgrImgList="vgr_img_lst.unx"
   let GllImgList="gll_img_lst.unx"
   let CasImgList="cas_img_lst.unx"
  end-if

!***************** Begin Voyager Test ***********************"
 IBISNAV table.ibis  PROJECT=VGR-1 TARGET=IO +
    CKNAME=(NAIF,NAIF,"FARE",NAIF) +
    SCET=(1979,63,19,22,57,926, +
	  1979,63,19,24,33,926, +
	  1979,63,19,26,9,926, +
	  1979,63,19,27,45,926) +
    CAMERA=(7,7,7,7)

 LABEL-LIST table.ibis
 IBIS-LIST table.ibis

 IBISNAV table.ibis FILENAME=&"VgrImgList" PROJECT=VGR-1 TARGET=JUPITER
 LABEL-LIST table.ibis
 IBIS-LIST table.ibis

!***************** Begin Gailileo Test ***********************"
 IBISNAV OUT=table.ibis CKNAME=(FARE,FARE) TARGET=venus PROJECT=GLL +
         SCET=(1990,44,5,58,16,962, +
               1990,44,7,58,36,296) CAMERA=(1,1)
 LABEL-LIST table.ibis
 IBIS-LIST table.ibis

 IBISNAV table.ibis FILENAME=&"GllImgList" TARGET=earth PROJECT=GLL
 LABEL-LIST table.ibis
 IBIS-LIST table.ibis

!****************** Test summation mode image ********************"
 IBISNAV table.ibis TARGET=Ganymede PROJECT=GLL CAMERA=(2,2,2,2) +
         SCET=(1996,178,8,47,5,459,  +
               1996,178,8,46,4,893,  +
               1996,178,8,46,20,126, +
               1996,178,8,46,35,293)
 LABEL-LIST table.ibis
 IBIS-LIST table.ibis

!***************** Begin Cassini Test ***********************"
 IBISNAV table.ibis  PROJECT=CASSI TARGET=JUPITER +
    SCET=(2000,342,16,10,56,162, +
	  2001,2,1,4,40,72,+
	  2000,348,4,2,5,505) +
    CAMERA=(1,22,41)
 LABEL-LIST table.ibis
 IBIS-LIST table.ibis

 IBISNAV table.ibis FILENAME=&"CasImgList" PROJECT=CASSI TARGET=JUPITER
 LABEL-LIST table.ibis
 IBIS-LIST table.ibis
end-proc
$!-----------------------------------------------------------------------------
$ create gll_img_lst.unx
/project/test_work/testdata/mipl/gll/ptp_gll_red.img
/project/test_work/testdata/mipl/gll/ptp_gll_grn.img
/project/test_work/testdata/mipl/gll/ptp_gll_blu.img
/project/test_work/testdata/mipl/gll/earth1.img
/project/test_work/testdata/mipl/gll/earth2.img
$!-----------------------------------------------------------------------------
$ create vgr_img_lst.unx
/project/test_work/testdata/mipl/vgr/ptp_vgr_red.img
/project/test_work/testdata/mipl/vgr/ptp_vgr_blu.img
$!-----------------------------------------------------------------------------
$ create gll_img_lst.vms
WMS_TEST_WORK:[TESTDATA.MIPL.GLL]ptp_gll_red.img
WMS_TEST_WORK:[TESTDATA.MIPL.GLL]ptp_gll_grn.img
WMS_TEST_WORK:[TESTDATA.MIPL.GLL]ptp_gll_blu.img
WMS_TEST_WORK:[TESTDATA.MIPL.GLL]earth1.img
WMS_TEST_WORK:[TESTDATA.MIPL.GLL]earth2.img
$!-----------------------------------------------------------------------------
$ create vgr_img_lst.vms
wms_test_work:[testdata.mipl.vgr]ptp_vgr_red.img
wms_test_work:[testdata.mipl.vgr]ptp_vgr_blu.img
$!-----------------------------------------------------------------------------
$ create cas_img_lst.unx
/project/test_work/testdata/cassini/casIss/n1354897340.1
/project/test_work/testdata/cassini/casIss/w379.img
/project/test_work/testdata/cassini/casIss/n013.img
$!-----------------------------------------------------------------------------
$ create cas_img_lst.vms
wms_test_work:[testdata.cassini.cas$i$ss]n1354897340.1
wms_test_work:[testdata.cassini.cas$i$ss]w379.img
wms_test_work:[testdata.cassini.cas$i$ss]n013.img
$ Return
$!#############################################################################
