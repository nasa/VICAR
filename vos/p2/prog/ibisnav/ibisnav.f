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
