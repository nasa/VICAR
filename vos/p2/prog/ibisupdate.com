$!****************************************************************************
$!
$! Build proc for MIPL module ibisupdate
$! VPACK Version 1.9, Thursday, July 13, 2006, 12:31:35
$!
$! Execute by entering:		$ @ibisupdate
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
$ write sys$output "*** module ibisupdate ***"
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
$ write sys$output "Invalid argument given to ibisupdate.com file -- ", primary
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
$   if F$SEARCH("ibisupdate.imake") .nes. ""
$   then
$      vimake ibisupdate
$      purge ibisupdate.bld
$   else
$      if F$SEARCH("ibisupdate.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisupdate
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisupdate.bld "STD"
$   else
$      @ibisupdate.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisupdate.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisupdate.com -mixed -
	-s ibisupdate.f -
	-p ibisupdate.pdf -
	-i ibisupdate.imake -
	-t tstibisupdate.pdf tstibisupdate_ops.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisupdate.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR PROGRAM IBISUPDATE
C
        include 'VICMAIN_FOR'
	subroutine main44
        implicit none

C-------Constants
        integer OLDFILE ! The number of columns old IBISNAV (prior to 11/12/96)
                        ! file has
        PARAMETER (OLDFILE=37)

C-------Variables
	real*8 om(9),om0(9),oma8,omb8,omc8,d(3),buf8(100)

	real*4 oma(50),omb(50),omc(50),rs1(50),rs2(50),rs3(50)
	real*4 oma0(50),omb0(50),omc0(50)
	real*4 om4(9),c(3),data(40),conv(3600), r_isn(50)

C txh::corrected declaration of 'buf' from type 'real*4' to 'integer*4'
        integer*4 buf(200)

	integer*4 fds(50),scet(6,50),idata(40), isn(50)
        integer*4 scet_ibis(50,6),proj
        character*4 src(50), SPKID(50)
        integer iu,istat,ind,num,ifds,icnt,idef,SPKIDCount,SPKIDdef
        integer i,j,n
        integer NumOfColumn  ! This will serve as a IBISNAV file version
                             ! indicator

	logical*4 iupdate,xvptst,error
        character*4  sr
        character*26 msrc
        character*5 project

        integer IBIS
        equivalence (buf,buf8)
	equivalence (data,om),(data,idata)

        call xvmessage ('IBISUPDATE Version 06 July 2006',' ')
C
	sr = 'sedr'
	msrc = ' source of orig pc is xxxx'
        iupdate = xvptst('update')

	call xvparm('project',project,icnt,idef,1)
        call ucase(project,project)
 
        if(project.eq.'GLL  ')then
          proj = -77
          call mvcl('GLL',conv,4)	! for CONVEV call
        endif

	! ensure that the subroutines get the right name/code
	! whatever the user enters:
        if (project(1:3).eq.'CAS') then
	  project = 'CASSI'
	  proj = -82
	endif

        if(xvptst('object'))then
           idata(39)=8
        else
           if(project.eq.'GLL  ')then
              idata(39)=7
           else
              call  xvmessage('Only object sp supported if non-GLL',' ')
              call abend
           endif
        endif

C
C-------GET DATA FROM IBIS SEDR FILE
	call  xvunit(iu,'inp',1,istat,' ')
        call  ibis_file_open(iu,IBIS,'read',0,0,' ',' ',istat)
	if(istat .ne. 1) call ibis_signal_u(iu,istat,1)
        call ibis_file_get(IBIS,'NR',num,1,1)
        call ibis_file_get(IBIS,'NC',NumOfColumn,1,1)
	call prnt(4,1,num,' number of frames.')
        call ibis_column_read(IBIS,fds,1,1,num,istat)
        call ibis_column_read(IBIS,src,2,1,num,istat) ! orig source
        call ibis_column_read(IBIS,r_isn,4,1,num,istat) ! camera s/n
        call ibis_column_read(IBIS,rs1,5,1,num,istat)
        call ibis_column_read(IBIS,rs2,6,1,num,istat)
        call ibis_column_read(IBIS,rs3,7,1,num,istat)
        call ibis_column_read(IBIS,oma0,8,1,num,istat)! originals
        call ibis_column_read(IBIS,omb0,9,1,num,istat)
        call ibis_column_read(IBIS,omc0,10,1,num,istat)
        call ibis_column_read(IBIS,oma,21,1,num,istat)! updates
        call ibis_column_read(IBIS,omb,22,1,num,istat)
        call ibis_column_read(IBIS,omc,23,1,num,istat)

        CALL xvparm('SPKID',SPKID,SPKIDCount,SPKIDdef,50)
        IF (SPKIDdef.EQ.0 .AND. NumOfColumn.GT.OLDFILE) THEN
          call xvmessage('WARNING: Parameter SPKID ignored because',' ')
          call xvmessage('         input file is in new IBISNAV file',
     +                   ' ')
          call xvmessage('         file format', ' ')
        END IF
        IF (NumOfColumn.GT.OLDFILE) THEN
          call ibis_column_read(IBIS,SPKID, 38,1,num,istat)
        END IF


        if( project.eq.'GLL  '.or.project.eq.'CASSI') then
          call  xvparm('scet',scet,ifds,idef,500)
          ifds = ifds/6
          if (ifds.gt.0) then
            do n=1,ifds
              call chk_scet_date(scet(1,n),istat)
              if(istat .ne. 1) then
                call xvmessage('SCET HAS INCORRECT FORMAT.',' ')
                call xvmessage(' ',' ')
                call xvmessage('YEAR MUST BE 4-DIGITS.',' ')
                call xvmessage('DAY  MUST BE BETWEEN 1 AND 366',' ')
                call xvmessage('HOUR MUST BE BETWEEN 0 AND 23',' ')
                call xvmessage('MIN  MUST BE BETWEEN 0 AND 59', ' ')
                call xvmessage('SEC  MUST BE BETWEEN 0 AND 59',' ')
                call xvmessage('MSEC MUST BE BETWEEN 0 AND 999',' ')
C          Close file before abending.
                CALL ibis_file_close(IBIS,' ',istat)
                if (istat .ne. 1) call ibis_signal(IBIS,istat,1)
                call abend
              endif
            enddo
            goto 50
          endif

          call xvmessage('Obtaining SCET from ibis file cols:32-37',' ')
          ifds=num
          call ibis_column_read(IBIS,scet_ibis(1,1),32,1,num,istat)
          call ibis_column_read(IBIS,scet_ibis(1,2),33,1,num,istat)
          call ibis_column_read(IBIS,scet_ibis(1,3),34,1,num,istat)
          call ibis_column_read(IBIS,scet_ibis(1,4),35,1,num,istat)
          call ibis_column_read(IBIS,scet_ibis(1,5),36,1,num,istat)
          call ibis_column_read(IBIS,scet_ibis(1,6),37,1,num,istat)
          do i=1,ifds
            do j=1,6
              scet(j,i)=scet_ibis(i,j)
            enddo
          enddo
        endif


50      CALL ibis_file_close(IBIS,' ',istat)
        if (istat .ne. 1) call ibis_signal(IBIS,istat,1)
        IF (NumOfColumn.LE.OLDFILE .AND. SPKIDdef.EQ.0 .AND. 
     +      SPKIDCount.NE.ifds) THEN
          call mabend('Counts of parameter SPKID incorrect',' ')
        END IF

C-------Convert the camera s/n from real to integer
        DO n=1,num
          isn(n) = r_isn(n)           ! convert camera s/n from real to integer
        END DO
C-------Override Camera S/N if specified
        call xvparm('CAMERA',isn,icnt,idef,1)
        IF (idef.EQ.0 .AND. icnt.NE.ifds) THEN
          call mabend('Counts of parameter CAMERA incorrect',' ')
        END IF

C-------LOOP THROUGH EACH IMAGE IN IBIS FILE
	do 100 n=1,num
	call  xvmessage(' ',' ')
        if (project.eq.'GLL  ') then
          call prnt(4,6,scet(1,n),'SCET:y d h m s ms.')
        else
          call prnt(4,1,fds(n),' fds .')
        endif
C
C-------GET PREDICT SEDR RECORD FOR THIS IMAGE
	call msedr(ind,fds(n),isn(n),buf,buf8,data,
     +             scet(1,n),project,proj)
	if (ind .ne. 1) go to 998
	call toeuler(data,d(1),d(2),d(3),error)
	if (error) go to 997
C
C-------"d" in first character means debug (ffm)
	call prnt(8,3,d,' sedr angles.')
Cd	call prnt(8,3,d,' sedr angles.')
	d(1) = oma0(n)
	d(2) = omb0(n)
	d(3) = omc0(n)
	call prnt(8,3,d,' orig angles.')
Cd	call prnt(8,3,d,' orig angles.')
	if (oma(n) .eq. 0) then
		call  xvmessage(' NO OM UPDATE',' ')
	else
		d(1) = oma(n)
		d(2) = omb(n)
		d(3) = omc(n)
		call prnt(8,3,d,' updt angles.')
Cd		call prnt(8,3,d,' updt angles.')
	end if
C
	call prnt(8,3,buf8(22),' sedr rs.')
Cd	call prnt(8,3,buf8(22),' sedr rs.')
	c(1) = rs1(n) 
	c(2) = rs2(n)
	c(3) = rs3(n)
	call prnt(7,3,c,' updt rs.')
Cd	call prnt(7,3,c,' updt rs.')
	call convev(ind,data,data,c(1),c(2),data(31),data(32),1,conv)
	call prnt(7,2,c,' sedr planet center l,s=.')
C
C-------CONVERT ORIGNAL OM ANGLES TO R*8
	oma8 = oma0(n)
	omb8 = omb0(n)
	omc8 = omc0(n)
C-------COMPUTE ORIGINAL OM MATRIX ITSELF
	call buildcm(om,oma8,omb8,omc8)
	call mattran(om)
        call mve(8,9,om,om0,1,1)
	call convev(ind,data,data,c(1),c(2),data(31),data(32),1,conv)
	call prnt(7,2,c,' orig ibis center l,s=.')
	if (oma(n) .ne. 0) then
C-------CONVERT UPDATED OM ANGLES TO R*8
	   oma8 = oma(n)
	   omb8 = omb(n)
	   omc8 = omc(n)
C-------COMPUTE UPDATED OM MATRIX ITSELF
	   call buildcm(om,oma8,omb8,omc8)
	   call mattran(om)
	   call convev(ind,data,data,c(1),c(2),data(31),data(32),1,conv)
	   call prnt(7,2,c,' updt ibis center l,s=.')
	end if
        msrc(23:26) = src(n)
c	call mvlc(src(n),msrc(23:26),4)
	call  xvmessage(MSRC,' ')
C
C-------CONVERT THIS UPDATED OM TO R*4 FOR COMPARE AND STORAGE
	call prnt(8,9,buf8(59),' sedr om.')
Cd	call prnt(8,9,buf8(59),' sedr om.')
	call mve(-9,9,om0,om4,1,1)
	call prnt(7,9,om4,' orig om.')
Cd	call prnt(7,9,om4,' orig om.')
	if (oma(n) .ne. 0) then
	   call mve(-9,9,om,om4,1,1)
	   call prnt(7,9,om4,' updt om.')
Cd	   call prnt(7,9,om4,' updt om.')
C
	   if(iupdate) then
		call mve(8,9,om,buf8(59),1,1)
                IF (NumOfColumn.GT.OLDFILE) THEN
C txh::corrected the call from mvlc to mvcl
C                  call mvlc(SPKID(N),buf(14),4)
                   call mvcl(SPKID(N),buf(14),4)
                END IF
                call omtoc(buf8(59),buf8(50),buf8(41)) ! cmatrix
                call cmsource(buf,ind)
                call putspice2('NEAR','IBISUP',buf,ind)
                if (ind .eq. 1) then
	           call  xvmessage('SEDR UPDATED WITH SOURCE NEAR',' ')
		else
	           call  xvmessage(' SEDR NOT UPDATED',' ')
	           call prnt(4,1,ind,' ind =.')
		end if
           end if
        end if
C
100	continue
C
	return
997     call  xvmessage(' ERROR IN TOEULER',' ')
	call abend
999	call  xvmessage(' ERROR OPENNING INPUT FILE',' ')
	call prnt(4,1,istat,' stat =.')
	call abend
998	call  xvmessage(' ERROR ACCESSING SEDR',' ')
	call prnt(4,1,ind,' ind =.')
	call abend
C
	end

C *********************************************************************
C computes c matrix from om & me matrix
      subroutine omtoc(om,me,c)
      implicit none
      real*8 om(9),me(9),c(9),omt(9)
      integer i,j,k

      do i=0,2
         do j=0,2
            omt(j*3+i+1)=om(i*3+j+1)
         enddo
      enddo
      do k=0,8,3
         do j=0,2
            c(k+j+1)=omt(k+1)*me(j+1)+
     +               omt(k+2)*me(j+4)+
     +               omt(k+3)*me(j+7)
         enddo
      enddo
      return
      end

C ***********************************************************************
      subroutine buildcm(c,alpha,delta,kappa)
      implicit real*8 (a-h,o-z)
      real*8 c(9)	!output c matrix
      real*8 alpha      !input ra of instrument boresight (degrees)
      real*8 delta	!input declination of instrument boresight (degrees)
      real*8 kappa	!input image rotation angle (north angle) (degrees)

      pi = 3.141592653589793d0
      degrad = pi/180.d0

      sin_alpha = sin(alpha*degrad)
      cos_alpha = cos(alpha*degrad)

      sin_delta = sin(delta*degrad)
      cos_delta = cos(delta*degrad)

      sin_kappa = sin(kappa*degrad)
      cos_kappa = cos(kappa*degrad)

      c(1) =  -sin_alpha*cos_kappa - cos_alpha*sin_delta*sin_kappa
      c(2) =   cos_alpha*cos_kappa - sin_alpha*sin_delta*sin_kappa
      c(3) =   cos_delta*sin_kappa

      c(4) =   sin_alpha*sin_kappa - cos_alpha*sin_delta*cos_kappa
      c(5) =  -cos_alpha*sin_kappa - sin_alpha*sin_delta*cos_kappa
      c(6) =   cos_delta*cos_kappa

      c(7) =   cos_alpha*cos_delta
      c(8) =   sin_alpha*cos_delta
      c(9) =   sin_delta
      return
      end

C ***********************************************************
      subroutine msedr(ind,fds,isn,buf,buf8,data,
     +                 scet,project,proj)
      implicit none
      integer*4 fds,isn,scet(6),proj
      real*4 buf(200),data(40)
      real*8 buf8(100)
      character*5 project

      integer*4 system,mode,count,def,ind
      character*11 default
      character*5 camera
      character*12 target,user_target
      character*10 usr_info(10)
      real*8 pi,r2,dtor,rp2,re2
      real*4 r
      logical xvptst
C
      pi = 3.141592653589793d0
      dtor = pi/180.d0

C get camera constants
      call getcamcon(project,isn,data(27),data(28),data(29),
     +               data(30),ind)
      if(ind.ne.0)then
         call  xvmessage('GETCAMCON: bad indicator',' ')
         call abend
      endif

C      write(*,*)project,source,target,fds,isn
C      write(*,*)scet(1),scet(2),scet(3),scet(4),scet(5),scet(6)

C     ....Get target name
      call xvparm('target',user_target,count,def,0)
      if (count.eq.1) call ucase(user_target,target)

      usr_info(1) = 'NONE'		!institute
      usr_info(2) = 'NONE'		!purpose
      usr_info(3) = '*NONE*'		!program
      usr_info(4) = 'NONE'		!spkid
      usr_info(5) = 'NONE'		!reqnum
      usr_info(6) = '0000'		!year
      usr_info(7) = '0000'		!month,day
      usr_info(8) = '0000'		!hour,min
      usr_info(10) = '*NONE*'		!userid

      if (project.eq.'GLL  ') then
         IF (isn.EQ.2) THEN   ! Summation Mode
           camera = 'SSI2'
         ELSE                 ! Full Frame
           camera = 'SSI1'
         END IF
         system = 1
      endif

      if (project.eq.'CASSI') then
	! from getspice2:
	if (isn.eq.1) camera = 'ISSN'
	if (isn.eq.21) camera = 'ISS2'
	if (isn.eq.41) camera = 'ISS4'
	if (isn.eq.2) camera = 'ISSW'
	if (isn.eq.22) camera = 'ISW2'
	if (isn.eq.42) camera = 'ISW4'
	system = 1
      endif

      if (xvptst('remote')) then
         mode=1
      else if (xvptst('local')) then
         mode = 0
      else
         call xgetenv_vic('DEFAULTSPICE',default)
         mode = 0
         if (default.eq.'REMOTESPICE') mode=1 
      endif          

      call getspice95(mode,proj,camera,scet,target,
     &          system,'NAIF',usr_info,buf8,ind)
      if (ind.ne.1) then
          call prnt(4,1,ind,'GETSPICE95: bad ind=.')
          call abend
      endif

      call mve(4,1,buf(9),data(36),1,1) !Target ID

      data(25) = buf8(15)		!Polar radius
	rp2=data(25)*data(25)
      data(26) = (buf8(13)+buf8(14))/2.0
	re2=data(26)*data(26)		!Equatorial radius
	r2=1.0d0*rp2/re2

      data(38) = buf8(27)		!Spacecraft-to-target distance

      data(31) = buf8(30)		!Subspacecraft lat,lon
      r=buf8(31)
      data(32) = amod(r+360.,360.)

      r=buf8(68)			!North angle
      data(35) = amod(r+90.,360.)

      call mve(8,9,buf8(59),data,1,1)		!OM matrix
      call mve(8,3,buf8(22),data(19),1,1)	!RS vector
      return
      end


C ********************************************************************
	subroutine toeuler (c, alpha, delta, kappa, error)
	implicit none
	real*8	c(3,3)      ! output - derived rotation matrix 
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)

C  this routine performs the functional inverse of routine fromeuler.
C  this routine takes an input rotation matrix, and computes the three euler
C  angles representing the matrix.  (these 3 angles are called alpha, delta,
C  and kappa by mert davies etc.)  if the matrix is not a valid rotation
C  matrix (i.e. if the length of the row and column vectors is not within
C  0.0001 of unity) then error is returned true.
C
C  the 9 elements of the matrix are stored in order of increasing address as
C
C                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
C                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
C                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
C

        real*8 pi,dtr,rtd
	real*8	collength, rowlength
	integer i, j
	logical*4 error

        pi = 3.141592653589793d0
        dtr = pi/180.d0
        rtd = 180.d0/pi

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
	    delta = asin(c(3,3))*rtd
	    if (c(3,1) .ne. 0.0) then
		alpha = atan2(c(3,2),c(3,1))*rtd
	    else if (c(3,2) .eq. 0.0) then
		alpha = 0.0
	    else
		alpha = sign(dble(90.0),c(3,2))
	    endif
	    if (alpha .lt. 0.0) alpha = alpha + 360.0
	    if (c(2,3) .ne. 0.0) then
		kappa = atan2(c(1,3), c(2,3))*rtd
	    else if (c(1,3) .eq. 0.0) then
		kappa = atan2( -c(1,1), -c(2,1))*rtd
	    else
		kappa = sign(dble(90.0),c(1,3))
	    endif
	    if (kappa .lt. 0.0) kappa = kappa + 360.0
	    if (abs(dcos(delta*dtr))*dcos(kappa*dtr) - c(2,3)
     +						 .gt. 0.0001) 
     +		kappa = 180.0 - kappa
	endif

	return
	end

C ****************************************************************
         subroutine mattran(min)
         implicit none
         real*8 min(3,3),mou(3,3)
         integer i,j
C
C do a simple matrix transpose
C
         do i=1,3
           do j=1,3
            mou(i,j) = min(j,i)
           enddo
         enddo
C
	call mve(8,9,mou,min,1,1)
         return
         end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ibisupdate.pdf
process help=*
PARM INP 	 STRING
PARM PROJECT     TYPE=(STRING,5)  COUNT=1 +
    VALID=("VGR-1","VGR-2","MAR10","MAR-9","VIKOR","GLL  ","CASSI")
PARM TARGET      TYPE=(STRING,12) COUNT=1
PARM SCET        TYPE=INTEGER     COUNT=0:300                      DEFAULT=--
PARM MODE 	 TYPE=KEYWORD     COUNT=0:1    VALID=(UPDATE,NOUPDATE) + 
                                                               DEFAULT=NOUPDATE
PARM CAMERA	 TYPE=INTEGER 	  COUNT=0:50                       DEFAULT=--
PARM OBJECT      TYPE=KEYWORD                  VALID=(OBJECT,IMAGE) +    
                                                                  DEFAULT=IMAGE              
PARM SPKID      TYPE=(STRING,4)   COUNT=0:50                       DEFAULT=NONE
PARM SPICEMODE  TYPE=KEYWORD      COUNT=0:1 VALID=(LOCAL,REMOTE)   DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)   COUNT=0:1			   DEFAULT=--
PARM PURPOSE    TYPE=(STRING,4)   COUNT=1			   DEFAULT=NONE
PARM REQNUM     TYPE=(STRING,4)   COUNT=1			   DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12)  COUNT=1		   DEFAULT=000000000000
PARM GROUPID    TYPE=(STRING,3)   COUNT=0:1			   DEFAULT=--
END-PROC

.TITLE
IBISUPDATE - Copy corrected camera pointing from IBIS SEDR file to
      SPICE C-kernel

.HELP
IBISUPDATE is a VICAR program which copies corrected camera pointing data
from an IBIS SEDR file to a SPICE C-kernel.  IBISUPDATE is one of a suite
of programs used primarily for creating digital mosaics.  The program
currently works for Galileo SSI only.

EXECUTION:

  IBISUPDATE INP=ibisfile  TARGET=targetname  user-parameters...
where
  INP is the input IBIS SEDR file, and
  TARGET is the name of the planet, satellite, or asteroid in the image.

The IBIS SEDR file is originally created by IBISNAV, and later updated with
corrected pointing via OMCOR2.

.page
OPERATION:

The data for each image is read from the IBIS SEDR file.  If the SCET parameter
is specified, the records matching the input SCETs are processed.  Otherwise,
all records in the IBIS SEDR file are processed.  Processing consists extracting
the camera pointing and storing it in the MIPS C-kernel MIPS_NEAR.CK.

To later access the updated navigation information with programs like
MAP2, the user should use the CKNAME parameter with a value of NEAR.

.page
LOCAL AND REMOTE SPICE ACCESS:

Corrected camera pointing may be stored in local or remote (e.g. at JPL) SPICE
C-kernels.  Remote access is via a SPICE server.

SPICEMODE specifies whether local or remote SPICE access is to be used.
If defaulted, SPICEMODE is set to the value of the logical name (or
environmental variable) DEFAULTSPICE.

PARAMETERS FOR STORING THE IMPROVED CAMERA POINTING:

The following optional parameters are used to store provenance information along
with the improved (C-Smithed) camera pointing computed by the program.  This
provenance information is stored in the (C kernel) segment identifier for the
camera pointing data.  In cases where there are several versions of camera
pointing for an image in a given C kernel, this provenance information can
later be used to retrieve a specific instance of camera pointing from the
kernel.

PURPOSE identifies the purpose for creating the camera pointing.
REQNUM identifies the request number associated with the camera pointing.
CDATE specifies the date and time the camera pointing was created.
GROUPID identifies the group which created the camera pointing.
INSTITUTE identifies the facility which created the camera pointing.

See the level 2 help (via the TAE tutor mode) for further details.

Example:  'REMOTE INSTITUTE=DLR USERID=TYR stores the improved camera pointing
          via the SPICE server.  Provenance data regarding the facility (DLR)
          and user (Thomas Roatsch) who created the data is included.

.page
GALILEO'S FULL FRAME MODE AND SUMMATION MODE

In conformance to new convention, the CAMERA s/n for GLL is now used to specify
Full Frame Mode (if s/n = 1) or Summation Mode (if s/n = 2).  If the CAMERA
is not specified the program defaults CAMERA to Full Frame Mode (s/n = 1).

.page
EXAMPLES:

             IBISNAV out=IB.SDR  user-parameters...

                ( run OMCOR )

             IBISUPDATE IB.SDR  TARGET=JUPITER CAM=5 'UPDATE +
                     PROJECT=VGR-1
     -or-    IBISUPDATE IB.SDR  TARGET=JUPITER 'UPDATE +
                     PROJECT=GLL

             MAP2 image  SEDRSRC=NEAR parameters

PROGRAM HISTORY:

Original programmer: Jean Lorre
Cognizant programmer: Jean Lorre
Revisions:
  When         Who  What
  -----------  ---  -----------------------------------------------------------
  10 Jan 1997  SMC  Corrected .PDF documentation                      (FR89979)
  14 Nov 1996  SMC  Fixed so that SPKID from the new IBISNAV file format is 
                    recognized and passed to PUTSPICE95 to write to CK (FR89225)
  28 Aug 1996  SMC  Implemented support for GLL Summation Mode processing
                    Obtain Camera S/N from IBIS file, use CAMERA as override 
                    (DFR)
   6 Jun 1996  GMY  Implemented GETSPICE95/PUTSPICE95 interface
  20 Oct 1995  FFM  Ported to Unix
  24 Aug 1998  RRP  Made it Y2K compliant. Put check for scet (Y2K FRD<901>).
  04 Jan 1999  TXH  Corrected call to 'mvcl' from 'mvlc' in moving SPKID.
                    (AR-101223).
  14 Aug 2002  GMY  Linux compile changes:  cosd(angle) to cos(angle*dtr)
  29 Jun 2006  LWK  enabled GETSPICE95 call to work for Cassini

.level1
.vari INP
String
input IBIS SEDR file
.VARI PROJECT
The project ID.
.VARI OBJECT
Specifies whether the 
images are geometrically 
corrected.
IMAGE valid only 
for GLL project.
.VARI SCET
The spacescraft event 
time for each image.
Required only for GLL project.
.vari TARGET
String 12 characters
The name of the target body
.vari CAMERA
Optional Array of Integers
serial number of camera
.VARI SPICEMODE
Optional keyword
Location of SPICE kernels
(LOCAL or REMOTE)
.VARI INSTITUTE
Optional 4-char string
Facility which created camera pointing
.VARI PURPOSE
Optional 4-char string
Purpose for camera pointing
.VARI REQNUM
Optional 4-char string
IPL request number for created camera pointing
.VARI CDATE
Optional 12-char string
Date and time camera pointing was created
.VARI GROUPID
Optional 3-char string
Group which created camera pointing

.level2
.vari INP
String
Name of the input IBIS SEDR file.

.VARI PROJECT
The project ID.

.VARI OBJECT
Specifies whether the data is in OBJECT space (geometricaly corrected) or
IMAGE space (uncorrected).

IMAGE (uncorrected) is only valid for GLL project.
.VARI SCET
The spacescraft event time for each image.  Required only for GLL project.
If omitted the scet times will be extracted from columns 32-37 in the input
ibis file. Groups of 6 integers for each image in the order:
Year, day, hour, minute, second, milisecond.
.vari MODE
Keyword
Indicator of whether to update or not update the Voyager SEDR file.  
        Valid: UPDATE and NOUPDATE.  Default: NOUPDATE.
.vari CAMERA
Array of Integers
The serial number of the camera which took all images.
By default the program will obtain the cameras from the input IBIS file.
This parameter is used to override the camera s/n specified in the IBIS table.

If the override is desired on the n-th record, all cameras from the first
record up to the n-th are to be specified.

For GLL's case, which actually has only one camera, this parameter specifies if
an image was taken in Full Frame Mode (CAMERA = 1) or Summation Mode (CAMERA = 
2).

.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  A complete list of valid
target names is located in the ASCII file assigned the logical name (or
environmental variable) BODY_IDS.
If defaulted, the target is retrieved from the VICAR label or other TBD means.

.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be accessed from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be accessed
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.

Note that if SPICE data is not found in LOCAL or REMOTE mode, the other mode
is attempted in order to retrieve SPICE data.  However, when improved camera
pointing data is stored, only the specified or default mode is used.

.VARI INSTITUTE
INSTITUTE is a four character string identifying the facility which creates
the improved (C-Smithed) camera pointing.  If defaulted, the value of the
logical name (or environmental variable) VICAR_SITE is used.

Ex:  INSTITUTE=ASU identifies ASU as the creator of the improved camera
     pointing.

.VARI PURPOSE
PURPOSE is a four character string identifying the purpose of the observation
or the purpose of processing.  For example,
  PURPOSE=MOSA identifies the image as part of a mosaic sequence
  PURPOSE=COLO identifies the image as part of a color sequence

.VARI REQNUM
REQUNUM is a four character string identifying the IPL request number for
which the camera pointing was created.  REQNUM must contain exactly 4 digits.

Ex:  REQNUM=0123 identifies (somewhat) request number R000123

.VARI CDATE
Date and time the camera pointing was created in the form 'YEARMMDDHHMM'.
The date string must contain exactly 12 digits.

Ex:  CDATE=199602291200 specifies that the pointing was created at noon
     on February 29, 1996.

If defaulted, the current date and time is used.

.VARI GROUPID
GROUPID is a three character string which identifies the group of the user
running this program to store improved camera pointing.  (The user ID is
automatically determined by the program).

Ex:  GROUPID=040 identifies group 040 as the creator of the camera pointing.

On VMS, this parameter is ignored since the program automatically determines
the group ID of the current user.

If GROUPID is defaulted on Unix, the program uses the value of the
environmental variable GROUPID.  Note that GROUPID is not a system-defined
variable, and should be defined in your .cshrc as in the following example:

Ex:  setenv GROUPID 040
.end
$ Return
$!#############################################################################
$Imake_File:
$ create ibisupdate.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ibisupdate

   To Create the build file give the command:

		$ vimake ibisupdate			(VMS)
   or
		% vimake ibisupdate			(Unix)


************************************************************************/
#define PROGRAM	ibisupdate
#define R2LIB

#define MODULE_LIST ibisupdate.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_MATH77
#define LIB_NETWORK

/* #define DEBUG         /* Comment out upon delivery */
/* #define LIB_LOCAL     /* Comment out upon delivery */ 
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstibisupdate.pdf
procedure
refgbl $echo
refgbl $syschar
body
let $echo="yes"
let _onfail="continue"
local path string
!
!FIND A SET OF FRAMES WITH ORIGINAL SEDR 
!IBISNAV out=ibis1.fil FDS=(1636832,1636834,1636836,1636838) +
!        SEDR=(SEDR,SEDR,SEDR,SEDR) +
!	PROJECT=VGR-1 PLANET=JUPITER CAMERA=7
!ibisupdate inp=ibis1.fil planet=jupiter +
!        camera=7 project=VGR-1 'object 'update
!dcl del ibis1.fil;*
!
! Test of Galileo
ibisnav out=gasedr.int target=venus camera=(1,1)+
   project=GLL scet=(1990,44,5,58,16,962,1990,44,7,58,36,296)
ibisupdate inp=gasedr.int target=venus project=GLL 'update +
      scet=(1990,44,5,58,16,962,1990,44,7,58,36,296)
ibisupdate inp=gasedr.int target=venus project=GLL 'update

ibisnav out=gasedr2.int target=venus camera=(1,1)+
   project=GLL scet=(1990,44,5,58,16,962,1990,44,7,58,36,296)

let $echo="no"
write "*******************************************"
write "Test invalid scet format."
write "*******************************************"
let $echo="yes"

ibisupdate inp=gasedr2.int target=venus project=GLL 'update +
      scet=(1990,44,5,58,16,962,90,44,7,58,36,296)

let $echo="no"
write "*******************************************"
write "Test for Cassini using pointing files "
write "generated by C.Avis. (See TSTOMCOR2.PDF.)"
write "*******************************************"
let $echo="yes"

# copy the file to local directory so as to avoid changing the SIT version:
ush cp /project/test_work/testdata/cassini/iss/dione.nav .
ibisupdate dione.nav 'update proj=CASSI 'object target=DIONE

if ( $syschar(1) = "UNIX" )
    ush rm gasedr.int
    ush rm dione.nav
else
    dcl delete gasedr.*;*
end-if
end-proc
$!-----------------------------------------------------------------------------
$ create tstibisupdate_ops.log
tstibisupdate
let _onfail="continue"
local path string
ibisnav out=gasedr.int target=venus camera=(1,1) +
   project=GLL scet=(1990,44,5,58,16,962,1990,44,7,58,36,296)
Beginning VICAR task ibisnav
IBISNAV version Oct 31, 2002
 Using best available C-kernel source
************************
sclk=          1
SCET:y,d,h,m,s,msec
                 1990         44          5         58         16        962
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
 RS vector=
           -1.624E+06 -1.045E+05 -8.401E+04
 OM angles=
            3.633E+00  3.103E+00  1.454E+00
************************
sclk=          2
SCET:y,d,h,m,s,msec
                 1990         44          7         58         36        296
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
 RS vector=
           -1.669E+06 -1.105E+05 -8.576E+04
 OM angles=
            3.747E+00  3.076E+00  1.475E+00
ibisupdate inp=gasedr.int target=venus project=GLL 'update  +
      scet=(1990,44,5,58,16,962,1990,44,7,58,36,296)
Beginning VICAR task ibisupdate
IBISUPDATE Version 06 July 2006
 number of frames          2

SCET:y d h m s ms
                 1990         44          5         58         16        962
 sedr angles
            3.633E+00  3.103E+00  1.454E+00
 orig angles
            3.633E+00  3.103E+00  1.454E+00
 updt angles
            3.633E+00  3.103E+00  1.454E+00
 sedr rs
           -1.624E+06 -1.045E+05 -8.401E+04
 updt rs
           -1.624E+06 -1.045E+05 -8.401E+04
 sedr planet center l,s=
            1.431E+02  4.736E+02
 orig ibis center l,s=
            1.431E+02  4.736E+02
 updt ibis center l,s=
            1.431E+02  4.736E+02
 source of orig pc is NAV
 sedr om
           -6.472E-02 -5.239E-02  9.965E-01  9.976E-01 -2.875E-02  6.328E-02  2.533E-02  9.982E-01  5.412E-02
 orig om
           -6.472E-02 -5.239E-02  9.965E-01  9.976E-01 -2.875E-02  6.328E-02  2.533E-02  9.982E-01  5.412E-02
 updt om
           -6.472E-02 -5.239E-02  9.965E-01  9.976E-01 -2.875E-02  6.328E-02  2.533E-02  9.982E-01  5.412E-02
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
SEDR UPDATED WITH SOURCE NEAR

SCET:y d h m s ms
                 1990         44          7         58         36        296
 sedr angles
            3.747E+00  3.076E+00  1.475E+00
 orig angles
            3.747E+00  3.076E+00  1.475E+00
 updt angles
            3.747E+00  3.076E+00  1.475E+00
 sedr rs
           -1.669E+06 -1.105E+05 -8.576E+04
 updt rs
           -1.669E+06 -1.105E+05 -8.576E+04
 sedr planet center l,s=
            1.557E+02  4.670E+02
 orig ibis center l,s=
            1.557E+02  4.670E+02
 updt ibis center l,s=
            1.557E+02  4.670E+02
 source of orig pc is NAV
 sedr om
           -6.671E-02 -5.185E-02  9.964E-01  9.974E-01 -2.919E-02  6.526E-02  2.570E-02  9.982E-01  5.367E-02
 orig om
           -6.671E-02 -5.185E-02  9.964E-01  9.974E-01 -2.919E-02  6.526E-02  2.570E-02  9.982E-01  5.367E-02
 updt om
           -6.671E-02 -5.185E-02  9.964E-01  9.974E-01 -2.919E-02  6.526E-02  2.570E-02  9.982E-01  5.367E-02
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
SEDR UPDATED WITH SOURCE NEAR
ibisupdate inp=gasedr.int target=venus project=GLL 'update
Beginning VICAR task ibisupdate
IBISUPDATE Version 06 July 2006
 number of frames          2
Obtaining SCET from ibis file cols:32-37

SCET:y d h m s ms
                 1990         44          5         58         16        962
 sedr angles
            3.633E+00  3.103E+00  1.454E+00
 orig angles
            3.633E+00  3.103E+00  1.454E+00
 updt angles
            3.633E+00  3.103E+00  1.454E+00
 sedr rs
           -1.624E+06 -1.045E+05 -8.401E+04
 updt rs
           -1.624E+06 -1.045E+05 -8.401E+04
 sedr planet center l,s=
            1.431E+02  4.736E+02
 orig ibis center l,s=
            1.431E+02  4.736E+02
 updt ibis center l,s=
            1.431E+02  4.736E+02
 source of orig pc is NAV
 sedr om
           -6.472E-02 -5.239E-02  9.965E-01  9.976E-01 -2.875E-02  6.328E-02  2.533E-02  9.982E-01  5.412E-02
 orig om
           -6.472E-02 -5.239E-02  9.965E-01  9.976E-01 -2.875E-02  6.328E-02  2.533E-02  9.982E-01  5.412E-02
 updt om
           -6.472E-02 -5.239E-02  9.965E-01  9.976E-01 -2.875E-02  6.328E-02  2.533E-02  9.982E-01  5.412E-02
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
SEDR UPDATED WITH SOURCE NEAR

SCET:y d h m s ms
                 1990         44          7         58         36        296
 sedr angles
            3.747E+00  3.076E+00  1.475E+00
 orig angles
            3.747E+00  3.076E+00  1.475E+00
 updt angles
            3.747E+00  3.076E+00  1.475E+00
 sedr rs
           -1.669E+06 -1.105E+05 -8.576E+04
 updt rs
           -1.669E+06 -1.105E+05 -8.576E+04
 sedr planet center l,s=
            1.557E+02  4.670E+02
 orig ibis center l,s=
            1.557E+02  4.670E+02
 updt ibis center l,s=
            1.557E+02  4.670E+02
 source of orig pc is NAV
 sedr om
           -6.671E-02 -5.185E-02  9.964E-01  9.974E-01 -2.919E-02  6.526E-02  2.570E-02  9.982E-01  5.367E-02
 orig om
           -6.671E-02 -5.185E-02  9.964E-01  9.974E-01 -2.919E-02  6.526E-02  2.570E-02  9.982E-01  5.367E-02
 updt om
           -6.671E-02 -5.185E-02  9.964E-01  9.974E-01 -2.919E-02  6.526E-02  2.570E-02  9.982E-01  5.367E-02
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
SEDR UPDATED WITH SOURCE NEAR
ibisnav out=gasedr2.int target=venus camera=(1,1) +
   project=GLL scet=(1990,44,5,58,16,962,1990,44,7,58,36,296)
Beginning VICAR task ibisnav
IBISNAV version Oct 31, 2002
 Using best available C-kernel source
************************
sclk=          1
SCET:y,d,h,m,s,msec
                 1990         44          5         58         16        962
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
 RS vector=
           -1.624E+06 -1.045E+05 -8.401E+04
 OM angles=
            3.633E+00  3.103E+00  1.454E+00
************************
sclk=          2
SCET:y,d,h,m,s,msec
                 1990         44          7         58         36        296
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
 RS vector=
           -1.669E+06 -1.105E+05 -8.576E+04
 OM angles=
            3.747E+00  3.076E+00  1.475E+00
let $echo="no"
*******************************************
Test invalid scet format.
*******************************************
ibisupdate inp=gasedr2.int target=venus project=GLL 'update  +
      scet=(1990,44,5,58,16,962,90,44,7,58,36,296)
Beginning VICAR task ibisupdate
IBISUPDATE Version 06 July 2006
 number of frames          2
Warning::year number is not 4-digit.
SCET HAS INCORRECT FORMAT.

YEAR MUST BE 4-DIGITS.
DAY  MUST BE BETWEEN 1 AND 366
HOUR MUST BE BETWEEN 0 AND 23
MIN  MUST BE BETWEEN 0 AND 59
SEC  MUST BE BETWEEN 0 AND 59
MSEC MUST BE BETWEEN 0 AND 999
 ** ABEND called **
continue
let $echo="no"
*******************************************
Test for Cassini using pointing files 
generated by C.Avis. (See TSTOMCOR2.PDF.)
*******************************************
ush cp /project/test_work/testdata/cassini/iss/dione.nav .
ibisupdate dione.nav 'update proj=CASSI 'object target=DIONE
Beginning VICAR task ibisupdate
IBISUPDATE Version 06 July 2006
 number of frames         12
Obtaining SCET from ibis file cols:32-37

 fds  1507741300
 sedr angles
            9.477E+00  1.816E+00  2.701E+02
 orig angles
            9.477E+00  1.816E+00  2.701E+02
 updt angles
            9.475E+00  1.820E+00  2.700E+02
 sedr rs
           -4.269E+04 -7.368E+03 -1.220E+03
 updt rs
           -4.269E+04 -7.368E+03 -1.220E+03
 sedr planet center l,s=
            1.432E+03  1.101E+03
 orig ibis center l,s=
            1.432E+03  1.101E+03
 updt ibis center l,s=
            1.439E+03  1.113E+03
 source of orig pc is NAIF
 sedr om
            3.101E-02 -1.647E-01  9.859E-01  6.675E-03  9.863E-01  1.646E-01 -9.995E-01  1.477E-03  3.169E-02
 orig om
            3.101E-02 -1.647E-01  9.859E-01  6.675E-03  9.863E-01  1.646E-01 -9.995E-01  1.477E-03  3.169E-02
 updt om
            3.128E-02 -1.646E-01  9.859E-01  5.518E-03  9.864E-01  1.645E-01 -9.995E-01  2.928E-04  3.176E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507741460
 sedr angles
            9.212E+00  1.575E+00  2.701E+02
 orig angles
            9.208E+00  1.576E+00  2.699E+02
 updt angles
            9.209E+00  1.576E+00  2.699E+02
 sedr rs
           -4.126E+04 -6.927E+03 -1.210E+03
 updt rs
           -4.126E+04 -6.927E+03 -1.210E+03
 sedr planet center l,s=
            1.436E+03  2.738E+02
 orig ibis center l,s=
            1.447E+03  2.756E+02
 updt ibis center l,s=
            1.443E+03  2.748E+02
 source of orig pc is NEAR
 sedr om
            2.690E-02 -1.601E-01  9.867E-01  5.820E-03  9.871E-01  1.600E-01 -9.996E-01  1.439E-03  2.748E-02
 orig om
            2.742E-02 -1.600E-01  9.867E-01  2.771E-03  9.871E-01  1.599E-01 -9.996E-01 -1.652E-03  2.751E-02
 updt om
            2.734E-02 -1.600E-01  9.867E-01  3.210E-03  9.871E-01  1.600E-01 -9.996E-01 -1.206E-03  2.750E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507741620
 sedr angles
            8.946E+00  1.335E+00  2.701E+02
 orig angles
            8.940E+00  1.338E+00  2.699E+02
 updt angles
            8.941E+00  1.338E+00  2.699E+02
 sedr rs
           -3.984E+04 -6.497E+03 -1.201E+03
 updt rs
           -3.984E+04 -6.497E+03 -1.201E+03
 sedr planet center l,s=
            1.436E+03 -5.589E+02
 orig ibis center l,s=
            1.450E+03 -5.529E+02
 updt ibis center l,s=
            1.447E+03 -5.534E+02
 source of orig pc is NEAR
 sedr om
            2.280E-02 -1.555E-01  9.876E-01  5.025E-03  9.878E-01  1.555E-01 -9.997E-01  1.419E-03  2.330E-02
 orig om
            2.337E-02 -1.553E-01  9.876E-01  1.701E-03  9.879E-01  1.553E-01 -9.997E-01 -1.951E-03  2.335E-02
 updt om
            2.329E-02 -1.554E-01  9.876E-01  2.207E-03  9.879E-01  1.554E-01 -9.997E-01 -1.439E-03  2.335E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507741809
 sedr angles
            8.913E+00  1.396E+00  2.701E+02
 orig angles
            8.908E+00  1.396E+00  2.699E+02
 updt angles
            8.909E+00  1.396E+00  2.699E+02
 sedr rs
           -3.816E+04 -6.006E+03 -1.189E+03
 updt rs
           -3.816E+04 -6.006E+03 -1.189E+03
 sedr planet center l,s=
            6.090E+02 -5.559E+02
 orig ibis center l,s=
            6.196E+02 -5.558E+02
 updt ibis center l,s=
            6.163E+02 -5.566E+02
 source of orig pc is NEAR
 sedr om
            2.386E-02 -1.550E-01  9.876E-01  5.076E-03  9.879E-01  1.549E-01 -9.997E-01  1.317E-03  2.436E-02
 orig om
            2.439E-02 -1.548E-01  9.876E-01  1.716E-03  9.879E-01  1.548E-01 -9.997E-01 -2.081E-03  2.436E-02
 updt om
            2.429E-02 -1.548E-01  9.876E-01  2.349E-03  9.879E-01  1.548E-01 -9.997E-01 -1.440E-03  2.436E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507741973
 sedr angles
            8.633E+00  1.736E+00  2.701E+02
 orig angles
            8.630E+00  1.738E+00  2.699E+02
 updt angles
            8.631E+00  1.737E+00  2.700E+02
 sedr rs
           -3.669E+04 -5.593E+03 -1.179E+03
 updt rs
           -3.669E+04 -5.593E+03 -1.179E+03
 sedr planet center l,s=
            6.122E+02  2.707E+02
 orig ibis center l,s=
            6.189E+02  2.748E+02
 updt ibis center l,s=
            6.166E+02  2.742E+02
 source of orig pc is NEAR
 sedr om
            2.975E-02 -1.501E-01  9.882E-01  5.881E-03  9.887E-01  1.500E-01 -9.995E-01  1.348E-03  3.030E-02
 orig om
            3.032E-02 -1.500E-01  9.882E-01  2.320E-03  9.887E-01  1.500E-01 -9.995E-01 -2.255E-03  3.032E-02
 updt om
            3.009E-02 -1.501E-01  9.882E-01  3.771E-03  9.887E-01  1.500E-01 -9.995E-01 -7.874E-04  3.032E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507742134
 sedr angles
            8.357E+00  2.082E+00  2.701E+02
 orig angles
            8.355E+00  2.083E+00  2.699E+02
 updt angles
            8.355E+00  2.082E+00  2.700E+02
 sedr rs
           -3.525E+04 -5.201E+03 -1.169E+03
 updt rs
           -3.525E+04 -5.201E+03 -1.169E+03
 sedr planet center l,s=
            6.124E+02  1.103E+03
 orig ibis center l,s=
            6.197E+02  1.105E+03
 updt ibis center l,s=
            6.176E+02  1.104E+03
 source of orig pc is NEAR
 sedr om
            3.574E-02 -1.454E-01  9.887E-01  6.604E-03  9.894E-01  1.452E-01 -9.993E-01  1.339E-03  3.632E-02
 orig om
            3.622E-02 -1.452E-01  9.887E-01  3.465E-03  9.894E-01  1.452E-01 -9.993E-01 -1.834E-03  3.634E-02
 updt om
            3.594E-02 -1.453E-01  9.887E-01  5.349E-03  9.894E-01  1.452E-01 -9.993E-01  6.973E-05  3.633E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507742295
 sedr angles
            8.078E+00  2.429E+00  2.701E+02
 orig angles
            8.076E+00  2.433E+00  2.699E+02
 updt angles
            8.076E+00  2.433E+00  2.700E+02
 sedr rs
           -3.382E+04 -4.820E+03 -1.159E+03
 updt rs
           -3.382E+04 -4.820E+03 -1.159E+03
 sedr planet center l,s=
            6.123E+02  1.927E+03
 orig ibis center l,s=
            6.208E+02  1.937E+03
 updt ibis center l,s=
            6.182E+02  1.936E+03
 source of orig pc is NEAR
 sedr om
            4.178E-02 -1.406E-01  9.892E-01  7.262E-03  9.901E-01  1.404E-01 -9.991E-01  1.318E-03  4.239E-02
 orig om
            4.225E-02 -1.404E-01  9.892E-01  4.382E-03  9.901E-01  1.404E-01 -9.991E-01 -1.596E-03  4.245E-02
 updt om
            4.201E-02 -1.405E-01  9.892E-01  6.050E-03  9.901E-01  1.404E-01 -9.991E-01  8.727E-05  4.245E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507742601
 sedr angles
            7.823E+00  2.283E+00  2.701E+02
 orig angles
            7.821E+00  2.286E+00  2.700E+02
 updt angles
            7.821E+00  2.286E+00  2.700E+02
 sedr rs
           -3.108E+04 -4.132E+03 -1.140E+03
 updt rs
           -3.108E+04 -4.132E+03 -1.140E+03
 sedr planet center l,s=
           -2.163E+02  1.097E+03
 orig ibis center l,s=
           -2.091E+02  1.106E+03
 updt ibis center l,s=
           -2.089E+02  1.106E+03
 source of orig pc is NEAR
 sedr om
            3.932E-02 -1.362E-01  9.899E-01  6.553E-03  9.907E-01  1.360E-01 -9.992E-01  1.140E-03  3.984E-02
 orig om
            3.948E-02 -1.361E-01  9.899E-01  5.749E-03  9.907E-01  1.360E-01 -9.992E-01  3.231E-04  3.989E-02
 updt om
            3.958E-02 -1.361E-01  9.899E-01  4.990E-03  9.907E-01  1.360E-01 -9.992E-01 -4.423E-04  3.989E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507742761
 sedr angles
            7.534E+00  2.080E+00  2.701E+02
 orig angles
            7.532E+00  2.084E+00  2.700E+02
 updt angles
            7.532E+00  2.084E+00  2.700E+02
 sedr rs
           -2.964E+04 -3.789E+03 -1.130E+03
 updt rs
           -2.964E+04 -3.789E+03 -1.130E+03
 sedr planet center l,s=
           -2.124E+02  2.631E+02
 orig ibis center l,s=
           -2.058E+02  2.752E+02
 updt ibis center l,s=
           -2.055E+02  2.750E+02
 source of orig pc is NEAR
 sedr om
            3.583E-02 -1.312E-01  9.907E-01  5.887E-03  9.914E-01  1.310E-01 -9.993E-01  1.137E-03  3.629E-02
 orig om
            3.603E-02 -1.311E-01  9.907E-01  4.920E-03  9.914E-01  1.310E-01 -9.993E-01  1.554E-04  3.636E-02
 updt om
            3.613E-02 -1.311E-01  9.907E-01  4.147E-03  9.914E-01  1.310E-01 -9.993E-01 -6.233E-04  3.636E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507742919
 sedr angles
            7.245E+00  1.887E+00  2.701E+02
 orig angles
            7.236E+00  1.891E+00  2.701E+02
 updt angles
            7.240E+00  1.887E+00  2.700E+02
 sedr rs
           -2.822E+04 -3.463E+03 -1.120E+03
 updt rs
           -2.822E+04 -3.463E+03 -1.120E+03
 sedr planet center l,s=
           -2.133E+02 -5.615E+02
 orig ibis center l,s=
           -1.874E+02 -5.486E+02
 updt ibis center l,s=
           -1.998E+02 -5.606E+02
 source of orig pc is NAV
 sedr om
            3.252E-02 -1.262E-01  9.915E-01  5.292E-03  9.920E-01  1.260E-01 -9.995E-01  1.148E-03  3.292E-02
 orig om
            3.259E-02 -1.260E-01  9.915E-01  5.297E-03  9.920E-01  1.259E-01 -9.995E-01  1.148E-03  3.300E-02
 updt om
            3.270E-02 -1.260E-01  9.915E-01  3.838E-03  9.920E-01  1.260E-01 -9.995E-01 -3.130E-04  3.292E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507743058
 sedr angles
            7.040E+00  1.687E+00  2.701E+02
 orig angles
            7.040E+00  1.687E+00  2.701E+02
 updt angles
            7.035E+00  1.687E+00  2.700E+02
 sedr rs
           -2.697E+04 -3.186E+03 -1.111E+03
 updt rs
           -2.697E+04 -3.186E+03 -1.111E+03
 sedr planet center l,s=
           -3.690E+02 -1.398E+03
 orig ibis center l,s=
           -3.690E+02 -1.398E+03
 updt ibis center l,s=
           -3.565E+02 -1.396E+03
 source of orig pc is NAIF
 sedr om
            2.907E-02 -1.226E-01  9.920E-01  4.717E-03  9.925E-01  1.225E-01 -9.996E-01  1.117E-03  2.943E-02
 orig om
            2.907E-02 -1.226E-01  9.920E-01  4.717E-03  9.925E-01  1.225E-01 -9.996E-01  1.118E-03  2.943E-02
 updt om
            2.926E-02 -1.225E-01  9.920E-01  3.241E-03  9.925E-01  1.224E-01 -9.996E-01 -3.663E-04  2.943E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR

 fds  1507742440
 sedr angles
            8.110E+00  2.493E+00  2.701E+02
 orig angles
            8.108E+00  2.495E+00  2.700E+02
 updt angles
            8.108E+00  2.495E+00  2.700E+02
 sedr rs
           -3.252E+04 -4.488E+03 -1.150E+03
 updt rs
           -3.252E+04 -4.488E+03 -1.150E+03
 sedr planet center l,s=
           -2.198E+02  1.928E+03
 orig ibis center l,s=
           -2.134E+02  1.934E+03
 updt ibis center l,s=
           -2.134E+02  1.934E+03
 source of orig pc is NEAR
 sedr om
            4.291E-02 -1.411E-01  9.891E-01  7.235E-03  9.900E-01  1.409E-01 -9.991E-01  1.109E-03  4.350E-02
 orig om
            4.306E-02 -1.411E-01  9.891E-01  6.391E-03  9.900E-01  1.409E-01 -9.991E-01  2.533E-04  4.353E-02
 updt om
            4.306E-02 -1.411E-01  9.891E-01  6.391E-03  9.900E-01  1.409E-01 -9.991E-01  2.533E-04  4.353E-02
CKNAME=NAIF  SPKID=N468  PROGRAM=Y CASS  
SEDR UPDATED WITH SOURCE NEAR
if ( $syschar(1) = "UNIX" )
    ush rm gasedr.int
    ush rm dione.nav
else
end-if
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC

NOTE:  the log generated for tstibisupdate under Select O on Linux
generated identical output except for one line:

242c243
<             1.436E+03  2.738E+02
---
>             1.435E+03  2.738E+02

11 July 2006,  - LWK
$ Return
$!#############################################################################
