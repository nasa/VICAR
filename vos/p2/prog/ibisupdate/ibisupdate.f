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
