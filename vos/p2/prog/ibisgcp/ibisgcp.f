C VICAR program IBISGCP: Output Ground Control Points to an IBIS file.

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT INTEGER (A-Z)

        parameter (maxpix=100)
	integer*4 sn(maxpix),scet(6,maxpix)
        real*4 rsvec(maxpix,3),omang(maxpix,3),radpol(maxpix)
        real*4 radeq(maxpix),focal(maxpix),optaxl(maxpix)
        real*4 optaxs(maxpix),scale(maxpix),r_cam(maxpix)

        real*4 buf(200),conv(3600)	!spice and is-to-os buffers
        integer*4 idata(40)		!convev buffer
        real*4 data(40)
        real*8 data8(20)        
        equivalence (data,idata,data8)

c       ...Output ground-control points: frame index, line, samp, lat, lon
	real*4 f(1000),l(1000),s(1000),lt(1000),ln(1000)

	REAL*4 RL,RS,RLN,RLT,RLNX,RLTX,FID(100)
        real*8 alpha,delta,kappa

        character*12 planet
        character*5 project
        character*80 infile

        logical input_file/.false./
        integer iu
	integer ibis_in, ibis_out
        character*6 format(5)/5*'REAL'/

	logical xvptst
c------------------------------------------------------------------

        CALL XVPARM('PROJECT',project,ICNT,IDEF,' ')
        CALL ucase (project, project)
c
c       If present, read IBIS input
c
        CALL XVPARM('INP',infile,icnt,idef,' ')
        if (idef.eq.0) then
          input_file=.true.
          call xvunit(iu,'INP',1,istat,' ')
          if ( istat.ne.1) call mabend('Err on IBIS unit.',' ')
          call ibis_file_open(iu,ibis_in,'read',0,0,' ',' ',istat)
          if ( istat .ne. 1 ) call ibis_signal_u(iu,istat)

          call ibis_file_get (ibis_in,'NR', npict,1,1)
          ifds=npict

          call xvparm('id',fid,ifid,idef,' ')
          do i=1,ifid
            if (nint(fid(i)).gt.ifds)
     +         call mabend('ID # larger than # IBIS records',' ')
          enddo
          ifds=ifid

          call ibis_column_read(ibis_in,r_cam,4,1,npict,istat)
          if (istat.ne.1) call ibis_signal(ibis_in,' ',istat)
          DO I=1,npict
            SN(i) = r_cam(i)
          END DO

          call ibis_column_read(ibis_in,rsvec(1,1),5,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,rsvec(1,2),6,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,rsvec(1,3),7,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,omang(1,1),21,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,omang(1,2),22,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,omang(1,3),23,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,radpol,26,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,radeq,27,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,focal,28,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,optaxl,29,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,optaxs,30,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_column_read(ibis_in,scale,31,1,npict,istat) 
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

          call ibis_file_close(ibis_in,' ',istat)
          if ( istat .ne. 1 ) call ibis_signal(ibis_in,' ',istat)

        else

          call init_spice		!Initialize spice server
          CALL XVPARM('TARGET',planet,ICNT,IDEF,' ')
          CALL ucase(planet, planet)

          call xvparm('SCET',scet,ifds,idef,' ')
          ifds=ifds/6
          if (ifds.eq.0) call mabend('SCET parameter missing',' ')

          CALL XVPARM('CAMERA',SN,isn,IDEF,' ')
          if (isn.ne.ifds)
     +		 call mabend('CAMERA required for each frame.',' ')

          CALL XVPARM('ID',FID,ifid,IDEF,' ')
          if (ifid.ne.ifds)
     +       call mabend('frame sequence # req for each frame.',' ')

        endif

        idata(39)=7
        if (project.eq.'VGR-1' .or. project.eq.'VGR-2'
     +		.or. project.eq.'VIKOR') idata(39)=8
        if (xvptst('OBJECT')) idata(39)=8

	TOT = 0
        call mvcl( project, conv, 5)
	call xvparm('linc',linc,icnt,idef,' ')
	call xvparm('sinc',sinc,icnt,idef,' ')


C-------Main loop
	DO 200 JJ=1,IFDS
	    call xvmessage(' ***********************',' ')
            call mve(4,1,sn(i),conv(3),1,1)

            if(input_file)then
               i=nint(fid(jj))
               call prnt(4,1,i,'IBIS SPICE entry # .')
               data(25)=radpol(i)
               data(26)=radeq(i)
               data(27)=focal(i)
               data(28)=optaxl(i)
               data(29)=optaxs(i)
               data(30)=scale(i)
               data8(10)=rsvec(i,1)
               data8(11)=rsvec(i,2)
               data8(12)=rsvec(i,3)
               data(38)=dsqrt(data8(10)**2+data8(11)**2+data8(12)**2)
               alpha=omang(i,1)
               delta=omang(i,2)
               kappa=omang(i,3)
               call fromeuler(alpha,delta,kappa,data8)
          
            else 
               call prnt(4,6,scet(1,jj),'SCET:y,d,h,m,s,ms.')
               CALL MSEDR(IND,SN,BUF,buf,DATA,planet,
     +             scet(1,jj),project)

            endif

            call prnt(8,9,data(1),'OM:.')
            call prnt(8,3,data(19),'RS:.')

            RL = 0          ! Initialize
            RS = SINC

            DO 10 I=1,1000
                RL = RL + LINC
                IF (RL.GT.1000-LINC) THEN
      	            RL = LINC
	            RS = RS + SINC
	            IF (RS.GT.1000-SINC) GO TO 15  !GO TO NEXT FRAME
                END IF

                CALL CONVEV(IND,DATA,DATA,RL,RS,RLT,RLN,2,CONV)
                IF (IND .EQ. 0) THEN

C-------IS RL,RS WITHIN 125 PIXELS OF LIMB? YES=SKIP IT
                  CALL CONVEV(IND,DATA,DATA,RL-125.,RS,RLTX,RLNX,2,CONV)
                  IF (IND .NE. 0) GO TO 10
                  CALL CONVEV(IND,DATA,DATA,RL+125.,RS,RLTX,RLNX,2,CONV)
      	          IF (IND .NE. 0) GO TO 10
   	          CALL CONVEV(IND,DATA,DATA,RL,RS-125.,RLTX,RLNX,2,CONV)
	          IF (IND .NE. 0) GO TO 10
	          CALL CONVEV(IND,DATA,DATA,RL,RS+125.,RLTX,RLNX,2,CONV)
	          IF (IND .NE. 0) GO TO 10

	          TOT = TOT + 1
                  L(TOT) = RL
	          S(TOT) = RS
	          LT(TOT) = RLT
	          LN(TOT) = RLN
	          F(TOT) = FID(JJ)
	          IF (TOT .EQ. 1000) GO TO 15	!END
	        END IF
10 	    CONTINUE				!POINT LOOP


15	    TOTTMP = TOT - LASTTOT
   	    LASTTOT = TOT
	    CALL PRNT(4,1,TOTTMP,' POINTS GATHERED = .')
	    IF (TOT .EQ. 1000) GO TO 201	!END

200	CONTINUE				!FRAME LOOP


c
c       finished processing - output data
c

201	continue

	call xvunit(ou,'out',1,istat,' ')
        call ibis_file_open(ou,ibis_out,'write',5,tot,' ',' ',istat)
        if ( istat .ne. 1 ) call ibis_signal_u(ou,istat)

        CALL xvmessage(' ***********************',' ')
	CALL PRNT(4,1,TOT,'0TOTAL POINTS GATHERED = .')

	call ibis_column_write(ibis_out,F,1,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,L,2,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,S,3,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,LT,4,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	call ibis_column_write(ibis_out,LN,5,1,tot,ISTAT)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
        call ibis_file_close(ibis_out,' ',istat)
          if ( istat .ne. 1 ) call ibis_signal(ibis_out,' ',istat)
	return
        end

c **************************************************************
	subroutine fromeuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)
	real*8	c(3,3)      ! output - derived rotation matrix 

c  this routine performs the functional inverse of routine toeuler.  the
c  three euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

	real*8  cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8  cos_kappa, sin_kappa, dtor

        dtor = 3.141592653589793D0/180.D0
	sin_alpha = sin(alpha*dtor)
	cos_alpha = cos(alpha*dtor)
	sin_delta = sin(delta*dtor)
	cos_delta = cos(delta*dtor)
	sin_kappa = sin(kappa*dtor)
	cos_kappa = cos(kappa*dtor)
	c(1,1) = -sin_alpha * cos_kappa - cos_alpha * sin_delta *
     &							 sin_kappa
	c(1,2) =  cos_alpha * cos_kappa - sin_alpha * sin_delta *
     &							 sin_kappa
	c(1,3) =  cos_delta * sin_kappa
	c(2,1) =  sin_alpha * sin_kappa - cos_alpha * sin_delta *
     &							 cos_kappa
	c(2,2) = -cos_alpha * sin_kappa - sin_alpha * sin_delta *
     &							 cos_kappa
	c(2,3) =  cos_delta * cos_kappa
	c(3,1) =  cos_alpha * cos_delta
	c(3,2) =  sin_alpha * cos_delta
	c(3,3) =  sin_delta
	return
	end

c ***********************************************************
      SUBROUTINE MSEDR(IND,ISN,BUF,buf8,DATA,PLANET,
     +                 scet,project)
      integer*4 ind,isn

      REAL*4 DATA(40),BUF(200)
      REAL*8 PI,R2,DTOR,buf8(100)

      character*12 planet
      character*5 project
      integer*4 scet(6),fds


C
      PI = 3.141592653589793D0
      dtor = pi/180.D0

c get camera constants
      call getcamcon(project,isn,data(27),data(28),data(29),
     +               data(30),ind)
      if(ind.ne.0)then
         call xvmessage('GETCAMCON: bad indicator',' ')
         call abend
      endif

      call getspice3(project,planet,isn,fds,scet,.TRUE.,buf,ind)
      if(ind.ne.1)then
         call prnt(4,1,ind,'GETSPICE3: bad ind=.')
         call abend
      endif
      CALL cmsource(buf, ind)

C          PICTURE BODY
      DATA(36) = BUF(9)

C          PLANET RADII
      DATA(25) = BUF8(15)
	rp2=data(25)*data(25)
      DATA(26) = (BUF8(13)+buf8(14))/2.0
	re2=data(26)*data(26)
	r2=1.0D0*rp2/re2

C          RANGE
      DATA(38) = BUF8(27)

C          SUBSPACECRAFT (LAT,LON)
      DATA(31) = BUF8(30)
      r=buf8(31)
      DATA(32) = AMOD(r+360.,360.)

C          NORTH ANGLE
      r=buf8(68)
      DATA(35) = AMOD(r+90.,360.)

C-----OM MATRIX
      CALL MVE(8,9,BUF8(59),DATA,1,1)

C-----RS VECTOR
      CALL MVE(8,3,BUF8(22),DATA(19),1,1)      !UPDATED ONE

      RETURN
      END
