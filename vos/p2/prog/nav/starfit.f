CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to improve the camera pointing by fitting to the star
C background.
C
      SUBROUTINE STARFIT(IMG,PIC,HPIC,NL,NS,FOV)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      COMMON/CSTAR1/ISAO,SAOFILE
      COMMON/CSTAR2/STARNAME,STARTYPE
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG,HIMAG1
      CHARACTER*72 SAOFILE
      CHARACTER*13 STARNAME(2000)
      CHARACTER*3 STARTYPE(2000)

      COMMON/GSCCMN/ GSCPFX
      CHARACTER*199 GSCPFX

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      INTEGER*4 SHIST(13)
      LOGICAL PARMTST,XVIPTST
C
C set ISAO to -2 if we are to use GSC catalog instead of SAO catalog
      call xvparm('GSCPFX',gscpfx,icnt,idef,' ')
      if (icnt.eq.1 .and. gscpfx(1:1).ne. ' ') then
        isao = -2
      else
        CALL XVPARM('SAO',saofile,icnt,idef,' ') !Get SAO catalog file spec
        if (icnt.eq.0) then
          call xvmessage(' Missing SAO parameter....',' ')
          call xvmessage(' See help on SAO parameter.',' ')
          call xvmessage(' NAV task cancelled',' ')
	  call abend
        endif
      endif

      MODEL=5
      CALL GETNAV		!Install planet geometry
      IF (ISAO.EQ.-1) THEN
         CALL XVUNIT(ISAO,'S',1,IND,'U_NAME',SAOFILE,' ')
         CALL XVSIGNAL(ISAO,IND,.TRUE.)
      ENDIF
      CALL T1950(IDATE,ITIME,RSC,t)
      CALL GETSTARS(ISAO,CM,FOV,T,stars,spts,nstars,
     &     starname,startype,*20)
      CALL STARHIST(STARS,SPTS,NSTARS,NL,NS,shist,lomag,himag)
      CALL DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,0)
C
   20 CALL XVINTRACT('STARFIT','STARFIT ')
      IF (XVIPTST('HELP')) CALL XVINTRACT('STARFIT',' ')
      IF (XVIPTST('EXIT')) RETURN

      CALL ISETCOLOR

      IF (XVIPTST('SSTARS')) THEN
          IF (NSTARS.GT.0)
     &		 CALL DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,0)
          GOTO 20
      ENDIF

      IF (PARMTST('HIMAG',HIMAG,I)) THEN
          IF (NSTARS.GT.0)
     &		 CALL DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,1)
          GOTO 20
      ENDIF

      IF (XVIPTST('MSTARS')) THEN
          CALL MOVESTAR(PIC,HPIC,NL,NS,*20)
          CALL STARHIST(STARS,SPTS,NSTARS,NL,NS,shist,lomag,himag1)
          IF (NSTARS.GT.0)
     &		 CALL DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,1)
          GOTO 20
      ENDIF

      IF (XVIPTST('TIEPOINT')) THEN
          CALL TIESTAR(*20)
c         CALL GETSTARS(ISAO,CM,OAL,OAS,ZSCALE,FOV,T,
c    &		stars,spts,nstars,starname,startype,*20)
c  above call has 3 arguments not in the (current) function!
c  remove these to avoid warnings during build ... (lwk / oct09)
          CALL GETSTARS(ISAO,CM,FOV,T,
     &		stars,spts,nstars,starname,startype,*20)
          CALL STARHIST(STARS,SPTS,NSTARS,NL,NS,shist,lomag,himag1)
          IF (NSTARS.GT.0)
     &		 CALL DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,1)
          GOTO 20
      ENDIF

      IF (XVIPTST('EDIT')) THEN
          CALL EDITNAV(IMG,PIC,HPIC,NL,NS)
          CALL UPDATESTAR  !VRH 7/4/89 update stars - new routine
          CALL STARHIST(STARS,SPTS,NSTARS,NL,NS,shist,lomag,himag1)
          GOTO 20
      ENDIF

      IF (XVIPTST('PARAMS')) THEN
         CALL RPARAM
         GOTO 20
      ENDIF

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 20
      CALL SDISPLAY(PIC,HPIC,NL,NS,IND) !VRH 6/28/89 new calling list
      IF (IND.NE.0) GOTO 20
      GOTO 20
C
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read SAO catalog and returns stars within field-of-view of camera
C
C Inputs:  CM=C-matrix
C	   OAL,OAS=Line-sample of optical-axis intercept point
C          ZSCALE=FL*PICSCALE
C          FOV=camera field-of-view (radians)
C	   T=days since EME50
C
C Outputs: STARS(3,N)=(RA,DEC,MAGNITUDE)
C	   SPTS(2,N)=(RLINE,RSAMP)
C	   NSTARS = number of stars returned
C
C SAO catalog format:  In the file, the data
C is grouped into 18 segments of 10 degrees each
C and is ordered in RA within each segment.
C Each star has a data record of 36 bytes.
C RA (radians), RAMu (pm in sec time/year)
C DEC (radians), DECMu (pm in sec arc/year)
C Visual Magnitude,  13-byte Name and 3-byte Spectral type
C The first 4 records of the output catalog are 18 sets
C of pointers to the records for the start and end of
C each segment.  Record count starts at 0.
C The real values (RA, RAMu, DEC, DECMu and Mag) are in IEEE
C The pointer integers are in network byte order INTFMT=HIGH

      SUBROUTINE GETSTARS(ISAO,CM,FOV,T,stars,spts,nstars,
     &                    starname,startype,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 STARS(3,2000),SPTS(2,2000)
      CHARACTER*13 STARNAME(2000)
      CHARACTER*3 STARTYPE(2000)
      REAL*8 CM(3,3)

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      COMMON/CONST/PI,DTR,RTD
      REAL*8 LODEC,LORA,JTIME
      CHARACTER*80 MSG

      COMMON/GSCCMN/ GSCPFX
      CHARACTER*199 GSCPFX

      common /starinfo/ stardata, starnm, startp
      real*4 stardata(5)
      character*13 starnm
      character*3 startp

      integer*4 conv_buf(12)
      integer*4 ptr(2,18),pointers(36)
      real*4 DATA(5)

      logical from
      doubleprecision oara, oadec, oakappa, oauv(3), oarov, kappa0
      integer maxstars / 2000 /
      save

CBTC if the VICAR unit number of the SAO Catalog file is -2, then use the 
CBTC STScI Guide Star Catalog
      if ( isao .eq. -2) then

CBTC - get optic axis, convert to GSC reference frame & unit vector
CBTC - set the Radius-Of-View to be the input FOV - so we will get stars 
CBTC     within a circle of diameter twice the FOV around the optic axis
        CALL COMPCM(CM,oara,oadec,oakappa)
CBTC
CBTC - convert oara, oadec to GSC reference frame
        from = .false.
        call fromorto_star( oara, oadec, from)
        call radrec( 1d0, oara, oadec, oauv)
        oarov = FOV
CBTC
CBTC - load stars within oarov of optic axis into the stars() array

        call xvmessage( 'GSC search disabled in this version', ' ')
        nstars = 0
CBTCGSCDISABLED x        call seagsc( oara, oadec, oauv, oarov, gscpfx
CBTCGSCDISABLED x     &             , maxstars, stars, nstars)

CBTC
CBTC - convert stars' ra & dec back to local reference frame
        IF (NSTARS.GT.0) then
          from = .true.
          do i1=1,nstars
            ra = stars(1,i1)
            dec = stars(2,i1)
            call fromorto_star( ra, dec, from)
            stars(1,i1) = ra
            stars(2,i1) = dec
          enddo
          CALL UPDATESTAR
          return
        endif
        CALL XVMESSAGE('***No stars found in GSC within field-of-view'
     .                ,' ')
        RETURN1
      endif
CBTC
  110 FORMAT('STAR=',I4,2F8.3,F5.2,2F10.5,' New=',2F8.5)

      JTIME = T/365.25D0		!Years since 1-1-1950      
      WRITE(MSG,*)'Years of plate since 1/1/1950 is:',JTIME
      CALL XVMESSAGE(MSG,' ')
C     ....Get right-ascension & declination of optical-axis intercept
      CALL COMPCM(CM,ra0,dec0,kappa0)
CBTC
CBTC convert optic axis to SAO reference frame
      from = .false.
      call fromorto_star( ra0, dec0, from)
      from = .true.
CBTC
C     ....Determine ra,dec limits of a box twice the camera's
C     ....field-of-view
      TFOV = 2.*FOV
      HIDEC = DEC0 + TFOV/2.
      LODEC = HIDEC - TFOV
      HIDEC = MIN(PI/2.,HIDEC)
      LODEC = MAX(-PI/2.,LODEC)
      HIRA = RA0 + TFOV/2./DCOS(DEC0)  !VRH 8/16/89 correct for smaller
      LORA = HIRA - TFOV/DCOS(DEC0)    !VRH 8/16/89 RA circles at poles
      IF (LORA .LT. 0.) LORA = LORA + 2.*PI
      IF (HIRA .GT. 2.*PI) HIRA = HIRA - 2.*PI
      IF (DEC0.GT.(PI/2.-TFOV).OR.DEC0.LT.(TFOV-PI/2.)) THEN  !VRH 8/16/89
          LORA = 0.      ! If within FOV of pole, whole RA range is in FOV
          HIRA = 2.*PI
      ENDIF
CBTC      WRITE(MSG,*)'RA FOV limits:',lora,hira
      WRITE(MSG,*)'RA FOV limits(EME50):',lora/dtr,hira/dtr
      CALL XVMESSAGE(MSG,' ')
      IF (LORA .GT. HIRA) 
     &   CALL XVMESSAGE('FOV contains 0 RA, limits reversed',' ')
CBTC      WRITE(MSG,*)'Dec FOV limits:',lodec,hidec
      WRITE(MSG,*)'Dec FOV limits(EME50):',lodec/dtr,hidec/dtr
      CALL XVMESSAGE(MSG,' ')

C     ....Determine the blocks of declination which span the plate fov.
      IBTOP = INT((90.-HIDEC*RTD)/10.) + 1 
      IBBOT = INT((90.-LODEC*RTD)/10.) + 1 
      WRITE(MSG,*)'Star Catalog blocks used from',ibtop,' to',ibbot
      CALL XVMESSAGE(MSG,' ')

C     ....Open SAO catalogue compressed by IDL procedure CONVERT_SAO.PRO
      CALL XVOPEN(ISAO,IND,'COND','NOLABELS NOBLOCK',
     &		'I_FORMAT','FULL','CONVERT','OFF','U_NS',9,
     &          'OPEN_ACT','SA','IO_ACT','SA',' ')
      NSTARS = 0

c read pointers: star catalog data is BigEndian (HIGH) and IEEE
c integer elements (pointers)
      CALL XVREAD(ISAO,pointers,IND,'LINE',1,' ')  !Read pointers from header
      CALL XVREAD(ISAO,pointers(10),IND,'LINE',2,' ')
      CALL XVREAD(ISAO,pointers(19),IND,'LINE',3,' ')
      CALL XVREAD(ISAO,pointers(28),IND,'LINE',4,' ')
      call xvtrans_in(conv_buf,'FULL','FULL','HIGH','IEEE', status)
      call xvtrans(conv_buf, pointers, ptr, 36)

C     ....Now get all the stars in the area
      DO 50 I1=IBTOP,IBBOT	   !Loop through blocks
      WRITE(MSG,*)'RECORD RANGE=',PTR(1,I1),PTR(2,I1)
      CALL XVMESSAGE(MSG,' ')

      IF(PTR(1,I1).EQ.0) GOTO 50 ! In case no data VRH 6/22/89

c Narrow down RA range
      JPTR1 = PTR(1,I1)
      JPTR2 = PTR(2,I1)
      NSTEP = (JPTR2-JPTR1)/36
      IF (LORA.LT.HIRA.AND.NSTEP.GT.10) THEN
        DO 30 I2=PTR(1,I1),PTR(2,I1),NSTEP
        CALL XVREAD(ISAO,stardata,IND,'LINE',I2+1,' ')  !Read star data
        call xvtrans_in(conv_buf,'REAL','REAL','HIGH','IEEE', status)
        call xvtrans(conv_buf, stardata, data, 5)
        IF (DATA(1).LT.LORA) JPTR1 = I2
        IF (DATA(1).GT.HIRA.AND.JPTR2.EQ.PTR(2,I1)) JPTR2 = I2
30      CONTINUE
      ENDIF
      
      WRITE(MSG,*)'Record inside block RANGE=',JPTR1,JPTR2
      CALL XVMESSAGE(MSG,' ')

      DO 40 I2=JPTR1,JPTR2 !Loop through valid records in block I1
      CALL XVREAD(ISAO,stardata,IND,'LINE',I2+1,' ')  !Read star data from file
      call xvtrans_in(conv_buf,'REAL','REAL','HIGH','IEEE', status)
      call xvtrans(conv_buf, stardata, data, 5)

      IF (LORA.GT.HIRA) THEN
         IF (DATA(1).LT.LORA.AND.DATA(1).GT.HIRA) GOTO 40
      ELSE
         IF (DATA(1).GT.HIRA) GOTO 50    !Determine if star
         IF (DATA(1).LT.LORA) GOTO 40    !is within fov...
      ENDIF
      IF (DATA(3).GT.HIDEC) GOTO 40
      IF (DATA(3).LT.LODEC) GOTO 40
C     ....Apply proper motion to star
      RA = DATA(1)+(DATA(2)*JTIME/240.D0)*DTR
      DEC = DATA(3)+(DATA(4)*JTIME/3600.D0)*DTR

CBTC  ....Convert to ISYSTEM
      call fromorto_star( ra, dec, from)
CBTC
      IF (NSTARS.GE.2000) THEN
         CALL XVMESSAGE('***More than 2000 stars within field-of-view',
     .      ' ')
         CALL XVMESSAGE('***Remaining stars ignored',' ')
         GOTO 100
      ENDIF
      NSTARS = NSTARS + 1
      STARS(1,NSTARS) = RA
      STARS(2,NSTARS) = DEC
      STARS(3,NSTARS) = DATA(5)	!Visual magnitude
      ICHAR = INDEX(STARNM,CHAR(0))
      IF (ICHAR .GT. 0) THEN
         STARNAME(NSTARS) = STARNM(1:ICHAR-1)
      ELSE IF (ICHAR.EQ.1) THEN
         STARNAME(NSTARS) = ' '
      ELSE 
         STARNAME(NSTARS) = STARNM
      ENDIF
      ICHAR = INDEX(STARTP,CHAR(0))
      IF (ICHAR .GT. 0) THEN
         STARTYPE(NSTARS) = STARTP(1:ICHAR-1)
      ELSE IF (ICHAR.EQ.1) THEN
         STARTYPE(NSTARS) = ' '
      ELSE
         STARTYPE(NSTARS) = STARTP
      ENDIF
      IF (IBUG.EQ.1) THEN
         WRITE(MSG,110) NSTARS,DATA(1),DATA(3),DATA(5),
     &     DATA(2),DATA(4),RA,DEC
         CALL XVMESSAGE(MSG,' ')
      ENDIF
   40 CONTINUE
   50 CONTINUE

  100 CALL XVCLOSE(ISAO,IND,' ')
      CALL UPDATESTAR  !VRH 7/4/89 update stars - new routine
      IF (NSTARS.GT.0) RETURN
      CALL XVMESSAGE('***No stars found in catalog within field-of-view'
     .      ,' ')
      RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE STARHIST(STARS,SPTS,NSTARS,NL,NS,shist,lomag,himag)
      REAL*4 STARS(3,2000),SPTS(2,2000)
      INTEGER*4 SHIST(13),LOMAG,HIMAG
      CHARACTER*80 MSG

      RNL = NL		!Float the image size for efficiency
      RNS = NS
      CALL ZIA(SHIST,13)

      DO 5 I=1,NSTARS
      RLINE = SPTS(1,I)
      IF (RLINE.LT.1.OR.RLINE.GT.RNL) GOTO 5
      RSAMP = SPTS(2,I)
      IF (RSAMP.LT.1.OR.RSAMP.GT.RNS) GOTO 5
      IMAG = STARS(3,I)
      IF (IMAG.LT.0) IMAG=0
      IF (IMAG.GT.12) IMAG=12      
      SHIST(IMAG+1) = SHIST(IMAG+1)+1
    5 CONTINUE

      HIMAG = 1
      LOMAG = 2000

      IF (SHIST(1).GT.0) THEN
         WRITE(MSG,*) 'There are ',SHIST(1),
     &	' stars at or below magnitude 0'
         CALL XVMESSAGE(MSG,' ')
      ENDIF

      DO 10 I=1,11
      IF (SHIST(I+1).EQ.0) GOTO 10
      WRITE(MSG,*) 'There are ',SHIST(I+1),' stars at magnitude',I
      CALL XVMESSAGE(MSG,' ')
      HIMAG = I + 1
      IF (LOMAG.EQ.2000) LOMAG=I
   10 CONTINUE

      IF (LOMAG.EQ.2000) LOMAG=I

      IF (SHIST(13).GT.0) THEN
         WRITE(MSG,*) 'There are ',SHIST(13),
     &	' stars at or above magnitude 12'
         CALL XVMESSAGE(MSG,' ')
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to manually register the star background and update OM matrix.
C
      SUBROUTINE MOVESTAR(PIC,HPIC,NL,NS,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CONST/PI,DTR,RTD

      REAL*4 CPTS(2,100),APTS(2,100),rl4,rs4
      INTEGER*2 IPTS(100)
      LOGICAL XVIPTST
      CHARACTER*80 MSG
  110 FORMAT('DL=',F8.3,' DS=',F8.3,' DT=',F8.4,' RMS=',F8.3,' pixels')

      CALL STARPTS(PIC,HPIC,NL,NS,STARS,SPTS,NSTARS,LOMAG,HIMAG,
     &		cpts,apts,ipts,npts)
      IF (NPTS.EQ.0) RETURN

c      do i=1,npts
c	write(*,'(a,i3,a,f6.1,X,f6.1,a,f6.1,x,f6.1)') ' star ',
c    &   i,': c-',cpts(1,i),cpts(2,i),' a-',apts(1,i),apts(2,i)
c      end do

    5 CALL XVINTRACT('CHISQ',
     &   ' Specify type of fit (Enter ''CHI2, ''CHI3 or ''EXIT)')
      IF (XVIPTST('EXIT')) RETURN1
      IF (XVIPTST('CHI2')) THEN
          IFIT = 2
          DT = 0.D0
      ELSE
          IF (XVIPTST('CHI3')) THEN
               IFIT = 3
          ELSE
               GOTO 5
          ENDIF
      ENDIF

      IF (ITYPE.EQ.7) THEN
         XOAL = OAL_IS
         XOAS = OAS_IS
      ELSE
         XOAL = OAL
         XOAS = OAS
      ENDIF

      CALL GETANGLES2(CM,theta_n,theta_a,theta_b)
      LOOP_COUNT = 0
      IMODE = 0

      IF(theta_a.LT.10.*DTR) THEN !VRH 7/26/89 automatic direct CM correction
         CALL XVMESSAGE(
     .      'NOTE: ThetaA (Optic axis to celes pole) <10 deg',' ')
         CALL XVMESSAGE(
     .      'Making pointing correction directly to C-Matrix',' ')
c12345678901234567890123456789012345678901234567890123456789012345678907234567890
      ENDIF

   10 IF (IFIT.EQ.2) THEN
           CALL FIT2(APTS,CPTS,NPTS,dl,ds)
      ELSE
           CALL FIT3(APTS,CPTS,NPTS,XOAL,XOAS,dl,ds,dt,*999)
           THETA_N = THETA_N + DT
      ENDIF

      IF (ITYPE.EQ.7) THEN 	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF

      IF (theta_a.LT.10.*DTR) THEN  !VRH 7/26/89 corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES2(CM,theta_n,theta_a,theta_b)
      ELSE ! Regular way
         CALL MOVE1(DL,DS,theta_n,theta_a,theta_b,ZSCALE)
         CALL OMMATRIX(THETA_N,THETA_A,THETA_B,cm)	!Update CM-matrix
      ENDIF
      RMS = 0.D0

      DO I=1,NPTS
         J = IPTS(I)
         RAS = STARS(1,J)
         DEC = STARS(2,J)
         CALL STARINV(RAS,DEC,rline,rsamp,CM,OAL,OAS,ZSCALE)
         IF (ITYPE.EQ.7) then
	   CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(RLINE),sngl(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	   rline=rl4
	   rsamp=rs4
	 endif
         CPTS(1,I) = RLINE
         CPTS(2,I) = RSAMP
         RMS = RMS + (APTS(1,I)-RLINE)**2 + (APTS(2,I)-RSAMP)**2
      ENDDO

      RMS = DSQRT(RMS/NPTS)
      WRITE(MSG,110) DL,DS,DT*RTD,RMS
      CALL XVMESSAGE(MSG,' ')

      IF (IMODE.EQ.1) GOTO 20
      DIFF = DSQRT(DL**2 + DS**2)
      IF (DIFF.LT.0.01.AND.DABS(DT).LT.0.0001D0)
     &     IMODE=1			!Set up for last iteration
      LOOP_COUNT = LOOP_COUNT + 1
      IF (LOOP_COUNT.GE.15) THEN
           CALL XVMESSAGE('***Star fit converges slowly',' ')
           IMODE = 1			!Set up for last iteration
      ENDIF
      GOTO 10
C
   20 CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL UPDATENAV
      CALL UPDATESTAR
      RETURN

  999 RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to acquire star tiepoints by matching the computed star background
C to the imaged stars via the cursor.
C Outputs: CPTS,APTS=(line,samp) coordinates for matching computed stars
C             and star images, respectively.
C          IPTS=pointer to SPTS array for each tiepoint.
C	   NPTS=number of tiepoints acquired.
C
      SUBROUTINE STARPTS(PIC,HPIC,NL,NS,STARS,SPTS,NSTARS,LOMAG,HIMAG,
     &      cpts,apts,ipts,npts)
      INTEGER HIMAG

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 STARS(3,2000),SPTS(2,2000),CPTS(2,100),APTS(2,100)
      INTEGER*2 IPTS(100)
      LOGICAL XVIPTST
      REAL*8 RL8,RS8

      NPTS = 0
      CALL XVMESSAGE('Begin manual registration of star background...',
     1 ' ')

    5 CALL XVMESSAGE('To select a star, move cursor to a graphics star',
     1 ' ')
      CALL XVMESSAGE('and hit RETURN.  You will then be asked to match',
     1 ' ')
      CALL XVMESSAGE('the selected star to its image via the cursor',
     1 ' ')
      CALL XVMESSAGE(
     &  'Repeat to acquire more points or type EXIT when done',' ')
      CALL XVMESSAGE('You may delete a point by typing D',' ')
      CALL XVMESSAGE(
     &  'You may adjust the display by using H,CZOOM,STRETCH, or ASTR',
     1 ' ')

   10 CALL XVINTRACT('TRACECRV','READY')
      IF (XVIPTST('HELP')) GOTO 5
      IF (XVIPTST('EXIT')) RETURN
      IF (XVIPTST('D')) THEN
         IF (NPTS.EQ.0) GOTO 10
         CALL FINDPT(CPTS,NPTS,1,imin)	!Find closest star to cursor
C               Delete it
         CALL DRAWDOT(APTS(1,IMIN),APTS(2,IMIN),0)
         NPTS = NPTS - 1
         DO I=IMIN,NPTS
             IPTS(I) = IPTS(I+1)
             CPTS(1,I) = CPTS(1,I+1)
             CPTS(2,I) = CPTS(2,I+1)
             APTS(1,I) = APTS(1,I+1)
             APTS(2,I) = APTS(2,I+1)
         ENDDO                         
         GOTO 10
      ENDIF

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.EQ.1) THEN
          IF (NSTARS.GT.0)
     &	     CALL DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,0)
          IF (NPTS.GT.0) CALL DRAWCURVE(APTS,NPTS,0)
      ENDIF
      IF (IND.GT.0) GOTO 10
C     ....Here to acquire a new star
      CALL FINDSTAR(STARS,SPTS,NSTARS,HIMAG,imin) !Find closest star to cursor
      CALL XVINTRACT('READY','Cursor star image and hit RETURN')
      IF (XVIPTST('EXIT')) GOTO 10
      CALL CURSOR(RL8,RS8)		!Read cursor position
      RLINE = RL8
      RSAMP = RS8
      PRINT 1000,CHAR(7)
 1000 FORMAT(1X,A1)

      IF (IPROJ.EQ.4) THEN
         CALL VGRCLEAR(ind,3,RES,BLEM,NBLEMS,3.,RLINE,RSAMP)
         IF (IND.EQ.0) GOTO 10
      ENDIF

      CALL DRAWDOT(SPTS(1,IMIN),SPTS(2,IMIN),255)
      CALL DRAWDOT(RLINE,RSAMP,255)	!Draw a dot there
      NPTS = NPTS + 1			!Record the star
      IPTS(NPTS) = IMIN
      CPTS(1,NPTS) = SPTS(1,IMIN)
      CPTS(2,NPTS) = SPTS(2,IMIN)
      APTS(1,NPTS) = RLINE
      APTS(2,NPTS) = RSAMP
      IF (NPTS.LT.100) GOTO 10
      CALL XVMESSAGE('Maximum number of points acquired',' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Update the star map with current navigation info (VRH)
C
      SUBROUTINE UPDATESTAR
      IMPLICIT REAL*8 (A-H,O-Z)

      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL
      real*4 rl4,rs4

      DO I=1,NSTARS		!Recompute star background
         RAS = STARS(1,I)
         DEC = STARS(2,I)
         CALL STARINV(RAS,DEC,rline,rsamp,CM,OAL,OAS,ZSCALE)
         IF (ITYPE.EQ.7) then
	   CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(RLINE),sngl(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	   rline=rl4
	   rsamp=rs4
	 endif
         SPTS(1,I) = RLINE
         SPTS(2,I) = RSAMP
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Find star in starmap that is closest to cursor.
C Output: IMIN = index of closest star.
C
      SUBROUTINE FINDSTAR(STARS,SPTS,NSTARS,HIMAG,imin)
      REAL*4 STARS(3,NSTARS),SPTS(2,NSTARS)
      INTEGER HIMAG
      REAL*8 RL8,RS8

      CALL CURSOR(RL8,RS8)		!Read cursor position
      RLINE = RL8
      RSAMP = RS8
      PRINT 1000,CHAR(7)		!Make bell go "ding"
 1000 FORMAT(1X,A1)
      RMIN = 1.D+20

C     ....Find closest point to cursor position
      DO 10 I=1,NSTARS
      IF (STARS(3,I).GT.FLOAT(HIMAG)) GOTO 10
      R = (SPTS(1,I)-RLINE)**2 + (SPTS(2,I)-RSAMP)**2
      IF (R.LT.RMIN) THEN
          RMIN = R
          IMIN = I
      ENDIF
   10 CONTINUE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to check cursor position and issue warning message if it
C is within radius R of a Voyager reseau mark or blemish.
C This routine is a modification of CLEANVGR.
C
C MODE=1 ...check for reseau marks
C     =2 ...check for camera blemishes
C     =3 ...check for both
C
C Return indicator (IND) =1 for normal return, =0 if cursor position
C is rejected.
C
      SUBROUTINE VGRCLEAR(ind,MODE,RES,BLEM,NBLEM,R,RLINE,RSAMP)
      REAL*4 RES(2,202),BLEM(4,1000)
      INTEGER*2 ROW_TYPE(24)/1,2,1,2,3,2,4,2,4,2,4,2,4,2,
     &    4,2,4,2,4,2,1,2,1,1/
      INTEGER*2 IROW(24)/1,13,24,36,47,51,62,66,77,81,92,
     &    96,107,111,122,126,137,141,152,156,167,179,190,190/
      INTEGER*2 ICOL(12,4)/0,1,2,3,4,5,6,7,8,9,10,11,
     &                     0,1,2,3,4,5,6,7,8,9,10,10,
     &                     0,1,1,1,1,155,155,155,155,2,2,3,
     &                     0,1,1,1,1,1,2,2,2,2,2,3/
      REAL*4 OFFSETS(4),LOFFSET,LEFTSAMP
      INTEGER*4 RTYPE
      LOGICAL XVIPTST
      CHARACTER*80 MSG  ! VRH 5/25/95 added variable previously undeclared
C
      IND = 1
      R2 = R**2			!Square of pixel radius
      IF (RES(1,195).LT.900.0) THEN  !If image-space, then
         RNL = 800.0		!number of lines=800
         RNS = 800.0		!number of samps=800
      ELSE			!else, object-space.
         RNL = 1000.0
         RNS = 1000.0
      ENDIF
      IF (RSAMP.LT.1.0 .OR. RSAMP.GT.RNS) RETURN
      IF (RLINE.LT.1.0 .OR. RLINE.GT.RNL) RETURN

      IF (MODE.EQ.2) GOTO 50
C     ....First, see if point is near a reseau mark
      DL = (RES(1,172)-RES(1,29))/18	!Pixel spacing between rows
      DS = (RES(2,105)-RES(2,97))/8	!Pixel spacing between columns
      TL1 = 0.5*(RES(1,6)+RES(1,18))	!Boundary btwn first two rows
      TL2 = 0.5*(RES(1,18)+RES(1,29))	!Boundary btwn second and third rows
      BL2 = 0.5*(RES(1,173)+RES(1,184))	!Boundary btwn rows 21 and 22
      BL1 = 0.5*(RES(1,184)+RES(1,196))	!Boundary btwn last two rows
      LOFFSET = 3.0*DL - 0.5*(RES(1,29)+RES(1,41))	!Line offset
      OFFSETS(1) = 6.0*DS - 0.5*(RES(2,29)+RES(2,30))	!Samp offset row type 1
      OFFSETS(2) = 5.0*DS - 0.5*(RES(2,100)+RES(2,101))	!Samp offset row type 2
      OFFSETS(3) = 6.0*DS - 0.5*(RES(2,48)+RES(2,49))	!Samp offset row type 3
      OFFSETS(4) = 6.0*DS - 0.5*(RES(2,93)+RES(2,94))	!Samp offset row type 4
C     ....Boundary btwn first two marks on rows 2 and 22
      LEFTSAMP = 0.5*(RES(2,13)+RES(2,14))
C     ....Boundary btwn last two marks on rows 2 and 22
      RIGTSAMP = 0.5*(RES(2,22)+RES(2,23))
C
      IL = (RLINE+LOFFSET)/DL
      IF (RLINE.LT.TL2) THEN
         IF (RSAMP.LT.LEFTSAMP) THEN
            D = (RES(1,1)-RLINE)**2 + (RES(2,1)-RSAMP)**2
            IF (D.LT.R2) GOTO 990
            D = (RES(1,13)-RLINE)**2 + (RES(2,13)-RSAMP)**2
            IF (D.LT.R2) GOTO 990
         ELSE
            IF (RSAMP.GT.RIGTSAMP) THEN
               D = (RES(1,12)-RLINE)**2 + (RES(2,12)-RSAMP)**2
               IF (D.LT.R2) GOTO 990
               D = (RES(1,23)-RLINE)**2 + (RES(2,23)-RSAMP)**2
               IF (D.LT.R2) GOTO 990
            ENDIF
         ENDIF
         IF (RLINE.LT.TL1) THEN
            IL = 0
         ELSE
            IL = 1
         ENDIF
      ENDIF
C
      IF (RLINE.GT.BL2) THEN
         IF (RSAMP.LT.LEFTSAMP) THEN
            D = (RES(1,190)-RLINE)**2 + (RES(2,190)-RSAMP)**2
            IF (D.LT.R2) GOTO 990
            D = (RES(1,179)-RLINE)**2 + (RES(2,179)-RSAMP)**2
            IF (D.LT.R2) GOTO 990
         ELSE
            IF (RSAMP.GT.RIGTSAMP) THEN
               D = (RES(1,201)-RLINE)**2 + (RES(2,201)-RSAMP)**2
               IF (D.LT.R2) GOTO 990
               D = (RES(1,189)-RLINE)**2 + (RES(2,189)-RSAMP)**2
               IF (D.LT.R2) GOTO 990
            ENDIF
         ENDIF
         IF (RLINE.GT.BL1) THEN
            IL = 22
         ELSE
            IL = 21
         ENDIF
      ENDIF
C
      RTYPE = ROW_TYPE(IL+1)
      OFFSET = OFFSETS(RTYPE)
      IS = (RSAMP+OFFSET)/DS
      IRES = IROW(IL+1) + ICOL(IS+1,RTYPE)
      D = (RES(1,IRES)-RLINE)**2 + (RES(2,IRES)-RSAMP)**2
      IF (D.LT.R2) GOTO 990

   50 IF (MODE.EQ.1) RETURN	!Cursor position accepted
C     ....Now check for camera blemishes
      DO I=1,NBLEM
         D = (BLEM(2,I)-RLINE)**2 + (BLEM(3,I)-RSAMP)**2
         IF (D.LT.R2) GOTO 992
      ENDDO
      RETURN			!Cursor position accepted

  990 D = SQRT(D)
      WRITE(MSG,1000) D
 1000 FORMAT('***Warning: Cursor position is within',F4.1,
     &' pixels from a reseau mark')
      GOTO 993
  992 D = SQRT(D)
      WRITE(MSG,1002) D
 1002 FORMAT('***Warning: Cursor position is within',F4.1,
     &' pixels from a camera blemish')
  993 CALL XVMESSAGE(MSG,' ')

  995 CALL XVINTRACT('QUERRY',
     &	' Do you wish to accept the point? (Enter Y or N)')
      IF (XVIPTST('Y')) RETURN  !Cursor position accepted
      IF (.NOT.XVIPTST('N')) GOTO 995
      IND = 0			!Cursor position rejected
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to derive an estimate for the C-matrix via tiepoint data
C (line,samp,ra,dec) for two stars.
C The C and OM matrices are updated.
C
C This routine is a modification of subroutine ESTIMATE,
C written by Vance Hammerle, 1987.
C
      SUBROUTINE TIESTAR(*)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      REAL*8 ME,INCL

      COMMON/CONST/PI,DTR,RTD

      REAL*8 L1,L2
      real*4 ll1,ll2,ss1,ss2
      REAL*4 PAR(2)
      LOGICAL rPARMTST,XVIPTST,LST

      HPI = PI/2.D0

      CALL XVMESSAGE('Begin TIEPOINTS mode...',' ')
      CALL XVMESSAGE('Select two stars with the cursor and',' ')
      CALL XVMESSAGE('supply their right-ascension and declination',' ')
      CALL XVMESSAGE('in response to the following prompts:',' ')

      CALL XVINTRACT('READY','Cursor first star and hit RETURN')
      IF (XVIPTST('EXIT')) GOTO 990
      CALL CURSOR(L1,S1)	!Read coordinates of first point
      ll1 = l1
      ss1 = s1
      CALL DRAWDOT(lL1,sS1,255)	!Draw a dot there
      CALL XVINTRACT('ANGLE','Enter right-ascension (hr,min,sec)')
      LST=rPARMTST('ANG',PAR,I)
      CALL HMSTORAD(PAR,a1)
      CALL XVINTRACT('ANGLE','Enter declination (deg,min,sec)')
      LST=rPARMTST('ANG',PAR,I)
      CALL DMSTORAD(PAR,d1)

      CALL XVINTRACT('READY','Cursor second star and hit RETURN')
      IF (XVIPTST('EXIT')) GOTO 990
      CALL CURSOR(L2,S2)	!Read coordinates of first point
      ll2 = l2
      ss2 = s2
      CALL DRAWDOT(L2,S2,255)	!Draw a dot there
      CALL XVINTRACT('ANGLE','Enter right-ascension (hr,min,sec)')
      LST=rPARMTST('ANG',PAR,I)
      CALL HMSTORAD(PAR,a2)
      CALL XVINTRACT('ANGLE','Enter declination (deg,min,sec)')
      LST=rPARMTST('ANG',PAR,I)
      CALL DMSTORAD(PAR,d2)
C
      IF (ITYPE.EQ.7) THEN	!If image-space, convert to object-space
        CALL CONVISOS(PROJECT,ICAM,sngl(L1),sngl(S1),ll1,ss1,1,CONV,
     &	 NPH,NPV,ind)
        CALL CONVISOS(PROJECT,ICAM,sngl(L2),sngl(S2),ll2,ss2,1,CONV,
     &	 NPH,NPV,ind)
	l1=ll1
	l2=ll2
	s1=ss1
	s2=ss2
      ENDIF

      if (a1.eq.a2) then
	PHI_12 = hpi
      else
	PHI_12 = DATAN2(D2-D1,A1-A2)
      endif
      if (a1.eq.a2) then
	THETA_12 = hpi
      else
	THETA_12 = DATAN2(L1-L2,S2-S1)
      endif
      R_12 = DSQRT((L2-L1)*(L2-L1) + (S2-S1)*(S2-S1))
C
      if (a1.eq.a2) then
	THETA_OA = hpi
      else
	THETA_OA = DATAN2(L1-OAL,OAS-S1)
      endif
      PHI_OA = THETA_OA - THETA_12 + PHI_12 - HPI
      R_OA = DSQRT((L1-OAL)*(L1-OAL) + (S1-OAS)*(S1-OAS))
C
      ALPHA_OA = A1 + (A1-A2)*(R_OA/R_12)*(DSIN(PHI_OA)/DCOS(PHI_12))
      DEC_OA = D1 + (D2-D1)*(R_OA/R_12)*(DCOS(PHI_OA)/DSIN(PHI_12))
C
      THETA_N = 3.D0*HPI - THETA_12 + PHI_12
      THETA_N = MOD(THETA_N,4.D0*HPI)
      THETA_A = HPI - DEC_OA
      THETA_B = 2.D0*HPI - ALPHA_OA
      CALL OMMATRIX(THETA_N,THETA_A,THETA_B,cm)		!Update CM-matrix
      CALL GETANGLES(CM,ME(1,3),PSC,angln,angla,anglb)
      CALL UPDATENAV
      RETURN
C
  990 CALL XVMESSAGE('TIEPOINTS mode cancelled',' ')
      RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert hrs,min,sec to radians.
C
      SUBROUTINE HMSTORAD(T,rad)
      REAL*4 T(3)       !Input hr,min,sec
      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD
C     ....rad = 15(h+(m+s/60)/60)*pi/180
      rad = 15.D0*(T(1)+(T(2)+T(3)/60.D0)/60.D0)*DTR
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert deg,min,sec to radians.
C
      SUBROUTINE DMSTORAD(A,rad)
      REAL*4 A(3)	!Input degrees,min,sec
      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD
C     ....rad = (deg + min/60 + sec/3600)*pi/180
      D = (A(2)+A(3)/60.D0)/60.D0
      IF (A(1).LT.0.0) D = -D
      rad = (A(1) + D)*DTR
      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert right-ascension from radians to hrs,min,sec.
C
      SUBROUTINE RADTOHMS(RAD,ih,im,rs)
      real*8 rad,rs

      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD

      DEG = RAD*RTD		!Convert from radians to degrees
      DEG = AMOD(DEG+360.,360.)	!Make sure it's positive
      rs = 240.0*DEG		!Convert degrees to secs-time
      ih = rs/3600.0
      rs = rs - 3600.0*ih
      im = rs/60.0
      rs = rs - 60.0*im
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert declination from radians to deg,min,secs.
C
      SUBROUTINE RADTODMS(RAD,id,im,rs)
      real*8 rad,rs

      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD

      DEG = RAD*RTD		!Convert from radians to degrees
      id = DEG
      rs = 3600.0*(DEG-id)	!Convert fractional degrees to secs-arc
      IF (DEG.LT.0.0) rs=-rs	!Minutes and seconds are always positive
      im = rs/60.0
      rs = rs - 60.0*im
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert (ra,dec) from NAV reference system (isystem) from-or-to
C reference system of the catalog.
C
      subroutine fromorto_star(ra, dec, from)
      implicit doubleprecision (a-h,o-z)
      logical from

      COMMON/CSTAR1/ISAO,SAOFILE
      CHARACTER*72 SAOFILE

      if ( isao .eq. -2) then	!Using STScI Guide Star Catalog
         call fromortogsc( ra, dec, from)
      else			!Otherwise, using SAO or equivalent
         call fromortosao( ra, dec, from)
      endif
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert star (RA,DEC) between B1950 (=SAO Catalog Ref. Frame) 
C and NAV local reference frame (ISYSTEM in SEDR common block)
C
      subroutine fromortosao(ra, dec,from)
      implicit doubleprecision (a-h,o-z)
      logical from
      doubleprecision mtx(3,3), v3(3), range
      integer b1950, toref
      character*80 msg

      COMMON/SEDR/SEDR(200),LBUF(100),ISYSTEM
      REAL*4 SEDR
      INTEGER LBUF
      INTEGER ISYSTEM	!1=b2000, 2=b1950

      integer oldsys
      data oldsys / -1 /
      save

      if (isystem .eq. 2) return

      if (oldsys .eq. isystem) goto 50	!skip if already done
      oldsys = isystem
      call irfnum( 'B1950', b1950)	!get SPICE index of B1950 ref frame

C       ...get SPICE index of ISYSTEM reference frame
      if (isystem .eq. 2) then
         toref = b1950
      elseif (isystem .eq. 1) then
         call irfnum( 'J2000', toref)
         call xvmessage( 'FROMORTOSAO:  J2000 chosen', ' ')
      else		!unknown ISYSTEM, use B1950, issue warning
         toref = b1950
         write(msg,'(1x,a,i3.3,a)') 
     &                 'FROMORTOSAO:  bad ISYSTEM (', isystem
     &               , '; using B1950; contact programmer'
         call xvmessage( msg, ' ')
      endif
      call irfrot(b1950,toref,mtx)	!Get rotation matrix

   50 if (b1950 .eq. toref) return	!Skip if b1950=isystem
      call radrec(1d0,ra,dec,v3)	!Convert ra, dec to vector
      if (from) then
         call mxv(mtx,v3,v3)		!Convert from b1950 to isystem
      else
         call mtxv(mtx,v3,v3)		!Convert from isystem to b1950
      endif
      call recrad(v3,range,ra,dec)	!Convert vector to ra, dec
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert star (RA,DEC) between J2000 (i.e. reference frame of GSC) 
C and NAV local reference frame (ISYSTEM in SEDR common block)
C
      subroutine fromortogsc(ra, dec, from)
      implicit doubleprecision (a-h,o-z)
      logical from
      doubleprecision mtx(3,3), v3(3), range
      integer j2000, toref
      character*80 msg

      COMMON/SEDR/SEDR(200),LBUF(100),ISYSTEM
      REAL*4 SEDR
      INTEGER LBUF
      INTEGER ISYSTEM	!1=b2000, 2=b1950

      integer oldsys
      data oldsys / -1 /
      save

      if (isystem .eq. 1) return	!Skip if isystem already b2000

      if ( oldsys .eq. isystem) goto 50	!Skip if matrix already computed
      oldsys = isystem
      call irfnum( 'J2000', j2000)	!get SPICE index of J2000
C     ...get SPICE index of ISYSTEM reference frame
      if ( isystem .eq. 1) then
         toref = j2000
      elseif ( isystem .eq. 2) then
         call irfnum( 'B1950', toref)
         call xvmessage( 'FROMORTOGSC:  B1950 chosen', ' ')
      else			!unknown ISYSTEM, use J2000, issue warning
         toref = J2000
         write(msg,'(1x,a,i3.3,a)') 
     &                 'FROMORTOBGSC:  bad ISYSTEM (', isystem
     &               , '; using J2000; contact programmer'
         call xvmessage( msg, ' ')
      endif
      call irfrot(j2000, toref, mtx)	!get rotation matrix

   50 if ( j2000 .eq. toref) return	!skip if isystem=j2000
      call radrec( 1d0,ra,dec,v3)	!convert ra, dec to vector
      if (from) then
         call mxv(mtx,v3,v3)		!isystem to j2000
      else
         call mtxv(mtx,v3,v3)		!j2000 to isystem
      endif
      call recrad(v3,range,ra,dec)	!convert vector to ra, dec
      return
      end
