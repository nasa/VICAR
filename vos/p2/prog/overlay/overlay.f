      INCLUDE 'VICMAIN_FOR'
C
C VICAR PROGRAM OVERLAY: Draws latitude-longitude grid on an image.
C Program documentation may be obtained by executing:
C
C	@OVERLAY DOC
C
C This creates file OVERLAY.DOC.
C
C Author:		Joel Mosher
C Date:         	February 1984
C Cognizant programmer:	L.W.Kamp
C
C Revisions:
C FEB 23 84  JAM   Original OVERLAY created by modifying PHOTFUNC.
C MAR  2 84  JAM   Add no input picture option.
C MAR  7 84  JAM   Fixed errors in longitude lines removed redundant grid points
C MAR  8 84  JAM   Calculate points to put lat-lon numbers
C MAR 14 84  JAM   Added MODULATE, LINC, and SINC keywords.
C OCT 27 85  JAM   Convert to VICAR2
C MAR  1 86  JAM   Fixed error in no input mode, added MAPLAB
C MAR 10 86  JAM   Added FIXRECT
C MAY 17 86  JAM   Added GLAT,GLON, rewrote point & number selection algorithms.
C MAY 27 87  JAM   VICAR2 parameter processing.
C MAR 11 88  GMY   Major code reorganization and clean-up:
C		    0) Rewrote Help File.
C		    1) Modularized algorithms.
C		    2) Fixed size field implementation.
C		    3) Fixed TARGET parameter.
C		    4) Fixed calculation of OM from C and ME.
C		    5) Fixed extraction of subspacecraft point from SEDR.
C		    6) Fixed extraction of radii from SEDR.
C		    7) Removed all redundant parameters.
C		    8) Replaced MODULATE algorithm with ALTERNATE algorithm.
C		    9) Fixed no input image option.
C		   10) Fixed annotation so equation is labeled.
C		   11) Fixed spacing of annotation.
C		   12) Fixed problem of extra grid line at SUPPRESS latitude.
C JUN 12 88  FFM  Rename parameter SEDR to SFILE.
C MAY 12 88  FFM  Incorporate SEDRSRC keyword.
C JUN 07 88  GMY  Fix processing of SIZE parameter.
C Jan 22 89  GMY  FRs 37250,37251,37298,42709,42701.
C Jul 19 89  MEM  Instead of drawing annotation at each intersection,
C		        merely draw it once for every grid line.
C Jul 27 89  MEM  When the picture scale is specified, draw a scale bar
C			in the lower left-hand corner.
C DEC    90  JJL  Conversion to project independent routines.
C Mar 30 92  JFM  Corrected handling of FINDLAT capture of constant latitudal
C                 grid lines for Lambert Conformal Conic and Sinusoidal
C		  projections near the poles, where projected image does not
C		  file the image file (FR 66586).
C Apr 02 92  JFM  Scale in lower-left corner of output made optional (FR 66545).
C Apr 17 92  JFM  Longitude and latitude annotation placements check against
C		  boundaries of image (FR 66549).
C May 26 92  JFM  SFILE parameter removed; HELP file slightly revised;
C		  RADIUS, REQUATOR, RPOLE limits removed; Error in no input
C		  image grid computation corrected (FR 68870, 75745, 76934).
c feb 93     jjl  Corrected annotation, added perspective, transverse mercator
c mar 93     jjl  Removed SFILE parameter
c oct 94     lwk  added options to draw limb & terminator
c mar 95     lwk  made program portable;  removed all map parameters, spice i/f,
c		etc., require that input image have map label.
c jul 95     lwk  fixed bug in limbpt that can cause divide by 0
c aug 95     lwk  changed xvparm('source',...) to 'sedrsrc' to match PDF name(!)
c nov 95     lwk  fixed min/max long limits for wraparound at 0 meridian
c		(was causing infinite loop in PREPSUB for skinny VJBARS case)
c apr 96     lwk  fixed xvmessage bug in nav1 & initializations in defgrid()
c jul 96     oam  Modified to call getspice2 instead of getspice. Included
c                 parameters for retrieving camera pointing from spice (DFR).
c may 97     lwk  added code to get subsolar lat/lon from nims label
c jun 97     lwk  fixed calculation of terminator points:  allow for sections
c		where limb is out of image but terminator is in it;  fixed
c		errors in evaluation of visibility of terminator;  don't cancel
c		task if no grid points found and limb/terminator requested
c sep 97     lwk  added code to LIMBPT to stop searching for point at suitable
c		distance once the N.pole is reached -- program was in an
c		infinite loop here for one case
c nov 97     lwk  more fixes to terminator algorithm:  non-visible points
c		were being selected for lat's where no limb is visible;  and
c		if limb didn't reach to N.pole, terminator stopped too
c 25nov97  -lwk- replaced FIXRECT by CONVEVR, to ensure that *every* call to
c		CONVEV is followed by a fix-up for Rectangular projection
c 25mar98  -lwk- added Planetographic option for Cube (LAT_TYP keyword)
c  8apr98  -lwk- fixed longitude lines so that they are more likely to
c		agree with the the numbers in the annotation
c  3may98  -lwk- fixed longitude lines again so they are always at integer
c		locations (as long as DLO1/DLO2 are integer!)
c  2jun98  -lwk- put in another check for infinite loop in LIMBPT (dlat=0)
c   Jun98  RRP  Broke long statements into smaller one to make it compile
c		under hp platform. AR-9644
c  9oct00  -lwk- remove lat_typ dependence for Simp.Cyl.
c 05aug02  KLEM Modified to compile/run on LINUX.  Symbol '&' (e.g., &999)
c               changed to '*' (e.g., *999).  Trig intrinsics (e.g., cosd,
c               sind, tand, etc.) change to cos, sin, tan, atan.  Intrinsic
c               'FLOATJ' changed to 'FLOAT'.  Variable ITYPE (INT*4) added
c               to subroutine FINDLON (line 899); call at 1059 attempted to
c               pass ITYPE.  Now consistent with FINDLAT. 
c 17dec02  -lwk- put in check for infinite loop in FINDLON similar to the
c		one in FINDLAT
c 13feb03  -lwk- merged my changes with Gary/Klem's
c 19aug03  -ntt- Enabled for 3D images.  Added I/O for BSP and BIL orgs.
c                Prohibit BIP organization types.
c 12jun04  -lwk- fixed bug in DRAWGRID (LINEOUT was reset to 0 too often);
c		added VIMSCMM to tasks with Sub-Sol point in label
c 23jun04  -lwk- allow for case where only terminator is drawn (no grid or
c		limb points) -- image was blank in this case;  fixed bugs
c		in LIMITS and DRAWNUM


      SUBROUTINE MAIN44

      REAL*4 NUMBER
      INTEGER*4 SL,SS,SAMP,OUNIT,LIMBDN,TERMDN,DNGRID,SB
      INTEGER*4 MAXPTS/300000/
      INTEGER*2 BUF,LINEBUF,SAMPBUF
      character*12 planet/'            '/
      CHARACTER*4 FMT
      REAL*8 PI,DTR,RTD
      real*8 psun1(3)
      real*4 ssol(2),data(40)
      COMMON/C1/BUF(10000),LINEBUF(300000),SAMPBUF(300000)
      COMMON/C1/NUMBER(2,6*360)
      COMMON/CONST/PI,DTR,RTD
      common/lattyp/lat_typ,rep2

      LOGICAL XVPTST,EAST,ALTERNATE,dlimb,dterm,nogrid

      CALL XVMESSAGE('*** Program OVERLAY version 23-Jun-2004 ***',' ')
      PI = 3.14159265358D0
      DTR = PI/180.D0
      RTD = 180.D0/PI

      IPRNT = 0
      IF (XVPTST('PRINT')) IPRNT=1

c     ...Open input image
      CALL IPOPEN(iunit,fmt,maxdn,sl,ss,sb,nlo,nso,nbo,*999)

C  process map label on input image
      call maplabel( iunit, planet, itype, scale, circum, data, ind)
      if (ind.lt.0) then
	call prnt(4,1,ind,' IND=.')
	call mabend('Error processing map labels')
      endif
      if (itype.eq.7.or.itype.eq.8) call mabend(
     & '*** Input image requires a map label!')

c  determine whether lats are planetocentric or graphic:
      lat_typ = 1	! by default centric
      if (.not.xvptst('PCENTRIC')) lat_typ = 0
      ! Simp.Cyl. is independent of detic/centric:
      if (itype.eq.10) lat_typ = 1
      if (lat_typ.eq.0) then
	if (data(25).eq.data(26)) then
	  lat_typ = 1	! after all, since no difference for a sphere
	else
	  rep2 = data(26)/data(25)
	  rep2 = rep2*rep2
	  call xvmessage(' Latitudes are Planetographic',' ')
	endif
      endif

      nogrid = xvptst('NOGRID')
      dlimb = xvptst('LIMB')
      if (dlimb.and.itype.ne.16) then
	call XVMESSAGE('** LIMB only allowed in perspective image!',
     1				 ' ')
	dlimb = .false.
      endif
      dterm = xvptst('TERMINAT')
      if (dterm.and..not.dlimb) then
	call XVMESSAGE(
     1		' ** TERMINATOR option requires LIMB option also!',
     1				 ' ')
	dterm = .false.
      endif

      if (dterm) then	! must get Sun position somehow ...
	call xvparm( 'SUBSOL', ssol, icnt, j, 2)	! subsolar lat/long
	if (icnt.eq.2) then
	  ! construct the unit Sun vector:
	  cosp = dcos(ssol(1)*dtr)
	  psun1(1) = cosp * dcos(ssol(2)*dtr)
	  psun1(2) = -cosp * dsin(ssol(2)*dtr)	! (West long.)
	  psun1(3) = dsin(ssol(1)*dtr)
	else 
	  ! if terminator wanted, and user did not supply subsolar loc,
	  ! then try to get sun position (only) from history labels:
	  call nav1( iunit, psun1, planet, ind)
	  if (ind.ne.0) then
	    dterm = .false.
	    call xvmessage('*** Unable to determine subsolar location',
     1			    ' ')
	    call xvmessage('*** TERMINATOR option cancelled...',' ')
	  endif
	ENDIF
      ENDIF

      if (nogrid .and. .not.dlimb .and. .not.dterm) call mabend(
     & ' *** GRID/LIMB/TERM all off ... nothing to do!! ***')

      IF (NLO*NSO.EQ.0) GOTO 990

      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUNIT,IND,.TRUE.)

      npts = -1			! to distingish from case where 0 points found
      if (nogrid) go to 400

C     ...Determine annotation text size
      CALL XVP('EXPAND',IZOOM,ICNT)

C     ...Compute latitude-longitude grid
      CALL GETGRID(DATA,CONV,SL,SS,NLO,NSO,SCALE,CIRCUM,MAXPTS,IPRNT,
     &	IZOOM,ITYPE,linebuf,sampbuf,number,npts,numnum,*400)

      CALL XVOPEN(OUNIT,ind,'OP','WRITE','U_FORMAT','WORD',
     &		'O_FORMAT',FMT,'U_NL',NLO,'U_NS',NSO, 'U_NB', NBO,
     &		'OPEN_ACT','SA','IO_ACT','SA',' ')

C     ...Determine type of grid desired.
      IF (XVPTST('WHITE')) DNGRID=MAXDN		!White grid specified.
      IF (XVPTST('BLACK')) DNGRID=0		!Black grid specified.
      ALTERNATE = .FALSE.
      IF (XVPTST('ALTERNAT')) THEN
	IF (IUNIT.GE.0) THEN
	  ALTERNATE = .TRUE.
	  DNGRID = MAXDN
	ELSE
	  CALL XVMESSAGE(' ***No input picture specified',' ')
	  CALL XVMESSAGE(' ***ALTERNAT keyword ignored',' ')
	ENDIF
      ENDIF

C     ...Draw the grid lines
      CALL DRAWGRID(IUNIT,OUNIT,LINEBUF,SAMPBUF,NPTS,
     &		SL,SS,SB,NLO,NSO,NBO,DNGRID,ALTERNATE,BUF)
C
C     ...Put lat,lon annotation at each grid intersection of overlay
      IF (XVPTST('NONUMBER')) GOTO 300		!Skip if NONUMBERS specified
      EAST = XVPTST('EAST')

      IF (IPRNT.NE.0) THEN
          CALL XVMESSAGE
     *      ('     PT#   LINE   SAMP   LAT    LONG  PUT IN NUMBERS',' ')
          CALL XVMESSAGE(' ',' ')
      ENDIF

      CALL XVCLOSE(OUNIT,IND,' ')
      CALL XVOPEN(OUNIT,IND,'OP','UPDATE','U_FORMAT','HALF',
     &       'O_FORMAT',FMT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
      ICOUNT = 0
C
C     ...Add the lat-lon annotation at the grid intersections
      DO 200 I=1,NUMNUM
        RLAT = NUMBER(1,I)
        RLON = NUMBER(2,I)
	IFLAG=0
	IF (RLON.GT.5000.) THEN
	  IFLAG=1
	  RLON = RLON - 7200.
	ENDIF
        rrlon = rlon + iflag*12.*scale*izoom ! 12 L for a longitude.
     $      + (1-iflag)*6.*scale*izoom       !  6 L for a latitude.
        call convevr(circum,ind,DATA,DATA,rline,rsamp,RLAT,RRLON,1,
     &  CONV)
        IF (IND.NE.0) GOTO 200
        LINE = RLINE - SL
        SAMP = RSAMP - SS
        IF (LINE.LT.1.OR.LINE.GT.NLO) GOTO 200
        IF (SAMP.LT.1.OR.SAMP.GT.NSO) GOTO 200
        ICOUNT = ICOUNT + 1
        TLON = AMOD(RLON,360.)
        IF (EAST) THEN
		TLON = 360. - TLON
	        TLON = AMOD(TLON,360.)
	ENDIF
        IF (IPRNT.NE.0) CALL XPRINT(ICOUNT,LINE,SAMP,RLAT,TLON,40)
        ILON = TLON
        ILAT = RLAT
        CALL DRAWNUM(OUNIT,LINE,SAMP,ILAT,ILON,NLO,NSO,SB,NBO,
     $     DNGRID,MAXDN,ALTERNATE,IZOOM,IFLAG,buf)
  200 CONTINUE
C
C    ...Add a scale bar 50 pixels wide at lower left corner.
  300 IF ((DATA(7).GT.0.).AND.(XVPTST('SCALEBAR'))) THEN
	 CALL SCALEBAR(OUNIT,DATA(7),NLO,NSO,SB,NBO,DNGRID,MAXDN,
     +					ALTERNATE,buf)
      ENDIF
      
C
  400 IF (.not.DLIMB) then
	if (npts.eq.0) go to 999
	GO TO 500
      endif

c   ... Planet Limb option
      ZSCALE = DATA(27)*DATA(30)
      ! compute some constants:
      CALL GETPC( DATA(19), DATA(26), DATA(26), DATA(25))
      CALL LIMBPT( DATA, DATA(19), PSUN1, DATA(32), DATA(28), 
     & DATA(29), ZSCALE, SL, SS, NLO, NSO, DTERM, maxpts,
     & linebuf, sampbuf, nptl, nptt)
      IF (npts.le.0 .and. nptl.le.0 .and. nptt.le.0) then
	IF (.NOT.NOGRID.AND.NPTS.NE.0) CALL XVCLOSE(OUNIT,IND,' ')
	go to 999
      endif
      IF (NOGRID .OR. NPTS.EQ.0) THEN
	CALL XVOPEN(OUNIT,ind,'OP','WRITE','U_FORMAT','WORD',
     &		'O_FORMAT',FMT,'U_NL',NLO,'U_NS',NSO,'U_NB',
     &		NBO,'OPEN_ACT','SA','IO_ACT','SA',' ')
      ELSE
	CALL XVCLOSE(OUNIT,IND,' ')
	CALL XVOPEN(OUNIT,IND,'OP','UPDATE','U_FORMAT','HALF',
     &       'O_FORMAT',FMT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      ENDIF
      IF (NPTL.GT.0) THEN
	CALL XVPARM( 'DNLIMB', LIMBDN, I, J, 1)
	junit = ounit
	if (nogrid .or. npts.eq.0) junit = iunit
	CALL DRAWGRID( JUNIT,OUNIT,LINEBUF,SAMPBUF,NPTL,SL,SS,SB,
     1		              NLO, NSO, NBO, LIMBDN, 0, BUF)
      ENDIF
      IF (NPTT.GT.0) THEN	! this means that DTERM=.true.
	if ((nogrid .or. npts.eq.0) .and. nptl.eq.0) then
	  junit = iunit
	else
	  junit = ounit
	  CALL XVCLOSE(OUNIT,IND,' ')
	  CALL XVOPEN(OUNIT,IND,'OP','UPDATE','U_FORMAT','HALF',
     &     'O_FORMAT',FMT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	endif
	CALL XVPARM( 'DNTERM', TERMDN, I, J, 1)
	CALL XVPARM( 'TSPACE', ITSP, I, J, 1)
	IF (ITSP.GT.1) THEN
	  NPTT1 = 0
	  DO I=1,NPTT,ITSP
	    NPTT1 = NPTT1+1
	    LINEBUF(NPTL+NPTT1) = LINEBUF(NPTL+I)
	    SAMPBUF(NPTL+NPTT1) = SAMPBUF(NPTL+I)
	  ENDDO
	  NPTT = NPTT1
	ENDIF
	CALL DRAWGRID( JUNIT,OUNIT,LINEBUF(NPTL+1),SAMPBUF(NPTL+1), 
     1		 NPTT, SL, SS, SB, NLO, NSO, NBO, TERMDN, 0, BUF)
      ENDIF

  500 CALL XVCLOSE(OUNIT,IND,' ')
      CALL XVMESSAGE(' OVERLAY task completed',' ')
      RETURN
C
C     ...Error conditions
  990 CALL XVMESSAGE(' ***Output image size is not specified',' ')
  999 CALL XVMESSAGE(' ***OVERLAY task cancelled',' ')
      CALL ABEND
      END

c *********************************************************************
      SUBROUTINE IPOPEN(iunit,fmt,maxdn,sl,ss,sb,nlo,nso,nbo,*)

C Open input image and get size and data format
C All arguments are outputs.
C
      CHARACTER*4 fmt
      CHARACTER*4 ORGIN
      INTEGER*4 SL,SS,SB,IPAR(4)

      CALL XVPCNT('INP',ni)
      IF (NI.EQ.0) THEN
         iunit = -1
         fmt = 'BYTE'
         maxdn = 255
         CALL XVP('SIZE',ipar,icnt)
         IF (ICNT.EQ.4) THEN
             SL = IPAR(1)
             SS = IPAR(2)
             NLO = IPAR(3)
             NSO = IPAR(4)
         ELSE
             sl = 1
             ss = 1
             CALL XVP('NL',nlo,icnt)
             CALL XVP('NS',nso,icnt)
         ENDIF
         nli = nlo
         nsi = nso
         RETURN
      ENDIF
C
C     ...Here if input image is specified
      CALL XVUNIT(iunit,'INP',1,ind,' ')
      CALL XVSIGNAL(IUNIT,ind,.TRUE.)
      CALL XVOPEN(IUNIT,ind,'U_FORMAT','HALF',
     &		'OPEN_ACT','SA','IO_ACT','SA',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,ind,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

C     ...Input and output size
      CALL XVSIZE(sl,ss,nlo,nso,nli,nsi)
      CALL XVBANDS(sb,nbo,nbi)
      IF (sl+nlo-1.GT.NLI) GOTO 990
      IF (ss+nso-1.GT.NSI) GOTO 990
      IF (sb.GT.NBI) GOTO 990

      IF ( sb + nbo - 1 .GT. nbi) THEN
         CALL XVMESSAGE('***Number of bands truncated', ' ') 
         nbo = nbi + 1 - sb
      ENDIF

      CALL XVGET(IUNIT,ind,'FORMAT',fmt,' ')

      IF (fmt.EQ.'BYTE') THEN
	CALL XVMESSAGE(' Input is in byte data format',' ')
	maxdn = 255
      ELSE IF (fmt.EQ.'HALF'.OR.fmt.EQ.'WORD') THEN
	CALL XVMESSAGE(' Input is in halfword data format',' ')
	maxdn = 32767
      ELSE
	CALL XVMESSAGE('*** Unsupported format, must be BYTE or HALF'
     +							,' ')
	RETURN1
      ENDIF

      CALL XVPARM('MAXDN',ival,icnt,idef,1)
      IF (IDEF.EQ.0) maxdn=IVAL
      RETURN

  990 CALL XVMESSAGE(' ***Invalid output size field',' ')
      RETURN1
      END

c *********************************************************************
	subroutine maplabel( iunit, planet, itype, scale, circum,
     1				 buf, ind)
	character*12 planet
	integer status
	real*4 buf(40)
	common/const/pi,dtr,rtd
	real*8 pi,dtr,rtd,mp

	ind = 0

	call mp_init( mp, status)
	if (status.ne.0) then
	  ind = -3
	  return
	endif

	call mp_label_read( mp, iunit, status)
	if (status.ne.0) then
	  ind = -2
	  return
	endif

	call mp_mpo2buf( mp, buf, status)
	if (status.ne.0) then
	  ind = -1
	  return
	endif

	call mve( 4, 1, buf(39), itype, 1, 1)

c  calculate approximate scale in picture in degrees/pixel
	req = buf(26)
	f = buf(7)               !Scale in km/pixel
	if (itype.eq.16) then
	  fl = buf(27)
	  pixpmm = buf(30)
	  scrange = buf(38)
	  scale = ((scrange-req)/(req*fl*pixpmm))*rtd
	else
	  scale = (f/req)*rtd
	endif
	call prnt(7,1,scale,' Scale(deg/pixel) = .')

c  and circumference, if needed:
	if (itype.eq.6.or.itype.eq.9.or.itype.eq.10) then
	  circum = 2.0*pi*req/f
	  call prnt(7,1,circum,' Circumference in pixels  = .')
	endif

c  try to get Planet keyword:
	call mp_get_value_str( mp, 'TARGET_BODY', planet, status)

	return
	end


c *********************************************************************
	SUBROUTINE NAV1( iunit, psun1, planet, ind)

c  attempt to compute subsolar lat/lon if not supplied by user

	IMPLICIT REAL*8 (A-H,O-Z)
	real*4 ssll(2), sunlat1, sunlat2, sunlon1, sunlon2

	REAL*8 buf8(100), psun1(3)
	INTEGER*4 ilab(80)
	character*7 project,gsproj,ckname
	character*12 planet
	character*30 msg
	character*8 cubtasks(5)/'NIMSCMM','NIMSCMM2','VISIS','VISIS2',
     1			 'VIMSCMM'/, cubtask
	COMMON/CONST/PI,DTR,RTD
c	LOGICAL XVPTST

c  determine the project
	project='NOPRO'
	call getproj( iunit, project, i, j, ind)
	if (ind.eq.0) then
	  write(msg,1000) project 
1000	  format('Project in label is: ', a5)
	  call xvmessage( msg, ' ')
	else

c  check if Cube label:
	  do i=1,5
	    cubtask = cubtasks(i)
	    call xlget(iunit,'HISTORY','B_SSLLAT',sunlat1,
     +              istatus,'FORMAT','REAL','HIST',cubtask,' ')
	    if (istatus.eq.1) go to 10
	  enddo
	  go to 11
10	  ind = 0
	  call xlget(iunit,'HISTORY','E_SSLLAT',sunlat2,
     +              istatus,'FORMAT','REAL','HIST',cubtask,' ')
	  if (istatus.ne.1) call mabend(' Error reading Cube label')
	  ssll(1) = 0.5*(sunlat1+sunlat2)
	  call xlget(iunit,'HISTORY','B_SSLLON',sunlon1,
     1			  istatus,'FORMAT','REAL','HIST',cubtask,' ')
	  if (istatus.ne.1) call mabend(' Error reading Cube label')
	  call xlget(iunit,'HISTORY','E_SSLLON',sunlon2,
     1			  istatus,'FORMAT','REAL','HIST',cubtask,' ')
	  if (istatus.ne.1) call mabend(' Error reading Cube label')
	  ssll(2) = 0.5*(sunlon1+sunlon2)
	  cosp = dcos(ssll(1)*dtr) 	! cos(lat)
	  sinp = dsin(ssll(1)*dtr) 	! sin(lat)
	  cosl = dcos(ssll(2)*dtr) 	! cos(lon)
	  sinl = dsin(ssll(2)*dtr) 	! sin(lon)
	  go to 20

11	  call xvmessage( 'No project in label, user spec used', ' ')
	  call xvparm( 'MISSION', project, icnt, idef, 1)
	  if (icnt.eq.0) then
	    call xvmessage('Unable to determine project!',' ')
	    ind = -1
	    return
	  endif
	  gsproj = project
	  if (project.eq.'GLL') gsproj = 'GLL  ' ! getspice is picky
	endif

c  SPICE/SEDR ckname (usually defaulted):
	call xvparm( 'CKNAME', ckname, icnt, idef, 1)

c  try to get project-specific label info:
	call getlabcon(iunit,project,ilab,ind)
	if (ind.gt.1) then
	  call prnt(4,1,ind,'GETLABCON: bad indicator=.')
	  return
	endif
	if (planet.eq.' ') call mvlc(ilab(25),planet,12)

c  get SPICE/SEDR info:
	call getspice2(iunit,.true.,buf8,ind)
c  for getspice2 success = 1
	if (ind.ne.1) then
	  call prnt(4,1,ind,'GETSPICE2: bad indicator=.')
	  return
	endif
c   reset ind =0
        ind = 0 

c   sub-solar point:
	cosp = dcos(buf8(28)*dtr)	! cos(lat)
	sinp = dsin(buf8(28)*dtr)	! sin(lat)
	cosl = dcos(buf8(29)*dtr)	! cos(lon)
	sinl = dsin(buf8(29)*dtr)	! sin(lon)
	ssll(1) = datan2(sinp,cosp)*rtd
	ssll(2) = datan2(sinl,cosl)*rtd
20	call prnt( 7, 2, ssll, 'Subsolar lat/long =.')

c   construct the unit Sun vector:
	psun1(1) = cosp*cosl
	psun1(2) = -cosp*sinl	! (West long.)
	psun1(3) = sinp

	RETURN
	END

c *********************************************************************
      SUBROUTINE GETGRID(DATA,CONV,SL,SS,NLO,NSO,SCALE,CIRCUM,MAXPTS,
     &	IPRNT,IZOOM,ITYPE,linebuf,sampbuf,number,npts,numnum,*)
C Compute latitude-longitude grid.
      REAL*4 DATA(40),CONV(2216),NUMBER(2,6*360)
      INTEGER*2 LINEBUF(MAXPTS),SAMPBUF(MAXPTS)
      INTEGER*4 SL,SS,EL,ES
      REAL*4 LATWRIT,LONWRIT

C     GLAT 	array of latitudes at which grid lines are to be drawn
C     GLON	array of longitudes at which grid lines are to be drawn
      REAL*4 GLAT(3,181),GLON(3,361)

      REAL*4 PAR(360)

      CALL XVP('DLATITUD',latinc,icnt)	!lati increment for numbers
      CALL XVP('DLONGITU',loninc,icnt)	!long increment for numbers

C     Calculate ending line and sample

      EL = SL + NLO - 1
      ES = SS + NSO - 1

C     Find maximum and minimum latitudes and longitudes of image

      CALL LIMITS(DATA,CONV,SCALE,SL,SS,EL,ES,
     &		rminlat,rmaxlat,rminlon,rmaxlon,circum)

      IPTR = 0

C     Draw lines of constant latitude

      CALL XVP('GLATITUD',par,numlat)
      DO J=1,NUMLAT
         GLAT(1,J) = PAR(J)
         GLAT(2,J) = RMINLON
         GLAT(3,J) = RMAXLON
         CALL FINDLAT(DATA,CONV,GLAT(1,J),RMINLON,RMAXLON,SL,SS,EL,ES,
     *      SCALE,CIRCUM,MAXPTS,IPRNT,linebuf,sampbuf,iptr,ITYPE,*999)
      ENDDO


C     Draw lines of constant longitude

      CALL XVP('GLONGITU',par,numlon)
      DO J=1,NUMLON
         GLON(1,J) = PAR(J)
         GLON(2,J) = RMINLAT
         GLON(3,J) = RMAXLON
         CALL FINDLON(DATA,CONV,GLON(1,J),RMINLAT,RMAXLAT,SL,SS,EL,ES,
     *     SCALE,CIRCUM,MAXPTS,IPRNT,linebuf,sampbuf,iptr,ITYPE,*999)
      ENDDO

      GLATMIN = 90.
      GLATMAX = -90.
      GLONMIN = 360.
      GLONMAX = 0.
      DO I = 1,NUMLON
	DO J = 1,NUMLAT
C	  Convert latitude/longitude values to line/sample values
	  call convevr(circum,ind,DATA,DATA,rline,rsamp,GLAT(1,J),
     $       GLON(1,I),1,CONV)
	  IF (IND.EQ.0 .AND. RLINE.GT.SL .AND. RSAMP.GT.SS .AND.
     $         RLINE.LT.EL .AND. RSAMP.LT.ES) THEN
	    GLATMIN=MIN(GLATMIN,GLAT(1,J))
	    GLATMAX=MAX(GLATMAX,GLAT(1,J))
	    GLONMIN=MIN(GLONMIN,GLON(1,I))
	    GLONMAX=MAX(GLONMAX,GLON(1,I))
	  ENDIF
	ENDDO
      ENDDO
c      write(*,*) ' glatmin,glatmax=',glatmin,glatmax
c      write(*,*) ' glonmin,glonmax=',glonmin,glonmax
      CALL FINDMID(GLONMIN,GLONMAX,GLON,NUMLON,lonwrit)
      CALL FINDMID(GLATMIN,GLATMAX,GLAT,NUMLAT,latwrit)
c      write(*,*) ' latwrit,lonwrit=',latwrit,lonwrit

      NUMNUM = 0
      DO I=1,NUMLAT,LATINC
	CALL ANNOTATE(GLAT(1,I),LONWRIT,number,numnum)
      ENDDO
      DO J=1,NUMLON,LONINC
	CALL ANNOTATE(LATWRIT,GLON(1,J)+7200.,number,numnum)
      ENDDO
      NPTS = IPTR

      IF (NPTS.EQ.0) CALL DEFGRID(DATA,CONV,SL,SS,NLO,NSO,SCALE,CIRCUM,
     &	MAXPTS,IPRNT,RMINLAT,RMAXLAT,RMINLON,RMAXLON,LATINC,LONINC,
     &	IZOOM,ITYPE,linebuf,sampbuf,number,npts,numnum,*999)
      IF (NPTS.EQ.0) GOTO 995
C
C     ...Sort the grid points by image line number
      CALL I2SORT(LINEBUF,SAMPBUF,NPTS)
      CALL PRNT(4,1,NPTS,' Number of points used = .')
      IF (IPRNT.NE.0) THEN
         DO I=1,NPTS
           CALL XPRINT(I,LINEBUF(I),SAMPBUF(I),-99.,-99.,22)
         ENDDO
      ENDIF
      RETURN

  995 CALL XVMESSAGE(' ***No grid points found',' ')
  999 RETURN1
      END

c *********************************************************************
      SUBROUTINE FINDMID(GMIN,GMAX,GL,NUML,lwrit)
C
C  Find a point exactly between two grid lines such that the
C    point is nearest the center of the grid.
C
      REAL*4 GL(3,*),LWRIT

      if (numl.le.0) return

      GMID = (GMAX + GMIN)/2.
      DMINUS = -1000.
      DPLUS = 1000.
      IMINUS = NUML
      IPLUS = 1
      DO I = 1,NUML
	DELTA = GL(1,I) - GMID
 	IF (DELTA.LT.0 .AND. DELTA.GT.DMINUS) THEN
	  DMINUS = DELTA
	  IMINUS = I
	ELSE IF (DELTA.GE.0 .AND. DELTA.LT.DPLUS) THEN
 	  DPLUS = DELTA
	  IPLUS = I
	ENDIF
      ENDDO
      LWRIT = (GL(1,IPLUS)+GL(1,IMINUS))/2.
      RETURN
      END

c *********************************************************************
      SUBROUTINE LIMITS(DATA,CONV,SCALE,SL,SS,EL,ES,
     &		rminlat,rmaxlat,rminlon,rmaxlon,circum)
C
C Find bounding lat-lons for input image.
C Inputs: DATA,NLI,NSI,IPRNT
C Outputs: rminlat,rminlon,rmaxlat,rmaxlon
C
      INTEGER*4 DATA(40),CONV(2216),SL,SS,EL,ES

      REAL*4 PAR(4)
      INTEGER*4 S10
      CHARACTER*50 PTSMSG
      LOGICAL XVPTST

      CALL XVPARM('LIMITS',par,icnt,idef,4)
      RMINLAT = PAR(1)
      RMAXLAT = PAR(2)
      RMINLON = PAR(3)
      RMAXLON = PAR(4)
      IF (IDEF.EQ.0) GOTO 100	!Skip if user has specified limits.

      NL = EL - SL + 1
      NS = ES - SS + 1
      IF (.NOT.XVPTST('AUTO').AND.SCALE*NS.GE.90.) RETURN
C
C     ...Here if AUTO option is used.
C     ...User should use this option only for near-encounter pictures
      CALL PRNT(7,1,SCALE*NS,' AUTO Option enabled, degrees extent= .')
      IF (XVPTST('PRINT')) THEN
	IPRNT = 1
	CALL XVMESSAGE(' #     LINE   SAMPLE  LATITUDE LONGITUDE',' ')
      ELSE
          IPRNT = 0
      ENDIF

      RMINLAT = 999.
      RMINLON = 999.
      RMAXLAT = -999.
      RMAXLON = -999.
C     ....Check for visible poles.
      call convevr(circum,ind,DATA,DATA,rline,rsamp,90.D0,0.D0,1,CONV)
      IF (IND.EQ.0.AND.
     +	  RLINE.GT.1. .AND.RSAMP.GT.1. .AND.
     +    RLINE.LT.EL .AND.RSAMP.LT.ES) THEN
           RMAXLAT = 90.D0
           RMINLON = 0.D0
           RMAXLON = 360.D0
      ENDIF
      call convevr(circum,ind,DATA,DATA,rline,rsamp,-90.D0,0.D0,1,CONV)
      IF (IND.EQ.0.AND.
     +	  RLINE.GT.1. .AND.RSAMP.GT.1. .AND.
     +    RLINE.LT.EL .AND.RSAMP.LT.ES) THEN
           RMINLAT = -90.D0
           RMINLON = 0.D0
           RMAXLON = 360.D0
      ENDIF


      L10 = (NL+9)/10
      S10 = (NS+9)/10

      iptr = 0
      DO 10 I=0,10
        DO 10 J=0,10
	  iptr = iptr+1
          RLINE = MIN(I*L10+SL,EL)
          RSAMP = MIN(J*S10+SS,ES)
          RLAT = -999.
          RLON = -999.
          call convevr(circum,ind,DATA,DATA,RLINE,RSAMP,rlat,rlon,
     1			  2,CONV)
          IF (IND.NE.0) GOTO 10
          RMINLAT = MIN(RMINLAT,RLAT)
          RMINLON = MIN(RMINLON,RLON)
          RMAXLAT = MAX(RMAXLAT,RLAT)
          RMAXLON = MAX(RMAXLON,RLON)
          IF (IPRNT.EQ.0) GOTO 10
	  WRITE(PTSMSG,1000) IPTR, rLINE, rSAMP, rLAT, rLON
1000	  FORMAT(1X,I3,2(1X,F7.2),2(3X,F7.3))
          CALL XVMESSAGE(PTSMSG(1:40),' ')
   10 CONTINUE
C     ....Add margin on account of coarseness of grid
      DELTA = MAX0(L10,S10)*SCALE
      RMINLAT = MAX(RMINLAT-DELTA,-90.)
      RMINLON = MAX(RMINLON-DELTA,0.)
      RMAXLAT = MIN(RMAXLAT+DELTA,90.)
      RMAXLON = MIN(RMAXLON+DELTA,360.)

c  if min/max long differ by >180 degrees, then it's likely that
c  they wrap around the 0 meridian ... to avoid problems, just assume
c  that the entire range is present:
      if ((rmaxlon-rminlon).gt.180.) then
	rmaxlon = 360.
	rminlon = 0.
      endif

  100 RETURN
      END

c *********************************************************************
      SUBROUTINE FINDLAT(DATA,CONV,GLAT,RMINLON,RMAXLON,SL,SS,EL,ES,
     &	   SCALE,CIRCUM,MAXPTS,IPRNT,linebuf,sampbuf,iptr,ITYPE,*)

C Find all the points in the picture along a constant latitude.
C     GLAT is the input latitude
C
      REAL*4 DATA(40),CONV(2216),GLAT(3,1)
      INTEGER*2 LINEBUF(MAXPTS),SAMPBUF(MAXPTS)
      INTEGER*4 SL,SS,EL,ES,ITYPE,counter

      INTEGER SAMP,SAMP0
      REAL*4 LONLO,LONHI

      N = 10
      IFLAG = 0
      RLAT = GLAT(1,1)			!latitude of grid line
      LONLO = MAX(GLAT(2,1),RMINLON)	!lower limit of longitude
      LONHI = MIN(GLAT(3,1),RMAXLON)	!upper limit of longitude
      IF (IPRNT.NE.0) THEN
	PRINT *,' AT LAT = ',RLAT,' LONG = ',LONLO,LONHI
	CALL XVMESSAGE
     1 ('     PT#   LINE   SAMP   LAT    LONG  PUT IN LATI. LINES',
     1		 ' ')
      ENDIF
      RLON = LONLO
      ASCALE = SCALE
      RLINE0 = -999.
      counter=0

   10 IF (RLON.GT.LONHI) RETURN
      IF (IPTR.EQ.4550) GOTO 18
      GOTO 19
   18 N = 10

C     ...Convert latitude,longitude to line,samp for point
   19 call convevr(circum,ind,DATA,DATA,rline,rsamp,RLAT,RLON,1,CONV)
      IF (IND.NE.0) GO TO 20
      IF (RLINE.LT.-1.0e+09.OR.RLINE.GT.1.0e+09) GOTO 20
      IF (RSAMP.LT.-1.0e+09.OR.RSAMP.GT.1.0e+09) GOTO 20
      LINE = RLINE + .5
      SAMP = RSAMP + .5
      IF (LINE.LT.SL.OR.LINE.GT.EL) GO TO 20
      IF (SAMP.LT.SS.OR.SAMP.GT.ES) GO TO 20
      GO TO 21
   20 RLON = RLON + SCALE*N	!Skip N points and try again
      RLINE0 = -999.		!at new longitude in range.
      IFLAG = 1
      GOTO 10

C     ...Here if we have found a point in the picture
   21 counter=counter+1
      IF (IFLAG.EQ.1) THEN		!If we have been skipping,
          RLON = RLON - SCALE*(N-1)	!back up a bit
          IFLAG = 0
          GOTO 10
      ENDIF

C     ...Distance constraint applied for next point found on grid line
      IF (RLINE0.NE.-999.) THEN
         D = (RLINE-RLINE0)**2 + (RSAMP-RSAMP0)**2
         IF (D.GT.1.0) THEN
C
C	    If projection type is Lambert Conformal Conic (ITYPE,5) or
C           sinusoidal (ITYPE,12) and the reference longitude is more
C	    than 180.0 degrees less than the center meridian, then override
C           the distance constraint of D and accept CONVEV RLINE and RSAMP 
C           at the opposite side of the projected image.  This occurs at
C           latitudes where the projected image does not fill the image file
C           completely.   (change date - March 27, 1992 - JFM059 - FR66586)
C
            if(counter.gt.10) goto 25   !are in a loop
	    IF (((ITYPE.EQ.5).OR.(ITYPE.EQ.12))
     &		.AND.(RLON.GT.(DATA(6)-180.0))) GOTO 25
            RLON = RLON - 0.3*ASCALE
            ASCALE = 0.7*ASCALE
            GOTO 10
         ELSE IF (D.LT.0.5) THEN
            RLON = RLON + 0.3*ASCALE
            ASCALE = 1.3*ASCALE
            GOTO 10
         ENDIF
      ENDIF

 25   RLINE0 = RLINE
      RSAMP0 = RSAMP
      counter=0

C     ...If new point is not the same as the previous point and
C     ...the maximum number of points of the buffer is not exceeded,
C     ...load new line,samp of point into array of grid line points.
      IF (LINE0.NE.LINE.OR.SAMP0.NE.SAMP) THEN
          IF (IPTR.GE.MAXPTS) GOTO 990
          IPTR = IPTR + 1
          LINEBUF(IPTR) = LINE
          SAMPBUF(IPTR) = SAMP
          IF (IPRNT.NE.0) 
     &       CALL XPRINT(IPTR,LINEBUF(IPTR),SAMPBUF(IPTR),GLAT,RLON,40)
          LINE0 = LINE
          SAMP0 = SAMP
      ENDIF

      RLON = RLON + ASCALE
      GOTO 10

  990 CALL XVMESSAGE(' ***Maximum number of grid points exceeded',' ')
      CALL XVMESSAGE(' ***Increase spacing between grid lines',' ')
      RETURN1
      END

c *********************************************************************
      SUBROUTINE FINDLON(DATA,CONV,GLON,RMINLAT,RMAXLAT,SL,SS,EL,ES,
     *		SCALE,CIRCUM,MAXPTS,IPRNT,linebuf,sampbuf,iptr,ITYPE,*)

C Find all points in the picture along a constant longitude and
C between specified latitude limits.
C
      REAL*4 DATA(40),CONV(2216),GLON(3,361)
      INTEGER*2 LINEBUF(MAXPTS),SAMPBUF(MAXPTS)
      INTEGER*4 SL,SS,EL,ES,ITYPE,counter
      REAL*4 LATLO,LATHI
      INTEGER SAMP,SAMP0

      N = 10
      IFLAG = 0
      RLON = GLON(1,1)			!longitude for grid line
      LATLO = MAX(GLON(2,1),RMINLAT)	!lower latitude limit
      LATHI = MIN(GLON(3,1),RMAXLAT)	!upper latitude limit
      RLAT = LATLO
      IF (IPRNT.NE.0) THEN
	PRINT *,' AT LON = ',RLON,' LAT = ',LATLO,LATHI
	CALL XVMESSAGE
     1 ('     PT#   LINE   SAMP   LAT    LONG  PUT IN LONG. LINES',' ')
      ENDIF

      ASCALE = SCALE
      RLINE0 = -999.

      counter = 0
   10 IF (RLAT.GE.LATHI) RETURN
      call convevr(circum,ind,DATA,DATA,rline,rsamp,RLAT,RLON,1,CONV)
      IF (IND.NE.0) GO TO 20
      IF (RLINE.LT.-1.0e+09.OR.RLINE.GT.1.0e+09) GOTO 20
      IF (RSAMP.LT.-1.0e+09.OR.RSAMP.GT.1.0e+09) GOTO 20
      LINE = RLINE + .5
      SAMP = RSAMP + .5
      IF (LINE.LT.SL.OR.LINE.GT.EL) GO TO 20
      IF (SAMP.LT.SS.OR.SAMP.GT.ES) GO TO 20
      GO TO 21
   20 RLAT = RLAT + SCALE*N	!Skip N points and try again
      RLINE0 = -999.		!at new longitude in range.
      IFLAG = 1
      GOTO 10

C     ....Here if we found a point in the picture
   21 counter = counter+1
      IF (IFLAG.EQ.1) THEN
         RLAT = RLAT-SCALE*(N-1)	!If we have been skipping,
         IFLAG = 0			!back up a bit.
         GOTO 10
      ENDIF

      IF (RLINE0.NE.-999.) THEN
	if (counter.gt.10) goto 25   !are in a loop
        D = (RLINE-RLINE0)**2 + (RSAMP-RSAMP0)**2
        IF (D.GT.1.0) THEN
          RLAT = RLAT - 0.3*ASCALE
          ASCALE = 0.7*ASCALE
          GOTO 10
        ELSE IF (D.LT.0.5) THEN
          RLAT = RLAT + 0.3*ASCALE
          ASCALE = 1.3*ASCALE
          GOTO 10
        ENDIF
      ENDIF

   25 RLINE0 = RLINE
      RSAMP0 = RSAMP
      counter = 0

      IF (LINE0.NE.LINE.OR.SAMP0.NE.SAMP) THEN
         IF (IPTR.GE.MAXPTS) GOTO 990
         IPTR = IPTR+1
         LINEBUF(IPTR) = LINE
         SAMPBUF(IPTR) = SAMP
         IF(IPRNT.NE.0) CALL XPRINT(IPTR,LINEBUF(IPTR),SAMPBUF(IPTR),
     +		RLAT,RLON,40)
         LINE0 = LINE
         SAMP0 = SAMP
      ENDIF

      RLAT = RLAT + ASCALE
      GOTO 10

  990 CALL XVMESSAGE(' ***Maximum number of grid points exceeded',' ')
      CALL XVMESSAGE(' ***Increase grid spacing',' ')
      RETURN1
      END

c *********************************************************************
      SUBROUTINE DEFGRID(DATA,CONV,SL,SS,NLO,NSO,SCALE,CIRCUM,MAXPTS,
     &		IPRNT,RMINLAT,RMAXLAT,RMINLON,RMAXLON,LATINC,LONINC,
     &		IZOOM,ITYPE,linebuf,sampbuf,number,npts,numnum,*)
C Compute default latitude-longitude grid.
      REAL*4 DATA(40),CONV(2216),NUMBER(2,6*360),LAT,LON
      INTEGER*2 LINEBUF(MAXPTS),SAMPBUF(MAXPTS)
      INTEGER*4 SL,SS,EL,ES
      REAL*4 GLAT(3,543),GLON(3,1083),LATWRIT(3),LONWRIT(3)

      CALL XVP('DLA1',dla1,icnt)	!latitude grid spacing
      CALL XVP('DLA2',dla2,icnt)	!latitude grid spacing near pole
      CALL XVP('DLO1',dlo1,icnt)	!longitude grid spacing
      CALL XVP('DLO2',dlo2,icnt)	!longitude grid spacing near pole

      numlon=0	! initialize
      numlat=0

      CALL XVPARM('SUPPRESS',supp,icnt,idef,1)!polar latitude for spacing change
      IF (IDEF.EQ.1) THEN   !Adjust SUPP to avoid extra latitude line near pole
         DO R=0.,90.,DLA1
            IF (R+DLA1.GT.SUPP) GOTO 5
         ENDDO
    5    IF (DLA1.LE.DLA2) SUPP=R
      ENDIF

c  ensure that the longitude lines are at "nice" numbers -- should
c  really check that they match the numbers written in ANNOTATE, but
c  we'll leave that for TBD
      rminlon1 = dlo1*float(int(rminlon/dlo1)) 
      if (rminlon1.lt.rminlon) rminlon1 = rminlon1 + dlo1
      rmaxlon1 = dlo1*float(int(rmaxlon/dlo1)) 
      if (rmaxlon1.gt.rmaxlon) rmaxlon1 = rmaxlon1 - dlo1
      rminlon2 = dlo2*float(int(rminlon/dlo2)) 
      if (rminlon2.lt.rminlon) rminlon2 = rminlon2 + dlo2
      rmaxlon2 = dlo2*float(int(rmaxlon/dlo2)) 
      if (rmaxlon2.gt.rmaxlon) rmaxlon2 = rmaxlon2 - dlo2

C  south polar area
      rval=89.99
      CALL LATGRID(SUPP+DLA2,rval,DLA2,-1,RMINLAT,RMAXLAT,numlat,
     $  glat,RMINLON,RMAXLON)
      rval=-90.0
      CALL LONGRID(DLO2,RMINLON2,RMAXLON2,numlon,glon,rval,-SUPP)
C  equatorial area
      NSTEP = INT(SUPP/DLA1)
      CALL LATGRID(-DLA1*NSTEP,DLA1*NSTEP,DLA1,1,RMINLAT,RMAXLAT,
     $  numlat,glat,RMINLON,RMAXLON)
      CALL LONGRID(DLO1,RMINLON1,RMAXLON1,numlon,glon,-SUPP,SUPP)
C  north polar area
      rval=89.99
      CALL LATGRID(SUPP,rval,DLA2,1,RMINLAT,RMAXLAT,numlat,glat,
     $  RMINLON,RMAXLON)
      rval=90.0
      CALL LONGRID(DLO2,RMINLON2,RMAXLON2,numlon,glon,SUPP,rval)

      IF (IPRNT.GT.0) THEN
          CALL XVMESSAGE(' LATITUDE:LIMITS-LONGITUDE    LONGITUDE',' ')
          DO I=1,NUMLAT
               CALL PRNT(7,3,GLAT(1,I),' LAT.')
          ENDDO
          CALL XVMESSAGE(' LONGITUDE:LIMITS-LATITUDE    LATITUDE',' ')
          DO I=1,NUMLON
               CALL PRNT(7,3,GLON(1,I),' LON.')
          ENDDO
      ENDIF

      EL = SL + NLO - 1
      ES = SS + NSO - 1
      IPTR = 0
      DO I=1,NUMLAT
          CALL FINDLAT(DATA,CONV,GLAT(1,I),RMINLON,RMAXLON,SL,SS,EL,ES,
     *       SCALE,CIRCUM,MAXPTS,IPRNT,linebuf,sampbuf,iptr,ITYPE,*999)
      ENDDO
      DO I=1,NUMLON
          CALL FINDLON(DATA,CONV,GLON(1,I),RMINLAT,RMAXLAT,SL,SS,EL,ES,
     *       SCALE,CIRCUM,MAXPTS,IPRNT,linebuf,sampbuf,iptr,ITYPE,*999)
      ENDDO
      NPTS = IPTR			!Number of grid points

C     ...Poleward (NORTH) finding of lats/lons of annotation
      CALL PREPSUB(DATA,CONV,EL,ES,RMINLAT,RMAXLAT,RMINLON2,
     $   RMAXLON2,SUPP,90.,DLA2,DLO2,1,IZOOM,latwrit,lonwrit,
     &   circum)
      NSTEP = INT(SUPP/DLA1)

C     ...Equatorward finding of lats/lons of annotation
      CALL PREPSUB(DATA,CONV,EL,ES,RMINLAT,RMAXLAT,RMINLON1,
     $   RMAXLON1,-DLA1*NSTEP,DLA1*NSTEP,DLA1,DLO1,2,
     $   IZOOM,latwrit,lonwrit,circum)

C     ...Poleward (SOUTH) finding of lats/lons of annotation
      CALL PREPSUB(DATA,CONV,EL,ES,RMINLAT,RMAXLAT,RMINLON2,
     $   RMAXLON2,-SUPP,-90.,-DLA2,DLO2,3,IZOOM,latwrit,lonwrit,
     &   circum)

c	write(*,*) ' latwrit,lonwrit=',latwrit,lonwrit
      NUMNUM = 0   ! numnum = number of annotation numbers to add.
      DO LON=RMINLON2,RMAXLON2,DLO2*LONINC      ! do polar areas first...
        IF (LON.GE.RMINLON.AND.LON.LE.RMAXLON) THEN
	  IF (LATWRIT(1).GT.-500.) CALL ANNOTATE(LATWRIT(1),
     $       LON+7200.,number,numnum)
	  IF (LATWRIT(3).GT.-500.) CALL ANNOTATE(LATWRIT(3),
     $       LON+7200.,number,numnum)
	ENDIF
      ENDDO

      DO LAT=SUPP+DLA2*LATINC,89.99,DLA2*LATINC
        IF (LAT.GE.RMINLAT .AND. LAT.LE.RMAXLAT .AND.
     $       LONWRIT(1).GT.-500.) THEN
	  CALL ANNOTATE(LAT,LONWRIT(1),number,numnum) !write latitude numbers
        ENDIF
	R = -LAT
	IF (R.GE.RMINLAT .AND. R.LE.RMAXLAT .AND.
     $      LONWRIT(3).GT.-500.) THEN
	  CALL ANNOTATE(LAT,LONWRIT(3),number,numnum)
	ENDIF
      ENDDO

      DO LON=RMINLON1,RMAXLON1,DLO1*LONINC
        IF (LON.GE.RMINLON .AND. LON.LE.RMAXLON .AND.
     $      LATWRIT(2).GT.-500.) THEN
	  CALL ANNOTATE(LATWRIT(2),LON+7200.,number,numnum)
 	ENDIF
      ENDDO

      IF (LONWRIT(2).GT.-500.) THEN
       DO J = 0,NSTEP,LATINC
	LAT = DLA1*J
        IF (LAT.GE.RMINLAT .AND. LAT.LE.RMAXLAT) THEN
	  CALL ANNOTATE(LAT,LONWRIT(2),number,numnum) !write lat. numbers in
        ENDIF
       ENDDO
       DO J = LATINC,NSTEP,LATINC
	LAT = -DLA1*J
        IF (LAT.GE.RMINLAT .AND. LAT.LE.RMAXLAT) THEN
	  CALL ANNOTATE(LAT,LONWRIT(2),number,numnum) !write lat. numbers in
        ENDIF
       ENDDO
      ENDIF

      RETURN
  999 RETURN1
      END

c *********************************************************************
      SUBROUTINE PREPSUB(DATA,CONV,EL,ES,RMINLAT,RMAXLAT,RMINLON,
     $  RMAXLON,DL1,DL2,LATINT,LONINT,INDEX,IZOOM,latwrit,lonwrit,
     &  circum)
C
C PURPOSE:  Given the grid information, find the latitudes
C    and longitudes at which to write the annotation numbers.
C    The north polar, equatorial, and south polar areas
C    are treated as completely separate grids.
C INPUTS: DATA,CONV  (integer arrays)
C         EL,ES (integer parameters)
C         RMINLAT,RMAXLAT,RMINLON,RMAXLON,
C                SUPP,DLA1,DLA2,DLO1,DLO2 (real parameters)
C OUTPUTS: latwrit,lonwrit  (3-element real arrays)
C BUGS:  What should this routine do if the grid lines are all
C     user-specified?  (i.e. GLAT and GLON are parameters but DLA1,
C     DLA2,DLO1,DLO2, and SUPP are not?)
C
      REAL*4 DATA(40),CONV(2216)
      INTEGER*4 EL,ES
      REAL*4 LATWRIT(3),LONWRIT(3)

      REAL*4 LATINT,LONINT,LATGRIDINT,LONGRIDINT
      REAL*4 LATMIN,LATMAX,LONMIN,LONMAX,LONMAXTEMP
      REAL*4 LATSTART,LATEND,MAXLATSPACE,MINLATSPACE,LATSPACE

      LATMIN  = 999.
      LONMIN  = 999.
      LATMAX  = -999.
      LONMAX  = -999.
C     ....Check for visible pole.
      ISIGN = 2 - INDEX
      IF (ABS(ISIGN).EQ.1) THEN
        call convevr(circum,ind,DATA,DATA,rline,rsamp,ISIGN*90.D0,0.D0,
     &	 1,CONV)
        IF (IND.EQ.0 .AND. (isign*90..eq.rminlat .or.
     +    isign*90..eq.rmaxlat) .AND.
     +	  RLINE.GT.1. .AND. RSAMP.GT.1. .AND.
     +    RLINE.LT.EL .AND. RSAMP.LT.ES) THEN
           LATMAX  = ISIGN*90.D0
           LONMIN  = 0.D0
           LONMAX  = 360.D0
        ENDIF
      ENDIF

C     ...Find minimum and maximum longitudes of image
      LONGRIDINT = 1.
      DO DLAT = DL1,DL2,LATINT		!Poleward latitudes
	IF (ABS(DLAT).gt.89.99) THEN
	  DLAT1=SIGN(89.99,DLAT)
	ELSE
	  DLAT1=DLAT
	ENDIF
        DO DLON=RMINLON,RMAXLON,LONGRIDINT
	  call convevr(circum,ind,DATA,DATA,rline,rsamp,DLAT1,DLON,
     &	   1,CONV)
	  IF ((IND.EQ.0).AND.(RLINE.GT.1.).AND.(RSAMP.GT.1.).AND.
     $     (RLINE.LT.EL).AND.(RSAMP.LT.ES)) THEN
	      LONMIN=MIN(LONMIN,DLON)
	      LONMAX=MAX(LONMAX,DLON)
	  ENDIF
	ENDDO
      ENDDO

c  if min/max long differ by >180 degrees, then it's likely that
c  they wrap around the 0 meridian ... to avoid problems, just assume
c  that the entire range is present:
      if ((lonmax-lonmin).gt.180.) then
	lonmax = 360.
	lonmin = 0.
      endif

C     ...Find minimum and maximum latitudes of image
      LATGRIDINT = 1.
      MAXLATSPACE = -1000.
      MINLATSPACE = 1000.
      DO DLON = LONMIN,LONMAX,LONINT
        LATSTART = -100
	LATEND = -100
	DO DLAT = DL2,DL1,-LATGRIDINT
	  IF (ABS(DLAT).gt.89.99) THEN
	    DLAT1=SIGN(89.99,DLAT)
	  ELSE
	    DLAT1=DLAT
	  ENDIF
	  IF (DLAT1.GE.RMINLAT.AND.DLAT1.LE.RMAXLAT) THEN
	    call convevr(circum,ind,DATA,DATA,rline,rsamp,DLAT1,DLON,
     &	     1,CONV)
	    IF (IND.EQ.0 .AND. RLINE.GT.1. .AND. RSAMP.GT.1. .AND.
     $         RLINE.LT.EL .AND. RSAMP.LT.ES) THEN
	      IF (LATSTART.LT.-91) LATSTART=DLAT1
	      LATEND=DLAT1
	      LATMIN=MIN(LATMIN,DLAT1)
	      LATMAX=MAX(LATMAX,DLAT1)
	    ENDIF
	  ENDIF
	ENDDO
	LATSPACE = ABS(LATEND-LATSTART)
	IF (LATSPACE.GT.MAXLATSPACE) THEN
	  MAXLATSPACE = LATSPACE
	  LONMAXTEMP = DLON
	ELSE IF (LATSPACE.LT.MINLATSPACE) THEN
	  MINLATSPACE = LATSPACE
	ENDIF
      ENDDO
      I = INDEX
      IF (MAXLATSPACE.EQ.0 .OR. MAXLATSPACE.EQ.-1000.) THEN
	LONWRIT(I) = -999.
	LATWRIT(I) = -999.
	RETURN
      ENDIF
      IF ((ABS(MAXLATSPACE-MINLATSPACE)/MAXLATSPACE).GT.0.1) THEN
	LONWRIT(I) = LONMAXTEMP
      ELSE
	LONWRIT(I) = (LONMAX+LONMIN)/2.0
      ENDIF
      LONWRIT(I) = LONINT*FLOAT(INT(LONWRIT(I)/LONINT)) + LONINT/2.0
      LATWRIT(I) = (LATMAX+LATMIN)/2.0
      LATWRIT(I) = LATINT*FLOAT(INT(LATWRIT(I)/LATINT)) + LATINT/2.0
    
C     ...Ensure latitude and longitude annotation is printed within image
C        boundaries. (FR 66549)
      DO DLON=LONMIN,LONMAX,LONINT
	call convevr(circum,ind,DATA,DATA,rline,rsamp,LATWRIT(I),DLON,
     &	 1,CONV)
	iter = 1 
	DO WHILE (IND.EQ.0 .AND.( RLINE.GT.EL .OR. RSAMP.GT.ES .OR.
     $   RLINE.LT.1.0 .OR. RSAMP.LT.1.0))
	  IF ( RLINE.LT.1.0 ) RLINE = RLINE + (40*IZOOM)
	  IF ( RSAMP.LT.1.0 ) RSAMP = RSAMP + (40*5*IZOOM)
	  IF ( RLINE.GT.EL ) RLINE = RLINE - (30*IZOOM)
	  IF ( RSAMP.GT.ES ) RSAMP = RSAMP - (30*5*IZOOM)
	  call convevr(circum,ind,DATA,DATA,RLINE,RSAMP,latwrit(I),
     &	   dummy,2,CONV)
	  iter = iter+1
	  if (iter.gt.4) go to 10
	ENDDO
10    ENDDO
      if (IND.ne.0 .or.( RLINE.GT.EL .OR. RSAMP.GT.ES .OR.
     $   RLINE.LT.1.0 .OR. RSAMP.LT.1.0)) then
	call xvmessage(' PREPSUB unable to find lat/long for annotation',
     1				 ' ')
      endif
      RETURN
      END


c *********************************************************************
	SUBROUTINE ANNOTATE(LAT,LON,number,numnum)
C Add another pair (lat,lon) to the annotation array NUMBER.
 	REAL*4 LAT,LON,NUMBER(2,6*360)
	NUMNUM=NUMNUM+1
	NUMBER(1,NUMNUM)=LAT
	NUMBER(2,NUMNUM)=LON
c	write(*,*) ' ANNOTATE: lat,lon=',lat,amod(lon,360.)
	RETURN
	END

c *********************************************************************
	SUBROUTINE ANNOFIND(LMIN,LMAX,LSTP,ISGN,RMIN,RMAX,g1,g2)
C Find the closest values in the sequence {lmin to lmax step lstp}
C    to the numbers rmin and rmax.
	REAL*4 LMIN,LMAX,LSTP,L
	G1 = -1000.
	DO R = LMIN,LMAX,LSTP
	  L=ISGN*R
	  IF (L.GE.RMIN .AND. L.LE.RMAX) THEN
	    IF (G1.LT.-999.) G1=L
	    G2=L
	  ENDIF
	ENDDO
	RETURN
        END

c *********************************************************************
	SUBROUTINE LATGRID(LMIN,LMAX,LSTP,ISGN,RMINLAT,RMAXLAT,NUMLAT,
     $      GLAT,ENDPT1,ENDPT2)
C Add some grid lines of constant latitude between longitudes endpt1
C   and endpt2.
	REAL*4 LMIN,LMAX,LSTP,GLAT(3,543)
	DO S = LMIN,LMAX,LSTP
	  R = S*ISGN
	  IF (R.GE.RMINLAT .AND. R.LE.RMAXLAT) THEN
	    NUMLAT = NUMLAT + 1
	    GLAT(1,NUMLAT) = R
	    GLAT(2,NUMLAT) = ENDPT1
	    GLAT(3,NUMLAT) = ENDPT2
	  ENDIF
	ENDDO
	RETURN
        END

c *********************************************************************
	SUBROUTINE LONGRID(LSTP,RMINLON,RMAXLON,NUMLON,GLON,
     +						ENDPT1,ENDPT2)
C Add some grid lines of constant longitude between latitudes endpt1
C   and endpt2.
	REAL*4 LSTP,GLON(3,1083)
	DO R = RMINLON,RMAXLON,LSTP
	  IF (R.GE.RMINLON .AND. R.LE.RMAXLON) THEN
	    NUMLON = NUMLON + 1
	    GLON(1,NUMLON) = R
	    GLON(2,NUMLON) = ENDPT1
	    GLON(3,NUMLON) = ENDPT2
	  ENDIF
	ENDDO
	RETURN
	END

c *********************************************************************
      SUBROUTINE CONVEVR(CIRCUM,IND,DATA,IDATA,RLINE,RSAMP,RLAT,RLON,
     & MODE,CONV)

C call CONVEV and fix the sample for rectangular projections
c
C The sample calulated by CONVEV can be off by the circumference
C of the planet.
C     CIRCUM = number of pixels in circumference of image (input)
C
      common/lattyp/lat_typ,rep2
      INTEGER IDATA(39)

      rlat1 = rlat
      if (lat_typ.eq.0 .and. mode.eq.1 .and. abs(rlat).lt.89.9999) 
     & rlat1=atan(tan(rlat)/rep2)

      CALL CONVEV(IND,DATA,DATA,RLINE,RSAMP,RLAT1,RLON,MODE,CONV)

      if (mode.eq.2) then
	rlat = rlat1
	if (lat_typ.eq.0 .and. abs(rlat).lt.89.9999) 
     &   rlat=atan(tan(rlat)*rep2)
      endif

      IF (IDATA(39).NE.6.AND.IDATA(39).NE.9.AND.IDATA(39).NE.10)
     & RETURN
      IF (RSAMP.LT.0.) THEN
          DO WHILE(RSAMP.LT.0.)
             RSAMP=RSAMP+CIRCUM
          ENDDO
      ELSE IF (RSAMP.GT.CIRCUM) THEN
          DO WHILE(RSAMP.GT.CIRCUM)
             RSAMP=RSAMP-CIRCUM
          ENDDO
      ENDIF
      RETURN
      END

c *********************************************************************
      SUBROUTINE DRAWGRID(IUNIT,OUNIT,LINEBUF,SAMPBUF,NPTS,
     &		SL,SS,SB,NLO,NSO,NBO,DNGRID,ALTERNATE,BUF)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 LINEBUF(NPTS),SAMPBUF(NPTS),BUF(10000)
      INTEGER*4 DNGRID
      LOGICAL ALTERNATE
      INTEGER*4 BANDOUT,LINEOUT,BAND
C
      CALL ZIA(BUF,(NSO+1)/2)

      BANDOUT = 0
      DO 110 BAND=SB,SB+NBO-1
        BANDOUT = BANDOUT + 1

C       ...Copy input picture to output until we get to grid points
        IF (IUNIT.NE.OUNIT) THEN
          LINEOUT = 0
	  DO LINE=SL,LINEBUF(1)-1
            LINEOUT = LINEOUT + 1
	    IF (IUNIT.GE.0) CALL XVREAD(IUNIT,BUF,IND,'LINE',LINE,
     1			    'SAMP',SS,'NSAMPS',NSO,'BAND',BAND,' ')
	    CALL XVWRIT(OUNIT,BUF,IND,'LINE',LINEOUT,'BAND',BANDOUT,' ')
          ENDDO
        ENDIF

        J = 1
C
C       ...Now do the section with grid points
        IF (IUNIT.EQ.OUNIT) LINEOUT = LINEBUF(1)-1 
        DO LINE=LINEBUF(1),LINEBUF(NPTS)
          LINEOUT = LINEOUT + 1
          IF (IUNIT.GE.0) THEN
            CALL XVREAD(IUNIT,BUF,IND,'LINE',LINE,'SAMP',SS,'NSAMPS',
     1			    NSO, 'BAND', BAND, ' ')
          ELSE
	    CALL ZIA(BUF,(NSO+1)/2)
          ENDIF
          J0 = J
      
          DO J=J0,NPTS
            IF (LINEBUF(J).GT.LINE) GOTO 100
            ISAMP = SAMPBUF(J) - SS + 1
            IF (ALTERNATE) THEN
              IMOD = MOD(LINE+ISAMP,2)
              IF (IMOD.EQ.0) THEN
                BUF(ISAMP)=0
              ELSE
                BUF(ISAMP)=DNGRID
              ENDIF
            ELSE
              BUF(ISAMP) = DNGRID
            ENDIF
          ENDDO

100       CALL XVWRIT(OUNIT,BUF,IND,'LINE',LINEOUT,'BAND',BANDOUT,' ')
        ENDDO 
C
        IF (IUNIT.EQ.OUNIT) GO TO 110

C       ...Write out rest of picture that has no points
        EL = SL + NLO - 1
        CALL ZIA(BUF,(NSO+1)/2)
        DO LINE=LINEBUF(NPTS)+1,EL
          LINEOUT = LINEOUT + 1
	  IF (IUNIT.GE.0) CALL XVREAD(IUNIT,BUF,IND,'LINE',LINE,
     1			'SAMP',SS, 'NSAMPS',NSO,'BAND',BAND,' ')
	  CALL XVWRIT(OUNIT,BUF,IND,'LINE',LINEOUT,'BAND',BANDOUT,' ')
        ENDDO

110   ENDDO

      RETURN
      END

c *********************************************************************
      SUBROUTINE DRAWNUM(OUNIT,LINE,SAMP,ILAT,ILON,NLO,NSO,sb,nbo,
     &		DNGRID,MAXDN,ALTERNATE,IZOOM,IFLAG,buf)
C
C Put a number (ILAT or ILON) next to a (lat,lon) intersection at
C   screen location (LINE,SAMP).  If IFLAG=0, it's a latitude, and
C   if IFLAG=1, it's a longitude.
C
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 BUF(10000)
      INTEGER*4 SAMP,DNGRID
      LOGICAL ALTERNATE,XVPTST

      LOGICAL*1 OUTBUF(420)
      INTEGER*2 HBUF(420)
      INTEGER WHITE
      CHARACTER*7 CBUF
      INTEGER*4 BANDOUT,BAND

      MAXDN255 = MAXDN/255
      IF (IFLAG.EQ.0) THEN
C  ...Determine number of characters in latitude (NUMC)
C  & Convert latitude to ASCII.
        IF (ILAT.LE.-10) THEN
	  NUMC = 3
	  WRITE(CBUF,1003) ILAT
1003	  FORMAT(I3)
        ELSE
	  NUMC = 2
	  WRITE(CBUF,1002) ILAT
1002	  FORMAT(I2)
        ENDIF
      ELSE

C  ...Determine number of characters in longitude (NUML)
C  & Convert longitude to ASCII.
        NUMC = 1
	IF (ILON.LT.0) NUMC=ALOG10(FLOAT((-1)*ILON))+2
        IF (ILON.GT.0) NUMC=ALOG10(FLOAT(ILON))+1
        IF (NUMC.EQ.1) WRITE(CBUF,1001) ILON
        IF (NUMC.EQ.2) WRITE(CBUF,1002) ILON
        IF (NUMC.EQ.3) WRITE(CBUF,1003) ILON
        IF (NUMC.EQ.4) WRITE(CBUF,1004) ILON
1001    FORMAT(I1)
1004    FORMAT(I4)
        IF (XVPTST('EAST')) THEN
	  CBUF = CBUF(:NUMC)//'E'
	ELSE
	  CBUF = CBUF(:NUMC)//'W'
	ENDIF
	NUMC = NUMC+1
      ENDIF
      NUMB = NUMC*IZOOM*6			!Total number of bytes
      IEND = MIN0(NLO-LINE,7*IZOOM)
      JEND = MIN0(NSO-SAMP,NUMB)

      WHITE = DNGRID
      BANDOUT = 0
      DO BAND=SB,SB+NBO-1
        BANDOUT = BANDOUT + 1
      IF (.NOT.ALTERNATE) GOTO 50	!Skip if ALTERNATE not specified
C
C     ....Find mean dn in area where text will be so we can make letters
C     ....that will contrast with the background data.
        I = 0
        SUM = 0
        DO 20 I=0,IEND
          CALL XVREAD(OUNIT,buf,IND,'LINE',LINE+I,'BAND',BAND,' ')
          DO 20 J=1,JEND
   20     SUM = BUF(SAMP+J) + SUM
      AVE = SUM/((IEND+1)*JEND)		!Complement text if average DN
      IF (AVE.GT.MAXDN/2) WHITE=0	!is greater than half-scale.

   50 IREC = LINE
      K = 0		!The characters are 7 lines tall (K=0 to 6)
C
C     ...Add the numbers to the output image
      DO 100 I=0,IEND
        CALL XVREAD(OUNIT,buf,IND,'LINE',IREC,'BAND',BAND,' ')
        IF (MOD(I,IZOOM).EQ.0) THEN
          CALL TEXT(CBUF,NUMC,K,outbuf,6*IZOOM,255)  !Create text
          CALL MVE(3,JEND,OUTBUF,hbuf,1,1)	!Convert bytes to half
          K = K + 1
        ENDIF
        DO J=1,JEND
          IF (HBUF(J).NE.0) BUF(SAMP+J)=WHITE	!Insert text
        ENDDO
        CALL XVWRIT(OUNIT,BUF,IND,'LINE',IREC,'BAND',BANDOUT,' ')
c        CALL XVWRIT(OUNIT,BUF,IND,'LINE',LINEOUT,'BAND',BANDOUT,' ')
  100   IREC = IREC + 1
        ENDDO

      RETURN
      END

c *********************************************************************
	SUBROUTINE SCALEBAR(OUNIT,KPP,NLO,NSO,SB,NBO,DNGRID,MAXDN,
     $    ALTERNATE,buf)
C
C       This routine writes a scale bar and labels it.
C
C    OUNIT = output file handle
C    KPP = kilometers per pixel ( = data(7))
C    NLO = number of lines in output image file
C    NSO = number of samples in output image file
C    NBO = number of bands in output image file
C    DNGRID = data number of grid (either 'WHITE or 'BLACK)
C    MAXDN = maximum data number (255 or 32767)
C    ALTERNATE = true if keyword 'ALTERNATE specified
C    BUF = line buffer
C 
      REAL*4 NUM,KPP
      LOGICAL*1 OUTBUF(420)
      INTEGER*2 HBUF(420),BUF(10000)
      INTEGER*4 DNGRID,OUNIT,LINE,SAMP
      LOGICAL ALTERNATE
      CHARACTER*7 CBUF
      CHARACTER*1 CBUF1
      INTEGER*4 SB,NBO
      INTEGER*4 BANDOUT, BAND

      BANDOUT = 0
      DO BAND=SB,SB+NBO-1
       BANDOUT = BANDOUT + 1
       CALL XVREAD(OUNIT,buf,ind,'LINE',NLO-21,'BAND',BAND,' ')
       DO J = 11,60
	   BUF(J) = DNGRID        ! write 50-pixel horizontal bar
       END DO
      ENDDO
    
      BANDOUT = 0
      DO BAND=SB,SB+NBO-1
       BANDOUT = BANDOUT + 1
       DO I = NLO-23,NLO-19
        CALL XVREAD(OUNIT,buf,ind,'LINE',I,'BAND',BAND,' ')
	   BUF(11) = DNGRID     ! write vertical bars, 5 pixels high
	   BUF(60) = DNGRID
	   CALL XVWRIT(OUNIT,BUF,ind,'LINE',I,'BAND',BANDOUT,' ')
       END DO
      ENDDO
      
      NUM = 50 * KPP          ! kilometers in 50 pixels
      INUM = NUM
      IFRAC = 0
      IF (NUM.GE.1000.) THEN
	NTEXT = 4                  ! determine number of characters
	WRITE(CBUF,1004) INUM
1004	FORMAT(I4)
      ELSE IF (NUM.GE.100.) THEN   !     in the scale label number.
	NTEXT = 3
	WRITE(CBUF,1003) INUM
1003	FORMAT(I3)
      ELSE IF (NUM.GE.10.) THEN
	NTEXT = 2
	WRITE(CBUF,1002) INUM
1002	FORMAT(I2)
      ELSE
	NFRAC = INT(MOD(NUM,1.0)*10)   ! for numbers smaller than 10,
	IFRAC = 1                      ! add one decimal place.
	NTEXT = 1
	WRITE(CBUF,1001) INUM
1001	FORMAT(I1)
      ENDIF
      IF (IFRAC.EQ.1) THEN
	WRITE(CBUF1,1001) NFRAC
	CBUF = CBUF(:NTEXT)//'.'//CBUF1//' KM'
      ENDIF
      NUMC = NTEXT+2*IFRAC
      CBUF = CBUF(:NUMC)//' KM'

      NUMC = NUMC+3			! number of characters written
      NUMB = NUMC*6                 ! number of bytes to modify
      IEND = MIN0(NLO-LINE,7)
      JEND = MIN0(NSO-SAMP,NUMB)

      BANDOUT = 0
      DO 105 BAND=SB,SB+NBO-1
      BANDOUT = BANDOUT + 1
      LINE = NLO - 12              ! top line to put text
      SAMP = 35 - 3*NUMC           ! leftmost sample to put text

      WHITE = DNGRID               ! WHITE = text color
      IF (.NOT.ALTERNATE) GOTO 50
      I = 0           ! If 'ALTERNATE is specified, find the mean DN
      SUM = 0         ! in the area where the text will be so we can
      DO 20 I = 0,IEND   ! make letters that will contrast with the
	CALL XVREAD(OUNIT,buf,IND,'LINE',LINE+I,'BAND',BAND,' ')  ! background data.
	DO 20 J = 1,JEND
	  SUM = BUF(SAMP+J) + SUM
20    CONTINUE
      AVE = SUM/((IEND+1)*JEND)
      IF (AVE.GT.MAXDN/2) WHITE=0

50    IREC = LINE
      K = 0         ! The characters are 7 lines tall (K = 0 to 6)
       DO 100 I = 0,IEND
	  CALL XVREAD(OUNIT,buf,IND,'LINE',IREC,'BAND',BAND,' ')
	  CALL TEXT(CBUF,NUMC,K,outbuf,6,255)   ! create text
	  CALL MVE(3,NUMB,OUTBUF,hbuf,1,1)    ! convert bytes to halfwords
	  K = K + 1
	  DO J = 1,JEND
	   IF (HBUF(J).NE.0) BUF(SAMP+J) = WHITE     ! Insert text
	  ENDDO
	  CALL XVWRIT(OUNIT,BUF,IND,'LINE',IREC,'BAND',BANDOUT,' ')
 	  IREC = IREC + 1
100   CONTINUE
105   CONTINUE

      RETURN
      END

c *********************************************************************
      SUBROUTINE XPRINT(IPTR,LINE,SAMP,LAT,LONG,NCOL)
      REAL LAT,LONG
      INTEGER*2 LINE,SAMP
      CHARACTER*50 MSG
      WRITE(MSG,1000) IPTR, LINE, SAMP, LAT, LONG
 1000 FORMAT(1X,I6,2(2X,I5),1X,F6.1,1X,F7.1)
c      CALL OUTCON(IPTR,msg(7),6)
c      CALL OUTCON(LINE,msg(14),-5)
c      CALL OUTCON(SAMP,msg(21),-5)
c      CALL OUTCON(LAT,msg(27),6,1)	! leaves no space -- fixed in WRITE
c      CALL OUTCON(LONG,msg(35),7,1)
      CALL XVMESSAGE(MSG(:NCOL),' ')
      RETURN
      END

c *********************************************************************
      SUBROUTINE LIMBPT(OM,PSC3,PSUN3,SCLON,SOAL,SOAS,SCALE,SL,SS,
     &                  NL,NS,ITERM,MAXNUM,linebuf,sampbuf,nptl,nptt)

C Routine to compute the planet limb and (optionally) terminator.
C
C Inputs:
c   OM = matrix transforming from Planet to Camera coordinates
c   PSC3 = vector from Planet to s/c in planet coordinates
c   PSUN3 = unit vector from Planet to sun in planet coordinates
c   SCLON = sub-s/c longitude (degrees)
c   SOAL,SOAS = optical axis line/samp
c   SCALE = scale (pix/radian)
c   SL,...,NS = image dimensions
c   ITERM = 0: don't compute terminator points, = 1: do terminator
c   MAXNUM = max. number of points in buffer

c Outputs:
C  LINEBUF,SAMPBUF contain the limb-points as (line,sample) pairs.
c  NPTL = number of limb points in these buffers
c  NPTT = number of terminator points in these buffers (starting at NPTL+1)
C
c NOTE:  Image Space (Map type = 7) is not supported

c 94-10-15 -lwk- adapted from pgm NAV

      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 SL,SS

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 OM(3,3),PSC3(3),PSUN3(3)
      REAL*4 SCLON,SOAL,SOAS,SCALE
      INTEGER*2 LINEBUF(MAXNUM),SAMPBUF(MAXNUM)

      REAL*4 LINE,SAMP,LINE0,SAMP0,LINE2,SAMP2,linet,sampt,linet0,
     & sampt0,linet2,sampt2,maxdis
      REAL*8 PI/3.141592653589793D0/
      INTEGER*4 SSPFLAG
      LOGICAL ILL0,ILL1
C
      PI2 = PI/2.D0
      DEGRAD = PI/180.D0
      RADDEG = 180.D0/PI

c  convert to d.p.:
      OAL = SOAL
      OAS = SOAS
      ZSCALE = SCALE

      CALL XVPARM( 'LSPACE', maxdis, icnt, idef, 1) 
      MAXDIS = MAXDIS*MAXDIS

c  constants for visibility & solar illumination check 
c  (AI2 etc. are computed in GETPC):
      XSC = AI2*PSC3(1)
      YSC = BI2*PSC3(2)
      ZSC = CI2*PSC3(3)
      XSUN = AI2*PSUN3(1)
      YSUN = BI2*PSUN3(2)
      ZSUN = CI2*PSUN3(3)

      RPSC = DSQRT(PSC3(1)*PSC3(1)+PSC3(2)*PSC3(2)+PSC3(3)*PSC3(3))
      RPLAN = 1.0/DSQRT(AI2)

C           Compute picture window
      RSL = SL 		! Starting line
      RSS = SS 		! Starting sample
      REL = SL + NL - 1 	! Ending line
      RES = SS + NS - 1 	! Ending sample
C
      DSCLON = SCLON*DEGRAD
      CLON = DCOS(DSCLON)
      SLON = DSIN(DSCLON)
C         Check for spacecraft position special cases...
      IF (SCLON.EQ.0.0 .OR. PSC3(2).EQ.0.) THEN
	SSPFLAG = 1		!x-z plane coincides with plane of spacecraft
      ELSE
	SSPFLAG = 0
      ENDIF
C
      NPTL = 0
      NPTT = 0
      RLAT = -PI2		!Start at the south pole
      DLAT = 0.5D0*DEGRAD	!Initial step size is 0.5 degrees
      LINE0 = -999.D0		!Set initial point to flag
      line = -999.d0
      line2 = -999.d0
      linet0 = -999.d0
C
C     ....Beginning at the south pole, search at one-degree latitude
C     ....increments for a limb point.
      ind1 = 0			! flag indicating no limb found yet
   10 CALL GETLIMB(IND,RLAT,CLON,SLON,z,x1,y1,x2,y2,SSPFLAG)
      IF (IND.EQ.0) THEN		!Branch if limb point is found
	if (iterm.gt.0) then		! look for a terminator point
	  xlon1 = 0.
	  xlon2 = 2.*pi
	  clat = dcos(rlat)
	  slat = dsin(rlat)
	  r = 1.d0/dsqrt(ai2*(clat*clon)**2+
     1				  bi2*(clat*slon)**2+ci2*slat**2)
	  rr = r*clat
	  go to 60
	endif
        RLAT = RLAT + DEGRAD		!Otherwise, step one degree
        IF (RLAT.LT.PI2) GOTO 10	!and try again.
        GOTO 110			!Return if no limb found.
      ENDIF
      if (ind1.eq.0) then
	DLAT = 0.5D0*DEGRAD		! reset in case terminator changed it
	ind1 = 1
      endif
      IF (RLAT.EQ.-PI2) GOTO 40		!Branch if at south pole.
      RLAT = RLAT - DLAT		!Back up .5 degrees
C     ....Find the beginning of the limb by cutting the latitude
C     ....increment in half at each step.
   30 CALL GETLIMB(IND,RLAT,CLON,SLON,z,x1,y1,x2,y2,SSPFLAG)
      IF (IND.EQ.1) THEN			   !If we are on the limb.
         CALL GETLS(X1,Y1,Z,line,samp,             !Convert limb points to
     &               OM,PSC3,OAL,OAS,ZSCALE)	   !(LINE,SAMP) and
         CALL GETLS(X2,Y2,Z,line2,samp2,      	   !(LINE2,SAMP2) and check
     &               OM,PSC3,OAL,OAS,ZSCALE)	   !pixel distance between
         DIS = (LINE-LINE2)**2 + (SAMP-SAMP2)**2   !the points.
         IF (DIS.LT.maxdis) GOTO 40		   !If close enough, start...
         DLAT = DLAT/2				   !Else, reduce step size,
         RLAT = RLAT - DLAT			   !and back up some more.
      ELSE					   !If we are off the limb,
         DLAT = DLAT/1.9			   !reduce step size,
         RLAT = RLAT + DLAT			   !and move forward a notch.
      ENDIF
      GOTO 30
C
C          Here when we have found the start of the limb
C          Now step around the limb until we get to the north pole...
C
C	   First, get limb points at current RLAT...
   40 CALL GETLIMB(IND,RLAT,CLON,SLON,z,x1,y1,x2,y2,SSPFLAG)
      IF (IND.EQ.0) THEN			   !Stepped off limb
	IF (RLAT.EQ.PI2) GOTO 110		   !Stop if at N.pole
	DIS = (LINE-LINE2)**2 + (SAMP-SAMP2)**2
	! if terminator option, continue to N.pole and don't dither
	! around end of limb -- otherwise, stop when off image or
	! converged
	IF (LINE0.EQ.-999.D0 .or. DIS.LT.maxdis) then
	  if (iterm.eq.0) go to 110
	  xlon1 = 0.
	  xlon2 = 2.*pi
	  clat = dcos(rlat)
	  slat = dsin(rlat)
	  r = 1.d0/dsqrt(ai2*(clat*clon)**2+bi2*(clat*slon)**2+
     &     ci2*slat**2)
	  rr = r*clat
	  DLAT = 0.5D0*DEGRAD
	  go to 60
	endif
	DLAT = DLAT/2				   !Else back up half a step.
	if (dlat.le.1.0e-6) go to 110
	RLAT = RLAT - DLAT
	GOTO 40
      ENDIF

c	convert x,y,z to line/sample:
      CALL GETLS(X1,Y1,Z,line,samp,OM,PSC3,OAL,OAS,ZSCALE)

C            Check spacing between points
      IF (LINE0.NE.-999.D0) THEN
          DIS = (LINE-LINE0)**2 + (SAMP-SAMP0)**2
          IF (DIS.LT.0.5*maxdis.AND.	!If spacing is less than .5 pixel
     &           RLAT.LT.PI2) THEN
             RLAT = RLAT - DLAT		!back up
             DLAT = 1.5D0*DLAT		!and increase step size by 50%
             IF (RLAT+DLAT.LT.PI2) THEN
                  RLAT = RLAT + DLAT
             ELSE
                  DLAT = PI2 - RLAT
                  RLAT = PI2
             ENDIF
             GOTO 40
          ENDIF
          IF (DIS.GT.maxdis) THEN      !If spacing is greater than .5 pixel
             RLAT = RLAT - DLAT		!back up
             DLAT = 0.5D0*DLAT		!and decrease step size by 50%
             RLAT = RLAT + DLAT
             GOTO 40
          ENDIF
      ENDIF

C                Now do (x2,y2,z)
      CALL GETLS(X2,Y2,Z,line2,samp2,OM,PSC3,OAL,OAS,ZSCALE)

      if (iterm.eq.0) go to 70
c     ....look for terminator point at this latitude (note that limb 
c     ....points need not be in image for terminator to be so!)
      rr = dsqrt(x1*x1+y1*y1) ! (biaxial assumed, s/b same if x2,y2 used)
c     ....this defines the long. arc from limb to limb:
      xlon1 = datan2(-y1,x1)	! West long.
      if (xlon1.lt.0.) xlon1 = xlon1+2.0*pi
      xlon2 = datan2(-y2,x2)
      if (xlon2.lt.0.) xlon2 = xlon2+2.0*pi
c     ....must now figure out which part is visible:
      if (xlon1.gt.xlon2) then	! put them in ascending order
	temp = xlon1
	xlon1 = xlon2
	xlon2 = temp
      endif
c     ...check if midpoint is visible, if not then use the other
c     ...part of the arc:
      xlon0 = 0.5*(xlon1+xlon2)
      xx = rr*dcos(xlon0)
      yy = -rr*dsin(xlon0)	! West longitude
      dot1 = x1*xSC + y1*ySC + z*zSC	! dot product at one limb
      if (xx*xSC + yy*ySC + z*zSC .lt. dot1) then	! it's not
	temp = xlon2-2.0*pi
	xlon2 = xlon1
	xlon1 = temp
      endif

   60 dxlon = dabs(xlon2-xlon1)
c     ....rough estimate of how many pixels along this lat. line
      n1 = dxlon*rplan*zscale*dcos(rlat)/rpsc+1.0	! (+1 to ensure > 0)
      dxlon = dxlon/n1
      xx = rr*dcos(xlon1)
      yy = -rr*dsin(xlon1)	! West longitude
      ill0 = xx*xSUN + yy*ySUN + z*zSUN .gt. 0.D0
c     ...find point(s) at which illumination state changes:
      linet = -9999999.d0
      linet2 = -999.d0
      do i=1,n1
	xx = rr*dcos(xlon1)
	yy = -rr*dsin(xlon1)	! West longitude
	ill1 = xx*xSUN + yy*ySUN + z*zSUN .gt. 0.D0
	if (ill0.neqv.ill1) then
	  ill0 = ill1		! look for 2nd point
	  if (xx*xSC + yy*ySC + z*zSC .gt. 0.0) then	! it's visible
	    xlon1 = xlon1-0.5*dxlon  ! crude interpolation (TBD: refine this?)
	    xx = rr*dcos(xlon1)
	    yy = -rr*dsin(xlon1)	! West longitude
	    if (linet.le.-9999999.) then
	      call getls(xx,yy,z,linet,sampt,om,psc3,oal,oas,zscale)
	    else
	      call getls(xx,yy,z,linet2,sampt2,om,psc3,oal,oas,zscale)
	      go to 70
	    endif
	  endif
	endif
	xlon1 = xlon1+dxlon
      enddo

c     check spacing of pixels if no limb yet:
   70 if (ind1.eq.0 .and. linet.gt.-990.d0 .and. linet0.gt.-990.d0) then
	dis = (linet-linet0)**2 + (sampt-sampt0)**2
	if (dis.gt.4.*maxdis) dlat = 0.5d0*dlat
      endif

C     ....If we get this far, the limb-point is accepted.  
      LINE0 = LINE			!Update coordinates of last
      SAMP0 = SAMP			!limb-point found in image.
      linet0 = linet
      sampt0 = sampt

      IF (LINE.GE.RSL.AND.LINE.LE.REL.AND.SAMP.GE.RSS.AND.SAMP.LE.RES)
     & THEN
	if (nptt+nptl+1.gt.maxnum) go to 90
	NPTL = NPTL + 1
	LINEBUF(NPTL) = LINE+0.5
	SAMPBUF(NPTL) = SAMP+0.5
      ENDIF
      IF (LINE2.GE.RSL.AND.LINE2.LE.REL.AND.SAMP2.GE.RSS.AND.
     & SAMP2.LE.RES) THEN
	if (nptt+nptl+1.gt.maxnum) go to 90
	NPTL = NPTL + 1
	LINEBUF(NPTL) = LINE2+0.5
	SAMPBUF(NPTL) = SAMP2+0.5
      ENDIF
      if (linet.ge.rsl.and.linet.le.rel.and.sampt.ge.rss.and.
     & sampt.le.res) then
	if (nptt+nptl+1.gt.maxnum) go to 90
	nptt = nptt + 1
	linebuf(maxnum-nptt+1) = linet+0.5
	sampbuf(maxnum-nptt+1) = sampt+0.5
      else
	linet0 = -999.d0
      endif
      if (linet2.ge.rsl.and.linet2.le.rel.and.sampt2.ge.rss.and.
     & sampt2.le.res) then
	if (nptt+nptl+1.gt.maxnum) go to 90
	nptt = nptt + 1
	linebuf(maxnum-nptt+1) = linet2+0.5
	sampbuf(maxnum-nptt+1) = sampt2+0.5
      endif
      go to 100
C
   90 CALL XVMESSAGE(' ***Maximum number of limb points computed',' ')
      GOTO 110

C     ....If neither point is in the image, take one degree steps
C     ....until it is -- if terminator requested, check that too
  100 IF ( 
     & (LINE.LT.RSL.OR.LINE.GT.REL.OR.SAMP.LT.RSS.OR.SAMP.GT.RES)
     & .AND. 
     & (LINE2.LT.RSL.OR.LINE2.GT.REL.OR.SAMP2.LT.RSS.OR.SAMP2.GT.RES)
     & .and.
     & (((linet.lt.rsl.or.linet.gt.rel.or.sampt.lt.rss.or.sampt.gt.res)
     & .and.
     & (linet2.lt.rsl.or.linet2.gt.rel.or.sampt2.lt.rss.or.
     & sampt2.gt.res)) .or. iterm.eq.0) ) THEN
        RLAT = RLAT + DEGRAD
        LINE0 = -999.D0		!Set initial point to flag
	linet0 = -999.d0
        IF (RLAT.GE.PI2) GOTO 110	!Stop if at north pole
	if (ind1.eq.0) go to 10
        GOTO 40
      ENDIF

      IF (RLAT.GE.PI2) GOTO 110		!If at north pole, we are done
      RLAT = RLAT + DLAT		!Otherwise, take another step.
      IF (RLAT.GT.PI2) THEN		!If we step too far,
        DLAT = PI2 - RLAT + DLAT	!adjust step size
        RLAT = PI2			!and back up to north pole
      ENDIF
      if (ind1.eq.0) go to 10
      GOTO 40				!Go back and repeat process.
C
  110 CONTINUE

      IF (NPTL.GT.1) THEN
	CALL I2SORT(LINEBUF,SAMPBUF,NPTL)	! Sort the array
	CALL PRNT(4,1,NPTL,' Number of limb points = .')
      ELSE
	CALL XVMESSAGE(' No limb points found',' ')
      ENDIF

      IF (NPTT.GT.1) THEN
	DO I=1,NPTT
	  LINEBUF(NPTL+I) = LINEBUF(MAXNUM-NPTT+I)
	  SAMPBUF(NPTL+I) = SAMPBUF(MAXNUM-NPTT+I)
	ENDDO
	CALL I2SORT(LINEBUF(NPTL+1),SAMPBUF(NPTL+1),NPTT)
	CALL PRNT(4,1,NPTT,' Number of terminator points = .')
      ELSE IF (ITERM.NE.0) THEN
	CALL XVMESSAGE(' No terminator points found',' ')
      ENDIF

      RETURN
      END

c *********************************************************************
      SUBROUTINE GETLIMB(IND,RLAT,CLON,SLON,Z,X1,Y1,X2,Y2,SSPFLAG)

C Given latitude RLAT, find point on limb (x,y,z) at that latitude.
C Inputs:  RLAT,CLON,SLON,SSPFLAG, and PCONST elements
C Outputs: IND, (X1,Y1,Z) (X2,Y2,Z)
C Upon return, IND=1 if limb point is found, =0 otherwise.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      INTEGER*4 SSPFLAG

C           First, compute Z as a function of RLAT....
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
C             Compute planetocentric radius
      r = 1.D0/DSQRT(AI2*(CLAT*CLON)**2+BI2*(CLAT*SLON)**2+CI2*SLAT**2)
      Z = r*SLAT

      IF (SSPFLAG.EQ.1) GOTO 50
      B = B1*Z + B0
      C = C2*Z**2 + C1*Z + C0
      D = B**2 - A0*C

      IF (D.LT.0.D0) THEN
           IND = 0		!No limb point at this latitude...
      ELSE
           D = DSQRT(D)
           X1 = (-B + D)/A0
           X2 = (-B - D)/A0
           Y1 = (1.D0 - AI2XS*X1 - CI2ZS*Z)/BI2YS
           Y2 = (1.D0 - AI2XS*X2 - CI2ZS*Z)/BI2YS
           IND = 1
      ENDIF

      RETURN

C            Here for spacecraft position special cases: SCLON=0 or
C            SCLAT = PI/2 or -PI/2.  In these cases PSC3(2) = 0, so that
C            BI2YS=0.
   50 X1 = (1.D0 - CI2ZS*Z)/AI2XS
      D  = 1 - AI2*X1**2 - CI2*Z**2
      IF (D.LT.0.D0) THEN
           IND = 0		!No limb point at this latitude...
      ELSE
           Y1 = DSQRT(D/BI2)
           X2 = X1
           Y2 = -Y1
           IND = 1
      ENDIF

      RETURN
      END

c *********************************************************************
      SUBROUTINE GETLS(X,Y,Z,LINE,SAMP,OM,PSC3,OAL,OAS,ZSCALE)

C Compute (line,sample) coordinates for limb points (x,y,z)
C Inputs:  (x,y,z) expressed in (x3,y3,z3) coordinates.
C                 OM,PSC3,OAL,OAS,ZSCALE
C Outputs: (LINE,SAMP) corresponding to (x,y,z)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3),xc(3)
      REAL*4 LINE,SAMP

C           Compute vector from spacecraft to limb point
      xc(1) = X - PSC3(1)
      xc(2) = Y - PSC3(2)
      xc(3) = Z - PSC3(3)
C           Rotate vector into camera coordinates
      call mxv( om, xc, xc)

C          Scale vector into pixels
      S = ZSCALE/xc(3)
      LINE = S*xc(2) + OAL
      SAMP = S*xc(1) + OAS

      RETURN
      END

c *********************************************************************
      SUBROUTINE GETPC(PSC3,RA,RB,RC)

C Routine to compute miscellaneous projection constants for LIMBPT
C Inputs: PSC3,RA,RB,RC
C Outputs: All data in COMMON/PCONST/
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 PSC3(3)
      real*4 ra,rb,rc

      XS = PSC3(1)
      YS = PSC3(2)
      ZS = PSC3(3)
C             For projection routines...
      AI2 = (1.D0/RA)**2
      BI2 = (1.D0/RB)**2
      CI2 = (1.D0/RC)**2

      AI2XS = (XS/RA)/RA
      BI2YS = (YS/RB)/RB
      CI2ZS = (ZS/RC)/RC

      CTERM = (XS/RA)**2 + (YS/RB)**2 + (ZS/RC)**2 - 1
C             For limb point computation...
      A0 = (XS/RA)**2 + (YS/RB)**2
      B0 = -XS
      B1 = (XS/RC)*(ZS/RC)
      C0 = (RB+YS)*(RB-YS)*(RA/RB)**2
      C1 = -2*ZS*(RA/RC)**2
      C2 = ((YS/RB)**2 + (ZS/RC)**2)*(RA/RC)**2
      RETURN
      END
