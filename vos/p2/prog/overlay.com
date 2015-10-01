$!****************************************************************************
$!
$! Build proc for MIPL module overlay
$! VPACK Version 1.9, Thursday, June 24, 2004, 11:23:51
$!
$! Execute by entering:		$ @overlay
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
$ write sys$output "*** module overlay ***"
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
$ write sys$output "Invalid argument given to overlay.com file -- ", primary
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
$   if F$SEARCH("overlay.imake") .nes. ""
$   then
$      vimake overlay
$      purge overlay.bld
$   else
$      if F$SEARCH("overlay.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake overlay
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @overlay.bld "STD"
$   else
$      @overlay.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create overlay.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack overlay.com -mixed -
	-s overlay.f -
	-p overlay.pdf -
	-t tstoverlay.pdf -
	-i overlay.imake -
	-t tstoverlay.log_solos tstoverlay.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create overlay.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create overlay.pdf
process help=*
PARM INP      TYPE=STRING  COUNT=1
PARM OUT      TYPE=STRING  COUNT=1
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM SIZE     TYPE=INTEGER COUNT=4			DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=0:1   VALID=(0:10000)  DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=0:1   VALID=(0:10000)  DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=0:1   VALID=(0:10000)  DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=0:1   VALID=(0:10000)  DEFAULT=0
PARM MAXDN    TYPE=INTEGER		VALID=(0:32767)	DEFAULT=255
PARM AUTO     TYPE=KEYWORD COUNT=0:1   VALID=AUTO	DEFAULT=--
PARM NOGRID   TYPE=KEYWORD COUNT=0:1   VALID=NOGRID	DEFAULT=--
PARM LIMITS   TYPE=REAL    COUNT=4  VALID=-360.:360 DEFAULT=(-90.,90.,0.,360.)
PARM SUPPRESS TYPE=REAL	   COUNT=1     VALID=(0.:90.)	DEFAULT=80.
PARM DLA1     TYPE=REAL    COUNT=1     VALID=(.01:90.)	DEFAULT=15.
PARM DLA2     TYPE=REAL    COUNT=1     VALID=(.01:90.)	DEFAULT=15.
PARM DLO1     TYPE=REAL    COUNT=1     VALID=(.01:360.)	DEFAULT=15.
PARM DLO2     TYPE=REAL    COUNT=1     VALID=(.01:360.)	DEFAULT=30.
PARM GLATITUD TYPE=REAL    COUNT=0:181 VALID=(-90.:90.)	DEFAULT=--
PARM GLONGITU TYPE=REAL    COUNT=0:361 VALID=(0.:360.)	DEFAULT=--
PARM DN       TYPE=KEYWORD COUNT=0:1   VALID=(BLACK,WHITE)  DEFAULT=WHITE
PARM ALTERNAT TYPE=KEYWORD COUNT=0:1   VALID=ALTERNAT	DEFAULT=--
PARM NONUMBER TYPE=KEYWORD COUNT=0:1   VALID=NONUMBER	DEFAULT=--
PARM SCALEBAR TYPE=KEYWORD COUNT=0:1   VALID=(SCALEBAR,NOSCALE)	+
  DEFAULT=NOSCALE
PARM EXPAND   TYPE=INTEGER COUNT=1     VALID=(1:10)	DEFAULT=1
PARM EAST     TYPE=KEYWORD COUNT=0:1   VALID=EAST	DEFAULT=--
PARM DLATITUD TYPE=INTEGER COUNT=1     VALID=(1:181)	DEFAULT=1
PARM DLONGITU TYPE=INTEGER COUNT=1     VALID=(1:360)	DEFAULT=1
PARM LIMB     TYPE=KEYWORD COUNT=0:1   VALID=LIMB	DEFAULT=--
PARM DNLIMB   TYPE=INTEGER		VALID=(0:32767)	DEFAULT=128
PARM LSPACE   TYPE=REAL    		VALID=(0.2:20.) DEFAULT=0.8
PARM TERMINAT TYPE=KEYWORD COUNT=0:1   VALID=TERMINAT	DEFAULT=--
PARM TSPACE   TYPE=INTEGER		VALID=(1:20)    DEFAULT=1
PARM DNTERM   TYPE=INTEGER		VALID=(0:32767)	DEFAULT=64
PARM SUBSOL   TYPE=REAL    COUNT=0:2	VALID=(-360.:360.) DEFAULT=--
PARM LAT_TYPE KEYWORD VALID=(PGRAPHIC,PDETIC,PCENTRIC) DEFAULT=PCENTRIC
PARM MISSION  KEYWORD COUNT=0:1 VALID=("CASSINI", "GLL", "VGR-1", "VGR-2",+
 "MAR10", "VIKOR", "MAR-9","NOPRO") DEFAULT=--
PARM PRINT    TYPE=KEYWORD COUNT=0:1 VALID=PRINT	DEFAULT=--
END-PROC
.TITLE
VICAR program OVERLAY
.HELP
PURPOSE:

OVERLAY is a VICAR program which superimposes a latitude-longitude grid 
on a planetary image.  If the image is a perspective one ("object space"), 
then the option also exists to draw a limb and/or terminator.  The target
body is assumed to be a sphere or oblate-spheroid, as determined by its 
polar and equatorial radii.

    OVERLAY INP=PIC OUT=OPIC user-parameters

were PIC is an optional input image and OPIC is the output image.

PIC must have a standard VICAR map label, as generated by MAP3, PERSLAB,
or project-specific logging programs such as NIMSCMM.

The image must be in either byte or 16-bit integer (halfword) data format.

OPIC will be the same data format as PIC.  If PIC is omitted, then the
latitude-longitude grid will be displayed over a background of 0 DN and
the output data format will be byte.

The grid is normally displayed in west-longitude, planetocentric latitudes.
Note, however, that the grid overlay may be optionally displayed in east-
longitude (see EAST keyword).

If the image is in Perspective projection, then the option exists to draw
the limb or terminator.  In this case, the position of the Sun (specified
as the sub-solar point on the target body) must be available.  This can
either be supplied by the user with the SUBSOL parameter, or else retrieved
from the label if the input image is an unresampled product of one of the
supported set of flight projects.  (See HELP MISSION for a list of the
latter.)

.page
SPECIFYING THE LATITUDE-LONGITUDE GRID:

All latitudes must be planetocentric.  All longitudes must be entered as 
west-longitudes.

The latitude-longitude grid overlay is determined by specifying its limits
(minimum and maximum latitudes and longitudes), latitude and longitude
spacing, and contrast (black, white, or modulating grid lines), and the
size and spacing of the grid annotation.  Note that the grid can be turned
off altogether using the 'NOGRID keyword;  in this case, the user must
specifiy either LIMB or TERMINAT (or both), or the program will have
nothing to do and quit.

    (1) Grid limits:  The execution of OVERLAY can be speeded up by limiting
	the calculation of latitude and longitude lines to the region within
	the picture's field-of-view.  These limits may be specified explicitly
	via the LIMITS parameter.  Alternatively, the program can be asked
	to determine these limits automatically via the AUTO keyword.  The
	AUTO keyword should only be used for high-resolution frames, where
	the target-body completely fills the field-of-view.

	LIMITS=(r1,r2,r3,r4) - specifies the minimum latitude, maximum latitude,
			       minimum longitude, and maximum longitude boun-
			       daries of the grid overlay.
	'AUTO		     - determine latitude-longitude limits automatically

    (2) Grid spacing:  The grid spacing along latitude and longitude lines
	is specified via the DLA1, DLO1, DLA2, DLO2 parameters, where DLA1
	and DLO1 specify the spacing near the equator, and  DLA2 and DLO2
	specify the spacing near the poles.  The SUPPRESS parameter specifies
	the latitude boundary between the equatorial and polar regions.  The
	grid spacing (expressed in degrees) is usually increased in the polar
	region to prevent overcrowding of grid lines (the exceptions are the
        polar projections, for which the opposite holds true).  The default
        values for these parameters are only appropriate for image or object
        space full-disk (far-encounter) images.

	Alternatively, the latitude and longitude lines may be specified
	explicitly via the GLATITUD and GLONGITU parameters.

	SUPPRESS=r - latitude at which grid spacing switches from r1 to r2
	DLA1=r1	   - latitude spacing btwn grid lines, equatorward of r (deg)
	DLA2=r2    - latitude spacing btwn grid lines, poleward of r (deg)
	DLO1=r1    - longitude spacing btwn grid lines, equatorward of r (deg)
	DLO2=r2	   - longitude spacing btwn grid lines, poleward of r (deg)

	GLATITUD=(r1,r2,r3,...) - draws grid lines along latitudes r1,r2,r3,...
	GLONGITU=(r1,r2,r3,...) - draws grid lines along longitudes r1,r2,r3...
        'PRINT	   - prints grid intersections, etc. (massive printout)

    (3) Grid contrast:  The grid may be drawn in black, white, or data-
	dependent combination of the two, or the grid can ALTERNATe between
	black and white.

	MAXDN=n1     - specifies maximum DN of output
	'dn          - specifies DN of grid.  Valid values are WHITE (DN=n1)
		       and BLACK (DN=0)
	'ALTERNAT    - alternat DN of grid between black and white.

    (4) Grid annotation:

	'NONUMBER	- suppresses all grid annotations.
	'SCALEBAR	- adds scale bar to output image for km per pixel
	'EAST		- annotates grid using east-longitudes.
	EXPAND=n	- specifies scale factor for increasing size of 
			 annotations.
	DLATITUD=n	- annotates every nth grid intersection along latitude 
			 lines.
	DLONGITU=n	- annotates every nth grid intersection along longitude
			 lines.
.page
EXAMPLES:

  1)  OVERLAY  PIC  OPIC  GLAT=(-32.,-20.,-8.,0.,7.,19.,22.,30.) GLON=115.

      The target body is assumed to be Jupiter, and the grid lines have been 
      selected to mark several of Jupiter's belts and zones and the central 
      meridian.

  2)  MAP3  OUT=PIC  NL=1000  NS=1000  'JUPITER  'POLE  'ORTHO +
	LINE=1436.0  SAMP=671  LATI=-90.0  LONG=85.877  SCALE=75.0

      OVERLAY  PIC  OPIC DLA1=20  DLA2=10  DLO1=30  DLO2=60   SUPP=80. +
	DLAT=3  DLONG=4

      A latitude-longitude grid representing a polar-orthographic projection
      of the south-polar region of Jupiter will be drawn in white, over a
      black background.  Since there is no associated input image, the pro-
      jection information must be input manually to MAP3 in a preceding step.

      Between latitudes -80 and the equator, the latitude lines are spaced
      20 degrees apart (centered on the equator), and the longitude lines are
      spaced 30 degrees apart.  Poleward of -80 degrees, there will be no
      latitude lines and the longitude lines will be 60 degrees apart.

      The numbers denoting lat-long line intersections will be at every third
      latitude and every fourth longitude intersection.
.page
PARAMETERS FOR RETRIEVING CAMERA POINTING FROM SPICE:

The following parameters permit the user to retrieve a specific instance of
camera pointing from the SPICE kernels:

SPICEMODE specifies whether SPICE data is retrieved from LOCAL kernels or
or via the REMOTE SPICE server.  If defaulted, SPICEMODE is set to the value
of the environmental variable DEFAULTSPICE.

CKNAME and CKID are alternative ways to specify the C kernel to be used.  For
example, CKNAME=FARE or CKID=M904 specifies that MIPS_FARENC.CK is to be used.
When specified, the CKID parameter overrides the CKNAME parameter.  If the
camera pointing data is not found in the requested C kernel, the other C kernels
are searched.

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

The above parameters are optional, and if defaulted (or if no data is found for
the requested version), the program will attempt to locate the "best" data
available for the given image.  See the level 2 help (via the TAE tutor mode)
for further details.

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.

           'REMOTE CKNAME=FARE INSTITUTE=MIPS SPKID=N015 USERID=ADC retrieves
          the camera pointing created by Amy Culver at MIPS using the SP kernel
          GLL_LONG_2.BSP from file MIPS_FARENC.CK via the SPICE server.  (whew!)

It takes longer to search for SPICE data on the basis of provenance
information.  If all provenance parameters are specified, for example, the
program first searches through all the C kernels for an exact match.  If no
match is found, the search is relaxed by removing the CDATE criteria.  If no
match is found, the REQNUM criteria is removed.  Etc.

.page
PROGRAM HISTORY

WRITTEN BY: JOEL MOSHER,   27 MARCH 1984
COGNIZANT PROGRAMMER: Gary Yagi
REVISIONS:
 FEB 23 84  JAM   Original OVERLAY created by modifying PHOTFUNC.
 MAR  2 84  JAM   Add no input picture option.
 MAR  7 84  JAM   Fixed errors in longitude lines removed redundant grid points
 MAR  8 84  JAM   Calculate points to put lat-lon numbers
 MAR 14 84  JAM   Added MODULATE, LINC, and SINC keywords.
 OCT 27 85  JAM   Convert to VICAR2
 MAR  1 86  JAM   Fixed error in no input mode, added MAPLAB
 MAR 10 86  JAM   Added FIXRECT
 MAY 17 86  JAM   Added GLAT,GLON, rewrote point & number selection algorithms.
 MAY 27 87  JAM   VICAR2 parameter processing.
 MAR 11 88  GMY   Major code reorganization and clean-up:
		    0) Rewrote Help File.
		    1) Modularized algorithms.
		    2) Fixed size field implementation.
		    3) Fixed TARGET parameter.
		    4) Fixed calculation of OM from C and ME.
		    5) Fixed extraction of subspacecraft point from SEDR.
		    6) Fixed extraction of radii from SEDR.
		    7) Removed all redundant parameters.
		    8) Replaced MODULATE algorithm with ALTERNATE algorithm.
		    9) Fixed no input image option.
		   10) Fixed annotation so equation is labeled.
		   11) Fixed spacing of annotation.
		   12) Fixed problem of extra grid line at SUPPRESS latitude.
 JUN 12 88  FFM  Rename parameter SEDR to SFILE.
 MAY 12 88  FFM  Incorporate SEDRSRC keyword.
 JUN 07 88  GMY  Fix processing of SIZE parameter.
 Jan 22 89  GMY  FRs 37250,37251,37298,42709,42701.
 Jul 19 89  MEM  Instead of drawing annotation at each intersection,
		        merely draw it once for every grid line.
 Jul 27 89  MEM  When the picture scale is specified, draw a scale bar
			in the lower left-hand corner.
 DEC    90  JJL  Conversion to project independent routines.
 Mar 30 92  JFM  Corrected handling of FINDLAT capture of constant latitudal
                 grid lines for Lambert Conformal Conic and Sinusoidal
		 projections near the poles, where projected image does not
		 file the image file (FR 66586).
 Apr 02 92  JFM  Scale in lower-left corner of output made optional (FR 66545).
 Apr 17 92  JFM  Longitude and latitude annotation placements check against
		 boundaries of image (FR 66549).
 May 26 92  JFM  SFILE parameter removed; HELP file slightly revised;
		 RADIUS, REQUATOR, RPOLE limits removed; Error in no input
	  	 image grid computation corrected (FR 68870, 75745, 76934).
 mar 93     jjl  Removed SFILE parameter
 feb    93  jjl  Added transverse mercator and perspective projections.
 OCT    94  LWK  Added LIMB/TERMINATOR options
 MAR    95  LWK  Made program portable.  Removed all SPICE/SEDR and label-
		processing code (except as needed to determine subsolar point),
		require input image to have map label.
 JUL    96  OAM  Modified to call getspice2 instead of getspice.
                 Included provenance parameters in overlay.pdf.
 jun 98  lwk  added code to get subsolar lat/lon from nims label;  fixed 
	calculation of terminator points:  allow for sections where limb is 
	out of image but terminator is in it;  added code to LIMBPT to stop 
	searching for point at suitable distance once the N.pole is reached 
	-- program was in an infinite loop here for one case;  non-visible 
	points were being selected for lat's where no limb is visible;  and
	if limb didn't reach to N.pole, terminator stopped too;  replaced 
	FIXRECT by CONVEVR, to ensure that *every* call to CONVEV is followed 
	by a fix-up for Rectangular projection;  added Planetographic option 
	for NIMS (LAT_TYP keyword);  fixed longitude lines so that they are 
	more likely to agree with the the numbers in the annotation and are
	always at integer locations (as long as DLO1/DLO2 are integer!)
 Jun98  RRP  Broke long statements into smaller one to make it compile
	under hp platform. AR-9644
 9oct00 lwk- remove lat_typ dependence for Simp.Cyl.
 Aug 02 GMY - Minor changes to compile on Linux:  Change & to * in subroutine
	calls.  Change all trig calls such as cosd(theta) to cos(theta*dtr)
17dec02 lwk - put in check for infinite loop in FINDLON similar to the
	one in FINDLAT
13feb03 lwk- merged my changes with RRP/GMY/KLEM's

.LEVEL1
.VARI INP
Input image.
.VARI OUT
Output image.
.VARI SIZE
Standard VICAR size field.
.VARI SL
Starting line
.vari SS
Starting sample
.vari NL
Number of lines
.VARI NS
Number of samples
.VARI MISSION
mission override or if
no input
.VARI LIMITS
(lat,lon) region to grid
minlat,maxlat,minlon,maxlon
.VARI AUTO
Automatically determines
lat,lon limits of image.
.VARI DLA1
The spacing of latitude lines
equatorward of SUPPRESS (deg)
.VARI DLA2
The spacing of latitude lines
poleward of SUPPRESS (deg)
.VARI DLO1
The spacing of longitude lines
equatorward of SUPPRESS (deg)
.VARI DLO2
The spacing of longitude lines
poleward of SUPPRESS (deg)
.VARI SUPPRESS
Latitude above which DLO2 and 
DLA2 values are used.
.VARI GLATITUD
Specifies latitude lines.
.VARI GLONGITU
Specifies longitude lines.
.VARI PRINT
Prints out grid intersections
(beware--massive printout)
.VARI DN
VALID=WHITE,BLACK
WHITE: Grid lines will be MAXDN
BLACK: Grid lines will be zero DN
.VARI ALTERNAT
Alternates Dn value of grid
between black and white.
.VARI MAXDN
Maximum DN value of
output image.
.VARI SCALEBAR
Adds kilometer per pixel
scale bar to output file
.VARI NONUMBER
Suppresses labeling of
grid intersections (with
lat-lon coordinates).
.VARI EAST
Longitude numbers on output
image will be EAST-longitude
.VARI EXPAND
Magnifies grid annotation
by integer factor
.VARI DLATITUD
DLAT=n causes every nth
intersection along latitude
lines to be labeled.
.VARI DLONGITU
DLON=n causes every nth
intersection along longitude
lines to be labeled.
.vari NOGRID
Suppress lat/long grid
(must specify LIMB or
TERMINAT)
.vari LIMB
draw planet limb?
.VARI LSPACE
spacing of limb points
.VARI TSPACE
spacing of terminator
points
.vari DNLIMB
DN for limb points
.vari TERMINAT
draw terminator?
.vari DNTERM
DN for terminator points
.vari SUBSOL
Subsolar lat/long (for
Terminator)
.vari LAT_TYPE
Planetographic/centric lat's?
.VARI TARGET
Optional 12-char string
Target name (planet,
  satellite, or asteroid)
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


.LEVEL2

.VARI INP
Input image file name.  The input must have a standard VICAR map label.

Any projection which is supported by the map-projection program MAP3 is 
also supported by OVERLAY.

.VARI OUT
Output image file name.  The output image will be the same data format as
the input image.  

.VARI SIZE
	SIZE=(sl,ss,nl,ns)
Specifies the VICAR size field: starting line, starting sample, number of
lines, and number of samples.  The output image will be nl x ns, where
pixel (1,1) corresponds to pixel (sl,ss) of the input.

.VARI SL
Starting line in input 

.VARI SS
Starting sample in input

.VARI NL
Number of lines

.VARI NS
Number of samples

.VARI MISSION
Specifies the mission if the input is not a standard label or if there
is no input image. Defaults to input label mission type.
Valid are: MAR-9 MAR10 VIKOR VGR-1 VGR-2 GLL NOPRO (for no project)

.VARI LIMITS
REAL--OPTIONAL
LIMITS=(r1,r2,r3,r4) specifies the minumum latitude, maximum latitude,
minimum longitude, and maximum longitude boundaries of the grid overlay.
This parameter is primarily used to speed up execution by limiting the
grid calculation to the planet area within the field-of-view of the
picture.

.VARI AUTO
KEYWORD--OPTIONAL
The keyword AUTO causes OVERLAY to automatically determine the latitude-
longitude boundaries of the image (see LIMITS parameter).

.VARI SUPPRESS
SUPP=r specifies the latitude above which DLO2 and DLA2 values are used
(instead of DLO1 and DLA1).  See DLA1, DLO1, DLA2, DLO2 parameters.

.VARI DLA1
REAL--OPTIONAL
DLA1=r specifies the spacing of latitude lines (in deg) equatorward of
SUPPRESS.

.VARI DLA2
REAL--OPTIONAL
DLA2=r specifies the spacing of latitude lines (in deg) poleward of SUPPRESS.
The spacing of latitude lines poleward of SUPPRESS

.VARI DLO1
REAL--OPTIONAL
DLO1=r specifies the spacing of longitude lines (in deg) equatorward of
SUPPRESS.

.VARI DLO2
REAL--OPTIONAL
DLO2=r specifies the spacing of longitude lines (in deg) poleward of SUPPRESS.

.VARI GLATITUD
REAL--OPTIONAL
GLATIDU=(r1,r2,r3,...) specifies that lines of constant latitude are to be
drawn at latitudes r1, r2, r3, etc.  Up to 181 latitudes may be specified.
This turns off the default grid.

.VARI GLONGITU
REAL--OPTIONAL
GLONGITU=(r1,r2,r3,...) specifies that lines of constant longitude are to be
drawn at longitudes r1, r2, r3, etc.  Up to 360 longitudes may be specified.
This turns off the default grid.

.VARI PRINT
KEYWORD--OPTIONAL
Causes the program to print out the following information at each grid
intersection of the image: TBD

.VARI MAXDN
INTEGER--OPTIONAL
MAXD=I3 where I3 is an integer specifying the maximum DN value of a grid
line.  The default is 255 for byte images and 32767 for halfword images.

.VARI DN
KEYWORD--OPTIONAL
Valid values are BLACK and WHITE, where WHITE causes white grid lines to
be drawn (DN=MAXDN), and BLACK causes black grid lines to be drawn (DN=0).
The default is WHITE.

.VARI ALTERNAT
Dn values of grid points are alternated between black and white.
This is not always successful, leading to less than pleasing results.

.VARI SCALEBAR
KEYWORD--OPTIONAL
Adds kilometer per pixel scale bar to output file; value taken from
DATA(7) of MAP label of input image file.

.VARI NONUMBER
KEYWORD--OPTIONAL
Suppresses latitude-longitude annotation of grid.

.VARI DLATITUD
INTEGER--OPTIONAL
DLAT=n annotates every nth grid intersection along latitude lines.

.VARI DLONGITU
DLON=n annotates every nth grid intersection along longitude lines.

.VARI EXPAND
INTEGER--OPTIONAL
EXPAND=n causes the latitude-longitude annotation to be magnified by
a factor of n.

.VARI EAST
KEYWORD--OPTIONAL
Causes the grid to be annotated using east-longitudes.

.VARI START
START=I6 where I6 is an integer specifying the first byte in the
GEOMA dataset which starts the tiepoints. This is the first word
after the TIEPTS keyword. The default is for the program to look
for tiepoints in the dataset or if the input picture is from a
standard project, a value is set.

.vari NOGRID
If this keyword is specified, the latitude/longitude grid is not drawn.
In this case, the user must specify either LIMB or TERMINAT (q.v.) (or
both), else the program will have nothing to do and quits.

.vari LIMB
This keyword specifies that the planet limb should be drawn in the image.
This option is only available in perspective map projection.

.VARI LSPACE
This specifies the approximate spacing of limb points (in pixels).
Setting it much below 1 should guarantee a solid limb.  Setting it to a
value greater than 1 should cause a dotted limb to be generated, although
this cannot be guaranteed to be regular.

Increasing the spacing will also reduce the number of limb points needed,
in case this proves to be a limiting factor.

.VARI TSPACE
This specifies the approximate spacing of terminator points. 
If TSPACE=1, then the spacing will be the same as that of the limb points
(see parameter LSPACE).  A larger value of TSPACE increase the spacing by
a corresponding factor, .e.g., TSPACE=3 means that every third point will
be used.  (It is not possible to reduce the spacing to less than that of
LSPACE.)

.vari DNLIMB
This specifies the DN to be used for limb points if 'LIMB has been
specified.

The default value in the PDF is 128, which is suitable for Byte images.
If the image is Halfword, then a value in the range (0,32767) should be
supplied.

.vari TERMINAT
This keyword specifies that the terminator (dividing line between the
illuninated and shadow regions) should be drawn in the image.
This option is only available in perspective map projection.
Also, this option requires that LIMB also be specified.

The default value in the PDF is 64, which is suitable for Byte images.
If the image is Halfword, then a value in the range (0,32767) should be
supplied.

.vari DNTERM
This specifies the DN to be used for terminator points if 'TERMINAT has 
been specified.

.vari SUBSOL
  SUBSOL = (latitude, longitude)

This parameter specifies the subsolar latitude & longitude to be used in
the computation of the Terminator (if the TERMINAT keyword has been
selected).  It is needed only for images for which no navigation data are 
available in the label.  

.var LAT_TYPE
 For planets that are not modelled as perfect spheres, this keyword
 controls the type of latitudes output by the program:

 LAT_TYPE = PCENTRIC specifes that latitudes are planetocentric, i.e.,
 determined by the vector from the planet center.

 LAT_TYPE = PGRAPHIC or PDETIC specify planetographic latitudes, which
 are measured with reference to the surface normal.

 The default is PCENTRIC, since this is the VICAR standard.

.VARI TARGET
Ex: TARGET=GANYMEDE specifies that GANYMEDE is the target in the input image.

The TARGET may be a planet, satellite, or asteroid.  If defaulted, the target
name is extracted from the VICAR label or determined by other TBD means.

A complete list of valid target names is located in the ASCII file assigned
the logical name (or environmental variable) BODY_IDS.

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

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstoverlay.pdf
PROCEDURE
REFGBL $ECHO
BODY
LET $ECHO="YES"
let _onfail="continue"
refgbl $syschar

if ($syschar(1) = "UNIX")	

  ush cp /project/it/testdata/mipl/vgr/f1636832.geo geo
  ush cp /project/it/testdata/mipl/gll/venus.img venus

else				! on Alpha:

  DCL ASS WMS_TEST_WORK:[TESTDATA.MIPL.VGR]f1636832.geo geo
  DCL ASS WMS_TEST_WORK:[TESTDATA.MIPL.GLL]venus.img venus
!  dcl set def v2$scratch
  dcl set def scx

end-if

!  prepare the map-projected images for OVERLAY to place overlay grids on
!  images of IO
FIT geo a.img  'BYTE EXCLUDE=(-32768,0) PERC=1.	! skip if no MAP3
MAP3 a.img mapo.img NL=500 NS=500 'POLE 'ORTH +
   SCAL=6 LONG=150 LINE=250 SAMP=250 'remote target=IO
MAP3 a.img maps.img NL=500 NS=500 'STER LATI=40 LONG=150 SCAL=6 'remote +
 target=IO
MAP3 a.img tmerc.img nl=500 ns=500 scale=10. 'tmercato 'recenter +
  latitude=20. longitud=180. plat=-30. plong=150. 'remote target=IO
map3 a.img out=perspective.img nl=500 ns=500 scale=10. 'perspect +
  north=45. latitude=80. longitud=150. 'RECENTER 'remote target=IO
! this case caused OVERLAY error fixed on 2003.01.13:
map3 a.img obcy.img NL=500 NS=500 'OBCY SCALE=10. 'remote +
     'RECENT target=io
!  for GLL:
map3 venus out=venus.map 'perspec 'remote target=VENUS

!Test Polar Orthographic projection...
OVERLAY  mapo.img  mapo.ovly DLO1=30. DLO2=60. DLA1=30. DLA2=10.
LIST mapo.ovly inc=50

!Test Stereographic projection...
OVERLAY  maps.img  maps.ovly  DLO1=30. DLO2=60. +
   DLA1=30. DLA2=10. DLAT=2 EXPAN=2
LIST maps.ovly inc=50

! Transverse Mercator projection recenter
overlay tmerc.img tmerc.ovly
list tmerc.ovly inc=100

! Test Perspective projection
overlay perspective.img perspective.ovly
list perspective.ovly inc=100 

! Test Oblique simple cylindrica projection
overlay obcy.img obcy.ovly
list obcy.ovly inc=100 

!  Test Limb option
overlay perspective.img perspective1.ovly 'limb 
list perspective1.ovly inc=100

!  add Terminator, without grid
overlay perspective.img perspective2.ovly 'limb 'term dnterm=255 'nogrid +
 target=IO
list perspective2.ovly inc=100 

! Test for GLL
overlay inp=venus.map out=venus.ovly 
list venus.ovly inc=60

! Test limb/terminator for GLL & test CKNAME
! (specify NAIF because of problem with corrupted MIPS CK on Unix server)
overlay inp=venus.map out=venus.ovly 'limb dnlimb=6000 'term dnterm=3000 +
 CKNAME=naif
list venus.ovly inc=60

WRITE "SOME OF THE LISTINGS MAY CHANGE BECAUSE THE SPICE/SEDR DATA"
WRITE "WERE UPDATED IN THE DATABASE ... IN THIS CASE, CHECK THE *.OVLY"
WRITE "IMAGES VISUALLY TO MAKE SURE THAT THE GRIDS ARE CORRECT."

!
! Test 3D images
!
WRITE "Test on 3D images"
gen x3d 500 500 3 SINC=0 LINC=0 BINC=0
!labswtch (mapo.img x3d) maps.cub
!labswtch doesnt do 3D, swapped with label-switch (ntt - 11.16.03)
label-switch (mapo.img x3d) maps.cub
insert3d (maps.cub mapo.img) band=1 'UPDATE
insert3d (maps.cub mapo.img) band=2 'UPDATE
insert3d (maps.cub mapo.img) band=3 'UPDATE
overlay inp=maps.cub out=mapscub.ovly 'SCALEBAR
LIST mapscub.ovly inc=50

! clean up
if ($syschar(1) = "UNIX")	
  ush rm geo
  ush rm venus
  ush rm x3d
  ush rm maps.cub
end-if

END-PROC
$ Return
$!#############################################################################
$Imake_File:
$ create overlay.imake
#define PROGRAM overlay
#define MODULE_LIST overlay.f 
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
/*#define DEBUG			/* remove on delivery */
/*#define LIB_LOCAL		/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstoverlay.log_solos
tstoverlay
let _onfail="continue"
refgbl $syschar
if ($syschar(1) = "UNIX")
  ush cp /project/it/testdata/mipl/vgr/f1636832.geo geo
  ush cp /project/it/testdata/mipl/gll/venus.img venus
else
end-if
FIT geo a.img  'BYTE EXCLUDE=(-32768,0) PERC=1.
Beginning VICAR task FIT

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   964.247 STANDARD DEVIATION=  1762.497 NUMBER OF ELEMENTS= 1000000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=  1848.165 STANDARD DEVIATION=  2055.618 NUMBER OF ELEMENTS=  525097

MINIMUM DN OF      1   SCALED TO     0

MAXIMUM DN OF   5746   SCALED TO   255
FIT task completed
MAP3 a.img mapo.img NL=500 NS=500 'POLE 'ORTH  +
   SCAL=6 LONG=150 LINE=250 SAMP=250 'remote target=IO
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS POLAR ORTHOGRAPHIC
 DATA=
            2.500E+02  2.500E+02 -9.000E+01  0.000E+00  0.000E+00  1.500E+02  6.000E+00 -1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 a.img maps.img NL=500 NS=500 'STER LATI=40 LONG=150 SCAL=6 'remote  +
 target=IO
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS OBLIQ STEREOGRAPHIC
 DATA=
            2.500E+02  2.500E+02  4.000E+01  0.000E+00  0.000E+00  1.500E+02  6.000E+00  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 a.img tmerc.img nl=500 ns=500 scale=10. 'tmercato 'recenter  +
  latitude=20. longitud=180. plat=-30. plong=150. 'remote target=IO
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS TRANSVERSE MERCATOR
 DATA=
            1.655E+02  7.914E+01  2.000E+01  0.000E+00  0.000E+00  1.800E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
map3 a.img out=perspective.img nl=500 ns=500 scale=10. 'perspect  +
  north=45. latitude=80. longitud=150. 'RECENTER 'remote target=IO
Beginning VICAR task map3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
Projection is Perspective.
Projection is Perspective.
 DATA(25-38)=
            1.816E+03  1.824E+03  1.500E+03  5.000E+02  5.000E+02  8.482E+01  8.000E+01  1.500E+02  5.000E+02  5.000E+02
            4.500E+01  0.000E+00  0.000E+00  1.274E+06
map3 a.img obcy.img NL=500 NS=500 'OBCY SCALE=10. 'remote  +
     'RECENT target=io
Beginning VICAR task map3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
Center of projection before oblique rotation:
Latitude =  -13.086061
Longitude=  176.22607
Center of projection after oblique rotation:
Latitude =  -13.086061
Longitude=  176.22607
OBLIQUE SIMPLE CYLINDRICAL PROJECTION
 DATA=
            2.380E+02  2.082E+02  9.000E+01  0.000E+00  0.000E+00  0.000E+00  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
map3 venus out=venus.map 'perspec 'remote target=VENUS
Beginning VICAR task map3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS IMAGE SPACE
    CAMERA=          1
GETLABCON: warning ind=          1
flight label
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2447935.7
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/  98493.4/ 6137.000/ 6137.000/ 6137.000/1629971.9/   -2.954/  176.320/
    OM MATRIX
/-0.064721/ 0.997582/ 0.025331
/-0.052389/-0.028746/ 0.998213
/ 0.996527/ 0.063278/ 0.054123
    RS VECTOR (TARGET COORDINATES)
      -1624449.2       -104477.9        -84007.7
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =  40.43, LONGITUDE = 159.89
Projection is Perspective.
 DATA(25-38)=
            6.137E+03  6.137E+03  1.501E+03  4.000E+02  4.000E+02  6.562E+01 -2.954E+00  1.763E+02  4.000E+02  4.000E+02
            1.865E+02  0.000E+00  0.000E+00  1.630E+06
OVERLAY  mapo.img  mapo.ovly DLO1=30. DLO2=60. DLA1=30. DLA2=10.
Beginning VICAR task OVERLAY
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.18791662
 Number of points used =        5647
 OVERLAY task completed
LIST mapo.ovly inc=50
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:48 2004
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 250 189 207 145 140 127  96 108 137
    101     253 220 252 153 173 135 137  82  78  51
    151     240 231 249 181 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0
OVERLAY  maps.img  maps.ovly  DLO1=30. DLO2=60.  +
   DLA1=30. DLA2=10. DLAT=2 EXPAN=2
Beginning VICAR task OVERLAY
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.18791662
 Number of points used =        4084
 OVERLAY task completed
LIST maps.ovly inc=50
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:48 2004
     Samp     1     101     201     301     401
   Line
      1     125 113 109  89  69  77  71  30  12   0
     51     222 188 112 183 155 117 117  92  52  21
    101     173 144 149 145 121  96  67  80 102 255
    151     204 184 146 177 125 103 122 116  48  25
    201     214 197 173 123 163 126 128 125 122  40
    251     232 190 187 168 192 166 158 142 103  55
    301     180 187 182 202 210 192 180 141  96  33
    351     180 255 255 234 240 233 222 170 118  64
    401     197 185 231 252 255 179 189 178 134  68
    451     205 155 172 244 215 204 204 191 136 255
overlay tmerc.img tmerc.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31319436
 Number of points used =        9562
 OVERLAY task completed
list tmerc.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:49 2004
     Samp     1     201     401
   Line
      1     130 173 137 161 209
    101     142 111 191 232 228
    201     238 255 231 180 160
    301     175 252 146 169 117
    401       0   0 110 157  72
overlay perspective.img perspective.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31405920
 Number of points used =        2085
 OVERLAY task completed
list perspective.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:49 2004
     Samp     1     201     401
   Line

    401       0   0   0   0 101
overlay obcy.img obcy.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31319436
 Number of points used =        9729
 OVERLAY task completed
list obcy.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:50 2004
     Samp     1     201     401
   Line
      1       0  31  64 124 132
    101       0 250 152 158 212
    201       0 144 184 193 242
    301       0 245 224 171 135
    401       0 191 170 141 123
overlay perspective.img perspective1.ovly 'limb
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31405920
 Number of points used =        2085
 Number of limb points =         448
 OVERLAY task completed
list perspective1.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:50 2004
     Samp     1     201     401
   Line

    401       0   0   0   0 101
overlay perspective.img perspective2.ovly 'limb 'term dnterm=255 'nogrid  +
 target=IO
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31405920
Project in label is: VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Subsolar lat/long =
            5.413E-01  1.713E+02
 Number of limb points =         448
 Number of terminator points =         312
 OVERLAY task completed
list perspective2.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:50 2004
     Samp     1     201     401
   Line

    401       0   0   0   0 101
overlay inp=venus.map out=venus.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in halfword data format
 Scale(deg/pixel) =  0.15392223
 PREPSUB unable to find lat/long for annotation
 PREPSUB unable to find lat/long for annotation
 Number of points used =       16668
 OVERLAY task completed
list venus.ovly inc=60
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:RTO313    Date_Time:Thu May 24 14:30:43 1990
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:52 2004
     Samp       1    61   121   181   241   301   361   421   481   541   601   661   721   781
   Line

    181         0     0 32767     0     0     0     0     0     0     0     0     0     0     0
    241         0     0  4077  4428  4666     0     0     0     0     0     0     0     0     0
    301         0  3631  4266  4221  3815  3497  3242  2858  2161  1485   759    45 32767     0
    361         0  3888  4051  4028  3922  3775  3346  2752  2235  1591   790    70    46     0
    421         0  3989  4457  4464  3872  3636  3367  2762  2225  1528   788    44    44     0
    481         0  3793  4333  4600  4235  3603  3234  2625  1964  1345   611    43     0     0
    541         0  2836  4287  4277  4010  3682  2842  2392  1734  1090   340    43     0     0
    601         0     0-32768  4289  4140  3375  2895  2109  1505   772    72    43     0     0
    661         0     0     0-32768-32768-32768-32768-32768-32768-32768    43    43     0     0
    721         0     0     0     0-32768 32767-32768-32768-32768-32768     0     0     0     0
overlay inp=venus.map out=venus.ovly 'limb dnlimb=6000 'term dnterm=3000  +
 CKNAME=naif
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in halfword data format
 Scale(deg/pixel) =  0.15392223
Project in label is: GLL
Subsolar lat/long =
           -2.579E+00  1.295E+02
 PREPSUB unable to find lat/long for annotation
 PREPSUB unable to find lat/long for annotation
 Number of points used =       16668
 Number of limb points =        3826
 Number of terminator points =        1910
 OVERLAY task completed
list venus.ovly inc=60
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:RTO313    Date_Time:Thu May 24 14:30:43 1990
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:52 2004
     Samp       1    61   121   181   241   301   361   421   481   541   601   661   721   781
   Line

    181         0     0 32767     0     0     0     0     0     0     0     0     0     0     0
    241         0     0  4077  4428  4666     0     0     0     0     0     0     0     0     0
    301         0  3631  4266  4221  3815  3497  3242  2858  2161  1485   759    45 32767     0
    361         0  3888  4051  4028  3922  3775  3346  2752  2235  1591   790    70    46     0
    421         0  3989  4457  4464  3872  3636  3367  2762  2225  1528   788    44    44     0
    481         0  3793  4333  4600  4235  3603  3234  2625  1964  1345   611    43     0     0
    541         0  2836  4287  4277  4010  3682  2842  2392  1734  1090   340    43     0     0
    601         0     0-32768  4289  4140  3375  2895  2109  1505   772  3000    43     0     0
    661         0     0     0-32768-32768-32768-32768-32768-32768-32768    43    43     0     0
    721         0     0     0     0-32768 32767-32768-32768-32768-32768     0     0     0     0
WRITE "SOME OF THE LISTINGS MAY CHANGE BECAUSE THE SPICE/SEDR DATA"
SOME OF THE LISTINGS MAY CHANGE BECAUSE THE SPICE/SEDR DATA
WRITE "WERE UPDATED IN THE DATABASE ... IN THIS CASE, CHECK THE *.OVLY"
WERE UPDATED IN THE DATABASE ... IN THIS CASE, CHECK THE *.OVLY
WRITE "IMAGES VISUALLY TO MAKE SURE THAT THE GRIDS ARE CORRECT."
IMAGES VISUALLY TO MAKE SURE THAT THE GRIDS ARE CORRECT.
WRITE "Test on 3D images"
Test on 3D images
gen x3d 500 500 3 SINC=0 LINC=0 BINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-switch (mapo.img x3d) maps.cub
Beginning VICAR task label
insert3d (maps.cub mapo.img) band=1 'UPDATE
Beginning VICAR task insert3d
INSERT3D version 2-May-1994
insert3d (maps.cub mapo.img) band=2 'UPDATE
Beginning VICAR task insert3d
INSERT3D version 2-May-1994
insert3d (maps.cub mapo.img) band=3 'UPDATE
Beginning VICAR task insert3d
INSERT3D version 2-May-1994
overlay inp=maps.cub out=mapscub.ovly 'SCALEBAR
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.18791662
 Number of points used =       10342
 OVERLAY task completed
LIST mapscub.ovly inc=50
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:56 2004
 ***********
 Band =     1
 ***********
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 255 189 207 145 140 127  96 108 137
    101     253 220 255 153 173 135 137  82  78  51
    151     240 231 249 255 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0

    351       0   0   0   0   0   0   0 255   0   0
    401       0   0   0   0   0   0   0   0 255   0
    451       0   0   0   0   0   0   0   0   0 255


 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:56 2004
 ***********
 Band =     2
 ***********
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 255 189 207 145 140 127  96 108 137
    101     253 220 255 153 173 135 137  82  78  51
    151     240 231 249 255 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0

    351       0   0   0   0   0   0   0 255   0   0
    401       0   0   0   0   0   0   0   0 255   0
    451       0   0   0   0   0   0   0   0   0 255


 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:49:56 2004
 ***********
 Band =     3
 ***********
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 255 189 207 145 140 127  96 108 137
    101     253 220 255 153 173 135 137  82  78  51
    151     240 231 249 255 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0

    351       0   0   0   0   0   0   0 255   0   0
    401       0   0   0   0   0   0   0   0 255   0
    451       0   0   0   0   0   0   0   0   0 255
if ($syschar(1) = "UNIX")
  ush rm geo
  ush rm venus
  ush rm x3d
  ush rm maps.cub
end-if
END-PROC
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
$!-----------------------------------------------------------------------------
$ create tstoverlay.log_linux
tstoverlay
let _onfail="continue"
refgbl $syschar
if ($syschar(1) = "UNIX")
  ush cp /project/it/testdata/mipl/vgr/f1636832.geo geo
  ush cp /project/it/testdata/mipl/gll/venus.img venus
else
end-if
FIT geo a.img  'BYTE EXCLUDE=(-32768,0) PERC=1.
Beginning VICAR task FIT

FIT version 5 August, 2003

     RAW HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=   964.247 STANDARD DEVIATION=  1762.497 NUMBER OF ELEMENTS= 1000000

EXCLUDED HISTOGRAM STATISTICS...
AVERAGE GRAY LEVEL=  1848.165 STANDARD DEVIATION=  2055.618 NUMBER OF ELEMENTS=  525097

MINIMUM DN OF      1   SCALED TO     0

MAXIMUM DN OF   5746   SCALED TO   255
FIT task completed
MAP3 a.img mapo.img NL=500 NS=500 'POLE 'ORTH  +
   SCAL=6 LONG=150 LINE=250 SAMP=250 'remote target=IO
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS POLAR ORTHOGRAPHIC
 DATA=
            2.500E+02  2.500E+02 -9.000E+01  0.000E+00  0.000E+00  1.500E+02  6.000E+00 -1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 a.img maps.img NL=500 NS=500 'STER LATI=40 LONG=150 SCAL=6 'remote  +
 target=IO
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS OBLIQ STEREOGRAPHIC
 DATA=
            2.500E+02  2.500E+02  4.000E+01  0.000E+00  0.000E+00  1.500E+02  6.000E+00  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
MAP3 a.img tmerc.img nl=500 ns=500 scale=10. 'tmercato 'recenter  +
  latitude=20. longitud=180. plat=-30. plong=150. 'remote target=IO
Beginning VICAR task MAP3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
PROJECTION IS TRANSVERSE MERCATOR
 DATA=
            1.655E+02  7.914E+01  2.000E+01  0.000E+00  0.000E+00  1.800E+02  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
map3 a.img out=perspective.img nl=500 ns=500 scale=10. 'perspect  +
  north=45. latitude=80. longitud=150. 'RECENTER 'remote target=IO
Beginning VICAR task map3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
Projection is Perspective.
Projection is Perspective.
 DATA(25-38)=
            1.816E+03  1.824E+03  1.500E+03  5.000E+02  5.000E+02  8.482E+01  8.000E+01  1.500E+02  5.000E+02  5.000E+02
            4.500E+01  0.000E+00  0.000E+00  1.274E+06
map3 a.img obcy.img NL=500 NS=500 'OBCY SCALE=10. 'remote  +
     'RECENT target=io
Beginning VICAR task map3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS OBJECT SPACE
    CAMERA=          7
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
GETLABCON: warning ind=          1
flight label
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2443937.3
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/ 127248.2/ 1829.400/ 1819.300/ 1815.700/ 806029.9/   -0.032/  156.474/
    OM MATRIX
/ 0.225295/-0.516547/ 0.826088
/ 0.330342/-0.757163/-0.563541
/ 0.916579/ 0.399854/ 0.000052
    RS VECTOR (TARGET COORDINATES)
       -739030.7       -321741.5          -456.1
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE = -13.09, LONGITUDE = 176.23
Center of projection before oblique rotation:
Latitude =  -13.086059
Longitude=  176.22612
Center of projection after oblique rotation:
Latitude =  -13.086059
Longitude=  176.22612
OBLIQUE SIMPLE CYLINDRICAL PROJECTION
 DATA=
            2.380E+02  2.082E+02  9.000E+01  0.000E+00  0.000E+00  0.000E+00  1.000E+01  1.000E+00  5.570E+01  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  1.816E+03  1.829E+03  0.000E+00  0.000E+00  0.000E+00  0.000E+00
            0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00  0.000E+00
map3 venus out=venus.map 'perspec 'remote target=VENUS
Beginning VICAR task map3
Map3 version 6-Dec-1999
 LABEL SAYS INPUT PICTURE IS IMAGE SPACE
    CAMERA=          1
GETLABCON: warning ind=          1
flight label
Actual source of SEDR is: DAVI
OM matrix obtained from SEDR
RS vector obtained from SEDR
Target radii obtained from SEDR
 JULIAN DATE OF EVENT FRAME=  2447935.7
/CAM FOC L/    RA   /    RB   /    RC   /   RMAG  /LA SSC PT/LO SSC PT/
/  98493.4/ 6137.000/ 6137.000/ 6137.000/1629971.9/   -2.954/  176.320/
    OM MATRIX
/-0.064721/ 0.997582/ 0.025331
/-0.052396/-0.028746/ 0.998213
/ 0.996527/ 0.063278/ 0.054130
    RS VECTOR (TARGET COORDINATES)
      -1624449.2       -104477.9        -84007.7
COORDINATES OF CENTER OF INPUT PICTURE--LATITUDE =  40.58, LONGITUDE = 159.85
Projection is Perspective.
 DATA(25-38)=
            6.137E+03  6.137E+03  1.501E+03  4.000E+02  4.000E+02  6.562E+01 -2.954E+00  1.763E+02  4.000E+02  4.000E+02
            1.866E+02  0.000E+00  0.000E+00  1.630E+06
OVERLAY  mapo.img  mapo.ovly DLO1=30. DLO2=60. DLA1=30. DLA2=10.
Beginning VICAR task OVERLAY
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.18791662
 Number of points used =        5647
 OVERLAY task completed
LIST mapo.ovly inc=50
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:37 2004
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 250 189 207 145 140 127  96 108 137
    101     253 220 252 153 173 135 137  82  78  51
    151     240 231 249 181 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0
OVERLAY  maps.img  maps.ovly  DLO1=30. DLO2=60.  +
   DLA1=30. DLA2=10. DLAT=2 EXPAN=2
Beginning VICAR task OVERLAY
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.18791662
 Number of points used =        4084
 OVERLAY task completed
LIST maps.ovly inc=50
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:37 2004
     Samp     1     101     201     301     401
   Line
      1     125 113 109  89  69  77  71  30  12   0
     51     222 188 112 183 155 117 117  92  52  21
    101     173 144 149 145 121  96  67  80 102 255
    151     204 184 146 177 125 103 122 116  48  25
    201     214 197 173 123 163 126 128 125 122  40
    251     232 190 187 168 192 166 158 142 103  55
    301     180 187 182 202 210 192 180 141  96  33
    351     180 255 255 234 240 233 222 170 118  64
    401     197 185 231 252 255 179 189 178 134  68
    451     205 155 172 244 215 204 204 191 136 255
overlay tmerc.img tmerc.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31319436
 Number of points used =        9562
 OVERLAY task completed
list tmerc.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:37 2004
     Samp     1     201     401
   Line
      1     130 173 137 161 209
    101     142 111 191 232 228
    201     238 255 231 180 160
    301     175 252 146 169 117
    401       0   0 110 157  72
overlay perspective.img perspective.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31405920
 Number of points used =        2085
 OVERLAY task completed
list perspective.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:37 2004
     Samp     1     201     401
   Line

    401       0   0   0   0 101
overlay obcy.img obcy.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31319436
 Number of points used =        9730
 OVERLAY task completed
list obcy.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:37 2004
     Samp     1     201     401
   Line
      1       0  31  64 124 132
    101       0 250 152 158 212
    201       0 144 184 193 242
    301       0 245 224 171 135
    401       0 191 170 141 123
overlay perspective.img perspective1.ovly 'limb
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31405920
 Number of points used =        2085
 Number of limb points =         448
 OVERLAY task completed
list perspective1.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:38 2004
     Samp     1     201     401
   Line

    401       0   0   0   0 101
overlay perspective.img perspective2.ovly 'limb 'term dnterm=255 'nogrid  +
 target=IO
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.31405920
Project in label is: VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Subsolar lat/long =
            5.413E-01  1.713E+02
 Number of limb points =         448
 Number of terminator points =         312
 OVERLAY task completed
list perspective2.ovly inc=100
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:38 2004
     Samp     1     201     401
   Line

    401       0   0   0   0 101
overlay inp=venus.map out=venus.ovly
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in halfword data format
 Scale(deg/pixel) =  0.15392224
 PREPSUB unable to find lat/long for annotation
 PREPSUB unable to find lat/long for annotation
 Number of points used =       16659
 OVERLAY task completed
list venus.ovly inc=60
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:RTO313    Date_Time:Thu May 24 14:30:43 1990
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:39 2004
     Samp       1    61   121   181   241   301   361   421   481   541   601   661   721   781
   Line

    241         0     0  4089  4432  4671     0     0     0     0     0     0     0     0     0
    301         0  3623  4269  4223 32767  3542  3217  2837  2165  1473   798    46 32767     0
    361         0  3889  4044  4017  3949  3759  3348  2753  2238  1604   787    51    46     0
    421         0  3984  4464  4452  3876  3625  3373  2716  2233  1542   769    44    44     0
    481         0  3799  4327  4589  4205  3602  3220  2628  1972  1333   610    43     0     0
    541         0  2881  4299  4280  3996  3643  2840  2378  1757  1101   360    43 32767     0
    601         0     0-32768  4273  4123  3384  2919  2092  1514   768    72    44     0     0
    661         0     0     0-32768-32768-32768-32768-32768-32768-32768    43    43     0     0
    721         0     0     0     0-32768 32767-32768-32768-32768-32768     0     0     0     0
overlay inp=venus.map out=venus.ovly 'limb dnlimb=6000 'term dnterm=3000  +
 CKNAME=naif
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in halfword data format
 Scale(deg/pixel) =  0.15392224
Project in label is: GLL
Subsolar lat/long =
           -2.579E+00  1.295E+02
 PREPSUB unable to find lat/long for annotation
 PREPSUB unable to find lat/long for annotation
 Number of points used =       16659
 Number of limb points =        3826
 Number of terminator points =        1910
 OVERLAY task completed
list venus.ovly inc=60
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:RTO313    Date_Time:Thu May 24 14:30:43 1990
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:39 2004
     Samp       1    61   121   181   241   301   361   421   481   541   601   661   721   781
   Line

    241         0     0  4089  4432  4671     0     0     0     0     0     0     0     0     0
    301         0  3623  4269  4223 32767  3542  3217  2837  2165  1473   798    46 32767     0
    361         0  3889  4044  4017  3949  3759  3348  2753  2238  1604   787    51    46     0
    421         0  3984  4464  4452  3876  3625  3373  2716  2233  1542   769    44    44     0
    481         0  3799  4327  4589  4205  3602  3220  2628  1972  1333   610    43     0     0
    541         0  2881  4299  4280  3996  3643  2840  2378  1757  1101   360    43 32767     0
    601         0     0-32768  4273  4123  3384  2919  2092  1514   768  3000    44     0     0
    661         0     0     0-32768-32768-32768-32768-32768-32768-32768    43    43     0     0
    721         0     0     0     0-32768 32767-32768-32768-32768-32768     0     0     0     0
WRITE "SOME OF THE LISTINGS MAY CHANGE BECAUSE THE SPICE/SEDR DATA"
SOME OF THE LISTINGS MAY CHANGE BECAUSE THE SPICE/SEDR DATA
WRITE "WERE UPDATED IN THE DATABASE ... IN THIS CASE, CHECK THE *.OVLY"
WERE UPDATED IN THE DATABASE ... IN THIS CASE, CHECK THE *.OVLY
WRITE "IMAGES VISUALLY TO MAKE SURE THAT THE GRIDS ARE CORRECT."
IMAGES VISUALLY TO MAKE SURE THAT THE GRIDS ARE CORRECT.
WRITE "Test on 3D images"
Test on 3D images
gen x3d 500 500 3 SINC=0 LINC=0 BINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
label-switch (mapo.img x3d) maps.cub
Beginning VICAR task label
insert3d (maps.cub mapo.img) band=1 'UPDATE
Beginning VICAR task insert3d
INSERT3D version 2-May-1994
insert3d (maps.cub mapo.img) band=2 'UPDATE
Beginning VICAR task insert3d
INSERT3D version 2-May-1994
insert3d (maps.cub mapo.img) band=3 'UPDATE
Beginning VICAR task insert3d
INSERT3D version 2-May-1994
overlay inp=maps.cub out=mapscub.ovly 'SCALEBAR
Beginning VICAR task overlay
*** Program OVERLAY version 23-Jun-2004 ***
 Input is in byte data format
 Scale(deg/pixel) =  0.18791662
 Number of points used =       10342
 OVERLAY task completed
LIST mapscub.ovly inc=50
Beginning VICAR task LIST

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:42 2004
 ***********
 Band =     1
 ***********
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 255 189 207 145 140 127  96 108 137
    101     253 220 255 153 173 135 137  82  78  51
    151     240 231 249 255 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0

    351       0   0   0   0   0   0   0 255   0   0
    401       0   0   0   0   0   0   0   0 255   0
    451       0   0   0   0   0   0   0   0   0 255


 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:42 2004
 ***********
 Band =     2
 ***********
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 255 189 207 145 140 127  96 108 137
    101     253 220 255 153 173 135 137  82  78  51
    151     240 231 249 255 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0

    351       0   0   0   0   0   0   0 255   0   0
    401       0   0   0   0   0   0   0   0 255   0
    451       0   0   0   0   0   0   0   0   0 255


 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:15:53 1984
 Task:OVERLAY   User:lwk       Date_Time:Thu Jun 24 08:48:42 2004
 ***********
 Band =     3
 ***********
     Samp     1     101     201     301     401
   Line
      1       0   0 196 204 162 163 188 144 162   0
     51       0 255 189 207 145 140 127  96 108 137
    101     253 220 255 153 173 135 137  82  78  51
    151     240 231 249 255 137 143 101 126 144  80
    201     221 205 205 152 124 140  77  84  62  49
    251     173 107  91  66  62   0   0   0   0   0

    351       0   0   0   0   0   0   0 255   0   0
    401       0   0   0   0   0   0   0   0 255   0
    451       0   0   0   0   0   0   0   0   0 255
if ($syschar(1) = "UNIX")
  ush rm geo
  ush rm venus
  ush rm x3d
  ush rm maps.cub
end-if
END-PROC
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
$ Return
$!#############################################################################
