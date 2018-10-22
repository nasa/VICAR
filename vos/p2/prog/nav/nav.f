      INCLUDE 'VICMAIN_FOR'
C NAV - Image Navigation Program
C User guide is in NAV.PDF
C Programmer's guide may be extracted from COM file by doing:
C	$@NAV DOC
C which creates the file NAV.DOC 
C
      SUBROUTINE MAIN44
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/SEDR/SEDR(200),LBUF(100),ISYSTEM
      REAL*4 SEDR
      INTEGER LBUF

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT
      COMMON/CPAR/PAR(20),PAR2(20)
      REAL*4 PAR,PAR2
      INTEGER*4 IPAR(20)
      EQUIVALENCE (PAR,IPAR)
      LOGICAL XVIPTST

      COMMON/C1/PIC(1200,1200)
      BYTE PIC

      COMMON/C1H/HPIC(1200,1200)
      INTEGER*2 HPIC

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEM
      REAL*4 RES,BLEM

      COMMON/CLIMB/ISL,ISS,INL,INS
      COMMON/CLIMB/NPTS,LPTS(2,3000),ALPTS(2,3000)
      REAL*4 LPTS,ALPTS

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

      COMMON/COE/OE(110,4),OEF,OEFNAME	!Orbital elements for J,S,U,N
      INTEGER*4 OEF
      CHARACTER*256 OEFNAME

      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      COMMON/CSTAR1/ISAO,SAOFILE
      COMMON/CSTAR2/STARNAME,STARTYPE
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG
      CHARACTER*72 SAOFILE
      CHARACTER*13 STARNAME(2000)
      CHARACTER*3 STARTYPE(2000)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/CHIST/HIS(256),HFLG,HBEG,HINC,NSPIKES,LHPER(2)
      INTEGER HIS,HFLG,HBEG,HINC
      REAL*4 LHPER

      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

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

      CHARACTER*5 FORMAT
      CHARACTER*12 tname

      CALL XVMESSAGE('NAV version 2016-02-18',' ')
c  (latest change was in subr. SPICESUB, to add INIT_SPICE before PBDATA)
      CALL DEVICE(ind)		!Initialize display device
      IF (IND.EQ.0) GOTO 999
      PI = 3.141592653589793D0
      DTR = PI/180.D0
      RTD = 180.D0/PI
      IBUG = 0
      HFLG = 0
      NLW = 11
      NSW = 3
      NSPIKES = 3
      NSEARCH = 9
      IFIT = 2
      NRINGS = 0
      ISAO = -1
      OEF = -1
C     ....Open input image
      CALL XVUNIT(img,'INP',1,ind,' ')
      CALL XVOPEN(IMG,ind,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IMG,ind,'NL',nl,'NS',ns,'FORMAT',format,' ')
      IF (NL*NS.GT.1440000) THEN
	CALL XVMESSAGE('***Input image too large',' ')
	GO TO 999
      ENDIF
      ICODE = 0
      IF (FORMAT.EQ.'BYTE') THEN
	ICODE=1
      ELSEIF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') THEN
	ICODE=2
      ELSE
	CALL XVMESSAGE('** NAV supports byte or halfword data only **',
     1   ' ')
	GO TO 999
      ENDIF

C     ....Read the input image into memory
      LHPER(1) = 0.5
      LHPER(2) = 0.5
      CALL READIMAGE(IMG,HPIC,NL,NS,LHPER(1),LHPER(2),icode,pic,
     & his,hflg,hbeg,hinc)
C     ....Display the entire image
      CALL HOME(PIC,NL,NS,NLDS,NSDS,sl,ss,izoom,zoom)
      CALL MVE(4,4,SL,isl,1,1)
      CALL MVE(4,4,SL,jsl,1,1)
C
C     ....Get project, frame and camera ID, and GETLABCON buffer
      CALL FRAMEID(IMG,project,lbuf,frame_id,icam,iproj,
     &			isystem,igeo,ind)
      IF (IND.EQ.0) GOTO 999
C     ....Determine image type
      CALL MAPLABEL(IMG,IPROJ,itype,*999)	!7=image-space, 8=object-space
C     ....Get target name
      CALL XVPARM('TARGET',tname,icnt,idef,' ')
      IF (IDEF.NE.1) THEN      
         CALL UPRCASE(tname)
         CALL PBID(tname,target_id,*999)
      ELSE
         TARGET_ID = 0
      ENDIF
C     ....Get camera pointing from SPICE kernels
      CALL SPICESUB(IMG,PROJECT,LBUF,NL,NS,sedr,ind)
      IF(IND.NE.0) GOTO 999
      CALL GETANGLES(CM,ME(1,3),PSC,angln,angla,anglb)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,om) !Compute OM-matrix
      CALL PUTNAV		!Save the planet
      CALL SAVESEDR		!Save SPICE pointing
      CALL SAVELAST
C
C     ....Get reseau locations and camera distortion correction parameters
      CALL GETRESLOC(ITYPE,IPROJ,ICAM,res,nres,*999)
      IF (ITYPE.EQ.7) CALL GETGEOPAR(RES,IPROJ,ICAM,OAL,OAS)

      IF (IPROJ.EQ.4) THEN	!Only Voyager blemishes handled for now
	 CALL VGRBLEMS(ITYPE,ICAM,blem,nblem,*20)
      ELSE
	 NBLEM = 0
      ENDIF
C
   20 CALL XVMESSAGE('Specify feature to be fitted',' ')
      CALL XVINTRACT('NAV',' ')
      IF (XVIPTST('EXIT')) GOTO 100

      IF (XVIPTST('LIMB')) THEN
         CALL LIMBFIT(PIC,HPIC,NL,NS,iproj)
      ELSEIF (XVIPTST('RING')) THEN
         CALL RINGFIT(PIC,HPIC,NL,NS)
      ELSEIF (XVIPTST('STAR')) THEN
         FOV = MAX(NS,NL)/ZSCALE        !camera field-of-view in radians
c         IF (MOD(ICAM,2).EQ.0) THEN
c             FOV = .056			  !camera field-of-view in radians
c         ELSE
c             FOV = .0074		!this looks VGR specific....
c         ENDIF
         CALL STARFIT(IMG,PIC,HPIC,NL,NS,FOV)
      ELSEIF (XVIPTST('ANIMATE')) THEN  !VRH 7/17/89
CCC        CALL ANIMATE(PIC,HPIC,NL,NS)
           CALL XVMESSAGE('***ANIMATE command not available',' ')
      ENDIF
      GOTO 20
C
  100 CALL GETNAV
      CALL PNAV
      CALL UPDT_SPICE(IND,PROJECT,SEDR,SEDR,*999)
      CALL XVMESSAGE('NAV task completed',' ')
      CALL DEVOFF
      RETURN
C
  999 CALL XVMESSAGE('***NAV task cancelled',' ')
      CALL DEVOFF
      CALL ABEND
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get the project, frame, and camera IDs
C   IMG = input logical unit number for frame
C   LBUF = output buffer returned by GETLABCON
C
      SUBROUTINE FRAMEID(IMG,project,lbuf,frame_id,icam,
     &		iproj,isystem,igeo,ind)
      CHARACTER*5 PROJECT
      INTEGER*4 LBUF(100)
      INTEGER*4 FRAME_ID,ICAM,IPROJ
      LOGICAL PARMTST,XVIPTST,LST

      IGEO = 0			!Default is planetocentric latitudes
      ISYSTEM = 2		!Default is EME50 coordinates
      CALL GETPROJ(IMG,project,icam,frame_id,ind)
      IF (IND.EQ.0) GOTO 12

   10 CALL XVINTRACT('STRING','Enter project ID or NONE')
      IF (XVIPTST('EXIT')) THEN
	IND = 0
	RETURN
      ENDIF
      CALL XVIPARM('STRNG',project,icnt,idef,' ')
      IF (IDEF.NE.0) GOTO 10
      CALL UPRCASE(project)

   12 IF (PROJECT.EQ.'MAR-9') THEN
         IPROJ = 1		!Mariner 9
      ELSE IF (PROJECT.EQ.'MAR10') THEN
         IPROJ = 2		!Mariner 10
      ELSE IF (PROJECT.EQ.'VIKOR') THEN
         IPROJ = 3		!Viking Orbitor
      ELSE IF (PROJECT.EQ.'VGR-1' .OR. PROJECT.EQ.'VGR-2') THEN
	 IPROJ = 4		!Voyager
	 IGEO = 1		!Planetographic latitudes
      ELSE IF (PROJECT.EQ.'GLL') THEN
	 IPROJ = 5		!Galileo
	 ISYSTEM = 1		!J2000 coordinate system
      ELSE IF (PROJECT.EQ.'CASSI') THEN
	 IPROJ = 6		!Cassini
	 ISYSTEM = 1		!J2000 coordinate system
      ELSE IF (PROJECT.EQ.'NONE') THEN
         IPROJ = 0		!No project
	 ISYSTEM = 1		!J2000 coordinate system
         RETURN
      ELSE
         CALL XVMESSAGE('***Invalid project ID',' ')
         GOTO 10
      ENDIF
      CALL GETLABCON(IMG,PROJECT,lbuf,ind)
      IF (IND.EQ.2 .OR. LBUF(2).EQ.-999) THEN
         CALL XVINTRACT('IVALUE','Enter frame number')
         LST=PARMTST('VALUE',frame_id,I)
      ENDIF

      IF (IND.EQ.2 .OR. LBUF(6).EQ.-999) THEN
         IF (IPROJ.EQ.5) THEN
            ICAM = 1		!Only one camera on Galileo
         ELSE
            CALL XVINTRACT('IVALUE','Enter camera serial number')
            LST=PARMTST('VALUE',ICAM,I)
         ENDIF
      ENDIF
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C User specification of processing parameters.
C
      SUBROUTINE RPARAM
      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT
      LOGICAL PARMTST,XVIPTST
      CHARACTER*80 MSG
  110 FORMAT('NLW=',I2,'  NSW=',I2,'  NSEARCH=',I3)
  111 FORMAT('Debug flag=',I1,'  CHISQ mode=',I1)

   20 CALL XVINTRACT('PARAMS',' ')
      IF (XVIPTST('EXIT')) RETURN
      IF (PARMTST('NLW',IVAL,I)) NLW=2*(IVAL/2)+1
      IF (PARMTST('NSW',IVAL,I)) NSW=2*(IVAL/2)+1
      IF (PARMTST('NSEARCH',IVAL,I)) NSEARCH=IVAL
      IF (XVIPTST('CHI2')) IFIT=2
      IF (XVIPTST('CHI3')) IFIT=3
      IF (XVIPTST('DBUG')) IBUG=1
      IF (XVIPTST('NODBUG')) IBUG=0
      IF (XVIPTST('STATUS')) THEN
          WRITE(MSG,110) NLW,NSW,NSEARCH
          CALL XVMESSAGE(MSG,' ')
          WRITE(MSG,111) IBUG,IFIT
          CALL XVMESSAGE(MSG,' ')
      ENDIF
      GOTO 20

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Get user command of the form KEYWORD=VALUE.
C  Upon return, PARMTST=.TRUE. if user has entered the command "PARM",
C     =.FALSE. otherwise.
C  The values are stored in array NN and the number of values is returned
C  in COUNT.
C
      LOGICAL FUNCTION PARMTST(PARM,NN,COUNT)
      DIMENSION NN(1)
      CHARACTER*(*) PARM
      INTEGER CNT,COUNT,DEF
      PARMTST = .FALSE.
      CALL XVIPARM(PARM,NN,CNT,DEF,' ')
      IF (DEF .EQ. 0) then
	PARMTST = .TRUE.
	COUNT = CNT
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Get user command if the command is of the form KEYWORD=(real*8 value)
C  Upon return, dPARMTST=.TRUE. if user has entered the command "PARM",
C     =.FALSE. otherwise.
C  The values are stored in array xx and the number of values is returned
C  in COUNT.
C
      LOGICAL FUNCTION dPARMTST(PARM,xx,COUNT)
      real*8 xx(1)
      CHARACTER*(*) PARM
      INTEGER CNT,COUNT,DEF
      dPARMTST = .FALSE.
      CALL XVIPARMd(PARM,xx,CNT,DEF,' ')
      IF (DEF .EQ. 0) then
	dPARMTST = .TRUE.
	count = cnt
      endif
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Get user command if the command is of the form KEYWORD=(real*4 value)
C  Upon return, rPARMTST=.TRUE. if user has entered the command "PARM",
C     =.FALSE. otherwise.
C  The values are stored in array xx and the number of values is returned
C  in COUNT.
C
      LOGICAL FUNCTION rPARMTST(PARM,xx,COUNT)
      real*4 xx(1)
      CHARACTER*(*) PARM
      INTEGER CNT,COUNT,DEF
      rPARMTST = .FALSE.
      CALL XVIPARM(PARM,xx,CNT,DEF,' ')
      IF (DEF .EQ. 0) then
	rPARMTST = .TRUE.
	count = cnt
      endif
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read the input image into memory
C
      SUBROUTINE READIMAGE(IMG,HPIC,NL,NS,LPER,HPER,icode,pic,
     &		his,hflg,hbeg,hinc)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      INTEGER*4 HIS(0:255),HFLG,HBEG,HINC
      REAL*4 LPER,HPER

      INTEGER*4 HIST(-32768:32767)	!Temporary histogram buffer

      CHARACTER*5 FORMAT

C     ....Determine data format of input image
      CALL XVGET(IMG,ind,'FORMAT',format,' ')
      ICODE = 0
      IF (FORMAT.EQ.'BYTE') ICODE=1
      IF (FORMAT.EQ.'HALF'.OR.FORMAT.EQ.'WORD') ICODE=2
C
      IF (ICODE.EQ.1) THEN
         DO I=1,NL	!Read byte image into memory
            CALL XVREAD(IMG,PIC(1,I),ind,' ')
         ENDDO
         HBEG = 0
         HINC = 1
      ELSE
         CALL ZIA(HIST,65536)
         DO L=1,NL	!Read halfword image into memory
            CALL XVREAD(IMG,HPIC(1,L),ind,' ')
            CALL HSUB2(HPIC(1,L),NS,1,HIST) !Accumulate histogram
         ENDDO
         NFREQ = NL*NS
         CALL ASTRC2(HIST,NFREQ,LPER,HPER,IMIN,IMAX)
         NLEV = IMAX - IMIN + 1		!Number of halfword grey levels
         HINC = (NLEV-1)/256 + 1
         HBEG = (IMIN/HINC)*HINC
         CALL HWTOBYTE(HPIC,HBEG,HINC,NL*NS,pic) !Convert image to byte
         DO J=0,255	!Compress the histogram to 256 grey-levels
            CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J),1)
         ENDDO
         HFLG = 1	!Set histogram flag
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert an image from halfword to byte.
C Inputs: HBUF=halfword image.
C         HBEG,HINC=beginning DN of halfword data and DN step size.
C         NPIXELS=total number of pixels in image
C Output: BUF=byte image
C
      SUBROUTINE HWTOBYTE(HBUF,HBEG,HINC,NPIXELS,buf)
      IMPLICIT INTEGER(A-Z)
      INTEGER*2 HBUF(NPIXELS)
      BYTE BUF(NPIXELS),TBL(-32768:32767)

      HEND = HBEG + 255*HINC
C     ....Generate a halfword-to-byte lookup table
      CALL MVE(-5,HBEG+32769,0,TBL,0,1)
      N = HEND - HBEG + 1
      CALL INTRPA(1,N,TBL(HBEG),0,255)
      CALL MVE(-5,32768-HEND,255,TBL(HEND),0,1)

      DO I=1,NPIXELS
         BUF(I) = TBL(HBUF(I))
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get projection type (image-space or object-space) by examining
C input image label and presence of input reseau locations.
C
      SUBROUTINE MAPLABEL(IMG,IPROJ,itype,*)
      include 'mp_for_defs'
      INTEGER IPROJ
      LOGICAL XVIPTST
      parameter (maxtasks=100)
      character*8 tasks(maxtasks)
      integer inst(maxtasks)
      character*1440 cbuf
      character*32 projn
      real*8 mp

      CALL XVPCNT('RES',ICNT,IDEF)
      IF (icnt.gt.0) THEN	!If reseau locations are specified,
          JTYPE = 7		!then image-space is assumed.
      ELSE
          JTYPE = 8		!else, object-space is assumed.
      ENDIF
C     ....Now verify by checking image label
      call mp_init( mp, istat)
      call mp_label_read( mp, img, ind)
      if (ind.eq.mp_success) then
	call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',projn,istat)
	if (projn.eq.'POINT_PERSPECTIVE') then
          call XVMESSAGE('Warning: Using Perspective Projected Image',
     -' ')
 	  itype = 8		! = Object Space
	else
	  CALL XVMESSAGE('***Input picture has been map-projected',' ')
	  RETURN1
	endif
      ENDIF      

C  Check if image has been geometrically corrected ...
c  search thru all tasks present:
      ntasks = maxtasks		! on input, set to max. value
      call xlhinfo( img, tasks, inst, ntasks, istat, ' ')
      call chkstat( istat,' ** too many tasks ***',1)
      itype = 7
      do i=1,ntasks
	if (index(tasks(i),'GEOM').gt.0 .or. tasks(i).eq.'FARENC') then
	  itype = 8
	  go to 5
	endif
      enddo

c  check Vicar1 labels too ...
      call vic1lab( img, istat, nlabs, cbuf, 20)
      if (index(cbuf,'GEOM').gt.0 .or. index(cbuf,'FARENC').gt.0) then
	itype = 8
	go to 5
      endif

5     IF (IPROJ.EQ.0) ITYPE=8

      IF (IPROJ.EQ.0.OR.IPROJ.GT.4) RETURN
      IF (ITYPE.EQ.JTYPE) RETURN

      CALL XVMESSAGE('***WARNING:',' ')	!Warning messages follow...
      IF (ITYPE.EQ.8) GOTO 10
      IF (ITYPE.EQ.7) GOTO 20
      CALL XVMESSAGE('***Label indicates image has been map projected',
     .               ' ')
      RETURN1

   10 CALL XVMESSAGE('***Label indicates image has been geometrically',
     .               ' ')
      CALL XVMESSAGE('***corrected.',' ')
      GOTO 30

   20 CALL XVMESSAGE('***Label indicates image has not been',' ')
      CALL XVMESSAGE('***geometrically corrected',' ')

   30 CALL XVINTRACT('DISTOR',
     &  ' Specify image type by entering IMAGE or OBJECT')
      IF (XVIPTST('EXIT')) RETURN1
      IF (XVIPTST('IMAGE')) THEN
         IF (ITYPE.EQ.7) GOTO 990
         RETURN
      ENDIF
      IF (XVIPTST('OBJECT')) THEN
         ITYPE = 8
         RETURN
      ENDIF
      GOTO 30

  990 CALL XVMESSAGE('***Use RES parameter for image-space frames',' ')
      RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE GETRESLOC(ITYPE,IPROJ,ICAM,res,nres,*)
      INTEGER*4 IPROJ,ICAM
      REAL*4 RES(2,202)		!Output reseau coordinates
      CHARACTER*120 RFNAME      !VRH: Vicar U_NAME has size limit of 120

      integer ibis
      character *6 format(409) /5*'FULL',404*'REAL'/
      integer status
      integer record1,record2

      NRES = 0
      IF (IPROJ.GE.5) RETURN	!No reseau for GLL
      IF (IPROJ.GE.6) RETURN	!No reseau for CASSI

      IF (ITYPE.EQ.7) GOTO 50
C     ....Here if image is geometrically corrected (itype=8)
      IF (IPROJ.EQ.3) THEN
         CALL VOOS(ICAM,RES)	!Viking Orbiter
         NRES = 103
      ELSE IF (IPROJ.EQ.4) THEN
	 CALL VGROS(ICAM,RES)  !Voyager
	 NRES = 202
      ENDIF
      RETURN
C
C     ....Here if image is not geometrically corrected.
C     ....Get user-specified reseau locations.
   50 CALL XVPARM('RES',RFNAME,ICNT,IDEF,' ')          
      IF (IDEF.EQ.1) GOTO 992
      CALL XVUNIT(IUNITR,'R',1,IND,'U_NAME',RFNAME,' ')
c
c     all the following stuff is to accommodate the new
c     Reseau file format - bam 9/96
c
!      CALL XVREAD(IUNITR,RES,IND,' ') all of the below replaces this


      in2 = iunitr ! just cause its is the same as other programs


      call ibis_file_open(in2,ibis,'read',409,99999,   ! open reseau file
     -                        format,0,status)
      if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)
      call ibis_record_open(ibis,record1,'format:FULL',! open integer record
     -                               0,0,'FULL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)

      call ibis_record_open(ibis,record2,'format:REAL', ! then reseau marks
     -                               0,0,'REAL',status)
      if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
      call ibis_record_read(record2,res,l,status) ! read reseau marks
      call ibis_file_close(ibis,' ',status) ! close reseau file
      nres = 202         !Voyager
      IF (IPROJ.EQ.3) nres = 103    !Viking Orbiter
      RETURN

  992 CALL XVMESSAGE(
     & '***Input image assumed to be geometrically uncorrected',' ')
      CALL XVMESSAGE('***RESLOC parameter must be specified',' ')
      RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get camera distortion parameters:  All values in common area DISTOR
C are set except ITYPE.
C
      SUBROUTINE GETGEOPAR(RES,IPROJ,ICAM,OAL,OAS)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 RES(2,202),R1,R2,oal4,oas4
      INTEGER*4 IPROJ,ICAM

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      IF (IPROJ.EQ.3) THEN
	 CALL GEOMVO(conv,ICAM,RES)	!Viking Orbiter
         NPH = 22
         NPV = 9
      ELSE IF (IPROJ.EQ.4) THEN
         CALL GEOMAV(conv,ICAM,RES)	!Voyager
         NPH = 24
         NPV = 23
      ENDIF

      CALL MVE(7,2208,CONV(9),CONV,1,1)	!Move so buffer begins with tiepoints
C     ....Compute optical-axis intercept point in image-space
      CALL CONVISOS(PROJECT,ICAM,oal4,oas4,sngl(oal),sngl(oas),0,CONV,
     & NPH,NPV,ind)
      oal_is=oal4
      oas_is=oas4
C     ....Compute scale difference between image and object space
      CALL CONVISOS(PROJECT,ICAM,sngl(OAL_IS+1.),sngl(OAS_IS),r1,r2,1,
     & CONV,NPH,NPV,ind)
      CTOS = DSQRT((OAL-R1)**2 + (OAS-R2)**2)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read in blemish locations from blemish File
C Inputs: ITYPE,ICAM
C Outputs: blem(4,nblem), where
C		blem(1,I) = index to reference reseau mark
C		blem(2,I) = line coordinate
C		blem(3,I) = sample coordinate
C		blem(4,I) = radius of blemish
C          nblem = number of blemishes returned.
C Work area: LAB  --used to read labels to extract camera S/N of blemish
C		    file.
C
      SUBROUTINE VGRBLEMS(ITYPE,ICAM,blem,nblem,*)

      integer testos
      REAL*4 blem(4,1000)
      BYTE LAB(72,20)
      CHARACTER*120 BFNAME  !VRH 10/16/02: VICAR U_NAME parameter is max 120
      character*1 clab
      CHARACTER*27 ISBLEMS(4)/	!Image-space blemishes (VMS filenames)
     &		'WMS_VGR:[VGR2]BLEMLOC.WA   ',		!S/N 4
     &		'WMS_VGR:[VGR2]BLEMLOC.NA   ',		!S/N 5
     &		'WMS_VGR:[VGR1.WA]BLEMLOC.WA',		!S/N 6
     &		'WMS_VGR:[VGR1.NA]BLEMLOC.NA'/		!S/N 7
      CHARACTER*29 OSBLEMS(4)/		!Object-space blemishes
     &		'WMS_VGR:[VGR2]OSBLEMLOC.WA   ',	!S/N 4
     &		'WMS_VGR:[VGR2]OSBLEMLOC.NA   ',	!S/N 5
     &		'WMS_VGR:[VGR1.WA]OSBLEMLOC.WA',	!S/N 6
     &		'WMS_VGR:[VGR1.NA]OSBLEMLOC.NA'/	!S/N 7
c  unix filenames:
      CHARACTER*31 UISBLEMS(4)/		!Image-space blemishes
     &		'/project/vgr/vgr2/blemloc.wa   ',	!S/N 4
     &		'/project/vgr/vgr2/blemloc.na   ',	!S/N 5
     &		'/project/vgr/vgr1/wa/blemloc.wa',	!S/N 6
     &		'/project/vgr/vgr1/na/blemloc.na'/	!S/N 7
      CHARACTER*33 UOSBLEMS(4)/		!Object-space blemishes
     &		'/project/vgr/vgr2/osblemloc.wa   ',	!S/N 4
     &		'/project/vgr/vgr2/osblemloc.na   ',	!S/N 5
     &		'/project/vgr/vgr1/wa/osblemloc.wa',	!S/N 6
     &		'/project/vgr/vgr1/na/osblemloc.na'/	!S/N 7
C
C     ...Get Blemish File name

      CALL XVPARM('BLEMS',bfname,icnt,idef,' ')
      IF (IDEF.EQ.1) THEN
	i = testos(iosys)
	IF (ITYPE.EQ.8) THEN
	  if (iosys.eq.0) then		! VMS
	    BFNAME = OSBLEMS(ICAM-3)
	  else
	    BFNAME = UOSBLEMS(ICAM-3)
	  endif
	ELSE
	  if (iosys.eq.0) then		! VMS
	    BFNAME = ISBLEMS(ICAM-3)
	  else
	    BFNAME = UISBLEMS(ICAM-3)
	  endif
	ENDIF
      ENDIF
C
      CALL XVUNIT(IUNITB,'Y',1,IND,'U_NAME',BFNAME,' ')
      CALL XVOPEN(IUNITB,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
C     ...Check that camera S/N of blemishes matches that of input frame
      CALL XLGET(IUNITB,'HISTORY','CAMNUM',ival,ind,'HIST','BLEMFIX',
     & ' ')
      IF (IND.NE.1) THEN
	CALL VIC1LABX(IUNITB,IND,nlabs,lab,10) !VRH 10/16/02 fix
C	CALL BCDBIN(LAB(33,2),ival,1)
	call mvlc(LAB(33,2),clab,1)
	read(clab,1000) ival
1000	format(i1)
      ENDIF
      IF (IVAL.NE.ICAM) GOTO 990	!Make sure you use the right file
      CALL XVGET(IUNITB,IND,'NS',nsb,' ')
C     ...Compute number of blemishes (nblem)
      nblem = NSB/4			!Get number of blemishes
c      write(msg,'(a,i3)') 'VGRBLEMS: Number of blemishes read: ',nblem
c      call xvmessage(msg,' ')
      CALL XVREAD(IUNITB,blem,ind,' ')	!Read in the blemishes (blem)
      CALL XVCLOSE(IUNITB,ind,' ')
      RETURN
C
C     ...Error condition
  990 CONTINUE
      CALL XVCLOSE(IUNITB,ind,' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('***Camera S/N do not match',' ')
      CALL PRNT(4,1,IVAL,'***blemish file SN=.')
      CALL PRNT(4,1,ICAM,'***Image SN=.')
      RETURN1
      END
