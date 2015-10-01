$!****************************************************************************
$!
$! Build proc for MIPL module nav
$! VPACK Version 1.9, Saturday, February 04, 2012, 09:54:59
$!
$! Execute by entering:		$ @nav
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
$!   DOC         Only the documentation files are created.
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
$ write sys$output "*** module nav ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Doc = ""
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
$ if primary .eqs. "DOC" then Create_Doc = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Create_Doc .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to nav.com file -- ", primary
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
$ if Create_Doc then gosub Doc_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
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
$   Create_Doc = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Create_Doc = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("nav.imake") .nes. ""
$   then
$      vimake nav
$      purge nav.bld
$   else
$      if F$SEARCH("nav.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake nav
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @nav.bld "STD"
$   else
$      @nav.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create nav.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack nav.com -mixed -
	-s nav.f spicesub.f limbfit.f ringfit.f phasprof.f starfit.f t1950.f -
	   carea.f getangles.f planet.f editnav.f display.f dpic.f -
	-p nav.pdf -
	-t tstnav.pdf tstnav.scr tstnavstar.pdf tstnavstar_vms.scr -
	   tstnavstar_unix.scr tstnav.log_solos -
	-d nav.doc -
	-i nav.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create nav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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

      CALL XVMESSAGE('NAV version 06jul2011',' ')
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create spicesub.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Routine to get navigation data from the Voyager SEDR
C
      SUBROUTINE SPICESUB(IMG,PROJECT,LBUF,NL,NS,buf,ind)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 IMG
      CHARACTER*5 PROJECT		!GLL or VGR
      INTEGER*4 LBUF(100)		!GETLABCON label buffer
      REAL*8 BUF(100)			!Record returned by GETSPICE2

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,planet_id,target_id

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

c      LOGICAL XVPTST
      COMMON/CONST/PI,DTR,RTD

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      REAL*4 PBUF(20)			!Planet constants from PBDATA
      real*4 fl4,oal4,oas4,pscale4
      CHARACTER*12 tname
      CHARACTER*12 BLANK/'            '/

      IF (PROJECT.EQ.'NONE') THEN
         FL = 72000.0			!Space telescope values
         PSCALE = 65.0			!  "      "        "
         OAL = NL/2
         OAS = NS/2
      ELSE
        CALL GETCAMCON(PROJECT,ICAM,fl4,oal4,oas4,pscale4,ind)
	FL=FL4
	OAL=OAL4
	OAS=OAS4
	PSCALE=PSCALE4
      ENDIF
      ZSCALE = FL*PSCALE		!Object space scale (pixels/radian)

C     ....Get SPICE BUF for frame
      IF (TARGET_ID.NE.0) THEN
         ID = TARGET_ID
         IF (ID.EQ.3.OR.(ID.GE.5.AND.ID.LE.9)) ID=100*ID+99
         CALL PBNAME(ID,tname,*999)
      ELSE
         TNAME = BLANK
      ENDIF

      IF (PROJECT.EQ.'NONE') THEN
         CALL NOSPICE(TARGET_ID,buf,ind)
         IF (IND.NE.0) GOTO 999
      ELSE
         CALL GETSPICE2(IMG,.FALSE.,BUF,IND) 
         IF (IND.NE.1) THEN
	    CALL PRNT(4,1,ind,'***GETSPICE2 err=.')
            GO TO 999
	ENDIF
	CALL CMSOURCE(BUF,isource)		!Determine C-matrix source
	CALL GETSCET(BUF,target_id,idate,itime) !Get target_ID and SCET
      ENDIF

C     ....Get planet_id from target_id
      PLANET_ID = TARGET_ID
      IF (PLANET_ID.GT.9.AND.PLANET_ID.LT.9999) PLANET_ID=PLANET_ID/100
      IF (PLANET_ID.GT.9) PLANET_ID=5	!for GLL asteroids???

C     ....Get target_body constants
      CALL PBNAME(TARGET_ID,tname,*999)

c  call init_spice, since getspice2 clears the kernel pool and the new
c  PBDATA will fail:
      call init_spice

      CALL PBDATA(tname,pbuf,*999)
      RA = PBUF(1)
      RB = PBUF(2)
      RC = PBUF(3)
      RLORA = PBUF(4)*DTR
      ROT = PBUF(5)			! Rotation period (days)
c  (we could have obtained radii from the getspice2 buffer, but not ROT)

C     ....Only Saturn's rings are bright enough to obscure limb
      IF (PLANET_ID.EQ.6) THEN		! Saturn
            RMIN = 74000.0		! Inner edge of C-ring (km)
            RMAX = 137000.0		! Outer edge of F-ring (Km)
      ELSE
            RMIN = 0.0
            RMAX = 0.0
      ENDIF
C
      CALL MVE(8,9,BUF(50),ME,1,1)	! ME MATRIX
      CALL MVE(8,9,BUF(41),CM,1,1)	! C-matrix
      CALL MVE(8,3,BUF(19),PSC,1,1)	! Spacecraft-to-target vector
      RSC = DSQRT(PSC(1)**2+PSC(2)**2+PSC(3)**2)    !Spacecraft range
      DO I=1,3
         PSC(I) = -PSC(I)/RSC		!target-to-spacecraft unit vector
      ENDDO

      CALL FROMEME(ME,PSC,sclat,sclon)  !Spacecraft lat-lon
      IF (RA.EQ.RB) RLORA=SCLON		!*** Cludge for the sake of 1/2 pixel
      CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,psc3) !Compute s/c vector
      CALL GETPC(PSC3,RA,RB,RC)         !Compute projection constants...

C     ....Compute solar position
      RSUN = BUF(25)			!Range from target body to sun (km)
      SUNLAT = BUF(28)*DTR		!Subsolar latitude
      SUNLON = (360.D0-BUF(29))*DTR	!Subsolar east-longitude
      IF (IGEO.EQ.1) SUNLAT=GEOCEN(SUNLAT,SUNLON) !Convert to geocentric

      CALL TOEME(ME,SUNLAT,SUNLON,psun)	!Solar vector in EME50 coordinates
      CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3)  ! in (x3,y3,z3) coordinates
      IND = 0

c     ....Get navigation data for non-flight projects  A.VASAVADA 4/96
      if (IPROJ.eq.0) call nonflightspice

      IF (IBUG.EQ.0) RETURN

      CALL QPRINT(' Vector from S/C to central body.') !VRH 5/10/95 added
      CALL PRINTMTX(BUF(16),1)

      CALL PRNT(8,1,ROT,' Target body rotation period (days).')
      CALL XVMESSAGE('C-Matrix=',' ')
      CALL PRINTMTX(CM,3)
      CALL ORTHOT(CM)

      CALL XVMESSAGE('ME=',' ')
      CALL PRINTMTX(ME,3)
      CALL ORTHOT(ME)

      CALL XVMESSAGE('OM = MET*C',' ')
      DO 40 I=1,3
      DO 40 J=1,3
   40 OM(I,J) = ME(1,I)*CM(1,J) + ME(2,I)*CM(2,J) + ME(3,I)*CM(3,J)
C
      CALL PRINTMTX(OM,3)
      RETURN

  999 CALL XVMESSAGE('***Illegal planet id',' ')
      IND = -99
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Returns a SEDR buffer when there is no project SEDR/SPICE.
C This is a cludge to keep the program from bombing.  Correct
C data must be entered in EDITNAV.
C Currently implemented for Saturn only.
C
      SUBROUTINE NOSPICE(TARGET_ID,buf,ind)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER TARGET_ID
      REAL*8 BUF(100)
      REAL*8 ME0(3,3),CM(3,3),PSC(3)

      COMMON/CONST/PI,DTR,RTD
C        Solar range in A.U.
      REAL*4 SRANGE(9)/0.387098,0.723331,1.0,1.523679,5.2027,9.546,
     &   19.2,30.09,39.5/
      REAL*4 AUTOKM/149597870.66/		!Km per AU

C        Set C-matrix equal to identity matrix.  This is wrong but will be
C        updated when we solve for the planet center.
      DO I=1,3
         DO J=1,3
            CM(I,J) = 0.
            IF (I.EQ.J) CM(I,J)=1.D0
         ENDDO
      ENDDO
      CALL MVE(8,9,CM,buf(41),1,1)
C        Compute ME-matrix (needs to be corrected in EDITNAV by inputing
C        subspacecraft lat-lon).  For planets with ring systems, RA and
C        DEC of pole must be compatible with RINGORBS values.  For other
C        target-bodies, unit matrix is used.
      IPLANET = TARGET_ID
      IF (IPLANET.GT.10) IPLANET=IPLANET/100
      IF (IPLANET.EQ.6) THEN
         RA = 38.409D0*DTR		!right ascension of Saturn's pole
         DEC = 83.324D0*DTR		!declination of Saturn's pole
         CALL ME0CALC(RA,DEC,me0)
      ELSE IF (IPLANET.EQ.7) THEN
        ALPHAp = 76.5969D0*DTR		!Right-ascension and declination
        DELTAp = 15.1117D0*DTR		!of Uranus' north pole.
        CALL ME0CALC(RA,DEC,me0)
      ELSE IF (IPLANET.EQ.8) THEN
        ALPHAp = 298.852D0*DTR		!Right-ascension and declination
        DELTAp =  42.421D0*DTR 		!Neptunes' north pole.
        CALL ME0CALC(RA,DEC,me0)
      ELSE
        CALL MVE(8,9,CM,ME0,1,1)	!Unit matrix
      ENDIF
      CALL MVE(8,9,ME0,buf(50),1,1)
C        For solar vector
      RSUN = SRANGE(IPLANET)*AUTOKM		!Solar range
      SUNLAT = 0.0D0
      SUNLON = 0.0D0
      BUF(25) = RSUN
      BUF(28) = SUNLAT
      BUF(29) = SUNLON
C	 Set spacecraft vector to solar range
      PSC(1) = RSUN
      PSC(2) = 0.D0
      PSC(3) = 0.D0
      CALL MVE(8,3,PSC,buf(19),1,1)
      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get target-id and Spacecraft Event Time from MIPL SPICE buffer.
C
C Outputs: TARGET_ID = SPICE # for target body
C          IDATE = SCET date in the form YYYYDDD (1000*year + day)
C          ITIME = SCET time in the form HHMMSSMMM
C          
      SUBROUTINE GETSCET(IBUF,target_id,idate,itime)
      INTEGER*4 IBUF(9),TARGET_ID

      IDATE = 1000*IBUF(3) + IBUF(4)  ! IDATE = YYYYDDD
      ITIME = 10000000*IBUF(5) + 100000*IBUF(6)
     &         + 1000*IBUF(7) + IBUF(8)		! ITIME = HHMMSSMMM
      TARGET_ID = IBUF(9)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute miscellaneous projection constants
C Inputs: PSC3,RA,RB,RC
C Outputs: All data in COMMON/PCONST/
C
      SUBROUTINE GETPC(PSC3,RA,RB,RC)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 PSC3(3)

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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Store improved C matrix in SPICE C-kernel
C If return indicator (IND) is less 1, error updating C kernel.
C
      SUBROUTINE UPDT_SPICE(ind,PROJECT,sedr,dsedr,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*12 PROJECT
      REAL*4 SEDR(200)
      REAL*8 DSEDR(100)

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID
      integer*2 hcam

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

      REAL*8 OMp(3,3)
      REAL*4 NALINE,NASAMP,waline,wasamp
      LOGICAL XVIPTST

      CHARACTER*80 MSG
   99 FORMAT('O.S. NA planet center=(',F9.2,',',F9.2,')')

      IND = 1
      IF (PROJECT.EQ.'NONE') RETURN
C     ....Compute planet center
      CALL PLAINV(IND,SCLAT,SCLON,scline,scsamp,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.EQ.0) THEN
           CALL XVMESSAGE('***Err calculating planet center',' ')
           IND = -999
           RETURN1
      ENDIF

      IF (IPROJ.EQ.4.AND.MOD(ICAM,2).EQ.0) THEN
	hcam = ICAM
	waline = scline
	wasamp = scsamp
	CALL MWATNA(hCAM,waLINE,waSAMP,NALINE,NASAMP,*999)
	WRITE(MSG,99,ERR=2) NALINE,NASAMP
    2	CALL XVMESSAGE(MSG,' ')
      ENDIF

    5 CALL XVINTRACT('QUERRY',
     &  'Store improved pointing in C kernel? (Enter ''Y or ''N)')
      IF (XVIPTST('N')) RETURN
      IF (.NOT.XVIPTST('Y')) GOTO 5

      CALL MVCL('NAV ',SEDR(11),4)	!Set NAV flag
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON,OMp) !Compute OM' matrix
      CALL MXM(ME,OMp,DSEDR(41))	!C = ME*OMp

C     ....Transpose to reqular convention (camera-to-planet)
      CALL XPOSE(OMp,OMp)
      CALL MVE(8,9,OMp,DSEDR(59),1,1)
C          Compute RS vector
      CALL VECTOR3(RSC,SCLAT,SCLON,DSEDR(22))
      DSEDR(69) = SCLINE
      DSEDR(70) = SCSAMP
      CALL PUTSPICE2('NAV','NAV',SEDR,IND)
      IF (IND.EQ.1) RETURN
  999 RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Return the dot product of two vectors...
C
      FUNCTION DOT(A,B)
      REAL*8 DOT,A(3),B(3)
      DOT = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to multiply a 3x3 matrix by a rotation matrix about one of
C the (X,Y,Z) axes.
C
      SUBROUTINE ROTATE1(A,THETA,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(3,3)		! Matrix to be rotated (will contain result)
      REAL*8 THETA		! Angle of rotation in radians
      INTEGER*4 N		! Axis to be rotated
      REAL*8 B(3,3)             ! Work array to temporarily hold result
C
      C = DCOS(THETA)
      S = DSIN(THETA)      
C
C N=1  Rotate about x-axis...
C	| A11  A12  A13 |   | 1  0  0 |   | A11  C*A12-S*A13  S*A12+C*A13 |
C	| A21  A22  A23 | * | 0  C  S | = | A21  C*A22-S*A23  S*A22+C*A23 |
C	| A31  A32  A33 |   | 0 -S  C |   | A31  C*A32-S*A33  S*A32+C*A33 |
      IF (N.EQ.1) THEN
             DO I=1,3
             B(I,1) = A(I,1)
             B(I,2) = C*A(I,2) - S*A(I,3)
             B(I,3) = S*A(I,2) + C*A(I,3)
             ENDDO
      ENDIF

C N=2  Rotate about y-axis...
C	| A11  A12  A13 |   | C  0 -S |   | C*A11+S*A13  A12  -S*A11+C*A13 |
C	| A21  A22  A23 | * | 0  1  0 | = | C*A21+S*A23  A22  -S*A21+C*A23 |
C	| A31  A32  A33 |   | S  0  C |   | C*A31+S*A33  A32  -S*A31+C*A33 |
      IF (N.EQ.2) THEN
             DO I=1,3
             B(I,1) =  C*A(I,1) + S*A(I,3)
             B(I,2) =  A(I,2)
             B(I,3) = -S*A(I,1) + C*A(I,3)
             ENDDO
      ENDIF

C N=3  Rotate about z-axis...
C	| A11  A12  A13 |   | C  S  0 |   | C*A11-S*A12  S*A11+C*A12  A13 |
C	| A21  A22  A23 | * |-S  C  0 | = | C*A21-S*A22  S*A21+C*A22  A23 |
C	| A31  A32  A33 |   | 0  0  1 |   | C*A31-S*A32  S*A31+C*A32  A33 |
      IF (N.EQ.3) THEN
             DO I=1,3
             B(I,1) = C*A(I,1) - S*A(I,2)
             B(I,2) = S*A(I,1) + C*A(I,2)
             B(I,3) = A(I,3)
             ENDDO
      ENDIF
C
      CALL MVE(8,9,B,A,1,1)		! Replace A with result
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Test orthogonality of a 3x3 rotation matrix
C
      SUBROUTINE ORTHOT(A)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*132 MSG
      DATA MSG/' '/
      REAL*8 A(3,3)
C
      DO 20 I=1,3
      DO 10 J=1,3
      DIJ = A(I,1)*A(J,1) + A(I,2)*A(J,2) + A(I,3)*A(J,3)
10    WRITE (MSG(15*J-13:15*J),'(F14.10)') DIJ
20    CALL XVMESSAGE(MSG(2:80),' ')
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Print out a 3xN matrix
C
      SUBROUTINE PRINTMTX(MTX,N)
      REAL*8 MTX(3,N)
      CHARACTER*80 MSG
  110 FORMAT(9X,3F12.7)

      DO I=1,N
         WRITE(MSG,110,ERR=10) (MTX(I,J),J=1,3)
   10    CALL XVMESSAGE(MSG,' ')
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c Retrieves navigation data from MAP2 perspective projection label,
c command line, or user-input, in that order.  Non-flight projects only.
c Added by A. VASAVADA 4/96
      SUBROUTINE NONFLIGHTSPICE
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'mp_for_defs'
      real*8 mp

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,planet_id,target_id

      COMMON/CMAP/MODEL,IGEO,MRING,NRINGS
      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ,PHIp
      COMMON/CMAP/FL,OAL,OAS,PSCALE,ZSCALE
      COMMON/CMAP/ROT,RA,RB,RC,RLORA,RMIN,RMAX,ASDSUN
      COMMON/CMAP/CM(3,3),PSC(3),PSUN(3),PSAT(3),RSC,RSUN
      COMMON/CMAP/ME(3,3),OM(3,3),ANGLN,ANGLA,ANGLB
      COMMON/CMAP/PSC3(3),SCLAT,SCLON
      COMMON/CMAP/PSUN3(3),SUNLAT,SUNLON
      COMMON/CMAP/SMAA,ECC,INCL,OMEGAZ,PIZERO,dOMEGA_dt,dw_dt
      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS
      REAL*8 ME,INCL
      COMMON/CONST/PI,DEGRAD,RADDEG
c
      character*1 cval
!      character*7200 clabel
      character*16 persp/'*** PERSPECTIVE'/
!      integer ctest,dtest,nchar
      integer ctest,dtest
      real*4 nondata(40)
      character*32 projn


c
c SET PARAMETERS INITIALLY TO OBVIOUS DEFAULTS
        fl = -99999.
        pscale = -99999.
        oal = -99999.
        oas = -99999.
        rsc = -99999.
        sclat = -99999.
        sclon = -99999.
        scline = -99999.
        scsamp = -99999.
        angln = -99999.
        sunlat = -99999.
        sunlon = -99999.
 
c CHECK FOR PERSLAB LABEL
c IF FOUND READ OUT PARAMETERS USING SEARCV2.COM
!      nchar = 7200
!      call xlgetlabel(img,%REF(clabel),nchar,istat)
!      if (index(clabel(1:nchar),persp).ne.0) then
      call mp_init( mp, istat)
      call mp_label_read( mp, img, ind)

      if (ind.eq.mp_success) then
        call mp_get_value_str(mp,'MAP_PROJECTION_TYPE',projn,istat)
	if (projn.eq.'POINT_PERSPECTIVE') then
          call xvmessage(
     -    'Using MP Label Perspective Projection Parameters',' ')

!         call searcv2(img,ctest,clabel,nondata,inondata,xxx)

          call mp_mpo2buf(mp,nondata,ind)

          fl = nondata(27)
          pscale = nondata(30)
          rsc= nondata(38)
          oal = nondata(28)
          oas = nondata(29)
          sclat = nondata(31)
          sclon = nondata(32)
	  ! convert to East long.:
	  sclon = 360.0-sclon
          scline = nondata(33)
          scsamp = nondata(34)
          angln = nondata(35)
        end if
      endif
c
c IF NOT IN LABEL, ATTMEPT TO READ FROM COMMAND-LINE
c IF NON THERE, PROMPT USER
      if (fl.lt.-999.) then
        call xvparm('FOCAL',fl,ctest,dtest,0)             ! FOCAL LENGTH
        if (ctest.eq.0) then
          call xvmessage('Enter camera focal length (mm)',' ')
          read(*,'(f9.3)') fl
        endif
      endif
      if (pscale.lt.-999.) then
        call xvparm('SCALE',pscale,ctest,dtest,0)         ! PIXEL SCALE
        if (ctest.eq.0) then
          call xvmessage('Enter detector scale (pix/mm)',' ')
          read(*,'(f9.3)') pscale
        endif
      endif
      if (oal.lt.-999.) then
        call xvparm('OALINE',oal,ctest,dtest,0)           ! OPITCAL AXIS
        if (ctest.eq.0) then
          call xvmessage('Enter optical axis line',' ')
          read(*,'(f9.3)') oal
        endif
      endif
      if (oas.lt.-999.) then
        call xvparm('OASAMP',oas,ctest,dtest,0)
        if (ctest.eq.0) then
          call xvmessage('Enter optical axis sample',' ')
          read(*,'(f9.3)') oas
        endif
      endif
      if (rsc.lt.-999.) then
        call xvparm('RANGE',rsc,ctest,dtest,0)            ! RANGE
        if (ctest.eq.0) then
          call xvmessage('Enter s/c range (km)',' ')
          read(*,'(f9.3)') rsc
        endif
      endif
      if (sclat.lt.-999.) then
        call xvparm('SCLAT',sclat,ctest,dtest,0)          ! SUB S/C POINT
        if (ctest.eq.0) then
          call xvmessage('Enter sub-s/c lat (deg)',' ')
          read(*,'(f9.3)') sclat
        endif
      endif
      if (sclon.lt.-999.) then
        call xvparm('SCLON',sclon,ctest,dtest,0)
        if (ctest.eq.0) then
          call xvmessage('Enter sub-s/c lon (East,deg)',' ')
          read(*,'(f9.3)') sclon
        endif
      endif
      if (scline.lt.-999.) then                         ! PLANET CENTER
        call xvparm('CENTLINE',scline,ctest,dtest,0)
        if (ctest.eq.0) then
          call xvmessage('Enter initial planet center line',' ')
          read(*,'(f9.3)') scline
        endif
      endif
      if (scsamp.lt.-999.) then
        call xvparm('CENTSAMP',scsamp,ctest,dtest,0)
        if (ctest.eq.0) then
          call xvmessage('Enter initial planet center sample',' ')
          read(*,'(f9.3)') scsamp
        endif
      endif
      if (angln.lt.-999.) then
        call xvparm('NORTH',angln,ctest,dtest,0)          ! NORTH ANGLE
        if (ctest.eq.0) then
CBTC          call xvmessage('Enter north angle (deg from vertical)',' ')
          call xvmessage('Enter north angle (CW deg from vertical)',' ')
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC72
          read(*,'(f9.3)') angln
        endif
      endif
      if (sunlat.lt.-999.) then
        call xvparm('SUNLAT',sunlat,ctest,dtest,0)
        if (ctest.eq.0) then
          call xvmessage
     &    ('Enter sub-solar latitude (deg), -999. for s/c lat',
     &    ' ')
          read(*,'(f9.3)') sunlat
        endif
      endif
      if (sunlon.lt.-999.) then
        call xvparm('SUNLON',sunlon,ctest,dtest,0)
        if (ctest.eq.0) then
          call xvmessage
     &    ('Enter sub-solar longitude (East,deg), -999. for s/c lon',
     &    ' ')
          read(*,'(f9.3)') sunlon
        endif
      endif
c SET INITIAL SUNLAT AND SUNLON = SUBSPACECRAFT POINT IF NOT SPECIFIED
      if (sunlat.lt.-900.) sunlat = sclat
      if (sunlon.lt.-900.) sunlon = sclon
 
c CONVERT INPUT QUANTITIES, CALCULATE NAV VECTORS AND MATRICES
      zscale = pscale*fl
      sclat = sclat*degrad
      sclon = sclon*degrad
      sunlat = sunlat*degrad
      sunlon = sunlon*degrad
      angln = (angln-90.)*degrad
      call farenc(sclat,sclon,rlora,scline,scsamp,pscale,fl,
     &            oal,oas,psc,me,cm,om,angln,angla,anglb)
      call toeme(me,sclat,sclon,psc)                     ! PSC
      call vector3(rsc,sclat,sclon-rlora,psc3)           ! PSC3
      call toeme(me,sunlat,sunlon,psun)                  ! psun
      call vector3(rsun,sunlat,sunlon-rlora,psun3)       ! psun3
      call getpc(psc3,ra,rb,rc)                          ! planet cons
      call getangles(cm,me(1,3),psc,angln,angla,anglb)   ! angles
      call ommatrix(angln,angla,anglb-sclon+rlora,om)    ! OM-matrix
 
c PRINT OUT NAVIGATION SUMMARY
      call pnav
      call xvmessage('Parameters entered correctly? (enter Y or N)',
     & ' ')
      read(*,'(a1)') cval
      if (cval.eq.'n'.or.cval.eq.'N') then
        call xvmessage(' NAV task cancelled',' ')
        call abend
      endif
 
      RETURN
      END 
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create limbfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to improve the camera pointing by fitting the planet limb
C
      SUBROUTINE LIMBFIT(PIC,HPIC,NL,NS,iproj)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      REAL*4 WORK3(2,5000)

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CLIMB/ISL,ISS,INL,INS
      COMMON/CLIMB/NPTS,LPTS(2,3000),ALPTS(2,3000)
      REAL*4 LPTS,ALPTS

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

      REAL*8 DELTAD
      REAL*4 DELDIS
      LOGICAL XVIPTST
C
      MODEL=3
      CALL GETNAV			!Install planet geometry
C
   20 CALL XVINTRACT('LIMBFIT','LIMBFIT ')
      IF (XVIPTST('HELP')) CALL XVINTRACT('LIMBFIT',' ')
      IF (XVIPTST('EXIT')) RETURN

      call isetcolor		!Check for graphics color parm GCOLOR (BTC)

      IF (XVIPTST('SCAN')) THEN
         CALL LIMBSCAN(PIC,HPIC,NL,NS,*20)
         GOTO 20
      ENDIF

      IF (XVIPTST('TRACE')) THEN
         CALL LIMBTRACE(PIC,HPIC,NL,NS,WORK3,*20)
         GOTO 20
      ENDIF

      IF (XVIPTST('SEF')) THEN
         IF (NPTS.LE.0) THEN
            CALL XVMESSAGE('No limb points found yet!',' ')
            GOTO 20
         ENDIF
         IF (IPROJ.EQ.4) CALL CLEANVGR(LPTS,NPTS,RES,5.0)
         DELTAD = 0.5
         CALL SEARCH(ICODE,PIC,HPIC,NL,NS,LPTS,alpts,NPTS,
     &		NLW,NSW,NSEARCH,DELTAD,1)
         DELDIS = 5.0
         IF (IPROJ.EQ.4) CALL CLEANVGR(ALPTS,NPTS,RES,DELDIS)
         DELDIS = 3.0
         CALL CLEANPT1(LPTS,ALPTS,NPTS,DELDIS)
         IF (NPTS.GT.0) CALL DRAWCURVE(ALPTS,NPTS,1)
         GOTO 20
      ENDIF
C
      IF (XVIPTST('SCPTS')) THEN	! Redraw the computed pts
         IF (NPTS.GT.0) CALL DRAWCURVE(LPTS,NPTS,1)
         GOTO 20
      ENDIF
C
      IF (XVIPTST('SAPTS')) THEN	! Redraw the acquired pts
         IF (NPTS.GT.0) CALL DRAWCURVE(ALPTS,NPTS,1)
         GOTO 20
      ENDIF
C
      IF (XVIPTST('PARAMS')) THEN
         CALL RPARAM
         GOTO 20
      ENDIF

      IF (XVIPTST('EDIT')) THEN
         CALL EDITNAV(IMG,PIC,HPIC,NL,NS)
         GOTO 20
      ENDIF

      CALL DISPLAY(PIC,HPIC,NL,NS,ind)
      IF (IND.NE.0) GOTO 20
      CALL PDISPLAY(PIC,HPIC,NL,NS,IND) !VRH 6/28/89 new calling list
      GOTO 20
C
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Scan the planet limb for high-contrast points and update OM matrix.
C
      SUBROUTINE LIMBSCAN(PIC,HPIC,NL,NS,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/DISTORI/ITYPE,NPH,NPV

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CLIMB/ISL,ISS,INL,INS
      COMMON/CLIMB/NPTS,LPTS(2,3000),ALPTS(2,3000)
      REAL*4 LPTS,ALPTS

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

      REAL*4 DELDIS
      REAL*8 MINDIS,MAXDIS
      LOGICAL RPARMTST,XVIPTST
      CHARACTER*80 MSG
  110 FORMAT('PC (LINE,SAMP)=(',F10.2,',',F10.2,')  ANGLN=',F8.3)
  112 FORMAT('North angle has been shifted by',F8.3,' degrees')
C
      ISL = 1
      ISS = 1
      INL = NL
      INS = NS !Reset scan area to whole frame VRH 6/28/89
      MINDIS = 1.5D0
      MAXDIS = 4.D0
C     ....First, show the limb based on current pointing knowledge
      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,RMIN,RMAX,OAL,OAS,ZSCALE,
     &       ISL,ISS,INL,INS,NLW,NSW,NSEARCH,MINDIS,MAXDIS,1,LPTS,NPTS)
      IF (NPTS.GT.0) CALL DRAWCURVE(LPTS,NPTS,1)
      CALL MOVELIMB

    5 CALL XVINTRACT('QUERRY',
     &        ' Do you wish to do a limb scan? (Enter ''Y or ''N)')
      IF (XVIPTST('N')) THEN
         CALL UPDATENAV
         RETURN
      ENDIF
      IF (XVIPTST('Y')) GOTO 6
      GOTO 5

    6 CALL XVINTRACT('QUERRY',
     & ' Do you wish to specify the scan area? (Enter ''Y or ''N)')
      IF (XVIPTST('N')) GOTO 70
      IF (.NOT.XVIPTST('Y')) GOTO 6
      CALL CAREA(ISL,ISS,INL,INS)	! User specifies scan area
      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,RMIN,RMAX,OAL,OAS,ZSCALE,
     &       ISL,ISS,INL,INS,NLW,NSW,NSEARCH,MINDIS,MAXDIS,1,LPTS,NPTS)
      IF (NPTS.EQ.0) THEN
         CALL XVMESSAGE('***Limb is not in the picture',' ')
         RETURN1
      ENDIF
      CALL DRAWCURVE(LPTS,NPTS,1)

   70 CALL FITPARAMS(ITYPE,xoal,xoas)
      IF (ANGLA.LT.10.*DTR) THEN !VRH 7/26/89 automatic direct CM correction
         CALL XVMESSAGE(
     .   'NOTE: ANGLA (Optic axis to Planet pole) <10 deg', ' ')
         CALL XVMESSAGE(
     .   'Making pointing correction directly to C-Matrix',' ')
      ENDIF

      SCLINE0 = -99.0
      SCSAMP0 = -99.0
      NSW1 = 1			!Initial correlation window is 1 pixel wide
      NSEARCH1 = NSEARCH
      DELTAD1 = 1.0		!Initial step-size is 1 pixel
      ANGLN0 = ANGLN
      DT = 0.0D0
      LOOP_COUNT = 0
C
C     ....Iterate through several limb scans until we get close
   80 CALL SEARCH(ICODE,PIC,HPIC,NL,NS,LPTS,alpts,NPTS,
     &		NLW,NSW1,NSEARCH1,DELTAD1,0)
      DELDIS = 5.0	!Delete points near Voyager reseau
      IF (IPROJ.EQ.4) CALL CLEANVGR(ALPTS,NPTS,RES,DELDIS)
      DELDIS = 3.0	!Delete isolated points
      CALL CLEANPT1(LPTS,ALPTS,NPTS,DELDIS)
      IF (NPTS.EQ.0) THEN
         CALL XVMESSAGE('***No limb points found',' ')
         RETURN1
      ENDIF
      CALL DRAWCURVE(ALPTS,NPTS,1)
      IF (IFIT.EQ.2) THEN	!Find offets (DL,DS) and optionally DT
         CALL CHISQ2(IND,LPTS,ALPTS,NPTS,dl,ds,0)
      ELSE
         CALL CHISQ3(IND,LPTS,ALPTS,NPTS,XOAL,XOAS,dl,ds,dt,0)
         ANGLN = ANGLN + DT
      ENDIF
      IF (IND.EQ.0) RETURN1
      IF (ITYPE.EQ.7) THEN	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF
      IF (ANGLA.LT.10.*DTR) THEN  !VRH 7/20/89 apply corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      ELSE ! Regular way
         CALL MOVE1(DL,DS,angln,angla,anglb,ZSCALE) !Update ANGLA and ANGLB
      ENDIF
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,om)	!Compute new OM matrix
      MINDIS = 1.D0
      MAXDIS = 3.D0
      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,RMIN,RMAX,OAL,OAS,ZSCALE,
     &       ISL,ISS,INL,INS,NLW,NSW,NSEARCH,MINDIS,MAXDIS,1,LPTS,NPTS)
      CALL PLAINV(ind,SCLAT,SCLON,scline,scsamp,	!Compute planet center
     &	     OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      WRITE(MSG,110,ERR=86) SCLINE,SCSAMP,ANGLN*RTD
   86 CALL XVMESSAGE(MSG,' ')
      DIFF = (SCLINE-SCLINE0)**2+(SCSAMP-SCSAMP0)**2
      SCLINE0 = SCLINE
      SCSAMP0 = SCSAMP
      IF (DIFF.LT.0.5.AND.DABS(DT).LT.0.0001) GOTO 90  !VRH 6/28/89 abs of DT
      NSEARCH1 = 7			!tighten search radius after first pass
      DELTAD1 = 0.5			!and use 0.5 pixel search steps
      LOOP_COUNT = LOOP_COUNT + 1
      IF (LOOP_COUNT.LT.3) GOTO 80
      CALL XVMESSAGE('***Limb Scan converging slowly',' ')
      CALL XVMESSAGE('***There may be some bad points',' ')

   90 DELTAD = 0.5
      CALL SEARCH(ICODE,PIC,HPIC,NL,NS,LPTS,ALPTS,NPTS,
     &		NLW,NSW,3,DELTAD,0)
      DELDIS = 5.0
      IF (IPROJ.EQ.4) CALL CLEANVGR(ALPTS,NPTS,RES,DELDIS)
      IF (NPTS.EQ.0) THEN
         CALL XVMESSAGE('***No limb points found',' ')
         RETURN1
      ENDIF
      CALL DRAWCURVE(ALPTS,NPTS,1)

  100 CALL XVINTRACT('QUERRY',
     &        ' Do you wish to clean the curve? (Enter ''Y or ''N)')
      IF (.NOT.XVIPTST('Y').AND..NOT.XVIPTST('N')) GOTO 100
      IF (XVIPTST('Y')) THEN

  120    CALL XVINTRACT('ACLEAN',
     &        ' Enter CLEAN=x,''H,CZOOM,STRETCH,ASTR or ''EXIT')
         IF (XVIPTST('EXIT')) GOTO 130
         IF (XVIPTST('QUIT')) RETURN1
         IF (RPARMTST('CLEAN',deldis,I)) THEN
            CALL CLEANPTS(ALPTS,NPTS,deldis)
            GOTO 120
         ENDIF
         CALL DISPLAY(PIC,HPIC,NL,NS,IND)
         IF (IND.EQ.0) CALL PDISPLAY(PIC,HPIC,NL,NS,IND)
         IF (IND.EQ.1.AND.NPTS.GT.0) CALL DRAWCURVE(ALPTS,NPTS,1)
         GOTO 120
      ENDIF

  130 IF (IFIT.EQ.2) THEN
         CALL CHISQ2(IND,LPTS,ALPTS,NPTS,dl,ds,1)
      ELSE
         CALL CHISQ3(IND,LPTS,ALPTS,NPTS,XOAL,XOAS,dl,ds,dt,1)
         ANGLN = ANGLN + DT
      ENDIF
      IF (IND.EQ.0) RETURN1
      IF (ITYPE.EQ.7) THEN	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF
      IF (ANGLA.LT.10.*DTR) THEN  !VRH 7/20/89 apply corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      ELSE ! Regular way
         CALL MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE)
      ENDIF
      CALL UPDATENAV
      DIFF = DSQRT((SCLINE-SCLINE0)**2+(SCSAMP-SCSAMP0)**2)
      CALL PRNT(7,1,DIFF,' Final shift in planet center (pixels)=.')
      IF (IFIT.EQ.3) THEN
         WRITE(MSG,112) (ANGLN-ANGLN0)*RTD
         CALL XVMESSAGE(MSG,' ')
      ENDIF
      MINDIS = 1.5D0
      MAXDIS = 4.D0
      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,RMIN,RMAX,OAL,OAS,ZSCALE,
     &       ISL,ISS,INL,INS,NLW,NSW,NSEARCH,MINDIS,MAXDIS,1,LPTS,NPTS)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to manually trace the planet limb and update OM matrix.
C
      SUBROUTINE LIMBTRACE(PIC,HPIC,NL,NS,SLPTS,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 SLPTS(2,3000)	!Work area to hold points from SRCHINV

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CLIMB/ISL,ISS,INL,INS
      COMMON/CLIMB/NPTS,LPTS(2,3000),ALPTS(2,3000)
      REAL*4 LPTS,ALPTS

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

      CHARACTER*80 MSG
  110 FORMAT('SSP (LINE,SAMP)=(',F8.2,',',F8.2,')  ANGLN=',F8.3)
  111 FORMAT('North angle has been shifted by ',F8.3,' degrees')

      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,0.D0,0.D0,OAL,OAS,ZSCALE,
     &    -500,-500,NL+1000,NS+1000,0,0,0,2.D0,5.D0,0,LPTS,NPTS)
      IF (NPTS.EQ.0) THEN
          CALL XVMESSAGE('***Limb is not in picture',' ')
          RETURN1
      ENDIF

      CALL TRACECRV(PIC,HPIC,NL,NS,ALPTS,NALPTS)
      IF (NALPTS.LT.3) THEN
           CALL XVMESSAGE('***Insufficient number of points specified',
     .        ' ')
           CALL XVMESSAGE('***Limb trace terminated',' ')
           RETURN1
      ENDIF

      CALL FITPARAMS(ITYPE,xoal,xoas)
      IF(ANGLA.LT.10.*DTR) THEN !VRH 7/26/89 automatic direct CM correction
         CALL XVMESSAGE(
     .        'NOTE: ANGLA (Optic axis to Planet pole) <10 deg',' ')
         CALL XVMESSAGE(
     .        'Making pointing correction directly to C-Matrix',' ')
      ENDIF

      ANGLN0 = ANGLN
      SCLINE0 = -99.0
      SCSAMP0 = -99.0
      DT = 0.D0
      LOOP_COUNT = 0
      IMODE = 0

   10 CALL SRCHINV(LPTS,NPTS,ALPTS,NALPTS,SLPTS)
      IF (IFIT.EQ.2) THEN
           CALL CHISQ2(IND,SLPTS,ALPTS,NALPTS,DL,DS,IMODE)
      ELSE
           CALL CHISQ3(IND,SLPTS,ALPTS,NALPTS,XOAL,XOAS,DL,DS,DT,IMODE)
           ANGLN = ANGLN + DT
      ENDIF
      IF (IND.EQ.0) RETURN1
      IF (IFIT.EQ.7) THEN 	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF
      IF (ANGLA.LT.10.*DTR) THEN  !VRH 7/20/89 apply corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      ELSE ! Regular way
         CALL MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE)
      ENDIF
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix
      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,0.D0,0.D0,OAL,OAS,ZSCALE,
     &    1,1,NL,NS,0,0,0,1.D0,3.D0,0,LPTS,NPTS)
      IF (NPTS.EQ.0) THEN
           CALL XVMESSAGE('***Limb is not in picture',' ')
           RETURN1
      ENDIF
      CALL PLAINV(ind,SCLAT,SCLON,scline,scsamp,   !Compute planet center
     &		OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      WRITE(MSG,110,ERR=18) SCLINE,SCSAMP,ANGLN*RTD
   18 CALL XVMESSAGE(MSG,' ')
      DIFF = (SCLINE-SCLINE0)**2 + (SCSAMP-SCSAMP0)**2
      IF (IMODE.EQ.1) GOTO 20
      IF (DIFF.LT.0.01.AND.DABS(DT).LT.0.0001D0)
     &     IMODE=1			!Set up for last iteration
      SCLINE0 = SCLINE
      SCSAMP0 = SCSAMP
      LOOP_COUNT = LOOP_COUNT + 1
      IF (LOOP_COUNT.GE.15) THEN
           CALL XVMESSAGE('***Limb fit converges slowly',' ')
           IMODE = 1			!Set up for last iteration
      ENDIF
      GOTO 10
C
   20 DIFF = DSQRT(DIFF)
      CALL PRNT(7,1,DIFF,' Final shift in planet center (pixels)=.')
      IF (IFIT.EQ.3) THEN
          WRITE(MSG,111) (ANGLN-ANGLN0)*RTD
          CALL XVMESSAGE(MSG,' ')
      ENDIF

      CALL UPDATENAV
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute the planet limb.
C
C Outputs: LPTS(2,NPTS) contains the limb-points as (line,sample) pairs.
C	   NPTS is the total number of points returned.
C
C The limb-points are first computed in object-space, and converted to
C image-space if necessary.
C
C Obscured points are stored as (-line,-samp) and are spaced 8 pixels apart
C so that they form a broken-lined curve.
C
C IMODE=1 to check for solar illumination, 0 otherwise.
C
      SUBROUTINE LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,RMIN,RMAX,
     &    OAL,OAS,ZSCALE,ISL,ISS,INL,INS,NLW,NSW,NSEARCH,
     &    MINDIS,MAXDIS,IMODE,lpts,npts)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3),PSUN3(3),MINDIS,MAXDIS
      REAL*4 LPTS(2,3000)

      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256) !VRH 8/12/89 add
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2

      REAL*4 LINE,SAMP,LINE0,SAMP0,LINE2,SAMP2,osnl,osns
      REAL*8 MINDIS2,MAXDIS2
      REAL*8 PI/3.141592653589793D0/
      INTEGER*4 SSPFLAG
C
      PI2 = PI/2.D0
      DTR = PI/180.D0
      RTD = 180.D0/PI
      MINDIS2 = MINDIS**2	!Minimum pixel distance betwn points (squared)
      MAXDIS2 = MAXDIS**2	!Maximum pixel distance betwn points (squared)

      RMIN2 = RMIN**2		!Minimum ring radius (km)
      RMAX2 = RMAX**2		!Maximum ring radius (km)

      xSUN = AI2*PSUN3(1)	!constants for solar illumination check
      ySUN = BI2*PSUN3(2)	!(used in computation of dot product of
      zSUN = CI2*PSUN3(3)	!surface normal with solar vector)

C           Compute picture window
      RSL = ISL + NSEARCH/2 + NLW/2		! Starting line
      RSS = ISS + NSEARCH/2 + NSW/2		! Starting sample
      REL = ISL + INL - 1 - NSEARCH/2 - NLW/2	! Ending line
      RES = ISS + INS - 1 - NSEARCH/2 - NSW/2	! Ending sample
C     ....Compute approximate object-space frame size
      IF (ITYPE.EQ.7) THEN
          OSSS = 1.
          OSSL = 1.
          CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),osnl,osns,
     &		1,CONV,NPH,NPV,ind)
          RSL = MAX(RSL,1.D0)
          RSS = MAX(RSS,1.D0)
          REL = MIN(REL,DFLOAT(NL))
          RES = MIN(RES,DFLOAT(NS))
      ELSE
          OSSS = MIN(SS,1)
          OSSL = MIN(SL,1)
          OSNL = MAX(DFLOAT(NL),DFLOAT(NLDS)/ZOOM + SL)
          OSNS = MAX(DFLOAT(NS),DFLOAT(NSDS)/ZOOM + SS)
      ENDIF
C
      CLON = DCOS(SCLON-RLORA)
      SLON = DSIN(SCLON-RLORA)
C         Check for spacecraft position special cases...
      IF (SCLON.EQ.RLORA.OR.SCLON.EQ.-RLORA.OR.PSC3(2).EQ.0.) THEN
           SSPFLAG = 1		!x-z plane coincides with plane of spacecraft
      ELSE
           SSPFLAG = 0
      ENDIF
C
      RLAT = -PI2		!Start at the south pole
      DLAT = 0.5D0*DTR	!Initial step size is 0.5 degrees
      NPT1 = 0			!Number of limb points (x,y,z)
      NPT2 = 0			!Number of limb points (x,-y,z)
      LINE0 = -999.D0		!Set initial point to flag
C
C     ....Beginning at the south pole, search at one degree latitude
C     ....increments for a limb-point.
   10 CALL GETLIMB(IND,RLAT,CLON,SLON,z,x1,y1,x2,y2,SSPFLAG)
      IF (IND.EQ.0) THEN		!Branch if limb point is found
         RLAT = RLAT + DTR		!Otherwise, step one degree
         IF (RLAT.LT.PI2) GOTO 10	!and try again.
         GOTO 110			!Return if no limb found.
      ENDIF
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
         IF (DIS.LT.MAXDIS2) GOTO 40		   !If close enough, start...
         DLAT = DLAT/2				   !Else, reduce step size,
	 if (dlat.le.1.0d-10) then
	   call devoff
	   call mabend('LIMBPT failed to converge ...')
	 endif
         RLAT = RLAT - DLAT			   !and back up some more.
      ELSE					   !If we are off the limb,
         DLAT = DLAT/1.9			   !reduce step size,
	 if (dlat.le.1.0d-10) then
	   call devoff
	   call mabend('LIMBPT failed to converge ...')
	 endif
         RLAT = RLAT + DLAT			   !and move forward a notch.
      ENDIF
      GOTO 30
C
C          Here when we have found the start of the limb
C          Now step around the limb until we get to the north pole...
C
C	   First, get limb points at current RLAT...

   40 	CALL GETLIMB(IND,RLAT,CLON,SLON,z,x1,y1,x2,y2,SSPFLAG)

      IF (IND.EQ.0) THEN			   !Stepped off limb
          IF (LINE0.EQ.-999.D0) GOTO 110	   !Stop if off image.
          DIS = (LINE-LINE2)**2 + (SAMP-SAMP2)**2  !If the last points are
          IF (DIS.LT.MAXDIS2) GOTO 110		   !close, then finished.
          DLAT = DLAT/2				   !Else back up half a step.
	  if (dlat.le.1.0d-10) then
	    call devoff
	    call mabend('LIMBPT failed to converge ...')
	  endif
          RLAT = RLAT - DLAT
          GOTO 40
      ENDIF
C           Compute vector from spacecraft to limb point (x1,y1,z)...
      xc = x1 - PSC3(1)
      yc = y1 - PSC3(2)
      zc = z  - PSC3(3)
C           Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      LINE = S*y0 + OAL			!Line-sample coordinates of point
      SAMP = S*x0 + OAS
C            Check spacing between points
      IF (LINE0.NE.-999.D0) THEN
          DIS = (LINE-LINE0)**2 + (SAMP-SAMP0)**2
          IF (DIS.LT.MINDIS2.AND.	!If spacing is less than 1 pixel
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
          IF (DIS.GT.MAXDIS2) THEN      !If spacing is greater than 3 pixels
             RLAT = RLAT - DLAT		!back up
             DLAT = 0.5D0*DLAT		!and decrease step size by 50%
	     if (dlat.le.1.0d-10) then
		call devoff
		call mabend('LIMBPT failed to converge ...')
	     endif
             RLAT = RLAT + DLAT
             GOTO 40
          ENDIF
      ENDIF
C                Now do (x2,y2,z)
      xc = x2 - PSC3(1)
      yc = y2 - PSC3(2)
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      LINE2 = S*y0 + OAL
      SAMP2 = S*x0 + OAS
C     ....If neither point is in the image, take one degree steps
C     ....until it is.
      IF ((LINE.LT.OSSL.OR.LINE.GT.OSNL.OR.SAMP.LT.OSSS.OR.SAMP.GT.OSNS)
     &.AND.(LINE2.LT.OSSL.OR.LINE2.GT.OSNL.OR.SAMP2.LT.OSSL
     &.OR.SAMP2.GT.OSNS)) THEN   !VRH 8/12/89 redefine edge
          RLAT = RLAT + DTR
          LINE0 = -999.D0		!Set initial point to flag
          IF (RLAT.GE.PI2) GOTO 110	!Stop if at north pole
          GOTO 40
      ENDIF
C     ....If we get this far, the limb-point is accepted.  However, it
C     ....may be flagged below if it is obscured.
      LINE0 = LINE			!Update coordinates of last
      SAMP0 = SAMP			!limb-point found in image.
      IFLAG1 = 0			!Clear obscured-point-flags
      IFLAG2 = 0
C            See if point is obscured by ring
      IF (RMAX.LT.1.0) GOTO 70	    !Skip test if planet has no ring
      IF (zc.eq.0.0) GOTO 70       !Skip if spacecraft is in ring plane
      S = -PSC3(3)/zc
      IF (S.LT.0.0) GOTO 70	    !Skip if camera is looking away from ring
      RSQ = (S*xc+PSC3(1))**2 + (S*yc+PSC3(2))**2
      IF (RSQ.GT.RMIN2.AND.RSQ.LT.RMAX2) THEN
             IFLAG1=1
             IFLAG2=1
             GOTO 80
      ENDIF
C           Check for solar illumination: (b2c2x1,a2c2y1,a2b2z)oPSUN .GT. 0 ?
   70 IF (IMODE.NE.1) GOTO 80
      IF (x1*xSUN + y1*ySUN + z*zSUN .LE. 0.D0) THEN
             IFLAG1 = 1		!Point is not illuminated
      ELSE
             IF (RMAX.GT.1.D0) THEN	!Check for ring shadow
                   S = z/PSUN3(3)
                   RAD2 = (x1-S*PSUN3(1))**2 + (y1-S*PSUN3(2))**2 
                   IF (RAD2.GT.RMIN2.AND.RAD2.LT.RMAX2) IFLAG1=1
             ENDIF
      ENDIF

C           Check for solar illumination: (b2c2x2,a2c2y2,a2b2z)oPSUN .GT. 0 ?
      IF (x2*xSUN + y2*ySUN + z*zSUN .LE. 0.D0) THEN
           IFLAG2 = 1		!Point is not illuminated
      ELSE
             IF (RMAX.GT.1.D0) THEN	!Check for ring shadow
                   S = z/PSUN3(3)
                   RAD2 = (x2-S*PSUN3(1))**2 + (y2-S*PSUN3(2))**2 
                   IF (RAD2.GT.RMIN2.AND.RAD2.LT.RMAX2) IFLAG2 = 1
             ENDIF
      ENDIF

   80 IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,line,samp,LINE,SAMP,
     &		0,CONV,NPH,NPV,ind)
      IF (LINE.LT.RSL.OR.LINE.GT.REL) GOTO 90	!Point not in scan area
      IF (SAMP.LT.RSS.OR.SAMP.GT.RES) GOTO 90
      IF (IFLAG1.EQ.0) THEN
           NPT1 = NPT1 + 1
           LPTS(1,NPT1) = LINE
           LPTS(2,NPT1) = SAMP
      ELSE
           DIS = (LINE-SLINE)**2 + (SAMP-SSAMP)**2
           IF (DIS.GT.64.D0) THEN
               NPT1 = NPT1 + 1
               SLINE = LINE
               SSAMP = SAMP
               LPTS(1,NPT1) = -LINE
               LPTS(2,NPT1) = -SAMP
           ENDIF
      ENDIF

   90 IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,line2,samp2,
     &	LINE2,SAMP2,0,CONV,NPH,NPV,ind)
      IF (LINE2.LT.RSL.OR.LINE2.GT.REL) GOTO 100
      IF (SAMP2.LT.RSS.OR.SAMP2.GT.RES) GOTO 100
      IF (IFLAG2.EQ.0) THEN
          NPT2 = NPT2 + 1	!Store I.S. line-samples starting from end
          LPTS(1,3001-NPT2) = LINE2
          LPTS(2,3001-NPT2) = SAMP2
      ELSE
          DIS = (LINE2-SLINE2)**2 + (SAMP2-SSAMP2)**2
          IF (DIS.GT.64.D0) THEN
              NPT2 = NPT2 + 1
              SLINE2 = LINE2
              SSAMP2 = SAMP2
              LPTS(1,3001-NPT2) = -LINE2  !Store image-space line-samples
              LPTS(2,3001-NPT2) = -SAMP2
          ENDIF
      ENDIF
C
  100 IF (NPT1+NPT2.GE.3000) THEN
         CALL XVMESSAGE('***Maximum number of limb points computed',' ')
         GOTO 110
      ENDIF

      IF (RLAT.GE.PI2) GOTO 110		!If at north pole, we are done
      RLAT = RLAT + DLAT		!Otherwise, take another step.
      IF (RLAT.GT.PI2) THEN		!If we step too far,
         RLAT = PI2			!back up to north pole
         DLAT = PI2 - RLAT - DLAT	!and adjust step size.
      ENDIF
      GOTO 40				!Go back and repeat process.
C
  110 J = 3000 - NPT2
C            Condense the array
      DO I=1,NPT2
          LPTS(1,NPT1+I) = LPTS(1,J+I)
          LPTS(2,NPT1+I) = LPTS(2,J+I)
      ENDDO
C
      NPTS = NPT1 + NPT2
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given latitude RLAT, find point on limb (x,y,z) at that latitude.
C Inputs:  RLAT,CLON,SLON,SSPFLAG, and PCONST elements
C Outputs: IND, (X1,Y1,Z) (X2,Y2,Z)
C Upon return, IND=1 if limb point is found, =0 otherwise.
C
      SUBROUTINE GETLIMB(IND,RLAT,CLON,SLON,Z,X1,Y1,X2,Y2,SSPFLAG)
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

C            Here for spacecraft position special cases: SCLON=RLORA or
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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute (line,sample) coordinates for limb points (x,y,z)
C Inputs:  (x,y,z) expressed in (x3,y3,z3) coordinates.
C                 OM,PSC3,OAL,OAS,ZSCALE
C Outputs: (LINE,SAMP) corresponding to (x,y,z)
C
      SUBROUTINE GETLS(X,Y,Z,LINE,SAMP,OM,PSC3,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3)
      REAL*4 LINE,SAMP

C           Compute vector from spacecraft to limb point
      xc = X - PSC3(1)
      yc = Y - PSC3(2)
      zc = Z - PSC3(3)
C           Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      LINE = S*y0 + OAL
      SAMP = S*x0 + OAS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE FITPARAMS(ITYPE,xoal,xoas)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
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


      IF (ITYPE.EQ.7) THEN
         XOAL = OAL_IS
         XOAS = OAS_IS
      ELSE
         XOAL = OAL
         XOAS = OAS
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to manually move the computed limb with the cursor
C Outputs are updated values for ANGLA, ANGLB, OM, and LPTS
C
      SUBROUTINE MOVELIMB
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT
      LOGICAL XVIPTST

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      REAL*4 CONV
      CHARACTER*5 PROJECT

      COMMON/DISTORI/ITYPE,NPH,NPV

      COMMON/CLIMB/ISL,ISS,INL,INS  
      COMMON/CLIMB/NPTS,LPTS(2,3000),ALPTS(2,3000)
      REAL*4 LPTS,ALPTS

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

      REAL*4 RL4,RS4
C
      CALL XVMESSAGE('Begin manual registration of limb...',' ')
      CALL XVMESSAGE('Move Cursor to a point on computed limb',' ')
      CALL XVINTRACT('READY',
     &  ' Hit Return when ready or type ''EXIT to skip')
      IF (XVIPTST('EXIT')) RETURN
      CALL CURSOR(rline,rsamp)
      IF (ITYPE.EQ.7) then
	CALL CONVISOS(PROJECT,ICAM,sngl(RLINE),sngl(RSAMP),rl4,rs4,
     &		1,CONV,NPH,NPV,ind)
	rline=rl4
	rsamp=rs4
      endif
C
   50 CALL XVMESSAGE('Move Cursor to actual limb',' ')
      CALL XVINTRACT('READY',
     &  ' Hit Return when ready or type ''EXIT if done')
      IF (XVIPTST('EXIT')) RETURN
      CALL CURSOR(RLINE2,RSAMP2)
      IF (ITYPE.EQ.7) then
	CALL CONVISOS(PROJECT,ICAM,sngl(RLINE2),sngl(RSAMP2),rl4,rs4,
     &		1,CONV,NPH,NPV,ind)
	rline2=rl4
	rsamp2=rs4
      endif
      DL = RLINE2 - RLINE
      DS = RSAMP2 - RSAMP
      CALL MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix
      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,RMIN,RMAX,OAL,OAS,ZSCALE,
     &       ISL,ISS,INL,INS,NLW,NSW,NSEARCH,1.5D0,4.D0,1,LPTS,NPTS)
      IF (NPTS.GT.0) THEN
            CALL DRAWCURVE(LPTS,NPTS,1)
      ELSE
            CALL XVMESSAGE('***Limb is not in picture',' ')
      ENDIF
      RLINE = RLINE2
      RSAMP = RSAMP2
      GOTO 50
C
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given nominal limb points (LPTS), scan through the image for the
C actual limb points (ALPTS) by searching radially + and - NSEARCH
C pixels from each nominal limb point for a high contrast area which
C correlates well to the limb function (F).
C
C Output: ALPTS
C
      SUBROUTINE SEARCH(ICODE,PIC,HPIC,NL,NS,LPTS,alpts,NPTS,
     &		INLW,NSW,NSEARCH,DELTAD,MODE)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 LPTS(2,3000),ALPTS(2,3000)
      REAL*8 DELTAD	!step size in pixels (0.5 or 1.0)

      INTEGER*4 S
      INTEGER*4 d1,d2,d3,d4
      include 'fortport'
C     ....Limb function at half-pixel steps
      REAL*4 F0(25)/15.2,15.5,15.9,16.3,
     &              16.8,17.4,18.2,19.4,21.2,24.1,30.4,42.4,
     &              63.6,93.8,131.2,169.5,202.3,225.9,243.8,
     &              258.0,268.8,277.1,283.6,289.8,295.1/
      REAL*4 F(21),R(101),PS(151),PS2(151),CF(151)
      REAL*4 L0,S0,L1,S1,L2,S2,LJ,SJ
      CHARACTER*80 MSG
 1000 FORMAT(F20.1)
C
      RTHRESH = 0.8			! Correlation threshold
      INC = 1.01/DELTAD			! This routine expects DELTAD=.5 OR 1.
      NLW = 2*((INLW*INC-1)/2) + 1      ! Scale correlation window to step size
      NSTEPS = 2*INC*NSEARCH + 1	! Total # of steps taken in search
      NLAREA = NSTEPS + NLW - 1         ! Total # of lines needed for search
      AREA = NLW*NSW			! Total # of pixels in correlation area
      DMIN = NSTEPS/2 + 1
      FSUM = 0.0
      FSUM2 = 0.0
      INC = 2.01*DELTAD
      J = 15 - (NLW*INC-1)/2
C            Generate limb function
      DO I=1,NLW
         F(I) = F0(J)
         FSUM = FSUM + F(I)
         J = J + INC
      ENDDO
C            Normalize function so that mean=0
      FMEAN = FSUM/NLW
      DO I=1,NLW
         F(I) = F(I) - FMEAN
         FSUM2 = FSUM2 + F(I)**2
      ENDDO
      FSSUM2 = FSUM2*NSW

      IF (MODE.EQ.1) THEN
         DO I=1,NLAREA
            CF(I) = 0.0
         ENDDO
      ENDIF
C
C              Main search loop thru each limb point
      DO 100 N=1,NPTS
      L1 = LPTS(1,N)          	! Nominal limb point is at (L1,S1)
      S1 = LPTS(2,N)
      ALPTS(1,N) = -99.0	! If no match is found, actual points
      ALPTS(2,N) = -99.0	! will be flagged as (-99.,-99.)
      IF (L1.LT.0.0) GOTO 100	! Skip if nominal point is flagged
C            Compute directional cosines for perpendicular at (L1,S1)
      N1 = N + 1
      IF (N1.GT.NPTS) N1=1
      L2 = LPTS(1,N1)
      S2 = LPTS(2,N1)
      D = SQRT((L2-L1)**2 + (S2-S1)**2)
      IF (D.GT.20.0) THEN
          N1 = N - 1
          IF (N1.EQ.0) N1=NPTS
          L2 = LPTS(1,N1)
          S2 = LPTS(2,N1)
          D = SQRT((L2-L1)**2 + (S2-S1)**2)
          IF (D.GT.20.0) GOTO 100
          COSL = -(S1-S2)/D
          COSS = -(L2-L1)/D
      ELSE
          COSL = (S1-S2)/D
          COSS = (L2-L1)/D
      ENDIF
C
C            Transform picture area to NLAREAxNSW correlation plane
      DO 20 L=1,NLAREA
      D0 = (L-NLAREA/2-1)*DELTAD
      L0 = D0*COSL + L1
      S0 = D0*COSS + S1
      PSUM = 0.
      PSUM2 = 0.
      J = -NSW/2
C
      DO 19 S=1,NSW
      Lj = L0 + J*COSS
      Sj = S0 - J*COSL
      IL = Lj		! Compute DN at (Lj,Sj) by interpolating
      IS = Sj		! over four nearest neighbors
      x = Lj - IL
      y = Sj - IS
      IF (ICODE.EQ.1) THEN
         d1 = BYTE2INT(PIC(IS,IL))
         d2 = BYTE2INT(PIC(IS+1,IL))
         d3 = BYTE2INT(PIC(IS,IL+1))
         d4 = BYTE2INT(PIC(IS+1,IL+1))
      ELSE
         d1 = HPIC(IS,IL)
         d2 = HPIC(IS+1,IL)
         d3 = HPIC(IS,IL+1)
         d4 = HPIC(IS+1,IL+1)
      ENDIF
      dn = d1 + (d2-d1)*x + (d3-d1)*y + (d1-d2-d3+d4)*x*y
      PSUM = PSUM + dn      
      PSUM2 = PSUM2 + dn**2
   19 J = J + 1
C
      IF (MODE.EQ.1) CF(L)=CF(L)+PSUM
      PS(L) = PSUM
   20 PS2(L) = PSUM2
C
      RMAX = -99999.
C                       ! Correlate limb at each step on perpendicular
C
      DO 90 LL=1,NSTEPS
      EL = LL + NLW - 1
C              Compute double sums of dn and dn**2 over correlation area
      IF (LL.EQ.1) THEN
         PSSUM = 0.
         PSSUM2 = 0.
         DO L=1,NLW
            PSSUM = PSSUM + PS(L)
            PSSUM2 = PSSUM2 + PS2(L)
         ENDDO
      ELSE
         PSSUM  = PSSUM  - PS(LL-1)  + PS(EL)
         PSSUM2 = PSSUM2 - PS2(LL-1) + PS2(EL)
      ENDIF
C
      PF = 0.
      DO 80 L=1,NLW
   80 PF = PF + PS(LL+L-1)*F(L)
C
      DENOM = (PSSUM2-PSSUM**2/AREA)*FSSUM2
      IF (DENOM.GT.0.1) THEN
         RCOR = PF**2/DENOM
         IF (RCOR.GT.RMAX) THEN
            RMAX = RCOR
            LMAX = LL
         ENDIF
      ENDIF
C
   90 R(LL) = RCOR
      IF (RMAX.LT.RTHRESH) GOTO 100	!Reject match if poor correlation
      IF (LMAX.EQ.1.OR.LMAX.EQ.NSTEPS) GOTO 100	!Reject match if at ends of search
C            Use parabola to interpolate around correlation maximum
      R1 = R(LMAX-1)
      R2 = R(LMAX+1)
      D0 = (0.5*(R1-R2)/(R1+R2-2.*RMAX)+LMAX-DMIN)*DELTAD
      ALPTS(1,N) = D0*COSL + L1
      ALPTS(2,N) = D0*COSS + S1
  100 CONTINUE
C
      IF (MODE.NE.1) RETURN
      CALL XVMESSAGE('Computed limb function',' ')
      DO L=1,NLAREA
         WRITE(MSG,1000) CF(L)/(NPTS*NSW)
         CALL XVMESSAGE(MSG,20)
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ringfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to improve the camera pointing by fitting to a known ring 
C or ring radius.
C
      SUBROUTINE RINGFIT(PIC,HPIC,NL,NS)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CPICX/T0,T

      COMMON/COE/OE(110,4),OEF,OEFNAME
      INTEGER*4 OEF
      CHARACTER*256 OEFNAME

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH,width
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE   ! 1=outer edge, 2=inner edge, 3=thin edge
      CHARACTER*1 RINGS(15)

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

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
      CHARACTER*1 RING_ID(20) 
      LOGICAL PARMTST,RPARMTST,XVIPTST
      REAL*4 PINPUT(3),mindis,maxdis,deldis,r
      CHARACTER*80 MSG
      REAL*4 WORK3(2,5000)
C
      CALL T1950(IDATE,ITIME,RSC,t) 
      CALL RINGIN(PLANET_ID,rings,nrings,oe,oef,oefname) !Ring geometry from OEF
      CALL MVE(8,4,OE(2,PLANET_ID-4),ALPHAp,1,1)
      CALL GETEPOCH(PLANET_ID,ALPHAP,DELTAP,THETA,ZETAZ,phip,t0)
      CALL ERING0(PLANET_ID,T,T0,0)
    
      IF (RA.NE.RB) THEN
         CALL XVMESSAGE('***WARNING: Target-ID is probably incorrect',
     .        ' ')
         CALL XVMESSAGE('***Enter EDIT and check status',' ')
      ENDIF

      MODEL = 4			!Set target model to ring-plane
      MRING = 1			!Initial reference plane is planet's equator
      CALL GETNAV
      ITRACE = 0
C
   20 CALL XVINTRACT('RINGFIT','RINGFIT')
      IF (XVIPTST('HELP')) CALL XVINTRACT('RINGFIT',' ')
      IF (XVIPTST('EXIT')) RETURN
      CALL ISETCOLOR		!Check for graphics color parm GCOLOR

      IF (XVIPTST('SCAN')) THEN
         ITRACE = 2
         CALL RINGSCAN(PIC,HPIC,NL,NS,*20)
         GOTO 20
      ENDIF

      IF (XVIPTST('TRACE')) THEN
         ITRACE = 1
         CALL RINGTRACE(PIC,HPIC,NL,NS,WORK3,1,*20)
         GOTO 20
      ENDIF

      IFIT = 0
      IF (XVIPTST('CHI2')) IFIT=2
      IF (XVIPTST('CHI3')) IFIT=3
      IF (IFIT.EQ.0) GOTO 23
      IF (ITRACE.EQ.1) THEN
         CALL GETSEDR		!Restore SEDR pointing
         CALL UPDATENAV
         CALL RINGTRACE(PIC,HPIC,NL,NS,WORK3,0,*20)
      ENDIF
      GOTO 20

   23 IF (XVIPTST('SEF')) THEN
         ETYPE = 0
   25    CALL XVINTRACT('RSEARCH',
     &      'Specify edge type (Enter ''INNER, ''OUTER, or WIDTH)')
         IF (XVIPTST('OUTER')) ETYPE = 1
         IF (XVIPTST('INNER')) ETYPE = 2
         IF (SCLAT.LT.0.D0) ETYPE=3
         IF (RPARMTST('WIDTH',WIDTH,I)) ETYPE = 3
         IF (ETYPE.EQ.0) GOTO 25
         DELDIS = 5.0
         IF (IPROJ.EQ.4) CALL CLEANVGR(RPTS,NRPTS,RES,deldis)
         DELTAD2 = 0.5
         CALL RSEARCH(ICODE,PIC,HPIC,NL,NS,RPTS,ARPTS,NRPTS,
     &		NLW,NSW,NSEARCH,DELTAD2,ETYPE,WIDTH,1)
         IF (NRPTS.GT.0) CALL DRAWCURVE(ARPTS,NRPTS,1)
         GOTO 20
      ENDIF
C
      IF (RPARMTST('RADIUS',R,I)) THEN	! Draw ring of radius R
         CALL GETRING(MRING)
         IF (MRING.EQ.1) THEN
            CALL XVMESSAGE('Reference plane is planet''s equator',' ')
         ELSE
            WRITE(MSG,116) RINGS(MRING)
116         FORMAT('Reference plane is that of ',A1,'-Ring')
            CALL XVMESSAGE(MSG,' ')
         ENDIF
         SMAA = R
         ECC = 0.D0
         MINDIS = 16.0
         MAXDIS = 32.0
         CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &       OAL,OAS,ZSCALE,SL,SS,NINT(NLDS/ZOOM),NINT(NSDS/ZOOM),
     &       0,0,0,0,mindis,maxdis,RPTS,NRPTS)  
         IF (NRPTS.GT.0) CALL DRAWCURVE(RPTS,NRPTS,0)
         GOTO 20
      ENDIF

      IF (PARMTST('RING',RING_ID(1),I)) THEN
         CALL UPRCASE(RING_ID(1))
         DO IRING=2,15
            IF (RING_ID(1).EQ.RINGS(IRING)) THEN
               CALL GETRING(IRING)		!Get orbital data
               IF (SMAA.EQ.0.) GOTO 20		!Skip zero ring
               MINDIS = 16.0
               MAXDIS = 32.0
               CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,
     &               RA,RC,OAL,OAS,ZSCALE,SL,SS,NINT(NLDS/ZOOM),
     &               NINT(NSDS/ZOOM),0,0,0,0,mindis,maxdis,RPTS,NRPTS) 
               IF (NRPTS.EQ.0) THEN
                  CALL XVMESSAGE('***Ring is not in picture - 1',' ')
               ELSE
                  CALL DRAWCURVE(RPTS,NRPTS,0)
               ENDIF
               GOTO 20
            ENDIF
         ENDDO
         CALL XVMESSAGE('***Invalid ring ID',' ')
         GOTO 20
      ENDIF
C
      IF (XVIPTST('SCPTS')) THEN	! Redraw the computed pts
          IF (NRPTS.GT.0) CALL DRAWCURVE(RPTS,NRPTS,1)
          GOTO 20
      ENDIF
C
      IF (XVIPTST('SAPTS')) THEN	! Redraw the acquired pts
          IF (NRPTS.GT.0) CALL DRAWCURVE(ARPTS,NRPTS,1)
          GOTO 20
      ENDIF
C
      IF (XVIPTST('SRINGS')) THEN
         DO IRING=2,NRINGS
            CALL GETRING(IRING)	!Get orbital data
            MINDIS = 16.0
            MAXDIS = 32.0
            CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,
     &          RA,RC,OAL,OAS,ZSCALE,SL,SS,NINT(NLDS/ZOOM),
     &          NINT(NSDS/ZOOM),0,0,0,0,mindis,maxdis,RPTS,NRPTS)
            IF (NRPTS.GT.0) CALL DRAWCURVE(RPTS,NRPTS,0)
         ENDDO
         GOTO 20
      ENDIF

      IF (PARMTST('MASTER',RING_ID(1),I)) THEN
         CALL UPRCASE(RING_ID)
         DO J=1,15
            IF (RING_ID(1).EQ.RINGS(J)) THEN
               MRING=J
               GOTO 20
            ENDIF
         ENDDO
         CALL XVMESSAGE('***Invalid ring name',' ')
         GOTO 20
      ENDIF

      IF (RPARMTST('PREDICT',PINPUT,IC)) THEN
         CALL PREDICT(PIC,HPIC,NL,NS,IND,IC,PINPUT)
         GOTO 20
      ENDIF

      IF (XVIPTST('ERING')) THEN
         CALL ERING
         GOTO 20
      ENDIF

      IF (XVIPTST('EDIT')) THEN
         CALL EDITNAV(IMG,PIC,HPIC,NL,NS)
         GOTO 20
      ENDIF

      IF (XVIPTST('PARAMS')) THEN
         CALL RPARAM
         GOTO 20
      ENDIF

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 20
      CALL RDISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 20
      GOTO 20
C
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Locate the ring by scanning for high-contrast points.
C
      SUBROUTINE RINGSCAN(PIC,HPIC,NL,NS,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      LOGICAL RPARMTST,XVIPTST
      INTEGER*2 HPIC(NS,NL)

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      COMMON/CRINGJ/NPTS(10)
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

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

      real*4 mindis,maxdis,deldis
      CHARACTER*80 MSG
  111 FORMAT('North angle has been shifted by',F8.3,' degrees')

      JSL = 1
      JSS = 1
      JNL = NL
      JNS = NS		!Reset scan area to whole frame

      CALL RINGIDS(PIC,HPIC,NL,NS,1,DUMMY)
      CALL MOVERING(*999)
      IF (NRF.EQ.0) THEN
          CALL XVMESSAGE('***No rings selected',' ')
          RETURN1
      END IF

   16 CALL XVINTRACT('QUERRY',
     &        ' Do you wish to do a RING scan? (Enter ''Y or ''N)')
      IF (XVIPTST('N')) RETURN
      IF (.NOT.XVIPTST('Y')) GOTO 16

   20 CALL XVINTRACT('QUERRY',
     & ' Do you wish to specify the scan area? (Enter ''Y or ''N)')
      IF (XVIPTST('N')) GOTO 30
      IF (.NOT.XVIPTST('Y')) GOTO 20
      CALL CAREA(JSL,JSS,JNL,JNS)	! User specifies scan area
      MINDIS = 2.0
      MAXDIS = 4.0
      CALL RINGPTS(1,NLW,NSW,NSEARCH,MINDIS,MAXDIS,*999)

   30 CALL FITPARAMS(ITYPE,xoal,xoas)
      IF (ANGLA.LT.10.*DTR) THEN 	!Apply correction to C matrix
         CALL XVMESSAGE(
     .        'NOTE: ANGLA (Optic axis to Planet pole) <10 deg', ' ')
         CALL XVMESSAGE(
     .        'Making pointing correction directly to C-Matrix',' ')
      ENDIF

      ANGLN0 = ANGLN
      NSW1 = 1
      NSEARCH1 = NSEARCH
      DELTAD1 = 1.0
      LOOP_COUNT = 0
C
C            Iterate through several ring scans until we get close
   90 IPT = 1
      DO I=1,NRF
          NPT = NPTS(I)
          ETYPE = EDGE(I)
          WIDTH = RWIDTH(I)
          CALL RSEARCH(ICODE,PIC,HPIC,NL,NS,RPTS(1,IPT),ARPTS(1,IPT),
     &		NPT,NLW,NSW1,NSEARCH1,DELTAD1,ETYPE,WIDTH,0)
          IPT = IPT + NPT
      ENDDO
C	 ....Delete points near Voyager reseau
      DELDIS = 5.0
      IF (IPROJ.EQ.4) CALL CLEANVGR(ARPTS,NRPTS,RES,DELDIS)
      CALL CLEANPT1(RPTS,ARPTS,NRPTS,deldis)		!Delete isolated points
      IF (NRPTS.EQ.0) THEN
         CALL XVMESSAGE('***No ring points found',' ')
         RETURN1
      ENDIF
      CALL DRAWCURVE(ARPTS,NRPTS,1)
      IF (IFIT.EQ.2) THEN
          CALL CHISQ2(IND,RPTS,ARPTS,NRPTS,dl,ds,0)	!Find offests (DL,DS)
      ELSE
          CALL CHISQ3(IND,RPTS,ARPTS,NRPTS,XOAL,XOAS,dl,ds,dt,0)
          ANGLN = ANGLN + DT
      ENDIF

      IF (IND.EQ.0) RETURN1
      IF (ITYPE.EQ.7) THEN	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF

      IF (ANGLA.LT.10.*DTR) THEN  	!corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      ELSE ! Regular way
         CALL MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE) !Update ANGLA and ANGLB
      ENDIF

      CALL UPDATENAV		!Update C and all OM matrices
      MINDIS = 1.0
      MAXDIS = 3.0
      CALL RINGPTS(1,NLW,NSW,NSEARCH,MINDIS,MAXDIS,*999)
      DIFF = DL**2 + DS**2
      IF (DIFF.LT.0.5.AND.DABS(DT).LT.0.01) GOTO 100 !VRH 6/28/89 abs of DT
      NSEARCH1 = 7			!For next pass, tighten search radius
      DELTAD1 = 0.5			!and use 0.5 pixel search steps
      LOOP_COUNT = LOOP_COUNT + 1
      IF (LOOP_COUNT.LT.3) GOTO 90
      CALL XVMESSAGE('***RING Scan converging slowly',' ')
      CALL XVMESSAGE('***There may be some bad points',' ')

  100 IPT = 1
      DO I=1,NRF
          NPT = NPTS(I)
          ETYPE = EDGE(I)
          WIDTH = RWIDTH(I)
	  DELTAD2 = 0.5
          CALL RSEARCH(ICODE,PIC,HPIC,NL,NS,RPTS(1,IPT),ARPTS(1,IPT),
     &		NPT,NLW,NSW,3,DELTAD2,ETYPE,WIDTH,0)
          IPT = IPT + NPT
      ENDDO

      DELDIS = 5.0
      IF (IPROJ.EQ.4) CALL CLEANVGR(ARPTS,NRPTS,RES,DELDIS)
      IF (NRPTS.EQ.0) THEN
              CALL XVMESSAGE('***No ring points found',' ')
              RETURN1
      ENDIF
      CALL DRAWCURVE(ARPTS,NRPTS,1)

  115 CALL XVINTRACT('QUERRY',
     &        ' Do you wish to clean the curve? (Enter ''Y or ''N)')
      IF (.NOT.XVIPTST('Y').AND..NOT.XVIPTST('N')) GOTO 115
      IF (XVIPTST('Y')) THEN
  120     CALL XVINTRACT('ACLEAN',
     &        ' Enter ''CLEAN,''H,CZOOM,STRETCH,ASTR, or ''EXIT')
          IF (XVIPTST('EXIT')) GOTO 130

          IF (RPARMTST('CLEAN',deldis,I)) THEN
            CALL CLEANPTS(ARPTS,NRPTS,deldis)
            GOTO 120
          ENDIF

          CALL DISPLAY(PIC,HPIC,NL,NS,IND)
          IF (IND.EQ.1.AND.NRPTS.GT.0) CALL DRAWCURVE(ARPTS,NRPTS,1)
          GOTO 120
      ENDIF

  130 IF (IFIT.EQ.2) THEN
          CALL CHISQ2(IND,RPTS,ARPTS,NRPTS,DL,DS,1)
      ELSE
          CALL CHISQ3(IND,RPTS,ARPTS,NRPTS,XOAL,XOAS,DL,DS,DT,1)
          ANGLN = ANGLN + DT
      ENDIF
      IF (IND.EQ.0) RETURN1
      IF (ITYPE.EQ.7) THEN	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF

      IF (ANGLA.LT.10.*DTR) THEN  !VRH 7/4/89 apply corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      ELSE ! Regular way
         CALL MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE)
      ENDIF
      CALL UPDATENAV
      DIFF = DSQRT(DL**2 + DS**2)
      CALL PRNT(7,1,DIFF,' Final shift (pixels)=.')
      IF (IFIT.EQ.3) THEN
           WRITE(MSG,111) (ANGLN-ANGLN0)*RTD
           CALL XVMESSAGE(MSG,' ')
      ENDIF
      MINDIS = 1.0
      MAXDIS = 3.0
      CALL RINGPTS(1,NLW,NSW,NSEARCH,MINDIS,MAXDIS,*999)
      RETURN
C
  999 RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to manually trace a ring or system of rings.  If radii are
C specified, rings are assumed to be circular and lie in the SPECIFIED
C plane (MASTER by default).  Ring may also be identified by name, in 
C which case orbital data is fetched.
C
C If IMODE=0, fit only...
C
C
      SUBROUTINE RINGTRACE(PIC,HPIC,NL,NS,SRPTS,IMODE,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 SRPTS(2,2000)	!Work area to hold points from SRCHINV

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT
 
      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      COMMON/CRINGJ/NPTS(10)
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 RINGID,EDGE,ETYPE,PLANE
      CHARACTER*1 RINGS(15)

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

      REAL*4 MINDIS,MAXDIS
      LOGICAL XVIPTST
      CHARACTER*80 MSG
  110 FORMAT('(DL,DS)=(',F9.3,',',F9.3,')  DT=',F8.4,' degrees')
  111 FORMAT('Total shift in north angle=',F8.3,' degrees')

      DT = 0.D0 !VRH 6/28/89 in case CHI3 was set earlier and IFIT=2
      IF (IMODE.EQ.0) GOTO 55
      CALL RINGIDS(PIC,HPIC,NL,NS,0,SRPTS)
      IF (NRPTS.LT.3) THEN
           CALL XVMESSAGE('***Insufficient number of points specified',
     .        ' ')
           CALL XVMESSAGE('***Ring trace terminated',' ')
           RETURN1
      ENDIF

   50 CALL XVINTRACT('CHISQ',
     &   ' Specify type of fit (Enter ''CHI2, ''CHI3 or ''EXIT)')
      IF (XVIPTST('EXIT')) RETURN1 !VRH 7/4/89 option added
      IF (XVIPTST('CHI2')) THEN
          IFIT = 2
      ELSE
          IF (XVIPTST('CHI3')) THEN
               IFIT = 3
          ELSE
               GOTO 50
          ENDIF
      ENDIF

   55 IF (ITYPE.EQ.7) THEN
         XOAL = OAL_IS
         XOAS = OAS_IS
      ELSE
         XOAL = OAL
         XOAS = OAS
      ENDIF

      IF(ANGLA.LT.10.*DTR) THEN !VRH 7/26/89 automatic direct CM correction
         CALL XVMESSAGE(
     .        'NOTE: ANGLA (Optic axis to Planet pole) <10 deg',' ')
         CALL XVMESSAGE(
     .        'Making pointing correction directly to C-Matrix',' ')
      ENDIF

      CALL GETNAV	!Restore planet coordinate system & ANGLN VRH 6/28/89
      ANGLN0 = ANGLN
      LOOP_COUNT = 0
      IMARGIN = 500	!Add margin around picture for safety's sake
      MINDIS = 2.0	!Minimum and maximum pixel spacing between
      MAXDIS = 4.0	!computed points
      IFLAG = 0		!Flag for last iteration

   60 IPT = 1

      DO I=1,NRF
          IRING = RINGID(I)
          CALL GETRING(IRING)
          IF (IRING.EQ.1) THEN !VRH IRING=1 can be in any plane 6/7/89
             CALL GETRING(PLANE(I))
             SMAA = RADII(I)
             ECC = 0.0D0
          ENDIF
          CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &       OAL,OAS,ZSCALE,-IMARGIN,-IMARGIN,NL+2*IMARGIN,NS+2*IMARGIN,
     &       0,0,0,0,MINDIS,MAXDIS,SRPTS,NSRPTS)  !VRH 8/12/89 fix
          IF (NSRPTS.EQ.0) THEN
               CALL XVMESSAGE('***Ring is not in picture - 2',' ')
               RETURN1
          ENDIF
          NPT = NPTS(I)
          CALL SRCHINV(SRPTS,NSRPTS,ARPTS(1,IPT),NPT,RPTS(1,IPT))
          IPT = IPT + NPT
      ENDDO

      IF (IBUG.EQ.1) THEN  !VRH 7/29/89 follow fits
          CALL DRAWCURVE(SRPTS,NSRPTS,1)
          CALL DRAWCURVE(ARPTS,NRPTS,0)
      ENDIF

      CALL GETNAV	!Restore planet coordinate system & ANGLN VRH 6/28/89
      IF (IFIT.EQ.2) THEN
           CALL CHISQ2(IND,RPTS,ARPTS,NRPTS,DL,DS,IFLAG)
      ELSE
           CALL CHISQ3(IND,RPTS,ARPTS,NRPTS,XOAL,XOAS,
     &            DL,DS,DT,IFLAG)
           ANGLN = ANGLN + DT
      ENDIF
      IF (IND.EQ.0) RETURN1
      IF (ITYPE.EQ.7) THEN	!Scale displacements to object-space
         DL = DL*CTOS
         DS = DS*CTOS
      ENDIF
      WRITE(MSG,110) DL,DS,DT*RTD
      CALL XVMESSAGE(MSG,' ')

      IF (ANGLA.LT.10.*DTR) THEN  !VRH 7/4/89 apply corrections directly to CM
         SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
         CALL ROTATE1(CM,-DL/SCALE,1)
         CALL ROTATE1(CM,DS/SCALE,2)
         IF(IFIT.EQ.3) CALL ROTATE1(CM,DT,3)
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      ELSE ! Regular way
         CALL MOVE2(DL,DS,SCLAT,SCLON,PSC3,ANGLN,ANGLA,ANGLB,*999)
      ENDIF
      CALL UPDATENAV	!Update C and all OM-matrices

      DIFF = DL**2 + DS**2
      IF (IFLAG.EQ.1) GOTO 70
      IF (DIFF.LT.0.01.AND.DABS(DT).LT.0.001) IFLAG=1 !Set up for last iteration
      IMARGIN = 50
      MINDIS = 1.0
      MAXDIS = 3.0
      LOOP_COUNT = LOOP_COUNT + 1
      IF (LOOP_COUNT.LT.15 .OR. IFLAG.EQ.1) GOTO 60	!Iterate again
      CALL XVMESSAGE('***Ring fit does not converge',' ')
      RETURN1
C
   70 DIFF = DSQRT(DIFF)
      CALL PRNT(7,1,DIFF,' Final shift (pixels)=.')
      IF (IFIT.EQ.3) THEN
          WRITE(MSG,111) (ANGLN-ANGLN0)*RTD
          CALL XVMESSAGE(MSG,' ')
      ENDIF
      RETURN

  999 RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Subroutine PREDICT to predict the position of an object in one frame
C  from its position in another.  Two modes of operation:  Predicting
C  position of object in another frame from its current position (cursor
C  is used) or predicting the position of an object in the current frame
C  (cursor is placed) from its position in another frame...
C
C  If Count = 1, [PREDICT=FRAME]: The cursor position is read to determine
C  the objects current MASTER system longitude.  Then its longitude in
C  FRAME is predicted.  Only the longitude is derived from the cursor, for
C  MASTER=P then the radius is also taken from the cursor position. 
C
C  If Count = 2, [PREDICT=(FRAME,RADIUS)] then the position in FRAME
C  is predicted from the given RADIUS and current position.  Uses circular
C  orbit in MASTER plane at given RADIUS.  LONGITUDE used is determined from
C  cursor.
C
C  If Count = 2, [PREDICT=(FRAME,LONGITUDE)] then the position in the current
C  frame is predicted from the given LONGITUDE and FRAME.  If in present
C  field, the cursor is placed there.  Uses orbit defined by MASTER ring.
C  IF MASTER=P must also specify radius.
C
C  If Count = 3, [PREDICT=(FRAME,LONGITUDE,RADIUS)] same as above but for a
C  circular orbit in the MASTER plane.
C
      SUBROUTINE PREDICT(PIC,HPIC,NL,NS,IND,COUNT,PINPUT) 
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*8 M,N_SQRD,N
      REAL*4 PINPUT(3)
      INTEGER COUNT,FRAME_1

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRINGC/ RINGS
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
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

      DOUBLE PRECISION GM(4)/0.0,37931200.0,5793939.0,6836937.909/
      DOUBLE PRECISION J2(4)/0.0,00.0162992,0.0033461,0.003384825/
      DOUBLE PRECISION J4(4)/0.0,-0.0009167,-0.0000321,0.0/
      DOUBLE PRECISION J6(4)/0.0,0.0000813,0.0,0.0/

      COMMON/CONST/PI,DTR,RTD

      include 'fortport'

      integer xdcset

      CHARACTER*80 MSG
      LOGICAL NOW ! Predict for current frame?

  110 FORMAT('(L,S)=(',F7.2,',',F7.2,') (R,LON)=(',
     &         F10.1,',',F7.2,')')
  111 FORMAT('Reference plane is that of ',A1,'-Ring')
  112 FORMAT('PHASE=(',F7.2,')   DN=(',I6,')')
  113 FORMAT('Longitude predicted for frame ',I7,' is ',F6.2,' deg')
  114 FORMAT('Radius value derived from cursor: ',F10.1,' Km')
  115 FORMAT('Number of orbits: ',F8.2)

      IF (IPROJ.EQ.4) THEN  !Voyager VRH fix 10/10/02
        ITIME0 = FRAME_ID/100      ! # FDS hours   (48 min)
        ITIME1 = MOD(FRAME_ID,100) ! # FDS minutes (48 sec)
	FRAME_1 = NINT(PINPUT(1))
        ITIME0_1 = FRAME_1/100
        ITIME1_1 = MOD(FRAME_1,100)
        DTIME = (ITIME0_1 - ITIME0)*48.*60. + (ITIME1_1 - ITIME1)*48. !Seconds
      ELSE IF (IPROJ.EQ.6) THEN !Cassini SCLK is in seconds VRH add 10/10/02
        DTIME = FRAME_1 - FRAME_ID
      ELSE
       CALL XVMESSAGE('Time/FRAME_ID relation unknown for this mission',
     .              ' ')
        RETURN
      ENDIF

      CALL GETRING(MRING)
      EC = ECC  ! Eccentricity of orbit used in calculations
      IF (COUNT.EQ.1) THEN  ! Use present position to predict in past/future
C PREDICT=(FRAME)
          NOW = .FALSE.
          CALL CURSOR(RLINE,RSAMP) ! Use cursor to find LONGITUDE
          CALL LATLON(ISTATUS,RLINE,RSAMP,RADIUS,RLON)
          IF (MRING.NE.1)  THEN
              RADIUS = SMAA ! Use defined RADIUS & ECC
          ELSE
              WRITE(MSG,114) RADIUS
              CALL XVMESSAGE(MSG,' ')
          ENDIF
      ELSE IF (COUNT.EQ.2) THEN
C PREDICT=(FRAME,RADIUS) or PREDICT=(FRAME,LONGITUDE)
          IF (PINPUT(2).LT.360.) THEN !Predict current using other Long.
              NOW = .TRUE.
              RLON = PINPUT(2)*DTR
              IF (MRING.NE.1) THEN
	          RADIUS = SMAA
              ELSE
                  CALL XVMESSAGE('***Must specify radius for MASTER=P',
     .                           ' ')
                  RETURN
              ENDIF
          ELSE !Predict other using given Radius
              NOW = .FALSE.
              CALL CURSOR(RLINE,RSAMP) ! Use cursor to find LONGITUDE
              CALL LATLON(ISTATUS,RLINE,RSAMP,RADIUS,RLON)
              RADIUS = PINPUT(2) ! Use given radius
              CALL XVMESSAGE('Using circular orbit at specified radius',
     .                    ' ')
              EC = 0.  !Circular orbit
          ENDIF
      ELSE
C PREDICT=(FRAME,LONGITUDE,RADIUS)
          NOW = .TRUE.
          RLON = PINPUT(2)*DTR
          RADIUS = PINPUT(3)
          CALL XVMESSAGE('Using circular orbit at specified radius',' ')
          EC = 0.  !Circular orbit
      ENDIF
      IF (NOW) DTIME = -DTIME

C True anomaly to mean anomaly using Elliptical anomaly
C
      EL =  2.D0*DATAN(DSQRT((1.D0+EC)/(1.D0-EC))*DTAN(RLON/2.D0))
      M = EL - EC*DSIN(EL)
C
C Calculate change to mean anomaly from mean motion
C
      SCALE = RA/RADIUS
      SCALE_2 = SCALE   * SCALE
      SCALE_4 = SCALE_2 * SCALE_2
      SCALE_6 = SCALE_2 * SCALE_4
C
C  Mean motion
      N_SQRD=1.0 + SCALE_2*J2(PLANET_ID-4)* 3.0/ 2.0*(1.D0+2.D0*EC*EC)
     #           - SCALE_4*J4(PLANET_ID-4)*15.0/ 8.0
     #           + SCALE_6*J6(PLANET_ID-4)*35.0/16.0

      N_SQRD = N_SQRD * GM(PLANET_ID-4)/RADIUS/RADIUS/RADIUS
      N = DSQRT(N_SQRD)
C
      M = M + DTIME*N
c      print*,'mean motion = ',n*RTD,' degrees/sec'
c      print*,'new m = ',m*RTD
      WRITE(MSG,115) (DTIME*N)/(2.D0*PI)
      CALL XVMESSAGE(MSG,' ')
C
C Mean anomaly to true anomaly (Iteration)
C
C      RLON = M + (2.D0*EC-EC*EC*EC/4.D0)*DSIN(M) 
C     #         + 3.D0/4.D0*EC*EC*DSIN(2.D0*M) 
C     #         + 13.D0/12.D0*EC*EC*EC*DSIN(3.D0*M)
C
      EL1 = M
      DO I = 1,1000
         EL = M + EC*DSIN(EL1)
         IF(ABS(EL-EL1).LT.1.D-7) GOTO 10
         EL1 = EL
      END DO
      CALL XVMESSAGE('*** Elliptical anomaly converges slowly',' ')
10    RLON = 2.D0*DATAN(DSQRT((1.D0-EC)/(1.D0+EC))*DTAN(EL/2.D0))
      RLON = MOD(RLON,2.D0*PI)
      IF (RLON.LT.0.) RLON = RLON + 2.D0*PI

      IF (.NOT.NOW) THEN
          IF (MRING.EQ.1) THEN
              CALL XVMESSAGE('Reference plane is planet''s equator',' ')
          ELSE
              WRITE(MSG,111) RINGS(MRING)
              CALL XVMESSAGE(MSG,' ')
          ENDIF
          WRITE(MSG,113) FRAME_1,RLON*RTD
          CALL XVMESSAGE(MSG,' ')
      ELSE
C     Convert (RADIUS,RLON) to planet centered (X,Y,0)
          CONSTANT = RADIUS*(1.D0-EC**2)
          R = CONSTANT/(1.D0+EC*DCOS(RLON)) !Radius of orbit at RLON
C
          CALL LINSAM(ISTATUS,R,RLON,rline,rsamp) !Compute (l,s)
          IF (ISTATUS.EQ.1) THEN
              IF (MRING.EQ.1) THEN
                 CALL XVMESSAGE('Reference plane is planet''s equator',
     .                          ' ')
              ELSE
                  WRITE(MSG,111) RINGS(MRING)
                  CALL XVMESSAGE(MSG,' ')
              ENDIF
              WRITE(MSG,110) RLINE,RSAMP,R,RLON*RTD
              CALL XVMESSAGE(MSG,' ')
              CALL PHASE(R,RLON,PHA,DUMMY,DUMMY,DUMMY) !VRH 3/1/94 new arg
              IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &          RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
              ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
              ELSE
                IDN = HPIC(RSAMP,RLINE)
              ENDIF
              WRITE(MSG,112) PHA*RTD,IDN
              CALL XVMESSAGE(MSG,' ')
              ILINE = (RLINE-SL)*ZOOM + 1.5
              ISAMP = (RSAMP-SS)*ZOOM + 1.5
              IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &	      .AND.ISAMP.LE.NSDS) THEN
                  ISTATUS=XDCSET(IDEV,TB,ISAMP,ILINE)
              ELSE
                  CALL XVMESSAGE('***Position not in field',' ')
              ENDIF
          ENDIF
      END IF

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to get ring ID from user and load CMAP with ring data.
C The points on the ring RPTS are computed and displayed in graphics.
C
      SUBROUTINE RINGIDS(PIC,HPIC,NL,NS,MODE,SRPTS)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 SRPTS(2,5000)

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      COMMON/CRINGJ/NPTS(10)
      REAL*4 RPTS,ARPTS,RADII,RWIDTH,width,rrad
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

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
      CHARACTER*1 RING_ID(20)
      LOGICAL PARMTST,RPARMTST,XVIPTST
      CHARACTER*80 CMSG
      real*4 mindis,maxdis

      NRF = 0		!Number of rings used in fit
      NRPTS = 0		!Total number of computed points

      IF (MRING.EQ.1) THEN ! VRH added to indicate plane 6/7/89
          CALL XVMESSAGE('Default plane for RADIUS is planet''s equator'
     .                  ,' ')
      ELSE
          WRITE(CMSG,115) RINGS(MRING)
115       FORMAT('Default plane for RADIUS is that of ',A1,'-Ring')
          CALL XVMESSAGE(CMSG,' ')
      ENDIF

   10 CALL XVMESSAGE('Enter ring radius(km) [and plane] or ring name',
     .        ' ')
      CALL XVINTRACT('RINGRAD',' Enter RADIUS, PLANE, RING or ''EXIT')
      IF (XVIPTST('EXIT')) RETURN

      IF (RPARMTST('RADIUS',rrad,I)) THEN
	   R = rrad	! real*4 -> real*8
           IF (R.LE.0.0D0) GOTO 10
           IRING = 1
           IPLANE = MRING
           IF (PARMTST('PLANE',RING_ID(1),I)) THEN
               CALL UPRCASE(RING_ID)
               DO J=1,15  ! To include new rings VRH 6/7/89
                   IF (RING_ID(1).EQ.RINGS(J)) THEN
                       IPLANE = J			!Set the plane-id
                       GOTO 12
                   ENDIF
                ENDDO
                CALL XVMESSAGE('***Invalid ring ID',' ')
                GOTO 10
           END IF
           GOTO 12
      ENDIF

      IF (PARMTST('RING',RING_ID(1),I)) THEN
           CALL UPRCASE(RING_ID)
           DO J=1,15  ! To include new rings VRH 6/7/89
               IF (RING_ID(1).EQ.RINGS(J)) THEN
                   IRING = J			!Set the ring-id
                   IF (IRING.GT.1) GOTO 12
                   GOTO 10
               ENDIF
          ENDDO
          CALL XVMESSAGE('***Invalid ring ID',' ')
          GOTO 10
      ENDIF
      GOTO 10
C           Check to see if ring is in picture...
   12 CALL GETRING(IRING)		!Get orbital data
      IF (IRING.EQ.1) THEN
         CALL GETRING(IPLANE)		!Get orbital data
         SMAA = R		!Set ring radius
         ECC  = 0.D0		!Set ring eccentricity
         IF (IPLANE.EQ.1) THEN ! VRH added to indicate plane 6/7/89
            CALL XVMESSAGE('Plane is planet''s equator',' ')
         ELSE
            WRITE(CMSG,116) RINGS(IPLANE)
116         FORMAT('Plane is that of ',A1,'-Ring')
            CALL XVMESSAGE(CMSG,' ')
         ENDIF
      ENDIF
      IF (SMAA.EQ.0.) THEN     !Skip zero ring VRH 6/14/89
          CALL XVMESSAGE('***Ring SMAA not defined',' ')
          GOTO 10
      END IF

      IF (MODE.EQ.1) THEN
	   mindis = 16.0
	   maxdis = 32.0
           CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &           OAL,OAS,ZSCALE,JSL,JSS,JNL,JNS,0,0,0,0,
     &           mindis,maxdis,RPTS(1,NRPTS+1),NPT)
           IF (NPT.EQ.0) THEN
                CALL XVMESSAGE('***Ring is not in picture - 3',' ')
                GOTO 10
           ENDIF
           CALL DRAWCURVE(RPTS(1,NRPTS+1),NPT,0)
   14      CALL XVINTRACT('RSEARCH',
     &           'Specify edge type (Enter ''OUTER,''INNER, or WIDTH)')
           IF (XVIPTST('OUTER')) ETYPE = 1
           IF (XVIPTST('INNER')) ETYPE = 2
C           Reverse direction of edge if the south side of the ring is visible.
           IF (SCLAT.LT.0.0D0) ETYPE = 3-ETYPE
           IF (RPARMTST('WIDTH',WIDTH,I)) ETYPE = 3
           IF (ETYPE.EQ.0) GOTO 14
      ELSE
	   mindis = 16.0
	   maxdis = 32.0
           CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &      OAL,OAS,ZSCALE,-500,-500,NL+2*500,NS+2*500,0,0,0,0,
     &      mindis,maxdis,SRPTS,NSRPTS)  
           CALL TRACECRV(PIC,HPIC,NL,NS,ARPTS(1,NRPTS+1),NPT)
           IF (NPT.EQ.0) GOTO 10
           ETYPE = 0
           WIDTH = 0
      ENDIF

      NRF = NRF + 1
      RINGID(NRF) = IRING
      RADII(NRF) = SMAA
      PLANE(NRF) = IPLANE
      EDGE(NRF) = ETYPE
      RWIDTH(NRF) = WIDTH
      NPTS(NRF) = NPT
      NRPTS = NRPTS + NPT
      IF (NRF.LT.10.AND.NRPTS.LT.1000) GOTO 10

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute ring points for a system of rings.
C Outputs: NRPTS,RPTS,NPTS
C NRF may also be updated...
C
      SUBROUTINE RINGPTS(IMODE,NLW,NSW,NSEARCH,MINDIS,MAXDIS,*)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      COMMON/CRINGJ/NPTS(10)
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

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
      REAL*4 MINDIS,MAXDIS

      NRPTS = 0

      DO I=1,NRF
          IRING = RINGID(I)
          CALL GETRING(IRING)
          IF (IRING.EQ.1) THEN !VRH IRING=1 can be in any plane 6/7/89
             CALL GETRING(PLANE(I))
             SMAA = RADII(I)
             ECC = 0.0D0
          ENDIF
          CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &        OAL,OAS,ZSCALE,JSL,JSS,JNL,JNS,NLW,NSW,NSEARCH,
     &        IMODE,MINDIS,MAXDIS,RPTS(1,NRPTS+1),NPT)
          IF (NPT.EQ.0) THEN
                CALL XVMESSAGE('***Ring is not in picture - 4',' ')
                RETURN1
          ENDIF
          IF (NRPTS+NPT.GT.20000/MINDIS) THEN
              CALL XVMESSAGE('***Maximum number of points exceeded',' ')
              CALL XVMESSAGE('***Remaining rings ignored',' ')
              NRF = I
              RETURN
          ENDIF
          NPTS(I) = NPT
          NRPTS = NRPTS + NPT
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute ring points at a specified ring RADIUS.
C
C Outputs: RPTS(2,npts) contains the ring-points (line,sample) pairs.
C	   NPTS is the total number of points returned.
C
C   IMODE=1 if ring points in front of planet are to be flagged
C        =0 otherwise
C   MINDIS = minimum pixel spacing between ring points
C   MAXDIS = maximum pixel spacing between ring points
C       if (MINDIS.EQ.0) then constant angular spacing will be used.
C
C Obscured points are stored as (-line,-samp) and are spaced 8 pixels
C apart so that they form a broken-lined curve.
C
      SUBROUTINE RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &     OAL,OAS,ZSCALE,ISL,ISS,INL,INS,NLW,NSW,NSEARCH,
     &     IMODE,MINDIS,MAXDIS,rpts,npts)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      REAL*8 OM(3,3),PSC3(3),PSUN3(3)
      REAL*4 RPTS(2,5000),MINDIS,MAXDIS,osnl,osns,rline,rsamp
      REAL*8 LINE1,SAMP1,LINE2,SAMP2,MINDIS2,MAXDIS2
      REAL*8 PI/3.141592653589793D0/

      TWOPI = 2.D0*PI
      DTR = PI/180.D0
      EPSLN = (RA/RC)**2
      EPSLN2 = PSC3(1)**2 + PSC3(2)**2 + EPSLN*PSC3(3)**2 - RA**2

      MINDIS2 = MINDIS**2
      MAXDIS2 = MAXDIS**2
C           Compute picture window
      RSL = ISL + NSEARCH/2 + NLW/2		! Starting line
      RSS = ISS + NSEARCH/2 + NSW/2		! Starting sample
      REL = ISL + INL - 1 - NSEARCH/2 - NLW/2	! Ending line
      RES = ISS + INS - 1 - NSEARCH/2 - NSW/2	! Ending sample
C     ....Compute approximate object-space frame size
      IF (ITYPE.EQ.7) THEN
          OSSS = 1.
          OSSL = 1.
          CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),osnl,osns,
     &		1,CONV,NPH,NPV,ind)
          RSL = MAX(RSL,1.D0)
          RSS = MAX(RSS,1.D0)
          REL = MIN(REL,DFLOAT(NL))
          RES = MIN(RES,DFLOAT(NS))
      ELSE
          OSSS = MIN(SS,1)  !Edge of data frame or screen image
          OSSL = MIN(SL,1)
          OSNL = MAX(DFLOAT(NL),DFLOAT(NLDS)/ZOOM + SL)
          OSNS = MAX(DFLOAT(NS),DFLOAT(NSDS)/ZOOM + SS)
      ENDIF
C
      A0 = PSUN3(1)**2 + PSUN3(2)**2 + EPSLN*PSUN3(3)**2
      CONSTANT = SMAA*(1.D0-ECC**2)
C            Initialize longitude step size DLON
      CALL RINV(IND,SMAA,SCLON,line1,samp1,
     &              OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      CALL RINV(IND,SMAA,SCLON+DTR,line2,samp2,
     &              OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      DLON = MAXDIS*DTR/DSQRT((LINE2-LINE1)**2+(SAMP2-SAMP1)**2)

      RLON = 0.0D0
      RLINE0 = -999.D0
      STEP = DTR                ! VRH 10/9/89 introduce STEP 
      NPTS = 0			! Number of ring points
C
C Loop to compute the ring points: Points are accumulated at approximately
C 1 pixel intervals around the ring, from 0 degrees longitude to 360
C degrees longitude.  If the north side of the ring is visible (SCLAT>0),
C then the points will appear to increment clockwise around the ring.
C If the south side of the ring is visible (SCLAT<0), then the points
C will increment counter-clockwise around the ring.
C
C          Convert (RADIUS,RLON) to planet centered (x,y,0)
   40 RADIUS = CONSTANT/(1.D0+ECC*DCOS(RLON))
      x = RADIUS*DCOS(RLON-RLORA)
      y = RADIUS*DSIN(RLON-RLORA)
C          Compute vector from Spacecraft to point on ring
      xc =  x - PSC3(1)
      yc =  y - PSC3(2)
      zc =    - PSC3(3)
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
C NOTE: The current method may miss ring entirely if range
C       of longitude in frame is less than a degree VRH 10/9/89
C    
      IF (RLINE.LT.OSSL.OR.RLINE.GT.OSNL.OR.
     &    RSAMP.LT.OSSS.OR.RSAMP.GT.OSNS) THEN  !If point is not in frame
             RLINE0 = -999.D0
             RLON = RLON + STEP		        !step one degree or DLON
             GOTO 92				!and continue.
      ELSE IF(RLINE0.EQ.-999.D0) THEN           !Found point while searching
             IF(STEP.EQ.DTR) THEN
                 RLON = RLON - DTR              !Backup and step at DLON
                 STEP = DLON                    !to avoid ring starting in
                 GOTO 90                        !middle of frame VRH 10/9/89
             ELSE
                 STEP = DTR                     !Reset STEP
             END IF
      ENDIF
C          Check spacing between points
      IF (MINDIS.GT.0.0.AND.RLINE0.NE.-999.D0) THEN
          DIS = (RLINE-RLINE0)**2 + (RSAMP-RSAMP0)**2
          IF (DIS.LT.MINDIS2) THEN	!If spacing is less than 1 pixel
             RLON = RLON - DLON		!back up
             DLON = 1.5D0*DLON		!and increase step size by 50%.
             RLON = RLON + DLON
             GOTO 40
          ENDIF
          IF (DIS.GT.MAXDIS2) THEN	!If spacing is greater than 3 pixels
             RLON = RLON - DLON		!back up
             DLON = 0.5D0*DLON		!and decrease step size by 50%
             RLON = RLON + DLON
             GOTO 40
          ENDIF
      ENDIF

      RLINE0 = RLINE
      RSAMP0 = RSAMP
      IFLAG = 0			!Clear obscured point flag
C          Check to see if point on ring (x,y,0) is in planet shadow
      B0 = x*PSUN3(1) + y*PSUN3(2)
      C0 = x**2 + y**2 - RA**2
      D0 = B0**2 - A0*C0
      IF (D0.GT.0) THEN			!vector intercepts planet
             R = (-B0-DSQRT(D0))/A0
             IF (R.GT.0) GOTO 90	!and planet is between
      ENDIF
C          See if vector (xc,yc,zc) passes through planet
      A = xc**2 + yc**2 + EPSLN*zc**2
      B = xc*PSC3(1) + yc*PSC3(2) + EPSLN*zc*PSC3(3)
      D = B*B - A*EPSLN2
      IF(D.GE.0.D0) THEN
          R = (-B-DSQRT(D))/A
          IF (R.LT.1.0) GOTO 90		!Ring behind planet
          IF (IMODE.EQ.1) IFLAG=1	!Ring in front of planet
      ENDIF

      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,rline,rsamp,
     &	       RLINE,RSAMP,0,CONV,NPH,NPV,ind)
      IF (RLINE.LT.RSL.OR.RLINE.GT.REL) GOTO 90  !VRH 6/29/89 move tests
      IF (RSAMP.LT.RSS.OR.RSAMP.GT.RES) GOTO 90  !outside of IF statement

      IF (IFLAG.EQ.0) THEN
          NPTS = NPTS + 1
          RPTS(1,NPTS) = RLINE
          RPTS(2,NPTS) = RSAMP
      ELSE
          DIS = (RLINE-SLINE)**2 + (RSAMP-SSAMP)**2
          IF (DIS.GT.64.D0) THEN
              NPTS = NPTS + 1
              SLINE = RLINE
              SSAMP = RSAMP
              RPTS(1,NPTS) = -RLINE
              RPTS(2,NPTS) = -RSAMP
           ENDIF
      ENDIF

      IF (NPTS.EQ.5000) THEN
         CALL XVMESSAGE('***Maximum number of ring points exceeded',' ')
         RETURN
      ENDIF

   90 RLON = RLON + DLON
   92 IF (RLON.LT.TWOPI) GOTO 40

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute ring points at a specified ring RADIUS.
C
C Outputs: RPTS(2,npts) contains the ring-points (line,sample) pairs.
C	   NPTS is the total number of points returned.
C
C   IMODE=1 if ring points in front of planet are to be flagged
C        =0 otherwise
C   MINDIS = minimum pixel spacing between ring points
C   MAXDIS = maximum pixel spacing between ring points
C       if (MINDIS.EQ.0) then constant angular spacing will be used.
C
C Obscured points are stored as (-line,-samp) and are spaced 8 pixels
C apart so that they form a broken-lined curve.
C
C Routine to compute ring points at a specified ring RADIUS.
C
C Outputs: RPTS(2,npts) contains the ring-points as (line,sample) pairs.
C          NPTS is the total number of points returned.
C   MINDIS = minimum pixel spacing between ring points
C   MAXDIS = maximum pixel spacing between ring points
C
C This version only computes ring points between starting and ending
C longitudes SLON and ELON.  Note that no check is made for obscured
C points.
C
      SUBROUTINE RINGPT2(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,
     &     OAL,OAS,ZSCALE,SL0,SS0,NL,NS,
     &     MINDIS,MAXDIS,SLON,ELON,RPTS,NPTS)
      IMPLICIT REAL*8 (A-H,O-Z)
CBTC      INTEGER*4 SL0,SS0,sl,ss,el,es
      INTEGER*4 SL0,SS0
      real*4 sl,ss,el,es

      COMMON/DISTOR/CONV(2216),OAL_IS,OAS_IS,CTOS,PROJECT
      COMMON/DISTORI/ITYPE,NPH,NPV
      REAL*4 CONV
      CHARACTER*5 PROJECT

      REAL*8 OM(3,3),PSC3(3),PSUN3(3)
      REAL*4 RPTS(2,5000),MINDIS,MAXDIS
      REAL*8 LINE1,SAMP1,LINE2,SAMP2,MINDIS2,MAXDIS2
      REAL*8 PI/3.141592653589793D0/
C
      MINDIS2 = MINDIS**2
      MAXDIS2 = MAXDIS**2
C           Compute picture window
      SL = SL0
      SS = SS0
      EL = SL + NL - 1		! Ending line
      ES = SS + NS - 1		! Ending sample
      IF (ITYPE.EQ.7) THEN	!Convert image-space window to object-space
	CALL CONVISOS(PROJECT,ICAM,SL,SS,sl,ss,1,CONV,NPH,NPV,ind)
	CALL CONVISOS(PROJECT,ICAM,EL,ES,el,es,1,CONV,NPH,NPV,ind)
      ENDIF
C
      TWOPI = 2.D0*PI
      DTR = PI/180.D0
      RTD = 180.D0/PI
C            Initialize longitude step size DLON
      CALL RINV(IND,SMAA,SCLON,LINE1,SAMP1,
     &              OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      CALL RINV(IND,SMAA,SCLON+DTR,LINE2,SAMP2,
     &              OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      DLON = MAXDIS*DTR/DSQRT((LINE2-LINE1)**2+(SAMP2-SAMP1)**2)

      CONSTANT = SMAA*(1.D0-ECC**2)
      RLON = SLON
      RLINE0 = -999.D0
      NPTS = 0			! Number of ring points
C
C Loop to compute the ring points: Points are accumulated at approximately
C 1 pixel intervals around the ring, from 0 degrees longitude to 360
C degrees longitude.  If the north side of the ring is visible (SCLAT>0),
C then the points will appear to increment clockwise around the ring.
C If the south side of the ring is visible (SCLAT<0), then the points
C will increment counter-clockwise around the ring.
C
C          Convert (RADIUS,RLON) to planet centered (X,Y,0)
   10 RADIUS = CONSTANT/(1.D0+ECC*DCOS(RLON))
      X = RADIUS*DCOS(RLON-RLORA)
      Y = RADIUS*DSIN(RLON-RLORA)
C          Compute vector from Spacecraft to point on ring
      xc =  X - PSC3(1)
      yc =  Y - PSC3(2)
      zc =    - PSC3(3)
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
C          Check spacing between points
      IF (RLINE0.NE.-999.D0) THEN
          DIS = (RLINE-RLINE0)**2 + (RSAMP-RSAMP0)**2
          IF (DIS.LT.MINDIS2) THEN	!If spacing is less than 1 pixel
             RLON = RLON - DLON		!back up
             DLON = 1.5D0*DLON		!and increase step size by 50%.
             RLON = RLON + DLON
             GOTO 10
          ENDIF
          IF (DIS.GT.MAXDIS2) THEN	!If spacing is greater than 3 pixels
             RLON = RLON - DLON		!back up
             DLON = 0.5D0*DLON		!and decrease step size by 50%
             RLON = RLON + DLON
             GOTO 10
          ENDIF
      ENDIF

      IF (RLINE.LT.SL.OR.RLINE.GT.EL) GOTO 20
      IF (RSAMP.LT.SS.OR.RSAMP.GT.ES) GOTO 20
      NPTS = NPTS + 1
      RPTS(1,NPTS) = RLINE
      RPTS(2,NPTS) = RSAMP
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,rpts(1,npts),
     & rpts(2,npts),RPTS(1,NPTS),RPTS(2,NPTS),0,CONV,NPH,NPV,ind)
      IF (NPTS.EQ.5000) THEN
         CALL XVMESSAGE('***Maximum number of ring points exceeded',' ')
         RETURN
      ENDIF
   20 RLINE0 = RLINE
      RSAMP0 = RSAMP
      RLON = RLON + DLON
      IF (RLON.LT.ELON) GOTO 10

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to manually move the computed ring with the cursor
C
      SUBROUTINE MOVERING(*)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      COMMON/CRINGJ/NPTS(10)
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 RINGID,EDGE,ETYPE,PLANE
      CHARACTER*1 RINGS(15)

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

      common/navdv2/xdw,xdb
      integer xdw,xdb

      real*4 rl4,rs4,mindis,maxdis

      LOGICAL XVIPTST,XST,XDIFILL
C
      CALL XVMESSAGE('Move Cursor to a point on computed RING',' ')
      CALL XVINTRACT('READY',
     &  ' Hit Return when ready')
      IF (XVIPTST('EXIT')) RETURN
      CALL CURSOR(RLINE,RSAMP)
      IF (ITYPE.EQ.7) then
	CALL CONVISOS(PROJECT,ICAM,sngl(RLINE),sngl(RSAMP),rl4,rs4,
     &		1,CONV,NPH,NPV,ind)
	rline=rl4
	rsamp=rs4
      endif
C
   50 CALL XVMESSAGE('Move Cursor to actual RING',' ')
      CALL XVINTRACT('READY',
     &  ' Hit Return when ready or type ''EXIT if done')
      IF (XVIPTST('EXIT')) RETURN
      CALL CURSOR(RLINE2,RSAMP2)
      IF (ITYPE.EQ.7) then
	CALL CONVISOS(PROJECT,ICAM,sngl(RLINE2),sngl(RSAMP2),rl4,rs4,
     &		1,CONV,NPH,NPV,ind)
	rline2=rl4
	rsamp2=rs4
      endif
      DL = RLINE2 - RLINE
      DS = RSAMP2 - RSAMP
      CALL GETNAV		! Restore planet coordinate system
      CALL MOVE2(DL,DS,*999)
      CALL UPDATENAV		! Update C and all OM-matrices
      XST = XDIFILL(IDEV,G,xdb)
      NRPTS = 0

      DO I=1,NRF
           IRING = RINGID(I)
           CALL GETRING(IRING)
           IF (IRING.EQ.1) THEN !VRH IRING=1 can be in any plane 6/7/89
             CALL GETRING(PLANE(I))
             SMAA = RADII(I)
             ECC = 0.0D0
           ENDIF
	   mindis = 16.0
	   maxdis = 32.0
           CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &       OAL,OAS,ZSCALE,1,1,NL,NS,0,0,0,1,
     &       mindis,maxdis,RPTS(1,NRPTS+1),NPT)
           IF (NPT.EQ.0) THEN
                CALL XVMESSAGE('***Ring is not in picture - 5',' ')
                RETURN1
           ENDIF
           CALL DRAWCURVE(RPTS(1,NRPTS+1),NPT,0)
           NRPTS = NRPTS + NPT
      ENDDO

      RLINE = RLINE2
      RSAMP = RSAMP2
      GOTO 50
C
  999 RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to manually trace a curve via the cursor.
C Outputs are the points on the acquired curve (APTS) stored as
C (line,sample) pairs and the number of points acquired (NPTS).
C
      SUBROUTINE TRACECRV(PIC,HPIC,NL,NS,APTS,NPTS)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 APTS(2,2000)
      real*8 rl8,rs8
      LOGICAL XVIPTST
c      BYTE BELL/Z07/

      CALL XVMESSAGE('Begin manual tracing of curve...',' ')
      CALL XVMESSAGE(
     .      'Move Cursor to a point on the curve and hit RETURN',' ')
      CALL XVMESSAGE(
     &  'Repeat to acquire more points or type ''EXIT when done',' ')
      CALL XVMESSAGE('You may delete a point by typing ''D',' ')
      CALL XVMESSAGE(
     . 'You may adjust the display by using ''H,CZOOM,STRETCH, or ASTR',
     .      ' ')
      NPTS = 0

c   10 CALL PRINT(IND,515,1,BELL)		!Make terminal go "Ding"
   10 print 1000,char(7)
 1000 format(1x,a1)
      CALL XVINTRACT('TRACECRV','TRACECRV')
      IF (XVIPTST('HELP')) CALL XVINTRACT('TRACECRV',' ')
      IF (XVIPTST('EXIT')) RETURN
      IF (XVIPTST('D')) THEN
          IF (NPTS.EQ.0) GOTO 10
          CALL FINDPT(APTS,NPTS,0,imin)
C               Delete it
          CALL DRAWDOT(APTS(1,IMIN),APTS(2,IMIN),0)
          NPTS = NPTS - 1
          DO I=IMIN,NPTS
              APTS(1,I) = APTS(1,I+1)
              APTS(2,I) = APTS(2,I+1)
          ENDDO                         
          GOTO 10
      ENDIF

      CALL DISPLAY(PIC,HPIC,NL,NS,ind)
      IF (IND.EQ.1) THEN
          IF (NPTS.GT.0) CALL DRAWCURVE(APTS,NPTS,1)
      ENDIF
      IF (IND.GT.0) GOTO 10 !VRH 7/29/89 Do not accept point if STRETCH, etc.
      CALL CURSOR(RL8,RS8)		!Read cursor position
      rline = rl8
      rsamp = rs8
      CALL DRAWDOT(RLINE,RSAMP,255)	!Draw a dot there
      NPTS = NPTS + 1
      APTS(1,NPTS) = RLINE
      APTS(2,NPTS) = RSAMP
      IF (NPTS.LT.2000) GOTO 10
      CALL XVMESSAGE('Maximum number of points acquired',' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Find closest point to cursor.
C Output: IMIN = index of closest point.
C
      SUBROUTINE FINDPT(APTS,NPTS,MODE,imin)
      REAL*4 APTS(2,NPTS)
c      BYTE BELL/Z07/
      real*8 rl8,rs8

      CALL CURSOR(RL8,RS8)		!Read cursor position
      rline = rl8
      rsamp = rs8
      IF (MODE.EQ.1) then
c	CALL PRINT(IND,515,1,BELL)
	print 1000,char(7)
1000	format(1x,a1)
      endif
      RMIN = (APTS(1,NPTS)-RLINE)**2 + (APTS(2,NPTS)-RSAMP)**2
      IMIN = NPTS
C		Find closest point to cursor position
      DO I=1,NPTS
          R = (APTS(1,I)-RLINE)**2 + (APTS(2,I)-RSAMP)**2
          IF (R.LT.RMIN) THEN
             RMIN = R
             IMIN = I
          ENDIF
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to find computed points closest to traced points.
C Inputs: CPTS=computed points	NCPTS=# of computed points
C         APTS=traced points	NAPTS=# of traced points
C Output: PTS=points on computed curve closest to APTS.
C
      SUBROUTINE SRCHINV(CPTS,NCPTS,APTS,NAPTS,PTS)
      REAL*4 CPTS(2,NCPTS),APTS(2,NAPTS),PTS(2,NAPTS)
      REAL*4 L0,L1,L2,L3

      MINJ = NCPTS/2
C
      DO 100 I=1,NAPTS
      L0 = APTS(1,I)		!Cursored point (L0,S0)
      S0 = APTS(2,I)
      MINDIS = (CPTS(1,MINJ)-L0)**2 + (CPTS(2,MINJ)-S0)**2
C		Find computed point closest to (L0,S0)
      DO 50 J=1,NCPTS
      L1 = CPTS(1,J)
      IF (L1.LE.0.0) GOTO 50
      DIS = (L1-L0)**2
      IF (DIS.GE.MINDIS) GOTO 50
      DIS = DIS + (CPTS(2,J)-S0)**2
      IF (DIS.GE.MINDIS) GOTO 50
      MINDIS = DIS
      MINJ = J
   50 CONTINUE

      L1 = CPTS(1,MINJ)
      S1 = CPTS(2,MINJ)
C           Find 2nd nearest point to (L0,S0)
      IF (MINJ.GT.1) THEN
          L2 = CPTS(1,MINJ-1)
          S2 = CPTS(2,MINJ-1)
      ELSE
          L2 = CPTS(1,NCPTS)
          S2 = CPTS(2,NCPTS)
      ENDIF

      IF (MINJ.LT.NCPTS) THEN
          L3 = CPTS(1,MINJ+1)
          S3 = CPTS(2,MINJ+1)
      ELSE
          L3 = CPTS(1,1)
          S3 = CPTS(2,1)
      ENDIF

      D12 = (L2-L0)**2 + (S2-S0)**2
      D13 = (L3-L0)**2 + (S3-S0)**2 
      IF (D13.LT.D12) THEN
           L2 = L3
           S2 = S3
      ENDIF
C            (L1,S1) and (L2,S2) are the first and second closest points on
C            the computed curve to (L0,S0).  Now interpolate between them to
C            find the closest point to (L0,S0).
      RNUM = (S1-S0)*(L2-L0) - (S2-S0)*(L1-L0)
      DENOM = (S2-S1)**2 + (L2-L1)**2
      PTS(1,I) = L0 + (S1-S2)*RNUM/DENOM
      PTS(2,I) = S0 + (L2-L1)*RNUM/DENOM
  100 CONTINUE
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given points on computed curve (CPTS), scan through the image for
C points on the actual curve (APTS) by searching radially + and - NSEARCH
C pixels from each point for a high contrast area which correlates well 
C with the edge function (F).
C
C Output: APTS
C
      SUBROUTINE RSEARCH(ICODE,PIC,HPIC,NL,NS,CPTS,apts,NPTS,
     &		INLW,NSW,NSEARCH,DELTAD,ITYPE,WIDTH,IMODE)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      REAL*4 CPTS(2,5000),APTS(2,5000)
      REAL*8 DELTAD

      INTEGER*4 d1,d2,d3,d4,S
      include 'fortport'
C     ....Edge function for outer edge of ring
      REAL*4 F1(21)/16.8,17.4,18.2,19.4,21.2,
     &              24.1,30.4,42.4,63.6,93.8,
     &              131.2,169.5,202.3,225.9,243.8,
     &              258.0,268.8,277.1,283.6,289.8,
     &              295.1/
      REAL*4 F(21),R(101),PS(151),PS2(151),CF(151)
      REAL*4 L0,S0,L1,S1,L2,S2,LJ,SJ
 1000 FORMAT(F20.1)
C
      ! check to avoid crash -- lwk
      if (npts.le.1) then
	call xvmessage(' insufficient points ...',' ')
	return
      endif

      RTHRESH = 0.35
      INC = 1.01/DELTAD			! This routine expects DELTAD=.5 OR 1.
      NLW = 2*((INLW*INC-1)/2) + 1      ! Scale correlation window to step size
      NSTEPS = 2*INC*NSEARCH + 1	! Total # of steps taken in search
      NLAREA = NSTEPS + NLW - 1         ! Total # of lines needed for search
      AREA = NLW*NSW			! Total # of pixels in correlation area
      DMIN = NSTEPS/2 + 1
      FSUM = 0.0
      FSUM2 = 0.0
C            Select type of edge function
      IF (ITYPE.EQ.3) THEN
         I0 = NLW/2 + 1		!Use Gaussian for thin rings...
         SIGMA = WIDTH/(4.0*DELTAD)
         DO I=1,NLW
            F(I) = 255.0*EXP(-((I-I0)/SIGMA)**2/2.0)
            FSUM = FSUM + F(I)
         ENDDO
      ELSE
         INC = 2.01*DELTAD	!Use tables for inner or outer edge...
         J = 11 - (NLW*INC-1)/2
         DO I=1,NLW
            F(I) = F1(J)
            FSUM = FSUM + F(I)
            J = J + INC
         ENDDO
      ENDIF
C            Normalize function so that mean=0
      FMEAN = FSUM/NLW
      DO I=1,NLW
         F(I) = F(I) - FMEAN
         FSUM2 = FSUM2 + F(I)**2
      ENDDO
      FSSUM2 = FSUM2*NSW

C            If inner edge, reverse the function
      IF (ITYPE.EQ.2) THEN
         NLWH = NLW/2
         DO I=1,NLWH
            TEMP = F(I)
            F(I) = F(NLW-I+1)
            F(NLW-I+1) = TEMP
         ENDDO
      ENDIF

      IF (IMODE.EQ.1) THEN
         DO L=1,NLAREA
            CF(L) = 0.0D0
         ENDDO
      ENDIF
C
C              Main search loop thru each point on the computed curve
      DO 100 N=1,NPTS
      L1 = CPTS(1,N)          	! Center of search is at (L1,S1)
      S1 = CPTS(2,N)
      APTS(1,N) = -99.0	! If no match is found, actual points
      APTS(2,N) = -99.0	! will be flagged as (-99.,-99.)
      IF (L1.LT.0.0) GOTO 100	! Skip if nominal point is flagged
C            Compute directional cosines for perpendicular at (L1,S1)
      N1 = N + 1
      IF (N1.GT.NPTS) N1=1
      L2 = CPTS(1,N1)
      S2 = CPTS(2,N1)
      D = SQRT((L2-L1)**2 + (S2-S1)**2)
      IF (D.GT.20.0) THEN
         N1 = N - 1
         IF (N1.EQ.0) N1=NPTS
         L2 = CPTS(1,N1)
         S2 = CPTS(2,N1)
         D = SQRT((L2-L1)**2 + (S2-S1)**2)
         IF (D.GT.20.0) GOTO 100
         COSL = -(S1-S2)/D
         COSS = -(L2-L1)/D
      ELSE
         COSL = (S1-S2)/D
         COSS = (L2-L1)/D
      ENDIF
C
C            Transform picture area to NLAREAxNSW correlation plane
      DO 20 L=1,NLAREA
      D0 = (L-NLAREA/2-1)*DELTAD
      L0 = D0*COSL + L1
      S0 = D0*COSS + S1
      PSUM = 0.
      PSUM2 = 0.
      J = -NSW/2
C
      DO 19 S=1,NSW
      Lj = L0 + J*COSS
      Sj = S0 - J*COSL
      IL = Lj		! Compute DN at (Lj,Sj) by interpolating
      IS = Sj		! over four nearest neighbors
      x = Lj - IL
      y = Sj - IS
      IF (ICODE.EQ.1) THEN
	d1 = byte2int(PIC(IS,IL))
	d2 = byte2int(PIC(IS+1,IL))
	d3 = byte2int(PIC(IS,IL+1))
	d4 = byte2int(PIC(IS+1,IL+1))
      ELSE
        d1 = HPIC(IS,IL)
        d2 = HPIC(IS+1,IL)
        d3 = HPIC(IS,IL+1)
        d4 = HPIC(IS+1,IL+1)
      ENDIF
      dn = d1 + (d2-d1)*x + (d3-d1)*y + (d1-d2-d3+d4)*x*y
      PSUM = PSUM + DN      
      PSUM2 = PSUM2 + DN**2
   19 J = J + 1
C
      IF (IMODE.EQ.1) CF(L)=CF(L)+PSUM
      PS(L) = PSUM
   20 PS2(L) = PSUM2
C
      RMAX = -99999.
C
C                       ! Correlate at each step normal to curve
      DO 90 LL=1,NSTEPS
      EL = LL + NLW - 1
C              Compute double sums of dn and dn**2 over correlation area
      IF (LL.EQ.1) THEN
          PSSUM = 0.
          PSSUM2 = 0.
          DO L=1,NLW
          PSSUM = PSSUM + PS(L)
          PSSUM2 = PSSUM2 + PS2(L)
          ENDDO
      ELSE
          PSSUM  = PSSUM  - PS(LL-1)  + PS(EL)
          PSSUM2 = PSSUM2 - PS2(LL-1) + PS2(EL)
      ENDIF
C
      PF = 0.
      DO 80 L=1,NLW
   80 PF = PF + PS(LL+L-1)*F(L)
C
      DENOM = (PSSUM2-PSSUM**2/AREA)*FSSUM2
      IF (DENOM.GT.0.1) THEN
         RCOR = PF**2/DENOM
         IF (RCOR.GT.RMAX) THEN
            RMAX = RCOR
            LMAX = LL
         ENDIF
      ELSE
         RCOR = -1. ! Tag this point as bad VRH 7/13/89 (used to use last)
      ENDIF
C
   90 R(LL) = RCOR
C            Reject match if correlation is too low
      IF (RMAX.LT.RTHRESH) GOTO 100
C            Reject match if it is on the end of the search
      IF (LMAX.EQ.1.OR.LMAX.EQ.NSTEPS) GOTO 100
C            Use parabola to interpolate around correlation maximum
      R1 = R(LMAX-1)
      R2 = R(LMAX+1)
      IF (R1.EQ.-1..OR.R2.EQ.-1.) THEN !VRH 7/13/89 test for bad point
         CALL XVMESSAGE('I would have used a bad point in RSEARCH',' ')
         GOTO 100
      END IF
      D0 = (0.5*(R1-R2)/(R1+R2-2.*RMAX)+LMAX-DMIN)*DELTAD
      APTS(1,N) = D0*COSL + L1
      APTS(2,N) = D0*COSS + S1
  100 CONTINUE
C
      IF (IMODE.NE.1) RETURN
      CALL XVMESSAGE('Computed edge function',' ')
      DO L=1,NLAREA
         WRITE(MSG,1000) CF(L)/(NPTS*NSW)
         CALL XVMESSAGE(MSG,20)
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create phasprof.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute and output constant phase profile....
C
      SUBROUTINE PHASPROF(PIC,HPIC,NL,NS)
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'fortport'        ! defines int2byte
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE   ! 1=outer edge, 2=inner edge, 3=thin edge
      CHARACTER*1 RINGS(15)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      common/navdv2/xdw,xdb
      integer xdw,xdb
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

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

      COMMON/CLIMIT/SRR,ERR,SLONR,ELONR
      REAL*4 SRR,ERR,SLONR,ELONR

      COMMON/CONST/PI,DTR,RTD

      INTEGER*4 d1,d2,d3,d4
      BYTE L1(4),L2(4),L3(4),L4(4)
c      EQUIVALENCE (L1,d1),(L2,d2),(L3,d3),(L4,d4)

      INTEGER*4 SAMP,S0,XX(2),YY(2)
      REAL X0,Y0,DX,DY

      CHARACTER*80 CMSG
      CHARACTER*132 MSG
      CHARACTER*60 OUTFILE
      LOGICAL PARMTST,XVIPTST
      LOGICAL XDIPOLYLINE,XST,XDCLOCATION,XDIFILL
      LOGICAL REDO,no_rtd

      doubleprecision csun0(3), csun(3), cnorm(3)
      doubleprecision ctrvec(3), widvec1(3), widvec2(3), corvec(3)
      doubleprecision ctrcrs(3)
      doubleprecision ctrmag,lenmag,lenang
      doubleprecision phaze, pha1, pha2, xincid, xemiss
      doubleprecision vdot, vnorm
      doubleprecision vec1(3), vec2(3)
      doubleprecision vecrot0(3), veccrs(3), vecrot1(3)
      doubleprecision vecsun(3)

      integer IILON, IIRAD, IIDN, IIINC, IIEMI, IIPHA, IILAST
      parameter (IILON = 1)
      parameter (IIRAD = IILON + 1)
      parameter (IIDN  = IIRAD + 1)
      parameter (IIINC = IIDN + 1)
      parameter (IIEMI = IIINC + 1)
      parameter (IIPHA = IIEMI  + 1)
      parameter (IILAST = IIPHA)
      REAL*4 WORK3(IILAST,5000)
      doubleprecision nominals( IILAST)

CBTCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      bmdn = int2byte(xdw)

CBTC
CBTC target-to-sc vector in camera coordinates
CBTC target-to-sun vector in camera coordinates
CBTC ***N.B. Assumes all scan-to-sun vectors are parallel i.e. distance to
CBTC         sun is large compared to area scanned
CBTC
CBTC ***N.B. NAV & SPICE rotation matrices are transposes of each other,
CBTC         so use mtxv with CM to convert EME50/J2k to camera
CBTC
      CALL GETRING(MRING)	! VRH change to work in Master plane 5/30/89
      call mtxv( cm, psun, csun0)
      call vscl( rsun, csun0, csun0)
      cnorm(1) = om(3,1)
      cnorm(2) = om(3,2)
      cnorm(3) = om(3,3)
CBTC
CBTC get image limits of display
      rllo = sl
      rslo = ss
      rlhi = (nlds - 1) / zoom + sl
      rshi = (nsds - 1) / zoom + ss

      REDO = .true.
      igotem = 0
      iindx = 0

      CALL XVMESSAGE('Scan along constant phase',' ')
      IF (MRING.EQ.1) THEN ! VRH added to indicate plane 5/30/89
          CALL XVMESSAGE('Reference plane is planet''s equator',' ')
      ELSE
          WRITE(CMSG,116) RINGS(MRING)
116       FORMAT(' Reference plane is that of ',A1,'-Ring')
          CALL XVMESSAGE(CMSG,' ')
      ENDIF

   10 CONTINUE

      CALL XVINTRACT('PHASPROF','Constant-Phase Profiling')
      IF (XVIPTST('EXIT')) RETURN
      IF (XVIPTST('HELP')) THEN  ! VRH add RL,LS option 5/30/89
         CALL XVMESSAGE('Enter ''Plot, OUtput=<filespec>,' 
     &     // ' ''CEnterpt, ''COrnerpt,',' ')
         CALL XVMESSAGE( '''PLot,''Dnplot, ''Longplot, ''RAdiplot, '
     &                // '''EMisplot, ''Inciplot, ''PHasplot', ' ')
         CALL XVMESSAGE('''C, ''H, CZoom=<IZ>, ''REFresh' 
     &     // ' STretch=(lo,hi), ''GErase, ', ' ')
         CALL XVMESSAGE('''EDit, GColor=<color>' 
     &     // ' (or ''<color>), or ''EXit',' ')
         GOTO 10
      ENDIF

CBTC check for graphics color parm GCOLOR
      call isetcolor

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.EQ.0) CALL RDISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 10

      IF (XVIPTST('EDIT')) THEN
        CALL EDITNAV(IMG,PIC,HPIC,NL,NS)
        XST = XDIFILL(IDEV,G,xdb)
        CALL GETRING(MRING)	! VRH change to work in Master plane 5/30/89
        call mtxv( cm, psun, csun0)
        call vscl( rsun, csun0, csun0)
        cnorm(1) = om(3,1)
        cnorm(2) = om(3,2)
        cnorm(3) = om(3,3)
        rllo = sl
        rslo = ss
        rlhi = (nlds - 1) / zoom + sl
        rshi = (nsds - 1) / zoom + ss
        igotem = 0
        REDO = .true.
        iindx = 0
        GOTO 10
      ENDIF	!'edit

      IF (XVIPTST('REFRESH')) THEN
         XST = XDIFILL(IDEV,G,xdb)
      ENDIF

CBTC cursor is centerpoint of scan, calculate phase and plot points

      IF ( XVIPTST('CENTERPT') 
     &  .or. ( XVIPTST('REFRESH') .and. (and(igotem,1) .eq. 1))
     &  ) THEN

        if ( XVIPTST('CENTERPT') ) then
          call cursor( rcline, rcsamp)
          call img2obj( rcline, rcsamp, ctrvec)

CBTC start ring-specific code
CBTC find rate of change along ring axis wrt camera vector, divide it into
CBTC   s/c Z distance below ring plane to get distance along camera 
CBTC   vector to ring plane
          xdist = vdot( ctrvec, cnorm)
          if ( xdist .ne. 0d0) xdist = -psc3(3) / xdist
CBTC end ring-specific code

          if ( xdist .le. 0d0) then
            call xvmessage( 'That point does not intersect target, '
     &                     // 'try again', ' ')
            igotem = 0
            goto 10
          endif

CBTC add planet-to-s/c & s/c-to-intersection vectors,
CBTC   result is planet-to-intersection vector

          call mtxv( om, psc3, vec1)
          call vscl( xdist, ctrvec, vec2)
          call vadd( vec1, vec2, vec1)

CBTC - convert to planet coordinates, use it to get rrad, rlon, rlat

          call mxv( om, vec1, vec2)
          rrad = vnorm( vec2)
          if ( rrad .gt. 0d0) then
            rlon = dmod(datan2(vec2(2),vec2(1))+rlora+2d0*pi,2d0*pi)
            if (MODEL .ne. 4 .and. vec2(3) .ne. 0d0) then
              rlat = geocen( dasin( vec2(3) / rrad), rlon)
            else
              rlat = 0d0
            endif
          else
            rlon= 0d0
            rlat = 0d0
          endif

CBTC - reverse, add to planet-sun vector, & vhat => intersection-to-sun unit vec

          call vminus( vec1, vec1)
          call vadd( csun0, vec1, csun)
CBTCEND NEW CODE 2
          call vhat( csun, csun)


CBTC check if ctrvec is at 0 or 180 phase

          call vcrss( csun, ctrvec, ctrcrs)
          call unorm( ctrcrs, ctrcrs, ctrmag)
          if ( ctrmag .eq. 0d0) then
            call xvmessage( '0/180 phase point chosen, try again', ' ')
            igotem = 0
            goto 10
          endif

CBTC N.B. phaze 180-<actual-phase>; same for pha2 & pha1 later

          phaze = dacos( vdot( ctrvec, csun))
          xincid = vdot( csun, cnorm)
          xemiss = - vdot( ctrvec, cnorm)

CBTC ensure incidence angle <= 90

          if ( xincid .lt. 0d0 .or. 
     &        (xincid .eq. 0d0 .and. xemiss .lt. 0d0)) then
            xincid = dacos( -xincid)
            xemiss = dacos( -xemiss)
          else
            xincid = dacos( xincid)
            xemiss = dacos( xemiss)
          endif
          WRITE(CMSG,'(a)') 
     &             'Nomimal Incidence, Emission & Phase angles (deg) '
     &          // 'at center point of scan are'
          CALL XVMESSAGE(CMSG,' ')
          WRITE (MSG(2:52),'(1pg17.9,2g17.9)') 
     &                     xincid*rtd, xemiss*rtd, 180d0 - phaze*rtd
          CALL XVMESSAGE(MSG(2:51),' ')
          REDO = .true.
          igotem = 1
        endif

CBTC GRAPHICS: 
CBTC calculate delrot, approx rotation of center vector around csun per 5 pixels
CBTC   - assume zscale >> sin(phaze) => 2*sin(delrot/2) ~ delrot
CBTC   - limit delrot to 10 degrees
CBTC rotate ctrvec around csun ~5 pixels at a time, plot points

        delrot = min( 10d0*dtr, 5d0 / (abs(sin(phaze))*zscale*zoom) )
        i = 1
        nrpts = 0
        rot = delrot
        dowhile (rot .lt. (180d0*dtr) .and. nrpts .le. 19998)
          call rotvecs2img( ctrvec, csun, rot, rllo, rslo, rlhi, rshi
     &                    , nrpts, rpts(1,nrpts+1))
          i = i + 1
          rot = i * delrot
        enddo
        call DRAWCURVE(rPTS,NrPTS,0)

CBTC drop through if this is a 'REFRESH i.e. not a 'CENTERPT

        if ( XVIPTST('CENTERPT') ) goto 10
      ENDIF	!'centerpt


CBTC corner point of scan wrt center point determines length & width of scan

      IF ( XVIPTST('CORNERPT') 
     &  .or. ( XVIPTST('REFRESH') .and. (and(igotem,2) .eq. 2))
     &  ) THEN

        if ( and(igotem,1) .ne. 1) then
          call xvmessage( 'Set the ''CENTERPT before the ''CORNERPT'
     &                  , ' ')
          goto 10
        endif

        if ( XVIPTST('CORNERPT') ) then

CBTC (1) determine length of scan from corner wrt scan center (ctrvec) 
CBTC   - convert cursor position to corner vector
CBTC   - test for 0/180 phase point
CBTC   - get half-length of scan in terms of rotation about sun vector by
CBTC     finding angle between center-cross-sun & length-cross-sun vectors
CBTC     - test for zero-length scan along the way

          call cursor( rxline, rxsamp)
          call img2obj( rxline, rxsamp, corvec)
          call vcrss( csun, corvec, vec1)
          call unorm( vec1, vec1, lenmag)
          if ( lenmag .eq. 0d0) then
            call xvmessage( '0/180 phase point chosen, try again', ' ')
            goto 10
          endif
          lenang = vdot( vec1, ctrcrs)
          if ( lenang .ge. 1d0) then
            call xvmessage( 'Zero-length scan chosen, try again', ' ')
            goto 10
          endif
          lenang = dacos(lenang)

CBTC (2) convert cursor position wrt scan center to width of scan
CBTC   - find phase angles of corner vector & its "mirror" about center vector
CBTC   - test for 0/180 phase point
CBTC   - rotate center vector around sun-cross-center vector to width vectors
          pha1 = dacos( vdot( csun, corvec))
          pha2 = 2 * phaze - pha1
          if ( min(pha1,pha2).le.0d0 .or. max(pha1,pha2).ge.PI) then
            call xvmessage( '0/180 phase point inside scan, try again'
     &                    , ' ')
            goto 10
          endif
          call rotvec( pha1-phaze, ctrvec, ctrcrs, widvec1)
          call rotvec( pha2-phaze, ctrvec, ctrcrs, widvec2)

          write( cmsg, '(a,1pg14.7, a, g14.7)') 'Phase range (deg) is '
     &         , 180d0 - max(pha1,pha2)*rtd, ' to '
     &         , 180d0 - min(pha1,pha2)*rtd
          call xvmessage( cmsg, ' ') 

          REDO = .true.
          igotem = 3
        endif

CBTC GRAPHICS:  put lines at end of scan to show length

        nrpts = 0
        call rotvecs2img( widvec1, csun, lenang
     &                    , -1d30, -1d30, 1d30, 1d30
     &                    , nrpts, rpts(1,nrpts+1))
        call rotvecs2img( widvec2, csun, lenang
     &                    , -1d30, -1d30, 1d30, 1d30
     &                    , nrpts, rpts(1,nrpts+1))
        YY(1) = ZOOM*(rpts(1,1)-SL) + 1.5
        XX(1) = ZOOM*(rpts(2,1)-SS) + 1.5
        YY(2) = ZOOM*(rpts(1,3)-SL) + 1.5
        XX(2) = ZOOM*(rpts(2,3)-SS) + 1.5
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
        YY(1) = ZOOM*(rpts(1,2)-SL) + 1.5
        XX(1) = ZOOM*(rpts(2,2)-SS) + 1.5
        YY(2) = ZOOM*(rpts(1,4)-SL) + 1.5
        XX(2) = ZOOM*(rpts(2,4)-SS) + 1.5
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

CBTC GRAPHICS:  put dot every 10 DISPLAY pixels to show width of scan

        delrot = min( 10d0*dtr
     &       , 1d1/(min(abs(sin(pha1)),abs(sin(pha2)))*zscale*zoom))

        i = 1
        nrpts = 0
        rot = delrot
        dowhile (rot .lt. (180d0*dtr) .and. nrpts .le. 19996)
          call rotvecs2img( widvec1, csun, rot
     &                    , rllo, rslo, rlhi, rshi
     &                    , nrpts, rpts(1,nrpts+1))
          call rotvecs2img( widvec2, csun, rot
     &                    , rllo, rslo, rlhi, rshi
     &                    , nrpts, rpts(1,nrpts+1))
          i = i + 1
          rot = i * delrot
        enddo
        call DRAWCURVE(rPTS,NrPTS,0)

CBTC drop through if this is a 'REFRESH i.e. not a 'CENTERPT

        IF ( XVIPTST('CORNERPT') ) GOTO 10

      endif	!'cornerpt

CBTC do the scan
CBTC - no scan unless we drop through 'edit, 'centerpt & 'cornerpt above

      if ( igotem .eq. 3 .and. redo) then
        call xvmessage( 'Scanning ...', ' ')
        delrot2 = 1d0 / zscale
        delrot = delrot2 / sin(phaze)
        n3 = min( 5000, max( 3, int(1 + lenang * 2 / delrot)))
        phadel = abs(pha1-pha2)
        n4 = max( 3, int( 1 + phadel / delrot2))
        if ( mod(n4,2) .eq. 0) n4 = n4 + 1
        n3a = 0
        do j=1,n3
          rot0 = ((j-1) * 2 * lenang) / (n3-1) - lenang
          call rotvec( rot0, ctrvec, csun, vecrot0)
          call vcrss( csun, vecrot0, veccrs)
          call vhat( veccrs, veccrs)
          sumlon = 0d0
          sumrad = 0d0
          sumdn  = 0d0
          sumpha = 0d0
          suminc = 0d0
          sumemi = 0d0
          n4a = 0
          do i=1,n4
            rot1 = ((i-1) * phadel) / (n4-1) - phadel / 2
            call rotvec( rot1, vecrot0, veccrs, vecrot1)
            call obj2img( vecrot1, rline, rsamp)

CBTC start ring-specific code
            xdist = vdot( vecrot1, cnorm)
            if ( xdist .ne. 0d0) xdist = -psc3(3) / xdist
CBTC end ring-specific code

            if ( xdist .le. 0d0) goto 00058

CBTC add planet-to-s/c & s/c-to-intersection vectors,
CBTC   result is planet-to-intersection vector

            call mtxv( om, psc3, vec1)
            call vscl( xdist, vecrot1, vec2)
            call vadd( vec1, vec2, vec1)

CBTC - convert to planet coordinates, use it to get rrad, rlon, rlat

            call mxv( om, vec1, vec2)
            rrad = vnorm( vec2)
            if ( rrad .gt. 0d0) then
              rlon = dmod(datan2(vec2(2),vec2(1))+rlora+2d0*pi,2d0*pi)
              if (MODEL .ne. 4 .and. vec2(3) .ne. 0d0) then
                rlat = geocen( dasin( vec2(3) / rrad), rlon)
              else
                rlat = 0d0
              endif
            else
              rlon= 0d0
              rlat = 0d0
            endif

CBTC - reverse, add to planet-sun vector, & vhat => intersection-to-sun unit vec

            call vminus( vec1, vec1)
            call vadd( csun0, vec1, vecsun)

CBTCEND NEW CODE 3

            call vhat( vecsun, vecsun)
            vecpha = dacos( vdot( vecrot1, vecsun))
            vecinc = vdot( vecsun, cnorm)
            vecemi = - vdot( vecrot1, cnorm)
            if ( vecinc .lt. 0d0 .or. 
     &          (vecinc .eq. 0d0 .and. vecemi .lt. 0d0)) then
              vecinc = dacos( -vecinc)
              vecemi = dacos( -vecemi)
            else
              vecinc = dacos( vecinc)
              vecemi = dacos( vecemi)
            endif
C             ....Compute DN value at (rline,rsamp) via bilinear interpolation
            IL = RLINE
            IS = RSAMP
            IF (IL.LT.1.OR.IL.GE.NL) GOTO 58
            IF (IS.LT.1.OR.IS.GE.NS) GOTO 58
            x = RLINE - IL
            y = RSAMP - IS
            IF (ICODE.EQ.1) THEN
              L1(1) = PIC(IS,IL)
              L2(1) = PIC(IS+1,IL)
              L3(1) = PIC(IS,IL+1)
              L4(1) = PIC(IS+1,IL+1)
              d1 = byte2int(l1(1))
              d2 = byte2int(l2(1))
              d3 = byte2int(l3(1))
              d4 = byte2int(l4(1))
            ELSE
              d1 = HPIC(IS,IL)
              d2 = HPIC(IS+1,IL)
              d3 = HPIC(IS,IL+1)
              d4 = HPIC(IS+1,IL+1)
            ENDIF
            dn = d1 + (d2-d1)*x + (d3-d1)*y + (d1-d2-d3+d4)*x*y
            sumrad = sumrad + rrad
            sumlon = sumlon + rlon
            sumdn  = sumdn  + dn
            suminc = suminc + (vecinc-xincid)
            sumemi = sumemi + (vecemi-xemiss)
CBTC reverse phase delta because phaze & vecpha are calculated using reverse 
CBTC emission vector
            sumpha = sumpha + (phaze-vecpha)
            n4a = n4a + 1
00058         continue
          enddo	!i=1,n4
          if ( n4a .gt. 0) then
            work3(IILON,j) = rtd * sumlon / n4a
            work3(IIRAD,j) = sumrad / n4a
            work3(IIDN ,j) = sumdn  / n4a
            work3(IIINC,j) = rtd * suminc / n4a
            work3(IIEMI,j) = rtd * sumemi / n4a
            work3(IIPHA,j) = rtd * sumpha / n4a
            n3a = n3a + 1
          else
            work3(IILON,j) = -999e10
          endif
        enddo	!j=1,n3
        if ( n3a .lt. n3) then
          msg(1:80) = ' '
CBTC             12345678901234567890123456789012345678901234567890
          msg = '*** Warning:  x234 out of x234 scanpoint'
     &       // 's are outside image/geometry limits'
          write(msg(15:18),'(i4)') n3-n3a
          write(msg(27:30),'(i4)') n3
          if ( (n3-n3a) .eq. 1) msg(43:45) = ' is'
          call xvmessage( msg(1:76), ' ')
        endif
        redo = .false.

      endif	!igotem.eq.3 .and. redo

CBTC determine PLOT type, default to DNplot

      if ( .not.redo) then
        if ( xviptst('plot') .or. xviptst('dnplot')) then
          iindx = iidn
        elseif ( xviptst('longplot')) then
          iindx = iilon
        elseif ( xviptst('radiplot')) then
          iindx = iirad
        elseif ( xviptst('emisplot')) then
          iindx = iiemi
        elseif ( xviptst('inciplot')) then
          iindx = iiinc
        elseif ( xviptst('phasplot')) then
          iindx = iipha
        endif
      endif

CBTC do the plot

      if ( (iindx .gt. 0 .and. .not.redo) .or. 
     &     (iindx .lt. 0 .and. xviptst('refresh')) ) then
        iindx = -abs(iindx)
        do j=1,IILAST
          nominals(j) = 0d0
        enddo
        nominals(IIINC) = xincid * rtd
        nominals(IIEMI) = xemiss * rtd
        nominals(IIPHA) = 180d0 - phaze * rtd
        nrpts = 0
        do j=1,n3
          if ( work3(IILON,j) .ne. -999e10) then
            nrpts = nrpts + 1
            rpts(1,nrpts) = j
            rpts(2,nrpts) = work3(-iindx,j) + nominals( -iindx)
          endif
        enddo
        YY(1) = ZOOM*(RcLINE-SL) + 1.5
        XX(1) = ZOOM*(RcSAMP-SS) + 1.5
        IF (nrpts.LE.0) THEN
          CALL XVMESSAGE('*** Error, nothing to plot',' ')
          GOTO 10
        ELSE IF (nrpts.GT.1000) THEN
          S0 = 19
        ELSE
          S0 = 19
          IF (XX(1).LE.256) S0=271
        ENDIF
        L0 = 237
        IF (YY(1).LE.256) L0=493
        no_rtd = .true.
        CALL PRDISPLAY(rpts,nrpts,no_rtd,L0,S0,X0,Y0,DX,DY)
      endif

      IF ( XVIPTST('CP') .and. n3a.gt.0 
     &     .and. .not.redo .and. iindx.lt.0) THEN
   12    XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
CBTC            123456789012345678901234567890123456789012
         msg = 'PHASE EMISS INCID DN    RADIUSLONGIT'
         WRITE(CMSG,115) LINE,SAMP
     &                 , msg(37+iindx*6:42+iindx*6)
     &                 , X0+DX*FLOAT(SAMP-S0), Y0+DY*FLOAT(LINE-L0)
  115    FORMAT('(L,S)=(',I4,',',I4,') (PIXEL,',a5,')=(',
     &         1pg15.7,',',g15.7,')')
         CALL XVMESSAGE(CMSG,' ')
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 12
         GOTO 10
      ENDIF

CBTC 'OUTPUT to file

      IF ( PARMTST('OUTPUT',OUTFILE,I) .and. igotem.eq.3 .and.
     &     .not.redo) THEN

CBTC
CBTC Open file, write out file header info

        OPEN(1,FILE=OUTFILE,STATUS='NEW',ERR=994)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' 10 x234 4 - first & last lines of data,'
     &     // ' line with nominal inc, emi, phase'
        WRITE (MSG(5:8),'(I4)') n3a + 9
        WRITE(1,113) (MSG(I:I),I=1,74)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Constant-phase plot of frame '
        WRITE (MSG(31:40),'(I10)') FRAME_ID
        CALL XVMESSAGE(MSG(2:40),' ')
        WRITE(1,113) (MSG(I:I),I=1,40)
  113   FORMAT(80A1)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Nominal Incidence, Emission, & Phase an'
     &     // 'gles (degrees, at center point):'
        CALL XVMESSAGE(MSG(2:72),' ')
        WRITE(1,113) (MSG(I:I),I=1,72)
        MSG(1:80) = ' '
CBTC
        WRITE (MSG(2:52),'(1pg17.9,2g17.9)') 
     &                     xincid*rtd, xemiss*rtd, 180d0 - phaze*rtd
        CALL XVMESSAGE(MSG(2:51),' ')
        WRITE(1,113) (MSG(I:I),I=1,52)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Scan''s phase angle width is               degrees'
        WRITE (MSG(30:44),'(1pg15.7)') abs(pha1-pha2)*rtd
        CALL XVMESSAGE(MSG(2:44),' ')
        WRITE(1,113) (MSG(I:I),I=1,44)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' Photometric angles in table below are d'
     &     // 'eltas from nominal'
        CALL XVMESSAGE(MSG(2:59),' ')
        WRITE(1,113) (MSG(I:I),I=1,59)
        MSG(1:80) = ' '
CBTC               123456789012345678901234567890123456789012345678901234567890
        IF (MRING.EQ.1) THEN
            MSG = ' Reference plane is planet''s equator'
        ELSE
            WRITE(CMSG,116) RINGS(MRING)
            MSG(1:34) = CMSG(1:34)
        ENDIF
CBTC        CALL XVMESSAGE(MSG(2:36),' ')
        WRITE(1,113) (MSG(I:I),I=1,36)
        MSG(1:80) = ' '
CBTC           123456789012345678901234567890123456789012345678901234567890
        MSG = ' INDX    LONG     RADIUS       DN       '
     &     // '   dINCID         dEMISS          dPHASE'
        WRITE(1,113) (MSG(I:I),I=1,80)
        MSG = ' ----    ----     ------       --       '
     &     // '   ------         ------          ------'
        WRITE(1,113) (MSG(I:I),I=1,80)
        MSG(1:80) = ' '
        DO J=1,N3
          if ( work3(IILON,j) .ne. -999e10) then
            WRITE (MSG(2:5),'(i4)') j
            WRITE (MSG(7:13),'(F7.2)') WORK3(IILON,J)
            WRITE (MSG(15:24),'(I10)') int(WORK3(IIRAD,J))
            WRITE (MSG(26:33),'(f8.2)') WORK3(IIDN,J)
            WRITE (MSG(35:49),'(1pg15.7)') WORK3(IIINC,J)
            WRITE (MSG(51:64),'(1pg14.7)') WORK3(IIEMI,J)
            WRITE (MSG(66:80),'(1pg15.7)') WORK3(IIPHA,J)
            WRITE(1,113) (MSG(I:I),I=1,80)
          endif
        ENDDO
        CLOSE(1,ERR=995)
        CALL XVMESSAGE('Written to disc',' ')
        GOTO 10
  994   CALL XVMESSAGE('***Error opening output file',' ')
        GOTO 10
  995   CALL XVMESSAGE('***Error closing output file',' ')
        GOTO 10
      ENDIF	!'output

      GOTO 10
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute and output radial or longitudinal profile....
C
      SUBROUTINE PROFILE(PIC,HPIC,NL,NS)
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'fortport'        ! defines int2byte
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE   ! 1=outer edge, 2=inner edge, 3=thin edge
      CHARACTER*1 RINGS(15)

      REAL*4 WORK3(2,5000),rl4,rs4,mindis,maxdis

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      common/navdv2/xdw,xdb
      integer xdw,xdb
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

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

      COMMON/CLIMIT/SRR,ERR,SLONR,ELONR
      REAL*4 SRR,ERR,SLONR,ELONR,par(2),WLONR

      COMMON/CONST/PI,DTR,RTD

      INTEGER*4 d1,d2,d3,d4
      BYTE L1(4),L2(4),L3(4),L4(4)
c      EQUIVALENCE (L1,d1),(L2,d2),(L3,d3),(L4,d4)

      INTEGER*4 SAMP,S0,XX(2),YY(2),SDIST
      REAL X0,Y0,DX,DY

      CHARACTER*1 PTYPE
      CHARACTER*80 CMSG
      CHARACTER*132 MSG
      CHARACTER*60 OUTFILE
      LOGICAL PARMTST,rPARMTST,XVIPTST
      LOGICAL XDIPOLYLINE,XST,XDCLOCATION,XDIFILL
      LOGICAL RPLOT,REDO,DOPHA
      DATA RPLOT/.TRUE./
      LOGICAL DPLOT,SPLOT

      bmdn = int2byte(xdw)

      CALL GETRING(MRING)	! VRH change to work in Master plane 5/30/89
      REDO = .TRUE.

   10 IF (RPLOT) THEN
         IF(DPLOT) THEN
           CALL XVMESSAGE('Diagonal Radial scan',' ')
           CALL XVMESSAGE(
     &     '*** Pay attention to width at both ends ***',' ')
         ELSE
           CALL XVMESSAGE('Radial scan',' ')
         ENDIF
      ELSE
         IF(SPLOT) THEN
           CALL XVMESSAGE('Longitudinal smear scan',' ')
         ELSE
           CALL XVMESSAGE('Longitudinal scan',' ')
         ENDIF
      ENDIF
      IF (MRING.EQ.1) THEN ! VRH added to indicate plane 5/30/89
          CALL XVMESSAGE('Reference plane is planet''s equator',' ')
      ELSE
          WRITE(CMSG,116) RINGS(MRING)
116       FORMAT(' Reference plane is that of ',A1,'-Ring')
          CALL XVMESSAGE(CMSG,' ')
      ENDIF
      IF(.NOT.SPLOT) THEN
         IF (SR.NE.0..OR.ER.NE.0.) THEN  ! VRH bug fix 5/30/89
            WRITE(CMSG,111) SR,ER
111         FORMAT(' Radius limits: ',F9.1,',',F9.1,' (km)')
            CALL XVMESSAGE(CMSG,' ')
         ELSE
            CALL XVMESSAGE('No radius limits',' ')
         ENDIF
      ELSE
         IF(RCENT.NE.0) THEN
           WRITE(CMSG,119) SMANG*RTD
119        FORMAT('Smear direction: ',F7.2,' (degrees)')
           CALL XVMESSAGE(CMSG,' ')
           WRITE(CMSG,120) SDIST
120        FORMAT('Smear length: ',I4,' (pixels)')
           CALL XVMESSAGE(CMSG,' ')
           WRITE(CMSG,121) RCENT
121        FORMAT('Center Radius of scan: ',F9.1,' (km)')
           CALL XVMESSAGE(CMSG,' ')
         ELSE
           CALL XVMESSAGE('Smear not yet defined',' ')
         ENDIF
      ENDIF
      IF (SLON.NE.0..OR.ELON.NE.0.) THEN  ! VRH bug fix 5/30/89
         IF(DPLOT) THEN
           WRITE(CMSG,117) SLON*RTD,ELON*RTD
117        FORMAT('Center longitudes: ',F7.2,',',F7.2,' (degrees)')
         ELSE
           WRITE(CMSG,114) SLON*RTD,ELON*RTD
114        FORMAT('Longitude limits: ',F7.2,',',F7.2,' (degrees)')
         ENDIF
         CALL XVMESSAGE(CMSG,' ')
      ELSE
         CALL XVMESSAGE('No longitude limits',' ')
      ENDIF
      IF(DPLOT) THEN
         IF (WLON.NE.0.D0) THEN
           WRITE(CMSG,118) WLON*RTD
118        FORMAT('Longitude width: ',F7.2,' (degrees)')
           CALL XVMESSAGE(CMSG,' ')
         ELSE
           CALL XVMESSAGE('No longitude width',' ')
         ENDIF
      ENDIF

      CALL XVINTRACT('PROFILE','PROFILE')
      IF (XVIPTST('EXIT')) RETURN
      IF (XVIPTST('HELP')) THEN  ! VRH add RL,LS option 5/30/89
         CALL XVMESSAGE('Enter TYPE, RLIMITS, LLIMITS,' //
     .        ' ''PLOT, OUTPUT, ''PHASE, ''CP',' ')
         CALL XVMESSAGE('RL=(rad,long), LS=<line,samp>, ''C, ''H, '
     &        // 'CZOOM=<iz>, STRETCH=(l,h), ''GERASE, or ''EXIT',' ')
         GOTO 10
      ENDIF

CBTC check for graphics color parm GCOLOR
      call isetcolor

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.EQ.0) CALL RDISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 10

      IF (PARMTST('TYPE',PTYPE,I)) THEN
        REDO = .TRUE.
        RPLOT = .TRUE.
        DPLOT = .FALSE.
        SPLOT = .FALSE.
        WLON = 0.D0
        IF (PTYPE.EQ.'L'.OR.PTYPE.EQ.'l') RPLOT = .FALSE.
        IF (PTYPE.EQ.'D'.OR.PTYPE.EQ.'d') DPLOT = .TRUE.
        IF (PTYPE.EQ.'S'.OR.PTYPE.EQ.'s') THEN
            RPLOT = .FALSE.
            SPLOT = .TRUE.
   20       CALL XVMESSAGE('Specify smear direction:',' ')
            CALL XVMESSAGE('Move Cursor on one side of profile',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT to skip')
            IF (XVIPTST('EXIT')) GOTO 30
            XST = XDIFILL(IDEV,G,XDB)
            CALL CURSOR(RLINE,RSAMP)
            YY(1) = ZOOM*(RLINE-SL) + 1.5
            XX(1) = ZOOM*(RSAMP-SS) + 1.5
            XST = XDIPOLYLINE(IDEV,G,bmdn,1,XX,YY)
C
   21       CALL XVMESSAGE('Move Cursor to other side of profile',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT if done')
            IF (XVIPTST('EXIT')) GOTO 30
            CALL CURSOR(RLINE2,RSAMP2)
            IF((RLINE2-RLINE).EQ.0..AND.(RSAMP2-RSAMP).EQ.0.) THEN
              CALL XVMESSAGE(
     &           'Start and end points are same, try again',' ')
              GOTO 21
            ELSE
              SMANG = ATAN2((RLINE2 - RLINE),(RSAMP2 - RSAMP))
            END IF
            XST = XDIFILL(IDEV,G,xdb)
            YY(2) = ZOOM*(RLINE2-SL) + 1.5
            XX(2) = ZOOM*(RSAMP2-SS) + 1.5
            XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
            WRITE(CMSG,119) SMANG*RTD
            CALL XVMESSAGE(CMSG,' ')
            GOTO 21

   30       CALL XVMESSAGE('Specify smear length:',' ')
            CALL XVMESSAGE('Move Cursor on one endpoint of smear',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT to skip')
            IF (XVIPTST('EXIT')) GOTO 32
            CALL CURSOR(RLINE,RSAMP)
C
   31       CALL XVMESSAGE('Move Cursor to other endpoint of smear',' ')
            CALL XVINTRACT('READY',
     &        ' Hit Return when ready or type ''EXIT if done')
            IF (XVIPTST('EXIT')) GOTO 32
            CALL CURSOR(RLINE2,RSAMP2)
            SDIST = NINT(DSQRT((RLINE2-RLINE)**2 + (RSAMP2-RSAMP)**2))
            CENTLINE = (RLINE2 + RLINE)/2.D0
            CENTSAMP = (RSAMP2 + RSAMP)/2.D0
            CALL LATLON(IND,CENTLINE,CENTSAMP,RCENT,RLON)
            WRITE(CMSG,120) SDIST
            CALL XVMESSAGE(CMSG,' ')
            WRITE(CMSG,121) RCENT
            CALL XVMESSAGE(CMSG,' ')
            GOTO 31
   32       CONTINUE
        ENDIF
      ENDIF

      IF (rPARMTST('LLIMITS',par,I)) THEN
	slonr = par(1)
	elonr = par(2)
         IF (SLONR.GT.ELONR) THEN
            CALL XVMESSAGE('*** Error, Longitude limits in wrong order',
     .                     ' ')
            SLON = 0.
            ELON = 0.
         ELSE
            SLON = DBLE(SLONR)*DTR
            ELON = DBLE(ELONR)*DTR
         ENDIF
         REDO = .TRUE.
      ENDIF

      IF (rPARMTST('RLIMITS',par,I)) THEN
	srr = par(1)
	err = par(2)
         IF (SRR.GT.ERR) THEN
           CALL XVMESSAGE('*** Error, Radius limits in wrong order',' ')
            SR = 0.
            ER = 0.
         ELSE
            SR = DBLE(SRR)
            ER = DBLE(ERR)
         ENDIF
         REDO = .TRUE.
      ENDIF

      IF (rPARMTST('LWIDTH',wlonr,I)) THEN
         WLON = DBLE(WLONR)*DTR
         REDO = .TRUE.
      ENDIF

      IF (REDO) THEN
         IF (.NOT.SPLOT.AND.SR.EQ.0.D0.AND.ER.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, no radius limits',' ')
            GOTO 10
         ENDIF
         IF (SLON.EQ.0.D0.AND.ELON.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, no longitude limits',' ')
            GOTO 10
         ENDIF
         IF(DPLOT.AND.WLON.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, no longitude width',' ')
            GOTO 10
         ENDIF
         IF(SPLOT.AND.RCENT.EQ.0.D0) THEN
            CALL XVMESSAGE('*** Error, smear not defined',' ')
            GOTO 10
         ENDIF
         XST = XDIFILL(IDEV,G,xdb)
         AVGLON = (SLON+ELON)/2.D0
         IF(SPLOT) THEN
            AVGR = RCENT
            CALL LINSAM(IND,RCENT,SLON,SL1,SS1)
            CALL LINSAM(IND,RCENT,ELON,SL2,SS2)
         ELSE
            AVGR = (SR+ER)/2.D0
            CALL LINSAM(IND,SR,SLON,SL1,SS1)
            CALL LINSAM(IND,SR,ELON,SL2,SS2)
            CALL LINSAM(IND,ER,SLON,SL3,SS3)
            CALL LINSAM(IND,ER,ELON,SL4,SS4)
         ENDIF
      ENDIF

      IF(XVIPTST('PLOT').OR.REDO) THEN  ! Draw sector
	mindis = 0.6
	maxdis = 1.2
        IF(DPLOT) THEN

         YY(1) = ZOOM*(SL1-SL) + 1.5
         XX(1) = ZOOM*(SS1-SS) + 1.5
         YY(2) = ZOOM*(SL4-SL) + 1.5
         XX(2) = ZOOM*(SS4-SS) + 1.5
         XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

C        ....Establish longitudinal scale
         CALL RINGPT2(SR,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &        OAL,OAS,ZSCALE,1,1,NL,NS,  !VRH 8/12/89 fix start from (1,1)
     &        mindis,maxdis,SLON-0.5*WLON,SLON+0.5*WLON,RPTS,NPTS1)
         IF (NPTS1.GT.0) CALL DRAWCURVE(RPTS,NPTS1,0)

         CALL RINGPT2(ER,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &        OAL,OAS,ZSCALE,1,1,NL,NS,  !VRH 8/12/89 fix
     &        mindis,maxdis,ELON-0.5*WLON,ELON+0.5*WLON,RPTS,NPTS2)
         IF (NPTS2.GT.0) CALL DRAWCURVE(RPTS,NPTS2,0)

        ELSE
         IF(SPLOT) THEN
           YY(1) = ZOOM*(SL1-SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(1) = ZOOM*(SS1-COS(SMANG)*SDIST/2.-SS) + 1.5
           YY(2) = ZOOM*(SL1+SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(2) = ZOOM*(SS1+COS(SMANG)*SDIST/2.-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

           YY(1) = ZOOM*(SL2-SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(1) = ZOOM*(SS2-COS(SMANG)*SDIST/2.-SS) + 1.5
           YY(2) = ZOOM*(SL2+SIN(SMANG)*SDIST/2.-SL) + 1.5
           XX(2) = ZOOM*(SS2+COS(SMANG)*SDIST/2.-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

C          ....Establish longitudinal scale
           CALL RINGPT2(RCENT,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &          OAL,OAS,ZSCALE,1,1,NL,NS,  !VRH 8/12/89 fix start from (1,1)
     &          mindis,maxdis,SLON,ELON,RPTS,NPTS1)
           IF (NPTS1.GT.0) CALL DRAWCURVE(RPTS,NPTS1,0)
           NPTS2 = 0
         ELSE
           YY(1) = ZOOM*(SL1-SL) + 1.5
           XX(1) = ZOOM*(SS1-SS) + 1.5
           YY(2) = ZOOM*(SL3-SL) + 1.5
           XX(2) = ZOOM*(SS3-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

           YY(1) = ZOOM*(SL2-SL) + 1.5
           XX(1) = ZOOM*(SS2-SS) + 1.5
           YY(2) = ZOOM*(SL4-SL) + 1.5
           XX(2) = ZOOM*(SS4-SS) + 1.5
           XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)

C          ....Establish longitudinal scale
           CALL RINGPT2(SR,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &          OAL,OAS,ZSCALE,1,1,NL,NS,  
     &          mindis,maxdis,SLON,ELON,RPTS,NPTS1)
           IF (NPTS1.GT.0) CALL DRAWCURVE(RPTS,NPTS1,0)

           CALL RINGPT2(ER,0.0D0,OM,PSC3,PSUN3,SCLON,RLORA,
     &          OAL,OAS,ZSCALE,1,1,NL,NS,  
     &          mindis,maxdis,SLON,ELON,RPTS,NPTS2)
           IF (NPTS2.GT.0) CALL DRAWCURVE(RPTS,NPTS2,0)
        ENDIF
       ENDIF
      ENDIF

      IF (REDO) THEN
         NPTS = MAX0(NPTS1,NPTS2)
         IF (NPTS.EQ.0) THEN
            CALL XVMESSAGE('***Sector lies outside image',' ')
            RETURN
         ENDIF
         DLON = (ELON-SLON)/NPTS
         SCALE = DABS(DLON)
         WRITE(CMSG,112) SCALE*RTD
  112    FORMAT('Average longitudinal resolution is ',F10.4,
     &     ' degrees/pixel')
         CALL XVMESSAGE(CMSG,' ')

        IF(.NOT.SPLOT) THEN
C         ....Establish maximum radial resolution (km/pixel) in sector
C         ....Go across scan to find maximum pixel width    
	  RLON = SLON
          IF(DPLOT) RLON = RLON - 0.5*WLON
	  D1 = 0
          NPTS1 = 0
40        CALL LINSAM(IND,SR,RLON,SL5,SS5)
          IF(DPLOT) THEN
            CALL LINSAM(IND,ER,RLON+(ELON-SLON),SL6,SS6)
          ELSE
            CALL LINSAM(IND,ER,RLON,SL6,SS6)
          ENDIF
	  NPTS1 = NPTS1 + 1 
          IF (NPTS1.GT.5000) THEN
            CALL XVMESSAGE('***Maximum number of ring points exceeded',
     &                   ' ')
            GOTO 45
          ENDIF
          RPTS(1,NPTS1) = DSQRT((SL6-SL5)**2 + (SS6-SS5)**2)
          D1 = MAX(D1,INT(RPTS(1,NPTS1)+0.99))
	  RLON = RLON + DLON
          IF(DPLOT.AND.(RLON.LT.SLON+0.5*WLON)) GOTO 40
          IF(.NOT.DPLOT.AND.(RLON.LT.ELON)) GOTO 40

45        DR = (ER-SR)/D1
          WRITE(CMSG,110) DABS(DR)
  110     FORMAT('Maximum radial resolution is ',F10.2,' km/pixel')
          CALL XVMESSAGE(CMSG,' ')
        ENDIF

        IF (DPLOT) THEN ! RPTS = R,LON along diagonal line
          DIST = DSQRT((SL4-SL1)**2 + (SS4-SS1)**2)
          RPTS(1,1) = SR
          RPTS(2,1) = SLON
          DO I=2,INT(DIST)
            RLINE = (SL4-SL1)*FLOAT(I-1)/DIST + SL1
            RSAMP = (SS4-SS1)*FLOAT(I-1)/DIST + SS1
            CALL LATLON(IND,RLINE,RSAMP,R,RLON)
            RPTS(1,I) = R
            RPTS(2,I) = RLON
          END DO
          RPTS(1,INT(DIST)+1) = ER
          RPTS(2,INT(DIST)+1) = ELON
        ENDIF

        J = 0
        I = 0
        IF (RPLOT) THEN
           R = SR
        ELSE
           RLON = SLON
        ENDIF

   50   I = I + 1
        IF (.NOT.RPLOT.AND.RPTS(1,I).LT.1.0) THEN
           RLON = RLON + DLON
           IF (RLON.LE.ELON) GOTO 50
           GOTO 59
        END IF
	IF (RPLOT) THEN
           RLON = SLON
           IF (DPLOT) THEN ! Calculate Center longitude from RPTS
              DO K=1,INT(DIST)
                IF(R.GE.RPTS(1,K).AND.R.LE.RPTS(1,K+1)) GOTO 51
              END DO
   51         CENTL = (RPTS(2,K+1)-RPTS(2,K))*(R-RPTS(1,K))
              CENTL = CENTL/(RPTS(1,K+1)-RPTS(1,K)) + RPTS(2,K)
              RLON = CENTL - 0.5*WLON
           ENDIF
        ELSE
           R = SR
           IF(SPLOT) R = RCENT
        ENDIF
        SUMDN = 0.D0	!Sum of all dn's at radius R or longitude
        d1 = 0
        d2 = 0
        d3 = 0
        d4 = 0
        NPTS = 0		!Number of pixels summed
C       ....Convert from (R,RLON) to (rline,rsamp) coordinates 
   55   CALL RINV(IND,R,RLON,RLINE,RSAMP,OM,PSC3,RLORA,OAL,OAS,ZSCALE)
        IF (ITYPE.EQ.7) then
	  CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(RLINE),sngl(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	  rline=rl4
	  rsamp=rs4
	endif

   56   IF (SPLOT) THEN
          IF (NPTS.EQ.0) THEN
            RLINE = RLINE - SIN(SMANG)*SDIST/2.
            RSAMP = RSAMP - COS(SMANG)*SDIST/2.
          ELSE
            RLINE = RLINE + SIN(SMANG)
            RSAMP = RSAMP + COS(SMANG)
          ENDIF
        ENDIF

C       ....Compute DN value at (rline,rsamp) via bilinear interpolation
        IL = RLINE
        IS = RSAMP
        IF (IL.LT.1.OR.IL.GE.NL) GOTO 58
        IF (IS.LT.1.OR.IS.GE.NS) GOTO 58
        x = RLINE - IL
        y = RSAMP - IS
        IF (ICODE.EQ.1) THEN
          L1(1) = PIC(IS,IL)
          L2(1) = PIC(IS+1,IL)
          L3(1) = PIC(IS,IL+1)
          L4(1) = PIC(IS+1,IL+1)
	  d1 = byte2int(l1(1))
	  d2 = byte2int(l2(1))
	  d3 = byte2int(l3(1))
	  d4 = byte2int(l4(1))
        ELSE
           d1 = HPIC(IS,IL)
           d2 = HPIC(IS+1,IL)
           d3 = HPIC(IS,IL+1)
           d4 = HPIC(IS+1,IL+1)
        ENDIF
        dn = d1 + (d2-d1)*x + (d3-d1)*y + (d1-d2-d3+d4)*x*y
        SUMDN = SUMDN + dn
        NPTS = NPTS + 1
   58   IF (RPLOT) THEN
           RLON = RLON + DLON
           IF (DPLOT.AND.(RLON.LT.CENTL+0.5*WLON)) GOTO 55
           IF (RLON.LT.ELON) GOTO 55
        ELSE
           IF (.NOT.SPLOT) THEN
             R = R + DR
             IF (R.LT.ER) GOTO 55
           ELSE
             IF (NPTS.LE.SDIST) GOTO 56
           END IF
        ENDIF

        IF(NPTS.GT.0) J = J + 1
        IF(NPTS.GT.0) WORK3(2,J)=SUMDN/NPTS
        IF (RPLOT) THEN
           IF (NPTS.GT.0) THEN
              WORK3(1,J)=R
              IF (DPLOT) ARPTS(1,J)=CENTL
           ENDIF
           IF (J.EQ.5000) THEN
              CALL XVMESSAGE('*** Warning, filled plot array',' ')
           ELSE
              R = R + DR
              IF (R.LE.ER) GOTO 50
           ENDIF
        ELSE
           IF (NPTS.GT.0) WORK3(1,J)=RLON
           IF (J.EQ.5000) THEN
              CALL XVMESSAGE('*** Warning, filled plot array',' ')
           ELSE
              RLON = RLON + DLON
              IF (RLON.LE.ELON) GOTO 50
           ENDIF
        ENDIF
59      N3 = J
      ENDIF

      IF (XVIPTST('PLOT').OR.REDO) THEN
         CALL LINSAM(IND,AVGR,AVGLON,RLINE,RSAMP)
         YY(1) = ZOOM*(RLINE-SL) + 1.5
         XX(1) = ZOOM*(RSAMP-SS) + 1.5
         IF (N3.LE.0) THEN
            CALL XVMESSAGE('*** Error, nothing to plot',' ')
            GOTO 10
         ELSE IF (N3.GT.1000) THEN
           S0 = 19
         ELSE
           S0 = 19
           IF (XX(1).LE.256) S0=271
         ENDIF
         L0 = 237
         IF (YY(1).LE.256) L0=493
         CALL PRDISPLAY(WORK3,N3,RPLOT,L0,S0,X0,Y0,DX,DY)
         REDO = .FALSE.
      ENDIF

      IF (XVIPTST('CP')) THEN
   12    XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
         WRITE(CMSG,115) LINE,SAMP,X0+DX*FLOAT(SAMP-S0),
     &                   Y0+DY*FLOAT(LINE-L0)
  115    FORMAT('(L,S)=(',I4,',',I4,') (XAXIS,DN)=(',
     &         F9.2,',',F9.2,')')
         CALL XVMESSAGE(CMSG,' ')
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 12
         GOTO 10
      ENDIF

      IF (PARMTST('OUTPUT',OUTFILE,I)) THEN
        DOPHA = .FALSE.
        IF (XVIPTST('PHASE')) DOPHA=.TRUE.
        OPEN(1,FILE=OUTFILE,STATUS='NEW',ERR=994)

        MSG(1:80) = ' '
        MSG(1:25) = ' Plot of frame xxxxxxx   '
        WRITE (MSG(16:25),'(I10)') FRAME_ID
        CALL XVMESSAGE(MSG(2:25),' ')
        WRITE(1,113) (MSG(I:I),I=1,30)
  113   FORMAT(37A1)
        MSG(1:80) = ' ' ! VRH added to indicate plane 5/30/89
        IF (MRING.EQ.1) THEN
            MSG(1:36) = ' Reference plane is planet''s equator'
        ELSE
            WRITE(CMSG,116) RINGS(MRING)
            MSG(1:34) = CMSG(1:34)
        ENDIF
        CALL XVMESSAGE(MSG(2:36),' ')
        WRITE(1,113) (MSG(I:I),I=1,36)
        MSG(1:80) = ' ' ! VRH bug fix - clear line 5/31/89
        IF (DOPHA) THEN
          MSG(1:37) = '     LONG    RADIUS      DN     PHASE'
          WRITE(1,113) (MSG(I:I),I=1,37)
          MSG(1:37) = '     ----    ------      --     -----'
          WRITE(1,113) (MSG(I:I),I=1,37)
        ELSE
          MSG(1:28) = '     LONG    RADIUS      DN '
          WRITE(1,113) (MSG(I:I),I=1,30)
          MSG(1:28) = '     ----    ------      -- '
          WRITE(1,113) (MSG(I:I),I=1,30)
        ENDIF
        MSG(1:80) = ' ' ! VRH bug fix - clear line 5/31/89
        DO J=1,N3
          IF (RPLOT) THEN
            IF (DPLOT) THEN
              RLON = ARPTS(1,J) !CENTL
            ELSE
              RLON = AVGLON
            ENDIF
            WRITE (MSG(4:10),'(F7.2)') RLON*RTD
            WRITE (MSG(12:20),'(F9.1)') WORK3(1,J)
            IF (DOPHA)
     &         CALL PHASE(WORK3(1,J),RLON,PHA,DUMMY,DUMMY,DUMMY)
          ELSE
            WRITE (MSG(4:10),'(F7.2)') WORK3(1,J)*RTD
            WRITE (MSG(12:20),'(F9.1)') AVGR
            IF (DOPHA)
     &         CALL PHASE(AVGR,WORK3(1,J),PHA,DUMMY,DUMMY,DUMMY)
          ENDIF
          WRITE (MSG(23:30),'(F8.2)') WORK3(2,J)
          IF (DOPHA) THEN
            WRITE (MSG(32:37),'(F6.2)') PHA*RTD ! VRH Change to 37 5/31/89
            WRITE(1,113) (MSG(I:I),I=1,37)
          ELSE
            WRITE(1,113) (MSG(I:I),I=1,31)
          ENDIF
        END DO
        CLOSE(1,ERR=995)
        CALL XVMESSAGE('Written to disc',' ')
        GOTO 10
  994   CALL XVMESSAGE('***Error opening output file',' ')
        GOTO 10
  995   CALL XVMESSAGE('***Error closing output file',' ')
        GOTO 10
      ENDIF

      GOTO 10
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create starfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create t1950.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute time since 1950 (in days)
C
      SUBROUTINE T1950(IDATE,ITIME,RSC,t) !VRH 7/28/89 
      IMPLICIT REAL*8 (A-H,O-Z)

      IYEAR = IDATE/1000
      IDAY = MOD(IDATE,1000) + 365.25*(IYEAR-1900) - 18262.125 !days since 1950
      JTIME = ITIME/1000
      ISEC = MOD(JTIME,100)
      JTIME = JTIME/100
      IMIN = MOD(JTIME,100)
      IHOUR = JTIME/100
      HOUR = IHOUR + (ISEC/60.D0 + IMIN)/60.0D0
      T = IDAY + HOUR/24.0D0
C Make one-way light travel time correction to planet (SEDR79V2 does this
C for planet but T here is for rings).  VRH 7/28/89 - ERT changed to Object
      DELERT = RSC/2.997925D+5/86400.	! Light travel time in days
      T = T - DELERT	!Adjust to Planet observation time
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine longitude and time of standard epoch.
C
      SUBROUTINE GETEPOCH(PLANET_ID,ALPHAp,DELTAp,THETA,ZETAZ,phip,t0)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 PLANET_ID

      IF (PLANET_ID.LT.5) RETURN
C
C VRH 7/28/89 - Time of Epochs changed from Earth received time to
C               Planet time.
C
C Saturn:  Carolyn's epoch similar to French's except intersection of
C          Saturn's equator 1950 with Earth's equator at EME1950
C          Carolyn's epoch starts at 23:46 UT, 11/12/80 =
C          Time of Voyager 1 closest approch
C
C Uranus:  French's epoch starts at 20:00 UT, 3/10/77 
C
C Neptune: Epoch = 0h ET ( = UT + 56.184 sec), SCET, Aug 25 1989
C                  (determined by NAV team)
C
      IF (PLANET_ID.EQ.5) THEN		!****Jupiter undefined
         T0=0.D0
      ELSE IF (PLANET_ID.EQ.6) THEN	!Carolyn's epoch
         T0 = 365.25*80.0 - 18262.125 + (305.+12.) + 23./24. + 46./1440. 
      ELSE IF (PLANET_ID.EQ.7) THEN	!French's epoch
        T0 = 365.25*77.0 - 18262.125 + (31+28+10) + 20./24.  !days since EME50
      ELSE IF (PLANET_ID.EQ.8) THEN
        T0 = 365.25*89.0 - 18262.125 + (31+28+31+30+31+30+30+25)
      ENDIF
 
C     ....Calculate longitude PHIp
      A =  DSIN(THETA)*DCOS(ZETAZ)
      B = -DSIN(THETA)*DSIN(ZETAZ)
      C =  DCOS(THETA)
      D =  DCOS(DELTAp)*DCOS(ALPHAp)
      E =  DCOS(DELTAp)*DSIN(ALPHAp)
      F =  DSIN(DELTAp)
      X = B*F - C*E
      Y = C*D - A*F
      Z = A*E - B*D
      PHIp = DASIN(Z/DSQRT(X**2+Y**2+Z**2)/DCOS(DELTAp))
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the orbital data, navigate each of the rings.
C IMODE = 0  Get orbital data from OE (C2 common block)
C       = 1  Get orbital data using GETRING (most current parameters)
      SUBROUTINE ERING0(PLANET_ID,T,T0,IMODE) 
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 PLANET_ID

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

      common/coe/oe(110,4),oef,oefname	!Orbital elements for J,S,U,N
      integer*4 oef
      character*256 oefname

      COMMON/C2/DATE,PBUF(4),RBUF(7,15)
      REAL*8 ME0(3,3)

      CALL ME0CALC(ALPHAp,DELTAp,me0)	!Get planet's ME-matrix

      DO IRING=1,15  ! To include new rings VRH 6/7/89
          IF (IMODE.EQ.0) THEN  !VRH 7/27/89 old way - Get from OEF
            CALL MVE(7,14,RBUF(1,IRING),smaa,1,1)	!Get orbital elements
          ELSE
            CALL GETRING(IRING) !Use most current orbital elements
          ENDIF
!  where does T come from ?? <lwk>
          CALL MECALC(ME0,INCL,OMEGAZ,PHIp,PIZERO,
     &           dOMEGA_dt,dw_dt,T,T0,me)
          CALL FROMEME(ME,PSC,sclat,sclon)		! Compute SCLAT,SCLON
          CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,psc3)	! Compute PSC3
          CALL FROMEME(ME,PSUN,sunlat,sunlon)		! Compute SUNLAT,SUNLON
          CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,psun3)	! Compute PSUN3
C             Calculate OM-matrix for ring
          CALL GETANGLES(CM,ME(1,3),PSC,angln,angla,anglb)
          CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,om) !Compute OM-matrix
          CALL PUTRING(IRING)		!Save orbital data
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get ring data from orbital elements file
C
      SUBROUTINE RINGIN(PLANET_ID,rings,nrings,oe,oef,oefname)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 PLANET_ID,OEF
      CHARACTER*1 RINGS(15)
      REAL*8 OE(110,4)			!Orbital elements for J,S,U,N
      CHARACTER*256 OEFNAME

      COMMON/C2/DATE,PBUF(4),RBUF(7,15)
      CHARACTER*80 MSG

      CHARACTER*1 DRINGS(15)/'P','1','2','3','4','5','6','7','8','9',
     &			   'V','W','X','Y','Z'/ ! Default VRH 6/7/89
      CHARACTER*1 SRINGS(13)/'P','F','K','E','A','N','S','W','B','M',
     &			   'R','T','C'/
      CHARACTER*1 URINGS(11)/'P','6','5','4','A','B','N','G','D','L',
     &			   'E'/
      CHARACTER*1 RINGS8(11)/'P','A','B','C','5','3','4','2','1','T',
     &                     'N'/

      IF (PLANET_ID.LT.5) RETURN	!No rings for inner planets

      DO I=1,15
        RINGS(I) = DRINGS(I)		! Load Default rings VRH 6/7/89
      ENDDO
      IF (PLANET_ID.EQ.5) THEN
        NRINGS = 1
        RINGS(1) = SRINGS(1)		!Ring=P for Jupiter
      ELSE IF (PLANET_ID.EQ.6) THEN
	NRINGS = 13
	DO I=1,13
          RINGS(I) = SRINGS(I)		!Ring names for Saturn
	ENDDO
      ELSE IF (PLANET_ID.EQ.7) THEN
	NRINGS = 11
	DO I=1,13
          RINGS(I) = URINGS(I)		!Ring names for Uranus
	ENDDO
      ELSE
	NRINGS = 11
	DO I=1,11
          RINGS(I) = RINGS8(I)		!Ring names for Neptune
	ENDDO
      ENDIF
C     ....Open Ring Orbital Element File
      CALL XVPARM('OEF',oefname,icnt,idef,0)
      if (icnt.eq.0) then
        call xvmessage(' Missing OEF parameter....',' ')
        call xvmessage(' See help on OEF parameter.',' ')
        call xvmessage(' NAV task cancelled',' ')
	call abend
      endif
      CALL XVUNIT(oef,'OE',1,IND,'U_NAME',OEFNAME,' ')
      CALL XVSIGNAL(OEF,IND,.TRUE.)
      CALL XVOPEN(OEF,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
C     ....Read records for J,S,U, and N
      DO L=1,4
         CALL XVREAD(OEF,OE(1,L),ind,' ')
      ENDDO
      CALL XVCLOSE(OEF,ind,' ')
c VRH: 10/11/02 New Ringorbs format
      CALL MVE(8,110,OE(1,PLANET_ID-4),date,1,1)  ! fill C2 common block
      WRITE(MSG,100) NINT(OE(1,PLANET_ID-4))
100   FORMAT('Orbital Elements last update (YYYYDDD): ',I7)
      call xvmessage(msg,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Write orbital elements to Orbital Elements File.
C
      SUBROUTINE RINGOUT(OEF,OE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OE(110,4)		!Orbital elements for rings of J,S,U,N
      INTEGER*4 OEF

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
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

      COMMON/C2/DATE,PBUF(4),RBUF(7,15) !Reclen=8*110
      CHARACTER*16 CDATE

      CALL XVOPEN(OEF,IND,'OP','WRITE','U_NL',4,'U_NS',111,
     &	'U_FORMAT','DOUB','O_FORMAT','DOUB',
     &	'OPEN_ACT','S','IO_ACT','SA',' ')
      IF (IND.LT.1) RETURN
C     ....Build orbital elements for planet in PBUF,RBUF
      CALL DATFMT(1,cdate,i)		!Load current date
      DATE = i
      CALL MVE(8,4,ALPHAp,pbuf,1,1)	!Load planet orientation data
      DO J=1,15				!Load ring orbital elements VRH 6/7/89
         CALL GETRING(J)
         CALL MVE(7,14,SMAA,rbuf(1,j),1,1)
      ENDDO
c VRH: 10/11/02 New Ringorbs format
      CALL MVE(8,110,date,oe(1,planet_id-4),1,1)	!Move to OE buffer

      DO L=1,4
         CALL XVWRIT(OEF,OE(1,L),ind,' ')
      ENDDO
      CALL XVCLOSE(OEF,ind,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the right-ascension and declination of north pole, compute
C the planet's ME matrix.
C
      SUBROUTINE ME0CALC(ALPHAp,DELTAp,ME0)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME0(3,3)

      SINAp = DSIN(ALPHAp)
      COSAp = DCOS(ALPHAp)
      SINDp = DSIN(DELTAp)
      COSDp = DCOS(DELTAp)

      ME0(1,1) = -SINAp
      ME0(2,1) =  COSAp
      ME0(3,1) =  0.D0

      ME0(1,2) = -SINDp*COSAp
      ME0(2,2) = -SINDp*SINAp
      ME0(3,2) =  COSDp

      ME0(1,3) =  COSDp*COSAp
      ME0(2,3) =  COSDp*SINAp
      ME0(3,3) =  SINDp

      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate ME, given ME0, angles i, OMEGA, PHIp, and PIZERO,
C apsidal and nodal precession rates dOMEGA_dt and dw_dt,
C and times T and T0.
C Note: ERING data is in EME50 (calculated ME is transformed to ISYSTEM)
C
      SUBROUTINE MECALC(ME0,i,OMEGAZ,PHIp,PIZERO,dOMEGA_dt,
     &              dw_dt,T,T0,ME)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME0(3,3),ME(3,3),i
C
      COMMON/SEDR/SEDR(200),LBUF(100),ISYSTEM
      REAL*4 SEDR
      INTEGER LBUF,ISYSTEM  !1=j2000, 2=b1950
      integer b1950, toref
      doubleprecision mtx(3,3)
C
C         Get angles OMEGA and w
      OMEGA = OMEGAZ + dOMEGA_dT*(T-T0)
      wbar = PIZERO + dw_dt*(T-T0)
      w = wbar - OMEGA
C         The ME is initially matrix M1 (rotation from periapse to node
C         about z-axis).
      ME(1,1) =  DCOS(w)
      ME(2,1) =  DSIN(w)
      ME(3,1) =  0.D0

      ME(1,2) = -DSIN(w)
      ME(2,2) =  DCOS(w)
      ME(3,2) =  0.D0

      ME(1,3) =  0.D0
      ME(2,3) =  0.D0
      ME(3,3) =  1.D0
C         At this point, x-axis lies in intersection of planet's equatorial
C         plane and ring plane.  Rotate about x-axis thru inclination angle
C         i so that z-axis aligns with planet's pole (ME = M2*M1).
      DO 20 J=1,3
      TEMP    = DCOS(i)*ME(2,J) - DSIN(i)*ME(3,J)
      ME(3,J) = DSIN(i)*ME(2,J) + DCOS(i)*ME(3,J)
   20 ME(2,J) = TEMP
C         Rotate about z-axis thru angle OMEGA+PHIp so that x-axis coincides
C         with node of planet on EME1950 (ME = M3*M2*M1).
      DO 30 J=1,3
      TEMP    = DCOS(OMEGA+PHIp)*ME(1,J) - DSIN(OMEGA+PHIp)*ME(2,J)
      ME(2,J) = DSIN(OMEGA+PHIp)*ME(1,J) + DCOS(OMEGA+PHIp)*ME(2,J)
   30 ME(1,J) = TEMP
C         Now rotate into EME50 (ME = ME0*M3*M2*M1).
      DO 40 J=1,3
      TEMP1   = ME0(1,1)*ME(1,J)+ME0(1,2)*ME(2,J)+ME0(1,3)*ME(3,J)
      TEMP2   = ME0(2,1)*ME(1,J)+ME0(2,2)*ME(2,J)+ME0(2,3)*ME(3,J)
      ME(3,J) = ME0(3,1)*ME(1,J)+ME0(3,2)*ME(2,J)+ME0(3,3)*ME(3,J)
      ME(1,J) = TEMP1
   40 ME(2,J) = TEMP2
C
C VRH 10/11/02 - transform ME to J2000 if ISYSTEM=1
C
      IF (ISYSTEM.EQ.1) THEN
         call irfnum( 'B1950', b1950)      !get SPICE index of B1950 ref frame
         call irfnum( 'J2000', toref)
         call irfrot(b1950,toref,mtx)      !Get rotation matrix
         call mxm(me,mtx,me)               !Convert from b1950 to isystem
      ENDIF

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Edit orbital data for eccentric rings
C
      SUBROUTINE ERING
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE !VRH 6/28/89 for RINGPT - show whole ring
      INTEGER*4 SL,SS

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      CHARACTER*1 RINGS(15)

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

      COMMON/COE/OE(110,4),OEF,OEFNAME	!Orbital elements for J,S,U,N
      INTEGER*4 OEF
      CHARACTER*256 OEFNAME

      COMMON/C2/DATE,PBUF(4),RBUF(7,15)

      COMMON/CONST/PI,DTR,RTD

      REAL*8 ME0(3,3)
      CHARACTER*1 RING_ID(20)
      LOGICAL PARMTST,rPARMTST,XVIPTST
      real*4 mindis,maxdis,r

      IF (PLANET_ID.LT.6) THEN
        CALL XVMESSAGE('***No eccentric rings for Jupiter',' ')
        RETURN
      ENDIF
      IRING = -1	!Set to invalid ring-id

   20 CALL XVINTRACT('ERING','ERING')
      IF (XVIPTST('HELP')) CALL XVINTRACT('ERING',' ')
      IF (XVIPTST('EXIT')) RETURN
      ICHANGE = 0

      IF (PARMTST('RING',RING_ID(1),I)) THEN
          CALL UPRCASE(RING_ID)
          DO J=1,15 ! For new rings VRH 6/7/89
            IF (RING_ID(1).EQ.RINGS(J)) THEN
                IRING = J		!Set the ring-id
                CALL GETRING(IRING)	!Get orbital data
                IF (IRING.EQ.1) GOTO 20
                IF (SMAA.EQ.0.) GOTO 20      !Skip drawing zero ring VRH 6/8/89
		mindis = 5.0
		maxdis = 10.0
                CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &              OAL,OAS,ZSCALE,1,1,NL,NS,NLW,NSW, 
     &              NSEARCH,0,mindis,maxdis,RPTS,NRPTS)
                IF (NRPTS.GT.0) CALL DRAWCURVE(RPTS,NRPTS,0)
                GOTO 20
            ENDIF
          ENDDO
          CALL XVMESSAGE('***Invalid ring name',' ')
          GOTO 20
      ENDIF

      IF (IRING.EQ.-1) THEN
          CALL XVMESSAGE('***Enter ring-ID before editing',' ')
          CALL XVMESSAGE('   by using RING command',' ')
          GOTO 20
      ENDIF

      IF (XVIPTST('RESTORE')) THEN               
         L = PLANET_ID - 4
         CALL MVE(8,110,OE(1,L),date,1,1)
         IF (IRING.EQ.1) THEN !VRH 7/27/89 IRING=1 only changes ALPHAp to ZETAZ
             CALL MVE(8,4,PBUF,alphap,1,1)
         ELSE
             CALL MVE(7,14,RBUF(1,IRING),smaa,1,1)
         ENDIF
         ICHANGE = 1
      ENDIF

      IF (IRING.GT.1) THEN !VRH 7/27/89 IRING>1 only changes SMAA,...,dw_dt
        IF (rPARMTST('SMAA',R,I)) THEN
            IF (R.LT.1.D0) THEN ! VRH fix from SMAA to R 6/14/89 
                CALL XVMESSAGE('***Invalid SMAA',' ')
                GOTO 20
            ENDIF
            SMAA = R
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('ECC',R,I)) THEN
            IF (R.LT.0.D0.OR.R.GT.0.99D0) THEN ! VRH fix from ECC to R 6/14/89 
                 CALL XVMESSAGE('***Invalid eccentricity',' ')
                 GOTO 20
            ENDIF
            ECC = R
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('INCL',R,I)) THEN
            INCL = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('PIZERO',R,I)) THEN
            PIZERO = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('OMEGZ',R,I)) THEN
            OMEGAZ = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('DW',R,I)) THEN  !VRH 7/22/89 option add
            dw_dt = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('DOMEGZ',R,I)) THEN  !VRH 7/22/89 option add
            dOMEGA_dt = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('RA',R,I).OR.rPARMTST('DEC',R,I).OR.  !VRH 7/27/89
     &    rPARMTST('THETA',R,I).OR.rPARMTST('ZETAZ',R,I)) THEN
          CALL XVMESSAGE('***RA,DEC,THETA,ZETAZ: Valid for RING=P only',
     1     ' ')
        ENDIF
      ELSE
        IF (rPARMTST('RA',R,I)) THEN
            ALPHAp = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('DEC',R,I)) THEN
            DELTAp = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('THETA',R,I)) THEN
            THETA = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('ZETAZ',R,I)) THEN
            ZETAZ = R*DTR
            ICHANGE = 1
        ENDIF

        IF (rPARMTST('SMAA',R,I).OR.rPARMTST('ECC',R,I).OR.
     &      rPARMTST('INCL',R,I).OR.rPARMTST('PIZERO',R,I).OR.
     &      rPARMTST('OMEGZ',R,I).OR.rPARMTST('DW',R,I).OR.
     &      rPARMTST('DOMEGZ',R,I)) THEN
            CALL XVMESSAGE('*** SMAA,ECC,INCL,PIZERO,OMEGAZ,DW,DOMEGZ:',
     &       ' ')
            CALL XVMESSAGE('*** Not valid for RING=P',' ')
        ENDIF
      ENDIF

      IF (XVIPTST('STATUS')) CALL RSTATUS(IRING)

      IF (ICHANGE.EQ.1) THEN
          IF (IRING.NE.1) THEN  !VRH 7/27/89 Renavigate IRING
            IF (SMAA.EQ.0.) THEN     !Skip zero ring VRH 6/14/89
                CALL XVMESSAGE('***Ring SMAA not defined',' ')
                GOTO 20
            END IF
            CALL ME0CALC(ALPHAp,DELTAp,ME0)
C             Calculate ME-matrix for ring
            CALL MECALC(ME0,INCL,OMEGAZ,PHIp,PIZERO,
     &               dOMEGA_dt,dw_dt,T,T0,ME)
            CALL FROMEME(ME,PSC,SCLAT,SCLON)		! Compute SCLAT,SCLON
            CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)	! Compute PSC3
            CALL FROMEME(ME,PSUN,SUNLAT,SUNLON)	! Compute SUNLAT,SUNLON
            CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3) ! Compute PSUN3
C             Calculate OM-matrix for ring
            CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
            CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
	    mindis = 5.0
	    maxdis = 10.0
            CALL RINGPT(SMAA,ECC,OM,PSC3,PSUN3,SCLON,RLORA,RA,RC,
     &            OAL,OAS,ZSCALE,1,1,NL,NS,NLW,NSW,NSEARCH,0,
     &            mindis,maxdis,RPTS,NRPTS)
            IF (NRPTS.GT.0) CALL DRAWCURVE(RPTS,NRPTS,1)
            CALL PUTRING(IRING)
          ELSE  !Renavigate all rings VRH 7/27/89
            CALL XVMESSAGE('Updating all rings',' ')
            CALL GETEPOCH(PLANET_ID,ALPHAp,DELTAp,THETA,ZETAZ,phip,t0)
            CALL ERING0(PLANET_ID,T,T0,1)
            CALL GETRING(1)  !Restore P-ring
          ENDIF
          GOTO 20
      ENDIF

      IF (XVIPTST('WRITE')) CALL RINGOUT(OEF,OE)
      GOTO 20
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to print summary of ring orbital data
C
      SUBROUTINE RSTATUS(IRING)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRINGC/ RINGS
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

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
      CHARACTER*80 MSG
      CHARACTER PLANET*6,YEAR*4
 
  100 FORMAT('Orbital Data for ',A1,'-Ring')
  108 FORMAT('Semi-Major Axis (km)                SMAA  ',F10.0)
  109 FORMAT('Eccentricity                        ECC   ',F10.5)
  110 FORMAT('Inclination (deg)                   INCL  ',F10.5)
  111 FORMAT('Longitude of Periapse (deg)         PIZERO',F10.5)
  112 FORMAT('Longitude of Ascending Node (deg)   OMEGAZ',F10.5)
  113 FORMAT('Precession of Periapse (deg/day)    dw/dt ',F10.5)
  114 FORMAT('Precession of Node (deg/day)        dOM/dt',F10.5)
  115 FORMAT('R.A. of ',A6,' pole (deg)           RA    ',F10.5)
  116 FORMAT('DEC  of ',A6,' pole (deg)           DEC   ',F10.5)
  117 FORMAT('Earth pole ',A4,' (deg)               THETA ',F10.5)
  118 FORMAT('Earth pole ',A4,' (deg)               ZETAZ ',F10.5)

      WRITE(MSG,100) RINGS(IRING)
      CALL XVMESSAGE(MSG,' ')

      IF (IRING.GT.1) THEN
        WRITE(MSG,108) SMAA
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,109) ECC
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,110) INCL*RTD
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,111) PIZERO*RTD
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,112) OMEGAZ*RTD
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,113) dw_dt*RTD  !VRH 7/22/89 option add
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,114) dOMEGA_dt*RTD  !VRH 7/22/89 option add
        CALL XVMESSAGE(MSG,' ')
      ELSE
        IF (PLANET_ID.EQ.6) THEN
           PLANET = 'Saturn'
           YEAR = '1950'
        ELSE IF (PLANET_ID.EQ.7) THEN
           PLANET = 'Uranus'
           YEAR = '1950'
        ELSE
           PLANET = 'Neptune'
           YEAR = '1950'
        ENDIF

        WRITE(MSG,115) PLANET,ALPHAp*RTD
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,116) PLANET,DELTAp*RTD
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,117) YEAR,THETA*RTD
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,118) YEAR,ZETAZ*RTD
        CALL XVMESSAGE(MSG,' ')
      ENDIF

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create carea.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to enable the user to interactively specify an area of
C the picture by selecting the (top,left) and (bottom,right) corners
C via the cursor.
C
C Updated arguments: ISL,ISS,INL,INS
C
      SUBROUTINE CAREA(ISL,ISS,INL,INS)
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'fortport'        ! defines int2byte
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      common/navdv2/xdw,xdb
      integer xdw,xdb
      byte bmdn,bidn

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER X(2),Y(2),SAMP,LINE
      LOGICAL XST,XDCLOCATION,XDIPOLYLINE,XVIPTST

      bmdn = int2byte(xdw)
      bidn = int2byte(xdb)

      IEL = ISL + INL - 1
      IES = ISS + INS - 1

      CALL XVMESSAGE('Move Cursor to (top,left) corner of area',' ')
      CALL XVINTRACT('SKIP',
     &  ' Hit Return when ready or type ''S to skip')
      IF (XVIPTST('S')) GOTO 30

   10 XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
      X(1) = SAMP
      Y(1) = LINE
      X(2) = NSDS
      Y(2) = LINE
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      X(2) = SAMP
      Y(2) = NLDS
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      CALL XVINTRACT('CONTINUE',
     &      ' Hit Return to redo or type ''C to continue')
      IF (.NOT.XVIPTST('C')) THEN
           X(1) = SAMP				! Erase old stuff
           Y(1) = LINE
           X(2) = NSDS
           Y(2) = LINE
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           X(2) = SAMP
           Y(2) = NLDS
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           GOTO 10				! go back and redo
      ENDIF

      ISL = SL + LINE/ZOOM
      ISS = SS + SAMP/ZOOM
C
C              
   30 CALL XVMESSAGE('Move Cursor to (bottom,right) corner of area',' ')
      CALL XVINTRACT('SKIP',
     &  ' Hit Return when ready or type ''S to skip')
      IF (XVIPTST('S')) GOTO 50

   40 XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
      X(1) = 1
      Y(1) = LINE
      X(2) = SAMP
      Y(2) = LINE
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      X(1) = SAMP
      Y(1) = 1
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      CALL XVINTRACT('CONTINUE',
     &       ' Hit Return to redo or type ''C to continue')
      IF (.NOT.XVIPTST('C')) THEN
           X(1) = 1
           Y(1) = LINE
           X(2) = SAMP
           Y(2) = LINE
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           X(1) = SAMP
           Y(1) = 1
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           GOTO 40				! go back and redo
      ENDIF

      IEL = SL + LINE/ZOOM
      IES = SS + SAMP/ZOOM

   50 INL = IEL - ISL + 1
      INS = IES - ISS + 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to permit the user to delete bad points via the cursor
C All points within radius R of cursor are flagged as (-99.,-99.)
C Updated arguments: PTS
C
      SUBROUTINE CLEANPTS(PTS,NPTS,R)
      REAL*4 PTS(2,NPTS),r
      LOGICAL XVIPTST
      real*8 rl8,rs8
C
      R2 = R**2
C
  50  CALL XVINTRACT('READY',
     &  ' Hit Return to delete point or type ''EXIT if done')
      IF (XVIPTST('EXIT')) RETURN
      CALL CURSOR(RL8,RS8)		!Read cursor position
      rline = rl8
      rsamp = rs8
C
      DO 20 I=1,NPTS
      RL = PTS(1,I)
      DY = (RL-RLINE)**2
      IF (DY.GT.R2) GOTO 20
      RS = PTS(2,I)
      DX = (RS-RSAMP)**2
      IF (DX+DY.GT.R2) GOTO 20
      PTS(1,I) = -99.0
      PTS(2,I) = -99.0
      CALL DRAWDOT(RL,RS,0)
   20 CONTINUE
C
      GOTO 50
C
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Automatic cleaning routine searches for isolated bad points on the acquired
C curve.  A point is bad if it is more than R pixels away from its two nearest 
C neighbors.  All bad points  are flagged as (-99.,-99.)
C The computed curve CPTS is required to check for continuity.
C
C Updated arguments: APTS
C
      SUBROUTINE CLEANPT1(CPTS,APTS,NPTS,R)
      REAL*4 CPTS(2,NPTS),APTS(2,NPTS),r

      R2 = R**2
      CL2 = CPTS(1,1)
      CS2 = CPTS(2,1)
      CL3 = CPTS(1,2)
      CS3 = CPTS(2,2)
      AL2 = APTS(1,1)
      AS2 = APTS(2,1)
      AL3 = APTS(1,2)
      AS3 = APTS(2,2)

      DO 20 I=3,NPTS
      CL1 = CL2
      CS1 = CS2
      CL2 = CL3
      CS2 = CS3
      CL3 = CPTS(1,I)
      CS3 = CPTS(2,I)
      AL1 = AL2
      AS1 = AS2
      AL2 = AL3
      AS2 = AS3
      AL3 = APTS(1,I)
      AS3 = APTS(2,I)
      IF (AL2.LT.0.D0) GOTO 20
      IF ( (CL3-CL1)**2+(CS3-CS1)**2 .GT. 100.0 ) GOTO 20
      IF ( (AL2-AL1)**2+(AS2-AS1)**2 .LT. R2 ) GOTO 20
      IF ( (AL3-AL2)**2+(AS3-AS2)**2 .LT. R2 ) GOTO 20
      APTS(1,I-1) = -99.0
      APTS(2,I-1) = -99.0
   20 CONTINUE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to flag all (line,sample) coordinates that are within
C radius R of a Voyager reseau.  All points outside picture margin
C are also flagged.  Coordinates are flagged as (-99.0,-99.0)
C Note that for image-space frames, the algorithm fails to flag
C points near marks at the corners.  This is because the algorithm
C assumes that distortions are small.  The reseau marks affected
C are: S/N 4 (2,3,13,14,24,200), S/N 5 (2,3,14,35,137,152,167,199,200),
C S/N 6 (24,155,178), S/N 7 (10,11,24,191).
C
C  Updated argument: PTS
C
      SUBROUTINE CLEANVGR(pts,NPTS,RES,R)
      REAL*4 PTS(2,NPTS),RES(2,202),r
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
C
      R2 = R**2			!Square of pixel radius
      IF (RES(1,195).LT.900.0) THEN  !If image-space, then
         RNL = 800.0		!number of lines=800
         RNS = 800.0		!number of samps=800
      ELSE			!else, object-space.
         RNL = 1000.0
         RNS = 1000.0
      ENDIF

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
      DO 100 I=1,NPTS
      RLINE = PTS(1,I)
      RSAMP = PTS(2,I)
      IF (RSAMP.LT.1.0 .OR. RSAMP.GT.RNS) GOTO 90
      IL = (RLINE+LOFFSET)/DL

      IF (RLINE.LT.TL2) THEN
         IF (RLINE.LT.1.0) GOTO 90
         IF (RSAMP.LT.LEFTSAMP) THEN
            D = (RES(1,1)-RLINE)**2 + (RES(2,1)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
            D = (RES(1,13)-RLINE)**2 + (RES(2,13)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
         ELSE
            IF (RSAMP.GT.RIGTSAMP) THEN
               D = (RES(1,12)-RLINE)**2 + (RES(2,12)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
               D = (RES(1,23)-RLINE)**2 + (RES(2,23)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
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
         IF (RLINE.GT.RNL) GOTO 90
         IF (RSAMP.LT.LEFTSAMP) THEN
            D = (RES(1,190)-RLINE)**2 + (RES(2,190)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
            D = (RES(1,179)-RLINE)**2 + (RES(2,179)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
         ELSE
            IF (RSAMP.GT.RIGTSAMP) THEN
               D = (RES(1,201)-RLINE)**2 + (RES(2,201)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
               D = (RES(1,189)-RLINE)**2 + (RES(2,189)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
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
      IF (D.GT.R2) GOTO 100
   90 PTS(1,I) = -99.0
      PTS(2,I) = -99.0
  100 CONTINUE
C
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create getangles.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate ANGLN, ANGLA, and ANGLB from the C-matrix, spin vector,
C and spacecraft vector.
C
C Inputs: CM,N,PSC,SCLON,RLORA
C Outputs: ANGLN,ANGLA,ANGLB
C
      SUBROUTINE GETANGLES(CM,N,PSC,angln,angla,anglb)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3),N(3),PSC(3)
      REAL*8 N0(3),P0(3),P1(3),NX,NZ
      COMMON/CONST/PI,DTR,RTD
C
C        Rotate spin vector N and S/C vector PSC into image space by 
C        multiplying by CM-inverse.
      DO I=1,3
         N0(I) = CM(1,I)*N(1) + CM(2,I)*N(2) + CM(3,I)*N(3)
         P0(I) = CM(1,I)*PSC(1) + CM(2,I)*PSC(2) + CM(3,I)*PSC(3)
      ENDDO
C
C     ....Rotate spin vector thru ANGLN so that north is along X-axis
      ANGLN = DATAN2(N0(2),N0(1))	! Compute angle N
      NX = N0(1)*DCOS(ANGLN) + N0(2)*DSIN(ANGLN)
      NZ = N0(3)

C     ....Rotate S/C vector thru angles N and A
      ANGLA = DATAN2(NX,NZ)             ! Compute angle A
      P1(1) =  P0(1)*DCOS(ANGLN) + P0(2)*DSIN(ANGLN)
      P1(2) = -P0(1)*DSIN(ANGLN) + P0(2)*DCOS(ANGLN)
      P1(3) =  P0(3)
      PX = P1(1)*DCOS(ANGLA) - P1(3)*DSIN(ANGLA)
      PY = P1(2)
      ANGLB = DATAN2(PY,PX)		! Compute angle B
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Calculate THETA_N, THETA_A, and THETA_B for the C-matrix.  The angles
C are analoguous to ANGLN, ANGLA, and ANGLB for the OM-matrix.
C
C Input: CM
C Outputs: THETA_N,THETA_A,THETA_B
C
      SUBROUTINE GETANGLES2(CM,theta_n,theta_a,theta_b)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3),N(3),PSC(3)
      REAL*8 N0(3),P0(3),P1(3),NX,NZ
      COMMON/CONST/PI,DTR,RTD
C
C        In this routine, the EME50 coordinate system takes the role
C        of the planet coordinate system.
      N(1) = 0.		!Spin vector is z-axis
      N(2) = 0.
      N(3) = 1.

      PSC(1) = 1.		!Spacecraft vector is x-axis
      PSC(2) = 0.
      PSC(3) = 0.

C        Rotate spin vector N and S/C vector PSC into image space by 
C        multiplying by CM-inverse.
      DO I=1,3
      N0(I) = CM(1,I)*N(1) + CM(2,I)*N(2) + CM(3,I)*N(3)
      P0(I) = CM(1,I)*PSC(1) + CM(2,I)*PSC(2) + CM(3,I)*PSC(3)
      ENDDO
C
C        Rotate spin vector thru THETA_N so that north is along X-axis
      if (n0(1).eq.0.0) then
	THETA_N = 0.5*pi
      else
	THETA_N = DATAN2(N0(2),N0(1))	! Compute angle N
      endif
      NX = N0(1)*DCOS(THETA_N) + N0(2)*DSIN(THETA_N)
      NZ = N0(3)
C         Rotate S/C vector thru angles N and A
      if (nz.eq.0.0) then
	THETA_A = 0.5*pi
      else
	THETA_A = DATAN2(NX,NZ)             ! Compute angle A
      endif
      P1(1) =  P0(1)*DCOS(THETA_N) + P0(2)*DSIN(THETA_N)
      P1(2) = -P0(1)*DSIN(THETA_N) + P0(2)*DCOS(THETA_N)
      P1(3) =  P0(3)
      PX = DCOS(THETA_A)*P1(1) - DSIN(THETA_A)*P1(3)
      PY = P1(2)
      if (px.eq.0.0) then
	THETA_B = 0.5*pi
      else
	THETA_B = DATAN2(PY,PX)		! Compute angle B
      endif
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This routine takes an input C-matrix (C) and computes the three Euler
C angles (ALPHA,DELTA,KAPPA) representing the matrix.
C
C Outputs: ALPHA,DELTA,KAPPA: Euler angles in radians.
C
C The 9 elements of the C matrix are stored in order of increasing address
C as
C                  |  c1  c4  c7  |
C                  |  c2  c5  c8  |
C                  |  c3  c6  c9  |
C
      SUBROUTINE COMPCM(C,alpha,delta,kappa)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 C(9)	!Input C-matrix
      REAL*8 ALPHA	!Output RA of instrument boresight (degrees)
      REAL*8 DELTA	!Output Declination of instrument boresight (degrees)
      REAL*8 KAPPA	!Output rotation about optical axis (degrees)

      PI = 3.141592653589793D0
      RTD = 180.D0/PI

      if (c(7).eq.0.0) then
	ALPHA = 0.5*pi
      else
	ALPHA = DATAN2(C(8),C(7))
      endif
      DELTA = DASIN(C(9))
      if (c(6).eq.0.0) then
	KAPPA = 0.5*pi
      else
	KAPPA = DATAN2(C(3),C(6))
      endif
      IF (ALPHA.LT.0.D0) ALPHA=ALPHA+2.D0*PI
      IF (KAPPA.LT.0.D0) KAPPA=KAPPA+2.D0*PI
CCC      ALPHA = ALPHA*RTD
CCC      DELTA = DELTA*RTD
CCC      KAPPA = KAPPA*RTD
C Temporary code to test whether this works...
CCC      CALL BUILDCM(ctest,ALPHA,DELTA,KAPPA)
CCC      CALL ORTHOT(CTEST)
CCC      CALL PRNT(8,9,C,' C-matrix=.')
CCC      CALL PRNT(8,9,CTEST,' COMPCM C-matrix=.')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to calculate the camera-to-planet rotation matrix (OM)
C from angles ANGLN, ANGLA, and ANGLB.
      SUBROUTINE OMMATRIX(ANGLN,ANGLA,ANGLB,OM)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3)
C
C        The OM-matrix is initially matrix M1 (north angle rotation about
C        z0-axis).
      OM(1,1) =  DCOS(ANGLN)
      OM(2,1) = -DSIN(ANGLN)
      OM(3,1) =  0.D0
C
      OM(1,2) =  DSIN(ANGLN)
      OM(2,2) =  DCOS(ANGLN)
      OM(3,2) =  0.D0
C
      OM(1,3) =  0.D0
      OM(2,3) =  0.D0
      OM(3,3) =  1.D0
C        OM = M2*M1 (rotate about y1-axis through angle A)
      DO 20 J=1,3
      TEMP    = DCOS(ANGLA)*OM(1,J) - DSIN(ANGLA)*OM(3,J)
      OM(3,J) = DSIN(ANGLA)*OM(1,J) + DCOS(ANGLA)*OM(3,J)
   20 OM(1,J) = TEMP
C        OM = M3*M2*M1 (rotate about z2-axis through angle B)
      DO 30 J=1,3
      TEMP    =  DCOS(ANGLB)*OM(1,J) + DSIN(ANGLB)*OM(2,J)
      OM(2,J) = -DSIN(ANGLB)*OM(1,J) + DCOS(ANGLB)*OM(2,J)
   30 OM(1,J) = TEMP
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute the C-matrix from ME and OM' matrices...
C Output: CM
C
      SUBROUTINE CMATRIX(ME,ANGLN,ANGLA,ANGLB,SCLON,CM)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),CM(3,3),OMp(3,3)

C           Compute OM' matrix
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON,OMp)

      DO 150 J=1,3
      DO 150 I=1,3
  150 CM(I,J) = ME(I,1)*OMp(1,J)+ME(I,2)*OMp(2,J)+ME(I,3)*OMp(3,J)

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given a unit vector in EME50 coordinates, compute the latitude-longitude
C coordinates...
C
C Inputs: ME matrix
C         P = unit vector
C Outputs: RLAT,RLON
C
      SUBROUTINE FROMEME(ME,P,RLAT,RLON)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),P(3),xdot
      COMMON/CONST/PI,DTR,RTD

      RLAT = DASIN(DOT(P,ME(1,3)))			!RLAT = P o N
      xdot = DOT(P,ME(1,1))
      if (xdot.eq.0.0) then
	RLON = 0.5*pi
      else
	RLON = DATAN2(DOT(P,ME(1,2)),xdot)
      endif
      RLON = DMOD(RLON+2.0D0*PI,2.0D0*PI)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the latitude-longitude coordinates, compute a unit vector in
C the EME-coordinate system...
C
C Inputs: ME(3,3) = EME50 conversion matrix
C         RLAT,RLON
C
C Outputs: Q(3) = unit vector in EME50 coordinates
C
      SUBROUTINE TOEME(ME,RLAT,RLON,Q)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),P(3),Q(3)
C             Compute unit vector in target centered coordinates...
      CALL VECTOR3(1.D0,RLAT,RLON,P)
C             Convert to EME50 coordinates
      Q(1) = ME(1,1)*P(1) + ME(1,2)*P(2) + ME(1,3)*P(3)
      Q(2) = ME(2,1)*P(1) + ME(2,2)*P(2) + ME(2,3)*P(3)
      Q(3) = ME(3,1)*P(1) + ME(3,2)*P(2) + ME(3,3)*P(3)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the latitude-longitude coordinates and range of a point, compute
C its vector in target centered coordinates.
C
C Inputs: RANGE,RLAT,RLON
C Outputs: P(3)
C
      SUBROUTINE VECTOR3(RANGE,RLAT,RLON,P)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 P(3)

      P(1) = RANGE*DCOS(RLAT)*DCOS(RLON)
      P(2) = RANGE*DCOS(RLAT)*DSIN(RLON)
      P(3) = RANGE*DSIN(RLAT)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute offsets (DL,DS) which minimize the distance
C between the computed and actual curves:
C             CHISQ2 = SIGMA((DPi - DL*COSLi - DS*COSSi)**2)
C Inputs:  CPTS,APTS,NPTS,IMODE
C     IMODE=1 if goodness of fit measures are to be printed out
C          =0 otherwise
C Outputs: IND,DL,DS
C     IND = 1 if fit was successful
C         = 0 if error (Det=0)
C
      SUBROUTINE CHISQ2(IND,CPTS,APTS,NPTS,DL,DS,IMODE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 CPTS(2,NPTS),APTS(2,NPTS)
C
      DP = 0.D0
      A11 = 0.D0
      A12 = 0.D0
      A22 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      N = 0			!Count number of points used in fit

      DO 50 I=1,NPTS
      IF (APTS(1,I).LT.0.0) GOTO 50		!Skip point if flagged as bad
      N = N + 1
      DLi = APTS(1,I) - CPTS(1,I)
      DSi = APTS(2,I) - CPTS(2,I)
      DPi = DLi**2 + DSi**2
      IF (DPi.EQ.0.0) GOTO 50
      DP = DP + DPi
      DPi = DSQRT(DPi)
      COSSi = DSi/DPi
      COSLi = DLi/DPi
      A11 = A11 + COSSi**2
      A12 = A12 + COSSi*COSLi
      A22 = A22 + COSLi**2
      B1 = B1 + DSi
      B2 = B2 + DLi
   50 CONTINUE
C
      Det = A11*A22 - A12**2
      IF (DABS(Det).LT.1.D-6) GOTO 990
      DL = (B2*A11 - B1*A12)/Det
      DS = (B1*A22 - B2*A12)/Det
      IND = 1
      IF (IMODE.EQ.0) RETURN
C
C     ....The remainder of the routine measures "goodness of fit"
C     ....Compute CHISQ2 and Covariance
      CALL PRNT(4,1,N,' Number of points used in fit=.')
      IF (N.LE.2) GOTO 992
      CHISQ = DP + DS**2*A11 + DL**2*A22
     &       - 2*DS*B1 - 2*DL*B2 + 2*DS*DL*A12
      CHISQ = CHISQ/DFLOAT(N-2)/0.5  !Using measurement error of 0.5 in L & S
      CALL PRNT(8,1,CHISQ,' CHISQ2=.')
      CALL COVMT2(A11,A12,A22,2.*CHISQ)
      RETURN

  990 CALL XVMESSAGE('***Error fitting curve',' ')
      CALL PRNT(4,1,N,'***Number of points used in fit=.')
      IND = 0
      RETURN

  992 CALL XVMESSAGE(
     & '***Warning: Insufficient number of points used in fit',' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE COVMT2(A11,A12,A22,VAR)  !Fixed by VRH 5/23/89
      IMPLICIT NONE
      REAL*8 A11,A12,A22,VAR
      REAL*8 CA11,CA12,CA22,DETA
      CHARACTER*80 MSG
C
      DETA = A11*A22 - A12**2
      IF (DETA.LE.0.D0) THEN
          CALL XVMESSAGE('***Warning: Covariance matrix is singular',
     &	   ' ')
          RETURN
      ENDIF
C
      CA11 =  A22*VAR/DETA
      CA12 = -A12*VAR/DETA
      CA22 =  A11*VAR/DETA
C
      CALL XVMESSAGE('Covariant Matrix:  S (pixels), L (pixels)',' ')
      WRITE(MSG,110) CA11,CA12
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CA12,CA22
      CALL XVMESSAGE(MSG,' ')
  110 FORMAT('   ',2F20.10)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to Compute offsets (DL,DS) and DT which minimize the distance
C between the computed and actual curves:
C             CHISQ3 = SIGMA((DPi - DL*COSLi - DS*COSSi -DT*RXDi)**2)
C Inputs:  CPTS,APTS,NPTS,OAL,OAS,IMODE
C     IMODE=1 if goodness of fit measures are to be printed out
C          =0 otherwise
C Outputs: IND,DL,DS,DT
C     IND = 1 if fit was successful
C         = 0 if error (Det=0)
C
      SUBROUTINE CHISQ3(IND,CPTS,APTS,NPTS,OAL,OAS,DL,DS,DT,IMODE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 CPTS(2,NPTS),APTS(2,NPTS)
      REAL*8 LAi
C
      DP = 0.D0
      A11 = 0.D0
      A22 = 0.D0
      A33 = 0.D0
      A12 = 0.D0
      A13 = 0.D0
      A23 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      B3 = 0.D0
      N = 0			!Count number of points used in fit
C
      DO 50 I=1,NPTS
      LAi = APTS(1,I)
      IF (LAi.LT.0.0) GOTO 50		!Skip point if flagged as bad
      N = N + 1
      SAi = APTS(2,I)
      DLi = LAi - CPTS(1,I)
      DSi = SAi - CPTS(2,I)
      DPi = DLi**2 + DSi**2
      IF (DPi.LE.0.0) GOTO 50
      DP = DP + DPi
      DPi = DSQRT(DPi)
      COSSi = DSi/DPi
      COSLi = DLi/DPi
      RXDi = (SAi-OAS)*COSLi - (LAi-OAL)*COSSi
      A11 = A11 + COSSi**2
      A22 = A22 + COSLi**2
      A33 = A33 + RXDi**2
      A12 = A12 + COSSi*COSLi
      A13 = A13 + COSSi*RXDi
      A23 = A23 + COSLi*RXDi
      B1 = B1 + DSi
      B2 = B2 + DLi
      B3 = B3 + DPi*RXDi
   50 CONTINUE
C
      D11 = A22*A33 - A23**2
      D22 = A11*A33 - A13**2
      D33 = A11*A22 - A12**2
      D12 = A12*A33 - A13*A23
      D13 = A12*A23 - A13*A22
      D23 = A11*A23 - A13*A12
      Det = A11*D11 - A12*D12 + A13*D13
      IF (DABS(Det).LT.1.D-6) THEN
           CALL PRNT(4,1,N,' Number of points used in fit=.')
           CALL XVMESSAGE('***Error fitting curve',' ')
           IND = 0
           RETURN
      ENDIF
      DS =  (B1*D11 - B2*D12 + B3*D13)/Det
      DL = (-B1*D12 + B2*D22 - B3*D23)/Det
      DT =  (B1*D13 - B2*D23 + B3*D33)/Det
      IND = 1
      IF (IMODE.EQ.0) RETURN
C
C          The rest of the routine measures "goodness of fit"
      CALL PRNT(4,1,N,' Number of points used in fit=.')
      IF (N.LE.3) THEN
           CALL XVMESSAGE(
     &      '***Warning: Insufficient number of points used in fit',
     &	    ' ')
           RETURN
      ENDIF
C          Compute RMS and Covariance
      CHISQ = DP + DS**2*A11 + DL**2*A22 + DT**2*A33
     &           + 2*DS*DL*A12 + 2*DS*DT*A13 + 2*DL*DT*A23
     &           - 2*DS*B1     - 2*DL*B2     - 2*DT*B3
      CHISQ = CHISQ/DFLOAT(N-3)/0.5  !Using measurement error of 0.5 in L & S
      CALL PRNT(8,1,CHISQ,' CHISQ3=.')
      CALL COVMT3(A11,A12,A13,A22,A23,A33,2.*CHISQ)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE COVMT3(A11,A12,A13,A22,A23,A33,VAR)  !Fixed by VRH 5/23/89
      IMPLICIT NONE
      REAL*8 A11,A12,A13,A22,A23,A33,VAR
      REAL*8 CA11,CA12,CA13,CA22,CA23,CA33,DETA
      CHARACTER*80 MSG
C
      DETA = A11*(A22*A33-A23*A23) - A12*(A12*A33-A23*A13)
     *     + A13*(A12*A23-A22*A13)
      IF (DETA.LE.0.D0) THEN
        CALL XVMESSAGE('***Warning: Covariance matrix is singular',
     1   ' ')
        RETURN
      ENDIF
C
      CA11 = (A22*A33 - A23*A23)*VAR/DETA
      CA12 = (A23*A13 - A12*A33)*VAR/DETA
      CA13 = (A12*A23 - A22*A13)*VAR/DETA
      CA22 = (A11*A33 - A13*A13)*VAR/DETA
      CA23 = (A12*A13 - A11*A23)*VAR/DETA
      CA33 = (A11*A22 - A12*A12)*VAR/DETA
C
      CALL XVMESSAGE('Covariant Matrix: S, L (pixels), N (radians)',' ')
      WRITE(MSG,110) CA11,CA12,CA13
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CA12,CA22,CA23
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,110) CA13,CA23,CA33
      CALL XVMESSAGE(MSG,' ')
  110 FORMAT('   ',3F20.10)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given computed star coordinates CPTS and the actual coordinates APTS,
C determine displacements (DL,DS) which register the two.
C
      SUBROUTINE FIT2(APTS,CPTS,NPTS,DL,DS)
      REAL*4 APTS(2,NPTS),CPTS(2,NPTS)
      REAL*8 DL,DS

      DL = 0.D0
      DS = 0.D0

      DO 50 I=1,NPTS
      DL = DL + APTS(1,I) - CPTS(1,I)
      DS = DS + APTS(2,I) - CPTS(2,I)
   50 CONTINUE

      DL = DL/NPTS
      DS = DS/NPTS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the computed star coordinates CPTS and their actual line-sample
C coordinates APTS compute the (line,sample) and north angle displacements
C (DL,DS) and DT to register the two sets.
C RETURN1 on error.
C
      SUBROUTINE FIT3(APTS,CPTS,NPTS,OAL,OAS,dl,ds,dt,*)
      IMPLICIT REAL*8 (A-H,O-Z)		! VRH added for OAL,... Real*8 6/8/89
      REAL*4 APTS(2,NPTS),CPTS(2,NPTS)

      A13 = 0.D0
      A23 = 0.D0
      A33 = 0.D0
      B1 = 0.D0
      B2 = 0.D0
      B3 = 0.D0

      DO 50 I=1,NPTS
      DL = APTS(1,I) - CPTS(1,I)
      DS = APTS(2,I) - CPTS(2,I)
      V = OAL - APTS(1,I)
      U = APTS(2,I) - OAS
      A13 = A13 + U
      A23 = A23 + V
      A33 = A33 + U**2 + V**2
      B1 = B1 + DL
      B2 = B2 + DS
      B3 = B3 + U*DL + V*DS
   50 CONTINUE

      N = NPTS
      Det = N*(N*A33-A13**2-A23**2)
      IF (Det.EQ.0.0) THEN
          CALL XVMESSAGE('***Fit3 error.',' ')
          RETURN1
      ENDIF

      DL = (N*(B1*A33-B3*A13) + A23*(B2*A13-B1*A23))/Det
      DS = (N*(B2*A33-B3*A23) + A13*(B1*A23-B2*A13))/Det
      DT = (N*(N*B3-B1*A13-B2*A23))/Det
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given a displacement of the optical axis of (DL,DS) in the image plane,
C compute the resulting changes in ANGLA and ANGLB.
C
C Inputs: DL,DS,ANGLN,ZSCALE (where DL and DS are in object-space)
C Updated: ANGLA,ANGLB
C
      SUBROUTINE MOVE1(DL,DS,ANGLN,ANGLA,ANGLB,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      SCALE = DSQRT(ZSCALE**2+DL**2+DS**2)
      DUX = DS/SCALE
      DUY = DL/SCALE
      DA = DUX*DCOS(ANGLN) + DUY*DSIN(ANGLN)
      DB = (DUX*DSIN(ANGLN) - DUY*DCOS(ANGLN))/DSIN(ANGLA)
      ANGLA = ANGLA + DA
      ANGLB = ANGLB + DB
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given a displacement of the optical axis of (DL,DS) in the image plane,
C computes the resulting changes in ANGLA and ANGLB.  This version uses
C the old farenc algorithm due to Ingersoll, circa 1980.
C
C Inputs: DL,DS,ANGLN
C Updated: ANGLA,ANGLB
C 95-11-1 -lwk- ANGLA,ANGLB,ANGLN,PSC3,SCLAT,SCLON are in common /CMAP/, 
c		removed from argument list
C
C Note: CMAP must be in planet coordinate system
C
      SUBROUTINE MOVE2(DL,DS,*)
      IMPLICIT REAL*8 (A-H,O-Z)

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

      REAL*8 OMp(3,3),RS(3)

C            Get current planet center....
      CALL PLAINV(IND,SCLAT,SCLON,SCLINE,SCSAMP,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.EQ.0) THEN
            CALL XVMESSAGE('***Err calculating planet center',' ')
            RETURN1
      ENDIF
C             Update planet center...
      SCLINE = SCLINE + DL
      SCSAMP = SCSAMP + DS
C            Calculate OMp transpose...
      SCLA = SCLAT*RTD
      SCLO = (2.D0*PI-SCLON)*RTD
      ANGN = ANGLN*RTD + 90.D0	!IPL north angle
      CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,PSCALE,FL,SCLO,SCLA,ANGN,
     &       RSC,OMp,RS)
C            Calculate C-matrix...
      DO 20 J=1,3
      DO 20 I=1,3
   20 CM(I,J) = ME(I,1)*OMp(J,1)+ME(I,2)*OMp(J,2)+ME(I,3)*OMp(J,3)
C            Compute angles N, A, and B...
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Update C-matrix and all OM-matrices
C
      SUBROUTINE UPDATENAV
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

      real*4 scl4,scs4
      REAL*8 CM0(3,3)
      CHARACTER*80 MSG
  112 FORMAT('Object Space PC (LINE,SAMP)=(',F10.2,',',F10.2,')  ANGLN='
     &,F8.3)
  114 FORMAT(' Image Space PC (LINE,SAMP)=(',F10.2,',',F10.2,')')

      CALL CMATRIX(ME,ANGLN,ANGLA,ANGLB,SCLON,CM0)  !Compute C-matrix
C           Update the pointing for the planet...
      CALL GETNAV
      CALL MVE(7,18,CM0,CM,1,1)
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
      CALL PUTNAV
      CALL PLAINV(ind,SCLAT,SCLON,scline,scsamp,      !Compute planet center
     &		OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      WRITE(MSG,112,ERR=10) SCLINE,SCSAMP,ANGLN*RTD   !Print object-space PC
   10 CALL XVMESSAGE(MSG,' ')
      IF (ITYPE.EQ.7) THEN
         CALL CONVISOS(PROJECT,ICAM,scl4,scs4,SNGL(SCLINE),
     &   SNGL(SCSAMP),0,CONV,NPH,NPV,ind)
         SCLINE=SCL4
         SCSAMP=SCS4
         WRITE(MSG,114,ERR=12) SCLINE,SCSAMP	      !Print image-space PC
   12    CALL XVMESSAGE(MSG,' ')
      ENDIF
C          Now update pointing for all the rings...
      DO IRING=1,NRINGS
          CALL GETRING(IRING)
          CALL GETANGLES(CM0,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
          CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
          CALL PUTRING(IRING)
      ENDDO

      CALL GETNAV	!Restore the planet's reference frame
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Update each of the ring geometries as a result of changes to PSC, RSC,
C PSUN,RSUN,RLORA, or CM.
C
      SUBROUTINE UPDTRING
      IMPLICIT REAL*8 (A-H,O-Z)
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

      CALL PUTNAV			! First save the planet

      DO IRING=1,NRINGS
          CALL GETRING(IRING)
          CALL FROMEME(ME,PSC,SCLAT,SCLON)	       ! Compute SCLAT,SCLON
          CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)     ! Compute PSC3
          CALL FROMEME(ME,PSUN,SUNLAT,SUNLON)	       ! Compute SUNLAT,SUNLON
          CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3) ! Compute PSUN3
          CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
          CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM
          CALL PUTRING(IRING)
      ENDDO

      CALL GETNAV			! Restore the planet
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routines to save and retrieve planet and ring navigation data..
C Navigation data is saved in three buffers: (1) ORIG, (2) LAST, and
C (3) CURR.  ORIG contains the original data retrieved from the SEDR.
C The version of the camera pointing contained in ORIG is controlled
C by the SEDRSRC parameter.  LAST contains
C the last saved version of the data.  CURR contains the current version
C of the data.   Each of these three save areas contain all the navigation
C data for the planet and each of the rings.  The buffer CMAP contains either
C the planet geometry, or one of the ring geometries, and is the buffer
C which all the navigation routines use.
C
C  GETNAV moves the planet geometry from CURR to CMAP (inverse is PUTNAV)
C  GETRING moves a ring geometry from RSAV to CMAP (inverse is PUTRING)
C  GETLAST moves LAST to CURR (inverse is SAVELAST).  GETNAV is then called.
C  GETSEDR moves ORIG to CURR (inverse is SAVESEDR).  GETNAV is then called.
C
      SUBROUTINE GETNAV
      IMPLICIT REAL*8 (A-H,O-Z)
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

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2

      COMMON/CURR/CUR(71),PCCUR(13)
      COMMON/LAST/LAS(71),PCLAS(13)
      COMMON/ORIG/ORG(71),PCORG(13)
      COMMON/RSAV/RSAVE(38,15)
      REAL*8 LAS

      IRING = 1
      CALL MVE(8,71,CUR,FL,1,1)		!Get planet geometry
      CALL MVE(8,13,PCCUR,AI2,1,1)
      RETURN
C
      ENTRY PUTNAV
      CALL MVE(8,71,FL,CUR,1,1)		!Put planet geometry
      CALL MVE(8,13,AI2,PCCUR,1,1)
      RETURN

      ENTRY GETRING(JRING)
      CALL MVE(8,38,RSAVE(1,JRING),ME,1,1)	!Get orbital data for ring
      RETURN

      ENTRY PUTRING(JRING)
      CALL MVE(8,38,ME,RSAVE(1,JRING),1,1)	!Put orbital data for ring
      RETURN

      ENTRY GETSEDR
      IRING = 1
      CALL MVE(8,84,ORG,CUR,1,1)	!Get nominal planet geometry
      CALL MVE(8,71,CUR,FL,1,1)		!Get planet geometry, CM etc.
      CALL MVE(8,13,PCCUR,AI2,1,1)
      CALL ERING0(PLANET_ID,T,T0,1)	!Navigate rings 7/27/89 ",1" added
      CALL MVE(8,71,CUR,FL,1,1)		!Restore planet geometry 
      CALL MVE(8,13,PCCUR,AI2,1,1)
      RETURN

      ENTRY SAVESEDR
      CALL MVE(8,84,CUR,ORG,1,1)	!Save nominal planet geometry
      RETURN

      ENTRY GETLAST
      IRING = 1
      CALL MVE(8,84,LAS,CUR,1,1)	!Get last planet geometry
      CALL MVE(8,71,CUR,FL,1,1)		!Get planet geometry, CM etc.
      CALL MVE(8,13,PCCUR,AI2,1,1)
      CALL ERING0(PLANET_ID,T,T0,1)	!Navigate rings 7/27/89 ",1" added
      CALL MVE(8,71,CUR,FL,1,1)		!Restore planet geometry
      CALL MVE(8,13,PCCUR,AI2,1,1)
      RETURN

      ENTRY SAVELAST
      CALL MVE(8,84,CUR,LAS,1,1)	!Save last planet geometry
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create planet.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transforms from (line,sample) to (latitude,longitude) for a
C perspective projection camera system.  An oblate spheroid model of the
C planet (RE,RP) is used.  All angles are in radians.  Planetocentric
C latitudes and east-longitudes are used.
C
C Inputs:
C     RLINE,RSAMP   :  input image coordinates
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)*DCOS(SCLON-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)*DSIN(SCLON-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     A2B2,B2C2,etc :  constants derived from planet radii & s/c vector
C     RLORA         :  longitude of planet's major equatorial radius
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :	IND=1 if resulting (RLAT,RLON) are valid
C		    :	IND=0 if point is off the planet
C     RLAT,RLON     :  computed planet coordinates.
C
      SUBROUTINE PLANET(IND,RLINE,RSAMP,RLAT,RLON,OM,PSC3,RLORA,
     &                OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 OM(3,3),PSC3(3)
      REAL*8 PI/3.141592653589793D0/
C
C         Vector from spacecraft to point on planet in camera coordinate system
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = ZSCALE
C         Convert vector to planet coordinate system (x3,y3,z3)
      xc = OM(1,1)*x0 + OM(1,2)*y0 + OM(1,3)*z0
      yc = OM(2,1)*x0 + OM(2,2)*y0 + OM(2,3)*z0
      zc = OM(3,1)*x0 + OM(3,2)*y0 + OM(3,3)*z0
C          Find where vector intersects planet
      A = AI2*xc**2 + BI2*yc**2 + CI2*zc**2
      B = AI2XS*xc  + BI2YS*yc  + CI2ZS*zc
      D = B*B - A*CTERM
      IF(D.LT.0.) THEN
          IND = 0		!Point off the planet
          RETURN
      ENDIF
      r = (-B-DSQRT(D))/A	!Choose smaller root for point in front
C
      x3 = r*xc + PSC3(1)
      y3 = r*yc + PSC3(2)
      z3 = r*zc + PSC3(3)
      D = DSQRT(x3**2+y3**2)
      RLAT = DATAN2(z3,D)
      RLON = DMOD(DATAN2(y3,x3)+RLORA+2.D0*PI,2.D0*PI)
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transforms from (latitude,longitude) to (line,sample) in a
C perspective projection camera system.  An oblate spheroid model of the
C planet (RE,RP) is used.  All angles are in radians.  Planetocentric
C latitudes and east-longitudes are used.
C
C Inputs:
C     RLAT,RLON     :  input planet coordinates.
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)DCOS(SCLAT-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)DSIN(SCLAT-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     RLORA         :  longitude of planet's major equatorial radius
C     ABC,A2B2,etc  :  constants derived from planet radii & s/c vector
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :  IND=1 if resulting (RLINE,RSAMP) are valid
C                   :  IND=0 if point is behind the planet
C     RLINE,RSAMP   :  computed image coordinates
C
      SUBROUTINE PLAINV(IND,RLAT,RLON,RLINE,RSAMP,OM,PSC3,RLORA,
     &                    OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
      REAL*8 OM(3,3),PSC3(3)
C
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
      CLON = DCOS(RLON-RLORA)
      SLON = DSIN(RLON-RLORA)
C     ...Compute planetocentric radius
      R = 1.D0/DSQRT(AI2*(CLAT*CLON)**2+BI2*(CLAT*SLON)**2+CI2*SLAT**2)
C     ...Convert (lat,lon) point on planet to rectangular coordinates
      xp = R*CLAT*CLON
      yp = R*CLAT*SLON
      zp = R*SLAT
C          Compute vector from camera to point on planet
      xc =  xp - PSC3(1)
      yc =  yp - PSC3(2)
      zc =  zp - PSC3(3)
C          Vance Haemmerle's back-of-planet test
      DOTPROD = xc*xp + yc*yp + zc*zp
      IF (DOTPROD.GT.0) THEN	!If the angle between the two vectors
           IND = 0		!is less than 90 degrees, then point
           RETURN		!is behind the planet.
      ENDIF
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transform from camera coordinates (line,sample) to ring plane
C coordinates (radius,longitude) for a perspective projection camera system.  
C All angles are in radians.  Planetocentric latitudes and east-longitudes
C are used.
C
C Inputs:
C     RLINE,RSAMP   :  input image coordinates
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)DCOS(SCLON-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)DSIN(SCLON-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     SCLON         :  spacecraft longitude
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :  return indicator.  Upon return,
C		    :	    IND=1 if resulting (RADIUS,RLON) are valid
C                   : 	    IND=0 if the ring plane is not visible
C     RADIUS,RLON   :  computed ring coordinates.
C
      SUBROUTINE RING(IND,RLINE,RSAMP,RADIUS,RLON,OM,PSC3,RLORA,
     &       OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3)
      REAL*8 PI/3.141592653589793D0/
C
C         Vector from spacecraft to point on ring in camera coordinate system
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = ZSCALE
C         Convert vector to planet centered coordinates
      xc = OM(1,1)*x0 + OM(1,2)*y0 + OM(1,3)*z0
      yc = OM(2,1)*x0 + OM(2,2)*y0 + OM(2,3)*z0
      zc = OM(3,1)*x0 + OM(3,2)*y0 + OM(3,3)*z0
C
      S = -PSC3(3)/zc
      IF (S.LT.0.) THEN
	IND = 0
        RETURN
      ENDIF
C
      X = S*xc + PSC3(1)
      Y = S*yc + PSC3(2)
      RADIUS = DSQRT(X**2+Y**2)
      RLON = DMOD(DATAN2(Y,X)+RLORA+2.D0*PI,2.D0*PI)
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Transforms from ring plane coordinates (radius,longitude) to
C camera coordinates (line,sample) in a perspective projection camera system.
C All angles are in radians.  Planetocentric latitudes and east-longitudes
C are used.
C
C Inputs:
C     RADIUS,RLON   :  input ring coordinates.
C     OM	    :  camera to planet transformation matrix
C     PSC3          :  spacecraft vector in (x3,y3,z3) coordinates, where
C		    :     PSC3(1) = RSC*DCOS(SCLAT)DCOS(SCLON-RLORA)
C		    :     PSC3(2) = RSC*DCOS(SCLAT)DSIN(SCLON-RLORA)
C		    :     PSC3(3) = RSC*DSIN(SCLAT)
C     SCLON         :  spacecraft longitude.
C     OAL,OAS       :  image coordinates of optical axis intercept point
C     ZSCALE        :  camera constant = FL*PSCALE
C
C Outputs:
C     IND           :  IND=1 always.  Output (line,sample) are always valid.
C     RLINE,RSAMP   :  computed image coordinates
C
      SUBROUTINE RINV(IND,RADIUS,RLON,RLINE,RSAMP,
     &      OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OM(3,3),PSC3(3)
C          Compute vector from camera to point on ring
      xc =  RADIUS*DCOS(RLON-RLORA) - PSC3(1)
      yc =  RADIUS*DSIN(RLON-RLORA) - PSC3(2)
      zc =                          - PSC3(3)
C          Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(2,1)*yc + OM(3,1)*zc
      y0 = OM(1,2)*xc + OM(2,2)*yc + OM(3,2)*zc
      z0 = OM(1,3)*xc + OM(2,3)*yc + OM(3,3)*zc
C          Scale vector into pixels
      S = ZSCALE/z0
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the line-sample image coordinates of a star, calculate
C its right-ascension and declination.
C
C Outputs: RA,DEC
C
      SUBROUTINE STAR(RLINE,RSAMP,ra,dec,CM,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3)
      REAL*8 PI/3.141592653589793D0/

C     ....Compute vector in camera coordinates
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = ZSCALE
C     ....Rotate vector into EME50 coordinate system using C-matrix
      x = CM(1,1)*x0 + CM(1,2)*y0 + CM(1,3)*z0
      y = CM(2,1)*x0 + CM(2,2)*y0 + CM(2,3)*z0
      z = CM(3,1)*x0 + CM(3,2)*y0 + CM(3,3)*z0
C     ....Convert from rectangular to polar coordinates
      D = DSQRT(x**2 + y**2)
      DEC = DATAN2(z,D)
      RA = DMOD(DATAN2(y,x)+2.D0*PI,2.D0*PI)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the right-ascension and declination of a star, calculate
C its line-sample coordinates in the image.
C
C Outputs: RLINE,RSAMP
C
      SUBROUTINE STARINV(RA,DEC,rline,rsamp,CM,OAL,OAS,ZSCALE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CM(3,3)

C     ....Convert RA,DEC to unit vector (x,y,z) in EME50 coordinates.
      CD = DCOS(DEC)
      x = DCOS(RA)*CD
      y = DSIN(RA)*CD
      z = DSIN(DEC)
C     ....Rotate vector into camera coordinates using inverse C-matrix
      x0 = CM(1,1)*x + CM(2,1)*y + CM(3,1)*z
      y0 = CM(1,2)*x + CM(2,2)*y + CM(3,2)*z
      z0 = CM(1,3)*x + CM(2,3)*y + CM(3,3)*z
C     ....Project onto focal-plane and scale vector into pixels
      S = ZSCALE/z0		!ZSCALE=FL*PICSCALE
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert from (line,sample) to (lat,lon) or (radius,lon) 
C
      SUBROUTINE LATLON(IND,RLINE,RSAMP,rlat,rlon)
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
      real*4 rl4,rs4

      IF (ITYPE.EQ.7) THEN  !If image-space, convert to object=space.
	CALL CONVISOS(PROJECT,ICAM,sngl(RLINE),sngl(RSAMP),rl4,rs4,1,
     &   CONV,NPH,NPV,ind)
	rl=rl4
	rs=rs4
      ELSE		    !Else, no conversion necessary.
	RL = RLINE
	RS = RSAMP
      ENDIF

      IF (MODEL.LE.3) THEN	!Convert from (line,samp) to (lat,lon)
          CALL PLANET(IND,RL,RS,rlat,rlon,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
          IF (IND.EQ.0) CALL XVMESSAGE('***Point off planet',' ')
          RETURN
      ENDIF

      IF (MODEL.EQ.4) THEN	!Convert from (line,samp) to (radius,lon)
        CALL RING(IND,RL,RS,radius,rlon,
     &   OM,PSC3,RLORA,OAL,OAS,ZSCALE)
        RLAT = RADIUS
        IF (IND.EQ.0) CALL XVMESSAGE('***Ring plane is not visible',
     &   ' ')
        RETURN
      ENDIF

      IF (MODEL.LE.5) THEN	!Convert from (line,samp) to (RA,DEC)
          CALL STAR(RL,RS,ras,dec,CM,OAL,OAS,ZSCALE)
          RLAT = RAS
          RLON = DEC
          IND = 1
          RETURN
      ENDIF

      CALL PRNT(4,1,MODEL,' ***Invalid target model=.')
      RETURN 
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert from (lat,lon) or (radius,lon) to (line,sample) 
C
      SUBROUTINE LINSAM(IND,RLAT,RLON,rline,rsamp)
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
      real*4 rl4,rs4

      IF (MODEL.LT.3.OR.MODEL.GT.5) THEN
          CALL PRNT(4,1,MODEL,' ***Invalid target model=.')
          IND = 0
          RETURN
      ENDIF

      IF (MODEL.LE.3) THEN	!Convert from (lat,lon) to (line,samp)
          CALL PLAINV(IND,RLAT,RLON,rline,rsamp,
     &           OM,PSC3,RLORA,OAL,OAS,ZSCALE)
          IF (IND.EQ.0) THEN
             CALL XVMESSAGE('***Point behind the planet',' ')
             RETURN
          ENDIF
      ENDIF

      IF (MODEL.EQ.4) THEN	!Convert from (radius,lon) to (line,samp)
          RADIUS = RLAT
          CALL RINV(IND,RADIUS,RLON,rline,rsamp,
     &            OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      ENDIF

      IF (MODEL.EQ.5) THEN	!Convert from (RA,DEC) to (line,samp)
          RAS = RLAT
          DEC = RLON
          CALL STARINV(RAS,DEC,rline,rsamp,CM,OAL,OAS,ZSCALE)
      ENDIF

      IF (ITYPE.EQ.7) then
	CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(RLINE),sngl(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	rline=rl4
	rsamp=rs4
      endif
      IND = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to calculate the phase angle at a particular point (radius,longitude)
C for the ring and (latitude,longitude) for the planet or satellite
C
      SUBROUTINE PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 add arg
      IMPLICIT REAL*8 (A-H,O-Z)
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

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,C0,C1,C2
C
C  All vectors are planet centered co-ordinates.  Convert (radius,longitude) 
C  or (latitude,longitude) of intercept point to X,Y,Z
      IF (MODEL.EQ.3) THEN
        CLAT = DCOS(RADIUS)			!RLAT = RADIUS
        SLAT = DSIN(RADIUS)
        CLON = DCOS(RLON-RLORA) ! Fixed by VRH 5/26/89 (RLORA replaces
        SLON = DSIN(RLON-RLORA) ! Fixed by VRH 5/26/89      SCLON)
CCC        EPSLN = (RA/RC)**2
CCC        R = RE/DSQRT((1-EPSLN)*CLAT**2+EPSLN)	!geocentric radius
        R = 1.D0/DSQRT(AI2*(CLAT*CLON)**2	!Planetocentric radius
     &		 + BI2*(CLAT*SLON)**2 + CI2*SLAT**2)
        XP3 = R*CLAT*CLON
        YP3 = R*CLAT*SLON
        ZP3 = R*SLAT
      ELSE IF (MODEL.EQ.4) THEN
        XP3 = RADIUS*DCOS(RLON-RLORA) !Fixed VRH 5/26/89  (RLORA replaces
        YP3 = RADIUS*DSIN(RLON-RLORA) !Fixed VRH 5/26/89       SCLON)
        ZP3 = 0.D0
      ENDIF
C  Get components of Spacecraft from intercept point, vector DX,DY,DZ
      DX = PSC3(1) - XP3
      DY = PSC3(2) - YP3
      DZ = PSC3(3) - ZP3
      RANGE = DSQRT(DX*DX + DY*DY + DZ*DZ)
c
c VRH added code to calculate particular info (one time deal) 3/94
c      sin_delta = DZ/RANGE
c      delta = dasind(sin_delta)
c      print*,'S/C is',delta,' deg above ring plane as seen from pt'
c
C  Get components of Sun from intercept point, vector DXSUN,DYSUN,DZSUN
      DXSUN = PSUN3(1) - XP3
      DYSUN = PSUN3(2) - YP3
      DZSUN = PSUN3(3) - ZP3
      SUNRAD = DSQRT(DXSUN*DXSUN + DYSUN*DYSUN + DZSUN*DZSUN)
C  Calculate Cos(phase)
      COSPHA = (DX*DXSUN + DY*DYSUN + DZ*DZSUN)/RANGE/SUNRAD
      PHA = DACOS(COSPHA)
C  Calculate Image scale (km/pixel)  ! VRH 3/1/94 add
      SCALE = RANGE/ZSCALE
C  Calculate Radial image scale (km/pixel) !VRH 3/1/94 add
      IF(MODEL.EQ.4) THEN
        COSPHI = -(DX*XP3 + DY*YP3)/RADIUS/RANGE
        PHI = DACOS(COSPHI)
        RSCALE = SCALE/DSIN(PHI)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to convert input latitude (RLAT) from planetodetic to planetocentric
C if IGEO=1.  If IGEO=0, the latitude is returned unchanged.
C
      FUNCTION GEOCEN(RLAT,RLON)
      IMPLICIT REAL*8 (A-H,O-Z)
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

      IF (DABS(RLAT).GE.PI/2.D0) THEN
           GEOCEN = DSIGN(PI/2.D0,RLAT)
           RETURN
      ENDIF

      IF (IGEO.NE.1) THEN
           GEOCEN = RLAT
      ELSE
           EPS = (RC/(RA*RB))**2*
     &              DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON-RLORA)**2)
           GEOCEN = DATAN(DTAN(RLAT)*EPS)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to convert input latitude (RLAT) from planetocentric to
C planetodetic if IGEO=1.  If IGEO=0, the latitude is returned
C unchanged. ! VRH changed description from GEOFLAG to IGEO 9/6/91
C
      FUNCTION GEODET(RLAT,RLON)
      IMPLICIT REAL*8 (A-H,O-Z)
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

      IF (DABS(RLAT).GE.PI/2.D0) THEN
         GEODET = DSIGN(PI/2.D0,RLAT)
         RETURN
      ENDIF

      IF (IGEO.NE.1) THEN
         GEODET = RLAT
      ELSE
         EPS = ((RA*RB)/RC)**2/
     &              DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON-RLORA)**2)
         GEODET = DATAN(DTAN(RLAT)*EPS)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert (line,sample) to camera unit vector
C
      SUBROUTINE IMG2OBJ(RLINE,RSAMP,vout)
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

      DOUBLEPRECISION VIN(3),VOUT(3),VLCL(3)
      REAL*4 RL,RS

      IF (ITYPE.EQ.7) THEN  !If image space, convert to object space.
         CALL CONVISOS(PROJECT,ICAM,SNGL(RLINE),SNGL(RSAMP),rl,rs,1,
     &                CONV,NPH,NPV,ind)
      ELSE		    !Else, no conversion necessary.
         RL = RLINE
         RS = RSAMP
      ENDIF

      VOUT(1) = RS - OAS
      VOUT(2) = RL - OAL
      VOUT(3) = ZSCALE
      CALL VHAT(VOUT,vout)	!Make it a unit vector
      RETURN

C********************************************************************
C Convert camera vector to (line,sample)
C
      ENTRY OBJ2IMG(VIN,rline,rsamp)

C     ....Test for vector pointing along or behind image plane
      IF (VIN(3).LE.0D0) THEN
        RLINE = -999D0
        RSAMP = RLINE
        RETURN
      ENDIF
C
C     ....Convert to vector with Z = ZSCALE
      CALL VSCL(ZSCALE/VIN(3),VIN,VLCL)
      RSAMP = VLCL(1) + OAS
      RLINE = VLCL(2) + OAL

      IF (ITYPE.EQ.7) THEN  !Convert object-space to image-space,
         CALL CONVISOS(PROJECT,ICAM,rl,rs,SNGL(RLINE),SNGL(RSAMP),0,
     &                CONV,NPH,NPV,ind)
	RLINE=RL
	RSAMP=RS
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert (line,sample) <=> object l,s <=> camera unit vec
C rotate one vector around another
C
      SUBROUTINE ROTVEC(ROT,VECIN,AXIVEC,VECOUT)
      IMPLICIT NONE
      DOUBLEPRECISION ROT,VECIN(3),AXIVEC(3),VECOUT(3)
      DOUBLEPRECISION QUART(0:3),QMTX(3,3),OLDROT,SINROT2,ROT2
      DATA QUART(0) / 1D0 /
      DATA OLDROT / 0D0 /
      DATA SINROT2 / 0D0 /
      SAVE

      IF (ROT .EQ. -OLDROT) THEN
         SINROT2 = -SINROT2
         OLDROT = ROT
      ELSE IF (ROT .NE. OLDROT) THEN
         ROT2 = ROT / 2D0
         QUART(0) = DCOS(ROT2)
         SINROT2 = DSIN(ROT2)
         OLDROT = ROT
      ENDIF
      IF (QUART(0) .EQ. 1D0) THEN
        CALL VEQU(VECIN,VECOUT)
      ELSE
        CALL VSCL(SINROT2,AXIVEC,QUART(1))
        CALL Q2M(QUART,QMTX)
        CALL MXV(QMTX,VECIN,VECOUT)
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CBTC
CBTC END - rotvec
CBTC START + & - rotate vector around axis,convert to image space,update
CBTC       array of image space line,samp,count how many are within
CBTC       image space limits
CBTC SUBROUTINE ROTVECS2IMG
CBTC Inputs:
CBTC   VECIN(3)    DP   Vector to rotate
CBTC   AXIVEC(3)   DP   Axis of rotation
CBTC   ROT         DP   Rotation,radians
CBTC   SL,SS,EL,ES DP   Image space limits - determines which results cause
CBTC                      NPTS to be updated
CBTC Outputs:
CBTC   NPTS        INT  Counter to be incremented for each line,samp 
CBTC                      within limits
CBTC   PTS(2,2)    R4   Array of line,samp pairs for positive & negative rot
CBTC
      subroutine rotvecs2img(vecin,axivec,rot,sl,ss,el,es
     &                      ,npts,pts)
      implicitnone
      doubleprecision vecin(3),axivec(3),rot,sl,ss,el,es
      real*4 pts(2,2)
      integer npts
CBTC
      doubleprecision vec1(3),rline,rsamp
      integer i
CBTC********************************************************************
CBTC rotate negative
      call rotvec(-rot,vecin,axivec,vec1)
      call obj2img(vec1,rline,rsamp)
CBTC
CBTC copy rline,rsamp to pts(,1) if both are not outside limits & update npts,
CBTC otherwise copy to pts(,2)
      if (.not. (rline.lt.sl .or. rline.gt.el 
     &        .or. rsamp.lt.ss .or. rsamp.gt.es)) then
        pts(1,1) = rline
        pts(2,1) = rsamp
        i = 2
        npts = npts + 1
      else
        pts(1,2) = rline
        pts(2,2) = rsamp
        i = 1
      endif
CBTC
CBTC rotate positive 
      call rotvec(rot,vecin,axivec,vec1)
      call obj2img(vec1,rline,rsamp)
CBTC
CBTC copy rline,rsamp to pts(,i) & update npts if necessary
      pts(1,i) = rline
      pts(2,i) = rsamp
      if (.not. (rline.lt.sl .or. rline.gt.el 
     &        .or. rsamp.lt.ss .or. rsamp.gt.es)) npts = npts + 1
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create editnav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Routine to edit navigation data.  All changes are first made in the planet's
C coordinate system.  Then each ring coordinate system is updated
C as a result of possible changes to PSC,RSC,PSUN,RSUN,RLORA, or CM.
C
      SUBROUTINE EDITNAV(IMG,PIC,HPIC,NL,NS)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/SEDR/SEDR(200),LBUF(100),ISYSTEM
      REAL*4 SEDR
      INTEGER LBUF

      COMMON/CPAR/PAR(20),PAR2(20)
      REAL*4 PAR,PAR2
      INTEGER*4 IPAR(20)
      EQUIVALENCE (PAR,IPAR)

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID
      integer*2 hcam

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

      REAL*4 R,oal4,oas4,scl4,scs4,naline,nasamp,waline,wasamp,scale
      LOGICAL PARMTST,RPARMTST,XVIPTST
      CHARACTER*4 ckname
      CHARACTER*12 tname
      CHARACTER*80 MSG
  100 FORMAT('O.S. NA CENTER=(',F10.2,',',F10.2,')')

      CALL GETNAV

   20 CALL XVINTRACT('EDIT','EDIT')
      IF (XVIPTST('HELP')) CALL XVINTRACT('EDIT',' ')

      IF (XVIPTST('EXIT')) THEN
           CALL PUTNAV			! Save planet geometry
           RETURN
      ENDIF

      IF (XVIPTST('GEOCEN')) IGEO=0
      IF (XVIPTST('GEODET')) IGEO=1

      IF (RPARMTST('RA',R,I)) THEN	
         RA = R				! Change POLAR RADIUS
         CALL GETPC(PSC3,RA,RB,RC)	! Recalculate planet constants
      ENDIF

      IF (RPARMTST('RB',R,I)) THEN	
         RB = R				! Change POLAR RADIUS
         CALL GETPC(PSC3,RA,RB,RC)	! Recalculate planet constants
      ENDIF

      IF (RPARMTST('RC',R,I)) THEN	
         RC = R				! Change POLAR RADIUS
         CALL GETPC(PSC3,RA,RB,RC)	! Recalculate planet constants
      ENDIF

      IF (RPARMTST('LORA',R,I)) THEN	
         RLORA = R*DTR		! Change longitude of RA
         CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix
         CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)     ! Update PSC3
         CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3) ! Update PSUN3
         CALL GETPC(PSC3,RA,RB,RC)	!Update planet constants
         CALL UPDTRING			!Update ring coordinate systems
      ENDIF

      IF (RPARMTST('FL',R,I)) THEN		! Change FOCAL length
           FL = R
           ZSCALE = PSCALE*FL
      ENDIF

      IF (RPARMTST('OAXIS',PAR,I)) THEN		! Change optical axis intercept
        OAL = PAR(1)
        OAS = PAR(2)
        IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,oal4,oas4,
     &	    SNGL(OAL),SNGL(OAS),0,CONV,NPH,NPV,ind)
	OAL_IS = OAL4
	OAS_IS = OAS4
      ENDIF

      IF (RPARMTST('SC',R,I)) THEN
         PSCALE=R				! Change picture scale
         ZSCALE=PSCALE*FL
      ENDIF

      IF (RPARMTST('RANGE',R,I)) THEN
         RSC = R		 		! Change spacecraft range
         CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)     ! Update PSC3
         CALL GETPC(PSC3,RA,RB,RC)
         CALL UPDTRING				!Update ring coordinate systems
      ENDIF

      IF (RPARMTST('SSP',PAR,I)) THEN		! Change subspacecraft point
          SCLON = PAR(2)*DTR
          SCLAT = GEOCEN(PAR(1)*DTR,SCLON)
          CALL TOEME(ME,SCLAT,SCLON,PSC)		! Update PSC
          CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)      ! Update PSC3
          CALL GETPC(PSC3,RA,RB,RC)
          CALL FARENC(SCLAT,SCLON,RLORA,SCLINE,SCSAMP,PSCALE,FL,
     &             OAL,OAS,PSC,ME,cm,om,angln,angla,anglb)
          CALL UPDTRING			!Update ring coordinate systems
      ENDIF

      IF (RPARMTST('PC',PAR,I)) THEN		!Change planet center
            SCLINE = PAR(1)
            SCSAMP = PAR(2)			!Update OM and CM
            CALL FARENC(SCLAT,SCLON,RLORA,SCLINE,SCSAMP,PSCALE,FL,
     &             OAL,OAS,PSC,ME,cm,om,angln,angla,anglb)
            CALL UPDTRING			!Update ring coordinate systems
            GOTO 20
      ENDIF

      IF (RPARMTST('ISPC',PAR,I)) THEN		!Change I.S. planet center
            IF (ITYPE.EQ.8) THEN
               CALL XVMESSAGE('***ISPC invalid for object-space frames',
     &		' ')
               GOTO 20
            ENDIF
            SCLINE = PAR(1)
            SCSAMP = PAR(2)
            CALL CONVISOS(PROJECT,ICAM,PAR(1),PAR(2),scl4,scs4,1,
     &		CONV,NPH,NPV,ind)
	    scline=scl4
	    scsamp=scs4
            CALL FARENC(SCLAT,SCLON,RLORA,SCLINE,SCSAMP,PSCALE,FL,
     &             OAL,OAS,PSC,ME,cm,om,angln,angla,anglb)
            CALL UPDTRING			!Update ring coordinate systems
            GOTO 20
      ENDIF

      IF (RPARMTST('WAPC',PAR,I)) THEN	! Change WA center to NA center
	IF (IPROJ.NE.4) THEN
	  CALL XVMESSAGE('***WAPC only valid for Voyager',' ')
	  GOTO 20
	ENDIF
	WALINE = PAR(1)
	WASAMP = PAR(2)
	HCAM = ICAM
	CALL MWATNA(HCAM,WALINE,WASAMP,scl4,scs4,*999)
	SCLINE=SCL4
	SCSAMP=SCS4
	CALL FARENC(SCLAT,SCLON,RLORA,SCLINE,SCSAMP,PSCALE,FL,
     &             OAL,OAS,PSC,ME,cm,om,angln,angla,anglb)
	WRITE (MSG,100,ERR=30) SCLINE,SCSAMP
   30	CALL XVMESSAGE(MSG,' ')
	CALL UPDTRING			!Update ring coordinate systems
      ENDIF

      IF (RPARMTST('WAISPC',PAR,I)) THEN	! Change WA center to NA center
	IF (ITYPE.EQ.8) THEN
	  CALL XVMESSAGE('***WAISPC invalid for object-space frames',' ')
	  GOTO 20
	ENDIF
	IF (IPROJ.NE.4) THEN
	  CALL XVMESSAGE('***WAISPC only valid for Voyager',' ')
	  GOTO 20
	ENDIF
	CALL CONVISOS(PROJECT,ICAM,PAR(1),PAR(2),scl4,scs4,
     &		1,CONV,NPH,NPV,ind)
	hcam = ICAM
	CALL MWATNA(HCAM,SCL4,SCS4,naline,nasamp,*999)
	SCLINE=NALINE
	SCSAMP=NASAMP
	CALL FARENC(SCLAT,SCLON,RLORA,SCLINE,SCSAMP,PSCALE,FL,
     &             OAL,OAS,PSC,ME,cm,om,angln,angla,anglb)
	WRITE (MSG,100,ERR=40) SCLINE,SCSAMP
   40	CALL XVMESSAGE(MSG,' ')
	CALL UPDTRING			!Update ring coordinate systems
      ENDIF

      IF (RPARMTST('ANGLN',R,I)) THEN		!Change NORTH ANGLE
	ANGLN = R*DTR
	CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix
	CALL CMATRIX(ME,ANGLN,ANGLA,ANGLB,SCLON,CM)
	CALL UPDTRING			!Update ring coordinate systems
      ENDIF

      IF (XVIPTST('ECM')) THEN
	CALL EDITCM
	CALL UPDTRING			!Update ring coordinate systems
	GOTO 20
      ENDIF

      IF (XVIPTST('EME')) THEN
	CALL EDITME
	GOTO 20
      ENDIF

      IF (RPARMTST('SCVECTOR',PAR,I)) THEN
	xx = par(1)+par(2)+par(3)
	if (i.ne.3 .or. xx.le.0.0) then
	  call xvmessage(' ** invalid SCVECTOR, try again ... ***',' ')
	else
	  CALL EDITPSC(PAR)
	  CALL UPDTRING			!Update ring coordinate systems
	endif
	GOTO 20
      ENDIF

      IF (PARMTST('CAMERA',IVAL,I)) THEN		! Change camera S/N
         ICAM = IVAL
         CALL GETCAMCON(PROJECT,ICAM,R,oal4,oas4,scale,ind)
c         CALL VGRCAM(ICAM,FL)
         IF (IND .EQ. 0) THEN
            FL = R
            OAL = oal4
            OAS = oas4
            PSCALE = scale
            ZSCALE = PSCALE*FL
            IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,oal4,oas4,
     &	        SNGL(OAL),SNGL(OAS),0,CONV,NPH,NPV,ind)
	    OAL_IS = OAL4
	    OAS_IS = OAS4
         ENDIF
       ENDIF

      IF (RPARMTST('SOL',PAR,I)) THEN		!Change solar position
         SUNLON = PAR(2)*DTR
         SUNLAT = GEOCEN(PAR(1)*DTR,SUNLON)
         CALL TOEME(ME,SUNLAT,SUNLON,PSUN)		! Compute PSUN 
         CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3)	! Compute PSUN3
         CALL UPDTRING			!Update ring coordinate systems
      ENDIF

      IF (RPARMTST('RLIM',PAR,I)) THEN
         RMIN = PAR(1)
         RMAX = PAR(2)
      ENDIF

      IF (XVIPTST('STATUS')) CALL PNAV
      IF (XVIPTST('SAVE')) CALL SAVELAST
      IF (XVIPTST('RESTORE')) CALL GETLAST
      IF (XVIPTST('GETSEDR')) CALL GETSEDR

c  (parameters CKNAME/CKID are used in subroutine GETSPICE2)
      CALL XVIPARM('CKNAME',ckname,icnt,idef1,0)
      CALL XVIPARM('CKID',ickid,icnt,idef2,0)
      IF (IDEF1.NE.1 .OR.IDEF2.NE.1) THEN
         CALL SPICESUB(IMG,PROJECT,LBUF,nl,ns,sedr,ind)
         IF (IND.NE.0) GOTO 20
         CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
         CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM)
         CALL PUTNAV
         CALL T1950(IDATE,ITIME,RSC,t) ! VRH 7/28/89 - change calling statement
         CALL ERING0(PLANET_ID,T,T0,1)
         CALL GETNAV  ! Restore planet geometry VRH 6/29/89
         CALL SAVESEDR
      ENDIF

      IF (PARMTST('TARGET',tname,I)) THEN	! Change target id
         CALL UPRCASE(tname)
         CALL PBID(tname,target_id,*20)
         PLANET_ID = TARGET_ID
         IF (PLANET_ID.GT.9) PLANET_ID=PLANET_ID/100
         CALL PBDATA(tname,par2,*20)
         RA = PAR2(1)				! Update planet radii
         RB = PAR2(2)
         RC = PAR2(3)
         RLORA = PAR2(4)*DTR			! Update longitude of RA
C                New longitude system (x3,y3,z3)...
         CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix
         CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)     ! Update PSC3
         CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3) ! Update PSUN3
         CALL GETPC(PSC3,RA,RB,RC)
         GOTO 20
      ENDIF

      CALL DISPLAY(PIC,HPIC,NL,NS,IND)
      IF (IND.NE.0) GOTO 20
      IF (MODEL.LE.3) CALL PDISPLAY(PIC,HPIC,NL,NS,IND) !VRH 6/28/89 new
      IF (MODEL.EQ.4) THEN
           CALL PUTNAV				! Save the planet
           CALL RDISPLAY(PIC,HPIC,NL,NS,IND)
           CALL GETNAV				! Restore the planet
      ENDIF
      IF (IND.NE.0) GOTO 20
      GOTO 20

  999 RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to edit the C-matrix
C
      SUBROUTINE EDITCM
      IMPLICIT REAL*8 (A-H,O-Z)
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

      REAL*8 KAPPA

      COMMON/CONST/PI,DTR,RTD
      LOGICAL dPARMTST,XVIPTST,LST
      CHARACTER*30 MSG

      ALPHA = DATAN2(CM(2,3),CM(1,3))*RTD
      DELTA = DASIN(CM(3,3))*RTD
      KAPPA = DATAN2(CM(3,1),CM(3,2))*RTD
      GOTO 30

   20 CALL XVINTRACT('ECM','Enter changes or type EXIT')
      IF (XVIPTST('HELP')) CALL XVINTRACT('ECM',' ')
      LST=dPARMTST('ALPHA',ALPHA,I)
      LST=dPARMTST('DELTA',DELTA,I)
      LST=dPARMTST('KAPPA',KAPPA,I)

   30 CALL XVMESSAGE('Camera orientation angles in degrees:',' ')
      WRITE(MSG,100) ALPHA
  100 FORMAT('   ALPHA=',F12.7)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,101) DELTA
  101 FORMAT('   DELTA=',F12.7)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,102) KAPPA
  102 FORMAT('   KAPPA=',F12.7)
      CALL XVMESSAGE(MSG,' ')
      IF (.NOT.XVIPTST('EXIT')) GOTO 20

C         Rebuild CM and OM matrices...
      CALL BUILDCM(CM,ALPHA,DELTA,KAPPA)
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to edit the ME-matrix
C
      SUBROUTINE EDITME
      IMPLICIT REAL*8 (A-H,O-Z)
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
      LOGICAL dPARMTST,XVIPTST,LST
      CHARACTER*30 MSG

C        Compute current euler angles...
      ALPHA = DATAN2(ME(2,3),ME(1,3))*RTD
      DELTA = DASIN(ME(3,3))*RTD
      OMEGA = DATAN2(ME(3,1),ME(3,2))*RTD
      GOTO 30

   20 CALL XVINTRACT('EME','Enter changes or type EXIT')
      IF (XVIPTST('HELP')) CALL XVINTRACT('EME',' ')
      LST=dPARMTST('ALPHA',ALPHA,I)
      LST=dPARMTST('DELTA',DELTA,I)
      LST=dPARMTST('OMEGA',OMEGA,I)

   30 CALL XVMESSAGE('ME-matrix orientation angles in degrees:',' ')
      WRITE(MSG,100) ALPHA
  100 FORMAT('   ALPHA=',F12.7)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,101) DELTA
  101 FORMAT('   DELTA=',F12.7)
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG,102) OMEGA
  102 FORMAT('   OMEGA=',F12.7)
      CALL XVMESSAGE(MSG,' ')
      IF (.NOT.XVIPTST('EXIT')) GOTO 20

C         Rebuild ME matrix...
      CALL BUILDCM(ME,ALPHA,DELTA,OMEGA)
      CALL FROMEME(ME,PSC,SCLAT,SCLON)		! Compute SCLAT,SCLON
      CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)	! Compute PSC3
      CALL FROMEME(ME,PSUN,SUNLAT,SUNLON)	! Compute SUNLAT,SUNLON
      CALL VECTOR3(RSUN,SUNLAT,SUNLON-RLORA,PSUN3) ! Compute PSUN3
      CALL GETPC(PSC3,RA,RB,RC)			! Compute projection constants
C         Rebuild OM matrices...
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to edit the spacecraft vector...
C
      SUBROUTINE EDITPSC(PAR)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 PAR(3)

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

      CHARACTER*80 MSG

      CALL XVMESSAGE('Spacecraft vector in celestial coordinates (km):',
     & ' ')
      WRITE(MSG,100) PAR
  100 FORMAT(F13.2,2F14.2)
      CALL XVMESSAGE(MSG,' ')
C          Compute spacecraft range...
      SUMSQ = 0.D0
      DO I=1,3
          X = PAR(I)
          SUMSQ = SUMSQ + X**2
      ENDDO
      RSC = DSQRT(SUMSQ)
C          Normalize the vector...
      DO I=1,3
           PSC(I) = PAR(I)/RSC
      ENDDO

      CALL FROMEME(ME,PSC,SCLAT,SCLON)		! Compute SCLAT,SCLON
      CALL VECTOR3(RSC,SCLAT,SCLON-RLORA,PSC3)	! Compute PSC3
      CALL GETPC(PSC3,RA,RB,RC)			! Compute projection constants
C         Rebuild OM matrices...
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Compute OM-matrix
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to compute the C-matrix given as input the three Euler angles
C defining the orientation of the camera.
C
C The 9 elements of the C matrix are stored in order of increasing address
C as
C                  |  1   4   7  |
C                  |  2   5   8  |
C                  |  3   6   9  |
C
      SUBROUTINE BUILDCM(C,ALPHA,DELTA,KAPPA)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 C(9)	!Output C matrix
      REAL*8 ALPHA      !Input RA of instrument boresight (degrees)
      REAL*8 DELTA	!Input Declination of instrument boresight (degrees)
      REAL*8 KAPPA	!Input Image rotation angle (north angle) (degrees)

      PI = 3.141592653589793D0
      DTR = PI/180.D0

      SIN_ALPHA = SIN(ALPHA*DTR)
      COS_ALPHA = COS(ALPHA*DTR)

      SIN_DELTA = SIN(DELTA*DTR)
      COS_DELTA = COS(DELTA*DTR)

      SIN_KAPPA = SIN(KAPPA*DTR)
      COS_KAPPA = COS(KAPPA*DTR)

      C(1) =  -SIN_ALPHA*COS_KAPPA - COS_ALPHA*SIN_DELTA*SIN_KAPPA
      C(2) =   COS_ALPHA*COS_KAPPA - SIN_ALPHA*SIN_DELTA*SIN_KAPPA
      C(3) =   COS_DELTA*SIN_KAPPA

      C(4) =   SIN_ALPHA*SIN_KAPPA - COS_ALPHA*SIN_DELTA*COS_KAPPA
      C(5) =  -COS_ALPHA*SIN_KAPPA - SIN_ALPHA*SIN_DELTA*COS_KAPPA
      C(6) =   COS_DELTA*COS_KAPPA

      C(7) =   COS_ALPHA*COS_DELTA
      C(8) =   SIN_ALPHA*COS_DELTA
      C(9) =   SIN_DELTA
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the planet center (SCLINE,SCSAMP), the SSP (LAT,LON), and the
C north angle ANGLN, calculate ANGLA, ANGLB, CM, and OM.
C
C Outputs: ANGLA,ANGLB,CM,OM
C Note that ANGLN is also recalculated and may differ due to round off errs...
C
      SUBROUTINE FARENC(SCLAT,SCLON,RLORA,SCLINE,SCSAMP,PSCALE,FL,
     &        OAL,OAS,PSC,ME,CM,OM,ANGLN,ANGLA,ANGLB)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CONST/PI,DTR,RTD
      REAL*8 PSC(3),ME(3,3),CM(3,3),OM(3,3),OMp(3,3),RS(3)

C            Calculate OMp transpose...
      SCLA = SCLAT*RTD
      SCLO = (2.D0*PI-SCLON)*RTD
      ANGN = ANGLN*RTD + 90.D0	!IPL north angle
      CALL MOMATI(OAL,OAS,SCLINE,SCSAMP,PSCALE,FL,SCLO,SCLA,ANGN,
     &       0.D0,OMp,RS)	!Note the RS is ignored
C            Calculate C-matrix...
      DO 20 J=1,3
      DO 20 I=1,3
   20 CM(I,J) = ME(I,1)*OMp(J,1)+ME(I,2)*OMp(J,2)+ME(I,3)*OMp(J,3)
C            Compute angles N, A, and B...
      CALL GETANGLES(CM,ME(1,3),PSC,ANGLN,ANGLA,ANGLB)
      CALL OMMATRIX(ANGLN,ANGLA,ANGLB-SCLON+RLORA,OM) !Update OM-matrix
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to print summary of navigation data
C
      SUBROUTINE PNAV
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/SEDR/SEDR(200),LBUF(100),ISYSTEM
      REAL*4 SEDR
      INTEGER LBUF

      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,PLANET_ID,TARGET_ID

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

      CHARACTER*12 tname
      CHARACTER*80 MSG
      real*4 scl4,scs4

  100 FORMAT('NAVIGATION DATA FOR FRAME ',I10)
  101 FORMAT('S/C Event Time (yyyyddd hhmmssmmm)  SCET  ',I7,I10)
  102 FORMAT('Target body                         TARG  ',A8)
  103 FORMAT('Major equatorial radius (km)        RA    ',F8.1)
  104 FORMAT('Minor equatorial radius (km)        RB    ',F8.1)
  105 FORMAT('Polar radius (km)                   RC    ',F8.1)
  106 FORMAT('Longitude of major eq. radius (deg) LORA  ',F7.2)
  108 FORMAT('Spacecraft range (km)               RANG  ',I10)
  109 FORMAT('Spacecraft position (lat,lon(East)) SSP   ',
     & '(',F6.2,',',F7.2,')')
  110 FORMAT('O.S. planet center (line,sample)    PC    ',
     & '(',F10.2,',',F10.2,')')
  112 FORMAT('I.S. planet center (line,sample)    ISPC  ',
     & '(',F10.2,',',F10.2,')')
  115 FORMAT('North Angle (CW degrees from right) ANGLN ',F7.2)
  116 FORMAT('Camera Serial Number                CAM   ',I8)
  117 FORMAT('Focal length (mm)                   FL    ',F10.3)
  118 FORMAT('O.S. optical axis (line,sample)     OAXIS ',
     & '(',F6.1,',',F6.1,')')
  120 FORMAT('Scale (pixels/mm at focal plane)    SC    ',F8.4)
  121 FORMAT('Min and max ring radii (km)         RLIM  ',2F8.0)
  122 FORMAT('Solar position (lat,lon(East))      SOL   ',
     & '(',F6.2,',',F7.2,')')

      WRITE(MSG,100) FRAME_ID
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,101) IDATE,ITIME
      CALL XVMESSAGE(MSG,' ')

      CALL PBNAME(TARGET_ID,tname,*10)
      WRITE(MSG,102) tname
      CALL XVMESSAGE(MSG,' ')

   10 WRITE(MSG,103) RA
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,104) RB
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,105) RC
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,106) RLORA*RTD
      CALL XVMESSAGE(MSG,' ')

      IRANGE = RSC + 0.5
      WRITE(MSG,108) IRANGE
      CALL XVMESSAGE(MSG,' ')

      RLAT = GEODET(SCLAT,SCLON)*RTD
      RLON = SCLON*RTD
      WRITE(MSG,109) RLAT,RLON
      CALL XVMESSAGE(MSG,' ')

      CALL PLAINV(IND,SCLAT,SCLON,SCLINE,SCSAMP,OM,PSC3,RLORA,
     &       OAL,OAS,ZSCALE)
      WRITE(MSG,110,ERR=50) SCLINE,SCSAMP
   50 CALL XVMESSAGE(MSG,' ')
      IF (ITYPE.EQ.7) THEN
         CALL CONVISOS(PROJECT,ICAM,scl4,scs4,sngl(scline),
     &	  sngl(scsamp),0,CONV,NPH,NPV,ind)
	 scline=scl4
	 scsamp=scs4
         WRITE(MSG,112,ERR=52) SCLINE,SCSAMP
   52    CALL XVMESSAGE(MSG,' ')
      ENDIF

      WRITE(MSG,115) ANGLN*RTD
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,116) ICAM
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,117) FL
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,118) OAL,OAS
      CALL XVMESSAGE(MSG,' ')

      PSCALE = ZSCALE/FL
      WRITE(MSG,120) PSCALE
      CALL XVMESSAGE(MSG,' ')

      WRITE(MSG,121) RMIN,RMAX
      CALL XVMESSAGE(MSG,' ')

      RLAT = GEODET(SUNLAT,SUNLON)*RTD
      RLON = SUNLON*RTD
      WRITE(MSG,122) RLAT,RLON
      CALL XVMESSAGE(MSG,' ')

      IF (IGEO.EQ.0) THEN
          CALL XVMESSAGE('All latitudes are planetocentric',' ')
      ELSE
          CALL XVMESSAGE('All latitudes are planetographic',' ')
      ENDIF

      CALL CMSOURCE(SEDR,isource)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create display.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to display the image
c Return indicator IND=0 if no action, =1 if image is redisplayed,
C =2 if other.
C
      SUBROUTINE DISPLAY(PIC,HPIC,NL,NS,ind)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEMS
      REAL*4 RES,BLEM

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB

      COMMON/CHIST/HIS(256),HFLG,HBEG,HINC,NSPIKES,LHPER(2)
      INTEGER HIS,HFLG,HBEG,HINC
      REAL*4 LHPER

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CONST/PI,DTR,RTD

      LOGICAL XST,XDIFILL,PARMTST,RPARMTST,XVIPTST,XDCSET

      INTEGER*4 HIST(-32768:32767)
      INTEGER LOHI(2)

      IND = 2

      IF (PARMTST('CZOOM',IVAL,I)) THEN		!Trackball zoom routine
         IF (IVAL.EQ.0) THEN
            CALL XVMESSAGE('***CZOOM=0 is invalid',' ')
            RETURN
         ENDIF
         CALL CURSOR(RLINE,RSAMP)		! Read cursor position
	 IZOOM = MIN0(IVAL,4)			! Get zoom factor
	 ZOOM = IZOOM		
         IF (IZOOM.LT.0) ZOOM = -1.0/IZOOM
	 Z = 2.*ZOOM
	 SL = RLINE - NLDS/Z			! and center display
	 SS = RSAMP - NSDS/Z			! about it
         IF (SS.LT.1) SS=1
         IF (SL.LT.1) SL=1
	 CALL DPIC(PIC,SL,SS,NL,NS)		! Display image
         XST = XDCSET(IDEV,TB,NSDS/2,NLDS/2)	! Center cursor
         HFLG = 0
         IND = 1
         RETURN
      ENDIF

      IF (PARMTST('STRETCH',lohi,I)) THEN	! STRECH picture
         IF (ICODE.EQ.2) THEN
            CALL HSTRETCH(HPIC,SL,SS,NL,NS,	! Scale HPIC to PIC
     &		pic,lohi(1),lohi(2),nlev,hbeg,hinc)
            CALL DPIC(PIC,SL,SS,NL,NS)	! Display image
            HFLG = 0
         ENDIF
         CALL STRECH(LOHI(1),LOHI(2),stbl)
         CALL LUTWRITE(IDEV,STBL)
         RETURN
      ENDIF

      IF (RPARMTST('ASTRETCH',LHPER,I)) THEN	! STRECH picture
         IF (ICODE.EQ.1) THEN
            CALL HISTGEN1(PIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,his,hflg)
            CALL ASTRCH(HIS,ilow,ihigh,LHPER(1),LHPER(2),256)
         ELSE
            CALL HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
            CALL ASTRC2(HIST,NFREQ,LHPER(1),LHPER(2),ilow,ihigh)
            CALL HSTRETCH(HPIC,SL,SS,NL,NS,
     &		pic,ilow,ihigh,nlev,hbeg,hinc)
            DO J=0,255	!Compress the histogram to 256 grey-levels
               CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J+1),1)
            ENDDO
            CALL DPIC(PIC,SL,SS,NL,NS)	!Redisplay image
            HFLG = 1
         ENDIF
         CALL STRECH(ILOW,IHIGH,STBL)
         CALL LUTWRITE(IDEV,STBL)
         RETURN
      ENDIF

      IF (XVIPTST('H'))	THEN		!Restore original picture
         IF (ICODE.EQ.1) THEN
            ILOW = 0			!Restore the original
            IHIGH = 255			!stretch limits
         ELSE
            SL = 1
            SS = 1
            IZOOM = MAX0((NL-1)/NLDS,(NS-1)/NSDS) + 1
            IF (IZOOM.GT.1) THEN
               ZOOM = 1.0/IZOOM
               IZOOM = -IZOOM
            ELSE
               ZOOM = 1.0D0
               IZOOM = 1
            ENDIF
            CALL HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
            CALL ASTRC2(HIST,NFREQ,0.5,0.5,ilow,ihigh)
            CALL HSTRETCH(HPIC,SL,SS,NL,NS,
     &		pic,ilow,ihigh,nlev,hbeg,hinc)
            DO J=0,255	!Compress the histogram to 256 grey-levels
               CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J+1),1)
            ENDDO
         ENDIF
         CALL HOME(PIC,NL,NS,NLDS,NSDS,sl,ss,izoom,zoom)
         CALL STRECH(ILOW,IHIGH,STBL)
         CALL LUTWRITE(IDEV,STBL)
         HFLG = 0
         IND = 1
         RETURN
      ENDIF

      IF (PARMTST('SPIKES',N,I)) NSPIKES=N

      IF (XVIPTST('HIST')) THEN  !Compute and display the histogram
         IF (ICODE.EQ.1) THEN
            CALL HISTGEN1(PIC,SL,SS,NL,NS,NLDS,NSDS,IZOOM,ZOOM,
     &		his,HFLG)
         ELSEIF (HFLG.EQ.0) THEN
            CALL HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
            DO J=0,255	!Compress the histogram to 256 grey-levels
               CALL SUMV(4,HINC,HIST(HBEG+J*HINC),his(J+1),1)
            ENDDO
         ENDIF
         CALL HDISPLAY(IDEV,G,HIS,101,101,100,HBEG,HINC,NSPIKES)
      ENDIF

      IF (XVIPTST('GERASE')) THEN
         XST = XDIFILL(IDEV,G,XDB)
         IND = 1
         RETURN
      ENDIF

      IF (XVIPTST('SRES')) THEN
         CALL DRAWCURVE(RES,202,0)
         IND = 2
         RETURN
      ENDIF

      IF (XVIPTST('SBLEMS')) THEN
         CALL DRAWBLEMS(RES,BLEM,NBLEMS,3)
         IND = 2
         RETURN
      ENDIF

      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to support display of the planet
C Return indicator IND=0 if no action, =1 if image is redisplayed,
C =2 if other
C
      SUBROUTINE PDISPLAY(PIC,HPIC,NL,NS,IND) 
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/CPAR/PAR(20),PAR2(20)
      REAL*4 PAR,PAR2

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CONST/PI,DTR,RTD
      LOGICAL XST,PARMTST,RPARMTST,XVIPTST,XDCSET

      REAL*4 R4
      include 'fortport'

      CHARACTER*80 MSG
  110 FORMAT('(L,S)=(',F8.2,',',F8.2,')  (LAT,LON)=(',
     &         F6.2,',',F7.2,')')
  112 FORMAT('PHASE=(',F7.2,')   DN=(',I6,')   RANGE=(',
     &         F10.1,')   SCALE=(',F8.1,')')  !VRH 3/1/94 new data

      IND = 2

      IF (XVIPTST('C')) THEN
   10    CALL CURSOR(RLINE,RSAMP)
         CALL LATLON(ISTATUS,RLINE,RSAMP,RLAT,RLON)
         IF (ISTATUS.EQ.1) THEN
c             CALL DRAWDOT(RLINE,RSAMP,255)  !VRH 7/31/89 remove drawing dot
             R = GEODET(RLAT,RLON)
             WRITE(MSG,110) RLINE,RSAMP,R*RTD,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RLAT,RLON,PHA,RANGE,SCALE,DUMMY) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN,RANGE,SCALE !VRH 3/1/94 new data
             CALL XVMESSAGE(MSG,' ')
         ENDIF
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 10
         RETURN
      ENDIF

      IF (PARMTST('OVERLAY',int,i)) THEN  !Overlay planet with lat-lon grid
         CALL OVERLAY1(INT)
         RETURN
      ENDIF

      IF (RPARMTST('LATI',r4,i)) THEN
	RLAT = R4
        CALL OVERLAT(RLAT*DTR)
        RETURN
      ENDIF

      IF (RPARMTST('LONG',r4,i)) THEN
	RLON = R4
        CALL OVERLON(RLON*DTR)
        RETURN
      ENDIF

      IF (RPARMTST('LL',par,i)) THEN		! User inputs (lat,lon)
         RLAT = PAR(1)
         RLON = PAR(2)*DTR
         IF (DABS(RLAT).GT.90.D0) THEN
            CALL XVMESSAGE('***Invalid latitude value',' ')
            RETURN
         ENDIF
         RLAT = GEOCEN(RLAT*DTR,RLON)
         CALL LINSAM(ISTATUS,RLAT,RLON,RLINE,RSAMP) !Compute corresponding (l,s)
         IF (ISTATUS.EQ.1) THEN
             WRITE(MSG,110) RLINE,RSAMP,PAR(1),PAR(2)
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RLAT,RLON,PHA,RANGE,SCALE,DUMMY) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN,RANGE,SCALE !VRH 3/1/94 new data
             CALL XVMESSAGE(MSG,' ')
             ILINE = (RLINE-SL)*ZOOM + 1.5
             ISAMP = (RSAMP-SS)*ZOOM + 1.5
             IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         ENDIF
         RETURN
      ENDIF

      IF (RPARMTST('LS',par,i)) THEN
         RLINE = PAR(1)
         RSAMP = PAR(2)
         CALL LATLON(ISTATUS,RLINE,RSAMP,RLAT,RLON)
         IF (ISTATUS.EQ.1) THEN
            R = GEODET(RLAT,RLON)
            WRITE(MSG,110) RLINE,RSAMP,R*RTD,RLON*RTD
            CALL XVMESSAGE(MSG,' ')
            CALL PHASE(RLAT,RLON,PHA,RANGE,SCALE,DUMMY) !VRH 3/1/94 new arg
            IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
               IDN = 0
            ELSE IF (ICODE.EQ.1) THEN
               IDN = BYTE2INT(PIC(RSAMP,RLINE))
            ELSE
               IDN = HPIC(RSAMP,RLINE)
            ENDIF
            WRITE(MSG,112) PHA*RTD,IDN,RANGE,SCALE !VRH 3/1/94 new data
            CALL XVMESSAGE(MSG,' ')
         ENDIF
         ILINE = (RLINE-SL)*ZOOM + 1.5
         ISAMP = (RSAMP-SS)*ZOOM + 1.5
         IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         RETURN
      ENDIF

      IND = 0
      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to support display of the ring plane
C Return indicator IND=0 if no action, =1 if image is redisplayed, =2 if other
C
      SUBROUTINE RDISPLAY(PIC,HPIC,NL,NS,IND)
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRINGC/ RINGS
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

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
      REAL*4 PAR(2)     !VRH added to support RL,LS options 1/19/89
      LOGICAL XST,RPARMTST,XVIPTST,XDCSET

      INCLUDE 'fortport'

      CHARACTER*80 MSG
  110 FORMAT('(L,S)=(',F7.2,',',F7.2,') (R,LON)=(',
     &         F10.1,',',F7.2,')')
  111 FORMAT('Reference plane is that of ',A1,'-Ring')
  112 FORMAT('PHASE=(',F7.2,')   DN=(',I6,')')
  113 FORMAT(' RANGE=(',F10.1,')   SCALE=(',
     &         F8.1,')   RSCALE=(',F8.1,')') !VRH 3/1/94
      IND = 2

      IF (XVIPTST('C')) THEN
         CALL GETRING(MRING)
         IF (MRING.EQ.1) THEN
             CALL XVMESSAGE('Reference plane is planet''s equator',' ')
         ELSE
             WRITE(MSG,111) RINGS(MRING)
             CALL XVMESSAGE(MSG,' ')
         ENDIF

   10    CALL CURSOR(RLINE,RSAMP)
c         CALL DRAWDOT(RLINE,RSAMP,255)  !VRH 7/31/89 remove drawing dot
         CALL LATLON(ISTATUS,RLINE,RSAMP,RADIUS,RLON)
         IF (ISTATUS.EQ.1) THEN
             WRITE(MSG,110) RLINE,RSAMP,RADIUS,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,113) RANGE,SCALE,RSCALE !VRH 3/1/94
             CALL XVMESSAGE(MSG,' ')
         ENDIF
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 10
         RETURN
      ENDIF

      IF (RPARMTST('RL',PAR,I)) THEN		!User inputs (RADIUS,LON)
         RADIUS = PAR(1)                        !VRH added RL option 1/19/89
         RLON = PAR(2)*DTR
         CALL GETRING(MRING)
         CALL LINSAM(ISTATUS,RADIUS,RLON,rline,rsamp) !Compute (l,s)
         IF (ISTATUS.EQ.1) THEN
             IF (MRING.EQ.1) THEN
                 CALL XVMESSAGE('Reference plane is planet''s equator',
     &		  ' ')
             ELSE
                 WRITE(MSG,111) RINGS(MRING)
                 CALL XVMESSAGE(MSG,' ')
             ENDIF
             WRITE(MSG,110) RLINE,RSAMP,RADIUS,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,113) RANGE,SCALE,RSCALE !VRH 3/1/94
             CALL XVMESSAGE(MSG,' ')
             ILINE = (RLINE-SL)*ZOOM + 1.5
             ISAMP = (RSAMP-SS)*ZOOM + 1.5
             IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         ENDIF
         RETURN
      ENDIF

      IF (RPARMTST('LS',PAR,I)) THEN ! VRH added LS option to RDISPLAY 1/19/89
         RLINE = PAR(1)
         RSAMP = PAR(2)
         CALL GETRING(MRING)
         CALL LATLON(ISTATUS,RLINE,RSAMP,RADIUS,RLON)
         IF (ISTATUS.EQ.1) THEN
             IF (MRING.EQ.1) THEN
                 CALL XVMESSAGE('Reference plane is planet''s equator',
     &		  ' ')
             ELSE
                 WRITE(MSG,111) RINGS(MRING)
                 CALL XVMESSAGE(MSG,' ')
             ENDIF
             WRITE(MSG,110) RLINE,RSAMP,RADIUS,RLON*RTD
             CALL XVMESSAGE(MSG,' ')
             CALL PHASE(RADIUS,RLON,PHA,RANGE,SCALE,RSCALE) !VRH 3/1/94 new arg
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) PHA*RTD,IDN
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,113) RANGE,SCALE,RSCALE !VRH 3/1/94
             CALL XVMESSAGE(MSG,' ')
         ENDIF
         ILINE = (RLINE-SL)*ZOOM + 1.5
         ISAMP = (RSAMP-SS)*ZOOM + 1.5
         IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         RETURN
      ENDIF

      IF (XVIPTST('PROFILE')) THEN
          CALL PROFILE(PIC,HPIC,NL,NS)
          RETURN
      ENDIF

      IF (XVIPTST('PHASPROF')) THEN
          CALL PHASPROF(PIC,HPIC,NL,NS)
          RETURN
      ENDIF

      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to support display of the celestial sphere.
C Return indicator IND=0 if no action, =1 if image is redisplayed,
C =2 if other
C
      SUBROUTINE SDISPLAY(PIC,HPIC,NL,NS,IND) 
      IMPLICIT REAL*8 (A-H,O-Z)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG

      COMMON/CSTAR1/ISAO,SAOFILE
      CHARACTER*72 SAOFILE

      COMMON/CSTAR2/STARNAME,STARTYPE
      CHARACTER*13 STARNAME(2000)
      CHARACTER*3 STARTYPE(2000)

      COMMON/CONST/PI,DTR,RTD
      REAL*4 PAR(2)

      LOGICAL XST,RPARMTST,XVIPTST,XDCSET

CBTC  ...convert *TO* GSC or SAO reference frame 
      logical from /.false./
      CHARACTER*7 CSYSTEM

      INCLUDE 'fortport'

      CHARACTER*80 MSG
  110 FORMAT('(L,S)=(',F8.2,',',F8.2,')  RA=',I2,':',I2.2,':',F4.1, 
     &         ' DEC=',I3,' deg',I3,'''',F4.1,'"',1x,a7)
  112 FORMAT('DN=(',I6,')')

      IF (ISAO .EQ. -2) THEN
         CSYSTEM = '(J2000)'
      ELSE
         CSYSTEM = '(EME50)'
      ENDIF

      IND = 2

      IF (XVIPTST('C')) THEN
   10    CALL CURSOR(rline,rsamp)
c         CALL DRAWDOT(RLINE,RSAMP,255)  !VRH 7/31/89 remove drawing dot
         CALL LATLON(ISTATUS,RLINE,RSAMP,ra,dec)
CBTC
         RA0 = ra
         DEC0 = dec
CBTC  ...convert *TO* Catalog (SAO or GSC) reference frame
         call fromorto_star( ra, dec, from)
CBTC
         IF (ISTATUS.EQ.1) THEN
             CALL RADTOHMS(RA,ih,im,rs)
             CALL RADTODMS(DEC,ideg,imin,rsec)
             WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC,csystem
CBTC             WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC
             IF (MSG(37:37) .EQ. ' ') MSG(37:37) = '0'
             CALL XVMESSAGE(MSG,' ')
             WRITE(MSG,'(a33,2f8.3,a,2f8.3)') 
     &         'RA,DEC '//csystem// ' // (local):  '
     &         , ra*rtd, dec*rtd, ' // ', ra0*rtd, dec0*rtd
             CALL XVMESSAGE(MSG,' ')
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) IDN
             CALL XVMESSAGE(MSG,' ')
C		****Temporary code for debugging proper motion***
         RMIN = 1.E+10
         IMIN = 1
         DO I=1,NSTARS		!Find nearest star in catalog
             R = (SPTS(1,I)-RLINE)**2 + (SPTS(2,I)-RSAMP)**2
             IF (R.LT.RMIN) THEN
                RMIN = R
                IMIN = I
             ENDIF
         ENDDO
CBTC
         ra = stars(1,imin)
         dec = stars(2,imin)
         RA0 = ra
         DEC0 = dec
CBTC
CBTC convert to catalog reference frame
         call fromorto_star( ra, dec, from)
         WRITE(MSG,'(a,i4,a)') 'Closest star info: (star #',IMIN,')'
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,'(a33,2f8.3,a,2f8.3)') 
     &         'RA,DEC '//csystem// ' // (local):  '
     &         , ra*rtd, dec*rtd, ' // ', ra0*rtd, dec0*rtd
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,'(a,f8.2,a1,f8.2,a1)') '(L,S)=('
     &             , spts(1,imin), ',', spts(2,imin), ')'
         CALL XVMESSAGE(MSG,' ')
         WRITE(MSG,'(a,f8.3)') 'MAG = ', stars(3,imin) 
         IF (STARNAME(IMIN) .NE. ' ')
     &       WRITE(MSG(15:),113) STARNAME(IMIN)
         IF (STARTYPE(IMIN).NE.' ') 
     &      WRITE(MSG(36:),114) STARTYPE(IMIN)
  113    FORMAT(' NAME = ',a13)
  114    FORMAT(' TYPE = ',a3)
         CALL XVMESSAGE(MSG,' ')
CBTC
CBTC         WRITE(MSG,111) IMIN,(STARS(J,IMIN)*RTD,J=1,2),STARS(3,IMIN)
CBTC  111 FORMAT('Closest star is ',I3,' (RA,DEC,MAG)=',3F8.3,' (EME50)')
CBTC         CALL XVMESSAGE(MSG,' ')

         ENDIF
C		***End temporary code ***
         CALL XVINTRACT('READY',
     &            ' Hit Return when ready or type ''EXIT')
         IF (.NOT.XVIPTST('EXIT')) GOTO 10
         RETURN
      ENDIF

      IF (RPARMTST('RD',PAR,I)) THEN		!User inputs (RA,DEC)
         RA = PAR(1)*DTR
         DEC = PAR(2)*DTR
         CALL LINSAM(ISTATUS,RA,DEC,rline,rsamp) !Compute corresponding (l,s)
         IF (ISTATUS.EQ.1) THEN
             CALL RADTOHMS(RA,ih,im,rs)
             CALL RADTODMS(DEC,ideg,imin,rsec)
             WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC
             IF (MSG(37:37) .EQ. ' ') MSG(37:37) = '0'
             CALL XVMESSAGE(MSG,' ')
             IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
                IDN = 0
             ELSE IF (ICODE.EQ.1) THEN
		IDN = BYTE2INT(PIC(RSAMP,RLINE))
             ELSE
                IDN = HPIC(RSAMP,RLINE)
             ENDIF
             WRITE(MSG,112) IDN
             CALL XVMESSAGE(MSG,' ')
             ILINE = (RLINE-SL)*ZOOM + 1.5
             ISAMP = (RSAMP-SS)*ZOOM + 1.5
             IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         ENDIF
         RETURN
      ENDIF

      IF (RPARMTST('LS',PAR,I)) THEN
         RLINE = PAR(1)
         RSAMP = PAR(2)
         CALL LATLON(ISTATUS,RLINE,RSAMP,ra,dec)
         IF (ISTATUS.EQ.1) THEN
            CALL RADTOHMS(RA,ih,im,rs)
            CALL RADTODMS(DEC,ideg,imin,rsec)
            WRITE(MSG,110) RLINE,RSAMP,IH,IM,RS,IDEG,IMIN,RSEC
            IF (MSG(37:37) .EQ. ' ') MSG(37:37) = '0'
            CALL XVMESSAGE(MSG,' ')
            IF (RLINE.LT.1.OR.RSAMP.LT.1.OR.
     &           RLINE.GT.NL.OR.RSAMP.GT.NS) THEN
               IDN = 0
            ELSE IF (ICODE.EQ.1) THEN
               IDN = BYTE2INT(PIC(RSAMP,RLINE))
            ELSE
               IDN = HPIC(RSAMP,RLINE)
            ENDIF
            WRITE(MSG,112) IDN
            CALL XVMESSAGE(MSG,' ')
         ENDIF
         ILINE = (RLINE-SL)*ZOOM + 1.5
         ISAMP = (RSAMP-SS)*ZOOM + 1.5
         IF (ILINE.GE.1.AND.ISAMP.GE.1.AND.ILINE.LE.NLDS
     &		.AND.ISAMP.LE.NSDS) XST=XDCSET(IDEV,TB,ISAMP,ILINE)
         RETURN
      ENDIF

      IND = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generic plot routine
C Routine to draw a plot onto graphics screen.
C Outputs: X0,Y0,DX,DY plot transformation on graphics
C
      SUBROUTINE PLOTDISP(PTS,NVAL,NPTS,ix,iy,BADVAL1,L0,S0
     &                   ,X0,Y0,DX,DY)
      include 'fortport'        ! defines int2byte

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      REAL*8 PI,DTR,RTD
      COMMON/CONST/PI,DTR,RTD
C
      INTEGER*4 NVAL,NPTS, ix, iy
      REAL*4 PTS(NVAL,NPTS), BADVAL1
      INTEGER*4 L0,S0
      REAL*4 X0,Y0,DX,DY
C
      INTEGER*4 XX(2),YY(2), SLEN
      INTEGER ILOG
      REAL*4 MAX,MIN,XDIFF,YDIFF
      REAL*4 SDIVX,BDIVX,SDIVY,BDIVY,NUM
      LOGICAL XST,XDIPOLYLINE,XDTROTATE,XDTTEXT
      CHARACTER*6 NUMB

      bmdn = int2byte(xdw)

      LLEN = NINT( NLDS * 0.4)
      L1 = L0 - LLEN
      SLEN = NINT( NSDS * 0.4)
      IF ((NPTS/NSDS).GT.2) SLEN = NINT( NSDS * 0.8)
      S1 = S0 + SLEN

      XX(1) = S0
      YY(1) = L0
      XX(2) = S1 
      YY(2) = L0
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XX(2) = S0
      YY(2) = L1
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XST = XDTTEXT(IDEV,G,S0-8,L1-3,1,2,'DN')
c
c      IF (RPLOT) THEN
c        CALL XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'R')
c      ELSE
c        CALL XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'L')
c      ENDIF
C 
      X0 = PTS(1,1)
      XDIFF = PTS(1,NPTS) - X0
c
c      IF(.NOT.RPLOT) THEN
c        X0 = RTD*X0
c        XDIFF = RTD*XDIFF
c      END IF
c
      ISIGN = NINT(XDIFF/ABS(XDIFF))
      IF (ABS(XDIFF).LT.2.) XDIFF = 2.*ISIGN
      ILOG = INT(LOG10(ABS(XDIFF)))
      NUM  = ABS(XDIFF)/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVX = 0.1*10.**ILOG
        BDIVX = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVX = 0.2*10.**ILOG
        BDIVX = 1.0*10.**ILOG
      ELSE
        SDIVX = 0.5*10.**ILOG
        BDIVX = 2.0*10.**ILOG
      ENDIF
      IF (XDIFF.LT.0.) THEN
        SDIVX = -SDIVX
        BDIVX = -BDIVX
      ENDIF
      IF (MOD(X0,SDIVX).EQ.0) THEN
         X0 = X0 - SDIVX*ISIGN
         XDIFF = XDIFF + SDIVX*ISIGN
      ENDIF
      X0 = INT(X0/SDIVX)*SDIVX
      DX = (INT((X0+XDIFF+SDIVX)/SDIVX)*SDIVX-X0)/FLOAT(SLEN)

      DO I = 1,100
        NUM = X0 + (I-1)*SDIVX
        XX(1) = S0 + (NUM-X0)/DX + 0.5
        IF(XX(1).GT.S1) GOTO 100
        YY(1) = L0
        XX(2) = XX(1)
        YY(2) = L0 + 2
        IF(MOD(NINT(NUM/SDIVX),NINT(BDIVX/SDIVX)).EQ.0) THEN !VRH 7/28/89
           YY(2) = L0 + 5
c
            WRITE(NUMB,'(F6.1)') X0+(I-1)*SDIVX
c           IF (RPLOT) THEN
c               WRITE(NUMB,'(I6)') NINT(X0+(I-1)*SDIVX)
c           ELSE
c               WRITE(NUMB,'(F6.1)') X0+(I-1)*SDIVX
c           ENDIF
c
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,XX(1),L0+15,2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC         call mvcl(numb,number,1)
CBTC         CALL XDTTEXT(IDEV,G,XX(1)-4*NCHAR,L0+15,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

  100 MAX = -32768.
      MIN =  32767.
      DO I = 1,NPTS
        IF(PTS(2,I).GE.MAX) MAX = PTS(2,I)
        IF(PTS(2,I).LE.MIN) MIN = PTS(2,I)
      ENDDO

      Y0 = MIN
      YDIFF = MAX - MIN
      IF (YDIFF.LT.2.) YDIFF = 2.
      ILOG = INT(LOG10(YDIFF))
      NUM = YDIFF/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVY = 0.1*10.**ILOG
        BDIVY = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVY = 0.2*10.**ILOG
        BDIVY = 1.0*10.**ILOG
      ELSE
        SDIVY = 0.5*10.**ILOG
        BDIVY = 2.0*10.**ILOG
      ENDIF
      IF (MOD(Y0,SDIVY).EQ.0.OR.Y0.LT.0.) THEN
         Y0 = Y0 - SDIVY
         YDIFF = YDIFF + SDIVY
      ENDIF
      Y0 = INT(Y0/SDIVY)*SDIVY
      DY = -(INT((Y0+YDIFF+SDIVY)/SDIVY)*SDIVY-Y0)/FLOAT(LLEN)

      XST = XDTROTATE(90.) !Y-axis text vertical
      DO I = 1,100
        NUM = Y0 + (I-1)*SDIVY
        YY(1) = L0 + (NUM-Y0)/DY + 0.5
        IF(YY(1).LT.L1) GOTO 200
        XX(1) = S0
        YY(2) = YY(1)
        XX(2) = S0 - 2
        IF(MOD(NINT(NUM/SDIVY),NINT(BDIVY/SDIVY)).EQ.0) THEN
           XX(2) = S0 - 5
           IF(BDIVY.LT.1.) THEN
              WRITE(NUMB,'(F6.1)') Y0+(I-1)*SDIVY
           ELSE
              WRITE(NUMB,'(I6)') NINT(Y0+(I-1)*SDIVY)
           ENDIF
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,S0-7,YY(1),2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC	   call mvcl(numb,number,1)
CBTC           CALL XDTTEXT(IDEV,G,S0-7,YY(1)+4*NCHAR,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

200   XST = XDTROTATE(0.) !Return text to horizontal
      DO I=1,NPTS
         XX(1) = S0 + (PTS(1,I)-X0)/DX + 0.5
c
c        IF (RPLOT) THEN
c          XX(1) = S0 + (PTS(1,I)-X0)/DX + 0.5
c        ELSE
c          XX(1) = S0 + (RTD*PTS(1,I)-X0)/DX + 0.5
c        ENDIF
c
        YY(1) = L0 + (PTS(2,I)-Y0)/DY + 0.5
        XX(2) = XX(1)
        YY(2) = YY(1)
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
        IF (.NOT. XST) THEN
           CALL PRNT(4,1,XST,'XDIPOLYLINE Error=.')
           RETURN
        ENDIF
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw a profile plot onto graphics screen.
C Outputs: X0,Y0,DX,DY plot transformation on graphics
C
      SUBROUTINE PRDISPLAY(PTS,NPTS,RPLOT,L0,S0,X0,Y0,DX,DY)
      include 'fortport'        ! defines int2byte
      REAL*4 PTS(2,NPTS)
      LOGICAL RPLOT

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      COMMON/CONST/PI,DTR,RTD
      REAL*8 PI,DTR,RTD

      INTEGER*4 NPTS
      INTEGER*4 S0,S1,SLEN
      INTEGER*4 XX(2),YY(2)
      INTEGER ILOG
      REAL*4 X0,Y0,DX,DY,MAX,MIN,XDIFF,YDIFF
      REAL*4 SDIVX,BDIVX,SDIVY,BDIVY,NUM
      LOGICAL XST,XDIPOLYLINE,XDTROTATE,XDTTEXT
      CHARACTER*6 NUMB

      bmdn = int2byte(xdw)

      LLEN = 220
      L1 = L0 - LLEN
      SLEN = LLEN
      IF (NPTS.GT.1000) SLEN = 476
      S1 = S0 + SLEN

      XX(1) = S0
      YY(1) = L0
      XX(2) = S1 
      YY(2) = L0
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XX(2) = S0
      YY(2) = L1
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      XST = XDTTEXT(IDEV,G,S0-8,L1-3,1,2,'DN')
      IF (RPLOT) THEN
        XST = XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'R')
      ELSE
        XST = XDTTEXT(IDEV,G,S1+5,L0+3,1,1,'L')
      ENDIF
C 
      X0 = PTS(1,1)
      XDIFF = PTS(1,NPTS) - X0
      IF(.NOT.RPLOT) THEN
        X0 = RTD*X0
        XDIFF = RTD*XDIFF
      END IF
      ISIGN = NINT(XDIFF/ABS(XDIFF))
      IF (ABS(XDIFF).LT.2.) XDIFF = 2.*ISIGN
      ILOG = INT(LOG10(ABS(XDIFF)))
      NUM  = ABS(XDIFF)/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVX = 0.1*10.**ILOG
        BDIVX = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVX = 0.2*10.**ILOG
        BDIVX = 1.0*10.**ILOG
      ELSE
        SDIVX = 0.5*10.**ILOG
        BDIVX = 2.0*10.**ILOG
      ENDIF
      IF (XDIFF.LT.0.) THEN
        SDIVX = -SDIVX
        BDIVX = -BDIVX
      ENDIF
      IF (MOD(X0,SDIVX).EQ.0) THEN
         X0 = X0 - SDIVX*ISIGN
         XDIFF = XDIFF + SDIVX*ISIGN
      ENDIF
      X0 = INT(X0/SDIVX)*SDIVX
      DX = (INT((X0+XDIFF+SDIVX)/SDIVX)*SDIVX-X0)/FLOAT(SLEN)

      DO I = 1,100
        NUM = X0 + (I-1)*SDIVX
        XX(1) = S0 + (NUM-X0)/DX + 0.5
        IF(XX(1).GT.S1) GOTO 100
        YY(1) = L0
        XX(2) = XX(1)
        YY(2) = L0 + 2
        IF(MOD(NINT(NUM/SDIVX),NINT(BDIVX/SDIVX)).EQ.0) THEN !VRH 7/28/89
           YY(2) = L0 + 5
           IF (RPLOT) THEN
               WRITE(NUMB,'(I6)') NINT(X0+(I-1)*SDIVX)
           ELSE
               WRITE(NUMB,'(F6.1)') X0+(I-1)*SDIVX
           ENDIF
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,XX(1),L0+15,2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC         call mvcl(numb,number,1)
CBTC         CALL XDTTEXT(IDEV,G,XX(1)-4*NCHAR,L0+15,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

  100 MAX = -32768.
      MIN =  32767.
      DO I = 1,NPTS
        IF(PTS(2,I).GE.MAX) MAX = PTS(2,I)
        IF(PTS(2,I).LE.MIN) MIN = PTS(2,I)
      ENDDO

      Y0 = MIN
      YDIFF = MAX - MIN
      IF (YDIFF.LT.2.) YDIFF = 2.
      ILOG = INT(LOG10(YDIFF))
      NUM = YDIFF/10.**ILOG
      IF (NUM.LE.2.) THEN
        SDIVY = 0.1*10.**ILOG
        BDIVY = 0.5*10.**ILOG
      ELSE IF (NUM.LE.4.) THEN
        SDIVY = 0.2*10.**ILOG
        BDIVY = 1.0*10.**ILOG
      ELSE
        SDIVY = 0.5*10.**ILOG
        BDIVY = 2.0*10.**ILOG
      ENDIF
      IF (MOD(Y0,SDIVY).EQ.0.OR.Y0.LT.0.) THEN
         Y0 = Y0 - SDIVY
         YDIFF = YDIFF + SDIVY
      ENDIF
      Y0 = INT(Y0/SDIVY)*SDIVY
      DY = -(INT((Y0+YDIFF+SDIVY)/SDIVY)*SDIVY-Y0)/FLOAT(LLEN)

      XST = XDTROTATE(90.) !Y-axis text vertical
      DO I = 1,100
        NUM = Y0 + (I-1)*SDIVY
        YY(1) = L0 + (NUM-Y0)/DY + 0.5
        IF(YY(1).LT.L1) GOTO 200
        XX(1) = S0
        YY(2) = YY(1)
        XX(2) = S0 - 2
        IF(MOD(NINT(NUM/SDIVY),NINT(BDIVY/SDIVY)).EQ.0) THEN
           XX(2) = S0 - 5
           IF(BDIVY.LT.1.) THEN
              WRITE(NUMB,'(F6.1)') Y0+(I-1)*SDIVY
           ELSE
              WRITE(NUMB,'(I6)') NINT(Y0+(I-1)*SDIVY)
           ENDIF
           NCHAR = 6
CBTC
           DOWHILE ( NCHAR .GT. 1 .AND. NUMB(1:1) .EQ. ' ')
             NCHAR = NCHAR - 1
             NUMB = NUMB(2:)
           ENDDO
           XST = XDTTEXT(IDEV,G,S0-7,YY(1),2,NCHAR,NUMB)
CBTC
CBTC           DO J=1,5
CBTC              IF (NUMB(1:1).eq.' ') THEN
CBTC                 NCHAR = NCHAR - 1
CBTC                 NUMB = NUMB(2:)
CBTC              ENDIF
CBTC           ENDDO
CBTC	   call mvcl(numb,number,1)
CBTC           CALL XDTTEXT(IDEV,G,S0-7,YY(1)+4*NCHAR,1,NCHAR,NUMBER)
CBTC
        ENDIF
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
      END DO

200   XST = XDTROTATE(0.) !Return text to horizontal
      DO I=1,NPTS
        IF (RPLOT) THEN
          XX(1) = S0 + (PTS(1,I)-X0)/DX + 0.5
        ELSE
          XX(1) = S0 + (RTD*PTS(1,I)-X0)/DX + 0.5
        ENDIF
        YY(1) = L0 + (PTS(2,I)-Y0)/DY + 0.5
        XX(2) = XX(1)
        YY(2) = YY(1)
        XST = XDIPOLYLINE(IDEV,G,bmdn,2,XX,YY)
        IF (.NOT. XST) THEN
           CALL PRNT(4,1,XST,'XDIPOLYLINE Error=.')
           RETURN
        ENDIF
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Scale HPIC to PIC, stretching from ILOW TO IHIGH.
C
      SUBROUTINE HSTRETCH(HPIC,SL,SS,NL,NS,
     &		pic,ilow,ihigh,nlev,hbeg,hinc)
      BYTE PIC(NS,NL)
      INTEGER*2 HPIC(NS,NL)
      INTEGER*4 HBEG,HINC

      NLEV = IABS(IHIGH-ILOW) + 1	!Number of halfword grey levels
      HINC = (NLEV-1)/256 + 1
      IF (ILOW.LT.IHIGH) THEN
         HBEG = (ILOW/HINC)*HINC
      ELSE
         HBEG = (IHIGH/HINC)*HINC
      ENDIF
      CALL HWTOBYTE(HPIC,HBEG,HINC,NL*NS,pic) !Convert image to byte
      ILOW = (ILOW-HBEG)/HINC
      IHIGH = (IHIGH-HBEG)/HINC
      RETURN
      END


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Display the input entire input image (at possibly reduced resolution).
C
      SUBROUTINE HOME(PIC,NL,NS,NLDS,NSDS,sl,ss,izoom,zoom)
      BYTE PIC(NS,NL)
      INTEGER*4 SL,SS

      SL = 1
      SS = 1
      IZOOM = MAX0((NL-1)/NLDS,(NS-1)/NSDS) + 1
      IF (IZOOM.GT.1) THEN
         ZOOM = 1.0/IZOOM
         IZOOM = -IZOOM
      ELSE
         ZOOM = 1.0D0
         IZOOM = 1
      END IF
      CALL DPIC(PIC,SL,SS,NL,NS)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw a curve on the graphics screen.
C All arguments are inputs.
C   IFLAG=1 erase graphics plane before drawing curve, =0 otherwise.
C 
      SUBROUTINE DRAWCURVE(PTS,NPTS,IFLAG)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      REAL*4 PTS(2,NPTS)
      INTEGER SAMP,LINE
      LOGICAL XST,XDIPIXELWRITE,XDIFILL

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      include 'fortport'	! for int2bte
C
      IF (IFLAG.EQ.1) XST=XDIFILL(IDEV,G,XDB)	!Erase graphics plane
C
      DO 10 I=1,NPTS
      RLINE = PTS(1,I)
      RSAMP = PTS(2,I)
      IF (RLINE.LT.0.0) THEN			!Removing these comments
          IF (RLINE.EQ.-99.0) GOTO 10	!will cause obscured
          RLINE = ABS(RLINE)			!parts of curve to be
          RSAMP = ABS(RSAMP)			!displayed as well.
      ENDIF
      LINE = ZOOM*(RLINE-SL) + 1.5
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) GOTO 10
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) GOTO 10
      XST = XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(xdw))
      IF (.NOT. XST) RETURN
   10 CONTINUE
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw boxes around the camera blemishes on the graphics
C screen.  All arguments are inputs.
C 
      SUBROUTINE DRAWBLEMS(RES,BLEM,NBLEM,IRMAX)
      include 'fortport'        ! defines int2byte
      REAL*4 RES(2,202),BLEM(4,1000)

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER X(5),Y(5),SAMP,LINE
      LOGICAL XST,XDIPOLYLINE,XDIPIXELWRITE

      bmdn = int2byte(xdw)

      DO 10 I=1,NBLEM
      M = BLEM(1,I)
      RLINE = BLEM(2,I) + RES(1,M)
      RSAMP = BLEM(3,I) + RES(2,M)
      LINE = ZOOM*(RLINE-SL) + 1.5	!Convert to screen coordinates
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) GOTO 10
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) GOTO 10
      IRADIUS = BLEM(4,I)
      IF (IRADIUS.GT.IRMAX) IRADIUS=IRMAX
      IF (IRADIUS.LE.0) THEN		!If radius=0, draw a dot.
         XST=XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(xdw))
         GOTO 10
      ENDIF
      X(1) = SAMP - IRADIUS	!Define the box
      Y(1) = LINE - IRADIUS
      X(2) = SAMP + IRADIUS
      Y(2) = LINE - IRADIUS
      X(3) = SAMP + IRADIUS
      Y(3) = LINE + IRADIUS
      X(4) = SAMP - IRADIUS
      Y(4) = LINE + IRADIUS
      X(5) = X(1)
      Y(5) = Y(1)
      XST = XDIPOLYLINE(IDEV,G,bmdn,5,X,Y) !Draw it
      IF (.NOT. XST) GOTO 990
   10 CONTINUE
      RETURN

  990 CALL PRNT(4,1,XST,' ***XDIPOLYLINE Error=.')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw the star background on the graphics screen.
C All arguments are inputs.
C   IFLAG=1 erase graphics plane before drawing curve, =0 otherwise.
C 
      SUBROUTINE DRAWSTARS(STARS,SPTS,NSTARS,LOMAG,HIMAG,IFLAG)
      include 'fortport'        ! defines int2byte
      REAL*4 STARS(3,NSTARS),SPTS(2,NSTARS)
      INTEGER LOMAG,HIMAG

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      byte bmdn

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER X(5),Y(5),SAMP,LINE
      LOGICAL XST,XDIPOLYLINE,XDIPIXELWRITE,XDIFILL
C
      bmdn = int2byte(xdw)

      IF (IFLAG.EQ.1) XST=XDIFILL(IDEV,G,XDB)	!Erase graphics plane
C
      DO 10 I=1,NSTARS
      IF (STARS(3,I).GT.FLOAT(HIMAG).AND.STARS(3,I).NE.99.99) GOTO 10 
      IMAG = STARS(3,I)
      RLINE = SPTS(1,I)
      RSAMP = SPTS(2,I)
      LINE = ZOOM*(RLINE-SL) + 1.5		!Convert to screen coordinates
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) GOTO 10
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) GOTO 10
      IF (STARS(3,I).EQ.99.99) THEN  !VRH 8/12/89 Special added stars
          X(1) = SAMP - 2	!Define the box
          Y(1) = LINE - 2
          X(2) = SAMP + 2
          Y(2) = LINE - 2
          X(3) = SAMP + 2
          Y(3) = LINE + 2
          X(4) = SAMP - 2
          Y(4) = LINE + 2
          X(5) = X(1)
          Y(5) = Y(1)
          XST = XDIPOLYLINE(IDEV,G,BMDN,5,X,Y) !Draw it
          GOTO 10
      END IF
      IRADIUS = 4 + LOMAG - IMAG
CBTC      IF (IRADIUS.LT.1) IRADIUS=1
      IRADIUS = min(10,max(1,iradius))
      X(1) = SAMP + IRADIUS		!Draw horizontal line
      Y(1) = LINE
      X(2) = SAMP - IRADIUS
      Y(2) = LINE
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
      IF (.NOT. XST) GOTO 990
      X(1) = SAMP			!Draw vertical line
      Y(1) = LINE + IRADIUS
      X(2) = SAMP
      Y(2) = LINE - IRADIUS
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
      IF (.NOT. XST) GOTO 990
      IF (IRADIUS.GT.2) THEN
         IRADIUS = IRADIUS - 2
         X(1) = SAMP + IRADIUS
         Y(1) = LINE + IRADIUS
         X(2) = SAMP - IRADIUS
         Y(2) = LINE - IRADIUS
         XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
         IF (.NOT. XST) GOTO 990
         X(1) = SAMP + IRADIUS
         Y(1) = LINE - IRADIUS
         X(2) = SAMP - IRADIUS
         Y(2) = LINE + IRADIUS
         XST = XDIPOLYLINE(IDEV,G,BMDN,2,X,Y)
         IF (.NOT. XST) GOTO 990
      ENDIF
      XST = XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(xdb))
   10 CONTINUE
C
      RETURN

  990 CALL PRNT(4,1,XST,' ***XDIPOLYLINE Error=.')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to draw a dot on the graphics screen.
C   If DN=255, the dot is drawn.  If DN=0, the dot is erased.
C 
      SUBROUTINE DRAWDOT(RLINE,RSAMP,DN)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM
      LOGICAL XST,XDIPIXELWRITE

      common/navdv2/xdw,xdb
      integer xdw,xdb

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER SAMP,LINE,DN,lcldn
      include 'fortport'	! for int2bte
C
      LINE = ZOOM*(RLINE-SL) + 1.5	! 0.5 is added for rounding
      SAMP = ZOOM*(RSAMP-SS) + 1.5
      IF (LINE.LT.1 .OR. SAMP.LT.1) RETURN
      IF (LINE.GT.NLDS.OR.SAMP.GT.NSDS) RETURN
      if (dn.eq.255) then
        lcldn = xdw
      elseif (dn.eq.0) then
        lcldn = xdb
      else
        lcldn = dn
      endif
      XST = XDIPIXELWRITE(IDEV,G,SAMP,LINE,int2byte(lcldn))
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routines to overlay a lat-lon grid over the planet
C
C OVERLAY -- Draw lat-lon grid
C OVERLAT -- Draw a parallel at latitude RLAT1
C OVERLON -- Draw a meridian at longitude RLON1
C
      SUBROUTINE OVERLAY1(INT)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      REAL*4 LPTS(2,2000)

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

      real*4 osnl,osns,rl4,rs4

      EPS0 = (RC/(RA*RB))**2
      RA4 = RA**4
      RB4 = RB**4
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),
     &	osnl,osns,1,CONV,NPH,NPV,ind)

      DO 20 J=-90,90,INT	!Draw parallels at INT intervals
      IF (J.EQ.90.OR.J.EQ.-90) GOTO 20
      
      DO 10 I=0,359		! Draw dots at 1 degree intervals
      RLON = I*DTR
      RLAT = J*DTR
      IF (IGEO.EQ.1) THEN
         EPS = EPS0*DSQRT(RA4 + (RB4-RA4)*DCOS(RLON-RLORA)**2)
         RLAT = DATAN(DTAN(RLAT)*EPS)
      ENDIF
      CALL PLAINV(ind,RLAT,RLON,rline,rsamp,
     &		OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 10
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 10
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 10
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,sngl(rline),sngl(rsamp),
     &		0,CONV,NPH,NPV,ind)
	 rline=rl4
	 rsamp=rs4
      ENDIF
      CALL DRAWDOT(sngl(RLINE),sngl(RSAMP),255)
   10 CONTINUE
   20 CONTINUE

      DO 40 J=0,359,INT		!Draw meridians at INT intervals
      RLON = J*DTR
      EPS = (RC/(RA*RB))**2*
     &         DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON-RLORA)**2)

      DO 30 I=-90,90		!Draw dots at 1 degree intervals
      RLAT = I*DTR
      IF (IGEO.EQ.1.AND.IABS(I).LT.90) RLAT=DATAN(DTAN(RLAT)*EPS)
      CALL PLAINV(IND,RLAT,RLON,RLINE,RSAMP,
     &        OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 30
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 30
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 30
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,SNGL(RLINE),SNGL(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	 RLINE=RL4
	 RSAMP=RS4
      ENDIF
      CALL DRAWDOT(SNGL(RLINE),SNGL(RSAMP),255)
   30 CONTINUE
   40 CONTINUE

      CALL LIMBPT(OM,PSC3,PSUN3,SCLON,RLORA,0.D0,0.D0,OAL,OAS,ZSCALE,
     &    SL,SS,NINT(NLDS/ZOOM),NINT(NSDS/ZOOM),0,0,0,1.D0,3.D0,
     &    0,LPTS,NPTS)
      IF (NPTS.GT.0) CALL DRAWCURVE(LPTS,NPTS,0)
      RETURN

C Routine to draw a parallel at latitude RLAT1
      ENTRY OVERLAT(RLAT1)
      EPS0 = (RC/(RA*RB))**2
      RA4 = RA**4
      RB4 = RB**4
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),
     &	osnl,osns,1,CONV,NPH,NPV,ind)
C
      DO 50 I=0,359		! Draw dots at 1 degree intervals
      RLON = I*DTR
      RLAT = RLAT1
      IF (IGEO.EQ.1) THEN
         EPS = EPS0*DSQRT(RA4 + (RB4-RA4)*DCOS(RLON-RLORA)**2)
         RLAT = DATAN(DTAN(RLAT1)*EPS)
      ENDIF
      CALL PLAINV(IND,RLAT,RLON,RLINE,RSAMP,
     &        OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 50
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 50
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 50
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,SNGL(RLINE),SNGL(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	 RLINE=RL4
	 RSAMP=RS4
      ENDIF
      CALL DRAWDOT(sngl(RLINE),sngl(RSAMP),255)
   50 CONTINUE
      RETURN

C Routine to draw a meridian at longitude RLON1
C
      ENTRY OVERLON(RLON1)
      IF (ITYPE.EQ.7) CALL CONVISOS(PROJECT,ICAM,FLOAT(NL),FLOAT(NS),
     &	osnl,osns,1,CONV,NPH,NPV,ind)

      EPS = (RC/(RA*RB))**2*
     &              DSQRT(RA**4 + (RB**4-RA**4)*DCOS(RLON1-RLORA)**2)
C
      DO 60 I=-90,90            !Draw dots at 1 degree intervals
      RLAT = I*DTR
      IF (IGEO.EQ.1.AND.IABS(I).LT.90) RLAT=DATAN(DTAN(RLAT)*EPS)
      CALL PLAINV(IND,RLAT,RLON1,rline,rsamp,
     &               OM,PSC3,RLORA,OAL,OAS,ZSCALE)
      IF (IND.NE.1) GOTO 60
      IF (ITYPE.EQ.7) THEN
         IF (RLINE.LT.1..OR.RLINE.GT.OSNL) GOTO 60
         IF (RSAMP.LT.1..OR.RSAMP.GT.OSNS) GOTO 60
         CALL CONVISOS(PROJECT,ICAM,rl4,rs4,SNGL(RLINE),SNGL(RSAMP),
     &		0,CONV,NPH,NPV,ind)
	 RLINE=RL4
	 RSAMP=RS4
      ENDIF
      CALL DRAWDOT(sngl(RLINE),sngl(RSAMP),255)
   60 CONTINUE
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute a histogram of displayed image area.
C Output: HIS = byte histogram (256 grey-levels)
C Updated: HFLG=1 if histogram exists
C
      SUBROUTINE HISTGEN1(PIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,his,hflg)
      BYTE PIC(NS,NL)
      INTEGER*4 HIS(256),SL,SS,EL,HFLG
      REAL*4 ZOOM

      IF (HFLG.EQ.1) RETURN
      NLO = NLDS/ZOOM + 0.001
      NSO = NSDS/ZOOM + 0.001
      NLO = MIN0(NL-SL+1,NLO)
      NSO = MIN0(NS-SS+1,NSO)
      INC = 1
      IF (IZOOM.LT.0) INC=-IZOOM
      EL = SL + NLO - 1
      CALL ZIA(HIS,256)
      DO L=SL,EL,INC
          CALL HSUB(1,NSO,PIC(SS,L),HIS)
      ENDDO
      HFLG = 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute a histogram of displayed image area.
C Output: HIST = halfword histogram (65536 grey-levels)
C         NFREQ=total number of frequencies
C
      SUBROUTINE HISTGEN2(HPIC,SL,SS,NL,NS,
     &		NLDS,NSDS,IZOOM,ZOOM,hist,nfreq)
      INTEGER*2 HPIC(NS,NL)
      INTEGER*4 HIST(-32768:32767),SL,SS,EL
      REAL*4 ZOOM

      NLO = NLDS/ZOOM + 0.001
      NSO = NSDS/ZOOM + 0.001
      NLO = MIN0(NL-SL+1,NLO)
      NSO = MIN0(NS-SS+1,NSO)
      INC = 1
      IF (IZOOM.LT.0) INC=-IZOOM
      EL = SL + NLO - 1
      CALL ZIA(hist,65536)
      DO L=SL,EL,INC
         CALL HSUB2(HPIC(SS,L),NSO,INC,HIST)
      ENDDO
      NFREQ = ((NLO-1)/INC+1)*((NSO-1)/INC+1)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE HSUB2(BUF,NS,INC,hist)
      INTEGER*2 BUF(1)
      INTEGER*4 HIST(-32768:32767)

      DO I=1,NS,INC
         IDN = BUF(I)
         HIST(IDN) = HIST(IDN) + 1
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to display a histogram on the graphics plane G, starting at
C pixel coordinates (L0,S0).
C
      SUBROUTINE HDISPLAY(IDEV,G,HIS,L0,S0,IHGHT,HBEG,HINC,NSPIKES)
c      IMPLICIT LOGICAL(X)	! ??? (lwk)
      include 'fortport'        ! defines int2byte
      INTEGER IDEV,G,HBEG,HINC
      INTEGER XX(10),YY(10)
      CHARACTER*132 MSG
CBTC      INTEGER HIS(1),S0,X,Y,DX,TIC
      INTEGER HIS(256),S0,X,Y,DX,TIC
      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB
      BYTE BMDN
      LOGICAL XST,XDIPOLYLINE,XDTTEXT

      BMDN = INT2BYTE(XDW)

C          LINC must be a power of two
      N = NSPIKES + 1
      MAXS = HIS(2)          ! INIITIALIZE to first element in set.
C          Scale histogram to max frequency
      DO J=1,N
         MAX = 0
         DO I=2,254
            IFREQ = HIS(I)
            IF (IFREQ.GT.MAX.AND. (IFREQ.LT.MAXS.OR.J.EQ. 1)) MAX=IFREQ
         END DO
         MAXS = MAX
      END DO
C
      HEIGHT = IHGHT
      ZSCALE = 1.
      IF (MAX.GT.1) ZSCALE=HEIGHT/ALOG10(FLOAT(MAX))
      X = S0
      Y = L0
      IDN = HBEG

      DO 50 L=0,255
      TIC = 0
      IF (MOD(L,16).EQ.0) TIC=2
      IF (MOD(L,32).EQ.0) THEN
         WRITE (MSG(1:6),'(I6)') IDN
         XST = XDTTEXT(IDEV,G,X-56,Y+4,1,6,MSG)
         IF (.NOT.XST) CALL XVMESSAGE('HDISPLAY: Error in XDTTEXT',' ')
         TIC = 4
         IDN = IDN + 32*HINC
      ENDIF

      IFREQ = HIS(L+1)
      IF (IFREQ.GT.1) THEN
           DX = ZSCALE*ALOG10(FLOAT(IFREQ))
      ELSE
           DX = 0.0
      ENDIF

      IF(DX.LE.HEIGHT) GOTO 30
      DX = HEIGHT
      XX(1) = X + DX + 2
      YY(1) = Y
      XX(2) = X + DX + 2
      YY(2) = Y
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,XX,YY)
      IF (.NOT.XST) CALL XVMESSAGE('HDSPLY:Error in XDIPOLYLINE',' ')
   30 XX(1) = X - TIC
      YY(1) = Y
      XX(2) = X + DX
      YY(2) = Y
      XST = XDIPOLYLINE(IDEV,G,BMDN,2,XX,YY)
      IF (.NOT.XST) CALL XVMESSAGE('HDSPLY:Error in XDIPOLYLINE',' ')
      Y = Y + 1
   50 CONTINUE
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate a stretch table (STBL) to linearly stretch an image.
C
      SUBROUTINE STRECH(I1,I2,STBL)
      IMPLICIT INTEGER(A-Z)
      integer STBL(1)
C
      IMAX = 255
      D = I2 - I1
      IF (D .EQ. 0) D = 1
      OFFSET = D/2 - IMAX*I1
      K = 0
C
   10 I = (IMAX*K+OFFSET)/D
      IF (I .LT. 0) I=0
      IF (I .GT. IMAX) I=IMAX
      STBL(K+1) = I
      K = K + 1
      IF (K .LE. IMAX) GOTO 10
C
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine stretch limits from the histogram
C
      SUBROUTINE ASTRC2(HIST,NFREQ,LPER,HPER,min,max)
      INTEGER*4 HIST(-32768:32767)
      REAL*4 LPER,HPER

      MIN = -32767
      MAX = 32766

      IF (NFREQ.EQ.0) RETURN

      ISUM = 0
      KOUNT = NFREQ*.01*LPER + 1
      DO WHILE (ISUM.LT.KOUNT)
         ISUM = ISUM + HIST(MIN)
         MIN = MIN + 1
      ENDDO

      ISUM = 0
      KOUNT = NFREQ*.01*HPER+1
      DO WHILE(ISUM.LT.KOUNT)
         ISUM = ISUM + HIST(MAX)
         MAX = MAX - 1
      ENDDO

      IF (MIN.GT.32767) MIN=32767
      IF (MAX.LT.MIN) MAX=MIN
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to read the cursor position and translate from display
C coordinates to image coordinates.
C
      SUBROUTINE CURSOR(RLINE,RSAMP)
      real*8 rline,rsamp

      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/IPIC/IMG,SL,SS,NL,NS,ICODE
      INTEGER*4 SL,SS

      INTEGER LINE,SAMP
      LOGICAL XST,XDCLOCATION

      XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
      RLINE = (LINE-1)/ZOOM + SL
      RSAMP = (SAMP-1)/ZOOM + SS
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Open and initialize the video plane, graphics plane, and cursor.
C Return IND=1 if successful, =0 otherwise.
C
      SUBROUTINE DEVICE(ind)
      IMPLICIT INTEGER (A-W,Y-Z),LOGICAL (X)
      include 'fortport'	! defines int2byte
      COMMON/DEV/IDEV,V1,G1,TB1,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 V1,G1,TB1,STBL
      REAL*4 S,ZOOM
      INTEGER INFO(80),CON(4),XDSVNL,XDSVNS,xdgcolor,xdssection
      LOGICAL XDIFILL,XDCSET

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB

      character*8 gcolor
      integer ncol

      IND = 0
      V1 = 1
      TB1 = 1
      IF (.NOT.XDEACTION(2,2,3)) RETURN	!Define VRDI error action
      IF (.NOT.XDDUNIT(IDEV)) RETURN	!Get VRDI unit number
      IF (.NOT.XDDOPEN(IDEV)) RETURN	!Open device
      IF (.NOT.XDDACTIVATE(IDEV,.TRUE.)) RETURN	!Activate device
      IF (.NOT.XDDINFO(IDEV,1,80,INFO)) RETURN
c      G1 = 4
c      g1 = info(34)	! see below ...

      MAXTB = MIN0(INFO(48),INFO(60))
      NLDS = XDSVNL(IDEV)
      NSDS = XDSVNS(IDEV)
C     ....Set up initial configuration attempt
      CON(1) = 3			!Monochrome
      CON(2) = INFO(12)			!Image Memory Plane size
      CON(3) = 0			!Video Output size
      CON(4) = 0                        !1x1 Aspect ratio

      IF (.NOT.XDDCONFIGURE(IDEV,CON)) RETURN !Lo Res Err

      IF (.NOT.XDIFILL(IDEV,V1,0)) RETURN	!Erase display plane

      nluts = info(3)
      if (nluts.eq.3) then
	nsection = xdssection( idev, n1)
	if (.not. xdglinit( idev, nsection)) return
	do n1 = 1, nluts
          if (.not. xdlramp( idev, n1, nsection )) return
	  if (.not. xdlconnect( idev, 1, n1, nsection, .false. ))
     1     return
	end do
      else
	call xvmessage(' *** device does not support LUTs',' ')
      endif

      igraph = info(30)
      if (igraph.gt.0) then
	g1 = xdsgraph(idev)
	if (.not.xddinfo(idev,35,1,isec)) return
	if (.not.xdgconnect(idev,g1,isec,.false.)) return
	if (.not.xdgon(idev)) return		!Turn G1 on
	if (.not.xdglinit(idev,isec)) return
      else
	g1 = 0
	call xvmessage('*** no Graphics plane available',' ')
	return
       endif

c      IF (.NOT.XDIFILL(IDEV,G1,VAL)) RETURN	!Erase graphics plane
c      R = 255
c      G = 255
c      B = 255
c      IF (.NOT.XDGLCONSTANT(IDEV,SEC,R,G,B)) RETURN  !Set G1 color

CBTC      xdw = xdgcolor(idev,'white')
      xdw = xdgcolor(idev,'cyan')
c      xdw = 255			! above doesn't seem to work ...
      xdb = xdgcolor(idev,'transparent')
      IF (.NOT.XDIFILL(IDEV,G1,xdb)) RETURN	!Erase graphics plane
c      bmdn = int2byte(xdw)
c      bidn = int2byte(xdb)
c      if (.not.xdimfill( idev, g1, bmdn, bidn)) return

      FONT = 30
      IF (.NOT.XDTFONT(FONT)) RETURN		!Set font type
      H = 8
      S = 1.0
      IF (.NOT.XDTSIZE(H,S)) RETURN		!Set text size
      IF (.NOT.XDTROTATE(0.)) RETURN
CBTC      IF (.NOT.XDTROTATE(0)) RETURN
      BLNK = 0
      FORM = 1
      IF (.NOT.XDCON(IDEV,TB1,FORM,BLNK)) RETURN !Turn cursor on
      XST = XDCSET(IDEV,TB1,(1+nsds)/2,(1+nlds)/2)	! Center cursor
      IF (MAXTB.EQ.0) THEN
         CALL XVMESSAGE('***No trackballs available',' ')
         RETURN
      ELSE IF (MAXTB.EQ.1) THEN
         IF (.NOT.XDCAUTOTRACK(IDEV,TB1,TB1,.TRUE.)) RETURN
      ENDIF
      IND = 1
      RETURN
CBTC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C set graphics color - available from LIMBFIT, RINGFIT, & STARFIT
C via GCOLOR parameter
C
      entry isetcolor
C
      call xviparm( 'GCOLOR', gcolor, ncol, ndef, 0)
      if ( ncol .lt. 1) return
C
C get non-blank portion of gcolor
C
      ncol = len(gcolor)
      dowhile ( ncol .gt. 1 .and. 
     &   index(' '//char(0)//char(9), gcolor(ncol:ncol)) .gt. 0)
        ncol = ncol - 1
      enddo
C
C test for valid color
C
      ndef = xdgcolor(idev,gcolor(1:ncol))
      if ( ndef .gt. 0) xdw = ndef
C
      return
CBTC
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine devoff
      COMMON/DEV/IDEV,V1,G1,TB1,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      call xddclose(idev)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine lutwrite(idev,stbl)
      integer stbl(256)
      integer xdlwrite

      do lut = 1, 3
         ierr = xdlwrite(idev,lut,1,stbl)
         if (ierr .ne. 1) call XVMESSAGE('LUTWRITE:Error in xdlwrite',
     1    ' ')
      end do
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dpic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Write image area to video display.  Area starts at (SL,SS).  Size of
C area is function of size of display (NLDS X NSDS) and ZOOM factor.
C
      SUBROUTINE DPIC(PIC,SL,SS,NL,NS)
      IMPLICIT INTEGER(A-V)
      BYTE PIC(NS,NL)
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      COMMON/NAVDV2/XDW,XDB
      INTEGER XDW,XDB

      BYTE BUF(8192)
      INTEGER*2 BUF1(1024,2)
      LOGICAL XST,XDIFILL
      CHARACTER*132 MSG
      REAL*4 EPS/1.001/
C
      WRITE (MSG,9900,IOSTAT=IOSTAT) SL,SS,IZOOM
9900  FORMAT (' (SL,SS)=(',I4,',',I4,')  ZOOM=',I2)
      CALL XVMESSAGE(MSG(2:29),' ')

C     ....Input image area is (SL,SS,NLI,NSI)
      IF (IZOOM.GE.1) THEN
         NLI = NLDS/IZOOM
         NSI = NSDS/IZOOM
      ELSE
         NLI = -NLDS*IZOOM
         NSI = -NSDS*IZOOM
      ENDIF
      NLI = MIN(NLI,NL-SL+1)
      NSI = MIN(NSI,NS-SS+1)

C     ....Output display area is (1,1,NLO,NSO)
      IF (IZOOM.GE.1) THEN
         NLO = (NLI-1)*IZOOM + 1
         NSO = (NSI-1)*IZOOM + 1
      ELSE
         NLO = -NLI/IZOOM
         NSO = -NSI/IZOOM
      ENDIF

      CALL ZIA(BUF,2048)
      CALL ZIA(BUF1,1024)
      XST = XDIFILL(IDEV,G,XDB)
      XST = XDIFILL(IDEV,VID,0)

      IF (IZOOM.EQ.1) THEN
         LL = SL
         DO L=1,NLO
            CALL AVLINE(PIC(SS,LL),L,1,1,NSO)
            LL = LL + 1
         ENDDO
         CALL AVLINE(BUF,1,1,0,0)	!flush buffer
         RETURN
      ENDIF

      IF (IZOOM.LT.1) THEN
         INC = -IZOOM
         LL = SL
         DO L=1,NLO
            CALL MVE(1,NSI,PIC(SS,LL),BUF,INC,1)
            CALL AVLINE(BUF,L,1,1,NSO)
            LL = LL + INC
         ENDDO
         CALL AVLINE(BUF,1,1,0,0)
         RETURN
      ENDIF

      IF (IZOOM.GT.1) THEN
         INC = IZOOM
         LL = SL
         ISW = 1
         CALL MAGS(PIC(SS,LL),BUF1(1,ISW),NSI,INC,NSO)
         DO L=1,NLO-1,INC
            ISW = 3 - ISW
            LL = LL + 1
            CALL MAGS(PIC(SS,LL),BUF1(1,ISW),NSI,INC,NSO)
            CALL MAGLD(BUF1(1,3-ISW),BUF1(1,ISW),BUF,NSO,INC)
            CALL AVLINE(BUF,L,1,INC,NSO)
         ENDDO
         CALL AVLINE(BUF,1,1,0,0)	!flush buffer
         RETURN
      ENDIF

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Routine to magnify the image resolution by interpolating between
C  video lines.
C  Inputs: BUF1,BUF2 - two successive video lines
C  Ouput:  OBUF      - INC interpolated lines
C
      SUBROUTINE MAGLD(BUF1,BUF2,OBUF,NSO,INC)
      IMPLICIT INTEGER(A-Z)
      BYTE OBUF(NSO,INC)
      INTEGER*2 BUF1(NSO),BUF2(NSO)
      include 'fortport'

      NAREA = INC*INC
      DO I=1,NSO
         D1 = BUF1(I)
         D2 = BUF2(I)
         R = INC*D1
         DO J=1,INC
	    VAL = R/NAREA
            OBUF(I,J) = INT2BYTE(VAL)
            R = R - D1 + D2
         ENDDO
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to magnify a video line by a factor of INC
C
      SUBROUTINE MAGS(BUF,OBUF,NSI,INC,NSO)
      IMPLICIT INTEGER(A-Z)
      BYTE BUF(NSI)
      INTEGER*2 OBUF(NSO)
      include 'fortport'

      DN2 = BYTE2INT(BUF(1))
      R = DN2*INC
      II = 1

      DO I=2,NSI
         DN1 = DN2
	 DN2 = BYTE2INT(BUF(I))
         DO J=1,INC
            OBUF(II) = R
            R = R - DN1 + DN2
            II = II + 1
         ENDDO
      ENDDO
      OBUF(II) = R
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c Write from 1 to n lines to a video device.
c All arguments are inputs:
c	buf(insds,inlds)     - input pixel array
c	islds  - starting line of display window
c	issds  - starting sample of display window
c	inlds   - no. of lines to be written
c	insds   - no. of samples per line
c
      subroutine avline(buf,islds,issds,inlds,insds)
      implicit integer(a-w),logical(x)
      byte buf		!Input image area inlds x insds

      common/dev/idev,vid,g,tb,nlds,nsds,zoom,izoom,stbl(256)
      integer*4 vid,g,tb,stbl
      real*4 zoom

      logical xst,xdiawlocation,xdiawset,xdiawwrite

c     ....Up to 100,000 bytes if data will be buffered before writing to
c     ....display device.
      integer lbufsiz
      parameter (lbufsiz=100000)
      byte lbuf(lbufsiz)

      data csl/-999/,css/-999/	!save starting l,s of current display area
      data cnl/-1/,cns/-1/	!save nl,ns of current display area
      data bufptr/1/		!save the current pointer into the local buffer
      data errcnt/0/		!save display error count
      save

c     ....If new display area, or if no more data will fit in buffer
c     ....flush the buffer and start again
      flush = 0
      if (css.ne.issds) flush=1
      if (csl+cnl.ne.islds) flush=1
      if (bufptr+inlds*insds-1.gt.lbufsiz) flush=1
      if (flush.eq.0) goto 50
      if (cnl.gt.0 .and. cns.gt.0) then
         xst = xdiawlocation(idev,vid,le,to,ri,bo)
         xst = xdiawset(idev,vid,css,csl,css+cns-1,csl+cnl-1)
         xst = xdiawwrite(idev,vid,bufptr-1,lbuf)
         if (.not. xst) goto 999
         xst = xdiawset(idev,vid,le,to,ri,bo)
      endif
      csl = islds
      css = issds
      cnl = 0
      cns = 0
      bufptr = 1

c     ....move the data into the local buffer
   50 if (inlds.eq.0 .or. insds.eq.0) return
      call mve(1,inlds*insds,buf,lbuf(bufptr),1,1)
      bufptr = bufptr + inlds*insds
      cnl = cnl + inlds
      cns = insds
      return

  999 if (errcnt.gt.0) return
      call xvmessage('AVLINE: Error in xdiawwrite',' ')
      errcnt = errcnt + 1
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create nav.pdf
PROCESS HELP=*
SUBCMD-DEFAULT MAIN
  PARM INP	TYPE=STRING     COUNT=1
  PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
  PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
  PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
  PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
  PARM INSTITUTE  TYPE=(STRING,4)  COUNT=0:1			DEFAULT=--
  PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
  PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
  PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
  PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
  PARM RES	TYPE=(STRING,120)     COUNT=0:1		DEFAULT=--
  PARM BLEMS    TYPE=(STRING,120)     COUNT=0:1		DEFAULT=--
  PARM SAO      TYPE=(STRING,72) COUNT=0:1 DEFAULT=/project/vgr/sao_idl.str
  PARM OEF	TYPE=(STRING,72) COUNT=0:1 +
	DEFAULT=/project/test_work/testdata/roef.dat
  PARM GSCPFX   TYPE=(STRING,199) COUNT=0:1 DEFAULT=--
END-SUBCMD
SUBCMD DISTOR
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM IMAGE    TYPE=KEYWORD	COUNT=0:1  VALID=IMAGE	DEFAULT=--
  PARM OBJECT	TYPE=KEYWORD	COUNT=0:1  VALID=OBJECT DEFAULT=--
END-SUBCMD
SUBCMD NAV
  PARM LIMB     TYPE=KEYWORD	COUNT=0:1  VALID=LIMB	DEFAULT=--
  PARM RING	TYPE=KEYWORD	COUNT=0:1  VALID=RING   DEFAULT=--
  PARM STAR	TYPE=KEYWORD	COUNT=0:1  VALID=STAR   DEFAULT=--
  PARM ANIMATE	TYPE=KEYWORD	COUNT=0:1  VALID=ANIMATE DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD LIMBFIT
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM SCAN	TYPE=KEYWORD    COUNT=0:1  VALID=SCAN   DEFAULT=--
  PARM TRACE	TYPE=KEYWORD    COUNT=0:1  VALID=TRACE  DEFAULT=--
  PARM SCPTS	TYPE=KEYWORD    COUNT=0:1  VALID=SCPTS  DEFAULT=--
  PARM SAPTS	TYPE=KEYWORD    COUNT=0:1  VALID=SAPTS  DEFAULT=--
  PARM SBLEMS	TYPE=KEYWORD    COUNT=0:1  VALID=SBLEMS DEFAULT=--
  PARM SRES	TYPE=KEYWORD    COUNT=0:1  VALID=SRES   DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1  VALID=(-4:4) DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1  VALID=H	DEFAULT=--
  PARM HIST	TYPE=KEYWORD    COUNT=0:1  VALID=HIST   DEFAULT=--
  PARM SPIKES	TYPE=INTEGER	COUNT=1	  VALID=(0:250)	DEFAULT=3
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2 VALID=(0.:100.) DEFAULT=0.5
  PARM GERASE   TYPE=KEYWORD    COUNT=0:1  VALID=GERASE DEFAULT=--
!CBTC
  PARM GCOLOR	TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
	VALID=(WHITE,CYAN,MAGENTA,YELLOW,RED,GREEN,BLUE,BLACK)
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
  PARM OVERLAY  TYPE=INTEGER    COUNT=0:1 VALID=(1:360) DEFAULT=--
  PARM LATI     TYPE=REAL       COUNT=0:1 VALID=(-90.:90.) DEFAULT=--
  PARM LONG     TYPE=REAL       COUNT=0:1 VALID=(-360.:360.) DEFAULT=--
  PARM LL       TYPE=REAL       COUNT=0:2 VALID=(-360.:360.) DEFAULT=--
  PARM LS       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM EDIT	TYPE=KEYWORD    COUNT=0:1  VALID=EDIT   DEFAULT=--
  PARM PARAMS   TYPE=KEYWORD    COUNT=0:1  VALID=PARAMS DEFAULT=--
  PARM SEF	TYPE=KEYWORD    COUNT=0:1  VALID=SEF    DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
END-SUBCMD
SUBCMD RINGFIT
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM SCAN	TYPE=KEYWORD    COUNT=0:1  VALID=SCAN   DEFAULT=--
  PARM TRACE	TYPE=KEYWORD    COUNT=0:1  VALID=TRACE  DEFAULT=--
  PARM CHI2	TYPE=KEYWORD    COUNT=0:1  VALID=CHI2   DEFAULT=--
  PARM CHI3	TYPE=KEYWORD    COUNT=0:1  VALID=CHI3   DEFAULT=--
  PARM SCPTS	TYPE=KEYWORD    COUNT=0:1  VALID=SCPTS  DEFAULT=--
  PARM SAPTS	TYPE=KEYWORD    COUNT=0:1  VALID=SAPTS  DEFAULT=--
  PARM SBLEMS	TYPE=KEYWORD    COUNT=0:1  VALID=SBLEMS DEFAULT=--
  PARM SRES	TYPE=KEYWORD    COUNT=0:1  VALID=SRES   DEFAULT=--
  PARM SRINGS	TYPE=KEYWORD    COUNT=0:1  VALID=SRINGS DEFAULT=--
  PARM RADIUS   TYPE=REAL       COUNT=0:1  VALID=(1.:999999.) DEFAULT=--
  PARM RING	TYPE=(STRING,1) COUNT=0:1		DEFAULT=--
  PARM MASTER	TYPE=(STRING,1) COUNT=0:1		DEFAULT=--
  PARM EDIT	TYPE=KEYWORD    COUNT=0:1  VALID=EDIT   DEFAULT=--
  PARM ERING	TYPE=KEYWORD    COUNT=0:1  VALID=ERING  DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1  VALID=(-4:4) DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1  VALID=H	DEFAULT=--
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
  PARM RL       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM LS       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM PREDICT  TYPE=REAL       COUNT=0:3               DEFAULT=--
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
  PARM GERASE   TYPE=KEYWORD    COUNT=0:1  VALID=GERASE DEFAULT=--
!CBTC
  PARM GCOLOR	TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
	VALID=(WHITE,CYAN,MAGENTA,YELLOW,RED,GREEN,BLUE,BLACK)
  PARM HIST	TYPE=KEYWORD    COUNT=0:1  VALID=HIST   DEFAULT=--
  PARM SPIKES	TYPE=INTEGER	COUNT=1	 VALID=(0:255)	DEFAULT=5
  PARM PROFILE	TYPE=KEYWORD    COUNT=0:1  VALID=PROFILE  DEFAULT=--
  PARM PHASPROF	TYPE=KEYWORD    COUNT=0:1  VALID=PHASPROF DEFAULT=--
  PARM PARAMS   TYPE=KEYWORD    COUNT=0:1  VALID=PARAMS DEFAULT=--
  PARM SEF	TYPE=KEYWORD    COUNT=0:1  VALID=SEF    DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
END-SUBCMD
SUBCMD PROFILE
  PARM TYPE	TYPE=(STRING,1) COUNT=0:1		DEFAULT=--
  PARM RLIMITS  TYPE=REAL       COUNT=0:2 VALID=(1.:99999999.) DEFAULT=--
  PARM LLIMITS  TYPE=REAL       COUNT=0:2 VALID=(-360.:360.) DEFAULT=--
  PARM LWIDTH   TYPE=REAL       COUNT=0:1 VALID=(0.:360.) DEFAULT=--
  PARM PLOT     TYPE=KEYWORD    COUNT=0:1  VALID=PLOT	DEFAULT=--
  PARM OUTPUT	TYPE=(STRING,60) COUNT=0:1		DEFAULT=--
  PARM PHASE    TYPE=KEYWORD    COUNT=0:1  VALID=PHASE  DEFAULT=--
  PARM CP       TYPE=KEYWORD    COUNT=0:1  VALID=CP	DEFAULT=--
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1  VALID=H	DEFAULT=--
  PARM RL       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM LS       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1  VALID=(-4:4) DEFAULT=--
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
  PARM GERASE   TYPE=KEYWORD    COUNT=0:1  VALID=GERASE DEFAULT=--
  PARM GCOLOR	TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
	VALID=(WHITE,CYAN,MAGENTA,YELLOW,RED,GREEN,BLUE,BLACK)
  PARM HELP     TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD PHASPROF
  PARM PLOT     TYPE=KEYWORD    COUNT=0:1  	DEFAULT=-- +
        VALID=(PLot,Dnplot,Longplot,RAdiplot,EMisplot,Inciplot,PHasplot)
  PARM OUTPUT	TYPE=(STRING,60) COUNT=0:1		DEFAULT=--
  PARM KEYS     TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
        VALID=(CENTERPT, CORNERPT, REFRESH, EDIT) 
  PARM CP       TYPE=KEYWORD    COUNT=0:1  VALID=CP	DEFAULT=--
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1  VALID=H	DEFAULT=--
  PARM RL       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM LS       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1  VALID=(-4:4) DEFAULT=--
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
  PARM GERASE   TYPE=KEYWORD    COUNT=0:1  VALID=GERASE DEFAULT=--
  PARM GCOLOR	TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
	VALID=(WHITE,CYAN,MAGENTA,YELLOW,RED,GREEN,BLUE,BLACK)
  PARM HELP     TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD STARFIT
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM HIMAG	TYPE=INTEGER	COUNT=0:1  VALID=(-3:20) DEFAULT=--
  PARM SSTARS	TYPE=KEYWORD    COUNT=0:1  VALID=SSTARS DEFAULT=--
  PARM SBLEMS	TYPE=KEYWORD    COUNT=0:1  VALID=SBLEMS DEFAULT=--
  PARM SRES	TYPE=KEYWORD    COUNT=0:1  VALID=SRES   DEFAULT=--
  PARM MSTARS	TYPE=KEYWORD    COUNT=0:1  VALID=MSTARS DEFAULT=--
  PARM TIEPOINT TYPE=KEYWORD    COUNT=0:1  VALID=TIEPOINT DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1  VALID=(-4:4) DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1  VALID=H	DEFAULT=--
  PARM HIST	TYPE=KEYWORD    COUNT=0:1  VALID=HIST   DEFAULT=--
  PARM SPIKES	TYPE=INTEGER	COUNT=1	  VALID=(0:250)	DEFAULT=3
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
  PARM GERASE   TYPE=KEYWORD    COUNT=0:1  VALID=GERASE DEFAULT=--
  PARM GCOLOR	TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
	VALID=(WHITE,CYAN,MAGENTA,YELLOW,RED,GREEN,BLUE,BLACK)
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
  PARM RD       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM LS       TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM EDIT	TYPE=KEYWORD    COUNT=0:1  VALID=EDIT   DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
END-SUBCMD
SUBCMD ANIMATE
  PARM LIMB	TYPE=KEYWORD	COUNT=0:1  VALID=LIMB	DEFAULT=--
  PARM RING	TYPE=KEYWORD	COUNT=0:1  VALID=RING   DEFAULT=--
  PARM STAR	TYPE=KEYWORD	COUNT=0:1  VALID=STAR   DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD PARAMS
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM NLW	TYPE=INTEGER	COUNT=1    VALID=(3:11) DEFAULT=11
  PARM NSW	TYPE=INTEGER	COUNT=1	   VALID=(1:11)	DEFAULT=3
  PARM NSEARCH	TYPE=INTEGER	COUNT=1	   VALID=(3:25)	DEFAULT=9
  PARM CHI2	TYPE=KEYWORD    COUNT=0:1  VALID=CHI2   DEFAULT=--
  PARM CHI3	TYPE=KEYWORD    COUNT=0:1  VALID=CHI3   DEFAULT=--
  PARM DBUG	TYPE=KEYWORD    COUNT=0:1  VALID=DBUG   DEFAULT=--
  PARM NODBUG	TYPE=KEYWORD    COUNT=0:1  VALID=NODBUG DEFAULT=--
  PARM STATUS	TYPE=KEYWORD    COUNT=0:1  VALID=STATUS DEFAULT=--
END-SUBCMD
SUBCMD QSCAN
  PARM SCAN	TYPE=KEYWORD    COUNT=0:1  VALID=SCAN   DEFAULT=--
  PARM TRACE	TYPE=KEYWORD    COUNT=0:1  VALID=TRACE  DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD RSEARCH
  PARM OUTER    TYPE=KEYWORD    COUNT=0:1  VALID=OUTER  DEFAULT=--
  PARM INNER    TYPE=KEYWORD    COUNT=0:1  VALID=INNER  DEFAULT=--
  PARM WIDTH    TYPE=REAL       COUNT=0:1  VALID=(0.001:31.0) DEFAULT=--
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD RINGRAD
  PARM RADIUS   TYPE=REAL       COUNT=0:1    VALID=(1.:999999.) DEFAULT=--
  PARM PLANE	TYPE=(STRING,1) COUNT=0:1		DEFAULT=--
  PARM RING	TYPE=(STRING,1) COUNT=0:1		DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD ANGLE
  PARM ANG	TYPE=REAL       COUNT=3  	        DEFAULT=(0.,0.,0.)
END-SUBCMD
SUBCMD RVALUE
  PARM VALUE    TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD IVALUE
  PARM VALUE    TYPE=INTEGER    COUNT=0:1  	        DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD STRING
  PARM STRNG	TYPE=(STRING,60) COUNT=0:1		DEFAULT=--
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD EDIT
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM GEODET	TYPE=KEYWORD    COUNT=0:1  VALID=GEODET DEFAULT=--
  PARM GEOCEN	TYPE=KEYWORD    COUNT=0:1  VALID=GEOCEN DEFAULT=--
  PARM RA       TYPE=REAL       COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
  PARM RB       TYPE=REAL       COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
  PARM RC       TYPE=REAL       COUNT=0:1 VALID=(1.:999999.) DEFAULT=--
!CBTC  PARM RA       TYPE=REAL       COUNT=0:1 VALID=(1.:99999.) DEFAULT=--
!CBTC  PARM RB       TYPE=REAL       COUNT=0:1 VALID=(1.:99999.) DEFAULT=--
!CBTC  PARM RC       TYPE=REAL       COUNT=0:1 VALID=(1.:99999.) DEFAULT=--
  PARM LORA     TYPE=REAL       COUNT=0:1 VALID=(-360.:360.) DEFAULT=--
  PARM CAMERA	TYPE=INTEGER	COUNT=0:1 VALID=(4:7)	DEFAULT=--
  PARM FL       TYPE=REAL       COUNT=0:1 VALID=(1.:99999.) DEFAULT=--
  PARM OAXIS    TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM SC       TYPE=REAL       COUNT=0:1 VALID=(1.:99999.) DEFAULT=--
  PARM SSP      TYPE=REAL       COUNT=0:2 VALID=(-360.:360.) DEFAULT=--
  PARM PC       TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM ISPC     TYPE=REAL       COUNT=0:2  	        DEFAULT=--
  PARM WAPC     TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM WAISPC   TYPE=REAL       COUNT=0:2               DEFAULT=--
  PARM ANGLN    TYPE=REAL       COUNT=0:1 VALID=(-360.:360.) DEFAULT=--
  PARM RANGE    TYPE=REAL       COUNT=0:1 VALID=(1.:9999999999.) DEFAULT=--
  PARM TARGET   TYPE=(STRING,8) COUNT=0:1 		DEFAULT=--
  PARM SOL      TYPE=REAL       COUNT=0:2 		DEFAULT=--
  PARM RLIM     TYPE=REAL       COUNT=0:2 		DEFAULT=--
  PARM ECM	TYPE=KEYWORD    COUNT=0:1  VALID=ECM DEFAULT=--
  PARM EME	TYPE=KEYWORD    COUNT=0:1  VALID=EME DEFAULT=--
  PARM SCVECTOR TYPE=REAL       COUNT=0:3               DEFAULT=--
  PARM STATUS   TYPE=KEYWORD    COUNT=0:1 VALID=STATUS  DEFAULT=--
  PARM SAVE     TYPE=KEYWORD    COUNT=0:1 VALID=SAVE    DEFAULT=--
  PARM RESTORE  TYPE=KEYWORD    COUNT=0:1 VALID=RESTORE DEFAULT=--
  PARM CKNAME     TYPE=(STRING,4)  COUNT=(0:1)		DEFAULT=--
  PARM CKID       TYPE=(STRING,4)  COUNT=(0:1)		DEFAULT=--
!CBTC  PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
!CBTC  PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
  PARM GETSEDR  TYPE=KEYWORD    COUNT=0:1 VALID=GETSEDR DEFAULT=--
  PARM SYSTEM   TYPE=KEYWORD    COUNT=0:1 VALID=(J2000,EME50)	DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1  VALID=(-4:4) DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1  VALID=H	DEFAULT=--
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
  PARM GERASE   TYPE=KEYWORD    COUNT=0:1  VALID=GERASE DEFAULT=--
  PARM GCOLOR	TYPE=KEYWORD    COUNT=0:1  DEFAULT=-- +
	VALID=(WHITE,CYAN,MAGENTA,YELLOW,RED,GREEN,BLUE,BLACK)
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
  PARM OVERLAY  TYPE=INTEGER    COUNT=0:1 VALID=(1:360) DEFAULT=--
  PARM LATI     TYPE=REAL       COUNT=0:1 VALID=(-90.:90.) DEFAULT=--
  PARM LONG     TYPE=REAL       COUNT=0:1 VALID=(-360.:360.) DEFAULT=--
END-SUBCMD
SUBCMD ECM
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM ALPHA    TYPE=REAL       COUNT=0:1               DEFAULT=--
  PARM DELTA    TYPE=REAL       COUNT=0:1               DEFAULT=--
  PARM KAPPA    TYPE=REAL       COUNT=0:1               DEFAULT=--
END-SUBCMD
SUBCMD EME
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM ALPHA    TYPE=REAL       COUNT=0:1               DEFAULT=--
  PARM DELTA    TYPE=REAL       COUNT=0:1               DEFAULT=--
  PARM OMEGA    TYPE=REAL       COUNT=0:1               DEFAULT=--
END-SUBCMD
SUBCMD ERING
  PARM EXIT	TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM RING	TYPE=(STRING,1) COUNT=0:1		DEFAULT=--
  PARM STATUS   TYPE=KEYWORD    COUNT=0:1 VALID=STATUS  DEFAULT=--
  PARM SMAA     TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM ECC      TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM INCL     TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM PIZERO   TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM OMEGZ    TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM DW       TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM DOMEGZ   TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM RA       TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM DEC      TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM THETA    TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM ZETAZ    TYPE=REAL       COUNT=0:1  	        DEFAULT=--
  PARM RESTORE  TYPE=KEYWORD    COUNT=0:1 VALID=RESTORE DEFAULT=--
  PARM WRITE	TYPE=KEYWORD    COUNT=0:1  VALID=WRITE  DEFAULT=--
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
END-SUBCMD
SUBCMD TRACECRV
  PARM D        TYPE=KEYWORD    COUNT=0:1 VALID=D	DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1 VALID=H	DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1 VALID=(-4:4)  DEFAULT=--
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
  PARM HELP	TYPE=KEYWORD    COUNT=0:1  VALID=HELP   DEFAULT=--
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
END-SUBCMD
SUBCMD ACLEAN
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM CLEAN	TYPE=REAL	COUNT=0:1  VALID=(1.0:999.0) DEFAULT=--
  PARM H        TYPE=KEYWORD    COUNT=0:1 VALID=H	DEFAULT=--
  PARM CZOOM    TYPE=INTEGER    COUNT=0:1 VALID=(-4:4)  DEFAULT=--
  PARM STRETCH  TYPE=INTEGER    COUNT=0:2 VALID=(-32768:32767) DEFAULT=--
  PARM ASTRETCH TYPE=REAL       COUNT=0:2		DEFAULT=0.5
END-SUBCMD
SUBCMD READY
  PARM EXIT     TYPE=KEYWORD    COUNT=0:1  VALID=EXIT   DEFAULT=--
  PARM QUIT     TYPE=KEYWORD    COUNT=0:1  VALID=QUIT   DEFAULT=--
END-SUBCMD
SUBCMD CONTINUE
  PARM C        TYPE=KEYWORD    COUNT=0:1  VALID=C      DEFAULT=--
END-SUBCMD
SUBCMD SKIP
  PARM S        TYPE=KEYWORD    COUNT=0:1  VALID=S      DEFAULT=--
END-SUBCMD
SUBCMD QUERRY
  PARM Y        TYPE=KEYWORD    COUNT=0:1  VALID=Y      DEFAULT=--
  PARM N        TYPE=KEYWORD    COUNT=0:1  VALID=N      DEFAULT=--
END-SUBCMD
SUBCMD HST
  PARM SCLAT    TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM SCLON    TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM SUNLAT   TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM SUNLON   TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM CENTLINE TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM CENTSAMP TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM RANGE    TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM FOCAL    TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM SCALE    TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM NORTH    TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM OALINE   TYPE=REAL COUNT=(0:1) DEFAULT=--
  PARM OASAMP   TYPE=REAL COUNT=(0:1) DEFAULT=--
END-SUBCMD

END-PROC
.TITLE
Image Navigation Program - NAV
.HELP
PURPOSE

NAV is an interactive program which refines the camera pointing (C-matrix)
of an image by locating one or more distinct features in the image.  Features
used include the limb, ring-plane, and star background.  The measured feature
positions are used in combination with navigation data retrieved from the
project SEDR or SPICE file to compute the C-matrix.  The C-matrix is then
used to compute the planet-center and (optionally) the north-angle.  The
computed C-matrix may be subsequently used in two ways:

    1) The C-matrix may be stored in the SEDR or SPICE file and subsequently
       retrieved by map-projection, mosaicking, or data analysis software.

    2) The planet-center (and optionally the north-angle) may be inserted as
       parameters to these latter software.

NAV contains interfaces to the Voyager, Galileo and Cassini SPICE kernels only.
For all other missions (e.g. Space Telescope) the navigation data typically
provided by SPICE must be manually input.

Note that NAV obtains the default radii and rotation rate of the target body from
a SPICE PC kernel that is labelled GLL_CONSTANTS in the SPICE_CONFIG_FILE,
regardless of the project actually in use.  Be sure that the SPICE_CONFIG_FILE
in your system contains an entry for GLL_CONSTANTS if you wish to use those
defaults.

NAV uses an east-longitude system.  For Voyager, latitudes are input and output
to the user as planetographic.  Planetocentric latitudes are used for all other
missions.  This may be modified by using the GEOCEN and GEODET commands.

NAV assumes that the image has been acquired via a camera system with a
shutter and focal plane (e.g. vidicon/CCD sensor).  See also FARENC, MANMATCH,
and NAV2.

.PAGE
EXECUTION

(Note that NAV, like all interactive VICAR programs, cannot be run from
the Unix shell -- it must be run under TAE/VICAR.)

	USE  EPA2		!Allocate a display device
	NAV  PIC

where PIC is the image to be navigated.  PIC must be a Voyager, Galileo or
Cassini image in either byte or halfword (16-bit integer) data format.  The
frame must be in image-space or object-space (no picture size reduction or
map-projection allowed).

Voyager images should be 800x800 for image-space frames and 1000x1000 for
object-space frames.  Galileo images are always in image-space and are
800x800 for full-frame images and 400x400 for summation-mode images.  Cassini
images can be 1024x1024, 512x512 or 256x256 and are always in image-space.

NAV will not operate on halfword images which contain binary labels (e.g.
Galileo EDR).  Use program COPY to strip off the binary labels prior to
input to NAV.

NAV requires a page file quota slightly larger than normal.  The program
is known to fail on a quota of 8000 pages and to work on a quota of
13000 pages.

Upon entry, the user will be prompted to input commands.

.page
GEOMETRIC CAMERA DISTORTIONS

Both images which contain geometric camera distortions (image-space frames)
and images which have been geometrically corrected (object-space frames)
may be input.  If a Voyager image-space frame is used, then the reseau locations
for the image must be input via the RES parameter:

  Example: RESLOC  (PIC,RESFILE)  R
           NAV  PIC  RES=R
    The reseau locations are located via the Voyager program RESLOC
    and input to NAV via the RES parameter.

NAV corrects for the Galileo SSI and Cassini WAC camera distortion using a
built-in radial distortion model.

RADIOMETRIC CAMERA DISTORTIONS

It is not necessary to correct the image for radiometric distortions
prior to input to NAV.  It will be desirable to do so, however, if the
PROFILE option is used to gather ring-profile data.

If the image is radiometrically corrected, care should be taken
to ensure that the dark-current subtraction does not truncate the limb
or ring, as this would lead to incorrect results when scanning for
these features.  This problem may be avoided by inputing the halfword image
(as output by FICOR77 or GALSOS) rather than the rescaled byte image (as
output by FIT).

.page
OPERATION

Upon entry, the program will display the input image on the monitor,
adjusting the resolution, as necessary, to display the complete field
of view.  If the image is in halfword data format, the program auto-
matically determines the dynamic range of the input DN values,
stretching the display to saturate 0.5 percent of the data at the low
and high ends of the DN range (see ASTRETCH parameter).

Geometric data is retrieved from SPICE kernels and the
camera pointing is reconstructed using the best available information.

The camera pointing is broken down into 3 angular components: ANGLN,
the orientation of the projected spin axis, and ANGLA and ANGLB, 
which produce offsets within the image plane.

The user is prompted to specify whether limb, ring, or star navigation
is desired:

    Specify feature to be fitted (enter 'LIMB, 'RING, 'STAR or 'EXIT)>

Typing 'LIMB, 'RING, or 'STAR passes control to the LIMBFIT, RINGFIT, or
STARFIT routines, respectively.  Typing EXIT will return control to TAE,
with a summary of the current camera pointing printed out.

Prior to exiting, the user is prompted as to whether the SEDR should
be updated.  If the user responds 'Y, then an updated C-matrix is
stored in the SEDR.  The OM-matrix, RS-vector and (line,sample) coordinates
of the planet center are also stored in the SEDR.

.page
WHAT TO DO IF SPICE KERNELS ARE NOT AVAILABLE

If the input image is from a mission other than Voyager, Galileo or Cassini,
the navigation data typically retrieved from the SEDR or SPICE files must be
manually input by the user.  First, the target body should be input in the
execution statement:
	NAV  INP=PIC  TARGET=SATURN
When the program issues the message "Unrecognizable project.  Enter project ID",
type NONE.

Enter the LIMBFIT routine by selecting 'LIMB and enter the following navigation
data via the 'EDIT command (see below): RANG,SSP,ANGLN,FL,SC,SOL,PC.  If SOL
is not known, set it equal to SSP.  Use your best guess for PC and enter it
last.  Type 'STATUS to display the current navigation data.

Type OVER=30 to draw a latitude-longitude grid of where NAV thinks the target
is located in the image.  Make any scale adjustments by changing FL, SCALE, or
RANG.

Adjust the planet center as follows: Exit from EDIT to get back to the
LIMBFIT prompt.  Type 'SCAN and manually move the graphics limb so that it
registers to the actual limb.  If everything looks OK, proceed with a limb
scan.  Otherwise re-enter 'EDIT and fiddle with the navigation data.

For images of planetary rings, the RINGFIT routine may be similarly used.

.PAGE
LIMBFIT ROUTINE

The LIMBFIT routine navigates the image by locating the planet limb.
The limb may be (1) located manually by tracing the limb via the cursor
or (2) automatically located by performing a radial scan about an
approximate planet-center.

The LIMBFIT command processor is identified by the prompt:

	LIMBFIT>

To obtain a list of the commands supported by the LIMBFIT routine,
type HELP:

	Enter EXIT,SCAN,TRACE,SCPTS,SAPTS,SBLEMS,SRES,CZOOM,H,HIST,STRETCH,
        ASTRETCH,GERASE,GCOLOR,C,OVERLAY,LATI,LONG,LL,LS,SPIKES,EDIT,PARAMS>

Entering SCAN will cause a limb scan.  See LIMBSCAN ALGORITHM below.

Entering TRACE will enable the user to trace the limb.  See
LIMBTRACE ALGORITHM below.

Entering SCPTS will display the computed limb (overlayed in graphics).

Entering SAPTS will display the acquired points (overlayed in graphics).

Entering SBLEMS will display the locations of the camera blemishes
(overlayed in graphics).  The blemishes are automatically retrieved from
blemish files located in WMS_VGR:[VGR1.NA], WMS_VGR:[VGR1.WA], and
WMS_VGR:[VGR2].  The user may optionally specify the location of the 
blemish file via the BLEMS parameter.  The program checks to ensure that 
the camera serial number associated with the file (as extracted from the 
label) matches the camera serial number associated with the image.

Entering SRES will display the reseau locations (overlayed in graphics).
If the reseau has not been removed, this display will give an indication
of how accurately the reseau has been located (and consequently the
accuracy of the geometric correction).

Entering CZOOM=2 will center the display about the current cursor
position and magnify the image to twice normal resolution. 
Pixel interpolation is performed during magnification.

Entering H will redisplay the original image (displayed at 1/2
resolution).

Entering HIST will cause the histogram (DN vs frequency) of the
displayed image area to be overlayed in graphics.  Note that a
logrithmic scale is used to display frequency.

Entering SPIKES=3 will scale the frequencies of the histogram so
that the third largest freqency corresponds to maximum scale.

Entering STRETCH=(20,100) will perform a hardware stretch (linear)
of the displayed image.

Entering ASTRETCH=(0.5,1.5) will perform an "ends-in" stretch:  The
histogram of the displayed image area is first computed.  Low and
high stretch limits are then chosen so as to saturate 0.5 and
1.5 percent of the data at the low and high ends of the histogram
(respectively).  The algorithm is described in detail in the
help information for VICAR program FIT.

Entering GERASE will erase the graphics plane.

Entering C will cause the (line,sample) and (lat,lon) or (radius,lon)
of the current cursor position to be reported.  Subsequent Returns
will repeat this until EXIT is entered.  Control then returns to
LIMBFIT or RINGFIT.

Entering OVERLAY=15 will cause a latitude-longitude grid at 
15 degree spacing to be overlayed on the planet.  Note that the
latitudes used are planetographic, unless the user has specified
planetocentric via the GEOCEN command.

Entering LATI=35.4 will cause a parallel at 35.4 degrees latitude
to be overlayed on the planet.

Entering LONG=115.0 will cause a meridian at 115 degrees
longitude to be overlayed on the planet.

LL and LS convert from latitude-longitude to line-sample and vice-versa.
When latitude-longitude coordinates LL=(23.3,130) are entered, the
program reports the corresponding line-sample coordinates.
When line-sample coordinates LS=(300,451.3) are entered, the program
reports the corresponding latitude-longitude coordinates.

Entering EDIT will enable the user to edit the navigation data.
See EDIT COMMAND below.

Entering PARAMS will enable the user to modify some obscure
processing parameters.  See PARAMS COMMAND below.

The SEF command is for program development only.  Don't try it.


.PAGE
LIMBSCAN ALGORITHM

STEP1: The planet outline is computed (CPTS) and drawn in graphics,
based on the nominal camera pointing (from the SEDR).  The limb
appears as a solid curve, with limb points spaced approximately two
pixels apart.  The unilluminated portion of the planet outline is
displayed as a broken line, with points spaced 8 pixels apart.

STEP2: The user is given an opportunity to manually improve the
camera pointing by moving the computed limb (drawn in graphics)
so that it registers with the actual limb (displayed on the video
plane):

	Move Cursor to a point on computed limb
        Hit Return when ready or type EXIT>

If the user is satisfied with the current pointing, he may skip
this step by typing EXIT.  Otherwise, use the trackball to select
a point on the limb and hit Return.

	Move Cursor to actual limb
	Hit Return when ready or type EXIT if done>

The user specifies the corresponding point on the actual limb and
enters Return.  The cursor displacement is used to move
the graphics limb to the new position.  The camera pointing is
updated in the process.

Any subsequent cursor movement followed by Return will produce
additional displacements of the graphics limb.  Typing EXIT will
transfer control to the next step.

STEP 3: A limb scan is performed, starting at each point on the
computed curve and scanning in a direction normal to the
limb, searching for a high-contrast point. 

	Do you wish to do a limb scan? (Enter Y or N)>

If the user types N, then the limb scan step is skipped and
control is returned to the LIMBFIT prompt with the manually
corrected pointing from STEP 2 in effect.

If the user types Y, a limb scan is performed.  Prior to the
scan, the user may limit the image area involved in the scan.

The limb is located by scanning iteratively for high-contrast
points, with each subsequent iteration using a smaller pixel spacing
between limb points, a smaller search radius, and smaller search steps:

    a) A limb scan is performed using an 11x1 correlation window
       and one-pixel search steps in a direction normal to the
       computed limb.  The length of the correlation window and 
       the search radius may be controlled via the NLW and NSEARCH
       parameters below.  The default search radius is 11 pixels.
       This implies that the user should manually register the
       graphics limb to within 10 pixels (see STEP 2) before
       beginning a limb scan.

       In performing the scan, limb points obscured by the planet's
       ring or ring shadow are avoided.

       If no high-contrast points are found, the LIMBSCAN algorithm
       is terminated and control returns to the LIMBFIT prompt.

    b) The high-contrast points acquired via the limb scan are
       stored in an an array APTS and displayed in graphics (after
       erasing the computed limb CPTS).  The program automatically
       deletes isolated points and points near reseau marks. 

    c) The positional differences between the computed limb points
       and the located high-contrast points are minimized in a
       least-square sense, resulting in line-sample offsets (DL,DS)
       and (if CHI3 is specified) rotation offset DT.

    d) Camera orientation angles ANGLA and ANGLB are corrected
       based on offsets (DL,DS).  ANGLN is corrected by rotation 
       offset DT.  The camera orientation matrix OM is then
       recalculated.

    e) The limb points CPTS are recomputed using the new camera
       orientation matrix.

Steps a) thru e) are repeated until no significant change occurs in
ANGLA and ANGLB.  For the second scan, the pixel spacing between limb
points is reduced to 1 pixel, the search step is reduced to 1/2 pixel,
and the search radius is reduced to 7 pixels.  

After each scan, the (line,sample) position of the planet center
is calculated using the updated pointing.

A final scan is performed using an 11x3 correlation window and a
3 pixel search radius.  Prior to the final fit, the user is
given an opportunity to delete any questionable points:

	Do you wish to clean the curve? (Enter Y or N)>

If the user types Y, control will be passed to a subcommand loop
identified by the prompt:

	Enter CLEAN, H, CZOOM, STRETCH, ASTRETCH or EXIT>

Entering CLEAN=5 will permit the user to delete all acquired high-
contrast points within a 5 pixel radius of the cursor position.

Entering EXIT will transfer control back to the LIMBSCAN routine
and a final fit is performed.

Upon completion of steps 1-3, control is passed back to the LIMBFIT
routine.

.PAGE
LIMBTRACE ALGORITHM

STEP1: The planet outline is computed and stored in array CPTS
(not displayed).  If no limb points are found, the following message
is printed:
		" ***Limb is not in picture"

and control is returned to the LIMBFIT prompt.  If the limb is indeed
in the picture, then something is wrong with the nominal pointing
information.  The user should enter the EDIT command to revue the
navigation data.  If the navigation data is correct, try changing
the planet center.

STEP2: The user traces the limb using the cursor.  This is performed
in a command loop identified by the prompt:

	TRACECRV>

To trace the limb, select limb points via the cursor and enter RETURN
to record the point.  Repeat this until done.  Then type EXIT to
continue to the next step.  The traced limb points are stored in the
array APTS.

Note that limb points may be selected even if they are obscured by the
planet ring or ring shadow.  However, be careful not to select points
on the unilluminated side of the planet.

Enter HELP to obtain a list of the commands available at this prompt:

	Enter D,H,CZOOM,STRETCH,ASTRETCH,HELP,EXIT

To delete a point, move the cursor over the point and enter D.

The commands H,CZOOM,STRETCH, and ASTRETCH enable the user to adjust the
display during the limb trace.

At least 3 limb points must be selected.  If you fail to do so, the
following message is printed:

	" ***Insufficient number of points specified"
	" ***Limb trace terminated"

and control is returned to the LIMBFIT prompt.

STEP3: The positional differences between the computed limb points
CPTS and the traced limb points APTS are minimized:

	Specify type of fit (Enter CHI2 or CHI3)>

Entering CHI2 will minimize differences between the computed
limb (CPTS) and the traced limb (APTS) by correcting for
translation errors in the image.  Angles ANGLA and ANGLB of the
camera orientation matrix (OM) are updated.

Entering CHI3 will correct for rotation and translation errors.
Angles ANGLA, ANGLB, and ANGLN (the north angle) of the OM
matrix are updated.  Selection of CHI2 or CHI3 has been moved to
the PARAMS option for LIMBFIT.

.PAGE
RINGFIT ROUTINE

The RINGFIT routine navigates the image by locating a ring of specified
radius.  The ring may be (1) located manually by tracing the ring via the
cursor or (2) automatically located by performing a radial scan about an
approximate planet-center.

The RINGFIT routine is identified by the prompt:

	RINGFIT>

The operation of the RINGFIT command processor is very similar
to that of LIMBFIT.  See LIMBFIT COMMAND above for a more complete
description of the available commands.

Entering RADIUS=117294 will cause a ring at that radius to be
displayed in graphics.  The ring is assumed to lie in the equatorial
plane of the planet.

For Uranus, the orbital elements of the ten known rings have been
stored within the program.  These orbital elements define the inclined-
plane eccentric-ring geometry of each ring.  The ten rings are identi-
fied within the program by the following nomenclature:

	6 = ring 6		N = eta ring
	5 = ring 5		G = gamma ring
	4 = ring 4		D = delta ring
	A = alpha ring		L = lambda ring (1986U1R)
        B = beta ring		E = epsilon ring

Entering RING=E will cause the epsilon ring to be displayed in
graphics.

Entering SRINGS will cause all nine rings to be displayed in graphics.

Entering ERING will enable the user to edit the orbital elements for
the nine Uranian rings (see ERING COMMAND below).

The MASTER command is used to specify the reference coordinate system
for making ring measurements.  When MASTER=D is entered, for example,
all (radius,longitude) measurements made using the cursor command C
are with respect to the inclined plane of to Delta ring, with zero
longitude at periapse.  When MASTER=P is entered, all measurements are
made in the planet's equatorial plane, with zero longitude defined to
be the intersection between Uranus' and Earth's mean equator 1950 
(ascending node) on 10 March 1977.

The RING, SRINGS, ERING, and MASTER commands work for Saturn and Neptune
as well.  The Saturn rings are identified as follows:

        F = F ring                   K = Keeler gap outer edge
        E = Outer edge Encke gap     A = Inner A ring edge
        N = Cassini inner edge of outer ring
        S = Cassini inner edge of 4th outer ring
        W = Outer B feature          B = Inner B ring edge
        M = Maxwell gap outer edge   R = Mid C ring feature
        T = Titan gap outer edge     C = Inner C ring edge 

For Saturn, the zero longitude is defined to be the intersection between
Saturn's and Earth's equator (ascending node) at EME 1950.

For Neptune, nine test orbits have been defined, the first three with
polar orbits and the remainder with equatorial orbits.  These rings are
identified as follows:
    
          Polar rings      New Satellites        Previous
	A =  50000 Km	   5 = 1989N5  orbit     T = Triton orbit
	B = 100000 Km	   3 = 1989N3            N = Nereid orbit
	C = 150000 Km	   4 = 1989N4
			   2 = 1989N2
			   1 = 1989N1

For Neptune, the zero longitude is defined to be the intersection between
Neptune's and Earth's mean equator 1950 (ascending node) at time of closest
approach.

Note that the reference systems for Jupiter's rings still uses the longitude
system of the rotating planet.

Entering SCAN will cause a ring scan.  See RINGSCAN ALGORITHM below.

Entering TRACE will enable the user to trace the RING.  See RINGTRACE ALGORITHM
below.

Entering PROFILE will enable the user to obtain a radial or longitudinal
plot of a specified sector of the ring plane.  See PROFILE ROUTINE below.

Entering 'PHASPROF will enable the user to obtain a constant-phase 
plot of a specified area of the image.  See PHASPROF ROUTINE below.

RL and LS convert from Radius-longitude to line-sample and vice-versa.
When Radius-longitude coordinates RL=(85000.,130) are entered, the
program reports the corresponding line-sample coordinates.
When line-sample coordinates LS=(300,451.3) are entered, the program
reports the corresponding radius-longitude coordinates for the ring
system that is currently used.

Command PREDICT is to predict the position of an object in one frame
from its position in another.  There are two modes of operation: Predicting
the position of object in another frame from its current position (cursor
is used) or predicting the position of an object in the current frame
(cursor is placed) from its position in another frame.  Predict only
understands the Frame/Time relationship for Voyager and Cassini.

PREDICT=FRAME: The cursor position is read to determine the object's
current MASTER system longitude.  Then its longitude in FRAME is predicted.
Only the longitude is derived from the cursor (the MASTER ring plane orbit
is used) , for MASTER=P then the radius is also taken from the cursor position.

PREDICT=(FRAME,RADIUS):  The cursor position is read to determine the object's
current MASTER system longitude.  Then its longitude in FRAME is predicted
by assuming a circular orbit at RADIUS.

PREDICT=(FRAME,LONGITUDE): The position in the current frame is predicted
from the given LONGITUDE at another FRAME.  If in present field, the cursor
is placed there. Uses orbit defined by MASTER ring.  IF MASTER=P one must
also specify radius.

PREDICT=(FRAME,LONGITUDE,RADIUS): Same as above but for a circular orbit
in the MASTER plane.

.PAGE
RINGSCAN ALGORITHM

The RINGSCAN algorithm is very similar to the LIMBSCAN algorithm.

STEP1: The user is first prompted to enter a ring radius or ring
name:
	Enter ring radius (km) [and plane] or ring name...
        Enter RADIUS, PLANE, RING or 'EXIT>

Entering RADIUS=5433.3 will cause the resulting scan to be performed
on a ring of radius 5433.3.  The ring is assumed to be in the
master plane and to have no eccentricity.  If another plane is wanted,
on the same line one also types PLANE=[ring plane name].  Then the 
scan will be performed on a non-eccentric ring of radius 5433.3 in
the specified ring plane.

Entering RING=E will cause a scan to be perfromed on the epsilon ring
of Uranus.

RINGFIT computes the ring points for the specified ring or radius
and draws the curve in graphics.

Entering 'EXIT goes to the next step.

STEP2: In order to do the ring scan, the program must know which
type of edge function to use:

	Specify edge type (Enter OUTER, INNER or WIDTH)>

If the width of the ring is greater than 19 pixels, the scan should
be performed on the inner or outer edge of the ring.  For thin rings,
specify its width (in pixels): E.g. WIDTH=11 will match the ring
profile to a gaussian with sigma=11/4.

STEP3: The user manually moves the curve over the actual ring
exactly as in the LIMBSCAN algorithm.

The ring scan proceeds in exactly the same way as in the LIMBSCAN
algorithm.  In performing the scan, ring points located in front
or in back of the planet or in the planet's shadow are avoided.

Upon completion of steps 1-3, control is passed to the RINGFIT
prompt.

.PAGE
RINGTRACE ALGORITHM

The RINGTRACE algorithm is similar to the LIMBTRACE algorithm
(above) except that the user is first prompted to enter a ring
radius or ring name:

	Enter ring radius (km) [and plane] or ring name...
        Enter RADIUS, PLANE, RING or 'EXIT>

Entering RADIUS=5433.3 will cause the resulting scan to be performed
on a ring of radius 5433.3.  The ring is assumed to be in the
master plane and to have no eccentricity.  If another plane is wanted,
on the same line one also types PLANE=[ring plane name].  Then the 
scan will be performed on a non-eccentric ring of radius 5433.3 in
the specified ring plane.

Entering RING=E will enable tracing of the epsilon ring of Uranus.

After tracing the ring, typing 'EXIT will return the user back to
the prompt:

	Enter ring radius (km) [and plane] or ring name...
        Enter RADIUS, PLANE, RING or 'EXIT>

The user may trace additional rings (up to 10 are permitted).  Upon
completion, the routine will simultaneously fit all rings traced,
computing a new C-matrix.  This C-matrix replaces the C-matrices
for all rings traced.

.page
PROFILE ROUTINE

Entering 'PROFILE at the RINGFIT prompt passes control to the PROFILE routine,
which is identified by the prompt:
		PROFILE>
The PROFILE routine is used to obtain radial or longitudinal plots of the
ring-plane (coincident with the Master ring plane).   Each time the
PROFILE prompt is issued, the specified profile type (radial or longitudinal),
radius limits, and longitude limits are reported.  Enter any missing information
to obtain a plot.

Entering 'HELP will display a list of commands available at this prompt:
    Enter TYPE, RLIMITS, LLIMITS, LWIDTH, 'PLOT, OUTPUT, PHASE, 'CP, 'C, 'H,
    RL, LS, CZOOM, STRETCH, 'GERASE, GCOLOR, or 'EXIT.

Entering TYPE=R causes a radial plot of the specified sector to be computed and
displayed.  The radial plot (DN vs radius) is computed by averaging all pixels
at a constant radius and lying between the starting and ending longitudes of the
sector.  Bilinear interpolation is used.

Enter TYPE=L to select a longitudinal plot.  Plot consists of DN averaged
over lines of constant longitude.

The ring-sector to be plotted is specified by the RLIMITS and LLIMITS commands.
Entering RLIMITS=(47600,48450) LLIMITS=(253,254) specifies the minimum and
maximum radius and longitude, respectively, of the desired sector.

Enter TYPE=D to select a diagonal radial plot, which is a radial plot centered
on a specified diagonal line.  Specify the end points of the diagonal line by
entering the end points (R1,L1) and (R2,L2) as RLIMITS=(R1,R2) LLIMITS=(L1,L2).
An additional parameter, LWIDTH (the width of the scan in degrees) is required
for diagonal plots.

This plot is useful in cases where the image does not contain the same range
of ring longitude at the ends of a desired ring radial plot, this method
allows one to still get a radial plot.  Instead of having a ring sector with
the same longitudinal limits at each end, the plot is done along a diagonal
line averaging over a constant longitudinal width (defined by the inputted
LWIDTH parameter in degrees).

Enter TYPE=S to select a Longitudinal smear plot, which is a longitudinal plot
but averaged not over a range of radii, but along a smear direction and smear
length.  You specify the longitude limits with LLIMITS=(L1,L2).  You then
specify the smear direction with the cursor on some smeared object, and then
specify the smear length (and center radius) using the cursor on the ring to
be plotted.

This is useful for cases where one wants to do a plot along longitude but for
which the image is smeared and lines of constant radius are not consistant
with the smear.  This allows one to define a smear direction (using a star for
example) and to define a smear length (to plot over).  Plot is done by
averaging DN along this line rather than along lines of constant radius.

Whenever new values are entered for TYPE, RLIMITS, or LLIMITS, or whenever
'PLOT is specified, a new plot is generated.

The 'C command may be used to read the (radius,longitude) coordinates of any
cursored position on the display.  This is useful in determining the desired
ring-sector limits.  The image display may be adjusted via the 'H, CZOOM,
STRETCH, and 'GERASE commands.

The plot is displayed over the image in graphics.  The (radius,DN) or
(longitude,DN) values of the plotted points may be read with the cursor via
the 'CP command.

Entering OUTPUT=PLOT.DAT will generate an ASCII file of record length 31 bytes,
containing the longitude, radius, and DN value for each longitude (or radius)
of the plot.  If the 'PHASE keyword is appended to the SAME COMMAND LINE as
OUTPUT, the ASCII file will include the phase angle at each plotted point (and
the record length will be 37 bytes).

.page
PHASPROF ROUTINE

Entering 'PHasprof at the RINGFIT prompt passes control to the PHASPROF routine,
which is identified by the prompt:

		Constant-Phase Profiling>

The PHASPROF routine is used to obtain constant-phase plots of the
ring-plane (coincident with the Master ring plane).   

Entering 'HELP will display a list of commands available at this prompt.

The commands are

  'CEnterpt, 'COrnerpt, 'REFresh, 'EDIT,
  'PLot, 'Dnplot, 'Longplot, 'RAdiplot, 'EMisplot, 'Inciplot, 'PHasplot,
  OUTPUT=<filespec>, 'CP, 'C, 'H, RL=(radius,longitude), LS=(line,sample),
  CZoom=<iz>, STretch=(hidn,lowdn), ASTRETCH=(hi%,low%), 'GERASE, 
  GCOLOR=<color> (or '<color>) 'Help, 'EXit

Positioning the cursor & entering 'CEnterpt causes the cursor position to be
saved as the center point of a constant-phase profile scan.  A dotted line 
will appear; it will be the centerline of the profile scan & represents a line 
of constant phase going through that point.  The incidence, emission, & phase 
angles at the center point will be printed out on the terminal.  Entering
'CEnterpt initiates the geometry for a new scan and any previous corner point
(below) is lost. 

Positioning the cursor & entering 'COrnerpt causes the cursor position to be 
saved as the corner point of a constant-phase profile scan i.e. the corner 
point determined both the width (range of phase to either side of the 
centerline) and length along the centerline of the scan.  Two dotted lines
will appear representing the width of the scan, as well as two solid lines
representing the length of the scan.  For a narrow scan, the solid lines may 
not be easily visible.  'CEnterpt must be entered before 'COrnerpt, and
entering 'CEnterpt (see above) will erase any previous corner point
information. You may adjust these points as needed, the screen is updated 

Entering 'REFresh will redraw the current centerline, width lines, & length 
limit lines, if present; previous scan lines will be erased.

Entering 'EDitnav will enter the EDITNAV routine, described elsewhere, and
will also erase any scan information.

The scan will sample 
the area, averaging across the width and reporting data along the length of 
the scan.  

  'CEnterpt, 'COrnerpt, 'REFresh, 'EDIT,
  'PLot, 'Dnplot, 'Longplot, 'RAdiplot, 'EMisplot, 'Inciplot, 'PHasplot,
  OUTPUT=<filespec>, 'CP, 'C, 'H, RL=(radius,longitude), LS=(line,sample),
  CZoom=<iz>, STretch=(hidn,lowdn), ASTRETCH=(hi%,low%), 'GERASE, 
  GCOLOR=<color> (or '<color>) 'Help, 'EXit

The 'C command may be used to read the (radius,longitude) coordinates of any
cursored position on the display.  This is useful in determining the desired
ring-sector limits.  The RL & LS commands convert between image & ring
coordinates without involving the cursor. The image display may be adjusted via
the 'H, CZoom, STretch, AStretch, GColor, and 'GErase commands.  

The plot is displayed over the image in graphics.  The abscissa represents 
the length (in pixels, approximately) of the scan, and the ordinate is
dependent on which plot command was issued.  Values of the plotted points may
be read with the cursor via the 'CP command. 

Entering OUTPUT=<filespec> will generate an ASCII file of the scan 
containing some header information, followed by a table with 7 columns of 
numbers.  The columns represent the scan profile index (i.e. distance along 
scan in pixels, approximately), longitude, radius, DN value, incidence angle,
emission angle, & phase angle.  The incidence, emission, & phase angle values
in the table are differences from nominal, not the actual angles themselves
i.e. the photometric values in the table are the actual angles with the nominal
values (listed in the header information) subtracted. 

.page
STARFIT ROUTINE

The STARFIT routine navigates the image by matching the stars in the image
with a starmap computed from data retrieved from the Smithsonian Astrophysical
Observatory (SAO) Star Catalog, Version 1984.  NAV uses a condensed version
of the catalog, containing only the right-ascention, declination (EME50),
magnitude, name and spectral type for each star.  The catalog location is
specified at the program execution statement via the SAO parameter. At MIPS,
sao_idl.str is located  in WMS_VGR:[000000] if VMS, or /project/vgr under Unix.
Outside users can get the sao.cat file from FEI using fei -g vgrrad/sao_idl.str
Previous star catalogs that that worked with NAV 

Note: Other star catalogs can also be used: 

Catalog file              Purpose                Magnitude   Name       Type?
agk3_idl.str      AGK3 (north of -2.5 deg)        to 12      AGK3     Spectral
jup_voy_idl.str   Jupiter approach, epoch '78     to 12      AGK3       None
sat_voy_idl.str   Saturn approach, epoch '78      to >13     AGK3       None
nep_voy_idl.str   Uranus/Neptune, epoch '86       to 12.7     SAO     Spectral


An alternate catalog is the Guide Star Catalog put out by the Space Telescope 
Science Institute.  The relevant PDF parameter is GSCPFX, and (if GSCPFX is set
to "GSCVOL" under VMS or "/cdrom/gscvol" under Eunuuchs(tm),) NAV assumes it 
can find the GSC files under 

  GSCVOL1: & GSCVOL2:

for VMS or under

  /cdrom/gscvol1/ & /cdrom/gscvol2/

for Eunuchs(tm).  The CDROM's may be mounted by one of the following methods:

VMS, local mount:

    $ mount /over=id /nowrite dka400: "" GSCVOL1
    $ mount /over=id /nowrite dka410: "" GSCVOL2

VMS, NFS mount using Multinet:

    $ mult nfsmount /nowrite "host.with.cds::/cdrom/gscvol1" GSCVOL1
    $ mult nfsmount /nowrite "host.with.cds::/cdrom/gscvol2" GSCVOL2

Generic Eunuchs(tm), local mount:

    # mount -t hsfs /dev/sr0 /cdrom/gscvol1
    # mount -t hsfs /dev/sr0 /cdrom/gscvol2

Generic Eunuchs(tm), NFS mount:

    # mount -t nfs host.with.cds::/cdrom/gscvol1 /gscvol1
    # mount -t nfs host.with.cds::/cdrom/gscvol2 /gscvol2


Upon entry, all stars within the camera's field-of-view are retrieved
from the catalog and overlayed (in graphics) on the image display.
A histogram displaying the number of stars at each visual magnitude is
output to the terminal.

The STARFIT command processor is identified by the prompt:

	STARFIT>

To obtain a list of the commands supported by the STARFIT routine,
type HELP:

	Enter EXIT,HIMAG,SSTARS,SBLEMS,SRES,MSTARS,CZOOM,H,HIST,
        SPIKES,STRETCH,ASTRETCH,GERASE,GCOLOR,C,RD,LS,EDIT,PARAMS>

Typing HIMAG=8 will redisplay the graphics starmap, such that all
stars higher than magnitude 8 are ignored.
!CBTC Typing HIMAG=8.5 will redisplay the graphics starmap, such that all
!CBTC stars higher than magnitude 8.5 are ignored.

Typing 'SSTARS redisplays the starmap.

Typing 'MSTARS will enable the user to register the graphics starmap
to the star background in the image.  See MOVESTAR ALGORITHM below.

Typing RD=(76.7,15.5) will cause the cursor to be moved to that
right-ascension and declination on the display screen.  The corresponding
line-sample coordinates are reported.

Typing line-sample coordinates LS=(455,581) will cause the corresponding
right-ascension and declination to be reported.

The 'C command may be used to read the (Right-ascension, declination)
coordinates of any cursored position on the display.  Also information
about the nearest star to the cursored position will be listed.  The image
display may be adjusted via the 'H, CZOOM, STRETCH, and 'GERASE commands.

.page
MOVESTAR ALGORITHM

Upon entry, the user is supplied with the following instructions:

	To select a star, move cursor to a graphics star
	and hit RETURN.  You will then be asked to match
	the selected star to its image via the cursor.
	Repeat to acquire more points or type 'EXIT when done.
	You may delete a point by typing D.
	You may adjust the display by using H,CZOOM,STRETCH,or ASTRETCH.

This is followed by the prompt:

	READY>

When the user has selected a graphics star via the cursor and enters
RETURN, MOVESTAR reads the line-sample position of the cursor, identifies
the star selected, and responds with a "beep" and the prompt:

	Cursor star image and hit RETURN>

The user then selects the corresponding star in the image and hits
RETURN.  This is followed by another "beep" and the READY prompt.  This
cycle is repeated until the user types 'EXIT.  Be careful not to move
the cursor until you hear the "beep", which indicates that the cursor
has been read.  Note that the graphics stars need not be selected
accurately since MOVESTAR searches for the star nearest the cursor
position.  However, the corresponding star image should be selected
with care.  The display resolution will limit the accuracy of this
selection.  Note that the default ZOOM=-2 will enable selection to the
nearest two pixels.  Use the CZOOM command to display areas of the image
at higher resolution.

When the user types 'EXIT to exit from the star selection loop, the
program responds with the prompt:

	Enter CHI2 or CHI3>

Entering CHI2 causes the line-sample position of the planet-center to
be updated.  Entering CHI3 causes the north angle to be updated as well.
If 3 or more stars have been selected, use CHI3.

Before returning control to STARFIT, the starmap is redisplayed, using
the updated camera pointing.

.PAGE
EDIT COMMAND

Typing EDIT will transfer control to the EDIT command processor,
identified by the prompt:

	EDIT>

The EDIT command processor enables the user to modify values which
effect the image geometry.  These include planet constants, camera
constants, and geometric data.  Under normal circumstances, the user
will never have to edit the image geometry data since these are
automatically retrieved from the Image Catalog and frame label.

Typing HELP will cause of all the available commands to be listed:

	Enter EXIT,HELP,GEODET,GEOCEN,RA,RB,RC,LORA,CAMERA,FL,OAXIS,
	SC,SSP,PC,ISPC,WAPC,WAISPC,ANGLN,RANGE,TARGET,ECM,EME,
	SCVECTOR,SOL,RLIM,STATUS,SAVE,RESTORE,GETSEDR,CZOOM,H,STRETCH,
        ASTRETCH,GERASE,GCOLOR,C,OVERLAY,LATI,LONG>

Typing EXIT will return control back to the RINGFIT or LIMBFIT
command processor.

Typing GEODET will cause all latitudes input by the user to be
treated as planetographic, and to report all latitudes to the user
as planetographic.  The default for Voyager is GEODET.  For all other
missions, the default is GEOCEN.

Typing GEOCEN will cause all input and output latitudes to be
treated as planetocentric.

Typing RA=241 RB=235 RC=232 will change the values of the planet's
major equatorial, minor equatorial, and polar radii to 241, 235, and
232 km, respectively.

Typing LORA=135.2 will change the orientation of the major equatorial
radius so that it points to longitude 135.2 degrees.  If RA=RB, then
LORA is undefined and should not be used.

Typing CAMERA=7 will change the camera-ID to VGR-1 NA.  This
will automatically cause the nominal constants for that camera
to be retrieved from tables and stored in FL, OAL, OAS, and
SC below.  The valid VGR camera serial numbers are:

	4 = VGR-2 WA		6 = VGR-1 WA
	5 = VGR-2 NA		7 = VGR-1 NA

        Valid Cassini camera serial numbers are:
        1=NAC  21=NAC 2x2 summation mode  41=NAC 4x4 summation mode
        2=WAC  22=WAC 2x2 summation mode  42=WAC 4x4 summation mode

Typing  FL=1200.175 will change the camera focal length to
1200.175 mm.

Typing OAXIS=(500,500) will place the optical axis intercept
at (line,sample)=(500,500).

Typing SC=84.821 will change the picture scale to 84.821 pixels
per mm on the focal plane.

Typing SSP=(-34.113,335.252) will change the spacecraft position
to (SCLAT,SCLON)=(-34.113,335.252).

Typing SOL=(32.3,124.5) will change the position of the sun to
(SUNLAT,SUNLON)=(32.3,124.5).

Typing PC=(445.3,558.4) will move the planet center to
(SCLINE,SCSAMP)=(445.3,558.4).  This will cause the camera pointing
to be updated.  The planet-center must be in object-space.  To
enter an image-space planet-center, use ISPC.  Note that if ISPC is
used, the input image must be in image-space.

If the is image is a narrow-angle frame from a simultaneous exposure
and if the planet-center for the wide-angle frame is known, it may
be entered using WAPC (e.g. WAPC=(455.1,376.2)).  The (line,sample)
coordinates are converted to the narrow-angle field of view and
the result used as the (line,sample) coordinates for the narrow-angle
planet-center.  The planet-center must be in object-space.  To enter
an image-space planet-center for the wide-angle frame, use WAISPC.

Typing ANGLN=190.9 will cause the orientation of the projected
spin axis to be changed to 190.9 degrees, measured clockwise
from right (the positive sample direction).

Typing RANGE=4794909 will cause the distance from spacecraft to
planet center to be changed to 4794909 km.

Typing TARGET=SATURN will change the target-ID to Saturn and
automatically changes the polar and equatorial radii to Saturn's
nominal values.

The parameters ECM, EME, and SCVECTOR may be used to change the
C-matrix, ME-matrix, and spacecraft vector by entering data as
provided by Mert Davies.  When 'ECM is typed, the program prints out
the current values for three angles defining the camera orientation
(ALPHA, DELTA, and KAPPA) and issues the prompt:

	Enter changes of type EXIT>

Typing ALPHA=25.2176 will modify the value of ALPHA, etc.  Similiarly,
typing 'EME will permit editing of the three angles defining the
ME-matrix (ALPHA, DELTA, and OMEGA).

Typing SCVECTOR=(10833.2,36556.4,371.6) will update the spacecraft
vector, as expressed in celestial coordinates (km).

Typing RLIM=(76000,140000) will set the minimum and maximum ring
radii to 76000 and 140000 km respectively.  Note that these values
are used only in avoiding limb points obscured by the ring or
ring shadow during a limb scan.

Typing STATUS will cause a summary of the navigation data to be
printed.

Typing SAVE will store the current navigation data in the array
SNAV.  Typing RESTORE will replace the navigation data by
whatever is in SNAV.  Typing GETSEDR will replace the navigation
data with the nominal pointing.

The remainder of the commands are used to update the image display
and are as described above (see LIMBFIT ROUTINE).

.PAGE
ERING COMMAND

The orbital elements of each planetary ring system are read from the
Ring Orbital Elements File (OEF).  These orbital elements define the
inclined-plane eccentric-ring geometry of each ring.  The file may be
specified when entering NAV, as in the following example:

	NAV  PIC  OEF=RINGDATA.DAT

A new orbital elements file may be created by executing program RINGORBS.
At MIPS, the file is located in ROEF.DAT (in WMS_VGR:[000000] if VMS, 
or /project/vgr under Unix).
Outside users can get the ROEF.DAT file from FEI using fei -g vgrrad/roef.dat

The ERING command is used to edit the orbital elements for the current
planetary ring system:

	ERING>

Entering HELP will display a list of the commands available
at this prompt:

	Enter EXIT,RING,STATUS,SMAA,ECC,INCL,PIZERO,OMEGZ,
        RA,DEC,THETA,ZETAZ,RESTORE,HELP>

Entering RING=A will cause the orbital elements of the alpha
ring to be retrieved for editing.  The ring points are computed
and displayed in graphics, based upon the current C-matrix.

Entering STATUS will display the orbital elements for the
retrieved ring.

Entering SMAA=5000. will change the semi-major axis to 5000 km.

Entering ECC=0.0123 will change the eccentricity to 0.0123.

Entering INCL=0.1093 will change the inclination angle of the
ring plane to 0.1093 degrees.

Entering PIZERO=169.3 will change the longitude of periapse
to 169.3 degrees.

Entering OMEGZ=82.3 will change the longitude of the ascending
node to 82.3 degrees.

Entering RA=76.6, DEC=15.1 will change the orientation of
planet's north pole to (RA,Dec)=(76.2,15.1) degrees.  Note that
the orientation of the pole is assumed to be constant (i.e. does
not precess).  Also, These parameters can only be changed for
RING=P since they are common to all of the rings.  NAV will then
update all of the rings.

Whenever one of the orbital elements is changed, the ring points
are re-computed and re-displayed in graphics.

Entering 'RESTORE wil restore the original orbital elements for
the ring.

Entering 'WRITE will update the Orbital Elements File with the edited values.
Note that the user cannot (i.e. does not have sufficient priviledge to) update
the default OEF.  If you need to tinker with the orbital elements, copy the
file into your own directory and use the OEF parameter described above.
Alternatively, a new OEF can be created by executing program RINGORBS.

One can define a new eccentric ring by entering parameters in several
"place holding" rings.  These extra rings can be accessed by specifing
RING = ...

     SATURN:  Y Z
     URANUS:  W X Y Z
     NEPTUNE: W X Y Z

These rings can then be used as the MASTER ring plane, in RINGTRACE and
RINGSCAN, they can be drawn as RING=-, and the plane can be used in PROFILE.
.PAGE
PARAMS COMMAND

Typing PARAMS will enable user modification of processing
parameters:

	Enter EXIT,NLW,NSW,NSEARCH,CHI2,CHI3,DBUG,NODBUG>

Entering NLW=9 NSW=5 will change the correlation window (used in the
limb scan) to a 9x5 area.  NLW and NSW must be specified as odd
positive integers.

Entering NSEARCH=25 will increase the search radius to 25 pixels.

Entering CHI2 will minimize differences between the computed
points (CPTS) and the actual points (APTS) by correcting for
translation errors in the image.  Angles ANGLA and ANGLB of the
camera orientation matrix (OM) are updated.

Entering CHI3 will correct for rotation and translation errors.
Angles ANGLA, ANGLB, and ANGLN (the north angle) of the OM
matrix are updated.

Entering DBUG or NODBUG enables or disables diagnostic messages.


GENERAL COMMENTS

The general rules regarding TAE parameters apply:

Commands may be abbreviated, as long as enough letters are typed
to insure uniqueness.

When a command is not followed by a value, it must be preceded
by an apostrophe, e.g. 'TRACE.  The exception is that if the first
keyword in a list is entered, the apostrophe may be ommitted,
e.g:
     >	Enter LIMB or RING L

If you need help or want to get out of a routine, try typing
HELP or EXIT.  Sometimes it works, sometimes it doesn't....


PROGRAM HISTORY

NAV was developed as a cooperative effort between JPL, Cal-Tech,
and the University of Arizona.  Software development at Cal-Tech was
under the direction of Andy Ingersoll.  Software development at Arizona
was under the direction of Carolyn Porco.

The algorithms implemented in NAV are based on notes provided by Andy
Ingersoll, 3-19-85 (basic algorithms), and 1-2-86 (eccentric ring geometry),
and by Carolyn Porco, 1-12-87 (stellar navigation and Neptune's rings).
The equations for the planet and planet-limb geometry have been generalized
to accomodate the tri-axial ellipsoid target-body model.

The major part of program development was undertaken at JPL.  The original
planet limb algorithm (LIMBPT) was written by Tom Brinck of Cal-Tech.  A major
portion of the ring and stellar geomtry algorithms were written by Vance
Haemmerle of Arizona.

Original Programmers: Gary Yagi and Vance Haemmerle, 30 September 1985
Current Cognizant Programmer: Gary Yagi
Revisions:
 Jan 01, 85  GMY  ...Add ringfit routines
 Feb 17, 85  GMY  ...Add eccentric ring routines
 Sep 11, 87  GMY  ...Change planet model from oblate spheroid to ellipsoid
 May 28, 88  GMY  ...New VRDI and SEDR interfaces.
 Jun 26, 88  GMY  ...Add starfit routines
                  ...Modify program to work on image-space images.
 Jun 30, 88  GMY  ...Modify program to work on 1024x1024 display.
 Jul 15, 88  GMY  ...Fix LIMBPT and RINGPT routines to skip over points
                     not in picture. 
 Jul 20, 88  GMY  ...Modify program to work on halfword images.
 Jul 26, 88  GMY  ...Merge code from Vance Haemmerle's version of NAV.
		  ...New capabilities include computation of phase angle,
		  ...orbital elements of Saturn's rings, longitudinal
		  ...ring profiles, and graphics display of ring profile.
 Mar 17, 89  GMY  ...Read orbital elements from file
                  ...Numerous bug fixes (FRs 38130,42711,42712)
 May 31, 89  VRH  ...RL, LS functions for rings added
		  ...Chisq, Covarient matrix, PHASE, LATI, LL fixed
                  ...PROFILE for Master ring system 
                  ...(* System directory dependences removed - UA version)
 Jun  8, 89  VRH  ...RADIUS in Master ring system for drawing & fiting
                  ...PLANE option in fiting (TRACE,SCAN) for RADIUS ring
                  ...Use ring place holders for defining new rings
                  ...available for every use except SRINGS
 Jul 29, 89  VRH  ...Direct C-Matrix corrections for ANGLA less than 10 degrees
                  ...Epoch's changed from ERT to Planet time
                  ...Cursor routines report DN value
                  ...Bug fixes: /C1/ common block, Scan limits, RA in LATLON,
                  ...GETSEDR,GETLAST... to end with planet navigation
                  ...ERING changed: ALPHAp,DELTAp,THETA,ZETAZ only changed
                  ...using RING=P then all rings updated
                  ...Labels added to profile plots
 Aug 16 89  VRH   ...Added PREDICT parameter & subroutine
 Jun 1, 90  GMY   ...Galileo conversion.
                  ...Minor bug fixes (FRs 48219,48452,48454,50799,48218,52848)
 Jul 26, 90 GMY   ...Change PLANET_NAME to TAREGET_NAME in call to SPICESUB.
 Feb 22, 91 GMY   ...Make it work when SEDR or SPICE kernels are not available.
 1-nov-95   lwk   ...ported program (limb-fit only, and no halfword data or
                     graphics under Unix)
 Apr 4, 96  ARV   ...Added capability to handle non-flight projects.
                  ...VICAR retrieves SCLAT, SCLON, SUNLAT, SUNLON, CENTLINE,
                  ...CENTSAMP, RANGE, FOCAL, SCALE, NORTH, OALINE, OASAMP
                  ...from a MAP2 Perspective Projection label, the command
                  ...line (these parameters added to command line), or
                  ...prompts user.  Changes tagged by 'A. VASAVADA'.
 7-apr-96   lwk   ...enabled graphics on all platforms
 24-jun-96  BAM   ...modified program for new Reseau File Format
 27-Aug-96  OAM   ...modified program to call getspice2 instead of getspice.
                     Included provenance parameters. Modified to reference
                     the OEF and SAO files only when they are needed.
 06-Nov-96  BTC   ...Changes from 06-Nov-96 version have the marker "CBTC"
 (Feb, 1997)          "CBTC" near them - this work was done in Feb, 1997
                  ...Fixed several problems with RINGFIT & STARFIT
                  ...See also RINGORBS.COM to get correct ROEF.DAT(Jupiter/GLL)
                  ...Fixed broken non-VGR logic around call to VGRCLEAR
                  ...Fixed mistakes regarding INTEGER vs REAL vars (XDROTATE
                     argument, HIMAG in SUBCMD STARFIT, SL,SS,EL,ES in 
                     RINGPT2()), and dimensions of HIStogram vector.
                  ...Fixed broken axis labels in PRDISPLAY (ring PROFILE plot)
                  ...Fixed broken display code when ZOOM is not 1
                  ...Improved speed of image display via buffering in AVLINE()
                  ...Added GCOLOR parameter to allow the user to define
                     the graphics overlay color (default is CYAN).
                  ...Added RPARAMS call to STARFIT()
                  ...Fixed STARFIT et al. for J2000 Reference Frame (ISYSTEM=1)
 23-Jul-98  RRP   ...Updated to obtain four digit year and use four digit year
                  ...through out the program. The variable name is IDATE and it
                  ...is used in calculating number of days since 1950 in T1950
                  ...subroutine (Y2K-FRD<901>).
 07-Jan-99  RRP   ...Updated to display four digit year (Y2K-FRD<901>).
 01 Apr 00  GMY  Fixed display of halfword data (AR 9059)
 12 May 00  GMY  Fixed RADIUS count in .PDF.
 13 May 00  GMY  Removed compiler errors on SGI and Linux.
 26 May 00  GMY  Fix display of halfword when going from CZOOM=2 back to 'H.
 12 Jun 00  GMY  Replace PARMTST1 with XVIPTST, replace UPRCASE w library vers.
  3 Jun 02  VRH   ...Increase max image size to accomodate Cassini images
                  ...Make default FOV not Voyager specific
                  ...Add Cassini as a recognized Project
 16 Oct 02  VRH  Fixed some compile warnings in LIMBFIT
                  ...Updated star catalog and GETSTARS to work on all platforms
                  ...New SAO catalog is sao_idl.str, no longer blocked
                  ...Star catalogs now contain star name and spectral type
                  ...For nearest star: name and type printed when cursored
                  ...Fixed time used for proper motion (T1950 call was gone)
                  ...Fixed T1950 routine, was calculating days from 1955
                  ...Upped maximum number of stars to 2000
                  ...Merged in bug fixes done at LPL 1989-1995
                  ...Added Diagonal and Smear plotting to PROFILE (LPL 1992)
                  ...Add more data to PHASE routine (LPL Mar 1, 94)
                  ...Added printout of S/C to central body (LPL May 10, 95)
                  ...Fixed bug in usage of CONVISOS in RINGFIT
                  ...Fixed Orbital Elements file code
                  ...ME for Rings now in ISYSTEM (B1950 or J2000) in MECALC
                  ...Use GETCAMCON instead of VGRCAM in EDITNAV CAMERA select
                  ...Fixed use of VGR BLEM files on unix (call VIC1LABX)
                  ...Update documentation and PDF
 29 may 06  lwk  Convert SCLON read in from MP to East.  Fixed some minor bugs
                 causing build problems or crashes.
 12 feb 10  lwk  Changed "west" to "east" in Help text for SCLON and SUNLON.
                 Added line to Help file reminding users that NAV can't be
                 run in shell-vicar.
  6 jul 11  lwk  Added INIT_SPICE before call to PBDATA in routine SPICESUB, since
                 the new PBDATA needs SPICE kernels and the call to GETSPICE2 clears
                 the kernel pool on return.

.LEVEL1
.VARI INP
STRING -- REQUIRED
Input image file name
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
.VARI FRAME
INTEGER - Image frame number.
.VARI CAMERA
INTEGER - Camera serial number
.VARI TARGET
STRING - Target name (e.g. GANYMEDE).
.VARI RES
STRING - Input reseau locations.
.VARI BLEMS
STRING - Input blemish location.
.VARI SAO
STRING - Input SAO or other star catalog
.VARI GSCPFX
STRING - Guide Star Catalog Prefix
 1: & 2: are appended (VMS)
 1/ & 2/ are appended (Eunuchs(tm)
.VARI OEF
STRING - Ring Orbital Elements File
.VARI NLW
INTEGER - Length of correlation window in pixels.
.VARI NSW
INTEGER - Width of correlation window in pixels.
.VARI NSEARCH
INTEGER - Search radius (pixels).
.VARI LIMB
Call Limbfit command processor
.VARI SCAN
KEYWORD - Scan the limb or ring
.VARI TRACE
KEYWORD - Trace the limb or ring
.VARI SCPTS
KEYWORD - Display computed points
.VARI SAPTS
KEYWORD - Display acquired high-contrast points
.VARI SBLEMS
KEYWORD - Display blemish locations
.VARI SRES
KEYWORD - Display reseau locations
.VARI CZOOM
INTEGER - Center display and zoom
.VARI H
KEYWORD - Display entire image
.VARI C
KEYWORD - Read cursor position
.VARI HIST
KEYWORD - Histogram of displayed image area
.VARI SPIKES
INTEGER - Number of spikes in histogram.
.VARI STRETCH
INTEGER - Stretch the displayed image
.VARI ASTRETCH
2 real values - Percentage saturation at
low and high ends of the histogram.
.VARI GERASE
Erase graphics plane
!CBTC
.VARI GCOLOR
Change graphics color
(W,R,G,BLUE,C,M,Y,BLACK)
.VARI SBLEMS
KEYWORD--OPTIONAL
Entering SBLEMS will display the locations of the camera blemishes
(overlayed in graphics).  The blemishes are automatically retrieved from
blemish files located in WMS_VGR:[VGR1.NA], WMS_VGR:[VGR1.WA], and
WMS_VGR:[VGR2], or the equivalent directories /project/vgr/vgr1/wa,
/project/vgr/vgr1/na, and /project/vgr/vgr2 under Unix.
The user may optionally specify the location of the blemish file via the 
BLEMS parameter.  The program checks to ensure that the camera serial 
number associated with the file (as extracted from the label) matches the 
camera serial number associated with the image.

.VARI SRES
KEYWORD--OPTIONAL
Entering SRES will display the reseau locations (overlayed in graphics).
If the reseau has not been removed, this display will give an indication
of how accurately the reseau has been located (and consequently the
accuracy of the geometric correction).
.VARI OVERLAY
INTEGER - Draw latitude-longitude grid
.VARI LATI
REAL - Draw parallel at given latitude
.VARI LONG
REAL - Draw meridian at given longitude
.VARI RL
REAL - Convert from radius-lon to line-sample
.VARI LL
REAL - Convert from lat-lon to line-sample
.VARI LS
REAL - Convert from line-sample to lat-lon
.VARI EDIT
Edit navigation data
.VARI PARAMS
Edit processing parameters
.VARI RADIUS
REAL - Ring radius (km)
.VARI RING
STRING - ring name.
.VARI SRINGS
KEYWORD - Show ring system
.VARI PROFILE
KEYWORD - Plot radial or longitudinal profile of ring sector.
.VARI TYPE
STRING - R for radial profile and L for longitudinal.
.VARI RLIMITS
REAL - Starting and ending radial limits for sector (km).
.VARI LLIMITS
REAL - Starting and ending longitude limits for sector (degrees).
.VARI LWIDTH
REAL - Width in longitude of diagonal radial scan (degrees).
.VARI CP
KEYWORD - Cursor up a value on the displayed plot.
.VARI OUTPUT
STRING - Specify file name for output of profile to disc.
.VARI PHASE
KEYWORD - Output phase angles also to profile file
.VARI PLOT
KEYWORD - Redisplay plot that was erased by redisplay of image.
.VARI MASTER
STRING - Specify master C-matrix
.VARI ERING
KEYWORD - Edit orbital parameters
.VARI PLANE
STRING - ring plane name
.VARI INNER
Inner edge of ring
.VARI OUTER
Outer edge of ring
.VARI WIDTH
REAL - Width of ring (pixels)
.VARI STAR
KEYWORD - Invokes STARFIT routine.
.VARI HIMAG
INT - highest magnitude star displayed.
!CBTC REAL - highest magnitude star displayed.
.VARI SSTARS
KEYWORD - Display starmap.
.VARI MSTARS
KEYWORD - Move (register) starmap.
.VARI RD
REAL - Right ascension and decilination.
.VARI CHI2
Solve for translation errors in camera pointing.
.VARI CHI3
Solve for translation and rotation errors in camera pointing.
.VARI DBUG
Cause diagnostic messages to be printed.
.VARI NODBUG
Suppress diagnostic messages (default).
.VARI GEODET
Use planetographic latitudes
.VARI GEOCEN
Use planetocentric latitudes
.VARI RB
REAL - Minor equatorial radius (km)
.VARI RC
REAL - Polar radius (km)
.VARI LORA
REAL - Longitude of RA (deg)
.VARI FL
REAL - Camera focal length (mm)
.VARI OAXIS
REAL - Optical axis intercept
(line,sample) coordinates.
.VARI SC
REAL - Picture scale (pixels/mm)
.VARI SSP
REAL - Spacecraft position (lat,lon)
.VARI SOL
REAL - Sun position (lat,lon)
.VARI PC
REAL - Object-space planet-center
 (line,sample)
.VARI ISPC
REAL - Image-space planet-center
 (line,sample)
.VARI WAPC
REAL - Wide-angle object-space
 planet-center (line,sample)
.VARI WAISPC
REAL - Wide-angle image-space
 planet-center (line,sample)
.VARI ANGLN
REAL - North angle
.VARI RANGE
REAL - Spacecraft range
.VARI RLIM
REAL - Min and max ring radii (km)
.VARI TARGET
STRING - Target body name
.VARI SAVE
KEYWORD - Save navigation data
.VARI RESTORE
KEYWORD - Restore navigation data
.VARI GETSEDR
KEYWORD - Retrieve SEDR nav data
.VARI STATUS
KEYWORD - Print navigation data or processing parameters
.VARI SMAA
REAL - Semi-major axis of ring
.VARI ECC
REAL - Eccentricity of ring
.VARI INCL
REAL - inclination angle of ring plane
.VARI PIZERO
REAL - Longitude of periapse
.VARI OMEGZ
REAL - Longitude of ascending node
.VARI RA
REAL - Right ascension of planet's pole
.VARI DEC
REAL - Declination of planet's pole
.VARI THETA
REAL - Orientation of Earth's pole, 1977
.VARI ZETAZ
REAL - Orientation of Earth's pole, 1977
.VARI SCLAT
 REAL - Sub spacecraft latitude
in  degrees.
.VARI SCLON
REAL - Sub spacecraft longitude
in degrees east.
.VARI SUNLAT
REAL - Sub solar latitude in
degrees.
.VARI SUNLON
REAL - Sub solar longitude in
degrees east.
.VARI CENTLINE
REAL - Line of planet center.
.VARI CENTSAMP
REAL - Sample of planet center
.VARI RANGE
REAL - Distance from spacecraft to planet center in km.
.VARI FOCAL
REAL - Camera focal length.
In mm.
.VARI SCALE
REAL - Camera scale. Pixels/mm.
.VARI NORTH
REAL - The north angle in
degrees clockwise from east.
.VARI OALINE
REAL - Line of the camera
optical axis.
.VARI OASAMP
REAL - Sample of the camera
optical axis.
.VARI CLEAN
Clean bad points
.VARI SEF
for program development only
.VARI HELP
List available commands
.VARI EXIT
Exit from routine


.LEVEL2
.VARI INP
	EX:  INP=PIC
where PIC is the image to be navigated.  PIC must be a Voyager, Galileo or
Cassini image in either byte or halfword (16-bit integer) data format.  The
frame must be in image-space or object-space (no picture size reduction or
map-projection allowed).

Voyager images should be 800x800 for image-space frames and 1000x1000 for
object-space frames.  Galileo images are always in image-space and are
800x800 for full-frame images and 400x400 for summation-mode images.  Cassini
images can be 1024x1024, 512x512 or 256x256 and are always in image-space.

The frame does not have to be radiometrically corrected.  It will be desirable
to do so, however, if the PROFILE option is used to gather ring-profile data.
.VARI SPICEMODE
SPICEMODE=LOCAL specifies that SPICE data is to be accessed from local
SPICE kernels.  SPICEMODE=REMOTE specifies that SPICE data is to be accessed
via the SPICE server.  If SPICEMODE is defaulted, the logical name (or
environmental variable) DEFAULTSPICE is used to determine whether LOCAL or
REMOTE is used.

Note that if SPICE data is not found in LOCAL or REMOTE mode, the other mode
is attempted in order to retrieve SPICE data.  However, when improved camera
pointing data is stored, only the specified or default mode is used.

.VARI CKNAME
CKNAME is a four character string specifying the C-kernel to be used in reading
camera pointing data:

  CKNAME	C KERNEL
  --------      -------------
  DAVI		MIPS_DAVI.CK
  NAV		MIPS_NAV.CK
  FARE		MIPS_FARENC.CK
  NAV2		MIPS_NAV2.CK
  NEAR		MIPS_NEAR.CK
  AMOS		MIPS_AMOS.CK
  NAIF		the best NAIF kernel is used

If defaulted, the kernels are searched in the above order.

.VARI CKID
CKID is an alternative way to specify the prefered C-kernel for reading
camera pointing data (see CKNAME parameter):

  CKID	  CKNAME	C KERNEL
  ----	  --------      -------------
  M906	  DAVI		MIPS_DAVI.CK
  M905	  NAV		MIPS_NAV.CK
  M904	  FARE		MIPS_FARENC.CK
  M903	  NAV2		MIPS_NAV2.CK
  M902	  NEAR		MIPS_NEAR.CK
  M901	  AMOS		MIPS_AMOS.CK
  varies  NAIF		there are a large number of these files

Ex:  CKID=M901 specifies the four character ID which uniquely identifies the
     C-kernel MIPS_AMOS.CK.

A complete list of the C-kernel IDs is located in the ASCII file assigned the
logical name (or environmental variable) KERNELDB.

If specified, CKID overrides the CKNAME parameter.

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
.VARI FRAME
INTEGER - Frame number of the input image.  For Voyager, this is the
   FDS Count specified as XXXXXYY where XXXXX is the Mod16 count and
   YY is the Mod60 count.  Default is the value in the picture label.
.VARI CAMERA
INTEGER - Camera serial number.  Default is the value in the picture
   label.  Valid Voyager camera serial numbers are:

	4 = VGR-2 WA		6 = VGR-1 WA
	5 = VGR-2 NA		7 = VGR-1 NA

       Galileo SSI has only one camera (serial number always 1).
       However, serial number of 2 is used for summation mode.

        Valid Cassini camera serial numbers are:
        1=NAC  21=NAC 2x2 summation mode  41=NAC 4x4 summation mode
        2=WAC  22=WAC 2x2 summation mode  42=WAC 4x4 summation mode

.VARI RES
STRING--OPTIONAL
Input reseau locations:
  Ex:  RESLOC  (PIC,RESFILE)  R		!Locate the reseau
       NAV  PIC  RES=R			!Input the reseau to NAV
The reseau locations must be input if the image contains geometric camera
distortions (i.e. if it is an image-space frame).
.VARI BLEMS
STRING--OPTIONAL
Input blemish locations:
  Ex:  VGRBLEM  PIC  B		!Retrieve the blemish locations
       NAV  PIC  BLEMS=B	!Input the blemishes to NAV
If defaulted, the program automatically accesses blemish files located
in WMS_VGR:[VGR1.NA], WMS_VGR:[VGR1.WA], and WMS_VGR:[VGR2], or the 
equivalent directories /project/vgr/vgr1/wa, /project/vgr/vgr1/na, and 
/project/vgr/vgr2 under Unix.

The program checks to ensure that the camera serial number associated 
with the file (as extracted from the label) matches the camera serial 
number associated with the image.
.VARI SAO
	EX:  SAO=VGR:[UID123]SAO.CAT
Specifies the file location of the Smithsonian Astrophysical Observatory (SAO)
or other star catalog.  The catalog is required for star navigation only.  At
MIPS the file sao_idl.str is located in WMS_VGR:[000000] if VMS, 
or /project/vgr under Unix.  Other star catalogs are available.
Outside users can get the sao.cat file from FEI using fei -g vgrrad/sao_idl.str
See HELP file for details on file contents and format.

.VARI GSCPFX
	EX:  GSCPFX=GSCVOL            !VMS
             GSCPFX="/cdrom/gscvol"   !Eunuchs(tm)

Specifies the prefix for ths location of the Guide Star Catalog CDROM's put 
out by the Space Telescope Science Institute.  If this parameter is specified 
then it will override the SAO parameter.

See HELP file for details on mounting the CDROM's.

.VARI OEF
	EX:  OEF=VGR:[UID123]OEF.DAT
Specifies the file location of the Orbital Elements File, which defines the
ring systems of the outer planets.  At MIPS, the orbital elements file is 
located in WMS_VGR:[000000] if VMS, or /project/vgr under Unix. This parameter
is required when the parameter RING is specified.
Outside users can get the ROEF.DAT file from FEI using fei -g vgrrad/roef.dat.

.VARI NLW
INTEGER - Length of correlation window in pixels.
    High contrast points (limb or ring) are determined by correlating
    with an edge function of area NLWxNSW.  Note that both NLW and
    NSW must be positive odd integers.
.VARI NSEARCH
INTEGER - Search radius (pixels).
Example: NSEARCH=11
    Starting at each computed limb (or ring) point, scan in a direction
    normal to the limb a distance + and - 11 pixels for high-contrast
    points.
.VARI CZOOM
INTEGER - Center display and zoom
Example: CZOOM=2
    Center the display around the current cursor position and magnify
    the image to twice normal resolution.
.VARI H
Display entire image.  The image may be displayed at reduced resolution
to accomplish this.
.VARI C
Read cursor positon.  Reports (line,sample) and (lat,lon) or (radius,lon)
coordinates.
.VARI HIST
KEYWORD - Overlay (in graphics plane) histogram of displayed image area.
  A logrithmic frequency scale is used.
.VARI SPIKES
INTEGER - Ex: SPIKES=n will scale the frequencies so that the n'th largest
  frequency corresponds to maximum scale.
.VARI STRETCH
INTEGER - Stretch the displayed image
Example: STRETCH=(25,100)
    A hardware stretch (linear transformation) is applied to the displayed
    image.
.VARI ASTRETCH
    ASTR=(lopercent,hipercent)
Entering ASTR=(0.5,1.5) will perform an "ends-in" stretch:  The
histogram of the displayed image area is first computed.  Low and
high stretch limits are then chosen so as to saturate 0.5 and
1.5 percent of the data at the low and high ends of the histogram
(respectively).  The algorithm is described in detail in the
help information for VICAR program FIT.
.VARI OVERLAY
INTEGER - Draw latitude-longitude grid
Ex: OVERLAY=30 draws parallels and meridians at 30 deg intervals
.VARI LATI
REAL - Draw a parallel
Ex: LATI=35.5 draws a parallel at latitude 35.5 degrees
.VARI LONG
REAL - Draw a meridian
Ex: LONG=180.0 draws a meridian a longitude 180.0 degrees
.VARI LL
REAL - Convert from lat-lon to line-sample
Ex: LL=(-35.2,183)
.VARI LS
REAL - Convert from line-sample to lat-lon
Ex: LS=(341,789.3)
.VARI RADIUS
REAL - Ring radius (km)
Example: RADIUS=136776
    A ring of radius 136776 km is displayed in graphics.
.VARI RING
STRING - Ring name. The ten Uranian rings are identified by the
following nomenclature:

	6 = ring 6		N = eta ring
	5 = ring 5		G = gamma ring
	4 = ring 4		D = delta ring
	A = alpha ring		L = lambda ring (1986U1R)
        B = beta ring		E = epsilon ring

 Saturns ring features are as follows:

        F = F ring              K = Outer Keeler gap
        E = Outer Encke gap     A = Inner A ring edge
        N = Cassini inner edge of outer ring
        S = Cassini inner edge of 4th outer ring
        W = Outer B feature     B = Inner B ring edge
        M = Maxwell gap         R = Mid C ring feature
        T = Titan gap           C = Inner C ring edge
 
For Neptune, nine test orbits have been defined, the first three with
polar orbits and the remainder with equatorial orbits.  These rings are
idenditied as follows:


          Polar rings      New Satellites        Previous
	A =  50000 Km	   5 = 1989N5  orbit     T = Triton orbit
	B = 100000 Km	   3 = 1989N3            N = Nereid orbit
	C = 150000 Km	   4 = 1989N4
			   2 = 1989N2
			   1 = 1989N1

E.g. RING=E
When RING is specified, the parameter OEF is required.
.VARI SRINGS
KEYWORD - Show ring system.  For Uranus, the ten known rings will
be displayed in graphics, their positions based upon current pointing
knowledge.
.VARI PREDICT
REAL (3) - PREDICT=FRAME: Longitude of cursored object is predicted
for FRAME assuming MASTER ring orbit.  Cursor determines current
object's MASTER radius and longitude.
PREDICT=(FRAME,RADIUS):  Longitude of cursored object is predicted
for FRAME assuming circular orbit of RADIUS in MASTER ring plane.
Cursor determines objects current MASTER longitude.
PREDICT=(FRAME,LONGITUDE): Position in current frame is determined
from MASTER longitude at different FRAME.
PREDICT=(FRAME,LONGITUDE,RADIUS): Position in current frame is determined
from MASTER longitude assuming circular orbit of RADIUS in MASTER plane
at different FRAME.
.VARI PROFILE
KEYWORD - Plot radial or longitudinal profile of specified ring sector.
The user will be prompted for radius and longitude boundaries of sector.
.VARI MASTER
STRING - Specifies master C-matrix.  All C-matrices are replaced by
the master C-matrix.
Ex: MASTER=D
.VARI ERING
KEYWORD - Enables editing of ring orbital parameters.
.VARI INNER
Inner edge of ring.
    Keywords INNER, OUTER, or THIN specity the type of edge function
    to apply when scanning for a ring radius.
.VARI STAR
Typing STAR invokes the STARFIT routine which navigates the image by
matching the stars in the image with a starmap computed from data
retrieved from the SAO catalog.
.VARI HIMAG
INT - highest magnitude star displayed.
!CBTC REAL - highest magnitude star displayed.
.VARI SSTARS
KEYWORD - Display starmap.
.VARI MSTARS
KEYWORD - Move (register) starmap.
.VARI RD
Typing RD=(76.7,15.5) will cause the cursor to be moved to that
right-ascension and declination on the display screen.  The corresponding
line-sample coordinates are reported.
.VARI GEODET
All input and output latitudes are reported as planetographic.
The default for Voyager is GEODET.  For all other projects, the default
is GEOCEN.
.VARI GEOCEN
All input and output latitudes are reported as planetocentric.
.VARI PC
REAL - Object-space planet-center (line,sample)
Example: PC=(433.2,315.3)
    The camera pointing is updated so that the planet-center is 
    located at (line,sample)=(433.2,315.3) in the image.
.VARI ISPC
REAL - Image-space planet-center (line,sample)
Example: ISPC=(433.2,315.3)
    The camera pointing is updated so that the planet-center is 
    located at (line,sample)=(433.2,315.3) in the image.  The input
    image must be an image-space frame.
.VARI WAPC
REAL - Object-space planet-center of VGR wide-angle frame.
The input frame is assumed to be the narrow-angle frame of a WA-NA
simultaneous exposure.  WAPC permits input of the planet-center
(line,sample) of the corresponding wide-angle frame.  The planet-center
for the narrow-angle frame is then derived using the known
transformation between the to camera fields-of-view.
EX: WAPC=(235.3,335.8)
.VARI WAISPC
REAL - Image-space planet-center of VGR wide-angle frame.  The input
frame must be in image-space and is assumed to be the narrow-angle
frame of a WA-NA simultaneous exposure.  WAISPC permits input of the
planet-center (line,sample) of the corresponding wide-angle frame.
The planet-center for the narrow-angle frame is then derived using
the known transformation between the to camera fields-of-view.
EX: WAISPC=(235.3,335.8)
.VARI ANGLN
REAL - North angle
Example: ANGLN=45.3
    The camera pointing is updated so that the planet's projected
    spin axis is oriented at a 45.3 degree angle, measured
    clockwise from up in the image.
.VARI RANGE
REAL - Spacecraft range (km).
    Distance from spacecraft to planet center.
.VARI RLIM
REAL - Minimum and maximum ring radii.  Used only to flag ring-obscured
limb points during limb scans.
EX: RLIM=(80000.,140000.)
.VARI TARGET
STRING - Target body name
Example: TARGET=SATURN
.VARI SAVE
KEYWORD - Save current navigation or orbital data.
.VARI RESTORE
KEYWORD - Restore previously saved navigation or orbital data.  If no
previous values were saved, initial values are retrieved.
.VARI GETSEDR
KEYWORD - Retrieve navigation data from SEDR.  This restores the
nominal camera pointing.
.VARI PIZERO
REAL - Longitude of periapse measured at Epoch.  For Uranus, French's
epoch is 20:00 UT on 10 March 1977.  For Saturn, Porco's epoch is
time of Voyager 1 closest approach.  For Neptune, the epoch is time of
Voyager 2 closest approach.
.VARI OMEGZ
REAL - Longitude of ascending node measured at Epoch.  For Uranus, French's
epoch is 20:00 UT on 10 March 1977.  For Saturn, Porco's epoch is
time of Voyager 1 closest approach.  For Neptune, the epoch is time of 
Voyager 2 closest approach.
.VARI RA
REAL -  Right ascension of planet's pole (assumed constant)
.VARI DEC
REAL - Declination of planet's pole (assumed constant)
.VARI SCLAT
Sub spacecraft planetocentric latitude in degrees.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI SCLON
Sub spacecraft longitude in degrees east.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI SUNLAT
Sub solar planetocentric latitude in degrees.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI SUNLON
Sub solar longitude in degrees east.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI CENTLINE
Line of planet center.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI CENTSAMP
Sample of planet center
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI RANGE
Distance from spacecraft to planet center in KM.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI FOCAL
Camera focal length in mm.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI SCALE
Camera scale in pixels/mm.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI NORTH
The angle measured in degrees clockwise from up of the projection of the
planet spin axis (north end) normally onto the image plane.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI OALINE
Line of the camera optical axis.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI OASAMP
Sample of the camera optical axis.
Normally this is obtained from the input label(s).
Only valid for non-flight projects.
.VARI CLEAN
INTEGER - Clean bad points
Example: CLEAN=5
    All high-contrast points within a 5 pixel radius of the current
    cursor position are deleted from the acquired curve.
.VARI SEF
The SEF command is for program development only.  Don't try it.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstnav.pdf
PROCEDURE
REFGBL $ECHO
REFGBL $SYSCHAR
BODY
LET _ONFAIL="CONTINUE"
LET $ECHO="NO"

LOCAL GLL_PATH  TYPE=STRING INIT="/project/test_work/testdata/gll/"
LOCAL MIPL_PATH TYPE=STRING INIT="/project/test_work/testdata/mipl/gll/"
LOCAL VGR_PATH  TYPE=STRING INIT="/project/test_work/testdata/mipl/vgr/"
LOCAL CAS_PATH  TYPE=STRING INIT="/project/test_work/testdata/cassini/casIss/"
LOCAL COMMAND   TYPE=STRING INIT="ush"
LOCAL COPY      TYPE=STRING INIT="cp"
LOCAL TO_THIS   TYPE=STRING INIT="."
LOCAL DELETE    TYPE=STRING INIT="rm"
LOCAL SUFFIX    TYPE=STRING INIT=""

IF ($SYSCHAR(1) = "VAX_VMS")
  LET GLL_PATH  = "WMS_TEST_WORK:[TESTDATA.GLL]"
  LET MIPL_PATH = "WMS_TEST_WORK:[TESTDATA.MIPL.GLL]"
  LET VGR_PATH  = "WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
  LET CAS_PATH  = "WMS_TEST_WORK:[TESTDATA.CASSINI.CAS$I$SS]"
  LET COMMAND   = "DCL"
  LET COPY      = "COPY"
  LET TO_THIS   = "*.*"
  LET DELETE    = "DEL"
  LET SUFFIX    = ";*"
END-IF

&COMMAND  &COPY &"VGR_PATH"f1636832.geo   &TO_THIS
&COMMAND  &COPY &"GLL_PATH"s0165034052.u  &TO_THIS
&COMMAND  &COPY &"MIPL_PATH"venus2.img    &TO_THIS
&COMMAND  &COPY &"GLL_PATH"s0349674100.u  &TO_THIS
&COMMAND  &COPY &"GLL_PATH"moon_limb.img  &TO_THIS
&COMMAND  &COPY &"CAS_PATH"n1354897340.1  &TO_THIS
&COMMAND  &COPY &"CAS_PATH"w1364364259.2  &TO_THIS

LET $ECHO="YES"

USE XWC0

ENABLE-SCRIPT tstnav.scr

END-PROC



$!-----------------------------------------------------------------------------
$ create tstnav.scr
! Allocate display device before running this file. First testcase is of a byte
! image.

NAV s0165034052.u SPICEMODE=REMOTE
'limb
'scan
'exit
'n
'exit
'exit
'n

! Next Half word image.

NAV  venus2.img SPICEMODE=REMOTE
'limb
'scan
'exit
'n
'exit
'exit
'n

! Test of gll summation mode image.

NAV s0349674100.u SPICEMODE=REMOTE
'limb
'scan
'exit
'n
'exit
'exit
'n

! Test gspice

NAV moon_limb.img SPICEMODE=REMOTE
'limb
'scan
'exit
'n
'exit
'exit
'n


! Test Voyager (show blems)

NAV f1636832.geo TARGET=IO SPICEMODE=REMOTE +
 blems=/project/test_work/testdata/mipl/vgr/osblemloc.na1
'limb
'scan
'exit
'n
'sblems
'exit
'exit
'n


! Test cassini

NAV n1354897340.1 SPICEMODE=REMOTE
'limb
'scan
'exit
'n
'exit
'exit
'n
$!-----------------------------------------------------------------------------
$ create tstnavstar.pdf
PROCEDURE
REFGBL $ECHO
REFGBL $SYSCHAR
BODY
LET _ONFAIL="CONTINUE"
LET $ECHO="NO"

LOCAL CAS_PATH  TYPE=STRING INIT="/project/test_work/testdata/cassini/casIss/"
LOCAL COMMAND   TYPE=STRING INIT="ush"
LOCAL COPY      TYPE=STRING INIT="cp"
LOCAL TO_THIS   TYPE=STRING INIT="."
LOCAL DELETE    TYPE=STRING INIT="rm"
LOCAL SUFFIX    TYPE=STRING INIT=""

IF ($SYSCHAR(1) = "VAX_VMS")
  LET CAS_PATH  = "WMS_TEST_WORK:[TESTDATA.CASSINI.CAS$I$SS]"
  LET COMMAND   = "DCL"
  LET COPY      = "COPY"
  LET TO_THIS   = "*.*"
  LET DELETE    = "DEL"
  LET SUFFIX    = ";*"
END-IF

&COMMAND  &COPY &"CAS_PATH"w1364364259.2  &TO_THIS

LET $ECHO="YES"

IF ($SYSCHAR(1) = "VAX_VMS")
  ENABLE-SCRIPT tstnavstar_vms.scr
END-IF

IF ($SYSCHAR(1) = "UNIX")
  ENABLE-SCRIPT tstnavstar_unix.scr
END-IF

END-PROC



$!-----------------------------------------------------------------------------
$ create tstnavstar_vms.scr
! Test SAO star catalog w/Cassini image
NAV w1364364259.2 SPICEMODE=REMOTE TARGET=SATURN SAO=WMS_VGR:[000000]SAO_IDL.STR
'star
stretch=(15,40)
'exit
'exit
'n

$!-----------------------------------------------------------------------------
$ create tstnavstar_unix.scr
! Test SAO star catalog w/Cassini image
NAV w1364364259.2 SPICEMODE=REMOTE TARGET=SATURN SAO=/project/test_work/testdata/mipl/vgr/sao_idl.str
'star
stretch=(15,40)
'exit
'exit
'n

$!-----------------------------------------------------------------------------
$ create tstnav.log_solos
tstnav
ush $VRDILIB/usedisp a XWC0
ENABLE-SCRIPT tstnav.scr
END-PROC
NAV s0165034052.u SPICEMODE=REMOTE
Beginning VICAR task NAV
NAV version 06jul2011
(SL,SS)=(   1,   1)  ZOOM= 1
CKNAME=NAIF  SPKID=N083  PROGRAM=ual sh  1993
Specify feature to be fitted
'limb
'scan
Begin manual registration of limb...
Move Cursor to a point on computed limb
'exit
'n
Object Space PC (LINE,SAMP)=(    418.77,   1776.61)  ANGLN= -27.706
 Image Space PC (LINE,SAMP)=(    419.01,   1793.78)
'exit
Specify feature to be fitted
'exit
NAVIGATION DATA FOR FRAME  165034052
S/C Event Time (yyyyddd hhmmssmmm)  SCET  1992343  43559910
Target body                         TARG  MOON
Major equatorial radius (km)        RA      1737.4
Minor equatorial radius (km)        RB      1737.4
Polar radius (km)                   RC      1737.4
Longitude of major eq. radius (deg) LORA    35.58
Spacecraft range (km)               RANG      113839
Spacecraft position (lat,lon(East)) SSP   ( 53.74,  35.58)
O.S. planet center (line,sample)    PC    (    418.77,   1776.61)
I.S. planet center (line,sample)    ISPC  (    419.01,   1793.78)
North Angle (CW degrees from right) ANGLN  -27.71
Camera Serial Number                CAM          1
Focal length (mm)                   FL      1501.039
O.S. optical axis (line,sample)     OAXIS ( 400.0, 400.0)
Scale (pixels/mm at focal plane)    SC     65.6168
Min and max ring radii (km)         RLIM        0.      0.
Solar position (lat,lon(East))      SOL   ( -0.19,  17.14)
All latitudes are planetocentric
CKNAME=NAIF  SPKID=N083  PROGRAM=ual sh  1993
'n
NAV task completed
NAV  venus2.img SPICEMODE=REMOTE
Beginning VICAR task NAV
NAV version 06jul2011
(SL,SS)=(   1,   1)  ZOOM= 1
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
Specify feature to be fitted
'limb
'scan
Begin manual registration of limb...
Move Cursor to a point on computed limb
'exit
'n
Object Space PC (LINE,SAMP)=(    149.11,    481.02)  ANGLN=  88.546
 Image Space PC (LINE,SAMP)=(    149.00,    481.05)
'exit
Specify feature to be fitted
'exit
NAVIGATION DATA FOR FRAME   18494445
S/C Event Time (yyyyddd hhmmssmmm)  SCET  1990044  55847329
Target body                         TARG  VENUS
Major equatorial radius (km)        RA      6051.8
Minor equatorial radius (km)        RB      6051.8
Polar radius (km)                   RC      6051.8
Longitude of major eq. radius (deg) LORA   183.68
Spacecraft range (km)               RANG     1630160
Spacecraft position (lat,lon(East)) SSP   ( -2.95, 183.68)
O.S. planet center (line,sample)    PC    (    149.11,    481.02)
I.S. planet center (line,sample)    ISPC  (    149.00,    481.05)
North Angle (CW degrees from right) ANGLN   88.55
Camera Serial Number                CAM          1
Focal length (mm)                   FL      1501.039
O.S. optical axis (line,sample)     OAXIS ( 400.0, 400.0)
Scale (pixels/mm at focal plane)    SC     65.6168
Min and max ring radii (km)         RLIM        0.      0.
Solar position (lat,lon(East))      SOL   ( -2.58, 230.52)
All latitudes are planetocentric
CKNAME=NAV   SPKID=N083  PROGRAM=  1111  
'n
NAV task completed
NAV s0349674100.u SPICEMODE=REMOTE
Beginning VICAR task NAV
NAV version 06jul2011
(SL,SS)=(   1,   1)  ZOOM= 1
CKNAME=NAV   SPKID=N052  PROGRAM=NAV 77  ADC IC        11/04/96
Specify feature to be fitted
'limb
'scan
Begin manual registration of limb...
Move Cursor to a point on computed limb
'exit
'n
Object Space PC (LINE,SAMP)=(    913.99,   2356.87)  ANGLN=  92.892
 Image Space PC (LINE,SAMP)=(    927.83,   2409.58)
'exit
Specify feature to be fitted
'exit
NAVIGATION DATA FOR FRAME  349674100
S/C Event Time (yyyyddd hhmmssmmm)  SCET  1996178 155044876
Target body                         TARG  JUPITER
Major equatorial radius (km)        RA     71492.0
Minor equatorial radius (km)        RB     71492.0
Polar radius (km)                   RC     66854.0
Longitude of major eq. radius (deg) LORA   329.02
Spacecraft range (km)               RANG     1481862
Spacecraft position (lat,lon(East)) SSP   ( -2.54, 329.02)
O.S. planet center (line,sample)    PC    (    913.99,   2356.87)
I.S. planet center (line,sample)    ISPC  (   1010.99,   2649.91)
North Angle (CW degrees from right) ANGLN   92.89
Camera Serial Number                CAM          2
Focal length (mm)                   FL      1501.039
O.S. optical axis (line,sample)     OAXIS ( 200.0, 200.0)
Scale (pixels/mm at focal plane)    SC     32.8084
Min and max ring radii (km)         RLIM        0.      0.
Solar position (lat,lon(East))      SOL   ( -1.80,   7.16)
All latitudes are planetocentric
CKNAME=NAV   SPKID=N052  PROGRAM=NAV 77  ADC IC        11/04/96
'n
NAV task completed
NAV moon_limb.img SPICEMODE=REMOTE
Beginning VICAR task NAV
NAV version 06jul2011
(SL,SS)=(   1,   1)  ZOOM= 1
CKNAME=NAV   SPKID=N014  PROGRAM=NAV     EZF 5
Specify feature to be fitted
'limb
'scan
Begin manual registration of limb...
Move Cursor to a point on computed limb
'exit
'n
Object Space PC (LINE,SAMP)=(    322.89,    337.82)  ANGLN= 176.852
 Image Space PC (LINE,SAMP)=(    322.89,    337.82)
'exit
Specify feature to be fitted
'exit
NAVIGATION DATA FOR FRAME   61183500
S/C Event Time (yyyyddd hhmmssmmm)  SCET  1990343 235217156
Target body                         TARG  MOON
Major equatorial radius (km)        RA      1737.4
Minor equatorial radius (km)        RB      1737.4
Polar radius (km)                   RC      1737.4
Longitude of major eq. radius (deg) LORA   263.95
Spacecraft range (km)               RANG      720674
Spacecraft position (lat,lon(East)) SSP   (-20.35, 263.95)
O.S. planet center (line,sample)    PC    (    322.89,    337.82)
I.S. planet center (line,sample)    ISPC  (    322.89,    337.82)
North Angle (CW degrees from right) ANGLN  176.85
Camera Serial Number                CAM          1
Focal length (mm)                   FL      1501.039
O.S. optical axis (line,sample)     OAXIS ( 400.0, 400.0)
Scale (pixels/mm at focal plane)    SC     65.6168
Min and max ring radii (km)         RLIM        0.      0.
Solar position (lat,lon(East))      SOL   ( -1.02, 266.85)
All latitudes are planetocentric
CKNAME=NAV   SPKID=N014  PROGRAM=NAV     EZF 5
'n
NAV task completed
NAV f1636832.geo TARGET=IO SPICEMODE=REMOTE  +
 blems=/project/test_work/testdata/mipl/vgr/osblemloc.na1
Beginning VICAR task NAV
NAV version 06jul2011
(SL,SS)=(   1,   1)  ZOOM= 1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
CKNAME=NAV   SPKID=N005  PROGRAM=NAV     GMY059  NONE  01/12/01
Specify feature to be fitted
'limb
'scan
Begin manual registration of limb...
Move Cursor to a point on computed limb
'exit
'n
Object Space PC (LINE,SAMP)=(    537.06,    603.21)  ANGLN= -34.301
'sblems
'exit
Specify feature to be fitted
'exit
NAVIGATION DATA FOR FRAME    1636832
S/C Event Time (yyyyddd hhmmssmmm)  SCET  1979063 192259820
Target body                         TARG  IO
Major equatorial radius (km)        RA      1829.4
Minor equatorial radius (km)        RB      1819.3
Polar radius (km)                   RC      1815.7
Longitude of major eq. radius (deg) LORA     0.00
Spacecraft range (km)               RANG      806060
Spacecraft position (lat,lon(East)) SSP   ( -0.03, 203.53)
O.S. planet center (line,sample)    PC    (    537.06,    603.21)
North Angle (CW degrees from right) ANGLN  -34.30
Camera Serial Number                CAM          7
Focal length (mm)                   FL      1500.190
O.S. optical axis (line,sample)     OAXIS ( 500.0, 500.0)
Scale (pixels/mm at focal plane)    SC     84.8214
Min and max ring radii (km)         RLIM        0.      0.
Solar position (lat,lon(East))      SOL   (  0.54, 188.72)
All latitudes are planetographic
CKNAME=NAV   SPKID=N005  PROGRAM=NAV     GMY059  NONE  01/12/01
'n
NAV task completed
NAV n1354897340.1 SPICEMODE=REMOTE
Beginning VICAR task NAV
NAV version 06jul2011
(SL,SS)=(   1,   1)  ZOOM= 1
CKNAME=NAV   SPKID=N009  PROGRAM=NAV     GMY059  NONE  02/08/03
Specify feature to be fitted
'limb
'scan
Begin manual registration of limb...
Move Cursor to a point on computed limb
'exit
'n
Object Space PC (LINE,SAMP)=(    138.69,    840.44)  ANGLN= -90.090
 Image Space PC (LINE,SAMP)=(    137.86,    841.17)
'exit
Specify feature to be fitted
'exit
NAVIGATION DATA FOR FRAME 1354897340
S/C Event Time (yyyyddd hhmmssmmm)  SCET  2000342 161056162
Target body                         TARG  JUPITER
Major equatorial radius (km)        RA     71492.0
Minor equatorial radius (km)        RB     71492.0
Polar radius (km)                   RC     66854.0
Longitude of major eq. radius (deg) LORA   312.22
Spacecraft range (km)               RANG    23592884
Spacecraft position (lat,lon(East)) SSP   (  3.57, 312.22)
O.S. planet center (line,sample)    PC    (    138.69,    840.44)
I.S. planet center (line,sample)    ISPC  (    138.69,    840.44)
North Angle (CW degrees from right) ANGLN  -90.09
Camera Serial Number                CAM          1
Focal length (mm)                   FL      2000.000
O.S. optical axis (line,sample)     OAXIS ( 512.0, 512.0)
Scale (pixels/mm at focal plane)    SC     83.3333
Min and max ring radii (km)         RLIM        0.      0.
Solar position (lat,lon(East))      SOL   (  2.94, 319.26)
All latitudes are planetocentric
CKNAME=NAV   SPKID=N009  PROGRAM=NAV     GMY059  NONE  02/08/03
'n
NAV task completed
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
$Doc_File:
$ create nav.doc
$ DECK/DOLLARS="$ VOKAGLEVE"

			NAV PROGRAMMER'S GUIDE

This document contains information useful to the programmer responsible
for maintaining and modifying the program NAV.

The NAV program is based on handwritten notes supplied by Andy Ingersoll,
3/19/85 (fundamental algorithms) and 1/2/86 (eccentric ring geometry), and
by Carolyn Porco, 1/12/87 (stellar navigation and Neptune's rings).
Frequent references are made in the program comments to these notes.
The equations originally derived by Ingersoll assumed an oblate spheroid
(ellipse of revolution) target-body model.  These equations have been
generalized for the tri-axial ellipsoid model (Sept 87).  Documentation
for the updated equations is currently being prepared for circulation.

NAV uses an east-longitude, planetocentric latitude convention in its
internal calculations.  All angles are input by the user or SEDR in
degrees, and output to the user in degrees.  All angles are maintained
internally in radians.

Note that most angle computations are performed in double precision.
This is necessary to maintain accuracy to 0.01 degree or pixel.


FACILITY DEPENDENT CODE
-----------------------

The subroutine VGRBLEMS contains a table of file specifications for the
blemish files for each Voyager camera, for both image and object space.
Also, in the PDF, the parameter SAO has a default file specification for
the SAO catalog.   These file specifications should be changed to point
to the correct locations of the corresponding files.


GLOBAL VARIABLES
----------------

All global variables are maintained in common areas:

<Note on ported version:  due to alignment constraints on the Alpha,
 many of these COMMONs were broken up into separate parts containing
 4-byte and 8-byte variables.>

MISCELLANEOUS USER SPECIFIABLE PARAMETERS:
      COMMON/CP/IBUG,NLW,NSW,NSEARCH,IFIT
      COMMON/CPAR/PAR(20),PAR2(20)
      REAL*4 PAR,PAR2
      INTEGER*4 IPAR(20)
      EQUIVALENCE (PAR,IPAR)

  IBUG = debug mode flag.  If IBUG=1, then various diagnostic messages
	are printed out.  Else IBUG=0.
  NLW,NSW = line and sample dimensions of NLWxNSW correlation window used
	in searching for high-contrast limb or ring points.
  NSEARCH = search radius (in pixels).
  IFIT = 2 for CHI2, =3 for CHI3
  PAR,PAR2 = temporary work areas for processing parameters.


IMAGE DATA AREAS:
      COMMON/C1/PIC(1200,1200)
      LOGICAL*1 PIC

  PIC = input image stored as a byte array.

  If the input image is in byte data format, then the image is read
  directly into PIC.  The contents of PIC are never altered and
  always contain an exact copy of the image.

  If the input image is in halfword data format, then VICAR array I/O
  is used to map the image on disk into virtual memory.  The address of
  the start of the file is stored in the pointer HPIC.

  Subroutines which require access to the image data reference pixels as
  PIC(samp,line) or HPIC(samp,line), where line and samp are the image
  coordinates of the pixel.  These arrays are passed to the subroutines as:
	LOGICAL*1 PIC(NS,NL)
        INTEGER*2 HPIC(NS,NL)
  where NL and NS are the line and sample dimensions of the image.
  If the input image is byte, PIC is always accessed.  If the input image
  is halfword, HPIC is accessed, except for subroutines which handle
  the image display.

  Halfword image data must be converted to byte format before it can be
  written to the display device.  Since it may be necessary to re-display
  the image frequently, the following strategy is used to minimize the
  number of times this data conversion is performed:  The halfword image
  is initially read into HPIC.  The image is then converted to byte format
  and stored in PIC.  The data conversion uses a histogram ends-in algorithm
  (identical to program FIT) to determine the dynamic range of the input DN
  values (see subroutines READIMAGE, HSUB2, ASTRC2, and HWTOBYTE).  The
  contents of PIC are then written to the display device (see subroutine
  DPIC).  The contents PIC are only altered if it is necessary to change
  the dynamic range of the DNs as a result of a STRETCH or ASTR command
  (see subroutine HSTRETCH).


CAMERA RESEAU AND BLEMISHES:
      COMMON/CRES/RES(2,202),BLEM(4,1000),NBLEM
      REAL*4 RES,BLEM

  RES = VGR reseau locations stored as (line,sample) pairs.
  BLEM = VGR blemish locations stored as (N,DL,DS,R) 
        where N is the reseau mark number, (DL,DS) are the line-sample
	offsets from the reseau mark, and R is the pixel radius of the
	blemish (see DRAWBLEM routine).
  NBLEMS = number of blemishes in BLEM.

TEMPORARY WORK BUFFERS:

  WORK1,WORK2,WORK3 = temporary work areas.  WORK1 and WORK2 are used
  whenever the picture is displayed.  WORK3 is used whenever a limbtrace
  or ringtrace is done.  HIST is used to compute a halfword histogram
  and convert the halfword data to byte.


LIMB DATA AREA
      COMMON/CLIMB/ISL,ISS,INL,INS
      COMMON/CLIMB/NPTS,LPTS(2,3000),ALPTS(2,3000)
      REAL*4 LPTS,ALPTS

  ISL,ISS,INL,INS = image size field.  When specified, the limb scan will
	be limited to an INLxINS area of the image.  The upper-left
        corner of the area is at (starting-line,starting-sample)=(ISL,ISS).
  NPTS = total number of points in array LPTS (and ALPTS).
  LPTS,ALPTS = locations of computed and actual limb points, stored as
	(line,sample) coordinates.  Obscured points are flagged as
        (-line,-sample) and bad points are flagged as (-99.0,-99.0).


RING DATA AREA:
      COMMON/CRINGI/JSL,JSS,JNL,JNS,NRPTS,ETYPE
      COMMON/CRINGI/NRF,RINGID(10),EDGE(10),PLANE(10)
      COMMON/CRING/RPTS(2,20000),ARPTS(2,20000),RADII(10),RWIDTH(10)
      COMMON/CRINGC/ RINGS
      REAL*4 RPTS,ARPTS,RADII,RWIDTH
      INTEGER*4 ETYPE,RINGID,EDGE,PLANE
      CHARACTER*1 RINGS(15)

  JSL,JSS,JNL,JNS = image size field.  When specified, the ring scan will
	be limited to an JNLxJNS area of the image.  The upper-left
        corner of the area is at (starting-line,starting-sample)=(JSL,JSS).
  ETYPE = 1 for the outer edge of the ring, 2 for the inner edge,
	and 3 for a thin ring.
  RINGS = ASCII names of up to 15 rings.
  NRPTS = total number of points in array RPTS (and ARPTS).
  RPTS,ARPTS = locations of computed and actual ring points, stored as
	(line,sample) coordinates.
  NRF = Number of rings used in fit (up to 10)
  RINGID,EDGE,PLANE,RADII,RWIDTH = Ring, Edge type, plane, radii and width
        of the NRF number of rings for fit.

STAR DATA AREA:
      COMMON/CSTAR/NSTARS,STARS(3,2000),SPTS(2,2000),LOMAG,HIMAG
      COMMON/CSTAR1/ISAO,SAOFILE
      COMMON/CSTAR2/STARNAME,STARTYPE
      REAL*4 STARS,SPTS
      INTEGER*4 HIMAG,LOMAG
      CHARACTER*72 SAOFILE
      CHARACTER*13 STARNAME(2000)
      CHARACTER*3 STARTYPE(2000)

  NSTARS = number of stars in array STARS and SPTS.
  STARS = (right-ascension,declination,visual-magnitude) of each star.
  SPTS = (line,sample) coordinates of each star.
  LOMAG = lowest magnitude star in display.
  HIMAG = highest magnitude star in display.
  SAOFILE = file specification for SAO catalog.
  STARNAME = name of each star in display (if available)
  STARTYPE = type (usually spectral) of each star in display (if available)


DISPLAY DEVICE CONTROL DATA:
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

  IDEV = logical unit number of display device
  VID  = logical unit number of video plane
  G    = logical unit number of graphics plane
  TB   = logical unit number of (trackball driven) cursor
  NLDS,NSDS = hardware display format, specified as an NLDSxNSDS area.

  IZOOM = displayed-picture scale factor.  If IZOOM is greater than 1,
        then the image is magnified to IZOOM times normal resolution.
	If IZOOM is less than 1, then the image is reduced by IZOOM
	times normal resolution.  Note that image magnification or
	reduction is restricted to an integral factor.

  ZOOM = display picture-scale (displayed pixel per image pixel).  E.g.
	if ZOOM=2.0, then the image will be displayed at twice normal
	resolution.  ZOOM and IZOOM are related as follows:

		IF (IZOOM.GT.0) ZOOM=IZOOM
                IF (IZOOM.LT.0) ZOOM=-1/IZOOM

  ILOW,IHIGH = low and high (linear) stretch limits.
  STBL = stretch table:  output_dn = STBL(input_dn+1)

HISTOGRAM DATA AREA:
      COMMON/CHIST/HIS(256),HFLG,HBEG,HINC,NSPIKES,LHPER(2)
      INTEGER HIS,HFLG,HBEG,HINC
      REAL*4 LHPER

  HIS = Histogram of displayed image area.
  HFLG = Histogram flag=1 if histogram for displayed area exists.
         HFLG=0 whenever new image area is displayed.
  HBEG,HINC = beginning DN and DN increment when histogram represents
        compressed version of halfword data.
  NSPIKES = specifies that nth largest frequency is to be set to maximum
        scale on histogram display.
  LHPER = low and high percentages used to saturate data during
	an ends-in stretch.

INPUT IMAGE CONTROL DATA:
      COMMON/IPIC/IMG,SL,SS,NLX,NSX,ICODE
      INTEGER*4 SL,SS

  IMG = logical unit number for input image
  SL,SS = (line,sample) coordinates of upper left corner of display.
  NLX,NSX = total number of lines and samples in the image.
  ICODE = image data format code (=1 for byte, =2 for halfword).

IMAGE IDENTIFIERS:
      COMMON/CPIC/IPROJ,ICAM,FRAME_ID,PLANET_ID
      COMMON/CPIC/TARGET_ID,IDATE,ITIME
      COMMON/CPICX/T0,T
      INTEGER*4 IPROJ,ICAM,FRAME_ID,TARGET_ID

  IPROJ=1 for Mariner 9, 2 for Mariner 10, 3 for Viking Orbiter,
	4 for Voyager, 5 for Galileo and 6 for Cassini.  Currently,
        only Voyager, Galileo and Cassini are supported.

  ICAM = camera serial number.  The valid VGR camera serial
	numbers are:

	4 = VGR-2 WA		6 = VGR-1 WA
	5 = VGR-2 NA		7 = VGR-1 NA

  	For Galileo, the camera serial number=1.

        Valid Cassini camera serial numbers are:
        1=NAC  21=NAC 2x2 summation mode  41=NAC 4x4 summation mode
        2=WAC  22=WAC 2x2 summation mode  42=WAC 4x4 summation mode


  FRAME_ID = frame number of the input image.  For VGR, this is the
	FDS count stored as XXXXXYY where XXXXX is the Mod16 count
	and YY is the Mod60 count.  For Cassini, frame number is
        the SCLK, a 10-digit number.

  PLANET_ID = 1 (Mercury), 2 (Venus), 3 (Earth),...

  TARGET_ID = SPICE target ID.  First digit=planet (Earth=3),
        next two digits=satellite or 99 for planet (see PLANET_ID).

  IDATE = year-of-century and day-of-year stored as YYDDD.
  ITIME = time of day stored as HHMMSSMMM.
  T0 = time of Epoch for ring co-ordinate system.  For Uranus, 20:00 UT,
       March 10, 1977, expressed in days from EME50 (Jan 1, 1950)
  T = SCET expressed in days from EME50.
  T and T0 should probably be expressed as Julian dates.  The useful
  quantity is actually T-T0.


IMAGE GEOMETRY DATA:
      COMMON/SEDR/SEDR(200),LBUF(80),ISYSTEM,P_SOURCE
      CHARACTER*4 P_SOURCE
      REAL*4 SEDR
      INTEGER LBUF

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

      COMMON/PCONST/AI2,BI2,CI2,AI2XS,BI2YS,CI2ZS,CTERM
      COMMON/PCONST/A0,B0,B1,B2,C0,C1,C2

  SEDR = SEDR buffer returned by GETSPICE containing image geometry info.
  LBUF = Data buffer returned by GETLABCON containing picture label info.
  ISYSTEM=1 for J2000, =2 for EME50.  EME50 is used for all missions up to
	and including Voyager.  J2000 is used beginning with Galileo.  
  P_SOURCE = C-matrix source (Mert Davies, FARENC, etc).

  MODEL = target model.  1=sphere, 2=oblate spheroid, 3=ellipsoid, and
	4=ring plane.  Note that only the ellipsoid and ring plane geometry
	are implemented.  The sphere and oblate spheroid are special cases
	of the ellipsoid and are covered with this model.
         
  IGEO = 0 if user latitudes are planetocentric, =1 if planetographic
        NAVIGATE treats all latitudes internally as planetocentric.  If
        GEOFLAG=1, then all latitudes input to the user are converted
        from planetographic to planetocentric, and all latitudes output to
        the user are converted from planetocentric to planetographic.

  MRING = master ring ID.  Specifies reference frame in which ring
        measurements are to be made (see C command).
  NRINGS = number of ring reference planes supported (planet's equatorial
	plane plus the eccentric ring planes for Uranus).

  FL = camera focal length (in mm).
  OAL,OAS = (line,sample) coordinates of the optical axis intercept.
  PSCALE = picture scale (pixels/mm at the focal plane).
  ZSCALE = camera constant = FL*PSCALE.

  ROT = planet's rotation period (in days).
  RA,RB,RC = planet's major equatorial, minor equatorial and polar radii (km).
  RLORA = longitude of the major equatorial radius (RA).

  RMIN,RMAX = minimum and maximum ring radii (in km).  These values are
	only used in the limb-fit algorithm to avoid limb areas obscured by
	the ring plane.
  ASDSUN = azimuthal diameter of sun (unimplemented for now...)

  ALPHAp,DELTAp = right ascension and declination of Uranus' pole
      (assumed constant).
  THETA,ZETAZ = angles defining orientation of Earth's pole at time T0.
  PHIp = Longitude of French's 1977 reference meridian, measured in the
    equatorial plane eastward from the prime meridian (node of Uranus
    equator on EME1950.

  CM = Transformation matrix from camera to celestial coordinates.
  PSC = Unit vector from target center to spacecraft in celestial coordinates.
  PSUN = Unit vector from target center to sun in celestial coordinates.
  PSAT = Unit vector from planet center to satellite in celestial coordinates.
  RSC = Distance from spacecraft to target center (km).
  RSUN = Distance from sun to target center (km).

  ME = Transformation matrix from target reference plane to celestial
       coordinates.  The target may be the planet (or satellite), the
       planet's ring (equatorial) plane, or the inclined plane of an
       eccentric ring.

  OM = Transformation matrix from camera's (x0,y0,z0) to planet's
	(x3,y3,z3) coordinates.  Note that this differs from the
	traditional definition of the OM-matrix.

  ANGLN,ANGLA,ANGLB = components of OM-matrix.  ANGLN is the angle
	of the planet's projected spin axis, measured counterclock-
	wise from the x0-axis.

  PSC3 = Vector from planet center to spacecraft in (x3,y3,z3) coordinates.
  SCLAT,SCLON = Spacecraft position (lat,lon).
	Note that:  PSC3(1) = RSC*DCOS(SCLAT)
		    PSC3(2) = 0
		    PSC3(3) = RSC*DSIN(SCLAT)

  PSUN3 = Vector from planet center to sun in (x3,y3,z3) coordinates.
  SUNLAT,SUNLON = Solar position (lat,lon).
	Note that: PSUN3(1) = RSUN*DCOS(SUNLAT)*DCOS(SUNLON-SCLON)
		   PSUN3(2) = RSUN*DCOS(SUNLAT)*DSIN(SUNLON-SCLON)
		   PSUN3(3) = RSUN*DSIN(SUNLAT)

  SMAA = semi-major axis (km) of ring
  ECC = eccentricity of ring
  INCL = inclination angle of ring plane
  OMEGAZ = longitude of ascending node measured at epoch (1977)
  PIZERO = longitude of periapse measured at epoch (1977)
  dOMEGA_dt = nodal precession rate (radians/day)
  dw_dt = apsidal precession rate (radians/day)

  AI2,BI2,CI2,et.al. = constants used by projection and limbfit routines
	(see PLANET, PLAINV, LIMBPT, GETLIMB).  These are all functions
	of RA, RB, RC, and PSC3 and are computed by GETPC.

NAVIGATION DATA SAVE AREAS:
      COMMON/CURR/CUR(71),PCCUR(13)
      COMMON/LAST/LAS(71),PCLAS(13)
      COMMON/ORIG/ORG(71),PCORG(13)
      COMMON/RSAV/RSAVE(38,15)
      REAL*8 LAS

  CURR contains the current version of the data. 
  LAST contains the last saved version of the data.
  ORIG contains the original data from the SEDR.

  Each of these three save areas contain all the navigation data for the
  planet and each of the rings.  The buffers in the each save area
  corresponds to the following data areas in COMMON/CMAP/ and /PC/:

  CUR,LAS,ORG = FL to dw_dt (this data is common to both planet and rings)
  RSAV = ME to dw_dt for each ring coordinate system
  PC = corresponds to COMMON/PC/

  COMMON/CMAP/ contains either the planet geometry, or one of the ring
  geometries, and is the buffer which all the navigation routines use.

  GETNAV moves the planet geometry from CURR to CMAP (inverse is PUTNAV)
  GETRING moves a ring geometry from RSAV to CMAP (inverse is PUTRING)
  GETLAST moves LAST to CURR (inverse is SAVELAST).  GETNAV is then called.
  GETSEDR moves ORIG to CURR (inverse is SAVESEDR).  GETNAV is then called.


CONVERSION CONSTANTS:
      COMMON/CONST/PI,DEGRAD,RADDEG

  PI = 3.141592653589793D0
  DEGRAD = degrees-to-radians = PI/180.D0
  RADDEG = radians-to-degrees = 180.D0/PI

NAV SUBROUTINES
---------------

The following are brief descriptions of the subroutines comprising the
NAV program:

Parameter processing routines:
  FRAMEID	Get frame and camera ID from image label
  RPARAM	Edit processing parameters
  PARMTST	Checks for existence of parameter and returns integer value
  DPARMTST	Checks for existence of parameter and returns real*8 value
  RPARMTST	Checks for existence of parameter and returns real*4 value
  PARMTST1	Check for existence of parameter
  UPRCASE       Converts lowercase letters to uppercase and numbers to spaces

Reading and converting the input image:
  READIMAGE	Reads byte image into PIC or halfword image into HPIC
  HWTOBYTE	Converts halfword image (HPIC) to byte image (PIC).

SPICE interface:
  SPICESUB	Get SPICE data for input frame
  NOSPICE       Handle case where SPICE is not available
  GETSCET       Get Spacecraft Event Time and target ID
  GETPC		Compute projection constants
  UPDTSEDR      Update Voyager SEDR for input frame
  DOT           Returns dot product of two vectors
  ROTATE1	Multiplies 3x3 matrix by a rotation matrix
  ORTHOT	Tests a rotation matrix for orthogonality
  PRINTMTX      Print a 3x3 matrix

Geometric camera distortion routines:
  MAPLABEL	Returns image type (7=image-space, 8=object-space)
  GETRESLOC	Retrieves reseau locations
  GETGEOPAR	Computes camera distortion parameters
  VGRBLEMS	Retrieve camera blemish locations

Limb-fit routines:
  LIMBFIT	Controlling routine
  LIMBSCAN	Limb-scan algorithm
  LIMBTRACE	Limb-trace algorithm
  LIMBPT	Generate computed limb (LPTS)
  GETLIMB	Given LAT, find limb point (x,y,z)
  GETLS		Find (line,sample) coordinates for limb point
  MOVELIMB	Manual registration of graphics limb
  SEARCH	Search for high-contrast points

Ring-fit routines
  RINGFIT	Controlling routine
  RINGSCAN	Ring-scan algorithm
  RINGTRACE	Ring-trace algorithm
  PREDICT	Predict position of object in frame from pos in another frm
  PHASPROF	Compute constant-phase profile of a ring sector
  PROFILE	Compute radial or longitude profile of a ring sector
  RINGIDS	Get ring ID and ring data
  RINGPTS	Compute ring points for a system of rings
  RINGPT	Compute points at a constant radius
  RINGPT2	Compute points btwn starting and ending longitudes
  MOVERING	Enable user to manually register ring
  RSEARCH	Search for high-contrast ring points
  TRACECRV	Enable user to trace a curve via cursor
  FINDPT	Find the closest (acquired) point to cursor.
  SRCHINV	Given high-contrast points, find points on computed curve

Starfit routines
  STARFIT	Controlling routine
  GETSTARS	Retrieve stars from the SAO catalog
  STARHIST	Compute and print histogram of visual magnitudes
  MOVESTAR	Manually register the starmap to the star background
  STARPTS	Acquire star tiepoints for MOVESTAR.
  UPDATESTAR	Update star map based on current nav information
  FINDSTAR	Find the closest star (in starmap) to cursor.
  VGRCLEAR	Flags points that are near reseau marks.
  TIESTAR	Estimates C-matrix using positions of two stars.
  HMSTORAD	Hours-min-sec to radians
    DMSTORAD	Degrees-min-sec to radians
  RADTOHMS	Radians to hours-min-sec
    RADTODMS	Radians to degrees-min-sec
  FROMORTO_STAR	Convert (ra,dec) between NAV ref system and catalog ref system
  FROMORTOSAO   Convert (ra,dec) between NAV ref system and SAO ref system
  FROMORTOGSC   Convert (ra,dec) between NAV ref system and GSC ref system

Eccentric ring geometry
  T1950		Compute time since 1950 in days
  GETEPOCH	Compute longitude and time of standard epoch
  ERING0        Initialize nav data for eccentric rings
  RINGIN	Read ring orbital elements from file
  RINGOUT	Write ring orbital elements to file
  ME0CALC       Compute ME matrix for equatorial plane in unrotated system
  MECALC        Compute ME matrix for an eccentric ring
  ERING         Edit orbital data for eccentric rings
  RSTATUS       List orbital data for eccentric ring

Deleting bad points on curve
  CAREA		User specifies image area to limit scan
  CLEANPTS	Enable user to manually delete bad points
  CLEANPT1	Delete isolated points
  CLEANVGR	Delete points near VGR reseau marks

Camera pointing routines
  GETANGLES	Compute ANGLN,ANGLA,ANGLB (of OM-matrix)
  GETANGLES2	Compute THETA_N,THETA_A,THETA_B (of C-matrix)
  COMPCM	Compute 3 Euler angles from C-matrix
  OMMATRIX	Compute OM-matrix from ANGLN,ANGLA, and ANGLB
  CMATRIX       Compute C-matrix from ME and OM' matrices
  FROMEME	Computes lat-lon from unit vector in EME50 coordinates
  TOEME		Computes unit vector in EME50 coordinates from lat-lon
  VECTOR3	Computes vector in planet coordinates from lat-lon and range
  CHISQ2	Compute offsets DL,DS which minimize diffs between curves
  COVMT2        Print CHISQ2 covariance matrix
  CHISQ3        Compute DL,DS, and DT which minimize diffs between curves
  COVMT3        Print CHISQ3 covariance matrix
  FIT2		Compute offsets DL,DS which minimize diffs between tiepoints
  FIT3		Compute DL,DS, and DT which minimize diffs between tiepoints 
  MOVE1		Given (DL,DS), updates ANGLA and ANGLB (or THETA_A,THETA_B)
  MOVE2         Alternate algorithm for MOVE
  UPDATENAV     Update C-matrix and all OM matrices
  UPDTRING	Updata all ring coordinate systems: OM,PSC3,PSUN3,et.al.
  GETNAV/PUTNAV Get/put current navigation data in planet reference system
    GETRING/    Get/put current nav data in ring-plane reference system
      PUTRING
    GETLAST/	Get/save last navigation data
      SAVELAST
    GETSEDR/    Get/save original navigation data from SEDR
      SAVESEDR

Projection routines
  PLANET	Transforms from (line,sample) to (lat,lon) on the planet
  PLAINV	Transforms from (lat,lon) to (line,sample)
  RING		Transforms from (line,sample) to (radius,lon) on the ring
  RINV		Transforms from (radius,lon) to (line,sample)
  STAR		Transforms from (line,sample) to (ra,dec) on the cel. sphere.
  STARINV	Transforms from (ra,dec) to (line,sample)
  LATLON	Calls either PLANET, RING, or STAR depending on MODEL
    LINSAM      Calls either PLAINV, RINV, or STARINV depending on MODEL
  PHASE		Computes phase angle
  GEOCEN        Converts from planetographic to planetocentric latitudes
  GEODET        Converts from planetocentric to planetographic latitudes
  IMG2OBJ       Converts from image line,sample to camera unit vec
    OBJ2IMG       inverse of obj2img
  ROTVEC        Rotate one vector around an axis represented by a unit vector
  ROTVECS2IMG   Rotate a camera vector around an axis, convert to image space, 
                  update array of image space line,samp, count how many are 
                  within image space limits


Edit navigation data
  EDITNAV	Edit navigation data
  EDITCM	Edit the C-matrix via Euler angles input by user.
  EDITME	Edit the ME-matrix via Euler angles input by user.
  EDITPSC	Edit the spacecraft vector via components in celestial coord.
  BUILDCM	Computes C or ME matrix from Euler angles.
  FARENC	Given (line,sample) of PC, comutes ANGLA,ANGLB,OM
  PNAV		Prints summary of navigation data

Display routines
  DISPLAY	Controlling routine for general display capabilities
  PDISPLAY      Controlling routine for planet display
  RDISPLAY      Controlling routine for ring display
  SDISPLAY      Controlling routine for star display
  PRDISPLAY     Graphics plot of ring profile 
  DRAWCURVE	Draw the curve (limb or ring) in graphics
  DRAWBLEMS	Draw the blemishes in graphics
  DRAWSTARS	Draw the starmap in graphics
  DRAWDOT       Draw a dot in the graphics plane
  OVERLAY1	Draw latitude-longitude grid in graphics plane
    OVERLAT     Draw a parallel at specified latitude in graphics
    OVERLON     Draw a meridian at specified longitude in graphics
  HOME		Display the entire image (at possibly reduced resolution)
  DPIC		Display an image area (controlling routine)
  MAGLD		Magnify image vertically
  MAGS		Magnify the image horizontally
  HISTGEN1	Compute a histogram of the displayed image area (byte)
  HISTGEN2	Compute a histogram of the displayed image area (halfword)
  HSUB2		Accumulates a histogram of halfword data.
  HDISPLAY      Display a histogram in graphics plane
  STRECH	Compute linear stretch table
  HSTRETCH	Converts halfword stretch limits to byte limits.
  ASTRC2	Determines low and high DN limits of an ends-in stretch.
  CURSOR	Read the cursor position
  DEVICE        Opens display device and obtains device characteristics
  AVLINE        Writes one or more lines to image display
  LUTWRITE      Writes look-up table to display device


MIPL LIBRARY SUBROUTINES
------------------------

Calls to the following subroutines occur quite frequently in MIPL programs
and should be installed on your system if you plan to import a significant
amount of MIPL software:

Copying arrays from memory to memory:
  MVL		Moves an array of bytes
  MVE		Moves an array of bytes, INTEGER*2, etc.

Miscellaneous routines:
  ITLA		Fills a logical array with an integer constant.
  ZIA		Zeroes out in integer array.
  SUM		Computes the sum of an array.
  INTRPA	Fills an array, interpolating between the two end-points.
  CMPR		Compares two strings.
  HSUB		Accumulates a histogram
  TRITRA	Convert (line,sample) coords from image-space to object-space.
  INSIDE	Determine if point is inside a quadrilateral
  INTERG	???
  TRIAG		???
  SIMQ		Solves system of simultaneous linear equations.
  ARGQ		Returns number of arguments passed to calling subroutine.
  NARGS		Same as ARGQ.
  CHKSTAT	Check I/O return status
  ABEND		Terminates program with abnormal end message.

Planet constants:
  PBNAME/PBID   Correspondence between SEDR target body ID and target name.
  PBDATA        Returns planet radii, rotation rate, etc for a given target.

Image navigation routines:
  MOMATI	Computes the OM matrix, given the planet center.
  BUILDC	Compute C-matrix from the 3 Euler angles.
  TOJ2000	Converts from EME50 to J2000 reference

Voyager specific routines:
  VGRCAM	Returns camera focal length, picture scale, and optical axis
		intercept point.
  VGROS		Get Voyager reseau locations
  MWATNA	Transforms from WA (line,sample) to NA (line,sample).
  ABLE77V2	Reads VICAR picture label and extracts image info.
  CMSOURCE	Determine source of camera pointing data (C-matrix, etc.)
  GEOMAV	Compute geometric camera distortion parameters.


NAV SUBROUTINE DEPENDENCE (routines called in parenthesis)
----------------------------------------------------------
NAV(readimage,frameid,maplabel,vgrblems,getresloc,getgeopar)
    limbfit,ringfit,starfit,
    spicesub,getnav,putnav,getangles,ommatrix,
    updtsedr,savesedr,savelast,pnav,
    device,home,devoff)
READIMAGE(hsub2,astrc2,hwtobyte)

SPICESUB(nospice,nonflightspice,getscet,vector3,getpc,toeme,fromeme,
         printmtx,orthot)
NOSPICE(me0calc),GETSCET,GETPC,UPDTSEDR(plainv,ommatrix,vector3)
NONFLIGHTSPICE(toeme,vector3,getpc,getangles,ommatrix,pnav)

LIMBFIT(limbscan,limbtrace,rsearch2,cleanvgr,cleanpt1,
        getnav,editnav,rparam,display,pdisplay,drawcurve)
LIMBSCAN(limbpt,movelimb,carea,search,cleanvgr,cleanpts,cleanpt1,
         chisq2,chisq3,move1,rotate1,getangles,ommatrix,plainv,updatenav,
         display,pdisplay,drawcurve,rparmtst)
LIMBTRACE(limbpt,tracecrv,srchinv,
         chisq2,chisq3,move1,rotate1,getangles,ommatrix,plainv,updatenav)
LIMBPT(getlimb,getls)
MOVELIMB(limbpt,move1,ommatrix,cursor,drawcurve)

RINGFIT(ringscan,ringtrace,ringpt,rsearch2,cleanvgr,
        getnav,getring,getsedr,ringin,getepoch,t1950,ering0,
        predict,editnav,updatenav,
        display,rdisplay,drawcurve,uprcase)
RINGSCAN(ringids,ringpts,movering,carea,rsearch,cleanpts,cleanpt1,cleanvgr,
         chisq2,chisq3,rotate1,getangles,move1,getnav,updatenav,
         display,drawcurve)
RINGTRACE(ringids,getring,ringpt,srchinv,
         chisq2,chisq3,rotate1,getangles,move2,getnav,updatenav,drawcurve)
PREDICT(getring,latlon,linsam,cursor)
RINGIDS(getring,ringpt,tracecrv,drawcurve,ringpts)
RINGPT2(rinv)
MOVERING(getring,ringpt,move2,getnav,updatenav,cursor,drawcurve)
TRACECRV(findpt,display,cursor,drawdot,drawcurve)
FINDPT(cursor)

PHASPROF(getring,editnav,img2obj,rotvecs2img,rotvec,rotvecs2img,
         display,rdisplay,prdisplay,drawcurve,cursor)
PROFILE(getring,ringpt2,linsam,rinv,display,rdisplay,prdisplay,drawcurve)

STARFIT(getstars,starhist,drawstars,movestar,tiestar,
        getnav,editnav,updatestar,display,sdisplay)
GETSTARS(fromorto_star,compcm,radrec,seagsc,updatestar)
MOVESTAR(starpts,getangles2,fit2,fit3,move1,ommatrix,
         starinv,updatenav, updatestar)
STARPTS(findstar,vgrclear,display,drawdot,drawcurve,drawstars,cursor)
UPDATESTAR(starinv)
FINDSTAR(cursor)
TIESTAR(hmstorad,dmstorad,ommatrix,getangles,updatenav,drawdot,cursor)
FROMORTO_STAR(fromortogsc,fromortosao)
FROMORTOSAO(radrec,recrad)
FROMORTOGSC(radrec,recrad)

ERING0(me0calc,mecalc,fromeme,vector3,getangles,ommatrix,getring,putring)
RINGOUT(getring)
ERING(ringpt,me0calc,fromeme,vector3,getangles,ommatrix,getepoch,ering0,
      rstatus,getring,putring,uprcase)

CMATRIX(ommatrix)
CHISQ2(covmt2)
CHISQ3(covmt3)
MOVE2(plainv,getangles)
UPDATENAV(getnav,putnav,getring,putring,getangles,ommatrix,cmatrix,plainv)
UPDTRING(getnav,putnav,getring,putring,getangles,ommatrix,fromeme,vector3)

LATLON(planet,ring,star)
LINSAM(plainv,rinv,starinv)

EDITNAV(spicesub,getangles,ommatrix,cmatrix,vector3,geocen,farenc,toeme,
        editcm,editme,editpsc,t1950,ering0,
        getnav,putnav,pnav,getpc,getsedr,savesedr,getlast,savelast,updtring,
        display,pdisplay,rdisplay)
EDITCM(buildcm,getangles,ommatrix)
EDITME(buildcm,fromeme,vector3,getpc,getangles,ommatrix)
EDITPSC(fromeme,vector3,getpc,getangles,ommatrix)
FARENC(getangles,ommatrix)
PNAV(geodet,plainv)

DISPLAY(home,dpic,histgen1,histgen2,astrch,astrc2,strech,hstretch,hdisplay,
        lutwrite,drawcurve,drawblems,cursor)
PDISPLAY(overlat,overlon,linsam,latlon,phase,geodet,drawdot,cursor)
RDISPLAY(profile,phasprof,getring,linsam,latlon,phase,drawdot,cursor)
SDISPLAY(fromorto_star,radtohms,radtodms,linsam,latlon,drawdot,cursor)
OVERLAY1(plainv,drawdot,drawcurve)
OVERLAT(plainv,drawdot,drawcurve)
OVERLON(plainv,drawdot)
HOME(dpic)
DPIC(avline,magld,mags)
HISTGEN2(hsub2)
HSTRETCH(hwtobyte,dpic)
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create nav.imake
#define PROGRAM nav

#define MODULE_LIST nav.f spicesub.f limbfit.f ringfit.f phasprof.f \
                    starfit.f t1950.f carea.f getangles.f planet.f \
                    editnav.f display.f dpic.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport mp_for_defs
#define R2LIB
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_VRDI
#define LIB_NETWORK
#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN

/*#define DEBUG	/* comment out on delivery */
/*#define LIB_LOCAL	/* comment out on delivery */
$ Return
$!#############################################################################
