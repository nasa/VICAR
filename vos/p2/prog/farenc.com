$!****************************************************************************
$!
$! Build proc for MIPL module farenc
$! VPACK Version 1.9, Tuesday, July 26, 2005, 09:41:40
$!
$! Execute by entering:		$ @farenc
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
$ write sys$output "*** module farenc ***"
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
$ write sys$output "Invalid argument given to farenc.com file -- ", primary
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
$   if F$SEARCH("farenc.imake") .nes. ""
$   then
$      vimake farenc
$      purge farenc.bld
$   else
$      if F$SEARCH("farenc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake farenc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @farenc.bld "STD"
$   else
$      @farenc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create farenc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack farenc.com -mixed -
	-s farenc.f -
	-i farenc.imake -
	-p farenc.pdf -
	-t tstfarenc.pdf tstfarenc.log_solos tstfarenc.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create farenc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C VICAR program FARENC: Fit planet limb to a circle or ellipse.
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      INCLUDE 'mp_for_defs'

      INTEGER*2 MAV(1400),MIV(1400),SMAV(1400),SMIV(1400),MAH(1400),
     *   MIH(1400),LMAH(1400),LMIH(1400),SAMP(5600),ACT(1400),
     *   BUF(1400,3)

      parameter (maxtasks=100)
      character*8 tasks(maxtasks)
      integer inst(maxtasks)
      character*1440 cbuf
      LOGICAL XVPTST
      logical provenance

      INTEGER*4 DNTH,ACTTH,DIST,PARMS(50),SERNO,IG(2700)
      INTEGER*4 Z,FDS,SCID,DSRNGE,W,UPDATE,CAM,SR,IPIX
      INTEGER*4 INP(2),OUT(3),SL,SS,idata(40),old,STATUS

      INTEGER*4 IBUFSE(200),ilab(80),ip(2800)
      REAL*8 mp

      REAL*4 RPARM(50),CL(2800),C(2800,5),COEF(5),ER(2800),ELICOE(5)
      REAL*4 LMBPTS(2800,2),rdata(40),os_pts(2800,2)
      REAL*4 is_line,is_samp,os_line,os_samp,c2(2800),cptr(2800)

      REAL*8 C8(1400,5),C8L(1400),LMBPT8(1400,2),bufse(100)
      REAL*8 LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,OM(9),RS(3)
      REAL*8 VABS,r8

      BYTE WHITE,BLACK,GREY
      BYTE LOG(7200)
      CHARACTER*12 PLANET
      character*5 project
      CHARACTER*32 FORMAT,proj_type
      character*4 FARE
      character*80 TMPBUF(3)
      character*160 msg

      EQUIVALENCE(C8,C,PARMS,RPARM,BUF),(C(2101,1),ACT),(C(1,3),SAMP),
     *    (LMBPTS(1,1),LMBPT8(1,1)),(LOG,C(1,5),ER,MAV),(ER(701),MIV),
     *    (ER(1401),SMAV),(ER(2101),SMIV),(C8L,CL,MAH),
     *    (CL(701),MIH),(CL(1401),LMAH),(CL(2101),LMIH)
      EQUIVALENCE (BUFSE,IBUFSE)

      COMMON/FARENC_FILES/INP,OUT
      COMMON/C1/ C8L,C8,LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,OM,RS

      DATA LIMIT/2800/,FOCA/-400./,SCAL/-400./,REQU/-400./,RPOL/-400./,
     *   FAR/-400./,RLATI/-400./,RLONGI/-400./,IDSRNP/-400./,IDSRNL/1/,
     *   ANGL/-400./,SMAA/-1./,SMIA/-1./,MEDI/0/,IBELOW/-32760/,FDS/0/,
     *   DIST/10/,DNTH/-32760/,ACTTH/55/,PERC/.20/,SIGM/1.7/,SIGA/2.5/,
     *   TOL/1.0/,NSWH/1/,IPRINT/0/,ISL/4/,ISS/1/,DELL/0.0/,DELS/0.0/,
     *   SERNO/0/,W/1/,SUNA/-400./,PUTNOR/-400./,SIZE/-1.0/,ICLUW/30/,
     *   ICLUN/24/,IAUTO/0/,LINC/5/,ASTRE/0.2/,LCIRCL/0/,RADRNG/0.0/,
     *   CY/0.0/,CENL/-999./,CENS/-999./,CX/0.0/,UPDATE/0/,LO/500.0/,
     *   SO/500./,fract/0./,old/0/,dsrnge/0/

      DATA WHITE /-1/     ! Hex FF
      DATA BLACK /0/
      DATA GREY /-128/    ! Hex 80
             ! W IS HALF-BYTE READ,WRITE SWITCH
C===================================================================
      FARE='FARE'
      PLANET=' '
      PROJ_TYPE= ' '
      project='NOPRO'
      IOFF=0

      CALL IFMESSAGE('FARENC version 26-Dec-2001')
      CALL INIT_SPICE
      CALL XVPARM('INP',TMPBUF,ICNT,IDEF,2)
      CALL XVUNIT(INP(1),'INP',1,ISTAT,' ')
      CALL CHKSTAT(ISTAT,' ERROR IN XVUNIT,STAT=',1,ISTAT,1)
      CALL XVOPEN(INP(1),ISTAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
      INP(2) = -1
      IF(ICNT.EQ.2)THEN
         CALL XVUNIT(INP(2),'INP',2,ISTAT,' ')
         CALL CHKSTAT(ISTAT,' ERROR IN XVUNIT,STAT=',1,ISTAT,1)
      ENDIF
      NDSIN=ICNT

c     get image mode (image space or object space)

      call mp_init( mp,istat)
      if(istat.ne.mp_success) call mabend('error in mp_init')
      call mp_label_read( mp, inp(1), istat)
      if(istat.eq.mp_success) then
         CALL MP_GET_VALUE_STR(MP,'MAP_PROJECTION_TYPE',PROJ_TYPE,
     &    STATUS)
         IF(STATUS.EQ. mp_success) THEN
          if(proj_type(1:5).eq.'POINT')then
            call xvmessage('input image has perspective map label',' ')
          else
            call xvmessage('input image is map projection type.',' ')
            call mabend(proj_type)
          endif
         ELSE
            call mabend(' mp_get_value_str error')
         ENDIF
      elseif(istat.eq.MP_NO_MAP_LABELS) then !old-style VICAR labels


C  Check if image has been geometrically corrected ...
c  search thru all tasks present:
      ntasks = maxtasks		! on input, set to max. value
      call xlhinfo( inp(1), tasks, inst, ntasks, istat, ' ')
      if ( istat .ne. 1)   call mabend(' ** too many tasks ***')
      idata(39) = 7
      do i=1,ntasks
	if (index(tasks(i),'GEOM').gt.0 .or. tasks(i).eq.'FARENC') then
	  idata(39) = 8
	  go to 5
	endif
      enddo

c  check Vicar1 labels too ...
      cbuf = ' '
      call vic1lab( inp(1), istat, nlabs, cbuf, 20)
      if (index(cbuf,'GEOM').gt.0 .or. index(cbuf,'FARENC').gt.0) then
	idata(39) = 8
	go to 5
      endif
5     continue
      
        if (idata(39) .eq. 7) then
         call xvmessage('input image is image space',' ')
        else
         call xvmessage('input image is object space',' ')
        endif
      else
         CALL MABEND('mp_label_read error')
      endif

c handle map label perspective case (may be no project )
      if(proj_type(1:5).eq.'POINT')then
c      if(idata(39).eq.16)then
c        perspective input case:
        call mp_get_value(mp,'SPACECRAFT_DISTANCE',r8,status)
	far = r8  !far=rdata(38)
c           rlati=rdata(31)
        call mp_get_value(mp,'SUB_SPACECRAFT_LATITUDE',r8,status)
	rlati = r8
c           rlongi=rdata(32)
        call mp_get_value(mp,'SUB_SPACECRAFT_LONGITUDE',r8,status)
 	rlongi = r8
        call mp_get_value(mp,'NORTH_ANGLE',r8,status)
	angl = r8						!angl=rdata(35)
        if(angl.gt.360.) angl=angl-360.
        call mp_get_value(mp,'FOCAL_PLANE_SCALE',r8,status)
	scal = r8						!scal=rdata(30)
        call mp_get_value(mp,'FOCAL_LENGTH',r8,status)
	foca = r8						!foca=rdata(27)
        call mp_get_value(mp,'A_AXIS_RADIUS',r8,status)
	requ = r8						!requ=rdata(26)
        if (status.ne.0) THEN
           call mp_get_value(mp,'B_AXIS_RADIUS',r8,status)
	   requ = r8
        else
           call mp_get_value(mp,'B_AXIS_RADIUS',r8,status)
           if (status .eq. 0)  requ = .5*(requ + r8)
        end if
       
        call mp_get_value(mp,'C_AXIS_RADIUS',r8,status)
	rpol = r8					!rpol=rdata(25)
        call mp_get_value(mp,'PLANET_CENTER_LINE',r8,status)
	pc_line = r8  
        call mp_get_value(mp,'PLANET_CENTER_SAMPLE',r8,status)
	pc_samp = r8  
        pc_error=200.
        call xlget(inp(1),'HISTORY','SUB_SOLAR_LATITUDE',sunlat,
     +              istatus,'FORMAT','REAL','HIST','PERSLAB',' ')
        if(istatus.ne.1) then	! check for NIMS label
           call xlget(inp(1),'HISTORY','B_SSLLAT',sunlat1,
     +              istatus,'FORMAT','REAL','HIST','NIMSCMM2',' ')
           call xlget(inp(1),'HISTORY','E_SSLLAT',sunlat2,
     +              istatus,'FORMAT','REAL','HIST','NIMSCMM2',' ')
	   sunlat = 0.5*(sunlat1+sunlat2)
           if(istatus.ne.1) then
             call prnt(4,1,istatus,'xlget: status= .')
             call xvmessage('Failure to get solar latitude',' ')
           endif
        endif
        call xlget(inp(1),'HISTORY','SUB_SOLAR_LONGITUDE',sunlon,
     +              istatus,'FORMAT','REAL','HIST','PERSLAB',' ')
        if(istatus.ne.1) then	! check for NIMS label
           call xlget(inp(1),'HISTORY','B_SSLLON',sunlon1,
     +              istatus,'FORMAT','REAL','HIST','NIMSCMM2',' ')
           call xlget(inp(1),'HISTORY','E_SSLLON',sunlon2,
     +              istatus,'FORMAT','REAL','HIST','NIMSCMM2',' ')
	   sunlon = 0.5*(sunlon1+sunlon2)
           if(istatus.ne.1) then
             call prnt(4,1,istatus,'xlget: status= .')
             call xvmessage('Failure to get solar longitude',' ')
           endif
        endif
        project='NOPRO'
        idata(39)=8
        update=0
      else
C        determine project
         call getproj(inp(1),project,serno,fds,ind)
         if(ind.ne.0) then
           call prnt(4,1,ind,'GETPROJ: unrecognized project.')
           project='NOPRO'
           idata(39)=8
           update=0        
         endif
         call xvmessage('project='//project,' ')
      endif

      CALL XVPARM('OUT',TMPBUF,ICNT,IDEF,3)
      NDSOUT=ICNT
      CALL XVSIZE(JSL,JSS,JNLO,JNSO,JNLI,JNSI)
      DO I=1,ICNT
	CALL XVUNIT(OUT(I),'OUT',I,ISTAT,' ')
	CALL CHKSTAT(ISTAT,' ERROR IN XVUNIT,STAT=',1,ISTAT,1)
        IF (I.NE.ICNT)
     *      CALL XVOPEN(OUT(I),ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     *      'OP','WRITE','U_NL',JNLI,'U_NS',JNSI,' ')
      ENDDO

C     FIND FORMAT OF PICTURE
      CALL XVGET(INP(1),ISTAT,'FORMAT',FORMAT,'PIX_SIZE',IPIX,' ')
      IF(IPIX.EQ.2)THEN
	W=0
	CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS HALFWORD',' ')
      ELSE IF(IPIX.EQ.1)THEN
        W=1
        CALL XVCLOSE(INP(1),ISTAT,' ')
        CALL XVOPEN( INP(1),ISTAT,'IO_ACT','SA','OPEN_ACT',
     &	             'SA','U_FORMAT','HALF',' ')
        CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS BYTE',' ')
      ENDIF

      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      NL=NLI
      NS=NSI          ! ns, nsi, npix, ins are # samples
      NLX=NL
      NSX=NS*(2-W)    ! NSx IS NUMBER OF BYTES PER RECORD
      NPIX=NSI
      IF(NL.GT.1400)NL=1400
      INL=NL-4
      IF(NS.GT.1400)NS=1400
      INS=NS
      nl_anneal=nl
      ns_anneal=ns
      if (proj_type(1:5) .ne. 'POINT')  THEN
          pc_error=max(nl,ns)/2.0  ! planet center range for unknown project
          pc_line=nl/2.0
          pc_samp=ns/2.0
      end if

      if(project.ne.'NOPRO')then     !!!!!!!! START OF BIG IF block.

c establish initial planet center range by project.
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2')) pc_error=200.
      if(project.eq.'GLL  ') pc_error=125.  ! these are divided by 2
      if(project.eq.'CASSI') pc_error=125.

c Get label constants.
      call getlabcon(inp(1),project,ilab,ind)
      if(ind.eq.1) call prnt(4,1,ind,'GETLABCON: warning ind=.')
      if(ind.gt.1)then
        call prnt(4,1,ind,'GETLABCON: fatal ind=.')
        call abend
      endif
      serno=ilab(6)
      fds=ilab(2)
      sr=ilab(5)
      cam=ilab(72)
      scid=ilab(72)

c get camera constants
      call getcamcon(project,serno,foca,rdata(28),
     +                idata(29),scal,ind)
      if(ind.ne.0) call prnt(4,1,ind,'GETCAMCON: error ind=.')
c      foca=float(idata(27))
c      scal=float(idata(30))
      
      IF (NDSIN .GT. 1)   DSRNGE=1  !IF SECOND INPUT FILE IS GIVEN, USE IT
                                    !FOR GEOMA PARAMETERS.

C  PARAMETER PROCESSOR

      CALL PARPRO(PARMS,RPARM,DNTH,ACTTH,DIST,PERC,SIGM,TOL,IPRINT
     *,SIGA,ISL,Z,ISS,INL,INS,NL,NS,MEDI,NSWH,IBELOW,ANGL,SMAA,SMIA,
     *FOCA,SCAL,REQU,RPOL,FAR ,RLATI,RLONGI,IGEOM,W,SIZE,SUNA,PUTNOR,
     *ASTRE,LINC,IAUTO,ICLUW,ICLUN,IOFF,MAXNUM,LCIRCL,RADRNG,FRACT,
     *FDS,SERNO,CENL,CENS,UPDATE,SUNLAT,SUNLON,PLANET,old,
     *pc_error,pc_line,pc_samp)

C  OBTAIN DATA FROM SEDR
      if(.not.xvptst('NOSEDR')) then
        if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))
     +     planet='            '

        provenance = .false.    !farenc can be flexible on initial navigation.
        call getspice2(INP(1),provenance,lmbpt8,ind)
        if (ind .ne. 1) then
            call xvmessage('***Err reading SPICE data',' ')
            call abend
        endif

        call mve(8,100,lmbpt8,bufse,1,1)
        far=sngl(lmbpt8(27,1))
        rlati=sngl(lmbpt8(30,1))
        rlongi=sngl(lmbpt8(31,1))
        sunlat=sngl(lmbpt8(28,1))
        sunlon=sngl(lmbpt8(29,1))
        angl=sngl(lmbpt8(68,1))+90.
        if(angl.gt.360.) angl=angl-360.
        requ=(sngl(lmbpt8(13,1))+sngl(lmbpt8(14,1)))/2.0
        rpol=sngl(lmbpt8(15,1))
        pc_line=sngl(lmbpt8(69,1))  ! predicted plant center
        pc_samp=sngl(lmbpt8(70,1))
      endif

      endif     !!!!!!!! END OF BIG IF block.

C  PARAMETER PROCESSOR
      CALL PARPRO(PARMS,RPARM,DNTH,ACTTH,DIST,PERC,SIGM,TOL,IPRINT
     *,SIGA,ISL,Z,ISS,INL,INS,NL,NS,MEDI,NSWH,IBELOW,ANGL,SMAA,SMIA,
     *FOCA,SCAL,REQU,RPOL,FAR ,RLATI,RLONGI,IGEOM,W,SIZE,SUNA,PUTNOR,
     *ASTRE,LINC,IAUTO,ICLUW,ICLUN,IOFF,MAXNUM,LCIRCL,RADRNG,FRACT,
     *FDS,SERNO,CENL,CENS,UPDATE,SUNLAT,SUNLON,PLANET,old,
     *pc_error,pc_line,pc_samp)

      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE('FINAL DATA FROM SEDR AND PARAMETERS ARE:',' ')
      CALL PRNT(7,1,FAR,'s/c range (km) =   .')
      CALL PRNT(7,1,RLATI,'sub s/c latitude =  .')
      CALL PRNT(7,1,RLONGI,'sub s/c longitude =  .')
      CALL PRNT(7,1,ANGL,'north angle = .')
      CALL PRNT(7,1,SCAL,'scale (km/pxl) = .')
      CALL PRNT(7,1,FOCA,'focal length (mm) = .')
      CALL PRNT(7,1,REQU,'equatorial radius (km) =  .')
      CALL PRNT(7,1,RPOL,'polar radius (km) =  .')
      CALL PRNT(7,1,SUNLAT,'sub solar latitude =.')
      CALL PRNT(7,1,SUNLON,'sub solar longitude =.')
      CALL PRNT(7,1,pc_line,'planet center line=.')
      CALL PRNT(7,1,pc_samp,'planet center sample=.')

c set thre and ibelow defaults
       if(w.eq.1)then   ! byte data
         if(ibelow.eq.-32760) ibelow=30
         if(dnth.eq.-32760) dnth=30
       else             ! all other cases
         if(ibelow.eq.-32760) ibelow=200
         if(dnth.eq.-32760) dnth=200
       endif

C  DETERMINE NATURE OF OUTPUT DATA SETS
       IF(NDSOUT.EQ.3)THEN
           IGEOMG=OUT(3)
           IDSRNP=OUT(2)
           IDSRNL=OUT(1)
       ELSE IF(NDSOUT.EQ.2)THEN
           IGEOMG=OUT(2)
           IDSRNL=OUT(1)
           IDSRNP=0
       ELSE IF(NDSOUT.EQ.1)THEN
           IDSRNL=0
           IDSRNP=0
           IGEOMG=OUT(1)
       ELSE 
           IDSRNL=0
           IDSRNP=0
           IGEOMG=0
       ENDIF

      IF(RADRNG.LE.0.0)THEN
        !COMPUTE PROJECTION OF OBLATE SPHEROID ON SKY AND ADDITIVE CORRECTION
        !TO GET PLANET CENTER FROM ELLIPSE CENTER
	CALL PROJEL(REQU,RPOL,FAR,RLATI,IND,FOCA,SCAL,SMAA,SMIA,
     *              ANGL,DELL,DELS)
      ELSE
        !COMPUTE PROJECTION OF RINGS ON SKY AND CORRECTION FROM RING
        !CENTER TO PLANET CENTER
	CALL RING(RADRNG,FAR,RLATI,FOCA,SCAL,SMAA,SMIA,ANGL,DELL,DELS)
      ENDIF

      !COMPUTE SUN ANGLE CLOCKWISE FROM UP
      CALL SUNPOS(SUNA,ANGL,RLATI,RLONGI,SUNLAT,SUNLON)

      !RESET ACTIVITY AND BELOW KEYWORDS IF AUTO SPECIFIED
      IF(IAUTO.EQ.1)CALL AUTO(W,CL,C,NLR2,ISL,ISS,INL,INS,FRACT,
     *          ASTRE,LINC,ACTTH,IBELOW,SMAA,SMIA,TOL,DNTH,IOFF,DSRNGE)

      !LOCATE LIMB POINTS ON THE PLANET
      CALL GETPTS(MAV,MIV,SMAV,SMIV,MAH,MIH,LMAH,LMIH,MEDI,NLR2,W,NS,
     *  SAMP,NSWH,IGEOM,NPIX,ACT,BUF,ISL,INL,ACTTH,DNTH,DIST,ISS,INS,
     *  NSOUT,IBELOW,PERC,NL,IOFF,LCIRCL,suna)

      !EDIT LIMB POINTS BEFORE THE LIMB FIT
      CALL FIXPTS(MAV,MIV,MAH,MIH,PERC,NPIX,NL,NE,K,SIGA,C,CL,SMAV,SMIV,
     *   LMAH,LMIH,NU,LIMIT,MAXNUM,LCIRCL,iclun)

      !REJECT POINTS WITH LESS THAN 'NUMBER' NEIGHBORS WITHIN 'ISIZE' RADIUS
      CALL CLUSTR(CL,C(1,1),C(1,2),C(1,3),C(1,4),C(1,5),NE,ICLUW,ICLUN)

      ! sort the points by line
c      call r4sort(c(1,1),c(1,2),ne)
      call mve(7,NE,C(1,1),C2(1),1,1)
      call mve(7,NE,C(1,2),CPTR(1),1,1)
      call ssortp(c(1,1),1,ne,ip)
      do ii = 1, ne
         c(ii,1) = c2(ip(ii))
         c(ii,2) = cptr(ip(ii))
      end do

      !SAVE RAW LIMB POINTS FOR LATER USE IN DISPLAYS
      DO IX=1,ne
          DO IY=1,2
             LMBPTS(IX,IY)=C(IX,IY)
             os_pts(ix,iy)=c(ix,iy)
         ENDDO
      ENDDO

C   GET AND SAVE GEOMA PARAMETERS IN BUFFER IG.
C   INP(2) = -1 & dsrnge=0 IF ONLY 1 INPUT; IN THIS CASE NOMINALS ARE USED.

      if(idata(39).eq.7)then
         call getgeom(INP(2),project,serno,dsrnge,ig,ig,nah,nav,ind)
         if(ind.ne.0) then
            call prnt(4,1,ind,'GETGEOM: bad indicator.')
            call abend
         endif
         nph=nah+1
         npv=nav+1

	 DO L=1,NE  ! CONVERT POINTS TO OBJECT SPACE
            is_line=c(L,1)
            is_samp=c(L,2)
            call convisos(project,serno,is_line,is_samp,os_line,os_samp,
     +                    1,ig(9),nph,npv,ind)
            if(ind.ne.0)call XVMESSAGE('CONVISOS: conversion error',' ')
            c(L,1)=os_line
            c(L,2)=os_samp
            os_pts(L,1)=os_line
            os_pts(L,2)=os_samp
	 ENDDO
         nl_anneal=nl*1.25
         ns_anneal=ns*1.25
      ENDIF

      !CREATE DISPLAY OF ALL CANDIDATE POINTS ON LIMB
      IF(IDSRNP.NE.0)THEN
	IF(IPRINT.NE.0)THEN 		! PRINT OUT RAW INPUT POINTS
	  CALL PRNT(7,NE,lmbpts(1,1),'LINE OF POINTS  .')
	  CALL PRNT(7,NE,lmbpts(1,2),'SAMPLE OF POINTS.')
	ENDIF
	CALL POINTD(IDSRNP,LMBPTS,NLX,GREY,LOG,NE,WHITE,NPIX,NLRD)
      ENDIF


c ************************ fitting logic ****************************

c Uses simulated annealing to solve the fitting problem with subroutine
c Metropolis and a cost function Cost_XXXX.
c Then do a conventional least squares.
C FOR CONSTRAINED ELLIPSE - SMIA,SMAA,ANGL KNOWN

        if(smaa.ne.-1..or.smia.ne.-1.)then
          if(smaa.eq.-1.)smaa=smia
          if(smia.eq.-1.)smia=smaa
          if(smaa.eq.smia)then

c Solve for constrained circle. cl(5)=line cl(6)=sample returned
            npts=40
            call select_points(c,ne,npts)
            call anneal_ccircle(npts,c,smaa,nl_anneal,ns_anneal,
     +        old,cl,pc_line,pc_samp,pc_error)
            planet_center_line=cl(5)
            planet_center_sample=cl(6)
c EVALUATE CONSTRAINED LEAST SQUARES CIRCLE
            SMAAA=SMAA+.01 ! cluge to fit circle
            coef(1)=planet_center_line
            coef(2)=planet_center_sample
            CALL CONEL(NE,NU,C,CL,COEF,C,TOL,SIGM,NDSOUT,IPRINT,SUNA,
     *	    SERNO,FAR,FOCA,SCAL,C8,C8L,ANGL,SMAAA,SMIA,RADRNG,project,
     *      lmbpts,os_pts,dell,dels)
            planet_center_line=coef(1)
            planet_center_sample=coef(2)

          else if(angl.ne.400.)then
c Solve for constrained ellipse. cl(5)=line cl(6)=sample returned
            npts=30
            call select_points(c,ne,npts)
            call anneal_cellipse(npts,c,angl,smaa,smia,nl_anneal,
     +         ns_anneal,old,cl,dell,dels,pc_line,pc_samp,pc_error)
            planet_center_line=cl(5)
            planet_center_sample=cl(6)
c EVALUATE CONSTRAINED LEAST SQUARES ELLIPSE
            SMAAA=SMAA
            coef(1)=planet_center_line
            coef(2)=planet_center_sample
            CALL CONEL(NE,NU,C,CL,COEF,C,TOL,SIGM,NDSOUT,IPRINT,SUNA,
     *	    SERNO,FAR,FOCA,SCAL,C8,C8L,ANGL,SMAAA,SMIA,RADRNG,project,
     *      lmbpts,os_pts,dell,dels)
            planet_center_line=coef(1)
            planet_center_sample=coef(2)

          else
            call MABEND('No ellipse solution if angle unknown.')
          endif
        else

c Solve for general circle. cl(1)=line cl(2)=sample returned
            npts=30
            call select_points(c,ne,npts)
            call anneal_circle(npts,c,smaa,nl_anneal,ns_anneal,old,cl)
            smia=smaa
            planet_center_line=cl(1)
            planet_center_sample=cl(2)
c EVALUATE UNCONSTRAINED LEAST SQUARES CIRCLE
            NU=3
            cx=planet_center_sample
            cy=planet_center_line
            r=smaa
            CALL CIRCLE(NE,NU,C,CL,COEF,CX,CY,R,ER,TOL,SIGM,NDSOUT,
     *      IPRINT,SAMP,SUNA,SERNO,FAR,FOCA,SCAL,C8,C8L,project,lmbpts,
     *      os_pts)
            planet_center_line=cy
            planet_center_sample=cx
            smaa=r
            smia=r
        endif       

      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         IF(SERNO.eq.4.or.serno.eq.6)THEN
            cl(1)=planet_center_line
            cl(2)=planet_center_sample
            CALL MWATNA(SERNO,cl(1),cl(2),cl(30),cl(31),*998)
            CALL PRNT(7,2,cl(30),
     *           'SIMULTANEOUS EXPOSURE NA LINE&SAMPLE =.')
         ENDIF
      endif
998   continue

      IF(IDSRNL.NE.0)THEN		! DRAW PLANET LIMB
        imgtype=idata(39)
        if(dsrnge.gt.0)imgtype=7
           cl(1)=planet_center_line
           cl(2)=planet_center_sample
           CALL DRAWLIMB(IDSRNL,NLX,NSX,NLR2,LOG,CY,R,CX,W,COEF,ELICOE,
     *	    CONCL,CONCS,SMAA,imgtype,nph,npv,ig(9),cl(1),cl(2),smia,
     *      angl,project,serno)
      ENDIF

      IF(IDSRNP.ne.0)then
         !SET TO ZERO ALL FINAL POINTS ON LIMB
         CALL POINTU(IDSRNP,LMBPTS,NPIX,LOG,NE,BLACK,NLRD,NLX)
      endif


c ************************ end of fitting *****************************

      IF(UPDATE.NE.1.or.project.eq.'NOPRO')GO TO 295

      !CALCULATE OMMATRIX AND RS VECTOR FOR UPDATING SEDR
      LSSP=planet_center_line
      SSSP=planet_center_sample
      THT=ANGL
      BL=RLONGI
      PHI=RLATI
      call getcamcon(project,serno,rdata(1),rdata(2),rdata(3),
     +               rdata(4),ind)
      if(ind.ne.0)call prnt(4,1,ind,'GETCAMCON: bad indic.')
      LO=rdata(2)
      SO=rdata(3)
      PIXPMM=SCAL
      VABS=DBLE(FAR)
      FIC=FOCA
      write (msg,'(a,9f12.6)') 'INPUT TO MOMATV=  ',LO,SO,LSSP,SSSP,
     +                         PIXPMM,FIC,BL,PHI,THT
      call xvmessage(msg,' ')
c      CALL PRNT(8,9,LO,'INPUT TO MOMATV=.')
      CALL MOMATV(LO,SO,LSSP,SSSP,PIXPMM,FIC,BL,PHI,THT,VABS,OM,RS)
      CALL PRNT(8,9,OM,'OM=.')
      CALL PRNT(8,3,RS,'RS=.')

      CALL MVCL(FARE,IBUFSE(11),4)
c update om matrix
      DO I=1,9
	BUFSE(58+I)=OM(I)
      ENDDO
c update rs vector
      BUFSE(22)=RS(1)
      BUFSE(23)=RS(2)
      BUFSE(24)=RS(3)
c compute c matrix
      call omtoc(bufse(59),bufse(50),bufse(41)) ! c matrix
c store spice/sedr

      call putspice2('FARE','FARENC',BUFSE, IND)

      IF(IND.EQ.1)THEN
	CALL XVMESSAGE('SPICE data HAS BEEN UPDATED',' ')
      ELSE
	CALL PRNT(4,1,IND,'PUTSPICE ERROR, IND=.')
	CALL ABEND
      ENDIF

295   CONTINUE

c  write out planet center into a file if input is perspective projection
      if(proj_type(1:5).eq.'POINT')then
         call xvunit(farunit,'NEWFILE',1,status,
     +               'U_NAME','FARENC.POS',' ')
         call xvopen(farunit,status,'U_NL',1,'U_NS',2,'OP','WRITE',
     +               'OPEN_ACT','AS','IO_ACT','AS','O_FORMAT','REAL',
     +               'U_FORMAT','REAL',' ')
         cl(1)=planet_center_line
         cl(2)=planet_center_sample
         call xvwrit(farunit,cl,status,' ')
         call xvclose(farunit,status,' ')
      endif

      if(igeomg.ne.0)then
C  OUTPUT GEOMA PARAMETERS TO TRANSLATE PLANET TO CENTER OF OUTPUT PICTURE
        cl(1)=planet_center_line
        cl(2)=planet_center_sample
        CALL GEOMIT(CL,CL,igeom,IGEOMG,Idata(39),DELL,DELS,IG(1),W,
     *  IPRINT,R,SMAA,RGEMA,SIZE,PUTNOR,ANGL,CENL,CENS,project,serno)
        call XVMESSAGE(' ',' ')
        CALL XVMESSAGE('GEOMA RECENTERING PARAMETERS WRITTEN',' ')
      endif
c
C  WRITE OUT LINE,SAMP AS TCL VARIABLES
	call xqini(IG,500,xabort)
	call xqreal(IG,'PCL',1,planet_center_line,xadd,ist)
	call xqreal(IG,'PCS',1,planet_center_sample,xadd,ist)
	call xvqout(IG,ist)
      RETURN

      END

c***********************************************************************
      subroutine omtoc(om,me,c)
c computes c matrix from om & me matrix
      real*8 om(9),me(9),c(9),omt(9)
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


c***********************************************************************

       subroutine cost_ccircle(x,array,array2,n,error,ind)

c Returns the cost function resulting from guessing x (answer)
c For the constrained circle..

c X     is the solution vector.
c ARRAY is an array of N data points if needed.
c ARRAY2 is another array if needed.
c N      is the number of elements/data points in ARRAY.
c ERROR is the returned cost.
c IND   is 0 for normal return, 1 for abnormal return.
c        If METROPOLIS senses an indicator of 1 it will generate another
c        guess somewhere else & try again.
c 
c Array2 is ordered as:
c array2(1)=nl array2(2)=ns
c array2(3)=% of points to accept within 3 pixels.
c array2(4)=circle radius
c array2(5)=initial line guess of planet center
c array2(6)=initial sample guess of planet center
c array2(7)=permitted range of motion of planet center
c
c x(1)=line center x(2)=sample center returned.

      real*4 array(2800,5),array2(10),x(4),dr

      ind=1
      rad=array2(4)
      if(x(1).gt.array2(5)+array2(7))return
      if(x(2).gt.array2(6)+array2(7))return
      if(x(1).lt.array2(5)-array2(7))return
      if(x(2).lt.array2(6)-array2(7))return
      sumdr=0.
      sumdr2=0.
      sum=0.
      range=min(rad/5.0,50.)
      range2=3.
      least=n*array2(3)/100.
      m=0
      mm=0

      do j=1,n
        dr=abs(rad-sqrt((array(j,3)-x(1))**2+(array(j,4)-x(2))**2))
        sum=sum+dr
        if(dr.lt.range)then
           m=m+1
           sumdr=sumdr+dr
        endif
        if(dr.lt.range2)then
           mm=mm+1
           sumdr2=sumdr2+dr
        endif
      enddo

      if(mm.ge.least)then
         error=sumdr2/mm
      else if(m.eq.0)then
         error=sum/n+1.0
      else
         error=sumdr/m + range/m
      endif
  
      ind=0
      return
      end

      
c ****************************************************************
       subroutine cost_cellipse(x,array,array2,n,error,ind)

c Returns the cost function resulting from guessing x (answer)
c For the constrained ellipse..

c Array2 is ordered as:
c array2(1)=maximum radius about center.
c array2(3)=% of points to accept within 3 pixels.
c array2(4)=rotation angle of ellipse.
c array2(5)=semimajor axis  array2(6)=semiminor axis
c
c x(1)=line center x(2)=sample center returned.

      real*4 array(2800,5),array2(10),x(4),dr

      ind=1
c rad=the greatest range from the picture center possible.
      rad=array2(1)
      if(x(1).gt.rad)return
      if(x(2).gt.rad)return
      if(x(1).lt.-rad)return
      if(x(2).lt.-rad)return
      sumdr=0.
      sumdr2=0.
      sum=0.
      range=min(array2(5)/5.0,50.)
      range2=3.
      least=n*array2(3)/100.
      m=0
      mm=0
      smaa2=array2(5)*array2(5)
      smia2=array2(6)*array2(6)
      sm2=smaa2*smia2

      do j=1,n
        yy=array(j,3)-x(1)    ! move coords to ellipse center
        xx=array(j,4)-x(2)
        if(xx.eq.0.0)xx=.01
        theta=atan2(yy,xx)     ! angl of point about ellipse
        sinth=sin(theta)
        costh=cos(theta)
        r=sqrt(sm2/(smaa2*sinth*sinth+smia2*costh*costh)) ! elips rad
        dr=abs(r-sqrt(yy*yy+xx*xx))  ! radial residual
        sum=sum+dr
        if(dr.lt.range)then
           m=m+1
           sumdr=sumdr+dr
        endif
        if(dr.lt.range2)then
           mm=mm+1
           sumdr2=sumdr2+dr
        endif
      enddo

      if(mm.ge.least)then
         error=sumdr2/mm
      else if(m.eq.0)then
         error=sum/n+1.0
      else
         error=sumdr/m + range/m
      endif
  
      ind=0
      return
      end

      
c ****************************************************************
       subroutine cost_circle(x,array,array2,n,error,ind)

c Returns the cost function resulting from guessing x (answer)
c For the general circle..
c 
c Array2 is ordered as:
c array2(1)=nl array2(2)=ns
c array2(3)=% of points to accept within 3 pixels.
c
c x(1)=line center x(2)=sample center x(3)=radius ,returned.

      real*4 array(2800,5),array2(10),x(4),dr

      ind=1
      if(x(1).gt.array2(1))return
      if(x(2).gt.array2(2))return
      if(x(1).lt.1.)return
      if(x(2).lt.1)return
      if(x(3).lt.10.)return
      if(x(3).gt.array2(2)/2.)return
      sumdr=0.
      sumdr2=0.
      sum=0.
      range=50.
      range2=3.
      least=n*array2(3)/100.
      m=0
      mm=0

      do j=1,n
        dr=abs(x(3)-sqrt((array(j,3)-x(1))**2+(array(j,4)-x(2))**2))
        sum=sum+dr
        if(dr.lt.range)then
           m=m+1
           sumdr=sumdr+dr
        endif
        if(dr.lt.range2)then
           mm=mm+1
           sumdr2=sumdr2+dr
        endif
      enddo

      if(mm.ge.least)then
         error=sumdr2/mm
      else if(m.eq.0)then
         error=sum/n + 1.0
      else
         error=sumdr/m + range/m
      endif
  
      ind=0
      return
      end

      
c ****************************************************************
      subroutine select_points(c,nptsin,nptsout)
c selects about nptsout points from the set of points present in:
c  c(nn,1)=line in and c(nn,2)=sample in
c and places them into c(n,3) and c(n,4) for output.
      real*4 c(2800,5)
      if(nptsin.le.nptsout)then
        do j=1,nptsin
          c(j,3)=c(j,1)
          c(j,4)=c(j,2)
        enddo
      else
        x=real(nptsin)/real(nptsout)
        y=0.0
        do j=1,nptsout
          y=y+x
          m=nint(y)
          c(j,3)=c(m,1)
          c(j,4)=c(m,2)
        enddo
      endif
      return
      end

      
c ****************************************************************
      subroutine anneal_ccircle(npts,c,rad,nl,ns,old,cl,
     +  pc_line,pc_samp,pc_error)
c To solve for the center of a constrained circle (radius known).
c npts=# input points on circle.
c c=points array
c rad=known radius
c nl=#lines ns=#samples input picture.
c old=1 means old FARENC least squares mode. Is =0 here.
c cl=returned center. cl(5)=line cl(6)=sample.
      external cost_ccircle       ! constrained circle cost function
      real*4 c(2800,5),range(2),answer(2),solution(3,10),cl(20)
      integer*4 old
      numten=1200
      limit=5300
      iprint=0
      narg=2
      norm=315
      c(1,5)=nl
      c(2,5)=ns
      call xvparm('BEGIN',c(3,5),icnt,idef,1)
      iter=nint((c(3,5)-10.)/10.)
      c(4,5)=rad
      c(5,5)=pc_line
      c(6,5)=pc_samp
      c(7,5)=pc_error
      n=0
      c(3,5)=c(3,5)+10.
200   range(1)=pc_error/2.0
      range(2)=pc_error/2.0
      answer(1)=pc_line
      answer(2)=pc_samp
      c(3,5)=c(3,5)-10.
      call metropolis(cost_ccircle,narg,c,c(1,5),range,numten,answer,
     +                limit,norm,npts,iprint,ind)
      if(ind.ne.0)then
        call XVMESSAGE('Metropolis: bad ind',' ')
        old=1
        return
      endif
      error=range(1)
      if(error.lt.1.0)then
         call XVMESSAGE(' ',' ')
         call XVMESSAGE
     +        ('Constrained annealing circle fit object space:',' ')
         call prnt(7,1,answer(1),'Line of center=.')
         call prnt(7,1,answer(2),'Samp of center=.')
         call prnt(7,1,error,'Radial fit mean residual=.')
         cl(5)=answer(1)
         cl(6)=answer(2)
         return
      else
         n=n+1
         solution(1,n)=answer(1)
         solution(2,n)=answer(2)
         solution(3,n)=error
         if(n.gt.iter)then
            error=solution(3,1)
            j=1
            do n=2,iter+1
               if(solution(3,n).lt.error)then
                  error=solution(3,n)
                  j=n
               endif
            enddo
            answer(1)=solution(1,j)
            answer(2)=solution(2,j)
            call XVMESSAGE(' ',' ')
            call XVMESSAGE
     +        ('Constrained annealing circle fit object space:',' ')
            call prnt(7,1,answer(1),'Line of center=.')
            call prnt(7,1,answer(2),'Samp of center=.')
            call prnt(7,1,error,'Radial fit mean residual=.')
            cl(5)=answer(1)
            cl(6)=answer(2)
            return
         else
            call prnt(7,1,error,'No  convergence,residual=.')
            goto 200
         endif
      endif
      end

      
c ****************************************************************
      subroutine anneal_cellipse(npts,c,angl,smaa,smia,nl,ns,old,cl,
     *  dell,dels,pc_line,pc_samp,pc_error)
c To solve for the center of a constrained ellipse (radii known).
c npts=# input points on circle.
c c=points array
c angl=rotation angle of minor axis clockwise frm up. Deg.
c smaa=semimajor axis
c smia=semiminor axis
c nl=#lines ns=#samples input picture.
c old=1 means old FARENC least squares mode. Is =0 here.
c cl=returned center. cl(5)=line cl(6)=sample.
      external cost_cellipse       ! constrained ellipse cost function
      real*4 c(2800,5),range(2),answer(2),solution(3,10),cl(20),d2r
      integer*4 old

c First rotate points about picture center to correct for
c ellipse rotation.
      d2r=acos(-1.0)/180.0
      cosangl=cos(angl*d2r)
      sinangl=sin(angl*d2r)
      do j=1,npts
         yy=pc_line-c(j,3)
         xx=c(j,4)-pc_samp
         x=xx*cosangl-yy*sinangl
         y=yy*cosangl+xx*sinangl
         c(j,3)=y
         c(j,4)=x
      enddo

      numten=1200
      limit=5300
      iprint=0
      narg=2
      norm=315
      c(1,5)=pc_error
      c(2,5)=ns    ! not used
      call xvparm('BEGIN',c(3,5),icnt,idef,1)
      iter=nint((c(3,5)-10.)/10.)
      c(4,5)=angl  ! not used
      c(5,5)=smaa
      c(6,5)=smia
      n=0
      c(3,5)=c(3,5)+10.
200   range(1)=pc_error/2.0
      range(2)=pc_error/2.0
      answer(1)=0.0
      answer(2)=0.0
      c(3,5)=c(3,5)-10.
      call metropolis(cost_cellipse,narg,c,c(1,5),range,numten,answer,
     +                limit,norm,npts,iprint,ind)
      if(ind.ne.0)then
        call XVMESSAGE('Metropolis: bad ind',' ')
        old=1
        return
      endif

c commented out residuals printing loop
c      smaa2=smaa*smaa
c      smia2=smia*smia
c      sm2=smaa2*smia2
c      write(*,*)answer(1),answer(2)
c      do j=1,npts
c        yy=c(j,3)-answer(1)    ! move coords to ellipse center
c        xx=c(j,4)-answer(2)
c        if(xx.eq.0.0)xx=.01
c        theta=atan2(yy,xx)     ! angl of point about ellipse
c        sinth=sin(theta)
c        costh=cos(theta)
c        r=sqrt(sm2/(smaa2*sinth*sinth+smia2*costh*costh)) ! elips rad
c        dr=(r-sqrt(yy*yy+xx*xx))  ! radial residual
c        write(*,*),j,c(j,3),c(j,4),dr
c      enddo

      error=range(1)
      if(error.lt.1.0)then
         call XVMESSAGE(' ',' ')
         call XVMESSAGE
     +        ('Constrained annealing ellipse fit object space:',' ')
c rotate center back to undo initial points rotation.
         xx=answer(2)*cosangl+answer(1)*sinangl
         yy=answer(1)*cosangl-answer(2)*sinangl
c re-introduce picture center offset.
         answer(1)=pc_line - yy
         answer(2)=xx + pc_samp         
         call prnt(7,1,answer(1)+dell,'Line of center=.')
         call prnt(7,1,answer(2)+dels,'Samp of center=.')
         call prnt(7,1,error,'Radial fit mean residual=.')
         cl(5)=answer(1)
         cl(6)=answer(2)
         return
      else
         n=n+1
         solution(1,n)=answer(1)
         solution(2,n)=answer(2)
         solution(3,n)=error
         if(n.gt.iter)then
            error=solution(3,1)
            j=1
            do n=2,iter+1
               if(solution(3,n).lt.error)then
                  error=solution(3,n)
                  j=n
               endif
            enddo
            answer(1)=solution(1,j)
            answer(2)=solution(2,j)
            call XVMESSAGE(' ',' ')
            call XVMESSAGE
     +        ('Constrained annealing ellipse fit object space:',' ')
c rotate center back to undo initial points rotation.
            xx=answer(2)*cosangl+answer(1)*sinangl
            yy=answer(1)*cosangl-answer(2)*sinangl
c re-introduce picture center offset.
            answer(1)=pc_line - yy
            answer(2)=xx + pc_samp         
            call prnt(7,1,answer(1)+dell,'Line of center=.')
            call prnt(7,1,answer(2)+dels,'Samp of center=.')
            call prnt(7,1,error,'Radial fit mean residual=.')
            cl(5)=answer(1)
            cl(6)=answer(2)
            return
         else
            call prnt(7,1,error,'No convergence,residual=.')
            goto 200
         endif
      endif
      end


      
c ****************************************************************
      subroutine anneal_circle(npts,c,smaa,nl,ns,old,cl)
c To solve for the center of general circle (radius unknown).
c npts=# input points on circle.
c c=points array
c nl=#lines ns=#samples input picture.
c old=1 means old FARENC least squares mode. Is =0 here.
c cl=returned center. cl(1)=line cl(2)=sample.
      external cost_circle       ! general circle cost function
      real*4 c(2800,5),range(3),answer(3),solution(4,10),cl(20)
      integer*4 old
      numten=6000
      limit=18000
      iprint=0
      narg=3
      norm=815
      c(1,5)=nl
      c(2,5)=ns
c      c(3,5)=90.
c      iter=6
      call xvparm('BEGIN',c(3,5),icnt,idef,1)
      iter=nint((c(3,5)-10.)/10.)
      n=0
      c(3,5)=c(3,5)+10.
200   range(1)=nl/6.
      range(2)=ns/6.
      range(3)=nl/4.
      answer(1)=nl/2.0
      answer(2)=ns/2.0
      answer(3)=nl/4.
      c(3,5)=c(3,5)-10.
      call metropolis(cost_circle,narg,c,c(1,5),range,numten,answer,
     +                limit,norm,npts,iprint,ind)
      if(ind.ne.0)then
        call XVMESSAGE('Metropolis: bad ind',' ')
        old=1
        return
      endif
      error=range(1)
      if(error.lt.1.0)then
         call XVMESSAGE(' ',' ')
         call XVMESSAGE
     +        ('General circle annealing fit object space:',' ')
         call prnt(7,1,answer(1),'Line of center=.')
         call prnt(7,1,answer(2),'Samp of center=.')
         call prnt(7,1,answer(3),'Radius=.')
         call prnt(7,1,error,'Radial fit mean residual=.')
         cl(1)=answer(1)
         cl(2)=answer(2)
         smaa=answer(3)
         return
      else
         n=n+1
         solution(1,n)=answer(1)
         solution(2,n)=answer(2)
         solution(3,n)=answer(3)
         solution(4,n)=error
         if(n.gt.iter)then
            error=solution(4,1)
            j=1
            do n=2,iter+1
               if(solution(4,n).lt.error)then
                  error=solution(4,n)
                  j=n
               endif
            enddo
            answer(1)=solution(1,j)
            answer(2)=solution(2,j)
            answer(3)=solution(3,j)
            call XVMESSAGE(' ',' ')
            call XVMESSAGE
     +        ('General circle annealing fit object space:',' ')
            call prnt(7,1,answer(1),'Line of center=.')
            call prnt(7,1,answer(2),'Samp of center=.')
            call prnt(7,1,answer(3),'Radius=.')
            call prnt(7,1,error,'Radial fit mean residual=.')
            cl(1)=answer(1)
            cl(2)=answer(2)
            smaa=answer(3)
            return
         else
            call prnt(7,1,error,'No convergence,residual=.')
            goto 200
         endif
      endif
      end

      
c ****************************************************************
      subroutine metropolis(cost,narg,array,array2,range,numten,answer,
     +                       limits,norm,npts,iprnt,ind)

c routine to minimize function presented by COST subroutine.
      integer*4 fail,iseed,iprnt
      real*4 range(narg),answer(narg),rannum
      real*4 array(1),array2(1)
      real*4 temp(10),x(10),minx(10),mincost
      character*132 msg
      logical jumpflag

      external cost

      jumpflag=.false.
      pi=acos(-1.0)
      pi2=pi/2.0
      limit=limits

c  Compute a random number seed based on the time of day
C      call time(buf)
C      if((buf(2)/2)*2.eq.buf(2)) buf(2)=buf(2)-1
C      iseed=buf(2)
      call get_seconds(iseed)

c  Compute the cost at position ANSWER and assign to variable C1.

      call cost(answer,array,array2,npts,c1,ind)
      if(ind.ne.0)then
         CALL XVMESSAGE
     &        ('Failure in COST function at initial guess',' ')
         return
      endif

c  Set initial temperatures to the range estimates.

      do j=1,narg
         temp(j)=range(j)
      enddo

      fail=0
      loop=1
      loop1=0
      loop2=0
      loop3=0
      mincost=1.0e+38
      numreset=numten/10
      scale=exp((log(0.1))/real(numten))

      if(iprnt.gt.0)then
         call PRNT(7,1,scale,'scale=')
         msg(1:50)='Solution  Temperature     Cost  #downhill #uphill '
         msg(51:72)='#rejected #outofbounds'
         call XVMESSAGE(msg,' ')
      endif

c   MAIN LOOP: loop on number of successful changes in solution space. 
      do while(loop.lt.limit)

c       Compute the delta_cost/temperature ratio for
c       normalization of probabilities.
c       Note that this is the Boltzman constant for this 'system'.

300     continue
        if(mod(loop,norm).eq.1 .or. jumpflag)then
           costsum=0.0
           k=0
           do j=1,narg
              x(j)=answer(j)
           enddo
           do j=1,narg
              x(j)=answer(j)-temp(j)
              call cost(x,array,array2,npts,c2,ind)
              if(ind.eq.0)then
                k=k+1
                costsum=costsum+abs(c1-c2)
              endif
              x(j)=answer(j)+temp(j)
              call cost(x,array,array2,npts,c2,ind)
              if(ind.eq.0)then
                k=k+1
                costsum=costsum+abs(c1-c2)
              endif
              x(j)=answer(j)
           enddo
           if(k.eq.0) then
              CALL XVMESSAGE('Failure in normalization procedure',' ')
              CALL XVMESSAGE(' solution + - range outofbounds',' ')
              return
           endif              

            if (temp(1) .EQ. 0.0)  call mabend(
     .     'ERROR in routine METROPOLIS: Please check input parameters')

           boltzman=5.0*(costsum/k)/temp(1)
           if(iprnt.gt.0) CALL PRNT(7,1,boltzman,'Boltzman = ')
        endif
                     
c       Decrement the temperature according to the multiplicative
c       cooling schedule.

        do j=1,narg
           temp(j)=temp(j)*scale
        enddo
        energy=boltzman*temp(1)

c       Compute a solution space guess using a Cauchy-Lorentzian
c       random probability distribution function.

91      do j=1,narg
           call rangen(iseed,rannum)
           x(j)=temp(j)*tan(pi*rannum+pi2)+answer(j)
        enddo
        call cost(x,array,array2,npts,c2,ind)
        if(ind.ne.0)then
           loop3=loop3+1
           goto 91
        endif

        if(c2.lt.c1)then

c           Accept lower cost position.
c           We always accept a downhill cost route if offered.

            c1=c2
            do j=1,narg
               answer(j)=x(j)
            enddo
            loop1=loop1+1
            monitor=0

        else
c           Compute probability of accepting higher cost position.
c           This comes from the Boltzman probability of our system 
c           transitioning from energy state c1 to energy state c2.

            if (energy .eq. 0.0)  call mabend(
     .     'ERROR in routine METROPOLIS: Please check input parameters')

            c3=(c2-c1)/energy
            if(c3.gt.50.)then
               fail=fail+1
               monitor=monitor+1
               if(monitor.gt.20)then
                  monitor=0
                  jumpflag=.true.
                  goto 300
               endif
               goto 91
            endif
c           prob=1.0/(1.0+exp(c3))
            prob=1.0/exp(c3)

c           Evaluate the probability by comparing it against chance.

            call rangen(iseed,rannum)
            if(prob.gt.rannum)then
c               Accept higher cost position.
                c1=c2
                do j=1,narg
                   answer(j)=x(j)
                enddo
                loop2=loop2+1
                monitor=0
            else
c               Reject higher cost position.
                fail=fail+1
                monitor=monitor+1
                if(monitor.gt.20)then
                   monitor=0
                   jumpflag=.true.
                   goto 300
                endif
                goto 91
            endif
        endif

c       Save the minimum cost and associated solution as we go.

        if(c1.lt.mincost)then
            mincost=c1
            do j=1,narg
               minx(j)=answer(j)
            enddo
        endif

c       Reset the solution pointer to the minimum cost
c       location every numreset successful iterations.

        if(mod(loop,numreset).eq.0)then
            c1=mincost
            do j=1,narg
               answer(j)=minx(j)
            enddo
        endif

        loop=loop+1

c       Print out a status every IPRNT iterations.

        if(iprnt.gt.0)then
           if(mod(loop,iprnt).eq.0)then
               write(msg,100)answer(1),temp(1),c1,loop1,loop2,fail,loop3
               call XVMESSAGE(msg,' ')
               if(narg.gt.1)then
                  do j=2,narg
                      write(msg,100) answer(j),temp(j)
                      call XVMESSAGE(msg,' ')
                  enddo
               endif
100            format(3(1x,g11.5),4(i5,3x))
               loop1=0
               loop2=0
               loop3=0
               fail=0
           endif
        endif

      enddo
c     END of MAIN LOOP

c     Put minimum solution into ANSWER & it's cost into
c     RANGE.

      do j=1,narg
         answer(j)=minx(j)
      enddo
      range(1)=mincost

      return
      end


      
c ****************************************************************
      SUBROUTINE ACTIV(TOP,MID,BOT,DNTH,ACTTH,ISS,NSIN,NSOUT,
     *  SAMP,ACT,IDNDEV,IBELOW,IOFF,suna)
C  ACTIVITY COMPUTING ALGORIYHM
C  DNTH=DN THRESHOLD, ACTTH=ACTIVITY THRESHOLD, SAMP=SAMPLE LOCATION
C  ACT IS ACTIVITY VALUE
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3),ACTTH,DNTH,MIDLM1,MIDLP1,TOPL,BOTL
      INTEGER*4 TOPLM1,BOTLP1,TOPLP1,BOTLM1
      INTEGER*2 TOP(*),MID(*),BOT(*),SAMP(*),ACT(*)
      NS=NSIN+ISS-2
      K=ISS+1
      NSOUT=0
      scale=180./3.14159
      DO 100 L=K,NS

         IF(MID(L).LT.DNTH)GO TO 100 !CHECK FOR DNTH

C        CHECK FOR ACTIVITY
         MIDLM1=MID(L-1)
         MIDLP1=MID(L+1)
         TOPL=TOP(L)
         BOTL=BOT(L)
         I1=MIDLM1-MIDLP1
         I2=TOPL-BOTL
         ITOT=IABS(I1)+IABS(I2)
         IF(ITOT.LT.ACTTH)GO TO 100

         TOPLM1=TOP(L-1)
         TOPLP1=TOP(L+1)
         BOTLM1=BOT(L-1)
         BOTLP1=BOT(L+1)
         I1=TOPLM1-BOTLP1
         I2=TOPLP1-BOTLM1
         ITOT=IABS(I1)+IABS(I2)
         IF(ITOT.LT.ACTTH)GO TO 100

         I1=TOP(L-1)
         I2=TOP(L)
         I3=TOP(L+1)
         I4=MID(L-1)
         I5=MID(L+1)
         I6=BOT(L-1)
         I7=BOT(L)
         I8=BOT(L+1)

C        CHECK FOR BELOW
c         IF(IBELOW.EQ.-32760)GO TO 101
         MIN=MIN0(I1,I2,I3,I4,I5,I6,I7,I8)
         IF(MIN.GT.IBELOW)GO TO 100

C        CHECK FOR MISSING LINE TEST
         IF(I1.LE.IOFF.AND.I2.LE.IOFF.AND.I3.LE.IOFF)GO TO 100
         IF(I6.LE.IOFF.AND.I7.LE.IOFF.AND.I8.LE.IOFF)GO TO 100

c101      CONTINUE   
c check that the limb point is facing away from the sunangle
         if(suna.eq.-400.)goto 102
         topl=i1+i2+i3-(i6+i7+i8)
         botl=i3+i5+i8-(i1+i4+i6)
         if(topl.eq.0.and.botl.eq.0)goto 100
         angl=atan2(real(topl),real(botl))
         angl=270.-angl*scale ! angl points towards the planet center.
         if(angl.gt.360.) angl=angl-360.
         if(abs(suna-angl).gt.90.and.abs(suna-angl).lt.270.)goto 100
102      continue

c accept point
         NSOUT=NSOUT+1
         IF(ITOT.GT.32767)ITOT=32767   !ADDED IN CONVERSION
         IF(ITOT.LT.-32768)ITOT=-32768 !ADDED IN CONVERSION
         ACT(NSOUT)=ITOT
         SAMP(NSOUT)=L
100   CONTINUE
      RETURN
      END

c***********************************************************************
      SUBROUTINE AUTO(IHALF,IN,HIST,NLR2,ISL,ISS,INL,INS,FRACT,
     *  PERC,LINC,IACTTH,IBELOW,SMAA,SMIA,TOL,IDNTH,IOFF,DSRNGE)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3),IN(*),HIST(14000),DSRNGE

C  COMPUTE PICTURE HISTOGRAM
      N=INL+ISL-1
      CALL MVE(4,14000,0,HIST,0,1)
      ITOT=0
C     HSUB DOES NOT WANT PIXELS UNPACKED 
      CALL XVCLOSE(INP(1),IND,' ')
      CALL XVOPEN(INP(1),IND,'IO_ACT','SA','OPEN_ACT','SA',' ')
      JTOT=0
      DO L=ISL,N,LINC
           CALL XVREAD(INP(1),IN,ISTAT,'LINE',L,'SAMP',ISS,
     *                 'NSAMPS',INS,' ')
           CALL HSUB(2-IHALF,INS,IN,HIST,0,13998)
           NREC=NREC+1
      enddo
      CALL XVCLOSE(INP(1),IND,' ')
      CALL XVOPEN(INP(1),IND,'IO_ACT','SA','OPEN_ACT','SA',
     *            'U_FORMAT','HALF',' ')

C  GET TOP 'PERCENT' POINT
      ISUM=0
      DO J=1,14000
         ISUM=ISUM+HIST(J)
      enddo
      IPER=PERC*ISUM/100.+0.5
      ISUM=0
      I=14000
120   I=I-1
      ISUM=ISUM+HIST(I)
      IF(ISUM.LE.IPER)GO TO 120

      IF(FRACT.LE.0.0)GO TO 160
         !HERE WE TRY TO COMPENSATE FOR THE BLACK SKY AROUND THE PLANET
         JAREA=3.14159*SMAA*SMIA/LINC  !JAREA IS NUMBER OF PIXELS OF PLANET
         IF(DSRNGE.NE.0)JAREA=JAREA*.8*.8 !CONVERT TO IMAGE SPACE
         JAREA=IFIX(FLOAT(JAREA)*FRACT)
         !COUNT DOWN FROM TOP TO FIND LOWEST DN ON PLANET
         JSUM=0
         J=14000
150      J=J-1
         JSUM=JSUM+HIST(J)
         IF(JSUM.LE.JAREA.AND.J.GT.0)GO TO 150
         I=J

160   JSUM=0
      !COMPUTE AVERAGE PIXEL VALUE

C  RESET FARENC PARAMETERS
      call prnt(4,1,i,'Peak DN value= .')
      IACTTH=(I-IOFF)/7
      CALL PRNT(4,1,IACTTH,'ACTIVITY RESET TO .')
      ibelow=(i-ioff)/7+ioff
      call prnt(4,1,ibelow,'BELOW RESET TO .')
      idnth=ibelow
      call prnt(4,1,idnth,'DNTHRESH RESET TO .')

      RETURN
      END
      
c ****************************************************************
      SUBROUTINE CIRCLE(NE,NU,C4,C4L,COEF,CX,CY,R,ER,TOLL,SIGM,NDSOUT,
     *  IPRINT,SAMP,SUN,SERNO,FAR,FOCA,SCAL,C,CL,project,lmbpts,
     *  os_pts)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3),serno
      REAL*4 ER(*),COEF(*),NALS(2),C4(2800,5),C4L(2800)
      real*4 lmbpts(2800,2),os_pts(2800,2)
      REAL*8 C(1400,5),CL(1400),R2,CXX(5)
      character*5 project
      CHARACTER*30 MSG
      INTEGER*2 SAMP(*)
      TOL=TOLL

C  UNPACK R*4 TO R*8 SET UP LSQP
      K=NE+1
      DO J=1,NE
         K=K-1
         C(K,1)=os_pts(k,1)
         C(K,2)=os_pts(k,2)
         C(K,3)=1.0D0
         CL(K)=C(K,1)**2+C(K,2)**2
      enddo
      initial=0
      goto 100 ! edit points first
262   CALL LSQP(NE,NU,C,CL,CXX)

C  EVALUATE COEFFICIENTS FOR A CIRCLE
      CY=CXX(1)/2.0
      CX=CXX(2)/2.0
      R2=CXX(3)+CXX(1)**2/4.D0+CXX(2)**2/4.0D0
      R=DSQRT(R2)
      initial=1

C  COMPUTE SIGMA FOR ERROR IN RADIUS SQUARED
100   ERMAX=0.0
      IPOS=1
      SUM=0.0
      SUM2=0.0
      DO 260 J=1,NE
         ER(J)=DSQRT((C(J,1)-CY)**2+(C(J,2)-CX)**2)-R
         SUM=SUM+ER(J)
         SUM2=SUM2+ER(J)**2
         IF(ABS(ER(J)).LT.ERMAX)GO TO 260
         ERMAX=ABS(ER(J))
         IPOS=J
260   CONTINUE
      SIGMA=SIGM*SQRT((SUM2-SUM*SUM/NE)/(NE-1))
      if(ermax.le.tol.and.initial.eq.0) goto 262
      IF(ERMAX.LE.TOL)GO TO 300
C  THIS ASSURES AT LEAST ONE POINT REJECTED PER LOOP
      IF(SIGMA.GT.TOL.AND.SIGMA.GT.ERMAX)SIGMA=ERMAX-.001

C  REJECT ALL POINTS WHOSE ERROR IS .GT.SIGMA AND TOL
      BIG=SIGMA
      IF(SIGMA.LT.TOL)BIG=TOL
      K=0
      DO 270 J=1,NE
         IF(ABS(ER(J)).GT.BIG)GO TO 270
            K=K+1
            CL(K)=CL(J)
            C(K,1)=C(J,1)
            C(K,2)=C(J,2)
            lmbpts(k,1)=lmbpts(j,1)
            lmbpts(k,2)=lmbpts(j,2)
            os_pts(k,1)=os_pts(j,1)
            os_pts(k,2)=os_pts(j,2)
270   CONTINUE
      IF(K.LT.6)CALL XVMESSAGE(
     *          'RAN OUT OF DATA,TERMINATE WITH LAST ITERATION',' ')
      IF(K.LT.6)GO TO 261
      IF(NDSOUT.EQ.2.AND.IPRINT.NE.0)CALL PRNT(4,1,NE-K,' DELETE.')
      NE=K
      GO TO 262

300   continue

261   CONTINUE
      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE('LEAST SQUARES CIRCLE SOLUTION, OBJECT SPACE:',' ')
      CALL PRNT(7,1,CY,'LINE   POSITION OF PLANET CENTER.')
      CALL PRNT(7,1,CX,'SAMPLE POSITION OF PLANET CENTER.')
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         if(serno.eq.4.or.serno.eq.6)then
             CALL MWATNA(SERNO,CY,CX,NALS(1),NALS(2),*999)
             CALL PRNT(7,2,NALS,
     *                 'SIMULTANEOUS EXPOSURE NA LINE & SAMPLE=.')
         endif
      ENDIF
 999  CONTINUE
      CALL PRNT(7,1,R,'RADIUS OF PLANET IN PIXELS=.')
      IF(FAR.EQ.-400.)GO TO 263
      IF(FOCA.EQ.-400.)GO TO 263
      IF(SCAL.EQ.-400.)GO TO 263
      S=R*FAR/(DSQRT(R2+(DBLE(FOCA)*SCAL)**2)-R)
      CALL PRNT(7,1,S,'RADIUS OF PLANET IN KM .')
263   CONTINUE
      CALL PRNT(7,1,SIGMA/SIGM,'SIGMA OF RADIUS ERRORS .')
      CALL PRNT(7,1,ERMAX,'LARGEST RADIUS ERROR FOR ANY POINT.')
      CALL PRNT(4,1,NE,'NUMBER OF POINTS USED FOR FIT     .')
C  PRINT OUT FINAL POINTS
      IF(IPRINT.ne.0)then
         call XVMESSAGE(' ',' ')
         CALL XVMESSAGE('  LINE     SAMPLE    RADIUS ERROR',' ')
         DO 280 J=1,NE
            WRITE(MSG(1:10),'(F10.1)') C(J,1)
            WRITE(MSG(11:20),'(F10.1)') C(J,2)
            WRITE(MSG(21:30),'(F10.1)') ER(J)
            CALL XVMESSAGE(MSG,' ')
280      CONTINUE
      endif
285   CONTINUE
      RETURN
      END

c ****************************************************************
      SUBROUTINE CLUSTR(CX,CL,CS,CLL,CSS,CXX,NE,ISIZE,NUMBER)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3)
      REAL*4 CL(*),CX(*),CS(*),CLL(*),CSS(*),CXX(*)
      NN=0
      SIZE=ISIZE
      DO 10 L=1,NE
      AL=CL(L)
      AS=CS(L)
      N=0
      DO 20 J=1,NE
         A=ABS(AL-CL(J))
         B=ABS(AS-CS(J))
         SEP=AMAX1(A,B)
         IF(SEP.GT.SIZE)GO TO 20
            N=N+1
20    CONTINUE
      IF(N.LT.NUMBER)GO TO 10
      NN=NN+1
      CLL(NN)=AL
      CSS(NN)=AS
      CXX(NN)=CX(L)
10    CONTINUE
      IF(NN.GT.0)GO TO 30
      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE('CLUSTER REJECTS ALL POINTS',' ')
      CALL ABEND
30    CALL MVE(7,NN,CLL,CL,1,1)
      CALL MVE(7,NN,CSS,CS,1,1)
      CALL MVE(7,NN,CXX,CX,1,1)
      NE=NN
      CALL PRNT(4,1,NE,'Points remaining after de-cluster =.')
      IF(NE.LT.6)CALL mabend('INSUFFICIENT POINTS LOCATED')
      RETURN
      END
      

c***********************************************************************
      SUBROUTINE CONEL(NE,NU,C4,C4L,COEF,ER,TOL,SIGM,NDSOUT,IPRINT,SUN,
     *   SERNO,FAR,FOCA,SCAL,C,CL,ANGL,SMAA,SMIA,RADRNG,project,
     *   lmbpts,os_pts,dell,dels)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3),serno
      character*5 project
      REAL*4 ER(*),COEF(*),LINE(1400),SAMP(1400),LIN,C4(2800,5),
     * C4L(2800),lmbpts(2800,2),os_pts(2800,2),nals(2)
      REAL*8 C(1400,5),CL(1400)
      CHARACTER*30 MSG
      PI=ACOS(-1.0)
      COSE=COS(-ANGL*PI/180.)
      SINE=SIN(-ANGL*PI/180.)
C  C4(J,1)=LINE  C4(J,2)=SAMPLE

C  SAVE LINE,SAMPLE FOR LATER ROUTINES
      DO 100 J=1,NE
         LINE(J)=os_pts(j,1)
         SAMP(J)=os_pts(j,2)
100   CONTINUE
      cy=coef(1)
      cx=coef(2)
      initial=0
      goto 400   ! check points first

C  EVALUATE COEFFICIENTS FOR A CONSTRAINED ELLIPSE
262   CALL CONELF(C,CL,NE,COEF,ANGL,SMAA,SMIA,SERNO,LINE,SAMP,IPRINT,
     *  project)
      CY=COEF(1)
      CX=COEF(2)
      initial=1
      IF(IPRINT.EQ.1)CALL PRNT(7,1,CX,' CX= .')
      IF(IPRINT.EQ.1)CALL PRNT(7,1,CY,' CY= .')
      IF(IPRINT.EQ.1)CALL PRNT(4,1,NE,' NE= .')

C  COMPUTE SIGMA FOR ERROR IN RADIUS SQUARED
400   ERMAX=0.0
      IPOS=1
      SUM=0.0
      SUM2=0.0
      DO 260 J=1,NE
         LIN=(SAMP(J)-CX)*SINE+(LINE(J)-CY)*COSE
         SAM=(SAMP(J)-CX)*COSE-(LINE(J)-CY)*SINE
         IF(SAM.EQ.0.0)SAM=.01
         SL=LIN/SAM
         A=SQRT(SMIA*SMIA+SMAA*SMAA*SL*SL)
         X1=SMAA*SMIA/A
         Y1=SL*X1
         ER(J)=SQRT((ABS(LIN)-ABS(Y1))**2+(ABS(SAM)-ABS(X1))**2)
         SUM=SUM+ER(J)
         SUM2=SUM2+ER(J)**2
         IF(ABS(ER(J)).LT.ERMAX)GO TO 260
         ERMAX=ABS(ER(J))
         IPOS=J
260   CONTINUE
      IF ( (SUM2-SUM*SUM/NE) .LE. 0.0) THEN
         SIGMA=0.0
      ELSE
         SIGMA=SIGM*SQRT((SUM2-SUM*SUM/NE)/(NE-1))
      ENDIF
      if(ermax.le.tol.and.initial.eq.0) goto 262
      IF(ERMAX.LE.TOL)GO TO 300
C  THIS ASSURES AT LEAST ONE POINT REJECTED PER LOOP
      IF(SIGMA.GT.TOL.AND.SIGMA.GT.ERMAX)SIGMA=ERMAX-.001

C  REJECT ALL POINTS WHOSE ERROR IS .GT.SIGMA AND TOL
      BIG=SIGMA
      IF(SIGMA.LT.TOL)BIG=TOL
      K=0
      DO 270 J=1,NE
         IF(ABS(ER(J)).GT.BIG)GO TO 270
         K=K+1
         LINE(K)=LINE(J)
         SAMP(K)=SAMP(J)
         lmbpts(k,1)=lmbpts(j,1)
         lmbpts(k,2)=lmbpts(j,2)
         os_pts(k,1)=os_pts(j,1)
         os_pts(k,2)=os_pts(j,2)
270   CONTINUE
      IF(K.LT.6)CALL XVMESSAGE
     *          ('RAN OUT OF DATA,TERMINATE WITH LAST ITERATION',' ')
      IF(K.LT.6)GO TO 261
      IF(NDSOUT.EQ.2.AND.IPRINT.NE.0)CALL PRNT(4,1,NE-K,' DELETE.')
      NE=K
      GO TO 262

300   continue

C  RESET REAL*8 BUFFER FROM STORED LINE,SAMPLE VALUES
261   continue
      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE
     *         ('LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:',' ')
      CALL PRNT(7,1,CY+dell,' LINE   POSITION OF PLANET CENTER.')
      CALL PRNT(7,1,CX+dels,' SAMPLE POSITION OF PLANET CENTER.')
      if((project.eq.'VGR-1').or.(project.eq.'VGR-2'))then
         if(serno.eq.4.or.serno.eq.6)then
             CALL MWATNA(SERNO,CY,CX,NALS(1),NALS(2),*999)
             CALL PRNT(7,2,NALS,
     *                 'SIMULTANEOUS EXPOSURE NA LINE & SAMPLE=.')
         endif
      ENDIF
 999  CONTINUE
      CALL PRNT(7,1,SIGMA/SIGM,'SIGMA OF RADIUS ERRORS .')
      CALL PRNT(7,1,ERMAX,'LARGEST RADIUS ERROR FOR ANY POINT.')
      CALL PRNT(4,1,NE,'NUMBER OF POINTS USED FOR FIT     .')

C  PRINT OUT FINAL POINTS
      IF(IPRINT.EQ.0)GO TO 285
      call XVMESSAGE(' ',' ')
      CALL XVMESSAGE('  LINE     SAMPLE    RADIUS ERROR        ',' ')
      DO 280 J=1,NE
            WRITE(MSG(1:10),'(F10.1)') LINE(J)
            WRITE(MSG(11:20),'(F10.1)') SAMP(J)
            WRITE(MSG(21:30),'(F10.1)') ER(J)
            CALL XVMESSAGE(MSG,' ')
280   CONTINUE
285   CONTINUE

      RETURN
      END
      
      
c ****************************************************************
      SUBROUTINE CONELF(C,CL,NE,COEF,ANGL,SMAA,SMIA,SERNO,LINE,SAMP,
     *   IPRINT, project)
C  CONSTRAINED ELLIPSE KNOWING   SMAA,SMIA,ANGL
C  ON INPUT   SAMP=SAMPLE  LINE=LINE
      COMMON/FARENC_FILES/INP,OUT
      character*5 project
      INTEGER*4 INP(2),OUT(3),serno
      REAL*4 COEF(*),LINE(*),SAMP(*)
      REAL*8 C(1400,5),CL(1400),CXX(5)
      REAL*8 PI,CO,SI,A,B,CO2,SI2,RL,RS
      PI=3.1415926536
      CO=DCOS(ANGL*PI/180.D0)
      SI=DSIN(ANGL*PI/180.D0)
      IF (CO .EQ. 0.0 .OR. SI .EQ. 0.0)  THEN
          ANGL= ANGL +.001            !JEAN LORRE THOUGHT ANGL = 0 MIGHT BE
          CO=DCOS(ANGL*PI/180.D0)    !A SINGULARITY AND THAT THE CODE SHOULD
          SI=DSIN(ANGL*PI/180.D0)    !NOT DEPEND CRITICALLY ON EXACT VALUE
                                     !OF ANGL. PERHAPS SETUP OF CL NEEDS TO BE
      END IF                         !REDERIVED - LOOKS FISHY.  (SXP)
      A=DBLE(SMAA)**2
      B=DBLE(SMIA)**2
      CO2=CO*CO
      SI2=SI*SI
      DO 10 L=1,NE
         RL=LINE(L)
         RS=SAMP(L)
         C(L,1)=2.D0*(-RS*CO2*B-RS*SI2*A-RL*SI*CO*B+RL*SI*CO*A)
         C(L,2)=2.D0*(-RS*SI*CO*B+RS*SI*CO*A-RL*SI2*B-RL*CO2*A)
         C(L,3)=CO2*B+SI2*A
         C(L,4)=SI2*B+CO2*A
         C(L,5)=2.D0*(SI*CO*B-SI*CO*A)
         CL(L)=A*B-RS*RS*CO2*B-RS*RS*SI2*A
     *     -RL*RL*SI2*B-RL*RL*CO2*A+2.D0*RL*RS*SI*CO*(A-B)
10    CONTINUE
      NU=5
      CALL LSQP(NE,NU,C,CL,CXX)
      CALL LSQING(CXX,CL,C,NE,IPRINT)

      COEF(1)=CXX(2)
      COEF(2)=CXX(1)
      RETURN
      END

      
c ****************************************************************
      SUBROUTINE DRAWLIMB(Q,NLX,NSX,NLR2,LOG,CY,R,CX,IH,COEF,
     *    ELICOE,CONCL,CONCS,SMAA,type,nph,npv,ig,centy,centx,
     *    smia,angl,project,serno)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3),q,type,serno
      REAL*4 COEF(*),ELICOE(*),ig(2700),d2r
      INTEGER*2 LOG(*),WHITE
c      CHARACTER*72 LAB(2)
      character*5 project

      d2r=acos(-1.0)/180.0
      NL=NLX
      NS=NSX
      NB=NSX
      IF(IH.EQ.0)THEN
          NS=NS/2
          WHITE=10000
      ELSE
         WHITE=255
      ENDIF

      CALL XVCLOSE(Q,ISTAT,' ')
c      CALL ITLA(32,LAB,144)
c      CALL MVL(' FARENC CURVES DISPLAY FILE',LAB(1,2),27)

      CALL XVOPEN(Q,ISTAT,'IO_ACT','SA','OPEN_ACT','SA',
     &     'U_NL',NLX,'U_NS',NSX,'OP','UPDATE','U_FORMAT','HALF',' ')
      call chkstat(istat,' error in xvopen,istat=',1,istat)
      CALL XLADD(Q,'HISTORY','FARENC','FARENC CURVES DISPLAY FILE',
     *            ISTAT,'FORMAT','STRING',' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLADD FOR CURVES FILE,STAT=',1,ISTAT)
      LINE=1

C       copy input to curves file
      do l=1,nl 
         call xvread(inp(1),log,istat,'LINE',L,' ')
         call xvwrit(q,     log,istat,'LINE',L,' ')
      enddo
      
      dtheta=360./(2.*smaa)

      if(smaa.eq.smia)then
c circle:
         do dt=0.,360.,dtheta
            il=nint(smaa*sin(dt*d2r)+centy)
            is=nint(smaa*cos(dt*d2r)+centx)
            call writecurve(q,log,type,nph,npv,ig,white,il,is,nl,ns,
     +                      project,serno)
         enddo
      else
c ellipse:
         cosangl=cos(angl*d2r)
         sinangl=sin(angl*d2r)
         smaa2=smaa*smaa
         smia2=smia*smia
         do dt=0.,360.,dtheta
            costh=cos(dt*d2r)
            sinth=sin(dt*d2r)
            r=sqrt((smaa2*smia2)/(smaa2*sinth*sinth+smia2*costh*
     *              costh))
            y=r*sinth
            x=r*costh
            yy=y*cosangl-x*sinangl
            xx=x*cosangl+y*sinangl
            il=nint(centy-yy)
            is=nint(xx+centx)
            call writecurve(q,log,type,nph,npv,ig,white,il,is,nl,ns,
     +                      project,serno)
         enddo
      endif    
      
      RETURN
      END

      
c ****************************************************************
      SUBROUTINE FIXPTS(MAV,MIV,MAH,MIH,PERC,NPIX,NL,NE,K,SIGA,C,CL,
     *  SMAV,SMIV,LMAH,LMIH,NU,LIMIT,MAXNUM,LCIRCL,iclun)
c Selects points to use . returns line in c(n,1), sample in c(n,2).
c NE= # points.
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3),KKMK,ip(2800)
      INTEGER*2 MAV(1400),MIV(1400),MAH(1400),MIH(1400),SMAV(1400),
     *SMIV(1400),LMAH(1400),LMIH(1400)
      REAL*4 C(2800,5),CL(2800),C2(2800),CPTR(2800)
      REAL*8 SUM2

      IF(LCIRCL.EQ.1)GO TO 110

C  CHECK IF SECOND VERTICAL MAX IS LESS THAN PERC*1-ST MAX
      DO 240 J=1,NPIX
      IF(MAH(J)*PERC.GT.MIH(J))MIH(J)=0
240   CONTINUE
110   CONTINUE

C  OMIT ACTIVITY VALUES OUTSIDE OF MEAN+-SIGA*SIGMA OF ACTIVITY

C  COMPUTE MEAN AND SIGMA
      SUM=0.
      SUM2=0.d0
      K=0
      DO 300 J=1,NPIX
      IF(MAH(J).EQ.0)GO TO 301
      K=K+1
      SUM=SUM+MAH(J)
c      MAHJ=MAH(J)	
c      SUM2=SUM2+MAHJ**2
      SUM2=SUM2+dble(MAH(J))*dble(MAH(J))
301   IF(MIH(J).EQ.0)GO TO 300
      K=K+1
      SUM=SUM+MIH(J)
c      MIHJ=MIH(J)
c      SUM2=SUM2+MIHJ**2
      SUM2=SUM2+dble(MIH(J))*dble(MIH(J))
300   CONTINUE
      DO 310 J=1,NL
         IF(MAV(J).EQ.0)GO TO 311
            K=K+1
            SUM=SUM+MAV(J)
c            MAVJ=MAV(J)
c            SUM2=SUM2+MAVJ**2
            SUM2=SUM2+dble(MAV(J))*dble(MAV(J))
311      IF(MIV(J).EQ.0)GO TO 310
            K=K+1
            SUM=SUM+MIV(J)
c            MIVJ=MIV(J)
c            SUM2=SUM2+MIVJ**2
            SUM2=SUM2+dble(MIV(J))*dble(MIV(J))
310   CONTINUE
      NE=K
      IF(NE.LT.6)CALL MABEND('INSUFFICIENT POINTS LOCATED')
      SIGACT=SIGA*SQRT((SUM2-SUM*SUM/K)/(K-1))
      RMEAN=SUM/K
      CALL PRNT(7,1,RMEAN, 'AVE ACTIVITY.')
      CALL PRNT(7,1,SIGACT,'      + -   .')

C  DELETE POINTS MORE THAN RMEAN+-SIGACT
      IL=RMEAN-SIGACT+0.5
c      IR=RMEAN+SIGACT+0.5	! not used, MA/MI are both maxima (?) -- lwk
      DO 320 J=1,NPIX
      IF(MAH(J).LT.IL)MAH(J)=0
      IF(MIH(J).LT.IL)MIH(J)=0
320   CONTINUE
      DO 330 J=1,NL
      IF(MAV(J).LT.IL)MAV(J)=0
      IF(MIV(J).LT.IL)MIV(J)=0
330   CONTINUE

C  PACK DATA INTO BUFFERS FOR LSQP INPUT

      K=0
      DO 250 J=1,NPIX
      IF(MAH(J).EQ.0)GO TO 251
      K=K+1
      C(K,1)=LMAH(J)
      C(K,3)=J
251   IF(MIH(J).EQ.0)GO TO 250
      K=K+1
      C(K,1)=LMIH(J)
      C(K,3)=J
250   CONTINUE
      DO 252 L=1,NL
      IF(K-4.GT.LIMIT)GO TO 100
      IF(MAV(L).EQ.0)GO TO 253
      K=K+1
      C(K,1)=L
      C(K,3)=SMAV(L)
253   IF(MIV(L).EQ.0)GO TO 252
      K=K+1
      C(K,1)=L
      C(K,3)=SMIV(L)
252   CONTINUE
100   NE=K
      NU=3

C  SORT POINTS BY LINE
c      CALL R4SORT(C(1,1),C(1,3),NE)
      call mve(7,NE,C(1,1),C2(1),1,1)
      call mve(7,NE,C(1,3),CPTR(1),1,1)
      call ssortp(c(1,1),1,ne,ip)
      do ii = 1, ne
         c(ii,1) = c2(ip(ii))
         c(ii,3) = cptr(ip(ii))
      end do
      call XVMESSAGE(' ',' ')
c     CALL PRNT(4,1,NE,' NUMBER OF POINTS BEFORE LINE ELIMINATION.')
C     DELETE POINTS WHEN MORE THAN MAXNUM ARE ON SAME LINE

      IPTIN=1
      IPTOUT=1
      CLINE=C(1,1)
      ICNTR=0
c     CALL PRNT2(4,1,MAXNUM,' MAXNUM=.',80)
 404  N=1
 401  IF(IPTIN+N.GE.NE)GO TO 405
      IF(C(IPTIN+N,1).NE.CLINE)GO TO 405
      N=N+1
      GO TO 401
 405  IF(N.GT.MAXNUM)GO TO 402
      CALL MVE(7,N,C(IPTIN,1),C(IPTOUT,1),1,1)
      CALL MVE(7,N,C(IPTIN,3),C(IPTOUT,3),1,1)
      IPTOUT=IPTOUT+N
      ICNTR=IPTOUT-1
 402  IPTIN=IPTIN+N
      IF(IPTIN.GT.NE)GO TO 403
      CLINE=C(IPTIN,1)
      GO TO 404
 403  NE=ICNTR

C  FOR EACH LINE SORT BUFFER BY SAMPLE

      call prnt(4,1,ne,'Initial number of points located= .')
c     CALL PRNT2(4,1,NE,' NUMBER OF POINTS AFTER LINE ELIMINATION.',80)
      K=0
370   K=K+1
      IF(K+1.GT.NE)GO TO 371
      L=C(K,1)
      LL=C(K+1,1)
      IF(L.NE.LL)GO TO 370
      KK=K+1
372   KK=KK+1
      IF(KK.GT.NE)GO TO 371
      LL=C(KK,1)
      IF(LL.EQ.L)GO TO 372
c      CALL R4SORT(C(K,3),C(K,1),KK-K)
      KKMK=KK-K
      call mve(7,KKMK,C(K,3),C2(1),1,1)
      call mve(7,KKMK,C(K,1),CPTR(1),1,1)
      call ssortp(c(K,3),1,KKMK,ip)
      do ii = 1, KKMK
         c(K-1+ii,3) = c2(ip(ii))
         c(K-1+ii,1) = cptr(ip(ii))
      end do
      K=KK-1
      GO TO 370
371   CONTINUE

C  DELETE DUPLICATE POINTS AND SET UP LSQP BUFFERS

      RL=C(1,1)
      RS=C(1,3)
      K=1
      DO 360 L=2,NE
      IF(RL.EQ.C(L,1).AND.RS.EQ.C(L,3))GO TO 360
      RL=C(L,1)
      RS=C(L,3)
      K=K+1
       C(K,1)=RL
      C(K,3)=RS
360   CONTINUE
      NE=K
      IF(NE.LT.6)CALL MABEND('INSUFFICIENT POINTS LOCATED')
c      IF(NE.GT.LIMIT/2)CALL QPRINT(' POINTS TRUNCATED AT 1400',25)
c      IF(NE.GT.LIMIT/2)NE=LIMIT/2

      if(ne.gt.limit/2)then
c select every other point
         l=0
         do k=1,ne,2
            l=l+1
            c(l,1)=c(k,1)
            c(l,3)=c(k,3)
         enddo
         iclun=iclun/2
         ne=l
         call prnt(4,1,ne,'points decimated to:.')
      endif

      CALL MVE(7,NE,C(1,3),C(1,2),1,1)
      RETURN
      END
      
c ****************************************************************
      SUBROUTINE GEOMIT(CL,ICL,IGEOM,DSRN,imgtype,DELL,DELS,C,IH,IPRINT,
     *  R,SMAA,RGEMA,SIZE,PUTNOR,ANGL,CENY,CENX,project,serno)
C  TO TRANSFORM CENTER OF PLANET TO CENTER OF OUTPUT PICTURE
C  R=RADIUS CIRCLE  RGEMA=GEN ELLIPSE SEMMAJ AXIS  SMAA=CON ELLIPS SEMMA
c  cl(1)=line center  cl(2)=sample center
      INTEGER*4 INP(2),OUT(3),DSRN,ICL(*),serno
      REAL*4 CL(2800),C(2300)
      character*5 project
      COMMON/FARENC_FILES/INP,OUT

      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)

C  DETERMINE THE SHIFT TO CENTER OUTPUT
      IF(CENY.NE.-999.AND.CENX.NE.-999.) GO TO 5
C      CENY=ICL(23)/2.
C      CENX=ICL(24)/2.
C      IF(IH.EQ.0)CENX=ICL(24)/4.
      CENY=NL/2.
      CENX=NS/2.
5     CONTINUE
      call XVMESSAGE(' ',' ')
      call XVMESSAGE
     *     ('After the geom the new planet center will be at:',' ')
      CALL PRNT(7,1,CENY,'PLANET CENTER MOVED TO LINE  =.')
      CALL PRNT(7,1,CENX,'PLANET CENTER MOVED TO SAMPLE=.')
      SHS=CENX-CL(2)-DELS
      SHL=CENY-CL(1)-DELL

C  PREPARE OUTPUT DATA SET FOR GEOMA

      CALL MVE(4,9,ICL(21),ICL(1),1,1)
      ICL(7)=1
      ICL(8)=1
      IF(imgtype.eq.8)then

C  SET UP SYS000 TO CONTAIN GEOMA PARAMETERS (NO GEOM DATA SET PROVIDED)
      ICL(10)=34
c      CALL MVCL(NAH,ICL(11),4)
C      CALL MVCL(BLANK,ICL(12),4)
      ICL(13)=1
c      CALL MVCL(NAV,ICL(14),4)     ! ICL(14)=NAV
C      CALL MVCL(BLANK,ICL(15),4)   ! ICL(15)=BLANK
      ICL(16)=1
C      CALL MVCL(TIEP,ICL(17),4)    ! ICL(17)=TIEP
C      CALL MVCL(BLANK,ICL(18),4)   ! ICL(18)=BLANK
       CL(19)=1.+SHL
       CL(20)=1.+SHS
       CL(21)=1.
       CL(22)=1.
       CL(23)=1.+SHL
       CL(24)=1000.+SHS
       CL(25)=1.
       CL(26)=1000.
       CL(27)=1000.+SHL
       CL(28)=1.+SHS
       CL(29)=1000.
       CL(30)=1.
       CL(31)=1000.+SHL
       CL(32)=1000.+SHS
       CL(33)=1000.
       CL(34)=1000.

      else if(project.eq.'GLL  ' .or. project.eq.'CASSI') then
c       create a 14 by 14 grid of synthetic reseaus, using GLLGCOR to
c       compute the object locations.
c        CALL MVCL(NAH,ICL(11),4)      !  ICL(11)=NAH
C        CALL MVCL(BLANK,ICL(12),4)    !  ICL(12)=BLANK
        icl(13)=13
c        CALL MVCL(NAV,ICL(14),4)      !  ICL(14)=NAV
C        CALL MVCL(BLANK,ICL(15),4)    !  ICL(15)=BLANK
        icl(16)=13
C        CALL MVCL(TIEP,ICL(17),4)     !  ICL(17)=TIEP
C        CALL MVCL(BLANK,ICL(18),4)    !   ICL(18)=BLANK
        n1=(icl(13)+1)*(icl(16)+1)
        icl(10)=n1*4+18
        k=0
        do j=1,icl(16)+1
          do i=1,icl(13)+1
            k=k+1
            l=(k-1)*4+19
            cl(l+2)=(j-1)*61.54
            cl(l+3)=(i-1)*61.54
            call gllgcor(ind,cl(l+2),cl(l+3),cl(l),cl(l+1),1,serno)
            cl(l)=cl(l)+shl
            cl(l+1)=cl(l+1)+shs
          enddo
         enddo

      else
C        SET UP SYS000 FOR PROVIDED GEOMA DATA SET
C        FIRST OFFSET NEWLINE, NEWSAMP BY SHL,SHS
         CALL MVE(4,1,C(3),N1,1,1)
         CALL MVE(4,1,C(6),N2,1,1)
         N1=(N1+1)*(N2+1)
         N2=5
         DO J=1,N1
           N2=N2+4
           C(N2)=C(N2)+SHL
           C(N2+1)=C(N2+1)+SHS
         ENDDO
         CALL MVE(4,N1*4+8,C,CL(11),1,1)
         ICL(10)=N1*4+18
      endif

C  SCALE OUTPUT IMAGE SO PLANET MAJOR AXIS IS 'SIZE' OF SMALLEST
C  OUTPUT PICTURE DIMENSION/2
      IF(SIZE.ne.-1.0)then
         IF(SIZE.LT..001.OR.SIZE.GT.100.)then
            CALL PRNT(7,1,SIZE,' UNACCEPTABLE SIZE VALUE .')
            GO TO 15
         endif
         if(size.gt.1.0)then
            call XVMESSAGE
     *           ('RESCALE PARAMETER WARNING: planet > image',' ')
         endif
         RR=SMAA
         N1=(ICL(13)+1)*(ICL(16)+1)
         S=CENX
         IF(CENY.LT.CENX)S=CENY
         S=SIZE*S/RR
         call XVMESSAGE(' ',' ')
         CALL PRNT(7,1,S,'PLANET EXPANDED BY A FACTOR OF .')
         DO J=1,N1
            K=(J-1)*4+19
            CL(K)=(CL(K)-CENY)*S+CENY
            CL(K+1)=(CL(K+1)-CENX)*S+CENX
         ENDDO
      endif
15    CONTINUE

C  ROTATE PLANET ABOUT CENTER SO THE NORTH ANGLE IS NOW
C  SET AT PUTNOR
      IF(PUTNOR.EQ.-400.)GO TO 21
      IF(ANGL.eq.-400.)then
        call XVMESSAGE(' ',' ')
        CALL MABEND('NORTH ANGLE MUST BE SPECIFIED FOR PUTNOR OPTION')
      endif
      if(abs(angl-putnor).gt.45.)then
        call XVMESSAGE(' ',' ')
        CALL MABEND('Rotation > 45 deg., GEOMA rules violated')
      endif         
22    N1=(ICL(13)+1)*(ICL(16)+1)
      A=(ANGL-PUTNOR)*.017453293
      SINTH=SIN(A)
      COSTH=COS(A)
      DO J=1,N1
          K=(J-1)*4+19
          A=CL(K)
          CL(K)= -(CL(K+1)-CENX)*SINTH +(A-CENY)*COSTH +CENY
          CL(K+1)=(CL(K+1)-CENX)*COSTH +(A-CENY)*SINTH +CENX
      ENDDO
      call prnt(7,1,angl-putnor,'Image rotated clock (deg)=.')
21    CONTINUE

C  PUT IN 'HALF' IF INPUT IS HALFWORD
      IF(IH.EQ.1)GO TO 14
c      N2=ICL(10)
c      CALL MVE(4,N2-10,ICL(N2),ICL(N2+2),-1,-1)
c      ICL(11)=HALF
c      ICL(12)=BLANK
c      ICL(10)=N2+2
14    IND=0
c      CALL PRNT2(4,1,DSRN,' DSRN               =.',80)
c      CALL PRNT2(4,1,ICL(10),' #PAR WORDS PASSED.',80)
      NBPAR=ICL(10)*4
C     NBPAR IS NUMBER OF BYTES OF PARAMETERS TO BE WRITTEN
C--------------------------------------------------------
C-----DELETE CALL TO SYS000    NOT FOR VICAR2
C      CALL SYS000(IND,ICL,14)
C      IF(IND.NE.0)CALL PRNT2(4,1,IND,' SYS000 IND=.')
C--------------------------------------------------------

C     HERE WE WRITE A GEOMA PARAMETER DATASET
C     THIS DATASET CAN BE INPUT TO MAP2 FOR A COMPOSITE TRANSFORMATION

C     GET RID OF FIRST 10 PARAMETER WORDS TO WRITE DATA SET
C      N2=ICL(10)
C      NBPAR=(ICL(10)-9)*4
C      ICL(1)=ICL(10)-10
c      CALL PRNT2(4,1,ICL(1),' ICL(1)=.',80)
C      CALL MVE(4,N2-10,ICL(11),ICL(2),1,1)
C      NREC=(NBPAR/3600.0)+1.0

C      CALL ITLA(32,LAB,144)
C      call mvl(' GEOMA PARAMETER FILE (FARENC)',LAB(1,2),30)
C      CALL XVCLOSE(DSRN,ISTAT,' ')
C      CALL XVOPEN( DSRN,ISTAT,'IO_ACT','SA','OPEN_ACT','SA','U_NL',NREC,
C     *  'U_NS',900,'OP','WRITE','O_FORMAT','REAL','U_FORMAT','REAL',' ')
C      CALL XLDEL(DSRN,'SYSTEM','FORMAT',ISTAT,' ')
C      CALL CHKSTAT(ISTAT,' ERR IN XLDEL,STAT=',1,ISTAT)
C      CALL XLADD(DSRN,'SYSTEM','FORMAT','REAL',ISTAT,
C     *           'FORMAT','STRING',' ')
C      CALL CHKSTAT(ISTAT,' ERR IN XLADD,STAT=',1,ISTAT)

C      DO I=1,NREC
C         CALL XVWRIT(DSRN,ICL(1+(I-1)*900),ISTAT,'LINE',I,' ')
C      enddo

C100   CONTINUE
C      IF(IPRINT.EQ.0)RETURN
C      N1=18
C      IF(IH.EQ.1)N1=20
C      N=(ICL(10)-N1)/8
C      N2=N1-7
c      DO J=1,N
c          N2=N2+8
c          CALL PRNT2(7,8,CL(N2),80)
c      enddo

C...WRITE GEOMETRIC CORRECTION PARAMETERS AS AN IBIS TIEPOINT FILE

      CALL IWRITE_TIEPOINTS(DSRN,ICL(13),ICL(16),0,CL(19),4)

      RETURN
      END

      
c ****************************************************************
      SUBROUTINE GETPTS(MAV,MIV,SMAV,SMIV,MAH,MIH,
     *LMAH,LMIH,MEDI,NLR2,W,NS,SAMP,NSWH,IGEOM,NPIX,
     *ACT,BUF,ISL,INL,ACTTH,DNTH,DIST,ISS,INS,NSOUT,
     *IBELOW,PERC,NL,IOFF,LCIRCL,suna)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3)
      INTEGER*2 MAV(*),MIV(*),SMAV(*),SMIV(*),MAH(*),
     *MIH(*),LMAH(*),LMIH(*),SAMP(*),ACT(*),BUF(1400,3)
      INTEGER*4 W,DNTH,ACTTH,DIST,Z

      Z=ISL
      LAST=ISL+INL-2
      IDNDEV=ACTTH*0.35
      CALL MVE(2,NL,0,MAV ,0,1)
      CALL MVE(2,NL,0,MIV ,0,1)
      CALL MVE(2,NL,0,SMAV,0,1)
      CALL MVE(2,NL,0,SMIV,0,1)
      CALL MVE(2,NPIX,0,MAH,0,1)
      CALL MVE(2,NPIX,0,MIH,0,1)
      CALL MVE(2,NPIX,0,LMAH,0,1)
      CALL MVE(2,NPIX,0,LMIH,0,1)

C  MAIN ACTIVITY ACCUMULATOR LOOP

      IF(MEDI.NE.0)THEN
	CALL XVREAD(INP(1),SAMP(NSWH),ISTAT,'LINE',Z,' ')
	CALL MVE(2,NSWH-1,SAMP(NSWH),SAMP(NSWH-1),0,-1)
	CALL MVE(2,NSWH-1,SAMP(NSWH+NPIX-1),SAMP(NSWH+NPIX),0,1)
	CALL MEDIAN(IND,NPIX,MEDI,SAMP,BUF(1,1),ACT,256)

	CALL XVREAD(INP(1),SAMP(NSWH),ISTAT,'LINE',Z+1,' ')
	CALL MVE(2,NSWH-1,SAMP(NSWH),SAMP(NSWH-1),0,-1)
	CALL MVE(2,NSWH-1,SAMP(NSWH+NPIX-1),SAMP(NSWH+NPIX),0,1)
	CALL MEDIAN(IND,NPIX,MEDI,SAMP,BUF(1,2),ACT,256)

	CALL XVREAD(INP(1),SAMP(NSWH),ISTAT,'LINE',Z+2,' ')
	CALL MVE(2,NSWH-1,SAMP(NSWH),SAMP(NSWH-1),0,-1)
	CALL MVE(2,NSWH-1,SAMP(NSWH+NPIX-1),SAMP(NSWH+NPIX),0,1)
	CALL MEDIAN(IND,NPIX,MEDI,SAMP,BUF(1,3),ACT,256)

      ELSE
	CALL XVREAD(INP(1),BUF(1,1),ISTAT,'LINE',Z,' ')
	CALL XVREAD(INP(1),BUF(1,2),ISTAT,'LINE',Z+1,' ')
	CALL XVREAD(INP(1),BUF(1,3),ISTAT,'LINE',Z+2,' ')
      ENDIF

      Z=Z+1
      DO 300 L=Z,LAST
      LTOP=L-1-((L-2)/3)*3
      LMID=L-((L-1)/3)*3
      LBOT=L+1-(L/3)*3

C  COMPUTE ACTIVITY BUFFER    ACT
      CALL ACTIV(BUF(1,LTOP),BUF(1,LMID),BUF(1,LBOT),DNTH,ACTTH,
     *  ISS,INS,NSOUT,SAMP,ACT,IDNDEV,IBELOW,IOFF,suna)
      IF(NSOUT.EQ.0)GO TO 290
      IF(LCIRCL.EQ.1)GO TO 250

C  ORIGINAL FARENC POINT SELECTION ALGORITHM

C  COMPUTE MAX VALUE IN HORIZONTAL BUFFER ACT
      MAX=ACT(1)
      JJ=1
      DO 210 J=1,NSOUT
      IF(ACT(J).LT.MAX)GO TO 210
      JJ=J
      MAX=ACT(J)
210   CONTINUE
      MAV(L)=MAX
      SMAV(L)=SAMP(JJ)
      IF(NSOUT.EQ.1)GO TO 221
C  COMPUTE NEXT LARGEST MAX AT LEAST DIST PIXELS AWAY IN HORIZ BUFFER
      DO 222 J=1,NSOUT
         ISEP=SMAV(L)-SAMP(J)
         IF(IABS(ISEP).LT.DIST)GO TO 222
         MAX=ACT(J)
         JJ=J
       GO TO 223
222   CONTINUE
      GO TO 224
223   CONTINUE
      DO 220 J=1,NSOUT
         IF(ACT(J).LT.MAX)GO TO 220
         ISEP=SMAV(L)-SAMP(J)
         IF(IABS(ISEP).LT.DIST)GO TO 220
         JJ=J
         MAX=ACT(J)
220   CONTINUE
221   CONTINUE

C  CHECK IF 2-ND LARGEST HORIZ MAX IS LESS THAN PERC*1-ST LARGEST MAX
      SMIV(L)=SAMP(JJ)
      IF(MAV(L)*PERC.LE.MAX)MIV(L)=MAX
224   CONTINUE

C  UPDATE RUNNING 1-ST AND SECOND MAX IN VERTICAL BUFFERS
      DO 230 J=1,NSOUT
      I=SAMP(J)
      IF(ACT(J).LT.MAH(I))GO TO 231
      MIH(I)=MAH(I)
      LMIH(I)=LMAH(I)
      MAH(I)=ACT(J)
      LMAH(I)=L
      ISEP=L-LMIH(I)
      IF(ISEP.GE.DIST)GO TO 230
      LMIH(I)=0
      MIH(I)=0
      GO TO 230
231   CONTINUE
      IF(ACT(J).LT.MIH(I))GO TO 230
      ISEP=L-LMAH(I)
      IF(ISEP.LT.DIST)GO TO 230
      MIH(I)=ACT(J)
      LMIH(I)=L
230   CONTINUE
      GO TO 290

C  LCIRCLE OPTION FOR POINT SELECTION
C  SELECT MOST LEFT & RIGHT POINTS FOR EACH ROW
C     "    "   TOP & BOTTOM    "    "    "  COLUMN

250   CONTINUE
      MAV(L)=ACT(1)
      SMAV(L)=SAMP(1)
      IF(NSOUT.EQ.1)GO TO 251
      MIV(L)=ACT(NSOUT)
      SMIV(L)=SAMP(NSOUT)
251   CONTINUE
      DO 252 J=1,NSOUT
         I=SAMP(J)
         IF(LMAH(I).NE.0)GO TO 253
         LMAH(I)=L
         MAH(I)=ACT(J)
         GO TO 252
253         LMIH(I)=L
            MIH(I)=ACT(J)
252   CONTINUE

290   LINEXT=L+2-((L+1)/3)*3
      IF(L+2.GT.NL)RETURN
      IF(MEDI.NE.0)THEN
	CALL XVREAD(INP(1),SAMP(NSWH),ISTAT,'LINE',L+2,' ')
	CALL MVE(2,NSWH-1,SAMP(NSWH),SAMP(NSWH-1),0,-1)
	CALL MVE(2,NSWH-1,SAMP(NSWH+NPIX-1),SAMP(NSWH+NPIX),0,1)
	CALL MEDIAN(IND,NPIX,MEDI,SAMP,BUF(1,LINEXT),ACT,256)
      ELSE
	CALL XVREAD(INP(1),BUF(1,LINEXT),ISTAT,'LINE',L+2,' ')
      ENDIF

300   CONTINUE
      RETURN
      END
      
c ****************************************************************
      SUBROUTINE LSQING(COEF,CL,C,NE,IPRINT)
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3)
      REAL*8 C(1400,5),CL(1400),COEF(*)
      X=COEF(2)
      Y=COEF(1)
      IMAX=100
      ERR=0.0025D0
      DO 20 ITER=1,IMAX
      UU=0.D0
      UV=0.D0
      VV=0.D0
      UW=0.D0
      VW=0.D0
      DO 10 L=1,NE
      W=CL(L)-X*X*C(L,4)-Y*Y*C(L,3)-X*Y*C(L,5)-X*C(L,2)-Y*C(L,1)
      U=2.D0*X*C(L,4)+Y*C(L,5)+C(L,2)
      V=2.D0*Y*C(L,3)+X*C(L,5)+C(L,1)
      UU=UU+U*U
      UV=UV+U*V
      VV=VV+V*V
      UW=UW+U*W
10    VW=VW+V*W
C      DET=VV*(UU-UV*(UV/VV))        !       UU*VV-UV*UV
C      DX=UW*(VV/DET)-VW*(UV/DET)     !      (UW*VV-VW*UV)/DET
C      DY=VW*(UU/DET)-UV*(UW/DET)     !      (VW*UU-UV*UW)/DET
	DUU=UV/UU
	DUV=UV/VV
	DX=(UW-VW*DUV)/ (UU-UV*DUV)
	DY=(VW-UW*DUU)/ (VV-UV*DUU)
      X=X+DX
      Y=Y+DY
      IF((DX*DX+DY*DY).LE.ERR)GOTO 30
20    CONTINUE
      CALL XVMESSAGE('**INGERSOL ITERATION DOES NOT CONVERGE',' ')
      RETURN
30    COEF(2)= X
      COEF(1)=Y
      IF(IPRINT.EQ.1)
     *CALL PRNT(4,1,ITER,'INGERSOL CONVERGES AFTER ITER=.')
      RETURN
      END
      
c ****************************************************************
      SUBROUTINE LSQP(NE,NU,C,CL,X1)
C2    THE INFORMATION FROM THE MAIN PROGRAM IS,C%I,J<#COEFFICIENT MATRIX
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C2    THE INFORMATION FROM THE MAIN PROGRAM IS,C%I,J<#COEFFICIENT MATRIX
C     CL%I<#ARRAY OF FREE TERMS,  NE#NUMBER OF
C     EQUATIONS AND NU#NUMBER OF UNKNOWNS%NE AND NU NOT TO EXCEED THE
C     DIMENSION SPECIFICATIONS BELOW<.
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS,X1(J)=THE COMPUTED
C     VALUES OF THE UNKNOWNS, V#RESIDUALS I.E. OBSERVED MINUS COMPUTED,
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
      COMMON/FARENC_FILES/INP,OUT
      INTEGER*4 INP(2),OUT(3)
      REAL*8 C(1400,5),CL(1400),X1(5)
      REAL*8 A(5,5),AL(5),R(5,5),RL(5),Q(5,5),X(5),SUM
      DO 57 J=1,NU
         DO 57 I=1,NU
            A(I,J)=0.
            R(I,J)=0.
57       Q(I,J)=0.
      DO 100 I=1,NU
         DO 100 J=1,NU
            DO 100 K=1,NE
100           A(I,J)=A(I,J)+C(K,I)*C(K,J)
      DO 102 I=1,NU
         AL(I)=0.
         DO 102 K=1,NE
102            AL(I)=AL(I)+ C(K,I)*CL(K)

      DO I=1,NU
         if (A(I,I) .eq. 0.0)  call mabend(
     .     'ERROR in routine LSQP: Please check input parameters')
      END DO

      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)

      DO I=1,NU
         if (A(I,I) .eq. 0.0)  call mabend(
     .     'ERROR in routine LSQP: Please check input parameters')
      END DO

      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      RETURN
      END
      
c ****************************************************************
      SUBROUTINE MEDIAN(DCODE,NS,NSW,IN,OUT,HIST,NLEV)
      IMPLICIT INTEGER(A-Z)
      COMMON/FARENC_FILES/INPUT,OUTPUT
      INTEGER*4 INPUT(2),OUTPUT(3)
      INTEGER*2 HIST(*),IN(*),OUT(*)

      N=NSW/2
      CALL MVE(2,NLEV,0,HIST,0,1)

C          HISTOGRAM FOR LEFTMOST POINT
      DO I=1,NSW
          DN=MIN0(IN(I)+1,NLEV)
          DN=MAX0(DN,1)
          HIST(DN)=HIST(DN)+1
      ENDDO

C          FIND INITIAL MEDIAN(K)
      NFREQ=0
      K=0

   20 K=K+1
      NFREQ=NFREQ+HIST(K)
      IF(NFREQ.LE.N)GOTO 20

C          PROCESS NS SAMPLES
      DO 100 I=1,NS
      OUT(I)=K-1
      D1=MIN0(IN(I)+1,NLEV)
      D2=MIN0(IN(NSW+I)+1,NLEV)
      D1=MAX0(D1,1)
      D2=MAX0(D2,1)
      HIST(D1)=HIST(D1)-1
      HIST(D2)=HIST(D2)+1
      IF(D1.GT.D2)GO TO 60
      IF(D1.GT.K)GO TO 100
      IF(D2.LE.K)GO TO 100
C          D1.LE.K.LT.D2
      NFREQ=NFREQ-1
      IF(NFREQ.GT.N)GO TO 100

   50 K=K+1
      NFREQ=NFREQ+HIST(K)
      IF(NFREQ.LE.N)GO TO 50
      GOTO 100

   60 IF(D2.GT.K)GO TO 100
      IF(D1.LT.K)GO TO 100
C          D2.LE.K.LE.D1
      IF(K.LT.D1)NFREQ=NFREQ+1
      J=K+1
      NF=NFREQ

   64 NFREQ=NF

   65 J=J-1
      NF=NFREQ-HIST(J)
      IF(NF.EQ.NFREQ)GOTO 65
      IF(NF.GT.N)GOTO 64
      K=J

  100 CONTINUE

      RETURN
      END

c***********************************************************************
      SUBROUTINE PARPRO(PARMS,RPARM,DNTH,ACTTH,DIST,PERC,SIGM,TOL,IPRINT
     *,SIGA,ISL,Z,ISS,INL,INS,NL,NS,MEDI,NSWH,IBELOW,ANGL,SMAA,SMIA,
     *FOCA,SCAL,REQU,RPOL,FAR ,RLATI,RLONGI,IGEOM,IHALF,SIZE,SUNA,PUTN,
     *ASTRE,LINC,IAUTO,ICLUW,ICLUN,IOFF,MAXNUM,LCIRCL,RADRNG,FRACT,
     *FDS,SERNO,CENL,CENS,UPDATE,SUNLAT,SUNLON,PLANET,old,
     *pc_error,pc_line,pc_samp)
      LOGICAL XVPTST
      REAL*4 RPARM(*),pdata(20)
      INTEGER*4 PARMS(*),DNTH,ACTTH,DIST,Z,FDS,SERNO,UPDATE,old
      CHARACTER*12 planet,cbuf
      INTEGER MSGFLAG		      !ONLY PRINT MSG ONCE

      DATA MSGFLAG/0/
      CALL XVPARM('ACTIVITY',ACTTH,ICNT,IDEF,1)

      CALL XVPARM('ANGLE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)ANGL=RPARM(1)
      CALL XVPARM('NORANGLE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)ANGL=RPARM(1)

      CALL XVPARM('RANGE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)pc_error=RPARM(1)/2.0

      CALL XVPARM('INITCEN',RPARM,ICNT,IDEF,2)
      IF(ICNT.GT.0)then
         pc_line=RPARM(1)
         pc_samp=rparm(2)
      endif

      CALL XVPARM('AREA',PARMS,ICNT,IDEF,4)
      IF(ICNT.GT.0)THEN
	ISL=PARMS(1)
	Z=ISL-((ISL-1)/3)*3
	IF(Z.EQ.2)ISL=ISL+2
	IF(Z.EQ.3)ISL=ISL+1
	ISS=PARMS(2)
	INL=PARMS(3)
	INS=PARMS(4)
	IF(INL+ISL.GT.NL)INL=NL-ISL
	IF(INS+ISS.GT.NS)INS=NS-ISS
      ENDIF

      IF(XVPTST('AUTO'))IAUTO=1

      CALL XVPARM('BELOW',PARMS,ICNT,IDEF,1)
      IF(ICNT.GT.0)IBELOW=PARMS(1)

      CALL XVPARM('CENTER',RPARM,ICNT,IDEF,2)
      IF(ICNT.GT.0)THEN
	CENL=RPARM(1)
	CENS=RPARM(2)
      ENDIF

      CALL XVPARM('CLUSTER',PARMS,ICNT,IDEF,2)
      ICLUW=PARMS(1)
      ICLUN=PARMS(2)

      CALL XVPARM('DISTANCE',DIST,ICNT,IDEF,1)

      CALL XVPARM('DNTHRESH',DNTH,ICNT,IDEF,1)

      CALL XVPARM('FAR',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        FAR=RPARM(1)
      endif
      CALL XVPARM('RMAG',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        FAR=RPARM(1)
      endif

      CALL XVPARM('FDS',PARMS,ICNT,IDEF,1)
      IF(ICNT.GT.0)FDS=PARMS(1)

      CALL XVPARM('FOCAL',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)FOCA=RPARM(1)
      CALL XVPARM('FOCL',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)FOCA=RPARM(1)

C  'FORMAT':
      IF(XVPTST('BYTE'))IHALF=1
      IF(XVPTST('HALF'))IHALF=0

      CALL XVPARM( 'FRACTION', FRACT, ICNT, IDEF, 1)

      CALL XVPARM('GEOM',IGEOM,ICNT,IDEF,1)

      CALL XVPARM('HEIGHT',PERC,ICNT,IDEF,1)

      CALL XVPARM('LATITUDE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        RLATI=RPARM(1)
      endif
      CALL XVPARM('SLATITUD',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        RLATI=RPARM(1)
      endif

      IF(XVPTST('LCIRCLE'))LCIRCL=1

      CALL XVPARM('LINC',LINC,ICNT,IDEF,1)

      CALL XVPARM('LONGITUD',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        RLONGI=RPARM(1)
      endif
      CALL XVPARM('MAXNUM',MAXNUM,ICNT,IDEF,1)

      CALL XVPARM('MEDIAN',PARMS,ICNT,IDEF,1)
      IF(ICNT.GT.0)THEN
         MEDI=PARMS(1)
         IF(MEDI.LT.3.OR.MEDI.GT.NS)MEDI=101
         IF((MEDI/2)*2.EQ.MEDI)MEDI=MEDI+1
         NSWH=MEDI/2+1
      ENDIF

      CALL XVPARM('OFFSET',PARMS,ICNT,IDEF,1)
      IF(ICNT.GT.0)IOFF=PARMS(1)

      if( xvptst('OLD')) old=1

      CALL XVPARM('PERCENT',ASTRE,ICNT,IDEF,1)

      CALL XVPARM('PLANET',CBUF,ICNT,IDEF,1)
      IF(ICNT .GT. 0)then
        PLANET=CBUF
        call getplacon(planet,pid,pdata,ind)
        if(ind.ne.0)  call MABEND('GETPLACON: bad indicator')
        requ=(pdata(1)+pdata(2))/2.0
        rpol=pdata(3)
      endif

      IF(XVPTST('PRINT'))IPRINT=1

      CALL XVPARM('PUTNORTH',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        PUTN=RPARM(1)
      endif
      CALL XVPARM('RADIUS',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)THEN
          RPOL=RPARM(1)
          REQU=RPOL
      ENDIF

      CALL XVPARM('REQUATOR',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)THEN
          REQU=rparm(1)
      ENDIF
      CALL XVPARM('RPOLE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
          RPOL=RPARM(1)
      endif

      CALL XVPARM('RESCALE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)  SIZE=RPARM(1)

      CALL XVPARM('RING',RADRNG,ICNT,IDEF,1)

      CALL XVPARM('SIGACT',SIGA,ICNT,IDEF,1)

      CALL XVPARM('SIGMA',SIGM,ICNT,IDEF,1)

      CALL XVPARM('SMAA',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)SMAA=RPARM(1)

      CALL XVPARM('SMIA',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)SMIA=RPARM(1)

      CALL XVPARM('SCALE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)SCAL=RPARM(1)
      CALL XVPARM('PSCALE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)SCAL=RPARM(1)

      CALL XVPARM('SERNO',PARMS,ICNT,IDEF,1)
      IF(ICNT.GT.0)SERNO=PARMS(1)

      CALL XVPARM('SOLAR',RPARM,ICNT,IDEF,2)
      IF(ICNT.GT.0)THEN
	SUNLAT=RPARM(1)
	SUNLON=RPARM(2)
      ENDIF

      CALL XVPARM('SUNANGLE',RPARM,ICNT,IDEF,1)
      IF(ICNT.GT.0)then
        SUNA=RPARM(1)
      endif

      CALL XVPARM('TOLERANC',TOL,ICNT,IDEF,1)

      IF(XVPTST('UPDATE'))THEN
	UPDATE=1
	IF(MSGFLAG.EQ.0)THEN
	  CALL XVMESSAGE('  ** IMAGE CATALOG WILL BE UPDATED',' ')
	  CALL XVMESSAGE
     *         ('  ** CAUTION:  USE "SELCAT S" OR CHECK DATA!',' ')
	  MSGFLAG=1
	ENDIF
      ENDIF

      RETURN

      END

c***********************************************************************
      SUBROUTINE POINTD(IP,C,NLX,GREY,LOG,NE,WHITE,NSX,NLRD)
      INTEGER*4 INP(2),OUT(3),lcb(4)
      REAL*4 C(2800,5)
      BYTE GREY,WHITE
      BYTE LOG(*)
      CHARACTER*72 lab(2)
      COMMON/FARENC_FILES/INP,OUT

      call xvclose(ip,istat,' ')
      CALL XVOPEN(IP,ISTAT,'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     * 'U_NL',NLX,'U_NS',NSX,'U_FORMAT','BYTE','O_FORMAT','BYTE',' ')
      lab(1)='FARENC POINTS DISPLAY FILE'
      lab(2)=' '
	LCB(1)=2
	LCB(2)=NLX
	LCB(3)=NSX
	LCB(4)=1
      CALL XLADD(IP,'HISTORY','FARENC','FARENC POINTS DISPLAY FILE',
     * ISTAT,'FORMAT','STRING',' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLADD IN POINTS,STAT=',1,ISTAT)
      CALL XLDEL(IP,'SYSTEM','NL',ISTAT,' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLDEL POINTS FILE,STAT=',1,ISTAT)
      CALL XLDEL(IP,'SYSTEM','NS',ISTAT,' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLDEL POINTS FILE,STAT=',1,ISTAT)
      CALL XLDEL(IP,'SYSTEM','FORMAT',ISTAT,' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLDEL POINTS FILE,STAT=',1,ISTAT)

      CALL XLADD(IP,'SYSTEM','NL',NLX,ISTAT,'FORMAT','INT',' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLADD POINTS FILE,STAT=',1,ISTAT)
      CALL XLADD(IP,'SYSTEM','NS',NSX,ISTAT,'FORMAT','INT',' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLADD POINTS FILE,STAT=',1,ISTAT)
      CALL XLADD(IP,'SYSTEM','FORMAT','BYTE',ISTAT,
     &           'FORMAT','STRING',' ')
      CALL CHKSTAT(ISTAT,' ERR IN XLADD POINTS FILE,STAT=',1,ISTAT)
      LINE=C(1,1)
      K=1
      NONE=1
      DO 350 L=1,NLX
         IF(NONE.GT.0) then
            do ii=1,NSX
               LOG(ii) = GREY
            enddo
         endif
         NONE=0
         IF(K.GT.NE)GO TO 351
352      IF(LINE.GT.L)GO TO 351
         NONE=NONE+1
         I=C(K,2)
         IF(I.GE.1.AND.I.LE.NSX)GO TO 353
         NONE=NONE-1
         GO TO 354
353      CONTINUE
         LOG(I)=WHITE
354      CONTINUE
         K=K+1
         IF(K.GT.NE)GO TO 351
         LINE=C(K,1)
         GO TO 352
351      CONTINUE
         CALL XVWRIT(IP,LOG,ISTAT,' ')
350   CONTINUE

      CALL XVCLOSE(IP,ISTAT,' ')
      RETURN
      END
      
c ****************************************************************
      SUBROUTINE POINTU(IP,C,NSX,LOG,NE,BLACK,NLRD,NLX)
      INTEGER*4 INP(2),OUT(3)
      REAL*4 C(2800,5)
      BYTE LOG(7200)
      BYTE BLACK
      COMMON/FARENC_FILES/INP,OUT

      CALL XVOPEN(IP,ISTAT,'OP','UPDATE','OPEN_ACT','SA',
     &            'IO_ACT','SA',' ')
      DO 10 L=1,NE
         LAST=C(L,1)
         IF(LAST.LT.1.OR.LAST.GT.NLX)GO TO 10
         GO TO 11
10    CONTINUE
      RETURN

11    CALL XVREAD(IP,LOG,ISTAT,'LINE',LAST,' ')
      DO 381 L=1,NE
         LINE=C(L,1)
         ISAM=C(L,2)
         IF(LINE.LT.1.OR.LINE.GT.NLX)GO TO 381
         IF(ISAM.LT.1.OR.ISAM.GT.NSX)GO TO 381
         IF(LINE.NE.LAST)GO TO 382
383      LOG(ISAM)=BLACK
         GO TO 381
382      CALL XVWRIT(IP,LOG,ISTAT,'LINE',LAST,' ')
         LAST=LINE
         CALL XVREAD(IP,LOG,ISTAT,'LINE',LINE,' ')
      GO TO 383
381   CONTINUE
      CALL XVCLOSE(IP,ISTAT,' ')
      RETURN
      END
      
c ****************************************************************
      SUBROUTINE PROJEL(A1,B1,D1,RLAT,IND,F,SCAL,SEMAA,SEMIA,RNA,DELL,
     *  DELS)
C  A1=EQUATORIAL RADIUS (KM)
C  B1=POLAR RADIUS (KM)
C  D1=DISTANCE S.C. FROM PLANET CENTER
C  RLAT =LATITUDE SUB  SPACECRAFT POINT  (DEGREES)
C  IND=0 NORMAL RETURN
C  F=FOCAL LENGTH (MM)
C  SCAL=SCALE IN IMAGE PLANE (PIXEL/MM)
C  SEMAA=APPARENT ELLIPSE SEMIMAJOR AXIS (PIXELS)      RETURNED
C  SEMIA=APPARENT ELLIPSE SEMIMINOR AXIS (PIXELS)      RETURN
C  RNA=NORTH ANGLE (DEGREES CLOCKWISE OF SPIN AXIS NORTH)
C  DELL=ADDITIVE LINE OFFSET TO ELLIPSE CENTER TO GIVE PLANET CENTER
C  DELS=ADDITIVE SAMP OFFSET TO ELLIPSE CENTER TO GIVE PLANET CENTER
C  LAST TWO VALUES ARE RETURNED ALSO
      INTEGER*4 INP(2),OUT(3)
      REAL*8 SI,CO,A,B,D,PI,C,SL1,SL2
      REAL*4 SEMAA,SEMIA
      COMMON/FARENC_FILES/INP,OUT

      PI=acos(-1.0)

C  CHECK IF PROJECTED ELLIPSE HAS ALREADY BEEN SPECIFIED
      IF(SEMAA.NE.-1..AND.SEMIA.NE.-1.)RETURN

C  CHECK IF DATA IS AVAILABLE FOR SPHERICAL PLANET
      IF(F.EQ.-400.)RETURN
      IF(SCAL.EQ.-400.)RETURN
      IF(A1  .EQ.-400.)RETURN
      IF(B1  .EQ.-400.)RETURN
      IF(D1  .EQ.-400.)RETURN
      IF(A1.NE.B1)GO TO 20

C.....check for spacecraft in interior of planet to avoid sqrt of negative.
       IF (A1 .GT. D1)    call mabend(
     .     'ERROR in routine PROJEL: Please check input parameters')

      SEMAA=SCAL*F*A1/SQRT(D1*D1-A1*A1)
      SEMIA=SEMAA
      CALL PRNT(7,1,SEMAA,'PROJECTED PLANET RADIUS (pxls) =')
      RETURN
20    CONTINUE

C  CHECK IF DATA IS AVAILABLE FOR OBLATE PLANET
      IF(RLAT.EQ.-400.)RETURN
      IF(RNA.EQ.400.)RETURN
      IND=0
      A=A1
      B=B1
      D=D1
      CO=DCOS(RLAT*PI/180.D0)
      SI=DSIN(RLAT*PI/180.D0)
      CO=DABS(CO)
      SI=DABS(SI)
C  SOLVE FOR SLOPES OF TWO LINES IN PLANE OF SPIN AXIS
      C=D*D*CO*CO*B*B+A*A*D*D*SI*SI-A*A*B*B
      IF(C.LT.0.D0)CALL XVMESSAGE('S.C. INSIDE PLANET',' ')
      IF(C.LT.0.D0)GO TO 999
      C=DSQRT(C)
      SL1=(D*CO*D*SI+C)/(D*D*CO*CO-A*A)
      SL2=(D*CO*D*SI-C)/(D*D*CO*CO-A*A)
      IF(ABS(RLAT).NE.90.)GO TO 10
      ALPHA1=DABS(DATAN((SL1-SL2)/(1.D0+SL1*SL2))/2.)
      ALPHA2=ALPHA1
      GO TO 12
10    C=DTAN(DABS(RLAT*PI/180.D0))
C  SOLVE FOR ANGLES BETWEEN LINES AND PLANET CENTER AS SEEN FROM S.C.
C  IN THIS CASE THEY ARE NOT EQUAL
      ALPHA2=DABS(DATAN((C-SL2)/(1.D0+SL2*C)))
      ALPHA1=DABS(DATAN((C-SL1)/(1.D0+SL1*C)))
      IF(ALPHA1.LT.ALPHA2)GO TO 12
      C=ALPHA1
      ALPHA1=ALPHA2
      ALPHA2=C
12    CONTINUE
C  SOLVE FOR SEMIMINOR AXIS IN PIXELS
      SEMIA=ABS(F*SCAL*TAN((ALPHA1+ALPHA2)/2.))
C  RECOMPUTE B FOR CASE OF MAJOR AXIS
      B=DSQRT((A*A*B*B)/(A*A*SI*SI+B*B*CO*CO))
C  SOLVE FOR SLOPES OF LINES PERPENDICULAR TO SPIN AXIS (APPROXIMATE)
      C=D*D-B*B
      IF(C.LT.0.D0)CALL XVMESSAGE('S.C. INSIDE PLANET',' ')
      IF(C.LT.0.D0)GO TO 999
      SL1=A/DSQRT(C)
      DELTA=2.*DABS(DATAN(SL1))
C  SOLVE FOR SEMIMAJOR AXIS IN PIXELS
      SEMAA=ABS(F*SCAL*TAN(DELTA/2.))
      CALL PRNT(7,1,SEMAA,'Projected major axis (pxls)=')
      CALL PRNT(7,1,SEMIA,'Projected minor axis (pxls)=')
C  SOLVE FOR ADDITIVE CORRECTION TO ELLIPSE CENTER TO GET TRUE
C   CENTER OF OBLATE SPHEROID
      DEL=(SEMIA)*((ALPHA2-ALPHA1)/(ALPHA2+ALPHA1))
      DELL=-DEL*DCOS(RNA*PI/180.D0)
      DELS=DEL*DSIN(RNA*PI/180.D0)
      IF(RLAT.LT.0.0)DELL=-DELL
      IF(RLAT.LT.0.0)DELS=-DELS
      call XVMESSAGE
     &         ('Computed offset of planet from ellipse center',' ')
      CALL PRNT(7,1,DELL,'LINE OFFSET')
      CALL PRNT(7,1,DELS,'SAMPLE OFFSET')
      RETURN
999   RNA=400.
      RETURN
      END
            
c ****************************************************************
      SUBROUTINE RING(RR,DD,RLAT,F,SCAL,SEMAA,SEMIA,RNA,DELL,DELS)
C R=RING RADIUS (KM)
C D=DIST OF S.C. FROM PLANET CENTER
C RLAT=LAT OF SUB S.C. POINT (DEG)
C F=FOCAL LENGTH (MM)
C SCAL=SCALE IN IMAGE PLANE (PIXELS/MM)
C SEMAA,SEMIA=ELLIPSE SEMI MAJOR,MINOR AXES (PIXELS)  RETURNED
C RNA=NORTH ANGL DEG CLOCKWISE FROM UP OF PROJECTED SPIN AXIS
C DELL,DELS=ADDITIVE LINE,SAMP CORRECTION FROM ELLIPSE CENTER TO
C  PLANET CENTER        -  RETURNED

      INTEGER*4 INP(2),OUT(3)
      REAL*8 PI,R,D,S1SQ,S2SQ,S1,S2,THETA,THETA1,GAMMA,THETA3,XP,Z,S
      COMMON/FARENC_FILES/INP,OUT

      PI=acos(-1.0)
      IF(SEMAA.NE.-1..AND.SEMIA.NE.-1.)RETURN
      IF(RR.EQ.0.0)GO TO 100
      IF(F.EQ.-400.)GO TO 100
      IF(SCAL.EQ.-400.)GO TO 100
      IF(DD.EQ.-400.)GO TO 100
      IF(RLAT.EQ.-400.)GO TO 100
      IF(RNA.EQ.400.)GO TO 100
      CALL XVMESSAGE('FARENC FIT TO SATURN RINGS',' ')

      R=RR
      D=DD
      S1SQ=R*R+D*D-2.*R*D*DABS(DCOS(RLAT*PI/180.))
      S2SQ=R*R+D*D-2.*R*D*(DCOS(PI-DABS(RLAT*PI/180.)))
      S1=DSQRT(S1SQ)
      S2=DSQRT(S2SQ)
      THETA=DABS(dAcos((S1SQ+S2SQ-(2.*R)**2)/(2.*S1*S2)))
      THETA1=DABS(dAcos((S1SQ+D*D-R*R)/(2.*S1*D)))
      GAMMA=DABS(THETA/2.-(THETA-THETA1))
      THETA3=PI-THETA1-DABS(RLAT*PI/180.)
      XP=DABS(S1*DSIN(THETA/2.)/DSIN(PI-THETA/2.-THETA3))
      Z=DABS(XP*DSIN(THETA3)/DSIN(THETA/2.))
      S=DSQRT(R*R-(R-XP)**2)
      DEL=F*SCAL*DABS(DTAN(GAMMA))
      SEMMA=F*SCAL*S/Z
      SEMIA=F*SCAL*DABS(DTAN(THETA/2.))
      CALL PRNT(7,1,SEMMA,'SEMMA')
      CALL PRNT(7,1,SEMIA,'SEMIA')

      DELL=-DEL*DCOS(RNA*PI/180.)
      DELS=DEL*DSIN(RNA*PI/180.)
      IF(RLAT.LT.0.0)DELL=-DELL
      IF(RLAT.LT.0.0)DELS=-DELS
      CALL PRNT(7,1,DELL,'LINE OFFSET')
      CALL PRNT(7,1,DELS,'SAMP OFFSET')
      RETURN
100   RR=0.0
      RETURN
      END

c ****************************************************************
      SUBROUTINE SUNPOS(SUNA,ANGL,RLATI,RLONGI,SUNLAT,SUNLON)
      INTEGER*4 INP(2),OUT(3)
      COMMON/FARENC_FILES/INP,OUT
C  COMPUTES SUN ILLUMINATION ANGLE

      IF(SUNA.NE.-400.)RETURN
      IF(RLONGI.EQ.-400.)THEN
         CALL XVMESSAGE('NO TERMINATOR REJECTION APPLIED',' ')
         RETURN
      ENDIF

      CONV=3.14159/180.
      A=(90.-RLATI)*CONV
      B=(90.-SUNLAT)*CONV
      IF(SUNLAT.EQ.90.)B=CONV
      IF(SUNLAT.EQ.-90.)B=179.*CONV
      C=(SUNLON-RLONGI)*CONV
      D=SIN(C)
C       3/21/83 -JAM-SUBSTITUTED 1/TAN FOR COTAN
      E=SIN(A)/TAN(B)-COS(A)*COS(C)
      IF(D.EQ.0..AND.E.EQ.0.)THEN
         CALL XVMESSAGE('NO TERMINATOR REJECTION APPLIED',' ')
         RETURN
      ENDIF
      R=ATAN2(D,E)
      SUNA=ANGL-R/CONV
      IF(SUNA.LT.0.)SUNA=SUNA+360.
      IF(SUNA.GE.360.)SUNA=SUNA-360.
      CALL PRNT(7,1,SUNA,'Projected sun angle (deg)=')
      RETURN
      END
      
c ****************************************************************
      subroutine writecurve(out,log,imgtype,nph,npv,ig,dn,
     *   inline,insamp,nl,ns,project,serno)
      implicit integer(a-z)
      integer*2 log(*),dn
      real*4 ig(2700) ! geoma distortion correct parameters
      character*5 project
      integer serno
      real*4 is_line,is_samp,os_line,os_samp
      
      line=inline
      samp=insamp
      if(imgtype.eq.7)then ! convert to image space
c          rline=line
c          rsamp=samp
c          call tritra(ind,ig,nph,npv,rline,rsamp,0)
c          if(ind.ne.0)call mabend(' tritra error')
c          line=rline
c          samp=rsamp
           os_line=inline
           os_samp=insamp
           call convisos(project,serno,is_line,is_samp,os_line,
     +                   os_samp,0,ig,nph,npv,ind)
           if(ind.ne.0)call mabend('convisos error')
           line=nint(is_line)
           samp=nint(is_samp)
      endif
      if(line.ge.1.and.line.le.nl.and.samp.ge.1.and.samp.le.ns)then
          call xvread(out,log,istat,'LINE',line,' ')
          log(samp)=dn
          call xvwrit(out,log,istat,'LINE',line,' ')
      endif
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create farenc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM farenc

   To Create the build file give the command:

		$ vimake farenc			(VMS)
   or
		% vimake farenc			(Unix)


************************************************************************/


#define PROGRAM	farenc
#define R2LIB

#define MODULE_LIST farenc.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST mp_for_defs
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create farenc.pdf
process help=*
PARM INP       TYPE=STRING  COUNT=(1:2)
PARM OUT       TYPE=STRING  COUNT=(0:3)                       DEFAULT=--
PARM SIZE      TYPE=INTEGER COUNT=4                           DEFAULT=(1,1,0,0)
PARM SL        TYPE=INTEGER                                   DEFAULT=1
PARM SS        TYPE=INTEGER                                   DEFAULT=1
PARM NL        TYPE=INTEGER                                   DEFAULT=0
PARM NS        TYPE=INTEGER                                   DEFAULT=0
PARM PLANET    TYPE=(STRING,12) COUNT=0:1 DEFAULT=--
PARM RANGE     TYPE=REAL    COUNT=(0:1) VALID=(0:999999999)   DEFAULT=--
PARM INITCEN   TYPE=REAL    COUNT=(0,2) VALID=(1:32767)       DEFAULT=--
PARM BEGIN     TYPE=REAL COUNT=0:1 VALID=(10.:100.)           DEFAULT=80.
PARM RESCALE   TYPE=REAL    COUNT=0:1                         DEFAULT=--
PARM FORMAT    TYPE=KEYWORD COUNT=0:1 VALID=(BYTE,HALF)       DEFAULT=--
PARM DNTHRESH  TYPE=INTEGER COUNT=0:1                         DEFAULT=--
PARM ACTIVITY  TYPE=INTEGER                                   DEFAULT=55
PARM DISTANCE  TYPE=INTEGER                                   DEFAULT=10
PARM HEIGHT    TYPE=REAL                                      DEFAULT=.6
PARM BELOW     TYPE=INTEGER COUNT=0:1                         DEFAULT=--
PARM AUTO      TYPE=KEYWORD COUNT=0:1 VALID=AUTO              DEFAULT=--
PARM FRACTION  TYPE=REAL                                      DEFAULT=0.0
PARM OFFSET    TYPE=INTEGER COUNT=0:1                         DEFAULT=--
PARM PERCENT   TYPE=REAL                                      DEFAULT=.2
PARM LINC      TYPE=INTEGER                                   DEFAULT=5
PARM CLUSTER   TYPE=INTEGER COUNT=2                           DEFAULT=(30,24)
PARM AREA      TYPE=INTEGER COUNT=(0,4)                       DEFAULT=--
PARM MEDIAN    TYPE=INTEGER COUNT=0:1                         DEFAULT=--
PARM LCIRCLE   TYPE=KEYWORD COUNT=0:1 VALID=LCIRCLE           DEFAULT=--
PARM SIGACT    TYPE=REAL                                      DEFAULT=2.5
PARM SIGMA     TYPE=REAL              VALID=(.01:9999)        DEFAULT=1.7
PARM TOLERANC  TYPE=REAL                                      DEFAULT=1.0
PARM SUNANGLE  TYPE=REAL    COUNT=0:1 VALID=(-360:360)        DEFAULT=--
PARM PRINT     TYPE=KEYWORD COUNT=0:1 VALID=PRINT             DEFAULT=--
PARM MAXNUM    TYPE=INTEGER                                   DEFAULT=10
PARM FDS       TYPE=INTEGER COUNT=0:1                         DEFAULT=--
PARM SERNO     TYPE=INTEGER COUNT=0:1                         DEFAULT=--
PARM CENTER    TYPE=REAL    COUNT=(0,2)                       DEFAULT=--
PARM UPDATE    TYPE=KEYWORD COUNT=0:1 VALID=UPDATE            DEFAULT=--
PARM NOSEDR    TYPE=KEYWORD COUNT=0:1 VALID=NOSEDR            DEFAULT=--
PARM NORANGLE  TYPE=REAL    COUNT=0:1 VALID=(-360:360)                         DEFAULT=--
PARM ANGLE     TYPE=REAL    COUNT=0:1 VALID=(-360:360)                         DEFAULT=--
PARM SMAA      TYPE=REAL    COUNT=0:1 VALID=(1:999999)                         DEFAULT=--
PARM SMIA      TYPE=REAL    COUNT=0:1 VALID=(1:999999)                         DEFAULT=--
PARM FOCL      TYPE=REAL    COUNT=0:1 VALID=(.0001:999999)                         DEFAULT=--
PARM FOCAL     TYPE=REAL    COUNT=0:1 VALID=(.0001:999999)                         DEFAULT=--
PARM PUTNORTH  TYPE=REAL    COUNT=0:1 VALID=(-360:360 )                         DEFAULT=--
PARM SCALE     TYPE=REAL    COUNT=0:1 VALID=(.0001:9999)                         DEFAULT=--
PARM PSCALE    TYPE=REAL    COUNT=0:1 VALID=(.0001:9999)                         DEFAULT=--
PARM RADIUS    TYPE=REAL    COUNT=0:1 VALID=(.01:750000)                         DEFAULT=--
PARM REQUATOR  TYPE=REAL    COUNT=0:1 VALID=(.01:750000)                         DEFAULT=--
PARM RPOLE     TYPE=REAL    COUNT=0:1 VALID=(.01:750000)                         DEFAULT=--
PARM FAR       TYPE=REAL    COUNT=0:1 VALID=(.1:9999999999)                         DEFAULT=--
PARM RMAG      TYPE=REAL    COUNT=0:1 VALID=(.1:9999999999)                         DEFAULT=--
PARM SLATITUD  TYPE=REAL    COUNT=0:1 VALID=(-90:90)          DEFAULT=--
PARM LATITUDE  TYPE=REAL    COUNT=0:1 VALID=(-90:90)          DEFAULT=--
PARM LONGITUD  TYPE=REAL    COUNT=0:1 VALID=(-360:360)        DEFAULT=--
PARM SOLAR     TYPE=REAL    COUNT=(0,2) VALID=(-360:360 )     DEFAULT=--
PARM RING      TYPE=REAL                VALID=(0:600000)      DEFAULT=0.0
!!!PARM SEDRSRC   TYPE=(STRING,4) COUNT=0:1 +  !replaced by CKNAME
!!!VALID=("FARE","NEAR","NAV ","NAV2","DAVI","SEDR","NAIF")  DEFAULT="DAVI"
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=0:1			DEFAULT=--
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
LOCAL (DUM1,DUM2) REAL
PARM PCL       TYPE=NAME    DEFAULT=DUM1
PARM PCS       TYPE=NAME    DEFAULT=DUM2
END-PROC
.TITLE
  "farenc"
.HELP
 PURPOSE:
 "farenc" is a Vicar program to determine the center of a target body
 presenting part of its limb in a picture.  The orientation
 and lighting geometry of the target body can be made constraints.
 The program has 4 main modes. These are:
 
 1. Determining constraints from the SEDR or from parameters.
    Constraints determine the shape,size,and orientation of the limb.
 2. Locating candidate limb points.
 3. Fitting these points to a conic.
 4. Generating GEOM parameters, 
    Updating the SPICE/SEDR OM matrix, and
    Writing the planet center to a file (if the input is a perspective
    projection with a map3 label)

 Three limb fitting functions are available:

              1. An unconstrained circle
              2. A constrained circle
              3. A constrained ellipse

.PAGE
LOCAL AND REMOTE SPICE ACCESS:

FARENC accesses SPICE data either locally or via the MIPS SPICE server.
Currently (Jan 2002) SPICE data is available for Cassini, GLL, and
VGR (at Jupiter only).  For images without SPICE data, parameters may be used
to specify the target geometry.

SPICEMODE specifies whether local or remote SPICE access is to be used.
If defaulted, SPICEMODE is set to the value of the logical name (or
environmental variable) DEFAULTSPICE.


PARAMETERS FOR RETRIEVING THE INITIAL CAMERA POINTING:

Initial camera pointing data is first retrieved from predict C kernels or
from MIPS C kernels.  The following optional parameters permit the user to
specify where this initial pointing is retrieved:

CKNAME and CKID are alternative ways to specify the C kernel from which camera
pointing is to be retrieved.  For example, CKNAME=FARE or CKID=M904 specifies
that the camera pointing should be retrieved from the file MIPS_FARENC.CK.
The CKID is the unique kernel ID assigned to each C kernel.  When CKID is
specified, it overrides the CKNAME parameter.  A complete list
of kernel IDs is located in the ASCII file assigned the logical name (or
environmental variable) KERNELDB.


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

Examples:  'LOCAL CKNAME=NAIF specifies that SPICE data be retrieved from
          local kernels using camera pointing from predicts or AACS telemetry.
          Improved camera pointing will later be stored in the local C kernel
          specific to this program.

           'REMOTE INSTITUTE=DLR USERID=TYR retrieves SPICE data remotely
          (from MIPS) via the SPICE server.  When improved camera pointing is
          stored (at MIPS), provenance data regarding the facility (DLR) and
          user (Thomas Roatsch) who created the data is included.

.PAGE

 "farenc" determines limb points by computing a value called
 activity.  Activity is defined as the sum of the absolute
 values of the differences between the DN's of opposite
 sides of a 3 by 3 pixel box, i.e.:

          --- --- ---
         | 1 | 2 | 3 |
          --- --- ---
         | 4 | 5 | 6 |    |DN1 - DN9| + |DN3 - DN7| = Activity
          --- --- ---
         | 7 | 8 | 9 |
          --- --- ---
.PAGE
 Points having the highest activity lie along the limb of a
 target body.  If ACTI is the activity threshold and DNTH is the DN
 threshold ( As applied to DN in the above figure) then the
 tests required for a point to be accepted as a limb candidate are:

       1. DN5 >= DNTH
       2. |DN2 - DN8| + |DN4 - DN6| >= ACTI
       3. |DN1 - DN9| + |DN3 - DN7| >= ACTI
       4. MIN (DN1,DN2,DN3,DN4,DN6,DN7,DN8,DN9) <= BELOW
       5. The direction of the planet center obtained from:
         atan2(dn1+dn2+dn3-dn7-dn8-dn9,dn3+dn6+dn9-dn1-dn4-dn7)
          must be within 90 degrees of the sun illumination 
          direction.
.PAGE
 For each line and column, two limb candidate points are chosen.
 These points must be the two largest activity values in that
 dimension which:

       1. are more than or equal to DIST pixels apart.
       2. form a ratio of smallest/largest activity at least
           equal to HEIGHT.

 Points are then removed which fail to have sufficient numbers
 of neighbors, implying that they are isolated events.

.PAGE
 Limb fitting is performed in two stages. First an initial fit is
 made using simulated annealing. Then a least squares fit is performed
 which permits iterative point rejection until all the points
 lie within TOLERANCE.
 
 1. Simulated annealing. This is a complex method which treats
    the problem like a thermodynamic cooling system and whose
    object is to cool the system into the lowest energy state.
    The lowest state corresponds to the minimum residual fit
    error for the limb points.

 2. Iterative least squares fit.
    Each cycle rejects values which depart from the
    mean error by more than +/-SIGMA * THETA, until all values fall
    within computed radius +/-TOLE.

.PAGE
 If an oblate spheroid is imaged from a distance less than about
 20 diameters and if the oblateness is greater than about 5%,
 the center of the target body may not coincide acceptably with the
 center of the projected ellipse.  An approximate solution to
 the problem ( assuming the projected shape is an ellipse )
 which provides an additive offset to the computed ellipse
 center to give the true target body center is provided by "farenc".
 This correction is not made to the ellipse center, but is
 only printed along with the ellipse center.  A target body whose
 oblateness is 10%, viewed from two diameters at a subspace-
 craft latitude of +/-45 degrees will suffer a target body center
 discrepancy error from that of the ellipse center of about
 1% of the ellipse major axis.

 Unless data specific to the constrained ellipse or circle
 has been provided, "farenc" will compute only the general
 circle.  Unless 'NOSEDR is specified,
 data for the constrained fit will be obtained from the SEDR
 or SPICE.  User specified parameters will over-
 ride the equivalent SEDR values.

 If a reseau or GEOMA data has been provided, the ellipse
 and circle centers will be computed in object space by
 first transforming all of the limb points to object space.
.page
 The principal output file is a dataset containing GEOMA parameters.
 (This file must always be the LAST output.)  This data set can be
 input to GEOMA to geometrically correct and translate the target body
 such that the center of the target body will lie at NL/2.0, NS/2.0.
 This data set could also be input to other programs as a
 geometric correction data set.  These other programs such
 as "photfunc", "photcat", "photcal" and "map2" must have "farenc"
 modes for determining the OM matrix (the camera to picture
 body rotation matrix).  The object space target body center
 will be NL/2.0,NS/2.0 by default.
.PAGE
EXECUTION:

 "farenc" may be executed using the following format:

    farenc INP=(IN,GEOM) OUT=(CURVE,POINTS,G) SIZE=(SL,SS,NL,NS) PARAMS

  where PARAMS represents parameters such as those that follow.

SUGGESTED EXECUTION FORMAT:
   farenc inp=(in,geom) out=g 'auto

Note: If the input image has a map3 perspective label placed there by
      the "perslab" program "farenc" will ignore the sedr/spice and
      will write the planet center to an output file called: FARENC.POS
      This file is a Vicar image, has 1 line & 2 samples with the
      real values line,sample. See test file for example using
      Space Telescope project imagery.

 The following parameters perform editing operations on the
 initial set of limb points in order to reject spurious points
 from the final least squares fit:

   	SIGACT  SIGMA  TOLERANCE   SUNANGLE  PRINT   MAXNUM

 For additional information type help followed by the parameter name.

.PAGE
 The following parameters control the nature of the geometric
 transformation parameters ouput in data set "G".

       GEOM              RESCALE

 For additional information type HELP followed by the parameter name.

.PAGE
 The following parameters allow the constrained ellipse to be
 used provided that either of the two sets of parameters which follow
 are completely specified.  In the first case the center of the
 projected ellipse will be computed but not the correction from
 ellipse center to oblate spheroid target body center ( they can disagree ).
 If SEDR data are available, the values for SET #II will be
 automatically computed. If data from the SEDR are not desired or available
 the 'NOSEDR keyword should be used. The default is to read the
 SEDR (Image Catalog).

 SET #I
    NORANGLE or ANGLE       SMAA              SMIA

 SET #II
          NOSEDR
    NORANGLE or ANGLE FOCL or FOCAL SCALE  RADIUS
    REQUATOR    RPOLE FAR   RMAG  SLAT   LATI  RING
    LATI  LONG    SOLAR  PLANET

 For additional information type HELP followed by the parameter name.
.page
 Program Operation Under Annealing Step.

 Each of the three functions (general circle, constrained circle &
 constrained ellipse) is allowed 6 attempts to bring N % of the
 limb points within 1 pixel of mean radial error. If all six
 attempts fail the smallest residual case is selected.
 The residual returned & printed will be either:
 1. The mean radial error if N % of points are within 1 pixel of 
    the fit.
 2. The mean error + (smaa/5)/ (# points selected within 50 
    pixels radial error)
 For each attempted fit N becomes: 80,70,60,50,40,and 30 %.   

 The annealing cost or objective function to be minimized is:
 (if n= total # pts, m=# points within smaa/5 radial error,
  k=# points within 3 pixels radial error, sumdr=sum radial
  residuals)

  if(k > N% of n)then cost=sumdr/k
  else if(m > 0)then cost=sumdr/m + (smaa/5)/m
  else cost=sumdr/n + 1

  The cost function is designed to punish for poor fits
  yet reward for including more points. It's a tradeoff.
.PAGE

 PRECISION: 
  Some differences in intermediate results such as shown below might
occur between successive runs of FARENC:
*****************
No  convergence,residual=  22.380320

Constrained annealing circle fit object space:
Line of center=  282.28452
Samp of center=  410.81024
Radial fit mean residual=  22.374784
******
No  convergence,residual=  22.374847

Constrained annealing circle fit object space:
Line of center=  282.29230
Samp of center=  410.80789
Radial fit mean residual=  22.374775
******************
  The differences above come from the first stage of the planet center
computation (with subroutine METROPOLIS).  This stage uses random numbers
in searching for the planet center location which best agrees with the points
found on the planet limb.  Because of the random numbers, the results of the
first stage vary slightly from run to run.
  The second stage of the planet center computation, the least squares
solution, refines the results of the first stage and produces line and sample
coordinates that agree well from run to run and machine to machine.  The
"Sigma of Radius Errors" and the "Largest Radius Error" often will vary
slightly for different machine types.
  In the test log for FARENC, there can be slight differences in output from
program GEOMA for different machine types.  Likewise, there are slight
differences in output for different machine types when FARENC is run on an
image written by program GEOMA.  This is due to round-off building up over
multiple processing steps.

Also, updates to the SPICE kernels may cause differences in the section showing
FINAL DATA FROM SEDR and PARAMETERS.

.page
PROGRAM HISTORY:

 WRITTEN BY: J. J. Lorre         December 1, 1976
 CONVERTED TO VAX BY:  Joel Mosher,  23 Aug. 1983
 COGNIZANT PROGRAMMER: J. J. Lorre
 REVISIONS:
  1/25/81 -JAM-UPDATE VGR SEDR WITH OM MATRIX AND RS VECTOR
  1/22/82 -JAM- REPLACE WATONA WITH MWATNA
  9/29/82 -JAM- PUT BACK IN MESSAGE FOR NUMBER OF POINTS USED
  10/6/82 -JAM- PUT IN CHANGE TO RESET 'OFFSET' WHEN AUTO IS USED
  10/9/82 -JAM- PUT IN NEW FIX FOR OFFSET USING # PIXELS ON PLANET
  10/15/82 -JAM- PUT IN CODE TO SAVE GEOMA PARAMETERS
  10/25/82 -JAM- REPLACE CALL TO SEDR79 WITH GETCDR
  10/31/82 -JAM- FIX CONVERSION BACK TO GEN. ELLIPSE IN IMAGE SPACE
  2/21/83 -JAM- REPLACE CALLS TO R4SORT WITH RBSORT
  8/17/83 -JAM- REPLACE CALLS TO RBSORT WITH R4SORT
  5/10/84 -CCA- DEL LABEL IO & SYS000 TO COMPUTE PLANET CENTER AND RADIUS
  11 Feb. 1985  - L.W.Kamp -  Get radii from PBDATA, fixed bug in IG
                array size, removed parm INTERACT, added parm FRACTION,
                cleaned up code.
  4/12/85 -JAM- CONVERT TO VICAR2
  5/9/85  -JAM- PUT IN 'NOSEDR KEYWORD
  5/12/85 -jam- make "nogeom" the default
  10/24/85 -CCA- FIXED OVERFLOW BUG IN LSQING, COMMENTED OUT NEW AUTO CODE
  86-2-11 -LWK- replaced hard-coded radii by call to PBDATA; also
                used PBDATA for TARGET parameter;
                fixed overflow of IG;  removed parm INTERACT;
                removed subs CURSOR, WATONA, PRNT2, RESO;
                removed DSRNSE, NOGEOM(always=1); changed COUNT of INP to 2;
                revised treatment of parameter defaults;
                restored JJL's AUTO code, controlled by new parm FRACTION.
 9/29/87 JJL 1. Introduced Metropolis annealing function.
             2. Introduced terminator rejection based on Sobel edge
                direction relative to the sun.
             3. Changed excess point truncation to alternate point selection.
 01 Oct 1987 -  J. J. Lorre - extensive modifications:
                 1. Selection of points only on solar illuminated limb
                    by computing the angle of the local edge.
                 2. Introduction of annealing code to avoid
                    instabilities in least squares routines when
                    bad data is present. 'OLD keyword.
 5/9/88  -FFM-  Incorporate 'SEDRSRC keyword
 10 Jun 1988 -  G.Yagi - fix determination of planet from ERT.
 10 Jun 1988 -  G.Yagi - fix test of ABLE77V2 indicator.
 1/10/90  JJL   Converted to project independent subroutines.
 2/1/93  jjl  Placed annealing and least squares methods in sequence.
 15 Aug 93 jjl  Accepts map3 perspective input label.
 15 APR 94 CCA  Add output to TCL variables PCL, PCS
 10 Jul 95  ams  (CRI) Made portable for UNIX
 11 Sep 95  gmy Changed GETSPICE call to use remote SPICE server (FR 85898)
 27 Mar 96  SP  Changed parameters and method for obtaining SPICE data according
                to new method specified by Gary Yagi using GETSPICE2/PUTSPICE2
                instead of GETSPICE/PUTSPICE.  Old parameter SEDRSRC is replaced
                by parameter CKNAME.       (Note that
                the ported version does not support a second input file.)
                Also added some VALID range checks in PDF for extreme value
                handling.
 12 jun 96  SP   Add code to distinguish between OBJECT SPACE and IMAGE SPACE.
                 (IMAGE space is raw image.  Object space is corrected for
                 camera geometric distortion.)  This corrects an error for GLL
                 where coordinates of limb points not converted to OBJECT
                 SPACE for calculations of planet center.
 22 Jul 96  SP   Upgraded to handle IBIS tiepoint files and other mission
                 besides GLL (allow second input file).  Put back in CCA's
                 option for outputting planet center to TCL variables.
                 JJL made a major fix to correct first stage (METROPOLIS)
                 computation of planet center.
 18 Aug 96 lwk  Fixed bugs in the case of Point Perspective input label;
		added support for NIMS labels (subsolar point)
 20 Dec 01 GMY  Add Cassini capability (CR 7953-MIPS).  Major changes to
                test script.
 20 Jul 05 lwk  Added another check in LSQP to prevent infinite loop when a matrix
                element becomes zero

.LEVEL1

.VARIABLE INP
 STRING- Input image;
 (optional:) GEOM file.

.VARIABLE OUT
 STRING _ Output files:
 (optional:) CURVE, POINTS, GEOMA

.VARIABLE SIZE
 4 INTEGERS - VICAR size field

.VARIABLE SL
 INTEGER - Starting line 

.VARIABLE SS
 INTEGER - Starting sample

.VARIABLE NL
 INTEGER - Number of lines

.VARIABLE NS
 INTEGER - Number of samples

.VARIABLE FORMAT
 KEYWORD - ('HALF or 'BYTE)
 Halfword or byte format

.VARIABLE NOSEDR
 KEYWORD - The SEDR is
   not to be read

.VARIABLE RANGE
Real- uncertainty in
SPICE planet center

.VARIABLE INITCEN
Real, 2 values:
initial line,samp
of planet center

.VARIABLE FDS
  Integer picture number.

.VARIABLE SERNO
  Integer
  camera serial number

.VARIABLE CENTER
 2 integers for line and
 sample of planet center 
 in output file.

.VARIABLE PUTNORTH
 real value of direction
 of north in output.

.VARIABLE DNTHRESH
 INTEGER - Lower pixel DN 
 THRESHOLD.

.VARIABLE ACTIVITY
 INTEGER - Lower threshold for
 ACTIVITY level.

.VARIABLE DISTANCE
 INTEGER - Distance between
 maximum activity value pixels.

.VARIABLE HEIGHT
 REAL - acceptable ratio of
 maximum activity values.

.VARIABLE BELOW
 INTEGER - Upper pixel value
 limit to be considered.

.VARIABLE AUTO
 KEYWORD  - ('AUTO)-
Sets ACTIVITY keyword
Sets DNTHRESH keyword
Sets BELOW keyword
 automatically.

.VARI FRACTION
 REAL - Fraction of target body
 in the image

.VARIABLE OFFSET
 INTEGER - Dark Current offset.

.VARIABLE PERCENT
 REAL - Bypass percent of top
 of histogram.

.VARIABLE LINC
 INTEGER - Interval between
 lines to be read for histogram.

.VARIABLE CLUSTER
 2 INTEGERS - Halfwidth of box,
 minimum number of limb points

.VARIABLE AREA
 4 INTEGERS - Field to be
 searched for limb SL,SS,NL,NS

.VARIABLE MEDIAN
 ODD INTEGER - Filter width

.VARIABLE LCIRCLE
 KEYWORD - ('LCIRCLE)-Chooses
 candidates from row and
 column extremes.

.VARIABLE SIGACT
 REAL - Maximum deviation from
 mean activity level

.VARIABLE SIGMA
 REAL - Upper limit of last
 least squares fit error.

.VARIABLE TOLERANC
 REAL - Tolerance for least
 square radius from limb.

.VARIABLE SUNANGLE
 REAL - Direction of sun in
 degrees.

.VARIABLE PRINT
 STRING - Prints line, sample
 and radial error.

.VARIABLE MAXNUM
 INTEGER - Maximum points on a
 line to be used for a fit.

.VARIABLE RESCALE
 REAL - Ratio of the diameter
 to smallest picture dimension

.VARIABLE UPDATE
 KEYWORD - Update SEDR

.VARIABLE NORANGLE
 REAL - Rotation angle of
 minor axis from north.

.VARIABLE ANGLE
 REAL - Rotation angle of
 minor axis from north.

.VARIABLE SMAA
 REAL - Semimajor axis of the
 ellipse in pixels.

.VARIABLE SMIA
 REAL - Semiminor axis of the
 ellipse in pixels.

.VARIABLE FOCL
 REAL - Camera focal length
 in millimeters.

.VARIABLE FOCAL
 REAL - Camera focal length
 in millimeters.

.VARIABLE SCALE
 REAL - Scale in pixels/mm.
 of the picture.

.VARIABLE PSCALE
 REAL - Number of pixels/mm.
 in the camera focal plane.

.VARIABLE RADIUS
 REAL - Radius of target body in
 kilometers.

.VARIABLE PLANET
 KEYWORD - Sets target body
 radii.

.VARIABLE BEGIN
Initial percentage
of points required
to fit function.

.VARIABLE REQUATOR
 REAL - target body equatorial
 radius in kilometers.

.VARIABLE RPOLE
 REAL - target body polar
 radius in kilometers.

.VARIABLE FAR
 REAL - Kilometers between
 the target body and spacecraft.

.VARIABLE RMAG
 REAL - Kilometers between the
 target body and spacecraft.

.VARIABLE SLATITUD
 REAL - Geocentric latitude of
 the subspacecraft point.

.VARIABLE LATITUDE
 REAL - Geocentric latitude of
 the subspacecraft point.

.VARIABLE LONGITUD
 REAL - Geocentric longitude of
 the subspacecraft point.

.VARIABLE SOLAR
 2 REALS - Solar subspacecraft
 point, latitude, longitude.

.VARIABLE RING
 REAL - Radius of Saturns's
 ring.

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
Group storing camera pointing

.VARIABLE PCL
 NAME--OPTIONAL
 Specifies the name of a
 TCL variable to contain the
 derived planet center line.

.VARIABLE PCS
 NAME--OPTIONAL
 Specifies the name of a
 TCL variable to contain the
 derived planet center sample.
.LEVEL2

.VARIABLE INP
 1 or 2 input files.

       The required first file is an input picture containing the
       target body. The maximum size is 1400 by 1400.

       The optional second file (IBIS tiepoint format) contains GEOMA
       parameters for geometrically correcting the input picture. 

.VARIABLE OUT
 0-3 Output Files:

 The LAST output file contains GEOMA parameters which the user can use
 as a secondary input to GEOMA (along with the input image to FARENC) to
 produce an image with the target body centered on NL/2.,NS/2.
 This file will have the format of an IBIS tiepoint file  and will contain the 
 GEOM parameters necessary to translate the picture to the center of the 
 output picture.  If the second input is provided, geometric correction
 will be included in the translation. Since for GLL project there are
 no reseaus the geometric correction is included for all inputs
 not already corrected.
 (See parameter GEOM)

 In addition 2 optional output files may be specified: CURVE and POINTS:

   CURVE must be the first output data set, if it is desired.  It is the
   size of the input picture, and displays all of the limb fitted curves
   superimposed on the target body.

   POINTS must be the the second output data set, if it is desired.  It
   is a byte image the size of the input picture containing a display of
   all limb points. 
   All limb points used in the fitting are in black.
   All candidate limb points not used in the fit are in white.

Note: If the input image has a map3 perspective label placed there by
      the PERSLAB program Farenc will ignore the sedr/spice and
      will write the planet center to an output file called: FARENC.POS
      This file is a Vicar image, has 1 line & 2 samples with the
      real values line,sample.

.VARIABLE SIZE
 4 INTEGERS - A Vicar SIZE field which is ignored unless the GEOM
 is specified, then Number-of-lines and Number-of-samples refer
 to the size of the output file containing the translated image.

.VARIABLE SL
 INTEGER - Starting line in the input file to be processed

.VARIABLE SS
 INTEGER - Starting sample in the input file to be processed

.VARIABLE NL
 INTEGER - Number of lines in the input file to be processed

.VARIABLE NS
 INTEGER - Number of samples in the input file to be processed

.VARIABLE FORMAT
 ('HALF or 'BYTE) - Specifies the Input picture, the Curve data set
  and the GEOMed image are to be in halfword or byte format.
  Default is to read the format parameter in the VICAR label.

.VARIABLE NOSEDR
   A keyword which specifies that SEDR file information is to be ignored.
   The default is to read the SEDR file.

.VARIABLE RANGE
  The maximum permitted error in the SPICE/SEDR location of the
  planet center in pixels.  This range is used to restrict farenc
  in it's search for the planet center from the predicted SPICE
  planet center location. Default is 400 for VGR and 250 for GLL.
  If no sedr/spice available it defaults to the picture size.
  Range is the diameter of the error circle.
  Does not apply if an unconstrained circle is used.

.VARIABLE INITCEN
Real, 2 values representing the initial line,samp location of the
planet center. The default is to consult the sedr/spice. If no
sedr/spice is available this will default to the image center.
Ignored for the unconstrained circle. This is an object space coordinate.
Example: initcen=(256.,256.)  


.VARIABLE DNTHRESH
 INTEGER - All pixels whose DN's are less than DNTHRESH will
 be ignored.  No activity value will be computed for such
 pixels and they will never be considered as a potential
 target body limb position.
 Default is DNTHRESH = 30 for byte and 200 for halfword.
 See AUTO keyword.

.VARIABLE ACTIVITY
 INTEGER - All pixels whose activity value is less than
 ACTIVITY will be ignored.
 If 'AUTO keyword is NOT used: Default is ACTIVITY = 55.
 See AUTO keyword.

.VARIABLE DISTANCE
 INTEGER - Each column and row through out the picture is
 searched for two maximum activity values.  The values must be
 at least DISTANCE apart.  If no two acceptable values occur
 at a separation of at least DISTANCE pixels, the value with
 the least activity measure will be discarded. This value is
 used to assure both sides of the target body are represented.
 Default is DISTANCE = 10

.VARIABLE HEIGHT
 REAL - The maximum accepted ratio between the activity
 values of the second and first maximum on each line and
 column.  A HEIGHT of 1 would indicate that the second largest
 value must be at least equal to the largest value.
 This would virtually preclude a second value.
 Default is HEIGHT = 0.60.

.VARIABLE BELOW
 At least one of the
 adjacent 8 pixels must lie at or below the value BELOW.
 To use this parameter the value of BELOW must be above
 the dark current.
 Default is 30 for byte and 200 for halfword pictures. 
 See AUTO keyword.

.VARIABLE AUTO
 'AUTO - 
 AUTOmatically resets ACTIVITY
 AUTOmatically resets DNTHRESH
 AUTOmatically resets BELOW
 depending on the maximum DN in the picture.  The maximum
 DN is determined by computing a certain percentage of the
 way down from the top of the histogram (See PERCENT).
 If this value is MAXDN then the following defaults are
 evaluated:

       ACTIVITY = (MAXDN-OFFSET)/7 
       DNTHRESH = (MAXDN-OFFSET)/7+OFFSET
       BELOW    = (MAXDN-OFFSET)/7+OFFSET

 Parameters override all defaults and auto values.
 For the case of halfword pictures, DN
 values above 14000 will be truncated to 14000.

.vari FRACTION
 This parameter is a user-supplied estimate of the fraction of the
 target body that is contained in the image.  It is only used in the
 AUTO algorithm (see parameter AUTO), in estimating the lowest DN
 on the planet.  Its most reliable usage is when the entire planet is
 in view, in which case the user would specify FRACTION=1.0 in order
 to enable this algorithm.  (Estimating the fraction of a partially
 covered target body may lead to erroneous results, and should be
 done with care.)

 Note that the default value of 0.0 does NOT mean that by default
 the target body is 100% off the image, but merely serves to disable
 this algorithm:  by default, the AUTO algorithm will not attempt to
 take account of the size of the target body in the image.

.VARIABLE OFFSET
 INTEGER _ Specified when a Dark Current OFFSET has been
 added to the picture. The offset is used only in computing
 the new ACTIVITY value in the AUTO mode.
 Default is OFFSET = 0.

.VARIABLE PERCENT
 REAL - The PERCENTage to bypass at the top of the histogram
 for AUTO.
 Default is 0.20.
 NOTE PERCENT is ignored if AUTO is not specified.

.VARIABLE LINC
 INTEGER - The interval between lines to be read for computing
 the AUTO histogram.
 Default is 5.
 LINC is ignored if AUTO is not specified.

.VARIABLE CLUSTER
 2 INTEGERS - (C1,C2) -  The half-width of a box in pixels (C1) and
 the minimum number of limb points to be contained within the box(C2).
 For each candidate limb point, a check is made of the number of
 other points lying within a box of dimension 2*C1 pixels centered at the
 point. If the number is less than C2, the point in question is
 rejected from the set of possible limb points.  Rejected points
 do not appear as white on the points display data set of FARENC.
 Default is C1 = 30, C2 = 24. ( C1 > C2)

.VARIABLE AREA
 4 INTEGERS - (Starting-Line, Starting-Sample, Number-of-Lines,
 Number-of-Samples) - The search for candidate limb points will only
 be performed within the specified area.
 Default is to search the entire image.  Points located will be with
 respect to L=1, S=1.

.VARIABLE MEDIAN
 ODD INTEGER - The filter width.  If specified , each picture line
 will be low pass filtered. The median filter has the advantage of
 passing unchanged all samples which are monotonic within MEDIAN pixels
 Default is to perform no filtering.

.VARIABLE LCIRCLE
 KEYWORD - ('LCIRCLE) - Causes the candidate limb points to be
 selected from the extremum ends of each row and column, i.e., the
 furthest left and right on the line and the furthest top and bottom
 on the column.  LCIRCLE is to be used with RING.

.VARIABLE SIGACT
 REAL - One pass is made through the initial set of potential
 limb points in order to reject all values of activity which
 deviate from the mean activity by more than SIGACT activity.
 Default is SIGACT = 2.5.

.VARIABLE SIGMA
 REAL - In order to eliminate spurious points the program will iterate,
 each time rejecting points which give an error in radius from the
 last least squares fit of a circle by more than +/-SIGMA * sigma where
 sigma is the standard deviation of all the errors.  The process ceases
 when all points lie within +/- TOLERANCE.
 Default is SIGMA = 1.70.

.VARIABLE TOLERANC
 REAL - A final solution for the target body center and radius requires all
 points on the limb must lie within +/-TOLERANCE of a least square radi
 (see operation)
 Default is TOLERANCE = 1.0.

.VARIABLE SUNANGLE
 REAL - The direction of the sun in degrees measured about the target body
 center clockwise from up.  If SEDR is available SUNANGLE will be
 computed automatically.  SUNANGLE allows FARENC to reject terminator
 points for low phase angles.

.VARIABLE PRINT
 KEYWORD - ('PRINT) - Causes a listing of line, sample and radial
 error of all points which were used to compute the target body center.

.VARIABLE MAXNUM
 INTEGER - The maximum number at points on any video line which
 can be usd in the line fit .  This eliminates the use of a bad
 line in the fit.
 Default is MAXNUM = 10.

.VARIABLE RESCALE
 REAL - Specifies the ratio of the desired target body diameter
 (major axis) in pixels to the smallest picture dimension
 (obtained from the size field).  RESCALE provides a
 magnification effect if GEOM is also specified.
 Default is no magnification.
 NOTE: if rescale > 1 the planet will exceed the picture size !

.VARIABLE UPDATE
 KEYWORD - Specifies that the SPICE kernel/SEDR data will
 be updated.

.VARIABLE NORANGLE
 REAL - Specifies the rotation angle ( in degrees ) of the minor
 axis of the ellipse measured clockwise from up about the ellipse
 center.
    Or the north angle of the projected spin axis of the target body
 into the picture measured clockwise from up about the target body
 center in degrees.

.VARIABLE ANGLE
 REAL - Specifies the rotation angle ( in degrees ) of the minor
 axis of the ellipse measured clockwise from up about the ellipse
 center.
    Or the north angle of the projected spin axis of the target body
 into the picture measured clockwise from up about the target body
 center in degrees.

.VARIABLE SMAA
 REAL - Specifies the semimajor axis of the ellipse in the picture
 in pixels.

.VARIABLE SMIA
 REAL - The semiminor axis of the ellipse in the picture in pixels.

.VARIABLE FOCL
 REAL - The camera focal length in millimeters.

.VARIABLE FOCAL
 REAL - The camera focal length in millimeters.

.VARIABLE SCALE
 REAL - The scale in pixels/mm. of the picture.

.VARIABLE PSCALE
 REAL - The number of pixels per millimeter in the camera focal plane.

.VARIABLE RADIUS
 REAL - The polar and equatorial radius of the target body in kilometers

.VARIABLE REQUATOR
 REAL - The target body equatorial radius in kilometers.

.VARIABLE RPOLE
 REAL - The target body polar radius in kilometers.

.VARIABLE FAR
 REAL - The distance between the target body center and the spacecraft
 measured in kilometers.
 Default is -400.

.VARIABLE RMAG
 REAL - The distance between the target body center and the spacecraft
 measured in kilometers.
 Default is -400.

.VARIABLE SLATIUD
 REAL - the geocentric latitude of the subspacecraft point in degrees.

.VARIABLE LATITUDE
 REAL - the geocentric latitude of the subspacecraft point in degrees.

.VARIABLE RING
 REAL - The radius of Saturn's ring (km).  If RING is specified,
 REQUATOR and RPOLAR are unnecessary.  This causes a fit to be
 made to the ring rather than to the target body. (see LCIRCLE)

.VARIABLE LONGITUD
 REAL - The geocentric longitude of the subspacecraft point in degrees.

.VARIABLE SOLAR
 2 REALS - (Solar-Latitude, Solar-Longitude) - Subsolar latitude and
 longitude.

.VARIABLE PLANET
 KEYWORD - This parameter selects the name of a solar system object.
 Specification of any of these keywords sets the polar and
 equatorial radii to the values currently stored in the Vicar
 subroutine GETPLACON.  (The values returned are in km.)

 The program default values indicating "no data available" are:
            Polar radius = Equatorial radius = -400.0 km.

.VARIABLE BEGIN
Percentage of the initial limb candidate points required to fall
within 1 pixel of the limb fit function (circle or ellipse).
Only used in the 'NEW mode of FARENC (as opposed to 'OLD mode).
If this condition fails then the program reduces the percentage
by 10 percent and tries the fit all over again. Eventually the percentage 
may get down from BEGIN to 10 percent, the lowest permitted percentage.
If there still is no solution the solution for the smallest
residual is returned.
Defaults to 80 percent.

.VARIABLE FDS
 FDS=integer overrides the FDS number in the input image label.

.VARIABLE SERNO
 SERNO=integer overrides the camera serial number in the input image label.

.VARIABLE CENTER
 CENTER=(line,sample) causes the center of the target in the output image
 to be set to the specified coordinates. The default is to put the center
 at the center of the output image.

.VARIABLE PUTNORTH
 PUTNORTH=(real) causes the pole of the target body in the output image
 to be rotated to the PUTNORTH position. The coordinates are clockwise 
 from up.
 The pole must be positioned such
 that the total rotation is < 45 degrees
  PUTNORTH=0. puts the pole vertical in the output image.

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

.VARIABLE PCL
 NAME--OPTIONAL
 Specifies the name of a TCL variable to contain the derived planet 
 center line.

.VARIABLE PCS
 NAME--OPTIONAL
 Specifies the name of a TCL variable to contain the derived planet 
 center sample.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfarenc.pdf
! tstfarenc.pdf: Unit test for program FARENC
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar
LOCAL DIR     TYPE=STRING 
LOCAL VGRIMG  TYPE=STRING 
LOCAL VENUS   TYPE=STRING
LOCAL PIX     TYPE=STRING
LOCAL GEO     TYPE=STRING
LOCAL GPR     TYPE=STRING
LOCAL RAW     TYPE=STRING
LOCAL SUMMATION   TYPE=STRING
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!.........................Voyager test.........................................
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/vgr/"
else 
   LET DIR  ="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
end-if
LET RAW = "&DIR"//"f1636832.raw"	!Raw image (byte)
LET PIX = "&DIR"//"f1636832.fic"	!FICORed image (halfword)
LET GPR = "&DIR"//"f1636832.gpribis"	!Geometric correction parameters

!...Test on FICOR'd image.  SPICE data is not used.
!...Instead, all image geometry data are input via parameters.
farenc INP=(&PIX,&GPR) 'nosedr +
       requator=1830. rpole=1830. +
       rmag=806022 lati=-.0962 long=155. +
       nora=285 focal=1500.19 initcen=(600,600) +
       'auto nl=100 ns=100 resca=.8 area=(100 300 800 700)

!...The same as above except that SPICE data is used.  Results should match.
farenc INP=(&PIX,&GPR) target=IO +
      'auto nl=100 ns=100 resca=.8 area=(100 300 800 700)

!...Test on raw image.  Rescale image so that it fits in a 512x512 format with
!...target center at (256,256) with north pointing at 1 o'clock.
farenc INP=(&RAW,&GPR) OUT=(a.img,b.img,g.img) target=IO +
 'AUTO range=800. AREA=(100,200,600,500) +
  NL=512 NS=512 PUTNORTH=30. RESCALE=1. 
geoma INP=(&RAW,g.img) OUT=c.img NL=512 NS=512	!Project the image
list c.img LINC=35 SINC=35	!Low res printout of Io

!...Test on object-space image.  We do the same as before, but first projecting
!...the image to object space before running FARENC.  The outputs c.img and
!...d.img should be nearly identical, but a lot of things have to go right.
geoma INP=(&RAW,&GPR) OUT=os.img NL=1000 NS=1000
farenc INP=os.img  OUT=(a.img,b.img,g.img) target=IO +
 'AUTO range=800. AREA=(100,200,600,500) +
  NL=512 NS=512 PUTNORTH=30. RESCALE=1. 
geoma INP=(os.img,g.img) OUT=d.img NL=512 NS=512
list d.img LINC=35 SINC=35	!Low res printout of Io
f2 (c.img,d.img) diff.img func="in1-in2+128"
hist diff.img

!...Testing PERSLAB label interface.  PERSLAB will access the VGR SPICE kernels
!...and store that data into the label.  FARENC should not access SPICE
!...directly, but get the information from the label.
perslab os.img a.img target=IO
label-list a.img	!Print the PERSLAB label
farenc a.img
list FARENC.POS		!Print the target center output by FARENC
write "Should get  about 5.425E+02  6.07E+02"
!
!.........................Galileo test........................................
!
write " "
write " test of galileo"
write " "
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/gll/"
else 
   LET DIR  ="WMS_TEST_WORK:[TESTDATA.MIPL.GLL]"
end-if
let VENUS= "&DIR"//"venus.img"
LET SUMMATION ="&DIR"//"s0349632122.1"         ! summation mode GALILEO image.
!
!...Test on full-frame image
farenc INP=&VENUS OUT=g.img begin=90 +
       radius=6138.2 'update 'auto below=500 dnth=500 'remote  
!...Use output GEOMA paramters (g.img) to recenter target at (400,400)
geoma INP=(&VENUS,g.img) OUT=f.img 
!...Run FARENC to check that center is at 400 400.
local (pl,ps) real
farenc INP=f.img begin=90 pcl=pl pcs=ps +
 radius=6138.2 'auto below=500 dnth=500 range=400. initcen=(399.,399.)
disp (pl,ps)
list  f.img LINC=80 SINC=80
!
!...Test on summation mode image
farenc &SUMMATION  'remote
!
!..........................Cassini test (only works on Unix).................
!
if ($syschar(1) = "UNIX")
  farenc INP=/project/cassini_work/spice_examples/n1354897340.1 +
    OUT=g.img 'auto initcen=(138,872) 
  geoma INP=(/project/cassini_work/spice_examples/n1354897340.1,g.img) OUT=f.img
end-if
!
!..........................Space Telescope test...............................
! TEST WITH PERSPECTIVE INPUT LABEL & SPACE TELESCOPE DATA
!perslab inp=(&INHLF, &INHDR, &INDAT) out=x.img north=93.
! the solution is about:  centline=246.79 centsamp=240.98
!farenc inp=x.img area=(60,60,680,680)
!list FARENC.POS
!
!........................test showing LSQP bug (fixed 20jul05).................
!... input is a band from a Cassini VIMS cube --  test is for unix only ...

! in old FARENC, this cmd gives infinite loop (on Solaris only) -- break it by doing
! 'kill -9' on taetm process from another window:
! after the fix, this command will result in an error message: "ERROR in routine LSQP"
farenc /project/test_work/testdata/mipl/cassini/farenc/ENC_S12_SQ10B.b166b (p,c) +
 solar=(-21.08 243.1) 'auto clus=(10,2)

! for completeness, show how to avoid error msg in fixed version by tweaking PERC
! parameter:
farenc /project/test_work/testdata/mipl/cassini/farenc/ENC_S12_SQ10B.b166b (p,c) +
 solar=(-21.08 243.1) 'auto clus=(10,2) perc=0.1

end-proc
$!-----------------------------------------------------------------------------
$ create tstfarenc.log_solos
tstfarenc
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/vgr/"
else
end-if
LET RAW = "/project/test_work/testdata/mipl/vgr/"//"f1636832.raw"
LET PIX = "/project/test_work/testdata/mipl/vgr/"//"f1636832.fic"
LET GPR = "/project/test_work/testdata/mipl/vgr/"//"f1636832.gpribis"
farenc INP=(/project/test_work/t+
estdata/mipl/vgr/f1636832.fic,/proj+
ect/test_work/testdata/mipl/vgr/f1636832.g+
pribis) 'nosedr         requator=1830. rpole=1830+
.         rmag=806022 lati=-.0962 long=155.         nora=285 focal=1500.19 initcen=(600,600)         'auto nl=100 ns=100 resca=.8 a+
rea=(100 300 800 700)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS HALFWORD
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806022.00
sub s/c latitude =   -0.09620000
sub s/c longitude =    155.00000
north angle =   285.00000
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1830.0000
polar radius (km) =    1830.0000
sub solar latitude =  0.000E+00
sub solar longitude =  0.000E+00
planet center line=  600.00000
planet center sample=  600.00000
PROJECTED PLANET RADIUS (pxls) =  288.90637
Projected sun angle (deg)=  15.206055
Peak DN value=        5784
ACTIVITY RESET TO         826
BELOW RESET TO         826
DNTHRESH RESET TO         826
AVE ACTIVITY  3587.0220
      + -     5333.5635

Initial number of points located=         707
Points remaining after de-cluster =        356
RANGEN::: Random Seed value of 1122328265 adjusted to 15032009

Constrained annealing circle fit object space:
Line of center=  545.04108
Samp of center=  607.16193
Radial fit mean residual= 0.94137669

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  545.64905
 SAMPLE POSITION OF PLANET CENTER  606.02570
SIGMA OF RADIUS ERRORS  0.26582822
LARGEST RADIUS ERROR FOR ANY POINT 0.96797073
NUMBER OF POINTS USED FOR FIT             135
farenc INP=(/project/test_work/tes+
tdata/mipl/vgr/f1636832.fic,/project/test_work/testdata/mipl/vgr/f1636832.gpribis) target=IO        'auto nl=100 ns=100 resca=.8 ar+
ea=(100 300 800 700)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS HALFWORD
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3501
polar radius (km) =    1815.7000
sub solar latitude = 0.54129398
sub solar longitude =  171.27632
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01163
Projected minor axis (pxls)=  286.64597
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93915
Peak DN value=        5784
ACTIVITY RESET TO         826
BELOW RESET TO         826
DNTHRESH RESET TO         826
AVE ACTIVITY  4172.8286
      + -     5286.4580

Initial number of points located=         534
Points remaining after de-cluster =        292
RANGEN::: Random Seed value of 1122328267 adjusted to 15032011

Constrained annealing ellipse fit object space:
Line of center=  542.56335
Samp of center=  610.23273
Radial fit mean residual= 0.53746158

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.07703
 SAMPLE POSITION OF PLANET CENTER  609.08447
SIGMA OF RADIUS ERRORS  0.21663935
LARGEST RADIUS ERROR FOR ANY POINT 0.95107061
NUMBER OF POINTS USED FOR FIT             182
farenc INP=(/project/test_work/testdata/mipl/vgr/f1636832.+
raw,/project/test_work/testdata/mipl/vgr/f+
1636832.gpribis) OUT=(a.img,b.img,g.img) target=IO   'AUTO range=800. AREA=(100,200,600,500)    NL=512 NS=512 PUTNORTH=30. RESCALE=+
1.
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS BYTE
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3501
polar radius (km) =    1815.7000
sub solar latitude = 0.54129398
sub solar longitude =  171.27632
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01163
Projected minor axis (pxls)=  286.64597
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93915
Peak DN value=         256
ACTIVITY RESET TO          36
BELOW RESET TO          36
DNTHRESH RESET TO          36
AVE ACTIVITY  248.19231
      + -     207.95432

Initial number of points located=         956
Points remaining after de-cluster =        561
RANGEN::: Random Seed value of 1122328269 adjusted to 15032013

Constrained annealing ellipse fit object space:
Line of center=  543.22681
Samp of center=  608.01007
Radial fit mean residual= 0.52072036

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.08643
 SAMPLE POSITION OF PLANET CENTER  608.12500
SIGMA OF RADIUS ERRORS  0.25898346
LARGEST RADIUS ERROR FOR ANY POINT 0.99244314
NUMBER OF POINTS USED FOR FIT             478

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  256.00000
PLANET CENTER MOVED TO SAMPLE=  256.00000

PLANET EXPANDED BY A FACTOR OF  0.88885301
Image rotated clock (deg)=  25.698944

GEOMA RECENTERING PARAMETERS WRITTEN
geoma INP=(/project/test_work/testdata/mipl/vgr/f1636832.raw,g.img) OUT=c.img NL=512 NS=512
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
list c.img LINC=35 SINC=35
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:GEOMA     User:lwk       Date_Time:Mon Jul 25 14:51:11 2005
     Samp     1      71     141     211     281     351     421     491
   Line
      1       7   6   7   7   6   6   4  82   7   5   7   6   8   9  10
     36       7   7   7   6 218 132 196 159 124  82 121   7   8   8   9
     71       8   7   6 124 130 136 180 153 123 124 151 169  73   8   9
    106       8   8 183 151 189 193 147 181 173 166 170 164 131  19   9
    141       8 207 153 255 170 156 171 181 165 197 193 181 174 121   9
    176       8 177 239 210 200 232 156 200 202 173 212 198 218 173  30
    211       7 255 255 252 222 225 224 192 190 206 218 254 205 213 104
    246     172 240 255 255 221 212 194 185 239 228 199 213 223 174 126
    281      74 255 250 207 188 222 206 201 217 232 233 195 200 220 106
    316      13 232 211 216 239 231 241 215 165 176 230 236 212 181 100
    351      14 238 248 213 212 187 167 204 203 196 171 168 185 108  12
    386      16  58 215 229 187 202 182 178 185 183 147 147 128  99  12
    421      13  17 185 176 170 179 158 169 146 169 152 212 123  13  12
    456      13  14  15 167 166 161 147 127 131 118  94  97  14  13  13
    491      13  14  14  14  16 108 111  88 176 116  44  14  13  13  13
geoma INP=(/project/test_work/testdata/mipl/vgr/f1636832.raw,/project/test_work/testdata/mipl/vgr/f1636832.gpribis) OUT=os.img NL=1+
000 NS=1000
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
farenc INP=os.img  OUT=(a.img,b.img,g.img) target=IO  +
 'AUTO range=800. AREA=(100,200,600,500)  +
  NL=512 NS=512 PUTNORTH=30. RESCALE=1.
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is object space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS BYTE
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3501
polar radius (km) =    1815.7000
sub solar latitude = 0.54129398
sub solar longitude =  171.27632
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01163
Projected minor axis (pxls)=  286.64597
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93915
Peak DN value=         256
ACTIVITY RESET TO          36
BELOW RESET TO          36
DNTHRESH RESET TO          36
AVE ACTIVITY  219.75871
      + -     152.83124

Initial number of points located=         957
Points remaining after de-cluster =        525
RANGEN::: Random Seed value of 1122328274 adjusted to 15032018

Constrained annealing ellipse fit object space:
Line of center=  543.28851
Samp of center=  608.03674
Radial fit mean residual= 0.38426512

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.20654
 SAMPLE POSITION OF PLANET CENTER  607.93457
SIGMA OF RADIUS ERRORS  0.24224898
LARGEST RADIUS ERROR FOR ANY POINT 0.97985995
NUMBER OF POINTS USED FOR FIT             484

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  256.00000
PLANET CENTER MOVED TO SAMPLE=  256.00000

PLANET EXPANDED BY A FACTOR OF  0.88885301
Image rotated clock (deg)=  25.698944

GEOMA RECENTERING PARAMETERS WRITTEN
geoma INP=(os.img,g.img) OUT=d.img NL=512 NS=512
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
list d.img LINC=35 SINC=35
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:GEOMA     User:lwk       Date_Time:Mon Jul 25 14:51:15 2005
     Samp     1      71     141     211     281     351     421     491
   Line
      1       7   7   7   7   6   6   4  80   8   5   6   6   8   9   4
     36       8   7   7   6 219 129 195 158 124  82 117   7   7   7   8
     71       8   7   6 128 130 134 180 153 123 124 150 170  76   8   9
    106       8   8 181 153 189 196 147 182 172 166 169 165 131  24   9
    141       8 207 149 255 171 155 174 181 167 197 193 180 173 120   9
    176       8 176 240 210 200 232 157 200 203 174 211 198 218 173  32
    211       7 254 255 252 222 224 225 191 190 205 219 254 205 214 105
    246     149 243 255 255 223 213 194 185 239 228 198 219 223 175 127
    281      63 255 250 207 188 222 206 201 216 232 234 198 201 220 106
    316      13 231 211 215 239 231 240 215 169 176 230 236 212 179 100
    351      13 239 249 213 212 187 168 204 203 194 171 168 184 108  12
    386      16  58 216 229 187 200 183 179 186 183 150 147 128  98  12
    421      13  17 183 177 170 179 158 167 146 168 152 209 123  13  12
    456      13  14  15 164 165 160 147 127 129 118  94  97  14  13  13
    491      14  13  15  14  16 108 110  87 177 116  42  14  13  13  13
f2 (c.img,d.img) diff.img func="in1-in2+128"
Beginning VICAR task f2
F2 version 2-04-94
F2 using byte table lookup
FUNCTION EVALUATED 65536 TIMES
hist diff.img
Beginning VICAR task hist
HIST version 17-SEP-03

         89*      2
         92*      3
         93       1
         95*      1
         96       1
         98*      2
         99       1
        100       4
        101       5
        102       4
        103       3
        104       5
        105       2
        106       6
        107       6
        108      10
        109      14
        110      12
        111      11
        112      22
        113      14
        114      15
        115      36
        116      49
        117      50
        118      84
        119     114
        120     208
        121     322
        122     573
        123    1008    *
        124    1930    **
        125    4155    ****
        126   11077    ************
        127   44748    **************************************************  2
        128  135349    **************************************************  1
        129   42867    ***********************************************
        130   10532    ***********
        131    4114    ****
        132    1880    **
        133     981    *
        134     581
        135     335
        136     222
        137     170
        138     113
        139      98
        140      91
        141      65
        142      62
        143      35
        144      33
        145      32
        146      17
        147      19
        148       7
        149       8
        150      11
        151       9
        152       3
        153       1
        154       3
        155       1
        156       1
        157       3
        159*      1
        160       1
        161       1

AVERAGE GRAY LEVEL=128.0018
STANDARD DEVIATION=1.631582
NUMBER ELEMENTS=  262144
MIN. DN=        89
MAX. DN=       161

perslab os.img a.img target=IO
Beginning VICAR task perslab
PERSLAB version (August 28, 2002)
Project is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Correcting the NORTH_ANGLE with 90-degree offset
WARNING: unable to obtain GLL Planet ID
Target planet body is: IO
Planet id number        501
label-list a.img
Beginning VICAR task label
************************************************************
 
        ************  File a.img ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                1000 lines per band
                1000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1818.699951171875
C_AXIS_RADIUS=1815.300048828125
FOCAL_LENGTH=1500.18994140625
FOCAL_PLANE_SCALE=84.82142639160156
NORTH_ANGLE=55.69894409179688
OPT_AXIS_INTERCEPT_LINE=500.0
OPT_AXIS_INTERCEPT_SAMPLE=500.0
PLANET_CENTER_LINE=541.8434448242188
PLANET_CENTER_SAMPLE=607.6538696289062
SUB_SPACECRAFT_LATITUDE=-0.03242464736104012
SUB_SPACECRAFT_LONGITUDE=156.4738159179688
TARGET_CENTER_DISTANCE=806029.875
MAP_PROJECTION_DESC=(
'Projection used to show target bodies as seen by the observer (camera).', 
'Central meridian and a particular parallel (if shown) are straight ', 
'lines.  Other meridians and parallels are usually arcs of circles or ellipses,', 
'but some may be parabolas or hyperbolas.  This projection is neither conformal', 
'nor equal-area.', 
'The algorithm is described in the JPL memo: ''Planet-to-Camera geometry for flight images''', 
'Gary Yagi, 8 Oct.1985', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: CONV12 -- User: DFS -- Thu May  3 11:31:13 1984 ----
LAB01=
'77                   800     800 800 800 L 1                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/JS4459/02                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'INSERT            05 06 83 13:12:50          JAM                      GL'
NLABS=11
---- Task: GEOMA -- User: lwk -- Mon Jul 25 14:51:12 2005 ----
---- Task: PERSLAB -- User: lwk -- Mon Jul 25 14:51:17 2005 ----
SUB_SOLAR_LATITUDE=0.541294
SUB_SOLAR_LONGITUDE=171.276
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1818.699951171875
C_AXIS_RADIUS=1815.300048828125
FOCAL_LENGTH=1500.18994140625
FOCAL_PLANE_SCALE=84.82142639160156
NORTH_ANGLE=55.69894409179688
OPT_AXIS_INTERCEPT_LINE=500.0
OPT_AXIS_INTERCEPT_SAMPLE=500.0
PLANET_CENTER_LINE=541.8434448242188
PLANET_CENTER_SAMPLE=607.6538696289062
SUB_SPACECRAFT_LATITUDE=-0.03242464736104012
SUB_SPACECRAFT_LONGITUDE=156.4738159179688
TARGET_CENTER_DISTANCE=806029.875
 
************************************************************
farenc a.img
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image has perspective map label
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3500
polar radius (km) =    1815.3000
sub solar latitude = 0.54129398
sub solar longitude =  171.27600
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01160
Projected minor axis (pxls)=  286.58282
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93921
AVE ACTIVITY  202.50861
      + -     178.78754

Initial number of points located=        1081
Points remaining after de-cluster =        629
RANGEN::: Random Seed value of 1122328279 adjusted to 15032023

Constrained annealing ellipse fit object space:
Line of center=  543.33459
Samp of center=  607.80017
Radial fit mean residual= 0.52072752

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.46399
 SAMPLE POSITION OF PLANET CENTER  607.67908
SIGMA OF RADIUS ERRORS  0.24065477
LARGEST RADIUS ERROR FOR ANY POINT 0.99243617
NUMBER OF POINTS USED FOR FIT             530
list FARENC.POS
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FARENC    User:lwk       Date_Time:Mon Jul 25 14:51:19 2005
     Samp             1           2
   Line
      1       5.435E+02   6.077E+02
write "Should get  about 5.425E+02  6.07E+02"
Should get  about 5.425E+02  6.07E+02
write " "
 
write " test of galileo"
 test of galileo
write " "
 
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/gll/"
else
end-if
let VENUS= "/project/test_work/testdata/mipl/gll/"//"venus.img"
LET SUMMATION ="/project/test_work/testdata/mipl/gll/"//"s0349632122.1"
farenc INP=/project/test_work/testdata+
/mipl/gll/venus.img OUT=g.img begin=90         radius=6138.2 'update 'auto below=500 dnth=500 'remote
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=GLL
SYSTEM LABEL SAYS INPUT IS HALFWORD
GETLABCON: warning ind=          1
  ** IMAGE CATALOG WILL BE UPDATED
  ** CAUTION:  USE "SELCAT S" OR CHECK DATA!

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     1629971.9
sub s/c latitude =   -2.9542966
sub s/c longitude =    176.32004
north angle =   178.54636
scale (km/pxl) =   65.616798
focal length (mm) =   1501.0389
equatorial radius (km) =    6138.2002
polar radius (km) =    6138.2002
sub solar latitude = -2.5792704
sub solar longitude =  129.47615
planet center line=  143.25665
planet center sample=  473.59515
PROJECTED PLANET RADIUS (pxls) =  370.91214
Projected sun angle (deg)=  269.31094
Peak DN value=        4744
ACTIVITY RESET TO         677
BELOW RESET TO         677
DNTHRESH RESET TO         677
AVE ACTIVITY  3302.5637
      + -     777.90149

Initial number of points located=         715
Points remaining after de-cluster =        360
RANGEN::: Random Seed value of 1122328283 adjusted to 15032027

Constrained annealing circle fit object space:
Line of center=  143.12100
Samp of center=  473.49564
Radial fit mean residual= 0.25872725

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  143.33217
 SAMPLE POSITION OF PLANET CENTER  473.53003
SIGMA OF RADIUS ERRORS  0.14742866
LARGEST RADIUS ERROR FOR ANY POINT 0.61871964
NUMBER OF POINTS USED FOR FIT             360
INPUT TO MOMATV=    400.000000  400.000000  143.332169  473.530029   65.616798 1501.038940  176.320038   -2.954297  178.546356
INPUT TO MOMAT--SSCPT=(  143,  474)
OM=
           -6.473E-02 -5.238E-02  9.965E-01  9.976E-01 -2.883E-02  6.328E-02  2.541E-02  9.982E-01  5.412E-02
RS=
           -1.624E+06 -1.045E+05 -8.401E+04
SPICE data HAS BEEN UPDATED

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  400.00000
PLANET CENTER MOVED TO SAMPLE=  400.00000

GEOMA RECENTERING PARAMETERS WRITTEN
geoma INP=(/project/test_work/testdata/mipl/gll/venus.img,g.img) OUT=f.img
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS HALFWORD
local (pl,ps) real
farenc INP=f.img begin=90 pcl=pl pcs=ps  +
 radius=6138.2 'auto below=500 dnth=500 range=400. initcen=(399.,399.)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is object space
project=GLL
SYSTEM LABEL SAYS INPUT IS HALFWORD
GETLABCON: warning ind=          1

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     1629971.9
sub s/c latitude =   -2.9542966
sub s/c longitude =    176.32004
north angle =   178.54636
scale (km/pxl) =   65.616798
focal length (mm) =   1501.0389
equatorial radius (km) =    6138.2002
polar radius (km) =    6138.2002
sub solar latitude = -2.5792704
sub solar longitude =  129.47615
planet center line=  399.00000
planet center sample=  399.00000
PROJECTED PLANET RADIUS (pxls) =  370.91214
Projected sun angle (deg)=  269.31094
Peak DN value=        4735
ACTIVITY RESET TO         676
BELOW RESET TO         676
DNTHRESH RESET TO         676
AVE ACTIVITY  3015.6306
      + -     809.44604

Initial number of points located=         739
Points remaining after de-cluster =        369
RANGEN::: Random Seed value of 1122328287 adjusted to 15032031

Constrained annealing circle fit object space:
Line of center=  399.58841
Samp of center=  399.83746
Radial fit mean residual= 0.23426971

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  399.94708
 SAMPLE POSITION OF PLANET CENTER  399.87531
SIGMA OF RADIUS ERRORS  0.15884003
LARGEST RADIUS ERROR FOR ANY POINT 0.66890758
NUMBER OF POINTS USED FOR FIT             369
disp (pl,ps)

pl=399.94708252
ps=399.875305176

list  f.img LINC=80 SINC=80
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:RTO313    Date_Time:Thu May 24 14:30:43 1990
 Task:GEOMA     User:lwk       Date_Time:Mon Jul 25 14:51:25 2005
     Samp       1    81   161   241   321   401   481   561   641   721
   Line

    321       113  4188  4454  3777  3389  2783  2111  1221   184    38
    401       112  4268  4072  3784  3578  2987  2239  1283   262    43
    481        74  4184  4697  4234  3441  2930  2036  1199   182    43
    561        74  3604  4261  4321  3527  2565  1902   932    42     2
    641    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
    721    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
farenc /project/test_work/testdata/mipl/gll/s0349632122.1  'remote
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=GLL
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     665638.56
sub s/c latitude =   -8.0669250
sub s/c longitude =    155.55841
north angle =   194.30228
scale (km/pxl) =   32.808399
focal length (mm) =   1501.0389
equatorial radius (km) =    2632.3450
polar radius (km) =    2632.3501
sub solar latitude = -1.8733770
sub solar longitude =  125.66879
planet center line=  204.34587
planet center sample=  236.88409
Projected major axis (pxls)=  194.75325
Projected minor axis (pxls)=  194.75362
Computed offset of planet from ellipse center
LINE OFFSET -0.000E+00
SAMPLE OFFSET  0.000E+00
Projected sun angle (deg)=  274.14514
AVE ACTIVITY  266.67990
      + -     146.39409

Initial number of points located=         891
Points remaining after de-cluster =        521
RANGEN::: Random Seed value of 1122328289 adjusted to 15032033

Constrained annealing ellipse fit object space:
Line of center=  203.97545
Samp of center=  236.85457
Radial fit mean residual= 0.20064901

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  203.97820
 SAMPLE POSITION OF PLANET CENTER  236.87457
SIGMA OF RADIUS ERRORS  0.15499400
LARGEST RADIUS ERROR FOR ANY POINT 0.75007969
NUMBER OF POINTS USED FOR FIT             521
if ($syschar(1) = "UNIX")
  farenc INP=/project/cassini_work/spice_examples/n1354897340.1  +
    OUT=g.img 'auto initcen=(138,872)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=CASSI
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     2.359E+07
sub s/c latitude =    3.5729308
sub s/c longitude =    47.781807
north angle =   359.91000
scale (km/pxl) =   83.333336
focal length (mm) =   2000.0000
equatorial radius (km) =    71492.000
polar radius (km) =    66854.000
sub solar latitude =  2.9425383
sub solar longitude =  40.735386
planet center line=  138.00000
planet center sample=  872.00000
Projected major axis (pxls)=  505.04163
Projected minor axis (pxls)=  472.40900
Computed offset of planet from ellipse center
LINE OFFSET -0.01195199
SAMPLE OFFSET -1.877E-05
Projected sun angle (deg)=  94.823608
Peak DN value=         202
ACTIVITY RESET TO          28
BELOW RESET TO          28
DNTHRESH RESET TO          28
AVE ACTIVITY  78.808334
      + -     21.691105

Initial number of points located=         235
Points remaining after de-cluster =        176
RANGEN::: Random Seed value of 1122328292 adjusted to 15032036
No convergence,residual=  2.6875916
RANGEN::: Random Seed value of 1122328292 adjusted to 15032036
No convergence,residual=  2.6875901
RANGEN::: Random Seed value of 1122328292 adjusted to 15032036

Constrained annealing ellipse fit object space:
Line of center=  139.03928
Samp of center=  839.53809
Radial fit mean residual= 0.18759461

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  138.78581
 SAMPLE POSITION OF PLANET CENTER  840.98999
SIGMA OF RADIUS ERRORS  0.14513496
LARGEST RADIUS ERROR FOR ANY POINT 0.54348469
NUMBER OF POINTS USED FOR FIT             117

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  512.00000
PLANET CENTER MOVED TO SAMPLE=  512.00000

GEOMA RECENTERING PARAMETERS WRITTEN
  geoma INP=(/project/cassini_work/spice_examples/n1354897340.1,g.img) OUT=f.img
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
end-if
farenc /project/test_work/testdata/mipl/cassini/farenc/ENC_S12_SQ10B.b166b (p,c)  +
 solar=(-21.08 243.1) 'auto clus=(10,2)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image has perspective map label
xlget: status=         -44
Failure to get solar latitude
xlget: status=         -44
Failure to get solar longitude
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     36444.684
sub s/c latitude =   -47.858871
sub s/c longitude =    195.20094
north angle =   67.290337
scale (km/pxl) =   18.779343
focal length (mm) =   426.00000
equatorial radius (km) =    251.79999
polar radius (km) =    244.60001
sub solar latitude = -21.080000
sub solar longitude =  243.10001
planet center line=  52.764778
planet center sample=  43.763611
Projected major axis (pxls)=  55.274082
Projected minor axis (pxls)=  54.568226
Computed offset of planet from ellipse center
LINE OFFSET 0.00413486
SAMPLE OFFSET -0.00988003
Projected sun angle (deg)=  355.10956
Peak DN value=         251
ACTIVITY RESET TO          35
BELOW RESET TO          35
DNTHRESH RESET TO          35
AVE ACTIVITY  279.41510
      + -     228.22243

Initial number of points located=         106
Points remaining after de-cluster =         71
RANGEN::: Random Seed value of 1122328295 adjusted to 15032039
No convergence,residual=  1.7268059
RANGEN::: Random Seed value of 1122328295 adjusted to 15032039

Constrained annealing ellipse fit object space:
Line of center=  52.392872
Samp of center=  43.366814
Radial fit mean residual= 0.93047953
ERROR in routine LSQP: Please check input parameters
 ** ABEND called **
continue
farenc /project/test_work/testdata/mipl/cassini/farenc/ENC_S12_SQ10B.b166b (p,c)  +
 solar=(-21.08 243.1) 'auto clus=(10,2) perc=0.1
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image has perspective map label
xlget: status=         -44
Failure to get solar latitude
xlget: status=         -44
Failure to get solar longitude
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     36444.684
sub s/c latitude =   -47.858871
sub s/c longitude =    195.20094
north angle =   67.290337
scale (km/pxl) =   18.779343
focal length (mm) =   426.00000
equatorial radius (km) =    251.79999
polar radius (km) =    244.60001
sub solar latitude = -21.080000
sub solar longitude =  243.10001
planet center line=  52.764778
planet center sample=  43.763611
Projected major axis (pxls)=  55.274082
Projected minor axis (pxls)=  54.568226
Computed offset of planet from ellipse center
LINE OFFSET 0.00413486
SAMPLE OFFSET -0.00988003
Projected sun angle (deg)=  355.10956
Peak DN value=         254
ACTIVITY RESET TO          36
BELOW RESET TO          36
DNTHRESH RESET TO          36
AVE ACTIVITY  279.74527
      + -     227.17860

Initial number of points located=         106
Points remaining after de-cluster =         71
RANGEN::: Random Seed value of 1122328296 adjusted to 15032040
No convergence,residual=  1.7501464
RANGEN::: Random Seed value of 1122328297 adjusted to 15032041

Constrained annealing ellipse fit object space:
Line of center=  52.859142
Samp of center=  43.155312
Radial fit mean residual= 0.94470507

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  53.052616
 SAMPLE POSITION OF PLANET CENTER  43.605724
SIGMA OF RADIUS ERRORS  0.27185541
LARGEST RADIUS ERROR FOR ANY POINT 0.97934788
NUMBER OF POINTS USED FOR FIT              35

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  25.000000
PLANET CENTER MOVED TO SAMPLE=  49.000000

GEOMA RECENTERING PARAMETERS WRITTEN
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
$!-----------------------------------------------------------------------------
$ create tstfarenc.log_linux
tstfarenc
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/vgr/"
else
end-if
LET RAW = "/project/test_work/testdata/mipl/vgr/"//"f1636832.raw"
LET PIX = "/project/test_work/testdata/mipl/vgr/"//"f1636832.fic"
LET GPR = "/project/test_work/testdata/mipl/vgr/"//"f1636832.gpribis"
farenc INP=(/project/test_work/t+
estdata/mipl/vgr/f1636832.fic,/proj+
ect/test_work/testdata/mipl/vgr/f1636832.g+
pribis) 'nosedr         requator=1830. rpole=1830+
.         rmag=806022 lati=-.0962 long=155.         nora=285 focal=1500.19 initcen=(600,600)         'auto nl=100 ns=100 resca=.8 a+
rea=(100 300 800 700)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS HALFWORD
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806022.00
sub s/c latitude =   -0.09620000
sub s/c longitude =    155.00000
north angle =   285.00000
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1830.0000
polar radius (km) =    1830.0000
sub solar latitude =  0.000E+00
sub solar longitude =  0.000E+00
planet center line=  600.00000
planet center sample=  600.00000
PROJECTED PLANET RADIUS (pxls) =  288.90637
Projected sun angle (deg)=  15.206055
Peak DN value=        5784
ACTIVITY RESET TO         826
BELOW RESET TO         826
DNTHRESH RESET TO         826
AVE ACTIVITY  3587.0220
      + -     5333.5630

Initial number of points located=         707
Points remaining after de-cluster =        356
RANGEN::: Random Seed value of 1122328580 adjusted to 15032324

Constrained annealing circle fit object space:
Line of center=  545.04004
Samp of center=  607.16254
Radial fit mean residual= 0.94132328

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  545.64905
 SAMPLE POSITION OF PLANET CENTER  606.02576
SIGMA OF RADIUS ERRORS  0.26582637
LARGEST RADIUS ERROR FOR ANY POINT 0.96797049
NUMBER OF POINTS USED FOR FIT             135
farenc INP=(/project/test_work/tes+
tdata/mipl/vgr/f1636832.fic,/project/test_work/testdata/mipl/vgr/f1636832.gpribis) target=IO        'auto nl=100 ns=100 resca=.8 ar+
ea=(100 300 800 700)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS HALFWORD
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3501
polar radius (km) =    1815.7000
sub solar latitude = 0.54129398
sub solar longitude =  171.27632
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01163
Projected minor axis (pxls)=  286.64597
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93918
Peak DN value=        5784
ACTIVITY RESET TO         826
BELOW RESET TO         826
DNTHRESH RESET TO         826
AVE ACTIVITY  4172.8286
      + -     5286.4580

Initial number of points located=         534
Points remaining after de-cluster =        292
RANGEN::: Random Seed value of 1122328581 adjusted to 15032325

Constrained annealing ellipse fit object space:
Line of center=  542.56909
Samp of center=  610.24036
Radial fit mean residual= 0.53776169

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.07703
 SAMPLE POSITION OF PLANET CENTER  609.08447
SIGMA OF RADIUS ERRORS  0.21663995
LARGEST RADIUS ERROR FOR ANY POINT 0.95103300
NUMBER OF POINTS USED FOR FIT             182
farenc INP=(/project/test_work/testdata/mipl/vgr/f1636832.+
raw,/project/test_work/testdata/mipl/vgr/f+
1636832.gpribis) OUT=(a.img,b.img,g.img) target=IO   'AUTO range=800. AREA=(100,200,600,500)    NL=512 NS=512 PUTNORTH=30. RESCALE=+
1.
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS BYTE
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3501
polar radius (km) =    1815.7000
sub solar latitude = 0.54129398
sub solar longitude =  171.27632
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01163
Projected minor axis (pxls)=  286.64597
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93918
Peak DN value=         256
ACTIVITY RESET TO          36
BELOW RESET TO          36
DNTHRESH RESET TO          36
AVE ACTIVITY  248.19231
      + -     207.95425

Initial number of points located=         956
Points remaining after de-cluster =        561
RANGEN::: Random Seed value of 1122328582 adjusted to 15032326

Constrained annealing ellipse fit object space:
Line of center=  543.23193
Samp of center=  608.01349
Radial fit mean residual= 0.52069724

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.08643
 SAMPLE POSITION OF PLANET CENTER  608.12500
SIGMA OF RADIUS ERRORS  0.25898197
LARGEST RADIUS ERROR FOR ANY POINT 0.99238294
NUMBER OF POINTS USED FOR FIT             478

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  256.00000
PLANET CENTER MOVED TO SAMPLE=  256.00000

PLANET EXPANDED BY A FACTOR OF  0.88885301
Image rotated clock (deg)=  25.698944

GEOMA RECENTERING PARAMETERS WRITTEN
geoma INP=(/project/test_work/testdata/mipl/vgr/f1636832.raw,g.img) OUT=c.img NL=512 NS=512
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
list c.img LINC=35 SINC=35
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:GEOMA     User:lwk       Date_Time:Mon Jul 25 14:56:23 2005
     Samp     1      71     141     211     281     351     421     491
   Line
      1       7   6   7   7   6   6   4  82   7   5   7   6   8   9  10
     36       7   7   7   6 218 132 196 159 124  82 121   7   8   8   9
     71       8   7   6 124 130 136 180 153 123 124 151 169  73   8   9
    106       8   8 183 151 189 193 147 181 173 166 170 164 131  19   9
    141       8 207 153 255 170 156 171 181 165 197 193 181 174 121   9
    176       8 177 239 210 200 232 156 200 202 173 212 198 218 173  30
    211       7 255 255 252 222 225 224 192 190 206 218 254 205 213 104
    246     172 240 255 255 221 212 194 185 239 228 199 213 223 174 126
    281      74 255 250 207 188 222 206 201 217 232 233 195 200 220 106
    316      13 232 211 216 239 231 241 215 165 176 230 236 212 181 100
    351      14 238 248 213 212 187 167 204 203 196 171 168 185 108  12
    386      16  58 215 229 187 202 182 178 185 183 147 147 128  99  12
    421      13  17 185 176 170 179 158 169 146 169 152 212 123  13  12
    456      13  14  15 167 166 161 147 127 131 118  94  97  14  13  13
    491      13  14  14  14  16 108 111  88 176 116  44  14  13  13  13
geoma INP=(/project/test_work/testdata/mipl/vgr/f1636832.raw,/project/test_work/testdata/mipl/vgr/f1636832.gpribis) OUT=os.img NL=1+
000 NS=1000
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
farenc INP=os.img  OUT=(a.img,b.img,g.img) target=IO  +
 'AUTO range=800. AREA=(100,200,600,500)  +
  NL=512 NS=512 PUTNORTH=30. RESCALE=1.
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is object space
project=VGR-1
SYSTEM LABEL SAYS INPUT IS BYTE
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3501
polar radius (km) =    1815.7000
sub solar latitude = 0.54129398
sub solar longitude =  171.27632
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01163
Projected minor axis (pxls)=  286.64597
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93918
Peak DN value=         256
ACTIVITY RESET TO          36
BELOW RESET TO          36
DNTHRESH RESET TO          36
AVE ACTIVITY  219.75871
      + -     152.83124

Initial number of points located=         957
Points remaining after de-cluster =        525
RANGEN::: Random Seed value of 1122328584 adjusted to 15032328

Constrained annealing ellipse fit object space:
Line of center=  543.28241
Samp of center=  608.03174
Radial fit mean residual= 0.38437805

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.19934
 SAMPLE POSITION OF PLANET CENTER  607.93701
SIGMA OF RADIUS ERRORS  0.24370646
LARGEST RADIUS ERROR FOR ANY POINT 0.98431593
NUMBER OF POINTS USED FOR FIT             485

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  256.00000
PLANET CENTER MOVED TO SAMPLE=  256.00000

PLANET EXPANDED BY A FACTOR OF  0.88885301
Image rotated clock (deg)=  25.698944

GEOMA RECENTERING PARAMETERS WRITTEN
geoma INP=(os.img,g.img) OUT=d.img NL=512 NS=512
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
list d.img LINC=35 SINC=35
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:GEOMA     User:lwk       Date_Time:Mon Jul 25 14:56:26 2005
     Samp     1      71     141     211     281     351     421     491
   Line
      1       7   7   7   7   6   6   4  79   8   5   6   6   8   9   4
     36       8   7   7   6 219 129 195 158 124  82 117   7   7   7   9
     71       8   7   6 128 130 134 180 153 123 124 150 170  76   8   9
    106       8   8 181 153 189 196 147 182 172 166 169 165 131  24   9
    141       8 207 149 255 171 155 174 181 167 197 193 180 173 120   9
    176       8 177 240 210 200 232 157 200 203 174 211 198 218 173  32
    211       7 254 255 252 222 224 225 191 190 205 219 254 205 214 105
    246     148 243 255 255 223 213 194 185 239 228 198 219 223 175 127
    281      63 255 250 207 188 222 206 201 216 232 234 198 201 220 106
    316      13 231 211 215 239 231 240 215 169 176 230 236 212 179 100
    351      13 239 249 213 212 187 168 204 203 194 171 168 184 108  12
    386      16  58 216 229 187 200 183 179 186 183 150 147 128  98  12
    421      13  17 183 177 170 179 158 167 146 168 152 209 123  13  12
    456      13  14  15 164 165 160 147 127 129 118  94  97  14  13  13
    491      14  13  15  14  16 108 110  87 177 116  42  14  13  13  13
f2 (c.img,d.img) diff.img func="in1-in2+128"
Beginning VICAR task f2
F2 version 2-04-94
F2 using byte table lookup
FUNCTION EVALUATED 65536 TIMES
hist diff.img
Beginning VICAR task hist
HIST version 17-SEP-03

         89*      1
         90       1
         92*      2
         93       2
         95*      1
         97*      1
         98       1
         99       2
        100       2
        101       5
        102       4
        103       3
        104       5
        105       4
        106       3
        107       5
        108      10
        109      14
        110       9
        111      15
        112      16
        113      19
        114      17
        115      26
        116      49
        117      49
        118      78
        119     107
        120     185
        121     315
        122     528
        123     981    *
        124    1852    **
        125    4039    ****
        126   10922    ************
        127   44777    **************************************************  2
        128  136408    **************************************************  1
        129   42794    ***********************************************
        130   10271    ***********
        131    4002    ****
        132    1839    **
        133     930    *
        134     540
        135     324
        136     229
        137     158
        138     115
        139      95
        140      87
        141      59
        142      63
        143      35
        144      30
        145      32
        146      16
        147      17
        148       9
        149       6
        150      10
        151       8
        152       5
        153       1
        154       3
        155       2
        157*      3
        159*      1
        160       1
        161       1

AVERAGE GRAY LEVEL=128.0016
STANDARD DEVIATION=1.605138
NUMBER ELEMENTS=  262144
MIN. DN=        89
MAX. DN=       161

perslab os.img a.img target=IO
Beginning VICAR task perslab
PERSLAB version (August 28, 2002)
Project is VGR-1
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Warning::year number is not 4-digit.
Correcting the NORTH_ANGLE with 90-degree offset
WARNING: unable to obtain GLL Planet ID
Target planet body is: IO
Planet id number        501
label-list a.img
Beginning VICAR task label
************************************************************
 
        ************  File a.img ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                1000 lines per band
                1000 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Property: MAP ----
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1818.699951171875
C_AXIS_RADIUS=1815.300048828125
FOCAL_LENGTH=1500.18994140625
FOCAL_PLANE_SCALE=84.82142639160156
NORTH_ANGLE=55.69894409179688
OPT_AXIS_INTERCEPT_LINE=500.0
OPT_AXIS_INTERCEPT_SAMPLE=500.0
PLANET_CENTER_LINE=541.8434448242188
PLANET_CENTER_SAMPLE=607.6538696289062
SUB_SPACECRAFT_LATITUDE=-0.03242464736104012
SUB_SPACECRAFT_LONGITUDE=156.4738159179688
TARGET_CENTER_DISTANCE=806029.875
MAP_PROJECTION_DESC=(
'Projection used to show target bodies as seen by the observer (camera).', 
'Central meridian and a particular parallel (if shown) are straight ', 
'lines.  Other meridians and parallels are usually arcs of circles or ellipses,', 
'but some may be parabolas or hyperbolas.  This projection is neither conformal', 
'nor equal-area.', 
'The algorithm is described in the JPL memo: ''Planet-to-Camera geometry for flight images''', 
'Gary Yagi, 8 Oct.1985', 
'The value of the COORDINATE_SYSTEM_NAME item determines whether latitudes are', 
'planetographic or planetocentric;  if this  keyword is absent, then', 
'the default is planetographic.', 
'The direction of increasing longitude is defined by the POSITIVE_LONGITUDE_DIRECTION', 
'item;  if this keyword is absent, then the direction is determined by', 
'COORDINATE_SYSTEM_NAME:  it is East if the system is planetographic, West if it is', 
'planetocentric.', 
'NOTE: Portions of above text taken from U.S. Geological Survey Professional Paper 1395,', 
'second printing 1989, ''Map Projections - A Working Manual'' by John Snyder.', 
'See text for detailed formulae.')
---- Task: CONV12 -- User: DFS -- Thu May  3 11:31:13 1984 ----
LAB01=
'77                   800     800 800 800 L 1                          SC'
LAB02=
'VGR-1   FDS 16368.32   PICNO 0548J1-001   SCET 79.063 19:23:00         C'
LAB03=
'NA CAMERA  EXP 00360.0 MSEC  FILT 0(CLEAR )  LO GAIN  SCAN RATE  1:1   C'
LAB04=
'ERT 79.063 20:00:36   1/ 1 FULL    RES   VIDICON TEMP  -81.00 DEG C    C'
LAB05=
'IN/107971/07 OUT/JS4459/02                 DSS #**   BIT SNR   34.133  C'
LAB06=
' 01865 A/9F000BC0 B/7082 C/C201 D/007F0000 ETLM/CC2BA1CA736ADEB02512F AC'
LAB07=
'NA OPCAL 00(*****.* MSEC) PIXAVG 192/0 OPERATIONAL MODE 4(BOTALT)     AC'
LAB08=
'CAM ECAL CYCLE BEAM  RESET OPEN  CLOSE FLOOD AEXPM  FIL G1 SHUT MODE  AC'
LAB09=
'NA   NO   READ  YES   NO    NO    NO    NO    NO    1 P  * NORMAL     AC'
LAB10=
'WA   NO   PREP  YES   YES   YES   YES   YES   NO    4 P  3 NORMAL     AC'
LAB11=
'INSERT            05 06 83 13:12:50          JAM                      GL'
NLABS=11
---- Task: GEOMA -- User: lwk -- Mon Jul 25 14:56:23 2005 ----
---- Task: PERSLAB -- User: lwk -- Mon Jul 25 14:56:26 2005 ----
SUB_SOLAR_LATITUDE=0.541294
SUB_SOLAR_LONGITUDE=171.276
MAP_PROJECTION_TYPE='POINT_PERSPECTIVE'
COORDINATE_SYSTEM_NAME='PLANETOCENTRIC'
POSITIVE_LONGITUDE_DIRECTION='WEST'
A_AXIS_RADIUS=1830.0
B_AXIS_RADIUS=1818.699951171875
C_AXIS_RADIUS=1815.300048828125
FOCAL_LENGTH=1500.18994140625
FOCAL_PLANE_SCALE=84.82142639160156
NORTH_ANGLE=55.69894409179688
OPT_AXIS_INTERCEPT_LINE=500.0
OPT_AXIS_INTERCEPT_SAMPLE=500.0
PLANET_CENTER_LINE=541.8434448242188
PLANET_CENTER_SAMPLE=607.6538696289062
SUB_SPACECRAFT_LATITUDE=-0.03242464736104012
SUB_SPACECRAFT_LONGITUDE=156.4738159179688
TARGET_CENTER_DISTANCE=806029.875
 
************************************************************
farenc a.img
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image has perspective map label
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     806029.88
sub s/c latitude =   -0.03242465
sub s/c longitude =    156.47382
north angle =   55.698944
scale (km/pxl) =   84.821426
focal length (mm) =   1500.1899
equatorial radius (km) =    1824.3500
polar radius (km) =    1815.3000
sub solar latitude = 0.54129398
sub solar longitude =  171.27600
planet center line=  541.84344
planet center sample=  607.65387
Projected major axis (pxls)=  288.01160
Projected minor axis (pxls)=  286.58282
Computed offset of planet from ellipse center
LINE OFFSET  0.000E+00
SAMPLE OFFSET -0.000E+00
Projected sun angle (deg)=  327.93921
AVE ACTIVITY  202.50861
      + -     178.78754

Initial number of points located=        1081
Points remaining after de-cluster =        629
RANGEN::: Random Seed value of 1122328587 adjusted to 15032331

Constrained annealing ellipse fit object space:
Line of center=  543.34222
Samp of center=  607.79327
Radial fit mean residual= 0.52073568

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  543.46399
 SAMPLE POSITION OF PLANET CENTER  607.67908
SIGMA OF RADIUS ERRORS  0.24065498
LARGEST RADIUS ERROR FOR ANY POINT 0.99243361
NUMBER OF POINTS USED FOR FIT             530
list FARENC.POS
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:CONV12    User:DFS       Date_Time:Thu May  3 11:31:13 1984
 Task:FARENC    User:lwk       Date_Time:Mon Jul 25 14:56:27 2005
     Samp             1           2
   Line
      1       5.435E+02   6.077E+02
write "Should get  about 5.425E+02  6.07E+02"
Should get  about 5.425E+02  6.07E+02
write " "
 
write " test of galileo"
 test of galileo
write " "
 
if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/gll/"
else
end-if
let VENUS= "/project/test_work/testdata/mipl/gll/"//"venus.img"
LET SUMMATION ="/project/test_work/testdata/mipl/gll/"//"s0349632122.1"
farenc INP=/project/test_work/testdata+
/mipl/gll/venus.img OUT=g.img begin=90         radius=6138.2 'update 'auto below=500 dnth=500 'remote
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=GLL
SYSTEM LABEL SAYS INPUT IS HALFWORD
GETLABCON: warning ind=          1
  ** IMAGE CATALOG WILL BE UPDATED
  ** CAUTION:  USE "SELCAT S" OR CHECK DATA!

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     1629971.9
sub s/c latitude =   -2.9542966
sub s/c longitude =    176.32004
north angle =   178.54636
scale (km/pxl) =   65.616798
focal length (mm) =   1501.0389
equatorial radius (km) =    6138.2002
polar radius (km) =    6138.2002
sub solar latitude = -2.5792704
sub solar longitude =  129.47615
planet center line=  143.25665
planet center sample=  473.59515
PROJECTED PLANET RADIUS (pxls) =  370.91211
Projected sun angle (deg)=  269.31094
Peak DN value=        4744
ACTIVITY RESET TO         677
BELOW RESET TO         677
DNTHRESH RESET TO         677
AVE ACTIVITY  3302.5637
      + -     777.90216

Initial number of points located=         715
Points remaining after de-cluster =        360
RANGEN::: Random Seed value of 1122328589 adjusted to 15032333

Constrained annealing circle fit object space:
Line of center=  143.12416
Samp of center=  473.49738
Radial fit mean residual= 0.25872955

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  143.33217
 SAMPLE POSITION OF PLANET CENTER  473.53000
SIGMA OF RADIUS ERRORS  0.14742841
LARGEST RADIUS ERROR FOR ANY POINT 0.61872613
NUMBER OF POINTS USED FOR FIT             360
INPUT TO MOMATV=    400.000000  400.000000  143.332169  473.529999   65.616798 1501.038940  176.320038   -2.954297  178.546356
INPUT TO MOMAT--SSCPT=(  143,  474)
OM=
           -6.473E-02 -5.238E-02  9.965E-01  9.976E-01 -2.883E-02  6.328E-02  2.541E-02  9.982E-01  5.412E-02
RS=
           -1.624E+06 -1.045E+05 -8.401E+04
SPICE data HAS BEEN UPDATED

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  400.00000
PLANET CENTER MOVED TO SAMPLE=  400.00000

GEOMA RECENTERING PARAMETERS WRITTEN
geoma INP=(/project/test_work/testdata/mipl/gll/venus.img,g.img) OUT=f.img
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS HALFWORD
local (pl,ps) real
farenc INP=f.img begin=90 pcl=pl pcs=ps  +
 radius=6138.2 'auto below=500 dnth=500 range=400. initcen=(399.,399.)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is object space
project=GLL
SYSTEM LABEL SAYS INPUT IS HALFWORD
GETLABCON: warning ind=          1

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     1629971.9
sub s/c latitude =   -2.9542966
sub s/c longitude =    176.32004
north angle =   178.54636
scale (km/pxl) =   65.616798
focal length (mm) =   1501.0389
equatorial radius (km) =    6138.2002
polar radius (km) =    6138.2002
sub solar latitude = -2.5792704
sub solar longitude =  129.47615
planet center line=  399.00000
planet center sample=  399.00000
PROJECTED PLANET RADIUS (pxls) =  370.91211
Projected sun angle (deg)=  269.31094
Peak DN value=        4735
ACTIVITY RESET TO         676
BELOW RESET TO         676
DNTHRESH RESET TO         676
AVE ACTIVITY  3015.6226
      + -     809.46527

Initial number of points located=         739
Points remaining after de-cluster =        369
RANGEN::: Random Seed value of 1122328591 adjusted to 15032335

Constrained annealing circle fit object space:
Line of center=  399.59128
Samp of center=  399.83826
Radial fit mean residual= 0.23426819

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  399.94708
 SAMPLE POSITION OF PLANET CENTER  399.87527
SIGMA OF RADIUS ERRORS  0.15884058
LARGEST RADIUS ERROR FOR ANY POINT 0.66893315
NUMBER OF POINTS USED FOR FIT             369
disp (pl,ps)

pl=399.94708252
ps=399.875274658

list  f.img LINC=80 SINC=80
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:TASK      User:RTO313    Date_Time:Thu May 24 14:30:43 1990
 Task:GEOMA     User:lwk       Date_Time:Mon Jul 25 14:56:30 2005
     Samp       1    81   161   241   321   401   481   561   641   721
   Line

    321       113  4188  4454  3777  3389  2783  2111  1221   184    38
    401       112  4268  4072  3784  3578  2987  2239  1283   262    43
    481        74  4184  4697  4234  3441  2930  2036  1199   182    43
    561        74  3604  4261  4321  3527  2565  1902   932    42     2
    641    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
    721    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
farenc /project/test_work/testdata/mipl/gll/s0349632122.1  'remote
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=GLL
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     665638.56
sub s/c latitude =   -8.0669250
sub s/c longitude =    155.55841
north angle =   194.30228
scale (km/pxl) =   32.808399
focal length (mm) =   1501.0389
equatorial radius (km) =    2632.3450
polar radius (km) =    2632.3501
sub solar latitude = -1.8733770
sub solar longitude =  125.66879
planet center line=  204.34587
planet center sample=  236.88409
Projected major axis (pxls)=  194.75325
Projected minor axis (pxls)=  194.75362
Computed offset of planet from ellipse center
LINE OFFSET -0.000E+00
SAMPLE OFFSET  0.000E+00
Projected sun angle (deg)=  274.14514
AVE ACTIVITY  266.67990
      + -     146.39404

Initial number of points located=         891
Points remaining after de-cluster =        521
RANGEN::: Random Seed value of 1122328592 adjusted to 15032336

Constrained annealing ellipse fit object space:
Line of center=  203.97472
Samp of center=  236.85432
Radial fit mean residual= 0.20064951

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  203.97820
 SAMPLE POSITION OF PLANET CENTER  236.87457
SIGMA OF RADIUS ERRORS  0.15499631
LARGEST RADIUS ERROR FOR ANY POINT 0.75005126
NUMBER OF POINTS USED FOR FIT             521
if ($syschar(1) = "UNIX")
  farenc INP=/project/cassini_work/spice_examples/n1354897340.1  +
    OUT=g.img 'auto initcen=(138,872)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image is image space
project=CASSI
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     2.359E+07
sub s/c latitude =    3.5729308
sub s/c longitude =    47.781807
north angle =   359.91000
scale (km/pxl) =   83.333336
focal length (mm) =   2000.0000
equatorial radius (km) =    71492.000
polar radius (km) =    66854.000
sub solar latitude =  2.9425383
sub solar longitude =  40.735386
planet center line=  138.00000
planet center sample=  872.00000
Projected major axis (pxls)=  505.04163
Projected minor axis (pxls)=  472.40900
Computed offset of planet from ellipse center
LINE OFFSET -0.01195199
SAMPLE OFFSET -1.877E-05
Projected sun angle (deg)=  94.823608
Peak DN value=         202
ACTIVITY RESET TO          28
BELOW RESET TO          28
DNTHRESH RESET TO          28
AVE ACTIVITY  78.808334
      + -     21.691114

Initial number of points located=         235
Points remaining after de-cluster =        176
RANGEN::: Random Seed value of 1122328594 adjusted to 15032338
No convergence,residual=  2.6875916
RANGEN::: Random Seed value of 1122328594 adjusted to 15032338
No convergence,residual=  2.6876647
RANGEN::: Random Seed value of 1122328594 adjusted to 15032338

Constrained annealing ellipse fit object space:
Line of center=  139.03922
Samp of center=  839.53986
Radial fit mean residual= 0.18761444

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  138.78581
 SAMPLE POSITION OF PLANET CENTER  840.98993
SIGMA OF RADIUS ERRORS  0.14513226
LARGEST RADIUS ERROR FOR ANY POINT 0.54348415
NUMBER OF POINTS USED FOR FIT             117

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  512.00000
PLANET CENTER MOVED TO SAMPLE=  512.00000

GEOMA RECENTERING PARAMETERS WRITTEN
  geoma INP=(/project/cassini_work/spice_examples/n1354897340.1,g.img) OUT=f.img
Beginning VICAR task geoma
GEOMA version 13-Nov-95
SYSTEM LABEL SAYS INPUT IS BYTE
end-if
farenc /project/test_work/testdata/mipl/cassini/farenc/ENC_S12_SQ10B.b166b (p,c)  +
 solar=(-21.08 243.1) 'auto clus=(10,2)
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image has perspective map label
xlget: status=         -44
Failure to get solar latitude
xlget: status=         -44
Failure to get solar longitude
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     36444.684
sub s/c latitude =   -47.858871
sub s/c longitude =    195.20094
north angle =   67.290337
scale (km/pxl) =   18.779343
focal length (mm) =   426.00000
equatorial radius (km) =    251.79999
polar radius (km) =    244.60001
sub solar latitude = -21.080000
sub solar longitude =  243.10001
planet center line=  52.764778
planet center sample=  43.763611
Projected major axis (pxls)=  55.274082
Projected minor axis (pxls)=  54.568226
Computed offset of planet from ellipse center
LINE OFFSET 0.00413486
SAMPLE OFFSET -0.00988003
Projected sun angle (deg)=  355.10956
Peak DN value=         251
ACTIVITY RESET TO          35
BELOW RESET TO          35
DNTHRESH RESET TO          35
AVE ACTIVITY  279.41510
      + -     228.22240

Initial number of points located=         106
Points remaining after de-cluster =         71
RANGEN::: Random Seed value of 1122328595 adjusted to 15032339
No convergence,residual=  1.7267609
RANGEN::: Random Seed value of 1122328595 adjusted to 15032339
No convergence,residual=  1.7267779
RANGEN::: Random Seed value of 1122328595 adjusted to 15032339
No convergence,residual=  1.7267779
RANGEN::: Random Seed value of 1122328595 adjusted to 15032339

Constrained annealing ellipse fit object space:
Line of center=  52.277367
Samp of center=  43.274502
Radial fit mean residual= 0.83791006

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  52.960331
 SAMPLE POSITION OF PLANET CENTER  43.626186
SIGMA OF RADIUS ERRORS  0.26886171
LARGEST RADIUS ERROR FOR ANY POINT 0.96062738
NUMBER OF POINTS USED FOR FIT              35

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  25.000000
PLANET CENTER MOVED TO SAMPLE=  49.000000

GEOMA RECENTERING PARAMETERS WRITTEN
farenc /project/test_work/testdata/mipl/cassini/farenc/ENC_S12_SQ10B.b166b (p,c)  +
 solar=(-21.08 243.1) 'auto clus=(10,2) perc=0.1
Beginning VICAR task farenc
FARENC version 26-Dec-2001
input image has perspective map label
xlget: status=         -44
Failure to get solar latitude
xlget: status=         -44
Failure to get solar longitude
SYSTEM LABEL SAYS INPUT IS BYTE

FINAL DATA FROM SEDR AND PARAMETERS ARE:
s/c range (km) =     36444.684
sub s/c latitude =   -47.858871
sub s/c longitude =    195.20094
north angle =   67.290337
scale (km/pxl) =   18.779343
focal length (mm) =   426.00000
equatorial radius (km) =    251.79999
polar radius (km) =    244.60001
sub solar latitude = -21.080000
sub solar longitude =  243.10001
planet center line=  52.764778
planet center sample=  43.763611
Projected major axis (pxls)=  55.274082
Projected minor axis (pxls)=  54.568226
Computed offset of planet from ellipse center
LINE OFFSET 0.00413486
SAMPLE OFFSET -0.00988003
Projected sun angle (deg)=  355.10956
Peak DN value=         254
ACTIVITY RESET TO          36
BELOW RESET TO          36
DNTHRESH RESET TO          36
AVE ACTIVITY  279.74527
      + -     227.17862

Initial number of points located=         106
Points remaining after de-cluster =         71
RANGEN::: Random Seed value of 1122328595 adjusted to 15032339
No convergence,residual=  1.7501497
RANGEN::: Random Seed value of 1122328595 adjusted to 15032339

Constrained annealing ellipse fit object space:
Line of center=  52.854515
Samp of center=  43.157982
Radial fit mean residual= 0.94472522

LEAST SQUARES ELLIPSE SOLUTION, OBJECT SPACE:
 LINE   POSITION OF PLANET CENTER  53.052616
 SAMPLE POSITION OF PLANET CENTER  43.605724
SIGMA OF RADIUS ERRORS  0.27185598
LARGEST RADIUS ERROR FOR ANY POINT 0.97934294
NUMBER OF POINTS USED FOR FIT              35

After the geom the new planet center will be at:
PLANET CENTER MOVED TO LINE  =  25.000000
PLANET CENTER MOVED TO SAMPLE=  49.000000

GEOMA RECENTERING PARAMETERS WRITTEN
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
$ Return
$!#############################################################################
