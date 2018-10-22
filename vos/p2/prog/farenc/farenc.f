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

