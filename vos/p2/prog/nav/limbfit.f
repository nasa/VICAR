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
