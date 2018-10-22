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
