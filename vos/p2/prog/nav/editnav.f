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
