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
