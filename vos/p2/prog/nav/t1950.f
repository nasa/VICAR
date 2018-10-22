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
