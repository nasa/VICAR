      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM RINGORBS --Write orbital elements for each planetary ring
C system to a file.
C
C	RINGORBS  ORBITAL.DAT
C
C  5-MAY-95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
      SUBROUTINE MAIN44
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER*4 OUNIT,PLANET,IDATE
      INTEGER*2 FLAG
      CHARACTER*16 DATE

      COMMON/COE/OE(110)	!Orbital elements
      COMMON/CONST/PI,DTR,RTD

      CALL IFMESSAGE('RINGORBS version 15-OCT-02')
      PI = DACOS(-1.0D0)
      DTR = PI/180.D0
      RTD = 180.D0/PI

      CALL XVUNIT(OUNIT,'OUT',1,IND,' ')
      CALL XVSIGNAL(OUNIT,IND,.TRUE.,' ')
      CALL XVOPEN(OUNIT,IND,'OP','WRITE','U_NL',4,'U_NS',110,
     &	'U_FORMAT','DOUB','O_FORMAT','DOUB',
     &	'OPEN_ACT','SA','IO_ACT','SA',' ')

      FLAG=1
      CALL DATFMT(FLAG,date,idate)
      CALL XLADD(OUNIT,'HISTORY','DATE',DATE,IND,'FORMAT','STRING',' ')
      OE(1) = DBLE(idate)

      DO PLANET=5,8
         DO i=2,110
           OE(i) = 0.D0
         END DO
         CALL GETRINGS(PLANET,oe)
         CALL XVWRIT(OUNIT,OE,ind,' ')
      ENDDO

      RETURN
      END
C Get planet orientation and ring orbital elements for planet
C
      SUBROUTINE GETRINGS(IPLANET,OE)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 IPLANET
      REAL*8 OE(111), INCL	      !Reclen=8*111

      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ
      COMMON/CONST/PI,DTR,RTD

      IF (IPLANET.EQ.5) THEN
         NRINGS = 1
CBTC
CBTC RA & DEC of Jupiter's polar axis in B1950 converted from J2000
CBTC
         ALPHAp = 267.9927D0*DTR	!J2000:  268.05
         DELTAp = 64.4996D0*DTR		!J2000:   64.49
CBTC         ALPHAp = 0.D0
CBTC         DELTAp = 0.D0
         THETA = 0.D0*DTR
         ZETAZ = 0.D0*DTR
      ENDIF

C  For epoch of data see RINGDATA subroutines

      IF (IPLANET.EQ.6) THEN		!Set up Saturn ring system
        NRINGS = 13
        ALPHAp = 38.409D0*DTR		!Right-ascension and declination
        DELTAp = 83.324D0*DTR		!of Saturn's north pole in EME1950
        THETA = 0.0D0*DTR		!Orientation of the Earth's
        ZETAZ = 0.0D0*DTR		!north pole at 1950 in EME1950
      ENDIF

      IF (IPLANET.EQ.7) THEN		!Set up Uranus ring system
        NRINGS = 11
        ALPHAp = 76.5969D0*DTR !+/-.0034 Right-ascension and declination
        DELTAp = 15.1117D0*DTR !+/-.0033 of Uranus' north pole.
        THETA = 0.0D0*DTR		!Orientation of the Earth's
        ZETAZ = 0.0D0*DTR		!north pole at T0.
      ENDIF

      IF (IPLANET.EQ.8) THEN		!Set up Neptune ring system
        NRINGS = 11
        ALPHAp = 298.852D0*DTR ! +/-?   Right-ascension and declination
        DELTAp =  42.421D0*DTR ! +/-?   Neptunes' north pole.
        THETA = 0.D0*DTR		!Orientation of the Earth's
        ZETAZ = 0.D0*DTR		!north pole at 1950.
      ENDIF

      OE(2)=ALPHAp
      OE(3)=DELTAp
      OE(4)=THETA
      OE(5)=ZETAZ

      DO J=1,NRINGS
         CALL RINGDATA(IPLANET,J,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &           dOMEGA_dt,dw_dt)
         OE(7*(j-1)+6)=SMAA
         OE(7*(j-1)+7)=ECC
         OE(7*(j-1)+8)=INCL
         OE(7*(j-1)+9)=OMEGAZ
         OE(7*(j-1)+10)=PIZERO
         OE(7*(j-1)+11)=DOMEGA_DT
         OE(7*(j-1)+12)=DW_DT
      ENDDO

      RETURN
      END
C Given ring-id IRING, return orbital elements.
C
      SUBROUTINE RINGDATA(IPLANET,IRING,smaa,ecc,incl,omegaz,pizero,
     &      domega_dt,dw_dt)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 INCL
      COMMON/CONST/PI,DTR,RTD

      IF (IPLANET.EQ.5) THEN
         SMAA = 0.D0
         ECC = 0.D0
         INCL = 0.D0
         OMEGAZ = 0.D0
         PIZERO = 0.D0
         dOMEGA_dt = 0.D0
         dw_dt = 0.D0
      ELSE IF (IPLANET.EQ.6) THEN
         CALL SRINGDATA(IRING,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &      dOMEGA_dt,dw_dt)
      ELSE IF (IPLANET.EQ.7) THEN
         CALL URINGDATA(IRING,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &      dOMEGA_dt,dw_dt)
      ELSE
         CALL NRINGDATA(IRING,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &      dOMEGA_dt,dw_dt)
      END IF
C           Convert all angles to radians
      INCL = INCL*DTR
      OMEGAZ = OMEGAZ*DTR
      PIZERO = PIZERO*DTR
      dOMEGA_dt = dOMEGA_dt*DTR
      dw_dt = dw_dt*DTR
      RETURN
      END
C SUBROUTINE SRINGDATA
C Orbital data from Carolyn Porco circa Dec 1987.
C
C Saturn:  Carolyn's epoch similar to French's except intersection of
C          Saturns equator 1950 with Earths equator at EME1950
C          Carolyn's epoch starts at 23:46 UT, 11/12/80 + light travel time
C          to earth = Earth observed time of Voyager 1 closest approch
C
C Values for IRING: 1=planet, 2=F, 3=Outer Keeler gap, 4=Outer Encke gap,
C                   5=Inner A ring edge, 6=Cassini inner edge of outer ring,
C                   7=Cassini inner edge of 4th outer ring, 8=Outer B feature,
C                   9=Inner B ring edge, 10=Outer Maxwell gap,
C                   11=Mid C ring feature, 12=Outer Titan gap,
C                   13=Inner C ring edge
      SUBROUTINE SRINGDATA(IRING,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &      dOMEGA_dt,dw_dt)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 INCL

      REAL*4  RA(13),  ECCS(13), INCLI(13), OMEG0(13), w0(13)
      REAL*4  DOMDT(13), dwdt(13)

      DATA RA/0.0,140185.,136531.,133754.,122056.,120047.6,118975.,
     &        117118.,92006.,87614.,82046.4,77925.,74493./
      DATA ECCS/0.0,2.66,11*0.0/
      DATA INCLI/13*0.0/
      DATA OMEG0/13*0.0/
      DATA w0/0.0,223.,11*0.0/
      DATA DOMDT/13*0.0/
      DATA dwdt/0.0,2.7016,11*0.0/

      SMAA      =    RA(IRING)		!Semi-major axis (km)
      ECC       =  ECCS(IRING)*0.001D0  !Eccentricity
      INCL      = INCLI(IRING)		!Inclination
      OMEGAZ    = OMEG0(IRING)		!Longitude of ascending node (deg)
      PIZERO    =    w0(IRING)		!Longitude of periapse (deg)
      dOMEGA_dt = DOMDT(IRING)		!Nodal regression rate (deg/day)
      dw_dt     =  dwdt(IRING)		!Apsidal precession rate (deg/day)

      RETURN
      END
C SUBROUTINE URINGDATA
C Orbital data from Uranian Rings orbit solution Draft manuscript
C December 1988.  Values in parentheses were set to zero.
C
C Uranus:  French's epoch starts at 20:00 UT, 3/10/77 (Earth time)
C
C Values for IRING: 1=planet, 2=6, 3=5, 4=4, 5=alpha, 6=beta, 7=eta,
C                   8=gamma, 9=delta, 10=lambda (1986U1R), 11=epsilon
      SUBROUTINE URINGDATA(IRING,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &      dOMEGA_dt,dw_dt)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 INCL

      REAL*4  RA(11), ECCS(11), INCLI(11), OMEG0(11), w0(11), DOMDT(11)
      REAL*4  dwdt(11)

      DATA RA/0.0,41837.15,42234.82,42570.91,44718.45,45661.03,
     &        47175.91,47626.87,48300.12,50023.94,51149.32/
      DATA ECCS/0.0,1.013,1.899,1.059,0.761,0.442,
     &          0.0,0.109,0.0,0.0,7.936/
      DATA INCLI/0.0,0.0616,0.0536,0.0323,0.0152,0.0051,
     &           0.0,0.0,0.0011,0.0,0.0/
      DATA OMEG0/0.0,12.12,286.57,89.26,63.08,310.05,
     &           0.0,0.0,260.7,0.0,0.0/
      DATA w0/0.0,242.80,170.31,127.28,333.24,224.88,
     &        0.0,132.1,216.7,0.0,214.97/
      DATA DOMDT/0.0,-2.75629,-2.66604,-2.59271,-2.18326,-2.02778,
     &           0.0, 0.0, 0.0, 0.0, 0.0/
      DATA dwdt/0.0,2.76156,2.67151,2.59816,2.18531,2.03083,
     &          0.0, 1.75075, 0.0, 0.0, 1.36325/

      SMAA      =    RA(IRING)		!Semi-major axis (km)
      ECC       =  ECCS(IRING)*0.001D0  !Eccentricity
      INCL      = INCLI(IRING)		!Inclination
      OMEGAZ    = OMEG0(IRING)		!Longitude of ascending node (deg)
      PIZERO    =    w0(IRING)		!Longitude of periapse (deg)
      dOMEGA_dt = DOMDT(IRING)		!Nodal regression rate (deg/day)
      dw_dt     =  dwdt(IRING)		!Apsidal precession rate (deg/day)

      RETURN
      END
C Orbital data for trial rings for Neptune
C Neptune: The proposed epoch follows...
C          Epoch = 0h ET ( = UT + 56.184 sec), SCET, Aug 25 1989
C                  (determined by NAV team)
C  1=planet 2=triton stablized polar ring (50,000 km) 3=polar (100,000 km)
C  4=polar (150,000 km) 5=1989N5  6=1989N3  7=1989N4  8=1989N2
C  9=1989N1  10=Triton  11=Nereid
C
      SUBROUTINE NRINGDATA(IRING,SMAA,ECC,INCL,OMEGAZ,PIZERO,
     &      dOMEGA_dt,dw_dt)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 INCL
      REAL*4  dwdt8(11), RA8(11)  ! A dynamic
      REAL*4  ECCS8(11), INCLI8(11), OMEG08(11), w08(11), DOMDT8(11)

      REAL*8  ALPHAt ! Alpha, Dec of Triton's orbit pole
      REAL*8  DELTAt ! at Epoch (+/- 0.02 for both A,D)
      REAL*8  ALPHAn ! Alpha, Dec of Nereids's orbit pole
      REAL*8  DELTAn ! at Epoch (+/- 0.01 for both A,D)

      COMMON/CMAP/ALPHAp,DELTAp,THETA,ZETAZ
      COMMON/CONST/PI,DTR,RTD

      DATA RA8/0.0,5.E4,1.E5,1.5E5,49200.,52527.26,61954.79,
     &         73550.47,117650.29,354775.,5505484./  ! A dynamic
      DATA ECCS8/0.0,0.0,0.0,0.0,0.0,1.163,0.734,2.037,.677,0.0,753.85/
      DATA INCLI8/0.0,90.,90.,90.,0.0,
     &            .4606,.3770,.1552,.1325,157.727,29.0835/
      DATA OMEG08/0.0,0.0,0.0,0.0,0.0,189.7,179.3,170.0,338.4,0.0,0.0/
      DATA w08/0.0,0.0,0.0,0.0,0.0,
     &             132.9,135.9,140.0,125.6,0.0,18.8732/
      DATA DOMDT8/11*0.0/
      DATA dwdt8/11*0.0/

      DATA  ALPHAt/115.65/ ! Alpha, Dec of Triton's orbit pole
      DATA  DELTAt/-20.30/ ! at Epoch (+/- 0.02 for both A,D)
      DATA  ALPHAn/262.82/ ! Alpha, Dec of Nereids's orbit pole
      DATA  DELTAn/ 62.55/ ! at Epoch (+/- 0.01 for both A,D)

      SMAA      =    RA8(IRING)  !Semi-major axis (km)
      ECC       =  ECCS8(IRING)*0.001D0!Eccentricity
      INCL      = INCLI8(IRING)	 !Inclination
      OMEGAZ    = OMEG08(IRING)	 !Longitude of ascending node (deg)
C Special cases follow...
      IF(INCL.EQ.90.) THEN ! calculate polar ring nodal line position
        CALL NPOLARNODE(ALPHAp,DELTAp,THETA,ZETAZ,ALPHAt*DTR,DELTAt*DTR
     &  ,OMEGAZ)
        OMEGAZ = OMEGAZ/DTR ! NRINGDATA routine returns degrees
      ELSE IF (IRING.EQ.10) THEN !Triton
        CALL NPOLARNODE(ALPHAp,DELTAp,THETA,ZETAZ,ALPHAt*DTR,DELTAt*DTR
     &  ,OMEGAZ)
        OMEGAZ = (OMEGAZ-PI/2.D0)/DTR 
      ELSE IF (IRING.EQ.11) THEN !Nereid
        CALL NPOLARNODE(ALPHAp,DELTAp,THETA,ZETAZ,ALPHAn*DTR,DELTAn*DTR
     &  ,OMEGAZ)
        OMEGAZ = (OMEGAZ-PI/2.D0)/DTR
      ENDIF
      PIZERO    =    w08(IRING)	 !Longitude of periapse (deg)
C Compute precession, regression...
      IF (IRING.GT.5) THEN
         CALL NRATES(SMAA,ECC,dOMEGA_dt,dw_dt)
      ELSE
         dOMEGA_dt = DOMDT8(IRING)	 !Nodal regression rate (deg/day)
         dw_dt     =  dwdt8(IRING)	 !Apsidal precession rate (deg/day)
      ENDIF
      RETURN
      END
C
      SUBROUTINE NRATES(Rs,RE,dO,dw)
      IMPLICIT DOUBLE PRECISION (A-Z)
C
      REAL*8 R_cb, GM, J2, J4, J6

      DATA R_cb/25225.0/
      DATA GM/6836937.909/
      DATA J2/0.003384825/
      DATA J4/0.0/
      DATA J6/0.0/

      SCALE = R_cb/Rs
      SCALE_2 = SCALE   * SCALE
      SCALE_4 = SCALE_2 * SCALE_2
      SCALE_6 = SCALE_2 * SCALE_4
C
C	CALCULATE N 
C
      N_SQRD = 1.0 + SCALE_2 * J2 *  3.0 /  2.0 * (1. + 2.*RE*RE)
     #	           - SCALE_4 * J4 * 15.0 /  8.0
     #	           + SCALE_6 * J6 * 35.0 / 16.0

      N_SQRD = N_SQRD * GM/Rs/Rs/Rs
      N = SQRT(N_SQRD)
C
C	CALCULATE KAPA
C
      KAPA_SQRD = 1.0 - SCALE_2 * J2 *   3.0 /  2.0 * (1. + 2.*RE*RE)
     #	              + SCALE_4 * J4 *  45.0 /  8.0
     #	              - SCALE_6 * J6 * 175.0 / 16.0

      KAPA_SQRD = KAPA_SQRD * GM/Rs/Rs/Rs
      KAPA = SQRT(KAPA_SQRD)
C
C	CALCULATE Nu 
C
      Nu_SQRD = 1.0 + SCALE_2 * J2 *   9.0 /  2.0 * (1. + 2.*RE*RE)
     #	            - SCALE_4 * J4 *  75.0 /  8.0
     #	            + SCALE_6 * J6 * 245.0 / 16.0

      Nu_SQRD = Nu_SQRD * GM/Rs/Rs/Rs
      Nu = SQRT(Nu_SQRD)

      RTOD = 90.0 / DASIN(1.D0)
      FACT = RTOD*86400.
      dO = FACT*(N - Nu)  ! Degrees per day
      dw = FACT*(N - KAPA)
      RETURN
      END
C Calculate ascending node of Triton's orbit plane on Neptune's equator plane
C This equals north pole of polar ring plane... 
C Thus ascending node of polar ring (OMEGAZ) = Longitude of Polar pole + Pi/2
C Source: C. Porco 6/89
      SUBROUTINE NPOLARNODE(ALPHAp,DELTAp,THETA,ZETAZ,ALPHAt,DELTAt
     &,OMEGAZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME0(3,3),TO(3)
      COMMON/CONST/PI,DTR,RTD

      CALL ME0CALC(ALPHAp,DELTAp,ME0)	!Get planet's ME-matrix
C     ....Calculate longitude PHIp
      A =  DSIN(THETA)*DCOS(ZETAZ)  ! EME1950 x,y,z of Ep
      B = -DSIN(THETA)*DSIN(ZETAZ)
      C =  DCOS(THETA)
      D =  DCOS(DELTAp)*DCOS(ALPHAp)  ! EME1950 x,y,z of Np
      E =  DCOS(DELTAp)*DSIN(ALPHAp)
      F =  DSIN(DELTAp)
      X = B*F - C*E  !Node of Neptune equator on Earth equator in EME1950
      Y = C*D - A*F
      Z = A*E - B*D
      PHIp = DASIN(Z/DSQRT(X**2+Y**2+Z**2)/DCOS(DELTAp)) ! Amount meridian is
                                                         ! off from EME50 node

      A =  DCOS(DELTAt)*DCOS(ALPHAt)  ! EME1950 x,y,z of TOp
      B =  DCOS(DELTAt)*DSIN(ALPHAt)
      C =  DSIN(DELTAt)
      TO(1) = E*C - F*B  !TO node on Neptune equator in EME1950
      TO(2) = F*A - D*C
      TO(3) = D*B - E*A

      CALL FROMEME(ME0,TO,RLAT,RLON) ! Find longitude in Neptune co-ord.
      OMEGAZ = RLON + PI/2.D0 - PHIp ! Ascending node of polar ring plane
      RETURN
      END
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
C Given a unit vector in EME50 coordinates, compute the latitude-longitude
C coordinates...
C
C Inputs: ME matrix
C         P = unit vector
C Outputs: RLAT,RLON
C
      SUBROUTINE FROMEME(ME,P,RLAT,RLON)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ME(3,3),P(3)
      COMMON/CONST/PI,DTR,RTD

      RLAT = DASIN(DOT(P,ME(1,3)))			!RLAT = P o N
      RLON = DATAN2(DOT(P,ME(1,2)),DOT(P,ME(1,1)))
      RLON = DMOD(RLON+2.0D0*PI,2.0D0*PI)
      RETURN
      END
C Return the dot product of two vectors...
C
      FUNCTION DOT(A,B)
      REAL*8 DOT,A(3),B(3)
      DOT = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
      RETURN
      END
