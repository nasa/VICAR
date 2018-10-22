      subroutine pproj_mp(data,line,samp,lat,lon,imode,ind)
 
c DATA = standard 40-word geometry buffer (see subroutine CONVEV) for 
c       Perspective case, except that word 37 = equatorial semi-minor axis
 
c LINE,SAMP = object space location of a point
 
c LAT,LON = planetary coordinates of point
c           (radians, planetocentric Lat., West Lon.)
 
c MODE:  1 = (LAT,LON) to (LINE,SAMP)   2 = (LINE,SAMP) to (LAT,LON) 
 
c IND = return indicator.  0=success, 1=failure. (to coincide w/mp_routines)
 
c  31oct93  lwk  implemented tri-axial ellipsoid model with extra radius
c               in word 31 of DATA
c  15may94  lwk  fixed BOP test for triaxial ellipsoid using NAIF calls
c    jun96  pxa  cleaned up for use with MP routines, changed failure indicator 
c               from 0 to 1, 3rd radius from word 31 to 37 in DATA
c    jul96  pxa  replaced naif/spice calls to 'surfnm','vsep', 'vnorm', and 
c		 'halfpi' with relevant in-line code, to remove dependency of 
c		 naif/ spice routines in mp_routines
c  30jul01  lwk  fixed bug in replacement of vnorm code
 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CP(3),OP(3),CPP(3),OM(3,3),RS(3),xnorm(3),M
      REAL*8 EPS/1.E-6/
      REAL*8 U1(3),U2(3),VTEMP(3)
      REAL*4 LAT,LON,LINE,SAMP,DATA(*)
      DATA PI, RADDEG, DEGRAD / 3.141592653589793D0, 5.72957795130823D1,
     & 1.74532925199433D-2/
C
      HALFPI = PI/2.0
      IND = 1

      FL = DATA(27)	! camera focal length (mm)
      OAL = DATA(28)	! optical axis Line
      OAS = DATA(29)	! optical axis Sample
      SCALE = DATA(30)	! camera scale (pix/mm)

      RP = DATA(25)     ! polar radius
      RA = DATA(26)     ! equatorial semi-major radius
 
      RB = DATA(37)
c         check if RB is garbage, in which case assume oblate spheroid:
      IF (RB.LT.RP .OR. RB.GT.RA) RB = RA
      E1 = (RA/RP)
      E1 = E1*E1
      E2 = (RB/RP)
      E2 = E2*E2

c OM-matrix (Camera-to-Body coordinates) and RS-vector (body center to camera)
c are stored in the first 24 words of the buffer:
c      CALL MVL(DATA,OM,72)
c      CALL MVL(DATA(19),RS,24)
      call mve(1,72,data,om,1,1)
      call mve(1,24,data(19),rs,1,1)
C
      IF(IMODE.EQ.2) GOTO 30
C     ....Here to convert (lat,lon) to (line,samp)

      RLAT = LAT
      RLON = LON	
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
      CLON = DCOS(RLON)
      SLON = DSIN(RLON)
C          COMPUTE GEOCENTRIC RADIUS
      D1 = RP*RP*CLAT*CLAT
      RB2 = RB*RB
      RA2 = RA*RA
      CLN2 = CLON*CLON
      SLN2 = SLON*SLON
      SLT2 = SLAT*SLAT
      R = (RA*RB*RP)/DSQRT(D1*RB2*CLN2+D1*RA2*SLN2+RA2*RB2*SLT2)
      CP(1) = R*CLAT*CLON - RS(1)
      CP(2) = -R*CLAT*SLON - RS(2)
      CP(3) = R*SLAT - RS(3)
C
c  BOP test for triaxial ellipsoid
c  *******************************************************************
c  The following code replaces the functionality of 'surfnm','vsep',and
c  halfpi() routines. This needed to be done to remove spice/naif
c  dependencies from mp_routines
c  ******************************************************************
c      call surfnm( ra, rb, rp, op, xnorm)
 
c      note CP is -1 * vector from P to C, so reverse criterion:
c      ibop = 0
c      if ( vsep(cp,xnorm) .lt. halfpi()) ibop = 1

      op(1) = r*clat*clon 
      op(2) = -r*clat*slon 
      op(3) = r*slat 

      M=MIN(RA,RB,RP)
      A1=M/RA
      B1=M/RB
      C1=M/RP

      XNORM(1) = OP(1)*(A1*A1)
      XNORM(2) = OP(2)*(B1*B1)
      XNORM(3) = OP(3)*(C1*C1)

      VMAX1 = MAX(DABS(XNORM(1)),DABS(XNORM(2)),DABS(XNORM(3)))
      IF (VMAX1 .EQ. 0.) THEN
	VNORM1 = 0.
      ELSE
	VNORM1 = VMAX1*DSQRT((XNORM(1)/VMAX1)**2 + (XNORM(2)/VMAX1)**2 +
     &			(XNORM(3)/VMAX1)**2)
      END IF
      VMAG1 = VNORM1
      IF (VMAG1 .GT. 0.) THEN
	XNORM(1) = XNORM(1)/VMAG1
        XNORM(2) = XNORM(2)/VMAG1
        XNORM(3) = XNORM(3)/VMAG1
      ELSE
	XNORM(1) = 0.
	XNORM(2) = 0.
	XNORM(3) = 0.
      END IF
      ibop = 0
c     Now compute vsep(cp,xnorm), and assign it a value
      VMAX2 = MAX(DABS(CP(1)),DABS(CP(2)),DABS(CP(3)))
      IF (VMAX2 .EQ. 0.) THEN
	VNORM2 = 0.
      ELSE
	VNORM2 = VMAX2*DSQRT((CP(1)/VMAX2)**2 + (CP(2)/VMAX2)**2 +
     &			(CP(3)/VMAX2)**2)
      END IF
      VMAG2 = VNORM2
      IF (VMAG2 .GT. 0.) THEN
	U1(1) = CP(1)/VMAG2
	U1(2) = CP(2)/VMAG2
	U1(3) = CP(3)/VMAG2
      ELSE
	U1(1) = 0.
	U1(2) = 0.
	U1(3) = 0.
      END IF
      IF (VMAG2 .EQ. 0.) THEN
	VSEP1 = 0.
      END IF

      VMAX3 = MAX(DABS(XNORM(1)),DABS(XNORM(2)),DABS(XNORM(3)))
      IF (VMAX3 .EQ. 0.) THEN
	VNORM3 = 0.
      ELSE
	VNORM3 = VMAX3*DSQRT((XNORM(1)/VMAX3)**2 + (XNORM(2)/VMAX3)**2 +
     &			(XNORM(3)/VMAX3)**2)
      ENDIF
      VMAG3 = VNORM3
      IF (VMAG3 .GT. 0.) THEN
	U2(1) = XNORM(1)/VMAG3
	U2(2) = XNORM(2)/VMAG3
	U2(3) = XNORM(3)/VMAG3
      ELSE
	U2(1) = 0.
	U2(2) = 0.
	U2(3) = 0.
      END IF
      IF (VMAG3 .EQ. 0.) THEN
	VSEP1 = 0.
      END IF
      VDOT1 = U1(1)*U2(1) + U1(2)*U2(2) + U1(3)*U2(3)
      IF (VDOT1 .EQ. 0.) THEN
	VSEP1 = PI/2.0
      ELSE
        VTEMP(1) = U1(1) - U2(1)
        VTEMP(2) = U1(2) - U2(2)
        VTEMP(3) = U1(3) - U2(3)
        VMAX4 = MAX(DABS(VTEMP(1)),DABS(VTEMP(2)),DABS(VTEMP(3)))
        IF (VMAX4 .EQ. 0.) THEN
          VNORM4 = 0.
        ELSE
          VNORM4 = VMAX4*DSQRT((VTEMP(1)/VMAX4)**2 + (VTEMP(2)/VMAX4)**2 +
     &                         (VTEMP(3)/VMAX4)**2)
        END IF
        IF (VDOT1 .GT. 0.) THEN
	  VSEP1 = 2.0 * ASIN(0.5 * VNORM4)
        ELSE 				! VDOT1 .LT. 0.
	  VSEP1 = PI - 2.0*ASIN(0.5*VNORM4)
        END IF
      END IF
      IF (VSEP1 .LT. HALFPI) IBOP=1 
c
c  *****************************************************************
c 
      DO 20 I=1,3
      D1 = 0.D0
      DO 10 J=1,3
   10 D1 = D1 + OM(I,J)*CP(J)
   20 CPP(I) = D1
C
      S = FL*SCALE/CPP(3)
      LINE = OAL + S*CPP(2)
      SAMP = OAS + S*CPP(1)
      IND = IBOP
      RETURN
C
C     ....Here to convert (line,samp) to lat,lon)
 
   30 X = SAMP - OAS

      Y = LINE - OAL
      Z = FL*SCALE
C
      DO 40 I=1,3
   40 CP(I) = OM(1,I)*X + OM(2,I)*Y + OM(3,I)*Z
C
      A = E2*CP(1)*CP(1) + E1*CP(2)*CP(2) + E1*E2*CP(3)*CP(3)
      B = E2*CP(1)*RS(1) + E1*CP(2)*RS(2) + E1*E2*CP(3)*RS(3)
      C = E2*RS(1)*RS(1) + E1*RS(2)*RS(2) + E1*E2*RS(3)*RS(3)
     &   - E1*E2*RP*RP
      D = B*B - A*C
      IF (D.lt.0.) return
C
      IND = 0
      S = (-B-DSQRT(D))/A
C
      DO I=1,3
        OP(I) = S*CP(I) + RS(I)
      ENDDO
C
      X = OP(1)
      Y = OP(2)
      Z = OP(3)
      X1 = DABS(X)
      Y1 = DABS(Y)
      Z1 = DABS(Z)
      D = DSQRT(X*X+Y*Y)
      IF(D.LT.Z1*EPS) GOTO 98
      LAT = DATAN(Z/D)*RADDEG         ! GEOCENTRIC LAT.
      LAT = LAT*DEGRAD
c      ENDIF
      IF(Y1.LT.X1*EPS) GOTO 96
      IF(X1.LT.Y1*EPS) GOTO 94
      LON = 360. - DATAN2(Y,X)*RADDEG
      LON = LON*DEGRAD
c      IF(LON.GT.360.) LON=LON-360.
      IF(LON.GT.(2.*PI)) LON=LON-(2.*PI)
      RETURN
C
   94 LON = 270.
      LON = LON*DEGRAD
c      IF(Y.LT.0.) LON=90.
      IF(Y.LT.0) LON=PI/2.
      RETURN
   96 LON = 0.
      LON = LON*DEGRAD
c      IF(X.LT.0.) LON=180.
      IF(X.LT.0.) LON=PI
      RETURN
C
   98 LAT = 90.
      LAT = LAT*DEGRAD
      IF(Z.LT.0.) LAT=-LAT
      LON = 0.
      LON = LON*DEGRAD
      RETURN
      END

