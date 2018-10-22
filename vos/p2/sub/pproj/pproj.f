      SUBROUTINE PPROJ(DATA,LINE,SAMP,LAT,LON,IMODE,ILAT,RADIUS,SRANGE,
     & IND)
c   2Aug90  lwk  added ILAT parameter
c  10Sep90  lwk  added RTANG parameter
c  27Jun91  lwk  renamed RTANG to RADIUS (& fixed it for ellipsoidal case),
c                  added SRANGE parameter
c  10dec92  lwk  used RADIUS in mode=2 to allow disabling of BOP test
c   4nov94  sxp  Made portable for UNIX: changed REAL*8 to DOUBLE PRECISION,
c                REAL*4 to REAL, and calls to MVL to calls to MVE.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION CP(3),OP(3),CPP(3),OM(3,3),RS(3)
      REAL EPS/1.E-6/
      REAL LAT,LON,LINE,SAMP,RADIUS,SRANGE,DATA(*)
      DATA RADDEG, DEGRAD / 5.72957795130823D1,
     & 1.74532925199433D-2/
C
      IND = 0
      FL = DATA(27)
      OAL = DATA(28)
      OAS = DATA(29)
      SCALE = DATA(30)
      RP = DATA(25)
      RE = DATA(26)
      E = RE/RP
      E2 = E*E
      CALL MVE(8,9,DATA,OM,1,1)
      CALL MVE(8,3,DATA(19),RS,1,1)
C
      IF(IMODE.EQ.2) GOTO 30
C     ....Here to convert (lat,lon) to (line,samp)
      RLAT = LAT*DEGRAD
      RLON = LON*DEGRAD
C          CONVERT FROM GEODETIC TO GEOCENTRIC LATITUDE
      IF (ILAT.EQ.1 .AND. ABS(LAT).NE.90.D0) RLAT=DATAN(DTAN(RLAT)/E2)
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
C          COMPUTE GEOCENTRIC RADIUS
      D1 = RP*CLAT
      D2 = RE*SLAT
      R = (RE*RP)/DSQRT(D1*D1+D2*D2)
      CP(1) = R*CLAT*DCOS(RLON) - RS(1)
      CP(2) = -R*CLAT*DSIN(RLON) - RS(2)
      CP(3) = R*SLAT - RS(3)
C
C     ....Back of planet test
      D1 = 0.
      D2 = 0.
      DO I=1,3
         D1 = D1 + CP(I)**2
         D2 = D2 + RS(I)**2
      ENDDO
      IBOP = 0
      IF (D1+R**2.GT.D2) THEN		!Point behind planet
	IF (RADIUS.GE.0.) RETURN
	IBOP = 1
      ENDIF
C
      DO 20 I=1,3
      D1 = 0.D0
      DO 10 J=1,3
   10 D1 = D1 + OM(I,J)*CP(J)
   20 CPP(I) = D1
C
      S = FL*SCALE/CPP(3)
      LINE = OAL + S*CPP(2)
      SAMP = OAS + S*CPP(1)
      IND = 1-IBOP
      RETURN
C
C     ....Here to convert (line,samp) to lat,lon)
   30 RADIUS = 0.0
      SRANGE = 0.0
      X = SAMP - OAS
      Y = LINE - OAL
      Z = FL*SCALE
C
      DO 40 I=1,3
   40 CP(I) = OM(1,I)*X + OM(2,I)*Y + OM(3,I)*Z
C
      A = CP(1)*CP(1) + CP(2)*CP(2) + E2*CP(3)*CP(3)
      B = CP(1)*RS(1) + CP(2)*RS(2) + E2*CP(3)*RS(3)
      C = RS(1)*RS(1) + RS(2)*RS(2) + E2*RS(3)*RS(3) - RE*RE
      D = B*B - A*C
      IF (D.GE.0.) GO TO 50
C
C     ... point is off planet.  Just to be nice, we will proceed assuming
C	  that the tangent radius along this line of sight is the correct 
C	  radius.  (This allows off-limb points to be plotted, for 
C	  Orthographic & Perspective projections.)
      D = 0
      GO TO 60
C
   50 IND = 1
   60 S = (-B-DSQRT(D))/A
C
C	OP is the vector from planet center to surface intercept
C	S*CP is the vector from s/c to surface intercept
      RADIUS = 0.
      SRANGE = 0.
      DO I=1,3
	OP(I) = S*CP(I) + RS(I)
	RADIUS = RADIUS + OP(I)*OP(I)
	SRANGE = SRANGE + CP(I)*CP(I)
      ENDDO
      RADIUS = SQRT(RADIUS)
      SRANGE = S*SQRT(SRANGE)
C
      X = OP(1)
      Y = OP(2)
      Z = OP(3)
      X1 = DABS(X)
      Y1 = DABS(Y)
      Z1 = DABS(Z)
      D = DSQRT(X*X+Y*Y)
      IF(D.LT.Z1*EPS) GOTO 98
      IF (ILAT.EQ.0) THEN
	LAT = DATAN(Z/D)*RADDEG		! GEOCENTRIC LAT.
      ELSE
	LAT = DATAN(E2*Z/D)*RADDEG	! GEODETIC LAT.
      ENDIF
      IF(Y1.LT.X1*EPS) GOTO 96
      IF(X1.LT.Y1*EPS) GOTO 94
      LON = 360. - DATAN2(Y,X)*RADDEG
      IF(LON.GT.360.) LON=LON-360.
      RETURN
   94 LON = 270.
      IF(Y.LT.0.) LON=90.
      RETURN
   96 LON = 0.
      IF(X.LT.0.) LON=180.
      RETURN
C
   98 LAT = 90.
      IF(Z.LT.0.) LAT=-LAT
      LON = 0.
      RETURN
      END
