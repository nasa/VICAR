CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given (lat,lon) of a point on target and GETSPICE95 navigation data,
C caluclate phase, incidence, and emission angles.
C
      SUBROUTINE LIGHTING(BUF,LAT,LON,phase,incidence,emission)
      IMPLICIT NONE
      REAL*8 BUF(100)		!GETSPICE95 buffer (input)
      REAL*8 LAT,LON		!Point where angles are calculated
      REAL*8 PHASE,INCIDENCE,EMISSION	!Output lighting angles

      REAL*8 RA,RC		!Target radii (km)

      REAL*8 SUNRANGE		!Distance from Sun to target center (km)
      REAL*8 SUNLAT,SUNLON	!Sun lat,lon (radians)

      REAL*8 SCRANGE		!Distance from S/C to target center (km)
      REAL*8 SCLAT,SCLON	!Spacecraft lat,lon (radians)

      REAL*8 VSC(3),VSUN(3),V(3),N(3),S(3),C(3),MAGNITUDE,GCR,D
      REAL*8 RLAT,RLON

      REAL*8 PI,RTD,DTR

      PI = 3.141592653589D0
      RTD = 180.D0/PI
      DTR = PI/180.D0

      RA = BUF(13)
      RC = BUF(15)
      SUNRANGE = BUF(25)
      SUNLAT = BUF(28)*DTR
      SUNLON = (360.D0 - BUF(29))*DTR!Convert to East longitude
      CALL LATREC(SUNRANGE,SUNLON,SUNLAT,vsun)!Vector from center to Sun
C     ....Spacecraft position vector
      SCRANGE = BUF(27)
      SCLAT = BUF(30)*DTR
      SCLON = (360.D0 - BUF(31))*DTR!Convert to East longitude
C     ....Vector from center to S/C
      CALL LATREC(SCRANGE,SCLON,SCLAT,vsc)
C     ....Compute geocentric radius
      RLAT = LAT*DTR
      RLON = (360.D0 - LON)*DTR
      GCR = RA*RC/DSQRT((RC*DCOS(RLAT))**2 + (RA*DSIN(RLAT))**2)
C     ....Vector from center of target to surface pt
      CALL LATREC(GCR,RLON,RLAT,v)
C     ....Compute unit normal N at surface point
      N(1) = V(1)*(RC/RA)**2
      N(2) = V(2)*(RC/RA)**2
      N(3) = V(3)
      CALL UNORM(N,n,magnitude)      
C     ....Compute unit vector from surface point to Sun
      S(1) = VSUN(1) - V(1)
      S(2) = VSUN(2) - V(2)
      S(3) = VSUN(3) - V(3)
      CALL UNORM(S,s,magnitude)
C     ....Compute unit vector from surface point to spacecraft
      C(1) = VSC(1) - V(1)
      C(2) = VSC(2) - V(2)
      C(3) = VSC(3) - V(3)
      CALL UNORM(C,c,magnitude)
C     ....phase angle = ARCCOS(S o C)
      D = S(1)*C(1) + S(2)*C(2) + S(3)*C(3)
      IF (D.GT.1.D0) D=1.D0
      IF (D.LT.-1.D0) D=-1.D0
      PHASE = DACOS(D)*RTD
C     ....incidence angle = ARCCOS(N o S)
      D = N(1)*S(1)+N(2)*S(2)+N(3)*S(3)
      IF (D.GT.1.D0) D=1.D0
      IF (D.LT.-1.D0) D=-1.D0
      INCIDENCE = DACOS(D)*RTD
C     ....emission angle = ARCCOS(N o C)
      D = N(1)*C(1)+N(2)*C(2)+N(3)*C(3)
      IF (D.GT.1.D0) D=1.D0
      IF (D.LT.-1.D0) D=-1.D0
      EMISSION = DACOS(D)*RTD
      RETURN
      END
