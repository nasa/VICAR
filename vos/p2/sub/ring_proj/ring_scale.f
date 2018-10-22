C Compute the scale at a given ring radius in km/pixel.  The scale is computed
C at the point on the visible ring closest to the camera.
C
      SUBROUTINE RING_SCALE(OM,VSC,FL,OAL,OAS,SCALE,NL,NS,R,
     &		lon,srange,rline,rsamp,rscl,ascl,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)            !Camera to planet transformation matrix
      REAL*8 VSC(3)             !Vector from target center to spacecraft
      REAL*4 FL                 !Camera focal length in mm 
      REAL*4 OAL,OAS            !Optical axis intercept line-sample
      REAL*4 SCALE              !Picture scale in pixels/mm
      INTEGER NL,NS		!Number of lines and samples in image
      REAL*8 R			!Input ring radius
      REAL*8 LON		!Output longitude
      REAL*8 SRANGE		!Slant range
      REAL*8 RLINE,RSAMP        !Output image coordinates
      REAL*8 RSCL,ASCL		!Radial and azimuthal ring scale (km/pixel)
      INTEGER IND		! =1 if RSCL and ASCL are valid, =0 otherwise

      REAL*8 L,S,LON0,SR,V(3),PIXELS,KILOMETERS,PI,RPD
      INTEGER I

      PI = 3.141592653589793D0
      RPD = PI/180.D0          !radians per degree

      RSCL = -999.
      ASCL = -999.
      SRANGE = 2.D+9
C     ...Scan at 1 degree increments for the point on the ring closest
C     ...to the camera (minimum slant range).
      DO 20 I=1,360
      LON0 = 1.0*I
      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON0,l,s,ind)
      IF (IND.EQ.0) GOTO 20
      IF (L.LT.1. .OR. L.GT.NL) GOTO 20
      IF (S.LT.1. .OR. S.GT.NS) GOTO 20
      V(1) = R*DCOS(LON0*RPD) - VSC(1)     !Vector from camera to ring pt
      V(2) = -R*DSIN(LON0*RPD) - VSC(2)
      V(3) =                  - VSC(3)
      SR = DSQRT(V(1)**2 + V(2)**2 + V(3)**2)    !Distance to camera
      IF (SR .GE. SRANGE) GOTO 20
      SRANGE = SR
      LON = LON0
      RLINE = L
      RSAMP = S
   20 CONTINUE

      IF (SRANGE .LT. 1.D+9) GOTO 100

C     ...Repeat scanning for the point on the ring closest to the camera,
C     ...but without restricting it to points in the image.
      DO 30 I=1,360
      LON0 = 1.0*I
      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON0,l,s,ind)
      IF (IND.EQ.0) GOTO 30
      V(1) = R*DCOS(LON0*RPD) - VSC(1)     !Vector from camera to ring pt
      V(2) = -R*DSIN(LON0*RPD) - VSC(2)
      V(3) =                  - VSC(3)
      SR = DSQRT(V(1)**2 + V(2)**2 + V(3)**2)    !Distance to camera
      IF (SR .GE. SRANGE) GOTO 30
      SRANGE = SR
      LON = LON0
      RLINE = L
      RSAMP = S
   30 CONTINUE

      IF (SRANGE .GT. 1.D+9) THEN
         SRANGE = -999.
         IND = 0
         RETURN
      ENDIF

C     ...Compute the arc length making up 1 deg of the ring in pixels
  100 CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON+1.,l,s,ind)
      PIXELS = DSQRT((RLINE-L)**2 + (RSAMP-S)**2)
C     ...Compute the same arc length in kilometers: S = R*DLON
      KILOMETERS = R*RPD
      ASCL = KILOMETERS/PIXELS		!Azimuthal scale
      
C     ...Compute the pixels representing 1 km radial distance
      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R+1.,LON,l,s,ind)
      PIXELS = DSQRT((RLINE-L)**2 + (RSAMP-S)**2)
      RSCL = 1./PIXELS			!Radial scale
      RETURN
      END
