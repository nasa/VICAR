C Convert from ring plane coordinates (radius,longitude) to camera coordinates
C (line,sample) in a perspective projection camera system.
C
      SUBROUTINE RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,RADIUS,RLON,
     &		rline,rsamp,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RADIUS,RLON	!Input ring coordinates.
      REAL*8 RLINE,RSAMP 	!Output image coordinates
      INTEGER*4 IND		! =1 if RLINE,RSAMP are valid, =0 otherwise

      REAL*8 x0,y0,z0,xc,yc,zc,S,ELON,PI,RPD

      PI = 3.141592653589793D0
      RPD = PI/180.D0		!radians per degree
      ELON = 360.D0 - RLON	!East longitude

C     ...Compute vector from camera to point on ring
      xc =  RADIUS*DCOS(ELON*RPD) - VSC(1)
      yc =  RADIUS*DSIN(ELON*RPD) - VSC(2)
      zc =                        - VSC(3)
C     ...Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(1,2)*yc + OM(1,3)*zc
      y0 = OM(2,1)*xc + OM(2,2)*yc + OM(2,3)*zc
      z0 = OM(3,1)*xc + OM(3,2)*yc + OM(3,3)*zc
      IF (z0 .LE. 0.) THEN	!Camera is looking away from ring plane
        RLINE = -999.
        RSAMP = -999.
	IND = 0
        RETURN
      ENDIF

C     ...Scale vector into pixels
      S = FL*SCALE/z0			!pixels/km
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END
