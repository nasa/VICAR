CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert from camera coordinates (line,sample) to ring plane coordinates
C (radius,longitude) for a perspective projection camera system.  The
C line-sample coordinates are assumed to be in object space.
C
      SUBROUTINE RING_INV(OM,VSC,FL,OAL,OAS,SCALE,RLINE,RSAMP,
     &		radius,rlon,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RLINE,RSAMP 	!Input image coordinates
      REAL*8 RADIUS,RLON	!Output ring coordinates.
      INTEGER*4 IND		! =1 If RADIUS,RLON are valid, =0 otherwise

      REAL*8 x0,y0,z0,xc,yc,zc,x,y,S
      REAL*8 PI/3.141592653589793D0/,DPR

      DPR = 180.D0/PI          !degrees per radian

C     ...Vector from spacecraft to point on ring in camera coordinate system
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = FL*SCALE
C     ...Convert vector to target-centered coordinates
      xc = OM(1,1)*x0 + OM(2,1)*y0 + OM(3,1)*z0
      yc = OM(1,2)*x0 + OM(2,2)*y0 + OM(3,2)*z0
      zc = OM(1,3)*x0 + OM(2,3)*y0 + OM(3,3)*z0

      S = -VSC(3)/zc		!Scale in km per pixel
      IF (S.LT.0.) THEN		!Camera is looking away from ring plane
	IND = 0
        RETURN
      ENDIF

      X = S*xc + VSC(1)
      Y = S*yc + VSC(2)
      RADIUS = DSQRT(X**2+Y**2)
      RLON = - DMOD(DATAN2(Y,X)*DPR,360.D0)	!West longitude
      IF (RLON .LT. 0.D0) RLON= RLON+360.D0
      IND = 1
      RETURN
      END
