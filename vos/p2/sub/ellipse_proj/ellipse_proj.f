C Given the vector from target center to surface point, compute the
C (line,sample) location of the image of the surface point in a perspective
C projection camera system.
C
      SUBROUTINE ELLIPSE_PROJ(OM,VSC,FL,OAL,OAS,SCALE,RA,RB,RC,
     &		V,rline,rsamp,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RA,RB,RC		!Target radii
      REAL*8 V(3)		!Vector from target center to surface point
      REAL*8 RLINE,RSAMP 	!Output image coordinates
      INTEGER*4 IND		! =1 if RLINE,RSAMP are valid, =0 otherwise

      REAL*8 x0,y0,z0,xc,yc,zc,S,DOT

C     ...Compute vector from camera to surface point
      xc = V(1) - VSC(1)
      yc = V(2) - VSC(2)
      zc = V(3) - VSC(3)

C     ...Compute dot product of vector with surface normal
      DOT = xc*(v(1)/ra) + yc*(v(2)/rb) + zc*(v(3)/rc)
      IF (DOT .GT. 0.) THEN	!Point is on back side of target
	IND = 0
        RETURN
      ENDIF

C     ...Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(1,2)*yc + OM(1,3)*zc
      y0 = OM(2,1)*xc + OM(2,2)*yc + OM(2,3)*zc
      z0 = OM(3,1)*xc + OM(3,2)*yc + OM(3,3)*zc

C     ...Scale vector into pixels
      S = FL*SCALE/z0			!pixels/km
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END
