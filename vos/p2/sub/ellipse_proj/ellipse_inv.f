CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the (line,sample) coordinates of a point on an ellipsoid target
C surface, compute the vector from target center to surface point.
C The line-sample coordinates are assumed to be in object space.
C
      SUBROUTINE ELLIPSE_INV(OM,VSC,FL,OAL,OAS,SCALE,RA,RB,RC,
     &		RLINE,RSAMP,v,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RA,RB,RC		!Target radii
      REAL*8 RLINE,RSAMP 	!Input image coordinates
      REAL*8 V(3)		!Vector from target center to surface point
      INTEGER*4 IND		! =0 if is not on the target, =1 success

      real*8 x0,y0,z0,xc,yc,zc,s,a,b,c,d
      real*8 x1,y1,z1

C     ...Vector from spacecraft to surface point in camera coordinate system
      x0 = rsamp - oas
      y0 = rline - oal
      z0 = fl*scale

C     ...Convert vector to target-centered coordinates
      xc = OM(1,1)*x0 + OM(2,1)*y0 + OM(3,1)*z0
      yc = OM(1,2)*x0 + OM(2,2)*y0 + OM(3,2)*z0
      zc = OM(1,3)*x0 + OM(2,3)*y0 + OM(3,3)*z0

c Solve for the scale factor s which extends the vector to where it intersects
c the target surface...

      x0 = xc/ra
      y0 = yc/rb
      z0 = zc/rc

      x1 = vsc(1)/ra
      y1 = vsc(2)/rb
      z1 = vsc(3)/rc

      a = x0*x0 + y0*y0 + z0*z0
      b = x0*x1 + y0*y1 + z0*z1
      c = x1*x1 + y1*y1 + z1*z1 - 1.
      d = b*b - a*c

      ind = 0
      if (d .lt. 0.) return       !The vector does not intercept the target

C There are two intercepts (+b and -b).  The shorter one is visible,
C the other is on the back side of the target.

      s = (-b-dsqrt(d))/a      !s*cp is the vector from s/c to surface point
      v(1) = s*xc + vsc(1)
      v(2) = s*yc + vsc(2)
      v(3) = s*zc + vsc(3)
      ind = 1
      return
      end
