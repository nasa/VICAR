C Compute the phase, incidence, and emission angles for a point on the ring.
C
      subroutine ring_light(vsc,vsun,r,lon,i,e,p)
      implicit none
      real*8 vsc(3)             !Vector from target center to S/C (RS vector)
      real*8 vsun(3)            !Vector from target center to sun
      real*8 r,lon		!Radius and longitude of ring point
      real*8 i,e,p		!Incidence, emission, and phase angles (deg)
 
      real*8 v(3),s(3),c(3),PI,rpd,dpr
      real*8 solrange,scrange,elon
      integer j
 
      PI = 3.141592653589793D0
      rpd = PI/180.d0		!Radians per degree
      dpr = 180.d0/PI           !Degrees per radian
      elon = 360.d0 - lon	!East longitude

C     ....Compute vector from planet center to ring point
      v(1) = r*dcos(elon*rpd)
      v(2) = r*dsin(elon*rpd)
      v(3) = 0.

      do j=1,3
         s(j) = vsun(j) - v(j)		!Vector from ring point to sun
         c(j) = vsc(j)  - v(j)		!Vector from ring point to S/C
      enddo

C     ....Convert to unit vectors
      solrange = dsqrt(s(1)**2+s(2)**2+s(3)**2)		!Sun range
      scrange  = dsqrt(c(1)**2+c(2)**2+c(3)**2)		!S/C range
      do j=1,3
         s(j) = s(j)/solrange
         c(j) = c(j)/scrange
      enddo
 
C     ....First assume the sun illuminates the northern half of the ring plane.
C     ....Unit normal to ring plane is n = (0,0,1)
C     ....Cosine of incidence angle is dot product of s and n
      i = dacos(s(3))*dpr
C     ....Cosine of emission angle is dot product of c and n
      e = dacos(c(3))*dpr
C     ....cosine of phase angle is dot product of s and c
      p = dacos(s(1)*c(1)+s(2)*c(2)+s(3)*c(3))*dpr
C     ....If sun illuminates southern half of ring plane, then
C     ....unit normal to ring plane is n = (0,0,-1)
      if (i .gt. 90.) then
         i = 180. - i
         e = 180. - e
      endif
      return
      end
