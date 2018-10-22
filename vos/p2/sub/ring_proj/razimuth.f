C Compute north azimuth, spacecraft azimuth, and solar azimuth angles at a
C point on the ring plane.  The ring point is defined by the vector op.
C The solar vector is first projected onto the ring plane.  The projected
C vector is then projected into the image plane.  The solar azimuth is the
C angle between the P5-P6 direction and this projected vector, measured
C clockwise.  The other azimuths are defined similarly.
C
      subroutine razimuth(om,op,vsc,vsun,noraz,scaz,sunaz)
      implicit none
      real*8 om(3,3)		!Target to camera transformation (OM matrix)
      real*8 op(3)		!Vector from target center to ring point
      real*8 vsc(3)		!Vector from target center to S/C (RS vector)
      real*8 vsun(3)		!Vector from target center to sun
      real*8 noraz		!Output north azimuth (degrees)
      real*8 scaz		!Output spacecraft azimuth (degrees)
      real*8 sunaz		!Output solar azimuth (degrees)

      real*8 n(3),s(3),c(3),dpr
      integer i

C Note: For the ring plane, the north vector is replaced by the vector from
C ring point to target center.

C     ...Compute the three vectors of interest
      do i=1,3
         c(i) = vsc(i) - op(i)		!Vector from ring point to spacecraft
         s(i) = vsun(i) - op(i)		!Vector from ring point to sun
         n(i) = - op(i)			!Vector from ring point to center
      enddo

C Project vectors into the ring plane

      n(3) = 0.
      c(3) = 0.
      s(3) = 0.

C Project the vectors into the image plane

      call mxv(om,n,n)
      call mxv(om,c,c)
      call mxv(om,s,s)

      noraz = datan2(n(2),n(1))*dpr()
      if (noraz .lt. 0.) noraz=noraz+360.

      scaz = datan2(c(2),c(1))*dpr()
      if (scaz .lt. 0.) scaz=scaz+360.

      sunaz = datan2(s(2),s(1))*dpr()
      if (sunaz .lt. 0.) sunaz=sunaz+360.
      return
      end
