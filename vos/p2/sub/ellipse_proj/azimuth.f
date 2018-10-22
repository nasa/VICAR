C Compute north azimuth, spacecraft azimuth, and solar azimuth angles.
C The azimuth angles are measured at the surface point defined by the vector V.
C To compute the solar azimuth, the solar vector is (1) projected into the
C plane tangent to the surface point, and (2) this projected vector is then
C projected into the image plane.  The solar azimuth is the angle between a
C horizontal line pointing to the right in the image and this projected vector,
C measured in a clockwise direction.  The other azimuths are defined similarly.
C
      subroutine azimuth(om,op,vsc,vsun,ra,rb,rc,flg,noraz,scaz,sunaz)
      implicit none
      real*8 om(3,3)		!Target to camera transformation (OM matrix)
      real*8 op(3)		!Vector from target center to surface point
      real*8 vsc(3)		!Vector from target center to S/C (RS vector)
      real*8 vsun(3)		!Vector from target center to sun
      real*8 ra,rb,rc		!Target radii
      integer*4 flg		!=1 if surface pt is target center, =0 otherwise
      real*8 noraz		!Output north azimuth (degrees)
      real*8 scaz		!Output spacecraft azimuth (degrees)
      real*8 sunaz		!Output solar azimuth (degrees)

      real*8 n(3),s(3),c(3),v(3),m(3,3),r,rlat,rlon,halfpi,dpr
      integer i

C     ...Compute vector from target center to north pole
      n(1) = 0.
      n(2) = 0.
      n(3) = rc

C     ...Compute the three vectors of interest
      do i=1,3
         c(i) = vsc(i) - op(i)		!Vector from surface point to spacecraft
         s(i) = vsun(i) - op(i)		!Vector from surface point to sun
         n(i) = n(i) - op(i)		!Vector from surface point to north pole
      enddo
C
C We need to project these vectors into the tangent plane.  To do this, we
C transform the vectors to a coordinate system where the z-axis is normal to the
C surface.  The projection into the tangent plane is then simply the x and y
C components of the vector.
C
C If the surface point is the target center, the spacecraft vector projects
C to a single point and the azimuth is undefined.  For this case, the S/C
C vector is projected directly onto the image plane.
C
C The tangent plane is defined by the surface normal vector v:
C
      v(1) = op(1)*(rc/ra)**2
      v(2) = op(2)*(rc/ra)**2
      v(3) = op(3)

C Convert to lat-lon and compute transformation matrix m

      call reclat(v,r,rlon,rlat)
      call eul2m(0.d0,halfpi()-rlat,halfpi()+rlon,3,1,3,m)

C Transform the vectors to "tangent plane coordinate system"

      call mxv(m,n,n)
      if (flg .ne. 1) call mxv(m,c,c)
      call mxv(m,s,s)

C Project vectors into plane

      n(3) = 0.
      if (flg .ne. 1) c(3) = 0.
      s(3) = 0.

C Now project the projected vectors into the image plane by first transforming
C the vectors back into planet coordinates, and then using to OM matrix to
C tranform them into image plane coordinates.

      call mtxv(m,n,n)
      if (flg .ne. 1) call mtxv(m,c,c)
      call mtxv(m,s,s)

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
