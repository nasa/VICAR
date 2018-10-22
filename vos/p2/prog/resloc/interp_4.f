ccccccccccccccccccccccccccccccccccccccccccc
c Interpolate over the kth element of z by by using its 4 valid neighbors.

      subroutine interp_4(k,vnk,x,y,z,ezk)
      implicit none
c Inputs...
      integer k 		!mark to be filled
      integer vnk(4)		!valid neighbors of k
      real*4 x(202),y(202),z(202)
c Output...
      real*4 ezk		!estimated value of z(k)

c Local variables...
      real*4 a(4,4),b(4)
      integer i,ki,ind

c Model the behavior of z in the local image region as a bilinear
c transfomation using its four neighbors as data points.

      do i=1,4
         ki = vnk(i)
         a(i,1) = 1
         a(i,2) = x(ki)
         a(i,3) = y(ki)
         a(i,4) = x(ki)*y(ki)
         b(i) = z(ki)
      enddo

      call simq(a,b,4,ind)
      if (ind.eq.0) then
         ezk = b(1) + b(2)*x(k) + b(3)*y(k) 
     &		+ b(4)*x(k)*y(k)
      else
         call prnt(2,1,k,'***simq err at mark.')
         ezk = -999
      endif

      return
      end
