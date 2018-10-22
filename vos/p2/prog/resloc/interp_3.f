ccccccccccccccccccccccccccccccccccccccccccc
c Interpolate over the kth element of z by using its 3 valid neighbors.

c Examples of use:
c         call interp_3(k,vnk,snom,lnom,l)
c         call interp_3(k,vnk,snom,lnom,s)
c         call interp_3(k,vnk,sdc,ldc,dc)

      subroutine interp_3(k,vnk,x,y,z,ezk)
      implicit none
c Inputs...
      integer k 		!mark to be filled
      integer vnk(3)		!valid neighbors of k
      real*4 x(202),y(202),z(202)
c Output...
      real*4 ezk		!estimated value of z(k)

c Local variables...
      real*4 a(3,3),b(3)
      integer i,ki,ind

c Model the behavior of z in the local image region as an affine transformation 
c using its 3 neighbors as data points.

      do i=1,3
         ki = vnk(i)
         a(i,1) = 1
         a(i,2) = x(ki)
         a(i,3) = y(ki)
         b(i) = z(ki)
      enddo

c     ...Solve the simultaneous equation Ac = b
      call simq(a,b,3,ind)	!note: c is returned in b
      if (ind.eq.0) then
         ezk = b(1) + b(2)*x(k) + b(3)*y(k)
      else
         call prnt(2,1,k,'***simq err at mark.')
         ezk = -999
      endif

      return
      end
