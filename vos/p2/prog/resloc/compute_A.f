ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute reseau shape function A, where A is an inverted Gaussian:
c	A(i,j) = 255.*(1-exp(s*(i**2+j**2)))
c where
c	s = -1/(2*sig**2)
c i=-ia,...,ia and j=-ja,...,ja.

      subroutine compute_A(A,ia,ja)
      implicit none
c Inputs...
      integer ia,ja			!template half widths
c Output...
      integer*2 A(-ia:ia,-ja:ja)	!shape function

c Routine also sets these values in common...
      common/ca/area,meanA,sigA
      real area,meanA,sigA		!area, mean, and sigma of A

      common/cd/npixels,sii,sjj
      real npixels
      real sii,sjj			!sum of i**2 and j**2

c Local variables...
      real sig,s
      integer sumA,sumA2

      integer i,j,m,n,count
      logical xvptst
      character*80 msg
  100 format('sig=',f6.2)
  101 format(11i4)
  102 format(' meanA=',f6.2,' sigmaA=',f6.2)
  103 format('sii=',f10.2,' sjj=' f10.2)

c Compute the Gaussian constant s...
      call xvp('sigma',sig,count)
      write(msg,100) sig
      call xvmessage(msg,0)
      s = -1/(2*sig**2)	

c Compute A, mean(A) and sigma(A)...
      sumA = 0.
      sumA2 = 0.
      do j=-ja,ja
         do i=-ia,ia
            A(i,j) = 255*(1 - exp(s*(i**2+j**2)))
            sumA = sumA + A(i,j)
            sumA2 = sumA2 + A(i,j)**2
         enddo
      enddo

c Set the values in common/ca/...
      area = (2*ia+1)*(2*ja+1)
      meanA = sumA/area				!mean of A
      sigA = sqrt(sumA2/area - meanA**2)	!sigma of A
      npixels = area
      sii = (ia*(ia+1)*(2*ia+1)*(2*ja+1))/3.
      sjj = (ja*(ja+1)*(2*ja+1)*(2*ia+1))/3.

      if (xvptst('print')) then
         call xvmessage('reseau shape template A...',' ')
         do j=-ja,ja
            write(msg,101) (a(i,j),i=-ia,ia)
            call xvmessage(msg,' ')
         enddo
         write(msg,102) meanA,sigA
         call xvmessage(msg,' ')
ccc        write(msg,103) sii,sjj
ccc        call xvmessage(msg,0)
      endif

      return
      end
