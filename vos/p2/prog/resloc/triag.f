cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Assemble complete row of quadrilateral grid and pack in geoma format.
c From Jean Lorre's subroutine triag.
     
      subroutine triag(row,is,os,geo)
      implicit none
c Inputs...
      integer row
      real*4 is(2,13),os(2,13)
c Output...
      real*4 geo(4,24)
c Local variables...
      integer i,j,k,m,n

c The inputs have 13 tiepoints if the row is odd and 12 if even.
c The output always has 24 tiepoints using the follow rules:
c   If the row is odd, all 12 positions are replicated.
c   If the row is even, only the 11 internal positions are replicated. 

      k = 0
      m = 0
      if (row.eq.2*(row/2)) m=1

      do j=1,12
         do i=1,2
            k = k + 1
            m = m + 1
            n = (m + 1)/2
            geo(1,k) = os(1,n)
            geo(2,k) = os(2,n)
            geo(3,k) = is(1,n)
            geo(4,k) = is(2,n)
         enddo
      enddo

      return
      end
