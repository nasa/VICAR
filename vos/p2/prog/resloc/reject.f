cccccccccccccccccccccccccccccccccccccccccccccccc
c Allow user to reject the location of any mark.

      subroutine reject(cl,cs)
      implicit none
c Updated...
      real*4 cl(202),cs(202)
c Local variables...
      integer*4 i,k
      integer*4 ibuf(202),cnt

      call xvp('reject',ibuf,cnt)

      do i=1,cnt
         k = ibuf(i)
         cl(k) = -999
         cs(k) = -999
      enddo
      return
      end
