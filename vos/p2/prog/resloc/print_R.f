cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print correlation matrix.

      subroutine print_R(R,ir,jr)
      implicit none
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)

      integer buf(-12:12)
      integer i,j
      character*100 msg
  101 format(8x,25i4)

      call xvmessage('Search area 100*rho...',' ')
      do j=-jr,jr
         do i=-ir,ir
            if (R(i,j).ge.-1) then
               buf(i) = 100*R(i,j)
            else
               buf(i) = -999
            endif
         enddo
         write(msg,101) (buf(i),i=-ir,ir)
         call xvmessage(msg,' ')
      enddo
      return
      end
