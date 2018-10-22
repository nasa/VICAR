ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print window.

      subroutine print_window(W,iw,jw)
      implicit none
      integer iw,jw
      integer*2 W(-iw:iw,-jw:jw)


      integer i,j
      character*132 msg		!room for 33 4-digit integers
  101 format(25i4)

      do j=-jw,jw
         write(msg,101) (W(i,j),i=-iw,iw)
         call xvmessage(msg,0)
      enddo

      return
      end
