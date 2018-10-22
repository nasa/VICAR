ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copy the pixel area centered at D(l,s) into window W.
c Pixels that lie outside the image are set to -999 DN.

      subroutine get_window(D,l,s,W,iw,jw)
      implicit none
c Inputs...
      integer l,s		!center of window is at D(l,s)
      byte D(800,800)	!d is nl x ns
      integer iw,jw		!window half-widths
c Output...
      integer*2 W(-iw:iw,-jw:jw)	!W(i,j) = d(s+i,l+j)

c Local variables...
      integer i,j
      integer nl,ns
      byte tbl(0:255)
      include 'fortport'

      do j=-jw,jw
         if (l+j.ge.1 .and. l+j.le.800) then
            do i=-iw,iw
               if (s+i.ge.1 .and. s+i.le.800) then
               W(i,j) = byte2int(D(s+i,l+j))	!convert from byte to i*2
               else
                  W(i,j) = -999
               endif
            enddo
         else
            do i=-iw,iw
               W(i,j) = -999
            enddo
         endif
      enddo

      return
      end
