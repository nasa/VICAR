cccccccccccccccccccccccccccccccccccccccccccccccc
c Print the rows output by fill_gaps...

      subroutine print_rows(is,os)
      implicit none
c inputs...
      real*4 is(2,13,23)	!IS coordinates with gaps filled
      real*4 os(2,13,23)	!OS coordinates with gaps filled
c local variables..
      integer row
      integer i,j
      logical odd
      character*132 msg
  101 format(3x,12f6.1)
  102 format(13f6.1)

      do i=1,2
         if (i.eq.1) then
            call xvmessage('Image space line coordinates...',0)
         else
            call xvmessage('Image space sample coordinates...',0)
         endif
         do row=1,23
            odd = .not.row.eq.2*(row/2)
            if (odd) then
               write(msg,101) (is(i,j,row),j=1,12)
               call xvmessage(msg,0)
            else
               write(msg,102) (is(i,j,row),j=1,13)
               call xvmessage(msg,0)
            endif
         enddo
      enddo

      do i=1,2
         if (i.eq.1) then
            call xvmessage('Object space line coordinates...',0)
         else
            call xvmessage('Object space sample coordinates...',0)
         endif
         do row=1,23
            odd = .not.row.eq.2*(row/2)
            if (odd) then
               write(msg,101) (os(i,j,row),j=1,12)
               call xvmessage(msg,0)
            else
               write(msg,102) (os(i,j,row),j=1,13)
               call xvmessage(msg,0)
            endif
         enddo
      enddo

      return
      end
