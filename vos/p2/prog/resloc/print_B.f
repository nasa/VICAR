ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Print B.

      subroutine print_B(B,ib,jb,k)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!B(i,j) = d(s+i,l+j)
      integer k				!reseau mark number

c Local variables...
      integer i,j
      character*132 msg
  102 format('tres=',i3,' B=...')
  103 format(30i4)

      call xvmessage('.page.',0)
      write(msg,102) k
      call xvmessage(msg,0)

      do j=-jb,jb
         write(msg,103) (B(i,j),i=-ib,ib)
         call xvmessage(msg,0)
      enddo
      return
      end
