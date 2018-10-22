cccccccccccccccccccccccccccccccccc
c Copy pixels from B to W so that W(0,0)=B(i,j)...

      subroutine get_W(B,ib,jb,W,ia,ja,i,j)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)
      integer*4 i,j
      integer ia,ja
c Output...
      integer*2 W(-ia:ia,-ja:ja)
c Local variables...
      integer ii,jj

      do jj=-ja,ja
         do ii=-ia,ia
             W(ii,jj) = B(i+ii,j+jj)
         enddo
      enddo
      return
      end
