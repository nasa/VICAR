cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Test for local maxima.

      logical function local_max(R,ir,jr,i,j)
      implicit none
c Inputs...
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)   !correlation coefficient (rho) over search area
      integer i,j       !pixel of interest is R(i,j).

c Local variables
      integer ii,jj     !inner pixel loop
      real rho

c           R(i-1,j-1)  R(i,j-1)  R(i+1,j-1)
c           R(i-1,j)    R(i,j)    R(i+1,j)
c           R(i-1,j+1)  R(i,j+1)  R(i+1,j+1)

      local_max = .false.
      rho = R(i,j)
      do jj=-1,1
         do ii=-1,1
            if (ii.ne.0 .or. jj.ne.0) then
               if (rho.le.R(i+ii,j+jj)) return  !not local max
            endif
         enddo
      enddo

      local_max = .true.
      return
      end
