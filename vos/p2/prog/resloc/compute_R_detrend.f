cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the matrix R.
c This version detrends every search window W before computing rho(A,W).

      subroutine compute_R_detrend(A,ia,ja,B,ib,jb,R,ir,jr,
     &		W,rsw,csw)
      implicit none
c Inputs...
      integer ia,ja
      integer*2 A(-ia:ia,-ja:ja)	!nlw x nsw shape function.
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!search window centered at D(lnom,snom).
      integer ir,jr
c Scratch space...
      integer*2 W(-ia:ia,-ja:ja)
      integer rsw(-ja:ja)		!sums along each row of w
      integer csw(-ib:ib)		!sums along each column of B
c Output...
      real*4 R(-ir:ir,-jr:jr)		!rho for nver x nhor search area

c Template A mean and sigma...
      common/ca/area,meanA,sigA
      real area,meanA,sigA

c Local variables...
      integer ii,jj		!search window offsets
      integer i,j		!A(i,j) = B(i+ii,j+jj) 

      integer sw,sw2,saw
      real meanW,varW,covAW,sigW

      do 20 jj=-jr,jr		!row loop thru R
      do 10 ii=-ir,ir		!column loop thru R
      call detrend_window(B,ib,jb,ii,jj,W,ia,ja,rsw,csw)
      sw = 0
      sw2 = 0
      saw = 0
      do j=-ja,ja
         do i=-ia,ia
            sw = sw + w(i,j)
            sw2 = sw2 + w(i,j)**2
            saw = saw + a(i,j)*w(i,j)
         enddo
      enddo
      meanW = sw/area
      varW = sw2/area - meanW**2
      if (varW.gt.0) then	!do this if there is a signal
         covAW = saw/area - meanA*meanW
         sigW = sqrt(varW)
         R(ii,jj) = covAW/(sigA*sigW)
      else
         R(ii,jj) = -999	!no pulse, rho has no meaning.
      endif
   10 continue
   20 continue

      return
      end
