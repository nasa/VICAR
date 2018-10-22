cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the correlation coefficient rho for every pixel in the search
c area and return in matrix R.

c This version of compute_R ignores pixels in data gaps or off the
c edge in the calculation of rho.

      subroutine compute_R_gap(A,ia,ja,B,ib,jb,R,ir,jr)
      implicit none
c Inputs...
      integer ia,ja
      integer*2 A(-ia:ia,-ja:ja)	!shape template.
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!search window centered at D(snom,lnom)
      integer ir,jr
c Output...
      real*4 R(-ir:ir,-jr:jr)		!rho for every pixel in search area

c Local variables...
      integer ii,jj		!window W is centered over B(ii,jj) 
      integer i,j
      integer aij		!aij = A(i,j)
      integer wij		!wij = B(i+ii,j+jj)
      real sa,sw,sa2,saw,sw2	!sums of A and W
      real meana,meanw		!mean of A and mean of W
      real vara,varw,covaw	!variance of A and W and covariance of A and W
      real*4 fract
      integer narea		!number of pixels in correlation window
      real n			!number of valid pixels in correlation window

      narea = (2*ia+1)*(2*ja+1)	!total number of pixels in template A
      fract = 0.5		!fraction of pixels required for computation


c Compute rho for every pixel in the search area...

      do 20 jj=-jr,jr		!search area row index
      do 10 ii=-ir,ir		!search area column index
      sa = 0
      sw = 0
      sa2 = 0
      saw = 0
      sw2 = 0
      n = 0

c     ...compute rho for window W centered at B(ii,jj)
      do j=-ja,ja		!shape template row index
         do i=-ia,ia		!shape template col index
            aij = A(i,j)	!pixel in A
            wij = B(i+ii,j+jj)	!corresponding pixel in W
            if (wij.ge.0) then	!do if the pixel is valid
               sa = sa + aij
               sw = sw + wij
               sa2 = sa2 + aij**2
               saw = saw +aij*wij
               sw2 = sw2 + wij**2
               n = n + 1
            endif
         enddo
      enddo


      R(ii,jj) = -999		!initialize as invalid
      if (n.lt.fract*narea) goto 10	!reject if too few valid pixels
      meanw = sw/n
      varw = sw2/n - meanw**2	!variance of B
      if (varw.le.0) goto 10	!if no pulse, rho has no meaning.
      meana = sa/n
      vara = sa2/n - meana**2	!variance of A
      covaw = saw/n - meana*meanw
      R(ii,jj) = covaw/sqrt(vara*varw)
   10 continue
   20 continue

      return
      end
