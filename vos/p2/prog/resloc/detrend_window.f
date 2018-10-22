cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Detrend the window centered at B(ii,jj) and output to W.

      subroutine detrend_window(B,ib,jb,ii,jj,W,ia,ja,rsw,csw)
      implicit none
c Inputs...
      integer ib,jb
      integer*2 B(-ib:ib,-jb:jb)	!search area
      integer ia,ja
      integer ii,jj			!center window at B(ii,jj)  
c Output...
      integer*2 W(-ia:ia,-ja:ja)	!detrended window
c Scratch space...
      integer rsw(-ja:ja)	!row sums of w(i,j)
      integer csw(-ia:ia)	!column sums of w(i,j)

      common/cd/npixels,sii,sjj
      real npixels,sii,sjj

c Local variables...
      integer i,j		!w(i,j) = b(i+ii,j+jj)
      integer rsumw
      integer sumw,sumwi,sumwj
      real*8 c1,c2,c3		!plane constants


c Fit window W to a brightness plane D, where

c	d(i,j) = c1*i + c2*j + c3

c solving for the coefficients c1, c2, and c3 that minimize the
c least-squares error.

      sumw = 0
      do i=-ia,ia
         csw(i) = 0
      enddo

      do j=-ja,ja
         rsumw = 0
         do i=-ia,ia
            w(i,j) = b(i+ii,j+jj)	!copy window from B to W 
            rsumw = rsumw + w(i,j)
            csw(i) = csw(i) + w(i,j)
         enddo
         sumw = sumw + rsumw
         rsw(j) = rsumw
      enddo

      sumwi = 0
      do i=-ia,ia
         sumwi = sumwi + csw(i)*i
      enddo

      sumwj = 0
      do j=-ja,ja
         sumwj = sumwj + rsw(j)*j
      enddo

      c1 = sumwi/sii
      c2 = sumwj/sjj
      c3 = sumw/npixels


c Detrend the window...

      do j=-ja,ja
         do i=-ia,ia
            w(i,j) = w(i,j) - (c1*i+c2*j+c3)
         enddo
      enddo

      return
      end
