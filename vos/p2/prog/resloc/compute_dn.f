cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Do a surface interpolation W to compute its minimum value.
c Note: W(0,0) is assumed to be an integral minimum.

      subroutine compute_dn(W,ia,ja,dn)
      implicit none
c Inputs...
      integer*4 ia,ja		!half-window size of shape template
      integer*2 W(-ia:ia,-ja:ja)
c Output...
      real*4 dn			!surface interpolated min value of B

c Local variables...
      integer a,b,c		!because the W(i,j) are integers
      real x,y			!because we divide by 2.
      real dnx,dny		!horizontal and vertical minima
      integer i,j,dnij

c Interpolate mindn to sub-pixel accuracy...
c The calculations are centered at W(0,0) and involves only 5 pixels:
c                  W(0,-1)
c         W(-1,0)  W(0,0)   W(+1,0)
c                  W(0,+1)

c Find the horizontal minimum by fitting W(-1,0), W(0,0), W(+1,0)
c to a parabola...

      if (W(0,0).lt.0) goto 50	!no interpolation is possible

c Horizontal interpolation using W(-1,0), W(0,0) and W(1,0)...
      dnx = -999
      if (W(-1,0).lt.0 .or. W(1,0).lt.0) goto 30
      a = W(1,0) - 2*W(0,0) + W(-1,0)
      if (a.eq.0) goto 30	!skip if straight line
      b = W(1,0) - W(-1,0)
      c = W(0,0)
      x = -b/(2.*a)		!x value of minimum
      dnx = a*x**2 + b*x + c	!interpolated minimum dn

   30 continue

c Vertical interpolation using W(0,-1), W(0,0) and W(0,1)...
      dny = -999
      if (W(0,-1).lt.0 .or. W(0,1).lt.0) goto 40
      a = W(0,1) - 2*W(0,0) + W(0,-1)
      if (a.eq.0) goto 40
      b = W(0,1) - W(0,-1)
      c = W(0,0)
      y = -b/(2.*a)
      dny = a*y**2 + b*y + c

   40 continue
      dn = 256
      if (dnx.ne.-999) then
         if (dny.ne.-999) then
            dn = (dnx+dny)/2
         else
            dn = dnx
         endif
      else
         if (dny.ne.-999) dn=dny
      endif
      if (dn.lt.0) dn=0
      if (dn.eq.256) dn=W(0,0)	!Use center if both interpolations failed
      return


c Here if W(0,0) is invalid.  Set dn equal to minimum of 8 neighbors...

   50 continue
      dn = 256
      do j=-1,1
         do i=-1,1
            dnij = W(i,j)
            if (dnij.ge.0 .and. dnij.lt.dn) dn=dnij
         enddo
      enddo
      if (dn.ne.256) return	!return if dn has been set

c If all else fails, set dn equal to minimum of W...

      do j=-ja,ja
         do i=-ia,ia
            dnij = W(i,j)
            if (dnij.ge.0 .and. dnij.lt.dn) dn=dnij
         enddo
      enddo
      return
      end
