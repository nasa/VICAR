cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fit the surface R to a parabola to obtain sub-pixel offsets (dx,dy)
c for the reseau mark coordinates. 
c
c Reference: "Quadratic interpolation of the surface maximum",
c Gary Yagi, JPL IOM April 10, 2005

c The 3 horizontal pixels R(i-1,j),R(i,j),R(i+1,j) are fit to a parabola
c and then solving for the maximum of the parabola.  This determines dx.

c The 3 vertical pixels R(i,j-1),R(i,j),R(i,j+1) are treated the same way
c to obtain dy.

c Because of the 3x3 window, do not call this routine for (i,j) along
c the margins of the search area.

      subroutine interp_max(R,ir,jr,i,j,noin,rho,dx,dy)
      implicit none
c Inputs...
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)		!corr coef over search area
      integer i,j			!R(i,j) is the integer maximum
      logical noin
c Outputs...
      real*4 rho			!interpolated at (dx,dy)
      real*4 dx,dy			!sub-pixel offsets


c Local variables...
      real a,b,c
      real rx,ry

      c = R(i,j)
      rx = c
      ry = c
      dx = 0
      dy = 0
      if (noin) return

c Fit R(i-1,j), R(i,j), R(i+1,j) to a parabola to solve for dx
      if (R(i-1,j).gt.-1. .and. R(i+1,j).gt.-1.) then
         a = R(i+1,j) - 2*R(i,j) + R(i-1,j)
         b = R(i+1,j)-R(i-1,j)
         if (abs(a).gt.0.0001) then
            dx = -b/(2*a)
            rx = a*dx**2 + b*dx + c
         endif
      endif

c Fit R(i,j-1), R(i,j), R(i,j+1) to a parabola to solve for dy
      if (R(i,j-1).gt.-1. .and. R(i,j+1).gt.-1.) then
         a = R(i,j+1) - 2*R(i,j) + R(i,j-1)
         b = R(i,j+1) - R(i,j-1)
         if (abs(a).gt.0.0001) then
            dy = -b/(2*a)
            ry = a*dy**2 + b*dy + c
         endif
      endif
      rho = (rx + ry)/2
      if (rho.gt.1) rho=1
      return
      end
