cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Find the 3 highest local maxima of R.
 
      subroutine find_rmax(R,ir,jr,rho,di,dj)
      implicit none
c Inputs...
      integer ir,jr
      real*4 R(-ir:ir,-jr:jr)	!correlation coefficient over search area
c Outputs...
      real*4 rho(3)	!First, second and third highest local maximum of R
      integer*4 di(3)	!(i,j) location of 3 maxima
      integer*4 dj(3)

c Local variables...
      integer i,j,k
c Function calls...
      logical local_max		!true if r(i,j) is a local maximum

c We do not allow pixels on the perimeter of the search area to be
c local maxima.  Therefore, we limit our search to pixels away from
c the edges...

      do 50 j=-jr+1,jr-1	!Loop thru interior of search area	
      do 40 i=-ir+1,ir-1
      if (.not.local_max(R,ir,jr,i,j)) goto 40	!skip if not a local max
      if (r(i,j).gt.rho(1)) then	!if r(i,j) is greater than 1,
         rho(3) = rho(2)		!replace 3 with 2
         di(3) = di(2)
         dj(3) = dj(2)
         rho(2) = rho(1)		!replace 2 with 1,
         di(2) = di(1)
         dj(2) = dj(1)
         rho(1) = r(i,j)		!and replace 1 with r(i,j)
         di(1) = i
         dj(1) = j
      elseif (r(i,j).gt.rho(2)) then	!If r(i,j) is greater than 2,
         rho(3) = rho(2)		!replace 3 with 2
         di(3) = di(2)
         dj(3) = dj(2)
         rho(2) = r(i,j)		!and replace 2 with r(i,j).
         di(2) = i
         dj(2) = j
      elseif (r(i,j).gt.rho(3)) then	!elsif r(i,j) is greater than 3,
         rho(3) = r(i,j)		!replace 3 with r(i,j).
         di(3) = i
         dj(3) = j
      endif
   40 continue
   50 continue

      return
      end
