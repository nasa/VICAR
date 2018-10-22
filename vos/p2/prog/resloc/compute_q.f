cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccct
c
c Compute composite match quality for all 3x202 candidates.

      subroutine compute_q(rho,dn,dedge,dc,eps,maxeps,tres,g,h,q)
      implicit none
c Inputs...
      real*4 rho(3,202)		!correlation coefficient
      real*4 dn(3,202)		!dn of reseau mark
      real*4 dedge(3,202)	!avg dn of pixels on the perimeter
      real*4 dc(202)		!estimate of dark current
      real*4 eps(3,202)		!distance from expected location
      real*4 maxeps		!maximum displacement	
      integer tres		!test reseau mark number
c Output...
      real*4 g(3,202)		!darkness measure
      real*4 h(3,202)		!closeness measure
      real*4 q(3,202)		!composite match quality
c Local variables...
      integer i,k
      real dnthresh

      dnthresh = 10.		!warning: arbitrary noise floor

      do 202 k=1,202

      do 10 i=1,3		!compute q for candidates 1,2,and 3
      if (rho(i,k).eq.-999) goto 10	!skip invalid values
      g(i,k) = 1 - (dn(i,k)-dc(k))/(dedge(i,k)-dc(k))
      h(i,k) = 1 - eps(i,k)/maxeps 
      if (dedge(i,k)-dc(k).gt.dnthresh) then
         q(i,k) = (rho(i,k)+g(i,k)+h(i,k))/3
      else
         q(i,k) = (rho(i,k)+h(i,k))/2	!ignore value of g
      endif
      if (k.eq.tres) then
         call ptres7(i,dc(k),dn(i,k),dedge(i,k),eps(i,k))
         call ptres8(rho(i,k),g(i,k),h(i,k),q(i,k))
      endif
   10 continue

  202 continue

      return
      end
