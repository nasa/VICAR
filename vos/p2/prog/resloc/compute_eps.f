ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Compute the residuals from the model:
c          s = snom + e
c          l = lnom + f

      subroutine compute_eps(l,s,lnom,snom,e,f,tres,eps)
      implicit none
c Inputs...
      real*4 l(3,202),s(3,202)		!3 best locations
      real*4 lnom(202),snom(202)		!nominal locations	
      real e,f
      integer*4 tres
c Outputs...
      real*4 eps(3,202)

c Local variables..
      integer i,k
      real dl,ds
      character*132 msg

      do 50 k=1,202
      do 40 i=1,3
      if (l(i,k).ne.-999) then
         dl = l(i,k) - lnom(k) - f
         ds = s(i,k) - snom(k) - e
         eps(i,k) = sqrt(dl**2+ds**2)
      else
         eps(i,k) = -999
      endif
   40 continue
   50 continue

      if (tres.eq.0) return
      k = tres
      write(msg,101) lnom(k),snom(k),e,f
      call xvmessage(msg,0)
      do i=1,3
         write(msg,102) i,l(i,k),s(i,k),eps(i,k)
         call xvmessage(msg,0)
      enddo
      return

  101 format('(lnom,snom)=(',f5.1,',',f5.1,') e=',f5.2,' f=',f5.2)
  102 format(i1,': (l,s)=(', f6.1,',',f6.1,') eps=',f6.1)
      end
