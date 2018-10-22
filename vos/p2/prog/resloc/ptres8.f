cccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine ptres8(f,g,h,q)
      implicit none
c Inputs...
      real*4 f,g,h,q
c Local variables...
      character*80 msg
  102 format(' rho=',f8.3,' g=',f8.3,' h=',f8.3,' q=',f8.3)

      write(msg,102) f,g,h,q
      call xvmessage(msg,0)
      return
      end
