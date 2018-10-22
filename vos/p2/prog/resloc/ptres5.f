cccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine ptres5(i,di,dj,dx,dy,k,rho,dn,dedge)
      implicit none
c Inputs...
      integer*4 i	!candidate 1,2 or 3
      integer*4 k	!reseau mark index
      integer*4 di(3),dj(3)
      real*4 dx(3),dy(3)
      real*4 rho(3,202),dn(3,202),dedge(3,202)
c Local variables...
      character*80 msg
  102 format(i1,': (di,dj)=(',i2,',',i2,') (dx,dy)=(',f5.1,',',f5.1,
     &	') rho=',f5.3,' dn=',f5.1,' dedge=',f5.1)

      write(msg,102) i,di(i),dj(i),dx(i),dy(i),
     &	rho(i,k),dn(i,k),dedge(i,k)
      call xvmessage(msg,0)
      return
      end
