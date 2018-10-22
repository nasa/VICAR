cccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine ptres7(i,dc,dn,dedge,eps)
      implicit none
c Inputs...
      integer*4 i	!candidate 1,2 or 3
      real*4 dc,dn,dedge,eps
c Local variables...
      character*80 msg
  101 format('DN contribution of camera: dc=',f7.2)
  102 format(i1,' dn=',f7.2,' dedge=',f7.2,' eps=',f7.2)

      if (i.eq.1) then
         write(msg,101) dc
         call xvmessage(msg,0)
      endif
      write(msg,102) i,dn,dedge,eps
      call xvmessage(msg,0)
      return
      end
