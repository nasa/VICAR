cccccccccccccccccccccccccccccc
c Reject a reseau mark if rho is smaller than rthresh...

      subroutine check_rho(crho,cl,cs,dbug)
      implicit none
c Inputs...
      real*4 crho(202)
      logical dbug
c Updated...
      real*4 cl(202),cs(202)
c Local variables...
      integer k,cnt
      real rthresh
      character*80 msg
  100 format('Rho threshold=',f5.1)
  101 format('Mark',i3,' rho=',f6.3,' is too small')

      call xvp('rthresh',rthresh,cnt)
      write(msg,100) rthresh
      call xvmessage(msg,0)

      do 20 k=1,202
      if (cl(k).eq.-999) goto 20	!already rejected
      if (crho(k).lt.rthresh) then
         cl(k) = -999			!reject the measurement
         cs(k) = -999
         if (dbug) then
            write(msg,101) k,crho(k)
            call xvmessage(msg,0)
         endif
      endif
   20 continue

      return
      end
