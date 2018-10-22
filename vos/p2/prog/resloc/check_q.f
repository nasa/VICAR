cccccccccccccccccccccccccccccc
c Check for low match quality...

      subroutine check_q(cq,cl,cs,dbug)
      implicit none
c Inputs...
      real*4 cq(202)
      logical dbug
c Updates...
      real*4 cl(202),cs(202)
c Local variables...
      integer k,count
      real qthresh
      character*80 msg
  100 format('q threshold=',f5.2)
  101 format('Mark',i3,' q=',f7.3,' is too small')

      call xvp('qthresh',qthresh,count)
      write(msg,100) qthresh
      call xvmessage(msg,0)

      do 20 k=1,202
      if (cl(k).eq.-999) goto 20
      if (cq(k).lt.qthresh) then
         cl(k) = -999
         cs(k) = -999
         if (dbug) then
            write(msg,101) k,cq(k)
            call xvmessage(msg,0)
         endif
      endif
   20 continue

      return
      end
