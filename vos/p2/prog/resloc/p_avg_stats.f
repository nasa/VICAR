ccccccccccccccccccccccccccccccccccccccc
c Print the inputs...

      subroutine p_avg_stats(ceps,crho,cq,cl)
      implicit none
c Inputs...
      real*4 ceps(202),crho(202),cq(202),cl(202)
c Local variables...
      integer k,n
      real esum,rsum,qsum
      real eavg,ravg,qavg
      character*80 msg

      n = 0
      esum = 0
      rsum = 0
      qsum = 0
      do k=1,202
         if (cl(k).ne.-999) then
            n = n + 1
            esum = esum + ceps(k)
            rsum = rsum + crho(k)
            qsum = qsum + cq(k)
         endif
      enddo
      if (n.gt.0) then 
         eavg = esum/n
         ravg = rsum/n
         qavg = qsum/n
      else
         eavg = -999
         ravg = -999
         qavg = -999
      endif

      write(msg,101) eavg
  101 format('average eps=',f8.3)
      call xvmessage(msg,0) 

      write(msg,102) ravg
  102 format('average rho=',f8.3)
      call xvmessage(msg,0) 

      write(msg,103) qavg
  103 format('average q=',f8.3)
      call xvmessage(msg,0) 

      write(msg,109) 202-n
  109 format(i3,' reseau marks not found')
      call xvmessage(msg,0)
      return
      end
