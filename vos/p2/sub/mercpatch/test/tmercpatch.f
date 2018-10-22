      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer(a-z)
      real    rdata(40)
      integer idata(40)
      equivalence(idata,rdata)
      idata(39)=6       !mercator
      rdata(1)=50.      !sample
      rdata(2)=19.34389 !line
      rdata(3)=0.       !lat
      rdata(6)=205.3172 !west long
      rdata(7)=100.     !scale
      rdata(25)=1815.   !io radii
      rdata(26)=rdata(25)
      call prnt(7,26,rdata,'input data=.')
      call XVMESSAGE
     *('input lat=0.,long=205.3172,line=19.34389,sample=50.',' ')
      call XVMESSAGE('input is mercator, scale=100.,target is io',' ')
      call mercpatch(rdata)
      call prnt(7,1,rdata(1),'sample  = .')
      call prnt(7,1,rdata(2),'line    = .')
      call prnt(7,1,rdata(3),'latitude= .')
      call prnt(7,1,rdata(6),'longitude=.')
      call prnt(7,26,rdata,'output data=.')
      call XVMESSAGE
     *('output should be line=1.,samp=1.,lati=50.,long=360.',' ')
c     line=1,samp=1,lati=50.,long=360. (or 0.)
      return
      end
