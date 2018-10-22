      INCLUDE 'VICMAIN_FOR'
      subroutine main44
      implicit integer(a-z)
      real*4 rdata(40)
      integer idata(40)
      equivalence(idata,rdata)
      idata(39)=10      !simple cylindrical
      rdata(1)=49.99999      !sample  (changed from 50 to make output prettier.)
      rdata(2)=50.      !line
      rdata(3)=-64.6828 !lat
      rdata(6)=205.3172 !west long
      rdata(7)=100.     !scale
      rdata(25)=1815.   !io radii
      rdata(26)=rdata(25)
      call prnt(7,26,rdata,'input data=.')
      CALL XVMESSAGE('AT LINE=50,SAMP=50,LAT=-64.6828,LONG=205.3172',
     .               ' ')
      call rectpatch(rdata)
      CALL prnt(7,1,RDATA(2),'LINE=.')
      CALL prnt(7,1,RDATA(1),'SAMPLE=.')
      CALL prnt(7,1,RDATA(3),'LAT=.')
      CALL prnt(7,1,RDATA(6),'LONG=.')
      CALL XVMESSAGE('SHOULD BE: LINE=29.51,SAMP=1.,LATI=0,LONG=360',
     .               ' ')
      call prnt(7,26,rdata,'output data=.')
c     line=1,samp=1,lati=50.,long=360. (or 0.)
      return
      end
