C************************************************************************
C  This test routine is used to test CYLPATCH, a FORTRAN subroutine.
C  CLYPATCH  computes the special line and sample point and special
C  latitude and longitude points for the normal cylindrical projection.
C  This test routine builds the necessary data in a standard MAP 
C  buffer.  After testing the subroutine using this FORTRAN driver, a
C  "C" version is invoked "tzcylpatch".  tzcylpatch uses a bridge
C  "zcylpatch" to invoke CYLPATCH.  The test cases are the same, only
C  in "C".
C************************************************************************
C  RDATA is a real*4 40 element array as described in CONVEV.
C   rdata(1) - sample 
C   rdata(2) - line
C   rdata(3) - Latitude 
C   rdata(6) - Longitude 
C   rdata(7) - scale (km/pixel)
C   rdata(39) - projection type = 9
C   rdata(25) - polar radius (km)
C   rdata(26) - equatorial radius (km)
C  Some values will be changed after execution of CYLPATCH.
C   rdata(1) - special sample point
C   rdata(2) - special line point
C   rdata(3) - Latitude at sample 1
C   rdata(6) - Longitude (west) at sample 1
C  If rdata(39) is not equal to 9 (integer), then data is not for a
C  cylindrical projection. CYLPATCH returns without making any changes.
C************************************************************************
  
      include 'VICMAIN_FOR'
      subroutine main44
      implicit none
      real rdata(40)
      integer idata(40),j
      character*80 msg
      equivalence(idata(1),rdata(1))
      idata(39)=9
      rdata(1)=1.
      rdata(2)=1.
      rdata(3)=85.7461
      rdata(6)=239.916
      rdata(7)=10.
      rdata(25)=1815.
      rdata(26)=1815.
      call xvmessage
     +     ('at line=1. sample=1. lati=85.7461 long=239.916',' ')
      call xvmessage('radius=1815., scal=10',' ')
      write(msg,40)(RDATA(j),j=25,26)
      call xvmessage(msg,' ')
      write(msg,50)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      call cylpatch(rdata)
      call xvmessage('output should be lati=0 at line=182, samp=761 long
     *=239.916',' ')
      write(msg,41) RDATA(1)
      call xvmessage(msg,' ')
      write(msg,42) RDATA(2)
      call xvmessage(msg,' ')
      write(msg,43) RDATA(3)
      call xvmessage(msg,' ')
      write(msg,44) RDATA(6)
      call xvmessage(msg,' ')
      write(msg,52)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      rdata(1)=100.
      rdata(2)=100.
      rdata(3)=26.8586
      rdata(6)=208.6638
      call xvmessage
     +    ('at line=100,samp=100,lati=26.8586,long=208.6638',' ')
      write(msg,50)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      call cylpatch(rdata)
      call xvmessage('output should be lati=0 at line=182, samp=761 long
     *=239.916', ' ')
      write(msg,41) RDATA(1)
      call xvmessage(msg,' ')
      write(msg,42) RDATA(2)
      call xvmessage(msg,' ')
      write(msg,43) RDATA(3)
      call xvmessage(msg,' ')
      write(msg,44) RDATA(6)
      call xvmessage(msg,' ')
      write(msg,52)(RDATA(j),j=1,5)
      call xvmessage(msg,' ')
      write(msg,51)(RDATA(j),j=6,10)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')
      call xvmessage('NOW TRY "C" INTERFACE',' ')
      call tzcylpatch 
40    format ('RADII=',2f10.4)
41    format ('sample=',f12.5)
42    format ('line=  ',f12.5)
43    format ('lati=  ',f12.5)
44    format ('long=  ',f12.5)
50    format ('input  data=',5f12.7)
51    format ('            ',5f12.7)
52    format ('output data=',5f12.7)
      return
      end
