      subroutine rectpatch(rdata)

C     RETURNS CALCULATED LINE OF LATITUDE 0. AND LONGITUDE AT SAMPLE 1

      REAL*8 PI,CONST,REQ,SCALE,RSAMP,RLAT
      real rdata(40)
      DATA pi/3.141592653589793D0/
C==================================================================
      call mve(4,1,rdata(39),l,1,1)
      if(l.ne.10)return
      SCALE=rdata(7)   ! KM/PIXEL
      req=rdata(26)
      flag=-999
      RLINE=rdata(2)
      RSAMP=rdata(1)
      RLAT=rdata(3)
      RLON=rdata(6)
      IF(RLAT.EQ.0..AND.RSAMP.EQ.1.)RETURN
      CONST=SCALE/REQ/PI*180.  !DEGREES/PIXEL

C     CALCULATE LONGITUDE
      RLON=RLON+CONST*(RSAMP-1.)
      if(rlon.eq.0.)rlon=360.
      IF(RLON .GT. 360.) RLON=AMOD(360.+RLON,360.)

C     CALCULATE LINE OF EQUATOR
      RLINE=RLINE+RLAT/CONST
      rdata(1)=1.
      rdata(2)=RLINE
      rdata(3)=0.
      rdata(6)=RLON
      if(rdata(6).eq.0.)rdata(6)=360.
      RETURN
      END

