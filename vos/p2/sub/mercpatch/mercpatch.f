      subroutine mercpatch(rdata)
c subroutine to calulate lat,long at line 1,sample 1 for mercator projection
c It updates rdata buffer elements 1 and 2.

      real scl,rdata(40),rad,req,leq,pi,smp,alog,atan
      data pi/3.1415926/
C==================================================================
      call mve(4,1,rdata(39),l,1,1)
      if(l.ne.6)return
      SCL=RDATA(7)
      RAD=57.29578
      REQ=RDATA(26)
      IF(RDATA(2).NE.1.)THEN
C        FIND LINE OF EQUATOR
         LEQ=(REQ/SCL)*ALOG(TAN(PI/4.+RDATA(3)/(2.*RAD)))+RDATA(2)
C        FIND LATITUDE OF LINE 1.
         RDATA(3)=(2.*(ATAN(EXP(SCL*(LEQ-1.)/REQ))-PI/4.)*RAD)
         RDATA(2)=1.
      ENDIF
      IF(RDATA(1).NE.1.)THEN
C        FIND SAMPLE OF PRIME MERIDIAN
         SMP=(REQ/SCL)*(RDATA(6)/RAD)+RDATA(1)
C        FIND LONGITUDE OF SAMPLE 1.
         RDATA(6)=((SMP-1.)*SCL*RAD)/REQ
         RDATA(1)=1.
      ENDIF
      return
      end

