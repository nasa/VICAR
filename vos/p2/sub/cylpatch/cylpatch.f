      SUBROUTINE CYLPATCH (rdata)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                          CYLPATCH
C       ----------------                                          --------
C	General routine for computing ZC, the line intersecting the 
C       equator, CPSI, the longitude at sample 1, and CSAM00, 
C       the sample at longitude zero.
C
C	Fortran format of call:
C
C	CALL CYLPATCH (RDATA)
C
C	"C" format of call:
C
C	cylpatch (rdata);
C
C	Parameters:-
C
C	RDATA   (input/output)  REAL Input Matrix.
C
C   REVISION HISTORY
C
C      20-05-94   CRI  MSTP S/W Conversion (VICAR Porting)
C      11-05-98   RRP  Updated tcylpatch to compile under
C                 vms system by merging the divided string
C                 parameter to xvmessage to max possible
C                 string. 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     this code was lifted from MAP2 cylcen subroutine
      implicit none
      real*4 rdata(40),PIFAC
      real*4 cphi,cpsi,xc,zc,f,req,rpole,fl
      real*4 phi,sinphi,rfl,radius,CSAM00

      integer*4 l

      data PIFAC/.017453293/

      call  ifmessage ('CYLPATCH version 11-May-98') 

      call mve(4,1,rdata(39),l,1,1)  ! Check Type of Data
      if(l.ne.9)return

      cphi=rdata(3)
      cpsi=rdata(6)
      xc=rdata(1)
      zc=rdata(2)
      f=rdata(7)
      req=rdata(26)
      rpole=rdata(25)
      fl=rpole

C     THIS ROUTINE COMPUTES ZC, THE LINE INTERCEPTING THE EQUATOR,
C     CPSI = THE LONGITUDE AT SAMPLE 1, AND CSAM00 = THE SAMPLE OF LONG ZERO.

      IF(xc.EQ.1..AND.cphi.EQ.0.)GO TO 50
C        GET W. LONG AT SAMPLE 1
2        cpsi=f*(xc-1.)/req/PIFAC+cpsi
         xc=1.
         cpsi=AMOD(720.+cpsi,360.)
C        FIND LINE OF EQUATOR
         phi=cphi*PIFAC
         sinphi=SIN(phi)
         rfl=(req/fl)**2                               !new
         radius=req/(f*sqrt(rfl+(1.-rfl)*cos(phi)**2)) !new
         zc=zc+sinphi*radius                           !new

C        ROUND
         zc=FLOAT(INT(zc+0.5))
         cphi=0.
         rdata(2)=zc
         rdata(3)=cphi

C        CALCULATE SAMP OF LONG ZERO AND ROUND
50    CSAM00=req*cpsi*PIFAC/f+1.
      CSAM00=FLOAT(INT(CSAM00+0.5))

C     ADJUST LONGITUDE OF SAMPLE 1.
      cpsi=f*(CSAM00-1.)/req/PIFAC
      rdata(1)=CSAM00
      rdata(6)=cpsi
      RETURN
      END
