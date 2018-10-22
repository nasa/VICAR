ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Far Encounter mode:  If line and sample of planet center is input via
c parameters, compute OM matrix using FARENC algorithm (momati).
c
      subroutine farenc_mode(rdata,om,rs)
      implicit none
      real rdata(40)
      real*8 om(9),rs(3)

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      integer ind,ifar,count,def
      real r,far(2)
      real*8 lo,so,lssp,sssp,pixpmm,fic,bl,phi,tht,vabs
      logical xvptst

      ifar = 0
      if (xvptst('FARENC')) ifar=1

      call xvparm('ISSCPT',far,count,def,' ')	!if image space line-sample,
      if (def.eq.0) then			!convert to object space.
         call convisos(project,icam,far(1),far(2),rdata(33),rdata(34),
     +               1,conv(9),nph,npv,ind)
         ifar = 1
      endif

      call xvparm('OSSCPT',far,count,def,' ')
      if (def.eq.0) then
         rdata(33) = far(1)	!object space line
         rdata(34) = far(2)	!object space sample
         ifar = 1
      endif

      call xvparm('SSCPT',far,count,def,' ')
      if (def.eq.0) then
         rdata(33) = far(1)	!object space line
         rdata(34) = far(2)	!object space sample
         ifar = 1
      endif

      if (ifar.eq.0) return		!skip if not farenc mode
      call xvmessage('FARENC mode specified',' ')
      call xvparm('NORANGLE',r,count,def,' ')
      if (def.eq.0) rdata(35)=r       !north angle

      fic = rdata(27)		!focal length (mm)
      lo = rdata(28)		!optical axis intercept line
      so = rdata(29)		!optical axis intercept sample
      pixpmm = rdata(30)	!o.s. scale (pixels/mm)
      lssp = rdata(33)		!subspacecraft point line
      sssp = rdata(34)		!subspacecraft point sample
      phi = rdata(31)		!s/c latitude in degrees
      bl = rdata(32)		!s/c longitude in degrees
      tht = rdata(35)		!North angle (90 degrees from up)
      vabs = rdata(38)		!range to target body (km)

      if (tht.eq.-999.) call mabend('NORANGLE parameter not specified')

c compute om matrix and rs vector.
      call momati(lo,so,lssp,sssp,pixpmm,fic,bl,phi,tht,vabs,om,rs)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Check for tiepoints mode parameter.
c If present, use tiepoints to compute OM matrix.
c
      subroutine tiepoints_mode(rdata,om,rs)
      implicit none
      real rdata(40)
      real*8 om(9),rs(3)


      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      integer i,j,npoint,ind,count,def
      real is_line,is_samp,os_line,os_samp,cl,cs
      real aa(100),bb(6)

c tiepoints mode allows up to 25 tiepoints of 4 words each
c it returns om matrix and rs vector

      call xvparm('TIEPOINT',aa,count,def,' ')
      if (def.ne.0) return	!skip if tiepoints mode not specified
      npoint=count/4
      call xvmessage('TIEPOINTS mode specified:',' ')
      call prnt(4,1,npoint,'0NUMBER OF TIEPOINTS =.')
      call prnt(7,npoint*4,aa,' TIEPOINTS     =         .')

      if (idist.eq.1) then	!convert tiepoints from is to os
         do i=1,npoint,4
            j=i*4-3
            is_line=aa(j)
            is_samp=aa(j+1)
            call convisos(project,icam,is_line,is_samp,
     +           os_line,os_samp,1,conv(9),nph,npv,ind)
            if (ind.ne.0) call prnt(4,1,ind,'CONVISOS: bad ind=.')
            aa(j)=os_line
            aa(j+1)=os_samp
         enddo
      endif

c set up parameters for fomclv
      bb(1) = rdata(27)*rdata(30)	!fl*scale
      bb(2) = rdata(26) - rdata(25)	!req - rpole
      bb(3) = rdata(38)			!rmag
      bb(4) = rdata(31)			!spacecraft lat
      bb(5) = rdata(32)			!spacecraft long
      bb(6) = rdata(26)			!req
      cl = rdata(28)			!optical axis line
      cs = rdata(29)			!optical axis samp

      call fomclv(ind,npoint,aa,bb,om,rs,cl,cs)		!compute om
      if (ind.ne.0) then
         call prnt(4,1,ind,'0TIEPOINT MODE ERROR,IND=.')
         call abend
      endif
      return
      end
