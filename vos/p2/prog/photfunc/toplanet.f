ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Convert from (line,sample) to vector from target-center to surface point.
c
      subroutine toplanet(line,samp,v,ind)
      implicit none
      integer*4 line,samp
      real*8 v(3)
      integer ind	!1=normal, other=point off planet

      common/cmp/mp
      real*8 mp

      common/cplanet/vsc,ra,rb,rc
      real*8 vsc(3),ra,rb,rc

      common/c3/rdata
      real rdata(40)
      real*8 data(20),om(3,3),rs(3)
      real fic,lo,so,pixpmm             !camera focal length, op-axis, scale
      integer idata(40),itype
      equivalence (data,rdata,idata),(data(1),om),(data(10),rs),
     * (rdata(27),fic),(rdata(28),lo),(rdata(29),so),(rdata(30),pixpmm),
     * (idata(39),itype)

      common/dist/idist,nph,npv,conv,icam,project
      integer*4 idist,nph,npv,icam
      real*4 conv(2720)
      character*5 project

      common/konst/rad
      real rad

      real is_line,is_samp,os_line,os_samp
      real*8 rline,rsamp,rlat,rlon,gcr,lora
      integer ll_type

      if (idist.eq.1) then
         is_line = line
         is_samp = samp
         call convisos(project,icam,is_line,is_samp,os_line,os_samp,
     +               1,conv(9),nph,npv,ind)
         rline = os_line
         rsamp = os_samp
      else
         rline = line
         rsamp = samp
      endif

      if (itype.ne.16) then
         ll_type = 1
         call mp_xy2ll(mp,rline,rsamp,rlat,rlon,ll_type,ind)
         if (ind.eq.0) ind=1
         lora = rdata(36)
         call ellipse_radius(rlat,rlon,ra,rb,rc,lora,gcr)
         call latrec(gcr,-rlon/rad,rlat/rad,v)
      else
         call ellipse_inv(om,rs,fic,lo,so,pixpmm,ra,rb,rc,rline,rsamp,
     +		v,ind)
      endif
      return
      end

cccccccccccccccccccccccccccccccc
c Calculate phase, incidence, and emission angles.
c
      subroutine light_angles(v,tp,cp,ci,ce)
      implicit none
      real*8 v(3)		!Point where angles are calculated
      real tp,cp		!Tangent and cosine of phase angle
      real ci,ce		!Cosine of incidence and emission angles

      common/cplanet/vsc,ra,rb,rc
      real*8 vsc(3),ra,rb,rc

      common/csun/vsun,s,ax,bx,cx
      real*8 vsun(3),s(3),ax,bx,cx

      real*8 n(3),c(3)	!unit vectors
      real*8 magnitude,sp,dcp

c compute unit normal at surface point
      n(1) = v(1)*ax
      n(2) = v(2)*bx
      n(3) = v(3)*cx
      call unorm(n,n,magnitude)      

c compute unit vector from surface point to spacecraft
      c(1) = vsc(1) - v(1)
      c(2) = vsc(2) - v(2)
      c(3) = vsc(3) - v(3)
      call unorm(c,c,magnitude)

c The unit vector from surface point to sun is assumed to be constant for the
c entire image.  Tests show this to be valid, even for Mercury.  The vector
c is therefore computed once in the main routine.
ccc      s(1) = vsun(1) - v(1)
ccc      s(2) = vsun(2) - v(2)
ccc      s(3) = vsun(3) - v(3)
ccc      call unorm(s,s,magnitude)

c cosine of phase angle = S o C
      dcp = s(1)*c(1) + s(2)*c(2) + s(3)*c(3)
      if (dcp.gt.1.d0) dcp=1.d0
      if (dcp.lt.-1.d0) dcp=-1.d0
      cp = dcp

c tangent of phase angle:
      sp = dsqrt(1.d0-dcp**2)
      if (dabs(dcp).gt.0.000000001d0) then
         tp = sp/dcp
      else
         tp = 1000.
      endif

c cosine of incidence angle = n o s
      ci = n(1)*s(1)+n(2)*s(2)+n(3)*s(3)
      if (ci.gt.1.d0) ci=1.d0
      if (ci.lt.-1.d0) ci=-1.d0

c cosine of emission angle = n o c
      ce = n(1)*c(1)+n(2)*c(2)+n(3)*c(3)
      if (ce.gt.1.d0) ce=1.d0
      if (ce.lt.-1.d0) ce=-1.d0
      return
      end
