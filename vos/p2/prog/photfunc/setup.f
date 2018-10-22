c ********************************************************************
c set up the brightness correction buffer for photfunc
c
      subroutine setup(iprnt,gbuf,line,ncol,bbrite,numoff)
      implicit none
      integer iprnt		!print brightness values
      real gbuf(1)		!grid buffer
      integer line		!image line number
      integer ncol		!ncol = nso/sinc
      real bbrite(ncol)		!returned bright values
      integer numoff		!running count of bad points

      common/pf0/linc,sinc,grid,term,limb,const,maxcor
      integer linc,sinc,grid
      real term,limb,const,maxcor

      common/pf2/k,w,b,h,c,ck,d,xlg2
      real k,w,b,h,c,ck,d(6),xlg2

      common/pix_size/sl,ss,nlo,nso,nli,nsi
      integer sl,ss,nlo,nso,nli,nsi

      common/konst/rad
      real rad

      integer i,j,icol,samp,nsamp,ind
      real pdata(5)
      real*8 v(3),gcr,dlat,dlon

c bbrite(icol)=0.   means point off target or past terminator.
c bbrite(icol)=-1.  means point off target or past terminator.
c                   and violates limb * term limits.
c bbrite(icol)=>0.   means point ok.

      do 100 icol=1,ncol
      j = (icol-2)*sinc
      nsamp = min0(nso-j,sinc)
      samp = ss + j + nsamp
      call toplanet(line,samp,v,ind)
      if (ind.ne.1) then
         numoff=numoff+1		!point is off the target
         bbrite(icol)=0.0
         goto 100
      endif

      if (iprnt.gt.0 .or. grid.ne.0) then
         call reclat(v,gcr,dlon,dlat)
         dlon = 360.-dlon*rad
         dlat = dlat*rad
      endif

      if (grid.ne.0) then
         call interp(grid,dlat,dlon,gbuf,pdata)
         k=pdata(1)	!note this code is not valid for ihapke > 1
         w=pdata(1)
         b=pdata(2)
         h=pdata(3)
         c=pdata(4)
         ck=pdata(5)
      endif
         
      call phot_sub(v,limb,term,bbrite(icol)) !compute correction
      if (bbrite(icol).eq.0.) numoff=numoff+1

      if (iprnt.gt.0) call xprint(iprnt,line,samp,
     +		dlat,dlon,bbrite(icol),icol)
  100 continue

      do i=1,ncol
         bbrite(i)=bbrite(i)*const	!add in free multiplicative constant
      enddo
      return
      end

c ********************************************************************
      subroutine interp(grid,lat,lon,buf,pdata)
      implicit none
      integer grid
      real lat,lon,buf(1),pdata(5)

      common/pf1/minn,icook,ihapke
      integer minn,icook,ihapke

       common/c9/gdlat,gdlon,gslat,gslon,numlat,numlon,nlrg,nlg,nbg
     &    ,numsam
      real gdlat,gdlon,gslat,gslon
      integer numlat,numlon,nlrg,nlg,nbg,numsam

      integer nwords,numgrp,num,igrp,jgrp,line,samp,ilat,ilon
      integer i,ind
      real dellat,dellon,vall,valr
      real ul(5),ur(5),ll(5),lr(5)

      if (minn.gt.2 .or. minn.lt.1) return	!skip if minnaert or hapke
      if (minn.eq.1) nwords=2
      if (minn.eq.2) then
         nwords=5
         if (ihapke.eq.1 .and. icook.eq.0) nwords=4
      endif

      numgrp = numsam/nwords	!number of groups (lat/lon points) per line
      if (lon.gt.gslon) then
         ilon = (lon-gslon)/gdlat+1
      else
	ilon = (360.+lon-gslon)/gdlat+1
      endif
      ilat = (lat-gslat)/gdlat+1
      igrp = (ilat-1)*numlon+ilon
      jgrp = mod(igrp-1,numgrp)+1
      line = (igrp-jgrp)/numgrp+1
      samp = 1+(igrp-1)*nwords

      call xvread(grid, buf, ind, 'LINE', line,' ')
      call mve(4,nwords,buf(samp),ul,1,1)
      call mve(4,nwords,buf(samp+nwords),ur,1,1)
      call xvread(grid, buf, ind, 'LINE', line+1,' ')
      call mve(4,nwords,buf(samp),ll,1,1)
      call mve(4,nwords,buf(samp+nwords),lr,1,1)

c  interpolate
c  find position in box relative to upper left
      dellat = (lat-gslat+(ilat-1)*gdlat)/gdlat
      dellon = (lon-gslon+(ilon-1)*gdlon)/gdlon

c  compute values on left and right side of box
      num = nwords
      do i=1,num
         vall=(ll(i)-ul(i))*dellat+ul(i)
         valr=(lr(i)-ur(i))*dellat+ur(i)
         pdata(i)=(valr-vall)*dellon+vall
      enddo
      return
      end

c********************************************************************
      subroutine xprint(iprnt,line,samp,lat,long,val,icol)
      implicit none
      integer iprnt,line,samp,icol
      real lat,long,val

      common/pf4/refl,ci,ce,cg,tg,rlat,rlon
      real refl,ci,ce,cg,tg,rlat,rlon

      common/konst/rad
      real rad

      real inc,emis,phase	!incidence, emission, and phase angles

      character*132 msg
      data msg /' '/

      if (iprnt.eq.2 .and. ci.lt.0.) return	!skip unilluminated points

      inc=(acos(ci))*rad
      emis=(acos(ce))*rad
      phase=(acos(cg))*rad

      write(msg(1:7), '(I7)') line		
      write(msg(8:15), '(I8)') samp		
      write(msg(17:25), '(f9.4)') lat		
      write(msg(26:35), '(f10.4)') long		
      write(msg(38:43), '(f6.2)') inc		
      write(msg(45:50), '(f6.2)') emis		
      write(msg(51:57), '(f7.2)') phase		
      write(msg(60:65), '(f6.3)') val		
      write(msg(66:70), '(I4)') icol		
      call xvmessage(msg,' ')
      return
      end
