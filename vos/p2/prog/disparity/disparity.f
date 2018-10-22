c program disparity

      include 'VICMAIN_FOR'
      subroutine main44
      
      parameter (nlmax=1100,nsmax=1100)
      real*4 ldis(nsmax,nlmax),sdis(nsmax,nlmax)
      real*4 dis(nsmax,nlmax)
      real*4 unitv(2),rd(2),bounds(2)
      integer*4 count,def,status,unit(2),ounit
c      logical xvptst

      flag=-10

c parameters
      call xveaction('SA',' ')
      call xvparm('BOUNDS',bounds,count,def,2)
      if(flag.gt.bounds(1))flag=bounds(1)

c open inputs
      do i=1,2
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
        if(nl.gt.nlmax)then
          call xvmessage('Line column too long',' ')
          call abend
        endif
      enddo

c open output
      call xvunit(ounit,'OUT',1,status,' ')
        call xvopen(ounit,status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')

c read input images
      do j=1,nl
        call xvread(unit(1),ldis(1,j),status,'LINE',j,' ')
      enddo
      do j=1,nl
        call xvread(unit(2),sdis(1,j),status,'LINE',j,' ')
      enddo

c compute radial disparity & put into buf.
      cs=ns/2.0
      cl=nl/2.0
      do j=1,nl
        do i=1,ns

          dis(i,j)=flag ! bad point flag
          if(ldis(i,j).eq.0.0.and.sdis(i,j).eq.0.0)goto 10
          
          rd(1)=sdis(i,j)-i
          rd(2)=ldis(i,j)-j

c         Radial disparity
          unitv(1)=i-cs
          unitv(2)=j-cl
          a=sqrt(unitv(1)**2+unitv(2)**2)
          if(a.eq.0.0)a=1.
          unitv(1)=unitv(1)/a
          unitv(2)=unitv(2)/a
c          disr=sqrt((rd(1)*unitv(1))**2+(rd(2)*unitv(2))**2)
          disr=sqrt(rd(1)**2+rd(2)**2)

c         Check angle between disparity vector and radial direction.
          a=sqrt(rd(1)**2+rd(2)**2)
          rd(1)=rd(1)/a
          rd(2)=rd(2)/a
          costh=rd(1)*unitv(1)+rd(2)*unitv(2)
          th=acos(costh)*57.3
          if(th.gt.20.)goto 10

          dis(i,j)=disr
10        continue          
        enddo
      enddo
      
c remove radial geom
      j=nl/2
      n=0
      slope=0.0
      do j=1,nl,10
        do i=1,ns,10
          rad=sqrt((i-cs)**2+(j-cl)**2)
          if(dis(i,j).ne.flag.and.rad.gt.0.0)then
            slope=slope+dis(i,j)/rad
            n=n+1
          endif
        enddo
      enddo
      slope=slope/n
      do j=1,nl
        do i=1,ns
          rad=sqrt((i-cs)**2+(j-cl)**2)
          if(dis(i,j).ne.flag)then
            dis(i,j)=dis(i,j)-slope*rad
          endif
        enddo
      enddo

c fill in the out of bounds dn's 
20    continue
      nc=0
      do j=2,nl-1
        do i=2,ns-1
          if(dis(i,j).gt.bounds(1).and.dis(i,j).lt.bounds(2))goto 21
          sum=0.0
          n=0
          do jj=j-1,j+1
            do ii=i-1,i+1
              if(i.eq.ii.and.j.eq.jj)goto 22
              if(dis(ii,jj).gt.bounds(1).and.dis(ii,jj).lt.
     +          bounds(2))then
                sum=sum+dis(ii,jj)
                n=n+1
              endif
22            continue
            enddo
          enddo
          if(n.gt.0)then
            dis(i,j)=sum/n
            nc=nc+1
          endif
21        continue
        enddo
      enddo
      if(nc.gt.0)then
        write(*,*)nc,' pixels interpolated'
        goto 20
      endif
      
c write radial disparity
      do j=1,nl
        call xvwrit(ounit,dis(1,j),status,' ')
      enddo

      end





