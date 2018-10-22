c program bestfocus

      include 'VICMAIN_FOR'
      subroutine main44
      
      parameter (nlmax=1100,nsmax=1100,maxpix=10)
      real*4 buf(nsmax,nlmax),obuf(nsmax,nlmax,maxpix)
      real*4 inbuf(nsmax,nlmax,maxpix)
      integer*4 sborder,count,def,status,unit(maxpix),ounit(2)
      real*8 sum(nsmax),sumc(nsmax),sumc2(nsmax),normalize
      real*8 sum2(nsmax)
      logical inverse,xvptst

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      if(nin.gt.maxpix)then
        write(*,*)'Too many input images, increase maxpix'
        call abend
      endif
      call xvpcnt('OUT',nout)
      call xvparm('NLW',nlw,count,def,1)
      if(nlw.eq.(nlw/2)*2)nlw=nlw+1
      call xvparm('NSW',nsw,count,def,1)
      if(nsw.eq.(nsw/2)*2)nsw=nsw+1
      inverse=xvptst('INVERSE')

c open inputs
      do i=1,nin
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

c open outputs
        call xvunit(ounit(1),'OUT',1,status,' ')
        call xvopen(ounit(1),status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')
        call xvunit(ounit(2),'OUT',2,status,' ')
        call xvopen(ounit(2),status,'U_FORMAT','REAL',
     +              'OP','WRITE','O_FORMAT','REAL',' ')

      normalize=nlw*nsw
      sborder=(nsw-1)/2
      lborder=(nlw-1)/2
      nlo=nl+2*lborder
      nso=ns+2*sborder
      if(nlo.gt.nlmax.or.nso.gt.nsmax)then
        write(*,*)'internal image buffer too small'
        stop
      endif

      image=0
10    image=image+1
      do j=1,nl
        call xvread(unit(image),buf(1,j),status,'LINE',j,' ')
      enddo

c save inputs
      do j=1,nl
        do i=1,ns
          inbuf(i,j,image)=buf(i,j)
        enddo
      enddo

c shift image area to leave a border around it.
      do j=nl,1,-1
        do i=ns,1,-1
          buf(i+sborder,j+lborder)=buf(i,j)
        enddo
      enddo

c reflect the image into the borders
      k=lborder
      do j=lborder,1,-1
        k=k+1
        do i=1,nso
          buf(i,j)=buf(i,k)
        enddo
      enddo
      k=nlo-lborder+1
      do j=nlo-lborder+1,nlo
        k=k-1
        do i=1,nso
          buf(i,j)=buf(i,k)
        enddo
      enddo
      k=sborder
      do i=sborder,1,-1
        k=k+1
        do j=1,nlo
          buf(i,j)=buf(k,j)
        enddo
      enddo
      k=nso-sborder+1
      do i=nso-sborder+1,nso
        k=k-1
        do j=1,nlo
          buf(i,j)=buf(k,j)
        enddo
      enddo

c collect first sums of columns to get started.
      do i=1,nso
        sumc(i)=0.d0
        sumc2(i)=0.d0
        do j=1,nlw
          sumc(i)=sumc(i)+buf(i,j)
          sumc2(i)=sumc2(i)+dble(buf(i,j))**2
        enddo
      enddo

      do j=1,nl

c compute low pass
        sum(1)=0.d0
        sum2(1)=0.d0
        do i=1,nsw
          sum(1)=sum(1)+sumc(i)
          sum2(1)=sum2(1)+sumc2(i)
        enddo
        do i=2,ns
          sum(i)=sum(i-1)+sumc(i+nsw-1)-sumc(i-1)
          sum2(i)=sum2(i-1)+sumc2(i+nsw-1)-sumc2(i-1)
        enddo

c update column sums
        do i=1,nso
          sumc(i)=sumc(i)+buf(i,j+nlw)-buf(i,j)
          sumc2(i)=sumc2(i)+dble(buf(i,j+nlw))**2-dble(buf(i,j))**2
        enddo

c compute standard deviation
        do i=1,ns
          obuf(i,j,image)=sqrt(sum2(i)/normalize-(sum(i)/normalize)**2)
        enddo

      enddo

      if(image.lt.nin)goto 10

c create best focus image
      do j=1,nl
        do i=1,ns
          best=-1.0
          do k=1,nin
            if(obuf(i,j,k).gt.best)then
              best=obuf(i,j,k)
              n=k
            endif
          enddo
          buf(i,j)=inbuf(i,j,n)
        enddo
        call xvwrit(ounit(1),buf(1,j),status,' ')
      enddo

      if(nout.lt.2)return

c create relative topographic image
      nlow=0
      nhigh=0
      do j=1,nl
        do i=1,ns
          best=-1.0
          do k=1,nin
            if(obuf(i,j,k).gt.best)then
              best=obuf(i,j,k)
              n=k ! the index of the highest point
            endif
          enddo
          if(n.eq.1)then
            buf(i,j)=n
            nlow=nlow+1
          else if(n.eq.nin)then
            buf(i,j)=nin
            nhigh=nhigh+1
          else
            a1=obuf(i,j,n-1)
            b1=obuf(i,j,n)
            c1=obuf(i,j,n+1)
            d=2.0*b1-c1-a1
            if(d.ne.0.0)then
              buf(i,j)=n-0.5*(a1-c1)/d
            else
              buf(i,j)=n
            endif
          endif
          if(inverse)buf(i,j)=nin-buf(i,j)+1
        enddo
        call xvwrit(ounit(2),buf(1,j),status,' ')
      enddo
      if(nlow.gt.0)then
        write(*,*)100.*real(nlow)/real(nl*ns),
     +  ' % of points had a solution below image 1, set to 1'
      endif
      if(nhigh.gt.0)then
        write(*,*)100.*real(nhigh)/real(nl*ns),
     +  ' % of points had a solution above image ',nin,
     +  ' ,set to ',nin
      endif

      end





