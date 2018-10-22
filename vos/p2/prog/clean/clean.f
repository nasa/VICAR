c
c program clean
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maximage=1000000, maxpsf=10000)
      real*4 pixbuf(maximage),psf(maxpsf),scr(maximage),noise,gain
      integer*4 inunit1,inunit2,ounit1,ounit2,def,count,status
      integer*4 ounit3
      integer*2 hist(maximage),maxh
      logical xvptst,convolve,total

c parameters
c      call xvpcnt('INP',nids)      
      convolve=xvptst('CONVOLVE')
      total=xvptst('TOTAL')
      call xvparm('GAIN',gain,count,def,1)
      call xvparm('NOISE',noise,count,def,1)
      call xvparm('ITER',maxiter,count,def,1)
      call xvparm('MAXMOD',maxmod,count,def,1)
      call xvparm('CHANGE',change,count,def,1)
      if(total)then
        call xvmessage('Mode is TOTAL',' ')
      else
        call xvmessage('Mode is POSITIVE',' ')
      endif

c read image into memory
      call xvunit(inunit1,'INP',1,status,' ')
      call xvopen(inunit1,status,'U_FORMAT','REAL',' ')
      call xvget(inunit1,status,'NL',nl,'NS',ns,' ')
      if(ns*nl.gt.maximage)then
          call xvmessage(' NS*NS of image storage buffer too small',' ')
          call abend
      endif
      k=1
      do j=1,nl                    ! line loop
          call xvread(inunit1,pixbuf(k),status,'LINE',j,' ')
          k=k+ns
      enddo

c read psf into memory
      call xvunit(inunit2,'INP',2,status,' ')
      call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
      call xvget(inunit2,status,'NL',nl2,'NS',ns2,' ')
      if(ns2*nl2.gt.maxpsf)then
          call xvmessage(' NS*NL of psf storage buffer too small',' ')
          call abend
      endif
      k=1
      do j=1,nl2                    ! line loop
          call xvread(inunit2,psf(k),status,'LINE',j,' ')
          k=k+ns2
      enddo

c normalize the psf to 1 max.
      call psfnorm(psf,nl2,ns2)

c open outputs
      call xvunit(ounit1,'OUT',1,status,' ')
      call xvopen(ounit1,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')
      call xvunit(ounit2,'OUT',2,status,' ')
      call xvopen(ounit2,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')
      call xvunit(ounit3,'OUT',3,status,' ')
      call xvopen(ounit3,status,'U_FORMAT','REAL','OP','WRITE',
     +    'O_FORMAT','REAL',' ')

c convolve input
      if(convolve)then
        call xvmessage('Convolving the input image',' ')
        call convolveit(pixbuf,nl,ns,psf,nl2,ns2,1,scr)
        do j=1,nl*ns
            pixbuf(j)=scr(j)
        enddo
      endif

c clean
      do j=1,nl*ns
          scr(j)=0.0
          hist(j)=0
      enddo
      call stats(pixbuf,nl,ns,total,sdev)
      old_sdev=sdev
      write(*,*)'initial residual ',sdev
      write(*,*)'iteration residual %decrease maxhist'
      do iter=1,maxiter
        if(total)then
          call clean_abs(pixbuf,nl,ns,psf,nl2,ns2,gain,noise,
     +                 hist,maxmod,scr)
        else
          call clean_pos(pixbuf,nl,ns,psf,nl2,ns2,gain,noise,
     +                 hist,maxmod,scr)
        endif
        if(mod(iter,ns*nl/10).eq.0)then
            call stats(pixbuf,nl,ns,total,sdev)
            step=100.*(old_sdev-sdev)/old_sdev
            old_sdev=sdev
            call maxhist(hist,nl,ns,maxh)
            write(*,100)iter,sdev,step,maxh
100         format(i8,2f10.3,i6)
            if(step.lt.change)goto 10
            if(sdev.lt.noise)goto 10
        endif
      enddo
      call stats(pixbuf,nl,ns,total,sdev)
      call maxhist(hist,nl,ns,maxh)
10    write(*,*)'final residual ',sdev
      write(*,*)iter,' iterations '
      write(*,*)'maxhist ',maxh

c write outputs 1 & 2
      k=1
      do j=1,nl
          call xvwrit(ounit1,scr(k),status,' ')
          call xvwrit(ounit2,pixbuf(k),status,' ')
          k=k+ns
      enddo

c convolve delta function image
      call convolveit(scr,nl,ns,psf,nl2,ns2,0,pixbuf)

c write output 3
      k=1
      do j=1,nl
          call xvwrit(ounit3,pixbuf(k),status,' ')
          k=k+ns
      enddo


      return
      end

c***************************************************************************
      subroutine maxhist(hist,nl,ns,maxh)
      integer*2 hist(ns,nl),maxh
      maxh=0
      do j=1,nl
        do i=1,ns
          if(hist(i,j).gt.maxh) maxh=hist(i,j)
        enddo
      enddo
      return
      end

c***************************************************************************
      subroutine stats(pix,nl,ns,total,sdev)
      logical total
      real*4 pix(ns,nl)
      sum=0.0
      if(total)then
        do j=1,nl
          do i=1,ns
            sum=sum+pix(i,j)**2
          enddo
        enddo
        sdev=sqrt(sum/(ns*nl))
      else
        n=0
        do j=1,nl
          do i=1,ns
            if(pix(i,j).gt.0.0)then
              sum=sum+pix(i,j)**2
              n=n+1
            endif
          enddo
        enddo
        if(n.eq.0)n=1
        sdev=sqrt(sum/n)
      endif
      return
      end

c***************************************************************************
      subroutine clean_pos(pix,nl,ns,psf,nl2,ns2,gain0,noise,
     +                   hist,maxmod,scr)
c clean algorithm
c pix is image of ns by nl
c psf is the psf of ns2 by nl2
c gain is the clean gain
c noise is the noise estimated standard deviation.
c maxmod is the maximum permitted number of changes permitted per pixel
c scr is the output image ns by nl
      real*4 pix(ns,nl),psf(ns2,nl2),gain,noise
      real*4 scr(ns,nl)
      integer*4 s,l
      integer*2 hist(ns,nl)

      ks=ns2/2
      kl=nl2/2
      gain=gain0

c find the max dn
      big=0.
      ii=0
      do j=1,nl
        do i=1,ns
          if(hist(i,j).lt.maxmod)then
            if(pix(i,j).gt.big)then
              big=pix(i,j)
              ii=i
              jj=j
            endif
          endif
        enddo
      enddo
      if(ii.eq.0)return

c subtract the psf
      l=0
      do j=jj-kl,jj+kl
        l=l+1
        s=0 
        do i=ii-ks,ii+ks
          s=s+1
          if(i.lt.1)goto 10
          if(i.gt.ns)goto 10
          if(j.lt.1)goto 10
          if(j.gt.nl)goto 10
          pix(i,j)=pix(i,j)-psf(s,l)*gain*big
10        continue
        enddo
      enddo

c update the delta function image
      scr(ii,jj)=scr(ii,jj)+gain*big

c update the histogram (number of times a pixel has been set)
      hist(ii,jj)=hist(ii,jj)+1

      return
      end

c***************************************************************************
      subroutine clean_abs(pix,nl,ns,psf,nl2,ns2,gain,noise,
     +                   hist,maxmod,scr)
c clean algorithm
c pix is image of ns by nl
c psf is the psf of ns2 by nl2
c gain is the clean gain
c noise is the noise estimated standard deviation.
c maxmod is the maximum permitted number of changes permitted per pixel
c scr is the output image ns by nl
      real*4 pix(ns,nl),psf(ns2,nl2),gain,noise
      real*4 scr(ns,nl)
      integer*4 s,l
      integer*2 hist(ns,nl)

      ks=ns2/2
      kl=nl2/2

c find the max dn
      big=-1.0
      ii=0
      do j=1,nl
        do i=1,ns
          if(hist(i,j).lt.maxmod)then
            if(abs(pix(i,j)).gt.big)then
              signbig=pix(i,j)
              big=abs(signbig)
              ii=i
              jj=j
            endif
          endif
        enddo
      enddo
      if(ii.eq.0)return

c subtract the psf
      l=0
      do j=jj-kl,jj+kl
        l=l+1
        s=0 
        do i=ii-ks,ii+ks
          s=s+1
          if(i.lt.1)goto 10
          if(i.gt.ns)goto 10
          if(j.lt.1)goto 10
          if(j.gt.nl)goto 10
          pix(i,j)=pix(i,j)-psf(s,l)*gain*signbig
10        continue
        enddo
      enddo

c update the delta function image
      scr(ii,jj)=scr(ii,jj)+gain*signbig

c update the histogram (number of times a pixel has been set)
      hist(ii,jj)=hist(ii,jj)+1

      return
      end

c***************************************************************************
c normalize psf to unity amplitude
      subroutine psfnorm(psf,nl2,ns2)
      real*4 psf(ns2,nl2)
      if(2*(ns2/2).eq.ns2)then
        write(*,*)'psf samples must be odd number'
        call abend
      endif
      if(2*(nl2/2).eq.ns2)then
        write(*,*)'psf lines must be odd number'
        call abend
      endif
      big=-1.0e+20
      do j=1,nl2
        do i=1,ns2
          if(psf(i,j).gt.big)then
            big=psf(i,j)
            ii=i
            jj=j
          endif
        enddo
      enddo
      do j=1,nl2
        do i=1,ns2
          psf(i,j)=psf(i,j)/big
        enddo
      enddo
      if((ii.ne.ns2/2+1).or.(jj.ne.nl2/2+1))then
        write(*,*)'psf center at sample ',ii,' line ',jj
        write(*,*)'should be at sample ',ns2/2+1,' line ',nl2/2+1
        call abend
      endif
      return
      end

c***************************************************************************
c convolve an image pix with a point spread function psf.
c pix is image of ns by nl
c psf is the psf of ns2 by nl2
c scr is the output of ns by nl
      subroutine convolveit(pix,nl,ns,psf,nl2,ns2,norm,scr)
      real*4 pix(ns,nl),psf(ns2,nl2),scr(ns,nl)
      integer*4 s,l
      ks=ns2/2
      kl=nl2/2
      do j=1,nl
        do i=1,ns
          sum=0.0
          sumw=0.0
          l=0
          do jj=j-kl,j+kl
            l=l+1
            s=0 
            do ii=i-ks,i+ks
              s=s+1
              if(ii.lt.1)goto 10
              if(ii.gt.ns)goto 10
              if(jj.lt.1)goto 10
              if(jj.gt.nl)goto 10
              sum=sum+pix(ii,jj)*psf(s,l)
              sumw=sumw+psf(s,l)
10            continue
            enddo
          enddo
          if(sumw.gt.0.0)then
            if(norm.eq.1)then
              scr(i,j)=sum/sumw
            else
              scr(i,j)=sum
            endif
          else
            scr(i,j)=0.0
          endif
        enddo
      enddo
      return
      end

