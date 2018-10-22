c program bestscale

      include 'VICMAIN_FOR'
      subroutine main44
      
      parameter (nlmax=1100,nsmax=1100)
      real*4 inbuf(nsmax,nlmax),obuf(nsmax,nlmax)
      real*4 scale(2),refbuf(nsmax,nlmax)
      real*4 cor(100),sc(100)
      integer*4 count,def,status,unit,ounit
      real*8 r,sumx,sumy,sumx2,sumy2,sumxy

      call xvpcnt('INP',nin)
      call xvpcnt('OUT',nout)

c parameters
      call xveaction('SA',' ')
      call xvpcnt('INP',nin)
      call xvpcnt('OUT',nout)
      if(nin.ne.nout)then
        write(*,*)'inputs must match outputs in number'
        call abend
      endif
      call xvparm('SCALE',scale,count,def,2)

c open input1
      call xvunit(unit,'INP',1,status,' ')
      call xvopen(unit,status,'U_FORMAT','REAL',' ')
      call xvget(unit,status,'NL',nl,'NS',ns,' ')
      if(ns.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
      endif
      if(nl.gt.nlmax)then
          call xvmessage('Line column too long',' ')
          call abend
      endif

c open output1
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')

c copy first input to first output
      do j=1,nl
        call xvread(unit,refbuf(1,j),status,'LINE',j,' ')
        call xvwrit(ounit,refbuf(1,j),status,' ')
      enddo
      call xvclose(unit,status,'CLOS_ACT','FREE',' ')
      call xvclose(ounit,status,'CLOS_ACT','FREE',' ')

      sinc=(scale(2)-scale(1))/50. ! can go to 100
      cx=ns/2.0
      cy=nl/2.0
      inc=5
      mode=0 ! bilinear
      write(*,*)'Image   scale   correlation'
      write(*,*)1,1.0,1.0
      inp=1
10    inp=inp+1

c read next image
      call xvselpi(inp) ! use this input label for the output.
      call xvunit(unit,'INP',inp,status,' ')
      call xvopen(unit,status,'U_FORMAT','REAL',' ')
      do j=1,nl
        call xvread(unit,inbuf(1,j),status,'LINE',j,' ')
      enddo

c compute correlations between each image and refimage
c as a function of scale.
      k=0
      do s=scale(1),scale(2),sinc
        k=k+1
        n=0
        sumx=0.d0
        sumx2=0.d0
        sumy=0.d0
        sumy2=0.d0
        sumxy=0.d0
        do j=1,nl,inc
          y=(j-cy)*s+cy
          do i=1,ns,inc
            x=(i-cx)*s+cx
            call interpolate(inbuf,nlmax,nsmax,nl,ns,mode,
     +                         x,y,dn,ind)
            if(ind.eq.0)then
              n=n+1
              sumx=sumx+refbuf(i,j)
              sumx2=sumx2+refbuf(i,j)*refbuf(i,j)
              sumy=sumy+dn
              sumy2=sumy2+dn*dn
              sumxy=sumxy+refbuf(i,j)*dn
            endif
          enddo
        enddo
        r=(sumxy-sumx*sumy/n)**2/
     +    ((sumx2-sumx**2/n)*(sumy2-sumy**2/n))
        cor(k)=dsqrt(r)
        sc(k)=s
c        write(*,*)k,sc(k),cor(k)
      enddo
      
c obtain the best estimate for the scale
      best=-1.0
      do i=1,k
        if(cor(i).gt.best)then
          best=cor(i)
          n=i
        endif
      enddo
      if(n.eq.1)then
        s=sc(1)
      else if(n.eq.k)then
        s=sc(k)
      else
        s=sc(n)-0.5*
     +    ((sc(n)-sc(n-1))**2*(cor(n)-cor(n+1))-(sc(n)-sc(n+1))**2*
     +    (cor(n)-cor(n-1)))  /
     +    ((sc(n)-sc(n-1))*(cor(n)-cor(n+1))-(sc(n)-sc(n+1))*
     +    (cor(n)-cor(n-1)))
      endif
      write(*,*)inp,s,cor(n)
      
c generate image at best scale
      do j=1,nl
        y=(j-cy)*s+cy
        do i=1,ns
          x=(i-cx)*s+cx
          call interpolate(inbuf,nlmax,nsmax,nl,ns,mode,
     +                         x,y,dn,ind)
          obuf(i,j)=dn
        enddo
      enddo

c write next output
      call xvunit(ounit,'OUT',inp,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL',
     +              'OP','WRITE',' ')
      do j=1,nl
        call xvwrit(ounit,obuf(1,j),status,' ')
      enddo
      call xvclose(unit,status,'CLOS_ACT','FREE',' ')
      call xvclose(ounit,status,'CLOS_ACT','FREE',' ')
      
      if(inp.lt.nin)goto 10

      return
      end


      subroutine interpolate(inbuf,nlbuf,nsbuf,nl,ns,mode,
     +  x,y,dn,ind)
c inbuf of dimension nsbuf by nlbuf, input , real, ns by nl data.
c mode is the interpolation mode.
c mode=0 bilinear interpolation, 4 neighbors.
c mode=1 nearest neighbor.
c mode=2 2nd order, 6 neighbors.
c mode=3 sampling theorem, 15 by 15 convolution.
c mode=4 bicubic spline
c x,y are real coordinates of input point, input, real.
c dn is interpolated intensity, returned, real.
c ind=0 if dn computed ok. ind=1 if x,y out of bounds.
c    returned, integer.
c If ind=0 on return, dn is set to zero.

      parameter (kerw=9)
      real*4 inbuf(nsbuf,nlbuf),tw,bw,lw,rw
      real*4 kerx(-kerw:kerw),kery(-kerw:kerw)
      real*4 ya(4),y1(4),y2(4),y12(4),c(4,4),cl(16),xa(16)
      real*4 wt(16,16)
      DATA WT/1.,0.,-3.,2.,4*0.,-3.,0.,9.,-6.,2.,0.,-6.,
     *  4.,8*0.,3.,0.,-9.,6.,-2.,0.,6.,-4.,10*0.,9.,-6.,
     *  2*0.,-6.,4.,2*0.,3.,-2.,6*0.,-9.,6.,2*0.,6.,-4.,
     *  4*0.,1.,0.,-3.,2.,-2.,0.,6.,-4.,1.,0.,-3.,2.,8*0.,
     *  -1.,0.,3.,-2.,1.,0.,-3.,2.,10*0.,-3.,2.,2*0.,3.,
     *  -2.,6*0.,3.,-2.,2*0.,-6.,4.,2*0.,3.,-2.,0.,1.,-2.,
     *  1.,5*0.,-3.,6.,-3.,0.,2.,-4.,2.,9*0.,3.,-6.,3.,0.,
     *  -2.,4.,-2.,10*0.,-3.,3.,2*0.,2.,-2.,2*0.,-1.,1.,
     *  6*0.,3.,-3.,2*0.,-2.,2.,5*0.,1.,-2.,1.,0.,-2.,4.,
     *  -2.,0.,1.,-2.,1.,9*0.,-1.,2.,-1.,0.,1.,-2.,1.,10*0.,
     *  1.,-1.,2*0.,-1.,1.,6*0.,-1.,1.,2*0.,2.,-2.,2*0.,-1.,1./

      ind=1
      dn=0.0

      if(mode.eq.0)then ! bilinear
        i=x
        j=y
        if(i.lt.1)return
        if(j.lt.1)return
        if(i.ge.ns)return
        if(j.ge.nl)return
        ind=0
        lw=x-i
        rw=1.0-lw
        tw=y-j
        bw=1.0-tw
        top=lw*inbuf(i+1,j)+rw*inbuf(i,j)
        bot=lw*inbuf(i+1,j+1)+rw*inbuf(i,j+1)
        dn=bw*top+tw*bot

      else if(mode.eq.1)then ! nearest neighbor
        i=nint(x)
        j=nint(y)
        if(i.lt.1)return
        if(j.lt.1)return
        if(i.gt.ns)return
        if(j.gt.nl)return
        ind=0
        dn=inbuf(i,j)

      else if(mode.eq.2)then ! second order
        i=nint(x)
        j=nint(y)
        if(i.lt.2)return
        if(j.lt.2)return
        if(i.ge.ns)return
        if(j.ge.nl)return
        ind=0
        p=x-i
        q=y-j
        dn=q*(q-1.0)*0.5*inbuf(i,j-1)+
     +     p*(p-1.0)*0.5*inbuf(i-1,j)+
     +    (1.0+p*q-p*p-q*q)*inbuf(i,j)+
     +    p*(p-2.0*q+1.0)*0.5*inbuf(i+1,j)+
     +    q*(q-2.0*p+1.0)*0.5*inbuf(i,j+1)+
     +    p*q*inbuf(i+1,j+1)

      else if(mode.eq.3)then ! Sampling Theorem
        ii=nint(x)
        jj=nint(y)
        if(ii-kerw.lt.1)return
        if(jj-kerw.lt.1)return
        if(ii+kerw.gt.ns)return
        if(jj+kerw.gt.nl)return
        pi=3.14159
        ind=0
        xx=x-ii
        do i=-kerw,kerw
          d=pi*(i-xx)
          if(d.ne.0.0)then
            kerx(i)=sin(d)/d
          else
            kerx(i)=1.0
          endif
        enddo
        yy=y-jj
        do j=-kerw,kerw
          d=pi*(j-yy)
          if(d.ne.0.0)then
            kery(j)=sin(d)/d
          else
            kery(j)=1.0
          endif
        enddo
        dn=0.0
        do j=-kerw,kerw
          do i=-kerw,kerw
            dn=dn+kery(j)*kerx(i)*inbuf(ii+i,jj+j)
          enddo
        enddo

      else if(mode.eq.4)then ! bicubic spline
        i=x
        j=y
        if(i.lt.2)return
        if(j.lt.2)return
        if(i.gt.ns-2)return
        if(j.gt.nl-2)return
        ind=0
        ya(1)=inbuf(i,j)
        ya(2)=inbuf(i+1,j)
        ya(3)=inbuf(i+1,j+1)
        ya(4)=inbuf(i,j+1)
        y1(1)=(inbuf(i+1,j)-inbuf(i-1,j))/2.0
        y1(2)=(inbuf(i+2,j)-inbuf(i,j))/2.0
        y1(3)=(inbuf(i+2,j+1)-inbuf(i,j+1))/2.0
        y1(4)=(inbuf(i+1,j+1)-inbuf(i-1,j+1))/2.0
        y2(1)=(inbuf(i,j+1)-inbuf(i,j-1))/2.0
        y2(2)=(inbuf(i+1,j+1)-inbuf(i+1,j-1))/2.0
        y2(3)=(inbuf(i+1,j+2)-inbuf(i+1,j))/2.0
        y2(4)=(inbuf(i,j+2)-inbuf(i,j))/2.0
        y12(1)=(inbuf(i+1,j+1)-inbuf(i+1,j-1)-
     +          inbuf(i-1,j+1)+inbuf(i-1,j-1))/4.0
        y12(2)=(inbuf(i+2,j+1)-inbuf(i+2,j-1)-
     +          inbuf(i,j+1)+inbuf(i,j-1))/4.0
        y12(3)=(inbuf(i+2,j+2)-inbuf(i+2,j)-
     +          inbuf(i,j+2)+inbuf(i,j))/4.0
        y12(4)=(inbuf(i+1,j+2)-inbuf(i+1,j)-
     +          inbuf(i-1,j+2)+inbuf(i-1,j))/4.0
        ii=i
        jj=j
        DO 11 I=1,4
          Xa(I)=Ya(I)
          Xa(I+4)=Y1(I)
          Xa(I+8)=Y2(I)
          Xa(I+12)=Y12(I)
11      CONTINUE
        DO 13 I=1,16
          XX=0.
          DO 12 K=1,16
            XX=XX+WT(I,K)*Xa(K)
12        CONTINUE
          CL(I)=XX
13      CONTINUE
        L=0
        DO 15 I=1,4
          DO 14 J=1,4
            L=L+1
            C(I,J)=CL(L)
14        CONTINUE
15      CONTINUE
        T=x-ii
        U=y-jj
        dn=0.
        DO 16 I=4,1,-1
          dn=T*dn+((C(I,4)*U+C(I,3))*U+C(I,2))*U+C(I,1)
16      CONTINUE

      else
        write(*,*)'INTERPOLATE: invalid mode'
        stop
      endif
      return
      end



