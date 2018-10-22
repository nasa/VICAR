c
c program remnoise
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter(npix=20000)
      integer*4 ounit,def,count_tol,status
      integer*4 count_factor
      real*4 buf1(npix),buf2(npix),buf3(npix),obuf(npix)
      real*4 sdev(npix)
      real*4 border(8)
      character*32 format
      logical roundoff

c set constants
      w=.7072        ! weight for a corner pixel.
      nreset=0       ! # pixels changed
      
c parameters
      call xvparm('TOL',tol_fixed,count_tol,def,1)
      call xvparm('FACTOR',factor,count_factor,def,1)
      call xvpcnt('INP',nids)
            
c open input 1
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','REAL',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      roundoff=.false.
      if(format.eq.'BYTE')roundoff=.true.
      if(format.eq.'HALF')roundoff=.true.
      if(format.eq.'FULL')roundoff=.true.
      if(roundoff)write(*,*)'Integer roundoff correction applied'
      if(ns.gt.npix)then
        write(*,*)'Max number of pixels/line=',npix
        call abend
      endif

c open input 2
      if(nids.gt.1)then
        call xvunit(inunit2,'INP',2,status,' ')
        call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
        bigsig=0.0
        do j=1,nl
          call xvread(inunit2,sdev,status,'LINE',j,' ')
          do i=1,ns
            if(sdev(i).gt.bigsig)bigsig=sdev(i)
          enddo
        enddo
        write(*,*)'largest standard deviation is',bigsig
        scale=ntab/bigsig
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL','OP','WRITE',' ')
      
c line loop
      do j=1,nl
      
c       read picture data
        if(j.eq.1)then
          call xvread(inunit,buf2(2),status,'LINE',1,' ')
          buf2(1)=buf2(2)
          buf2(ns+2)=buf2(ns+1)
          call xvread(inunit,buf3(2),status,'LINE',2,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)
          call mve(7,ns+2,buf2,buf1,1,1)
        else if(j.eq.nl)then
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
        else
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
          call xvread(inunit,buf3(2),status,'LINE',j+1,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)      
        endif
        
c       read standard deviation data
        if(nids.gt.1)call xvread(inunit2,sdev(2),status,'LINE',j,' ') 
                 
c       pixel loop
        do i=2,ns+1

c         interpolate medians around 3 by 3 box border.
c         border(8) border(1) border(5)
c         border(3)           border(4)
c         border(7) border(2) border(6)
          sum=0.0
          sumr=0.0
          call median(buf1(i-1),w,buf1(i),1.,buf1(i+1),w,sum,sumr
     +     ,border(1))
          call median(buf3(i-1),w,buf3(i),1.,buf3(i+1),w,sum,sumr
     +     ,border(2))
          call median(buf1(i-1),w,buf2(i-1),1.,buf3(i-1),w,sum,sumr
     +     ,border(3))
          call median(buf1(i+1),w,buf2(i+1),1.,buf3(i+1),w,sum,sumr
     +     ,border(4))
          call median(buf1(i),1.,buf1(i+1),w,buf2(i+1),1.,sum,sumr
     +     ,border(5))
          call median(buf2(i+1),1.,buf3(i+1),w,buf3(i),1.,sum,sumr
     +     ,border(6))
          call median(buf3(i),1.,buf3(i-1),w,buf2(i-1),1.,sum,sumr
     +     ,border(7))
          call median(buf2(i-1),1.,buf1(i-1),w,buf1(i),1.,sum,sumr
     +     ,border(8)) 
          surface=sum/sumr
          dif=abs(buf2(i)-surface)
                    
c         compute tol          
          if(nids.eq.1)then
            tol=factor*((abs(border(8)-border(6))+
     +                   abs(border(5)-border(7))+
     +                   abs(border(3)-border(4))+
     +                   abs(border(1)-border(2)))/4.0 )+
     +          tol_fixed
          else
            tol=factor*sdev(i) + tol_fixed
          endif
          
c         determine if pixel is changed.
          if(dif.gt.tol)then
            obuf(i)=surface
            nreset=nreset+1
          else
            obuf(i)=buf2(i)
          endif
          
c         roundoff for integer output
          if(roundoff)obuf(i)=float(nint(obuf(i)))
          
        enddo 
        call xvwrit(ounit,obuf(2),status,' ')
      enddo
      
c statistics
      write(*,*)nreset,' pixels reset'
            
      return
      end
      
c************************************************************************
      subroutine median(a,wa,b,wb,c,wc,sum,sumr,med)
c given 3 numbers a,b,c and weights wa,wb,wc 
c return sum=sum+median(a,b,c) and sumr=sumr+corresponding weight.
      real*4 med
      if((b.ge.a).and.(b.le.c))then
        med=b
        sum=sum+b*wb
        sumr=sumr+wb
      else if((b.le.a).and.(b.ge.c))then
        med=b
        sum=sum+b*wb
        sumr=sumr+wb
      else if((a.ge.b).and.(a.le.c))then
        med=a
        sum=sum+a*wa
        sumr=sumr+wa
      else if((a.le.b).and.(a.ge.c))then
        med=a
        sum=sum+a*wa
        sumr=sumr+wa
      else if((c.ge.a).and.(c.le.b))then
        med=c
        sum=sum+c*wc
        sumr=sumr+wc
      else if((c.le.a).and.(c.ge.b))then
        med=c
        sum=sum+c*wc
        sumr=sumr+wc
      else
        write(*,*)'MEDIAN: should not be here'
      endif
      return
      end
