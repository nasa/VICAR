c
c program mpftpt1
c
      include 'VICMAIN_FOR'
      subroutine main44

      integer maxns
      parameter (maxns=5000,maxnl=15)

      integer*4 ounit(3),def,count
      integer*4 unit(2),status,nl,ns,search,template,shift
      real*4 left(maxns,maxnl),right(maxns,maxnl)
      real*4 cl(maxns),cs(maxns)
      real*4 sum(maxns),qualcor(maxns)
c      real*4 qualcor2(maxns),cs2(maxns),cl2(maxns)
      real*8 sumxy,sumx,sumy,sumx2,sumy2,d1,d2

c parameters
      call xvparm('TEMPLATE',template,count,def,1)
      if((template/2)*2.eq.template)template=template+1
      call xvparm('SEARCH',search,count,def,1)
      if((search/2)*2.eq.search)search=search+1
      call xvparm('SHIFT',shift,count,def,1)
      call xvparm('QUALITY',quality,count,def,1)
      call xvparm('NLW',nlw,count,def,1)
      if((nlw/2)*2.eq.nlw)nlw=nlw+1
      nlw2=nlw/2

      call xveaction('SA',' ')
      if(search.le.template)then
        call xvmessage("search must be > template"," ")
        call abend
      endif

      template1=template
      search1=search
      template2=(2*template)/3
      search2=search
      if((template2/2)*2.eq.template2)template2=template2+1
      if((search2/2)*2.eq.search2)search2=search2+1

c open all inputs
      do i=1,2
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','REAL',' ')
        call xvget(unit(i),status,'NL',nl,'NS',ns,' ')
        if(ns.gt.maxns)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
      enddo

c open all outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE',' ')
      enddo

      do line=1,nl                         ! line loop

        loop=0
100     loop=loop+1

c process images

        i_left=(search-template)/2 + 1
        i_right=ns-(search-template)/2-template+1
        nconvolution=search-template+1
        noffset=(search-template)/2+template/2
        nof=template/2
      
c       Read blocks of nlw lines
        lineright=line+shift
        do k=1,nlw
          kline=line-nlw2+k-1
          if(kline.lt.1)kline=1
          if(kline.gt.nl)kline=nl
          call xvread(unit(1),left(1,k),status,'LINE',kline,' ')
        enddo
        do k=1,nlw
          kline=line+shift-nlw2+k-1
          if(kline.lt.1)kline=1
          if(kline.gt.nl)kline=nl
          call xvread(unit(2),right(1,k),status,'LINE',kline,' ')
        enddo
        
        m=0
        do i=i_left,i_right                     ! pixel loop
        
c         pre compute template statistics
          sumx=0.d0
          sumx2=0.d0
          do kk=1,nlw
            do k=1,template
              sumx=sumx+left(i+k-1,kk)
              sumx2=sumx2+left(i+k-1,kk)*left(i+k-1,kk)
            enddo
          enddo
          m=m+1
          
          do j=1,nconvolution
            sumxy=0.d0
            sumy=0.d0
            sumy2=0.d0
            do kk=1,nlw
              do k=1,template
                sumy=sumy+right(j+m+k-2,kk)
                sumxy=sumxy+left(i+k-1,kk)*right(j+m+k-2,kk)
                sumy2=sumy2+right(j+m+k-2,kk)*right(j+m+k-2,kk)
              enddo
            enddo
            d1=sumx2-sumx*sumx/(template*nlw)
            d2=sumy2-sumy*sumy/(template*nlw)
            if(d1*d2.ne.0.0)then
              sum(j)=(sumxy-sumx*sumy/(template*nlw))**2/(d1*d2)
            else
              sum(j)=0.0
            endif
          enddo
          
          k=1
          do j=2,nconvolution
            if(sum(j).gt.sum(k))k=j
          enddo
          if(sum(k).gt.quality)then
            cs(m+noffset)=m+k+nof-1
            cl(m+noffset)=lineright
            qualcor(m+noffset)=sum(k)
          else
            cs(m+noffset)=0.0
            cl(m+noffset)=0.0
            qualcor(m+noffset)=0.0
          endif
        enddo   ! pixel loop

        do i=1,noffset ! fill in ends of line
          cl(i)=0.0  ! empty
          cs(i)=0.0  ! empty
          cl(ns-i+1)=0.0  ! empty
          cs(ns-i+1)=0.0  ! empty
        enddo

c        if(loop.eq.1)then
c          do i=1,ns
c            cs2(i)=cs(i)
c            cl2(i)=cl(i)
c            qualcor2(i)=qualcor(i)
c          enddo
c          template=template2
c          search=search2
c          goto 100
c        else
c          do i=1,ns
c            if((qualcor(i).eq.0.).or.
c     +         (qualcor2(i).eq.0.).or.
c     +         (abs(cs(i)-cs2(i)).gt.2.1))then
c              cs(i)=0.
c              cl(i)=0.
c              qualcor(i)=0.
c            endif
c          enddo
c          template=template1
c          search=search1
c        endif

        call xvwrit(ounit(1),cl,status,' ')
        call xvwrit(ounit(2),cs,status,' ')
        call xvwrit(ounit(3),qualcor,status,' ')
      enddo     ! line loop

      end

