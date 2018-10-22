c program correlate1d
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (nlmax=2000,nsmax=2000)
      real*4 lbuf(nsmax,nlmax),rbuf(nsmax,nlmax)
      real*4 ldis(nsmax,nlmax),sdis(nsmax,nlmax)
      real*4 quality2(nsmax)
      real*4 sorted(100),rbestsamp(-100:nlmax+100)
      real*8 sumx,sumy,sumx2,sumy2,sumxy,rn
      real*4 sumxl,sumyl,sumx2l,sumy2l,sumxyl
      real*4 sumxr,sumyr,sumx2r,sumy2r,sumxyr
      integer*4 bestl,bests,bestsamp(nlmax),fw2
      integer*4 hist(-50:50)
      logical set_offset
      
      integer*4 count,def,status,geom(2),unit(2),ounit(2)


c parameters
      call xvparm('NLW',nlw,count,def,1)
      if(nlw.eq.(nlw/2)*2)nlw=nlw+1
      call xvparm('NSW',nsw,count,def,1)
      if(nsw.eq.(nsw/2)*2)nsw=nsw+1
      call xvparm('MOTION',motion,count,def,1)
      call xvparm('GEOM',geom,count,def,2)
      linedif=geom(1)
      fw2=geom(2)
      call xvparm('OFFSET',jbias,count,def,1)
      set_offset=.false.
      if(count.gt.0)set_offset=.true.
      call xvparm('THRESH',thresh,count,def,1)
      call xvparm('QUALITY',tquality,count,def,1)
      tquality=tquality**2

      call xveaction('SA',' ')

c open 2 inputs
        call xvunit(unit(1),'INP',1,status,' ')
        call xvopen(unit(1),status,'U_FORMAT','REAL',' ')
        call xvget(unit(1),status,'NL',nlleft,'NS',nsleft,' ')
        call xvunit(unit(2),'INP',2,status,' ')
        call xvopen(unit(2),status,'U_FORMAT','REAL',' ')
        call xvget(unit(2),status,'NL',nlright,'NS',nsright,' ')
        if(nsleft.gt.nsmax)then
          call xvmessage('Line length too long',' ')
          call abend
        endif
        if(nlleft.gt.nlmax)then
          call xvmessage('Too many lines',' ')
          call abend
        endif

c read inputs into memory
      do line=1,nlleft                         ! line loop
        call xvread(unit(1),lbuf(1,line),status,'LINE',line,' ')
      enddo
      do line=1,nlright                         ! line loop
        call xvread(unit(2),rbuf(1,line),status,'LINE',line,' ')
      enddo

c open 2 outputs
      do i=1,2
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','REAL',
     +   'U_FORMAT','REAL','OP','WRITE',' ')
      enddo


c*************** Establish geometry relating the two input images **********
      if(set_offset)goto 80

      write(*,*)'Begin determination of vertical shift'

      do i=-linedif,linedif
        hist(i)=0
      enddo

c correlate every linc lines
      linc=nlleft/25
      icen=nsleft/2
      rn=fw2*2+1.
      do kk=linedif+6,nlleft-linedif-6,linc                             ! line centers
        bestquality=-1.
        do line=kk-linedif,kk+linedif                 ! local line
          do il=fw2+4,nsleft-fw2-4                    ! sample centers
            sumx=0.d0
            sumy=0.d0
            sumx2=0.d0
            sumy2=0.d0
            sumxy=0.d0
            k=icen-il
            do i=il-fw2,il+fw2                        ! convolution
                x=lbuf(i+k,kk)                        ! template
                y=rbuf(i,line)                        ! search area
                sumx=sumx+x
                sumy=sumy+y
                sumx2=sumx2+x*x
                sumy2=sumy2+y*y
                sumxy=sumxy+x*y
            enddo
            quality=(sumxy-sumx*sumy/rn)**2/
     +          ((sumx2-sumx*sumx/rn)*(sumy2-sumy*sumy/rn))
            if(quality.gt.bestquality)then
              bestquality=quality
              bestl=line
              bests=il
            endif
          enddo
        enddo
        hist(bestl-kk)=hist(bestl-kk)+1
      enddo

      k=-1                    ! locate most frequent line shift
      do i=-linedif,linedif
        if(hist(i).gt.k)then
          k=hist(i)
          jbias=i
        endif
      enddo
      write(*,*)'Line offset parameter set to ',jbias

80    continue

c***************** Get the horizontal shift of each line *****************

      write(*,*)'Begin construction of horizontal shift model'

c perform 1-d correlation of line center with rest of line
      do line=1,nlleft                              ! line centers
        bestsamp(line)=0                            ! no data flag
        bestquality=-1.
        do il=fw2+4,nsleft-fw2-4                    ! sample centers
          sumx=0.d0
          sumy=0.d0
          sumx2=0.d0
          sumy2=0.d0
          sumxy=0.d0
          k=icen-il
          kk=line+jbias
          if(kk.lt.1)goto 70
          if(kk.gt.nlright)goto 70
          do i=il-fw2,il+fw2                        ! convolution
              x=lbuf(i+k,line)                      ! template
              y=rbuf(i,kk)                          ! search area
              sumx=sumx+x
              sumy=sumy+y
              sumx2=sumx2+x*x
              sumy2=sumy2+y*y
              sumxy=sumxy+x*y
          enddo
          quality=(sumxy-sumx*sumy/rn)**2/
     +        ((sumx2-sumx*sumx/rn)*(sumy2-sumy*sumy/rn))
          if(quality.gt.bestquality)then
            bestquality=quality
            bestsamp(line)=il
          endif
        enddo
        bestsamp(line)=bestsamp(line)-icen
70      continue
      enddo

c Try to patch errors by replacing shift by local median.
      n=15
      m=2*n+1
      do i=1,nlleft
        rbestsamp(i)=bestsamp(i)
      enddo
      do i=0,-n,-1
        rbestsamp(i)=rbestsamp(1-i)
      enddo
      do i=1,n
        rbestsamp(nlleft+i)=rbestsamp(nlleft-i)
      enddo
      do j=1,nlleft
        k=0
        do i=j-n,j+n
          k=k+1
          sorted(k)=rbestsamp(i)
        enddo
        call heapsort(m,sorted)
        bestsamp(j)=sorted(n+1)
      enddo

c Fill zeroes

c********************** begin correlation ************************

      write(*,*)'Begin correlations'

c set constants
      nlwc=nlw/2+1
      nswc=nsw/2+1
      nlw2=nlw/2
      nsw2=nsw/2

c clear disparity images
      do j=1,nlleft
        do i=1,nsleft
          ldis(i,j)=0.0
          sdis(i,j)=0.0
        enddo
      enddo

      ncorel=0
      avecor=0.
      rn=nswc*nlw
      line_top=nlwc
      line_bot=nlleft-nlw2
      if(jbias.lt.0)line_top=line_top+abs(jbias)
      if(jbias.gt.0)line_bot=line_bot-abs(jbias)

      do jl=line_top,line_bot                 ! center line
        ibias=bestsamp(jl)           ! offset sample bias

        do il=nswc+motion,nsleft-nsw2-motion  ! center sample

          sumxl=0.0               ! precompute template sums
          sumx2l=0.0
          sumxr=0.0
          sumx2r=0.0
          do j=jl-nlw2,jl+nlw2    ! each line of nlw lines
            do i=il-nsw2,il   
              x=lbuf(i,j)         ! left half of template
              sumxl=sumxl+x
              sumx2l=sumx2l+x*x
            enddo
            do i=il,il+nsw2
              x=lbuf(i,j)         ! right half of template
              sumxr=sumxr+x
              sumx2r=sumx2r+x*x
            enddo
          enddo

          do is=il-motion,il+motion      ! center sample in search
            k=is-il+ibias
            if(il-nsw2+k.lt.1)goto 100        ! outside right image
            if(il+nsw2+k.gt.nsright)goto 100  ! "
            sumyl=0.0
            sumy2l=0.0
            sumxyl=0.0
            sumyr=0.0
            sumy2r=0.0
            sumxyr=0.0

            do j=jl-nlw2,jl+nlw2               ! each line of nlw lines
              jr=j+jbias                       ! line in right image
              do i=il-nsw2,il             ! correlate 1 line
                x=lbuf(i,j)     ! left half of template
                y=rbuf(i+k,jr)  ! search area
                sumyl=sumyl+y
                sumy2l=sumy2l+y*y
                sumxyl=sumxyl+x*y
              enddo
              do i=il,il+nsw2             ! correlate 1 line
                x=lbuf(i,j)     ! right half of template
                y=rbuf(i+k,jr)  ! search area
                sumyr=sumyr+y
                sumy2r=sumy2r+y*y
                sumxyr=sumxyr+x*y
              enddo
            enddo
            qleft=(sumxyl-sumxl*sumyl/rn)**2/
     +          ((sumx2l-sumxl*sumxl/rn)*(sumy2l-sumyl*sumyl/rn))
            qright=(sumxyr-sumxr*sumyr/rn)**2/
     +          ((sumx2r-sumxr*sumxr/rn)*(sumy2r-sumyr*sumyr/rn))
            quality2(is)=max(qleft,qright)
          enddo

          bestquality=-1.0                    ! get best quality at kk
          do is=il-motion,il+motion
            if(quality2(is).gt.bestquality)then
              bestquality=quality2(is)
              kk=is
            endif
          enddo

          if(bestquality.lt.tquality)goto 100 ! abort if quality is poor
          if(kk.eq.il-motion) goto 100        ! abort if is at the end
          if(kk.eq.il+motion) goto 100

          if((quality2(kk-1).lt.quality2(kk)).and.
     +       (quality2(kk+1).lt.quality2(kk)))then  ! interpolate for s
c            fa=sqrt(quality2(kk-1))
c            fb=sqrt(quality2(kk))
c            fc=sqrt(quality2(kk+1))
            fa=quality2(kk-1)
            fb=quality2(kk)
            fc=quality2(kk+1)
            s=kk-0.5*(fa-fc)/(2.0*fb-fc-fa)
          else
            s=kk
          endif
            
          ncorel=ncorel+1
          ldis(il,jl)=jl+jbias       ! line disparity
          sdis(il,jl)=s+ibias        ! sample disparity
          avecor=avecor+fb

100       continue
        enddo

      enddo
      write(*,*)'Performed ',ncorel,' correlations'
      write(*,*)'Average correlation quality was ',sqrt(avecor/ncorel)

c********************** Check correlations in reverse *************


      write(*,*)'Begin inverse correlation check'
      nrejected=0
      do jline=1,nlleft
        do isamp=1,nsleft
          if(sdis(isamp,jline).eq.0.0)goto 120

          jl=nint(ldis(isamp,jline)) ! right line
          il=nint(sdis(isamp,jline)) ! right sample
          ibias=isamp-il
          jbias=jline-jl

          sumxl=0.0               ! precompute template sums
          sumx2l=0.0
          sumxr=0.0
          sumx2r=0.0
          do j=jl-nlw2,jl+nlw2    ! each line of nlw lines
            do i=il-nsw2,il   
              x=rbuf(i,j)         ! left half of template
              sumxl=sumxl+x
              sumx2l=sumx2l+x*x
            enddo
            do i=il,il+nsw2
              x=rbuf(i,j)         ! right half of template
              sumxr=sumxr+x
              sumx2r=sumx2r+x*x
            enddo
          enddo

          do is=il-motion,il+motion      ! center sample in search
            k=is-il+ibias
            if(il-nsw2+k.lt.1)goto 131        ! outside left image
            if(il+nsw2+k.gt.nsleft)goto 131  ! "
            sumyl=0.0
            sumy2l=0.0
            sumxyl=0.0
            sumyr=0.0
            sumy2r=0.0
            sumxyr=0.0

            do j=jl-nlw2,jl+nlw2               ! each line of nlw lines
              jr=j+jbias                       ! line in left image
              do i=il-nsw2,il             ! correlate 1 line
                x=rbuf(i,j)     ! left half of template
                y=lbuf(i+k,jr)  ! search area
                sumyl=sumyl+y
                sumy2l=sumy2l+y*y
                sumxyl=sumxyl+x*y
              enddo
              do i=il,il+nsw2             ! correlate 1 line
                x=rbuf(i,j)     ! right half of template
                y=lbuf(i+k,jr)  ! search area
                sumyr=sumyr+y
                sumy2r=sumy2r+y*y
                sumxyr=sumxyr+x*y
              enddo
            enddo
            qleft=(sumxyl-sumxl*sumyl/rn)**2/
     +          ((sumx2l-sumxl*sumxl/rn)*(sumy2l-sumyl*sumyl/rn))
            qright=(sumxyr-sumxr*sumyr/rn)**2/
     +          ((sumx2r-sumxr*sumxr/rn)*(sumy2r-sumyr*sumyr/rn))
            quality2(is)=max(qleft,qright)
          enddo

          bestquality=-1.0                 ! get best quality at kk
          do is=il-motion,il+motion
            if(quality2(is).gt.bestquality)then
              bestquality=quality2(is)
              kk=is
            endif
          enddo

          if(bestquality.lt.tquality)goto 130 ! abort if quality is poor
          if(kk.eq.il-motion) goto 130        ! abort if is at the end
          if(kk.eq.il+motion) goto 130

          if((quality2(kk-1).lt.quality2(kk)).and.
     +       (quality2(kk+1).lt.quality2(kk)))then  ! interpolate for s
c            fa=sqrt(quality2(kk-1))
c            fb=sqrt(quality2(kk))
c            fc=sqrt(quality2(kk+1))
            fa=quality2(kk-1)
            fb=quality2(kk)
            fc=quality2(kk+1)
            s=kk-0.5*(fa-fc)/(2.0*fb-fc-fa)
          else
            s=kk
          endif

          if(abs(s+ibias-isamp).gt.thresh) goto 130 ! check inverse

          goto 120
130       continue  ! failed the test
          nrejected=nrejected+1
131       continue
          ldis(isamp,jline)=0.0
          sdis(isamp,jline)=0.0

120       continue
        enddo
      enddo
      write(*,*)'Inverse rejected ',nrejected,' points'


c write disparity images
      do line=1,nlleft
        call xvwrit(ounit(1),ldis(1,line),status,' ')
        call xvwrit(ounit(2),sdis(1,line),status,' ')
      enddo

      end

      SUBROUTINE heapsort(N,RA)
c Heapsort algorithm (Numerical Recipes SORT).
c Real array RA of length N.
c RA is replaced upon return with a sorted version of itself.
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END



