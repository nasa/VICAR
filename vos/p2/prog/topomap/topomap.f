c
c program topomap
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxnl=1000, maxns=1000)
      character*132 msg
      integer*4 def,count,status,unit1,unit2,unit3,ounit
      real*4 buf(maxns,maxnl),bufnew(maxns,maxnl)
      real*4 mark(4,1000),linedisp(4000)
      real*4 sampdisp(4000),dn(1000),r(1000)
      real*8 length(-3:362),lpf_length(-3:362)
      integer*4 hist(0:359),pass

      call xvpcnt('INP',nids)
      call xvparm('RADIUS',nw,count,def,1)
      call xvparm('MINPTS',minpts,count,def,1)
      call xvparm('MAXPASS',maxpass,count,def,1)

      call xvunit(unit1,'INP',1,status,' ')
      call xvopen(unit1,status,' ')
      call xvget(unit1,status,'NL',nl1,'NS',ns1,' ')
      call xvunit(unit2,'INP',2,status,' ')
      call xvopen(unit2,status,'U_FORMAT','REAL',' ')
      call xvget(unit2,status,'NL',nl2,'NS',ns2,' ')
      if(nids.eq.3)then
        call xvunit(unit3,'INP',3,status,' ')
        call xvopen(unit3,status,'U_FORMAT','REAL',' ')
      endif

      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +    'OP','WRITE',' ')

c     Checks
      if(nl1.gt.maxnl)then
        call xvmessage('Too many input lines',' ')
        call abend()
      endif
      if(ns1.gt.maxns)then
        call xvmessage('Too many input samples',' ')
        call abend()
      endif

c     Clear image buffer.
      do j=1,nl1
        do i=1,ns1
          buf(i,j)=0.0
          bufnew(i,j)=0.0
        enddo
      enddo

c     Determine histogram of vector direction.
      do i=0,359
        hist(i)=0
        length(i)=0.d0
      enddo
      if(nids.eq.2)then   ! MARK input
        do j=1,nl2
          call xvread(unit2,mark,status,'LINE',j,' ')
          do i=1,ns2/4
            difs=mark(4,i)-mark(2,i)
            difl=mark(3,i)-mark(1,i)
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              angle=atan2(-difl,difs)*57.29578
              if(angle.lt.0.0)angle=360.+angle
              k=angle
              if(k.gt.359)k=359
              if(k.lt.0)k=0
              hist(k)=hist(k)+1
              length(k)=length(k)+sqrt(difs*difs+difl*difl)
            endif
          enddo
        enddo
      else                 ! DISPARITY inputs
        do j=1,nl2
          call xvread(unit2,linedisp,status,'LINE',j,' ')
          call xvread(unit3,sampdisp,status,'LINE',j,' ')
          do i=1,ns2
            difs=sampdisp(i)-i
            difl=linedisp(i)-j
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              angle=atan2(-difl,difs)*57.29578
              if(angle.lt.0.0)angle=360.+angle
              k=angle
              if(k.gt.359)k=359
              if(k.lt.0)k=0
              hist(k)=hist(k)+1
              length(k)=length(k)+sqrt(difs*difs+difl*difl)
            endif
          enddo
        enddo
      endif

c     Determine major vector direction
      length(-1)=length(359)
      length(-2)=length(358)
      length(-3)=length(357)
      length(360)=length(0)
      length(361)=length(1)
      length(362)=length(2)
      call uniflt(8,366,length(-3),lpf_length(-3),7)
      max_length=0
      k=-1
      do i=0,359
c       write(*,*)i,hist(i),length(i),lpf_length(i)
        if(lpf_length(i).gt.max_length)then
          max_length=lpf_length(i)
          k=i
        endif
      enddo
      if(k.eq.-1)then
        call xvmessage('All disparities are zero',' ')
        call abend()
      endif
      write(*,*)'Mean disparity is at ',k,' degrees'
      vector_angle=k
      if(vector_angle.gt.180.)vector_angle=vector_angle-360.
      vector_angle=vector_angle/57.29578
      vector_length=lpf_length(k)/hist(k)
      write(*,*)'Mean component vector length is ',
     +           vector_length,' pixels'

c     Compute the topomap at specific input values.
      if(nids.eq.2)then   ! MARK input
        do j=1,nl2
          call xvread(unit2,mark,status,'LINE',j,' ')
          do i=1,ns2/4
            difs=mark(4,i)-mark(2,i)
            difl=mark(3,i)-mark(1,i)
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              dangle=atan2(-difl,difs)-vector_angle
              vlength=sqrt(difs*difs+difl*difl)
              ii=nint(mark(2,i))
              jj=nint(mark(1,i))
              buf(ii,jj)=vlength*cos(dangle)
            endif
          enddo
        enddo
      else                 ! DISPARITY inputs
        do j=1,nl2
          call xvread(unit2,linedisp,status,'LINE',j,' ')
          call xvread(unit3,sampdisp,status,'LINE',j,' ')
          do i=1,ns2
            difs=sampdisp(i)-i
            difl=linedisp(i)-j
            if((difs.ne.0.0).or.(difl.ne.0.0))then
              dangle=atan2(-difl,difs)-vector_angle
              vlength=sqrt(difs*difs+difl*difl)
              buf(i,j)=vlength*cos(dangle)
            endif
          enddo
        enddo
      endif

c     Fillin the missing pixels.
      rnw=nw*nw+.1 ! radius squared of collection circle + a bit
      pass=0
100   count=0
      pass=pass+1
      do j=1,nl1
        do i=1,ns1
          if(buf(i,j).ne.0.0)goto 10
          k=0
          do jj=max(j-nw,1),min(j+nw,nl1)
            j2=(j-jj)**2
            do ii=max(i-nw,1),min(i+nw,ns1)
              if(buf(ii,jj).ne.0.0)then
c               rad=(i-ii)**2+(j-jj)**2
                rad=(i-ii)**2+j2
                if(rad.le.rnw)then
                  k=k+1
                  dn(k)=buf(ii,jj)
                  r(k)=rad
                endif
              endif
            enddo
          enddo
          if(k.lt.minpts)goto 10
          sum1=0.0
          sum2=0.0
          do n=1,k
            sum1=sum1+dn(n)/r(n)
            sum2=sum2+1.0/r(n)
          enddo
          bufnew(i,j)=sum1/sum2
          count=count+1
10        continue
        enddo
      enddo
      do j=1,nl1
        do i=1,ns1
          if(bufnew(i,j).ne.0.0)then
            buf(i,j)=bufnew(i,j)
            bufnew(i,j)=0.0
          endif
        enddo
      enddo
      write(*,*)count,' pixels interpolated'
      if((count.gt.0).and.(pass.lt.maxpass))then
        goto 100
      endif


c     Write output
      do j=1,nl1
        call xvwrit(ounit,buf(1,j),status,' ')
      enddo

      return
      end
