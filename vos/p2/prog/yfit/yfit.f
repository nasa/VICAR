c
c program yfit
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsamp=20000)
      integer*4 ounit,def,count,hist(0:32767)
      integer*4 inunit,status
      real*4 buf(maxsamp),obuf(maxsamp)

c parameters
      call xvparm('PERCENT',percent,count,def,1)
      call xvparm('MAXY',ymax,count,def,1)

c checks
      call xveaction('SA',' ')

c open inputs & outputs
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','REAL',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,' ')
      if(ns.gt.maxsamp)then
        call xvmessage('Line length too long',' ')
        call abend
      endif
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'O_FORMAT','REAL','U_FORMAT','REAL',
     +              'OP','WRITE',' ')

c compute max dn
      bigy=0.0
      do line=1,nl
        call xvread(inunit,buf,status,'LINE',line,' ')
        do i=1,ns
          if((buf(i).ge.0.).and.(buf(i).ne.32767.))then
            if(bigy.lt.buf(i))bigy=buf(i)
          endif
        enddo
      enddo
      scale=32767./bigy
      write(msg,*)'max Y value is',bigy
      call xvmessage(msg,' ')

c compute histogram
      m=0
      do i=0,32767
        hist(i)=0
      enddo
      do line=1,nl
        call xvread(inunit,buf,status,'LINE',line,' ')
        do i=1,ns
          if((buf(i).ge.0.).and.(buf(i).ne.32767.))then
            j=nint(buf(i)*scale)
            hist(j)=hist(j)+1
            m=m+1
          endif
        enddo
      enddo
      count=percent*m/100.

c compute percent location
      m=0
      do i=32766,0,-1
        m=m+hist(i)
        if(m.ge.count)then
          location=i
          goto 10
        endif
      enddo
10    continue
      truelocation=location/scale

c compute scaling factor
      scale=ymax/truelocation
      write(msg,*)truelocation,' becomes ',ymax
      call xvmessage(msg,' ')

c process image
      m=0
      do line=1,nl
        call xvread(inunit,buf,status,'LINE',line,' ')
        do i=1,ns
          if(buf(i).eq.32767.)then
            obuf(i)=100.
          else
            obuf(i)=buf(i)*scale
            if(obuf(i).gt.100.)then
              obuf(i)=100.
              m=m+1
            endif
          endif
        enddo
        call xvwrit(ounit,obuf,status,' ')
      enddo

      write(msg,*)m,' pixels saturated to 100'
      call xvmessage(msg,' ')

      end
