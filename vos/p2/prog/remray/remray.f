c
c program remray
c
      include 'VICMAIN_FOR'
      subroutine main44

      parameter(npix=10000)
      integer*4 ounit,def,count,count1,count2,status
      real*4 buf1(npix),buf2(npix),buf3(npix),obuf(npix)
      real*4 med1(npix),med2(npix),med3(npix)
      integer*4 hist(-32768:32767)

c set constants
      nreset=0       ! # pixels changed
      
c parameters
      call xvparm('GAIN1',gain1,count1,def,1)
      call xvparm('GAIN2',gain2,count2,def,1)
      call xvparm('TOL',tolerance,count,def,1)
            
c open input 1
      call xvunit(inunit,'INP',1,status,' ')
      call xvopen(inunit,status,'U_FORMAT','REAL',' ')
      call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
      if(ns.gt.npix)then
        write(*,*)'Max number of pixels/line=',npix
        call abend
      endif

c open input 2
      call xvunit(inunit2,'INP',2,status,' ')
      call xvopen(inunit2,status,'U_FORMAT','REAL',' ')
      call xvget(inunit2,status,'NL',nl2,'NS',ns2,' ')
      if((ns2.ne.ns).or.(nl2.ne.nl))then
        write(*,*)'Input images are of unequal size'
        call abend
      endif
      
c compute histogram of medians & GAIN1 if defaulted.
      if(count1.eq.0)then
        do i=-32768,32767
          hist(i)=0
        enddo
        do j=1,nl
          call xvread(inunit2,med1,status,'LINE',j,' ')
          do i=1,ns
            k=nint(med1(i))
            hist(k)=hist(k)+1
          enddo
        enddo
        k=0
        n=nl*ns*0.02
        do i=32767,-32768,-1
          k=k+hist(i)
          if(k.ge.n)then
            upper_dn=i
            goto 10
          endif
        enddo
10      continue
        write(*,*)'Upper DN range is about ',upper_dn
        if(upper_dn.eq.0.)upper_dn=1.
        gain1=tolerance/upper_dn
        write(*,*)'GAIN1 reset to ',gain1
      endif
      
c compute histogram of activity & GAIN2 if defaulted.
      if(count2.eq.0)then
        do i=0,32767
          hist(i)=0
        enddo
        do j=1,nl
      
c         read picture data
          call getlines(j,nl,ns,inunit,inunit2,
     +      buf1,buf2,buf3,med1,med2,med3)
       
c         pixel loop
          do i=2,ns+1
            dn_sum=med1(i-1)+med1(i)+med1(i+1)+
     +             med2(i-1)+        med2(i+1)+
     +             med3(i-1)+med3(i)+med3(i+1)
            deviation=abs(8.0*med2(i)-dn_sum)/8.0
            k=nint(deviation*1000.)
            if(k.gt.32767)k=32767
            if(k.lt.-32768)k=-32768
            hist(k)=hist(k)+1
          enddo
        enddo
        k=0
        n=nl*ns*0.02
        do i=32767,0,-1
          k=k+hist(i)
          if(k.ge.n)then
            upper_dn=i/1000.
            goto 11
          endif
        enddo
11      continue
        write(*,*)'Upper activity range is about ',upper_dn
        if(upper_dn.eq.0.)upper_dn=1.
        gain2=tolerance/upper_dn
        write(*,*)'GAIN2 reset to ',gain2
      endif

c open output
      call xvunit(ounit,'OUT',1,status,' ')
      call xvopen(ounit,status,'U_FORMAT','REAL','OP','WRITE',' ')
      
c Process data.
      do j=1,nl
      
c       read picture data
        call getlines(j,nl,ns,inunit,inunit2,
     +    buf1,buf2,buf3,med1,med2,med3)
       
c       pixel loop
        do i=2,ns+1

          dn_sum=med1(i-1)+med1(i)+med1(i+1)+
     +           med2(i-1)+        med2(i+1)+
     +           med3(i-1)+med3(i)+med3(i+1)
          deviation=abs(8.0*med2(i)-dn_sum)/8.0
          tol=gain1*med2(i)+gain2*deviation
          
c         determine if pixel is changed.
          if(buf2(i)-med2(i).gt.tol)then
            dn_mean=(dn_sum+med2(i))/9.0
            obuf(i)=dn_mean   ! reset to mean
            nreset=nreset+1
          else
            obuf(i)=buf2(i)   ! no change
          endif
          
        enddo 
        call xvwrit(ounit,obuf(2),status,' ')
        
      enddo
      
c statistics
      write(*,*)nreset,' pixels reset'            
      return
      end
      
c********************************************************************      
      subroutine getlines(j,nl,ns,inunit,inunit2,
     +    buf1,buf2,buf3,med1,med2,med3)
      real*4 buf1(*),buf2(*),buf3(*),med1(*),med2(*),med3(*)
      integer*4 status
c j=picture line
c buf1,2,3 are 3 consecutive lines top down centered on line j
c med1,2,3 are the same but medians.
      
        if(j.eq.1)then
c         picture
          call xvread(inunit,buf2(2),status,'LINE',1,' ')
          buf2(1)=buf2(2)
          buf2(ns+2)=buf2(ns+1)
          call xvread(inunit,buf3(2),status,'LINE',2,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)
          call mve(7,ns+2,buf2,buf1,1,1)
c         median
          call xvread(inunit2,med2(2),status,'LINE',1,' ')
          med2(1)=med2(2)
          med2(ns+2)=med2(ns+1)
          call xvread(inunit2,med3(2),status,'LINE',2,' ')
          med3(1)=med3(2)
          med3(ns+2)=med3(ns+1)
          call mve(7,ns+2,med2,med1,1,1)

        else if(j.eq.nl)then
c         picture
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
c         median
          call mve(7,ns+2,med2,med1,1,1)
          call mve(7,ns+2,med3,med2,1,1)

        else
c         picture
          call mve(7,ns+2,buf2,buf1,1,1)
          call mve(7,ns+2,buf3,buf2,1,1)
          call xvread(inunit,buf3(2),status,'LINE',j+1,' ')
          buf3(1)=buf3(2)
          buf3(ns+2)=buf3(ns+1)
c         median
          call mve(7,ns+2,med2,med1,1,1)
          call mve(7,ns+2,med3,med2,1,1)
          call xvread(inunit2,med3(2),status,'LINE',j+1,' ')
          med3(1)=med3(2)
          med3(ns+2)=med3(ns+1)
            
        endif
        return
        end
      
