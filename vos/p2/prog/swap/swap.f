c program swap

      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsize=20000)
      complex*8 bufin(maxsize),bufout(maxsize)
      integer*4 inunit,outunit,status

c open input
      call xvunit(inunit,'INP',1,status,' ')
      call xvsignal(inunit,status,1)
      call xvopen(inunit,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit,status,1)
      call xvget(inunit,status,'NL',nl,'NS',ns,' ')
      call xvsignal(inunit,status,1)
      if(ns.gt.maxsize)then
        call xvmessage('Picture records too long',' ')
        call abend
      endif
      if((ns/2)*2.ne.ns)then
        call xvmessage('NS must be even',' ')
        call abend
      endif
      if((nl/2)*2.ne.nl)then
        call xvmessage('NL must be even',' ')
        call abend
      endif

c open output
      call xvunit(outunit,'OUT',1,status,' ')
      call xvsignal(outunit,status,1)
      call xvopen(outunit,status,'U_FORMAT','COMP','OP','WRITE',' ')
      call xvsignal(outunit,status,1)
  
c process data
c     copy bottom half to top half
      do line=(nl/2)+2,nl
        call xvread(inunit,bufin,status,'LINE',line,' ')
        call xvsignal(inunit,status,1)
        k=0
        do i=(ns/2)+2,ns
          k=k+1
          bufout(k)=bufin(i)
        enddo
        do i=1,(ns/2)+1
          k=k+1
          bufout(k)=bufin(i)
        enddo
        call xvwrit(outunit,bufout,status,' ')
        call xvsignal(outunit,status,1)
      enddo
c     copy top half to bottom half
      do line=1,(nl/2)+1
        call xvread(inunit,bufin,status,'LINE',line,' ')
        call xvsignal(inunit,status,1)
        k=0
        do i=(ns/2)+2,ns
          k=k+1
          bufout(k)=bufin(i)
        enddo
        do i=1,(ns/2)+1
          k=k+1
          bufout(k)=bufin(i)
        enddo
        call xvwrit(outunit,bufout,status,' ')
        call xvsignal(outunit,status,1)
      enddo

      return
      end
