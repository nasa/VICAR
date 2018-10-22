c program swap

      include 'VICMAIN_FOR'
      subroutine main44

      parameter (maxsize=20000)
      complex*8 bufin1(maxsize),bufin2(maxsize),bufout(maxsize)
      integer*4 inunit1,inunit2,outunit,status

c open inputs
      call xvunit(inunit1,'INP',1,status,' ')
      call xvsignal(inunit1,status,1)
      call xvopen(inunit1,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit1,status,1)
      call xvget(inunit1,status,'NL',nl,'NS',ns,' ')
      call xvsignal(inunit1,status,1)
      if(ns.gt.maxsize)then
        call xvmessage('Picture records too long',' ')
        call abend
      endif
      call xvunit(inunit2,'INP',2,status,' ')
      call xvsignal(inunit2,status,1)
      call xvopen(inunit2,status,'U_FORMAT','COMP',' ')
      call xvsignal(inunit2,status,1)
      call xvget(inunit2,status,'NL',nl2,'NS',ns2,' ')
      call xvsignal(inunit2,status,1)
      if((nl.ne.nl2).or.(ns.ne.ns2))then
        call xvmessage('Pictures of unequal size',' ')
        call abend
      endif

c open output
      call xvunit(outunit,'OUT',1,status,' ')
      call xvsignal(outunit,status,1)
      call xvopen(outunit,status,'U_FORMAT','COMP','OP','WRITE',' ')
      call xvsignal(outunit,status,1)
  
c process data
      do line=1,nl
        call xvread(inunit1,bufin1,status,' ')
        call xvsignal(inunit1,status,1)
        call xvread(inunit2,bufin2,status,' ')
        call xvsignal(inunit2,status,1)
        do i=1,ns
          bufout(i)=bufin1(i)+bufin2(i)
        enddo
        call xvwrit(outunit,bufout,status,' ')
        call xvsignal(outunit,status,1)
      enddo

      return
      end
