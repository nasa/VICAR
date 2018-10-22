cccccccccccccccc
c Print the inputs...
      subroutine ptres1(hm1,h0,h1,vm1,v0,v1)
      implicit none
      integer*2 hm1,h0,h1
      integer*2 vm1,v0,v1
      character*80 msg
  101 format(3i5)

      call xvmessage('horizontal pixels...',0)
      write(msg,101) hm1,h0,h1
      call xvmessage(msg,0)
      
      call xvmessage('vertical pixels...',0)
      write(msg,101) vm1,v0,v1
      call xvmessage(msg,0)

      return
      end
      
