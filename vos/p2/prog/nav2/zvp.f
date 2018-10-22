      include 'VICMAIN_FOR'
      subroutine main44
      common/zvp/nz,zvp(2,1000)
      character*80 msg
  101 format(f6.2,',',f6.2,',')

      rtd = 180.d0/3.141592653589793d0
      call xvunit(iunit,'Z',1,ind,'U_NAME','JUPITER.ZVP')
      call xvsignal(iunit,ind,.true.)
      call xvopen(iunit,ind,'OPEN_ACT','SA','IO_ACT','SA',' ')
      call xvget(iunit,ind,'NL',nl,'NS',ns,' ')
      nz = (ns-2)/2
      call prnt(4,1,nz,'nz=.')
      call xvread(iunit,zvp,ind,' ')
      call xvclose(iunit,ind,' ')
      do i=1,nz
         write(msg,101) rtd*zvp(1,i),zvp(2,i)
         call xvmessage(msg,' ')
      enddo
      return
      end
