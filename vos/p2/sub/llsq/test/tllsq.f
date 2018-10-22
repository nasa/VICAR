c*****************************************************************************
c Unit test program TLLSQ.F for subroutine LLSQ
c Ported to UNIX 11/10/1992
c ier=0 indicates success during individual runs...
c*****************************************************************************
      include 'VICMAIN_FOR'
      subroutine main44
      real*4       x(2,2), qr(5,3)
      integer*4    m, n, ip, ifail, ipiv(2)
      real*4       a(3,2) /1.1, 1.2, 1.0, 0.9, 1.0, 1.0/
      real*4       b(3,2) /2.2, 2.3, 2.1, 4.4, 4.6, 4.2/
      real*4       eps/1.e-7/
      character*80 msg

c  1.1  0.9  2.2 
c  1.2  1.0  2.3
c  1.0  1.0  2.1
c*****************************************************************************
c  Fortran test
c*****************************************************************************
      call xvmessage('Testing FORTRAN-callable LLSQ',' ')
      m = 3
      n = 2
      ip = 2
      ifail = 1

      call llsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      if (ifail.eq.0) go to 20
      write (msg,99996) ifail
      call xvmessage(msg,' ')
      return
20    continue
      call xvmessage('output should be 1.301 0.7935 2.602 1.587',' ')
      call prnt(7,4,x,'first run = .')

c  check error handling

      call xvmessage('test error handling. should get ier=1', ' ')
      call zia(a,3)

      call llsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      write (msg,99996) ifail
      call xvmessage(msg,' ')

c*****************************************************************************
c C test
c*****************************************************************************
      call xvmessage('Testing C-callable LLSQ',' ')
      m = 3
      n = 2
      ip = 2
      ifail = 1
      a(1,1) = 1.1
      a(2,1) = 1.2
      a(3,1) = 1.0
      a(1,2) = 0.9
      a(2,2) = 1.0
      a(3,2) = 1.0
      b(1,1) = 2.2
      b(2,1) = 2.3
      b(3,1) = 2.1
      b(1,2) = 4.4
      b(2,2) = 4.6
      b(3,2) = 4.2

      call tzllsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      if (ifail.eq.0) go to 40
      write (msg,99996) ifail
      call xvmessage(msg,' ')
      return
40    continue
      call xvmessage('output should be 1.301 0.7935 2.602 1.587',' ')
      call prnt(7,4,x,'C-callable output = .')

c  check error handling

      call xvmessage('error handling test. should get ier=1', ' ')
      call zia(a,3)

      call tzllsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      write (msg,99996) ifail
      call xvmessage(msg,' ')

      return
99996 format ('error in llsq, ifail =  ' , i2)
      end
