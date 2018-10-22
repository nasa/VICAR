C*****************************************************************************
C Unit test program TDGELG.F for subroutine DGELG
C Ported to UNIX 11/10/1992
C*****************************************************************************
      include 'VICMAIN_FOR'
      subroutine main44

      integer i, ifail, ir, n, eps
c     .. local arrays ..
      double precision a(3,3)/33,-24,-8,16,-10,-4,72,-57,-17/
      double precision b(3,2)/-359,281,85,-718,562,170/
      character*80 string

      n = 3
      ir = 2

c.. Test the fortran subroutine
      call xvmessage(' Testing DGELG.F',' ')
      call xvmessage(' ...input R matrix:',' ')
      do 10 i=1,n
        write (string,99995) b(i,1)
        call xvmessage(string,' ')
        write (string,99995) b(i,2)
        call xvmessage(string,' ')
 10   continue
      call xvmessage(' ...input A coefficients:',' ')
      do 20 i=1,n
        write (string,99995) a(i,1)
        call xvmessage(string,' ')
        write (string,99995) a(i,2)
        call xvmessage(string,' ')
        write (string,99995) a(i,3)
        call xvmessage(string,' ')
 20   continue

      call dgelg(b,a,n,ir,eps,ifail)

      if (ifail .ne. 0) go to 100
      call xvmessage(' ...solution matrix:',' ')
      do 30 i=1,n
        write (string,99995) b(i,1)
        call xvmessage(string,' ')
        write (string,99995) b(i,2)
        call xvmessage(string,' ')
 30   continue

c.. Test the C-bridge
      call xvmessage(' Testing the C-bridge',' ')
      b(1,1) = -359
      b(2,1) = 281
      b(3,1) = 85 
      b(1,2) = -718
      b(2,2) = 562
      b(3,2) = 170
      call xvmessage(' ...input R matrix:',' ')
      do 40 i=1,n
        write (string,99995) b(i,1)
        call xvmessage(string,' ')
        write (string,99995) b(i,2)
        call xvmessage(string,' ')
 40   continue
      a(1,1) = 33
      a(2,1) = -24
      a(3,1) = -8
      a(1,2) = 16
      a(2,2) = -10
      a(3,2) = -4
      a(1,3) = 72
      a(2,3) = -57
      a(3,3) = -17
      call xvmessage(' ...input A coefficients:',' ')
      do 50 i=1,n
        write (string,99995) a(i,1)
        call xvmessage(string,' ')
        write (string,99995) a(i,2)
        call xvmessage(string,' ')
        write (string,99995) a(i,3)
        call xvmessage(string,' ')
 50   continue

      call tzdgelg(b,a,n,ir,eps,ifail)

      if (ifail .ne. 0) go to 100
      call xvmessage(' ...solution matrix:',' ')
      do 60 i=1,n
       write (string,99995) b(i,1)
       call xvmessage(string,' ')
       write (string,99995) b(i,2)
       call xvmessage(string,' ')
60    continue

      return

100   write(string,99996) ifail
      call xvmessage(string,' ')
      return

99996 format (' error in dgelg, ifail = ', i2)
99995 format ('   ',1x , f10.5)
      end
