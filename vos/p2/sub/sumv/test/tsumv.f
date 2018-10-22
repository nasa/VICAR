      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      implicit integer (a - z)
C*******************************************
C     Data for dcode=1
      byte b
      byte a(5)
      data a/2,3,100,-2,-3/
C******************************************
C     Data for dcode=2
      integer*2 d
      integer*2 c(10)
      data c/-999,1000,999,-1000,31000,255,-31000,-100,100,255/
C******************************************
C     Data for dcode=4
      integer f
      integer e(10)
      data e/11111111,1,-11111111,-1,22222222,-22222222,1,
     .       -999,999,22222222/
C******************************************
C     Data for dcode=7
      real h,g(16)
      data g /1.1,2.22,3.333,4.4444,5.55555,6.666666,
     .        7.77777,8.888,8.8,7.77,6.666,5.5555,
     . 	      4.444,3.33,2.2,1.0/
C******************************************
C     Data for dcode=8
      double precision j,i(7)
      data i /111111.d8,222222.d8,333333.d8,444444.d5,
     .        222222.d8,-444444.d5,111111.d8/
C******************************************
C     Data for dcode=3
      integer*2 l
      byte k(11)
      data k /-127,126,127,112,-1,-112,1,-126,
     .        -1,-1,100/
C******************************************
C     Data for dcode=-3
      byte n
      integer*2 m(6)
      data m /250,500,-210,-310,210,310/
C******************************************
C     Data for dcode=6
      integer*2 o(11)
      data o/-999,1000,999,-1000,31000,255,31000,-100,100,-255,31000/
C*******************************************
C     Data for dcode=-6
      integer*4 p(7)
      data p/66666,-66666,-999,1000,999,-1000,-31000/
C*******************************************
C     Test for byte to byte
      call sumv(1,5,a,b, 1)
      call XVMESSAGE('B should be equal to 100 ',' ')
      call prnt (1,1,b,'b=.')
      call sumv(1,3,a,b,2)
      call XVMESSAGE('B should be equal to 99 ',' ')
      call prnt (1,1,b,'b=.')
      call sumv(1,2,a,b,3)
      call XVMESSAGE('B should be equal to 0 ',' ')
      call prnt (1,1,b,'b=.')
C*********************************************
C     Test for half to half
      call sumv(2,10,c,d, 1)
      call XVMESSAGE('D should be equal to 510 ',' ')
      call prnt (2,1,d,'d=.')
      call sumv(2,5,c,d,2)
      call XVMESSAGE('D should be equal to 100 ',' ')
      call prnt (2,1,d,'d=.')
C*********************************************
C     Test for full to full
      call sumv(4,10,e,f, 1)
      call XVMESSAGE('F should be equal to 22222223 ',' ')
      call prnt (4,1,f,'f=.')
C*********************************************
C     Test for real to real
      call sumv(7,16,g,h, 1)
      call XVMESSAGE('H should be approximately equal to 79.750886 ',
     .                ' ')
      call prnt (7,1,h,'h=.')
      call sumv(7,3,g,h,7)
      call XVMESSAGE('H should be approximately equal to 12.188 ',' ')
      call prnt (7,1,h,'h=.')
C*********************************************
C     Test for double to double
      call sumv(8,7,i,j, 1)
      call XVMESSAGE('J should be approximately equal to 999999.d8 ',
     .               ' ')
      call prnt (8,1,j,'j=.')
      call sumv(8,2,i,j,5)
      call XVMESSAGE('J should be approximately equal to 110666.d8 ',
     .                ' ')
      call prnt (8,1,j,'j=.')
C*********************************************
C     Test for byte fo halfword
      call sumv(3,11,k,l, 1)
      call XVMESSAGE('L should be equal to 1634 ',' ')
      call prnt (2,1,l,'l=.')
      call sumv(3,6,k,l,2)
      call XVMESSAGE('L should be equal to 867 ',' ')
      call prnt (2,1,l,'l=.')
C*********************************************
C     Test for halfword to byte
      call sumv(-3,6,m,n, 1)
      call XVMESSAGE('N should be equal to 238 ',' ')
      call prnt (1,1,n,'N=.')
      call sumv(-3,3,m,n,2)
      call XVMESSAGE('N should be equal to 250 ',' ')
      call prnt (1,1,n,'N=.')
C*********************************************
C     Test for byte to fullword
      call sumv(5,11,k,f, 1)
      call XVMESSAGE('F should be equal to 1634 ',' ')
      call prnt (4,1,f,'f=.')
C*********************************************
C     Test for fullword to byte
      call sumv(-5,7,e,b, 1)
      call XVMESSAGE('B should be equal to 1 ',' ')
      call prnt (1,1,b,'b=.')
C*********************************************
C     Test for halfword to fullword
      call sumv(6,11,o,f, 1)
      call XVMESSAGE('F should be equal to 93000 ',' ')
      call prnt (4,1,f,'f=.')
      call sumv(6,6,o,f,2)
      call XVMESSAGE('F should be equal to 93100 ',' ')
      call prnt (4,1,f,'f=.')
C*********************************************
C     Test for fullword to halfword
      call sumv(-6,7,p,d, 1)
      call XVMESSAGE('D should be equal to -31000 ',' ')
      call prnt (2,1,d,'d=.')
C*********************************************
C     Test for real to double
      call sumv(9,16,g,j, 1)
      call XVMESSAGE('J should be approximately equal to 79.750886 ',
     .                ' ')
      call prnt (8,1,j,'j=.')
C*********************************************
C     Test for double to real
      call sumv(-9,7,i,h, 1)
      call XVMESSAGE('H should be approximately equal to 999999.d8 ',
     .               ' ')
      call prnt (7,1,h,'h=.')
C******************************************************************
      CALL TZSUMV      ! TEST C INTERFACE

      RETURN
      END
