      INCLUDE 'VICMAIN_FOR'
 
      SUBROUTINE MAIN44
C TEST PROGRAM FOR THE SUBROUTINE MINMAX
      BYTE  BUF(25)
      DATA BUF/1,2,3,4,5,6,7,0,1,2,3,-1,34,55,6,78,47,24,
     &      11,22,33,44,55,66,77/     ! -1 IS DN OF 255.
      INTEGER*2  I2BUF(2)
      DATA I2BUF/1,2/
      INTEGER*4  I4BUF(30)
      DATA I4BUF/1,12,123,1234,12345,123456,7,78,789,
     &      10,-11,12,13,14,154,16789,-170,123,1905,20345,21345,
     &      229876,23098,24567,25,.26,270001,2.8,29.678,.30/
      REAL*4     R4BUF(10)
      DATA R4BUF/-1278.0,123.45,345.12,4987.23,5678.12,
     &      -612.11,-.7,8,.91,0/
      REAL*8     R8BUF(5)
      DATA R8BUF/1.23456789,0.0,-12345678,987654,12/
      COMPLEX    CBUF(5) 
      DATA CBUF/ ( -1., -1.), ( 0., 0.), ( 1., 0.), 
     &                     ( 0., 1.),   ( 1., 1.)   /

      INTEGER*4 DCODE
      INTEGER*4  N,MIN,MAX,IMIN,IMAX
      REAL*4	  RMIN,RMAX
      REAL*8	  RMIN8,RMAX8

C TEST FOR BYTE ARRAY
      DCODE = 1
      N = 25

      CALL MINMAX(DCODE,N,BUF,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*2 ARRAY
      DCODE = 2
      N = 2

      CALL MINMAX(DCODE,N,I2BUF,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*4 ARRAY
      DCODE = 4
      N = 25

      CALL MINMAX(DCODE,N,I4BUF,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')
C TEST FOR REAL*4
      DCODE = 7
      N = 10

      CALL MINMAX(DCODE,N,R4BUF,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('********************.',' ')

C TEST FOR REAL*8 ARRAY

      DCODE = 8
      N = 5

      CALL MINMAX(DCODE,N,R8BUF,RMIN8,RMAX8,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(8,1,RMIN8,'RMIN =.')
      CALL PRNT(8,1,RMAX8,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR COMPLEX ARRAY

      DCODE = 10
      N = 5

      CALL MINMAX(DCODE,N,CBUF,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')


      call XVMESSAGE( 'NOW TEST MINMAXE',' ')

C TEST FOR BYTE ARRAY
      DCODE = 1
      N = 25

      CALL MINMAXE(DCODE,N,BUF,0,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*2 ARRAY
      DCODE = 2
      N = 2

      CALL MINMAXE(DCODE,N,I2BUF,0,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*4 ARRAY
      DCODE = 4
      N = 25

      CALL MINMAXE(DCODE,N,I4BUF,0,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')
C TEST FOR REAL*4
      DCODE = 7
      N = 10

      CALL MINMAXE(DCODE,N,R4BUF,0.,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('********************.',' ')

C TEST FOR REAL*8 ARRAY

      DCODE = 8
      N = 5

      CALL MINMAXE(DCODE,N,R8BUF,0.,RMIN8,RMAX8,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(8,1,RMIN8,'RMIN =.')
      CALL PRNT(8,1,RMAX8,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR COMPLEX ARRAY

      DCODE = 10
      N = 5

      CALL MINMAXE(DCODE,N,CBUF,0.,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

      CALL XVMESSAGE(
     . 'Repeat some cases in C to test C interface: zminmax & zminmaxe',
     .   ' ')

      call tzminmax

      RETURN
      END

