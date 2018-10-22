C
C	Test program for DFORM subroutine
C
C	Author: Justin McNeill
C	Date:	November 1993
C	
C
      include 'VICMAIN_FOR'

      subroutine main44

      character*80 buffer
      integer hisbuf(256)
      integer coll

      byte obuf(10)
      byte try(10)
      integer*2 try1(10)
      integer*4 try2(10)
      data try/25,26,27,28,29,30,31,32,33,34/
      data try1/1125,1126,1127,1128,1129,1130,1131,1132,1133,1134/
      data try2/1000,1050,1100,1150,1200,1250,1300,1350,1400,1450/
!
!     test 1 checks the collect flag and byte-to-byte transfer
!
      isw = 1
      coll = 0
      call xvmessage(' Test of byte, halfword and fullword',' ')
      call xvmessage(' conversions and scaling',' ')
      call xvmessage(' with FORTRAN interface ****',' ')
      call xvmessage(' ',' ')
      call xvmessage(' Byte data input ',' ')
      call xvmessage(' ',' ')
     
      write(buffer,100) try(1),try(2),try(3),try(4),try(5)
      call xvmessage(buffer,' ')
      write(buffer,100) try(6),try(7),try(8),try(9),try(10)
      call xvmessage(buffer,' ')

      call dform( try, obuf, 10, hisbuf, isw, 0, 2, 0, 7, 
     - 			0., 100., coll)

      isw = 0
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Entries scaled by a factor of 2.55',' ')
      call xvmessage(' ',' ')
     
      call dform( try, obuf, 10, hisbuf, isw, 0, 0, 0, 0, 
     - 			0., 0., coll )

      write(buffer,100) obuf(1),obuf(2),obuf(3),obuf(4),obuf(5)
      call xvmessage(buffer,' ')
      write(buffer,100) obuf(6),obuf(7),obuf(8),obuf(9),obuf(10)
      call xvmessage(buffer,' ')
      call xvmessage(' ',' ')
!
!     test 2 checks the halfword-to-byte transfer
!
      isw = 1
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Halfword data input ',' ')
      call xvmessage(' ',' ')
     
      write(buffer,100) try1(1),try1(2),try1(3),try1(4),try1(5)
      call xvmessage(buffer,' ')
      write(buffer,100) try1(6),try1(7),try1(8),try1(9),try1(10)
      call xvmessage(buffer,' ')

      call dform( try1, obuf, 10, hisbuf, isw, 1, 2, 0, 7, 
     - 			1125., 1135., coll)

      isw = 0
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Entries scaled by a factor of 25.5',' ')
      call xvmessage(' on a range between 1125 and 1135.',' ')
      call xvmessage(' ',' ')
     
      call dform( try1, obuf, 10, hisbuf, isw, 0, 0, 0, 0, 
     - 			0., 0., coll )

      write(buffer,100) obuf(1),obuf(2),obuf(3),obuf(4),obuf(5)
      call xvmessage(buffer,' ')
      write(buffer,100) obuf(6),obuf(7),obuf(8),obuf(9),obuf(10)
      call xvmessage(buffer,' ')
      call xvmessage(' ',' ')

      if(obuf(7).gt.127) then
	call xvmessage('BYTE on SIG is greater than +127',' ')
      endif
!
!     test 3 checks the fullword-to-byte transfer
!
      isw = 1
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Fullword data input ',' ')
      call xvmessage(' ',' ')
     
      write(buffer,100) try2(1),try2(2),try2(3),try2(4),try2(5)
      call xvmessage(buffer,' ')
      write(buffer,100) try2(6),try2(7),try2(8),try2(9),try2(10)
      call xvmessage(buffer,' ')

      call dform( try2, obuf, 10, hisbuf, isw, 2, 2, 0, 7, 
     - 			1000., 1450., coll)

      isw = 0
      coll = 0
      call xvmessage(' ',' ')
      call xvmessage(' Entries scaled by a factor of 0.56',' ')
      call xvmessage(' on a range between 1000 and 1450.',' ')
      call xvmessage(' ',' ')
     
      call dform( try2, obuf, 10, hisbuf, isw, 0, 0, 0, 0, 
     - 			0., 0., coll )

      write(buffer,100) obuf(1),obuf(2),obuf(3),obuf(4),obuf(5)
      call xvmessage(buffer,' ')
      write(buffer,100) obuf(6),obuf(7),obuf(8),obuf(9),obuf(10)
      call xvmessage(buffer,' ')
      call xvmessage(' ',' ')

      call xvmessage(' ',' ')
      call xvmessage(' TEST C INTERFACE ****',' ')
      call xvmessage(' ',' ')

      call tzdform
      
      return

 100  format(' ',I4,' ',I4,' ',I4,' ',I4,' ',I4,' ')
      end
