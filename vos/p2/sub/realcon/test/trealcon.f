C TEST SUBROUTINE realcon
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE REALCON     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	real*4 r(10)
        data r / 5.5e20, -3.e15, 1.7e8, 1.0, 0.9, -0.665,
     .	 0.0, 10.0E-10, 1.0711E-10, -5.67e7/
	character*20 cbuf
        data cbuf/' R =XXXXXXXXXXXXXXXX'/

c first try all numbers in field of 8 chars:
	do i=1,10
	  call realcon( r(i), cbuf(5:), 8)
          call xvmessage( cbuf(1:13), ' ')
	enddo

c now try field of 15:
	do i=1,10
	  call realcon( r(i), cbuf(5:), 15)
          call xvmessage( cbuf, ' ')
	enddo
        return
	end
