	include 'VICMAIN_FOR'

	subroutine main44

!	implicit none
!       Local variables
	real x(2)
	byte bbuf(10,10), bx
	integer*2 hbuf(5,5), hx
        integer i, j
        character*132 buffer

!       Local variable initialization
        DATA   i /0/, j /0/
	DATA   x /2*0/

!       Initiate call to 'C' version of test program to test bridge
        call tzifp

	call xvmessage ('byte input buffer:', ' ')
	do i=1,10
	  do j=1,10
	    bbuf(j,i) = 5*(i+j-1)	! construct a ramp
	  enddo
          write (buffer, 100) (bbuf(j,i),j=1,10)
100       format ('     ',10I4)
          CALL xvmessage (buffer, ' ')
	enddo

	x(1) = 5.1
	x(2) = 4.3

        write (buffer, 101) x(1), x(2)
101     format ('At X/Y=    ',2E11.4)
        CALL xvmessage (buffer, ' ')

	bx = ifp( x(1), x(2), 10, 10, bbuf, 0, 0)

        write (buffer, 103) bx
103     format ('IFP =  ', I6)
        CALL xvmessage (buffer, ' ')
c
	call xvmessage (' ', ' ')
	call xvmessage ('halfword input buffer:', ' ')
	do i=1,5
	  do j=1,5
	    hbuf(j,i) = 100*(i+j-1)	! construct a ramp
	  enddo
          write (buffer, 201) (hbuf(j,i),j=1,5)
201       format ('     ',5I6)
          CALL xvmessage (buffer, ' ')
	enddo

	x(1) = 2.1
	x(2) = 3.3

        write (buffer, 101) x(1), x(2)
        CALL xvmessage (buffer, ' ')

	hx = ifp( x(1), x(2), 5, 5, hbuf, 0, 1)

        write (buffer, 203) hx
203     format ('IFP =     ', I5)
        CALL xvmessage (buffer, ' ')
c
	return
	end
