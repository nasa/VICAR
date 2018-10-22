C  This is the Fortran routine to test the subroutine nibble.



  	include 'VICMAIN_FOR'
        subroutine main44
	byte in(10),ot(10)
        integer*4 num
	num = 9
        do i=1,10
	in(i)=-3+i
	ot(i)=0
	end do
	call prnt(4,1,num,'nbytes_out.')
	call prnt(1,5,in,'in in dec.')
	call nibble(in,ot,9)
	call prnt(1,9,ot,'ot in dec.')
        call tznibble  ! test the "C" interface
	return
	end
