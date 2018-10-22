        subroutine tbswap()

        integer*4 j
	byte in(10)

	do j=1,10
           in(j)= j - 3
	end do

	call xvmessage('nbyte_pairs 5',' ')
	call prnt(1,10,in,'input in decimal')

	call BSWAP(in,5)

	call prnt(1,10,in,'output in decimal')

        return
	end


