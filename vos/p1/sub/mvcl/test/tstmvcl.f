	include 'VICMAIN_FOR'
	subroutine main44
	byte byt(100)
	character*100 ch, outchar

	ch = 'This is a test. ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

	call mvcl(ch, byt, 100)

	do i=1,100
	   outchar(i:i) = char(byt(i))
	end do

	call xvmessage('The output is:',' ')
	call xvmessage(outchar, ' ')

	return
	end
