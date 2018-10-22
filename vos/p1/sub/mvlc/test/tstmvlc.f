	include 'VICMAIN_FOR'
	subroutine main44
	byte byt(100)
	character*100 ch, outchar

	ch = 'This is a test. ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

	do i=1,100
	   byt(i) = ichar(ch(i:i))
	end do

	call mvlc(byt, outchar, 100)


	call xvmessage('The output is:',' ')
	call xvmessage(outchar, ' ')

	return
	end
