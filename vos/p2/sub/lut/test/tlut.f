C*****************************************************************************
C Unit test program TLUT.F for subroutine LUT
c Ported to UNIX 12/7/1992.
C*****************************************************************************
	include 'VICMAIN_FOR'
	subroutine main44
        include 'fortport'
c        
        integer*4     i,x
	byte          buf(256), tab(256), obuf(256), bx
c
	call xvmessage('construct complement lookup table',' ')
	do i=1,256
	  x = 256-i
          bx = int2byte(x)
	  tab(i) = bx

	  x = i - 1
          bx = int2byte(x)
	  buf(i) = bx
	enddo

	call prnt(1, 256, buf, ' input buffer =.')

	call lut( 256, buf, tab, obuf)
	call prnt(1, 256, obuf, ' output buffer =.')

	call lut( 256, buf, tab, obuf)
        call xvmessage('next buffer should = input buffer',' ')
	call prnt(1, 256, buf, ' output buffer =.')

	return
	end





