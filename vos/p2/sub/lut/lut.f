c*****************************************************************************
c LUT.F - Subroutine to translate NS bytes in buffer 'BUF' using look-up 
c         table 'TAB'
c
c 12-7-1992 M. O'Shaughnessy     Converted LUT back to Fortran from assembler
c                                code and ported it to UNIX. Removed optional
c                                number of parameters. No C-bridge.
c*****************************************************************************
	subroutine lut( ns, buf, tab, obuf)
        include 'fortport'

        integer*4 ns,i,wor
	byte      buf(*),tab(256), obuf(*)

	do i=1,ns
           wor = byte2int(buf(i))
	   obuf(i) = tab(wor+1)
        enddo

	return
	end
c*****************************************************************************
c end module
c*****************************************************************************
