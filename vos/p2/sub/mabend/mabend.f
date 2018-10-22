	subroutine MABEND( msg)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c  85-5-5  ...lwk...
c  92-9-18    sp    MADE PORTABLE FOR UNIX.  MADE MSG A REQUIRED PARAMETER.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	implicit integer (a-z)
        CHARACTER*(*) msg
C==================================================================
        call xvmessage( msg, ' ')
	call abend
	end
C##################################################################

