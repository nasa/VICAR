	subroutine IFMESSAGE( msg)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	implicit integer (a-z)
        PARAMETER (NOMESSAGE_PAR = 512)  !SEE RTL ROUTINE XVZINIT.
        CHARACTER*(*) msg
C==================================================================
        CALL XVP('$switch', SWTCH, N)
        SWTCH = SWTCH/NOMESSAGE_PAR     !TEST THE 512 BIT OF $SWITCH.

        IF ( MOD(SWTCH,2) .EQ. 0)  call xvmessage( msg, ' ')
	RETURN
	end
C##################################################################

