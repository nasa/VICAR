	include 'VICMAIN_FOR'
	subroutine main44
	character*200 value

	value = ' '
	call xvmessage(' ',' ')
	call xvmessage('this should be a non-existent name:', ' ')
	call xgetenv_vic( 'XYZZY', value)
	call xvmessage( 'XYZZY = '//value, ' ')

	value = ' '
	call xvmessage(' ',' ')
	call xvmessage('this should always return a value:', ' ')
	call xgetenv_vic( 'SPICEKER', value)
	call xvmessage( 'SPICEKER = '//value, ' ')

	value = ' '
	call xvmessage(' ',' ')
	call xvmessage(
	1 'this name has a system value in VMS but none in Unix:',' ')
	call xgetenv_vic( 'CLUE$HISTORY', value)
	call xvmessage( 'CLUE$HISTORY = '//value, ' ')

	return
	end
