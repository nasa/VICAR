      INCLUDE 'VICMAIN_FOR'
c  test subroutine CCASE

        SUBROUTINE MAIN44
	implicit integer (a-z)

	character*12 c
        data c/'a#*defgHIJKL'/
	character*8  s
        data s/'a2 defgh'/

	call xvmessage('Original string:'//c, ' ')
	call ccase(c,0,-1)
	call xvmessage('After reversing case:'//c, ' ')
	call ccase(c,1,12)
	call xvmessage('In upper case:'//c, ' ')
	call ccase(c,-1,-1)
	call xvmessage('In lower case:'//c, ' ')
	call uprcase(c)
	call xvmessage('In upper case again:'//c, ' ')

	call xvmessage('Second string:'//s, ' ')
	call ccase(s,0,5)
	call xvmessage('After reversing case of first five chars:'//s,
     .                  ' ')
	call ccase(s,1,12)      ! Make sure ccase can handles max > length.
	call xvmessage('In upper case:'//s, ' ')
	call ccase(s,-1,-1)
	call xvmessage('In lower case:'//s, ' ')
	call uprcase(s)
	call xvmessage('In upper case again:'//s, ' ')

c  Repeat test cases in C to test C interface: zccase

      CALL XVMESSAGE('REPEAT SOME TESTS FROM C LANGUAGE',' ')
      call tzccase

      RETURN
      END
