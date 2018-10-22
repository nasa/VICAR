c test CHKSTAT

c	Include 'r2lib:main.fin/nolist'
        Include 'VICMAIN_FOR'
	Subroutine main44
	Implicit Integer (a-z)
	Integer s(2)/100,200/
c
c   First, if status = 1, the message will NOT be printed
c
	Call Chkstat( 1, 'Error in X',        0, 0, 0)
c
c   2nd,  the message will be printed since status = 2
c
	Call Chkstat( 2, 'Error in X',        0, 0, 0)
c
c   Cases 3, 4, and 5:  Make sure additional DATA is printed according to NDAT
c
	Call Chkstat( 3, 'Error in X, More Data:', 0, s, 0)
	Call Chkstat( 4, 'Error in X, More Data:', 0, s, 1)
	Call Chkstat( 5, 'Error in X, More Data:', 0, s, 2)
c
c   Case 6:  Check ABEND flag functionality
c
	Call Chkstat( 6, 'Terminal Error',    1, 0, 0)
 
	Return
	End
