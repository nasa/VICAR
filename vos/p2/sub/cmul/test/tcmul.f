	include 'VICMAIN_FOR'

	subroutine main44


	complex*8 c1(3)/(1.,5.),(2.,2.),(3.,.5)/
	complex*8 c2(3)/(.5,2.),(1.,5.),(2.,1.)/,c3(3)
        integer n, i, j
        character*132 string
        data n /0/, i /0/, j /0/

        call xvmessage (' ',' ')
        call xvmessage 
     &   ('Perform test cases in C to test c interface: zcmul', ' ')
        call xvmessage (' ',' ')
        call tzcmul

        call xvmessage (' ',' ')
        call xvmessage 
     &   ('Repeat test cases using fortran interface: cmul', ' ')
        call xvmessage (' ',' ')

	n=3
	i=0
	j=0
        write (string,100) c1(1),c1(2),c1(3)
  100   format ('inl    '6E12.4)
        call xvmessage (string, ' ')

        write (string,110) c2(1),c2(2),c2(3)
  110   format ('in2    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for both not conjugated',' ')

	call cmul(n,c1,i,c2,j,c3)

        write (string,120) c3(1),c3(2),c3(3)
  120   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for first conjugated',' ')

	i = 1
	call cmul(n,c1,i,c2,j,c3)

        write (string,130) c3(1),c3(2),c3(3)
  130   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for second conjugated',' ')

	i=0
	j=1
	call cmul(n,c1,i,c2,j,c3)

        write (string,140) c3(1),c3(2),c3(3)
  140   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	call xvmessage ('calculate product and print result',' ')
	call xvmessage ('for both conjugated',' ')
	i=1

	call cmul(n,c1,i,c2,j,c3)

        write (string,150) c3(1),c3(2),c3(3)
  150   format ('result    ',6E12.4)
        call xvmessage (string, ' ')

	return
	end
