        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 ot(6)
        character*80 message

        do i=1,6
        ot(i)=-3+i
        end do
        write (message,'(A,6I4,A)')
     +     '    A =(',ot(1),ot(2),ot(3),ot(4),ot(5),ot(6),'  )'
        call xvmessage(message,' ')
	call ZIA(ot,6)
	write (message,'(A,6I4,A)')
     +     'ZIA(A)=(',ot(1),ot(2),ot(3),ot(4),ot(5),ot(6),'  )'
        call xvmessage(message,' ')
	return
	end
