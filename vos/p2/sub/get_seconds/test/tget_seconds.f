        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 time_sec
        character*100 msg
       
        call get_seconds( time_sec )
        write(msg,100) time_sec
        call xvmessage(msg,' ') 

 100    format(' The GMT time in seconds since 1-JAN-1970 is: ', I)

	return
	end
