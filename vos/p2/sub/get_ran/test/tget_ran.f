        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 seed1
        character*200 msg
        real randout
        seed1 = 15289

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

        write(msg,100) seed1
        call xvmessage(msg,' ') 
        call get_ran(seed1, randout)
        write(msg,101) randout
        call xvmessage(msg,' ') 

 100    format(' The input seed value: ', I)
 101    format(' The output seed value: ', F12.6)

	return
	end
