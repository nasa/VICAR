C This is the Fortran routine to test the subroutine rangen

        include 'VICMAIN_FOR'
        subroutine main44
        integer*4 seed
        character*100 msg
        real*4 rand_num

        seed = 1073741969
	do 1001 j=1,25
          call rangen(seed, rand_num )
          write(msg,100) seed, rand_num
          call xvmessage(msg,' ')
 100      format(I8, '   ',F16.14)
 1001   continue
	end
