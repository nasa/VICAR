	include 'VICMAIN_FOR'
	subroutine main44

	implicit none
	real*4	buffer(32000)
	real*4	mean,sigma
	real*4	x,y,u,t
	real*4	ranx,rany
	integer*4	nl,ns
	integer*4	cnt,status
	integer*4	line,samp
	integer*4	unit,seed
	integer*8 dseed
	character*8 format

	call xvmessage('gausnois version 2016-06-08',' ')
	call xvp('NL',nl,cnt)
	call xvp('NS',ns,cnt)
	call xvp('FORMAT',format,cnt)

	call xvunit(unit,'OUT',1,status,' ')
	call xvopen(unit,status, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT',format,
     +		'U_NL',NL, 'U_NS',NS,' ')
	call xvp('SEED',seed,cnt)
	dseed = seed
c	get_seconds parm is (long)
	if (cnt .eq. 0)  then				!no seed entered
		call get_seconds(dseed)			!p2 subroutine
	endif
        call xvp('MEAN',mean,cnt)
        call xvp('SIGMA',sigma,cnt)
c	rangen parms are (long,float)
	do line = 1,nl
	    do samp = 1,ns
		call rangen(dseed,ranx)
		call rangen(dseed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt((-2.)*log(x)) * cos(2*3.1415927*y)
		t = sigma*u
		buffer(samp) = sigma*u + mean
	    enddo
	    call xvwrit(unit,buffer,status,' ')
	enddo

	call xvclose(unit,status,' ')

	return
	end
