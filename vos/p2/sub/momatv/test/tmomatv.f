        Include 'VICMAIN_FOR'
c
C-----THIS PROGRAM TESTS SUBROUTINE MOMATV
c
        Subroutine  Main44
	implicit real*8 (a-z)
	common/c/fl,oal,oas,scl,slat,slon,lss,sss,na,rng
	real*8 om(9),rs(3)
        character*80   header
        Data Header /'           fl   oal   oas   scl   slat   slon   lss   sss
     1  na   rng'/

C
C-----SET UP THE TEST INPUT BUFFER
	fl = 1500.1904
	oal = 500.
	oas = 500.
	scl = 84.821431
	slat = 3.4825339
	slon = 116.72441
	lss = -121.600
	sss = 1662.700
	na = 160.8923
 	rng = 14967727.
c
c-----print the input parameters
c
c	call qprint('           fl   oal   oas   scl   slat   slon   lss   sss  
c     1  na   rng',67)

        Call Xvmessage(Header, ' ')  
	call prnt(8,10,fl,'.')
c
C-----CALL THE SUBROUTINE TO BE TESTED
c
	CALL momatv(oal,oas,lss,sss,scl,fl,slon,slat,na,rng,om,rs)
c
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
c
	CALL PRNT(8,9,oM,' OM MATRIX.')
	CALL PRNT(8,3,rs,' RS VECTOR.')

        Return  
	END
