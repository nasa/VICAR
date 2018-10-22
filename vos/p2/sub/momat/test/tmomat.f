        Include  'VICMAIN_FOR'
c
C-----THIS VICAR Module TESTS Subroutine MOMAT
c
        Subroutine Main44
	Implicit real*8 (a-z)
	common/c/fl,oal,oas,scl,slat,slon,lss,sss,na,rng
	real*8 om(9),rs(3)
        character*80   header
c       Data Header /'           fl   oal   oas   scl   slat   slon   lss   sss
c    1  na   rng'/
	Header = '           fl   oal   oas   scl   slat   slon   ' //
     1		'lss   sss  na   rng'
C
C-----SET UP THE TEST INPUT BUFFER
c
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
	SEDRNA = 1
c
c-----Print the input parameters
c	call qprint('           fl   oal   oas   scl   slat   slon   lss   sss
c     1  na   rng',67)
        Call Xvmessage(Header, ' ')
	Call prnt(8,10,fl,'.')
c
C-----CALL THE SUBROUTINE TO BE TESTED
c
	CALL Momat(oal,oas,lss,sss,scl,fl,slon,slat,na,rng,om,rs,SEDRNA)
c
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
c
	Call  Prnt (8,9,oM,' OM MATRIX.')
	Call  Prnt (8,3,rs,' RS VECTOR.')
        Call  Exit(1)
c
C-----SET UP THE TEST INPUT BUFFER
c
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
	SEDRNA = O
c
c-----Print the input parameters
c
c	call qprint('           fl   oal   oas   scl   slat   slon   lss   sss
c     1  na   rng',67)
        Call Xvmessage( Header, ' ')
	Call Prnt(8,10,fl, '.')
c
C-----CALL THE SUBROUTINE TO BE TESTED
c
	Call Momat(oal,oas,lss,sss,scl,fl,slon,slat,na,rng,om,rs,SEDRNA)
c
C-----PRINT THE RESULTING OM MATRIX AND RS VECTOR
c
	Call Prnt(8,9,oM,' OM MATRIX.')
	Call Prnt(8,3,rs,' RS VECTOR.')

c	CALL EXIT

        Return
	END
