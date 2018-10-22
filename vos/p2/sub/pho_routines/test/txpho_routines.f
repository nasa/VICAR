c  Program TXPHO_ROUTINES

	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44

        INCLUDE 'pho'

	integer cnt, i, num, MillMode, TillMode, SunShadow, ViewShadow, status
	real*4 temp
c	real*8 dval1, IncAng, EmAng, PhasAng, pval
	real*8 dval1, pval
	real*8 MIllum(3),TIllum(3) !Illum(1,2,3)=[cos(inc),cos(em),cos(phase)] 
	character*132 msg
	character*(pho_max_func_name_length) cval1
c	character*8 cval1
c	character*(pho_max_key_length) keylist(4)
	character*(pho_max_keywd_length) keylist(pho_max_param_func)

	integer pho

	call xvmessage(' program PHO_TEST', ' ')
	call xvmessage( ' ', ' ')

	call xveaction(' ',' ')

	call pho_init( pho, status)
c get the photometric function and there input parameters from the PDF
c and set these in the photometric object :

	call pho_get_Parms( pho, status)

	i = 0

c get the list of parameter keywords for the current photometric function : 

	call pho_get_keys( pho, keylist, num, status)

c get the photometric function name :

	call pho_get_func( pho, cval1, status)

	msg = ' Function = '//cval1
	call xvmessage( msg, ' ')

	call xvmessage( ' ', ' ')

	  write( msg, 1000)  num
1000	  format( 'parameter number = ', i4)
	  call xvmessage( msg, ' ')

	do i=1,num
	  call pho_get_val( pho, keylist(i), dval1, status)
	  write( msg, 1010) keylist(i), dval1
1010	  format( a<pho_max_keywd_length>, ' = ', 1pe10.3)
	  call xvmessage( msg, ' ')
	enddo

c  reads in the function arguments from the PDF :

	call xvp('INC_ANG', temp, cnt)
	MIllum(1) = temp

	call xvp('EM_ANG', temp, cnt)
	MIllum(2) = temp

	call xvp('PHAS_ANG', temp, cnt)
	MIllum(3) = temp

	call xvmessage( ' ', ' ')

	write( msg, 1001) MIllum(1)
1001	format(' Incidence Angle = ', 1pd10.3)
	call xvmessage( msg, ' ')

	write( msg, 1002) MIllum(2)
1002	format(' Emission Angle = ', 1pd10.3)
	call xvmessage( msg, ' ')

	write( msg, 1003) MIllum(3)
1003	format(' Phase Angle = ', 1pd10.3)
	call xvmessage( msg, ' ')

	call xvmessage( ' ', ' ')


c get the bidirectional reflectance value :

	MillMode = illEllCos
	MIllum(1)=cos(MIllum(1)*rad_deg)
	MIllum(2)=cos(MIllum(2)*rad_deg)
	MIllum(3)=cos(MIllum(3)*rad_deg)

	SunShadow = illNoShadow
	ViewShadow = illNoShadow
	call pho_bidi_ref(pho,MillMode,SunShadow,ViewShadow,MIllum,pval,status) 

	write( msg, 1004) pval
1004	format(' Bidirectional Reflectance Value = ', 1pd10.3)
	call xvmessage( msg, ' ')


c get the photometric function value :

	MillMode = illEllCos
	call pho_func(pho, MillMode, SunShadow, ViewShadow, MIllum, 
     f	pval, status)

	write( msg, 1005) pval
1005	format(' Photometric Function value = ', 1pd10.3)
	call xvmessage( msg, ' ')


c get the photometric correction function value to nadir :

	MillMode = illEllCos
	TillMode = illEllCos

	TIllum(1) =1.0
	TIllum(2) =1.0
	TIllum(3) =1.0

	call pho_correct(pho,MillMode,SunShadow,ViewShadow,MIllum,
     f  TillMode,SunShadow,ViewShadow,TIllum,pval,status)

	write( msg, 1006) pval
1006	format(' to-nadir Correction Value = ', 1pd10.3)
	call xvmessage( msg, ' ')

	call xvmessage( ' ', ' ')

	call pho_free( pho, status)
	return
	end
