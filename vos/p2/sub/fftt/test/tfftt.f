C--------------------------------------------------------------
C THIS IS A TEST OF MODULE RFT2CH
C 
C PORTED TO UNIX 6/25/93
C--------------------------------------------------------------
	INCLUDE 'VICMAIN_FOR'
	subroutine main44
	complex*8 cc(16)
	integer*2 in(16)
c
C---------------------------------
C FORTRAN - CALLABLE
C---------------------------------
c
        CALL XVMESSAGE('*******FORTRAN-CALLABLE RFT2CH******',' ')
        CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
        CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
	ipow=4
	num=16
        CALL XVREAD(IUNIT,in,STAT,'LINE',1,'nsamps',num,' ')
	do 11 l=1,num
11	cc(l) = cmplx(in(l)+0.0,0.0)
c
	call xvmessage(' print the input array',' ')
	call prnt(7,2*num,cc,' input.')
	call fftt(ipow,-1,cc)
	call xvmessage(' print the direct result',' ')
	call prnt(7,2*num,cc,' direct.')
c
	call fftt(ipow,+1,cc)
	call xvmessage(' print the inverse of the direct result',' ')
	call prnt(7,2*num,cc,' inverse.')
c
        CALL XVCLOSE(IUNIT,STAT,' ')
C
C--------------------------------------------------------------
C ----C-CALLABLE
C--------------------------------------------------------------
C
        CALL XVMESSAGE('**********C-CALLABLE RFT2CH*******',' ')
        CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
        CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
	ipow=4
	num=16
        CALL XVREAD(IUNIT,in,STAT,'LINE',1,'nsamps',num,' ')
	do 111 l=1,num
111	cc(l) = cmplx(in(l)+0.0,0.0)
c
	call xvmessage(' print the input array',' ')
	call prnt(7,2*num,cc,' input.')
	call tzfftt(ipow,-1,cc)
	call xvmessage(' print the direct result',' ')
	call prnt(7,2*num,cc,' direct.')
c
	call tzfftt(ipow,+1,cc)
	call xvmessage(' print the inverse of the direct result',' ')
	call prnt(7,2*num,cc,' inverse.')
c
        CALL XVCLOSE(IUNIT,STAT,' ')
	return
	end
