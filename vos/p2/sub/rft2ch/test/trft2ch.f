C--------------------------------------------------------------
C THIS IS A TEST OF MODULE RFT2CH
C 
C PORTED TO UNIX 6/25/93
C--------------------------------------------------------------
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	REAL*4 BUF(20),BUF2(20,22)
C---------------------------------
C FORTRAN - CALLABLE
C---------------------------------

        CALL XVMESSAGE('*******FORTRAN-CALLABLE RFT2CH******',' ')
	CALL QPRINT('01-D CASE',9)
C
C-----CONSTRUCT INPUT VECTOR
	N=20	
	DO 10 I=1,N
10	BUF(I) = I
	CALL PRNT(7,N,BUF,' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL RFT2CH(BUF,1,N,1)
C
	CALL PRNT(7,N+2,BUF,' CMPLX XFORM.')
C-----INVERT TRANSFORM
	CALL RFT2CH(BUF,1,N,-1)
C
C-----RESCALE
	A=2*N
	CALL DIVV(7,N,A,BUF,0,1)
	CALL PRNT(7,N,BUF,' INVRS XFORM.')
C
C
C---------------------------------------------------------
	CALL QPRINT('02-D CASE',9)
	N=20
	M=20
C-----CONSTRUCT INPUT ARRAY
	DO 20 J=1,M
	DO 150 I=1,N
150	BUF2(I,J) = I + J
20	CALL PRNT(7,N,BUF2(1,J),' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL RFT2CH(BUF2,M,N,1)
	DO 30 J=1,M+2
30	CALL PRNT(7,N,BUF2(1,J),' CMPLX XFORM.')
C
C-----DO INVERSE
	CALL RFT2CH(BUF2,M,N,-1)
C
	A=2*M*N
	DO 40 J=1,M
	CALL DIVV(7,N,A,BUF2(1,J),0,1)
40	CALL  PRNT(7,N,BUF2(1,J),' INVRS XFORM.')
C
C--------------------------------------------------------------
C ----C-CALLABLE
C--------------------------------------------------------------
        CALL XVMESSAGE('**********C-CALLABLE RFT2CH*******',' ')
	CALL QPRINT('01-D CASE',9)
C
C-----CONSTRUCT INPUT VECTOR
	N=20	
	DO 210 I=1,N
210	BUF(I) = I
	CALL PRNT(7,N,BUF,' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL TZRFT2CH(BUF,1,N,1)
C
	CALL PRNT(7,N+2,BUF,' CMPLX XFORM.')
C-----INVERT TRANSFORM
	CALL TZRFT2CH(BUF,1,N,-1)
C
C-----RESCALE
	A=2*N
	CALL DIVV(7,N,A,BUF,0,1)
	CALL PRNT(7,N,BUF,' INVRS XFORM.')
C
C
C---------------------------------------------------------
	CALL QPRINT('02-D CASE',9)
	N=20
	M=20
C-----CONSTRUCT INPUT ARRAY
	DO 220 J=1,M
	DO 350 I=1,N
350	BUF2(I,J) = I + J
220	CALL PRNT(7,N,BUF2(1,J),' INPUT R*4.')
C
C-----DO TRANSFORM
	CALL TZRFT2CH(BUF2,M,N,1)
	DO 230 J=1,M+2
230	CALL PRNT(7,N,BUF2(1,J),' CMPLX XFORM.')
C
C-----DO INVERSE
	CALL TZRFT2CH(BUF2,M,N,-1)
C
	A=2*M*N
	DO 240 J=1,M
	CALL DIVV(7,N,A,BUF2(1,J),0,1)
240	CALL  PRNT(7,N,BUF2(1,J),' INVRS XFORM.')
C
	RETURN
	END

