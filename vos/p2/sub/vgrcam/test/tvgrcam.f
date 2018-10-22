C----------------------------------------------------------
C-----THIS PROGRAM WILL TEST SUBROUTINE VGRCAM
C-----PORTED TO UNIX 07/12/93
C----------------------------------------------------------
        INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	REAL*4 BUF(4)
C---------------------------------
C FORTRAN - CALLABLE
C---------------------------------

        CALL XVMESSAGE('*******FORTRAN-CALLABLE RFT2CH******',' ')
	CALL VGRCAM(4,BUF)
	CALL PRNT(7,4,BUF,' SN 4 VALUES.')
	CALL VGRCAM(5,BUF)
	CALL PRNT(7,4,BUF,' SN 5 VALUES.')
	CALL VGRCAM(6,BUF)
	CALL PRNT(7,4,BUF,' SN 6 VALUES.')
	CALL VGRCAM(7,BUF)
	CALL PRNT(7,4,BUF,' SN 7 VALUES.')
C
C--------------------------------------------------------------
C ----C-CALLABLE
C--------------------------------------------------------------
C
        CALL XVMESSAGE('**********C-CALLABLE RFT2CH*******',' ')
	CALL TZVGRCAM(4,BUF)
	CALL PRNT(7,4,BUF,' SN 4 VALUES.')
	CALL TZVGRCAM(5,BUF)
	CALL PRNT(7,4,BUF,' SN 5 VALUES.')
	CALL TZVGRCAM(6,BUF)
	CALL PRNT(7,4,BUF,' SN 6 VALUES.')
	CALL TZVGRCAM(7,BUF)
	CALL PRNT(7,4,BUF,' SN 7 VALUES.')
	CALL EXIT
	END
