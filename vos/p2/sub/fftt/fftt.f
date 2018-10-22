C*****************************************************************
C
C     Subroutine FFTT
C     
C     Purpose:  To compute 1-D ffts using RFT2CH instead of
C     FFTT.   It will accept and return the assumed formats.
C     MD comes in with  -1 = direct  +1 = inverse.
C
C        1 JULY 1993   PORTED TO UNIX  (T. L. TRUONG)
C        9-88  SP      MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C*****************************************************************
C
	SUBROUTINE FFTT(IPOW,MD,COM)
	REAL*4 BUF(1026),COM(2050)
C
C ...Where    IPOW is the input size = 2**power elements
C             MD   is +1 for inverse transform
C                     -1 for direct transform
C             COM  is a complex*8 data array
C             BUF  is a buffer containing data to be processed
C
        M=1
	N= 2**IPOW
	MODE = -MD          !CHANGE TO -1 = INVERSE
	IF(MODE .EQ. -1) GO TO 10
C
C------------------------------------
C    FORWARD
C------------------------------------
	CALL MVE(7,N,COM,BUF,2,1)    !STRIP REALS OUT OF COMPLEX
	CALL RFT2CH(BUF,M,N,MODE)    !RETURNED AS COMPLEX
        CALL MVE(7,N+2,buf,com,1,1)
        RETURN
C
C------------------------------------
C    INVERSE
C------------------------------------
10	CALL MVE(7,N+2,COM,BUF,1,1)
	CALL RFT2CH(BUF,M,N,MODE)    !PROCESS COMPLEX DATA
	CALL ZIA(COM,2*N)
	S = 2 * N                    !SCALING FACTOR
	CALL DIVV(7,N,S,BUF,0,1)      !GET VALUES TO PROPER SCALE
	CALL MVE(7,N,BUF,COM,1,2)    !STICK REALS INTO COMPLEX C
        RETURN
C
        END

