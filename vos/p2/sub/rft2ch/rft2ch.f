C************************************************************************
C
C     Subroutine RFT2CH
C
C     Purpose:  To compute 1-D and 2-D real FFT's in core.
C     RFT2CH calls RFT2, DFFT, and REALTR.
C
C     25 JUN 93   T. L. Truong   PORTED TO UNIX
C     10 OCT 83    ...CCA...     CONVERT TO VAX
C     5 MAR 79    ...JJL...      INITIAL RELEASE
c 15-Jan-2013  ...lwk... fixed continued CHARACTER constant for new compiler flag on Solaris
C
C************************************************************************

      SUBROUTINE RFT2CH (BUF,M,N,MODE)
      REAL*4 BUF(*)
C
C ...Where   M     is the number of lines in the data matrix to be transformed
C            N     is the number of samples in data matrix to be transformed
C            MODE  is the direction of the transform, 1(direct) or -1(inverse)
C            BUF   is a buffer containing the data to be transformed
C
      IF(M.GT.1) GO TO 10
      K=N/2
      IF(N.NE.K*2) GO TO 1
      IF(MODE.LT.0) GO TO 11
C****************************
C  DIRECT REAL 1-D FFT
C****************************
      CALL DFFT(BUF(1),BUF(2),K,K,K,2,*1,*2)
      CALL REALTR(BUF(1),BUF(2),K,2)
      RETURN
C****************************
C  INVERSE REAL 1-D FFT
C****************************
11    CONTINUE
      CALL REALTR(BUF(1),BUF(2),K,-2)
      CALL DFFT(BUF(1),BUF(2),K,K,K,-2,*1,*2)
      RETURN
10    CONTINUE
C****************************
C  REAL 2-D FFT DIRECT AND INVERSE
C****************************
      CALL RFT2(BUF,M,N,MODE,ISTATUS)

      IF (ISTATUS.NE.1) THEN
       IF (ISTATUS.EQ.1) GOTO 1
       IF (ISTATUS.EQ.2) GOTO 2
       IF (ISTATUS.EQ.3) GOTO 3
      ENDIF

      RETURN
C****************************
C  PRINT ERROR MESSAGES
C****************************
1     CALL QPRINT(
     +  '0A PRIME FACTOR OF M OR N EXCEEDS 23, RFT2 ABEND',48)
      CALL ABEND
2     CALL QPRINT('0THE PRODUCT OF THE SQUARE-FREE FACTORS OF M OR N IS TOO LARGE, RFT2 ABEND',75)
      CALL ABEND
3     CALL QPRINT('0M IS ODD, RFT2 ABEND',21)
      CALL ABEND
      RETURN
      END
