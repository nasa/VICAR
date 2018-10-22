C
      SUBROUTINE EXTRAP(N,L,SS,ES,PTS,BUF,RM)
C
C-----THIS IS A FORTRANIZED VERSION OF EXTRAP
      IMPLICIT INTEGER (A-Z)
      REAL*4 R,R1,R2,NOOM,DENO
      INTEGER*2 PTS(3,N),BUF(1)
C
      RMAX = RM
      K = 0
C
      DO 10 J=SS,ES
      K = K + 1
      BUF(K) = 0
      NOOM = 0.0
      DENO = 0.0
        DO 5 I=1,N
        S = I
        R1 = J - PTS(1,I)
        R2 = L - PTS(2,I)
        R = R1**2 + R2**2
        IF(R .EQ. 0.0) GO TO 6
        IF(R .GT. RMAX) GO TO 5
        NOOM = NOOM + PTS(3,I) / R
        DENO = DENO + 1./R
    5   CONTINUE
C
      IF(NOOM .EQ. 0.0 .OR. DENO .EQ. 0.0) GO TO 10
      BUF(K) = NOOM / DENO + 0.5
      GO TO 10
    6 BUF(K) = PTS(3,S)
   10 CONTINUE
C
      RETURN
      END
