C Routine to sort an INTEGER*4 array
C
      SUBROUTINE SORTIN(BUF,N)
      INTEGER*4 BUF(N)
C
      IF (N.LE.1) RETURN
      L = N
      M = N/2
C
   10 K = M
      IBUF = BUF(K)
C
   20 J = 2*K
      IF (J.GT.N) GOTO 25
      IF (J.LT.N.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 25
      BUF(K) = BUF(J)
      K = J
      GOTO 20
C
   25 BUF(K) = IBUF
      M = M - 1
      IF (M.GT.0) GOTO 10
C
   30 K = 1
      IBUF = BUF(K)
C
   40 J = 2*K
      IF (J.GT.L) GOTO 45
      IF (J.LT.L.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 45
      BUF(K) = BUF(J)
      K = J
      GOTO 40
C
   45 BUF(K) = IBUF
      IBUF = BUF(1)
      BUF(1) = BUF(L)
      BUF(L) = IBUF
      L = L - 1
      IF (L.GT.1) GOTO 30
C
      RETURN
      END
C Routine to sort an INTEGER*2 array, with pointer.
C This is a modification of SORTIN
C
      SUBROUTINE I2SORT(BUF,PTR,N)
      INTEGER*2 BUF(N),PTR(N)
C
      IF (N.EQ.1) RETURN
      L = N
      M = N/2
C
   10 K = M
      IBUF = BUF(K)
      IPTR = PTR(K)
C
   20 J = 2*K
      IF (J.GT.N) GOTO 25
      IF (J.LT.N.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 25
      BUF(K) = BUF(J)
      PTR(K) = PTR(J)
      K = J
      GOTO 20
C
   25 BUF(K) = IBUF
      PTR(K) = IPTR
      M = M - 1
      IF (M.GT.0) GOTO 10
C
   30 K = 1
      IBUF = BUF(K)
      IPTR = PTR(K)
C
   40 J = 2*K
      IF (J.GT.L) GOTO 45
      IF (J.LT.L.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 45
      BUF(K) = BUF(J)
      PTR(K) = PTR(J)
      K = J
      GOTO 40
C
   45 BUF(K) = IBUF
      PTR(K) = IPTR
      IBUF = BUF(1)
      IPTR = PTR(1)
      BUF(1) = BUF(L)
      PTR(1) = PTR(L)
      BUF(L) = IBUF
      PTR(L) = IPTR
      L = L - 1
      IF (L.GT.1) GOTO 30
C
      RETURN
      END
