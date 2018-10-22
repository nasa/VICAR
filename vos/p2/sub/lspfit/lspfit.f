CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Solve the linear equation SX = U for X where X is the COEF matrix
C and U is the UMOM matrix.  This routine sets up the call for DGELG
C which actually does the work.
C
      SUBROUTINE LSPFIT(M,MOM,UMOM,coef,ier)
      IMPLICIT NONE
      INTEGER M,IER
      REAL*8 MOM(1),UMOM(1)
      REAL*8 S(5151)		!Output S matrix
                                !M <= 100, therfore the max size for S should 
                                !not be greater than 5151.
                                !(i.e. (100+1)(100+2)/2 = 5151)

      REAL*8 COEF(1)		!Output coefficients

      REAL*8 EPS/1.D-15/
      INTEGER*4 NU,IX,IY,IS,IM,J,K,L,JJ,KK

C
C     ...Check for boundary conditions
      IF ((M .LT. 0) .OR. (M .GT. 100)) THEN
          IER = 1 
         CALL MABEND ('Invalid M value.  See lspfit.hlp.')
      END IF

C
C     ....Generate S-array from moments
      IS = 0
C
      DO 20 J=0,M
      DO 20 K=0,J
C
      DO 10 JJ=0,M
      DO 10 KK=0,JJ
      IX = (JJ-KK) + (J-K)		!power of x
      IY = KK + K			!power of y
      L = IX + IY			!degree of term
      IM = (L*(L+1))/2 + IY + 1
      IS = IS + 1
   10 S(IS) = MOM(IM)

   20 CONTINUE
C
      NU = (M+1)*(M+2)/2
      CALL MVE(8,2*NU,UMOM,COEF,1,1)
CCC      CALL PRNT(8,2*NU,COEF,'UMOM=.')
CCC      CALL PRNT(8,NU*NU,S,'S=.')
      CALL DGELG(COEF,S,NU,2,EPS,IER)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Constrained Linear FIT routine CLFIT.  Finds coefficients a,b,e,f such that
C          U = a*X + b*Y + e
C          V =-b*X + a*Y + f
C subject to various constraints on a,b,e, and f.
C
      SUBROUTINE CLFIT(IFIT,MOM,UMOM,coef,ier)
      IMPLICIT NONE
      INTEGER IFIT,IER
      REAL*8 MOM(6),UMOM(6),COEF(6)

      INTEGER ROTATE,SCALE,OFFSET
      REAL*8 N,X,Y,X2,Y2,U,XU,YU,V,XV,YV,A,B,E,F,R,R1,R2
      REAL*8 EPS/1.D-15/
C
C         IFIT  ROTATE SCALE  OFFSET
C          1      0      0      1
C          2      0      1      0
C          3      0      1      1
C          4      1      0      0
C          5      1      0      1
C          6      1      1      0
C          7      1      1      1
C
      IER = -1
      IF (IFIT.LT.0.OR.IFIT.GT.7) RETURN
      IF (MOM(1).LE.1) RETURN
      N = MOM(1)
      X = MOM(2)
      Y = MOM(3)
      X2 = MOM(4)
      Y2 = MOM(6)
      U = UMOM(1)
      XU = UMOM(2)
      YU = UMOM(3)
      V = UMOM(4)
      XV = UMOM(5)
      YV = UMOM(6)
C          INITIALIZE WITH IDENTITY TRANSORMATION
      A = 1.D0
      B = 0.D0
      E = 0.D0
      F = 0.D0
      OFFSET = MOD(IFIT,2)
      SCALE = MOD(IFIT/2,2)
      ROTATE = IFIT/4
      R1 = XU + YV
      R2 = YU - XV
      R = X2 + Y2
      IF (OFFSET.EQ.1) THEN
         R1 = N*R1 - (X*U+Y*V)
         R2 = N*R2 - (Y*U-X*V)
         R = N*R - (X*X+Y*Y)
      ENDIF
      IF(SCALE.EQ.0) R=DSQRT(R1*R1+R2*R2)
      IF (ROTATE+SCALE.GT.0) THEN
         IF (DABS(R).LE.EPS) RETURN
         A = R1/R
         IF (ROTATE.EQ.1) B=R2/R
      ENDIF
      IF (OFFSET.EQ.1) THEN
         E = (U - A*X - B*Y)/N
         F = (V + B*X - A*Y)/N
      ENDIF

      COEF(1) = E
      COEF(2) = A
      COEF(3) = B
      COEF(4) = F
      COEF(5) = -B
      COEF(6) = A
      IER = 0
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute the moments of the x,y and u,v coordinates.
C
      SUBROUTINE MOMENT(NPOW,PTS,N,mom,umom)
      IMPLICIT NONE
      INTEGER NPOW,N
      REAL*4 PTS(4,1)
      REAL*8 MOM(1),UMOM(1)

      INTEGER MPOW,IPOW,NU,NM,I,J,K,L,IP1
      REAL*8 XI,YI,UI,VI,U,V,Z
C
      MPOW = 2*NPOW
      NU = (NPOW+1)*(NPOW+2)/2
      NM = (MPOW+1)*(MPOW+2)/2
      U = 0.D0
      V = 0.D0
      CALL MVE(8,NM,U,MOM,0.,1)
      CALL MVE(8,2*NU,U,UMOM,0.,1)
C
      DO 10 K=1,N
      XI = PTS(1,K)
      YI = PTS(2,K)
      UI = PTS(3,K)
      VI = PTS(4,K)
      U = U + UI
      V = V + VI
      L = 1
C
      DO 10 I=1,MPOW
      IP1 = I + 1
      IPOW = I
      Z = XI**I
      DO 10 J=1,IP1
      L = L + 1
      IF (J.GT.1) THEN
         IF (J.LT.IP1) THEN
            Z = XI**IPOW*YI**(I-IPOW)
         ELSE
            Z = YI**I
         ENDIF
      ENDIF
      MOM(L) = MOM(L) + Z
      IF (I.LE.NPOW) THEN
         UMOM(L) = UMOM(L) + UI*Z
         UMOM(NU+L) = UMOM(NU+L) + VI*Z
      ENDIF
   10 IPOW = IPOW - 1
C
      MOM(1) = MOM(1) + N
      UMOM(1) = UMOM(1) + U
      UMOM(NU+1) = UMOM(NU+1) + V
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute RMS error of polynomial fit.
C
      SUBROUTINE RMSFIT(MODE,NPOW,PTS,N,coef,drms,dmax,imax)
      IMPLICIT NONE
      INTEGER MODE,NPOW,N,IMAX
      REAL*4 PTS(4,1)
      REAL*8 COEF(100),DRMS,DMAX

      INTEGER K
      REAL*8 DU,DV
      REAL*4 X,Y,U,V
C
      DRMS = 0.D0
      DMAX = 0.D0
      IMAX = 0
C
      DO 20 K=1,N
      X = PTS(1,K)
      Y = PTS(2,K)
      CALL POLYTRAN(NPOW,COEF,X,Y,U,V)
      DU = U - PTS(3,K)
      DV = V - PTS(4,K)
      DU = DU*DU + DV*DV
      IF (DMAX.LT.DU) THEN
         DMAX = DU
         IMAX = K
      ENDIF
      DRMS = DRMS + DU
      IF (MODE.EQ.1) THEN
         PTS(3,K) = U
         PTS(4,K) = V
      ENDIF
   20 CONTINUE
C
      DMAX = SQRT(DMAX)
      DRMS = DSQRT(DRMS/N)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute coordinates (U0,V0) as a polynomial of coordinates (X0,Y0)
C  u(x,y) = a00 + a10*x + a01*y + a20*x**2 + a11*x*y + a02*y**2 + ... + a0m*y**m
C  v(x,y) = b00 + b10*x + b01*y + b20*x**2 + b11*x*y + b02*y**2 + ... + b0m*y**m
C
      SUBROUTINE POLYTRAN(M,COEF,X0,Y0,u0,v0)
      IMPLICIT NONE
      INTEGER M			!Order of the polynomial
      REAL*8 COEF(100)		!Coefficients aij and bij
      REAL*4 X0,Y0,U0,V0	!Input and outputs coordinates

      REAL*8 X,Y,U,V,Z		!All calculations done in double precision
      INTEGER NU,J,K,L
C
      NU = (M+1)*(M+2)/2	!Index to start of V values
      X = X0			!Convert inputs to double precision
      Y = Y0
      U = 0.
      V = 0.
      L = 0
C
      DO 10 J=0,M
      DO 10 K=0,J
      Z = X**(J-K)*Y**K
      L = L + 1
      U = U + COEF(L)*Z
   10 V = V + COEF(NU+L)*Z
C
      U0 = U			!Convert outputs to single precision
      V0 = V

      RETURN
      END
