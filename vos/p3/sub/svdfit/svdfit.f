C*******************************************************************************
      SUBROUTINE SVDFIT(X,Y,SIG,NDATA,INDVAR,IORDER,NCOEFFS,COEFFS,
     +			U,V,W,CHISQ)
C
C	Taken from Numerical Recipes
C	Adapted to make fitting funtion a IORDER polynomial fit of INDVAR
C	independent variables.       11/30/93   Ron Alley
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(NMAX=1000,MMAX=50,TOL=1.0E-14)
      REAL*8 X(INDVAR,NDATA)		! input	- Independent variable values
      REAL*8 Y(NDATA)			! input	- Dependent variable values
      REAL*8 SIG(NDATA)			! input - Relative error estimate
      INTEGER NDATA			! input - Number of data points
      INTEGER INDVAR			! input - Num of independent variables
      INTEGER IORDER			! input - Order of the polynomial fit
      INTEGER NCOEFFS			! input - Number of coefficients output
      REAL*8 COEFFS(NCOEFFS)		! output- Fitting function coefficients
      REAL*8 U(NDATA,NCOEFFS)		! output- Scratch work buffer
      REAL*8 V(NCOEFFS,NCOEFFS)		! output- Scratch work buffer
      REAL*8 W(NCOEFFS)			! output- Scratch work buffer
      REAL*8 CHISQ			! output- chi-squared, figure of merit
C
      REAL*8 B(NMAX),AFUNC(MMAX)
C						compute terms for observed data
	DO I=1,NDATA
	    CALL NPOLY(X(1,I),AFUNC,W,NCOEFFS,INDVAR,IORDER)
	    TMP = 1.0/SIG(I)
	    DO  J=1,NCOEFFS
	        U(I,J) = AFUNC(J)*TMP
	    END DO
	    B(I) = Y(I)*TMP
	END DO
C
	CALL SVDCMP(U,NDATA,NCOEFFS,NDATA,NCOEFFS,W,V)
C						edit out singular values near 0
	WMAX = 0.0
	DO J=1,NCOEFFS
	    IF(W(J).GT.WMAX) WMAX=W(J)
	END DO
	THRESH = TOL*WMAX
	DO J=1,NCOEFFS
	    IF(W(J).LT.THRESH) W(J)=0.0
	END DO
C
	CALL SVBKSB(U,W,V,NDATA,NCOEFFS,NDATA,NCOEFFS,B,COEFFS)
C							compute chi-squared
	CHISQ = 0.0
	DO I=1,NDATA
	       CALL NPOLY(X(1,I),AFUNC,W,NCOEFFS,INDVAR,IORDER)
	    SUM = 0.0
	    DO J=1,NCOEFFS
	        SUM = SUM + COEFFS(J)*AFUNC(J)
	    END DO
	    CHISQ = CHISQ+((Y(I)-SUM)/SIG(I))**2
	END DO
	RETURN
	END
C*******************************************************************************
      SUBROUTINE SVDCMP(A,M,N,MP,NP,W,V)
C
C	Taken from Numerical Recipes
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NMAX=100)
      DIMENSION A(MP,NP),W(NP),V(NP,NP),RV1(NMAX)
      G=0.0
      SCALE=0.0
      ANORM=0.0
      DO 25 I=1,N
        L=I+1
        RV1(I)=SCALE*G
        G=0.0
        S=0.0
        SCALE=0.0
        IF (I.LE.M) THEN
          DO 11 K=I,M
            SCALE=SCALE+ABS(A(K,I))
11        CONTINUE
          IF (SCALE.NE.0.0) THEN
            DO 12 K=I,M
              A(K,I)=A(K,I)/SCALE
              S=S+A(K,I)*A(K,I)
12          CONTINUE
            F=A(I,I)
            G=-SIGN(SQRT(S),F)
            H=F*G-S
            A(I,I)=F-G
            IF (I.NE.N) THEN
              DO 15 J=L,N
                S=0.0
                DO 13 K=I,M
                  S=S+A(K,I)*A(K,J)
13              CONTINUE
                F=S/H
                DO 14 K=I,M
                  A(K,J)=A(K,J)+F*A(K,I)
14              CONTINUE
15            CONTINUE
            ENDIF
            DO 16 K= I,M
              A(K,I)=SCALE*A(K,I)
16          CONTINUE
          ENDIF
        ENDIF
        W(I)=SCALE *G
        G=0.0
        S=0.0
        SCALE=0.0
        IF ((I.LE.M).AND.(I.NE.N)) THEN
          DO 17 K=L,N
            SCALE=SCALE+ABS(A(I,K))
17        CONTINUE
          IF (SCALE.NE.0.0) THEN
            DO 18 K=L,N
              A(I,K)=A(I,K)/SCALE
              S=S+A(I,K)*A(I,K)
18          CONTINUE
            F=A(I,L)
            G=-SIGN(SQRT(S),F)
            H=F*G-S
            A(I,L)=F-G
            DO 19 K=L,N
              RV1(K)=A(I,K)/H
19          CONTINUE
            IF (I.NE.M) THEN
              DO 23 J=L,M
                S=0.0
                DO 21 K=L,N
                  S=S+A(J,K)*A(I,K)
21              CONTINUE
                DO 22 K=L,N
                  A(J,K)=A(J,K)+S*RV1(K)
22              CONTINUE
23            CONTINUE
            ENDIF
            DO 24 K=L,N
              A(I,K)=SCALE*A(I,K)
24          CONTINUE
          ENDIF
        ENDIF
        ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
25    CONTINUE
      DO 32 I=N,1,-1
        IF (I.LT.N) THEN
          IF (G.NE.0.0) THEN
            DO 26 J=L,N
              V(J,I)=(A(I,J)/A(I,L))/G
26          CONTINUE
            DO 29 J=L,N
              S=0.0
              DO 27 K=L,N
                S=S+A(I,K)*V(K,J)
27            CONTINUE
              DO 28 K=L,N
                V(K,J)=V(K,J)+S*V(K,I)
28            CONTINUE
29          CONTINUE
          ENDIF
          DO 31 J=L,N
            V(I,J)=0.0
            V(J,I)=0.0
31        CONTINUE
        ENDIF
        V(I,I)=1.0
        G=RV1(I)
        L=I
32    CONTINUE
      DO 39 I=N,1,-1
        L=I+1
        G=W(I)
        IF (I.LT.N) THEN
          DO 33 J=L,N
            A(I,J)=0.0
33        CONTINUE
        ENDIF
        IF (G.NE.0.0) THEN
          G=1.0/G
          IF (I.NE.N) THEN
            DO 36 J=L,N
              S=0.0
              DO 34 K=L,M
                S=S+A(K,I)*A(K,J)
34            CONTINUE
              F=(S/A(I,I))*G
              DO 35 K=I,M
                A(K,J)=A(K,J)+F*A(K,I)
35            CONTINUE
36          CONTINUE
          ENDIF
          DO 37 J=I,M
            A(J,I)=A(J,I)*G
37        CONTINUE
        ELSE
          DO 38 J= I,M
            A(J,I)=0.0
38        CONTINUE
        ENDIF
        A(I,I)=A(I,I)+1.0
39    CONTINUE
      DO 49 K=N,1,-1
        DO 48 ITS=1,30
          DO 41 L=K,1,-1
            NM=L-1
            IF ((ABS(RV1(L))+ANORM).EQ.ANORM)  GO TO 2
            IF ((ABS(W(NM))+ANORM).EQ.ANORM)  GO TO 1
41        CONTINUE
1         C=0.0
          S=1.0
          DO 43 I=L,K
            F=S*RV1(I)
            IF ((ABS(F)+ANORM).NE.ANORM) THEN
              G=W(I)
              H=SQRT(F*F+G*G)
              W(I)=H
              H=1.0/H
              C= (G*H)
              S=-(F*H)
              DO 42 J=1,M
                Y=A(J,NM)
                Z=A(J,I)
                A(J,NM)=(Y*C)+(Z*S)
                A(J,I)=-(Y*S)+(Z*C)
42            CONTINUE
            ENDIF
43        CONTINUE
2         Z=W(K)
          IF (L.EQ.K) THEN
            IF (Z.LT.0.0) THEN
              W(K)=-Z
              DO 44 J=1,N
                V(J,K)=-V(J,K)
44            CONTINUE
            ENDIF
            GO TO 3
          ENDIF
          IF (ITS.EQ.30) PAUSE 'No convergence in 30 iterations'
          X=W(L)
          NM=K-1
          Y=W(NM)
          G=RV1(NM)
          H=RV1(K)
          F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
          G=SQRT(F*F+1.0)
          F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
          C=1.0
          S=1.0
          DO 47 J=L,NM
            I=J+1
            G=RV1(I)
            Y=W(I)
            H=S*G
            G=C*G
            Z=SQRT(F*F+H*H)
            RV1(J)=Z
            C=F/Z
            S=H/Z
            F= (X*C)+(G*S)
            G=-(X*S)+(G*C)
            H=Y*S
            Y=Y*C
            DO 45 NM=1,N
              X=V(NM,J)
              Z=V(NM,I)
              V(NM,J)= (X*C)+(Z*S)
              V(NM,I)=-(X*S)+(Z*C)
45          CONTINUE
            Z=SQRT(F*F+H*H)
            W(J)=Z
            IF (Z.NE.0.0) THEN
              Z=1.0/Z
              C=F*Z
              S=H*Z
            ENDIF
            F= (C*G)+(S*Y)
            X=-(S*G)+(C*Y)
            DO 46 NM=1,M
              Y=A(NM,J)
              Z=A(NM,I)
              A(NM,J)= (Y*C)+(Z*S)
              A(NM,I)=-(Y*S)+(Z*C)
46          CONTINUE
47        CONTINUE
          RV1(L)=0.0
          RV1(K)=F
          W(K)=X
48      CONTINUE
3       CONTINUE
49    CONTINUE
      RETURN
      END
C*******************************************************************************
      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)
C
C	Taken from Numerical Recipes
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NMAX=100)
      DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP),TMP(NMAX)
      DO 12 J=1,N
        S=0.
        IF(W(J).NE.0.)THEN
          DO 11 I=1,M
            S=S+U(I,J)*B(I)
11        CONTINUE
          S=S/W(J)
        ENDIF
        TMP(J)=S
12    CONTINUE
      DO 14 J=1,N
        S=0.
        DO 13 JJ=1,N
          S=S+V(J,JJ)*TMP(JJ)
13      CONTINUE
        X(J)=S
14    CONTINUE
      RETURN
      END
C*******************************************************************************
	SUBROUTINE NPOLY(X,AFUNC,IPOW,NCOEFFS,INDVAR,IORDER)
C
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 X(INDVAR),AFUNC(NCOEFFS)
	INTEGER IPOW(INDVAR)
	LOGICAL UNFINISHED
C
	LOC = 0
	DO I=1,INDVAR
	    IPOW(I) = IORDER
	END DO
C
	DO WHILE(IPOW(1).GE.0)
	    N = 0
	    DO I=1,INDVAR
		N = N+IPOW(I)
	    END DO
	    IF (N .LE. IORDER) THEN
		LOC = LOC+1
		AFUNC(LOC) = 1.0
		DO I=1,INDVAR
		    AFUNC(LOC) = AFUNC(LOC)*(X(I)**IPOW(I))
		END DO
	    END IF
	    M = INDVAR
	    UNFINISHED = .TRUE.
	    DO WHILE (UNFINISHED)
		IPOW(M) = IPOW(M)-1
		IF (IPOW(M).GE.0 .OR. M.EQ.1) THEN
		    UNFINISHED = .FALSE.
		ELSE
		    IPOW(M) = IORDER
		    M = M-1
		END IF
	    END DO
	END DO
	RETURN
	END
