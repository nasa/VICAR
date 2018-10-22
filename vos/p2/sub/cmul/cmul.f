	SUBROUTINE CMUL(N,R1,I,R2,J,ROUT)
C-------THIS ROUTINE REPLACES THE OLD BAL VERSION FROM 1970
C-------IT DOES A COMPLEX MULTIPLICATION OF ARRAYS
	COMPLEX*8 R1(N),R2(N),ROUT(N),A,B
C
	DO 10 L=1,N
           A = R1(L)
	   B = R2(L)
	   IF (I.EQ.1) THEN
              A = CONJG(A)
           END IF
	   IF (J.EQ.1) THEN
              B = CONJG(B)
           END IF
	   ROUT (L) = A*B
10      continue

	RETURN
	END

