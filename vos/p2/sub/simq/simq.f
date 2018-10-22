C Solve a set of simultaneous linear equations: AX = B
C Uses MATH77 (from LINPACK) routines SGEFA AND SGESLD.
C SINGLE PRECISION version.
C
      SUBROUTINE SIMQ(A,B,N,IFAIL)
      REAL A(N,N)		!Input matrix
      REAL B(N)			!Input right-hand side, output as X
      INTEGER N			!Matrix dimension
      INTEGER IFAIL		!0=success, 1=singular matrix
      INTEGER IPVT(100),IND

      IF (N .GT. 100) THEN
	CALL XVMESSAGE('***SIMQ matrix size too large',' ')
	CALL ABEND
      END IF

      IFAIL = 1
      CALL SGEFA(A,N,N,IPVT,IND)	!Get the lu decomposition of matrix A.
      IF (IND .NE. 0) RETURN		!Quit if matrix is singular
      CALL SGESLD(A,N,N,IPVT,B)		!Solve for right hand side
      IFAIL = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Solve a set of simultaneous linear equations: AX = B
C Uses MATH77 (from LINPACK) routines DGEFA AND DGESLD.
C DOUBLE PRECISION version.
C
      SUBROUTINE dsimq2(A,B,N,IFAIL)
      REAL*8 A(N,N)		!Input matrix
      REAL*8 B(N)		!Input right-hand side, output as X
      INTEGER N			!Matrix dimension
      INTEGER IFAIL		!0=success, 1=singular matrix
      INTEGER IPVT(100),IND

      IF (N .GT. 100) THEN
	CALL XVMESSAGE('***SIMQ matrix size too large',' ')
	CALL ABEND
      END IF

      IFAIL = 1
      CALL DGEFA(A,N,N,IPVT,IND)	!Get the lu decomposition of matrix A.
      IF (IND .NE. 0) RETURN		!Quit if matrix is singular
      CALL DGESLD(A,N,N,IPVT,B)		!Solve for right hand side
      IFAIL = 0
      RETURN
      END
