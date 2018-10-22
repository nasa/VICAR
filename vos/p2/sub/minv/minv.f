      SUBROUTINE MINV (A, N, D, L, M)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                               MINV
C       ----------------                                               ---
C	General routine for inverting an array calculating its determinant
C	Fortran format of call:
C
C	CALL MINV (A, N, D, L, M)
C
C	Parameters:-
C
C	A   (input/output)  REAL N by N Input Matrix.
C                           Returned as output the resulting inverse.
C       N   (input)         order of matrix A.
C       D   (output)        resultant determinant.  D=0 if A is singular.
C       L   (input)         work vector of length N.
C       M   (input)         work vector of length N.
C
C       Inverts a matrix and calculates its determinant using
C       MATH77 (from LINPACK) routines SGEFA, SGED and SGEI.
C
C   REVISION HISTORY
C
C      07-03-95   CRI  Removed LIB_LOCAL as per FR85780
C      14-04-94   CRI  MSTP S/W Conversion (VICAR Porting)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      REAL A(N,N), L(N), M(N), D, DET(2)
      INTEGER N


      PARAMETER ( RMIN_PAR = -37 )  ! VALID MAGNITUDES FOR REAL*4 ARE ROUGHLY
      PARAMETER ( RMAX_PAR =  37 )  ! 1.E-37 TO 1.E+37.


C==================================================================

C..GET THE LU DECOMPOSITION OF MATRIX A.

      CALL SGEFA( A,N,N, L, INFO )

      IF ( INFO .NE. 0 )  THEN
         D = 0.0                   ! A IS SINGULAR.

      ELSE

C..IF NOT SINGULAR, THEN INVERT AND CALCULATE DETERMINANT.

         CALL SGED (A,N,N,L,DET)	! DETERMINANT
         CALL SGEI (A,N,N,L,M)		! INVERSE

         IF ( DET(2) .GT. RMAX_PAR )  THEN
            CALL QPRINT( ' DETERMINANT TOO LARGE IN SUB MINV' )
            DET(2) = RMAX_PAR
         END IF

         IF ( DET(2) .LT. RMIN_PAR )  THEN
            D = 0.0

         ELSE
            D = DET(1)*10.0**DET(2)

         END IF
         
      END IF

      RETURN
      END
