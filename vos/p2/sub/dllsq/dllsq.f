      SUBROUTINE DLLSQ(A,B,M,N,L,X,IPIV,EPS,IER,AUX)

C 4 April 1994 ... CRI ... MSTP S/W Conversion (VICAR Porting)

C Calculates the least squares solution using the MATH77 routine DHFTI.

      DOUBLE PRECISION A(M,N),B(M,L),X(N,L),AUX(N,2),EPS
      INTEGER M,N,L,IER, IPIV(N)

C LOCAL ARRAYS AND VARIABLES.

      INTEGER I,J                       ! Loop control variables
      INTEGER RN_DIM
      PARAMETER (RN_DIM = 500)          ! MAX NUMBER OF RIGHT HAND SIDES
                                        ! = MAX VALUE FOR L.
      DOUBLE PRECISION RNORM(RN_DIM)
      INTEGER KRANK
      CHARACTER*132 CBUF
C==================================================================
      ! If the number of right-hand sides passed to DLLSQ exceeds the
      ! number of elements provided by DLLSQ then WRITE output error
      ! message and terminate
      IF (L .GT. RN_DIM) THEN
          WRITE (CBUF, 90100)
90100     FORMAT ('LOCAL ARRAYS IN SUBR DLLSQ NEED TO BE ENLARGED')
          CALL XVMESSAGE (CBUF, ' ')
          CALL ABEND
      END IF

C  CALL MATH77 ROUTINE TO SOLVE LEAST SQUARES PROBLEM

      IF ( M .GE. N ) THEN

         CALL DHFTI( A,M,M,N, B,M,L, EPS, KRANK, RNORM, AUX(1,1),
     &               AUX(1,2), IPIV )

         IF (KRANK .EQ. 0)  THEN
             IER = - 1

         ELSE IF (KRANK .LT. N)  THEN
             IER = KRANK

         ELSE

           IER = 0         ! ERROR UNLESS PSEUDO-RANK = N.

           DO J = 1, L     ! NOW COPY ANSWERS TO OUTPUT ARRAY.
           DO I = 1, N
              X(I,J) = B(I,J)
           END DO
           END DO
         END IF

      ELSE
         IER = -2        ! SINCE DHFTI USES B ARRAY FOR BOTH INPUT AND OUTPUT,
                         ! THERE IS NO CONVENIENT WAY I SEE TO HANDLE THE
                         ! CASE OF N GT M WITH THE DLLSQ CALLING SEQUENCE.

      END IF
      RETURN
      END
