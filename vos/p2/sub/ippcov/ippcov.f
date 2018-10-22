      SUBROUTINE IPPCOV(RLAT,RLON,Z,X,A,RS,OM,E,CL,CS,FLAG)
C
C
C        FUNCTION -
C
C          CALCULATES LATITUDE AND LONGITUDE FOR SPECIFIED DATA
C          POINT ON AN INVERSE PERSPECTIVE PROJECTION
C
C        PARAMETERS -
C
C          RLAT = CALCULATED LATITUDE(RADIANS)
C          RLON = CALCULATED LONGITUDE (E. RADIANS)
C          Z = LINE VALUE OF INPUT POINT
C          X = SAMPLE VALUE OF INPUT POINT
C          A = DESCRIPTIVE ARRAY
C              A(1) = EQUATORIAL RADIUS
C              A(2) = CAMERA FOCAL LENGTH
C              A(3) = DISTANCE PLANET TO SPACECRAFT
C
C        OTHER VARIABLES -
C
C          OM = ROTATION MATRIX PLANET TO CAMERA - om matrix
C          E = ECCENTRICITY MATRIX (1.,0.,0.,0.,1.,0.,0.,0.,re/rp)
C          RS = POSITION OF SPACECRAFT IN PLANET COORDINATES-rs vector
C
C
      REAL A(3)
      DOUBLE PRECISION RS(3), OM(3,3),E(3,3),T(3),U(3),V(3),AA,BB,CC,
     * DENOM,D,XX,ZZ
      DATA TWOPI/6.283185308/
C
C        INITIALIZATION
C
      AA = 0.
      BB = 0.
      CC = 0.
      DO 20 I = 1,3
         T(I) = 0.
         U(I) = 0.
   20 CONTINUE
      XX=X-CS
      ZZ=Z-CL
C
C        CALCULATE QUADRATIC COEFFICIENTS
C          AA = (NORM(U))**2
C          BB = 2 * DOT(U,V)
C          CC = (NORM(V))**2 - R**2
C              WHERE
C          T = R * (XX,ZZ,A(2))
C          U = E * T
C          V = E * RS
C
C        NOTE -- R IS ROTATION FROM PLANET TO CAMERA COORDINATES,
C          SO MULTIPLY IN TRANSPOSE SENSE
C
      DO 40 I = 1,3
   40 T(I) = OM(1,I)*XX + OM(2,I)*ZZ + OM(3,I)*A(2)
      DO 50 I = 1,3
      U(I) = E(I,1)*T(1) + E(I,2)*T(2) + E(I,3)*T(3)
   50 V(I) = E(I,1)*RS(1) + E(I,2)*RS(2) + E(I,3)*RS(3)
C
      AA = U(1)*U(1) + U(2)*U(2) + U(3)*U(3)
      BB = U(1)*V(1) + U(2)*V(2) + U(3)*V(3)
      CC = V(1)*V(1) + V(2)*V(2) + V(3)*V(3)
      BB = 2. * BB
      CC = CC - A(1)*A(1)
C
C        CALCULATE DEPTH FACTOR 'D'.  NEGATIVE DISCRIMINANT MEANS
C        POINT OFF PLANET.  USE -SQRT TO INSURE VISIBLE SIDE OF
C        PLANET.
C
      D = BB*BB - 4.*AA*CC
      IF (D .GE. 0.) GO TO 70
      RLAT=FLAG
      RLON=FLAG
      RETURN
   70 D = (-BB - DSQRT(D))/(2.*AA)
C
C        FIND RLAT AND RLON
C
      DO 80 I = 1,3
   80 T(I) = D * T(I) + RS(I)
      DENOM = DSQRT(T(1)*T(1) + T(2)*T(2))
      RLAT = DATAN(T(3)/DENOM)
      RLON = DATAN2(T(2),T(1)) + TWOPI
      RLON = AMOD(RLON,TWOPI)
      RETURN
      END
