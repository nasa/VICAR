      SUBROUTINE ITER(CHI,PHI,EPS,PI)
C     21 JAN 93    SP  PORTED TO UNIX
C	5/21/83 -JAM- CONVERT VAX
C     16 JAN 78    ...JJL...   INITIAL RELEASE
C  GET PHI FROM CHI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      VAL(EPS,PHIL)=((1.D0+EPS*DSIN(PHIL))/(1.D0-EPS*DSIN(PHIL)))**
     * (EPS/2.D0)
C==================================================================
      A=DTAN(PI/4.D0+CHI/2.D0)
      Q=A*VAL(EPS,CHI)
      PHIL=2.D0*(DATAN(Q)-PI/4.D0)
10    Q=A*VAL(EPS,PHIL)
      PHI =2.D0*(DATAN(Q)-PI/4.D0)
      IF(DABS(PHI-PHIL).LT.1.D-7) RETURN
      PHIL=PHI
      GO TO 10
      END
