CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Convert from output (L,S) to input (RLINE,RSAMP)
C
      SUBROUTINE PSUB(INCLUDE,L,S,rline,rsamp,ind)
      IMPLICIT NONE
      INTEGER*4 INCLUDE,L,S,IND
      REAL*8 RLINE,RSAMP

      COMMON/C1/OM(3,3),RS(3),FL,OAL,OAS,SCALE,ZSCALE
      COMMON/C1/ICAM,ITYPE,CONV(3),PROJECT
      REAL*8 OM,RS,FL,OAL,OAS,SCALE,ZSCALE
      INTEGER*4 ICAM,ITYPE,CONV
      CHARACTER*5 PROJECT

      COMMON/C2/OM2(3,3),RS2(3),RX2(3),RX2MAG
      COMMON/C2/FL2,OAL2,OAS2,SCALE2,ZSCALE2
      COMMON/C2/ICAM2,ITYPE2,CONV2(3),PROJECT2
      REAL*8 OM2,RS2,RX2,RX2MAG,FL2,OAL2,OAS2,SCALE2,ZSCALE2
      INTEGER*4 ICAM2,ITYPE2,CONV2
      CHARACTER*5 PROJECT2

      COMMON/PCONST/RA,RB,RC
      REAL*8 RA,RB,RC

      INTEGER*4 NPH,NPV		!dummy variables for CONVISOS
      REAL*8 A,B,C,D,R,SCL
      REAL*8 X,Y,Z,X0,Y0,Z0,UX0,UY0,UZ0,UX3,UY3,UZ3
      REAL*4 L4,S4

      IND = 0
C     ....Vector from ref S/C to point on planet in camera coordinates
      IF (ITYPE2.EQ.7) THEN
         L4 = L
         S4 = S
         CALL CONVISOS(PROJECT2,ICAM2,L4,S4,l4,s4,1,CONV2,NPH,NPV,ind)
         ux0 = S4 - OAS2
         uy0 = L4 - OAL2
      ELSE
         ux0 = S - OAS2
         uy0 = L - OAL2
      ENDIF
      uz0 = ZSCALE2

C     ....Convert vector to planet coordinate system (x3,y3,z3)
      ux3 = OM2(1,1)*ux0 + OM2(2,1)*uy0 + OM2(3,1)*uz0
      uy3 = OM2(1,2)*ux0 + OM2(2,2)*uy0 + OM2(3,2)*uz0
      uz3 = OM2(1,3)*ux0 + OM2(2,3)*uy0 + OM2(3,3)*uz0

C     ....Find where vector intersects planet
      x0 = ux3/ra
      y0 = uy3/rb
      z0 = uz3/rc

      a = x0*x0 + y0*y0 + z0*z0
      b = x0*rx2(1) + y0*rx2(2) + z0*rx2(3)
      c = rx2mag - 1.
      d = b*b - a*c
      if (d.lt.0.) return       !Point off the planet

      r = (-b-dsqrt(d))/a       !Choose smaller root for point in front
      x = r*ux3 + RS2(1)        !(x,y,z) is point on planet
      y = r*uy3 + RS2(2)
      z = r*uz3 + RS2(3)

      ux3 = x - RS(1)           !vector from S/C to surface point
      uy3 = y - RS(2)
      uz3 = z - RS(3)

C     ....Back-of-planet test
      if (include.eq.1) goto 50 !Skip test if sky is included
      d = ux3*(x/ra) + uy3*(y/rb) + uz3*(z/rc)
      if (d.gt.0) return

C     ....Rotate vector into camera coordinates
   50 ux0 = OM(1,1)*ux3 + OM(1,2)*uy3 + OM(1,3)*uz3
      uy0 = OM(2,1)*ux3 + OM(2,2)*uy3 + OM(2,3)*uz3
      uz0 = OM(3,1)*ux3 + OM(3,2)*uy3 + OM(3,3)*uz3
C     ....Scale vector into pixels
      SCL = ZSCALE/uz0
      IF (ITYPE.EQ.7) THEN
         L4 = SCL*uy0 + OAL
         S4 = SCL*ux0 + OAS
         CALL CONVISOS(PROJECT,ICAM,l4,s4,L4,S4,0,CONV,NPH,NPV,ind)
         RLINE = L4
         RSAMP = S4
      ELSE
         RLINE = SCL*uy0 + OAL
         RSAMP = SCL*ux0 + OAS
      ENDIF
      IND = 1
      RETURN
      END
