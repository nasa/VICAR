C Given latitude and longitude of a point on an oblate spheroid,
C returns the line and sample values of that point for a geometrically
C corrected perspective projection.
C
C        PARAMETERS -
C
C          IND = RETURN INDICATOR
C              0 = NORMAL RETURN
C              99 = POINT BEHIND PLANET
C              -1 ON ENTRY SUPPRESSES BACK-OF-PLANET TEST
C          XL = RETURNED LINE VALUE (REAL
C          YS = RETURNED SAMPLE VALUE (REAL)
C          RLAT = INPUT LATITUDE (DEGREES)
C          RLON = INPUT LONGITUDE (DEGREES)
C            RS = INPUT VECTOR TO SPACECRAFT IN PLANET COORDINATE SYSTEM
C            FLAT = INPUT POLAR FLATTENING (EQ. RAD - POL. RAD.)
C            FOCL = INPUT CAMERA FOCAL LENGTH
C            REQ = INPUT PLANET EQUATORIAL RADIUS
C
C
      SUBROUTINE CORCAV(IND,RLAT,RLON,OM,RS,FOCL,REQ,FLAT,XL,YS,
     &CL,CS,FLAG)
      DOUBLE PRECISION OM(3,3),RS(3)
      DIMENSION SP(3),CFP(3)
      REAL PIFAC/57.29577951/
      RAD(PHI,REQ,FLAT)=REQ/SQRT(  REQ*REQ/((REQ-FLAT)**2)+(1.-REQ*REQ/
     &((REQ-FLAT)**2))*(COS(PHI*3.14159265/180.))**2 )

C        SOLVE FOR CAMERA FOCAL PLANE (CFP) COORDINATES

      CPJ = COS(RLAT/PIFAC)
      SPJ = SIN(RLAT/PIFAC)
      CLJ = COS(RLON/PIFAC)
      SLJ = SIN(RLON/PIFAC)

C     ....Compute geocentric radius
      IF (RLAT.EQ.0. .OR. ABS(RLAT).EQ.180.) THEN
         RJ = REQ
      ELSE
         RJ=RAD(RLAT,REQ,FLAT)
      ENDIF

      SP(1)=RJ*CPJ*CLJ-RS(1)
      SP(2)=RJ*CPJ*SLJ-RS(2)
      SP(3)=RJ*SPJ-RS(3)

      DO 120 I = 1,3
  120 CFP(I) = OM(I,1)*SP(1) + OM(I,2)*SP(2) + OM(I,3)*SP(3)

C  SKIP BACK-OF-PLANET TEST IF IND=-1

      IF(IND.EQ.-1)GO TO 130

C        TEST FOR BACK OF PLANET

      ZETA=0.
      TST=0.
      DO 125 I=1,3
      ZETA=ZETA+SP(I)**2
125   TST=TST+RS(I)**2
      TST=TST-RJ**2
      IF(ZETA.LE.TST) GO TO 130
      XS=FLAG
      ZL=FLAG
      IND = 99
      RETURN

C        SOLVE FOR PIXEL COORDINATES

130   U=FOCL*CFP(1)/CFP(3)
      V=FOCL*CFP(2)/CFP(3)
      XL=CL+V
      YS=CS+U
      IND = 0
      RETURN
      END
