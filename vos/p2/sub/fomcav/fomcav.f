c
      Subroutine  FOMCAV(IND,NPOINT,A,B,OM,RS,CL,CS)
c
C     11 JAN 78   ...JJL...   INITIAL RELEASE
C     27 JAN 87   ...SP....   REPLACED CALL TO NAG F04AMF WITH CALL TO
C                             MATH77 DHFTI.
C     12 FEB 87   ...SP....   ADDED ERROR CHECKING AFTER DHFTI CALL.
c     15 Jan 93   ...WPL...   Ported for UNIX Conversion
C
C     This routine is a modification of the routine CORCAL as it
C     was in SUPERMAP.   Given a control net of three or more
C     points  on a planetary image, the routine will calculate the
C     planet to camera transformation matrix.
C     This version allows more than three points to be used in the
C     transformation calculation.  If more than 3 image points are
C     specified, then a Linear Least Squares solution to the
C     problem is obtained.  Redundancy in control point specification
C     improves the transformation matrix soln.
C
C     P A R A M E T E R S . .
C
C     IND    = RETURN INDICATOR
C              0 = NORMAL RETURN
C               1 = INITIALIZATION FAILURE
C     A      = RETICLE DESCRIPTIVE ARRAY
C          A(1,J) = LINE VALUE OF JTH RETICLE POINT
C               A(2,J) = SAMP VALUE OF JTH RETICLE POINT
C               A(3,J) = SLANT RANGE TO JTH RETICLE POINT
C               A(4,J) = LATITUDE OF JTH RETICLE POINT
C               A(5,J) = LONGITUDE OF JTH RETICLE POINT
C                  , J=1,2,...,NPOINT-1,NPOINT
C     B      = PLANET AND SPACECRAFT DESCRIPTIVE ARRAY
C               B(1) = CAMERA FOCAL LENGTH (IN PIXELS)
C               B(2) = POLAR FLATTENING (RADIUS AT EQ-RAD.AT POLE)
C               B(3) = DISTANCE PLANET (CENTER ) TO SPACECRAFT
C               B(4) = LATITUDE OF SUBSPACECRAFT POINT
C               B(5) = LONGITUDE OF SUBSPACECRAFT POINT
C               B(6) = EQUATORIAL RADIUS
C     OM - R*8 PLANET TO CAMERA TRANSFORMATION MATRIX
C     RS - PLANET TO CAMERA POSITION VECTOR IN PLANET SYSTEM
C     CL/CS - LINE/SAMPLE OF CAMERA AXIS
C
C
      Implicit  Real*8 (A-H,O-Z)
      Double Precision  OMT(20,3)
c     ,OM(3,3)
      Real*4   A(5,20),B(6), CL,CS
      Real*8   RS(3), OM(3,3)
      Real*8  DETOMT, PLANET(20,3), CAMERA(20,3)
c       , AUX(20,3)
      Double Precision  RNORM(3), H(3), G(3)
      Integer    IPIV(3)
      Equivalence   (OMT, CAMERA)
C
C      MAXN - MAX VALUE OFNPOINT AND NO.OF ROWS IN PLANET/CAMERA ARRAYS
c
c      INTEGER  MAXN
c      Data MAXN/20/
c      LOGICAL*1 MSG1(45)/' ','*','*','*',' ','R','A','W',' ','(','T','R'
c    &,'A','N','S','F','O','R','M','A','T','I','O','N',')',' ','D','E',
c    &'T','E','R','M','I','N','A','N','T',' ','=',' ','X','X','.','X',
c    &'X'
c    &/
      Character*80  MSG1
      Data MSG1/'*** RAW (TRANSFORMATION) DETERMINANT = XX.XX'/

      RAD(PHI,REQ,FLAT) = REQ/DSQRT(REQ*REQ/((REQ-FLAT)**2)+(1.D0-
     *  REQ*REQ/((REQ-FLAT)**2))*(DCOS(PHI*PIIS/180.D0))**2)
      PIIS = 3.141592653589793D0
      PIFAC = 180.D0/PIIS
      EPS = 1.D-14
C
C     Initialize coefficient and right hand matrices
C
      CPS = DCOS(B(4)/PIFAC)
      SPS = DSIN(B(4)/PIFAC)
      CLS = DCOS(B(5)/PIFAC)
      SLS = -DSIN(B(5)/PIFAC)
      RS(1) = B(3)*CPS*CLS
      RS(2) = B(3)*CPS*SLS
      RS(3) = B(3)*SPS
C
C        Generate matrices of control point coordinates given in
C        camera centered planet coordinates (PLANET) and in camera
C        centered camera coordinates (CAMERA)
C
      Do J = 1, NPOINT
        RJ =  RAD(DBLE(A(4,J)),DBLE(B(6)),DBLE(B(2)))
        CPJ = DCOS(A(4,J)/PIFAC)
        SPJ = DSIN(A(4,J)/PIFAC)
        CLJ = DCOS(A(5,J)/PIFAC)
        SLJ = -DSIN(A(5,J)/PIFAC)
C
        PLANET(J,1) = RJ*CPJ*CLJ-RS(1)
        PLANET(J,2) = RJ*CPJ*SLJ-RS(2)
        PLANET(J,3) = RJ*SPJ-RS(3)
        A(3,J) = DSQRT(PLANET(J,1)**2+PLANET(J,2)**2+PLANET(J,3)**2)
C
        DENOM = DSQRT((A(1,J)-DBLE(CL))**2+(A(2,J)-DBLE(CS))**2+
     *    B(1)*DBLE(B(1)))
        FACT = A(3,J)/DENOM
C
        CAMERA(J,1) = (A(2,J)-CS)*FACT
        CAMERA(J,2) = (A(1,J)-CL)*FACT
        CAMERA(J,3) = B(1)*FACT
      END DO
C
C        Solve for transpose (INVERSE) of orientation matrix (OMT)
C        and check for errors
C
C        Find a least squares solution to the following problem
C
C        (PLANET)X(OMT) = (CAMERA)
C
C        where planet is a matrix of npoint row vectors
C        of planetary points in camera centered planet coords.
C        Camera is a matrix of npoint row vectors of the same
C        planetary points given in camera centered camera coords. And
C        OMT is the orthogonal transformation matrix to rotate
C        from planet to camera coordinates.
C

      Call DHFTI( PLANET,20, NPOINT,3, CAMERA, 20,3, 1.D-15,
     .            KRANK, RNORM, H, G, IPIV)
      If (KRANK .LT. 3)  Then   ! CHECK FOR UNDERDETERMINED CASE.
          IND = 1
          Return
      END IF

C
C        We will force the resulting rotation matrix to be
C        orthogonal since orthogonality constraints were not
C        incorporated into Least Squares Solution.
C        Validity of this approach rests on the assumption that the
C        sample points are fairly accurate and the calculated
C        transformation is close to being orthogonal.
C        (another approach would be to utilize lagrangian
C        multipliers to insure orthogonality).
C        The (Least Squares) calculated rotation matrix will be multiplied
C        by the scalar  (1/D**(1/3)) , where D is the
C        transformation determinant.

      DETOMT = OMT(1,1)*(OMT(2,2)*OMT(3,3)-OMT(2,3)*OMT(3,2))
     &-OMT(1,2)*(OMT(2,1)*OMT(3,3)-OMT(2,3)*OMT(3,1))
     &+OMT(1,3)*(OMT(2,1)*OMT(3,2)-OMT(2,2)*OMT(3,1))
      D = DETOMT
c     CALL OUTCON(D,MSG1(45),5,2)
c     CALL QPRINT(MSG1,45)
      Write(MSG1(40:44), '(F5.2)') D
      Call Xvmessage(MSG1, ' ')
      IF (DETOMT.LE.0.01D0 .OR. DETOMT.GE.10.D0) THEN
         IND = 3
         Return
      End IF
      DETOMT = 1.D0/(DETOMT**(1./3.))
c
C         TRANSPOSE RESULT
c
      DO I = 1, 3
         DO J = 1, 3
            OM(I,J) = OMT(J,I)*DETOMT
         END DO
      END DO
C
      IND=0
c 
      Return
      End
