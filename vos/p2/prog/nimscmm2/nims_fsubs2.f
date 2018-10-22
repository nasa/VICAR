      SUBROUTINE nPPROJ(DATA,LINE,SAMP,LAT,LON,IMODE,ILAT,RADIUS,SRANGE,
     & IND)

c  (this is also a VICAR RTL routine, but this version has several
c   NIMS-specific idiosyncracies)

c DATA = standard 40-word geometry buffer (see subroutine CONVEV) for 
c	Perspective case, except that word 31 = equatorial semi-minor axis

c LINE,SAMP = object space location of a point

c LAT,LON = planetary coordinates of point (degrees, West Lon.)

c MODE:  1 = (LAT,LON) to (LINE,SAMP)   2 = (LINE,SAMP) to (LAT,LON) 

c ILAT:  0 = planetocentric latitudes   1 = planetodetic latitudes
c	NOTE: planetodetic is not allowed for tri-axial case

c RADIUS = distance from planet center to the intercept point of the line 
c	of sight with the planet surface.  If the point falls off the planet 
c	then it is the tangent radius to the line of sight from planet center.
c	Ignored when MODE=1.

c SRANGE = distance from the spacecraft to the intercept point of the line
c	of sight with the planet surface.  If the point falls off the planet 
c	then it is the distance to the tangent point (see RADIUS)
c	Ignored when MODE=1.

c IND = return indicator.  1=success, 0=failure.

c   2Aug90  lwk  added ILAT parameter
c  10Sep90  lwk  added RTANG parameter
c  27Jun91  lwk  renamed RTANG to RADIUS (& fixed it for ellipsoidal case),
c                  added SRANGE parameter
c  10dec92  lwk  used RADIUS in mode=2 to allow disabling of BOP test
c  31oct93  lwk  implemented tri-axial ellipsoid model with extra radius
c		in word 31 of DATA
c  15may94  lwk  fixed BOP test for triaxial ellipsoid using NAIF calls

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CP(3),OP(3),CPP(3),OM(3,3),RS(3),xnorm(3)
      REAL*4 EPS/1.E-6/
      REAL*4 LAT,LON,LINE,SAMP,RADIUS,SRANGE,DATA(*)
      DATA PI, RADDEG, DEGRAD / 3.141592653589793D0, 5.72957795130823D1,
     & 1.74532925199433D-2/
C
      IND = 0
      FL = DATA(27)
      OAL = DATA(28)
      OAS = DATA(29)
      SCALE = DATA(30)
      RP = DATA(25)	! polar radius
      RA = DATA(26)	! equatorial semi-major radius
      RB = DATA(31)	! equatorial semi-minor radius
	! check if RB is garbage, in which case assume oblate spheroid:
      IF (RB.LT.RP .OR. RB.GT.RA) RB = RA
	! we don't have code for this yet:
      IF (RA.NE.RB .AND. ILAT.EQ.1) CALL MABEND(
     & ' *** PPROJ:  TRIAXIAL BODY WITH PLANETODETIC LATS ***')
      E1 = (RA/RP)
      E1 = E1*E1
      E2 = (RB/RP)
      E2 = E2*E2
c      CALL MVL(DATA,OM,72)
c      CALL MVL(DATA(19),RS,24)
      call mve(1,72,data,om,1,1)
      call mve(1,24,data(19),rs,1,1)
C
      IF(IMODE.EQ.2) GOTO 30
C     ....Here to convert (lat,lon) to (line,samp)
      RLAT = LAT*DEGRAD
      RLON = LON*DEGRAD
C          CONVERT FROM GEODETIC TO GEOCENTRIC LATITUDE
      IF (ILAT.EQ.1 .AND. ABS(LAT).NE.90.D0) RLAT=DATAN(DTAN(RLAT)/E1)
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
      CLON = DCOS(RLON)
      SLON = DSIN(RLON)
C          COMPUTE GEOCENTRIC RADIUS
      D1 = RP*RP*CLAT*CLAT
      RB2 = RB*RB
      RA2 = RA*RA
      CLN2 = CLON*CLON
      SLN2 = SLON*SLON
      SLT2 = SLAT*SLAT
      R = (RA*RB*RP)/DSQRT(D1*RB2*CLN2+D1*RA2*SLN2+RA2*RB2*SLT2)
      CP(1) = R*CLAT*CLON - RS(1)
      CP(2) = -R*CLAT*SLON - RS(2)
      CP(3) = R*SLAT - RS(3)
C
C     ....Back of planet test (for sphere!)
c      D1 = 0.
c      D2 = 0.
c      DO I=1,3
c         D1 = D1 + CP(I)**2
c         D2 = D2 + RS(I)**2
c      ENDDO
c      IBOP = 0
c      IF (D1+R**2.GT.D2) THEN		!Point behind planet
c	IF (RADIUS.GE.0.) RETURN
c	IBOP = 1
c      ENDIF

c  BOP test for triaxial ellipsoid, using NAIF routines SURFNM & VSEP
      op(1) = r*clat*clon 
      op(2) = -r*clat*slon 
      op(3) = r*slat 
      call surfnm( ra, rb, rp, op, xnorm)
	! note CP is -1 * vector from P to C, so reverse criterion:
      ibop = 0
      if ( vsep(cp,xnorm) .lt. halfpi()) then
	if (radius.ge.0.) return
	ibop = 1
      end if

      DO 20 I=1,3
      D1 = 0.D0
      DO 10 J=1,3
   10 D1 = D1 + OM(I,J)*CP(J)
   20 CPP(I) = D1
C
      S = FL*SCALE/CPP(3)
      LINE = OAL + S*CPP(2)
      SAMP = OAS + S*CPP(1)
      IND = 1-IBOP
      RETURN
C
C     ....Here to convert (line,samp) to lat,lon)
   30 RADIUS = 0.0
      SRANGE = 0.0
      X = SAMP - OAS
      Y = LINE - OAL
      Z = FL*SCALE
C
      DO 40 I=1,3
   40 CP(I) = OM(1,I)*X + OM(2,I)*Y + OM(3,I)*Z
C
      A = E2*CP(1)*CP(1) + E1*CP(2)*CP(2) + E1*E2*CP(3)*CP(3)
      B = E2*CP(1)*RS(1) + E1*CP(2)*RS(2) + E1*E2*CP(3)*RS(3)
      C = E2*RS(1)*RS(1) + E1*RS(2)*RS(2) + E1*E2*RS(3)*RS(3)
     &   - E1*E2*RP*RP
      D = B*B - A*C
      IF (D.GE.0.) GO TO 50
C
C     ... point is off planet.  Just to be nice, we will proceed assuming
C	  that the tangent radius along this line of sight is the correct 
C	  radius.  (This allows off-limb points to be plotted, for 
C	  Orthographic & Perspective projections.)
      D = 0
      GO TO 60
C
   50 IND = 1
   60 S = (-B-DSQRT(D))/A
C
C	OP is the vector from planet center to surface intercept
C	S*CP is the vector from s/c to surface intercept
      RADIUS = 0.
      SRANGE = 0.
      DO I=1,3
	OP(I) = S*CP(I) + RS(I)
	RADIUS = RADIUS + OP(I)*OP(I)
	SRANGE = SRANGE + CP(I)*CP(I)
      ENDDO
      RADIUS = SQRT(RADIUS)
!     SRANGE = S*SQRT(SRANGE)
      SRANGE = abs(S)*SQRT(SRANGE)	! fixed 18mar05 - lwk
C
      X = OP(1)
      Y = OP(2)
      Z = OP(3)
      X1 = DABS(X)
      Y1 = DABS(Y)
      Z1 = DABS(Z)
      D = DSQRT(X*X+Y*Y)
      IF(D.LT.Z1*EPS) GOTO 98
      IF (ILAT.EQ.0) THEN
	LAT = DATAN(Z/D)*RADDEG		! GEOCENTRIC LAT.
      ELSE
	LAT = DATAN(E1*Z/D)*RADDEG	! GEODETIC LAT.
      ENDIF
      IF(Y1.LT.X1*EPS) GOTO 96
      IF(X1.LT.Y1*EPS) GOTO 94
      LON = 360. - DATAN2(Y,X)*RADDEG
      IF(LON.GT.360.) LON=LON-360.
      RETURN
C
   94 LON = 270.
      IF(Y.LT.0.) LON=90.
      RETURN
   96 LON = 0.
      IF(X.LT.0.) LON=180.
      RETURN
C
   98 LAT = 90.
      IF(Z.LT.0.) LAT=-LAT
      LON = 0.
      RETURN
      END


	SUBROUTINE NIMSBOOM(CONE,CLOCK,IOBS,FILNAM)
C_TITLE	NIMSBOOM - Find boom obscuration for a given cone and clock angle
C
C_ARGS	Type      Variable I/O	Description
	REAL	  CONE     !I   Cone angle (degrees)
	REAL 	  CLOCK    !I   Clock angle (degrees)
	INTEGER*4 IOBS	   !O   Obscuration flag 
C				0=no obscuration
C				1=image is obscured
C
C_DESC	Read the boom obscuration chart the first time the subroutine
C	is called.  Use the input cone and clock angle to find the
C	proper line and sample of the chart and return the obscuration
C	flag value.
C
C_FILE	ISIS$NIMSDATA:BOOM_OBSCURATION.NIM
C
C_HIST	Dec  4 1990  Kay Edwards U.S.G.S. Flagstaff Original Version
c	   --lwk-- changed filename to argument, used LUN=1
c  11dec93 --lwk-- added error exit to OPEN statement
c  12nov96 --lwk-- used NAIF GETLUN call instead of hard-coded unit #
c  08dec96 --lwk-- always open file on 1st call, so that the GETLUN can
c		be done at start of job
c  10aug02 --lwk-- removed READONLY param from OPEN statement for Linux
c  18mar05 --lwk-- fixed bug in computation of SRANGE (probably not too
c		significant, only makes a difference in pathological cases)
c  10jan11 --lwk-- changed NAME to FILE in OPEN call for picky new linux compiler
C
C_END
C
	LOGICAL*1 BOOM(720,170)
	CHARACTER*132 LABEL
	character*80 filnam
	integer iflag/0/
	save iflag

C	Read obscuration chart
	IF(IFLAG.EQ.0) THEN
	  IFLAG=1
	  J=1
	  CALL GETLUN(LUN)
c	  OPEN( LUN, NAME='NIMS$BOOM_OBSCUR', TYPE='OLD', READONLY)
	  OPEN( UNIT=LUN, FILE=filnam, STATUS='OLD', iostat=ios)
	  if (ios.ne.0) call mabend(
     1     '** UNABLE TO OPEN BOOM OBSCURATION FILE **')
   10	  READ(LUN,20,END=90) LABEL
   20	  FORMAT(A132)
	  IF(LABEL(4:7).NE.'CONE') GO TO 10
	  DO I=1,170
	    READ(LUN,30) (BOOM(K,I),K=J,J+89)
   30	    FORMAT(10X,90I1)
	  ENDDO
	  J=J+90
	  IF(J.LT.720) GO TO 10
   90	  CLOSE(LUN)
	ENDIF

C	The image data is totally obscured below 30 degrees cone angle
	IF(CONE.LE.30.) THEN
	  IOBS=1
	  RETURN
	ENDIF

C	No data is obscured above 115 degres cona angle
	IF(CONE.GE.115.) THEN
	  IOBS=0
	  RETURN
	ENDIF

C	Resolution of chart is .5 degrees
C	Top edge cone in chart is 30. degrees
C	Center of first pixel in chart is 30.25 cone, .25 clock
	ICONE=(CONE-30)*2+1
	IF(CLOCK.LT.0.) CLOCK=CLOCK+360.
	IF(CLOCK.GT.360.) CLOCK=CLOCK-360.
	ICLOCK=CLOCK*2+1
	IOBS=BOOM(ICLOCK,ICONE)
	RETURN
	END


	SUBROUTINE xNIMSBOOM( CONE, CLOCK, IOBS, bFILNAM, i)
c  this is part of the "C-bridge" nonsense needed for porting code ...
c  it is called by zNIMSBOOM.c (in-line to NIMSCMM.C)
	real cone, clock
	byte bfilnam(1)
	character*80 filnam

	if (i.gt.80) call xvmessage('** NIMS Boom filename too long')
	filnam = ' '
	call mvlc( bfilnam, filnam, i)
	call nimsboom( cone, clock, iobs, filnam)
	return
	end


c***********************************************************************
C$Procedure      ROTADKx ( Rotate alpha, delta, kappa )

C  INCLUDED IN-LINE IN NIMSCMM(2) BECAUSE ROTADK IS NOT ON UNIX SPICE
C  REMOVE THIS MODULE WHEN ROTADK IS PORTED.

      SUBROUTINE ROTADKx ( INA, INREF, OUTREF, OUTA )

C$ Abstract
C
C     Rotate alpha, delta, and kappa angles from one inertial 
C     reference frame to another.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     COORDINATES
C     FRAME
C     ROTATION
C     VECTOR
C
C$ Declarations

      DOUBLE PRECISION      INA     ( * )
      INTEGER               INREF
      INTEGER               OUTREF
      DOUBLE PRECISION      OUTA    ( * )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INA        I   Input alpha, delta, kappa.
C     INREF      I   Input inertial reference frame.
C     OUTREF     I   Desired output inertial reference frame.
C     OUTA       O   Output alpha, delta, kappa.
C
C$ Detailed_Input
C
C     INA        is the array containing the input angles
C                alpha, delta, and kappa. The angles are defined in such
C                a way that the transformation (C-matrix) from the 
C                input inertial reference frame to the local reference 
C                frame determined by the angles is given by
C                
C                   C = [kappa] [pi/2 - delta] [pi/2 + alpha]
C                              3              1              3
C                              
C                a 3-1-3 transformation.
C                
C                INC( 1 ) = alpha
C                INC( 2 ) = delta
C                INC( 3 ) = kappa
C                
C                Angles are measured in radians, and are assumed to be 
C                in the reference frame indicated by INREF.
C
C     INREF      is the index of the standard inertial frame that the
C                input angles are referenced to. The relationship
C                a reference frame and its index is defined in the 
C                routine CHGIRF.
C
C                You may use IRFNAM (an entry point of CHGIRF) to find
C                the character name of a given inertial reference
C                frame's index, and IRFNUM to go from a name to the
C                standard index number.
C
C     OUTREF     is the index of the desired output inertial reference.
C
C$ Detailed_Output
C
C      OUTA       are the angles alpha, delta, and kappa, rotated to 
C                 the output inertial reference frame.
C                 
C                 OUTC(1) = alpha
C                 OUTC(2) = delta
C                 OUTC(1) = kappa
C
C      OUTDEC     is the output declination, the angle from the XY
C                 plane to the point.  OUTDEC ranges from - ( pi/2 )
C                 to ( pi/2 ) radians.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The rotation is performed by
C
C     1) Constructing a C-matrix from the input angles. This will be a
C        matrix which rotates vectors from the input reference frame to
C        the local ("camera-fixed") frame. Call the matrix C(i,c).
C        
C                                  o         o
C                  C(i,c) = [K] [90  - D] [90  + A]
C                              3         1         3
C     
C     2) Constructing a matrix which rotates vectors from the output 
C        reference frame to the input reference frame. Call it T(o,i)
C        
C     3) Performing the matrix multiplication
C     
C             C(i,c)T(o,i)
C             
C        to obtain the C-matrix C(o,c), which rotates vectors from the 
C        output frame to "camera-fixed" coordinates.
C        
C     4) Decomposing C(o,c) into the rotated components alpha, delta, 
C        kappa.
C        
C$ Examples
C
C     The following example gets pointing (alpha, delta, kappa) from a
C     selected segment in a C-kernel pointing file, in its stored
C     reference frame, and then rotates the result to both the B1950
C     and J2000 inertial reference frames.
C
C     C
C     C     Select the appropriate file and segment for the input
C     C     spacecraft/instrument pair and time.
C     C
C           CALL CKSFS ( SCINST, TIMEIN, TIMTYP,
C          .             HANDLE, DESCR,  IDENT, FOUND )
C     C
C     C     Get the pointing without rotating the result.
C     C
C           CALL CKPFS ( HANDLE, DESCR, TIMEIN, TIMTYP, PNTNG, TIMOUT )
C     C
C     C     The reference frame is stored in the integer component of
C     C     the descriptor.
C     C
C           CALL CKUSD ( DESCR, DPDES, INTDES )
C           REF = INTDES( 2 )
C     C
C     C     Find the name of the reference frame.
C     C
C           CALL IRFNAM ( REF, REFNAM )
C           
C           WRITE (*,*) 
C           WRITE (6,*) 'Pointing: ', PNTNG
C           WRITE (6,*) 'Reference frame: ', REFNAM
C     C
C     C     Rotate to B1950, then J2000.  
C     C
C           CALL IRFNUM ( 'B1950', NEWREF )
C           CALL ROTADK ( PNTNG, REF, NEWREF, ROTPNT )
C           
C           WRITE (*,*) 
C           WRITE (6,*) 'Pointing in B1950 coordinates:'
C           WRITE (6,*) ROTPNT
C
C           CALL IRFNUM ( 'J2000', NEWREF )
C           CALL ROTRAD ( PNTNG, REF, NEWREF, ROTPNT )
C           
C           WRITE (*,*) 
C           WRITE (6,*) 'Pointing in J2000 coordinates:'
C           WRITE (6,*) ROTPNT
C
C
C$ Restrictions
C
C     None.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     R.E. Thurman (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 27-MAR-1989 (RET)
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI

C
C     Local variables
C
      DOUBLE PRECISION      C   ( 3, 3 )
      DOUBLE PRECISION      T   ( 3, 3 )


C%&END_DECLARATIONS


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ROTADK' )
      END IF

C     
C     Construct C(i,c), the transformation from the input frame to the
C     the "camera-fixed" frame.
C     
      CALL ROTATE (    HALFPI() + INA(1), 3, C )
      CALL ROTMAT ( C, HALFPI() - INA(2), 1, C )
      CALL ROTMAT ( C,            INA(3), 3, C )

C     
C     Construct T(o,i), the transformation from the output frame to
C     the input frame.
C     
      CALL IRFROT ( OUTREF, INREF, T )
      
C     
C     Construct C(o,c), the transformation from the output frame to 
C     "camera-fixed" frame by matrix multiplication.
C     
C         C(o,c) = C(i,c)*T(o,i)
C     
      CALL MXM ( C, T, C )

C     
C     Decompose C(o,c), a 3-1-3 rotation matrix, into the components 
C     alpha, delta, kappa.
C     
C     Any 3-1-3 rotation matrix
C     
C        A = [g]  [f]  [e]
C               3    2    3
C                      
C     can be decomposed as follows:
C        
C        tan (e) = A[3,1] / -A[3,2]
C           
C        cos (f) = A[3,3]
C           
C        tan (g) = A[1,3] / A[2,3]
C                      
C     In our case, we have 
C     
C        C(o,c) = [kappa]  [pi/2 - delta]  [pi/2 + alpha]
C                        3               1               3
C                        
C                        
C     The following identities are useful in relating the equations
C     above to the code below:
C        
C        1) cos (pi/2 - delta) = sin (delta)
C           
C        2) tan (pi/2 + alpha) = -1 / tan (alpha)
C           
      OUTA(1) = ATAN2 ( C(3,2),  C(3,1) )
      OUTA(2) = ASIN  ( C(3,3)          )
      OUTA(3) = ATAN2 ( C(2,3), -C(1,3) ) - HALFPI() 

      IF ( OUTA(1) .LT. 0.D0 )  OUTA(1) = OUTA(1) + TWOPI()
      
      CALL CHKOUT ( 'ROTADK' )
      RETURN
      END
