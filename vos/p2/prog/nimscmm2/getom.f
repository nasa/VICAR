C$Procedure  GETOM( Get OM matrix )

      SUBROUTINE GETOM (  A,        B,        C,
     .                    OBSLAT,   OBSLON,   RANGE,
     .                    XPTLAT,   XPTLON,
     .                    V,        PRJANG,   OM,     VISIBL )

      IMPLICIT NONE

C$ Abstract
C
C     Find the OM matrix corresponding to a given observer location,
C     optical axis intercept point, and angular position of a
C     body-fixed vector, using an ellipsoidal target body model.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     GEOMETRY
C     MATRIX
C     TRANSFORMATION
C
C$ Declarations

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      OBSLAT
      DOUBLE PRECISION      OBSLON
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      XPTLAT
      DOUBLE PRECISION      XPTLON
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      PRJANG
      DOUBLE PRECISION      OM     ( 3, 3 )
      LOGICAL               VISIBL

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Larger equatorial radius of target body (km).
C     B          I   Smaller equatorial radius of target body (km).
C     C          I   Polar radius of target body (km).
C     OBSLAT     I   Observer's planetocentric latitude (rad).
C     OBSLON     I   Observer's planetocentric longitude (rad).
C     RANGE      I   Observer's range to target body center (km).
C     XPTLAT     I   Optical axis intercept planetocentric lat. (rad).
C     XPTLAN     I   Optical axis intercept planetocentric lon. (rad).
C     V          I   A vector in body-fixed coordinates.
C     PRJANG     I   Angular position of proj of V on focal plane (rad).
C     OM         O   OM matrix.
C     VISIBL     O   Flag indicating whether intercept is visible.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C              are, respectively, the larger equatorial, smaller
C                    equatorial, and polar radii of the target body.
C
C     OBSLAT,
C     OBSLON         are, respectively, the planetocentric latitude
C                    and longitude of a sub-observer point, given in
C                    radians.
C
C     RANGE          is the distance between the observer and the
C                    target body center, in units of km.
C
C     XPTLAT,
C     XPTLON         are, respectively, the planetocentric latitude
C                    and longitude of the surface intercept point of
C                    the optical axis on the target body, given in
C                    radians.
C
C     V,
C     PRJANG         are, respectively, a vector specified in body-fixed
C                    coordinates and the angular position of the
C                    vector's projection P on the M-N plane of the
C                    instrument.  If P is specified in polar coordinates
C                    as (r, theta), then PRJANG is theta.  Units are
C                    radians.
C
C$ Detailed_Output
C
C     OM             is a coordinate transformation matrix that
C                    converts vectors from camera coordinates to
C                    body-fixed coordinates by left multiplication:
C
C                       OM * V         =   V
C                             camera        body-fixed
C
C                    for any vector
C
C                       V
C                        camera
C
C                    specified relative to the camera frame.
C
C
C     VISIBL         is a logical flag indicating whether the surface
C                    location defined by XPTLAT, XPTLON, A, B, and C
C                    is visible from the observer location specified by
C                    OBSLAT, OBSLON, and RANGE.  The OM matrix is
C                    calculated whether or not the surface location is
C                    visible.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If any of the input body radii are non-positive, the
C         error SPICE(NONPOSITIVEAXES) is signalled.
C
C     2)  If the observer location is inside the target body, the
C         error SPICE(INVALIDRANGE) is signalled.
C
C     3)  If the projection of the input vector V on the instrument
C         M-N plane is the zero vector, the error SPICE(DEGENERATECASE)
C         is signalled.
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     1)  Find the OM matrix when the "North position angle" is known.
C
C         In this case, the vector V is the body-fixed North direction
C         vector (0,0,1).
C
C         The angle PRJANG is related to the North position angle as
C         shown in the picture below:
C
C
C                    ^
C                -N  |
C                    |.
C   line 1  +------------.----+
C           |             .North position angle
C           |             .   |
C           |   +L into page  |
C           |        +----.----> + M
C           |        |\  .   .|
C           |        | \*   .PRJANG
C           |        |  \ *   |
C  line 800 +--------|---\----+
C                    v
C                    +N
C
C     Let NPA represent the North position angle, in units of radians.
C     Then
C
C        PRJANG  =  NPA - pi/2
C
C     Presuming that we know the other inputs to GETOM_G, the call
C
C        CALL GETOM_G (  A,        B,             C,
C       .                OBSLAT,   OBSLON,        RANGE,
C       .                XPTLAT,   XPTLON,
C                        V,        NPA-HALFPI(),  OM,    VISIBL  )
C
C     will produce the desired OM matrix.
C
C$ Restrictions
C
C     For review by Lucas Kamp.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 3.0.0, 20-APR-1993 (NJB)
C
C-&

C
C$ Index_Entries
C
C     get OM matrix
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local variables
C
      DOUBLE PRECISION      UVEC   ( 3 )

      DOUBLE PRECISION      LVEC   ( 3 )
      DOUBLE PRECISION      MVEC   ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      NVEC   ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OXVEC  ( 3 )
      DOUBLE PRECISION      VPRJ   ( 3 )
      DOUBLE PRECISION      XPOS   ( 3 )

      INTEGER               I

      LOGICAL               FOUND

C
C     Saved variables
C
      DOUBLE PRECISION      ORIGIN ( 3 )
      SAVE                  ORIGIN
C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETOM_G' )
      END IF

C
C     Body radii must be positive.
C
      IF (      ( A .LE. 0.D0 )
     .     .OR. ( B .LE. 0.D0 )
     .     .OR. ( C .LE. 0.D0 ) ) THEN

         CALL SETMSG ( 'A = #,  B = #,  C = #'  )
         CALL ERRDP  ( '#',  A                  )
         CALL ERRDP  ( '#',  B                  )
         CALL ERRDP  ( '#',  C                  )
         CALL SIGERR ( 'SPICE(NONPOSITIVEAXES)' )
         CALL CHKOUT ( 'GETOM_G'                )
         RETURN

      END IF

C
C     Observer's position in rectangular body-fixed coordinates:
C
      CALL LATREC ( RANGE, OBSLON, OBSLAT, OBSPOS )

C
C     Make sure the observer not inside the target body.
C
      IF (      ( OBSPOS(1)**2  /  A**2 )
     .       +  ( OBSPOS(2)**2  /  B**2 )
     .       +  ( OBSPOS(3)**2  /  C**2 )   .LT.  1  ) THEN

         CALL SETMSG ( 'observer is inside target body; range = #'  )
         CALL ERRDP  ( '#',  RANGE                                  )
         CALL SIGERR ( 'SPICE(INVALIDRANGE)'                        )
         CALL CHKOUT ( 'GETOM_G'                                    )
         RETURN

      END IF

C
C     Ok, the inputs should be valid, with the possible exception of
C     V, which we'll come back to later.  We'll now find the vector
C     from the camera to the optical axis intercept, using body-fixed
C     rectangular coordinates.
C
C
C     Optical axis intercept position in rectangular body-fixed
C     coordinates:   find the surface intercept of the unit vector
C     centered at the origin that points in the direction defined
C     by XPTLON, XPTLAT.
C
      CALL LATREC ( 1.D0,   XPTLON,  XPTLAT,    UVEC )

      CALL SURFPT ( ORIGIN, UVEC,    A, B, C,   XPOS,   FOUND )
C
C     There's no way a ray emanating from the origin can fail to
C     intersect the surface, so we won't check FOUND.
C
C
C     Observer-intercept vector in body-fixed coordinates:
C
      CALL VSUB ( XPOS, OBSPOS, OXVEC )

C
C     The L-axis in body-fixed coordinates:
C
      CALL VHAT ( OXVEC, LVEC )

C
C     The next step is to determine the M-axis in body-fixed
C     coordinates.  Since the projection of V onto the M-N plane
C     is parallel to the vector obtained by rotating M by PRJANG
C     radians about the L axis, rotating the M-N projection of V
C     by -PRJANG radians about the L axis, and then unitizing the
C     result, will give us the M vector.
C
C     Find the component of V perpendicular to L---this is the
C     projection of V onto the M-N plane:
C
      CALL VPERP ( V, LVEC, VPRJ )

C
C     One last check:  VPRJ must not be the zero vector:
C
      IF ( VZERO(VPRJ) ) THEN

         CALL SETMSG ( 'V is parallel to the optical axis; not '     //
     .                 'enough informatation available to define '   //
     .                 'OM matrix.'                                   )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                        )
         CALL CHKOUT ( 'GETOM_G'                                      )
         RETURN

      END IF

C
C     Rotate VPRJ about the L-axis and unitize the result:
C
      CALL VROTV ( VPRJ, LVEC, -PRJANG, MVEC )
      CALL VHAT  ( MVEC,                MVEC )

C
C     Find the N vector:
C
      CALL VCRSS ( LVEC, MVEC, NVEC )

C
C     Form the OM matrix (the transpose of the body-fixed-to-camera
C     transformation matrix):
C
      DO I = 1, 3
         OM(I,1)  =  MVEC(I)
         OM(I,2)  =  NVEC(I)
         OM(I,3)  =  LVEC(I)
      END DO
C
C     Decide whether the intercept point is visible from the observer's
C     location.   The point is visible if and only if the observer is
C     visible from the surface point, which happens precisely when the
C     angular separation of the surface point--observer vector and the
C     outward normal at the surface point is less than pi/2 radians.
C     Equivalently, the angular separation of the outward normal and the
C     L-vector must be no less than pi/2 radians.
C
      CALL SURFNM ( A, B, C, XPOS, NORMAL )

      IF (  VSEP( LVEC, NORMAL )  .GE.  HALFPI()  ) THEN
          VISIBL = .TRUE.
      ELSE
          VISIBL = .FALSE.
      END IF

      CALL CHKOUT ( 'GETOM_G' )
      RETURN
      END

