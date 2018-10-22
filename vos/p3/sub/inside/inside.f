CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Logical function INSIDE: Returns TRUE if point is inside or on boundary of a
C    polygon with N corners.  Returns FALSE otherwise.
C
C The method is to trace a ray from the POINT in the positive x direction
C (ie, a horizontal ray to the right), and count the number of intersections
C with the edge.  If the point is inside the polygon, it will have an odd
C number of intersections; if it is outside, the number of intersections
C will be even.  Special handling is necessary for the case where the ray
C touches a vertex, but does not cross the polygon boundary.  This does
C not count as an intersection.
C
      LOGICAL FUNCTION INSIDE(POINT,CORNER,N)
      IMPLICIT NONE
      INTEGER N         !Number of vertices in the polygon
      REAL POINT(2)     ! (x,y) coordinates of the given point
      REAL CORNER(2,N)  ! (x,y) coordinates of the N vertices of the polygon

C POINT and CORNER may be given as (x,y), (LINE,SAMPLE), or (SAMPLE,LINE)
C as long the usage is consistent between POINT and CORNER.

      INTEGER I,I2,NUM
      REAL X,Y,X0,X1,X2,Y1,Y2
      REAL XMIN,YMIN,XMAX,YMAX

      INSIDE = .TRUE.
      NUM = 0
      X = POINT(1)
      Y = POINT(2)

      DO 50 I=1,N               !Loop through each edge of polygon
      I2 = MOD(I,N) + 1
      X1 = CORNER(1,I)          !First vertex is (X1,Y1)
      Y1 = CORNER(2,I)
      X2 = CORNER(1,I2)         !Second vertex is (X2,Y2)
      Y2 = CORNER(2,I2)

C     ...Eliminate obvious cases where ray would never intersect edge
      YMIN = MIN(Y1,Y2)
      YMAX = MAX(Y1,Y2)
      IF (Y.LT.YMIN .OR. Y.GT.YMAX) GOTO 50
      XMIN = MIN(X1,X2)
      XMAX = MAX(X1,X2)
      IF (X.GT.XMAX) GOTO 50

C     ...Horizontal edges are ignored unless the point is on it
      IF (Y1 .EQ. Y2) THEN
         IF (X.GE.XMIN) RETURN          !Point is on this edge
         GOTO 50
      ENDIF

C     ...Check if ray passes through a vertex
      IF (Y.EQ.YMIN) THEN               !Ignore YMAX case to avoid dble-counting
         X0 = X1                        !Find the X associated with YMIN
         IF (Y2.LT.Y1) X0=X2
         IF (X.LT.X0) THEN              !Ray passes through vertex
            NUM = NUM + 1
            GOTO 50
         ENDIF
      ENDIF

C     ...Normal case: Y is between Y1 and Y2
      X0 = (Y-Y2)*(X2-X1)/(Y2-Y1) + X2  !compute intersection
      IF (X.EQ.X0) RETURN               !Point is on this edge
      IF (Y.EQ.Y1 .OR. Y.EQ.Y2) GOTO 50 !Skip to avoid double-counting
      IF (X.LT.X0) NUM=NUM+1            !Ray intersects edge
   50 CONTINUE

      INSIDE = MOD(NUM,2) .NE. 0
      RETURN
      END
