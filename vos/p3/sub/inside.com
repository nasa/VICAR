$!****************************************************************************
$!
$! Build proc for MIPL module inside
$! VPACK Version 1.9, Wednesday, April 02, 2003, 09:54:18
$!
$! Execute by entering:         $ @inside
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module inside ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to inside.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("inside.imake") .nes. ""
$   then
$      vimake inside
$      purge inside.bld
$   else
$      if F$SEARCH("inside.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake inside
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @inside.bld "STD"
$   else
$      @inside.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create inside.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack inside.com -mixed -
        -s inside.f zinside.c -
        -i inside.imake -
        -t tinside.f tzinside.c tinside.imake tinside.pdf tstinside.pdf -
        -o inside.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create inside.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zinside.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zinside - test if given point inside specified polygon*/
/************************************************************************/

int zinside( point, corner, n)
float point[2];
float *corner;
int n;                          /* number of corners in polygon */

{
return FTN_NAME(inside)( point, corner, &n);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create inside.imake
/* Imake file for VICAR FUNCTION INSIDE */

#define SUBROUTINE inside

#define MODULE_LIST inside.f zinside.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create tinside.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  PROGRAM TINSIDE
C  THIS IS A TEST PROGRAM FOR LOGICAL FUNCTION INSIDE
C  INSIDE IS USED TO DETERMINE WHETHER A SPECIFIED POINT
C  LIES WITHIN OR ON A SPECIFIED POLYGON.  THE VALUE TRUE
C  IS RETURNED IF THE POINT LIES INSIDE OR ON THE BOUNDARY
C  OF THE POLYGON.  OTHERWISE FALSE IS RETURNED.  THE ARRAY
C  TCORNERS CONTAINS THE X AND Y COORDINATES OF THE VERTICES
C  OF THE POLYGON.  N CONTAINS THE NUMBER OF VERTICES OF THE
C  POLYGON.  TPOINT CONTAINS X AND Y FOR THE POINT OF INTEREST.

       REAL*4 TPOINT(2)
       REAL*4 TCORNERS(2,25)
       INTEGER*4 N
       CALL XVMESSAGE('POLYGON WITH FOUR VERTICES (A SQUARE)',' ')
       N=4
       TCORNERS(1,1) = 0
       TCORNERS(2,1) = 0
       TCORNERS(1,2) = 3
       TCORNERS(2,2) = 0
       TCORNERS(1,3) = 3
       TCORNERS(2,3) = 3
       TCORNERS(1,4) = 0
       TCORNERS(2,4) = 3
       CALL XVMESSAGE(' ',' ')
       CALL PRNT(7,8,TCORNERS,' VERTICES OF THE POLYGON:.')
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING INSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 1
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING ON BOUNDARY OF POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 0
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING OUTSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = -1
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = -1
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 4
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = -1
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('POLYGON WITH THREE VERTICES (A TRIANGLE)',' ')
       CALL XVMESSAGE(' ',' ')
       N = 3
       TCORNERS(1,1) = 0
       TCORNERS(2,1) = 0
       TCORNERS(1,2) = 3
       TCORNERS(2,2) = 0
       TCORNERS(1,3) = 0
       TCORNERS(2,3) = 3
       CALL XVMESSAGE(' ',' ')
       CALL PRNT(7,6,TCORNERS,' VERTICES OF THE POLYGON:.')
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING INSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 1
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING ON BOUNDARY OF POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 0
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINT LYING OUTSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = -1
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = -1
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1
       TPOINT(2) = -1
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 1
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('POLYGON WITH TEN VERTICES',' ')
       CALL XVMESSAGE(' ',' ')
       N=10
       TCORNERS(1,1) = 0
       TCORNERS(2,1) = 2
       TCORNERS(1,2) = 4
       TCORNERS(2,2) = 4
       TCORNERS(1,3) = 5
       TCORNERS(2,3) = 0
       TCORNERS(1,4) = 6
       TCORNERS(2,4) = 4
       TCORNERS(1,5) = 8
       TCORNERS(2,5) = 6
       TCORNERS(1,6) = 5
       TCORNERS(2,6) = 4
       TCORNERS(1,7) = 5
       TCORNERS(2,7) = 7
       TCORNERS(1,8) = 4
       TCORNERS(2,8) = 5
       TCORNERS(1,9) = 2
       TCORNERS(2,9) = 6
       TCORNERS(1,10) = 3
       TCORNERS(2,10) = 5
       CALL XVMESSAGE(' ',' ')
       CALL PRNT(7,20,TCORNERS,' VERTICES OF THE POLYGON:.')
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING INSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 3
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 5
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING ON BOUNDARY OF POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 0
       TPOINT(2) = 2
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 4
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 5
       TPOINT(2) = 0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 6
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 8
       TPOINT(2) = 6
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 5
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 5
       TPOINT(2) = 7
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 4
       TPOINT(2) = 5
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 6
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 5
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 4
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 7
       TPOINT(2) = 5
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 5
       TPOINT(2) = 6
       CALL LOCATE(TPOINT,TCORNERS,N)
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING OUTSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 2
       TPOINT(2) = 5
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3
       TPOINT(2) = 3
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 4
       TPOINT(2) = 6
       CALL LOCATE(TPOINT,TCORNERS,N)

C
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('BIGGER TRIANGLE',' ')
       N=3

       TCORNERS(1,1) = 1.
       TCORNERS(2,1) = 1.

       TCORNERS(1,2) = 100.
       TCORNERS(2,2) = 100.

       TCORNERS(1,3) = 1.
       TCORNERS(2,3) = 200.

       CALL XVMESSAGE(' ',' ')
       CALL PRNT(7,8,TCORNERS,' VERTICES OF THE POLYGON:.')
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING INSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 1.5
       TPOINT(2) = 10.0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 10.0
       TPOINT(2) = 100.0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 50.0
       TPOINT(2) = 100.0
       CALL LOCATE(TPOINT,TCORNERS,N)
C
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING ON BOUNDARY OF POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 1.0
       TPOINT(2) = 200.0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1.0
       TPOINT(2) = 100.0
       CALL LOCATE(TPOINT,TCORNERS,N)
C
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING OUTSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 1.5
       TPOINT(2) = 1.0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0.0
       TPOINT(2) = 100.0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1.000001
       TPOINT(2) = 200.0
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 101.
       TPOINT(2) = 100.0
       CALL LOCATE(TPOINT,TCORNERS,N)

C
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST SEGMENTS ON SAME LINE',' ')
       N=9

       TCORNERS(1,1) = 1.
       TCORNERS(2,1) = 1.
       TCORNERS(1,2) = 2.
       TCORNERS(2,2) = 1.
       TCORNERS(1,3) = 3.
       TCORNERS(2,3) = 1.

       TCORNERS(1,4) = 3.
       TCORNERS(2,4) = 2.
       TCORNERS(1,5) = 4.
       TCORNERS(2,5) = 2.
       TCORNERS(1,6) = 5.
       TCORNERS(2,6) = 2.

       TCORNERS(1,7) = 5.
       TCORNERS(2,7) = 3.
       TCORNERS(1,8) = 1.
       TCORNERS(2,8) = 3.
       TCORNERS(1,9) = 1.
       TCORNERS(2,9) = 2.

       CALL XVMESSAGE(' ',' ')
       CALL PRNT(7,9,TCORNERS,' VERTICES OF THE POLYGON:.')
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING INSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 2.
       TPOINT(2) = 2.
       CALL LOCATE(TPOINT,TCORNERS,N)
C
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING ON BOUNDARY OF POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 1.
       TPOINT(2) = 1.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 2.
       TPOINT(2) = 1.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3.
       TPOINT(2) = 1.
       CALL LOCATE(TPOINT,TCORNERS,N)

       TPOINT(1) = 3.
       TPOINT(2) = 2.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 4.
       TPOINT(2) = 2.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 5.
       TPOINT(2) = 2.
       CALL LOCATE(TPOINT,TCORNERS,N)
C
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE('TEST POINTS LYING OUTSIDE THE POLYGON',' ')
       CALL XVMESSAGE(' ',' ')
       TPOINT(1) = 0.
       TPOINT(2) = 1.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 4.
       TPOINT(2) = 1.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 0.
       TPOINT(2) = 2.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 6.
       TPOINT(2) = 2.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 1.
       TPOINT(2) = 0.
       CALL LOCATE(TPOINT,TCORNERS,N)
       TPOINT(1) = 3.
       TPOINT(2) = 0.
       CALL LOCATE(TPOINT,TCORNERS,N)

C
      CALL XVMESSAGE(
     . 'Repeat two cases in C to test C interface: zinside', ' ')

      call tzinside

       return
       END
C*******************************************************************************
       SUBROUTINE LOCATE(TPOINT,TCORNERS,N)
       REAL*4  TPOINT(2), TCORNERS(2,25)
       INTEGER*4 N
       LOGICAL  INSIDE, LOCPT
       CALL XVMESSAGE(' ',' ')
       CALL PRNT(7,2,TPOINT,' X AND Y COORDINATES FOR POINT:.')
       LOCPT = INSIDE(TPOINT,TCORNERS,N)
       IF (LOCPT) THEN
          CALL XVMESSAGE('     ANSWER: T',' ')
       ELSE
          CALL XVMESSAGE('     ANSWER: F',' ')
       END IF
       RETURN
       END
$!-----------------------------------------------------------------------------
$ create tzinside.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzinside)() 
{
        float point[2];
        float corner[25][2];
        int   n;
/*  ==================================================================  */
        n = 3;
        corner[0][0] = 0.0;
        corner[0][1] = 0.0;
        corner[1][0] = 3.0;
        corner[1][1] = 0.0;
        corner[2][0] = 0.0;
        corner[2][1] = 3.0;
        point[0]     = 2.0;
        point[1]     = 1.0;
        if ( zinside(  point, corner, n) )
           zvmessage("zinside got correct answer", "");
        else
           zvmessage("zinside got wrong answer", "");

        point[0]     = 2.0;
        point[1]     = 2.0;
        if ( zinside(  point, corner, n) )
           zvmessage("zinside got wrong answer", "");
        else
           zvmessage("zinside got correct answer", "");


}
$!-----------------------------------------------------------------------------
$ create tinside.imake
/* Imake file for Test of VICAR subroutine INSIDE */

#define PROGRAM tinside

#define MODULE_LIST tinside.f tzinside.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tinside.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstinside.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
! THIS IS A TEST OF THE LOGICAL FUNCTION INSIDE.
! INSIDE IS USED TO DETERMINE WHETHER A SPECIFIED POINT
! LIES WITHIN OR ON A SPECIFIED POLYGON.  THE VALUE TURE
! IS RETURNED IF THE POINT LIES INSIDE OR ON THE BOUNDARY
! OF THE POLYGON.  OTHERWISE FALSE IS RETURNED.  THE TEST
! USES A SIMPLE POLYGON WITH FOUR VERTICES (A SQUARE),
! A POLYGON WITH THREE VERTICES (A TRIANGLE), AND A POLYGON
! WITH TEN VERTICES.
tinside
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create inside.hlp
1  INSIDE

    To determine whether a specified point lies within 
    or on a specified polygon.

    INSIDE is a LOGICAL FUNCTION, not a subroutine.  It
    returns the value TRUE if the specified point lies inside or on the
    boundary of the polygon.  It returns FALSE otherwise.


  FORTRAN Calling Sequence:      IF  ( INSIDE(POINT,CORNER,N) ) ...
  C Calling Sequence:            if  ( zinside(point,corner,n) ) ...


2  ARGUMENTS

     POINT     is a REAL*4 array (2 elements) containing x and y for
               the point of interest.

     CORNER    is a REAL*4 array, dimensioned (2,N)
               (or (2,M) where M is some number >= N.
               CORNER(1,I) is the x coordinate of the I-th vertex of
               a polygon.
               CORNER(2,I) is the y coordinate.
               For zinside the declaration should be
               float [n][2], or float [M][2] where M is some number >= n.
             

     N         is an INTEGER*4 variable or constant containing the
               number of vertices of the polygon.
               (n is passed by value for zinside.)
2  HISTORY

     Original Programmers: John Kreznar & Denis A. Elliott, about 1977
     Current Cognizant Programmer: Gary Yagi
     Source Language: Fortran

 Revision History
  92-12-29 ...SP.... Made portable.  Added zinside for calls from C.
  03-03-21 ...REA... Rewritten to fix bug and remove restriction of
                     25 vertices.
  03-03-30 ...GMY... Implemented Brian Carcich algorithm

2  OPERATION

     INSIDE generates a semi-infinite ray (parallel to the X axis) from 
     the point of interest toward infinity in the +x direction.  The point is
     inside if this ray intersects an odd number of edges of the polygon, 
     and is outside if it intersects an even number.

     A special case occurs when the ray passes through a vertex.  The original
     code treated this by adding a small offset to the given point to make it
     "improbable" that this case would occur.  The current code treats this
     case correctly and is based on an algorithm submitted by Brian Carcich of
     Cornell University.
$ Return
$!#############################################################################
