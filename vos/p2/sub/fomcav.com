$!****************************************************************************
$!
$! Build proc for MIPL module fomcav
$! VPACK Version 1.9, Monday, December 07, 2009, 16:18:18
$!
$! Execute by entering:		$ @fomcav
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
$ write sys$output "*** module fomcav ***"
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
$ write sys$output "Invalid argument given to fomcav.com file -- ", primary
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
$   if F$SEARCH("fomcav.imake") .nes. ""
$   then
$      vimake fomcav
$      purge fomcav.bld
$   else
$      if F$SEARCH("fomcav.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fomcav
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fomcav.bld "STD"
$   else
$      @fomcav.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fomcav.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fomcav.com -mixed -
	-s fomcav.f -
	-i fomcav.imake -
	-t tfomcav.f tfomcav.imake tfomcav.pdf tstfomcav.pdf -
	-o fomcav.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fomcav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fomcav.imake
/* Imake file for VICAR subroutine  FOMCAV  */

#define SUBROUTINE  fomcav

#define MODULE_LIST  fomcav.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tfomcav.f
      Include  'VICMAIN_FOR'
c
      Subroutine Main44
c
C  This is a TEST Program for SUBROUTINES FOMCLV AND FOMCAV.
C  FOMCAV COMPUTES THE OMMATRIX, THE PLANET TO CAMERA TRANSFORMATION
C  MATRIX, AND THE RSVECTOR, THE PLANET TO CAMERA POSITION VECTOR
C  EXPRESSED IN THE PLANET COORDINATE SYSTEM.  FOMCAV IS ACCESSED
C  THROUGH FOMCLV, WHICH MERELY SETS UP THE RETICLE DESCRIPTIVE
C  ARRAY FROM THE INPUT ARRAYS AA AND B, AND THEN CALLS FOMCAV.
C  THE CONSTANTS FOR THE PLANET AND SPACECRAFT DESCRIPTIVE ARRAY,
C  B, REFER TO THE JUPITER MOON, IO.  
c
        REAL*4  AA(4,20)
c       , A(5,20), 
        Real*4  B(6), CL, CS
        REAL*8  OM(3,3), RS(3)
        INTEGER*4  IND, NPOINT
c 
C  PLANET AND SPACECRAFT DESCRIPTIVE ARRAY
c
        B(1) = 1500.19 * 84.821
        B(2) = 0.
	B(3) = 806061.
	B(4) = -.02
	B(5) = 155.07
	B(6) = 1815. 
c 
C  LINE/SAMPLE OF CAMERA AXIS
c       	
	CL = 500.
	CS = 500.
c
C  IMAGE POINTS (LINE, SAMPLE, LATITUTE, LONGITUDE)
c
	AA(1,1) = 381.86
        AA(2,1) = 382.64
	AA(3,1) = 19.35
	AA(4,1) = 229.21 
c
	AA(1,2) = 382.17
	AA(2,2) = 498.94
	AA(3,2) = 25.83
	AA(4,2) = 190.00
c 
	AA(1,3) = 381.98
	AA(2,3) = 615.31
	AA(3,3) = 32.86
	AA(4,3) = 163.58
c 
        NPOINT = 3
c  
       Call Xvmessage(' TEST USING THREE IMAGE POINTS', ' ')
       Call XVmessage('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',
     &  ' ')
       Call Prnt (7, 12, AA, '.')
       Call Xvmessage('  ',' ')

	Call  FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	Call  Prnt(8, 9, OM,' OMMATRIX = .')
	Call  Prnt(8, 3, RS,' RSVECTOR = .')
        Call  Prnt(4,1,IND,' IND =.')
        CALL  Xvmessage('  ',' ')
	CALL  XVmessage(' ********************',' ')

        CALL  Xvmessage('  ',' ')
        AA(1,4) = 498.40
  	AA(2,4) = 498.49
	AA(3,4) = 2.539
	AA(4,4) = 179.32

        AA(1,5) = 498.53
        AA(2,5) = 615.11
        AA(3,5) = 8.65
        AA(4,5) = 156.13

        AA(1,6) = 497.52
      	AA(2,6) = 732.24
    	AA(3,6) = 15.14
	AA(4,6) = 132.25

  	NPOINT = 6
   
       Call Xvmessage('  ',' ')
       Call Xvmessage(' TEST USING SIX IMAGE POINTS',' ')
       Call Xvmessage('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',
     &                ' ')
       Call Prnt(7, 24, AA, '.')
       Call Xvmessage('  ', ' ')

       Call  FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
       Call  Prnt(8, 9, OM,' OMMATRIX = .')
       Call  Prnt(8, 3, RS,' RSVECTOR = .')
       Call  Prnt(4, 1, IND,' IND =.')
       Call  Xvmessage('  ', ' ')
  
  	AA(1,7) = 613.50
    	AA(2,7) = 380.84
	AA(3,7) = -26.74
	AA(4,7) = 206.78

 	AA(1,8) = 613.95
	AA(2,8) = 497.92
	AA(3,8) = -20.18
	AA(4,8) = 174.26

	AA(1,9) = 614.02
	AA(2,9) = 614.04
	AA(3,9) = -13.80
	AA(4,9) = 150.12

	AA(1,10) = 613.13
	AA(2,10) = 731.88
	AA(3,10) = -7.33
	AA(4,10) = 126.19
	
	AA(1,11) = 729.89
	AA(2,11) = 496.11
	AA(3,11) = -47.37
	AA(4,11) = 172.84
	
	AA(1,12) = 729.79
	AA(2,12) = 613.04
	AA(3,12) = -38.93
	AA(4,12) = 141.21

	AA(1,13) = 728.78
	AA(2,13) = 730.46
	AA(3,13) = -31.16
	AA(4,13) = 112.36

  	NPOINT = 13
   
	Call Xvmessage(' ********************',' ')
        Call Xvmessage('  ',' ')
        Call Xvmessage(' TEST USING THIRTEEN IMAGE POINTS',' ')
	Call Xvmessage('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',
     1                ' ')
        Call Prnt(7, 52, AA, '.')
        Call Xvmessage('  ', ' ')

	Call FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	Call Prnt(8, 9, OM,' OMMATRIX = .')
	Call Prnt(8, 3, RS,' RSVECTOR = .')
        Call Prnt(4, 1, IND,' IND = .')
        Call Xvmessage('  ',' ')
	Call Xvmessage(' ********************',' ')
        Call Xvmessage('  ',' ')

	Call Xvmessage(' TEST ERROR RETURN (IND = 3):',' ')
        Call Xvmessage('  ',' ')


	AA(1,1) = 346
	AA(2,1) = 432
	AA(3,1) = 45
	AA(4,1) = 63

	AA(1,2) = 479
	AA(2,2) = 316
	AA(3,2) = 120
	AA(4,2) = 90
	
	AA(1,3) = 723
	AA(2,3) = 529
	AA(3,3) = 80
	AA(4,3) = 62
 
	Call Xvmessage('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',
     1                 ' ')
        Call Prnt(7, 12, AA, '.')
        Call Xvmessage('  ',' ')

	NPOINT = 3

	Call FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	Call Prnt(8, 9, OM,' OMMATRIX =.')
	Call Prnt(8, 3, RS,' RSVECTOR =.')
	Call Prnt(4, 1, IND,' IND = .')

	Call Xvmessage(' ********************',' ')
        Call Xvmessage('  ',' ')
c        Call ZIA(AA,100)
        Call Zia(AA, 80)
	Call Xvmessage('   SETS OF LINE, SAMPLE, LAT, LONG (ARRAY AA):',
     1                 ' ')
        Call Prnt(7, 12, AA, '.')
        Call Xvmessage('  ',' ')
	Call FOMCLV(IND,NPOINT,AA,B,OM,RS,CL,CS)
	Call Prnt(8, 9, OM, ' OMMATRIX = .')
	Call Prnt(8, 3, RS, ' RSVECTOR = .')
	Call Prnt(4, 1, IND,' IND = .')
c
 	Return
	End
$!-----------------------------------------------------------------------------
$ create tfomcav.imake
/* IMAKE file for Test of VICAR subroutine  FOMCAV  */

#define PROGRAM  tfomcav

#define MODULE_LIST tfomcav.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/*  #define   LIB_LOCAL  */   /*  Disable during delivery   */
#define   LIB_P2SUB         
#define   LIB_MATH77  
$!-----------------------------------------------------------------------------
$ create tfomcav.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstfomcav.pdf
Procedure
Refgbl $Echo
Body
Let  _Onfail="Continue"
Let  $Echo="Yes"
TFOMCAV
Let  $Echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create fomcav.hlp
1 FOMCAV

  Calculates the planet to camera transformation matrix.

  Calling Sequence:  FOMCAV(IND,NPOINT,A,B,OM,RS,CL,CS)

  Arguments:

	IND (output)	Return indicator
	NPOINT (input)	Number of points
	A (input)	Reticle descriptive array
	B (input)	Planet and spacecraft descriptive array
	OM (output)	Planet to camera transformation matrix
	RS (output)	Planet to cmmera position vector in planet system
	CL (input)	Line of camera axis
	CS (input)	Sample of camera axis

2 History

  Original Programmer: JJL
  Current Cognizant Programmer:   Helen De Rueda
  Source Language: Fortran

  Ported for UNIX Conversion:  W.P. Lee,  Jan-15-1993


2 Operation

       This routine is a modification of the routine CORCAL as it
     was in SUPERMAP.   Given a control net of three or more
     points  on a planetary image, the routine will calculate the
     planet to camera transformation matrix.
       This version allows more than three points to be used in the
     transformation calculation.  If more than 3 image points are
     specified, then a Linear Least Squares solution to the
     problem is obtained.  Redundancy in control point specification
     improves the transformation matrix soln.

2 Arguments

     IND (output)   RETURN INDICATOR
	              0 = NORMAL RETURN
	              1 = INITIALIZATION FAILURE
     NPOINT (input) NUMBER OF POINTS
     A (input)      RETICLE DESCRIPTIVE ARRAY
		      A(1,J) = LINE VALUE OF JTH RETICLE POINT
	              A(2,J) = SAMP VALUE OF JTH RETICLE POINT
	              A(3,J) = SLANT RANGE TO JTH RETICLE POINT
	              A(4,J) = LATITUDE OF JTH RETICLE POINT
	              A(5,J) = LONGITUDE OF JTH RETICLE POINT
                         , J=1,2,...,NPOINT-1,NPOINT
     B (input)      PLANET AND SPACECRAFT DESCRIPTIVE ARRAY
	              B(1) = CAMERA FOCAL LENGTH (IN PIXELS)
	              B(2) = POLAR FLATTENING (RADIUS AT EQ-RAD.AT POLE)
	              B(3) = DISTANCE PLANET (CENTER ) TO SPACECRAFT
	              B(4) = LATITUDE OF SUBSPACECRAFT POINT
	              B(5) = LONGITUDE OF SUBSPACECRAFT POINT
	              B(6) = EQUATORIAL RADIUS
     OM (output)    R*8 PLANET TO CAMERA TRANSFORMATION MATRIX
     RS (output)    PLANET TO CAMERA POSITION VECTOR IN PLANET SYSTEM
     CL (input)     LINE OF CAMERA AXIS
     CS (input)     SAMPLE OF CAMERA AXIS
$ Return
$!#############################################################################
