$!****************************************************************************
$!
$! Build proc for MIPL module raddist
$! VPACK Version 1.8, Friday, June 20, 1997, 13:25:22
$!
$! Execute by entering:		$ @raddist
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
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module raddist ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to raddist.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("raddist.imake") .nes. ""
$   then
$      vimake raddist
$      purge raddist.bld
$   else
$      if F$SEARCH("raddist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake raddist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @raddist.bld "STD"
$   else
$      @raddist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create raddist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack raddist.com -
	-s raddist.f -
	-i raddist.imake -
	-p raddist.pdf -
	-t tstraddist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create raddist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C  REVISION HISTORY
c    1-97 ...cca... added distortion center as parameters cl,cs
C    4-94 ...CCA... INITIAL RELEASE
C    6-97 ...RRD... MADE PORTABLE FOR UNIX

        INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44 
      IMPLICIT INTEGER(A-Z)
      REAL*4 OBUF(1800),A,RADSQ,DSC,DLC,D,LC,SC
      INTEGER*4 GSL,GSS,GSS0

	CALL XVMESSAGE('RADDIST VERSION 1.15.97',' ')
C-------GRID SIZE FROM PARAMETERS
	CALL XVPARM('NROW',NROW,ICNT,IDEF,0)
	CALL XVPARM('NCOL',NCOL,ICNT,IDEF,0)
	CALL XVPARM('INCR',INC,ICNT,IDEF,0)
	CALL XVPARM('CONSTANT',A,ICNT,IDEF,0)

C-------LINE AND SAMPLE CENTER ADDED ON 1/97
	CALL XVPARM('CL',LC,ICNT,IDEF,0)
	CALL XVPARM('CS',SC,ICNT,IDEF,0)	
C
	NPOINTS = NROW*NCOL
	IF (NPOINTS .EQ. 0) GO TO 994
	IF (NPOINTS .GT. 900) GO TO 998

C-------STARTING VALUES
	GSL = INC
	GSS = INC
	GSS0 = GSS
C
c*******old code****
C-------FIND CENTER OF GRID
C	LINE NUMBER OF FIRST GRID = GSL
C	LINE NUMBER OF LAST GRID  = GSL + ((NROW-1)*INC)/2
C	CENTER = (FIRST + LAST)/2
CCCCCCC	LC = GSL + (NROW-1)*INC/2
CCCCCCC	SC = GSS + (NCOL-1)*INC/2
c*******
C
C-------DERIVATION:   (r = Rold)
C	Assumed distortion model:  Rnew = r + A*r**3
c	deltaInRadius = A*r**3 
c	triangle with sides: deltaInRadius, deltaInLine, deltaInSamp is
c       similar to the triangle with sides: r, L-LC, S-SC
c	So, deltaInLine = A*r**3*(L-LC)/r = A*r**2*(L-LC)
c	So, deltaInSamp = A*r**3*(S-SC)/r = A*r**2*(S-SC)
c
	K = 1

	DO J=1,NROW		!ROWS
          DO I=1,NCOL   	!COLUMNS
	      LINE = GSL
	      SAMP = GSS

	      DLC = (LINE - LC)
	      DSC = (SAMP - SC)

	      RADSQ = DLC*DLC + DSC*DSC

	      D = DLC*A*RADSQ
              OBUF(K) = GSL + D

	      D = DSC*A*RADSQ
              OBUF(K+1) = GSS + D
              K = K + 2
              GSS = GSS + INC
          ENDDO
          GSS = GSS0
          GSL = GSL + INC
	ENDDO

	CALL XVUNIT(IO,'OUT',1,IST,' ')

	CALL XVOPEN(IO,IST,'U_NL',1,'U_NS',NPOINTS*2,'O_FORMAT','REAL',
     1             'U_FORMAT','REAL','OP','WRITE','OPEN_ACT','SA',
     2             'IO_ACT','SA',' ')
	CALL XVWRIT(IO,OBUF,IST,' ')
	CALL XLADD(IO,'HISTORY','GRID_NROW',NROW,STAT,'FORMAT','INT',
     1             ' ')
	CALL XLADD(IO,'HISTORY','GRID_NCOL',NCOL,STAT,'FORMAT','INT',
     1             ' ')
	CALL XLADD(IO,'HISTORY','CONSTANT',A,STAT,'FORMAT','REAL',' ')
	CALL XVCLOSE(IO,IST,' ')

	RETURN
C
994	CALL XVMESSAGE('GRID SIZE MUST BE GT ZERO',' ')
	CALL ABEND
998	CALL XVMESSAGE('NUMBER OF COORDINATES MUST NOT EXCEED 900',' ')
	CALL ABEND
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create raddist.imake
#define  PROGRAM   raddist

#define MODULE_LIST raddist.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create raddist.pdf
process help=*
PARM OUT	STRING
PARM NROW	INTEGER 
PARM NCOL	INTEGER
PARM INCR	INTEGER   
PARM CONSTANT	REAL    
PARM CL		REAL
PARM CS		REAL
END-PROC
.TITLE
VICAR Program RADDIST
.HELP

RADDIST is a VICAR applications program which produces a MARK-format
coordinate set similar to GRIDGEN output.  However, RADDIST output is a
rectilinear set which has been transformed by a radial distortion.

EXECUTION

	raddist out parms

OPERATION

A geometric radial distortion is applied to set of points in a regular
grid.  The geometry of the undistorted grid is defined by the NROW, NCOL,
and INCR parameters.  The coordinates of the output points are written
in MARK format (see VICAR program MARK).  This is just a Real*4 file
of Line, Sample pairs on one record.

The distortion is applied using the equation:

	Rnew = Rold + A*Rold**3

where Rnew is the new radius from center to point,
      Rold is the old radius from center to point,
      A is the input constant.

The center is specified by parameters CL and CS.
	

RESTRICTIONS

1.  No more than 900 coordinates.

EXAMPLE

   raddist out=R ncol=20 nrow=20 inc=20 constant=1.0E-06 cl=512 cs=512


WRITTEN BY:             Charlie Avis

COGNIZANT PROGRAMMER:   Charlie Avis

REVISIONS:
     4-94 ...CCA... INITIAL RELEASE
     1-97 ...CCA... ADDED CL CS PARAMETERS FOR CENTER OF DISTORTION
     6-97 ...RRD... MADE PORTABLE FOR UNIX

.LEVEL1
.VARIABLE OUT
Output file name
.VARIABLE NROW
Number of rows in 
the coordinate grid
.VARIABLE NCOL
Number of columns in 
the coordinate grid
.VARIABLE INCR
Line and Sample increment
between rows and columns
.VARIABLE CONSTANT
The distortion constant
.VARIABLE CL
Line of center of 
distortion function.
.VARIABLE CS
Sample of center of 
distortion function.
.LEVEL2
.VARIABLE OUT
STRING
Output filename
.VARIABLE NROW
INTEGER
Number of rows in the coordinate grid.
.VARIABLE NCOL
INTEGER
Number of columns in the coordinate grid.
.VARIABLE INCR
INTEGER
Line and Sample increment between rows and columns.
.VARIABLE CONSTANT
REAL
The distortion constant A in the equation:
	Rnew = Rold + A*Rold**3
.VARIABLE CL
INTEGER
CENTER LINE NUMBER
.VARIALBE CS
INTEGER 
CENTER SAMPLE NUMBER
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstraddist.pdf
PROCEDURE
refgbl $echo
BODY 
let $echo="yes"
!
raddist ZERO NCOL=11 NROW=11 INCR=20 CONST=0.0 CL=120 CS=120
!The 0.0 constant should give (20,20) for the first list and
!(200,200) for the second
list ZERO (1,1,1,2)
list ZERO (1,241,1,2)
!
raddist EM6 NCOL=11 NROW=11 INCR=20 CONST=1.0E-06 CL=120 CS=120
!The 1.0e-06 constant should give (18,18) for the first list and
!(220,220) for the second
list EM6 (1,1,1,2)
list EM6 (1,241,1,2)
END-PROC
$ Return
$!#############################################################################
