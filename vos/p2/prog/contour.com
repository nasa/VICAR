$!****************************************************************************
$!
$! Build proc for MIPL module contour
$! VPACK Version 1.7, Friday, November 25, 1994, 12:45:57
$!
$! Execute by entering:		$ @contour
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
$ write sys$output "*** module contour ***"
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
$ write sys$output "Invalid argument given to contour.com file -- ", primary
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
$   if F$SEARCH("contour.imake") .nes. ""
$   then
$      vimake contour
$      purge contour.bld
$   else
$      if F$SEARCH("contour.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake contour
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @contour.bld "STD"
$   else
$      @contour.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create contour.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack contour.com -
	-s contour.f -
	-i contour.imake -
	-p contour.pdf -
	-t tstcontour.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create contour.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM CONTOUR

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM CONTOUR
C  PURPOSE ---
C
C	Create a graphics file of vectors representing contours 
C	derived from an input "elevation" image.
C
C  INPUT ---
C
C	DIM	Dimension of graphics file (2D or 3D)
C	ZSTART	Starting elevation for contour generation
C	ZEND	Ending elevation for contour generation
C	CONTINT	Contour interval
C	SMOOTH	Smoothing level
C	ZERO	Switch to INCLude or OMIT contouring next to 0
C		valued pixels
C
C  OUTPUT ---
C	A 2D or 3D graphics file is generated
C
C  RESTRICTIONS ---
C
C
C  SUBROUTINES CALLED ---
C	XVCLOSE		Close a file from within VICAR/TAE
C	XVOPEN		Open a file from within VICAR/TAE
C	XVP		Parameter acquisition routine
C	XVSIZE		Get window size of incomming image
C	XVUNIT		Get the unit number
C
C  COMMENTS ---
C
C  HISTORY:
C	1-95   MAC   CRI   MSTP S/W CONVERSION (VICAR PORTING)
C
C
C  MODE DECLARATIONS ---
	INTEGER LINE, SAMP, COUNT, SL, SS, NL, NS, NUMPTS
	INTEGER DIM, STATUS, UNITIN, I, NLI, NSI
	INTEGER LL(10000), BF(10000), CONTINV, ZMIN, ZMAX
	CHARACTER*4 ZERO
	LOGICAL FRINGE

C  COMMON STATEMENTS ---
C	None
C
C  LOCAL VARIABLE DESCRIPTIONS ---
C	None
C
C-----------*** BEGINNING OF EXECUTABLE CODE ***-----------------

C		+=================+
C		| INITIALIZATIONS |
C		+=================+

        CALL IFMESSAGE('CONTOUR version 02-JAN-95')
	CALL XVUNIT (UNITIN, 'INP', 1, STATUS,' ')
	CALL XVOPEN(UNITIN, STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     *            'U_FORMAT','FULL',' ')
	CALL XVSIZE (SL,SS,NL,NS,NLI,NSI)

C		Extract the contouring parameters

	CALL XVP ('DIM',DIM,COUNT)
	CALL XVP ('ZSTART',ZMIN,COUNT)
	CALL XVP ('ZEND',ZMAX, COUNT)
	CALL XVP ('CONTINT',CONTINV,COUNT)
	CALL XVP ('NUMPTS',NUMPTS,COUNT)
	CALL XVP ('ZERO',ZERO,COUNT)
	IF (ZERO(1:4).EQ.'OMIT') THEN
	    FRINGE = .FALSE.
	ELSE
	    FRINGE = .TRUE.
	END IF

C		Open the output graphics file

	CALL WRGR(1,1,DIM) ! Open a graphics output file


C	   ************ Begin EXECUTION *************

C------------------------ C O N T O U R ----------------------------

C	Program Contour

	CALL XVREAD (UNITIN,LL,STATUS,'LINE',SL,
     *		     'SAMP',SS,'NSAMPS',NS,' ')

	DO LINE = SL+1, NL+SL-1
	    CALL XVREAD (UNITIN,BF,STATUS,'LINE',LINE,'SAMP',SS,
     &                   'NSAMPS',NS,' ')
	    DO SAMP = 1, NS-1
		IF (FRINGE) THEN
		    CALL CNTRGRID (LL(SAMP),LL(SAMP+1),
     *		    BF(SAMP),BF(SAMP+1),CONTINV,ZMIN,ZMAX,
     *		    NUMPTS,FLOAT(LINE-1),FLOAT(SAMP+SS-1))
		ELSE
		    IF (.NOT.((LL(SAMP).EQ.0).OR.(LL(SAMP+1).EQ.0).OR.
     *		    (BF(SAMP).EQ.0).OR.(BF(SAMP+1).EQ.0))) THEN
			CALL CNTRGRID (LL(SAMP),LL(SAMP+1),
     *			BF(SAMP),BF(SAMP+1),CONTINV,ZMIN,ZMAX,
     *			NUMPTS,FLOAT(LINE-1),FLOAT(SAMP+SS-1))
		    END IF
		END IF
	    END DO
	    DO I = 1, NS
		LL(I)=BF(I)
	    END DO
	END DO

	CALL XVCLOSE(UNITIN,STATUS,' ')
	CALL CLGR(1)

	RETURN
	END

C ------------- C O N T O U R G R I D (CNTRGRID) --------------------

	SUBROUTINE CNTRGRID (F00,F01,F10,F11,CONTINV,ZMIN,
     *		ZMAX,NUMPTS,XOFF,YOFF)

	IMPLICIT NONE

	INTEGER F00, F01, F10, F11, NUMPTS
	INTEGER NUMPAIRS, BRANCH, POINT, I, CONTINV, ZMIN, ZMAX
	REAL XOFF, YOFF, K, X0, Y0, R, X, Y, TEMP, PHI, PHI1, PHI2
	REAL A, B, C, D, L, XEDGE(4), YEDGE(4)

	A = F10 - F00
	B = F11 - F10 - F01 + F00
	C = F01 - F00
	D = F00

	L = ZMIN + 0.5

	DO WHILE (L.LE.ZMAX)

	    CALL GETECRDS (NUMPAIRS, XEDGE, YEDGE, F00, F01, F10, F11, L)

	    IF (NUMPAIRS .GT. 0) THEN
		IF (B .EQ. 0) THEN
		    CALL PUTGR (1,XEDGE(1)+XOFF,YEDGE(1)+YOFF,L)
		    CALL PUTGR (1,XEDGE(2)+XOFF,YEDGE(2)+YOFF,L)
		    CALL PUTGR (1,0,0,0)
		ELSE
		    X0 = - C/B
		    Y0 = - A/B
		    K = 2*(A*C + B*(L-D)) / (B*B)
		    IF ((NUMPAIRS .EQ. 2) .AND. (K .GT. 0)) THEN
			TEMP = YEDGE(2)
			YEDGE(2) = YEDGE(4)
			YEDGE(4) = TEMP
			TEMP = XEDGE(2)
			XEDGE(2) = XEDGE(4)
			XEDGE(4) = TEMP
		    END IF

		    DO BRANCH = 1, NUMPAIRS
			I = 2*BRANCH - 1
			PHI1 = ATAN2 (YEDGE(I)-Y0, XEDGE(I)-X0)
			PHI2 = ATAN2 (YEDGE(I+1)-Y0, XEDGE(I+1)-X0)

			DO POINT = 0, NUMPTS-1
			    PHI = ((PHI2-PHI1)/(NUMPTS-1))*POINT + PHI1
			    IF (PHI .EQ. 0) PHI = 1.0E-8
			    R = SQRT(ABS( K/SIN(2*PHI) ))
			    X = R*COS(PHI) + X0
			    Y = R*SIN(PHI) + Y0
			    CALL PUTGR (1,X+XOFF,Y+YOFF,L)
			END DO
			CALL PUTGR (1,0.0,0.0,0.0)
		    END DO
	    	END IF
	    END IF
	    L = L + CONTINV
	END DO
	RETURN
	END

C ------------ G E T E D G E C O O R D S (GETECRDS) ------------------

	SUBROUTINE GETECRDS (NUMPAIRS, X, Y, F00, F01, F10, F11, L)

	IMPLICIT NONE

	INTEGER NUMPAIRS, J, F00, F01, F10, F11
	REAL X(4), Y(4), EDGE, L

	J = 0
	EDGE = (L-F00) / (F10-F00+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = EDGE
	    Y(J) = 0
	END IF
	EDGE = (L-F10) / (F11-F10+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = 1
	    Y(J) = EDGE
	END IF
	EDGE = (L-F01) / (F11-F01+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = EDGE
	    Y(J) = 1
	END IF
	EDGE = (L-F00) / (F01-F00+0.0001)
	IF ((EDGE .GE. 0) .AND. (EDGE .LE. 1)) THEN
	    J = J + 1
	    X(J) = 0
	    Y(J) = EDGE
	END IF
	NUMPAIRS = J/2

	RETURN
	END

C  ------------------ E N D   O F   S O U R C E ----------------------
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create contour.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM contour

   To Create the build file give the command:

		$ vimake contontour 			(VMS)
   or
		% vimake contour	       		(Unix)


************************************************************************/


#define PROGRAM	contour
#define R2LIB

#define MODULE_LIST contour.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77  
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create contour.pdf
PROCESS		HELP=*
!
! CONTOUR PDF - Create a graphics file of contours or "isolines"
!
PARM INP TYPE=(STRING,72)
PARM OUT TYPE=(STRING,72)
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM DIM TYPE=INTEGER VALID=2:3 DEFAULT=3
PARM ZSTART TYPE=INTEGER DEFAULT=0
PARM ZEND TYPE=INTEGER DEFAULT=255
PARM CONTINT TYPE=INTEGER DEFAULT=10
PARM NUMPTS TYPE=INTEGER VALID=2:10 DEFAULT=2
PARM ZERO TYPE=(STRING,4) VALID=("INCL","OMIT") DEFAULT="OMIT"
END-PROC
.HELP

PURPOSE

	Create a graphics file of isolines.

TAE COMMAND LINE FORMAT

	contour INP=ELEVATION.IMG OUT=ELVLEVELS.CTR ZSTART=100 +
		ZEND=500 CONTINT=25 NUMPTS=3
.PAGE
EXAMPLES

	contour INP=SURFACE.IMG OUT=SURFACE.CTR CONTINT=5 +
		ZSTART=100 ZEND=200 NUMPTS=5

	In this example, contours will be generated starting at
100 and continuing through 200, at an interval of 5 units. Each
cell of 4 pixels (2 by 2) used to calculate the contours will be
devided into 5 small increments. This will increase the appearance
of a smooth contour line.

.PAGE
OPERATION

	The output graphics file coordinates will be in "pixel-space"
units. This means that a subwindow of a surface image contoured will
not start with values of 1,1 but in the approximate range of the
starting line, starting sample used in the SIZE parameter (assuming
there were pixels in the correct Z value range).

.PAGE
RESTRICTIONS

	Sized down surface images should be contoured as apposed to
contouring at the original resolution. Since the algorithm moves
accross and down the surface image there are a lot of pen ups, change
Z values, and pen downs. Typically a 1/5 scale image is sufficient
to generate a believable contour map of the surface.

NOTE

	The program MOD3DCTR can be run on the output of CONTOUR. This
process will sort the output of CONTOUR in Z order as well as allow
for scaling and offsets to the 3D graphics file. (Sorting is important
if a CALCOMP plot of the contours is needed. This allows for plotting
one entire Z value contour without the constant changes of pens, as would
be the case without the postprocess MOD3DCTR).

.PAGE
REVISIONS
	1-95   Meredith Cox  (CRI) - Made portable for UNIX
.LEVEL1
.VARIABLE INP
A VICAR image
(A surface image)
.VARIABLE OUT
IBIS graphics file
.VARIABLE SIZE
Normal VICAR size field
.VARIABLE DIM
Output graphics type
(2=2D, 3=3D)
.VARIABLE ZSTART
Starting "elevation"
for isolines
.VARIABLE ZEND
Ending "elevation"
.VARIABLE CONTINT
Contour interval
.VARIABLE NUMPTS
Number of increments
per pixel (smoothing)
.VARIABLE ZERO
Switch to include
contouring next
to ZERO valued
pixels
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstcontour.pdf
procedure
refgbl $autousage
body
  let $autousage="none"
  
  ! make an image with circular contours
  f2 out=img nl=8 ns=8 fun="(line-4)**2 + (samp-4)**2"
  
  ! Test 3d option
  contour img graph zstart= 10 zend=50 contint=10 dim=3  
  ibis-list graph  gr1dim=3
  
  !Test 2d option
  contour img graph zstart= 10 zend=50 contint=10  dim=2 
  ibis-list graph gr1dim=2

  ! Test smoothing feature
  contour img graph zstart= 10 zend=50 contint=10 numpts=5 dim=3  
  ibis-list graph gr1dim=3

  ! Test ZERO options
  contour img graph zstart=0 zend=50 contint=10  dim=3  
  ibis-list graph  gr1dim=3
  
  
end-proc

$ Return
$!#############################################################################
