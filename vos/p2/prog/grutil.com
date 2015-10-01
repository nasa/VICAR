$!****************************************************************************
$!
$! Build proc for MIPL module grutil
$! VPACK Version 1.8, Tuesday, January 30, 1996, 12:44:36
$!
$! Execute by entering:		$ @grutil
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
$ write sys$output "*** module grutil ***"
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
$ write sys$output "Invalid argument given to grutil.com file -- ", primary
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
$   if F$SEARCH("grutil.imake") .nes. ""
$   then
$      vimake grutil
$      purge grutil.bld
$   else
$      if F$SEARCH("grutil.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake grutil
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @grutil.bld "STD"
$   else
$      @grutil.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create grutil.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack grutil.com -
	-s grutil.f -
	-p grutil.pdf -
	-i grutil.imake -
	-t tstgrutil.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create grutil.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE
	INTEGER	COUNT, DEF
	INTEGER	INPCOUNT, OUTCOUNT
	INTEGER	DIM, OUTDIM
	INTEGER	I, GRFILE
	REAL	FIRST, SECOND, THIRD(40)
	REAL	ZEROS(40)/40*0.0/
	REAL	X, Y, X1, Y1, X2, Y2, DX, DY, DIST, INTERVAL, DELTA, T
	LOGICAL	EOF, EOL,   ALLZERO
	CHARACTER*16  OPTION

        
	CALL XVPARM ('OPTION', OPTION, COUNT, DEF, 0)

	CALL XVPARM ('DIM', DIM, COUNT, DEF, 0)
	


	IF (OPTION(1:6) .EQ. 'APPEND') THEN

	    CALL wrgr_grutil (1, 1, DIM)
	    CALL XVPCNT ('INP', INPCOUNT)
	    DO I = 1, INPCOUNT
		CALL rdgr_grutil (I, I+1, DIM)
		EOF = .FALSE.
		DO WHILE (.NOT. EOF)
		    CALL NEXTGR (I+1, EOF, FIRST, SECOND, THIRD)
		    IF (.NOT. EOF) THEN
			EOL = .FALSE.
			DO WHILE (.NOT. EOL)
			    CALL PUTGR (1, FIRST, SECOND, THIRD)
			    CALL GETGR (I+1, EOL, EOF, FIRST, SECOND, THIRD)
			ENDDO
			CALL PUTGR (1, 0.0, 0.0, ZEROS)
		    ENDIF
		ENDDO
		CALL CLGR (I+1)
	    ENDDO
	    CALL CLGR (1)


	ELSE IF (OPTION(1:4) .EQ. 'SWAP') THEN

	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, DIM)
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		CALL NEXTGR (1, EOF, FIRST, SECOND, THIRD)
		IF (.NOT. EOF) THEN
		    EOL = .FALSE.
		    DO WHILE (.NOT. EOL)
			CALL PUTGR (2, SECOND, FIRST, THIRD)
			CALL GETGR (1, EOL, EOF, FIRST, SECOND, THIRD)
		    ENDDO
		    CALL PUTGR (2, 0.0, 0.0, ZEROS)
		ENDIF
	    ENDDO
	    CALL CLGR (1)
	    CALL CLGR (2)


	ELSE IF (OPTION(1:6) .EQ. '2DTO3D') THEN

	    CALL wrgr_grutil (1, 1, 3)
	    CALL XVPCNT ('INP', INPCOUNT)
	    DO I = 1, INPCOUNT
		CALL rdgr_grutil (I, I+1, 2)
		THIRD(1) = FLOAT(I)
		EOF = .FALSE.
		DO WHILE (.NOT. EOF)
		    CALL NEXTGR (I+1, EOF, FIRST, SECOND)
		    IF (.NOT. EOF) THEN
			EOL = .FALSE.
			DO WHILE (.NOT. EOL)
			    CALL PUTGR (1, FIRST, SECOND, THIRD)
			    CALL GETGR (I+1, EOL, EOF, FIRST, SECOND)
			ENDDO
			CALL PUTGR (1, 0.0, 0.0, 0.0)
		    ENDIF
		ENDDO
		CALL CLGR (I+1)
	    ENDDO
	    CALL CLGR (1)


	ELSE IF (OPTION(1:6) .EQ. '3DTO2D') THEN

	    CALL rdgr_grutil (1, 1, 3)
	    CALL XVPCNT ('OUT', OUTCOUNT)
	    DO I = 1, OUTCOUNT
		CALL wrgr_grutil (I, I+1, 2)
	    ENDDO
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		CALL NEXTGR (1, EOF, FIRST, SECOND, THIRD)
		GRFILE = INT(THIRD(1))
		IF (.NOT. EOF .AND. 
     +			GRFILE .GE. 1 .AND. GRFILE .LE. OUTCOUNT) THEN
		    EOL = .FALSE.
		    DO WHILE (.NOT. EOL)
			CALL PUTGR (GRFILE+1, FIRST, SECOND)
			CALL GETGR (1, EOL, EOF, FIRST, SECOND, THIRD)
		    ENDDO
		    CALL PUTGR (GRFILE+1, 0.0, 0.0)
		ENDIF
	    ENDDO
	    CALL CLGR (1)
	    DO I = 1, OUTCOUNT
		CALL CLGR (I+1)
	    ENDDO


	ELSE IF (OPTION(1:5) .EQ. 'REDIM') THEN

	    CALL XVPARM ('OUTDIM', OUTDIM, COUNT, DEF, 0)
	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, OUTDIM)
	    SECOND = 0.0
	    DO I = 3, OUTDIM
		THIRD(I-2) = 0.0
	    ENDDO
	    EOF = .FALSE.
	    DO WHILE (.NOT. EOF)
		CALL NEXTGR (1, EOF, FIRST, SECOND, THIRD)
		IF (.NOT. EOF) THEN
		    EOL = .FALSE.
		    DO WHILE (.NOT. EOL)
			CALL PUTGR (2, FIRST, SECOND, THIRD)
			CALL GETGR (1, EOL, EOF, FIRST, SECOND, THIRD)
		    ENDDO
		    CALL PUTGR (2, 0.0, 0.0, ZEROS)
		ENDIF
	    ENDDO
	    CALL CLGR (1)
	    CALL CLGR (2)


	ELSE IF (OPTION(1:4) .EQ. 'FILL') THEN

	    CALL XVP ('INTERVAL', INTERVAL, COUNT)
	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, DIM)

	    DO WHILE (.TRUE.)
C			Scan for the beginning of a line string
		X1 = 0.0
		Y1 = 0.0
		DO WHILE (X1 .EQ. 0.0 .AND. Y1 .EQ. 0.0)
		    CALL GETGR (1, ALLZERO, EOF, Y1, X1, THIRD)
		    IF ( EOF ) GO TO 30
		ENDDO
C			Put out the first point in the linestring
		CALL PUTGR (2, Y1, X1, THIRD)
						! Get the next pair
		CALL GETGR (1, ALLZERO, EOF, Y2, X2, THIRD)
		IF ( EOF ) GO TO 30
		EOL = (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0)
	
		IF (.NOT. EOL ) THEN
		    DO WHILE (.NOT. EOL .AND. .NOT. EOF)
			DY = Y2 - Y1
			DX = X2 - X1
			DIST = SQRT( DY**2 + DX**2 )
			IF (DIST .GT. 0.0) THEN
			    DELTA = INTERVAL/DIST
			ELSE
			    DELTA = 2.0
			ENDIF
				! Fill in the points in between
			T = DELTA
			DO WHILE (T .LT. 1.0)
			    Y = Y1 + T*DY
			    X = X1 + T*DX
			    CALL PUTGR (2, Y, X, THIRD)
			    T = T + DELTA
			ENDDO
			CALL PUTGR (2, Y2, X2, THIRD)   
				! Get the next point in linestring
			Y1 = Y2
			X1 = X2
			CALL GETGR (1, ALLZERO, EOF, Y2, X2, THIRD)
			EOL = (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0)
			IF ( EOF ) GO TO 30
		    ENDDO
		ENDIF
		CALL PUTGR (2, 0.0, 0.0, ZEROS)
	    ENDDO
   30	    CONTINUE
	    CALL CLGR (1)
	    CALL CLGR (2)



	ELSE IF (OPTION(1:6) .EQ. 'SMOOTH') THEN

	    CALL XVP ('INTERVAL', INTERVAL, COUNT)
	    INTERVAL = INTERVAL**2
	    CALL rdgr_grutil (1, 1, DIM)
	    CALL wrgr_grutil (1, 2, DIM)

	    DO WHILE (.TRUE.)
C			Scan for the beginning of a line string
		X1 = 0.0
		Y1 = 0.0
		DO WHILE (X1 .EQ. 0.0 .AND. Y1 .EQ. 0.0)
		    CALL GETGR (1, ALLZERO, EOF, Y1, X1, THIRD)
		    IF ( EOF ) GO TO 40
		ENDDO
C			Put out the first point in the linestring
		CALL PUTGR (2, Y1, X1, THIRD)

		EOL = .FALSE.
		DO WHILE (.NOT. EOL)
				! Get the next point in linestring
		    CALL GETGR (1, ALLZERO, EOF, Y2, X2, THIRD )
		    IF ( EOF ) GO TO 40
		    EOL = (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0)
		    IF (.NOT. EOL) THEN
C		       Output if distance from last output point is large enough
			DIST = (Y2-Y1)**2 + (X2-X1)**2
			IF (DIST .GE. INTERVAL) THEN
			    CALL PUTGR (2, Y2, X2, THIRD)
			    Y1 = Y2
			    X1 = X2
			ENDIF
		    ENDIF
		ENDDO
		CALL PUTGR (2, 0.0, 0.0, ZEROS)
	    ENDDO
 40	    CONTINUE
	    CALL CLGR (1)
	    CALL CLGR (2)

	ENDIF

	RETURN
	END



c
c	Code added to check for missing files - fr 088285
c


      subroutine wrgr_grutil(instance,vicarfile,dimension)

      integer instance, vicarfile, dimension
      integer status, wrgr

      status = wrgr(instance,vicarfile,dimension)
      if ( status .ne. 1 ) call signalgr(vicarfile,status,1)

      return
      end

      subroutine rdgr_grutil(instance,vicarfile,dimension)

      integer instance, vicarfile, dimension
      integer status, rdgr

      status = rdgr(instance,vicarfile,dimension)
      if ( status .ne. 1 ) call signalgr(vicarfile,status,1)

      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create grutil.pdf
PROCESS HELP=*
 PARM INP	TYPE=(STRING,64)  COUNT=1:19
 PARM OUT	TYPE=(STRING,64)  COUNT=1:19
 PARM DIM       TYPE=INTEGER VALID=1:40  DEFAULT=2
 PARM OPTION    TYPE=KEYWORD +
             VALID=(APPEND,SWAP,2DTO3D,3DTO2D,REDIM,FILL,SMOOTH) 
 PARM OUTDIM    TYPE=INTEGER VALID=1:40 DEFAULT=3
 PARM INTERVAL  TYPE=REAL  valid=1:32767 DEFAULT=1.0
END-PROC
.TITLE
VICAR/IBIS Program GRUTIL
.HELP
PURPOSE

    GRUTIL is a utility program for IBIS graphics-1 format files.
It can append files, convert between 3D and 2D files, etc.

.PAGE
EXECUTION

    GRUTIL (GR1.GRA,GR2.GRA,GR3.GRA) GROUT.GRA DIM=3 'APPEND

    GRUTIL GRIN.GRA  GROUT.GRA  DIM=2    'SWAP

    GRUTIL (GR1.GRA,GR2.GRA,GR3.GRA)  GROUT.GRA   '2DTO3D

    GRUTIL GRIN.GRA  (GR1.GRA,GR2.GRA,GR3.GRA)  '3DTO2D

    GRUTIL GRIN.GRA  GROUT.GRA  DIM=3  OUTDIM=2  'REDIM

    GRUTIL GRIN.GRA  GROUT.GRA  DIM=2   'FILL INTERVAL=0.1

    GRUTIL GRIN.GRA  GROUT.GRA  DIM=2   'SMOOTH INTERVAL=10


Original Programmer:  Frank Evans         April 1987

Cognizant Programmer: Frank Evans

.LEVEL1
.VARIABLE INP
The input graphics file(s)
.VARIABLE OUT
The output graphics file(s)
.VARIABLE DIM
The dimension of the input
graphics files
.VARIABLE OPTION
Keyword for the operation
to perform:
'APPEND  to append files
'SWAP    to swap first two 
          coords in each set
          (e.g. x-y to y-x)
'3DTO2D  to convert a 3D file
          to multiple 2D files
'2DTO3D  to convert multiple 2D
          files to a 3D file
'REDIM   to change dimension of
          file
'FILL    to add points to line
          strings
'SMOOTH  to lower resolution of
          file by deleting points
.VARIABLE OUTDIM
The dimension of the output
file for REDIM option.
.VARIABLE INTERVAL
The point spacing for the
FILL and SMOOTH options.

.LEVEL2
.VARIABLE INP
The input graphics file(s).  The maximum number of files is 19.
.VARIABLE OUT
The output graphics file(s).  The maximum number of files is 19.
.VARIABLE DIM
The dimension of the input graphics files.
.VARIABLE OPTION
Keyword for the operation to perform:
'APPEND  Appends graphics files.  All of the input files are assumed
           to have dimension of DIM.  Output file has same dimension.

'SWAP    Swaps first two coordinates of each set (e.g. x-y to y-x).

'3DTO2D  Converts a 3D file whose third coordinate is a file or level 
           number to multiple 2D files.  The file number of a line string
           is the integer part of the third coordinate in the first set.
           Linestrings whose file number is outside the range (one to
           the number of output files) are not output.
           
'2DTO3D  Converts multiple 2D files to one 3D file with the third 
           coordinate containing the file number.  The third coordinate
           in each set (except the zero terminator) contains the file
           number (i.e. 1, 2, . . .).

'REDIM   Changes the dimension of the file.  The dimension of the output
           file is specified with the OUTDIM parameter.  If the output
           dimension is smaller than the input then only the beginning
           coordinates are used.  If the output dimension is larger then
           the extra coordinates are zero.

'FILL    Adds points to line strings so there is a point every INTERVAL
	   distance.  Every point in the input file is transfered to the
	   output file.  If points in the original file are larger than the
	   specified distance apart, then points are added on the joining
	   lines with the appropriate spacing.  The distance calculations
	   are performed in 2-D, i.e. using the first two coordinates.

'SMOOTH  Lowers the spatial resolution of a graphics file by deleting points
	   so that the points are greater than INTERVAL distance apart.
	   The first point in every line string is output.  Succeeding points
	   are output if they are greater than the specified distance from
	   the previously output point.  The distance calculations
	   are performed in 2-D, i.e. using the first two coordinates.

.VARIABLE OUTDIM
The dimension of the output graphics file for the REDIM option.
.VARIABLE INTERVAL
INTERVAL specifies the desired point spacing in the graphcis line strings
for the FILL and SMOOTH options.  
.END
$ Return
$!#############################################################################
$Imake_File:
$ create grutil.imake
#define PROGRAM grutil

#define MODULE_LIST grutil.f

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$Test_File:
$ create tstgrutil.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
!	Make three 2-D graphics files
  IBIS-GEN A  DATA=(100,10,150,10,150,15,0,0) NC=2 datacol=(1,2)
  IBIS-GEN B  DATA=(200,20,250,20,250,25,0,0) NC=2 datacol=(1,2)
  IBIS-GEN C  DATA=(300,30,350,30,350,35,0,0) NC=2 datacol=(1,2)
  IBIS-LIST A
  IBIS-LIST B
  IBIS-LIST C
!	Test append option
  GRUTIL (A,B,C) D  DIM=2 'APPEND
  IBIS-LIST D GR1=2 
!	Test 2D to 3D option
  GRUTIL (A,B,C) E  '2DTO3D
  IBIS-LIST E   GR1=3
!	Test 3D to 2D option
  GRUTIL E  (F,G)   '3DTO2D
  IBIS-LIST F     GR1=2
  IBIS-LIST G     GR1=2
!	Test swap option
  GRUTIL D  H  DIM=2  'SWAP
  IBIS-LIST H     GR1=2
!	Test redim option
  GRUTIL E  I  DIM=3 OUTDIM=2 'REDIM
  IBIS-LIST I     GR1=2
!	Test fill option
  GRUTIL D  J  INTERVAL=2  'FILL
  IBIS-LIST J     GR1=2  
!	Test smooth option
  GRUTIL J  K  INTERVAL=10  'SMOOTH
  IBIS-LIST K     GR1=2  
end-proc
$ Return
$!#############################################################################
