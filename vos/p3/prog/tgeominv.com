$!****************************************************************************
$!
$! Build proc for MIPL module tgeominv
$! VPACK Version 1.8, Friday, April 04, 2003, 19:36:23
$!
$! Execute by entering:		$ @tgeominv
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
$ write sys$output "*** module tgeominv ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tgeominv.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("tgeominv.imake") .nes. ""
$   then
$      vimake tgeominv
$      purge tgeominv.bld
$   else
$      if F$SEARCH("tgeominv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tgeominv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tgeominv.bld "STD"
$   else
$      @tgeominv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tgeominv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tgeominv.com -
	-s tgeominv.f -
	-p tgeominv.pdf -
	-i tgeominv.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tgeominv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	March 21, 2003     ...rea...   Initial release
C	March 28, 2003     ...rea...   Change background fill value to -1.0
C
	IMPLICIT NONE
	EXTERNAL INVERT_FILE
	INTEGER INUNIT,IOUTUNIT,ISTAT,ISL,ISS,NL,NS,NLIN,NSIN
	INTEGER NLOUT,NSOUT,NUM,IDEF,LEN1,LEN2,LEN3,LEN4
C								      open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C								 get output size
	CALL XVPARM('NLOUT',NLOUT,NUM,IDEF,1)
	CALL XVPARM('NSOUT',NSOUT,NUM,IDEF,1)
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'U_NL',NLOUT,'U_NS',NSOUT,'U_NB',2,
     +		    'U_ORG','BIL','IO_ACT','SA','OPEN_ACT','SA','OP',
     +		    'WRITE','O_FORMAT','REAL','U_FORMAT','REAL', ' ')
C
	LEN1 = 4*NLIN*NSIN
	LEN2 = LEN1
	LEN3 = 4*NLOUT*NSOUT
	LEN4 = LEN3
	CALL STACKA(12,INVERT_FILE,4,LEN1,LEN2,LEN3,LEN4,INUNIT,
     +		    IOUTUNIT,NLIN,NSIN,NLOUT,NSOUT)
	RETURN
	END
C******************************************************************************
	SUBROUTINE INVERT_FILE(XLINE_IN,LEN1,SAMP_IN,LEN2,XLINE_OUT,
     +			       LEN3,SAMP_OUT,LEN4,INUNIT,IOUTUNIT,NLIN,
     +			       NSIN,NLOUT,NSOUT)
C
	IMPLICIT NONE
	REAL*8 X(2,4),Y(4),COEF_L(3),COEF_S(3),ARR1(12),ARR2(9),ARR3(3)
	REAL*8 CHISQ
	REAL*8 SIG(4)/4*1.0/
	REAL XLINE_IN(NSIN,NLIN),SAMP_IN(NSIN,NLIN),POINT(2)
	REAL XLINE_OUT(NSOUT,NLOUT),SAMP_OUT(NSOUT,NLOUT),CORNER(2,4)
	INTEGER LEN1,LEN2,LEN3,LEN4,INUNIT,IOUTUNIT,NLIN,NSIN,NLOUT
	INTEGER NSOUT,IL,IS,JL,JS,ISTAT,MINL,MINS,MAXL,MAXS
	LOGICAL INSIDE,QPOINT,QFIRST
C							      read in input grid
	DO IL=1,NLIN
	    CALL XVREAD(INUNIT,XLINE_IN(1,IL),ISTAT,'LINE',IL,'BAND',1,
     +			' ')
	    CALL XVREAD(INUNIT,SAMP_IN(1,IL),ISTAT,'LINE',IL,'BAND',2,
     +			' ')
	END DO
C							initialize output arrays
	DO IL=1,NLOUT
	    DO IS=1,NSOUT
		XLINE_OUT(IS,IL) = -99999.9
		SAMP_OUT(IS,IL) = -99999.9
	    END DO
	END DO
C					       looping through each 4-pixel cell
	DO IL=2,NLIN
	    DO IS=2,NSIN
		QPOINT = .TRUE.
		CALL FIND_RANGE(XLINE_IN(IS-1,IL-1),XLINE_IN(IS,IL-1),
     +				XLINE_IN(IS-1,IL),XLINE_IN(IS,IL),NLOUT,
     +				MINL,MAXL,QPOINT)
		IF (QPOINT) CALL FIND_RANGE(SAMP_IN(IS-1,IL-1),
     +				SAMP_IN(IS,IL-1),SAMP_IN(IS-1,IL),
     +				SAMP_IN(IS,IL),NSOUT,MINS,MAXS,QPOINT)
C
		IF (QPOINT) THEN
		    CORNER(1,1) =  SAMP_IN(IS-1,IL-1)
		    CORNER(2,1) =  XLINE_IN(IS-1,IL-1)
		    CORNER(1,2) =  SAMP_IN(IS,IL-1)
		    CORNER(2,2) =  XLINE_IN(IS,IL-1)
		    CORNER(1,3) =  SAMP_IN(IS,IL)
		    CORNER(2,3) =  XLINE_IN(IS,IL)
		    CORNER(1,4) =  SAMP_IN(IS-1,IL)
		    CORNER(2,4) =  XLINE_IN(IS-1,IL)
		    QFIRST = .TRUE.
C					  loop through range of inversion points
		    DO JL=MINL,MAXL
			DO JS=MINS,MAXS
			    POINT(1) = FLOAT(JS)
			    POINT(2) = FLOAT(JL)
C					 first check the upper left corner point
			    IF (POINT(1).EQ.CORNER(1,1) .AND.
     +					POINT(2).EQ.CORNER(2,1)) THEN
				XLINE_OUT(JS,JL) =  FLOAT(IL-1)
				SAMP_OUT(JS,JL) = FLOAT(IS-1)
C							     then check interior
			    ELSE IF (INSIDE(POINT,CORNER,4)) THEN
				IF (QFIRST) THEN
C							compute the fitting 
C						       cofficients for this cell
				    CALL MVE(9,8,CORNER,X,1,1)
				    Y(1) = DFLOAT(IL-1)
				    Y(2) = DFLOAT(IL-1)
				    Y(3) = DFLOAT(IL)
				    Y(4) = DFLOAT(IL)
				    CALL SVDFIT(X,Y,SIG,4,2,1,3,COEF_L,
     +						ARR1,ARR2,ARR3,CHISQ)
				    Y(1) = DFLOAT(IS-1)
				    Y(2) = DFLOAT(IS)
				    Y(3) = DFLOAT(IS)
				    Y(4) = DFLOAT(IS-1)
				    CALL SVDFIT(X,Y,SIG,4,2,1,3,COEF_S,
     +						ARR1,ARR2,ARR3,CHISQ)
				    QFIRST = .FALSE.
				END IF
				XLINE_OUT(JS,JL) = COEF_L(1)*FLOAT(JS) +
     +					COEF_L(2)*FLOAT(JL) + COEF_L(3)
				SAMP_OUT(JS,JL) = COEF_S(1)*FLOAT(JS) +
     +					COEF_S(2)*FLOAT(JL) + COEF_S(3)
			    END IF
			END DO
		    END DO
		END IF
	    END DO
	END DO
C							      write output image
	DO IL=1,NLOUT
	    CALL XVWRIT(IOUTUNIT,XLINE_OUT(1,IL),ISTAT,' ')
	    CALL XVWRIT(IOUTUNIT,SAMP_OUT(1,IL),ISTAT,' ')
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FIND_RANGE(X1,X2,X3,X4,IEND,LOW,IHI,QPOINT)
C
C	This routine finds the low (LOW) and high (IHI) integers that are
C	spanned by X1 through X4.  The range is truncated to the range 1 
C	to IEND. If no integers are spanned by X1-X4, then QPOINT is set
C	to FALSE.
C
	IMPLICIT NONE
	REAL X1,X2,X3,X4,X
	INTEGER IEND,LOW,IHI,I
	LOGICAL QPOINT
C
	X = MIN(X1,X2,X3,X4)
	IF (X .LE. 0.0) THEN
	    QPOINT = .FALSE.
	    RETURN
	END IF
	I = INT(X)
	IF (X .EQ. FLOAT(I)) THEN
	    LOW = I
	ELSE
	    LOW = I + 1
	END IF
C
	IHI = MIN(IEND,INT(MAX(X1,X2,X3,X4)))
	IF (LOW .GT. IHI) QPOINT = .FALSE.
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tgeominv.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,60)
PARM OUT      TYPE=(STRING,60)
PARM NLOUT    TYPE=INTEGER
PARM NSOUT    TYPE=INTEGER
END-PROC
.TITLE
VICAR Program TGEOMINV
.HELP
PURPOSE
      TGEOMINV is a program to produce the inverse transformation map, for
use by the program TGEOM2.  That is, if the input file to TGEOMINV can be used
to geometrically correct (register) ImageA to ImageB, then the output file
from TGEOMINV can be used to register ImageB to ImageA.


WRITTEN BY:            Ron Alley, March, 2003

COGNIZANT PROGRAMMER:  Ron Alley

REVISIONS: New


.LEVEL1
.VARIABLE INP
Rectification dataset
.VARIABLE OUT
Inverted rectification dataset
.VARIABLE NLOUT
Number of lines output 
.VARIABLE NSOUT
Number of samples output 
.LEVEL2
.VARIABLE INP
The input file must be a rectification dataset in the format that is
output by TGEOM1.
.VARIABLE OUT
The output file will be a rectification dataset that is appropriate for
performing the reverse (master to slave) registration to the one specified
by the input dataset.  It will have the format that is appropriate for use
in TGEOM2
.VARIABLE NLOUT
Number of lines in the output file
.VARIABLE NSOUT
Number of samples in the output file
.END
$ Return
$!#############################################################################
$Imake_File:
$ create tgeominv.imake
#define  PROGRAM   tgeominv

#define MODULE_LIST tgeominv.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
