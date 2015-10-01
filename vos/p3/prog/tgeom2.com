$!****************************************************************************
$!
$! Build proc for MIPL module tgeom2
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:34:25
$!
$! Execute by entering:		$ @tgeom2
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
$ write sys$output "*** module tgeom2 ***"
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
$ write sys$output "Invalid argument given to tgeom2.com file -- ", primary
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
$   if F$SEARCH("tgeom2.imake") .nes. ""
$   then
$      vimake tgeom2
$      purge tgeom2.bld
$   else
$      if F$SEARCH("tgeom2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tgeom2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tgeom2.bld "STD"
$   else
$      @tgeom2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tgeom2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tgeom2.com -mixed -
	-s tgeom2.f -
	-p tgeom2.pdf -
	-i tgeom2.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tgeom2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	CHARACTER*8 FMT
	LOGICAL XVPTST
	EXTERNAL GEOM,GEOMNOIN
C								open inputs
	CALL XVUNIT(IGRIDUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(IGRIDUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVUNIT(INUNIT,'INP',2,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,'NL',NLRAW,'NS',NSRAW,' ')
C						    		get size field
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (ISL+NL-1 .GT. NLIN .OR. ISS+NS-1 .GT. NSIN) THEN
	    CALL XVMESSAGE(
     +	     'Area requested extended beyond the rectification dataset',
     +		' ')
	    CALL ABEND
	ENDIF
	CALL XVGET(INUNIT,ISTAT,'NB',NB,' ')
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_NL',NL,'U_NS',NS,'U_NB',NB,'U_ORG','BSQ',
     +		  'OP','WRITE','O_FORMAT',FMT,'U_FORMAT','REAL', ' ')
C
C					compute buffer sizes & call STACKA to 
C					allocate buffers.  STACKA will call
C					GEOM or GEOMNOIN to perform the 
C					transformation.
	LEN1 = 4*NL*NS
	LEN2 = LEN1
	LEN3 = 4*NS
	IF (XVPTST('NOINTERP')) THEN
	    LEN4 = 4*NLRAW*NSRAW
	    CALL STACKA(15,GEOMNOIN,4,LEN1,LEN2,LEN3,LEN4,ISL,ISS,NL,NS,
     +			NB,NLRAW,NSRAW,INUNIT,IGRIDUNIT,IOUTUNIT)
	ELSE
	    LEN4 = 4*(NLRAW+2)*(NSRAW+2)
	    CALL STACKA(15,GEOM,4,LEN1,LEN2,LEN3,LEN4,ISL,ISS,NL,NS,
     +			NB,NLRAW,NSRAW,FMT,INUNIT,IGRIDUNIT,IOUTUNIT)
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE GEOMNOIN(GRIDLINE,LEN1,GRIDSAMP,LEN2,OUTIMG,LEN3,
     +			    RAWIMG,LEN4,ISL,ISS,NL,NS,NB,NLRAW,NSRAW,
     +			    INUNIT,IGRIDUNIT,IOUTUNIT)
C
C		Routine for geometric transformation when no interpolation
C		(nearest neighbor) is requested.
C
	REAL GRIDLINE(NS,NL),GRIDSAMP(NS,NL),OUTIMG(NS)
	REAL RAWIMG(NSRAW,NLRAW)
C							read in transform grid
	LOC = 1
	IEL = ISL+NL-1
	DO LINE=ISL,IEL
	    CALL XVREAD(IGRIDUNIT,GRIDLINE(1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',1,' ')
	    CALL XVREAD(IGRIDUNIT,GRIDSAMP(1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',2,' ')
	    LOC = LOC + 1
	END DO
C
	DO IBAND=1,NB
C							read in entire raw image
	    DO LINE=1,NLRAW
		CALL XVREAD(INUNIT,RAWIMG(1,LINE),ISTAT,'LINE',LINE,
     +			    'SAMP',1,'NSAMPS',NSRAW,'BAND',IBAND,' ')
	    END DO
C
	    DO LINE=1,NL
		DO ISAMP=1,NS
		    LINELOC = NINT(GRIDLINE(ISAMP,LINE))
		    ISAMPLOC = NINT(GRIDSAMP(ISAMP,LINE))
		    IF (LINELOC.LT.1 .OR. LINELOC.GT.NLRAW .OR. 
     +			    ISAMPLOC.LT.1 .OR. ISAMPLOC.GT.NSRAW) THEN
			OUTIMG(ISAMP) = 0.0
		    ELSE
			OUTIMG(ISAMP) = RAWIMG(ISAMPLOC,LINELOC)
		    END IF
		END DO
		CALL XVWRIT(IOUTUNIT,OUTIMG,ISTAT,' ')
	    END DO
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE GEOM(GRIDLINE,LEN1,GRIDSAMP,LEN2,OUTIMG,LEN3,
     +			    RAWIMG,LEN4,ISL,ISS,NL,NS,NB,NLRAW,NSRAW,
     +			    FMT,INUNIT,IGRIDUNIT,IOUTUNIT)
C
C		Routine for geometric transformation when bilinear
C		interpolation is requested.
C
	REAL GRIDLINE(NS,NL),GRIDSAMP(NS,NL),OUTIMG(NS)
	REAL RAWIMG(NSRAW+2,NLRAW+2)
	CHARACTER*8 FMT
C							read in transform grid
	LOC = 1
	IEL = ISL+NL-1
	DO LINE=ISL,IEL
	    CALL XVREAD(IGRIDUNIT,GRIDLINE(1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',1,' ')
	    CALL XVREAD(IGRIDUNIT,GRIDSAMP(1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',ISS,'NSAMPS',NS,'BAND',2,' ')
	    LOC = LOC + 1
	END DO
C
	DO IBAND=1,NB
C							read in entire raw image
C						       repeating all edge pixels
	    DO LINE=1,NLRAW
		CALL XVREAD(INUNIT,RAWIMG(2,LINE+1),ISTAT,'LINE',LINE,
     +			    'SAMP',1,'NSAMPS',NSRAW,'BAND',IBAND,' ')
		RAWIMG(1,LINE+1) = RAWIMG(2,LINE+1)
		RAWIMG(NSRAW+2,LINE+1) = RAWIMG(NSRAW+1,LINE+1)
	    END DO
	    CALL MVE(7,NSRAW+2,RAWIMG(1,2),RAWIMG(1,1),1,1)
	    CALL MVE(7,NSRAW+2,RAWIMG(1,NLRAW+1),RAWIMG(1,NLRAW+2),1,1)
C
	    IF (FMT .EQ. 'REAL    ') THEN
		DO LINE=1,NL
		    DO ISAMP=1,NS
			LINELOC = INT(GRIDLINE(ISAMP,LINE)) + 1
			ISAMPLOC = INT(GRIDSAMP(ISAMP,LINE)) + 1
		        IF (LINELOC.LT.1 .OR. LINELOC.GT.NLRAW+1 .OR. 
     +			    ISAMPLOC.LT.1 .OR. ISAMPLOC.GT.NSRAW+1) THEN
			    OUTIMG(ISAMP) = 0.0
			ELSE
			    XBAR = ISAMPLOC - GRIDSAMP(ISAMP,LINE)
			    YBAR = LINELOC - GRIDLINE(ISAMP,LINE)
			    X = 1.0-XBAR
			    Y = 1.0-YBAR
			    DN = XBAR*YBAR*RAWIMG(ISAMPLOC,LINELOC) +
     +				 X*YBAR*RAWIMG(ISAMPLOC+1,LINELOC) +
     +				 XBAR*Y*RAWIMG(ISAMPLOC,LINELOC+1) +
     +				 X*Y*RAWIMG(ISAMPLOC+1,LINELOC+1)
			    OUTIMG(ISAMP) = DN
     
			END IF
		    END DO
		    CALL XVWRIT(IOUTUNIT,OUTIMG,ISTAT,' ')
		END DO
	    ELSE
		DO LINE=1,NL
		    DO ISAMP=1,NS
			LINELOC = INT(GRIDLINE(ISAMP,LINE)) + 1
			ISAMPLOC = INT(GRIDSAMP(ISAMP,LINE)) + 1
		        IF (LINELOC.LT.1 .OR. LINELOC.GT.NLRAW+1 .OR. 
     +			    ISAMPLOC.LT.1 .OR. ISAMPLOC.GT.NSRAW+1) THEN
			    OUTIMG(ISAMP) = 0.0
			ELSE
			    XBAR = ISAMPLOC - GRIDSAMP(ISAMP,LINE)
			    YBAR = LINELOC - GRIDLINE(ISAMP,LINE)
			    X = 1.0-XBAR
			    Y = 1.0-YBAR
			    DN = XBAR*YBAR*RAWIMG(ISAMPLOC,LINELOC) +
     +				 X*YBAR*RAWIMG(ISAMPLOC+1,LINELOC) +
     +				 XBAR*Y*RAWIMG(ISAMPLOC,LINELOC+1) +
     +				 X*Y*RAWIMG(ISAMPLOC+1,LINELOC+1)
			    OUTIMG(ISAMP) = NINT(DN)
			END IF
		    END DO
		    CALL XVWRIT(IOUTUNIT,OUTIMG,ISTAT,' ')
		END DO
	    END IF
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tgeom2.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,60) COUNT=2
PARM OUT      TYPE=(STRING,60)
PARM SIZE     TYPE=INTEGER COUNT=4		DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1		DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1		DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1		DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1		DEFAULT=0
PARM INTERP   TYPE=KEYWORD VALID=(NOINTERP,BILINEAR) DEFAULT=BILINEAR
END-PROC
.TITLE
VICAR Program TGEOM2
.HELP
PURPOSE
      TGEOM2 is the third and final program in the triangular geometric
rectification sequence, following TRIGRID and TGEOM1.  TGEOM2 accepts as input
the rectification dataset that was output by TGEOM1 and the raw image to be
rectified.  It produces as output the geometrically rectified image of the
second input.  The user may choose between bilinear and nearest neighbor
interpolation methods.

RESTRICTIONS:          
      TGEOM2 reads the entire raw image into memory.  This can result in
swapping and similar efficiency problems for large input images.  The point
at which this becomes a problem depends upon the size of the input image, 
the amount of memory on the machine, and the loading of the machine by other
jobs.

WRITTEN BY:            Ron Alley, November, 1993

COGNIZANT PROGRAMMER:  Ron Alley

REVISIONS: New


.LEVEL1
.VARIABLE INP
(1) Rectification dataset
(2) Image to be transformed
.VARIABLE OUT
Transformed image dataset
.VARIABLE SIZE
VICAR Size field, in
output image coordinates
.VARIABLE SL
Start Line, output image
.VARIABLE SS
Start sample, output image
.VARIABLE NL
Number of lines output 
.VARIABLE NS
Number of samples output 
.VARIABLE INTERP
Interpolation Method.
Valid: BILINEAR, NOINTERP
.LEVEL2
.VARIABLE INP
The first input image must be the rectification dataset produced by TGEOM1.
The second input dataset must be the raw image to be rectified.
.VARIABLE OUT
Transformed image dataset
.VARIABLE SIZE
This refers to the size field (StartingLine,StartingSample,NumberLines,
NumberSamples) in output image coordinates, which is the same as the subarea
from the first input.  It does not affect the second input (the raw image)
at all.
.VARIABLE SL
This is the number of the first line of the rectification dataset to be used,
to produce the first line of the output dataset.
.VARIABLE SS
This is the number of the first sample of the rectification dataset to be used,
to produce the first sample of the output dataset.
.VARIABLE NL
Number of lines output 
.VARIABLE NS
Number of samples output 
.VARIABLE INTERP
Two interpolation methods have been implemented, selectable by this keyword.
BILINEAR specifies the use of a bilinear interpolation scheme, while NOINTERP
is specicied when the nearest neighbor approach is desired.
$ Return
$!#############################################################################
$Imake_File:
$ create tgeom2.imake
#define  PROGRAM   tgeom2

#define MODULE_LIST tgeom2.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
