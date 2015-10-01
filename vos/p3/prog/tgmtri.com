$!****************************************************************************
$!
$! Build proc for MIPL module tgmtri
$! VPACK Version 1.8, Friday, October 06, 2000, 16:49:23
$!
$! Execute by entering:		$ @tgmtri
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
$ write sys$output "*** module tgmtri ***"
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
$ write sys$output "Invalid argument given to tgmtri.com file -- ", primary
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
$   if F$SEARCH("tgmtri.imake") .nes. ""
$   then
$      vimake tgmtri
$      purge tgmtri.bld
$   else
$      if F$SEARCH("tgmtri.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tgmtri
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tgmtri.bld "STD"
$   else
$      @tgmtri.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tgmtri.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tgmtri.com -
	-s tgmtri.f -
	-p tgmtri.pdf -
	-i tgmtri.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tgmtri.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL XL1(2000),XL2(2000),XL3(2000),XS1(2000),XS2(2000),XS3(2000)
	REAL BUF(12)
	INTEGER*2 IMAGE(20000)
C								statement func
	XSECT(XL,XLT,XLB,XST,XSB) = ((XLB-XL)*XST + (XL-XLT)*XSB) /
     +				    (XLB-XLT)
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C						    		get size field
	CALL XVSIZE(ISL,ISS,NL,NS,NTRI,NSIN)
	IEL = ISL + NL - 1
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_NL',NL,'U_NS',NS,'OP','WRITE','O_FORMAT','HALF',
     +		    'U_FORMAT','HALF',' ')
C								get triangles
	DO I=1,NTRI
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    XL1(I) = BUF(1)
	    XS1(I) = BUF(2)
	    XL2(I) = BUF(3)
	    XS2(I) = BUF(4)
	    XL3(I) = BUF(5)
	    XS3(I) = BUF(6)
	END DO
C							      build output image
	DO LINE=ISL,IEL
	    XLINE = LINE
	    CALL MVE(2,NS,0,IMAGE,0,1)			! zero out image line
	    DO I=1,NTRI
		IF (XL1(I) .GT. XLINE) GO TO 500
		IF (XL3(I) .GE. XLINE) THEN
		    A = XSECT(XLINE,XL1(I),XL3(I),XS1(I),XS3(I))
		    IF (XL2(I) .GT. XLINE .OR.
     +		       (XL2(I).EQ.XLINE .AND. XL3(I).EQ.XLINE) ) THEN
			B = XSECT(XLINE,XL1(I),XL2(I),XS1(I),XS2(I))
		    ELSE
			B = XSECT(XLINE,XL2(I),XL3(I),XS2(I),XS3(I))
		    END IF
C					A and B are the sample locations where
C					Line "LINE" intersects triangle "I"
C					
		    IST = MIN(A,B,FLOAT(NS)) + 1.0
		    LAST = MAX(A,B,1.0)
		    DO J=IST,LAST
			IMAGE(J) = I
		    END DO
		END IF
	    END DO
  500	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,IMAGE(ISS),ISTAT,' ')
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tgmtri.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,40)
PARM OUT      TYPE=(STRING,40)
PARM SL       TYPE=INTEGER COUNT=1 DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1 DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1
PARM NS       TYPE=INTEGER COUNT=1
END-PROC
.TITLE
VICAR Program TGMTRI
.HELP
PURPOSE
     TGMTRI accepts as input the triangles file that is output by TRIGRID, and
produces an image of those triangles.  That is, the output file is an image
dataset of the size of the rectified image. The value of each pixel (halfword)
is the number of the triangle to be used to generatate that pixel during
rectification.
     For example, assume that Line 50, Sample 30 in the output image has the 
value 200.  This means that triangle 200 is to be used to compute the input
pixel location that maps to Line 50, Sample 30.  The coefficients for this
transformation will be in Record (Line) 200 of the input file.  In the output
image from TGMTRI, the pixels with the value 200 will form a triangular
region that includes Line 50, Sample 30.

WRITTEN BY:            Ron Alley, November, 1993

COGNIZANT PROGRAMMER:  Ron Alley

REVISIONS: New


.LEVEL1
.VARIABLE INP
Input triangles dataset
.VARIABLE OUT
Output triangle image dataset
.VARIABLE SL
Line number of first output line
.VARIABLE SS
Sample number of first output sample
.VARIABLE NL
Number of lines in output image
.VARIABLE NS
Number of samps in output image
.LEVEL2
.VARIABLE INP
The input dataset must be a triangles dataset, normally the output dataset
from the program TRIGRID.
.VARIABLE OUT
The output dataset will be a halfword image dataset. The value at each pixel
corresponds to the record (line) number in the input file that would be used
to compute the old pixel location that maps to this point.
.VARIABLE NL
NL is the number of lines in the output image.  It should be the number of
lines in the rectified image that will be created by TGEOM2.
.VARIABLE NS
NS is the number of samples in the output image.  It should be the number of
samples in the rectified image that will be created by TGEOM2.
$ Return
$!#############################################################################
$Imake_File:
$ create tgmtri.imake
#define  PROGRAM   tgmtri

#define MODULE_LIST tgmtri.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
