$!****************************************************************************
$!
$! Build proc for MIPL module tgeom1
$! VPACK Version 1.5, Thursday, November 04, 1993, 16:34:43
$!
$! Execute by entering:		$ @tgeom1
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
$ write sys$output "*** module tgeom1 ***"
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
$   if F$SEARCH("tgeom1.imake") .nes. ""
$   then
$      vimake tgeom1
$      purge tgeom1.bld
$   else
$      if F$SEARCH("tgeom1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tgeom1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tgeom1.bld "STD"
$   else
$      @tgeom1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tgeom1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tgeom1.com -
	-s tgeom1.f -
	-p tgeom1.pdf -
	-i tgeom1.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tgeom1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL XL1(2000),XL2(2000),XL3(2000),XS1(2000),XS2(2000),XS3(2000)
	REAL BUF(12),COEFFS(6,2000),BUFLINE(20000),BUFSAMP(20000)
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
     +		  'U_NL',NL,'U_NS',NS,'U_NB',2,'U_ORG','BIL',
     +		  'OP','WRITE','O_FORMAT','REAL','U_FORMAT','REAL', ' ')
C								get triangles
	DO I=1,NTRI
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    XL1(I) = BUF(1)
	    XS1(I) = BUF(2)
	    XL2(I) = BUF(3)
	    XS2(I) = BUF(4)
	    XL3(I) = BUF(5)
	    XS3(I) = BUF(6)
	    COEFFS(1,I) = BUF(7)
	    COEFFS(2,I) = BUF(8)
	    COEFFS(3,I) = BUF(9)
	    COEFFS(4,I) = BUF(10)
	    COEFFS(5,I) = BUF(11)
	    COEFFS(6,I) = BUF(12)
	END DO
C							      build output image
	DO LINE=ISL,IEL
	    XLINE = LINE
	    CALL MVE(7,NS,0.0,BUFLINE,0,1)		! zero out line buffer
	    CALL MVE(7,NS,0.0,BUFSAMP,0,1)		! zero out sample buffer
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
		    IST = MAX(MIN(A,B,FLOAT(NS))+1.0, 1.0)
		    LAST = MIN(MAX(A,B,1.0), FLOAT(NS))
		    BUFLINE(IST) = COEFFS(1,I)*XLINE +
     +					 COEFFS(2,I)*IST + COEFFS(3,I)
		    BUFSAMP(IST) = COEFFS(4,I)*XLINE +
     +					 COEFFS(5,I)*IST + COEFFS(6,I)
		    DO J=IST+1,LAST
			BUFLINE(J) = BUFLINE(J-1) + COEFFS(2,I)
			BUFSAMP(J) = BUFSAMP(J-1) + COEFFS(5,I)
		    END DO
		END IF
	    END DO
  500	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,BUFLINE,ISTAT,' ')
	    CALL XVWRIT(IOUTUNIT,BUFSAMP,ISTAT,' ')
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tgeom1.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,40)
PARM OUT      TYPE=(STRING,40)
PARM NL       TYPE=INTEGER COUNT=1
PARM NS       TYPE=INTEGER COUNT=1
END-PROC
.TITLE
VICAR Program TGEOM1
.HELP
PURPOSE
     TGEOM1 accepts as input a triangles dataset, such as one produced by the
program TRIGRID.  It outputs a transformation (image) dataset, to be used by
TGEOM2 to geometrically rectify images. The output dataset from TGEOM1 contains
the number of lines and samples that the rectified images will possess.  It
contains two bands (BIL organization) of floating point (real) pixels.  The
value of each pixel is the location in the raw image that corresponds to this
pixel in the corrected image.  The first band holds the line number, while the
second band holds the sample number.

WRITTEN BY:            Ron Alley, November, 1993

COGNIZANT PROGRAMMER:  Ron Alley

REVISIONS: New


.LEVEL1
.VARIABLE INP
Input triangles dataset
.VARIABLE OUT
Output transformation map 
dataset
.VARIABLE NL
Number of lines in 
transformed image
.VARIABLE NS
Number of samples in 
transformed image
.LEVEL2
.VARIABLE INP
The input dataset must have been generated by the program TRIGRID, or be
organized in the same manner.
.VARIABLE OUT
The output dataset will contain the transformation map to be used by TGEOM2 to
geometrically correct images.  The output dataset will have the number of lines
and samples specified by the parameters NL and NS.  It will contain two bands
(REAL format, BIL organization).  The first band holds the line number and the
second band holds the sample number in the raw image that maps to this location.
.VARIABLE NL
NL is the number of lines to be output by TGEOM1 and TGEOM2.
There is no default value; the user must specify its value.
.VARIABLE NS
NS is the number of samples to be output by TGEOM1 and TGEOM2.
There is no default value; the user must specify its value.
$ Return
$!#############################################################################
$Imake_File:
$ create tgeom1.imake
#define  PROGRAM   tgeom1

#define MODULE_LIST tgeom1.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
