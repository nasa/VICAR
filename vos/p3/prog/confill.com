$!****************************************************************************
$!
$! Build proc for MIPL module confill
$! VPACK Version 1.5, Monday, March 29, 1993, 13:35:26
$!
$! Execute by entering:		$ @confill
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
$ write sys$output "*** module confill ***"
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
$   if F$SEARCH("confill.imake") .nes. ""
$   then
$      vimake confill
$      purge confill.bld
$   else
$      if F$SEARCH("confill.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake confill
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @confill.bld "STD"
$   else
$      @confill.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create confill.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack confill.com -
	-s confill.f -
	-i confill.imake -
	-p confill.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create confill.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	11 MAR 1992  ...REA...    New Program
C
	LOGICAL XVPTST
	CHARACTER*4 FMT
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL',' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','OP','WRITE',' ')
C
	CALL XVPCNT('OUT',NOUT)
	IF (NOUT .EQ. 2) THEN
	    CALL XVUNIT(IOUT2,'OUT',2,ISTAT,' ')
	    CALL XVOPEN(IOUT2,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','HALF','O_FORMAT','HALF',
     +			'OP','WRITE',' ')
	END IF
C								get parameters
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,' ')
	IF (FMT.EQ.'REAL') THEN
	    ROUND = 0.0
	ELSE
	    ROUND = 0.5
	END IF
	CALL XVPARM('BACKGRND',BACKGRND,ICNT,IDEF,0)
C
	IF (XVPTST('LINEAR')) THEN
	    IF (NOUT .NE. 2) THEN
		CALL LININTERP(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +			       ROUND)
	    ELSE
		CALL LININTERP2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +			        BACKGRND,ROUND)
	    END IF
	ELSE
	    IF (NOUT .NE. 2) THEN
		CALL CUBICSPLINE(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +				 ROUND)
	    ELSE
		CALL CUBICSPLINE2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +				  BACKGRND,ROUND)
	    END IF
	END IF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE LININTERP(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +			     ROUND)
C
	REAL BUF(15000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find first contour line
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) GO TO 250
	    END DO
C							no contours found
	    WRITE (PRT,200) LINE
  200	    FORMAT(' No contour lines found on Line',I5)
	    CALL XVMESSAGE(PRT,' ')
	    GO TO 300
C						extend the first contour's value
C						to the start of the line
  250	    CONTINUE
	    II = ISAMP + 1
	    X1 = ISAMP
	    Y1 = BUF(ISAMP)
	    ISTART = II
	    DO IPIX=1,II-2
		BUF(IPIX) = Y1
	    END DO
C						search for the next contour and
C						and interpolate pixels between
	    DO ISAMP=II,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    DELTA = (BUF(ISAMP)-Y1) / (ISAMP-X1)
		    DO IPIX=ISTART,ISAMP-1
			BUF(IPIX) = Y1 + (IPIX-X1)*DELTA + ROUND
		    END DO
		    X1 = ISAMP
		    Y1 = BUF(ISAMP)
		    ISTART = ISAMP + 1
		END IF
	    END DO
C							fill remainder of line
	    DO IPIX=ISTART,NS
		BUF(IPIX) = Y1
	    END DO
C
  300	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE LININTERP2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +			      BACKGRND,ROUND)
C
	REAL BUF(15000)
	INTEGER*2 BUF2(15000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find first contour line
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) GO TO 250
	    END DO
C							no contours found
	    WRITE (PRT,200) LINE
  200	    FORMAT(' No contour lines found on Line',I5)
	    CALL XVMESSAGE(PRT,' ')
	    DO ISAMP=1,NS
		BUF2(ISAMP) = 32767
	    END DO
	    GO TO 300
C						extend the first contour's value
C						to the start of the line
  250	    CONTINUE
	    X1 = ISAMP
	    Y1 = BUF(ISAMP)
	    ISTART = ISAMP + 1
	    DO IPIX=1,ISAMP
		BUF(IPIX) = Y1
		BUF2(IPIX) = ISAMP - IPIX
	    END DO
C						search for the next contour and
C						and interpolate pixels between
	    II = ISTART
	    DO ISAMP=II,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    DELTA = (BUF(ISAMP)-Y1) / (ISAMP-X1)
		    DO IPIX=ISTART,ISAMP-1
			XX = IPIX
			BUF(IPIX) = Y1 + (XX-X1)*DELTA + ROUND
			BUF2(IPIX) = MIN(XX-X1,ISAMP-XX)
		    END DO
		    BUF2(ISAMP) = 0
		    X1 = ISAMP
		    Y1 = BUF(ISAMP)
		    ISTART = ISAMP + 1
		END IF
	    END DO
C							fill remainder of line
	    DO IPIX=ISTART,NS
		BUF(IPIX) = Y1
		BUF2(IPIX) = IPIX - ISTART + 1
	    END DO
C
  300	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    CALL XVWRIT(IOUT2,BUF2,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE CUBICSPLINE(INUNIT,IOUTUNIT,ISL,ISS,NL,NS,BACKGRND,
     +				ROUND)
C
	REAL BUF(15000),X(5000),Y(5000),Y2(5000),U(5000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find all contours that
C							cross this line
	    NUM = 1
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    NUM = NUM + 1
		    X(NUM) = ISAMP
		    Y(NUM) = BUF(ISAMP)
		END IF
	    END DO
C
	    IF (NUM .EQ. 1) THEN
		WRITE (PRT,200) LINE
  200		FORMAT(' No contour lines found on Line',I5)
		CALL XVMESSAGE(PRT,' ')
	    ELSE
C						copy the first and last values
C						to beyond the line endpoints
C						to force good behavior at ends
		X(1) = 0.0
		Y(1) = Y(2)
		NUM = NUM + 1
		X(NUM) = NS + 1
		Y(NUM) = Y(NUM-1)
C						fit a spline
		CALL SPLINEA(X,Y,Y2,U,NUM)
		CALL SPLINEB(BUF,X,Y,Y2,NUM,NS,ROUND)
	    END IF
C
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE CUBICSPLINE2(INUNIT,IOUTUNIT,IOUT2,ISL,ISS,NL,NS,
     +				BACKGRND,ROUND)
C
	REAL BUF(15000),X(5000),Y(5000),Y2(5000),U(5000)
	INTEGER*2 BUF2(15000)
	CHARACTER*80 PRT
C								process image,
C								line by line
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'SAMP',ISS,'NSAMPS',NS,
     +			'LINE',LINE,' ')
C							find all contours that
C							cross this line
	    NUM = 1
	    DO ISAMP=1,NS
		IF (BUF(ISAMP) .NE. BACKGRND) THEN
		    NUM = NUM + 1
		    X(NUM) = ISAMP
		    Y(NUM) = BUF(ISAMP)
		END IF
	    END DO
C
	    IF (NUM .EQ. 1) THEN
		WRITE (PRT,200) LINE
  200		FORMAT(' No contour lines found on Line',I5)
		CALL XVMESSAGE(PRT,' ')
		DO IPIX=1,NS
		    BUF2(IPIX) = 32767
		END DO
	    ELSE
C						copy the first and last values
C						to beyond the line endpoints
C						to force good behavior at ends
		X(1) = 0.0
		Y(1) = Y(2)
		NUM = NUM + 1
		X(NUM) = NS + 1
		Y(NUM) = Y(NUM-1)
C						fit a spline
		CALL SPLINEA(X,Y,Y2,U,NUM)
		CALL SPLINEB(BUF,X,Y,Y2,NUM,NS,ROUND)
C						fill in the distance buffer
		I1 = X(2)
		DO IPIX = 1,I1
		    BUF2(IPIX) = I1 - IPIX
		END DO
C
		LOC = 3
		I2 = X(LOC)
		I3 = X(NUM-1)
		DO IPIX=I1+1,I3
		    BUF2(IPIX) = MIN(IPIX-I1,I2-IPIX)
		    IF (IPIX .EQ. I2) THEN
			LOC = LOC + 1
			I1 = I2
			I2 = X(LOC)
		    END IF
		END DO
C
		DO IPIX=I3+1,NS
		    BUF2(IPIX) = IPIX - I3
		END DO
	    END IF
C
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NS,' ')
	    CALL XVWRIT(IOUT2,BUF2,ISTAT,'NSAMPS',NS,' ')
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SPLINEA(X,Y,Y2,U,NUM)
C
C	This routine computes the
	REAL X(*),Y(*),Y2(*),U(*)
C
	Y2(1) = 0.0
	U(1) = 0.0
	QN = 0.0
	UN = 0.0
C
	DO I=2,NUM-1
	    SIG   = (X(I)-X(I-1)) / (X(I+1)-X(I-1))
	    P     = SIG*Y2(I-1) + 2.0
	    Y2(I) = (SIG-1.0) / P
	    U(I)  = (6.0*((Y(I+1)-Y(I))/(X(I+1)-X(I)) - (Y(I)-Y(I-1)) /
     .		    (X(I)-X(I-1))) / (X(I+1)-X(I-1)) - SIG*U(I-1)) / P
	END DO
C
	Y2(NUM) = (UN - QN*U(NUM-1))  /  (QN*Y2(NUM-1) + 1.0)
	DO I=NUM-1,1,-1
	    Y2(I) = Y2(I)*Y2(I+1) + U(I)
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SPLINEB(BUF,X,Y,Y2,NUM,NS,ROUND)
C
	REAL BUF(*),X(*),Y(*),Y2(*)
C
	LOC1 = 1
	LOC2 = 2
	WIDTH = X(LOC2) - X(LOC1)
	DO ISAMP=1,NS
	    XX = ISAMP
	    IF (XX .GT. X(LOC2)) THEN
		LOC1 = LOC2
		LOC2 = LOC2 + 1
		WIDTH = X(LOC2) - X(LOC1)
	    END IF
	    A = (X(LOC2) - XX) / WIDTH
	    B = (XX - X(LOC1)) / WIDTH
	    BUF(ISAMP) = A*Y(LOC1) + B*Y(LOC2) + ROUND +
     +		 ((A*A*A-A)*Y2(LOC1)+(B*B*B-B)*Y2(LOC2))*WIDTH*WIDTH/6.0
	END DO
C						
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create confill.imake
#define  PROGRAM   confill

#define MODULE_LIST confill.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create confill.pdf
process help=*
 PARM INP         TYPE=(STRING,60)
 PARM OUT         TYPE=(STRING,60)     COUNT=(1:2)
 PARM SIZE        TYPE=INTEGER         COUNT=4          DEFAULT=(1,1,0,0)
 PARM SL          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM SS          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM NL          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM NS          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM BACKGRND    TYPE=REAL				DEFAULT=0.0
 PARM MODE	  TYPE=KEYWORD	VALID=(LINEAR,SPLINE)	DEFAULT=SPLINE
 END-PROC
.TITLE
 CONFILL --  CONtour line image FILL-in program
.HELP
      CONFILL is a VICAR program that fills by interpolation an image of
 contour lines.  The input image must consist of contour lines, each of whose
 DN is equal to the value of the contour.  The background pixels must all have
 a single DN value.  Interpolation is performed in the horizontal direction,
 and the user may choose either a linear or cubic spline interpolation.  The
 values of the outermost contours are extended to the edges of the image.
.PAGE
 ORIGINAL PROGRAMMER:  Ron Alley    6 March 1992
 
 CURRENT COGNIZANT PROGRAMMER: Ron Alley
.LEVEL1
.VARIABLE INP
 input contour image
.VARIABLE OUT
 (1) output filled image
 (2) [optional] image of
     horizontal distance to
     nearest contour, in 
     pixels. HALFword format
.VARIABLE SIZE
 output image window
 (SL,SS,NL,NS)
.VARIABLE SL
 starting line
.VARIABLE SS
 starting sample
.VARIABLE NL
 number of lines
.VARIABLE NS
 number of samples
.VARIABLE BACKGRND
 DN value of background pixels
.VARIABLE MODE
 interpoloation mode
 Valid: LINEAR, SPLINE
.LEVEL2
.VARIABLE INP
 input data set
.VARIABLE OUT
 Either 1 or 2 output datasets may be specified.  The first dataset will
 contain the filled in image that has been generated from the contour image.
 The second (optional) dataset will contain a map of the distance (in pixels)
 to the nearest contour line used for filling that pixel in the first output.
 .VARIABLE SIZE
 image size (SL,SS,NL,NS)
.VARIABLE SL
 The first line of the input image to be output.
.VARIABLE SS
 The first pixel of each input line to be output.
.VARIABLE NL
 The number of lines to be output.
.VARIABLE NS
 The number of pixels to be output for each line.
.VARIABLE BACKGRND
 CONFILL expects all background pixels to be of a single DN value, specified
 by this parameter (Default value is 0).  All pixels with different values
 are assumed to be part of a contour line.
.VARIABLE MODE
 Two interpolation modes are currently available in CONFILL: linear and cubic
 spline.  Extrapolation, when needed, is done by extension of the outermost
 contour. The valid keywords are: LINEAR, and SPLINE. SPLINE is the default.
.END
$ Return
$!#############################################################################
