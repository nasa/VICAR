$!****************************************************************************
$!
$! Build proc for MIPL module zfill
$! VPACK Version 1.8, Friday, April 06, 2001, 14:20:14
$!
$! Execute by entering:		$ @zfill
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
$ write sys$output "*** module zfill ***"
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
$ write sys$output "Invalid argument given to zfill.com file -- ", primary
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
$   if F$SEARCH("zfill.imake") .nes. ""
$   then
$      vimake zfill
$      purge zfill.bld
$   else
$      if F$SEARCH("zfill.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zfill
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zfill.bld "STD"
$   else
$      @zfill.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zfill.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zfill.com -
	-s zfill.f -
	-p zfill.pdf -
	-i zfill.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zfill.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     VICAR PROGRAM ZFILL      S.Z. FRIEDMAN    MARCH 1983
C     MODIFIED FOR VAX CONVERSION BY ASM, 6 SEPT 1983
C     MODIFIED TO VICAR2 AND SPARSE/DENSE ALGORITHMS ADDED BY REA, 20 MAY 1986
C     MODIFIED to UNIX/VICAR BY REA, 3 SEPTEMBER 1991
C     CHANGE 'MAIN' SUBROUTINE NAME TO 'WORK' - REA, 6 APR 2001
C**********************************************************************
      SUBROUTINE MAIN44
C
C  PROGRAM ZFILL USES A WINDOW OF NLW LINES BY NSW SAMPLES TO
C  FILL IN VOID AREAS OF AN IMAGE.  VOID AREAS ARE ASSUMED
C  TO BE ZERO DN, BUT CAN BE RESPECIFIED BY PARAMETER.  VOIDS ARE
C  FILLED WITH THE MEAN VALUE OF ALL 'NON-VOID DN' PIXELS IN
C  THE WINDOW.  A MASK SHOWING FILLED PIXELS CAN BE
C  GENERATED UPON REQUEST
C
C
C  TAE STATEMENT FORM:
C     ZFILL IN OUT SIZE=(SL,SS,NL,NS) PARAMETERS       OR
C     ZFILL IN (OUT,MASK) SIZE=(SL,SS,NL,NS) PARAMETERS
C
C  PARAMETERS:
C
C     NLW=n            NUMBER OF LINES IN WINDOW (DEFAULT=3)
C
C     NSW=m            NUMBER OF SAMPLES IN WINDOW (DEFAULT=3)
C
C     REPLACE=n        DEFINES DN(N) TO BE THE 'VOID' DN.  ALL PIXELS
C                      OF VALUE N WILL BE REPLACED BY THE MEAN OF ALL
C                      'NON-N' VALUES IN THE WINDOW.  (DEFAULT=0)
C
C     EXCLUDE=n        DN N WILL NOT BE USED IN THE INTERPOLATION
C                      OF VALUE FOR REPLACEMENT DN.  ITS FUNCTION
C                      IS SIMILAR TO AN AREA MASK OR BARRIER.
C                      (NO DEFAULT)
C
C     DENSE	       INVOKES THE ALGORITHM FOR USE WHEN THE INPUT
C		       IS MOSTLY FULL.
C
	EXTERNAL WORK
	LOGICAL XVPTST,QDENSE/.FALSE./,QMASK/.FALSE./
	COMMON /QQ/ ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IREPLACE,IEXCLUDE,
     +		    INUNIT,IOUTUNIT,MASKUNIT,QDENSE,QMASK 
C
	CALL XVMESSAGE('ZFILL Version 6-April-2001',' ')
C							 call for parameters
	CALL XVPARM('NLW',NLW,ICNT,IDEF,0)
	CALL XVPARM('NSW',NSW,ICNT,IDEF,0)
	CALL XVPARM('REPLACE',IREPLACE,ICNT,IDEF,0)
	CALL XVPARM('EXCLUDE',IEXCLUDE,ICNT,IDEF,0)
	QDENSE = XVPTST('DENSE')
C								open datasets
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF','OP','WRITE',' ')
	CALL XVPCNT('OUT',NOUT)
	IF (NOUT.EQ.2) THEN
	    QMASK = .TRUE.
	    CALL XVUNIT(MASKUNIT,'OUT',2,ISTAT,' ')
	    CALL XVOPEN(MASKUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','BYTE','O_FORMAT','BYTE','OP','WRITE',' ')
	END IF
C					     check parameters, adjust if needed
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (ISL+NL-1 .GT. NLIN) NL=NLIN-ISL+1
	IF (ISS+NS-1 .GT. NSIN) NS=NSIN-ISS+1
	IF (NL.LE.0 .OR. NS.LE.0) THEN
	    CALL XVMESSAGE(' Size field error',' ')
	    CALL ABEND
	ENDIF
	NLW = 2*(NLW/2) + 1
	NSW = 2*(NSW/2) + 1
C		           determine buffer sizes and call work through stacka
	N1 = NS+NSW-1
	N2 = NLW
	INSIZE = 2*N1*N2
	IOUTSIZE = 2*NS
	IF (QMASK) THEN
	    MSIZE = NS
	ELSE
	    MSIZE = 4
	END IF
	CALL STACKA(7,WORK,3,MSIZE,INSIZE,IOUTSIZE,N1,N2)
	RETURN
	END
C
C**********************************************************************
C
	SUBROUTINE WORK(MBUF,MSIZE,INBUF,INSIZE,OUTBUF,IOUTSIZE,N1,N2)
C
C  VICAR PROGRAM ZFILL      S.Z. FRIEDMAN    MARCH 1983
C
C  SUBROUTINE WORK HANDLES ALL I/O, CALLS MEAN ANALYSIS ROUTINE
C
C
C     MBUF    OPTIONAL ARRAY FOR MASK OUTPUT DS FOR LINE N
C     INBUF   INPUT BUFFER
C     OUTBUF  OUTPUT ARRAY FOR LINE N
C
	CHARACTER*80 PRT
	INTEGER*2 INBUF(N1,N2),OUTBUF(NS)
	BYTE MBUF(MSIZE)
	LOGICAL QDENSE,QMASK
	COMMON /QQ/ ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IREPLACE,IEXCLUDE,
     +		    INUNIT,IOUTUNIT,MASKUNIT,QDENSE,QMASK
C
C							label processing
C
	IF (IEXCLUDE.NE.-32768) THEN
	    WRITE(PRT,10) NLW,NSW,IREPLACE,IEXCLUDE
   10	    FORMAT(' ZFILL:  NLW =',I3,', NSW =',I3,', REPLACE =',I5,
     +		   ', EXCLUDE =',I6)
	ELSE
	    WRITE(PRT,20) NLW,NSW,IREPLACE
   20	    FORMAT(' ZFILL:  NLW =',I3,', NSW =',I3,', REPLACE =',I5)
	END IF
	CALL XLADD(IOUTUNIT,'HISTORY','PARMS',PRT,ISTAT,'FORMAT',
     +		   'STRING',' ')
	CALL XVMESSAGE(PRT,' ')
C
	IF(QMASK) THEN
	    CALL XLADD(MASKUNIT,'HISTORY','PARMS',PRT,ISTAT,'FORMAT',
     +		       'STRING',' ')
	    CALL XLADD(MASKUNIT,'HISTORY','MASK',
     +		       ' 0=CHANGED   255=NO CHANGE',ISTAT,'FORMAT',
     +		       'STRING',' ')
	END IF
C
C					Read in initial lines; set pointers
C
	NLW2 = NLW/2
	NSW2 = NSW/2
	LINE1 = MAX(1,ISL-NLW2)				! First line to be read
	LINEN = MIN(NLIN,ISL+NLW2)			! Last line to be read
	ISAMP1 = MAX(1,ISS-NSW2)			! First sample read
	ISAMPN = MIN(NSIN,ISS+NS-1+NSW2)		! Last sample read
	NSAMPS = ISAMPN-ISAMP1+1			! Number of samps read
	LOCL = MAX(1,NLW2-ISL+2)		! Loc of 1st line in INBUF
	LOCS = MAX(1,NSW2-ISS+2)		! Loc of 1st samp in INBUF
C
C					Initialize all values in INBUF to the
C					excluded value
	DO I=1,N2
	    DO J=1,N1
		INBUF(J,I) = IEXCLUDE
	    END DO
	END DO
C						Read input to fill INBUF
C
	DO I=LINE1,LINEN
	    CALL XVREAD(INUNIT,INBUF(LOCS,LOCL),ISTAT,'LINE',I,
     +			'SAMP',ISAMP1,'NSAMPS',NSAMPS,' ')
	    LOCL = LOCL+1
	END DO
	LINELOC = NLW2
	LINEN = LINEN+1
	LOCL = 1
	IF (QDENSE) THEN
C
C **********************************************************    DENSE ALGORITHM
C
	    DO I=1,NL
		IF (QMASK) CALL ITLA(255,MBUF,NS)
		LINELOC = LINELOC+1
		IF (LINELOC.GT.NLW) LINELOC=1
	    	ISAMPLOC = NSW2
C							      check each sample
	    	DO J=1,NS
		    ISAMPLOC = ISAMPLOC+1
		    IF (INBUF(ISAMPLOC,LINELOC).EQ.IREPLACE) THEN
C
C						      compute replacement value
		        ISUM = 0
		        NPTS = 0
		        DO K=1,NLW
			    DO L=ISAMPLOC-NSW2,ISAMPLOC+NSW2
			    	IF(INBUF(L,K).NE.IREPLACE .AND.
     +			       	   INBUF(L,K).NE.IEXCLUDE)      THEN
				    ISUM = ISUM+INBUF(L,K)
				    NPTS = NPTS+1
			    	END IF
			    END DO
		    	END DO
		    	IF (NPTS.NE.0) THEN
			    OUTBUF(J) = FLOAT(ISUM)/FLOAT(NPTS)+0.5
			    IF (QMASK) MBUF(J) = 0
		    	ELSE					! nothing in
			    OUTBUF(J) = IREPLACE		! window, leave
		    	END IF					! as is
		    ELSE
		    	OUTBUF(J) = INBUF(ISAMPLOC,LINELOC)	! replacement
		    END IF					! not needed
	    	END DO
	    	CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')		!write out line
	    	IF (QMASK) CALL XVWRIT(MASKUNIT,MBUF,ISTAT,' ')
C								! read new line
	    	IF (LINEN.LE.NLIN) THEN
		    CALL XVREAD(INUNIT,INBUF(LOCS,LOCL),ISTAT,'LINE',
     +				LINEN,'SAMP',ISAMP1,'NSAMPS',NSAMPS,' ')
	        ELSE
		    DO J=1,N1
		    	INBUF(J,LOCL) = IEXCLUDE
		    END DO
	    	END IF
	    	LINEN = LINEN+1
	    	LOCL = LOCL+1
	    	IF (LOCL.GT.NLW) LOCL=1
	    END DO
	ELSE
C
C*********************************************************** SPARSE ALGORITHM
C
	    DO II=1,NL
C						    initialize ISUM and NPTS
		ISUM = 0
		NPTS = 0
		DO I=1,NLW
		    DO J=1,NSW
		        IF (INBUF(J,I).NE.IREPLACE .AND.
     +			    INBUF(J,I).NE.IEXCLUDE)		THEN
			    ISUM = ISUM+INBUF(J,I)
			    NPTS = NPTS+1
			END IF
		    END DO
		END DO
		ISAMPL = 1
		ISAMPR = NSW+1
C							set pointers
		IF (QMASK) CALL ITLA(255,MBUF,NS)
		LINELOC = LINELOC+1
		IF (LINELOC.GT.NLW) LINELOC=1
	    	ISAMPLOC = NSW2
C							check each sample
	    	DO J=1,NS
		    ISAMPLOC = ISAMPLOC+1
		    IF (INBUF(ISAMPLOC,LINELOC).EQ.IREPLACE) THEN
		    	IF (NPTS.NE.0) THEN		   ! compute new value
			    OUTBUF(J) = FLOAT(ISUM)/FLOAT(NPTS)+0.5
			    IF (QMASK) MBUF(J) = 0
		    	ELSE
			    OUTBUF(J) = IREPLACE	   ! nothing in window;
		    	END IF				   ! leave as is
		    ELSE
		    	OUTBUF(J) = INBUF(ISAMPLOC,LINELOC)	! don't replace
		    END IF
C							update ISUM and NPTS
		    IF (J.NE.NS) THEN
			DO K=1,NLW
			    IF(INBUF(ISAMPL,K).NE.IREPLACE .AND.
     +			       INBUF(ISAMPL,K).NE.IEXCLUDE)	THEN
				ISUM = ISUM-INBUF(ISAMPL,K)
				NPTS = NPTS-1
			    END IF
			    IF(INBUF(ISAMPR,K).NE.IREPLACE .AND.
     +			       INBUF(ISAMPR,K).NE.IEXCLUDE)	THEN
				ISUM = ISUM+INBUF(ISAMPR,K)
				NPTS = NPTS+1
			    END IF
			END DO
			ISAMPL = ISAMPL+1
			ISAMPR = ISAMPR+1
		    END IF
	    	END DO
C							     write out results
	    	CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')
	    	IF (QMASK) CALL XVWRIT(MASKUNIT,MBUF,ISTAT,' ')
C							     read new line into
C							     INBUF
	    	IF (LINEN.LE.NLIN) THEN
		    CALL XVREAD(INUNIT,INBUF(LOCS,LOCL),ISTAT,'LINE',
     +				LINEN,'SAMP',ISAMP1,'NSAMPS',NSAMPS,' ')
	        ELSE
		    DO J=1,N1
		    	INBUF(J,LOCL) = IEXCLUDE
		    END DO
	    	END IF
	    	LINEN = LINEN+1
	    	LOCL = LOCL+1
	    	IF (LOCL.GT.NLW) LOCL=1
	    END DO
	END IF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create zfill.pdf
process help=*
 PARM 	INP 	TYPE=(STRING,40)
 PARM 	OUT 	TYPE=(STRING,40) 	COUNT=1:2
 PARM 	SIZE 	TYPE=INTEGER 	COUNT=4 	DEFAULT=(1,1,0,0)
 PARM 	SL 	TYPE=INTEGER 			DEFAULT=1
 PARM 	SS 	TYPE=INTEGER 			DEFAULT=1
 PARM 	NL 	TYPE=INTEGER 			DEFAULT=0
 PARM 	NS 	TYPE=INTEGER 			DEFAULT=0
 PARM 	NLW 	TYPE=INTEGER 			DEFAULT=3
 PARM 	NSW 	TYPE=INTEGER 			DEFAULT=3
 PARM 	REPLACE TYPE=INTEGER 			DEFAULT=0
 PARM 	EXCLUDE TYPE=INTEGER 			DEFAULT=-32768
 PARM 	MODE	TYPE=KEYWORD	COUNT=(0:1)	DEFAULT="SPARSE" +
				VALID=("SPARSE","DENSE")
 END-PROC
.TITLE
 ZFILL
.HELP
 PURPOSE:
      ZFILL uses a window of NLW lines by NSW samples to fill in void areas
 of an image.  Void areas are assumed to be zero DN, but can be respecified
 by parameter.  Voids are filled with the mean value of all 'non-void DN'
 pixels in the window.  A mask showing filled pixels can be generated on 
 request.
      The user can select between two algorithms in ZFILL. The results of the
 algorithms are identical, but computation times vary dramatically. The
'sparse' algorithm is faster if the input image is mostly empty; the 'dense'
algorithm is faster if the input image is mostly complete.
.PAGE
 EXECUTION:

 Examples

	ZFILL A B 'DENSE

	This command will use the default window (3 by 3) to scan
	input image A for pixels with value 0.  For each such pixel,
	the average of the surrounding pixels will be calculated, and 
	substituted for the original value. The 'dense' algorithm has
	been chosen, for the input has only a few 0 values to be replaced.

	ZFILL A B REPL=10 EXCL=8

	This command will use the default window (3 by 3) to scan
	input image A for pixels with value 10.  For each such pixel,
	the average of the surrounding pixels (excluding those valued 8)
	will be calculated, and substituted for the original value.

	ZFILL A (B,MSK)  NLW=2  NSW=5

	This command will scan image A for occurrences of 0-valued pixels
	(zero is the default value for replacement).  These will again
	be replaced by the averages of the surrounding values.  Note
	the window size which is specified to be 2 by 5.  Even dimensions
	are automatically increased to the next odd integer, so the
	window size which will be actually used is NLW=3, NSW=5.
	Note that the command above produces an mask showing which
	pixels were changed; mask pixels have DN 0 if they were changed
	in the output image, and 255 otherwise.

.PAGE

 OPERATION:

      ZFILL contains two algorithms. In the 'sparse' (default) mode, the 
  data lines needed for the window of the first pixel are read in and averaged;
  after this point, as the window moves across the image, the left (and top) 
  values which move out of the window are subtracted from the accumulating
  variables, and the new pixels appearing at the bottom and right of the window
  are added in.  Any time a pixel needs to be replaced, the average may be 
  easily calculated from these variables.
      In the 'dense' mode, pixels are checked for replacement first; the
 replacement value is computed only if needed. The entire window must be 
 examined to compute the replacement value each time a pixel is replaced.
      As a rule of thumb, the default (sparse) algorithm is faster if the
 fraction of pixels to be replaced is more than 2/NSW. The 'dense' algorithm
 is faster otherwise.
      When the filling window extends beyond the image (not image window)
 boundaries, the outside pixels are treated as excluded values.

 WRITTEN BY:  S. Z. Friedman, March 1983
 COGNIZANT PROGRAMMER:  Ron Alley
 REVISION:  1 20 May 1986

.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE OUT
Output image file(s)
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE NLW
Window length (lines)
.VARIABLE NSW
Window width (samples)
.VARIABLE REPLACE
DN value to replace
.VARIABLE EXCLUDE
DN value to exclude
.VARIABLE MODE
Valid: SPARSE, DENSE
.LEVEL2
.VARIABLE NLW
 The length in lines of the window used to scan the input image.
.VARIABLE NSW
 The width in pixels of the window used to scan the input image.
.VARIABLE REPLACE
 The value specified by REPL will be replaced with the average of the
 other pixels within the window.
.VARIABLE EXCLUDE
 The value specified by EXCL will be ignored in the calculation of the
 average of the pixels within a given window.
.VARIABLE MODE
 Two algorithms (MODEs) are available.  Both produce the same results,
 but their speeds differ, depending upon the number of pixels to be
 replaced.  As a rule of thumb, use the dense algorithm if the fraction 
 of pixels to be replaced is less than 2/NSW, and use the sparse algorithm
 otherwise.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create zfill.imake
#define  PROGRAM   zfill

#define MODULE_LIST zfill.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
