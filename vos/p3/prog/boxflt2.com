$!****************************************************************************
$!
$! Build proc for MIPL module boxflt2
$! VPACK Version 1.8, Friday, April 04, 2003, 18:43:51
$!
$! Execute by entering:		$ @boxflt2
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
$ write sys$output "*** module boxflt2 ***"
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
$ write sys$output "Invalid argument given to boxflt2.com file -- ", primary
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
$   if F$SEARCH("boxflt2.imake") .nes. ""
$   then
$      vimake boxflt2
$      purge boxflt2.bld
$   else
$      if F$SEARCH("boxflt2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake boxflt2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @boxflt2.bld "STD"
$   else
$      @boxflt2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create boxflt2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack boxflt2.com -
	-s boxflt2.f -
	-i boxflt2.imake -
	-p boxflt2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create boxflt2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C**********************************************************************
	SUBROUTINE MAIN44
C
C        MODIFIED FOR VAX CONVERSION BY ALAN MAZER 28-JUL-83
C        CONVERTED TO VICAR2 BY J. REIMER 14-AUG-85
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C        5-91  REA  REWRITTEN
C	 4-03  REA  EXTEND keyword added
C
	EXTERNAL LOW,HIGH
	CHARACTER*4 FORMAT
	LOGICAL XVPTST,QLCYCLE,QSCYCLE,QEXTEND,QINT
	COMMON /BXFLT2/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,
     +			DC,XMIN,XMAX,QLCYCLE,QSCYCLE,QEXTEND,QINT
C
C						open datasets, check size field
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +		  'IO_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF(ISL+NL-1 .GT. NLIN) THEN
	    CALL XVMESSAGE(
     +              ' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
	    CALL ABEND
	ENDIF
	IF(ISS+NS-1 .GT. NSIN) THEN
	    CALL XVMESSAGE(
     +          ' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
	    CALL ABEND
	ENDIF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &		  'OPEN_ACT','SA','IO_ACT','SA','U_NL',NL,'U_NS',NS,' ')
C
C							      process parameters
 	CALL XVP('DCLEVEL',DC,ICOUNT)
	CALL XVP('NLW',NLW,ICOUNT)
	CALL XVP('NSW',NSW,ICOUNT)
	IF (XVPTST('CYCLE')) THEN
	    QLCYCLE = .TRUE.
	    QSCYCLE = .TRUE.
	ELSE
	    QLCYCLE = XVPTST('LCYCLE')
	    QSCYCLE = XVPTST('SCYCLE')
	END IF
	QEXTEND = XVPTST('EXTEND')
C
	IF(NLW/2*2.EQ.NLW) CALL XVMESSAGE(
     +                            ' WARNING-NLW IS AN EVEN INTEGER',' ')
	IF(NSW/2*2.EQ.NSW) CALL XVMESSAGE(
     +                            ' WARNING-NSW IS AN EVEN INTEGER',' ')
C
C							determine cutoff values
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FORMAT,' ')
	IF (FORMAT .EQ. 'BYTE') THEN
	    XMIN = 0.0
	    XMAX = 255.0
	    QINT = .TRUE.
	ELSE IF (FORMAT .EQ. 'HALF') THEN
	    XMIN = -32768.0
	    XMAX = 32767.0
	    QINT = .TRUE.
	ELSE IF (FORMAT .EQ. 'FULL') THEN
	    XMIN = -2147483000
	    XMAX =  2147483000
	    QINT = .TRUE.
	ELSE
	    XMIN = -1.0E33
	    XMAX =  1.0E33
	    QINT = .FALSE.
	END IF
C
	NSX = NS + NSW
	NLWX = NLW
	NSO = NS
	N1 = 4*NLWX*NSX				! bytes in input array
	N2 = 4*NSO				! bytes in output array
C
	IF (XVPTST('HIGHPASS')) THEN
	    CALL STACKA(7,HIGH,2,N1,N2,NSX,NLWX,NSO)
	ELSE
	    CALL STACKA(7,LOW,2,N1,N2,NSX,NLWX,NSO)
	END IF
C
	CALL XVCLOSE(IUNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
	RETURN
	END
C**********************************************************************
	SUBROUTINE LOW(BUF,N1,OUT,N2,NSX,NLWX,NSO)
C
	REAL BUF(NSX,NLWX),OUT(NSO)
	LOGICAL QLCYCLE,QSCYCLE,QEXTEND,QINT
	COMMON /BXFLT2/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,
     +			DC,XMIN,XMAX,QLCYCLE,QSCYCLE,QEXTEND,QINT
C
	PIXELS = NLW*NSW		! number of pixels in filter window
	NLW2 = NLW/2			! half of the line weights
	NSW2 = NSW/2			! half of the sample weights
	IST = MAX(ISS-NSW2,1)		! first sample to be read in
	LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
	NLEFT = IST - ISS + NSW2	! number of pixels to pad on left
	NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
	NSREAD = LAST - IST + 1		! number of samples to be read
	IEND = NLEFT + NSREAD		! location of final pixel in inp buffer
	LOC = NLW			! line position in input buffer
C
C						Set up input buffer
	DO LINE = ISL+NLW2, ISL-NLW2, -1
	    IF (LINE .GE. 1) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       NLIN+LINE,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE IF (QEXTEND) THEN
			CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW2+1),BUF(1,LOC),1,1)
		ELSE
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC - 1
	END DO
C
	LOC = 1					!location for next line into BUF
	LINE = ISL + NLW2 +1			!line number of next line
C						   looping thru all output lines
	DO L=1,NL
C							   initialize window sum
	    SUM = 0.0
	    DO I=1,NLW
		DO J=1,NSW
		    SUM = SUM + BUF(J,I)
		END DO
	    END DO
C						    compute value for each pixel
	    DO ISAMP=1,NS
		OUT(ISAMP) = SUM/PIXELS
		IF (QINT) OUT(ISAMP) = NINT(OUT(ISAMP))
		DO I=1,NLW
		    SUM = SUM + BUF(ISAMP+NSW,I) - BUF(ISAMP,I)
		END DO
	    END DO
	    CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	    IF (LINE .LE. NLIN) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       LINE-NLIN,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    LOC2 = LOC - 1
		    IF (LOC2 .LT. 1) LOC2 = NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		ELSE
		    LOC2 = LOC - (LINE - NLIN)
		    IF (LOC2 .LT. 1) LOC2=LOC2+NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC + 1
	    IF (LOC .GT. NLW) LOC = 1
	    LINE = LINE + 1
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE HIGH(BUF,N1,OUT,N2,NSX,NLWX,NSO)
C
	REAL BUF(NSX,NLWX),OUT(NSO)
C
	LOGICAL QLCYCLE,QSCYCLE,QEXTEND,QINT
	COMMON /BXFLT2/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,
     +			DC,XMIN,XMAX,QLCYCLE,QSCYCLE,QEXTEND,QINT
C
	PIXELS = NLW*NSW		! number of pixels in filter window
	NLW2 = NLW/2			! half of the line weights
	NSW2 = NSW/2			! half of the sample weights
	IST = MAX(ISS-NSW2,1)		! first sample to be read in
	LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
	NLEFT = IST - ISS + NSW2	! number of pixels to pad on left
	NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
	NSREAD = LAST - IST + 1		! number of samples to be read
	IEND = NLEFT + NSREAD		! location of final pixel in inp buffer
	LOC = NLW			! line position in input buffer
C
C						Set up input buffer
	DO LINE = ISL+NLW2, ISL-NLW2, -1
	    IF (LINE .GE. 1) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       NLIN+LINE,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE IF (QEXTEND) THEN
			CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW2+1),BUF(1,LOC),1,1)
		ELSE
		    CALL MVE(7,NS+NSW-1,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC - 1
	END DO
C
	LOC = 1					!location for next line into BUF
	LINE = ISL + NLW2 +1			!line number of next line
	INLOC = NLW2 + 1			!location of current line in BUF
C						   looping thru all output lines
	DO L=1,NL
C							   initialize window sum
	    SUM = 0.0
	    DO I=1,NLW
		DO J=1,NSW
		    SUM = SUM + BUF(J,I)
		END DO
	    END DO
C						    compute value for each pixel
	    DO ISAMP=1,NS
		OUT(ISAMP) = BUF(NSW2+ISAMP,INLOC) - SUM/PIXELS + DC
		OUT(ISAMP) = MIN(XMAX, MAX(XMIN, OUT(ISAMP)))
		IF (QINT) OUT(ISAMP) = NINT(OUT(ISAMP))
		DO I=1,NLW
		    SUM = SUM + BUF(ISAMP+NSW,I) - BUF(ISAMP,I)
		END DO
	    END DO
	    CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	    IF (LINE .LE. NLIN) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE IF (QEXTEND) THEN
		    CALL EXTEND(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       LINE-NLIN,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
		ELSE IF (QEXTEND) THEN
		    LOC2 = LOC - 1
		    IF (LOC2 .LT. 1) LOC2 = NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		ELSE
		    LOC2 = LOC - (LINE - NLIN)
		    IF (LOC2 .LT. 1) LOC2=LOC2+NLW
		    CALL MVE(7,NS+NSW-1,BUF(1,LOC2),BUF(1,LOC),1,1)
		END IF
	    END IF
	    LOC = LOC + 1
	    IF (LOC .GT. NLW) LOC = 1
	    INLOC = INLOC +1
	    IF (INLOC .GT. NLW) INLOC = 1
	    LINE = LINE + 1
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE CYCLE(BUF,NLEFT,NRIGHT,LEN)
C
	REAL BUF(LEN)
C
	N = LEN - NLEFT - NRIGHT
	DO I=1,NLEFT
	    BUF(I) = BUF(I+N)
	END DO
	DO I=LEN-NRIGHT+1, LEN
	    BUF(I) = BUF(I-N)
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE REFLCT(BUF,NLEFT,NRIGHT,LEN)
C
	REAL BUF(LEN)
C
	DO I=1,NLEFT
	    BUF(I) = BUF(2*NLEFT-I+1)
	END DO
	DO I=1,NRIGHT
	    BUF(LEN-I+1) = BUF(LEN-2*NRIGHT+I)
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE EXTEND(BUF,NLEFT,NRIGHT,LEN)
C
	REAL BUF(LEN)
C
	DO I=1,NLEFT
	    BUF(I) = BUF(NLEFT+1)
	END DO
	DO I=1,NRIGHT
	    BUF(LEN-I+1) = BUF(LEN-NRIGHT)
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create boxflt2.imake
#define  PROGRAM   boxflt2

#define MODULE_LIST boxflt2.f

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
$ create boxflt2.pdf
process help=*
PARM INP TYPE=(STRING,60)
PARM OUT TYPE=(STRING,60)
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM NSW TYPE=INTEGER DEFAULT=11
PARM NLW TYPE=INTEGER DEFAULT=11
PARM FILTER TYPE=KEYWORD VALID=(HIGHPASS,LOWPASS) DEFAULT=LOWPASS
PARM DCLEVEL TYPE=REAL DEFAULT=128.0
PARM EDGE TYPE=KEYWORD VALID=(CYCLE,LCYCLE,SCYCLE,EXTEND,REFLECT) +
  DEFAULT=REFLECT
END-PROC
.TITLE
BOXFLT2
.HELP
PURPOSE:
BOXFLT2 applies a low-pass filter to an input image by taking the local
mean of all pixels contained within a prescribed window centered at
each pixel of the input image.  This mean then replaces the input value.
A highpass option is available which replaces each input value with the
difference between the input and the local mean, plus a constant DC-level
offset.

EXECUTION:

Examples
	BOXFLT2  INP  OUT  NLW=21  NSW=451

	This example performs a lowpass filter of size 451 samples by 21
	lines on the input. Reflection is performed at image boundaries.

	BOXFLT2  INP  OUT  NLW=101  NSW=1  'HIGHPASS  DCLEVEL=90  'CYCLE

	This examples performs a highpass filter of size 101 lines by 1
	sample with an output DCLEVEL of 90, performing cycling at the
	image boundaries.  (The omitted keywords are FILTER and EDGE,
	respectively.)

.PAGE	
Modes of handling boundaries:
		a = pixel (1,1)		b = pixel (1,NS)
		c = pixel (NL,1)	d = pixel (NL,NS)

	+-------+-------+-------+	+-------+-------+-------+
	| d   c | c   d | d   c |	| a   b | a   b | a   b |
	|       |       |       |	|       |       |       |
	| b   a | a   b | b   a |	| c   d | c   d | c   d |
	|-------|-------|-------|	|-------|-------|-------|
	| b   a | a   b | b   a |	| a   b | a   b | a   b |
	|       |       |       |	|       |       |       |
	| d   c | c   d | d   c |	| c   d | c   d | c   d |
	|-------|-------|-------|	|-------|-------|-------|
	| d   c | c   d | d   c |	| a   b | a   b | a   b |
	|       |       |       |	|       |       |       |
	| b   a | a   b | b   a |	| c   d | c   d | c   d |
	+-------+-------+-------+	+-------+-------+-------+
		REFLECTING			 CYCLING

	+-------+-------+-------+
	| a   a | a   b | b   b |
	|       |       |       |
	| a   a | a   b | b   b |
	|-------|-------|-------|
	| a   a | a   b | b   b |
	|       |       |       |
	| c   c | c   d | d   d |
	|-------|-------|-------|
	| c   c | c   d | d   d |
	|       |       |       |
	| c   c | c   d | d   d |
	+-------+-------+-------+
		EXTENDING
.PAGE
OPERATION:
BOXFLT2 performs a lowpass filter operation by taking the local mean of
all pixels contained within a prescribed window of NLW by NSW dimensions
centered at each pixel of the input image.  This mean then replaces the 
input value.  If the HIGHPASS option is specified, then the difference
between the input and the local mean plus a constant DC-level offset
replaces the input value. 

BOXFLT2 provides the user with the choice of using "reflection", "cycling"
(wrap-around), or "extending" (repeat the edge pixel) at image boundaries.
In the default case, image reflection is used, and may be depicted as above
in the EXECUTION section.  If cycling is desired, where the left boundary of
the image is equivalent to the right boundary, and the upper boundary is
equivalent to the lower boundary.

WRITTEN BY:  W. D. Benton, 1 June 1976
COGNIZANT PROGRAMMER:  Ron Alley
REVISION:  Version 2.1  April 4, 2003
.LEVEL1
.VARIABLE INP
Input dataset
.VARIABLE OUT
Output dataset
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
.VARIABLE NSW
Filter width in pixels
.VARIABLE NLW
Filter length in pixels
.VARIABLE FILTER
Selects type of filtering 
Valid: LOWPASS, HIGHPASS
.VARIABLE DCLEVEL
Highpass constant
.VARIABLE EDGE
Method of handling edges 
REFLECT, EXTEND,
CYCLE, LCYCLE, SCYCLE
.LEVEL2
.VARIABLE NSW
NSW is the width in pixels of the box filter.  It must be less than
twice the image width in pixels and defaults to 11.
.VARIABLE NLW
NLS is the length in lines of the box filter.  It must be less than
twice the image length in pixels and defaults to 11.
.VARIABLE FILTER
FILTER=HIGHPASS specifies that the output is to be the highpass, rather than
the lowpass, version of the input, i.e., OUT = IN - LOW + DCLEVEL.
The default is lowpass filtering.
.VARIABLE DCLEVEL
Specifies (for highpass filter) the constant to be added to the 
difference (IN-LOW) in the highpass output image.  Default is 128.
.VARIABLE EDGE
Specifies image handling at image boundaries.  Setting EDGE=CYCLE or 'CYCLE
causes the program to treat the image as if it wrapped around at boundaries
in both directions.  'LCYCLE and 'SCYCLE cause wrap-around in the line and
sample direction only, respectively.  'EXTEND causes the edge pixel to be
repeated for all locations beyond the boundaries. The default is for the 
program to reflect the image at the boundaries.
.END
$ Return
$!#############################################################################
