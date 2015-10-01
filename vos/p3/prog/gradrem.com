$!****************************************************************************
$!
$! Build proc for MIPL module gradrem
$! VPACK Version 1.8, Tuesday, June 03, 1997, 14:39:45
$!
$! Execute by entering:		$ @gradrem
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
$ write sys$output "*** module gradrem ***"
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
$ write sys$output "Invalid argument given to gradrem.com file -- ", primary
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
$   if F$SEARCH("gradrem.imake") .nes. ""
$   then
$      vimake gradrem
$      purge gradrem.bld
$   else
$      if F$SEARCH("gradrem.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gradrem
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gradrem.bld "STD"
$   else
$      @gradrem.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gradrem.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gradrem.com -
	-s gradrem.f -
	-p gradrem.pdf -
	-i gradrem.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gradrem.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C        MODIFIED FOR VAX CONVERSION BY ASM, 6 SEPT 83
C        CONVERTED TO VICAR2 BY JHR  26 AUG 85
C        CORRECTED DIVIDE BY 0 IN GAIN COMPUTATION   SP 19 MAY 86
C	 ADD MEANSIG OPTION; ALLOW NON-BYTE FORMATS  REA 1 JUNE 87
C	 CONVERTED TO UNIX/VICAR                     REA 30 APRIL 91
C**********************************************************************
	SUBROUTINE MAIN44
	EXTERNAL MODE0,MODE1,MODE4
	REAL XMS(2)
	CHARACTER*8 FMT
	LOGICAL*4 XVPTST
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C							     open input data set
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL',
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
C						  get size information and check
	CALL XVSIZE(ISL,ISS,NLO,NSO,NLI,NSI)
	IF(ISL+NLO-1 .GT. NLI) THEN
	    CALL XVMESSAGE(
     +		 'number of lines requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
	IF(ISS+NSO-1 .GT. NSI) THEN
	    CALL XVMESSAGE(
     +		   'number of samples requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FMT,'PIX_SIZE',IFMT,' ')
	IF (FMT.EQ.'REAL') IFMT=7
C
C        OPEN OUTPUT DATA SET
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &		'OPEN_ACT','SA','IO_ACT','SA','U_NL',NLO,'U_NS',NSO,' ')
C
C							      process parameters
C									START
	CALL XVPARM('START',IPARM,ICOUNT,IDEF,1)
	IF (ICOUNT.NE.0) THEN
	    ISTART = IPARM
	ELSE
	    ISTART = ISL
	END IF
C									LENGTH
	CALL XVPARM('LENGTH',IPARM,ICOUNT,IDEF,1)
	IF (ICOUNT.NE.0) THEN
	    IEND = ISTART+IPARM-1
	ELSE
	    IEND = ISL+NLO-1
	END IF
C									LINC
	CALL XVPARM('LINC',LINC,ICOUNT,IDEF,1)
C									FILTER
	CALL XVPARM('FILTER',IPARM,ICOUNT,IDEF,1)
	IF (ICOUNT.NE.0) THEN
	    NSW = IPARM
	ELSE
	    NSW = 1
	END IF
C									PERCENT
	CALL XVPARM('PERCENT',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.NE.0) THEN
	    MODE = 1
	    SAT = RPARM
	    IF (IFMT.NE.1) THEN
		CALL XVMESSAGE(
     +	      ' Only byte data is permitted with the PERCENT option',' ')
		CALL ABEND
	    ENDIF
	ELSE
	    MODE = 0
	END IF
C									GAIN
	CALL XVPARM('GAIN',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.NE.0) THEN
	    MODE = 2
	    GFAC = RPARM
	ELSE
	    GFAC = 100.0
	END IF
C									OFF
	CALL XVPARM('OFF',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.NE.0) THEN
	    MODE = 2
	    OFF = RPARM
	ELSE
	    OFF = 0.0
	END IF
C									NOSAT
	IF(XVPTST('NOSAT')) MODE=3
C									MEANSIG
	CALL XVPARM('MEANSIG',XMS,ICOUNT,IDEF,2)
	IF (ICOUNT.EQ.2) MODE=4
C									NOPRINT
	QPR = .NOT. XVPTST('NOPRINT')
C					compute lengths of arrays,  call STACKA
	II = 4*NSO
	JJ = II
	KK = II
	IF (MODE.EQ.1) THEN
	    CALL STACKA(6,MODE1,3,II,JJ,KK,QPR)
	ELSE IF (MODE.EQ.4) THEN
	    LL = 8*NSO
	    CALL STACKA(8,MODE4,4,II,JJ,KK,LL,QPR,XMS)
	ELSE
	    CALL STACKA(7,MODE0,3,II,JJ,KK,QPR,MODE)
	END IF
	RETURN
	END
C***********************************************************************
	SUBROUTINE MODE0(SUM,II,BUF,JJ,GAIN,KK,QPR,MODE)
C
	REAL GAIN(NSO),SUM(NSO),BUF(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C					   compute average DN for each sample
	CALL ZIA(SUM,NSO)
	NUM = 0
	DO I=ISTART,IEND,LINC
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     &			'NSAMPS',NSO,' ')
	    NUM = NUM+1
	    DO J=1,NSO
		SUM(J) = SUM(J)+BUF(J)
	    END DO
	END DO
	POP = NUM*NSO
	X = NUM
	GRANDSUM = 0.0
	DO I=1,NSO
	    GRANDSUM = GRANDSUM+SUM(I)
	    GAIN(I) = SUM(I)/X
	END DO
	XMEAN = GRANDSUM/POP
	CALL XVMESSAGE(' ',' ')
	WRITE(OUT,100) XMEAN
  100	FORMAT(' MEAN =',F10.3)
	CALL XVMESSAGE(OUT,' ')
C				   if necessary,  compute gain factor and offset
	IF(MODE.EQ.0) GFAC=XMEAN
	IF(NSW.NE.1) CALL FILTER(GAIN,SUM,NSW,NSO)
	IF(MODE.EQ.3) CALL MINMAX(7,NSO,GAIN,GFAC,XMAX,IMIN,IMAX)
	CALL WORK(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GFAC,OFF,
     &		  GAIN,BUF)
	RETURN
	END
C**********************************************************************
	SUBROUTINE MODE1(ISUM,II,IN,JJ,GAIN,KK,QPR)
C
	REAL GAIN(NSO)
	INTEGER*4 ISUM(NSO),IHIST(256)
	INTEGER*2 IN(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','HALF',
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
C			   compute average DN for each sample and form histogram
	CALL ZIA(IHIST,256)
	CALL ZIA(ISUM,NSO)
	NUM = 0
	DO I=ISTART,IEND,LINC
	    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',I,'SAMP',ISS,
     +			'NSAMPS',NSO,' ')
	    NUM = NUM+1
	    DO J=1,NSO
		N = IN(J)
		ISUM(J) = ISUM(J)+N
		IHIST(N+1) = IHIST(N+1)+1
	    END DO
	END DO
	POP = NUM*NSO
	X = NUM
	DO I=1,NSO
	    GAIN(I) = ISUM(I)/X
	END DO
	ITOT = 0
	DO I=1,256
	    ITOT = ITOT+IHIST(I)*(I-1)
	END DO
	XMEAN = FLOAT(ITOT)/POP
	CALL XVMESSAGE(' ',' ')
	WRITE(OUT,100) XMEAN
  100	FORMAT(' MEAN =',F7.2)
	CALL XVMESSAGE(OUT,' ')
C				   		compute gain factor and offset
	NSAT = POP*SAT/200.0
	N = 0
	I = 1
	DO WHILE (N.LT.NSAT .AND. I.LE.256)
	    N = N+IHIST(I)
	    I = I+1
	END DO
	XLOW = FLOAT(I-1)/XMEAN
	N = 0
	I = 256
	DO WHILE(N.LT.NSAT .AND. I.GE.1)
	    N = N+IHIST(I)
	    I = I-1
	END DO
	HIGH = FLOAT(I+1)/XMEAN
	GFAC = 255.0/(HIGH-XLOW)
	OFF = -GFAC*XLOW
	IF(NSW.NE.1) CALL FILTER(GAIN,ISUM,NSW,NSO)
C					reset the data type for the I/O buffer
	CALL XVCLOSE(INUNIT,ISTAT,' ')
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL',
     +		    'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL WORK(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GFAC,OFF,
     &		  GAIN,IN)
	RETURN
	END
C***********************************************************************
	SUBROUTINE MODE4(SUM,II,BUF,JJ,GAIN,KK,SUMSQ,LL,QPR,XMS)
C
	REAL*8 SUMSQ(NSO),Z
	REAL GAIN(NSO),SUM(NSO),BUF(NSO),XMS(2)
	LOGICAL*1 QPR
	COMMON /PARMCOM/INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,ISTART,
     &			IEND,LINC,SAT,NSW,GFAC,OFF
C					   compute average DN for each sample
	CALL ZIA(SUM,NSO)
	NUM = 0
	DO I=ISTART,IEND,LINC
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     &			'NSAMPS',NSO,' ')
	    NUM = NUM+1
	    DO J=1,NSO
		Z = BUF(J)
		SUM(J) = SUM(J)+Z
		SUMSQ(J) = SUMSQ(J) + Z*Z
	    END DO
	END DO
	POP = NUM*NSO
	X = NUM
	DO I=1,NSO
	    Z = SUM(I)/X
	    BUF(I) = Z				! BUF now contains the means
	    VAR = SUMSQ(I)/X - Z*Z
	    IF (VAR.GT.0.0) THEN
		GAIN(I) = XMS(2)/SQRT(VAR)
	    ELSE
		GAIN(I) = 1.0
	    END IF
	END DO
	IF(NSW.NE.1) CALL FILTER(BUF,SUMSQ,NSW,NSO)
	DO I=1,NSO
	    SUM(I) = XMS(1) - GAIN(I)*BUF(I)	! SUM now contains the offsets
	END DO
	CALL WORK4(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GAIN,
     &		   SUM,BUF)
	RETURN
	END
C**********************************************************************
	SUBROUTINE WORK4(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GAIN,
     &			 OFF,BUF)
C
	REAL GAIN(NSO),OFF(NSO),BUF(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
C				      if necessary, print out all average values
	IF(QPR) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('     SAMPLE     GAIN VALUES',' ')
	    DO I=1,NSO,15
		II = MIN(I+14,NSO)
		WRITE (OUT,100) I,I+14,(GAIN(J),J=I,II)
  100		FORMAT(I5,'-',I5,15F8.2)
		CALL XVMESSAGE(OUT,' ')
	    END DO
C
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('     SAMPLE     OFFSET VALUES',' ')
	    DO I=1,NSO,15
		II = MIN(I+14,NSO)
		WRITE (OUT,100) I,I+14,(OFF(J),J=I,II)
		CALL XVMESSAGE(OUT,' ')
	    END DO
	END IF
C
	IF (IFMT.EQ.7) THEN
	    XMAX = 1.7E38
	    XMIN = -XMAX
	ELSE
	    DO I=1,NSO
		OFF(I) = OFF(I)+0.5
	    END DO
	    IF (IFMT.EQ.1) THEN
		XMIN = 0.0
		XMAX = 255.0
	    ELSE IF (IFMT.EQ.2) THEN
		XMIN = -32768.0
		XMAX = 32767.0
	    ELSE 
		XMIN = -2147483648.0
		XMAX = 2147483647.0
	    END IF
	END IF
C					read in each line, apply gain and offset
C					to each pixel, write each line back out
	IEL = ISL+NLO-1
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     +			'NSAMPS',NSO,' ')
	    DO J=1,NSO
		BUF(J) = MIN(XMAX,MAX(XMIN,GAIN(J)*BUF(J)+OFF(J)))
	    END DO
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NSO,' ')
	END DO
	RETURN
	END
C**********************************************************************
	SUBROUTINE WORK(INUNIT,IOUTUNIT,ISL,ISS,NLO,NSO,IFMT,QPR,GFAC,
     &			OFF,GAIN,BUF)
C
	REAL GAIN(NSO),BUF(NSO)
	CHARACTER*132 OUT
	LOGICAL*1 QPR
C				      if necessary, print out all average values
	IF(QPR) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('     SAMPLE     AVERAGE VALUES',' ')
	    DO I=1,NSO,15
		II = MIN(I+14,NSO)
		WRITE (OUT,100) I,I+14,(GAIN(J),J=I,II)
  100		FORMAT(I5,'-',I5,15F8.2)
		CALL XVMESSAGE(OUT,' ')
	    END DO
	END IF
C
C	   make the the gain = gain factor / average dn for that sample location
C
	WRITE (OUT,200) GFAC,OFF
  200	FORMAT(' GAIN',F8.2,'    OFFSET',F7.1)
	CALL XVMESSAGE(OUT,' ')
C
	IF (IFMT.EQ.1) THEN
	    XMIN = 0.0
	    XMAX = 255.0
	    OFF = OFF+0.5
	ELSE IF (IFMT.EQ.2) THEN
	    XMIN = -32768.0
	    XMAX = 32767.0
	    OFF = OFF+0.5
	ELSE IF (IFMT.EQ.4) THEN
	    XMIN = -2147483648.0
	    XMAX = 2147483647.0
	    OFF = OFF+0.5
	ELSE
	    XMAX = 1.7E38
	    XMIN = -XMAX
	END IF
	DO I=1,NSO
	    IF (GAIN(I) .NE. 0)  GAIN(I) = GFAC/GAIN(I)   !  avoid divide by 0.0
	END DO
C					read in each line, apply gain and offset
C					to each pixel, write each line back out
	IEL = ISL+NLO-1
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,'SAMP',ISS,
     +			'NSAMPS',NSO,' ')
	    DO J=1,NSO
		BUF(J) = MIN(XMAX,MAX(XMIN,GAIN(J)*BUF(J)+OFF))
	    END DO
	    CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NSO,' ')
	END DO
	RETURN
	END
C**********************************************************************
	SUBROUTINE FILTER(GAIN,AVG,NSW,NSO)
C
	REAL GAIN(NSO),AVG(NSO)
C
	M = NSW/2
	SUM = 0.0
	DO I=1,M
	    SUM = SUM+GAIN(I)
	END DO
C					    filter window truncated at left edge
	J = 0
	N = M+1
	DO I=N,NSW
	    J = J+1
	    SUM = SUM+GAIN(I)
	    AVG(J) = SUM/FLOAT(I)
	END DO
C					 move filter window across to right edge
	X = NSW
	N = NSW+1
	DO I=N,NSO
	    SUM = SUM+GAIN(I)-GAIN(I-NSW)
	    J = J+1
	    AVG(J) = SUM/X
	END DO
C					   filter window truncated at right edge
	N = J-M
	L = NSO-M-1
	DO I=N,L
	    SUM = SUM-GAIN(I)
	    X = X-1.0
	    J = J+1
	    AVG(J) = SUM/X
	END DO
	DO I=1,NSO
	    GAIN(I) = AVG(I)
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create gradrem.pdf
process help=*
PARM INP     TYPE=(STRING,60)
PARM OUT     TYPE=(STRING,60)
PARM SIZE    TYPE=INTEGER COUNT=4     DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER 	      DEFAULT=1
PARM SS      TYPE=INTEGER 	      DEFAULT=1
PARM NL      TYPE=INTEGER 	      DEFAULT=0
PARM NS      TYPE=INTEGER 	      DEFAULT=0
PARM START   TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM LENGTH  TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM LINC    TYPE=INTEGER             DEFAULT=1
PARM FILT    TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM GAIN    TYPE=REAL    COUNT=(0:1) DEFAULT=--
PARM OFF     TYPE=REAL    COUNT=(0:1) DEFAULT=--
PARM PERCENT TYPE=REAL    COUNT=(0:1) DEFAULT=--
PARM MEANSIG TYPE=REAL	  COUNT=(0:2) DEFAULT=--
PARM NOPRINT TYPE=KEYWORD COUNT=0:1   DEFAULT=-- VALID=(NOPRINT) 
PARM NOSAT   TYPE=KEYWORD COUNT=0:1   DEFAULT=-- VALID=(NOSAT)   
END-PROC
.TITLE
GRADREM
.HELP
PURPOSE:
Wide angle aircraft scanners often exhibit large, systematic, and non-linear
artificial brightness gradients along scan lines.  GRADREM seeks to remove
these gradients by ratioing the input picture with an estimate of the gradient
upon a flat field.

EXECUTION:

Example 
	GRADREM INP=A OUT=B SIZE=(50,100,500,500) LINC=10 PERCENT=2.0

	In this example, every tenth line of the entire region specified by the
	SIZE parameter is used to compute the gradient function.  The output
	picture will be about 2.0 percent saturated (1.0 high, 1.0 low).

	GRADREM INP=A OUT=B LENGTH=300 GAIN=150.0

	In this example, the first 300 lines are used to compute the
	gradient function.  DNout = 150.0 * (DNin / gradient).

	GRADREM INP=A OUT=B 'NOSAT

	In this example, the entire region is used to compute the gradient
	function; the program sets the offset (OFF) to 0 and chooses a
	gain such that no unsaturated input pixel becomes saturated in the
	output image.  


OPERATION:
The user selects a region of the picture whose differences in brightness
are believed to be predominantly artificial.  The lines in this region are
averaged to produce a 'standard' DN level at each sample.  If requested, a
box filter is used to smooth the standard DN function.  Each pixel is then
ratioed with its appropriate standard DN, and the specified gains and
offsets applied.  Floating point arithmetic is used throughout.


WRITTEN BY:  Ron Alley, 7 June 1979
COGNIZANT PROGRAMMER:  Steve Pohorsky

.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE OUT
Output image file
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
.VARIABLE START
Start of grad estimation region
.VARIABLE LENGTH
Lines in grad estimation region
.VARIABLE LINC
Gather stats every LINC'th line
.VARIABLE FILT
Number of filter weights
.VARIABLE GAIN
Output DN calculation factor
.VARIABLE OFF
Output DN calculation offset
.VARIABLE PERCENT
Percent saturation
.VARIABLE MEANSIG
Forces this mean and sigma for
the estimation region
.VARIABLE NOPRINT
Suppresses gradient function
printing. Valid: NOPRINT
.VARIABLE NOSAT
Selects gain for no saturation 
Valid: NOSAT
.LEVEL2
.VARIABLE START
START specifies the starting line of the region used to estimate the gradient.
The default is 1.
.VARIABLE LENGTH
LENGTH specifies the length of the region used to estimate the gradient.  The
default is for the entire picture to be used (as determined by the SIZE field).
.VARIABLE LINC
Every LINC-th line will be used to estimate the gradient.  Default is 1.
.VARIABLE FILT
FILT selects box filtering in order to smooth the gradient function, and
specifies the weight.  The default is not to perform any smoothing.
.VARIABLE GAIN
The output DN for any pixel is given by the formula
		 DNout = GAIN * (DNin / gradient) + OFFset.
GAIN specifies the factor to be used in the computation.  If OFF (the offset)
is specified, but GAIN isn't, the gain defaults to 100.0.  If neither GAIN nor
OFF is specified, and the user doesn't select the PERCENT or NOSAT options,
the gain is set to the input's mean DN value.
.VARIABLE OFF
The output DN for any pixel is given by the formula
		 DNout = GAIN * (DNin / gradient) + OFFset.
OFF specifies the offset to be used in the computation.  The default 
offset is 0.0.
.VARIABLE PERCENT
When this parameter is specified, the program will compute and use values
for GAIN and OFF which will lead to PERCENT percent saturation in the output
picture.
.VARIABLE NOPRINT
'NOPRINT suppresses the printing of the computer gradient function.
.VARIABLE NOSAT
The 'NOSAT option sets the offset to zero and chooses a gain such that no
unsaturated input pixel becomes saturated in the output image.
.VARIABLE MEANSIG
MEANSIG has two values: the first is the desired mean for each sample (column)
in the gradient estimation region.  The second value is the desired 
standard deviation for this region.  In this mode, GRADREM forces a gradient
removal to best approximate these values.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create gradrem.imake
#define  PROGRAM   gradrem

#define MODULE_LIST gradrem.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P3SUB
#define LIB_P2SUB
$ Return
$!#############################################################################
