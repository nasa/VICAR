$!****************************************************************************
$!
$! Build proc for MIPL module filter
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:11:30
$!
$! Execute by entering:		$ @filter
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
$ write sys$output "*** module filter ***"
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
$ write sys$output "Invalid argument given to filter.com file -- ", primary
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
$   if F$SEARCH("filter.imake") .nes. ""
$   then
$      vimake filter
$      purge filter.bld
$   else
$      if F$SEARCH("filter.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake filter
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @filter.bld "STD"
$   else
$      @filter.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create filter.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack filter.com -mixed -
	-s filter.f -
	-p filter.pdf -
	-i filter.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create filter.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C       10-91  REA  REWRITTEN
C	10-02  REA  Second input file option added. The second input, if used,
C		    holds the filter window, and overrides the NLW, NSW, SYMM,
C		    and WEIGHTS parameters.
C
	PARAMETER (MAXSIZE=90000)
	EXTERNAL FILT
	REAL*4 WTS(MAXSIZE),BUF(5000)
	CHARACTER*4 FORMAT
	LOGICAL XVPTST,QLCYCLE,QSCYCLE,QINT
	COMMON /FLTCOM/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,
     +			OFFSET,GAIN,DIV,XMIN,XMAX,QLCYCLE,QSCYCLE,QINT
C
C						open datasets, check size field
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +		  'IO_ACT','SA',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF(ISL+NL-1 .GT. NLIN) THEN
	    CALL XVMESSAGE(
     +             ' Number of lines requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
	IF(ISS+NS-1 .GT. NSIN) THEN
	    CALL XVMESSAGE(
     +             ' Number of samples requested exceeds input size',
     +		   ' ')
	    CALL ABEND
	ENDIF
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &		  'OPEN_ACT','SA','IO_ACT','SA','U_NL',NL,'U_NS',NS,' ')
C
C						determine default cutoff values
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
C							  check for second input
C							 and process, if present
	CALL XVPCNT('INP',ICOUNT)
	IF (ICOUNT .EQ. 2) THEN
	    CALL XVUNIT(INUNIT2,'INP',2,ISTAT,' ')
	    CALL XVOPEN(INUNIT2,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +			'IO_ACT','SA',' ')
	    CALL XVGET(INUNIT2,ISTAT,'NL',NLW,'NS',NSW,' ')
	    INDEX = 1
	    DO I=1,NLW
		IF (INDEX+NSW-1 .GT. MAXSIZE) THEN
		    CALL XVMESSAGE(
     +			'The filter window is too large to process',' ')
		    CALL ABEND
		ENDIF
		CALL XVREAD(INUNIT2,WTS(INDEX),ISTAT,' ')
		INDEX = INDEX + NSW
	    END DO
	ELSE
C						process NLW, NSW, SYMM & WEIGHTS

	    CALL XVP('NLW',NLW,ICOUNT)
	    CALL XVP('NSW',NSW,ICOUNT)
C							    build weights matrix
	    CALL XVPARM('WEIGHTS',BUF,ICOUNT,IDEF,0)
	    IF (XVPTST('ALL') .OR. XVPTST('ASYMMETR')) THEN
		IF (ICOUNT .NE. NLW*NSW) THEN
		    CALL XVMESSAGE(
     +			' NLW*NSW weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		CALL MVE(7,ICOUNT,BUF,WTS,1,1)
	    ELSE IF (XVPTST('UPPER').OR.XVPTST('NONSYMME')) THEN
		IF (ICOUNT .NE. NSW*((NLW+1)/2)) THEN
		    CALL XVMESSAGE(
     +		  'NSW*((NLW+1)/2) weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		CALL MVE(7,ICOUNT,BUF,WTS,1,1)
		LOC1 = ICOUNT - 2*NSW + 1
		LOC2 = ICOUNT + 1
		DO I=1,NLW/2
		    CALL MVE(7,NSW,BUF(LOC1),WTS(LOC2),1,1)
		    LOC1 = LOC1 - NSW
		    LOC2 = LOC2 + NSW
		END DO
	    ELSE IF (XVPTST('LEFT')) THEN
		IF (ICOUNT .NE. NLW*((NSW+1)/2)) THEN
		    CALL XVMESSAGE(
     +		    'NLW*((NSW+1)/2) weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		LOC1 = 1
		LOC2 = 1
		NUM = (NSW+1)/2
		DO I=1,NLW
		    CALL MVE(7,NUM,BUF(LOC1),WTS(LOC2),1,1)
		    CALL MVE(7,NUM-1,BUF(LOC1+NUM-2),WTS(LOC2+NUM),-1,1)
		    LOC1 = LOC1 + NUM
		    LOC2 = LOC2 + NSW
		END DO
	    ELSE
		IF (ICOUNT .NE. ((NLW+1)/2)*((NSW+1)/2) ) THEN
		    CALL XVMESSAGE(
     +	' ((NLW+1)/2) * ((NSW+1)/2) weights values must be provided',' ')
		    CALL ABEND
		ENDIF
		LOC1 = 1
		LOC2 = 1
		NUM = (NSW+1)/2
		DO I=1,(NLW+1)/2
		    CALL MVE(7,NUM,BUF(LOC1),WTS(LOC2),1,1)
		    CALL MVE(7,NUM-1,BUF(LOC1+NUM-2),WTS(LOC2+NUM),-1,1)
		    LOC1 = LOC1 + NUM
		    LOC2 = LOC2 + NSW
		END DO
		LOC1 = LOC1 - 2*NUM
		DO I=1,(NLW-1)/2
		    CALL MVE(7,NUM,BUF(LOC1),WTS(LOC2),1,1)
		    CALL MVE(7,NUM-1,BUF(LOC1+NUM-2),WTS(LOC2+NUM),-1,1)
		    LOC1 = LOC1 - NUM
		    LOC2 = LOC2 + NSW
		END DO
	    END IF
	END IF
C
	IF((NLW/2)*2 .EQ. NLW) THEN
	    CALL XVMESSAGE(' NLW cannot be an even integer',' ')
	    CALL ABEND
	ENDIF
	IF((NSW/2)*2 .EQ. NSW) THEN
	    CALL XVMESSAGE(' NSW cannot be an even integer',' ')
	    CALL ABEND
	ENDIF
C
	CALL XVP('SCALE',BUF,ICOUNT)
	OFFSET = BUF(1)
	GAIN = BUF(2)
C
	CALL XVP('RANGE',BUF,ICOUNT)
	IF (ICOUNT .EQ. 2) THEN
	    XMIN = BUF(1)
	    XMAX = BUF(2)
	ELSE
	    CALL XVP('DNMIN',BUF,ICOUNT)
	    IF (ICOUNT .EQ. 1) XMIN = BUF(1)
	    CALL XVP('DNMAX',XMAX,ICOUNT)
	    IF (ICOUNT .EQ. 1) XMAX = BUF(1)
	END IF
C							edge treatment
	IF (XVPTST('CYCLE')) THEN
	    QLCYCLE = .TRUE.
	    QSCYCLE = .TRUE.
	ELSE
	    QLCYCLE = XVPTST('LCYCLE')
	    QSCYCLE = XVPTST('SCYCLE')
	END IF
	CALL XVP('DIVIDE',DIV,ICOUNT)
	IF ( ICOUNT .EQ. 0) THEN
	    DIV = 0.0
	    DO I=1,NLW*NSW
		DIV = DIV + WTS(I)
	    END DO
	END IF
	IF (DIV .EQ. 0.0) DIV = 1.0
C
	NSX = NS + NSW
	NSO = NS
	N1 = 4*NLW*NSX				! bytes in input array
	N2 = 4*NSO				! bytes in output array
C
	CALL STACKA(9,FILT,2,N1,N2,WTS,NSX,NLW,NSW,NSO)
C
	CALL XVCLOSE(IUNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
	RETURN
	END
C**********************************************************************
	SUBROUTINE FILT(BUF,N1,OUT,N2,WTS,NSX,NLW,NSW,NSO)
C
	REAL BUF(NSX,NLW),OUT(NSO),WTS(NSW,NLW)
	LOGICAL QLCYCLE,QSCYCLE,QINT
	COMMON /FLTCOM/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,
     +			OFFSET,GAIN,DIV,XMIN,XMAX,QLCYCLE,QSCYCLE,QINT
C
	NLW2 = NLW/2			! half of the line weights
	NSW2 = NSW/2			! half of the sample weights
	IST = MAX(ISS-NSW2,1)		! first sample to be read in
	LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
	NLEFT = IST - ISS + NSW2	! number of pixels to pad on left
	NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
	NSREAD = LAST - IST + 1		! number of samples to be read
	LOC = NLW			! line position in input buffer
C
C						Set up input buffer
	DO LINE = ISL+NLW2, ISL-NLW2, -1
	    IF (LINE .GE. 1) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		ELSE
		    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		END IF
	    ELSE
		IF (QLCYCLE) THEN
		    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',
     +			       NLIN+LINE,'SAMP',IST,'NSAMPS',NSREAD,' ')
		    IF (QSCYCLE) THEN
			CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    ELSE
			CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
		    END IF
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
C						    compute value for each pixel
	    DO ISAMP=1,NS
		SUM = 0.0
		DO I=1,NLW
		    LLOC = MOD(LOC+I-2,NLW) + 1
		    DO J=1,NSW
			SUM = SUM + WTS(J,I)*BUF(ISAMP+J-1,LLOC)
		    END DO
		END DO
		SUM = GAIN*(SUM/DIV) + OFFSET
		OUT(ISAMP) = MIN(MAX(SUM,XMIN), XMAX)
		IF (QINT) OUT(ISAMP) = NINT(OUT(ISAMP))
	    END DO
	    CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	    IF (LINE .LE. NLIN) THEN
		CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			    'SAMP',IST,'NSAMPS',NSREAD,' ')
		IF (QSCYCLE) THEN
		    CALL CYCLE(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create filter.pdf
process help=*
PARM	INP	TYPE=(STRING,40) COUNT=(1:2)
PARM	OUT	TYPE=(STRING,40)
PARM	SIZE	TYPE=INTEGER	COUNT=4        DEFAULT=(1,1,0,0)
PARM	SL	TYPE=INTEGER			DEFAULT=1
PARM	SS	TYPE=INTEGER			DEFAULT=1
PARM	NL	TYPE=INTEGER			DEFAULT=0
PARM	NS	TYPE=INTEGER			DEFAULT=0
PARM	NLW	TYPE=INTEGER			DEFAULT=15
PARM	NSW 	TYPE=INTEGER			DEFAULT=15
PARM	SYMM    TYPE=KEYWORD    COUNT=1         DEFAULT=UL +
    VALID=(UL,UPPER,LEFT,ALL,NONSYMME,ASYMMETR)
PARM    EDGE    TYPE=KEYWORD VALID=(CYCLE,LCYCLE,SCYCLE,REFLECT) DEFAULT=REFLECT
PARM 	WEIGHTS	TYPE=REAL	COUNT=(0:300)	DEFAULT= +
                (  -23,  -19,    5,   31,   43,   42,   37,   34, +
                   -16,   17,   42,   36,    7,  -20,  -36,  -40, +
                    15,   44,   22,  -28,  -59,  -52,  -28,  -17, +
                    41,   25,  -40,  -69,  -12,   78,  137,  153, +
                    41,  -21,  -76,   -8,  135,  185,  103,   40, +
                    22,  -61,  -58,  102,  193,  -67, -566, -855, +
                     3,  -80,  -19,  173,   94, -607,-1617,-2104, +
                    -3,  -84,    0,  191,   16, -897,-2141,30001)
PARM	SCALE	TYPE=REAL	COUNT=2		DEFAULT=(0,1)
PARM	DIVIDE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	RANGE	TYPE=REAL	COUNT=(0,2)	DEFAULT=--
PARM	DNMIN	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	DNMAX	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM    PARMS   TYPE=(STRING,40) COUNT=(0:1)    DEFAULT=--
END-PROC
.TITLE
VICAR PROGRAM FILTER
.HELP
PURPOSE:

FILTER is a VICAR applications program which performs two-dimensional
convolution filtering.  For each output sample, the program computes
a weighted average of a rectangular set of input pixels followed by
a linear transformation.  FILTER may be used to perform high-pass or 
low-pass filtering.

.page
EXECUTION:

   The following is the execution statement format for FILTER:

             FILTER INP=PIX OUT=OPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 
.page
OPERATION:

FILTER accepts as input a rectangular set of weights and calculates each
output point OUT(l,s) as follows:
             _                                                       _
            |  nlw nsw                                                |   /
 XXX(l,s) = | SUM SUM  IN[l-(nlw+1)/2 + i , s-(nsw+1)/2 + j] * W(i,j) |  / DIV
            |_ i=1 j=1                                               _| /
and
   OUT(l,s) = (GAIN * XXX(l,s)) + OFF
where
	IN       is the input file
	W        is the weight matrix
	nlw      is the number of lines in the weight matrix
	nsw      is the number of samples in the weight matrix
	OFF,GAIN are the SCALE parameters
	DIV      is the DIVIDE parameter
.PAGE
EXAMPLES:

1) FILTER IN OUT NLW=3 NSW=5 WEIGHTS=(0,-1,-2,-1,-2,10)
   The file will be filtered with the weight table shown below:
         0   -1   -2   -1   0
	-1   -2   10   -2   -1
	 0   -1   -2   -1   0
   The final linear transformation is
		FP(L,S) = IN(L,S) / 6
   where 6 is the default for the DIVIDE parameter, (i.e., the sum of the
   entries in the weight table).
.PAGE
2) FILTER IN OUT NLW=3 NSW=5 SCALE=(-30,1) DIVIDE=3 WEIGHTS=(0,-1,-2,-1,-2,10)
   This is the same as the example above except the final transformation is 
   given by:
		FP(L,S) = 1.0*(IN(L,S)/3.0) -30.0

3) FILTER IN OUT NLW=3 NSW=5 'UPPER DNMIN=15 WEIGHTS=(-2,5,4,3,-1,-5,20,3,1,1)
   These parameters generate the horizontally nonsymmetric weights shown
   below:
   	 -2   5   4   3   -1
	 -5  20   3   1    1
	 -2   5   4   3   -1
   and output will be computed as follows:  If the output is less
   than or equal to 15 DN, it is set to 15 DN.  
.PAGE
4) FILTER IN OUT NLW=3 NSW=3 'ALL WEIGHTS=(-1,0,0,0,0,0,0,0,1)
   These parmeters will produce a diagonal gradient picture using this weight
   matrix:  
  	-1   0   0
	 0   0   0
	 0   0   1

5) FILTER (IN,KERNAL) OUT
   In this case, the filter weights are provided in the second input file.
.PAGE

COGNIZANT PROGRAMMER:  Ron Alley 

.LEVEL1
.VARI INP
1. Input image file
2. (Optional) Weights as a
   VICAR image file
.VARI OUT
Filtered image file
.VARI SIZE
Vicar size field
.VARI SL
Size field starting line
.VARI SS
Size field starting sample
.VARI NL
Size field number of lines
.VARI NS
Size field number of samples
.VARI NLW
Number of lines of weights
.VARI NSW
Number of samples of weights
.VARI SYMM
WEIGHTS are what part of the
weights matrix?  Valid:
UL    - for upper left quadrant
UPPER - for upper half
LEFT  - for left side
ALL   - for entire matrix
.VARI WEIGHTS
Defines the weight matrix
.VARI DIVIDE
Scaling paramater upon output
.VARI SCALE
Linear output scaling:
(offset,scale)
.VARI DNMAX
Defines maximum output dn
.VARI DNMIN
Defines minimum output dn
.VARI RANGE
DN interval to which output
will be clipped.
(Synonym for DNMAX,DNMIN)
.VARIABLE EDGE
Method of handling edges 
REFLECT, CYCLE, LCYCLE, SCYCLE
.VARI PARMS
 Parameter data set name 
.LEVEL2
.VARI INP
The first file is the input image file that is to be filtered.
The second file, if present, contains the entire window of filter weights,
in a VICAR labelled image file.  If the second file is present, the parameters
NLW, NSW, SYMM, and WEIGHTS are ignored, since their values are either 
supplied or implied by the contents of the secon input file.
.VARI OUT
A file to write the filtered product into
.VARI SIZE
The standard size field defining the area of the input picture that is to
be filtered.
.VARI SL
Starting line of the area to be filtered.
.VARI SS
Starting sample of the area to be filtered
.VARI NL
Number of lines in the area to be filtered.
.VARI NS
Number of samples in the area to be filtered.
.VARI NLW
This specifies the number of lines of weights. Must be odd.
.VARI NSW
This specifies the number of samples in each line of the weight matrix.
Must be odd.
.VARI SYMM
This keyword parameter indicates which portion of the weights matrix is 
given in the WEIGHTS parameter.  When only part of the weights matrix is
given, the remaining parts are generated by reflection through the center
lines.
      UL     - (the default) indicates that the upper left quadrant has
               been specified.
      UPPER  - indicates that the upper half of the matrix has been given.
      LEFT   - indicates that the left half of the matrix has been given.
      ALL    - indicates that all elements of the matrix have been given.
For compatibility, the values NONSYMME and ASYMMETR are also allowed. They
have the same meanings as UPPER and ALL, respectively.
.VARI WEIGHTS
These are the weights to be used in the filter.  They should be ordered from
left to right, the top to bottom.
.VARI DIVIDE
The result of the filtering and rescaling (via SCALE parameters) is divided
by this value prior to the clipping to the RANGE parameter.
Each output point O(l,s) is given by:

        O(l,s) = GAIN*(T(l,s)/DIVIDE) + OFFSET

where GAIN and OFFSET are defined by SCALE and T is the output of the 
convolution.  The default is that the sum of the weights is used.  If this 
sum is zero, then 1.0 is used.
.VARI SCALE
This keyword specifies the application of a linear transformation to each
filtered value T(l,s):

        O(l,s) = GAIN*(T(l,s)/DIVIDE) + OFFSET

where SCALE=(OFFSET,GAIN).  Default is SCALE=(0,1).
See also DIVIDE.
.VARI DNMIN
All pixels with DN less than DNMIN upon output are set to DNMIN. 
DNMIN is synonymous with RANGE(1).
The default is the least value of the data format.
.VARI DNMAX
All pixels with DN greater than DNMAX upon output are set to DNMAX. 
DNMAX is synonymous with RANGE(2).
The default is the largest value of the data format.
.VARI RANGE
The pair of values (a,b) specifies the range of DN to which the output will
be clipped.
RANGE is synonymous with (DNMIN, DNMAX).  If both RANGE and DNMIN or DNMAX
are specified, then RANGE will take precedence.
.VARIABLE EDGE
Specifies image handling at image boundaries.  Setting EDGE=CYCLE or 'CYCLE
causes the program to treat the image as if it wrapped around at boundaries
in both directions.  'LCYCLE and 'SCYCLE cause wrap-around in the line and
sample direction only, respectively.  The default is for the program to 
reflect the image at the boundaries.
.VARI PARMS
 PARMS can be used to specify the name of an optional parameter data
 set. Any combination of the allowable parameters may be given. If
 any of the parameters are given interactively, the interactive value
 takes precedence.
$ Return
$!#############################################################################
$Imake_File:
$ create filter.imake
#define  PROGRAM   filter

#define MODULE_LIST filter.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
