$!****************************************************************************
$!
$! Build proc for MIPL module fft11
$! VPACK Version 1.8, Thursday, October 24, 1996, 18:23:08
$!
$! Execute by entering:		$ @fft11
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
$ write sys$output "*** module fft11 ***"
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
$ write sys$output "Invalid argument given to fft11.com file -- ", primary
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
$   if F$SEARCH("fft11.imake") .nes. ""
$   then
$      vimake fft11
$      purge fft11.bld
$   else
$      if F$SEARCH("fft11.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fft11
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fft11.bld "STD"
$   else
$      @fft11.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fft11.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fft11.com -
	-s fft11.f -
	-i fft11.imake -
	-p fft11.pdf -
	-t tstfft11.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fft11.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C  PROGRAM FFT11  --  1-D FAST FOURIER TRANSFORM
C
C  03 APR 85   ...LWK...   WRITTEN TO PROVIDE NON-AP EQUIVALENT OF FFT1
C  03 MAY 93   ...JFM...   MADE PORTABLE, CALL TO RCSFFT REPLACED BY CALL
C			   TO DFFT (renamed ported version of RCSFFT).
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	IMPLICIT INTEGER (A-Z)
	COMMON/PASS/ SL, SS, NLO, NSO, NLI, NSI, IUN, EXT, CMP, ODD,
     .   INT
	LOGICAL XVPTST, EXT, FWD, CMP, ODD, INT
	CHARACTER*8 FMT
 	CHARACTER*50 STRING
	EXTERNAL FORWRD, INVERS

	FWD = .NOT.XVPTST( 'INVERSE')
	CMP = .FALSE.			! INITIALIZE
	ODD = .FALSE.
	INT = .FALSE.

	CALL XVUNIT( IUN, 'INP', 1, I, ' ')

	IF (FWD) THEN
	  CALL XVMESSAGE( 'FORWARD TRANSFORM',' ' )
	  CALL XVOPEN( IUN, I, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .	   'U_FORMAT', 'REAL', ' ' )
	  CALL XVGET( IUN, I, 'FORMAT', FMT, ' ')
	  IF (FMT.EQ.'COMP'.OR.FMT.EQ.'COMPLEX') THEN
	    CALL XVCLOSE( IUN, I, ' ' )
	    CALL XVOPEN( IUN, I, 'OPEN_ACT', 'SA', 'IO_ACT', 
     .		'SA', ' ')
	    CMP = .TRUE.
	  ENDIF
          STRING(1:15) = 'INPUT FORMAT = '
	  STRING(16:) = FMT(1:)
	  CALL XVMESSAGE( STRING, ' ' )

	ELSE
	  CALL XVMESSAGE( 'INVERSE TRANSFORM', ' ' )
	  CALL XVOPEN( IUN, I, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA', ' ')
	  CALL XVGET( IUN, I, 'FORMAT', FMT, ' ' )
	  IF (FMT.NE.'COMP'.AND.FMT.NE.'COMPLEX') THEN
	    CALL XVMESSAGE( ' ** FFT FORMAT IS NOT COMPLEX **', ' ')
	    CALL ABEND
	  ENDIF

	  CALL XVPARM( 'OFORM', FMT, I, J, 1, 0)
	  IF (FMT.EQ.'COMP'.OR.FMT.EQ.'COMPLEX') CMP = .TRUE.
	  IF (FMT.EQ.'BYTE' .OR. FMT.EQ.'HALF' .OR. FMT.EQ.'FULL')
     .	   INT = .TRUE.
	ENDIF

	CALL XVSIZE( SL, SS, NLO, NSO, NLI, NSI)
	IF (.NOT.FWD) THEN
	  IF (SS.NE.1 .OR. NSI.NE.NSO) THEN
	    CALL XVMESSAGE(' ** INVERSE TRANSFORM USES ENTIRE LINE **',
     .		' ' )
	    CALL ABEND
	  ENDIF
	  IF (NSI/2.NE.(NSI+1)/2 .AND. .NOT.CMP) THEN
	    CALL XVMESSAGE(' ** FFT OF REAL IMAGE MUST HAVE EVEN NS **',
     . 		' ' )
	    CALL ABEND
	  ENDIF
	ENDIF
	CALL PRNT( 4, 2, NLI, 'INPUT NL, NS = .')
	CALL PRNT( 4, 2, NLO, 'OUTPUT NL, NS = .')

	IF (.NOT.CMP .AND. NSO/2.NE.(NSO+1)/2) ODD = .TRUE.

	IF (SS-1+NSO .GT. NSI) THEN
	  NSI = NSI-SS+1		! # SAMPLES TO READ
	  IF (NSO-NSI.GT.1024) THEN
	    CALL XVMESSAGE(' ** EXTRAPOLATION BUFFER TOO SMALL **', ' ')
	    CALL ABEND
	  ENDIF
	  EXT = .TRUE.			! EXTRAPOLATION REQUIRED
	ENDIF

	BUFL = MAX0(NSO,NSI)*8+8
	IF (FWD) THEN
	  CALL STACKA ( 4, FORWRD, 1, BUFL, I)
	ELSE
	  CALL STACKA ( 5, INVERS, 1, BUFL, FMT, I)
	ENDIF

	RETURN
	END

C******************************************************************
	SUBROUTINE FORWRD( BUF, LEN)
C  PERFORM FORWARD FFT.

	IMPLICIT INTEGER (A-Z)
	CHARACTER*50 STRING
	REAL*4 BUF(LEN/4)
	LOGICAL EXT, CMP, ODD, REAL, INT
	COMMON/PASS/ SL, SS, NLO, NSO, NLI, NSI, IUN, EXT, CMP, ODD,
     .	 INT

	REAL = .NOT.(CMP.OR.ODD)

C  OPEN OUTPUT DATA SET:
	CALL XVUNIT( OUN, 'OUT', 1, I, ' ')
	CALL XVOPEN( OUN, I, 'OP', 'WRITE', 'OPEN_ACT', 'SA', 'IO_ACT',
     .	 'SA', 'U_NL', NLO, 'U_NS', NSO, 'O_FORMAT', 'COMP',
     .   'U_FORMAT', 'COMP', ' ')

C  INITIALIZE READS:
	CALL XVREAD( IUN, BUF, I, 'LINE', SL, 'SAMP', SS,
     .	 'NSAMPS', NSO, ' ' )

	N = NSO				! DIMENSION OF FFT
	IF (REAL) N = NSO/2

	DO L = 1,NLO

	  IF (REAL) THEN
	    IF (EXT) CALL EXTRPR( BUF, NSI, NSO)
	  ELSEIF (ODD) THEN			! FORM COMPLEX ELEMENTS
	    CALL MVE( 7, NSI, BUF(NSI), BUF(2*NSI-1), -2, -2)  ! REAL PARTS
	    CALL MVE( 7, NSI, 0., BUF(2), 0, 2)		! IMAGINARY PARTS
	    IF (EXT) CALL EXTRPC( BUF, NSI, NSO)
	  ELSE
	    IF (EXT) CALL EXTRPC( BUF, NSI, NSO)
	  ENDIF

C		PERFORM THE TRANSFORM:
	  CALL DFFT( BUF(1), BUF(2), N, N, N, 2, *901, *902)

	  IF (REAL) THEN
	    CALL REALTR( BUF(1), BUF(2), N, 2)
C		FILL THE REDUNDANT HALF:
	    CALL MVE( 7, (NSO-1)/2, BUF(3), BUF(2*NSO-1), 2, -2) ! REAL PARTS
	    CALL MVE( 7, (NSO-1)/2, BUF(4), BUF(2*NSO), 2, -2) ! IMAG'Y PARTS
	    CALL MULV( 7, (NSO-1)/2, -1.0, BUF(2*NSO), 0, -2) ! COMPLEX CONJ.
C  		CONVERT TO FFT1 FORMAT:  SCALE BY 0.5 
	    CALL MULV( 7, 2*NSO, 0.5, BUF, 0, 1)
	  ENDIF

	  IF (NSO/2 .EQ. (NSO+1)/2) THEN	! (ONLY IF EVEN)
C		TWITCH FOR FFT1 FORMAT:
	    BUF(NSO/2+1) = -BUF(NSO/2+1)
	    BUF(NSO/2+2) = -BUF(NSO/2+2)
	    BUF(NSO/2+NSO+1) = -BUF(NSO/2+NSO+1)
	    BUF(NSO/2+NSO+2) = -BUF(NSO/2+NSO+2)
	  ENDIF

C		FFT1 FORMAT: TAKE COMPLEX CONJUGATE
	  CALL MULV( 7, NSO, -1.0, BUF(2), 0, 2)
	  CALL XVWRIT( OUN, BUF, I, ' ' )

	  IF (L.LT.NLO) THEN
	    IF (SS.EQ.1 .AND. NSI.EQ.NSO) THEN
	      CALL XVREAD( IUN, BUF, I, ' ' )
	    ELSE
	      CALL XVREAD( IUN, BUF, I, 'SAMP', SS, 'NSAMPS',
     .	       NSO, ' ' )
	    ENDIF
	  ENDIF

	ENDDO

	RETURN
C
C     *** ERROR TERMINATION **

901	CALL XVMESSAGE(' A PRIME FACTOR OF NS EXCEEDS 23', ' ')
	CALL ABEND
902	STRING(1:44)=' TOO MANY SQUARE-FREE OR PRIME FACTORS IN NS'
	CALL XVMESSAGE(STRING, ' ')
	CALL ABEND
C
	END

C***************************************************************
	SUBROUTINE INVERS( BUF, LEN, FMT)
C  PERFORM INVERSE FFT.

	IMPLICIT INTEGER (A-Z)
	REAL*4 BUF(LEN/4), SCALE
	CHARACTER*8 FMT
	CHARACTER*50 STRING
	LOGICAL EXT, CMP, ODD, REAL, INT
	COMMON/PASS/ SL, SS, NLO, NSO, NLI, NSI, IUN, EXT, CMP, ODD,
     .	 INT

	REAL = .NOT.(CMP.OR.ODD)
	N = NSO

C  OPEN OUTPUT DATA SET:
	CALL XVUNIT( OUN, 'OUT', 1, I, ' ')
	IF (CMP) THEN
	  CALL XVOPEN( OUN, I, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .	   'IO_ACT', 'SA', 'U_NL', NLO, 'U_NS', NSO, ' ')
	ELSE				! IMAGE IS TO BE REAL
	  CALL XVOPEN( OUN, I, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .	   'IO_ACT', 'SA', 'U_NL', NLO, 'U_NS', NSO,
     .	   'U_FORMAT', 'REAL', 'O_FORMAT', FMT, ' ')
	  IF (.NOT.ODD) N = NSO/2
	ENDIF

	SCALE = 1./FLOAT(N)
	IF (REAL) SCALE = SCALE/4.

C  INITIALIZE READS:
	CALL XVREAD( IUN, BUF, I, 'LINE', SL, ' ')

	DO L = 1,NLO

C		CONVERT FROM FFT1 FORMAT: 
	  CALL MULV( 7, NSI, -1.0, BUF(2), 0, 2) ! TAKE COMPLEX CONJUGATE
	  IF (REAL) CALL MULV( 7, 2*NSI, 2.0, BUF, 0, 1)	! & SCALE BY 2.0
	  IF (NSO/2 .EQ. (NSO+1)/2) THEN	! (ONLY IF EVEN)
	    BUF(NSO/2+1) = -BUF(NSO/2+1)
	    BUF(NSO/2+2) = -BUF(NSO/2+2)
	    BUF(NSO/2+NSO+1) = -BUF(NSO/2+NSO+1)
	    BUF(NSO/2+NSO+2) = -BUF(NSO/2+NSO+2)
	  ENDIF

C		PERFORM THE TRANSFORM:
	  IF (REAL) CALL REALTR( BUF, BUF(2), N, -2)
	  CALL DFFT( BUF, BUF(2), N, N, N, -2, *901, *902)

	  IF (ODD) CALL MVE( 7, N-1, BUF(3), BUF(2), 2, 1) ! PACK REAL PARTS
	  CALL MULV( 7, 2*N, SCALE, BUF, 0, 1)	! APPLY SCALE FACTOR
	  IF (INT) CALL ADDV( 7, 2*N, 0.5, BUF, 0, 1)  ! ROUND UP

	  CALL XVWRIT( OUN, BUF, I, ' ')

	  IF (L.LT.NLO) CALL XVREAD( IUN, BUF, I, ' ')

	ENDDO

	RETURN
C
C     *** ERROR TERMINATION **

901	CALL XVMESSAGE(' A PRIME FACTOR OF NS EXCEEDS 23', ' ')
	CALL ABEND
902	STRING(1:44)=' TOO MANY SQUARE-FREE OR PRIME FACTORS IN NS'
	CALL XVMESSAGE(STRING, ' ')
	CALL ABEND
C
	END

C**************************************************************
	SUBROUTINE EXTRPC( BUF, NS, NT)
C
C   EXTRAPOLATE COMPLEX DATA WITH A COSINE FUNCTION

	IMPLICIT INTEGER (A-Z)
	REAL*4	ACOS(2048),D,THETA,AMPL,P1,P2,DC,A,PI/3.1415928/
	COMPLEX*8 BUF(1)

C  LOAD COSINE BUFFER
	D = PI/(NT-NS)
	NCOS = 2*(NT-NS)
	THETA = D
	DO J=1,NCOS
	  ACOS(J) = COS(THETA)
	  THETA = THETA+D
	ENDDO

C  PERFORM EXTRAPOLATION
	P1 = BUF(NS)
	P2 = BUF(1)
	AMPL = (P1-P2)*0.5
	JPHI = 0
	IF (P1.LT.P2) THEN
	  AMPL = -AMPL
	  JPHI = NT-NS
	ENDIF
	DC = (P1+P2)*0.5
	JTHETA = JPHI
	JFIRST = NS+1
	JLAST = NT 

	DO J=JFIRST,JLAST
	  JTHETA = JTHETA+1
	  A = AMPL*ACOS(JTHETA)+DC
	  BUF(J) = CMPLX(A,0.0)
	ENDDO

	RETURN
	END

C*************************************************************
	SUBROUTINE EXTRPR( BUF, NS, NT)
C
C   EXTRAPOLATE REAL DATA WITH A COSINE FUNCTION

	IMPLICIT INTEGER (A-Z)
	REAL*4	ACOS(2048),D,THETA,AMPL,P1,P2,DC,PI/3.1415928/
	REAL*4  BUF(1)

C  LOAD COSINE BUFFER
	D = PI/(NT-NS)
	NCOS = 2*(NT-NS)
	THETA = D
	DO J=1,NCOS
	  ACOS(J) = COS(THETA)
	  THETA = THETA+D
	ENDDO

C  PERFORM EXTRAPOLATION
	P1 = BUF(NS)
	P2 = BUF(1)
	AMPL = (P1-P2)*0.5
	JPHI = 0
	IF (P1.LT.P2) THEN
	  AMPL = -AMPL
	  JPHI = NT-NS
	ENDIF
	DC= (P1+P2)*0.5
	JTHETA = JPHI
	JFIRST = NS+1
	JLAST = NT 

	DO J=JFIRST,JLAST
	  JTHETA = JTHETA+1
	  BUF(J) = AMPL*ACOS(JTHETA)+DC
	ENDDO

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fft11.imake
#define  PROGRAM   fft11

#define MODULE_LIST fft11.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create fft11.pdf
PROCESS help=*
PARM 	INP	TYPE=STRING
PARM 	OUT	TYPE=STRING
PARM	SIZE	TYPE=INTEGER	COUNT=4	DEFAULT=(1,1,0,0)
PARM	SL	TYPE=INTEGER		DEFAULT=1
PARM	SS	TYPE=INTEGER		DEFAULT=1
PARM	NL	TYPE=INTEGER		DEFAULT=0
PARM	NS	TYPE=INTEGER		DEFAULT=0
PARM INVERSE TYPE=KEYWORD VALID="INVERSE" COUNT=(0:1) DEFAULT=--
PARM OFORM KEYWORD VALID=(BYTE,HALF,FULL,REAL,DOUB,COMPLEX) DEFAULT=BYTE
END-PROC
.TITLE
VICAR PROGRAM FFT11
.HELP
PURPOSE

FFT11 is a VICAR applications program which computes the forward 
or inverse complex Fourier Transform on a line-by-line basis.
It is analogous to the program FFT1AP except that it does not use
the array processor and is more flexible in its input format
requirements:  it will allow any data format and most FFT sizes.
.page
EXECUTION:

The following is the execution statement format for FFT11:

	FFT11 IN OUT SIZE INVERSE OFORM

where:	INP	is the input file,
	OUT	is the output file,
	SIZE	is the standard Vicar2 size parameter,
	INVERSE specifies whether or not the transform is inverse,
	OFORM   specifies the output data format for inverse
		 transforms.
.page
OPERATION:

FFT11 uses subroutine DFFT (formerly module RCSFFT) to compute the fast 
fourier transform (FFT) of an input image, line by line.  For computational 
details, see Help (in DCL) for that routine.  It uses an algorithm by 
Richard C. Singleton of Stanford Research Institute.

The FFT output by the program (or input in inverse mode) is in the same
format as that of program FFT1AP.  The output of the transformation performed
by DFFT is not scaled in forward mode, and is scaled by 1/N in the
inverse mode.

If SS-1+NS exceeds the number of samples of the input image, then the input
data will be extrapolated using a cosine function.  The extrapolation
algorithm is identical to that used in FFT1AP, but results may differ slightly
as the extrapolation is performed on REAL*4 data in FFT11, but is rounded
to the input data type in FFT1AP.  This extrapolation uses a buffer of
1024 elements, which is the maximum number of samples by which the input
line can be increased.
.page
RESTRICTIONS:

1. For an inverse transform, the input format must be complex.

2. The dimension of the FFT (i.e., the number of samples of the output in
  forward mode) may be any number, subject to the following constraints:

  a. It may not contain a prime factor greater than 23.

  b. The number of prime factors may not exceed 208.

  c. The square-free portion may not exceed 210.  (A factor P of a number
    N is square-free if it cannot be paired with another identical factor
    of N; i.e., each prime occurring an odd number of times in N is a
    square-free factor of N.  The square-free portion of N is the product
    of its square-free factors.)

E.g., 221 (=13*17) fails because the square-free part exceeds 210, and
202 (=2*101) fails because a prime factor exceeds 23, but 210 (=2*3*5*7)
and 216 (= 2**3 * 3**3) are acceptable.
.page
TIMING:

The time required for the DFFT algorithm depends strongly on whether the
dimension of the FFT, N, is composite.  This dependance goes approximately
as N * SUMF, where SUMF is the sum of the prime factors of N.  Factors of
5 or less are favoured by special coding in the subroutine.

The following times were measured for RCSFFT on the IBM:
	0.842 sec. for N = 2048 (2**10)
	0.933 sec. for N = 2000 (2**4 * 5**3)
	1.343 sec. for N = 2187 (3**7)
	2.042 sec. for N = 2197 (13**3)

As a simple rule of thumb, choosing a power of 2 will be the most efficient.
.page
WRITTEN BY:  L.W.Kamp,  4 Apr.1985

COGNIZANT PROGRAMMER:  L.W.Kamp

HISTORY:

  30aug90 -- changed 'COMPLEX' to 'COMP' in F.T. label.
  03may93 -- made portable
  24Oct96 -- Fixed printing of the INPUT FORMAT field (DFR)

.LEVEL1
.VARI INP
The input VICAR image file.
.VARI OUT
The output VICAR image file.
.VARI SIZE
The VICAR size field.
.VARI SL
Integer - size field starting line
.VARI SS
Integer - Size field starting sample
.VARI NL
Integer - Size field number of lines
.VARI NS
Integer - Size field number of samples
.VARI INVERSE
Keyword - Indicates the inverse mode.
.vari OFORM
Keyword - Output data format
(for Inverse mode only).
.LEVEL2
.VARI INP
A VICAR labelled image file.  In Inverse mode, this must be of COMPLEX
format.
.VARI OUT
The output file.  In Forward mode, this will be of COMPLEX format.
.VARI SIZE
The standard size field defining the area of the input picture that is to
be processed: (SL, SS, NL, NS), or:
 (starting line, stating sample, number of lines, number of samples).

If SS-1+NS exceeds the number of samples of the input image, then the input
data will be extrapolated using a cosine function.  The extrapolation
algorithm is identical to that used in FFT1AP, but results may differ slightly
as the extrapolation is performed on REAL*4 data in FFT11, but is rounded
to the input data type in FFT1AP.

For an Inverse transform, SS must be 1 and NS must equal the input size.
.VARI SL
Starting line of the area to be processed.  See SIZE.
.VARI SS
Starting sample of the area to be processed.  See SIZE.
.VARI NL
Number of lines in the area to be processed.  See SIZE.
.VARI NS
Number of samples in the area to be processed.  See SIZE.
.VARI INVERSE
Indicates that the inverse Fourier Transformation is to be performed
on the input file.   Default: forward transform is performed.
.vari OFORM
This specifies the output image format in the Inverse mode.  It is ignored
in the Forward mode.
$ Return
$!#############################################################################
$Test_File:
$ create tstfft11.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MODULE FFT11
!THIS IS A FOUR PART TEST
!FIRST, THE TRANSFORM OF AN POWER-OF-TWO IMAGE IS CREATED AND CHECKED.
!SECOND, THE INVERSE TRANSFORM OF THE TRANSFORM IS CHECKED
!   FOR BEING IDENTICAL WITH THE ORIGINAL IMAGE.
!THIRD, REPEAT OF #1 WITH A NON-POWER-OF-TWO NUMBER OF SAMPLES.
!FOURTH, REPEAT OF #2 WITH AN IMAGE WITH NON-POWER-OF-TWO
!   NUMBER OF SAMPLES.
!MAKE AN IMAGE WITH # OF SAMPLES A POWER-OF-TWO
gen a nl=10 ns=32
!LIST PART OF THE ORIGINAL IMAGE
list a (1,1,10,10)
!NOW THE DIRECT TRANSFORM
fft11 a b
label-list b
list b (1,1,10,40) 'real4
!THE IBM ANSWER :
!  496. 0. -16. 162.45 -16. 80.437 ......
!  528. 0.  "     "	"    "
!  560. 0.  "     "	"    "
!    .
!    .
!    .
!NOW, THE INVERSE TRANSFORM
fft11 b c 'inve
label-list c
list c (1,1,10,10) 
!THESE NUMBERS SHOULD BE SAME AS THE ORIGINAL IMAGE
!NOW LETS DO A NON-POWER-OF-TWO IMAGE
gen a nl=10 ns=28
fft11 a b
label-list b
list b (1,1,10,40) 'real4
!THE IBM ANSWER :
!  417.  0. -86.59  131.23  -64.78  27.565 ......
!  449.  0.    "	 "	  "	 "
!  481.  0.    "	 "	  "	 "
!    .
!    .
!    .
!NOW, THE INVERSE TRANSFORM
fft11 b c 'inve
list c (1,1,10,10)
end-proc
$ Return
$!#############################################################################
