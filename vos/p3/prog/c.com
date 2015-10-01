$!****************************************************************************
$!
$! Build proc for MIPL module c
$! VPACK Version 1.8, Monday, February 07, 2000, 14:15:06
$!
$! Execute by entering:		$ @c
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
$ write sys$output "*** module c ***"
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
$ write sys$output "Invalid argument given to c.com file -- ", primary
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
$   if F$SEARCH("c.imake") .nes. ""
$   then
$      vimake c
$      purge c.bld
$   else
$      if F$SEARCH("c.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake c
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @c.bld "STD"
$   else
$      @c.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create c.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack c.com -
	-s c.f -
	-i c.imake -
	-p c.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create c.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	C -- PROGRAM TO CONVERT BETWEEN DIFFERENT STORAGE FORMATS FOR
C	     IMAGES AND TO PERFORM LINEAR TRANSFORMATIONS ON THEM
C
C  ORIGINAL WRITTEN BY J. J. LORRE
C  5/83 - 6/83	... TRANSLATED FROM PL/1 TO FORTRAN BY DAN STANFILL
C  84-12-17  ...LWK... PARTIAL (I/O ONLY) VICAR2 CONVERSION TO FIX LABEL
C			PROCESSING
C  85-1-26   ...LWK... CONVERT PARAMS TO VICAR2 & FIX BUGS
C  85-2-18   ...LWK... ENABLE 'COMPLEX' TYPE, REMOVE 'FORMAT' PARAMETER,
C			CHECK FOR OVERFLOW IN REAL TO INTEGER CONVERSION
C  89-9-21   ...REA... FIX ERROR IN ROUNDING
C  90-5-23   ...REA... PUT STRETCH IN VICAR LABEL
C  91-4-12   ...REA... MERGE ASU AND PLDSJ1 VERSIONS FOR UNIX
C  00-2-7    ...REA... INCREASE BUFFER SIZES
C
	PROGRAM C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	COMPLEX CIN(20000),COUT(20000)
	REAL*8 DIN(20000),DOUT(20000)
	REAL RIN(40000),ROUT(40000),SLOFF(2),RANG(2,2)
	INTEGER*4 IN(40000),IOUT(40000)
	INTEGER*2 HIN(80000),HOUT(80000)
	CHARACTER*80 MSG
	CHARACTER*7 IFMT, OFMT, UFMT
C
	EQUIVALENCE (CIN,DIN,RIN,IN,HIN), (COUT,DOUT,ROUT,IOUT,HOUT)
	EQUIVALENCE (SLOFF(1),GAIN), (SLOFF(2),OFF)
C
	CALL XVMESSAGE(' C VERSION 5',' ')
C
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,0)
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',0)
	CALL XVGET(INUNIT,ISTAT,'PIX_SIZE',NBPP,'FORMAT',IFMT,0)
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C
	IF (IFMT .EQ. 'WORD') IFMT = 'HALF'
	IF (IFMT .EQ. 'REAL' .AND. NBPP .EQ. 8) IFMT = 'DOUB'
C
	IF (IFMT.EQ.'BYTE') THEN
	    CALL XVCLOSE(INUNIT,ISTAT,0)
	    CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     .	 		'U_FORMAT','HALF',0)
	END IF
C							 OUTPUT FORMAT PARAMETER
	CALL XVPARM('OFORM',OFMT,I,IDEF,0)
	IF (IDEF.EQ.1) THEN
	    IF (IFMT.EQ.'BYTE') THEN
		OFMT = 'HALF'
	    ELSE
		OFMT = 'BYTE'
	    END IF
	END IF
C								  OPEN OUTPUT
	UFMT = OFMT
	IF (OFMT .EQ. 'BYTE') UFMT = 'HALF'
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,0)
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','U_NL',NL, 'U_NS',NS,
     .             'O_FORMAT', OFMT, 'U_FORMAT', UFMT,
     .		   'OPEN_ACT', 'SA', 'IO_ACT', 'SA', 0)
C								  SLOPE/OFFSET
	CALL XVPARM( 'SO', SLOFF, I, IDEF, 0)
	IF (IDEF.EQ.1) THEN
	    CALL XVPARM( 'IRANGE', RANG(1,1), I ,IRDEF, 0)
	    CALL XVPARM( 'ORANGE', RANG(1,2), I ,ORDEF, 0)
	    IF (IRDEF.NE.ORDEF) THEN
		CALL XVMESSAGE(
     +          ' ** SPECIFY BOTH IRANGE/ORANGE, OR NEITHER **',' ')
		CALL ABEND
	    ENDIF
	    IF (IRDEF.EQ.0) THEN
		GAIN = (RANG(1,2)-RANG(2,2))/(RANG(1,1)-RANG(2,1))
		OFF  = RANG(1,2)-GAIN*RANG(1,1)
	    END IF
	END IF
C
	WRITE (MSG,100) GAIN,OFF
  100	FORMAT(' DNout =',F12.5,'*IN',SP,F12.5,S)
	CALL XVMESSAGE(MSG,' ')
	MSG = MSG(9:35)
	CALL XLADD(IOUTUNIT,'HISTORY','DNOUT',MSG,STAT,'FORMAT',
     +		   'STRING',0)
C						  FINAL IN/OUT FORMAT MESSAGE 
	WRITE (MSG,200) IFMT(1:4)
  200	FORMAT(' Input Format = ',A4)
	CALL XVMESSAGE(MSG,' ')
	WRITE (MSG,300) OFMT(1:4)
  300	FORMAT(' Output Format = ',A4)
	CALL XVMESSAGE(MSG,' ')
C
	IF (OFMT .EQ. 'BYTE') THEN
	    MINDN = 0
	    MAXDN = 255
	ELSE IF (OFMT .EQ. 'HALF') THEN
	    MINDN = -32768
	    MAXDN = 32767
	END IF
C								  MAIN LINE LOOP
C							      byte or half input
	IF (IFMT.EQ.'BYTE' .OR. IFMT.EQ.'HALF') THEN
	    IF (OFMT.EQ.'BYTE' .OR. OFMT.EQ.'HALF') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,HIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			HOUT(I) = MIN(MAXDN, MAX(MINDN, 
     +				      NINT(GAIN*HIN(I)+OFF)))
		    END DO
		    CALL XVWRIT(IOUTUNIT,HOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'FULL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,HIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			IOUT(I) = NINT(GAIN*HIN(I)+OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,IOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'REAL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,HIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			ROUT(I) = GAIN*HIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,ROUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'DOUB') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,HIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			DOUT(I) = GAIN*HIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,DOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'COMPLEX') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,HIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			COUT(I) = CMPLX(GAIN*HIN(I) + OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,COUT,ISTAT,0)
		END DO
	    END IF
C								      full input
	ELSE IF (IFMT.EQ.'FULL') THEN
	    IF (OFMT.EQ.'BYTE' .OR. OFMT.EQ.'HALF') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			HOUT(I) = MIN(MAXDN, MAX(MINDN, 
     +				      NINT(GAIN*IN(I)+OFF)))
		    END DO
		    CALL XVWRIT(IOUTUNIT,HOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'FULL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			IOUT(I) = NINT(GAIN*IN(I)+OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,IOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'REAL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			ROUT(I) = GAIN*IN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,ROUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'DOUB') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			DOUT(I) = GAIN*IN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,DOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'COMPLEX') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,IN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			COUT(I) = CMPLX(GAIN*IN(I) + OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,COUT,ISTAT,0)
		END DO
	    END IF
C								      real input
	ELSE IF (IFMT.EQ.'REAL') THEN
	    IF (OFMT.EQ.'BYTE' .OR. OFMT.EQ.'HALF') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,RIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			HOUT(I) = MIN(MAXDN, MAX(MINDN, 
     +				      NINT(GAIN*RIN(I)+OFF)))
		    END DO
		    CALL XVWRIT(IOUTUNIT,HOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'FULL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,RIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			IOUT(I) = NINT(GAIN*RIN(I)+OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,IOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'REAL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,RIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			ROUT(I) = GAIN*RIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,ROUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'DOUB') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,RIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			DOUT(I) = GAIN*RIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,DOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'COMPLEX') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,RIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			COUT(I) = CMPLX(GAIN*RIN(I) + OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,COUT,ISTAT,0)
		END DO
	    END IF
C								real*8 input
	ELSE IF (IFMT.EQ.'DOUB') THEN
	    IF (OFMT.EQ.'BYTE' .OR. OFMT.EQ.'HALF') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,DIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			HOUT(I) = MIN(MAXDN, MAX(MINDN, 
     +				      NINT(GAIN*DIN(I)+OFF)))
		    END DO
		    CALL XVWRIT(IOUTUNIT,HOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'FULL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,DIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			IOUT(I) = NINT(GAIN*DIN(I)+OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,IOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'REAL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,DIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			ROUT(I) = GAIN*DIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,ROUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'DOUB') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,DIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			DOUT(I) = GAIN*DIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,DOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'COMPLEX') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,DIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			COUT(I) = CMPLX(GAIN*DIN(I) + OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,COUT,ISTAT,0)
		END DO
	    END IF
C								complex input
	ELSE IF (IFMT.EQ.'COMPLEX') THEN
	    IF (OFMT.EQ.'BYTE' .OR. OFMT.EQ.'HALF') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,CIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			HOUT(I) = MIN(MAXDN, MAX(MINDN, 
     +				      NINT(GAIN*ABS(CIN(I))+OFF)))
		    END DO
		    CALL XVWRIT(IOUTUNIT,HOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'FULL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,CIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			IOUT(I) = NINT(GAIN*ABS(CIN(I))+OFF)
		    END DO
		    CALL XVWRIT(IOUTUNIT,IOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'REAL') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,CIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			ROUT(I) = GAIN*ABS(CIN(I)) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,ROUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'DOUB') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,CIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			DOUT(I) = GAIN*ABS(CIN(I)) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,DOUT,ISTAT,0)
		END DO
	    ELSE IF (OFMT .EQ. 'COMPLEX') THEN
		DO LINE=ISL,ISL+NL-1
		    CALL XVREAD(INUNIT,CIN,ISTAT,'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,0)
		    DO I=1,NS
			COUT(I) = GAIN*CIN(I) + OFF
		    END DO
		    CALL XVWRIT(IOUTUNIT,COUT,ISTAT,0)
		END DO
	    END IF
        END IF
C
	CALL XVMESSAGE(' CONVERSION COMPLETE',' ')
	CALL XVCLOSE( IUN, STAT, 0)
	CALL XVCLOSE( OUN, STAT, 0)
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create c.imake
#define  PROGRAM   c

#define MODULE_LIST c.f

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
$ create c.pdf
process help=*
!
! FILE NAMES
PARM INP TYPE=STRING COUNT=(1:10)
PARM OUT TYPE=STRING COUNT=(1:4)
!
! SIZE FIELD
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL INTEGER DEFAULT=1
PARM SS INTEGER DEFAULT=1
PARM NL INTEGER DEFAULT=0
PARM NS INTEGER DEFAULT=0
!
! INPUT FILE PARAMETER
PARM IRANGE TYPE=REAL  COUNT=2  DEFAULT=(0,0)
!
! OUTPUT FILE PARAMETERS
PARM OFORM KEYWORD VALID=(BYTE,HALF,FULL,REAL,REAL8,DOUB,COMPLEX)+
 COUNT=0:1 DEFAULT=--
PARM ORANGE TYPE=REAL  COUNT=2  DEFAULT=(0,0)
!
! SLOPE AND OFFSET
PARM SO TYPE=REAL COUNT=2 DEFAULT=(1.,0.)
END-PROC
!
! HELP TEXT
.TITLE
C -- Program to convert image data formats.
.HELP
PURPOSE
  C is a VICAR applications program which converts images between all
  recognized format character types and performs linear transformations
  on images.

EXECUTION

  C  INP  OUT  PARAMS

  where INP and OUT are input and output file parameters and PARAMS are
  other parameter that are described in Tutor mode.

METHOD

  The program reads each image line into a buffer and treats the contents
  of the buffer as data of type implied by the label.  Byte data are read
  in as halfwords.  A simple linear stretch is then applied to these data
  and they are written out in the output format specified.

  If no format code is specified for OUT the following action is taken:
    1.  If input format is BYTE, then output format is HALF.
    2.  If input format is not BYTE, then output format is BYTE.
.page
 COMPLEX DATA:

  The linear relation between IN and OUT must be qualified for complex
  data according to the following table:

       INPUT        OUTPUT       Transformation Type
      Format        Format

      non-complex   complex       OUT = (IN*SL+OF) + 0.0i
      complex       non-complex   OUT = |IN| * SL + OF
      complex       complex       OUT = IN * SL + OF
.page
EXAMPLES

  C A B (1,1,10,20) IRANGE=(10,20) ORANGE=(-20,2000) OFORM=FULL
    Data set B is fullword integer where the values have been scaled
    linearly such that 10 -> -20 and 20 -> 2000.  Only the first 10
    lines and 20 samples of A are passed to B.

  C A B OFORM=HALF SO=(2,0)
    Data set B's format is a halfword integer, with each input value
    multiplied by 2.
.page
RESTRICTIONS

 The maximum permitted line length (bytes per line) is 60,000 for BYTE
 format and 120,000 otherwise.
.page
COGNIZANT PROGRAMMER

Written by: J. J. Lorre    18 April 1980
Converted to VAX by: Dan Stanfill    June 1983
Last Revision:  L. W. Kamp,  19 Feb. 1985
Current Cognizant Programmer:  L. W. Kamp

.LEVEL1
.VARI INP
Input file name.
.VARI OUT
Output file name.
.VARI SIZE
Standard VICAR size field
.VARI SL
Starting line
.vari SS
Starting sample
.vari NL
Number of lines
.vari NS
Number of samples
.VARI IRANGE
Pair of numbers for performing
stretch.  ORANGE must also be
specified.
.VARI OFORM
Output storage format.
.VARI ORANGE
Output range values for stretch.
(See IRANGE.)
.VARI SO
(Slope,offset) for linear
transformations.
.LEVEL2
.VARI INP
 Input file name
.VARI OUT
 Output file name
.VARI SIZE
The size field is specified with four arguments,
      SIZE=(SL,SS,NL,NS)
where:
SL is the starting line of the input file to be read.
SS is the starting sample to be read of each input line.
NL is the number of lines to be read.
NS is the number of samples to be read for each line.
For example, SIZE=(1,1,30,20)
would read the first 30 lines and 20 samples of the input picture.

These parameters can also be specified separately using SL, SS, NL,
and NS.
.VARI OFORM
OFORM (Keyword) specifies the format to which the input is to be converted.
The following are the valid values:

         BYTE              Unsigned 8-bit binary integer.
         HALF              Signed 16-bit binary integer.
         FULL              Signed 32-bit binary integer.
         REAL              VAX single precision floating point (REAL*4).
         DOUB or REAL8     VAX double precision floating point (REAL*8).
         COMPLEX           Two single precision floating point values
                           representing the real and imaginary parts of
                           a complex number (COMPLEX*8).
.VARI SO
     SO          SO is a keyword  followed by  two  values (real or
                 integer)  which specify  the slope (SL) and offset
                 (OF) of the  transformation  to be applied between
                 the input and output data sets in the sense:
                             OUT = IN * SL + OF
                 (Note that for output  formats  of BYTE, HALF, FULL,
                 the specified value of OF will be  increased by 0.5
                 for positive values of OF and will  be decreased by
                 0.5  for  negative  values  of  OF  to  correct for
                 truncation.   This  additional 0.5 will  not appear
                 in the  picture label.)  The default is SO=(1.0,0.0).
.VARI IRANGE
IRANGE  is a  keyword followed  by two values which specify two values
in the input  which  map to two values in the output (ORANGE must also 
be specified).  IRANGE and ORANGE values are converted into equivalent
SO values by "C".  If SO is specified, then IRANGE and ORANGE are ignored.
If none of SO, IRANGE or ORANGE are specified, the default transformation
is performed:  SO=(1.0,0.0).
.VARI ORANGE
ORANGE is a keyword followed by two values which are used in conjunction
with the two values specified by IRANGE in order to determine a slope and
offset for a linear transformation.   See IRANGE.
.END
$ Return
$!#############################################################################
