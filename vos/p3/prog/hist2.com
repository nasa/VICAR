$!****************************************************************************
$!
$! Build proc for MIPL module hist2
$! VPACK Version 1.5, Wednesday, April 21, 1993, 14:45:48
$!
$! Execute by entering:		$ @hist2
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
$ write sys$output "*** module hist2 ***"
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
$   if F$SEARCH("hist2.imake") .nes. ""
$   then
$      vimake hist2
$      purge hist2.bld
$   else
$      if F$SEARCH("hist2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hist2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hist2.bld "STD"
$   else
$      @hist2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hist2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hist2.com -
	-s hist2.f -
	-i hist2.imake -
	-p hist2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hist2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	INTEGER IHIST(256,256),ISORT(65536)
	INTEGER*2 IN1(30000),IN2(30000)
	LOGICAL XVPTST,RESCALE,ROOT_SCALE
	CHARACTER*80 PRT
	CHARACTER*4 FORMAT
C								open inputs
	CALL XVUNIT(IUNIT1,'INP',1,ISTAT,' ')
	CALL XVOPEN(IUNIT1,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
	CALL XVUNIT(IUNIT2,'INP',2,ISTAT,' ')
	CALL XVOPEN(IUNIT2,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','HALF',' ')
C								get parameters
	CALL XVPARM('LINC',INCL,ICOUNT,IDEF,0)
	CALL XVPARM('SINC',INCS,ICOUNT,IDEF,0)
	CALL XVPARM('INC',I,ICOUNT,IDEF,0)
	IF (IDEF.EQ.0) THEN
	    INCL = I
	    INCS = I
	END IF
	RESCALE = .NOT.XVPTST('NONE')
	IF (RESCALE) THEN
	    ROOT_SCALE = XVPTST('ROOT')
	    CALL XVPARM('SPIKES',NORM,ICOUNT,IDEF,0)
	    FORMAT = 'BYTE'
	ELSE
	    FORMAT = 'FULL'
	END IF
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IEL = ISL+NL-1
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'U_FORMAT','FULL','O_FORMAT',FORMAT,
     &		    'OP','WRITE','U_NL',256,'U_NS',256,
     &		    'OPEN_ACT','SA','IO_ACT','SA',' ')
C								initialize hist
	DO I=1,256
	    DO J=1,256
		IHIST(I,J) = 0
	    END DO
	END DO
C								form histogram
	DO I=ISL,IEL,INCL
	    CALL XVREAD(IUNIT1,IN1,ISTAT,'LINE',I,'SAMP',ISS,
     &			'NSAMPS',NS,' ')
	    CALL XVREAD(IUNIT2,IN2,ISTAT,'LINE',I,'SAMP',ISS,
     &			'NSAMPS',NS,' ')
	    DO J = 1,NS,INCS
		IHIST(IN1(J)+1,IN2(J)+1) = IHIST(IN1(J)+1,IN2(J)+1) + 1
	    END DO
	END DO
C							do rescaling
C
	IF (RESCALE) THEN
	    CALL MVE(4,65536,IHIST,ISORT,1,1)		! find the normalizing
	    CALL SORTIN(ISORT,65536)			! bin by sorting;
	    X = ISORT(65537-NORM)			! store normalizing
	    WRITE (PRT,100) X				! constant in X
  100	    FORMAT(' Output Normalized to',F9.1)
	    CALL XVMESSAGE(PRT,' ')
	    IF (ROOT_SCALE) THEN			! rescale using sqrt
		DO I=1,256
		    DO J=1,256
			IHIST(I,J) = MIN(255,
     &				        NINT(255.0*SQRT(IHIST(I,J)/X)))
		    END DO
		END DO
	    ELSE					! linear rescaling
		DO I=1,256
		    DO J=1,256
			IHIST(I,J) = MIN(255,NINT(255.0*IHIST(I,J)/X))
		    END DO
		END DO
	    END IF
	END IF
C							write output, reversing
C							the order of the lines
	DO I=256,1,-1
	    CALL XVWRIT(IOUTUNIT,IHIST(1,I),ISTAT,'NSAMPS',256,' ')
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hist2.imake
#define  PROGRAM   hist2

#define MODULE_LIST hist2.f

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
$ create hist2.pdf
PROCESS HELP=*
PARM INP	STRING  COUNT=2
PARM OUT	STRING
PARM SIZE	INTEGER	COUNT=4		DEFAULT=(1,1,0,0)
PARM SL		INTEGER			DEFAULT=1
PARM SS		INTEGER			DEFAULT=1
PARM NL		INTEGER			DEFAULT=0
PARM NS		INTEGER			DEFAULT=0
PARM MODE	KEYWORD	DEFAULT=LINEAR	VALID=(ROOT,NONE,LINEAR)
PARM SPIKES	INTEGER			DEFAULT=1
PARM INC	INTEGER			DEFAULT=1
PARM LINC	INTEGER			DEFAULT=1
PARM SINC	INTEGER			DEFAULT=1
End-proc
 
.TITLE
VICAR PROCESS HIST2
.HELP
PURPOSE:

   HIST2 is a VICAR program that accepts two inputs (byte only) and computes 
a two dimensional histogram (scatterplot). The output is a 256 by 256 image, 
with the first input displayed as the x-axis and the second input displayed
as the y-axis. The brightness of the output pixels corresponds to the 
population of the input DN pairs.
.LEVEL1
.VARI INP
2 input datasets (byte only)
.VARI OUT
256 by 256 histogram image
.VARI SIZE
The portion of the inputs to
be used, if not the entire 
image.
.VARI SL
The first image line to be
used.
.VARI SS
The first sample to be used.
.VARI NL
Number of lines used.
.VARI NS
Number of samples used.
.VARI MODE
Brightness rescaling parameter.
VALID: LINEAR, ROOT, NONE
.VARI SPIKES
Normalize to the Nth most
populous DN pair.
.VARI INC
Sample every nth sample of
every nth line.
.VARI LINC
Sample every nth line.
.VARI SINC
Sample every nth sample.
.LEVEL2
.VARI INP
Two byte image datasets
.VARI OUT
256 by 256 histogram image. The DN at Line i, Sample j corresponds to the
number of pixels with DN j-1 in Input 1 and DN i-1 in Input 2.
.VARI SIZE
The portion of the inputs to be used, if not the entire image.
.VARI SL
The first image line to be used.
.VARI SS
The first sample to be used.
.VARI NL
Number of lines used.
.VARI NS
Number of samples used.
.VARI MODE
By default, the output is normalized by dividing each population by the
population Nth most populous value, as specified by the NORM parameter, then
multiplying by 255.
     If the ROOT keyword is used, the square root is taken prior to multiplying
by 255. This has the effect of compressing the high values and expanding the low
values.
     If the NONE keyword is used, no rescaling or normalization is performed.
The output is the number of pixels in each population. When NONE is specified,
the output is fullword integer, otherwise the output is byte.
.VARI SPIKES
     This parameter is analogous to the SPIKES parameter in HIST (and
elsewhere). The N most populous values are set to 255, and all other values
are rescaled to be in the proper proportion to the Nth most populous value.
.VARI INC
To form the histogram, only every nth sample of every nth line is used,
starting with the first sample of the first line. INC=n has the same effect
as LINC=n and SINC=n. The specification of INC will override the values given
by LINC and SINC.
.VARI LINC
To form the histogram, only every nth line is used, starting with the first
line.
.VARI SINC
To form the histogram, only every nth sample is used, starting with the first
sample.
.END
$ Return
$!#############################################################################
