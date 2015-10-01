$!****************************************************************************
$!
$! Build proc for MIPL module specfil
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:28:42
$!
$! Execute by entering:		$ @specfil
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
$ write sys$output "*** module specfil ***"
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
$ write sys$output "Invalid argument given to specfil.com file -- ", primary
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
$   if F$SEARCH("specfil.imake") .nes. ""
$   then
$      vimake specfil
$      purge specfil.bld
$   else
$      if F$SEARCH("specfil.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake specfil
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @specfil.bld "STD"
$   else
$      @specfil.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create specfil.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack specfil.com -mixed -
	-s specfil.f -
	-p specfil.pdf -
	-i specfil.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create specfil.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL BUF(33000),OUT(33000),RAW(10000),FLT(10000)
	CHARACTER*4 FORMAT
	CHARACTER*3 ORG
C						   open datasets, get parameters
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	CALL XVBANDS(ISB,NB,NBIN)
	IEL = ISL+NL-1
	IES = ISS+NS-1
	IEB = ISB+NB-1
	CALL XVGET(INUNIT,ISTAT,'FORMAT',FORMAT,'ORG',ORG,' ')
	CALL XVPARM('WEIGHTS',NWTS,ICNT,IDEF,0)
	IF (NWTS .EQ. 2*(NWTS/2) ) NWTS=NWTS+1 		! force NWTS to be odd
C
	IF (NBIN.EQ.1) THEN					! MSS format
	    CALL XVPARM('MSS',NCHAN,ICNT,IDEF,0)
	    IF (NCHAN.EQ.1) THEN
		CALL XVMESSAGE(
     +	       ' The number is bands must be specified for MSS format',
     +		' ')
		CALL ABEND
	    ENDIF
	    NSCHAN = NSIN/NCHAN
	    IF (NS.EQ.NSIN) NS=NS/NCHAN
	    IES = ISS+NS-1
	    IF (NB.EQ.1) NB=NCHAN
	    IOFF = NSCHAN*(ISB-1)
	    CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	    CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +			'OP','WRITE','U_NL',NL,'U_NS',NS*NB,
     +			'U_FORMAT','REAL','O_FORMAT',FORMAT,' ')
C
	    DO I=ISL,IEL
		CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,' ')
		K = 1
		DO J=ISS,IES
		    CALL MVE(7,NB,BUF(IOFF+J),RAW,NSCHAN,1)
		    CALL UNIFLT(7,NB,RAW,FLT,NWTS)
		    CALL MVE(7,NB,FLT,OUT(K),1,NS)
		    K = K+1
		END DO
		CALL XVWRIT(IOUTUNIT,OUT,ISTAT,' ')
	    END DO
	ELSE
C
	    IF (ORG.EQ.'BSQ') THEN
		ORG='BIL'
		CALL XVMESSAGE(
     +		 ' NOTE: File organization has been changed to BIL',' ')
	    END IF
	    CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	    CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'OP','WRITE','U_NL',NL,'U_NS',NS,'U_NB',NB,
     +		    'U_FORMAT','REAL','O_FORMAT',FORMAT,'U_ORG',ORG,' ')
	    IF (ORG.EQ.'BIP') THEN				    ! BIP format
		DO I=ISL,IEL
		    DO J=ISS,IES
			CALL XVREAD(INUNIT,RAW,ISTAT,'LINE',I,'SAMP',J,
     +				    'BAND',ISB,'NBANDS',NB,' ')
			CALL UNIFLT(7,NB,RAW,FLT,NWTS)
			CALL XVWRIT(IOUTUNIT,FLT,ISTAT,' ')
		    END DO
		END DO
	    ELSE 					     ! BIL or BSQ format
		DO I=ISL,IEL
		    N = 1
		    DO J=ISB,IEB
			CALL XVREAD(INUNIT,BUF(N),ISTAT,'LINE',I,
     +				    'SAMP',ISS,'BAND',J,'NSAMPS',NS,' ')
			N = N+NS
		    END DO
		    DO J=1,NS
			CALL MVE(7,NB,BUF(J),RAW,NS,1)
			CALL UNIFLT(7,NB,RAW,FLT,NWTS)
			CALL MVE(7,NB,FLT,OUT(J),1,NS)
		    END DO
		    N = 1
		    DO J=1,NB
			CALL XVWRIT(IOUTUNIT,OUT(N),ISTAT,' ')
			N = N+NS
		    END DO
		END DO
	    END IF
	END IF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create specfil.pdf
PROCESS HELP=*
  PARM INP  	(STRING,40)
  PARM OUT  	(STRING,40)
  PARM SIZE 	INTEGER DEFAULT=--	COUNT=0:4 
  PARM BANDS 	INTEGER DEFAULT=--	COUNT=0:2 
  PARM SL 	INTEGER DEFAULT=1
  PARM SS 	INTEGER DEFAULT=1
  PARM SB 	INTEGER DEFAULT=1
  PARM NL 	INTEGER DEFAULT=0
  PARM NS 	INTEGER DEFAULT=0
  PARM NB 	INTEGER DEFAULT=0
  PARM MSS	INTEGER DEFAULT=1
  PARM WEIGHTS	INTEGER	DEFAULT=3
END-PROC
.HELP
.TITLE
SPECFIL
.HELP
     This program produces a low pass box filtered image in the spectral
dimension for data in MSS or any of the 3-D formats. If the input is a 3-D
file with BSQ organization, on output it will be changed to BIL organization.
.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name
.VARIABLE SIZE
Window into input
.VARIABLE BANDS
Window into input
in band dimension
.VARIABLE SL
Starting line
= size(1)
.VARIABLE SS
Starting sample
= size(2)
.VARIABLE SB
Starting band
= bands(1)
.VARIABLE NL
Number of lines
= size(3)
.VARIABLE NS
Number of samples
= size(4)
.VARIABLE NB
Number of bands
= bands(2)
.VARIABLE MSS
Number of MSS bands,
if MSS format
.VARIABLE WEIGHTS
Width of filter
.LEVEL2
.VARIABLE INP
Name of a single input file, either in MSS or 3-D format
.VARIABLE OUT
Name of a single output file. If the input is a BSQ 3-D file, the output
will be changed to 3-D BIL organization.
.VARIABLE SIZE
The size parameter determines the boundaries in the input
file from which the filter is to take place.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line
.VARIABLE BANDS
The bands parameter determines the bands in the input
file from which the filter is to take place.  It is specified
as (SB,NB), where
	SB is the starting band
	NB is the number of bands to be copied
.VARIABLE SL
The first line of the input image to be processed.
.VARIABLE SS
The first sample location of the input image to be processed.
.VARIABLE SB
The first band of the input image to be filtered.
.VARIABLE NL
The total number of lines to be processed.
.VARIABLE NS
The number of samples of each line to be processed.
.VARIABLE NB
The number of bands to be processed.
.VARIABLE MSS
The number of MSS bands in the input and output images. If the input dataset
is not in MSS format, this parameter is ignored.
.VARIABLE WEIGHTS
The number of elements to be used in computing the output value of each
pixel. This must be an odd integer.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create specfil.imake
#define  PROGRAM   specfil

#define MODULE_LIST specfil.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
