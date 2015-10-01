$!****************************************************************************
$!
$! Build proc for MIPL module ternary
$! VPACK Version 1.5, Monday, December 13, 1993, 16:03:21
$!
$! Execute by entering:		$ @ternary
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
$ write sys$output "*** module ternary ***"
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
$   if F$SEARCH("ternary.imake") .nes. ""
$   then
$      vimake ternary
$      purge ternary.bld
$   else
$      if F$SEARCH("ternary.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ternary
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ternary.bld "STD"
$   else
$      @ternary.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ternary.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ternary.com -
	-s ternary.f -
	-p ternary.pdf -
	-i ternary.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ternary.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     1  MAY 89   ...REA...    INITIAL RELEASE
C     3  DEC 93   ...REA...    Port to Unix
C
	REAL BUF(30000,3)
	INTEGER ISUBAREA(200),INUNIT(3),IOUT(1001,900)
	CHARACTER*80 MSG
	CHARACTER*4 FMT
C
	SQRT3 = SQRT(3.0)
C								open inputs
	DO I=1,3
	    CALL XVUNIT(INUNIT(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(INUNIT(I),ISTAT,'OPEN_ACT','SA',
     +		        'IO_ACT','SA','U_FORMAT','REAL',' ')
	END DO
C							        get parameters
C								width
	CALL XVPARM('WIDTH',NS,ICNT,IDEF,0)
	NL = NS*SQRT3/2.0 + 0.5
	SAMPOFFSET = (NS+1.0)/2.0 + 0.5
C								format
	CALL XVPARM('FORMAT',FMT,ICNT,IDEF,0)
	IF (FMT.EQ.'BYTE') THEN
	    IBACK = 255
	    MAXVAL = 255
	ELSE
	    IBACK = -1
	    MAXVAL = 32767
	    IF (FMT.EQ.'FULL') MAXVAL=1E9
	END IF
C								area
	CALL XVPARM('AREA',ISUBAREA,N_AREA_PARS,IND,0)
	IF (N_AREA_PARS.EQ.0) THEN
	    N_AREA_PARS = 4
	    ISUBAREA(1) = 1
	    ISUBAREA(2) = 1
	    CALL XVGET(INUNIT(1),ISTAT,'NL',ISUBAREA(3),
     +		       'NS',ISUBAREA(4),' ')
	END IF
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'U_NL',NL,'U_NS',NS,'U_NB',1,
     +		    'U_ORG','BSQ','OP','WRITE','OPEN_ACT','SA',
     +		    'IO_ACT','SA','O_FORMAT',FMT,'U_FORMAT','FULL',' ')
C
C					      zero output array, fill background
	DO I=1,NL
	    LOW = SAMPOFFSET - (I-1)/SQRT3
	    IHI = SAMPOFFSET + (I-1)/SQRT3
	    DO J=1,LOW-1
		IOUT(J,I) = IBACK
	    END DO
	    DO J=LOW,IHI
		IOUT(J,I) = 0
	    END DO
	    DO J=IHI+1,NS
		IOUT(J,I) = IBACK
	    END DO
	END DO
C					             get the areas to process
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' Area(s) Processed:',' ')
	CALL XVMESSAGE(' ',' ')
	DO I=1,N_AREA_PARS,4
	    ISL = ISUBAREA(I)
	    ISS = ISUBAREA(I+1)
	    NLIN = ISUBAREA(I+2)
	    NSIN = ISUBAREA(I+3)
	    WRITE (MSG,100) ISL,ISS,NLIN,NSIN
  100	    FORMAT(I11,3I5)
	    CALL XVMESSAGE(MSG,' ')
C							gather data
	    DO J=1,NLIN
		DO K=1,3
		    CALL XVREAD(INUNIT(K),BUF(1,K),ISTAT,'LINE',J,
     +				'SAMP',ISS,'NSAMPS',NSIN,' ')
		END DO
C
		DO L=1,NSIN
		    X1 = BUF(L,1) + 0.001
		    X2 = BUF(L,2) + 0.001
		    X3 = BUF(L,3) + 0.001
		    ILINE = (1.0-X2/(X1+X2+X3))*(NL-1.0) + 1.5
		    ISAMP = ((X3-X1)*(ILINE-1.0))/((X3+X1)*SQRT3)
     +			    + SAMPOFFSET
		    IOUT(ISAMP,ILINE) = IOUT(ISAMP,ILINE) + 1
		END DO
	    END DO
	END DO
C								write output
	DO I=1,NL
	    DO J=1,NS
		IOUT(J,I) = MIN(IOUT(J,I),MAXVAL)
	    END DO
	    CALL XVWRIT(IOUTUNIT,IOUT(1,I),ISTAT,' ')
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ternary.pdf
process help=*
PARM INP     TYPE=(STRING,40)     COUNT=3
PARM OUT     TYPE=(STRING,40)
PARM WIDTH   TYPE=INTEGER         DEFAULT=101    VALID=(1:1001)
PARM FORMAT  TYPE=KEYWORD         DEFAULT=BYTE   VALID=(BYTE,HALF,FULL)
PARM AREA    TYPE=INTEGER    COUNT=(0:200)                DEFAULT=--
END-PROC
.TITLE
	Program TERNARY
.HELP
PURPOSE:
      TERNARY produces a ternary diagram as an image dataset. The output is 
an equilateral triangle of length specified by the WIDTH parameter. The 
background of the triangle is filled with 255 for byte output and -1 otherwise.
The values within the triangle represent the number of pixels associated with
that location in the ternary diagram.
.LEVEL1
.VARIABLE INP
3 input data sets
.VARIABLE OUT
output data set
.VARIABLE WIDTH
Pixels of resolution
.VARIABLE FORMAT
Output data format
(BYTE, HALF, or FULL)
.VARIABLE AREA
The subareas to be included
in the ternary diagram. Up
to 50 regions (SL,SS,NL,NS)
may be entered. Default is 
to use the entire image.
.LEVEL2
.VARIABLE INP
The three input datasets. They will be represented on the ternary diagram as the
left, top, and right corners, respectively.
.VARIABLE OUT
Output dataset containing the ternary diagram.
.VARIABLE WIDTH
Pixels of resolution. This will be the number of samples in the output dataset.
The number of lines in the output will be sqrt(3)/(2*WIDTH).
.VARIABLE FORMAT
The output data format. Valid options are byte, halfword, or fullword.
.VARIABLE AREA
Sets of (Starting_line, Starting_sample, Number_of_lines, Number_of_samples)
are given to define subareas used to generate the ternary diagram. Up to 50
sets of subareas may be entered.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create ternary.imake
#define  PROGRAM   ternary

#define MODULE_LIST ternary.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
