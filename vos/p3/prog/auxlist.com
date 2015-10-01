$!****************************************************************************
$!
$! Build proc for MIPL module auxlist
$! VPACK Version 1.5, Monday, March 29, 1993, 13:32:17
$!
$! Execute by entering:		$ @auxlist
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
$ write sys$output "*** module auxlist ***"
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
$   if F$SEARCH("auxlist.imake") .nes. ""
$   then
$      vimake auxlist
$      purge auxlist.bld
$   else
$      if F$SEARCH("auxlist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake auxlist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @auxlist.bld "STD"
$   else
$      @auxlist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create auxlist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack auxlist.com -
	-s auxlist.f -
	-i auxlist.imake -
	-p auxlist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create auxlist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C     29 APR 91   ...REA...    CONVERT TO UNIX/VICAR
C     30 NOV 89   ...REA...    INITIAL RELEASE
C
	REAL BUF(14)
	INTEGER IDN(12)
	CHARACTER*80 PR
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('  Line Blackbody Temp  Band 1    Band 2    Band 
     +3    Band 4    Band 5    Band 6',' ')
	CALL XVMESSAGE('          Low   High  Low High  Low High  Low Hi
     +gh  Low High  Low High  Low High',' ')
C
	IEL = ISL+NL-1   
	DO I=ISL,IEL
	    CALL XVREAD(INUNIT,BUF,ISTAT,'LINE',I,' ')
	    DO J=1,12
		IDN(J) = BUF(J+2)
	    END DO
	    WRITE (PR,50) I,BUF(1),BUF(2),(IDN(J),J=1,12)
   50	    FORMAT(I6,2F7.1,12I5)
	    CALL XVMESSAGE(PR,' ')
	END DO
C
	CALL XVMESSAGE(' ',' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create auxlist.imake
#define  PROGRAM   auxlist

#define MODULE_LIST auxlist.f

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
$ create auxlist.pdf
process help=*
PARM INP     TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
PARM SL INTEGER DEFAULT=1
PARM SS INTEGER DEFAULT=1
PARM NL INTEGER DEFAULT=0
PARM NS INTEGER DEFAULT=0


END-PROC
.TITLE
	Program AUXLIST
.HELP
PURPOSE:
      AUXLIST produces a listing of a TIMS AUX (calibration) file. The
input dataset must be the second output dataset from TIMSLOG.
.LEVEL1
.VARIABLE INP
input datasat
(second output of TIMSLOG)
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
(Ignored)
.VARIABLE NL
Number of lines listed
.VARIABLE NS
(Ignored)
.LEVEL2
.VARIABLE INP
The input dataset.  AUXLIST assumes that this dataset is of the form generated
by TIMSLOG; REAL format, 14 samples.
.VARIABLE SIZE
Standard VICAR size field. All columns are listed; therefore SS and NS are
ignored.
.VARIABLE SL
Starting line
.VARIABLE SS
(Ignored)
.VARIABLE NL
Number of lines listed
.VARIABLE NS
(Ignored)
.END
$ Return
$!#############################################################################
