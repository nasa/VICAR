$!****************************************************************************
$!
$! Build proc for MIPL module tplist
$! VPACK Version 1.5, Tuesday, September 21, 1993, 14:36:37
$!
$! Execute by entering:		$ @tplist
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
$ write sys$output "*** module tplist ***"
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
$   if F$SEARCH("tplist.imake") .nes. ""
$   then
$      vimake tplist
$      purge tplist.bld
$   else
$      if F$SEARCH("tplist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tplist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tplist.bld "STD"
$   else
$      @tplist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tplist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tplist.com -
	-s tplist.f -
	-p tplist.pdf -
	-i tplist.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tplist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C     2  MAY 89   ...REA...    INITIAL RELEASE
C    12  AUG 93   ...REA...    PORT TO UNIX
C
	REAL BUF(800)
	CHARACTER*80 PR
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
C
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(
     +	'Tiepoint  New Line   New Sample     Old Line   Old Sample',' ')
	CALL XVMESSAGE(
     +	'--------  --- ----   --- ------     --- ----   --- ------',' ')
C
	DO I=1,10
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    DO N=1,200
		J = 4*N-3
C
		IF (BUF(J) .EQ. 0.0 .AND. BUF(J+1) .EQ. 0.0) GO TO 100
		WRITE (PR,50) N,BUF(J),BUF(J+1),BUF(J+2),BUF(J+3)
   50		FORMAT(I6,F11.2,F12.2,F14.2,F12.2)
		CALL XVMESSAGE(PR,' ')
	    END DO
	END DO
C
  100	CONTINUE
	CALL XVMESSAGE(' ',' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tplist.pdf
process help=*
PARM INP     TYPE=STRING
END-PROC
.TITLE
	Program TPLIST
.HELP
PURPOSE:
      TPLIST produces a listing of tiepoints. It assumes that the input
dataset was created as the first output dataset of PICREG, or is in a
format compatible to it.
.LEVEL1
.VARIABLE INP
input datasat
(first output of PICREG)
.LEVEL2
.VARIABLE INP
The input dataset.  TPLIST assumes that this dataset is of the form generated
by PICREG; REAL format, 10 lines by 800 samples (200 tiepoint pairs of
NL, NS, OL, OS), with the last valid tiepoint followed by zero fill.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create tplist.imake
#define  PROGRAM   tplist

#define MODULE_LIST tplist.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
