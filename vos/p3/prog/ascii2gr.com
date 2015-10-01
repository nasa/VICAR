$!****************************************************************************
$!
$! Build proc for MIPL module ascii2gr
$! VPACK Version 1.8, Wednesday, May 29, 1996, 15:26:47
$!
$! Execute by entering:		$ @ascii2gr
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
$ write sys$output "*** module ascii2gr ***"
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
$ write sys$output "Invalid argument given to ascii2gr.com file -- ", primary
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
$   if F$SEARCH("ascii2gr.imake") .nes. ""
$   then
$      vimake ascii2gr
$      purge ascii2gr.bld
$   else
$      if F$SEARCH("ascii2gr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ascii2gr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ascii2gr.bld "STD"
$   else
$      @ascii2gr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ascii2gr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ascii2gr.com -
	-s ascii2gr.f -
	-p ascii2gr.pdf -
	-i ascii2gr.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ascii2gr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C     READS NUMBERS IN FROM AN ASCII TEXT FILE INTO AN IBIS 3-D GRAPHICS FILE
C
	SUBROUTINE MAIN44
	IMPLICIT NONE

	CHARACTER*72 INNAME, MSGBUF
	REAL X, Y, Z
	INTEGER COUNT, STATUS, I, WRGR

	CALL XVP ('INPUT', INNAME, COUNT)
        OPEN (UNIT=51, STATUS='OLD', FILE=INNAME)

	STATUS = WRGR(1, 1, 3)
	IF ( STATUS .NE. 1 ) CALL SIGNALGR(1,STATUS,1)

	DO I=1,1000000
	    READ (UNIT=51, FMT=*, IOSTAT=STATUS, END=150)  X,Y,Z
	    IF (STATUS .NE. 0) THEN
		WRITE (MSGBUF, '(A,I6)' )  'READ ERROR IN ROW : ', I
		CALL XVMESSAGE (MSGBUF,' ')
	    ELSE
		CALL PUTGR (1, X, Y, Z)
	    END IF
	END DO
C
  150	CONTINUE
	CALL PUTGR (1, 0.0, 0.0, 0.0)
	CALL CLGR (1)
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ascii2gr.pdf
PROCESS HELP=*
 PARM INPUT	TYPE=(STRING,72)
 PARM OUT	TYPE=(STRING,72)
END-PROC
.TITLE
VICAR Program ASCII2GR
.HELP
PURPOSE
ASCII2GR is a program that converts a 3 column ASCII file into an
IBIS 3-D Graphics file.
.LEVEL1
.VARIABLE INPUT
The input ASCII file
.VARIABLE OUT
The output graphics file
.LEVEL2
.VARIABLE INPUT
The input ASCII file.  The file should consist of three columns of
numbers.  Additional columns are ignored. Rows that do not have
three columns of numbers, or have text, produce a warning message and
are then ignored.
.VARIABLE OUT
The output IBIS 3-D graphics file.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create ascii2gr.imake
#define PROGRAM ascii2gr

#define MODULE_LIST ascii2gr.f

#define MAIN_LANG_FORTRAN
#define R2LIB
#define R3LIB

#define USES_FORTRAN

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
