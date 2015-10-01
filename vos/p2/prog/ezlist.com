$!****************************************************************************
$!
$! Build proc for MIPL module ezlist
$! VPACK Version 1.9, Friday, April 13, 2001, 14:58:55
$!
$! Execute by entering:		$ @ezlist
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
$ write sys$output "*** module ezlist ***"
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
$ write sys$output "Invalid argument given to ezlist.com file -- ", primary
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
$   if F$SEARCH("ezlist.imake") .nes. ""
$   then
$      vimake ezlist
$      purge ezlist.bld
$   else
$      if F$SEARCH("ezlist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ezlist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ezlist.bld "STD"
$   else
$      @ezlist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ezlist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ezlist.com -mixed -
	-s ezlist.f -
	-i ezlist.imake -
	-p ezlist.pdf -
	-t tstezlist.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ezlist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  REVISION HISTORY
C   4-97  SP  Made portable for UNIX.
c   4-01  lwk removed RECLEN from OPEN, since it gives errors on linux
c	      and is not needed on VMS

	SUBROUTINE MAIN44
C
C	VICAR PROGRAM EZLIST
C
        PARAMETER (MAXNS = 1024)
	INTEGER	WIDTH,SLEN
	REAL	DATA(MAXNS)
        LOGICAL XVPTST, REMOVEDOTS
	CHARACTER*80 FORMAT, FORM
        character*255 OUTNAME
	CHARACTER*20000 STRING

	REMOVEDOTS = XVPTST ('INTEGER')
	IF (XVPTST('W132')) THEN
	    WIDTH = 132
	ELSE
	    WIDTH = 80
	ENDIF

	CALL XVP ('OUT', OUTNAME, ICNTO)
	IF (ICNTO .GT. 0) THEN
           OPEN (UNIT=1,FILE=OUTNAME,STATUS='UNKNOWN',
     1            ERR=999, IOSTAT=J)
	ENDIF

	CALL XVUNIT(inunit,'INP',1,IST,' ')
	CALL XVOPEN(inunit,IST,'U_FORMAT','REAL','OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')

	IF (IST .NE. 1) GO TO 998
	CALL XVGET(inunit,IST,'NL',NL,'NS',NS,' ')
        IF (NS .GT. MAXNS)
     .     CALL MABEND('ERR: Input NS too large. Try COPY of subimage')

	CALL XVPARM ('FORMAT',FORMAT,ICNT,IDEF,1)
	IF (ICNT .EQ. 0) THEN
	   FORMAT = '(F9.2,2X)'
	END IF
        CALL UPRCASE(FORMAT)    ! INSURE IT IS UPPER CASE.

	IF (INDEX(FORMAT(1:),'F') .EQ. 0) GO TO 996

	LENF = INDEX(FORMAT(1:),' ')-1

C-------if format is missing parens, add them
	IF (FORMAT(1:1) .NE. '(' ) THEN
		FORMAT(1:LENF+2) = '('// FORMAT(1:LENF) //')'
		LENF=LENF+2
	END IF
c-------add Repeat factor of NS to 'run-time' format

	FORM = '(xxxx' // FORMAT(1:LENF) // ')'
	WRITE (FORM(2:5), '(I4)') NS
c
	DO K = 1, NL
            CALL XVREAD(INUNIT,DATA,IST,' ')
	    WRITE (STRING, FORM) (DATA(I), I = 1, NS)
	    IF (REMOVEDOTS)  CALL REMOVEPOINTS (STRING)
            LENS = SLEN(STRING)
	    IF (ICNTO .GT. 0)  THEN
		WRITE (1, '(1X, A)', IOSTAT=J,ERR=999)  STRING(1:LENS)
	    ELSE
		CALL XVMESSAGE ( STRING(1: MIN(LENS,WIDTH)) ,' ')
	    ENDIF
	ENDDO
	CALL XVCLOSE (INUNIT,IST,' ')

	IF (ICNTO .GT. 0) THEN
	    CLOSE (UNIT=1)
	ENDIF

	RETURN
996	CALL XVMESSAGE('NO F IN FORMAT PARAMETER',' ')
	CALL ABEND
998	CALL XVMESSAGE('ERROR OPENING INPUT FILE',' ')
	CALL ABEND
999	CALL XVMESSAGE('ERROR WRITING OUTPUT FILE',' ')
	CALL PRNT(4,1,J,'IOSTAT=.')
	CALL ABEND
	END
	SUBROUTINE REMOVEPOINTS (STRING)
	IMPLICIT NONE
	CHARACTER*(*) STRING
	INTEGER	I

	DO I = 1, LEN(STRING)
	    IF (STRING(I:I+1) .EQ. '. ')  STRING(I:I) = ' '
	ENDDO

	RETURN
	END
	INTEGER FUNCTION SLEN (STRING)
	IMPLICIT NONE
	CHARACTER*(*) STRING
	INTEGER	N

	N = LEN(STRING)
	DO WHILE (N .GT. 1 .AND. STRING(N:N) .EQ. ' ')
	    N = N - 1
	ENDDO
	SLEN = N
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ezlist.imake
#define  PROGRAM   ezlist

#define MODULE_LIST ezlist.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create ezlist.pdf
PROCESS HELP=*
    PARM INP    TYPE=STRING
    PARM OUT    TYPE=STRING      DEFAULT=""
    PARM FORMAT TYPE=(STRING,80) COUNT=0:1 DEFAULT=--
    PARM WIDTH  TYPE=KEYWORD COUNT=1 VALID=(W132,W80) DEFAULT=W80
    PARM DOTS   TYPE=KEYWORD VALID=(FLOATING,INTEGER) DEFAULT=FLOATING

!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=(LIST,ASCII,EZ)

END-PROC
.TITLE
Similar to LIST, but output may be an ASCII text file
.HELP
PURPOSE
    EZLIST lists the contents of VICAR image files to ASCII text files.
    It is similar to LIST but the output may go to a text file and it 
    contains no headers at all.  Thus the ASCII output file is EZ (easy)
    for other programs to read.

EXECUTION

  ezlist I D.LIS FORMAT="(F8.3,2X)" 'W132 'INTEGER
       (Output goes to file named D.LIS.)
  ezlist I
       (Output goes to terminal screen.)

OPERATION
   The output floating point format can be specifed with the FORMAT parameter.
Each data element will have this format applied.  
This parameter is a string with a FORTRAN format specification (any legal
FORTRAN format specification is allowed).  The default format is F9.2,2X
which allows seven columns to be displayed (seven values per 80 character line.)

    If the trailing decimal point for a (Fn.0) format is not desired 
then the 'INTEGER keyword should be used.  This changes all decimal points
(periods) that are followed by a space into a space.  This is the way to
get the "integer" appearance for BYTE, HALF, or FULL data.

EZLIST supports all VICAR data formats.  All data is converted internally
to REAL format when it is read.

Each line of the file is converted to one line of output.  The output
file record length is approximately the FORMAT field with (plus spacing)
times the number of samples.
If no output file is specified, the output is written to the terminal screen
with lines TRUNCATED based on the WIDTH parameter.

RESTRICTIONS:
 1.  The maximum number of samples in the input file is 1024.
 2.  The SIZE field is not supported.  (If you want it, see Cognizant.)
     To run EZLIST on a window of a file, use COPY to copy the window to
     a temporary file.

Original Programmer:   C. Avis  5/94

Cognizant Programmer:   C. Avis  5/94

.LEVEL1
.VARIABLE INP
The input VICAR file.
.VARIABLE OUT
An (optional) output
file for the data.
.VARIABLE FORMAT
The FORTRAN floating point
format (e.g. "(F10.3,2X)")
.VARIABLE WIDTH
Specifies the width of the
terminal output in chars/line.
.VARIABLE DOTS
Specifies to change decimal 
points followed by spaces to 
spaces.
.LEVEL2
.VARIABLE INP
STRING
The input VICAR file.
.VARIABLE OUT
STRING - OPTIONAL
An output non-VICAR ASCII file for the data.
.VARIABLE FORMAT
STRING
The FORTRAN format statement syntax for the output (e.g. "(F10.3,2x)").
Only the floating point format syntax and 'x' syntax is allowed. 
No embedded blanks allowed in the string entered for this parameter.
EACH DATA ELEMENT WILL HAVE THIS FORMAT APPLIED.  That is, whatever the
user enters for FORMAT will be converted to NS(FORMAT) before processing
the records (where NS is the number of samples per line).
Examples:
	FORMAT="(F5.2)"
	FORMAT="F5.2"
	FORMAT="(2X,F5.2)"
.VARIABLE WIDTH
KEYWORD
Specification of the width of the terminal output.  File output is not
affected.
 W80 for 80 chars/line (default)
 W132 for 132 chars/line.
.VARIABLE DOTS
KEYWORD
 INTEGER to change decimal points followed by spaces to spaces.
 FLOATING to leave unchanged- default.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstezlist.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
gen A 5 5 
ezlist A
ezlist A FORMAT="f5.0" 'INT
ezlist A XZ.ZTMP FORMAT="F5.0" 'INT
typetext XZ.ZTMP

gen A 5 5 'HALF
ezlist A FORMAT="F5.0" 'INT

gen A 5 5 'REAL
ezlist A FORMAT="F5.1"
 
gen A 5 20
ezlist A 'W132 FORMAT="F5.0" 'INT

! test output file 
ezlist A 'W132 FORMAT="F5.0" 'INT out=a.txt
if ($syschar(1) = "UNIX")
  ush more a.txt
else
  dcl type a.txt
end-if

end-proc
$ Return
$!#############################################################################
