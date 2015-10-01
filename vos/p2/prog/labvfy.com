$!****************************************************************************
$!
$! Build proc for MIPL module labvfy
$! VPACK Version 1.7, Thursday, July 28, 1994, 08:29:59
$!
$! Execute by entering:		$ @labvfy
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
$ write sys$output "*** module labvfy ***"
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
$ write sys$output "Invalid argument given to labvfy.com file -- ", primary
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
$   if F$SEARCH("labvfy.imake") .nes. ""
$   then
$      vimake labvfy
$      purge labvfy.bld
$   else
$      if F$SEARCH("labvfy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake labvfy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @labvfy.bld "STD"
$   else
$      @labvfy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create labvfy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack labvfy.com -
	-s labvfy.f -
	-i labvfy.imake -
	-p labvfy.pdf -
	-t tstlabvfy.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create labvfy.f
$ DECK/DOLLARS="$ VOKAGLEVE"
        INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44 

	IMPLICIT NONE
	INCLUDE 'pgminc' 	! TAE CONSTANTS & PARAMETERS
	INTEGER  VBLOCK(xprdim)

	INTEGER	UNIT, STATUS, COUNT, TASKDEF
	INTEGER	POS, RESULT, NHIST, N, I
	INTEGER	INSTANCES(1000)
	CHARACTER*8  KEYWORD, TASK, TASKLIST(1000)
	CHARACTER*132	VALUE, LABEL

        CALL IFMESSAGE('LABVFY version 05-SEP-94')

	CALL XVUNIT(UNIT, 'INP', 1, STATUS,' ')
	CALL XVOPEN(UNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',' ')


	CALL XVP ('KEYWORD', KEYWORD, COUNT)
	CALL XVPARM ('TASK', TASK, COUNT, TASKDEF,1)
	CALL XVP ('LABEL', LABEL, COUNT)

	N = 132
	DO WHILE (LABEL(N:N) .EQ. ' ')
	    N = N - 1
	ENDDO

	RESULT = 0

	IF (TASKDEF .EQ. 1) THEN
	    NHIST = 1000
	    CALL XLHINFO (UNIT,TASKLIST,INSTANCES,NHIST,STATUS,' ')
	    DO I = 1, NHIST
		CALL XLGET (UNIT, 'HISTORY', KEYWORD, VALUE, STATUS,
     +			'HIST',TASKLIST(I), 'INSTANCE',INSTANCES(I),
     +                  'FORMAT','STRING',' ' )
		IF (STATUS .EQ. 1) THEN
		    POS = INDEX (VALUE, LABEL(1:N))
		    IF (POS .GT. 0)  RESULT = 1
		ENDIF
	    ENDDO
	ELSE
	    CALL XLGET (UNIT, 'HISTORY', KEYWORD, VALUE, STATUS,
     +			'HIST',TASK,'FORMAT','STRING',' ')
	    IF (STATUS .EQ. 1) THEN
		POS = INDEX (VALUE, LABEL(1:N))
		IF (POS .GT. 0)  RESULT = 1
	    ENDIF
	ENDIF

	CALL XVCLOSE (UNIT, STATUS,' ')


C		CREATE V-BLOCK
	CALL XQINI( VBLOCK, xprdim, xabort)

	CALL XQINTG (VBLOCK, 'RESULT', 1, RESULT, xadd, STATUS)

	CALL XVQOUT( VBLOCK, STATUS)


	RETURN
	END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create labvfy.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM labvfy

   To Create the build file give the command:

		$ vimake labvfy			(VMS)
   or
		% vimake labvfy			(Unix)


************************************************************************/


#define PROGRAM	labvfy
#define R2LIB

#define MODULE_LIST labvfy.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create labvfy.pdf
PROCESS    HELP=*
 PARM INP      TYPE=STRING
 PARM KEYWORD  TYPE=(STRING,8) 
 PARM TASK     TYPE=(STRING,8)  DEFAULT=""
 PARM LABEL    TYPE=STRING
 LOCAL DUMMY   TYPE=INTEGER  INITIAL=0
 PARM RESULT   TYPE=NAME  DEFAULT=DUMMY
END-PROC
.TITLE
VICAR Program LABVFY
.HELP
PURPOSE

    labvfy verifies that the label of an image contains a specified
string, and thus is actually the desired image.  It is meant to
be used in TCL procedures.


EXECUTION 

    local  flag  integer

    labvfy  IMAGE.IMG  KEYWORD="LAB01"  LABEL="218S11" RESULT=FLAG
or
    labvfy  IMAGE.IMG  TASK="CONVIM" KEYWORD="LAB01" LABEL="218S11" RESULT=FLAG

    if (FLAG = 0)
	write "Wrong image!"
    end-if

.PAGE
OPERATION

    labvfy matches the string specified by the LABEL parameter with
the value of the string keyword specified by the KEYWORD parameter.
If the TASK parameter is given then the match is only performed for
the first instance of that task in the history labels.  If the TASK
parameter is defaulted then all tasks are used for the match.  If
the LABEL string is found in any of the tasks then a match will be 
indicated.  The results of the matching are returned to an integer
TCL variable whose name is specified with the RESULT parameter.
A result of 1 is returned for a match, and 0 for no match.


Original Programmer:	Frank Evans		June 1986

Cognizant Programmer:	Frank Evans

Made Portable For UNIX: CRI                     05-SEP-94

.LEVEL1
.VARIABLE INP
The input image
.VARIABLE TASK
The task to check
the history label for.
(Default is all tasks)
.VARIABLE KEYWORD
The name of the keyword
to match (must be a
string keyword)
.VARIABLE LABEL
The string value to match
.VARIABLE RESULT
The name of an integer
TCL variable that will
receive the flag
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlabvfy.pdf
procedure
refgbl $autousage
body
    local flag integer

    let $autousage="none"

    gen a nl=10 ns=10
    f2 a b fun="in1+1"

    write "All of the responses below should begin with"
    write "Correct; If any begin with Error: there is a problem"

    labvfy  a  TASK="F2" KEYWORD="FUNCTION"  LABEL="in1+1" RESULT=FLAG
    write "FLAG=&FLAG"
    if (FLAG = 1)
	write "Error: should not detect FUNCTION=in1+1"
    else
	write "Correct: Image A does not contain in1+1 as FUNCTION"
    end-if

    labvfy  b  TASK="F2" KEYWORD="FUNCTION"  LABEL="in1+1" RESULT=FLAG
    write "FLAG=&FLAG"
    if (FLAG = 1)
	write "Correct: Image B uses in1+1 as FUNCTION"
    else
	write "Error: did not detect FUNCTION=in1+1"
    end-if

    labvfy  b  TASK="GEN" KEYWORD="FUNCTION"  LABEL="in1+1" RESULT=FLAG
    write "FLAG=&FLAG"
    if (FLAG = 1)
	write "Error: should not detect FUNCTION=in1+1 in GEN"
    else
	write "Correct: Image B does not use in1+1 as FUNCTION in GEN"
    end-if

end-proc
$ Return
$!#############################################################################
