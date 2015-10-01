$!****************************************************************************
$!
$! Build proc for MIPL module img2ascii
$! VPACK Version 1.8, Thursday, February 22, 2001, 17:33:48
$!
$! Execute by entering:		$ @img2ascii
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
$ write sys$output "*** module img2ascii ***"
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
$ write sys$output "Invalid argument given to img2ascii.com file -- ", primary
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
$   if F$SEARCH("img2ascii.imake") .nes. ""
$   then
$      vimake img2ascii
$      purge img2ascii.bld
$   else
$      if F$SEARCH("img2ascii.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake img2ascii
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @img2ascii.bld "STD"
$   else
$      @img2ascii.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create img2ascii.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack img2ascii.com -
	-s img2ascii.f -
	-i img2ascii.imake -
	-p img2ascii.pdf -
	-t tstimg2ascii.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create img2ascii.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  PROGRAM IMG2ASCII

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM IMG2ASCII
C  PURPOSE ---
C
C	Convert the contents of an image to an ASCII file
C
C  REVISION HISTORY ---
C       23-Feb-2001  ...REA...  Bug fix to print last 12 samples of each line
C
C  INPUT ---
C	INP	File name of image
C
C  OUTPUT ---
C	OUT	An ASCII file
C
C  RESTRICTIONS ---
C
C
C  SUBROUTINES CALLED ---
C	ABEND		Stops execution and makes a clean exit
C	CLOSE		Normal FORTRAN file close
C	OPEN		Normal FORTRAN file open
C	XVMESSAGE       Print a message to the user
C	XVCLOSE		Close a file from within VICAR/TAE
C	XVOPEN		Open a file from within VICAR/TAE
C	XVP		Parameter acquisition routine
C	XVPARM		Extract parameter value
C	XVSIZE		Get window size of incomming image
C	XVUNIT		Get the unit number
C	XVWRIT		Writes an array of data to a file
C
C  COMMENTS ---
C
C)
C
C  MODE DECLARATIONS ---
	INTEGER LINE, COUNT, SL, SS, NL, NS
	INTEGER STAT, UNITIN, UNITOUT, I, J, IEND
	INTEGER STRIP(10000), ZMIN, ZMAX, NLI, NSI
	CHARACTER*72 STRING
	CHARACTER*40 OUTFILE
	CHARACTER*3  NOTE
        CHARACTER*3  STATUS
	LOGICAL TRIM, STATS

C  COMMON STATEMENTS ---
C	None
C
C  LOCAL VARIABLE DESCRIPTIONS ---
C	None
C
C-----------*** BEGINNING OF EXECUTABLE CODE ***-----------------

C		+=================+
C		| INITIALIZATIONS |
C		+=================+

        data unitin/0/, stat/0/, sl/0/, ss/0/, nl/0/, ns/0/
        data nli/0/, nsi/0/, count/0/, strip/10000*0/
        data string/' '/, OUTFILE/' '/, NOTE/' '/,STATUS/' '/

	ZMIN = 32768
	ZMAX = -32767
	STATS = .FALSE.

        CALL IFMESSAGE ('IMG2ASCII version 16-Feb-2001')
        CALL XVEACTION ('SA',' ')

C	Open up the input image

	CALL XVUNIT (UNITIN, 'INP', 1, STAT,' ')
	CALL XVOPEN (UNITIN, STAT,'U_FORMAT','FULL',' ')
	CALL XVSIZE (SL,SS,NL,NS,NLI,NSI)
	CALL XVGET  (UNITIN, STAT, 'NL', NLI,  'NS', NSI,' ')

C	Check for too many lines and/or samples
	IF ((SL.GT.NLI).OR.(SS.GT.NSI)) THEN
	    CALL XVMESSAGE ('Invalid SIZE parameters entered !',' ')
	    GOTO 999
	END IF

	TRIM = .FALSE.
	IF ((SL+NL-1).GT.NLI) THEN
	    TRIM = .TRUE.
	    NL = NLI-SL + 1
	ENDIF

	IF ((SS+NS-1).GT.NSI) THEN
	    TRIM = .TRUE.
	    NS = NSI-SS + 1
	ENDIF

C	Open up the output text file

	CALL XVP('OUT',OUTFILE,COUNT)

C	Open an output text file (no VICAR label)

!       Obtain unit number for output file
	CALL XVUNIT (UNITOUT, 'OUT', 1, STAT,' ')

!       If output file currently exists, Open file as an existing 'OLD' file
	OPEN(UNIT=UNITOUT,FILE=OUTFILE,STATUS='OLD',ERR=10100)
        goto 10101

!       Else open output file as a 'NEW' file
10100	OPEN(UNIT=UNITOUT,FILE=OUTFILE,STATUS='NEW',ERR=999)

C		Should we give file info at the end ?

10101   continue
	CALL XVP('NOTES',NOTE,COUNT)
	IF(NOTE(1:2).EQ.'ON') STATS = .TRUE.

C	   ************ Begin EXECUTION *************

C------------------------ I M G 2 A S C I I ----------------------------

C	Program IMG2ASCII

	DO LINE = SL, NL+SL-1
	    CALL XVREAD (UNITIN,STRIP,STAT,'LINE',LINE,
     *                  'SAMP',SS,'NSAMPS',NS,' ')
	    DO I=1, NS, 12
	      IEND = MIN(NS,I+11)
	      WRITE (UNITOUT,'(1X,12I6)') (STRIP(J),J=I,IEND)
	    END DO

C		Check for the min and max DN values
	    IF (STATS) CALL MINMAX (STRIP,ZMIN,ZMAX,NS)
	END DO

	IF (STATS) THEN
	  WRITE (STRING,'(A,I6,A,I6)') 'Minimum value: ', ZMIN, 
     *		' Maximum value: ', ZMAX
	  CALL XVMESSAGE (STRING,' ')
	  WRITE (STRING,'(A,I4,A,I4)') 
     *         'The output text file is dimensioned ',
     *		NL, ' by ', NS
	  CALL XVMESSAGE (STRING,' ')
	ELSE
	 IF (TRIM) THEN
	  CALL XVMESSAGE 
     *      ('SIZE field exceeded the image size! Output truncated',' ')
	  WRITE (STRING,'(A,I4,A,I4)') 
     *           'The output text file is dimensioned ',
     *		NL, ' by ', NS
	  CALL XVMESSAGE (STRING,' ')
	 END IF
	END IF

	CLOSE (UNIT=UNITOUT)
999     continue	
        CALL XVCLOSE(UNITIN,STAT,' ')

	RETURN
	END

C  -------------------------------------------------------------------

	SUBROUTINE MINMAX (STRIP,ZMIN,ZMAX,NS)

	INTEGER STRIP(*), ZMIN, ZMAX, NS, I

	DO 100 I = 1, NS
	    IF (STRIP(I).LT.ZMIN) ZMIN = STRIP(I)
	    IF (STRIP(I).GT.ZMAX) ZMAX = STRIP(I)
100	CONTINUE

	RETURN
	END
C  ------------------ E N D   O F   S O U R C E ----------------------
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create img2ascii.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM img2ascii

   To Create the build file give the command:

		$ vimake img2ascii			(VMS)
   or
		% vimake img2ascii			(Unix)

************************************************************************/
#define PROGRAM	img2ascii
#define R2LIB
#define MODULE_LIST img2ascii.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create img2ascii.pdf
PROCESS		HELP=*
!
! IMG2ASCII - Create an ASCII text file of DN values from an image
!
PARM INP TYPE=(STRING,72)
PARM OUT TYPE=(STRING,72)
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM NOTES TYPE=(STRING,3) VALID=("ON","OFF") DEFAULT="OFF"
END-PROC
.HELP

PURPOSE

	Create an ASCII file of values from a VICAR image

TAE COMMAND LINE FORMAT

	IMG2ASCII INP OUT PARMS

	Where valid parms are SIZE and NOTES
.PAGE
EXAMPLES

    IMG2ASCII INP=SURFACE.IMG OUT=SURFACE.TXT SIZE=(100,100,50,50) NOTES=ON

	In this example, IMG2ASCII produces an ASCII text file, SURFACE.TXT
of values from the VICAR image SURFACE.IMG starting at line and sample 100
and ending at line and sample 150. Upon program completion, with the
NOTES switch on, the min and max values encountered and the dimension of
the text file written are reported to the user.

.PAGE
OPERATION

	Only the DN values are output to the text file. The line and
sample values are not output to the text file. The program will notify
the user as to the dimension of the newly created text file (in the case
that the user specifies a SIZE larger than the input picture the output
file will trim to the max line and samples of the input picture).


RESTRICTIONS

	Converting binary VICAR images to ASCII text files will create
files that are much larger in disk storage space than the images. An image
100 square that uses 21 blocks will create an ASCII file of 134 blocks.
Currently only BYTE and HALF images function with IMG2ASCII.

Revisions:

  27 June 1994  F. Moss   Add test pdf

  Made portable for UNIX ...  J. Turner (CRI)   5 Sept 1994 
.LEVEL1
.VARIABLE INP
A VICAR image
(BYTE or HALF)
.VARIABLE OUT
ASCII text file
(No VICAR label)
.VARIABLE SIZE
Normal VICAR size field
.VARIABLE NOTES
Switch (ON or OFF)
.LEVEL2
.VARIABLE NOTES
ON and OFF switch
indicating the min
and max values and
the dimension of 
the text file
written to disk.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstimg2ascii.pdf
procedure
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen inp1.dat 100 100
img2ascii inp1.dat out1.dat size=(1,1,20,20)
!! if ($syschar(1) = "UNIX")
!!    ush cat out1.dat
!! else
!!    dcl type out1.dat
!! end-if
gen inp2.dat 50 50
img2ascii inp2.dat out2.dat
!! if ($syschar(1) = "UNIX")
!!    ush cat out2.dat
!! else
!!    dcl type out2.dat
!! end-if
end-proc
$ Return
$!#############################################################################
