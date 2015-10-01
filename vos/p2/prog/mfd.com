$!****************************************************************************
$!
$! Build proc for MIPL module mfd
$! VPACK Version 1.8, Thursday, October 29, 1998, 10:53:09
$!
$! Execute by entering:		$ @mfd
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
$ write sys$output "*** module mfd ***"
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
$ write sys$output "Invalid argument given to mfd.com file -- ", primary
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
$   if F$SEARCH("mfd.imake") .nes. ""
$   then
$      vimake mfd
$      purge mfd.bld
$   else
$      if F$SEARCH("mfd.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mfd
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mfd.bld "STD"
$   else
$      @mfd.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mfd.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mfd.com -
	-s mfd.f -
	-p mfd.pdf -
	-i mfd.imake -
	-t tstmfd.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mfd.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c	Fiddle to get around the fact that mf and knuth are both written
c       for floating poing data only
c
c
c       bam 10/98
c


C
	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44
	IMPLICIT NONE

        INTEGER iUNIT, iSTAT, DEF
        INTEGER INCOLS(100),OUTCOLS(100),COLS(100)
        REAL*8 VALUE
	INTEGER	INCOL_COUNT, OUTCOL_COUNT
        INTEGER I,J
        INTEGER IXTRAN
        REAL*8  DMIN,DMAX,SUM
        INTEGER IMIN,IMAX
        INTEGER COUNT    

        logical xvptst

        INTEGER IBIS
        INTEGER RECORDD
        INTEGER NR,NC
	INTEGER	ROW
	REAL*8 ROWBUF(100)
        REAL*8 TEMP(100)

C*********************************************************************


        CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')

        CALL IFMESSAGE('MFD version 20-OCT-98')
        CALL XVEACTION('SA',' ')


C       GET THE INPUT/OUTPUT COLUMN/S

	CALL XVPARM ('INCOLS', INCOLS, INCOL_COUNT, DEF, 100)
	CALL XVPARM ('OUTCOLS',OUTCOLS,OUTCOL_COUNT, DEF, 100)
	CALL XVPARMD('VALUE',VALUE,COUNT,DEF,1)

C	OPEN IBIS INTERFACE FILE

	CALL IBIS_FILE_OPEN(IUNIT,IBIS,'UPDATE',0,0,' ',' ',ISTAT)
        IF (ISTAT.NE.1) CALL IBIS_SIGNAL_U(IUNIT,ISTAT,1)

C       get # of rows and columns

        CALL IBIS_FILE_GET(IBIS,'NC',NC,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',NR,1,1)


       IF(XVPTST('ADD'))  IXTRAN = 1
       IF(XVPTST('SUB'))  IXTRAN = 2
       IF(XVPTST('MULT')) IXTRAN = 3
       IF(XVPTST('DIV'))  IXTRAN = 4
       IF(XVPTST('INT'))  IXTRAN = 5
       IF(XVPTST('ABS'))  IXTRAN = 6
       IF(XVPTST('SQR'))  IXTRAN = 7
       IF(XVPTST('MIN'))  IXTRAN = 8
       IF(XVPTST('MAX'))  IXTRAN = 9
       IF(XVPTST('SET'))  IXTRAN = 10
       IF(XVPTST('INDEX'))IXTRAN = 11
       IF(XVPTST('EQ'))   IXTRAN = 12
       IF(XVPTST('NE'))   IXTRAN = 13
       IF(XVPTST('LE'))   IXTRAN = 14
       IF(XVPTST('LT'))   IXTRAN = 15
       IF(XVPTST('GE'))   IXTRAN = 16
       IF(XVPTST('GT'))   IXTRAN = 17



C*******************************************************************


C      DEFAULT THE COLS ARRAY
       
       DO I = 1,NC      ! GET ALL THE COLUMNS JUST CAUSE IT'S EASIER
           COLS(I) = I 
       END DO                


C      OPEN A RECORD = ROW

       CALL IBIS_RECORD_OPEN(IBIS,RECORDD,'FORMAT:DOUB',
     *      COLS,NC,'DOUB',ISTAT)


C      READ THROUGH ALL THE ROWS

       DO ROW = 1,NR
          CALL IBIS_RECORD_READ(RECORDD, ROWBUF, ROW, ISTAT)
          IF (ISTAT.NE.1) CALL IBIS_SIGNAL(IBIS,ISTAT,1)

          GO TO (201,202,203,204,205,206,207,208,209,210, ! DO FUNCTION
     -        211,212,213,214,215,216,217),IXTRAN

 201      CONTINUE    ! ADD
          DO I = 1, OUTCOL_COUNT
              SUM = 0.0
              DO J = 1, INCOL_COUNT
                  SUM = SUM + ROWBUF(INCOLS(J))
              END DO
          ROWBUF(OUTCOLS(I)) = SUM         
          END DO
          GO TO 300

 202      CONTINUE    ! SUBTRACT
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = ROWBUF(INCOLS(1))-ROWBUF(INCOLS(2))
          END DO
          GO TO 300

 203      CONTINUE    ! MULTIPLY
          DO I = 1, OUTCOL_COUNT
              SUM = 1.0
              DO J = 1, INCOL_COUNT
                  SUM = SUM * ROWBUF(INCOLS(J))
              END DO
          ROWBUF(OUTCOLS(I)) = SUM         
          END DO
          GO TO 300

 204      CONTINUE    ! DIVIDE
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = ROWBUF(INCOLS(1))/ROWBUF(INCOLS(2))
          END DO
          GO TO 300

 205      CONTINUE    ! INT
          IF (INCOL_COUNT .NE. OUTCOL_COUNT) THEN
	   CALL MABEND ('INPUT/OUTPUT COLUMN COUNT MUST BE THE SAME')
  	  ENDIF
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DINT(ROWBUF(INCOLS(I)))
          END DO
          GO TO 300

 206      CONTINUE    ! ABS
          IF (INCOL_COUNT .NE. OUTCOL_COUNT) THEN
	   CALL MABEND ('INPUT/OUTPUT COLUMN COUNT MUST BE THE SAME')
  	  ENDIF
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DABS(ROWBUF(INCOLS(I)))
          END DO
          GO TO 300

 207      CONTINUE    ! SQUARE ROOT
          IF (INCOL_COUNT .NE. OUTCOL_COUNT) THEN
	   CALL MABEND ('INPUT/OUTPUT COLUMN COUNT MUST BE THE SAME')
  	  ENDIF
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DSQRT(ROWBUF(INCOLS(I)))
          END DO
          GO TO 300

 208      CONTINUE    ! MINIMUM
          DO I = 1, NC
              TEMP(I) = ROWBUF(I)
          END DO
          CALL MINMAX(8,NC,TEMP,DMIN,DMAX,IMIN,IMAX)
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DMIN
          END DO
          GO TO 300

 209      CONTINUE    ! MAXIMUM
          DO I = 1, NC
              TEMP(I) = ROWBUF(I)
          END DO
          CALL MINMAX(8,NC,TEMP,DMIN,DMAX,IMIN,IMAX)
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = DMAX
          END DO
          GO TO 300

 210      CONTINUE    ! SET VALUE
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = VALUE
          END DO
          GO TO 300

 211      CONTINUE    ! INDEX
          DO I = 1, OUTCOL_COUNT
              ROWBUF(OUTCOLS(I)) = ROW
              ROWBUF(OUTCOLS(I)) = DINT(ROWBUF(OUTCOLS(I)))
          END DO
          GO TO 300

 212      CONTINUE    ! EQ
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .EQ. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 213      CONTINUE    ! NE
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .NE. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 214      CONTINUE    ! LE
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .LE. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 215      CONTINUE    ! LT
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .LT. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300


 216      CONTINUE    ! GE
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .GE. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO
          GO TO 300

 217      CONTINUE    ! GT
          DO I = 1, OUTCOL_COUNT
              IF ( ROWBUF(INCOLS(1)) .GT. VALUE ) THEN
                   ROWBUF(OUTCOLS(I)) = 1
              ELSE 
                   ROWBUF(OUTCOLS(I)) = 0
              END IF
          END DO

 300      CALL IBIS_RECORD_WRITE(RECORDD,ROWBUF,ROW,ISTAT)
       END DO
  
       CALL IBIS_RECORD_CLOSE(RECORDD,ISTAT)
       CALL IBIS_FILE_CLOSE(IBIS,' ',ISTAT)
       IF (ISTAT.NE.1) CALL IBIS_SIGNAL(IBIS,ISTAT,1)

       RETURN
       END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create mfd.pdf
PROCESS HELP=*
PARM INP TYPE=STRING
PARM INCOLS TYPE=INTEGER COUNT=1:100 DEFAULT=1
PARM OUTCOLS TYPE=INTEGER COUNT=1:100 DEFAULT=1
PARM VALUE TYPE=REAL DEFAULT=0.0
PARM FUNC TYPE=KEYWORD VALID=(ADD,SUB,MULT,DIV,INT,+
ABS,SQR,MIN,MAX,SET,INDEX,EQ,NE,LE,LT,GE,GT)
END-PROC
.TITLE
IBIS program MFD
.HELP
PURPOSE

    MFD is a linited version arithmetic IBIS function program that operates
in double-precision.  Currently, program MF accepts IBIS columns in 'DOUB"
format but performs all calculations using only "real" precision.  MF calls 
KNUTH and XKNUTH which only handle real and complex data. 
     
    MFD preserves full double-precision accuracy and data detail.  However,
only the most basic functional expressions are available in MFD, and only 
one expression is permotted for each call of the program.

EXECUTION:

mfd   int  params

where
int     is the input IBIS Interface (tabular) file opened for update
Params  is a standard VICAR parameter field

The input file is an IBIS interface file which is also used for output. 
The VICAR SIZE file is not used.

.PAGE
The available functions include:

ADD - Add two or more columns together.
SUB - Subtract columns. 
MULT - Multiply columns. 
DIV - Divide columns.
INT - Take the integer value of the columns.
ABS - Determine the absolute value of the columns.
SQR - Find the square root of the columns
MIN - Find the minimum value in the columns.
MAX - Find the maximum value in the columns.
SET - Put the provided VALUE in the columns.
INDEX - Put a run of numbers from 1 to NROW in the column.
.PAGE
For the following logicals, the output column is set to TRUE=1 orFALSE=0
EQ - If the column equals the provided VALUE.
NE - If the column is not equal to the provided VALUE.
LE - If the column is less than or equal to the provided VALUE.
LT - If the column is less than the provided VALUE.
GE - If the column is greater than or equal to the provided VALUE.
GT - If the column is greater than the provided VALUE.
.PAGE
EXAMPLES
MFD        A  INCOLS=(1,2,3)  OUTCOLS=1  'ADD

In this example, the data in columns 1, 2, and 3 are added together, 
and the sum is placed in C1, overwritting the previous data.


MFD       A  INCOLS=(4,5)  OUTCOLS=(6,7)  'INT

The integer values of the data in C4 are put in C6, and the integer
 values of C5 are put in C7.


MFD       A  INCOLS=9  OUTCOL=10  'EQ  VALUE=128

If the value in C9 is equal to 128, then a 1 is placed in C10, 
otherwise a 0 is placed in C10.

.PAGE
MFD       A  OUTCOL=11  'SET  VALUE=999

The number 999 is placed in all rows of C11.


MFD       A  OUTCOL=8  'INDEX

A run of numbers from 1 to the number of rows is placed in C8.

RESTRICTIONS:

The maximum input line length is 255 characters.
The maximum number of columns in the interface file is 100.

HISTORY:

BAM - jpl - October, 1998

.LEVEL1
.VARIABLE INPUT
Input IBIS interface 
data file
.VARIABLE INCOLS
The input column/s on 
which the function will
operate.
.VARIABLE OUTCOLS
The output column/s on 
which the result of the
function will be placed.
.VARIABLE VALUE
Value for comparison in 
logical functions;
value to be inserted into
column for SET function.
.VARIABLE FUNC
ADD,SUB,MULT,DIV,INT,ABS,SQR,MIN
MAX,SET,INDEX,EQ,NE,LE,LT,GE,GT
.LEVEL2
.VARIABLE INPUT
Input IBIS interface data file to be updated.
.VARIABLE INCOLS
The input column/s on which the function will operate.
.VARIABLE OUTCOLS
The output column/s in which the result of the function will be placed.
.VARIABLE VALUE
	Value for comparison in logical functions;
	value to be inserted into column for SET function.
.VARIABLE FUNC
		Function to be performed:

ADD - Add two or more columns together.
SUB - Subtract columns. 
MULT - Multiply columns. 
DIV - Divide columns.
INT - Take the integer value of the columns.
ABS - Determine the absolute value of the columns.
SQR - Find the square root of the columns
MIN - Find the minimum value in the columns.
MAX - Find the maximum value in the columns.
SET - Put the provided VALUE in the columns.
INDEX - Put a run of numbers from 1 to NROW in the column.

For the following logicals, the output column is set to TRUE=1 orFALSE=0

EQ - If the column equals the provided VALUE.
NE - If the column is not equal to the provided VALUE.
LE - If the column is less than or equal to the provided VALUE.
LT - If the column is less than the provided VALUE.
GE - If the column is greater than or equal to the provided VALUE.
GT - If the column is greater than the provided VALUE.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create mfd.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM MFD

   To Create the build file give the command:

		$ vimake mfd			(VMS)
   or
		% vimake mfd			(Unix)


************************************************************************/


#define PROGRAM	mfd
#define R2LIB

#define MODULE_LIST mfd.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define DEBUG
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstmfd.pdf
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar
body

let _onfail="continue"
let $autousage="none"
let $echo="yes"

flag-add NOMESSAGE

!
! Critical to life and happiness is a little test file called
! mfd.int.  It is in /home/bam.
!
!
!
! add
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int incols=(1,3) outcol=1 'add
ibis-list a.int 'format

! subtract
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int incols=(1,3) outcol=1 'sub
ibis-list a.int 'format

! multiply
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int incols=(1,3) outcol=7 'mult
ibis-list a.int 'format

! divide
! after the divide, a should look as new
mfd a.int incols=(1,7) outcol=7 'div
ibis-list a.int 'format

! int! 
mfd a.int incols=2 outcol=3 'int
ibis-list a.int 'format

! absolute value
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int incols=4 outcol=5 'abs
ibis-list a.int 'format

! square root
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int incols=1 outcol=1 'sqr
ibis-list a.int 'format

! minimum value
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int outcol=1 'min
ibis-list a.int 'format

! maximum value
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int outcol=7 'max
ibis-list a.int 'format

! set a value
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int outcol=1 'set value=99999.0
ibis-list a.int 'format

! index
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
mfd a.int outcol=7 'index
ibis-list a.int 'format

!
! for all logicals, there will be two tests
! after the second test, the file should be 
! the same as it started
!
! equal
if ( $syschar(1) = "UNIX" )
	ush cp mfd.int a.int
else
	dcl copy mfd.int a.int
end-if
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=.05 'eq
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=1.0 'eq
ibis-list a.int 'format

! not equal
mfd a.int incol=5 outcol=7 value=1.0 'ne
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=.05 'ne
ibis-list a.int 'format

! less than or equal
mfd a.int incol=5 outcol=7 value=.05 'le
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=1.0 'le
ibis-list a.int 'format

! less than 
mfd a.int incol=5 outcol=7 value=.05 'lt
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=1.0 'lt
ibis-list a.int 'format

! greater than or equal
mfd a.int incol=5 outcol=7 value=2.0 'ge
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=1.0 'ge
ibis-list a.int 'format

! greater than 
mfd a.int incol=5 outcol=7 value=2.0 'gt
ibis-list a.int 'format
mfd a.int incol=5 outcol=7 value=0.5 'gt
ibis-list a.int 'format


flag-delete NOMESSAGE

end-proc
$ Return
$!#############################################################################
