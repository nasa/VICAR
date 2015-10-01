$!****************************************************************************
$!
$! Build proc for MIPL module rowop
$! VPACK Version 1.9, Monday, December 07, 2009, 17:01:44
$!
$! Execute by entering:		$ @rowop
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
$ write sys$output "*** module rowop ***"
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
$ write sys$output "Invalid argument given to rowop.com file -- ", primary
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
$   if F$SEARCH("rowop.imake") .nes. ""
$   then
$      vimake rowop
$      purge rowop.bld
$   else
$      if F$SEARCH("rowop.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rowop
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rowop.bld "STD"
$   else
$      @rowop.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rowop.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rowop.com -mixed -
	-s rowop.f -
	-p rowop.pdf -
	-i rowop.imake -
	-t tstrowop.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rowop.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C---- VICAR/IBIS PROGRAM ROWOP.
C     PURPOSE:  TO DELETE OR SELECT SELECTED ROWS OF THE INPUT IBIS
C	INTERFACE FILE WHILE COPYING THE FILE
C
C	REVISION 2	FRANK EVANS	MARCH 1986
C			ADDED SELECT OPTION, PUT IN NCOPY PARAMETER,
C			MADE THE PDF AGREE WITH THE CODE
C
C	REVISION 3	FRANK EVANS	JUNE 1987
C			ADDED PICK OPTION, IMPROVED HELP FILE
C
C	REVISION 4	NILES RITTER	MAY 1994
C			PORTED TO UNIX; USES IBIS2 CALLS
c
c       REVISION 5      BARBARA MCGUFFIE  OCTOBER 26, 1998
c			MODIFIED FOR DOUBLE PRECISION DATA
C
	IMPLICIT NONE

	REAL	BUF(100), PREC, RANGE(200), LOWER(100), UPPER(100)
        real*8  dbuf(100)
	INTEGER	KEYCOL(100), NCOPY(2)
	INTEGER STATUS,IBISIN,IBISOUT,USIZE,OUTSIZE
        INTEGER INRECORD,INRECORDD
	BYTE	COL(4000000)
	INTEGER*4 FULLCOL(1000000)
	INTEGER NKEY,DEF,NRANGE,COUNT,NUMCOPIES,INDEXCOL
	INTEGER RUNIT,WUNIT,CLEN,NCOL,I,OUTLEN,IREC
	INTEGER ICOL,PTR,N,INPTR,J,OFFSET,GR1DIM
	LOGICAL*1 KEEP(1000000)
	LOGICAL  DELETE, SELECT, PICK, XVPTST
	CHARACTER*12 ORG,CFORM
	CHARACTER*5 FORMATS(1024)
	EQUIVALENCE(COL,FULLCOL)  !Only used to save space.

        integer doubflag


C	Get the input parameters

	DELETE = XVPTST('DELETE')
	SELECT = XVPTST('SELECT')
	PICK = XVPTST('PICK')
	CALL XVPARM ('KEYCOL', KEYCOL, NKEY, DEF, 100)
	CALL XVPARM ('RANGE', RANGE, NRANGE, DEF, 200)
	CALL XVP ('PREC', PREC, COUNT)
	CALL XVP ('NCOPY', NCOPY, COUNT)
	NUMCOPIES = NCOPY(1)
	INDEXCOL = NCOPY(2)
	IF (COUNT .EQ. 1)  INDEXCOL = 0

c		Open the input interface file
	CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
	CALL XVP('GR1DIM',GR1DIM,DEF)

	CALL IBIS_FILE_OPEN( RUNIT, IBISIN, 'READ',
     +     GR1DIM, 0, ' ', ' ',STATUS )
     	IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)

C    Get the File description	
	CALL IBIS_FILE_GET( IBISIN, 'NR', CLEN, 1, 1)
	CALL IBIS_FILE_GET( IBISIN, 'NC', NCOL, 1, 1)
	CALL IBIS_FILE_GET( IBISIN, 'ORG', ORG, 1, 1)
	CALL IBIS_FILE_GET( IBISIN, 'FORMATS', FORMATS, 1, 1024)
C
C    Check for double precision columns
C
         doubflag =  0
         do i = 1, ncol
             if ( formats(i) .eq. 'DOUB' ) then
                 doubflag = 1 
                 go to 10
             end if
         end do                 


C    Create the Record
 10     continue
        if ( doubflag .eq. 0 ) then
  	    CALL IBIS_RECORD_OPEN( IBISIN, INRECORD, ' ',
     +            KEYCOL, NKEY, 'REAL', STATUS)
        else
  	    CALL IBIS_RECORD_OPEN( IBISIN, INRECORDD, ' ',
     +            KEYCOL, NKEY, 'DOUB', STATUS)
        end if
    	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)

	IF (2*NKEY .NE. NRANGE) THEN
	    CALL XVMESSAGE ('The number of ranges must equal ' //
     * 'the number of key columns.', ' ')
	    CALL ABEND
	ENDIF

	DO I = 1, NKEY
	    LOWER(I) = RANGE(2*I-1) - PREC
	    UPPER(I) = RANGE(2*I) + PREC
	ENDDO

C		Go through each row and flag acceptable rows
	IF (DELETE) THEN
	    OUTLEN = 0
	    DO IREC = 1, CLEN
    		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
                 if ( doubflag .eq. 0 ) then
	    	   CALL IBIS_RECORD_READ(INRECORD,BUF,IREC,STATUS )
                else
	    	   CALL IBIS_RECORD_READ(INRECORDD,DBUF,IREC,STATUS )
                end if
		KEEP(IREC) = .TRUE.
		DO I = 1, NKEY
                  if ( doubflag .eq. 0 ) then
		    IF ( (BUF(I) .GE. LOWER(I)) .AND.
     +		         (BUF(I) .LE. UPPER(I)) )  KEEP(IREC) = .FALSE.
                  else
		    IF ( (DBUF(I) .GE. LOWER(I)) .AND.
     +		         (DBUF(I) .LE. UPPER(I)) ) KEEP(IREC) = .FALSE.
                  end if
		ENDDO
		IF (KEEP(IREC)) OUTLEN = OUTLEN+1
	    ENDDO
	ELSE IF (SELECT) THEN
	    OUTLEN = 0
	    DO IREC = 1, CLEN
                 if ( doubflag .eq. 0 ) then
	    	   CALL IBIS_RECORD_READ(INRECORD,BUF,IREC,STATUS )
                else
	    	   CALL IBIS_RECORD_READ(INRECORDD,DBUF,IREC,STATUS )
                end if
    		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
		KEEP(IREC) = .TRUE.
		DO I = 1, NKEY
                  if ( doubflag .eq. 0 ) then
		    IF (.NOT. ((BUF(I) .GE. LOWER(I)) .AND.
     +		         (BUF(I) .LE. UPPER(I))) )  KEEP(IREC) = .FALSE.
                  else
		    IF ( .NOT. ((DBUF(I) .GE. LOWER(I)) .AND.
     +		         (DBUF(I) .LE. UPPER(I))) ) KEEP(IREC) = .FALSE.
                  end if
		ENDDO
		IF (KEEP(IREC)) OUTLEN = OUTLEN+1
	    ENDDO
	ELSE IF (PICK) THEN
	    OUTLEN = 0
	    DO IREC = 1, CLEN
                 if ( doubflag .eq. 0 ) then
	    	   CALL IBIS_RECORD_READ(INRECORD,BUF,IREC,STATUS )
                else
	    	   CALL IBIS_RECORD_READ(INRECORDD,DBUF,IREC,STATUS )
                end if
    		IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
		KEEP(IREC) = .FALSE.
		DO I = 1, NKEY
                  if ( doubflag .eq. 0 ) then
		    IF (((BUF(I) .GE. LOWER(I)) .AND.
     +		         (BUF(I) .LE. UPPER(I))) )  KEEP(IREC) = .TRUE.
                  else
		    IF (((DBUF(I) .GE. LOWER(I)) .AND.
     +		         (DBUF(I) .LE. UPPER(I))) ) KEEP(IREC) = .TRUE.
                  end if
		ENDDO
		IF (KEEP(IREC)) OUTLEN = OUTLEN+1
	    ENDDO
	ENDIF

        if ( doubflag .eq. 0 ) then
  	     CALL IBIS_RECORD_CLOSE( INRECORD, STATUS )
        else
  	     CALL IBIS_RECORD_CLOSE( INRECORDD, STATUS )
        end if
    	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)


c***************************************************************
c


C		Copy from input to output one column at a time
c		    keeping only the desired rows.
c		If necessary make duplicate copies of the rows
c		    and put out the index numbers.

	CALL XVUNIT(WUNIT,'OUT',1,STATUS,' ')

	CALL IBIS_FILE_OPEN(WUNIT,IBISOUT,'WRITE',
     +       NCOL,NUMCOPIES*OUTLEN, FORMATS, ORG, STATUS)
    	IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(WUNIT,STATUS,1)


	DO ICOL = 1, NCOL
	  IF (ICOL .NE. INDEXCOL) THEN
	     CALL IBIS_COLUMN_GET(IBISOUT,'FORMAT',CFORM ,ICOL,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)

	     IF (CFORM(1:1).EQ.'A') THEN !dont format ASCII
	       CALL IBIS_COLUMN_SET(IBISIN,'U_FORMAT','NONE',ICOL,
     +                              STATUS)
    	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
	       CALL IBIS_COLUMN_SET(IBISOUT,'U_FORMAT','NONE',ICOL,
     +                              STATUS)
	       IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	     ENDIF

	     CALL IBIS_COLUMN_GET(IBISOUT,'U_SIZE', USIZE ,ICOL,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	     CALL IBIS_COLUMN_READ(IBISIN,COL,ICOL,1,CLEN,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)

	     OUTSIZE = OUTLEN * USIZE

	     PTR = 1
	     INPTR = 1

	     DO I = 1, CLEN
               IF (KEEP(I)) THEN
		   DO J = 1, USIZE
			COL(PTR) = COL(INPTR)
			PTR=PTR+1
			INPTR=INPTR+1
		   ENDDO
	       ELSE
	    	   INPTR = INPTR+USIZE
	       ENDIF
  	     ENDDO

	     OFFSET=OUTSIZE
	     DO N = 1, NUMCOPIES-1
		DO I = 1, OUTSIZE
		   COL(OFFSET+I) = COL(I)
		 ENDDO
		 OFFSET=OFFSET+OUTSIZE
	     ENDDO
	     CALL IBIS_COLUMN_WRITE(IBISOUT,COL,
     +          ICOL,1,NUMCOPIES*OUTLEN,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	  ELSE
	     CALL IBIS_COLUMN_SET(IBISOUT,'U_FORMAT','FULL',ICOL,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	     OFFSET=0
	     DO N = 1, NUMCOPIES
	        DO I = 1, OUTLEN
		   FULLCOL(OFFSET+I) = N
	        ENDDO
		OFFSET=OFFSET+OUTLEN
	     ENDDO
	     CALL IBIS_COLUMN_WRITE(IBISOUT,FULLCOL,
     +               ICOL,1,NUMCOPIES*OUTLEN,STATUS)
	     IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)
	  ENDIF
	ENDDO

	CALL IBIS_FILE_CLOSE(IBISIN,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISIN,STATUS,1)
	CALL IBIS_FILE_CLOSE(IBISOUT,' ',STATUS)
	IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBISOUT,STATUS,1)

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create rowop.pdf
PROCESS HELP=*
PARM INP     TYPE=(STRING,72)
PARM OUT     TYPE=(STRING,72)
PARM KEYCOL  TYPE=INTEGER COUNT=(1:100)
PARM RANGE   TYPE=REAL    COUNT=(2:200)
PARM MODE    TYPE=KEYWORD VALID=(DELETE,SELECT,PICK)  DEFAULT=DELETE
PARM PREC    TYPE=REAL    DEFAULT=0.0
PARM NCOPY   TYPE=INTEGER COUNT=1:2 DEFAULT=1
PARM GR1DIM  TYPE=INTEGER COUNT=1 DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program ROWOP
.HELP
PURPOSE

    ROWOP performs row operations on an IBIS interface (tabular) file.
There are three modes of operation:  delete, select, and pick.  The modes
all involve checking the values in a row against an input range
of allowed values.  The delete option deletes the rows that satisfy any
range criterion, the select option selects the rows that satify all range
criteria, and the pick option selects the rows that satisfy any range 
criterion.  The changes are made to the output file, not the input file.  
The number of columns is unchanged.


EXECUTION

ROWOP INP=INPUT.INT OUT=OUTPUT.INT   KEYCOL=(C1,C2,..Cn)  +
			RANGE=(L1,U1, L2,U2, ..., Ln,Un)  'mode

    INP is the input IBIS interface file, and OUT is the output IBIS 
interface file.   KEYCOL are the columns for matching data values.  RANGE 
specifies that the values in column Ci of INP are checked against the 
numeric range designated by Li and Ui.  The precision parameter, PREC, 
specifies by how much the range limits will be enlarged to overcome
precision problems;  the lower limit is lowered by P, and the upper
limit is increased by P.  The default for P is zero.  The 'mode parameter
chooses the mode of operation -- how the ranges will be used to select
the rows.


EXAMPLES

ROWOP INP=A OUT=B KEYCOL=(1,1,5) RANGE=(0,10, 100,1000, 20.35,20.35) PREC=0.01

    The delete mode has been invoked.  For each row if the value in 
column one is between -0.01 and 10.01 or between 99.99 and 1000.01, or 
if the value in column five is between 20.34 and 20.36, then the row 
containing those values will be deleted.


ROWOP INP=A OUT=B KEYCOL=(2,6)  RANGE=( 1.5,3.0, 250,400)  'SELECT

    The select mode has been invoked.  For each row if the value in 
column two is between 1.5 and 3.0 inclusive AND the value in column six 
is between 250 and 400 inclusive then the row will be selected.


ROWOP INP=A OUT=B KEYCOL=(1,2)  RANGE=( 1,1, 10,20)  'PICK

    The pick mode has been invoked.  For each row if the value in 
column one is 1  OR  the value in column two is between 10 and 20 
inclusive then the row will be selected.



ROWOP INP=INPUT.INT OUT=OUTPUT.INT   KEYCOL=(C1,C2,..Cn)  +
			RANGE=(L1,U1, L2,U2, ..., Ln,Un)  +
			NCOPY=(N,ICOL)

    The first number, N, in the NCOPY parameter specifies the number of 
times the accepted rows will be copied, and the second number specifies 
which column to put the index number in.  The index number tells which copy 
the row belongs to (1 for the first copy, 2 for the second, etc.)   If no 
index column is specified then the index numbers will not be put in the file.  
The copies are made by copying the whole block of output rows, not by copying 
each row separately.  If the NCOPY parameter is omitted altogether then just 
one copy will be made.


ROWOP IN=A OUT=B KEYCOL=(2,3) RANGE=(-90,90, 0,360) 'SELECT NCOPY=(5,4)

    The select mode has been invoked.  If the values in column two of IN are
between -90 and 90 and those of column three between 0 and 360, then
the rows containing those values are selected (i.e. only those rows 
satisfying all the criterion are copied to the output file).  Five
copies of the selected rows will be made, with the index number going
into column 4.



RESTRICTIONS

    The number of L,U pairs after RANGE must equal the number of columns 
listed after KEYCOL.  The maximum number of rows from the input interface 
file is limited to 1,000,000.  The maximum number of ranges that can be 
checked is 100.


WRITTEN BY		H. Wilczynski		26 Jan 1977
COGNIZANT PROGRAMMER	N. D. Ritter
REVISION		4			May 1994


.LEVEL1
.VARIABLE INP
Input interface file
.VARIABLE OUT
Output interface file
.VARIABLE KEYCOL
The list of match columns
.VARIABLE RANGE
Lower and upper check limits
.VARIABLE MODE
Keyword for which mode.
DELETE, SELECT, or PICK
.VARIABLE PREC
The precision of the range
limits.
.VARIABLE NCOPY
1. The number of copies
    to make in the output.
2. The index column.
.VARIABLE GR1DIM
Dimension (Graphics-1 only)

.LEVEL2
.VARIABLE INP
INP is the input IBIS interface file.
.VARIABLE OUT
OUT is the output IBIS interface file.
.VARIABLE KEYCOL
KEYCOL is list of columns for matching.
.VARIABLE RANGE
RANGE specifies the values in the columns specified by KEYCOL of INP to be
checked against the numeric range designated by the Lower and Upper limits.
.VARIABLE MODE
The MODE keyword specifies the operation mode (DELETE, SELECT, or PICK).
The DELETE mode deletes those rows that satisfy any of the range criterion.
The SELECT mode selects those rows that satify all of the range criteria.
The PICK mode selects those rows that satisfy any of the range criterion. 
The default mode is DELETE.
.VARIABLE PREC
The precision parameter, PREC, specifies by how much the range limits 
will be enlarged to overcome precision problems;  the lower limit is 
lowered by PREC, and the upper limit is increased by PREC.  
The default for P is zero.  
.VARIABLE NCOPY
    The first number in the NCOPY parameter specifies the number of 
times the accepted rows will be copied, and the second number specifies 
which column to put the index number in.  The index number tells which copy 
the row belongs to (1 for the first copy, 2 for the second, etc.)   If no 
index column is specified then the index numbers will not be put in the file.  
The copies are made by copying the whole block of output rows, not by copying 
each row separately.  If the NCOPY parameter is omitted altogether then just 
one copy will be made.
.VARIABLE GR1DIM
GRAPHICS-1 files do not have any explicit dimensions in the
file label. This allows the user to properly dimension a
graphics file so that it may be used by this program.
IBIS-2 format graphics files do not need or use this.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create rowop.imake
#define PROGRAM rowop

#define MODULE_LIST rowop.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB
$ Return
$!#############################################################################
$Test_File:
$ create tstrowop.pdf
procedure
refgbl $autousage
body
let $autousage="none"
!
! Generate a new IBIS-2 file (and throw in some ASCII)
!
ibis-gen a nr=10 nc=4 format=(FULL,FULL,REAL,A8)
mf a fun=("C1=INDEX","C2=2*C1")
ibis-list a
!
! Test Delete option
rowop a b key=1 range=(2,3) 'delete
ibis-list b
rowop a b key=(1,2) range=(2,3,6,7) 'delete
ibis-list b
!
! Test Select option
rowop a b key=1 range=(2,3) 'select
ibis-list b
rowop a b key=(1,2) range=(2,3,6,7) 'select
ibis-list b
!
! Test Pick option
rowop a b key=1 range=(2,3) 'pick
ibis-list b
rowop a b key=(1,2) range=(2,3,6,7) 'pick
ibis-list b

!
! Generate an old IBIS-1 file 
!
ibis-gen a nr=10 nc=4 'IBIS-1
mf a fun=("C1=INDEX","C2=2*C1")
ibis-list a
!
! Test Delete option
rowop a b key=1 range=(2,3) 'delete
ibis-list b
rowop a b key=(1,2) range=(2,3,6,7) 'delete
ibis-list b
!
! Test Select option
rowop a b key=1 range=(2,3) 'select
ibis-list b
rowop a b key=(1,2) range=(2,3,6,7) 'select
ibis-list b
!
! Test Pick option
rowop a b key=1 range=(2,3) 'pick
ibis-list b
rowop a b key=(1,2) range=(2,3,6,7) 'pick
ibis-list b

!
! Generate an old GRAPHICS-1 file; test GR1DIM
!
ibis-gen a nr=10 nc=4 'IBIS-1 'ROW
mf a fun=("C1=INDEX","C2=2*C1")  GR1DIM=4
ibis-list a GR1DIM=4
!
! Test Delete option
rowop a b key=1 range=(2,3) 'delete GR1DIM=4
ibis-list b GR1DIM=4
rowop a b key=(1,2) range=(2,3,6,7) 'delete GR1DIM=4
ibis-list b GR1DIM=4
!
! Test Select option
rowop a b key=1 range=(2,3) 'select GR1DIM=4
ibis-list b GR1DIM=4
rowop a b key=(1,2) range=(2,3,6,7) 'select GR1DIM=4
ibis-list b GR1DIM=4
!
! Test Pick option
rowop a b key=1 range=(2,3) 'pick GR1DIM=4
ibis-list b GR1DIM=4
rowop a b key=(1,2) range=(2,3,6,7) 'pick GR1DIM=4
ibis-list b GR1DIM=4
!
!
! and now for the double precision
!
!
ibis-gen a nr=10 nc=4 format=(DOUB,DOUB,DOUB,A8)
!mf a fun=("C1=INDEX","C2=2*C1")
mfd a 'index outcols=1
mfd a 'set outcols=2 value=2
mfd a 'mult incols=(1,2) outcols=2
mfd a 'int incols=2 outcols=2
ibis-list a 'format
!
! Test Delete option
rowop a b key=1 range=(2,3) 'delete
ibis-list b  'format
rowop a b key=(1,2) range=(2,3,6,7) 'delete
ibis-list b  'format
!
! Test Select option
rowop a b key=1 range=(2,3) 'select
ibis-list b  'format
rowop a b key=(1,2) range=(2,3,6,7) 'select
ibis-list b  'format
!
! Test Pick option
rowop a b key=1 range=(2,3) 'pick
ibis-list b  'format
rowop a b key=(1,2) range=(2,3,6,7) 'pick
ibis-list b  'format

end-proc
$ Return
$!#############################################################################
