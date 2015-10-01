$!****************************************************************************
$!
$! Build proc for MIPL module transcol
$! VPACK Version 1.9, Thursday, May 10, 2012, 11:43:19
$!
$! Execute by entering:		$ @transcol
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
$ write sys$output "*** module transcol ***"
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
$ write sys$output "Invalid argument given to transcol.com file -- ", primary
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
$   if F$SEARCH("transcol.imake") .nes. ""
$   then
$      vimake transcol
$      purge transcol.bld
$   else
$      if F$SEARCH("transcol.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake transcol
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @transcol.bld "STD"
$   else
$      @transcol.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create transcol.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack transcol.com -mixed -
	-s transcol.f -
	-i transcol.imake -
	-p transcol.pdf -
	-t tsttranscol.pdf testtranscol.pdf tsttranscol.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create transcol.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C     VICAR2 PROGRAM TRANSCOL   *** REV. C ***
C
C
C**********************************************************************
C
C     UPDATE HISTORY
C
C     REVISION A 
C      MODIFIED FOR VAX CONVERSION BY ASM, 8 AUG 1983
C
C     REVISION B 
C      LMB    4/85      MODIFIED FOR VICAR2
C
C     REVISION C
C      9-94  CRI        MSTP S/W/CONVERSION (VICAR PORTING)
C
C
C**********************************************************************
C
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      INTEGER DATACOL,COUNT,INDEX,IDEF,VDEF,I,FRMCNT
      INTEGER FDEF,NCOL,NDEF,TDEF,MAXTOC
      INTEGER*4 FROM_COL(200), ITOCOL(100), IVALUE(100)
      INTEGER*4 VALUE(100), TOCOL(100)
      LOGICAL*1 TRANS,TRANSC,NZERO,NZEROC
C
      CALL IFMESSAGE('TRANSCOL version 05-SEP-94')
      CALL XVEACTION('SA',' ')
C     GET PARAMETER INFORMATION
C
      CALL XVP ( 'DATACOL', DATACOL, COUNT )
       IF ( COUNT .EQ. 0 ) THEN      
	  CALL XVMESSAGE('DATA KEYWORD ABSENT - PROGRAM TERMINATED',' ')
	  CALL ABEND
       END IF
C
      CALL XVPARM ( 'INDEXCOL',   INDEX, COUNT, IDEF, 1)
C
      CALL XVPARM ( 'VALUES',  IVALUE, COUNT, VDEF, 100)
      DO I = 1, COUNT
       VALUE(I) = IVALUE(I)
      END DO
C
      CALL XVPARM ( 'FROMCOL', FROM_COL, FRMCNT, FDEF, 200 )
C
      CALL XVPARM ( 'NCOL',    NCOL,   COUNT, NDEF, 1)
C
      CALL XVPARM ( 'TOCOL',   ITOCOL, COUNT, TDEF, 100)
C 
      MAXTOC = -1
      DO I = 1, COUNT
          TOCOL(I) = ITOCOL(I)
          IF (TOCOL(I).GT.MAXTOC) MAXTOC=TOCOL(I)
      END DO
C    
      TRANS  = .FALSE.
      TRANSC = .FALSE.
      NZERO  = .TRUE.
      NZEROC = .TRUE.
C
      IF (IDEF .EQ. 1 .OR. INDEX.EQ.0) THEN
	NZEROC = .FALSE.
      ELSE
	TRANSC = .TRUE.
      ENDIF
      IF (VDEF .EQ. 1 .OR. IVALUE(1).EQ.0) THEN
	NZEROC = .FALSE.
      ELSE
	TRANSC = .TRUE.
      ENDIF
      IF (TDEF .EQ. 1 .OR. ITOCOL(1).EQ.0) THEN
	NZEROC = .FALSE.
      ELSE
	TRANSC = .TRUE.
      ENDIF
      IF (FDEF .EQ. 1 .OR. FROM_COL(1).EQ.0) THEN
	NZERO = .FALSE.
      ELSE
	TRANS = .TRUE.
      ENDIF
      IF (NDEF .EQ. 1 .OR. NCOL.EQ.0) THEN
	NZERO = .FALSE.
      ELSE
	TRANS = .TRUE.
      ENDIF
C
      
      IF (TRANS.AND..NOT.TRANSC.AND.NZERO) THEN

	  CALL TRANS2(DATACOL,NCOL,FROM_COL,FRMCNT)
      ELSE IF (.NOT.TRANS.AND.TRANSC.AND.NZEROC) THEN

          CALL TRANSCOL(DATACOL, INDEX, VALUE, TOCOL, COUNT, MAXTOC)
      ELSE
          CALL XVMESSAGE('BAD COMBINATION OF PARAMETERS',' ')
          CALL ABEND
      END IF
C
      RETURN
      END
C
C**********************************************************************
C  IBIS ROUTINE TRANSCOL
C
C  PURPOSE:  CHANGES A VERTICALLY ALIGNED TABULAR FILE TO A DIAGONALLY-
C  TABULAR FILE WITH ZERO FILL.  AGGRG2 CAN BE USED TO PRODUCE A HORIZON
C  ALIGNED FILE.
C
C  USER PARAMETERS:
C
C  DATACOL,K - THE INTEGER K SPECIFIES THE COLUMN WHICH CONTAINS DATA VA
C            TO BE TRANSFERRED TO THE OUTPUT COLUMNS WHEN A MATCH ON THE
C            COLUMN OCCURS.
C  INDEX, 
C  VALUES,N1,...,NW - THE INTEGERS N1,...,NW WILL BE CHECKED AGAINST THE
C            IN THE INDEX COLUMN.
C  TOCOL,M1,...,MW - THE INTEGERS M1,...,MW SPECIFY THE OUTPUT COLUMNS W
C            DATACOL VALUES WILL BE TRANSFERRED TO BASED ON THE VALUE MA
C
C  MODIFIED FOR VAX CONVERSION BY ALAN MAZER  3-AUG-83
C
C     IBIS-2 LIBRARY CALLS ADDED BY MEREDITH COX 17-AUG-94
C
C**********************************************************************
C
      SUBROUTINE TRANSCOL(DATACOL,INDEX,VALUE,TOCOL,COUNT,MAXTOC)
      IMPLICIT NONE
C
      INTEGER MAXTOC,CLEN,ICOL,INDEX,I
      INTEGER IX,COUNT,DATACOL,V
      INTEGER*4 IUNIT, OUNIT, STATUS, INIBIS, OUTIBIS
      INTEGER*4 IN_ROWS,IN_COLUMNS
      INTEGER*4 VALUE(100),TOCOL(100)
      INTEGER*4  OUT_COLUMN(1024)
C
      REAL*4 NULL
      REAL*4 INDATA(250000)
      REAL*4 FCOL1(250000)
C
      INTEGER*2 CS(250000)
C
C     GET UNIT NO. AND OPEN VICAR DATA SETS
C
      NULL = 0.0

      CALL XVUNIT ( IUNIT, 'INP', 1, STATUS,' ' )
      IF ( STATUS .NE. 1 ) THEN
       CALL XVMESSAGE
     &    ('INPUT FILE INITIALIZATION ERROR - PROGRAM TERMINATED',' ')
       CALL ABEND
      END IF
C
      CALL XVUNIT ( OUNIT, 'OUT', 1, STATUS,' ' )
      IF ( STATUS .NE. 1 ) THEN
       CALL XVMESSAGE
     &   ('OUTPUT FILE INITIALIZATION ERROR - PROGRAM TERMINATED',' ')
       CALL ABEND
      END IF
C
      CALL IBIS_FILE_OPEN(IUNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
      IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(IUNIT,STATUS,1)

C     GET THE SIZE

      CALL IBIS_FILE_GET(INIBIS,'NR',IN_ROWS,1,1)
      CALL IBIS_FILE_GET(INIBIS,'NC',IN_COLUMNS,1,1)


C
C    OUTPUT FILE IS OPENED TO A COLUMN LENGTH THAT IS
C    EQUAL TO THE HIGHEST COLUMN NUMBER SPECIFIED IN
C    THE 'TOCOL' PARAMETER.  THIS WILL ALLOW THE OUTPUT
C    TO BE SMALLER, LARGER OR SAME SIZE AS INPUT DEPENDING
C    ON THE MAXIMUM 'TOCOL' VALUE SPECIFIED AS INPUT
C


      CALL IBIS_FILE_OPEN(OUNIT,OUTIBIS,'WRITE',MAXTOC,
     &  IN_ROWS,' ','COLUMN',STATUS)
      IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(OUNIT,STATUS,1)

      CLEN=IN_ROWS

      ! Mark the columns which will be handled special
      CALL ZIA(OUT_COLUMN,MAXTOC*4) !all false
c     DO ICOL=1,MAXTOC
c  above was bug found by Mike Smyth ... should be:
      DO ICOL=1,COUNT
	OUT_COLUMN(TOCOL(ICOL)) = 1
      ENDDO

C   Copy all of the normal columns directly
      DO 100 ICOL=1,MAXTOC
	IF (OUT_COLUMN(ICOL).EQ.1) GOTO 100  !will be processed later
        CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',ICOL,STATUS) 
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
        CALL IBIS_COLUMN_READ(INIBIS,FCOL1,
     &                   ICOL,1,IN_ROWS,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
        CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT','REAL',ICOL,STATUS)
        CALL IBIS_COLUMN_WRITE(OUTIBIS,FCOL1,ICOL,1,IN_ROWS,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
100   CONTINUE


C
C  STORE INDICES IN CS
C

C -- Read in INDEX column; use 'HALF' since 'CS' is INTEGER*2:
      CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','HALF',INDEX,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
      CALL IBIS_COLUMN_READ(INIBIS,CS,
     &                   INDEX,1,IN_ROWS,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

C
C
C  TRANSFER DATA ACCORDING TO CS INDICES.
C

C -- Right now, the data translation is all 'REAL', meaning
C -- That a 'DOUB' or 'COMP' column will lose information.
C -- This will do for now, but really should have different
C -- statements for each 'DOUB','COMP' and 'ASCII' types.

      CALL IBIS_COLUMN_READ(INIBIS,INDATA,
     &                   DATACOL,1,IN_ROWS,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

      DO 810 IX=1,COUNT
        V = VALUE(IX)
        DO 800 I=1,CLEN

C Transfer data if VALUE matches this row's INDEX (CS)
          IF (CS(I).EQ.V) THEN
		 FCOL1(I) = INDATA(I)
	  ELSE
		 FCOL1(I) = NULL
	  ENDIF

800     CONTINUE

      CALL IBIS_COLUMN_WRITE(OUTIBIS,FCOL1,TOCOL(IX),1,IN_ROWS,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
810   CONTINUE
C
C     CLOSE DATA SETS
C
      CALL IBIS_FILE_CLOSE ( INIBIS,' ', STATUS )
      IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(IUNIT,STATUS,1)
C
      CALL IBIS_FILE_CLOSE ( OUTIBIS,' ', STATUS )
      IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(OUNIT,STATUS,1)
C

      RETURN
      END
C
C**********************************************************************
C
C---- IBIS PROGRAM "TRANS2".
C     PURPOSE: TO CONVERT SHORT VERTICALLY ALIGNED
C              COLUMNS OF DATA TO A LONGER COLUMN.
C              THIS PROGRAM IS THE INVERSE OF "TRANSCOL".
C     USER PARAMETERS:
C     FROM_COL,N1,...NK    THE INTEGERS N1,...NK SPECIFY
C                         THE SHORT COLUMNS CONTAINING THE
C                         DATA TO BE TRANSFERED TO LONG COLUMN.
C     DATACOL,M           THE INTEGER M SPECIFIES THE DESTINATION
C                         COLUMN.
C     NCOL,J              THE INTEGER J SPECIFIES THE NUMBER
C                         OF SHORT COLUMNS TO BE PROCESSED.
C                         IF J<M, M COLUMNS WILL BE PROCESSED.
C
C     MODIFIED FOR VAX CONVERSION BY ALAN MAZER  3-AUG-83
C
C     IBIS-2 LIBRARY CALLS ADDED BY NILES RITTER 28-JUN-94
C**********************************************************************
C
      SUBROUTINE TRANS2(DATACOL,NCOL,FROM_COL,FRMCNT)
      IMPLICIT NONE
      
      INTEGER NUM_OUT_COLS,NCOL,INUNIT,OUTUNIT
      INTEGER ICOL,I,K
      INTEGER*4 COL(250000),DATA(250000)
      INTEGER*4 FROM_COL(200),DATACOL,NUM_FROM_COLS
      INTEGER*4 IN_ENTRIES,OUT_ENTRIES
      INTEGER*4 FRMCNT
      INTEGER*4 INIBIS,OUTIBIS,STATUS
C
C---- READ PARAMETERS.
C
      NUM_FROM_COLS = FRMCNT
      NUM_OUT_COLS  = MAX0(NCOL,DATACOL)
C
C---- OPEN FILES, READ COLUMN LENGTH.
C
C
      CALL XVUNIT ( INUNIT, 'INP', 1, STATUS,' ' )
      IF ( STATUS .NE. 1 ) THEN
       CALL XVMESSAGE
     &    ('INPUT FILE INITIALIZATION ERROR - PROGRAM TERMINATED',' ')
       CALL ABEND
      END IF
C
      CALL IBIS_FILE_OPEN(INUNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
      CALL IBIS_FILE_GET(INIBIS,'NR',IN_ENTRIES,1,1)
C
C     COMPUTE COLUMN LENGTH OF OUTPUT 
C
      OUT_ENTRIES = IN_ENTRIES * NUM_FROM_COLS
      CALL XVUNIT ( OUTUNIT, 'OUT', 1, STATUS,' ' )
      IF ( STATUS .NE. 1 ) THEN
       CALL XVMESSAGE
     &   ('OUTPUT FILE INITIALIZATION ERROR - PROGRAM TERMINATED',' ')
       CALL ABEND
      END IF
      CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE', 
     +      NUM_OUT_COLS,OUT_ENTRIES,' ','COLUMN',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)

C
C---- MOVE "FROM"-COLUMNS INTO "DATA"-COLUMN.
C
      DO ICOL = 1,NUM_FROM_COLS
         CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',ICOL,STATUS)
	 IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
         CALL IBIS_COLUMN_READ(INIBIS,COL,
     +                   FROM_COL(ICOL),1,IN_ENTRIES,STATUS)
	 IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
         DO  I = 1,IN_ENTRIES
            DATA((I-1) * NUM_FROM_COLS + ICOL) = COL(I)
         ENDDO
      ENDDO
      CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT','REAL',DATACOL,STATUS)
      CALL IBIS_COLUMN_WRITE(OUTIBIS,DATA,DATACOL,1,OUT_ENTRIES,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
C
C---- STRETCH REMAINING COLUMNS.
C
      DO 300 ICOL = 1,NCOL
         IF(ICOL.EQ.DATACOL) GO TO 300
         CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',ICOL,STATUS)
	 IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
         CALL IBIS_COLUMN_READ(INIBIS,COL,ICOL,1,IN_ENTRIES,STATUS)
	 IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
         DO 310 I = 1,IN_ENTRIES
            DO 311 K = 1,NUM_FROM_COLS
               DATA((I-1) * NUM_FROM_COLS + K) = COL(I)
  311       CONTINUE
  310    CONTINUE
         CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT','REAL',ICOL,STATUS)
         CALL IBIS_COLUMN_WRITE(OUTIBIS,DATA,
     +                  ICOL,1,OUT_ENTRIES,STATUS)
         IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
  300 CONTINUE
C
      CALL IBIS_FILE_CLOSE ( INIBIS,' ', STATUS )
      IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(INUNIT,STATUS,1)
      CALL IBIS_FILE_CLOSE ( OUTIBIS,' ', STATUS )
      IF (STATUS .NE. 1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create transcol.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM transcol

   To Create the build file give the command:

		$ vimake transcol 			(VMS)
   or
		% vimake transcol          		(Unix)


************************************************************************/

#define PROGRAM	transcol
#define R2LIB

#define MODULE_LIST transcol.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77  
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create transcol.pdf
process help=*
PARM INP      TYPE=STRING                              DEFAULT=""
PARM OUT      TYPE=STRING                              DEFAULT=""
PARM NULL     TYPE=REAL      			       DEF=0.0
PARM DATACOL  TYPE=INTEGER                             DEFAULT=0
PARM INDEXCOL TYPE=INTEGER                             DEFAULT=0
PARM VALUES   TYPE=INTEGER    COUNT=1:20               DEFAULT=(0)
PARM TOCOL    TYPE=INTEGER    COUNT=1:20               DEFAULT=(0)
PARM FROMCOL  TYPE=INTEGER    COUNT=1:20               DEFAULT=(0)
PARM NCOL     TYPE=INTEGER                             DEFAULT=0
END-PROC
.TITLE
"transcol"
.HELP
PURPOSE:

"transcol" performs two functions, depending on the options specified.

The first of these is to convert long, vertically-aligned columns
of data in an IBIS-format file into smaller vertical columns based 
on a list of values supplied by the user.  Anytime one of these values
is found in a user-specified index column, the value in a corresponding
data column is transferred to a specified output column.  This operation
may be followed by AGGRG2 to obtain a more compact file.

The second function "transcol" performs is the inverse of the first.
If the relevant options are specified, "transcol" will convert short
vertically-aligned columns of data to a longer column.

.PAGE
EXECUTION:

    Note that if the options specified don't fully and uniquely
specify one of the two modes, an error message will be generated.
Options are listed with their relevant function numbers in 
parentheses in the parameter description section.  All parameters
for a given mode must be specified.

.PAGE
Example 1

transcol INP=A OUT=B INDEX=1 DATA=3 VALUES=(1,2) TOCOL=(5,6)

This TAE command will look for the values 1 and 2 in index column 1
of IBIS file A, and transfer the corresponding data values from data
column 3 to columns 5 and 6.  The modified file is given the name B.
Note that parameters are only checked to four characters.  This is
an example of "transcol's" first mode.  Note, too, that all of the
parameters specified above are required; omitting any of them will
cause an error.  Finally, unused spaces in columns 5 and 6 are 
filled with zeroes.

.PAGE
Example 2

	INDEXCOL    1
	DATACOL	    2
	VALUES      1 2 3
        TOCOL    4 6 8

      Col 1   Col 2    Col 4  Col 6   Col 8

	1	47	47.0	0.0	0.0
	2	29	 0.0   29.0	0.0
	3	32	 0.0	0.0    32.0
	1	27	27.0	0.0	0.0
	2	62 	 0.0   62.0	0.0
	3	66	 0.0	0.0    66.0

.PAGE
Example 3	

transcol INP=B OUT=C FROMCOL=(5,6) DATACOL=7 NCOL=7

This command will cause "transcol" to operate in its second mode, 
taking data from columns 5 and 6 of IBIS-file B and putting the
data in column 7 as illustrated in the next example.  Note that
all of the parameters specified are required, and that mixing any
of these (with the exception of DATACOL) with any of the parameters 
of the first mode will generate an error.  Finally, if NCOL is 
less than DATACOL, DATACOL columns will be processed.

.PAGE
Example 4

	FROMCOL	1 2 3
	DATACOL	6
	NCOL	6

Input file    Col 1   Col 2   Col 3   Col 4   Col 5   Col 6
		1	2	3	0	0	0
		4	5	6	0	0	0
		7	8	9	0	0	0
.PAGE
Output file   Col 1   Col 2   Col 3   Col 4   Col 5   Col 6
		1	2	3	0	0	1
		1	2	3	0	0	2	
		1	2	3	0	0	3
		4	5	6	0	0	4
		4	5	6	0	0	5
		4	5	6	0	0	6
		7	8	9	0	0	7
		7	8	9	0	0	8
		7	8	9	0	0	9

Note that all of the columns of the output file, up to the column
MAX( DATACOL, NCOL) are "stretched" to correspond to the new data 
column.
.PAGE


Restrictions:
The only restrictions are the conventional
restrictions on the size of IBIS interface files.

Maximum column length is 250,000.


WRITTEN BY:  A.L. Zobrist (first mode)     1 December 1976 
	     B. Gokhman  (second mode)     9 October 1980
             L. Bynum (written for VICAR2) 30 April 1985   

COGNIZANT PROGRAMMER:  Niles Ritter

REVISION:  
	8-94 - Meredith Cox (CRI) - Made portable for UNIX

.LEVEL1
.VARIABLE INP
STRING - IBIS input 
         file (1,2)
.VARIABLE OUT
STRING - IBIS output 
         file (1,2)
.VARIABLE DATACOL
INTEGER - Destination/source
          column (1,2)
.VARIABLE INDEXCOL
INTEGER - Indexing 
          column (1)
.VARI NULL
REAL - VALUE OF NULLIFIED ENTRIES

.VARIABLE VALUES
INTEGER - List of values to 
          match (1)
.VARIABLE TOCOL
INTEGER - List of output 
          cols (1)
.VARIABLE FROMCOL
INTEGER - list of short 
          cols (2)
.VARIABLE NCOL
INTEGER - # of short 
          cols (2)
.LEVEL2
.VARIABLE INP
                    Input IBIS Interface file

.VARIABLE OUT 
                    Output IBIS Interface file
.VARIABLE DATACOL
          Integer DATACOL is applicable to both modes. 
          
          In the first, it indicates the column number 
          from which the data values to be moved come 
          from.  

          In the second mode, DATACOL indicates the 
          destination column to which the short col-
          umns will be sent.
.VARIABLE INDEXCOL
          INDEXCOL specifies, for mode 1, which column 
          will be used as an index to match the values 
          specified through the VALUES parameter.
.VARI NULL
	  SPECIFYS THE VALUE OF OTHERWISE ZEROED OUT ENTRIES.
.VARIABLE VALUES
          VALUES is a list of values to match against 
          the index column in mode 1. Each time one of 
          these user-specified values is found in the 
          index column, the data element corresponding 
          to the index column entry is moved to the
          the column specified by TOCOL which corresponds 
          to the value number.
.VARIABLE TOCOL
          TOCOL is the list of columns to which data will
          be sent in mode 1. Whenever the index value for 
          a data element matches one of the values specified 
          by the user through the VALUES parameter, that piece
          of data is moved to a column corresponding to the
          value matched.

          TOCOL, then, must have the same number of entries 
          as VALUES.
.VARIABLE FROMCOL
          FROMCOL is relevant to mode 2 only, and indicates 
          which short columns contain data to be moved to the
          column specified by DATACOL.
.VARIABLE NCOL
          NCOL is the number of short columns to be processed, 
          and so is only applicable in the second mode.  If 
          NCOL is less than integer DATACOL, DATACOL columns 
          will be processed.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttranscol.pdf
procedure
refgbl $autousage
body

let $autousage="none"

! We don't test IBIS-1 with ROW because those
! are graphics files, which won't be supported in
! transcol unless an upgrade is requested. Support
! for IBIS-2 graphics is transparent, but we test it
! here just to make sure.

!Note: non-ported TRANSCOL will only work with "IBIS-1"

testtranscol "IBIS-1" "COLUMN"
testtranscol "IBIS-2" "COLUMN"
testtranscol "IBIS-2" "ROW" 

end-proc
$!-----------------------------------------------------------------------------
$ create testtranscol.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-1"
parm org string def="column"
body

let $autousage="none"
let $echo="y"

write "***testing transcol with Version=&version, Org=&org***"

!Example 1  Test first mode
!
ibis-gen a vers=&version org=&org datacol=(1,3) +
   data=(1,2,2,4,1,6,2,8) nc=6 nr=50
ibis-list a csize=6 nr=5
transcol a b index=1 data=3 values=(1,2) tocol=(5,6)
ibis-list b csize=6 nr=5

!Example 2

ibis-gen a vers=&version org=&org datacol=(1,2, 3) nc=8 nr=50 +
  data=( +
	1	47   5 +
	2	29   7 +
	3	32   9 +
	1	27  11 +
	2	62  13 +
	3	66  15 )

ibis-list a csize=6 nr=5 cols=(1,2,3)
transcol a b +
	INDEXCOL  = 1 +
	DATACOL	  = 2 +
	VALUES    =(1 2 3) +
        TOCOL  =(4 6 8) 
ibis-list b nr=5 cols=(1,2,3,4,6,8) csize=6

!Example 3 -- Second mode:

ibis-gen a vers=&version org=&org datacol=(3,4) nc=5 nr=50 +
  data=( +
	1	47 +
	2	29 +
	3	32 +
	1	27 +
	2	62 +
	3	66 )
ibis-list a csize=6 nr=5
transcol inp=a out=b fromcol=(3,4) datacol=5 ncol=5
ibis-list b csize=6 nr=5

!Example 4
ibis-gen a vers=&version org=&org datacol=(1,2,3) nc=6 nr=50 +
  data=( +
	1	2	3 +
	4	5	6 +
	7	8	9 +
	)
ibis-list a csize=6 nr=4
transcol a b +
	FROMCOL = (1 2 3) +
	DATACOL	= 6 +
        NCOL=6
ibis-list b csize=6 nr=4

let $echo="n"

end-proc
$!-----------------------------------------------------------------------------
$ create tsttranscol.log_solos
tsttranscol
write "***testing transcol with Version=IBIS-1, Org=COLUMN***"
***testing transcol with Version=IBIS-1, Org=COLUMN***
ibis-gen a vers=IBIS-1 org=COLUMN datacol=(1,3)  +
   data=(1,2,2,4,1,6,2,8) nc=6 nr=50
Beginning VICAR task ibis
ibis-list a csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  0.00  2.00  0.00  0.00  0.00
  2.00  0.00  4.00  0.00  0.00  0.00
  1.00  0.00  6.00  0.00  0.00  0.00
  2.00  0.00  8.00  0.00  0.00  0.00
  0.00  0.00  0.00  0.00  0.00  0.00
transcol a b index=1 data=3 values=(1,2) tocol=(5,6)
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  0.00  2.00  0.00  2.00  0.00
  2.00  0.00  4.00  0.00  0.00  4.00
  1.00  0.00  6.00  0.00  6.00  0.00
  2.00  0.00  8.00  0.00  0.00  8.00
  0.00  0.00  0.00  0.00  0.00  0.00
ibis-gen a vers=IBIS-1 org=COLUMN datacol=(1,2, 3) nc=8 nr=50  +
  data=(  +
	1	47   5  +
	2	29   7  +
	3	32   9  +
	1	27  11  +
	2	62  13  +
	3	66  15 )
Beginning VICAR task ibis
ibis-list a csize=6 nr=5 cols=(1,2,3)
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 8       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----
   C:1   C:2   C:3
+-----+-----+-----
  1.00 47.00  5.00
  2.00 29.00  7.00
  3.00 32.00  9.00
  1.00 27.00 11.00
  2.00 62.00 13.00
transcol a b  +
	INDEXCOL  = 1  +
	DATACOL	  = 2  +
	VALUES    =(1 2 3)  +
        TOCOL  =(4 6 8)
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b nr=5 cols=(1,2,3,4,6,8) csize=6
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 8       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:6   C:8
+-----+-----+-----+-----+-----+-----
  1.00 47.00  5.00 47.00  0.00  0.00
  2.00 29.00  7.00  0.00 29.00  0.00
  3.00 32.00  9.00  0.00  0.00 32.00
  1.00 27.00 11.00 27.00  0.00  0.00
  2.00 62.00 13.00  0.00 62.00  0.00
ibis-gen a vers=IBIS-1 org=COLUMN datacol=(3,4) nc=5 nr=50  +
  data=(  +
	1	47  +
	2	29  +
	3	32  +
	1	27  +
	2	62  +
	3	66 )
Beginning VICAR task ibis
ibis-list a csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 5       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5
+-----+-----+-----+-----+-----
  0.00  0.00  1.00 47.00  0.00
  0.00  0.00  2.00 29.00  0.00
  0.00  0.00  3.00 32.00  0.00
  0.00  0.00  1.00 27.00  0.00
  0.00  0.00  2.00 62.00  0.00
transcol inp=a out=b fromcol=(3,4) datacol=5 ncol=5
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5
+-----+-----+-----+-----+-----
  0.00  0.00  1.00 47.00  1.00
  0.00  0.00  1.00 47.00 47.00
  0.00  0.00  2.00 29.00  2.00
  0.00  0.00  2.00 29.00 29.00
  0.00  0.00  3.00 32.00  3.00
ibis-gen a vers=IBIS-1 org=COLUMN datacol=(1,2,3) nc=6 nr=50  +
  data=(  +
	1	2	3  +
	4	5	6  +
	7	8	9  +
	)
Beginning VICAR task ibis
ibis-list a csize=6 nr=4
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-1  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  2.00  3.00  0.00  0.00  0.00
  4.00  5.00  6.00  0.00  0.00  0.00
  7.00  8.00  9.00  0.00  0.00  0.00
  0.00  0.00  0.00  0.00  0.00  0.00
transcol a b  +
	FROMCOL = (1 2 3)  +
	DATACOL	= 6  +
        NCOL=6
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=4
Beginning VICAR task ibis
 
Number of Rows:150  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  2.00  3.00  0.00  0.00  1.00
  1.00  2.00  3.00  0.00  0.00  2.00
  1.00  2.00  3.00  0.00  0.00  3.00
  4.00  5.00  6.00  0.00  0.00  4.00
let $echo="n"
write "***testing transcol with Version=IBIS-2, Org=COLUMN***"
***testing transcol with Version=IBIS-2, Org=COLUMN***
ibis-gen a vers=IBIS-2 org=COLUMN datacol=(1,3)  +
   data=(1,2,2,4,1,6,2,8) nc=6 nr=50
Beginning VICAR task ibis
ibis-list a csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  0.00  2.00  0.00  0.00  0.00
  2.00  0.00  4.00  0.00  0.00  0.00
  1.00  0.00  6.00  0.00  0.00  0.00
  2.00  0.00  8.00  0.00  0.00  0.00
  0.00  0.00  0.00  0.00  0.00  0.00
transcol a b index=1 data=3 values=(1,2) tocol=(5,6)
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  0.00  2.00  0.00  2.00  0.00
  2.00  0.00  4.00  0.00  0.00  4.00
  1.00  0.00  6.00  0.00  6.00  0.00
  2.00  0.00  8.00  0.00  0.00  8.00
  0.00  0.00  0.00  0.00  0.00  0.00
ibis-gen a vers=IBIS-2 org=COLUMN datacol=(1,2, 3) nc=8 nr=50  +
  data=(  +
	1	47   5  +
	2	29   7  +
	3	32   9  +
	1	27  11  +
	2	62  13  +
	3	66  15 )
Beginning VICAR task ibis
ibis-list a csize=6 nr=5 cols=(1,2,3)
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 8       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----
   C:1   C:2   C:3
+-----+-----+-----
  1.00 47.00  5.00
  2.00 29.00  7.00
  3.00 32.00  9.00
  1.00 27.00 11.00
  2.00 62.00 13.00
transcol a b  +
	INDEXCOL  = 1  +
	DATACOL	  = 2  +
	VALUES    =(1 2 3)  +
        TOCOL  =(4 6 8)
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b nr=5 cols=(1,2,3,4,6,8) csize=6
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 8       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:6   C:8
+-----+-----+-----+-----+-----+-----
  1.00 47.00  5.00 47.00  0.00  0.00
  2.00 29.00  7.00  0.00 29.00  0.00
  3.00 32.00  9.00  0.00  0.00 32.00
  1.00 27.00 11.00 27.00  0.00  0.00
  2.00 62.00 13.00  0.00 62.00  0.00
ibis-gen a vers=IBIS-2 org=COLUMN datacol=(3,4) nc=5 nr=50  +
  data=(  +
	1	47  +
	2	29  +
	3	32  +
	1	27  +
	2	62  +
	3	66 )
Beginning VICAR task ibis
ibis-list a csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5
+-----+-----+-----+-----+-----
  0.00  0.00  1.00 47.00  0.00
  0.00  0.00  2.00 29.00  0.00
  0.00  0.00  3.00 32.00  0.00
  0.00  0.00  1.00 27.00  0.00
  0.00  0.00  2.00 62.00  0.00
transcol inp=a out=b fromcol=(3,4) datacol=5 ncol=5
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5
+-----+-----+-----+-----+-----
  0.00  0.00  1.00 47.00  1.00
  0.00  0.00  1.00 47.00 47.00
  0.00  0.00  2.00 29.00  2.00
  0.00  0.00  2.00 29.00 29.00
  0.00  0.00  3.00 32.00  3.00
ibis-gen a vers=IBIS-2 org=COLUMN datacol=(1,2,3) nc=6 nr=50  +
  data=(  +
	1	2	3  +
	4	5	6  +
	7	8	9  +
	)
Beginning VICAR task ibis
ibis-list a csize=6 nr=4
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  2.00  3.00  0.00  0.00  0.00
  4.00  5.00  6.00  0.00  0.00  0.00
  7.00  8.00  9.00  0.00  0.00  0.00
  0.00  0.00  0.00  0.00  0.00  0.00
transcol a b  +
	FROMCOL = (1 2 3)  +
	DATACOL	= 6  +
        NCOL=6
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=4
Beginning VICAR task ibis
 
Number of Rows:150  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  2.00  3.00  0.00  0.00  1.00
  1.00  2.00  3.00  0.00  0.00  2.00
  1.00  2.00  3.00  0.00  0.00  3.00
  4.00  5.00  6.00  0.00  0.00  4.00
let $echo="n"
write "***testing transcol with Version=IBIS-2, Org=ROW***"
***testing transcol with Version=IBIS-2, Org=ROW***
ibis-gen a vers=IBIS-2 org=ROW datacol=(1,3)     +
data=(1,2,2,4,1,6,2,8) nc=6 nr=50
Beginning VICAR task ibis
ibis-list a csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  0.00  2.00  0.00  0.00  0.00
  2.00  0.00  4.00  0.00  0.00  0.00
  1.00  0.00  6.00  0.00  0.00  0.00
  2.00  0.00  8.00  0.00  0.00  0.00
  0.00  0.00  0.00  0.00  0.00  0.00
transcol a b index=1 data=3 values=(1,2) tocol=(5,6)
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  0.00  2.00  0.00  2.00  0.00
  2.00  0.00  4.00  0.00  0.00  4.00
  1.00  0.00  6.00  0.00  6.00  0.00
  2.00  0.00  8.00  0.00  0.00  8.00
  0.00  0.00  0.00  0.00  0.00  0.00
ibis-gen a vers=IBIS-2 org=ROW datacol=(1,2, 3) nc=8 nr=50    d+
ata=(  	1	+
47   5  	2	+
29   7  	3	+
32   9  	1	+
27  11  	2	+
62  13  	3	+
66  15 )
Beginning VICAR task ibis
ibis-list a csize=6 nr=5 cols=(1,2,3)
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 8       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:5
+-----+-----+-----
   C:1   C:2   C:3
+-----+-----+-----
  1.00 47.00  5.00
  2.00 29.00  7.00
  3.00 32.00  9.00
  1.00 27.00 11.00
  2.00 62.00 13.00
transcol a b  +
	INDEXCOL  = 1  +
	DATACOL	  = 2  +
	VALUES    =(1 2 3)  +
        TOCOL  =(4 6 8)
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b nr=5 cols=(1,2,3,4,6,8) csize=6
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 8       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:6   C:8
+-----+-----+-----+-----+-----+-----
  1.00 47.00  5.00 47.00  0.00  0.00
  2.00 29.00  7.00  0.00 29.00  0.00
  3.00 32.00  9.00  0.00  0.00 32.00
  1.00 27.00 11.00 27.00  0.00  0.00
  2.00 62.00 13.00  0.00 62.00  0.00
ibis-gen a vers=IBIS-2 org=ROW datacol=(3,4) nc=5 nr=50    d+
ata=(  	1	+
47  	2	+
29  	3	+
32  	1	+
27  	2	+
62  	3	+
66 )
Beginning VICAR task ibis
ibis-list a csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 5       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5
+-----+-----+-----+-----+-----
  0.00  0.00  1.00 47.00  0.00
  0.00  0.00  2.00 29.00  0.00
  0.00  0.00  3.00 32.00  0.00
  0.00  0.00  1.00 27.00  0.00
  0.00  0.00  2.00 62.00  0.00
transcol inp=a out=b fromcol=(3,4) datacol=5 ncol=5
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=5
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5
+-----+-----+-----+-----+-----
  0.00  0.00  1.00 47.00  1.00
  0.00  0.00  1.00 47.00 47.00
  0.00  0.00  2.00 29.00  2.00
  0.00  0.00  2.00 29.00 29.00
  0.00  0.00  3.00 32.00  3.00
ibis-gen a vers=IBIS-2 org=ROW datacol=(1,2,3) nc=6 nr=50    d+
ata=(  	1	+
2	3  	4	+
5	6  	7	+
8	9  	)
Beginning VICAR task ibis
ibis-list a csize=6 nr=4
Beginning VICAR task ibis
 
Number of Rows:50  Number of Columns: 6       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:4
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  2.00  3.00  0.00  0.00  0.00
  4.00  5.00  6.00  0.00  0.00  0.00
  7.00  8.00  9.00  0.00  0.00  0.00
  0.00  0.00  0.00  0.00  0.00  0.00
transcol a b  +
	FROMCOL = (1 2 3)  +
	DATACOL	= 6  +
        NCOL=6
Beginning VICAR task transcol
TRANSCOL version 05-SEP-94
ibis-list b csize=6 nr=4
Beginning VICAR task ibis
 
Number of Rows:150  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----+-----+-----+-----+-----+-----
   C:1   C:2   C:3   C:4   C:5   C:6
+-----+-----+-----+-----+-----+-----
  1.00  2.00  3.00  0.00  0.00  1.00
  1.00  2.00  3.00  0.00  0.00  2.00
  1.00  2.00  3.00  0.00  0.00  3.00
  4.00  5.00  6.00  0.00  0.00  4.00
let $echo="n"
exit
slogoff
$ Return
$!#############################################################################
