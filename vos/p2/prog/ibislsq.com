$!****************************************************************************
$!
$! Build proc for MIPL module ibislsq
$! VPACK Version 1.9, Monday, December 07, 2009, 16:29:15
$!
$! Execute by entering:		$ @ibislsq
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
$ write sys$output "*** module ibislsq ***"
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
$ write sys$output "Invalid argument given to ibislsq.com file -- ", primary
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
$   if F$SEARCH("ibislsq.imake") .nes. ""
$   then
$      vimake ibislsq
$      purge ibislsq.bld
$   else
$      if F$SEARCH("ibislsq.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibislsq
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibislsq.bld "STD"
$   else
$      @ibislsq.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibislsq.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibislsq.com -mixed -
	-s ibislsq.f -
	-i ibislsq.imake -
	-p ibislsq.pdf -
	-t tstibislsq.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibislsq.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  IBIS ROUTINE "ibislsq"
C
C  PURPOSE:  PERFORMS A LEAST SQUARES LINEAR FIT TO A NUMBER
C  OF COLUMNS IN AN IBIS INTERFACE FILE PLACING THE SOLUTION
C  COEFFICIENTS AND/OR  RESIDUALS IN OTHER COLUMNS AS DESIRED.
C  THE SOLUTION IS ALSO PRINTED. 
C
C   1-95  AMS   (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C   1-87  SXP   Modified to use LLSQ to solve linear least 
C               squares system of equations.


	IMPLICIT NONE
	INTEGER	MAXLEN, MAXIND, MAXDEP, MAXTOTLEN
	PARAMETER (MAXLEN = 5000, MAXIND=20, MAXDEP=5,  MAXTOTLEN=10000)
	REAL INDBUF(MAXLEN*MAXIND), A(MAXLEN*MAXIND)
        REAL ASAVE(MAXLEN*MAXIND), DEPBUF(MAXLEN*MAXDEP)
        REAL B(MAXLEN*MAXDEP), BSAVE(MAXLEN*MAXDEP)
	REAL RES(MAXLEN*MAXDEP), RESBUF(MAXTOTLEN*MAXDEP)
	REAL SOLUTION(MAXDEP*MAXIND), SOLBUF(MAXTOTLEN*MAXDEP)
	REAL CONTROL(MAXTOTLEN), LASTCONTROL
	REAL IPIV(MAXIND), AUX(2*MAXIND)
	REAL ROWBUF(40)
	REAL EPS
	INTEGER	CLEN, NCOL, COLS(40),  UNIT, STATUS, UPIBIS, UPRECORD
	INTEGER	INDCOL(MAXIND), DEPCOL(MAXDEP), RESCOL(MAXDEP)
	INTEGER	SOLCOL(MAXDEP), NINDCOL, NDEPCOL, NRESCOL, NSOLCOL
	INTEGER	RESDEF, SOLDEF, CONCOL, DUMMY, CONDEF
	INTEGER	LEN, I,J, ROW, STARTROW, IER, SET
	INTEGER	INDPTR, DEPPTR, RESPTR, SOLPTR
	LOGICAL NOPRINT, XVPTST, NOSOLUTION
	CHARACTER*78	STRING
	DATA	EPS/1.E-7/

	EXTERNAL TRANSPOSE

        CALL IFMESSAGE('IBISLSQ version 2-JAN-95')
        CALL XVEACTION('SA',' ')

C		GET PARAMETERS

	CALL XVP ('INDCOL', INDCOL, NINDCOL)
	CALL XVP ('DEPCOL', DEPCOL, NDEPCOL)
	CALL XVPARM ('SOLCOL', SOLCOL, NSOLCOL, SOLDEF, MAXDEP)
	CALL XVPARM ('RESCOL', RESCOL, NRESCOL, RESDEF, MAXDEP)
	CALL XVPARM ('CONCOL', CONCOL, DUMMY, CONDEF, 1)
	NOPRINT = XVPTST ('NOPRINT')


	IF (RESDEF .EQ. 0) THEN
	    IF (NRESCOL .NE. NDEPCOL) THEN
		CALL XVMESSAGE
     +	  ('NUMBER OF RESIDUAL COLS MUST EQUAL NUMBER OF DEP. COLS',' ')
		CALL ABEND
	    ENDIF
	ENDIF
	IF (SOLDEF .EQ. 0) THEN
	    IF (NSOLCOL .NE. NDEPCOL) THEN
		CALL XVMESSAGE 
     +	  ('NUMBER OF SOLUTION COLS MUST EQUAL NUMBER OF DEP. COLS',' ')
		CALL ABEND
	    ENDIF
	ENDIF

C		INITIALIZE VARIABLES
	RESPTR = 1
	SOLPTR = 1
	DO I = 1,40
	    COLS(I) = I
	ENDDO
	SET = 1
	ROW = 1

C		OPEN THE INTERFACE FILE
        CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')
	CALL IBIS_FILE_OPEN(UNIT,UPIBIS,'UPDATE',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
        CALL IBIS_FILE_GET(UPIBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(UPIBIS,'NR',CLEN,1,1)

	IF (CLEN .GT. MAXTOTLEN) THEN
	    CALL MABEND ('COLUNM LENGTH TOO LONG')
	ENDIF

C		READ IN CONTROL COLUMN
	IF (CONDEF .EQ. 0) THEN
	   CALL IBIS_COLUMN_READ(UPIBIS,CONTROL,CONCOL,1,CLEN,STATUS)
           IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UPIBIS,STATUS,1)
	ENDIF


C		LOOP THROUGH ALL OF THE SETS OF DATA
100	CONTINUE

C		FILL MATRICES FOR LLSQ

	STARTROW = ROW
	LASTCONTROL = CONTROL(STARTROW)
	INDPTR = 0
	DEPPTR = 0
        CALL IBIS_RECORD_OPEN(UPIBIS,UPRECORD,'FORMAT:REAL',
     &                        COLS,NCOL,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
	DO WHILE (ROW .LE. CLEN .AND. CONTROL(ROW) .EQ. LASTCONTROL )
	    CALL IBIS_RECORD_READ(UPRECORD, ROWBUF, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UPIBIS,STATUS,1)
	    DO I = 1, NINDCOL
		INDBUF(INDPTR+I) = ROWBUF(INDCOL(I))
	    ENDDO
	    INDPTR = INDPTR + NINDCOL
	    DO I = 1, NDEPCOL
		DEPBUF(DEPPTR+I) = ROWBUF(DEPCOL(I))
	    ENDDO
	    DEPPTR = DEPPTR + NDEPCOL
	    LASTCONTROL = CONTROL(ROW)
	    ROW = ROW + 1
	ENDDO
	LEN = ROW - STARTROW
	IF (LEN .GT. MAXLEN) THEN
	    CALL MABEND('SET LENGTH TOO LONG')
	ENDIF


	NOSOLUTION = (LEN .LT. NINDCOL)
	IF (NOSOLUTION) GO TO 999

	CALL TRANSPOSE (INDBUF, A, ASAVE, LEN, NINDCOL)
	CALL TRANSPOSE (DEPBUF, B, BSAVE, LEN, NDEPCOL)


C  PERFORM LEAST SQUARES FIT.

	CALL LLSQ (A, B, LEN, NINDCOL, NDEPCOL,
     +			SOLUTION, IPIV, EPS, IER, AUX)
	IF (IER .NE. 0) THEN
	    CALL XVMESSAGE ('MATRIX RANK TOO SMALL',' ')
	    NOSOLUTION = .TRUE.
	ENDIF

999	CONTINUE
	IF (NOSOLUTION) THEN
	    DO I = 1, NDEPCOL*NINDCOL
		SOLUTION(I) = -999.0
	    ENDDO
	ENDIF


C		CALCULATE THE RESIDUALS

	IF (RESDEF .EQ. 0) THEN
	    CALL CALCRES (ASAVE, BSAVE, SOLUTION, RES,
     +				LEN, NINDCOL, NDEPCOL,   NOSOLUTION)
	    CALL TRANSPOSE (RES, RESBUF(RESPTR), BSAVE, NDEPCOL, LEN)
	    RESPTR = RESPTR + LEN*NDEPCOL
	ENDIF

C		PRINT OUT THE SOLUTION

	CALL TRANSPOSE (SOLUTION, SOLBUF(SOLPTR), BSAVE, NDEPCOL, 
     +                  NINDCOL)
	IF (.NOT. NOPRINT) THEN
	    WRITE (STRING, '(A,I3)' )  ' SOLUTION FOR SET: ', SET
	    CALL XVMESSAGE (STRING,' ')
	    DO J = 0, NINDCOL-1
               DO I = 1,NDEPCOL
		  WRITE (STRING((1+(I-1)*14):(1+(I-1)*14+13)),
     +                   '(E12.5,2X)') SOLBUF(SOLPTR+J*NDEPCOL+(I-1))
               ENDDO
	       CALL XVMESSAGE (STRING,' ')
	    ENDDO
	    CALL XVMESSAGE (' ',' ')
	ENDIF
	SOLPTR = SOLPTR + LEN*NDEPCOL


	SET = SET + 1
	IF (ROW .LE. CLEN) GO TO 100
C		GO BACK FOR MORE SETS


	CALL IBIS_FILE_CLOSE (UPIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)



C   REOPEN FILE IF WE NEED TO PUT RESIDUALS OR SOLUTIONS IN IT

	IF (RESDEF .EQ. 0 .OR. SOLDEF .EQ. 0) THEN
            CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')
	    CALL IBIS_FILE_OPEN(UNIT,UPIBIS,'UPDATE',0,0,
     &                          ' ',' ',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
            CALL IBIS_FILE_GET(UPIBIS,'NC',NCOL,1,1)
            CALL IBIS_FILE_GET(UPIBIS,'NR',CLEN,1,1)
	    RESPTR = 0
	    SOLPTR = 0
            CALL IBIS_RECORD_OPEN(UPIBIS,UPRECORD,'FORMAT:REAL',
     &                            COLS,NCOL,'REAL',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
	    DO ROW = 1, CLEN
	        CALL IBIS_RECORD_READ(UPRECORD, ROWBUF, ROW, STATUS)
                IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UPIBIS,STATUS,1)
		IF (RESDEF .EQ. 0) THEN
		    DO I = 1, NDEPCOL
			ROWBUF(RESCOL(I)) = RESBUF(RESPTR+I)
		    ENDDO
		    RESPTR = RESPTR + NDEPCOL
		ENDIF
		IF (SOLDEF .EQ. 0) THEN
		    DO I = 1, NDEPCOL
			ROWBUF(SOLCOL(I)) = SOLBUF(SOLPTR+I)
		    ENDDO
		    SOLPTR = SOLPTR + NDEPCOL
		ENDIF
	        CALL IBIS_RECORD_WRITE(UPRECORD, ROWBUF, ROW, STATUS)
                IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UPIBIS,STATUS,1)
	    ENDDO
	    CALL IBIS_FILE_CLOSE (UPIBIS,' ',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
	ENDIF


	RETURN
	END



	SUBROUTINE TRANSPOSE (IN, OUT, SAVE,  M, N)
	IMPLICIT NONE
	INTEGER	I,J,  M,N
	REAL	IN(N,M), OUT(M,N), SAVE(M,N)

	DO I = 1, M
	    DO J = 1, N
		OUT(I,J) = IN(J,I)
		SAVE(I,J) = OUT(I,J)
	    ENDDO
	ENDDO

	RETURN
	END


	SUBROUTINE CALCRES (A, B, X, RES, M, N, L, NOSOLUTION)
	IMPLICIT NONE
	INTEGER	I,J,K,  M,N,L
	REAL	A(M,N), B(M,L), X(N,L), RES(M,L), SUM
	LOGICAL	NOSOLUTION

	IF (NOSOLUTION) THEN
	    DO I = 1, M
		DO J = 1, L
		    RES(I,J) = 0.0
		ENDDO
	    ENDDO
	ELSE
	    DO I = 1, M
		DO J = 1, L
		    SUM = B(I,J)
		    DO K = 1, N
			SUM = SUM - A(I,K)*X(K,J)
		    ENDDO
		    RES(I,J) = SUM
		ENDDO
	    ENDDO
	ENDIF

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibislsq.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ibislsq

   To Create the build file give the command:

		$ vimake ibislsq			(VMS)
   or
		% vimake ibislsq			(Unix)


************************************************************************/


#define PROGRAM	ibislsq
#define R2LIB

#define MODULE_LIST ibislsq.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ibislsq.pdf
PROCESS       HELP=*
PARM INP TYPE=(STRING)
PARM INDCOL TYPE=INTEGER COUNT=1:20 
PARM DEPCOL TYPE=INTEGER COUNT=1:5 
PARM SOLCOL TYPE=INTEGER COUNT=1:5 DEFAULT=0
PARM RESCOL TYPE=INTEGER COUNT=1:5 DEFAULT=0
PARM CONCOL TYPE=INTEGER DEFAULT=1
PARM NOPRINT TYPE=KEYWORD COUNT=0:1 VALID=(--,NOPRINT) DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program "ibislsq"
.HELP
PURPOSE

   "ibislsq" performs least squares fits on data in an IBIS interface
(tabular) file.  The solutions and/or residuals can be placed in 
specified columns of the file.  The solutions can also be output
to the terminal.  Multiple fits can be done on different parts of
one file.


EXECUTION

     ibislsq INP=DATA.INT  INDCOL=(1,2,3) DEPCOL=(4,5) CONCOL=7 +
	     SOLCOL=(10,11) RESCOL=(8,9)  'NOPRINT

    This example shows the use of all of the parameters.  The input
file, DATA.INT, is an IBIS interface file.  The data for the independent
variables are in columns (1,2,3), and the data for the dependent 
variables are in columns (4,5).  The control column is used for
multiple fits to be done in one run.  The least square fits are done 
on sets of rows; a new set is started whenever the value in the control 
column changes.  If no control column is specified then one fit
is done on the whole file.  The SOLCOL and RESCOL parameters specify
in which columns, of the input file, the results will be put.  If either
or both are not specified then they will not be output.  There must be
as many solution columns and residual columns as there are dependent 
columns.  The data in the residual columns correspond one-to-one with
the data in the dependent columns.  The solution is put out in the
first N rows of the SOLCOL columns, where N is the number of independent
columns; the rest of the rows in the set are filled with zeros.   Normally
the solution for each set is printed to the terminal, but this can 
be turned off with the 'NOPRINT keyword.

    The length of each set should, of course, be longer than the
number of independent variables (columns).  If it is not then the
least squares fit will not be called and values of -999.0 will be
put out for the solution.  If some columns of the independent data
are dependent then the error MATRIX RANK TOO SMALL be be printed,
and -999.0's will be put out for the solution.  If there is no
solution then zeros will be put out for the residuals.



EXAMPLES

   Suppose that columns 1 and 2 contain points (x,y) in a plane 
   and  column  7 contains a function  f(x,y).   The  following 
   sequence  will perform a quadratic least squares fit  h(x,y) 
   and place the residuals in column 8.

   mf INP=A FUNCTION=("C3=C1*C1","C4=C2*C2","C5=C1*C2","C6=1")
   ibislsq INP=A INDCOL=(1,2,3,4,5,6) DEPCOL=7 RESCOL=8


RESTRICTIONS

The maximum number of independent columns (variables) is 20.
The maximum number of dependent columns (variables) is 5.
The maximum length of any set is 5000.
The maximum column length of the file is 10000.
Interface files are assumed to have 40 or less columns.



WRITTEN BY:            K. F. Evans	April 1986

COGNIZANT PROGRAMMER:  K. F. Evans

REVISION:  A. Scop (CRI) 2 Jan. 1995  Made portable for UNIX


.LEVEL1
.VARIABLE INP
 Input IBIS interface file
.VARIABLE INDCOL
 Independent variable columns
.VARIABLE DEPCOL
 Dependent variable columns
.VARIABLE SOLCOL
 Solutions columns
.VARIABLE RESCOL
 Residuals columns
.VARIABLE CONCOL
 Control column
.VARIABLE NOPRINT
Keyword to suppress printout

.LEVEL2
.VARIABLE INP
   INP=file           Input IBIS interface file
.VARIABLE INDCOL
   INDCOL=(N1,...,Nk) The   integers   N1,...,Nk  specify   the 
                      columns  which contain the data  for  the 
                      independent variables.  Each row contains 
                      one data point.

.VARIABLE DEPCOL
   DEPCOL=(M1,...,Ml) The   integers   M1,...,Ml  specify   the 
                      columns  which contain the data  for  the 
                      dependent variables.  Each row contains 
                      one data point.

.VARIABLE SOLCOL
   SOLCOL=(S1,...,Sl) The integers  S1,...,Sl specify columns 
                      which receive the solution coefficients 
                      corresponding to the dependent variables.
		      The first k rows contain the solutions for
		      the corresponding k independent columns.
		      If this parameter is omitted, then the  
		      solution is not stored.
.VARIABLE RESCOL
   RESCOL=(R1,...,Rl) The  integers  R1,...,Rl specify columns 
                      which  receive the residuals corresponding 
		      to the dependent variables for each data
		      point.  If this parameter is omitted,  
		      then no residuals are stored.
.VARIABLE CONCOL
   CONCOL=C           The integer C specifies a control column 
                      which produces a separate fit for each 
                      group of identical numbers in the control 
                      column.  If this keyword is omitted, then 
                      the whole file is DGELGd in one least 
                      squares fit.
.VARIABLE NOPRINT
    'NOPRINT	      Keyword to suppress printout of solution.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstibislsq.pdf
procedure
refgbl $echo
refgbl $autousage
parm mean real def=0.0
parm sdev real def=1.0
parm seed real def=9.0
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!	The test is a least squares fit of a linear 2-D vector field.
!		x' =  2.5 + 0.65x - 0.30y + Nx
!		y' = -1.3 + 1.20x + 0.15y + Ny 
!	   where Nx and Ny are gaussian noise with a sigma of 0.01. 
!	   The indepedent x and y are set up to be an 8 by 8 grid 
!		of unit spacing. 
!
!  The columns of the IBIS interface file are as follows: 
!
!		Input columns          |         Output columns 
!   1    2   3    4   5      6     7       8       9        10       11   
!  1's   x   y    x'  y'   Index Cntrl    Res x'  Res y'   Sol x'   Sol y'
!
!
  ibis-gen TEST. NC=15 NR=128
  mf TEST. FUNCTION=("C1=(sin(3.7*index**2)+1)/2",  +
           "C2=(sin(5.3*(index+&seed)**2)+1)/2",  +
           "C14=(sqrt(-2*alog(C1))*sin(2*(3.1415926)*C2))", +
           "C14=&mean + &sdev*C14")
  let seed = 8.0
  mf TEST. FUNCTION=("C1=(sin(3.7*index**2)+1)/2",  +
           "C2=(sin(5.3*(index+&seed)**2)+1)/2",  +
           "C15=(sqrt(-2*alog(C1))*sin(2*(3.1415926)*c2))",  +
           "C15=&mean + &sdev*c15")
  mf TEST. FUNCTION=("C6=INDEX-1", "C7=INT(C6/64)", "C1=1",  +
           "C2=MOD(C6,8)", "C3=INT(C6/8)"  +
           "C4 =  2.5*C1 + 0.65*C2 - 0.30*C3 + C14",  +
           "C5 = -1.3*C1 + 1.20*C2 + 0.15*C3 + C15" )
  ibislsq TEST. INDCOL=(1,2,3)  DEPCOL=(4,5)  CONCOL=7 +
	  RESCOL=(8,9)  SOLCOL=(10,11)
END-PROC
$ Return
$!#############################################################################
