$!****************************************************************************
$!
$! Build proc for MIPL module ibisregr
$! VPACK Version 1.9, Monday, December 07, 2009, 16:30:00
$!
$! Execute by entering:		$ @ibisregr
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
$ write sys$output "*** module ibisregr ***"
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
$ write sys$output "Invalid argument given to ibisregr.com file -- ", primary
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
$   if F$SEARCH("ibisregr.imake") .nes. ""
$   then
$      vimake ibisregr
$      purge ibisregr.bld
$   else
$      if F$SEARCH("ibisregr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisregr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisregr.bld "STD"
$   else
$      @ibisregr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisregr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisregr.com -mixed -
	-s ibisregr.f -
	-i ibisregr.imake -
	-p ibisregr.pdf -
	-t tstibisregr.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisregr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 2 JAN 95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  IBIS ROUTINE IBISREGR
C
C  PURPOSE:  
C	IBISREGR PERFORMS A SERIES OF MULTIPLE REGRESSION ANALYSES
C       ON VARIOUS COMBINATIONS OF COLUMNS IN AN IBIS TABULAR FILE,
C       SEARCHING FOR OPTIMAL FITS.

C  THIS PROGRAM IS BASED ON PROGRAM 'IBISSTAT'

	IMPLICIT NONE

	INTEGER	MAXCOLLEN, MAXTOTLEN, MAXCOLS, MAXSOLNS
	PARAMETER( MAXCOLLEN = 500, MAXTOTLEN=10000, MAXCOLS=40)
	PARAMETER( MAXSOLNS = 20)

	INTEGER	UNIT, OUTUNIT, STATUS, COUNT, DEF, IBIS, OUTIBIS
	INTEGER	CLEN, NCOL, NINCOLS, MAXINCOLS, MININC, MAXINC
	INTEGER INCOLNO(MAXCOLS), INCOLS(MAXCOLS), TEMP(2)
	INTEGER BESTCOLS(MAXCOLS,MAXSOLNS), MBEST(MAXSOLNS)
	INTEGER	I, J, K, M, N, BUFPTR, DEPCOL, ERR, IPIV(MAXCOLS+1)
	INTEGER	NBEST, NBEST1, ICOL, JCOL

	REAL ALLDATA(MAXCOLLEN,MAXCOLS), OUTDATA(MAXCOLLEN)
	REAL DATA(MAXTOTLEN), DATA2(MAXTOTLEN)
	REAL RESID(MAXCOLLEN), BESTRES( MAXCOLLEN, MAXSOLNS)
	REAL DEPDATA(MAXCOLLEN), DEPDATA2(MAXCOLLEN), WORK(MAXTOTLEN)
	REAL BESTRSQ(MAXSOLNS), BESTSIG(MAXSOLNS)
	REAL SOLUTION(MAXCOLS+1), BESTSOL( MAXCOLS+1, MAXSOLNS)
	REAL INTERVALS(MAXCOLS+1), SSR, RSQUARED, SEE, EPSILON

	LOGICAL NOPRINT, XVPTST, EITHER, BOTH, RSONLY, SDONLY

	CHARACTER*8  COLNAMES(MAXCOLS), DEPNAME
	CHARACTER*80 STRING
	CHARACTER*16 FSTR,TMP1,TMP2

C---------------------------------------------------------------------
        CALL IFMESSAGE('IBISREGR version 2-JAN-95')
        CALL XVEACTION('SA',' ')

	NOPRINT = XVPTST( 'NOPRINT')
	BOTH = XVPTST( 'BOTH')
	EITHER = XVPTST( 'EITHER')
	RSONLY = XVPTST( 'RSQUARED')
	SDONLY = XVPTST( 'STDERR')
	
C		OPEN IBIS INTERFACE FILE
        CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(UNIT,IBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)

	N = CLEN
	IF (N .LT. 2)  CALL MABEND('COLUMN LENGTH TOO SHORT')
	IF (N .GT. MAXCOLLEN)  CALL MABEND('COLUMN LENGTH TOO LONG')

	CALL XVPARM( 'COLNAMES', COLNAMES, COUNT, DEF, MAXCOLS)
	DO I = COUNT+1, NCOL
	    IF (I.LE.9) THEN
		WRITE( COLNAMES(I), '(A6,I1)') 'COLUMN', I
	    ELSE
		WRITE( COLNAMES(I), '(A6,I2)') 'COLUMN', I
	    ENDIF
	ENDDO

C  GET THE DATA FOR THE DEPENDANT VARIABLE:
	CALL XVPARM( 'DEPCOL', DEPCOL, COUNT, DEF, 1)
	IF (COUNT.EQ.0) DEPCOL = NCOL
	CALL XVPARM( 'DEPNAME', DEPNAME, COUNT, DEF, 1)
	IF (COUNT.EQ.0) DEPNAME = COLNAMES(DEPCOL)
	CALL IBIS_COLUMN_READ(IBIS,DEPDATA,DEPCOL,1,CLEN,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)

C  COLUMNS TO SEARCH THROUGH FOR INDEPENDENT VARIABLES:
	CALL XVPARM( 'COLS', INCOLNO, MAXINCOLS, DEF, MAXCOLS)
	IF (MAXINCOLS.EQ.0) THEN
	  MAXINCOLS = NCOL-1
	  J = 0
	  DO I = 1,NCOL
	    IF (I.NE.DEPCOL) THEN
	      J = J+1
	      INCOLNO(J) = I
	    ENDIF
	  ENDDO
	ENDIF

C  GET MINIMUM/MAXIMUM NUMBER OF COLUMN COMBINATIONS TO TRY:
	CALL XVPARM( 'COLRANGE', TEMP, COUNT, DEF, 2)
	IF (COUNT.EQ.0) THEN
	  MININC = 2
	  MAXINC = MAXINCOLS
        ELSE
	  MININC = TEMP(1)
	  MAXINC = TEMP(2)
	ENDIF
	IF (MAXINC.GT.MAXINCOLS) CALL MABEND('RANGE TOO HIGH')

	IF ((MAXINCOLS+1)*N .GT. MAXTOTLEN)  
     +			CALL MABEND('TOO MUCH DATA FOR BUFFER')
	IF (N .LT. MAXINC+1)  CALL MABEND('NOT ENOUGH ROWS OF DATA')

C  READ ALL THE COLUMN DATA TO BE SEARCHED INTO WORKING BUFFER:
	DO I = 1,MAXINCOLS
	   CALL IBIS_COLUMN_READ(IBIS,ALLDATA(1,I),INCOLNO(I),
     +                           1,N,STATUS)
           IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	ENDDO

	CALL XVPARM( 'NBEST', NBEST, I, DEF, 1)
	IF (NBEST.GT.MAXSOLNS) CALL MABEND('NBEST TOO BIG')

	CALL XVMESSAGE('MULTIPLE REGRESSION SEARCH',' ')
	WRITE( STRING, '(2A)') 'DEPENDENT VARIABLE: ', DEPNAME
	CALL XVMESSAGE(STRING,' ')

C  START LOOPING OVER ALL POSSIBLE COMBINATIONS OF BETWEEN 'MININC'
C  AND 'MAXINC' COLUMNS.  WE MUST STORE THE 'NBEST' BEST SOLUTIONS.
	DO I = 1,NBEST
	  BESTRSQ(I) = -1.E30
	  BESTSIG(I) = 1.E30
	ENDDO

C  OUTERMOST LOOP IS OVER THE NUMBER OF COLUMNS, 'NINCOLS', WHICH VARIES
C   FROM 'MININC' TO 'MAXINC'.
C  THEN WE LOOP OVER THE COMBINATIONS OF COLUMNS, VARYING THE LAST
C   ONE MOST RAPIDLY AND THE FIRST ONE LEAST RAPIDLY.  (THIS CORRESPONDS
C   TO 'NINCOLS' EMBEDDED DO-LOOPS.)  

	NINCOLS = MININC
100	DO I = 1,NINCOLS			! BEGIN LOOP OVER # COLUMNS
	  INCOLS(I) = I
	ENDDO

C  BEGIN LOOP OVER COLUMNS:
C  PERFORM LEAST-SQS FIT FOR CURRENT 'INCOLS' COMBINATION:
200	IF (.NOT. NOPRINT) THEN
	  CALL XVMESSAGE(' ',' ')
	  WRITE( STRING, '(A6)') 'COLS: '
          J=7
     	  DO I=1,NINCOLS
	     WRITE(STRING(J+3*(I-1):J+3*(I-1)+2),'(I3)')
     *              INCOLNO(INCOLS(I))
          ENDDO
	  CALL XVMESSAGE(STRING,' ')
	ENDIF

	DO I = 1, N
	  DATA(I) = 1.0
	ENDDO
	DO I = 1, NINCOLS
	  BUFPTR = I*N + 1
	  CALL MVE( 7, N, ALLDATA(1,INCOLS(I)), DATA(BUFPTR),1,1)
	ENDDO
	M = NINCOLS + 1

	DO I = 1, N*M
	  DATA2(I) = DATA(I)
	ENDDO
	DO I = 1, N
	  DEPDATA2(I) = DEPDATA(I)
	ENDDO
	
	EPSILON = 1.0E-7
	CALL LLSQ( DATA2, DEPDATA2, N, M, 1,
     +			SOLUTION, IPIV, EPSILON, ERR, WORK)
	IF (ERR.NE.0) CALL MABEND('** COLUMNS ARE DEPENDENT **')
	SSR = WORK(1)

	CALL CALCRES( DATA, DEPDATA, SOLUTION, RESID, N, M, SSR)

	CALL REGSTAT (DATA, DEPDATA, N, M, SSR, SEE, RSQUARED, INTERVALS)

	IF (.NOT. NOPRINT) THEN
	  CALL XVMESSAGE(' ',' ')
	  WRITE (STRING, '(A)') 
     +			' VARIABLE     COEFFICIENT         ERROR'
	  CALL XVMESSAGE(STRING,' ')
          TMP1=FSTR(SOLUTION(1),12,6)
          TMP2=FSTR(INTERVALS(1),12,6)
	  WRITE (STRING, '(A,3X,A12,3X,A12)') '  CONSTANT', TMP1,TMP2
	  CALL XVMESSAGE(STRING,' ')
	  DO I = 2, M
             TMP1=FSTR(SOLUTION(I),12,6)
             TMP2=FSTR(INTERVALS(I),12,6)
	     WRITE (STRING, '(2X,A,3X,A12,3X,A12)') 
     +		    COLNAMES( INCOLNO(INCOLS(I-1))), TMP1,TMP2
	     CALL XVMESSAGE(STRING,' ')
	  ENDDO
	  CALL XVMESSAGE(' ',' ')
	  WRITE (STRING, '(A,F7.4)') 'R SQUARED = ', RSQUARED
	  CALL XVMESSAGE(STRING,' ')
          TMP1=FSTR(SEE,11,4)
	  WRITE (STRING, '(A,A11)') 'STD ERR OF EST = ', TMP1
	  CALL XVMESSAGE(STRING,' ')
	ENDIF

C  SEE IF THIS IS ONE OF THE 'NBEST' SOLUTIONS:
	DO I = 1,NBEST
	  IF ( (BOTH .AND. RSQUARED.GT.BESTRSQ(I) .AND. 
     &        SEE.LT.BESTSIG(I))
     &	  .OR. (EITHER .AND. (RSQUARED.GT.BESTRSQ(I) .OR.
     &	        SEE.LT.BESTSIG(I)))
     &	  .OR. (RSONLY .AND. RSQUARED.GT.BESTRSQ(I)) 
     &	  .OR. (SDONLY .AND. SEE.LT.BESTSIG(I))) THEN
	    GO TO 300
	  ENDIF
	ENDDO
	GO TO 310
300	DO J = NBEST,I+1,-1
	  BESTRSQ(J) = BESTRSQ(J-1)
	  BESTSIG(J) = BESTSIG(J-1)
	  MBEST(J) = MBEST(J-1)
	  CALL MVE( 4, NINCOLS, BESTCOLS(1,J-1), BESTCOLS(1,J),1,1)
	  CALL MVE( 7, NINCOLS+1, BESTSOL(1,J-1), BESTSOL(1,J),1,1)
	  CALL MVE( 7, N, BESTRES(1,J-1), BESTRES(1,J),1,1)
	ENDDO
	BESTRSQ(I) = RSQUARED
	BESTSIG(I) = SEE
	MBEST(I) = NINCOLS+1
	DO J = 1,NINCOLS
	  BESTCOLS(J,I) = INCOLNO( INCOLS(J))
	ENDDO
	DO J = 1,NINCOLS+1
	  BESTSOL(J,I) = SOLUTION(J)
	ENDDO
	CALL MVE( 7, N, RESID, BESTRES(1,I),1,1)

310	DO ICOL = NINCOLS,1,-1
	  IF (INCOLS(ICOL) .LT. MAXINCOLS-NINCOLS+ICOL) GO TO 400
	ENDDO
	GO TO 500		! END OF ALL COMBINATIONS OF CURRENT # COL'S

400	INCOLS(ICOL) = INCOLS(ICOL)+1
	DO JCOL = ICOL+1,NINCOLS
	  INCOLS(JCOL) = INCOLS(JCOL-1)+1
	ENDDO
	GO TO 200				! END LOOP OVER COLUMNS

500	NINCOLS = NINCOLS+1			! END LOOP OVER NUMBER OF COL'S
	IF (NINCOLS.LE.MAXINC) GO TO 100

C  OUTPUT THE 'NBEST' BEST SOLUTIONS TO DISK & TERMINAL:

C  FIRST CHECK THAT THERE WERE AT LEAST 'NBEST' SOLUTIONS:
	DO I = 1,NBEST
	  IF (BESTRSQ(I).GT.0.) NBEST1 = I
	ENDDO
	NBEST = NBEST1

	CALL XVPCNT( 'OUT', COUNT)
	IF (COUNT.GE.1) THEN
C  OPEN CORRECT SIZED IBIS OUTPUT FILE
           CALL XVUNIT(OUTUNIT, 'OUT', 1, STATUS, ' ')

	   CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',3*NBEST,N,
     *                         ' ',' ',STATUS)
           IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)

	   DO J = 1,NBEST
              CALL IBIS_COLUMN_SET(OUTIBIS,'FORMAT','FULL',3*J-2,STATUS)
              CALL IBIS_COLUMN_WRITE(OUTIBIS,BESTCOLS(1,J),3*J-2,
     +                       2,MBEST(J)+1,status)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	      CALL ZIA( OUTDATA, MAXINC+1)
	      DO I = 1, MBEST(J)
	         OUTDATA(I) = BESTSOL(I,J)
	      ENDDO
              CALL IBIS_COLUMN_WRITE(OUTIBIS,OUTDATA,3*J-1,
     +                       1,MBEST(J),status)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
              CALL IBIS_COLUMN_WRITE(OUTIBIS,BESTRES(1,J),3*J,
     +                       1,N,status)
              IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	   ENDDO
	   CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
           IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	ENDIF

        CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('BEST SOLUTIONS FOUND:',' ')

	DO J = 1,NBEST
	  CALL XVMESSAGE(' ',' ')
	  M = MBEST(J)-1
	  WRITE( STRING, '(A6)') 'COLS: '
          K=7
     	  DO I=1,M
	     WRITE(STRING(K+3*(I-1):K+3*(I-1)+2),'(I3)') BESTCOLS(I,J)
          ENDDO
	  CALL XVMESSAGE(STRING,' ')
          TMP1=FSTR(BESTSIG(J),11,4)
	  WRITE( STRING, '(A,F7.4,A,A11)') 'R SQUARED = ', BESTRSQ(J),
     &	   '  STD ERR = ', TMP1
	  CALL XVMESSAGE(STRING,' ')
	ENDDO

	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

	RETURN
	END




	CHARACTER*16 FUNCTION FSTR( VALUE, FIELDWIDTH, NDECIMALS)
	IMPLICIT NONE
	REAL	VALUE
	INTEGER	FIELDWIDTH, NDECIMALS

	IF ( ABS(VALUE) .GE. 10**(FIELDWIDTH-NDECIMALS-2) .OR. 
     +	    ( ABS(VALUE) .LT. 0.1**NDECIMALS .AND. VALUE .NE. 0.0) ) THEN
           IF (FIELDWIDTH .EQ. 11) THEN
	      WRITE( FSTR, '(E11.4)', ERR=100 )  VALUE
	   ELSE
	      WRITE( FSTR, '(E12.6)', ERR=100 )  VALUE
           END IF
	ELSE
           IF (FIELDWIDTH .EQ. 11) THEN
	      WRITE( FSTR, '(F11.4)', ERR=100 )  VALUE
	   ELSE
	      WRITE( FSTR, '(F12.6)', ERR=100 )  VALUE
           END IF
	ENDIF
	RETURN

100	FSTR = '****************'
	RETURN
	END



	SUBROUTINE REGSTAT(C, Y, N, M, SSR, SEE, RSQUARED, INTERVALS)
	IMPLICIT NONE
	INTEGER	N, M
	REAL	C(N,M), Y(N), SSR, SEE, RSQUARED, INTERVALS(M)
	INTEGER	I, J, K, L, ERR, WORK2(41), WORK3(41)
	REAL	A(41*41), SUM, EPS, WORK1(41)
	REAL	MEAN, SUMSQR


	SUM = 0.0
	DO I = 1, N
	    SUM = SUM + Y(I)
	ENDDO
	MEAN = SUM/N
	SUMSQR = 0.0
	DO I = 1, N
	    SUMSQR = SUMSQR + (Y(I) - MEAN)**2
	ENDDO
	RSQUARED = (SUMSQR - SSR) / SUMSQR
	SEE = SQRT(SSR/(N-M))


C		CALCULATE CONFIDENCE INTERVALS FOR THE REGRESSION COEFS
	L = 1
	DO J = 1, M
	    DO K = 1, M
		SUM = 0.0
		DO I = 1, N
		    SUM = SUM + C(I,J)*C(I,K)
		ENDDO
		A(L) = SUM
		L = L + 1
	    ENDDO
	ENDDO

	EPS = 1.0E-7
	CALL MINV (A, M, EPS, ERR, WORK1, WORK2, WORK3)
	IF (ERR .NE. 0) CALL MABEND ('SINGULAR MATRIX')

	DO K = 1, M
	    INTERVALS(K) = SEE*SQRT(A(K+M*(K-1)))
	ENDDO

	RETURN
	END


	SUBROUTINE CALCRES( A, B, X, RES, N, M, SSR)
	IMPLICIT NONE
	INTEGER	I,J,  N,M
	REAL	A(N,M), B(N), X(M), RES(N), SSR, SUM

	DO I = 1, N
	    SUM = B(I)
	    DO J = 1, M
		SUM = SUM - A(I,J)*X(J)
	    ENDDO
	    RES(I) = SUM
	ENDDO

	SSR = 0.0
	DO I = 1, N
	    SSR = SSR + RES(I)**2
	ENDDO

	RETURN
	END



      SUBROUTINE MINV( A,N,E,K,X,J2,I2)
C*     MATRIX INVERSION ROUTINE-FORMULATED BY E. G. CLAYTON
C
C      A--SQUARE ARRAY (SINGLE PRECISION) CONTAINING ORIGINAL MATRIX
C      N--ORDER OF ORIGINAL MATRIX
C      E--TEST CRITERION FOR NEAR ZERO DIVISOR
C      K--LOCATION FOR SINGULARITY OR ILL-CONDITION INDICATOR
C         K=0 =) MATRIX NONSINGULAR.
C         K=1 =) MATRIX SINGULAR (OR ILL-CONDITIONED)
C      X--A WORK VECTOR OF SIZE N
C      J2--AN INTEGER WORK VECTOR OF SIZE N
C      I2--AN INTEGER WORK VECTOR OF SIZE N
C
      DIMENSION A(N,N),X(N),J2(N),I2(N)
C
C     INITIALIZATION
C
      K=1
      I2(1)=0
      J2(1)=0
C
C     BEGIN COMPUTATION OF INVERSE
C
      DO 15 L=1,N
      L1=L-1
      IF(L1.EQ.0)L1=1
      BIGA=-1.0
C
C     LOOK FOR THE ELEMENT OF GREATEST ABSOLUTE VALUE,CHOOSING
C     ONE FROM A ROW AND COLUMN NOT PREVIOUSLY USED.
C
      DO 5 I=1,N
      DO 1 I3=1,L1
      IF(I .EQ. I2(I3))  GO TO 5
    1 CONTINUE
      DO 4 J=1,N
      DO 2 I3=1,L1
      IF(J .EQ. J2(I3))  GO TO 4
    2 CONTINUE
      AT=ABS(A(I,J))
      IF(BIGA .GE. AT)  GO TO 4
      BIGA=AT
      J1=J
      I1=I
    4 CONTINUE
    5 CONTINUE
C
C     TAG THE ROW AND COLUMN FROM WHICH THE ELEMENT IS CHOSEN.
C
      J2(L)=J1
      I2(L)=I1
      DIV=A(I1,J1)
C
C     TEST ELEMENT AGAINST ZERO CRITERION
      IF(ABS(DIV) .LE. E)  GO TO 221
C
C     PERFORM THE COMPUTATIONS
C
      OOD = 1./DIV
      DO 7 J=1,N
      A(I1,J)=A(I1,J)*OOD
    7 CONTINUE
      A(I1,J1)=OOD
      DO 11 I=1,N
      IF(I1 .EQ. I)  GO TO 11
      DO 10 J=1,N
      IF(J1 .EQ. J)  GO TO 10
      A(I,J)=A(I,J)-A(I1,J)*A(I,J1)
   10 CONTINUE
   11 CONTINUE
      DO 14 I=1,N
      IF(I1 .EQ. I)  GO TO 14
      A(I,J1)=-A(I,J1)*A(I1,J1)
   14 CONTINUE
   15 CONTINUE
C
C     COMPUTATION COMPLETE AT THIS POINT
C     UNSCRAMBLE THE INVERSE
C
      DO 18 J=1,N
      DO 16 I=1,N
      I1=I2(I)
      J1=J2(I)
      X(J1)=A(I1,J)
   16 CONTINUE
      DO 17 I=1,N
      A(I,J)=X(I)
17    CONTINUE
   18 CONTINUE
      DO 21 I=1,N
      DO 19 J=1,N
      I1=I2(J)
      J1=J2(J)
      X(I1)=A(I,J1)
   19 CONTINUE
      DO 20 J=1,N
      A(I,J)=X(J)
   20 CONTINUE
   21 CONTINUE
      K=0
  221 RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibisregr.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ibisregr

   To Create the build file give the command:

		$ vimake ibisregr			(VMS)
   or
		% vimake ibisregr			(Unix)


************************************************************************/


#define PROGRAM	ibisregr
#define R2LIB

#define MODULE_LIST ibisregr.f

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
$ create ibisregr.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING)
PARM OUT      TYPE=(STRING)  COUNT=0:1 DEFAULT=--
PARM DEPCOL   TYPE=INTEGER VALID=1:40 
PARM COLS INTEGER COUNT=0:40 DEFAULT=--
PARM COLRANGE TYPE=INTEGER COUNT=0:2 DEFAULT=--
PARM NBEST    INTEGER DEFAULT=3 VALID=(1:20)
PARM DEPNAME  TYPE=(STRING,8) DEFAULT=-- COUNT=0:1
PARM COLNAMES TYPE=(STRING,8) COUNT=0:40 DEFAULT=--
PARM CRITERIA KEYWORD VALID=(BOTH,RSQUARED,STDERR,EITHER) DEFAULT=BOTH
PARM PRINT  TYPE=KEYWORD COUNT=0:1 VALID=(PRINT,NOPRINT) DEFAULT=NOPRINT
END-PROC
.TITLE
VICAR/IBIS Program "ibisregr"
.HELP
PURPOSE

 "ibisregr" performs a series of multiple linear regression analyses on IBIS
 tabular files, searching for a best fit.  This can be used, for example, 
 to find the best combination of spectral channels for determination of a
 physical parameter.


METHOD

 The program loops through all possible combinations of independent variables
 (columns) that are allowed by the user parameters, performing a linear
 least-squares fit ("regression analysis") of the dependent variable for
 each case.  The NBEST best solutions are retained and are printed to the
 terminal and optionally written to the output file.

 The criterion for the best fit is that the R-squared statistic be maximized,
 or the standard error be minimized, or both. The R-squared statistic is the
 fraction of the total variance in the dependent data that is explained by
 the regression.  The standard error of the estimate is the RMS average of
 the residuals (the misfit between the predicted and actual dependent data).  

 The optional output file contains three columns for each of the NBEST best
 fits:  the first shows the M input columns used for this solution (the
 first entry in this column is in row 2, and row 1 always contains a zero,
 denoting the constant term), the second column contains the M+1 regression
 coefficients (the first being the constant), and the third contains the
 residuals.

 NOTE: The multiple regression technique assumes that the residuals are
 uncorrelated and come from a normal distribution.

.page
EXAMPLE

   ibisregr DATA.TAB REGR.TAB COLS=(1,2,5,7,8) COLRANGE=(3,4) DEPCOL=10 +
     NBEST=4 'STDERR 'PRINT 

 This case will search through all possible combinations of three and four of
 the five specified columns (columns 1, 2, 5, 7, and 8) in the input file
 DATA.TAB.  The dependent variable is in column 10.  The best four solutions
 will be retained and written to the file REGR.TAB, using the standard error
 as the criterion.  The solution for each combination will be printed to the
 terminal.

.page
RESTRICTIONS

 The maximum number of input columns is 40.
 The maximum column length is 500.
 The maximum amount of data (number of columns times column length) is 10000.
 The maximum number of solutions that can be retained is 20.


WRITTEN BY:            L.W.Kamp,	July 1987
 (based on program "ibisstat")
REVISIONS:
  JAN 2 1995 AS (CRI) Made portable for UNIX

.LEVEL1
.VARIABLE INP
Input IBIS tabular file
.VARIABLE OUT      
An output IBIS tabular file
.VARIABLE DEPCOL
Dependent variable column
.VARI COLS
Columns to search through
.VARIABLE COLRANGE
(Minimum,maximum) number of
combinations of columns 
.VARIABLE NBEST
Number of solutions to retain
.VARIABLE DEPNAME
Heading for the dependent
variable.
.VARIABLE COLNAMES
Headings for the columns
.VARI CRITERIA
Criteria for best solution
.VARIABLE PRINT
Print output for each 
iteration?
.LEVEL2
.VARIABLE INP
 Input IBIS tabular file containing the data to be fit.
.VARIABLE OUT      
 An output IBIS tabular file containing the NBEST best solutions.
.VARIABLE DEPCOL
 The column in the input file containing the dependent variable.

 The default is the last column.
.VARI COLS
 The columns in the input file through which to search (i.e., those
 containing all possible combinations of independent variables).

 The default is all columns except the last.
.VARIABLE COLRANGE
 The minimum and maximum number of combinations of columns to try.
 The program will start by examining all solutions using COLRANGE(1)
 columns (selected from those specified by the COLS parameter), then
 all solutions using [COLRANGE(1)+1] columns, up through COLRANGE(2)
 columns.

 The default is (2,NCOLS), where NCOLS is the number of columns
 specified in COLS.
.VARIABLE NBEST
 The number of highest-ranked solutions to retain, in order of the
 criteria specified by the CRITERIA parameter.
.VARIABLE DEPNAME
 An optional eight character heading for the dependent variable.
.VARIABLE COLNAMES
 Optional eight character headings for the columns containing the
 independent variables.
.VARI CRITERIA
 Criteria for the best solution(s).  Valid values are:

 RSQUARE:  only the R-squared value will be used as criterion (a higher 
  value is better).

 STDERR:  only the standard error will be used as criterion (a lower value
  is better).

 BOTH (default):  both the R-squared value and the standard error will be
  used as criteria, i.e., a solution is only accepted as better than a
  previous one if both criteria are satisfied.  

 EITHER:  a solution is accepted as better if either the R-squared or the
  standard error criterion is satisfied.

 Note that for BOTH and EITHER, the order in which the solutions are
 encountered will influence the final result if the two do not agree: if
 one solution is best by one of the criteria and a different one by the
 other, then in the case of BOTH the first solution encountered will be
 chosen and in the case of EITHER the second one.

 In general, the R-squared criterion will favor solutions using more
 dependent variables, whereas using more variables will be somewhat
 penalized by the standard deviation.
 
.VARIABLE PRINT
 Print output for each iteration?
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstibisregr.pdf
PROCEDURE
refgbl $autousage
refgbl $echo
BODY

!let $autousage="none"
let _onfail="continue"
let $echo="yes"

! Generate an artifical dataset, with linear
!   relationships, then add some uncorrelated periodic
! "noise" to the input and output.

   ibis-gen data nc=10  nr=100
! Independent pure data
   mf data fun=("c1=index","c2=(8+C1)%16","c5=(53+C1)%7", +
	"C7=C1%19","C8=C1%23")
! Dependent relationship
   mf data fun=("c10=C1 + 2*C2  -7*C7+4")
! Periodic noise
   mf data fun=("c3=index*3.14/100","C1=C1+0.01*SIN(C3)", +
        "C2=C2+0.07*COS(C3*3)","C5=C5+0.02*SIN(C3)",+
        "C10=C10+0.1*COS(C3*2)")

! This case will search through all possible combinations of three and four of
! the five specified columns (columns 1, 2, 5, 7, and 8) in the input file
! DATA.TAB.  The dependent variable is in column 10.  The best four solutions
! will be retained and written to the file regr_tab, using the standard error
! as the criterion.  The solution for each combination will be printed to the
! terminal.

   ibisregr data  regr_tab  COLS=(1,2,5,7,8) COLRANGE=(3,4) DEPCOL=10 +
     NBEST=4 'STDERR 'PRINT 

!-- NOTE!!! The regr_tab file has the columns (1,4,7,10...) in FULL format!
!           This cannot be determined by observation for IBIS-1 files.
!--         When porting be sure the column has the correct integer format.

   ibis-list regr_tab intcol=(1,4,7,10) nr=5

   ibisregr data  regr_tab  COLS=(1,2,5,7,8) COLRANGE=(3,4) DEPCOL=10 +
     NBEST=4 'RSQUARE 'PRINT 
   ibis-list regr_tab intcol=(1,4,7,10) nr=5

   ibisregr data  regr_tab  COLS=(1,2,5,7,8) COLRANGE=(3,4) DEPCOL=10 +
     NBEST=4 'BOTH 'PRINT 
   ibis-list regr_tab intcol=(1,4,7,10) nr=5

   ibisregr data  regr_tab  COLS=(1,2,5,7,8) COLRANGE=(3,4) DEPCOL=10 +
     NBEST=2 'EITHER 'PRINT 
   ibis-list regr_tab intcol=(1,4) nr=5

END-PROC
$ Return
$!#############################################################################
