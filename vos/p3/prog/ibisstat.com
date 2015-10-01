$!****************************************************************************
$!
$! Build proc for MIPL module ibisstat
$! VPACK Version 1.9, Thursday, August 25, 2005, 16:51:41
$!
$! Execute by entering:		$ @ibisstat
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
$ write sys$output "*** module ibisstat ***"
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
$ write sys$output "Invalid argument given to ibisstat.com file -- ", primary
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
$   if F$SEARCH("ibisstat.imake") .nes. ""
$   then
$      vimake ibisstat
$      purge ibisstat.bld
$   else
$      if F$SEARCH("ibisstat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ibisstat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ibisstat.bld "STD"
$   else
$      @ibisstat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ibisstat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ibisstat.com -mixed -
	-s ibisstat.f -
	-i ibisstat.imake -
	-p ibisstat.pdf -
	-t tstibisst.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ibisstat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  IBIS ROUTINE IBISSTAT
C
C  PURPOSE:  
C	IBISSTAT PERFORMS A VARIETY OF STATISTICAL ANALYSES 
C		ON IBIS TABULAR FILES
C
C	ORIGINAL PROGRAMMER:    FRANK EVANS    OCTOBER 1986
C
C	VERSION 1:
C
C   1-87  SXP   Modified to use LLSQ to solve linear least 
C               squares system of equations.
C   1-87  SXP   changed MAXTOTLEN to 200,000 to reduce memory usage
C               to prevent quota problems.
C   2-95  MAC   CRI  MSTP S/W CONVERSION (VICAR PORTING)
C
	IMPLICIT NONE
	INTEGER	MAXCOLLEN, MAXTOTLEN, MAXCOLS
	PARAMETER (MAXCOLLEN = 100000, MAXTOTLEN=200000, MAXCOLS=40)

	INTEGER	UNIT, OUTUNIT, STATUS, COUNT, DEF
	INTEGER	CLEN, NCOL, NINCOLS
	INTEGER	INCOLS(MAXCOLS), IN2COLS(MAXCOLS), COLS(MAXCOLS)
	INTEGER	I, II, J, JJ, K, M, N
	INTEGER	INDEX, BUFPTR
	INTEGER	NUMBINS, BIN, MAXCOUNTS, HISTCOUNT(1000)
	INTEGER	SCATSIZE(2), XWIDTH, YWIDTH
	INTEGER	XBIN, YBIN, SCATTER(75,50)
	INTEGER	SWATH, ROW, NUMCOL, STARTCOL
	INTEGER	DEPCOL, ERR, IPIV(MAXCOLS+1)
	INTEGER	ERRCOL, ERRDEF
	INTEGER	REPLICS(MAXCOLS), TOTREP, NDF(3)
	INTEGER	NA, NB, NUMPOINT(2)
	INTEGER	N1, N2, INIBIS, OUTIBIS, BEGIN

	REAL	DATA(MAXTOTLEN), DATA2(MAXTOTLEN)
	REAL	OUTDATA(MAXCOLLEN), RESID(MAXCOLLEN)
	REAL	DEPDATA(MAXCOLLEN), DEPDATA2(MAXCOLLEN)
	REAL	WORK(10000), DUMMY(10000)

	REAL	MEDIAN, MEAN, STANDEV, MINDATA, MAXDATA
	REAL	SCALE, OFFSET, BINVALUE(1000), HIST(1000), HISTFACTOR
	REAL	XSCALE, YSCALE, XOFFSET, YOFFSET
	REAL	MINX, MAXX, MINY, MAXY
	REAL	CORRARRAY(MAXCOLS*MAXCOLS), CORRMATRIX(MAXCOLS,MAXCOLS)
	REAL	CORRPROB(MAXCOLS,MAXCOLS), COVARRAY(MAXCOLS*MAXCOLS)
	REAL	R, DENOM, T
	REAL	SOLUTION(MAXCOLS+1), INTERVALS(MAXCOLS+1)
	REAL	SSR, RSQUARED, SEE
	REAL	DURWATSTAT, FRATIO, FPROB, PROB, CHISQR, CHIPROB
	REAL	EPSILON
	REAL	YBAR, EFFECTS(MAXCOLS), SSARRAY(3)
	REAL	T2, CRIT
	REAL	SUM
	REAL*8  EIGENCORR(MAXCOLS,MAXCOLS), EIGENVAL(MAXCOLS)
	REAL	PHI, CHI2, STUDIS, FISH

	LOGICAL NOPRINT, OUTPUT, XVPTST
	CHARACTER*8	OPTION, COLNAMES(MAXCOLS), DEPNAME, DIST
	CHARACTER*80	STRING, STARS, SCATLINE, SLINE


C		GET PARAMETERS
        CALL IFMESSAGE('IBISSTAT version 02-JAN-95')
	CALL XVP ('OUT', STRING, COUNT)
	OUTPUT = (COUNT .EQ. 1)  

	NOPRINT = XVPTST ('NOPRINT')
	CALL XVP ('OPTION', OPTION, COUNT)
	CALL XVP ('COLS', INCOLS, NINCOLS)

	DO I = 1, MAXCOLS
	    IF (INCOLS(I) .LE. 9) THEN
		WRITE (COLNAMES(I), '(A6,I1)') 'COLUMN', INCOLS(I)
	    ELSE
		WRITE (COLNAMES(I), '(A6,I2)') 'COLUMN', INCOLS(I)
	    ENDIF
	ENDDO
	CALL XVPARM ('COLNAMES', COLNAMES, COUNT, DEF, 40)
	
	IF (.NOT. NOPRINT)   CALL XVMESSAGE (' ',' ')


C		OPEN THE INTERFACE FILE
        CALL XVUNIT(UNIT,'INP',1,STATUS,' ')
        IF (STATUS.NE.1) THEN
          CALL XVMESSAGE
     & (' INPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
          CALL ABEND
        END IF
        CALL IBIS_FILE_OPEN(UNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
        CALL IBIS_FILE_GET(INIBIS,'NR',CLEN,1,1)
        CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)

	N = CLEN
	IF (N .LT. 2)  CALL MABEND (' COLUMN LENGTH TOO SHORT')
	IF (N .GT. MAXCOLLEN)  CALL MABEND (' COLUMN LENGTH TOO LONG')


	IF (OPTION(1:3) .EQ. 'SUM') GOTO 1000
	IF (OPTION(1:3) .EQ. 'HIS') GOTO 2000
	IF (OPTION(1:3) .EQ. 'SCA') GOTO 2500
	IF (OPTION(1:3) .EQ. 'COR') GOTO 3000
	IF (OPTION(1:3) .EQ. 'BEH') GOTO 4000
	IF (OPTION(1:3) .EQ. 'REG') GOTO 5000
	IF (OPTION(1:3) .EQ. 'ANO') GOTO 6000
	IF (OPTION(1:3) .EQ. 'FAC') GOTO 7000
	IF (OPTION(1:3) .EQ. 'DEN') GOTO 8000




C    *************************************************************************
C			STATISTICAL SUMMARY SECTION
1000	CONTINUE

	IF (OUTPUT) THEN
          CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
          IF (STATUS.NE.1) THEN
            CALL XVMESSAGE
     &  (' OUTPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
            CALL ABEND
          END IF
          CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',NCOL,CLEN,' ',
     &           'COLUMN',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	ENDIF

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('              STATISTICAL SUMMARY ',' ')
	    WRITE (STRING, '(A,I6)') 'NUMBER OF CASES:', N
	    CALL XVMESSAGE (' ',' ')
	    CALL XVMESSAGE (STRING,' ')
            STRING(1:28) = 'COLUMN   NAME         MEDIAN'
            STRING(35:74) = 'MEAN      STD DEV        MIN         MAX'
	    CALL XVMESSAGE (STRING,' ')
	ENDIF

	DO INDEX = 1, NINCOLS
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA,INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	    CALL SORTR(DATA,N)

	    IF (MOD(N,2) .EQ. 0) THEN
		MEDIAN = ( DATA(N/2) + DATA(N/2+1) ) /2.0
	    ELSE
		MEDIAN = DATA((N+1)/2)
	    ENDIF
	    MINDATA = DATA(1)
	    MAXDATA = DATA(N)

	    MEAN = 0.0
	    DO I = 1, N
		MEAN = MEAN + DATA(I)
	    ENDDO
	    MEAN = MEAN/FLOAT(N)

	    STANDEV = 0.0
	    DO I = 1, N
		STANDEV = STANDEV + (DATA(I) - MEAN)**2
	    ENDDO
	    STANDEV = SQRT(STANDEV/(N-1.0))

	    IF (OUTPUT) THEN
		OUTDATA(1) = MEDIAN
		OUTDATA(2) = MEAN
		OUTDATA(3) = STANDEV
		OUTDATA(4) = MINDATA
		OUTDATA(5) = MAXDATA
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',INDEX,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,OUTDATA,INDEX,
     &                     1,CLEN,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	    ENDIF

	    IF (.NOT. NOPRINT) THEN
		WRITE (STRING, '(2X,I3,3X,A,5(1X,F11.4))') 
     +			INCOLS(INDEX), COLNAMES(INDEX),
     +			MEDIAN, MEAN, STANDEV, MINDATA, MAXDATA
		CALL XVMESSAGE (STRING,' ')
	    ENDIF

	ENDDO

	IF (OUTPUT) THEN
          CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
        ENDIF

	GOTO 9999




C    *************************************************************************
C			HISTOGRAM SECTION
2000	CONTINUE

	CALL XVP ('BINS', NUMBINS, COUNT)

	IF (OUTPUT) THEN
          CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
          IF (STATUS.NE.1) THEN
            CALL XVMESSAGE
     & (' OUTPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
            CALL ABEND
          END IF
          CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',NCOL,CLEN,' ',
     &           'COLUMN',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
        ENDIF

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('      HISTOGRAM  ',' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,I6)') 'NUMBER OF CASES:', N
	    CALL XVMESSAGE (STRING,' ')
	    CALL XVMESSAGE (' ',' ')
	ENDIF

	DO INDEX = 1, NINCOLS
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA,INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	    IF (.NOT. NOPRINT) THEN
		CALL XVMESSAGE (' ',' ')
		WRITE (STRING, '(A,A)') 
     +			'       HISTOGRAM FOR : ', COLNAMES(INDEX)
		CALL XVMESSAGE (STRING,' ')
		CALL XVMESSAGE (' ',' ')
	    ENDIF

	    MINDATA = +1.0E38
	    MAXDATA = -1.0E38
	    DO I = 1, N
		MINDATA = MIN (MINDATA, DATA(I))
		MAXDATA = MAX (MAXDATA, DATA(I))
	    ENDDO
	    IF (MAXDATA .EQ. MINDATA) THEN
		WRITE (STRING, '(A,F10.3)') 
     +			'       COLUMN MAX EQUALS MIN: ', MAXDATA
		CALL XVMESSAGE (STRING,' ')
		GOTO 2099
	    ENDIF
	    SCALE = (NUMBINS-0.01)/(MAXDATA-MINDATA)
	    OFFSET = -SCALE*MINDATA + 0.5

	    DO I = 1, N
		BIN = NINT(SCALE*DATA(I) + OFFSET)
		HISTCOUNT(BIN) = HISTCOUNT(BIN) + 1
	    ENDDO

	    DO BIN = 1, NUMBINS
		BINVALUE(BIN) =  (FLOAT(BIN) - OFFSET)/SCALE
		HIST(BIN) = FLOAT(HISTCOUNT(BIN))
		MAXCOUNTS = MAX (MAXCOUNTS,HISTCOUNT(BIN))
	    ENDDO


	    IF (OUTPUT) THEN
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',2*INDEX-1,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,BINVALUE,2*INDEX-1,
     &                     1,NUMBINS,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',2*INDEX,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,HIST,2*INDEX,
     &                     1,NUMBINS,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	    ENDIF

	    IF (.NOT. NOPRINT) THEN
		STARS(1:1) = '|'
		HISTFACTOR = MIN (1.0, 60.0/MAXCOUNTS)
		DO I = 1, NUMBINS
	            STARS(2:79) = ' '	
                    COUNT = NINT(HISTFACTOR*HISTCOUNT(I)) + 1
		    DO II = 2, COUNT
                      STARS(II:II) ='*'
                    ENDDO 
                    WRITE (STRING, '(F10.3,1X,I5,2X,A60)') 
     +				BINVALUE(I), HISTCOUNT(I), STARS
		    CALL XVMESSAGE (STRING,' ')
		ENDDO
		CALL XVMESSAGE (' ',' ')
	    ENDIF
2099	    CONTINUE
	ENDDO

	IF (OUTPUT) THEN
          CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
        ENDIF


	GOTO 9999





C    *************************************************************************
C			SCATTER PLOT SECTION
2500	CONTINUE


	IF (NOPRINT)  GOTO 9999

	CALL XVMESSAGE ('                  SCATTER PLOT  ',' ')
	CALL XVMESSAGE (' ',' ')
	WRITE (STRING, '(A,I6)') 'NUMBER OF CASES:', N
	CALL XVMESSAGE (STRING,' ')
	CALL XVMESSAGE (' ',' ')

	CALL XVP ('SCATSIZE', SCATSIZE, COUNT)
	XWIDTH = MIN (SCATSIZE(1), 75)
	YWIDTH = MIN (SCATSIZE(2), 50)

	DO INDEX = 1, NINCOLS-1, 2
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA,INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX+1),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA2,INCOLS(INDEX+1),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	    WRITE (STRING, '(8X,A8,A,A8)') 
     +			COLNAMES(INDEX), ' VS ', COLNAMES(INDEX+1)
	    CALL XVMESSAGE (STRING,' ')
	    CALL XVMESSAGE (' ',' ')

	    MINX = +1.0E38
	    MAXX = -1.0E38
	    DO I = 1, N
		MINX = MIN (MINX, DATA(I))
		MAXX = MAX (MAXX, DATA(I))
	    ENDDO
	    IF (MAXX .EQ. MINX) THEN
		CALL XVMESSAGE ('MAX X EQUALS MIN X',' ')
		GOTO 2599
	    ENDIF
	    XSCALE = (XWIDTH-0.01)/(MAXX-MINX)
	    XOFFSET = -XSCALE*MINX + 0.5

	    MINY = +1.0E38
	    MAXY = -1.0E38
	    DO I = 1, N
		MINY = MIN (MINY, DATA2(I))
		MAXY = MAX (MAXY, DATA2(I))
	    ENDDO
	    IF (MAXY .EQ. MINY) THEN
		CALL XVMESSAGE ('MAX Y EQUALS MIN Y',' ')
		GOTO 2599
	    ENDIF
	    YSCALE = (YWIDTH-0.01)/(MAXY-MINY)
	    YOFFSET = -YSCALE*MINY + 0.5


	    DO I = 1, N
		XBIN = NINT(XSCALE*DATA(I) + XOFFSET)
		YBIN = NINT(YSCALE*DATA2(I) + YOFFSET)
		SCATTER(XBIN,YBIN) = SCATTER(XBIN,YBIN) + 1
	    ENDDO


	    WRITE (STRING, '(F9.2)')  MAXY
	    CALL XVMESSAGE (STRING,' ')
	    DO YBIN = YWIDTH, 1, -1
		DO XBIN = 1, XWIDTH
		    COUNT = SCATTER(XBIN,YBIN)
		    COUNT = MIN(COUNT,9) + 48
		    IF (COUNT .EQ. 49)  COUNT = 42
		    IF (COUNT .EQ. 48)  COUNT = 32
		    SCATLINE(XBIN:XBIN) = CHAR(COUNT)
		ENDDO
		WRITE (STRING, '(A1,A60)') '+', SCATLINE
		CALL XVMESSAGE (STRING,' ')
	    ENDDO
	    STRING(1:1) = ' '
	    DO I = 1, XWIDTH+1
		STRING(I:I) = '+'
	    ENDDO
	    CALL XVMESSAGE(STRING,' ')
	    WRITE (STRING, '(F9.2)')  MINY
	    CALL XVMESSAGE(STRING,' ')
	    WRITE (STRING, '(8X,F9.2,30X,F9.2)')  
     +			MINX, MAXX
	    CALL XVMESSAGE(STRING,' ')

2599	    CONTINUE
	    CALL XVMESSAGE (' ',' ')
	ENDDO

	GOTO 9999




C    *************************************************************************
C			CORRELATION SECTION
3000	CONTINUE

	IF (NINCOLS*N .GT. MAXTOTLEN)  CALL MABEND (' TOO MUCH DATA')

	DO J = 1, NINCOLS
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(J),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA((J-1)*N+1),INCOLS(J),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	ENDDO
	M = NINCOLS

	CALL CORRE (N, M, 1, DATA, 
     +			DUMMY(1), DUMMY(MAXCOLS+1), COVARRAY,
     +		  CORRARRAY, DUMMY(MAXCOLS**2+1), WORK, WORK(MAXCOLS+1) )

	K = 1
	DO I = 1, M
	    DO J = 1, I
		CORRMATRIX(I,J) = CORRARRAY(K)
		CORRMATRIX(J,I) = CORRMATRIX(I,J) 
		K = K + 1
	    ENDDO
	ENDDO

	DO I = 1, M*M
	    COVARRAY(I) = COVARRAY(I)/N
	ENDDO

	DO I = 1, M
	    DO J = 1, I
		R = CORRMATRIX(I,J)
		DENOM = (1.0 - R**2) / (N-2)
		IF (DENOM .GT. 0.0) THEN
		    T = SQRT( R**2/DENOM )
		    CORRPROB(I,J) = 1.0 - STUDIS(N-2, T)
		ELSE
		    CORRPROB(I,J) = 0.0
		ENDIF
		CORRPROB(J,I) = CORRPROB(I,J)
	    ENDDO
	ENDDO


	IF (OUTPUT) THEN
	    DO I = 1, M
		COLS(I) = I
	    ENDDO
            CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
            IF (STATUS.NE.1) THEN
            CALL XVMESSAGE
     & (' OUTPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
              CALL ABEND
            END IF
            CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',M,M,' ',
     &           'COLUMN',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	    DO I = 1, M
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',I,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,COVARRAY(M*(I-1)+1),
     &                     I,1,M,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	    ENDDO
            CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	ENDIF

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('            CORRELATION   ',' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,I6)') 'NUMBER OF CASES:', N
	    CALL XVMESSAGE (STRING,' ')
	    CALL XVMESSAGE (' ',' ')
	    DO SWATH = 1, INT((M-1)/6)+1
		STARTCOL = 6*(SWATH-1)+1
		NUMCOL = MIN(6, M-STARTCOL+1)
		CALL XVMESSAGE (' ',' ')

                STRING(1:80) = ' '
                SLINE(1:80) = ' '
                BEGIN = 1
                DO JJ = 1,NUMCOL
                  SLINE(BEGIN:BEGIN+7) = COLNAMES(JJ)
                  BEGIN = BEGIN + 11
                ENDDO

                STRING(14:10+(NUMCOL*11)) = SLINE
		CALL XVMESSAGE (STRING,' ')
		DO ROW = 1, M
		    K = MIN (ROW-STARTCOL+1,NUMCOL)
		    IF (K .GT. 0) THEN
                        STRING(1:80) = ' '
                        SLINE(1:80) = ' '
                        BEGIN = 12
                        STRING(1:8) = COLNAMES(ROW)
                        DO JJ = 1, K
                          WRITE (SLINE, '(2X,F7.4,2X)') 
     +                           CORRMATRIX(JJ+STARTCOL-1,ROW)
                          STRING(BEGIN:BEGIN+11) = SLINE
                          BEGIN = BEGIN + 11
                        ENDDO
!			WRITE (STRING, '(3X,<K>(2X,F7.4,2X))') 
!     +				(CORRMATRIX(J+STARTCOL-1,ROW), J = 1,K)
			CALL XVMESSAGE (STRING,' ')
                        STRING(1:80) = ' '
!			WRITE (STRING, '(11X,<K>(1X,F10.3))') 
!     +			 (COVARRAY(J+STARTCOL-1+M*(ROW-1)), J = 1,K)
                        BEGIN = 12
                        DO JJ = 1, K
                          WRITE (SLINE, '(1X,F10.3)')
     +                           COVARRAY(JJ+STARTCOL-1+M*(ROW-1))
                          STRING(BEGIN:BEGIN+11) = SLINE
                          BEGIN = BEGIN + 11
                        ENDDO
			CALL XVMESSAGE (STRING,' ')
                        STRING(1:80) = ' '
                        BEGIN = 12
                        DO JJ = 1, K
                          WRITE (SLINE, '(2X,2HS=,F5.3,2X)')
     +                           CORRPROB(JJ+STARTCOL-1,ROW)
                          STRING(BEGIN:BEGIN+11) = SLINE
                          BEGIN = BEGIN + 11
                        ENDDO
!			WRITE (STRING, '(11X,<K>(2X,2HS=,F5.3,2X))') 
!     +				(CORRPROB(J+STARTCOL-1,ROW), J = 1,K)
			CALL XVMESSAGE (STRING,' ')
			CALL XVMESSAGE (' ',' ')
		    ENDIF
		ENDDO
	    ENDDO
	ENDIF


	GOTO 9999




C    *************************************************************************
C			BEHRENS-FISHER TEST SECTION
4000	CONTINUE

	IF (NINCOLS*N .GT. MAXTOTLEN)  CALL MABEND (' TOO MUCH DATA')

	CALL XVP ('BCOLS', IN2COLS, COUNT)
	IF (COUNT .NE. NINCOLS) 
     +		CALL MABEND (' TWO SAMPLES MUST HAVE SAME DIMENSION')

	CALL XVPARM ('NUMPOINT', NUMPOINT, COUNT, DEF, 2)
	IF (DEF .EQ. 0)  THEN
	    NA = NUMPOINT(1)
	    NB = NUMPOINT(2)
	ELSE
	    NA = N
	    NB = N
	ENDIF

	DO INDEX = 1, NINCOLS
	    BUFPTR = N*(INDEX-1) + 1
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA(BUFPTR),INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 IN2COLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA2(BUFPTR),IN2COLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	ENDDO
	M = NINCOLS


	CALL BEHERN (DATA, NA, DATA2, NB, M, N,N, M, 
     +		DUMMY, 0.01, T2, CORRARRAY, 
     +		WORK(3*MAXCOLS+1), WORK(MAXCOLS+1), WORK(2*MAXCOLS+1),
     +		NDF, CRIT)

	FRATIO = T2*(NA-M)/(M*(NA-1))
	FPROB = FISH(FRATIO, M, NA-M)

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('            BEHRENS-FISHER TEST  ',' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,I6,1X,I6)') 'NUMBER OF DATA POINTS:',
     +						 NA, NB
	    CALL XVMESSAGE (STRING,' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,F10.3,A,F7.3)') 
     +			'HOTELLING T SQUARED = ', T2,
     +			'      PROB = ', FPROB
	    CALL XVMESSAGE (STRING,' ')
	ENDIF


	GOTO 9999




C    *************************************************************************
C			MULTIPLE REGRESSION SECTION
5000	CONTINUE

	IF ((NINCOLS+1)*N .GT. MAXTOTLEN)  
     +			CALL MABEND (' TOO MUCH DATA TO FIT')
	IF (N .LT. NINCOLS+1)  CALL MABEND (' NOT ENOUGH ROWS OF DATA')

	DO I = 1, N
	    DATA(I) = 1.0
	ENDDO
	DO INDEX = 1, NINCOLS
	    BUFPTR = INDEX*N + 1
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA(BUFPTR),INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	ENDDO
	M = NINCOLS + 1

	CALL XVP ('DEPCOL', DEPCOL, COUNT)
	CALL XVP ('DEPNAME', DEPNAME, COUNT)
        CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 DEPCOL,STATUS)
        CALL IBIS_COLUMN_READ(INIBIS,DEPDATA,DEPCOL,
     &                 1,CLEN,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	DO I = 1, N*M
	    DATA2(I) = DATA(I)
	ENDDO
	DO I = 1, N
	    DEPDATA2(I) = DEPDATA(I)
	ENDDO

	
	EPSILON = 1.0E-7
	CALL LLSQ (DATA2, DEPDATA2, N, M, 1,
     +			SOLUTION, IPIV, EPSILON, ERR, WORK)
	IF (ERR .NE. 0)  CALL MABEND (' DEPENDENT COLUMNS')
	SSR = WORK(1)

	CALL CALCRES (DATA, DEPDATA, SOLUTION, RESID, N, M, 
     +				SSR, DURWATSTAT)

	CALL REGSTAT (DATA, DEPDATA, N, M, SSR, 
     +			SEE, RSQUARED, INTERVALS, FRATIO, FPROB)

	CALL XVPARM ('ERRCOL', ERRCOL, COUNT, ERRDEF, 1)
	IF (ERRDEF .EQ. 0) THEN
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 ERRCOL,STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,OUTDATA,ERRCOL,
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	    CHISQR = 0.0
	    DO I = 1, N
		CHISQR = CHISQR + ( RESID(I)/OUTDATA(I) )**2
	    ENDDO
	    CHIPROB = CHI2(CHISQR, FLOAT(N-M))
	    IF (CHIPROB .GT. 0.5)  CHIPROB = 1.0 - CHIPROB
	ENDIF

	PROB = 1.0 - (STUDIS(N-M, 1.0) - 0.5)


	IF (OUTPUT) THEN
	    DO I = 1, M
		OUTDATA(I) = SOLUTION(I)
	    ENDDO
          CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
          IF (STATUS.NE.1) THEN
            CALL XVMESSAGE
     & (' OUTPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
            CALL ABEND
          END IF
          CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',NCOL,CLEN,' ',
     &           'COLUMN',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',1,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,OUTDATA,1,1,
     &                     N,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',2,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,RESID,2,
     &                     1,N,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
            CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	ENDIF
	

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('            MULTIPLE REGRESSION  ',' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,I6)') 'NUMBER OF CASES:', N
	    CALL XVMESSAGE (STRING,' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,A)') 'DEPENDENT VARIABLE: ', DEPNAME
	    CALL XVMESSAGE (STRING,' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,F7.3)') 
     +			' VARIABLE     COEFFICIENT     ERROR: ', PROB
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(2X,A,4X,F11.4,4X,F11.4)') 
     +			'CONSTANT', SOLUTION(1), INTERVALS(1)
	    CALL XVMESSAGE (STRING,' ')
	    DO I = 2, M
		WRITE (STRING, '(2X,A,4X,F11.4,4X,F11.4)') 
     +			COLNAMES(I-1), SOLUTION(I), INTERVALS(I)
		CALL XVMESSAGE (STRING,' ')
	    ENDDO
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,F7.4)') 'R SQUARED = ', RSQUARED
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A,F11.4)') 'STD ERR OF EST = ', SEE
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A,F11.4,A,F7.4)') 'OVERALL :   F RATIO = ',
     +			FRATIO, '      PROB = ', FPROB
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A,F7.4)') 
     +			'DURBIN-WATSON STATISTIC = ', DURWATSTAT
	    CALL XVMESSAGE (STRING,' ')
	    IF (ERRDEF .EQ. 0) THEN
		WRITE (STRING, '(A,F12.4,A,F6.3)') 
     +			'GOODNESS OF FIT:  CHI**2/DF = ', CHISQR/(N-M),
     +			'     PROB = ', CHIPROB
		CALL XVMESSAGE (STRING,' ')
	    ENDIF
	ENDIF


	GOTO 9999




C    *************************************************************************
C			ANOVA SECTION
6000	CONTINUE

	IF (NINCOLS*N .GT. MAXTOTLEN)  CALL MABEND (' TOO MUCH DATA')

	CALL XVPARM ('REPLICS', REPLICS, COUNT, DEF, 40)
	IF (DEF .EQ. 1) THEN
	    DO I = 1, NINCOLS
		REPLICS(I) = N
	    ENDDO
	ELSE IF (COUNT .NE. NINCOLS) THEN
	    CALL MABEND
     +		 (' MUST INPUT NUMBER OF REPLICATIONS FOR EACH GROUP')
	ENDIF

	BUFPTR = 1
	DO INDEX = 1, NINCOLS
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA(BUFPTR),INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	    BUFPTR = BUFPTR + REPLICS(INDEX)
	ENDDO
	TOTREP = BUFPTR - 1
	M = NINCOLS

	CALL ANOV1 (DATA, TOTREP, REPLICS, M, RESID, YBAR, EFFECTS,
     +			SSARRAY, NDF, DUMMY, FRATIO, FPROB, ERR)
	IF (ERR .NE. 0) CALL MABEND (' SUM OF SQUARES OVERFLOW')

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('    ONEWAY ANALYSIS OF VARIANCE ',' ')
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,F11.4)') 'GRAND MEAN = ', YBAR
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A)') 
     +			' VARIABLE     ESTIMATE OF EFFECT '
	    CALL XVMESSAGE (STRING,' ')
	    DO I = 1, M
		WRITE (STRING, '(2X,A,4X,F11.4)') 
     +			COLNAMES(I), EFFECTS(I)
		CALL XVMESSAGE (STRING,' ')
	    ENDDO
	    CALL XVMESSAGE (' ',' ')
	    WRITE (STRING, '(A,F11.4,A,I4)') 
     +		  'FACTOR:      SS = ', SSARRAY(1)
     +			,	'     DF = ', NDF(1)
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A,F11.4,A,I4)') 
     +		  'RESIDUAL:    SS = ', SSARRAY(2)
     +			,	'     DF = ', NDF(2)
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A,F11.4,A,I4)') 
     +		  'TOTAL:       SS = ', SSARRAY(3)
     +			,	'     DF = ', NDF(3)
	    CALL XVMESSAGE (STRING,' ')
	    WRITE (STRING, '(A,F11.4,A,F7.4)') 
     +		'F RATIO = ', FRATIO
     +			, '      PROB = ', FPROB
	    CALL XVMESSAGE (STRING,' ')
	ENDIF

	GOTO 9999




C    *************************************************************************
C			FACTOR ANALYSIS SECTION
C			(PRINCIPAL COMPONENTS)
7000	CONTINUE

	IF (NINCOLS*N .GT. MAXTOTLEN)  CALL MABEND (' TOO MUCH DATA')

	DO J = 1, NINCOLS
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(J),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA((J-1)*N+1),INCOLS(J),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	ENDDO
	M = NINCOLS

	CALL CORRE (N, M, 1, DATA, 
     +			DUMMY(1), DUMMY(MAXCOLS+1), COVARRAY,
     +		  CORRARRAY, DUMMY(MAXCOLS**2+1), WORK, WORK(MAXCOLS+1) )

	K = 1
	DO I = 1, M
	    DO J = 1, M
		EIGENCORR(I,J) = DBLE(COVARRAY(K))/N
		K = K + 1
	    ENDDO
	ENDDO

	CALL SEIGEN (MAXCOLS, M, EIGENCORR, EIGENVAL,
     +					 WORK, EIGENCORR, ERR)
	IF (ERR .NE. 0) CALL MABEND (' DIAGONALIZATION FAILED')



	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE('   PRINCIPLE COMPONENTS FACTOR ANALYSIS  ',
     +				' ')
	    CALL XVMESSAGE (' ',' ')
	    CALL XVMESSAGE ('   FACTOR       EIGEN VALUE ',' ')
	    DO I = M, 1, -1
		WRITE (STRING, '(4X,I3,4X,F15.4)') M-I+1,SNGL(EIGENVAL(I))
		CALL XVMESSAGE (STRING,' ')
	    ENDDO
	    CALL XVMESSAGE (' ',' ')

	    DO SWATH = 1, INT((M-1)/6)+1
		STARTCOL = 6*(SWATH-1)+1
		NUMCOL = MIN(6, M-STARTCOL+1)
		CALL XVMESSAGE (' ',' ')
                STRING(1:80) = ' '
                SLINE(1:80) = ' '
                BEGIN = 1
                DO JJ = 1,NUMCOL
                  SLINE(BEGIN:BEGIN+7) = COLNAMES(JJ)
                  BEGIN = BEGIN + 11
                ENDDO
                STRING(15:10+(NUMCOL*11)) = SLINE
                CALL XVMESSAGE (STRING,' ')
                STRING(1:80) = ' '
                SLINE(1:80) = ' '

		DO J = 1, M
                  STRING(1:6) = 'FACTOR'		
                  WRITE(SLINE, '(I2)') J
                  STRING(7:8) = SLINE
                  STRING(11:11) = '='
                  BEGIN = 15
                  DO JJ = 1, NUMCOL
                    WRITE(SLINE,'(F7.4)')
     +                SNGL(EIGENCORR(JJ+STARTCOL-1,M-J+1))
                    STRING(BEGIN:BEGIN+6) = SLINE    
                    BEGIN = BEGIN + 11
                  ENDDO
!    MACOX      !!    WRITE (STRING, '(A6,I2,A4,<NUMCOL>(2X,F7.4,2X))') 
!     +			'FACTOR', J, '  = ',
!     +			(SNGL(EIGENCORR(K+STARTCOL-1,M-J+1)), K = 1,NUMCOL)
		    CALL XVMESSAGE (STRING,' ')
		ENDDO
	    ENDDO
	ENDIF



	IF (OUTPUT) THEN
	    DO I = 1, N
		DO J = 1, M
		    SUM = 0.0
		    DO K = 1, M
			SUM = SUM + EIGENCORR(K,M-J+1)*DATA(N*(K-1)+I)
		    ENDDO
		    DATA2(N*(J-1)+I) = SUM
		ENDDO
	    ENDDO
          CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
          IF (STATUS.NE.1) THEN
            CALL XVMESSAGE
     & (' OUTPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
            CALL ABEND
          END IF
          CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',NCOL,CLEN,' ',
     &           'COLUMN',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	    DO J = 1, M
              CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',J,STATUS)
              CALL IBIS_COLUMN_WRITE(OUTIBIS,DATA2(N*(J-1)+1),J,
     &                     1,N,STATUS)
              IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	    ENDDO
            CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	ENDIF


	GOTO 9999



C    *************************************************************************
C			PROBABILITY DENSITY SECTION
8000	CONTINUE

	CALL XVP ('DISTRIB', DIST, COUNT)
	CALL XVP ('NDF', NDF, COUNT)

	IF (OUTPUT) THEN
          CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
          IF (STATUS.NE.1) THEN
            CALL XVMESSAGE
     & (' OUTPUT FILE INITIAILIZATION ERROR-PROGRAM TERMINATED',' ')
            CALL ABEND
          END IF
          CALL IBIS_FILE_OPEN(OUTUNIT,OUTIBIS,'WRITE',2*NINCOLS,
     &           CLEN,' ','COLUMN',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
	ENDIF

	IF (.NOT. NOPRINT) THEN
	    CALL XVMESSAGE ('       PROBABILITY DENSITIES   ',' ')
	    CALL XVMESSAGE (' ',' ')
	ENDIF


	DO INDEX = 1, NINCOLS
            CALL IBIS_COLUMN_SET(INIBIS,'U_FORMAT','REAL',
     &                 INCOLS(INDEX),STATUS)
            CALL IBIS_COLUMN_READ(INIBIS,DATA,INCOLS(INDEX),
     &                 1,CLEN,STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)

	    IF (DIST(1:3) .EQ. 'NOR') THEN
		DO I = 1, CLEN
		    DATA2(I) = PHI (DATA(I))
		ENDDO
		IF (.NOT. NOPRINT) THEN
		    WRITE (STRING, '(A)') 
     +				'NORMAL (GAUSSIAN) DISTRIBUTION'
		ENDIF

	    ELSE IF (DIST(1:3) .EQ. 'CHI') THEN
		N1 = NDF(INDEX)
		DO I = 1, CLEN
		    DATA2(I) = CHI2 (DATA(I), FLOAT(N1) )
		ENDDO
		IF (.NOT. NOPRINT) THEN
		    WRITE (STRING, '(A,I3,A)') 
     +				'CHI SQUARED DISTRIBUTION FOR ',
     +				N1, ' DEGREES OF FREEDOM'
		ENDIF

	    ELSE IF (DIST(1:3) .EQ. 'STU') THEN
		N1 = NDF(INDEX)
		DO I = 1, CLEN
		    DATA2(I) = STUDIS (N1, DATA(I) )
		ENDDO
		IF (.NOT. NOPRINT) THEN
		    WRITE (STRING, '(A,I3,A)') 
     +				'STUDENTS T DISTRIBUTION FOR ',
     +				N1, ' DEGREES OF FREEDOM'
		ENDIF

	    ELSE IF (DIST(1:3) .EQ. 'FIS') THEN
		N1 = NDF(2*INDEX-1)
		N2 = NDF(2*INDEX)
		DO I = 1, CLEN
		    DATA2(I) = FISH (DATA(I), N1, N2)
		ENDDO
		IF (.NOT. NOPRINT) THEN
		    WRITE (STRING, '(A,I3,1X,I3,A)') 
     +				'FISHERS F DISTRIBUTION FOR ',
     +				N1, N2, ' DEGREES OF FREEDOM'
		ENDIF
	    ENDIF


	    IF (OUTPUT) THEN
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',2*INDEX-1,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,DATA,2*INDEX-1,
     &                     1,CLEN,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
                CALL IBIS_COLUMN_SET(OUTIBIS,'U_FORMAT',
     &                     'REAL',2*INDEX,STATUS)
                CALL IBIS_COLUMN_WRITE(OUTIBIS,DATA2,2*INDEX,
     &                     1,CLEN,STATUS)
                IF (STATUS.NE.1) 
     &                     CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)

	    ENDIF

	    IF (.NOT. NOPRINT) THEN
		CALL XVMESSAGE (STRING,' ')
	    ENDIF

	ENDDO

	IF (OUTPUT) THEN
          CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(OUTUNIT,STATUS,1)
        ENDIF

	GOTO 9999





9999	CONTINUE
        CALL IBIS_FILE_CLOSE(INIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

	IF (.NOT. NOPRINT)   CALL XVMESSAGE(' ',' ')

	RETURN
	END




!	CHARACTER*16 FUNCTION FSTR (VALUE, FIELDWIDTH, NDECIMALS)
!	IMPLICIT NONE
!	REAL	VALUE
!	INTEGER	FIELDWIDTH, NDECIMALS
!
!	IF ( ABS(VALUE) .GE. 10**(FIELDWIDTH-NDECIMALS-2) .OR. 
!     +	     (ABS(VALUE) .LT. 0.1**NDECIMALS .AND. VALUE .NE. 0.0) ) THEN
!	    WRITE (FSTR, '(E<FIELDWIDTH>.<NDECIMALS>)', ERR=100 )  VALUE
!	ELSE
!	    WRITE (FSTR, '(F<FIELDWIDTH>.<NDECIMALS>)', ERR=100 )  VALUE
!	ENDIF
!	RETURN
!
!100	FSTR = '****************'
!	RETURN
!	END



	SUBROUTINE REGSTAT(C, Y, N, M, SSR, SEE, RSQUARED, INTERVALS,
     +					FRATIO, FPROB)
	IMPLICIT NONE
	INTEGER	N, M
	REAL	C(N,M), Y(N), SSR, SEE, RSQUARED, INTERVALS(M)
	REAL	FRATIO, FPROB, FISH
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
	IF (ERR .NE. 0) CALL MABEND (' SINGULAR MATRIX')

	DO K = 1, M
	    INTERVALS(K) = SEE*SQRT(A(K+M*(K-1)))
	ENDDO

	IF (RSQUARED .EQ. 1.0) THEN
	    FRATIO = 99999999999.00
	    FPROB = 1.0
	ELSE
	    FRATIO = ( RSQUARED/(M-1) ) / ( (1-RSQUARED)/(N-M))
	    FPROB = FISH(FRATIO, M-1, N-M)
	ENDIF

	RETURN
	END





	SUBROUTINE CALCRES (A, B, X, RES, N, M, SSR, DURWATSTAT)
	IMPLICIT NONE
	INTEGER	I,J,  N,M
	REAL	A(N,M), B(N), X(M), RES(N), SSR, DURWATSTAT, SUM

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
	SUM = 0.0
	DO I = 2, N
	    SUM = SUM + (RES(I)-RES(I-1))**2
	ENDDO
	DURWATSTAT = SUM/SSR

	RETURN
	END


      SUBROUTINE CORRE (N,M,IO,X,XBAR,STD,RX,R,B,D,T)
C
C ----PURPOSE:  COMPUTE MEANS, STANDARD DEVIATIONS, SUMS OF SQUARES
C     AND CROSS-PRODUCTS OF DEVIATIONS FROM MEANS, AND CORRELATION
C     COEFFICIENTS.
C
C ----CALLING SEQUENCE:  CALL CORRE(N,M,IO,X,XBAR,STD,RX,R,B,D,T)
C
C     N     NUMBER OF OBSERVATION.  N MUST BE GREATER THAN
C           OR EQUAL TO 2  (INPUT)
C
C     M     NUMBER OF VARIABLES  (INPUT)
C
C     IO    =0, IF A USER PROVIDED SUBROUTINE NAMED DATA(M,D) IS TO BE
C           USED TO READ THE DATA FROM A SPECIFIC INPUT DEVICE
C           =1, IF ALL DATA ARE IN CORRE IN X  (INPUT)
C
C     X     THE N BY M DATA MATRIX (INPUT IF IO=1)
C
C     XBAR  THE VECTOR OF LENGTH M CONTAINING MEANS  (OUTPUT)
C
C     STD   THE VECTOR OF LENGTH M CONTAINING STANDARD DEVIATIONS
C           (OUTPUT)
C
C     RX    THE M BY M MATRIX CONTAINING SUMS OF SQUARES AND CROSS-
C           PRODUCTS OF DEVIATIONS FROM MEANS. (SSCP MATRIX) (OUTPUT)
C
C     R     OUTPUT VECTOR CONTAINING CORRELATION COEFFICIENTS.
C           ONLY THE UPPER TRIANGULAR PORTION OF THE CORRELATION
C           MATRIX IS STORED.  THE COEFFICIENT IN THE I-TH ROW AND
C           THE J-TH COLUMN IS IN POSITION K=(I*(I-1)/2)+J OF R(K).
C
C     B     THE VECTOR OF LENGTH M CONTAINING THE DIAGONAL ELEMENTS
C           OF R, I.E. THE SUMS OF SQUARES   (OUTPUT)
C
C     D,T   WORKING VECTORS OF LENGTH M
C
C ----RESTRICTIONS AND COMMENTS:  THE USER MUST SUPPLY A
C     SUBROUTINE CALLED DATA(M,D)
C
C   --IF IO=0, DATA(M,D) READS ONE VECTOR, D, OF LENGTH M,
C     FROM A SPECIFIC INPUT DEVICE, I.E. ONE OF N OBSERVATION VECTORS,
C     EACH OF LENGTH M, IS STORED IN D EACH TIME DATA(M,D) IS CALLED.
C
C   --IF IO=1, AN UNUSED SUBROUTINE NAMED DATA MUST BE SUPPLIED.
C     FOR EXAMPLE:
C           SUBROUTINE DATA
C           RETURN
C           END
      DIMENSION X(1),XBAR(1),STD(1),RX(1),R(1),B(1),D(1),T(1)
C
C ----INITIALIZATION
      DO 100 J=1,M
      B(J)=0.0
  100 T(J)=0.0
      K=(M*M+M)/2
      DO 102 I=1,K
  102 R(I)=0.0
      FN=N
      L=0
C
C ----TEST IO
      IF(IO.EQ.0) GO TO 127
C
C ----DATA ARE ALREADY IN CORE
      DO 108 J=1,M
      DO 107 I=1,N
      L=L+1
      T(J)=T(J)+X(L)
  107 CONTINUE
      XBAR(J)=T(J)
      T(J)=T(J)/FN
  108 CONTINUE
C
      DO 115 I=1,N
      JK=0
      L=I-N
      DO 110 J=1,M
      L=L+N
      D(J)=X(L)-T(J)
  110 B(J)=B(J)+D(J)
      DO 115 J=1,M
      DO 115 K=1,J
      JK=JK+1
      R(JK)=R(JK)+D(J)*D(K)
  115 CONTINUE
      GO TO 205
C
C ----STORE PART OF THE DATA IN RX AND COMPUTE PARTIAL MEANS
  127 IF(N-M) 130, 130, 135
  130 KK=N
      GO TO 137
  135 KK=M
  137 DO 140 I=1,KK
C      CALL DATA (M,D)
      DO 140 J=1,M
      T(J)=T(J)+D(J)
      L=L+1
  140 RX(L)=D(J)
      FKK=KK
      DO 150 J=1,M
      XBAR(J)=T(J)
  150 T(J)=T(J)/FKK
C
C ----CALCULATE SUMS OF CROSS-PRODUCTS OF DEVIATIONS
C     FROM TEMPORARY MEANS FOR M OBSERVATIONS
      L=0
      DO 180 I=1,KK
      JK=0
      DO 170 J=1,M
      L=L+1
  170 D(J)=RX(L)-T(J)
      DO 180 J=1,M
      B(J)=B(J)+D(J)
      DO 180 K=1,J
      JK=JK+1
      R(JK)=R(JK)+D(J)*D(K)
  180 CONTINUE
C
C ----TEST KK
      IF(N-KK) 205, 205, 185
C
C ----READ THE REST OF OBSERVATIONS ONE AT A TIME, SUM
C     THE OBSERVATION, AND CALCULATE SUMS OF CROSS-
C     RPODUCTS OF DEVIATIONS FROM TEMPORARY MEANS
  185 KK=N-KK
      DO 200 I=1,KK
      JK=0
C      CALL DATA (M,D)
      DO 190 J=1,M
      XBAR(J)=XBAR(J)+D(J)
      D(J)=D(J)-T(J)
  190 B(J)=B(J)+D(J)
      DO 200 J=1,M
      DO 200 K=1,J
      JK=JK+1
      R(JK)=R(JK)+D(J)*D(K)
  200 CONTINUE
C
C ----CALCULATE MEANS
  205 JK=0
      DO 210 J=1,M
      XBAR(J)=XBAR(J)/FN
C
C ----ADJUST SUMS OF CROSS-PRODUCTS OF DEVIATIONS
C     FROM TEMPORARY MEANS
      DO 210 K=1,J
      JK=JK+1
      R(JK)=R(JK)-B(J)*B(K)/FN
  210 CONTINUE
C
C ----CALCULATE CORRELATION COEFFICIENTS
      JK=0
      DO 220 J=1,M
      JK=JK+J
  220 STD(J)= SQRT( ABS(R(JK)))
      DO 230 J=1,M
      DO 230 K=J,M
      JK=J+(K*K-K)/2
      L=M*(J-1)+K
      RX(L)=R(JK)
      L=M*(K-1)+J
      RX(L)=R(JK)
      IF(STD(J)*STD(K)) 225, 222, 225
  222 R(JK)=0.0
      GO TO 230
  225 CONTINUE
      R(JK)=R(JK)/(STD(J)*STD(K))
  230 CONTINUE
C
C ----CALCULATE STANDARD DEVIATIONS
      FN=SQRT(FN-1.0)
      DO 240 J=1,M
  240 STD(J)=STD(J)/FN
C
C ----COPY THE DIAGONAL OF THE MATRIX OF SUMS OF CROSS-PRODUCTS OF
C     DEVIATIONS FROM MEANS
      L=-M
      DO 250 I=1,M
      L=L+M+1
  250 B(I)=RX(L)
      RETURN
      END



      SUBROUTINE ANOV1(Y,N,J,I,R,YBAR,S,SS,NDF,SM,F,PF,IND)             
C
C ----PURPOSE:  PERFORM ONE WAY ANALYSIS OF VARIANCE WITH AN UNEQUAL
C     NUMBER OF REPLICATIONS IN EACH CELL
C
C ----CALLING SEQUENCE: ANOV1(Y,N,J,I,R,YBAR,S,SS,NDF,SM,F,PF,IND)
C
C     Y     IS THE ARRAY OF DATA
C
C     N     IS THE TOTAL NUMBER OF REPLICATIONS
C
C     J     IS THE ARRAY OF THE NUMBER OF REPLICATIONS FOR EACH GROUP
C
C     I     IS THE NUMBER OF GROUPS
C
C     R     IS THE ARRAY OF RESIDUALS
C
C     YBAR  IS THE GRAND MEAN
C
C     S     IS THE ESTIMATE OF EFFECTS ARRAY
C
C     SS    IS THE SUMS OF SQUARES ARRAY
C
C     NDF   IS THE ARRAY OF THE DEGREES OF FREEDOM FOR EACH SUM
C           OF SQUARES
C
C     SM    IS THE  MEAN SQUARES ARRAY
C
C     F     IS THE F-STATISTIC
C
C     PF    IS THE PROBABILITY THAT AN F-DISTRIBUTED VARIABLE EXCEEDS F
C
C     IND   =0, IF OVERFLOW DOES NOT OCCUR
C           =1, IF OVERFLOW COMPUTING THE SUMS OF SQUARES OCCURS
C
C ----RESTRICTIONS AND COMMENTS: FISH AND ERFF ARE REQUIRED
      DIMENSION Y(N),R(N),J(I),SS(3),NDF(3),SM(3) ,S(I)
      XMAX=1.E30
      IND=1
C
C ----INITIALIZATION
      SS(1)=0.0
      SS(2)=0.0
      SS(3)=0.0
      LJ=1
      MJ=0
C
C ----CALCULATE THE GRAND MEAN
      YBAR=0.0
      DO 3 KI=1,N
    3 YBAR=YBAR+Y(KI)
      YBAR=YBAR/N
C
C ----CALCULATE THE GROUP MEANS
      DO 2 KI=1,I
      MJ=MJ+J(KI)
      YX=0.0
      DO 4  K=LJ,MJ
    4 YX=YX+Y( K)
      YX=YX/J(KI)
C
C ----CALCULATE THE GROUP EFFECTS
      S(KI)=YX-YBAR
C
C ----CALCULATE THE GROUP SUM OF SQUARES
      SS(1)=SS(1)+S(KI)*S(KI)*J(KI)
      DO 1 K=LJ,MJ
      R(K)=Y(K)-YX
C
C ----CALCULATE RESIDUAL SUM OF SQUARES
      SS(2)=SS(2)+R(K)*R(K)
C
C ----CALCULATE TOTAL SUM OF SQUARES
      SS(3)=SS(3)+(Y(K)-YBAR)*(Y(K)-YBAR)
      IF(SS(3).GT.XMAX) RETURN
    1 CONTINUE
    2 LJ=MJ+1
C
C ----CALCULATE THE DEGREES OF FREEDOM
      NDF(1)=I-1
      NDF(2)=N-I
      NDF(3)=N-1
C
C ----CALCULATE THE MEAN SQUARES
      SM(1)=SS(1)/NDF(1)
      SM(2)=SS(2)/NDF(2)
C
C ----CALCULATE THE F-STATISTIC
      F=SM(1)/SM(2)
C
C ----CALCULATE THE PROBABILITY THAT THE F-STATISTIC IS EXCEEDED
C     UNDER THE HYPOTHESIS THAT THERE IS NO DIFFERENCE BETWEEN THE
C     GROUPS
      PF=FISH(F,NDF(1),NDF(2))
      IND=0
      RETURN
      END


      SUBROUTINE BEHERN(A,NA,B,NB,M,NNA,NNB,MM,X,AL,T2,S,U,AF,AT,N2,
     .                  CRIT)
C
C ----PURPOSE: BEHERN  IS A SUBROUTINE FOR TESTING THE EQUALITY OF
C     TWO MULTIVARIATE NORMAL MEANS WHEN COVARIANCE MATRICES ARE
C     ASSUMED UNEQUAL.
C
C ----CALLING SEQUENCE: CALL BEHERN(A,NA,B,NB,M,NNA,NNB,MM,X,AL,T2,
C     S,U,AF,AT,N2,CRIT)
C
C     A     THE NA BY M ARRAY OF DATA FROM THE FIRST POPULATION
C
C     NA    THE NUMBER OF DATA POINTS IN THE FIRST POPULATION
C
C     B     THE NB BY M ARRAY OF DATA FROM THE SECOND POPULATION
C
C     NB    THE NUMBER OF DATA POINTS IN THE SECOND POPULATION
C
C     M     THE NUMBER OF VARIABLES (DIMENSIONS) IN THE
C           MULTIVARIATE POPULATIONS
C
C     NNA   THE MAXIMUM FIRST DIMENSION OF A
C
C     NNB   THE MAXIMUM FIRST DIMENSION OF B
C
C     MM   THE MAXIMUM FIRST DIMENSION OF S AND U
C
C     X     THE VECTOR OF DIMENSION M WHICH ARE THE VALUES OF THE
C           NULL HYPOTHESIS OF MEAN DIFFERENCES
C
C     AL    THE CONFIDENCE INTERVAL TYPE 1 ERROR
C
C     T2    THE HOTELLING T**2 VALUE
C
C     S     THE M BY M WORK ARRAY CONTAINING THE INVERSE OF THE
C           VARIANCE-COVARIANCE MATRIX, ON RETURN
C
C     U     AN M BY M WORK ARRAY
C
C     AF    IS AN M DIMENSIONAL WORK VECTOR
C
C     AT    AN M DIMENSIONAL WORK VECTOR
C
C     N2    DEGREES OF FREEDOM FOR TESTING (OUTPUT)
C
C     CRIT  THE COMPUTED CRITICAL VALUE FOR THE GIVEN AL (OUTPUT)
C
C ----RESTRICTIONS AND COMMENTS:  ONE MAY NOT HAVE PRIOR KNOWLEDGE THAT
C     COVARIANCE-A.NE.COVARIANCE-B  (SEE SUBROUTINE T2DIFF FOR EQUAL
C     COVARIANCE ASSUMPTION)
      REAL*8 S,U,AF,AT
      DIMENSION A(NNA,MM),B(NNB,MM),X(MM),S(MM,MM),U(MM,MM),AF(MM),
     2          AT(MM)
C
C ----CALCULATE VARIANCE-COVARIANCE MATRICES
      CALL COVARI(A,NA,M,NNA,MM,S,AF)
      CALL COVARI(B,NB,M,NNB,MM,U,AT)
C
C ----POOL VARIANCE-COVARIANCE MATRICES
      DO 30 I=1,M
   30 X(I)=X(I)+AF(I)-AT(I)
      MIN=NA
      IF(MIN.LE.NB) GO TO 21
      MIN=NB
      CALL POOLT2(B,NB,A,NA,M,NNB,NNA,MM,X,S,AT,AF,MIN,U)
      GO TO 29
   21 CALL POOLT2(A,NA,B,NB,M,NNA,NNB,MM,X,S,AF,AT,MIN,U)
   29 CONTINUE
C
      TOL=-1.
      CALL GINV (S,MM,MM,M,M,K,U,AF,AT)
C
      DO 40 I=1,M
      AF(I)=0.
      DO 40 J=1,M
   40 AF(I)=AF(I) + X(J)*S(J,I)
      T2=0.
      DO 50 I=1,M
   50 T2=T2 + AF(I)*X(I)
      T2=MIN*T2
      N2=MIN-M
      IF(N2.GT.0) GO TO 58
      RETURN
   58 CRIT= FLOAT((MIN-1)*M)*FISHIN(AL,M,N2)/N2
      RETURN
      END

      SUBROUTINE COVARI(A,N,M,NN,MM,S,X)
C
C ----PURPOSE: TO COMPUTE A COVARIANCE AS REQUIRED BY BEHER AND T2DIFF
C
C ----CALLING SEQUENCE: CALL COVARI(A,N,M,NN,MM,S,X)
C
C     A     THE NN BY MM ARRAY HOLDING THE N BY M ARRAY
C           OF N DATA VECTORS EACH OF LENGTH M  (INPUT)
C
C     N     THE NUMBER OF OBSERVED VECTORS  (INPUT)
C
C     M     THE LENGTH OF EACH OBSERVATION VECTOR  (INPUT)
C
C     NN    THE MAXIMUM NUMBER OF OBSERVATIONS  (INPUT)
C     MM    THE MAXIMUM LENGTH OF THE OBSERVATION VECTORS (INPUT)
C     S     THE MM BY MM ARRAY HOLDING THE M BY M COVARIANCE MATRIX
C           (OUTPUT)
C
C     X     THE MM VECTOR HOLDING THE M SAMPLE MEANS  (OUTPUT)
C
C ----RESTRICTIONS AND COMMENTS:  NONE
      REAL*8 S,X
      DIMENSION A(NN,MM),S(MM,MM),X(MM)
C
C ----INITIALIZATION
      MN=M-1
      DO 1 I=1,MN
      L=I+1
      DO 1 J=L,M
    1 S(I,J)=0.0
      DO 3 I=1,M
      SS=0.0
C
C ----COMPUTE SAMPLE MEANS
      DO 2 K=1,N
    2 SS=SS+A(K,I)
    3 X(I)=SS/FLOAT(N)
C
C ----COMPUTE THE COVARIANCE MATRIX
      DO 5 I=1,M
      SS=0.0
      DO 4 K=1,N
    4 SS=SS+(A(K,I)-X(I))**2
    5 S(I,I)=SS
      DO 7 K=1,N
      L=0
      DO 6 I=1,MN
      L=I+1
      DO 6 J=L,M
    6 S(I,J)=S(I,J)+(A(K,I)-X(I))*(A(K,J)-X(J))
    7 CONTINUE
      DO 8 I=2,M
      L=I-1
      DO 8 J=1,L
    8 S(I,J)=S(J,I)
      DO 9 I=1,M
      DO 9 J=1,M
    9 S(I,J)=S(I,J)/FLOAT(N-1)
      RETURN
      END

      SUBROUTINE POOLT2(A,NA,B,NB,M,NNA,NNB,MM,X,S,AF,AT,MIN,WORK)
C
C ----PURPOSE: TO COMPUTE THE POOLED COVARINACE MATRIX FOR THE BEHERN
C     TEST WHEN THE COVARIANCE MATRICES ARE ASSUMED UNEQUAL.
C
C ----CALLING SEQUENCE: CALL POOLT2(A,NA,B,NB,M,NNA,NNB,MM,X,S,AF,AT,MIN
C                                    ,WORK)
C
C     A     THE NA BY M ARRAY OF DATA FOR THE FIRST POPULATION (INPUT)
C
C     NA    THE NUMBER OF DATA POINTS OF THE FIRST POPULATION (NA LESS
C           THAN OR EQUAL TO NB) (INPUT)
C
C     B     THE NB BY M ARRAY OF DATA FOR THE SECOND POPULATION (INPUT)
C
C     NB    THE NUMBER OF DATA POINTS OF THE SECOND POPULATION (NB
C           GREATER THAN OR EQUAL TO NA) (INPUT)
C
C     M     THE NUMBER OF VARIABLES (DIMENSION) IN THE MULTIVARIATE
C           POPULATION (INPUT)
C
C     NNA   THE MAXIMUM FIRST DIMENSION OF A (INPUT)
C
C     NNB   THE MAXIMUM FIRST DIMENSION OF B (INPUT)
C
C     MM    THE MAXIMUM FIRST DIMENSION OF S AND U (INPUT)
C
C     X     THE WORKING ARRAY OF MEANS: X(I)=X(I)+AF(I)-AT(I), WHERE THE
C           X(I) ON THE RIGHT ARE CONSTANTS SPECIFIED BY THE NULL
C           HYPOTHESIS. (INPUT)
C
C     S     THE M BY M POOLED COVARIANCE MATRIX RETURNED TO THE USER
C           (OUTPUT)
C
C     AF    THE VECTOR OF SAMPLE MEANS FOR THE FIRST POPULATION (INPUT)
C
C     AT    THE VECTOR OF SAMPLE MEANS FOR THE SECOND POPULATION (INPUT)
C
C     MIN   THE MINIMUM OF NA AND NB (INPUT)
C
C     WORK  A WORKING ARRAY OF DIMENSION M
C
C ----RESTRICTIONS AND COMMENTS:THE ARRAYS S, AF, AT, AND WORK ARE
C     DOUBLE PRECISION ARRAYS.
      REAL*8 S,AF,AT,WORK
      DIMENSION A(NNA,MM),B(NNB,MM),X(MM),S(MM,MM),AF(MM),AT(MM),
     *WORK(MM)
C
      SNBNA=1./(MIN-1.)
      IF(NA.EQ.NB) GO TO 100
      SNA=1./NA
      SNAOB=SQRT(FLOAT(NA)/FLOAT(NB))
C
      DO 10 I=1,M
      W=0.
      DO 9 J=1,MIN
    9 W=W+B(J,I)
   10 WORK(I)= W*SNA
      DO 20 I=1,M
      DO 20 J=I,M
      W=0.
      DO 19 K=1,MIN
   19 W=W+(A(K,I)-AF(I)-X(I)-SNAOB*(B(K,I)-WORK(I)))*
     2    (A(K,J)-AF(J)-X(J)-SNAOB*(B(K,J)-WORK(J)))
      S(I,J)=W*SNBNA
   20 S(J,I)=S(I,J)
      RETURN
  100 DO 120 I=1,M
      DO 120 J=I,M
      W=0.
      DO 119 K=1,NA
  119 W=W+(A(K,I)-AF(I)-X(I)-B(K,I)+AT(I))*
     2    (A(K,J)-AF(J)-X(J)-B(K,J)+AT(J))
      S(I,J)=W*SNBNA
  120 S(J,I)=S(I,J)
      RETURN
      END


       FUNCTION STUDIS(N,T)
C
C ----PURPOSE: TO COMPUTE AN APPROXIMATION TO THE CUMMULATIVE DISTRIBU-
C     TION FUNCTION OF STUDENT'S T-DISTRIBUTION
C
C ----CALLING SEQUENCE: VAR=STUDIS(N,T)
C
C     N     THE DEGREES OF FREEDOM,N.GE.1  (INPUT)
C
C     T     THE VALUE AT WHICH THE CUMULATIVE PROBABILITY IS DESIRED
C           (INPUT)
C
C    STUDIS THE VALUE OF THE FUNCTION (OUTPUT)
C
C ----RESTRICTIONS AND COMMENTS;  FOR LARGE N USE NORMAL APPROX-
C     MATIONS
C
C ----INITIALIZATION
C
       AN=N
       TH=ATAN(T/SQRT(AN))
C
C ----TESTN = 1
C
       W=COS(TH)
       A=W*W
      IF(N.EQ.1) GOTO 60
      J=N-2
C
C ----TEST N EVEN
C
      IF (MOD(N,2).EQ.0) GOTO13
C
C ----COMPUTE EXPAN FOR N ODD AND N.GE.3
C
       SU=W
      IF (N.EQ.3) GOTO 11
       R=0.
       V=1.
       DO 10 I=3,J,2
       R=R+2.0
       V=V+2.0
       W=W*(R/V)*A
       SU=SU+W
   10 CONTINUE
   11 A=2./3.1415927*(TH+SIN(TH)*SU)
       GOTO 50
C
C ----COMPUTE STUDIS FOR EVEN
C
   13  W=1.
       SU=1.
      IF (N.EQ.2) GOTO 40
       R=-1.
       V=0.
       DO 20 I=2,J,2
       R=R+2.
       V=V+2.
       W=W*R/V*A
       SU=W+SU
   20  CONTINUE
   40  A=SIN(TH)*SU
       GOTO 50
   60 A=2./3.1415927*TH
C
C ----CHANGE FROM PR(ABS(X).LE.T) TO PR(X.LE.T)
C
   50  CONTINUE
      A=0.5*A+0.5
      STUDIS=A
       RETURN
       END



      FUNCTION CHI2(X,V)
C
C-----PURPOSE:  TO COMPUTE AN APPROXIMATE VALUE OF THE CHI-SQUARE
C     CUMULATIVE DISTRIBUTION FUNCTION GIVEN A POINT AND THE
C     DEGREES OF FREEDOM.
C
C-----CALLING SEQUENCE:  VARIABLE = CHI2(X,V)
C
C     X     THE POINT AT WHICH THE FUNCTION IS TO BE EVALUATED.
C     V     DEGREES OF FREEDOM  (REAL,POSITIVE)
C     CHI2  COMPUTED VALUE OF THE FUNCTION.
C ----RESTRICTIONS AND COMMENTS:  FUNCTIONS GAMINC AND PHI ARE REQUIRED
C
      IF (X.LT.160.) GO TO 5
      Z = (X-V)/SQRT(2.*V)
      CHI2 = PHI(Z)
      RETURN
C
    5 X2=X/2.0
      A=V/2.0
      CHI2=GAMINC(X2,A)
      RETURN
      END


      FUNCTION GAMINC(X,A)
C
C ----PURPOSE:  TO COMPUTE AN APPROXIMATION TO THE CUMULATIVE
C     DISTRIBUTION FUNCTION OF A GAMMA DISTRIBUTION, I.E.
C     APPROXIMATE INCOMPLETE GAMMA INTEGRALS.
C
C ----CALLING SEQUENCE:  VARIABLE = GAMINC(X,A)
C
C     X     THE VALUE OF THE RANDOM VARIABLE, X.GE.0  (INPUT)
C
C     A     THE VALUE OF THE PARAMETER, A.GE.0  (INPUT)
C
C     GAMINC THE VALUE OF THE CUMULATIVE DISTRIBUTION FUNCTION (OUTPUT)
C
C ----RESTRICTIONS AND COMMENTS:  NONE
      DATA A1,A2,A3,A4,A5,A6,A7,A8/-.5771916  ,.9882058  ,-.8970569  ,
     1.9182068  ,-.7567040  ,.4821993  ,-.1935278  ,.0358683  /
      DATA RELEPS/1.E-7/
      IF(X.GT.0.) GO TO 1
      GAMINC=0.0
      RETURN
    1 N=A
      F=A-N
C
C ----COMPUTE (X**F)EXP(-X)/GAMMA(F+1) FOR COEFFICIENTS
C     SEE EQN. 6.1.36,P. 257  ABRAMOWITZ AND STEGUN
      P=EXP(-X+F*ALOG(X))/(1.+F*(A1+F*(A2+F*(A3+F*(A4+F*(A5+F*(A6+F*(A7+
     1F*A8))))))))
C
C ----COMPUTE P*(1+F)...(N+F) TO COMPLETE THE COMPUTATION OF GAMMA (A+1)
      IF(N.EQ.0)  GO TO 5
      DO 2 I=1,N
    2 P=P*X/(F+I)
C
C ----INITIALIZE GAMINC
    5 GAMINC=P
      IF (P.EQ.0.) GO TO 6
      F=A+1.
C
C ----COMPUTE NEXT TERM IN THE APPROXIMATING SUM
    3 P=P*X/F
C
C ----TEST RELATIVE ERROR
      IF((P/GAMINC).LT.RELEPS) GO TO 4
C
C ----ACCUMULATE SUM
      GAMINC=GAMINC+P
      F=F+1.0
      GO TO 3
    4 RETURN
    6 GAMINC = -1.0
      RETURN
      END


      FUNCTION FISH(F,NN1,NN2)
C
C ----PURPOSE: TO COMPUTE THE CUMULATIVE DISTRIBUTION FUNCTION FOR
C     FISHER F-DISTRIBUTION
C
C ----CALLING SEQUENCE:  VARIABLE = FISH(F,N1,N2)
C     F     THE RATION (U/N1)/(V/N2), WHERE U AND V ARE INDEPENDENT
C           CHI-SQUARE RANDOM VARIABLES WITH N1 AND N2 DEGREES
C           OF FREEDOM, RESPECTIVELY.  (INPUT)
C
C     N1    THE FIRST DEGREES OF FREEDOM, N1.GE.1  (INPUT)
C
C     N2    THE SECOND DEGREES OF FREEDOM, N2.GE.1  (INPUT)
C
C ----RESTRICTIONS AND COMMENTS:  SUBROUTINE ERFF IS REQUIRED TO
C     EVALUATE PHI(X)
      LOGICAL E1,E2,E3
      N1 = NN1
      N2 = NN2
       IF(N1.GE.100.AND.N2.GE.100) GOTO 9
C
C ----INITIALIZATION AND SETTING OF LOGICAL SWITCHES TO .TRUE. IF
C     THE DEGREES OF FREEDOM ARE EVEN
      E1=.FALSE.
      E2=.FALSE.
      E3=.FALSE.
      IF(MOD(N1,2).EQ.0) E1=.TRUE.
      IF(MOD(N2,2).EQ.0) E2=.TRUE.
      X=N2/(N2+N1*F)
      IF(.NOT.(E1.OR.E2)) GO TO 5
      IF(E1.AND..NOT.E2) GO TO 1
      IF(.NOT.E1.AND.E2) GO TO 2
      IF(N1.LE.N2) GO TO 1
C
C ----INITIALIZATION FOR SECOND DEGREE OF FREEDOM EVEN AND LESS THAN
C     FIRST DEGREE OF FREEDOM IF IT TOO IS EVEN
    2 I=N1
      N1=N2
      N2=I
      X=1.0-X
      E3=.TRUE.
C
C ----INITIALIZATION FOR FIRST DEGREE OF FREEDOM EVEN AND LESS THAN
C     SECOND DEGREE OF FREEDOM IF IT IS EVEN
    1 Y=1.0-X
C
C ----CALCULATION OF PROBABILITY FOR AT LEAST ONE DEGREE OF FREEDOM
C     EVEN
      FISH=0.0
      H=SQRT(X**N2)
      M=N1/2
      DO  3 I=1,M
      FISH=FISH+H
    3 H=(H*Y*(N2+2.*(I-1))) / (2.*I)
      IF(E3) GO TO 4
C
C ----ADJUST CALCULATED PROBABILITY IF ITS ONES COMPLEMENT WAS
C     CALCULATED ORIGINALLY
      FISH=1.0-FISH
      RETURN
    4 I=N1
      N1=N2
      N2=I
      RETURN
C
C ----CALCULATION OF THE PROBABILITY FOR BOTH DEGREES OF FREEDOM ODD
    5 Y=1.0-X
      H=.63661977*SQRT(X*Y)
      FISH=.63661977* ACOS(SQRT(X))
      IF(N2.EQ.1) GO TO 8
      M=N2-2
      DO  6  I=1,M,2
      FISH=FISH+H
    6 H=H*X*(I+1)/(I+2)
    8 IF(N1.EQ.1) RETURN
      H=H*N2
      M=N1-2
      DO  7  I=1,M,2
      FISH=FISH-H
    7 H=H*Y*(N2+I)/(I+2)
      RETURN
    9  D1=N1
       D2=N2
       DT=(D1/D2)*F
       DN=SQRT((2.*D2-1.)*DT)-SQRT(2.*D1-1.)
       X=DN/SQRT(1.+DT)
       FISH=PHI(X)
       RETURN
      END


      FUNCTION ERFF(X,N1)
C ----PURPOSE:  TO COMPUTE THE VALUE OF THE ERROR FUNCTION OR
C     THE STANDARD NORMAL CUMULATIVE DISTRIBUTION FUNCTION
C     AT A GIVEN POINT.
C
C ----CALLING SEQUENCE :  VARIABLE = ERFF(X,N1)
C
C     X     IS THE POINT WHERE THE FUNCTION IS EVALUATED (INPUT)
C
C     N1    =1, IF THE ERROR FUNCTION IS DESIRED.
C           =2, IF THE COMPLIMENTARY ERROR FUNCTION IS DESIRED.
C           =3, IF THE STANDARD NORMAL CUMULATIVE DISTRIBUTION
C           FUNCTION IS DESIRED.
C
C ----RESTRICTIONS AND COMMENTS: NONE.
      REAL ABS,T,V
      GO TO (1,2,3),N1
C
C ----ERF
      ENTRY ERF(X)
    1 N=1
      T=X
      GO TO 4
C
C ----ERFC
      ENTRY ERFC(X)
    2 N=2
      T=X
      GO TO 4
C
C ----PHI
      ENTRY PHI(X)
    3 N=3
      T=X/1.4142136
    4 M=1
      IF(X.LT.0.) M=2
      V=ABS(T)
      U=T*T
C
C ----TEST V
      IF(V-0.5) 10,10,20
C
C ----CASE V.LE.0.5
   10 ERFF= ((0.31665289*U+1.7222758)*U+21.385332)/((U+7.8437457)*U
     1    +18.952257)*T
C
C ----BRANCH TO RETURN THE APPROPRIATE FUNCTION
      GO TO (5,6,7),N
    5 RETURN
    6 ERFF=1.0-ERFF
      RETURN
    7 ERFF=0.5+0.5*ERFF
      RETURN
C
   20 X1=V
      X2=V*V
      X3=V*X2
      X4=X2*X2
C
C ----TEST V
      IF(V-8.0) 25,25,30
C
C ----CASE  V BETWEEN 0.5 AND 8.0 (HART #5703,P 294)
   25 ERFF=EXP(-X2)*(6.1337529+X1*6.1772458+X2*2.8501393+X3*0.56409092)
     1  /  (6.1337546+X1*13.098327+X2*11.497651+X3*5.0472398+X4)
      GO TO 40
C
C ----EXCEPT FOR THE LAST CASE, ERFF=0
   30 ERFF=0.
C
C ----TEST N AND V
      IF((N.NE.2.AND.M.EQ.1).OR.(N.NE.3.AND.M.EQ.2).OR.(V.GT.13.))GOTO40
C
C ----CASE  V BETWEEN 8. AND 13.5 AND ERFC OR PHI(-V) IS DESIRED
C     (HART #5721,P 296)
      ERFF=EXP(-X2)*(0.14845921+X1*0.56418774)/(0.51143728+
     1     X1*0.26277059+X2)
C
C ----BRANCH IF X.LT.0 ,OTHERWISE BRANCH TO RETURN THE
C     APPROPRIATE FUNCTION.
   40 IF(M.GT.1) GO TO 50
      GO TO(41,5,42),N
   41 ERFF=1.0-ERFF
      RETURN
   42 ERFF=0.5-0.5*ERFF+0.5
      RETURN
C
C ----BRANCH TO RETURN THE APPROPRIATE FUNCTION
   50 GO TO (51,52,53),N
   51 ERFF=ERFF-1.0
      ERF=ERFF
      RETURN
   52 ERFF=2.0-ERFF
      ERFC=ERFF
      RETURN
   53 ERFF=0.5*ERFF
      PHI=ERFF
      RETURN
      END


       FUNCTION FISHIN(ALPHA,N1,N2)
C
C ----PURPOSE:  TO COMPUTE THE INVERSE OF FISHER'S
C     F-DISTRIBUTION, GIVEN THE CONFIDENCE COEFFICIENT.
C
C ----CALLING SEQUENCE:  VARIABLE = FISHIN (ALPHA,N1,N2)
C
C     FISHIN  THE NAME OF THE FUNCTION CONTAINING THE
C             VALUE OF THE INVERSE (OUTPUT)
C
C     ALPHA   THE SIGNIFICANCE LEVEL (E.G. .01,.05.,ETC) (INPUT)
C
C     N1      THE DEGREES OF FREEDOM OF THE NUMERATOR (INPUT)
C
C     N2      THE DEGREES OF FREEDOM OF THE DENOMINATOR (INPUT)
C ----RESTRICTIONS AND COMMENTS:  THE SUBROUTINES FISH, PHINV, AND
C     ERFF ARE REQUIRED.
C
C ----CALCULATES THE INVERSE 'F' VALUE GIVEN THE CONFIDENCE COEFFICIENT
C     ALPHA AND THE DEGREES OF FREEDOM(N).
      Y1=N1
      Y2=N2
C
C ----ADJUST FOR DEGREES OF FREEDOM EQUAL TO 1
      IF(N1.EQ.1)  Y1=2
      IF(N2.EQ.1)  Y2=2
C
C ----CALL PHINV TO GET INVERSE NORMAL VALUE OF 1.-ALHPA
       X=PHINV(1.-ALPHA)
C
C ----COMPUTE LAMDA VALUE
      Y=(X**2-3.)/6.
      IC=0
C
C ----COMPUTE THE INITIAL APPROXIMATION TO THE INVERSE 'F' FUNCTION
      Y1=1./(Y1-1.)
      Y2=1./(Y2-1.)
      H=2./(Y1+Y2)
      X=X*SQRT(H+Y)/H-(Y1-Y2)*(Y+5./6.-2./(3.*H))
      X=EXP(2.*X)
      ISIZ=1
      IF(N1.GT.100.OR.N2.GT.100) ISIZ=2
C
C ----COMPUTE THE CONSTANT TO THE 'F' DISTRIBUTION, TESTING FOR N1 AND/
C     OR N2 ODD OR EVEN.
      G=1.
      IB1=2
      IF(MOD(N1,2).EQ.0) GO TO 1
      G=1.7724539
      IB1=1
    1 IB2=2
      IF(MOD(N2,2).EQ.0) GO TO 2
      G=G*1.7724539
      IB2=1
    2 IB3=2
      IF(MOD(N1+N2,2).EQ.0) GO TO 3
      G=G/1.7724539
      IB3=1
    3 IF((IB1+IB2).NE.2)  G=2.*G
      IF((N1+N2).LE.3)  GO TO 5
      ND=N1+N2-2-IB3 + 1
      DO 4 J=1,ND,2
      I=J-1
      IF((IB1+I).LE.(N1-2))  G=G*(IB1+I)
      IF((IB2+I).LE.(N2-2))  G=G*(IB2+I)
    4 G=G/(IB3+I)
C
C ----COMPUTE THE VALUE OF FISHIN
      GO TO (5,55),ISIZ
    5 Y2=N2/(N2+N1*X)
      Y1=1.-Y2
      Y=1.+(G*(1.-ALPHA-FISH(X,N1,N2)))/SQRT(Y1**N1*Y2**N2)
      GO TO 6
 55   Y2=N2/(N2+N1*X)
      Y1=(N1*X)/(N2+N1*X)
      Z=((X**(1./3.)*((9.*N2-2.)/(9.*N2)))-((9.*N1-2.)/(9.*N1)))/SQRT((2
     *./9.)*(1./N1+(X**(2./3.))*(1./N2)))
      F=PHI(Z)
      DENOM=N1/2.*ALOG(Y1)+N2/2.*ALOG(Y2)
      Y=1.+EXP(ALOG(G)-DENOM)*(1.0-ALPHA-F)
    6 FISHIN=X*Y
C
C ----IF FISHIN IS NEGATIVE, RESET FISHIN TO .5*LAST APPROXIMATION(X).
      IF(Y.LT.0.)  FISHIN=.5*X
C
C ----IF THE ABSOLUTE VALUE OF THE DIFFERENCE IS LESS THAN .5E-6, RETURN
      IF(ABS(X/FISHIN-1.).LT.(.5E-6))  GO TO 7
C
C ----IF THE RELATIVE VALUE OF THE DIFFERENCE IS LESS THAN .5E-6, RETURN
      IF(ABS(X-FISHIN).LT.(.5E-6)) GO TO 7
      IC=IC+1
      IF(IC.GT.100)  RETURN
C
C ----SET THE APPROXIMATION EQUAL TO FISHNN AND CONTINUE TO ITERATE
      X=FISHIN
      GO TO (5,55),ISIZ
    7 RETURN
      END


      FUNCTION PHINV( PR )
C
C ----PURPOSE:  TO COMPUTE THE INVERSE OF THE STANDARD NORMAL CUMULATIVE
C     DISTRIBUTION FUNCTION (MEAN 0, VARIANCE 1)
C
C ----CALLING SEQUENCE:  VARIABLE=PHINV(P)
C     P     THE PROBABILITY FOR WHICH THE INVERSE
C           IS TO BE CALCULATED (INPUT)
C
C -----RESTRICTIONS AND COMMENTS:  THE SUBPROGRAM ERFF(PHI)
C     IS REQUIRED
C
C ----TEST P
      P = PR
      IF (P.GT.0.0. AND.P.LT.1.0) GO TO 5
      IF (P.EQ.1.0) PHINV=1.0E+38
      IF(P.EQ.0.0) PHINV=-1.0E+38
C
C ----ERROR IF P NOT BETWEEN ZERO AND ONE
      IF(P.LT.0.0.OR.P.GT.1.0) PHINV=1.0E20
      RETURN
C
C ----SET FLAG:  X POSITIVE
C     WHEN K=1, NEGATIVE
C     WHEN K=2.
    5 K = 1
      IF(P .GT. 0.5)GO TO 47
C
C ----COMPUTE FIRST APPROXIMATION, EQUATION 26.2.23, PAGE 933,
C     ABRAMOWITZ AND STEGUN
    8 T3=SQRT(-2.0*ALOG(P))
      T4P=2.515517+.802853*T3+.010328*T3*T3
      T5P=1.0+1.432788*T3+.189269*T3*T3+.001308*T3*T3*T3
      XT=T3-T4P/T5P
      XT=-XT
C
C ----NEWTON RHAPSON
C     ITERATION
   13 DO 53 I=1,100
      PHP = EXP(-0.5*XT*XT)
      PT = PHI (XT)
      IF(ABS(P-PT) .LT. P*4.0E-8)GO TO 99
      Z = (P-PT)*2.50662827 / PHP
      XT = XT + Z
   53 CONTINUE
      GO TO 99
   47  P = 1.0 - P
      K = 2
      GO TO 8
C
C ----ASSIGN SIGN
C     + OR - TO X.
   99 GO TO (26,27),K
   26 PHINV = XT
      RETURN
   27 PHINV = -XT
      P = 1.0 - P
      RETURN
      END


      SUBROUTINE SEIGEN(NM,N,A,D,E,Z,IERR)
C
      INTEGER I,J,K,L,M,N,II,JP1,NM,MML
      REAL*8 A(NM,N),D(N),E(N),Z(NM,N)
      REAL*8 B,C,F,G,H,P,R,S,HH,SCALE,MACHEP
      REAL*8 DSQRT,DABS,DSIGN
C
C     THIS SUBROUTINE IS A MERGER OF THE EISPACK SUBROUTINES TRED2 AND
C     TQL2, WHICH ARE THEMSELVES TRANSLATED FROM THE ALGOL PROCEDURES.
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C     MATRIX EIGENSYSTEM ROUTINES - EISPACK GUIDE, 30(1974).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS OF A
C     FULL SYMMETRIC REAL MATRIX.
C
C     THE WORK IS DONE IN TWO PARTS.  THE FIRST (CORRESPONDING TO
C     TRED2) REDUCES THE FULL SYMMETRIC MATRIX TO A SYMMETRIC
C     TRIDIAGONAL MATRIX USING AND ACCUMULATING ORTHOGONAL SIMILARITY
C     TRANSFORMATIONS.  THE SECOND (TQL2) FINDS THE EIGENVALUES AND
C     EIGENVECTORS OF THE SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.
C
C     ON INPUT:
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT;
C
C        N IS THE ORDER OF THE MATRIX;
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C        E IS A REAL VECTOR FOR INTERMEDIATE STORAGE.
C
C     ON OUTPUT:
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1;
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES;
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
C
         DO 100 J = 1, I
            Z(I,J) = A(I,J)
  100 CONTINUE
C
      IF (N .EQ. 1) GO TO 320
C     :::::::::: FOR I=N STEP -1 UNTIL 2 DO -- ::::::::::
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0D0
         SCALE = 0.0D0
         IF (L .LT. 2) GO TO 130
C     :::::::::: SCALE ROW (ALGOL TOL THEN NOT NEEDED) ::::::::::
         DO 120 K = 1, L
  120    SCALE = SCALE + DABS(Z(I,K))
C
         IF (SCALE .NE. 0.0D0) GO TO 140
  130    E(I) = Z(I,L)
         GO TO 290
C
  140    DO 150 K = 1, L
            Z(I,K) = Z(I,K) / SCALE
            H = H + Z(I,K) * Z(I,K)
  150    CONTINUE
C
         F = Z(I,L)
         G = -DSIGN(DSQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         Z(I,L) = F - G
         F = 0.0D0
C
         DO 240 J = 1, L
            Z(J,I) = Z(I,J) / (SCALE * H)
            G = 0.0D0
C     :::::::::: FORM ELEMENT OF A*U ::::::::::
            DO 180 K = 1, J
  180       G = G + Z(J,K) * Z(I,K)
C
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
  200       G = G + Z(K,J) * Z(I,K)
C     :::::::::: FORM ELEMENT OF P ::::::::::
  220       E(J) = G / H
            F = F + E(J) * Z(I,J)
  240    CONTINUE
C
         HH = F / (H + H)
C     :::::::::: FORM REDUCED A ::::::::::
         DO 260 J = 1, L
            F = Z(I,J)
            G = E(J) - HH * F
            E(J) = G
C
            DO 260 K = 1, J
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)
  260    CONTINUE
C
         DO 280 K = 1, L
  280    Z(I,K) = SCALE * Z(I,K)
C
  290    D(I) = H
  300 CONTINUE
C
  320 D(1) = 0.0D0
      E(1) = 0.0D0
C     :::::::::: ACCUMULATION OF TRANSFORMATION MATRICES ::::::::::
      DO 500 I = 1, N
         L = I - 1
         IF (D(I) .EQ. 0.0D0) GO TO 380
C
         DO 360 J = 1, L
            G = 0.0D0
C
            DO 340 K = 1, L
  340       G = G + Z(I,K) * Z(K,J)
C
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * Z(K,I)
  360    CONTINUE
C
  380    D(I) = Z(I,I)
         Z(I,I) = 1.0D0
         IF (L .LT. 1) GO TO 500
C
         DO 400 J = 1, L
            Z(I,J) = 0.0D0
            Z(J,I) = 0.0D0
  400    CONTINUE
C
  500 CONTINUE
C
C     :::::::::: LAST CARD OF TRED2 ::::::::::
C
C     ------------------------------------------------------------------
C
C     :::::::::: MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C                IT IS THE SMALLEST POSITIVE NUMBER SUCH THAT 1.+MACHEP=
C                MACHEP = 16.0D0**(-13) FOR LONG FORM ARITHMETIC
C                ON S360 ::::::::::
C                MACHEP = 2.0D0**(-55) FOR LONG FORM ARITHMETIC ON PDP
      MACHEP=2.775557564D-17
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO1100 I = 2, N
 1100 E(I-1) = E(I)
C
      F = 0.0D0
      B = 0.0D0
      E(N) = 0.0D0
C
      DO1240 L = 1, N
         J = 0
         H = MACHEP * (DABS(D(L)) + DABS(E(L)))
         IF (B .LT. H) B = H
C     :::::::::: LOOK FOR SMALL SUB-DIAGONAL ELEMENT ::::::::::
         DO1110 M = L, N
            IF (DABS(E(M)) .LE. B) GO TO1120
C     :::::::::: E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ::::::::::
 1110    CONTINUE
C
 1120    IF (M .EQ. L) GO TO1220
 1130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     :::::::::: FORM SHIFT ::::::::::
         P = (D(L+1) - D(L)) / (2.0D0 * E(L))
         R = DSQRT(P*P+1.0D0)
         H = D(L) - E(L) / (P + DSIGN(R,P))
C
         DO1140 I = L, N
 1140    D(I) = D(I) - H
C
         F = F + H
C     :::::::::: QL TRANSFORMATION ::::::::::
         P = D(M)
         C = 1.0D0
         S = 0.0D0
         MML = M - L
C     :::::::::: FOR I=M-1 STEP -1 UNTIL L DO -- ::::::::::
         DO1200 II = 1, MML
            I = M - II
            G = C * E(I)
            H = C * P
            IF (DABS(P) .LT. DABS(E(I))) GO TO1150
            C = E(I) / P
            R = DSQRT(C*C+1.0D0)
            E(I+1) = S * P * R
            S = C / R
            C = 1.0D0 / R
            GO TO1160
 1150       C = P / E(I)
            R = DSQRT(C*C+1.0D0)
            E(I+1) = S * E(I) * R
            S = 1.0D0 / R
            C = C * S
 1160       P = C * D(I) - S * G
            D(I+1) = H + S * (C * G + S * D(I))
C     :::::::::: FORM VECTOR ::::::::::
            DO1180 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * H
               Z(K,I) = C * Z(K,I) - S * H
 1180       CONTINUE
C
 1200    CONTINUE
C
         E(L) = S * P
         D(L) = C * P
         IF (DABS(E(L)) .GT. B) GO TO1130
 1220    D(L) = D(L) + F
 1240 CONTINUE
C     :::::::::: ORDER EIGENVALUES AND EIGENVECTORS ::::::::::
      DO1300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO1260 J = II, N
            IF (D(J) .GE. P) GO TO1260
            K = J
            P = D(J)
 1260    CONTINUE
C
         IF (K .EQ. I) GO TO1300
         D(K) = D(I)
         D(I) = P
C
         DO1280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
 1280    CONTINUE
C
 1300 CONTINUE
C
      GO TO 1001
C     :::::::::: SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ::::::::::
 1000 IERR = L
 1001 RETURN
C     :::::::::: LAST CARD OF TQL2 ::::::::::
C
C     :::::::::: LAST CARD OF SEIGEN ::::::::::
      END



      SUBROUTINE MINV  (A,N,E,K,X,J2,I2)
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



      SUBROUTINE GINV  (A,MR,MC,NR,NC,KZ,U,AFLAG,ATEMP)
C-----------------------------------------------------------------------
C
C          THIS ROUTINE COMPUTES THE GENERALIZED INVERSE OF A MATRIX
C          AND STORES THE TRANSPOSE OF THE INVERSE IN A
C  MR      IS THE MAXIMUM ROW DIMENSION OF A
C  MC      IS THE MAXIMUM COLUMN DIMENSION OF A
C  NR      IS THE NUMBER OF ROWS IN A
C  NC      IS THE NUMBER OF COLUMNS IN A
C  KZ      OUTPUT, KZ = 0 IMPLIES THAT A IS SINGULAR
C                  KZ = 1 IMPLIES THAT A IS NONSINGULAR
C  U       TEMPORARY WORKING STORAGE, DIMENSIONED MC BY MC
C  AFLAG   TEMPORARY WORKING STORAGE, DIMENSIONED MC
C  ATEMP   TEMPORARY WORKING STORAGE, DIMENSIONED MC
C
C-----------------------------------------------------------------------
      DIMENSION A(MR,MC),U(MC,MC),AFLAG(MC),ATEMP(MC)
      DOUBLE PRECISION A,U,AFLAG,ATEMP,FAC,DOT1,DOT2,TOL
     *,DOT,DSQRT
      KZ = 1
      TOL=.1D-21
      DO 10 I=1,NC
      DO 5 J=1,NC
    5 U(I,J)=0.0D0
   10 U(I,I)=1.0D0
      DO 12 L=1,NC
      FAC=DOT(MR,MC,NR,A,L,L)
      K=L
      IF(FAC.GT.TOL) GO TO 11
      DO 13 J=1,NR
   13 A(J,L)=0.0D0
      KZ=0
   12 AFLAG(L)=0.0D0
      GO TO 9997
   11 FAC = 1.0D0/DSQRT(FAC)
      L=K
      AFLAG(L)=1.0D0
      DO 15 I=1,NR
   15 A(I,L)=A(I,L)*FAC
      DO 20 I=1,NC
   20 U(I,L)=U(I,L)*FAC
C
      L1=L+1
      IF(L1.LE.NC) GO TO 21
      DO 22 I=1,NR
   22 A(I,L)=A(I,L)*FAC
      GO TO 9997
   21 DO 100 J=2,NC
      DOT1=DOT(MR,MC,NR,A,J,J)
      JM1=J-1
      DO 50 L=1,2
      DO 30 K=1,JM1
   30 ATEMP(K)=DOT(MR,MC,NR,A,J,K)
      DO 45 K=1,JM1
      DO 35 I=1,NR
   35 A(I,J)=A(I,J)-ATEMP(K)*A(I,K)*AFLAG(K)
      DO 40 I=1,NC
   40 U(I,J)=U(I,J)-ATEMP(K)*U(I,K)
   45 CONTINUE
   50 CONTINUE
      DOT2=DOT(MR,MC,NR,A,J,J)
      IF((DOT2/DOT1)-TOL) 55,55,70
   55 DO 60 I=1,JM1
      ATEMP(I)=0.00D0
      DO 60 K=1,I
   60 ATEMP(I)=ATEMP(I)+U(K,I)*U(K,J)
      DO 65 I=1,NR
      A(I,J)=0.0D0
      DO 65 K=1,JM1
   65 A(I,J)=A(I,J)-A(I,K)*ATEMP(K)*AFLAG(K)
      AFLAG(J)=0.0D0
      KZ = 0
C     FAC=DOT(MC,NC,U,J,J)
      FAC=DOT(MC,MC,NC,U,J,J)
      IF(FAC.GT.TOL) GO TO 66
      DO 67 I=1,NC
   67 U(I,J)=0.0D0
      GO TO 100
   66 FAC = 1.0D0/DSQRT(FAC)
      GO TO 75
   70 IF(DOT2.GT.TOL) GO TO 71
      KZ=0
      AFLAG(J)=0.0D0
      DO 72 I=1,NR
   72 A(I,J)=0.0D0
      GO TO 100
   71 AFLAG(J)=1.0D0
      FAC = 1.0D0/DSQRT(DOT2)
   75 DO 80 I=1,NR
   80 A(I,J)=A(I,J)*FAC
      DO 85 I=1,NC
   85 U(I,J)=U(I,J)*FAC
  100 CONTINUE
      DO 130 J=1,NC
      DO 130 I=1,NR
      FAC = 0.0D0
      DO 120 K=J,NC
  120 FAC=FAC+A(I,K)*U(J,K)
  130 A(I,J)=FAC
 9997 RETURN
      END
      REAL*8 FUNCTION DOT(MR,MC,NR,A,JC,KC)
      REAL*8 A(MR,MC)
C
C     COMPUTES THE INNER PRODUCT OF COLUMNS JC AND KC
C     OF MATRIX A.
      DOT = 0.0D0
      DO 5 I=1,NR
    5 DOT=DOT+A(I,JC)*A(I,KC)
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ibisstat.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ibisstat

   To Create the build file give the command:

		$ vimake ibisstat 			(VMS)
   or
		% vimake ibisstat          		(Unix)


************************************************************************/


#define PROGRAM	ibisstat
#define R2LIB

#define MODULE_LIST ibisstat.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ibisstat.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,72)
PARM OUT      TYPE=(STRING,72)  COUNT=0:1 DEFAULT=--
PARM COLS     TYPE=INTEGER COUNT=1:40 
PARM COLNAMES TYPE=(STRING,8) COUNT=0:40 DEFAULT=--
PARM OPTION   TYPE=KEYWORD COUNT=1  DEFAULT=SUMMARY  VALID=(SUMMARY,HISTOGRA,+
 		SCATTER,CORRELAT,BEHRENS,REGRESS,ANOVA,FACTOR,DENSITY) 
PARM NOPRINT  TYPE=KEYWORD COUNT=0:1 VALID=(--,NOPRINT) DEFAULT=--
PARM BINS     TYPE=INTEGER VALID=2:1000 DEFAULT=20
PARM SCATSIZE TYPE=INTEGER COUNT=2 VALID=5:75 DEFAULT=(60,20)
PARM DEPCOL   TYPE=INTEGER VALID=1:40 DEFAULT=2
PARM DEPNAME  TYPE=(STRING,8) DEFAULT="DEP VAR"
PARM ERRCOL   TYPE=INTEGER VALID=1:40 DEFAULT=3
PARM REPLICS  TYPE=INTEGER COUNT=2:40 DEFAULT=(0,0)
PARM BCOLS    TYPE=INTEGER COUNT=1:40 DEFAULT=2
PARM NUMPOINT TYPE=INTEGER COUNT=2 DEFAULT=(0,0)
PARM DISTRIB  TYPE=KEYWORD COUNT=1  DEFAULT=NORMAL  +
         VALID=(NORMAL, CHISQ, STUDENTT, FISHERF)
PARM NDF      TYPE=INTEGER COUNT=0:80 DEFAULT=(4,20)
END-PROC
.TITLE
"ibisstat"
.HELP
PURPOSE

    "ibisstat" performs statistical analyses on IBIS tabular files.
    Output is to the terminal and/or another tabular file.
    Currently, nine types of statistical analysis methods are available:

     'SUMMARY     Statistical summary (median, mean, etc.)
     'HIST        Histogram 
     'SCATTER     Scatter plot 
     'CORR        Correlation
     'BEHRENS     Behrens-Fisher test for different means (multivariate)
     'REGRESS     Multiple linear regression
     'ANOVA       One-way analysis of variance
     'FACTOR      Principal components factor analysis
     'DENSITY     Selected probability densities

.PAGE
EXECUTION

    ibisstat  INP=TABLE.INT OUT=STAT.INT  'method COLS=( , )  COLNAMES=("","")

    ibisstat TABLE.INT   COLS=(...)  'method   (parameters)



    Each option requires that the COLS parameter be specified.  This
parameter specifies the columns in the tabular file which contain the 
data to be operated on.  COLNAMES is an optional parameter that is used
to apply an eight character label to each column for use in the printouts.
The 'NOPRINT keyword can be used to suppress all printout for when the
output should only go to a file.  An output IBIS tabular file may be
optionally specified, however, not all of the options use an output file.
.PAGE

DESCRIPTION OF THE STATISTICAL ANALYSIS METHODS


'SUMMARY            STATISTICAL SUMMARY

        The statistical summary option calculates some simple statistical 
    values independently for each column specified.  A table is printed 
    showing the median, mean, standard deviation, minimum, and maximum 
    for each column.  The standard deviation is calculated with N-1 
    weighting.  The optional output tabular file has 5 rows (median, 
    mean, std, min, max) and as many columns as input.


.PAGE

'HIST                 HISTOGRAM 

        The histogram option produces a terminal plot of the histogram
    of each column specified.  The limits on the histogram are the
    minimum and maximum data values in the column, and the number
    of bins is given by the BINS parameter (default is 20).  For
    each bin the center data value and the number of data points
    in the bin are printed and then the appropriate number of *'s
    are printed.  If more than one column is specified then the
    histograms are done in order.
        The optional output file consists of two columns for every
    input column and a row for each histogram bin.  The first column
    in each pair has the center data value of the bins and the second
    column has the histogram count.

.PAGE
'SCATTER              SCATTER PLOT

    The scatter plot option produces terminal plots of two variables.
    The COLS parameter specifies a pair of columns for each plot: the first 
    column in the pair is the X variable and the second is the Y variable.  
    The size of the plot (in characters on the terminal) is specified with 
    the SCATSIZE parameter.  The plot prints *'s where there is one data point, 
    the digits "2" through "8" for two to eight data points, and "9" for 
    nine or more points.
    The optional output file is not used.


.PAGE
'CORR                 CORRELATION

        The correlation option calculates the correlation, covariance,
    and the significance level for each pair of variables in a 
    multivariate dataset.  Each column is a different variable, and
    each row is a data point.  If there are M variables (columns) then
    there is an M by M matrix of correlation values.  The matrix is
    symmetric, and only the lower triangular portion is printed.  For
    each matrix entry in the printout the first number is the correlation,
    the second number is the covariance, and the third one is the 
    probability or significance level.  On the diagonal of the matrix
    the correlations are 1 (variables are perfectly correlated with
    themselves) and the covariances are the variance of each variable.
    The significance level is the probability that the level of correlation
    has come about by chance.  This test assumes that the data is normally 
    distributed.  The test is a one-tailed test of statistical significance.
    The optional output file has M rows and M columns and contains the
    covariance matrix.
.PAGE
'BEHRENS           BEHRENS-FISHER TEST 

        The Behrens option is used to test whether two multivariate samples
    have the same mean vector.  It is a multivariate generalization of
    the Students T test, and does not assume that the two distributions
    have the same size and shape.  The columns containing the first
    sample are specified with the COLS parameter, while the columns
    with the second sample are specified with the BCOLS parameter.
    The numbers of data points in the two samples are specified with the
    NUMPOINTS parameter, and thus can be different.  Hotelling's T squared
    statistic and the attained level of significance are printed out.
    The samples are assumed to come from two multivariate normal 
    distributions with possibly different covariance matrices.
    The optional output file is not used.
.PAGE
'REGRESS            MULTIPLE REGRESSION

        The regression option performs a multiple linear regression and
    calculates some statistics.  The COLS parameter specifies the
    columns containing the independent variables and the DEPCOL parameter 
    specifies the dependent variable column.  The optional ERRCOL parameter 
    specifies the column that contains the estimated errors (uncertainties) 
    in the dependent data.  
        The regression constant and coefficient for each variable are printed 
    out in the COEFFICIENT column.  The one-sigma confidence interval for 
    each regression coefficient is printed out in the ERROR column.  The 
    number after the 'ERROR:' is the probability of the coefficient being 
    within plus or minus the error.  If a higher probability confidence 
    interval is desired just multiply the interval by the appropriate 
    number from the T statistic for N-M-1 degrees of freedom, e.g. 2.660 
    for 99% probability for df=60  (N is number of data points and M is 
    number of variables).  This confidence interval is calculating from 
    the scatter in the data and is not based in any way on the input 
    estimated uncertainties.  
        The R squared statistic is the fraction of the total variance in
    the dependent data that is explained by the regression.  The standard
    error of the estimate is the RMS average of the residuals (the misfit
    between the predicted and actual dependent data).  The F ratio test 
    determines the significance of the overall regression, i.e. the
    probability that not all of the coefficients are actually zero. A high
    level of significance does not mean that all of the variables are
    significant just that at least one is.  The Durbin-Watson statistic
    indicates how sequentially correlated the residuals are;  uncorrelated
    residuals have a statistic around 2, while correlated residual will
    have a statistic less than 1.
        If the ERRCOL is specified then the goodness of fit chi squared will
    be calculated.  The chi squared statistic is the sum of the squares
    of each residual divided by each uncertainty.  If the estimated
    uncertainties are correct and the regression fits the data then the
    statistic will be about 1.  The associated probability printed is the 
    probability of the statistic being that far from 1 just due to random
    chance.
        The optional output file contains two columns:  the first contains
    the M+1 regression coefficients (including the constant), and the
    second contains the residuals.
        NOTE: The multiple regression technique assumes that the residuals 
    are uncorrelated and come from a normal distribution.

.PAGE
'ANOVA                ANALYSIS OF VARIANCE

        The ANOVA option performs one-way analysis of variance on a
    table of data.  Each column represents a separate group that has
    been treated differently.  Analysis of variance is a statistical
    test that determines whether any of the groups has a significantly
    different mean value from the rest (e.g. whether the "treatment" has
    had any significant effect).  The groups need not have the same size:
    the number of replications for each group is specified with the
    REPLICS parameter.  The analysis of variance technique assumes that
    the data points in each group are sampled from a normal distribution
    with the same variance.
        In the printout the grand mean is mean of all of the groups 
    put together, and the estimate of effects is the difference between
    the group means and the grand mean.  The F test determines the 
    significance level of the hypothesis that at least one group
    has a non zero estimate of effect.
        The optional output file is not used.
.PAGE
'FACTOR              FACTOR ANALYSIS

        The factor option performs principal components factor analysis
    on a multivariate dataset.  This involves calculating the covariance
    matrix of the data, and then finding the eigenvalues and eigenvectors
    of the matrix.  The eigenvectors are the coefficients for linearly
    translating the original variables into a new set of variable or factors.
    This new set of variables has the property that the data expressed in 
    terms of the factors has no cross-correlation between different variables.
    The eigenvalues are the variances of the factors.   The table gives
    the coefficients for calculating the factors in terms of the original
    variables.
        Factor analysis can be used to find a new set of variables that
    is smaller than the original set but that explain most of the variance
    in the data.  The first few factors may explain most of the variance
    (according to the eigenvalues) and the rest can be ignored.  In
    geometrical terms the data points are a cloud with a certain size, shape,
    and direction.  Principal component analysis finds how to rotate the
    coordinates so that the principal axis of the cloud lie along the
    coordinate axis.  The square root of the variances is roughly the size
    of the cloud in the principal directions.  The eigenvectors, which are
    ortho-normal, are the unit vectors in the rotated space.
        The optional output file contains the original data translated to
    the new set of variables.  The correlation option of IBISSTAT will
    verify that the transformed data has no cross-correlation.



.PAGE
'DENSITY           PROBABILITY DENSITIES

    The density option calculates the cumulative probability of given
    values for four distributions: normal, chi squared, Student's T, and
    Fisher's F.  The input IBIS tabular file contains the values of the
    statistic.  The output IBIS file contains two columns for each input
    column: first, the values of statistic from the input file, and second
    the corresponding cumulative probabilities.  The parameter DISTRIB 
    specifies the particular probability distribution (or density).  
    The chi squared and Student's T distributions required one degrees 
    of freedom parameter, and the Fisher's F distribution requires two.  
    The parameter NDF specifies the number of degrees of freedom to use 
    for each column.  In this way a table can be generated with different 
    columns for each number of df's desired.
.PAGE

EXAMPLES

To perform a cubic fit to some data:

   Generate some fake data:
    ibis-gen  DATA.INT   NC=4 NR=50
    mf        DATA.INT   FUNCTION=("C1=INDEX/25", +
				   "C4=11.5 +5.0*C1 -2.3*C1*C1 +0.3*(C1**3)")
   Assume X in column 1, and Y in column 4.
    mf         DATA.INT  FUNC=("C2=C1*C1", "C3=C1*C1*C1")
    ibisstat   DATA.INT  'REGRESS  COLS=(1,2,3) DEPCOL=4  +
				COLNAMES=("X", "X**2", "X**3")


To perform principal components analysis on an MSS format image:

    mssibis   DATA.MSS  DATA.INT  MSS=4  NL=500 NS=400 INC=5
    ibisstat  DATA.INT  FACT.INT  COLS=(1,2,3,4)
    mssibis   FACT.INT  FACT.MSS  'TOMSS  NL=100 NS=80

.PAGE
RESTRICTIONS

    The maximum column length is 100,000.
    The maximum amount of data (columns times column length) is 250,000.
    The maximum number of input columns is 40.


WRITTEN BY:            K. F. Evans	October 1986

COGNIZANT PROGRAMMER:  K. F. Evans

REVISION:  
           2-95 - Meredith Cox (CRI) - Made portable for UNIX

.LEVEL1
.VARIABLE INP
Input IBIS tabular file.
.VARIABLE OUT      
An optional output file.
Not used by all methods.
.VARIABLE COLS     
The columns on which to perform
the statistical analysis.
.VARIABLE COLNAMES 
An optional eight character
heading for each column.
.VARIABLE OPTION   
Keyword to select the
analysis method.
(SUMMARY,HIST,SCATTER,CORR,BEHRENS,
REGRESS,ANOVA,FACTOR,DENSITY) 
.VARIABLE NOPRINT
Keyword to suppress printout.
.VARIABLE BINS     
The number of bins.
Only used for HISTOGRAM.
.VARIABLE SCATSIZE
The size of the plot (x,y).
Only used for SCATTER.
.VARIABLE DEPCOL
Dependent variable column.
Only used for REGRESSION.
.VARIABLE DEPNAME
An optional eight character
heading for the dependent
variable.
Only used for REGRESSION.
.VARIABLE ERRCOL   
The column containing the
estimated errors of the
dependent data. (optional).
Only used for REGRESSION.
.VARIABLE REPLICS  
The number of replications for
each cell.  Only used for ANOVA.
.VARIABLE BCOLS
The columns for the
"B" multivariate sample. 
Only used in BEHRENS.
.VARIABLE NUMPOINT 
The number of data points
for the A and B samples.
Only used in BEHRENS.
.VARIABLE DISTRIB
Keyword for the probability
distribution.
(NORMAL,CHISQ,STUDENTT,FISHERF)
Only used in DENSITY.
.VARIABLE NDF
The number of degrees of freedom
for each column.  One per column
for CHISQ and STUDENTT, and two
per column for FISHERF.
Only used in DENSITY.

.LEVEL2
.VARIABLE INP
Input IBIS tabular file.
.VARIABLE OUT      
An optional output IBIS tabular file.
Not used by all methods.
.VARIABLE COLS     
The columns on which to perform the statistical analysis.
.VARIABLE COLNAMES 
An optional eight character heading for each column.
.VARIABLE OPTION   
Keyword to select the analysis method.  See the help section for a full
description of each method.
(SUMMARY, HIST, SCATTER, CORR, BEHRENS, REGRESS, ANOVA, FACTOR, DENSITY) 
.VARIABLE NOPRINT
Keyword to suppress printout.
.VARIABLE BINS     
The number of bins in the histogram.
Only used for HISTOGRAM.
.VARIABLE SCATSIZE
The size of the scatter plot in characters on the terminal  (x,y).
Only used for SCATTER.
.VARIABLE DEPCOL
Dependent variable column.
Only used for REGRESSION.
.VARIABLE DEPNAME
An optional eight character heading for the dependent variable.
Only used for REGRESSION.
.VARIABLE ERRCOL   
The column containing the estimated errors of the dependent data. (optional).
If specified the chi squared statistic is calculated.
Only used for REGRESSION.
.VARIABLE REPLICS  
The number of replications for each cell (column).
Only used for ANOVA.
.VARIABLE BCOLS
The columns for the "B" multivariate sample. 
Only used in BEHRENS.
.VARIABLE NUMPOINT 
The number of data points for the A and B samples.
Only used in BEHRENS.
.VARIABLE DISTRIB
Keyword for the probability distribution.
(NORMAL, CHISQ, STUDENTT, FISHERF)
Only used in DENSITY.
.VARIABLE NDF
The number of degrees of freedom for each column.  One per column
for CHISQ and STUDENTT, and two per column for FISHERF.
Only used in DENSITY.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstibisst.pdf
PROCEDURE
  refgbl $echo
  refgbl $autousage
  parm nrows integer def=100
  parm mean real def=0.0
  parm sdev real def=1.0
  parm seed real def=9.0
BODY
  let _onfail="continue"
  let $autousage="none"
!    gausnois  N     NL=8 NS=128 FORMAT=REAL MEAN=0 SIGMA=1 SEED=7382382
!    pcopin    N  T  LCOL=100 NCOL=11 COL=(6,7,8,9,10,11)

!
!    The following use of "ibis-gen" and "mf" generate an IBIS file
!    with a uniform-distribution in columns #1 and #2, with a
!    gaussian distribution in columns #6,#7,#8,#9,#10,#11.
!
!    The generation of an "IBIS-1" type file facilitates the testing
!    and comparison of the unported (which uses on IBIS-1) with the
!    ported versions of "ibisstat".  

    ibis-gen  T  nc=11 nr=&nrows 'ibis-1

    mf  T   FUNCTION=("c1=(sin(3.7*index**2)+1)/2",                   +
                      "c2=(sin(5.3*(index+&seed)**2)+1)/2",           +
                      "c6=(sqrt(-2*alog(c1))*sin(2*(3.1415926)*c2))", +      
                      "c6=&mean + &sdev*c6")
    let seed = 8.0

    mf  T   FUNCTION=("c2=(sin(5.3*(index+&seed)**2)+1)/2",           +
                      "c7=(sqrt(-2*alog(c1))*sin(2*(3.1415926)*c2))", +      
                      "c7=&mean + &sdev*c7")

    let seed = 7.0

    mf  T   FUNCTION=("c2=(sin(5.3*(index+&seed)**2)+1)/2",           +
                      "c8=(sqrt(-2*alog(c1))*sin(2*(3.1415926)*c2))", +      
                      "c8=&mean + &sdev*c8")

    let seed = 6.0

    mf  T   FUNCTION=("c2=(sin(5.3*(index+&seed)**2)+1)/2",           +
                      "c9=(sqrt(-2*alog(c1))*sin(2*(3.1415926)*c2))", +      
                      "c9=&mean + &sdev*c9")

    let seed = 5.0

    mf  T   FUNCTION=("c2=(sin(5.3*(index+&seed)**2)+1)/2",           +
                      "c10=(sqrt(-2*alog(c1))*sin(2*(3.1415926)*c2))", +      
                      "c10=&mean + &sdev*c10")

    let seed = 4.0

    mf  T   FUNCTION=("c2=(sin(5.3*(index+&seed)**2)+1)/2",           +
                      "c11=(sqrt(-2*alog(c1))*sin(2*(3.1415926)*c2))", +      
                      "c11=&mean + &sdev*c11")
 
  let $echo="yes"
 
    mf        T     FUNCTION=("C1=(INDEX-50)/25", "C2=C1*C1", "C3=C1*C2", +
		      "C4=11.5+5.0*C1-2.3*C2+0.3*C3+0.1*C6", "C5=0.1")

    ibisstat  T  O  'REGRESS COLS=(1,2,3) DEPCOL=4 ERRCOL=5   +
			COLNAME=("X","X SQUARE","X CUBED") DEPNAME="Y-VALUE"
    ibisstat  O     'HIST COLS=2 COLNAME="RESID"  BINS=16


    mf        T     FUNCTION=( "C1=4.1*C6-0.5*C7+1.2*C8",  +
			"C2=0.3*C1-7.4*C2+1.7*C8", "C3=-0.5*C1+0.9*C2+6.2*C3")

    ibisstat  T     'SUMMARY  COLS=(1,2,3,6,7,8)
    ibisstat  T     'SCATTER  COLS=(1,2, 6,7)
    ibisstat  T  O  'CORRE    COLS=(1,2,3)  'NOPRINT
!    qrep      O     'LABEL  
    ibis-list  O    
    ibisstat  T  O  'FACTOR   COLS=(1,2,3)
    ibisstat  O     'CORRE    COLS=(1,2,3)
 

    mf        T      FUNCTION=( "C7=C7-0.2", "C8=C8+0.3", "C9=C9+0.45", +
				"C11=2.0*C11+C10")
    ibisstat  T      'ANOVA    COLS=(6,7,8,9,10)  REPLICS=(70,68,85,46,78)
    ibisstat  T      'BEHRENS  COLS=(6,7,8)  BCOLS=(9,10,11) NUMPOINT=(90,60)

    mf        T      FUNCTION="C1=(INDEX-50)/20"
    ibisstat  T  O   'DENSITY  'STUDENT  COLS=(1,1)  NDF=(5,20)
!    qrep      O      'LABEL  SROW=75 NROW=20  FORMAT="(4F12.5)"
    ibis-list O  sr=75 nr=20
    LET $ECHO = "NO"
END-PROC
$ Return
$!#############################################################################
