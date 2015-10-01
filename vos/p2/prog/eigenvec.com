$!****************************************************************************
$!
$! Build proc for MIPL module eigenvec
$! VPACK Version 1.8, Thursday, October 24, 1996, 17:12:06
$!
$! Execute by entering:		$ @eigenvec
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
$ write sys$output "*** module eigenvec ***"
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
$ write sys$output "Invalid argument given to eigenvec.com file -- ", primary
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
$   if F$SEARCH("eigenvec.imake") .nes. ""
$   then
$      vimake eigenvec
$      purge eigenvec.bld
$   else
$      if F$SEARCH("eigenvec.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake eigenvec
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @eigenvec.bld "STD"
$   else
$      @eigenvec.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create eigenvec.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack eigenvec.com -
	-s eigenvec.f -
	-i eigenvec.imake -
	-p eigenvec.pdf -
	-t tsteigen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create eigenvec.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     19 OCT 79   ...JDA...    INITIAL RELEASE
C     5 OCT 81    ...REA...    FIX PROBLEM WITH MSS,12 & ENLARGE BUFFERS
C     1 FEB 82    ...REA...    CHANGE PRINTOUT FORMAT
C     20 AUG 82   ...REA...     MAKE COMPATIBLE WITH AP VERSION OF XFORM
C     5 JAN 83    ...REA...     ADD SAVE AND TSAVE KEYWORDS
C     10 APR 83   ...REA...   2.1 FOR CORR, MODIFY EV'S TO NORMALIZE VAR
C     1 SEPT 85   ...REA...   3.0 CONVERT TO VICAR2
C     5 NOV 85    ...REA...   3.1 ADD DSTRETCH OPTION
C     13 NOV 85   ...REA...   3.1 ADD DSCALE PARAMETER
C     01 JULY 94  ...CRI...    MSTP S/W CONVERSION (VICAR PORTING)
C
	IMPLICIT NONE 
        INCLUDE 'fortport'
        REAL*8 VAL1,VAL2,VALUE3,VALUE4,VALUE5
	REAL*8 COV(528),MEAN(32),EVEC(1024),COR(528)
	REAL EXCLUD,XMIN,RCOV,BUF(50000),STDEV(32),DATABUF(500)
	REAL GAINFAC(32),EVEC_HOLD(1024),DSCALE(32),X,Y,PTS1,MAX
	INTEGER BND(32),OUTPUT(32),ISUBAREA(200),INUNIT(10),J,I
        INTEGER NI,NL,NS,NV,NO,N,L,K,N1,N2,ISL,ISS,IEL,IND,INC,PTS
        INTEGER NLIN,NSIN,ICOUNT,NVMSS,IDUMMY,SINC,LINC,NDATA,NVAR
        INTEGER ISTATUS,N_AREA_PARS,NS_PER_CHAN,NCOV,NUM,PTR
        INTEGER IHOLD
 	CHARACTER*30 SAVE,TSAVE
        CHARACTER*132 UNCONFMT
        CHARACTER*132 PR
        DATA PR/' '/
        DATA DSCALE/32*1.0/
	LOGICAL XVPTST,MSS,USEFLA,CORR,DSTR
	EQUIVALENCE (STDEV,GAINFAC),(BUF,EVEC_HOLD)
C
	CALL XVMESSAGE(' ',' ')
	CALL IFMESSAGE('EIGENVEC version 01-JULY-94')
C								open inputs
	CALL XVEACTION('SA',' ')
        CALL XVPCNT('INP',NI)
	DO I=1,NI
	    CALL XVUNIT(INUNIT(I),'INP',I,ISTATUS,' ')
	    CALL CHKSTAT(ISTATUS,' XVUNIT FAILED FOR INPUT',-1)
	    CALL XVOPEN(INUNIT(I),ISTATUS,'U_FORMAT','REAL',' ')
	END DO
C							        get parameters
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C								mss, use
	CALL XVPARM('MSS',NV,ICOUNT,IND,1)
	IF (ICOUNT.EQ.0) THEN
		NV = NI
		MSS = .FALSE.
		NS_PER_CHAN = 0
	    ELSE
		MSS= .TRUE.
		NVMSS = NV
		NS_PER_CHAN = NSIN/NVMSS
	END IF
C
	CALL XVPARM('USE',BND,ICOUNT,IND,32)
	IF (ICOUNT.EQ.0) THEN
		DO I=1,NV
		    BND(I) = I
		END DO
		USEFLA = .FALSE.
	    ELSE
		NV = ICOUNT
		USEFLA = .TRUE.
	END IF
C								msso, output
	CALL XVPARM('MSSO',NO,ICOUNT,IND,1)
	IF (ICOUNT.EQ.0) CALL XVPCNT('OUT',NO)
	CALL XVPARM('OUTPUT',OUTPUT,ICOUNT,IND,32)
	IF (ICOUNT.EQ.0) THEN
		DO I=1,NV
		    OUTPUT(I) = I
		END DO
	    ELSE
		NO = ICOUNT
	END IF
C								inc, linc
	CALL XVPARM('INC',INC,IDUMMY,IND,1)
	SINC = INC
	CALL XVPARM('LINC',LINC,ICOUNT,IND,1)
	IF (ICOUNT.NE.1) LINC=INC
C								exclude, area
	CALL XVPARM('EXCLUDE',EXCLUD,ICOUNT,IND,1)
	IF (ICOUNT.NE.1) EXCLUD=-99999.9
	CALL XVPARM('AREA',ISUBAREA,N_AREA_PARS,IND,200)
	IF (N_AREA_PARS.EQ.0) THEN
	    ISUBAREA(1) = ISL
	    ISUBAREA(2) = ISS
	    ISUBAREA(3) = NL
	    N_AREA_PARS = 4
	    IF (MSS .AND. NS.EQ.NSIN) THEN
		    ISUBAREA(4) = NS/NVMSS
		ELSE
		    ISUBAREA(4) = NS
	    END IF
	END IF
C								dstretch,corr
C								dscale,
C								save,tsave
	DSTR = XVPTST('DSTRETCH')
	CORR = XVPTST('CORR')
	CALL XVPARM('DSCALE',DSCALE,IDUMMY,IND,32)
	CALL XVPARM('SAVE',SAVE,IDUMMY,IND,1)
	CALL XVPARM('TSAVE',TSAVE,IDUMMY,IND,1)
C								dscale
C								data, nvar
	CALL XVPARM('DATA',DATABUF,NDATA,IND,500)
	CALL XVPARM('NVAR',NVAR,ICOUNT,IND,1)
C
        PTS = 0
        CALL ZIA(BUF,50000)
	CALL ZIA(MEAN,64)
	CALL ZIA(COV,1056)
	IF (ICOUNT.NE.0) THEN
C						data entered as parameters
		NV = NVAR
		PTS = NDATA/NV
		DO N=1,NDATA,NV
		    L = 1
		    DO J=1,NV
			MEAN(J) = MEAN(J)+DATABUF(N+J-1)
			DO K=1,J
			    COV(L)=COV(L)+DATABUF(N+J-1)*DATABUF(N+K-1)
			    L = L+1
			END DO
		    END DO
		END DO
	    ELSE
C						     data from input datasets;
C					             get the areas to process
		CALL XVMESSAGE(' ',' ')
		CALL XVMESSAGE('AREA(S) PROCESSED',' ')
		CALL XVMESSAGE(' ',' ')
		DO I=1,N_AREA_PARS,4
		    ISL = ISUBAREA(I)
		    ISS = ISUBAREA(I+1)
		    NL = ISUBAREA(I+2)
		    NS = ISUBAREA(I+3)
                    WRITE (PR(1:11),'(I11)') ISL
                    WRITE (PR(12:16),'(I5)') ISS
                    WRITE (PR(17:21),'(I5)') NL
                    WRITE (PR(22:26),'(I5)') NS
                    CALL XVMESSAGE(PR(2:26),' ')
C							gather data
		    IEL = ISL+NL-1
		    DO J=ISL,IEL,LINC
			IF (MSS) THEN
			        CALL XVREAD(INUNIT(1),BUF,ISTATUS,
     +				     'LINE',J,'NSAMPS',NSIN,' ')
				IF (NS.NE.NS_PER_CHAN .OR. USEFLA) THEN
				    DO K=1,NV
					N1 = NS_PER_CHAN*(BND(K)-1)+ISS
					N2 = NS*(K-1) + 1
					CALL MVE(7,NS,BUF(N1),BUF(N2),
     +                                           1,1)
				    END DO
				END IF
			    ELSE
				DO K=1,NI
				    CALL XVREAD(INUNIT(K),
     +					BUF(NS*(K-1)+1),ISTATUS,
     +					'LINE',J,'SAMP',ISS,'NSAMPS',
     +                                  NS,' ')
				END DO
			END IF
C					        collect covariance stats
C
			CALL COVAR(BUF,NS,SINC,NV,MEAN,COV,EXCLUD,PTS)
		    END DO
		END DO
C						report LINC, SINC, and EXCLUDE
C
                PR(1:22) = '      LINC=      SINC='
                WRITE (PR(12:14),'(I3)') LINC
                WRITE (PR(23:25),'(I3)') SINC
		CALL XVMESSAGE(' ',' ')
                CALL XVMESSAGE(PR(2:25),' ')
		IF (EXCLUD.NE.-99999.9) THEN
                    PR(1:43) = 
     +'      PIXEL EXCLUDED IF DN=XXX IN ALL BANDS'
                    WRITE (PR(28:30),'(I3)') NINT(EXCLUD)
		    CALL XVMESSAGE(' ',' ')
                    CALL XVMESSAGE(PR(2:43),' ')
		END IF
	END IF
C
C					        compute covariance matrix
C
	PTS1 = MAX(PTS-1,1)
	L = 0
	XMIN = 1E30
	DO J=1,NV
	    DO K=1,J
		L = L+1
                VAL1 = PTS 
                VAL2 = PTS1
                VALUE3 = COV(L)
                VALUE4 = MEAN(J)
                VALUE5 = MEAN(K)
                VALUE3 = (VALUE3 - VALUE4*VALUE5/VAL1 ) / VAL2

		COV(L) = (COV(L) - MEAN(J)*MEAN(K)/PTS) / PTS1

	    END DO
	    STDEV(J) = DSQRT(COV(L))
	    IF(XMIN.GT.STDEV(J)) XMIN=STDEV(J)
	END DO
C				         compute & use the correlation matrix
	IF (CORR) THEN
	    L = 0
	    DO J=1,NV
		DO K=1,J
		    L = L+1
		    IF (STDEV(J)*STDEV(K) .NE. 0.0) THEN
			    COR(L) = COV(L)/(STDEV(J)*STDEV(K))
			ELSE
			    COR(L) = 0.0
		    END IF
		END DO
	    END DO
	    NCOV = (NV*(NV+1))/2
	    CALL MVE(8,NCOV,COR,COV,1,1)
	END IF
C
C					        		print matrix
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' ',' ')
	IF (CORR) THEN
		CALL XVMESSAGE('CORRELATION MATRIX',' ')
		NUM = 6
	    ELSE
		CALL XVMESSAGE('COVARIANCE MATRIX',' ')
		NUM = 3
	END IF
	CALL XVMESSAGE(' ',' ')
	L = 0
	DO J=1,NV
	    PTR = 0
	    DO K=1,J
		L = L+1
		RCOV = COV(L)
		PTR = PTR+12
                WRITE (UNCONFMT,'(''(F12.'',I4.4,'')'')') NUM
                WRITE (PR(PTR-11:PTR),UNCONFMT) RCOV
		IF (PTR.GE.120) THEN
                    CALL XVMESSAGE(PR(2:PTR),' ')
		    PTR = 0
		END IF
	    END DO
            IF (PTR.NE.0) CALL XVMESSAGE(PR(2:PTR),' ')
	    CALL XVMESSAGE(' ',' ')
	END DO
C
C	        	Call deigen to compute eigen-values & eigen-vectors
C       		Eigen-values are returned in the diagonal of COV
C       		Eigen-values are returned in descending order
C
	CALL DEIGEN(COV,EVEC,NV,0)
C
C			When using the correlation matrix eigenfunctions,
C     			the input data must be variance normalized.
C
	IF (CORR) THEN
	    DO I=1,NV
		K = NV*(I-1)
		DO J=1,NV
		    EVEC(K+J) = EVEC(K+J)*XMIN/STDEV(J)
		END DO
	    END DO
	END IF
C
C				        print eigen-values & eigen-vectors
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('  EIGEN',' ')
	CALL XVMESSAGE('  VALUE             EIGEN-VECTOR',' ')
	CALL XVMESSAGE(' ',' ')
	DO I=1,NV
	    L = I+(I*I-I)/2
            WRITE (PR(1:12),'(F12.3)') COV(L)
	    PTR = 12
	    K = NV*(I-1)
	    DO J=1,NV
		PTR = PTR+12
                WRITE (PR(PTR-11:PTR),'(F12.4)') EVEC(K+J)
		IF (PTR.GE.120) THEN
                    CALL XVMESSAGE(PR(2:PTR),' ')
                    PR(1:12) = ' '
		    PTR = 12
		END IF
	    END DO
	    IF (PTR.NE.12) THEN
                CALL XVMESSAGE(PR(2:PTR),' ')
		CALL XVMESSAGE(' ',' ')
	    END IF
	END DO
C
C							dstretch processing
C
	IF (DSTR) THEN
	    DO I=1,NV*NV
		EVEC_HOLD(I) = EVEC(I)
	    END DO
	    X = COV(3)
	    DO I=1,NV
		Y = COV(I+(I*I-I)/2)
		GAINFAC(I) = SQRT(X/Y)*DSCALE(I)
	    END DO
	    CALL DSTMAT(GAINFAC,EVEC_HOLD,EVEC,NV)
	END IF
C
C		     save the eigenvector matrices in parameter datasets
C
C								save
	NUM = NV*NO
	CALL XVPOPEN(ISTATUS,1,4*NUM,SAVE,'SA',IHOLD)
	K = 1
	DO I=1,NO
	    DO J=1,NV
		BUF(K) = EVEC(NV*(OUTPUT(I)-1)+J)
		K = K+1
	    END DO
	END DO
	CALL XVPOUT(ISTATUS,'MATRIX',BUF,'REAL',NUM)
	CALL XVPCLOSE(ISTATUS)
C								tsave
	CALL XVPOPEN(ISTATUS,1,4*NUM,TSAVE,'SA',IHOLD)
 	K = 1
	DO I=1,NV
	    DO J=1,NO
		BUF(K) = EVEC(NV*(OUTPUT(J)-1)+I)
		K = K+1
	    END DO
	END DO
 	CALL XVPOUT(ISTATUS,'MATRIX',BUF,'REAL',NUM)
	CALL XVPCLOSE(ISTATUS)
      RETURN
      END
C
C     ..................................................................
C
C        SUBROUTINE DEIGEN
C
C        PURPOSE
C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
C           MATRIX
C
C        USAGE
C           CALL DEIGEN(A,R,N,MV)
C
C        DESCRIPTION OF PARAMETERS
C           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
C               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
C               MATRIX A IN DESCENDING ORDER.
C           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
C               IN SAME SEQUENCE AS EIGENVALUES)
C           N - ORDER OF MATRICES A AND R
C           MV- INPUT CODE
C                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
C                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
C                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
C                       SEQUENCE)
C
C        REMARKS
C           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
C           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
C           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
C           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
C           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
C
C     ..................................................................
C
      SUBROUTINE DEIGEN(A,R,N,MV)
      IMPLICIT   NONE
      DIMENSION A(1),R(1)
      INTEGER I,IJ,IQ,IA,IND,L,M,N,MV,J,MQ,LQ,LM,LL,MM,ILQ,IMQ,IM,IL
      INTEGER ILR,IMR,JQ,K
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
      DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,
     1                 COSX2,SINCS,RANGE
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
C        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
C        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
C        BE CHANGED TO 1.0D-12.
C
C        ...............................................................
C
C        GENERATE IDENTITY MATRIX
C
    5 RANGE=1.D-12
      IF(MV-1) 10,25,10
   10 IQ=-N
      DO 20 J=1,N
      IQ=IQ+N
      DO 20 I=1,N
      IJ=IQ+I
      R(IJ)=0.D0
      IF(I-J) 20,15,20
   15 R(IJ)=1.D0
   20 CONTINUE
C
C        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
C
   25 ANORM=0.D0
      DO 35 I=1,N
      DO 35 J=I,N
      IF(I-J) 30,35,30
   30 IA=I+(J*J-J)/2
      ANORM=ANORM+A(IA)*A(IA)
   35 CONTINUE
      IF(ANORM) 165,165,40
   40 ANORM=1.414D0*DSQRT(ANORM)
      ANRMX=ANORM*RANGE/DFLOAT(N)
C
C        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
C
      IND=0
      THR=ANORM
   45 THR=THR/DFLOAT(N)
   50 L=1
   55 M=L+1
C
C        COMPUTE SIN AND COS
C
   60 MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
   62 IF(DABS(A(LM))-THR) 130,65,65
   65 IND=1
      LL=L+LQ
      MM=M+MQ
      X=0.5D0*(A(LL)-A(MM))
   68 Y=-A(LM)/DSQRT(A(LM)*A(LM)+X*X)
      IF(X) 70,75,75
   70 Y=-Y
   75 SINX=Y/DSQRT(2.D0*(1.D0+(DSQRT(1.D0-Y*Y))))
      SINX2=SINX*SINX
   78 COSX=DSQRT(1.D0-SINX2)
      COSX2=COSX*COSX
      SINCS =SINX*COSX
C
C        ROTATE L AND M COLUMNS
C
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO 125 I=1,N
      IQ=(I*I-I)/2
      IF(I-L) 80,115,80
   80 IF(I-M) 85,115,90
   85 IM=I+MQ
      GO TO 95
   90 IM=M+IQ
   95 IF(I-L) 100,105,105
  100 IL=I+LQ
      GO TO 110
  105 IL=L+IQ
  110 X=A(IL)*COSX-A(IM)*SINX
      A(IM)=A(IL)*SINX+A(IM)*COSX
      A(IL)=X
  115 IF(MV-1) 120,125,120
  120 ILR=ILQ+I
      IMR=IMQ+I
      X=R(ILR)*COSX-R(IMR)*SINX
      R(IMR)=R(ILR)*SINX+R(IMR)*COSX
      R(ILR)=X
  125 CONTINUE
      X=2.D0*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
C
C        TESTS FOR COMPLETION
C
C        TEST FOR M = LAST COLUMN
C
  130 IF(M-N) 135,140,135
  135 M=M+1
      GO TO 60
C
C        TEST FOR L = SECOND FROM LAST COLUMN
C
  140 IF(L-(N-1)) 145,150,145
  145 L=L+1
      GO TO 55
  150 IF(IND-1) 160,155,160
  155 IND=0
      GO TO 50
C
C        COMPARE THRESHOLD WITH FINAL NORM
C
  160 IF(THR-ANRMX) 165,165,45
C
C        SORT EIGENVALUES AND EIGENVECTORS
C
  165 IQ=-N
      DO 185 I=1,N
      IQ=IQ+N
      LL=I+(I*I-I)/2
      JQ=N*(I-2)
      DO 185 J=I,N
      JQ=JQ+N
      MM=J+(J*J-J)/2
      IF(A(LL)-A(MM)) 170,185,185
  170 X=A(LL)
      A(LL)=A(MM)
      A(MM)=X
      IF(MV-1) 175,185,175
  175 DO 180 K=1,N
      ILR=IQ+K
      IMR=JQ+K
      X=R(ILR)
      R(ILR)=R(IMR)
  180 R(IMR)=X
  185 CONTINUE
      RETURN
      END
C***********************************************************************
	SUBROUTINE COVAR(BUF,NS,SINC,NCHAN,SUM,PRODUCT,EXCLUD,PTS)
	IMPLICIT NONE 
        INTEGER SINC,NCHAN,PTS,I,J,K,M,N,NS
	REAL BUF(NS,NCHAN),EXCLUD
        REAL*8 TEMP,TEST2,TEST1
	REAL*8 SUM(32),PRODUCT(528)
C
	DO I=1,NS,SINC
	    J = 1
	    DO WHILE (J.LE.NCHAN)
		IF (BUF(I,J).EQ.EXCLUD) THEN
			J = J+1
		   ELSE
			PTS = PTS+1
			N = 1
			DO M=1,NCHAN
			    SUM(M) = SUM(M)+BUF(I,M)
			    DO K=1,M
                              TEST2 = BUF(I,M)
                              TEST1 = BUF(I,K)
                              TEMP = TEST2*TEST1   
				PRODUCT(N)=PRODUCT(N)+TEMP
				N = N+1
			    END DO
			END DO
			J = 99999
		END IF
	    END DO
	END DO
	RETURN
	END
C***********************************************************************
C
	SUBROUTINE DSTMAT(GAINFAC,EVEC_HOLD,EVEC,NV)
C
C	This subroutine produces the composite transformation matrix used
C	to perform a color decorrelation stretch (DSTRETCH). The output 
C	matrix (in EVEC) is formed by multiplying the eigenvector rotation
C	matrix (EVEC_HOLD) by a scaling vector (GAINFAC), and by the 
C	transpose of the original matrix (to do the reverse rotation)
C
	IMPLICIT NONE
        INTEGER NV,I,J,K
        REAL*8 EVEC(NV,NV)
	REAL*4 GAINFAC(NV),EVEC_HOLD(NV,NV),X,BIGGEST
C
	BIGGEST = 1.0
	DO I=1,NV
	    DO J=1,NV
		X = 0.0
		DO K=1,NV
		    X = X + GAINFAC(K)*EVEC_HOLD(I,K)*EVEC_HOLD(J,K)
		END DO
		EVEC(J,I) = X
		BIGGEST = MAX(BIGGEST,ABS(X))
	    END DO
	END DO
C						Make all coefficients 1.0
C						or smaller. XFORM's
C						autoscaling works better
C						this way.
	DO I=1,NV
	    DO J=1,NV
		EVEC(J,I) = EVEC(J,I)/BIGGEST
	    END DO
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create eigenvec.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM eigenvec

   To Create the build file give the command:

		$ vimake eigenvec			(VMS)
   or
		% vimake eigenvec			(Unix)


************************************************************************/


#define PROGRAM	eigenvec
#define R2LIB

#define MODULE_LIST eigenvec.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create eigenvec.pdf
process help=*
PARM INP     TYPE=STRING     COUNT=(0:10)                 DEFAULT=--
PARM OUT     TYPE=STRING     COUNT=(0:10)                 DEFAULT=--
PARM SIZE    TYPE=INTEGER    COUNT=4                      DEFAULT=(1,1,0,0)
PARM SL	     TYPE=INTEGER   				  DEFAULT=1
PARM SS	     TYPE=INTEGER				  DEFAULT=1
PARM NL      TYPE=INTEGER				  DEFAULT=0
PARM NS      TYPE=INTEGER				  DEFAULT=0
PARM LINC    TYPE=INTEGER    COUNT=(0:1)                  DEFAULT=--
PARM INC     TYPE=INTEGER                                 DEFAULT=1
PARM CORR    TYPE=KEYWORD    COUNT=(0:1)   VALID="CORR"   DEFAULT=--
PARM DSTRETCH TYPE=KEYWORD   COUNT=(0:1) VALID="DSTRETCH" DEFAULT=--
PARM DSCALE  TYPE=REAL       COUNT=(0:32)                 DEFAULT=--
PARM EXCLUDE TYPE=REAL	     COUNT=(0:1)		  DEFAULT=--
PARM MSS     TYPE=INTEGER    COUNT=(0:1)   VALID=(1:32)   DEFAULT=--
PARM USE     TYPE=INTEGER    COUNT=(0:32)  VALID=(1:32)   DEFAULT=--
PARM MSSO    TYPE=INTEGER    COUNT=(0:1)   VALID=(1:32)   DEFAULT=--
PARM OUTPUT  TYPE=INTEGER    COUNT=(0:32)  VALID=(1:32)   DEFAULT=--
PARM SAVE    TYPE=STRING     				  DEFAULT="EMATRIX"
PARM TSAVE   TYPE=STRING     				  DEFAULT="TMATRIX"
PARM AREA    TYPE=INTEGER    COUNT=(0:200)                DEFAULT=--
PARM NVAR    TYPE=INTEGER    COUNT=(0:1)                  DEFAULT=--
PARM DATA    TYPE=REAL       COUNT=(0:500)                DEFAULT=--
END-PROC
.TITLE
	Program EIGENVEC
.HELP
PURPOSE:
	EIGENVEC will compute the principal component transformation 
matrix for up to 32 input channels.  The covariance matrix, the transformation
matrix of eigen-vectors, and the eigen-values are printed.  This program
should normally be run via the procedure EIGEN.

OPERATION:

The measure of inter-dimensional correlation in the multi-variate
system is usually defined by the covariance matrix of the multi-variate
data.  The linear transformation that diagonalizes the covariance matrix 
can also be applied to the original data and produce a multi-variate 
system with an inter-dimensional correlation of zero, i.e.; completely
uncorrelated multi-variate data.  The linear transformation that
accomplishes this is the matrix of eigen-vectors or characteristic vectors.

A common application of this transformation is to reduce the 
dimensionality of a multi-variate system.  The objective is to summarize
most of the variance, or information content, in a system with a 
lessor number of 'artificial' variates, i.e.; principal components. 
Effectively, by uncorrelating the system, we are compressing most of the
information into a system with lower dimensionality. 

Assume an n-variate system ( n channels of data ).  Let K be the 
n x n covariance matrix of this data and A be the matrix of eigen-vectors
of K.  Associated with each eigen-vector A(j) there is an eigen-value or 
characteristic root, e(j).

	Let A(j) = (a(1j),a(2j),...,a(nj)) be the eigen-vector corresponding 
	to the jth largest eigen-value.

	Let X = (x(1),x(2),...,x(n)) be the n-variate observation 
	( n-dimensional pixel ).

	Then the jth principle component is: v(j) = A(j)  X = a(ij)x(i)

NOTE:  The jth eigen-value is actually the variance in the jth 
principle componemt dimension.  Therefore, the eigen-values are useful 
as a measure the information content that can be expected in the output 
pictures.
.PAGE
WRITTEN BY:  J. D. Addington,   19 Oct. 1979

CURRENT COGNIZANT PROGRAMMER:  Ron Alley
Made portable for UNIX         W. Huey (CRI) 1994
Removed aliases and lets from tst pdf.  FR76949  May 1995

.LEVEL1
.VARIABLE INP
input data sets
.VARIABLE OUT
output data sets
.VARIABLE SIZE
The standard Vicar size
 field (sl,ss,nl,ns)
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE LINC
Compute transform
from every nth line.
.VARIABLE INC
Compute transform from every
nth line and nth sample
.VARIABLE CORR
Compute eigenvectors from
the correlation matrix
.VARIABLE DSTRETCH
Output decorrelation stretch 
matrix, rather than eigenvector
matrix
.VARIABLE DSCALE
(Used only with DSTRETCH)
Adjust the variance equalization
scaling factors by the specified
values.
.VARIABLE EXCLUDE
Pixels with this DN in all
bands will be excluded from
all computations.
.VARIABLE MSS
The number of input bands,
when the input is in MSS format.
.VARIABLE USE
When input is MSS and not all
bands are to be used, use these
bands.
.VARIABLE MSSO
The number of output components,
if the output is to be in MSS
format.
.VARIABLE OUTPUT
The components to be output.
Default is to output the first
components, in order.
.VARIABLE SAVE
The name for the parameter 
dataset to hold the eigenvector
matrix.
.VARIABLE TSAVE
The name for the parameter
dataset to hold the transpose
of the eigenvector matrix.
.VARIABLE AREA
The subareas to be used to
compute statistics. Up to 50
regions (SL,SS,NL,NS) may be
entered. Default is to use
the entire image.
.VARIABLE NVAR
FOR NON-IMAGE DATA ONLY
The number of dimensions
to the data.
.VARIABLE DATA
FOR NON-IMAGE DATA ONLY
Up to 500 data values for
computing statistics.
.LEVEL2
.VARIABLE INP
Input datasets containing registered images, or a single input dataset 
in MSS format.
.VARIABLE OUT
Output datasets containing transformed images, or a single output
dataset in MSS format.
.VARIABLE SIZE
The standard Vicar size field ( starting_line, starting_sample, 
number_of_lines, number_of_samples).
.VARIABLE LINC
Statistics are gathered using only every n'th line of the image. If
the auto-scaling option of XFORM is invoked, every n'th line is used to
determine the scaling parameters.
.VARIABLE INC
Statistics are gathered using only every n'th line and n'th sample of the
image. If different line and sample increments are desired, use INC to
specify the sample increment and use LINC to override the INC line increment
value.
.VARIABLE CORR
The correlation matrix is used for computing eigenvectors instead of the
covariance matrix.
.VARIABLE DSTRETCH
When the DSTRETCH keyword is used, the output matrix will be a composite
matrix that describes the color decorrelation stretch transformation. This
matrix is the product of the eigenvector matrix, a variance equalization
vector (which has the effect of contrast stretching), and the transpose of
the original matrix (for the "back rotation" step).
.VARIABLE DSCALE
This parameter has effect only when used with the DSTRETCH parameter. The
DSTRETCH operation consists of a rotation into principal component space,
rescaling to cause variance equalization among components, and the reverse
rotation out of principal component space. DSCALE applies an additional
scaling at the variance equalization stage. This can be used to suppress
components known to be noisy, at the expense of re-introducing some 
correlation among bands.
.VARIABLE EXCLUDE
A pixel is excluded from use in determining the transformation if the 
DN is equal to 'EXCLUDE' in each channel. If the auto-scaling option of 
XFORM is used, these pixels will also be excluded from the computation of
scaling parameters.
.VARIABLE MSS
Denotes the input data is in MSS format and contains 'MSS' number of 
channels of data.
.VARIABLE USE
If the input is in MSS format, use only the listed channels. The default
is that all channels are used.
.VARIABLE MSSO
Denotes that that output is to be in MSS format, and will contain 'MSSO'
components (bands) of data.
.VARIABLE OUTPUT
The output will consist of the specified components. The default is to
output the first n components, where n is the number of output datasets,
or for MSS format output, the value of MSSO.
.VARIABLE SAVE
The eigenvector transformation matrix will be saved as a VICAR2 parameter
dataset with the name denoted by the SAVE parameter.
.VARIABLE TSAVE
The transpose of the eigenvector transformation matrix will be saved as a
VICAR2 parameter dataset with the name denoted by the TSAVE parameter. This
matrix produces the rotation necessary to return to the original coordinate
system.
.VARIABLE AREA
Sets of (Starting_line, Starting_sample, Number_of_lines, Number_of_samples)
are given to define subareas used to generate the image statistics. If
auto-scaling is used in XFORM, only the first subarea listed will be used
to gather the auto-scaling statistics. The default is that the entire image 
is sampled.
.VARIABLE NVAR
Integer specifying the dimensionality of the data.  This parameter is 
only used when multi-variate data is input through the parameter field
(DATA).
.VARIABLE DATA
1 to 500 data values may be input through the parameter field. That is, the
number of case times NVAR must be no greater than 500. No input data sets 
are used in this mode.  The covariance matrix and the eigenvectors for this 
set of data are computed and printed.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsteigen.pdf
procedure
  refgbl $echo
  refgbl $autousage
  RefGbl $SysChar

LOCAL DIR     TYPE=STRING
LOCAL UVBIMG  TYPE=STRING
LOCAL UVHIMG  TYPE=STRING
LOCAL VIOBIMG TYPE=STRING
LOCAL VIOHIMG TYPE=STRING
LOCAL BLUBIMG TYPE=STRING
LOCAL BLUHIMG TYPE=STRING
body

  IF ($SysChar(1)="VAX_VMS")
    Let DIR="wms_test_work:[testdata.mipl.vgr]"
  Else
    Let DIR="/project/test_work/testdata/mipl/vgr/"
  End-If
let $autousage="none"
let $echo="yes"
let UVBIMG= "&DIR"//"uvb.img"
let UVHIMG= "&DIR"//"uvh.img"
let VIOBIMG= "&DIR"//"viob.img"
let VIOHIMG= "&DIR"//"vioh.img"
let BLUBIMG= "&DIR"//"blub.img"
let BLUHIMG= "&DIR"//"bluh.img"
!
write " THIS IS A TEST FOR PROGRAM EIGENVEC"
write " TEST BYTE INPUT"
write " TEST PARM INC"
eigenvec INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) INC=10
write " TEST PARM LINC"
eigenvec INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) LINC=20
write " TEST PARM AREA"
eigenvec INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) +
 AREA=(20,1,10,20,40,11,10,20)
write " TEST PARM EXCLUD"
eigenvec INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) EXCLUD=11 +
 AREA=(20,1,10,20,40,11,10,20)
write " TEST PARM CORR"
eigenvec INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) 'CORR INC=50
write " TEST PARM SIZE"
eigenvec INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) SIZE=(11,11,20,20)
write " TEST HALFWORD DATA"
write " TEST PARM INC"
eigenvec INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) INC=100
write " TEST PARM LINC"
eigenvec INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) LINC=20
write " TEST PARM AREA"
eigenvec INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) AREA=(400,1,10,10)
write " TEST PARM EXCLUD"
eigenvec INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) EXCLUD=12 +
 AREA=(101,11,10,10,201,11,10,10)
write " TEST PARM CORR"
eigenvec INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) INC=50 'CORR
write " TEST PARM SIZE"
eigenvec INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) SIZE=(100,101,10,100)
write " TEST PARM DATA"
eigenvec NVAR=3 DATA=(1.,33.,179.,991.,127.3,0.,341.0,987.6,99.3,111.,123.,+
 50.,333.,15.,999.,1234.,378.,111.1,255.,678.,1911.)
mss INP=(&UVHIMG,&VIOHIMG,&BLUHIMG) OUT=eigen.msh
mss INP=(&UVBIMG,&VIOBIMG,&BLUBIMG) OUT=eigen.msb
write " TEST MSS MODE OF BYTE DATA"
write " TEST PARM INC"
eigenvec  eigen.msb MSS=3 INC=100
write " TEST PARM LINC"
eigenvec  eigen.msb MSS=3 LINC=20
write " TEST PARM AREA"
eigenvec  eigen.msb MSS=3 AREA=(101,1,10,10)
write " TEST PARM USE"
eigenvec  eigen.msb MSS=3 USE=(1,3) LINC=100
write " TEST PARM CORR AND AREA"
eigenvec  eigen.msb MSS=3 'CORR AREA=(101,1,10,10,201,11,10,10)
write " TEST PARM EXCLUD AND SIZE"
eigenvec  eigen.msb MSS=3 EXCLUD=8 SIZE=(11,11,10,10)
write " TEST MSS MODE OF HALFWORD DATA"
write " TEST PARM INC"
eigenvec  eigen.msh MSS=3 INC=100
write " TEST PARM LINC"
eigenvec  eigen.msh MSS=3 LINC=20 
write " TEST PARM AREA"
eigenvec  eigen.msh MSS=3 AREA=(101,1,10,10)
write " TEST PARM USE"
eigenvec  eigen.msh MSS=3 USE=(1,3) LINC=100
write " TEST PARM CORR AND AREA"
eigenvec  eigen.msh MSS=3 'CORR AREA=(101,1,10,10,201,11,10,10)
write " TEST PARM EXCLUD AND SIZE"
eigenvec  eigen.msh MSS=3 EXCLUD=8 SIZE=(11,11,10,10)
  let $echo="no"
end-proc
$ Return
$!#############################################################################
