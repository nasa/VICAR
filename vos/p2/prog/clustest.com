$!****************************************************************************
$!
$! Build proc for MIPL module clustest
$! VPACK Version 1.9, Friday, March 05, 2010, 11:11:50
$!
$! Execute by entering:		$ @clustest
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
$ write sys$output "*** module clustest ***"
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
$ write sys$output "Invalid argument given to clustest.com file -- ", primary
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
$   if F$SEARCH("clustest.imake") .nes. ""
$   then
$      vimake clustest
$      purge clustest.bld
$   else
$      if F$SEARCH("clustest.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake clustest
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @clustest.bld "STD"
$   else
$      @clustest.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create clustest.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack clustest.com -mixed -
	-s clustest.f -
	-i clustest.imake -
	-p clustest.pdf -
	-t tstclustest.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create clustest.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	CLUSTEST - CLUSTER TESTING PROGRAM
C		   DETERMINES WHETHER CLUSTERS COME FROM DIFFERENT POPULATIONS
C
C
C	REVISION:  A		NEW
C
C
C	ALGORITHM DESIGNER AND PROGRAMMER:	FRANK EVANS      NOVEMBER 1985
C
C	COGNIZANT PROGRAMMER:	KFE
C
C  7-APR-87  ...LWK...  ADDED FIX FOR BYTE-FORMAT USTATS FILES,  FIXED BUG IN
C                      CLUSTER-PAIRS DISTANCE MATRIX.
C
C  5-SEP-94  ...CRS (CRI) .. PORTED TO UNIX
c  5 feb 0   ...lwk...  replaced variable in format statement with internal writes
C
	IMPLICIT  NONE
	INTEGER	  MAXCLUS, MAXDIM, MAXRECSIZ
	PARAMETER ( MAXCLUS = 200 )		
	PARAMETER ( MAXDIM = 12 )
	PARAMETER ( MAXRECSIZ = MAXDIM*(MAXDIM+1)/2+MAXDIM+3 )

	INTEGER	UNIT(2), NCLUS(2)
	INTEGER BUFFER(MAXRECSIZ)
	INTEGER	NUMBER(MAXCLUS,2)
	INTEGER FILE, STATUS, NUMFILES, NWORDS, NDIM, COUNT
	INTEGER C, C1,C2, N,M, K,  LEVEL
	INTEGER N1,N2
	INTEGER STARTP, ENDP, NUMP,  NCLUSPAIR, NW1
	REAL*8	MEAN(MAXDIM,MAXCLUS,2), COV(MAXDIM,MAXDIM,MAXCLUS,2)
	REAL	MEANVECT(MAXDIM), XVECTOR(MAXDIM), SIGMA(MAXDIM,MAXDIM)
	REAL	R2 , MAHADIST(MAXCLUS,MAXCLUS)
	REAL	MINMAHA(MAXCLUS), AVGMAHA(MAXCLUS), MINTOT, AVGTOT, MAXTOT
	INTEGER	MINCLUS(MAXCLUS)
	REAL	COMP(MAXDIM), MAXCOMP(MAXDIM)
	REAL	AVGCOMP(MAXDIM)
	REAL	RBUF(MAXRECSIZ), RNDIM
	REAL	MAHAC1, MAHAC2,  X0
	REAL	MAHAMU, MAHAVAR, MU0, U, NOBJ, PHI
	REAL	T2, F, FISH
	LOGICAL	ERROR, CONFID, XVPTST
	CHARACTER*80 STRING
	CHARACTER*72 INPFILES(2)
	CHARACTER*8 INFMT
	character*20 fstrng
	INTEGER TEMPINT
	REAL*8 	RNDOFF
	EQUIVALENCE (RBUF,BUFFER)

	DATA RNDOFF/1000000./
	CALL IFMESSAGE('CLUSTEST version 5-FEB-10')
C--		OPEN THE ONE OR TWO STATISTICS DATASETS
	CALL XVP ('INP', INPFILES, NUMFILES)
	DO FILE = 1,NUMFILES
	    CALL XVUNIT(UNIT(FILE),'INP',FILE,STATUS,' ')
	    CALL XVOPEN(UNIT(FILE),STATUS, 'IO_ACT','SA',
     &       'OPEN_ACT','SA',' ')
	    CALL XVGET(UNIT(FILE),STATUS, 'NL',NCLUS(FILE), 'NS', NWORDS,
     &	     'FORMAT', INFMT,' ')
	    IF (INFMT.EQ.'BYTE') THEN			!FOR USTATS OUTPUT
	      NWORDS = NWORDS/4
	    ELSEIF (INFMT.NE.'FULL') THEN
	      CALL MABEND(' INVALID FILE FORMAT',' ')
	    ENDIF
	    IF (FILE.EQ.1) NW1 = NWORDS
	    IF (FILE.EQ.2) THEN
	      IF (NWORDS.NE.NW1) CALL MABEND(
     &		'INPUT FILES MUST HAVE EQUAL # DIMENSIONS',' ')
	    ENDIF
	ENDDO
	IF (NUMFILES .EQ. 1)   NCLUS(2) = NCLUS(1)
	
C--		COMPUTE THE NUMBER OF DIMENSIONS FROM THE SIZE OF THE RECORD
C	NWORDS = NDIM*(NDIM+1)/2 + NDIM + 3  =  (NDIM**2 + 3*NDIM + 6)/2
	RNDIM = (SQRT(8.*FLOAT(NWORDS)-15.)-3.)/2.
	NDIM = NINT(RNDIM)

C--		TEST FOR VARIOUS ERROR CONDITIONS
	IF (ABS(RNDIM - NDIM) .GT. 0.001) THEN
	    CALL XVMESSAGE('Not a valid statistics data set record',' ')
	    CALL ABEND
	ENDIF
	IF (NDIM .GT. MAXDIM) THEN
	    CALL XVMESSAGE('Too many dimensions (channels)',' ')
	    CALL ABEND
	ENDIF
	IF (NCLUS(1) .LT. 2  .AND.  NUMFILES .EQ. 1) THEN
	    CALL XVMESSAGE('Must be at least two clusters',' ')
	    CALL ABEND
	ENDIF
	DO FILE = 1, NUMFILES
	    IF (NCLUS(FILE) .GT. MAXCLUS) THEN
	        CALL XVMESSAGE('Too many clusters',' ')
	        CALL ABEND
	    ENDIF
	ENDDO



C--		READ IN THE INFORMATION FROM THE STAT DATASET
C-		  THE NUMBER OF OBJECTS, THE MEAN VECTOR, AND
C-		  THE COVARIANCE MATRIX STORED IN A COMPRESSED FORMAT
	DO FILE = 1,NUMFILES
	  DO C = 1,NCLUS(FILE)
	    CALL XVREAD(UNIT(FILE),BUFFER,STATUS,' ')
	    NUMBER(C,FILE) = BUFFER(3+NDIM)
	    COUNT = 1
	    DO N = 1,NDIM
              TEMPINT = NINT(RBUF(2+N)*RNDOFF)
	      MEAN(N,C,FILE) = DBLE(TEMPINT/RNDOFF)
	      DO M = 1,N
	        TEMPINT = NINT(RBUF(3+NDIM+COUNT)*RNDOFF)
	        COV(M,N,C,FILE) = DBLE(TEMPINT/RNDOFF)
		COV(N,M,C,FILE) = COV(M,N,C,FILE)
		COUNT = COUNT + 1
	      ENDDO
	    ENDDO
	  ENDDO
	  CALL XVCLOSE(UNIT(FILE),STATUS,' ')
	ENDDO

C1101	      FORMAT(A,F18.15,A,F18.15)

C-		FIND OUT HOW MUCH STUFF TO LIST OUT
	CALL XVP('LEVEL',LEVEL,COUNT)

C--		LIST THE INFORMATION IN THE STAT DATASET
	IF (LEVEL .GE. 3) THEN
	  DO FILE = 1, NUMFILES
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' ',' ')
	    WRITE (STRING,99), 'STATISTICAL SUMMARY FOR FILE',FILE
	    CALL NPRINT (STRING)
	    CALL XVMESSAGE(' ',' ')
	    DO C = 1,NCLUS(FILE)
		WRITE (STRING,101),'CLUSTER: ', C,
     +			 '      NUMBER OF MEMBERS: ',NUMBER(C,FILE)
		CALL NPRINT (STRING)
		IF (LEVEL .GE. 4) THEN
		    CALL XVMESSAGE(' MEAN           STD',' ')
		    DO N = 1,NDIM
			WRITE (STRING,105),MEAN(N,C,FILE),SQRT(COV(N,N,C,FILE))
			CALL NPRINT (STRING)
		    ENDDO
		ENDIF
	    ENDDO
	  ENDDO
	ENDIF
99	FORMAT (A,I2)
101	FORMAT (A,I3,A,I6)
105	FORMAT (E10.3,4X,E10.3)


	CONFID = .NOT. XVPTST('MAHA')

C--		
	IF (NUMFILES .EQ. 1) THEN

	    DO  N = 1, NDIM
	        MAXCOMP(N) = -1.0E20
	        AVGCOMP(N) = 0.0
	    ENDDO

C--		GO THROUGH ALL PAIRS OF CLUSTERS
	    DO C1 = 2,NCLUS(1)
	        DO C2 = 1,C1-1

		    N1 = NUMBER(C1,1)
		    N2 = NUMBER(C2,1)
C-			CALCULATE THE VECTOR BETEEN THE MEANS
		    DO N = 1,NDIM
		        MEANVECT(N) = MEAN(N,C1,1) - MEAN(N,C2,1)
		    ENDDO
C-			CALCULATE THE POOLED COVARIANCE MATRIX
		    DO N = 1,NDIM
		        DO M = 1,NDIM
			    SIGMA(M,N) = ( (N1-1)*COV(M,N,C1,1)
     +				     +     (N2-1)*COV(M,N,C2,1) )
     +					 / (N1+N2-2)
		        ENDDO
		    ENDDO

C--			COMPUTE THE INVERSE OF THE POOLED COVARIANCE MATRIX
C-			  TIMES THE MEAN VECTOR
		    CALL GAUSSES( SIGMA, MEANVECT, XVECTOR, NDIM, ERROR )

		    IF (ERROR) THEN
			WRITE (STRING,199), 'CLUSTERS', C1, C2,
     +				'  HAVE TOO SMALL WITHIN GROUP VARIANCE'
199			FORMAT (A,I4,I4,A)
			CALL NPRINT (STRING)
		    ENDIF

C--			DO THE DOT PRODUCT TO GET THE SQUARE OF THE MAHALANOBIS
		    R2  = 0
		    DO  N = 1, NDIM
		        COMP(N) = XVECTOR(N) * MEANVECT(N)
		        R2 = R2 + COMP(N)
		    ENDDO

		    IF (R2 .GE. 0.0 .AND. .NOT. ERROR) THEN
C-				GET SOME INFO ON THE COMPONENTS OF THE MAHA 
		        DO  N = 1, NDIM
		            MAXCOMP(N) = MAX(MAXCOMP(N),COMP(N)/R2)
		            AVGCOMP(N) = AVGCOMP(N) + COMP(N)/R2
		        ENDDO
		        NCLUSPAIR = NCLUSPAIR + 1

		        MAHADIST(C1,C2) = SQRT(R2)
C--			    IF DESIRED COMPUTE THE APPROXIMATION TO THE
C-				CONFIDENCE LEVEL
		        IF (CONFID) THEN
C--				FIND THE RELATIVE SIZES OF THE TWO CLUSTERS
C-				  BY CALCULATING THE MAHALANOBIS DISTANCE WITH
C-				  EACH COVARIANCE MATRIX SEPARATELY
			    CALL GAUSSES( COV(1,1,C1,1), MEANVECT,
     +					  XVECTOR, NDIM, ERROR )
			    MAHAC1 = 0.0
			    DO N = 1, NDIM
				MAHAC1 = MAHAC1 + XVECTOR(N)*MEANVECT(N)
			    ENDDO
			    CALL GAUSSES( COV(1,1,C2,1), MEANVECT,
     +					  XVECTOR, NDIM, ERROR )
			    MAHAC2 = 0.0
			    DO N = 1, NDIM
				MAHAC2 = MAHAC2 + XVECTOR(N)*MEANVECT(N)
			    ENDDO
C ABS FUNCTION ADDED TO PREVENT SQRT OF NEGATIVE NUMBER ERROR:
			    MAHAC1 = SQRT(ABS(MAHAC1))
			    MAHAC2 = SQRT(ABS(MAHAC2))
C--				FROM THE FRACTIONAL SIZES OF THE CLUSTERS,
C-				  THE NUMBER OF OBJECTS IN BOTH CLUSTERS,
C-				  AND THE NUMBER OF DIMENSIONS THE CONFIDENCE
C-				  LEVEL IS APPROXIMATED FOR THE UNIFORM
C-				  PROBABILITY DENSITY CASE
			    X0 = MAHAC1/(MAHAC1+MAHAC2)
		  	    NOBJ = N1 + N2
			    MU0 = SQRT(3./ (3.*X0**2 - 3.*X0 + 1) )
			    MAHAMU = MU0 + (25.0/NOBJ)*(NDIM-1)
			    MAHAVAR = 10.0 * NOBJ**(-1.08)
			    U = (MAHADIST(C1,C2) - MAHAMU) / SQRT(MAHAVAR)
			    MAHADIST(C1,C2) = PHI(U)
		        ENDIF
		    ELSE
		        MAHADIST(C1,C2) = -1.0
		    ENDIF
	        ENDDO
	    ENDDO


C--		FILL IN THE OTHER HALF OF THE DISTANCE MATRIX
	    DO C1 = 2,NCLUS(1)
	        DO C2 = 1,C1-1
		    MAHADIST(C2,C1) = MAHADIST(C1,C2)
	        ENDDO
	    ENDDO

	ELSE


C--		IF COMPARING TWO STAT DATASETS THEN COMPARE EACH CLUSTER
C-		    IN FIRST WITH EACH IN SECOND
	    DO C1 = 1,NCLUS(1)
	        DO C2 = 1,NCLUS(2)

		    N1 = NUMBER(C1,1)
		    N2 = NUMBER(C2,2)
		    DO N = 1,NDIM
		        MEANVECT(N) = MEAN(N,C1,1) - MEAN(N,C2,2)
		    ENDDO
		    DO N = 1,NDIM
		        DO M = 1,NDIM
			    SIGMA(N,M) = ( (N1-1)*COV(M,N,C1,1)
     +				     +     (N2-1)*COV(M,N,C2,2) )
     +					   /(N1+N2-2)
		        ENDDO
		    ENDDO

		    CALL GAUSSES( SIGMA, MEANVECT, XVECTOR, NDIM, ERROR )
		    IF (ERROR) THEN
			WRITE (STRING,199), 'CLUSTERS', C1, C2,
     +				'  HAVE TOO SMALL WITHIN GROUP VARIANCE'
			CALL NPRINT (STRING)
		    ENDIF

		    R2  = 0
		    DO  N = 1, NDIM
		        R2 = R2 + XVECTOR(N) * MEANVECT(N)
		    ENDDO

		    IF (R2 .GE. 0.0 .AND. .NOT. ERROR) THEN
			IF (CONFID) THEN
C--				COMPUTE HOTELLINGS T SQUARED STATISTIC
C-				  AND GET THE CONFIDENCE LEVEL FOR IT
			    T2 = R2/( 1./N1 + 1./N2 )
			    F = (N1+N2-NDIM-1) * T2/ ( (N1+N2-2)*NDIM )
			    MAHADIST(C1,C2) = FISH(F, NDIM, N1+N2-NDIM-1)
			ELSE
		 	    MAHADIST(C1,C2) = SQRT(R2)
			ENDIF
		    ELSE
		        MAHADIST(C1,C2) = -1.0
		    ENDIF
	        ENDDO
	    ENDDO

	ENDIF


	IF (LEVEL .GE. 2) THEN
C--			PRINT OUT THE MATRIX IF DESIRED
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' ',' ')
	    IF (CONFID) THEN
		CALL XVMESSAGE(
     &           'CONFIDENCE LEVELS BETWEEN PAIRS OF CLUSTERS',' ')
	    ELSE
	        CALL XVMESSAGE(
     &            'MAHALANOBIS DISTANCE BETWEEN PAIRS OF CLUSTERS',' ')
	    ENDIF
	    DO STARTP = 1,NCLUS(1),9
		CALL XVMESSAGE(' ',' ')
		CALL XVMESSAGE(' ',' ')
		ENDP = MIN(NCLUS(1),STARTP+9-1)
		NUMP = MIN(9,NCLUS(1)-STARTP+1)
		write(fstrng,109) nump
		WRITE (STRING,fstrng), (K,  K=STARTP,ENDP)
		CALL NPRINT (STRING)
		DO C2 = 1,NCLUS(2)
		    CALL XVMESSAGE(' ',' ')
		    IF (CONFID) THEN
		        write(fstrng,111) nump
			WRITE (STRING,fstrng),C2,(MAHADIST(K,C2),  K=STARTP,ENDP)
		    ELSE
		        write(fstrng,113) nump
			WRITE (STRING,fstrng),C2,(MAHADIST(K,C2),  K=STARTP,ENDP)
		    ENDIF
		    CALL NPRINT (STRING)
		ENDDO
	    ENDDO
c109	    FORMAT ('      ',<NUMP>(I3,5X))
c111	    FORMAT (I3,' :',<NUMP>(F6.3,2X))
c113	    FORMAT (I3,' :',<NUMP>(F6.2,2X))
c  variables in format stmts not supported by new compiler, replaced by
109	    format ('(''      ''',i2,'(I3,5X))')
111	    format ('(I3,'' :''',i2,'(F6.3,2X))')
113	    format ('(I3,'' :''',i2,'(F6.2,2X))')
	ENDIF



	IF (NUMFILES .EQ. 1) THEN

C--			CALCULATE THE MINIMUM AND AVERAGE MAHA OR CONFID LEVEL
	    MINTOT = 1.0E20
	    AVGTOT = 0.0
	    DO C1 = 1,NCLUS(1)
	        MINMAHA(C1) = 1.0E20
	        AVGMAHA(C1) = 0.0
	        DO C2 = 1,NCLUS(1)
		    IF (C1 .NE. C2) THEN
		        MINMAHA(C1) = MIN(MAHADIST(C1,C2),MINMAHA(C1))
		        AVGMAHA(C1) = AVGMAHA(C1) + MAHADIST(C1,C2)
		    ENDIF
	        ENDDO
	        AVGMAHA(C1) = AVGMAHA(C1)/(NCLUS(1)-1)
	        MINTOT = MIN(MINMAHA(C1),MINTOT)
	        AVGTOT = AVGTOT + AVGMAHA(C1)
	    ENDDO
	    AVGTOT = AVGTOT/NCLUS(1)

	    IF (LEVEL .GE. 1) THEN
		CALL XVMESSAGE(' ',' ')
		CALL XVMESSAGE(' ',' ')
	        CALL XVMESSAGE('CLUSTER   MINIMUM   AVERAGE',' ')
	        DO C1 = 1,NCLUS(1)
		    IF (CONFID) THEN
	                WRITE (STRING,116), C1, MINMAHA(C1), AVGMAHA(C1)
			CALL NPRINT (STRING)
		    ELSE
	                WRITE (STRING,117), C1, MINMAHA(C1), AVGMAHA(C1)
			CALL NPRINT (STRING)
		    ENDIF
	        ENDDO
	    ENDIF
116	    FORMAT (' ',I3,' :  ',F6.3,4X,F6.3)
117	    FORMAT (' ',I3,' :  ',F6.2,4X,F6.2)


	    CALL XVMESSAGE (' ',' ')
	    IF (CONFID) THEN
		WRITE (STRING,118), 'OVERALL MINIMUM AND AVERAGE : ',
     +				 MINTOT, AVGTOT
	    ELSE
		WRITE (STRING,119), 'OVERALL MINIMUM AND AVERAGE : ',
     +				 MINTOT, AVGTOT
	    ENDIF
	    CALL NPRINT (STRING)
	    CALL XVMESSAGE(' ',' ')
118	    FORMAT (A,F6.3,3X,F6.3)
119	    FORMAT (A,F6.2,3X,F6.2)


C--			LIST OUT THE MAX AND AVERAGE OF COMPONENTS OF THE MAHA
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(
     &         'COMPONENTS OF THE MAHALANOBIS DISTANCE BY BAND',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('BAND       MAX    AVERAGE   ',' ')
	    DO N = 1,NDIM
		AVGCOMP(N) = AVGCOMP(N)/NCLUSPAIR
		WRITE (STRING,123), N,MAXCOMP(N),AVGCOMP(N)
		CALL NPRINT (STRING)
	    ENDDO
123		FORMAT(I3,5X,F7.3,2X,F7.3)

	ELSE

	    MINTOT = 1.0E20
	    MAXTOT = -1.0E20
	    DO C1 = 1,NCLUS(1)
	        MINMAHA(C1) = 1.0E20
	        DO C2 = 1,NCLUS(2)
		    IF (MAHADIST(C1,C2) .LT. MINMAHA(C1)) THEN
		        MINMAHA(C1) = MAHADIST(C1,C2)
		        MINCLUS(C1) = C2
		    ENDIF
	        ENDDO
	        MINTOT = MIN(MINMAHA(C1),MINTOT)
	        MAXTOT = MAX(MINMAHA(C1),MAXTOT)
	    ENDDO

	    MINTOT = 1.0E20
	    MAXTOT = -1.0E20
	    DO C1 = 1,NCLUS(1)
	        MINMAHA(C1) = 1.0E20
	        DO C2 = 1,NCLUS(2)
		    IF (MAHADIST(C1,C2) .LT. MINMAHA(C1)) THEN
		        MINMAHA(C1) = MAHADIST(C1,C2)
		        MINCLUS(C1) = C2
		    ENDIF
	        ENDDO
	        MINTOT = MIN(MINMAHA(C1),MINTOT)
	        MAXTOT = MAX(MINMAHA(C1),MAXTOT)
	    ENDDO


	    IF (LEVEL .GE. 1) THEN
		CALL XVMESSAGE(' ',' ')
		CALL XVMESSAGE(' ',' ')
	        CALL XVMESSAGE(
     &                 'CLUSTER      MINIMUM    MIN CLUSTER',' ')
	        DO C1 = 1,NCLUS(1)
		    IF (CONFID) THEN
	 		WRITE (STRING,130), C1,MINMAHA(C1),MINCLUS(C1)
		    ELSE
	 		WRITE (STRING,131), C1,MINMAHA(C1),MINCLUS(C1)
		    ENDIF
		    CALL XVMESSAGE(STRING,' ')
	        ENDDO
	    ENDIF
130	    FORMAT (' ',I3,'    :  ',F6.3,'    : ',I3)
131	    FORMAT (' ',I3,'    :  ',F6.2,'    : ',I3)

	    CALL XVMESSAGE(' ',' ')
	    IF (CONFID) THEN
	        WRITE (STRING,132), 
     +			'MINIMUM AND MAXIMUM FOR CLOSEST CLUSTER: ',
     +			 MINTOT,MAXTOT
	    ELSE
	        WRITE (STRING,133),
     +			 'MINIMUM AND MAXIMUM FOR CLOSEST CLUSTER: ',
     +			 MINTOT,MAXTOT
	    ENDIF
	    CALL NPRINT (STRING)
	    CALL XVMESSAGE(' ',' ')
132	    FORMAT (A,F6.3,3X,F6.3)
133	    FORMAT (A,F6.2,3X,F6.2)


	ENDIF

	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' ',' ')

	RETURN
	END



	SUBROUTINE NPRINT(STRING)
C    USES XVMESSAGE TO PRINT A STRING BUT DOES NOT PRINT THE EXTRA SPACES AT END
	IMPLICIT NONE
	CHARACTER*(*) STRING
	INTEGER	I

	I = LEN(STRING)
	DO WHILE (STRING(I:I) .EQ. ' ' .AND. I .GT. 0)
	    I = I - 1
	ENDDO

	CALL XVMESSAGE(STRING(1:I),' ')

	RETURN
	END





C     STOLEN FROM VICAR PROGRAM DENDSTAT
	SUBROUTINE GAUSSES (A, Y, X, NCOL, ERROR)
C#######################################################################
C  NAME OF ROUTINE
C     GAUSSES ( GAUSSian Elimination for Symmetric matrix )
C  PURPOSE
C ----- FORTRAN subroutine to solve simultaneous equations
C ----- using Gaussian elimination.
C      special thanks to STEVE ADAMS for supplying this routine's ancestor.
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C  CALLING SEQUENCE
C      Standard subroutine call and return. 
C  INPUT AND OUTPUT PARAMETERS     
C        This routine solves the equation  y = Ax, where y is a known
C        vector, A is a known symmetric square matrix, and x is the 
C        unknown vector.
C
C      A array    - matrix A stored in normal mode.
C      Y array    - vector y.
C      X array    - vector x, the solution.
C      NCOL       - number of rows and columns in A matrix.
C      ERROR      - .TRUE. if no solution obtained. .FALSE. otherwise.
C  CALLED BY
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PARAMETER ( MAXDIM = 12 )          ! MAXIMUM NUMBER OF CHANNELS
C				MUST BE THE SAME AS IN CALLING ROUTINE
	LOGICAL ERROR
	INTEGER NCOL, I, J, N, N1, I1, K, L
	REAL*4  Y(MAXDIM), X(MAXDIM), A(MAXDIM,MAXDIM)
	REAL*4  B(MAXDIM,MAXDIM), W(MAXDIM), BIG, AB, 
     &          SUM, T, TEMP
C
C=================START OF EXECUTABLE CODE===============================
C
	ERROR = .FALSE.
	N = NCOL

	DO 20 I = 1, N
	  DO 10 J = 1, N
	    B(I,J) = A(I,J)
10	CONTINUE
	  W(I) = Y(I)
20	CONTINUE
	N1 = N - 1
	DO 80 I = 1, N1
	  BIG = ABS(B(I,I))
	  L = I
	  I1 = I+1
	  DO 30 J = I1, N
	    AB = ABS(B(J,I))
	    IF (AB .LE. BIG) GO TO 30
	      BIG = AB
	      L = J
30	  CONTINUE
	  IF (BIG .EQ. 0.0) GO TO 80
	  IF ( L  .EQ.  I ) GO TO 50
	  DO 40 J = 1, N
            TEMP   = B(L,J)               ! SWAP ROW L AND ROW J.
            B(L,J) = B(I,J)
            B(I,J) = TEMP
40	  CONTINUE
            TEMP   = W(L)
            W(L)   = W(I)
            W(I)   = TEMP
50	  CONTINUE
	  DO 70 J = I1, N
	    T = B(J,I) / B(I,I)
	    DO 60 K = I1, N
	      B(J,K) = B(J,K) - T * B(I,K)
60	    CONTINUE
	    W(J) = W(J) - T * W(I)
70	  CONTINUE
80	CONTINUE

	IF (B(N,N) .EQ. 0.0)   THEN
            ERROR = .TRUE.
            GO TO 110
        END IF

	X(N) = W(N) / B(N,N)
	I = N - 1
C ----- Back substitution.
90	SUM = 0
	  I1 = I+1
	  DO 100 J = I1, N
	    SUM = SUM + B(I,J) * X(J)
100	  CONTINUE

	IF (B(I,I) .EQ. 0.0)   THEN
            ERROR = .TRUE.
            GO TO 110
        END IF

	X(I) = (W(I) - SUM) / B(I,I)
	  I = I - 1
	IF (I .GT. 0) GO TO 90

110     CONTINUE
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create clustest.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM clustest

   To Create the build file give the command:

		$ vimake clustest			(VMS)
   or
		% vimake clustest			(Unix)


************************************************************************/


#define PROGRAM	clustest
#define R2LIB

#define MODULE_LIST clustest.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/****    #define LIB_LOCAL  ******/
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create clustest.pdf
PROCESS HELP=*
PARM INP TYPE=STRING COUNT=1:2
PARM LEVEL TYPE=INTEGER DEFAULT=2 VALID=(0:4)
PARM MODE TYPE=KEYWORD COUNT=0:1 VALID=(MAHA,CONFID) DEFAULT=CONFID
END-PROC
.TITLE
VICAR Program CLUSTEST
.HELP
PURPOSE

    CLUSTEST determines how well the clusters in a statistics dataset are
separated by computing the Mahalanobis distance between pairs of clusters.
It can also help determine whether the the clusters are statistically
significant.
    In another mode of operation CLUSTEST can compare two statistics datasets
to determine how far apart the clusters of the different datasets are.  This
is useful for finding out if two different datasets are essentially the same.



EXECUTION

CLUSTEST STAT.SDS LEVEL=4 'CONFID

or

CLUSTEST (STAT1.SDS,STAT2.SDS)  LEVEL=3 'MAHA

    where the input files are statistical datasets of the type produced by
STATS, USTATS, and CLUSAN.  LEVEL is a parameter that specifies the amount
of printout desired with LEVEL=4 typing out everything.  If two input datasets
are specified then the comparison mode of operation goes into effect.  The
keywords (CONFID or MAHA) specify whether the Mahalanobis distance or the
corresponding fractional confidence level is output.




OPERATION

    For each cluster the statistical dataset contains the following
information:
1.  The class name (8 characters) which is not used in this program.
2.  The number of objects in the cluster.
3.  The mean or centroid of the cluster in each band.
4.  The covariance matrix of the cluster which contains information about the
	size, shape, and orientation of the cluster.


    For each pair of clusters CLUSTEST calculates the Mahalanobis distance
between the clusters.  The Mahalanobis distance is the distance between the
means of the clusters divided by the average of the sizes of the clusters.
Thus a large Maha distance indicates that the clusters are well separated
relative to their size.  If the clusters are up against each other (but not
intermingled) the Maha distance will be about 3 to 4.  If the Maha distance
is greater than about 4.5 then the clusters are quite distinct.  The mathe-
matical formula used is:

	MahaDist = Sqrt[ (Mu2-Mu1) *  S**-1  * (Mu2-Mu1) ]

	where Mu1 and Mu2 are the mean vectors for the clusters
	    and S is the pooled covariance matrix:

		S = ( (n1-1)*Cov1 + (n2-1)*Cov2 ) / (n1+n2-2)

	where n1 and n2 are the number of objects in the clusters and
	    Cov1 and Cov2 are the covariance matrices for the two clusters


    Determining whether the clusters are statistically significant is a tricky
business.  Even if the original data came from a population that was uniform
and had no intrinsic structure the random sampling process will introduce
some variations that the clustering algorithm will put into different clusters.
Thus it is necessary to know how well separated the clusters are to determine
if they do indeed represent actual structure in the underlying probability 
distribution that we are trying to learn about.  
    To determine if the clusters are statistically significant a statistical
test must be devised, which requires making some assumptions.  For this program
the null hypothesis (i.e. that the clusters are not statistically significant)
is that the data points come from a multidimesional uniform distribution which
has been cut in two along one axis.  For example, in two dimensions the null 
hypothesis is that the data points are equally likely to lie in any part of a 
square with all of those in one cluster coming from one side and all of those
in the other cluster coming from the other side.  Experimental tests have been
carried out to determine the probability of a particular Mahalanobis distance
for various numbers of data points, number of dimensions, and cutoff values.
The program contains an approximation to these results, so that Mahalanobis
distances can be translated into confidence levels.  The uniform distribution
assumption gives the most conservative confidence levels.  Other assumptions,
however, would be far too lenient unless one is assured that the the data 
followed the assumption.  The uniform assumption is likely to be too
conservative when there are many dimensions because there are then a huge
number of corners where real data is not likely to live.



RESTRICTIONS

The maximum number of clusters accepted is 200.
The maximum number of bands or channels is 12.


PRECISION AND PORTING NOTES  5-sEPT-94
    1.	The original test PDFs for both CLUSAN and CLUSTEST called GAUSNOIS
 	to create data files without specifying a SEED for the Random number
 	generator.  In order to produce repeatable test results, the procedures
 	have been changed to specify the same SEED each time the data sets are
 	generated.
 
    2.	CLUSTEST  uses double precision. However, the CLUSAN output 
 	statistical files which are used by CLUSTEST are single precision
 	(REAL*4).  The stated precision (from the VAX FORTRAN manuals) for
 	REAL*4 is "approximately 2**23, that is, typically seven decimal
 	digits".  In order to eliminate test differences (VAX vs. UNIX)
 	caused by reading the single precision values into double precision
 	variables, it is necessary to truncate the input values to six
 	decimal digits.  The truncation is currently being implemented by
 	this "ported' version of CLUSTEST.

 
    3.	The changes made for prting cause CLUSAN and CLUSTEST to perform
 	differently than the original baseline versions.  The main difference
 	appears to be the result of specifying default values for TEMP0 and
 	NCYCLES.  Not specifing default values for these parameters produce
 	inconsistent results between the different platforms.  For comparison
 	testing purposes, the original program was modified to use the same 
 	random number generator with the same seed value as the new ported 
 	versions.  However, even with these changes the original program does
 	not produce exactly the same results when run mutliple times.



Written by:	Frank Evans  	November 1985

Ported to UNIX: C.R. Schenk (CRI) 5-Sept-94

Cognizant Programmer:   K. F. Evans


.LEVEL1
.VARIABLE INP
Input statistical dataset(s)
.VARIABLE LEVEL
Specifies amount of printout
.VARIABLE MODE
'MAHA for Mahalanobis distances
'CONFID for confidence levels (default)
.LEVEL2
.VARIABLE INP
Input statistical dataset(s)
One stat dataset for within set cluster tests
Two stat datasets for between set comparison
.VARIABLE LEVEL
Specifies amount of printout according to the following:
Level      Prints out this and everything below
4	Means and variances for each band for each cluster
3	The number of objects in each cluster
2	The matrix of Mahalanobis distances or confidence levels
1	Just the minimum and average for each cluster
0	Just the overall minimum and average
.VARIABLE MODE
'MAHA for Mahalanobis distances
'CONFID for confidence levels (default)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstclustest.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
local DATA string
!	Make three two dimensional gaussian clusters and put them in MSS format
!       The following test data should already exist in current directory,
!       otherwise it will be created in the current default directory.
!dcl define/nolog gaussfile "DATA"
!dcl if f$search("gausnois.data").eqs."" then define/nolog gaussfile "NODATA"
!translog gaussfile DATA
!if (DATA = "NODATA")
!   WRITE "!!!!!! Cannot find gausnois.data file, creating new one."
!   gausnois FORMAT=REAL NL=10 NS=50 SEED=784374261 SIGMA=1 MEAN=0 OUT=a
!   gausnois FORMAT=REAL NL=10 NS=50 SEED=353227180 SIGMA=1 MEAN=2 OUT=b
!   gausnois FORMAT=REAL NL=10 NS=50 SEED=627591341 SIGMA=1 MEAN=-2 OUT=c
!   gausnois FORMAT=REAL NL=10 NS=50 SEED=261458087 SIGMA=1 MEAN=0 OUT=d
!   gausnois FORMAT=REAL NL=10 NS=50 SEED=466143364 SIGMA=1 MEAN=2 OUT=e
!   gausnois FORMAT=REAL NL=10 NS=50 SEED=313795670 SIGMA=0.5 MEAN=0 OUT=f
!   mss (a,b) g
!   mss (c,d) h
!   mss (e,f) i
!   append inp=(g,h,i) out=gausnois.data

!	Cluster the data and look at the clusters
  clusan gausnois.data  (sds,clus)  mss=2  nclus=(3,0.1)
  clustest  sds  level=4 'maha
  clustest  sds  level=4 

end-proc
$ Return
$!#############################################################################
