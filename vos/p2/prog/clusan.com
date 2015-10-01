$!****************************************************************************
$!
$! Build proc for MIPL module clusan
$! VPACK Version 1.9, Friday, March 05, 2010, 11:07:15
$!
$! Execute by entering:		$ @clusan
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
$ write sys$output "*** module clusan ***"
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
$ write sys$output "Invalid argument given to clusan.com file -- ", primary
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
$   if F$SEARCH("clusan.imake") .nes. ""
$   then
$      vimake clusan
$      purge clusan.bld
$   else
$      if F$SEARCH("clusan.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake clusan
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @clusan.bld "STD"
$   else
$      @clusan.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create clusan.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack clusan.com -mixed -
	-s clusan.f -
	-i clusan.imake -
	-p clusan.pdf -
	-t tstclusan.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create clusan.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	CLUSAN  - CLUSTERING USING SIMULATED ANNEALING
C
C
C	REVISION:  A		NEW
C
C	ALGORITHM DESIGNER AND PROGRAMMER:  FRANK EVANS   DECEMBER 1985

C  6-APR-87  ...lwk... added LINC/SINC, THRESH parameters, fixed use of SS,NS
c  5-SEPT-94  Ported to Unix CRS (CRI)
C 23-MAR-01  ...rea... fixed bug in TEMP parameter (xvp returns real*4 value in
C                      real*8 variable)
C 05-FEB-10  ...lwk... replaced ENCODE statement with internal write

	IMPLICIT NONE
	INTEGER	MAXNPNTS,MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)	! maximum number of data points
	PARAMETER (MAXNCLUS = 100)	! maximum number of clusters
	INTEGER	UNIT, STATUS, COUNT,DEF ! Vicar I/O and parameter call stuff
	INTEGER	SL,SS,NL,NS,NLI,NSI 	! Vicar standard (NOT QUITE -- lwk)
	INTEGER	NSB			! number of samps per band in MSS image
	INTEGER	NPNTS			! number of points (object) to cluster
	INTEGER	NDIM			! number of dimensions (variables)
	INTEGER	NCLUS			! current number of clusters
	INTEGER	LINE,SAMP,  N,M, C, I	! counters
	INTEGER	MSS			! number of MSS bands in image
	INTEGER	NCYCLE			! number of temp cycles to do
	INTEGER	NITER			! number of iterations per temp cycle
	INTEGER	NCLUSWANT		! number of clusters desired
	INTEGER	MINNCLUS,MAXCLUS	! min and max number of clusters
	INTEGER	MINSIZE			! min number of objects in clusters
	INTEGER TRIES
	INTEGER	NCHANGE, NCYCSTOP
	INTEGER	SEED,   RECSIZ
	INTEGER	CYCLE, ITER		! current temp cycle and iteration
	INTEGER	OBJECT, TOCLUS, ORIGCLUS
	INTEGER	CLUS1,CLUS2, NUMT
	INTEGER	SPLTCLUS,SPLTVAR
	REAL*8	DISTFUNC, DIST, MINDIST, MINCLUS ! for finding closest clusters
	REAL*8	TEMP,TEMP0,TEMPFAC
	REAL*8	LUMPFRAC,SPLITFRAC	! how often to try to lump and split
	REAL	NCLUSFAC,NCLUSPAR(2),TEMPX
	REAL*8	ENERGY,NEWENERGY, DELTAE
	REAL*8	BOLTZ, AVGENERGY,MINENERGY
	LOGICAL	XVPTST, RESTART
C
	INTEGER	BANDS(12)		! the bands (variables) to cluster
	INTEGER	CLUS(MAXNPNTS)		! the cluster number for each object
	INTEGER NUMBER(MAXNCLUS)	! the number of objects in each cluster
	REAL*8	MEAN(12,MAXNCLUS)	! the cluster mean vectors
	REAL*8	COV(12,12,MAXNCLUS)	! the cluster covariance matrices
	REAL*8	DATA(12,MAXNPNTS)	! each object's values in all dims
	REAL*8	VARMEAN(12)		! the mean over all obj for each dim
	REAL*8	VARSTD(12)		! the std dev over all obj for each dim
	REAL*8	VARDATA(12)		! the variance over all for each dim
	REAL*8	SUM(12,MAXNCLUS)	! the sum of all objs in each cluster
	REAL*8	SUMSQ(12,MAXNCLUS)	! the sum of squares of all obj in clus
	REAL*8	PI(12,MAXNCLUS)		! the variance in each dim for all clus
	REAL*8	SUMPI(12)		! the sum of all cluster variances
C
        REAL	RANNUM
	REAL	BUFFER(60000)
	REAL	REALBUF(100)
	INTEGER	INTBUF(100)
	CHARACTER*8  CLASNAM
	CHARACTER*32 INFORMAT
	CHARACTER*80	STRING

	EQUIVALENCE (REALBUF,INTBUF) , (CLASNAM,INTBUF)

C
C  ADDED BY LWK:
	INTEGER INCR,LINC,SINC,NLO,NSO,NSBO,NS0,LINE0, THRESHDEF
	REAL THRESH
	INTEGER NUMPLIN(10000)

C	DATA	RNDOFF/1000000./
	DATA	MINENERGY/-.0000005/
C
C  'NUMPLIN' KEEPS TRACK OF HOW MANY POINTS LIE ABOVE 'THRESH' ON EACH LINE,
C  IS NEEDED ONLY TO KEEP THE STRUCTURE OF THE CLUSTER DATASET OUTPUT SOMEWHAT 
C  SIMILAR TO THAT OF THE INPUT.  IT IMPOSES A RESTRICTION ON # LINES OF THE
C  INPUT THAT ARE USED.
	

	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM

C		GET THE INPUT PARAMETERS
        CALL IFMESSAGE('CLUSAN (double precision) vers. 05-FEB-10')
	RESTART = XVPTST('RESTART')

	CALL XVP('MSS',MSS,COUNT)		! number of bands in image
	CALL XVPARM('BANDS',BANDS,NDIM,DEF,12)	! which bands to use
	IF (NDIM.EQ.0)  THEN
	  NDIM = MSS
	  IF (RESTART) NDIM = NDIM-1
	  DO N = 1,NDIM
	    BANDS(N) = N
	  ENDDO
	ENDIF


C		OPEN THE IMAGE DATA FILE AND GET THE SIZE PARAMETERS
	CALL XVUNIT(UNIT,'INP',1,STATUS,' ')
	CALL XVOPEN(UNIT,STATUS, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'U_FORMAT','REAL',' ')
	CALL XVGET(UNIT,STATUS, 'FORMAT',INFORMAT,'NS',NS,' ')
	CALL XVSIZE(SL,SS,NL,NS0,NLI,NSI) 
c        !note NS0 is user-specified maximum to use
	IF (NS0.EQ.NS) NS0 = NS/MSS

C  LINE/SAMP INCREMENTS
	CALL XVPARM( 'INCR', INCR, COUNT, DEF,1)
	IF (DEF .EQ. 1) THEN
	  CALL XVPARM( 'LINC', LINC, COUNT, DEF,1)
	  CALL XVPARM( 'SINC', SINC, COUNT, DEF,1)
	ELSE
	  LINC = INCR
	  SINC = INCR
	ENDIF
C  THRESH:
	CALL XVPARM( 'THRESH', THRESH, COUNT, THRESHDEF,1)

C		READ IN THE DATA POINTS INTO DATA(DIM,PNT) FROM THE IMAGE 
	NSB = NS/MSS		! number of samples per band in image
	NPNTS = 0
	LINE0 = 0
	DO LINE=1,NL,LINC
	    LINE0 = LINE0+1
	    CALL XVREAD(UNIT,BUFFER,STATUS,'SAMP',1,'NSAMPS',NS,
     +			'LINE',LINE+SL-1,' ')
	    NUMPLIN(LINE0) = 0
	    DO SAMP = SS,NS0,SINC
		IF (THRESHDEF .EQ. 0) THEN
		  DO N = 1,NDIM
		    IF (BUFFER(SAMP+(BANDS(N)-1)*NSB).LT.THRESH) GO TO 100
		  ENDDO
		ENDIF
		NPNTS = NPNTS + 1
		IF (NPNTS .GT. MAXNPNTS) THEN
		    CALL xvmessage(
     +                'MAXIMUM NUMBER OF DATA POINTS EXCEEDED',' ')
		    CALL ABEND
		ENDIF
		NUMPLIN(LINE0) = NUMPLIN(LINE0)+1
		DO N = 1,NDIM
		    DATA(N,NPNTS) = BUFFER(SAMP+(BANDS(N)-1)*NSB)
		ENDDO
C		   if restarting then also get which cluster point was in
		IF (RESTART)  CLUS(NPNTS) = NINT(BUFFER(SAMP+(MSS-1)*NSB))
100		CONTINUE
	    ENDDO
	ENDDO
	CALL XVCLOSE(UNIT,STATUS,' ')
	WRITE (STRING,101), 'NUMBER OF POINTS: ', NPNTS
101	FORMAT (A,I6)
	CALL NPRINT(STRING)
	IF (NPNTS.LE.0) CALL MABEND(' NO POINTS FOUND!')


C		FIND THE MEAN AND VARIANCE IN EACH DIMENSION
	DO N = 1,NDIM
	    VARMEAN(N) = 0.0
	    VARSTD(N) = 0.0	
	    DO I = 1,NPNTS
		VARMEAN(N) = VARMEAN(N) + DATA(N,I)
		VARSTD(N) = VARSTD(N) + DATA(N,I)**2
	    ENDDO
	    VARMEAN(N) = VARMEAN(N)/NPNTS
	    VARDATA(N) = (VARSTD(N) - NPNTS*VARMEAN(N)**2)/(NPNTS-1)
	    VARSTD(N) = DSQRT(VARDATA(N))
	    WRITE (STRING,103),'VARIABLE: ', N, 
     +			'   MEAN: ', VARMEAN(N), '   STD: ', VARSTD(N)
103	    FORMAT (A,I2,A,E11.3,A,E11.3)
	    CALL NPRINT(STRING)
	ENDDO
	CALL XVMESSAGE(' ',' ')



C		GET THE NUMBER OF CLUSTERS DESIRED
	CALL XVP('NCLUS',NCLUSPAR,COUNT)
	MINNCLUS = 2
	MAXCLUS = MAXNCLUS
	NCLUSWANT = NINT(NCLUSPAR(1))
	NCLUSFAC = 0.05/NCLUSPAR(2)**2	 ! set up the parabolic energy constant
	NCLUS = NCLUSWANT

	MINSIZE = 3



	IF (.NOT. RESTART) THEN
C	  START WITH NCLUS RANDOMLY PICKED CLUSTERS

	    TRIES = 0
120	    TRIES = TRIES + 1
	    IF (TRIES .GT. 5) CALL MABEND('CAN NOT MAKE ENOUGH CLUSTERS')
C	    SEED = 3923*SECNDS(0.0)	! seed the random number generator
C	    SEED = 3923*GET_SECONDS()   ! Portable version of SECNDS
C    the following is used during portablity testing:
	    SEED = 3923*(25*TRIES)      ! pick NCLUS objects at random
            DO C = 1,NCLUS
                CALL RANGEN(SEED,RANNUM)
                OBJECT = NPNTS*RANNUM + 1
	        DO N = 1,NDIM			! use their location for
	 	    MEAN(N,C) = DATA(N,OBJECT)	!  the seeds for the clusters
	        ENDDO
		NUMBER(C) = 0
	    ENDDO
C		ASSIGN ALL OF THE OBJECTS TO THEIR CLOSEST CLUSTER
	    DO I = 1,NPNTS
	        MINDIST = 1.0E20
	        DO C = 1,NCLUS
		    DIST = DISTFUNC(DATA(1,I),MEAN(1,C))
		    IF (DIST .LT. MINDIST) THEN
		        MINDIST = DIST
		        MINCLUS = C
		    ENDIF
	        ENDDO
	        CLUS(I) = MINCLUS
	        NUMBER(MINCLUS) = NUMBER(MINCLUS) + 1
	    ENDDO
C		MAKE SURE THE CLUSTERS HAVE ENOUGH MEMBERS, OTHERWISE GO BACK
	    DO C = 1,NCLUS
	        IF (NUMBER(C) .LT. MINSIZE) GOTO 120
	    ENDDO
	ENDIF

	CALL XVP('NCYCLE',NCYCLE,COUNT)
	IF (COUNT.EQ.0) NCYCLE = 100
	CALL XVPARM('NITER',NITER,COUNT,DEF,1)
	IF (DEF .EQ. 1) NITER = 4*NPNTS


C		CALCULATE THE STARTING ENERGY AND 
C		    SET UP ALL OF THE INTERMEDIATE CALCULATIONS
	CALL CALC(ENERGY)


C		SET UP CONTROL CONSTANTS

	CALL XVP('TEMP',TEMPX,COUNT)	! get the initial temperature
	IF (COUNT.EQ.0) TEMPX = 0.20
	TEMP0 = TEMPX/NPNTS		! internal temps are per object
	TEMPFAC = 3.0/NCYCLE		! set up the cooling rate

	LUMPFRAC = 4./NPNTS		! how often we try to lump
	SPLITFRAC = 4./NPNTS		! how often we try to split
	NCYCSTOP = 0
	AVGENERGY = ENERGY


	WRITE (STRING,121),'   ENERGY',ENERGY, '   TEMP',TEMP0*NPNTS
	CALL NPRINT (STRING)

C		CYCLE THROUGH THE COOLING CYCLES
	DO CYCLE = 1,NCYCLE
	    WRITE (STRING,117),'CYCLE: ',CYCLE
117	    FORMAT (A,I3)
	    CALL NPRINT (STRING)

	    TEMP = TEMP0 * DEXP(-TEMPFAC*(CYCLE-1))	! lower the temp

	    NCHANGE = 0
C		DO NITER ITERATIONS PER TEMP CYCLE
	    DO ITER = 1,NITER

C		EVERY SO OFTEN TRY LUMPING SOME CLUSTERS
		CALL RANGEN(SEED,RANNUM)
		IF (RANNUM .LT. LUMPFRAC .AND. NCLUS.GT.MINNCLUS) THEN
		    CALL NEWLUMP(CLUS1,CLUS2,NEWENERGY)
		    DELTAE = NEWENERGY - ENERGY
		    IF (DELTAE .LE. MINENERGY) THEN
		        CALL LUMP(CLUS1,CLUS2,ENERGY)
		    ELSE
			NUMT = NUMBER(CLUS1)+NUMBER(CLUS2)
		        BOLTZ = EXP(-DELTAE/(TEMP*NUMT))
			CALL RANGEN(SEED,RANNUM)
		        IF (RANNUM .LT. BOLTZ) THEN
		 	    CALL LUMP(CLUS1,CLUS2,ENERGY)
		        ENDIF
		    ENDIF
		ENDIF

C		EVERY SO OFTEN TRY SPLITTING SOME CLUSTERS
		CALL RANGEN(SEED,RANNUM)
		IF (RANNUM .LT. SPLITFRAC .AND. NCLUS.LT.MAXCLUS) THEN
		    CALL NEWSPLIT(SPLTCLUS,SPLTVAR,NEWENERGY)
		    DELTAE = NEWENERGY - ENERGY
		    IF (DELTAE .LE. MINENERGY) THEN
			CALL SPLIT(SPLTCLUS,SPLTVAR,ENERGY)
		    ELSE
		        BOLTZ = EXP(-DELTAE/(TEMP*NUMBER(SPLTCLUS)))
			CALL RANGEN(SEED,RANNUM)
		        IF (RANNUM .LT. BOLTZ) THEN
			    CALL SPLIT(SPLTCLUS,SPLTVAR,ENERGY)
		        ENDIF
		    ENDIF
		ENDIF


C			PICK AN OBJECT AT RANDOM
220		CONTINUE
		    CALL RANGEN(SEED,RANNUM)
		    OBJECT = NPNTS*RANNUM + 1
		    ORIGCLUS = CLUS(OBJECT)
		IF (NUMBER(ORIGCLUS) .LE. MINSIZE) GOTO 220
C			RANDOMLY ASSIGN IT TO ANOTHER CLUSTER
230		CONTINUE
		    CALL RANGEN(SEED,RANNUM)
		    TOCLUS = NCLUS*RANNUM + 1
		IF (TOCLUS .EQ. ORIGCLUS) GOTO 230

C			CALCULATE THE CHANGE IN ENERGY
		CALL NEWE(OBJECT,TOCLUS,NEWENERGY)
		DELTAE = NEWENERGY - ENERGY
		IF (DELTAE .LE. MINENERGY) THEN		! if lower then accept 
		    CALL RECALC(OBJECT,TOCLUS,ENERGY)	! the change
		    NCHANGE = NCHANGE + 1
		ELSE
		    BOLTZ = EXP(-DELTAE/TEMP)
		    CALL RANGEN(SEED,RANNUM)
		    IF (RANNUM .LT. BOLTZ) THEN		! else sometimes accept
			CALL RECALC(OBJECT,TOCLUS,ENERGY)	! it anyway
			NCHANGE = NCHANGE + 1
		    ENDIF
		ENDIF

	    ENDDO

	    WRITE (STRING,119),'   CHANGES:',NCHANGE,
     +				 '   CLUSTERS:',NCLUS
	    CALL NPRINT (STRING)

	    WRITE (STRING,121),'   ENERGY',ENERGY, '   TEMP',TEMP*NPNTS
	    CALL NPRINT (STRING)

C		IF THE ENERGY HASN'T CHANGED MUCH LATELY THEN STOP
	    IF (ABS(ENERGY-AVGENERGY) .LE. 0.005) THEN
		NCYCSTOP = NCYCSTOP + 1
		IF (NCYCSTOP .GE. 3 ) GOTO 290
	    ELSE
		NCYCSTOP = 0
	    ENDIF
	    AVGENERGY = 0.25*ENERGY + 0.75*AVGENERGY
	ENDDO

290	CONTINUE

119	FORMAT (A,I6,A,I3)
121	FORMAT (A,F9.5,A,F9.5)


C	DONE ANNEALING !



C		CALCULATE STUFF FOR OUTPUT

	DO C = 1,NCLUS			! zero the mean and covariance arrays
	    DO N = 1,NDIM
		MEAN(N,C) = 0.0
		DO M = 1,NDIM
		    COV(M,N,C) = 0.0
		ENDDO
	    ENDDO
	ENDDO
	DO I = 1,NPNTS			! calculate the cluster means
	    C = CLUS(I)
	    DO N = 1,NDIM
		MEAN(N,C) = MEAN(N,C) + DATA(N,I)
	    ENDDO
	ENDDO
	DO C = 1,NCLUS
	  DO N = 1,NDIM
	    MEAN(N,C) = MEAN(N,C)/NUMBER(C)
	  ENDDO
	ENDDO

	DO I = 1,NPNTS			! calculate the cluster covariances
	    C = CLUS(I)
	    DO N = 1,NDIM
	      DO M = 1,NDIM
	          COV(M,N,C)= COV(M,N,C)+(DATA(M,I)-MEAN(M,C))
     +				*(DATA(N,I)-MEAN(N,C))
		ENDDO
	    ENDDO
	ENDDO
	DO C = 1,NCLUS
	  DO N = 1,NDIM
	    DO M = 1,NDIM
	    COV(M,N,C) = COV(M,N,C)/(NUMBER(C)-1)
	    ENDDO
	  ENDDO
	ENDDO




C	OUTPUT THE STATISTICS DATASET

	CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
	RECSIZ = 2+NDIM+1+NDIM*(NDIM+1)/2
	CALL XVOPEN(UNIT,STATUS, 'U_NL',NCLUS, 'U_NS',RECSIZ,
     +	     'OP','WRITE', 'U_FORMAT','FULL', 'O_FORMAT','FULL',' ')

	DO C = 1,NCLUS			! for each cluster
c	    ENCODE(3,111,CLASNAM(6:8)) C	! 8 byte class name
	    ! encode not supported by new compiler, replace by:
	    write(clasnam(6:8),111) c
111	    FORMAT (I3)
	    CLASNAM(1:5) = 'CLASS'
	    INTBUF(3+NDIM) = NUMBER(C)		! number of objects in cluster
	    COUNT = 1
	    DO N = 1,NDIM
		REALBUF(2+N) = SNGL(MEAN(N,C))	! the means in each dim
		DO M = 1,N			! the upper triangular part of
		    REALBUF(3+NDIM+COUNT) = SNGL(COV(M,N,C))	! covariance matrix
		    COUNT = COUNT + 1			! by rows
		ENDDO
	    ENDDO
	    CALL XVWRIT(UNIT,INTBUF,STATUS,' ')	! one image line per cluster
	ENDDO
	CALL XVCLOSE(UNIT,STATUS,' ')

C	OUTPUT THE CLUSTER DATASET
C		CONTAINS THE NDIM COORDINATES AND THE CLUSTER NUMBER FOR
C		EACH DATA POINT (OBJECT) IN REAL MSS FORMAT WITH THE 
C		SAME NUMBER OF SAMPLES PER BAND AS IN THE INPUT IMAGE
	NLO = 1+(NL-SL)/LINC
	NSBO = 1+(NS0-SS)/SINC
	NSO = NSBO*(NDIM+1)
	CALL XVUNIT(UNIT,'OUT',2,STATUS,' ')
	CALL XVOPEN( UNIT, STATUS, 'U_NL',NLO, 'U_NS', NSO,
     +	    'OP','WRITE', 'U_FORMAT','REAL', 'O_FORMAT',INFORMAT,' ')

	I = 0
	DO LINE = 1,NLO
	    DO SAMP = 1,NSO
		BUFFER(SAMP) = 0.0
	    ENDDO
	    DO SAMP = 1,NUMPLIN(LINE)
		I = I + 1
		DO N = 1,NDIM
		    BUFFER(SAMP+(N-1)*NSBO) = SNGL(DATA(N,I))
		ENDDO
		BUFFER(SAMP+NDIM*NSBO) = FLOAT(CLUS(I))
	    ENDDO
	    CALL XVWRIT(UNIT,BUFFER,STATUS,' ')
	ENDDO
	CALL XVCLOSE(UNIT,STATUS,' ')

	RETURN
	END


	SUBROUTINE CALC(E)
C    CALC SETS UP THE INTERMEDIATE CALCULATED VALUES AND CALCULATES THE ENERGY
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	INTEGER	NDIM,NPNTS,NCLUS, N,I,C, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM

	DO C = 1,NCLUS
	    NUMBER(C) = 0
	    DO N = 1,NDIM
		SUM(N,C) = 0.0
		SUMSQ(N,C) = 0.0
	    ENDDO
	ENDDO
	DO N = 1,NDIM
	    SUMPI(N) = 0.0
	ENDDO

C	CALCULATE THE SUM AND SUM OF SQUARES OF ALL OF THE OBJECTS IN EACH CLUS
	DO I = 1,NPNTS
	    C = CLUS(I)
	    NUMBER(C) = NUMBER(C) + 1
	    DO N = 1,NDIM
		SUM(N,C) = SUM(N,C) + DATA(N,I)
		SUMSQ(N,C) = SUMSQ(N,C) + DATA(N,I)**2
	    ENDDO
	ENDDO
C	CALCULATE THE VARIANCE IN EACH DIM FOR EACH CLUSTER 
C	    AND THE SUM OF THE CLUSTER VARIANCES
	DO C = 1,NCLUS
	    DO N = 1,NDIM
		PI(N,C) = SUMSQ(N,C) - SUM(N,C)**2/NUMBER(C)
		SUMPI(N) = SUMPI(N) + PI(N,C)
	    ENDDO
	ENDDO

C	NORMALIZE THE SUM OF VARIANCES IN EACH DIMENSION BY THE TOTAL
C	    VARIANCE OF ALL OF THE DATA IN THAT DIM
C	    AND SUM OVER ALL THE DIMENSIONS
	E = 0.0
	DO N = 1,NDIM
	    E = E + SUMPI(N)/VARDATA(N)
	ENDDO

C	NORMALIZE THE ENERGY ACCORDING TO THE NUMBER OF OBJECTS, THE NUMBER
C	    OF DIMENSIONS AND THE NUMBER OF CLUSTERS SO THAT IT IS
C	    OF ORDER 1 AND IS INDEPENDENT OF THESE QUANTITIES.
C	  ADD IN THE PARABOLIC ENERGY FACTOR FOR THE DESIRED NUMBER OF CLUSTERS
	E = NCLUS**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS-NCLUSWANT)**2

	RETURN
	END



	SUBROUTINE RECALC(OBJECT,TO,E)
C    RECALC MOVES OBJECT TO CLUSTER TO AND RECALCULATES THE INTERMEDIATE
C	VALUES AND THE NEW ENERGY
C	USES THE INTERMEDIATE VALUES TO MINIMIZE THE NUMBER OF CALCULATIONS
C	  REQUIRED TO CALCULATE THE NEW ENERGY
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	INTEGER	OBJECT,FROM,TO
	INTEGER	NDIM,NPNTS,NCLUS, N, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM

	IF (TO .GT. NCLUS)  NCLUS = TO
	FROM = CLUS(OBJECT)
	CLUS(OBJECT) = TO
	E = 0.0
	NUMBER(FROM) = NUMBER(FROM) - 1
	NUMBER(TO) = NUMBER(TO) + 1
	DO N = 1,NDIM
	    SUMPI(N) = SUMPI(N) - PI(N,FROM) - PI(N,TO)
	    SUM(N,FROM) = SUM(N,FROM) - DATA(N,OBJECT)
	    SUM(N,TO) = SUM(N,TO) + DATA(N,OBJECT)
	    SUMSQ(N,FROM) = SUMSQ(N,FROM) - DATA(N,OBJECT)**2
	    SUMSQ(N,TO) = SUMSQ(N,TO) + DATA(N,OBJECT)**2
	    PI(N,FROM) = SUMSQ(N,FROM) - SUM(N,FROM)**2/NUMBER(FROM)
	    PI(N,TO) = SUMSQ(N,TO) - SUM(N,TO)**2/NUMBER(TO)
	    SUMPI(N) = SUMPI(N) + PI(N,FROM) + PI(N,TO)

	    E = E + SUMPI(N)/VARDATA(N)
	ENDDO
	E = NCLUS**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS-NCLUSWANT)**2

	RETURN
	END

	SUBROUTINE NEWE(OBJECT,TO,E)
C    NEWE CALCULATES WHAT THE ENERGY WOULD BE IF OBJECT WAS MOVED TO
C	CLUSTER TO, BUT DOES NOT ACTUALLY MOVE THE OBJECT OR CHANGE
C	THE INTERMEDIATE VALUES
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	INTEGER	NUMF,NUMT
	REAL*8	X0,X2,SUMF,SUMT,SUMSQF,SUMSQT,PIF,PIT,SUMPI0
	INTEGER	OBJECT,FROM,TO
	INTEGER	NDIM,NPNTS,NCLUS, N, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM

	FROM = CLUS(OBJECT)
	E = 0.0
	NUMF = NUMBER(FROM) - 1
	NUMT = NUMBER(TO) + 1
	DO N = 1,NDIM
	    X0 = DATA(N,OBJECT)
	    SUMF = SUM(N,FROM) - X0
	    SUMT = SUM(N,TO) + X0
	    X2 = X0**2
	    SUMSQF = SUMSQ(N,FROM) - X2
	    SUMSQT = SUMSQ(N,TO) + X2
	    PIF = SUMSQF - SUMF**2/NUMF
	    PIT = SUMSQT - SUMT**2/NUMT
	    SUMPI0 = SUMPI(N) + PIF - PI(N,FROM) + PIT - PI(N,TO)
	    E = E + SUMPI0/VARDATA(N)
	ENDDO
	E = NCLUS**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS-NCLUSWANT)**2

	RETURN
	END


	SUBROUTINE LUMP(CLUS1,CLUS2,E)
C    LUMP LUMPS CLUS1 AND CLUS2 TOGETHER AND RECALCULATES EVERYTHING
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	INTEGER	CLUS1,CLUS2
	INTEGER	NDIM,NPNTS,NCLUS, N,I, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM


	NUMBER(CLUS1) = NUMBER(CLUS1) + NUMBER(CLUS2)
	DO N = 1,NDIM
	    SUM(N,CLUS1) = SUM(N,CLUS1) + SUM(N,CLUS2)
	    SUMSQ(N,CLUS1) = SUMSQ(N,CLUS1) + SUMSQ(N,CLUS2)
	    SUMPI(N) = SUMPI(N) - PI(N,CLUS1) - PI(N,CLUS2)
	    PI(N,CLUS1) = SUMSQ(N,CLUS1) - SUM(N,CLUS1)**2/NUMBER(CLUS1)
	    SUMPI(N) = SUMPI(N) + PI(N,CLUS1)
	ENDDO

	DO N = 1,NDIM
	    SUM(N,CLUS2) = SUM(N,NCLUS)
	    SUM(N,NCLUS) = 0.0
	    SUMSQ(N,CLUS2) = SUMSQ(N,NCLUS)
	    SUMSQ(N,NCLUS) = 0.0
	    PI(N,CLUS2) = PI(N,NCLUS)
	    PI(N,NCLUS) = 0.0
	ENDDO
	NUMBER(CLUS2) = NUMBER(NCLUS)
	NUMBER(NCLUS) = 0
	DO I = 1,NPNTS
	    IF (CLUS(I) .EQ. CLUS2)  CLUS(I) = CLUS1
	    IF (CLUS(I) .EQ. NCLUS)  CLUS(I) = CLUS2
	ENDDO
	NCLUS = NCLUS - 1
	E = 0.0
	DO N = 1,NDIM
	    E = E + SUMPI(N)/VARDATA(N)
	ENDDO
	E = NCLUS**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS-NCLUSWANT)**2

	RETURN
	END


	SUBROUTINE NEWLUMP(CLUS1,CLUS2,E)
C    NEWLUMP  CALCULATES WHICH CLUSTERS TO LUMP BASED ON THE MINIMUM
C	MAHALANOBIS DISTANCE BETWEEN CLUSTERS AND CALCULATES WHAT THE
C	NEW ENERGY WOULD BE, BUT DOES NOT ACTUALLY CHANGE ANYTHING
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	REAL*8	SUM0,SUMSQ0, PI0,SUMPI0
	REAL*8	MAHA,MINMAHA
	INTEGER	CLUS1,CLUS2, C1,C2,   NUM0
	INTEGER	NDIM,NPNTS,NCLUS, N, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM

	MINMAHA = 1.0E20
	DO C1 = 1,NCLUS-1
	    DO C2 = C1+1,NCLUS
		MAHA = 0.0
		DO N = 1,NDIM
		    MAHA = MAHA +
     +			 ( SUM(N,C1)/NUMBER(C1) - SUM(N,C2)/NUMBER(C2) )**2
     +			 /( (PI(N,C1) + PI(N,C2))/(NUMBER(C1)+NUMBER(C2)) )
		ENDDO
		IF (MAHA .LT. MINMAHA) THEN
		    MINMAHA = MAHA
		    CLUS1 = C1
		    CLUS2 = C2
		ENDIF
	    ENDDO
	ENDDO


	NUM0 = NUMBER(CLUS1) + NUMBER(CLUS2)
	E = 0.0
	DO N = 1,NDIM
	    SUM0 = SUM(N,CLUS1) + SUM(N,CLUS2)
	    SUMSQ0 = SUMSQ(N,CLUS1) + SUMSQ(N,CLUS2)
	    SUMPI0 = SUMPI(N) - PI(N,CLUS1) - PI(N,CLUS2)
	    PI0 = SUMSQ0 - SUM0**2/NUM0
	    SUMPI0 = SUMPI0 + PI0
	    E = E + SUMPI0/VARDATA(N)
	ENDDO
	E = (NCLUS-1)**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS-1 - NCLUSWANT)**2

	RETURN
	END


	SUBROUTINE SPLIT(SPLTCLUS,SPLTVAR,E)
C    SPLIT SPLITS CLUSTER SPLTCLUS ALONG THE SPLTVAR VARIABLE AND RECALCULATES
C	THE ENERGY AND EVERYTHING
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	INTEGER	SPLTCLUS,SPLTVAR
	INTEGER	NDIM,NPNTS,NCLUS, N,I, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E, SPLTMEAN
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM

	NCLUS = NCLUS + 1
	SPLTMEAN = SUM(SPLTVAR,SPLTCLUS)/NUMBER(SPLTCLUS)
	DO I = 1,NPNTS
	    IF (CLUS(I) .EQ. SPLTCLUS) THEN
		IF (DATA(SPLTVAR,I) .LT. SPLTMEAN) THEN
		    CLUS(I) = NCLUS
		    NUMBER(SPLTCLUS) = NUMBER(SPLTCLUS) - 1
		    NUMBER(NCLUS) = NUMBER(NCLUS) + 1
		    DO N = 1,NDIM
			SUM(N,SPLTCLUS) = SUM(N,SPLTCLUS) - DATA(N,I)
			SUMSQ(N,SPLTCLUS) = SUMSQ(N,SPLTCLUS) - DATA(N,I)**2
			SUM(N,NCLUS) = SUM(N,NCLUS) + DATA(N,I)
			SUMSQ(N,NCLUS) = SUMSQ(N,NCLUS) + DATA(N,I)**2
		    ENDDO
		ENDIF
	    ENDIF
	ENDDO
	DO N = 1,NDIM
	    SUMPI(N) = SUMPI(N) - PI(N,SPLTCLUS)
	    PI(N,SPLTCLUS) = SUMSQ(N,SPLTCLUS)
     +			   - SUM(N,SPLTCLUS)**2/NUMBER(SPLTCLUS)
	    PI(N,NCLUS) = SUMSQ(N,NCLUS)
     +			 - SUM(N,NCLUS)**2/NUMBER(NCLUS)
	    SUMPI(N) = SUMPI(N) + PI(N,SPLTCLUS) + PI(N,NCLUS) 
	ENDDO

	E = 0.0
	DO N = 1,NDIM
	    E = E + SUMPI(N)/VARDATA(N)
	ENDDO
	E = NCLUS**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS-NCLUSWANT)**2

	RETURN
	END


	SUBROUTINE NEWSPLIT(SPLTCLUS,SPLTVAR,E)
C    NEWSPLIT DETERMINES WHICH CLUSTER TO SPLIT ACCORDING TO THE MAXIMUM
C	VARIANCE  AND CALCULATES WHAT THE NEW ENERGY WOULD BE
	IMPLICIT NONE
	INTEGER	MAXNPNTS, MAXNCLUS
	PARAMETER (MAXNPNTS = 50000)
	PARAMETER (MAXNCLUS = 100)
	REAL*8	SUM1(12),SUM2(12),SUMSQ1(12),SUMSQ2(12), SUMPI0, PI1,PI2
	INTEGER	NUM1,NUM2
	INTEGER	SPLTCLUS,SPLTVAR
	INTEGER	NDIM,NPNTS,NCLUS, N,I,C, NCLUSWANT
	INTEGER	NUMBER(MAXNCLUS), CLUS(MAXNPNTS)
	REAL*8	DATA(12,MAXNPNTS), VARDATA(12)
	REAL*8	SUM(12,MAXNCLUS), SUMSQ(12,MAXNCLUS)
	REAL*8	PI(12,MAXNCLUS), SUMPI(12)
	REAL*8	E, SPLTMEAN, CLUSVAR,MAXCLUSVAR
	REAL	NCLUSFAC
	COMMON	/CLUS/	NPNTS,NCLUS, VARDATA, DATA, NUMBER, CLUS,
     +		SUM,SUMSQ, PI, SUMPI, NCLUSWANT,NCLUSFAC
	COMMON  /CLUS1/ NDIM


	MAXCLUSVAR = -1.0E20
	DO C = 1,NCLUS
	    DO N = 1,NDIM
		CLUSVAR = PI(N,C)/VARDATA(N)
		IF (CLUSVAR .GT. MAXCLUSVAR) THEN
		    MAXCLUSVAR = CLUSVAR
		    SPLTCLUS = C
		    SPLTVAR = N
		ENDIF
	    ENDDO
	ENDDO

	NUM1 = 0
	NUM2 = 0
	DO N = 1,NDIM
	    SUM1(N) = 0
	    SUMSQ1(N) = 0
	    SUM2(N) = 0
	    SUMSQ2(N) = 0
	ENDDO
	SPLTMEAN = SUM(SPLTVAR,SPLTCLUS)/NUMBER(SPLTCLUS)
	DO I = 1,NPNTS
	    IF (CLUS(I) .EQ. SPLTCLUS) THEN
		IF (DATA(SPLTVAR,I) .LT. SPLTMEAN) THEN
		    NUM1 = NUM1 + 1
		    DO N = 1,NDIM
			SUM1(N) = SUM1(N) + DATA(N,I)
			SUMSQ1(N) = SUMSQ1(N) + DATA(N,I)**2
		    ENDDO
		ELSE
		    NUM2 = NUM2 + 1
		    DO N = 1,NDIM
			SUM2(N) = SUM2(N) + DATA(N,I)
			SUMSQ2(N) = SUMSQ2(N) + DATA(N,I)**2
		    ENDDO
		ENDIF
	    ENDIF
	ENDDO

	E = 0.0
	DO N = 1,NDIM
	    SUMPI0 = SUMPI(N) - PI(N,SPLTCLUS)
	    PI1 = SUMSQ1(N) - SUM1(N)**2/NUM1
	    PI2 = SUMSQ2(N) - SUM2(N)**2/NUM2
	    SUMPI0 = SUMPI0 + PI1 + PI2
	    E = E + SUMPI0/VARDATA(N)
	ENDDO
	E = (NCLUS+1)**(2./NDIM)* E/(NPNTS*NDIM)
     +		 + NCLUSFAC*(NCLUS+1 - NCLUSWANT)**2

	RETURN
	END


	REAL FUNCTION DISTFUNC(X1,X2)
C    RETURNS EUCLIDEAN DISTANCE BETWEEN X1 AND X2
	IMPLICIT INTEGER (A-Z)
	REAL*8  X1(12),X2(12), D
	COMMON /CLUS1/ NDIM

	D = 0
	DO N = 1,NDIM
	    D = D + (X1(N)-X2(N))**2
	ENDDO
	DISTFUNC = SQRT(D)
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create clusan.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM clusan

   To Create the build file give the command:

		$ vimake clusan			(VMS)
   or
		% vimake clusan			(Unix)


************************************************************************/


#define PROGRAM	clusan
#define R2LIB

#define MODULE_LIST clusan.f

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
$ create clusan.pdf
PROCESS HELP=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING COUNT=2
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM MSS TYPE=INTEGER VALID=2:50
PARM BANDS TYPE=INTEGER COUNT=(0,2:12) DEFAULT=--
PARM INCR INTEGER DEFAULT=1
PARM LINC INTEGER DEFAULT=1
PARM SINC INTEGER DEFAULT=1
PARM THRESH REAL COUNT=0:1 DEFAULT=--
PARM NCLUS TYPE=REAL COUNT=2
PARM NCYCLE TYPE=INTEGER DEFAULT=50
PARM NITER TYPE=INTEGER DEFAULT=0
PARM TEMP TYPE=REAL DEFAULT=0.2
PARM RESTART TYPE=KEYWORD COUNT=0:1 VALID=(RESTART,NORESTART) DEFAULT=NORESTART
END-PROC
.TITLE
VICAR Program clusan  
.HELP

PURPOSE

    clusan is a clustering algorithm that uses the simulated annealing
optimization technique to find the best cluster partition.  The input is
multidimensional data in MSS format.  The data are grouped into clusters 
which have the minimum scaled variance.  This clustering algorithm is
designed for finding the best cluster partition of a small number of
multidimensional data points; the maximum is 50000 points.


EXECUTION EXAMPLES

clusan DATA.MSS  (DATA.SDS,DATA.CLS)  MSS=6 BANDS=(2,3,5,6) NCLUS=(6,4) +
          INCR=10 SIZE=(100,100,500,500) THRESH=10.

The input data are in MSS format; i.e. the bands are appended together
in the sample direction using the program MSS.  The pixels can be in any
format since they are converted to real in the program.  The MSS parameter 
specifies how many bands are in the input file.  

The first output file is a statistics dataset (like those produced by 
USTATS, etc)  which contains the number of data points, the mean vector, 
and the covariance matrix for each cluster.  This file can be used by
programs CLUSTEST and DENDSTAT.

The second output file contains the input data for those bands that were 
used in the clustering and another band which gives the cluster number 
that the pixel was assigned to.  This file is in MSS format and has the 
same pixel format as the input file.

The BANDS parameter specifies which bands in the input file will be
used for the clustering.  This allows the user to try clustering various
combinations of bands without having to make up a new input dataset each
time.  There must be at least two, but no more than twelve bands.

The NCLUS parameter guides the algorithm in choosing the number of 
clusters (see theory of operation).  The first number is the desired number
of clusters and the second number is about how many clusters the algorithm
can stray from the desired number.

The INCR (or LINC/SINC) parameter is used to cut down on the number of
points to be used (since the maximum number is 50000).  This is also the
purpose of the SIZE parameter.  

The THRESH parameter sets a minimum value for valid pixels (objects).
Note that in the cluster dataset output all pixels on a line that lie
below THRESH are moved to the end of the line and assigned the value 0.


clusan DATA.CLS  (DATA2.SDS,DATA2.CLS)  MSS=5 BANDS=(1,2,3,4) NCLUS=(8,2) +
			'RESTART  NCYCLE=25 NITER=100 TEMP=0.5

This example which uses the remaining parameters shows how to restart a 
clustering run.  To restart the run, the RESTART keyword is specified
and the second output file from the previous run is used for the input
file.  Note that the bands in the new input file are in different positions
than the bands in the old input file, and that the INCR and SIZE parameters
need not be respecified, as the file has been compressed accordingly.
MSS is now 5, as the input contains only the 4 bands that were processed
in the first run, plus an additional pseudo-band containing cluster numbers.

The last three parameters will not usually have to be specified, but allow
some control over the optimization algorithm (see theory of operation).
The NCYCLE parameter is the maximum number of temperature cycles the 
algorithm will perform.  If it is large then the algorithm will lower the
temperature very slowly, but if it is too small the system will be
quenched and will not be at a true minimum energy configuration.
The algorithm will stop before doing NCYCLE temperature cycles if
the system is in a stable state (i.e. the energy is no longer decreasing).

The NITER parameter specifies how many iterations are to be done for
each cooling cycle.  The default is to do 4 times the number of data points,
so that on average each point will be considered about four times per
cycle.

The TEMP parameter gives the initial temperature to start the system
off with.  The initial temperature should be of the order of unity.
A higher starting temperature will make the system behave more randomly
at first.
.PAGE
RESTRICTIONS

1. The maximum number of data points is 50,000.  (The LINC/SINC parameters
may be used to reduce the number of points used from the input image.)

2. The maximum number of clusters is 100.

3. The number of bands to cluster must be between 2 and 12 inclusive.

4. The maximum sample size of the input MSS file is 60,000 samples.

5. The maximum number of lines that are used (i.e., (NL-SL)/LINC+1)
  in the input MSS file is 10,000.
.page
THEORY OF OPERATION

    Clustering is the technique of grouping data points (or objects) into a
number of groups based on similarities in the objects' measured attributes.
While for a small number of attributes (one or two variables) graphing the
data points can be used to easily find the clusters, as the number of attri-
butes becomes larger visual methods become impossible.  Many computer
algorithms exist which cluster data points in any number of dimensions.
One approach to the problem of clustering is that of optimization.  An 
objective function which measures the degree of clusterness is maximized over
all possible partitions of the data points into clusters.  Of course, a wide
range of such functions could be imagined.  There are also a wide variety of
optimization methods available to maximize or minimize functions.  Most such
methods are inappropriate because of the nature of clustering.  Clustering
involves discrete movements of objects between clusters, so that derivatives
are not a particularly useful concept.  Also there are many local minima
for optimization algorithms to get caught in.  One new algorithm which over-
comes these difficulties is called simulated annealing.
    Simulated annealing is analogous to the cooling and crystalization process
of a crystalline solid.  The objective function to be minimized is called the
energy of the system.  An object is randomly moved from one cluster to another
in analogy with the random motions of the atoms in a crystal.  If the energy
is thus decreased the change is accepted.  If the energy is increased the
change is still accepted with a probability depending on the Boltzmann factor:
exp(-deltaE/Temp).  Thus increases in the energy comparable to the temperature
still have a good chance of being accepted while large increases in the energy
have very little chance of being accepted.  This allows the algorithm to
rattled its way out of local minima.  The temperature is at first set high so
that a lot of random motion occurs.  It is then gradually lowered until the
system freezes.  How exactly the temperature is lowered is called the
annealing schedule.  There are many possibilities for the annealing schedule,
but the one used in this implementation is that of Newton's law of cooling,
i.e. the temperature decreases exponentially:
  Temp = InitialTemp * exp(-const*Time) .

    The objective function in this program is the normalized average of the
cluster variances.  The variances in each attribute dimension are scaled by
the total variance of all of the data points in that dimension.  Thus the
results are independent of all scalings and translations of the attributes.
The variances of the clusters are averaged by weighting by the number of
points in each cluster.  Finally the average of the variances is scaled by
the number of attributes and scaled according to the number of clusters, so
that the energy function is invariant and can be compared between configura-
tions with different number of clusters.  The energy should always be of
order one.  The energy function treats all of the attributes the same and will
tend to make hyperspherical clusters even if the clusters should be long and
stringy.

    The algorithm also uses the simulated annealing technique to find the
number of clusters.  Every once in a while, it will try lumping two clusters
together or splitting one cluster up into two and will do the same Boltzmann
calculation to determine whether to accept the change.  The algorithm can be
guided to give about a certain number of clusters with the NCLUS parameter.
The first value gives the number of clusters desired and the second value gives
a rough number of how many in either direction is acceptable.  NCLUS=(6,2) 
means "I want 6 clusters give or take about 2".  This feature is implemented
by adding a parabolic term to the energy function, i.e. a term whose value
goes as the square of the difference between the actual number of clusters and
the desired number.



OPERATION STRATEGY

    The program starts out by randomly selecting the specified number of 
objects and setting all of the objects to the closest cluster.  Run the 
program a few times on the same data to see if same minimum energy is 
reached.  If you want the program to decide entirely by itself the best 
number of clusters, make NCLUS(2) have some large value.  If the program 
has trouble converging to the same result it probably means that the data 
is not particularly clustered.  Generally the program quickly and reliably 
converges in cases where the data has well defined groupings.


PRECISION:   5-Sept-94
  1.	This portable version uses double precision,which provides a 
	capability to adjust the sensitivity to changes in the "ENERGY"
	For portablity testing, this version ignores delta "ENERGY" 
	changes less than .0000005.

  2.	CLUSTEST has always used double precision. The CLUSAN output 
 	statistical files which are used by CLUSTEST are single precision
 	(REAL*4).  The stated precision (from the VAX FORTRAN manuals) for
 	REAL*4 is "approximately 2**23, that is, typically seven decimal
 	digits".  In order to eliminate test differences (VAX vs. UNIX)
 	caused by reading the single precision values into double precision
 	variables, it is necessary to truncate the input values to six
 	decimal digits.  The truncation is currently being implemented by
 	the "ported' version of CLUSTEST.

  3.	The changes made for porting cause CLUSAN and CLUSTEST to perform
 	differently than the original baseline versions.  The main difference
 	appears to be the result of specifying default values for TEMP0 and
 	NCYCLES.  Not specifing default values for these parameters produce
 	inconsistent results between the different platforms.  For comparison
 	testing purposes, the original program was modified to use the same 
 	random number generator with the same seed value as the new ported 
 	versions.  However, even with these changes the original program does
 	not produce exactly the same results when run mutliple times.


REFERENCES

Optimization by Simulated Annealing
	Kirkpatrick, Gelatt, and Vecchi, 
	Science, May 13, 1983 ,  No. 220

Cluster Analysis For Applications,
	Anderberg, Michael R.,  Academic Press  (1973).

Classification: Methods for the Exploratory Analysis of Multivariate Data,
	Gordon, A. D.,  Chapman and Hall (1981).

.PAGE
HISTORY

Original Programmer:   Frank Evans       November 1985
Cognizant Progrmmer:   Frank Evans

Revisions:
  1:  6-April-1987,  L.W.Kamp,  added INCR/LINC/SINC & THRESH parameters, fixed
				SS/NS, increased BUFFER size.
  2:  5-Sept-1994 ,  C.R. Schenk (CRI) Ported to UNIX & changed to double
                                precision. 

.LEVEL1
.VARIABLE INP
Input attribute file
.VARIABLE OUT
Output statistics data set
and cluster file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample (per band !)
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples (per band !)
.VARIABLE MSS
Number of bands in MSS format
.VARIABLE BANDS
List of bands (variables)
to cluster
.VARIABLE INCR
Line/sample increment
.VARIABLE LINC
Line increment
.VARIABLE SINC
Sample increment
.VARIABLE THRESH
Minimum valid DN
.VARIABLE NCLUS
The number of clusters desired
The range in number of clusters
.VARIABLE NCYCLE
The number of temperature cycles
.VARIABLE NITER
The number of iterations per 
cycle
.VARIABLE TEMP
The initial temperature
.VARIABLE RESTART
'RESTART to restart a clustering

.LEVEL2
.VARIABLE INP
The input data are in MSS format; i.e. the bands are appended together
in the sample direction using the program MSS.  The pixels can be in any
format since they are converted to real in the program.  The MSS parameter 
specifies how many bands are in the input file.  
.VARIABLE OUT
    The first output file is a statistics dataset (like those produced by 
USTATS, etc)  which contains the number of data points, the mean vector, 
and the covariance matrix for each cluster. 
 
    The second output file is intended for the RESTART option.  It contains 
the input data for those bands that were used in the clustering and another 
band which gives the cluster number that the pixel was assigned to.  This 
file is in MSS format and has the same pixel format as the input file.  If 
SIZE and/or INCR was specified on the first run, then the data will have 
been compressed accordingly and these parameters must not be respecified on 
a restart.

.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample.  This is PER BAND, i.e., it does not apply (as is standard
for the MIPL SS parameter) to the entire input image, but (because the image
is in this case in MSS format) to each band separately.
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples.  This is PER BAND, i.e., it does not apply (as is standard
for the MIPL NS parameter) to the entire input image, but (because the image
is in this case in MSS format) to each band separately.
.VARIABLE MSS
The MSS parameter specifies how many bands are in the input MSS
format file.  
.VARIABLE BANDS
The BANDS parameter specifies which bands in the input file will be
used for the clustering.  This allows the user to try clustering various
combinations of bands without having to make up a new input dataset each
time.  There must be at least two, but no more than twelve bands.

Default is to use all bands in the input file in the order in which they
occur in the file.  However, if RESTART has been specified, then the last
band is not used.  I.e., default is:
  BANDS = (1,2,...,MSS)  if not RESTART,
  BANDS = (1,2,...,MSS-1)  if RESTART.
.VARIABLE INCR
Increment in line and sample direction.  This parameter specifies both
LINC and SINC, q.v.
.VARIABLE LINC
Increment in the line direction.  This parameter specifies that only
every LINCth line (starting with SL) will be used to select objects from
the image for the algorithm.

Default = 1 (every line).
.VARIABLE SINC
Increment in the sample direction.  This parameter specifies that only
every SINCth sample (starting with SS) will be used to select objects from
the image for the algorithm.

Default = 1 (every sample).
.VARIABLE THRESH
THRESH specifies the minimum DN value that is to be accepted as a valid
pixel.  If an object has a DN less than THRESH in any band (of those 
specified by the BAND parameter), then it is rejected.

The default is to not do thresholding; all pixels in size region are used.

In the second output file, all pixels on a given line that have been rejected
are written at the end of that line, with 0 DN.  Hence THRESH does not need 
to be specified on a RESTART, even if it was specified on the first run.

.VARIABLE NCLUS
The NCLUS parameter guides the algorithm in choosing the number of 
clusters (see theory of operation).  The first number is the desired number
of clusters and the second number is about how many clusters the algorithm
can stray from the desired number.
.VARIABLE NCYCLE
The NCYCLE parameter is the maximum number of temperature cycles the 
algorithm will perform.  If it is large then the algorithm will lower the
temperature very slowly, but if it is too small the system will be
quenched and will not be at a true minimum energy configuration.
The default will usually be adequate.
.VARIABLE NITER
The NITER parameter specifies how many iterations are to be done for
each cooling cycle.  The default is to do 4 times the number of data points,
so that on average each point will be considered about four times per
cycle.  The default will usually be adequate.
.VARIABLE TEMP
The TEMP parameter gives the initial temperature to start the system
off with.  The initial temperature should be of the order of unity.
A higher starting temperature will make the system behave more randomly
at first.  The default will usually be adequate.
.VARIABLE RESTART
To restart the run, the RESTART keyword is specified and the second output 
file from the previous run is used for the input file.  Note that the 
bands in the new input file are in different positions than the bands in 
the old input file and the input includes an additional band containing
cluster numbers, hence MSS will in general be different than on the first
run.  If SIZE or INCR (or equivalent parameters) were specified in the 
first run they must not be specified on a RESTART, as the data will have 
been compressed accordingly.
.END
Ox$ Return
$ Return
$!#############################################################################
$Test_File:
$ create tstclusan.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
!local DATA string
!	Make three two dimensional gaussian clusters and put them in MSS format
!       The following test data should already exist in current directory,
!       otherwise it will be created in the current default directory.
!dcl define/nolog gaussfile "DATA"
!dcl if f$search("gausnois.data").eqs."" then define/nolog gaussfile "NODATA"
!translog gaussfile DATA
!if (DATA = "NODATA")
!   WRITE "!!!!!! Cannot find gausnois.data file, creating new one."
   gausnois FORMAT=REAL NL=10 NS=50 SEED=784374261 SIGMA=1 MEAN=0 OUT=a
   gausnois FORMAT=REAL NL=10 NS=50 SEED=353227180 SIGMA=1 MEAN=2 OUT=b
   gausnois FORMAT=REAL NL=10 NS=50 SEED=627591341 SIGMA=1 MEAN=-2 OUT=c
   gausnois FORMAT=REAL NL=10 NS=50 SEED=261458087 SIGMA=1 MEAN=0 OUT=d
   gausnois FORMAT=REAL NL=10 NS=50 SEED=466143364 SIGMA=1 MEAN=2 OUT=e
   gausnois FORMAT=REAL NL=10 NS=50 SEED=313795670 SIGMA=0.5 MEAN=0 OUT=f
   mss (a,b) g
   mss (c,d) h
   mss (e,f) i
   append inp=(g,h,i) out=gausnois.data

!	Cluster the data and look at the clusters
  clusan gausnois.data  (sds,clus)  mss=2  nclus=(4,3)
  clustest  sds  level=4 'maha

  clusan gausnois.data  (sds,clus)  mss=2 thresh=-2 incr=2  nclus=(3,1)
  clustest  sds  level=4 'maha

end-proc
$ Return
$!#############################################################################
