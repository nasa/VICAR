$!****************************************************************************
$!
$! Build proc for MIPL module eigen
$! VPACK Version 1.8, Tuesday, December 09, 2003, 16:32:08
$!
$! Execute by entering:		$ @eigen
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
$ write sys$output "*** module eigen ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to eigen.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("eigen.imake") .nes. ""
$   then
$      vimake eigen
$      purge eigen.bld
$   else
$      if F$SEARCH("eigen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake eigen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @eigen.bld "STD"
$   else
$      @eigen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create eigen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack eigen.com -
	-s eigen.f -
	-i eigen.imake -
	-p eigen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create eigen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	This is a full rewrite for the procedure EIGEN. It is based upon the
C	VICAR program DESTRETCH, which itself had been based upon the VICAR
C	program EIGENVEC. This version combines the steps of the previous EIGEN
C	procedure (the programs EIGENVEC and XFORM) into a single process.
C
C	The permitted number of input/output channels has been increased to 300.
C	BSQ and BIL formats are now supported, while separate single channel
C	files, MSS format files, and non-image data input via the parameter
C	DATA are no longer supported. The method for rescaling the output has
C	also changed.  Rather than selecting a target saturation level (via the
C	PERCENT, LPERCENT, HPERCENT, SPREAD, and CENTER parameters) target mean
C	and standard deviation values are selected (via the MEAN and SIGMA
C	parameters).
C
C	The printed output of the statistical data used in EIGEN can now be
C	optionally summarized or suppressed.
C
C     8 Dec 03    ...rea...  Initial release.
C                            
	IMPLICIT NONE
	INTEGER NDIM,NAREAS,MAXSAMP,IBUFSIZE
	PARAMETER (NDIM=300,NAREAS=200,MAXSAMP=4000)
	PARAMETER (IBUFSIZE=MAXSAMP*NDIM)
C
	REAL*8 SUMX(NDIM),SUMXY(NDIM*NDIM),OMATRIX(NDIM,NDIM)
	REAL*8 EVEC(NDIM,NDIM),EVAL(NDIM)
	REAL*8 CHAN_MEAN(NDIM),CHAN_SIGMA(NDIM)
	REAL DATA(IBUFSIZE),OUTBUF(MAXSAMP),OFFSET(NDIM),BUF(NDIM*NDIM)
	REAL TMATRIX(NDIM,NDIM),XMEAN,SIGMA,EXCLUDE
	INTEGER NBAND(NDIM),IOUT(NDIM),ISUBAREA(NAREAS)
	INTEGER ISL,ISS,NL,NS,NLIN,NSIN,ISB,NB,NBIN,INC,ICNT,LOOP
	INTEGER INUNIT,IOUTUNIT,ISTATUS,NI,NO,IDEF,I,J,K,K2,K3,M
	INTEGER NUM,NPIX,IDUMMY1,IDUMMY2,IDUMMY3,N_AREA_PARS,IFMT
	CHARACTER*133 PR
	CHARACTER*80 SAVE
	CHARACTER*15 PR2(8)
	CHARACTER*4 FMT
	CHARACTER*3 ORG
	LOGICAL XVPTST,CORR,QEXCL,QSAVE,QPRINT,QSUMMARY
C
	CALL XVMESSAGE('EIGEN version 08-December-2003',' ')
C								      open input
	CALL XVUNIT(INUNIT,'INP',1,ISTATUS,' ')
	CALL XVOPEN(INUNIT,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_FORMAT','REAL',' ')
C						   check for valid input formats
C
	CALL XVGET(INUNIT,ISTATUS,'ORG',ORG,'FORMAT',FMT,' ')
	IF (ORG .EQ. 'BIP') THEN
	    CALL XVMESSAGE('BIP format is not currently supported',' ')
	    CALL ABEND
	ENDIF
	IF (FMT .EQ. 'BYTE') THEN
	    IFMT = 1
	ELSE IF (FMT .EQ. 'HALF') THEN
	    IFMT = 2
	ELSE IF (FMT .EQ. 'FULL') THEN
	    IFMT = 3
	ELSE IF (FMT .EQ. 'REAL') THEN
	    IFMT = 4
	ELSE
	    WRITE (PR,50) FMT
   50	    FORMAT(A4,' pixel format is not currently supported')
	    CALL XVMESSAGE(PR,' ')
	    CALL ABEND
	END IF
C
C						  get list of input bands to use
	CALL XVBANDS(ISB,NB,NBIN)
	CALL XVPARM('USEBANDS',NBAND,NI,IDEF,NDIM)
	IF (NI .EQ. 0) THEN
	    IF (NBIN .GT. NDIM) THEN
		WRITE (PR,100) NDIM
  100		FORMAT('The maximum number of input channels to use is',
     +			I4)
		CALL XVMESSAGE(PR,' ')
		CALL ABEND
	    END IF
	    NI = NBIN
	    DO I=1,NI
		NBAND(I) = I
	    END DO
	ELSE
	    DO I=1,NI
		IF (NBAND(I).GT.NBIN .OR. NBAND(I).LE.0) THEN
		    CALL XVMESSAGE('Invalid BAND parameter',' ')
		    CALL ABEND
		ENDIF
	    END DO
	END IF
C							        get parameters
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF (NI*NSIN .GT. IBUFSIZE) THEN
	    CALL XVMESSAGE(' Work buffer is too small',' ')
	    CALL ABEND
	ENDIF
C								inc
	CALL XVPARM('INC',INC,ICNT,IDEF,1)
C								area
	CALL XVPARM('AREA',ISUBAREA,N_AREA_PARS,IDEF,NAREAS)
	IF (N_AREA_PARS.EQ.0) THEN
	    ISUBAREA(1) = ISL
	    ISUBAREA(2) = ISS
	    ISUBAREA(3) = NL
            ISUBAREA(4) = NS
	    N_AREA_PARS = 4
	END IF
C								covariance
C								mean, sigma
	CORR = .NOT. XVPTST('COV')
	CALL XVPARM('MEAN',XMEAN,ICNT,IDEF,1)
	CALL XVPARM('SIGMA',SIGMA,ICNT,IDEF,1)
C								exclude
	CALL XVPARM('EXCLUDE',EXCLUDE,ICNT,IDEF,1)
	QEXCL = ICNT .NE. 0
C								xform_parms
	CALL XVPCNT('XFORM_PARMS',NUM)
	IF (NUM .GT. 0) THEN
	    QSAVE = .TRUE.
	    CALL XVPARM('XFORM_PARMS',SAVE,ICNT,IDEF,1)
	ELSE
	    QSAVE = .FALSE.
	END IF
C								output_pcs
	CALL XVPARM('OUTPUT_PCS',IOUT,NO,IDEF,NDIM)
	IF (NO .EQ. 0) THEN
	    NO = NI
	    DO I=1,NO
		IOUT(I) = I
	    END DO
	ELSE
	    DO I=1,NO
		IF (IOUT(I).GT.NI .OR. IOUT(I).LE.0) THEN
		    CALL XVMESSAGE('Invalid OUTPUT_PCS parameter',' ')
		    CALL ABEND
		ENDIF
	    END DO
	END IF
C								print level
	QPRINT = XVPTST('ALL')
	QSUMMARY = XVPTST('SUMMARY') .OR. QPRINT
C							       gather statistics
	CALL ZIA(SUMX,2*NI)
	CALL ZIA(SUMXY,2*NI*NI)
	NPIX = 0
	IF (QSUMMARY) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' Area(s) sampled for statistics:',' ')
	END IF
	DO I=1,N_AREA_PARS,4
	    IF (QSUMMARY) THEN
		WRITE (PR,150) ISUBAREA(I),ISUBAREA(I+1),ISUBAREA(I+2),
     +			       ISUBAREA(I+3)
  150		FORMAT(2X,4I6)
		CALL XVMESSAGE(PR,' ')
	    END IF
	    IF (QEXCL) THEN
		CALL Z_GATHER_STATS(INUNIT,NI,NBAND,EXCLUDE,ISUBAREA(I),
     +			      ISUBAREA(I+1),ISUBAREA(I+2),ISUBAREA(I+3),
     +			      INC,DATA,NPIX,SUMX,SUMXY)
	    ELSE
		CALL GATHER_STATS(INUNIT,NI,NBAND,ISUBAREA(I),
     +			     ISUBAREA(I+1),ISUBAREA(I+2),ISUBAREA(I+3),
     +			     INC,DATA,NPIX,SUMX,SUMXY)
	    END IF
	END DO
C								report INC, NPIX
C
	IF (QSUMMARY) THEN
	    WRITE (PR,200) INC
  200	    FORMAT('      INC =',I3)
	    CALL XVMESSAGE(PR,' ')
	    WRITE (PR,250) NPIX
  250	    FORMAT('      PIXELS USED =',I10)
	    CALL XVMESSAGE(PR,' ')
	END IF
	IF (NPIX .LE. 1) THEN
	    CALL XVMESSAGE(
     +	    'Inadequate number of pixels in statistics gathering',' ')
	    CALL ABEND
	ENDIF
C					       compute means and cov/corr matrix
	CALL COMPUTE_STATS(NI,NPIX,SUMX,SUMXY,CORR,QPRINT,QSUMMARY,
     +			   CHAN_MEAN,CHAN_SIGMA,OMATRIX)
C						compute principal component
C						eigenvalues and eigenvectors
	CALL JACOBI(OMATRIX,NI,EVAL,EVEC,LOOP)
	IF (QSUMMARY) THEN
	    CALL XVMESSAGE(' ',' ')
	    WRITE (PR,275) LOOP
  275	    FORMAT('Jacobi Routine needed',I3,' iterations')
	    CALL XVMESSAGE(PR,' ')
	END IF
C						sort eigenvalues
	I = 1
	J = 2
	DO WHILE (J .LE. NI)
	    IF (EVAL(J) .GT. EVAL(I)) CALL SWAP(NI,EVEC(1,I),EVEC(1,J),
     +						EVAL(I),EVAL(J))
	    J = J + 1
	    IF (J. GT. NI) THEN
		I = I+1
		J = I+1
	    END IF
	END DO
C						print eigenvalues and vectors
	IF (QPRINT) THEN
	    CALL XVMESSAGE('      Eigen',' ')
	    CALL XVMESSAGE('      Value             Eigenvector',' ')
	    DO I=1,NI
		K = 1
		K2 = MIN(NI,10)
		IF (EVAL(I).GE.0.001 .AND. EVAL(I).LT.100000.0) THEN
		    WRITE (PR,300) I,EVAL(I),(EVEC(J,I),J=K,K+K2-1)
  300		    FORMAT(I3,F10.4,10F12.5)
		ELSE
		    WRITE (PR,320) I,EVAL(I),(EVEC(J,I),J=K,K+K2-1)
  320		    FORMAT(I3,1P,E10.3,0P,10F12.5)
		END IF
		CALL XVMESSAGE(PR,' ')
		K = K + K2
		DO M=11,NI,10
		    K3 = MIN(NI-M+1,10)
		    WRITE (PR,400) (EVEC(J,I),J=K,K+K3-1)
  400		    FORMAT(13X,10F12.5)
		    CALL XVMESSAGE(PR,' ')
		    K = K + K3
		END DO
	    END DO
	ELSE IF (QSUMMARY) THEN
	    CALL XVMESSAGE(' Eigenvalues:',' ')
	    DO I=1,NI,8
		K = MIN(NI,I+7)
		K2 = 0
		DO J=I,K
		    K2 = K2 + 1
		    IF (EVAL(J).GE.0.0001 .AND. EVAL(J).LT.1.0E7) THEN
			WRITE (PR2(K2),410) EVAL(J)
  410			FORMAT(F15.6)
		    ELSE
			WRITE (PR2(K2),420) EVAL(J)
  420			FORMAT(1P,E15.6)
		    END IF
		END DO
		WRITE (PR,430) (PR2(J),J=1,K2)
  430		FORMAT(8A15)
		CALL XVMESSAGE(PR,' ')
	    END DO
	END IF
C						Compute and report the final
C						transformation matrix.
C
	CALL COMPUTE_TRANSFORM(NI,NO,IOUT,EVAL,EVEC,CHAN_MEAN,
     +			     CHAN_SIGMA,CORR,XMEAN,SIGMA,TMATRIX,OFFSET)
	IF (QPRINT) CALL REPORT_TRANSFORM(NI,NO,TMATRIX,OFFSET)
C						Save the transformation matrix,
C						if requested
	IF (QSAVE) THEN
	    NUM = 0
	    DO I=1,NO
		DO J=1,NI
		    NUM = NUM + 1
		    BUF(NUM) = TMATRIX(J,I)
		END DO
	    END DO
	    CALL XVPOPEN(ISTATUS,IDUMMY1,IDUMMY2,SAVE,'SA',IDUMMY3)
	    CALL XVPOUT(ISTATUS,'MATRIX',BUF,'REAL',NUM)
	    DO I=1,NO
		BUF(I) = 1.0
	    END DO
	    CALL XVPOUT(ISTATUS,'GAIN',BUF,'REAL',NI)
	    CALL XVPOUT(ISTATUS,'OFFSET',OFFSET,'REAL',NI)
	    CALL XVPCLOSE(ISTATUS)
	END IF
C						   check for existence of output
	CALL XVPCNT('OUT',ICNT)
	IF (ICNT .NE. 0) THEN
C								open output
	    CALL XVUNIT(IOUTUNIT,'OUT',1,ISTATUS,' ')
	    CALL XVOPEN(IOUTUNIT,ISTATUS,'U_NL',NL,'U_NS',NS,'U_NB',NO,
     +			'OPEN_ACT','SA','IO_ACT','SA','U_ORG','BIL',
     +			'U_FORMAT','REAL','OP','WRITE',' ')
C
C						Perform the transformation and
C						write output
C
	    CALL DO_TRANSFORM(INUNIT,IOUTUNIT,NBAND,ISL,ISS,NL,NS,NI,NO,
     +			      IFMT,TMATRIX,OFFSET,DATA,OUTBUF)
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE Z_GATHER_STATS(INUNIT,NI,NBAND,EXCLUDE,ISL,ISS,NL,NS,
     +				  INC,DATA,NPIX,SUMX,SUMXY)
C
C	This routine is used only if pixels are to be excluded.
C
C	This routine accumulates the sums needed to compute the necessary
C	statistics. In each call, it passes through one area, as defined
C	by the area parameter.
C
	IMPLICIT NONE
	REAL*8 SUMX(NI),SUMXY(NI,NI)
	REAL EXCLUDE,DATA(NS,NI)
	INTEGER INUNIT,NI,NBAND(NI),ISL,ISS,NL,NS,INC,NPIX,IEL,IBAND
	INTEGER JBAND,LINE,ISAMP,ISTAT
C
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL,INC
C								read in data
	    DO IBAND=1,NI
		CALL XVREAD(INUNIT,DATA(1,IBAND),ISTAT,'LINE',LINE,
     +			 'SAMP',ISS,'NSAMPS',NS,'BAND',NBAND(IBAND),' ')
	    END DO
C								update sums
	    DO ISAMP = 1,NS,INC
		DO IBAND=1,NI
		    IF (DATA(ISAMP,IBAND) .NE. EXCLUDE) GO TO 100
		END DO
		GO TO 200
  100		CONTINUE
		DO IBAND = 1,NI
		    DO JBAND = 1,IBAND
			SUMXY(JBAND,IBAND) = SUMXY(JBAND,IBAND) +
     +				     DATA(ISAMP,JBAND)*DATA(ISAMP,IBAND)
		    END DO
		    SUMX(IBAND) = SUMX(IBAND) + DATA(ISAMP,IBAND)
		END DO
		NPIX = NPIX + 1
  200		CONTINUE
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GATHER_STATS(INUNIT,NI,NBAND,ISL,ISS,NL,NS,INC,DATA,
     +				NPIX,SUMX,SUMXY)
C
C	This routine accumulates the sums needed to compute the necessary
C	statistics. In each call, it passes through one area, as defined
C	by the area parameter.
C
	IMPLICIT NONE
	REAL*8 SUMX(NI),SUMXY(NI,NI)
	REAL DATA(NS,NI)
	INTEGER INUNIT,NI,NBAND(NI),ISL,ISS,NL,NS,INC,NPIX,IEL,LINE
	INTEGER ISAMP,IBAND,JBAND,ISTAT
C
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL,INC
C								read in data
	    DO IBAND=1,NI
		CALL XVREAD(INUNIT,DATA(1,IBAND),ISTAT,'LINE',LINE,
     +			 'SAMP',ISS,'NSAMPS',NS,'BAND',NBAND(IBAND),' ')
	    END DO
C								update sums
	    DO ISAMP = 1,NS,INC
		DO IBAND = 1,NI
		    DO JBAND = 1,IBAND
			SUMXY(JBAND,IBAND) = SUMXY(JBAND,IBAND) +
     +				     DATA(ISAMP,JBAND)*DATA(ISAMP,IBAND)
		    END DO
		    SUMX(IBAND) = SUMX(IBAND) + DATA(ISAMP,IBAND)
		END DO
		NPIX = NPIX + 1
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE COMPUTE_STATS(NI,NPIX,SUMX,SUMXY,CORR,QPRINT,
     +				 QSUMMARY,CHAN_MEAN,CHAN_SIGMA,OMATRIX)
C
	IMPLICIT NONE
	INTEGER NDIM
	PARAMETER (NDIM=300)
C
	REAL*8 SUMX(NI),SUMXY(NI,NI),OMATRIX(NDIM,NDIM),SIZE
	REAL*8 CHAN_MEAN(NI),CHAN_SIGMA(NI),AVG1,AVG2
	REAL XMIN,XMAX
	INTEGER NI,NPIX,NPIX_1,I,J,K,J1,LOC,IMIN,IMAX
	LOGICAL CORR,QPRINT,QSUMMARY
	CHARACTER*133 PRT
C
	NPIX_1 = NPIX-1
	AVG1 = 0.0
	AVG2 = 0.0
C
	DO I=1,NI
C								compute means
	    CHAN_MEAN(I) = SUMX(I)/NPIX
C							     compute covariances
	    DO J=1,I
		OMATRIX(J,I)=(SUMXY(J,I)-CHAN_MEAN(J)*CHAN_MEAN(I)*NPIX)
     +			     / NPIX_1
		OMATRIX(I,J) = OMATRIX(J,I)
	    END DO
C							compute std deviations
	    IF (OMATRIX(I,I) .GT. 0.0) THEN
		CHAN_SIGMA(I) = SQRT(OMATRIX(I,I))
	    ELSE
		CHAN_SIGMA(I) = 0.0
	    END IF
	    AVG1 = AVG1 + CHAN_MEAN(I)
	    AVG2 = AVG2 + CHAN_SIGMA(I)
	END DO
	AVG1 = AVG1 / FLOAT(NI)
	AVG2 = AVG2 / FLOAT(NI)
C							report means, sigmas
	IF (QPRINT) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('   Channel          Mean     Std. Dev.',' ')
	    DO I=1,NI
		WRITE (PRT,100) I,CHAN_MEAN(I),CHAN_SIGMA(I)
  100		FORMAT(I10,F14.5,F14.5)
		CALL XVMESSAGE(PRT,' ')
	    END DO
	END IF
C						summarize means and sigmas
	IF (QSUMMARY) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL MINMAX(8,NI,CHAN_MEAN,XMIN,XMAX,IMIN,IMAX)
	    WRITE (PRT,110) AVG1,XMIN,XMAX
  110	    FORMAT('Means average',G15.6,' and range from',G15.6,' to',
     +		   G15.6)
	    CALL XVMESSAGE(PRT,' ')
	    WRITE (PRT,120) IMIN,IMAX
  120	    FORMAT(48X,'(',I3,')',13X,'(',I3,')')
	    CALL XVMESSAGE(PRT,' ')
	    CALL MINMAX(8,NI,CHAN_SIGMA,XMIN,XMAX,IMIN,IMAX)
	    WRITE (PRT,130) AVG2,XMIN,XMAX
  130	    FORMAT('Sigmas average',G15.6,' and range from',G15.6,' to',
     +		   G15.6)
	    CALL XVMESSAGE(PRT,' ')
	    WRITE (PRT,140) IMIN,IMAX
  140	    FORMAT(49X,'(',I3,')',13X,'(',I3,')')
	    CALL XVMESSAGE(PRT,' ')
	END IF
C							report covariance matrix
	IF (QSUMMARY) THEN
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE('Covariance Matrix:',' ')
	END IF
	IF (QPRINT) THEN
	    DO J=1,NI,10
		DO I=1,NI
		    J1 = MIN(I, J+9)
		    IF (J1 .GE. J) THEN
			WRITE (PRT,190) I
  190			FORMAT(I3)
			LOC = 4
			DO K=J,J1
			    SIZE = ABS(OMATRIX(K,I))
			    IF (SIZE.GE.0.001 .AND. SIZE.LT.1000000.0) THEN
				WRITE (PRT(LOC:LOC+12),200) OMATRIX(K,I)
  200				FORMAT(F12.4)
			    ELSE
				WRITE (PRT(LOC:LOC+12),210) OMATRIX(K,I)
  210				FORMAT(1P,E12.5)
			    END IF
			    LOC = LOC + 12
			END DO
			CALL XVMESSAGE(PRT,' ')
		    END IF
		END DO
		WRITE (PRT,220) (K,K=J,J1)
  220		FORMAT(I11,9I12)
		CALL XVMESSAGE(PRT,' ')
		CALL XVMESSAGE(' ',' ')
	    END DO
	END IF
	IF (QSUMMARY) CALL MATRIX_SUMMARY(OMATRIX,NI)
C						if requested, compute and report
C						the correlation matrix
	IF (CORR) THEN
	    DO I=1,NI
		DO J=1,I
		    IF (CHAN_SIGMA(I)*CHAN_SIGMA(J) .NE. 0.0) THEN
			OMATRIX(J,I) = OMATRIX(J,I) /
     +				       (CHAN_SIGMA(I)*CHAN_SIGMA(J))
			OMATRIX(I,J) = OMATRIX(J,I)
		    ELSE
			OMATRIX(J,I) = 0.0
			OMATRIX(I,J) = 0.0
		    END IF
		END DO
	    END DO
C							print correlation matrix
	    IF (QSUMMARY) THEN
		CALL XVMESSAGE(' ',' ')
		CALL XVMESSAGE('Correlation Matrix:',' ')
	    END IF
	    IF (QPRINT) THEN
		DO J=1,NI,10
		    DO I=1,NI
			J1 = MIN(I, J+9)
			IF (J1 .GE. J) THEN
			    WRITE (PRT,250) I,(OMATRIX(K,I),K=J,J1)
  250			    FORMAT(I3,F13.6,9F12.6)
			    CALL XVMESSAGE(PRT,' ')
			END IF
		    END DO
		    WRITE (PRT,270) (K,K=J,J1)
  270		    FORMAT(I10,9I12)
		    CALL XVMESSAGE(PRT,' ')
		    CALL XVMESSAGE(' ',' ')
		END DO
	    END IF
	    IF (QSUMMARY) CALL MATRIX_SUMMARY(OMATRIX,NI)
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE COMPUTE_TRANSFORM(NI,NO,IOUT,EVAL,EVEC,CHAN_MEAN,
     +				     CHAN_SIGMA,CORR,XMEAN,SIGMA,
     +				     TMATRIX,OFFSET)
C
C	This routine computes the overall transformation needed to produce
C	a stretched principal component image. The matrix transformation is
C	stored in the array TMATRIX, and the necessary offsets to keep the 
C	results centered about the desired output mean values are stored in
C	the array OFFSET.
C
C	NI	           - (input)  integer number of input channels used
C	NO		   - (input)  integer number of pc's output
C	IOUT(NO)           - (input)  integer array of the pc's to be output
C	EVAL(NI)           - (input)  real array of eigenvalues
C	EVEC(NDIM,NDIM)    - (input)  real matrix of eigenvectors (rotation
C					   matrix)
C	CHAN_MEAN(NI)      - (input)  real array of the means of the input
C					   images
C	CHAN_SIGMA(NI)     - (input)  real array of the standard deviations of
C					   the input images
C	CORR               - (input) logical flag; 
C				      TRUE => correlation matrix was used
C	XMEAN              - (input)  real target mean for each output channel.
C       SIGMA              - (input)  real target standard deviation for each
C				           output channel.
C	TMATRIX(NDIM,NDIM) - (output) real transformation matrix needed to
C					   produce the output images from the
C					   inputs
C	OFFSET(NI)         - (output) real array of offsets applied to pixels
C					   after the transformation matrix, to
C					   center output values around XMEAN
C
	IMPLICIT NONE
	INTEGER NDIM
	PARAMETER (NDIM=300)
C
	REAL*8 EVEC(NDIM,NDIM),EVAL(NI),CHAN_MEAN(NI),CHAN_SIGMA(NI)
	REAL TMATRIX(NDIM,NDIM),OFFSET(NI),GAIN(NDIM),GAININ(NDIM)
	REAL XMEAN,SIGMA,X
	INTEGER NI,NO,I,J,IOUT(NO)
	LOGICAL CORR
C
	DO J=1,NO
C				GAIN(I) is the vector that will be multiplied
C				with the rotation matrix. 
C				If an eigenvalue (EVAL) is 0.0, there is no
C				appropriate value for GAIN. EVAL cannot
C				correctly be less than 0.0
C					
	    I = IOUT(J)
	    IF (EVAL(I).GT.0.0 .AND. SIGMA.GT.0.0) THEN
		GAIN(J) = SIGMA/SQRT(EVAL(I))
	    ELSE
		GAIN(J) = 1.0
	    END IF
	END DO
C				When using the correlation matrix eigenfunctions
C				the input data must first be variance 
C				normalized. The GAININ values effectively makes
C				this adjustment.
C
	DO I=1,NI
	    IF (CORR .AND. CHAN_SIGMA(I).NE.0.0) THEN
		GAININ(I) = 1.0/CHAN_SIGMA(I)
	    ELSE
		GAININ(I) = 1.0
	    END IF
	END DO
C				This loop generates the output matrix, which is
C				formed by the product of the rotation matrix
C				(EVEC), the vector of variance normalizing
C				values (GAIN), and, if the correlation matrix
C				is used, the input normalizing values (GAININ).
C				X is used to compute the output location of the
C				     central input point. The OFFSET array is
C				     computed from X, to force the output to be
C				     centered at the requested value.
	DO I=1,NO
	    X = 0.0
	    DO J=1,NI
		TMATRIX(J,I) = EVEC(J,IOUT(I)) * GAININ(J) * GAIN(I)
		X = X + TMATRIX(J,I)*CHAN_MEAN(J)
	    END DO
	    OFFSET(I) = XMEAN - X
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE DO_TRANSFORM(INUNIT,IOUTUNIT,NBAND,ISL,ISS,NL,NS,NI,
     +				NO,IFMT,TMATRIX,OFFSET,DATA,OUTBUF)
C
C	This routine performs the principal component stretch transformation 
C	and produces the output file.
C
C	INUNIT   - input,integer        Unit number of input
C	IOUTUNIT - input,integer        Unit number of output
C	NBAND    - input,integer array  Input bands to be used
C	ISL      - input,integer        Starting line of image
C	ISS      - input,integer        Starting sample of image
C	NL       - input,integer        Number of lines to process
C	NS       - input,integer        Number of samples to process
C	NI       - input,integer        Number of input bands
C	NO       - input,integer        Number of output bands
C	IFMT     - input,integer        Pixel format (1=BYTE, 2=HALF, 3=FULL,
C                                       4=REAL)
C	TMATRIX  - input,real array     Transformation matrix
C       OFFSET   - input,real array     Offset to be applied after
C				        transformation matrix
C	DATA     - buffer,real array    Storage buffer for input data
C	OUTBUF   - buffer,real array    storage buffer for outbut data
C
	IMPLICIT NONE
	INTEGER NDIM
	PARAMETER (NDIM=300)
C
	REAL TMATRIX(NDIM,NDIM),OFFSET(NI),DATA(NS,NI),OUTBUF(*),X
	INTEGER INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NI,NO,IFMT,IEL,LINE,ISAMP
	INTEGER NBAND(NI),ISTAT,I,IBAND,ILO,IHI
C
	IEL = ISL + NL - 1
C				    separate loops for integer and real outputs,
C					in order to round integer values and
C					properly clip the output data range.
	IF (IFMT .EQ. 4) THEN
	    DO LINE=ISL,IEL
C								read in data
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT,DATA(1,IBAND),ISTAT,
     +				'BAND',NBAND(IBAND),'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
C								do transform
		DO IBAND=1,NO
		    DO ISAMP = 1,NS
			X = OFFSET(IBAND)
			DO I = 1,NI
			    X = X + TMATRIX(I,IBAND)*DATA(ISAMP,I)
			END DO
			OUTBUF(ISAMP) = X
		    END DO
		    CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')
		END DO
	    END DO
	ELSE
C							   set data range limits
	    IF (IFMT .EQ. 1) THEN
		ILO = 0
		IHI = 255
	    ELSE IF (IFMT .EQ. 2) THEN
		ILO = -32768
		IHI = 32767
	    ELSE
		ILO = -2147483648
		IHI =  2147483647
	    END IF
C
	    DO LINE=ISL,IEL
C								read in data
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT,DATA(1,IBAND),ISTAT,
     +				'BAND',NBAND(IBAND),'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
C								do transform
		DO IBAND=1,NO
		    DO ISAMP = 1,NS
			X = OFFSET(IBAND)
			DO I = 1,NI
			    X = X + TMATRIX(I,IBAND)*DATA(ISAMP,I)
			END DO
			OUTBUF(ISAMP) = MIN(IHI,MAX(ILO,NINT(X)))
		    END DO
		    CALL XVWRIT(IOUTUNIT,OUTBUF,ISTAT,' ')
		END DO
	    END DO
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE JACOBI(OMATRIX,NI,EVAL,EVEC,LOOP)
C
	IMPLICIT NONE
	INTEGER NDIM
	PARAMETER (NDIM=300)
C
	REAL*8 OMATRIX(NDIM,NDIM),EVAL(NDIM),EVEC(NDIM,NDIM)
	REAL*8 B(NDIM),Z(NDIM),SUM,THRESH,THRESH2,G,H,DIF
	REAL*8 SIN,COS,TAN,THETA,TAU
	INTEGER NI,ICOL,IROW,LOOP,IP,IQ,J
C								initialize
	SUM = 0.0
	DO ICOL=1,NI
	    DO IROW=1,NI
		IF (IROW .EQ. ICOL) THEN
		    EVEC(ICOL,IROW) = 1.0
		    EVAL(ICOL) = OMATRIX(ICOL,ICOL)
		    B(ICOL) = EVAL(ICOL)
		    Z(ICOL) = 0.0
		ELSE
		    EVEC(ICOL,IROW) = 0.0
		    SUM = SUM + ABS(OMATRIX(ICOL,IROW))/2.0
		END IF
	    END DO
	END DO
	LOOP = 1
C								main loop
	DO WHILE (LOOP.LE.100 .AND. SUM .GT. 0.0)
C
	    IF (LOOP .LT. 4) THEN
		THRESH = 0.2*SUM/NI*NI
	    ELSE
		THRESH = 0.0
	    END IF
C
	    DO IP=1,NI-1
		DO IQ=IP+1,NI
C							if the magnitude of the
C							eigenvalues is as large
C							as THRESH2, there is no
C							benefit to rotation.
C						       (within machine accuracy)
		    THRESH2 = 1.0D18 * ABS(OMATRIX(IP,IQ))
		    IF ((LOOP .GT. 4) .AND. 
     +			(ABS(EVAL(IP)) .GE. THRESH2) .AND.
     +			(ABS(EVAL(IQ)) .GE. THRESH2)) THEN
			OMATRIX(IP,IQ) = 0.0
		    ELSE IF (ABS(OMATRIX(IP,IQ)) .GT. THRESH) THEN
			DIF = EVAL(IQ) - EVAL(IP)
			IF (ABS(DIF) .GE. THRESH2) THEN
			    TAN = OMATRIX(IP,IQ) / DIF
			ELSE
			    THETA = 0.5*DIF/OMATRIX(IP,IQ)
			    TAN = 1.0 / (ABS(THETA)+SQRT(1.0+THETA*THETA))
			    IF (THETA .LT. 0.0) TAN = -TAN
			END IF
			COS = 1.0 / SQRT(1.0 + TAN*TAN)
			SIN = TAN*COS
			TAU = SIN / (1.0 + COS)
			H = TAN*OMATRIX(IP,IQ)
			Z(IP) = Z(IP) - H
			Z(IQ) = Z(IQ) + H
			EVAL(IP) = EVAL(IP) - H
			EVAL(IQ) = EVAL(IQ) + H
			OMATRIX(IP,IQ) = 0.0
			DO J=1,IP-1
			    G = OMATRIX(J,IP)
			    H = OMATRIX(J,IQ)
			    OMATRIX(J,IP) = G - SIN*(H+G*TAU)
			    OMATRIX(J,IQ) = H + SIN*(G-H*TAU)
			END DO
			DO J=IP+1,IQ-1
			    G = OMATRIX(IP,J)
			    H = OMATRIX(J,IQ)
			    OMATRIX(IP,J) = G - SIN*(H+G*TAU)
			    OMATRIX(J,IQ) = H + SIN*(G-H*TAU)
			END DO
			DO J=IQ+1,NI
			    G = OMATRIX(IP,J)
			    H = OMATRIX(IQ,J)
			    OMATRIX(IP,J) = G - SIN*(H+G*TAU)
			    OMATRIX(IQ,J) = H + SIN*(G-H*TAU)
			END DO
			DO J=1,NI
			    G = EVEC(J,IP)
			    H = EVEC(J,IQ)
			    EVEC(J,IP) = G - SIN*(H+G*TAU)
			    EVEC(J,IQ) = H + SIN*(G-H*TAU)
			END DO
		    END IF
		END DO
	    END DO
	    SUM = 0.0
	    DO IROW = 1,NI
		B(IROW) = B(IROW) + Z(IROW)
		EVAL(IROW) = B(IROW)
		Z(IROW) = 0.0
		DO ICOL = 1,IROW-1
		    SUM = SUM + ABS(OMATRIX(ICOL,IROW))
		END DO
	    END DO
	    LOOP = LOOP + 1
	END DO
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE SWAP(NI,VEC1,VEC2,X1,X2)
C
C	This routine swaps vectors VEC1 and VEC2, and swaps scalars X1 and X2
C
	IMPLICIT NONE
	REAL*8 VEC1(NI),VEC2(NI),X1,X2,X
	INTEGER NI,I
C
	X = X1
	X1 = X2
	X2 = X
C
	DO I=1,NI
	    X = VEC1(I)
	    VEC1(I) = VEC2(I)
	    VEC2(I) = X
	END DO
	RETURN
	END
C****************************************************************************
	SUBROUTINE REPORT_TRANSFORM(NI,NO,TMATRIX,OFFSET)
C
C	This routine prints out the final transformation equations.
C
	IMPLICIT NONE
	INTEGER NDIM
	PARAMETER (NDIM=300)
C
	REAL TMATRIX(NDIM,NDIM),OFFSET(NDIM),SIZE
	INTEGER NI,NO,I,I2,I3,J,NJ,LOC
	CHARACTER*133 PR
C
	CALL XVMESSAGE(' ',' ')
	DO I=1,NO
	    SIZE = ABS(OFFSET(I))
	    IF (SIZE .GE. 0.0001 .AND. SIZE .LT. 1.0E6) THEN
		WRITE (PR,100) I,OFFSET(I)
  100		FORMAT('Output',I3,' = ',F12.4)
	    ELSE
		WRITE (PR,200) I,OFFSET(I)
  200		FORMAT('Output',I3,' = ',1P,E12.4)
	    END IF
	    NJ = MIN(NI,8)
	    DO J=1,NJ
		LOC = 13*(J-1) + 25
		SIZE = ABS(TMATRIX(J,I))
		IF (SIZE .LT. 0.0001 .OR. SIZE .GE. 100000.0) THEN
		    WRITE (PR(LOC:LOC+12),300) TMATRIX(J,I),J
  300		    FORMAT(SP,1P,E8.1,'*in',SS,I2.2)
		ELSE IF (SIZE .LT. 100.0) THEN
		    WRITE (PR(LOC:LOC+12),400) TMATRIX(J,I),J
  400		    FORMAT(SP,F8.4,'*in',SS,I2.2)
		ELSE
		    WRITE (PR(LOC:LOC+12),500) TMATRIX(J,I),J
  500		    FORMAT(SP,F8.1,'*in',SS,I2.2)
		END IF
	    END DO
	    CALL XVMESSAGE(PR(1:LOC+12),' ')
	    DO I2=9,NI,8
		PR(1:24) = '                        '
		NJ = MIN(NI,I2+7)
		DO I3=I2,NJ
		    LOC = 13*(I3-I2) + 25
		    SIZE = ABS(TMATRIX(I3,I))
		    IF (SIZE .LT. 0.0001 .OR. SIZE .GE. 100000.0) THEN
			IF (I3 .LT. 100) THEN
			    WRITE (PR(LOC:LOC+12),600) TMATRIX(I3,I),I3
  600			    FORMAT(SP,1P,E8.1,'*in',SS,I2.2)
			ELSE
			    WRITE (PR(LOC:LOC+12),650) TMATRIX(I3,I),I3
  650			    FORMAT(SP,1P,E8.1,'*i',SS,I3)
			END IF
		    ELSE IF (SIZE .LT. 100.0) THEN
			IF (I3 .LT. 100) THEN
			    WRITE (PR(LOC:LOC+12),700) TMATRIX(I3,I),I3
  700			    FORMAT(SP,F8.4,'*in',SS,I2.2)
			ELSE
			    WRITE (PR(LOC:LOC+12),750) TMATRIX(I3,I),I3
  750			    FORMAT(SP,F8.4,'*i',SS,I3)
			END IF
		    ELSE
			IF (I3 .LT. 100) THEN
			    WRITE (PR(LOC:LOC+12),800) TMATRIX(I3,I),I3
  800			    FORMAT(SP,F8.1,'*in',SS,I2.2)
			ELSE
			    WRITE (PR(LOC:LOC+12),850) TMATRIX(I3,I),I3
  850			    FORMAT(SP,F8.1,'*i',SS,I3)
			END IF
		    END IF
		END DO
		CALL XVMESSAGE(PR(1:LOC+12),' ')
	    END DO
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE MATRIX_SUMMARY(OMATRIX,NI)
C
	IMPLICIT NONE
	INTEGER NDIM
	PARAMETER (NDIM=300)
C
	REAL*8 OMATRIX(NDIM,NDIM),XMIN,XMAX,AVG1,AVG2,SIZE
	INTEGER NI,MINCOL,MINROW,MAXCOL,MAXROW,I,J
	CHARACTER*80 PRT
C							accumulate summary stats
	XMIN = OMATRIX(1,2)
	XMAX = OMATRIX(1,2)
	AVG1 = OMATRIX(1,2)
	AVG2 = ABS(OMATRIX(1,2))
	MINCOL = 1
	MINROW = 2
	MAXCOL = 1
	MAXROW = 2
	DO I=3,NI
	    DO J=1,I-1
		SIZE = OMATRIX(J,I)
		AVG1 = AVG1 + SIZE
		AVG2 = AVG2 + ABS(SIZE)
		IF (SIZE .LT. XMIN) THEN
		    XMIN = SIZE
		    MINROW = I
		    MINCOL = J
		END IF
		IF (SIZE .GT. XMAX) THEN
		    XMAX = SIZE
		    MAXROW = I
		    MAXCOL = J
		END IF
	    END DO
	END DO
	AVG1 = AVG1 / FLOAT(NI*(NI-1)/2)
	AVG2 = AVG2 / FLOAT(NI*(NI-1)/2)
C							print summary stats
	WRITE (PRT,100) AVG1,AVG2
  100	FORMAT('Interchannel values average',G15.6,',      (',G15.6,
     +		   ' in magnitude)')
	CALL XVMESSAGE(PRT,' ')
	WRITE (PRT,200) XMIN,XMAX
  200	FORMAT('They range from',G15.6,' to',G15.6)
	CALL XVMESSAGE(PRT,' ')
	WRITE (PRT,300) MINROW,MINCOL,MAXROW,MAXCOL
  300	FORMAT(17X,'(',I3,',',I3,')',10X,'(',I3,',',I3,')')
	CALL XVMESSAGE(PRT,' ')
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create eigen.imake
#define  PROGRAM   eigen

#define MODULE_LIST eigen.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create eigen.pdf
process help=*
PARM INP         TYPE=(STRING,80)
PARM OUT         TYPE=(STRING,80) COUNT=(0:1)		  DEFAULT=--
PARM SIZE        TYPE=INTEGER    COUNT=4                  DEFAULT=(1,1,0,0)
PARM SL	         TYPE=INTEGER   			  DEFAULT=1
PARM SS	         TYPE=INTEGER				  DEFAULT=1
PARM NL          TYPE=INTEGER				  DEFAULT=0
PARM NS          TYPE=INTEGER				  DEFAULT=0
PARM MATRIX      TYPE=KEYWORD    VALID=("CORR","COV")     DEFAULT="COV"
PARM INC         TYPE=INTEGER                             DEFAULT=3
PARM USEBANDS    TYPE=INTEGER    COUNT=(0:300)            DEFAULT=--
PARM OUTPUT_PCS  TYPE=INTEGER    COUNT=(0:300)            DEFAULT=--
PARM AREA        TYPE=INTEGER    COUNT=(0:200)            DEFAULT=--
PARM MEAN        TYPE=REAL                                DEFAULT=127.5
PARM SIGMA       TYPE=REAL                                DEFAULT=45.0
PARM EXCLUDE     TYPE=REAL       COUNT=(0:1)              DEFAULT=--
PARM PRINT       TYPE=KEYWORD VALID=("ALL","SUMMARY","NOPRINT") DEFAULT="ALL"
PARM XFORM_PARMS TYPE=STRING     COUNT=(0:1)              DEFAULT=--
END-PROC
.TITLE
Process EIGEN -- Principal component analysis using eigenvectors.
.HELP
PURPOSE:
        EIGEN will compute the principal component (aka the eigenvector or
Karhunen-Loeve) transformation matrix of up to 300 input channels.
The covariance (and, optionally, correlation) matrix, the transformation
matrix of eigen-vectors, and the eigen-values are printed, unless the user
requests otherwise.  If an output is provided, the principal component 
transformation is applied to the input image.

THEORY:

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

        Then the jth principal component is: v(j) = A(j)  X = a(ij)x(i)

NOTE:  The jth eigen-value is actually the variance in the jth
principal component dimension.  Therefore, the eigen-values are useful
as a measure the information content that can be expected in the output
pictures.

OPERATION:

The input image is first statistically sampled, using the INC and AREA, 
parameters to select the sampling grid and region(s) of interest. The
user may choose to exclude all pixels that have a specified (by the 
parameter EXCLUDE) value in all input channels.  The channel by channel 
means and variances are computed, as well as the interchannel covariance 
(optionally, correlation) matrix.

From the calculated matrix, the related eigenvalues and eigenvectors are
computed. The matrix of these eigenvectors is often called the principal
component rotation matrix.

A "stretching vector" (or Normalization vector) is formed by taking the
reciprocal of the square root of each element of the eigenvalue vector,
and multiplying it by the SIGMA parameter. This has the effect of 
rescaling the output pixels in such a way that the standard deviation
of each output channel is equal to SIGMA.

Next, an offset value is computed for each output component. The offset
is seleceted to force the mean pixel value in each output channel to be
equal to the target value specified by the MEAN parameter.

Finally, if an output dataset has been provided, this transformation is
applied to the input dataset, and written to the output dataset.

The user may select a subset of the input bands to process, by using the
USEBANDS parameter.  Similarly, the user may save or use a subset of the
principal components, by using the OUTPUT_PCS parameter.

WRTTEN BY:   John Addington
             Ron Alley

COGNIZANT PROGRAMMER: Ron Alley

REVISION:        8 December  2003    REA   Rewritten, adapted from
                                           DESTRETCH, XFORM, and EIGENVEC

.LEVEL1
.VARIABLE INP
input dataset
.VARIABLE OUT
(Optional) output dataset
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
.VARIABLE MATRIX
Use correlation or
covariance statistics?
(Valid: CORR, COV)
.VARIABLE INC
Compute statistics from every
nth line and nth sample
.VARIABLE USEBANDS
Use only these bands in the
calculation. (Default is to
use all bands.)
.VARIABLE OUTPUT_PCS
Restrict the output to the
following principal components.
(Default is to output all
components.)
.VARIABLE AREA
The subareas to be used to
compute statistics. Up to 50
regions (SL,SS,NL,NS) may be
entered. Default is to use
the entire image.
.VARIABLE MEAN
Desired image mean for each
output channel.
.VARIABLE SIGMA
Desired image standard deviation
for each output channel.
.VARIABLE EXCLUDE
Pixels with this DN in all
bands will be excluded from
all computations.
.VARIABLE PRINT
Determines the detail of the
printed information output
(Valid: ALL, SUMMARY, NOPRINT)
.VARIABLE XFORM_PARMS
The name for the parameter
dataset to hold the
transformation matrix.
.LEVEL2
.VARIABLE INP
Input is a single file in either BSQ or BIL format, containing at least 3 
channels.  The user may specify which channels are to be used, by means of 
the USEBANDS parameter.
.VARIABLE OUT
OUT contains the name of the output dataset that contains the transformed
image.  If no output dataset is specified, the statistics will be gathered
and reported, but no transformation will be applied.
.VARIABLE SIZE
The standard Vicar size field ( starting_line, starting_sample, 
number_of_lines, number_of_samples).
.VARIABLE SL
Starting line of the portion of the image that you wish to process.
.VARIABLE SS
Starting sample of the portion of the image that you wish to process.
.VARIABLE NL
Number of lines in the portion of the image that you wish to process.
.VARIABLE NS
Number of samples in the portion of the image that you wish to process.
.VARIABLE MATRIX
If the value of the parameter MATRIX is "COV" (the default), the image's
covariance matrix is used to determine the principal components.  If the
value of MATRIX is "CORR", then the correlation matrix is used instead.
.VARIABLE INC
Statistics are gathered using only every n'th line and n'th sample of the
image, or region of interest (AREA) within the image. The parameter INC
specifies the value on "n".
.VARIABLE USEBANDS
If this parameter is defaulted, all of the channels in the input file are
used in the transformation.  If the user wishes to use only a subset of 
the channels, the USEBANDS parameter is used to select the channels to be
used.
.VARIABLE OUTPUT_PCS
If this parameter is defaulted, the number of output components (channels)
will be equal to the number of input channels used.  If the user wishes
to output only a subset of the principal components, the OUTPUT_PCS
parameter is used to select the principal components to be output.
.VARIABLE AREA
Sets of (Starting_line, Starting_sample, Number_of_lines, Number_of_samples)
are given to define subareas used to generate the image statistics. 
Up to 50 set of subareas may be supplied.  The default is that the entire image 
is sampled.
.VARIABLE MEAN
A rescaling factor is included in the overall transformation to reposition the
output values in a range appropriate for output data type (BYTE, HALF, FULL,
REAL). The MEAN parameter specifies the desired mean value for the output 
image channels.  If MEAN is defaulted, a value of 127.5 is used.  If the AREA
parameter has been used, this target mean is for the AREA(s) of interest only.
To suppress rescaling entirely, assign the parameter SIGMA a negative value.
.VARIABLE SIGMA
A rescaling factor is included in the overall transformation to reposition the
output values in a range appropriate for the data output. The SIGMA parameter
specifies the desired standard deviation from the mean value in each of the 
output image channels.  If SIGMA is defaulted, then a value of 45.0 is used.
If SIGMA is assigned a negative value, the rescaling operation is suppressed.
If the AREA parameter has been used, this target standard deviation is for 
the AREA(s) of interest only.
.VARIABLE EXCLUDE
A pixel is excluded from use in determining the transformation if the
DN is equal to 'EXCLUDE' in each channel.
.VARIABLE PRINT
The value of the PRINT variable determines the detail of the printed 
information output to the standard output and session log.

If ALL (the default), the complete list of input means, standard deviations,
      interchannel covariances and correlations (if computed), eigenvalues,
      eigenvectors, and transformation equations is printed. 
If SUMMARY, these tables of values are only summarized, and the eigenvectors
      and transformation equations are omitted.
If NOPRINT, only the EIGEN version number is printed.
.VARIABLE XFORM_PARMS
If the XFORM_PARM parameter is given a value, the transformation matrix
will be saved as a VICAR parameter dataset, and have the dataset name
specified by the save parameter. This parameter dataset may then be included
in the parameters for XFORM, to repeat this transformation on other datasets.
.END
$ Return
$!#############################################################################
