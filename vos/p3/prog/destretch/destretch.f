      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     8 Feb 98    ...rea...  Initial release. Based upon EIGENVEC, the
C                            standalone 'destretch' program, and the ASTER
C                            Decorrelation Stretch Standard Product algorithm
C    21 Mar 00    ...rea...  Add SAVE parameter, analogous to the SAVE parameter
C                            in EIGEN/EIGENVEC
C    14 Jun 00    ...rea...  Add EXCLUDE parameter to exclude pixels that are
C			     zero in all bands.
C    16 Nov 00    ...rea...  Add QA plane processing.
C
	REAL*8 SUMX(32),SUMXY(1024)
	REAL DATA(200000),EVEC(32,32),EVAL(32),OFFSET(32)
	REAL DSCALE(32),CHAN_MEAN(32),CHAN_SIGMA(32),OMATRIX(32,32)
	REAL BUF(1024)
	INTEGER INUNIT(32),IOUTUNIT(32),NBAND(32),ISUBAREA(200)
	LOGICAL XVPTST,CORR,Q3D,Q3DOUT,QSAVE,QEXCL
	CHARACTER*132 PR
	CHARACTER*60 SAVE
C
	DATA DSCALE/32*1.0/
C								open inputs
	CALL XVPCNT('INP',NI)
	DO I=1,NI
	    CALL XVUNIT(INUNIT(I),'INP',I,ISTATUS,' ')
	    CALL XVOPEN(INUNIT(I),ISTATUS,'OPEN_ACT','SA',
     +			'IO_ACT','SA','U_FORMAT','REAL',' ')
	END DO
	IF (NI .EQ. 1) THEN
	    Q3D = .TRUE.
	    CALL XVBANDS(ISB,NB,NBIN)
	    CALL XVPARM('BANDS',NBAND,NI,IDEF,32)
	    IF (NI .EQ. 0) THEN
		NI = MIN(NB,32)
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
	ELSE
	    Q3D = .FALSE.
	END IF
C								QA
	CALL XVPARM('QA',IQA,ICNT,IDEF,0)
	IF (.NOT.Q3D .AND. IQA.NE.0) THEN
	    IQA = NI
	    NI = NI-1
	END IF
C							        get parameters
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C								inc
	CALL XVPARM('INC',INC,ICNT,IDEF,0)
C								area
	CALL XVPARM('AREA',ISUBAREA,N_AREA_PARS,IDEF,0)
	IF (N_AREA_PARS.EQ.0) THEN
	    ISUBAREA(1) = ISL
	    ISUBAREA(2) = ISS
	    ISUBAREA(3) = NL
            ISUBAREA(4) = NS
	    N_AREA_PARS = 4
	END IF
C								covariance
C								exclude, mean
C								sigma, dscale
	CORR = .NOT. XVPTST('COV')
	QEXCL = XVPTST('EXCLUDE')
	CALL XVPARM('MEAN',XMEAN,ICNT,IDEF,0)
	CALL XVPARM('SIGMA',SIGMA,ICNT,IDEF,0)
	CALL XVPARM('DSCALE',DSCALE,ICNT,IDEF,0)
C								save
	CALL XVPCNT('SAVE',NUM)
	IF (NUM .GT. 0) THEN
	    QSAVE = .TRUE.
	    CALL XVPARM('SAVE',SAVE,ICNT,IDEF,0)
	ELSE
	    QSAVE = .FALSE.
	END IF
C								open outputs
	CALL XVPCNT('OUT',NO)
	IF (NO .EQ. 1) THEN
	    CALL XVUNIT(IOUTUNIT(1),'OUT',1,ISTATUS,' ')
	    CALL XVOPEN(IOUTUNIT(1),ISTATUS,'U_NL',NL,'U_NS',NS,
     +			'U_NB',NI,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','BYTE','O_FORMAT','BYTE',
     +			'U_ORG','BIL','OP','WRITE',' ')
	    Q3DOUT = .TRUE.
	ELSE
	    IF (NI .NE. NO) THEN
		CALL XVMESSAGE(
     +	'Number of input bands does not match the number of outputs',' ')
		CALL ABEND
	    ENDIF
	    DO I=1,NO
		CALL XVUNIT(IOUTUNIT(I),'OUT',I,ISTATUS,' ')
		CALL XVOPEN(IOUTUNIT(I),ISTATUS,'U_NL',NL,'U_NS',NS,
     +			    'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     +			    'U_FORMAT','BYTE','O_FORMAT','BYTE',
     +			    'OP','WRITE',' ')
	    END DO
	    Q3DOUT = .FALSE.
	END IF
C							       gather statistics
	CALL ZIA(SUMX,2*NI)
	CALL ZIA(SUMXY,2*NI*NI)
	NPIX = 0
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' Area(s) sampled for statistics:',' ')
	DO I=1,N_AREA_PARS,4
	    WRITE (PR,100) ISUBAREA(I),ISUBAREA(I+1),ISUBAREA(I+2),
     +			   ISUBAREA(I+3)
  100	    FORMAT(2X,4I6)
	    CALL XVMESSAGE(PR,' ')
	    IF (QEXCL .OR. IQA.NE.0) THEN
		CALL Z_GATHER_STATS(INUNIT,NI,IQA,QEXCL,Q3D,NBAND,
     +				ISUBAREA(I),ISUBAREA(I+1),ISUBAREA(I+2),
     +				ISUBAREA(I+3),INC,DATA,NPIX,SUMX,SUMXY)
	    ELSE
		CALL GATHER_STATS(INUNIT,NI,Q3D,NBAND,ISUBAREA(I),
     +			     ISUBAREA(I+1),ISUBAREA(I+2),ISUBAREA(I+3),
     +			     INC,DATA,NPIX,SUMX,SUMXY)
	    END IF
	END DO
C								report INC, NPIX
C
	WRITE (PR,200) INC
  200	FORMAT('      INC =',I3)
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(PR,' ')
	WRITE (PR,250) NPIX
  250	FORMAT('      PIXELS USED =',I10)
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(PR,' ')
C					       compute means and cov/corr matrix
	CALL COMPUTE_STATS(NI,NPIX,SUMX,SUMXY,CORR,CHAN_MEAN,
     +			   CHAN_SIGMA,OMATRIX)
C						compute principal component
C						eigenvalues and eigenvectors
	CALL JACOBI(OMATRIX,NI,EVAL,EVEC)
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
	CALL XVMESSAGE('   Eigen',' ')
	CALL XVMESSAGE('   Value             Eigenvector',' ')
	DO I=1,NI
	    K = 1
	    K2 = MIN(NI,10)
	    WRITE (PR,300) EVAL(I),(EVEC(J,I),J=K,K+K2-1)
  300	    FORMAT(F11.4,10F12.5)
	    CALL XVMESSAGE(PR,' ')
	    K = K + K2
	    DO M=11,NI,10
		K3 = MIN(NI-M+1,10)
		WRITE (PR,400) (EVEC(J,I),J=K,K+K3-1)
  400		FORMAT(11X,10F12.5)
		CALL XVMESSAGE(PR,' ')
		K = K + K3
	    END DO
	END DO
C						Compute and report the final
C						transformation matrix.
C
	CALL COMPUTE_TRANSFORM(NI,EVAL,EVEC,CHAN_MEAN,CHAN_SIGMA,CORR,
     +			     DSCALE,XMEAN,SIGMA,OMATRIX,OFFSET)
	CALL XVMESSAGE(' ',' ')
	DO I=1,NI
	    NJ = MIN(NI,8)
	    WRITE (PR,500) I,OFFSET(I),(OMATRIX(J,I),J,J=1,NJ)
  500	    FORMAT(' Output',I2,' = ',F12.4,8(SP,F9.4,'*in',SS,I1))
	    CALL XVMESSAGE(PR,' ')
	    DO I2=9,NI,8
		NJ = MIN(NI,I2+7)
		WRITE (PR,600) (OMATRIX(J,I),J,J=I2,NJ)
  600		FORMAT(24X,8(SP,F8.4,'*in',SS,I2))
		CALL XVMESSAGE(PR,' ')
	    END DO
	END DO
C						Save the transformation matrix,
C						if requested
	IF (QSAVE) THEN
	    NUM = 0
	    DO I=1,NI
		DO J=1,NI
		    NUM = NUM + 1
		    BUF(NUM) = OMATRIX(J,I)
		END DO
	    END DO
	    CALL XVPOPEN(ISTATUS,IDUMMY1,IDUMMY2,SAVE,'SA',IDUMMY3)
	    CALL XVPOUT(ISTATUS,'MATRIX',BUF,'REAL',NUM)
	    DO I=1,NI
		BUF(I) = 1.0
	    END DO
	    CALL XVPOUT(ISTATUS,'GAIN',BUF,'REAL',NI)
	    CALL XVPOUT(ISTATUS,'OFFSET',OFFSET,'REAL',NI)
	    CALL XVPCLOSE(ISTATUS)
	END IF
C						Perform the transformation and
C						write output
C
	CALL DO_TRANSFORM(INUNIT,IOUTUNIT,NBAND,IQA,ISL,ISS,NL,NS,NI,
     +			  Q3D,Q3DOUT,OMATRIX,OFFSET,DATA)
C
      RETURN
      END
C*******************************************************************************
	SUBROUTINE Z_GATHER_STATS(INUNIT,NI,IQA,QEXCL,Q3D,NBAND,ISL,ISS,
     +				  NL,NS,INC,DATA,NPIX,SUMX,SUMXY)
C
C	This routine is used only if pixels are to be excluded.
C
C	This routine accumulates the sums needed to compute the necessary
C	statistics. In each call, it passes through one area, as defined
C	by the area parameter.
C
	REAL*8 SUMX(NI),SUMXY(NI,NI)
	REAL DATA(NS,NI),XQA(10000)
	INTEGER INUNIT(NI),NBAND(NI)
	LOGICAL QZERO,QEXCL,Q3D
C								initialize QA 
	DO I=1,NS
	    XQA(I) = 0.0
	END DO
C
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL,INC
C								read in data
	    IF (Q3D) THEN
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT(1),DATA(1,IBAND),ISTAT,
     +				'BAND',NBAND(IBAND),'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
		IF (IQA .NE. 0) CALL XVREAD(INUNIT(1),XQA,ISTAT,
     +					    'BAND',IQA,'LINE',LINE,
     +					    'SAMP',ISS,'NSAMPS',NS,' ')
	    ELSE
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT(IBAND),DATA(1,IBAND),ISTAT,
     +		 		'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
		IF (IQA .GT. 0) CALL XVREAD(INUNIT(IQA),XQA,ISTAT,
     +			 		    'LINE',LINE,'SAMP',ISS,
     +					    'NSAMPS',NS,' ')
	    END IF
C								update sums
	    DO ISAMP = 1,NS,INC
		IF (XQA(ISAMP) .LE. 15.0) THEN
		    QZERO = .TRUE.
		    DO IBAND = 1,NI
			DO JBAND = 1,IBAND
			    SUMXY(JBAND,IBAND) = SUMXY(JBAND,IBAND) +
     +				     DATA(ISAMP,JBAND)*DATA(ISAMP,IBAND)
			END DO
			SUMX(IBAND) = SUMX(IBAND) + DATA(ISAMP,IBAND)
			IF (DATA(ISAMP,IBAND) .NE. 0.0) QZERO = .FALSE.
		    END DO
		    IF (.NOT.QZERO .OR. .NOT.QEXCL) NPIX = NPIX + 1
		END IF
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE GATHER_STATS(INUNIT,NI,Q3D,NBAND,ISL,ISS,NL,NS,INC,
     +                          DATA,NPIX,SUMX,SUMXY)
C
C	This routine accumulates the sums needed to compute the necessary
C	statistics. In each call, it passes through one area, as defined
C	by the area parameter.
C
	REAL*8 SUMX(NI),SUMXY(NI,NI)
	REAL DATA(NS,NI)
	INTEGER INUNIT(NI),NBAND(NI)
	LOGICAL Q3D
C
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL,INC
C								read in data
	    IF (Q3D) THEN
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT(1),DATA(1,IBAND),ISTAT,
     +				'BAND',NBAND(IBAND),'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
	    ELSE
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT(IBAND),DATA(1,IBAND),ISTAT,
     +		 		'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
	    END IF
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
	SUBROUTINE COMPUTE_STATS(NI,NPIX,SUMX,SUMXY,CORR,CHAN_MEAN,
     +				 CHAN_SIGMA,OMATRIX)
C
	REAL*8 SUMX(NI),SUMXY(NI,NI)
	REAL CHAN_MEAN(NI),CHAN_SIGMA(NI),OMATRIX(32,32)
	LOGICAL CORR
	CHARACTER*132 PRT
C
	NPIX_1 = NPIX-1
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
	END DO
C					       report means, sigmas, covariances
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('   Channel          Mean     Std. Dev.',' ')
	DO I=1,NI
	    WRITE (PRT,100) I,CHAN_MEAN(I),CHAN_SIGMA(I)
  100	    FORMAT(I10,F14.5,F14.5)
	    CALL XVMESSAGE(PRT,' ')
	END DO
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE('Covariance Matrix:',' ')
	DO I=1,NI
	    DO J=1,I,10
		J1 = MIN(I, J+9)
		WRITE (PRT,200) (OMATRIX(K,I),K=J,J1)
  200		FORMAT(X,10F13.4)
		CALL XVMESSAGE(PRT,' ')
	    END DO
	END DO
	CALL XVMESSAGE(' ',' ')
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
	    CALL XVMESSAGE('Correlation Matrix:',' ')
	    DO I=1,NI
		DO J=1,I,10
		    J1 = MIN(I, J+9)
		    WRITE (PRT,200) (OMATRIX(K,I),K=J,J1)
		    CALL XVMESSAGE(PRT,' ')
		END DO
	    END DO
	    CALL XVMESSAGE(' ',' ')
	END IF
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE SWAP(NI,VEC1,VEC2,X1,X2)
C
C	This routine swaps vectors VEC1 and VEC2, and swaps scalars X1 and X2
C
	REAL*4 VEC1(NI),VEC2(NI)
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
C*******************************************************************************
	SUBROUTINE COMPUTE_TRANSFORM(NI,EVAL,EVEC,CHAN_MEAN,CHAN_SIGMA,
     +				     CORR,DSCALE,XMEAN,SIGMA,
     +				     OMATRIX,OFFSET)
C
C	This routine computes the overall transformation needed to produce
C	a decorrelation stretched image. The matrix transformation is stored
C	array OMATRIX, and the necessary offsets to keep the results in the
C	0-255 range are stored in the array OFFSET.
C
C	NI	       - (input) integer number of input channels used
C	EVAL(NI)       - (input) real array of eigenvalues
C	EVEC(32,32)    - (input) real matrix of eigenvectors (rotation matrix)
C	CHAN_MEAN(NI)  - (input) real array of the means of the input images
C	CHAN_SIGMA(NI) - (input) real array of the standard deviations of the
C				      input images
C	CORR           - (input) logical flag; 
C				      TRUE => correlation matrix was used
C	DSCALE(NI)     - (input) real array of scaling factors to adjust the
C				      weights of each principal component.
C				      Default is for all values = 1.0
C	XMEAN          - (input) real target mean for each output channel.
C       SIGMA          - (input) real target standard deviation for each
C				      output channel.
C	OMATRIX(32,32) - (output)real transformation matrix needed to produce
C				      the output images from the inputs
C	OFFSET(NI)     - (output)real array of offsets applied to pixels after 
C				      the transformation matrix, to center 
C				      output values in 0-255 range.
C
	REAL EVEC(32,32),OMATRIX(32,32)
	REAL EVAL(NI),CHAN_MEAN(NI),CHAN_SIGMA(NI),DSCALE(NI),OFFSET(NI)
	REAL GAIN(32),GAININ(32)
	LOGICAL CORR
C
	DO I=1,NI
C				GAIN(I) is the vector that will be multiplied
C				with the rotation matrix. 
C				If an eigenvalue (EVAL) is 0.0, there is no
C				appropriate value for GAIN. EVAL cannot
C				correctly be less than 0.0
C					
	    IF (EVAL(I) .GT. 0.0) THEN
		GAIN(I) = DSCALE(I)*SIGMA/SQRT(EVAL(I))
	    ELSE
		GAIN(I) = 1.0
	    END IF
C				When using the correlation matrix eigenfunctions
C				the input data must first be variance 
C				normalized. The GAININ values effectively makes
C				this adjustment.
C
	    IF (CORR .AND. CHAN_SIGMA(I).NE.0.0) THEN
		GAININ(I) = 1.0/CHAN_SIGMA(I)
	    ELSE
		GAININ(I) = 1.0
	    END IF
	END DO
C				This loop generates the output matrix, which is
C				formed by the product of the rotation matrix
C				(EVEC), the vector of variance normalizing
C				values (GAIN), and the back rotation matrix
C				(the transpose of EVEC).
C				X is an accumulator for computing each output
C				     matrix element.
C				Y is used to compute the output location of the
C				     central input point. The OFFSET array is
C				     computed from Y, to force the output to be
C				     centered in the byte data range of 0-255.
	DO I=1,NI
	    Y = 0.0
	    DO J=1,NI
		X = 0.0
		DO K=1,NI
		    X = X + GAIN(K)*EVEC(I,K)*EVEC(J,K)
		END DO
		OMATRIX(J,I) = X * GAININ(J)
		Y = Y + OMATRIX(J,I)*CHAN_MEAN(J)
	    END DO
	    OFFSET(I) = XMEAN - Y
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE DO_TRANSFORM(INUNIT,IOUTUNIT,NBAND,IQA,ISL,ISS,NL,NS,
     +				NI,Q3D,Q3DOUT,OMATRIX,OFFSET,DATA)
C
C	This routine performs the decorrelation stretch transformation 
C	and produces the output file(s).
C
C	INUNIT   - input,integer array  Unit numbers of inputs
C	IOUTUNIT - input,integer array  Unit numbers of outputs
C	NBAND    - input,integer array  For 3D input, bands to be used
C	IQA      - input,integer        QA mask flag
C	ISL      - input,integer        Starting line of image
C	ISS      - input,integer        Starting sample of image
C	NL       - input,integer        Number of lines to process
C	NS       - input,integer        Number of samples to process
C	NI       - input,integer        Number of input (and output) bands
C	Q3D      - input,logical        Flag for 3-D input
C	Q3DOUT   - input,logical        Flag for 3-D output
C	OMATRIX  - input,real array     Transformation matrix
C       OFFSET   - input,real array     Offset to be applied after
C				        transformation matrix
C	DATA     - buffer,real array    Storage buffer for input data
C
	REAL OMATRIX(32,32),OFFSET(NI),DATA(NS,NI),XQA(10000)
	INTEGER INUNIT(NI),IOUTUNIT(NI),NBAND(NI)
	LOGICAL Q3D,Q3DOUT
	BYTE OUT(30000)
C								initialize QA 
	DO I=1,NS
	    XQA(I) = 0.0
	END DO
C
	IEL = ISL + NL - 1
	DO LINE=ISL,IEL
C								read in data
	    IF (Q3D) THEN
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT(1),DATA(1,IBAND),ISTAT,
     +				'BAND',NBAND(IBAND),'LINE',LINE,
     +				'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
		IF (IQA .GT. 0) CALL XVREAD(INUNIT(1),XQA,ISTAT,
     +					    'BAND',IQA,'LINE',LINE,
     +					    'SAMP',ISS,'NSAMPS',NS,' ')
	    ELSE
		DO IBAND=1,NI
		    CALL XVREAD(INUNIT(IBAND),DATA(1,IBAND),ISTAT,
     +		 		'LINE',LINE,'SAMP',ISS,'NSAMPS',NS,' ')
		END DO
		IF (IQA .GT. 0) CALL XVREAD(INUNIT(IQA),XQA,ISTAT,
     +			 		    'LINE',LINE,'SAMP',ISS,
     +					    'NSAMPS',NS,' ')
	    END IF
C								do transform
	    DO IBAND=1,NI
		DO ISAMP = 1,NS
		    IF (XQA(ISAMP) .NE. 144.0) THEN
			X = 0.0
			DO I = 1,NI
			    X = X + OMATRIX(I,IBAND)*DATA(ISAMP,I)
			END DO
			X = X + OFFSET(IBAND)
C						force the output to be within
C						the byte data range and unsigned
			N = NINT(MIN(MAX(X,0.0),255.0))
			IF (N.GE.128) N=N-256
			OUT(ISAMP) = N
		    ELSE
			OUT(ISAMP) = 0
		    END IF
		END DO
		IF (Q3DOUT) THEN
		    CALL XVWRIT(IOUTUNIT(1),OUT,ISTAT,' ')
		ELSE
		    CALL XVWRIT(IOUTUNIT(IBAND),OUT,ISTAT,' ')
		END IF
	    END DO
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE JACOBI(OMATRIX,NI,EVAL,EVEC)
C
	REAL OMATRIX(32,32),EVAL(32),EVEC(32,32),B(32),Z(32)
	CHARACTER*80 PRT
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
	DO WHILE (LOOP.LE.50 .AND. SUM .GT. 0.0)
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
		    THRESH2 = 1.0E10 * ABS(OMATRIX(IP,IQ))
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
	WRITE (PRT,100) LOOP
  100	FORMAT('Jacobi Routine needed',I3,' iterations')
	CALL XVMESSAGE(PRT,' ')
	RETURN
	END
