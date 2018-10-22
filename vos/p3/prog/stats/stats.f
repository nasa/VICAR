	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C     PROGRAM STATS
C      6 MAY  1977 ...JDA... INITIAL RELEASE
C     28 JUNE 1978 ...JDA... CHANGE CALL LABELC TO CALL LABELB
C     19 SEPT 1981 ...REA... FIX BUG THAT EATS CORE, AND
C                            CHANGE LABELB TO LABELC
C     10 APR  1983 ...REA... EXPAND INPUT BUFFER TO 19072 BYTES
C      1 OCT  1983 ...AJR... MODIFY PARAMETERS AND VAX CONVERSION
C     25 JUL  1985 ...REA... FIX BUGS TO MAKE THE 'ALL' FEATURE OF
C				HIST AND SPEC WORK
C     25 FEB  1986 ...SP.... CONVERTED TO VICAR2 CALLS.
C     25 FEB  1986 ...SP.... CHANGED TO ALLOW UP TO 12 INPUT FILES.
C     25 FEB  1986 ...SP.... RENAMED ROUTINE FORMAT AS HEADING TO AVOID CONFLICT
C     25 FEB  1986 ...SP.... CORRECTED BUG IN PFIELD FOR LISTING OF VERTICES.
C     11 OCT  1988 ...SP.... CORRECTED BUG IN PFIELD PRINTING MORE THAN 4 DIGITS
C     31 OCT  1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C     15 MAR  2001 ...REA... EXTENSIVE REWORKING
C
C        'STATS'   STATISTICS PROCESSOR PROGRAM
C
	IMPLICIT INTEGER (A-Z)
	INCLUDE 'fortport'
C
	REAL*4  MEAN(12,50),DEV(12,50),COV(78),COR(78),SMEAN
	REAL*8 XMEAN(12),XCOV(78),XPTS
	INTEGER*4 PARM(2000),EXCLUD(10),SPLOT(50),VERTS(50),CNAME(2,50)
	INTEGER*4 PTRBUF(12),BND(12),HIST(12),SPEC(12),IUNIT(12)
	INTEGER*4 HISBUF(256,12),CLSNAM(2),OUNIT(2)
	INTEGER*4 IEDGE_L(5000),IEDGE_S(5000),LOOPSTART(33),LOOPSEGS(33)
	INTEGER*2 INBUF(90000)
	BYTE OBUF(20000),BNAME(8,50)
	BYTE CLASS(8),NUM(10),BDN
	CHARACTER CCLASS(8), CNUM(10)
	EQUIVALENCE(CLASS,CCLAS), (NUM,CNUM)
	LOGICAL XVPTST,MSS,NOPRNT,ADD,STAT,SCRIBE
	CHARACTER*132 BUFFER
	CHARACTER*80 MSG
	CHARACTER*8 FORMAT
	EQUIVALENCE (CNAME,BNAME)
C
	DATA CCLASS/'C','L','A','S','S','X',' ',' '/
	DATA CNUM/'0','1','2','3','4','5','6','7','8','9'/
	DATA BND/1,2,3,4,5,6,7,8,9,10,11,12/
	DATA VERTS/50*1/
	DATA HIST/1,2,3,4,5,6,7,8,9,10,11,12/
	DATA SPEC/1,2,3,4,5,6,7,8,9,10,11,12/
C
	CALL IFMESSAGE('STATS version 13-MAR-01')
C								  GET PARAMETERS
	CALL XVPCNT('INP',NI)
C									     mss
	CALL XVPARM('MSS',NCHAN,ICOUNT,IDEF,1)
	IF(NCHAN .GT. 0) THEN
	    MSS = .TRUE.
C									    band
	    CALL XVPARM('BAND',PARM,ICOUNT,IDEF,32)
	    IF(PARM(1) .GT. 0) THEN
		NUSE = ICOUNT
		DO J=1,NUSE
		    BND(J) = PARM(J)
		END DO
	    ELSE
		NUSE = NCHAN
	    END IF
	ELSE
	    MSS = .FALSE.
	    NUSE = NI
	END IF
C									    hist
	CALL XVPARM('HIST',PARM,ICOUNT,IDEF,32)
	IF(PARM(1) .EQ. 0)   THEN
	    NHIST =N USE
	ELSE IF (PARM(1) .NE. -1)  THEN
	    NHIST = ICOUNT
	    DO J=1,NHIST
		HIST(J) = PARM(J)
	    END DO
	ELSE
	    NHIST = 0
	END IF
C									    spec
	CALL XVPARM('SPEC',PARM,ICOUNT,IDEF,32)
	IF(PARM(1) .EQ. 0)   THEN
	    NSPEC = NUSE
	ELSE IF (PARM(1) .NE. -1)  THEN
	    NSPEC = ICOUNT
	    DO J=1,NSPEC
		SPEC(J) = PARM(J)
	    END DO
	ELSE
	    NSPEC = 0
	END IF
C									    nopr
	NOPRNT = XVPTST('NOPRINT')
C									    excl
	CALL XVPARM('EXCLUDE',PARM,ICOUNT,IDEF,10)
	IF (ICOUNT .GT. 0) THEN
	    NEXCL = ICOUNT
	    DO J=1,NEXCL
		EXCLUD(J) = PARM(J)
	    END DO
	ELSE
	    NEXCL = 0
	END IF
C									   splot
	CALL XVPARM('SPLOT',PARM,ICOUNT,IDEF,32)
	IF(PARM(1).GT.0) THEN      
	    NPLOT = ICOUNT
	    DO J=1,NPLOT
		SPLOT(J) = PARM(J)
	    END DO
	ELSE
	    NPLOT = 0
	    DO J=1,50
		SPLOT(J) = J
	    END DO
	END IF
C									  scribe
	CALL XVPARM('SCRIBE',PARM,ICOUNT,IDEF,1)
	IF (PARM(1) .GT. 0) THEN
	    SCRIBE = .TRUE.
	    ISCRIBE = PARM(1)
C									      dn
	    CALL XVPARM('DN',PARM,ICOUNT,IDEF,1)
	    IF (ICOUNT .GT. 0) THEN
		BDN = INT2BYTE(PARM(1))
		ADD = .FALSE.
	    ELSE
		ADD = .TRUE.
	    END IF
	ELSE
	    SCRIBE = .FALSE.
	END IF
C									    vert
	CALL XVPARM('VERT',PARM,ICOUNT,IDEF,50)
	IF (PARM(1) .GT. 0) THEN
	    DO J=1,ICOUNT
		IF(PARM(J).LE.50 .AND. PARM(J).GE.1) VERTS(PARM(J))=2
	    END DO
	END IF
C							   open input dataset(s)
	DO I=1,NI
	    CALL XVUNIT(IUNIT(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(IUNIT(I),ISTAT, 'OP', 'READ','U_FORMAT','HALF',
     +			'OPEN_ACT','SA','IO_ACT','SA',' ')
	END DO
C							 sort out sizes & format
	CALL XVGET(IUNIT(1),ISTAT,'FORMAT',FORMAT,'NL',NLI,'NS',
     +                 NSI,' ')
	IF(FORMAT.NE.'BYTE') THEN
	    CALL XVMESSAGE('STATS accepts byte data only',' ')
	    CALL ABEND
	ENDIF
	IF (MSS) THEN
	    NSCHAN = NSI/NCHAN
	    IF (NSI .GT. 90000) THEN
		CALL XVMESSAGE(
     +		'Input image size greater than 90,000 samples wide',' ')
		CALL ABEND
	    ENDIF
	ELSE
	    NSCHAN = NSI
	    IF (NI*NSI .GT. 90000) THEN
		CALL XVMESSAGE(
     +			'# inputs * # samples greater than 90000',' ')
		CALL ABEND
	    ENDIF
	END IF
	MTRX = (NUSE*(NUSE+1))/2
	NSO = 4*(NUSE+MTRX+3)
C							  open output dataset(s)
	CALL XVPCNT('OUT',NO)
	DO I=1,NO
	    CALL XVUNIT(OUNIT(I),'OUT',I,ISTAT,' ')
	END DO
	IF (NO .EQ. 2) THEN
	    STATUNIT = OUNIT(1)
	    OSCRIBE = OUNIT(2)
	    SCRIBE = .TRUE.
	    STAT = .TRUE.
	ELSE
	    IF (SCRIBE) THEN
		OSCRIBE = OUNIT(1)
		STAT = .FALSE.
	    ELSE
		STATUNIT = OUNIT(1)
		STAT = .TRUE.
	    END IF
	END IF
C
	IF(STAT) THEN  
	    CALL XVOPEN(STATUNIT, ISTAT, 'OP', 'WRITE', 'U_NL',50,
     +			'U_NS',NSO,'OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','BYTE','O_FORMAT','BYTE',' ')
	END IF
	IF (SCRIBE)  THEN
	    CALL ZIA(HISBUF(1,1),256)
	    CALL XVOPEN(OSCRIBE,ISTAT,'OP','WRITE','U_FORMAT','HALF',
     +			'U_NL',NLI,'U_NS',NSCHAN,'OPEN_ACT','SA',
     +			'IO_ACT','SA',' ')
C							 copy the scribe picture
C							 and compute image mean
	    IF (MSS) THEN
		PTR = NSCHAN*(ISCRIBE-1) + 1
		ISCRUNIT = IUNIT(1)
	    ELSE
		PTR = 1
		ISCRUNIT = IUNIT(ISCRIBE)
	    END IF
	    DO II=1,NLI
		CALL XVREAD(ISCRUNIT,INBUF,ISTAT,'SAMP',PTR,'NSAMPS',
     +			    NSCHAN,' ')
		IF (ADD) CALL BLDHIST(INBUF,HISBUF,1,NSCHAN)
		CALL XVWRIT(OSCRIBE,INBUF, ISTAT,' ')
	    END DO
	    ISUM = 0
	    DO J=1,255
		ISUM = ISUM + J*HISBUF(J+1,1)
	    END DO
	    SMEAN = FLOAT(ISUM) / (FLOAT(NSCHAN)*FLOAT(NLI))
C
	    CALL XVCLOSE(OSCRIBE, ISTAT,' ')
	    CALL XVOPEN(OSCRIBE, ISTAT, 'OP', 'UPDATE', 'U_NL',NLI,
     +			'U_NS',NSCHAN,'OPEN_ACT','SA','IO_ACT','SA',' ')
	END IF
C						 initialize buffers and pointers
	NCLS = 0
	PARNXT = 0
	DO J=1,NUSE
	    PTRBUF(J) = NSCHAN*(BND(J)-1) + 1
	END DO
C
	IF(NEXCL.GT.0)  CALL PRNT(4,NEXCL,EXCLUD,
     & 		      'THE FOLLOWING DN ARE EXCLUDED FROM STATISTICS:.')
C
C	    loop through the CLASSnn parameters and handle the specified classes
C
	DO ICLASS = 1,50
	    IF (ICLASS .LT. 10) THEN
		WRITE(MSG,200) ICLASS
  200		FORMAT('CLASS',I1)
	    ELSE
		WRITE(MSG,210) ICLASS
  210		FORMAT('CLASS',I2)
	    END IF
	    CALL XVPARM(MSG,PARM,ICOUNT,IDEF,2000)
	    IF (ICOUNT .GE. 4) THEN
C							   we have another class
		NCLS = NCLS+1
C							     form the class name
		CALL MVE(1,8,CLASS,BNAME(1,NCLS),1,1)
		IF (ICLASS .LT. 10) THEN
		    BNAME(6,NCLS) = NUM(ICLASS) + 1
		ELSE
		    BNAME(6,NCLS) = NUM(ICLASS/10) + 1
		    BNAME(7,NCLS) = NUM(MOD(ICLASS,10)+1)
		END IF
		IF (NHIST.GT.0)  CALL ZIA(HISBUF(1,1),3072)
		CALL ZIA(XMEAN,24)
		CALL ZIA(XCOV,156)
		NPTS = 0
		FORM = VERTS(ICLASS)
C							 print the class heading
		IF (.NOT.NOPRNT) THEN
		    CALL XVMESSAGE(' ',' ')
		    CALL XVMESSAGE('******************************',' ')
		    WRITE(MSG,310) NCLS,CNAME(1,NCLS),CNAME(2,NCLS)
  310		    FORMAT('TRAINING AREAS FOR CLASS #',I2,' "',2A4,'"')
		    CALL XVMESSAGE(MSG,' ')
		    IF (FORM .EQ. 2) THEN
			CALL XVMESSAGE('IRREGULAR AREA VERTICES',' ')
			WRITE (MSG,320)
  320			FORMAT(6(' LINE SAMPLE '))
			CALL XVMESSAGE(MSG,' ')
		    END IF
		END IF
C
		IF (FORM .EQ. 1) THEN
C							     process rectangular
C								training fields
		    IF (MOD(ICOUNT,4) .NE. 0) THEN
			CALL XVMESSAGE(
     +		      'Error in specifying this training area',' ')
			CALL ABEND
		    ENDIF
		    DO PAR=1,ICOUNT,4
			IF(.NOT.NOPRNT) CALL PFIELD(FORM,PARM,PAR,I)
			ISL = PARM(PAR)
			ISS = PARM(PAR+1)
			NS = PARM(PAR+3)
			IEL = ISL + PARM(PAR+2) - 1
			IES = ISS + NS -1
			DO LINE=ISL,IEL
			    CALL READLINE(LINE,INBUF,MSS,IUNIT,NUSE,
     +					  PTRBUF)
			    DO J=1,NHIST
				K = HIST(J)
				CALL BLDHIST(INBUF(PTRBUF(K)),
     +					     HISBUF(1,K),ISS,IES)
			    END DO
			    CALL BLDSUMS(INBUF,PTRBUF,NUSE,ISS,IES,
     +					 XMEAN,XCOV,NPTS,NEXCL,EXCLUD)
			END DO
		    END DO
		ELSE
C								process vertices
C								training fields
		    NLOOPS = 0
		    NEDGE = 0
		    PAR = 1
		    DO WHILE (PAR+3 .LE. ICOUNT)
C							      find a closed loop
			PTR = PAR
			DO I=1,(ICOUNT/2) - 1
			    PTR= PTR+2         
			    IF(PARM(PTR).EQ.PARM(PAR) .AND. 
     +				PARM(PTR+1).EQ.PARM(PAR+1)) GO TO 330
			END DO
			CALL XVMESSAGE(
     +			   'Vertices training area does not close',' ')
			CALL ABEND
  330			CONTINUE
			IF (.NOT.NOPRNT) CALL PFIELD(FORM,PARM,PAR,I)
			CALL FIND_EDGES(PARM(PAR),I,IEDGE_L,IEDGE_S,
     +					NEDGE)
C							 save the loop locations
			NLOOPS = NLOOPS + 1
			LOOPSTART(NLOOPS) = PAR
			LOOPSEGS(NLOOPS) = I
			PAR = PAR + 2*(I+1)
		    END DO
		    CALL SORT_EDGES(IEDGE_L,IEDGE_S,NEDGE)
C
		    LASTLINE = 0
		    DO I=1,NEDGE,2
			IF (IEDGE_L(I) .NE. IEDGE_L(I+1)) THEn
			    CALL XVMESSAGE(
     +				'Unresolved vertex error by STATS',' ')
			    CALL ABEND
			ENDIF
			IF (IEDGE_L(I) .NE. LASTLINE) THEN
			    LASTLINE =IEDGE_L(I)
			    CALL READLINE(LASTLINE,INBUF,MSS,IUNIT,
     +					  NUSE,PTRBUF)
			END IF
			ISS = IEDGE_S(I)
			IES = IEDGE_S(I+1)
			DO J=1,NHIST
			    K = HIST(J)
			    CALL BLDHIST(INBUF(PTRBUF(K)),
     +					 HISBUF(1,K),ISS,IES)
			END DO
			CALL BLDSUMS(INBUF,PTRBUF,NUSE,ISS,IES,
     +				     XMEAN,XCOV,NPTS,NEXCL,EXCLUD)
		    END DO
		END IF
C								   compute stats
		XPTS = NPTS
		KL = 0
C								     covariances
		DO J=1,NUSE
		    DO L = 1,J
			KL = KL+1
			IF (XPTS .GT. 1.0) THEN
			    XCOV(KL) = (XCOV(KL) -
     +				     XMEAN(J)*XMEAN(L)/XPTS)/(XPTS-1.0)
			ELSE
			    XCOV(KL) = 0.0
			END IF
			COV(KL) = XCOV(KL)
		    END DO
C							     standard deviations
		    IF (XCOV(KL).GT.0.0) THEN
			DEV(J,NCLS) = DSQRT(XCOV(KL))
		    ELSE
			DEV(J,NCLS) = 0.0
		    END IF
		END DO
C							  means and correlations
		KL= 0
		DO J=1,NUSE
		    MEAN(J,NCLS) = XMEAN(J)/XPTS
		    DO L=1,J
			KL = KL+1
			IF (DEV(J,NCLS).EQ.0.0 .OR. DEV(L,NCLS).EQ.0.0)
     +								   THEN
			    COR(KL) = 0.0
			ELSE
			    COR(KL)= COV(KL)/(DEV(J,NCLS)*DEV(L,NCLS))
			END IF
		    END DO
		END DO
C								  report results
		IF(.NOT. NOPRNT) THEN
		    WRITE(BUFFER,400) NPTS
  400		    FORMAT(5X,'TOTAL POINTS =',I7)
		    CALL XVMESSAGE(BUFFER,' ')
		    WRITE(BUFFER,410) NCLS,CNAME(1,NCLS),CNAME(2,NCLS)
  410		    FORMAT(' STATISTICS FOR CLASS # ',I2,'   "',2A4,'"')
		    CALL XVMESSAGE(BUFFER,' ')
		    WRITE(BUFFER,420) (I,I=1,NUSE)
  420		    FORMAT('   CHANNEL',4X,12(I2,7X))
		    CALL XVMESSAGE(BUFFER,' ')
		    WRITE(BUFFER,430) (MEAN(I,NCLS),I=1,NUSE)
  430		    FORMAT('    MEAN ',12F9.2)
		    CALL XVMESSAGE(BUFFER,' ')
		    WRITE(BUFFER,440) (DEV(I,NCLS),I=1,NUSE)
  440		    FORMAT('   ST DEV',12F9.2)
		    CALL XVMESSAGE(BUFFER,' ')
		    CALL XVMESSAGE('   COVARIANCE MATRIX',' ')
		    KL1 = 1
		    DO J=1,NUSE
			KL1 = KL1+J-1
			KL2 = KL1+J-1
			WRITE(BUFFER,460) (COV(I),I=KL1,KL2)
  460			FORMAT(9X,12F9.2)
			CALL XVMESSAGE(BUFFER,' ')
		    END DO
		    CALL XVMESSAGE('   CORRELATION MATRIX',' ')
		    KL1 = 1
		    DO J=1,NUSE
			KL1 = KL1+J-1
			KL2 = KL1+J-1
			WRITE(BUFFER,470) (COR(I),I=KL1,KL2)
  470			FORMAT(9X,12F9.4)
			CALL XVMESSAGE(BUFFER,' ')
		    END DO
		END IF
		CALL MVE(1,8,CNAME(1,NCLS),CLSNAM,1,1)
		IF(NHIST.GT.0)  CALL HISTGM(HISBUF,HIST,NHIST,CLSNAM,
     +					    NEXCL,EXCLUD)
C				write means & covariance matrix on stat data set
		IF (STAT) THEN
		    NN = 4*NUSE
		    NM = 4*MTRX
		    CALL MVE(1,8,CNAME(1,NCLS),OBUF(1),1,1)
		    CALL MVE(7,NUSE,MEAN(1,NCLS),OBUF(9),1,1)
		    CALL MVE(7,1,NPTS,OBUF(NN+9),1,1)
		    CALL MVE(7,MTRX,COV(1),OBUF(NN+13),1,1)
		    CALL XVWRIT( STATUNIT, OBUF, ISTAT,' ')
		END IF
C						  do the scribing on the picture
		IF (SCRIBE) THEN
C								  set outline dn
		    IF (ADD) THEN
			IF (MEAN(ISCRIBE,NCLS) .GE. SMEAN) THEN
			    BDN = INT2BYTE(0)
			ELSE
			    BDN = INT2BYTE(255)
			END IF
		    END IF
C
		    IF (FORM .EQ. 1) THEN
C								      rectangles
			DO PAR=1,ICOUNT,4
			    ISL = PARM(PAR)
			    ISS = PARM(PAR+1)
			    NL = PARM(PAR+2)
			    NS = PARM(PAR+3)
			    IEL = ISL + NL - 1
			    IES = ISS + NS - 1
C									top line
			    CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',ISL,
     +					' ')
			    CALL MVE(1,NS,BDN,OBUF(ISS),0,1)
			    CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',ISL,
     +					' ')
C									   sides
			    DO LINE=ISL+1,IEL-1
				CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',
     +					    LINE,' ')
				OBUF(ISS) = BDN
				OBUF(IES) = BDN
				CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',
     +					    LINE,' ')
			    END DO
C									bot line
			    CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',IEL,
     +					' ')
			    CALL MVE(1,NS,BDN,OBUF(ISS),0,1)
			    CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',IEL,
     +					' ')
			END DO
		    ELSE
C									vertices
			DO I=1,NLOOPS
			    CALL DRAW_VERTS(PARM(LOOPSTART(I)),
     +					LOOPSEGS(I),OSCRIBE,OBUF,BDN)
			END DO
		    END IF
		END IF
		PAR = PAR + PARNXT
	    END IF
	END DO
C
	IF (NCLS .EQ. 0) THEN
	    CALL XVMESSAGE('*** NO TRAINING AREAS SPECIFIED',' ')
	    CALL ABEND
	ENDIF
C
C							   print the class table
	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE(' CLASS NUMBERS ASSIGNED',' ')
	CALL XVMESSAGE(' ----------------------',' ')
	DO J=1,NCLS
	    WRITE(BUFFER,700) J,CNAME(1,J),CNAME(2,J)
  700	    FORMAT(I6,' = ',2A4)
	    CALL XVMESSAGE(BUFFER,' ')
	END DO
C
	IF(NPLOT.EQ.0)  NPLOT= NCLS
	IF(NSPEC.GT.0)  CALL SPECTL(SPEC,SPLOT,NSPEC,NPLOT,DEV,MEAN)

	IF (SCRIBE) CALL XVCLOSE(OSCRIBE, ISTAT,' ')
C
	IF (STAT) THEN
	    CALL XLDEL( STATUNIT, 'SYSTEM', 'NL', ISTAT,' ')
	    CALL XLADD(STATUNIT,'SYSTEM','NL',NCLS,ISTAT,
     +					'FORMAT','INT',' ')
	    CALL XVCLOSE(STATUNIT, ISTAT,' ')
	END IF
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE PFIELD(FORM,PARM,PAR,NPNTS)
C
	IMPLICIT INTEGER (A-Z)
	CHARACTER*132 BUFFER
	INTEGER*4 PARM(2000)
C
	IF (FORM .EQ. 1) THEN
	    WRITE(BUFFER,100) PARM(PAR),PARM(PAR+1),PARM(PAR+2),
     +			      PARM(PAR+3)
  100	    FORMAT(9X,'SL=',I5,'   SS=',I5,'   NL=',I5,'   NS=',I5)
	    CALL XVMESSAGE(BUFFER,' ')
	ELSE
	    NPT2 = 2*(NPNTS+1)
	    DO K=1,NPT2,12
		KEND = MIN(K+11,NPT2)
		WRITE(BUFFER,200) (PARM(PAR+I-1),I=K,KEND)
  200		FORMAT(6(I5,I7,X))
		CALL XVMESSAGE(BUFFER,' ')
	    END DO
	END IF
C
	RETURN
	END
C*******************************************************************************
      SUBROUTINE SPECTL(SPEC,SPLOT,NSPEC,NPLOT,DEV,MEAN)
      IMPLICIT INTEGER (A-Z)
      INTEGER SPEC(12),SPLOT(50)
      REAL DEV(12,50),MEAN(12,50)
      CHARACTER*132 BUFFER
      CHARACTER BLANK(132),BAR,DASH
C
      DATA BLANK/132*' '/
      DATA BAR/'|'/
      DATA DASH/'-'/
C
      CALL XVMESSAGE('                                                  
     +   SPECTRAL PLOT',' ')
      LINE_COUNT = 1
C					plot each requested spectral band
      DO I=1,NSPEC
	  LINE_COUNT = LINE_COUNT+NPLOT+12
	  IF (LINE_COUNT .GT. 60) THEN
	      CALL XVMESSAGE('1',' ')
	      LINE_COUNT = NPLOT+13
	  END IF
	  II = SPEC(I)
	  CALL XVMESSAGE(' ',' ')
	  CALL XVMESSAGE(' ',' ')
	  CALL XVMESSAGE(' ',' ')
	  CALL XVMESSAGE(' ',' ')
          BUFFER(1:12) = ' C     BAND '
          WRITE (BUFFER(13:14),'(I2)') II
          CALL XVMESSAGE(BUFFER(2:14),' ')
	  CALL XVMESSAGE('L            +/- 1 STANDARD DEVIATION',' ')
	  CALL XVMESSAGE('A',' ')
	  CALL XVMESSAGE('S',' ')
          buffer(1:46)='S 0   10   20   30   40   50   60   70   80   '
          buffer(47:91)='90   100  110  120  130  140  150  160  170  '
          buffer(92:131)='180  190  200  210  220  230  240  250  '
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('S 0   10   20   30   40   50   60   70   80   90
c     +   100  110  120  130  140  150  160  170  180  190  200  210  220
c     +  230  240  250  ',' ')

          buffer(1:46)='--|----|----|----|----|----|----|----|----|---'
          buffer(47:88)='-|----|----|----|----|----|----|----|----|'
          buffer(89:131)='----|----|----|----|----|----|----|----|---'
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('--|----|----|----|----|----|----|----|----|----|
c     +----|----|----|----|----|----|----|----|----|----|----|----|----|-
c     +---|----|----|---',' ')
C
C						plot each requested class
C
	  DO J=1,NPLOT
	      JJ = SPLOT(J)
C              CALL MVLC(BLANK,BUFFER(1:132),132)
              BUFFER = ' '
              WRITE (BUFFER(1:3),'(I3)') J
	      LEFT = (MEAN(II,JJ)-DEV(II,JJ)+9)/2.0
	      RIGHT = (MEAN(II,JJ)+DEV(II,JJ)+9)/2.0
	      LEFT = MIN(132,MAX(4,LEFT))
	      RIGHT = MIN(132,MAX(4,RIGHT))
	      DO K=LEFT+1,RIGHT-1
                  BUFFER(K:K) = '-'
	      END DO
              BUFFER(LEFT:LEFT) = '|'
              BUFFER(RIGHT:RIGHT) = '|'
	      IF (JJ.LT.10) THEN
		      CENTER = (MEAN(II,JJ)+9.0)/2.0
                      WRITE (BUFFER(CENTER-0:CENTER),'(I1)') JJ
		  ELSE
		      CENTER = (MEAN(II,JJ)+10.0)/2.0
                      WRITE (BUFFER(CENTER-1:CENTER),'(I2)') JJ
	      END IF
              CALL XVMESSAGE(BUFFER(2:132),' ')
	  END DO
          buffer(1:46)='--|----|----|----|----|----|----|----|----|---'
          buffer(47:88)='-|----|----|----|----|----|----|----|----|'
          buffer(89:131)='----|----|----|----|----|----|----|----|---'
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('--|----|----|----|----|----|----|----|----|----|
c     +----|----|----|----|----|----|----|----|----|----|----|----|----|-
c     +---|----|----|---',' ')

          buffer(1:46)='  0   10   20   30   40   50   60   70   80   '
          buffer(47:91)='90   100  110  120  130  140  150  160  170  '
          buffer(92:131)='180  190  200  210  220  230  240  250  '
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('   0   10   20   30   40   50   60   70   80   90
c     +   100  110  120  130  140  150  160  170  180  190  200  210  220
c     +  230  240  250  ',' ')
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HISTGM(TALLY,HISVEC,NOHIST,TTL,NEXCL,EXCLUD)
      IMPLICIT INTEGER (A-Z)
      include 'fortport'
      character*132 string
      CHARACTER*132 BUFFER
      INTEGER TALLY(256,12),TTL(2),indx
      character*244 hisbuf
      INTEGER HISVEC(12),EXCLUD(10)
      REAL XSCALE,XSHFT
      INTEGER*2 XAXIS(14)
      character*4 char
      character*1 sym
      byte sbyte
      DATA MOVE/'00000041'X/
C
C
C        INITIALIZE
      PAGSIZ= 70
      YSIZ= 15
      INC= 2
      XSIZ= 121
      XLOW= 0
      XHGH= XLOW+INC*(XSIZ-1)
      XSIZ2= INC*(XSIZ-1)+1
C
      JPTCNT= PAGSIZ/(YSIZ+9)
      DSIZ= (XSIZ+8)/10+1
      XSCALE= FLOAT(XLOW-XHGH)/(XSIZ-1)
      XSHFT= FLOAT(XSIZ*XHGH-XLOW)/(XSIZ-1)
      DO 90 I=1,DSIZ
      K= DSIZ-I+1
90    XAXIS(K)= (10*I-9)*XSCALE+XSHFT+0.501
C
C
      JCNT= JPTCNT
      DO 900 JF=1,NOHIST
      JFEAT= HISVEC(JF)
      IF(JCNT.LT.JPTCNT)  GO TO 300
      call xvmessage(' ',' ')
      WRITE(BUFFER,1002) TTL(1),TTL(2)
1002  FORMAT('HISTOGRAM FOR:  "',2A4,'"')
      CALL XVMESSAGE(BUFFER,' ')
      JCNT= 0
C
C        SCALE & PRINT THE HISTOGRAM
300   MAX= 0
      JJ1= XHGH+3
      DO 350 J=(JJ1+1),256
      TALLY(JJ1,JFEAT)= TALLY(JJ1,JFEAT)+TALLY(J,JFEAT)
350   TALLY(J,JFEAT)= 0
      IF(NEXCL.EQ.0)  GO TO 375
      DO 360 J=1,NEXCL
      JJ= EXCLUD(J)+1
360   TALLY(JJ,JFEAT)= 0
375   YSCALE= 1
      JCNT= JCNT+1
      DO 400 K=1,(XSIZ2+INC),INC
      J= XLOW+K
      JK= TALLY(J,JFEAT)+TALLY(J+1,JFEAT)
400   IF(JK.GT.MAX)  MAX= JK
      IF(MAX.GT.YSIZ)  YSCALE= (MAX+(YSIZ-1))/YSIZ
      WRITE(BUFFER,1004)  JFEAT
1004  FORMAT('CHANNEL ',I2)
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,6004)  YSCALE
6004  FORMAT('   EACH * REPRESENTS',I3,'  POINT(S).')
      CALL XVMESSAGE(BUFFER,' ')
C
      DO 600 JY=1,YSIZ
      JH= (YSIZ-(JY-1))*YSCALE
      IK= JH-YSCALE
      DO 500 I=1,(XSIZ2+INC),INC
      HISBUF(I:i)= ' '
      IZ= XLOW+I
      JK= TALLY(IZ,JFEAT)+TALLY(IZ+1,JFEAT)
      SYM= '*'
      IF(JK.GE.JH)  GO TO 490
      IF(JK.LE.IK)  GO TO 500
      JK= JK-IK
      NUMIC= JK
C      CALL BINBCD(NUMIC,CHAR)
      write(char(4:4),'(i1)') numic
      
      sym(1:1)=char(4:4)
      IF(JK.LT.10)  GO TO 490
      JK= JK-10
      jk = jk+move
      sbyte = int2byte(jk)
      jk = jk - move
      write(sym,'(a1)') sbyte
      if(JK.LT.100)then
        write(char(1:2),'(I2)')JK
      else 
        write(char(1:3),'(I3)')JK
      end if 
         
      IF(JK.LT.26)  GO TO 490
      SYM(1:1)= '$'
490   continue
      HISBUF(I:i)= SYM(1:1)
500   CONTINUE
      indx = 1
      do 505 i = 1,122
         string(i:i) = hisbuf(indx:indx)
         indx = indx + 2
505   continue
C
      write(buffer,8700) jh,string(1:122)
8700  format(i4,' I',1x,a122)
      call xvmessage(buffer,' ')

c      WRITE(BUFFER,5002)  JH,(HISBUF(I),I=1,(XSIZ2+INC),INC)
c5002  FORMAT(1X,I4,' I',1X,124A1)
c      CALL XVMESSAGE(buffer,' ')
600   CONTINUE
      WRITE(BUFFER,6001)
6001  FORMAT(7X,12('+---------'),'+=>')
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,6002)  (XAXIS(I),I=1,DSIZ)
6002  FORMAT(5X,12(I3,7X),I3)
      CALL XVMESSAGE(BUFFER,' ')
900   CONTINUE
      RETURN
      END
C*******************************************************************************
	SUBROUTINE READLINE(LINE,INBUF,MSS,IUNIT,NUSE,PTRBUF)
C
	IMPLICIT NONE
	INTEGER*4 LINE,IUNIT(12),NUSE,PTRBUF(12),ISTAT,I
	INTEGER*2 INBUF(*)
	LOGICAL MSS
C
	IF (MSS) THEN
	    CALL XVREAD(IUNIT(1),INBUF,ISTAT,'LINE',LINE,' ')
	ELSE
	    DO I=1,NUSE
		CALL XVREAD(IUNIT(I),INBUF(PTRBUF(I)),ISTAT,
     +			    'LINE',LINE,' ')
	    END DO
	END IF
	RETURN
	END
C*******************************************************************************
	SUBROUTINE BLDHIST(INBUF,HISBUF,ISS,IES)
C
	IMPLICIT NONE
	INTEGER*4 HISBUF(*),ISS,IES,I,N
	INTEGER*2 INBUF(*)
C
	DO I=ISS,IES
	    N = INBUF(I) + 1
	    HISBUF(N) = HISBUF(N) + 1
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE BLDSUMS(INBUF,PTRBUF,NUSE,ISS,IES,XMEAN,XCOV,NPTS,
     +			   NEXCL,EXCLUD)
C
	IMPLICIT NONE
	REAL*8 XMEAN(12),XCOV(78)
	INTEGER*4 PTRBUF(12),NUSE,ISS,IES,NPTS,NEXCL,EXCLUD(10),ISAMP
	INTEGER*4 ICHAN,ITEST,KL,JCHAN,IEXCL
	INTEGER*2 INBUF(*),IDN(12)
C
	DO ISAMP=ISS,IES
	    DO ICHAN=1,NUSE
		IDN(ICHAN) = INBUF(PTRBUF(ICHAN)+ISAMP-1)
	    END DO
	    DO IEXCL=1,NEXCL
		ITEST = EXCLUD(IEXCL)
		DO ICHAN=1,NUSE
		    IF (IDN(ICHAN).EQ.ITEST) GO TO 500
		END DO
	    END DO
	    KL = 0
	    DO ICHAN=1,NUSE
		XMEAN(ICHAN) = XMEAN(ICHAN) + IDN(ICHAN)
		DO JCHAN=1,ICHAN
		    KL = KL + 1
		    XCOV(KL) = XCOV(KL) + IDN(ICHAN)*IDN(JCHAN)
		END DO
	    END DO
	    NPTS = NPTS +1
  500	    CONTINUE
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE FIND_EDGES(IVERT,NSEGS,IEDGE_L,IEDGE_S,NEDGE)
C
C	This routine finds all of the edge pixels of a training field,
C	storing the line locations in the IEDGE_L array, and the sample
C	locations in the IEDGE_S array. Edge pixels are inside the training
C	field.
C
	IMPLICIT NONE
	REAL*4 SLOPE,SAMP
	INTEGER*4 IVERT(2,1000),IEDGE_L(5000),IEDGE_S(5000),NSEGS,NEDGE
	INTEGER*4 ISEG,INC,I,ISTART,IEND,MMAX,MMIN
C
	DO ISEG=1,NSEGS
	    IF (IVERT(1,ISEG+1) .NE. IVERT(1,ISEG)) THEN
		SLOPE = FLOAT(IVERT(2,ISEG+1)-IVERT(2,ISEG)) /
     +			     (IVERT(1,ISEG+1)-IVERT(1,ISEG))
		IF (IVERT(1,ISEG+1) .GT. IVERT(1,ISEG)) THEN
		    INC = 1
		ELSE
		    INC = -1
		    SLOPE = -SLOPE
		END IF
		SAMP = IVERT(2,ISEG)
		ISTART = IVERT(1,ISEG)
		IEND = IVERT(1,ISEG+1)
		IF (ISEG.GT.1) THEN
C
C			The first point of this segment is a duplicate of the
C			last point of the previous segment.  We want to keep the
C			duplicate, if it is a local minimum or maximum line, but
C			discard it otherwise.
C
		    MMAX = MAX(IVERT(1,ISEG-1),IVERT(1,ISEG),
     +			       IVERT(1,ISEG+1))
		    MMIN = MIN(IVERT(1,ISEG-1),IVERT(1,ISEG),
     +			       IVERT(1,ISEG+1))
		    IF ((IVERT(1,ISEG) .NE. MMAX) .AND.
     +			(IVERT(1,ISEG) .NE .MMIN)) THEN
			ISTART = IVERT(1,ISEG) + INC
			IEND = IVERT(1,ISEG+1)
			SAMP = SAMP + SLOPE
		    END IF
		END IF
		DO I=ISTART,IEND,INC
		    NEDGE = NEDGE + 1
		    IEDGE_L(NEDGE) = I
		    IEDGE_S(NEDGE) = NINT(SAMP)
		    SAMP = SAMP + SLOPE
		END DO
	    END IF
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SORT_EDGES(LINE,ISAMP,NUM)
C
	IMPLICIT NONE
	INTEGER*4 LINE(*),ISAMP(*),NUM,I,J,K,L,M,IHOLD
C								sort
	M = NUM/2
	DO WHILE (M.GT.0)
	    K = NUM-M
	    J = 1
	    DO WHILE (J.LE.K)
 		I = J
		DO WHILE (I.GE.1)
		    L = I+M
		    IF ((LINE(I).EQ.LINE(L) .AND. ISAMP(I).GT.ISAMP(L))
     +			.OR. LINE(I).GT.LINE(L))	THEN
			IHOLD = LINE(I)
			LINE(I) = LINE(L)
			LINE(L) = IHOLD
			IHOLD = ISAMP(I)
			ISAMP(I) = ISAMP(L)
			ISAMP(L) = IHOLD
			I = I-M
		    ELSE
			I = -1
		    END IF
		END DO
		J = J+1
	    END DO
	    M = M/2
	END DO
C
	RETURN
	END
C*******************************************************************************
	SUBROUTINE DRAW_VERTS(PARM,NSEGS,OSCRIBE,OBUF,BDN)
C
C	This routine scribes a vertices training area onto the scribe output.
C
	IMPLICIT NONE
	REAL*4 X,XINC
	INTEGER*4 PARM(2000),NSEGS,OSCRIBE,I,ISL,ISS,IEL,IES,LINE,ISTAT
	INTEGER*4 NS,I1,I2,ISAMP,INC
	BYTE OBUF(*),BDN
C
	DO I=1,2*NSEGS,2
	    IF (PARM(I+2) .GE. PARM(I)) THEN
		ISL = PARM(I)
		ISS = PARM(I+1)
		IEL = PARM(I+2)
		IES = PARM(I+3)
	    ELSE
		ISL = PARM(I+2)
		ISS = PARM(I+3)
		IEL = PARM(I)
		IES = PARM(I+1)
	    END IF
C
	    IF (ISS .EQ. IES) THEN
C								        vertical
		DO LINE=ISL,IEL
		    CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',LINE,' ')
		    OBUF(ISS) = BDN
		    CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',LINE,' ')
		END DO
	    ELSE IF (ISL .EQ. IEL) THEN
C								      horizontal
		NS = ABS(ISS-IES) + 1
		ISS = MIN(ISS,IES)
		CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',ISL,' ')
		CALL MVE(1,NS,BDN,OBUF(ISS),0,1)
		CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',ISL,' ')
	    ELSE
		X = FLOAT(ISS)
		XINC = FLOAT(IES-ISS) / FLOAT(IEL-ISL)
		IF (ABS(XINC) .LT. 1.0) THEN
C								 mostly vertical
		    DO LINE=ISL,IEL
			ISAMP = NINT(X)
			CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',LINE,' ')
			OBUF(ISAMP) = BDN
			CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',LINE,' ')
			X = X + XINC
		    END DO
		ELSE
C							       mostly horizontal
		    IF (XINC .GT. 0.0) THEN
			INC = 1
		    ELSE
			INC = -1
		    END IF
		    I2 = ISS - INC
		    X = X + XINC/2.0
		    DO LINE=ISL,IEL-1
			I1 = I2 + INC
			I2 = INT(X)
			CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',LINE,' ')
			DO ISAMP = I1,I2,INC
			    OBUF(ISAMP) = BDN
			END DO
			CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',LINE,' ')
			X = X + XINC
		    END DO
		    I1 = I2 + INC
		    I2 = IES
		    CALL XVREAD(OSCRIBE,OBUF,ISTAT,'LINE',IEL,' ')
		    DO ISAMP = I1,I2,INC
			OBUF(ISAMP) = BDN
		    END DO
		    CALL XVWRIT(OSCRIBE,OBUF,ISTAT,'LINE',IEL,' ')
		END IF
	    END IF
	END DO
C
	RETURN
	END
