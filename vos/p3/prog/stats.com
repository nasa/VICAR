$!****************************************************************************
$!
$! Build proc for MIPL module stats
$! VPACK Version 1.8, Thursday, April 05, 2001, 15:11:42
$!
$! Execute by entering:		$ @stats
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
$ write sys$output "*** module stats ***"
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
$ write sys$output "Invalid argument given to stats.com file -- ", primary
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
$   if F$SEARCH("stats.imake") .nes. ""
$   then
$      vimake stats
$      purge stats.bld
$   else
$      if F$SEARCH("stats.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stats
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stats.bld "STD"
$   else
$      @stats.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stats.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stats.com -
	-s stats.f -
	-i stats.imake -
	-p stats.pdf -
	-t tststats.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stats.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create stats.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM stats

   To Create the build file give the command:

		$ vimake stats			(VMS)
   or
		% vimake stats			(Unix)


************************************************************************/


#define PROGRAM	stats
#define R2LIB

#define MODULE_LIST stats.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create stats.pdf
process help=*
PARM INP      TYPE=STRING   COUNT=(1:12)
PARM OUT      TYPE=STRING   COUNT=(1:2)
PARM SIZE     TYPE=INTEGER  COUNT=4	      DEFAULT=(1,1,0,0)
PARM MSS      TYPE=INTEGER		      DEFAULT=0
PARM BAND     TYPE=INTEGER  COUNT=(1:12)    DEFAULT=0
PARM SCRIBE   TYPE=INTEGER		      DEFAULT=0
PARM DN       TYPE=INTEGER  COUNT=(0:1)     DEFAULT=--
PARM HIST     TYPE=INTEGER  COUNT=(1:12)    DEFAULT=-1
PARM SPEC     TYPE=INTEGER  COUNT=(1:12)    DEFAULT=-1
PARM SPLOT    TYPE=INTEGER  COUNT=(1:12)    DEFAULT=-1
PARM EXCLUDE  TYPE=INTEGER  COUNT=(0:10)    DEFAULT=--
PARM NOPRINT  TYPE=KEYWORD  COUNT=(0:1) VALID="NOPRINT" DEFAULT=--
PARM VERT     TYPE=INTEGER  COUNT=(1:50)    DEFAULT=(0,0)
PARM CLASS1   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS2   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS3   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS4   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS5   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS6   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS7   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS8   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS9   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS10  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS11  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS12  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS13  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS14  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS15  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS16  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS17  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS18  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS19  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS20  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS21  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS22  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS23  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS24  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS25  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS26  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS27  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS28  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS29  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS30  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS31  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS32  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS33  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS34  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS35  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS36  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS37  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS38  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS39  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS40  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS41  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS42  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS43  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS44  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS45  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS46  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS47  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS48  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS49  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS50  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASSx   TYPE=INTEGER                  DEFAULT=0
END-PROC
.TITLE
   Program stats
.HELP
PURPOSE:  stats computes the statistics of specified training areas on
 multi-spectral data.  The output consists of page printer output, a
 statistics data set compatible with the multispectral classifier
 FASTCLAS, and (optionally) an image containing the scribed training
 areas.  Input multispectral data must be in separate VICAR data sets or
 in MSS format.  Refer to the help for program MSS.
EXECUTION:
The following is the execution statement format for stats:
	stats INP OUT PARAMS
where INP, OUT, PARAMS are parameters discussed in their respective
parameter sections.
 The training areas for the classes may be specified either in rectangular
or vertices format.  stats no longer allows some training fields in a given
training area to be in vertices format and some to be in rectangular
format.  Rectangular format is the default and is thus not 
specified explicitly.   The RECT parameter (which was present in the IBM
version of stats), for expliciltly specifying rectangular format, is no
longer used.  The VERT parameter is used to list any and every class using the
vertices format. 
  The training area for the class is composed of one or more training fields.
Each training field is a closed region of the image.  The format must be 
the same for all fields within a class.  In rectangular format the training 
fields are defined by starting line, starting sample, number of lines,
and number of samples.
  In vertices format, only the vertices (line number and sample number) of an
irregular area need be stated.  
.page
EXAMPLES

1.    stats INP=(A, B, C, D)   OUT=ST  HIST=0 SPEC=0 EXCLUDE=0 VERT=2 +
            CLASS1=( 50,60,20,20  85,100,10,15 )                      +
            CLASS2=( 125,50  140,60  150,40  130,30  125,50 )

In this example four spectral bands are input in separate VICAR images.
Histograms and spectral plots of all four bands and both classes are
produced.  Any pixel with a DN of 0 is excluded from the statistics.
The class 1 statistics come from two rectangular training fields.  The 
class 2 statistics come from an irregular training field.
.page

2.    stats INP=MS OUT=(ST,SC) MSS=5  SPEC=0 'NOPRINT SCRIBE=2 VERT=1     +
            CLASS1=( 25,20  35,30  40,50  55,30  30,15  25,20             +
                     85,40 100,55  70,55  85,40 )                         +
            CLASS2=( 200,100,20,30 )                                      +
            CLASS3=( 150,140,5,10 )

In this example five spectral bands are input in MSS format (in one file).
Statistics are computed on all five bands.  Spectral plots of all five bands 
and all three classes are produced, but all other printer (terminal) output is
suppressed.  The second spectral band (in this case, extracted from the MSS
format input) is copied to the second output file and scribed with the training
areas.  Class 1 statistics are gathered from two irregular training fields.
Classes 2 and three both have one rectangular training field.
.page
3.  The last example is the test procedure for stats.  This is
    a complete example that could be run by the user and that 
    demonstrates uses of the possible parameters.

     gen gen1 nl=128 ns=128
     gen gen2 nl=128 ns=128 sinc=2 linc=2
     gen gen3 nl=128 ns=128 sinc=4 linc=4
     !
     !  First tests will check standard image format files
     !
     ! Copy first band and scribe training areas on this image
     stats (gen1,gen2,gen3) (stat1,stat2) 'noprint +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Print Histograms for the DN values in each training area
     stats (gen1,gen2,gen3) stat1 hist=0 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Print Spectral Plot for all bands and classes
     stats (gen1,gen2,gen3) stat1 spec=0 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Try out Vertice format
     stats (gen1,gen2,gen3) stat1 hist=0 vert=5 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
     !
     !  Now check MSS format images
     !
     al mss 128 384
     mss (gen1,gen2,gen3) mss (1,1,128,128)
     ! Copy second band and scribe training areas on this image
     stats mss (stat1,stat2) mss=3 scribe=2 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Gather stats for first and third bands
     stats mss stat1 mss=3 band=(1,3) hist=0 'noprint +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Print spectral plots for second and third band for first and fourth class
     stats mss stat1 mss=3 splot=(1,4) spec=(2,3) +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Try out Vertice format
     stats mss (stat1,stat2) mss=3 vert=5 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
.page
OPERATION:  If the scribe option is requested, stats begins by copying
 the specified input picture to the secondary output data set.  Then the first
 training area parameters are the processed and the training area is
 read from all input bands.  Statistics on this area are compiled and the
 area is scribed on the scribed picture.  After compiling statistics for
 all training fields for a given class, the statistics are written on the
 output statistics data set.  The record number on which they were written is
 the class number of that class.  The record contains the
 means, the number of pixels, and the covariance matrix for that class.  The
 record length is a function of the number of spectral bands used.  The
 maximum record length is 372 bytes which corresponds to 12 spectral bands.
 Maximum number of records is 50 since class numbers must range from 1 to
 50.  If desired, histograms of each training class in each band are
 printed.   Spectral plots of the spectral signatures
 can also be printed.  The 'EXCLUDE' option allows the user to specify
 certain DNs for which statistics are ignored.  This is useful, for
 instance, if the training areas lie at the edge of a skewed picture.  To
 prevent statistics from including pixels in the background skew,
 'EXCLUDE,0' could be specified.
      Page printer output includes the training areas for the class,
 means, and standard deviations for each band, and the covariance matrix.
 The covariance matrix is simply the covariance taken between all combina-
 tions of spectral band pairs.  The keyword NOPRINT supresses the page
 printer option.
RESTRICTIONS:  stats can handle up to 12 spectral bands in MSS format or
 in separate data set format, and compute statistics on 50 training
 classes.  The maximum size for any training field is 600 lines by 600
 samples.  The input files must have byte data format.
      Histograms and scribing must be performed in separate executions.
 If both are specified then the histograms will be deleted.
.page
WRITTEN BY: J. D. Addington               30 September 1974

CONVERTED TO VAX BY:  A. J. Runkle	  22 Dec. 1983

CURRENT COGNIZANT PROGRAMMER: Steve Pohorsky

MADE PORTABLE FOR UNIX: CRI               31 OCT 1994


TIMING:  Execution time depends on the number and size of the training
 areas and the number of spectral bands.  Typical running time is between
 1 and 2 minutes.
.LEVEL1
.VARIABLE INP
STRING - Contains the input data
set name(s)
.VARIABLE OUT
STRING - Contains output data   
set name(s)
.VARIABLE SIZE
Standard Vicar size field 
(Not used - can be defaulted).
.VARIABLE MSS
Specifies the number of spectral
bands in MSS format.
.VARIABLE BAND
Denotes that bands in MSS format
to be used as input.
.VARIABLE SCRIBE
Denotes which input band is to
be copied to output  data set.
.VARIABLE DN
Specifies the outline DN value.
.VARIABLE HIST
Denotes the bands for which
histograms will be printed.
.VARIABLE SPEC
Denotes which spectral plots are
to be printed.
.VARIABLE SPLOT
Specifies the limits  of 
spectral plots for classes.
.VARIABLE EXCLUDE
Ignore pixels with given DN
values.
.VARIABLE NOPRINT
Specifies whether training area
information is to be printed.
.VARIABLE VERT
Specifies the training
areas that are in vertex format.
.VARIABLE CLASS
Dummy argument do NOT use.
.VARIABLE CLASSx
Dummy arguement do NOT use.
.VARIABLE CLASS1
Training area coordinates.
.VARIABLE CLASS2
Training area coordinates.
.VARIABLE CLASS3
Training area coordinates.
.VARIABLE CLASS4
Training area coordinates.
.VARIABLE CLASS5
Training area coordinates.
.VARIABLE CLASS6
Training area coordinates.
.VARIABLE CLASS7
Training area coordinates.
.VARIABLE CLASS8
Training area coordinates.
.VARIABLE CLASS9
Training area coordinates.
.VARIABLE CLASS10
Training area coordinates.
.VARIABLE CLASS11
Training area coordinates.
.VARIABLE CLASS12
Training area coordinates.
.VARIABLE CLASS13
Training area coordinates.
.VARIABLE CLASS14
Training area coordinates.
.VARIABLE CLASS15
Training area coordinates.
.VARIABLE CLASS16
Training area coordinates.
.VARIABLE CLASS17
Training area coordinates.
.VARIABLE CLASS18
Training area coordinates.
.VARIABLE CLASS19
Training area coordinates.
.VARIABLE CLASS20
Training area coordinates.
.VARIABLE CLASS21
Training area coordinates.
.VARIABLE CLASS23
Training area coordinates.
.VARIABLE CLASS24
Training area coordinates.
.VARIABLE CLASS25
Training area coordinates.
.VARIABLE CLASS26
Training area coordinates.
.VARIABLE CLASS27
Training area coordinates.
.VARIABLE CLASS28
Training area coordinates.
.VARIABLE CLASS29
Training area coordinates.
.VARIABLE CLASS30
Training area coordinates.
.VARIABLE CLASS31
Training area coordinates.
.VARIABLE CLASS32
Training area coordinates.
.VARIABLE CLASS33
Training area coordinates.
.VARIABLE CLASS34
Training area coordinates.
.VARIABLE CLASS35
Training area coordinates.
.VARIABLE CLASS36
Training area coordinates.
.VARIABLE CLASS37
Training area coordinates.
.VARIABLE CLASS38
Training area coordinates.
.VARIABLE CLASS39
Training area coordinates.
.VARIABLE CLASS40
Training area coordinates.
.VARIABLE CLASS41
Training area coordinates.
.VARIABLE CLASS42
Training area coordinates.
.VARIABLE CLASS43
Training area coordinates.
.VARIABLE CLASS44
Training area coordinates.
.VARIABLE CLASS45
Training area coordinates.
.VARIABLE CLASS46
Training area coordinates.
.VARIABLE CLASS47
Training area coordinates.
.VARIABLE CLASS48
Training area coordinates.
.VARIABLE CLASS49
Training area coordinates.
.VARIABLE CLASS50
Training area coordinates.
.LEVEL2
.VARIABLE INP
STRING - Input data sets used by stats.  If input data is in MSS format
then only one data set is required.  Otherwise there must be one data set
per spectral band examined.
.VARIABLE OUT
STRING - Output data sets used by STATS.  First data set will contain the
statistics output.  The second output data set (optional) will contain
the input picture with training areas scribed.  (If scribing is requested
and their is only one output file, the output file is assumed to be the
scribed image, and no statistics output file is generated.)

The statistics output file is a classification statistics file (statistics
data set).  Each record (excluding labels) in a classification statistics file
contains statistics for a particular class of pixels.  The statistics are
derived from the multispectral (multi-channel) data.  The size of records in
the file is based on the number of channels (bands).  If 'N' is used to denote
the number of channels, then each record contains the following data in the
order shown. 

   *  2 words (8 bytes) of class name in character (ASCII) format.  This
      takes the form 'CLASSnnn' where nnn is the class number.
      (nnn is left-justified with trailing blanks.)

   *  N words for mean values for this class for each of the N channels.
      Each mean value is stored in REAL*4 format.

   *  1 word, stored in INTEGER*4 format, containing the number of pixels 
      in this class.

   *  N*(N+1)/2 words containing the variances and covariances for the N
      channels for this class.  Each variance and covariance is stored in
      REAL*4 format.

The VICAR system label contains the number of records in the number of lines
field and the number of bytes per record in the number of samples field.
.VARIABLE SIZE
INTEGER - Standard Vicar size field (Not used - can be defaulted).
.VARIABLE MSS
INTEGER - Denotes that input data is in MSS format and contains the
supplied number of spectral bands.
.VARIABLE BAND
INTEGER - Specifies which MSS format bands are to be used as input (The
default is for all bands specified by the MSS parameter).
Syntax is:   BAND = (b1,...)   where b1,... is a list of bands.
.VARIABLE DN
INTEGER - Denotes that the outline will be the given DN in scribing the
training areas.  The default is to use 0 or 255 depending on which contrasts
better with the image.
.VARIABLE SCRIBE - Denotes which input band is going to be copied with
the training classes scribed on the image.
.VARIABLE HIST
INTEGER - Denotes that histograms of the training classes from the
specified bands are to be printed on the line printer.  If the value of
0 is used, histograms for all bands are printed.
Syntax is:   HIST = (b1,...)   where b1,... is a list of bands.

The histogram output uses vertical bars to show the number of pixels in the
DN bins.  The bars are made of a column of  * symbols with a number (or other
symbol) on top.  The number of pixels represented by each * is shown at the
top of the histogram.  The number on top of a column is an additional number of
pixels in the bin beyond those represented by *s.  The letters A through Z are
used to represent the values 10 through 35, respectively.  The $ symbol
represents a value greater than 35. 
.VARIABLE SPEC
INTEGER - Denotes that a spectral plot of training classes from the given
bands is to be printed.  If the value of 0 is used, all the bands are 
included.
Syntax is:   SPEC = (b1,...)   where b1,... is a list of bands.
.VARIABLE SPLOT
INTEGER - Limits spectral plots to the specified classes (default is that
all classes are included).
Syntax is:   SPLOT = (c1,...)   where c1,... is a list of classes.
.VARIABLE EXCLUDE
INTEGER - Ignore any pixel which has the same DN value as one specified.
There is a maximum of 10 DN values that can be specified.
Syntax is:   EXCLUDE = (d1,...)   where d1,... is a list of DNs.
.VARIABLE NOPRINT
KEYWORD - Controls the printing of means, standard deviations, covariance
matrices, and training area coordinates on the line printer (default is
to PRINT this information).
.VARIABLE VERT
INTEGER - Specifies which  training areas defined by the CLASSx keyword
are to be read in the vertices format.
 The training areas for the classes may be specified either in rectangular
or vertices format.  stats no longer allows some training fields in a given
training area to be in vertices format and some to be in rectangular
format.  Rectangular format is the default and is thus not 
specified explicitly.   The RECT parameter (which was present in the IBM
version of stats), for expliciltly specifying rectangular format, is no
longer used.  The VERT parameter is used to list any and every class using the
vertices format. 
.VARIABLE CLASS
Actual keyword is of the form CLASSx where x is a number from 1 to 50.
The number defines which class number this training set belongs.
e.g.  CLASS69 SL,SS,NL,NS    or    CLASS32  L1,S1,L2,S2,...
See CLASSx for a more complete definition.
.VARIABLE CLASSx
INTEGER - Denotes the training area information in either rectangular or
vertices formats (default is rectangular).  Rectangular coordinates are
of the form: SL,SS,NL,NS   SL,SS,NL,NS ...  (the standard Vicar size field) 
while vertices format is of the form: L1,S1,L2,S2,... etc.  The range of values
for 'x' is from 1 to 50 such that the valid keywords would be of the form:
CLASS1, ... CLASS50. 
  The training area for the class is composed of one or more training fields.
Each training field is a closed region of the image.  The format must be 
the same for all fields within a class.  In rectangular format the training 
fields are defined by starting line, starting sample, number of lines,
and number of samples.
  In vertices format, only the vertices (line number and sample number) of an
irregular area need be stated.  The program interpolates between each vertex to
determine the perimeter of the training field.  Several rules must be adhered
to in using the vertices format:

1)  The first vertex entered must be the topmost point of the area; that is, 
    the lowest line number.
2)  The vertices must follw in a clockwise order. 
3)  The last vertex must be the same as the first; that is, close the area
    for each training field.

  The maximum size for any training field, whether rectangular or vertices,
is 600 lines by 600 samples.
.VARIABLE CLASS1
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS2
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS3
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS4
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS5
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS6
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS7
CINTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS8
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS9
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS10
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS11
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS12
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS13
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS14
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS15
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS16
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS17
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS18
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS19
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS20
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS21
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS22
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS23
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS24
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS25
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS26
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS27
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS28
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS29
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS30
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS31
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS32
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS33
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS34
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS35
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS36
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS37
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS38
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS39
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS40
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS41
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS42
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS43
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS44
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS45
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS46
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS47
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS48
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS49
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS50
INTEGER - See CLASSx for a proper definition.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststats.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
!
!
refgbl $syschar
!
! First VMS
if ($syschar(1) = "VAX_VMS")
  dcl ass stats.st1 stat1
  dcl ass stats.st2 stat2
  dcl ass stats.gn1 gen1
  dcl ass stats.gn2 gen2
  dcl ass stats.gn3 gen3
  dcl ass stats.mss mss
else
! Now UNIX
  ush alias stat1 stats.st1
  ush alias stat2 stats.st2
  ush alias gen1 stats.gn1
  ush alias gen2 stats.gn2
  ush alias gen3 stats.gn3
  ush alias mss stats.mss
end-if
!
!
let $echo="yes"
!
!  Test script for Vicar routine STATS
!
!DCL ASS STATS.ST1 STAT1
!DCL ASS STATS.ST2 STAT2
!DCL ASS STATS.GN1 GEN1
!DCL ASS STATS.GN2 GEN2
!DCL ASS STATS.GN3 GEN3
!DCL ASS STATS.MSS MSS
gen gen1 nl=128 ns=128
gen gen2 nl=128 ns=128 sinc=2 linc=2
gen gen3 nl=128 ns=128 sinc=4 linc=4
!
!  First tests will check standard image format files
!
! Copy first band and scribe training areas on this image
stats (gen1,gen2,gen3) (stat1,stat2) 'noprint +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Print Histograms for the DN values in each training area
stats (gen1,gen2,gen3) stat1 hist=0 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Print Spectral Plot for all bands and classes
stats (gen1,gen2,gen3) stat1 spec=0 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Try out Vertice format
stats (gen1,gen2,gen3) stat1 hist=0 vert=5 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
!
!  Now check MSS format images
!
!al mss 128 384
mss (gen1,gen2,gen3) mss (1,1,128,128)
! Copy second band and scribe training areas on this image
stats mss (stat1,stat2) mss=3 scribe=2 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Gather stats for first and third bands
stats mss stat1 mss=3 band=(1,3) hist=0 'noprint +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Print spectral plots for second and third band for first and forth class
stats mss stat1 mss=3 splot=(1,4) spec=(2,3) +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Try out Vertice format
stats mss (stat1,stat2) mss=3 vert=5 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
!
if ($syschar(1) = "VAX_VMS")
  dcl del stats.st1;,stats.st2;,stats.gn1;,stats.gn2;
  dcl del stats.gn3;,stats.mss;
  dcl deass stat1
  dcl deass stat2
  dcl deass gen1
  dcl deass gen2
  dcl deass gen3
  dcl deass mss
end-if
!
!DCL DEL STATS.ST1;,STATS.ST2;,STATS.GN1;,STATS.GN2;
!DCL DEL STATS.GN3;,STATS.MSS;
!DCL DEASS STAT1
!DCL DEASS STAT2
!DCL DEASS GEN1
!DCL DEASS GEN2
!DCL DEASS GEN3
!DCL DEASS MSS
end-proc
$ Return
$!#############################################################################
