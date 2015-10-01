$!****************************************************************************
$!
$! Build proc for MIPL module maskv
$! VPACK Version 1.8, Thursday, April 05, 2001, 16:39:12
$!
$! Execute by entering:		$ @maskv
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
$ write sys$output "*** module maskv ***"
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
$ write sys$output "Invalid argument given to maskv.com file -- ", primary
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
$   if F$SEARCH("maskv.imake") .nes. ""
$   then
$      vimake maskv
$      purge maskv.bld
$   else
$      if F$SEARCH("maskv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake maskv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @maskv.bld "STD"
$   else
$      @maskv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create maskv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack maskv.com -
	-s maskv.f -
	-p maskv.pdf -
	-i maskv.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create maskv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C	10 MAR 89    ...REA..     REWRITE FROM MASKV
C	14 MAY 91    ...REA..     CONVERT FOR SUN/UNIX
C
	COMMON /IOCOMMON/ IOUTUNIT,ILINE
	COMMON /LABS/ NLABS,LAB
	INTEGER ISTR(2)
	LOGICAL XVPTST
	CHARACTER*80 BUF
	CHARACTER*70 LAB(100)
	LOGICAL*1 LUT(256)
	LOGICAL*1 QSTRETCH,QSYS,QTASK,QUSER,QHIST,QID,QLOGO,QTIMS
C
	CALL XVMESSAGE('MASKV Version April 5, 2001',' ')
C							     open input data set
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C								size parameters
	CALL XVGET(INUNIT,ISTAT,'PIX_SIZE',NBPP,' ')
	IF (NBPP.GT.2) THEN
	    CALL XVMESSAGE(' UNSUPPORTED DATA FORMAT',' ')
	    CALL ABEND
	ENDIF
	CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
	IF(NSIN.LT.ISS+NS-1) NS=NSIN-ISS+1
	IF(NLIN.LT.ISL+NL-1) NL=NLIN-ISL+1
C									expand
	CALL XVPARM('EXPAND',LFACT,ICOUNT,IDEF,0)
	IF (LFACT.GT.10) LFACT=10
C								stretch params
	CALL XVPARM('STRETCH',ISTR,ICOUNT,IDEF,0)
	IF (IDEF .EQ. 0) THEN
	    QSTRETCH = .TRUE.
	    INMIN = ISTR(1)
	    INMAX = ISTR(2)
	ELSE
	    INMIN = 0
	    IF (NBPP .EQ. 2) THEN
		QSTRETCH = .TRUE.
		INMAX = 32767
	    ELSE
		QSTRETCH = .FALSE.
		INMAX = 255 
	    END IF
	END IF
	IF (XVPTST('COMP')) THEN
	    QSTRETCH = .TRUE.
	    N = INMIN
	    INMIN = INMAX
	    INMAX = N
	END IF
C					generate lookup table for byte input
	IF(QSTRETCH) THEN
	    WRITE (BUF,100) INMIN,INMAX
  100	    FORMAT(' STRETCH',I6,' -',I6)
	    CALL XVMESSAGE(BUF,' ')
	    IF (NBPP.EQ.1) THEN
		ISPAN = INMAX-INMIN
		IF (ISPAN.EQ.0) ISPAN=1
		A = 255./ISPAN
		B = -A*INMIN + 0.5
		DO J=1,256
		    IDN = A*(J-1)+B
		    IDN = MIN(255,MAX(0,IDN))
		    CALL ITLA(IDN,LUT(J),1)
		END DO
	    END IF
	END IF
C									labels
	QSYS  = .NOT. XVPTST('NOSYSTEM')
	QTASK = .NOT. XVPTST('NOTASK')
	QUSER = .NOT. XVPTST('NOUSER')
	QHIST = .NOT. XVPTST('NOHISTOR')
	QTIMS = XVPTST('TIMS')
	IF (QTIMS) LFACT = MIN(10,(NS/1000)+1)
	CALL BUILD_LABELS(QSYS,QTASK,QUSER,QHIST,QSTRETCH,QTIMS,
     +			  INUNIT,INMIN,INMAX,NL,NS,NBPP)
C								  id and logos
	QID   = .NOT. XVPTST('NOID')
	QLOGO =       XVPTST('LOGO') 
	CALL XVPARM('LLOGO',LOGO1,ICOUNT,IDEF,0)
	CALL XVPARM('RLOGO',LOGO2,ICOUNT,IDEF,0)
	IF (QLOGO .AND. LOGO1+LOGO2.EQ.0) THEN
	    LOGO1 = 1
	    LOGO2 = 1
	END IF
	QLOGO = (LOGO1+LOGO2 .GT. 0)
C						compute number of output lines
	NLO = NL + (18+9*NLABS)*LFACT + 33
	IF (QLOGO) NLO = NLO + 66*LFACT
	IF (QID .AND. .NOT.QLOGO) NLO = NLO + 26*LFACT
C						compute number of output samples
	NSO = NS + 12*LFACT + 85
	NSO = 4*((NSO+3)/4)		  ! force it to be a multiple of 4 (MDA)
	NSOMIN = 500*LFACT
	IF (NSO .LT. NSOMIN) THEN	  ! force it to be big enough for labels
	    MARGIN = (NSOMIN-NSO)/2
	    NSO = NSOMIN
	ELSE
	    MARGIN = 0
	END IF
C						      print output picture size
	WRITE (BUF,200) NLO,NSO
  200	FORMAT(' *****  OUTPUT:',I6,' LINES -',I5,' SAMPLES  *****')
	CALL XVMESSAGE(BUF,' ')
C							   open output data set
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OP','WRITE','OPEN_ACT','SA',
     +		   'IO_ACT','SA','U_NL',NLO,'U_NS',NSO,
     +		   'O_FORMAT','BYTE','U_FORMAT','BYTE',' ') 
C							     process the image
	CALL WORK(INUNIT,ISL,ISS,NL,NS,NSO,NBPP,LFACT,MARGIN,
     +		  INMIN,INMAX,LOGO1,LOGO2,LUT,QSTRETCH,QLOGO,QID)
C
	RETURN
	END
C****************************************************************************
	SUBROUTINE BUILD_LABELS(QSYS,QTASK,QUSER,QHIST,QSTRETCH,QTIMS,
     +				INUNIT,INMIN,INMAX,NL,NS,NBPP)
C
	COMMON /LABS/ NLABS,LAB
	CHARACTER*70 LAB(100)
	CHARACTER*61 STRING
	CHARACTER*28 DATTIM
	CHARACTER*8 TASK(100),USER,KEY,FORMAT
	CHARACTER*4 PIXSIZ(2)/'BYTE','HALF'/
	INTEGER INST(100)
	LOGICAL*1 QSYS,QTASK,QUSER,QHIST,QSTRETCH,QTIMS
C								system label
	NLABS = 0
	IF (QSYS) THEN
	    NLABS = NLABS+1
	    IF (QSTRETCH) THEN
		WRITE(LAB(NLABS),100) PIXSIZ(NBPP),NS,NL,INMIN,INMAX,'*'
  100		FORMAT('FORMAT=',A4,'    NS=',I5,' NL=',I5,
     +		       '   STRETCH',I6,' -',I6,12X,A1)
	    ELSE
		WRITE(LAB(NLABS),110) PIXSIZ(NBPP),NS,NL,'*'
  110		FORMAT('FORMAT=',A4,'    NS=',I5,' NL=',I5,36X,A1)
	    END IF
	END IF
C
	IF (QUSER) QTASK=.TRUE.
	IF (QTASK .OR. QHIST) THEN
	  NHIST = 100
	  CALL XLHINFO(INUNIT,TASK,INST,NHIST,ISTAT,' ')
C
	  DO I=1,NHIST
	    IF (QTASK) THEN				! task and user labels
	      NLABS = NLABS+1
	      IF (QUSER) THEN
		CALL XLGET(INUNIT,'HISTORY','DAT_TIM',DATTIM,ISTAT,
     +			   'FORMAT','STRING','HIST',TASK(I),
     +			   'INSTANCE',INST(I),' ')
		CALL XLGET(INUNIT,'HISTORY','USER',USER,ISTAT,'HIST',
     +		       TASK(I),'INSTANCE',INST(I),'FORMAT','STRING',' ')
		WRITE(LAB(NLABS),200) TASK(I),DATTIM,USER
  200		FORMAT(A8,'     ',A28,'          ',A8)
	      ELSE
		WRITE(LAB(NLABS),210) TASK(I)
  210		FORMAT(A8)
	      END IF
	    END IF
C								history labels
	    IF (QHIST) THEN
	      CALL XLINFO(INUNIT,'HISTORY','TASK',FORMAT,MAXL,NUM,
     +			    ISTAT,'HIST',TASK(I),'INSTANCE',INST(I),' ')
	      DO WHILE (KEY .NE. 'TASK')
		CALL XLNINFO(INUNIT,KEY,FORMAT,MAXL,NUM,ISTAT,' ')
		IF (ISTAT .EQ. -57) KEY='TASK'
		IF (KEY.NE.'DAT_TIM' .AND. KEY.NE.'USER' .AND.
     +			      KEY.NE.'NLABS' .AND. KEY.NE.'TASK') THEN
		  IF (.NOT.QTIMS .OR. KEY(1:4).EQ.'INFO') THEN
		   NLABS = NLABS+1
		   IF (FORMAT .EQ. 'STRING') THEN
		    CALL XLGET(INUNIT,'HISTORY',KEY,STRING,ISTAT,'HIST',
     +		       TASK(I),'INSTANCE',INST(I),'FORMAT','STRING',' ')
		    WRITE(LAB(NLABS),300) KEY,STRING
  300		    FORMAT(A8,'=',A61)
		   ELSE IF (FORMAT .EQ. 'INT') THEN
		    CALL XLGET(INUNIT,'HISTORY',KEY,INT,ISTAT,'HIST',
     +			  TASK(I),'INSTANCE',INST(I),'FORMAT','INT',' ')
		    WRITE(LAB(NLABS),310) KEY,INT
  310		    FORMAT(A8,'=',I10)
		   ELSE IF (FORMAT .EQ. 'REAL') THEN
		    CALL XLGET(INUNIT,'HISTORY',KEY,REAL,ISTAT,'HIST',
     +			 TASK(I),'INSTANCE',INST(I),'FORMAT','REAL',' ')
		    WRITE(LAB(NLABS),320) KEY,REAL
  320		    FORMAT(A8,'=',G14.7)
		   ELSE
		    NLABS = NLABS-1
		   END IF
		  END IF
		END IF
	      END DO
	      KEY = 'XXXX'
	    END IF
	  END DO
	END IF
	RETURN
	END      
C****************************************************************************
      SUBROUTINE TICGEN(TICBUF,SIZE)
C
C        THIS ROUTINE GENERATES 1 CYCLE OF TIC MARKS
C
      BYTE TICBUF(100,10)
      BYTE BLK/-1/
C
      CALL ZIA(TICBUF,250)
C						     put box on every 100th tic
      DO I=2,4
	  TICBUF( 1,I) = BLK
	  TICBUF(99,I) = BLK
      END DO
C						        tic mark at 50 and 100
      DO I=2,10
	  TICBUF( 50,I) = BLK
	  TICBUF(100,I) = BLK
      END DO
C						        tic mark every 10
      DO INT=10,40,10
	  DO I=4,8
	      TICBUF(INT,I) = BLK
	      TICBUF(INT+50,I) = BLK
	  END DO
      END DO
C
      IF(SIZE.GT.2000) RETURN
C						        tic mark every 2
      DO INT=2,92,10
	  DO I=7,8
	      TICBUF(INT,I) = BLK
	      TICBUF(INT+6,I) = BLK
	  END DO
	  DO I=6,8
	      TICBUF(INT+2,I) = BLK
	      TICBUF(INT+4,I) = BLK
	  END DO
      END DO
      RETURN
      END
C***************************************************************************
	SUBROUTINE WORK(INUNIT,ISL,ISS,NL,NS,NSO,NBPP,LFACT,MARGIN,
     +			INMIN,INMAX,LOGO1,LOGO2,LUT,QSTRETCH,QLOGO,QID)
C
	COMMON /IOCOMMON/ IOUTUNIT,ILINE
	INTEGER*2 INBUF(32767)
	LOGICAL*1 LUT(256),QSTRETCH,QLOGO,QID
	LOGICAL*1 TICBUF(100,10),OUTBUF(32767)
C
	NTICS = MAX(NL,NS)
	CALL TICGEN(TICBUF,NTICS)
C
	ILINE = 1
	ISLPIC = 17 + 7*LFACT
	ISSPIC = 17 + 6*LFACT + MARGIN
	IELPIC = ISLPIC +NL - 1
	IESPIC = ISSPIC +NS - 1
C		      ISSPIC = sample loc in output of first sample of the input
C		      IESPIC = sample loc in output of last sample of the input
C
C						determine location of gray wedge
	NLGLEV = MAX((NL-2)/16,1)
	ISLWEDG = ISLPIC
	IELWEDG = ISLWEDG + 16*NLGLEV + 1
	ISSWEDG = 17 + 6*LFACT + IESPIC
	NSWEDG = 28
C				write upper border, sample labels, and tic marks
	CALL ITLA(0,OUTBUF,NSO)
	CALL WLINE(NSO,OUTBUF,5)
	CALL SAMLAB(OUTBUF,NSO,ISLPIC,ISSPIC,IELPIC,IESPIC,LFACT,NS,
     +		    ILINE,TICBUF)
	CALL SAMTIC(1,OUTBUF,ISLPIC,ISSPIC,IELPIC,IESPIC,NS,NSO,
     +		    LFACT,ILINE,TICBUF)
C							      process the image
	IEL = ISL+NL-1
	IESBORD = ISSPIC-1
	IESTIC = ISSPIC-2
	IF (NBPP.EQ.1) THEN
C							      *** BYTE INPUT ***
	    DO LINE=ISL,IEL
		CALL XVREAD(INUNIT,OUTBUF(ISSPIC),ISTAT,'LINE',LINE,  ! image
     +			    'SAMP',ISS,'NSAMPS',NS,' ')
		IF (QSTRETCH) CALL TBL(OUTBUF(ISSPIC),LUT,NS)	      ! stretch
		CALL ITLA(0,OUTBUF,IESTIC)
		CALL LINTIC(OUTBUF,ISLPIC,ISSPIC,IELPIC,IESPIC,ILINE, !tic marks
     +			    LFACT,TICBUF)
		CALL GWEDGE(OUTBUF,ISSWEDG,NSWEDG,ISLWEDG,IELWEDG,   !gray wedge
     +			    NLGLEV)
		CALL WLINE(NSO,OUTBUF,1)			     ! write out
	    END DO
	ELSE
C						          *** HALFWORD INPUT ***
	    GAIN = 255.0/(INMAX-INMIN)
	    OFFSET = -GAIN*INMIN + 0.5
C
	    DO LINE=ISL,IEL
		CALL XVREAD(INUNIT,INBUF,ISTAT,'LINE',LINE,'SAMP',ISS,   ! image
     +			    'NSAMPS',NS,' ')
		DO ISAMP=1,NS					       ! stretch
		    IDN = GAIN*INBUF(ISAMP) + OFFSET
		    IDN = MIN( MAX(0,IDN) ,255)
		    CALL ITLA(IDN,OUTBUF(IESBORD+ISAMP),1)
		END DO
		CALL ITLA(0,OUTBUF,IESTIC)
		CALL LINTIC(OUTBUF,ISLPIC,ISSPIC,IELPIC,IESPIC,      ! tic marks
     +			    ILINE,LFACT,TICBUF)
		CALL GWEDGE(OUTBUF,ISSWEDG,NSWEDG,ISLWEDG,IELWEDG,   !gray wedge
     +			    NLGLEV)
		CALL WLINE(NSO,OUTBUF,1)			     ! write out
	    END DO
	END IF
C					 	write lower tic marks, sample
C						labels, and annotation
C
	CALL SAMTIC(0,OUTBUF,ISLPIC,ISSPIC,IELPIC,IESPIC,NS,NSO,LFACT,
     +		    ILINE,TICBUF)
	CALL SAMLAB(OUTBUF,NSO,ISLPIC,ISSPIC,IELPIC,IESPIC,LFACT,NS,
     +		    ILINE,TICBUF)
	CALL WRITE_LABELS(NSO,LFACT,LOGO1,LOGO2,QLOGO,QID,OUTBUF)
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE LINTIC(BUF,SLPIC,SSPIC,ELPIC,ESPIC,ILINE,LFACT,
     +			  TICBUF)
C
C        PUT 'LINE' LABEL, NUMBERS, AND TICS ON SIDES
C
	IMPLICIT INTEGER(A-Z)
	CHARACTER*5 CNUM
	BYTE INUM(5)
	CHARACTER*4 LABEL/'LINE'/
	BYTE TICBUF(100,10),BUF(*),BLK/-1/
	LOGICAL*1 QFIRST/.TRUE./
	EQUIVALENCE (INUM,CNUM)
	SAVE QFIRST
	
C
	IF(QFIRST) THEN						! initialize
	    QFIRST = .FALSE.
	    ITIC = 99
	    NUM = 0
	    CEN = 5 + 7*LFACT + 10 + 1
	    SLNUM = CEN-4*LFACT+1
	    ELNUM = CEN+4*LFACT
	    SLLAB = ELNUM + 2*LFACT
	    ELLAB = SLLAB+4*8*LFACT-1
	    SSLAB = SSPIC-11-6*LFACT
	END IF
C
	IF (ILINE.GE.SLLAB .AND. ILINE.LE.ELLAB) THEN		! labelling
	    IF(ILINE.LE.SLLAB) THEN
		REPEAT = LFACT
		LSLICE = 0
		IICHAR = 1
	    END IF
c	    CALL TEXT(LABEL(IICHAR),1,LSLICE,BUF(SSLAB),6*LFACT,255)
	    CALL TEXT(LABEL,1,LSLICE,BUF(SSLAB),6*LFACT,255)
	    CALL MVL(BUF(SSLAB),BUF(ESPIC+12),6*LFACT)
	    REPEAT = REPEAT-1
	    IF (REPEAT.EQ.0) LSLICE=LSLICE+1
	    IF (REPEAT.EQ.0) REPEAT=LFACT
	    IF (LSLICE.EQ.8) IICHAR=IICHAR+1
	    IF (LSLICE.EQ.8) LSLICE=0
	ELSE							! numbering
	    IF (ILINE.GE.SLNUM .AND. ILINE.LE.ELNUM .AND. 
     +		ELNUM.LE.ELPIC+12+7*LFACT) THEN
		IF (ILINE.LE.SLNUM) THEN
		    WRITE (CNUM,100) NUM
  100		    FORMAT(I5)
		    REPEAT = LFACT
		    NSLICE = 0
		    IF (NUM .LT. 100) THEN
			IDIGIT = 5
		    ELSE IF (NUM .LT. 1000) THEN
			IDIGIT = 3
		    ELSE IF (NUM .LT. 10000) THEN
			IDIGIT = 2
		    ELSE
			IDIGIT = 1
		    END IF
		END IF
		CALL TEXT(INUM(IDIGIT),1,NSLICE,BUF(SSLAB),6*LFACT,255)
		CALL MVL(BUF(SSLAB),BUF(ESPIC+12),6*LFACT)
		REPEAT = REPEAT-1
		IF(REPEAT.EQ.0) NSLICE=NSLICE+1
		IF(REPEAT.EQ.0) REPEAT=LFACT
		IF(NSLICE.EQ.8) IDIGIT=IDIGIT+1
		IF(NSLICE.EQ.8) NSLICE=0
		IF(IDIGIT.GT.5) THEN
		    IF (LFACT.LE.2) THEN
			NUM = NUM+100
			CEN = CEN+100
		    ELSE IF (LFACT.LE.4) THEN
			NUM = NUM+200
			CEN = CEN+200
		    ELSE
			NUM = NUM+500
			CEN = CEN+500
		    END IF
		    IF (CEN.LE.ELPIC) THEN
			NDIGIT = 3
			IF(NUM.GE.1000) NDIGIT=4
			IF(NUM.GE.10000) NDIGIT=5
			SLNUM = CEN-4*NDIGIT*LFACT+1
			ELNUM = CEN+4*NDIGIT*LFACT
		    END IF
		END IF
	    END IF
	END IF
C
	IF (ILINE.LT.SLPIC-2) RETURN
	IF (ILINE.EQ.ELPIC+1) CALL ITLA(0,BUF(ESPIC+1),12)
	IF (ILINE.GT.ELPIC) RETURN
C					tic marks for left and right columns
	DO L=1,10
	    CALL MVL(TICBUF(ITIC,L),BUF(SSPIC-12+L),1)
	    CALL MVL(TICBUF(ITIC,L),BUF(ESPIC+12-L),1)
	END DO
C				white border lines on left and right of picture
	CALL MVL(BLK,BUF(SSPIC-1),1)
	CALL MVL(BLK,BUF(ESPIC+1),1)
	ITIC = ITIC+1
	IF (ITIC.GT.100) ITIC=1
	RETURN
	END
C***********************************************************************
	SUBROUTINE SAMTIC(TOP,OUTBUF,SLPIC,SSPIC,ELPIC,ESPIC,
     +			  NSO,NSW,LFACT,ILINE,TICBUF)
C
C						put tic marks on sample scale
C
      IMPLICIT INTEGER(A-Z)
      BYTE OUTBUF(*),TICBUF(100,10)
C
C
      IF(TOP.NE.1) THEN             !draw white border along bottom of picture
	  CALL LINTIC(OUTBUF,SLPIC,SSPIC,ELPIC,ESPIC,ILINE,LFACT,TICBUF)
	  CALL ITLA(255,OUTBUF(SSPIC-1),NSO+2)
	  CALL WLINE(NSW,OUTBUF,1)
      END IF
C
      DO K=1,10
	  L = K
	  IF(TOP.EQ.0) L=11-K
	  CALL ITLA(0,OUTBUF,NSW)
	  CALL LINTIC(OUTBUF,SLPIC,SSPIC,ELPIC,ESPIC,ILINE,LFACT,TICBUF)
	  CALL MVL(TICBUF(99,L),OUTBUF(SSPIC-2),2)
	  NPIX = 100
	  JS = SSPIC
  40	  CONTINUE
	  IF (JS+NPIX-1.GT.ESPIC) NPIX=ESPIC-JS+1
	  CALL MVL(TICBUF(1,L),OUTBUF(JS),NPIX)
	  JS = JS+100
	  IF(NPIX.GE.100) GO TO 40
	  CALL WLINE(NSW,OUTBUF,1)
      END DO
      CALL ITLA(0,OUTBUF,NSW)
C
      IF(TOP.NE.0) THEN    	! draw white border line across top of picture
	  CALL LINTIC(OUTBUF,SLPIC,SSPIC,ELPIC,ESPIC,ILINE,LFACT,TICBUF)
	  CALL ITLA(255,OUTBUF(SSPIC-1),NSO+2)
	  CALL WLINE(NSW,OUTBUF,1)
	  CALL ITLA(0,OUTBUF,NSW)
      END IF
C
      RETURN
      END
C*****************************************************************************
      SUBROUTINE SAMLAB(BUF,NSW,ISLPIC,ISSPIC,IELPIC,IESPIC,LFACT,NSO,
     +			ILINE,TICBUF)
C
C        WRITE 'SAMPLE' LABEL AND NUMBERS ALONG TOP AND BOTTOM
C
      CHARACTER*5 CNUM
      BYTE TICBUF(*),BUF(*),LNUM(5)
      CHARACTER*6 LABEL/'SAMPLE'/
      EQUIVALENCE (LNUM,CNUM)
C
	DO K=1,7
	    CALL ITLA(0,BUF,NSW)
	    NUM = 0
	    ICEN = ISSPIC-1
	    DO WHILE (ICEN.LE.IESPIC)
		NDIGIT = 1
		IF (NUM.GE.  100) NDIGIT=3
		IF (NUM.GE. 1000) NDIGIT=4
		IF (NUM.GE.10000) NDIGIT=5
		LOC = ICEN-3*LFACT*NDIGIT
		WRITE (CNUM,100) NUM
  100		FORMAT(I5)
		CALL TEXT(LNUM(6-NDIGIT),NDIGIT,K-1,BUF(LOC),6*LFACT,
     +			  255)
		IF (LFACT.LE.2) THEN
		    ICEN = ICEN+100
		    NUM = NUM+100
		ELSE IF (LFACT.LE.4) THEN
		    ICEN = ICEN+200
		    NUM = NUM+200
		ELSE
		    ICEN = ICEN+500
		    NUM = NUM+500
		END IF
	    END DO
C									'SAMPLE'
	    IF (NSO.GE.100) THEN
		NCHAR = 6
		IF (LFACT.GT.1) NCHAR=4
		CALL TEXT(LABEL,NCHAR,K-1,BUF(ISSPIC+30),6*LFACT,255)
	    END IF
	    DO L=1,LFACT
	        CALL LINTIC(BUF,ISLPIC,ISSPIC,IELPIC,IESPIC,ILINE,LFACT,
     +			    TICBUF)
	        CALL WLINE(NSW,BUF,1)
	    END DO
	END DO
	RETURN
	END
C**************************************************************************
      SUBROUTINE GWEDGE(BUF,SSWEDG,NSWEDG,SLWEDG,ELWEDG,NLGL)
      IMPLICIT INTEGER(A-Z)
      COMMON /IOCOMMON/ IOUTUNIT,ILINE
      INTEGER*4 DN(16)/0,17,34,51,68,85,102,119,136,153,170,187,204,
     &                 221,238,255/
      LOGICAL*1 BUF(*)
C
      IF(ILINE.EQ.SLWEDG.OR.ILINE.EQ.ELWEDG) THEN
	  CALL ITLA(255,BUF(SSWEDG),NSWEDG)
      ELSE IF(ILINE.LE.ELWEDG) THEN
	  I = 16-(ILINE-SLWEDG-1)/NLGL
	  CALL ITLA(DN(I),BUF(SSWEDG+1),NSWEDG-2)
      ELSE IF(ILINE.EQ.ELWEDG+1) THEN
	  CALL ITLA(0,BUF(SSWEDG),NSWEDG)
      END IF
      RETURN
      END
C**************************************************************************
      SUBROUTINE WLINE(NSW,BUF,NLINE)
      COMMON /IOCOMMON/ IOUTUNIT,ILINE
      LOGICAL*1 BUF(*)
C
      DO I=1,NLINE
	  CALL XVWRIT(IOUTUNIT,BUF,ISTAT,'NSAMPS',NSW,' ')
      END DO
      ILINE = ILINE+NLINE
      RETURN
      END
C**********************************************************************
	SUBROUTINE WRITE_LABELS(NSO,LFACT,LOGO1,LOGO2,QLOGO,QID,OUTBUF)
C
	COMMON /LABS/ NLABS,LAB
	CHARACTER*70 LAB(100)
	INTEGER TIME
	CHARACTER*50 CID
	CHARACTER*24 DATTIME,USER,CTIME
	LOGICAL*1 QID,QLOGO,OUTBUF(*)
	BYTE ID(70)
C
	LMARG = (NSO-70*6*LFACT)/2 + 1
C							add 4*LFACT black lines
	CALL ITLA(0,OUTBUF,NSO)
	CALL WLINE(NSO,OUTBUF,4*LFACT)
C							add annotation labels
	DO I=1,NLABS
	    NCHAR = LEN(LAB(I))
	    CALL MVL(LAB(I),ID,NCHAR)
	    DO J=0,6
		CALL ITLA(0,OUTBUF,NSO)
		CALL TEXT(ID,NCHAR,J,OUTBUF(LMARG),6*LFACT,255)
		CALL WLINE(NSO,OUTBUF,LFACT)
	    END DO
	    CALL ITLA(0,OUTBUF,NSO)
	    CALL WLINE(NSO,OUTBUF,2*LFACT)
	END DO
C							add id and logos
	IF (QID .OR. QLOGO) THEN
	    CALL WLINE(NSO,OUTBUF,2*LFACT)
	END IF
C								top of logos
	IF (QLOGO) THEN
	    LOCR = NSO - 64*LFACT - 20
	    DO I=1,20
		CALL ADDLOGO(LOGO1,LOGO2,I,LFACT,LOCR,OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
	END IF
C									id
	IF (QID) THEN
	    ITIME = TIME()
	    DATTIME = CTIME(ITIME)
	    CALL XGETENV_VIC('USER',USER)
	    CID = ' ' // DATTIME // ' ' // USER
	    CALL MVL(CID,ID,50)
	    DO I=27,50
		IF (ID(I).EQ.0 .OR. ID(I).EQ.ichar(' ')) GO TO 200
	    END DO
  200	    CONTINUE
	    NCHAR = I-1
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE(' The MASKV image identifier is:',' ')
	    CALL XVMESSAGE(CID,' ')
	    CALL XVMESSAGE(' ',' ')
	    IPLST = (NSO-6*LFACT*NCHAR)/2 + 1
	    ILOC1 = IPLST+2
	    ILOC2 = IPLST+6+6*24+3
C						        top of box around pic id
	    CALL ITLA(255,OUTBUF(IPLST+2),4+6*24+4)
	    IF (LFACT.GT.1) CALL EXPAND(OUTBUF(IPLST),26*6,LFACT)
	    IF (QLOGO) CALL ADDLOGO(LOGO1,LOGO2,21,LFACT,LOCR,OUTBUF)
	    CALL WLINE(NSO,OUTBUF,LFACT)
C							     upper sides of box
	    CALL ITLA(0,OUTBUF,NSO)
	    DO K=1,2
		CALL ITLA(255,OUTBUF(ILOC1),1)
		CALL ITLA(255,OUTBUF(ILOC2),1)
		IF (LFACT.GT.1) CALL EXPAND(OUTBUF(IPLST),26*6,LFACT)
		IF (QLOGO) CALL ADDLOGO(LOGO1,LOGO2,21+K,LFACT,LOCR,
     +					OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
C								    write id
	    DO K=1,7
		CALL TEXT(ID(1),NCHAR,K-1,OUTBUF(IPLST),6,255)
		CALL ITLA(255,OUTBUF(ILOC1),1)
		CALL ITLA(255,OUTBUF(ILOC2),1)
		IF(LFACT.GT.1) CALL EXPAND(OUTBUF(IPLST),6*NCHAR,LFACT)
		IF (QLOGO) CALL ADDLOGO(LOGO1,LOGO2,23+K,LFACT,LOCR,
     +					OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
C						  	  lower sides of box
	    DO K=1,2
		CALL ITL(255,OUTBUF(ILOC1))
		CALL ITL(255,OUTBUF(ILOC2))
		IF (LFACT.GT.1) CALL EXPAND(OUTBUF(IPLST),26*6,LFACT)
		IF (QLOGO) CALL ADDLOGO(LOGO1,LOGO2,30+K,LFACT,LOCR,
     +					OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
C							  lower border of box
	    CALL ITLA(255,OUTBUF(IPLST+2),4+6*24+4)
	    IF (LFACT.GT.1) CALL EXPAND(OUTBUF(IPLST),26*6,LFACT)
	    IF (QLOGO) CALL ADDLOGO(LOGO1,LOGO2,33,LFACT,LOCR,OUTBUF)
	    CALL WLINE(NSO,OUTBUF,LFACT)
C					   blank lines between id and jpl label
	    CALL ITLA(0,OUTBUF,NSO)
	    DO K=34,37
		IF(QLOGO) CALL ADDLOGO(LOGO1,LOGO2,K,LFACT,LOCR,OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
C							   jpl image processing
	    CALL MVL('JPL IMAGE PROCESSING',ID,20)
	    JPLST = (NSO-6*23*LFACT)/2 + 1
	    DO K=1,7
		CALL TEXT(ID,20,K-1,OUTBUF(JPLST),6*LFACT,255)
		IF (QLOGO) CALL ADDLOGO(LOGO1,LOGO2,37+K,LFACT,LOCR,
     +					OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
C
	ELSE
C								logo, but no id
	    DO I=21,44
		CALL ADDLOGO(LOGO1,LOGO2,I,LFACT,LOCR,OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
	END IF
C								bottom of logos
	IF (QLOGO) THEN
	    CALL ITLA(0,OUTBUF,NSO)
	    DO I=45,64
		CALL ADDLOGO(LOGO1,LOGO2,I,LFACT,LOCR,OUTBUF)
		CALL WLINE(NSO,OUTBUF,LFACT)
		CALL ITLA(0,OUTBUF,NSO)
	    END DO
	END IF
C							     5 final blank lines
	CALL WLINE(NSO,OUTBUF,5)
C
	RETURN
	END
C*****************************************************************************
      SUBROUTINE ADDLOGO(LOGO1,LOGO2,LOGLIN,LFACT,LOCR,OUTBUF)
C
C	This routine adds the logos to the buffer OUTBUF. It adds a single
C	slice, designated by LOGLIN
C
      LOGICAL*1 OUTBUF(*)
C
      CALL LOGO(255,LOGLIN,LOGO1,0,OUTBUF(20))
      CALL LOGO(255,LOGLIN,LOGO2,0,OUTBUF(LOCR))
      IF(LFACT.GT.1) THEN
	  CALL EXPAND(OUTBUF(20),64,LFACT)
	  CALL EXPAND(OUTBUF(LOCR),64,LFACT)
      END IF
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create maskv.pdf
process help=*
 PARM INP         TYPE=(STRING,40)
 PARM OUT         TYPE=(STRING,40)
 PARM SIZE        TYPE=INTEGER         COUNT=4          DEFAULT=(1,1,0,0)
 PARM SL          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM SS          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM NL          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM NS          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM COMP        TYPE=KEYWORD COUNT=0:1 VALID=COMP DEFAULT=--
 PARM STRETCH     TYPE=INTEGER         COUNT=2          DEFAULT=(0,255)
 PARM EXPAND      TYPE=INTEGER         DEFAULT=1
 PARM TASK        TYPE=KEYWORD VALID=(TASK,NOTASK)      DEFAULT="TASK" 
 PARM USER        TYPE=KEYWORD VALID=(USER,NOUSER)      DEFAULT="NOUSER" 
 PARM HISTORY     TYPE=KEYWORD VALID=(HISTORY,NOHISTOR,TIMS) DEFAULT="HISTORY"  
 PARM ID          TYPE=KEYWORD VALID=(ID,NOID) 		DEFAULT=ID
 PARM LOGO	  TYPE=KEYWORD VALID=(LOGO,NOLOGO)	DEFAULT=NOLOGO
 PARM LLOGO       TYPE=INTEGER         VALID=(0:5)      DEFAULT=0
 PARM RLOGO       TYPE=INTEGER         VALID=(0:5)      DEFAULT=0
 PARM SYSTEM	  TYPE=KEYWORD VALID=(SYSTEM,NOSYSTEM)  DEFAULT=SYSTEM
 END-PROC
.TITLE
 MASKV  --  Format images for hardcopy display
.HELP
 MASKV formats images for hardcopy display. This includes the 
 numbering of lines and samples, a grey wedge along the right side,
 and, optionally, Vicar labels, logos, and a job id.

.PAGE
 RESTRICTIONS:

 MASKV only displays up to 70 characters per label line.
 Any label that exceeds 70 characters will be truncated to 70 characters
 on the hard copy.

.page
 ORIGINAL PROGRAMMER:  John H. Reimer,  10 MAY 1982
 
 CONVERTED TO VAX BY:  F. F. Moss,  20 JAN 1984
 
 CURRENT COGNIZANT PROGRAMMER: Ron Alley
.LEVEL1
.VARIABLE INP
 input data set
.VARIABLE OUT
 output data set
.VARIABLE SIZE
 image size






.VARIABLE SL
 starting line
.VARIABLE SS
 starting sample
.VARIABLE NL
 number of lines
.VARIABLE NS
 number of samples
.VARIABLE COMP
 complement the input image
 VALID: COMP
.VARIABLE STRETCH
 apply a linear stretch
 to the input image
.VARIABLE EXPAND
 enlarge the characters in
 the output by this factor
.VARIABLE ID
 ID's displayed
 VALID: ID, NOID
.VARIABLE LOGO
 LOGO's displayed
 VALID: LOGO, NOLOGO
.VARIABLE LLOGO
 left logo number (1-5)
.VARIABLE RLOGO
 right logo number (1-5)
.VARIABLE TASK
 display task names from the
 VICAR labels
 VALID: TASK, NOTASK
.VARIABLE USER
 display user name and date-
 time from the VICAR label
 VALID: USER, NOUSER
.VARIABLE HISTORY
 display VICAR history 
 labels
 VALID: 
 HISTORY, NOHISTOR, TIMS
.VARIABLE SYSTEM
 display VICAR system labels
 VALID: SYSTEM, NOSYSTEM
.LEVEL2
.VARIABLE INP
 The image to be masked for hardcopy output.
.VARIABLE OUT
 The output file, containing the masked image.
.VARIABLE SIZE
 (starting line,starting sample,number of lines,number of samples)
.VARIABLE SL
 starting line of the input image to be masked.
.VARIABLE SS
 starting sample of the input image to be masked.
.VARIABLE NL
 number of lines of the image to be included in the mask. If NL=0, all
 lines are used.
.VARIABLE NS
 number of samples of the image to be included in the mask. If NS=0,
 all samples are used.

.VARIABLE COMP
 COMP specifies that the input image is to be complemented before
 masking. (Default is not to complement)

.VARIABLE STRETCH
 STRETCH=(N1,N2) specifies that a linear stretch will be applied to
 the input image before masking.
 Defaults are:
   for byte:  N1=0, N2=255
   for halfword:   N1=0, N2=32767.

.VARIABLE EXPAND
 EXPAND=N specifies that all characters in the output will be 7*N
 lines by 6*N samples

 .VARIABLE ID
 This parameter controls the display of the account name and picture id.
 NOID specifies that id labels will not be displayed in the
 output. (Default is to display them)
 
.VARIABLE LOGO
 LOGO specifies that the JPL Logo will be displayed to both sides
 of the IPL id labels. (Default is not to display the logo)

.VARIABLE LLOGO
 LLOGO specifies a left logo. Values for LLOGO and corresponding
 logos are:
                  1 = JPL logo
                  2 = Mickey Mouse logo
                  3 = Goddard logo
                  4 = NASA logo
                  5 = ASTER logo
 Default is that no logos are displayed

.VARIABLE RLOGO
 RLOGO specifies a left logo. Values for RLOGO and corresponding
 logos are:
                  1 = JPL logo
                  2 = Mickey Mouse logo
                  3 = Goddard logo
                  4 = NASA logo
                  5 = ASTER logo
 RLOGO is similar to keyword LLOGO except that this keyword
 specifies a right logo. Default is that no logos are displayed.

.VARIABLE TASK
 If TASK is specified, MASKV searches for all occurrences of
 keyword 'TASK' in the VICAR history labels and then displays
 the values of 'TASK'.


.VARIABLE USER
 If USER is specified, MASKV searches for all occurrences of
 keywords 'DATETIME' and 'USER' in the VICAR history labels
 and then displays all of their respective values.
 
.VARIABLE HISTORY 
 If HISTORY is specified, then all VICAR history labels will be
 printed in the order in which they are found, excepting those
 with keywords 'TASK', 'USER', and 'DATETIME'. 

.VARIABLE SYSTEM
 If NOSYSTEM is specified the VICAR system label, containing the
 number of lines, number of samples, and data format, is not
 displayed with the masked imaged. The default is to display the
 system label.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create maskv.imake
#define  PROGRAM   maskv

#define MODULE_LIST maskv.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN
/* #define DEBUG */
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
