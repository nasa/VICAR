$!****************************************************************************
$!
$! Build proc for MIPL module xform
$! VPACK Version 1.9, Wednesday, February 11, 2004, 11:47:50
$!
$! Execute by entering:		$ @xform
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
$ write sys$output "*** module xform ***"
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
$ write sys$output "Invalid argument given to xform.com file -- ", primary
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
$   if F$SEARCH("xform.imake") .nes. ""
$   then
$      vimake xform
$      purge xform.bld
$   else
$      if F$SEARCH("xform.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xform
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xform.bld "STD"
$   else
$      @xform.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xform.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xform.com -mixed -
	-s xform.f -
	-p xform.pdf -
	-i xform.imake -
	-t tstxform.pdf tstxform.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xform.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C        'XFORM'   LINEAR TRANSFORMATION PROGRAM
C
C	11 FEB  04       ...LWK... Replaced call to ABORT with MABEND because
C				   of conflict with Linix intrinsic
C	 8 JAN  04       ...REA... Add BIL output capability,
C				   Expand permitted number of channels to 300
C				   Correct rounding bug for negative numbers.
C       23 MAR  00       ...REA... Add BIL, BSQ input capability,
C                                  change USE parameter name to USEBANDS,
C				   and change HISTSIZE and RANGE defaults
C	15 APR  91       ...REA... COMBINE ASU & PLDSJ1 VERSIONS FOR UNIX
C	30 JUNE 86       ...REA... REWRITE, IGNORING AP COMPATIBILITY	
C        5 JAN  77	 ...JDA... INITIAL RELEASE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        PARAMETERS
C     'MATRIX',...  TRANSFORMATION MATRIX (NI COL  X  NO LINES)
C     'PRESET'      SCALE PARAMETERS ARE PRESET TO X=0.    Y=1.
C                   THE DEFAULT OF BOTH 'SCALE' & 'PRESET' IS AUTOSCAN
C                   WHICH SCANS DATA & COMPUTES SCALE PARAMETERS
C     'PERCENT',R   (DECIMAL) PERCENT HISTOGRAM SATURATION (DEFAULT=1.)
C     'HPERCENT',R   PERCENT SATURATION AT HIGH END.(DEFAULT=0.5)
C     'LPERCENT',R    PERCENT SATURATION AT LOW END. (DEFAULT=0.5)
C     'INC',N       LINE & SAMPLE INCREMENT IN AUTOSCAN
C     'LINC',N      LINE INCREMENT IN AUTOSCAN
C     'SINC',N      SAMPLE INCREMENT IN AUTOSCAN
C     'CENTER',R    OUTPUT DATA CENTERED ABOUT R
C     'SPREAD',R    (DECIMAL) DESIRED SPREAD OF OUTPUT HISTOGRAM
C     'FIRM'        TRUNCATES ALL VALUES TO THE 'SPREAD' RANGE
C     'MSS',I       I IS THE NUMBER OF INTERLEAVED BANDS ON INPUT.
C     'AREA',SL,SS,NL,NS   AUTO-SCAN SAMPLE AREA
C     'FORMAT','XXXX' OUTPUT FORMAT  ('BYTE', 'HALF', 'FULL', 'REAL') 
C     'USEBANDS',I,J...  I,J,... ARE THE BAND NUMBERS TO USE AS INPUT DATA.
C     'GAIN',I,Y... GAIN = Y FOR BAND I
C     'OFFSET',I,X... OFFSET = X FOR BAND I
C     'EXCLUD',R    EXCLUDE INPUT PIXEL R FROM SCAN
C     'HISTSIZE',I  NUMBER OF HISTOGRAM BINS PER OUTPUT
C     'RANGE',X,Y   VALUES ASSOCIATED WITH THE FIRST AND LAST HISTOGRAM BINS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C WE USE THE FOLLOWING CONVENTIONS:
C
C  NIDS	   =  NUMBER OF INPUT DATASETS
C  NODS	   =  NUMBER OF OUTPUT DATASETS
C  NI	   =  NUMBER OF INPUT BANDS
C  NO	   =  NUMBER OF OUTPUT BANDS
C  NB      =  NUMBER OF INPUT BANDS AVAILABLE (MSS, BIL, BSQ, 
C						or separate inputs)
C
C  NSICHN  =  NUMBER OF SAMPLES / INPUT BAND
C  NSI     =  NUMBER OF SAMPLES / INPUT LINE
C  NSO     =  NUMBER OF SAMPLES / OUTPUT LINE
C
C
	PARAMETER (NDIM=300)
	EXTERNAL SCN,XFM
	COMMON /SCANBLOCK/LINC,ISINC,BOT,TOP,EXCLUD,EXCL,CENTER,SPREAD,
     +			  HGAIN,HOFFSET,NAREAS,IAREA
	LOGICAL EXCL
	INTEGER IAREA(200)
	COMMON /IOBLOCK/NI,NO,NIDS,NODS,XMATRIX,INUNIT,IOUTUNIT,GAIN,
     +			OFFSET,IBND,NSICHN,ISL,ISS,NL,NSO,
     +			CUTOFFB,CUTOFFT,MSS,MSSSUB,IFMT
	REAL CLIPLO(4)/0.0,-32768.0,-2.14748E9,-1.70141E38/
	REAL CLIPHI(4)/255.0,32767.0,2.14748E9,1.70141E38/
	REAL GAIN(NDIM),OFFSET(NDIM),XMATRIX(NDIM,NDIM),RPARM(NDIM*NDIM)
	INTEGER IBND(NDIM),INUNIT(30),IOUTUNIT(NDIM),IPARM(NDIM)
	LOGICAL SCAN/.TRUE./
	LOGICAL MSS,MSSSUB,XVPTST
	CHARACTER*132 BUF
	CHARACTER*4 FORMAT,OFORMAT,ORG
C								      initialize
	CALL XVMESSAGE('XFORM version 11-JAN-2004',' ')
	DO I=1,NDIM
	    GAIN(I) = 1.0
	    OFFSET(I) = 0.0
	    IBND(I) = I
	END DO
C
C					open input datasets, get size field
	CALL XVPCNT('INP',NIDS)
	CALL XVPCNT('OUT',NODS)
	DO I=1,NIDS
	    CALL XVUNIT(INUNIT(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(INUNIT(I),ISTAT,'U_FORMAT','REAL',
     +			'IO_ACT','SA','OPEN_ACT','SA',' ')
            CALL XVGET(INUNIT(I),ISTAT,'ORG',ORG,' ')
            IF (ORG .EQ. 'BIP') CALL MABEND(
     +                                  'BIP format is not supported')
	END DO
	CALL XVSIZE(ISL,ISS,NL,NS,NLI,NSI)
	CALL XVGET(INUNIT(1),ISTAT,'FORMAT',FORMAT,'NB',NB,' ')
	IF(ISL+NL-1.GT.NLI) CALL MABEND(
     +		' Number of lines requested exceeds input size')
	IF (NIDS.GT.1) NB=NIDS
C
C					        *** PROCESS PARAMETERS ***
C
C									format
	CALL XVPARM('FORMAT',OFORMAT,ICOUNT,IDEF,0)
	IF (ICOUNT.EQ.1) FORMAT = OFORMAT
	IFMT = 0
	IF(FORMAT.EQ.'BYTE') IFMT=1
	IF(FORMAT.EQ.'HALF') IFMT=2
	IF(FORMAT.EQ.'FULL') IFMT=3
	IF(FORMAT.EQ.'REAL') IFMT=4
	IF(IFMT.EQ.0) THEN
	    WRITE (BUF,100) FORMAT
  100	    FORMAT(A8,' pixel format is not supported')
	    CALL MABEND(BUF)
	END IF
C									inc
	CALL XVPARM('INC',LINC,ICOUNT,IDEF,0)
	ISINC = LINC
C									linc
	CALL XVPARM('LINC',I,ICOUNT,IDEF,0)
	IF (ICOUNT.EQ.1) LINC=I
C									sinc
	CALL XVPARM('SINC',I,ICOUNT,IDEF,0)
	IF (ICOUNT.EQ.1) ISINC=I
C									percent
	CALL XVPARM('PERCENT',PCENT,ICOUNT,IDEF,0)
	BOT = PCENT/2.0
	TOP = PCENT/2.0
C									lpercent
	CALL XVPARM('LPERCENT',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.EQ.1) BOT=RPARM(1)
C									hpercent
	CALL XVPARM('HPERCENT',RPARM,ICOUNT,IDEF,1)
	IF(ICOUNT.EQ.1) TOP=RPARM(1)
	PCENT = BOT+TOP
C									exclude
	CALL XVPARM('EXCLUDE',EXCLUD,ICOUNT,IDEF,0)
	EXCL = ICOUNT .NE. 0 
C									spread
	CALL XVPARM('SPREAD',SPREAD,ICOUNT,IDEF,0)
	IF(ICOUNT.EQ.0) SPREAD = CLIPHI(IFMT) - CLIPLO(IFMT)
C									center
	CALL XVPARM('CENTER',CENTER,ICOUNT,IDEF,0)
	IF (ICOUNT.EQ.0) THEN
	    IF (IFMT .EQ. 1) THEN
		CENTER = SPREAD / 2.0
	    ELSE
		CENTER = 0.0
	    END IF
	END IF
C									firm
	IF (XVPTST('FIRM') ) THEN
	    CUTOFFB = CENTER-SPREAD/2.0
	    CUTOFFT = CENTER+SPREAD/2.0
	ELSE
	    CUTOFFB = CLIPLO(IFMT)
	    CUTOFFT = CLIPHI(IFMT)
	END IF
C									mss
	CALL XVPARM('MSS',NIMSS,ICOUNT,IDEF,0)
	IF (ICOUNT.EQ.1) THEN
	    MSS = .TRUE.
	    NB = NIMSS
	    NSICHN = NSI/NB
	    IF (NIMSS*NSICHN.NE.NSI) CALL MABEND(
     +' number of input samples is not consistent with number of bands')
	ELSE
	    MSS = .FALSE.
	    NSICHN = NSI
	END IF
	IF(NS.EQ.NSI) THEN
	    MSSSUB = .FALSE.
	    NSO = NSICHN
	ELSE
	    MSSSUB = .TRUE.
	    NSO = NS
	    IF(ISS+NSO-1.GT.NSI) CALL MABEND(
     +             ' Number of samples specified exceeds input size')
	END IF
C									usebands
	CALL XVPARM('USEBANDS',IPARM,ICOUNT,IDEF,NDIM)
	IF(ICOUNT.NE.0) THEN
	    MSSSUB = .TRUE.
	    NI = ICOUNT
 	    IF(NI.GT.NB) CALL MABEND(
     +				' Inconsistent MSS and USE parameters')
	    DO J=1,NI
		IBND(J) = IPARM(J)
	    END DO
	ELSE
	    NI = NB
	END IF
C									area
	CALL XVPARM('AREA',IAREA,ICOUNT,IDEF,0)
	IF (MOD(ICOUNT,4) .NE. 0) CALL MABEND(
     +			' Invalid number of area parameter values')
	IF(ICOUNT.EQ.0) THEN
	    IAREA(1) = ISL
	    IAREA(2) = ISS
	    IAREA(3) = NL
	    IAREA(4) = NSO
	    NAREAS = 1
	ELSE
	    NAREAS = ICOUNT/4
	END IF
C									matrix
	CALL XVPARM('MATRIX',RPARM,ICOUNT,IDEF,0)
	NO = ICOUNT/NI
	IF (NI*NO.NE.ICOUNT) CALL MABEND(
     +				' Invalid number of matrix elements')
	N = 1
	DO I=1,NO
	    DO J=1,NI
		XMATRIX(J,I) = RPARM(N)
		N = N+1
	    END DO
	END DO
C									preset
	IF(XVPTST('PRESET')) SCAN=.FALSE.
C									gain
	CALL XVPARM('GAIN',RPARM,ICOUNT,IDEF,0)
	IF (ICOUNT.NE.0) THEN
	    SCAN=.FALSE.
	    IF (ICOUNT.EQ.1) THEN
		DO J=1,NO
		    GAIN(J) = RPARM(1)
		END DO
	    ELSE
		DO J=1,ICOUNT
		    GAIN(J) = RPARM(J)
		END DO
	    END IF
	END IF
C									offset
	CALL XVPARM('OFFSET',RPARM,ICOUNT,IDEF,0)
	IF (ICOUNT.NE.0) THEN
	    SCAN=.FALSE.
	    IF (ICOUNT.EQ.1) THEN
		DO J=1,NO
                    OFFSET(J) = RPARM(1)
	        END DO
	    ELSE
		DO J=1,ICOUNT
		    OFFSET(J)=RPARM(J)
		END DO
	    END IF
	END IF
C									histsize
	CALL XVPARM('HISTSIZE',IHSTLEN,ICOUNT,IDEF,0)
C									range
	CALL XVPARM('RANGE',RPARM,ICOUNT,IDEF,0)
	HGAIN = (IHSTLEN-1)/(RPARM(2)-RPARM(1))
	HOFFSET = 1.0 - HGAIN*RPARM(1)
C								open outputs
	IF (NODS .EQ. 1) THEN
	    IF (NO .EQ. 1) THEN
		ORG = 'BSQ'
	    ELSE
		ORG = 'BIL'
	    END IF
	    CALL XVUNIT(IOUTUNIT(1),'OUT',1,ISTAT,' ')
	    CALL XVOPEN(IOUTUNIT(1),ISTAT,'U_NL',NL,'U_NS',NSO,
     +			'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     +			'U_FORMAT','REAL','O_FORMAT',FORMAT,'U_NB',NO,
     +			'U_ORG',ORG,' ') 
	    DO J=1,NO
		IOUTUNIT(J) = IOUTUNIT(1)
	    END DO
	ELSE
	    DO I=1,NODS
		CALL XVUNIT(IOUTUNIT(I),'OUT',I,ISTAT,' ')
		CALL XVOPEN(IOUTUNIT(I),ISTAT,'U_NL',NL,'U_NS',NSO,
     +			    'OP','WRITE','OPEN_ACT','SA','IO_ACT','SA',
     +			    'U_FORMAT','REAL','O_FORMAT',FORMAT,
     +			    'U_NB',1,'U_ORG','BSQ',' ') 
		CALL XLADD(IOUTUNIT(I),'HISTORY','BAND',I,ISTAT,
     +			   'FORMAT','INT',' ')
	    END DO
	END IF
C
	IF (MSS) THEN
	    WRITE (BUF,200) NSO
  200	    FORMAT(' *** Output  NS =',I5,' ***')
	    CALL XVMESSAGE(BUF,' ')
	END IF
C						print the transformation matrix
	CALL XVMESSAGE(' ',' ')
	WRITE (BUF,300) NO,NI
  300	FORMAT('     (',I2,',',I2,') TRANSFORMATION MATRIX')
	CALL XVMESSAGE(BUF,' ')
	NK = (NI-1)/13+1
	N1 = 1
	N2 = MIN0(NI,13)
	DO K=1,NK
	    CALL XVMESSAGE(' ',' ')
	    DO J=1,NO
		LOC = 2
		WRITE (BUF,400) (XMATRIX(I,J),I=N1,N2)
  400		FORMAT(1X,13F10.4)
		CALL XVMESSAGE(BUF,' ')
	    END DO
	    N1 = N2+1
	    N2 = MIN0(N2+13,NI)
	END DO
	CALL XVMESSAGE(' ',' ')
C							compute buffer sizes
	INSO = NSO
	INI = NI
	INO = NO
C					    no need to rescale floating point
	IF (IFMT.EQ.4) SCAN=.FALSE.
	IF (SCAN) THEN
C							auto-scale scan phase
C							print the scaling info
C
	    CALL XVMESSAGE(' *** AUTO-SCALE MODE ***',' ')
	    WRITE (BUF,500) SPREAD,CENTER,PCENT
  500	    FORMAT(5X,'Desired Spread',F8.1,'   Center',F8.1,
     +             '   Percent Sat.',F5.1)
	    CALL XVMESSAGE(BUF,' ')
C							   call SCN via STACKA
	    L1 = INSO*INI*4
	    L2 = IHSTLEN*INO*4
	    CALL STACKA(8,SCN,2,L1,L2,INSO,IHSTLEN,INI,INO)
	END IF
C
C							   transformation phase
	CALL XVMESSAGE(' *** SCALING FACTORS ***',' ')
	CALL XVMESSAGE('     OFFSET    GAIN',' ')
	DO K=1,NO
	    WRITE (BUF,600) OFFSET(K),GAIN(K)
  600	    FORMAT(F11.3,F9.3)
	    CALL XVMESSAGE(BUF,' ')
C						    to compensate for truncation
C						    when going to integer output
	END DO
C						 allocate buffers and call XFM
	L1 = 4*NI*NSO
	L2 = 4*NO*NSO
	CALL STACKA(7,XFM,2,L1,L2,INSO,INI,INO)
	RETURN
	END
C******************************************************************************
C								SUBROUTINE SCN
C
	SUBROUTINE SCN(XIN,L1,IHIST,L2,INSO,IHSTLEN,INI,INO)
	PARAMETER (NDIM=300)
	COMMON /SCANBLOCK/LINC,ISINC,BOT,TOP,EXCLUD,EXCL,CENTER,SPREAD,
     +			  HGAIN,HOFFSET,NAREAS,IAREA
	LOGICAL EXCL
	INTEGER IAREA(200)
	COMMON /IOBLOCK/NI,NO,NIDS,NODS,XMATRIX,INUNIT,IOUTUNIT,GAIN,
     +			OFFSET,IBND,NSICHN,ISL,ISS,NL,NSO,
     +			CUTOFFB,CUTOFFT,MSS,MSSSUB,IFMT
	REAL XMATRIX(NDIM,NDIM),GAIN(NDIM),OFFSET(NDIM)
	INTEGER INUNIT(30),IOUTUNIT(NDIM),IBND(NDIM)
	LOGICAL MSS,MSSSUB,QSUB
	INTEGER LOC(NDIM)
	REAL XIN(INSO,INI)
	INTEGER IHIST(IHSTLEN,INO)
	CHARACTER*132 BUF
	CHARACTER*8 KEY
C
	BOT_OUT = CENTER-SPREAD/2.0
	CALL ZIA(IHIST,IHSTLEN*NO)
C						       histogram gathering loop
	DO II=1,NAREAS
	    ISLA = IAREA(4*II-3)
	    ISSA = IAREA(4*II-2)
	    NLA = IAREA(4*II-1)
	    NSA = IAREA(4*II)
	    IELA = ISLA+NLA-1
	    WRITE (BUF,100) ISLA,ISSA,NLA,NSA,LINC,ISINC
  100	    FORMAT('     AREA SAMPLED (',I5,',',I5,',',I5,',',I5,
     +             ')   LINC=',I3,'   SINC=',I3)
	    CALL XVMESSAGE(BUF,' ')
	    WRITE (BUF,200) ISLA,ISSA,NLA,NSA,LINC,ISINC
  200	    FORMAT(I5,',',I5,',',I5,',',I5,')   LINC=',I3,'   SINC=',I3)
	    DO III=1,NODS
		KEY = 'SUBAREA' // CHAR(II+48)
		CALL XLADD(IOUTUNIT(III),'HISTORY',KEY,BUF,
     +			   ISTAT,'FORMAT','STRING',' ')
	    END DO
C							set MSS subarea pointers
	    IF (NSA.NE.NSICHN .OR. MSSSUB) THEN
		QSUB = .TRUE.
		DO I=1,NI
		    LOC(I) = (IBND(I)-1)*NSICHN + ISSA
		END DO
	    ELSE
		QSUB = .FALSE.
	    END IF
C							read data
	    DO I=ISLA,IELA,LINC
		IF (MSS) THEN			   ! MSS with subarea or subset
		    IF (QSUB) THEN
			DO J=1,NI
			    CALL XVREAD(INUNIT(1),XIN(1,J),ISTAT,'LINE',
     +				       I,'SAMP',LOC(J),'NSAMPS',NSA,' ')
			END DO
		    ELSE					! full MSS image
			CALL XVREAD(INUNIT(1),XIN,ISTAT,'LINE',I,' ')
		    END IF
		ELSE					   ! individual input ds
		    IF (NIDS.GT.1 .OR. NI.EQ.1) THEN
			DO J=1,NI
			    CALL XVREAD(INUNIT(J),XIN(1,J),ISTAT,
     +				'LINE',I,'SAMP',ISSA,'NSAMPS',NSA,' ')
			END DO
		    ELSE
			DO J=1,NI
			    CALL XVREAD(INUNIT(1),XIN(1,J),ISTAT,
     +					'LINE',I,'BAND',IBND(J),
     +					'SAMP',ISSA,'NSAMPS',NSA,' ')
			END DO
		    END IF
		END IF
C							        update histogram
		DO J=1,NSA,ISINC
		    DO K=1,NO
			X = 0.0
			DO L=1,NI
			    X = X+XMATRIX(L,K)*XIN(J,L)
			END DO
			N = MIN(MAX(NINT(X*HGAIN+HOFFSET),1),IHSTLEN)
			IHIST(N,K) = IHIST(N,K)+1
		    END DO
		END DO
	    END DO
	END DO
C					      remove spike, if EXCLUD specified
	IF (EXCL) THEN
	    DO I=1,NO
		X = 0.0
		DO J=1,NI
		    X = X+XMATRIX(J,I)*EXCLUD
		END DO
		N = MIN(MAX(NINT(X*HGAIN+HOFFSET),1),IHSTLEN)
		IHIST(N,I) = 0
	    END DO
	END IF
C
	DO I=1,NO
C							 sum histogram bins
	    N = 0
	    DO J=1,IHSTLEN
		N = N+IHIST(J,I)
	    END DO
	    BOT_SAT = MAX((N*BOT)/100.0, 0.5)
	    TOP_SAT = MAX((N*TOP)/100.0, 0.5)
C						 find low end saturation point
	    R = 0.0
	    J = 0
	    DO WHILE (R.LT.BOT_SAT)
		J = J+1
		R = R+IHIST(J,I)
	    END DO
	    BOT_VAL = (J-(R-BOT_SAT)/IHIST(J,I)-HOFFSET)/HGAIN
C						 find high end saturation point
	    R = 0.0
	    J = IHSTLEN+1
	    DO WHILE (R.LT.TOP_SAT)
		J = J-1
		R = R+IHIST(J,I)
	    END DO
	    TOP_VAL = (J+(R-TOP_SAT)/IHIST(J,I)-HOFFSET)/HGAIN
C							    compute gain/offset
	    R = TOP_VAL - BOT_VAL
	    IF (R.EQ.0.0) CALL MABEND(' Histogram has no spread')
	    GAIN(I) = SPREAD/R
	    OFFSET(I) = BOT_OUT-GAIN(I)*BOT_VAL
	END DO
C
	RETURN
	END
C******************************************************************************
C								SUBROUTINE XFM
C
	SUBROUTINE XFM(XIN,L1,XOUT,L2,INSO,INI,INO)
C
	PARAMETER (NDIM=300)
	COMMON /IOBLOCK/NI,NO,NIDS,NODS,XMATRIX,INUNIT,IOUTUNIT,GAIN,
     +			OFFSET,IBND,NSICHN,ISL,ISS,NL,NSO,
     +			CUTOFFB,CUTOFFT,MSS,MSSSUB,IFMT
	REAL XMATRIX(NDIM,NDIM),GAIN(NDIM),OFFSET(NDIM)
	INTEGER INUNIT(30),IOUTUNIT(NDIM),IBND(NDIM)
	LOGICAL MSS,MSSSUB
	INTEGER LOC(NDIM)
	REAL XIN(INSO,INO),XOUT(INSO,INO)
C							apply gain to matrix
	DO I=1,NO
	    DO J=1,NI
		XMATRIX(J,I) = XMATRIX(J,I)*GAIN(I)
	    END DO
	END DO
C							set MSS subarea pointers
	IF (MSSSUB) THEN
	    DO I=1,NI
		LOC(I) = (IBND(I)-1)*NSICHN + ISS
	    END DO
	END IF
C							    transformation loop
	IEL = ISL+NL-1
	DO I=ISL,IEL
C							read data
	    IF (MSS) THEN			   ! MSS with subarea or subset
		IF (MSSSUB) THEN
		    DO J=1,NI
			CALL XVREAD(INUNIT(1),XIN(1,J),ISTAT,'LINE',I,
     +				    'SAMP',LOC(J),'NSAMPS',NSO,' ')
		    END DO
		ELSE						! full MSS image
		    CALL XVREAD(INUNIT(1),XIN,ISTAT,'LINE',I,' ')
		END IF
	    ELSE					   ! individual input ds
		IF (NIDS.GT.1 .OR. NI.EQ.1) THEN
		    DO J=1,NI
			CALL XVREAD(INUNIT(J),XIN(1,J),ISTAT,'LINE',I,
     +				    'SAMP',ISS,'NSAMPS',NSO,' ')
		    END DO
		ELSE
		    DO J=1,NI
			CALL XVREAD(INUNIT(1),XIN(1,J),ISTAT,'LINE',I,
     +				    'BAND',IBND(J),'SAMP',ISS,
     +				    'NSAMPS',NSO,' ')
		    END DO
		END IF
	    END IF
C							  compute transformation
	    DO J=1,NSO
		DO K=1,NO
		    XOUT(J,K) = OFFSET(K)
		    DO L=1,NI
			XOUT(J,K) = XOUT(J,K)+XMATRIX(L,K)*XIN(J,L)
		    END DO
		    XOUT(J,K) = MIN(MAX(XOUT(J,K),CUTOFFB),CUTOFFT)
		END DO
	    END DO
C						       for integer pixels, round
	    IF (IFMT .NE. 4) THEN
		DO K=1,NO
		    DO J=1,NSO
			XOUT(J,K) = NINT(XOUT(J,K))
		    END DO
		END DO
	    END IF
C								   write output
	    DO J=1,NO
		CALL XVWRIT(IOUTUNIT(J),XOUT(1,J),ISTAT,'NSAMPS',NSO,
     +			   ' ')
	    END DO
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create xform.pdf
process help=*
PARM INP       TYPE=STRING     COUNT=(1:30)
PARM OUT       TYPE=STRING     COUNT=(1:30)
PARM SIZE      TYPE=INTEGER    COUNT=4                DEFAULT=(1,1,0,0)
PARM SL	       TYPE=INTEGER			      DEFAULT=1
PARM SS	       TYPE=INTEGER			      DEFAULT=1
PARM NL	       TYPE=INTEGER			      DEFAULT=0
PARM NS	       TYPE=INTEGER			      DEFAULT=0
PARM MATRIX    TYPE=REAL       COUNT=(0,1:200)        DEFAULT=--
PARM FORMAT    TYPE=KEYWORD    COUNT=(0:1)  VALID=(BYTE,HALF,FULL,REAL) +
		DEFAULT=--
PARM MSS       TYPE=INTEGER    COUNT=(0:1)  VALID=(0:32)     DEFAULT=--
PARM USEBANDS  TYPE=INTEGER    COUNT=(0:31) VALID=(1:32)     DEFAULT=--
PARM PRESET    TYPE=KEYWORD    COUNT=(0:1)  VALID="PRESET"   DEFAULT=--
PARM GAIN      TYPE=REAL       COUNT=(0:32)           DEFAULT=--
PARM OFFSET    TYPE=REAL       COUNT=(0:32)           DEFAULT=--
PARM AREA      TYPE=INTEGER    COUNT=(0:200)          DEFAULT=--
PARM INC       TYPE=INTEGER    			      DEFAULT=1
PARM LINC      TYPE=INTEGER    COUNT=(0,1)            DEFAULT=--
PARM SINC      TYPE=INTEGER    COUNT=(0,1)	      DEFAULT=--
PARM PERCENT   TYPE=REAL                              DEFAULT=1.0
PARM LPERCENT  TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM HPERCENT  TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM SPREAD    TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM FIRM      TYPE=KEYWORD    COUNT=(0:1)  VALID="FIRM"   DEFAULT=--
PARM CENTER    TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM EXCLUDE   TYPE=REAL       COUNT=(0,1)            DEFAULT=--
PARM HISTSIZE  TYPE=INTEGER			      DEFAULT=50001
PARM RANGE     TYPE=REAL       COUNT=(0,2)	DEFAULT=(-25000.0,25000.0)
PARM PARMS     TYPE=STRING     COUNT=(0,1)	      DEFAULT=--
END-PROC
.TITLE
 XFORM
.HELP
 "XFORM" will perform a linear transformation on the input data.
 The transformation is specified  by a matrix input as a 
 parameter. If desired, XFORM will scale the output to a user
 specified range, which defaults to the full range of the data 
 type. (XFORM will now accept a parameter dataset as input)

 EXECUTION:

 XFORM (INP1,INP2,...) (OUT1,OUT2,...) SIZE PARAMS          or
 XFORM  INP            (OUT1,OUT2,...) SIZE PARAMS          or
 XFORM (INP1,INP2,...)  OUT            SIZE PARAMS          or
 XFORM  INP             OUT            SIZE PARAMS          or

 OPERATION:

 Let each corresponding pixel from the input data sets be
 represented by a vector x[T] = (x1,x2,...xm)[T], where [T}
 denotes Transpose. Let A be the parameter matrix (n * m).
 Then XFORM computes the product:

 AX = V

 A gain and offset are then applied to each component before
 it is written to the output data set:

 V'(i) = P(i)  + Q(i).V(i)

 Auto-scale mode is the default condition if gains and offsets
 are not specified using preset or gain/offset.
 In auto-scale mode, the gains and offsets for each band are
 calculated by fitting the output histogram to user - specified
 parameters. This calculation requires a separate pass, which 
 can double the computation time.

 RESTRICTIONS:

 Currently, the program limits NI and NO to 32 because the relevant
 arrays are dimensioned to that size.

.PAGE
NOTES by Ron Alley on the version of XFORM delivered Feb.'04:

INP       -  BIL and BSQ format inputs are now permitted, in addition to
             the previously allowed MSS format and multiple files of single
             bands. Multiple files of multichannel data are not permitted.

OUT       -  Output is now always in BIL format for multichannel output, and
             BSQ for files containing a single channel.

USE       -  is renamed USEBANDS

MSSO      -  deleted, since MSS outp[ut format is no longer supported

DIVIDE    -  deleted, since MATRIX values may now be real numbers, this
             parameter is obsolete.

SCALE     -  deleted, since it is redundant with GAIN, OFFSET, and PRESET

SAMPLE    -  deleted, since it is redundant with LINC            

INC       -  default is now INC=1, rather than SAMPLE=10

This version of XFORM is meant to replace the previous procedure XFORM, and
XFORMEM, the process called by XFORM. The previous stucture was to accomodate
the array processor hardware and software located on one of the MIPL VAX'es,
and is no longer meaningful.

The previous version of XFORM incorrectly rounded negative output pixel values
one too high for halfword and fullword outputs. This has been fixed.

In the auto-scale mode, the default SPREAD and CENTER values for byte pixel
outputs has been changed from 256 and 128 to 255 and 127.5

The format of the output printed to the screen and session log has been very
slightly changed.
.PAGE
HISTORY:

 XFORM written by:  J.D.Addington,  Jan. 1977

 Array Processor version of XFORM written by:  L.W.Kamp,  ??? 1982

 XFORM converted to VAX by:  F.F.Moss, Jan. 1984

 XFORMEM (AP emulation) written by:  L.W.Kamp,  Sept. 1984

 XFORMEM converted to VICAR2 by:  J.H.Reimer, July 1985

 XFORMEM rewritten ignoring AP compatibility: Ron Alley, June 1986

 XFORMEM ported to Unix: Steve Pohorsky, July 1994

 Replaced proc XFORM calling program XFORMEM with new program XFORM;
 support for BSQ & BIL files added;  expanded permitted number of
 channels to 300; keywords MSSO, DIVIDE, SCALE, SAMPLE deleted: 
 Ron Alley, Feb. 2004

 CURRENT COGNIZANT PROGRAMMER:  L.W.Kamp, Feb.2004
.LEVEL1 
.VARIABLE INP
 Input file name(s)
.VARIABLE OUT
 Output file name(s)
.VARIABLE SIZE
 Standard VICAR size field
.VARIABLE MATRIX
 The input matrix
.VARIABLE FORMAT
 Output data format.
 Valid: BYTE, HALF, FULL, REAL.
.VARIABLE MSS
 Number of bands in MSS input
.VARIABLE USEBANDS
 Use only these bands of the
 multichannel input
.VARIABLE PRESET
 Non auto-scale mode
.VARIABLE GAIN
 Specify the gains for all
 outputs
.VARIABLE OFFSET
 Specify the offsets for all
 outputs
.VARIABLE AREA
 (PARM FOR AUTO-SCALE MODE)
 Specify the certain area
 to be sampled
.VARIABLE INC
 (PARM FOR AUTO-SCALE MODE)
 Line and sample increments
.VARIABLE LINC
 (PARM FOR AUTO-SCALE MODE)
 Line increments
.VARIABLE SINC
 (PARM FOR AUTO-SCALE MODE)
 Sample increments
.VARIABLE PERCENT
 (PARM FOR AUTO-SCALE MODE)
 Percent saturation in
 output histogram
.VARIABLE LPERCENT
 (PARM FOR AUTO-SCALE MODE)
 Percent saturation at the
 lower end in output histogram
.VARIABLE HPERCENT
 (PARM FOR AUTO-SCALE MODE)
 Percent saturation at the
 higher end in output histogram
.VARIABLE SPREAD
 (PARM FOR AUTO-SCALE MODE)
 Specify the spread in output
 histogram
.VARIABLE FIRM
 Causes output to be firmly
 clipped to range specified
 by SPREAD and CENTER
.VARIABLE CENTER
 (PARM FOR AUTO-SCALE MODE)
 Specify the center in output
 histogram
.VARIABLE EXCLUDE
 (PARM FOR AUTO-SCALE MODE)
 Ignore this input DN
.VARIABLE HISTSIZE
(PARM FOR AUTO-SCALE MODE)
Number of histogram bins per
output band
.VARIABLE RANGE
(PARM FOR AUTO-SCALE MODE)
Value of first and last
histogram bins.
.LEVEL2
.VARIABLE INP
 Input file names (can be byte, halfword, I*4, or R*4)
.VARIABLE OUT
 Output file names (can be byte, halfword, I*4, or R*4)
.VARIABLE SIZE
 The standard VICAR size field: starting line, starting sample,
 number of lines, and number of samples
 (if MSS is specified, the size field refers to one band, not to the
 entire image.)
.VARIABLE MATRIX
 MATRIX = (M11,M12...)   The matrix values are real numbers and are 
 entered in row order. The column number is the number of input data
 sets or bands, the row number is the number of output data sets or bands
.VARIABLE FORMAT
 Valid keyword values: BYTE, HALF, FULL, REAL.
 This specifies the output data format. 
.VARIABLE MSS
 MSS=NI            Denotes that the input data set is in MSS format with
 NI interleaved inputs.  limit: NI <= 32
.VARIABLE USEBANDS
 If the input is a single file of multichannel data, the USEBANDS parameter
 is used to specify which bands are to be used as input, and the order that
 must be used to be consistent with the MATRIX parameter.  The default is to
 use all bands, in ascending order.
.VARIABLE PRESET
 Offset  = 0 and gain = 1 for all output bands.
.VARIABLE GAIN
    GAIN=Q                   For all output bands, 
 or GAIN=(Q1,Q2,Q3,...)      The nth output band is scaled by  gain Qn. 
.VARIABLE OFFSET
    OFFSET=P                 For all output bands,
 or OFFSET=(P1,P2,P3,...)    The nth output band is scaled by offset Pn.
.VARIABLE AREA
 AREA=(SL,SS,NL,NS)     Indicates area to be sampled for auto-scaling.
.VARIABLE INC
 INC=N		For auto-scaling, every Nth sample of every Nth line is used.
.VARIABLE LINC
 LINC=N		For auto-scaling, every Nth line is used.
.VARIABLE SINC
 SINC=N		For auto-scaling, every Nth sample is used.
.VARIABLE PERCENT
 PERC=P       P percent saturation is to be allowed in output histograms,
 one-half at each end. (default=1)
.VARIABLE LPERCENT
 LPER=L      L percent saturation is to be allowed in output histograms
 at the low end. (default=0.5)
.VARIABLE HPERCENT
 HPER=H      H percent saturation is to be allowed in output histograms
 at the high end. (default=0.5)
.VARIABLE SPREAD
 SPREAD=S      Output histograms are to have a spread of s. (default=full
 range of data type)  See also parameter FIRM.
.VARIABLE FIRM
 Specify FIRM to cause the output to be clipped so that SPREAD and CENTER
 apply firmly.  Otherwise (the default) SPREAD and CENTER apply approximately.
.VARIABLE CENTER
 CENTER=C      Output histogram is to be centered about c. (default=SPREAD/2
 for byte data, =0 otherwise.)
.VARIABLE EXCLUDE
 EXCLUDE=E     This keyword instructs the program to ignore input pixels of 
 value e when determining saturation points.  The algorithm used simply 
 zeros the histogram bin which results from each input DN level e.
 However, other combinations of input DNs will in general also contribute
 to this bin and will be erroneously ignored. Therefore, this keyword should 
 only be used if a large contribution from spurious value (e.g., zero)
 is expected.
.VARIABLE HISTSIZE
In the auto-scale mode, a histogram is first formed to determine the range
of the output data without rescaling. This parameter allows the user to 
specify the number of bins used to form the histogram of each output band.
While this feature is rarely needed for byte data, the user may want to
adjust this (and the related RANGE parameter) for other data types, if the
unscaled output range is uncertain.
.VARIABLE RANGE
In the auto-scale mode, a histogram is first formed to determine the range
of the output data without rescaling. This parameter allows the user to 
specify the range of the bins used to form the histogram of each output band.
While this feature is rarely needed for byte data, the user may want to
adjust this (and the related HISTSIZE parameter) for other data types. The
auto-scaling algorithm works best when the histogram bins span only the
actual range of the data. Therefore, if it is known that the transformation
will produce values only within a certain range, that range should be 
specified here. 
$ Return
$!#############################################################################
$Imake_File:
$ create xform.imake
#define  PROGRAM   xform

#define MODULE_LIST xform.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$Test_File:
$ create tstxform.pdf
procedure
refgbl $echo
refgbl $becho
body
let _onfail="continue"
let $echo="yes"
let $becho=("yes","yes")

WRITE "THIS IS A SCRIPT FILE TO TEST PROCEDURE XFORM"

gen AX 40 40
gen BX 40 40 LINC=0 SINC=1
gen CX 40 40 LINC=1 SINC=0

WRITE "TEST THE REGULAR TRANSFORMATION PHASE"
WRITE "TEST THE BYTE OUTPUT"
WRITE "TEST THE PARM SIZE"
xform (AX,BX,CX) D (1,1,20,20) 'BYTE +
 MATR=(1.,2.,1.) 'PRES
list  D (1,1,5,5)
list  D (16,16,5,5)

WRITE "TEST WITHOUT THE SIZE SPECIFIED"
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.) 'PRES
list  D (1,1,5,5)

WRITE "TEST THE HALFWORD OUTPUT"
xform (AX,BX,CX) D 'HALF MATR=(1.,2.,1.) 'PRES
list  D (1,1,5,10) 'HALF

WRITE "TEST THE AUTO-SCALE MODE"
WRITE "TEST THE BYTE OUTPUT"
WRITE "TEST THE PARM LINC AND PERC, AND MULTIPLE OUTPUTS"
xform (AX,BX,CX) (D,E)  'BYTE MATR=(1.,2.,1.,2.,1.,2.) +
 LINC=5 PERC=12.
list  D (1,1,15,15)
list  E (1,1,15,15)

WRITE "TEST THE PARM LPER AND HPER"
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.) +
 LPER=1. HPER=3.
list  D (1,1,10,10)
list  D (31,31,10,10)

WRITE "TEST THE PARM SPREAD"
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.) +
 SPRE=128. PERC=1.
list  D (1,1,5,5)
list  D (36,36,5,5)

WRITE "TEST THE PARM CENTER & firm"
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.) +
 CENT=100 PERC=1 SPREAD=200 'FIRM
list  D (1,1,5,5)
list  D (36,36,5,5)

WRITE "TEST THE PARM AREA"
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.) +
 AREA=(11,11,20,20) PERC=12.
list  D (11,11,20,20)

WRITE "TEST THE HALFWORD OUTPUT"
xform (AX,BX,CX) D 'HALF  MATR=(1.,1.,1.) LINC=3 PERC=12.
list  D (1,1,5,10) 'HALF

WRITE "TEST THE BYTE OUTPUT WITH MORE THAN ONE OUTPUT"
WRITE "  "
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
copy AX BX
xform (AX,BX) (G,GG) 'BYTE MATR=(1.,-1.,-1.,1.) 'PRES
list  G
list  GG

WRITE "TEST THE MSS BYTE OUTPUT"
mss (AX,BX) MSS
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
xform MSS D 'BYTE MSS=2 MATR=(1,-1) 'PRES
list  D 

WRITE "TEST THE MSS SPECIFIED PARM USE"
mss (AX,CX,BX) MSS
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
xform MSS D 'BYTE MSS=3 USE=(1,3) MATR=(1,-1) 'PRES
list  D

WRITE "TEST THE MSS HALFWORD OUTPUT"
gen AY 10 20 'HALF
gen BY 10 20 'HALF
mss (AY,BY) MSS

WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
xform MSS D  'HALF MSS=2 MATR=(1,-1) 'PRES
list  D

write "=============================================="
WRITE "TEST THE AREA PARAMETER WITH ONE OR MORE AREAS"
gen AX 10 10 LINC=10 SINC=1
gen BX 10 10 LINC=10 SINC=1 IVAL=100
MSS (AX,BX) MSSB1
list  AX
WRITE "TRY AX AS A SINGLE FILE AND AS A MSS FILE WITH ONE BAND."
WRITE "SHOULD GET SAME RESULTS EITHER WAY. "
WRITE "OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255."
xform AX OUT=XFORM0 MATR=1. PERC=0. LINC=1
list  XFORM0 'NOEJECT
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. LINC=1
list  XFORM0 'NOEJECT

WRITE "TRY SIZE FIELD TO GET 7 BY 8 OUTPUT"
WRITE "OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255."
xform AX OUT=XFORM0 MATR=1. PERC=0. SIZE=(3,2,7,8) LINC=1
list  XFORM0 'NOEJECT
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. SIZE=(3,2,7,8) LINC=1
list  XFORM0 'NOEJECT

WRITE "TRY AREA"
WRITE "OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255 IN THE SPECIFIED AREA."
WRITE "OUTPUT IS SATURATED OUTSIDE THE AREA."
xform AX OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,7,8)
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,7,8)
list  XFORM0 'NOEJECT 'ZERO
xform AX OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8)
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8)
list  XFORM0 'NOEJECT 'ZERO
xform AX OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8  7,2,1,8)
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8  7,2,1,8)
list  XFORM0 'NOEJECT 'ZERO

WRITE "THE FOLLOWING GROUP WITH PERC=20. SHOULD ALL YIELD THE SAME OUTPUT"
WRITE "THE TOP AND BOTTOM LINES SHOULD BE SATURATED."
xform AX OUT=XFORM0 MATR=1. PERC=20. LINC=1
list  XFORM0 'NOEJECT 'ZERO 
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. LINC=1
list  XFORM0 'NOEJECT 'ZERO
xform AX OUT=XFORM0 MATR=1. PERC=20. SIZE=(1,1,10,10) LINC=1
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. SIZE=(1,1,10,10) LINC=1
list  XFORM0 'NOEJECT 'ZERO
xform AX OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,10)
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,10)
list  XFORM0 'NOEJECT 'ZERO
xform AX OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,2  1,3,10,6  1,9,10,2)
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,2  1,3,10,6  1,9,10,2)
list  XFORM0 'NOEJECT 'ZERO
xform AX OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,2,10  3,1,6,10  9,1,2,10)
list  XFORM0 'NOEJECT 'ZERO
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,2,10  3,1,6,10  9,1,2,10)
list  XFORM0 'NOEJECT 'ZERO

WRITE "OUTPUT SHOULD BE STRETCHED WITH 0 PERCENT SATURATION"
xform MSSB1 (X,Y) MSS=2 MATR=(1.,0., 0.,1.) linc=1 perc=0
list  X
list  Y
xform (AX BX) (X1,Y1) MATR=(1.,0., 0.,1.) linc=1 perc=0
WRITE "SHOULD GET 0 DIFFERENCES."
difpic (X X1)
difpic (Y Y1)

! more tests

! TEST EXCLUDE
gen A0 10 10 ival=0 linc=0 sinc=0
gen A 10 10 ival=100
fastmos (A A0) A1 SIZE=(1,1, 100,100)
list A1
label-list A1
xform A1 OUT=B1 MATR=1. PERC=10. INC=1 EXCLUDE=0
xform A OUT=B MATR=1. PERC=10. INC=1
! SHOULD GET 0 DIFFERENCES
difpic (B B1)

! TEST REAL OUTPUT
xform A OUT=B 'real MATR=1. 'preset
list B

! TEST FULL OUTPUT
xform A OUT=B 'FULL MATR=1. 'preset
list B

!TEST BSQ INPUT
gen A 40 40 3
xform A B (1,1,20,20) 'BYTE +
 MATR=(1.,2.,1.) 'PRES
label-list B
list  B (1,1,5,5) nb=1
list  B (16,16,5,5) nb=1
list  B (1,1,5,5) nb=1 sb=3
list  B (16,16,5,5) nb=1 sb=3

!TEST BIL OUTPUT
!THIS IS THE SAME AS THE AUTO-SCALE TEST ABOVE
gen AX 40 40
gen BX 40 40 LINC=0 SINC=1
gen CX 40 40 LINC=1 SINC=0
xform (AX,BX,CX) D 'BYTE MATR=(1.,2.,1.,2.,1.,2.) +
 LINC=5 PERC=12.
label-list D
list  D (1,1,15,15) 'bsq

end-proc
$!-----------------------------------------------------------------------------
$ create tstxform.log
tstxform
let $becho=("yes","yes")
WRITE "THIS IS A SCRIPT FILE TO TEST PROCEDURE XFORM"
THIS IS A SCRIPT FILE TO TEST PROCEDURE XFORM
gen AX 40 40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen BX 40 40 LINC=0 SINC=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen CX 40 40 LINC=1 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
WRITE "TEST THE REGULAR TRANSFORMATION PHASE"
TEST THE REGULAR TRANSFORMATION PHASE
WRITE "TEST THE BYTE OUTPUT"
TEST THE BYTE OUTPUT
WRITE "TEST THE PARM SIZE"
TEST THE PARM SIZE
xform (AX,BX,CX) D (1,1,20,20) 'BYTE  +
 MATR=(1.,2.,1.) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    2.0000    1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list  D (1,1,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
     Samp     1       3       5
   Line
      1       0   3   6   9  12
      2       2   5   8  11  14
      3       4   7  10  13  16
      4       6   9  12  15  18
      5       8  11  14  17  20
list  D (16,16,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
     Samp    16      18      20
   Line
     16      75  78  81  84  87
     17      77  80  83  86  89
     18      79  82  85  88  91
     19      81  84  87  90  93
     20      83  86  89  92  95
WRITE "TEST WITHOUT THE SIZE SPECIFIED"
TEST WITHOUT THE SIZE SPECIFIED
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    1.0000    1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list  D (1,1,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:48 2004
     Samp     1       3       5
   Line
      1       0   2   4   6   8
      2       2   4   6   8  10
      3       4   6   8  10  12
      4       6   8  10  12  14
      5       8  10  12  14  16
WRITE "TEST THE HALFWORD OUTPUT"
TEST THE HALFWORD OUTPUT
xform (AX,BX,CX) D 'HALF MATR=(1.,2.,1.) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    2.0000    1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list  D (1,1,5,10) 'HALF
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:48 2004
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     3     6     9    12    15    18    21    24    27
      2         2     5     8    11    14    17    20    23    26    29
      3         4     7    10    13    16    19    22    25    28    31
      4         6     9    12    15    18    21    24    27    30    33
      5         8    11    14    17    20    23    26    29    32    35
WRITE "TEST THE AUTO-SCALE MODE"
TEST THE AUTO-SCALE MODE
WRITE "TEST THE BYTE OUTPUT"
TEST THE BYTE OUTPUT
WRITE "TEST THE PARM LINC AND PERC, AND MULTIPLE OUTPUTS"
TEST THE PARM LINC AND PERC, AND MULTIPLE OUTPUTS
xform (AX,BX,CX) (D,E)  'BYTE MATR=(1.,2.,1.,2.,1.,2.)  +
 LINC=5 PERC=12.
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 2, 3) TRANSFORMATION MATRIX

     1.0000    2.0000    1.0000
     2.0000    1.0000    2.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 12.0
     AREA SAMPLED (    1,    1,   40,   40)   LINC=  5   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -52.306    1.923
    -51.948    1.396
list  D (1,1,15,15)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:49 2004
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   5  11  17  23  28
      2       0   0   0   0   0   0   0   0   0   3   9  15  21  27  32
      3       0   0   0   0   0   0   0   0   2   7  13  19  25  30  36
      4       0   0   0   0   0   0   0   0   5  11  17  23  28  34  40
      5       0   0   0   0   0   0   0   3   9  15  21  27  32  38  44
      6       0   0   0   0   0   0   2   7  13  19  25  30  36  42  48
      7       0   0   0   0   0   0   5  11  17  23  28  34  40  46  52
      8       0   0   0   0   0   3   9  15  21  27  32  38  44  50  55
      9       0   0   0   0   2   7  13  19  25  30  36  42  48  53  59
     10       0   0   0   0   5  11  17  23  28  34  40  46  52  57  63
     11       0   0   0   3   9  15  21  27  32  38  44  50  55  61  67
     12       0   0   2   7  13  19  25  30  36  42  48  53  59  65  71
     13       0   0   5  11  17  23  28  34  40  46  52  57  63  69  75
     14       0   3   9  15  21  27  32  38  44  50  55  61  67  73  78
     15       2   7  13  19  25  30  36  42  48  53  59  65  71  77  82
list  E (1,1,15,15)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:49 2004
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   3   7
      2       0   0   0   0   0   0   0   0   0   0   0   0   4   8  12
      3       0   0   0   0   0   0   0   0   0   0   1   5   9  14  18
      4       0   0   0   0   0   0   0   0   0   3   7  11  15  19  23
      5       0   0   0   0   0   0   0   0   4   8  12  16  21  25  29
      6       0   0   0   0   0   0   1   5   9  14  18  22  26  30  35
      7       0   0   0   0   0   3   7  11  15  19  23  28  32  36  40
      8       0   0   0   0   4   8  12  16  21  25  29  33  37  42  46
      9       0   0   1   5   9  14  18  22  26  30  35  39  43  47  51
     10       0   3   7  11  15  19  23  28  32  36  40  44  49  53  57
     11       4   8  12  16  21  25  29  33  37  42  46  50  54  58  63
     12       9  14  18  22  26  30  35  39  43  47  51  56  60  64  68
     13      15  19  23  28  32  36  40  44  49  53  57  61  65  70  74
     14      21  25  29  33  37  42  46  50  54  58  63  67  71  75  79
     15      26  30  35  39  43  47  51  56  60  64  68  72  77  81  85
WRITE "TEST THE PARM LPER AND HPER"
TEST THE PARM LPER AND HPER
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.)  +
 LPER=1. HPER=3.
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    1.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  4.0
     AREA SAMPLED (    1,    1,   40,   40)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -18.044    1.969
list  D (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:49 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   2   6  10  13  17
      2       0   0   0   0   2   6  10  13  17  21
      3       0   0   0   2   6  10  13  17  21  25
      4       0   0   2   6  10  13  17  21  25  29
      5       0   2   6  10  13  17  21  25  29  33
      6       2   6  10  13  17  21  25  29  33  37
      7       6  10  13  17  21  25  29  33  37  41
      8      10  13  17  21  25  29  33  37  41  45
      9      13  17  21  25  29  33  37  41  45  49
     10      17  21  25  29  33  37  41  45  49  53
list  D (31,31,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:49 2004
     Samp    31      33      35      37      39
   Line
     31     218 222 226 230 234 238 242 246 250 254
     32     222 226 230 234 238 242 246 250 254 255
     33     226 230 234 238 242 246 250 254 255 255
     34     230 234 238 242 246 250 254 255 255 255
     35     234 238 242 246 250 254 255 255 255 255
     36     238 242 246 250 254 255 255 255 255 255
     37     242 246 250 254 255 255 255 255 255 255
     38     246 250 254 255 255 255 255 255 255 255
     39     250 254 255 255 255 255 255 255 255 255
     40     254 255 255 255 255 255 255 255 255 255
WRITE "TEST THE PARM SPREAD"
TEST THE PARM SPREAD
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.)  +
 SPRE=128. PERC=1.
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    1.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   128.0   Center    64.0   Percent Sat.  1.0
     AREA SAMPLED (    1,    1,   40,   40)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
     -4.855    0.883
list  D (1,1,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:50 2004
     Samp     1       3       5
   Line
      1       0   0   0   0   2
      2       0   0   0   2   4
      3       0   0   2   4   6
      4       0   2   4   6   8
      5       2   4   6   8   9
list  D (36,36,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:50 2004
     Samp    36      38      40
   Line
     36     119 120 122 124 126
     37     120 122 124 126 128
     38     122 124 126 128 129
     39     124 126 128 129 131
     40     126 128 129 131 133
WRITE "TEST THE PARM CENTER & firm"
TEST THE PARM CENTER & firm
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.)  +
 CENT=100 PERC=1 SPREAD=200 'FIRM
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    1.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   200.0   Center   100.0   Percent Sat.  1.0
     AREA SAMPLED (    1,    1,   40,   40)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
     -7.586    1.379
list  D (1,1,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:50 2004
     Samp     1       3       5
   Line
      1       0   0   0   1   3
      2       0   0   1   3   6
      3       0   1   3   6   9
      4       1   3   6   9  12
      5       3   6   9  12  14
list  D (36,36,5,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:50 2004
     Samp    36      38      40
   Line
     36     186 188 191 194 197
     37     188 191 194 197 199
     38     191 194 197 199 200
     39     194 197 199 200 200
     40     197 199 200 200 200
WRITE "TEST THE PARM AREA"
TEST THE PARM AREA
xform (AX,BX,CX) D 'BYTE MATR=(1.,1.,1.)  +
 AREA=(11,11,20,20) PERC=12.
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    1.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 12.0
     AREA SAMPLED (   11,   11,   20,   20)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
   -246.762    4.798
list  D (11,11,20,20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:51 2004
     Samp    11      13      15      17      19      21      23      25      27      29
   Line
     11       0   0   0   0   0   0   3  12  22  32  41  51  60  70  80  89  99 108 118 127
     12       0   0   0   0   0   3  12  22  32  41  51  60  70  80  89  99 108 118 127 137
     13       0   0   0   0   3  12  22  32  41  51  60  70  80  89  99 108 118 127 137 147
     14       0   0   0   3  12  22  32  41  51  60  70  80  89  99 108 118 127 137 147 156
     15       0   0   3  12  22  32  41  51  60  70  80  89  99 108 118 127 137 147 156 166
     16       0   3  12  22  32  41  51  60  70  80  89  99 108 118 127 137 147 156 166 175
     17       3  12  22  32  41  51  60  70  80  89  99 108 118 127 137 147 156 166 175 185
     18      12  22  32  41  51  60  70  80  89  99 108 118 127 137 147 156 166 175 185 195
     19      22  32  41  51  60  70  80  89  99 108 118 127 137 147 156 166 175 185 195 204
     20      32  41  51  60  70  80  89  99 108 118 127 137 147 156 166 175 185 195 204 214
     21      41  51  60  70  80  89  99 108 118 127 137 147 156 166 175 185 195 204 214 223
     22      51  60  70  80  89  99 108 118 127 137 147 156 166 175 185 195 204 214 223 233
     23      60  70  80  89  99 108 118 127 137 147 156 166 175 185 195 204 214 223 233 243
     24      70  80  89  99 108 118 127 137 147 156 166 175 185 195 204 214 223 233 243 252
     25      80  89  99 108 118 127 137 147 156 166 175 185 195 204 214 223 233 243 252 255
     26      89  99 108 118 127 137 147 156 166 175 185 195 204 214 223 233 243 252 255 255
     27      99 108 118 127 137 147 156 166 175 185 195 204 214 223 233 243 252 255 255 255
     28     108 118 127 137 147 156 166 175 185 195 204 214 223 233 243 252 255 255 255 255
     29     118 127 137 147 156 166 175 185 195 204 214 223 233 243 252 255 255 255 255 255
     30     127 137 147 156 166 175 185 195 204 214 223 233 243 252 255 255 255 255 255 255
WRITE "TEST THE HALFWORD OUTPUT"
TEST THE HALFWORD OUTPUT
xform (AX,BX,CX) D 'HALF  MATR=(1.,1.,1.) LINC=3 PERC=12.
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    1.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread 65535.0   Center     0.0   Percent Sat. 12.0
     AREA SAMPLED (    1,    1,   40,   40)   LINC=  3   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
 -47087.289  603.683
list  D (1,1,5,10) 'HALF
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:47 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:51 2004
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
      2    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
      3    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32768
      4    -32768-32768-32768-32768-32768-32768-32768-32768-32768-32599
      5    -32768-32768-32768-32768-32768-32768-32768-32768-32599-31392
WRITE "TEST THE BYTE OUTPUT WITH MORE THAN ONE OUTPUT"
TEST THE BYTE OUTPUT WITH MORE THAN ONE OUTPUT
WRITE "  "
  
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
THE RESULT OF THIS TEST SHOULD BE ALL ZERO
copy AX BX
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
xform (AX,BX) (G,GG) 'BYTE MATR=(1.,-1.,-1.,1.) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 2, 2) TRANSFORMATION MATRIX

     1.0000   -1.0000
    -1.0000    1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
      0.000    1.000
list  G
Beginning VICAR task list
 ** The specified window is all zero.
list  GG
Beginning VICAR task list
 ** The specified window is all zero.
WRITE "TEST THE MSS BYTE OUTPUT"
TEST THE MSS BYTE OUTPUT
mss (AX,BX) MSS
Beginning VICAR task mss
* OUTPUT CONTAINS   2INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH     80SAMPLES **
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
THE RESULT OF THIS TEST SHOULD BE ALL ZERO
xform MSS D 'BYTE MSS=2 MATR=(1,-1) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   40 ***

     ( 1, 2) TRANSFORMATION MATRIX

     1.0000   -1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list  D
Beginning VICAR task list
 ** The specified window is all zero.
WRITE "TEST THE MSS SPECIFIED PARM USE"
TEST THE MSS SPECIFIED PARM USE
mss (AX,CX,BX) MSS
Beginning VICAR task mss
* OUTPUT CONTAINS   3INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH    120SAMPLES **
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
THE RESULT OF THIS TEST SHOULD BE ALL ZERO
xform MSS D 'BYTE MSS=3 USE=(1,3) MATR=(1,-1) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   40 ***

     ( 1, 2) TRANSFORMATION MATRIX

     1.0000   -1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list  D
Beginning VICAR task list
 ** The specified window is all zero.
WRITE "TEST THE MSS HALFWORD OUTPUT"
TEST THE MSS HALFWORD OUTPUT
gen AY 10 20 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen BY 10 20 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
mss (AY,BY) MSS
Beginning VICAR task mss
* OUTPUT CONTAINS   2INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH     40SAMPLES **
WRITE "THE RESULT OF THIS TEST SHOULD BE ALL ZERO"
THE RESULT OF THIS TEST SHOULD BE ALL ZERO
xform MSS D  'HALF MSS=2 MATR=(1,-1) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   20 ***

     ( 1, 2) TRANSFORMATION MATRIX

     1.0000   -1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list  D
Beginning VICAR task list
 ** The specified window is all zero.
write "=============================================="
==============================================
WRITE "TEST THE AREA PARAMETER WITH ONE OR MORE AREAS"
TEST THE AREA PARAMETER WITH ONE OR MORE AREAS
gen AX 10 10 LINC=10 SINC=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen BX 10 10 LINC=10 SINC=1 IVAL=100
Beginning VICAR task gen
GEN Version 6
GEN task completed
MSS (AX,BX) MSSB1
Beginning VICAR task MSS
* OUTPUT CONTAINS   2INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH     20SAMPLES **
list  AX
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2      10  11  12  13  14  15  16  17  18  19
      3      20  21  22  23  24  25  26  27  28  29
      4      30  31  32  33  34  35  36  37  38  39
      5      40  41  42  43  44  45  46  47  48  49
      6      50  51  52  53  54  55  56  57  58  59
      7      60  61  62  63  64  65  66  67  68  69
      8      70  71  72  73  74  75  76  77  78  79
      9      80  81  82  83  84  85  86  87  88  89
     10      90  91  92  93  94  95  96  97  98  99
WRITE "TRY AX AS A SINGLE FILE AND AS A MSS FILE WITH ONE BAND."
TRY AX AS A SINGLE FILE AND AS A MSS FILE WITH ONE BAND.
WRITE "SHOULD GET SAME RESULTS EITHER WAY. "
SHOULD GET SAME RESULTS EITHER WAY. 
WRITE "OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255."
OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255.
xform AX OUT=XFORM0 MATR=1. PERC=0. LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
      1.275    2.550
list  XFORM0 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:56 2004
     Samp     1       3       5       7       9
   Line
      1       1   4   6   9  11  14  17  19  22  24
      2      27  29  32  34  37  40  42  45  47  50
      3      52  55  57  60  62  65  68  70  73  75
      4      78  80  83  85  88  91  93  96  98 101
      5     103 106 108 111 113 116 119 121 124 126
      6     129 131 134 136 139 142 144 147 149 152
      7     154 157 159 162 164 167 170 172 175 177
      8     180 182 185 187 190 193 195 198 200 203
      9     205 208 210 213 215 218 221 223 226 228
     10     231 233 236 238 241 244 246 249 251 254
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
      1.275    2.550
list  XFORM0 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:56 2004
     Samp     1       3       5       7       9
   Line
      1       1   4   6   9  11  14  17  19  22  24
      2      27  29  32  34  37  40  42  45  47  50
      3      52  55  57  60  62  65  68  70  73  75
      4      78  80  83  85  88  91  93  96  98 101
      5     103 106 108 111 113 116 119 121 124 126
      6     129 131 134 136 139 142 144 147 149 152
      7     154 157 159 162 164 167 170 172 175 177
      8     180 182 185 187 190 193 195 198 200 203
      9     205 208 210 213 215 218 221 223 226 228
     10     231 233 236 238 241 244 246 249 251 254
WRITE "TRY SIZE FIELD TO GET 7 BY 8 OUTPUT"
TRY SIZE FIELD TO GET 7 BY 8 OUTPUT
WRITE "OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255."
OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255.
xform AX OUT=XFORM0 MATR=1. PERC=0. SIZE=(3,2,7,8) LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    7,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -76.875    3.750
list  XFORM0 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:57 2004
     Samp     1       3       5       7
   Line
      1       2   6   9  13  17  21  24  28
      2      39  43  47  51  54  58  62  66
      3      77  81  84  88  92  96  99 103
      4     114 118 122 126 129 133 137 141
      5     152 156 159 163 167 171 174 178
      6     189 193 197 201 204 208 212 216
      7     227 231 234 238 242 246 249 253
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. SIZE=(3,2,7,8) LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =    8 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    7,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -76.875    3.750
list  XFORM0 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:57 2004
     Samp     1       3       5       7
   Line
      1       2   6   9  13  17  21  24  28
      2      39  43  47  51  54  58  62  66
      3      77  81  84  88  92  96  99 103
      4     114 118 122 126 129 133 137 141
      5     152 156 159 163 167 171 174 178
      6     189 193 197 201 204 208 212 216
      7     227 231 234 238 242 246 249 253
WRITE "TRY AREA"
TRY AREA
WRITE "OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255 IN THE SPECIFIED AREA."
OUTPUT IS STRETCHED ROUGHLY FROM 0 TO 255 IN THE SPECIFIED AREA.
WRITE "OUTPUT IS SATURATED OUTSIDE THE AREA."
OUTPUT IS SATURATED OUTSIDE THE AREA.
xform AX OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,7,8)
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    7,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -76.875    3.750
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:58 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   2   6   9  13  17  21  24  28  32
      4      36  39  43  47  51  54  58  62  66  69
      5      73  77  81  84  88  92  96  99 103 107
      6     111 114 118 122 126 129 133 137 141 144
      7     148 152 156 159 163 167 171 174 178 182
      8     186 189 193 197 201 204 208 212 216 219
      9     223 227 231 234 238 242 246 249 253 255
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,7,8)
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    7,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -76.875    3.750
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:58 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   2   6   9  13  17  21  24  28  32
      4      36  39  43  47  51  54  58  62  66  69
      5      73  77  81  84  88  92  96  99 103 107
      6     111 114 118 122 126 129 133 137 141 144
      7     148 152 156 159 163 167 171 174 178 182
      8     186 189 193 197 201 204 208 212 216 219
      9     223 227 231 234 238 242 246 249 253 255
     10     255 255 255 255 255 255 255 255 255 255
xform AX OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8)
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    1,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
   -653.438   31.875
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:58 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0  16  48  80 112 143 175 207 239 255
      4     255 255 255 255 255 255 255 255 255 255
      5     255 255 255 255 255 255 255 255 255 255
      6     255 255 255 255 255 255 255 255 255 255
      7     255 255 255 255 255 255 255 255 255 255
      8     255 255 255 255 255 255 255 255 255 255
      9     255 255 255 255 255 255 255 255 255 255
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8)
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    1,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
   -653.438   31.875
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:59 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0  16  48  80 112 143 175 207 239 255
      4     255 255 255 255 255 255 255 255 255 255
      5     255 255 255 255 255 255 255 255 255 255
      6     255 255 255 255 255 255 255 255 255 255
      7     255 255 255 255 255 255 255 255 255 255
      8     255 255 255 255 255 255 255 255 255 255
      9     255 255 255 255 255 255 255 255 255 255
     10     255 255 255 255 255 255 255 255 255 255
xform AX OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8  7,2,1,8)
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    1,    8)   LINC=  1   SINC=  1
     AREA SAMPLED (    7,    2,    1,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
   -108.906    5.312
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:59 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   3   8  13  19  24  29  35  40  45
      4      50  56  61  66  72  77  82  88  93  98
      5     104 109 114 120 125 130 135 141 146 151
      6     157 162 167 173 178 183 189 194 199 205
      7     210 215 220 226 231 236 242 247 252 255
      8     255 255 255 255 255 255 255 255 255 255
      9     255 255 255 255 255 255 255 255 255 255
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=0. AREA=(3,2,1,8  7,2,1,8)
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    3,    2,    1,    8)   LINC=  1   SINC=  1
     AREA SAMPLED (    7,    2,    1,    8)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
   -108.906    5.312
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:46:59 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   3   8  13  19  24  29  35  40  45
      4      50  56  61  66  72  77  82  88  93  98
      5     104 109 114 120 125 130 135 141 146 151
      6     157 162 167 173 178 183 189 194 199 205
      7     210 215 220 226 231 236 242 247 252 255
      8     255 255 255 255 255 255 255 255 255 255
      9     255 255 255 255 255 255 255 255 255 255
     10     255 255 255 255 255 255 255 255 255 255
WRITE "THE FOLLOWING GROUP WITH PERC=20. SHOULD ALL YIELD THE SAME OUTPUT"
THE FOLLOWING GROUP WITH PERC=20. SHOULD ALL YIELD THE SAME OUTPUT
WRITE "THE TOP AND BOTTOM LINES SHOULD BE SATURATED."
THE TOP AND BOTTOM LINES SHOULD BE SATURATED.
xform AX OUT=XFORM0 MATR=1. PERC=20. LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:00 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:00 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX OUT=XFORM0 MATR=1. PERC=20. SIZE=(1,1,10,10) LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:01 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. SIZE=(1,1,10,10) LINC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:01 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,10)
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:01 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,10)
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:02 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,2  1,3,10,6  1,9,10,2)
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,    2)   LINC=  1   SINC=  1
     AREA SAMPLED (    1,    3,   10,    6)   LINC=  1   SINC=  1
     AREA SAMPLED (    1,    9,   10,    2)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:02 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,10,2  1,3,10,6  1,9,10,2)
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,   10,    2)   LINC=  1   SINC=  1
     AREA SAMPLED (    1,    3,   10,    6)   LINC=  1   SINC=  1
     AREA SAMPLED (    1,    9,   10,    2)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:02 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,2,10  3,1,6,10  9,1,2,10)
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,    2,   10)   LINC=  1   SINC=  1
     AREA SAMPLED (    3,    1,    6,   10)   LINC=  1   SINC=  1
     AREA SAMPLED (    9,    1,    2,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:03 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
xform AX MSS=1 OUT=XFORM0 MATR=1. PERC=20. AREA=(1,1,2,10  3,1,6,10  9,1,2,10)
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 20.0
     AREA SAMPLED (    1,    1,    2,   10)   LINC=  1   SINC=  1
     AREA SAMPLED (    3,    1,    6,   10)   LINC=  1   SINC=  1
     AREA SAMPLED (    9,    1,    2,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -28.333    3.148
list  XFORM0 'NOEJECT 'ZERO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:03 2004
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       3   6   9  13  16  19  22  25  28  31
      3      35  38  41  44  47  50  54  57  60  63
      4      66  69  72  76  79  82  85  88  91  94
      5      98 101 104 107 110 113 116 120 123 126
      6     129 132 135 139 142 145 148 151 154 157
      7     161 164 167 170 173 176 179 183 186 189
      8     192 195 198 201 205 208 211 214 217 220
      9     224 227 230 233 236 239 242 246 249 252
     10     255 255 255 255 255 255 255 255 255 255
WRITE "OUTPUT SHOULD BE STRETCHED WITH 0 PERCENT SATURATION"
OUTPUT SHOULD BE STRETCHED WITH 0 PERCENT SATURATION
xform MSSB1 (X,Y) MSS=2 MATR=(1.,0., 0.,1.) linc=1 perc=0
Beginning VICAR task xform
XFORM version 11-JAN-2004
 *** Output  NS =   10 ***

     ( 2, 2) TRANSFORMATION MATRIX

     1.0000    0.0000
     0.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
      1.275    2.550
   -253.725    2.550
list  X
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:04 2004
     Samp     1       3       5       7       9
   Line
      1       1   4   6   9  11  14  17  19  22  24
      2      27  29  32  34  37  40  42  45  47  50
      3      52  55  57  60  62  65  68  70  73  75
      4      78  80  83  85  88  91  93  96  98 101
      5     103 106 108 111 113 116 119 121 124 126
      6     129 131 134 136 139 142 144 147 149 152
      7     154 157 159 162 164 167 170 172 175 177
      8     180 182 185 187 190 193 195 198 200 203
      9     205 208 210 213 215 218 221 223 226 228
     10     231 233 236 238 241 244 246 249 251 254
list  Y
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:46:55 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:04 2004
     Samp     1       3       5       7       9
   Line
      1       1   4   6   9  11  14  17  19  22  24
      2      27  29  32  34  37  40  42  45  47  50
      3      52  55  57  60  62  65  68  70  73  75
      4      78  80  83  85  88  91  93  96  98 101
      5     103 106 108 111 113 116 119 121 124 126
      6     129 131 134 136 139 142 144 147 149 152
      7     154 157 159 162 164 167 170 172 175 177
      8     180 182 185 187 190 193 195 198 200 203
      9     205 208 210 213 215 218 221 223 226 228
     10     231 233 236 238 241 244 246 249 251 254
xform (AX BX) (X1,Y1) MATR=(1.,0., 0.,1.) linc=1 perc=0
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 2, 2) TRANSFORMATION MATRIX

     1.0000    0.0000
     0.0000    1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat.  0.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
      1.275    2.550
   -253.725    2.550
WRITE "SHOULD GET 0 DIFFERENCES."
SHOULD GET 0 DIFFERENCES.
difpic (X X1)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
difpic (Y Y1)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
gen A0 10 10 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen A 10 10 ival=100
Beginning VICAR task gen
GEN Version 6
GEN task completed
fastmos (A A0) A1 SIZE=(1,1, 100,100)
Beginning VICAR task fastmos
list A1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:05 2004
 Task:FASTMOS   User:lwk       Date_Time:Wed Feb 11 11:47:05 2004
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1     100 101 102 103 104 105 106 107 108 109   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2     101 102 103 104 105 106 107 108 109 110   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3     102 103 104 105 106 107 108 109 110 111   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4     103 104 105 106 107 108 109 110 111 112   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5     104 105 106 107 108 109 110 111 112 113   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6     105 106 107 108 109 110 111 112 113 114   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      7     106 107 108 109 110 111 112 113 114 115   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      8     107 108 109 110 111 112 113 114 115 116   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      9     108 109 110 111 112 113 114 115 116 117   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     10     109 110 111 112 113 114 115 116 117 118   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
label-list A1
Beginning VICAR task label
************************************************************
 
        ************  File A1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                100 lines per band
                100 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Wed Feb 11 11:47:05 2004 ----
IVAL=100.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: FASTMOS -- User: lwk -- Wed Feb 11 11:47:05 2004 ----
 
************************************************************
xform A1 OUT=B1 MATR=1. PERC=10. INC=1 EXCLUDE=0
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 10.0
     AREA SAMPLED (    1,    1,  100,  100)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
  -1767.445   17.385
xform A OUT=B MATR=1. PERC=10. INC=1
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 10.0
     AREA SAMPLED (    1,    1,   10,   10)   LINC=  1   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
  -1767.445   17.385
difpic (B B1)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
xform A OUT=B 'real MATR=1. 'preset
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list B
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:05 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:06 2004
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.000E+02   1.010E+02   1.020E+02   1.030E+02   1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02   1.090E+02
      2       1.010E+02   1.020E+02   1.030E+02   1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02   1.090E+02   1.100E+02
      3       1.020E+02   1.030E+02   1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02   1.090E+02   1.100E+02   1.110E+02
      4       1.030E+02   1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02   1.090E+02   1.100E+02   1.110E+02   1.120E+02
      5       1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02   1.090E+02   1.100E+02   1.110E+02   1.120E+02   1.130E+02
      6       1.050E+02   1.060E+02   1.070E+02   1.080E+02   1.090E+02   1.100E+02   1.110E+02   1.120E+02   1.130E+02   1.140E+02
      7       1.060E+02   1.070E+02   1.080E+02   1.090E+02   1.100E+02   1.110E+02   1.120E+02   1.130E+02   1.140E+02   1.150E+02
      8       1.070E+02   1.080E+02   1.090E+02   1.100E+02   1.110E+02   1.120E+02   1.130E+02   1.140E+02   1.150E+02   1.160E+02
      9       1.080E+02   1.090E+02   1.100E+02   1.110E+02   1.120E+02   1.130E+02   1.140E+02   1.150E+02   1.160E+02   1.170E+02
     10       1.090E+02   1.100E+02   1.110E+02   1.120E+02   1.130E+02   1.140E+02   1.150E+02   1.160E+02   1.170E+02   1.180E+02
xform A OUT=B 'FULL MATR=1. 'preset
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 1) TRANSFORMATION MATRIX

     1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
list B
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:05 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:07 2004
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1            100        101        102        103        104        105        106        107        108        109
      2            101        102        103        104        105        106        107        108        109        110
      3            102        103        104        105        106        107        108        109        110        111
      4            103        104        105        106        107        108        109        110        111        112
      5            104        105        106        107        108        109        110        111        112        113
      6            105        106        107        108        109        110        111        112        113        114
      7            106        107        108        109        110        111        112        113        114        115
      8            107        108        109        110        111        112        113        114        115        116
      9            108        109        110        111        112        113        114        115        116        117
     10            109        110        111        112        113        114        115        116        117        118
gen A 40 40 3
Beginning VICAR task gen
GEN Version 6
GEN task completed
xform A B (1,1,20,20) 'BYTE  +
 MATR=(1.,2.,1.) 'PRES
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 1, 3) TRANSFORMATION MATRIX

     1.0000    2.0000    1.0000

 *** SCALING FACTORS ***
     OFFSET    GAIN
      0.000    1.000
label-list B
Beginning VICAR task label
************************************************************
 
        ************  File B ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Wed Feb 11 11:47:07 2004 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: XFORM -- User: lwk -- Wed Feb 11 11:47:07 2004 ----
 
************************************************************
list  B (1,1,5,5) nb=1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:07 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:07 2004
     Samp     1       3       5
   Line
      1       4   8  12  16  20
      2       8  12  16  20  24
      3      12  16  20  24  28
      4      16  20  24  28  32
      5      20  24  28  32  36
list  B (16,16,5,5) nb=1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:07 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:07 2004
     Samp    16      18      20
   Line
     16     124 128 132 136 140
     17     128 132 136 140 144
     18     132 136 140 144 148
     19     136 140 144 148 152
     20     140 144 148 152 156
list  B (1,1,5,5) nb=1 sb=3
Beginning VICAR task list
 ** Requested area exceeds size of input picture.
 ** Number of bands printed reduced.
 ** Specified window contains no data.
list  B (16,16,5,5) nb=1 sb=3
Beginning VICAR task list
 ** Requested area exceeds size of input picture.
 ** Number of bands printed reduced.
 ** Specified window contains no data.
gen AX 40 40
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen BX 40 40 LINC=0 SINC=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen CX 40 40 LINC=1 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
xform (AX,BX,CX) D 'BYTE MATR=(1.,2.,1.,2.,1.,2.)  +
 LINC=5 PERC=12.
Beginning VICAR task xform
XFORM version 11-JAN-2004

     ( 2, 3) TRANSFORMATION MATRIX

     1.0000    2.0000    1.0000
     2.0000    1.0000    2.0000

 *** AUTO-SCALE MODE ***
     Desired Spread   255.0   Center   127.5   Percent Sat. 12.0
     AREA SAMPLED (    1,    1,   40,   40)   LINC=  5   SINC=  1
 *** SCALING FACTORS ***
     OFFSET    GAIN
    -52.306    1.923
    -51.948    1.396
label-list D
Beginning VICAR task label
************************************************************
 
        ************  File D ************
                3 dimensional IMAGE file
                File organization is BIL
                Pixels are in BYTE format from a SUN-SOLR host
                2 bands
                40 lines per band
                40 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Wed Feb 11 11:47:08 2004 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: XFORM -- User: lwk -- Wed Feb 11 11:47:09 2004 ----
SUBAREA1='    1,    1,   40,   40)   LINC=  5   SINC=  1'
 
************************************************************
list  D (1,1,15,15) 'bsq
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:08 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:09 2004
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   5  11  17  23  28
      2       0   0   0   0   0   0   0   0   0   3   9  15  21  27  32
      3       0   0   0   0   0   0   0   0   2   7  13  19  25  30  36
      4       0   0   0   0   0   0   0   0   5  11  17  23  28  34  40
      5       0   0   0   0   0   0   0   3   9  15  21  27  32  38  44
      6       0   0   0   0   0   0   2   7  13  19  25  30  36  42  48
      7       0   0   0   0   0   0   5  11  17  23  28  34  40  46  52
      8       0   0   0   0   0   3   9  15  21  27  32  38  44  50  55
      9       0   0   0   0   2   7  13  19  25  30  36  42  48  53  59
     10       0   0   0   0   5  11  17  23  28  34  40  46  52  57  63
     11       0   0   0   3   9  15  21  27  32  38  44  50  55  61  67
     12       0   0   2   7  13  19  25  30  36  42  48  53  59  65  71
     13       0   0   5  11  17  23  28  34  40  46  52  57  63  69  75
     14       0   3   9  15  21  27  32  38  44  50  55  61  67  73  78
     15       2   7  13  19  25  30  36  42  48  53  59  65  71  77  82


 Task:GEN       User:lwk       Date_Time:Wed Feb 11 11:47:08 2004
 Task:XFORM     User:lwk       Date_Time:Wed Feb 11 11:47:09 2004
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   3   7
      2       0   0   0   0   0   0   0   0   0   0   0   0   4   8  12
      3       0   0   0   0   0   0   0   0   0   0   1   5   9  14  18
      4       0   0   0   0   0   0   0   0   0   3   7  11  15  19  23
      5       0   0   0   0   0   0   0   0   4   8  12  16  21  25  29
      6       0   0   0   0   0   0   1   5   9  14  18  22  26  30  35
      7       0   0   0   0   0   3   7  11  15  19  23  28  32  36  40
      8       0   0   0   0   4   8  12  16  21  25  29  33  37  42  46
      9       0   0   1   5   9  14  18  22  26  30  35  39  43  47  51
     10       0   3   7  11  15  19  23  28  32  36  40  44  49  53  57
     11       4   8  12  16  21  25  29  33  37  42  46  50  54  58  63
     12       9  14  18  22  26  30  35  39  43  47  51  56  60  64  68
     13      15  19  23  28  32  36  40  44  49  53  57  61  65  70  74
     14      21  25  29  33  37  42  46  50  54  58  63  67  71  75  79
     15      26  30  35  39  43  47  51  56  60  64  68  72  77  81  85
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
