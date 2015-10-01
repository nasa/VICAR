$!****************************************************************************
$!
$! Build proc for MIPL module xform
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:40:33
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
$ write sys$output "Invalid argument given to xform.com file -- ", primary
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
	-i xform.imake
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
	CALL XVMESSAGE('XFORM version 8-JAN-2004',' ')
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
            IF (ORG .EQ. 'BIP') THEN
		CALL XVMESSAGE('BIP format is not supported',' ')
		CALL ABEND
	    ENDIF
	END DO
	CALL XVSIZE(ISL,ISS,NL,NS,NLI,NSI)
	CALL XVGET(INUNIT(1),ISTAT,'FORMAT',FORMAT,'NB',NB,' ')
	IF(ISL+NL-1.GT.NLI) THEN
	    CALL XVMESSAGE(
     +		' Number of lines requested exceeds input size',' ')
	    CALL ABEND
	ENDIF
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
	    CALL XVMESSAGE(BUF,' ')
	    CALL ABEND
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
	    IF (NIMSS*NSICHN.NE.NSI) THEN
		CALL XVMESSAGE(
     + ' number of input samples is not consistent with number of bands'
     +  ,' ')
		CALL ABEND
	    ENDIF
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
	    IF(ISS+NSO-1.GT.NSI) THEN
		CALL XVMESSAGE(
     +           ' Number of samples specified exceeds input size',' ')
		CALL ABEND
	    ENDIF
	END IF
C									usebands
	CALL XVPARM('USEBANDS',IPARM,ICOUNT,IDEF,NDIM)
	IF(ICOUNT.NE.0) THEN
	    MSSSUB = .TRUE.
	    NI = ICOUNT
 	    IF(NI.GT.NB) THEN
		CALL XVMESSAGE(' Inconsistent MSS and USE parameters',
     +					' ')
		CALL ABEND
	    ENDIF
	    DO J=1,NI
		IBND(J) = IPARM(J)
	    END DO
	ELSE
	    NI = NB
	END IF
C									area
	CALL XVPARM('AREA',IAREA,ICOUNT,IDEF,0)
	IF (MOD(ICOUNT,4) .NE. 0) THEN
	    CALL XVMESSAGE(
     +			' Invalid number of area parameter values',' ')
	    CALL ABEND
	ENDIF
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
	IF (NI*NO.NE.ICOUNT) THEN
	    CALL XVMESSAGE(' Invalid number of matrix elements',' ')
	    CALL ABEND
	ENDIF
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
  300	FORMAT('     (',I2,','I2,') TRANSFORMATION MATRIX')
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
     +		   '   Percent Sat.',F5.1)
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
     +		   ')   LINC=',I3,'   SINC=',I3)
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
	    IF (R.EQ.0.0) THEN
		CALL XVMESSAGE(' Histogram has no spread',' ')
		CALL ABEND
	    ENDIF
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
 XFORM WRITTEN BY:  J.D.Addington,  Jan. 1977

 XFORM CONVERTED TO VAX BY:  F.F.Moss, Jan. 1984

 XFORM WRITTEN BY:  L.W.Kamp,  Sept. 1984

 CONVERTED TO VICAR2 BY:  J.H.Reimer, July 1985

 REWRITTEN IGNORING AP COMPATIBILITY: Ron Alley, June 1986

 CURRENT COGNIZANT PROGRAMMER:  Ron Alley
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
#define LIB_P3SUB
$ Return
$!#############################################################################
