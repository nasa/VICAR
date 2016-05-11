$!****************************************************************************
$!
$! Build proc for MIPL module hist
$! VPACK Version 2.1, Wednesday, September 09, 2015, 16:21:44
$!
$! Execute by entering:		$ @hist
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
$ write sys$output "*** module hist ***"
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
$ write sys$output "Invalid argument given to hist.com file -- ", primary
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
$   if F$SEARCH("hist.imake") .nes. ""
$   then
$      vimake hist
$      purge hist.bld
$   else
$      if F$SEARCH("hist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hist.bld "STD"
$   else
$      @hist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hist.com -mixed -
	-s hist.f -
	-p hist.pdf -
	-i hist.imake -
	-t tsthist.pdf tsthist.log_solos tsthist.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 23 SEPT 1983
C     REVISION 1 BY ASM, FEBRUARY 7 1984 - SPEED ENHANCEMENTS
C         1) REPLACED CONVERSION OF ALL INPUT DATA TO REAL-TYPE WITH
C	     SEPARATE TABULATION ROUTINES FOR EACH TYPE
C         2) ADDED LOOK-UP TABLE FOR BYTE-IMAGE PROCESSING
c  84-10-9  ...LWK...  converted to Vicar2, check for rounding error in sdev.
c  84-10-11 ...LWK...  for byte data, compute stats from histogram.
c  84-12-13 ...LWK...  revised treatment of BINS, LIMITS.
c  85-4-17  ...REA...  fixed bug in LINC & AREA parameters
c  85-4-17  ...LWK...  revised processing of REAL*4 data
C  86-11-11 ...REA...  modify formatting, hist collection routines, add
C		       output parameters MEAN, SIGMA
C  87-1-12  ...REA...  add EXCLUDE, SCREEN parameters
C  87-2-4   ...REA...  add SPLOT, PPLOT and TITLE parameters
C  87-8-4   ...REA...  fix bug in x-axis scaling algorithm
C  87-10-27 ...REA...  add 3-D file capability
C  89-8-23  ...REA...  add BAR graph parameter
C  90-2-22  ...REA...  add output parameters MIN, MAX
C  90-10-5  ...REA...  add laserprinter capabilities
C  91-4-8   ...REA...  convert to UNIX
C  92-8-6   ...REA...  add WINDOW and WBAR options
C  96-10-25 ...REA...  remove Regis options
C  97-8-20  ...REA...  add BYCHAN option
C  00-7-12  ...REA...  adjust print-out column widths for DN and population
C  01-2-16  ...REA...  fix min/max bug when both regular and pen plots;
C                      consolidate statistics logic.
C
C**********************************************************************
      SUBROUTINE MAIN44

C**********************************************************************
C               DECLARATIONS AND INITIALIZATION
C**********************************************************************

      EXTERNAL WORK
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      INTEGER CNT,DEF,BATCH
      INTEGER SL,SS,SLI,SSI,SINC
      REAL*4 BOUNDS(2)
      COMMON/PAUSE/ QPAUSE
      LOGICAL*4 NOCUM,QPAUSE,XVPTST,QEXCLUDE,QPPLOT,QBYCHAN
      CHARACTER*80 MSG
      CHARACTER*8 FMT, FMT1
      CHARACTER*3 ORG


      call xvmessage( '*** p3/HIST version Sep 9 2015 ***',' ')

C********************************************************************
C			OPEN INPUT FILE
C********************************************************************

      CALL XVUNIT(INUN,'INP',1,ISTAT,0)
      CALL XVOPEN(INUN,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',0)
      CALL XVGET(INUN,ISTAT,'ORG',ORG,0)
      IF (ORG.EQ.'BIP') THEN
	  CALL XVMESSAGE(' HIST does not accept BIP input',' ')
	  CALL ABEND
      ENDIF
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      SL = SL-1
      SS = SS-1
      CALL XVBANDS(ISB,NB,NBI)
      IEB = ISB+NB-1
C**********************************************************************
C                       PROCESS PARAMETERS
C**********************************************************************

      QBYCHAN = XVPTST('BYCHAN')
      CALL XVGET(INUN,ISTAT,'FORMAT',FMT,0)
      CALL XVPARM('FORMAT',FMT1,CNT,DEF,1) 
      IF (DEF.EQ.0) FMT = FMT1

      IF (FMT.EQ.'BYTE') THEN
	 IFORM=1
         CALL XVCLOSE(INUN,ISTAT,0)
         CALL XVOPEN(INUN,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +               'U_FORMAT','HALF',0)
      ELSEIF (FMT.EQ.'HALF' .OR. FMT.EQ.'WORD') THEN
	IFORM=2
      ELSEIF (FMT.EQ.'FULL') THEN
	IFORM=4
      ELSEIF (FMT.EQ.'REAL') THEN
	IFORM=7
      ELSE
	CALL XVMESSAGE(' ** FORMAT '//FMT//' NOT SUPPORTED **',' ')
	CALL ABEND
      ENDIF

C ----- CHECK FOR NEW SINC, LINC, INC, OR SPIKE SPECIFICATION

      CALL XVPARM('LINC',LINC,CNT,DEF,1)
      CALL XVPARM('SINC',SINC,CNT,DEF,1)
      CALL XVPARM('INC',INC,CNT,DEF,1)
      IF (DEF.NE.1) THEN
	LINC = INC
	SINC = INC
      ENDIF
C
C DETERMINE PRINT MODE
C
      MODE = 0
      IF (XVPTST('NOHIST')) MODE=-1
      IF (XVPTST('WIDE'))   MODE=1
      IF (XVPTST('SCREEN')) MODE=2
      IF (XVPTST('WINDOW')) MODE=6
      IF (XVPTST('WBAR'))   MODE=7
      QPPLOT = (XVPTST('PPLOT'))
      IF (QPPLOT) THEN	
	CALL XVPARM('MODE',FMT,ICNT,IDEF,1)
	IF (IDEF.EQ.1) MODE=4
      END IF
      IF (MODE.EQ.1) NOCUM = XVPTST('NOCUM')
      IF (MODE.EQ.2 .AND. IFORM.NE.1) THEN
	 CALL XVMESSAGE(
     +	 ' Screen oriented histogram not supported for non-byte data',
     +   ' ')
	 CALL ABEND
      ENDIF
C
C CHECK FOR SPIKES
C
      CALL XVPARM('SPIKES',ISPIKE,CNT,DEF,1)
      IF (ISPIKE.LT.1) ISPIKE = 1
      IF (ISPIKE.GT.9) ISPIKE = 9
C
C NUMBER OF BINS:
C
      CALL XVPARM('BINS',IBINS,CNT,DEF,1)
      IF (IBINS.LT.2) IBINS=2

C NUMBER OF HISTOGRAMS IN DATA SET:

      CALL XVPARM('AREA',IAREA,CNT,DEF,600)
      IF (DEF.EQ.1) THEN
	IAREA(3) = NL
	IAREA(4) = NS
      ENDIF
      NUMARS = CNT/4

C PIXEL RANGE:

      CALL XVPARM('LIMITS',BOUNDS,CNT,DEF,2)
      IF (DEF.EQ.1 .AND. IFORM.NE.1) THEN
          BOUNDS(1) = -32768.
          BOUNDS(2) = 32767.
      END IF
      BOUNDL = BOUNDS(1)
      BOUNDU = BOUNDS(2)
C
C  COMPUTE BIN WIDTHS.  
C
      BINWID = (BOUNDU-BOUNDL)/(IBINS-1)
      IF (IFORM.NE.7 .AND. BINWID.LT.1.0) BINWID=1.0
      BOUNDL = BOUNDL-2.5*BINWID
      IBINS = IBINS+2
C
C EXCLUDE ZEROES?	INTERACTIVE PAUSES?	PEN PLOT?
C
      QEXCLUDE = XVPTST('EXCLUDE')
      QPAUSE = XVPTST('PAUSE') .AND. BATCH().NE.1
C*********************************************************
C			GO TO WORK
C*********************************************************
      M = 4*MAX(NSI,IBINS)
      N = 4*MAX(256,IBINS)
      IF (MODE.EQ.7) THEN
	  M = 4*MAX(NSI,3*IBINS+2)
	  N = 4*MAX(256,3*IBINS+2)
      END IF
      N2 = N
      IF (QBYCHAN) THEN
          ISTART = ISB
          IEND = IEB
          DO I=ISB,IEB
              CALL XVMESSAGE(' ',' ')
              CALL XVMESSAGE(' ',' ')
              CALL XVMESSAGE(' ',' ')
              WRITE (MSG,100) I
  100         FORMAT('---Channel',I4)
              CALL XVMESSAGE(MSG,' ')
              ISB = I
              IEB = I
              CALL STACKA(5,WORK,3,M,N,N2)
          END DO
      ELSE
          CALL STACKA(5,WORK,3,M,N,N2)
      END IF
      RETURN
      END
C**********************************************************************
      SUBROUTINE WORK(BUF,BUFSIZ,HIST,HSTSIZ,RHIST,RHSTSIZ)
C
      INCLUDE 'pgminc'
      COMMON /PARB/PARB
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
 
      INTEGER RHSTSIZ,HSTSIZ,BUFSIZ,MM(2),HIST(*),PARB(xprdim)
      INTEGER NL,NS,SL,SS,LINC,SINC,LINES
      REAL BUF(*),RMM(2),RHIST(*)
      REAL*8 SUM,SUM2,NSUM2
      LOGICAL*4 NOCUM, QEXCLUDE, QPPLOT
C
C**********************************************************************
C GET AREA SPECIFICATION IF ONE WAS GIVEN AND CHECK FOR LEGALITY
C**********************************************************************
      LINES = 0

      DO JAREA=1,NUMARS
	ISL = IAREA(1+(JAREA-1)*4)+SL
	ISS = IAREA(2+(JAREA-1)*4)+SS
	INL = IAREA(3+(JAREA-1)*4)+ISL-1
	INS = IAREA(4+(JAREA-1)*4)+ISS-1
	IF (INL .GT. NLI) THEN
	  CALL XVMESSAGE(
     *    ' DESIRED AREA EXCEEDS INPUT IMAGE SIZE, LINES TRUNCATED',' ')
	  LINES = LINES+1
	  INL = MIN0(NLI,INL)
	ENDIF
	IF (INS .GT. NSI) THEN
	  CALL XVMESSAGE(
     *  ' DESIRED AREA EXCEEDS INPUT IMAGE SIZE, SAMPLES TRUNCATED',' ')
	  LINES = LINES+1
	  INS = MIN0(NSI,INS)
	ENDIF
C
C			CALL APPROPRIATE TABULATING ROUTINE
C
	IF (IFORM.EQ.1) THEN
	  CALL TAB1(BUF,SUM2,HIST,MM,SUM,NZEROES)
	ELSE IF (IFORM.EQ.2) THEN
	  CALL TAB2(BUF,SUM2,HIST,MM,SUM,NZEROES)
	ELSE IF (IFORM.EQ.4) THEN
	  CALL TAB4(BUF,SUM2,HIST,MM,SUM,NZEROES)
	ELSE
	  CALL TAB7(BUF,SUM2,HIST,RMM,SUM,NZEROES)
	END IF
	IF (IFORM.NE.7) THEN
	  RMM(1) = MM(1)
	  RMM(2) = MM(2)
	END IF
	PIXNUM = (IEB-ISB+1)*(1+(INL-ISL)/LINC)*(1+(INS-ISS)/SINC)
	IF (QEXCLUDE) PIXNUM=PIXNUM-NZEROES
C									    mean
        IF (PIXNUM.NE.0.) THEN
            RMEAN = SUM/PIXNUM
        ELSE
            RMEAN = 0.0
        END IF
C							      standard deviation
        NSUM2 = PIXNUM*SUM2-SUM*SUM
        IF (PIXNUM.NE.0 .AND. NSUM2.GT.0) THEN
          SDEV = SQRT(NSUM2)/PIXNUM
        ELSE
          SDEV = 0.0
        END IF
C
C ----- PRINT HISTOGRAM
C
        CALL XVMESSAGE(' ',' ')
	IF (MODE.LE.1) THEN					! normal, wide,
	  CALL PHIST(HIST,RMEAN,SDEV,MM,RMM,PIXNUM)		! or nohist
	  CALL XVMESSAGE(' ',' ')
	ELSE IF (MODE.EQ.2) THEN				! screen
	  CALL SHIST(HIST,RMEAN,SDEV,MM,PIXNUM)
	END IF
	IF (MODE.GT.2 .OR. QPPLOT) THEN				! pplot
	  CALL PLOTXY(HIST,RHIST,BUF,RMEAN,SDEV,RMM,PIXNUM)
	END IF
      END DO
C								send MEAN, SIGMA
C								values to PDF
      CALL XQINI(PARB,xprdim,xcont,ISTAT)
      CALL XQREAL(PARB,'MIN',1,RMM(1),xadd,ISTAT)      
      CALL XQREAL(PARB,'MAX',1,RMM(2),xadd,ISTAT)      
      CALL XQREAL(PARB,'MEAN',1,RMEAN,xadd,ISTAT)      
      CALL XQREAL(PARB,'SIGMA',1,SDEV,xadd,ISTAT)      
      CALL XVQOUT(PARB,ISTAT)
C
      RETURN
      END
C
C**********************************************************************
C     DATA TABULATION ROUTINES FOR ALL DATA TYPES
C
      SUBROUTINE TAB1(IN,SUM2,HOLD,MM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      COMMON /BHIST/ HIST(256)
      LOGICAL*4 NOCUM
      INTEGER*2 IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST,MM(2),HOLD(256)
C
      NZEROES = 0
      CALL ZIA(HIST,256)
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
            HIST(IN(I)+1) = HIST(IN(I)+1) + 1
	  END DO
	END DO
      END DO
      NZEROES = HIST(1)
      SUM = 0.0
      SUM2 = 0.0
      MM(1) = -1
      CALL ZIA(HOLD,IBINS)
      DO I=1,256
	X = I-1
	SUM = SUM + X*DBLE(HIST(I))
	SUM2 = SUM2 + X*X*DBLE(HIST(I))
	IF (HIST(I).GT.0) THEN
	  MM(2) = X
	  IF (MM(1).LT.0) MM(1)=X
	END IF
	K = MIN(MAX1(1.0,(X-BOUNDL)/BINWID),IBINS)
	HOLD(K) = HOLD(K)+HIST(I)
      END DO
      RETURN
      END
C****************************************************************************
      SUBROUTINE TAB2(IN,SUM2,HIST,MM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      LOGICAL*4 NOCUM
      INTEGER*2 IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST(*),MM(2)
      NZEROES = 0
      CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      MINV = 99999
      MAXV = -99999
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
	    ITMP = IN(I)
	    IF (ITMP.LT.MINV) MINV=ITMP
	    IF (ITMP.GT.MAXV) MAXV=ITMP
	    IF (ITMP.EQ.0) NZEROES=NZEROES+1
            SUM = SUM + ITMP
            SUM2 = SUM2 + ITMP*ITMP
	    LOC = MIN(MAX1((ITMP-BOUNDL)/BINWID,1.0),IBINS)
            HIST(LOC) = HIST(LOC) + 1
	  END DO
	END DO
      END DO
      MM(1) = MINV
      MM(2) = MAXV
      RETURN
      END
C******************************************************************************
      SUBROUTINE TAB4(IN,SUM2,HIST,MM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      LOGICAL*4 NOCUM
      INTEGER IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST(*),MM(2)
      NZEROES = 0
      CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      MINV = 999999999
      MAXV = -999999999
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
	    ITMP = IN(I)
	    IF (ITMP.LT.MINV) MINV=ITMP
	    IF (ITMP.GT.MAXV) MAXV=ITMP
	    IF (ITMP.EQ.0) NZEROES=NZEROES+1
            SUM = SUM + ITMP
            SUM2 = SUM2 + FLOAT(ITMP)*FLOAT(ITMP)
	    LOC = MIN(MAX1((ITMP-BOUNDL)/BINWID,1.0),IBINS)
            HIST(LOC) = HIST(LOC) + 1
	  END DO
	END DO
      END DO
      MM(1) = MINV
      MM(2) = MAXV
      RETURN
      END
C****************************************************************************
      SUBROUTINE TAB7(IN,SUM2,HIST,RMM,SUM,NZEROES)
      
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      LOGICAL*4 NOCUM
      REAL RMM(2),IN(*)
      REAL*8 SUM,SUM2
      INTEGER SINC,HIST(*)
      NZEROES = 0
      CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      XMIN = 1.E30
      XMAX = -1.E30
      DO II=ISL,INL,LINC
	DO III=ISB,IEB
	  CALL XVREAD(INUN,IN,ISTAT,'LINE',II,'BAND',III,0)
	  DO I=ISS,INS,SINC
	    TMP = IN(I)
	    IF (TMP.LT.XMIN) XMIN=TMP
	    IF (TMP.GT.XMAX) XMAX=TMP
	    IF (TMP.EQ.0.0) NZEROES=NZEROES+1
            SUM = SUM + TMP
            SUM2 = SUM2 + TMP*TMP
	    LOC = MIN(MAX1((TMP-BOUNDL)/BINWID,1.0),IBINS)
            HIST(LOC) = HIST(LOC) + 1
	  END DO
	END DO
      END DO
      RMM(1) = XMIN
      RMM(2) = XMAX
      RETURN
      END
C**********************************************************************
      SUBROUTINE PHIST(HIST,RMEAN,SDEV,MM,RMM,PIXNUM)
      COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
      REAL*4 RMM(2)
      INTEGER HIST(*),SPIKES(9),SS,SL,NS,NL,SSI,SLI,NSI,NLI
      INTEGER LINC,SINC,LINES,MM(2)
      LOGICAL*4 QEXCLUDE,NOCUM
      CHARACTER*132 PRT
      CHARACTER*101 PLUS/'+         +         +         +         +     
     *    +         +         +         +         +         +'/
      CHARACTER*101 STAR/'**********************************************
     +*******************************************************'/
      CHARACTER*50 BLANK/'                                              
     +    '/
      CHARACTER*30 PRT2
      CHARACTER*11 COL1
      CHARACTER*11 LOWLIMIT  /'< LOW LIMIT'/
      CHARACTER*11 HIGHLIMIT /'>HIGH LIMIT'/
      CHARACTER*8 PRT3
      CHARACTER*6 COL3,PCTILE/'PERCNT'/,CDF/'   CDF'/
      CHARACTER*2 COL5
C
      IF (MODE.LT.0) GO TO 1000
C		 				FIND SPIKE LOCATIONS
      CALL ZIA(SPIKES,9)
      DO J=1,ISPIKE
          MAX=0
          DO I=1,IBINS
              IF(HIST(I) .GT. MAX) IMAX=I
              IF(HIST(I) .GT. MAX) MAX=HIST(I)
          END DO
          IF(MAX .EQ. 0) GO TO 6
          SPIKES(J)=HIST(IMAX)
          HIST(IMAX)=-J
      END DO
    6 IF (MAX.EQ.0 .AND. J.NE.1) MAX=SPIKES(J-1)
      GRAYLEVEL = BOUNDL + 0.5*BINWID
C					             CREATE/PRINT FORMAT HEADERS
      PT = 0.0
c                                                                   WIDE FORMAT
      IF (MODE.EQ.1) THEN
C							     print header lines
C
          WRITE (PRT,100) ISL,ISS,INL-ISL+1,INS-ISS+1,LINC,SINC
  100	  FORMAT(' FREQUENCY DISTRIBUTION     SL=',I5,'     SS=',I5,
     *           '     NL=',I5,'     NS=',I5,'     LINC=',I3,
     *		 '    SINC=',I3)
          CALL IPRNT(PRT,LINES)
          CALL IPRNT(' ',LINES)
C
	  IF (NOCUM) THEN
	      COL3 = PCTILE
	  ELSE
	      COL3 = CDF
	  END IF
          WRITE (PRT,200)  COL3,10,20,30,40,50,60,70,80,90,100
  200	  FORMAT('        GRAY    FREQ  ',A6,'   ',10I10,' ')
          CALL IPRNT(PRT,LINES)
          PRT = '                              ' // PLUS
	  CALL IPRNT(PRT,LINES)
C								    for each bin
	  DO I=1,IBINS
	      GRAYLEVEL = GRAYLEVEL+BINWID
	      IF (HIST(I).EQ.0) THEN
		  IF(I.NE.1.AND.HIST(I-1).NE.0) CALL IPRNT(' ',LINES)
	      ELSE
		  IF (I.EQ.1) THEN
		      COL1 = LOWLIMIT
		  ELSE IF (I.EQ.IBINS) THEN
		      COL1 = HIGHLIMIT
                  ELSE
                      WRITE (COL1,400) GRAYLEVEL
  400                 FORMAT (G11.5)
		  END IF
C		 		   if one of the n=spike largest, label on graph
        	  IF (HIST(I) .LT. 0) THEN
		      J = -HIST(I)
		      HIST(I) = SPIKES(J)
                      WRITE (COL5,500) J
  500                 FORMAT (I2)
                  ELSE
                      COL5 = '  '
		  ENDIF
C						    update number of pixels seen
		  IF (NOCUM) THEN
                      PT = 100.0*HIST(I)/PIXNUM
		  ELSE
		      PT = PT + 100.0*HIST(I)/PIXNUM
		  END IF
C							       draw bar of chart
		  J = (MIN(HIST(I),MAX) * 100) / MAX
		  IF (J.NE.0) THEN
                      WRITE (PRT2,600) COL1,HIST(I),PT
  600                 FORMAT (A11,I8,F8.3,'  ')
                      PRT = PRT2 // STAR(1:J) // PLUS(J+1:101) // COL5
                  ELSE
                      WRITE (PRT,700) COL1,HIST(I),PT,PLUS
  700                 FORMAT (A11,I8,F8.3,'  ',A101)
                  END IF
                  CALL IPRNT(PRT,LINES)
	      END IF
	  END DO
      ELSE
C                                                                  NARROW FORMAT
C								    for each bin
	  DO I=1,IBINS
	      GRAYLEVEL = GRAYLEVEL+BINWID
	      IF (HIST(I).NE.0) THEN
		  IF (I.EQ.1) THEN
		      COL1 = LOWLIMIT
		  ELSE IF (I.EQ.IBINS) THEN
		      COL1 = HIGHLIMIT
                  ELSE
                      WRITE (COL1,400) GRAYLEVEL
		  END IF
C		 		   if one of the n=spike largest, label on graph
        	  IF (HIST(I) .LT. 0) THEN
		      J = -HIST(I)
		      HIST(I) = SPIKES(J)
                      WRITE (COL5,500) J
                  ELSE
                      COL5 = '  '
		  ENDIF
C						    update number of pixels seen
		  IF (I.GT.2 .AND. HIST(I-1).EQ.0) THEN
                      COL3 = '*     '
                  ELSE
                      COL3 = '      '
                  END IF
                  WRITE (PRT3,800) HIST(I)
  800             FORMAT(I8)
C							       draw bar of chart
		  J = (MIN(HIST(I),MAX)*50)/MAX
		  IF (J.NE.0) THEN
                      PRT = COL1 // PRT3 // COL3 // STAR(1:J) //
     +                      BLANK(J+1:50) // COL5
                  ELSE
                      PRT = COL1 // PRT3 // COL3
                  END IF
		  CALL IPRNT(PRT,LINES)
	      END IF
	  END DO
      END IF
      CALL IPRNT(' ',LINES)
 1000 CONTINUE
C				      print statistics for graph data and return
C									  print 
      IF (QEXCLUDE) CALL IPRNT(' EXCLUDING PIXELS OF DN=0',LINES)
      IF (IFORM.NE.7) THEN
         WRITE (PRT,1100) RMEAN,MM(1)
 1100    FORMAT(' Average Gray Level = ',G14.6,10X,'Minimum = ',I10)
         CALL IPRNT(PRT,LINES)
         WRITE (PRT,1200) SDEV,MM(2)
 1200    FORMAT(' Standard Deviation = ',G14.6,10X,'Maximum = ',I10)
         CALL IPRNT(PRT,LINES)
      ELSE
         WRITE (PRT,1300) RMEAN,RMM(1)
 1300    FORMAT(' Average Gray Level = ',G14.6,10X,'Minimum = ',G14.6)
         CALL IPRNT(PRT,LINES)
         WRITE (PRT,1400) SDEV,RMM(2)
 1400    FORMAT(' Standard Deviation = ',G14.6,10X,'Maximum = ',G14.6)
         CALL IPRNT(PRT,LINES)
      END IF
      WRITE (PRT,1500) INT(PIXNUM)
 1500 FORMAT(' Number of Pixels =',I8)
      CALL IPRNT(PRT,LINES)
      RETURN
      END
C**********************************************************************
	SUBROUTINE PLOTXY(HIST,RHIST,BUF,RMEAN,SDEV,RMM,PIXNUM)
        COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
	REAL RMM(2),BUF(*),RHIST(*)
	INTEGER HIST(*),SPIKES(9),SS,SL,NS,NL,SSI,SLI,NSI,NLI
	INTEGER LINC,SINC,LINES
	LOGICAL*4 NOCUM,QPPLOT
        CHARACTER*133 PRT,PRT1,PRT2,PRT3
C
C							SCALE X-AXIS
C
	BOUNDL = BOUNDL+2.5*BINWID
	X1 = AMAX1(BOUNDL,RMM(1))			! start of hist
	X2 = AMIN1(BOUNDU,RMM(2))			! end of hist
	IF (X1.GE.X2) GO TO 300
	CALL AXISPTS(X1,X2,XLO,XHI,NXTIC)
C		 					SCALE Y-AXIS
	NSPIKES = MAX(ISPIKE,2)
	CALL ZIA(SPIKES,NSPIKES)
	DO I=2,IBINS-1
	    J = NSPIKES
	    DO WHILE (HIST(I).GT.SPIKES(J) .AND. J.GT.0)
		J = J-1
	    END DO
	    IF (J.NE.NSPIKES) THEN
		K = NSPIKES
		DO L=J+2,NSPIKES
		    SPIKES(K) = SPIKES(K-1)
		    K = K-1
		END DO
		SPIKES(J+1) = HIST(I)
	    END IF
	END DO
	IF (SPIKES(2).EQ.0) 
     +    CALL XVMESSAGE(' Only 1 bin occupied, no plotting done.',' ')
	IF (SPIKES(2).EQ.0) GO TO 300
	TOP = SPIKES(ISPIKE)
	YHI = 10**INT(1.0+ALOG10(TOP))
	IF (YHI/5.0 .GE. TOP) YHI = YHI/5.0
	IF (YHI/2.0 .GE. TOP) YHI = YHI/2.0
	DIV = YHI/10.0
	NYTIC = 10
	DO WHILE (TOP .LE. YHI-DIV)
	    YHI = YHI-DIV
	    NYTIC = NYTIC-1
	END DO
	DO I=2,IBINS-1					! truncate spikes
	    IF (HIST(I).GT.INT(YHI)) HIST(I)=YHI
	END DO
C
	IF (MODE.EQ.7) THEN                             ! for bar graph
	    X = BOUNDL + BINWID/2.0
	    II = 2
	    DO WHILE (HIST(II).EQ.0 .OR. X.LE.X1)	!     find left edge
		II = II + 1
		X = X + BINWID
	    END DO
	    BUF(1) = X-BINWID
	    X = BOUNDU - BINWID/2.0			!     find right edge
	    III = IBINS
	    DO WHILE(HIST(III).EQ.0 .OR. X.GE.X2)
		III = III - 1
		X = X - BINWID
	    END DO
	    J = 0					!     fill x (BUF) and
	    X = BUF(1)					!     y (RHIST) arrays
	    BUF(1) = MAX(X,XLO)
	    BUF(2) = BUF(1)
	    DO I=II,III
		J = J + 3
		X = X + BINWID
		BUF(J) = X
		BUF(J+1) = X
		BUF(J+2) = X
		RHIST(J-2) = 0.0
		RHIST(J-1) = HIST(I)
		RHIST(J) = HIST(I)
	    END DO
	    IBINS = J + 1
	    RHIST(IBINS) = 0.0
	    BUF(IBINS) = MIN(BUF(IBINS),XHI)
	    BUF(IBINS-1) = BUF(IBINS)
	ELSE
	    X = BOUNDL
	    IBINS = IBINS-2
	    DO I=1,IBINS
		RHIST(I) = HIST(I+1)
		BUF(I) = X
		X = X+BINWID
	    END DO
	END IF
C							PLOT THE DATA
C
C				      print statistics for graph data and return
  300	CONTINUE
	N = PIXNUM
	IF (IFORM.EQ.7) THEN
	    WRITE (PRT,500) N,RMM(1),RMM(2),RMEAN,SDEV
  500       FORMAT('#',I9,'#PIXELS###RANGE#',F14.3,'#TO#',F14.3,
     *             '###MEAN#',F12.3,'###STD#DEV#',F12.3)
            I2 = 101
        ELSE
            WRITE (PRT,510) N,RMM(1),RMM(2),RMEAN,SDEV
  510       FORMAT('#',I9,'#PIXELS###RANGE#',F12.1,'#TO#',F12.1,
     *             '###MEAN#',F12.3,'###STD#DEV#',F12.3)
            I2 = 97
	END IF
	CALL SQUEEZE(PRT,PRT2,I2)
	IF (MODE.GE.3)  CALL XVMESSAGE(PRT2,' ')
C								plot to a window
	IF (MODE.GE.6) THEN
	    OPEN (11,FILE='scrvhist001',STATUS='NEW')
	    WRITE(11,280) XLO,XHI,0.0,YHI
	    WRITE(11,270) NXTIC,NYTIC
  270	    FORMAT(2I10)
	    DO I=1,IBINS
		WRITE (11,280) BUF(I),RHIST(I)
  280		FORMAT(4E14.7)
	    END DO
	    CLOSE(11)
	    ISTAT = SYSTEM('idl vicarhist.inp')
	    OPEN (11,FILE='scrvhist001',STATUS='OLD')
	    CLOSE(11,STATUS='DELETE')
	END IF
C								hardcopy plot?
	IF (MODE.NE.4) THEN
	    CALL XVINTRACT('PLOTR','Print this plot (Yes, No) [NO]? ')
	    CALL XVIPARM('PENPLOT',PRT,ICNT,IDEF,1,0)
	    QPPLOT = PRT(1:1).EQ.'Y' .OR. PRT(1:1).EQ.'y'
	END IF
C								submit to the
C								laser printer
	IF (QPPLOT) THEN
	    CALL XVP('TITLE',PRT1,ICNT)
	    DO I=81,1,-1
		IF (PRT1(I:I) .NE. ' ') THEN
		    PRT1(I+1:I+1) = CHAR(0)
		    I1 = I
		    GO TO 650
		END IF
	    END DO
  650	    CONTINUE
C
	    CALL XVGET(INUN,ISTAT,'NAME',PRT3,0)
            WRITE (PRT,700) PRT3,ISL,ISS,INL-ISL+1,INS-ISS+1,LINC,SINC
  700	    FORMAT(A60,'###(', I5, ',', I5, ',', I5, ',', I5,
     +		   ')####LINC#=#', I4, '####SINC#=#', I4)
	    I3 = 118
	    CALL SQUEEZE(PRT,PRT3,I3)
	    CALL PSPLOT(BUF,RHIST,IBINS,XLO,XHI,0.0,YHI,NXTIC,
     +	                NYTIC,'DN','Frequency',PRT1,PRT2,PRT3,-1)
	END IF
C
	RETURN
	END
C**********************************************************************
	SUBROUTINE SHIST(HIST,RMEAN,SDEV,MM,PIXNUM)
        COMMON NUMARS,IAREA(600),SS,SL,NS,NL,SSI,SLI,NSI,NLI,LINC,
     *    SINC,IREC,IFORM,IBINS,BOUNDL,BOUNDU,ISPIKE,NOCUM,MODE,
     *    ISL,ISS,INL,INS,ISB,IEB,LINES,BINWID,INUN,QEXCLUDE,QPPLOT
	INTEGER HIST(256),SPIKES(9),SS,SL,NS,NL,SSI,SLI,NSI,NLI
	INTEGER LINC,SINC,LINES,MM(2),NEWBIN(80)
	LOGICAL*4 NOCUM
        CHARACTER*1 SCREEN(81,22)/1782*' '/
        CHARACTER*132 PRT
C
	BOUNDL = BOUNDL+2.5*BINWID
	IF (BOUNDL.EQ.0.0 .AND. BOUNDU.EQ.255.0) THEN
	    LOW = MM(1)
	    IHI = MM(2)
	ELSE
	    LOW = BOUNDL
	    IHI = BOUNDU
	END IF
	IBINWIDTH = 1 + (IHI-LOW)/80
	NBINS = 1 + (IHI-LOW)/IBINWIDTH
C
	CALL ZIA(NEWBIN,80)
	N = LOW
	DO I=1,NBINS
	    DO J=1,IBINWIDTH
		N = N+1
		NEWBIN(I) = NEWBIN(I)+HIST(N)
	    END DO
	END DO
C		 				FIND SPIKE LOCATIONS
	CALL ZIA(SPIKES,9)
	DO J=1,ISPIKE
	    MAX = 0
	    DO I=1,NBINS
		IF (NEWBIN(I) .GT. MAX) THEN
		    IMAX = I
		    MAX = NEWBIN(I)
		END IF
	    END DO
	    IF(MAX .EQ. 0) GO TO 6
	    SPIKES(J) = NEWBIN(IMAX)
	    NEWBIN(IMAX) = -J
	END DO
    6   IF (MAX.EQ.0 .AND. J.NE.1) MAX=SPIKES(J-1)
	DIVISOR = MAX/19.0
	NUM = LOW+IBINWIDTH/2
C								label x-axis
	DO I=1,NBINS
	    IF (MOD(I,5).EQ.1) THEN
                WRITE (PRT,100) NUM
  100           FORMAT(I3)
		DO J=1,3
		    SCREEN(I+1,J+19) = PRT(J:J)
		END DO
		NUM = NUM+5*IBINWIDTH
	    END IF
	    IF (NEWBIN(I).NE.0) THEN
C		 		   if one of the n=spike largest, label on graph
		IF (NEWBIN(I) .LT. 0) THEN
                    WRITE(SCREEN(I+1,1),200) -NEWBIN(I)
  200               FORMAT(I1)
		    N = 2
		ELSE
		    N = 20.5-NEWBIN(I)/DIVISOR
		END IF
		DO K=N,19
		    SCREEN(I+1,K) = '*'
		END DO
	    END IF
	END DO
	DO I=1,22
            WRITE (PRT,300) (SCREEN(J,I),J=1,80)
  300       FORMAT(80A1)
            CALL XVMESSAGE(PRT,' ')
	END DO
C				      print statistics for graph data and return
	N = PIXNUM
        WRITE (PRT,500) N,MM(1),MM(2),RMEAN,SDEV
  500   FORMAT(I10,'PIXELS   RANGE',I4,'-',I3,'     MEAN',F8.3,
     1  '     STD DEV',F8.3)
        CALL XVMESSAGE(PRT,' ')
C
	RETURN
	END
C**********************************************************************
C    PRINT MESSAGE, INCREASE LINE COUNT, AND CHECK FOR FULL SCREEN
C**********************************************************************
      SUBROUTINE IPRNT(PRT,LINES)
      COMMON/PAUSE/ QPAUSE
      LOGICAL QPAUSE
      CHARACTER*(*) PRT
C
      CALL XVMESSAGE(PRT,' ')
      LINES = LINES + 1
      IF (LINES.GE.23 .AND. QPAUSE) THEN
	  CALL XVINTRACT('PAGER','PRESS RETURN')
	  LINES = 0
      END IF
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create hist.pdf
process help=*
SUBCMD-DEFAULT NORMAL
LOCAL LMEAN  TYPE=REAL INITIAL=0.0
LOCAL LSIGMA TYPE=REAL INITIAL=0.0
LOCAL LMIN   TYPE=REAL INITIAL=0.0
LOCAL LMAX   TYPE=REAL INITIAL=0.0
PARM INP     TYPE=(STRING,40)
PARM SIZE    TYPE=INTEGER COUNT=4     DEFAULT=(1,1,0,0)
PARM BANDS   TYPE=INTEGER COUNT=(0:2) DEFAULT=--
PARM SL      TYPE=INTEGER DEFAULT=1
PARM SS      TYPE=INTEGER DEFAULT=1
PARM SB      TYPE=INTEGER DEFAULT=1
PARM NL      TYPE=INTEGER DEFAULT=0
PARM NS      TYPE=INTEGER DEFAULT=0
PARM NB      TYPE=INTEGER DEFAULT=0
PARM INC     TYPE=INTEGER DEFAULT=1
PARM LINC    TYPE=INTEGER DEFAULT=1
PARM SINC    TYPE=INTEGER DEFAULT=1
PARM FORMAT  TYPE=KEYWORD COUNT=0:1 VALID=(FULL,BYTE,REAL,HALF) DEFAULT=--
PARM SPIKES  TYPE=INTEGER DEFAULT=1
PARM BINS    TYPE=INTEGER DEFAULT=1000
PARM LIMITS  TYPE=REAL    COUNT=2 DEFAULT=(0.,255.)
PARM EXCLUDE TYPE=KEYWORD VALID=EXCLUDE COUNT=(0:1) DEFAULT=--
PARM BYCHAN  TYPE=KEYWORD VALID=BYCHAN COUNT=(0:1) DEFAULT=--
PARM MODE    TYPE=KEYWORD DEFAULT=NORMAL + 
                  VALID=(WIDE,NORMAL,SCREEN,NOHIST,WINDOW,WBAR)
PARM PPLOT   KEYWORD      COUNT=0:1 DEFAULT=-- VALID=PPLOT
PARM TITLE   TYPE=(STRING,80) DEFAULT="Histogram"
PARM NOCUM   KEYWORD      VALID=NOCUM COUNT=0:1 DEFAULT=--
PARM AREA    TYPE=INTEGER COUNT=4:200 DEFAULT=(1,1,0,0)
PARM PAUSE   KEYWORD   DEFAULT=NOPAUSE VALID=(PAUSE,NOPAUSE)
PARM MEAN    TYPE=NAME DEFAULT=LMEAN
PARM SIGMA   TYPE=NAME DEFAULT=LSIGMA
PARM MIN     TYPE=NAME DEFAULT=LMIN
PARM MAX     TYPE=NAME DEFAULT=LMAX
END-SUBCMD
SUBCMD PLOTR
PARM PENPLOT  TYPE=KEYWORD DEFAULT=-- VALID=(YES,NO,HP,LASER) COUNT=(0:1)
END-SUBCMD
SUBCMD PAGER
PARM NEWPAGE TYPE=STRING DEFAULT="NEWPAGE"
END-SUBCMD
END-PROC
.TITLE
VICAR Program HIST
.HELP
PURPOSE:
HIST prints DN-frequency histograms of one or more areas of an input data
set. It is accepts BYTE, HALF, FULL, and REAL image data types and allows 
the user to specify the maximum number of bars to be used in the graph, 
as well as the range of values which those bars represent. There are several
options available for the print format of the output. The DN axis may be
aligned vertically using 80 columns (NORMAL) or 132 columns (WIDE); or, the
DN axis can be horizontal and formatted to fit within a 23x80 screen (SCREEN).
The WINDOW and WBAR options plot to IDL windows.  If PPLOT is 
specified, the histogram is generated as a line plot on the printer. The 
SCREEN mode works only on byte data, and the WINDOW and WBAR modes will not 
run in batch. The PPLOT option and one of the other options may be run in the 
same execution.
.page
Note that HIST does not determine the histogram limits: these must be
set by the LIMITS parameter, which defaults to (0,255) for byte, or to
(-32768,32767) for other data types. (To obtain an estimate of the DN
range in an image, use the keyword NOHIST in a preliminary run, then
run HIST again with suitable LIMITS.)
.page
Examples

HIST A 

This command will print out an 80-column-format histogram of input image A
based on A's full area.  The histogram will be normalized such that the
first and second most frequent values are each represented by a fifty
character bar. (This is determined by the graph display format, the default
narrow format in this case, and by the default for the SPIKES option; the
wide-graph format has a different default normalization procedure which prints 
the graph so that the largest bar is 100 characters and all other bars are
proportional.  This narrow format allows the user to see in detail the size
of the shorter bars when some bars are much larger than others.  The SPIKES
option described here is described in more detail below.  The default is
SPIKES=1.) Pixel values will be distributed among a maximum of 256
(default) bars as appropriate; the default range of pixel values to accept
is 0 through 255 for byte data, and -32768 through 32767 for halfword,
real, and integer data. 
.page
HIST A BINS=21 LIMITS=(0.0,40.0) FORMAT=HALF

This command will print out an 80-column histogram as above, but will use
21 bins, evenly spaced, with centers from 0.0 to 40.0. The width of each bin,
then, is 2.0. There are, in addition, two bins to keep track of pixels which
are outside of the specified limits. These bins are also represented by 
labeled bars on the graph.  Note that the FORMAT option was used to specify 
that the image is to be read as HALF data; the default is to read the image 
data format from the image's system label. (The allowable data types are 
BYTE, HALF, FULL, and REAL.)
.page
HIST A 'SCREEN 'PPLOT TITLE="TRONA CLASSIFICATION RESULTS"

This command will plot the histogram of A, with DN values running horizontally
across the 80 column screen, and the populations running vertically. The 
format is set to allow viewing of the entire histogram at once. The bins are 
compressed as needed to fit on the screen, and only byte data is permitted for
the screen option. The PPLOT keyword causes a pen plot to be generated on the
printer, in addition to the screen histogram. The value of the TITLE 
parameter is put as a caption to the plot.
.page
HIST A 'WIDE AREA=(1,1,15,15)

This command will print out another histogram of image A, this time using
the wide (132 column) format.  Only the first 15 samples of the first 15
lines of the image will be plotted on the graph. 

HIST A AREA=(1,1,15,15,100,100,10,40) BINS=300

This command will print two graphs in the narrow format, using a maximum of
three hundred bars to represent the data.  The range of the bars will be
either of two defaults: 0 through 255 if image A is in byte format, and
-32768 through 32767 if the image is in another, larger format.  Note that
the request is for two graphs of the same image.  The first graph will be
for the area (1,1,15,15) and the second will be for the area
(100,100,10,40).  If the 'WIDE option is used, these graphs will be labeled
with headers giving the area represented; the default, compact histogram
does not give this information. 
.page	
HIST A SPIKES=3 LINC=2 SINC=3

This command will produce a histogram of image A for which only every third
sample of every second line will be read.  This is done mainly to speed
output of the program.  Note that there is also an INC option which may be
set to any similar increment, in effect setting LINC and SINC to the same
value.  Again, this is done mainly to speed processing of the image.  Note,
too, the use of the SPIKES option.  As mentioned in the example above,
SPIKES specifies the bar which is to be normalized to a bar value of 50,
which is the maximum; any values larger than this value will be reduced to
50, and the graph will indicate the rank of each bar which is 50 units
long.  This option, in effect, removes any large spikes in the graph which
would otherwise dwarf smaller bars in the narrow format.  The default value
of SPIKE is one. 

.page
PROGRAM HISTORY

WRITTEN BY:  Alan Mazer, September 1983
COGNIZANT PROGRAMMER:  Ray Bambery
REVISIONS:
     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 23 SEPT 1983
     REVISION 1 BY ASM, FEBRUARY 7 1984 - SPEED ENHANCEMENTS
         1) REPLACED CONVERSION OF ALL INPUT DATA TO REAL-TYPE WITH
	     SEPARATE TABULATION ROUTINES FOR EACH TYPE
         2) ADDED LOOK-UP TABLE FOR BYTE-IMAGE PROCESSING
  1984-10-9   LWK  converted to Vicar2, check for rounding error in sdev.
  1984-10-11  LWK  for byte data, compute stats from histogram.
  1984-12-13  LWK  revised treatment of BINS, LIMITS.
  1985-4-17   REA  fixed bug in LINC & AREA parameters
  1985-4-17   LWK  revised processing of REAL*4 data
  1986-11-11  REA  modify formatting, hist collection routines, add
		       output parameters MEAN, SIGMA
  1987-1-12   REA  add EXCLUDE, SCREEN parameters
  1987-2-4    REA  add SPLOT, PPLOT and TITLE parameters
  1987-8-4    REA  fix bug in x-axis scaling algorithm
  1987-10-27  REA  add 3-D file capability
  1989-8-23   REA  add BAR graph parameter
  1990-2-22   REA  add output parameters MIN, MAX
  1990-10-5   REA  add laserprinter capabilities
  1991-4-8    REA  convert to UNIX
  1992-8-6    REA  add WINDOW and WBAR options
  1996-10-25  REA  remove Regis options
  1997-8-20   REA  add BYCHAN option
  2000-7-12   REA  adjust print-out column widths for DN and population
  2001-2-16   REA  fix min/max bug when both regular and pen plots;
                   consolidate statistics logic.
  2015-08-10  WLB  replaced xqout call with xvqout call to pass out vars to shell vicar

.page
OPERATION:
The main part of the program, MAIN44, sets up defaults and handles most of
the options requested by the user, modifying variables and setting flags. 
MAIN44 then calls subroutine WORK (through STACKA for dynamic storage
allocation). 

WORK goes through each of the areas of the image, reading the lines which
are part of the area, and sorting pixels into buckets based on the number
of buckets and their range.  The bucket width equals the difference between
the limits of the range, divided by the number of buckets minus 1. The 
variables SUM and SUM2 are updated for each pixel to be used later for the 
calculation of the mean and standard deviation.  

Finally, when the entire area has been read in, PHIST, SHIST, or PLOTXY is 
called to dislay the histogram data, and the next area for the image, if any,
is similarly processed. 

.page
WRITTEN BY:  Alan Mazer, September 1983
COGNIZANT PROGRAMMER:  Ron Alley

.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE BANDS
Starting band, # of bands
(for 3-D format data)


.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE SB
Starting band
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE NB
Number of bands

.VARIABLE FORMAT
KEYWORD - Input data format 
(BYTE, HALF, FULL, REAL)
.VARIABLE SPIKES
Bar to be normalized 
.VARIABLE SINC
Sample increment
.VARIABLE LINC
Line increment
.VARIABLE INC
Line/sample increment
.VARIABLE AREA
Area(s) to be graphed
.VARIABLE MODE
Output format options, Valid:
NORMAL, WIDE, SCREEN,
WINDOW, WBAR, NOHIST
.VARIABLE PPLOT
PostScript print option
Valid: PPLOT
.VARIABLE TITLE
STRING - Optional title for
plot.
.VARI NOCUM
Non-cumulative percentages?
Valid: NOCUM
.VARIABLE BINS
Max number of graph bars
.VARIABLE LIMITS
Range of pixel values
.VARIABLE EXCLUDE
Exclude 0 DN pixels from
mean and std dev calculations
.VARIABLE BYCHAN
Produce a histogram for each
band? (Valid: BYCHAN)
(Default: 1 histogram of all
 selected bands)
.vari PAUSE
KEYWORD: adds interactive
pausing when screen full.
Valid: NOPAUSE, PAUSE
.vari MEAN
Output parameter
.vari SIGMA
Output parameter
.vari MIN
Output parameter
.vari MAX
Output parameter
.LEVEL2
.vari INP
INP is the name of the input image file.
.vari SIZE
SIZE specifies the portion of the input image which is to be processed by 
the program.  It contains 4 integers:

(Starting Line, Starting Sample, Number of Lines, Number of Samples).

These can also be specified separately by the parameters SL, SS, NL, and NS.
.vari BANDS
If the input data is in 3-D format, BANDS specifies the starting band
desired, and the number of bands to be processed. Note that the output
histogram will be an aggregate of all bands requested.
.vari SL
See SIZE.
.vari SS
See SIZE.
.vari SB
For 3-D data, this parameter denotes the first band to be included in the
processing.
.vari NL
See SIZE.
.vari NS
See SIZE.
.vari NB
For 3-D data, this parameter denotes the number of bands to be included in
the processing.
.VARIABLE FORMAT
FORMAT specifies the input image data type and may have any of four values:
BYTE, HALF, FULL, or REAL.  If FORMAT isn't specified, each image will
be treated as being in the data format specified by its system label.
.VARIABLE SPIKES
SPIKES=N specifies that the printed histogram will be normalized in frequency 
such that the N-th most frequent value is printed as the full width on 
the page.  All values which occur more frequently will also be represented 
by a full width bar, ranked by number on the right side. 

 SPIKES may have any value from 1 to 9, inclusive; the default is 1
.VARIABLE SINC
SINC=N specifies that every N-th sample in the chosen area is to be used.
The default is for every value to be used.

Note that if INC is specified, this parameter is ignored. (INC specifies
SINC and LINC.)
.VARIABLE LINC
LINC=N specifies that every N-th line in the chosen area is to be used.  The
default is for every value to be used.

Note that if INC is specified, this parameter is ignored. (INC specifies
SINC and LINC.)
.VARIABLE INC
INC=N specifies that every N-th sample in both dimensions in the chosen area 
is to be used.  Use of this keyword speeds histogram compilation.  The
default is for every pixel in the area to be used. 

This keyword overrides any values of SINC or LINC that are specified 
separately.
.VARIABLE AREA
AREA is used to specify the portion(s) of the input image which is/are to
be graphed.  If AREA is not specified, the program will use the entire image
as determined by the label of the input file.  Several areas may be specified
using the one keyword.
.VARIABLE MODE
    NOHIST - Only the statistical data (mean, standard deviation, minimum, 
             maximum, and number of pixels) is printed. No histogram is formed.
    NORMAL - (Default) An 80 column wide histogram is formed, with DN values 
             running vertically.
    WIDE   - A 132 column wide histogram is formed, with DN values running 
             vertically.
    SCREEN - A 22 row by (up to) 80 column histogram is formed, with DN values 
             running horizontally. The is designed to fit within a single 
             screen. Byte is the only permissible data format for this option.
    WINDOW - A line plot of the histogram data is formed, using a window and
             run through IDL software. It requires a terminal with windowing
             software.  After the plot is displayed, the user is given the
             option of also producing a hardcopy plot of the data.
    WBAR   - A bar graph of the histogram data is formed, using IDL and
             windowing software.  This is the window equivalent of the BAR
             option.
.VARIABLE PPLOT
This keyword is used to generate a plot on the printer. It may be used with 
any of the MODE options. However, if MODE is defaulted and PPLOT is specified,
no histogram is output to the terminal or log file. The WINDOW and WBAR options
automatically ask the user whether a printed plot is desired.
.VARIABLE TITLE
This title is placed beneath printed plots. It has no effect if PPLOT is not
specified. In addition to this line of annotation, two other caption lines
are always generated for pen plots. One line contains the histogram
statistics, while the other lists the file name, area, linc, and sinc.
.vari NOCUM
NOCUM specifies that the percentages printed for each bin in the WIDE format
 are the percentage of the pixels in each bin, instead of the cumulative
percentage of all bins up to and including the current one, which is the
default.

If the wide-format option has not been specified, this keyword is ignored.
.VARIABLE BINS
BINS specifies the maximum number of buckets to be used in sorting image
pixels.  The default is 256.
 
Note that the range spanned by these bins is determined by the LIMITS
parameter, and defaults to (0,255) for byte, (-32768,32767) for non-byte.
 
The value of the midpoint of each bin is printed on the output histogram
lines.
.VARIABLE LIMITS
LIMITS specifies the upper and lower bounds of the histogram.  The defaults
are 0 and 255 for byte data, and -32768 and 32767 for halfword or fullword
integer and real data.

.VARIABLE EXCLUDE
Exclude 0 DN pixels from the mean and standard deviation values reported
at the end of the histogram. This does not affect the histogram itself.
.VARIABLE BYCHAN
Under normal operation a single histogram is produced, including all bands
specified by the BANDS parameter.  If the BYCHAN keyword is used, a
separate histogram for each requested band is produced.
.VARI PAUSE
PAUSE specifies that the program is to pause when the screen is filled
in interactive mode.  Default is for the program not to pause every 23 lines.
This parameter has no effect in batch mode. 
.VARI MEAN
This is an output parameter. When HIST has completed running, this parameter
contains the mean value of the last histogram. This value may be used by
subsequent programs within a procedure by declaring MEAN as a local real
variable in the procedure.
.VARI SIGMA
This is an output parameter. When HIST has completed running, this parameter
contains the value of the standard deviation for the last histogram. This 
value may be used by subsequent programs within a procedure by declaring 
SIGMA as a local real variable in the procedure.
.VARI MIN
This is an output parameter. When HIST has completed running, this parameter
contains the minimum value for the last histogram. This value may be used by 
subsequent programs within a procedure by declaring MIN as a local real 
variable in the procedure.
.VARI MAX
This is an output parameter. When HIST has completed running, this parameter
contains the maximum value for the last histogram. This value may be used by 
subsequent programs within a procedure by declaring MAX as a local real 
variable in the procedure.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create hist.imake
#define  PROGRAM   hist

#define MODULE_LIST hist.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB

#define FTNINC_LIST pgminc
$ Return
$!#############################################################################
$Test_File:
$ create tsthist.pdf
procedure
refgbl $echo
refgbl $autousage
local sd  type=real		!Declare SD and AVG
local avg type=real		!as local TAE variables

! TEST SCRIPT FOR HIST derived from p2/tsthist.pdf
! tests BYTE, HALF, FULL, REAL, DOUB images
!
! Vicar Programs:
!       gen disp                 
! 
! parameters:
!   <none>
!
! Requires NO external test data: 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
body
let _onfail="stop"
let $echo="yes"
let $autousage="none"

! TEST SCRIPT FOR HIST
!
! BYTE IMAGE
gen g1515 15 15
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! TEST SPIKES
hist g1515 SPIKE=5
hist g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
let $echo="no"
putmsg "Print average and stdev variables" ""
let $echo="yes"
disp avg
disp sd
!!  Throw in some extra tests for good coverage.
!
! test SCREEN keyword
hist g1515 'screen spike=1
!
!  make an all zero file.  Hist should not blow up on 'exclude.
gen g 10 10 linc=0 sinc=0
hist g 'exclude
gen g 1024 1000 sinc=0 
let $echo="no"
write "Should get mean =128.0 because of exclude"
let $echo="yes"
hist g 'exclude
!
gen g 10 10 sinc=0
hist g inc=3
let $echo="no"
write "Mean should be 4.5."
let $echo="yes"
hist g inc=3 'exclude
let $echo="no"
write "Mean should be 6.0."
let $echo="yes"
!
! HALF
gen g1515 15 15 linc=-1000 sinc=-1000 'half
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! TEST SPIKES
hist g1515 SPIKE=5
hist g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist sigma=sd  mean=avg 	!Compute SD and AVG
let $echo="no"
putmsg "Print average and stdev variables" ""
let $echo="yes"
disp AVG
disp SD
!!
! try a case that HIST used to get wrong.  Should not be any
! > HIGH LIMIT entry.
gen g 10 10 linc=1000 'half
hist g spikes=9
!
! FULL
gen f1515 15 15 linc=-100000 sinc=-100000 'full
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist f1515 area=(1,1,3,3,10,10,2,2)
!
hist f1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist f1515 linc=2 sinc=3
!
! TEST SPIKES
HIST f1515 SPIKE=5
HIST f1515 SPIKE=1
!
! test NOHIST keyword
hist f1515 'nohist
!
!  test output to TCL variables.
hist f1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
let $echo="no"
putmsg "Print average and stdev variables" ""
let $echo="yes"
disp avg
disp sd
!!
!    ! test REAL*4 data
gen r1515 15 15 linc=1.e8 sinc=1.e8 'real4
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist r1515 area=(1,1,3,3,10,10,2,2)
!
hist r1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist r1515 linc=2 sinc=3
!
! TEST SPIKES
hist r1515 SPIKE=5
hist r1515 SPIKE=1
!
! test NOHIST keyword
hist r1515 'nohist
!
!  test output to TCL variables.
hist r1515 'nohist SIGMA=SD  MEAN=AVG 	!Compute SD and AVG
let $echo="no"
putmsg "Print average and stdev variables" ""
let $echo="yes"
disp AVG
disp SD
!!
gen r1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
let $echo="no"
write " Try some wild numbers.  3000 bins.  225 are non-empty."
write " Should skip all empty bins and put a * after DN to indicate skip."
let $echo="yes"
hist r1515

!  test of bands capability
gen x1515 NS=10 NL=10 NB=10
hist x1515
hist x1515 NB=4
!!
! test AR 112483.  Std dev should be exactly 0.  Depends on image size.
gen x1515 337 364 ival=200 linc=0 sinc=0
hist x1515
!
! test result on all-zero files:
gen g1515 10 10 ival=0 linc=0 sinc=0
hist g1515 'nohis
gen g1515 10 10 ival=0 linc=0 sinc=0 'real
hist g1515 'nohis
!
let $echo="no"

! clean up
ush rm -f ?1515
ush rm -f g

end-proc
$!-----------------------------------------------------------------------------
$ create tsthist.log_solos
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

let $autousage="none"
gen g1515 15 15
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           1      ****************
 1.0000           2      *********************************
 2.0000           3      **********************************************     1
 3.0000           2      *********************************
 4.0000           1      ****************

 Average Gray Level =    2.00000              Minimum =          0
 Standard Deviation =    1.15470              Maximum =          4
 Number of Pixels =       9


 18.000           1*     *************************
 19.000           2      **********************************************     1
 20.000           1      *************************

 Average Gray Level =    19.0000              Minimum =         18
 Standard Deviation =   0.707107              Maximum =         20
 Number of Pixels =       4

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 2.0000           1*     ****************
 3.0000           2      *********************************
 4.0000           3      **********************************************     1
 5.0000           2      *********************************
 6.0000           1      ****************

 Average Gray Level =    4.00000              Minimum =          2
 Standard Deviation =    1.15470              Maximum =          6
 Number of Pixels =       9


 20.000           1*     *************************
 21.000           2      **********************************************     1
 22.000           1      *************************

 Average Gray Level =    21.0000              Minimum =         20
 Standard Deviation =   0.707107              Maximum =         22
 Number of Pixels =       4

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           1      ****************
 2.0000           1*     ****************
 3.0000           1      ****************
 4.0000           1      ****************
 5.0000           1      ****************
 6.0000           2      *********************************
 7.0000           1      ****************
 8.0000           2      *********************************
 9.0000           2      *********************************
 10.000           2      *********************************
 11.000           2      *********************************
 12.000           3      **********************************************     1
 13.000           2      *********************************
 14.000           3      **********************************************
 15.000           2      *********************************
 16.000           2      *********************************
 17.000           2      *********************************
 18.000           2      *********************************
 19.000           1      ****************
 20.000           2      *********************************
 21.000           1      ****************
 22.000           1      ****************
 23.000           1      ****************
 24.000           1      ****************
 26.000           1*     ****************

 Average Gray Level =    13.0000              Minimum =          0
 Standard Deviation =    6.24500              Maximum =         26
 Number of Pixels =      40

hist g1515 SPIKE=5
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           1      ***
 1.0000           2      *******
 2.0000           3      ***********
 3.0000           4      ***************
 4.0000           5      *******************
 5.0000           6      ***********************
 6.0000           7      **************************
 7.0000           8      ******************************
 8.0000           9      **********************************
 9.0000          10      **************************************
 10.000          11      ******************************************
 11.000          12      **********************************************
 12.000          13      **********************************************     4
 13.000          14      **********************************************     2
 14.000          15      **********************************************     1
 15.000          14      **********************************************     3
 16.000          13      **********************************************     5
 17.000          12      **********************************************
 18.000          11      ******************************************
 19.000          10      **************************************
 20.000           9      **********************************
 21.000           8      ******************************
 22.000           7      **************************
 23.000           6      ***********************
 24.000           5      *******************
 25.000           4      ***************
 26.000           3      ***********
 27.000           2      *******
 28.000           1      ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

hist g1515 SPIKE=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           1      ***
 1.0000           2      ******
 2.0000           3      **********
 3.0000           4      *************
 4.0000           5      ****************
 5.0000           6      ********************
 6.0000           7      ***********************
 7.0000           8      **************************
 8.0000           9      ******************************
 9.0000          10      *********************************
 10.000          11      ************************************
 11.000          12      ****************************************
 12.000          13      *******************************************
 13.000          14      **********************************************
 14.000          15      **********************************************     1
 15.000          14      **********************************************
 16.000          13      *******************************************
 17.000          12      ****************************************
 18.000          11      ************************************
 19.000          10      *********************************
 20.000           9      ******************************
 21.000           8      **************************
 22.000           7      ***********************
 23.000           6      ********************
 24.000           5      ****************
 25.000           4      *************
 26.000           3      **********
 27.000           2      ******
 28.000           1      ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

hist g1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp avg

14.0

disp sd

6.11010074615

hist g1515 'screen spike=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

                1
               ***
               ***
              *****
             *******
            *********
           ***********
           ***********
          *************
         ***************
        *****************
       *******************
       *******************
      *********************
     ***********************
    *************************
   ***************************
   ***************************
  ****************************

           1    1    2    2
 0    5    0    5    0    5
       225PIXELS   RANGE   0- 28     MEAN  14.000     STD DEV   6.110
gen g 10 10 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g 'exclude
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.         100      **********************************************     1

 EXCLUDING PIXELS OF DN=0
 Average Gray Level =         0.              Minimum =          0
 Standard Deviation =         0.              Maximum =          0
 Number of Pixels =       0

gen g 1024 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
Should get mean =128.0 because of exclude
hist g 'exclude
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.        4000      **********************************************     1
 1.0000        4000      **********************************************
 2.0000        4000      **********************************************
 3.0000        4000      **********************************************
 4.0000        4000      **********************************************
 5.0000        4000      **********************************************
 6.0000        4000      **********************************************
 7.0000        4000      **********************************************
 8.0000        4000      **********************************************
 9.0000        4000      **********************************************
 10.000        4000      **********************************************
 11.000        4000      **********************************************
 12.000        4000      **********************************************
 13.000        4000      **********************************************
 14.000        4000      **********************************************
 15.000        4000      **********************************************
 16.000        4000      **********************************************
 17.000        4000      **********************************************
 18.000        4000      **********************************************
 19.000        4000      **********************************************
 20.000        4000      **********************************************
 21.000        4000      **********************************************
 22.000        4000      **********************************************
 23.000        4000      **********************************************
 24.000        4000      **********************************************
 25.000        4000      **********************************************
 26.000        4000      **********************************************
 27.000        4000      **********************************************
 28.000        4000      **********************************************
 29.000        4000      **********************************************
 30.000        4000      **********************************************
 31.000        4000      **********************************************
 32.000        4000      **********************************************
 33.000        4000      **********************************************
 34.000        4000      **********************************************
 35.000        4000      **********************************************
 36.000        4000      **********************************************
 37.000        4000      **********************************************
 38.000        4000      **********************************************
 39.000        4000      **********************************************
 40.000        4000      **********************************************
 41.000        4000      **********************************************
 42.000        4000      **********************************************
 43.000        4000      **********************************************
 44.000        4000      **********************************************
 45.000        4000      **********************************************
 46.000        4000      **********************************************
 47.000        4000      **********************************************
 48.000        4000      **********************************************
 49.000        4000      **********************************************
 50.000        4000      **********************************************
 51.000        4000      **********************************************
 52.000        4000      **********************************************
 53.000        4000      **********************************************
 54.000        4000      **********************************************
 55.000        4000      **********************************************
 56.000        4000      **********************************************
 57.000        4000      **********************************************
 58.000        4000      **********************************************
 59.000        4000      **********************************************
 60.000        4000      **********************************************
 61.000        4000      **********************************************
 62.000        4000      **********************************************
 63.000        4000      **********************************************
 64.000        4000      **********************************************
 65.000        4000      **********************************************
 66.000        4000      **********************************************
 67.000        4000      **********************************************
 68.000        4000      **********************************************
 69.000        4000      **********************************************
 70.000        4000      **********************************************
 71.000        4000      **********************************************
 72.000        4000      **********************************************
 73.000        4000      **********************************************
 74.000        4000      **********************************************
 75.000        4000      **********************************************
 76.000        4000      **********************************************
 77.000        4000      **********************************************
 78.000        4000      **********************************************
 79.000        4000      **********************************************
 80.000        4000      **********************************************
 81.000        4000      **********************************************
 82.000        4000      **********************************************
 83.000        4000      **********************************************
 84.000        4000      **********************************************
 85.000        4000      **********************************************
 86.000        4000      **********************************************
 87.000        4000      **********************************************
 88.000        4000      **********************************************
 89.000        4000      **********************************************
 90.000        4000      **********************************************
 91.000        4000      **********************************************
 92.000        4000      **********************************************
 93.000        4000      **********************************************
 94.000        4000      **********************************************
 95.000        4000      **********************************************
 96.000        4000      **********************************************
 97.000        4000      **********************************************
 98.000        4000      **********************************************
 99.000        4000      **********************************************
 100.00        4000      **********************************************
 101.00        4000      **********************************************
 102.00        4000      **********************************************
 103.00        4000      **********************************************
 104.00        4000      **********************************************
 105.00        4000      **********************************************
 106.00        4000      **********************************************
 107.00        4000      **********************************************
 108.00        4000      **********************************************
 109.00        4000      **********************************************
 110.00        4000      **********************************************
 111.00        4000      **********************************************
 112.00        4000      **********************************************
 113.00        4000      **********************************************
 114.00        4000      **********************************************
 115.00        4000      **********************************************
 116.00        4000      **********************************************
 117.00        4000      **********************************************
 118.00        4000      **********************************************
 119.00        4000      **********************************************
 120.00        4000      **********************************************
 121.00        4000      **********************************************
 122.00        4000      **********************************************
 123.00        4000      **********************************************
 124.00        4000      **********************************************
 125.00        4000      **********************************************
 126.00        4000      **********************************************
 127.00        4000      **********************************************
 128.00        4000      **********************************************
 129.00        4000      **********************************************
 130.00        4000      **********************************************
 131.00        4000      **********************************************
 132.00        4000      **********************************************
 133.00        4000      **********************************************
 134.00        4000      **********************************************
 135.00        4000      **********************************************
 136.00        4000      **********************************************
 137.00        4000      **********************************************
 138.00        4000      **********************************************
 139.00        4000      **********************************************
 140.00        4000      **********************************************
 141.00        4000      **********************************************
 142.00        4000      **********************************************
 143.00        4000      **********************************************
 144.00        4000      **********************************************
 145.00        4000      **********************************************
 146.00        4000      **********************************************
 147.00        4000      **********************************************
 148.00        4000      **********************************************
 149.00        4000      **********************************************
 150.00        4000      **********************************************
 151.00        4000      **********************************************
 152.00        4000      **********************************************
 153.00        4000      **********************************************
 154.00        4000      **********************************************
 155.00        4000      **********************************************
 156.00        4000      **********************************************
 157.00        4000      **********************************************
 158.00        4000      **********************************************
 159.00        4000      **********************************************
 160.00        4000      **********************************************
 161.00        4000      **********************************************
 162.00        4000      **********************************************
 163.00        4000      **********************************************
 164.00        4000      **********************************************
 165.00        4000      **********************************************
 166.00        4000      **********************************************
 167.00        4000      **********************************************
 168.00        4000      **********************************************
 169.00        4000      **********************************************
 170.00        4000      **********************************************
 171.00        4000      **********************************************
 172.00        4000      **********************************************
 173.00        4000      **********************************************
 174.00        4000      **********************************************
 175.00        4000      **********************************************
 176.00        4000      **********************************************
 177.00        4000      **********************************************
 178.00        4000      **********************************************
 179.00        4000      **********************************************
 180.00        4000      **********************************************
 181.00        4000      **********************************************
 182.00        4000      **********************************************
 183.00        4000      **********************************************
 184.00        4000      **********************************************
 185.00        4000      **********************************************
 186.00        4000      **********************************************
 187.00        4000      **********************************************
 188.00        4000      **********************************************
 189.00        4000      **********************************************
 190.00        4000      **********************************************
 191.00        4000      **********************************************
 192.00        4000      **********************************************
 193.00        4000      **********************************************
 194.00        4000      **********************************************
 195.00        4000      **********************************************
 196.00        4000      **********************************************
 197.00        4000      **********************************************
 198.00        4000      **********************************************
 199.00        4000      **********************************************
 200.00        4000      **********************************************
 201.00        4000      **********************************************
 202.00        4000      **********************************************
 203.00        4000      **********************************************
 204.00        4000      **********************************************
 205.00        4000      **********************************************
 206.00        4000      **********************************************
 207.00        4000      **********************************************
 208.00        4000      **********************************************
 209.00        4000      **********************************************
 210.00        4000      **********************************************
 211.00        4000      **********************************************
 212.00        4000      **********************************************
 213.00        4000      **********************************************
 214.00        4000      **********************************************
 215.00        4000      **********************************************
 216.00        4000      **********************************************
 217.00        4000      **********************************************
 218.00        4000      **********************************************
 219.00        4000      **********************************************
 220.00        4000      **********************************************
 221.00        4000      **********************************************
 222.00        4000      **********************************************
 223.00        4000      **********************************************
 224.00        4000      **********************************************
 225.00        4000      **********************************************
 226.00        4000      **********************************************
 227.00        4000      **********************************************
 228.00        4000      **********************************************
 229.00        4000      **********************************************
 230.00        4000      **********************************************
 231.00        4000      **********************************************
 232.00        4000      **********************************************
 233.00        4000      **********************************************
 234.00        4000      **********************************************
 235.00        4000      **********************************************
 236.00        4000      **********************************************
 237.00        4000      **********************************************
 238.00        4000      **********************************************
 239.00        4000      **********************************************
 240.00        4000      **********************************************
 241.00        4000      **********************************************
 242.00        4000      **********************************************
 243.00        4000      **********************************************
 244.00        4000      **********************************************
 245.00        4000      **********************************************
 246.00        4000      **********************************************
 247.00        4000      **********************************************
 248.00        4000      **********************************************
 249.00        4000      **********************************************
 250.00        4000      **********************************************
 251.00        4000      **********************************************
 252.00        4000      **********************************************
 253.00        4000      **********************************************
 254.00        4000      **********************************************
 255.00        4000      **********************************************

 EXCLUDING PIXELS OF DN=0
 Average Gray Level =    128.000              Minimum =          0
 Standard Deviation =    73.6116              Maximum =        255
 Number of Pixels = 1020000

gen g 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g inc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           4      **********************************************     1
 3.0000           4*     **********************************************
 6.0000           4*     **********************************************
 9.0000           4*     **********************************************

 Average Gray Level =    4.50000              Minimum =          0
 Standard Deviation =    3.35410              Maximum =          9
 Number of Pixels =      16

let $echo="no"
Mean should be 4.5.
hist g inc=3 'exclude
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           4      **********************************************     1
 3.0000           4*     **********************************************
 6.0000           4*     **********************************************
 9.0000           4*     **********************************************

 EXCLUDING PIXELS OF DN=0
 Average Gray Level =    6.00000              Minimum =          0
 Standard Deviation =    2.44949              Maximum =          9
 Number of Pixels =      12

let $echo="no"
Mean should be 6.0.
gen g1515 15 15 linc=-1000 sinc=-1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-3969.1           1*     ****************
-2985.1           2*     *********************************
-2001.1           3*     **********************************************     1
-1017.1           2*     *********************************
 32.540           1*     ****************

 Average Gray Level =   -2000.00              Minimum =      -4000
 Standard Deviation =    1154.70              Maximum =          0
 Number of Pixels =       9


-19976.           1*     *************************
-18992.           2*     **********************************************     1
-18008.           1*     *************************

 Average Gray Level =   -19000.0              Minimum =     -20000
 Standard Deviation =    707.107              Maximum =     -18000
 Number of Pixels =       4

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-6002.7           1*     ****************
-5018.7           2*     *********************************
-3969.1           3*     **********************************************     1
-2985.1           2*     *********************************
-2001.1           1*     ****************

 Average Gray Level =   -4000.00              Minimum =      -6000
 Standard Deviation =    1154.70              Maximum =      -2000
 Number of Pixels =       9


-22009.           1*     *************************
-21025.           2*     **********************************************     1
-19976.           1*     *************************

 Average Gray Level =   -21000.0              Minimum =     -22000
 Standard Deviation =    707.107              Maximum =     -20000
 Number of Pixels =       4

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-26011.           1*     ****************
-23977.           1*     ****************
-22993.           1*     ****************
-22009.           1*     ****************
-21025.           1*     ****************
-19976.           2*     *********************************
-18992.           1*     ****************
-18008.           2*     *********************************
-17024.           2*     *********************************
-15974.           2*     *********************************
-14990.           2*     *********************************
-14006.           3*     **********************************************     1
-13022.           2*     *********************************
-11972.           3*     **********************************************
-10988.           2*     *********************************
-10004.           2*     *********************************
-9020.3           2*     *********************************
-7970.7           2*     *********************************
-6986.7           1*     ****************
-6002.7           2*     *********************************
-5018.7           1*     ****************
-3969.1           1*     ****************
-2985.1           1*     ****************
-2001.1           1*     ****************
 32.540           1*     ****************

 Average Gray Level =   -13000.0              Minimum =     -26000
 Standard Deviation =    6245.00              Maximum =          0
 Number of Pixels =      40

hist g1515 SPIKE=5
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-27979.           1*     ***
-26995.           2*     *******
-26011.           3*     ***********
-25027.           4*     ***************
-23977.           5*     *******************
-22993.           6*     ***********************
-22009.           7*     **************************
-21025.           8*     ******************************
-19976.           9*     **********************************
-18992.          10*     **************************************
-18008.          11*     ******************************************
-17024.          12*     **********************************************
-15974.          13*     **********************************************     4
-14990.          14*     **********************************************     2
-14006.          15*     **********************************************     1
-13022.          14*     **********************************************     3
-11972.          13*     **********************************************     5
-10988.          12*     **********************************************
-10004.          11*     ******************************************
-9020.3          10*     **************************************
-7970.7           9*     **********************************
-6986.7           8*     ******************************
-6002.7           7*     **************************
-5018.7           6*     ***********************
-3969.1           5*     *******************
-2985.1           4*     ***************
-2001.1           3*     ***********
-1017.1           2*     *******
 32.540           1*     ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

hist g1515 SPIKE=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-27979.           1*     ***
-26995.           2*     ******
-26011.           3*     **********
-25027.           4*     *************
-23977.           5*     ****************
-22993.           6*     ********************
-22009.           7*     ***********************
-21025.           8*     **************************
-19976.           9*     ******************************
-18992.          10*     *********************************
-18008.          11*     ************************************
-17024.          12*     ****************************************
-15974.          13*     *******************************************
-14990.          14*     **********************************************
-14006.          15*     **********************************************     1
-13022.          14*     **********************************************
-11972.          13*     *******************************************
-10988.          12*     ****************************************
-10004.          11*     ************************************
-9020.3          10*     *********************************
-7970.7           9*     ******************************
-6986.7           8*     **************************
-6002.7           7*     ***********************
-5018.7           6*     ********************
-3969.1           5*     ****************
-2985.1           4*     *************
-2001.1           3*     **********
-1017.1           2*     ******
 32.540           1*     ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

hist g1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

hist g1515 'nohist sigma=sd  mean=avg
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp AVG

-14000.0

disp SD

6110.10107422

gen g 10 10 linc=1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g spikes=9
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540          10*     **********************************************     1
 1016.5          10*     **********************************************     2
 2000.6          10*     **********************************************     3
 2984.6          10*     **********************************************     4
 3968.6           2*     ************
 4034.2           8      **********************************************     9
 5018.2          10*     **********************************************     5
 6002.2          10*     **********************************************     6
 6986.2          10*     **********************************************     7
 7970.2           3*     ******************
 8035.8           7      *******************************************
 9019.8          10*     **********************************************     8

 Average Gray Level =    4504.50              Minimum =          0
 Standard Deviation =    2872.28              Maximum =       9009
 Number of Pixels =     100

gen f1515 15 15 linc=-100000 sinc=-100000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist f1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT       8      **********************************************     1
 32.540           1*     ******

 Average Gray Level =   -200000.              Minimum =    -400000
 Standard Deviation =    115470.              Maximum =          0
 Number of Pixels =       9


< LOW LIMIT       4      **********************************************     1

 Average Gray Level =  -0.190000E+07          Minimum =   -2000000
 Standard Deviation =    70710.5              Maximum =   -1800000
 Number of Pixels =       4

hist f1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT       9      **********************************************     1

 Average Gray Level =   -400000.              Minimum =    -600000
 Standard Deviation =    115470.              Maximum =    -200000
 Number of Pixels =       9


< LOW LIMIT       4      **********************************************     1

 Average Gray Level =  -0.210000E+07          Minimum =   -2200000
 Standard Deviation =    70709.4              Maximum =   -2000000
 Number of Pixels =       4

hist f1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT      39      **********************************************     1
 32.540           1*     *

 Average Gray Level =  -0.130000E+07          Minimum =   -2600000
 Standard Deviation =    624500.              Maximum =          0
 Number of Pixels =      40

HIST f1515 SPIKE=5
Beginning VICAR task HIST
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT     224      **********************************************     1
 32.540           1*     **********************************************     2

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

HIST f1515 SPIKE=1
Beginning VICAR task HIST
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT     224      **********************************************     1
 32.540           1*

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

hist f1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

hist f1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp avg

-1400000.0

disp sd

611010.0625

gen r1515 15 15 linc=1.e8 sinc=1.e8 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist r1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*     ******
>HIGH LIMIT       8*     **********************************************     1

 Average Gray Level =   0.200000E+09          Minimum =         0.
 Standard Deviation =   0.115470E+09          Maximum =   0.400000E+09
 Number of Pixels =       9


>HIGH LIMIT       4*     **********************************************     1

 Average Gray Level =   0.190000E+10          Minimum =   0.180000E+10
 Standard Deviation =   0.707099E+08          Maximum =   0.200000E+10
 Number of Pixels =       4

hist r1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

>HIGH LIMIT       9*     **********************************************     1

 Average Gray Level =   0.400000E+09          Minimum =   0.200000E+09
 Standard Deviation =   0.115470E+09          Maximum =   0.600000E+09
 Number of Pixels =       9


>HIGH LIMIT       4*     **********************************************     1

 Average Gray Level =   0.210000E+10          Minimum =   0.200000E+10
 Standard Deviation =   0.707113E+08          Maximum =   0.220000E+10
 Number of Pixels =       4

hist r1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*     *
>HIGH LIMIT      39*     **********************************************     1

 Average Gray Level =   0.130000E+10          Minimum =         0.
 Standard Deviation =   0.624500E+09          Maximum =   0.260000E+10
 Number of Pixels =      40

hist r1515 SPIKE=5
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*     **********************************************     2
>HIGH LIMIT     224*     **********************************************     1

 Average Gray Level =   0.140000E+10          Minimum =         0.
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

hist r1515 SPIKE=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*
>HIGH LIMIT     224*     **********************************************     1

 Average Gray Level =   0.140000E+10          Minimum =         0.
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

hist r1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   0.140000E+10          Minimum =         0.
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

hist r1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   0.140000E+10          Minimum =         0.
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp AVG

1400000000.0

disp SD

611010112.0

gen r1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
 Try some wild numbers.  3000 bins.  225 are non-empty.
 Should skip all empty bins and put a * after DN to indicate skip.
hist r1515
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT     224      **********************************************     1
 32.540           1*

 Average Gray Level =  -0.950617E+14          Minimum =  -0.190123E+15
 Standard Deviation =   0.536055E+14          Maximum =         0.
 Number of Pixels =     225

gen x1515 NS=10 NL=10 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist x1515
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           1
 1.0000           3      **
 2.0000           6      ****
 3.0000          10      ******
 4.0000          15      **********
 5.0000          21      **************
 6.0000          28      ******************
 7.0000          36      ************************
 8.0000          45      ******************************
 9.0000          55      ************************************
 10.000          63      ******************************************
 11.000          69      **********************************************
 12.000          73      **********************************************
 13.000          75      **********************************************     1
 14.000          75      **********************************************
 15.000          73      **********************************************
 16.000          69      **********************************************
 17.000          63      ******************************************
 18.000          55      ************************************
 19.000          45      ******************************
 20.000          36      ************************
 21.000          28      ******************
 22.000          21      **************
 23.000          15      **********
 24.000          10      ******
 25.000           6      ****
 26.000           3      **
 27.000           1

 Average Gray Level =    13.5000              Minimum =          0
 Standard Deviation =    4.97494              Maximum =         27
 Number of Pixels =    1000

hist x1515 NB=4
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

     0.           1      *
 1.0000           3      ****
 2.0000           6      ********
 3.0000          10      *************
 4.0000          14      *******************
 5.0000          18      *************************
 6.0000          22      ******************************
 7.0000          26      ************************************
 8.0000          30      *****************************************
 9.0000          34      **********************************************
 10.000          36      **********************************************     1
 11.000          36      **********************************************
 12.000          34      **********************************************
 13.000          30      *****************************************
 14.000          26      ************************************
 15.000          22      ******************************
 16.000          18      *************************
 17.000          14      *******************
 18.000          10      *************
 19.000           6      ********
 20.000           3      ****
 21.000           1      *

 Average Gray Level =    10.5000              Minimum =          0
 Standard Deviation =    4.21307              Maximum =         21
 Number of Pixels =     400

gen x1515 337 364 ival=200 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist x1515
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 200.00      122668*     **********************************************     1

 Average Gray Level =    200.000              Minimum =        200
 Standard Deviation =         0.              Maximum =        200
 Number of Pixels =  122668

gen g1515 10 10 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 'nohis
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =         0.              Minimum =          0
 Standard Deviation =         0.              Maximum =          0
 Number of Pixels =     100

gen g1515 10 10 ival=0 linc=0 sinc=0 'real
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 'nohis
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =         0.              Minimum =         0.
 Standard Deviation =         0.              Maximum =         0.
 Number of Pixels =     100

let $echo="no"
$!-----------------------------------------------------------------------------
$ create tsthist.log_linux
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

let $autousage="none"
gen g1515 15 15
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           1      ****************
 1.0000           2      *********************************
 2.0000           3      ************************************************** 1
 3.0000           2      *********************************
 4.0000           1      ****************

 Average Gray Level =    2.00000              Minimum =          0
 Standard Deviation =    1.15470              Maximum =          4
 Number of Pixels =       9


 18.000           1*     *************************
 19.000           2      ************************************************** 1
 20.000           1      *************************

 Average Gray Level =    19.0000              Minimum =         18
 Standard Deviation =   0.707107              Maximum =         20
 Number of Pixels =       4

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 2.0000           1*     ****************
 3.0000           2      *********************************
 4.0000           3      ************************************************** 1
 5.0000           2      *********************************
 6.0000           1      ****************

 Average Gray Level =    4.00000              Minimum =          2
 Standard Deviation =    1.15470              Maximum =          6
 Number of Pixels =       9


 20.000           1*     *************************
 21.000           2      ************************************************** 1
 22.000           1      *************************

 Average Gray Level =    21.0000              Minimum =         20
 Standard Deviation =   0.707107              Maximum =         22
 Number of Pixels =       4

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           1      ****************
 2.0000           1*     ****************
 3.0000           1      ****************
 4.0000           1      ****************
 5.0000           1      ****************
 6.0000           2      *********************************
 7.0000           1      ****************
 8.0000           2      *********************************
 9.0000           2      *********************************
 10.000           2      *********************************
 11.000           2      *********************************
 12.000           3      ************************************************** 1
 13.000           2      *********************************
 14.000           3      **************************************************
 15.000           2      *********************************
 16.000           2      *********************************
 17.000           2      *********************************
 18.000           2      *********************************
 19.000           1      ****************
 20.000           2      *********************************
 21.000           1      ****************
 22.000           1      ****************
 23.000           1      ****************
 24.000           1      ****************
 26.000           1*     ****************

 Average Gray Level =    13.0000              Minimum =          0
 Standard Deviation =    6.24500              Maximum =         26
 Number of Pixels =      40

hist g1515 SPIKE=5
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           1      ***
 1.0000           2      *******
 2.0000           3      ***********
 3.0000           4      ***************
 4.0000           5      *******************
 5.0000           6      ***********************
 6.0000           7      **************************
 7.0000           8      ******************************
 8.0000           9      **********************************
 9.0000          10      **************************************
 10.000          11      ******************************************
 11.000          12      **********************************************
 12.000          13      ************************************************** 4
 13.000          14      ************************************************** 2
 14.000          15      ************************************************** 1
 15.000          14      ************************************************** 3
 16.000          13      ************************************************** 5
 17.000          12      **********************************************
 18.000          11      ******************************************
 19.000          10      **************************************
 20.000           9      **********************************
 21.000           8      ******************************
 22.000           7      **************************
 23.000           6      ***********************
 24.000           5      *******************
 25.000           4      ***************
 26.000           3      ***********
 27.000           2      *******
 28.000           1      ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

hist g1515 SPIKE=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           1      ***
 1.0000           2      ******
 2.0000           3      **********
 3.0000           4      *************
 4.0000           5      ****************
 5.0000           6      ********************
 6.0000           7      ***********************
 7.0000           8      **************************
 8.0000           9      ******************************
 9.0000          10      *********************************
 10.000          11      ************************************
 11.000          12      ****************************************
 12.000          13      *******************************************
 13.000          14      **********************************************
 14.000          15      ************************************************** 1
 15.000          14      **********************************************
 16.000          13      *******************************************
 17.000          12      ****************************************
 18.000          11      ************************************
 19.000          10      *********************************
 20.000           9      ******************************
 21.000           8      **************************
 22.000           7      ***********************
 23.000           6      ********************
 24.000           5      ****************
 25.000           4      *************
 26.000           3      **********
 27.000           2      ******
 28.000           1      ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

hist g1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =    14.0000              Minimum =          0
 Standard Deviation =    6.11010              Maximum =         28
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp avg

14.0

disp sd

6.11010074615

hist g1515 'screen spike=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

                1
               ***
               ***
              *****
             *******
            *********
           ***********
           ***********
          *************
         ***************
        *****************
       *******************
       *******************
      *********************
     ***********************
    *************************
   ***************************
   ***************************
  ****************************

           1    1    2    2
 0    5    0    5    0    5
       225PIXELS   RANGE   0- 28     MEAN  14.000     STD DEV   6.110
gen g 10 10 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g 'exclude
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000         100      ************************************************** 1

 EXCLUDING PIXELS OF DN=0
 Average Gray Level =    0.00000              Minimum =          0
 Standard Deviation =    0.00000              Maximum =          0
 Number of Pixels =       0

gen g 1024 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
Should get mean =128.0 because of exclude
hist g 'exclude
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000        4000      ************************************************** 1
 1.0000        4000      **************************************************
 2.0000        4000      **************************************************
 3.0000        4000      **************************************************
 4.0000        4000      **************************************************
 5.0000        4000      **************************************************
 6.0000        4000      **************************************************
 7.0000        4000      **************************************************
 8.0000        4000      **************************************************
 9.0000        4000      **************************************************
 10.000        4000      **************************************************
 11.000        4000      **************************************************
 12.000        4000      **************************************************
 13.000        4000      **************************************************
 14.000        4000      **************************************************
 15.000        4000      **************************************************
 16.000        4000      **************************************************
 17.000        4000      **************************************************
 18.000        4000      **************************************************
 19.000        4000      **************************************************
 20.000        4000      **************************************************
 21.000        4000      **************************************************
 22.000        4000      **************************************************
 23.000        4000      **************************************************
 24.000        4000      **************************************************
 25.000        4000      **************************************************
 26.000        4000      **************************************************
 27.000        4000      **************************************************
 28.000        4000      **************************************************
 29.000        4000      **************************************************
 30.000        4000      **************************************************
 31.000        4000      **************************************************
 32.000        4000      **************************************************
 33.000        4000      **************************************************
 34.000        4000      **************************************************
 35.000        4000      **************************************************
 36.000        4000      **************************************************
 37.000        4000      **************************************************
 38.000        4000      **************************************************
 39.000        4000      **************************************************
 40.000        4000      **************************************************
 41.000        4000      **************************************************
 42.000        4000      **************************************************
 43.000        4000      **************************************************
 44.000        4000      **************************************************
 45.000        4000      **************************************************
 46.000        4000      **************************************************
 47.000        4000      **************************************************
 48.000        4000      **************************************************
 49.000        4000      **************************************************
 50.000        4000      **************************************************
 51.000        4000      **************************************************
 52.000        4000      **************************************************
 53.000        4000      **************************************************
 54.000        4000      **************************************************
 55.000        4000      **************************************************
 56.000        4000      **************************************************
 57.000        4000      **************************************************
 58.000        4000      **************************************************
 59.000        4000      **************************************************
 60.000        4000      **************************************************
 61.000        4000      **************************************************
 62.000        4000      **************************************************
 63.000        4000      **************************************************
 64.000        4000      **************************************************
 65.000        4000      **************************************************
 66.000        4000      **************************************************
 67.000        4000      **************************************************
 68.000        4000      **************************************************
 69.000        4000      **************************************************
 70.000        4000      **************************************************
 71.000        4000      **************************************************
 72.000        4000      **************************************************
 73.000        4000      **************************************************
 74.000        4000      **************************************************
 75.000        4000      **************************************************
 76.000        4000      **************************************************
 77.000        4000      **************************************************
 78.000        4000      **************************************************
 79.000        4000      **************************************************
 80.000        4000      **************************************************
 81.000        4000      **************************************************
 82.000        4000      **************************************************
 83.000        4000      **************************************************
 84.000        4000      **************************************************
 85.000        4000      **************************************************
 86.000        4000      **************************************************
 87.000        4000      **************************************************
 88.000        4000      **************************************************
 89.000        4000      **************************************************
 90.000        4000      **************************************************
 91.000        4000      **************************************************
 92.000        4000      **************************************************
 93.000        4000      **************************************************
 94.000        4000      **************************************************
 95.000        4000      **************************************************
 96.000        4000      **************************************************
 97.000        4000      **************************************************
 98.000        4000      **************************************************
 99.000        4000      **************************************************
 100.00        4000      **************************************************
 101.00        4000      **************************************************
 102.00        4000      **************************************************
 103.00        4000      **************************************************
 104.00        4000      **************************************************
 105.00        4000      **************************************************
 106.00        4000      **************************************************
 107.00        4000      **************************************************
 108.00        4000      **************************************************
 109.00        4000      **************************************************
 110.00        4000      **************************************************
 111.00        4000      **************************************************
 112.00        4000      **************************************************
 113.00        4000      **************************************************
 114.00        4000      **************************************************
 115.00        4000      **************************************************
 116.00        4000      **************************************************
 117.00        4000      **************************************************
 118.00        4000      **************************************************
 119.00        4000      **************************************************
 120.00        4000      **************************************************
 121.00        4000      **************************************************
 122.00        4000      **************************************************
 123.00        4000      **************************************************
 124.00        4000      **************************************************
 125.00        4000      **************************************************
 126.00        4000      **************************************************
 127.00        4000      **************************************************
 128.00        4000      **************************************************
 129.00        4000      **************************************************
 130.00        4000      **************************************************
 131.00        4000      **************************************************
 132.00        4000      **************************************************
 133.00        4000      **************************************************
 134.00        4000      **************************************************
 135.00        4000      **************************************************
 136.00        4000      **************************************************
 137.00        4000      **************************************************
 138.00        4000      **************************************************
 139.00        4000      **************************************************
 140.00        4000      **************************************************
 141.00        4000      **************************************************
 142.00        4000      **************************************************
 143.00        4000      **************************************************
 144.00        4000      **************************************************
 145.00        4000      **************************************************
 146.00        4000      **************************************************
 147.00        4000      **************************************************
 148.00        4000      **************************************************
 149.00        4000      **************************************************
 150.00        4000      **************************************************
 151.00        4000      **************************************************
 152.00        4000      **************************************************
 153.00        4000      **************************************************
 154.00        4000      **************************************************
 155.00        4000      **************************************************
 156.00        4000      **************************************************
 157.00        4000      **************************************************
 158.00        4000      **************************************************
 159.00        4000      **************************************************
 160.00        4000      **************************************************
 161.00        4000      **************************************************
 162.00        4000      **************************************************
 163.00        4000      **************************************************
 164.00        4000      **************************************************
 165.00        4000      **************************************************
 166.00        4000      **************************************************
 167.00        4000      **************************************************
 168.00        4000      **************************************************
 169.00        4000      **************************************************
 170.00        4000      **************************************************
 171.00        4000      **************************************************
 172.00        4000      **************************************************
 173.00        4000      **************************************************
 174.00        4000      **************************************************
 175.00        4000      **************************************************
 176.00        4000      **************************************************
 177.00        4000      **************************************************
 178.00        4000      **************************************************
 179.00        4000      **************************************************
 180.00        4000      **************************************************
 181.00        4000      **************************************************
 182.00        4000      **************************************************
 183.00        4000      **************************************************
 184.00        4000      **************************************************
 185.00        4000      **************************************************
 186.00        4000      **************************************************
 187.00        4000      **************************************************
 188.00        4000      **************************************************
 189.00        4000      **************************************************
 190.00        4000      **************************************************
 191.00        4000      **************************************************
 192.00        4000      **************************************************
 193.00        4000      **************************************************
 194.00        4000      **************************************************
 195.00        4000      **************************************************
 196.00        4000      **************************************************
 197.00        4000      **************************************************
 198.00        4000      **************************************************
 199.00        4000      **************************************************
 200.00        4000      **************************************************
 201.00        4000      **************************************************
 202.00        4000      **************************************************
 203.00        4000      **************************************************
 204.00        4000      **************************************************
 205.00        4000      **************************************************
 206.00        4000      **************************************************
 207.00        4000      **************************************************
 208.00        4000      **************************************************
 209.00        4000      **************************************************
 210.00        4000      **************************************************
 211.00        4000      **************************************************
 212.00        4000      **************************************************
 213.00        4000      **************************************************
 214.00        4000      **************************************************
 215.00        4000      **************************************************
 216.00        4000      **************************************************
 217.00        4000      **************************************************
 218.00        4000      **************************************************
 219.00        4000      **************************************************
 220.00        4000      **************************************************
 221.00        4000      **************************************************
 222.00        4000      **************************************************
 223.00        4000      **************************************************
 224.00        4000      **************************************************
 225.00        4000      **************************************************
 226.00        4000      **************************************************
 227.00        4000      **************************************************
 228.00        4000      **************************************************
 229.00        4000      **************************************************
 230.00        4000      **************************************************
 231.00        4000      **************************************************
 232.00        4000      **************************************************
 233.00        4000      **************************************************
 234.00        4000      **************************************************
 235.00        4000      **************************************************
 236.00        4000      **************************************************
 237.00        4000      **************************************************
 238.00        4000      **************************************************
 239.00        4000      **************************************************
 240.00        4000      **************************************************
 241.00        4000      **************************************************
 242.00        4000      **************************************************
 243.00        4000      **************************************************
 244.00        4000      **************************************************
 245.00        4000      **************************************************
 246.00        4000      **************************************************
 247.00        4000      **************************************************
 248.00        4000      **************************************************
 249.00        4000      **************************************************
 250.00        4000      **************************************************
 251.00        4000      **************************************************
 252.00        4000      **************************************************
 253.00        4000      **************************************************
 254.00        4000      **************************************************
 255.00        4000      **************************************************

 EXCLUDING PIXELS OF DN=0
 Average Gray Level =    128.000              Minimum =          0
 Standard Deviation =    73.6116              Maximum =        255
 Number of Pixels = 1020000

gen g 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g inc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           4      ************************************************** 1
 3.0000           4*     **************************************************
 6.0000           4*     **************************************************
 9.0000           4*     **************************************************

 Average Gray Level =    4.50000              Minimum =          0
 Standard Deviation =    3.35410              Maximum =          9
 Number of Pixels =      16

let $echo="no"
Mean should be 4.5.
hist g inc=3 'exclude
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           4      ************************************************** 1
 3.0000           4*     **************************************************
 6.0000           4*     **************************************************
 9.0000           4*     **************************************************

 EXCLUDING PIXELS OF DN=0
 Average Gray Level =    6.00000              Minimum =          0
 Standard Deviation =    2.44949              Maximum =          9
 Number of Pixels =      12

let $echo="no"
Mean should be 6.0.
gen g1515 15 15 linc=-1000 sinc=-1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-3969.1           1*     ****************
-2985.1           2*     *********************************
-2001.1           3*     ************************************************** 1
-1017.1           2*     *********************************
 32.540           1*     ****************

 Average Gray Level =   -2000.00              Minimum =      -4000
 Standard Deviation =    1154.70              Maximum =          0
 Number of Pixels =       9


-19976.           1*     *************************
-18992.           2*     ************************************************** 1
-18008.           1*     *************************

 Average Gray Level =   -19000.0              Minimum =     -20000
 Standard Deviation =    707.107              Maximum =     -18000
 Number of Pixels =       4

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-6002.7           1*     ****************
-5018.7           2*     *********************************
-3969.1           3*     ************************************************** 1
-2985.1           2*     *********************************
-2001.1           1*     ****************

 Average Gray Level =   -4000.00              Minimum =      -6000
 Standard Deviation =    1154.70              Maximum =      -2000
 Number of Pixels =       9


-22009.           1*     *************************
-21025.           2*     ************************************************** 1
-19976.           1*     *************************

 Average Gray Level =   -21000.0              Minimum =     -22000
 Standard Deviation =    707.107              Maximum =     -20000
 Number of Pixels =       4

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-26011.           1*     ****************
-23977.           1*     ****************
-22993.           1*     ****************
-22009.           1*     ****************
-21025.           1*     ****************
-19976.           2*     *********************************
-18992.           1*     ****************
-18008.           2*     *********************************
-17024.           2*     *********************************
-15974.           2*     *********************************
-14990.           2*     *********************************
-14006.           3*     ************************************************** 1
-13022.           2*     *********************************
-11972.           3*     **************************************************
-10988.           2*     *********************************
-10004.           2*     *********************************
-9020.3           2*     *********************************
-7970.7           2*     *********************************
-6986.7           1*     ****************
-6002.7           2*     *********************************
-5018.7           1*     ****************
-3969.1           1*     ****************
-2985.1           1*     ****************
-2001.1           1*     ****************
 32.540           1*     ****************

 Average Gray Level =   -13000.0              Minimum =     -26000
 Standard Deviation =    6245.00              Maximum =          0
 Number of Pixels =      40

hist g1515 SPIKE=5
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-27979.           1*     ***
-26995.           2*     *******
-26011.           3*     ***********
-25027.           4*     ***************
-23977.           5*     *******************
-22993.           6*     ***********************
-22009.           7*     **************************
-21025.           8*     ******************************
-19976.           9*     **********************************
-18992.          10*     **************************************
-18008.          11*     ******************************************
-17024.          12*     **********************************************
-15974.          13*     ************************************************** 4
-14990.          14*     ************************************************** 2
-14006.          15*     ************************************************** 1
-13022.          14*     ************************************************** 3
-11972.          13*     ************************************************** 5
-10988.          12*     **********************************************
-10004.          11*     ******************************************
-9020.3          10*     **************************************
-7970.7           9*     **********************************
-6986.7           8*     ******************************
-6002.7           7*     **************************
-5018.7           6*     ***********************
-3969.1           5*     *******************
-2985.1           4*     ***************
-2001.1           3*     ***********
-1017.1           2*     *******
 32.540           1*     ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

hist g1515 SPIKE=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

-27979.           1*     ***
-26995.           2*     ******
-26011.           3*     **********
-25027.           4*     *************
-23977.           5*     ****************
-22993.           6*     ********************
-22009.           7*     ***********************
-21025.           8*     **************************
-19976.           9*     ******************************
-18992.          10*     *********************************
-18008.          11*     ************************************
-17024.          12*     ****************************************
-15974.          13*     *******************************************
-14990.          14*     **********************************************
-14006.          15*     ************************************************** 1
-13022.          14*     **********************************************
-11972.          13*     *******************************************
-10988.          12*     ****************************************
-10004.          11*     ************************************
-9020.3          10*     *********************************
-7970.7           9*     ******************************
-6986.7           8*     **************************
-6002.7           7*     ***********************
-5018.7           6*     ********************
-3969.1           5*     ****************
-2985.1           4*     *************
-2001.1           3*     **********
-1017.1           2*     ******
 32.540           1*     ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

hist g1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

hist g1515 'nohist sigma=sd  mean=avg
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   -14000.0              Minimum =     -28000
 Standard Deviation =    6110.10              Maximum =          0
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp AVG

-14000.0

disp SD

6110.10107422

gen g 10 10 linc=1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g spikes=9
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540          10*     ************************************************** 1
 1016.5          10*     ************************************************** 2
 2000.6          10*     ************************************************** 3
 2984.6          10*     ************************************************** 4
 3968.6           2*     ************
 4034.2           8      ************************************************** 9
 5018.2          10*     ************************************************** 5
 6002.2          10*     ************************************************** 6
 6986.2          10*     ************************************************** 7
 7970.2           3*     ******************
 8035.8           7      *******************************************
 9019.8          10*     ************************************************** 8

 Average Gray Level =    4504.50              Minimum =          0
 Standard Deviation =    2872.28              Maximum =       9009
 Number of Pixels =     100

gen f1515 15 15 linc=-100000 sinc=-100000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist f1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT       8      ************************************************** 1
 32.540           1*     ******

 Average Gray Level =   -200000.              Minimum =    -400000
 Standard Deviation =    115470.              Maximum =          0
 Number of Pixels =       9


< LOW LIMIT       4      ************************************************** 1

 Average Gray Level =  -0.190000E+07          Minimum =   -2000000
 Standard Deviation =    70710.7              Maximum =   -1800000
 Number of Pixels =       4

hist f1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT       9      ************************************************** 1

 Average Gray Level =   -400000.              Minimum =    -600000
 Standard Deviation =    115470.              Maximum =    -200000
 Number of Pixels =       9


< LOW LIMIT       4      ************************************************** 1

 Average Gray Level =  -0.210000E+07          Minimum =   -2200000
 Standard Deviation =    70710.7              Maximum =   -2000000
 Number of Pixels =       4

hist f1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT      39      ************************************************** 1
 32.540           1*     *

 Average Gray Level =  -0.130000E+07          Minimum =   -2600000
 Standard Deviation =    624500.              Maximum =          0
 Number of Pixels =      40

HIST f1515 SPIKE=5
Beginning VICAR task HIST
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT     224      ************************************************** 1
 32.540           1*     ************************************************** 2

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

HIST f1515 SPIKE=1
Beginning VICAR task HIST
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT     224      ************************************************** 1
 32.540           1*

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

hist f1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

hist f1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =  -0.140000E+07          Minimum =   -2800000
 Standard Deviation =    611010.              Maximum =          0
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp avg

-1400000.0

disp sd

611010.0625

gen r1515 15 15 linc=1.e8 sinc=1.e8 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist r1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*     ******
>HIGH LIMIT       8*     ************************************************** 1

 Average Gray Level =   0.200000E+09          Minimum =    0.00000
 Standard Deviation =   0.115470E+09          Maximum =   0.400000E+09
 Number of Pixels =       9


>HIGH LIMIT       4*     ************************************************** 1

 Average Gray Level =   0.190000E+10          Minimum =   0.180000E+10
 Standard Deviation =   0.707107E+08          Maximum =   0.200000E+10
 Number of Pixels =       4

hist r1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

>HIGH LIMIT       9*     ************************************************** 1

 Average Gray Level =   0.400000E+09          Minimum =   0.200000E+09
 Standard Deviation =   0.115470E+09          Maximum =   0.600000E+09
 Number of Pixels =       9


>HIGH LIMIT       4*     ************************************************** 1

 Average Gray Level =   0.210000E+10          Minimum =   0.200000E+10
 Standard Deviation =   0.707107E+08          Maximum =   0.220000E+10
 Number of Pixels =       4

hist r1515 linc=2 sinc=3
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*     *
>HIGH LIMIT      39*     ************************************************** 1

 Average Gray Level =   0.130000E+10          Minimum =    0.00000
 Standard Deviation =   0.624500E+09          Maximum =   0.260000E+10
 Number of Pixels =      40

hist r1515 SPIKE=5
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*     ************************************************** 2
>HIGH LIMIT     224*     ************************************************** 1

 Average Gray Level =   0.140000E+10          Minimum =    0.00000
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

hist r1515 SPIKE=1
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 32.540           1*
>HIGH LIMIT     224*     ************************************************** 1

 Average Gray Level =   0.140000E+10          Minimum =    0.00000
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

hist r1515 'nohist
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   0.140000E+10          Minimum =    0.00000
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

hist r1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =   0.140000E+10          Minimum =    0.00000
 Standard Deviation =   0.611010E+09          Maximum =   0.280000E+10
 Number of Pixels =     225

let $echo="no"
Print average and stdev variables
disp AVG

1400000000.0

disp SD

611010112.0

gen r1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
let $echo="no"
 Try some wild numbers.  3000 bins.  225 are non-empty.
 Should skip all empty bins and put a * after DN to indicate skip.
hist r1515
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

< LOW LIMIT     224      ************************************************** 1
 32.540           1*

 Average Gray Level =  -0.950617E+14          Minimum =  -0.190123E+15
 Standard Deviation =   0.536055E+14          Maximum =    0.00000
 Number of Pixels =     225

gen x1515 NS=10 NL=10 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist x1515
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           1
 1.0000           3      **
 2.0000           6      ****
 3.0000          10      ******
 4.0000          15      **********
 5.0000          21      **************
 6.0000          28      ******************
 7.0000          36      ************************
 8.0000          45      ******************************
 9.0000          55      ************************************
 10.000          63      ******************************************
 11.000          69      **********************************************
 12.000          73      ************************************************
 13.000          75      ************************************************** 1
 14.000          75      **************************************************
 15.000          73      ************************************************
 16.000          69      **********************************************
 17.000          63      ******************************************
 18.000          55      ************************************
 19.000          45      ******************************
 20.000          36      ************************
 21.000          28      ******************
 22.000          21      **************
 23.000          15      **********
 24.000          10      ******
 25.000           6      ****
 26.000           3      **
 27.000           1

 Average Gray Level =    13.5000              Minimum =          0
 Standard Deviation =    4.97494              Maximum =         27
 Number of Pixels =    1000

hist x1515 NB=4
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 0.0000           1      *
 1.0000           3      ****
 2.0000           6      ********
 3.0000          10      *************
 4.0000          14      *******************
 5.0000          18      *************************
 6.0000          22      ******************************
 7.0000          26      ************************************
 8.0000          30      *****************************************
 9.0000          34      ***********************************************
 10.000          36      ************************************************** 1
 11.000          36      **************************************************
 12.000          34      ***********************************************
 13.000          30      *****************************************
 14.000          26      ************************************
 15.000          22      ******************************
 16.000          18      *************************
 17.000          14      *******************
 18.000          10      *************
 19.000           6      ********
 20.000           3      ****
 21.000           1      *

 Average Gray Level =    10.5000              Minimum =          0
 Standard Deviation =    4.21307              Maximum =         21
 Number of Pixels =     400

gen x1515 337 364 ival=200 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist x1515
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 200.00      122668*     ************************************************** 1

 Average Gray Level =    200.000              Minimum =        200
 Standard Deviation =    0.00000              Maximum =        200
 Number of Pixels =  122668

gen g1515 10 10 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 'nohis
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =    0.00000              Minimum =          0
 Standard Deviation =    0.00000              Maximum =          0
 Number of Pixels =     100

gen g1515 10 10 ival=0 linc=0 sinc=0 'real
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 'nohis
Beginning VICAR task hist
*** p3/HIST version Sep 9 2015 ***

 Average Gray Level =    0.00000              Minimum =    0.00000
 Standard Deviation =    0.00000              Maximum =    0.00000
 Number of Pixels =     100

let $echo="no"
$ Return
$!#############################################################################
