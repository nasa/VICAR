$!****************************************************************************
$!
$! Build proc for MIPL module ratio0
$! VPACK Version 1.5, Wednesday, March 31, 1993, 14:50:08
$!
$! Execute by entering:		$ @ratio0
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
$ write sys$output "*** module ratio0 ***"
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
$   if F$SEARCH("ratio0.imake") .nes. ""
$   then
$      vimake ratio0
$      purge ratio0.bld
$   else
$      if F$SEARCH("ratio0.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ratio0
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ratio0.bld "STD"
$   else
$      @ratio0.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ratio0.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ratio0.com -
	-s ratio0.f -
	-p ratio0.pdf -
	-i ratio0.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ratio0.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C     VAX CONVERSION BY ASM, JAN 1984
C     REVISION TO HANDLE F2 BEING CONVERTED TO VICAR2
C     ADD 'NOCENTER OPTION    -- REA 4JUN86
C     REVISED TO UNIX/VICAR   -- REA 3SEP91
C
C**********************************************************************
C
      SUBROUTINE MAIN44
      INCLUDE 'pgminc'
      EXTERNAL RAT,DIFF,LNRAT,LNDIFF
      REAL RPAR(2)
      INTEGER IPAR(4),IPOP(500),JPOP(500),PARB(xprdim)
      LOGICAL XVPTST
      LOGICAL*1 QLOG,QDIFF,QDISP,QFILT,QCENTER
      CHARACTER*60 FUNCSTR,FUNCSTR2
      COMMON/PARB/PARB
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      VALUE(I) = FLOAT(I-1)/SCALE+BOTTOM
C
C	OPEN INPUT DATASETS
C
      CALL XVUNIT(INP1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INP1,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA','U_FORMAT',
     +		  'REAL',' ')
      CALL XVUNIT(INP2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INP2,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA','U_FORMAT',
     +		  'REAL',' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C
C     GET AND PROCESS THE PARAMETERS
C
      QLOG = XVPTST('LOG')
      QDISP = .NOT.XVPTST('NODISPLA') 
      QFILT = .NOT.XVPTST('NOFILTER')
      QDIFF = XVPTST('DIFFEREN')
      QCENTER = XVPTST('CENTER')
C							INCLUDE
      CALL XVPARM('INCLUDE',RPAR,NUM,IND,0)
      IF (QDIFF.AND.IND.NE.0) THEN
	  BOTTOM = -249.5
	  TOP = 250.5
      ELSE
	  BOTTOM = MIN(RPAR(1),RPAR(2))
	  TOP = MAX(RPAR(1),RPAR(2))
      END IF
C							ATMOSPHERIC CORRECTIONS
      CALL XVPARM('ATM1',ATM1,NUM,IND,0)
      CALL XVPARM('ATM2',ATM2,NUM,IND,0)
C							PERCENT SATURATION
      CALL XVPARM('PERCENT',SAT,NUM,IND,0)
      SAT = 0.01*SAT
C							THRESHOLD
      CALL XVPARM('THRESHOL',THRESH,NUM,IND,0)
      THRESH = 0.01*THRESH
C							SUBSAMPLING PARAMS
      CALL XVPARM('SAMPLE',SUBSAMP,NUM,IND,0)
      INC = 1.0/(0.01*SUBSAMP)
      CALL XVPARM('LINC',LINC,NUM,IND,0)
      IF (IND.EQ.0) INC=LINC
      IF (INC.LE.0) INC=1
C							AREA
      CALL XVPARM('AREA',IPAR,NUM,IND,0)
      IF (IND.EQ.0) THEN
	  ISL = IPAR(1)
	  ISS = IPAR(2)
	  NL = IPAR(3)
	  NS = IPAR(4)
      END IF
C
C	IF GIVEN PARAMETERS ARE UNREASONABLE, ADJUST THEM
C
      IF(QLOG.AND.TOP.LT.0.0) TOP=250.5
      IF(QLOG.AND.BOTTOM.LT.-255.) BOTTOM=-255.0
      IF(QLOG.AND..NOT.QDIFF.AND.BOTTOM.LE.0.0) BOTTOM=0.2
      IF(QLOG.AND.QDIFF) ATM1=ATM1+256.0
C
C
      II = 4*NS
      JJ = II
      CALL ZIA(IPOP,500)
      IF(QLOG) THEN
	  IF(.NOT.QDIFF) THEN
C                                                     LOG RAT
	      BOTTOM = ALOG(BOTTOM)
	      SCALE = 500.0/(ALOG(TOP)-BOTTOM)
	      CALL STACKA(4,LNRAT,2,II,JJ)
	  ELSE
C                                                     LOG DIFF
	      BOTTOM = ALOG(BOTTOM+256.0)
	      SCALE = 500.0/(ALOG(TOP+256.0)-BOTTOM)
              CALL STACKA(4,LNDIFF,2,II,JJ)
	  END IF
C                                                     RAT & DIFF
      ELSE
          SCALE = 500.0/(TOP-BOTTOM)
          IF(QDIFF) CALL STACKA(4,DIFF,2,II,JJ)
          IF(.NOT.QDIFF) CALL STACKA(4,RAT,2,II,JJ)
      END IF
C
C     FIND MAXIMUM POPULATED BIN; FILTER HISTOGRAM IF REQUESTED
C
      IF(.NOT.QFILT) THEN
	  CALL MVE(4,500,IPOP,JPOP,1,1)
          MAXMUM = 0
          DO I=1,500
              MAXMUM = MAX(MAXMUM,IPOP(I))
	  END DO
      ELSE
          JPOP(3) = IPOP(1)+IPOP(2)+IPOP(3)+IPOP(4)+IPOP(5)
          JPOP(2) = JPOP(3)-IPOP(5)+IPOP(1)
          JPOP(1) = JPOP(2)-IPOP(4)+IPOP(2)
          MAXMUM = MAX(JPOP(1),JPOP(2),JPOP(3))
          DO  I=4,498
              JPOP(I) = JPOP(I-1)-IPOP(I-3)+IPOP(I+2)
              MAXMUM = MAX(MAXMUM,JPOP(I))
	  END DO
          JPOP(499) = JPOP(498)-IPOP(496)+IPOP(500)
          JPOP(500) = JPOP(499)-IPOP(497)+IPOP(499)
          MAXMUM = MAX(MAXMUM,JPOP(499),JPOP(500))
      END IF
C
C     ZERO OUT ALL BINS POPULATED BELOW THRESHOLD LEVEL;
C     COMPUTE THE MEAN
C
      ICUT = FLOAT(MAXMUM)*THRESH+0.5
      ISUM = 0
      IWT = 0
      DO I=1,500
          IF(JPOP(I).LT.ICUT) JPOP(I)=0
          ISUM = ISUM+JPOP(I)
          IWT = IWT+JPOP(I)*I
      END DO
      MEAN = FLOAT(IWT)/FLOAT(ISUM)+0.5
C
C     COMPUTE THE HIGH AND LOW BINS THAT CORRESPOND TO THE REQUESTED
C     SATURATION LEVEL
C
      N = 0
      NSAT = MAX(ISUM*SAT,1.0)
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(I)
          I=I+1
      END DO
      IF (N.LT.NSAT) THEN
	 CALL XVMESSAGE(' SATURATION GREATER THAN 100 PERCENT',' ')
	 CALL ABEND
      ENDIF
      LOW = I-1
      N = 0
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(501-I)
          I=I+1
      END DO
      IF (N.LT.NSAT) THEN
	 CALL XVMESSAGE(' SATURATION GREATER THAN 100 PERCENT',' ')
	 CALL ABEND
      ENDIF
      IHI = 502-I
C
C     IF NEEDED, FORCE THE MEAN TO BE CENTERED (OUTPUT DN=128)
C     BY DECREASING THE SATURATION AT ONE END.
C
      IF (QCENTER) THEN
          M = MEAN-LOW
          N = IHI-MEAN
          IF(N.GT.M) LOW=MEAN-N
          IF(M.GT.N) IHI=MEAN+M
      END IF
C
C     COMPUTE THE GAIN AND OFFSET;  IF REQUESTED, CALL DISPLY TO
C     PRINT OUT THE HISTOGRAM
C
      VAL0 = VALUE(LOW)
      VAL255 = VALUE(IHI)
      GAIN = 255.0/(VAL255-VAL0)
      OFF = -GAIN*VAL0
      IF(QDISP) CALL DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,
     +    QDIFF,QLOG)
C
C	GENERATE THE STRING THAT CONTAINS THE FUNCTION TO BE PASSED 
C	TO F2.
C
      IF (ATM1.EQ.0.0 .AND.ATM2.EQ.0.0) THEN
	  IF (QLOG) THEN
	      IF (QDIFF) THEN
		  WRITE (FUNCSTR,110) GAIN,OFF
  110		  FORMAT('"',G13.6,'*ALOG(IN1-IN2)',SP,G13.6,'"')
	      ELSE
		  WRITE (FUNCSTR,120) GAIN,OFF
  120		  FORMAT('"',G13.6,'*ALOG(IN1/IN2)',SP,G13.6,'"')
	      END IF
	      N = 42
	  ELSE
	      IF (QDIFF) THEN
		  WRITE (FUNCSTR,210) GAIN,OFF
  210		  FORMAT('"',G13.6,'*(IN1-IN2)',SP,G13.6,'"')
	      ELSE
		  WRITE (FUNCSTR,220) GAIN,OFF
  220		  FORMAT('"',G13.6,'*(IN1/IN2)',SP,G13.6,'"')
	      END IF
	      N = 38
	  END IF
      ELSE
	  IF (QLOG) THEN
	      IF (QDIFF) THEN
		  WRITE (FUNCSTR,310) GAIN,ATM1,ATM2,OFF
  310 		  FORMAT('"',G13.6,'*ALOG((IN1',SP,F6.1,
     +			 ')-(IN2',F6.1,'))',G13.6,'"')
	      ELSE
		  WRITE (FUNCSTR,320) GAIN,ATM1,ATM2,OFF
  320 		  FORMAT('"',G13.6,'*ALOG((IN1',SP,F6.1,
     +			 ')/(IN2',F6.1,'))',G13.6,'"')
	      END IF
	      N = 58
	  ELSE
	      IF (QDIFF) THEN
		  WRITE (FUNCSTR,410) GAIN,ATM1,ATM2,OFF
  410 		  FORMAT('"',G13.6,'*((IN1',SP,F6.1,
     +			 ')-(IN2',F6.1,'))',G13.6,'"')
	      ELSE
		  WRITE (FUNCSTR,420) GAIN,ATM1,ATM2,OFF
  420 		  FORMAT('"',G13.6,'*((IN1',SP,F6.1,
     +			 ')/(IN2',F6.1,'))',G13.6,'"')
	      END IF
	      N = 54
	  END IF
      END IF
      CALL SQUEEZE(FUNCSTR,FUNCSTR2,N)
      CALL XVMESSAGE(FUNCSTR2,' ')
C
C	SEND THE FUNCTION BACK TO THE PARAMETER 'FUNC' FOR F2
C
      CALL XQINI(PARB,xprdim,xcont,ISTATUS)
      CALL XQSTR(PARB,'FUNC',1,FUNCSTR2,xadd,ISTATUS)
      CALL XQOUT(PARB,ISTATUS)
C
      RETURN
      END
C**********************************************************************
C
      SUBROUTINE RAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING RATIO HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
          DO J=1,NS
              X = XARR(J)+ATM1
              Y = YARR(J)+ATM2
              IF (Y.NE.0.0) THEN
	          N = SCALE*(X/Y-BOTTOM)+1.0
	          IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	      END IF
	  END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE DIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING DIFFERENCE HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      OFFSET = ATM1-ATM2-BOTTOM
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
          DO J=1,NS
	      N = SCALE*(XARR(J)-YARR(J)+OFFSET)+1.0
	      IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	  END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE LNRAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG RATIO HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
          DO J=1,NS
              X = XARR(J)+ATM1
              Y = YARR(J)+ATM2
              IF (X.GT.0.0.AND.Y.GT.0.0) THEN
	          N = SCALE*(ALOG(X/Y)-BOTTOM)+1.0
	          IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	      END IF
	  END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE LNDIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG DIFFERENCE HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON/QQ/ ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      OFFSET = ATM1-ATM2
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +		      ' ')
          DO J=1,NS
	      N = SCALE*(ALOG(XARR(J)-YARR(J)+OFFSET)-BOTTOM)+1.0
	      IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	  END DO
      END DO
      RETURN
      END
C**********************************************************************
      SUBROUTINE DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,QDIFF,QLOG
     +    )
C
C     ROUTINE FOR PRINTING HISTOGRAMS
C     IF FILTERING WAS PERFORMED, THIS IS THE FILTERED HISTOGRAM.
C
      CHARACTER*80 PRT
      CHARACTER*5 FILL
      INTEGER JPOP(500)
      LOGICAL*1 QDIFF,QLOG
C
      BOTTOM = BOTTOM+0.5/SCALE
C
C     PRINT THE PROPER HEADING
C
      IF (QDIFF) THEN
	  IF (QLOG) THEN
	      CALL XVMESSAGE('L O G   D I F F E R E N C E',' ')
	  ELSE
	      CALL XVMESSAGE('D I F F E R E N C E',' ')
	  END IF
      ELSE
	  IF (QLOG) THEN
	      CALL XVMESSAGE('L O G   R A T I O',' ')
	  ELSE
	      CALL XVMESSAGE('R A T I O',' ')
	  END IF
      END IF
      CALL XVMESSAGE(' ',' ')
C
C     PRINT A LINE FOR EACH POPULATED BIN, NORMALIZED TO THE MAXIMUM
C     POPULATED BIN
C
      DO I=1,500
          IF(JPOP(I).NE.0) THEN
	      X = FLOAT(I-1)/SCALE+BOTTOM
C
C     LABEL THE MEAN (OUTPUT=128DN) AND SATURATION LEVEL BINS.  IN SOME
C     CASES, ONE OF '0DN' AND '255DN' WILL BE OUTSIDE THE POPULATED RANG
C     OF THE HISTOGRAM, AND WILL NOT APPEAR.
C
	      FILL = '     '
              IF(I.EQ.LOW)  FILL = '  0DN'
      	      IF(I.EQ.MEAN) FILL = ' MEAN'
	      IF(I.EQ.IHI)  FILL = '255DN'
	      N = (50*JPOP(I))/MAXMUM + 1
	      WRITE (PRT,100) X,FILL,('*',J=1,N)
  100	      FORMAT(F12.3,3X,A5,2X,51A1)
	      CALL XVMESSAGE(PRT,' ')
	    ELSE IF (I.NE.1 .AND. JPOP(I-1).NE.0) THEN
	      CALL XVMESSAGE(' ',' ')
	  END IF
      END DO
      CALL QPRINT(' ',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create ratio0.pdf
PROCESS HELP=*
PARM INP TYPE=(STRING,40) COUNT=2
PARM OUT TYPE=(STRING,40) DEFAULT=""
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM CENTER TYPE=KEYWORD VALID=(CENTER,NOCENTER) DEFAULT=CENTER
PARM MODE TYPE=KEYWORD VALID=(RATIO,DIFFEREN) DEFAULT=RATIO
PARM MODE2 TYPE=KEYWORD VALID=(NOLOG,LOG) DEFAULT=NOLOG
PARM AREA TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SAMPLE TYPE=REAL DEFAULT=5.0
PARM LINC TYPE=INTEGER DEFAULT=20
PARM INCLUDE TYPE=REAL COUNT=2 DEFAULT=(0.0,5.0)
PARM THRESHOL TYPE=REAL DEFAULT=0.0
PARM MODE3 TYPE=KEYWORD VALID=(FILTER,NOFILTER) DEFAULT=FILTER
PARM PERCENT TYPE=REAL DEFAULT=2.0
PARM MODE4 TYPE=KEYWORD VALID=(DISPLAY,NODISPLA) DEFAULT=DISPLAY
PARM ATM1 TYPE=REAL DEFAULT=0.0
PARM ATM2 TYPE=REAL DEFAULT=0.0
PARM FUNC TYPE=NAME
END-PROC
.TITLE
Vicar Program RATIO0
.HELP
PURPOSE:
RATIO0 operates as part of the procedure RATIO. It uses a NAME type parameter,
and therefore should not be run outside of its procedure. For details of its
operation see RATIO.
$ Return
$!#############################################################################
$Imake_File:
$ create ratio0.imake
#define  PROGRAM   ratio0

#define MODULE_LIST ratio0.f

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
