$!****************************************************************************
$!
$! Build proc for MIPL module ratio0
$! VPACK Version 2.1, Wednesday, September 09, 2015, 17:22:53
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
$ write sys$output "*** module ratio0 ***"
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
$ write sys$output "Invalid argument given to ratio0.com file -- ", primary
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
$ vpack ratio0.com -mixed -
	-s ratio0.f -
	-p ratio0.pdf -
	-i ratio0.imake -
	-t tstratio0.pdf tstratio0.log_solos tstratio0.log_linux
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

      call xvmessage( '*** p3/RATIO0 version Sep 9 2015 ***',' ')

C
C       OPEN INPUT DATASETS
C
      CALL XVUNIT(INP1,'INP',1,ISTATUS,' ')
      CALL XVOPEN(INP1,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA','U_FORMAT',
     +            'REAL',' ')
      CALL XVUNIT(INP2,'INP',2,ISTATUS,' ')
      CALL XVOPEN(INP2,ISTATUS,'OPEN_ACT','SA','IO_ACT','SA','U_FORMAT',
     +            'REAL',' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C
C     GET AND PROCESS THE PARAMETERS
C
      QLOG = XVPTST('LOG')
      QDISP = .NOT.XVPTST('NODISPLA') 
      QFILT = .NOT.XVPTST('NOFILTER')
      QDIFF = XVPTST('DIFFEREN')
      QCENTER = XVPTST('CENTER')
C                                                       INCLUDE
      CALL XVPARM('INCLUDE',RPAR,NUM,IND,0)
      IF (QDIFF.AND.IND.NE.0) THEN
          BOTTOM = -249.5
          TOP = 250.5
      ELSE
          BOTTOM = MIN(RPAR(1),RPAR(2))
          TOP = MAX(RPAR(1),RPAR(2))
      END IF
C                                                       ATMOSPHERIC CORRECTIONS
      CALL XVPARM('ATM1',ATM1,NUM,IND,0)
      CALL XVPARM('ATM2',ATM2,NUM,IND,0)
C                                                       PERCENT SATURATION
      CALL XVPARM('PERCENT',SAT,NUM,IND,0)
      SAT = 0.01*SAT
C                                                       THRESHOLD
      CALL XVPARM('THRESHOL',THRESH,NUM,IND,0)
      THRESH = 0.01*THRESH
C                                                       SUBSAMPLING PARAMS
      CALL XVPARM('SAMPLE',SUBSAMP,NUM,IND,0)
      INC = 1.0/(0.01*SUBSAMP)
      CALL XVPARM('LINC',LINC,NUM,IND,0)
      IF (IND.EQ.0) INC=LINC
      IF (INC.LE.0) INC=1
C                                                       AREA
      CALL XVPARM('AREA',IPAR,NUM,IND,0)
      IF (IND.EQ.0) THEN
          ISL = IPAR(1)
          ISS = IPAR(2)
          NL = IPAR(3)
          NS = IPAR(4)
      END IF
C
C       IF GIVEN PARAMETERS ARE UNREASONABLE, ADJUST THEM
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
C       GENERATE THE STRING THAT CONTAINS THE FUNCTION TO BE PASSED 
C       TO F2.
C
      IF (ATM1.EQ.0.0 .AND.ATM2.EQ.0.0) THEN
          IF (QLOG) THEN
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,110) GAIN,OFF
  110             FORMAT('"',G13.6,'*ALOG(IN1-IN2)',SP,G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,120) GAIN,OFF
  120             FORMAT('"',G13.6,'*ALOG(IN1/IN2)',SP,G13.6,'"')
              END IF
              N = 42
          ELSE
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,210) GAIN,OFF
  210             FORMAT('"',G13.6,'*(IN1-IN2)',SP,G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,220) GAIN,OFF
  220             FORMAT('"',G13.6,'*(IN1/IN2)',SP,G13.6,'"')
              END IF
              N = 38
          END IF
      ELSE
          IF (QLOG) THEN
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,310) GAIN,ATM1,ATM2,OFF
  310             FORMAT('"',G13.6,'*ALOG((IN1',SP,F6.1,
     +                   ')-(IN2',F6.1,'))',G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,320) GAIN,ATM1,ATM2,OFF
  320             FORMAT('"',G13.6,'*ALOG((IN1',SP,F6.1,
     +                   ')/(IN2',F6.1,'))',G13.6,'"')
              END IF
              N = 58
          ELSE
              IF (QDIFF) THEN
                  WRITE (FUNCSTR,410) GAIN,ATM1,ATM2,OFF
  410             FORMAT('"',G13.6,'*((IN1',SP,F6.1,
     +                   ')-(IN2',F6.1,'))',G13.6,'"')
              ELSE
                  WRITE (FUNCSTR,420) GAIN,ATM1,ATM2,OFF
  420             FORMAT('"',G13.6,'*((IN1',SP,F6.1,
     +                   ')/(IN2',F6.1,'))',G13.6,'"')
              END IF
              N = 54
          END IF
      END IF
      CALL SQUEEZE(FUNCSTR,FUNCSTR2,N)
      CALL XVMESSAGE(FUNCSTR2,' ')
C
C       SEND THE FUNCTION BACK TO THE PARAMETER 'FUNC' FOR F2
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
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
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
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
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
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
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
     +                ' ')
          CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'SAMP',ISS,'NSAMPS',NS,
     +                ' ')
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
  100         FORMAT(F12.3,3X,A5,2X,51A1)
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
.page
PROGRAM HISTORY

WRITTEN BY:
COGNIZANT PROGRAMMER:
REVISIONS:

  1984-01    ASM VAX Conversion
                 Revision to handle F2 being converted to VICAR2
  1986-06-04 REA Add 'NOCENTER OPTION
  1991-09-03 REA Revised to UNIX/VICAR
  2015-09-09 WLB Replaced xqout call with xvqout call to pass out vars to shell vicar
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
$Test_File:
$ create tstratio0.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

! NOTE THAT THIS PROC IS IDENTICAL TO TSTRATIO.PDF (IN RATIO.COM)

! RATIO TEST SCRIPT
! SET TERMINAL WIDTH=132 BEFORE RUNNING THIS SCRIPT
!
! CREATE 4 TEST IMAGES
gen 1 20 20
gen 2 20 20 IVAL=1
genthis tmp1 5 5 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,+
                     18,19,20,21,22,23,24,25)
size tmp1 3 zoom=4 'noin
genthis tmp2 10 10 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,+
                       18,19,20,21,22,23,24,25,26,27,28,29,30,31,+
                       32,33,34,35,36,37,38,39,40,41,42,43,44,45,+
                       46,47,48,49,50,51,52,53,54,55,56,57,58,59,+
                       60,61,62,63,64,65,66,67,68,69,70,71,72,73,+
                       74,75,76,77,78,79,80,81,82,83,84,85,86,87,+
                       88,89,90,91,92,93,94,95,96,97,98,99,100)
size tmp2 4 zoom=2 'noin
!
! RATIO OF 1 AND 2 WITH HISTOGRAM DISPLAY
ratio (1,2) C 
list C
!
! LOG RATIO OF 1 AND 2 WITH HISTOGRAM
ratio (1,2) C 'LOG
list C
!
! DIFFERENCE OF 2 AND 3 WITH HISTOGRAM
ratio (2,3) C 'DIFF
list C
!
! LOG DIFFERENCE OF 1 AND 4 WITH HISTOGRAM
ratio (1,4) C 'LOG 'DIFF
list C
!
! COMPUTATION ONLY (NO OUTPUT SET OR HISTOGRAM)
ratio (3,4) 'NODISP
! 
! COMPUTATION ONLY AND NO FILTERING
ratio (3,4) 'NODISP 'NOFILTER
!
! SAMPLING AREA SPECIFIED, ALONG WITH ATM CORRECTIONS
ratio (3,4) C AREA=(1,1,4,4) SAMPLE=100.0 ATM1=0.5 ATM2=0.5 'NODISP
label-list C
!
! ABOVE, WITH ALL LINES SAMPLES
ratio (3,4) C LINC=1 ATM1=0.5 ATM2=0.5 'NODISP 
label-list C
!
! RATIO BETWEEN 1 AND 2 WITH BOTTOM 10 PERCENT EXCLUDED
ratio (1,2) C THRESH=10.0
list C
!
! AS ABOVE, BUT INCLUDING ONLY THOSE RATIO'S ABOVE .90
ratio (1,2) C INCLUDE=(.90,5.0)
list C
!
! AS ABOVE, BUT WITH 10 PERCENT SATURATION
ratio (1,2) C INCLUDE=(.90,5.0) PERCENT=10.0
list C
! 
! END-SCRIPT
end-proc
$!-----------------------------------------------------------------------------
$ create tstratio0.log_solos
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

gen 1 20 20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen 2 20 20 IVAL=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
genthis tmp1 5 5 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, +
                     18,19,20,21,22,23,24,25)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size tmp1 3 zoom=4 'noin
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    5,    5)
     OUTPUT SIZE=     20 X     20
 PICTURE SIZE SCALED BY      4*NL,      4*NS
 SIZE task completed
genthis tmp2 10 10 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, +
                       18,19,20,21,22,23,24,25,26,27,28,29,30,31, +
                       32,33,34,35,36,37,38,39,40,41,42,43,44,45, +
                       46,47,48,49,50,51,52,53,54,55,56,57,58,59, +
                       60,61,62,63,64,65,66,67,68,69,70,71,72,73, +
                       74,75,76,77,78,79,80,81,82,83,84,85,86,87, +
                       88,89,90,91,92,93,94,95,96,97,98,99,100)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size tmp2 4 zoom=2 'noin
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=     20 X     20
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
ratio (1,2) C
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C     CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILT+
ER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.005     0DN  ***********
       0.015          ***********
       0.025          ******

       0.485          ******
       0.495          ******
       0.505          ******
       0.515          ******
       0.525          ******

       0.645          ******
       0.655          ******
       0.665          ******
       0.675          ******
       0.685          ******

       0.735          ******
       0.745          ******
       0.755          ******
       0.765          ******
       0.775          ******
       0.785          ******
       0.795          ******
       0.805          ******
       0.815          ***********
       0.825    MEAN  ***********
       0.835          ***********
       0.845          ***********
       0.855          ****************
       0.865          ****************
       0.875          ****************
       0.885          *********************
       0.895          **************************
       0.905          *******************************
       0.915          ************************************
       0.925          ***************************************************
       0.935          **********************************************
       0.945          *****************************************
       0.955          *******************************
       0.965          *********************
       0.975          ******


"155.488*(IN1/IN2)+0."
IF (OUT<>"") F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0."
 F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0."
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:55 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148
      2      78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148
      3     104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148
      4     117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149
      5     124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149
      6     130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149
      7     133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150
      8     136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150
      9     138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150
     10     140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150
     11     141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150
     12     143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150
     13     144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151
     14     144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151
     15     145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151
     16     146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151
     17     146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151
     18     147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151
     19     147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151
     20     148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151 152
ratio (1,2) C 'LOG
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C     CENTER=CENTER MODE=RATIO   MODE2=LOG MODE3=FILTER+
 MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
L O G   R A T I O

      -0.705     0DN  ********
      -0.698          ********
      -0.692          ********
      -0.686          ********
      -0.679          ********

      -0.415          ********
      -0.409          ********
      -0.402          ********
      -0.396          ********
      -0.389          ********

      -0.299          ********
      -0.293          ********
      -0.286          ********
      -0.280          ********
      -0.274          ********

      -0.235          ********
      -0.229          ********
      -0.222          ********
      -0.216          ********
      -0.209          ********

      -0.196          ********
      -0.190          ********
      -0.183          ********
      -0.177          ********
      -0.171          ********
      -0.164          ********
      -0.158    MEAN  ********
      -0.151          ********
      -0.145          ***************
      -0.138          ***************
      -0.132          ***************
      -0.126          ***************
      -0.119          **********************
      -0.113          ***************
      -0.106          **********************
      -0.100          **********************
      -0.093          *****************************
      -0.087          *****************************
      -0.080          *******************************************
      -0.074          *******************************************
      -0.068          ***************************************************
      -0.061          ***************************************************
      -0.055          *******************************************
      -0.048          *****************************
      -0.042          **********************
      -0.035          ********


"233.001*ALOG(IN1/IN2)+165.000"
IF (OUT<>"") F2 (1,2) C  FUNC="233.001*ALOG(IN1/IN2)+165.000"
 F2 (1,2) C  FUNC="233.001*ALOG(IN1/IN2)+165.000"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:55 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   3  71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153
      2       3  71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154
      3      71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154
      4      98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155
      5     113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155
      6     123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155
      7     129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156
      8     134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156
      9     138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157
     10     140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157
     11     143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157
     12     145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157
     13     146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158
     14     148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158
     15     149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158
     16     150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158
     17     151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158
     18     152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159
     19     152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159 159
     20     153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159 159 159
ratio (2,3) C 'DIFF
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (2,3) C     CENTER=CENTER MODE=DIFFEREN   MODE2=NOLOG MODE3=F+
ILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
D I F F E R E N C E

      -2.000          ********
      -1.000     0DN  ***************
       0.000          **********************
       1.000          ************************************
       2.000          *******************************************
       3.000          *******************************************
       4.000          ***************************************************
       5.000          ***************************************************
       6.000          *******************************************
       7.000          ***************************************************
       8.000    MEAN  ***************************************************
       9.000          *******************************************
      10.000          ***************************************************
      11.000          ***************************************************
      12.000          *******************************************
      13.000          *******************************************
      14.000          ************************************
      15.000          **********************
      16.000          ***************
      17.000   255DN  ********


"14.1667*(IN1-IN2)+21.2500"
IF (OUT<>"") F2 (2,3) C  FUNC="14.1667*(IN1-IN2)+21.2500"
 F2 (2,3) C  FUNC="14.1667*(IN1-IN2)+21.2500"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 176 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:55 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
      2      35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205 205 220 234 248
      3      50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220 220 234 248 255
      4      64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234 234 248 255 255
      5       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
      6      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
      7      35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205 205 220 234 248
      8      50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220 220 234 248 255
      9       0   7  21  35  35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205
     10       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
     11      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
     12      35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205 205 220 234 248
     13       0   0   7  21  21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191
     14       0   7  21  35  35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205
     15       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
     16      21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191 191 205 220 234
     17       0   0   0   7   7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177
     18       0   0   7  21  21  35  50  64  64  78  92 106 106 120 135 149 149 163 177 191
     19       0   7  21  35  35  50  64  78  78  92 106 120 120 135 149 163 163 177 191 205
     20       7  21  35  50  50  64  78  92  92 106 120 135 135 149 163 177 177 191 205 220
ratio (1,4) C 'LOG 'DIFF
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,4) C     CENTER=CENTER MODE=DIFFEREN   MODE2=LOG MODE3=FIL+
TER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
L O G   D I F F E R E N C E

       5.526     0DN  ********
       5.535          ******************
       5.544          *********************************
       5.552          *******************************************
       5.561    MEAN  ***************************************************
       5.570          *******************************************
       5.579          *********************************
       5.587          ******************
       5.596   255DN  ********


"3659.00*ALOG((IN1+256.0)-(IN2+0.0))-20204.6"
IF (OUT<>"") F2 (1,4) C  FUNC="3659.00*ALOG((IN1+256.0)-(IN2+0.0))-20204.6"
 F2 (1,4) C  FUNC="3659.00*ALOG((IN1+256.0)-(IN2+0.0))-20204.6"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 301 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:55 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1      71  85  85  99  99 114 114 128 128 142 142 156 156 170 170 184 184 198 198 212
      2      85  99  99 114 114 128 128 142 142 156 156 170 170 184 184 198 198 212 212 225
      3       0   0   0   0   0   0   0  13  13  28  28  42  42  57  57  71  71  85  85  99
      4       0   0   0   0   0  13  13  28  28  42  42  57  57  71  71  85  85  99  99 114
ratio (3,4) 'NODISP
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (3,4)      CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTE+
R MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"296.512*(IN1/IN2)-47.4419"
IF (OUT<>"") F2 (3,4)   FUNC="296.512*(IN1/IN2)-47.4419"
END-PROC
ratio (3,4) 'NODISP 'NOFILTER
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (3,4)      CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=NOFIL+
TER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"310.976*(IN1/IN2)-55.9756"
IF (OUT<>"") F2 (3,4)   FUNC="310.976*(IN1/IN2)-55.9756"
END-PROC
ratio (3,4) C AREA=(1,1,4,4) SAMPLE=100.0 ATM1=0.5 ATM2=0.5 'NODISP
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
 LET AREF="AREA=(1,1,4,4)"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
 LET FLD1=FLD1//" SAMPLE=1.000000000000e+02"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
 LET FLD2=FLD2//" ATM1=5.000000000000e-01"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
 LET FLD2=FLD2//" ATM2=5.000000000000e-01"
RATIO0 (3,4) C  AREA=(1,1,4,4)  SAMPLE=1.000000000000e+02  ATM1=5.00+
0000000000e-01 ATM2=5.000000000000e-01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"227.679*((IN1+0.5)/(IN2+0.5))+22.7679"
IF (OUT<>"") F2 (3,4) C  FUNC="227.679*((IN1+0.5)/(IN2+0.5))+22.7679"
 F2 (3,4) C  FUNC="227.679*((IN1+0.5)/(IN2+0.5))+22.7679"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 101 TIMES
END-PROC
label-list C
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GENTHIS -- User: wlb -- Wed Sep  9 17:20:54 2015 ----
---- Task: SIZE -- User: wlb -- Wed Sep  9 17:20:54 2015 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: wlb -- Wed Sep  9 17:20:56 2015 ----
FUNCTION='227.679*((IN1+0.5)/(IN2+0.5))+22.7679'
 
************************************************************
ratio (3,4) C LINC=1 ATM1=0.5 ATM2=0.5 'NODISP
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
 LET FLD1=FLD1//" LINC=1"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
 LET FLD2=FLD2//" ATM1=5.000000000000e-01"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
 LET FLD2=FLD2//" ATM2=5.000000000000e-01"
RATIO0 (3,4) C    LINC=1  ATM1=5.000000000000e-01 ATM2=5.00000000000+
0e-01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"318.750*((IN1+0.5)/(IN2+0.5))+35.0625"
IF (OUT<>"") F2 (3,4) C  FUNC="318.750*((IN1+0.5)/(IN2+0.5))+35.0625"
 F2 (3,4) C  FUNC="318.750*((IN1+0.5)/(IN2+0.5))+35.0625"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 101 TIMES
END-PROC
label-list C
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a SUN-SOLR host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GENTHIS -- User: wlb -- Wed Sep  9 17:20:54 2015 ----
---- Task: SIZE -- User: wlb -- Wed Sep  9 17:20:54 2015 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: wlb -- Wed Sep  9 17:20:56 2015 ----
FUNCTION='318.750*((IN1+0.5)/(IN2+0.5))+35.0625'
 
************************************************************
ratio (1,2) C THRESH=10.0
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
 LET FLD1=FLD1//" THRESH=1.000000000000e+01"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C    THRESH=1.000000000000e+01  CENTER=CENTER MODE=RATI+
O   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.005     0DN  ***********
       0.015          ***********
       0.025          ******

       0.485          ******
       0.495          ******
       0.505          ******
       0.515          ******
       0.525          ******

       0.645          ******
       0.655          ******
       0.665          ******
       0.675          ******
       0.685          ******

       0.735          ******
       0.745          ******
       0.755          ******
       0.765          ******
       0.775          ******
       0.785          ******
       0.795          ******
       0.805          ******
       0.815          ***********
       0.825    MEAN  ***********
       0.835          ***********
       0.845          ***********
       0.855          ****************
       0.865          ****************
       0.875          ****************
       0.885          *********************
       0.895          **************************
       0.905          *******************************
       0.915          ************************************
       0.925          ***************************************************
       0.935          **********************************************
       0.945          *****************************************
       0.955          *******************************
       0.965          *********************
       0.975          ******


"155.488*(IN1/IN2)+0."
IF (OUT<>"") F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0."
 F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0."
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:56 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148
      2      78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148
      3     104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148
      4     117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149
      5     124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149
      6     130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149
      7     133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150
      8     136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150
      9     138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150
     10     140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150
     11     141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150
     12     143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150
     13     144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151
     14     144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151
     15     145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151
     16     146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151
     17     146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151
     18     147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151
     19     147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151
     20     148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151 152
ratio (1,2) C INCLUDE=(.90,5.0)
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
 LET FLD1=FLD1//" INCLUDE=(9.000000000000e-01,5.000000000000e+00)"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C    INCLUDE=(9.000000000000e-01,5.000000000000e+00)  C+
ENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.904          **********************************
       0.912          **********************************
       0.920          ***************************************
       0.929    MEAN  ***************************************************
       0.937          ***************************************************
       0.945          ***************************************
       0.953          **********************************
       0.961          ***********************
       0.970   255DN  ******


"3109.76*(IN1/IN2)-2747.78"
IF (OUT<>"") F2 (1,2) C  FUNC="3109.76*(IN1/IN2)-2747.78"
 F2 (1,2) C  FUNC="3109.76*(IN1/IN2)-2747.78"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:56 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206
      2       0   0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214
      3       0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221
      4       0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227
      5       0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232
      6       0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238
      7       0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242
      8       0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247
      9      16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251
     10      51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255
     11      79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255
     12     103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255
     13     123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255
     14     140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255
     15     155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255
     16     168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255
     17     179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255
     18     189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255
     19     198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255 255
     20     206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255 255 255
ratio (1,2) C INCLUDE=(.90,5.0) PERCENT=10.0
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
 LET FLD1=FLD1//" INCLUDE=(9.000000000000e-01,5.000000000000e+00)"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
 LET FLD2=FLD2//" PERCENT=1.000000000000e+01"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C    INCLUDE=(9.000000000000e-01,5.000000000000e+00)  P+
ERCENT=1.000000000000e+01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.904          **********************************
       0.912          **********************************
       0.920          ***************************************
       0.929    MEAN  ***************************************************
       0.937          ***************************************************
       0.945          ***************************************
       0.953          **********************************
       0.961   255DN  ***********************
       0.970          ******


"3887.20*(IN1/IN2)-3466.60"
IF (OUT<>"") F2 (1,2) C  FUNC="3887.20*(IN1/IN2)-3466.60"
 F2 (1,2) C  FUNC="3887.20*(IN1/IN2)-3466.60"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:53 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:57 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226
      2       0   0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235
      3       0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244
      4       0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252
      5       0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255
      6       0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255
      7       0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255
      8       0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255
      9       0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255
     10      32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255
     11      67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255
     12      97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255
     13     122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255
     14     143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255
     15     161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255
     16     178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255
     17     192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255
     18     205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     19     216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     20     226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
end-proc
$!-----------------------------------------------------------------------------
$ create tstratio0.log_linux
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

gen 1 20 20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen 2 20 20 IVAL=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
genthis tmp1 5 5 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, +
                     18,19,20,21,22,23,24,25)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size tmp1 3 zoom=4 'noin
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,    5,    5)
     OUTPUT SIZE=     20 X     20
 PICTURE SIZE SCALED BY      4*NL,      4*NS
 SIZE task completed
genthis tmp2 10 10 dn=(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, +
                       18,19,20,21,22,23,24,25,26,27,28,29,30,31, +
                       32,33,34,35,36,37,38,39,40,41,42,43,44,45, +
                       46,47,48,49,50,51,52,53,54,55,56,57,58,59, +
                       60,61,62,63,64,65,66,67,68,69,70,71,72,73, +
                       74,75,76,77,78,79,80,81,82,83,84,85,86,87, +
                       88,89,90,91,92,93,94,95,96,97,98,99,100)
Beginning VICAR task genthis
 GENTHIS VERSION 2
 GENTHIS TASK COMPLETED
size tmp2 4 zoom=2 'noin
Beginning VICAR task size
 SIZE version 22 Aug 2013 (64-bit) - rjb
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=     20 X     20
 PICTURE SIZE SCALED BY      2*NL,      2*NS
 SIZE task completed
ratio (1,2) C
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C     CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILT+
ER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.005     0DN  ***********
       0.015          ***********
       0.025          ******

       0.485          ******
       0.495          ******
       0.505          ******
       0.515          ******
       0.525          ******

       0.645          ******
       0.655          ******
       0.665          ******
       0.675          ******
       0.685          ******

       0.735          ******
       0.745          ******
       0.755          ******
       0.765          ******
       0.775          ******
       0.785          ******
       0.795          ******
       0.805          ******
       0.815          ***********
       0.825    MEAN  ***********
       0.835          ***********
       0.845          ***********
       0.855          ****************
       0.865          ****************
       0.875          ****************
       0.885          *********************
       0.895          **************************
       0.905          *******************************
       0.915          ************************************
       0.925          ***************************************************
       0.935          **********************************************
       0.945          *****************************************
       0.955          *******************************
       0.965          *********************
       0.975          ******


"155.488*(IN1/IN2)+0.00000"
IF (OUT<>"") F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0.00000"
 F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0.00000"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148
      2      78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148
      3     104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148
      4     117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149
      5     124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149
      6     130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149
      7     133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150
      8     136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150
      9     138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150
     10     140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150
     11     141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150
     12     143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150
     13     144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151
     14     144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151
     15     145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151
     16     146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151
     17     146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151
     18     147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151
     19     147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151
     20     148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151 152
ratio (1,2) C 'LOG
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C     CENTER=CENTER MODE=RATIO   MODE2=LOG MODE3=FILTER+
 MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
L O G   R A T I O

      -0.705     0DN  ********
      -0.698          ********
      -0.692          ********
      -0.686          ********
      -0.679          ********

      -0.415          ********
      -0.409          ********
      -0.402          ********
      -0.396          ********
      -0.389          ********

      -0.299          ********
      -0.293          ********
      -0.286          ********
      -0.280          ********
      -0.274          ********

      -0.235          ********
      -0.229          ********
      -0.222          ********
      -0.216          ********
      -0.209          ********

      -0.196          ********
      -0.190          ********
      -0.183          ********
      -0.177          ********
      -0.171          ********
      -0.164          ********
      -0.158    MEAN  ********
      -0.151          ********
      -0.145          ***************
      -0.138          ***************
      -0.132          ***************
      -0.126          ***************
      -0.119          **********************
      -0.113          ***************
      -0.106          **********************
      -0.100          **********************
      -0.093          *****************************
      -0.087          *****************************
      -0.080          *******************************************
      -0.074          *******************************************
      -0.068          ***************************************************
      -0.061          ***************************************************
      -0.055          *******************************************
      -0.048          *****************************
      -0.042          **********************
      -0.035          ********


"233.001*ALOG(IN1/IN2)+165.000"
IF (OUT<>"") F2 (1,2) C  FUNC="233.001*ALOG(IN1/IN2)+165.000"
 F2 (1,2) C  FUNC="233.001*ALOG(IN1/IN2)+165.000"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   3  71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153
      2       3  71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154
      3      71  98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154
      4      98 113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155
      5     113 123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155
      6     123 129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155
      7     129 134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156
      8     134 138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156
      9     138 140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157
     10     140 143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157
     11     143 145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157
     12     145 146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157
     13     146 148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158
     14     148 149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158
     15     149 150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158
     16     150 151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158
     17     151 152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158
     18     152 152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159
     19     152 153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159 159
     20     153 154 154 155 155 155 156 156 157 157 157 157 158 158 158 158 158 159 159 159
ratio (2,3) C 'DIFF
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (2,3) C     CENTER=CENTER MODE=DIFFEREN   MODE2=NOLOG MODE3=F+
ILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
D I F F E R E N C E

      -2.000     0DN  ********
      -1.000          ***************
       0.000          **********************
       1.000          ************************************
       2.000          *******************************************
       3.000          *******************************************
       4.000          ***************************************************
       5.000          ***************************************************
       6.000          *******************************************
       7.000          ***************************************************
       8.000    MEAN  ***************************************************
       9.000          *******************************************
      10.000          ***************************************************
      11.000          ***************************************************
      12.000          *******************************************
      13.000          *******************************************
      14.000          ************************************
      15.000          **********************
      16.000          ***************
      17.000          ********


"12.7500*(IN1-IN2)+31.8750"
IF (OUT<>"") F2 (2,3) C  FUNC="12.7500*(IN1-IN2)+31.8750"
 F2 (2,3) C  FUNC="12.7500*(IN1-IN2)+31.8750"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 176 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1      32  45  57  70  70  83  96 108 108 121 134 147 147 159 172 185 185 198 210 223
      2      45  57  70  83  83  96 108 121 121 134 147 159 159 172 185 198 198 210 223 236
      3      57  70  83  96  96 108 121 134 134 147 159 172 172 185 198 210 210 223 236 249
      4      70  83  96 108 108 121 134 147 147 159 172 185 185 198 210 223 223 236 249 255
      5      19  32  45  57  57  70  83  96  96 108 121 134 134 147 159 172 172 185 198 210
      6      32  45  57  70  70  83  96 108 108 121 134 147 147 159 172 185 185 198 210 223
      7      45  57  70  83  83  96 108 121 121 134 147 159 159 172 185 198 198 210 223 236
      8      57  70  83  96  96 108 121 134 134 147 159 172 172 185 198 210 210 223 236 249
      9       6  19  32  45  45  57  70  83  83  96 108 121 121 134 147 159 159 172 185 198
     10      19  32  45  57  57  70  83  96  96 108 121 134 134 147 159 172 172 185 198 210
     11      32  45  57  70  70  83  96 108 108 121 134 147 147 159 172 185 185 198 210 223
     12      45  57  70  83  83  96 108 121 121 134 147 159 159 172 185 198 198 210 223 236
     13       0   6  19  32  32  45  57  70  70  83  96 108 108 121 134 147 147 159 172 185
     14       6  19  32  45  45  57  70  83  83  96 108 121 121 134 147 159 159 172 185 198
     15      19  32  45  57  57  70  83  96  96 108 121 134 134 147 159 172 172 185 198 210
     16      32  45  57  70  70  83  96 108 108 121 134 147 147 159 172 185 185 198 210 223
     17       0   0   6  19  19  32  45  57  57  70  83  96  96 108 121 134 134 147 159 172
     18       0   6  19  32  32  45  57  70  70  83  96 108 108 121 134 147 147 159 172 185
     19       6  19  32  45  45  57  70  83  83  96 108 121 121 134 147 159 159 172 185 198
     20      19  32  45  57  57  70  83  96  96 108 121 134 134 147 159 172 172 185 198 210
ratio (1,4) C 'LOG 'DIFF
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,4) C     CENTER=CENTER MODE=DIFFEREN   MODE2=LOG MODE3=FIL+
TER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
L O G   D I F F E R E N C E

       5.526     0DN  ********
       5.535          ******************
       5.544          *********************************
       5.552          *******************************************
       5.561    MEAN  ***************************************************
       5.570          *******************************************
       5.579          *********************************
       5.587          ******************
       5.596   255DN  ********


"3658.98*ALOG((IN1+256.0)-(IN2+0.0))-20204.5"
IF (OUT<>"") F2 (1,4) C  FUNC="3658.98*ALOG((IN1+256.0)-(IN2+0.0))-20204.5"
 F2 (1,4) C  FUNC="3658.98*ALOG((IN1+256.0)-(IN2+0.0))-20204.5"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 301 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1      71  85  85  99  99 114 114 128 128 142 142 156 156 170 170 184 184 198 198 212
      2      85  99  99 114 114 128 128 142 142 156 156 170 170 184 184 198 198 212 212 225
      3       0   0   0   0   0   0   0  13  13  28  28  42  42  56  56  71  71  85  85  99
      4       0   0   0   0   0  13  13  28  28  42  42  56  56  71  71  85  85  99  99 114
ratio (3,4) 'NODISP
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (3,4)      CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTE+
R MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"296.512*(IN1/IN2)-47.4419"
IF (OUT<>"") F2 (3,4)   FUNC="296.512*(IN1/IN2)-47.4419"
END-PROC
ratio (3,4) 'NODISP 'NOFILTER
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (3,4)      CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=NOFIL+
TER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"310.976*(IN1/IN2)-55.9756"
IF (OUT<>"") F2 (3,4)   FUNC="310.976*(IN1/IN2)-55.9756"
END-PROC
ratio (3,4) C AREA=(1,1,4,4) SAMPLE=100.0 ATM1=0.5 ATM2=0.5 'NODISP
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
 LET AREF="AREA=(1,1,4,4)"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
 LET FLD1=FLD1//" SAMPLE=1.000000000000e+02"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
 LET FLD2=FLD2//" ATM1=5.000000000000e-01"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
 LET FLD2=FLD2//" ATM2=5.000000000000e-01"
RATIO0 (3,4) C  AREA=(1,1,4,4)  SAMPLE=1.000000000000e+02  ATM1=5.00+
0000000000e-01 ATM2=5.000000000000e-01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"227.679*((IN1+0.5)/(IN2+0.5))+22.7679"
IF (OUT<>"") F2 (3,4) C  FUNC="227.679*((IN1+0.5)/(IN2+0.5))+22.7679"
 F2 (3,4) C  FUNC="227.679*((IN1+0.5)/(IN2+0.5))+22.7679"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 101 TIMES
END-PROC
label-list C
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GENTHIS -- User: wlb -- Wed Sep  9 17:20:18 2015 ----
---- Task: SIZE -- User: wlb -- Wed Sep  9 17:20:18 2015 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: wlb -- Wed Sep  9 17:20:18 2015 ----
FUNCTION='227.679*((IN1+0.5)/(IN2+0.5))+22.7679'
 
************************************************************
ratio (3,4) C LINC=1 ATM1=0.5 ATM2=0.5 'NODISP
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
 LET FLD1=FLD1//" LINC=1"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
 LET FLD2=FLD2//" ATM1=5.000000000000e-01"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
 LET FLD2=FLD2//" ATM2=5.000000000000e-01"
RATIO0 (3,4) C    LINC=1  ATM1=5.000000000000e-01 ATM2=5.00000000000+
0e-01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=NODISPLA FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
"318.750*((IN1+0.5)/(IN2+0.5))+35.0625"
IF (OUT<>"") F2 (3,4) C  FUNC="318.750*((IN1+0.5)/(IN2+0.5))+35.0625"
 F2 (3,4) C  FUNC="318.750*((IN1+0.5)/(IN2+0.5))+35.0625"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 101 TIMES
END-PROC
label-list C
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File C ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                20 lines per band
                20 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GENTHIS -- User: wlb -- Wed Sep  9 17:20:18 2015 ----
---- Task: SIZE -- User: wlb -- Wed Sep  9 17:20:18 2015 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: wlb -- Wed Sep  9 17:20:18 2015 ----
FUNCTION='318.750*((IN1+0.5)/(IN2+0.5))+35.0625'
 
************************************************************
ratio (1,2) C THRESH=10.0
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
 LET FLD1=FLD1//" THRESH=1.000000000000e+01"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C    THRESH=1.000000000000e+01  CENTER=CENTER MODE=RATI+
O   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.005     0DN  ***********
       0.015          ***********
       0.025          ******

       0.485          ******
       0.495          ******
       0.505          ******
       0.515          ******
       0.525          ******

       0.645          ******
       0.655          ******
       0.665          ******
       0.675          ******
       0.685          ******

       0.735          ******
       0.745          ******
       0.755          ******
       0.765          ******
       0.775          ******
       0.785          ******
       0.795          ******
       0.805          ******
       0.815          ***********
       0.825    MEAN  ***********
       0.835          ***********
       0.845          ***********
       0.855          ****************
       0.865          ****************
       0.875          ****************
       0.885          *********************
       0.895          **************************
       0.905          *******************************
       0.915          ************************************
       0.925          ***************************************************
       0.935          **********************************************
       0.945          *****************************************
       0.955          *******************************
       0.965          *********************
       0.975          ******


"155.488*(IN1/IN2)+0.00000"
IF (OUT<>"") F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0.00000"
 F2 (1,2) C  FUNC="155.488*(IN1/IN2)+0.00000"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148
      2      78 104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148
      3     104 117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148
      4     117 124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149
      5     124 130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149
      6     130 133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149
      7     133 136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150
      8     136 138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150
      9     138 140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150
     10     140 141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150
     11     141 143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150
     12     143 144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150
     13     144 144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151
     14     144 145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151
     15     145 146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151
     16     146 146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151
     17     146 147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151
     18     147 147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151
     19     147 148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151
     20     148 148 148 149 149 149 150 150 150 150 150 150 151 151 151 151 151 151 151 152
ratio (1,2) C INCLUDE=(.90,5.0)
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
 LET FLD1=FLD1//" INCLUDE=(9.000000000000e-01,5.000000000000e+00)"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C    INCLUDE=(9.000000000000e-01,5.000000000000e+00)  C+
ENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.904          **********************************
       0.912          **********************************
       0.920          ***************************************
       0.929    MEAN  ***************************************************
       0.937          ***************************************************
       0.945          ***************************************
       0.953          **********************************
       0.961          ***********************
       0.970   255DN  ******


"3109.76*(IN1/IN2)-2747.78"
IF (OUT<>"") F2 (1,2) C  FUNC="3109.76*(IN1/IN2)-2747.78"
 F2 (1,2) C  FUNC="3109.76*(IN1/IN2)-2747.78"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206
      2       0   0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214
      3       0   0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221
      4       0   0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227
      5       0   0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232
      6       0   0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238
      7       0   0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242
      8       0  16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247
      9      16  51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251
     10      51  79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255
     11      79 103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255
     12     103 123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255
     13     123 140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255
     14     140 155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255
     15     155 168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255
     16     168 179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255
     17     179 189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255
     18     189 198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255
     19     198 206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255 255
     20     206 214 221 227 232 238 242 247 251 255 255 255 255 255 255 255 255 255 255 255
ratio (1,2) C INCLUDE=(.90,5.0) PERCENT=10.0
LET FLD1 = ""
LET FLD2 = ""
LET SIZF = ""
LET AREF = ""
IF ((SIZE(3)<>0) OR (SIZE(4)<>0)) LET SIZF="SIZE=&SIZE"
IF ((AREA(3)<>0) OR (AREA(4)<>0)) LET AREF="AREA=&AREA"
IF (SAMPLE<>5.0) LET FLD1=FLD1//" SAMPLE=&SAMPLE"
IF (LINC<>20) LET FLD1=FLD1//" LINC=&LINC"
IF (INCLUDE(1)<>0.0 OR INCLUDE(2)<>5.0) LET FLD1=FLD1//" INCLUDE=&INCLUDE"
 LET FLD1=FLD1//" INCLUDE=(9.000000000000e-01,5.000000000000e+00)"
IF (THRESHOL<>0) LET FLD1=FLD1//" THRESH=&THRESHOL"
IF (PERCENT<>2.0) LET FLD2=FLD2//" PERCENT=&PERCENT"
 LET FLD2=FLD2//" PERCENT=1.000000000000e+01"
IF (ATM1<>0.0) LET FLD2=FLD2//" ATM1=&ATM1"
IF (ATM2<>0.0) LET FLD2=FLD2//" ATM2=&ATM2"
RATIO0 (1,2) C    INCLUDE=(9.000000000000e-01,5.000000000000e+00)  P+
ERCENT=1.000000000000e+01 CENTER=CENTER MODE=RATIO   MODE2=NOLOG MODE3=FILTER MODE4=DISPLAY FUNC=LFUNC
Beginning VICAR task RATIO0
*** p3/RATIO0 version Sep 9 2015 ***
R A T I O

       0.904          **********************************
       0.912          **********************************
       0.920          ***************************************
       0.929    MEAN  ***************************************************
       0.937          ***************************************************
       0.945          ***************************************
       0.953          **********************************
       0.961   255DN  ***********************
       0.970          ******


"3887.20*(IN1/IN2)-3466.60"
IF (OUT<>"") F2 (1,2) C  FUNC="3887.20*(IN1/IN2)-3466.60"
 F2 (1,2) C  FUNC="3887.20*(IN1/IN2)-3466.60"
Beginning VICAR task F2
F2 version 98-Aug-2015
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
 Task:F2        User:wlb       Date_Time:Wed Sep  9 17:20:18 2015
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0   0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226
      2       0   0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235
      3       0   0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244
      4       0   0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252
      5       0   0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255
      6       0   0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255
      7       0   0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255
      8       0   0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255
      9       0  32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255
     10      32  67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255
     11      67  97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255
     12      97 122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255
     13     122 143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255
     14     143 161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255
     15     161 178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255
     16     178 192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255
     17     192 205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255
     18     205 216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     19     216 226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
     20     226 235 244 252 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
end-proc
$ Return
$!#############################################################################
