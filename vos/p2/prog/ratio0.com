$!****************************************************************************
$!
$! Build proc for MIPL module ratio0
$! VPACK Version 1.9, Monday, September 26, 2011, 17:19:48
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
	-i ratio0.imake -
	-p ratio0.pdf -
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
C     TWEAKED TO PREVENT DIVIDE BY 0 FOR FLAT IMAGES.  UNCOMMENTED HISTOGRAM
C     PRINTING.               -- SXP 17SEP90
C     MSTP S/W Conversion (VICAR Porting)      CRI    10-JUL-95
C     14-Sep-2011 -lwk- added 'NSAMPS' to XVREADs to cover cases where NS<NSIN
C
C**********************************************************************
C
      SUBROUTINE MAIN44
      INCLUDE 'pgminc'        ! TAE CONSTANTS & PARAMETERS
      EXTERNAL RAT,DIFF,LNRAT,LNDIFF
      REAL 	RPAR(2)
      REAL*8	VAL0,VAL255,OFF,GAIN
      INTEGER 	IPAR(4),IPOP(500),JPOP(500),PARB(xprdim)
      LOGICAL 	XVPTST, QLOG,QDIFF,QDISP,QFILT,QCENTER
      character*50 unconfmt
      character*50 func, FUNCSTR
      EQUIVALENCE (FUNC,FUNCSTR)
      COMMON ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      VALUE(I) = FLOAT(I-1)/SCALE+BOTTOM       ! intrinsic function
C
      CALL IFMESSAGE('RATIO version 14-SEP-2011')
C
      CALL XVEACTION('SA',' ')
C
C	OPEN INPUT DATASETS
C
      CALL XVUNIT(INP1,'INP',1,ISTATUS,' ')
      write(func,'(i4)') istatus  
      CALL CHKSTAT(ISTATUS,' XVUNIT FAILED FOR FIRST INPUT DATASET',-1)
      CALL XVOPEN(INP1,ISTATUS,'U_FORMAT','REAL',' ')
      CALL XVUNIT(INP2,'INP',2,ISTATUS,' ')
      CALL CHKSTAT(ISTATUS,' XVUNIT FAILED FOR SECOND INPUT DATASET',-1)
      CALL XVOPEN(INP2,ISTATUS,'U_FORMAT','REAL',' ')
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
      CALL XVPARM('INCLUDE',RPAR,NUM,IND,2)
      IF (QDIFF.AND.IND.NE.0) THEN
	  BOTTOM = -249.5
	  TOP = 250.5
      ELSE
	  BOTTOM = MIN(RPAR(1),RPAR(2))
	  TOP = MAX(RPAR(1),RPAR(2))
      END IF
C							ATMOSPHERIC CORRECTIONS
      CALL XVPARM('ATM1',ATM1,NUM,IND,1)
      CALL XVPARM('ATM2',ATM2,NUM,IND,1)
C							PERCENT SATURATION
      CALL XVPARM('PERCENT',SAT,NUM,IND,1)
      SAT = 0.01*SAT
C							THRESHOLD
      CALL XVPARM('THRESHOL',THRESH,NUM,IND,1)
      THRESH = 0.01*THRESH
C							SUBSAMPLING PARAMS
      CALL XVPARM('SAMPLE',SUBSAMP,NUM,IND,1)
      INC = 1.0/(0.01*SUBSAMP)
      CALL XVPARM('LINC',LINC,NUM,IND,1)
      IF (IND.EQ.0) INC=LINC
      IF (INC.LE.0) INC=1
C							AREA
      CALL XVPARM('AREA',IPAR,NUM,IND,4)
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
      IF (N.LT.NSAT) CALL MABEND('SATURATION GREATER THAN 100 PERCENT')
      LOW = I-1
      N = 0
      I=1
      DO WHILE (I.LE.500 .AND. N.LT.NSAT)
          N = N+JPOP(501-I)
          I=I+1
      END DO
      IF (N.LT.NSAT) CALL MABEND('SATURATION GREATER THAN 100 PERCENT')
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

C...TWEAK FOR FLAT IMAGES TO PREVENT DIVIDE BY 0.

      IF ( VAL0 .EQ. VAL255)  THEN
         DELTA = VAL0 * .01
         IF (DELTA .EQ. 0)  DELTA=.01
         VAL0 = VAL0 - DELTA
         VAL255 = VAL255+DELTA
      END IF

      GAIN = 255.0/(VAL255-VAL0)
      OFF = -GAIN*VAL0
      IF(QDISP) CALL DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,
     +    QDIFF,QLOG)
C
C     GENERATE THE STRING THAT CONTAINS THE FUNCTION TO BE PASSED TO F2.
C
      func=' '
      M = 7-DLOG10(GAIN)
      FUNC(1:1) = '"'
      write(unconfmt,'(''(f10.'',i4.4,'')'')') m
      write(func(2:11),unconfmt) gain
      func(12:12) = '*'
      N = 13
      IF (QLOG) THEN
	  func(N:N+3) = 'ALOG'
	  N = N+4
      END IF
      FUNC(N:N) = '('
      N = N+1
      IF (ATM1.EQ.0.0) THEN
	      FUNC(N:N+2) = 'IN1'
	      N = N+3
	  ELSE
	      FUNC(N:N+3) = '(IN1'
	      write(FUNC(N+9-5:N+9),'(f6.1)') ATM1
	      IF (ATM1.GT.0.0) FUNC(N+4:N+4) = '+'
	      FUNC(N+10:N+10) = ')'
	      N = N+11
      END IF
      IF (QDIFF) THEN
	      FUNC(N:N) = '-'
	  ELSE
	      FUNC(N:N) = '/'
      END IF
      N = N+1
      IF (ATM2.EQ.0.0) THEN
	      FUNC(N:N+2) = 'IN2'
	      N = N+3
	  ELSE
	      FUNC(N:N+3) = '(IN2'
	      write(FUNC(N+9-5:N+9),'(f6.1)') ATM2
	      IF (ATM2.GT.0.0) FUNC(N+4:N+4) = '+'
	      FUNC(N+10:N+10) = ')'
	      N = N+11
      END IF
      FUNC(N:N) = ')'
      M = DLOG10(ABS(OFF)+1)+4
      write(unconfmt,'(''(f'',i4.4,''.1)'')') m
      write(FUNC(N+M-(M)+1:N+M),unconfmt) OFF
      IF (OFF.GE.0.0) FUNC(N+1:n+1)='+'
      FUNC(N+M+1:N+M+1) = '"'
      CALL XVMESSAGE('The ratio function is: ',' ' ) 
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(FUNC,' ')
      CALL XVMESSAGE(' ',' ')
C
C	SEND THE FUNCTION BACK TO THE PARAMETER 'FUNC' FOR F2
C
      CALL XQINI(PARB,xprdim,xabort)
      CALL XQSTR(PARB,'FUNC',1,FUNCSTR,xadd,ISTATUS)
      CALL XVQOUT(PARB,ISTATUS)
C
      RETURN
      END
C
C
C**********************************************************************
C
      SUBROUTINE RAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING RATIO HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      IF(JJ.NE.II) CALL MABEND('0INSUFFICIENT ROOM FOR STACKA BUFFERS')
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
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
C
C**********************************************************************
C
      SUBROUTINE DIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING DIFFERENCE HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      IF(JJ.NE.II) CALL MABEND('INSUFFICIENT ROOM FOR STACKA BUFFERS')
      OFFSET = ATM1-ATM2-BOTTOM
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
          DO J=1,NS
	      N = SCALE*(XARR(J)-YARR(J)+OFFSET)+1.0
	      IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	  END DO
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE LNRAT(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG RATIO HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      IF(JJ.NE.II) CALL MABEND('0INSUFFICIENT ROOM FOR STACKA BUFFERS')
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
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
C
C**********************************************************************
C
      SUBROUTINE LNDIFF(XARR,II,YARR,JJ)
C
C     ROUTINE FOR FORMING LOG DIFFERENCE HISTOGRAMS
C
      REAL XARR(II),YARR(JJ)
      INTEGER IPOP(500)
      COMMON ISL,ISS,NL,NS,INC,ATM1,ATM2,BOTTOM,SCALE,INP1,INP2,IPOP
      IF(JJ.NE.II) CALL MABEND('INSUFFICIENT ROOM FOR STACKA BUFFERS')
      OFFSET = ATM1-ATM2
      DO I=ISL,ISL+NL-1,INC
	  CALL XVREAD(INP1,XARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
	  CALL XVREAD(INP2,YARR,ISTATUS,'LINE',I,'NSAMPS',NS,' ')
          DO J=1,NS
	      N = SCALE*(ALOG(XARR(J)-YARR(J)+OFFSET)-BOTTOM)+1.0
	      IF(N.GE.1.AND.N.LE.500) IPOP(N) = IPOP(N)+1
	  END DO
      END DO
      RETURN
      END
C
C**********************************************************************
C
      SUBROUTINE DISPLY(SCALE,BOTTOM,LOW,MEAN,IHI,JPOP,MAXMUM,QDIFF,
     +                     QLOG)
C
C     ROUTINE FOR PRINTING HISTOGRAMS
C     IF FILTERING WAS PERFORMED, THIS IS THE FILTERED HISTOGRAM.
C
      INTEGER JPOP(500)
      character*132 buf, GRID
      LOGICAL QDIFF,QLOG                    !GRID(101)
      GRID=' '
      DO II=1,101,10
          GRID(II:II)='+'
      END DO
C
      BOTTOM = BOTTOM+0.5/SCALE
C
C     PRINT THE PROPER HEADING
C
      CALL XVMESSAGE(' ',' ')
      IF(.NOT.QDIFF.AND..NOT.QLOG) CALL XVMESSAGE('R A T I O',' ')
      IF(.NOT.QDIFF.AND.QLOG) CALL XVMESSAGE('L O G   R A T I O',' ')
      IF(QDIFF.AND..NOT.QLOG) CALL XVMESSAGE('D I F F E R E N C E',' ')
      IF(QDIFF.AND.QLOG) CALL XVMESSAGE('L O G   D I F F E R E N C E',
     +                                   ' ')
      buf(1:122) = ' '
      DO ii=0,100,10
          write(buf(22+ii-2:22+ii),'(i3)') ii
      END DO
c      BUF(1:1)='0'
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(BUF(1:122),' ')
      BUF(1:22) = ' '
C      CALL MVLC(GRID,BUF(23:123),101)
      BUF(22:122)=GRID(1:101)
C
C     PRINT A LINE FOR EACH POPULATED BIN, NORMALIZED TO THE MAXIMUM
C     POPULATED BIN
C
      DO I=1,500
          IF(JPOP(I).NE.0) THEN
	      X = FLOAT(I-1)/SCALE+BOTTOM
              write(buf(1:12),'(f12.3)') X
C
C     LABEL THE MEAN (OUTPUT=128DN) AND SATURATION LEVEL BINS.  IN SOME
C     CASES, ONE OF '0DN' AND '255DN' WILL BE OUTSIDE THE POPULATED RANG
C     OF THE HISTOGRAM, AND WILL NOT APPEAR.
C
              IF(I.EQ.LOW) buf(17:19) = '0DN'
      	      IF(I.EQ.MEAN) buf(16:19) = 'MEAN'
	      IF(I.EQ.IHI) buf(15:19) = '255DN'
	      BUF(22:122)=GRID(1:101)
	      N = (100*JPOP(I))/MAXMUM+1
	      do ic=22,22+N-1
		buf(ic:ic)='*'
	      enddo
              CALL XVMESSAGE(BUF(1:122),' ')
              buf(15:19) = '     '
	    ELSE IF (I.NE.1 .AND. JPOP(I-1).NE.0) THEN
	      CALL XVMESSAGE(' ',' ')
	  END IF
      END DO
      CALL XVMESSAGE(' ',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ratio0.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ratio

   To Create the build file give the command:

		% vimake ratio0			(Unix)


************************************************************************/


#define PROGRAM	ratio0
#define R2LIB

#define MODULE_LIST ratio0.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ratio0.pdf
PROCESS HELP=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING DEFAULT=""
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
Vicar Program ratio0
.HELP
PURPOSE:
ratio0 operates as part of the procedure ratio. It uses a NAME type parameter,
and therefore should not be run outside of its procedure. For details of its
operation see ratio.
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
tstratio
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
 SIZE version 06 Feb 2004
      INPUT AREA=(    1,    1,    5,    5)
     OUTPUT SIZE=     20 X     20
 Input data format=BYTE  Output data format=BYTE
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
 SIZE version 06 Feb 2004
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=     20 X     20
 Input data format=BYTE  Output data format=BYTE
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.005    0DN  *********************         +         +         +         +         +         +         +         +
       0.015         *********************         +         +         +         +         +         +         +         +
       0.025         ***********         +         +         +         +         +         +         +         +         +

       0.485         ***********         +         +         +         +         +         +         +         +         +
       0.495         ***********         +         +         +         +         +         +         +         +         +
       0.505         ***********         +         +         +         +         +         +         +         +         +
       0.515         ***********         +         +         +         +         +         +         +         +         +
       0.525         ***********         +         +         +         +         +         +         +         +         +

       0.645         ***********         +         +         +         +         +         +         +         +         +
       0.655         ***********         +         +         +         +         +         +         +         +         +
       0.665         ***********         +         +         +         +         +         +         +         +         +
       0.675         ***********         +         +         +         +         +         +         +         +         +
       0.685         ***********         +         +         +         +         +         +         +         +         +

       0.735         ***********         +         +         +         +         +         +         +         +         +
       0.745         ***********         +         +         +         +         +         +         +         +         +
       0.755         ***********         +         +         +         +         +         +         +         +         +
       0.765         ***********         +         +         +         +         +         +         +         +         +
       0.775         ***********         +         +         +         +         +         +         +         +         +
       0.785         ***********         +         +         +         +         +         +         +         +         +
       0.795         ***********         +         +         +         +         +         +         +         +         +
       0.805         ***********         +         +         +         +         +         +         +         +         +
       0.815         *********************         +         +         +         +         +         +         +         +
       0.825   MEAN  *********************         +         +         +         +         +         +         +         +
       0.835         *********************         +         +         +         +         +         +         +         +
       0.845         *********************         +         +         +         +         +         +         +         +
       0.855         *******************************         +         +         +         +         +         +         +
       0.865         *******************************         +         +         +         +         +         +         +
       0.875         *******************************         +         +         +         +         +         +         +
       0.885         *****************************************         +         +         +         +         +         +
       0.895         ***************************************************         +         +         +         +         +
       0.905         *************************************************************         +         +         +         +
       0.915         ***********************************************************************         +         +         +
       0.925         *****************************************************************************************************
       0.935         *******************************************************************************************         +
       0.945         *********************************************************************************         +         +
       0.955         *************************************************************         +         +         +         +
       0.965         *****************************************         +         +         +         +         +         +
       0.975         ***********         +         +         +         +         +         +         +         +         +


The ratio function is:

"  155.4878*(IN1/IN2)+0.0"

IF (OUT<>"") F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
 F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:18:59 2011
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
RATIO version 14-SEP-2011

L O G   R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
      -0.705    0DN  ***************     +         +         +         +         +         +         +         +         +
      -0.698         ***************     +         +         +         +         +         +         +         +         +
      -0.692         ***************     +         +         +         +         +         +         +         +         +
      -0.686         ***************     +         +         +         +         +         +         +         +         +
      -0.679         ***************     +         +         +         +         +         +         +         +         +

      -0.415         ***************     +         +         +         +         +         +         +         +         +
      -0.409         ***************     +         +         +         +         +         +         +         +         +
      -0.402         ***************     +         +         +         +         +         +         +         +         +
      -0.396         ***************     +         +         +         +         +         +         +         +         +
      -0.389         ***************     +         +         +         +         +         +         +         +         +

      -0.299         ***************     +         +         +         +         +         +         +         +         +
      -0.293         ***************     +         +         +         +         +         +         +         +         +
      -0.286         ***************     +         +         +         +         +         +         +         +         +
      -0.280         ***************     +         +         +         +         +         +         +         +         +
      -0.274         ***************     +         +         +         +         +         +         +         +         +

      -0.235         ***************     +         +         +         +         +         +         +         +         +
      -0.229         ***************     +         +         +         +         +         +         +         +         +
      -0.222         ***************     +         +         +         +         +         +         +         +         +
      -0.216         ***************     +         +         +         +         +         +         +         +         +
      -0.209         ***************     +         +         +         +         +         +         +         +         +

      -0.196         ***************     +         +         +         +         +         +         +         +         +
      -0.190         ***************     +         +         +         +         +         +         +         +         +
      -0.183         ***************     +         +         +         +         +         +         +         +         +
      -0.177         ***************     +         +         +         +         +         +         +         +         +
      -0.171         ***************     +         +         +         +         +         +         +         +         +
      -0.164         ***************     +         +         +         +         +         +         +         +         +
      -0.158   MEAN  ***************     +         +         +         +         +         +         +         +         +
      -0.151         ***************     +         +         +         +         +         +         +         +         +
      -0.145         ***************************** +         +         +         +         +         +         +         +
      -0.138         ***************************** +         +         +         +         +         +         +         +
      -0.132         ***************************** +         +         +         +         +         +         +         +
      -0.126         ***************************** +         +         +         +         +         +         +         +
      -0.119         *******************************************       +         +         +         +         +         +
      -0.113         ***************************** +         +         +         +         +         +         +         +
      -0.106         *******************************************       +         +         +         +         +         +
      -0.100         *******************************************       +         +         +         +         +         +
      -0.093         **********************************************************  +         +         +         +         +
      -0.087         **********************************************************  +         +         +         +         +
      -0.080         **************************************************************************************    +         +
      -0.074         **************************************************************************************    +         +
      -0.068         *****************************************************************************************************
      -0.061         *****************************************************************************************************
      -0.055         **************************************************************************************    +         +
      -0.048         **********************************************************  +         +         +         +         +
      -0.042         *******************************************       +         +         +         +         +         +
      -0.035         ***************     +         +         +         +         +         +         +         +         +


The ratio function is:

"  233.0006*ALOG(IN1/IN2)+165.0"

IF (OUT<>"") F2 (1,2) C  FUNC="  233.0006*ALOG(IN1/IN2)+165.0"
 F2 (1,2) C  FUNC="  233.0006*ALOG(IN1/IN2)+165.0"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:19:01 2011
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
RATIO version 14-SEP-2011

D I F F E R E N C E



                     0        10        20        30        40        50        60        70        80        90       100
      -2.000         ***************     +         +         +         +         +         +         +         +         +
      -1.000    0DN  ***************************** +         +         +         +         +         +         +         +
       0.000         *******************************************       +         +         +         +         +         +
       1.000         ************************************************************************        +         +         +
       2.000         **************************************************************************************    +         +
       3.000         **************************************************************************************    +         +
       4.000         *****************************************************************************************************
       5.000         *****************************************************************************************************
       6.000         **************************************************************************************    +         +
       7.000         *****************************************************************************************************
       8.000   MEAN  *****************************************************************************************************
       9.000         **************************************************************************************    +         +
      10.000         *****************************************************************************************************
      11.000         *****************************************************************************************************
      12.000         **************************************************************************************    +         +
      13.000         **************************************************************************************    +         +
      14.000         ************************************************************************        +         +         +
      15.000         *******************************************       +         +         +         +         +         +
      16.000         ***************************** +         +         +         +         +         +         +         +
      17.000  255DN  ***************     +         +         +         +         +         +         +         +         +


The ratio function is:

"  14.16667*(IN1-IN2)+21.2"

IF (OUT<>"") F2 (2,3) C  FUNC="  14.16667*(IN1-IN2)+21.2"
 F2 (2,3) C  FUNC="  14.16667*(IN1-IN2)+21.2"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 176 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:19:02 2011
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
RATIO version 14-SEP-2011

L O G   D I F F E R E N C E



                     0        10        20        30        40        50        60        70        80        90       100
       5.526    0DN  ****************    +         +         +         +         +         +         +         +         +
       5.535         ************************************    +         +         +         +         +         +         +
       5.544         ******************************************************************    +         +         +         +
       5.552         **************************************************************************************    +         +
       5.561   MEAN  *****************************************************************************************************
       5.570         **************************************************************************************    +         +
       5.579         ******************************************************************    +         +         +         +
       5.587         ************************************    +         +         +         +         +         +         +
       5.596  255DN  ****************    +         +         +         +         +         +         +         +         +


The ratio function is:

"  3659.000*ALOG((IN1+256.0)-IN2)-20204.6"

IF (OUT<>"") F2 (1,4) C  FUNC="  3659.000*ALOG((IN1+256.0)-IN2)-20204.6"
 F2 (1,4) C  FUNC="  3659.000*ALOG((IN1+256.0)-IN2)-20204.6"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 301 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:19:03 2011
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
RATIO version 14-SEP-2011
The ratio function is:

"  296.5116*(IN1/IN2)-47.4"

IF (OUT<>"") F2 (3,4)   FUNC="  296.5116*(IN1/IN2)-47.4"
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
RATIO version 14-SEP-2011
The ratio function is:

"  310.9756*(IN1/IN2)-56.0"

IF (OUT<>"") F2 (3,4)   FUNC="  310.9756*(IN1/IN2)-56.0"
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
RATIO version 14-SEP-2011
The ratio function is:

"  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"

IF (OUT<>"") F2 (3,4) C  FUNC="  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"
 F2 (3,4) C  FUNC="  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"
Beginning VICAR task F2
F2 version 26-Jul-11
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
---- Task: GENTHIS -- User: lwk -- Sat Sep 24 12:18:57 2011 ----
---- Task: SIZE -- User: lwk -- Sat Sep 24 12:18:57 2011 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: lwk -- Sat Sep 24 12:19:06 2011 ----
FUNCTION='  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8'
 
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
RATIO version 14-SEP-2011
The ratio function is:

"  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"

IF (OUT<>"") F2 (3,4) C  FUNC="  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"
 F2 (3,4) C  FUNC="  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"
Beginning VICAR task F2
F2 version 26-Jul-11
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
---- Task: GENTHIS -- User: lwk -- Sat Sep 24 12:18:57 2011 ----
---- Task: SIZE -- User: lwk -- Sat Sep 24 12:18:57 2011 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: lwk -- Sat Sep 24 12:19:07 2011 ----
FUNCTION='  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1'
 
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.005    0DN  *********************         +         +         +         +         +         +         +         +
       0.015         *********************         +         +         +         +         +         +         +         +
       0.025         ***********         +         +         +         +         +         +         +         +         +

       0.485         ***********         +         +         +         +         +         +         +         +         +
       0.495         ***********         +         +         +         +         +         +         +         +         +
       0.505         ***********         +         +         +         +         +         +         +         +         +
       0.515         ***********         +         +         +         +         +         +         +         +         +
       0.525         ***********         +         +         +         +         +         +         +         +         +

       0.645         ***********         +         +         +         +         +         +         +         +         +
       0.655         ***********         +         +         +         +         +         +         +         +         +
       0.665         ***********         +         +         +         +         +         +         +         +         +
       0.675         ***********         +         +         +         +         +         +         +         +         +
       0.685         ***********         +         +         +         +         +         +         +         +         +

       0.735         ***********         +         +         +         +         +         +         +         +         +
       0.745         ***********         +         +         +         +         +         +         +         +         +
       0.755         ***********         +         +         +         +         +         +         +         +         +
       0.765         ***********         +         +         +         +         +         +         +         +         +
       0.775         ***********         +         +         +         +         +         +         +         +         +
       0.785         ***********         +         +         +         +         +         +         +         +         +
       0.795         ***********         +         +         +         +         +         +         +         +         +
       0.805         ***********         +         +         +         +         +         +         +         +         +
       0.815         *********************         +         +         +         +         +         +         +         +
       0.825   MEAN  *********************         +         +         +         +         +         +         +         +
       0.835         *********************         +         +         +         +         +         +         +         +
       0.845         *********************         +         +         +         +         +         +         +         +
       0.855         *******************************         +         +         +         +         +         +         +
       0.865         *******************************         +         +         +         +         +         +         +
       0.875         *******************************         +         +         +         +         +         +         +
       0.885         *****************************************         +         +         +         +         +         +
       0.895         ***************************************************         +         +         +         +         +
       0.905         *************************************************************         +         +         +         +
       0.915         ***********************************************************************         +         +         +
       0.925         *****************************************************************************************************
       0.935         *******************************************************************************************         +
       0.945         *********************************************************************************         +         +
       0.955         *************************************************************         +         +         +         +
       0.965         *****************************************         +         +         +         +         +         +
       0.975         ***********         +         +         +         +         +         +         +         +         +


The ratio function is:

"  155.4878*(IN1/IN2)+0.0"

IF (OUT<>"") F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
 F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:19:08 2011
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.904         *******************************************************************   +         +         +         +
       0.912         *******************************************************************   +         +         +         +
       0.920         ******************************************************************************  +         +         +
       0.929   MEAN  *****************************************************************************************************
       0.937         *****************************************************************************************************
       0.945         ******************************************************************************  +         +         +
       0.953         *******************************************************************   +         +         +         +
       0.961         *********************************************     +         +         +         +         +         +
       0.970  255DN  ************        +         +         +         +         +         +         +         +         +


The ratio function is:

"  3109.758*(IN1/IN2)-2747.8"

IF (OUT<>"") F2 (1,2) C  FUNC="  3109.758*(IN1/IN2)-2747.8"
 F2 (1,2) C  FUNC="  3109.758*(IN1/IN2)-2747.8"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:19:10 2011
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.904         *******************************************************************   +         +         +         +
       0.912         *******************************************************************   +         +         +         +
       0.920         ******************************************************************************  +         +         +
       0.929   MEAN  *****************************************************************************************************
       0.937         *****************************************************************************************************
       0.945         ******************************************************************************  +         +         +
       0.953         *******************************************************************   +         +         +         +
       0.961  255DN  *********************************************     +         +         +         +         +         +
       0.970         ************        +         +         +         +         +         +         +         +         +


The ratio function is:

"  3887.196*(IN1/IN2)-3466.6"

IF (OUT<>"") F2 (1,2) C  FUNC="  3887.196*(IN1/IN2)-3466.6"
 F2 (1,2) C  FUNC="  3887.196*(IN1/IN2)-3466.6"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:18:56 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:19:11 2011
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
$!-----------------------------------------------------------------------------
$ create tstratio0.log_linux
tstratio
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
 SIZE version 06 Feb 2004
      INPUT AREA=(    1,    1,    5,    5)
     OUTPUT SIZE=     20 X     20
 Input data format=BYTE  Output data format=BYTE
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
 SIZE version 06 Feb 2004
      INPUT AREA=(    1,    1,   10,   10)
     OUTPUT SIZE=     20 X     20
 Input data format=BYTE  Output data format=BYTE
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.005    0DN  *********************         +         +         +         +         +         +         +         +
       0.015         *********************         +         +         +         +         +         +         +         +
       0.025         ***********         +         +         +         +         +         +         +         +         +

       0.485         ***********         +         +         +         +         +         +         +         +         +
       0.495         ***********         +         +         +         +         +         +         +         +         +
       0.505         ***********         +         +         +         +         +         +         +         +         +
       0.515         ***********         +         +         +         +         +         +         +         +         +
       0.525         ***********         +         +         +         +         +         +         +         +         +

       0.645         ***********         +         +         +         +         +         +         +         +         +
       0.655         ***********         +         +         +         +         +         +         +         +         +
       0.665         ***********         +         +         +         +         +         +         +         +         +
       0.675         ***********         +         +         +         +         +         +         +         +         +
       0.685         ***********         +         +         +         +         +         +         +         +         +

       0.735         ***********         +         +         +         +         +         +         +         +         +
       0.745         ***********         +         +         +         +         +         +         +         +         +
       0.755         ***********         +         +         +         +         +         +         +         +         +
       0.765         ***********         +         +         +         +         +         +         +         +         +
       0.775         ***********         +         +         +         +         +         +         +         +         +
       0.785         ***********         +         +         +         +         +         +         +         +         +
       0.795         ***********         +         +         +         +         +         +         +         +         +
       0.805         ***********         +         +         +         +         +         +         +         +         +
       0.815         *********************         +         +         +         +         +         +         +         +
       0.825   MEAN  *********************         +         +         +         +         +         +         +         +
       0.835         *********************         +         +         +         +         +         +         +         +
       0.845         *********************         +         +         +         +         +         +         +         +
       0.855         *******************************         +         +         +         +         +         +         +
       0.865         *******************************         +         +         +         +         +         +         +
       0.875         *******************************         +         +         +         +         +         +         +
       0.885         *****************************************         +         +         +         +         +         +
       0.895         ***************************************************         +         +         +         +         +
       0.905         *************************************************************         +         +         +         +
       0.915         ***********************************************************************         +         +         +
       0.925         *****************************************************************************************************
       0.935         *******************************************************************************************         +
       0.945         *********************************************************************************         +         +
       0.955         *************************************************************         +         +         +         +
       0.965         *****************************************         +         +         +         +         +         +
       0.975         ***********         +         +         +         +         +         +         +         +         +


The ratio function is:

"  155.4878*(IN1/IN2)+0.0"

IF (OUT<>"") F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
 F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
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
RATIO version 14-SEP-2011

L O G   R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
      -0.705    0DN  ***************     +         +         +         +         +         +         +         +         +
      -0.698         ***************     +         +         +         +         +         +         +         +         +
      -0.692         ***************     +         +         +         +         +         +         +         +         +
      -0.686         ***************     +         +         +         +         +         +         +         +         +
      -0.679         ***************     +         +         +         +         +         +         +         +         +

      -0.415         ***************     +         +         +         +         +         +         +         +         +
      -0.409         ***************     +         +         +         +         +         +         +         +         +
      -0.402         ***************     +         +         +         +         +         +         +         +         +
      -0.396         ***************     +         +         +         +         +         +         +         +         +
      -0.389         ***************     +         +         +         +         +         +         +         +         +

      -0.299         ***************     +         +         +         +         +         +         +         +         +
      -0.293         ***************     +         +         +         +         +         +         +         +         +
      -0.286         ***************     +         +         +         +         +         +         +         +         +
      -0.280         ***************     +         +         +         +         +         +         +         +         +
      -0.274         ***************     +         +         +         +         +         +         +         +         +

      -0.235         ***************     +         +         +         +         +         +         +         +         +
      -0.229         ***************     +         +         +         +         +         +         +         +         +
      -0.222         ***************     +         +         +         +         +         +         +         +         +
      -0.216         ***************     +         +         +         +         +         +         +         +         +
      -0.209         ***************     +         +         +         +         +         +         +         +         +

      -0.196         ***************     +         +         +         +         +         +         +         +         +
      -0.190         ***************     +         +         +         +         +         +         +         +         +
      -0.183         ***************     +         +         +         +         +         +         +         +         +
      -0.177         ***************     +         +         +         +         +         +         +         +         +
      -0.171         ***************     +         +         +         +         +         +         +         +         +
      -0.164         ***************     +         +         +         +         +         +         +         +         +
      -0.158   MEAN  ***************     +         +         +         +         +         +         +         +         +
      -0.151         ***************     +         +         +         +         +         +         +         +         +
      -0.145         ***************************** +         +         +         +         +         +         +         +
      -0.138         ***************************** +         +         +         +         +         +         +         +
      -0.132         ***************************** +         +         +         +         +         +         +         +
      -0.126         ***************************** +         +         +         +         +         +         +         +
      -0.119         *******************************************       +         +         +         +         +         +
      -0.113         ***************************** +         +         +         +         +         +         +         +
      -0.106         *******************************************       +         +         +         +         +         +
      -0.100         *******************************************       +         +         +         +         +         +
      -0.093         **********************************************************  +         +         +         +         +
      -0.087         **********************************************************  +         +         +         +         +
      -0.080         **************************************************************************************    +         +
      -0.074         **************************************************************************************    +         +
      -0.068         *****************************************************************************************************
      -0.061         *****************************************************************************************************
      -0.055         **************************************************************************************    +         +
      -0.048         **********************************************************  +         +         +         +         +
      -0.042         *******************************************       +         +         +         +         +         +
      -0.035         ***************     +         +         +         +         +         +         +         +         +


The ratio function is:

"  233.0006*ALOG(IN1/IN2)+165.0"

IF (OUT<>"") F2 (1,2) C  FUNC="  233.0006*ALOG(IN1/IN2)+165.0"
 F2 (1,2) C  FUNC="  233.0006*ALOG(IN1/IN2)+165.0"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
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
RATIO version 14-SEP-2011

D I F F E R E N C E



                     0        10        20        30        40        50        60        70        80        90       100
      -2.000    0DN  ***************     +         +         +         +         +         +         +         +         +
      -1.000         ***************************** +         +         +         +         +         +         +         +
       0.000         *******************************************       +         +         +         +         +         +
       1.000         ************************************************************************        +         +         +
       2.000         **************************************************************************************    +         +
       3.000         **************************************************************************************    +         +
       4.000         *****************************************************************************************************
       5.000         *****************************************************************************************************
       6.000         **************************************************************************************    +         +
       7.000         *****************************************************************************************************
       8.000   MEAN  *****************************************************************************************************
       9.000         **************************************************************************************    +         +
      10.000         *****************************************************************************************************
      11.000         *****************************************************************************************************
      12.000         **************************************************************************************    +         +
      13.000         **************************************************************************************    +         +
      14.000         ************************************************************************        +         +         +
      15.000         *******************************************       +         +         +         +         +         +
      16.000         ***************************** +         +         +         +         +         +         +         +
      17.000         ***************     +         +         +         +         +         +         +         +         +


The ratio function is:

"  12.75000*(IN1-IN2)+31.9"

IF (OUT<>"") F2 (2,3) C  FUNC="  12.75000*(IN1-IN2)+31.9"
 F2 (2,3) C  FUNC="  12.75000*(IN1-IN2)+31.9"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 176 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
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
RATIO version 14-SEP-2011

L O G   D I F F E R E N C E



                     0        10        20        30        40        50        60        70        80        90       100
       5.526    0DN  ****************    +         +         +         +         +         +         +         +         +
       5.535         ************************************    +         +         +         +         +         +         +
       5.544         ******************************************************************    +         +         +         +
       5.552         **************************************************************************************    +         +
       5.561   MEAN  *****************************************************************************************************
       5.570         **************************************************************************************    +         +
       5.579         ******************************************************************    +         +         +         +
       5.587         ************************************    +         +         +         +         +         +         +
       5.596  255DN  ****************    +         +         +         +         +         +         +         +         +


The ratio function is:

"  3658.980*ALOG((IN1+256.0)-IN2)-20204.5"

IF (OUT<>"") F2 (1,4) C  FUNC="  3658.980*ALOG((IN1+256.0)-IN2)-20204.5"
 F2 (1,4) C  FUNC="  3658.980*ALOG((IN1+256.0)-IN2)-20204.5"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 301 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
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
RATIO version 14-SEP-2011
The ratio function is:

"  296.5116*(IN1/IN2)-47.4"

IF (OUT<>"") F2 (3,4)   FUNC="  296.5116*(IN1/IN2)-47.4"
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
RATIO version 14-SEP-2011
The ratio function is:

"  310.9756*(IN1/IN2)-56.0"

IF (OUT<>"") F2 (3,4)   FUNC="  310.9756*(IN1/IN2)-56.0"
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
RATIO version 14-SEP-2011
The ratio function is:

"  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"

IF (OUT<>"") F2 (3,4) C  FUNC="  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"
 F2 (3,4) C  FUNC="  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8"
Beginning VICAR task F2
F2 version 26-Jul-11
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
---- Task: GENTHIS -- User: lwk -- Sat Sep 24 12:14:01 2011 ----
---- Task: SIZE -- User: lwk -- Sat Sep 24 12:14:01 2011 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: lwk -- Sat Sep 24 12:14:01 2011 ----
FUNCTION='  227.6786*((IN1+  0.5)/(IN2+  0.5))+22.8'
 
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
RATIO version 14-SEP-2011
The ratio function is:

"  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"

IF (OUT<>"") F2 (3,4) C  FUNC="  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"
 F2 (3,4) C  FUNC="  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1"
Beginning VICAR task F2
F2 version 26-Jul-11
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
---- Task: GENTHIS -- User: lwk -- Sat Sep 24 12:14:01 2011 ----
---- Task: SIZE -- User: lwk -- Sat Sep 24 12:14:01 2011 ----
COMMENT='PICTURE SIZE SCALED BY      4'
---- Task: F2 -- User: lwk -- Sat Sep 24 12:14:01 2011 ----
FUNCTION='  318.7500*((IN1+  0.5)/(IN2+  0.5))+35.1'
 
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.005    0DN  *********************         +         +         +         +         +         +         +         +
       0.015         *********************         +         +         +         +         +         +         +         +
       0.025         ***********         +         +         +         +         +         +         +         +         +

       0.485         ***********         +         +         +         +         +         +         +         +         +
       0.495         ***********         +         +         +         +         +         +         +         +         +
       0.505         ***********         +         +         +         +         +         +         +         +         +
       0.515         ***********         +         +         +         +         +         +         +         +         +
       0.525         ***********         +         +         +         +         +         +         +         +         +

       0.645         ***********         +         +         +         +         +         +         +         +         +
       0.655         ***********         +         +         +         +         +         +         +         +         +
       0.665         ***********         +         +         +         +         +         +         +         +         +
       0.675         ***********         +         +         +         +         +         +         +         +         +
       0.685         ***********         +         +         +         +         +         +         +         +         +

       0.735         ***********         +         +         +         +         +         +         +         +         +
       0.745         ***********         +         +         +         +         +         +         +         +         +
       0.755         ***********         +         +         +         +         +         +         +         +         +
       0.765         ***********         +         +         +         +         +         +         +         +         +
       0.775         ***********         +         +         +         +         +         +         +         +         +
       0.785         ***********         +         +         +         +         +         +         +         +         +
       0.795         ***********         +         +         +         +         +         +         +         +         +
       0.805         ***********         +         +         +         +         +         +         +         +         +
       0.815         *********************         +         +         +         +         +         +         +         +
       0.825   MEAN  *********************         +         +         +         +         +         +         +         +
       0.835         *********************         +         +         +         +         +         +         +         +
       0.845         *********************         +         +         +         +         +         +         +         +
       0.855         *******************************         +         +         +         +         +         +         +
       0.865         *******************************         +         +         +         +         +         +         +
       0.875         *******************************         +         +         +         +         +         +         +
       0.885         *****************************************         +         +         +         +         +         +
       0.895         ***************************************************         +         +         +         +         +
       0.905         *************************************************************         +         +         +         +
       0.915         ***********************************************************************         +         +         +
       0.925         *****************************************************************************************************
       0.935         *******************************************************************************************         +
       0.945         *********************************************************************************         +         +
       0.955         *************************************************************         +         +         +         +
       0.965         *****************************************         +         +         +         +         +         +
       0.975         ***********         +         +         +         +         +         +         +         +         +


The ratio function is:

"  155.4878*(IN1/IN2)+0.0"

IF (OUT<>"") F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
 F2 (1,2) C  FUNC="  155.4878*(IN1/IN2)+0.0"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:02 2011
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.904         *******************************************************************   +         +         +         +
       0.912         *******************************************************************   +         +         +         +
       0.920         ******************************************************************************  +         +         +
       0.929   MEAN  *****************************************************************************************************
       0.937         *****************************************************************************************************
       0.945         ******************************************************************************  +         +         +
       0.953         *******************************************************************   +         +         +         +
       0.961         *********************************************     +         +         +         +         +         +
       0.970  255DN  ************        +         +         +         +         +         +         +         +         +


The ratio function is:

"  3109.756*(IN1/IN2)-2747.8"

IF (OUT<>"") F2 (1,2) C  FUNC="  3109.756*(IN1/IN2)-2747.8"
 F2 (1,2) C  FUNC="  3109.756*(IN1/IN2)-2747.8"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:02 2011
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
RATIO version 14-SEP-2011

R A T I O



                     0        10        20        30        40        50        60        70        80        90       100
       0.904         *******************************************************************   +         +         +         +
       0.912         *******************************************************************   +         +         +         +
       0.920         ******************************************************************************  +         +         +
       0.929   MEAN  *****************************************************************************************************
       0.937         *****************************************************************************************************
       0.945         ******************************************************************************  +         +         +
       0.953         *******************************************************************   +         +         +         +
       0.961  255DN  *********************************************     +         +         +         +         +         +
       0.970         ************        +         +         +         +         +         +         +         +         +


The ratio function is:

"  3887.195*(IN1/IN2)-3466.6"

IF (OUT<>"") F2 (1,2) C  FUNC="  3887.195*(IN1/IN2)-3466.6"
 F2 (1,2) C  FUNC="  3887.195*(IN1/IN2)-3466.6"
Beginning VICAR task F2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 40 TIMES
END-PROC
list C
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sat Sep 24 12:14:01 2011
 Task:F2        User:lwk       Date_Time:Sat Sep 24 12:14:02 2011
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
