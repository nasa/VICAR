$!****************************************************************************
$!
$! Build proc for MIPL module lave
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:15:11
$!
$! Execute by entering:		$ @lave
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
$ write sys$output "*** module lave ***"
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
$ write sys$output "Invalid argument given to lave.com file -- ", primary
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
$   if F$SEARCH("lave.imake") .nes. ""
$   then
$      vimake lave
$      purge lave.bld
$   else
$      if F$SEARCH("lave.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lave
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lave.bld "STD"
$   else
$      @lave.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lave.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lave.com -mixed -
	-s lave.f -
	-p lave.pdf -
	-i lave.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lave.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C -------------------------------------------------------------
C   8 FEB 01    REA   Clean up debris from screen plotting option
C   4 OCT 00    REA   Clean up scratch buffer usage
C  19 APR 91    REA   CONVERT TO SUN/UNIX
C  15 NOV 89    REA   ADDED NOPRINT KEYWORD
C   3 JUN 89    REA   ADDED PLOT
C  26 AUG 86    SP    MODIFIED FOR FR 19204 TO IMPROVE ACCURACY OF STDEV,
C                     (ESPECIALLY WHEN THE STDEV IS VERY SMALL), AND TO
C                     AVOID SQRT OF NEGATIVE NUMBERS.  BEFORE COMPUTING STDEV
C                     OF A SET OF NUMBERS, THE FIRST ELEMENT OF THE SET IS 
C                     SUBTRACTED FROM EVERY ELEMENT OF THE SET.  THIS SHOULD
C                     REDUCE THE EFFECT OF ROUNDING IN MOST CASES.
C  18 DEC 85    JRH   REMOVED THE FORMAT KEYWORD.  IF THE USER DID
C                     NOT SPECIFY THE FORMAT FOR DATA OTHER THAN BYTE,
C                     HAD POSSIBLE PRECISION ERRORS. 
C   8 JAN 85    JRH   CONVERTED TO VICAR2.  MODIFIED TO HANDLE
C                     FULLWORD AND REAL*4.  INCREASED THE MAXIMUM
C                     TITLE LENGTH TO BE 122.
C  30 OCT 84    SP    REMOVED LENGTH FOR QPRINT CALL FOR TITLE.
C  29 OCT 84    SP    MADE LENGTH OF TITLE ARRAY EQUAL TO 70.
C  29 OCT 84    SP    ADDED XVP CALL FOR PROCESSING TITLE PARAMETER.
C  29 OCT 84    SP    CORRECTED PROBLEM WHERE QSTDEV WAS NOT PASSED TO
C                     SUBROUTINE FILTER AS A PARAMETER.
C  25 OCT 84    SP    MERGED VAX VERSION WITH VERSION FROM CJL WITH STDEV.
C  25 OCT 84    SP    REWROTE PROUT2 TO HANDLE MULTIPLE OF 1024 VALUES.
C  25 OCT 84    SP    CHANGED PDF TO USE SEPARATE PARAMETERS INSTEAD OF
C                     USING MODE WITH COUNT=(1:4).
C   7 AUG 84    CJL   COMPUTE MEAN & STANDARD DEVIATION IN REAL*8
C  25 AUG 83    ASM   MODIFIED FOR VAX CONVERSION 
C  21 JAN 80    REA   FIX BUG IN EXCLUDED VALUE CALCULATION
C   2 AUG 79    REA   EXCLUDE,IMAGE,FILTER KEYWORDS,  STACKA
C   2 FEB 79    REA   LABEL 77  CONVENTION
C  15 MAY 78    MAG   INCREASE BUFFER SIZE
C  27 JUN 75    DAH   CHANGES FOR CONVERSION TO 360/OS
C  29 AUG 74          ROUND MODIFICATION
C  30 JUN 71    RMR61 ALLOWS LAVE TO BE USED UNDER VICAF 4.1
C
      EXTERNAL VERT,HORI
C
      COMMON /C11/ISL,ISS,NL,NS,NODS,NEXCL,NSW,INUNIT,OUTUNIT,TITLE,
     +            DATA,QEXCL,QIMAGE,QSTDEV,QAIS,QHIGH,QPLOT,QPR
C
      LOGICAL      XVPTST
      LOGICAL*1    QVERT/.FALSE./,QEXCL/.FALSE./,QIMAGE/.FALSE./,
     +             QSTDEV/.FALSE./,QAIS/.FALSE./
      LOGICAL*1    QHIGH/.FALSE./,QPLOT,QPR
      CHARACTER*124 TITLE
      INTEGER      ICNT,INUNIT,NEXCL,NL,NODS,NS,NSW,OUTUNIT,STATUS
      CHARACTER*5  AIS,DATA,IMAGE,STDEV
      CHARACTER*8  HIGH,MODE
      CHARACTER*45 LBUF
      NSW = 1
C
C     PROCESS THE PARAMETERS
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','REAL','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
      CALL XVP('OUT',LBUF,NODS)
      IF (NODS.NE.0) CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVGET(INUNIT,STATUS,'FORMAT',DATA,' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
C                                                     VERTICAL
      CALL XVP('MODE',MODE,ICNT)
      IF (MODE.EQ.'VERTICAL') QVERT=.TRUE.
C                                                     EXCLUDE
      CALL XVP('EXCLUDE',NEXCL,ICNT)
      IF (ICNT.NE.0) QEXCL = .TRUE.
C                                                     FILTER
      CALL XVP('FILTER',NSW,ICNT)
C                                                     IMAGE
      CALL XVP('IMAGE',IMAGE,ICNT)
      IF (ICNT.NE.0) QIMAGE=.TRUE.
C                                                     STANDARD DEVIATION
      CALL XVP('STDEV',STDEV,ICNT)
      IF (ICNT.NE.0) QSTDEV = .TRUE.
C                                                     AIS PRINT FORMAT
      CALL XVP('AIS',AIS,ICNT)
      IF (ICNT.NE.0) QAIS = .TRUE.
C                                                     HIGHPASS
      CALL XVP('HIGHPASS',HIGH,ICNT)
      IF (ICNT.NE.0) QHIGH=.TRUE.
C                                                     TITLE
      CALL XVP('TITLE', TITLE,ICNT)
C                                                     PLOT/PRINT
      QPLOT = XVPTST('PLOT')
      QPR = .NOT. XVPTST('NOPRINT')
C
C
      IF (QIMAGE.AND.(NODS.NE.0))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +            'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',' ')
      IF ((.NOT.QIMAGE).AND.(NODS.NE.0).AND.(MODE.EQ.'VERTICAL'))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',1,'U_NS',NS,
     +            'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',' ')
      IF ((.NOT.QIMAGE).AND.(NODS.NE.0).AND.(MODE.EQ.'HORIZONT'))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',1,'U_NS',NL,
     +            'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',' ')
C     POSITION INPUT DS FOR READING, COMPUTE SIZE OF ARRAYS, CALL STACKA 
C
      II = 4*NS
      IF(QVERT) THEN
          JJ = 4*NS
          KK = 1
          IF (QEXCL) KK=4*NS
          LL = 8*NS
          CALL STACKA(7,VERT,5,II,JJ,KK,LL,LL)
      ELSE
          JJ = 4*NL
          IF (II.LT.JJ) II=JJ
          CALL STACKA(4,HORI,2,II,JJ)
      END IF
      CALL XVMESSAGE(' LAVE PROCESSING COMPLETE',' ')
      CALL XVCLOSE(INUNIT,STATUS,' ')
      IF (NODS .NE. 0) CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END
C****************************************************************************
      SUBROUTINE HORI(IN,II,OUT,JJ)
C
C     THIS SUBROUTINE COMPUTES AVERAGES FOR EACH LINE
C
      COMMON /C11/ISL,ISS,NL,NS,NODS,NEXCL,NSW,INUNIT,OUTUNIT,TITLE,
     +            DATA,QEXCL,QIMAGE,QSTDEV,QAIS,QHIGH,QPLOT,QPR
C
      REAL*4      IN(NS),OUT(NL), IN1, XADJ
      REAL*8      SUM,SUM2,XNS, XDIFF
      INTEGER     INUNIT,OUTUNIT
      CHARACTER*124 TITLE
      LOGICAL*1   QEXCL,QIMAGE,QSTDEV,QAIS,QHIGH,QPLOT,QPR
      CHARACTER*5 DATA
C
      IF (JJ.NE.4*NL) THEN
	  CALL XVMESSAGE(' INSUFFICIENT CORE FOR STACKA',' ')
	  CALL ABEND
      ENDIF
      CALL ZIA(OUT,NL)
      K=0
      IF (QEXCL) GO TO 400
      IF (QSTDEV) GO TO 340
C
C     PROCESSING FOR NO EXCLUDED VALUE, MEAN COMPUTED
C
      XNS = NS
      DO 300 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      SUM = 0
      DO 200 J=1,NS
      SUM = SUM+IN(J)
  200 CONTINUE
      K = K+1
      OUT(K) = SUM/XNS
  300 CONTINUE
      GO TO 700
C
C   PROCESSING FOR NO EXCLUDED VALUE, STANDARD DEVIATION COMPUTED
C
  340 XNS = NS
      DO 380 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      IN1 = IN(1)
      SUM = 0
      SUM2 = 0
      DO 360 J=1,NS
      XADJ = IN(J) - IN1        ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
      SUM = SUM + XADJ
      SUM2 = SUM2 + XADJ*XADJ
  360 CONTINUE
      K = K+1
      XDIFF = SUM2-SUM*SUM/XNS
      IF ( XDIFF .LE. 0.0D0 )  THEN
         OUT(K) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
      ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
         OUT(K) = SNGL(DSQRT(XDIFF/XNS))
      END IF
  380 CONTINUE
      GO TO 700
C
C     EXCLUDED VALUE PROCESSING, MEAN COMPUTED
  400 CONTINUE
      IF (QSTDEV) GO TO 640
      DO 600 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      SUM = 0
      NUM = 0
      DO 500 J=1,NS
      IF (IN(J).EQ.NEXCL) GO TO 500
      SUM = SUM+IN(J)
      NUM = NUM+1
  500 CONTINUE
      IF (NUM.EQ.0) NUM=1
      K = K+1
      OUT(K) = SUM/NUM
  600 CONTINUE
      GO TO 700
C
C   PROCESSING FOR EXCLUDED VALUES, STANDARD DEVIATION COMPUTED
C
  640 DO 680 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      SUM = 0
      SUM2 = 0
      NUM = 0
      DO 660 J=1,NS
      IF (IN(J) .EQ. NEXCL) GO TO 660
      IF ( NUM .EQ. 0 )  IN1 = IN(J)
      XADJ = IN(J) - IN1        ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
      SUM = SUM + XADJ
      SUM2 = SUM2 + XADJ*XADJ
      NUM = NUM+1
  660 CONTINUE
      IF (NUM .EQ. 0) NUM=1
      K = K+1
      XDIFF = SUM2-SUM*SUM/NUM
      IF ( XDIFF .LE. 0.0D0 )  THEN
         OUT(K) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
      ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
         OUT(K) = SNGL(DSQRT(XDIFF/NUM))
      END IF
  680 CONTINUE
C
C     PRINT OUT RESULTS, WRITE OUTPUT DS
C
  700 CONTINUE
      IF (QAIS) THEN  
          IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
             CALL PROUT2(OUT,NL,PRTBUF,QSTDEV,TITLE,DATA,32,32,18,6)
          ELSE
             CALL PROUT2(OUT,NL,PRTBUF,QSTDEV,TITLE,DATA,32,32,9,11)
          END IF
      ELSE
          IF (QPR) CALL PROUT(OUT,NL,DATA,QSTDEV)
      END IF
      IF (NSW.NE.1) CALL FILTER(OUT,IN,NSW,NL,DATA,QHIGH,QSTDEV,QPR)
      IF (QPLOT) THEN
	  DO I=1,NL
	      IN(I) = ISL+I-1
	  END DO
	  CALL PLOTXY(IN,OUT,NL,'LINE  ',INUNIT)
      END IF
      IF (NODS.EQ.0) RETURN
      IF (QIMAGE) GO TO 800
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.
     +   (DATA.EQ.'FULL')) THEN
         DO 780 I=1,NL
         OUT(I) = ANINT(OUT(I))
  780    CONTINUE
      ENDIF
      CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NL,' ')
      RETURN
  800 CONTINUE
C
C     FULL IMAGE OUTPUT 
C
  900 CONTINUE
      DO 950 I=1,NL
      DO 920 J=1,NS
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.
     +   (DATA.EQ.'FULL')) THEN
         IN(J) = ANINT(OUT(I))
      ELSE   
         IN(J) = OUT(I)
      ENDIF
  920 CONTINUE
      CALL XVWRIT(OUTUNIT,IN,STATUS,'NSAMPS',NS,' ')
  950 CONTINUE
      RETURN
      END
C****************************************************************************
      SUBROUTINE VERT(IN,II,OUT,JJ,IPOP,KK,TOT,LL,TOT2,L2)
C
C     THIS SUBROUTINE COMPUTES AVERAGES FOR EACH SAMPLE
C
      COMMON /C11/ISL,ISS,NL,NS,NODS,NEXCL,NSW,INUNIT,OUTUNIT,TITLE,
     +            DATA,QEXCL,QIMAGE,QSTDEV,QAIS,QHIGH,QPLOT,QPR
C
      REAL*4      IN(NS),OUT(NS), XADJ
      REAL*8      TOT(NS),TOT2(NS),XNL, XDIFF
      INTEGER     INUNIT,OUTUNIT
      INTEGER     IPOP(NS)
      CHARACTER*132 PRTBUF
      CHARACTER*124 TITLE
      LOGICAL*1   QIMAGE,QEXCL,QSTDEV,QAIS,QHIGH,QPLOT,QPR
      CHARACTER*5 DATA
      IF (L2.NE.8*NS) THEN
	  CALL XVMESSAGE(' INSUFFICIENT CORE FOR STACKA',' ')
	  CALL ABEND
      ENDIF
      CALL ZIA(TOT,2*NS)
      CALL ZIA(TOT2,2*NS)
      IF (QEXCL) GO TO 500
      IF (QSTDEV) GO TO 420
C
C     NO EXCLUDED VALUE, MEAN COMPUTED
C
      DO 300 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      DO 200 J=1,NS
      TOT(J) = TOT(J)+IN(J)
  200 CONTINUE
  300 CONTINUE
      XNL = NL
      DO 400 I=1,NS
      OUT(I) = TOT(I)/XNL
  400 CONTINUE
      GO TO 800
C
C   NO EXCLUDED VALUE, STANDARD DEVIATION COMPUTED
C
  420 DO 460 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      IF ( I .EQ. ISL )  CALL MVE(7, NS, IN, OUT, 1, 1)
      DO 440 J=1,NS

      XADJ = IN(J) - OUT(J)     ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
      TOT(J) = TOT(J) + XADJ
      TOT2(J) = TOT2(J) + XADJ*XADJ
  440 CONTINUE
  460 CONTINUE
      XNL = NL
      DO 480 I=1,NS
      XDIFF = TOT2(I)-TOT(I)*TOT(I)/XNL
      IF ( XDIFF .LE. 0.0D0 )  THEN
         OUT(I) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
      ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
         OUT(I) = SNGL(DSQRT(XDIFF/XNL))
      END IF
  480 CONTINUE
      GO TO 800
C
C     EXCLUDED VALUE PROCESSING, MEAN COMPUTED
C
  500 CONTINUE
      CALL ZIA(IPOP,NS)
      CALL ZIA(OUT,NS)
      IF (QSTDEV) GO TO 760
      DO 700 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      DO 600 J=1,NS
      IF (IN(J).EQ.NEXCL) GO TO 600
      TOT(J) = TOT(J)+IN(J)
      IPOP(J) = IPOP(J)+1
  600 CONTINUE
  700 CONTINUE
      DO 750 I=1,NS
      IF (IPOP(I).NE.0) OUT(I) = TOT(I)/IPOP(I)
  750 CONTINUE
      GO TO 800
C
C   EXCLUDED VALUE PROCESSING, STANDARD DEVIATION COMPUTED
C
  760 CONTINUE
      DO 780 I=ISL,(NL+ISL-1)
      CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,' ')
      DO 770 J=1,NS
      IF (IN(J).EQ.NEXCL) GO TO 770
      IF ( IPOP(J) .EQ. 0 )  OUT(J) = IN(J)   ! STORE FIRST ELEMENT IN OUT.
      XADJ = IN(J) - OUT(J)     ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
      TOT(J) = TOT(J) + XADJ
      TOT2(J) = TOT2(J) + XADJ*XADJ
      IPOP(J) = IPOP(J)+1
  770 CONTINUE
  780 CONTINUE
      DO 790 I=1,NL
      IF ( IPOP(I) .GT. 0 )   THEN
        XDIFF = TOT2(I)-TOT(I)*TOT(I)/ IPOP(I)
      ELSE
        XDIFF = 0.0D0
      END IF
      IF ( XDIFF .LE. 0.0D0 )  THEN
         OUT(I) = 0.0                   ! XDIFF SHOULD BE THEORETICALLY >=0. IF
      ELSE                              ! ROUNDOFF MAKES IT <0, SET TO 0.
         OUT(I) = SNGL(DSQRT(XDIFF/IPOP(I) ))
      END IF
  790 CONTINUE
C
C     PRINT OUT RESULTS, WRITE OUTPUT DS
C
  800 CONTINUE
      IF (QAIS) THEN  
          IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
             CALL PROUT2(OUT,NS,PRTBUF,QSTDEV,TITLE,DATA,32,32,18,6)
          ELSE
             CALL PROUT2(OUT,NS,PRTBUF,QSTDEV,TITLE,DATA,32,32,9,11)
          END IF
      ELSE
          IF (QPR) CALL PROUT(OUT,NS,DATA,QSTDEV)
      END IF
      IF (NSW.NE.1) CALL FILTER(OUT,IN,NSW,NS,DATA,QHIGH,QSTDEV,QPR)
      IF (QPLOT) THEN
	  DO I=1,NS
	      IN(I) = ISS+I-1
	  END DO
	  CALL PLOTXY(IN,OUT,NS,'SAMPLE',INUNIT)
      END IF
      IF (NODS.EQ.0) RETURN
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.
     +   (DATA.EQ.'FULL')) THEN
         DO 860 I=1,NS
         OUT(I) = ANINT(OUT(I))
  860    CONTINUE
      ENDIF
      IF (QIMAGE) GO TO 900
      CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NS,' ')
      RETURN
C
C     FULL IMAGE OUTPUT
C
  900 CONTINUE
      DO 950 I=1,NL
      CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NS,' ')
  950 CONTINUE
      RETURN
      END
C****************************************************************************
      SUBROUTINE FILTER(OUT,IN,NSW,NS,DATA,QHIGH,QSTDEV,QPR)
      INTEGER     NSW,NS,M
      REAL*4      OUT(NS),IN(NS),SUM,X
      LOGICAL*1   QHIGH,QSTDEV,QPR
      CHARACTER*5 DATA
      M = NSW/2
      SUM = 0
      DO 100 I=1,M
      SUM = SUM+OUT(I)
  100 CONTINUE
C
C     FILTER WINDOW TRUNCATED AT LEFT EDGE
C
      J = 0
      N = M+1
      DO 200 I=N,NSW
      J = J+1
      SUM = SUM+OUT(I)
      IN(J) = SUM/FLOAT(I)
  200 CONTINUE
C
C     MOVE FILTER WINDOW ACROSS PICTURE TO RIGHT EDGE
C
      X = NSW
      N = NSW+1
      DO 300 I=N,NS
      SUM = SUM+OUT(I)-OUT(I-NSW)
      J = J+1
      IN(J) = SUM/X
  300 CONTINUE
C
C     FILTER WINDOW TRUNCATED AT RIGHT EDGE
C
      N = J-M
      L = NS-M-1
      DO 400 I=N,L
      SUM = SUM-OUT(I)
      X = X-1.0
      J = J+1
      IN(J) = SUM/X
  400 CONTINUE
C
C     HIGHPASS FILTERING
C
      IF (.NOT.QHIGH) GO TO 700
      N = 128
      IF (DATA.NE.'BYTE') N=0
      DO 600 J=1,NS
      IN(J) = OUT(J)-IN(J)+N
      IF (DATA.NE.'BYTE') GO TO 600
      IF (IN(J).LT.0) IN(J)=0
      IF (IN(J).GT.255) IN(J)=255
  600 CONTINUE
  700 CONTINUE
      CALL MVL(IN,OUT,4*NS)
      IF (QPR) THEN
          CALL XVMESSAGE(' AFTER FILTERING',' ')
          CALL PROUT(OUT,NS,DATA,QSTDEV)
      END IF
      RETURN        
      END
C****************************************************************************
      SUBROUTINE PROUT(OUT,NS,DATA,QSTDEV)
C
C     THIS SUBROUTINE PRINTS OUT THE CONTENTS OF THE 'OUT' ARRAY
C
      REAL*4      OUT(NS)
      LOGICAL*1   QSTDEV
      CHARACTER*132 BUF
      CHARACTER*5 DATA
C
      IF (QSTDEV) THEN
          CALL XVMESSAGE('     SAMPLE  STANDARD DEVIATION VALUES',' ')
      ELSE
          CALL XVMESSAGE('     SAMPLE  AVERAGE VALUES',' ')
      END IF
C
      IF (DATA.EQ.'BYTE') THEN
	  DO I=1,NS,17
	      J = MIN(NS,I+16)
	      WRITE (BUF,100) I,J,(NINT(OUT(K)),K=I,J)
  100	      FORMAT(I6,'-',I5,17I4)
	      CALL XVMESSAGE(BUF,' ')
	  END DO
      ELSE IF (DATA.EQ.'HALF') THEN
	  DO I=1,NS,11
	      J = MIN(NS,I+10)
	      WRITE (BUF,200) I,J,(NINT(OUT(K)),K=I,J)
  200	      FORMAT(I6,'-',I5,11I6)
	      CALL XVMESSAGE(BUF,' ')
	  END DO
      ELSE IF (DATA.EQ.'FULL') THEN
	  DO I=1,NS,6
	      J = MIN(NS,I+5)
	      WRITE (BUF,300) I,J,(NINT(OUT(K)),K=I,J)
  300	      FORMAT(I6,'-',I5,6I11)
	      CALL XVMESSAGE(BUF,' ')
	  END DO
      ELSE
	  DO I=1,NS,6
	      J = MIN(NS,I+5)
	      WRITE (BUF,400) I,J,(OUT(K),K=I,J)
  400	      FORMAT(I6,'-',I5,6G11.4)
	      CALL XVMESSAGE(BUF,' ')
	  END DO
      END IF
      RETURN
      END
C*************************************************************************
      SUBROUTINE PROUT2(OUT,NV,BUF,QSTDEV,TITLE,DATA,ILB,ISB,LL,ISIZ )
C
C  PURPOSE
C   THIS SUBROUTINE PRINTS OUT THE CONTENTS OF THE 'OUT' ARRAY
C      AS ONE OR MORE ILBxISB AIS ARRAYS.  BECAUSE OF THE PAGE WIDTH
C      LIMITATION, EACH ILBxISB ARRAY WILL BE PRINTED IN MORE THAN ONE PIECE
C      IF ISB (the number of values per line of the AIS array) IS GREATER
C      THAN LL (the printed line length measured in terms of values).
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    OCT 1984
C
C  INPUT PARAMETERS ( all parameters are INTEGER*4 except as otherwise noted )
C      OUT(K) array      - VALUES TO BE PRINTED, FOR K = 1 TO NV.
C      NV                - NUMBER OF VALUES IN OUT. SHOULD BE A MULTIPLE OF
C                          ILB*ISB.
C      BUF               - WORK SPACE FOR PRINTER LINE BUFFER.
C      QSTDEV            - .TRUE. IF OUT ARRAY CONTAINS STANDARD DEVIATIONS.
C      TITLE             - TITLE SPECIFIED FOR THE USER FOR PRINTOUT.
C      ILB               - LINES PER BLOCK OF NUMBERS TO BE PRINTED.
C      ISB               - SAMPLES PER LINE IN BLOCK OF NUMBERS TO BE PRINTED.
C      LL                - MAXIMUM NUMBER OF VALUES TO FIT ON A PRINT LINE.
C      ISIZ              - NUMBER OF SPACES ALLOCATED FOR EACH VALUE ON A LINE
C                          (INCLUDING THE SPACES BETWEEN VALUES).
C  OUTPUT PARAMETERS
C      THE VALUES ARE PRINTED UNLESS NV IS NOT A MULTIPLE OF ILB*ISB,
C      IN WHICH CASE THE PROGRAM ABORTS.  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.
C
C
      REAL*4      OUT(ISB,ILB,*)        ! MAKE IT EASIER BY DECLARING AS 3-DIM.
      LOGICAL*1   QSTDEV
      CHARACTER*132 BUF
      CHARACTER*124 TITLE
      CHARACTER*5 DATA
C
      IF ( MOD(NV,ISB*ILB) .NE. 0) THEN
	 CALL XVMESSAGE(
     +     ' INCORRECT NUMBER OF INPUT SAMPLES FOR AIS PRINT FORMAT',
     +	   ' ')
	 CALL ABEND
      ENDIF
      IF (QSTDEV)   THEN
          CALL XVMESSAGE('    SAMPLE  STANDARD DEVIATION VALUES',' ')
      ELSE
          CALL XVMESSAGE('    SAMPLE  AVERAGE VALUES',' ')
      END IF
C
      NBLOCKS = NV / (ISB*ILB)    ! NUMBER OF BLOCKS (SECTIONS OR AIS ARRAYS).
C
      DO IBLOCK = 1, NBLOCKS      ! PRINT A BLOCK FOR EACH BLOCK OF VALUES.
C
	 CALL XVMESSAGE(' ',' ')
	 CALL XVMESSAGE(' ',' ')
	 CALL XVMESSAGE(' ',' ')
	 CALL XVMESSAGE(' ',' ')
	 CALL XVMESSAGE(TITLE,' ')
         IF (NBLOCKS .GT. 1) THEN     ! IF MORE THAN 1 SECTION, NUMBER THEM.
           CALL XVMESSAGE(' ',' ')
	   WRITE (BUF,100) IBLOCK
  100	   FORMAT(' SECTION',I3)
	   CALL XVMESSAGE(BUF,' ')
         END IF
C
         ISPTR = 1                     ! COLUMN POINTER TO BEGINNING OF PIECE.
         DO WHILE ( ISPTR .LE. ISB )   ! USE AS MANY PIECES PER BLOCK AS NEEDED
            ISLINE = MIN0( LL, ISB-ISPTR+1 )
            ISLAST = ISPTR + ISLINE - 1
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(' ',' ')
	    IF (ISIZ .EQ. 6) THEN
	       WRITE (BUF,200) (J,J=ISPTR,ISLAST)
  200	       FORMAT('      SAMP  ',20I6)
	    ELSE
	       WRITE (BUF,300) (J,J=ISPTR,ISLAST)
  300	       FORMAT('      SAMP  ',11I11)
	    END IF
	    CALL XVMESSAGE(BUF,' ')
	    CALL XVMESSAGE('  LINE',' ')
C
            DO L = 1, ILB
	       IF (DATA.EQ.'BYTE' .OR. DATA.EQ.'HALF') THEN
		  WRITE (BUF,400) L,
     +			(NINT(OUT(J,L,IBLOCK)), J=ISPTR,ISLAST)
  400		  FORMAT(I6,6X,20I6)
	       ELSE IF (DATA .EQ. 'FULL') THEN
		  WRITE (BUF,500) L,
     +			(NINT(OUT(J,L,IBLOCK)), J=ISPTR,ISLAST)
  500		  FORMAT(I6,6X,11I11)
	       ELSE
		  WRITE (BUF,600) L,(OUT(J,L,IBLOCK), J=ISPTR,ISLAST)
  600		  FORMAT(I6,6X,11G11.4)
	       END IF
               CALL XVMESSAGE(BUF,' ')
            END DO
            ISPTR = ISLAST + 1                     ! POINT TO NEXT PIECE
         END DO
      END DO
      RETURN
      END
C**********************************************************************
C
	SUBROUTINE PLOTXY(XBUF,YBUF,NUM,XAXIS,INUNIT)
C
	REAL XBUF(NUM),YBUF(NUM)
	CHARACTER*80 MSG
	CHARACTER*6 XAXIS
C								scale x-axis
	CALL MINMAX(7,NUM,XBUF,X1,X2,IMIN,IMAX)
	IF (X1.EQ.X2) THEN
	    CALL XVMESSAGE(
     +	    ' More than 1 averaged value is needed for plotting.',' ')
	    CALL ABEND
	ENDIF
	X = ALOG10(X2-X1)
	IF (X.GE.0.0) THEN				! span between tic-marks
	    DIV = 10.0**INT(X)
	ELSE
	    DIV = 10.0**INT(X-1.0)
	END IF
	IF (X1.GE.0.0) THEN				! lower limit of graph
	    XLO = DIV*INT(X1/DIV)
	ELSE
	    XLO = DIV*INT((X1/DIV)-1.0)
	END IF
	NXTIC = 1+(X2-XLO)/DIV				! # of tic-marks
	XHI = XLO+NXTIC*DIV				! upper limit of graph
C
	IF (NXTIC.LE.3) THEN				! adjust # of tic-marks
	    NXTIC = 5*NXTIC
	    DIV = DIV/5.0
	END IF
	IF (NXTIC.LE.7) THEN
	    NXTIC = 2*NXTIC
	    DIV = DIV/2.0
	END IF
	DO WHILE (X1 .GE. XLO+DIV)
	    XLO = XLO+DIV
	    NXTIC = NXTIC-1
	END DO
	DO WHILE (X2 .LE. XHI-DIV)
	    XHI = XHI-DIV
	    NXTIC = NXTIC-1
	END DO
C								scale Y-axis
	CALL MINMAX(7,NUM,YBUF,Y1,Y2,IMIN,IMAX)
	IF (Y1.EQ.Y2) Y2=Y1+1
	Y = ALOG10(Y2-Y1)
	IF (Y.GE.0.0) THEN				! span between tic-marks
	    DIV = 10.0**INT(Y)
	ELSE
	    DIV = 10.0**INT(Y-1.0)
	END IF
	IF (Y1.GE.0.0) THEN				! lower limit of graph
	    YLO = DIV*INT(Y1/DIV)
	ELSE
	    YLO = DIV*INT((Y1/DIV)-1.0)
	END IF
	NYTIC = 1+(Y2-YLO)/DIV				! # of tic-marks
	YHI = YLO+NYTIC*DIV				! upper limit of graph
C
	IF (NYTIC.LE.3) THEN				! adjust # of tic-marks
	    NYTIC = 5*NYTIC
	    DIV = DIV/5.0
	END IF
	IF (NYTIC.LE.7) THEN
	    NYTIC = 2*NYTIC
	    DIV = DIV/2.0
	END IF
	DO WHILE (Y1 .GE. YLO+DIV)
	    YLO = YLO+DIV
	    NYTIC = NYTIC-1
	END DO
	DO WHILE (Y2 .LE. YHI-DIV)
	    YHI = YHI-DIV
	    NYTIC = NYTIC-1
	END DO
C							submit to the printer
	CALL XVGET(INUNIT,ISTAT,'NAME',MSG,' ')
	CALL ADD0(MSG,40,LEN)
	CALL PSPLOT(XBUF,YBUF,NUM,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			XAXIS,'DN','Average Values',MSG,' ',-1)
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create lave.pdf
process help=*
PARM INP      TYPE=(STRING,40)
PARM OUT      TYPE=(STRING,40)  DEFAULT=""
PARM SIZE     TYPE=INTEGER DEFAULT=(1,1,0,0) COUNT=4
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER DEFAULT=0
PARM NS       TYPE=INTEGER DEFAULT=0
PARM MODE     TYPE=KEYWORD DEFAULT=HORIZONT VALID=(VERTICAL,HORIZONT)
PARM HIGHPASS TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=HIGHPASS
PARM IMAGE    TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=IMAGE
PARM STDEV    TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=STDEV
PARM PLOT     TYPE=KEYWORD DEFAULT=NOPLOT VALID=(PLOT,NOPLOT)
PARM PRINT    TYPE=KEYWORD DEFAULT=PRINT VALID=(PRINT,NOPRINT)
PARM FILTER   TYPE=INTEGER DEFAULT=1
PARM EXCLUDE  TYPE=INTEGER DEFAULT=0
PARM AIS      TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=AIS
PARM TITLE    TYPE=(STRING,122) COUNT=0:1 DEFAULT=--  
END-PROC
.TITLE
LAVE
.HELP
PURPOSE:
LAVE calculates the average DN values or the standard deviation values for each
line (HORIZONTAL mode) or each column (VERTICAL mode) of samples within a
picture.  The output is either a single line of the average (or standard
deviation)  DN values (default) or an image the same size as the input picture,
but with each pixel replaced by its average (or standard deviation) value
(IMAGE mode).  LAVE determines the data format from the VICAR label of the
image.  LAVE works on byte, halfword, fullword, and real*4 data.
.PAGE
EXECUTION:

LAVE A OUT=C FILTER=7		Find the averages of rows (default), perform 
				a box filter with window size 7, and send the 
				output to image file C.
LAVE A MODE=VERT 		Find column averages.  Print the results only.
LAVE A OUT=C MODE=IMAGE		Find averages of rows, and write image same
				size as original image (with each pixel
				replaced by its average value) out to C.
LAVE A EXCLUDE=1		Ignore all samples with value of 1.
LAVE A FILTER=3 'HIGH	        Do filter with window size 3, and highpass
				output.

LAVE A OUT=C SIZE=(50,20,100,200) 'IMAGE 'VERT

has the same effect as	LAVE A B SIZE=(50,20,100,200) MODE=VERT 
			SIZE B C SIZE=(1,1,100,200)

LAVE A C FILTER=7 

has the same effect as	LAVE A B
			BOXFLT2 B C NLW=1 NSW=7

OPERATION:
The input picture is read and the average DN values are computed, excluding
any pixels of the DN specified by the EXCLUDE keyword.  If there remain no
pixels to be averaged, the output pixel is assigned 0 DN.  All the average
values are then printed.  If a filter has been requested, the average values
are filtered and those values are also printed.  Unless the keyword IMAGE
was specified, a single line of average values is output.  Note that in the
HORIZONTAL mode, the number of samples output is the number of LINES input.
If the keyword IMAGE was used, a full-size image is produced by either
repeating the line of average values, once for each line (VERTICAL mode), or
repeating each average value once for each sample, with each average value
on its original line (HORIZONTAL mode).

If STDEV is specified, standard deviation values are used instead of averages.

WRITTEN BY:  R.M. Ruiz, 25 February 1971
COGNIZANT PROGRAMMER: Ron Alley 
REVISION:  8 February 2001

.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE OUT
Output image file, if any.
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NS
Number of lines
.VARIABLE NL
Number of samples
.VARIABLE MODE
VERTICAL or HORIZONT
.VARIABLE HIGHPASS
Perform highpass filter?
.VARIABLE PLOT
print a plot of results?
.VARIABLE PRINT
print the averages?
.VARIABLE IMAGE
Repeat output line or
column to make output file the 
size of input file.
.VARIABLE STDEV
Specifies standard deviation 
will be computed as the output
instead of averages.
.VARIABLE EXCLUDE
Value to exclude
.VARIABLE FILTER
Filter width
.VARIABLE AIS
Print output values
in block(s) of 32 by 32 values.
.VARIABLE TITLE
Title for AIS print-out
.LEVEL2
.VARIABLE OUT
OUT specifies the name of the output file.  If the output file specification
is omitted, output is sent only to the screen.
.VARIABLE MODE
MODE specifies one of the following options:

HORIZONT indicates that averaging will be done along each line (default).
VERTICAL indicates that averaging will be done along each column.
.VARIABLE HIGHPASS
HIGH causes the high-pass version of the filter to be output.  Pixel
	values created by the high-pass specification are determined
	by taking the average value of the pixels within the 
	user-specified window dimension and then subtracting the value
	of the pixel prior to filtering, and, for byte data only, adding 128 
        to the result.
.VARIABLE PLOT
If the keyword PLOT is used, the results are plotted as a PostScript file.
.VARIABLE PRINT
If the keyword NOPRINT is used, printing to the screen and session log are
suppressed. In the default (PRINT) mode, each of the average values is
printed.
.VARIABLE IMAGE
IMAG causes the creation of a full-size image where each pixel is replaced
	in value by the average for its particular row (HORIZONTAL mode)
	or column (VERTICAL mode).
.VARIABLE STDEV
The default is to compute averages.
.VARIABLE EXCLUDE
EXCLUDE selects a particular pixel value to be ignored by the program.
(Default is to include all samples.)
.VARIABLE FILTER
FILTER specifies the filtering option and the width of the window to
be used.  An m-element box filter will be performed on the line of
average DN values before the line is output.  (Default is no filtering
on the line.)
.VARIABLE AIS
By default, the print-out is not in AIS format.  The AIS keyword
does not affect the size of the output file.
.VARIABLE TITLE
This parameter is used with the AIS parameter. The maximum title length
is 122 characters.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create lave.imake
#define  PROGRAM   lave

#define MODULE_LIST lave.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
