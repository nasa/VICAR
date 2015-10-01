$!****************************************************************************
$!
$! Build proc for MIPL module lave
$! VPACK Version 1.9, Wednesday, March 18, 2015, 21:57:48
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
$ write sys$output "*** module lave ***"
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
$ write sys$output "Invalid argument given to lave.com file -- ", primary
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
	-i lave.imake -
	-p lave.pdf -
	-t tstlave.pdf tstlave.log
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
C  10 Dec 2010  RJB   Fixes for 64-bit linux, MacOSX
C  31 AUG 94    SVH   PORTED TO UNIX
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
	implicit none
      EXTERNAL VERT,HORI
      LOGICAL*4    QVERT ,QEXCL ,QIMAGE ,QSTDEV ,QAIS
      LOGICAL*4    QHIGH
      INTEGER*4    ICNT,INUNIT,NEXCL,NL,NS,NODS,NSW,OUTUNIT,STATUS
      INTEGER*4    ISL,ISS
      INTEGER*4    II,JJ,KK,LL
      INTEGER*4    DUM1,DUM2
      CHARACTER*123 TITLE
      CHARACTER*5  AIS,DATA,IMAGE,STDEV
      CHARACTER*8  HIGH,MODE
      CHARACTER*45 LBUF
      DATA    QVERT ,QEXCL ,QIMAGE ,QSTDEV ,QAIS /.false.,.false.,
     +.false.,.false.,.false./
      DATA    QHIGH                              /.false./
      DATA    TITLE                              /'  '/
      DATA    DUM1,DUM2 /1,1/
C
	call xvmessage ('** LAVE - Sep 03, 2013 - rjb (64-bit)',' ')
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
      CALL XVSIZE(ISL,ISS,NL,NS,DUM1,DUM2)
C                                                     VERTICAL
      CALL XVP('MODE',MODE,ICNT)
      IF (MODE.EQ.'VERTICAL') QVERT=.true.
C                                                     EXCLUDE
      CALL XVP('EXCLUDE',NEXCL,ICNT)
      IF (ICNT.NE.0) QEXCL = .true.
C                                                     FILTER
      CALL XVP('FILTER',NSW,ICNT)
C                                                     IMAGE
      CALL XVP('IMAGE',IMAGE,ICNT)
      IF (ICNT.NE.0) QIMAGE=.true.
C                                                     STANDARD DEVIATION
      CALL XVP('STDEV',STDEV,ICNT)
      IF (ICNT.NE.0) QSTDEV = .true.
C                                                     AIS PRINT FORMAT
      CALL XVP('AIS',AIS,ICNT)
      IF (ICNT.NE.0) QAIS = .true.
C                                                     HIGHPASS
      CALL XVP('HIGHPASS',HIGH,ICNT)
      IF (ICNT.NE.0) QHIGH= .true.
C                                                     TITLE
      CALL XVP('TITLE', TITLE,ICNT)
C
C
      IF ((QIMAGE).AND.(NODS.NE.0))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +                'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',
     +                ' ')
      IF ((.not.QIMAGE).AND.(NODS.NE.0).AND.(MODE.EQ.'VERTICAL'))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',1,'U_NS',NS,
     +                'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',
     +                ' ')
      IF ((.not.QIMAGE).AND.(NODS.NE.0).AND.(MODE.EQ.'HORIZONT'))
     +    CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',1,'U_NS',NL,
     +                'U_FORMAT','REAL','IO_ACT','SA','OPEN_ACT','SA',
     +                ' ')
C     POSITION INPUT DS FOR READING, COMPUTE SIZE OF ARRAYS, CALL STACKA 
C
      II = 4*NS
      IF(QVERT) GO TO 200
      JJ = 4*NL
      IF (II.LT.JJ) II=JJ
      CALL STACKA(20,HORI,2,II,JJ,ISL,ISS,NL,NS,DATA,NODS,NEXCL,QEXCL,
     +            NSW,QIMAGE,QSTDEV,QAIS,TITLE,QHIGH,INUNIT,OUTUNIT)
	call xvmessage('LAVE PROCESSING COMPLETE',' ')
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN

  200 CONTINUE
      JJ = 4*NS
      KK = 1
      IF (QEXCL) KK=4*NS
      LL = 8*NS
      CALL STACKA(23,VERT,5,II,JJ,KK,LL,LL,ISL,ISS,NL,NS,DATA,NODS,
     +            NEXCL,QEXCL,NSW,QIMAGE,QSTDEV,QAIS,TITLE,QHIGH,
     +            INUNIT,OUTUNIT)
	call xvmessage('LAVE PROCESSING COMPLETE',' ')
      RETURN
c  900 CONTINUE
      call xvmessage ('??E - Insufficient memory for stacka operation',
     +                ' ')
	call abend
      RETURN
      END
C
C=======================================================================
      SUBROUTINE HORI(IN,II,OUT,JJ,ISL,ISS,NL,NS,DATA,NODS,NEXCL,
     +                QEXCL,NSW,QIMAGE,QSTDEV,QAIS,TITLE,QHIGH,
     +                INUNIT,OUTUNIT)
C
C     THIS SUBROUTINE COMPUTES AVERAGES FOR EACH LINE
C
	implicit none
	INTEGER*4   ISL,ISS,NL,NS,NODS,NEXCL,NSW
      REAL*4      IN(NS),OUT(NL), IN1,XADJ
      REAL*8      SUM,SUM2,XNS, XDIFF
      INTEGER*4   INUNIT,OUTUNIT
      INTEGER*4   I,J,K,NUM
      INTEGER*4   II,JJ,dummy
      INTEGER*4   STATUS
      LOGICAL*4   QEXCL,QIMAGE,QSTDEV,QAIS,QHIGH
      CHARACTER*123 TITLE
      CHARACTER*5 DATA
	dummy = ii		!to prevent message
	in1 = 0
        IF (JJ.NE.4*NL) then
	    call xvmessage ('??E- Insufficient memory for STACKA buffers',' ')
	    call abend
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
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
          SUM = 0
          DO 200 J=1,NS
              SUM = SUM+IN(J)
  200     CONTINUE
          K = K+1
          OUT(K) = SUM/XNS
  300 CONTINUE
      GO TO 700
C
C   PROCESSING FOR NO EXCLUDED VALUE, STANDARD DEVIATION COMPUTED
C
  340 XNS = NS
      DO 380 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
          IN1 = IN(1)
          SUM = 0
          SUM2 = 0
          DO 360 J=1,NS
              XADJ = IN(J) - IN1        ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                        ! OF SET FROM EVERY ELEMENT OF SET.
              SUM = SUM + XADJ
              SUM2 = SUM2 + XADJ*XADJ
  360     CONTINUE
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
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
          SUM = 0
          NUM = 0
          DO 500 J=1,NS
              IF (IN(J).EQ.NEXCL) GO TO 500
              SUM = SUM+IN(J)
              NUM = NUM+1
  500     CONTINUE
          IF (NUM.EQ.0) NUM=1
          K = K+1
          OUT(K) = SUM/NUM
  600 CONTINUE
      GO TO 700
C
C   PROCESSING FOR EXCLUDED VALUES, STANDARD DEVIATION COMPUTED
C
  640 DO 680 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
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
  660     CONTINUE
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
             CALL PROUT2(OUT,NL,QSTDEV,TITLE,DATA,32,32,18,6)
          ELSE
             CALL PROUT2(OUT,NL,QSTDEV,TITLE,DATA,32,32,9,11)
          END IF
      ELSE
          CALL PROUT(OUT,NL,DATA,QSTDEV)
      END IF
c  740 continue
	IF (NSW.NE.1) CALL FILTER(OUT,IN,NSW,NL,DATA,QHIGH,QSTDEV)
      IF (NODS.EQ.0) RETURN
      IF (QIMAGE) GO TO 800
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.(DATA.EQ.'FULL')) THEN
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
c  900 CONTINUE
      DO 950 I=1,NL
          DO 920 J=1,NS
              IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.
     +            (DATA.EQ.'FULL')) THEN
                  IN(J) = ANINT(OUT(I))
              ELSE   
                  IN(J) = OUT(I)
              ENDIF
  920     CONTINUE
      CALL XVWRIT(OUTUNIT,IN,STATUS,'NSAMPS',NS,' ')
  950 CONTINUE
      RETURN
      END
C
C====================================================================
      SUBROUTINE VERT(IN,II,OUT,JJ,IPOP,KK,TOT,LL,TOT2,L2,ISL,ISS,
     +               NL,NS,DATA,NODS,NEXCL,QEXCL,NSW,QIMAGE,
     +               QSTDEV,QAIS,TITLE,QHIGH,INUNIT,OUTUNIT)
C
C     THIS SUBROUTINE COMPUTES AVERAGES FOR EACH SAMPLE
C
	implicit none
	INTEGER*4   ISL,ISS,NL,NS,NODS,NEXCL,NSW
      REAL*4      IN(NS),OUT(NS), XADJ
      REAL*8      TOT(NS),TOT2(NS),XNL, XDIFF
      INTEGER*4   INUNIT,OUTUNIT
      INTEGER*4   IPOP(NS)
      INTEGER*4   I,J
      INTEGER*4   II,JJ,KK,LL
      INTEGER*4   L2,dummy
      INTEGER*4   STATUS
      LOGICAL*4   QIMAGE,QEXCL,QSTDEV,QAIS,QHIGH
      CHARACTER*123 TITLE
      CHARACTER*5 DATA
	dummy = ii		!to prevent compiler message
	dummy = jj
	dummy = kk
	dummy = ll
      	IF (L2.NE.8*NS) then
	    call xvmessage ('??E - Insufficient core for STACKA buffers',' ')
	    call abend
	ENDIF
      CALL ZIA(TOT,2*NS)
      CALL ZIA(TOT2,2*NS)
      IF (QEXCL) GO TO 500
      IF (QSTDEV) GO TO 420
C
C     NO EXCLUDED VALUE, MEAN COMPUTED
C
      DO 300 I=ISL,(NL+ISL-1)
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
          DO 200 J=1,NS
              TOT(J) = TOT(J)+IN(J)
  200     CONTINUE
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
      IF ( I .EQ. ISL )  CALL MVE( 7, NS, IN, OUT,1,1)
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
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
          DO 600 J=1,NS
             IF (IN(J).EQ.NEXCL) GO TO 600
             TOT(J) = TOT(J)+IN(J)
             IPOP(J) = IPOP(J)+1
  600     CONTINUE
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
          CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',NS,'SAMP',ISS,'LINE',I,
     +                ' ')
          DO 770 J=1,NS
              IF (IN(J).EQ.NEXCL) GO TO 770
              IF ( IPOP(J) .EQ. 0 )  OUT(J) = IN(J)   ! STORE FIRST ELEMENT IN OUT.
              XADJ = IN(J) - OUT(J)     ! REDUCE ROUNDOFF BY SUBTRACTING FIRST ELEMENT
                                ! OF SET FROM EVERY ELEMENT OF SET.
              TOT(J) = TOT(J) + XADJ
              TOT2(J) = TOT2(J) + XADJ*XADJ
              IPOP(J) = IPOP(J)+1
  770     CONTINUE
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
             CALL PROUT2(OUT,NS,QSTDEV,TITLE,DATA,32,32,18,6)
          ELSE
             CALL PROUT2(OUT,NS,QSTDEV,TITLE,DATA,32,32,9,11)
          END IF
      ELSE
          CALL PROUT(OUT,NS,DATA,QSTDEV)
      END IF
c  840 continue
	IF (NSW.NE.1) CALL FILTER(OUT,IN,NSW,NS,DATA,QHIGH,QSTDEV)
      IF (NODS.EQ.0) RETURN
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF').OR.(DATA.EQ.'FULL')) THEN
         DO 860 I=1,NS
             OUT(I) = ANINT(OUT(I))
  860    CONTINUE
      ENDIF
c      IF (QIMAGE.NE.0) GO TO 900
	if (qimage) go to 900
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
C
C====================================================================
      SUBROUTINE FILTER(OUT,IN,NSW,NS,DATA,QHIGH,QSTDEV)
	implicit none
      INTEGER*4   NSW,NS,M
      INTEGER*4   I,J,N,L
      REAL*4      OUT(NS),IN(NS),SUM,X
      LOGICAL*4   QHIGH,QSTDEV
      CHARACTER*5 DATA
      INTEGER*4 DUM1,DUM3,DUM4
      DATA  DUM1,DUM3,DUM4 /7,1,1/
C

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
      IF (.not.QHIGH) GO TO 700
      N = 128
      IF (DATA.NE.'BYTE') N=0
      DO 600 J=1,NS
          IN(J) = OUT(J)-IN(J)+N
          IF (DATA.NE.'BYTE') GO TO 600
          IF (IN(J).LT.0) IN(J)=0
          IF (IN(J).GT.255) IN(J)=255
  600 CONTINUE
  700 CONTINUE
      CALL MVE(DUM1,NS,IN,OUT,DUM3,DUM4)
      CALL XVMESSAGE('??I - After filtering',' ')

      CALL PROUT(OUT,NS,DATA,QSTDEV)
      RETURN        
      END
C
C=====================================================================
      SUBROUTINE PROUT(OUT,NS,DATA,QSTDEV)
C
C     THIS SUBROUTINE PRINTS OUT THE CONTENTS OF THE 'OUT' ARRAY
C
	implicit none
	INTEGER*4   NS,INC
      REAL*4      OUT(NS)
      INTEGER*2   IVALH
      INTEGER*4   IVALF
      INTEGER*4   I,J
      CHARACTER*132 UNCONFMT
      CHARACTER*132 BUF
      LOGICAL*4 QSTDEV
      CHARACTER*5 DATA
C
      IF (DATA.EQ.'BYTE') THEN
         INC = 4
      ELSE IF (DATA.EQ.'HALF') THEN
         INC = 6
      ELSE 
         INC = 11
      END IF
      IF (QSTDEV) THEN
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('    SAMPLE  STANDARD DEVIATION VALUES',' ')
      ELSE
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE('    SAMPLE  AVERAGE VALUES',' ')
      END IF
	do i=1,132
	  BUF(i:i)=' '
        enddo
      BUF(1:6) = '    1-'
      J = 11
      DO 500 I=1,NS
C     Logically, it would make sense to have .LE.132, but that may break in
C     a different spot than the unported version, making test logs that don't
C     compare properly.
      IF ((J+INC).LE.131) GO TO 200
      WRITE (BUF(7:11),'(I5)') I-1
c	BUF(132:132) = ' '
      CALL XVMESSAGE(BUF,' ')
      J = 11
      WRITE (BUF(1:5),'(I5)') I
  200 CONTINUE
      J = J+INC
      IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
         IVALH = NINT(OUT(I))
         WRITE (UNCONFMT,201) INC
  201    FORMAT('(I',I4.4,')')
         WRITE (BUF(J-INC+1:J),UNCONFMT) IVALH
      ELSE IF (DATA.EQ.'FULL') THEN
         IVALF = NINT(OUT(I))
         WRITE (UNCONFMT,202) INC
  202    FORMAT('(I',I4.4,')')
         WRITE (BUF(J-(INC)+1:J),UNCONFMT) IVALF
      ELSE
         WRITE (UNCONFMT,203) (INC-1), (INC-1)-6
  203    FORMAT('(E',I4.4,'.',I4.4,')')
         WRITE (BUF(J-(INC-1)+1:J),UNCONFMT) OUT(I)
         BUF(J-INC+1:J-INC+1) = ' '
      END IF
  500 CONTINUE
      WRITE (BUF(7:11),'(I5)') NS
c      BUF(132:132) = ' '
      CALL XVMESSAGE(BUF(1:J),' ')
      RETURN
      END
C
C=======================================================================
      SUBROUTINE PROUT2(OUT,NV,QSTDEV,TITLE,DATA,ILB,ISB,LL,ISIZ )
C#######################################################################
C
C  PURPOSE
C   THIS SUBROUTINE PRINTS OUT THE CONTENTS OF THE 'OUT' ARRAY
C      AS ONE OR MORE ILBxISB AIS ARRAYS.  BECAUSE OF THE PAGE WIDTH
C      LIMITATION, EACH ILBxISB ARRAY WILL BE PRINTED IN MORE THAN ONE PIECE
C      IF ISB (the number of values per line of the AIS array) IS GREATER
C      THAN LL (the printed line length measured in terms of values).
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    OCT 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  INPUT PARAMETERS ( all parameters are INTEGER*4 except as otherwise noted )
C      OUT(K) array      - VALUES TO BE PRINTED, FOR K = 1 TO NV.
C      NV                - NUMBER OF VALUES IN OUT. SHOULD BE A MULTIPLE OF
C                          ILB*ISB.
C      QSTDEV            - 1 IF OUT ARRAY CONTAINS STANDARD DEVIATIONS.
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

C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

C
C
	implicit none
	INTEGER*4   NV,ILB,ISB,ISIZ
      REAL*4      OUT(ISB,ILB,*)        ! MAKE IT EASIER BY DECLARING AS 3-DIM.
      CHARACTER*132 UNCONFMT
C      BUF               - WORK SPACE FOR PRINTER LINE BUFFER.
      CHARACTER*132 BUF
      LOGICAL*4 QSTDEV
      INTEGER*2   IVALH
      INTEGER*4   IVALF
      INTEGER*4   IBLOCK,NBLOCKS,ISPTR,ISLINE,ISLAST,IPOS
      INTEGER*4   J,L
      INTEGER*4   LL
      CHARACTER*5 DATA
      CHARACTER*123 TITLE

C=============================================================================

      IF ( MOD(NV,ISB*ILB) .NE. 0) then
         CALL xvmessage('??E - Incorrect number of input samples for ais
     + print format',' ' )
	 call abend
      endif
      IF (QSTDEV)   THEN
          CALL XVMESSAGE(' ',' ')
          CALL XVMESSAGE('    SAMPLE  STANDARD DEVIATION VALUES',' ')
      ELSE
          CALL XVMESSAGE(' ',' ')
          CALL XVMESSAGE('    SAMPLE  AVERAGE VALUES',' ')
      END IF

      NBLOCKS = NV / (ISB*ILB)    ! NUMBER OF BLOCKS (SECTIONS OR AIS ARRAYS).

      DO IBLOCK = 1, NBLOCKS      ! PRINT A BLOCK FOR EACH BLOCK OF VALUES.

         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE( ' ' ,' ')
         CALL XVMESSAGE(TITLE,' ')
         IF (NBLOCKS .GT. 1) THEN     ! IF MORE THAN 1 SECTION, NUMBER THEM.
           CALL XVMESSAGE( ' ' ,' ')
           BUF(1:8) = ' SECTION'
           WRITE (BUF(9:11),'(I3)') IBLOCK
           CALL XVMESSAGE(BUF(2:11),' ')
         END IF

         ISPTR = 1                     ! COLUMN POINTER TO BEGINNING OF PIECE.
         DO WHILE ( ISPTR .LE. ISB )   ! USE AS MANY PIECES PER BLOCK AS NEEDED
            ISLINE = MIN0( LL, ISB-ISPTR+1 )
            ISLAST = ISPTR + ISLINE - 1
            CALL XVMESSAGE( ' ' ,' ')
            CALL XVMESSAGE( ' ' ,' ')
            BUF(1:132) = ' '
            BUF(1:12) = '      SAMP  '
            IPOS = 12 - ISIZ
            DO J = ISPTR, ISLAST, 2    ! PRINT SAMPLE NUMBERS ABOVE THE PIECE.
               IPOS = IPOS + 2*ISIZ
               WRITE (UNCONFMT,501) ISIZ
  501    FORMAT('(I',I4.4,')')
               WRITE (BUF(IPOS-(ISIZ)+1:IPOS),UNCONFMT) J
            END DO
            CALL XVMESSAGE(BUF(2:IPOS),' ')
            CALL XVMESSAGE(' LINE',' ')
      
            DO L = 1, ILB
               BUF(1:12) = ' '
               WRITE (BUF(2:6),'(I5)') L ! PRINT LINE NUMBER
               IPOS = 12 
               DO J = ISPTR, ISLAST
                  IPOS = IPOS + ISIZ               ! AND A LINE OF VALUES.
                  IF ((DATA.EQ.'BYTE').OR.(DATA.EQ.'HALF')) THEN
                     IVALH = NINT(OUT(J,L,IBLOCK))
                     WRITE (UNCONFMT,502) ISIZ
  502    FORMAT('(I',I4.4,')')
                     WRITE (BUF(IPOS-(ISIZ)+1:IPOS),UNCONFMT) IVALH
                  ELSE IF (DATA.EQ.'FULL') THEN
                     IVALF = NINT(OUT(J,L,IBLOCK))
                     WRITE (UNCONFMT,503) ISIZ
  503    FORMAT('(I',I4.4,')')
                     WRITE (BUF(IPOS-(ISIZ)+1:IPOS),UNCONFMT) IVALF
                  ELSE
                     WRITE (UNCONFMT,504) (ISIZ-1),(ISIZ-1)-6
  504    FORMAT('(E',I4.4,'.',I4.4,')')
                     WRITE (BUF(IPOS-(ISIZ-1)+1:IPOS),UNCONFMT) OUT(J,
     +L,IBLOCK)
                     BUF(IPOS-(ISIZ-1)+1:IPOS-ISIZ+1) = ' '
                  END IF
               END DO
               CALL XVMESSAGE(BUF(2:IPOS),' ')
            END DO

            ISPTR = ISLAST + 1                     ! POINT TO NEXT PIECE
         END DO
      END DO
      RETURN
      END
C
C=====================================================================
c      SUBROUTINE ABND(BUF,N)
c      BYTE BUF(N)
c      INTEGER N
C
c      CALL XVMESSAGE(BUF,' ')
c      CALL ABEND
c      RETURN
c      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lave.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM lave

   To Create the build file give the command:

		$ vimake lave			(VMS)
   or
		% vimake lave			(Unix)


************************************************************************/


#define PROGRAM	lave

#define MODULE_LIST lave.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create lave.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING DEFAULT=""
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM MODE TYPE=KEYWORD COUNT=0:1 DEFAULT=HORIZONT VALID=(VERTICAL,HORIZONT)
PARM HIGHPASS TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=HIGHPASS
PARM IMAGE    TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=IMAGE
PARM STDEV    TYPE=KEYWORD COUNT=0:1 DEFAULT=--  VALID=STDEV
PARM FILTER TYPE=INTEGER DEFAULT=1
PARM EXCLUDE TYPE=INTEGER DEFAULT=0
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
COGNIZANT PROGRAMMER:  R. J. Bambery
REVISIONS:
    1986-08-27 J. R. HEYADA
    1994-08-31 ??? - Ported to Unix
    2010-12-10 R. J. Bambery - Fixes for 64-bit linux,macosx
    2013-09-02 R. J. Bambery - Blank out 132 character buffer before filling
               to prevent garbage at end of xvmessage statements. 

.LEVEL1
.VARIABLE INP
STRING - Input image file
.VARIABLE OUT
STRING - Output image file, if 
any.
.VARIABLE SIZE
INTEGER - Standard VICAR size 
field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE MODE
KEYWORD - VERTICAL or HORIZONT
.VARIABLE HIGHPASS
KEYWORD - Perform highpass
filter.
.VARIABLE IMAGE
KEYWORD - Repeat output line or
column to make output file the 
size of input file.
.VARIABLE STDEV
KEYWORD - Specifies standard
deviation will be computed as
the output instead of averages.
.VARIABLE EXCLUDE
INTEGER - Value to exclude
.VARIABLE FILTER
INTEGER - Filter width
.VARIABLE AIS
KEYWORD - Print output values
in block(s) of 32 by 32 values.
.VARIABLE TITLE
STRING - Title for AIS print-out
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
$Test_File:
$ create tstlave.pdf
procedure
refgbl $echo
refgbl $syschar
! Jun 24, 2012 - RJB
! TEST SCRIPT FOR LAVE
! tests BYTE, HALF, FULL, REAL images
!
! Vicar Programs:
!       gen list qsar label-list
!
! External Programs:
!   <none>
! 
! Parameters:
!   <none>
!
! Requires NO external test data: 
!
! Removed VMS checks
!
body
let _onfail="stop"
let $echo="yes"
!
!   THIS IS A TEST OF PROGRAM LAVE
!
!      test LAVE for byte data.
!
gen lavea 11 10
!
lave lavea lavea1
list lavea1
!
lave lavea lavea2 'vert exclude=0
list lavea2
!
lave lavea lavea3  (6,2,5,7) 'horiz 'image
list lavea3
!
qsar lavea laveb area=(3,3,6,6,100)
!
lave laveb laveb1 'stdev
list laveb1
!
lave laveb laveb2 filter=3
list laveb2
!
lave laveb laveb3 filter=3 'highpass
list laveb3
!
gen lavec 1 2048
!
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
list lavec1 (1,1,1,10)
label-list lavec1
!
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
!
!
!      test LAVE for halfword data.
!
gen lavea 11 10  'half
!
lave lavea lavea1  
list lavea1
!
lave lavea lavea2 'vert exclude=0   
list lavea2
!
lave lavea lavea3  (6,2,5,7) 'horiz 'image  
list lavea3
!
qsar lavea laveb area=(3,3,6,6,100)
!
lave laveb laveb1 'stdev  
list laveb1
!
lave laveb laveb2 filter=3  
list laveb2
!
lave laveb laveb3 filter=3 'highpass  
list laveb3
!
gen lavec 1 2048  'half
!
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT" 
                     
list lavec1 (1,1,1,10)
label-list lavec1
!
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"  
!
!      test LAVE for fullword data.
!
gen lavea 11 10 'full
!
lave lavea lavea1
list lavea1
!
lave lavea lavea2 'vert exclude=0
list lavea2
!
lave lavea lavea3  (6,2,5,7) 'horiz 'image
list lavea3
!
lave lavea laveb1 'stdev
list laveb1
!
lave lavea laveb2 filter=3
list laveb2
!
lave lavea laveb3 filter=3 'highpass
list laveb3
!
gen lavec 1 2048 'full
!
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
list lavec1 (1,1,1,10)
label-list lavec1
!
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
!
!      test LAVE for real*4 data.
!
gen lavea 11 10 'real
!
lave lavea lavea1
list lavea1
!
lave lavea lavea2 'vert exclude=0
list lavea2
!
lave lavea lavea3  (6,2,5,7) 'horiz 'image
list lavea3
!
lave lavea laveb1 'stdev
list laveb1
!
lave lavea laveb2 filter=3
list laveb2
!
lave lavea laveb3 filter=3 'highpass
list laveb3
!
gen lavec 1 2048 'real
!
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
list lavec1 (1,1,1,10)
label-list lavec1
!
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
!   CLEANUP
!
let $echo="no"
!
end-proc

$!-----------------------------------------------------------------------------
$ create tstlave.log
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

gen lavea 11 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavea lavea1
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   11   5   6   7   8   9  10  11  12  13  14  15
LAVE PROCESSING COMPLETE
list lavea1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7       9      11
   Line
      1       5   6   7   8   9  10  11  12  13  14  15
lave lavea lavea2 'vert exclude=0
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10   6   6   7   8   9  10  11  12  13  14
LAVE PROCESSING COMPLETE
list lavea2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7       9
   Line
      1       6   6   7   8   9  10  11  12  13  14
lave lavea lavea3  (6,2,5,7) 'horiz 'image
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-    5   9  10  11  12  13
LAVE PROCESSING COMPLETE
list lavea3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7
   Line
      1       9   9   9   9   9   9   9
      2      10  10  10  10  10  10  10
      3      11  11  11  11  11  11  11
      4      12  12  12  12  12  12  12
      5      13  13  13  13  13  13  13
qsar lavea laveb area=(3,3,6,6,100)
Beginning VICAR task qsar
QSAR version 08-SEP-03
lave laveb laveb1 'stdev
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  STANDARD DEVIATION VALUES
    1-   11   3   3  49  49  49  49  49  49   3   3   3
LAVE PROCESSING COMPLETE
list laveb1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7       9      11
   Line
      1       3   3  49  49  49  49  49  49   3   3   3
lave laveb laveb2 filter=3
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   11   5   6  67  68  69  70  71  72  13  14  15
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   11   5  26  47  68  69  70  71  52  33  14  14
LAVE PROCESSING COMPLETE
list laveb2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7       9      11
   Line
      1       5  26  47  68  69  70  71  52  33  14  14
lave laveb laveb3 filter=3 'highpass
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   11   5   6  67  68  69  70  71  72  13  14  15
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   11 128 108 148 128 128 128 128 148 108 128 129
LAVE PROCESSING COMPLETE
list laveb3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7       9      11
   Line
      1     128 108 148 128 128 128 128 148 108 128 129
gen lavec 1 2048
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  1


     SAMP       1           3           5           7           9          11          13          15          17
 LINE
    1           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
    2          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
    3          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
    4          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
    5         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
    6         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
    7         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
    8         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
    9           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   10          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   11          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   12          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   13         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   14         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   15         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   16         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
   17           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   18          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   19          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   20          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   21         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   22         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   23         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   24         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
   25           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   26          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   27          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   28          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   29         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   30         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   31         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   32         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241


     SAMP      19          21          23          25          27          29          31
 LINE
    1          18    19    20    21    22    23    24    25    26    27    28    29    30    31
    2          50    51    52    53    54    55    56    57    58    59    60    61    62    63
    3          82    83    84    85    86    87    88    89    90    91    92    93    94    95
    4         114   115   116   117   118   119   120   121   122   123   124   125   126   127
    5         146   147   148   149   150   151   152   153   154   155   156   157   158   159
    6         178   179   180   181   182   183   184   185   186   187   188   189   190   191
    7         210   211   212   213   214   215   216   217   218   219   220   221   222   223
    8         242   243   244   245   246   247   248   249   250   251   252   253   254   255
    9          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   10          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   11          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   12         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   13         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   14         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   15         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   16         242   243   244   245   246   247   248   249   250   251   252   253   254   255
   17          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   18          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   19          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   20         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   21         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   22         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   23         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   24         242   243   244   245   246   247   248   249   250   251   252   253   254   255
   25          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   26          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   27          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   28         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   29         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   30         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   31         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   32         242   243   244   245   246   247   248   249   250   251   252   253   254   255





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  2


     SAMP       1           3           5           7           9          11          13          15          17
 LINE
    1           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
    2          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
    3          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
    4          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
    5         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
    6         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
    7         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
    8         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
    9           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   10          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   11          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   12          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   13         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   14         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   15         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   16         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
   17           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   18          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   19          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   20          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   21         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   22         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   23         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   24         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
   25           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   26          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   27          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   28          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   29         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   30         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   31         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   32         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241


     SAMP      19          21          23          25          27          29          31
 LINE
    1          18    19    20    21    22    23    24    25    26    27    28    29    30    31
    2          50    51    52    53    54    55    56    57    58    59    60    61    62    63
    3          82    83    84    85    86    87    88    89    90    91    92    93    94    95
    4         114   115   116   117   118   119   120   121   122   123   124   125   126   127
    5         146   147   148   149   150   151   152   153   154   155   156   157   158   159
    6         178   179   180   181   182   183   184   185   186   187   188   189   190   191
    7         210   211   212   213   214   215   216   217   218   219   220   221   222   223
    8         242   243   244   245   246   247   248   249   250   251   252   253   254   255
    9          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   10          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   11          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   12         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   13         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   14         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   15         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   16         242   243   244   245   246   247   248   249   250   251   252   253   254   255
   17          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   18          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   19          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   20         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   21         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   22         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   23         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   24         242   243   244   245   246   247   248   249   250   251   252   253   254   255
   25          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   26          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   27          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   28         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   29         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   30         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   31         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   32         242   243   244   245   246   247   248   249   250   251   252   253   254   255
LAVE PROCESSING COMPLETE
list lavec1 (1,1,1,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
label-list lavec1
Beginning VICAR task label
************************************************************
 
        ************  File lavec1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in BYTE format from a X86-LINUX host
                1 bands
                1 lines per band
                2048 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: wlb -- Wed Mar 18 21:52:32 2015 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LAVE -- User: wlb -- Wed Mar 18 21:52:32 2015 ----
 
************************************************************
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS DATA IN AIS FORMAT


     SAMP       1           3           5           7           9          11          13          15          17
 LINE
    1           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
    2          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
    3          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
    4          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
    5         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
    6         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
    7         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
    8         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
    9           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   10          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   11          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   12          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   13         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   14         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   15         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   16         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
   17           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   18          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   19          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   20          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   21         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   22         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   23         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   24         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
   25           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
   26          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
   27          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
   28          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
   29         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
   30         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
   31         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
   32         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241


     SAMP      19          21          23          25          27          29          31
 LINE
    1          18    19    20    21    22    23    24    25    26    27    28    29    30    31
    2          50    51    52    53    54    55    56    57    58    59    60    61    62    63
    3          82    83    84    85    86    87    88    89    90    91    92    93    94    95
    4         114   115   116   117   118   119   120   121   122   123   124   125   126   127
    5         146   147   148   149   150   151   152   153   154   155   156   157   158   159
    6         178   179   180   181   182   183   184   185   186   187   188   189   190   191
    7         210   211   212   213   214   215   216   217   218   219   220   221   222   223
    8         242   243   244   245   246   247   248   249   250   251   252   253   254   255
    9          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   10          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   11          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   12         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   13         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   14         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   15         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   16         242   243   244   245   246   247   248   249   250   251   252   253   254   255
   17          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   18          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   19          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   20         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   21         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   22         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   23         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   24         242   243   244   245   246   247   248   249   250   251   252   253   254   255
   25          18    19    20    21    22    23    24    25    26    27    28    29    30    31
   26          50    51    52    53    54    55    56    57    58    59    60    61    62    63
   27          82    83    84    85    86    87    88    89    90    91    92    93    94    95
   28         114   115   116   117   118   119   120   121   122   123   124   125   126   127
   29         146   147   148   149   150   151   152   153   154   155   156   157   158   159
   30         178   179   180   181   182   183   184   185   186   187   188   189   190   191
   31         210   211   212   213   214   215   216   217   218   219   220   221   222   223
   32         242   243   244   245   246   247   248   249   250   251   252   253   254   255
LAVE PROCESSING COMPLETE
gen lavea 11 10  'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavea lavea1
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   11     5     6     7     8     9    10    11    12    13    14    15
LAVE PROCESSING COMPLETE
list lavea1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7     8     9    10    11
   Line
      1         5     6     7     8     9    10    11    12    13    14    15
lave lavea lavea2 'vert exclude=0
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10     6     6     7     8     9    10    11    12    13    14
LAVE PROCESSING COMPLETE
list lavea2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         6     6     7     8     9    10    11    12    13    14
lave lavea lavea3  (6,2,5,7) 'horiz 'image
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-    5     9    10    11    12    13
LAVE PROCESSING COMPLETE
list lavea3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7
   Line
      1         9     9     9     9     9     9     9
      2        10    10    10    10    10    10    10
      3        11    11    11    11    11    11    11
      4        12    12    12    12    12    12    12
      5        13    13    13    13    13    13    13
qsar lavea laveb area=(3,3,6,6,100)
Beginning VICAR task qsar
QSAR version 08-SEP-03
lave laveb laveb1 'stdev
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  STANDARD DEVIATION VALUES
    1-   11     3     3    49    49    49    49    49    49     3     3     3
LAVE PROCESSING COMPLETE
list laveb1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7     8     9    10    11
   Line
      1         3     3    49    49    49    49    49    49     3     3     3
lave laveb laveb2 filter=3
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   11     5     6    67    68    69    70    71    72    13    14    15
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   11     5    26    47    68    69    70    71    52    33    14    14
LAVE PROCESSING COMPLETE
list laveb2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7     8     9    10    11
   Line
      1         5    26    47    68    69    70    71    52    33    14    14
lave laveb laveb3 filter=3 'highpass
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   11     5     6    67    68    69    70    71    72    13    14    15
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   11    -1   -20    20     0     0     0     0    20   -20     0     1
LAVE PROCESSING COMPLETE
list laveb3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7     8     9    10    11
   Line
      1        -1   -20    20     0     0     0     0    20   -20     0     1
gen lavec 1 2048  'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  1


     SAMP       1           3           5           7           9          11          13          15          17
 LINE
    1           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
    2          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
    3          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
    4          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
    5         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
    6         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
    7         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
    8         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
    9         256   257   258   259   260   261   262   263   264   265   266   267   268   269   270   271   272   273
   10         288   289   290   291   292   293   294   295   296   297   298   299   300   301   302   303   304   305
   11         320   321   322   323   324   325   326   327   328   329   330   331   332   333   334   335   336   337
   12         352   353   354   355   356   357   358   359   360   361   362   363   364   365   366   367   368   369
   13         384   385   386   387   388   389   390   391   392   393   394   395   396   397   398   399   400   401
   14         416   417   418   419   420   421   422   423   424   425   426   427   428   429   430   431   432   433
   15         448   449   450   451   452   453   454   455   456   457   458   459   460   461   462   463   464   465
   16         480   481   482   483   484   485   486   487   488   489   490   491   492   493   494   495   496   497
   17         512   513   514   515   516   517   518   519   520   521   522   523   524   525   526   527   528   529
   18         544   545   546   547   548   549   550   551   552   553   554   555   556   557   558   559   560   561
   19         576   577   578   579   580   581   582   583   584   585   586   587   588   589   590   591   592   593
   20         608   609   610   611   612   613   614   615   616   617   618   619   620   621   622   623   624   625
   21         640   641   642   643   644   645   646   647   648   649   650   651   652   653   654   655   656   657
   22         672   673   674   675   676   677   678   679   680   681   682   683   684   685   686   687   688   689
   23         704   705   706   707   708   709   710   711   712   713   714   715   716   717   718   719   720   721
   24         736   737   738   739   740   741   742   743   744   745   746   747   748   749   750   751   752   753
   25         768   769   770   771   772   773   774   775   776   777   778   779   780   781   782   783   784   785
   26         800   801   802   803   804   805   806   807   808   809   810   811   812   813   814   815   816   817
   27         832   833   834   835   836   837   838   839   840   841   842   843   844   845   846   847   848   849
   28         864   865   866   867   868   869   870   871   872   873   874   875   876   877   878   879   880   881
   29         896   897   898   899   900   901   902   903   904   905   906   907   908   909   910   911   912   913
   30         928   929   930   931   932   933   934   935   936   937   938   939   940   941   942   943   944   945
   31         960   961   962   963   964   965   966   967   968   969   970   971   972   973   974   975   976   977
   32         992   993   994   995   996   997   998   999  1000  1001  1002  1003  1004  1005  1006  1007  1008  1009


     SAMP      19          21          23          25          27          29          31
 LINE
    1          18    19    20    21    22    23    24    25    26    27    28    29    30    31
    2          50    51    52    53    54    55    56    57    58    59    60    61    62    63
    3          82    83    84    85    86    87    88    89    90    91    92    93    94    95
    4         114   115   116   117   118   119   120   121   122   123   124   125   126   127
    5         146   147   148   149   150   151   152   153   154   155   156   157   158   159
    6         178   179   180   181   182   183   184   185   186   187   188   189   190   191
    7         210   211   212   213   214   215   216   217   218   219   220   221   222   223
    8         242   243   244   245   246   247   248   249   250   251   252   253   254   255
    9         274   275   276   277   278   279   280   281   282   283   284   285   286   287
   10         306   307   308   309   310   311   312   313   314   315   316   317   318   319
   11         338   339   340   341   342   343   344   345   346   347   348   349   350   351
   12         370   371   372   373   374   375   376   377   378   379   380   381   382   383
   13         402   403   404   405   406   407   408   409   410   411   412   413   414   415
   14         434   435   436   437   438   439   440   441   442   443   444   445   446   447
   15         466   467   468   469   470   471   472   473   474   475   476   477   478   479
   16         498   499   500   501   502   503   504   505   506   507   508   509   510   511
   17         530   531   532   533   534   535   536   537   538   539   540   541   542   543
   18         562   563   564   565   566   567   568   569   570   571   572   573   574   575
   19         594   595   596   597   598   599   600   601   602   603   604   605   606   607
   20         626   627   628   629   630   631   632   633   634   635   636   637   638   639
   21         658   659   660   661   662   663   664   665   666   667   668   669   670   671
   22         690   691   692   693   694   695   696   697   698   699   700   701   702   703
   23         722   723   724   725   726   727   728   729   730   731   732   733   734   735
   24         754   755   756   757   758   759   760   761   762   763   764   765   766   767
   25         786   787   788   789   790   791   792   793   794   795   796   797   798   799
   26         818   819   820   821   822   823   824   825   826   827   828   829   830   831
   27         850   851   852   853   854   855   856   857   858   859   860   861   862   863
   28         882   883   884   885   886   887   888   889   890   891   892   893   894   895
   29         914   915   916   917   918   919   920   921   922   923   924   925   926   927
   30         946   947   948   949   950   951   952   953   954   955   956   957   958   959
   31         978   979   980   981   982   983   984   985   986   987   988   989   990   991
   32        1010  1011  1012  1013  1014  1015  1016  1017  1018  1019  1020  1021  1022  1023





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  2


     SAMP       1           3           5           7           9          11          13          15          17
 LINE
    1        1024  1025  1026  1027  1028  1029  1030  1031  1032  1033  1034  1035  1036  1037  1038  1039  1040  1041
    2        1056  1057  1058  1059  1060  1061  1062  1063  1064  1065  1066  1067  1068  1069  1070  1071  1072  1073
    3        1088  1089  1090  1091  1092  1093  1094  1095  1096  1097  1098  1099  1100  1101  1102  1103  1104  1105
    4        1120  1121  1122  1123  1124  1125  1126  1127  1128  1129  1130  1131  1132  1133  1134  1135  1136  1137
    5        1152  1153  1154  1155  1156  1157  1158  1159  1160  1161  1162  1163  1164  1165  1166  1167  1168  1169
    6        1184  1185  1186  1187  1188  1189  1190  1191  1192  1193  1194  1195  1196  1197  1198  1199  1200  1201
    7        1216  1217  1218  1219  1220  1221  1222  1223  1224  1225  1226  1227  1228  1229  1230  1231  1232  1233
    8        1248  1249  1250  1251  1252  1253  1254  1255  1256  1257  1258  1259  1260  1261  1262  1263  1264  1265
    9        1280  1281  1282  1283  1284  1285  1286  1287  1288  1289  1290  1291  1292  1293  1294  1295  1296  1297
   10        1312  1313  1314  1315  1316  1317  1318  1319  1320  1321  1322  1323  1324  1325  1326  1327  1328  1329
   11        1344  1345  1346  1347  1348  1349  1350  1351  1352  1353  1354  1355  1356  1357  1358  1359  1360  1361
   12        1376  1377  1378  1379  1380  1381  1382  1383  1384  1385  1386  1387  1388  1389  1390  1391  1392  1393
   13        1408  1409  1410  1411  1412  1413  1414  1415  1416  1417  1418  1419  1420  1421  1422  1423  1424  1425
   14        1440  1441  1442  1443  1444  1445  1446  1447  1448  1449  1450  1451  1452  1453  1454  1455  1456  1457
   15        1472  1473  1474  1475  1476  1477  1478  1479  1480  1481  1482  1483  1484  1485  1486  1487  1488  1489
   16        1504  1505  1506  1507  1508  1509  1510  1511  1512  1513  1514  1515  1516  1517  1518  1519  1520  1521
   17        1536  1537  1538  1539  1540  1541  1542  1543  1544  1545  1546  1547  1548  1549  1550  1551  1552  1553
   18        1568  1569  1570  1571  1572  1573  1574  1575  1576  1577  1578  1579  1580  1581  1582  1583  1584  1585
   19        1600  1601  1602  1603  1604  1605  1606  1607  1608  1609  1610  1611  1612  1613  1614  1615  1616  1617
   20        1632  1633  1634  1635  1636  1637  1638  1639  1640  1641  1642  1643  1644  1645  1646  1647  1648  1649
   21        1664  1665  1666  1667  1668  1669  1670  1671  1672  1673  1674  1675  1676  1677  1678  1679  1680  1681
   22        1696  1697  1698  1699  1700  1701  1702  1703  1704  1705  1706  1707  1708  1709  1710  1711  1712  1713
   23        1728  1729  1730  1731  1732  1733  1734  1735  1736  1737  1738  1739  1740  1741  1742  1743  1744  1745
   24        1760  1761  1762  1763  1764  1765  1766  1767  1768  1769  1770  1771  1772  1773  1774  1775  1776  1777
   25        1792  1793  1794  1795  1796  1797  1798  1799  1800  1801  1802  1803  1804  1805  1806  1807  1808  1809
   26        1824  1825  1826  1827  1828  1829  1830  1831  1832  1833  1834  1835  1836  1837  1838  1839  1840  1841
   27        1856  1857  1858  1859  1860  1861  1862  1863  1864  1865  1866  1867  1868  1869  1870  1871  1872  1873
   28        1888  1889  1890  1891  1892  1893  1894  1895  1896  1897  1898  1899  1900  1901  1902  1903  1904  1905
   29        1920  1921  1922  1923  1924  1925  1926  1927  1928  1929  1930  1931  1932  1933  1934  1935  1936  1937
   30        1952  1953  1954  1955  1956  1957  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968  1969
   31        1984  1985  1986  1987  1988  1989  1990  1991  1992  1993  1994  1995  1996  1997  1998  1999  2000  2001
   32        2016  2017  2018  2019  2020  2021  2022  2023  2024  2025  2026  2027  2028  2029  2030  2031  2032  2033


     SAMP      19          21          23          25          27          29          31
 LINE
    1        1042  1043  1044  1045  1046  1047  1048  1049  1050  1051  1052  1053  1054  1055
    2        1074  1075  1076  1077  1078  1079  1080  1081  1082  1083  1084  1085  1086  1087
    3        1106  1107  1108  1109  1110  1111  1112  1113  1114  1115  1116  1117  1118  1119
    4        1138  1139  1140  1141  1142  1143  1144  1145  1146  1147  1148  1149  1150  1151
    5        1170  1171  1172  1173  1174  1175  1176  1177  1178  1179  1180  1181  1182  1183
    6        1202  1203  1204  1205  1206  1207  1208  1209  1210  1211  1212  1213  1214  1215
    7        1234  1235  1236  1237  1238  1239  1240  1241  1242  1243  1244  1245  1246  1247
    8        1266  1267  1268  1269  1270  1271  1272  1273  1274  1275  1276  1277  1278  1279
    9        1298  1299  1300  1301  1302  1303  1304  1305  1306  1307  1308  1309  1310  1311
   10        1330  1331  1332  1333  1334  1335  1336  1337  1338  1339  1340  1341  1342  1343
   11        1362  1363  1364  1365  1366  1367  1368  1369  1370  1371  1372  1373  1374  1375
   12        1394  1395  1396  1397  1398  1399  1400  1401  1402  1403  1404  1405  1406  1407
   13        1426  1427  1428  1429  1430  1431  1432  1433  1434  1435  1436  1437  1438  1439
   14        1458  1459  1460  1461  1462  1463  1464  1465  1466  1467  1468  1469  1470  1471
   15        1490  1491  1492  1493  1494  1495  1496  1497  1498  1499  1500  1501  1502  1503
   16        1522  1523  1524  1525  1526  1527  1528  1529  1530  1531  1532  1533  1534  1535
   17        1554  1555  1556  1557  1558  1559  1560  1561  1562  1563  1564  1565  1566  1567
   18        1586  1587  1588  1589  1590  1591  1592  1593  1594  1595  1596  1597  1598  1599
   19        1618  1619  1620  1621  1622  1623  1624  1625  1626  1627  1628  1629  1630  1631
   20        1650  1651  1652  1653  1654  1655  1656  1657  1658  1659  1660  1661  1662  1663
   21        1682  1683  1684  1685  1686  1687  1688  1689  1690  1691  1692  1693  1694  1695
   22        1714  1715  1716  1717  1718  1719  1720  1721  1722  1723  1724  1725  1726  1727
   23        1746  1747  1748  1749  1750  1751  1752  1753  1754  1755  1756  1757  1758  1759
   24        1778  1779  1780  1781  1782  1783  1784  1785  1786  1787  1788  1789  1790  1791
   25        1810  1811  1812  1813  1814  1815  1816  1817  1818  1819  1820  1821  1822  1823
   26        1842  1843  1844  1845  1846  1847  1848  1849  1850  1851  1852  1853  1854  1855
   27        1874  1875  1876  1877  1878  1879  1880  1881  1882  1883  1884  1885  1886  1887
   28        1906  1907  1908  1909  1910  1911  1912  1913  1914  1915  1916  1917  1918  1919
   29        1938  1939  1940  1941  1942  1943  1944  1945  1946  1947  1948  1949  1950  1951
   30        1970  1971  1972  1973  1974  1975  1976  1977  1978  1979  1980  1981  1982  1983
   31        2002  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015
   32        2034  2035  2036  2037  2038  2039  2040  2041  2042  2043  2044  2045  2046  2047
LAVE PROCESSING COMPLETE
list lavec1 (1,1,1,10)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
label-list lavec1
Beginning VICAR task label
************************************************************
 
        ************  File lavec1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in HALF format from a X86-LINUX host
                1 bands
                1 lines per band
                2048 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: wlb -- Wed Mar 18 21:52:32 2015 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LAVE -- User: wlb -- Wed Mar 18 21:52:32 2015 ----
 
************************************************************
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS DATA IN AIS FORMAT


     SAMP       1           3           5           7           9          11          13          15          17
 LINE
    1           0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
    2          32    33    34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49
    3          64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80    81
    4          96    97    98    99   100   101   102   103   104   105   106   107   108   109   110   111   112   113
    5         128   129   130   131   132   133   134   135   136   137   138   139   140   141   142   143   144   145
    6         160   161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177
    7         192   193   194   195   196   197   198   199   200   201   202   203   204   205   206   207   208   209
    8         224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240   241
    9         256   257   258   259   260   261   262   263   264   265   266   267   268   269   270   271   272   273
   10         288   289   290   291   292   293   294   295   296   297   298   299   300   301   302   303   304   305
   11         320   321   322   323   324   325   326   327   328   329   330   331   332   333   334   335   336   337
   12         352   353   354   355   356   357   358   359   360   361   362   363   364   365   366   367   368   369
   13         384   385   386   387   388   389   390   391   392   393   394   395   396   397   398   399   400   401
   14         416   417   418   419   420   421   422   423   424   425   426   427   428   429   430   431   432   433
   15         448   449   450   451   452   453   454   455   456   457   458   459   460   461   462   463   464   465
   16         480   481   482   483   484   485   486   487   488   489   490   491   492   493   494   495   496   497
   17         512   513   514   515   516   517   518   519   520   521   522   523   524   525   526   527   528   529
   18         544   545   546   547   548   549   550   551   552   553   554   555   556   557   558   559   560   561
   19         576   577   578   579   580   581   582   583   584   585   586   587   588   589   590   591   592   593
   20         608   609   610   611   612   613   614   615   616   617   618   619   620   621   622   623   624   625
   21         640   641   642   643   644   645   646   647   648   649   650   651   652   653   654   655   656   657
   22         672   673   674   675   676   677   678   679   680   681   682   683   684   685   686   687   688   689
   23         704   705   706   707   708   709   710   711   712   713   714   715   716   717   718   719   720   721
   24         736   737   738   739   740   741   742   743   744   745   746   747   748   749   750   751   752   753
   25         768   769   770   771   772   773   774   775   776   777   778   779   780   781   782   783   784   785
   26         800   801   802   803   804   805   806   807   808   809   810   811   812   813   814   815   816   817
   27         832   833   834   835   836   837   838   839   840   841   842   843   844   845   846   847   848   849
   28         864   865   866   867   868   869   870   871   872   873   874   875   876   877   878   879   880   881
   29         896   897   898   899   900   901   902   903   904   905   906   907   908   909   910   911   912   913
   30         928   929   930   931   932   933   934   935   936   937   938   939   940   941   942   943   944   945
   31         960   961   962   963   964   965   966   967   968   969   970   971   972   973   974   975   976   977
   32         992   993   994   995   996   997   998   999  1000  1001  1002  1003  1004  1005  1006  1007  1008  1009


     SAMP      19          21          23          25          27          29          31
 LINE
    1          18    19    20    21    22    23    24    25    26    27    28    29    30    31
    2          50    51    52    53    54    55    56    57    58    59    60    61    62    63
    3          82    83    84    85    86    87    88    89    90    91    92    93    94    95
    4         114   115   116   117   118   119   120   121   122   123   124   125   126   127
    5         146   147   148   149   150   151   152   153   154   155   156   157   158   159
    6         178   179   180   181   182   183   184   185   186   187   188   189   190   191
    7         210   211   212   213   214   215   216   217   218   219   220   221   222   223
    8         242   243   244   245   246   247   248   249   250   251   252   253   254   255
    9         274   275   276   277   278   279   280   281   282   283   284   285   286   287
   10         306   307   308   309   310   311   312   313   314   315   316   317   318   319
   11         338   339   340   341   342   343   344   345   346   347   348   349   350   351
   12         370   371   372   373   374   375   376   377   378   379   380   381   382   383
   13         402   403   404   405   406   407   408   409   410   411   412   413   414   415
   14         434   435   436   437   438   439   440   441   442   443   444   445   446   447
   15         466   467   468   469   470   471   472   473   474   475   476   477   478   479
   16         498   499   500   501   502   503   504   505   506   507   508   509   510   511
   17         530   531   532   533   534   535   536   537   538   539   540   541   542   543
   18         562   563   564   565   566   567   568   569   570   571   572   573   574   575
   19         594   595   596   597   598   599   600   601   602   603   604   605   606   607
   20         626   627   628   629   630   631   632   633   634   635   636   637   638   639
   21         658   659   660   661   662   663   664   665   666   667   668   669   670   671
   22         690   691   692   693   694   695   696   697   698   699   700   701   702   703
   23         722   723   724   725   726   727   728   729   730   731   732   733   734   735
   24         754   755   756   757   758   759   760   761   762   763   764   765   766   767
   25         786   787   788   789   790   791   792   793   794   795   796   797   798   799
   26         818   819   820   821   822   823   824   825   826   827   828   829   830   831
   27         850   851   852   853   854   855   856   857   858   859   860   861   862   863
   28         882   883   884   885   886   887   888   889   890   891   892   893   894   895
   29         914   915   916   917   918   919   920   921   922   923   924   925   926   927
   30         946   947   948   949   950   951   952   953   954   955   956   957   958   959
   31         978   979   980   981   982   983   984   985   986   987   988   989   990   991
   32        1010  1011  1012  1013  1014  1015  1016  1017  1018  1019  1020  1021  1022  1023
LAVE PROCESSING COMPLETE
gen lavea 11 10 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavea lavea1
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10          5          6          7          8          9         10         11         12         13         14
   11-   11         15
LAVE PROCESSING COMPLETE
list lavea1
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              5          6          7          8          9         10         11         12         13         14

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp           11
   Line
      1             15
lave lavea lavea2 'vert exclude=0
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10          6          6          7          8          9         10         11         12         13         14
LAVE PROCESSING COMPLETE
list lavea2
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              6          6          7          8          9         10         11         12         13         14
lave lavea lavea3  (6,2,5,7) 'horiz 'image
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-    5          9         10         11         12         13
LAVE PROCESSING COMPLETE
list lavea3
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp            1          2          3          4          5          6          7
   Line
      1              9          9          9          9          9          9          9
      2             10         10         10         10         10         10         10
      3             11         11         11         11         11         11         11
      4             12         12         12         12         12         12         12
      5             13         13         13         13         13         13         13
lave lavea laveb1 'stdev
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  STANDARD DEVIATION VALUES
    1-   10          3          3          3          3          3          3          3          3          3          3
   11-   11          3
LAVE PROCESSING COMPLETE
list laveb1
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              3          3          3          3          3          3          3          3          3          3

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp           11
   Line
      1              3
lave lavea laveb2 filter=3
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10          5          6          7          8          9         10         11         12         13         14
   11-   11         15
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   10          5          6          7          8          9         10         11         12         13         14
   11-   11         14
LAVE PROCESSING COMPLETE
list laveb2
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              5          6          7          8          9         10         11         12         13         14

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
     Samp           11
   Line
      1             14
lave lavea laveb3 filter=3 'highpass
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10          5          6          7          8          9         10         11         12         13         14
   11-   11         15
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   10         -1          0          0          0          0          0          0          0          0          0
   11-   11          1
LAVE PROCESSING COMPLETE
list laveb3
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1             -1          0          0          0          0          0          0          0          0          0

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:32 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp           11
   Line
      1              1
gen lavec 1 2048 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  1


     SAMP            1                     3                     5                     7                     9
 LINE
    1                0          1          2          3          4          5          6          7          8
    2               32         33         34         35         36         37         38         39         40
    3               64         65         66         67         68         69         70         71         72
    4               96         97         98         99        100        101        102        103        104
    5              128        129        130        131        132        133        134        135        136
    6              160        161        162        163        164        165        166        167        168
    7              192        193        194        195        196        197        198        199        200
    8              224        225        226        227        228        229        230        231        232
    9              256        257        258        259        260        261        262        263        264
   10              288        289        290        291        292        293        294        295        296
   11              320        321        322        323        324        325        326        327        328
   12              352        353        354        355        356        357        358        359        360
   13              384        385        386        387        388        389        390        391        392
   14              416        417        418        419        420        421        422        423        424
   15              448        449        450        451        452        453        454        455        456
   16              480        481        482        483        484        485        486        487        488
   17              512        513        514        515        516        517        518        519        520
   18              544        545        546        547        548        549        550        551        552
   19              576        577        578        579        580        581        582        583        584
   20              608        609        610        611        612        613        614        615        616
   21              640        641        642        643        644        645        646        647        648
   22              672        673        674        675        676        677        678        679        680
   23              704        705        706        707        708        709        710        711        712
   24              736        737        738        739        740        741        742        743        744
   25              768        769        770        771        772        773        774        775        776
   26              800        801        802        803        804        805        806        807        808
   27              832        833        834        835        836        837        838        839        840
   28              864        865        866        867        868        869        870        871        872
   29              896        897        898        899        900        901        902        903        904
   30              928        929        930        931        932        933        934        935        936
   31              960        961        962        963        964        965        966        967        968
   32              992        993        994        995        996        997        998        999       1000


     SAMP           10                    12                    14                    16                    18
 LINE
    1                9         10         11         12         13         14         15         16         17
    2               41         42         43         44         45         46         47         48         49
    3               73         74         75         76         77         78         79         80         81
    4              105        106        107        108        109        110        111        112        113
    5              137        138        139        140        141        142        143        144        145
    6              169        170        171        172        173        174        175        176        177
    7              201        202        203        204        205        206        207        208        209
    8              233        234        235        236        237        238        239        240        241
    9              265        266        267        268        269        270        271        272        273
   10              297        298        299        300        301        302        303        304        305
   11              329        330        331        332        333        334        335        336        337
   12              361        362        363        364        365        366        367        368        369
   13              393        394        395        396        397        398        399        400        401
   14              425        426        427        428        429        430        431        432        433
   15              457        458        459        460        461        462        463        464        465
   16              489        490        491        492        493        494        495        496        497
   17              521        522        523        524        525        526        527        528        529
   18              553        554        555        556        557        558        559        560        561
   19              585        586        587        588        589        590        591        592        593
   20              617        618        619        620        621        622        623        624        625
   21              649        650        651        652        653        654        655        656        657
   22              681        682        683        684        685        686        687        688        689
   23              713        714        715        716        717        718        719        720        721
   24              745        746        747        748        749        750        751        752        753
   25              777        778        779        780        781        782        783        784        785
   26              809        810        811        812        813        814        815        816        817
   27              841        842        843        844        845        846        847        848        849
   28              873        874        875        876        877        878        879        880        881
   29              905        906        907        908        909        910        911        912        913
   30              937        938        939        940        941        942        943        944        945
   31              969        970        971        972        973        974        975        976        977
   32             1001       1002       1003       1004       1005       1006       1007       1008       1009


     SAMP           19                    21                    23                    25                    27
 LINE
    1               18         19         20         21         22         23         24         25         26
    2               50         51         52         53         54         55         56         57         58
    3               82         83         84         85         86         87         88         89         90
    4              114        115        116        117        118        119        120        121        122
    5              146        147        148        149        150        151        152        153        154
    6              178        179        180        181        182        183        184        185        186
    7              210        211        212        213        214        215        216        217        218
    8              242        243        244        245        246        247        248        249        250
    9              274        275        276        277        278        279        280        281        282
   10              306        307        308        309        310        311        312        313        314
   11              338        339        340        341        342        343        344        345        346
   12              370        371        372        373        374        375        376        377        378
   13              402        403        404        405        406        407        408        409        410
   14              434        435        436        437        438        439        440        441        442
   15              466        467        468        469        470        471        472        473        474
   16              498        499        500        501        502        503        504        505        506
   17              530        531        532        533        534        535        536        537        538
   18              562        563        564        565        566        567        568        569        570
   19              594        595        596        597        598        599        600        601        602
   20              626        627        628        629        630        631        632        633        634
   21              658        659        660        661        662        663        664        665        666
   22              690        691        692        693        694        695        696        697        698
   23              722        723        724        725        726        727        728        729        730
   24              754        755        756        757        758        759        760        761        762
   25              786        787        788        789        790        791        792        793        794
   26              818        819        820        821        822        823        824        825        826
   27              850        851        852        853        854        855        856        857        858
   28              882        883        884        885        886        887        888        889        890
   29              914        915        916        917        918        919        920        921        922
   30              946        947        948        949        950        951        952        953        954
   31              978        979        980        981        982        983        984        985        986
   32             1010       1011       1012       1013       1014       1015       1016       1017       1018


     SAMP           28                    30                    32
 LINE
    1               27         28         29         30         31
    2               59         60         61         62         63
    3               91         92         93         94         95
    4              123        124        125        126        127
    5              155        156        157        158        159
    6              187        188        189        190        191
    7              219        220        221        222        223
    8              251        252        253        254        255
    9              283        284        285        286        287
   10              315        316        317        318        319
   11              347        348        349        350        351
   12              379        380        381        382        383
   13              411        412        413        414        415
   14              443        444        445        446        447
   15              475        476        477        478        479
   16              507        508        509        510        511
   17              539        540        541        542        543
   18              571        572        573        574        575
   19              603        604        605        606        607
   20              635        636        637        638        639
   21              667        668        669        670        671
   22              699        700        701        702        703
   23              731        732        733        734        735
   24              763        764        765        766        767
   25              795        796        797        798        799
   26              827        828        829        830        831
   27              859        860        861        862        863
   28              891        892        893        894        895
   29              923        924        925        926        927
   30              955        956        957        958        959
   31              987        988        989        990        991
   32             1019       1020       1021       1022       1023





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  2


     SAMP            1                     3                     5                     7                     9
 LINE
    1             1024       1025       1026       1027       1028       1029       1030       1031       1032
    2             1056       1057       1058       1059       1060       1061       1062       1063       1064
    3             1088       1089       1090       1091       1092       1093       1094       1095       1096
    4             1120       1121       1122       1123       1124       1125       1126       1127       1128
    5             1152       1153       1154       1155       1156       1157       1158       1159       1160
    6             1184       1185       1186       1187       1188       1189       1190       1191       1192
    7             1216       1217       1218       1219       1220       1221       1222       1223       1224
    8             1248       1249       1250       1251       1252       1253       1254       1255       1256
    9             1280       1281       1282       1283       1284       1285       1286       1287       1288
   10             1312       1313       1314       1315       1316       1317       1318       1319       1320
   11             1344       1345       1346       1347       1348       1349       1350       1351       1352
   12             1376       1377       1378       1379       1380       1381       1382       1383       1384
   13             1408       1409       1410       1411       1412       1413       1414       1415       1416
   14             1440       1441       1442       1443       1444       1445       1446       1447       1448
   15             1472       1473       1474       1475       1476       1477       1478       1479       1480
   16             1504       1505       1506       1507       1508       1509       1510       1511       1512
   17             1536       1537       1538       1539       1540       1541       1542       1543       1544
   18             1568       1569       1570       1571       1572       1573       1574       1575       1576
   19             1600       1601       1602       1603       1604       1605       1606       1607       1608
   20             1632       1633       1634       1635       1636       1637       1638       1639       1640
   21             1664       1665       1666       1667       1668       1669       1670       1671       1672
   22             1696       1697       1698       1699       1700       1701       1702       1703       1704
   23             1728       1729       1730       1731       1732       1733       1734       1735       1736
   24             1760       1761       1762       1763       1764       1765       1766       1767       1768
   25             1792       1793       1794       1795       1796       1797       1798       1799       1800
   26             1824       1825       1826       1827       1828       1829       1830       1831       1832
   27             1856       1857       1858       1859       1860       1861       1862       1863       1864
   28             1888       1889       1890       1891       1892       1893       1894       1895       1896
   29             1920       1921       1922       1923       1924       1925       1926       1927       1928
   30             1952       1953       1954       1955       1956       1957       1958       1959       1960
   31             1984       1985       1986       1987       1988       1989       1990       1991       1992
   32             2016       2017       2018       2019       2020       2021       2022       2023       2024


     SAMP           10                    12                    14                    16                    18
 LINE
    1             1033       1034       1035       1036       1037       1038       1039       1040       1041
    2             1065       1066       1067       1068       1069       1070       1071       1072       1073
    3             1097       1098       1099       1100       1101       1102       1103       1104       1105
    4             1129       1130       1131       1132       1133       1134       1135       1136       1137
    5             1161       1162       1163       1164       1165       1166       1167       1168       1169
    6             1193       1194       1195       1196       1197       1198       1199       1200       1201
    7             1225       1226       1227       1228       1229       1230       1231       1232       1233
    8             1257       1258       1259       1260       1261       1262       1263       1264       1265
    9             1289       1290       1291       1292       1293       1294       1295       1296       1297
   10             1321       1322       1323       1324       1325       1326       1327       1328       1329
   11             1353       1354       1355       1356       1357       1358       1359       1360       1361
   12             1385       1386       1387       1388       1389       1390       1391       1392       1393
   13             1417       1418       1419       1420       1421       1422       1423       1424       1425
   14             1449       1450       1451       1452       1453       1454       1455       1456       1457
   15             1481       1482       1483       1484       1485       1486       1487       1488       1489
   16             1513       1514       1515       1516       1517       1518       1519       1520       1521
   17             1545       1546       1547       1548       1549       1550       1551       1552       1553
   18             1577       1578       1579       1580       1581       1582       1583       1584       1585
   19             1609       1610       1611       1612       1613       1614       1615       1616       1617
   20             1641       1642       1643       1644       1645       1646       1647       1648       1649
   21             1673       1674       1675       1676       1677       1678       1679       1680       1681
   22             1705       1706       1707       1708       1709       1710       1711       1712       1713
   23             1737       1738       1739       1740       1741       1742       1743       1744       1745
   24             1769       1770       1771       1772       1773       1774       1775       1776       1777
   25             1801       1802       1803       1804       1805       1806       1807       1808       1809
   26             1833       1834       1835       1836       1837       1838       1839       1840       1841
   27             1865       1866       1867       1868       1869       1870       1871       1872       1873
   28             1897       1898       1899       1900       1901       1902       1903       1904       1905
   29             1929       1930       1931       1932       1933       1934       1935       1936       1937
   30             1961       1962       1963       1964       1965       1966       1967       1968       1969
   31             1993       1994       1995       1996       1997       1998       1999       2000       2001
   32             2025       2026       2027       2028       2029       2030       2031       2032       2033


     SAMP           19                    21                    23                    25                    27
 LINE
    1             1042       1043       1044       1045       1046       1047       1048       1049       1050
    2             1074       1075       1076       1077       1078       1079       1080       1081       1082
    3             1106       1107       1108       1109       1110       1111       1112       1113       1114
    4             1138       1139       1140       1141       1142       1143       1144       1145       1146
    5             1170       1171       1172       1173       1174       1175       1176       1177       1178
    6             1202       1203       1204       1205       1206       1207       1208       1209       1210
    7             1234       1235       1236       1237       1238       1239       1240       1241       1242
    8             1266       1267       1268       1269       1270       1271       1272       1273       1274
    9             1298       1299       1300       1301       1302       1303       1304       1305       1306
   10             1330       1331       1332       1333       1334       1335       1336       1337       1338
   11             1362       1363       1364       1365       1366       1367       1368       1369       1370
   12             1394       1395       1396       1397       1398       1399       1400       1401       1402
   13             1426       1427       1428       1429       1430       1431       1432       1433       1434
   14             1458       1459       1460       1461       1462       1463       1464       1465       1466
   15             1490       1491       1492       1493       1494       1495       1496       1497       1498
   16             1522       1523       1524       1525       1526       1527       1528       1529       1530
   17             1554       1555       1556       1557       1558       1559       1560       1561       1562
   18             1586       1587       1588       1589       1590       1591       1592       1593       1594
   19             1618       1619       1620       1621       1622       1623       1624       1625       1626
   20             1650       1651       1652       1653       1654       1655       1656       1657       1658
   21             1682       1683       1684       1685       1686       1687       1688       1689       1690
   22             1714       1715       1716       1717       1718       1719       1720       1721       1722
   23             1746       1747       1748       1749       1750       1751       1752       1753       1754
   24             1778       1779       1780       1781       1782       1783       1784       1785       1786
   25             1810       1811       1812       1813       1814       1815       1816       1817       1818
   26             1842       1843       1844       1845       1846       1847       1848       1849       1850
   27             1874       1875       1876       1877       1878       1879       1880       1881       1882
   28             1906       1907       1908       1909       1910       1911       1912       1913       1914
   29             1938       1939       1940       1941       1942       1943       1944       1945       1946
   30             1970       1971       1972       1973       1974       1975       1976       1977       1978
   31             2002       2003       2004       2005       2006       2007       2008       2009       2010
   32             2034       2035       2036       2037       2038       2039       2040       2041       2042


     SAMP           28                    30                    32
 LINE
    1             1051       1052       1053       1054       1055
    2             1083       1084       1085       1086       1087
    3             1115       1116       1117       1118       1119
    4             1147       1148       1149       1150       1151
    5             1179       1180       1181       1182       1183
    6             1211       1212       1213       1214       1215
    7             1243       1244       1245       1246       1247
    8             1275       1276       1277       1278       1279
    9             1307       1308       1309       1310       1311
   10             1339       1340       1341       1342       1343
   11             1371       1372       1373       1374       1375
   12             1403       1404       1405       1406       1407
   13             1435       1436       1437       1438       1439
   14             1467       1468       1469       1470       1471
   15             1499       1500       1501       1502       1503
   16             1531       1532       1533       1534       1535
   17             1563       1564       1565       1566       1567
   18             1595       1596       1597       1598       1599
   19             1627       1628       1629       1630       1631
   20             1659       1660       1661       1662       1663
   21             1691       1692       1693       1694       1695
   22             1723       1724       1725       1726       1727
   23             1755       1756       1757       1758       1759
   24             1787       1788       1789       1790       1791
   25             1819       1820       1821       1822       1823
   26             1851       1852       1853       1854       1855
   27             1883       1884       1885       1886       1887
   28             1915       1916       1917       1918       1919
   29             1947       1948       1949       1950       1951
   30             1979       1980       1981       1982       1983
   31             2011       2012       2013       2014       2015
   32             2043       2044       2045       2046       2047
LAVE PROCESSING COMPLETE
list lavec1 (1,1,1,10)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              0          1          2          3          4          5          6          7          8          9
label-list lavec1
Beginning VICAR task label
************************************************************
 
        ************  File lavec1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in FULL format from a X86-LINUX host
                1 bands
                1 lines per band
                2048 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: wlb -- Wed Mar 18 21:52:33 2015 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LAVE -- User: wlb -- Wed Mar 18 21:52:33 2015 ----
 
************************************************************
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS DATA IN AIS FORMAT


     SAMP            1                     3                     5                     7                     9
 LINE
    1                0          1          2          3          4          5          6          7          8
    2               32         33         34         35         36         37         38         39         40
    3               64         65         66         67         68         69         70         71         72
    4               96         97         98         99        100        101        102        103        104
    5              128        129        130        131        132        133        134        135        136
    6              160        161        162        163        164        165        166        167        168
    7              192        193        194        195        196        197        198        199        200
    8              224        225        226        227        228        229        230        231        232
    9              256        257        258        259        260        261        262        263        264
   10              288        289        290        291        292        293        294        295        296
   11              320        321        322        323        324        325        326        327        328
   12              352        353        354        355        356        357        358        359        360
   13              384        385        386        387        388        389        390        391        392
   14              416        417        418        419        420        421        422        423        424
   15              448        449        450        451        452        453        454        455        456
   16              480        481        482        483        484        485        486        487        488
   17              512        513        514        515        516        517        518        519        520
   18              544        545        546        547        548        549        550        551        552
   19              576        577        578        579        580        581        582        583        584
   20              608        609        610        611        612        613        614        615        616
   21              640        641        642        643        644        645        646        647        648
   22              672        673        674        675        676        677        678        679        680
   23              704        705        706        707        708        709        710        711        712
   24              736        737        738        739        740        741        742        743        744
   25              768        769        770        771        772        773        774        775        776
   26              800        801        802        803        804        805        806        807        808
   27              832        833        834        835        836        837        838        839        840
   28              864        865        866        867        868        869        870        871        872
   29              896        897        898        899        900        901        902        903        904
   30              928        929        930        931        932        933        934        935        936
   31              960        961        962        963        964        965        966        967        968
   32              992        993        994        995        996        997        998        999       1000


     SAMP           10                    12                    14                    16                    18
 LINE
    1                9         10         11         12         13         14         15         16         17
    2               41         42         43         44         45         46         47         48         49
    3               73         74         75         76         77         78         79         80         81
    4              105        106        107        108        109        110        111        112        113
    5              137        138        139        140        141        142        143        144        145
    6              169        170        171        172        173        174        175        176        177
    7              201        202        203        204        205        206        207        208        209
    8              233        234        235        236        237        238        239        240        241
    9              265        266        267        268        269        270        271        272        273
   10              297        298        299        300        301        302        303        304        305
   11              329        330        331        332        333        334        335        336        337
   12              361        362        363        364        365        366        367        368        369
   13              393        394        395        396        397        398        399        400        401
   14              425        426        427        428        429        430        431        432        433
   15              457        458        459        460        461        462        463        464        465
   16              489        490        491        492        493        494        495        496        497
   17              521        522        523        524        525        526        527        528        529
   18              553        554        555        556        557        558        559        560        561
   19              585        586        587        588        589        590        591        592        593
   20              617        618        619        620        621        622        623        624        625
   21              649        650        651        652        653        654        655        656        657
   22              681        682        683        684        685        686        687        688        689
   23              713        714        715        716        717        718        719        720        721
   24              745        746        747        748        749        750        751        752        753
   25              777        778        779        780        781        782        783        784        785
   26              809        810        811        812        813        814        815        816        817
   27              841        842        843        844        845        846        847        848        849
   28              873        874        875        876        877        878        879        880        881
   29              905        906        907        908        909        910        911        912        913
   30              937        938        939        940        941        942        943        944        945
   31              969        970        971        972        973        974        975        976        977
   32             1001       1002       1003       1004       1005       1006       1007       1008       1009


     SAMP           19                    21                    23                    25                    27
 LINE
    1               18         19         20         21         22         23         24         25         26
    2               50         51         52         53         54         55         56         57         58
    3               82         83         84         85         86         87         88         89         90
    4              114        115        116        117        118        119        120        121        122
    5              146        147        148        149        150        151        152        153        154
    6              178        179        180        181        182        183        184        185        186
    7              210        211        212        213        214        215        216        217        218
    8              242        243        244        245        246        247        248        249        250
    9              274        275        276        277        278        279        280        281        282
   10              306        307        308        309        310        311        312        313        314
   11              338        339        340        341        342        343        344        345        346
   12              370        371        372        373        374        375        376        377        378
   13              402        403        404        405        406        407        408        409        410
   14              434        435        436        437        438        439        440        441        442
   15              466        467        468        469        470        471        472        473        474
   16              498        499        500        501        502        503        504        505        506
   17              530        531        532        533        534        535        536        537        538
   18              562        563        564        565        566        567        568        569        570
   19              594        595        596        597        598        599        600        601        602
   20              626        627        628        629        630        631        632        633        634
   21              658        659        660        661        662        663        664        665        666
   22              690        691        692        693        694        695        696        697        698
   23              722        723        724        725        726        727        728        729        730
   24              754        755        756        757        758        759        760        761        762
   25              786        787        788        789        790        791        792        793        794
   26              818        819        820        821        822        823        824        825        826
   27              850        851        852        853        854        855        856        857        858
   28              882        883        884        885        886        887        888        889        890
   29              914        915        916        917        918        919        920        921        922
   30              946        947        948        949        950        951        952        953        954
   31              978        979        980        981        982        983        984        985        986
   32             1010       1011       1012       1013       1014       1015       1016       1017       1018


     SAMP           28                    30                    32
 LINE
    1               27         28         29         30         31
    2               59         60         61         62         63
    3               91         92         93         94         95
    4              123        124        125        126        127
    5              155        156        157        158        159
    6              187        188        189        190        191
    7              219        220        221        222        223
    8              251        252        253        254        255
    9              283        284        285        286        287
   10              315        316        317        318        319
   11              347        348        349        350        351
   12              379        380        381        382        383
   13              411        412        413        414        415
   14              443        444        445        446        447
   15              475        476        477        478        479
   16              507        508        509        510        511
   17              539        540        541        542        543
   18              571        572        573        574        575
   19              603        604        605        606        607
   20              635        636        637        638        639
   21              667        668        669        670        671
   22              699        700        701        702        703
   23              731        732        733        734        735
   24              763        764        765        766        767
   25              795        796        797        798        799
   26              827        828        829        830        831
   27              859        860        861        862        863
   28              891        892        893        894        895
   29              923        924        925        926        927
   30              955        956        957        958        959
   31              987        988        989        990        991
   32             1019       1020       1021       1022       1023
LAVE PROCESSING COMPLETE
gen lavea 11 10 'real
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavea lavea1
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10 0.4500E+01 0.5500E+01 0.6500E+01 0.7500E+01 0.8500E+01 0.9500E+01 0.1050E+02 0.1150E+02 0.1250E+02 0.1350E+02
   11-   11 0.1450E+02
LAVE PROCESSING COMPLETE
list lavea1
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       4.500E+00   5.500E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp            11
   Line
      1       1.450E+01
lave lavea lavea2 'vert exclude=0
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10 0.5500E+01 0.6000E+01 0.7000E+01 0.8000E+01 0.9000E+01 0.1000E+02 0.1100E+02 0.1200E+02 0.1300E+02 0.1400E+02
LAVE PROCESSING COMPLETE
list lavea2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       5.500E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
lave lavea lavea3  (6,2,5,7) 'horiz 'image
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-    5 0.9000E+01 0.1000E+02 0.1100E+02 0.1200E+02 0.1300E+02
LAVE PROCESSING COMPLETE
list lavea3
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7
   Line
      1       9.000E+00   9.000E+00   9.000E+00   9.000E+00   9.000E+00   9.000E+00   9.000E+00
      2       1.000E+01   1.000E+01   1.000E+01   1.000E+01   1.000E+01   1.000E+01   1.000E+01
      3       1.100E+01   1.100E+01   1.100E+01   1.100E+01   1.100E+01   1.100E+01   1.100E+01
      4       1.200E+01   1.200E+01   1.200E+01   1.200E+01   1.200E+01   1.200E+01   1.200E+01
      5       1.300E+01   1.300E+01   1.300E+01   1.300E+01   1.300E+01   1.300E+01   1.300E+01
lave lavea laveb1 'stdev
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  STANDARD DEVIATION VALUES
    1-   10 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01 0.2872E+01
   11-   11 0.2872E+01
LAVE PROCESSING COMPLETE
list laveb1
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.872E+00   2.872E+00   2.872E+00   2.872E+00   2.872E+00   2.872E+00   2.872E+00   2.872E+00   2.872E+00   2.872E+00

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp            11
   Line
      1       2.872E+00
lave lavea laveb2 filter=3
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10 0.4500E+01 0.5500E+01 0.6500E+01 0.7500E+01 0.8500E+01 0.9500E+01 0.1050E+02 0.1150E+02 0.1250E+02 0.1350E+02
   11-   11 0.1450E+02
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   10 0.5000E+01 0.5500E+01 0.6500E+01 0.7500E+01 0.8500E+01 0.9500E+01 0.1050E+02 0.1150E+02 0.1250E+02 0.1350E+02
   11-   11 0.1400E+02
LAVE PROCESSING COMPLETE
list laveb2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       5.000E+00   5.500E+00   6.500E+00   7.500E+00   8.500E+00   9.500E+00   1.050E+01   1.150E+01   1.250E+01   1.350E+01

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp            11
   Line
      1       1.400E+01
lave lavea laveb3 filter=3 'highpass
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES
    1-   10 0.4500E+01 0.5500E+01 0.6500E+01 0.7500E+01 0.8500E+01 0.9500E+01 0.1050E+02 0.1150E+02 0.1250E+02 0.1350E+02
   11-   11 0.1450E+02
??I - After filtering

    SAMPLE  AVERAGE VALUES
    1-   10 -.5000E+00 0.0000E+00 0.0000E+00 0.0000E+00 0.0000E+00 0.0000E+00 0.0000E+00 0.0000E+00 0.0000E+00 0.0000E+00
   11-   11 0.5000E+00
LAVE PROCESSING COMPLETE
list laveb3
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1      -5.000E-01   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00   0.000E+00

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp            11
   Line
      1       5.000E-01
gen lavec 1 2048 'real
Beginning VICAR task gen
GEN Version 6
GEN task completed
lave lavec lavec1 'ais 'vert title="HERE IS SOME DATA PRINTED IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  1


     SAMP            1                     3                     5                     7                     9
 LINE
    1       0.0000E+00 0.1000E+01 0.2000E+01 0.3000E+01 0.4000E+01 0.5000E+01 0.6000E+01 0.7000E+01 0.8000E+01
    2       0.3200E+02 0.3300E+02 0.3400E+02 0.3500E+02 0.3600E+02 0.3700E+02 0.3800E+02 0.3900E+02 0.4000E+02
    3       0.6400E+02 0.6500E+02 0.6600E+02 0.6700E+02 0.6800E+02 0.6900E+02 0.7000E+02 0.7100E+02 0.7200E+02
    4       0.9600E+02 0.9700E+02 0.9800E+02 0.9900E+02 0.1000E+03 0.1010E+03 0.1020E+03 0.1030E+03 0.1040E+03
    5       0.1280E+03 0.1290E+03 0.1300E+03 0.1310E+03 0.1320E+03 0.1330E+03 0.1340E+03 0.1350E+03 0.1360E+03
    6       0.1600E+03 0.1610E+03 0.1620E+03 0.1630E+03 0.1640E+03 0.1650E+03 0.1660E+03 0.1670E+03 0.1680E+03
    7       0.1920E+03 0.1930E+03 0.1940E+03 0.1950E+03 0.1960E+03 0.1970E+03 0.1980E+03 0.1990E+03 0.2000E+03
    8       0.2240E+03 0.2250E+03 0.2260E+03 0.2270E+03 0.2280E+03 0.2290E+03 0.2300E+03 0.2310E+03 0.2320E+03
    9       0.2560E+03 0.2570E+03 0.2580E+03 0.2590E+03 0.2600E+03 0.2610E+03 0.2620E+03 0.2630E+03 0.2640E+03
   10       0.2880E+03 0.2890E+03 0.2900E+03 0.2910E+03 0.2920E+03 0.2930E+03 0.2940E+03 0.2950E+03 0.2960E+03
   11       0.3200E+03 0.3210E+03 0.3220E+03 0.3230E+03 0.3240E+03 0.3250E+03 0.3260E+03 0.3270E+03 0.3280E+03
   12       0.3520E+03 0.3530E+03 0.3540E+03 0.3550E+03 0.3560E+03 0.3570E+03 0.3580E+03 0.3590E+03 0.3600E+03
   13       0.3840E+03 0.3850E+03 0.3860E+03 0.3870E+03 0.3880E+03 0.3890E+03 0.3900E+03 0.3910E+03 0.3920E+03
   14       0.4160E+03 0.4170E+03 0.4180E+03 0.4190E+03 0.4200E+03 0.4210E+03 0.4220E+03 0.4230E+03 0.4240E+03
   15       0.4480E+03 0.4490E+03 0.4500E+03 0.4510E+03 0.4520E+03 0.4530E+03 0.4540E+03 0.4550E+03 0.4560E+03
   16       0.4800E+03 0.4810E+03 0.4820E+03 0.4830E+03 0.4840E+03 0.4850E+03 0.4860E+03 0.4870E+03 0.4880E+03
   17       0.5120E+03 0.5130E+03 0.5140E+03 0.5150E+03 0.5160E+03 0.5170E+03 0.5180E+03 0.5190E+03 0.5200E+03
   18       0.5440E+03 0.5450E+03 0.5460E+03 0.5470E+03 0.5480E+03 0.5490E+03 0.5500E+03 0.5510E+03 0.5520E+03
   19       0.5760E+03 0.5770E+03 0.5780E+03 0.5790E+03 0.5800E+03 0.5810E+03 0.5820E+03 0.5830E+03 0.5840E+03
   20       0.6080E+03 0.6090E+03 0.6100E+03 0.6110E+03 0.6120E+03 0.6130E+03 0.6140E+03 0.6150E+03 0.6160E+03
   21       0.6400E+03 0.6410E+03 0.6420E+03 0.6430E+03 0.6440E+03 0.6450E+03 0.6460E+03 0.6470E+03 0.6480E+03
   22       0.6720E+03 0.6730E+03 0.6740E+03 0.6750E+03 0.6760E+03 0.6770E+03 0.6780E+03 0.6790E+03 0.6800E+03
   23       0.7040E+03 0.7050E+03 0.7060E+03 0.7070E+03 0.7080E+03 0.7090E+03 0.7100E+03 0.7110E+03 0.7120E+03
   24       0.7360E+03 0.7370E+03 0.7380E+03 0.7390E+03 0.7400E+03 0.7410E+03 0.7420E+03 0.7430E+03 0.7440E+03
   25       0.7680E+03 0.7690E+03 0.7700E+03 0.7710E+03 0.7720E+03 0.7730E+03 0.7740E+03 0.7750E+03 0.7760E+03
   26       0.8000E+03 0.8010E+03 0.8020E+03 0.8030E+03 0.8040E+03 0.8050E+03 0.8060E+03 0.8070E+03 0.8080E+03
   27       0.8320E+03 0.8330E+03 0.8340E+03 0.8350E+03 0.8360E+03 0.8370E+03 0.8380E+03 0.8390E+03 0.8400E+03
   28       0.8640E+03 0.8650E+03 0.8660E+03 0.8670E+03 0.8680E+03 0.8690E+03 0.8700E+03 0.8710E+03 0.8720E+03
   29       0.8960E+03 0.8970E+03 0.8980E+03 0.8990E+03 0.9000E+03 0.9010E+03 0.9020E+03 0.9030E+03 0.9040E+03
   30       0.9280E+03 0.9290E+03 0.9300E+03 0.9310E+03 0.9320E+03 0.9330E+03 0.9340E+03 0.9350E+03 0.9360E+03
   31       0.9600E+03 0.9610E+03 0.9620E+03 0.9630E+03 0.9640E+03 0.9650E+03 0.9660E+03 0.9670E+03 0.9680E+03
   32       0.9920E+03 0.9930E+03 0.9940E+03 0.9950E+03 0.9960E+03 0.9970E+03 0.9980E+03 0.9990E+03 0.1000E+04


     SAMP           10                    12                    14                    16                    18
 LINE
    1       0.9000E+01 0.1000E+02 0.1100E+02 0.1200E+02 0.1300E+02 0.1400E+02 0.1500E+02 0.1600E+02 0.1700E+02
    2       0.4100E+02 0.4200E+02 0.4300E+02 0.4400E+02 0.4500E+02 0.4600E+02 0.4700E+02 0.4800E+02 0.4900E+02
    3       0.7300E+02 0.7400E+02 0.7500E+02 0.7600E+02 0.7700E+02 0.7800E+02 0.7900E+02 0.8000E+02 0.8100E+02
    4       0.1050E+03 0.1060E+03 0.1070E+03 0.1080E+03 0.1090E+03 0.1100E+03 0.1110E+03 0.1120E+03 0.1130E+03
    5       0.1370E+03 0.1380E+03 0.1390E+03 0.1400E+03 0.1410E+03 0.1420E+03 0.1430E+03 0.1440E+03 0.1450E+03
    6       0.1690E+03 0.1700E+03 0.1710E+03 0.1720E+03 0.1730E+03 0.1740E+03 0.1750E+03 0.1760E+03 0.1770E+03
    7       0.2010E+03 0.2020E+03 0.2030E+03 0.2040E+03 0.2050E+03 0.2060E+03 0.2070E+03 0.2080E+03 0.2090E+03
    8       0.2330E+03 0.2340E+03 0.2350E+03 0.2360E+03 0.2370E+03 0.2380E+03 0.2390E+03 0.2400E+03 0.2410E+03
    9       0.2650E+03 0.2660E+03 0.2670E+03 0.2680E+03 0.2690E+03 0.2700E+03 0.2710E+03 0.2720E+03 0.2730E+03
   10       0.2970E+03 0.2980E+03 0.2990E+03 0.3000E+03 0.3010E+03 0.3020E+03 0.3030E+03 0.3040E+03 0.3050E+03
   11       0.3290E+03 0.3300E+03 0.3310E+03 0.3320E+03 0.3330E+03 0.3340E+03 0.3350E+03 0.3360E+03 0.3370E+03
   12       0.3610E+03 0.3620E+03 0.3630E+03 0.3640E+03 0.3650E+03 0.3660E+03 0.3670E+03 0.3680E+03 0.3690E+03
   13       0.3930E+03 0.3940E+03 0.3950E+03 0.3960E+03 0.3970E+03 0.3980E+03 0.3990E+03 0.4000E+03 0.4010E+03
   14       0.4250E+03 0.4260E+03 0.4270E+03 0.4280E+03 0.4290E+03 0.4300E+03 0.4310E+03 0.4320E+03 0.4330E+03
   15       0.4570E+03 0.4580E+03 0.4590E+03 0.4600E+03 0.4610E+03 0.4620E+03 0.4630E+03 0.4640E+03 0.4650E+03
   16       0.4890E+03 0.4900E+03 0.4910E+03 0.4920E+03 0.4930E+03 0.4940E+03 0.4950E+03 0.4960E+03 0.4970E+03
   17       0.5210E+03 0.5220E+03 0.5230E+03 0.5240E+03 0.5250E+03 0.5260E+03 0.5270E+03 0.5280E+03 0.5290E+03
   18       0.5530E+03 0.5540E+03 0.5550E+03 0.5560E+03 0.5570E+03 0.5580E+03 0.5590E+03 0.5600E+03 0.5610E+03
   19       0.5850E+03 0.5860E+03 0.5870E+03 0.5880E+03 0.5890E+03 0.5900E+03 0.5910E+03 0.5920E+03 0.5930E+03
   20       0.6170E+03 0.6180E+03 0.6190E+03 0.6200E+03 0.6210E+03 0.6220E+03 0.6230E+03 0.6240E+03 0.6250E+03
   21       0.6490E+03 0.6500E+03 0.6510E+03 0.6520E+03 0.6530E+03 0.6540E+03 0.6550E+03 0.6560E+03 0.6570E+03
   22       0.6810E+03 0.6820E+03 0.6830E+03 0.6840E+03 0.6850E+03 0.6860E+03 0.6870E+03 0.6880E+03 0.6890E+03
   23       0.7130E+03 0.7140E+03 0.7150E+03 0.7160E+03 0.7170E+03 0.7180E+03 0.7190E+03 0.7200E+03 0.7210E+03
   24       0.7450E+03 0.7460E+03 0.7470E+03 0.7480E+03 0.7490E+03 0.7500E+03 0.7510E+03 0.7520E+03 0.7530E+03
   25       0.7770E+03 0.7780E+03 0.7790E+03 0.7800E+03 0.7810E+03 0.7820E+03 0.7830E+03 0.7840E+03 0.7850E+03
   26       0.8090E+03 0.8100E+03 0.8110E+03 0.8120E+03 0.8130E+03 0.8140E+03 0.8150E+03 0.8160E+03 0.8170E+03
   27       0.8410E+03 0.8420E+03 0.8430E+03 0.8440E+03 0.8450E+03 0.8460E+03 0.8470E+03 0.8480E+03 0.8490E+03
   28       0.8730E+03 0.8740E+03 0.8750E+03 0.8760E+03 0.8770E+03 0.8780E+03 0.8790E+03 0.8800E+03 0.8810E+03
   29       0.9050E+03 0.9060E+03 0.9070E+03 0.9080E+03 0.9090E+03 0.9100E+03 0.9110E+03 0.9120E+03 0.9130E+03
   30       0.9370E+03 0.9380E+03 0.9390E+03 0.9400E+03 0.9410E+03 0.9420E+03 0.9430E+03 0.9440E+03 0.9450E+03
   31       0.9690E+03 0.9700E+03 0.9710E+03 0.9720E+03 0.9730E+03 0.9740E+03 0.9750E+03 0.9760E+03 0.9770E+03
   32       0.1001E+04 0.1002E+04 0.1003E+04 0.1004E+04 0.1005E+04 0.1006E+04 0.1007E+04 0.1008E+04 0.1009E+04


     SAMP           19                    21                    23                    25                    27
 LINE
    1       0.1800E+02 0.1900E+02 0.2000E+02 0.2100E+02 0.2200E+02 0.2300E+02 0.2400E+02 0.2500E+02 0.2600E+02
    2       0.5000E+02 0.5100E+02 0.5200E+02 0.5300E+02 0.5400E+02 0.5500E+02 0.5600E+02 0.5700E+02 0.5800E+02
    3       0.8200E+02 0.8300E+02 0.8400E+02 0.8500E+02 0.8600E+02 0.8700E+02 0.8800E+02 0.8900E+02 0.9000E+02
    4       0.1140E+03 0.1150E+03 0.1160E+03 0.1170E+03 0.1180E+03 0.1190E+03 0.1200E+03 0.1210E+03 0.1220E+03
    5       0.1460E+03 0.1470E+03 0.1480E+03 0.1490E+03 0.1500E+03 0.1510E+03 0.1520E+03 0.1530E+03 0.1540E+03
    6       0.1780E+03 0.1790E+03 0.1800E+03 0.1810E+03 0.1820E+03 0.1830E+03 0.1840E+03 0.1850E+03 0.1860E+03
    7       0.2100E+03 0.2110E+03 0.2120E+03 0.2130E+03 0.2140E+03 0.2150E+03 0.2160E+03 0.2170E+03 0.2180E+03
    8       0.2420E+03 0.2430E+03 0.2440E+03 0.2450E+03 0.2460E+03 0.2470E+03 0.2480E+03 0.2490E+03 0.2500E+03
    9       0.2740E+03 0.2750E+03 0.2760E+03 0.2770E+03 0.2780E+03 0.2790E+03 0.2800E+03 0.2810E+03 0.2820E+03
   10       0.3060E+03 0.3070E+03 0.3080E+03 0.3090E+03 0.3100E+03 0.3110E+03 0.3120E+03 0.3130E+03 0.3140E+03
   11       0.3380E+03 0.3390E+03 0.3400E+03 0.3410E+03 0.3420E+03 0.3430E+03 0.3440E+03 0.3450E+03 0.3460E+03
   12       0.3700E+03 0.3710E+03 0.3720E+03 0.3730E+03 0.3740E+03 0.3750E+03 0.3760E+03 0.3770E+03 0.3780E+03
   13       0.4020E+03 0.4030E+03 0.4040E+03 0.4050E+03 0.4060E+03 0.4070E+03 0.4080E+03 0.4090E+03 0.4100E+03
   14       0.4340E+03 0.4350E+03 0.4360E+03 0.4370E+03 0.4380E+03 0.4390E+03 0.4400E+03 0.4410E+03 0.4420E+03
   15       0.4660E+03 0.4670E+03 0.4680E+03 0.4690E+03 0.4700E+03 0.4710E+03 0.4720E+03 0.4730E+03 0.4740E+03
   16       0.4980E+03 0.4990E+03 0.5000E+03 0.5010E+03 0.5020E+03 0.5030E+03 0.5040E+03 0.5050E+03 0.5060E+03
   17       0.5300E+03 0.5310E+03 0.5320E+03 0.5330E+03 0.5340E+03 0.5350E+03 0.5360E+03 0.5370E+03 0.5380E+03
   18       0.5620E+03 0.5630E+03 0.5640E+03 0.5650E+03 0.5660E+03 0.5670E+03 0.5680E+03 0.5690E+03 0.5700E+03
   19       0.5940E+03 0.5950E+03 0.5960E+03 0.5970E+03 0.5980E+03 0.5990E+03 0.6000E+03 0.6010E+03 0.6020E+03
   20       0.6260E+03 0.6270E+03 0.6280E+03 0.6290E+03 0.6300E+03 0.6310E+03 0.6320E+03 0.6330E+03 0.6340E+03
   21       0.6580E+03 0.6590E+03 0.6600E+03 0.6610E+03 0.6620E+03 0.6630E+03 0.6640E+03 0.6650E+03 0.6660E+03
   22       0.6900E+03 0.6910E+03 0.6920E+03 0.6930E+03 0.6940E+03 0.6950E+03 0.6960E+03 0.6970E+03 0.6980E+03
   23       0.7220E+03 0.7230E+03 0.7240E+03 0.7250E+03 0.7260E+03 0.7270E+03 0.7280E+03 0.7290E+03 0.7300E+03
   24       0.7540E+03 0.7550E+03 0.7560E+03 0.7570E+03 0.7580E+03 0.7590E+03 0.7600E+03 0.7610E+03 0.7620E+03
   25       0.7860E+03 0.7870E+03 0.7880E+03 0.7890E+03 0.7900E+03 0.7910E+03 0.7920E+03 0.7930E+03 0.7940E+03
   26       0.8180E+03 0.8190E+03 0.8200E+03 0.8210E+03 0.8220E+03 0.8230E+03 0.8240E+03 0.8250E+03 0.8260E+03
   27       0.8500E+03 0.8510E+03 0.8520E+03 0.8530E+03 0.8540E+03 0.8550E+03 0.8560E+03 0.8570E+03 0.8580E+03
   28       0.8820E+03 0.8830E+03 0.8840E+03 0.8850E+03 0.8860E+03 0.8870E+03 0.8880E+03 0.8890E+03 0.8900E+03
   29       0.9140E+03 0.9150E+03 0.9160E+03 0.9170E+03 0.9180E+03 0.9190E+03 0.9200E+03 0.9210E+03 0.9220E+03
   30       0.9460E+03 0.9470E+03 0.9480E+03 0.9490E+03 0.9500E+03 0.9510E+03 0.9520E+03 0.9530E+03 0.9540E+03
   31       0.9780E+03 0.9790E+03 0.9800E+03 0.9810E+03 0.9820E+03 0.9830E+03 0.9840E+03 0.9850E+03 0.9860E+03
   32       0.1010E+04 0.1011E+04 0.1012E+04 0.1013E+04 0.1014E+04 0.1015E+04 0.1016E+04 0.1017E+04 0.1018E+04


     SAMP           28                    30                    32
 LINE
    1       0.2700E+02 0.2800E+02 0.2900E+02 0.3000E+02 0.3100E+02
    2       0.5900E+02 0.6000E+02 0.6100E+02 0.6200E+02 0.6300E+02
    3       0.9100E+02 0.9200E+02 0.9300E+02 0.9400E+02 0.9500E+02
    4       0.1230E+03 0.1240E+03 0.1250E+03 0.1260E+03 0.1270E+03
    5       0.1550E+03 0.1560E+03 0.1570E+03 0.1580E+03 0.1590E+03
    6       0.1870E+03 0.1880E+03 0.1890E+03 0.1900E+03 0.1910E+03
    7       0.2190E+03 0.2200E+03 0.2210E+03 0.2220E+03 0.2230E+03
    8       0.2510E+03 0.2520E+03 0.2530E+03 0.2540E+03 0.2550E+03
    9       0.2830E+03 0.2840E+03 0.2850E+03 0.2860E+03 0.2870E+03
   10       0.3150E+03 0.3160E+03 0.3170E+03 0.3180E+03 0.3190E+03
   11       0.3470E+03 0.3480E+03 0.3490E+03 0.3500E+03 0.3510E+03
   12       0.3790E+03 0.3800E+03 0.3810E+03 0.3820E+03 0.3830E+03
   13       0.4110E+03 0.4120E+03 0.4130E+03 0.4140E+03 0.4150E+03
   14       0.4430E+03 0.4440E+03 0.4450E+03 0.4460E+03 0.4470E+03
   15       0.4750E+03 0.4760E+03 0.4770E+03 0.4780E+03 0.4790E+03
   16       0.5070E+03 0.5080E+03 0.5090E+03 0.5100E+03 0.5110E+03
   17       0.5390E+03 0.5400E+03 0.5410E+03 0.5420E+03 0.5430E+03
   18       0.5710E+03 0.5720E+03 0.5730E+03 0.5740E+03 0.5750E+03
   19       0.6030E+03 0.6040E+03 0.6050E+03 0.6060E+03 0.6070E+03
   20       0.6350E+03 0.6360E+03 0.6370E+03 0.6380E+03 0.6390E+03
   21       0.6670E+03 0.6680E+03 0.6690E+03 0.6700E+03 0.6710E+03
   22       0.6990E+03 0.7000E+03 0.7010E+03 0.7020E+03 0.7030E+03
   23       0.7310E+03 0.7320E+03 0.7330E+03 0.7340E+03 0.7350E+03
   24       0.7630E+03 0.7640E+03 0.7650E+03 0.7660E+03 0.7670E+03
   25       0.7950E+03 0.7960E+03 0.7970E+03 0.7980E+03 0.7990E+03
   26       0.8270E+03 0.8280E+03 0.8290E+03 0.8300E+03 0.8310E+03
   27       0.8590E+03 0.8600E+03 0.8610E+03 0.8620E+03 0.8630E+03
   28       0.8910E+03 0.8920E+03 0.8930E+03 0.8940E+03 0.8950E+03
   29       0.9230E+03 0.9240E+03 0.9250E+03 0.9260E+03 0.9270E+03
   30       0.9550E+03 0.9560E+03 0.9570E+03 0.9580E+03 0.9590E+03
   31       0.9870E+03 0.9880E+03 0.9890E+03 0.9900E+03 0.9910E+03
   32       0.1019E+04 0.1020E+04 0.1021E+04 0.1022E+04 0.1023E+04





HERE IS SOME DATA PRINTED IN AIS FORMAT

SECTION  2


     SAMP            1                     3                     5                     7                     9
 LINE
    1       0.1024E+04 0.1025E+04 0.1026E+04 0.1027E+04 0.1028E+04 0.1029E+04 0.1030E+04 0.1031E+04 0.1032E+04
    2       0.1056E+04 0.1057E+04 0.1058E+04 0.1059E+04 0.1060E+04 0.1061E+04 0.1062E+04 0.1063E+04 0.1064E+04
    3       0.1088E+04 0.1089E+04 0.1090E+04 0.1091E+04 0.1092E+04 0.1093E+04 0.1094E+04 0.1095E+04 0.1096E+04
    4       0.1120E+04 0.1121E+04 0.1122E+04 0.1123E+04 0.1124E+04 0.1125E+04 0.1126E+04 0.1127E+04 0.1128E+04
    5       0.1152E+04 0.1153E+04 0.1154E+04 0.1155E+04 0.1156E+04 0.1157E+04 0.1158E+04 0.1159E+04 0.1160E+04
    6       0.1184E+04 0.1185E+04 0.1186E+04 0.1187E+04 0.1188E+04 0.1189E+04 0.1190E+04 0.1191E+04 0.1192E+04
    7       0.1216E+04 0.1217E+04 0.1218E+04 0.1219E+04 0.1220E+04 0.1221E+04 0.1222E+04 0.1223E+04 0.1224E+04
    8       0.1248E+04 0.1249E+04 0.1250E+04 0.1251E+04 0.1252E+04 0.1253E+04 0.1254E+04 0.1255E+04 0.1256E+04
    9       0.1280E+04 0.1281E+04 0.1282E+04 0.1283E+04 0.1284E+04 0.1285E+04 0.1286E+04 0.1287E+04 0.1288E+04
   10       0.1312E+04 0.1313E+04 0.1314E+04 0.1315E+04 0.1316E+04 0.1317E+04 0.1318E+04 0.1319E+04 0.1320E+04
   11       0.1344E+04 0.1345E+04 0.1346E+04 0.1347E+04 0.1348E+04 0.1349E+04 0.1350E+04 0.1351E+04 0.1352E+04
   12       0.1376E+04 0.1377E+04 0.1378E+04 0.1379E+04 0.1380E+04 0.1381E+04 0.1382E+04 0.1383E+04 0.1384E+04
   13       0.1408E+04 0.1409E+04 0.1410E+04 0.1411E+04 0.1412E+04 0.1413E+04 0.1414E+04 0.1415E+04 0.1416E+04
   14       0.1440E+04 0.1441E+04 0.1442E+04 0.1443E+04 0.1444E+04 0.1445E+04 0.1446E+04 0.1447E+04 0.1448E+04
   15       0.1472E+04 0.1473E+04 0.1474E+04 0.1475E+04 0.1476E+04 0.1477E+04 0.1478E+04 0.1479E+04 0.1480E+04
   16       0.1504E+04 0.1505E+04 0.1506E+04 0.1507E+04 0.1508E+04 0.1509E+04 0.1510E+04 0.1511E+04 0.1512E+04
   17       0.1536E+04 0.1537E+04 0.1538E+04 0.1539E+04 0.1540E+04 0.1541E+04 0.1542E+04 0.1543E+04 0.1544E+04
   18       0.1568E+04 0.1569E+04 0.1570E+04 0.1571E+04 0.1572E+04 0.1573E+04 0.1574E+04 0.1575E+04 0.1576E+04
   19       0.1600E+04 0.1601E+04 0.1602E+04 0.1603E+04 0.1604E+04 0.1605E+04 0.1606E+04 0.1607E+04 0.1608E+04
   20       0.1632E+04 0.1633E+04 0.1634E+04 0.1635E+04 0.1636E+04 0.1637E+04 0.1638E+04 0.1639E+04 0.1640E+04
   21       0.1664E+04 0.1665E+04 0.1666E+04 0.1667E+04 0.1668E+04 0.1669E+04 0.1670E+04 0.1671E+04 0.1672E+04
   22       0.1696E+04 0.1697E+04 0.1698E+04 0.1699E+04 0.1700E+04 0.1701E+04 0.1702E+04 0.1703E+04 0.1704E+04
   23       0.1728E+04 0.1729E+04 0.1730E+04 0.1731E+04 0.1732E+04 0.1733E+04 0.1734E+04 0.1735E+04 0.1736E+04
   24       0.1760E+04 0.1761E+04 0.1762E+04 0.1763E+04 0.1764E+04 0.1765E+04 0.1766E+04 0.1767E+04 0.1768E+04
   25       0.1792E+04 0.1793E+04 0.1794E+04 0.1795E+04 0.1796E+04 0.1797E+04 0.1798E+04 0.1799E+04 0.1800E+04
   26       0.1824E+04 0.1825E+04 0.1826E+04 0.1827E+04 0.1828E+04 0.1829E+04 0.1830E+04 0.1831E+04 0.1832E+04
   27       0.1856E+04 0.1857E+04 0.1858E+04 0.1859E+04 0.1860E+04 0.1861E+04 0.1862E+04 0.1863E+04 0.1864E+04
   28       0.1888E+04 0.1889E+04 0.1890E+04 0.1891E+04 0.1892E+04 0.1893E+04 0.1894E+04 0.1895E+04 0.1896E+04
   29       0.1920E+04 0.1921E+04 0.1922E+04 0.1923E+04 0.1924E+04 0.1925E+04 0.1926E+04 0.1927E+04 0.1928E+04
   30       0.1952E+04 0.1953E+04 0.1954E+04 0.1955E+04 0.1956E+04 0.1957E+04 0.1958E+04 0.1959E+04 0.1960E+04
   31       0.1984E+04 0.1985E+04 0.1986E+04 0.1987E+04 0.1988E+04 0.1989E+04 0.1990E+04 0.1991E+04 0.1992E+04
   32       0.2016E+04 0.2017E+04 0.2018E+04 0.2019E+04 0.2020E+04 0.2021E+04 0.2022E+04 0.2023E+04 0.2024E+04


     SAMP           10                    12                    14                    16                    18
 LINE
    1       0.1033E+04 0.1034E+04 0.1035E+04 0.1036E+04 0.1037E+04 0.1038E+04 0.1039E+04 0.1040E+04 0.1041E+04
    2       0.1065E+04 0.1066E+04 0.1067E+04 0.1068E+04 0.1069E+04 0.1070E+04 0.1071E+04 0.1072E+04 0.1073E+04
    3       0.1097E+04 0.1098E+04 0.1099E+04 0.1100E+04 0.1101E+04 0.1102E+04 0.1103E+04 0.1104E+04 0.1105E+04
    4       0.1129E+04 0.1130E+04 0.1131E+04 0.1132E+04 0.1133E+04 0.1134E+04 0.1135E+04 0.1136E+04 0.1137E+04
    5       0.1161E+04 0.1162E+04 0.1163E+04 0.1164E+04 0.1165E+04 0.1166E+04 0.1167E+04 0.1168E+04 0.1169E+04
    6       0.1193E+04 0.1194E+04 0.1195E+04 0.1196E+04 0.1197E+04 0.1198E+04 0.1199E+04 0.1200E+04 0.1201E+04
    7       0.1225E+04 0.1226E+04 0.1227E+04 0.1228E+04 0.1229E+04 0.1230E+04 0.1231E+04 0.1232E+04 0.1233E+04
    8       0.1257E+04 0.1258E+04 0.1259E+04 0.1260E+04 0.1261E+04 0.1262E+04 0.1263E+04 0.1264E+04 0.1265E+04
    9       0.1289E+04 0.1290E+04 0.1291E+04 0.1292E+04 0.1293E+04 0.1294E+04 0.1295E+04 0.1296E+04 0.1297E+04
   10       0.1321E+04 0.1322E+04 0.1323E+04 0.1324E+04 0.1325E+04 0.1326E+04 0.1327E+04 0.1328E+04 0.1329E+04
   11       0.1353E+04 0.1354E+04 0.1355E+04 0.1356E+04 0.1357E+04 0.1358E+04 0.1359E+04 0.1360E+04 0.1361E+04
   12       0.1385E+04 0.1386E+04 0.1387E+04 0.1388E+04 0.1389E+04 0.1390E+04 0.1391E+04 0.1392E+04 0.1393E+04
   13       0.1417E+04 0.1418E+04 0.1419E+04 0.1420E+04 0.1421E+04 0.1422E+04 0.1423E+04 0.1424E+04 0.1425E+04
   14       0.1449E+04 0.1450E+04 0.1451E+04 0.1452E+04 0.1453E+04 0.1454E+04 0.1455E+04 0.1456E+04 0.1457E+04
   15       0.1481E+04 0.1482E+04 0.1483E+04 0.1484E+04 0.1485E+04 0.1486E+04 0.1487E+04 0.1488E+04 0.1489E+04
   16       0.1513E+04 0.1514E+04 0.1515E+04 0.1516E+04 0.1517E+04 0.1518E+04 0.1519E+04 0.1520E+04 0.1521E+04
   17       0.1545E+04 0.1546E+04 0.1547E+04 0.1548E+04 0.1549E+04 0.1550E+04 0.1551E+04 0.1552E+04 0.1553E+04
   18       0.1577E+04 0.1578E+04 0.1579E+04 0.1580E+04 0.1581E+04 0.1582E+04 0.1583E+04 0.1584E+04 0.1585E+04
   19       0.1609E+04 0.1610E+04 0.1611E+04 0.1612E+04 0.1613E+04 0.1614E+04 0.1615E+04 0.1616E+04 0.1617E+04
   20       0.1641E+04 0.1642E+04 0.1643E+04 0.1644E+04 0.1645E+04 0.1646E+04 0.1647E+04 0.1648E+04 0.1649E+04
   21       0.1673E+04 0.1674E+04 0.1675E+04 0.1676E+04 0.1677E+04 0.1678E+04 0.1679E+04 0.1680E+04 0.1681E+04
   22       0.1705E+04 0.1706E+04 0.1707E+04 0.1708E+04 0.1709E+04 0.1710E+04 0.1711E+04 0.1712E+04 0.1713E+04
   23       0.1737E+04 0.1738E+04 0.1739E+04 0.1740E+04 0.1741E+04 0.1742E+04 0.1743E+04 0.1744E+04 0.1745E+04
   24       0.1769E+04 0.1770E+04 0.1771E+04 0.1772E+04 0.1773E+04 0.1774E+04 0.1775E+04 0.1776E+04 0.1777E+04
   25       0.1801E+04 0.1802E+04 0.1803E+04 0.1804E+04 0.1805E+04 0.1806E+04 0.1807E+04 0.1808E+04 0.1809E+04
   26       0.1833E+04 0.1834E+04 0.1835E+04 0.1836E+04 0.1837E+04 0.1838E+04 0.1839E+04 0.1840E+04 0.1841E+04
   27       0.1865E+04 0.1866E+04 0.1867E+04 0.1868E+04 0.1869E+04 0.1870E+04 0.1871E+04 0.1872E+04 0.1873E+04
   28       0.1897E+04 0.1898E+04 0.1899E+04 0.1900E+04 0.1901E+04 0.1902E+04 0.1903E+04 0.1904E+04 0.1905E+04
   29       0.1929E+04 0.1930E+04 0.1931E+04 0.1932E+04 0.1933E+04 0.1934E+04 0.1935E+04 0.1936E+04 0.1937E+04
   30       0.1961E+04 0.1962E+04 0.1963E+04 0.1964E+04 0.1965E+04 0.1966E+04 0.1967E+04 0.1968E+04 0.1969E+04
   31       0.1993E+04 0.1994E+04 0.1995E+04 0.1996E+04 0.1997E+04 0.1998E+04 0.1999E+04 0.2000E+04 0.2001E+04
   32       0.2025E+04 0.2026E+04 0.2027E+04 0.2028E+04 0.2029E+04 0.2030E+04 0.2031E+04 0.2032E+04 0.2033E+04


     SAMP           19                    21                    23                    25                    27
 LINE
    1       0.1042E+04 0.1043E+04 0.1044E+04 0.1045E+04 0.1046E+04 0.1047E+04 0.1048E+04 0.1049E+04 0.1050E+04
    2       0.1074E+04 0.1075E+04 0.1076E+04 0.1077E+04 0.1078E+04 0.1079E+04 0.1080E+04 0.1081E+04 0.1082E+04
    3       0.1106E+04 0.1107E+04 0.1108E+04 0.1109E+04 0.1110E+04 0.1111E+04 0.1112E+04 0.1113E+04 0.1114E+04
    4       0.1138E+04 0.1139E+04 0.1140E+04 0.1141E+04 0.1142E+04 0.1143E+04 0.1144E+04 0.1145E+04 0.1146E+04
    5       0.1170E+04 0.1171E+04 0.1172E+04 0.1173E+04 0.1174E+04 0.1175E+04 0.1176E+04 0.1177E+04 0.1178E+04
    6       0.1202E+04 0.1203E+04 0.1204E+04 0.1205E+04 0.1206E+04 0.1207E+04 0.1208E+04 0.1209E+04 0.1210E+04
    7       0.1234E+04 0.1235E+04 0.1236E+04 0.1237E+04 0.1238E+04 0.1239E+04 0.1240E+04 0.1241E+04 0.1242E+04
    8       0.1266E+04 0.1267E+04 0.1268E+04 0.1269E+04 0.1270E+04 0.1271E+04 0.1272E+04 0.1273E+04 0.1274E+04
    9       0.1298E+04 0.1299E+04 0.1300E+04 0.1301E+04 0.1302E+04 0.1303E+04 0.1304E+04 0.1305E+04 0.1306E+04
   10       0.1330E+04 0.1331E+04 0.1332E+04 0.1333E+04 0.1334E+04 0.1335E+04 0.1336E+04 0.1337E+04 0.1338E+04
   11       0.1362E+04 0.1363E+04 0.1364E+04 0.1365E+04 0.1366E+04 0.1367E+04 0.1368E+04 0.1369E+04 0.1370E+04
   12       0.1394E+04 0.1395E+04 0.1396E+04 0.1397E+04 0.1398E+04 0.1399E+04 0.1400E+04 0.1401E+04 0.1402E+04
   13       0.1426E+04 0.1427E+04 0.1428E+04 0.1429E+04 0.1430E+04 0.1431E+04 0.1432E+04 0.1433E+04 0.1434E+04
   14       0.1458E+04 0.1459E+04 0.1460E+04 0.1461E+04 0.1462E+04 0.1463E+04 0.1464E+04 0.1465E+04 0.1466E+04
   15       0.1490E+04 0.1491E+04 0.1492E+04 0.1493E+04 0.1494E+04 0.1495E+04 0.1496E+04 0.1497E+04 0.1498E+04
   16       0.1522E+04 0.1523E+04 0.1524E+04 0.1525E+04 0.1526E+04 0.1527E+04 0.1528E+04 0.1529E+04 0.1530E+04
   17       0.1554E+04 0.1555E+04 0.1556E+04 0.1557E+04 0.1558E+04 0.1559E+04 0.1560E+04 0.1561E+04 0.1562E+04
   18       0.1586E+04 0.1587E+04 0.1588E+04 0.1589E+04 0.1590E+04 0.1591E+04 0.1592E+04 0.1593E+04 0.1594E+04
   19       0.1618E+04 0.1619E+04 0.1620E+04 0.1621E+04 0.1622E+04 0.1623E+04 0.1624E+04 0.1625E+04 0.1626E+04
   20       0.1650E+04 0.1651E+04 0.1652E+04 0.1653E+04 0.1654E+04 0.1655E+04 0.1656E+04 0.1657E+04 0.1658E+04
   21       0.1682E+04 0.1683E+04 0.1684E+04 0.1685E+04 0.1686E+04 0.1687E+04 0.1688E+04 0.1689E+04 0.1690E+04
   22       0.1714E+04 0.1715E+04 0.1716E+04 0.1717E+04 0.1718E+04 0.1719E+04 0.1720E+04 0.1721E+04 0.1722E+04
   23       0.1746E+04 0.1747E+04 0.1748E+04 0.1749E+04 0.1750E+04 0.1751E+04 0.1752E+04 0.1753E+04 0.1754E+04
   24       0.1778E+04 0.1779E+04 0.1780E+04 0.1781E+04 0.1782E+04 0.1783E+04 0.1784E+04 0.1785E+04 0.1786E+04
   25       0.1810E+04 0.1811E+04 0.1812E+04 0.1813E+04 0.1814E+04 0.1815E+04 0.1816E+04 0.1817E+04 0.1818E+04
   26       0.1842E+04 0.1843E+04 0.1844E+04 0.1845E+04 0.1846E+04 0.1847E+04 0.1848E+04 0.1849E+04 0.1850E+04
   27       0.1874E+04 0.1875E+04 0.1876E+04 0.1877E+04 0.1878E+04 0.1879E+04 0.1880E+04 0.1881E+04 0.1882E+04
   28       0.1906E+04 0.1907E+04 0.1908E+04 0.1909E+04 0.1910E+04 0.1911E+04 0.1912E+04 0.1913E+04 0.1914E+04
   29       0.1938E+04 0.1939E+04 0.1940E+04 0.1941E+04 0.1942E+04 0.1943E+04 0.1944E+04 0.1945E+04 0.1946E+04
   30       0.1970E+04 0.1971E+04 0.1972E+04 0.1973E+04 0.1974E+04 0.1975E+04 0.1976E+04 0.1977E+04 0.1978E+04
   31       0.2002E+04 0.2003E+04 0.2004E+04 0.2005E+04 0.2006E+04 0.2007E+04 0.2008E+04 0.2009E+04 0.2010E+04
   32       0.2034E+04 0.2035E+04 0.2036E+04 0.2037E+04 0.2038E+04 0.2039E+04 0.2040E+04 0.2041E+04 0.2042E+04


     SAMP           28                    30                    32
 LINE
    1       0.1051E+04 0.1052E+04 0.1053E+04 0.1054E+04 0.1055E+04
    2       0.1083E+04 0.1084E+04 0.1085E+04 0.1086E+04 0.1087E+04
    3       0.1115E+04 0.1116E+04 0.1117E+04 0.1118E+04 0.1119E+04
    4       0.1147E+04 0.1148E+04 0.1149E+04 0.1150E+04 0.1151E+04
    5       0.1179E+04 0.1180E+04 0.1181E+04 0.1182E+04 0.1183E+04
    6       0.1211E+04 0.1212E+04 0.1213E+04 0.1214E+04 0.1215E+04
    7       0.1243E+04 0.1244E+04 0.1245E+04 0.1246E+04 0.1247E+04
    8       0.1275E+04 0.1276E+04 0.1277E+04 0.1278E+04 0.1279E+04
    9       0.1307E+04 0.1308E+04 0.1309E+04 0.1310E+04 0.1311E+04
   10       0.1339E+04 0.1340E+04 0.1341E+04 0.1342E+04 0.1343E+04
   11       0.1371E+04 0.1372E+04 0.1373E+04 0.1374E+04 0.1375E+04
   12       0.1403E+04 0.1404E+04 0.1405E+04 0.1406E+04 0.1407E+04
   13       0.1435E+04 0.1436E+04 0.1437E+04 0.1438E+04 0.1439E+04
   14       0.1467E+04 0.1468E+04 0.1469E+04 0.1470E+04 0.1471E+04
   15       0.1499E+04 0.1500E+04 0.1501E+04 0.1502E+04 0.1503E+04
   16       0.1531E+04 0.1532E+04 0.1533E+04 0.1534E+04 0.1535E+04
   17       0.1563E+04 0.1564E+04 0.1565E+04 0.1566E+04 0.1567E+04
   18       0.1595E+04 0.1596E+04 0.1597E+04 0.1598E+04 0.1599E+04
   19       0.1627E+04 0.1628E+04 0.1629E+04 0.1630E+04 0.1631E+04
   20       0.1659E+04 0.1660E+04 0.1661E+04 0.1662E+04 0.1663E+04
   21       0.1691E+04 0.1692E+04 0.1693E+04 0.1694E+04 0.1695E+04
   22       0.1723E+04 0.1724E+04 0.1725E+04 0.1726E+04 0.1727E+04
   23       0.1755E+04 0.1756E+04 0.1757E+04 0.1758E+04 0.1759E+04
   24       0.1787E+04 0.1788E+04 0.1789E+04 0.1790E+04 0.1791E+04
   25       0.1819E+04 0.1820E+04 0.1821E+04 0.1822E+04 0.1823E+04
   26       0.1851E+04 0.1852E+04 0.1853E+04 0.1854E+04 0.1855E+04
   27       0.1883E+04 0.1884E+04 0.1885E+04 0.1886E+04 0.1887E+04
   28       0.1915E+04 0.1916E+04 0.1917E+04 0.1918E+04 0.1919E+04
   29       0.1947E+04 0.1948E+04 0.1949E+04 0.1950E+04 0.1951E+04
   30       0.1979E+04 0.1980E+04 0.1981E+04 0.1982E+04 0.1983E+04
   31       0.2011E+04 0.2012E+04 0.2013E+04 0.2014E+04 0.2015E+04
   32       0.2043E+04 0.2044E+04 0.2045E+04 0.2046E+04 0.2047E+04
LAVE PROCESSING COMPLETE
list lavec1 (1,1,1,10)
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
 Task:LAVE      User:wlb       Date_Time:Wed Mar 18 21:52:33 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
label-list lavec1
Beginning VICAR task label
************************************************************
 
        ************  File lavec1 ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a X86-LINUX host
                1 bands
                1 lines per band
                2048 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: wlb -- Wed Mar 18 21:52:33 2015 ----
IVAL=0.0
SINC=1.0
LINC=1.0
BINC=1.0
MODULO=0.0
---- Task: LAVE -- User: wlb -- Wed Mar 18 21:52:33 2015 ----
 
************************************************************
lave lavec size=(1,1,1,1024) 'ais 'vert title="HERE IS DATA IN AIS FORMAT"
Beginning VICAR task lave
** LAVE - Sep 03, 2013 - rjb (64-bit)

    SAMPLE  AVERAGE VALUES





HERE IS DATA IN AIS FORMAT


     SAMP            1                     3                     5                     7                     9
 LINE
    1       0.0000E+00 0.1000E+01 0.2000E+01 0.3000E+01 0.4000E+01 0.5000E+01 0.6000E+01 0.7000E+01 0.8000E+01
    2       0.3200E+02 0.3300E+02 0.3400E+02 0.3500E+02 0.3600E+02 0.3700E+02 0.3800E+02 0.3900E+02 0.4000E+02
    3       0.6400E+02 0.6500E+02 0.6600E+02 0.6700E+02 0.6800E+02 0.6900E+02 0.7000E+02 0.7100E+02 0.7200E+02
    4       0.9600E+02 0.9700E+02 0.9800E+02 0.9900E+02 0.1000E+03 0.1010E+03 0.1020E+03 0.1030E+03 0.1040E+03
    5       0.1280E+03 0.1290E+03 0.1300E+03 0.1310E+03 0.1320E+03 0.1330E+03 0.1340E+03 0.1350E+03 0.1360E+03
    6       0.1600E+03 0.1610E+03 0.1620E+03 0.1630E+03 0.1640E+03 0.1650E+03 0.1660E+03 0.1670E+03 0.1680E+03
    7       0.1920E+03 0.1930E+03 0.1940E+03 0.1950E+03 0.1960E+03 0.1970E+03 0.1980E+03 0.1990E+03 0.2000E+03
    8       0.2240E+03 0.2250E+03 0.2260E+03 0.2270E+03 0.2280E+03 0.2290E+03 0.2300E+03 0.2310E+03 0.2320E+03
    9       0.2560E+03 0.2570E+03 0.2580E+03 0.2590E+03 0.2600E+03 0.2610E+03 0.2620E+03 0.2630E+03 0.2640E+03
   10       0.2880E+03 0.2890E+03 0.2900E+03 0.2910E+03 0.2920E+03 0.2930E+03 0.2940E+03 0.2950E+03 0.2960E+03
   11       0.3200E+03 0.3210E+03 0.3220E+03 0.3230E+03 0.3240E+03 0.3250E+03 0.3260E+03 0.3270E+03 0.3280E+03
   12       0.3520E+03 0.3530E+03 0.3540E+03 0.3550E+03 0.3560E+03 0.3570E+03 0.3580E+03 0.3590E+03 0.3600E+03
   13       0.3840E+03 0.3850E+03 0.3860E+03 0.3870E+03 0.3880E+03 0.3890E+03 0.3900E+03 0.3910E+03 0.3920E+03
   14       0.4160E+03 0.4170E+03 0.4180E+03 0.4190E+03 0.4200E+03 0.4210E+03 0.4220E+03 0.4230E+03 0.4240E+03
   15       0.4480E+03 0.4490E+03 0.4500E+03 0.4510E+03 0.4520E+03 0.4530E+03 0.4540E+03 0.4550E+03 0.4560E+03
   16       0.4800E+03 0.4810E+03 0.4820E+03 0.4830E+03 0.4840E+03 0.4850E+03 0.4860E+03 0.4870E+03 0.4880E+03
   17       0.5120E+03 0.5130E+03 0.5140E+03 0.5150E+03 0.5160E+03 0.5170E+03 0.5180E+03 0.5190E+03 0.5200E+03
   18       0.5440E+03 0.5450E+03 0.5460E+03 0.5470E+03 0.5480E+03 0.5490E+03 0.5500E+03 0.5510E+03 0.5520E+03
   19       0.5760E+03 0.5770E+03 0.5780E+03 0.5790E+03 0.5800E+03 0.5810E+03 0.5820E+03 0.5830E+03 0.5840E+03
   20       0.6080E+03 0.6090E+03 0.6100E+03 0.6110E+03 0.6120E+03 0.6130E+03 0.6140E+03 0.6150E+03 0.6160E+03
   21       0.6400E+03 0.6410E+03 0.6420E+03 0.6430E+03 0.6440E+03 0.6450E+03 0.6460E+03 0.6470E+03 0.6480E+03
   22       0.6720E+03 0.6730E+03 0.6740E+03 0.6750E+03 0.6760E+03 0.6770E+03 0.6780E+03 0.6790E+03 0.6800E+03
   23       0.7040E+03 0.7050E+03 0.7060E+03 0.7070E+03 0.7080E+03 0.7090E+03 0.7100E+03 0.7110E+03 0.7120E+03
   24       0.7360E+03 0.7370E+03 0.7380E+03 0.7390E+03 0.7400E+03 0.7410E+03 0.7420E+03 0.7430E+03 0.7440E+03
   25       0.7680E+03 0.7690E+03 0.7700E+03 0.7710E+03 0.7720E+03 0.7730E+03 0.7740E+03 0.7750E+03 0.7760E+03
   26       0.8000E+03 0.8010E+03 0.8020E+03 0.8030E+03 0.8040E+03 0.8050E+03 0.8060E+03 0.8070E+03 0.8080E+03
   27       0.8320E+03 0.8330E+03 0.8340E+03 0.8350E+03 0.8360E+03 0.8370E+03 0.8380E+03 0.8390E+03 0.8400E+03
   28       0.8640E+03 0.8650E+03 0.8660E+03 0.8670E+03 0.8680E+03 0.8690E+03 0.8700E+03 0.8710E+03 0.8720E+03
   29       0.8960E+03 0.8970E+03 0.8980E+03 0.8990E+03 0.9000E+03 0.9010E+03 0.9020E+03 0.9030E+03 0.9040E+03
   30       0.9280E+03 0.9290E+03 0.9300E+03 0.9310E+03 0.9320E+03 0.9330E+03 0.9340E+03 0.9350E+03 0.9360E+03
   31       0.9600E+03 0.9610E+03 0.9620E+03 0.9630E+03 0.9640E+03 0.9650E+03 0.9660E+03 0.9670E+03 0.9680E+03
   32       0.9920E+03 0.9930E+03 0.9940E+03 0.9950E+03 0.9960E+03 0.9970E+03 0.9980E+03 0.9990E+03 0.1000E+04


     SAMP           10                    12                    14                    16                    18
 LINE
    1       0.9000E+01 0.1000E+02 0.1100E+02 0.1200E+02 0.1300E+02 0.1400E+02 0.1500E+02 0.1600E+02 0.1700E+02
    2       0.4100E+02 0.4200E+02 0.4300E+02 0.4400E+02 0.4500E+02 0.4600E+02 0.4700E+02 0.4800E+02 0.4900E+02
    3       0.7300E+02 0.7400E+02 0.7500E+02 0.7600E+02 0.7700E+02 0.7800E+02 0.7900E+02 0.8000E+02 0.8100E+02
    4       0.1050E+03 0.1060E+03 0.1070E+03 0.1080E+03 0.1090E+03 0.1100E+03 0.1110E+03 0.1120E+03 0.1130E+03
    5       0.1370E+03 0.1380E+03 0.1390E+03 0.1400E+03 0.1410E+03 0.1420E+03 0.1430E+03 0.1440E+03 0.1450E+03
    6       0.1690E+03 0.1700E+03 0.1710E+03 0.1720E+03 0.1730E+03 0.1740E+03 0.1750E+03 0.1760E+03 0.1770E+03
    7       0.2010E+03 0.2020E+03 0.2030E+03 0.2040E+03 0.2050E+03 0.2060E+03 0.2070E+03 0.2080E+03 0.2090E+03
    8       0.2330E+03 0.2340E+03 0.2350E+03 0.2360E+03 0.2370E+03 0.2380E+03 0.2390E+03 0.2400E+03 0.2410E+03
    9       0.2650E+03 0.2660E+03 0.2670E+03 0.2680E+03 0.2690E+03 0.2700E+03 0.2710E+03 0.2720E+03 0.2730E+03
   10       0.2970E+03 0.2980E+03 0.2990E+03 0.3000E+03 0.3010E+03 0.3020E+03 0.3030E+03 0.3040E+03 0.3050E+03
   11       0.3290E+03 0.3300E+03 0.3310E+03 0.3320E+03 0.3330E+03 0.3340E+03 0.3350E+03 0.3360E+03 0.3370E+03
   12       0.3610E+03 0.3620E+03 0.3630E+03 0.3640E+03 0.3650E+03 0.3660E+03 0.3670E+03 0.3680E+03 0.3690E+03
   13       0.3930E+03 0.3940E+03 0.3950E+03 0.3960E+03 0.3970E+03 0.3980E+03 0.3990E+03 0.4000E+03 0.4010E+03
   14       0.4250E+03 0.4260E+03 0.4270E+03 0.4280E+03 0.4290E+03 0.4300E+03 0.4310E+03 0.4320E+03 0.4330E+03
   15       0.4570E+03 0.4580E+03 0.4590E+03 0.4600E+03 0.4610E+03 0.4620E+03 0.4630E+03 0.4640E+03 0.4650E+03
   16       0.4890E+03 0.4900E+03 0.4910E+03 0.4920E+03 0.4930E+03 0.4940E+03 0.4950E+03 0.4960E+03 0.4970E+03
   17       0.5210E+03 0.5220E+03 0.5230E+03 0.5240E+03 0.5250E+03 0.5260E+03 0.5270E+03 0.5280E+03 0.5290E+03
   18       0.5530E+03 0.5540E+03 0.5550E+03 0.5560E+03 0.5570E+03 0.5580E+03 0.5590E+03 0.5600E+03 0.5610E+03
   19       0.5850E+03 0.5860E+03 0.5870E+03 0.5880E+03 0.5890E+03 0.5900E+03 0.5910E+03 0.5920E+03 0.5930E+03
   20       0.6170E+03 0.6180E+03 0.6190E+03 0.6200E+03 0.6210E+03 0.6220E+03 0.6230E+03 0.6240E+03 0.6250E+03
   21       0.6490E+03 0.6500E+03 0.6510E+03 0.6520E+03 0.6530E+03 0.6540E+03 0.6550E+03 0.6560E+03 0.6570E+03
   22       0.6810E+03 0.6820E+03 0.6830E+03 0.6840E+03 0.6850E+03 0.6860E+03 0.6870E+03 0.6880E+03 0.6890E+03
   23       0.7130E+03 0.7140E+03 0.7150E+03 0.7160E+03 0.7170E+03 0.7180E+03 0.7190E+03 0.7200E+03 0.7210E+03
   24       0.7450E+03 0.7460E+03 0.7470E+03 0.7480E+03 0.7490E+03 0.7500E+03 0.7510E+03 0.7520E+03 0.7530E+03
   25       0.7770E+03 0.7780E+03 0.7790E+03 0.7800E+03 0.7810E+03 0.7820E+03 0.7830E+03 0.7840E+03 0.7850E+03
   26       0.8090E+03 0.8100E+03 0.8110E+03 0.8120E+03 0.8130E+03 0.8140E+03 0.8150E+03 0.8160E+03 0.8170E+03
   27       0.8410E+03 0.8420E+03 0.8430E+03 0.8440E+03 0.8450E+03 0.8460E+03 0.8470E+03 0.8480E+03 0.8490E+03
   28       0.8730E+03 0.8740E+03 0.8750E+03 0.8760E+03 0.8770E+03 0.8780E+03 0.8790E+03 0.8800E+03 0.8810E+03
   29       0.9050E+03 0.9060E+03 0.9070E+03 0.9080E+03 0.9090E+03 0.9100E+03 0.9110E+03 0.9120E+03 0.9130E+03
   30       0.9370E+03 0.9380E+03 0.9390E+03 0.9400E+03 0.9410E+03 0.9420E+03 0.9430E+03 0.9440E+03 0.9450E+03
   31       0.9690E+03 0.9700E+03 0.9710E+03 0.9720E+03 0.9730E+03 0.9740E+03 0.9750E+03 0.9760E+03 0.9770E+03
   32       0.1001E+04 0.1002E+04 0.1003E+04 0.1004E+04 0.1005E+04 0.1006E+04 0.1007E+04 0.1008E+04 0.1009E+04


     SAMP           19                    21                    23                    25                    27
 LINE
    1       0.1800E+02 0.1900E+02 0.2000E+02 0.2100E+02 0.2200E+02 0.2300E+02 0.2400E+02 0.2500E+02 0.2600E+02
    2       0.5000E+02 0.5100E+02 0.5200E+02 0.5300E+02 0.5400E+02 0.5500E+02 0.5600E+02 0.5700E+02 0.5800E+02
    3       0.8200E+02 0.8300E+02 0.8400E+02 0.8500E+02 0.8600E+02 0.8700E+02 0.8800E+02 0.8900E+02 0.9000E+02
    4       0.1140E+03 0.1150E+03 0.1160E+03 0.1170E+03 0.1180E+03 0.1190E+03 0.1200E+03 0.1210E+03 0.1220E+03
    5       0.1460E+03 0.1470E+03 0.1480E+03 0.1490E+03 0.1500E+03 0.1510E+03 0.1520E+03 0.1530E+03 0.1540E+03
    6       0.1780E+03 0.1790E+03 0.1800E+03 0.1810E+03 0.1820E+03 0.1830E+03 0.1840E+03 0.1850E+03 0.1860E+03
    7       0.2100E+03 0.2110E+03 0.2120E+03 0.2130E+03 0.2140E+03 0.2150E+03 0.2160E+03 0.2170E+03 0.2180E+03
    8       0.2420E+03 0.2430E+03 0.2440E+03 0.2450E+03 0.2460E+03 0.2470E+03 0.2480E+03 0.2490E+03 0.2500E+03
    9       0.2740E+03 0.2750E+03 0.2760E+03 0.2770E+03 0.2780E+03 0.2790E+03 0.2800E+03 0.2810E+03 0.2820E+03
   10       0.3060E+03 0.3070E+03 0.3080E+03 0.3090E+03 0.3100E+03 0.3110E+03 0.3120E+03 0.3130E+03 0.3140E+03
   11       0.3380E+03 0.3390E+03 0.3400E+03 0.3410E+03 0.3420E+03 0.3430E+03 0.3440E+03 0.3450E+03 0.3460E+03
   12       0.3700E+03 0.3710E+03 0.3720E+03 0.3730E+03 0.3740E+03 0.3750E+03 0.3760E+03 0.3770E+03 0.3780E+03
   13       0.4020E+03 0.4030E+03 0.4040E+03 0.4050E+03 0.4060E+03 0.4070E+03 0.4080E+03 0.4090E+03 0.4100E+03
   14       0.4340E+03 0.4350E+03 0.4360E+03 0.4370E+03 0.4380E+03 0.4390E+03 0.4400E+03 0.4410E+03 0.4420E+03
   15       0.4660E+03 0.4670E+03 0.4680E+03 0.4690E+03 0.4700E+03 0.4710E+03 0.4720E+03 0.4730E+03 0.4740E+03
   16       0.4980E+03 0.4990E+03 0.5000E+03 0.5010E+03 0.5020E+03 0.5030E+03 0.5040E+03 0.5050E+03 0.5060E+03
   17       0.5300E+03 0.5310E+03 0.5320E+03 0.5330E+03 0.5340E+03 0.5350E+03 0.5360E+03 0.5370E+03 0.5380E+03
   18       0.5620E+03 0.5630E+03 0.5640E+03 0.5650E+03 0.5660E+03 0.5670E+03 0.5680E+03 0.5690E+03 0.5700E+03
   19       0.5940E+03 0.5950E+03 0.5960E+03 0.5970E+03 0.5980E+03 0.5990E+03 0.6000E+03 0.6010E+03 0.6020E+03
   20       0.6260E+03 0.6270E+03 0.6280E+03 0.6290E+03 0.6300E+03 0.6310E+03 0.6320E+03 0.6330E+03 0.6340E+03
   21       0.6580E+03 0.6590E+03 0.6600E+03 0.6610E+03 0.6620E+03 0.6630E+03 0.6640E+03 0.6650E+03 0.6660E+03
   22       0.6900E+03 0.6910E+03 0.6920E+03 0.6930E+03 0.6940E+03 0.6950E+03 0.6960E+03 0.6970E+03 0.6980E+03
   23       0.7220E+03 0.7230E+03 0.7240E+03 0.7250E+03 0.7260E+03 0.7270E+03 0.7280E+03 0.7290E+03 0.7300E+03
   24       0.7540E+03 0.7550E+03 0.7560E+03 0.7570E+03 0.7580E+03 0.7590E+03 0.7600E+03 0.7610E+03 0.7620E+03
   25       0.7860E+03 0.7870E+03 0.7880E+03 0.7890E+03 0.7900E+03 0.7910E+03 0.7920E+03 0.7930E+03 0.7940E+03
   26       0.8180E+03 0.8190E+03 0.8200E+03 0.8210E+03 0.8220E+03 0.8230E+03 0.8240E+03 0.8250E+03 0.8260E+03
   27       0.8500E+03 0.8510E+03 0.8520E+03 0.8530E+03 0.8540E+03 0.8550E+03 0.8560E+03 0.8570E+03 0.8580E+03
   28       0.8820E+03 0.8830E+03 0.8840E+03 0.8850E+03 0.8860E+03 0.8870E+03 0.8880E+03 0.8890E+03 0.8900E+03
   29       0.9140E+03 0.9150E+03 0.9160E+03 0.9170E+03 0.9180E+03 0.9190E+03 0.9200E+03 0.9210E+03 0.9220E+03
   30       0.9460E+03 0.9470E+03 0.9480E+03 0.9490E+03 0.9500E+03 0.9510E+03 0.9520E+03 0.9530E+03 0.9540E+03
   31       0.9780E+03 0.9790E+03 0.9800E+03 0.9810E+03 0.9820E+03 0.9830E+03 0.9840E+03 0.9850E+03 0.9860E+03
   32       0.1010E+04 0.1011E+04 0.1012E+04 0.1013E+04 0.1014E+04 0.1015E+04 0.1016E+04 0.1017E+04 0.1018E+04


     SAMP           28                    30                    32
 LINE
    1       0.2700E+02 0.2800E+02 0.2900E+02 0.3000E+02 0.3100E+02
    2       0.5900E+02 0.6000E+02 0.6100E+02 0.6200E+02 0.6300E+02
    3       0.9100E+02 0.9200E+02 0.9300E+02 0.9400E+02 0.9500E+02
    4       0.1230E+03 0.1240E+03 0.1250E+03 0.1260E+03 0.1270E+03
    5       0.1550E+03 0.1560E+03 0.1570E+03 0.1580E+03 0.1590E+03
    6       0.1870E+03 0.1880E+03 0.1890E+03 0.1900E+03 0.1910E+03
    7       0.2190E+03 0.2200E+03 0.2210E+03 0.2220E+03 0.2230E+03
    8       0.2510E+03 0.2520E+03 0.2530E+03 0.2540E+03 0.2550E+03
    9       0.2830E+03 0.2840E+03 0.2850E+03 0.2860E+03 0.2870E+03
   10       0.3150E+03 0.3160E+03 0.3170E+03 0.3180E+03 0.3190E+03
   11       0.3470E+03 0.3480E+03 0.3490E+03 0.3500E+03 0.3510E+03
   12       0.3790E+03 0.3800E+03 0.3810E+03 0.3820E+03 0.3830E+03
   13       0.4110E+03 0.4120E+03 0.4130E+03 0.4140E+03 0.4150E+03
   14       0.4430E+03 0.4440E+03 0.4450E+03 0.4460E+03 0.4470E+03
   15       0.4750E+03 0.4760E+03 0.4770E+03 0.4780E+03 0.4790E+03
   16       0.5070E+03 0.5080E+03 0.5090E+03 0.5100E+03 0.5110E+03
   17       0.5390E+03 0.5400E+03 0.5410E+03 0.5420E+03 0.5430E+03
   18       0.5710E+03 0.5720E+03 0.5730E+03 0.5740E+03 0.5750E+03
   19       0.6030E+03 0.6040E+03 0.6050E+03 0.6060E+03 0.6070E+03
   20       0.6350E+03 0.6360E+03 0.6370E+03 0.6380E+03 0.6390E+03
   21       0.6670E+03 0.6680E+03 0.6690E+03 0.6700E+03 0.6710E+03
   22       0.6990E+03 0.7000E+03 0.7010E+03 0.7020E+03 0.7030E+03
   23       0.7310E+03 0.7320E+03 0.7330E+03 0.7340E+03 0.7350E+03
   24       0.7630E+03 0.7640E+03 0.7650E+03 0.7660E+03 0.7670E+03
   25       0.7950E+03 0.7960E+03 0.7970E+03 0.7980E+03 0.7990E+03
   26       0.8270E+03 0.8280E+03 0.8290E+03 0.8300E+03 0.8310E+03
   27       0.8590E+03 0.8600E+03 0.8610E+03 0.8620E+03 0.8630E+03
   28       0.8910E+03 0.8920E+03 0.8930E+03 0.8940E+03 0.8950E+03
   29       0.9230E+03 0.9240E+03 0.9250E+03 0.9260E+03 0.9270E+03
   30       0.9550E+03 0.9560E+03 0.9570E+03 0.9580E+03 0.9590E+03
   31       0.9870E+03 0.9880E+03 0.9890E+03 0.9900E+03 0.9910E+03
   32       0.1019E+04 0.1020E+04 0.1021E+04 0.1022E+04 0.1023E+04
LAVE PROCESSING COMPLETE
let $echo="no"
$ Return
$!#############################################################################
