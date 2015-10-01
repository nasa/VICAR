$!****************************************************************************
$!
$! Build proc for MIPL module fftpic
$! VPACK Version 1.9, Friday, February 12, 2010, 12:21:32
$!
$! Execute by entering:		$ @fftpic
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
$ write sys$output "*** module fftpic ***"
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
$ write sys$output "Invalid argument given to fftpic.com file -- ", primary
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
$   if F$SEARCH("fftpic.imake") .nes. ""
$   then
$      vimake fftpic
$      purge fftpic.bld
$   else
$      if F$SEARCH("fftpic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fftpic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fftpic.bld "STD"
$   else
$      @fftpic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fftpic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fftpic.com -mixed -
	-s fftpic.f -
	-i fftpic.imake -
	-p fftpic.pdf -
	-t tstfftpic.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fftpic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

C     PROGRAM FFTPIC

C     TRANSFORM/COMPLEX PICTURE DISPLAY PROGRAM

c    20 sep 08     ...lwk...     fixed several bugs in the code
C     5 SEP 94     ...AS (CRI)...MSTP S/W CONVERSION (VICAR PORTING) 
C     1 FEB 85     ...LWK...     converted to Vicar2 
C     16 DEC 83    ...HBD...     FIX LABEL UPDATE FOR VAX COMPATIBLE
C				 RESTURUCTURED CODE.
C     12 OCT 78    ...JBS...     ACCOMODATE 77 LABEL
C   27 JUNE 1975   ...DAH...     CHANGES FOR CONVERSION TO 360/OS
C   11/24/71  ...TCR...  GENERAL OVERHAUL,COMPACT CODING, ERROR MESSAGES,
C                  SYMMETRIC PARAMETER PROCESSING, I/O OVERLAP, POSITIVE
C                  AND NEGATIVE HISTOGRAM PROCESSING, ADD THRESHOLD
C                  PARAMETER, INCREASE BUFFER SIZES, AND CHANGE
C                  DEFAULTS (NMIN AND NMAX)
C   11/1/71  ...TCR81...  FIX 'CALL  OPEN' ERROR

      IMPLICIT INTEGER*4 (A-U,W-Z)
      IMPLICIT REAL (V)
      COMMON /C1/ C(1200),CFACT(1200),VR(1202),VI(1202)
      COMPLEX C,CFACT,C0,CF
      INTEGER NAB(800),IOFFSW,QSWTCH(2),IFLG,MAXSW,MINSW
      INTEGER TAB(800),ISIGN
      INTEGER NMIN,NMAX,MODSW,LSW,RSW,IPIC,ISW
      REAL X0,Y0,SMIN,SMAX,THRESH
      REAL QRGT(601,2),QLFT(601,2),XY(2),FI,FR,PCT
      EQUIVALENCE (XY(1),X0), (XY(2),Y0)
      INTEGER*2 HPIC(1202)
      INTEGER NUMP,NUMN 
      CHARACTER*32 BUF1,BUF3(3)
      CHARACTER*64 MSG,MSG1
      CHARACTER*54 MSG2,LMINMG,LMAXMG
      CHARACTER*72 LPAR
      CHARACTER*3 LOG
      CHARACTER*6 LINEAR
      CHARACTER*9 AMPL,SINE,IMAG,INTE,ABSO
      CHARACTER*5 PHAS
      CHARACTER*4 COSIN,QPIC,AREAL,ASIGN
      EQUIVALENCE (VR,QRGT),(VI,QLFT)
      LOGICAL XVPTST

      CALL XVMESSAGE('*** Program FFTPIC version 12 Feb 2010 ***', ' ')

      IOFFSW=0
      IFLG=0
      MAXSW=0
      MINSW=0
      ISIGN=0
      NMIN=10
      NMAX=10
      MODSW=1
      LSW=1
      RSW=1
      IPIC=0
      ISW=1
      X0=0.0
      Y0=0.0
      SMIN=0.1
      SMAX=0.1
      THRESH=1.0

      CALL ZIA(NAB,800)
      CALL ZIA(TAB,800)
      NUMP=0
      NUMN=0
      QSWTCH(1)=1
      QSWTCH(2)=1
      LMINMG(1:51)=
     &    '*** ****** POINTS (***.*** 0/0) SATURATED BLACK = 0'
      LMAXMG(1:53)=
     &    '*** ****** POINTS (***.*** 0/0) SATURATED WHITE = 255'
      LOG(1:3)='LOG'
      LINEAR(1:6)='LINEAR'
      AMPL(1:9)='AMPLITUDE'
      PHAS(1:5)='PHASE'
      COSIN(1:4)='REAL'
      SINE(1:9)='IMAGINARY'
      QPIC(1:4)='CPIC'
      AREAL(1:4)='REAL'
      IMAG(1:9)='IMAGINARY'
      INTE(1:9)='INTENSITY'
      ASIGN(1:4)='SIGN'
      ABSO(1:8)='ABSOLUTE'

C     MODSW=0  FOR LOG DISPLAY
C     MODSW=1 FOR LINEAR DISPLAY
C
C     ISW/LSW/RSW = 1  AMPLITUDE
C                   2  PHASE (WITH OR WITHOUT SIGN)
C                   3  REAL OR COSINE
C                   4  IMAGINARY OR SINE
C                   5  INTENSITY

      CALL IFMESSAGE('FFTPIC version 5-SEP-94')
      CALL XVEACTION('SA',' ')

C  FIND REQUESTED MODE & DISP:
C  (DEFAULTS ARE TRANSFORM & AMPL)
      IF (XVPTST('CPIC')) IPIC=1

      IF (XVPTST('L_PHASE')) THEN
	LSW = 2
      ELSEIF (XVPTST('L_REAL') .OR. XVPTST('L_COSINE')) THEN
	LSW = 3
      ELSEIF (XVPTST('L_IMAG') .OR. XVPTST('L_SINE')) THEN
	LSW = 4
      ELSEIF (XVPTST('L_INTENS')) THEN
	LSW = 5
      ENDIF
      ISW = LSW

      CALL XVPARM( 'R_DISP', BUF1, I, DFLAG,0)
      IF (IPIC.EQ.1 .AND. DFLAG.EQ.0) THEN
	CALL XVMESSAGE
     &       ('*** CPIC MODE - MORE THAN 1 DISPLAY TYPE ILLEGAL',' ')
	CALL ABEND
      ENDIF

      IF (XVPTST('R_PHASE')) THEN
	RSW = 2
      ELSEIF (XVPTST('R_REAL') .OR. XVPTST('R_COSINE')) THEN
	RSW = 3
      ELSEIF (XVPTST('R_IMAG') .OR. XVPTST('R_SINE')) THEN
	RSW = 4
      ELSEIF (XVPTST('R_INTENS')) THEN
	RSW = 5
      ENDIF

      IF (XVPTST('LOG')) MODSW = 0		! 1 (LINEAR) IS DEFAULT

      CALL XVPARM( 'ORIGIN', XY, I, J,0)

      CALL XVPARM( 'NMIN', SMIN, I, J,0)
      CALL XVPARM( 'NMAX', SMAX, I, J,0)

      IF (XVPTST('SIGN')) ISIGN = 1		! 0 IS DEFAULT

      IF (THRESH.LT.0) THEN
	CALL XVMESSAGE('** REQUIRE THRESHOLD > 0.0 **',' ')
	CALL ABEND
      ELSEIF (THRESH.LT.1.0E-7) THEN
	THRESH = 1.0E-7
      ENDIF

C  OPEN INPUT & GET SIZE:

      CALL XVUNIT( IUN, 'INP', 1, ISTAT,' ')
      CALL XVOPEN( IUN, ISTAT,' ')
      CALL XVGET( IUN, ISTAT, 'NL', NLI, 'NS', NSAMPS,
     &            'PIX_SIZE', BPS,' ')
      NSI = NSAMPS*BPS
      NX=NSI/16+1
      IF(IPIC.NE.0)NX=NSI/8
      NXX=4*NX
      NNX=8*NX
      NY=NLI
      IF ((IPIC.EQ.0 .AND. NX.GT.601) .OR.
     &    (IPIC.NE.0 .AND. NX.GT.1200)) THEN
	CALL XVMESSAGE
     &       ('*** BUFFER LIMIT EXCEEDED - NSI .LE. 9600 BYTES',' ')
	CALL ABEND
      ENDIF

C  CHECK # OF SCRATCH FILES
      CALL XVPARM( 'OUT', BUF3, NDATS, I,0)
      IF (NDATS.EQ.1) THEN
	CALL XVPARM( 'INP', BUF3, NDATS, I,0)
	IF (NDATS.GT.1) THEN
	  CALL XVUNIT( SCR1, 'INP', 2, ISTAT,' ')
	  IF (NDATS.EQ.3) CALL XVUNIT( SCR2, 'INP', 3, ISTAT,' ')
	ENDIF
      ELSE
	CALL XVUNIT( SCR1, 'OUT', 2, ISTAT,' ')
	IF (NDATS.EQ.3) CALL XVUNIT( SCR2, 'OUT', 3, ISTAT,' ')
      ENDIF

      IF (IPIC.EQ.0 .AND. NDATS.LT.3) THEN
	CALL XVMESSAGE
     &      ('*** QUAD MODE - REQUIRE 2 SCRATCH DATA SETS',' ')
	CALL ABEND
      ENDIF
      IF (IPIC.NE.0 .AND. NDATS.LT.2) THEN
	CALL XVMESSAGE('*** CPIC MODE - REQUIRE A SCRATCH DATA SET',' ')
	CALL ABEND
      ENDIF

C     SET UP PHASE ORIGIN TRANSLATION BUFFER

      C0=CMPLX(1.0,0.0)
      V=6.283185*X0/FLOAT(NSI/8)
      CF=CMPLX(COS(V),-SIN(V))
      DO 100 I=1,NX
      CFACT(I)=C0
  100 C0=C0*CF
      V=6.283185*Y0/FLOAT(NY)
      CF=CMPLX(COS(V),SIN(V))
      C0=CMPLX(1.0,0.0)

C  OPEN FIRST IDS
      CALL XVOPEN( SCR1, ISTAT, 'OP', 'WRITE', 'O_FORMAT', 'REAL',
     . 'U_FORMAT', 'REAL', 'U_NL', NY, 'U_NS', NXX/4,' ')

      IF(IPIC.EQ.0) GO TO 500
C
C     CPIC - NO QUADRANT REARRANGEMENT - PROCESS TO 1 INTERMEDIATE
C            DATA SET
C
      DO I=1,NY
	CALL XVREAD( IUN, C, ISTAT,' ')
	IF (IOFFSW.NE.0 .AND. ISW.NE.1 .AND. ISW.NE.5) THEN
	  DO J=1,NX
	     C(J)=C0*CFACT(J)*C(J)
	  ENDDO
	  C0=C0*CF
	ENDIF
	CALL TVERT(NX,ISW,MODSW,ISIGN,THRESH,NUMP,NUMN,TAB,NAB,C,VR)
	CALL XVWRIT( SCR1, VR, ISTAT,' ')
      ENDDO
      CALL XVCLOSE( SCR1, ISTAT,' ')
C
C     GO DETERMINE VMAX AND VMIN
C
      GO TO 6000
C
C     QUADRANT REARRANGEMENT DISPLAY - PROCESS TO 2 INTERMEDIATE
C     DATA SETS
C
  500 CALL XVOPEN( SCR2, ISTAT, 'OP', 'WRITE', 'O_FORMAT', 'REAL',
     . 'U_FORMAT', 'REAL', 'U_NL', NY, 'U_NS', NXX/4,' ')

      DO I=1,NY
	CALL XVREAD( IUN, C, ISTAT,' ')

C RIGHT SIDE:
	IF (IOFFSW.NE.0 .AND. ((RSW.NE.1.AND.RSW.NE.5) .OR.
     &	 (LSW.NE.1.AND.LSW.NE.5))) THEN
	  DO J=1,NX
            C(J)=C0*CFACT(J)*C(J)
	  ENDDO
	  C0=C0*CF
	ENDIF
	CALL TVERT(NX,RSW,MODSW,ISIGN,THRESH,NUMP,NUMN,TAB,NAB,C,VR)
	CALL XVWRIT( SCR1, VR, ISTAT,' ')

C LEFT SIDE:
	IF (LSW.EQ.RSW .AND. LSW.NE.1 .AND. LSW.NE.3 .AND. LSW.NE.5) THEN
	  DO J=1,NX
            C(J)=CONJG(C(J))
	  ENDDO
	ENDIF
	CALL TVERT(NX,LSW,MODSW,ISIGN,THRESH,NUMP,NUMN,TAB,NAB,C,VI)
	CALL XVWRIT( SCR2, VI, ISTAT,' ')
      ENDDO

      CALL XVCLOSE( SCR1, ISTAT,' ')
      CALL XVCLOSE( SCR2, ISTAT,' ')
C
C     DETERMINE VMAX AND VMIN FROM TABULATIONS TAB AND NAB
C
 6000 CONTINUE
      VMAX=3.141593
      VMIN=0.0
      IF(ISIGN.EQ.1) VMIN=-3.141593
      VRMAX=VMAX
      VIMAX=VMAX
      VRMIN=VMIN
      VIMIN=VMIN
      FR=255.0/(VMAX-VMIN)
      FI=FR
      VMINP=VMIN
      VMAXP=VMAX
      VRMINP=VRMIN
      VIMINP=VIMIN
      VRMAXP=VRMAX
      VIMAXP=VIMAX
      IF(IPIC.EQ.0) GO TO 6001
      IF(ISW.EQ.2) GO TO 6019
      GO TO 6002
 6001 IF((LSW.EQ.2).AND.(RSW.EQ.2)) GO TO 6019
 6002 CONTINUE
      IF((NUMP.GT.0).OR.(NUMN.GT.0)) GO TO 6003
      CALL XVMESSAGE('*** NO DATA TABULATED',' ')
      CALL XVMESSAGE('*** NORMALIZATION IMPOSSIBLE',' ')
      CALL ABEND
 6003 CONTINUE
      NMIN=SMIN*FLOAT(NUMP+NUMN)/100.0
      NMAX=SMAX*FLOAT(NUMP+NUMN)/100.0
 6004 CONTINUE
C
C     COMPUTE VMIN
C
      IF(NUMN.LE.NMIN) GO TO 6007
C
C     VMIN NEGATIVE
C
      MINSW=1
      M1=0
      I=801
 6005 CONTINUE
      I=I-1
      M1=M1+NAB(I)
      IF(M1.LE.NMIN) GO TO 6005
 6006 VMIN=FLOAT(I-1)/50.0
      VPRT=THRESH*(10.0**VMIN)
      IF(MODSW.EQ.0) VPRT=ALOG10(VPRT)
      IF(MINSW.EQ.1) VPRT=-VPRT
      IF(MODSW.EQ.1) VMIN=10.0**VMIN
      IF(MINSW.EQ.1) VMIN=-VMIN
      GO TO 6009
C
C     VMIN POSITIVE
C
 6007 CONTINUE
      MINSW=0
      M1=NUMN
      I=0
 6008 CONTINUE
      I=I+1
      M1=M1+TAB(I)
      IF(M1.LE.NMIN) GO TO 6008
      GO TO 6006
C
C     COMPUTE VMAX
C
 6009 CONTINUE
      IF(NUMP.LE.NMAX) GO TO 6012
C
C     VMAX POSITIVE
C
      MAXSW=0
      M2=0
      J=801
 6010 CONTINUE
      J=J-1
      M2=M2+TAB(J)
      IF(M2.LE.NMAX) GO TO 6010
 6011 VMAX=FLOAT(J-1)/50.0
      VQRT=THRESH*(10.0**VMAX)
      IF(MODSW.EQ.0) VQRT=ALOG10(VQRT)
      IF(MAXSW.EQ.1) VQRT=-VQRT
      IF(MODSW.EQ.1) VMAX=10.0**VMAX
      IF(MAXSW.EQ.1) VMAX=-VMAX
      GO TO 6014
C
C     VMAX NEGATIVE
C
 6012 CONTINUE
      MAXSW=1
      M2=NUMP
      J=0
 6013 CONTINUE
      J=J+1
      M2=M2+NAB(J)
      IF(M2.LE.NMAX) GO TO 6013
      GO TO 6011
C
C     CHECK VMIN RELATIVE TO VMAX
C
 6014 CONTINUE
      IF(VMAX.GE.VMIN) GO TO 6015
      CALL XVMESSAGE('*** NORMALIZATION SEARCH ERROR',' ')
      CALL XVMESSAGE('*** SATURATION DECREASED BY FACTOR OF 2',' ')
      NMIN=NMIN/2
      NMAX=NMAX/2
      GO TO 6004
 6015 CONTINUE
      IF(VMAX.GT.VMIN) GO TO 6016
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('*** NORMALIZATION MAX AND MIN EQUAL',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('*** MAX ADJUSTED UPWARD',' ')
      IF(MAXSW.EQ.0) J=J+1
      IF(MAXSW.EQ.1) J=J-1
      IF(J.GT.0) GO TO 6011
      MAXSW=0
      J=1
      GO TO 6011
 6016 CONTINUE
      IF(IPIC.EQ.1) GO TO 6018
      IF(RSW.EQ.2) GO TO 6017
      VRMAX=VMAX
      VRMIN=VMIN
      FR=255.0/(VMAX-VMIN)
      VRMAXP=VQRT
      VRMINP=VPRT
 6017 CONTINUE
      IF(LSW.EQ.2) GO TO 6019
      VIMAX=VMAX
      VIMIN=VMIN
      FI=255.0/(VMAX-VMIN)
      VIMAXP=VQRT
      VIMINP=VPRT
      GO TO 6019
 6018 CONTINUE
      IF(ISW.EQ.2) GO TO 6019
      FR=255.0/(VMAX-VMIN)
      VMINP=VPRT
      VMAXP=VQRT
 6019 CONTINUE
      IF((NUMN+NUMP).LT.1) GO TO 6020
      WRITE(LMINMG(4:10),'(I7)')M1
      PCT=100.0*FLOAT(M1)/FLOAT(NUMN+NUMP)
      IF(PCT.LT.0.0) PCT=0.0
      IF(PCT.GT.100.0) PCT=100.0
      WRITE(LMINMG(20:26),'(F7.3)')PCT
      WRITE(LMAXMG(4:10),'(I7)')M2
      PCT=100.0*FLOAT(M2)/FLOAT(NUMN+NUMP)
      IF(PCT.LT.0.0) PCT=0.0
      IF(PCT.GT.100.0) PCT=100.0
      WRITE(LMAXMG(20:26),'(F7.3)')PCT
 6020 CONTINUE
C
C     PRINT OUT LINEAR SCALING PARAMETERS FOR OUTPUT
C
      IF(IPIC.EQ.1) GO TO 503
      IF(((NUMN+NUMP).LT.1).OR.((LSW.EQ.2).AND.(RSW.EQ.2))) GO TO 501
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(LMINMG,' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(LMAXMG,' ')
  501 CONTINUE
      WRITE(MSG,9900)VRMAXP,VRMINP
9900  FORMAT('*** RIGHTSIDE NORMALIZATION MAXIMUM=',F9.2,'  MINIMUM=',
     &       F9.2)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG,' ')
      WRITE(MSG1,9910)VIMAXP,VIMINP
9910  FORMAT('*** LEFTSIDE NORMALIZATION MAXIMUM=',F9.2,'  MINIMUM=',
     &       F9.2)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG1,' ')
      GO TO 504
  503 CONTINUE
      IF(((NUMN+NUMP).LT.1).OR.(ISW.EQ.2)) GO TO 502
      CALL XVMESSAGE(LMINMG,' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(LMAXMG,' ')
  502 CONTINUE
      WRITE(MSG2,9920)VMAXP,VMINP
9920  FORMAT('*** NORMALIZATION MAXIMUM=',F9.2,'  MINIMUM=',F9.2)
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG2,' ')
  504 CONTINUE
C
C  OPEN OUTPUT FILE 
C
    6 NNX = NX
      NLINS = NY
      NSMPS = NNX
      IF (IPIC.EQ.0) THEN
	NNX = 2*NX
	NSMPS = NNX
	IF (RSW.EQ.LSW) NSMPS = NNX-1
	NY=NY/2+1
	NLINS = 2*NY-2
      ENDIF
      CALL XVUNIT( OUN, 'OUT', 1, ISTAT,' ')
      CALL XVOPEN( OUN, ISTAT, 'OP', 'WRITE', 'O_FORMAT', 'BYTE',
     . 'U_FORMAT', 'HALF', 'U_NL', NLINS, 'U_NS', NSMPS,' ')

C  CONSTRUCT THE NEW HISTORY LABEL:
      J = 0
      LPAR=' '
      IF (ISW.EQ.2) MODSW=1
      IF ((LSW.EQ.2).AND.(RSW.EQ.2)) MODSW=1
      IF (MODSW.EQ.1) GO TO 713
      LPAR(1:3)=LOG
      J=J+5
      IF((LSW.EQ.2).OR.(RSW.EQ.2))GO TO 738
      GO TO 735
  713 LPAR((J+1):(J+6))=LINEAR
      J=J+8
      IF((ISW.NE.2).OR.((RSW.NE.2).AND.(LSW.NE.2))) GO TO 735
  738 IF(ISIGN.NE.1) GO TO 715
      LPAR((J+1):(J+4))=ASIGN
      J=J+6
      GO TO 735
  715 LPAR((J+1):(J+8))=ABSO
      J=J+10
  735 IF(IPIC.NE.0)GO TO 730
      I=LSW
      IFLG=0
  716 GO TO (717,719,721,723,727),I
  727 LPAR((J+1):(J+9))=INTE
      J=J+10
      GO TO 725
  717 LPAR((J+1):(J+9))=AMPL
      J=J+10
      GO TO 725
  719 LPAR((J+1):(J+5))=PHAS
      J=J+6
      GO TO 725
  721 CONTINUE
      LPAR((J+1):(J+4))=COSIN
      J=J+5
      GO TO 725
  723 CONTINUE
      LPAR((J+1):(J+9))=SINE
      J=J+10
  725 IF(IFLG.EQ.1) GO TO 726
      IFLG=1
      LPAR(J:J)='/'
      I=RSW
      GO TO 716
  730 IFLG=1
      LPAR((J+1):(J+4))=QPIC
      J=J+6
      GO TO (717,719,731,733,727),ISW
  731 LPAR((J+1):(J+4))=AREAL
      J=J+5
      GO TO 726
  733 LPAR((J+1):(J+9))=IMAG
      J=J+10
  726 CONTINUE
      J = MIN0( J, 72)
      CALL XLADD( OUN, 'HISTORY', 'PGM_LAB', LPAR, ISTAT, 'ULEN', J,
     . 'FORMAT', 'STRING',' ')

      CALL XVOPEN( SCR1, ISTAT, 'OP', 'READ', ' ')

      IF(IPIC.EQ.0) GO TO 740
C  CPIC MODE OUTPUT - NO QUADRANT REARRANGEMENT

      DO I=1,NY
	CALL XVREAD( SCR1, VR, ISTAT,' ')
	DO J=1,NX
	   M=FR*(VR(J)-VMIN)+0.5
	   IF(M.LT.0)M=0
	   IF(M.GT.255)M=255
	   HPIC(J)=M
	ENDDO
	CALL XVWRIT( OUN, HPIC, ISTAT,' ')
      ENDDO
      GO TO 800

C  QUAD MODE OUTPUT - QUADRANT REARRANGEMENT

  740 CALL XVOPEN( SCR2, ISTAT, 'OP', 'READ', ' ')	! NEED IDS 2

      IR0 = NY+1
      L0 = NY-1
      M1 = NX-1
      M2 = NX+1
      ISW = 1
      IF (RSW.NE.LSW) THEN
	M1 = NX
	L0 = IR0
      ENDIF

   10 KSW=1
      CALL XVREAD( SCR1, QRGT(1,KSW), ISTAT, 'LINE', IR0-1,' ')
      IF (RSW.EQ.LSW) THEN
	CALL XVREAD( SCR2, QLFT(1,KSW), ISTAT, 'LINE', L0+1,' ')
      ELSE
	CALL XVREAD( SCR2, QLFT(1,KSW), ISTAT, 'LINE', L0-1,' ')
      ENDIF

      DO I=1,NY
	MSW=KSW
	KSW=3-KSW
	K=I+1
	IF (K.LE.NY)
     .	 CALL XVREAD( SCR1, QRGT(1,KSW), ISTAT, 'LINE', IR0-K,' ')
	DO J=1,NX
	  M=FR*(QRGT(J,MSW)-VRMIN)+0.5
	  IF(M.LT.0) M=0
	  IF(M.GT.255) M=255
	  HPIC(M1+J)=M
	ENDDO
	IF (K.LE.NY) THEN
	  IF (LSW.NE.RSW) THEN
	    CALL XVREAD( SCR2, QLFT(1,KSW), ISTAT, 'LINE', L0-K,' ')
	  ELSEIF (ISW.EQ.1 .AND. K.EQ.NY) THEN
	    CALL XVREAD( SCR2, QLFT(1,KSW), ISTAT, 'LINE', 1,' ')
	  ELSE
	    CALL XVREAD( SCR2, QLFT(1,KSW), ISTAT, 'LINE', L0+K,' ')
	  ENDIF
	ENDIF
	DO J=1,NX
	  M=FI*(QLFT(J,MSW)-VIMIN)+0.5
	  IF(M.LT.0) M=0
	  IF(M.GT.255) M=255
	  HPIC(M2-J)=M
	ENDDO
	CALL XVWRIT( OUN, HPIC, ISTAT,' ')
      ENDDO

      IF(ISW.NE.2) THEN
         ISW=2
         NY=NY-2
         IR0=2*NY+3
         L0=IR0
         IF(RSW.EQ.LSW) L0=1
         GO TO 10
      ENDIF

  800 CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('*** FFTPIC TASK COMPLETED',' ')
      RETURN
      END

C********************************************************
      SUBROUTINE TVERT(NX,ISW,MODSW,ISIGN,THRESH,NUMP,NUMN,TAB,NAB,C,
     &OUT)
C  PROGRAM TO CONVERT COMPLEX DATA FOR DISPLAY
      COMPLEX C(1)
      INTEGER TAB(1),NAB(1)
      REAL OUT(1)
C
C     ISW = 1   AMPLITUDE
C           2   PHASE
C           3   REAL/COSINE
C           4   IMAGINARY/SINE
C           5   INTENSITY
C
C
C     MODSW = 0   LOG DISPLAY
C             1   LINEAR DISPLAY
C
C
      JSW=MODSW+1
      FACT=1.0/THRESH
      GO TO (1,6,8,14,20),ISW
C
C     AMPLITUDE CONVERSION
C
    1 CONTINUE
      DO 5 I=1,NX
         X=FACT*CABS(C(I))
         IF(X.LT.1.0) X=1.0
         Y=ALOG10(X)
         M=50.0*Y+1.5
         IF(M.LT.1) M=1
         IF(M.GT.800) M=800
         IF (JSW .EQ. 2) THEN
	    OUT(I) = X			! LINEAR
	 ELSE
            OUT(I)=Y			! LOG
	 ENDIF
         TAB(M)=TAB(M)+1
    5 CONTINUE
      NUMP=NUMP+NX
      RETURN
C
C     PHASE CONVERSION  (ALWAYS LINEAR)
C
    6 CONTINUE
      DO 7 I=1,NX
         X=REAL(C(I))
         Y=AIMAG(C(I))
         IF(X.EQ.0.0) THEN
            IF(Y.EQ.0.0) THEN
               X=0.0
            ELSE
               X=SIGN(1.570796,Y)
            ENDIF
         ELSE
            X=ATAN2(Y,X)
         ENDIF
         IF(ISIGN.EQ.0) X=ABS(X)
         OUT(I)=X
    7 CONTINUE
      RETURN
C
C     REAL/COSINE DISPLAY
C
    8 CONTINUE
      DO 13 I=1,NX
         X=FACT*REAL(C(I))
         IF(ABS(X).LT.1.0) X=SIGN(1.0,X)
         Y=ALOG10(ABS(X))
         M=50.0*Y+1.5
         IF(M.LT.1) M=1
         IF(M.GT.800) M=800
         IF (JSW .EQ. 2) THEN
	    OUT(I) = X			!LINEAR
	 ELSE
            OUT(I)=SIGN(Y,X)		!LOG
         ENDIF
         IF(X.LT.0.0) THEN
            NAB(M)=NAB(M)+1
            NUMN=NUMN+1
	 ELSE
            TAB(M)=TAB(M)+1
            NUMP=NUMP+1
	 ENDIF
   13 CONTINUE
      RETURN
C
C     IMAGINARY/SINE DISPLAY
C
   14 CONTINUE
      DO 19 I=1,NX
         X=FACT*AIMAG(C(I))
         IF(ABS(X).LT.1.0) X=SIGN(1.0,X)
         Y=ALOG10(ABS(X))
         M=50.0*Y+1.5
         IF(M.LT.1) M=1
         IF(M.GT.800) M=800
         IF (JSW .EQ. 2) THEN
            OUT(I) = X				! LINEAR
         ELSE
            OUT(I)=SIGN(Y,X)			! LOG
     	 ENDIF
         IF(X.LT.0.0) THEN
            NAB(M)=NAB(M)+1
            NUMN=NUMN+1
	 ELSE
            TAB(M)=TAB(M)+1
            NUMP=NUMP+1
         ENDIF
   19 CONTINUE
      RETURN
C
C     INTENSITY
C
   20 CONTINUE
      DO 24 I=1,NX
         X=REAL(C(I))
         Y=AIMAG(C(I))
         X=FACT*(X*X+Y*Y)
         IF(X.LT.1.0) X=1.0
         Y=ALOG10(X)
         M=50.0*Y+1.5
         IF(M.LT.1) M=1
         IF(M.GT.800) M=800
         IF (JSW .EQ. 2) THEN
	    OUT(I) = X				!LINEAR
	 ELSE
	    OUT(I) = Y				!LOG
	 ENDIF
         TAB(M)=TAB(M)+1
   24 CONTINUE
      NUMP=NUMP+NX
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fftpic.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fftpic

   To Create the build file give the command:

		$ vimake fftpic			(VMS)
   or
		% vimake fftpic			(Unix)


************************************************************************/


#define PROGRAM	fftpic
#define R2LIB

#define MODULE_LIST fftpic.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define DEBUG	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fftpic.pdf
PROCESS HELP=*
PARM INP (STRING,32) COUNT=(1:3)
PARM OUT (STRING,32) COUNT=(1:3)
PARM MODE KEYWORD VALID=(CPIC,TRANSFORM) DEFAULT=TRANSFORM
PARM L_DISP KEYWORD VALID=(L_AMPL,L_PHASE,L_COSINE,L_SINE,L_INTENS, +
  L_REAL,L_IMAG) DEFAULT=L_AMPL
PARM R_DISP KEYWORD VALID=(R_AMPL,R_PHASE,R_COSINE,R_SINE,R_INTENS, +
  R_REAL,R_IMAG) DEFAULT=R_AMPL
PARM SCALING KEYWORD VALID=(LOG,LINEAR)  DEFAULT=LINEAR
PARM ORIGIN REAL COUNT=2 DEFAULT=(1.,1.)
PARM NMIN REAL DEFAULT=0.1
PARM NMAX REAL DEFAULT=0.1
PARM SIGN KEYWORD VALID=SIGN COUNT=(0:1) DEFAULT=--
PARM THRESHOL REAL DEFAULT=1.0
END-PROC
!
.TITLE
Program "fftpic"
.HELP
PURPOSE:

"fftpic" processes input VICAR formatted complex fourier transformations
or complex pictures, extracting, reorganizing, and automatically scaling
various functions of the complex data for image display. Functions of 
the complex input that may be displayed include: amplitude, intensity 
(squared amplitude), phase (with or without sign), real part, and 
imaginary part. Linear or logarithmic scaling prior to output exists for 
all functions except phase.

NOTE: The input transform format is assumed to be that produced by VICAR
program FFT22.  See HELP FFT22 for more details.
.page 
EXECUTION:
               fftpic  IN  OUT  PARAMS
 where:

 IN denotes 1-3 input files: a primary input and one or two scratch files.
 
 OUT denotes 1-3 output files: the primary output and one or two scratch
  files.

  (Note that the scratch files may be specified as EITHER input OR
  output files, but not both.  TRANSFORM mode requires two scratch
  files, CPIC mode requires one.  See OPERATION, below.)
 
 PARAMS includes other available parameters, viz.:
 
MODE   SCALING   L_DISP   R_DISP   ORIGIN   NMIN    NMAX   SIGN   THRESHOL

Each is explained in their respective parameter section. (Use TUTOR mode.)
.page
OPERATION:

It is assumed the input data set is a complex*8 matrix with a standard 
Vicar2 label.  The picture size to be processed is derived from the system 
label, so no size field is allowed. The output system label is automatically
updated based on operating mode and input size information.  The output
file is a byte image suitable for display.  (It is referred to as the
"Display" in the following text.)

"fftpic" operates in two modes: the "Transform" mode and the "Complex Picture"
(CPIC) mode.  These are explained below.

In the TRANSFORM display mode, the data are assumed to be organized as 
described in the 'FFT22' Help text.  However the user should be aware that
the FT generated by FFT22, is transposed with respect to normal usage.
In this mode, the image is rearranged so that the DC term (the [0,0] 
frequency) is at the center of the display.  In this mode, the redundant 
frequencies on the positive and negative X-axis are replicated for clarity, 
so the display contains one or two (depending on display types requested) 
more samples than does the input.

In the COMPLEX PICTURE mode, the display has the same size (in samples) 
and structure as does the input.

Depending on the mode specified, the complex input data are first processed
into one (Complex Picture mode) or two (Transform mode) intermediate data 
sets.  During this processing the specified display function(s) of the 
complex input is (are) generated, scaled, and stored as real*4 data. In 
the Complex Picture mode the entire input is processed uniformly into one 
intermediate data set. Hence, only one scratch file is required. 
On the other hand, in the Transform mode the input conjugate symmetry is 
used to divide the display into two halves which may have display types 
specified independently. Hence two scratch files are required. In the 
Transform mode only the left half plus one samples are processed.

In order to efficiently scale and pack the resulting data into byte format, 
a histogram (which may be logarithmic) is accumulated for the entire input.
Then the histogram is analyzed to determine a linear transformation on the
intermediate data for packing into the output. For phase information displays,
the packing transformation is predefined between -PI and +PI or 0 and + PI 
depending on whether or not 'SIGN' has been specified, respectively.

If the two halves of the transform display are in the same mode, the output
display exhibits conjugate central symmetry with spatial frequency. DC (i.e.
(0,0) spatial frequency) is located at line (NY/2 + 1) and sample (NX/2 + 1).
Positive and negative horizontal spatial frequency components are positioned
to the right and left of this point, respectively, along a horizontal axis
similarly, positive and negative vertical frequency components are located
above and below this point along a vertical axis.

NOTE:  the user should be aware that the vertical dimension is reversed
with respect to normal image processing convention:  the positive Y-axis
is upward, whereas for an image the increasing LINE dimension is downward.

If the two halves are in different modes, the display is organized by spatial
frequency components symmetrically about the vertical line with sample coor-
dinate (NX/2 + 1.5).  Therefore, DC for the right half is at line (NY/2 + 1)
and sample (NX/2 + 2), while DC for the left half is at line (NY/2 + 1) and
sample (NX/2 + 1).  Positive and negative vertical spatial frequencies are 
located above and below these DC points, respectively, for each half of
the display along vertical axis.  Positive horizontal spatial frequency
increases to the right for the right hand display and to the left for the left
hand display along a horizontal spatial frequency components are not explicit-
ly displayed in this case.  The scaling applied to both halves is the same
(determined from the composite histogram) except if one half is phase and the
other is amplitude (or another non-phase display).  In this case the predefined
phase scaling is used on the appropriate half and that derived from the
histogram on the other.
.page
SCRATCH DATASETS:

These may be specified either as input files or as output files, but not
both.  The reason for this feature is that the initial version of FFTPIC
required that they be pre-allocated with the correct size, and hence were
input files.  Currently, Vicar2 allows run-time determination of file
size, so the scratch files are created by the program and were therefore
made output files.  However, for consistency with existing user practice
the option of input scratch files was retained.  In no case do they need
to be pre-allocated.
.page
EXAMPLE:
 
        gen A 256 256
        fft22 A (B,SCR)
        fftpic C (OUT,X1,X2) 'L_AMPL 'R_PHASE 'LOG 'SIGN +
          THRES=.01 NMIN=5.0 NMAX=7.0 ORIGIN=(6,8)
 
The input to this example is a 256 by 256 sample (2048 bytes) transform. 
The default display mode (TRANSFORM) is used. The output display matrix 
consists of a logarithmic amplitude display (left half) and a linear 
phase display with the sign of the angle preserved (right half). A 
multiplication of 1/.01 is performed prior to clipping results less than 
1.0 in absolute value (i.e output = 100*input). The phase origin location 
will be at line 8 and sample 6. Finally, 5 percent of the non-phase 
information portion of the display will be saturated black (DN=0) and 7 
percent will be saturated white (DN=255).

TIMING:
	NONE AVAILABLE FOR THE VAX

WRITTEN BY:  T. C. RINDFLEISCH		  NOVEMBER 22, 1971
CONVERTED TO VAX BY:  Helen De Rueda      DECEMBER 19, 1983
CONVERTED TO VICAR2 BY:  L. W. Kamp       5 FEB 1985
Made portable for UNIX by: A. Scop (CRI)  5 SEP 1994
Bug fixes:  L. W. Kamp                    12 FEB 2010
CURRENT COGNIZANT PROGRAMMER:  L. W. Kamp
.LEVEL1
.VARI INP
Input image, plus 1 or
2 intermediate files.
.VARI OUT
Output image, plus 1 or
2 intermediate files.
.VARI SCALING
KEYWORD. Histogram scaling.
Valid: LOG,LINEAR
.VARI L_DISP
KEYWORD: Left display type.
Valid: L_AMPL,L_PHASE,L_COSINE,
L_SINE,L_INTENS,L_REAL,L_IMAG
.VARI R_DISP
KEYWORD: Right display type.
Valid: R_AMPL,R_PHASE,R_COSINE,
R_SINE,R_INTENS,R_REAL,R_IMAG
.VARI ORIGIN
Coordinates of phase origin.
.VARI NMIN
Fraction of non-phase portion
of display to be saturated
to black.
.VARI NMAX
Fraction of non-phase portion
of display to be saturated to
white.
.VARI MODE
KEYWORD: operating mode.
Valid: TRANSFORM,CPIC
.VARI SIGN
Sign of angle.
.VARI THRESHOL
Threshold used to scale
display information.
.LEVEL2
.VARI INP
This specifies the input image, plus 1 or 2 intermediate (scratch) 
datasets used during the processing.  The scratch datasets may be 
specified as input files or as output files. (But not both.) The
scratch files are always created by the program, hence do not need
to be pre-allocated by the user.

In TRANSFORM mode, the primary input is assumed to be a complex
Fourier-transform image in the format produced by program FFT22.
In CPIC mode, the primary input may be any complex image 
(or a real image with even number of samples).
 
The sizes of the intermediate files will be equal to the input in samples,
but will of format REAL.
 
If the keyword 'CPIC' has been specified, then only one scratch file is 
required.  Two scratch files are required for TRANSFORM mode.
.VARI OUT
This specifies the output image, plus 1 or 2 intermediate (scratch) 
datasets used during the processing.  The scratch datasets may be 
specified as input files or as output files. (But not both.)
 
The size of the primary output will be depend on MODE: in TRANSFORM mode
it will be larger than the input in samples by 1 (if L_DISP is the same
as R_DISP) or 2 (if L_DISP and R_DISP are different) samples;  in CPIC
mode it will have the same number of samples.  In all cases, the format 
will be BYTE and the number of lines will be that of the primary input.
 
The sizes of the intermediate files will be equal to the input in samples,
but will of format REAL.
 
If the keyword 'CPIC' has been specified, then only one scratch file is
required.
.VARI SCALING
This parameter has two valid keyword values: LINEAR and LOG.
 
LINEAR specifies linear scaling of the display information selected.
This is the default.
  
LOG specifies logarithmic (base 10) scaling of the display information
selected. This modifier applies only to non-phase portions of the requested
displays. 
.VARI L_DISP
This parameter has 7 valid keyword values: L_AMPL, L_PHASE, L_COSINE, L_SINE,
L_INTENS, L_REAL, L_IMAG.  It specifies the information displayed in the
left half of the primary output for TRANSFORM mode, or for the entire
output for Complex Picture (CPIC) mode.
 
  L_AMPL displays amplitude information. This is the default.
  L_PHASE displays phase information without sign.
  L_COSINE displays real-part information. Same as L_REAL.
  L_SINE displays imaginary-part information. Same as L_IMAG.
  L_INTENS displays intensity (squared amplitude) information.
  L_REAL displays real-part information. Same as L_COSINE.
  L_IMAG displays imaginary-part information. Same as L_SINE.
.VARI R_DISP
This parameter has 7 valid keyword values: R_AMPL, R_PHASE, R_COSINE, R_SINE,
R_INTENS, R_REAL, R_IMAG.  It specifies the information displayed in the
right half of the primary output for TRANSFORM mode ONLY.
 
This parameter is ignored for Complex Picture (CPIC) mode.
 
  R_AMPL displays amplitude information. This is the default.
  R_PHASE displays phase information without sign.
  R_COSINE displays real-part information. Same as R_REAL.
  R_SINE displays imaginary-part information. Same as R_IMAG.
  R_INTENS displays intensity (squared amplitude) information.
  R_REAL displays real-part information. Same as R_COSINE.
  R_IMAG displays imaginary-part information. Same as R_SINE.
.VARI ORIGIN
ORIGIN = (X,Y), where X and Y are floating point numbers specifying the 
sample and line coordinates of the requested phase origin location.
NOTE: (1,1) is the upper left hand corner. 
Default = (1,1)
.VARI NMIN
NMIN is a floating point number specifying the fraction of non-phase 
information portions of the display which will be saturated to black (0). 
Default = 0.1 (10 percent)
.VARI NMAX
NMAX is a floating point number specifying the fraction of non-phase 
information portions of the  display which will be saturated to 
white (255). 
Default = 0.1 (10 percent)
.VARI SIGN
SIGN specifies that the sign of the angle (-P<ANGLE<=P, where the value
of PI is to be preserved in scaling phase information for display. The
specification of SIGN applies to both left and right halves of the 'transform'
display if two display types are called for. Default is suppression of SIGN
information in the phase angle displays).
.VARI THRESHOL
THRESHOL=R4 where R4 is a floating point number specifying a threshold used to
scale display information prior to clipping results less than 1.0 in absolute
value. Default is 1.0.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfftpic.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!This is a test for fftpic
!
! example from HELP:
gen A 256 256
fft22 A B
fftpic B (OUT,X1,X2) 'L_AMPL 'R_PHASE 'LOG 'SIGN +
          THRES=.01 NMIN=5.0 NMAX=7.0 ORIGIN=(6,8)
list B (110,110,30,30)
!
!This example fftpic's a 4x8 real4 picture using transform mode.
!
gen A 4 8 'REAL4
fftpic A (B,X1,X2)
list B
!
!Do the same with scratch files as inputs:
fftpic (A,Y1,Y2) B
list B
!
!This example fftpic's a 64x64 complex picture using complex mode.
!
gen A 64 128 'REAL4
fftpic A (B,X1) 'CPIC 'L_PHASE
list B
!
!This example fftpic's a 100x100 complex picture using the default mode.
!The output matrix consists of a logarithmic amplitude display (left half)
!and a linear phase display with the sign of the angle preserved (right half).
!A multiplication of 1/.01 is performed prior to clipping results less than
!1.0 in absolute value (i.e. output = 100 * input). The phase origin location
!will be at line 8 and sample 6. Finally, 5 percent of the non-phase 
!information portion of the display will be saturated black (DN=0) and 7
!percnet will be saturated white (DN=255).
gen A 100 200 'REAL4
fftpic A (B,X1,X2)  'L_AMPL 'R_PHASE 'LOG 'SIGN THRESH=.01+
NMIN=5.0 NMAX=7.0 ORIGIN=(6.0,8.)
list B (1,1,5,100)
list B (40,40,20,20)
!
end-proc
$ Return
$!#############################################################################
