$!****************************************************************************
$!
$! Build proc for MIPL module dfft
$! VPACK Version 1.9, Monday, December 07, 2009, 16:10:52
$!
$! Execute by entering:		$ @dfft
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module dfft ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to dfft.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("dfft.imake") .nes. ""
$   then
$      vimake dfft
$      purge dfft.bld
$   else
$      if F$SEARCH("dfft.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dfft
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dfft.bld "STD"
$   else
$      @dfft.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dfft.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dfft.com -mixed -
	-s dfft.f -
	-i dfft.imake -
	-t tdfft.f tdfft.imake tdfft.pdf tstdfft.pdf -
	-o dfft.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dfft.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C/*   15 JUNE 77  ..JEK..    INITIAL RELEASE */
      SUBROUTINE dfft(A,B,NTOT,N,NSPAN,ISN,*,*)
C  MULTIVARIATE COMPLEX FOURIER TRANSFORM, COMPUTED IN PLACE
C    USING MIXED-RADIX FAST FOURIER TRANSFORM ALGORITHM.
C  BY R. C. SINGLETON, STANFORD RESEARCH INSTITUTE, OCT. 1968
C  ARRAYS A AND B ORIGINALLY HOLD THE REAL AND IMAGINARY
C    COMPONENTS OF THE DATA, AND RETURN THE REAL AND
C    IMAGINARY COMPONENTS OF THE RESULTING FOURIER COEFFICIENTS.
C  MULTIVARIATE DATA IS INDEXED ACCORDING TO THE FORTRAN
C    ARRAY ELEMENT SUCCESSOR FUNCTION, WITHOUT LIMIT
C    ON THE NUMBER OF IMPLIED MULTIPLE SUBSCRIPTS.
C    THE SUBROUTINE IS CALLED ONCE FOR EACH VARIATE.
C    THE CALLS FOR A MULTIVARIATE TRANSFORM MAY BE IN ANY ORDER.
C  NTOT IS THE TOTAL NUMBER OF COMPLEX DATA VALUES.
C  N IS THE DIMENSION OF THE CURRENT VARIABLE.
C  NSPAN/N IS THE SPACING OF CONSECUTIVE DATA VALUES
C    WHILE INDEXING THE CURRENT VARIABLE.
C  ISN*NSPAN/N IS THE SPACING OF CONSECUTIVE DATA VALUES.
C  THE SIGN OF ISN DETERMINES THE SIGN OF THE COMPLEX
C    EXPONENTIAL, AND THE MAGNITUDE OF ISN IS NORMALLY ONE.
C  A TRI-VARIATE TRANSFORM WITH A(N1,N2,N3), B(N1,N2,N3)
C    IS COMPUTED BY
C      CALL FFT(A,B,N1*N2*N3,N1,N1,1)
C      CALL FFT(A,B,N1*N2*N3,N2,N1*N2,1)
C      CALL FFT(A,B,N1*N2*N3,N3,N1*N2*N3,1)
C  FOR A SINGLE-VARIATE TRANSFORM,
C    NTOT = N = NSPAN = (NUMBER OF COMPLEX DATA VALUES), E.G.
C      CALL FFT(A,B,N,N,N,1)
C  THE DATA MAY ALTERNATIVELY BE STORED IN A SINGLE COMPLEX
C    ARRAY A, THEN THE MAGNITUDE OF ISN CHANGED TO TWO TO
C    GIVE THE CORRECT INDEXING INCREMENT AND A(2) USED TO
C    PASS THE INITIAL ADDRESS FOR THE SEQUENCE OF IMAGINARY
C    VALUES, E.G.
C      CALL FFT(A,A(2),NTOT,N,NSPAN,2)
C  ARRAYS AT(MAXF), CK(MAXF), BT(MAXF), SK(MAXF), AND NP(MAXP)
C    ARE USED FOR TEMPORARY STORAGE.  IF THE AVAILABLE STORAGE
C    IS INSUFFICIENT, THE PROGRAM IS TERMINATED BY A STOP.
C    MAXF MUST BE .GE. THE MAXIMUM PRIME FACTOR OF N.
C    MAXP MUST BE .GT. THE NUMBER OF PRIME FACTORS OF N.
C    IN ADDITION, IF THE SQUARE-FREE PORTION K OF N HAS TWO OR
C    MORE PRIME FACTORS, THEN MAXP MUST BE .GE. K-1.
      DIMENSION A(1),B(1)
C  ARRAY STORAGE IN NFAC FOR A MAXIMUM OF 11 FACTORS OF N.
C  IF N HAS MORE THAN ONE SQUARE-FREE FACTOR, THE PRODUCT OF THE
C    SQUARE-FREE FACTORS MUST BE .LE. 210
      DIMENSION NFAC(11),NP(209)
C  ARRAY STORAGE FOR MAXIMUM PRIME FACTOR OF 23
      DIMENSION AT(23),CK(23),BT(23),SK(23)
      EQUIVALENCE (I,II)
C  THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS.
      MAXF=23
      MAXP=209
      IF(N .LT. 2) RETURN
      INC=ISN
      RAD=8.0*ATAN(1.0)
      S72=RAD/5.0
      C72=COS(S72)
      S72=SIN(S72)
      S120=SQRT(0.75)
      IF(ISN .GE. 0) GO TO 10
      S72=-S72
      S120=-S120
      RAD=-RAD
      INC=-INC
   10 NT=INC*NTOT
      KS=INC*NSPAN
      KSPAN=KS
      NN=NT-INC
      JC=KS/N
      RADF=RAD*FLOAT(JC)*0.5
      I=0
      JF=0
C  DETERMINE THE FACTORS OF N
      M=0
      K=N
      GO TO 20
   15 M=M+1
      NFAC(M)=4
      K=K/16
   20 IF(K-(K/16)*16 .EQ. 0) GO TO 15
      J=3
      JJ=9
      GO TO 30
   25 M=M+1
      NFAC(M)=J
      K=K/JJ
   30 IF(MOD(K,JJ) .EQ. 0) GO TO 25
      J=J+2
      JJ=J**2
      IF(JJ .LE. K) GO TO 30
      IF(K .GT. 4) GO TO 40
      KT=M
      NFAC(M+1)=K
      IF(K .NE. 1) M=M+1
      GO TO 80
   40 IF(K-(K/4)*4 .NE. 0) GO TO 50
      M=M+1
      NFAC(M)=2
      K=K/4
   50 KT=M
      J=2
   60 IF(MOD(K,J) .NE. 0) GO TO 70
      M=M+1
      NFAC(M)=J
      K=K/J
   70 J=((J+1)/2)*2+1
      IF(J .LE. K) GO TO 60
   80 IF(KT .EQ. 0) GO TO 100
      J=KT
   90 M=M+1
      NFAC(M)=NFAC(J)
      J=J-1
      IF(J .NE. 0) GO TO 90
C  COMPUTE FOURIER TRANSFORM
  100 SD=RADF/FLOAT(KSPAN)
      CD=2.0*SIN(SD)**2
      SD=SIN(SD+SD)
      KK=1
      I=I+1
      IF(NFAC(I) .NE. 2) GO TO 400
C  TRANSFORM FOR FACTOR OF 2 (INCLUDING ROTATION FACTOR)
      KSPAN=KSPAN/2
      K1=KSPAN+2
  210 K2=KK+KSPAN
      AK=A(K2)
      BK=B(K2)
      A(K2)=A(KK)-AK
      B(K2)=B(KK)-BK
      A(KK)=A(KK)+AK
      B(KK)=B(KK)+BK
      KK=K2+KSPAN
      IF(KK .LE. NN) GO TO 210
      KK=KK-NN
      IF(KK .LE. JC) GO TO 210
      IF(KK .GT. KSPAN) GO TO 800
  220 C1=1.0-CD
      S1=SD
  230 K2=KK+KSPAN
      AK=A(KK)-A(K2)
      BK=B(KK)-B(K2)
      A(KK)=A(KK)+A(K2)
      B(KK)=B(KK)+B(K2)
      A(K2)=C1*AK-S1*BK
      B(K2)=S1*AK+C1*BK
      KK=K2+KSPAN
      IF(KK .LT. NT) GO TO 230
      K2=KK-NT
      C1=-C1
      KK=K1-K2
      IF(KK .GT. K2) GO TO 230
      AK=C1-(CD*C1+SD*S1)
      S1=(SD*C1-CD*S1)+S1
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
C    ERROR.  IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE
C     C1=AK
      C1=0.5/(AK**2+S1**2)+0.5
      S1=C1*S1
      C1=C1*AK
      KK=KK+JC
      IF(KK .LT. K2) GO TO 230
      K1=K1+INC+INC
      KK=(K1-KSPAN)/2+JC
      IF(KK .LE. JC+JC) GO TO 220
      GO TO 100
C  TRANSFORM FOR FACTOR OF 3 (OPTIONAL CODE)
  320 K1=KK+KSPAN
      K2=K1+KSPAN
      AK=A(KK)
      BK=B(KK)
      AJ=A(K1)+A(K2)
      BJ=B(K1)+B(K2)
      A(KK)=AK+AJ
      B(KK)=BK+BJ
      AK=-0.5*AJ+AK
      BK=-0.5*BJ+BK
      AJ=(A(K1)-A(K2))*S120
      BJ=(B(K1)-B(K2))*S120
      A(K1)=AK-BJ
      B(K1)=BK+AJ
      A(K2)=AK+BJ
      B(K2)=BK-AJ
      KK=K2+KSPAN
      IF(KK .LT. NN) GO TO 320
      KK=KK-NN
      IF(KK .LE. KSPAN) GO TO 320
      GO TO 700
C  TRANSFORM FOR FACTOR OF 4
  400 IF(NFAC(I) .NE. 4) GO TO 600
      KSPNN=KSPAN
      KSPAN=KSPAN/4
  410 C1=1.0
      S1=0
  420 K1=KK+KSPAN
      K2=K1+KSPAN
      K3=K2+KSPAN
      AKP=A(KK)+A(K2)
      AKM=A(KK)-A(K2)
      AJP=A(K1)+A(K3)
      AJM=A(K1)-A(K3)
      A(KK)=AKP+AJP
      AJP=AKP-AJP
      BKP=B(KK)+B(K2)
      BKM=B(KK)-B(K2)
      BJP=B(K1)+B(K3)
      BJM=B(K1)-B(K3)
      B(KK)=BKP+BJP
      BJP=BKP-BJP
      IF(ISN .LT. 0) GO TO 450
      AKP=AKM-BJM
      AKM=AKM+BJM
      BKP=BKM+AJM
      BKM=BKM-AJM
      IF(S1 .EQ. 0.0) GO TO 460
  430 A(K1)=AKP*C1-BKP*S1
      B(K1)=AKP*S1+BKP*C1
      A(K2)=AJP*C2-BJP*S2
      B(K2)=AJP*S2+BJP*C2
      A(K3)=AKM*C3-BKM*S3
      B(K3)=AKM*S3+BKM*C3
      KK=K3+KSPAN
      IF(KK .LE. NT) GO TO 420
  440 C2=C1-(CD*C1+SD*S1)
      S1=(SD*C1-CD*S1)+S1
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
C    ERROR.  IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE
C     C1=C2
      C1=0.5/(C2**2+S1**2)+0.5
      S1=C1*S1
      C1=C1*C2
      C2=C1**2-S1**2
      S2=2.0*C1*S1
      C3=C2*C1-S2*S1
      S3=C2*S1+S2*C1
      KK=KK-NT+JC
      IF(KK .LE. KSPAN) GO TO 420
      KK=KK-KSPAN+INC
      IF(KK .LE. JC) GO TO 410
      IF(KSPAN .EQ. JC) GO TO 800
      GO TO 100
  450 AKP=AKM+BJM
      AKM=AKM-BJM
      BKP=BKM-AJM
      BKM=BKM+AJM
      IF(S1 .NE. 0.0) GO TO 430
  460 A(K1)=AKP
      B(K1)=BKP
      A(K2)=AJP
      B(K2)=BJP
      A(K3)=AKM
      B(K3)=BKM
      KK=K3+KSPAN
      IF(KK .LE. NT) GO TO 420
      GO TO 440
C  TRANSFORM FOR FACTOR OF 5 (OPTIONAL CODE)
  510 C2=C72**2-S72**2
      S2=2.0*C72*S72
  520 K1=KK+KSPAN
      K2=K1+KSPAN
      K3=K2+KSPAN
      K4=K3+KSPAN
      AKP=A(K1)+A(K4)
      AKM=A(K1)-A(K4)
      BKP=B(K1)+B(K4)
      BKM=B(K1)-B(K4)
      AJP=A(K2)+A(K3)
      AJM=A(K2)-A(K3)
      BJP=B(K2)+B(K3)
      BJM=B(K2)-B(K3)
      AA=A(KK)
      BB=B(KK)
      A(KK)=AA+AKP+AJP
      B(KK)=BB+BKP+BJP
      AK=AKP*C72+AJP*C2+AA
      BK=BKP*C72+BJP*C2+BB
      AJ=AKM*S72+AJM*S2
      BJ=BKM*S72+BJM*S2
      A(K1)=AK-BJ
      A(K4)=AK+BJ
      B(K1)=BK+AJ
      B(K4)=BK-AJ
      AK=AKP*C2+AJP*C72+AA
      BK=BKP*C2+BJP*C72+BB
      AJ=AKM*S2-AJM*S72
      BJ=BKM*S2-BJM*S72
      A(K2)=AK-BJ
      A(K3)=AK+BJ
      B(K2)=BK+AJ
      B(K3)=BK-AJ
      KK=K4+KSPAN
      IF(KK .LT. NN) GO TO 520
      KK=KK-NN
      IF(KK .LE. KSPAN) GO TO 520
      GO TO 700
C  TRANSFORM FOR ODD FACTORS
  600 K=NFAC(I)
      KSPNN=KSPAN
      KSPAN=KSPAN/K
      IF(K .EQ. 3) GO TO 320
      IF(K .EQ. 5) GO TO 510
      IF(K .EQ. JF) GO TO 640
      JF=K
      S1=RAD/FLOAT(K)
      C1=COS(S1)
      S1=SIN(S1)
      IF(JF .GT. MAXF) RETURN 1
      CK(JF)=1.0
      SK(JF)=0.0
      J=1
  630 CK(J)=CK(K)*C1+SK(K)*S1
      SK(J)=CK(K)*S1-SK(K)*C1
      K=K-1
      CK(K)=CK(J)
      SK(K)=-SK(J)
      J=J+1
      IF(J .LT. K) GO TO 630
  640 K1=KK
      K2=KK+KSPNN
      AA=A(KK)
      BB=B(KK)
      AK=AA
      BK=BB
      J=1
      K1=K1+KSPAN
  650 K2=K2-KSPAN
      J=J+1
      AT(J)=A(K1)+A(K2)
      AK=AT(J)+AK
      BT(J)=B(K1)+B(K2)
      BK=BT(J)+BK
      J=J+1
      AT(J)=A(K1)-A(K2)
      BT(J)=B(K1)-B(K2)
      K1=K1+KSPAN
      IF(K1 .LT. K2) GO TO 650
      A(KK)=AK
      B(KK)=BK
      K1=KK
      K2=KK+KSPNN
      J=1
  660 K1=K1+KSPAN
      K2=K2-KSPAN
      JJ=J
      AK=AA
      BK=BB
      AJ=0.0
      BJ=0.0
      K=1
  670 K=K+1
      AK=AT(K)*CK(JJ)+AK
      BK=BT(K)*CK(JJ)+BK
      K=K+1
      AJ=AT(K)*SK(JJ)+AJ
      BJ=BT(K)*SK(JJ)+BJ
      JJ=JJ+J
      IF(JJ .GT. JF) JJ=JJ-JF
      IF(K .LT. JF) GO TO 670
      K=JF-J
      A(K1)=AK-BJ
      B(K1)=BK+AJ
      A(K2)=AK+BJ
      B(K2)=BK-AJ
      J=J+1
      IF(J .LT. K) GO TO 660
      KK=KK+KSPNN
      IF(KK .LE. NN) GO TO 640
      KK=KK-NN
      IF(KK .LE. KSPAN) GO TO 640
C  MULTIPLY BY ROTATION FACTOR (EXCEPT FOR FACTORS OF 2 AND 4)
  700 IF(I .EQ. M) GO TO 800
      KK=JC+1
  710 C2=1.0-CD
      S1=SD
  720 C1=C2
      S2=S1
      KK=KK+KSPAN
  730 AK=A(KK)
      A(KK)=C2*AK-S2*B(KK)
      B(KK)=S2*AK+C2*B(KK)
      KK=KK+KSPNN
      IF(KK .LE. NT) GO TO 730
      AK=S1*S2
      S2=S1*C2+C1*S2
      C2=C1*C2-AK
      KK=KK-NT+KSPAN
      IF(KK .LE. KSPNN) GO TO 730
      C2=C1-(CD*C1+SD*S1)
      S1=S1+(SD*C1-CD*S1)
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
C    ERROR.  IF ROUNDED ARITHMETIC IS USED, THEY MAY
C    BE DELETED.
      C1=0.5/(C2**2+S1**2)+0.5
      S1=C1*S1
      C2=C1*C2
      KK=KK-KSPNN+JC
      IF(KK .LE. KSPAN) GO TO 720
      KK=KK-KSPAN+JC+INC
      IF(KK .LE. JC+JC) GO TO 710
      GO TO 100
C  PERMUTE THE RESULTS TO NORMAL ORDER---DONE IN TWO STAGES
C  PERMUTATION FOR SQUARE FACTORS OF N
  800 NP(1)=KS
      IF(KT .EQ. 0) GO TO 890
      K=KT+KT+1
      IF(M .LT. K) K=K-1
      J=1
      NP(K+1)=JC
  810 NP(J+1)=NP(J)/NFAC(J)
      NP(K)=NP(K+1)*NFAC(J)
      J=J+1
      K=K-1
      IF(J .LT. K) GO TO 810
      K3=NP(K+1)
      KSPAN=NP(2)
      KK=JC+1
      K2=KSPAN+1
      J=1
      IF(N .NE. NTOT) GO TO 850
C  PERMUTATION FOR SINGLE-VARIATE TRANSFORM (OPTIONAL CODE)
  820 AK=A(KK)
      A(KK)=A(K2)
      A(K2)=AK
      BK=B(KK)
      B(KK)=B(K2)
      B(K2)=BK
      KK=KK+INC
      K2=KSPAN+K2
      IF(K2 .LT. KS) GO TO 820
  830 K2=K2-NP(J)
      J=J+1
      K2=NP(J+1)+K2
      IF(K2 .GT. NP(J)) GO TO 830
      J=1
  840 IF(KK .LT. K2) GO TO 820
      KK=KK+INC
      K2=KSPAN+K2
      IF(K2 .LT. KS) GO TO 840
      IF(KK .LT. KS) GO TO 830
      JC=K3
      GO TO 890
C  PERMUTATION FOR MULTIVARIATE TRANSFORM
  850 K=KK+JC
  860 AK=A(KK)
      A(KK)=A(K2)
      A(K2)=AK
      BK=B(KK)
      B(KK)=B(K2)
      B(K2)=BK
      KK=KK+INC
      K2=K2+INC
      IF(KK .LT. K) GO TO 860
      KK=KK+KS-JC
      K2=K2+KS-JC
      IF(KK .LT. NT) GO TO 850
      K2=K2-NT+KSPAN
      KK=KK-NT+JC
      IF(K2 .LT. KS) GO TO 850
  870 K2=K2-NP(J)
      J=J+1
      K2=NP(J+1)+K2
      IF(K2 .GT. NP(J)) GO TO 870
      J=1
  880 IF(KK .LT. K2) GO TO 850
      KK=KK+JC
      K2=KSPAN+K2
      IF(K2 .LT. KS) GO TO 880
      IF(KK .LT. KS) GO TO 870
      JC=K3
  890 IF(2*KT+1 .GE. M) RETURN
      KSPNN=NP(KT+1)
C  PERMUTATION FOR SQUARE-FREE FACTORS OF N
      J=M-KT
      NFAC(J+1)=1
  900 NFAC(J)=NFAC(J)*NFAC(J+1)
      J=J-1
      IF(J .NE. KT) GO TO 900
      KT=KT+1
      NN=NFAC(KT)-1
      IF(NN .GT. MAXP) RETURN 2
      JJ=0
      J=0
      GO TO 906
  902 JJ=JJ-K2
      K2=KK
      K=K+1
      KK=NFAC(K)
  904 JJ=KK+JJ
      IF(JJ .GE. K2) GO TO 902
      NP(J)=JJ
  906 K2=NFAC(KT)
      K=KT+1
      KK=NFAC(K)
      J=J+1
      IF(J .LE. NN) GO TO 904
C  DETERMINE THE PERMUTATION CYCLES OF LENGTH GREATER THAN 1
      J=0
      GO TO 914
  910 K=KK
      KK=NP(K)
      NP(K)=-KK
      IF(KK .NE. J) GO TO 910
      K3=KK
  914 J=J+1
      KK=NP(J)
      IF(KK .LT. 0) GO TO 914
      IF(KK .NE. J) GO TO 910
      NP(J)=-J
      IF(J .NE. NN) GO TO 914
      MAXF=INC*MAXF
C  REORDER A AND B, FOLLOWING THE PERMUTATION CYCLES
      GO TO 950
  924 J=J-1
      IF(NP(J) .LT. 0) GO TO 924
      JJ=JC
  926 KSPAN=JJ
      IF(JJ .GT. MAXF) KSPAN=MAXF
      JJ=JJ-KSPAN
      K=NP(J)
      KK=JC*K+II+JJ
      K1=KK+KSPAN
      K2=0
  928 K2=K2+1
      AT(K2)=A(K1)
      BT(K2)=B(K1)
      K1=K1-INC
      IF(K1 .NE. KK) GO TO 928
  932 K1=KK+KSPAN
      K2=K1-JC*(K+NP(K))
      K=-NP(K)
  936 A(K1)=A(K2)
      B(K1)=B(K2)
      K1=K1-INC
      K2=K2-INC
      IF(K1 .NE. KK) GO TO 936
      KK=K2
      IF(K .NE. J) GO TO 932
      K1=KK+KSPAN
      K2=0
  940 K2=K2+1
      A(K1)=AT(K2)
      B(K1)=BT(K2)
      K1=K1-INC
      IF(K1 .NE. KK) GO TO 940
      IF(JJ .NE. 0) GO TO 926
      IF(J .NE. 1) GO TO 924
  950 J=K3+1
      NT=NT-KSPNN
      II=NT-INC+1
      IF(NT .GE. 0) GO TO 924
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dfft.imake
/* Imake file for VICAR subroutine dfft */

#define SUBROUTINE dfft

#define MODULE_LIST dfft.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tdfft.f

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C-----THIS IS A TEST PROGRAM FOR MODULES  dfft AND REALTR
C-----THESE MODULES ARE USED TOGETHER SO ARE TESTED TOGETHER.
C-----A REAL*4 IMAGE IS INPUT (CREATED WITH GEN.
      DIMENSION A(16,18)

	M = 16
	N = 16
        CALL XVUNIT(IUNIT,'INP',1,STAT, ' ')
        CALL XVOPEN(IUNIT,STAT,' ')
	DO I=1,M
           CALL XVREAD(IUNIT,A(1,I),STAT,'LINE',I,'NSAMPS',N, ' ')
        END DO
	M2 = M/2
      IF ( 2*M2 .NE. M ) GO TO 930
C THAT BECAUSE THE DIMENSION MUST BE EVEN IN THE REAL TRANSFORM
C DIRECTION.
      MP2=M + 2
      N2 = 2*N
C
C FORWARD TRANSFORM..
	DO 110 I=1,M
110	CALL PRNT(7,N,A(1,I), 'BEFORE.')
C
      DO 120 I=1,N
      CALL dfft(A(I,1),A(I,2),M2,M2,M2,N2,&950,&960)
      CALL REALTR(A(I,1),A(I,2),M2,N2)
120   CONTINUE
      DO 140 I=2,MP2,2
      CALL dfft(A(1,I-1),A(1,I),N,N,N,1,&950,&960)
140   CONTINUE
	CALL XVMESSAGE( 'THE RESULTING TRANSFORM', ' ')
	DO 130 I=1,MP2
130	CALL PRNT(7,N,A(1,I), 'TRANSFORM.')
C
C THE INVERSE TRANSFORM..
C
      DO 220 I=2,MP2,2
      CALL dfft(A(1,I-1),A(1,I),N,N,N,-1,&950,&960)
220   CONTINUE
      DO 240 I=1,N
      CALL REALTR(A(I,1),A(I,2),M2,-N2)
      CALL dfft(A(I,1),A(I,2),M2,M2,M2,-N2,&950,&960)
240   CONTINUE
	CALL XVMESSAGE( 'REVERSE THE TRANSFORM', ' ')
	S = 2*M*N
	DO 160 I=1,M
        DO J = 1, N
           A(J,I) = A(J,I)/S
        END DO
160	CALL PRNT(7,N,A(1,I), 'AFTER.')
      CALL XVCLOSE(IUNIT,STAT, ' ')
      RETURN
C
930 	CALL XVMESSAGE( 'NUMBER OF LINES MUST BE EVEN', ' ')
	RETURN
C
950 	CALL XVMESSAGE( 'M OR N HAS TOO LARGE A PRIME FACTOR', ' ')
	RETURN
C
960	CALL XVMESSAGE( 
     +  'PRODUCT OF SQUARE-FREE FACTORS OF M OR N TOO BIG',' ')
	RETURN

      END
C************************* START PDF *************************
CPROCESS
CPARM 	INP	TYPE=STRING
CEND-PROC
C**************************** END PDF ***************************
$!-----------------------------------------------------------------------------
$ create tdfft.imake
/* Imake file for Test of VICAR subroutine dfft */

#define PROGRAM tdfft

#define MODULE_LIST tdfft.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tdfft.pdf
! pdf for test pgm for subroutine dfft
PROCESS
PARM 	INP	TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tstdfft.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!tdfft
!THIS IS A TEST OF MODULES   dfft  AND   REALTR
!THESE ROUTINES ARE SO RELATED THAT A SINGLE TEST MUST BE
!USED TO CHECK THEM.  
!
!dfft IS A GENERAL PURPOSE FFT ROUTINE   AND  REALTR  ALLOWS
!THE USE OF REAL NUMBERS INSTEAD OF COMPLEX ONES.
!THE TEST WILL DO ITS THING ON A 16x16 IMAGE.  
!FIRST, THE INPUT REAL*4 BUFFER IS PRINTED OUT.
!THEN, THE TRANSFORM IS PRINTED.
!LAST, THE TRANSFORM IS REVERSED USING THE SAME ROUTINES
!AND THE RESULT IS PRINTED.  THIS LAST RESULT SHOULD BE
!IDENTICAL WITH THE ORIGINAL INPUT BUFFER.
!
!A COMPARISON RUN DONE ON THE IBM HAS BEEN PLACED IN THE
!FILE OF EACH ROUTINE.
GEN A 16 64 'REAL4
tdfft A
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create dfft.hlp
1  dfft

         A general purpose Fast Fourier Transform routine
                                                
2  PURPOSE

     dfft is a general purpose Fast Fourier Transform routine offering
     the following features:

        * The number, n, of samples need not be a power of 2.  In fact,
          all but a very small percentage of values of n may be used.

        * Complex sample arrays of any number of subscripts may be 
          transformed.  dfft is called once per subcript.

        * The transform is done in place.

     REALTR, used in conjunction with dfft, provides a fast transform
     capability in cases involving a one-dimensional array of real
     samples.  Compared with complex samples, this saves nearly a factor
     of two in memory space and also saves computer time.

2  CALLING SEQUENCE

	Note: Due to the nature of this subroutine's call interface, the
	      last two parameters, it is not recomended for use with the
	      C language.

   A) K-DIMENSIONAL COMPLEX TRANSFORM

        One call is made for each variate (subscript).  The calls may be
        made in any order.  For each variate, the call is of the form:

                  CALL dfft(A,B,NTOT,N,NSPAN,ISN,&P,&Q)

        ARGUMENTS

           A         is a REAL array containing the real components
                     of the data or Fourier coefficients.

           B         is a REAL array containing the imaginary components
                     ok the data or Fourier coefficients.

           NTOT      is the total number of complex data values.

           N         is the dimiension of the current variate.

           NSPAN/N   is the spacing of consecutive data values while
                     indexing the current variate.

           ISN       is positive for a forward transform, negative
                     for inverse.

           ISN*NSPAN/N   is the spacing of consecutive data values.           

           &P        is a reference to a statement number in the caller's
                     program to which control is transferred if N has a 
                     prime factor exceeding 23.
                        
           &Q        is a reference to a statement number in the caller's
                     program to which control is transferred if the square-
                     free prrtion of N has two or more factors and exceed 
                     210 or if the number of prime factors exceeds 208.

        If A and B are independent arrays, ISN should be plus or minus 1 for
        the forward and inverse transforms, respectively.  If real and 
        imaginary parts are adjacent, A(2) should be substituted for B in 
        the call sequence and the magnitude of ISN should be 2.

   B) ONE-DIMENSIONAL REAL TRANSFORM

        Forward transform

                  CALL dfft(A,A(2),N,N,N,2,&P,&Q)
                  CALL REALTR(A,A(2),N,2)

        Inverse transform

                  CALL REALTR(A,A(2),N,-2)
                  CALL dfft(A,A(2),N,N,N,-2,&P,&Q)

        ARGUMENTS 


          A          is a REAL array of dimension 2*N+2 containing the 2*n 
                     real data values or the N+1 complex coefficients.

          N          is one-half the number (which must be even) of real 
                     data values.

          &P,&Q      are as before.

2  HISTORY

     Original Programmer: Richard C Singleton (Stanford Research Institute)
     Current Cognizant Programmer: John Kreznar
     Ported to Unix by Payam Zamani on 4-Mar-1993
     Source Language: Fortran
     Revision: New,6 April 1977 (Incorporated into VICAR from
                                 Stanford Research Institute)
     Reference: Singleton, Richard C., "An Algorithm for Computing the 
                Mixed Radix Fast Fourier Transform," IEEE Trans. Audio
                Electroacoustics AU-17, 2 (June 1969) 93-103.

2  OPERATION

     The fast Fourier transform has been thoroughly treated in the liter-
     ature and will not be reviewed here.  Suffice it to say that the 
     technique achieves computational efficiency by avoiding redundant
     computations which arise if the number, n, of samples is composite.
     The FFT offers no advantage if n is prime and large advantage if n
     is the product of many small factors.

     These programs were written by Richard C. Singleton of the Stanford 
     Research Institute.  His paper, referenced above, should be studied
     by all who have more than a superficial interest in this software.
     Much of this document is taken verbatim from his paper.

     REALTR is axactly as received from SRI, which SRI claims agrees with
     the paper.  dfft differs from what was received from SRI in the
     following repects only:

      * The name was changed from FFT.

      * The error conditions which now cause error returns used to cause
        the printing of an error message followed by termination of the
        job.

     TRANSFORMING COMPLEX DATA

     dfft computes either a single-variate complex Fourier transform
     or the calculation for one variate of a multivariate transform.

     To compute a single-variate transform
         
                        n-1
                       _____
                       \
          (alpha)(k) =  \    X(j)exp(2(PI)ijk/n)
                        /
                       /____
                        j=0

     of complex data values,

            CALL dfft(A,B,n,n,n,1,&p,&q)

     The "inverse" transform

                       n-1
                     ______
                     \
             x(j) =   \       (alpha)(k)exp(-2(PI)ijk/n)
                      /
                     /_____
                      k=0

    is computed by

           CALL dfft(A,B,n,n,n,-1,&p,&q).

    Scaling is left to the user.  The two calls in succession give 
    the transformation

           T*Tx=nx,

    i.e., n times the original values, except for round-off errors.
    The arrays A and B originally hold the ral and imaginary com-
    ponents of the data, indexed from 1 to n; the data values are 
    replaced by the complex Fourier coefficients.  Thus the real
    component of (alpha)(k) is found in A(k+1), and the imaginary
    component in B(k+1), for k=0,1...,n-1.

    The difference between the transform and inverse calculation is
    primarily one of changing the sign of a variable holding the value
    2(PI).  The one additional change is to follow an alternative
    path within the radix-4 sectin of the program, using the angle
    -(PI)/2 rather than (PI)/2.

    To compute a bivariate transform on the data stored in rectangular
    arrays A and B, the subroutine is called once to transform the
    columns and again to transform the rows.  A multivariate transform
    is essentially a single-variate transform with modified indexing.
    Multivariate data are indexed according to the Fortran array
    element successor function, without limit on the number of implied
    multiple subscripts.  The subroutine is called once for each 
    variate.  These calls may be in any order.

    Restrictions on n-values result from finite array sizes built into
    the program.  The subroutine as presented permits a maximum prime 
    factor of 23, using four arrays of this dimension.  The dimension
    of these arrays may be reduced to 1 if n contains no prime factors
    greater than 5.  An array NP(209) is used in permuting the results
    to normal order; the present value permits a maximum of 210 for the
    product of the square-free if it cannot be paired with another factor
    of p in n; i.e., each prime occurring an odd number of times in n is 
    a square-free factor, the dimension of this array can be reduced to
    j+1, where j is the maximum number of prime factors if n.  A sixth
    array NFAC(11) holds the factors of n.  This is ample for any transform
    that can be done on a computer with core storage for 2**17 REAL values
    (2**16 complex values);

                    52 488 = 2 x 3**4 x 2 x 3**4 x 2

    is the only number <2**16 with as many as 11 factors, given the 
    factoring used in this algorithm.  The existing array dimensions
    do not permit unrestricted choice of n, but they rule out only a small 
    percentage of the possible values.

    The transform portion of the subroutine includes sections for factors
    of 2, 3, 4 and 5, as well as general section for odd factors.  The
    sections for 2 and 4 include multiplication of each result value by 
    the rotation factor; combining the two steps gives bout a 10 percent 
    speed improvement over using the general rotation factor section in
    the program, due to reduced indexing.  The sections for 3 and 5 are
    similar to the general odd factors section, and they improve speed 
    substantially for these factors by reducing indexing operations.  
    The odd factors section is used for odd primes >5, burt can handle
    any odd factor.  The rotation factor section works for any factor
    but is used only for odd factors.

    The permutation for square factors of n contains special code for
    single-variate transforms, since less indexing is required.  However,
    the permutation for multivariate transforms also works on 
    single-variate transforms.

    TRANSFORMING REAL DATA

    A single-variate Fourier transform of 2n real data values can be
    computed by use of a complex Fourier transform of dimension n. Sub-
    routine REALTR provides this capability.

    The real data vlaues are stored alternately in the arrays A and B,

                    A(1),B(1),A(2),B(2),...A(n),B(n),

    then we

                    CALL dfft(A,B,n,n,n,1)
                    CALL REALTR(A,B,n,1)

    After scaling by 0.5/n, the results in A and B are the Fourier
    cosine and sine coefficients, i.e.,

                    a(k) = A(k+1)
                      
                    b(k) = B(k+1)

    for k=0,1,...,n,with b(0)=b(n)=0.  The inverse operation,

                    CALL REALTR(A,B,n,-1)
                    CALL dfft(A,B,n,n,n,-1),

    after scaling by 1/2, evaluates the Fourier series and leaves 
    the time domain values stored

                    A(1),B(1),A(2),B(2),...A(n),B(n),

    as originally.

    The subroutine REALTR, called with ISN=1*, separates the complex
    transforms of the even- and odd-numbered data values, using the
    fact that the transform of real data has the complex conjugate
    symmetry
                                              *
                     (alpha)(n-k) = (alpha)(k) 

               * ISN is the fourth (last) argument to REALTR

    for k=1,2,...,n-1, then performs a final radix-2 step to complete 
    the transform for the 2n real values.  If called with ISN = -1,
    the inverse operation is performed.  The pair of calls

                      CALL REALTR(A,B,n,1)
                      CALL REALTR(A,B,n,-1)

    return the original values multiplied by 4, except for round-off 
    errors.

    The data may alternatively be stored in a single complex array A, 
    then the magnitude of ISN changed to two to give the correct indexing
    increment and A(2) used to pass the initial address for the sequence
    of imaginary values, e.g.,

                       CALL dfft(A,A(2),n,n,n,2)
                       CALL REALTR(A,A(2),n,2)

    In this case, the cosine and sine coefficients alternate in A.

2  TIMING AND ACCURACY
   
    The subroutine dfft was tested for time and accuracy on the IPL 
    360/65 computer.  The results are shown in Table 1 below.  The times 
    areCPU times measured with 1/60 second resolution.

    The data used in the trials were random normal deviates with a mean of 
    zero and a standard deviation of one (i.e., an expected rms value of 
    one). The subroutine was called twice:

                        CALL dfft(A,B,n,n,n,1)
                        CALL dfft(A,B,n,n,n,-1);

    then the result was scaled by 1/n.  The square deviations from the 
    original data values were summed, the real and imaginary quantities
    separately, then divided by n and square roots taken to yield an rms
    error value.  The two values were in all cases comparable in magnitude,
    and an average is reported in Table 1.

    The measured times were normalized in two ways, first by dividing by

              m
              ____
           n  \    n(i),
              /___
              1-l

     and second by dividing by

              n log(2)(n).

     To a first approximation, computing time for the mixed radix FFT is
     proportional to n times the sum of the factors of n, and we observe
     in the present case that a proportionality constant of 21 (us) gives 
     a fair fit to this model.  On the basis of counting complex multi-
     plications, we would expect a decline in this proportionality constant
     with increasing radix; a decline is observed for odd primes >5.  
     Factors of 5 or less are of course favored by special coding in the 
     program.  The second normalized time value places all times on a 
     comparable scale, allowing one to assess the relative efficiency of 
     using values other than powers of 2.

     Singleton's paper provides a table listing numbers up to 100,000 
     containing no prime factor greater than 5 to aid the user in 
     selecting efficient values of n.

                                Table 1
                                                             
                              Time,   Time, (mu)s   Time, (mu)s      rms
     Factoring of n         Seconds   -----------   -----------     error
                                      n(sigma)n(i)  n log(2)n      x10(**-6)
     _______________________________________________________________________

      512 = 4**2x2x4**2       .180        19.5         39.1           6.2
     1024 = 4**2x4x4**2       .432        21.1         42.2           8.3
     2048 = 4**2x2x2x2x4**2   .842        18.7         37.4           9.3
     4096 = 4**3x4**3        1.784        18.1         36.3          14.7
     2187 = 3**3x3x3**3      1.343        29.2         55.4          10.8
     3125 = 5**2x5x5**2      1.713        21.9         47.2          15.7     
     2401 = 7**2x7**2        2.152        32.0         79.8          15.3
     1331 = 11x11x11         1.142        26.0         82.6          15.2
     2197 = 13x13x13         2.042        23.8         83.7          20.1
      289 = 17x17             .216        22.0         91.4           9.1
      361 = 19x19             .288        21.0         93.7          10.0
      529 = 23x23             .506        20.8        105.7          14.1
     1000 = 2x5x2x5x2x5       .473        22.5         47.5           9.5
     2000 = 4x5x5x5x4         .933        20.3         42.6          11.4
      210 = 2x3x5x7           .122        34.1         75.1           5.8
      216 = 2**3x3**3         .093        28.8         55.7           6.0
      221 = 13x17*             ---         ---          ---           ---
      202 = 2x101**            ---         ---          ---           ---

              * Fails because square-free part exceeds 210 and has two
                factors.

             ** Fails because a prime factor exceeds 23.

2  RESTRICTIONS
 
     dfft cannot be used for values of n containing prime factors >23 or
     for values of n having square-free portions >210 containing two or more
     factors or for values of n having more than 208 prime factors.  These 
     restrictions can be relaxed by increasing certain array sizes in the 
     program and recompiling.

     SIZE

         dfft uses 10132(10) bytes.  REALTR uses 1054(10) bytes.

2  EXAMPLE

     A tri-variate transform with real components in A(n1,n2,n3) and
     imaginary components in B(n1,n2,n3) is computed by

                  CALL dfft(A,B,n1*n2*n3,n1,n1,1)
                  CALL dfft(A,B,n1*n2*n3,n2,n1*n2,1)
                  CALL dfft(A,B,n1*n2*n3,n3,n1*n2*n3,1).


$ Return
$!#############################################################################
