$!****************************************************************************
$!
$! Build proc for MIPL module locus2
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:23:41
$!
$! Execute by entering:		$ @locus2
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
$ write sys$output "*** module locus2 ***"
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
$ write sys$output "Invalid argument given to locus2.com file -- ", primary
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
$   if F$SEARCH("locus2.imake") .nes. ""
$   then
$      vimake locus2
$      purge locus2.bld
$   else
$      if F$SEARCH("locus2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake locus2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @locus2.bld "STD"
$   else
$      @locus2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create locus2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack locus2.com -mixed -
	-s locus2.f -
	-i locus2.imake -
	-p locus2.pdf -
	-t tstlocus2.pdf tstlocus2.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create locus2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C  REVISION HISTORY
c  10-Jan-2013 -lwk- fixed long continuation lines for new compiler flag on Solaris
C     6-97 ...RRD... MADE PORTABLE FOR UNIX
C     1-97 ...CCA... UPDATED HELP, added output of Unc Residual to 
C                    TCL variable
C     1-96 ...CCA... MADE GENGRID INC REAL, ADDED OUTPUT OF RADIAL
C                    COMPONENT OF VECTORS IN RADIAL TABLE
C    12-93 ...CCA... SIMPLIFIED LOCUS AS LOCUS2
C     2-73  LOCUS WRITTEN BY JOHN KREZNAR


        INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44 
        INCLUDE 'pgminc'
	REAL*4 Y(2,7200)
	integer*4 parb(500)
C
C-------OPEN INPUT COORDINATE DATA SETS
C-------INPUT FILES ARE COORDINATE DATA SETS IN 'MARK' FORMAT
C-------THIS FORMAT IS MERELY PAIRS OF REAL*4 VALUES (L1,S1,L2,S2,...)
C-------IN ONE RECORD.  EACH FILE MUST HAVE THE SAME NUMBER OF POINTS.
C
	CALL XVUNIT(IURAW,'INP',1,IST,' ')
	CALL XVOPEN(IURAW,IST,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
	CALL XVPCNT('INP',INPUTS)

C-------GET GRID SIZE FROM INPUT LABEL - GRIDLOCB
	CALL XLGET(IURAW,'HISTORY','GRID_NROW',NROWI,IST,'HIST',
     1                'GRIDLOCB','INSTANCE',1,'FORMAT','INT',' ')
	  IF (IST .NE. 1)
     1	  CALL XLGET(IURAW,'HISTORY','GRID_NROW',NROWI,IST,'HIST',
     2                'GRIDGEN','INSTANCE',1,'FORMAT','INT',' ')
	IF (IST .NE. 1) 
     1    CALL XVMESSAGE('GRID_NROW NOT FOUND IN INPUT',' ')

	CALL XLGET(IURAW,'HISTORY','GRID_NCOL',NCOLI,IST,'HIST',
     1                'GRIDLOCB','INSTANCE',1,'FORMAT','INT',' ')
	  IF (IST .NE. 1) 
     1	  CALL XLGET(IURAW,'HISTORY','GRID_NCOL',NCOLI,IST,'HIST',
     2                'GRIDGEN','INSTANCE',1,'FORMAT','INT',' ')
	IF (IST .NE. 1) 
     1    CALL XVMESSAGE('GRID_NCOL NOT FOUND IN INPUT',' ')

	IF (INPUTS .EQ. 2) THEN			!INPUT REFERENCE
	  CALL XVUNIT(IUREF,'INP',2,IST,' ')	
	  CALL XVOPEN(IUREF,IST,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C---------GET GRID SIZE FROM REFERENCE LABEL EITHER GRIDGEN OR RADDIST
	  CALL XLGET(IUREF,'HISTORY','GRID_NROW',NROWR,IST,'HIST',
     1                'GRIDGEN','INSTANCE',1,'FORMAT','INT',' ')
	  IF (IST .NE. 1) 
     1	  CALL XLGET(IUREF,'HISTORY','GRID_NROW',NROWR,IST,'HIST',
     2                'RADDIST','INSTANCE',1,'FORMAT','INT',' ')
	  IF (IST .NE. 1) CALL XVMESSAGE(
     +'KEYWORD GRID_NROW NOT FOUND IN REF',' ')

	  CALL XLGET(IUREF,'HISTORY','GRID_NCOL',NCOLR,IST,'HIST',
     1                'GRIDGEN','INSTANCE',1,'FORMAT','INT',' ')
	  IF (IST .NE. 1) 
     1	  CALL XLGET(IUREF,'HISTORY','GRID_NCOL',NCOLR,IST,'HIST',
     2                'RADDIST','INSTANCE',1,'FORMAT','INT',' ')
	  IF (IST .NE. 1) CALL XVMESSAGE(
     +'KEYWORD GRID_NCOL NOT FOUND IN REF',' ')

	END IF
C
C-------COMPARE INPUT AND REFERENCE LABEL GRID SIZES
	IF (NROWI .EQ. NROWR .AND. NCOLI .EQ. NCOLR) THEN
		NROW=NROWI
		NCOL=NCOLI
	ELSE IF (NROWI .EQ. 0) THEN  !NO INPUT LABEL
		NROW=NROWR
		NCOL=NCOLR
	ELSE IF (NROWR .EQ. 0) THEN   !NO REF LABEL
		NROW=NROWI
		NCOL=NCOLI
	END IF
C
C-------OVERRIDE WITH GRID SIZE FROM PARAMETERS
	CALL XVPARM('NROW',NR,ICNTR,IDEF,0)
	IF (ICNTR .EQ. 1) NROW=NR
	CALL XVPARM('NCOL',NC,ICNTC,IDEF,0)
	IF (ICNTC .EQ. 1) NCOL=NC
C
	NPTSLAB = NROW*NCOL
	IF (NCOL .GT. 100) GO TO 995
	IF (NPTSLAB .EQ. 0) GO TO 994
C
C-------CHECK SIZE VERSUS NUMBER OF SAMPLES
	CALL XVGET(IURAW,IST,'NS',NS1,' ')
	IF (INPUTS .EQ. 2) THEN
		CALL XVGET(IUREF,IST,'NS',NS2,' ')
		IF (NS1 .NE. NS2) GO TO 999
		IF (NPTSLAB .NE. NS2/2) GO TO 997
	END IF
	IF (NPTSLAB .NE. NS1/2) GO TO 996
	IF (NPTSLAB .GT. 900) GO TO 998
	NPOINTS = NPTSLAB
C
C-------READ INPUT FILES
	CALL RDPTS(Y,NPOINTS,NROW,NCOL,IURAW,IUREF,INPUTS)
C
	CALL XVCLOSE(IURAW,IST,' ')
	IF (INPUTS .EQ. 2) CALL XVCLOSE(IUREF,IST,' ')
C
C-------PROCESS USING ALL TRANSFORMATIONS, WITH :
C-------NPOINTS POINTS, 2 FRAMES, Y BUFFER (HOLDING INPUT AND OUTPUT
C-------DATA), RAW DATA SET IS 1ST SET OF POINTS IN Y AND REF DATA
C-------SET IS THE 2ND SET OF POINTS IN Y.
C-------UPON OUTPUT Y(1,1,N) WILL HOLD:
C-------   N=1   RAW DATA
C-------   N=2   REF DATA
C-------   N=3   UNTRANSFORMED RAW DATA
C-------   N=4   OFFSET TRANSFORM OF RAW DATA
C-------   N=5   ROTATION AND OFFSET OF RAW DATA
C-------   N=6   SCALE AND OFFSET OF RAW DATA
C-------   N=7   SCALE, ROTATION AND OFFSET OF RAW DATA
C-------   N=8   UNCONSTRAINED TRANSFORMATION OF RAW DATA
C
	CALL LSTIX(NPOINTS,2,Y,1,2,UNCR)
C
C-------PRODUCE THE OUTPUT FILES:
C-------LOCUS2.SROL = (N=7 TRANSFORMATION) - REF  FOR LINE COORDS
C-------LOCUS2.SROS = (N=7 TRANSFORMATION) - REF  FOR SAMP COORDS
C-------LOCUS2.SROM = MAGNITUDE OF DIFF  SQRT(7L**2+7S**2)
C-------LOCUS2.UNCL = (N=8 TRANSFORMATION) - REF  FOR LINE COORDS
C-------LOCUS2.UNCS = (N=8 TRANSFORMATION) - REF  FOR SAMP COORDS
C-------LOCUS2.UNCM = MAGNITUDE OF DIFF  SQRT(8L**2+8S**2)
C-------TABLE OF RADIAL DIST FROM CENTER VS. SRO & UNC MAGN & COMPONENTS

	CALL DIFFS(Y,NPOINTS,NROW,NCOL,*993)

	call xqini(parb,500,xabort)
	call xqreal(parb,'UNCRES',1,UNCR,xadd,ist)
	call xvqout(parb,ist)

	RETURN
994	CALL XVMESSAGE('NEED NROW,NCOL FROM LABEL OR PARMS',' ')
	CALL ABEND
995	CALL XVMESSAGE('NUMBER OF COLUMNS IN GRID CANT EXCEED 100',' ')
	CALL ABEND
996	CALL XVMESSAGE('GRID SIZE NOT CONSISTENT WITH',' ')
	CALL XVMESSAGE('NUMBER OF SAMPLES IN RAW FILE',' ')
	CALL ABEND
997	CALL XVMESSAGE('GRID SIZE NOT CONSISTENT WITH',' ')
	CALL XVMESSAGE('NUMBER OF SAMPLES IN REF FILE',' ')
	CALL ABEND
998	CALL XVMESSAGE('NUMBER OF COORDINATES MUST NOT EXCEED 900',' ')
	CALL ABEND
999	CALL XVMESSAGE(
     +'FILES MUST CONTAIN THE SAME NUMBER OF POINTS',' ')
993	CALL ABEND
	END
C
	SUBROUTINE RDPTS(Y,NPTS,NROW,NCOL,IURAW,IUREF,INPUTS)
	REAL*4 Y(2,NPTS,*),INC,GSL,GSS
C
C-------IF LOCUS2 NEEDS TO MAKE A PERFECT GRID (# INPUTS=1)
C-------TRY TO DETERMINE THE GRID SPACING IN THE RAW DATA, BUT
C-------DEFAULT IT TO 10 IF NECESSARY.
C
	CALL XVREAD(IURAW,Y(1,1,1),IST,' ')		!READ RAW POINTS
        IF (INPUTS .EQ. 2) THEN
		CALL XVREAD(IUREF,Y(1,1,2),IST,' ')  !READ REF POINTS IF ANY
C		CALL PGRID(Y(1,1,2),NROW,NCOL,DUM,0)
	ELSE
		INC = 10.        	!DEFAULTS TO 10
		M = NPTS/2 + 1       	!FIND GOOD POINTS NEAR MIDDLE
		DO 10 I=1,M-1
		N = M+I-1
		IF (Y(2,N+1,1) .GT. Y(2,N,1)) THEN
			INC = Y(2,N+1,1) - Y(2,N,1)	!INCREMENT
			GO TO 11
		END IF
10		CONTINUE
C
11		GSL = INC			!START LINE
		GSS = INC			!START SAMP
		CALL XVMESSAGE('COMPARING TO PERFECT GRID WITH',' ')
		CALL PRNT(7,1,GSL,' SL  = .')
		CALL PRNT(7,1,GSS,' SS  = .')
		CALL PRNT(7,1,INC,' INC = .')
		CALL GENGRID(Y(1,1,2),NROW,NCOL,GSL,GSS,INC)
C		CALL PGRID(Y(1,1,2),NROW,NCOL,DUM,0)
	END IF
C
	RETURN
	END
C
      SUBROUTINE GENGRID(OBUF,NROW,NCOL,GSL,GSS,INC)
      IMPLICIT INTEGER(A-Z)
      REAL*4 OBUF(1800),INC,GSL,GSS,GSS0
 
      GSS0 = GSS
      K = 1

      DO J=1,NROW		!ROWS
          DO I=1,NCOL   	!COLUMNS
              OBUF(K) = GSL
              OBUF(K+1) = GSS
              K = K + 2
              GSS = GSS + INC
          ENDDO
          GSS = GSS0
          GSL = GSL + INC
      ENDDO
      RETURN
      END
C
      SUBROUTINE LSTIX(NRES,NFRM,Y,J,M,R)
      REAL*4 Y(2,NRES,*)
      REAL*8 UI,VI,XI,YI,XIUI,YIUI,XIVI,XIXI,YIVI,YIYI,XIYI,DMLT,
     .       SIN1,COS1
      REAL*8 D,A(36),H(6),T(6)
      EQUIVALENCE(T(1),COS1),(T(2),SIN1),(T(4),D)
C
C-----Returning the Unconstrained residual R to calling program
C-----for output to local variable 1-16-97
c
      CALL QPRINT('1 LOCUS COORDINATE DETRENDING STATISTICS',40)
      CALL QPRINT(
     &'0 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS',
     & 111)
      CALL QPRINT(
     &'   IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION',
     & 90)
      UI=0.D0
      VI=0.D0
      NRET=0
C
      DO 22 I=1,NRES
      DO 21 K=1,NFRM
      IF(Y(1,I,K).EQ.-99) GO TO 22
   21 CONTINUE
      NRET=NRET+1
      UI=UI+Y(1,I,M)
      VI=VI+Y(2,I,M)
   22 CONTINUE
C
      XI=0.D0
      YI=0.D0
      XIUI=0.D0
      XIVI=0.D0
      YIUI=0.D0
      YIVI=0.D0
      XIXI=0.D0
      YIYI=0.D0
      XIYI=0.D0
      DO 25 I=1,NRES
      DO 24 K=1,NFRM
      IF(Y(1,I,K).EQ.-99) GO TO 25
   24 CONTINUE
      XI=XI+Y(1,I,J)
      YI=YI+Y(2,I,J)
      XIUI=XIUI+DMLT(Y(1,I,J),Y(1,I,M))
      XIVI=XIVI+DMLT(Y(1,I,J),Y(2,I,M))
      YIUI=YIUI+DMLT(Y(2,I,J),Y(1,I,M))
      YIVI=YIVI+DMLT(Y(2,I,J),Y(2,I,M))
      XIXI=XIXI+DMLT(Y(1,I,J),Y(1,I,J))
      YIYI=YIYI+DMLT(Y(2,I,J),Y(2,I,J))
      XIYI=XIYI+DMLT(Y(1,I,J),Y(2,I,J))
   25 CONTINUE
      T(1)=1.D0
      T(2)=0.D0
      T(3)=0.D0
      T(4)=1.D0
      T(5)=0.D0
      T(6)=0.D0
      R=RMS(NRES,Y,J,M,3,T)
      CALL TOPT('NONE                        ',T,R)
C OFFSET ONLY..
      T(5)=(UI-XI)/NRET
      T(6)=(VI-YI)/NRET
      R=RMS(NRES,Y,J,M,4,T)
      CALL TOPT('OFFSET ONLY                 ',T,R)
C OFFSET ROTATE..
      COS1= NRET*(XIUI+YIVI)-XI*UI-YI*VI
      SIN1=-NRET*(XIVI-YIUI)-YI*UI+XI*VI
      D=DSQRT(SIN1**2+COS1**2)
      SIN1=SIN1/D
      COS1=COS1/D
      T(3)=-SIN1
      T(4)=COS1
      T(5)=(UI-XI*COS1-YI*SIN1)/NRET
      T(6)=(VI+XI*SIN1-YI*COS1)/NRET
      R=RMS(NRES,Y,J,M,5,T)
      CALL TOPT('ROTATE, OFFSET              ',T,R)
C OFFSET MAGNIFY
      A(1)=NRET
      A(2)=0.D0
      A(3)=XI
      A(4)=0.D0
      A(5)=A(1)
      A(6)=YI
      A(7)=XI
      A(8)=YI
      A(9)=XIXI+YIYI
      H(1)=UI
      H(2)=VI
      H(3)=XIUI+YIVI

      CALL DGELG( H, A, 3, 1, EPS, IFAIL)  ! SOLVE SYSTEM OF LINEAR EQUATIONS.
      IF  (IFAIL .NE. 0)   GOTO 6000       ! CHECK FOR ERROR

      T(1)=H(3)
      T(2)=0.D0
      T(3)=0.D0
      T(4)=H(3)
      T(5)=H(1)
      T(6)=H(2)
      R=RMS(NRES,Y,J,M,6,T)
      CALL TOPT('MAGNIFY, OFFSET             ',T,R)
C OFFSET ROTATE MAGNIFY
      A(1)=NRET
      A(2)=0.D0
      A(3)=XI
      A(4)=YI
      A(5)=0.D0
      A(6)=A(1)
      A(7)=YI
      A(8)=-XI
      A(9)=XI
      A(10)=YI
      A(11)=XIXI+YIYI
      A(12)=0.D0
      A(13)=YI
      A(14)=-XI
      A(15)=0.D0
      A(16)=A(11)
      H(1)=UI
      H(2)=VI
      H(3)=XIUI+YIVI
      H(4)=YIUI-XIVI

      CALL DGELG( H, A, 4, 1, EPS, IFAIL)  ! SOLVE SYSTEM OF LINEAR EQUATIONS.
      IF  (IFAIL .NE. 0)   GOTO 6000       ! CHECK FOR ERROR

      T(1)=H(3)
      T(2)=H(4)
      T(3)=-H(4)
      T(4)=H(3)
      T(5)=H(1)
      T(6)=H(2)
      R=RMS(NRES,Y,J,M,7,T)
      CALL TOPT('MAGNIFY, ROTATE, OFFSET     ',T,R)
C UNCONSTRAINED..
      A(1)=NRET
      A(2)=0.D0
      A(3)=XI
      A(4)=YI
      A(5)=0.D0
      A(6)=0.D0
      A(7)=0.D0
      A(8)=A(1)
      A(9)=0.D0
      A(10)=0.D0
      A(11)=XI
      A(12)=YI
      A(13)=XI
      A(14)=0.D0
      A(15)=XIXI
      A(16)=XIYI
      A(17)=0.D0
      A(18)=0.D0
      A(19)=YI
      A(20)=0.D0
      A(21)=XIYI
      A(22)=YIYI
      A(23)=0.D0
      A(24)=0.D0
      A(25)=0.D0
      A(26)=XI
      A(27)=0.D0
      A(28)=0.D0
      A(29)=XIXI
      A(30)=XIYI
      A(31)=0.D0
      A(32)=YI
      A(33)=0.D0
      A(34)=0.D0
      A(35)=XIYI
      A(36)=YIYI
      H(1)=UI
      H(2)=VI
      H(3)=XIUI
      H(4)=YIUI
      H(5)=XIVI
      H(6)=YIVI

      CALL DGELG( H, A, 6, 1, EPS, IFAIL)  ! SOLVE SYSTEM OF LINEAR EQUATIONS.
      IF  (IFAIL .NE. 0)   GOTO 6000       ! CHECK FOR ERROR

      T(1)=H(3)
      T(2)=H(4)
      T(3)=H(5)
      T(4)=H(6)
      T(5)=H(1)
      T(6)=H(2)
      R=RMS(NRES,Y,J,M,8,T)
      CALL TOPT('UNCONSTRAINED LINEAR, OFFSET',T,R)
      RETURN

6000  CONTINUE
      CALL XVMESSAGE('ERROR COMPUTING COORDINATE TRANSFORMATION',' ')
      CALL ABEND
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION DMLT(X,Y)
      REAL*4 X,Y
      REAL*8 U,V
      U=X
      V=Y
      DMLT=U*V
      RETURN
      END
C
      FUNCTION RMS(N,Y,J,M,K,T)
      REAL*4 Y(2,N,8)
      REAL*8 T(6),DRMS,U,V
C
      DRMS=0.D0
      L=0
C
	DO 2 I=1,N
	 IF(Y(1,I,M).EQ.-99.OR.Y(1,I,J).EQ.-99.) THEN
		Y(1,I,K)=-99.
		Y(2,I,K)=-99.
	 ELSE
		U=T(1)*Y(1,I,J)+T(2)*Y(2,I,J)+T(5)
		V=T(3)*Y(1,I,J)+T(4)*Y(2,I,J)+T(6)
		DRMS=DRMS+(U-Y(1,I,M))**2+(V-Y(2,I,M))**2
		L=L+1
		Y(1,I,K)=U
		Y(2,I,K)=V
	 END IF
2	CONTINUE
C
	IF(L.EQ.0) THEN
		RMS=0.
	ELSE
		RMS=DSQRT(DRMS/L)
	END IF

      RETURN
      END
C
      SUBROUTINE TOPT(NAME,T,R)
      REAL*8 T(6)
      CHARACTER*111 B
      CHARACTER*28 NAME
      CHARACTER Z0
      DATA Z0/'0'/
      B=' '
      DO 1 I=1,28
1     B(I+2:I+2) = NAME(I:I)
      WRITE (B(35:50),'(F16.6)') T(1)
      WRITE (B(51:66),'(F16.6)') T(2)
      WRITE (B(74:89),'(F16.6)') T(5)
      DO 2 I=90,111
2     B(I:I) = B(2:2)
      B(1:1) = Z0
      CALL QPRINT(B,111)
      WRITE (B(35:50),'(F16.6)') T(3)
      WRITE (B(51:66),'(F16.6)') T(4)
      WRITE (B(74:89),'(F16.6)') T(6)
      WRITE (B(96:111),'(F16.6)') R
      DO 3 I=1,30
3     B(I:I) = B(2:2)
      CALL QPRINT(B,111)
      RETURN
      END
C
	SUBROUTINE DIFFS(Y,NPOINTS,NROW,NCOL,*)
C
C-------ROUTINE TO PRODUCE THE OUTPUT FILES:
C-------= (N=7 TRANSFORMATION) - REF  FOR LINE COORDS
C-------= (N=7 TRANSFORMATION) - REF  FOR SAMP COORDS
C-------= MAGNITUDE OF DIFF  SQRT(7L**2+7S**2)
C-------= (N=8 TRANSFORMATION) - REF  FOR LINE COORDS
C-------= (N=8 TRANSFORMATION) - REF  FOR SAMP COORDS
C-------= MAGNITUDE OF DIFF  SQRT(8L**2+8S**2)
C-------= TABLE OF RADIAL DIST. VS. SRO MAG,RAD COMP,AZM COMP
C-------=                       AND UNC MAG,RAD COMP,AZM COMP
C-        rad dist = sqrt( (Y(1,i,1)-nl/2)**2 + (Y(2,i,1)-ns/2)**2 )
C-        mag sro  = sqrt( (Y(1,i,7)**2 + Y(2,i,7)**2 )
C-        mag unc  = sqrt( (Y(1,i,8)**2 + Y(2,i,8)**2 )
C
	REAL*4 Y(2,NPOINTS,8),DL(100),DS(100),MAG(100),CEN(2)
	INTEGER*4 PTR(900), PTR2(900), IP(900)
        CHARACTER*1 TAB
	REAL*4 DIS(900), MAGU(900), MAGS(900), DRS(900),DAS(900)
	REAL*4 DRU(900), DAU(900), ANGREF, ANGOBS, DIS2(900)
	CHARACTER*255 FILES(3),FILE
        CHARACTER*132 PBUF
C
 	 TAB = CHAR(9)
	 IREC = NCOL*13

	CALL XVPARM('SROFILES',FILES,ICNT,IDEF,0)

	IF (ICNT .EQ. 3) THEN
C--------MAKE SRO OUTPUT FILES 
	 FILE = FILES(1)
	 OPEN(70,FILE=FILE,STATUS='UNKNOWN',
     1           RECL=IREC,IOSTAT=J,ERR=991)
	 FILE = FILES(2)
	 OPEN(71,FILE=FILE,STATUS='UNKNOWN',
     1           RECL=IREC,IOSTAT=J,ERR=991)
	 FILE = FILES(3)
	 OPEN(72,FILE=FILE,STATUS='UNKNOWN',
     1           RECL=IREC,IOSTAT=J,ERR=991)

C-------CALC AND OUTPUT DELTAS AND MAGNITUDE FOR TRANSFORMATION 7
C-------Y(.,.,2) IS REFERENCE

	M = 0              !POINT COUNTER
	DO 11 N=1,NROW

	DO 10 I=1,NCOL
	M = M + 1
	DL(I) = Y(1,M,7) - Y(1,M,2)            !RESIDUAL IN L
	DS(I) = Y(2,M,7) - Y(2,M,2)            !RESIDUAL IN S
	MAG(I) = SQRT( DL(I)*DL(I) + DS(I)*DS(I) )        !MAGNITUDE
10	CONTINUE

	WRITE(70,1,ERR=991) (DL(J),TAB,J=1,NCOL)
	WRITE(71,1,ERR=991) (DS(J),TAB,J=1,NCOL)
11	WRITE(72,1,ERR=991) (MAG(J),TAB,J=1,NCOL)

	 CLOSE(70)
	 CLOSE(71)
	 CLOSE(72)
	END IF

C-----------------------------------------------------------------
	CALL XVPARM('UNCFILES',FILES,ICNT,IDEF,0)

	IF (ICNT .EQ. 3) THEN
C--------MAKE UNC OUTPUT FILES 

	 FILE = FILES(1)
	 OPEN(80,FILE=FILE,STATUS='UNKNOWN',
     1           RECL=IREC,IOSTAT=J,ERR=991)
	 FILE = FILES(2)
	 OPEN(81,FILE=FILE,STATUS='UNKNOWN',
     1           RECL=IREC,IOSTAT=J,ERR=991)
	 FILE = FILES(3)
	 OPEN(82,FILE=FILE,STATUS='UNKNOWN',
     1           RECL=IREC,IOSTAT=J,ERR=991)

C-------CALC AND OUTPUT DELTAS AND MAGNITUDE FOR TRANSFORMATION 8
C-------Y(.,.,2) IS REFERENCE

	M = 0                    !POINT COUNTER
	DO 21 N=1,NROW

	DO 20 I=1,NCOL
	M = M + 1
	DL(I) = Y(1,M,8) - Y(1,M,2)
	DS(I) = Y(2,M,8) - Y(2,M,2)
20	MAG(I) = SQRT( DL(I)*DL(I) + DS(I)*DS(I) )

	WRITE(80,1,ERR=991) (DL(J),TAB,J=1,NCOL)
	WRITE(81,1,ERR=991) (DS(J),TAB,J=1,NCOL)
21	WRITE(82,1,ERR=991) (MAG(J),TAB,J=1,NCOL)

	 CLOSE(80)
	 CLOSE(81)
	 CLOSE(82)
	END IF

C----------------------------------------------------------
	CALL XVPARM('RADIAL',FILE,ICNT,IDEF,0)
	IF (ICNT .NE. 1) RETURN
	CALL XVPARM('CENTER',CEN,ICNT,IDEF,0)
	IF (ICNT .NE. 2) THEN
	  CALL XVMESSAGE(
     +'CENTER PARAMETER REQUIRED FOR RADIAL TABLE',' ')
	  RETURN 1
	END IF

C-------MAKE TABLE OF RADIAL DISTANCES VS. SRO MAG AND UNC MAG 
	IREC = 61
	OPEN(90,FILE=FILE,STATUS='UNKNOWN',
     1          RECL=IREC,IOSTAT=J,ERR=991)

	DO 30 M=1,NPOINTS
	XL = Y(1,M,2) - CEN(1)           !REF LINE - LINE OF CENTER
	XS = Y(2,M,2) - CEN(2)           !REF SAMP - SAMP OF CENTER
	DIS(M) = SQRT( XL*XL + XS*XS )   !DISTANCE FROM CENTER TO REF
	XLS = Y(1,M,7) - Y(1,M,2)        !SRO LINE - REF LINE
	XSS = Y(2,M,7) - Y(2,M,2)        !SRO SAMP - REF SAMP
	MAGS(M) = SQRT( XLS*XLS + XSS*XSS )
	XLU = Y(1,M,8) - Y(1,M,2)        !UNC LINE - REF LINE
	XSU = Y(2,M,8) - Y(2,M,2)        !UNC SAMP - REF SAMP
	MAGU(M) = SQRT( XLU*XLU + XSU*XSU )

	IF (DIS(M) .GT. 0.0) THEN
	 ANGREF = ATAN2(XL,XS)               !ANGLE TO RAW LOC FROM CENT

C-------DO COODINATE XFORMATION TO RADIUS-AZIMUTH
	 IF (MAGS(M) .NE. 0.0) THEN
	  ANGOBS = ATAN2(XLS,XSS)                  !ANGLE OF RESIDUAL VECTOR
	  DRS(M) = MAGS(M)*COS(ANGREF-ANGOBS)      !COMPONENT ALONG RADIUS
	  DAS(M) = MAGS(M)*SIN(ANGREF-ANGOBS)      !COMPONENT ALONG AZIMUTH    
	 ELSE
	  DRS(M) = 0.0
	  DAS(M) = 0.0
	 END IF

C-------DO COODINATE XFORMATION TO RADIUS-AZIMUTH
	 IF (MAGU(M) .NE. 0.0) THEN
	  ANGOBS = ATAN2(XLU,XSU)                  !ANGLE OF RESIDUAL VECTOR
	  DRU(M) = MAGU(M)*COS(ANGREF-ANGOBS)      !COMPONENT ALONG RADIUS
	  DAU(M) = MAGU(M)*SIN(ANGREF-ANGOBS)      !COMPONENT ALONG AZIMUTH    
	 ELSE
	  DRU(M) = 0.0
	  DAU(M) = 0.0
	 END IF

	ELSE                             !DIS(M) = 0.0
	 DRS(M) = MAGS(M)                !IF PT AT CENTER, VECTOR IS RADIAL
	 DRU(M) = MAGU(M)
	 DAS(M) = 0.0
	 DAU(M) = 0.0
	END IF

30	CONTINUE

C-------SORT DIS, MAGS, MAGU, DRS, DAS, DRU, DAU BY INCREASING DIS
        DO I=1,NPOINTS
         PTR(I) = I
        ENDDO

        CALL MVE(7,NPOINTS,DIS,DIS2,1,1)
        CALL MVE(7,NPOINTS,PTR,PTR2,1,1)
        CALL SSORTP(DIS,1,NPOINTS,IP)
        DO K = 1,NPOINTS
           DIS(K) = DIS2(IP(K))
           PTR(K) = PTR2(IP(K))
        ENDDO

C-------WRITE OUT DIS,MAGS,DRS,DAS, MAGU,DRU,DAU - ONE RECORD FOR EACH POINT
C-------K WILL POINT TO THE NEXT SORTED ONE FOR MAGS AND MAGU
	DO 40 J=1,NPOINTS      
	K = PTR(J)
40	WRITE(90,4) DIS(J),TAB,MAGS(K),TAB,DRS(K),TAB,DAS(K),
     1              TAB,MAGU(K),TAB,DRU(K),TAB,DAU(K)

	RETURN

1	FORMAT(100(F12.4,A1))
4	FORMAT(F7.2,A1,F8.4,A1,F8.4,A1,F8.4,A1,F8.4,A1,F8.4,A1,F8.4)
991	CALL XVMESSAGE('ERROR OPENING OUTPUT FILE',' ')
	WRITE(PBUF,2) J
        CALL XVMESSAGE(PBUF,' ')
2	FORMAT(' IOSTAT=',I5)
	WRITE(PBUF,3) FILE
        CALL XVMESSAGE(PBUF,' ')
3	FORMAT(' FILE= ',A9)
	CALL ABEND
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create locus2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM skew

   To Create the build file give the command:

		$ vimake skew			(VMS)
   or
		% vimake skew			(Unix)


************************************************************************/


#define PROGRAM	locus2
#define R2LIB

#define MODULE_LIST locus2.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define FTNINC_LIST pgminc
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create locus2.pdf
process help=*
LOCAL DMY1      REAL
PARM INP        STRING   COUNT=1:2
PARM SROFILES   STRING  COUNT=(0,3) DEFAULT=--
PARM UNCFILES   STRING  COUNT=(0,3) DEFAULT=--
PARM RADIAL     STRING  COUNT=0:1 DEFAULT=--
PARM CENTER     REAL    COUNT=0:2 DEFAULT=--
PARM NROW	INTEGER COUNT=0:1 DEFAULT=--
PARM NCOL	INTEGER COUNT=0:1 VALID=1:100 DEFAULT=--
PARM UNCRES     TYPE=NAME DEFAULT=DMY1
END-PROC
.TITLE
VICAR Program LOCUS2
.HELP

LOCUS2 is a VICAR applications program which transforms one set of 
MARK-format coordinates to match another.  Six fits are calculated with
the quality reported.  Two of the fits are used to produce output
text files for illustrating the spatial variation of the fit.

INPUT FORMAT
The one or two input files contain sets of coordinates:  the first is 
the set to be transformed and fit to the optional second (or reference) set; 
Each input file is assumed to contain one set of coordinates.  Each 
input file should be in VICAR REAL format, one line (record) of length 
2*n words, where n is the number of points in each set of coordinates.  
The standard VICAR SIZE parameter is not used in LOCUS2 because the input
files are not image files.

The second input is optional.  If it is not supplied, LOCUS2 will 
constuct a perfect grid to use as the reference grid (based upon the NROW and 
NCOL parameters).  

If the second input is supplied, LOCUS2 will look for
the grid size among the keywords in its label (GRID_NROW and GRID_NCOL).

The first input will normally have been generated by INTERLOC, GRIDLOCB or
FIXLOC.

EXECUTION

	locus2 (raw,ref) parms

OPERATION
LOCUS2 first check that the number of points in each file are identical.
It also checks that the grid size specified in the second input label 
(or by parameters) implies the number of samples in the input file records.

The program then does a best fit transformtion of the Raw data (first input)
to the Reference data (a perfect grid or the second input).  Six fits are 
performed with the transformed coordinates stored in the array for later use.
The six fits are:
	1. No change
	2. Offset
	3. Rotation and offset
	4. Scale and offset
	5. Scale, rotation and offset
	6. Unconstrained

Quality information is printed for all fits.

OUTPUT FILE FORMAT
Fits 5 and 6 produce output files which contain the residuals after the
fits to the reference. Three files are needed for each fit.  One file 
contains the residuals in Line, one the residuals in Samp and one the 
Magnitude of the residuals (sqrt(dl*dl+ds*ds)).

The output files are ASCII text files and therefore are not VICAR files.
The filenames are entered via the SROFILES (fit 5) and UNCFILES (fit 6).
The files are written as TAB-delimitted values (F8.4) with the number 
values per record being the number of grid columns and the number of 
records being the number of grid rows.

Format of output files: 
where residual value is denoted by res(i,j) (with i=row num., j=col num.)

	res(1,1)	res(1,2)	res(1,3)  ...   res(1,ncol)
	res(2,1)	res(2,2)	res(2,3)  ...   res(2,ncol)
            .
            .
            .
	res(nrow,1)	res(nrow,2)	res(nrow,3) ... res(nrow,ncol)

Example ASCII output:

	  6.2145	  5.5742	  5.0520  ...     6.6877	  
	  5.5688	  4.8335	  4.2088  ...     5.7503	  
            .
            .
            .
	  6.6733	  6.7403	  5.8532  ...     6.0715	  

In addition, the RADIAL parameter allows the generation of an output file
containing the magnitude and components of the residuals for fits 5 and 6 
sorted by distance from the center of the image.  The CENTER parameter 
is required if the RADIAL parameter is used.  The file is written as 
TAB-delimitted ASCII values, one record per point: 
                 fit 5 : SRO                     fit 6: UNC
radial_distance, mag., radial comp., azm. comp., mag., radial comp., azm. comp.

Example RADIAL table output:

  16.97	  0.4324  -0.2657   0.3412  0.4325  -0.2657   0.3413
  39.85	  0.2732  -0.2732   0.0015  0.2683  -0.2682  -0.0064
  39.85	  0.3513   0.0035  -0.3513  0.3419   0.0038  -0.3419
  53.74	  0.4494   0.4403  -0.0899  0.4375   0.4274  -0.0934
  63.15	  0.3666  -0.2700   0.2480  0.3599  -0.2694   0.2387
  63.15	  0.2719   0.2437  -0.1207  0.2646   0.2396  -0.1123
  72.72	  0.1763  -0.1643  -0.0640  0.1640  -0.1511  -0.0636
  72.72	  0.2237  -0.0442  -0.2193  0.2155  -0.0326  -0.2130
  87.69	  0.5201  -0.1314   0.5032  0.5203  -0.1442   0.4999

Also, the residual value for the Unconstrained fit may be optionally
output to a TCL variable in the calling procedure or session.  The 
user must specify the name of the declared (real) variable as the 
value of the UNCRES parameter.

RESTRICTIONS

1.  No more than 900 coordinates.

2.  MARK-format input files.  Real*4 (line,samp) pairs on one record.

3.  Both inputs must have the same number of coordinates and that must
    agree with the parameters or label values.

4.  No more than 100 columns of coordinates in grid.

EXAMPLES

1. Comparison with perfect input grid.
	gridlocb IN RAW
	gridgen REF NCOL=20 NROW=20
	locus2 (RAW,REF) NCOL=20 NROW=20 SROFILES=(A,B,C) UNCFILES=(D,E,F)

2. Comparison of two imaged grids.
	gridlocb IN  RAW
	gridlocb IN2 REF
	locus2 (RAW,REF) NCOL=20 NROW=20 UNCFILES=(A,B,C)

3. Comparison with internally-generated perfect grid.
	gridlocb IN RAW
        local U42 REAL
	locus2 RAW NROW=20 NCOL=20 UNCFILES=(A,B,C) +
		RADIAL=RAD.TBL CENTER=(512,512) UNCRES=U42
        disp U42

WRITTEN BY:             Charlie Avis

COGNIZANT PROGRAMMER:   Charlie Avis

REVISIONS:
   02-73  ...kreznar...  wrote LOCUS
   12-93  ...CCA...      inital simplification into LOCUS2
    1-96  ...CCA...      MADE GENGRID INC REAL, ADDED OUTPUT OF RADIAL
                         COMPONENT OF VECTORS IN RADIAL TABLE
    1-97  ...cca...      updated Help and added output of unc. res. to TCL
    6-97  ...rrd...      Made portable for UNIX.
.LEVEL1
.VARIABLE INP
1 or 2 input MARK-format
coordinate files
.VARIABLE NROW
Number of rows in 
the coordinate grid
.VARIABLE NCOL
Number of columns in 
the coordinate grid
.VARIABLE SROFILES
The names of the three
files to contain the 
output for the 
Scale, Rotation and
Offset fit.
.VARIABLE UNCFILES
The names of the three
files to contain the 
output for the 
uncontrained fit.
.VARIABLE RADIAL
The name of the file
to contain the table 
of radial distance 
vs. magnitudes and
components.
.VARIABLE CENTER
The (line,samp) of the
image center.
.VARIABLE UNCRES
The name of the TCL
variable to receive 
the value of the 
residuals for the
unconstrained fit.
.LEVEL2
.VARIABLE INP
String
1 or 2 input MARK-format coordinate files.  The first input holds the 
coordinates to be transformed.  The second input holds the reference
set (possibly generated by GRIDGEN).  The second input may have the 
keywords GRID_NROW and GRID_NCOL in the label to define the size
of the coordinates sets.  

The second input is optional.  If it is not supplied, LOCUS2 will
generate a perfect grid comparable to that generated by GRIDGEN.
.VARIABLE NROW
Integer
Number of rows in the coordinate grid.  
This is used to override
missing or invalid value of GRID_NROW in the second input label.  
It is also used to construct the perfect grid if no second input is
supplied.
.VARIABLE NCOL
Integer
Number of columns in the coordinate grid.  
This is only used to override
missing or invalid value of GRID_NCOL in the second input label.  
It is also used to construct the perfect grid if no second input is
supplied.
.VARIABLE SROFILES
String, count=3
The names of the three files to contain the output for the Scale, 
Rotation and Offset fit. The first file will get the tab-delimitted 
ASCII file containing the residual in the line direction after the 
SRO fit.  The second will get the tab-delimitted ASCII file containing 
the residual in the sample direction after the SRO fit.  The third 
will get the tab-delimitted ASCII file containing the magnitude of 
the residual after the SRO fit.
.VARIABLE UNCFILES
String, count=3
The names of the three files to contain the output for the Unconstrained
fit. The first file will get the tab-delimitted ASCII file containing 
the residual in the line direction after the Uncontrained fit.  The 
second will get the tab-delimitted ASCII file containing the residual 
in the sample direction after the Uncontrained fit.  The third will get 
the tab-delimitted ASCII file containing the magnitude of the residual 
after the Uncontrained fit.
.VARIABLE RADIAL
The name of the file to contain the table of radial distance vs. magnitudes 
and components.  The file is a tab-delimitted ASCII file containing one 
record for each coordinate point.  The record contains the radial distance 
from the center of the image, the magnitude, radial and azimuthal components
of the SRO residual, and the magnitude, radial and azimuthal components of the
Unconstrained residual. 
.VARIABLE CENTER
The (line,samp) of the image center.  Required if the RADIAL is given.
.VARIABLE UNCRES
NAME - OPTIONAL - DEFAULT=DMY1
The name of the TCL variable to receive the value of the residuals for the
unconstrained fit.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstlocus2.pdf
PROCEDURE
REFGBL $ECHO
REFGBL $AUTOUSAGE
BODY
LET $ECHO="YES"
LET $AUTOUSAGE="NONE"
!---verification of calculations
!FIRST RUN OF LOCUS2 VERIFIES THAT WE'RE GETTING OUT THE SAME
!DISTORTION THAT WE PUT IN
!SECOND RUN PUTS THE REFERENCE IN 2ND AS NORMALLY RUN
!THIRD RUN COMPARES THIS TO INTERNALLY GENERATED GRID RESULT

gridgen G NROW=20 NCOL=20 GSL=40 GSS=40

skew G GO OFFSET=(25,10) NROW=20 NCOL=20
locus2 (G,GO) NROW=20 NCOL=20
locus2 (GO,G) NROW=20 NCOL=20
locus2 GO NROW=20 NCOL=20

skew G GO ROTOFF=(-25,50,16) NROW=20 NCOL=20
locus2 (G,GO) NROW=20 NCOL=20
locus2 (GO,G) NROW=20 NCOL=20
locus2 GO NROW=20 NCOL=20

skew G GO SCALEOFF=(55,-10,1.2) NROW=20 NCOL=20
locus2 (G,GO) NROW=20 NCOL=20
locus2 (GO,G) NROW=20 NCOL=20
locus2 GO NROW=20 NCOL=20

skew G GO SRO=(5,-15,-55,2.1) NROW=20 NCOL=20
locus2 (G,GO) NROW=20 NCOL=20
locus2 (GO,G) NROW=20 NCOL=20
locus2 GO NROW=20 NCOL=20

skew G GO UNC=(1.3,0.8,0.9,1.8,25,10) NROW=20 NCOL=20
locus2 (G,GO) NROW=20 NCOL=20
locus2 (GO,G) NROW=20 NCOL=20
locus2 GO NROW=20 NCOL=20

!-----verification of products
locus2 GO NROW=20 NCOL=20 UNCFILES=(UNC.L,UNC.S,UNC.M) +
         SROFILES=(SRO.L,SRO.S,SRO.M)

local URCHK REAL
locus2 GO NROW=20 NCOL=20 RADIAL=RAD.DAT CENTER=(500,500) UNCRES=URCHK
disp URCHK

!-----verify label reading
label-l GO
locus2 GO NROW=20 NCOL=20 

label-l G
locus2 (GO,G) NROW=20 NCOL=20

END-PROC
$!-----------------------------------------------------------------------------
$ create tstlocus2.log_solos
tstlocus2
LET $AUTOUSAGE="NONE"
gridgen G NROW=20 NCOL=20 GSL=40 GSS=40
Beginning VICAR task gridgen
GRIDGEN version 1 July 1994

       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19    20
     40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0  40.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

      21    22    23    24    25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40
     80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0  80.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

      41    42    43    44    45    46    47    48    49    50    51    52    53    54    55    56    57    58    59    60
    120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0 120.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

      61    62    63    64    65    66    67    68    69    70    71    72    73    74    75    76    77    78    79    80
    160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0 160.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

      81    82    83    84    85    86    87    88    89    90    91    92    93    94    95    96    97    98    99   100
    200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0 200.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     101   102   103   104   105   106   107   108   109   110   111   112   113   114   115   116   117   118   119   120
    240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0 240.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     121   122   123   124   125   126   127   128   129   130   131   132   133   134   135   136   137   138   139   140
    280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0 280.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     141   142   143   144   145   146   147   148   149   150   151   152   153   154   155   156   157   158   159   160
    320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0 320.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     161   162   163   164   165   166   167   168   169   170   171   172   173   174   175   176   177   178   179   180
    360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0 360.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     181   182   183   184   185   186   187   188   189   190   191   192   193   194   195   196   197   198   199   200
    400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0 400.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     201   202   203   204   205   206   207   208   209   210   211   212   213   214   215   216   217   218   219   220
    440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0 440.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     221   222   223   224   225   226   227   228   229   230   231   232   233   234   235   236   237   238   239   240
    480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0 480.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     241   242   243   244   245   246   247   248   249   250   251   252   253   254   255   256   257   258   259   260
    520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0 520.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     261   262   263   264   265   266   267   268   269   270   271   272   273   274   275   276   277   278   279   280
    560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0 560.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     281   282   283   284   285   286   287   288   289   290   291   292   293   294   295   296   297   298   299   300
    600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0 600.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     301   302   303   304   305   306   307   308   309   310   311   312   313   314   315   316   317   318   319   320
    640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0 640.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     321   322   323   324   325   326   327   328   329   330   331   332   333   334   335   336   337   338   339   340
    680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0 680.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     341   342   343   344   345   346   347   348   349   350   351   352   353   354   355   356   357   358   359   360
    720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0 720.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     361   362   363   364   365   366   367   368   369   370   371   372   373   374   375   376   377   378   379   380
    760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0 760.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0

     381   382   383   384   385   386   387   388   389   390   391   392   393   394   395   396   397   398   399   400
    800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0 800.0
     40.0  80.0 120.0 160.0 200.0 240.0 280.0 320.0 360.0 400.0 440.0 480.0 520.0 560.0 600.0 640.0 680.0 720.0 760.0 800.0
GRIDGEN task completed
skew G GO OFFSET=(25,10) NROW=20 NCOL=20
Beginning VICAR task skew
locus2 (G,GO) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000             26.925823

 OFFSET ONLY                             1.000000        0.000000              25.000000
                                         0.000000        1.000000              10.000000              0.000000

 ROTATE, OFFSET                          1.000000        0.000000              25.000000
                                         0.000000        1.000000              10.000000              0.000000

 MAGNIFY, OFFSET                         1.000000        0.000000              25.000000
                                         0.000000        1.000000              10.000000              0.000000

 MAGNIFY, ROTATE, OFFSET                 1.000000        0.000000              25.000000
                                         0.000000        1.000000              10.000000              0.000000

 UNCONSTRAINED LINEAR, OFFSET            1.000000        0.000000              25.000000
                                         0.000000        1.000000              10.000000              0.000000
locus2 (GO,G) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000             26.925823

 OFFSET ONLY                             1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 ROTATE, OFFSET                          1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 MAGNIFY, OFFSET                         1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 MAGNIFY, ROTATE, OFFSET                 1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 UNCONSTRAINED LINEAR, OFFSET            1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000
locus2 GO NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   40.000000
 SS  =   40.000000
 INC =   40.000000

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000             26.925823

 OFFSET ONLY                             1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 ROTATE, OFFSET                          1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 MAGNIFY, OFFSET                         1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 MAGNIFY, ROTATE, OFFSET                 1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000

 UNCONSTRAINED LINEAR, OFFSET            1.000000        0.000000             -25.000000
                                         0.000000        1.000000             -10.000000              0.000000
skew G GO ROTOFF=(-25,50,16) NROW=20 NCOL=20
Beginning VICAR task skew
locus2 (G,GO) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            235.061493

 OFFSET ONLY                             1.000000        0.000000            -157.037787
                                         0.000000        1.000000             149.497595             90.793785

 ROTATE, OFFSET                          0.961262       -0.275637             -25.000008
                                         0.275637        0.961262              49.999992              0.000025

 MAGNIFY, OFFSET                         0.961262        0.000000            -140.767695
                                         0.000000        0.961262             165.767688             89.910187

 MAGNIFY, ROTATE, OFFSET                 0.961262       -0.275637             -25.000005
                                         0.275637        0.961262              49.999998              0.000024

 UNCONSTRAINED LINEAR, OFFSET            0.961262       -0.275637             -25.000005
                                         0.275637        0.961262              49.999997              0.000024
locus2 (GO,G) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            235.061493

 OFFSET ONLY                             1.000000        0.000000             157.037787
                                         0.000000        1.000000            -149.497595             90.793785

 ROTATE, OFFSET                          0.961262        0.275637              10.249684
                                        -0.275637        0.961262             -54.954013              0.000025

 MAGNIFY, OFFSET                         0.961262        0.000000             167.224495
                                         0.000000        0.961262            -127.436229             89.910187

 MAGNIFY, ROTATE, OFFSET                 0.961262        0.275637              10.249680
                                        -0.275637        0.961262             -54.954018              0.000024

 UNCONSTRAINED LINEAR, OFFSET            0.961262        0.275637              10.249681
                                        -0.275637        0.961262             -54.954018              0.000024
locus2 GO NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   38.450455
 SS  =   38.450455
 INC =   38.450455

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            235.325989

 OFFSET ONLY                             1.000000        0.000000             140.767503
                                         0.000000        1.000000            -165.767880             89.910187

 ROTATE, OFFSET                          0.961262        0.275637              -6.020600
                                        -0.275637        0.961262             -71.224298             12.636242

 MAGNIFY, OFFSET                         0.924023        0.000000             160.746454
                                         0.000000        0.924023            -122.499439             86.427162

 MAGNIFY, ROTATE, OFFSET                 0.924023        0.264959               9.852670
                                        -0.264959        0.924023             -52.825109              0.000040

 UNCONSTRAINED LINEAR, OFFSET            0.924023        0.264959               9.852671
                                        -0.264959        0.924023             -52.825108              0.000040
skew G GO SCALEOFF=(55,-10,1.2) NROW=20 NCOL=20
Beginning VICAR task skew
locus2 (G,GO) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            170.449432

 OFFSET ONLY                             1.000000        0.000000             139.000014
                                         0.000000        1.000000              74.000016             65.238045

 ROTATE, OFFSET                          1.000000        0.000000             139.000014
                                         0.000000        1.000000              74.000016             65.238045

 MAGNIFY, OFFSET                         1.200000        0.000000              54.999984
                                         0.000000        1.200000             -10.000013              0.000026

 MAGNIFY, ROTATE, OFFSET                 1.200000        0.000000              54.999984
                                         0.000000        1.200000             -10.000013              0.000026

 UNCONSTRAINED LINEAR, OFFSET            1.200000        0.000000              54.999984
                                         0.000000        1.200000             -10.000013              0.000026
locus2 (GO,G) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            170.449432

 OFFSET ONLY                             1.000000        0.000000            -139.000014
                                         0.000000        1.000000             -74.000016             65.238045

 ROTATE, OFFSET                          1.000000        0.000000            -139.000014
                                         0.000000        1.000000             -74.000016             65.238045

 MAGNIFY, OFFSET                         0.833333        0.000000             -45.833318
                                         0.000000        0.833333               8.333344              0.000022

 MAGNIFY, ROTATE, OFFSET                 0.833333        0.000000             -45.833318
                                         0.000000        0.833333               8.333344              0.000022

 UNCONSTRAINED LINEAR, OFFSET            0.833333        0.000000             -45.833317
                                         0.000000        0.833333               8.333343              0.000022
locus2 GO NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   48.000000
 SS  =   48.000000
 INC =   48.000000

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000             55.901711

 OFFSET ONLY                             1.000000        0.000000             -55.000014
                                         0.000000        1.000000               9.999984              0.000035

 ROTATE, OFFSET                          1.000000        0.000000             -55.000014
                                         0.000000        1.000000               9.999984              0.000035

 MAGNIFY, OFFSET                         1.000000        0.000000             -54.999981
                                         0.000000        1.000000              10.000013              0.000026

 MAGNIFY, ROTATE, OFFSET                 1.000000        0.000000             -54.999981
                                         0.000000        1.000000              10.000013              0.000026

 UNCONSTRAINED LINEAR, OFFSET            1.000000        0.000000             -54.999980
                                         0.000000        1.000000              10.000012              0.000026
skew G GO SRO=(5,-15,-55,2.1) NROW=20 NCOL=20
Beginning VICAR task skew
locus2 (G,GO) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000           1185.529907

 OFFSET ONLY                             1.000000        0.000000             813.386479
                                         0.000000        1.000000            -651.597583            565.070007

 ROTATE, OFFSET                          0.573576        0.819152             648.440510
                                        -0.819152        0.573576            -128.455873            358.809113

 MAGNIFY, OFFSET                         1.204511        0.000000             727.492028
                                         0.000000        1.204511            -737.492034            561.118469

 MAGNIFY, ROTATE, OFFSET                 1.204511        1.720219               4.999994
                                        -1.720219        1.204511             -15.000001              0.000051

 UNCONSTRAINED LINEAR, OFFSET            1.204511        1.720219               5.000000
                                        -1.720219        1.204511             -14.999993              0.000051
locus2 (GO,G) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000           1185.529907

 OFFSET ONLY                             1.000000        0.000000            -813.386479
                                         0.000000        1.000000             651.597583            565.070007

 ROTATE, OFFSET                          0.573576       -0.819152            -477.155123
                                         0.819152        0.573576            -457.492070            358.809113

 MAGNIFY, OFFSET                         0.273132        0.000000              83.123082
                                         0.000000        0.273132             483.256637            267.199310

 MAGNIFY, ROTATE, OFFSET                 0.273132       -0.390072              -7.216743
                                         0.390072        0.273132               2.146616              0.000024

 UNCONSTRAINED LINEAR, OFFSET            0.273132       -0.390072              -7.216742
                                         0.390072        0.273132               2.146611              0.000024
locus2 GO NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   48.180420
 SS  =   48.180420
 INC =   48.180420

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000           1178.131226

 OFFSET ONLY                             1.000000        0.000000            -727.492070
                                         0.000000        1.000000             737.491993            561.118469

 ROTATE, OFFSET                          0.573576       -0.819152            -391.260713
                                         0.819152        0.573576            -371.597661            292.099792

 MAGNIFY, OFFSET                         0.328990        0.000000             100.122624
                                         0.000000        0.328990             582.087692            321.844360

 MAGNIFY, ROTATE, OFFSET                 0.328990       -0.469846              -8.692643
                                         0.469846        0.328990               2.585621              0.000029

 UNCONSTRAINED LINEAR, OFFSET            0.328990       -0.469846              -8.692641
                                         0.469846        0.328990               2.585616              0.000029
skew G GO UNC=(1.3,0.8,0.9,1.8,25,10) NROW=20 NCOL=20
Beginning VICAR task skew
locus2 (G,GO) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            936.654175

 OFFSET ONLY                             1.000000        0.000000             486.999999
                                         0.000000        1.000000             724.000000            340.552490

 ROTATE, OFFSET                          0.999480       -0.032241             500.759694
                                         0.032241        0.999480             710.677009            340.300507

 MAGNIFY, OFFSET                         1.550000        0.000000             256.000000
                                         0.000000        1.550000             493.000001            289.465027

 MAGNIFY, ROTATE, OFFSET                 1.550000       -0.050000             276.999999
                                         0.050000        1.550000             472.000001            289.005188

 UNCONSTRAINED LINEAR, OFFSET            1.300000        0.800000              24.999999
                                         0.900000        1.800000              10.000000              0.000006
locus2 (GO,G) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            936.654175

 OFFSET ONLY                             1.000000        0.000000            -486.999999
                                         0.000000        1.000000            -724.000000            340.552490

 ROTATE, OFFSET                          0.999480        0.032241            -523.412502
                                        -0.032241        0.999480            -694.162398            340.300507

 MAGNIFY, OFFSET                         0.485893        0.000000             -20.705329
                                         0.000000        0.485893            -135.862069            162.069305

 MAGNIFY, ROTATE, OFFSET                 0.485893        0.015674             -38.636362
                                        -0.015674        0.485893            -121.645768            161.811844

 UNCONSTRAINED LINEAR, OFFSET            1.111111       -0.493827             -22.839505
                                        -0.555556        0.802469               5.864197              0.000007
locus2 GO NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   72.000000
 SS  =   72.000000
 INC =   72.000000

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            513.600037

 OFFSET ONLY                             1.000000        0.000000            -150.999999
                                         0.000000        1.000000            -388.000000            300.732452

 ROTATE, OFFSET                          0.999480        0.032241            -187.412502
                                        -0.032241        0.999480            -358.162398            300.218536

 MAGNIFY, OFFSET                         0.874608        0.000000             -37.269591
                                         0.000000        0.874608            -244.551723            291.724731

 MAGNIFY, ROTATE, OFFSET                 0.874608        0.028213             -69.545452
                                        -0.028213        0.874608            -218.962383            291.261322

 UNCONSTRAINED LINEAR, OFFSET            2.000000       -0.888889             -41.111109
                                        -1.000000        1.444444              10.555555              0.000013
locus2 GO NROW=20 NCOL=20 UNCFILES=(UNC.L,UNC.S,UNC.M)  +
         SROFILES=(SRO.L,SRO.S,SRO.M)
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   72.000000
 SS  =   72.000000
 INC =   72.000000

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            513.600037

 OFFSET ONLY                             1.000000        0.000000            -150.999999
                                         0.000000        1.000000            -388.000000            300.732452

 ROTATE, OFFSET                          0.999480        0.032241            -187.412502
                                        -0.032241        0.999480            -358.162398            300.218536

 MAGNIFY, OFFSET                         0.874608        0.000000             -37.269591
                                         0.000000        0.874608            -244.551723            291.724731

 MAGNIFY, ROTATE, OFFSET                 0.874608        0.028213             -69.545452
                                        -0.028213        0.874608            -218.962383            291.261322

 UNCONSTRAINED LINEAR, OFFSET            2.000000       -0.888889             -41.111109
                                        -1.000000        1.444444              10.555555              0.000013
local URCHK REAL
locus2 GO NROW=20 NCOL=20 RADIAL=RAD.DAT CENTER=(500,500) UNCRES=URCHK
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   72.000000
 SS  =   72.000000
 INC =   72.000000

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            513.600037

 OFFSET ONLY                             1.000000        0.000000            -150.999999
                                         0.000000        1.000000            -388.000000            300.732452

 ROTATE, OFFSET                          0.999480        0.032241            -187.412502
                                        -0.032241        0.999480            -358.162398            300.218536

 MAGNIFY, OFFSET                         0.874608        0.000000             -37.269591
                                         0.000000        0.874608            -244.551723            291.724731

 MAGNIFY, ROTATE, OFFSET                 0.874608        0.028213             -69.545452
                                        -0.028213        0.874608            -218.962383            291.261322

 UNCONSTRAINED LINEAR, OFFSET            2.000000       -0.888889             -41.111109
                                        -1.000000        1.444444              10.555555              0.000013
disp URCHK

URCHK=1.33251833176e-05

label-l GO
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File GO ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a SUN-SOLR host
                1 bands
                1 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GRIDGEN -- User: lwk -- Wed Mar 28 19:08:47 2012 ----
---- Task: SKEW -- User: lwk -- Wed Mar 28 19:08:57 2012 ----
 
************************************************************
locus2 GO NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
COMPARING TO PERFECT GRID WITH
 SL  =   72.000000
 SS  =   72.000000
 INC =   72.000000

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            513.600037

 OFFSET ONLY                             1.000000        0.000000            -150.999999
                                         0.000000        1.000000            -388.000000            300.732452

 ROTATE, OFFSET                          0.999480        0.032241            -187.412502
                                        -0.032241        0.999480            -358.162398            300.218536

 MAGNIFY, OFFSET                         0.874608        0.000000             -37.269591
                                         0.000000        0.874608            -244.551723            291.724731

 MAGNIFY, ROTATE, OFFSET                 0.874608        0.028213             -69.545452
                                        -0.028213        0.874608            -218.962383            291.261322

 UNCONSTRAINED LINEAR, OFFSET            2.000000       -0.888889             -41.111109
                                        -1.000000        1.444444              10.555555              0.000013
label-l G
Beginning VICAR task label
LABEL version 15-Nov-2010
************************************************************
 
        ************  File G ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in REAL format from a SUN-SOLR host
                1 bands
                1 lines per band
                800 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GRIDGEN -- User: lwk -- Wed Mar 28 19:08:47 2012 ----
 
************************************************************
locus2 (GO,G) NROW=20 NCOL=20
Beginning VICAR task locus2
GRID_NROW NOT FOUND IN INPUT
GRID_NCOL NOT FOUND IN INPUT
KEYWORD GRID_NROW NOT FOUND IN REF
KEYWORD GRID_NCOL NOT FOUND IN REF

 LOCUS COORDINATE DETRENDING STATISTICS

 DEGREES OF FREEDOM ALLOWED              MATRIX FOR LINEAR                OFFSET PART OF         RESULTING RMS
  IN MINIMIZING THE RMS                   PART OF TRANSFORMATION           TRANSFORMATION

 NONE                                    1.000000        0.000000               0.000000
                                         0.000000        1.000000               0.000000            936.654175

 OFFSET ONLY                             1.000000        0.000000            -486.999999
                                         0.000000        1.000000            -724.000000            340.552490

 ROTATE, OFFSET                          0.999480        0.032241            -523.412502
                                        -0.032241        0.999480            -694.162398            340.300507

 MAGNIFY, OFFSET                         0.485893        0.000000             -20.705329
                                         0.000000        0.485893            -135.862069            162.069305

 MAGNIFY, ROTATE, OFFSET                 0.485893        0.015674             -38.636362
                                        -0.015674        0.485893            -121.645768            161.811844

 UNCONSTRAINED LINEAR, OFFSET            1.111111       -0.493827             -22.839505
                                        -0.555556        0.802469               5.864197              0.000007
END-PROC
disable-log
$ Return
$!#############################################################################
