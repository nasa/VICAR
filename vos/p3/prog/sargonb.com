$!****************************************************************************
$!
$! Build proc for MIPL module sargonb
$! VPACK Version 1.8, Tuesday, March 05, 1996, 13:42:17
$!
$! Execute by entering:		$ @sargonb
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
$ write sys$output "*** module sargonb ***"
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
$ write sys$output "Invalid argument given to sargonb.com file -- ", primary
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
$   if F$SEARCH("sargonb.imake") .nes. ""
$   then
$      vimake sargonb
$      purge sargonb.bld
$   else
$      if F$SEARCH("sargonb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sargonb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sargonb.bld "STD"
$   else
$      @sargonb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sargonb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sargonb.com -
	-s sargonb.f -
	-i sargonb.imake -
	-p sargonb.pdf -
	-t tstsargonb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sargonb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C-----PROGRAM SARGONB                   E,SARGONB,A,B,,PAR
C-----THIS PROGRAM WILL INTERPOLATE OVER ARBITRARY POLYGONS.
C-----IT CAN HANDLE UP TO 25 POLYGONS OF UP TO 25 VERTICES EACH.

C     01 JUL 94   ...CRI...   MSTP S/W CONVERSION (VICAR PORTING)
C     28 JAN 94   ...LWK...   CHANGED DIMENSION OF 'LINE' BUFFER FROM 100000
C				BYTES TO 20000 BECAUSE OF HALFWORD CONVERSION
C				IN SUBR. OPRATE (TO SUPPORT MGN. IMAGES)
C     28 AUG 87   ...SP....   DELETED UNNECESSARY REFERENCE TO MISSPELLED 
C                             VARIABLE BUF IN COLECT.  REMOVED SOME DEAD CODE.
C     4-NOV-85   ...LWK...  CHANGE PROCESSING OF (RADIUS,PERC,MIN,MAX)
C     17 OCT 85   ...FFM...  1. PUT "NL,NS" IN COMMON AREA
C                            2. DELETE PARAMETERS "SIZE" AND "FORMAT"
C                            3. CONVERT TO VICAR2 I/O
C                            4. ADD 75 MORE KEYWORDS "FUNC1,CONST1,VERT1..."
C                               SO THE USER CAN SPECIFY UP TO 25 SEPARATE
C                               POLYGONS
C     29 DEC 83   ...HBD...   CHECKS IF SPECIFIED POLYGON IS COMPLETELY
C                             OUTSIDE IMAGE
C     20 OCT 82   ...CCA...   INITIAL RELEASE
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'fortport'
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
      COMMON/D/ICOD,DBUG
      COMMON/E/NL,NS
      COMMON/F/OUTUNIT
      CHARACTER*6 FUNCNAME(25)
      CHARACTER*7 CNSTNAME(25)
      CHARACTER*6 VERTNAME(25)
      INTEGER*4 NVERT(25),FUNC(25),ICONST(4,25),IRR(4)
      INTEGER*2 PTBUF(3,40000)/120000*0/
      INTEGER*2 SEGM(3,20000)/60000*0/
      INTEGER*2 SORT(2,40000)/80000*0/
      REAL*4 V(2,26,25),T
      BYTE LINE(20000)
      CHARACTER*8 FBUF
      LOGICAL XVPTST
C
      DATA FUNCNAME/'FUNC1 ','FUNC2 ','FUNC3 ','FUNC4 ',
     &'FUNC5 ','FUNC6 ','FUNC7 ','FUNC8 ','FUNC9 ','FUNC10','FUNC11',
     &'FUNC12','FUNC13','FUNC14','FUNC15','FUNC16','FUNC17','FUNC18',
     &'FUNC19','FUNC20','FUNC21','FUNC22','FUNC23','FUNC24','FUNC25'/
      DATA CNSTNAME/'CONST1 ','CONST2 ','CONST3 ','CONST4 ',
     &'CONST5 ','CONST6 ','CONST7 ','CONST8 ','CONST9 ','CONST10',
     &'CONST11','CONST12','CONST13','CONST14','CONST15','CONST16',
     &'CONST17','CONST18','CONST19','CONST20','CONST21','CONST22',
     &'CONST23','CONST24','CONST25'/
      DATA VERTNAME/'VERT1 ','VERT2 ','VERT3 ','VERT4 ',
     &'VERT5 ','VERT6 ','VERT7 ','VERT8 ','VERT9 ','VERT10','VERT11',
     &'VERT12','VERT13','VERT14','VERT15','VERT16','VERT17','VERT18',
     &'VERT19','VERT20','VERT21','VERT22','VERT23','VERT24','VERT25'/
      DO 6969 I=1,4
      DO 6969 J=1,25
        ICONST(1,J)=0
6969  CONTINUE
      DATA NVERT/25*0/,FUNC/25*0/,IRR/4*0/
      DATA V/1300*0.0/,T/0.0/
c      DATA LINE/20000*' '/
      DATA LINE/20000*0/	! don't think it needs blank fill. rgd 3/2010
      DATA FBUF/'        '/
C      DATA XVPTST/0/, INDEX/0/
C
      CALL IFMESSAGE('SARGONB version 01-JUL-94')
C     OPEN INPUT
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ') 
C
C     FIND OUT FORMAT,NUMBER OF LINES,NUMBER OF SAMPLES
      CALL XVGET(INUNIT,STATUS,'NL',NL,'NS',NS,'PIX_SIZE',
     &           BYTPIX,' ')
      IF (BYTPIX .EQ. 1) THEN
          ICOD=1
      ELSE IF (BYTPIX .EQ. 2) THEN
          ICOD=2
      ELSE
          CALL XVMESSAGE('WRONG INPUT FORMAT',' ')
          CALL ABEND
      END IF
C
C----------------------------------------------------------------------
C-----PROCESS PARAMETERS
C     param "DBUG"
      DBUG=0
      IF (XVPTST('DBUG')) DBUG=1
C
C----------------------------------------------------------------------
C-----SET DEFAULTS
      NAREA = 0
C------------------------------------------------------------------
C-----    FUNC OR FNC  =  0.......INTERPOLATE
C-----                    1.......MULTIPLY
C-----                    2.......ADD
C-----                    3.......SUBTRACT
C-----                    4.......DIVIDE
C-----                    5.......SET TO DN
C-----                    6.......ZERO OUT
C-------------------------------------------------------------------
C
      DO I=1,25
      CALL XVPARM(FUNCNAME(I),FBUF,COUNT,DEF,8)
      IF (COUNT.EQ.0 .OR. DEF.EQ.1) GO TO 1
      NAREA=NAREA+1
      IF (INDEX(FBUF,'INTERP').ne.0 .OR. INDEX(FBUF,'interp').ne.0) THEN
              FUNC(NAREA)=0

C					!DEFAULTS:
	      ICONST(1,NAREA) = 200	!RADIUS
	      ICONST(2,NAREA) = 30	!PERCENT
	      ICONST(3,NAREA) = -9999	!DNMIN
	      ICONST(4,NAREA) = 32767	!DNMAX
              CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	      IF (COUNT.GE.1) ICONST(1,NAREA) = IRR(1)
	      IF (COUNT.GE.2) ICONST(2,NAREA) = IRR(2)
	      IF (COUNT.GE.3) ICONST(3,NAREA) = IRR(3)
	      IF (COUNT.GE.4) ICONST(4,NAREA) = IRR(4)
      ELSEIF (INDEX(FBUF,'ZERO').ne.0 .OR. INDEX(FBUF,'zero').ne.0) THEN
              FUNC(NAREA)=6
      ELSEIF (INDEX(FBUF,'SETTO').ne.0 .OR. INDEX(FBUF,'setto').ne.0)
     &        THEN 
              FUNC(NAREA)=5
              CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
              ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'MULT').ne.0 .OR.INDEX(FBUF,'mult').ne.0) 
     &        THEN 
              FUNC(NAREA)=1
              CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	      ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'ADD').ne.0 .OR. INDEX(FBUF,'add').ne.0) THEN 
              FUNC(NAREA)=2
              CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	      ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'SUBTRACT').ne.0 .OR.
     &	      INDEX(FBUF,'subtract').ne.0) THEN 
              FUNC(NAREA)=3
              CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	      ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'DIVIDE').ne.0 .OR.
     &	      INDEX(FBUF,'divide').ne.0) THEN 
              FUNC(NAREA)=4
              CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	      ICONST(1,NAREA) = IRR(1)
      ELSE
              CALL XVMESSAGE('ILLEGAL FUNC STRING',' ')
              CALL ABEND
      END IF
      CALL XVPARM(VERTNAME(I),V(1,1,NAREA),COUNT,DEF,1300)
      IF (MOD(COUNT,2) .NE. 0) THEN 
               CALL XVMESSAGE('VERTICES HAVE TO BE IN PAIRS',' ')
               CALL ABEND
      ELSE IF (COUNT .LT. 6) THEN
               CALL XVMESSAGE('NEEDS AT LEAST 3 PAIRS OF 
     &                         VERTICES',' ')
               CALL ABEND
      END IF      
      NVERT(NAREA)=COUNT/2
      END DO
    1 RADI = RADI*RADI
C
C---------------------------------------------------------------------
C-----CHECK FOR DUICATED END VERTEX AND REARRANGE AS (S,L)
      DO 200 A=1,NAREA
         NV = NVERT(A)
         IF(V(1,NV,A).EQ.V(1,1,A) .AND. V(2,NV,A).EQ.V(2,1,A)) GO TO 150
         V(1,NV+1,A) = V(1,1,A)
         V(2,NV+1,A) = V(2,1,A)
         GO TO 160
  150    NV = NV -1
         NVERT(A) = NV
  160    CONTINUE
         N = NV + 1
         DO 180 J=1,N
            T = V(1,J,A)
            V(1,J,A) = V(2,J,A)
  180    V(2,J,A) = T
  200 CONTINUE
C
C-----------------------------------------------------------------
C     OPEN OUTPUT
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE',' ')
C
      DO 110 I=1,NL
      CALL XVREAD(INUNIT,LINE,STATUS,' ')
  110 CALL XVWRIT(OUTUNIT,LINE,STATUS,' ')
C
C     CLOSE INPUT AND OUTPUT
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
C     UPDATE OUTPUT
      CALL XVOPEN(OUTUNIT,STATUS,'OP','UPDATE','U_FORMAT',
     +            'HALF',' ')
C
C----------------------------------------------------------------
C-----BEGIN PROCESSING FOR EACH AREA
      DO 1500 A=1,NAREA
         NV = NVERT(A)
C-----CHECK IF POLYGON IS COMPLETELY OUTSIDE OF IMAGE
         DO 1510 I = 1, NV
            IF (V(1,I,A) .LE. NS .AND. V(2,I,A) .LE. NL) GOTO 1520
 1510    CONTINUE
         CALL XVMESSAGE('POLYGON IS COMPLETELY OUTSIDE OF IMAGE',' ')
         CALL XVMESSAGE('**** ABEND',' ')
         CALL ABEND
 1520    CALL COLECT(A,NSEG,NPTS,FUNC(A),ICONST(1,A),*998)
         CALL OPRATE(NSEG,FUNC(A),ICONST(1,A),NPTS,*998)
 1500 CONTINUE
C-----------------------------------------------------------------
C
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
  998 CALL ABEND
      END
C
C*****************************************************************
C
      SUBROUTINE COLECT(A,NSEG,NPTS,FUNC,CONST,*)
C-----THIS ROUTINE WILL COLLECT THE NECESSARY POINTS ABOUT THE
C-----PERIPHERY OF THE SPECIFIED POLYGON.
C
      IMPLICIT INTEGER (A-Z)
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
      COMMON/D/ICOD,DBUG
      COMMON/E/NL,NS
      COMMON /SEED/SEED
      INTEGER SEED
      INTEGER*4 NVERT(25),CONST(4)
      INTEGER*2 PTBUF(3,40000),SEGM(3,20000),PT(3,10000),SORT(2,40000)
      REAL*4 POINT(2),V(2,26,25),T,SLOPE,RANNUM
      CHARACTER*132 PBUF
      BYTE LINE(20000),INSIDE
C
C---------------------------------------------------------------
C-----FOR EACH SIDE OF POLYGON,COLLECT POINTS OF INTERSECTION
C-----WITH LINES.
C-----N IS THE NUMBER OF INTERSECTION POINTS
C-----NPTS IS THE NUMBER OF EXTERIOR POINTS
C-----NSEG IS THE NUMBER OF LINE SEGMENTS TO OPERATE ON
      NV = NVERT(A)
      NPTS = 0
      NSEG = 0
      N = 0
      DO 2200 J=1,NV
         SL = V(2,J,A)
         SS = V(1,J,A)
         EL = V(2,J+1,A)
         ES = V(1,J+1,A)
C-------------------------------------------------------------
C-----NOW COLLECT A DUPLICATE POINT FOR VERTICIES WHICH ARE
C-----LOCAL MINIMA OR MAXIMA, SO WE WILL END UP WITH TWO
C-----INTERSECTION POINTS AT THESE VERTICIES
         IF(J .EQ. 1) GO TO 550
         PL = V(2,J-1,A)
         PS = V(1,J-1,A)
         GO TO  560
  550    PL = V(2,NV,A)
         PS = V(1,NV,A)
  560    IF(PL .EQ. SL .OR. EL .EQ. SL) GO TO 570
         IF(MIN0(PL,SL,EL) .NE. SL .AND. MAX0(PL,SL,EL) .NE. SL)
     +      GO TO 570
         N = N + 1
         SORT(1,N) = SL
         SORT(2,N) = SS
  570    CONTINUE
C----------------------------------------------------------------
         IF(EL .GT. SL) INC = 1
         IF(EL .LT. SL) INC = -1
         IF(EL .NE. SL) GO TO 2100
         N = N + 1
         SORT(1,N) = SL
         SORT(2,N) = SS
         GO TO 2200
 2100    SLOPE = FLOAT(ES-SS) / FLOAT(EL-SL)
         L = SL
 2110    T = SLOPE * FLOAT(L-SL)
         S = IFIX(T+SS+0.5)
         N = N + 1
         SORT(1,N) = L
         SORT(2,N) = S
         L = L + INC
         IF(L .NE. EL) GO TO 2110
 2200 CONTINUE
C
C------------------------------------------------------------
C-----SORT INTERSECTION POINTS BY LINE AND SAMPLE SIMULTANEOUSLY
      CALL SORTX(SORT,N)
      IF(DBUG .EQ. 1) CALL PRNT(2,2*N,SORT,' AFT LINE.')
C--------------------------------------------------------------
C-----TRANSFORM INTERSECTION PTS INTO LINE SEGMENTS MAKING UP POLYGON
      MINL = SORT(1,1)
      MAXL = SORT(1,N)
      STL = 1
      IF(DBUG .EQ. 1) CALL PRNT(4,1,N,' NO INTRSCTN PTS.')
      DO 2300 L=MINL,MAXL
         NONL = 0
C-----FIND NUMBER OF POINTS ON LINE L
         DO 2250 P=STL,N
            IF(SORT(1,P) .NE. L) GO TO 2260
 2250    NONL = NONL + 1
 2260    CONTINUE
C-----CHECK FOR EVEN NUMBER OF POINTS ON THIS LINE
         IF(MOD(NONL,2) .EQ. 0) GO TO 2265
C-----FIXUP NECESSARY FOR THIS LINE.  IT HAS ODD # OF INTERSECTION
C-----POINTS.  WE WILL THROW OUT HE POINTS WHICH IS BOTH A VERTEX
C-----AND AN EVEN # OF POINTS FROM BEGINNING INTERSECTION.

         WRITE (PBUF,9900) NONL,L
9900  FORMAT (' ',I2,' INTERSECTION POINTS FOR LINE ',I4)
         CALL XVMESSAGE(PBUF(2:37),' ')
         DO 200 LL=2,NONL,2
            PX = STL + LL - 1
            DO 200 MM=1,NV
              IF(SORT(1,PX).EQ.V(2,MM,A) .AND. SORT(2,PX).EQ.V(1,MM,A))
     +           GOTO 250
  200    CONTINUE
         CALL XVMESSAGE('NO EVEN POINTS WERE VERTICIES',' ')
         CALL XVMESSAGE('PATHOLOGICAL FIGURE....TRY AGAIN',' ')
         RETURN 1
  250    MV = 2 * (N-PX)
         CALL MVE(2,MV,SORT(1,PX+1),SORT(1,PX),1,1)
         N = N - 1
         NONL = NONL - 1
         CALL XVMESSAGE('FIXUP SUCCESSFUL',' ')
         CALL PRNT(4,1,NONL,' PTS ON LINE.')
 2265    CONTINUE
C
C---------------------------------------------------------------
C-----FILL LINE SEGMENT AND EXTERIOR POINTS BUFFERS
         P = 1
 2268    NSEG = NSEG + 1
         SEGM(1,NSEG) = L
         NX = STL+P-1
         SEGM(2,NSEG) = SORT(2,NX)
         NX = NX + 1
         IF(P .EQ. NONL-1) GO TO 2269
         IF(SORT(2,NX) .NE. SORT(2,NX+1)) GO TO 2269
         NX = NX + 2
         P = P + 2
 2269    SEGM(3,NSEG) = SORT(2,NX)
         IF(SEGM(2,NSEG) .LE. 1) GO TO 2270
         NPTS = NPTS + 1
         PTBUF(1,NPTS) = SEGM(2,NSEG) - 1
         PTBUF(2,NPTS) = L
 2270    CONTINUE
         IF(SEGM(3,NSEG) .GE. NS) GO TO 2290
         NPTS = NPTS + 1
         PTBUF(1,NPTS) = SEGM(3,NSEG) + 1
         PTBUF(2,NPTS) = L
 2290    P = P + 2
         IF(P .LE. NONL) GO TO 2268
 2300 STL = STL + NONL
C
      IF(DBUG .EQ. 0) GO TO 2301
      CALL PRNT(4,1,NSEG,' NO LINE SEGMENTS.')
      CALL PRNT(2,3*NSEG,SEGM,' SEGM BUF.')
 2301 IF(FUNC .NE. 0) RETURN
C
C-----------------------------------------------------------------
C  INTERPOLATE:  GET CONSTANTS
      RADI = CONST(1)
      PERC = CONST(2)
      DNMIN = CONST(3)
      DNMAX = CONST(4)
C--------------------------------------------------------------
C-----COLLECT EXTERIOR POINTS ABOVE AND BELOW EACH SIDE BY
C-----MOVING IN SAMPLE DIRECTION (HENCE THE FUNNY SLOPE)
      DO 1400 J=1,NV
         SL = V(2,J,A)
         SS = V(1,J,A)
         EL = V(2,J+1,A)
         ES = V(1,J+1,A)
         S = SS
         IF(ES .GT. SS) INC = 1
         IF(ES .LT. SS) INC = -1
         IF(ES .EQ. SS) GO TO 1340
         SLOPE = FLOAT(EL-SL) / FLOAT(ES-SS)
 1310    T = SLOPE * FLOAT(S-SS) + SL
         IF(T .EQ. 1. .OR. T .EQ. 1.*NL) GO TO 1320
         L = IFIX(T - 0.001)
         POINT(1) = S
         POINT(2) = T - 1.
         IF(INSIDE(POINT,V(1,1,A),NV).NE.0) L = IFIX(T + 1. )
         CALL PTADD(L,S,NPTS)
 1320    S = S + INC
         IF(S .NE. ES) GO TO 1310
         GO TO 1400
 1340    MNL = MIN0(SL,EL)
         MXL = MAX0(SL,EL)
         IF(MNL .EQ. 1) GO TO 1350
         NPTS = NPTS + 1
         PTBUF(1,NPTS) = S
         PTBUF(2,NPTS) = MNL - 1
 1350    IF(MXL .EQ. NL) GO TO 1400
         NPTS = NPTS + 1
         PTBUF(1,NPTS) = S
         PTBUF(2,NPTS) = MXL + 1
 1400 CONTINUE
      IF(DBUG .EQ. 1) CALL PRNT(4,1,NPTS,' NO PTS TOTAL.')
C
 1500 CONTINUE
      IF(DBUG .EQ. 1) CALL PRNT(2,3*NPTS,PTBUF,' AFT SAMP.')
C
C-----------------------------------------------------------------
C-----CUT BACK ON THE NUMBER OF POINTS BY SELECTING (WEED) POINTS
C-----AT RANDOM (I HOPE).  MAKE SURE N IS IN RANGE 1 - NPTS AND
C-----IS NOT REPEATED.
      IF(PERC .EQ. 100) GO TO 1100
        WEED = PERC * NPTS / 100
	SEED = 15289
C	SEED2 = 8597
C
        DO 1000 I=1,WEED
1001      CALL RANGEN(SEED,RANNUM)
          N=RANNUM*NPTS
          IF(N .LT. 1 .OR. N .GT. NPTS) GO TO 1001
	  IF(I .EQ. 1) GO TO 1003
          DO 1002 L=1,I
            IF(N .EQ. PT(3,L)) GO TO 1001
1002      CONTINUE
1003	  PT(1,I) = PTBUF(1,N)
          PT(3,I) = N
1000      PT(2,I) = PTBUF(2,N)

        NPTS = WEED
        CALL MVE(2,3*NPTS,PT,PTBUF,1,1)
 1100 CONTINUE
      IF(DBUG .EQ. 1) CALL PRNT(4,1,NPTS,' NPTS AFT WEED.')
C
C----------------------------------------------------------------
C-----SORT EXTERIOR POINTS BY LINE AND SAMPLE IN ORDER TO READ DNS
      CALL MVE(2,NPTS,PTBUF(2,1),SORT(1,1),3,2)
      CALL MVE(2,NPTS,PTBUF(1,1),SORT(2,1),3,2)
      CALL SORTX(SORT,NPTS)
C
C-------------------------------------------------------------------
C-----FILL PTBUF BY READING DNS OF SORTED POINTS
      CALL GETDN(NPTS,*998)
C
C--------------------------------------------------------------------
C-----CHECK FOR THRESHHOLD CRITERIAN
      IF(DNMIN .EQ. -9999 .AND. DNMAX .EQ. 32768) GO TO 650
      I = 0
  600 I = I + 1
      IF(I .GT. NPTS) GO TO 650
      IF(PTBUF(3,I) .GE. DNMIN .AND. PTBUF(3,I) .LE. DNMAX) GO TO 600
      CALL MVE(2,3,PTBUF(1,NPTS),PTBUF(1,I),1,1)
      NPTS = NPTS - 1
      I = I - 1
      GO TO 600
  650 CONTINUE
C---------------------------------------------------------------------
      IF(NPTS .LT. 1) GO TO 997
      IF(DBUG .EQ. 0) RETURN
      CALL PRNT(4,1,NPTS,'0NPTS AFT THRESH.')
      CALL PRNT(2,NPTS*3,PTBUF,'0PTBUF.')
C
C----------------------------------------------------------------
C
      RETURN
  997 CALL XVMESSAGE('NO PTS IN RANGE MIN TO MAX',' ')
  998 RETURN 1
      END
C
C**************************************************************
C
      SUBROUTINE OPRATE(NSEG,FUNC,CONST,NPTS,*)
      IMPLICIT INTEGER (A-Z)
      COMMON/C/FIL(1325),PTBUF,LINE,FIL2(40000),SEGM
      COMMON/D/ICOD,DBUG
      COMMON/F/OUTUNIT
      INTEGER*4 CONST(4)
      INTEGER*2 PTBUF(3,40000),LINE(10000),SEGM(3,20000)
      INTEGER ULIM,LLIM
C
      DATA ULIM/255/,LLIM/0/
C
C-----------------------------------------------------------
C-----LOOP THROUGH ALL SEGMENTS
      IF(ICOD .EQ. 1) GO TO 10
      LLIM = -32768
      ULIM =  32767
   10 CONTINUE
C
C-----ZERO OUT LINE BUFFER
      IF(FUNC .EQ. 6) THEN
        CALL ZIA(LINE,5000)
      ENDIF
C-----SET LINE BUFFER HALFWORDS TO CONST
      CALL MVE(-6,10000,CONST(1),LINE,0,1)
C
      DO 1000 N=1,NSEG
         L = SEGM(1,N)
         SS = SEGM(2,N)
         ES = SEGM(3,N)
         NS = ES - SS + 1
         GO TO (810,810,810,810,900,900),FUNC
C-----INTERPOLATE
	 R = CONST(1)*CONST(1)
         CALL EXTRAP(NPTS,L,SS,ES,PTBUF,LINE,R)
         GO TO 900
C-----ADD SUBTRACT MULTIPLY DIVIDE
  810    CALL XVREAD(OUTUNIT,LINE,STATUS,'LINE',L,'SAMP',SS,
     +   'NSAMPS',NS,' ')
         GO TO (820,840,880,830),FUNC
C-----MULTIPLY
  820    DO 890 I=1,NS
            LINE(I) = LINE(I) * CONST(1)
            IF(LINE(I) .GT. ULIM) LINE(I) = ULIM
  890    IF(LINE(I) .LT. LLIM) LINE(I) = LLIM
         GO TO 900
C-----DIVIDE
  830    DO 891 I=1,NS
            LINE(I) = LINE(I) / CONST(1)
            IF(LINE(I) .GT. ULIM) LINE(I) = ULIM
  891    IF(LINE(I) .LT. LLIM) LINE(I) = LLIM
         GO TO 900
C-----ADD
  840    DO 892 I=1,NS
            LINE(I) = LINE(I) + CONST(1)
            IF(LINE(I) .GT. ULIM) LINE(I) = ULIM
  892    IF(LINE(I) .LT. LLIM) LINE(I) = LLIM
         GO TO 900
C-----SUBTRACT
  880    DO 893 I=1,NS
            LINE(I) = LINE(I) - CONST(1)
            IF(LINE(I) .GT. ULIM) LINE(I) = ULIM
  893    IF(LINE(I) .LT. LLIM) LINE(I) = LLIM
  900    CALL XVWRIT(OUTUNIT,LINE,STATUS,'LINE',L,'SAMP',SS,
     +   'NSAMPS',NS,' ')
C------------------------------------------------------------------
 1000 CONTINUE
C
      RETURN
C  998 RETURN 1
      END
C
C***************************************************************
C
      SUBROUTINE PTADD(L,S,NPTS)
C-----THIS ROUTINE WILL ADD THE NEW POINTS TO PTBUF OR SKIP THEM
C-----IF THE ARE ALREADY RECORDED THERE.
      IMPLICIT INTEGER (A-Z)
      COMMON/C/FIL(1325),PTBUF,FIL3(75000)
      INTEGER*2 PTBUF(3,40000)
C
      DO 90 J=1,NPTS
         D = IABS(L-PTBUF(2,J))
         IF(S .EQ. PTBUF(1,J) .AND. D .LE. 1) GO TO 100
   90 CONTINUE
      NPTS = NPTS + 1
      PTBUF(1,NPTS) = S
      PTBUF(2,NPTS) = L
  100 CONTINUE
C
      RETURN
      END
C
C**************************************************************
C
      SUBROUTINE GETDN(NPTS,*)
      IMPLICIT INTEGER (A-Z)
      COMMON/C/FIL(1325),PTBUF,LINE,SORT,FIL3(30000)
      COMMON/D/ICOD,DBUG
      COMMON/F/OUTUNIT
      INTEGER*2 PTBUF(3,40000),SORT(2,40000)
      INTEGER*2 LINE(10000)
C
C------------------------------------------------------------------
C-----SORT HAS BEEN SORTED BY LINE THEN SAMP
      NP = 0
      SPT = 1
      MINL = SORT(1,1)
      MAXL = SORT(1,NPTS)
C
C-----GET DNS ONE LINE AT A TIME
      DO 2000 L=MINL,MAXL
         NONL = 0
C
C-----FIND NUMBER OF POINTS ON THIS LINE
         DO 1150 I=SPT,NPTS
            IF(SORT(1,I) .NE. L) GO TO 1200
 1150    NONL = NONL + 1
C-----IFP & ILP ARE ELEMENTS OF SORT WHICH ARE FIRST & LAST PTS ON LINE
 1200    IF(NONL .EQ. 0) GO TO 2000
         IFP = SPT
         ILP = IFP + NONL - 1
C-----SFP & SLP ARE SAMP # IN PIC OF FIRST AND LAST POINT ON LINE L
         SFP = SORT(2,IFP)
         SLP = SORT(2,ILP)
         NB = SLP - SFP + 1
         CALL XVREAD(OUTUNIT,LINE,STATUS,'LINE',L,'SAMP',SFP,
     +   'NSAMPS',NB,' ')
C-----FOR EACH POINTS ON THIS LINE FILL PTBUF
         DO 1300 J=1,NONL
C-----S IS # WITHIN SORT OF THE JTH PT ON LINE
            S = IFP - 1 + J
C-----SAMP IS SAMP # OF J TH PT
            SAMP = SORT(2,S)
C-----E IS ELEMENT # WITHIN LINE BUFFER
            E = SAMP - SFP + 1
C           EX = 2 * E - 1
            NP = NP + 1
            PTBUF(1,NP) = SAMP
            PTBUF(2,NP) = L
C           IF(ICOD .EQ. 1) CALL MVE(3,1,LINE(E),PTBUF(3,NP))
C           IF(ICOD .EQ. 2) CALL MVE(2,1,LINE(EX),PTBUF(3,NP),1,1)
            CALL MVE(2,1,LINE(E),PTBUF(3,NP),1,1)
 1300    CONTINUE
         SPT = SPT +NONL
 2000 CONTINUE
      IF(NP .NE. NPTS) CALL XVMESSAGE('NO DNS READ NE NPTS',' ')
      RETURN
C  999 RETURN 1
      END
C
C*******************************************************************
C
	SUBROUTINE SORTX(BUF,N)
C-----THIS ROUTINE WILL SWAP THE HALFWORDS OF THE FULLWORD BUFFER
C-----SO THAT VAX WILL SORT LIKE THE IBM.

        INTEGER*2 BUF(2,N)
        INTEGER*2 BUFVAL1(40000), BUFVAL2(40000)
        INTEGER*4 BUFVAL3(40000)
        INTEGER*4 BUFNDX1(40000)
C
        DO 100 I=1,N
          BUFVAL3(I) = ((BUF(1,I)*32768) + BUF(2,I))
          BUFVAL1(I) = BUF(1,I)
          BUFVAL2(I) = BUF(2,I)
          BUFNDX1(I) = I
100     CONTINUE

	CALL INDSRTF(BUFVAL3,BUFNDX1,N)

        DO 200 I=1,N
          II = BUFNDX1(I)
          BUF(1,I) = BUFVAL1(II)
          BUF(2,I) = BUFVAL2(II)
200     CONTINUE
C
	RETURN
	END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sargonb.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM sargonb

   To Create the build file give the command:

		$ vimake sargonb			(VMS)
   or
		% vimake sargonb			(Unix)


************************************************************************/


#define PROGRAM	sargonb
#define R2LIB

#define MODULE_LIST sargonb.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
/* #define LIB_LOCAL */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create sargonb.pdf
process help=*
PARM INP      TYPE=STRING
PARM OUT      TYPE=STRING                        DEFAULT=SARGONB
PARM FUNC1    TYPE=(STRING,8)  COUNT=1 +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST1   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT1    TYPE=REAL        COUNT=0:50        
PARM FUNC2    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST2   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT2    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC3    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST3   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT3    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC4    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST4   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT4    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC5    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST5   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT5    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC6    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST6   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT6    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC7    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST7   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT7    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC8    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST8   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT8    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC9    TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST9   TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT9    TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC10   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=--   + 
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST10  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT10   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC11   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST11  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT11   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC12   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST12  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT12   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC13   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST13  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT13   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC14   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST14  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT14   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC15   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST15  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT15   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC16   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST16  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT16   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC17   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST17  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT17   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC18   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST18  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT18   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC19   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST19  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT19   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC20   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST20  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT20   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC21   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST21  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT21   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC22   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST22  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT22   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC23   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST23  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT23   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC24   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST24  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT24   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM FUNC25   TYPE=(STRING,8)  COUNT=0:1         DEFAULT=-- +
  VALID=(ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP)
PARM CONST25  TYPE=INTEGER     COUNT=0:4         DEFAULT=--
PARM VERT25   TYPE=REAL        COUNT=0:50        DEFAULT=0
PARM DBUG     TYPE=KEYWORD COUNT=0:1 VALID=DBUG DEFAULT=--
END-PROC
.TITLE
 SARGONB  --  operates on polygons inside image.
.HELP
 PURPOSE:

 SARGONB is a VICAR applications program which performs operations on user
 specified polygons. SARGONB operates on byte or halfword data, and is a
 batch version of SARGON.  The maximum image line length supported is
 10,000 samples.  SARGONB will interpolate over arbitrary polygons.
 It can handle up to 25 polygons of up to 25 vertices each.

 SARGONB stores all operations (parameters FUNCi and CONSTi) and areas (VERTi)
 specified, then operates on each area in order. Only one operation can be 
 performed for each VERT specified.  Each operation is specified by the
 associated FUNC parameter (specifies type of operation) and CONST parameter,
 which specifies 0-4 numerical constants needed.  (Most operations require
 only one CONST;  operation ZERO does not require any, and operation INTERP
 requires 4, which may be defaulted.)

.PAGE
 INTERPOLATION:

 For interpolation (FUNCi = INTERP), the associated values of CONSTi are:
   RADI, PERC,   MIN,   MAX.
   (200,   30, -9999, 32767) are the respective defaults.

 Those pixels pixels exterior to the polygon with MIN < DN < MAX will be saved
 for use. If PERC < 100, these exterior points will be randomly weeded out
 until the proper percent remains.  These final points will be fed to subroutine
 EXTRAP, which interpolates.  For each point being interpolated, only exterior 
 points within a radius of RADI will be used in the formula. The use of RADI
 and PERC, can greatly enhance the speed of the algorithm for large areas. 
 However, a small PERC can give unrepresentative results for small areas. 
 MIN and MAX are important for painless interpolation. For example, if a large
 dark gore is to be removed, one can set MIN above the gore's DN. Now if one
 side of the polygon accidently crosses into the gore, the interpolation
 formula will ignore the border points collected from within the gore.

.PAGE
 EXECUTION:

 The following is the execution statement for SARGONB:

   SARGONB  INP  OUT  FUNC1 CONST1 [FUNC2 CONST2 VERT2 ...]

 where INP, OUT, FUNCi, CONSTi, and VERTi are parameters discussed in their
 respective parameter section in TUTOR mode.  

 Note that SARGONB does not use a SIZE parameter!  This is because the
 parameters were designed to allow all to be specified postitionally, with
 up to 25 groups of (FUNC, CONST, and VERT).  See EXAMPLES (below) for
 details.
.page
 EXAMPLES:

    SARGONB A B INTERP VERT1=(72,1,86,121,118,298)

 This example interpolates over a large area.  Note that INTERP
 has been specified positionally;  it is a string-valued parameter,
 not a keyword, so that 'INTERP would not be valid!  (After all,
 there are 25 parameters with INTERP as valid values, so 'INTERP
 would not be unique!)

    SARGONB A B INTERP -- (72,1,86,121,118,298)

 This is identical to the preceding operation, with all parameters
 specified positionally.  The "--" (null value) for the CONST1
 parameter indicates that the default values are to be used.
.page
    SARGONB A B ZERO 0 (100,100,200,200,100,200)

 This example zeroes out a triangular area.  Again, all parameters
 are specified positionally; the "0" was not necessary, since the
 ZERO value for FUNC1 does not need a value.  However, if that "0"
 was omitted, then the next parameter would have to be preceded by
 the parameter-name "VERT1=", since it would no longer be specified
 positionally (because the parameter following FUNC1 is CONST1).

    SARGONB A B SETTO 50 (20,20,20,200,40,200,40,20)

 This example sets a rectangular area to DN=50.  All parameters
 are specified postionally.
.page
    SARGONB A B ADD 10 (2,2,2,10,10,10,10,2) +
     mult 2 (81,12,81,20,90,20,90,12) +
     divide 3 (21,21,21,25,30,25,30,21) +
     setto 250 (31,42,31,47,41,47,41,42) +
     zero 0 (51,51,51,55,55,55,55,51)

 This example operates on 5 polygons, with all parameters specified
 postionally.  
.PAGE
 RESTRICTIONS:

	1. Maximum number of vertices for a given area is 25.

	2. Sides of any given polygon should not cross.
	
 	3. Maximum line length of input image is 10000 bytes.

        4. Maximum number of areas is 25.

 TIMING: None available for the VAX

 WRITTEN BY: 	CHARLES C. AVIS		20 OCTOBER 1982

 COGNIZANT PROGRAMMER: FLORANCE MOSS

 MADE PORTABLE FOR UNIX: CRI            01 JULY 1994

.LEVEL1
.VARIABLE INP
 An input data set
.VARIABLE OUT
 An output data set
.VARIABLE FUNC1
 Operation to be performed
 on the 1st polygon.
 Valid strings are: ADD,
 SUBTRACT,MULT,DIVIDE,
 SETTO,ZERO,INTERP.
.VARIABLE CONST1
 Constant(s) required by
 FUNC1.
.VARIABLE VERT1
 1st polygon to be operated on.
 3:25 pairs of real numbers.
.VARIABLE FUNC2
 Next operation, see FUNC1.
.VARIABLE CONST2
 Next constant, see CONST1.
.VARIABLE VERT2
 Next polygon, see VERT1.
.VARIABLE FUNC3
 Next operation, see FUNC1.
.VARIABLE CONST3
 Next constant, see CONST1.
.VARIABLE VERT3
 Next polygon, see VERT1.
.VARIABLE FUNC4
 Next operation, see FUNC1.
.VARIABLE CONST4
 Next constant, see CONST1.
.VARIABLE VERT4
 Next polygon, see VERT1.
.VARIABLE FUNC5
 Next operation, see FUNC1.
.VARIABLE CONST5
 Next constant, see CONST1.
.VARIABLE VERT5
 Next polygon, see VERT1.
.VARIABLE FUNC6
 Next operation, see FUNC1.
.VARIABLE CONST6
 Next constant, see CONST1.
.VARIABLE VERT6
 Next polygon, see VERT1.
.VARIABLE FUNC7
 Next operation, see FUNC1.
.VARIABLE CONST7
 Next constant, see CONST1.
.VARIABLE VERT7
 Next polygon, see VERT1.
.VARIABLE FUNC8
 Next operation, see FUNC1.
.VARIABLE CONST8
 Next constant, see CONST1.
.VARIABLE VERT8
 Next polygon, see VERT1.
.VARIABLE FUNC9
 Next operation, see FUNC1.
.VARIABLE CONST9
 Next constant, see CONST1.
.VARIABLE VERT9
 Next polygon, see VERT1.
.VARIABLE FUNC10
 Next operation, see FUNC1.
.VARIABLE CONST10
 Next constant, see CONST1.
.VARIABLE VERT10
 Next polygon, see VERT1.
.VARIABLE FUNC11
 Next operation, see FUNC1.
.VARIABLE CONST11
 Next constant, see CONST1.
.VARIABLE VERT11
 Next polygon, see VERT1.
.VARIABLE FUNC12
 Next operation, see FUNC1.
.VARIABLE CONST12
 Next constant, see CONST1.
.VARIABLE VERT12
 Next polygon, see VERT1.
.VARIABLE FUNC13
 Next operation, see FUNC1.
.VARIABLE CONST13
 Next constant, see CONST1.
.VARIABLE VERT13
 Next polygon, see VERT1.
.VARIABLE FUNC14
 Next operation, see FUNC1.
.VARIABLE CONST14
 Next constant, see CONST1.
.VARIABLE VERT14
 Next polygon, see VERT1.
.VARIABLE FUNC15
 Next operation, see FUNC1.
.VARIABLE CONST15
 Next constant, see CONST1.
.VARIABLE VERT15
 Next polygon, see VERT1.
.VARIABLE FUNC16
 Next operation, see FUNC1.
.VARIABLE CONST16
 Next constant, see CONST1.
.VARIABLE VERT16
 Next polygon, see VERT1.
.VARIABLE FUNC17
 Next operation, see FUNC1.
.VARIABLE CONST17
 Next constant, see CONST1.
.VARIABLE VERT17
 Next polygon, see VERT1.
.VARIABLE FUNC18
 Next operation, see FUNC1.
.VARIABLE CONST18
 Next constant, see CONST1.
.VARIABLE VERT18
 Next polygon, see VERT1.
.VARIABLE FUNC19
 Next operation, see FUNC1.
.VARIABLE CONST19
 Next constant, see CONST1.
.VARIABLE VERT19
 Next polygon, see VERT1.
.VARIABLE FUNC20
 Next operation, see FUNC1.
.VARIABLE CONST20
 Next constant, see CONST1.
.VARIABLE VERT20
 Next polygon, see VERT1.
.VARIABLE FUNC21
 Next operation, see FUNC1.
.VARIABLE CONST21
 Next constant, see CONST1.
.VARIABLE VERT21
 Next polygon, see VERT1.
.VARIABLE FUNC22
 Next operation, see FUNC1.
.VARIABLE CONST22
 Next constant, see CONST1.
.VARIABLE VERT22
 Next polygon, see VERT1.
.VARIABLE FUNC23
 Next operation, see FUNC1.
.VARIABLE CONST23
 Next constant, see CONST1.
.VARIABLE VERT23
 Next polygon, see VERT1.
.VARIABLE FUNC24
 Next operation, see FUNC1.
.VARIABLE CONST24
 Next constant, see CONST1.
.VARIABLE VERT24
 Next polygon, see VERT1.
.VARIABLE FUNC25
 Next operation, see FUNC1.
.VARIABLE CONST25
 Next constant, see CONST1.
.VARIABLE VERT25
 Next polygon, see VERT1.
.VARIABLE MIN
 REAL/INTEGER - OPTIONAL
 - Minimum DN
.VARIABLE MAX
 REAL/INTEGER - OPTIONAL
 - Maximum DN
.VARIABLE RADIUS
 REAL/INTEGER - OPTIONAL
 - Interpolation radius
.VARIABLE PERC
 REAL/INTEGER - OPTIONAL
 - % of border used
.VARIABLE DBUG
 KEYWORD - OPTIONAL
 - produce debug print
.LEVEL2
.VARIABLE INP
 INP=IN where IN is the input filename. Standard VICAR input dataset
 parameter. (one dataset)
.VARIABLE OUT
 OUT=OUTFILE where OUTFILE is the output filename. Standard VICAR output
 dataset parameter. (one dataset)
.VARIABLE FUNC1
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC1=ADD      An integer CONST1 is added to each interior pixel in the
 output.

 FUNC1=SUBTRACT An integer CONST1 is subtracted from each interior pixel
 in the output.

 FUNC1=MULT     Each interior pixel is multiplied by CONST1 in the output.

 FUNC1=DIVIDE   Each interior pixel is divided by CONST1 in the output. 

 FUNC1=SETTO    Each interior pixel is set to CONST1 in the output.

 FUNC1=ZERO     Each interior pixel is set to zero in the output.

 FUNC1=INTERP   An interpolation is performed over all interior points.
		CONST1 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST1
 Integer:      ADD  -     OUT=IN+CONST1
               SUBTRACT - OUT=IN-CONST1
               MULT -     OUT=IN*CONST1
               DIVIDE -   OUT=IN/CONST1
               SETTO -    OUT=CONST1
               ZERO -     OUT=0
               INTERP -   CONST1=(RADI,PERC,MIN,MAX)
.VARIABLE VERT1
 VERT1=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC2
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC2=ADD      An integer CONST2 is added to each interior pixel in the
 output.

 FUNC2=SUBTRACT An integer CONST2 is subtracted from each interior pixel
 in the output.

 FUNC2=MULT     Each interior pixel is multiplied by CONST2 in the output.

 FUNC2=DIVIDE   Each interior pixel is divided by CONST2 in the output. 

 FUNC2=SETTO    Each interior pixel is set to CONST2 in the output.

 FUNC2=ZERO     Each interior pixel is set to zero in the output.

 FUNC2=INTERP   An interpolation is performed over all interior points.
		CONST2 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST2
 Integer:      ADD  -     OUT=IN+CONST2
               SUBTRACT - OUT=IN-CONST2
               MULT -     OUT=IN*CONST2
               DIVIDE -   OUT=IN/CONST2
               SETTO -    OUT=CONST2
               ZERO -     OUT=0
               INTERP -   CONST2=(RADI,PERC,MIN,MAX)
.VARIABLE VERT2
 VERT2=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC3
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC3=ADD      An integer CONST3 is added to each interior pixel in the
 output.

 FUNC3=SUBTRACT An integer CONST3 is subtracted from each interior pixel
 in the output.

 FUNC3=MULT     Each interior pixel is multiplied by CONST3 in the output.

 FUNC3=DIVIDE   Each interior pixel is divided by CONST3 in the output. 

 FUNC3=SETTO    Each interior pixel is set to CONST3 in the output.

 FUNC3=ZERO     Each interior pixel is set to zero in the output.

 FUNC3=INTERP   An interpolation is performed over all interior points.
		CONST3 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST3
 Integer:      ADD  -     OUT=IN+CONST3
               SUBTRACT - OUT=IN-CONST3
               MULT -     OUT=IN*CONST3
               DIVIDE -   OUT=IN/CONST3
               SETTO -    OUT=CONST3
               ZERO -     OUT=0
               INTERP -   CONST3=(RADI,PERC,MIN,MAX)
.VARIABLE VERT3
 VERT3=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC4
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC4=ADD      An integer CONST4 is added to each interior pixel in the
 output.

 FUNC4=SUBTRACT An integer CONST4 is subtracted from each interior pixel
 in the output.

 FUNC4=MULT     Each interior pixel is multiplied by CONST4 in the output.

 FUNC4=DIVIDE   Each interior pixel is divided by CONST4 in the output. 

 FUNC4=SETTO    Each interior pixel is set to CONST4 in the output.

 FUNC4=ZERO     Each interior pixel is set to zero in the output.

 FUNC4=INTERP   An interpolation is performed over all interior points.
		CONST4 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST4
 Integer:      ADD  -     OUT=IN+CONST4
               SUBTRACT - OUT=IN-CONST4
               MULT -     OUT=IN*CONST4
               DIVIDE -   OUT=IN/CONST4
               SETTO -    OUT=CONST4
               ZERO -     OUT=0
               INTERP -   CONST4=(RADI,PERC,MIN,MAX)
.VARIABLE VERT4
 VERT4=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC5
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC5=ADD      An integer CONST5 is added to each interior pixel in the
 output.

 FUNC5=SUBTRACT An integer CONST5 is subtracted from each interior pixel
 in the output.

 FUNC5=MULT     Each interior pixel is multiplied by CONST5 in the output.

 FUNC5=DIVIDE   Each interior pixel is divided by CONST5 in the output. 

 FUNC5=SETTO    Each interior pixel is set to CONST5 in the output.

 FUNC5=ZERO     Each interior pixel is set to zero in the output.

 FUNC5=INTERP   An interpolation is performed over all interior points.
		CONST5 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST5
 Integer:      ADD  -     OUT=IN+CONST5
               SUBTRACT - OUT=IN-CONST5
               MULT -     OUT=IN*CONST5
               DIVIDE -   OUT=IN/CONST5
               SETTO -    OUT=CONST5
               ZERO -     OUT=0
               INTERP -   CONST5=(RADI,PERC,MIN,MAX)
.VARIABLE VERT5
 VERT5=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC6
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC6=ADD      An integer CONST6 is added to each interior pixel in the
 output.

 FUNC6=SUBTRACT An integer CONST6 is subtracted from each interior pixel
 in the output.

 FUNC6=MULT     Each interior pixel is multiplied by CONST6 in the output.

 FUNC6=DIVIDE   Each interior pixel is divided by CONST6 in the output. 

 FUNC6=SETTO    Each interior pixel is set to CONST6 in the output.

 FUNC6=ZERO     Each interior pixel is set to zero in the output.

 FUNC6=INTERP   An interpolation is performed over all interior points.
		CONST6 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST6
 Integer:      ADD  -     OUT=IN+CONST6
               SUBTRACT - OUT=IN-CONST6
               MULT -     OUT=IN*CONST6
               DIVIDE -   OUT=IN/CONST6
               SETTO -    OUT=CONST6
               ZERO -     OUT=0
               INTERP -   CONST6=(RADI,PERC,MIN,MAX)
.VARIABLE VERT6
 VERT6=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC7
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC7=ADD      An integer CONST7 is added to each interior pixel in the
 output.

 FUNC7=SUBTRACT An integer CONST7 is subtracted from each interior pixel
 in the output.

 FUNC7=MULT     Each interior pixel is multiplied by CONST7 in the output.

 FUNC7=DIVIDE   Each interior pixel is divided by CONST7 in the output. 

 FUNC7=SETTO    Each interior pixel is set to CONST7 in the output.

 FUNC7=ZERO     Each interior pixel is set to zero in the output.

 FUNC7=INTERP   An interpolation is performed over all interior points.
		CONST7 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST7
 Integer:      ADD  -     OUT=IN+CONST7
               SUBTRACT - OUT=IN-CONST7
               MULT -     OUT=IN*CONST7
               DIVIDE -   OUT=IN/CONST7
               SETTO -    OUT=CONST7
               ZERO -     OUT=0
               INTERP -   CONST7=(RADI,PERC,MIN,MAX)
.VARIABLE VERT7
 VERT7=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC8
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC8=ADD      An integer CONST8 is added to each interior pixel in the
 output.

 FUNC8=SUBTRACT An integer CONST8 is subtracted from each interior pixel
 in the output.

 FUNC8=MULT     Each interior pixel is multiplied by CONST8 in the output.

 FUNC8=DIVIDE   Each interior pixel is divided by CONST8 in the output. 

 FUNC8=SETTO    Each interior pixel is set to CONST8 in the output.

 FUNC8=ZERO     Each interior pixel is set to zero in the output.

 FUNC8=INTERP   An interpolation is performed over all interior points.
		CONST8 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST8
 Integer:      ADD  -     OUT=IN+CONST8
               SUBTRACT - OUT=IN-CONST8
               MULT -     OUT=IN*CONST8
               DIVIDE -   OUT=IN/CONST8
               SETTO -    OUT=CONST8
               ZERO -     OUT=0
               INTERP -   CONST8=(RADI,PERC,MIN,MAX)
.VARIABLE VERT8
 VERT8=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC9
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC9=ADD      An integer CONST9 is added to each interior pixel in the
 output.

 FUNC9=SUBTRACT An integer CONST9 is subtracted from each interior pixel
 in the output.

 FUNC9=MULT     Each interior pixel is multiplied by CONST9 in the output.

 FUNC9=DIVIDE   Each interior pixel is divided by CONST9 in the output. 

 FUNC9=SETTO    Each interior pixel is set to CONST9 in the output.

 FUNC9=ZERO     Each interior pixel is set to zero in the output.

 FUNC9=INTERP   An interpolation is performed over all interior points.
		CONST9 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST9
 Integer:      ADD  -     OUT=IN+CONST9
               SUBTRACT - OUT=IN-CONST9
               MULT -     OUT=IN*CONST9
               DIVIDE -   OUT=IN/CONST9
               SETTO -    OUT=CONST9
               ZERO -     OUT=0
               INTERP -   CONST9=(RADI,PERC,MIN,MAX)
.VARIABLE VERT9
 VERT9=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC10
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC10=ADD      An integer CONST10 is added to each interior pixel in the
 output.

 FUNC10=SUBTRACT An integer CONST10 is subtracted from each interior pixel
 in the output.

 FUNC10=MULT     Each interior pixel is multiplied by CONST10 in the output.

 FUNC10=DIVIDE   Each interior pixel is divided by CONST10 in the output. 

 FUNC10=SETTO    Each interior pixel is set to CONST10 in the output.

 FUNC10=ZERO     Each interior pixel is set to zero in the output.

 FUNC10=INTERP   An interpolation is performed over all interior points.
		CONST10 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST10
 Integer:      ADD  -     OUT=IN+CONST10
               SUBTRACT - OUT=IN-CONST10
               MULT -     OUT=IN*CONST10
               DIVIDE -   OUT=IN/CONST10
               SETTO -    OUT=CONST10
               ZERO -     OUT=0
               INTERP -   CONST10=(RADI,PERC,MIN,MAX)
.VARIABLE VERT10
 VERT10=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC11
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC11=ADD      An integer CONST11 is added to each interior pixel in the
 output.

 FUNC11=SUBTRACT An integer CONST11 is subtracted from each interior pixel
 in the output.

 FUNC11=MULT     Each interior pixel is multiplied by CONST11 in the output.

 FUNC11=DIVIDE   Each interior pixel is divided by CONST11 in the output. 

 FUNC11=SETTO    Each interior pixel is set to CONST11 in the output.

 FUNC11=ZERO     Each interior pixel is set to zero in the output.

 FUNC11=INTERP   An interpolation is performed over all interior points.
		CONST11 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST11
 Integer:      ADD  -     OUT=IN+CONST11
               SUBTRACT - OUT=IN-CONST11
               MULT -     OUT=IN*CONST11
               DIVIDE -   OUT=IN/CONST11
               SETTO -    OUT=CONST11
               ZERO -     OUT=0
               INTERP -   CONST11=(RADI,PERC,MIN,MAX)
.VARIABLE VERT11
 VERT11=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC12
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC12=ADD      An integer CONST12 is added to each interior pixel in the
 output.

 FUNC12=SUBTRACT An integer CONST12 is subtracted from each interior pixel
 in the output.

 FUNC12=MULT     Each interior pixel is multiplied by CONST12 in the output.

 FUNC12=DIVIDE   Each interior pixel is divided by CONST12 in the output. 

 FUNC12=SETTO    Each interior pixel is set to CONST12 in the output.

 FUNC12=ZERO     Each interior pixel is set to zero in the output.

 FUNC12=INTERP   An interpolation is performed over all interior points.
		CONST12 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST12
 Integer:      ADD  -     OUT=IN+CONST12
               SUBTRACT - OUT=IN-CONST12
               MULT -     OUT=IN*CONST12
               DIVIDE -   OUT=IN/CONST12
               SETTO -    OUT=CONST12
               ZERO -     OUT=0
               INTERP -   CONST12=(RADI,PERC,MIN,MAX)
.VARIABLE VERT12
 VERT12=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC13
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC13=ADD      An integer CONST13 is added to each interior pixel in the
 output.

 FUNC13=SUBTRACT An integer CONST13 is subtracted from each interior pixel
 in the output.

 FUNC13=MULT     Each interior pixel is multiplied by CONST13 in the output.

 FUNC13=DIVIDE   Each interior pixel is divided by CONST13 in the output. 

 FUNC13=SETTO    Each interior pixel is set to CONST13 in the output.

 FUNC13=ZERO     Each interior pixel is set to zero in the output.

 FUNC13=INTERP   An interpolation is performed over all interior points.
		CONST13 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST13
 Integer:      ADD  -     OUT=IN+CONST13
               SUBTRACT - OUT=IN-CONST13
               MULT -     OUT=IN*CONST13
               DIVIDE -   OUT=IN/CONST13
               SETTO -    OUT=CONST13
               ZERO -     OUT=0
               INTERP -   CONST13=(RADI,PERC,MIN,MAX)
.VARIABLE VERT13
 VERT13=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC14
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC14=ADD      An integer CONST14 is added to each interior pixel in the
 output.

 FUNC14=SUBTRACT An integer CONST14 is subtracted from each interior pixel
 in the output.

 FUNC14=MULT     Each interior pixel is multiplied by CONST14 in the output.

 FUNC14=DIVIDE   Each interior pixel is divided by CONST14 in the output. 

 FUNC14=SETTO    Each interior pixel is set to CONST14 in the output.

 FUNC14=ZERO     Each interior pixel is set to zero in the output.

 FUNC14=INTERP   An interpolation is performed over all interior points.
		CONST14 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST14
 Integer:      ADD  -     OUT=IN+CONST14
               SUBTRACT - OUT=IN-CONST14
               MULT -     OUT=IN*CONST14
               DIVIDE -   OUT=IN/CONST14
               SETTO -    OUT=CONST14
               ZERO -     OUT=0
               INTERP -   CONST14=(RADI,PERC,MIN,MAX)
.VARIABLE VERT14
 VERT14=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC15
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC15=ADD      An integer CONST15 is added to each interior pixel in the
 output.

 FUNC15=SUBTRACT An integer CONST15 is subtracted from each interior pixel
 in the output.

 FUNC15=MULT     Each interior pixel is multiplied by CONST15 in the output.

 FUNC15=DIVIDE   Each interior pixel is divided by CONST15 in the output. 

 FUNC15=SETTO    Each interior pixel is set to CONST15 in the output.

 FUNC15=ZERO     Each interior pixel is set to zero in the output.

 FUNC15=INTERP   An interpolation is performed over all interior points.
		CONST15 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST15
 Integer:      ADD  -     OUT=IN+CONST15
               SUBTRACT - OUT=IN-CONST15
               MULT -     OUT=IN*CONST15
               DIVIDE -   OUT=IN/CONST15
               SETTO -    OUT=CONST15
               ZERO -     OUT=0
               INTERP -   CONST15=(RADI,PERC,MIN,MAX)
.VARIABLE VERT15
 VERT15=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC16
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC16=ADD      An integer CONST16 is added to each interior pixel in the
 output.

 FUNC16=SUBTRACT An integer CONST16 is subtracted from each interior pixel
 in the output.

 FUNC16=MULT     Each interior pixel is multiplied by CONST16 in the output.

 FUNC16=DIVIDE   Each interior pixel is divided by CONST16 in the output. 

 FUNC16=SETTO    Each interior pixel is set to CONST16 in the output.

 FUNC16=ZERO     Each interior pixel is set to zero in the output.

 FUNC16=INTERP   An interpolation is performed over all interior points.
		CONST16 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST16
 Integer:      ADD  -     OUT=IN+CONST16
               SUBTRACT - OUT=IN-CONST16
               MULT -     OUT=IN*CONST16
               DIVIDE -   OUT=IN/CONST16
               SETTO -    OUT=CONST16
               ZERO -     OUT=0
               INTERP -   CONST16=(RADI,PERC,MIN,MAX)
.VARIABLE VERT16
 VERT16=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC17
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC17=ADD      An integer CONST17 is added to each interior pixel in the
 output.

 FUNC17=SUBTRACT An integer CONST17 is subtracted from each interior pixel
 in the output.

 FUNC17=MULT     Each interior pixel is multiplied by CONST17 in the output.

 FUNC17=DIVIDE   Each interior pixel is divided by CONST17 in the output. 

 FUNC17=SETTO    Each interior pixel is set to CONST17 in the output.

 FUNC17=ZERO     Each interior pixel is set to zero in the output.

 FUNC17=INTERP   An interpolation is performed over all interior points.
		CONST17 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST17
 Integer:      ADD  -     OUT=IN+CONST17
               SUBTRACT - OUT=IN-CONST17
               MULT -     OUT=IN*CONST17
               DIVIDE -   OUT=IN/CONST17
               SETTO -    OUT=CONST17
               ZERO -     OUT=0
               INTERP -   CONST17=(RADI,PERC,MIN,MAX)
.VARIABLE VERT17
 VERT17=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC18
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC18=ADD      An integer CONST18 is added to each interior pixel in the
 output.

 FUNC18=SUBTRACT An integer CONST18 is subtracted from each interior pixel
 in the output.

 FUNC18=MULT     Each interior pixel is multiplied by CONST18 in the output.

 FUNC18=DIVIDE   Each interior pixel is divided by CONST18 in the output. 

 FUNC18=SETTO    Each interior pixel is set to CONST18 in the output.

 FUNC18=ZERO     Each interior pixel is set to zero in the output.

 FUNC18=INTERP   An interpolation is performed over all interior points.
		CONST18 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST18
 Integer:      ADD  -     OUT=IN+CONST18
               SUBTRACT - OUT=IN-CONST18
               MULT -     OUT=IN*CONST18
               DIVIDE -   OUT=IN/CONST18
               SETTO -    OUT=CONST18
               ZERO -     OUT=0
               INTERP -   CONST18=(RADI,PERC,MIN,MAX)
.VARIABLE VERT18
 VERT18=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC19
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC19=ADD      An integer CONST19 is added to each interior pixel in the
 output.

 FUNC19=SUBTRACT An integer CONST19 is subtracted from each interior pixel
 in the output.

 FUNC19=MULT     Each interior pixel is multiplied by CONST19 in the output.

 FUNC19=DIVIDE   Each interior pixel is divided by CONST19 in the output. 

 FUNC19=SETTO    Each interior pixel is set to CONST19 in the output.

 FUNC19=ZERO     Each interior pixel is set to zero in the output.

 FUNC19=INTERP   An interpolation is performed over all interior points.
		CONST19 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST19
 Integer:      ADD  -     OUT=IN+CONST19
               SUBTRACT - OUT=IN-CONST19
               MULT -     OUT=IN*CONST19
               DIVIDE -   OUT=IN/CONST19
               SETTO -    OUT=CONST19
               ZERO -     OUT=0
               INTERP -   CONST19=(RADI,PERC,MIN,MAX)
.VARIABLE VERT19
 VERT19=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC20
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC20=ADD      An integer CONST20 is added to each interior pixel in the
 output.

 FUNC20=SUBTRACT An integer CONST20 is subtracted from each interior pixel
 in the output.

 FUNC20=MULT     Each interior pixel is multiplied by CONST20 in the output.

 FUNC20=DIVIDE   Each interior pixel is divided by CONST20 in the output. 

 FUNC20=SETTO    Each interior pixel is set to CONST20 in the output.

 FUNC20=ZERO     Each interior pixel is set to zero in the output.

 FUNC20=INTERP   An interpolation is performed over all interior points.
		CONST20 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST20
 Integer:      ADD  -     OUT=IN+CONST20
               SUBTRACT - OUT=IN-CONST20
               MULT -     OUT=IN*CONST20
               DIVIDE -   OUT=IN/CONST20
               SETTO -    OUT=CONST20
               ZERO -     OUT=0
               INTERP -   CONST20=(RADI,PERC,MIN,MAX)
.VARIABLE VERT20
 VERT20=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC21
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC21=ADD      An integer CONST21 is added to each interior pixel in the
 output.

 FUNC21=SUBTRACT An integer CONST21 is subtracted from each interior pixel
 in the output.

 FUNC21=MULT     Each interior pixel is multiplied by CONST21 in the output.

 FUNC21=DIVIDE   Each interior pixel is divided by CONST21 in the output. 

 FUNC21=SETTO    Each interior pixel is set to CONST21 in the output.

 FUNC21=ZERO     Each interior pixel is set to zero in the output.

 FUNC21=INTERP   An interpolation is performed over all interior points.
		CONST21 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST21
 Integer:      ADD  -     OUT=IN+CONST21
               SUBTRACT - OUT=IN-CONST21
               MULT -     OUT=IN*CONST21
               DIVIDE -   OUT=IN/CONST21
               SETTO -    OUT=CONST21
               ZERO -     OUT=0
               INTERP -   CONST21=(RADI,PERC,MIN,MAX)
.VARIABLE VERT21
 VERT21=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC22
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC22=ADD      An integer CONST22 is added to each interior pixel in the
 output.

 FUNC22=SUBTRACT An integer CONST22 is subtracted from each interior pixel
 in the output.

 FUNC22=MULT     Each interior pixel is multiplied by CONST22 in the output.

 FUNC22=DIVIDE   Each interior pixel is divided by CONST22 in the output. 

 FUNC22=SETTO    Each interior pixel is set to CONST22 in the output.

 FUNC22=ZERO     Each interior pixel is set to zero in the output.

 FUNC22=INTERP   An interpolation is performed over all interior points.
		CONST22 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST22
 Integer:      ADD  -     OUT=IN+CONST22
               SUBTRACT - OUT=IN-CONST22
               MULT -     OUT=IN*CONST22
               DIVIDE -   OUT=IN/CONST22
               SETTO -    OUT=CONST22
               ZERO -     OUT=0
               INTERP -   CONST22=(RADI,PERC,MIN,MAX)
.VARIABLE VERT22
 VERT22=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC23
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC23=ADD      An integer CONST23 is added to each interior pixel in the
 output.

 FUNC23=SUBTRACT An integer CONST23 is subtracted from each interior pixel
 in the output.

 FUNC23=MULT     Each interior pixel is multiplied by CONST23 in the output.

 FUNC23=DIVIDE   Each interior pixel is divided by CONST23 in the output. 

 FUNC23=SETTO    Each interior pixel is set to CONST23 in the output.

 FUNC23=ZERO     Each interior pixel is set to zero in the output.

 FUNC23=INTERP   An interpolation is performed over all interior points.
		CONST23 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST23
 Integer:      ADD  -     OUT=IN+CONST23
               SUBTRACT - OUT=IN-CONST23
               MULT -     OUT=IN*CONST23
               DIVIDE -   OUT=IN/CONST23
               SETTO -    OUT=CONST23
               ZERO -     OUT=0
               INTERP -   CONST23=(RADI,PERC,MIN,MAX)
.VARIABLE VERT23
 VERT23=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC24
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC24=ADD      An integer CONST24 is added to each interior pixel in the
 output.

 FUNC24=SUBTRACT An integer CONST24 is subtracted from each interior pixel
 in the output.

 FUNC24=MULT     Each interior pixel is multiplied by CONST24 in the output.

 FUNC24=DIVIDE   Each interior pixel is divided by CONST24 in the output. 

 FUNC24=SETTO    Each interior pixel is set to CONST24 in the output.

 FUNC24=ZERO     Each interior pixel is set to zero in the output.

 FUNC24=INTERP   An interpolation is performed over all interior points.
		CONST24 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST24
 Integer:      ADD  -     OUT=IN+CONST24
               SUBTRACT - OUT=IN-CONST24
               MULT -     OUT=IN*CONST24
               DIVIDE -   OUT=IN/CONST24
               SETTO -    OUT=CONST24
               ZERO -     OUT=0
               INTERP -   CONST24=(RADI,PERC,MIN,MAX)
.VARIABLE VERT24
 VERT24=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE FUNC25
 Valid strings are: ADD,SUBTRACT,MULT,DIVIDE,SETTO,ZERO,INTERP

 FUNC25=ADD      An integer CONST25 is added to each interior pixel in the
 output.

 FUNC25=SUBTRACT An integer CONST25 is subtracted from each interior pixel
 in the output.

 FUNC25=MULT     Each interior pixel is multiplied by CONST25 in the output.

 FUNC25=DIVIDE   Each interior pixel is divided by CONST25 in the output. 

 FUNC25=SETTO    Each interior pixel is set to CONST25 in the output.

 FUNC25=ZERO     Each interior pixel is set to zero in the output.

 FUNC25=INTERP   An interpolation is performed over all interior points.
		CONST25 contains: RADI, PERC, MIN, and MAX.  (See HELP *.)

.VARIABLE CONST25
 Integer:      ADD  -     OUT=IN+CONST25
               SUBTRACT - OUT=IN-CONST25
               MULT -     OUT=IN*CONST25
               DIVIDE -   OUT=IN/CONST25
               SETTO -    OUT=CONST25
               ZERO -     OUT=0
               INTERP -   CONST25=(RADI,PERC,MIN,MAX)
.VARIABLE VERT25
 VERT25=(R1,R2,R3,...,R25) where R1-R25 are floating point numbers defining
 the polygon to be operated on.  The polygon need not be closed.
 (The program will close it.)  All values for VERT must be positive.
 This parameter requires at least 3 pairs of values and at the most
 25 pairs of values.
.VARIABLE MIN
 MIN=I2 where I2 is an integer value. It sets minimum DN for interpolation.
 Default is MIN = -9999.
.VARIABLE MAX
 MAX=I3 where I3 is an integer value. It sets maximum DN for interpolation.
 Default is MAX = 32768.
.VARIABLE RADIUS
 RADIUS=I4 where I4 is an integer value. The interpolation radius is set
 to I4. Default is RADIUS = 200.
.VARIABLE PERC
 PERC=I5 where I5 is an integer value. It specifies % of border points
 used in interpolation. Default is PERC = 30.
.VARIABLE DBUG
 DBUG produces debug messages. Default is no debug messages.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsargonb.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!This is a test of SARGONB
!First, we will test all functions on byte data
!generate and show the input data
gen NL=12 NS=15
list INP=GEN
!Function : MULT=2  (Should double all DNs in the area)
sargonb GEN B MULT 2 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : DIV=2    (Should half all DNs in the area)
sargonb GEN B DIVIDE 2 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : ADD=50   (Should increase DN values in area by 50)
sargonb GEN B ADD 50 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : SUB=5   (Should decrease DN values in area by 5)
sargonb GEN B SUBTRACT 5 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : SET=255  (Should SET area to uniform 255 DN)
sargonb GEN B SETTO 255 (1,10,9,10,9,11,1,11)
list INP=B
!Function : ZERO  (Should ZERO out the area)
sargonb B D ZERO VERT1=(1,7,9,7,9,8,1,8)
list INP=D
!Function : INTERPOLATE  (Should INTERP for values within area)
sargonb D B INTERP (200,100) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Subfunction : MIN=1 MAX=254 (Should IGNORE values outside this range 
!while interpolating.)
sargonb D B INTERP (200,100,1,254) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Subfunction : PERC=60 (Should get APPROX same interpolation as with
!PERC=100 but using fewer points)
sargonb D B INTERP (200,60) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Subfunction : RADI=2 (Should INTERP using only border points within 
!2 pixels of this pixel)
sargonb D B INTERP (2,100) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!
!Now, do same stuff with HALFWORD data
gen NL=12 NS=15 'HALF OUT=HEN
list INP=HEN
!Function : MULT=2  (Should double all DNs in the area)
sargonb HEN B MULT 2 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : DIV=2    (Should half all DNs in the area)
sargonb HEN B DIVIDE 2 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : ADD=50   (should increase DN values in area by 50)
sargonb HEN B ADD 50 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : SUB=50   (Should decrease DN values in area by 50)
sargonb HEN B SUBTRACT 50 (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Function : SET=255  (Should SET area to uniform 255 DN)
sargonb HEN B SETTO 255 (1,10,9,10,9,11,1,11)
list INP=B
!Function : ZERO  (Should ZERO out the area)
sargonb B D ZERO VERT1=(1,7,9,7,9,8,1,8)
list INP=D
!Function : INTERPOLATE  (Should INTERP for values within area)
sargonb D B INTERP (200,100) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Subfunction : MIN=1 MAX=254 (Should ignore values outside this range 
!while interpolating)
sargonb D B INTERP (200,100,1,254) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Subfunction : PERC=60 (Should get approx same interpolation as with
!PERC=100 but using fewer points)
sargonb D B INTERP (200,60) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Subfunction : RADI=2 (Should INTERP using only border points within 
!2 pixels of this pixel)
sargonb D B INTERP (2,100) (5,5,8,8,5,10,5,13,2,8)
list INP=B
!Finally, test the new feature "multiple polygons"
gen A 100 100 IVAL=120 LINC=0 SINC=0
sargonb A B ADD 10 (2,2,2,10,10,10,10,2) MULT 2 (81,12,81,20,90,20,90,12)+
divide 3 (21,21,21,25,30,25,30,21) SETTO 250 (31,42,31,47,41,47,41,42)+
zero -- (51,51,51,55,55,55,55,51)
list B (2,2,9,9)
list B (81,12,10,9)
list B (21,21,10,5)
list B (31,42,11,6)
list B (51,51,5,5)
end-proc
$ Return
$!#############################################################################
