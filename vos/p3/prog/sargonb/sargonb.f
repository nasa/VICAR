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

