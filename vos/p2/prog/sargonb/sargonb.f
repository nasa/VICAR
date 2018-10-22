C-----PROGRAM SARGONB                   E,SARGONB,A,B,,PAR
C-----THIS PROGRAM WILL INTERPOLATE OVER ARBITRARY POLYGONS.
C-----IT CAN HANDLE UP TO 25 POLYGONS OF UP TO 25 VERTICES EACH.

C     05 AUG 02   ..KLEM...  "&998" change to "*998" (lines 210, 211, 442)
C                            to compile on LINUX.  Explicit type declaration
C                            for INDEX (warning) not addressed.
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
	subroutine main44
C
c     IMPLICIT INTEGER (A-Z)
	implicit none
c      INCLUDE 'fortport'
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
      COMMON/D/DBUG,ICODE,DNMIN,DNMAX
      COMMON/E/NL,NS
      COMMON/F/OUTUNIT

c	integer*2 PTBUF(3,40000),SEGM(3,20000),SORT(2,40000)
        integer*4 inunit,outunit,a,count,dbug,def,i,j,n
        integer*4 nl,ns,npts,narea,nv,nseg,radi,stat,icode
        integer*4 NVERT(25),FUNC(25)		!,ICONST(4,25),IRR(4)
	integer*4 SORT(2,40000), SEGM(3,40000)
        real*4 V(2,26,25),T,dnmin,dnmax
c        BYTE LINE(20000)
c
c	PTBUF is real because ptbuf(3,n) is real (DN value)
c	ptbuf(1,n)=line  ptbuf(2,n)=samp
c
c	SEGM is I*4 segm(1,n) = line, segm(2,n) = SS, segm(3,n) = ES
c
c	SORT is I*4 sort(1,m) = line, sort(2,n) = sample
c 
c	LINE(40000) is input and output line buffer
c	
c	V holds vertex points
c 
	real*4  LINE(40000),PTBUF(3,40000)
	real*4 ICONST(4,25),IRR(4)
c
        logical*4 XVPTST

	character*6 FUNCNAME(25),INCHAR
	character*7 CNSTNAME(25)
	character*6 VERTNAME(25)
	character*8 FBUF
	character*8 fmt(4)/'BYTE','HALF','FULL','REAL'/
        character*8 format


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
      DATA NVERT/25*0/,FUNC/25*0/,IRR/4*0.0/
      DATA SORT/80000*0/
      DATA V/1300*0.0/,T/0.0/
c      DATA LINE/20000*' '/
      DATA FBUF/'        '/
C      DATA XVPTST/0/, INDEX/0/
C
      CALL IFMESSAGE('SARGONB version 2016-05-26')

      DO  I=1,4
          DO  J=1,25
              ICONST(1,J)=0.0
	  ENDDO
      ENDDO
      do i=1,3
	do j=1,40000
	  ptbuf(i,j) = 0.0
	  segm(i,j) = 0
	enddo
      enddo
      do i=1,2
        do j=1,40000
          sort(i,j) = 0
        enddo
      enddo

C     OPEN INPUT
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(INUNIT,STAT,' ') 
C
C     FIND OUT FORMAT,NUMBER OF LINES,NUMBER OF SAMPLES
      CALL XVGET(INUNIT,STAT,'NL',NL,'NS',NS,'FORMAT',
     &           format,' ')

        icode = 0
        if (format.eq.'BYTE') icode=1
        if (format.eq.'HALF'.or.format.eq.'WORD') icode=2
        if (format.eq.'FULL') icode=3
        if (format.eq.'REAL') icode=4
        if (icode.eq.0) then
                call xvmessage('??E - Unknown data format for input image',' ')
                call abend  
        endif
        call xvclose(inunit,stat,' ')
        call xvopen(inunit,stat,'OPEN_ACT','SA','IO_ACT','SA',
     &          'I_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')            !FMT(INCODE),' ')


c      IF (BYTPIX .EQ. 1) THEN
c          ICOD=1
c      ELSE IF (BYTPIX .EQ. 2) THEN
c          ICOD=2
c      ELSE
c         CALL XVMESSAGE('??E - Wrong input format',' ')
c          CALL ABEND
c      END IF
C
C----------------------------------------------------------------------
C-----PROCESS PARAMETERS
C     param "DBUG"
      DBUG=0
      IF (XVPTST('DBUG')) DBUG=1
	if (icode.eq.1) then
	   dnmin=0.0
	   dnmax=255.0
	elseif (icode.eq.2) then
           dnmin=-32768.0
           dnmax=32767.0
	elseif (icode.eq.3) then
           dnmin=-999999999.
           dnmax=2147483647.
	else
           dnmin=-999999999.
           dnmax=2147483647.
	endif
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

      INCHAR=FUNCNAME(I)
      CALL XVPARM(INCHAR,FBUF,COUNT,DEF,1)
      IF (COUNT.EQ.0 .OR. DEF.EQ.1) GO TO 1
      NAREA=NAREA+1
      IF (INDEX(FBUF,'INTERP').ne.0 .OR. INDEX(FBUF,'interp').ne.0) THEN 
          FUNC(NAREA)=0			!interpolate
c max val of 32 bit int = 2,147,483,647
C					!DEFAULTS:
	  ICONST(1,NAREA) = 200.	!RADIUS
	  ICONST(2,NAREA) = 30.	!PERCENT
	  ICONST(3,NAREA) = dnmin	!DNMIN
	  ICONST(4,NAREA) = dnmax	!DNMAX
c
c	legal cnstnames are: ZERO,SETTO,MULT,ADD,SUBTRACT,DIVIDE
          CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	  IF (COUNT.GE.1) ICONST(1,NAREA) = IRR(1)
	  IF (COUNT.GE.2) ICONST(2,NAREA) = IRR(2)
	  IF (COUNT.GE.3) ICONST(3,NAREA) = IRR(3)
	  IF (COUNT.GE.4) ICONST(4,NAREA) = IRR(4)
      ELSEIF (INDEX(FBUF,'ZERO').ne.0 .OR. INDEX(FBUF,'zero').ne.0) THEN
          FUNC(NAREA)=6		!zero
      ELSEIF (INDEX(FBUF,'SETTO').ne.0 .OR. INDEX(FBUF,'setto').ne.0) 
     &        THEN
          FUNC(NAREA)=5		!setto
          CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
          ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'MULT').ne.0 .OR.INDEX(FBUF,'mult').ne.0) 
     &        THEN
          FUNC(NAREA)=1		!multiply
          CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	  ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'ADD').ne.0 .OR.INDEX(FBUF,'add').ne.0) THEN 
          FUNC(NAREA)=2		!add
          CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	  ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'SUBTRACT').ne.0 .OR.
     &	  INDEX(FBUF,'subtract').ne.0) THEN 
          FUNC(NAREA)=3		!subtract
          CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	  ICONST(1,NAREA) = IRR(1)
      ELSEIF (INDEX(FBUF,'DIVIDE').ne.0 .OR.
     &	  INDEX(FBUF,'divide').ne.0) THEN 
          FUNC(NAREA)=4		!divide
          CALL XVPARM(CNSTNAME(I),IRR,COUNT,DEF,4)
	  ICONST(1,NAREA) = IRR(1)
      ELSE
          CALL XVMESSAGE('??E - Illegal function string',' ')
          CALL ABEND
      END IF
      CALL XVPARM(VERTNAME(I),V(1,1,NAREA),COUNT,DEF,1300)
      IF (MOD(COUNT,2) .NE. 0) THEN 
          CALL XVMESSAGE('??E - Vertices have to be in pairs',' ')
          CALL ABEND
      ELSE IF (COUNT .LT. 6) THEN
          CALL XVMESSAGE('??E - Needs to be at least 3 pairs of vertices',' ')
          CALL ABEND
      END IF      
          NVERT(NAREA)=COUNT/2
      END DO
c
c
    1   continue
	RADI = RADI*RADI
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
      CALL XVUNIT(OUTUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUTUNIT,STAT,'OP','WRITE','O_FORMAT',fmt(icode),'U_FORMAT',fmt(4),' ')
C
      DO 110 I=1,NL
      CALL XVREAD(INUNIT,LINE,STAT,' ')
  110 CALL XVWRIT(OUTUNIT,LINE,STAT,' ')
C
C     CLOSE INPUT AND OUTPUT
      CALL XVCLOSE(INUNIT,STAT,' ')
      CALL XVCLOSE(OUTUNIT,STAT,' ')
C     UPDATE OUTPUT
      CALL XVOPEN(OUTUNIT,STAT,'OP','UPDATE','O_FORMAT',fmt(icode),
     +       'U_FORMAT',fmt(4),' ')

C
C----------------------------------------------------------------
C-----BEGIN PROCESSING FOR EACH AREA
      DO 1500 A=1,NAREA
         NV = NVERT(A)
C-----CHECK IF POLYGON IS COMPLETELY OUTSIDE OF IMAGE
         DO 1510 I = 1, NV
            IF (V(1,I,A) .LE. NS .AND. V(2,I,A) .LE. NL) GOTO 1520
 1510    CONTINUE
         CALL XVMESSAGE('??E - Polygon is completely outside of image',' ')
         CALL XVMESSAGE('**** ABEND',' ')
         CALL ABEND
 1520    CALL COLECT(A,NSEG,NPTS,FUNC(A),ICONST(1,A),*998)
         CALL OPRATE(NSEG,FUNC(A),ICONST(1,A),NPTS,*998)
 1500 CONTINUE
C-----------------------------------------------------------------
C
      CALL XVCLOSE(OUTUNIT,STAT,' ')
      RETURN
  998 continue
	call xvmessage('??E - Error return from COLECT or OPRATE',' ')
	CALL ABEND
      END
C
C*****************************************************************
C
      SUBROUTINE COLECT(A,NSEG,NPTS,XFUNC,CONST,*)
C-----THIS ROUTINE WILL COLLECT THE NECESSARY POINTS ABOUT THE
C-----PERIPHERY OF THE SPECIFIED POLYGON.
C
c      IMPLICIT INTEGER (A-Z)
	implicit none
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
      COMMON/D/DBUG,ICODE,DNMIN,DNMAX
      COMMON/E/NL,NS
      COMMON /CSEED/SEED
	INTEGER*8 SEED
	integer*4 a,nseg,npts,xfunc,nv,nx,sl,ss,el,es,ps,pl
	integer*4 i,ii,j,l,n,p,ll,mm,s,px,stl,nonl,nl,ns,mxl,mnl,mv
	integer*4 dbug,minl,maxl,weed,perc,icode
	integer*4 inc
	integer*4 NVERT(25),SORT(2,40000),PT(3,40000)		!,CONST(4)
	integer*4 SEGM(3,40000)
c	INTEGER*2 PTBUF(3,40000),SEGM(3,20000),PT(3,10000),SORT(2,40000)
	REAL*4 POINT(2),V(2,26,25),T,SLOPE,RANNUM
c
	real*4 CONST(4)
	real*4 LINE(40000),PTBUF(3,40000)
	real*4 dnmin,dnmax,radi
c
	CHARACTER*132 PBUF
c	BYTE LINE(20000)
	logical*4 INSIDE
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
	inc=0
	px=0
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
      IF(DBUG .EQ. 1) CALL PRNT(2,2*N,SORT,'Aft line.')
C--------------------------------------------------------------
C-----TRANSFORM INTERSECTION PTS INTO LINE SEGMENTS MAKING UP POLYGON
      MINL = SORT(1,1)
      MAXL = SORT(1,N)
      STL = 1
      IF(DBUG .EQ. 1) CALL PRNT(4,1,N,'No intrsctn pts.')
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
9900  FORMAT (' ',I2,' Intersection points for line ',I4)
         CALL XVMESSAGE(PBUF(2:37),' ')
         DO 200 LL=2,NONL,2
            PX = STL + LL - 1
            DO 200 MM=1,NV
              IF(SORT(1,PX).EQ.V(2,MM,A) .AND. SORT(2,PX).EQ.V(1,MM,A))
     +           GOTO 250
  200    CONTINUE
         CALL XVMESSAGE('??E - No even points were vertices',' ')
         CALL XVMESSAGE('??E - Pathological figure....TRY AGAIN',' ')
	call abend
c         RETURN 1
  250    MV = 2 * (N-PX)
         CALL MVE(4,MV,SORT(1,PX+1),SORT(1,PX),1,1)
         N = N - 1
         NONL = NONL - 1
         CALL XVMESSAGE('??I - Fixup successful',' ')
         CALL PRNT(4,1,NONL,'Pts on line.')
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
      CALL PRNT(4,1,NSEG,'No line segments.')
      CALL PRNT(2,3*NSEG,SEGM,'Segm buf.')
 2301 IF(XFUNC .NE. 0) RETURN
C
C-----------------------------------------------------------------
C  INTERPOLATE:  GET CONSTANTS
      RADI = CONST(1)
      PERC = IFIX(CONST(2))		!PERCENT
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
         IF(INSIDE(POINT,V(1,1,A),NV)) L = IFIX(T + 1. )
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
      IF(DBUG .EQ. 1) CALL PRNT(4,1,NPTS,'No pts total.')
C
c 1500 CONTINUE
      IF(DBUG .EQ. 1) CALL PRNT(2,3*NPTS,PTBUF,'Aft samp.')
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
1001      call rangen(seed,rannum)
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
        CALL MVE(4,3*NPTS,PT,PTBUF,1,1)
 1100 CONTINUE
      IF(DBUG .EQ. 1) CALL PRNT(4,1,NPTS,'Npts aft weed.')
C
C----------------------------------------------------------------
C-----SORT EXTERIOR POINTS BY LINE AND SAMPLE IN ORDER TO READ DNS
	do ii=1,npts
           sort(1,ii) = ifix(ptbuf(2,ii))
           sort(2,ii) = ifix(ptbuf(1,ii))
	enddo
c      CALL MVE(4,NPTS,PTBUF(2,1),SORT(1,1),3,2)
c      CALL MVE(4,NPTS,PTBUF(1,1),SORT(2,1),3,2)
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
      CALL MVE(4,3,PTBUF(1,NPTS),PTBUF(1,I),1,1)
      NPTS = NPTS - 1
      I = I - 1
      GO TO 600
  650 CONTINUE
C---------------------------------------------------------------------
      IF(NPTS .LT. 1) GO TO 997
      IF(DBUG .EQ. 0) RETURN
      CALL PRNT(4,1,NPTS,'Npts aft thresh.')
      CALL PRNT(2,NPTS*3,PTBUF,'PTBUF.')
C
C----------------------------------------------------------------
C
      RETURN
  997 CALL XVMESSAGE('??E - No pts in range min to max',' ')
  998 RETURN 1
      END
C
C**************************************************************
C
      SUBROUTINE OPRATE(NSEG,XFUNC,CONST,NPTS,*)
c      IMPLICIT INTEGER (A-Z)
	implicit none
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
      COMMON/D/DBUG,ICODE,DNMIN,DNMAX
      COMMON/F/OUTUNIT
c      INTEGER*4 CONST(4)
c	fil substitutes for INVERT and V, FIL2 substitutes for SORT
	integer*4 nvert(25),SORT(2,40000),SEGM(3,40000)
c      INTEGER*2 LINE(10000),SEGM(3,20000)
      INTEGER*4 XFUNC
	integer*4 dbug,i,n,l,ss,es,ns,npts,status
	integer*4 outunit,nseg,icode
c	integer*4 PTBUF(3,40000)
c
	real*4 LINE(40000),PTBUF(3,40000),CONST(4)
	real*4 ULIM,LLIM,r,dnmin,dnmax
	real*4 V(2,26,25)
C
c      DATA ULIM/2147483647.0/,LLIM/-999999999.0/
C
C-----------------------------------------------------------
C-----LOOP THROUGH ALL SEGMENTS
c max val of 32 bit int = 2,147,483,647
c      IF(ICOD .EQ. 1) GO TO 10
      LLIM = dnmin
      ULIM = dnmax
c   10 CONTINUE
C
C-----ZERO OUT LINE BUFFER
      IF(XFUNC .EQ. 6) THEN
	do i=1,40000
         LINE(i) = 0.0
	enddo
      ENDIF
C-----SET LINE BUFFER HALFWORDS TO CONST
      CALL MVE(7,40000,CONST(1),LINE,0,1)		!-6
C
      DO 1000 N=1,NSEG
         L = SEGM(1,N)
         SS = SEGM(2,N)
         ES = SEGM(3,N)
         NS = ES - SS + 1
         GO TO (810,810,810,810,900,900),XFUNC
C-----INTERPOLATE
	 R = CONST(1)*CONST(1)
         CALL REXTRAP(NPTS,L,SS,ES,PTBUF,LINE,R)
         GO TO 900
C-----ADD SUBTRACT MULTIPLY DIVIDE
  810    continue
         CALL XVREAD(OUTUNIT,LINE,STATUS,'LINE',L,'SAMP',SS,
     +   'NSAMPS',NS,' ')
         GO TO (820,840,880,830),XFUNC
C-----MULTIPLY
  820    DO 890 I=1,NS
            LINE(I) = LINE(I) * CONST(1)
            IF(LINE(I) .GT. ULIM) LINE(I) = ULIM
  890    IF(LINE(I) .LT. LLIM) LINE(I) = LLIM
         GO TO 900
C-----DIVIDE
  830    DO 891 I=1,NS
            IF (CONST(1) .EQ. 0) GOTO 970
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
970   CALL XVMESSAGE ('??E - Invalid CONST value.',' ')
c999   
	CALL XVMESSAGE ('***SARGONB task cancelled',' ')
      CALL ABEND
      END
C
C***************************************************************
C
      SUBROUTINE PTADD(L,S,NPTS)
C-----THIS ROUTINE WILL ADD THE NEW POINTS TO PTBUF OR SKIP THEM
C-----IF THE ARE ALREADY RECORDED THERE.
c      IMPLICIT INTEGER (A-Z)
	implicit none
c	fil substitutes for INVERT and V, FIL3 substitutes for seqm and sort
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
c      INTEGER*2 PTBUF(3,40000)
	integer*4 nvert(25),SORT(2,40000),SEGM(3,40000)
	integer*4 j,l,npts,s,d
c
	real*4 V(2,26,25)
	real*4 PTBUF(3,40000)
	real*4 LINE(40000)
C
      DO 90 J=1,NPTS
         D = IABS(L-(IFIX(PTBUF(2,J))))
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
c      IMPLICIT INTEGER (A-Z)
	implicit none
c	fil substitutes for INVERT and V, FIL3 substitutes for SEGM
      COMMON/C/NVERT,V,PTBUF,LINE,SORT,SEGM
      COMMON/D/DBUG,ICODE,DNMIN,DNMAX
      COMMON/F/OUTUNIT
      INTEGER*4 SORT(2,40000),SEGM(3,40000)
c      INTEGER*2 LINE(10000)
	integer*4 i,j,l,np,spt,minl,maxl,nonl,ifp,ilp,npts
	integer*4 sfp,slp,nb,s,samp,e,dbug,icode
	integer*4 outunit,status
	integer*4 NVERT(25)
c
	real*4 V(2,26,25),dnmin,dnmax
	real*4 LINE(40000)
	real*4 PTBUF(3,40000)
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
            CALL MVE(7,1,LINE(E),PTBUF(3,NP),1,1)
 1300    CONTINUE
         SPT = SPT +NONL
 2000 CONTINUE
      IF(NP .NE. NPTS) CALL XVMESSAGE('??E - No dns read ne npts',' ')
      RETURN
C  999 RETURN 1
      END
C
C*******************************************************************
C
	SUBROUTINE SORTX(BUF,N)
C-----THIS ROUTINE WILL SWAP THE HALFWORDS OF THE FULLWORD BUFFER
C-----SO THAT VAX WILL SORT LIKE THE IBM.
	implicit none
	integer*4 I,N,II
c        INTEGER*2 BUF(2,N)
	INTEGER*4 BUF(2,N)
c        INTEGER*2 BUFVAL1(40000), BUFVAL2(40000)
	INTEGER*4 BUFVAL1(40000), BUFVAL2(40000)
        INTEGER*4 BUFVAL3(40000)
        INTEGER*4 BUFNDX1(40000)
C
        DO 100 I=1,N
          BUFVAL3(I) = ((BUF(1,I)*32768) + BUF(2,I))
          BUFVAL1(I) = BUF(1,I)
          BUFVAL2(I) = BUF(2,I)
          BUFNDX1(I) = I
100     CONTINUE
c
c	isortp is I*4, ssortp is R*4
        CALL ISORTP(BUFVAL3,1,N,BUFNDX1)

        DO 200 I=1,N
          II = BUFNDX1(I)
          BUF(1,I) = BUFVAL1(II)
          BUF(2,I) = BUFVAL2(II)
200     CONTINUE

C	INTEGER*2 BUF(2,N),J
C
C	DO 100 I=1,N
C	   J = BUF(1,I)
C	   BUF(1,I) = BUF(2,I)
C	   BUF(2,I) = J
C100	CONTINUE
C
C	CALL ISORT(BUF, 1, N)
C
C	DO 200 I=1,N
C	   J = BUF(1,I)
C	   BUF(1,I) = BUF(2,I)
C	   BUF(2,I) = J
C200	CONTINUE
C
	RETURN
	END
c***************************************************************
      SUBROUTINE REXTRAP(N,L,SS,ES,PTS,BUF,RM)
C
C-----THIS IS A FLOATING POINT VERSION OF EXTRAP FROM SUBLIB
c	
c      IMPLICIT INTEGER (A-Z)
	implicit none
      INTEGER*4 I,J,K,L,N,S,SS,ES
      REAL*4 R,R1,R2,NOOM,DENO,RMAX
c      INTEGER*2 PTS(3,N),BUF(1)	- BUF(1) to BUF(*) 2013-09-02
      REAL*4 PTS(3,N),BUF(*),RM
C
      RMAX = RM
      K = 0
C
      DO 10 J=SS,ES
      K = K + 1
      BUF(K) = 0.0
      NOOM = 0.0
      DENO = 0.0
        DO 5 I=1,N
        S = I
        R1 = J - PTS(1,I)
        R2 = L - PTS(2,I)
        R = R1**2 + R2**2
        IF(R .EQ. 0.0) GO TO 6
        IF(R .GT. RMAX) GO TO 5
        NOOM = NOOM + PTS(3,I) / R
        DENO = DENO + 1./R
    5   CONTINUE
C
      IF(NOOM .EQ. 0.0 .OR. DENO .EQ. 0.0) GO TO 10
      BUF(K) = NOOM / DENO + 0.5
      GO TO 10
    6 BUF(K) = PTS(3,S)
   10 CONTINUE
C
      RETURN
      END



