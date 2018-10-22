C*********************GEOMA**************************
C      2 AUG 02   ..KLEM..  LINUX had issues with precision of variables
C                           passed from GSUBH to GET.  LBUFBYT and LBUFINT
C                           added and EQUIVALENCED to BIGBUF so program
C                           would compile & work on HALFWORD images.
C     13 Nov 95   ...SP...  Discovered that really SGI was unhappy about
C                           bug in GET.  Added check in Subroutine GET to
C                           see if a line buffer was previously used before
C                           zeroing the old table entry.
C        AUG 95   ...SP...  Testing with halfword VENUS.IMG showed a need for
C                           double precision ATAB, RFRAC,PRFAC,....  Added
C                           SAVE statement to keep SGI happy.
C        JUL 95   ...SP...  Ported for UNIX and converted to accept IBIS 
C                           tiepoint file as second input.  Deleted dead code
C                           associated with RESTART.
C      1 JUL 94   ...JT...  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C     17 SEP 87   ...SP...  Corrected error handling for DGELG to abort if
C                           matrix is singular.
C     17 SEP 87   ...SP...  Corrected WORK array location for the case when
C                           tiepoints come from second input file.
C     12 MAY 87   ...SP...  Removed limit of 1250 tiepoints by making WORK
C                           array location dependent on number of tiepoints.
C      3 DEC 86   ...FFM... Modify PDF (Change the count of param TIEPOINT
C                           from 1:64 to 1:500 )
C     10 FEB 86   ...FFM... Minor modifications of the test file to fix
C                           the FR 16750.
C                           Disable the keyword "RESTART".
C      8 APR 85   ...HBD... Convert to Vicar2 & fix integer overflow FR
C                    Should one day be rewritten to eliminate BIGBUF
C      5 AUG 83   ...HBD... CONVERT GSUB AND GSUBH TO FORTRAN
C     11 FEB 81   ...JBS... CONVERT TO INTEGER INTENSITY INTERPOLATION
C     11 FEB 81   ...JBS... ALLOW NEGATIVE DATA
C     24 MAY 79   ...JDA... ADDED KEYWORDS 'RESTART' AND 'NOTIFY'
C     12 FEB 79   ...SKL... RELEASE 2K CORE FOR DUMP CAPABILITY
C     27 JUNE 75  ...DAH... CHANGES FOR CONVERSION TO 360/OS
C     19 JUNE 76  ...KRN... INITIAL RELEASE
C
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44

!     Common storage definitions
      REAL    ISKIP(4), FLN, FSM, FSMT
      DOUBLE PRECISION LARGE, SMALL
      INTEGER LN1, ONS, NOTE, NS,  INDEX, IUSE, IDEL, IMAX,
     &        NLO, NSO,  NLI, NSI,   NI,   NO, IR,
     &        TYPE, IMG, OUTFIL, SL, SS
      LOGICAL HALF

!     Common storage declarations

      COMMON /C1/ LN1,NOTE,NS,FLN,FSM,FSMT,LARGE,SMALL
      COMMON /C1/ INDEX,IUSE,IDEL,IMAX
      COMMON /C1/ ISKIP,SL,SS,NLO,NSO,NLI,NSI,NI,NO,TYPE,HALF,ONS,IR
      COMMON /FILES/ IMG, OUTFIL
      SAVE /C1/,/FILES/

      INTEGER STAT

C===================================================================

      CALL IFMESSAGE('GEOMA version 13-Nov-95')
C
C **** Open input and output data sets
      CALL XVUNIT(IMG,'INP',1,STAT,' ')
      CALL XVOPEN(IMG,STAT,'IO_ACT','SA','OPEN_ACT','SA',' ')
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
       IF (NLI .GT. 32767)     ! SEE COMMENT IN SUBROUTINE GET.
     .    CALL MABEND('ERROR: NLI TOO BIG')
      CALL XVUNIT(OUTFIL,'OUT',1,STAT,' ')
      CALL XVOPEN(OUTFIL,STAT,'IO_ACT','SA','OPEN_ACT','SA','OP',
     +      'WRITE','U_NL',NLO,'U_NS',NSO,' ')
      CALL XVPCNT('INP',NI)
      CALL XVPCNT('OUT',NO)
C
C  **** Call main routine to process parameters
      CALL MAIN
      RETURN
      END
C
C
      SUBROUTINE MAIN
C
      INTEGER NAH,NAV

      INTEGER*2 BIGBUF(700000)
      INTEGER PARMFIL, TCNT, IPAR(1), TPTR, STAT, CNT, DEF,
     .        IPARNUM
      LOGICAL XVPTST
      CHARACTER*5 FORMAT
      EQUIVALENCE (BIGBUF,IPAR)

      INTEGER LBUFINT(1)
      BYTE  LBUFBYT(1)
      EQUIVALENCE (BIGBUF, LBUFINT, LBUFBYT)

!     Common storage definitions
      REAL    ISKIP(4), FLN, FSM, FSMT
      DOUBLE PRECISION LARGE, SMALL
      INTEGER LN1, ONS, NOTE, NS,  INDEX, IUSE, IDEL, IMAX,
     &        NLO, NSO,  NLI, NSI,   NI,   NO, IR,
     &        TYPE, IMG, OUTFIL, SL, SS
      LOGICAL HALF

!     Common storage declarations

      COMMON /C1/ LN1,NOTE,NS,FLN,FSM,FSMT,LARGE,SMALL
      COMMON /C1/ INDEX,IUSE,IDEL,IMAX
      COMMON /C1/ ISKIP,SL,SS,NLO,NSO,NLI,NSI,NI,NO,TYPE,HALF,ONS,IR
      COMMON /FILES/ IMG, OUTFIL
      SAVE /C1/,/FILES/

      DATA  NAH/0/, NAV/0/
C
      DO I = 1, 4
        ISKIP(I) = 0
      END DO
      LN1 = 0
      NOTE = 0
      LARGE=.99D0
      SMALL=.01D0
      NBBUF = 700000      ! number of halfwords in bigbuf

C Process parameters
      CALL XVGET(IMG,STAT,'FORMAT',FORMAT,' ')
      IF (FORMAT .EQ. 'BYTE') THEN
         ONS = NSO
         HALF = .FALSE.
         CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS BYTE',' ')
      ELSE IF (FORMAT .EQ. 'HALF'.or. format .eq. 'WORD') THEN
         HALF = .TRUE.
         NSI = NSI * 2
         ONS = NSO
         NSO = NSO * 2
         CALL XVMESSAGE('SYSTEM LABEL SAYS INPUT IS HALFWORD',' ')
      ELSE
         CALL XVMESSAGE ('*** DATA FORMAT ERROR. ABEND.',' ')
         CALL ABEND
      ENDIF

C  Read parameters from disk, if there be any

      NS = ONS
      IF (NI .EQ. 1) GO TO 10

      CALL XVUNIT(PARMFIL,'INP',2,STAT,' ')

C...READ TIEPOINT INFO FROM IBIS TIEPOINT FILE.

      CALL IREAD_TIEPOINTS(PARMFIL,NAH,NAV,10000,IPAR,4)
      IF (NAH .EQ. 0)  GOTO 999

      TPTR = 1
      NTIEP = 4 * (NAH+1) * (NAV+1)
      IF (NTIEP .GT. 10000) GO TO 999
      GOTO 20

10    CONTINUE
C  Number of areas horizontally and vertically
      CALL XVPARM('NAH',NAH,CNT,DEF,0)
      CALL XVPARM('NAV',NAV,CNT,DEF,0)

C  Tiepoints
      CALL XVPARM('TIEPOINT',IPAR,TCNT,DEF,0)
      NTIEP = 4 * (NAH+1) * (NAV+1)
      TPTR = 1
      IF (NTIEP .NE. TCNT) GO TO 999

20    CONTINUE
C
C  No interpolation between samples.  Use nearest sample.
C
      IF (XVPTST('NOINTERP')) THEN
         LARGE=0.5D0
         SMALL=0.5D0
      END IF
      CALL XVPARM('NOTIFY',NOTE,CNT,DEF,0)
      IF (NOTE .LE. 0)  NOTE = 101
30    IF (NAV.LE.0.OR.NAH.LE.0.OR.(NAV+1)*(NAH+1).LE.0) GO TO 999
      MAXBUF = NBBUF-(53*NAV*NAH+4*NAH+2*NAV+1)/2*2

C     End of parameter processing
C  ALLOCATE SPACE IN BIGBUF FOLLOWING TIEPOINT DATA

      IPARNUM = NTIEP

      CALL MAIN2(BIGBUF(MAXBUF+1),BIGBUF(MAXBUF+1),
     *BIGBUF(MAXBUF+22*2*NAV*NAH+1),BIGBUF(MAXBUF+NAH*(52*NAV+4)+1),
     *BIGBUF(NBBUF-NAV*NAH+1),NAH,NAV,TPTR,MAXBUF,IPAR,BIGBUF,
     *BIGBUF,BIGBUF(2*IPARNUM +1),BIGBUF(2*IPARNUM+257),
     *LBUFINT,LBUFBYT)
      RETURN

  999 CALL XVMESSAGE ('*** ERROR IN NAH, NAV, OR TIEPOINT.',' ')
      CALL XVMESSAGE ('*** ABEND.',' ')
      CALL ABEND
      END


      SUBROUTINE MAIN2(ATAB,IATAB,SEGTAB,LSTART,TRYTAB,NAH,NAV,IP,
     *MAXBUF,PAR,BIGBUF,LBUF,WORK,TAB,LBUFINT,LBUFBYT)
C
!     Common storage definitions
      REAL    ISKIP(4), FLN, FSM, FSMT
      DOUBLE PRECISION LARGE, SMALL
      INTEGER LN1, ONS, NOTE, NS,  INDEX, IUSE, IDEL, IMAX,
     &        NLO, NSO,  NLI, NSI,   NI,   NO, IR,
     &        TYPE, IMG, OUTFIL, SL, SS
      LOGICAL HALF

!     Common storage declarations

      COMMON /C1/ LN1,NOTE,NS,FLN,FSM,FSMT,LARGE,SMALL
      COMMON /C1/ INDEX,IUSE,IDEL,IMAX
      COMMON /C1/ ISKIP,SL,SS,NLO,NSO,NLI,NSI,NI,NO,TYPE,HALF,ONS,IR
      COMMON /FILES/ IMG, OUTFIL
      SAVE /C1/,/FILES/

C.. ATAB and IATAB are equivalenced in that they are given the same address
C.. in the call to this routine.  How do you equivalence a real*8 array and an
C.. integer array: double the dimension of IATAB 22=2*11 AND MULTIPLY
CCC indices by two.


      REAL SEGTAB(2,NAH*(2*NAV+1)),  LSTART(*)
      REAL PAR(*)
      DOUBLE PRECISION ATAB(11,*),WORK(8,8),TAB(8),X
      INTEGER*2 BIGBUF(700000),VSEG,AV,AH,ROW
      INTEGER IATAB(22,*), NSAMPS, LINE
      INTEGER*2 TRYTAB(*)
      INTEGER NEITAB(4),AREA,SAMP,STAT
      BYTE LBUF(*)
      CHARACTER*132 MESSAGE
      INTEGER IPAR(10)

      INTEGER LBUFINT(*)
      BYTE  LBUFBYT(*)

      DATA NEITAB/2,4,1,3/
      DATA  IPAR/10*0/, NAREA/0/, I/0/, J/0/, K/0/, JTEMP/0/
      DATA  NRANK/0/,  J2/0/,     K2/0/, IER/0/, INDEX1/0/
      DATA  INDEX2/0/, INDEXA/0/, KNOTE/0/, INDX/0/
      DATA  LSAMP/0/,  SAMP/0/,   LSAMP1/0/, LSAMP2/0/
      DATA  JAREA/0/,  LSAMP3/0/
      DATA  X/0.0D0/
C
C      IF (ISKIP(3) .GT. 0) NLI=ISKIP(3)
C      IF (ISKIP(4) .GT.0) NSI=ISKIP(4)
C      IPAR(5) = NLI
C      IPAR(6) = NSI
      NAREA = NAH * NAV
C  SET UP TABLES
      DO 50 AV = 1, NAV
      DO 50 AH = 1, NAH
         AREA = AH + (AV-1) * NAH
C  CLEAR WORK ARRAY
         DO J = 1, 8
            DO K = 1, 8
               WORK(K,J) = 0.D0
            END DO
         END DO
         DO K = 1, 2				! Set up tiepoint matrix
            DO J = 1, 2
               I = 4 * (AH+J+(AV+K-2)*(NAH+1)) + IP - 8
               ROW = 4 * K + 2 * J - 5
               WORK(ROW,1) = PAR(I+1)
               WORK(ROW,2) = PAR(I)
               WORK(ROW+1,4) = WORK(ROW,1)
               WORK(ROW+1,5) = WORK(ROW,2)
               WORK(ROW,7) = WORK(ROW,1)*WORK(ROW,2)
               WORK(ROW+1,8) = WORK(ROW,7)
               WORK(ROW,3) = 1.
               WORK(ROW+1,6) = 1.
               TAB(ROW) = PAR(I+3) - WORK(ROW,1)
               TAB(ROW+1) = PAR(I+2) - WORK(ROW,2)
            END DO
         END DO
         JTEMP = AH + NAH * (AV-1)
         IATAB(2*9,AREA) = JTEMP
         IF (AV .GT. 1) IATAB(2*11,AREA-NAH)=IATAB(2*9,AREA)
         X = WORK(3,1) - WORK(1,1)
         IF (X .EQ. 0.0D0) THEN
           SEGTAB(1,JTEMP)=0.0
           SEGTAB(2,JTEMP)=WORK(1,2)
         ELSE
            SEGTAB(1,JTEMP)=(WORK(3,2)-WORK(1,2))/X
            SEGTAB(2,JTEMP)=(WORK(3,1)*WORK(1,2)-WORK(3,2)*WORK(1,1))/X
         ENDIF
         IF(AV .EQ. 1) THEN
            SEGTAB(1,JTEMP)=0.
            SEGTAB(2,JTEMP)=1.
         ENDIF
         JTEMP = NAH * (NAV+1) + AREA
         IATAB(2*10,AREA) = JTEMP
         X = WORK(7,2) - WORK(3,2)
         IF(X .EQ. 0.0D0) THEN
            SEGTAB(1,JTEMP) = 0.0
            SEGTAB(2,JTEMP) = WORK(3,1)
         ELSE
            SEGTAB(1,JTEMP)=(WORK(7,1)-WORK(3,1))/X
            SEGTAB(2,JTEMP)=(WORK(7,2)*WORK(3,1)-WORK(7,1)*WORK(3,2))/X
         ENDIF
         IF(AH .EQ. NAH) THEN
            SEGTAB(1,JTEMP) = 0.0
            SEGTAB(2,JTEMP) = NS
         ENDIF
         NRANK = 8
         DO 474 J = 1, 4		! Determine if area is a triangle or
           K = NEITAB(J)		! a rectangle
           J2 = 2 * J - 1
           K2 = 2 * K - 1
           DO 472 I = 1, 2
              IF(WORK(J2,I).NE.WORK(K2,I)) GO TO 474
 472       CONTINUE
           NRANK = 6
           IF (J.EQ.4 .OR. K.EQ.4) GO TO 473
           DO I = 1,8
              WORK(J2,I) = WORK(7,I)
              WORK(J2+1,I) = WORK(8,I)
           END DO
           TAB(J2) = TAB(7)
           TAB(J2+1) = TAB(8)
473        TAB(7) = 0.D + 0
           TAB(8) = 0.D + 0
           GO TO 478
 474    CONTINUE
 478    CALL DARRAY(2,NRANK,NRANK,8,8,WORK,WORK) 	! Determine coefficients
        CALL DGELG(TAB,WORK,NRANK,1,1.E-14,IER)
        DO ROW = 1, 8
           ATAB(ROW,AREA) = TAB(ROW)
        END DO
        IF (IER .NE. 0) GO TO 998
48      CONTINUE
        ATAB(1,AREA) = ATAB(1,AREA) + 1.D0
        ATAB(5,AREA) = ATAB(5,AREA) + 1.D0
        IATAB(2*11,AREA) = AH + NAH * AV
C
C
50    CONTINUE   !END OF NAH, NAV LOOPS

      DO AH = 1, NAH
         JTEMP = AH + NAH * NAV
         IATAB(2*11,NAREA+1-AH) = JTEMP
         SEGTAB(1,JTEMP) = 0.
         SEGTAB(2,JTEMP) = NLO
      END DO
C Set up LSTART table
      DO AV = 1, NAV
         JTEMP = 1 + NAH * AV
         LSTART(AV) = SEGTAB(1,JTEMP) + SEGTAB(2,JTEMP)
      END DO
C
C  Set up buffers to maximize space used
C
C  Clear buffers to zero
      DO I = 1, MAXBUF
         BIGBUF(I) = 0
      END DO
      IUSE = 4 * (NLI+1)	! IUSE-First byte in buffer after line table
      IDEL = ((NSI+3)/2) * 2	! IDEL-NO. bytes needed for each input buffer
      INDEX2 = MAXBUF - NSO	! INDEX2-Last halfwd available for input 
                                ! buffers, allowing 2 output buffers at end of
                                ! BIGBUF
      INDEX1 = 2 * INDEX2	! INDEX1-Last byte available for input buffers
      INDEX = INDEX1
      INDEXA = INDEX1 + NSO	! INDEXA-Last byte of first output buffer
      IMAX = INDEX1 - IDEL	! IMAX-Test value used to determine when last 
      IF (LN1 .LE. 0)  LN1 = 1	! available input buffer has been used
      KNOTE = NOTE
      VSEG = 1

      DO 140 LINE = LN1, NLO   !<<  OUTPUT LOOP
         SAMP = 1
  100    IF (LINE .LE. LSTART(VSEG)) GO TO 110
         VSEG = VSEG + 1
         GO TO 100
  110    AREA = NAH * (VSEG-1) + 1
  115    DO I = 1, NAREA	! Clear 'TRYTAB'  -  Area Trial Table
            TRYTAB(I) = 0
         END DO
C     Test if we are really in this area
C
C     Are we above the bottom line?
  120    K = 11*2
         IF (AREA.LE.NAREA-NAH .AND.
     *    LINE.GT.SEGTAB(1,IATAB(K,AREA))*SAMP+SEGTAB(2,IATAB(K,AREA)))
     1    GO TO 300
C     Are we below the top line?
         K = 9*2
         IF (AREA.GT.NAH .AND.
     *    LINE.LT.SEGTAB(1,IATAB(K,AREA))*SAMP+SEGTAB(2,IATAB(K,AREA)))
     1    GO TO 300
C     Are we left of the right line?
         K = 10*2
         IF (MOD(AREA,NAH).NE.0 .AND.
     *    SAMP.GT.SEGTAB(1,IATAB(K,AREA))*LINE+SEGTAB(2,IATAB(K,AREA)))
     1    GO TO 300
C     We are now in the right area
C  Predict next area (JAREA is increment to add to area)
C  LSAMP1 is intersection of line and top border
         INDX = IATAB(9*2,AREA)
         IF (SEGTAB(1,INDX ) .NE. 0.0) GO TO 125
         LSAMP1 = NS
         GO TO 126
125      LSAMP1 = (LINE-SEGTAB(2,INDX )) / SEGTAB(1,INDX )
126      INDX = IATAB(11*2,AREA)
C LSAMP3 is intersection of line and bottom border
         IF (SEGTAB(1,INDX ) .NE. 0.0) GO TO 127
         LSAMP3 = NS
         GO TO 128
127      LSAMP3 = (LINE-SEGTAB(2,INDX )) / SEGTAB(1,INDX )
128      INDX = IATAB(10*2,AREA)
C  LSAMP2 is intersection of line and right border
         LSAMP2 = LINE * SEGTAB(1,INDX ) + SEGTAB(2,INDX )
         IF (LSAMP1 .LT. SAMP) LSAMP1 = NS
         IF (LSAMP2 .LT. SAMP) LSAMP2 = NS
         IF (LSAMP3 .LT. SAMP) LSAMP3 = NS
         IF (LSAMP2.LE.LSAMP1.OR.LSAMP3.LE.LSAMP1) GO TO 1281
C  LSAMP1 is minumum.  Next move is up
         LSAMP = LSAMP1
         JAREA = -NAH
         GO TO 129
1281     IF (LSAMP1.LT.LSAMP2 .OR. LSAMP3.LE.LSAMP2) GO TO 1282
C  LSAMP2 is minumum.  Next move is right unless we wre on the right
C  border
         LSAMP = LSAMP2
         JAREA = 1
         IF (MOD(AREA,NAH) .EQ. 0) JAREA = 1 - NAH
         GO TO 129
C  LSAMP3 is minimum.  Next move is down
1282     LSAMP = LSAMP3
         JAREA = NAH
129   CONTINUE
C
C   *****CALL GSUB FOR BYTE FORMAT AND GSUBH FOR HALFWORD FORMAT
C
130   NSAMPS = LSAMP - SAMP + 1
      IF (HALF) THEN
         CALL GSUBH(LINE,SAMP,NSAMPS,ATAB(1,AREA),BIGBUF,LBUF,
     *LBUFBYT,LBUFINT)
      ELSE
         CALL GSUB(LINE,SAMP,NSAMPS,ATAB(1,AREA),BIGBUF,LBUF)
      ENDIF
C
         SAMP = LSAMP + 1
         IF (SAMP.GT.NS   ) GO TO 135
         AREA = AREA + JAREA
C  If area is out of range, begin area-by-area search
         IF (AREA.LT.1.OR.AREA.GT.NAREA) GO TO 380
         GO TO 115
  135    CALL XVWRIT (OUTFIL,LBUF(INDEX1+1),STAT,'NSAMPS',ONS,' ')
C  Switch output line indices in INDEX1 and INDEXA
         INDX = INDEX1
         INDEX1 = INDEXA
         INDEXA = INDX
         INDEX = INDEX1
         IF ((100*LINE)/NLO .LT. KNOTE)  GO TO 140

         WRITE (MESSAGE, 77000) KNOTE
77000    FORMAT (' GEOMA',I3,' PERCENT COMPLETED')
         CALL XVMESSAGE (MESSAGE,' ')
         KNOTE = KNOTE + NOTE
         GO TO 140  !!! Branch around embedded code 

  300 TRYTAB(AREA) = 1
      K = (K/2) - 8
      GO TO (310,315,320),K
C  WE ARE BELOW CURRENT AREA.  IF CURRENT AREA IS ON BOTTOM LINE
C  WE HAVE AN ERROR.
320   IF (AREA.GT.NAREA-NAH) GO TO 390
      IF (TRYTAB(AREA+NAH).EQ.1) GO TO 330
      AREA = AREA + NAH
      GO TO 120
  315 IF (MOD(AREA,NAH) .EQ. 0) GO TO 390
      IF (TRYTAB(AREA+1) .EQ. 1) GO TO 330
      AREA = AREA + 1
      GO TO 120
  310 IF (AREA.LE.NAH) GO TO 390
      IF (TRYTAB(AREA-NAH) .EQ. 1) GO TO 330
         AREA = AREA - NAH
         GO TO 120
  330 IF (MOD(AREA,NAH) .EQ. 0 .OR. TRYTAB(AREA+1) .EQ. 1) GO TO 380
         AREA = AREA + 1
         GO TO 120
  380 DO 381  AREA = 1, NAREA
         IF (TRYTAB(AREA).EQ.0) GO TO 120
  381 CONTINUE
  390 CALL XVMESSAGE('*** AREA ERROR. ABEND.',' ')
      CALL ABEND
! End of embedded code from label 300

  140 CONTINUE
      RETURN

  998 CONTINUE
           WRITE (MESSAGE, 99001) IER
99001      FORMAT ('CALL TO DGELG RESULTS IN IER = ',I3)
           CALL XVMESSAGE (MESSAGE,' ')
           CALL ABEND
      END
C
C
C
      SUBROUTINE GSUB(LINE, ISAMP, NSAMP, ATAB, BIGBUF, LBUF)
      INCLUDE 'fortport'
C
C   *****DECLARATIONS OF PASSED VARIALBLES
C
      INTEGER LINE,ISAMP,NSAMP
      INTEGER BIGBUF(*)
      DOUBLE PRECISION ATAB(11,*)
      BYTE LBUF(*)
C
C   *****VARIABLES IN COMMON BLOCK
C
!     Common storage definitions
      REAL    ISKIP(4), FLN, FSM, FSMT
      DOUBLE PRECISION LARGE, SMALL
      INTEGER LN1, ONS, NOTE, NS,  INDEX1, IUSE, IDEL, IMAX,
     &        NLO, NSO,  NLI, NSI,   NI,   NO, IR,
     &        TYPE, IMG, OUTFIL, SL, SS
      LOGICAL HALF

!     Common storage declarations

      COMMON /C1/ LN1,NOTE,NS,FLN,FSM,FSMT,LARGE,SMALL
      COMMON /C1/ INDEX1,IUSE,IDEL,IMAX
      COMMON /C1/ ISKIP,SL,SS,NLO,NSO,NLI,NSI,NI,NO,TYPE,HALF,ONS,IR
      COMMON /FILES/ IMG, OUTFIL
      SAVE /C1/,/FILES/
C
C
C   *****VARIABLES USED FOR INTERPOLATION
C
      INTEGER POINT, VAL1, VAL2, VAL3, VAL4
      BYTE TPOINT, TVAL1, TVAL2, TVAL3, TVAL4
      DOUBLE PRECISION VAL12, VAL34
C
C   ***** LOCAL VARIABLES
C
      DOUBLE PRECISION DELP, DELR, X, Y, PFRAC, RFRAC, P, R
      DOUBLE PRECISION A, B, C, D, E, F, G, H
      INTEGER IP, INDEX2
C
      DATA  VAL12/0.0D0/, VAL34/0.0D0/
      DATA  ZERO /0/
      DATA  DELP/0.0D0/,  DELR/0.0D0/,  X/0.0D0/, Y/0.0D0/ 
      DATA  PFRAC/0.0D0/, RFRAC/0.0D0/, P/0.0D0/, R/0.0D0/
      DATA  A/0.0D0/, B/0.0D0/, C/0.0D0/, D/0.0D0/, E/0.0D0/
      DATA  F/0.0D0/, G/0.0D0/, H/0.0D0/
      DATA  IP/0/, INDEX2/0/, IPOS/0/
      DATA  POINT/0/,  VAL1/0/,  VAL2/0/,  VAL3/0/,  VAL4/0/
      DATA  TPOINT/0/, TVAL1/0/, TVAL2/0/, TVAL3/0/, TVAL4/0/
C
C   ***** SET UP COEFFICIENTS FROM DGELG
C
      A = ATAB(1, 1)
      B = ATAB(2, 1)
      D = ATAB(3, 1)
      E = ATAB(4, 1)
      F = ATAB(5, 1)
      H = ATAB(6, 1)
      C = ATAB(7, 1)
      G = ATAB(8, 1)
C
C
      Y = LINE
      X = ISAMP
C
C     ***INCREMENTS FOR INPUT PIXEL LOCATIONS
C
      DELP = A + C * Y
      DELR = E + G * Y
C
C     ***CALCULATE PIXEL LOCATION IN INPUT IMAGE P = ISAMP, R = LINE
C
      P = DELP * X + (D + B * Y)
      R = DELR * X + (H + F * Y)
C
C     ***INDEX1  IS USED FOR INDEX OF OUTPUT BUFFER
C
      INDEX2 = INDEX1 + ISAMP
C
C     ***ISKIP(1) AND ISKIP(2) WILL ALWAYS BE 0
C     ***BEGIN MAIN PROCESSING LOOP
C
 220  IF (R .LT. ISKIP(1)) GOTO 999
      IR = R
      RFRAC = R - IR
      IF (P .LT. ISKIP(2)) GOTO 999
      IP = P
      PFRAC = P - IP
C
C   *****CHECK IF VALUES ARE WITHIN THE PICTURE
C
      IF (IP .GT. 0) GOTO 224
      IF (IP .LT. 0) GOTO 999
      IF (PFRAC .GT. LARGE) GOTO 230
      GOTO 999
C
 224  IF (IP .LT. NSI) GOTO 230
      IF (IP .GT. NSI) GOTO 999
      IF (PFRAC .GT. SMALL) GOTO 999
C
 230  IF (RFRAC .LT. SMALL) GOTO 233
      IF (RFRAC .LT. LARGE) GOTO 250
      IR = IR + 1
C
 233  IF (IR .LE. 0) GOTO 999
      IF (IR .GT. NLI) GOTO 999
C
C   ***** IS THIS LINE IN BIGBUF???
C   ***** IF NOT READ IT IN
C
      IF (BIGBUF(IR) .EQ. 0) CALL GET(LBUF,LBUF,LBUF)
      IPOS = BIGBUF(IR) + IP
C
C   *****GET VALUES FOR INTERPOLATION
C
      TVAL1 = LBUF(IPOS)
      LBUF(INDEX2) = TVAL1
      IF (PFRAC .LT. SMALL) GOTO 290
      TVAL2 = LBUF(IPOS + 1)
      LBUF(INDEX2) = TVAL2
      IF (PFRAC .GE. LARGE) GOTO 290
C
C   ***** INTERPOLATE BETWEEN TWO VALUES IF SMALL<=PFRAC<LARGE
C
      VAL1  = BYTE2INT (TVAL1) ! Provide portability for byte to integer
      VAL2  = BYTE2INT (TVAL2) ! Provide portability for byte to integer
      POINT = NINT( FLOAT(VAL1) + PFRAC * (VAL2 - VAL1) )
      TPOINT = INT2BYTE (POINT) ! Provide portability for integer to byte
      LBUF(INDEX2) = TPOINT
      GOTO 290
C
 250  IF (IR .LE. 0) GOTO 999
      IF (IR .GE. NLI) GOTO 999
C
C   ***** INTERPOLATE USING 4 CLOSEST VALUES TO PIXEL (R,P)
C   ***** IF LINE NOT IN CORE(BIGBUF) THEN READ IN
C
      IF (BIGBUF(IR) .EQ. 0) CALL GET(LBUF,LBUF,LBUF)
      IPOS = BIGBUF(IR) + IP
      TVAL1 = LBUF(IPOS)
      VAL1  = BYTE2INT (TVAL1) ! Provide portability for byte to integer
      TVAL2 = LBUF(IPOS + 1)
      VAL2  = BYTE2INT (TVAL2) ! Provide portability for byte to integer
      VAL12 = FLOAT(VAL1) + PFRAC * (VAL2 - VAL1)
C
C   ***** IF NEXT LINE NOT IN CORE(BIGBUF) THEN READ IN
C
      IR = IR + 1
      IF (BIGBUF(IR) .EQ. 0) CALL GET(LBUF,LBUF,LBUF)
C
 260  IPOS = BIGBUF(IR) + IP
      TVAL3 = LBUF(IPOS)
      VAL3  = BYTE2INT (TVAL3) ! Provide portability for byte to integer
      TVAL4 = LBUF(IPOS + 1)
      VAL4  = BYTE2INT (TVAL4) ! Provide portability for byte to integer
      VAL34 = FLOAT(VAL3) + PFRAC * (VAL4 - VAL3)
      POINT = NINT( ((VAL34 - VAL12) * RFRAC) + VAL12 )
      TPOINT = INT2BYTE (POINT) ! Provide portability for integer to byte
      LBUF(INDEX2) = TPOINT
C
C   *****INCREMENT VALUES AND CONTINUE PROCESSING SAMPLES
C
 290  INDEX2 = INDEX2 + 1
      VAL1 = 0
      VAL2 = 0
      VAL3 = 0
      VAL4 = 0
      R = R + DELR
      P = P + DELP
      NSAMP = NSAMP - 1
      IF (NSAMP .NE. 0) GOTO 220
      RETURN
C
C   ***** IF OUT OF PICTURE
C
 999  LBUF(INDEX2) = 0
      GOTO 290
      END
C
C
C
C
C
      SUBROUTINE GET(BIGBUF,BUF2,BUF)
C
C   ***** PASSED VARIABLES
C
      INTEGER BIGBUF(*)     !note: Although subroutine MAIN has an I*2 BIBUF,
                            !      here and most of the program it is INTEGER.
      INTEGER*2 BUF2(*)
      BYTE BUF(*)
C
!     Common storage definitions
      REAL    ISKIP(4), FLN, FSM, FSMT
      DOUBLE PRECISION LARGE, SMALL
      INTEGER LN1, ONS, NOTE, NS,  INDEX1, IUSE, IDEL, IMAX,
     &        NLO, NSO,  NLI, NSI,   NI,   NO, IR,
     &        TYPE, IMG, OUTFIL, SL, SS
      LOGICAL HALF

!     Common storage declarations

      COMMON /C1/ LN1,NOTE,NS,FLN,FSM,FSMT,LARGE,SMALL
      COMMON /C1/ INDEX1,IUSE,IDEL,IMAX
      COMMON /C1/ ISKIP,SL,SS,NLO,NSO,NLI,NSI,NI,NO,TYPE,HALF,ONS,IR
      COMMON /FILES/ IMG, OUTFIL
      SAVE /C1/,/FILES/
C
C
C   ***** LOCAL VARIABLES
C
      INTEGER TIUSE
C==================================================================
C   BIGBUF array elements are utilized as follows:
C    1 - NLI:  LINE BUFFER index - BIGBUF(IR)=0 if line IR not in memory,
C                   otherwise BIGBUF(IR)+1 is index of start of line IR in
C                   array BUF (or LBUF in calling routines).
C    NLI+1-    Buffers to hold individual lines.  Each buffer has an I*2
C  some max:   line header (that contains the line number of the line stored
C              there) followed by the pixel data for that line.  NLI is limited
C              to 32767 by this limitation.
C     
C   ***** LOCATION OF DATA FOR LINE 'IR' OF INPUT DATASET
      BIGBUF(IR) = IUSE                 !In LINE BUFFER index, store address 
                                        !of line to be read.
      IF (BUF2((IUSE + 1)/2) .GT. 0)    !Look in line header to find what line
     . BIGBUF(BUF2((IUSE + 1) / 2)) = 0 !was there before, and zero out its
                                        !entry in LINE BUFFER index UNLESS
                                        !there was no line there before.
      BUF2((IUSE + 1) / 2) = IR         !Put new line number in line header.
C
C   ***** TIUSE-LOCATION OF DATA
C
      TIUSE = IUSE
C
C   *****LOCATION FOR NEXT LINE OF DATA
C
      IUSE =IUSE + IDEL
C
C   ***** IF GREATER THAN BUFFERSIZE THEN RESET IUSE
C
      IF (IUSE .LE. IMAX) GOTO 100
         IUSE = (NLI + 1) * 4
C
 100  CALL XVREAD(IMG,BUF(TIUSE+1),STAT,'LINE',IR,' ')
C
 999  RETURN
C
      END



      SUBROUTINE GSUBH(LINE, ISAMP, NSAMP, ATAB, BIGBUF, LBUF, LBUFINT,
     *LBUFBYT)
C
C   *****PASSED IN VARIABLES
C
      INTEGER LINE, ISAMP, NSAMP
      DOUBLE PRECISION ATAB(11,*)
      INTEGER BIGBUF(*)
      INTEGER*2 LBUF(*)
      INTEGER LBUFINT(*)
      BYTE LBUFBYT(*)
C
!     Common storage definitions
      REAL    ISKIP(4), FLN, FSM, FSMT
      DOUBLE PRECISION LARGE, SMALL
      INTEGER LN1, ONS, NOTE, NS,  INDEX1, IUSE, IDEL, IMAX,
     &        NLO, NSO,  NLI, NSI,   NI,   NO, IR,
     &        TYPE, IMG, OUTFIL, SL, SS
      LOGICAL HALF

!     Common storage declarations

      COMMON /C1/ LN1,NOTE,NS,FLN,FSM,FSMT,LARGE,SMALL
      COMMON /C1/ INDEX1,IUSE,IDEL,IMAX
      COMMON /C1/ ISKIP,SL,SS,NLO,NSO,NLI,NSI,NI,NO,TYPE,HALF,ONS,IR
      COMMON /FILES/ IMG, OUTFIL
      SAVE /C1/,/FILES/
C
C   *****VARIABLES USED DURING INTERPOLATION
C
      INTEGER POINT, TVAL1, TVAL2, TVAL3, TVAL4
      DOUBLE PRECISION VAL12, VAL34
C
C   *****LOCAL VARIABLES
C
      DOUBLE PRECISION DELP, DELR, X, Y, PFRAC, RFRAC, P,R
      DOUBLE PRECISION A, B, C, D, E, F, G, H
      INTEGER IP, INDEX2
C
C
C   ***** SET UP COEFFICIENTS FROM DGELG
C
      A = ATAB(1, 1)
      B = ATAB(2, 1)
      D = ATAB(3, 1)
      E = ATAB(4, 1)
      F = ATAB(5, 1)
      H = ATAB(6, 1)
      C = ATAB(7, 1)
      G = ATAB(8, 1)
C
C
      Y = LINE
      X = ISAMP
C
C     ***INCREMENTS FOR INPUT PIXEL LOCATIONS
C
      DELP = A + C * Y
      DELR = E + G * Y
C
C     ***CALCULATE PIXEL LOCATION IN INPUT IMAGE P = ISAMP, R = LINE
C
      P = DELP * X + (D + B * Y)
      R = DELR * X + (H + F * Y)
C
C
C
C     ***INDEX1  IS USED FOR INDEX OF OUTPUT BUFFER
C
      INDEX2 = (INDEX1 + (ISAMP * 2) + 1)/2
C
C     ***ISKIP(1) AND ISKIP(2) WILL ALWAYS BE 0
C     ***BEGIN MAIN PROCESSING LOOP
C
 220  IF (R .LT. ISKIP(1)) GOTO 999
      IR = R
      RFRAC = R - IR
      IF (P .LT. ISKIP(2)) GOTO 999
      IP = P
      PFRAC = P - IP
      IP = IP * 2
C
C   *****CHECK IF VALUES ARE WITHIN THE PICTURE
C
      IF (IP .GT. 0) GOTO 224
      IF (IP .LT. 0) GOTO 999
      IF (PFRAC .GT. LARGE) GOTO 230
      GOTO 999
C
 224  IF (IP .LT. NSI) GOTO 230
      IF (IP .GT. NSI) GOTO 999
      IF (PFRAC .GT. SMALL) GOTO 999
C
 230  IF (RFRAC .LT. SMALL) GOTO 233
      IF (RFRAC .LT. LARGE) GOTO 250
      IR = IR + 1
C
 233  IF (IR .LE. 0) GOTO 999
      IF (IR .GT. NLI) GOTO 999
C
C   *****IF LINE 'IR' IS NOT IN BIGBUF, THEN READ IT IN
C
      IF (BIGBUF(IR) .EQ. 0) CALL GET(LBUFINT,LBUF,LBUFBYT)
      IPOS = (BIGBUF(IR) + IP + 1) / 2
C
      TVAL1 = LBUF(IPOS)
      LBUF(INDEX2) = TVAL1
C
      IF (PFRAC .LT. SMALL) GOTO 290
      TVAL2 = LBUF(IPOS + 1)
      LBUF(INDEX2) = TVAL2
      IF (PFRAC .GE. LARGE) GOTO 290
C
C   *****INTERPOLATE BETWEEN TWO VALUES IF SMALL<=PFRAC<LARGE 
C
      POINT = NINT( FLOAT(TVAL1) + PFRAC * (TVAL2 - TVAL1) )
      LBUF(INDEX2)=POINT
      GOTO 290
C
 250  IF (IR .LE. 0) GOTO 999
      IF (IR .GE. NLI) GOTO 999
C
C   ***** INTERPOLATE USING 4 CLOSEST VALUES TO PIXEL (R,P)
C   ***** IF LINE NOT IN BIGBUF, THEN READ IN
C
      IF (BIGBUF(IR) .EQ. 0) CALL GET(LBUFINT,LBUF,LBUFBYT)
      IPOS = (BIGBUF(IR) + IP + 1) / 2
C
      TVAL1 = LBUF(IPOS)
      TVAL2 = LBUF(IPOS + 1)
      VAL12 = FLOAT(TVAL1) + PFRAC * (TVAL2 - TVAL1)
C
C   ***** IF LINE NOT IN BIGBUF THEN READ IN
C
      IR = IR + 1
      IF (BIGBUF(IR) .EQ. 0) CALL GET(LBUFINT,LBUF,LBUFBYT)
C
 260  IPOS = (BIGBUF(IR) + IP + 1) / 2
      TVAL3 = LBUF(IPOS)
      TVAL4 = LBUF(IPOS + 1)
      VAL34 = FLOAT(TVAL3) + PFRAC * (TVAL4 - TVAL3)
      POINT = NINT( ((VAL34 - VAL12) * RFRAC) + VAL12 )
      LBUF(INDEX2) = POINT
C
C   ***** INCREMENT VALUES AND CONTINUE PROCESSING SAMPLES
C
 290  INDEX2 = INDEX2 + 1
      R = R + DELR
      P = P + DELP
      NSAMP = NSAMP - 1
      IF (NSAMP .gt. 0) GOTO 220
      RETURN
C
 999  LBUF(INDEX2) = 0
      GOTO 290
      END
