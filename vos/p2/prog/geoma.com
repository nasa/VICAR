$!****************************************************************************
$!
$! Build proc for MIPL module geoma
$! VPACK Version 1.9, Monday, December 07, 2009, 16:20:52
$!
$! Execute by entering:		$ @geoma
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
$ write sys$output "*** module geoma ***"
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
$ write sys$output "Invalid argument given to geoma.com file -- ", primary
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
$   if F$SEARCH("geoma.imake") .nes. ""
$   then
$      vimake geoma
$      purge geoma.bld
$   else
$      if F$SEARCH("geoma.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geoma
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geoma.bld "STD"
$   else
$      @geoma.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geoma.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geoma.com -mixed -
	-s geoma.f -
	-i geoma.imake -
	-p geoma.pdf -
	-t tstgeoma.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create geoma.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geoma.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM geoma

   To Create the build file give the command:

		$ vimake geoma			(VMS)
   or
		% vimake geoma			(Unix)
************************************************************************/
#define PROGRAM	geoma
#define R2LIB

#define MODULE_LIST geoma.f
#define FTNINC_LIST fortport
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create geoma.pdf
process help=*	
PARM INP	TYPE=STRING	COUNT=1:2
PARM OUT        TYPE=STRING     COUNT=1
PARM SIZE	TYPE=INTEGER 	COUNT=0:4                   DEFAULT=--
PARM NL		TYPE=INTEGER    COUNT=0:1                   DEFAULT=--
PARM NS 	TYPE=INTEGER    COUNT=0:1                   DEFAULT=--
PARM FORMAT	TYPE=KEYWORD	COUNT=0:1 VALID=(BYTE,HALF) DEFAULT=--
PARM NAH	TYPE=INTEGER	 	                    DEFAULT=0
PARM NAV	TYPE=INTEGER		                    DEFAULT=0
PARM TIEPOINT	TYPE=REAL	COUNT=(1:500)               DEFAULT=(1,1,1,1)
PARM NOINTERP	TYPE=KEYWORD	COUNT=0:1 VALID = NOINTERP  DEFAULT=--
PARM NOTIFY 	TYPE=INTEGER 		                    DEFAULT=101
PARM PARMS	TYPE=STRING	COUNT=0:1	            DEFAULT=--
END-PROC
!# ANNOT ICON = geoma
!# annot project=all
!# annot function=corrections/geometric
!# annot keywords=(geometric,corrections,distortion,transformation)
.TITLE
GEOMA -- perform geometric transformations
.HELP
PURPOSE:

GEOMA is a VICAR applications program which makes geometric changes in
pictures.  It can be used for correcting geometric distortion, or performing
other types of geometric transformations.  The geometric changes
(transformations) are defined as a mapping from a set of locations (called
tiepoints or control points) in the input image to a corresponding set of
locations in the output image.  Between the tiepoints, the mapping is defined
by bilinear interpolation.  GEOMA is similar in function to VICAR program
LGEOM. 
.PAGE
EXECUTION:

The following TAE command line formats show the most common usages:
      GEOMA INP=(a,a2) OUT=b SIZE=(1,1,nl,ns) optional parameters
      GEOMA INP=(a,a2) OUT=b  NL=nl NS=ns optional parameters
      GEOMA (a,a2) b (1,1,nl,ns) optional parameters
      GEOMA (a,a2) b optional parameters
      GEOMA INP=a OUT=b SIZE=(1,1,nl,ns) optional parameters
      GEOMA INP=a OUT=b  NL=nl NS=ns optional parameters
      GEOMA a b (1,1,nl,ns) optional parameters
      GEOMA a b optional parameters
      GEOMA INP=a OUT=b SIZE=(1,1,nl,ns) PARMS=c optional parameters
      GEOMA INP=a OUT=b  NL=nl NS=ns PARMS=c optional parameters
      GEOMA a b (1,1,nl,ns) PARMS=c optional parameters
      GEOMA a b PARMS=c optional parameters

       Here 'a' represents the input image file name,
       'a2' represents an IBIS input file containing tiepoint information,
       'b' represents the output image file name,
       and 'c' represents the parameter file name.

       Note: GEOMA no longer supports a non-IBIS second input file.  
             Old non-IBIS 'GEOMA' files (which were often used to specify
             the geometric transformation needed to correct for camera
             distortion) can be converted on the VAX or Alpha using the program
             OLDGEOMA2IBIS.
.page
USER INFORMATION:

In both GEOMA and LGEOM the picture to be geometrically transformed 
is subdivided into a number of four-sided areas (quadrilaterals).
The user must specify to the program the exact mapping from input picture to
transformed picture for each of the four corner points of each quadrilateral.
Within the quadrilaterals, the programs use a bilinear interpolation to map
the interior points from input picture to transformed picture. The LGEOM 
program restricts the specification of the mapping of the quadrilateral
corners (control points) such that, in the transformed (output) picture, they
form a rectangular grid, (i.e. the quadrilaterals are rectangles in the
output image.)  The GEOMA program does not impose this restriction, and 
allows more arbitrary specification of the control point mapping. In fact, 
two of the four control points forming the corners of any given quadrilateral
may coincide, causing the quadrilateral to degenerate into a triangle (a 
quadrilateral having one side of zero length). The potential user is WARNED 
that GEOMA can produce some undesirable side effects due to the basic 
difference from the LGEOM program.  This is explained further in the 
OPERATION section.

As with LGEOM, GEOMA will accept its control parameters as the second input
file or as a parameter file instead of or in addition to the conventional 
VICAR parameters.

The size of the output image is determined by the number of lines and number 
of samples in the SIZE field if the SIZE field is entered by the user.  If the
SIZE field is not entered, the output file is the same size as the input file.

The data type of the input image may either be byte or halfword data. The data
type is obtained from the VICAR label of the input image or from the FORMAT
parameter. The output image has the same data format  (byte or halfword) as the
input image. 

The tiepoint grid must be specified as a set of parameters in the
TAE command line or in the parameter file or in a second input file.
The tiepoint grid is specified using the parameters NAH, NAV, and TIEPOINT.

   NAH=nah  NAV=nav
     TIEPOINT=(nl1,ns1,ol1,os1   nl2,ns2,ol2,os2...
                   ...nlk,nsk,olk,osk)

where nah is the number of grid cells horizontally (across the top in the
sample direction), nav is the number of grid cells vertically in the output
image space, the point pairs (nli,nsi,oli,osi) are line-sample coordinates in
the output (new) and input (old) spaces respectively. The number of pairs k
must equal (nah+1)*(nav+1). 

The RESTART parameter has been removed since it was deemed not maintainable.
(See FR 16750 of February 1986.)
.page
Applications of GEOMA:

GEOMA and LGEOM are both VICAR programs for performing general geometric
transformations.  Both of them require a grid of tiepoints for defining the
transformation.  In LGEOM, the grid must be rectangular (i.e. the tiepoints
are arranged in straight rows and columns.)  In GEOMA the grid does not need
to be rectangular. This allows GEOMA to be more flexible or general purpose.

GEOMA and LGEOM are almost always used following a VICAR program that
generates the control information (tiepoints) in either a parameter
file or an input file.  This keeps the user from having to enter the
tiepoints manually.  This also makes most of the details regarding the
tiepoint grid transparent to the user.  GEOMA is often used following
VICAR program RESLOC and sometimes following VICAR program PICREG.

GEOMA's greater flexibility is not often needed.  GEOMA is generally 
slower than LGEOM, and it is less commonly used than LGEOM.  The
most common use of GEOMA is in conjunction with RESLOC.  In this
application GEOMA is used to remove geometric distortion in Voyager
pictures based on the reseau locations.   RESLOC writes the
control information in a file which is used as the second input file
for GEOMA.  Sometimes VICAR program FARENC is used between RESLOC 
and GEOMA to add a translation to the geometric transformation when
dealing with a limb of a planet or satelite.  GEOMA is used in
this application because the reseau do not lie in a rectangular grid.

In general, program LGEOM is recommended over GEOMA because of speed unless the
greater flexibility of GEOMA is needed.  For instance, it is more common to use
LGEOM than GEOMA following PICREG.  Where GEOMA is used, it is common to use a
grid of degenerate quadrilaterals (triangles) as shown below in the diagrams in
the OPERATION section.

Users can also use VICAR program MGEOM as an alternative to GEOMA when the
output tiepoints lie in a rectangular grid.  The advantages of MGEOM are
listed in the help for MGEOM.
.page
EXAMPLES

1.    RESLOC (IMG,TF) (R,G) 
      GEOMA  (IMG,G) IMGB (1,1,800,800)

In this example GEOMA is used to remove geometric distortion in a Voyager
image named IMG based on the reseau locations.   RESLOC writes the
control information in file G which is used as the second input file
for GEOMA.  GEOMA produces an 800 by 800 output image named IMGB.
.page
2.   PICREG (J,J2) (T,G)
       ... numerous steps to interactively select tiepoints
       NHOR=10 NVER=10 
       TPFORM=3
       'GEOMA
       'EXIT
     GEOMA INP=J OUT=JB PARMS=G (1,1,800,800)

In this example GEOMA is used to register image J to image J2 using
tiepoints selected interactively in program PICREG.  The tiepoints
selected by the user are converted into a rectangular grid with 10+1
rows and columns using a first-order surface fit (TPFORM=3).  Each
rectangle is divided into two triangles to make the final grid of
degenerate quadrilaterals (triangles) for GEOMA.  The control
information is passed to GEOMA via parameter file G.  GEOMA produces
the registered image JB.
.page
3.   GEOMA INP=T3 OUT=T4 SIZE=(1,1,20,20) NAH=1 NAV=1 +
        TIEPOINT=(1,1,1,1  1,20,1,15,  20,1,10,1  20,1,10,1)

In this example GEOMA is used to perform an affine (first order)
transformation defined by three tiepoints.  (1,1) is mapped to (1,1).
(1,15) is mapped to (1,20).  (10,1) is mapped to (20,1).  The rest of
the mapping is defined by linear interpolation.
.page
4.  The last example is the test procedure for GEOMA.  This is
    a complete example that could be run by the user and that 
    demonstrates uses of the possible parameters.

    !THIS IS A TEST FILE FOR GEOMA
    !THIS TAKES A 10 X 10 MATRIX AND ENLARGES IT TO A 20 X 20 MATRIX
    gen tgen1 10 10 linc=5 sinc=10
    geoma inp=tgen1 out=tgen2 size=(1,1,20,20) nah=1 nav=1 tiepoint=(1,1,1,1,+
    1,20,1,10,20,1,10,1,20,20,10,10)
    list tgen2
    !THIS TAKES A 10 X 10 HALFWORD MATRIX AND ENLARGES IT TO A 20 X 20 HALFWORD 
    !MATRIX
    gen tgen3 10 20 'half sinc=-1 linc=2
    geoma inp=tgen3 out=tgen4 size=(1,1,20,20) 'half nah=1 nav=1+
    tiepoint=(1,1,1,1,1,20,1,20,20,1,10,1,20,20,10,20)
    list tgen4
    !REPEAT WITH PARAMETER NOINTERP
    gen tgen1 10 10 ival=10 sinc=2 linc=5
    geoma inp=tgen1 out=tgen6 size=(1,1,100,100) nah=1 nav=1 tiepoint=(1,1,1,1,+
    1,100,1,10,100,1,10,1,100,100,10,10) 'nointerp
    list tgen6
    !THIS ROTATES A 10 X 10 MATRIX 90 DEGREES
    !ALSO TEST PARAMETER NOTIFY
    gen tgen1 10 10
    geoma inp=tgen1 out=tgen7 size=(1,1,10,10) nah=1 nav=1 tiepoint=(1,1,10,10,+
    1,10,10,1,10,1,1,10,10,10,1,1) notify=20
    list tgen7
    !GEOMETRICALLY CORRECTS AN IO IMAGE USING SPECIFIED PARAMETER FILE
    gen tgen1 10 10 ival=10 sinc=2 linc=5
    DCL DEFINE VGR MIPLDISK:[MIPL.VGR]
    geoma inp=(VGR:F1636832.FIC,VGR:F1636832.IGPR) out=tgen5
    label-list tgen5
.page
OPERATION:

GEOMA computes the DN value of each pixel in the output picture as follows.
First, decide within which quadrilateral area in the output picture the pixel
lies. Let the coordinates of the four control points defining that
quadrilateral in the output picture be called (x(j),y(j),j=1,2,3,4), and in the
input picture let them be called (x'(j), y'(j),j=1,2,3,4). (x may be considered
the line coordinate and y the sample, although they could as well be reversed
for this discussion.) The values of all 16 of these numbers are part of the
control parameters for GEOMA. Define a transformation by 
		x' = ax + by + cxy + d                                (1)
		y' = ex + fy + gxy + h
where the values of the coefficients a,b,..,h are determined by requiring that
		x'(j) = ax(j) + by(j) + cx(j)y(j) + d                 (2)
		y'(j) = ex(j) + fy(j) + gx(j)y(j) + h
		for j = 1,2,3,4

This is the condition that the defined transformation exactly maps
the control point coordinates as specified by the parameters.

The transformation is used to transform the line-sample coordinates of
the output picture pixel being processed into its corresponding coordinates in
the input picture. In general, although the output coordinates are always
integer, the input coordinates are not, so the input coordinates do not
correspond to any one pixel but instead fall in between the input pixels. If
intensity interpolation is NOT to be performed, as specified by the
NOINTERP parameter, then the DN value assigned to the output pixel
is that of the pixel in the input picture nearest the transformed coordinates.
The use of this mode considerably reduces the CPU time required by GEOMA. The
quality of the result is in general degraded but still may be quite
acceptable, depending on the nature of the transformation.

Assuming interpolation is to be performed to compute the DN value of
the output picture, the four pixels nearest to the transformed coordinates in
the input picture are selected.

The interpolation formula is
		DN = px' + qy' + rx'y' + s 			(3)
where x' and y' are the transformed coordinates of the output picture pixel,
DN is the data number to be assigned to the output pixel, and p,q,r and s are
constants chosen such that
		DN(k) = px'(k) + qy'(k) + rx'(k)y'(k) + s       (4)
		for k = 1,2,3,4
where x'(k), y'(k) and DN(k), k = 1,2,3,4 are the coordinates and DN values of
the four selected input pixels nearest to the transformed coordinates.

In order for GEOMA to work properly, the tiepoint coordinates in the
output picture must satisfy certain constraints. They must be organizable into
a rectangular matrix of points. That is, each tiepoint must belong to one row
and one column of points, each row must have the same number of points as every
other row, and each column must have the same number as every other column. It
should be possible to number the rows sequentially from top to bottom, and the
columns form left to right. Quadrilaterals used by the program for performing
the geometric transformation are formed by connecting each tiepoint to the ad-
jacent tiepoints in the same column and in the same row. The quadrilaterals
should all have the property that they are not concave (no interior angle of
any quadrilateral shoud be greater than 180 degrees). In addition, each tie-
point should be below any tiepoint in its column and in any preceding row, and
above any tiepoint in its column in any subsequent row. Similarly, each tie-
point should be to the right of any tiepoint in its row and in a preceding
column, and to the left of any tiepoint in its row and in a subsequent column.
The number of areas vertically is the number of rows of tiepoints less 1, and
the number of areas horizontally is the number of columns of tiepoints less 1.
The following caution is issued to potential users of GEOMA. Although the
transformation in equation (1) is very well behaved within the quadrilateral
in which it applies, there is no assurance that it is continuous across the
boundary between adjacent quadrilaterals unless the boundary is precisely
vertical or horizontal. The degree of discontinuity depends on the details
of the tiepoint specifications. In some cases the discontinuity may be so
small that the transformation picture has no visible defect, but in others
it may be quite visible.

The user has two choices to minimize the discontinuity. He may
carefully choose the transformation such that discontinuities are minimized,
or he may specify the tiepoints in such a way that the quadrilaterals
degenerate into triangles. A quadrilateral degenerates into a triangle if two
adjacent tiepoints defining it have the same coordinates. By suitable choice of
tiepoints, the user can specify the coordinates in such a way that all
quadrilaterals degenerate into triangles.  This, in fact, is commonly done.
(See programs RESLOC and PICREG.)  The figures below show sample arrays
of tiepoints satisfying this condition in output-image views.  (The rows
and columns are straight below because it was easier to draw, but they are not 
required to be straight.)
.PAGE
           1,2       3,4        5
            +---------+---------+
            | \       | \       |
            |   \     |   \     |
            |     \   |     \   |
            |       \7|8      \ |
          6 +---------+---------+ 9,10
            |       / |       / |
            |     /   |     /   |
            |   /     |   /     |
            | /       | /       |
            +---------+---------+
          11,12     13,14       15

   Sample Triangular Tiepoint Array: 4 areas horizontally, 2 areas vertically.
.PAGE
           1,2       3,4        5,6      7,8       
            +---------+---------+---------+
            | \       | \       | \       | \
            |   \     |   \     |   \     |   \
            |     \   |     \   |     \   |     \
            |       \ |10     \ |12     \ |14     \
          9 +---------+---------+---------+---------+ 16
            |       / |11     / |13     / |15     /
            |     /   |     /   |     /   |     /  
            |   /     |   /     |   /     |   /    
            | /       | /       | /       | /      
            +---------+---------+---------+
          17,18     19,20     21,22     23,24

   Sample Triangular Tiepoint Array: 7 areas horizontally, 2 areas vertically.
.PAGE
When an area is a triangle, the cross terms in equation (1) are
eliminated and the transformation becomes
		x' = ax + by + d				(5)
		y' = ex + fy + h
where the values of the coefficients are determined by requiring that
		x'(j) = ax(j) + by(j) + d			(6)
		y'(j) = ex(j) + fy(j) + h
		for j = 1,2,3,4
The values of j designate the three distinct tiepoints defining the triangle.
Because equation (5) is linear, the transformation is guaranteed to be
continuous at the triangle boundaries and discontinuities in the output
picture cannot occur.

There is a question about how to transform picture samples which fall
outside all the quadrilaterals defined by parameters. One answer is to avoid
this situation by specifying the control point locations so that every sample
in the output picture falls within some quadrilateral. This requires that some
control points fall exactly on or outside the border of the output picture.
The program is not affected in any way by using control points outside the
picture.

If some picture samples do fall outside the defined quadrilaterals, the
program will process each such sample by assigning it to a nearby quadrila-
teral. This is done by "extending" the boundaries of all "edge" quadrilaterals
to the picture borders. The details of the precise extrapolation algorithm
will not be given. The user should understand that the algorithm can lead to
discontinuities in the processed picture in the regions outside the defined
quadrilaterals, even when the control areas are triangles.
.page
RESTRICTIONS:
1. The input and output images must be byte or halfword data.
2. There are essentially no restrictions in GEOMA on picture size or 
   number of tiepoints.  (The input image may not have >32767 lines.)
3. The maximum number of tiepoints that can be passed via a 
   parameter file is currently 2047.  
4. The maximum number of tiepoints that can be specified as 
   part of the TAE command line is 125.
5. The maximum number of tiepoints that can be passed via an IBIS tiepoint
   file is set to 10,000.  (This could be increased, but you would probably
   want to increase the size of BIGBUF proportionally.)
.PAGE
TIMING:	

Under certain circumstances, such as 90 degree rotations, GEOMA will be much
slower than LGEOM.  (An "LGEOMA" could be written, using the internal algorithm
of LGEOM, which would be much faster in those circumstances. However, such a
program does not currently exist.)   For small rotations GEOMA will be faster
than LGEOM.  As with most VICAR programs, the elapsed time for execution will
be shortened if the input and output files are on separate disk drives. 

Under VMS the execution time and the number of page faults can be affected by
the working set size that was set by the system manager.  If a large
number of page faults are observed, the working set size should be increased.
Alternatively, it may be possible to reduce the NBBUF value in the program. 

.PAGE
PRECISION:

This portable version of GEOMA will generally produce identical output on
all of the MIPS-supported platforms.  This output will frequently not be
identical to the output of the unported version of GEOMA, but the differences
will be trivial and should be improvements.

This section is required by the MSTP SRD for all programs that
do not meet the precision requirements given in the SRD.  The two requirements
not met by GEOMA are:
1) Integer data output by a ported program must be the same as that produced
   by the unported program.
2) Integer data output by a ported program must be the same on all of the
   MIPS-supported platforms.
All of the programs that do geometric transformations are going to have problems
meeting one or both of these requirements.   This includes MAP3, LGEOM, MGEOM,
GEOMA, and several others.  GEOMA was modified to increase the use of
Double Precision floating point arithmetic so that the first requirement is met
"virtually always" and the second requirement is met "well enough". 
The definition of the above qouted terms is given towards the end of this 
section.

In the port, GEOMA was modified to retain the full precision returned from
subroutine DGELG.  This was done to eliminate or minimize the differences
produced on different platforms.  The results are somewhat more accurate
than those of the unported GEOMA, but since the output DN values are rounded
to the nearest integer, the output images are essentially the same as for the
unported GEOMA.

In computing the output pixel value (DN), GEOMA usually interpolates between
four input pixel values, but if the input line coordinate is close enough to an
integer value, it interpolates between two input pixel values.  For byte images,
the two pixel interpolation and the four pixel interpolation yield the
same result virtually always once the result is rounded to the nearest
integer.  For halfword images, where the pixel values are often a hundred times
greater, the two pixel interpolation and the four pixel interpolation
occassionally yield slightly different results.

The image f1636832.fic used in the GEOMA test pdf shows what type of differences
are likely to occur (with the ported GEOMA) between machines, and what type of
differences are likely to occur between the ported GEOMA and the unported GEOMA.
The output file from the ported GEOMA is identical on all of the MIPS supported
machines.  The output file from the ported GEOMA is very slightly different
from the output file from the ported GEOMA.  98% of the pixels are identical.
Almost all of the rest differ in value by 1.  The minimum difference is -10.
The maximum difference is 17.  Program DIFPIC shows the AVE VAL OF
DIFFS=-0.125000E-03, or approximately .0001. 

The cause of the differences is that the single precision computations
(unported GEOMA) round differently than the double precision computations
(ported GEOMA), resulting in differences in when two pixel interpolation is
used and when four pixel interpolation is used.  The above 
halfword image has a number of locations where the difference
between adjacent pixels is greater than 10,000.  This combines to produce
a slight but insignificant difference.  This difference is estimated at
less than 1% of the difference between using 'NOINTERP and using regular
interpolation, which is the definition I will use for "well enough".
I believe that such differences will not be humanly observable in an image
display.  

To complete this discussion, I want to define the other quoted term mentioned
above. "Virtually always" is always minus the probability of a difference in
output between different MIPS supported machines.  Differences are possible
when 
1) one machine does a two pixel interpolation while another machine does
   a four pixel interpolation.
2) one machine produces a double-precision output pixel value very slightly
   less than halfway between two integers and another machine produces an
   output pixel value equal to or slightly greater than halfway between
   two integers.
3) one machine determines that an output pixel lies outside the geometrically
   transformed boundaries of the input image while another machine decides
   the output pixel lies within the boundaries.

Possibilty 2 is approximately the probability of a double precision
result in the range between 0.0 and 1.0 being close enough to 0.5 to round
differently on two machines.  Double-precision rounding differences start at
about 1.E-14 on the MIPS-supported machines and might grow to a maximum of
1.E-8 going through the computations in GEOMA.  So the above probability is 
less than the probability of the result being in the range 0.49999999 and
0.50000000.  Assuming a fairly uniform distribution, this is probability
is 1.E-8.

Possibility 1 can be shown to have a similar probability.  Possibility 3 has an
even lower probability because the border pixels are such a small fraction of
the total pixels.  However, possibily 3 could cause one machine to generate a
pixel value of 0 while another machine could generate a relatively large pixel 
value from the edge of the input image.  The other possibilities only generate
a difference of 1 (possibility 2) or something small relative to the difference
between adjacent pixels (possibility 1).  In an image display, none of these
differences should be humanly observable.   Based on this discussion, "virtually
always" is defined as no more than 1 different pixel in a set of 1.E8 pixels.
This amounts to no more than one difference in a hundred 1000 by 1000 pixel
images.
.PAGE
WRITTEN BY:	J. SEIDMAN			22 DECEMBER 1970

CONVERTED TO VAX BY:   HELEN B. MORTENSON	15 SEPTEMBER 1983

CONVERTED TO VICAR2 BY: HELEN B. MORTENSON	 8 APRIL 1985

Made portable for UNIX ... Jim Turner (CRI)      5 September 1994

COGNIZANT PROGRAMMER:  Steve Pohorsky

REVISION HISTORY
  7-95    SP  Added support for an IBIS tiepoint file as a second input.
  6-96    OAM Deleted default value of parameter OUT in geoma.pdf to make 
              program compatible with SAGE (DFR).
.LEVEL1
.VARI INP
Input file name and optional 
 IBIS tiepoint file
.VARI OUT
Output file name.
.VARI SIZE
Standard VICAR size field.
.VARI NL
Number of lines in output.
.VARI NS
Number of samples in output.
.VARI FORMAT
Input data format.
Valid: BYTE,HALF.
.VARI NAH
Number of areas horizontally.
See explanation.
.VARI NAV
Number of areas vertically.
See explanation.
.VARI TIEPOINT
Specifies mapping of control
points between output and
input pictures.
.VARI NOINTERP
Specifies that intensity 
interpolation is not to be 
perfomed after the mapping 
of each pixel.
.VARI NOTIFY
Causes a message to be typed
at the user's terminal after
completion of each NOTIFY
percent of output lines.
.LEVEL2
.VARI INP
INP=(ds1,ds2)  There are two datasets allowed, one required and one optional.
The first is the input dataset containing a picture to be transformed in
standard VICAR format (byte or halfword).  If this dataset is the only input
dataset, then the syntax INP=ds1 is allowed.
The second is an optional input parameter dataset containing control
parameters. This dataset must be an IBIS file containing tiepoint data.
.VARI OUT
OUT=B where B is a string specifying the name of the VICAR output file.
Default is "GEOMA".
.VARI SIZE
SIZE=(1,1,NL,NS) is written in standard VICAR size field format. However it
does not have conventional meaning. The "starting line" and "starting sample"
parameters are meaningless and are ignored. The "number of lines" and "number
of samples" parameters specify the output picture size. 
.VARI NL
NL=I where I is an integer. This keyword along with NS=I1, can be used to
specify the VICAR size field format and can replace using the SIZE
parameter. NL specifies the number of lines in the output picture.
.VARI NS
NS=I1 where I1 is an integer. This keyword along with NL=I, can be used to
specify the VICAR size field format and can replace using the SIZE
parameter. NS specifies the number of samples in the output picture.
.VARI FORMAT
This parameter has two valid keyword values: BYTE and HALF.
 
BYTE specifies that the picture to be processed is in byte format
(one byte per sample).
 
HALF specifies that the picture is to be processed is in halfword format
(two bytes per sample).  The program also reads the format field of the
VICAR label and uses that as the default.  It is safest to default this
parameter.
.VARI NAH
NAH=N where N is an integer value specifing the number of areas horizontally;
which is also the number of columns of tiepoints less 1. NAH "must"
be specified on the command line or in a parameter file
unless an optional dataset containing the parameters NAH, NAV
and TIEPOINTs is specified.(2nd optional input dataset)   NAH must also
be >= 1.  
.VARI NAV
NAV=N where N is an integer value specifing the number of areas vertically;
which is also the number of rows of tiepoints less 1. NAV "must" 
be specified on the command line or in a parameter file
unless an optional dataset containing the parameters NAH, NAV, and TIEPOINTs
is specified(2nd optional input dataset). NAV must also be >= 1.
.VARI TIEPOINT
TIEPOINT=(newline(1),newsamp(1),oldline(1),oldsamp(1),newline(2),...). The
values of TIEPOINT specify the mapping of control points between output
and input pictures.  The
numbers which follow the keyword are in groups of four, one group of
four for each tiepoint. The numbers may be either integer or real type. The
total number of tiepoint numbers must be
			4*(NAH+1)*(NAV+1)
Within each group of four numbers describing a tiepoint, the first number
specifies the line coordinate of that tiepoint in the output (transformed)
picture, the second number specifies the sample coordinate of that tiepoint
in the output picture, the third number specifies the line coordinate of the
input picture and the fourth specifies the sample coordinate of the input
picture. The order in which the tiepoints are specified is left to right
within a horizontal row of tiepoints. The horizontal rows of tiepoints are
specified in top-to-bottom sequence. Tiepoint specification is further
clarified in the OPERATIONS and EXECUTION sections.  The TIEPOINT parameter
must be given on the command line or in a parameter file
unless an optional dataset containing the NAH, NAV, and
TIEPOINT parameters is specified.(2nd optional input dataset)
.VARI NOINTERP
This parameter has one valid keyword value: NOINTERP.
 
It specifies that intensity interpolation is not to be performed
after the mapping of each pixel is used. Instead the gray value (DN) of
the nearest pixel is used. Intensity interpolation is explained in the
OPERATION section of the help file.  NOINTERP is not suggested
unless saving execution time is important and the resulting picture quality
is satisfactory.  Interpolation is the default.
.VARI NOTIFY
NOTIFY=II This optional integer parameter will cause a message to be typed on
the operator console  as well as the user's terminal after completion of each
NOTIFY percent of output lines. NOTIFY should be in the range 1 to 100 for
messages or should be 101 for no messages.  The default is 101. 
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstgeoma.pdf
procedure
refgbl $echo
refgbl $syschar
body
LOCAL DIR TYPE=STRING init="wms_test_work:[testdata.vgr]"
LOCAL INPIC       TYPE=STRING
LOCAL TIEPOINTS   TYPE=STRING
let _onfail="stop"
if ($syschar(1) = "UNIX")
	let DIR = "/project/test_work/testdata/vgr/"
end-if
let $echo="yes"
LET INPIC = "&DIR"//"f1636832.fic"
LET TIEPOINTS = "&DIR"//"f1636832.gpribis"

!THIS IS A TEST FILE FOR GEOMA
!THIS TAKES A 10 X 10 MATRIX AND ENLARGES IT TO A 20 X 20 MATRIX
gen tgen1 10 10 linc=5 sinc=10
geoma inp=tgen1 out=tgen2 size=(1,1,20,20) nah=1 nav=1 tiepoint=(1,1,1,1,+
1,20,1,10,20,1,10,1,20,20,10,10)
list tgen2
!THIS TAKES A 10 X 10 HALFWORD MATRIX AND ENLARGES IT TO A 20 X 20 HALFWORD 
!MATRIX
gen tgen3 10 20 'half sinc=-1 linc=2
geoma inp=tgen3 out=tgen4 size=(1,1,20,20) 'half nah=1 nav=1+
tiepoint=(1,1,1,1,1,20,1,20,20,1,10,1,20,20,10,20)
list tgen4
!GEOMETRICALLY CORRECTS AN IO IMAGE USING SPECIFIED PARAMETER FILE
!!
! GEOMA no longer supports a second input file unless it is in IBIS format.
!Use OLDGEOMA2IBIS on Alpha to convert old VAX-style GEOMA files to IBIS format.
 
! DCL DEFINE VGR MIPLDISK:[MIPL.VGR]         original location of F1636832.FIC
geoma inp=(&INPIC, &TIEPOINTS) out=tgen5

WRITE " DIFPIC SHOULD SHOW   609878   DIFFERENCES"

DIFPIC (&INPIC, tgen5)
label-list tgen5
!REPEAT WITH PARAMETER NOINTERP
gen tgen1 10 10 ival=10 sinc=2 linc=5
geoma inp=tgen1 out=tgen6 size=(1,1,100,100) nah=1 nav=1 tiepoint=(1,1,1,1,+
1,100,1,10,100,1,10,1,100,100,10,10) 'nointerp
list tgen6
!THIS ROTATES A 10 X 10 MATRIX 90 DEGREES
!ALSO TEST PARAMETER NOTIFY
gen tgen1 10 10
geoma inp=tgen1 out=tgen7 size=(1,1,10,10) nah=1 nav=1 tiepoint=(1,1,10,10,+
1,10,10,1,10,1,1,10,10,10,1,1) notify=20
list tgen7
end-proc
$ Return
$!#############################################################################
