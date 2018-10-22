       PROGRAM  interloc
C#######################################################################
C  NAME OF ROUTINE
C      "interloc"( INTERsection LOCation)
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      "interloc" is an interactive VICAR applications program used in camera
C      calibration which locates roughly the grid intersections in an input
C      grid image.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    JANUARY 1985
C  FOR
C      MIPL
C
C  ORIGINAL STERGEN PROGRAM BY
C      J. GUSTAVSON
C
C  REVISION HISTORY
C     1-85  SP   CONVERTED FROM IBM VICAR VERSION: miscellaneous cleanup,
C                USED DE ANZA INSTEAD OF RAMTEK OR COMTAL,
C                CONVERTED IPIX AND ERROR ROUTINES FROM IBM ASSEMBLER TO 
C                FORTRAN, CONVERTED INPUT FILE I/O FROM BLOCK MODE TO RECORD
C                MODE.
C                USED SYSTEM STACKA INSTEAD OF THE OLD INTERLOC VERSION.
C     1-85  SP   ADDED TEST PARAMETER TO MAKE TESTING REPEATABLE AND 
C                SYSTEMATIC.
C     1-85  SP   TOOK VPIC FROM PROGRAM STERGEN AND PUT IN INTERLOC.
C     1-85  SP   ADDED FUNCTION IPCNT TO GET COUNT FOR SOME PARAMETERS.
C     1-85  SP   CHANGED GETCUR TO USE RCURSE AND XVINTRACT.
C     1-85  SP   REWROTE HORVER FOR NEW PARAMETER STRUCTURE.
C     1-85  SP   ADDED IS PARAMETER TO IPIX AND WPIX.
C     1-85  SP   ADDED COUNT PARAMETER TO THRU SUBROUTINE.
C     1-85  SP   CHANGED LABELB CALL TO LABELC.
C     1-85  SP   CHANGED ENTRY POINT FIX TO A SEPARATE SUBROUTINE.
C     1-85  SP   CHANGED SO DISP KEYWORD NOT REQUIRED ALONG WITH UP,RIGHT...
C                TO GET PICTURE TO CHANGE.
C     1-85  SP   ADDED WARNING MESSAGE IN PRSUB FOR WHEN AN INTERSECTION IS
C                NOT FOUND.
C     1-85  SP   ADDED LABELC CALL FOR WORK FILE.
C     1-85  SP   ADDED CLOSE AND OPEN BEFORE READING WORK FILE TO PREVENT 
C                READ ERROR.
C     1-85  SP   ADDED CHECK FOR DISID=0 IN GO,RETRY,UP,DOWN,LEFT,RIGHT,HOME
C                PARAMETER PROCESSING.
C     2-85  SP   CORRECTED PROBLEM WHERE THE VARIABLE LINE > MAXLINE AT LABEL
C                520 IN SUBROUTINE GO BY MOVING CALL TO FUNCTION IPIX BEFORE IF
C                STATEMENT.
C     2-85  SP   MODIFIED NXTCUR SINCE RLINE DOES NOT READ VERTICAL LINES.
C     2-85  SP   CORRECTED CALL TO VPIC TO USE NLI & NSI AS IMAGE SIZE.
C     2-85  SP   ADDED CHECK FOR NO PARAMETERS ENTERED AFTER IPARAM CALL.
C     2-85  SP   ADDED CLOSE AND OPEN BEFORE READING DIAGNOSTIC OUTPUT FILE 
C                TO PREVENT READ ERROR.
C     8-86  SP   CHANGED CALL AUTOT TO CALL AUTOT(1) TO REFLECT CHANGE IN AUTOT.
C     4-87  SP   CHANGED ORDER OF POINT COORDINATES IN OUTPUT MARK DATA SET TO
C                BE IN ROW ORDER INSTEAD OF COLUMN ORDER.
C     8-87  SP   CONVERTED TO VICAR2, DELETING GETPAR AND IPCNT.
C     9-92  mjn  fix fr 46507 by adding code in horver to put the 
C                cursor in the upper left corner before each search and
C                by not accepting points outside the image
C     5-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     7-97  RRD  Put grid size into output label.
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      EXTERNAL SUB1

      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      COMMON /COMNEW/ VBUF

      common /xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     .               ncurs, u, c, iform, iblink, iconoff, v,
     .               w, section, gplane
C
C=================START OF EXECUTABLE CODE===============================     

      CALL IFMESSAGE('INTERLOC version 14-JUL-97')
      WORKOPEN = 0
      CALL XVEACTION('SA', ' ')
      CALL XVUNIT ( INFILE, 'INP', 1, ISTAT,' ' )   ! OPEN INPUT FILE
      IF ( ISTAT .NE. 1 ) THEN
           CALL XVMESSAGE ( 'ERROR IN INPUT UNIT ROUTINE', ' ')
           CALL ABEND
      END IF
      CALL XVOPEN ( INFILE, ISTAT,  'OP', 'READ', ' ') 

C  GET SIZE VALUES FOR INPUT IMAGE.

      CALL XVSIZE( ISL, ISS, INL, INS, NLI, NSI )

C  OPEN FIRST OUTPUT FILE

      CALL XVUNIT(OUTFILE,'OUT',1,ISTAT,' ')
      IF ( ISTAT .NE. 1 ) THEN
           CALL XVMESSAGE ( 'ERROR IN OUTPUT UNIT ROUTINE', ' ')
           CALL ABEND
      END IF

      CALL XVOPEN(OUTFILE,ISTAT,'OP','WRITE', ' ')

      CALL XVUNIT(MARKFILE,'OUT',2,ISTAT,' ')
      IF ( ISTAT .NE. 1 ) THEN
           CALL XVMESSAGE ( 'ERROR IN OUTPUT UNIT ROUTINE', ' ')
           CALL ABEND
      END IF

      CALL XVUNIT(WORKFILE,'OUT',3,ISTAT,' ')
      IF ( ISTAT .NE. 1 ) THEN
           CALL XVMESSAGE ( 'ERROR IN OUTPUT UNIT ROUTINE', ' ')
           CALL ABEND
      END IF

      EFLAG=.FALSE.
      INT=.FALSE.
      NBYT = MIN0( 200000, 20000+20*NSI)
      CALL STACKA(3,SUB1,1,NBYT)
      RETURN
      END

C********************************************************************
      SUBROUTINE SUB1(A,LEN)
C********************************************************************
      BYTE  A(LEN)
      CALL SUB2(A,A,A,A,LEN)
      RETURN 
      END

C********************************************************************
      SUBROUTINE SUB2(PAR,HPAR,LPAR,RPAR,XLENG)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      COMMON /COMNEW/ VBUF
      COMMON /SIZDIS/ LSIZE,SSIZE
      DIMENSION LSIZE(5),SSIZE(5)

      DIMENSION PAR(*), IPAR(1000)
      INTEGER*2 HPAR(*)
      BYTE LPAR(*)
      LOGICAL   VERT, TRACK, SEARCH, XVPRM, XVPTST
      REAL*4 RPAR(*)
      CHARACTER*7 CPAR
C
C *************
      NBYTES=XLENG
      VBUF=0
      VDSRN= INFILE
      DISID=0
      NH=0
      NV=0
C *************
C
C INT SETS CONVERSATIONAL MODE OF OPERATION IF SPECIFIED

      INT = XVPTST( 'INT' )
      XVPRM = .TRUE.

C  PARAMETER PROCESSING LOOP.  XVPRM IS TRUE THE FIRST TIME THROUGH.

100   CONTINUE

C
C NH - NUMBER OF HORIZONTAL AREAS

      IF (XVPRM)  THEN
         CALL XVPARM( 'NH', IPAR, ICOUNT, IDEF, 0 )
      ELSE
         CALL XVIPARM( 'NH', IPAR, ICOUNT, IDEF, 0 )
      END IF

      IF (ICOUNT .GT. 0) THEN
          IF (NH.NE.0) CALL ERROR(13,*5000)
          NH = IPAR(1)
          CALL ALLOC(AHSL,NH,2,PAR)
          CALL ALLOC(AHSS,NH,2,PAR)
          CALL ALLOC(AMINL,NH,2,PAR)
          CALL ALLOC(AMAXL,NH,2,PAR)
          IF (NV  .NE.  0)  CALL ALLOC(AINSEC,NH*NV*2,4,PAR)
      END IF

C
C NV - NUMBER OF VERTICAL RULINGS
C

      IF (XVPRM)  THEN
         CALL XVPARM( 'NV', IPAR, ICOUNT, IDEF, 0 )
      ELSE      
         CALL XVIPARM( 'NV', IPAR, ICOUNT, IDEF, 0 )
      END IF

      IF (ICOUNT .GT. 0) THEN
          IF (NV.NE.0) CALL ERROR(14,*5000)
          NV = IPAR(1)
          CALL ALLOC(AVSL,NV,2,PAR)
          CALL ALLOC(AVSS,NV,2,PAR)
          IF (NH .NE. 0)   CALL ALLOC(AINSEC,NH*NV*2,4,PAR)
      END IF

C        HORI - GET STARTING LINE,SAMPLE OF HORIZONTAL GRID RULINGS

      IF (XVPRM)  THEN
         CALL XVPARM( 'HORI', IPAR, ICOUNT, IDEF, 0 )
      ELSE
         CALL XVIPARM( 'HORI', IPAR, ICOUNT, IDEF, 0 )
      END IF

      IF (ICOUNT .GT. 0) THEN
          IF (NH.EQ.0) CALL ERROR(5,*5000)
          IF (ICOUNT .NE. 2*NH)  CALL ERROR(15,*5000)
          VERT = .FALSE.
          TRACK= .FALSE.
          SEARCH=.FALSE.
          CALL HORVER(HPAR(AHSL),HPAR(AHSS),NH,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,0)
      END IF


C       VERT - GET STARTING LINE, SAMPLE OF VERTICAL GRID RULINGS

      IF (XVPRM)  THEN
         CALL XVPARM( 'VERT', IPAR, ICOUNT, IDEF, 0 )
      ELSE
         CALL XVIPARM( 'VERT', IPAR, ICOUNT, IDEF, 0 )
      END IF

      IF (ICOUNT .GT. 0) THEN
          IF (NV.EQ.0) CALL ERROR(5,*5000)
          IF (ICOUNT .NE. 2*NV)  CALL ERROR(15,*5000)
          VERT = .TRUE.
          TRACK= .FALSE.
          SEARCH=.FALSE.
          CALL HORVER(HPAR(AVSL),HPAR(AVSS),NV,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,0)
      END IF

C  IF IN BATCH MODE, ALL PARAMETERS HAVE BEEN READ.  EXECUTE GO, PRINT, & EXIT.

      IF (.NOT. INT)  GOTO 1000
      if(xvprm) goto 5000

C
C  HTRACK, VTRACK, HSEARCH, VSEARCH    CAN BE ENTERED AS KEYWORDS.

      CALL XVIPARM( 'TRACK', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN

       IF  (CPAR .EQ. 'HTRACK ')    THEN       ! HTRACK

               IF (NH.EQ.0) CALL ERROR(5,*5000)
               ICOUNT = 0
               VERT = .FALSE.
               TRACK= .TRUE.
               SEARCH=.FALSE.
               CALL HORVER(HPAR(AHSL),HPAR(AHSS),NH,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000

       ELSE IF  (CPAR .EQ. 'VTRACK ')    THEN       ! VTRACK

               IF (NV.EQ.0) CALL ERROR(5,*5000)
               ICOUNT = 0
               VERT = .TRUE.
               TRACK= .TRUE.
               SEARCH=.FALSE.
               CALL HORVER(HPAR(AVSL),HPAR(AVSS),NV,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000

       ELSE IF  (CPAR .EQ. 'HSEARCH')    THEN       ! HSEARCH
               IF (NH.EQ.0) CALL ERROR(5,*5000)
               ICOUNT = 0
               VERT = .FALSE.
               TRACK= .FALSE.
               SEARCH=.TRUE.
               CALL HORVER(HPAR(AHSL),HPAR(AHSS),NH,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000

       ELSE IF  (CPAR .EQ. 'VSEARCH')    THEN       ! VSEARCH

               IF (NV.EQ.0) CALL ERROR(5,*5000)
               ICOUNT = 0
               VERT = .TRUE.
               TRACK= .FALSE.
               SEARCH=.TRUE.
               CALL HORVER(HPAR(AVSL),HPAR(AVSS),NV,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000

       END IF
      END IF

C  HTRACK, VTRACK, HSEARCH, VSEARCH    CAN BE ENTERED AS PARAMETERS

      CALL XVIPARM( 'HTRACK', IPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (NH.EQ.0) CALL ERROR(5,*5000)
          VERT = .FALSE.
          TRACK= .TRUE.
          SEARCH=.FALSE.
          CALL HORVER(HPAR(AHSL),HPAR(AHSS),NH,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000
      END IF

      CALL XVIPARM( 'VTRACK', IPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (NV.EQ.0) CALL ERROR(5,*5000)
          VERT = .TRUE.
          TRACK= .TRUE.
          SEARCH=.FALSE.
          CALL HORVER(HPAR(AVSL),HPAR(AVSS),NV,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000
      END IF

      CALL XVIPARM( 'HSEARCH', IPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (NH.EQ.0) CALL ERROR(5,*5000)
          VERT = .FALSE.
          TRACK= .FALSE.
          SEARCH=.TRUE.
          CALL HORVER(HPAR(AHSL),HPAR(AHSS),NH,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000
      END IF

      CALL XVIPARM( 'VSEARCH', IPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (NV.EQ.0) CALL ERROR(5,*5000)
          VERT = .TRUE.
          TRACK= .FALSE.
          SEARCH=.TRUE.
          CALL HORVER(HPAR(AVSL),HPAR(AVSS),NV,IPAR,PAR,
     .                      VERT,TRACK,SEARCH,ICOUNT)
               GOTO 5000
      END IF

C
C UP - MOVE THE DISPLAY WINDOW UP ONE SCREEN SIZE
C
      CALL XVIPARM( 'UP', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (DISID .NE. 0)  THEN
             SL=MAX0(1,SL-LSIZE(DISID))
             NL=MIN0(NLI-SL+1,LSIZE(DISID))
          END IF
          CALL DISPLA(PAR, LPAR)                 ! REDISPLAY.
               GOTO 5000
      END IF

C
C DOWN - MOVE THE DISPLAY WINDOW DOWN ONE SCREEN SIZE
C
      CALL XVIPARM( 'DOWN', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0 .AND. SL+LSIZE(DISID) .LE. NLI) THEN
          IF (DISID .NE. 0)  THEN 
             SL= SL+LSIZE(DISID)
             NL=MIN0(NLI-SL+1,LSIZE(DISID))
          END IF
          CALL DISPLA(PAR, LPAR)                 ! REDISPLAY.
               GOTO 5000
      END IF
C
C LEFT - MOVE THE DISPLAY WINDOW LEFT ONE SCREEN WIDTH
C
      CALL XVIPARM( 'LEFT', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (DISID .NE. 0)  THEN
             SS=MAX0(1,SS-SSIZE(DISID))
             NS=MIN0(NSI-SS+1,SSIZE(DISID))
          END IF
          CALL DISPLA(PAR, LPAR)                 ! REDISPLAY.
               GOTO 5000
      END IF

C
C RIGHT - MOVE THE DISPLAY WINDOW RIGHT ONE SCREEN WIDTH
C
      CALL XVIPARM( 'RIGHT', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0 .AND. SS+SSIZE(DISID) .LE. NSI) THEN
          IF (DISID .NE. 0)  THEN
             SS=SS+SSIZE(DISID)
             NS=MIN0(NSI-SS+1,SSIZE(DISID))
          END IF
          CALL DISPLA(PAR, LPAR)                 ! REDISPLAY.
               GOTO 5000
      END IF
C
C HOME - MOVE THE DISPLAY WINDOW TO THE UPPER LEFT OF THE IMAGE
C
      CALL XVIPARM( 'HOME', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (DISID .NE. 0)  THEN
              SL=1
              SS=1
              NL=MIN0(LSIZE(DISID),NLI)
              NS=MIN0(SSIZE(DISID),NSI)
          END IF
          CALL DISPLA(PAR, LPAR)                 ! REDISPLAY.
               GOTO 5000
      END IF

C
C DISP - DISPLAY SECTION OF IMAGE ON VIDEO DEVICE SELECTED PREVIOUSLY
C
      CALL XVIPARM( 'DISP', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
         CALL DISPLA(PAR,LPAR)         ! REDISPLAY.
         GOTO 5000
      END IF
C
C FIX - CORRECT SELECTED GRID INTERSECTIONS USING THE TRACKBALL
C
      CALL XVIPARM( 'FIX', IPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (NH.EQ.0 .OR. NV.EQ.0) CALL ERROR(5,*5000)
          ICNT = ICOUNT/2
          CALL THRU(FH,LH,IPAR,ICNT)
          IF (EFLAG) GOTO 5000
          CALL THRU(FV,LV,IPAR(1+ICNT),ICNT)
          IF (EFLAG) GOTO 5000
          IF (LH.GT.NH .OR. LV.GT.NV) CALL ERROR(12,*5000)
          CALL FIX(FH,LH,FV,LV,RPAR(AINSEC))
          GOTO 5000
      END IF
C
C PRINT - PRINT THE GRID INTERSECTION COORDINATES...
C
      CALL XVIPARM( 'PRINT', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (NH.EQ.0 .OR. NV.EQ.0) CALL ERROR(5,*5000)
          CALL PRSUB(PAR(AINSEC))
               GOTO 5000
      END IF
      
C GO - START THE GRID LOCATION ALGORITHM...

      CALL XVIPARM( 'GO', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .EQ. 0) GOTO 1900
1000  CONTINUE
        IF ( INT .AND. XVPRM)  GOTO 5000
        IF (NH.EQ.0 .OR. NV.EQ.0) CALL ERROR(5,*5000)
        DO I=1,NH
          IF (HPAR(AHSL+I-1).EQ.0) CALL ERROR(7,*5000)
        END DO
        DO I=1,NV
          IF (HPAR(AVSL+I-1).EQ.0) CALL ERROR(7,*5000)
        END DO
        CALL ALLOC(ALSAMP,NSI,2,PAR)
        CALL GO(PAR,NBYTES,HPAR(AHSL),HPAR(AHSS),HPAR(AVSL),HPAR(AVSS),
     *     HPAR(ALSAMP),HPAR(AMINL),HPAR(AMAXL),RPAR(AINSEC))
        IF (.NOT.INT) GO TO 2000

        VDSRN= OUTFILE
    
        CALL XVCLOSE(OUTFILE, ISTAT, ' ')
        CALL XVOPEN(OUTFILE,ISTAT,'OP','READ', ' ')
    
        IF (DISID .NE. 0)  THEN
          SL=1
          SS=1
          NL=MIN0(LSIZE(DISID),NLI)
          NS=MIN0(SSIZE(DISID),NSI)
        END IF

1900   CONTINUE      

C
C EXIT - COMPLETE PROCESSING
C
      CALL XVIPARM( 'EXIT', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .EQ. 0) GOTO 2900
2000  CONTINUE
          IF (.NOT.INT) CALL PRSUB(PAR(AINSEC))

C     WRITE MARK DATA SET.  MARK DATA SET HAS REAL*4 DATA.

      CALL XVOPEN(MARKFILE,ISTAT,'OP','WRITE', 'U_FORMAT','REAL',
     &            'O_FORMAT','REAL','U_NL',1,'U_NS',2*NH*NV, ' ')

C-----ADD THE GRID SIZE TO THE VICAR LABEL
************************************************************************
      CALL XLADD(MARKFILE,'HISTORY','GRID_NROW',NH,ISTAT,'FORMAT','INT',
     &           ' ')
      CALL XLADD(MARKFILE,'HISTORY','GRID_NCOL',NV,ISTAT,'FORMAT','INT',
     &           ' ')
      CALL XVWRIT( MARKFILE, PAR(AINSEC), ISTAT, ' ' )

      CALL XVCLOSE( OUTFILE, ISTAT, ' ' )
      CALL XVCLOSE( MARKFILE, ISTAT, ' ' )
      IF (WORKOPEN .EQ. 1) THEN
        CALL XVCLOSE( WORKFILE, ISTAT, ' ' )
        WORKOPEN = 0
      END IF

      CALL XVMESSAGE('EXIT interloc', ' ')
      IF (DISID .NE. 0)   CALL CLOSE_DEVICE(IUNIT)
      GOTO 6000

2900  CONTINUE
C
C RETRY - SET UP PROGRAM FOR RETRY OF 'GO' ALGORITHM
C
      CALL XVIPARM( 'RETRY', CPAR, ICOUNT, IDEF, 0 )
      IF (ICOUNT .GT. 0) THEN
          IF (VDSRN.EQ. INFILE) GOTO 5000
          CALL XVCLOSE(OUTFILE, ISTAT, ' ')
          IF (WORKOPEN .EQ. 1) THEN
            CALL XVCLOSE(WORKFILE, ISTAT, ' ')
            WORKOPEN = 0
          END IF
          CALL XVOPEN ( INFILE, ISTAT,  'OP', 'READ', ' ')
          CALL XVOPEN(OUTFILE,ISTAT,'OP','WRITE', ' ')

          VDSRN=INFILE
          IF (DISID .NE. 0)  THEN
            SL=1
            SS=1
            NL=MIN0(LSIZE(DISID),NLI)
            NS=MIN0(SSIZE(DISID),NSI)
          END IF
      END IF

5000  CONTINUE
      XVPRM = .FALSE.                      ! END OF PARAMETER PROCESSING LOOP
      CALL XVMESSAGE ('interloc READY', ' ')

      CALL XVINTRACT( 'IPARAM', 'Enter parameters:' )   !wait for inter. params.

      GOTO 100
6000  RETURN
      END

C********************************************************************
      SUBROUTINE DISPLA(PAR, LPAR)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      COMMON /COMNEW/ VBUF
      COMMON /SIZDIS/ LSIZE,SSIZE
      DIMENSION LSIZE(5),SSIZE(5)

         common /xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     .                  ncurs, u, c, iform, iblink, iconoff, v,
     .                  w, section, gplane

      DIMENSION PAR(*)
      BYTE LPAR(*)

C==================================================================

          IF (DISID.EQ.0)   THEN
              CALL INITD( 'V1' )  ! INITIALIZE DISPLAY THE 1ST TIME.
              SL=1
              SS=1
              NL=MIN0(LSIZE(DISID),NLI)
              NS=MIN0(SSIZE(DISID),NSI)
          END IF

          IF (VBUF.EQ.0) CALL ALLOC(VBUF,2*NSI,1,PAR)
          IERR   = XDIFILL ( U, 1, 0 ) 

          CALL VPIC(VDISID,VDSRN,LPAR(VBUF),SL,SS,NLI,NSI,1,1,NL,NS,
     *      DISID,1,0,0)
          RETURN
          END

C********************************************************************
      SUBROUTINE GO(IOBUF,IOBLEN,HSL,HSS,VSL,VSS,LSAMP,MINL,MAXL,INSEC)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      PARAMETER  (IS_PAR=100)            ! MAXIMUM NUMBER OF LINE BUFFERS.
      INTEGER IS(IS_PAR)
      REAL INSEC(2,NV,NH)
      INTEGER*2 LSAMP(NSI),HSL(*),HSS(*),VSL(*),VSS(*),MINL(*),MAXL(*)
      BYTE IOBUF(*)
      CHARACTER*32 TXT

C==================================================================

C OPEN WORK FILE

      IF (WORKOPEN .EQ. 0) THEN
      CALL XVOPEN(WORKFILE,ISTAT,'OP','WRITE', 'U_FORMAT','HALF',
     &            'O_FORMAT','HALF','U_NL',NH,'U_NS', NSI, ' ')
      WORKOPEN = 1
      END IF

      CALL XVMESSAGE(' ',' ')
      WRITE (TXT,9900) IOBLEN
9900  FORMAT ('0I/O BUFFER SIZE ',I8,' BYTES.')
      CALL XVMESSAGE(TXT(2:32),' ')
      NBUF=IOBLEN/NSI
      IF (NBUF .LT.3 )  CALL ERROR(4,*999)
      NBUF = MIN0( IS_PAR, NBUF )

      IS(1) = 1
      DO I = 2, NBUF                   ! STORE THE STARTING INDICES OF THE
         IS(I) = IS(I-1) + NSI         ! NBUF LINE BUFFERS.
      END DO

      MINLINE=32769
      MAXLINE=0
      DO 100 NHOR=1,NH
      CLINE=HSL(NHOR)
      MINL(NHOR)=CLINE
      MAXL(NHOR)=CLINE
      CALL INITIO(CLINE,IOBUF)
      K=HSS(NHOR)
      DO 200 SAMP=1,K
200   LSAMP(SAMP)=CLINE
      K=K+1
      DO 300 SAMP=K,NSI
      LOW=IPIX(CLINE-1,SAMP,IOBUF,IS)
      THIS=IPIX(CLINE,SAMP,IOBUF,IS)
      HIGH=IPIX(CLINE+1,SAMP,IOBUF,IS)
      IF (LOW.LT.THIS .AND. LOW.LT.HIGH) CLINE=CLINE-1
      IF (HIGH.LT.THIS .AND. HIGH.LT.LOW) CLINE=CLINE+1
      IF (CLINE.LT.MINL(NHOR)) MINL(NHOR)=CLINE
      IF (CLINE.GT.MAXL(NHOR)) MAXL(NHOR)=CLINE
      LSAMP(SAMP)=CLINE
300   CONTINUE
      CALL XVWRIT( WORKFILE, LSAMP, ISTAT, ' ')

100   CONTINUE
      IF (WORKOPEN .EQ. 1) THEN
        CALL XVCLOSE( WORKFILE, ISTAT, ' ' )
        WORKOPEN = 0
      END IF
      CALL XVOPEN(WORKFILE,ISTAT,'OP','READ', 'U_FORMAT','HALF',
     &            'O_FORMAT','HALF', ' ')
      WORKOPEN = 1

      MINLINE=32769
      MAXLINE=0
      NIDS=0
      MINIDS=1
      CALL INITIO(1,IOBUF)
C     NOW DO VERTICAL RULINGS
      DO 400 LINE=1,NLI
      DO 500 NVER=1,NV
      X=VSS(NVER)
      LOW=IPIX(LINE,X-1,IOBUF,IS)          ! I MOVED THIS STATEMENT BEFORE IF
                                           ! TO KEEP MAXLINE >= LINE.
      IF (LINE.LE.VSL(NVER)) GO TO 520
      THIS=IPIX(LINE,X,IOBUF,IS)
      HIGH=IPIX(LINE,X+1,IOBUF,IS)
      IF (LOW.LT.THIS .AND. LOW.LT.HIGH) X=X-1
      IF (HIGH.LT.THIS .AND. HIGH.LT.LOW) X=X+1
520   VSS(NVER)=X
      CALL WPIX(LINE,X,IOBUF,IS)
500   CONTINUE
      IF (MINIDS.GT.NH) GO TO 800
      DO 700 NHOR=MINIDS,NH
      IF (LINE.LT.MINL(NHOR)) GO TO 800
      IF (LINE.GT.MAXL(NHOR)) GO TO 710
      IF (NIDS.EQ.NHOR) GO TO 750
      NIDS=NHOR
      CALL XVREAD(WORKFILE, LSAMP, ISTAT, 'LINE', NIDS, ' ')
750   CONTINUE
      DO 760 NVER=1,NV
      X=VSS(NVER)
      IF (LSAMP(X).NE.LINE) GO TO 760
      INSEC(1,NVER,NHOR)=FLOAT(LINE)
      INSEC(2,NVER,NHOR)=FLOAT(X)
760   CONTINUE
      DO 770 J=1,NSI
      IF (LSAMP(J).EQ.LINE) CALL WPIX(LINE,J,IOBUF,IS)
770   CONTINUE
      GO TO 700
710   MINIDS=NHOR+1
700   CONTINUE
800   CONTINUE
      CALL WPIX(LINE,-1,IOBUF,IS)
400   CONTINUE
      CALL XVCLOSE(INFILE, ISTAT, ' ')
      RETURN
999   CALL ABEND
      END
C********************************************************************
      SUBROUTINE PRSUB(INSEC)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      REAL INSEC(2,NV,NH)

      CHARACTER*22 MSG

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('( ROW,COL ) LINE SAMP', ' ')
      CALL XVMESSAGE('----------- ---- ----', ' ')
      DO 1000 NHOR=1,NH
      DO 2000 NVER=1,NV
      WRITE (MSG,9910) NHOR,NVER,NINT(INSEC(1,NVER,NHOR)),NINT(INSEC(2,
     +NVER,NHOR))
9910  FORMAT (' (',I4,',',I4,') ',I4,' ',I4)
      CALL XVMESSAGE(MSG(2:22),' ')
      IF (INSEC(1,NVER,NHOR) .EQ. 0.0 .OR. INSEC(2,NVER,NHOR) .EQ. 0.0)
     .    CALL XVMESSAGE('** WARNING: GRID INTERSECTION NOT FOUND',' ')
2000  CONTINUE
1000  CONTINUE
      IF (.NOT. INT) CALL XVMESSAGE(' ', ' ')
      RETURN
      END
C********************************************************************
      SUBROUTINE FIX(FH,LH,FV,LV,INSECX)
C********************************************************************

      IMPLICIT INTEGER (A-Z)
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      REAL*4 INSECX(2,NV,NH)

      DO 100 NHOR=FH,LH
      DO 100 NVER=FV,LV
      CALL GETCUR(L,S)
      INSECX(1,NVER,NHOR)=FLOAT(L)
      INSECX(2,NVER,NHOR)=FLOAT(S)
100   CONTINUE
      RETURN
      END
C********************************************************************
      SUBROUTINE INITIO(L,IOBUF)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      BYTE IOBUF(*)

      IF ( L .GE. MINLINE .AND. L .LE. MAXLINE ) RETURN
      L1 = MAX0(1, L - NBUF/2 )
      K=MIN0(NBUF, NLI-L1+1)    
      IS = 1
      DO 100 I=1,K
      CALL XVREAD(INFILE, IOBUF(IS), ISTAT, 'LINE', L1+I-1, ' ')
      IS = IS + NSI
100   CONTINUE
      FIRST=1
      MINLINE = L1
      MAXLINE = L1+K-1
      RETURN
      END
C********************************************************************
      SUBROUTINE WPIX( LINE, SAMP, IOBUF, IS )
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      INTEGER IS(*)       ! STARTING INDICES OF LINE BUFFERS.
      BYTE IOBUF(*)
      BYTE MAXDN
      DATA MAXDN / 'FF'X /     ! DN OF 255.

      KBUF = LINE - MINLINE + FIRST
      IF ( KBUF .GT. NBUF )  KBUF = KBUF - NBUF
      IF (SAMP.LT.0) GO TO 900
      INDEX = IS(KBUF) + SAMP - 1
      IOBUF(INDEX) = MAXDN
      RETURN
900   CALL XVWRIT(OUTFILE, IOBUF(IS(KBUF)), ISTAT, ' ')
      RETURN
      END
C********************************************************************
      SUBROUTINE ERROR(N,*)
C********************************************************************
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      K=IABS(N)

      IF (K .EQ. 1)  CALL XVMESSAGE('REQUIRED DATA SET MISSING',' ')
      IF (K .EQ. 2)  CALL XVMESSAGE('OUTPUT D.S. LRECL TOO SMALL',' ')       
      IF (K .EQ. 3)  CALL XVMESSAGE('IDS LRECL TOO SMALL',' ')
      IF (K .EQ. 4)  CALL XVMESSAGE('MORE CORE NEEDED FOR ARRAYS',' ')
      IF (K .EQ. 5)  CALL XVMESSAGE('NH OR NV UNSPECIFIED',' ')
      IF (K .EQ. 6)  CALL XVMESSAGE('DISPLAY DEV NOT SELECTED',' ')
      IF (K .EQ. 7)  CALL XVMESSAGE('A STARTING POINT IS UNDEFIND',' ')
      IF (K .EQ. 8)  CALL XVMESSAGE('BAD PARAMETER SPECIFICATION',' ')
      IF (K .EQ. 9)  CALL XVMESSAGE('COULD NOT OPEN DISPLAY DEVICE',' ')
      IF (K .EQ. 10) CALL XVMESSAGE('ERROR IN SPECIFYING RANGE',' ')
      IF (K .EQ. 11) CALL XVMESSAGE('MARK D.S. LRECL TOO SMALL',' ')
      IF (K .EQ. 12) CALL XVMESSAGE('INVALID ROW OR COLUMN NUMBER',' ')
      IF (K .EQ. 13) CALL XVMESSAGE('NH ALREADY SPECIFIED',' ')
      IF (K .EQ. 14) CALL XVMESSAGE('NV ALREADY SPECIFIED',' ')
      IF (K .EQ. 15)   
     &  CALL XVMESSAGE('INCORRECT NUMBER OF PARAMETER VALUES',' ')

      EFLAG=.TRUE.
      IF (N.LT.0) RETURN
      IF (INT) RETURN 1
      CALL ABEND
      END
C********************************************************************
      SUBROUTINE HORVER(LIN,SAM,N,IPAR,PAR,VERT,TRACK,SEARCH,ICOUNT)
C********************************************************************
C      CALL XVIPARM( 'VSEARCH', IPAR, ICOUNT, IDEF, 0 )
C      IF (ICOUNT .GT. 0) THEN
C          IF (NV.EQ.0) CALL ERROR(5,*5000)
C          VERT = .TRUE.
C          TRACK= .FALSE.
C          SEARCH=.TRUE.
C          CALL HORVER(HPAR(AVSL),HPAR(AVSS),NV,IPAR,PAR,
C     .                      VERT,TRACK,SEARCH,ICOUNT)
C      END IF

      IMPLICIT INTEGER (A-Z)

      INTEGER*4 IPAR(*), PAR(*)
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      COMMON /COMNEW/ VBUF
      LOGICAL VERT, TRACK, SEARCH
      INTEGER*2 LIN(*), SAM(*), TFLAG

      F=1
      L=N

      IF (TRACK)  THEN
         IF (ICOUNT .GT. 0)  CALL THRU(F,L,IPAR,ICOUNT)
         IF (L.GT.N) CALL ERROR(12,*999)
         IF (DISID.EQ.0) CALL ERROR(6,*999)
         DO I=F,L
            CALL GETCUR(DL,DS)
            LIN(I)=DL
            SAM(I)=DS
         END DO
      ELSE IF (SEARCH)  THEN
         IF (ICOUNT .GT. 0)  CALL THRU(F,L,IPAR,ICOUNT)
         IF (L.GT.N) CALL ERROR(12,*999)
         IF (DISID.EQ.0) CALL ERROR(6,*999)
         CALL WCURSE(IUNIT,CISID,1,1)    ! START CURSOR IN UPPER LEFT.
         CALL XVMESSAGE('SEARCHING...', ' ')
         IF (VBUF.EQ.0) CALL ALLOC(VBUF,2*NSI,1,PAR)
         DO I=F,L
            CALL NXTCUR(DL,DS,VERT,PAR)
            TFLAG = 0
            DO WHILE (DL.GT.NLI .OR. DS.GT.NSI)
             CALL XVMESSAGE('THE CURSOR IS OUTSIDE THE IMAGE ... ',' ')
          CALL XVMESSAGE('PLACE CURSOR WITHIN IMAGE AND TRY AGAIN',' ')
              TFLAG = TFLAG + 1
              CALL NXTCUR(DL,DS,VERT,PAR)
              IF (TFLAG.GT.5)   GOTO 100
            END DO

100         LIN(I)=DL
            SAM(I)=DS

         END DO

      ELSE
         K = 1
         DO I=F,L
            WORD = IPAR(K)
            K = K + 1
            IF (WORD.LT.1 .OR. WORD.GT.NLI) CALL ERROR(8,*999)
            LIN(I)=WORD
            WORD = IPAR(K)
            K = K + 1
            IF (WORD.LT.1 .OR. WORD.GT.NSI) CALL ERROR(8,*999)
            SAM(I)=WORD
         END DO
      END IF

999   RETURN
      END
C********************************************************************
      SUBROUTINE NXTCUR(DL,DS,VERT,PAR)     
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'fortport'
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      COMMON /COMNEW/ VBUF

      LOGICAL FOUND
      LOGICAL VERT
      INTEGER IFUDGE
      BYTE PAR(*)
      REAL*8 SUM,SUMSQ

      CALL RCURSE(IUNIT,CISID,LDS,SDS)
      IF (.NOT.VERT) GO TO 200
      RDO=SDS
      RNL=1
      RNS=MIN0(100,NS-SDS+1)
      GO TO 300
200   RDO=LDS
      RNL=MIN0(100,NL-LDS+1)
      RNS=1
300   RLIM=MAX0(RNL,RNS)
      IF  (RNS .EQ. 1)  THEN
        DO I=1, RNL                               ! READ A VERTICAL LINE.
           CALL RLINE(VDISID,PAR(VBUF+I-1),LDS+I-1,SDS,1,1)
        END DO
      ELSE
           CALL RLINE(VDISID,PAR(VBUF),LDS,SDS,RNL,RNS)
      END IF
      SUM=0.D0
      SUMSQ=0.D0
      COUNT=RLIM-8
      DO 100 I=9,RLIM
      IFUDGE=BYTE2INT(PAR(VBUF+I-1))
      SUM=SUM+DFLOAT(IFUDGE)
      SUMSQ=SUMSQ+DFLOAT(IFUDGE)**2
100   CONTINUE
      SUM=SUM/DFLOAT(COUNT)
      SUMSQ=DSQRT(DABS(SUMSQ/DFLOAT(COUNT) - SUM*SUM))
      MINIM=IDINT(SUM-3.0D0 * SUMSQ)
C IN OTHER WORDS, MINIM IS A DN VALUE 2 SIGMA BELOW THE MEAN DN
      FOUND=.FALSE.
      DO 600 I=9,RLIM
      J=I-1
      IFUDGE=BYTE2INT(PAR(VBUF+J))
      IF (IFUDGE.GT.MINIM) GO TO 700
      MINIM=IFUDGE
      FOUND=.TRUE.
      GO TO 600
700   IF (FOUND) GO TO 800
600   CONTINUE
800   CONTINUE
      J=J+RDO
      IF (.NOT.VERT) GO TO 900
      CALL WCURSE(IUNIT,CISID,LDS,J)
      GO TO 920
900   CALL WCURSE(IUNIT,CISID,J,SDS)
920   CONTINUE
      CALL GETCUR(DL,DS)
      RETURN
      END
C********************************************************************
      SUBROUTINE INITD(ARG)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN

         common /xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     .                  ncurs, u, c, iform, iblink, iconoff, v,
     .                  w, section, gplane

      COMMON /SIZDIS/ LSIZE,SSIZE
      DIMENSION LSIZE(5),SSIZE(5)
      LOGICAL INT, EFLAG

      BYTE ARG(2),FUDGE(4)
      CHARACTER*4 TB1
      DATA TB1 /'T1  '/
      INTEGER STAB(256)

      CALL OPEN_DEVICE(IUNIT)
      CALL BW_MODE(IUNIT)
      CALL AUTOTRACKING_MODE(.TRUE.,IUNIT) !ENABLE AUTOTRACKING OF TRACKBALL

      FUDGE(1)=ARG(1)
      IF (FUDGE(1) .LT. 0) THEN
         ITEMP1 = 256 + FUDGE(1)
      ELSE
         ITEMP1 = FUDGE(1)
      ENDIF
      FUDGE(2)=ARG(2)
      IF (FUDGE(2) .LT. 0) THEN
         ITEMP2 = 256 + FUDGE(1)
      ELSE
         ITEMP2 = FUDGE(2)
      ENDIF
      IFUDGE = ITEMP1 + (ITEMP2 * 256)
      VDISID=IFUDGE
      FUDGE(1) = 0
      FUDGE(2) = 0
      IFUDGE=0
      FUDGE(4)=ARG(2)

      DISID=1
      LSIZE(1) = 512               ! SCREEN SIZE FOR DE ANZA MONITORS.
      SSIZE(1) = 512

      CISID=1
      DO 400 I=1,256
400   STAB(I)=I-1
      CALL DSET(VDISID,'VLT',STAB)
      CALL CSET(CISID,'VISIBLE')
      CALL CSET(CISID,'NBLINK ')
      CALL WCURSE(IUNIT,CISID,1,1)        ! START CURSOR IN UPPER LEFT.
      IERR = XDIFILL( u, 1, 0)
999   RETURN
      END
C********************************************************************
      SUBROUTINE ALLOC(INDEX,NELM,ELSIZ,ARRAY)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      BYTE ARRAY(*)
      BYTE ZERO
      DATA ZERO / 0 /

      NBYT=(NBYTES/ELSIZ)*ELSIZ
      IF (NBYT.LT.NELM*ELSIZ) CALL ERROR(4,*999)
100   INDEX=(NBYT-NELM*ELSIZ)/ELSIZ+1
      NBYTES=(INDEX-1)*ELSIZ
      J=NBYTES+1
      K=ELSIZ*NELM+NBYTES
      DO 200 I=J,K
200   ARRAY(I)=ZERO
      RETURN
999   CALL ABEND
      END
C==================================================================
C********************************************************************
      SUBROUTINE GETCUR(L,S)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      BYTE MAXDN(9)
      DATA MAXDN /4*'FF'X,'00'X,4*'FF'X /
      INTEGER*4  ITEST(2)

        CALL XVMESSAGE('Position cursor and press <RETURN>', ' ')
        CALL XVINTRACT( 'CURSOR', 'Just <RETURN' )
        CALL RCURSE( IUNIT,CISID, LDS, SDS )

      L=LDS+SL-1
      S=SDS+SS-1
         CALL XVIPARM( 'TEST', ITEST, ICOUNT, IDEF, 2 )
         IF ( IDEF .NE. 1 )  THEN          ! GET CURSOR POSITION FROM PARAMETER
              L  = ITEST(1)                ! IF USING 'TEST'.
              S  = ITEST(2)
         END IF

      CALL VLINE(VDISID,MAXDN,LDS-1,SDS-1,1,3)
      CALL VLINE(VDISID,MAXDN(4),LDS,SDS-1,1,3)
      CALL VLINE(VDISID,MAXDN(7),LDS+1,SDS-1,1,3)
      RETURN
      END
C********************************************************************
      SUBROUTINE THRU(F,L,PAR,ICOUNT)
C********************************************************************
      IMPLICIT INTEGER (A-Z)
      INTEGER PAR(*)
C==================================================================
      F = PAR(1)
      L=F
      IF (ICOUNT .EQ. 2)  L = PAR(2)
      IF (F.LT.1 .OR. L.LT.F) CALL ERROR(10,*999)
999   RETURN
      END

C********************************************************************
      FUNCTION IPIX( LINE, SAMP, IOBUF, IS )
C********************************************************************
 
      IMPLICIT INTEGER  (A-Z)
      INCLUDE 'fortport'
      COMMON /COMIOS/ NBUF, FIRST, MINLINE, MAXLINE, NLI, NSI

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN
      LOGICAL INT, EFLAG

      BYTE IOBUF(*)
      INTEGER IS(*)

C======================================================================

      IF (LINE .LT. 1  .OR. LINE .GT. NLI )    GOTO 6000
      IF (SAMP .LT. 1  .OR. SAMP .GT. NSI )    GOTO 6000

      IF ( LINE .LT. MINLINE ) THEN
      DO WHILE ( LINE .LT. MINLINE ) 
         MINLINE = MINLINE - 1
         MAXLINE = MAXLINE - 1
         FIRST = FIRST - 1
         IF (FIRST .EQ. 0)  FIRST=NBUF
         CALL XVREAD(INFILE,IOBUF(IS(FIRST)),ISTAT,'LINE',MINLINE,' ')
      END DO
      END IF

      IF ( LINE .GT. MAXLINE ) THEN
      DO WHILE ( LINE .GT. MAXLINE ) 
         MINLINE = MINLINE + 1
         MAXLINE = MAXLINE + 1
         CALL XVREAD(INFILE,IOBUF(IS(FIRST)),ISTAT,'LINE',MAXLINE,' ')
         FIRST = FIRST + 1
         IF (FIRST .GT. NBUF)  FIRST=1
      END DO
      END IF

      KBUF = LINE - MINLINE + FIRST
      IF ( KBUF .GT. NBUF )  KBUF = KBUF - NBUF
      INDEX = IS(KBUF) + SAMP - 1
      IPIX = BYTE2INT( IOBUF(INDEX) )
      GOTO 8000

6000  CONTINUE
      IPIX = 256

8000  CONTINUE
      RETURN
      END
C**********************************************************************
C
C       VPIC: version with ZOOM but no histogram.
C             use together with subroutine EXPANDO.
C
C   REVISION HISTORY
C     2-85   SP  CHANGED DIMENSION OF BUF FROM 1 TO *.
C
C********************************************************************
       SUBROUTINE VPIC(IDISID,IDSRN,BUF,ISL,ISS,NL,NS,ISLDISP,ISSDISP,
     .                NLDISP,NSDISP,ISW,IZOOM,HSFLG,HISBUF)
C********************************************************************

C PURPOSE: DISPLAYS A PICTURE ON VIDEO SCREEN.
C      
C      ISL, ISS - STARTING LINE AND SAMPLE IN FILE FOR UPPER-LEFT CORNER
C                 OF DISPLAY WINDOW.
C      NL, NS   - TOTAL SIZE OF IMAGE FILE.
C      ISLDISP, - SCREEN COORDINATES OF UPPER-LEFT CORNER OF DISPLAY WINDOW.
C      ISSDISP
C      NLDISP,  - SIZE OF DISPLAY WINDOW.
C      NSDISP
C      IZOOM    - ZOOM FACTOR. >0 FOR MAGNIFY. <0 FOR SHRINK. =1 IS REGULAR.
C                 =0 IS INVALID.
C
C  VPIC CALLS ABEND FOR DISK READ ERRORS.

       BYTE BUF(*),HISBUF(1)
       LOGICAL HSFLG
C
C=============================================================

       IF (IZOOM .EQ. 0)   GOTO 8000

       IF (IZOOM .LT. 0)  IZOOMP = -IZOOM
       NSLEFT = NS - ISS + 1            ! NUMBER OF SAMPLES AND LINES
       NLLEFT = NL - ISL + 1            ! FROM ISL,ISS TO END OF FILE.

       IF (IZOOM .GT. 0)  THEN    ! DISPLAY TO EDGE OF WINDOW OR EDGE OF IMAGE.
           NLDISPLAY = MIN0( NLDISP, IZOOM*NLLEFT )  ! LINES TO DISPLAY
           NSDISPLAY = MIN0( NSDISP, IZOOM*NSLEFT )  ! SAMPLES TO DISPLAY.
           NSAMP     = (NSDISPLAY+IZOOM-1) / IZOOM
       ELSE
           NLDISPLAY = MIN0( NLDISP, (NLLEFT+IZOOMP-1)/IZOOMP )
           NSDISPLAY = MIN0( NSDISP, (NSLEFT+IZOOMP-1)/IZOOMP )
           NSAMP     = (NSDISPLAY-1)*IZOOMP +1
       END IF

       LINE   = ISL
       LDISP  = ISLDISP
       LREPEAT= 0

       DO I = 1, NLDISPLAY

         !!!!!!!!!!!!! ROUTINE GETLINE:  GET A LINE OF DATA TO SEND TO SCREEN.

         IF (LREPEAT .EQ. 0)  THEN       ! READ FILE IF NOT REPEATING LINE.
             CALL XVREAD( IDSRN, BUF, ISTAT, 'LINE', LINE, 'SAMP',ISS,
     .             'NSAMPS', NSAMP, ' ')
             IF (IZOOM .NE. 1)  CALL EXPANDO( BUF, NSDISPLAY, IZOOM )
         END IF

         IF (IZOOM .GT. 0) THEN
             LREPEAT = LREPEAT+1
             IF (LREPEAT .GE. IZOOM)  THEN
                 LREPEAT = 0
                 LINE = LINE+1
             END IF
         ELSE
             LREPEAT = 0
             LINE = LINE + IZOOMP
         END IF
          !!!!!!!!!!!!!!!! END GETLINE !!!!!!!!!!!!!!

C         DISPLAY THE LINE

         CALL VLINE( IDISID, BUF, LDISP, ISSDISP, 1, NSDISPLAY )
         LDISP = LDISP + 1

       END DO

8000   RETURN
       END
C********************************************************************
      SUBROUTINE EXPANDO(BUF,NSDS,IZOOM)
C********************************************************************

C PURPOSE:  EXPANDS IMAGE DATA BY ZOOM FACTOR.
      BYTE BUF(*)
C
C
      IF(IZOOM.LT.0)  GO TO 201
C
      NS=(NSDS+IZOOM-1)/IZOOM
      KK=NS*IZOOM
      DO 110 J=1,NS
      JJ=NS-J+1
      DO 100 K=1,IZOOM
      BUF(KK)=BUF(JJ)
100   KK=KK-1
110   CONTINUE
      RETURN
C
201   CALL MVE(1,NSDS,BUF,BUF,-IZOOM,1)
      RETURN
      END
C**********************************************************************
C
c        open_device
c
c         the devopen call is the first call that must be
c         made to the d routines. it will open the required
c         unit, configure them as required,
c         and activate them so that they can be read to and
c         from. the graphics plane is set to 4 and turned on.
c         a font is read in; character height is specified;
c         and text angle rotation is set to 0.
c
c         calling sequence ( iunit )
c         where :
c                iunit - device logical unit no.
c
          SUBROUTINE OPEN_DEVICE( IIUNIT )

          IMPLICIT INTEGER (A-Z)
          INTEGER  H, W
          INTEGER  CSETUP(4), C, U, IFORM, IBLINK, ICONOFF
          INTEGER  GPLANE, NLUTS, NIMPS, MAXSAMPS, XDSGRAPH
          INTEGER  IGRAPH, NCURS, NINTIO, SECTION, LMAX, SMAX
          INTEGER  PLANE,XDSSECTION
          LOGICAL  CAUTO, FLAG
          INTEGER  XDEACTION, XDDUNIT, XDDOPEN, XDDACTIVATE
          INTEGER  XDDCONFIGURE, XDTFONT, XDTSIZE
          INTEGER  XDTROTATE, XDGON, XDLRAMP, XDLCONNECT
          INTEGER  XDGCONNECT, XDGLINIT
          REAL     S, ANGLE

      COMMON /COMALL/ VDSRN, VDISID, DISID, CISID, NH, NV, NBYTES,
     .                NPW, NLAB12, KW, INT, EFLAG, SL, SS, NL, NS,
     .                INFILE, OUTFILE, MARKFILE, WORKFILE, IUNIT,
     .                WORKOPEN

      common /xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     .               ncurs, u, c, iform, iblink, iconoff, v,
     .               w, section, gplane
          DATA C / 1 /, CSETUP / 0, 0, 0, 0/
          DATA H / 7 /, S / 1.0 /
          DATA IFORM / 0 /, CAUTO / .true. / 
          DATA IBLINK / 0 /, ICONOFF / 0 / 
          DATA SECTION /1/
          DATA W / 1 / 
c
c        use default cursor and default unit
c
         data u / 1 /

          iunit = iiunit 
          U = IUNIT
c
c         open unit u
c
          IERR = XDEACTION( 2, 2, 3 )
          IERR = XDDUNIT( U )
          IERR = XDDOPEN( U )
c
c         activate the display unit so that we can write on it
c
          FLAG = .TRUE.
          IERR = XDDACTIVATE( U, FLAG )
c
c         now configure the display (csetup is all 0's - default)
c
          IERR = XDDCONFIGURE( U, CSETUP )
c
c         find out what type of device we have
c
          IERR = XDDINFO( U, 3, 1, NLUTS )
          IERR = XDDINFO( U, 4, 1, NIMPS )
          IERR = XDDINFO( U, 5, 1, MAXLINES )
          IERR = XDDINFO( U, 6, 1,  MAXSAMPS )
          IERR = XDDINFO( U, 30, 1, IGRAPH )
          IERR = XDDINFO( U, 34, 1, GPLANE )
          IERR = XDDINFO( U, 48, 1, NCURS )
          IERR = XDDINFO( U, 60, 1, NINTIO )
          LMAX = MAXLINES 
          SMAX = MAXSAMPS
c
c         read in a font file
c
          IFONT = 1
          IERR  = XDTFONT(IFONT) 
c
c         set the initial size
c
          IF ( LMAX .LE. 512 ) THEN
            H = 7
          ELSE 
            H = 14
          END IF 
          IERR = XDTSIZE(H,S) 
c
c         rotate at 0 degrees 
c
          ANGLE = 0.0
          IERR = XDTROTATE( ANGLE )
c
c         turn on the cursor
c
          IF ( NCURS .GT. 0 ) THEN
           ICONOFF = 1
           IERR = XDCON( U, C, IFORM, IBLINK ) 
          END IF 
c
c         and the ramps
c
          DO N1 = 1, NLUTS 
           NSECTION = XDSSECTION( U, N1)
           IERR = XDGLINIT( U, NSECTION)
           IERR = XDLRAMP ( U, N1, NSECTION )
           PLANE = XDSGRAPH(U)
           IERR = XDLCONNECT ( U, PLANE, N1, NSECTION, .FALSE. )
          END DO
c
c         connect the graphics plane to image plane 
c
          IF ( IGRAPH .GT. 0 ) THEN 
           PLANE = XDSGRAPH (U)
           IERR = XDDINFO ( U, 35, 1, SECTION)
           IERR = XDGCONNECT (U, PLANE, SECTION, .FALSE. )
c
c          turn on the graphics overlay plane
c
           IERR = XDGON (U) 
          END IF
c
          IUNIT = U 
          IIUNIT = U 
          RETURN
          END
C**********************************************************************
C
c        CLOSE_DEVICE
c
c         to deactivate the ability to modify the display unit u
c         and to deallocate it for the next user
c
c
	 SUBROUTINE CLOSE_DEVICE(IUNIT)
c
c        deactivate the device 
c
         INTEGER U
         LOGICAL XDDCLOSE, XDDACTIVATE, FLAG

         U = IUNIT
         FLAG = .FALSE. 
         IERR = XDDACTIVATE ( U, FLAG)
c
c        now close unit
c
         IERR = XDDCLOSE(U)
c
         RETURN
         END
C********************************************************************
C
c        bw_mode
c
c         this is the routine that connects image 1 to lut 1
c         2, and 3 and turns on the linear ramps.
c
          SUBROUTINE BW_MODE(IUNIT)
c
          IMPLICIT INTEGER (A-Z)
          INTEGER U, N1, SECTION, NLUTS
          INTEGER XDSSECTION,XDSGRAPH,PLANE
          INTEGER XDLCONNECT, XDLRAMP, XDDINFO
          common /xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     .                   ncurs, u, c, iform, iblink, iconoff, v,
     .                   w, section, gplane

          U = IUNIT
          PLANE = XDSGRAPH(U)
          IERR = XDDINFO ( U, 3, 1, NLUTS )
         
          DO N1 = 1, NLUTS 
           SECTION = XDSSECTION(U,N1)
           ICOLOR = 0
           IERR = XDLCONNECT (U,1,N1,SECTION, .FALSE.)
c
c          and the ramp
c
           IERR = XDLRAMP (U,N1,SECTION) 
          END DO
         RETURN 
         END
C
C**********************************************************************
C
c        AUTOTRACKING_MODE
c	
c         this is the routine that turns autotracking on
c
          SUBROUTINE AUTOTRACKING_MODE( ON, IUNIT )
          LOGICAL ON
          INTEGER XDCAUTOTRACK, XDDINFO
          INTEGER U, NINTIO, C
          DATA  C /1/

          U = IUNIT
          NINTIO=0
          IERR=0
          IERR = XDDINFO( U, 60, 1, NINTIO )
c
          IF ( NINTIO .GT. 0 ) THEN
           IF ( ON ) THEN 
              AUTOFLAG = 1
           ELSE
              AUTOFLAG = 0
           END IF
           IERR = XDCAUTOTRACK ( U,C,0,AUTOFLAG)
          END IF 
          RETURN
          END
c**************************************************************
	subroutine rcurse(idev,cur,iline,isamp)
	implicit integer (a-z)
	integer iline,isamp
	logical xst
c
	xst = xdclocation(idev,cur,x,y)
	if( .not. xst) go to 10
	iline = y
	isamp = x
c
	return
10	call XVMESSAGE('xdclocation error',' ')
	return 
	end
c**************************************************************
	subroutine wcurse(idev,cur,iline,isamp)
	implicit integer (a-z)
	integer iline,isamp
c	logical xst,xdcset
	integer xst,xdcset
c
	x = isamp
	y = iline
	xst = xdcset(idev,cur,x,y)
	if( xst .ne. 1) call XVMESSAGE('xdcset error',' ')
	return
	end
c**************************************************************
c        purpose:
c        this is the interface subroutine between the
c        display device interface routines and the
c        ramtek 'D' routines
c
         subroutine dsub
         IMPLICIT INTEGER (A-Z)

         integer  xdlwrite
         integer  xdilineread, xdilinewrite
         integer  xdcoff, xdcon
         integer  u, ilad, isad, nsamps
         integer  ivd
         integer  c, iform, iconoff, iblink
         integer  section
         integer  gplane
         integer  larray(256)
         integer  n
         byte buf(1)
         integer v, w
         character vdisid

c        save all pertinant variables for later entries
c
         common /xddev/ nluts, nimps, maxlines, maxsamps, igraph, 
     .                  ncurs, u, c, iform, iblink, iconoff, v,
     .                  w, section, gplane

c
c        set up the cursor array for cset here
c
         character*8 ikey(7), key
         data ikey(1) / 'VISIBLE'/
         data ikey(2) / 'INVISIB'/
         data ikey(3) / 'BLINK  '/
         data ikey(4) / 'NBLINK '/
c
         data vdisid / 'V' /
c
c------------------------------------------------------------------
c
c        vline/rline
c         vline will write from 1 to n lines of information
c         to a video device while rline will read information
c         from the video device.
c
c        calling sequence ( idisid, buf, islds, issds, nlds, nsds )
c        where:
c              idisid - video device logical unit no.
c              buf    - buffer for transfer of information
c              islds  - starting line coordinate
c              issds  - starting sample coordinate
c              nlds   - no. of lines to be written
c              nsds   - no. of samples per line
c
         entry vline ( idisid, buf, islds, issds, nlds, nsds )
c
c        set up start line and sample addresses
c
         isad = issds
         ilad = islds 
c
c        nsamps is the no. of samples to display / line
c        ensure nsamps is even so that xdilinewrite will work
c        correctly.
c
         if ( (isad-1) + nsds .gt. MAXSAMPS ) then
          nsamps = MAXSAMPS - isad - 1
         else
          nsamps = nsds
         end if
         nsamps = (( nsamps + 1 ) / 2 ) * 2
c
c        extract the display device no.
c
         ivd=1
c
c        write pixels 
c
         n    = nlds
         do j = 1, n  
          ierr  = xdilinewrite ( u, ivd, isad, ilad, nsamps, buf )
          ilad = ilad + 1
         end do
         return
c
c---------------------------------------------------------------
c
         entry rline ( idisid, buf, islds, issds, nlds, nsds )
c
c        set up start line and sample addresses
c
         isad = issds
         ilad = islds 
c
c        nsamps is the no. of samples to display / line
c        ensure nsamps is even so that xdilinewrite will work
c        correctly.
c
         if ( (isad-1) + nsds .gt. MAXSAMPS ) then
          nsamps = MAXSAMPS - isad - 1
         else
          nsamps = nsds
         end if
         nsamps = (( nsamps + 1 ) / 2 ) * 2
c
         ivd=1
c
c        read pixels 
c
         n    = nlds
         do j = 1, n  
          ierr  = xdilineread ( u, ivd, isad, ilad, nsamps, buf )
          ilad = ilad + 1
         end do
         return
c
c---------------------------------------------------------------
c
c        cset
c         this is the cursor set routine and may be used to
c         control the visibility characteristics of the 
c         cursor. it has the following posibilities:
c         a) cursor off
c         b) cursor on
c         the cursor on/off flag , iconoff, has the following values:
c         0 = off
c         1 = on
c         initially, the cursor is set to off.
c
c         in both these cases, the cursor can be blinking
c         or not blinking. obviously, if the cursor is off,
c         we can't see if it is blinking. 
c
c         where : itid - logical cursor identifier
c                 key  - cursor visibility characteristics
c                        flag
c                        visi = visible
c                        invi = invisible
c                        blin = blinking
c                        nbli = not blinking
c			 c1   = form 1 (five dots)
c			 c2   = form 2 
c	                 c3   = form 3
c
c
         entry cset ( itid, key )
c
         if ( ncurs .eq. 0 ) then
         return
         end if
c
c        see if the cursor is alive and visible
c
         if ( key .eq. ikey(1) ) then
          ierr = xdcon ( u, c, iform, iblink )
c
c         save cursor visibility flag
c
          iconoff = 1
          return
         end if
c
c        now invisible
c
         if ( key .eq. ikey(2) ) then
          ierr = xdcoff ( u, c )
c
c         save cursor visibility flag
c
          iconoff = 0
          return
         end if
c
c        set the blink on here if the cursor is visible
c
         if ( key .eq. ikey(3) ) then
c
          if ( iconoff .eq. 1 ) then
           iblink = 3 
           ierr   = xdcon ( u, c, iform, iblink )
           return
c
          else 
           iblink = 3
           return
          end if
         end if
c
c        set the blink off here if the cursor is visible
c
         if ( key .eq. ikey(4) ) then
c
          if ( iconoff .eq. 1 ) then
           iblink = 0 
           ierr   = xdcon ( u, c, iform, iblink )
           return
c
          else 
           iblink = 0
           return
          end if
         end if
c
c        set the cursor form to 1 for 'C1'
c
         if ( key .eq. ikey(5) ) then
c
          if ( iconoff .eq. 1 ) then
           iform = 1
           ierr   = xdcon ( u, c, iform, iblink )
           return
c
          else 
           iform = 1
           return
          end if
         end if
c
c        set the cursor form to 4 for 'C2'
c        set the blink off here if the cursor is visible
c
         if ( key .eq. ikey(6) ) then
c
          if ( iconoff .eq. 1 ) then
           iform = 4
           ierr   = xdcon ( u, c, iform, iblink )
           return
c
          else 
           iform = 4
           return
          end if
         end if
c
c        set the cursor form to 5 for 'C3'
c
         if ( key .eq. ikey(7) ) then
c
          if ( iconoff .eq. 1 ) then
           iform = 5
           ierr   = xdcon ( u, c, iform, iblink )
           return
c
          else 
           iform = 5
           return
          end if
         end if
        return
c---------------------------------------------------------------------
c
c        dset
c         dset appears to be a catch all routine that
c         allows the user to modify the characteristics
c         and display mode of the text as well as 
c         update the look up tables. as an added attraction,
c         we also get to connect up lookup tables to image
c         memory planes. this final, more interesting mode
c         is triggered by the fact that itest is none of the
c         more reasonable options. yuck...
c
c         calling sequence ( idisid, itest, larray )
c         where: idisid - logical device
c                itest  - character variation flag
c                         dw = double width
c                         dh = double height
c                         nw = normal width
c                         nh = normal height
c                         po = positive
c                         co = complementary
c                larray - look up table buffer
c
         entry dset ( idisid, itest, larray )
c
          ivd=1
          ierr = xdlwrite ( u, ivd, section, larray )
          return
        end
c********************************************************************
