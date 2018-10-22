CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to enable the user to interactively specify an area of
C the picture by selecting the (top,left) and (bottom,right) corners
C via the cursor.
C
C Updated arguments: ISL,ISS,INL,INS
C
      SUBROUTINE CAREA(ISL,ISS,INL,INS)
      IMPLICIT REAL*8 (A-H,O-Z)
      include 'fortport'        ! defines int2byte
      COMMON/DEV/IDEV,VID,G,TB,NLDS,NSDS,ZOOM,IZOOM,STBL(256)
      INTEGER*4 VID,G,TB,STBL
      REAL*4 ZOOM

      common/navdv2/xdw,xdb
      integer xdw,xdb
      byte bmdn,bidn

      COMMON/IPIC/IMG,SL,SS,NLI,NSI,ICODE
      INTEGER*4 SL,SS

      INTEGER X(2),Y(2),SAMP,LINE
      LOGICAL XST,XDCLOCATION,XDIPOLYLINE,XVIPTST

      bmdn = int2byte(xdw)
      bidn = int2byte(xdb)

      IEL = ISL + INL - 1
      IES = ISS + INS - 1

      CALL XVMESSAGE('Move Cursor to (top,left) corner of area',' ')
      CALL XVINTRACT('SKIP',
     &  ' Hit Return when ready or type ''S to skip')
      IF (XVIPTST('S')) GOTO 30

   10 XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
      X(1) = SAMP
      Y(1) = LINE
      X(2) = NSDS
      Y(2) = LINE
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      X(2) = SAMP
      Y(2) = NLDS
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      CALL XVINTRACT('CONTINUE',
     &      ' Hit Return to redo or type ''C to continue')
      IF (.NOT.XVIPTST('C')) THEN
           X(1) = SAMP				! Erase old stuff
           Y(1) = LINE
           X(2) = NSDS
           Y(2) = LINE
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           X(2) = SAMP
           Y(2) = NLDS
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           GOTO 10				! go back and redo
      ENDIF

      ISL = SL + LINE/ZOOM
      ISS = SS + SAMP/ZOOM
C
C              
   30 CALL XVMESSAGE('Move Cursor to (bottom,right) corner of area',' ')
      CALL XVINTRACT('SKIP',
     &  ' Hit Return when ready or type ''S to skip')
      IF (XVIPTST('S')) GOTO 50

   40 XST = XDCLOCATION(IDEV,TB,SAMP,LINE)
      X(1) = 1
      Y(1) = LINE
      X(2) = SAMP
      Y(2) = LINE
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      X(1) = SAMP
      Y(1) = 1
      XST = XDIPOLYLINE(IDEV,G,bmdn,2,X,Y)
      CALL XVINTRACT('CONTINUE',
     &       ' Hit Return to redo or type ''C to continue')
      IF (.NOT.XVIPTST('C')) THEN
           X(1) = 1
           Y(1) = LINE
           X(2) = SAMP
           Y(2) = LINE
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           X(1) = SAMP
           Y(1) = 1
           XST = XDIPOLYLINE(IDEV,G,bidn,2,X,Y)
           GOTO 40				! go back and redo
      ENDIF

      IEL = SL + LINE/ZOOM
      IES = SS + SAMP/ZOOM

   50 INL = IEL - ISL + 1
      INS = IES - ISS + 1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to permit the user to delete bad points via the cursor
C All points within radius R of cursor are flagged as (-99.,-99.)
C Updated arguments: PTS
C
      SUBROUTINE CLEANPTS(PTS,NPTS,R)
      REAL*4 PTS(2,NPTS),r
      LOGICAL XVIPTST
      real*8 rl8,rs8
C
      R2 = R**2
C
  50  CALL XVINTRACT('READY',
     &  ' Hit Return to delete point or type ''EXIT if done')
      IF (XVIPTST('EXIT')) RETURN
      CALL CURSOR(RL8,RS8)		!Read cursor position
      rline = rl8
      rsamp = rs8
C
      DO 20 I=1,NPTS
      RL = PTS(1,I)
      DY = (RL-RLINE)**2
      IF (DY.GT.R2) GOTO 20
      RS = PTS(2,I)
      DX = (RS-RSAMP)**2
      IF (DX+DY.GT.R2) GOTO 20
      PTS(1,I) = -99.0
      PTS(2,I) = -99.0
      CALL DRAWDOT(RL,RS,0)
   20 CONTINUE
C
      GOTO 50
C
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Automatic cleaning routine searches for isolated bad points on the acquired
C curve.  A point is bad if it is more than R pixels away from its two nearest 
C neighbors.  All bad points  are flagged as (-99.,-99.)
C The computed curve CPTS is required to check for continuity.
C
C Updated arguments: APTS
C
      SUBROUTINE CLEANPT1(CPTS,APTS,NPTS,R)
      REAL*4 CPTS(2,NPTS),APTS(2,NPTS),r

      R2 = R**2
      CL2 = CPTS(1,1)
      CS2 = CPTS(2,1)
      CL3 = CPTS(1,2)
      CS3 = CPTS(2,2)
      AL2 = APTS(1,1)
      AS2 = APTS(2,1)
      AL3 = APTS(1,2)
      AS3 = APTS(2,2)

      DO 20 I=3,NPTS
      CL1 = CL2
      CS1 = CS2
      CL2 = CL3
      CS2 = CS3
      CL3 = CPTS(1,I)
      CS3 = CPTS(2,I)
      AL1 = AL2
      AS1 = AS2
      AL2 = AL3
      AS2 = AS3
      AL3 = APTS(1,I)
      AS3 = APTS(2,I)
      IF (AL2.LT.0.D0) GOTO 20
      IF ( (CL3-CL1)**2+(CS3-CS1)**2 .GT. 100.0 ) GOTO 20
      IF ( (AL2-AL1)**2+(AS2-AS1)**2 .LT. R2 ) GOTO 20
      IF ( (AL3-AL2)**2+(AS3-AS2)**2 .LT. R2 ) GOTO 20
      APTS(1,I-1) = -99.0
      APTS(2,I-1) = -99.0
   20 CONTINUE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Routine to flag all (line,sample) coordinates that are within
C radius R of a Voyager reseau.  All points outside picture margin
C are also flagged.  Coordinates are flagged as (-99.0,-99.0)
C Note that for image-space frames, the algorithm fails to flag
C points near marks at the corners.  This is because the algorithm
C assumes that distortions are small.  The reseau marks affected
C are: S/N 4 (2,3,13,14,24,200), S/N 5 (2,3,14,35,137,152,167,199,200),
C S/N 6 (24,155,178), S/N 7 (10,11,24,191).
C
C  Updated argument: PTS
C
      SUBROUTINE CLEANVGR(pts,NPTS,RES,R)
      REAL*4 PTS(2,NPTS),RES(2,202),r
      INTEGER*2 ROW_TYPE(24)/1,2,1,2,3,2,4,2,4,2,4,2,4,2,
     &    4,2,4,2,4,2,1,2,1,1/
      INTEGER*2 IROW(24)/1,13,24,36,47,51,62,66,77,81,92,
     &    96,107,111,122,126,137,141,152,156,167,179,190,190/
      INTEGER*2 ICOL(12,4)/0,1,2,3,4,5,6,7,8,9,10,11,
     &                     0,1,2,3,4,5,6,7,8,9,10,10,
     &                     0,1,1,1,1,155,155,155,155,2,2,3,
     &                     0,1,1,1,1,1,2,2,2,2,2,3/
      REAL*4 OFFSETS(4),LOFFSET,LEFTSAMP
      INTEGER*4 RTYPE
C
      R2 = R**2			!Square of pixel radius
      IF (RES(1,195).LT.900.0) THEN  !If image-space, then
         RNL = 800.0		!number of lines=800
         RNS = 800.0		!number of samps=800
      ELSE			!else, object-space.
         RNL = 1000.0
         RNS = 1000.0
      ENDIF

      DL = (RES(1,172)-RES(1,29))/18	!Pixel spacing between rows
      DS = (RES(2,105)-RES(2,97))/8	!Pixel spacing between columns
      TL1 = 0.5*(RES(1,6)+RES(1,18))	!Boundary btwn first two rows
      TL2 = 0.5*(RES(1,18)+RES(1,29))	!Boundary btwn second and third rows
      BL2 = 0.5*(RES(1,173)+RES(1,184))	!Boundary btwn rows 21 and 22
      BL1 = 0.5*(RES(1,184)+RES(1,196))	!Boundary btwn last two rows
      LOFFSET = 3.0*DL - 0.5*(RES(1,29)+RES(1,41))	!Line offset
      OFFSETS(1) = 6.0*DS - 0.5*(RES(2,29)+RES(2,30))	!Samp offset row type 1
      OFFSETS(2) = 5.0*DS - 0.5*(RES(2,100)+RES(2,101))	!Samp offset row type 2
      OFFSETS(3) = 6.0*DS - 0.5*(RES(2,48)+RES(2,49))	!Samp offset row type 3
      OFFSETS(4) = 6.0*DS - 0.5*(RES(2,93)+RES(2,94))	!Samp offset row type 4
C     ....Boundary btwn first two marks on rows 2 and 22
      LEFTSAMP = 0.5*(RES(2,13)+RES(2,14))
C     ....Boundary btwn last two marks on rows 2 and 22
      RIGTSAMP = 0.5*(RES(2,22)+RES(2,23))
C
      DO 100 I=1,NPTS
      RLINE = PTS(1,I)
      RSAMP = PTS(2,I)
      IF (RSAMP.LT.1.0 .OR. RSAMP.GT.RNS) GOTO 90
      IL = (RLINE+LOFFSET)/DL

      IF (RLINE.LT.TL2) THEN
         IF (RLINE.LT.1.0) GOTO 90
         IF (RSAMP.LT.LEFTSAMP) THEN
            D = (RES(1,1)-RLINE)**2 + (RES(2,1)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
            D = (RES(1,13)-RLINE)**2 + (RES(2,13)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
         ELSE
            IF (RSAMP.GT.RIGTSAMP) THEN
               D = (RES(1,12)-RLINE)**2 + (RES(2,12)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
               D = (RES(1,23)-RLINE)**2 + (RES(2,23)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
            ENDIF
         ENDIF
         IF (RLINE.LT.TL1) THEN
            IL = 0
         ELSE
            IL = 1
         ENDIF
      ENDIF
C
      IF (RLINE.GT.BL2) THEN
         IF (RLINE.GT.RNL) GOTO 90
         IF (RSAMP.LT.LEFTSAMP) THEN
            D = (RES(1,190)-RLINE)**2 + (RES(2,190)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
            D = (RES(1,179)-RLINE)**2 + (RES(2,179)-RSAMP)**2
            IF (D.LT.R2) GOTO 90
         ELSE
            IF (RSAMP.GT.RIGTSAMP) THEN
               D = (RES(1,201)-RLINE)**2 + (RES(2,201)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
               D = (RES(1,189)-RLINE)**2 + (RES(2,189)-RSAMP)**2
               IF (D.LT.R2) GOTO 90
            ENDIF
         ENDIF
         IF (RLINE.GT.BL1) THEN
            IL = 22
         ELSE
            IL = 21
         ENDIF
      ENDIF
C
      RTYPE = ROW_TYPE(IL+1)
      OFFSET = OFFSETS(RTYPE)
      IS = (RSAMP+OFFSET)/DS
      IRES = IROW(IL+1) + ICOL(IS+1,RTYPE)
      D = (RES(1,IRES)-RLINE)**2 + (RES(2,IRES)-RSAMP)**2
      IF (D.GT.R2) GOTO 100
   90 PTS(1,I) = -99.0
      PTS(2,I) = -99.0
  100 CONTINUE
C
      RETURN
      END
