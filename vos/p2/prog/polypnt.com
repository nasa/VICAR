$!****************************************************************************
$!
$! Build proc for MIPL module polypnt
$! VPACK Version 1.9, Monday, December 07, 2009, 16:55:50
$!
$! Execute by entering:		$ @polypnt
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
$ write sys$output "*** module polypnt ***"
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
$ write sys$output "Invalid argument given to polypnt.com file -- ", primary
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
$   if F$SEARCH("polypnt.imake") .nes. ""
$   then
$      vimake polypnt
$      purge polypnt.bld
$   else
$      if F$SEARCH("polypnt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake polypnt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @polypnt.bld "STD"
$   else
$      @polypnt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create polypnt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack polypnt.com -mixed -
	-s polypnt.f -
	-p polypnt.pdf -
	-i polypnt.imake -
	-t tstpolypnt.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create polypnt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C  IBIS ROUTINE POLYPNT
C
C  POLYPNT IS USED TO TRANSFORM A STANDARD POLYGON FILE INTO AN IMAGE
C  FILE OF POLYGON BORDERS.  THE INPUT DATA IN COORDINATE-POINT FORMAT
C  IS USED TO FORM A RASTER BASE CONTAINING POLYGON OUTLINES.  BOTH THE
C  DN VALUES FOR THE POLYGON BORDERS AND THE BACKGROUND AREAS MAY BE
C  SPECIFIED.  PARAMETERS FOR CHAINING AND CLOSURE MAY BE INVOKED.
C
C  USER PARAMETERS:
C
C  DN,N -    SPECIFIES THE DN VALUE WHICH WILL BE USED FOR SCRIBING POLY
C            BOUNDARIES.  THE DEFAULT IS 255.
C  EXCLOSE - INDICATES THAT THE BEGINNING AND ENDING NODES OF EACH POLYG
C            MUST MATCH EXACTLY.
C  BDN,N -   SPECIFIES THE DN VALUE WHICH WILL BE USED AS BACKGROUND.
C            THE DEFAULT IS ZERO.
C  OUTSIDE - SPECIFIES THAT THE BOUNDARY OF RIGHT HAND CODED POLYGONS AR
C            EXPANDED BY ONE PIXEL.
C  IGNOR,X,Y - CAUSES ALL OCCURENCES OF THE POINT (X,Y) TO BE TREATED AS
C            NULL POINTS.  A NULL POINT INVOKES AUTOMATIC POLYGON CLOSUR
C            THE DEFAULT IS (0.,0.).
C  PDN,P -   SPECIFIES A DN VALUE TO BE PLACED AT THE ENDS OF EACH LINE
C            SEGMENT OR THE CORNERS OF EACH POLYGON.
C  RANGE N,N... ALLOWS THE USER TO SELECT RANGES OF G2 USER CODES
C  SELECT  - ALLOWS THE USER TO SELECT INDIVIDUAL G2 USER CODES
C  HALF    - SPECIFIES HALFWORD OUTPUT
C  PAINT   - CAUSES EACH LINE TO BE ENCODED WITH A UNIQUE DN
C  INTE    - CAUSES EACH INTERSECTION TO BE ENCODED WITH A UNIQUE DN
C
      IMPLICIT INTEGER(A-Z)
      REAL ABS,FLOAT,AMIN1,AMAX1
      REAL FXL,FXU,FYL,FYU,LINES,X1,X2,Y1,Y2,X,Y,XD,YD,TST(4),T,T1,T2,
     .XINC,YINC,TL,TU,XQ,YQ,XFIRST,YFIRST,LEPS,SEPS,
     .PX,PY,OX2,OY2,SQ,XT,YT,PMJ,PMN,CYCLE,XIGNOR,YIGNOR
      LOGICAL*1 WORK,DN(4),BDN(4),PDN(4)
      LOGICAL*1 PB(4,8)
      LOGICAL*1 OUTNAM(150)
      INTEGER*2 WORK2(1),BDN2(1),LNARY
      INTEGER*2 PH(2,8)
      INTEGER*4 RANGAR(200),SELEAR(100)
      CHARACTER*4 G2
      REAL*4 IGNOAR(2)
      LOGICAL XVPTST
      LOGICAL INLINE,CONTIN,OUTSID,LPRNT,G2FLAG,EXCLOS,NFLAG,DEBUG
      LOGICAL PAINT,HALF,INTE,PASS1
      COMMON /COM/ALIGN4,WORK(92000)
      DATA WSIZ/92000/
      COMMON /COM1/LINES(2000),LNARY(2000)
      DIMENSION G2CB(138)
      DIMENSION LABEL(10),EARRAY(2),STABLE(2,50)
      DIMENSION IPX(8)
      EQUIVALENCE (DN(1),IDN),(BDN(1),IBDN),(PDN(1),IPDN)
      EQUIVALENCE (WORK,WORK2),(BDN,BDN2)
      EQUIVALENCE (EARRAY(1),X2),(EARRAY(2),Y2)
      EQUIVALENCE (IPX(1),PH(1,1),PB(1,1))
      DATA XIGNOR,YIGNOR/2*0./,IPDN/255/
      DATA IDN,IBDN/255,0/,DEBUG/.FALSE./,CONTIN/.FALSE./
      DATA OUTSID/.FALSE./,CLOSR/-1/
      DATA OX2,OY2,XFIRST,YFIRST/4*0./,SEPS,LEPS/.1,.1/,LPRNT/.TRUE./
      DATA G2/'  G2'/,NPXL/1/,NCOL/5/,LABC/2/
      DATA HALF,PAINT/2*.FALSE./,TABSZ/512/,BPIX/1/
      DATA PASS1/.TRUE./


C
C---- INITIALIZE, GET PARAMETERS, OPEN FILES.
C
      STABLE(1,1) = 5000
      STABLE(2,1) = 99999
      TSIZE = 1
      HALF = XVPTST('HALF')

      IF (HALF) then
        NPXL = 2
        BPIX = 2
        IPDN = 32767
        IDN = 32767
      end if

      PAINT = XVPTST('PAINT')
      IF(PAINT)  IDN = 1

      CALL XVPARM('DN',IDN,NDN,DNDF,' ')
      CALL XVPARM('BDN',IBDN,NBDN,BDNDF,' ')
      IPDN = IDN
      CALL XVPARM('PDN',IPDN,NPDN,PDNDF,' ')
      OUTSID = XVPTST('OUTSIDE')
      EXCLOS = XVPTST('EXCLOSE')
      IF(EXCLOS) CLOSR = 0
      CALL XVPARM('IGNORE',IGNOAR,NIGNO,IGNODF,' ')
      XIGNOR = IGNOAR(1)
      YIGNOR = IGNOAR(2)
      CALL XVPARM('RANGE',RANGAR,NRANG,RANGDF,' ')

      IF (RANGDF.EQ.1) GOTO 73

      IF (MOD(NRANG,2).EQ.1) GOTO 403

      TSIZE = NRANG/2

      DO 71 I = 1,TSIZE
        STABLE(1,I+TSIZE) = RANGAR(I*2-1)
71      STABLE(2,I+TSIZE) = RANGAR(I*2)

      CALL XVPARM('SELECT',SELEAR,NSELE,SELEDF,' ')

      DO 72 I = 1,NSELE
        STABLE(1,I+TSIZE) = SELEAR(I)
72      STABLE(2,I+TSIZE) = SELEAR(I)

      TSIZE = NSELE+TSIZE

73    INTE = XVPTST('INTE')

      IF (INTE) IPDN = IDN


C
C---- OPEN FILES.
C
      CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(RUNIT,STATUS,'OPEN_ACT','SA',' ')
      CALL XVSIZE(SLINE,SBYTE,NLINE,NBYTE,nli,nsi,' ')

      CALL XVPARM('OUT',OUTNAM,NOUT,OUTDF,' ')
      CALL XVUNIT(WUNIT1,'OUT',1,STATUS,' ')
      CALL XVOPEN(WUNIT1,STATUS,'OP','WRITE','U_NL',NLINE,'U_NS',NBYTE,
     *            'OPEN_ACT','SA',' ')
      CALL XVCLOSE(RUNIT,STATUS,' ')

      SSAMP = ((SBYTE-1)/BPIX)+1
      NSAMP = NBYTE/BPIX
      NLINEU = NLINE+SLINE-1
      NSAMPU = NSAMP+SSAMP-1
      NSAMP4 = NSAMP+4-MOD(NSAMP,4)
      NBYTE4 = NSAMP4*BPIX

C  NSAMP4 PROVIDES MARGIN FOR SCRIBING AT BORDERS,ALSO FOR WORD ALIGNMEN

      REC = 0
C
C---- SET UP ONE 'BAND' OR 'LAYER' OF IMAGE FOR SCRIBING THE LINE
C     DATA SET. AS MUCH AS CAN BE HELD BY WORK VECTOR.
C
      LU = SLINE
      LD = WSIZ/NBYTE4
      WSUP = LD*NSAMP4
      NSUP = LD*NSAMP4 - NSAMP4
      FYL = FLOAT(SSAMP)-.6
      FYU = FLOAT(NSAMPU)+.6

100   continue
      CALL XVOPEN(RUNIT,STATUS,'OPEN_ACT','SA',' ')

      LNPTR = 0
      LL = LU
      LU = MIN0(LU+LD-1,NLINEU)
      FXL = FLOAT(LL)-.6
      FXU = FLOAT(LU)+.6
      IF (.not. HALF) then
        DO 2 I=1,WSIZ
 2      WORK(I) = BDN(1)
        GOTO 3
      end if

      WEND = WSIZ/2
      DO 22 I = 1,WEND
22      WORK2(I) = BDN2(1)
C
C---- READ THE ENTIRE LINES DATA SET.  LINES THAT CROSS THE BAND
C     ARE PROCESSSED.
C
    3 CONTINUE
      CALL GRREAD(RUNIT,G2CB,LINES,UC,TY,NS2,G2FLAG,NFLAG,*300)
      IF (TY.NE.1) GOTO 31
        IF (NS2.GT.LABC*4) NS2=LABC*4
        LNS = NS2
        CALL MVE(1,40,LINES,LABEL(1))
31    CONTINUE
      IF (.NOT.G2FLAG) GOTO 33
      DO 32 I = 1,TSIZE
        IF ((UC.GE.STABLE(1,I)).AND.(UC.LE.STABLE(2,I))) GOTO 33
32    CONTINUE
      GOTO 3
33    ZEND1 = 0
      SEND2 = 0
      LNPTR = LNPTR+1
      IF ((LNARY(LNPTR).NE.0).AND.(PAINT)) IDN = LNARY(LNPTR)
      IF (LPRNT) CALL PRNT(7,10,LINES,' A FEW PTS.')
      LPRNT = .FALSE.
      NSU = NS2/4
      IF (.NOT.G2FLAG) GOTO 35
        IF (NSU.NE.2) GOTO 34
          LINES(NSU+1)=LINES(NSU-1)
          LINES(NSU+2)=LINES(NSU)
          NSU = NSU+2
34      IF (NSU.GE.2000) GOTO 35
          LINES(NSU+1) = 0
          LINES(NSU+2) = 0
          NSU = NSU+2
35    IF ((.NOT.PAINT).OR.(LNARY(LNPTR).NE.0)) GOTO 36
        IF (INTE) IPDN = IPDN + 1
36    DO 89 JX=2,NSU,2
      X1 = OX2
      Y1 = OY2
      X2 = LINES(JX-1)
      Y2 = LINES(JX)
      OX2 = X2
      OY2 = Y2
      IF (ABS(X1-XIGNOR)+ABS(Y1-YIGNOR).LE.1.E-6) GO TO 88
      IF (ABS(X2-XIGNOR)+ABS(Y2-YIGNOR).LE.1.E-6) GO TO 88
      IF (CLOSR) 15,8,7
 7    IF (ABS(X1-XFIRST).GT.LEPS) GO TO 9
      IF (ABS(Y1-YFIRST).GT.SEPS) GO TO 9
      GO TO 88
 8    IF (X1.EQ.XFIRST.AND.Y1.EQ.YFIRST) GO TO 88
 9    IF (CONTIN) GO TO 15
      CONTIN = .TRUE.
      XFIRST = X1
      YFIRST = Y1
 15   IF (.NOT.OUTSID) GO TO 16
      PX = Y1-Y2
      PY = X2-X1
      SQ = SQRT(PX*PX+PY*PY)
      IF (SQ.LE..01) GO TO 16
      PX = PX/SQ
      PY = PY/SQ
      X1 = X1+PX
      Y1 = Y1+PY
      X2 = X2+PX
      Y2 = Y2+PY
C
C---- QUICK TEST FOR INTERSECTION
C
 16   IF ((X1-FXL).LT.0..AND.(X2-FXL).LT.0.) GO TO 89
      IF ((X1-FXU).GT.0..AND.(X2-FXU).GT.0.) GO TO 89
C
C---- MORE COMPLETE TEST FOR INTERSECTION.  ALSO FIND INITIAL SQUARE.
C
      IF (ABS(X2).GT.1.E6.OR.ABS(Y2).GT.1.E6) then
          IF (.NOT.PASS1) GOTO 89
          CALL PRNT(7,2,EARRAY,' X,Y, BAD DATA.')
          CALL PRNT(4,1,LNPTR,' LINE.')
          GOTO 89
      end if

      IF (X1-X2) 19,17,18
 17   IF (Y1-Y2) 19,19,18
 18   X = X1
      X1 = X2
      X2 = X
      Y = Y1
      Y1 = Y2
      Y2 = Y
 19   XD = (X2-X1)+1.141593E-9
      YD = (Y2-Y1)+1.141593E-9
      TST(1) = (FXL+.6-X1)/XD
      TST(2) = (FYL+.6-Y1)/YD
      TST(3) = (FXU-.6-X1)/XD
      TST(4) = (FYU-.6-Y1)/YD
      TL = 3.
      TU = -3.
      INLINE = .FALSE.
      DO 4 I=1,4
      X = XD*TST(I)+X1
      Y = YD*TST(I)+Y1
      IF (X.LT.FXL.OR.X.GT.FXU) GO TO 4
      IF (Y.LT.FYL.OR.Y.GT.FYU) GO TO 4
      INLINE = INLINE.OR.(TST(I).GE.0..AND.TST(I).LE.1.)
      TL = AMIN1(TL,TST(I))
      TU = AMAX1(TU,TST(I))
 4    CONTINUE
      IF (TL.GT.2.) GO TO 89
      T = AMAX1(TL,0.)
      TU = AMIN1(TU,1.)
      IF (INLINE) GO TO 5
      IF (X1.GE.FXL.AND.X1.LE.FXU.AND.Y1.GE.FYL.AND.Y1.LE.FYU) GO TO 5
      IF (X2.GE.FXL.AND.X2.LE.FXU.AND.Y2.GE.FYL.AND.Y2.LE.FYU) GO TO 5
      GO TO 89
 5    X = XD*T+X1
      Y = YD*T+Y1
      ISC = MAX0(MIN0(INT(X+.5),LU),LL)-LL+1
      JSC = MAX0(MIN0(INT(Y+.5),NSAMPU),SSAMP)-SSAMP+1
      XT = XD*TU+X1
      YT = YD*TU+Y1
      IST = MAX0(MIN0(INT(XT+.5),LU),LL)-LL+1
      JST = MAX0(MIN0(INT(YT+.5),NSAMPU),SSAMP)-SSAMP+1
      NSQT = MAX0(IABS(IST-ISC),IABS(JST-JSC))
      INDEX = (ISC-1)*NSAMP4+JSC
      LINDEX = (IST-1)*NSAMP4+JST
      IF ((T.LT..001).AND.(.NOT.PAINT))
     &  CALL PUTPIX(WORK,WORK,INDEX,PDN,PDN,HALF,PIX)
      IF ((NSQT.EQ.0).AND.(.NOT.PAINT)) GO TO 89
      XQ = FLOAT(ISC+LL-1)
      YQ = FLOAT(JSC+SSAMP-1)
C
C---- FINE ADJUSTMENT OF TRAJECTORY OF LINE.
C
      XINC = ABS(1./XD)
      YINC = ABS(1./YD)
      XBIT = NSAMP4
      YBIT = 1
      IF (XD.LT.0) XBIT = -XBIT
      IF (YD.LT.0) YBIT = -YBIT
      IF (XINC-YINC) 27,27,28
 27   YBIT = YBIT+XBIT
      YINC = YINC-XINC
      CYCLE = (XINC+YINC)/XINC
      PMN = Y-YQ
      IF (YD.LT.0.) PMN = -PMN
      PMJ = X-XQ
      IF (XD.LT.0.) PMJ = -PMJ
      T1 = XINC*.5
      T2 = YINC*(.5-PMN+PMJ/CYCLE)
      GO TO 29
 28   XBIT = XBIT+YBIT
      XINC = XINC-YINC
      CYCLE = (XINC+YINC)/YINC
      PMN = X-XQ
      IF (XD.LT.0.) PMN = -PMN
      PMJ = Y-YQ
      IF (YD.LT.0.) PMJ = -PMJ
      T1 = XINC*(.5-PMN+PMJ/CYCLE)
      T2 = YINC*.5
 29   CONTINUE
C
C---- THIS LOOP SCRIBES THE LINE
C
      SEND1 = ZEND1
      ZEND1 = INDEX
      I = 0
 50   IF ((INDEX.EQ.SEND1).OR.(INDEX.EQ.SEND2)) GOTO 54
      CALL PUTPIX(WORK,WORK,INDEX,DN,DN,HALF,PIX)
      IF (PIX.NE.IBDN) GOTO 53
      DO 80 K = 1,8
80    IPX(K) = 0
      IF (HALF) GOTO 83
        IF (INDEX.LE.NSAMP4) GOTO 81
        PB(4,2) = WORK(INDEX-NSAMP4-1)
        PB(4,3) = WORK(INDEX-NSAMP4)
        PB(4,4) = WORK(INDEX-NSAMP4+1)
81      PB(4,1) = WORK(INDEX-1)
        PB(4,5) = WORK(INDEX+1)
        IF (INDEX.GT.NSUP) GOTO 85
        PB(4,8) = WORK(INDEX+NSAMP4-1)
        PB(4,7) = WORK(INDEX+NSAMP4)
        PB(4,6) = WORK(INDEX+NSAMP4+1)
        GOTO 85
83      IF (INDEX.LE.NSAMP4) GOTO 84
        PH(2,2) = WORK2(INDEX-NSAMP4-1)
        PH(2,3) = WORK2(INDEX-NSAMP4)
        PH(2,4) = WORK2(INDEX-NSAMP4+1)
84      PH(2,1) = WORK2(INDEX-1)
        PH(2,5) = WORK2(INDEX+1)
        IF (INDEX.GT.NSUP) GOTO 85
        PH(2,8) = WORK2(INDEX+NSAMP4-1)
        PH(2,7) = WORK2(INDEX+NSAMP4)
        PH(2,6) = WORK2(INDEX+NSAMP4+1)
85    IF (IDN.EQ.IPX(2).AND.IPX(1).EQ.IPX(3)) PIX = IPX(1)
      IF (PIX.NE.IBDN) GOTO 53
      IF (IDN.EQ.IPX(4).AND.IPX(3).EQ.IPX(5)) PIX = IPX(3)
      IF (PIX.NE.IBDN) GOTO 53
      IF (IDN.EQ.IPX(6).AND.IPX(5).EQ.IPX(7)) PIX = IPX(5)
      IF (PIX.NE.IBDN) GOTO 53
      IF (IDN.EQ.IPX(8).AND.IPX(7).EQ.IPX(1)) PIX = IPX(7)
53    IF ((PIX.EQ.IBDN).OR.(PIX.EQ.IDN)) GOTO 54
      IF (INTE.AND.PIX.EQ.SPIX) IPDN = IPDN - 1
      CALL PUTPIX(WORK,WORK2,INDEX,PDN,PDN2,HALF,JUNK)
      IF (INTE) IPDN = IPDN + 1
54    SPIX = PIX
      I = I + 1
      IF (I.GT.NSQT) GOTO 57
      IF (T2-T1) 55,55,56
55    INDEX = INDEX+YBIT
      IF (INDEX.GT.WSUP) GOTO 57
      T2 = T2+YINC
      GO TO 50
 56   INDEX = INDEX+XBIT
      IF (INDEX.GT.WSUP) GOTO 57
      T1 = T1+XINC
      GOTO 50
 57   SEND2 = INDEX
      IF (PAINT) GOTO 89
      ISZ = INDEX/NSAMP4+1
      JSZ = INDEX-(ISZ-1)*NSAMP4
      ISTZ = (IST+ISZ)/2
      JSTZ = (JST+JSZ)/2
      ZINDEX = (ISTZ-1)*NSAMP4+JSTZ
      CALL PUTPIX(WORK,WORK,ZINDEX,DN,DN,HALF,PIX)
      IF (TU.GT..999) GOTO 67
        CALL PUTPIX(WORK,WORK,INDEX,DN,DN,HALF,PIX)
        GOTO 68
67    CALL PUTPIX(WORK,WORK,INDEX,PDN,PDN,HALF,PIX)
68    GO TO 89
 88   CONTIN = .FALSE.
      XFIRST = 0.
      IF (PAINT) IDN = IDN + 1
      IF (INTE) IDN = IPDN
 89   CONTINUE
      GOTO 3
C
C---- WRITE OUT THE SCRIBED BAND.
C
300   PTR = 1
      IF (LL.EQ.SLINE) GOTO 305
      LL = LL + 1
      PTR = PTR + NBYTE4
305   IF (LU.LT.NLINEU) LU = LU - 1
      DO I=LL,LU
          REC = REC+1
          CALL XVWRIT(WUNIT1,WORK(PTR),STATUS,
     -                'LINE',REC,'NSAMPS',NBYTE,' ')
          PTR = PTR+NBYTE4
      end do
      CALL XVCLOSE(RUNIT,STATUS,' ')
      PASS1 = .FALSE.
      IF (LU.LT.NLINEU) GO TO 100
C
400   continue
      CALL xvmessage(' POLYPNT NORMAL TERMINATION',0)
      RETURN
403   CALL xvmessage(' PARAM ERR',0)
      CALL ABEND
      RETURN
      END

C********************************************************************

      SUBROUTINE PUTPIX(WORK,WORK2,INDEX,DN,DN2,HALF,OPIX)
      IMPLICIT INTEGER(A-Z)
      LOGICAL*1 WORK,DN,PIX
      INTEGER*2 WORK2,DN2,PIX2,PDN2
      DIMENSION WORK(1),DN(1),WORK2(1),DN2(1),PIX(4),PIX2(2),PDN2(2)
      EQUIVALENCE (IPIX,PIX),(IPIX,PIX2),(PDN,PDN2)
      LOGICAL HALF

      IF ( .not. HALF) then
        PIX(1) = WORK(INDEX)
        WORK(INDEX) = DN(1)
      else
        PIX2(1) = WORK2(INDEX)
        WORK2(INDEX) = DN2(1)
      end if

      OPIX = IPIX
      RETURN
      END

C**************************************************************
      SUBROUTINE GRREAD(RUNIT,G2CB,BUF,UC,TY,NS,G2FLAG,NFLAG,*)
C
C     READ GRAPHICS 1 OR 2 LINE
C
      IMPLICIT INTEGER(A-Z)
      LOGICAL G2FLAG,NFLAG
      DIMENSION G2CB(138),BUF(1)
C
      NS = 512
      TY = 7
      CALL XVREAD(RUNIT,BUF,STATUS,'NSAMPS',NS,' ')
      IF (STATUS.ne.1) return 1

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create polypnt.pdf
PROCESS     HELP=*
! POLYPNT - VICAR/IBIS SOFTWARE
! VICAR2/MIPL VERSION
PARM INP TYPE=(STRING,72)
PARM OUT TYPE=(STRING,72) COUNT=(1:2)
PARM SIZE TYPE=INTEGER COUNT=4
PARM DN  TYPE=INTEGER DEFAULT=255
PARM BDN TYPE=INTEGER DEFAULT=0
PARM PDN TYPE=INTEGER DEFAULT=1
PARM IGNORE  TYPE=REAL COUNT=(0:2) DEFAULT=0.
PARM RANGE TYPE=INTEGER COUNT=(0:200) DEFAULT=0
PARM SELECT TYPE=INTEGER COUNT=(0:100) DEFAULT=0
PARM MODE TYPE=KEYWORD COUNT=(0:6) +
VALID=(EXCLOSE,OUTSIDE,HALF,PAINT,INTE,GR2) DEFAULT=--
END-PROC
.TITLE
VICAR/IBIS Program POLYPNT 
.HELP
PURPOSE

     POLYPNT is used to transform a standard polygon file into an 
     image  file of polygon borders with each border painted at a 
     unique DN level.   The input data in coordinate-point format 
     is used to form a raster image containing polygon  outlines.  
.PAGE
TAE COMMAND LINE FORMAT
     POLYPNT INP=G OUT=PIC SIZE PARAMS
     where
     G                        is  the IBIS Graphics polygon data to 
                              be scribed,
     PIC                      is  the output raster dataset  onto 
                              which  the  scribed image  will  be 
                              written,
     SIZE                     is  the standard VICAR size  field.  
                              The  size  field provides a  window 
                              through which the lines are scribed 
                              and is required by the program, and
     PARAMS                   is   a  standard  VICAR   parameter 
                              field.
.PAGE
OPERATION
     POLYPNT  scribes lines in a holding array which is a  subset 
     of the window described in the SIZE parameter.  It processes 
     one  line at a time calculating slope,  beginning and ending 
     points  to determine which pixels will be filled to  connect 
     the  points.   It looks for intersections (previously  coded 
     pixels)  and  outputs notations to  the  optionally  tabular 
     file.   The completed strip is written to the output dataset 
     before a new strip is started.
EXAMPLE
        POLYPNT INP=G OUT=PIC SIZE=(1,1,512,512) +
                'PAINT 'INTE
     In this example, POLYPNT reads the dataset, IN and writes 
     the scribed image onto the datset PIC. The output image is 
     512 x 512  and contains lines and intersections with sequential 
     DN values starting with DN 1.  The background is DN 0.
.PAGE

WRITTEN BY:            A. L. Zobrist, 27 April 1983

COGNIZANT PROGRAMMER:  B. A, Mc Guffie

REVISION:              New
.LEVEL1
.VARIABLE INP
Input Graphics file to scribe
.VARIABLE OUT
PIC - output image file
.VARIABLE SIZE
Standard size field (sl,ss,nl,ns)
.VARIABLE DN
DN value for the first line
.VARIABLE BDN
DN value for background
.VARIABLE PDN
DN for the first intersection
.VARIABLE RANGE
Range of G2 user codes to paint
.VARIABLE SELECT
G2 user codes to paint
.VARIABLE MODE
HALF - halfword output
PAINT- each line with unique DN
INTE - each intersection new DN
GR2  - Graphics II input
.LEVEL2
.VARIABLE INP
    INP=G                      is  the Graphics I or  II  polygon 
                               data to be scribed,
.VARIABLE OUT
    OUT=
     PIC                      is  the output raster dataset  onto 
                              which  the  scribed image  will  be 
                              written,
.VARIABLE DN
     DN,n                     the  integer  n  specifies  the  DN 
                              value  to  be  used  on  the  first 
                              scribed line.  The default is 1.
.VARIABLE BDN
     BDN,n                    the  interger  n specifies  the  DN 
                              value  of  the   background.    The 
                              default is zero.
.VARIABLE PDN
     PDN,n                    the  integer  n  specifies  the  DN 
                              value  to  be  used  on  the  first 
                              intersection.

.VARIABLE RANGE
     RANGE,l,h...             specifies Graphics user codes to 
                              paint.   Each  pair has a  low  and 
                              high integer.

.VARIABLE SELECT
     SELECT,n...              specifies Graphics user codes to 
                              paint.

.VARIABLE MODE
     HALF                     specifies halfword output.

     PAINT                    causes  each new line to be encoded 
                              with a unique DN.

     INTE                     cause  each new intersection to  be 
                              encoded with a unique DN.

     GR2                      specifies that the input and output 
                              datasets are in Graphics II format.  
                              This parameter is not necessary  if 
                              the  input  dataset is  labeled  as 
                              being Graphics II.

$ Return
$!#############################################################################
$Imake_File:
$ create polypnt.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM polypnt

   To Create the build file give the command:

		$ vimake polypnt			(VMS)
   or
		% vimake polypnt			(Unix)


************************************************************************/


#define PROGRAM	polypnt
#define R2LIB

#define MODULE_LIST polypnt.f 

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/* #define DEBUG */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstpolypnt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
POLYGEN OUT=a.dat MDATA=(10. 10. 40. 40. 10 10. 40. 40. 10. 10)
ibis-list a.dat gr1dim=2 nr=30
POLYPNT INP=a.dat OUT=pic.dat SIZE=(1 1 60 60) 'PAINT DN=100
LIST INP=pic.dat SIZE=(10,10,30,17)
end-proc
$ Return
$!#############################################################################
