$!****************************************************************************
$!
$! Build proc for MIPL module picreg
$! VPACK Version 1.9, Tuesday, January 15, 2013, 17:25:11
$!
$! Execute by entering:		$ @picreg
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
$ write sys$output "*** module picreg ***"
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
$ write sys$output "Invalid argument given to picreg.com file -- ", primary
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
$   if F$SEARCH("picreg.imake") .nes. ""
$   then
$      vimake picreg
$      purge picreg.bld
$   else
$      if F$SEARCH("picreg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake picreg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @picreg.bld "STD"
$   else
$      @picreg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create picreg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack picreg.com -mixed -
	-s picreg.f -
	-i picreg.imake -
	-p picreg.pdf -
	-t tstpicreg.pdf tstpicreg.scr
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create picreg.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C  PROGRAM PICREG:  INTERACTIVE IMAGE REGISTRATION PROGRAM.

c  10 Jan 2013  -lwk-  fixed CHARACTER continuation lines for new compiler flag on Solaris
C  15 APR 96   ...SP....  Added missing argument to some PRNT calls.  Changed
C                         QPRINT calls to XVMESSAGE.  Changed calls to XXPARM
C                         for keyword parameters to use a character variable.
C                         Deleted call to XVEACTION since it may cause trouble
C                         with IBIS when IBIS upgrade occurs.  Replace calls to
C                         MVE with in-line code for cases where destination and
C                         source overlap.
C  29 NOV 93   ...SP....  Made portable for UNIX.  Changed code to use 
C                         BYTE2INT AND INT2BYTE for converting 
C                         between BYTE and INTEGER.  Added XVEACTION and 
C                         IFMESSAGE calls and removed CHKSTAT calls.  Added
C                         XDSVNL etc. calls instead of some VRDI XDDINFO stuff.
C                         Replaced XDGLCONSTANT call with XDGLINIT and XDGCOLOR
C                         to allow PICREG to run on the IVAS.  Left-justified
C                         tiepoint numbering to be less confusing and increased
C                         text size to be more legible on non-DeAnza devices.
C                         Added TEXTSIZE parameter to adjust the size of
C                         tiepoint numbering.
C                         Replaced VMS system service calls with
C                         VICAR routine WAITFORINPUT in the zero trackball
C                         code.  Improved Help file.  Changed VRDI calls to use
C                         INT2BYTE(GDN) where a BYTE is required.  Added call
C                         to XDDCLOSE at end.   Replaced some 0.0 with 0.0D0
C                         in LSQP.
C  14 OCT 92   ...SP....  CORRECTED ACCURACY CHECK FOR SUBPIXEL INTERPOLATION
C                         WHEN INTERP SPECIFIED.  ADDED USER INFO IN HELP.
C  10 OCT 89   ...SP....  ADDED AN EXTRA DIGIT IN TIEPOINT LOCATION PRINTOUT
C                         AND IMAGE SIZE PRINTOUT TO HANDLE BIGGER IMAGES.
C   2 JUN 88   ...SP....  COMMENTED OUT LIN1 AND LIN2 PARAMETERS BECAUSE
C                         THIS DEPENDED ON A DEANZA FEATURE THAT WAS NEVER
C                         IMPLEMENTED IN THE VRDI.
C   8 SEP 87   ...SP....  CORRECTED NHOR VALUE FOLLOWING CALL TO TYTRI IN
C                         SUBROUTINE FORMT.
C  13 MAY 87   ...DFS...  Link shareable; removed subr PARUNIT
C  13 JUL 86   ...SP....  CHANGED XCOR2 TO SUBTRACT CENTROID LOCATION 
C                         INSTEAD OF ADD AND TO NOT TO MIX UP LINE WITH
C                         SAMPLE.  CORRESPONDING CHANGES IN DISPX.
C  11 JUL 86   ...SP....  CORRECTED ROUTINE HISTCN TO PREVENT NEGATIVE 
C                         TIEPOINT COORDINATES AND DIVIDE BY 0.
C  10 JUL 86   ...SP....  CORRECTED INTEGER OVERFLOW IN RECO67 BY USING
C                         FLOAT FUNCTION.
C  10 JUL 86   ...SP....  CORRECTED UNNECESSARY DELAY ASSOCIATED WITH MESSAGE
C                         'DISPLAY NOT ALL IN IMP'.  REMOVED ALL XVOPENs
C                         OF IMAGE FILES WITH U_FORMAT=HALF, AND MADE IN ARRAY
C                         BYTE.
C  10 JUL 86   ...SP....  EXCLUDED NEGATIVE VALUES IN CENTROID COMPUTATION.
C  19 MAR 86   ...LWK...  ADDED PHASE CORRELATION AND POSITION PARAMETER.
C  12 MAR 86   ...SP....  IN MY BRIEF STINT WITH THIS PROGRAM I NOTICED SOME
C                         UNFRIENDLY LOOKING IMPLICIT STATEMENTS.  BEWARE.
C  12 MAR 86   ...SP....  CORRECTED ROUTINE FORMT FOR OUTPUT TO TIECONM.
C  07 MAR 86   ...SP....  CONVERTED BACK TO USING TAE INTERACTIVE PARAMETER
C                         PROCESSING BY DELETING PARAM AND IPARAM2 AND BY
C                         USING XVINTRACT AND XVIPARM. REWROTE SUBROUTINE
C                         KEYWRD.
C  05 AUG 85   ...LWK...  INCLUDED "C" SUBROUTINE PARUNIT IN-LINE
C  29 JUL 85   ...LWK...  MAKE I/O VICAR2;  BUG FIXES.
C  30 NOV 84   ...CCA...   ADD 2000 TP CAPABILITY
C  01 OCT 84   ...CCA...   ADD XVP PARAM ROUTINES
C  15 FEB 84   ...CCA...   CONVERT TO VAX
C  02 MAY 83   ...JHR...   BUGS
C  10 MAR  83   ...JHR...   ALLOW USE OF DEANZA,CODE MODS
C  14 JULY 80   ...CCA...   MOD FOR CONVERSION TO 370/MVS
C  11 JUNE 80   ...CCA...   KEYWORDS 'FIT1-FIT7','HOME'
C  11 JUNE 80   ...CCA...   KEYWORDS 'PRINT','NOPRINT'
C  11 JUNE 80   ...CCA...   ABLE TO OUTPUT MGEOM DATASET
C  21 JAN 80   ...JJL...   OPEN DATASETS FOR BSIZE
C  10 APR 79   ...JJL...   MOD TO RUN UNDER LIBON PROC ONLY
C  28 APR 78   ...JJL...   INITIAL RELEASE
C
      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
      external   xviparm, xvparm

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

      COMMON/C7/ IN
      COMMON/C6/ LCEN,ICEN,IPOW,NUM,ISIGN,IPRINT,IDEL,NDEL
      COMMON/CX/ IPARM,PTS,IPT
      COMMON/CD/ IDEV,LEFT,TOP,RIGHT,BOTTOM,SCREEN,MAX,SLI,SSI,KDUM(4),
     .           GDN
      INTEGER GDN
      DOUBLE PRECISION    Z1(10,10),Z2(10,10),Z3(10,10)
      REAL    RPARM(90),RAR(1800),PTS(4,2000),PT(8000)
      REAL    R1(64,66),R2(64,128),INREAL(25000),OLDZ(2)
      REAL    COEF(20),CX(2000,10),CL(2000),V(2000),EX(10)
      INTEGER HISTH(1000),HISTV(1000),SCREEN,OUT1
      INTEGER IPARM(90),PAR(8192),SLI(2),SSI(2)
      INTEGER DISP1(64,64),DISP2(64,64),LUT(256)
      BYTE      IN(100000)
      INTEGER LUTV1,LUTL
      INTEGER V1,VL,VR,G1,T1,T2,LUTR
      INTEGER IDEV,LEFT(2),TOP(2),RIGHT(2),BOTTOM(2)
      BYTE WHITE,LUT1(256),LUT2(256)
      LOGICAL XST, XDDACTIVATE, XDDCLOSE
      CHARACTER*132 MSG15
      CHARACTER*132 LAB
      CHARACTER*132 MSG24
      CHARACTER*132 MSG13
      CHARACTER*132 MSG11
      CHARACTER*132 MSG14
      CHARACTER*256 OUTFILE(2)
      CHARACTER*8 FMT
      EQUIVALENCE (IPARM,RPARM),(PTS,PT)
C..the following EQUIVS are just to save space by using it in multiple ways.
      EQUIVALENCE (DISP1,R1,HISTH),(DISP2,R2,HISTV,PAR,RAR)
      EQUIVALENCE (INREAL,IN),(INREAL(1),CX),(INREAL(20001),CL)
      EQUIVALENCE (INREAL(22001),V),(INREAL(24001),EX)
      EQUIVALENCE (INREAL(24031),Z1),(INREAL(24011),COEF)
      EQUIVALENCE (INREAL(24231),Z2),(INREAL(24431),Z3)

      DATA LUTV1/1/,LUTL/2/
      DATA V1/1/,VL/2/,VR/3/,G1/4/,T1/1/,T2/2/,LUTR/3/
      DATA WHITE/-1/ ! -1 IS HEX ZZ= 255 DN
C==================================================================
      CALL IFMESSAGE('PICREG version 10-Jan-2013')
C
C        ***** SETUP *****
C
	CALL XVPARM('OUT',OUTFILE,ICNT,IDEF,0)
C        INITIALIZE AND SET DEFAULTS
      CFORM = 1
      CONV=-1
      NLW=1
      IPOW=6
      NUM=64
      MODE=1
      IHPF=1
      IPHAS = 0
      SS1=1
      SL1=1
      SS2=1
      SL2=1
      NHOR=20
      NVER=20
      NPTS=0
      INTER=0
      IZOOM1=1
      IZOOM2=1
      IPRINT = 0
      IFIT=0
      ISTRE=0
      IEXIT=0
      IFORM=0
      ISHOW=0
      NUSE=4
      MINL=0
      MINS=0
      MAXL=0
      MAXS=0
      LAB(1:44) = ' PICREG RAW TIEPOINT DATA SET  (XXXX POINTS)'
      MSG11(1:80) = 
     +' TIEPOINT NO.      HAS THE LARGEST DISAGREEMENT OF       PIXELS WITH FIT SURFACE'
      MSG13(1:59) = 
     +' L1=         S1=         L2=         S2=         PTNUM=    '
      MSG14(1:33) = ' TYPICAL RESIDUAL IS       PIXELS'
      MSG15(1:41) = ' NL1=      NS1=      NL2=      NS2=      '
      MSG24(1:51) =' CORRELATION QUALITY FOR TIEPOINT      QUESTIONABLE'
      CALL MVE(7,8000,0.,PTS,0,1)    ! ZERO PTS ARRAY.
      DO 2 I=1,256
      LUT(I)=I-1
      LUT1(I) = INT2BYTE(I-1)
      LUT2(I) = INT2BYTE(I-1)
    2 CONTINUE
C
C  OPEN FIRST AND SECOND INPUTS
	CALL XVUNIT( IN1, 'INP', 1, IST,' ')
	CALL XVUNIT( IN2, 'INP', 2, IST,' ')
	CALL XVOPEN( IN1, IST, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
	CALL XVOPEN( IN2, IST, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
C
C  GET SIZES OF INPUTS
	CALL XVGET( IN2, IST, 'NL', NLI2, 'NS', NSI2, 'FORMAT', FMT,' ')
	IF (FMT.NE.'BYTE') GO TO 993
	CALL XVGET( IN1, IST, 'NL', NLI1, 'NS', NSI1, 'FORMAT', FMT,' ')
	IF (FMT.NE.'BYTE') GO TO 993
C
C  PRINT SIZES OF INPUTS
      WRITE (MSG15(6:10),'(I5)') NLI1
      WRITE (MSG15(16:20),'(I5)') NSI1
      WRITE (MSG15(26:30),'(I5)') NLI2
      WRITE (MSG15(36:40),'(I5)') NSI2
      CALL XVMESSAGE(MSG15(2:41),' ')
C
C  PROCESS ORIGINAL PARAMETERS

      CALL XVPCNT( 'INP', NI )       ! NUMBER OF INPUT AND OUTPUT FILES.
      CALL XVPCNT( 'OUT', NO )

      CALL KEYWRD(xvparm,NPAR,LUT,LUT1,LUT2,IND,NO,.false.)
      IF(IND.NE.0) CALL ABEND
      ZOOM1=IZOOM1
      ZOOM2=IZOOM2
	IF (ZOOM1.EQ.0.) ZOOM1 = 1.0
	IF (ZOOM2.EQ.0.) ZOOM2 = 1.0
	IF (ZOOM1.LT.0.0) ZOOM1=-1.0/ZOOM1
	IF (ZOOM2.LT.0.0) ZOOM2=-1.0/ZOOM2
	OLDZ(1) = ZOOM1
	OLDZ(2) = ZOOM2
C
C  SET UP LABELS FOR FIRST OUTPUT DATA SET
3	CALL XVUNIT( OUT1, 'OUT', 1, IST,' ')
	CALL XVOPEN( OUT1, IST, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     &	 'OP', 'WRITE', 'U_NL', 10, 'U_NS', 800, 'O_FORMAT',
     &   'REAL', 'U_FORMAT', 'REAL',' ')
        WRITE (LAB(33:36),'(I4)') 0
	CALL XLADD( OUT1, 'HISTORY', 'COMMENT', LAB, IST, 'FORMAT', 
     +'STRING', 'ULEN', 44,' ')
C
	CALL XVWRIT( OUT1, PTS, IST,' ')	!INITIALIZE OUTPUT FILE
C
C  RE-OPEN FOR UPDATE:
	CALL XVCLOSE( OUT1, IST,' ')
	CALL XVOPEN( OUT1, IST, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     &	 'OP', 'UPDATE', 'U_NL', 10, 'U_NS', 800, 'O_FORMAT',
     &   'REAL', 'U_FORMAT', 'REAL',' ')
C
C  OPEN DEVICES  (VIDEO, GRAPHICS, AND TRACKBALLS)
      CALL DEVICE(V1,VL,VR,G1,T1,T2,LUTV1,LUTL,LUTR,NTB,CFORM,*950)
C
C-----DEFINE SCREEN AREAS
	NLDS = SCREEN
	NSDS = SCREEN /2 - 2
	LEFT(1) = 1
	TOP(1) = 1
	BOTTOM(1) = SCREEN
	RIGHT(1) = SCREEN/2 - 2           ! -2 TO MAKE IT EVEN
	LEFT(2) = SCREEN/2 + 3            ! +3 TO START ON HWD BOUND.
	TOP(2) = 1
	BOTTOM(2) = SCREEN
	RIGHT(2) = SCREEN
	IOFF = 4                       !GAP BETWEEN SIDES
C-----INITIAL DISPLAY
	CALL FILLIMP(VL,IN1,SL1,SS1,NLI1,NSI1,IZOOM1,ZOOM1)
	CALL FILLIMP(VR,IN2,SL2,SS2,NLI2,NSI2,IZOOM2,ZOOM2)
	CALL DISP(V1,2,SS1,SL1,ZOOM1,NSI1,NLI1,OLDZ,*10,*999)
	CALL DISP(V1,3,SS2,SL2,ZOOM2,NSI2,NLI2,OLDZ,*10,*999)
C
C        CHECK FOR PREVIOUS TIEPOINTS IN THIRD INPUT
      IF(NI.EQ.3) CALL GETPTS( G1, LAB, OUT1)
      IPT=NPTS
C
C
C        ***** MAIN TIEPOINT ACQUISTION LOOP *****
10    IREDO=0
      IDEL=0
	NDEL = 0
	CFORM = 0
      IFORM=0
      DELTA=0.0
      IDISP1=0
      IDISP2=0
      IFIT=0
      IEXIT=0
      ISTRE=0
      ISHOW=0
C
C        PARAMETER PROCESSOR
      CALL XVMESSAGE('PICREG READY',' ')

      CALL XVINTRACT( 'IPARAM', 'Enter parameters:' )   !wait for inter. params.

      CALL KEYWRD(xviparm,NPAR,LUT,LUT1,LUT2,IND,NO,.true.)
      IF(IND.NE.0) GO TO 10

C        IF ONLY CARRIAGE RETURN (NO PARAMETERS)  GET NEW TIEPOINT
      IF(NPAR.EQ.0) GO TO 65
C
C-------WANT NEW CURSOR FORM?
	IF(CFORM .NE. 0) CALL CRFORM(T1,CFORM,*10)
C
C        PROCESS ZOOM PARAMETERS
      ZOOM1=IZOOM1
      ZOOM2=IZOOM2
	IF (ZOOM1.EQ.0.) ZOOM1 = 1.0
	IF (ZOOM2.EQ.0.) ZOOM2 = 1.0
	IF (ZOOM1.LT.0.0) ZOOM1=-1.0/ZOOM1
	IF (ZOOM2.LT.0.0) ZOOM2=-1.0/ZOOM2
C
C        PROCESS NEW STRETCH PARAMETERS
      IF(ISTRE.NE.0) CALL USELUT(0,LUT)
C        CHECK FOR EXIT, FIT, FORMAT PARAMETERS
      IF(IEXIT.EQ.1) GO TO 800
      IF(IFIT.NE.0.OR.IFORM.NE.0) GO TO 600
C
C     REWRITE DISPLAY IF NECESSARY
	IF(IDISP1 .EQ. 0) GO TO 32
	CALL DISP(V1,2,SS1,SL1,ZOOM1,NSI1,NLI1,OLDZ,*10,*30)
	GO TO 31
30	CONTINUE
	CALL FILLIMP(VL,IN1,SL1,SS1,NLI1,NSI1,IZOOM1,ZOOM1)
	CALL DISP(V1,2,SS1,SL1,ZOOM1,NSI1,NLI1,OLDZ,*10,*999)
31	IF(NPTS .NE. 0) CALL REPLACE(G1,1)	   !REPLACE GRAPHICS
32	IF(IDISP2 .EQ. 0) GO TO 40
	CALL DISP(V1,3,SS2,SL2,ZOOM2,NSI2,NLI2,OLDZ,*10,*35)
	GO TO 39
35	CONTINUE
	CALL FILLIMP(VR,IN2,SL2,SS2,NLI2,NSI2,IZOOM2,ZOOM2) 
	CALL DISP(V1,3,SS2,SL2,ZOOM2,NSI2,NLI2,OLDZ,*10,*999)
39	IF(NPTS .NE. 0) CALL REPLACE(G1,2)	   !REPLACE GRAPHICS
C
C        IF REDO SPECIFIED,  ERASE THAT POINT
40    IF(IREDO.EQ.0) GO TO 45
      CALL UNDRAW(G1,IREDO)
      IPT=IREDO

      CALL XVMESSAGE('Position cursor on left side and press <RETURN>',
     .            ' ')
      CALL XVINTRACT( 'READY', '  <RETURN' )

      GO TO 65
C
C
C  IF DELETE SPECIFIED, DELETE POINTS AND RENUMBER REMAINING POINTS
45	CONTINUE
	IF(NDEL .NE. 0) CALL DELTPS(LAB,G1,3,OUT1)
C
C  IF SHOW SPECIFIED, DISPLAY TIEPOINT DISPLACEMENTS
	IF(ISHOW.NE.0) CALL SHOWTP(G1)
	GO TO 10
C
C  GET A NEW PAIR OF TIEPOINTS
65	CALL GTPAIR(G1,T1,T2,NTB)
C
	GO TO (300,110,120),MODE
C
C  AREA OPTION  (MODE=2)
110	CALL UNDRAW(G1,IPT)
	CALL HISTCN(NLW,HISTH,HISTV,PTS,NSI1,NSI2,NLI1,NLI2,CONV,
     &   IN1,IN2,IPT,IN)
C  REDRAW LEFT SIDE TIEPOINT AT NEW POSITION
	CALL DRAWGR(IPT,G1,1)
	GO TO 300
C
C  CORRELATION OPTION  (MODE=3)
120   IPASS=0
121   IPASS=IPASS+1
      OLDLIN=PTS(1,IPT)
      OLDSAM=PTS(2,IPT)
      IND=0
      CALL XCOR2(PTS,ILIN,IPT,IND,IN,R1,R2,NUM,NUM+2,IPASS,
     &           ISAM,SCALE,IN1,IN2,INTER)
      IF(IND.NE.0) CALL UNDRAW(G1,IPT)
      IF(IND.NE.0) GO TO 10
C        DECIDE IF ADDITIONAL PASS REQUIRED
      DELTA=SQRT((OLDLIN-PTS(1,IPT))**2+(OLDSAM-PTS(2,IPT))**2)
C!!!!!!!!      write (*,*) DELTA
      IF (INTER .EQ. 0) THEN
         IF(IPASS.LT.3.AND.DELTA.GT.1.1) GO TO 121
         IF(IPASS.GE.3.AND.DELTA.GE.2.1) WRITE (MSG24(35:38),'(I4)') IPT
         IF(IPASS.GE.3.AND.DELTA.GE.2.1) CALL XVMESSAGE(MSG24(2:51),' ')
      ELSE
         IF(IPASS.LT.3.AND.DELTA.GT. .1) GO TO 121
         IF (IPASS.GE.3.AND.DELTA.GE..2) WRITE (MSG24(35:38),'(I4)') IPT
         IF (IPASS.GE.3.AND.DELTA.GE..2) CALL XVMESSAGE(MSG24(2:51),' ')
      END IF
C        DISPLAY CROSS CORRELATION MATRIX
	CALL DISPX(V1,R2,NUM,SCALE,ILIN,ISAM,SCREEN)
C
C        DRAW RIGHT SIDE TIEPOINT
300   CALL DRAWGR(IPT,G1,2)
      WRITE (MSG13(5:11),'(F7.1)') PTS(3,IPT)
      WRITE (MSG13(17:23),'(F7.1)') PTS(4,IPT)
      WRITE (MSG13(29:35),'(F7.1)') PTS(1,IPT)
      WRITE (MSG13(41:47),'(F7.1)') PTS(2,IPT)
      WRITE (MSG13(56:59),'(I4)') IPT
      CALL XVMESSAGE(MSG13(2:59),' ')
C
C  WRITE RAW TIEPOINTS TO FIRST OUTPUT DATA SET
        WRITE (LAB(33:36),'(I4)') NPTS
	CALL XLDEL( OUT1, 'HISTORY', 'COMMENT', IST,' ')
	CALL XLADD( OUT1, 'HISTORY', 'COMMENT', LAB, IST, 'FORMAT', 
     +'STRING', 'ULEN', 44,' ')
	NL2 = (NPTS-1) / 200 + 1
	CALL XVGET( OUT1, IST, 'NL', NL02, ' ')
	IF (NL02.LT.NL2) THEN
	  CALL XLDEL( OUT1, 'SYSTEM', 'NL', IST,' ')
	  CALL XLADD( OUT1, 'SYSTEM', 'NL', NL2, IST,
     &	   'FORMAT', 'INT',' ')
	ENDIF
C
	DO I=1,NL2
	  CALL XVWRIT( OUT1, PTS(1,200*(I-1)+1), IST, 'LINE',I,' ')
	ENDDO
C
      GO TO 10
C
C
C       ***** FIT SURFACE TO TIEPOINTS *****
C
600   IF(IFORM.GT.7) GO TO 700
      IF(NPTS.LT.4) CALL XVMESSAGE('MORE THAN 3 TIEPOINTS NEEDED',' ')
      IF(NPTS.LT.4) GO TO 10
      IF(IFIT.EQ.1) CALL XVMESSAGE('ROTATION + OFFSET',' ')
      IF(IFIT.EQ.2) CALL XVMESSAGE('ROTATION + OFFSET + SCALE',' ')
      IF(IFIT.EQ.3) CALL XVMESSAGE('FIRST ORDER UNCONSTRAINED',' ')
      IF(IFIT.EQ.4) CALL XVMESSAGE('SCALE + OFFSET',' ')
      IF(IFIT.EQ.5) CALL XVMESSAGE('OFFSET',' ')
      IF(IFIT.EQ.6) CALL XVMESSAGE('SECOND ORDER UNCONSTRAINED',' ')
      IF(IFIT.EQ.7) CALL XVMESSAGE('THIRD ORDER UNCONSTRAINED',' ')
C
      IF(IFIT.EQ.3) GO TO 630
      IF(IFIT.EQ.6.OR.IFIT.EQ.7) GO TO 620
C
C        FIT OPTIONS 1, 2, 4, 5
      CALL OPT12(NPTS,CL,PTS,CX,COEF,V,E,EX,B,A,RMAG,RMAX,
     &           LMAX,IFIT,Z1,Z2,Z3,IPRINT,R2)
      GO TO 655
C
C        FIT OPTIONS 6, 7
620   IF(IFIT.EQ.6.AND.NPTS.LT.7)
     &  CALL XVMESSAGE('NEED MORE THAN 6 TIEPOINTS',' ')
      IF(IFIT.EQ.6.AND.NPTS.LT.7) GO TO 10
      IF(IFIT.EQ.7.AND.NPTS.LT.11)
     &  CALL XVMESSAGE('NEED MORE THAN 10 TIEPOINTS',' ')
      IF(IFIT.EQ.7.AND.NPTS.LT.11) GO TO 10
      CALL OPT67(NPTS,CL,PTS,CX,COEF,V,E,EX,RMAX,LMAX,
     &           IFIT,E1,Z1,Z2,Z3,IPRINT)
      GO TO 650
C
C        FIT OPTION 3
630   CALL OPT3(NPTS,CL,PTS,CX,COEF,V,E,EX,R2,RMAX,LMAX,
     &          E1,Z1,Z2,Z3,IPRINT)
C
650   E=(E+E1)/2.
655   WRITE (MSG14(22:26),'(F5.1)') E
      CALL XVMESSAGE(MSG14(2:33),' ')
      WRITE (MSG11(15:18),'(I4)') LMAX
      WRITE (MSG11(52:56),'(F5.1)') RMAX
      CALL XVMESSAGE(MSG11(2:80),' ')
      IF(IFORM.EQ.0) GO TO 10
C
C
C        **** FORMAT TIEPOINTS FOR TIECONM, GEOMA, LGEOM, OR MGEOM ****
C
700	CALL FORMT(COEF,A,B,RMAG,CL,CX,V,EX,E,Z1,Z2,Z3,R1,
     1              PAR,OUTFILE(2))
      GO TO 10
C
C
C        CLOSE DATA SETS
800   CALL XVCLOSE( IN1, IST,' ')
      CALL XVCLOSE( IN2, IST,' ')
      CALL XVCLOSE( OUT1, IST,' ')
C
	XST = XDDACTIVATE(IDEV,.FALSE.)	!DEACTIVATE DEVICE
	XST = XDDCLOSE(IDEV)		!CLOSE DEVICE

C PRINT OUT THE TIEPOINTS
900   IF(IPRINT.EQ.0) GO TO 950
      CALL XVMESSAGE('TIEPOINTS  NL,NS,OL,OS',' ')
      N=-3
      DO 920 L=1,NPTS
      N=N+4
  920 CALL PRNT(7,4,PT(N), '.')
C
950   RETURN
C
993	CALL XVMESSAGE('ONLY BYTE DATA ACCEPTED',' ')
999	CALL ABEND
      END
c
c******************************************************
      SUBROUTINE DRAWGR(IPT,G1,ISIDE)
      include 'fortport'  ! DEFINES INT2BYTE 

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM
      INTEGER LENG
	COMMON/CX/IP(90),PTS,jdum
	COMMON/CD/IDEV,KDUM(18), GDN
        INTEGER GDN

      REAL PTS(4,2000)
	INTEGER G1,PX(2),PY(2),IDEV
      CHARACTER*4 PTNUM
	LOGICAL XST, XDIPOLYLINE, XDTTEXT
      DATA PTNUM/' '/
C==================================================================
      IF(ISIDE.EQ.2) GO TO 50
C
C        CHECK IF TIEPOINT LOCATED IN DISPLAYED PORTION OF IMAGE 1
      X=PTS(4,IPT)-SS1+1
      X=X*ZOOM1
      IF(X.LT.6.0.OR.X.GT.NSDS-7) GO TO 50
      Y=PTS(3,IPT)-SL1+1
      Y=Y*ZOOM1
      IF(Y.LT.6.0.OR.Y.GT.NLDS-6) GO TO 50
C
C        DRAW CROSS ON GRAPHICS
	PX(1) = X-5.
	PX(2) = X+5.
	PY(1) = Y
	PY(2) = Y
	XST = XDIPOLYLINE(IDEV,G1,INT2BYTE(GDN),2,PX,PY)
	PX(1) = X
	PX(2) = X
	PY(1) = Y-5.
	PY(2) = Y+5.
	XST = XDIPOLYLINE(IDEV,G1,INT2BYTE(GDN),2,PX,PY)
C
C        LABEL CROSS WITH TIEPOINT NUMBER
      IF(X.GT.NSDS-24.OR.Y.GT.NLDS-15) GO TO 50
      WRITE (PTNUM(1:4),'(I4)') IPT
      IF (IPT .LT. 10) THEN
         LENG = 1
      ELSE IF (IPT .LT. 100) THEN
         LENG = 2
      ELSE IF (IPT .LT. 1000) THEN
         LENG = 3
      ELSE
         LENG = 4
      END IF
	PX(1) = X+6.          ! LEFT JUSTIFY DIGITS.
	PY(1) = Y+6.
	XST = XDTTEXT(IDEV,G1,PX,PY,1,LENG,PTNUM(4-LENG+1:4))
C
50    IF(ISIDE.EQ.1) RETURN
C
C        CHECK IF TIEPOINT LOCATED IN DISPLAYED PORTION OF IMAGE 2
      X=PTS(2,IPT)-SS2+1
      X=X*ZOOM2+NSDS+IOFF
      IF(X.LT.NSDS+7.OR.X.GT.2*NSDS-6) RETURN
      Y=PTS(1,IPT)-SL2+1
      Y=Y*ZOOM2
      IF(Y.LT.6.0.OR.Y.GT.NLDS-6) RETURN
C        DRAW CROSS ON GRAPHICS
	PX(1) = X-5.
	PX(2) = X+5.
	PY(1) = Y
	PY(2) = Y
	XST = XDIPOLYLINE(IDEV,G1,INT2BYTE(GDN),2,PX,PY)
	PX(1) = X
	PX(2) = X
	PY(1) = Y-5.
	PY(2) = Y+5.
	XST = XDIPOLYLINE(IDEV,G1,INT2BYTE(GDN),2,PX,PY)
C        LABEL CROSS WITH TIEPOINT NUMBER
      IF(X.GT.2*NSDS-23.OR.Y.GT.NLDS-15) RETURN
      WRITE (PTNUM(1:4),'(I4)') IPT
      IF (IPT .LT. 10) THEN
         LENG = 1
      ELSE IF (IPT .LT. 100) THEN
         LENG = 2
      ELSE IF (IPT .LT. 1000) THEN
         LENG = 3
      ELSE
         LENG = 4
      END IF
	PX(1) = X+6.          ! LEFT JUSTIFY DIGITS.
	PY(1) = Y+6.
	XST = XDTTEXT(IDEV,G1,PX,PY,1,LENG,PTNUM(4-LENG+1:4))
        RETURN
      END
c
c**************************************************************
      SUBROUTINE KEYWRD(XXPARM,NPAR,LUT,LUT1,LUT2,IND,NO,
     + interactive)

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

      COMMON/C6/ LCEN,ICEN,IPOW,NUM,ISIGN,IPRINT,IDEL,NDEL
      COMMON/CX/ IPARM,PTS,IPT
      REAL RPARM(90),PTS(4,2000)
      INTEGER IPARM(90)
      INTEGER SL1SAV,SL2SAV,SS1SAV,SS2SAV
      INTEGER LUT(256)
      logical interactive
      CHARACTER*132 LBUF
      BYTE LUT1(256),LUT2(256)
      EQUIVALENCE (IPARM,RPARM)
      CHARACTER*8 CPARM
      EXTERNAL XXPARM
      LOGICAL XST, XDTSIZE

C  REVISION HISTORY
C     3-7-86  SP  CONVERTED TO USE VICAR2 PARMETER PROCESSING.
C     3-7-86  SP  MADE NPAR AN OUTPUT PARAMETER. NPAR=0 IF NO
C                 INTERACTIVE PARAMETERS WERE ENTERED.
C     3-7-86  SP  MADE XXPARM A PARAMETER TO THIS ROUTINE THAT POINTS
C                 EITHER TO XVPARM OR XVIPARM. (SEE EXTERNAL STATEMENT IN
C                 MAIN44.)
C==========================================================

      NPAR = 0
      IND=0
C        SAVE DISPLAY PARAMETERS IN CASE OF USER ERROR
      SL1SAV=SL1
      SL2SAV=SL2
      SS1SAV=SS1
      SS2SAV=SS2
      IZ1SAV=IZOOM1
      IZ2SAV=IZOOM2

C        'EXIT'
      if(interactive)then
      CALL XXPARM( 'EXIT', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IEXIT=1
      END IF
      end if

C        'UP 1'

      CALL XXPARM( 'U1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL1=SL1-IPARM(1)
            IF(SL1.LT.1) SL1=1
            IDISP1=1
      END IF

C        'UP 2'

      CALL XXPARM( 'U2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL2=SL2-IPARM(1)
            IF(SL2.LT.1) SL2=1
            IDISP2=1
      END IF

C        'DOWN 1'

      CALL XXPARM( 'D1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL1=SL1+IPARM(1)
            IF(SL1.GT.NLI1) SL1=NLI1-NLDS+1
            IF(SL1.LT.1) SL1=1
            IDISP1=1
      END IF

C        'DOWN 2'

      CALL XXPARM( 'D2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL2=SL2+IPARM(1)
            IF(SL2.GT.NLI2) SL2=NLI2-NLDS+1
            IF(SL2.LT.1) SL2=1
            IDISP2=1
      END IF

C        'LEFT 1'

      CALL XXPARM( 'L1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SS1=SS1-IPARM(1)
            IF(SS1.LT.1) SS1=1
      	IF (MOD(SS1,2) .EQ. 0) SS1 = SS1 + 1      !TO MAKE ODD
            IDISP1=1
      END IF

C        'LEFT 2'

      CALL XXPARM( 'L2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SS2=SS2-IPARM(1)
            IF(SS2.LT.1) SS2=1
      	IF(MOD(SS2,2) .EQ. 0) SS2 = SS2 + 1      !TO MAKE ODD
            IDISP2=1
      END IF

C        'RIGHT 1'

      CALL XXPARM( 'R1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SS1=SS1+IPARM(1)
            IF(SS1.GT.NSI1) SS1=NSI1-NSDS+1
            IF(SS1.LT.1) SS1=1
            IF(MOD(SS1,2) .EQ. 0) SS1 = SS1 + 1      !TO MAKE ODD
            IDISP1=1
      END IF

C        'RIGHT 2'

      CALL XXPARM( 'R2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SS2=SS2+IPARM(1)
            IF(SS2.GT.NSI2) SS2=NSI2-NSDS+1
            IF(SS2.LT.1) SS2=1
            IF(MOD(SS2,2) .EQ. 0) SS2 = SS2 + 1      !TO MAKE ODD
            IDISP2=1
      END IF

C        'UP'

      CALL XXPARM( 'U', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL1=SL1-IPARM(1)
            IF(SL1.LT.1) SL1=1
            IDISP1=1
            SL2=SL2-IPARM(1)
            IF(SL2.LT.1) SL2=1
            IDISP2=1
      END IF

C        'DOWN'

      CALL XXPARM( 'D', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL1=SL1+IPARM(1)
            IF(SL1.GT.NLI1) SL1=NLI1-NLDS+1
            IF(SL1.LT.1) SL1=1
            IDISP1=1
            SL2=SL2+IPARM(1)
            IF(SL2.GT.NLI2) SL2=NLI2-NLDS+1
            IF(SL2.LT.1) SL2=1
            IDISP2=1
      END IF

C        'LEFT'

      CALL XXPARM( 'L', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SS1=SS1-IPARM(1)
            IF(SS1.LT.1) SS1=1
            IF(MOD(SS1,2) .EQ. 0) SS1 = SS1 + 1      !TO MAKE ODD
            IDISP1=1
            SS2=SS2-IPARM(1)
            IF(SS2.LT.1) SS2=1
            IF(MOD(SS2,2) .EQ. 0) SS2 = SS2 + 1      !TO MAKE ODD
            IDISP2=1
      END IF

C        'RIGHT'

      CALL XXPARM( 'R', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SS1=SS1+IPARM(1)
            IF(SS1.GT.NSI1) SS1=NSI1-NSDS+1
            IF(SS1.LT.1) SS1=1
            IF(MOD(SS1,2) .EQ. 0) SS1 = SS1 + 1      !TO MAKE ODD
            IDISP1=1
            SS2=SS2+IPARM(1)
            IF(SS2.GT.NSI2) SS2=NSI2-NSDS+1
            IF(SS2.LT.1) SS2=1
            IF(MOD(SS2,2) .EQ. 0) SS2 = SS2 + 1      !TO MAKE ODD
            IDISP2=1
      END IF

C        'NHOR'

      CALL XXPARM( 'NHOR', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          NH=IPARM(1)
          IF(NH.LT.2.OR.NH.GT.30) THEN
      		CALL XVMESSAGE('INVALID NHOR',' ')
      	        GO TO 950
          END IF
          NHOR = NH
      END IF

C        'NVER'

      CALL XXPARM( 'NVER', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          NV=IPARM(1)
          IF(NV.LT.2.OR.NV.GT.30) THEN
      		CALL XVMESSAGE('INVALID NVER',' ')
          	GO TO 950
          END IF
          NVER = NV
      END IF

C        'REDO'

      CALL XXPARM( 'REDO', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
        if(interactive)then
          NPAR = 1
          IF(IPARM(1).GT.NPTS.OR.IPARM(1).LT.1) THEN
                CALL XVMESSAGE('UNREASONABLE TIEPOINT NUMBER',' ')
            	GO TO 950
          END IF
          IREDO=IPARM(1)
        else
          call XVMESSAGE('REDO invalid on command line, ignored',' ')
        endif
      END IF

C        'DELETE'

      CALL XXPARM( 'DELETE', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
        if(interactive)then
          NPAR = 1
          I1=IPARM(1)
          I2=IPARM(2)
          IF(I1.LT.1.OR.I1.GT.NPTS.OR.I2.LT.I1.OR.I2.GT.NPTS) THEN
                CALL XVMESSAGE('INVALID TIEPOINT RANGE',' ')
      		GO TO 950
          END IF
          IDEL=I1
          NDEL=I2-I1+1
        else
          call XVMESSAGE('DELETE invalid on command line, ignored',' ')
        endif
      END IF

C        'AREA'

      CALL XXPARM( 'AREA', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          NLW=IPARM(1)
          IF(NLW.GT.1000) NLW=1000
          MODE=2
      END IF

C        'MIN' & 'MAX'

      CALL XXPARM( 'CONV', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IF (CPARM .EQ. 'MIN ')then
             CONV=-1
          else IF (CPARM .EQ. 'MAX ')then
             CONV= 1
          else
             call XVMESSAGE('Conv is either MIN ot MAX',' ')
          endif
      END IF

C        'POW'    

      CALL XXPARM( 'POWER', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          if(iparm(1) .lt. 1 .or. iparm(1) .gt. 6) then
		call XVMESSAGE('POW GT 6 OR LT 1...INVALID',' ')
		GO TO 950
          ENDIF
          IPOW=IPARM(1)
          MODE=3
          NUM=2**IPOW
      END IF

C        'CORRELATION'   &       'POINT'

      CALL XXPARM( 'MODE', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IF (CPARM .EQ. 'CORR')then
             MODE = 3
          else IF (CPARM .EQ. 'POINT')then
             MODE = 1
          else
             call XVMESSAGE('Mode is either CORR or POINT',' ')
          endif
      END IF

C        'HPF - HIGH PASS FILTER'

      CALL XXPARM( 'HIPASS', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IF (CPARM .EQ. 'HPF')then
             IHPF = 1
          else IF (CPARM .EQ. 'NOHPF')then
             IHPF = 0
          else
              call XVMESSAGE('Hipass is either HPF or NOHPF',' ')
          endif
      END IF

C        'PHASE - TURN ON PHASE CORRELATION

      CALL XXPARM( 'PHASE', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IF (CPARM .EQ. 'PHASE')then
             IPHAS = 1
          else IF (CPARM .EQ. 'NOPHASE')then
             IPHAS = 0
          else
             call XVMESSAGE('Phase is either PHASE or NOPHASE',' ')
          endif
      END IF

C        'FIT'

      CALL XXPARM( 'FIT', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
        if(interactive)then
          NPAR = 1
          IFIT=IPARM(1)
          IF(IFIT.LT.1.OR.IFIT.GT.7) THEN
      		CALL XVMESSAGE('INVALID FIT',' ')
      		GO TO 950
          END IF
        else
          call XVMESSAGE('FIT invalid on command line, ignored',' ')
        endif
      END IF

C        'TPFORM' ...............CHANGED FROM 'FORMAT'

      CALL XXPARM( 'TPFORM', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
        if(interactive)then
          NPAR = 1
          IF(NO .LT. 2) THEN
		CALL XVMESSAGE('NO OUTPUT PARAMETER FILE',' ')
		GO TO 950
          END IF
          IFORM=IPARM(1)
          IFIT=IFORM
          IF(IFORM.LT.1.OR.IFORM.GT.9) THEN
		CALL XVMESSAGE('INVALID FORMAT #',' ')
		GO TO 950
      	  END IF
        else
          call XVMESSAGE('TPFORM invalid on command line, ignored',' ')
        endif
      END IF

C        'USE'

      CALL XXPARM( 'USE', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          NUSE=IPARM(1)
          IF(NUSE.GT.10) NUSE=10
          IF(NUSE.LT.4) NUSE=4
      END IF

C        'INTERPOLATE'   &  'NO INTERPOLATION'

      CALL XXPARM( 'INTERP', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IF (CPARM .EQ. 'INTE' )then
             INTER=1
          else IF (CPARM .EQ. 'NOIN' )then
             INTER=0
          else
             call XVMESSAGE('Interp is either INTE or NOIN',' ')
          endif
      END IF

C        'TEXTSIZE'

      CALL XXPARM( 'TEXTSIZE', IPARM, ICOUNT, IDEF , 0)
      IF (ICOUNT .GT. 0)  THEN
            NPAR = 1
            XST = XDTSIZE(IPARM(1), .7)
            IDISP1=1
            IDISP2=1
      END IF

C        'ZOOM'

      CALL XXPARM( 'ZOOM', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            IZOOM1=IPARM(1)
            IF(IZOOM1.GT.8) CALL XVMESSAGE('ZOOM SET TO MAXIMUM OF 8',
     .                                     ' ')
            IF(IZOOM1.GT.8) IZOOM1=8
            IZOOM2=IZOOM1
            IDISP1=1
            IDISP2=1
      END IF

C        'ZOOM 1'

      CALL XXPARM( 'Z1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IZOOM1=IPARM(1)
          IF(IZOOM1.GT.8) CALL XVMESSAGE('ZOOM1 SET TO MAXIMUM OF 8',
     .                                   ' ')
          IF(IZOOM1.GT.8) IZOOM1=8
          IDISP1=1
      END IF

C        'ZOOM 2'

      CALL XXPARM( 'Z2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IZOOM2=IPARM(1)
          IF(IZOOM2.GT.8) CALL XVMESSAGE('ZOOM2 SET TO MAXIMUM OF 8',
     .                                   ' ')
          IF(IZOOM2.GT.8) IZOOM2=8
          IDISP2=1
      END IF

C        'STRETCH'

      CALL XXPARM( 'STRETCH', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            ILO=IPARM(1)
            IHI=IPARM(2)
            IF(ILO.EQ.IHI) ILO=IHI-1
            SLOPE=255./(IHI-ILO)
            OFFSET=(255.-SLOPE*IHI)
            DO L=1,256
            I=SLOPE*(L-1)+OFFSET +0.5
            IF(I.LT.0) I=0
            IF(I.GT.255) I=255
            LUT(L)=I
            LUT1(L) = INT2BYTE(I)
            LUT2(L) = INT2BYTE(I)
            END DO
            ISTRE=1
      END IF

C        'LINEAR'

      CALL XXPARM( 'LINEAR', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            ILO=IPARM(1)
            IHI=IPARM(2)
            IF(ILO.EQ.IHI) ILO=IHI-1
            SLOPE=255./(IHI-ILO)
            OFFSET=(255.-SLOPE*IHI)
            DO  L=1,256
            I=SLOPE*(L-1)+OFFSET +0.5
            IF(I.LT.0) I=0
            IF(I.GT.255) I=255
            LUT(L)=I
            LUT1(L) = INT2BYTE(I)
            LUT2(L) = INT2BYTE(I)
            END DO
            ISTRE=1
      END IF

C        'LINEAR 1'
C
C      CALL XXPARM( 'LIN1', IPARM, ICOUNT, IDEF , 0)
C      IF (IDEF .EQ. 0)  THEN
C            NPAR = 1
C            ILO=IPARM(1)
C            IHI=IPARM(2)
C            IF(ILO.EQ.IHI) ILO=IHI-1
C            SLOPE=255./(IHI-ILO)
C            OFFSET=(255.-SLOPE*IHI)
C            DO L=1,256
C            I=SLOPE*(L-1)+OFFSET +0.5
C            IF(I.LT.0) I=0
C            IF(I.GT.255) I=255
C            CALL ITLA(I,LUT1(L),1)
C            END DO
C            ISTRE=1
C      END IF
C
CC        'LINEAR 2'
C
C      CALL XXPARM( 'LIN2', IPARM, ICOUNT, IDEF , 0)
C      IF (IDEF .EQ. 0)  THEN
C            NPAR = 1
C            ILO=IPARM(1)
C            IHI=IPARM(2)
C            IF(ILO.EQ.IHI) ILO=IHI-1
C            SLOPE=255./(IHI-ILO)
C            OFFSET=(255.-SLOPE*IHI)
C            DO L=1,256
C            I=SLOPE*(L-1)+OFFSET +0.5
C            IF(I.LT.0) I=0
C            IF(I.GT.255) I=255
C            CALL ITLA(I,LUT2(L),1)
C            END DO
C            ISTRE=1
C      END IF

C        'SL1'

      CALL XXPARM( 'SL1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL1=IPARM(1)
            IF(SL1.LT.1.OR.SL1.GT.NLI1) THEN
      		CALL XVMESSAGE('SL1 ERROR',' ')
            	GO TO 950
            END IF
            IDISP1=1
      END IF

C        'SS1'

      CALL XXPARM( 'SS1', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          SS1=IPARM(1)
          IF(SS1.LT.1.OR.SS1.GT.NSI1) THEN
      		CALL XVMESSAGE('SS1 ERROR',' ')
          	GO TO 950
          END IF
          IF(MOD(SS1,2) .EQ. 0) SS1 = SS1 + 1      !TO MAKE ODD
          IDISP1=1
      END IF

C        'SL2'

      CALL XXPARM( 'SL2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          SL2=IPARM(1)
          IF(SL2.LT.1.OR.SL2.GT.NLI2) THEN
      		CALL XVMESSAGE('SL2 ERROR',' ')
		GO TO 950
          END IF
          IDISP2=1
      END IF

C        'SS2'

      CALL XXPARM( 'SS2', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          SS2=IPARM(1)
          IF(SS2.LT.1.OR.SS2.GT.NSI2) THEN
      		CALL XVMESSAGE('SS2 ERROR',' ')
		GO TO 950
          END IF
          IF(MOD(SS2,2) .EQ. 0) SS2 = SS2 + 1      !TO MAKE ODD
          IDISP2=1
      END IF

C        'MINL'

      CALL XXPARM( 'MINL', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          MINL=IPARM(1)
      END IF
      
C        'MINS'

      CALL XXPARM( 'MINS', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          MINS=IPARM(1)
      END IF

C        'MAXL'

      CALL XXPARM( 'MAXL', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          MAXL=IPARM(1)
      END IF

C        'MAXS'

      CALL XXPARM( 'MAXS', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          MAXS=IPARM(1)
      END IF

C        'HOME'

      CALL XXPARM( 'HOME', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            SL1=1
            SL2=1
            SS1=1
            SS2=1
            IDISP1=1
            IDISP2=1
      ENDIF

C        'PRINT'  &  'NOPRINT'

      CALL XXPARM( 'PRINT', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          IF ( CPARM .EQ. 'PRINT' )then
            IPRINT = 1
          else IF ( CPARM .EQ. 'NOPRINT' )then
            IPRINT = 0
          else
            call XVMESSAGE('Print is either PRINT or NOPRINT',' ')
          endif
      END IF

C        'DSTATE'

      CALL XXPARM( 'DSTAT', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
            NPAR = 1
            CALL XVMESSAGE('  SL1   SS1  ZOOM1    SL2   SS2  ZOOM2',' ')
            WRITE (LBUF,9900) SL1,SS1,IZOOM1,SL2,SS2,IZOOM2
9900  FORMAT ('  ',I5,' ',I5,'  ',I3,'    ',I5,' ',I5,'   ',I2,' ')
            CALL XVMESSAGE(LBUF(2:39),' ')
      ENDIF

C        'LOCATE'

      CALL XXPARM( 'LOCATE', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
        if(interactive)then
          NPAR = 1
          IPT=IPARM(1)
          IF(IPT.LT.1.OR.IPT.GT.NPTS) THEN
		CALL XVMESSAGE('INVALID TIEPOINT #',' ')
      		GO TO 950
          END IF
          ZOOM1=IZOOM1
          ZOOM2=IZOOM2
          IF(IZOOM1.LT.0) ZOOM1=-1.0/ZOOM1
          IF(IZOOM2.LT.0) ZOOM2=-1.0/ZOOM2
          SL1=PTS(3,IPT)-NLDS/(2*ZOOM1)
          IF(SL1.LT.1) SL1=1
          SS1=PTS(4,IPT)-NSDS/(2*ZOOM1)
          IF(SS1.LT.1) SS1=1
      	  IF(MOD(SS1,2) .EQ. 0) SS1 = SS1 + 1      !TO MAKE ODD
          SL2=PTS(1,IPT)-NLDS/(2*ZOOM2)
          IF(SL2.LT.1) SL2=1
          SS2=PTS(2,IPT)-NSDS/(2*ZOOM2)
          IF(SS2.LT.1) SS2=1
      	  IF(MOD(SS2,2) .EQ. 0) SS2 = SS2 + 1      !TO MAKE ODD
          IDISP1=1
          IDISP2=1
        else
          call XVMESSAGE('LOCATE invalid on command line, ignored',' ')
        endif
      END IF

C        'SHOW'

      CALL XXPARM( 'SHOW', CPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
         NPAR = 1
         ISHOW=1
      ENDIF

C        'CURS'

      CALL XXPARM( 'CURSOR', IPARM, ICOUNT, IDEF , 0)
      IF (IDEF .EQ. 0)  THEN
          NPAR = 1
          CFORM = IPARM(1)
      END IF

C
900   RETURN
C
C        BAD PARAMETER RETURN
950   CALL XVMESSAGE('RESPECIFY PARAMETER(S)',' ')
      SL1=SL1SAV
      SL2=SL2SAV
      SS1=SS1SAV
      SS2=SS2SAV
      IZOOM1=IZ1SAV
      IZOOM2=IZ2SAV
      IND=1
      RETURN
      END
C
c***********************************************************
      SUBROUTINE FFT2R (ID,IDSRN,IN,C,NX,MX,INDX)
C
C  COMPUTE 2D FFT2 TRANSFORM   RETURNED IN C
C  THE FOLLOWING PARAMETERS ARE REQUIRED
C  LCEN LINE CENTER OF DATA
C  ICEN SAMPLE CENTER OF DATA
C  IPOW 2**   POWER OF TRANSFORM
C  NUM   2**IPOW
C  NX = NUM
C  MX = NUM + 2
C  IDSRN = VICAR2 UNIT NUMBER
C  ID = 1: FIRST INPUT IMAGE,  = 2: SECOND.
C  ISIGN -1 DIRECT  +1 INVERSE
C  ALSO NEED NL,NS OF INPUT PICTURES
C

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

      COMMON/C6/ LCEN,ICEN,IPOW,NUM,ISIGN,idum(3)
      REAL     C(NX,MX)
      INTEGER  ISIZE(2,2)
      BYTE       IN(*)
      EQUIVALENCE (ISIZE,NLI1)
C
      NYQNUM = NX/2 + 1
      INDX = 0
      IF(ISIGN .EQ. -1) THEN
	      ISKP = ICEN - NX/2 - 1
	      L1   = LCEN - NX/2
	      IF(ISKP .LT. 0) INDX = 1
	      IF(L1   .LT. 1) INDX = 1
	      IF( ISIZE(ID,2)-ISKP .LT. NX) INDX = 1
	      IF( ISIZE(ID,1)-L1+1 .LT. NX) INDX = 1
	      IF(INDX .NE. 0) THEN
		CALL XVMESSAGE('TOO CLOSE TO PICTURE BORDER,TRY AGAIN',
     .                         ' ')
		RETURN 
	      END IF
C
	      DO 10 I=1,NX
	      CALL XVREAD( IDSRN, IN, IST, 'LINE', L1, 'SAMP', ISKP+1,
     &		'NSAMPS', NX,' ')
	      L1=0
10  	      CALL FLOATA(1,NX,IN,C(1,I))      ! DATA TYPE IS BYTE
C
	      CALL RFT2(C,NX,NX,1,IST)        ! DIRECT TRANSFORMATION
      ELSE                                  
   	      CALL RFT2(C,NX,NX,-1,IST)       ! INVERSE TRANSFORMTION
      END IF
C
      RETURN
      END
c
c**************************************************************
	SUBROUTINE CMULX(N,M,A,B)
C-------THIS ROUTINE WILL DO A COMPLEX MULTIPLY OF THE ARRAY 
C-------FORMAT RESULTING FROM AN RFT2 CALL
C-------THE FUNCTION IS     (A+Bi)*(C-Di)
	real a(n,m),b(n,m)
c
	do 10 l=1,m-1,2
	do 10 i=1,n
	r = a(i,l)   * b(i,l) + a(i,l+1) * b(i,l+1)
	s = a(i,l+1) * b(i,l) - a(i,l)   * b(i,l+1)
	b(i,l) = r
10	b(i,l+1) = s
c
	return
	end
C
C********************************************************
      SUBROUTINE GETPTS(G1,LAB,OUT1)

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

	COMMON/CX/IP(90),PTS,jdum
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN

	REAL PTS(4,2000)
	INTEGER NLB(4),OUT1
      CHARACTER*(*) LAB
	INTEGER IDEV,G1
C
	CALL XVUNIT( IN3, 'INP', 3, IST,' ')
	CALL XVOPEN( IN3, IST, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
	CALL XVGET( IN3, IST, 'NL', NLB(2), 'NS', NLB(3),' ')
	IF (NLB(3).NE.800) GO TO 999
C
	DO I=1,NLB(2)
	  CALL XVREAD( IN3, PTS(1,200*(I-1)+1), IST, 'NSAMPS', 800,' ')
	ENDDO
C
	CALL XVCLOSE( IN3, IST,' ')
C
C  FIND NUMBER OF TIEPOINTS IN RECORD
	J2 = 200*NLB(2)
	DO J=1,J2
	  IF (PTS(1,J).EQ.0.0) GO TO 20
	ENDDO
	J = J2+1
C
20	NPTS=J-1
	CALL PRNT(4,1,NPTS,'NUMBER OF POINTS IN INPUT 3 =.')
	IF(NPTS.EQ.0) RETURN
C
C  DISPLAY PREVIOUS TIEPOINTS
	DO I=1,NPTS
	  CALL DRAWGR(I,G1,3)
	ENDDO
C
C  WRITE PREVIOUS TIEPOINTS TO FIRST OUTPUT DATA SET
        WRITE (LAB(33:36),'(I4)') NPTS
	CALL XLDEL( OUT1, 'HISTORY', 'COMMENT', IST,' ')
	CALL XLADD( OUT1, 'HISTORY', 'COMMENT', LAB, IST, 'FORMAT', 
     +'STRING', 'ULEN', 44,' ')
C
	CALL XVGET( OUT1, IST, 'NL', NL02,' ')
	IF (NL02.LT.NLB(2)) THEN
	  CALL XLDEL( OUT1, 'SYSTEM', 'NL', IST,' ')
	  CALL XLADD( OUT1, 'SYSTEM', 'NL', NLB(2), IST,
     &	   'FORMAT', 'INT',' ')
	ENDIF
	DO I=1,NLB(2)
	  CALL XVWRIT( OUT1, PTS(1,200*(I-1)+1), IST, 'LINE',
     &	   I, 'NSAMPS', 800,' ')
	ENDDO
C
	RETURN
C
999	CALL XVMESSAGE('3RD INPUT NS NOT 800',' ')
	CALL ABEND
	END
C
C**********************************************************
      SUBROUTINE UNDRAW(G1,IPT)
      include 'fortport'  ! DEFINES INT2BYTE 

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

	COMMON/CX/IP(90),PTS,jdum
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN
      REAL PTS(4,2000)
      CHARACTER*4 TEXT
      INTEGER PX(2),PY(2),IDEV,G1
      LOGICAL XST,XDIPOLYLINE,XDTTEXT,XDTCOLOR
C
        WRITE (TEXT(1:4),'(I4)') IPT
C        LEFT SIDE
      X=PTS(4,IPT)-SS1+1
      Y=PTS(3,IPT)-SL1+1
      X=X*ZOOM1
      Y=Y*ZOOM1

      IF (IPT .LT. 10) THEN    ! number of digits to display.
         LENG = 1
      ELSE IF (IPT .LT. 100) THEN
         LENG = 2
      ELSE IF (IPT .LT. 1000) THEN
         LENG = 3
      ELSE
         LENG = 4
      END IF

      IF(X.LT.6..OR.X.GT.NSDS-7.OR.Y.LT.6..OR.Y.GT.NLDS-6) GO TO 50
C
	PX(1) = X-5.
	PX(2) = X+5.
	PY(1) = Y
	PY(2) = Y
	XST = XDIPOLYLINE(IDEV,G1,0,2,PX,PY)
	PX(1) = X
	PX(2) = X
	PY(1) = Y-5.
	PY(2) = Y+5.
	XST = XDIPOLYLINE(IDEV,G1,0,2,PX,PY)
	PX(1) = X+6.          ! LEFT JUSTIFY DIGITS TO BE ERASED.
	PY(1) = Y+6.
	XST = XDTCOLOR(0,0)
	XST = XDTTEXT(IDEV,G1,PX,PY,1,LENG,TEXT(4-LENG+1:4))
	XST = XDTCOLOR(INT2BYTE(GDN),0)
C        RIGHT SIDE
50    X=PTS(2,IPT)-SS2+1
      X=X*ZOOM2+NSDS+IOFF
      Y=PTS(1,IPT)-SL2+1
      Y=Y*ZOOM2
      IF(X.LT.NSDS+7.OR.X.GT.NSDS*2-6.OR.Y.LT.6.
     &           .OR.Y.GT.NLDS-6) RETURN
C
	PX(1) = X-5.
	PX(2) = X+5.
	PY(1) = Y
	PY(2) = Y
	XST = XDIPOLYLINE(IDEV,G1,0,2,PX,PY)
	PX(1) = X
	PX(2) = X
	PY(1) = Y-5.
	PY(2) = Y+5.
	XST = XDIPOLYLINE(IDEV,G1,0,2,PX,PY)
	PX(1) = X + 6.
	PY(1) = Y + 6.
	XST = XDTCOLOR(0,0)
	XST = XDTTEXT(IDEV,G1,PX,PY,1,LENG,TEXT(4-LENG+1:4))
	XST = XDTCOLOR(INT2BYTE(GDN),0)
      RETURN
      END
C
C***********************************************************
	SUBROUTINE DELTPS(LAB,G1,ISIDE,OUT1)

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

	COMMON/C6/FIL3(6),IDEL,NDEL
	COMMON/CX/IPARM(90),PTS,jdum
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	REAL PTS(4,2000)
	INTEGER OUT1
      CHARACTER*(*) LAB
	INTEGER IDEV,G1
c
      DO 46 IP=IDEL,NPTS
      CALL UNDRAW(G1,IP)
   46 CONTINUE
      NWORD=4*(NPTS-(IDEL+NDEL-1))
      NPTS=NPTS-NDEL
C        RENUMBER ANY POINTS BEYOND DELETED POINTS
      IF(NWORD.EQ.0) GO TO 48
C>REPLACED WITH IN-LINE CODE. CALL MVE(4,NWORD,PTS(1,IDEL+NDEL),PTS(1,IDEL),1,1)

      LTO   = IDEL			! array index for destination of move
      LFROM = IDEL+NDEL			! array index for source of move
      DO LMOVE = 1,NWORD,4		! move 4 values per iteration
         PTS(1,LTO) = PTS(1,LFROM)
         PTS(2,LTO) = PTS(2,LFROM)
         PTS(3,LTO) = PTS(3,LFROM)
         PTS(4,LTO) = PTS(4,LFROM)
         LFROM = LFROM + 1
         LTO   = LTO   + 1
      END DO

      DO 47 IP=IDEL,NPTS
      CALL DRAWGR(IP,G1,ISIDE)
   47 CONTINUE
C
C        ZERO BUFFER BEYOND GOOD POINTS AND UPDATE FIRST OUTPUT
48    CALL MVE(7,800-4*NPTS,0.,PTS(1,NPTS+1),0,1)
        WRITE (LAB(33:36),'(I4)') NPTS
	CALL XLDEL( OUT1, 'HISTORY', 'COMMENT', IST,' ')
	CALL XLADD( OUT1, 'HISTORY', 'COMMENT', LAB, IST, 'FORMAT', 
     +'STRING', 'ULEN', 44,' ')
	NL2 = (NPTS-1) / 200 + 1
	CALL XVGET( OUT1, IST, 'NL', NL02,' ')
	IF (NL02.LT.NL2) THEN
	  CALL XLDEL( OUT1, 'SYSTEM', 'NL', IST,' ')
	  CALL XLADD( OUT1, 'SYSTEM', 'NL', NL2, IST,
     &	   'FORMAT', 'INT',' ')
	ENDIF
C
	DO I=1,NL2
	  CALL XVWRIT( OUT1, PTS(1,200*(I-1)+1), IST, 'LINE',
     &	   I, 'NSAMPS', 800,' ')
	ENDDO
c
	RETURN
	END
C
C***********************************************************
	SUBROUTINE SHOWTP(G1)
      include 'fortport'  ! DEFINES INT2BYTE 

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

	COMMON/CX/IPARM(90),PTS,jdum
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	REAL PTS(4,2000)
	INTEGER IDEV,G1,PX(2),PY(2)
	LOGICAL XST, XDIPOLYLINE
C
      LINOFF=(PTS(1,1)-PTS(3,1))
      SAMOFF=(PTS(2,1)-PTS(4,1))
      DO 55 IP=1,NPTS
C        GET SCREEN COORDINATES AND CHECK IF TIEPOINT IS DISPLAYED
      X=PTS(4,IP)-SS1+1
      X=X*ZOOM1
      IF(X.LT.6.0.OR.X.GT.NSDS-7) GO TO 55
      Y=PTS(3,IP)-SL1+1
      Y=Y*ZOOM1
      IF(Y.LT.6.0.OR.Y.GT.NLDS-6) GO TO 55
C        CALCULATE DISPLACEMENT
      DY=(PTS(1,IP)-PTS(3,IP)-LINOFF)*ZOOM1
      DX=(PTS(2,IP)-PTS(4,IP)-SAMOFF)*ZOOM1
C        DRAW DISPLACEMENT
	PX(1) = X
	PX(2) = X + DX
	PY(1) = Y
	PY(2) = Y + DY
	XST = XDIPOLYLINE(IDEV,G1,INT2BYTE(GDN),2,PX,PY)
   55 CONTINUE
C
	RETURN
	END
c
c**************************************************************
	SUBROUTINE GTPAIR(G1,T1,T2,NTB)
C-----THIS ROUTINE WILL ACQUIRE A PAIR OF TIEPOINTS
C-----AND STORE THEM IN THE PTS ARRAY

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

	COMMON/CX/IPARM(90),PTS,IPT
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	INTEGER STEP
	INTEGER IDEV,T1,T2,G1
	REAL PTS(4,2000)
        INTEGER ITEST(2)
C
	STEP=1          !100 PIXELS
	IF(IREDO .NE. 0) GO TO 70
	NPTS=NPTS+1
      IF(NPTS.GT.1995) CALL XVMESSAGE('TIEPOINT BUFFER LIMIT IS 2000',
     .                                ' ')
      IPT=NPTS
C        READ CURSOR COORDINATES  (LEFT SIDE)
70    IF(NTB .EQ. 0) CALL KEY(T1,STEP)
	CALL RCURSE(IDEV,T1,ILINE,ISAMP)
      ILINE=(ILINE-1)/ZOOM1+1
      ISAMP=(ISAMP-1)/ZOOM1+1
      PTS(3,IPT)=SL1+ILINE-1
      PTS(4,IPT)=SS1+ISAMP-1

         CALL XVIPARM( 'POSITION', ITEST, ICOUNT, IDEF, 2 )
         IF ( IDEF .NE. 1 )  THEN          ! GET CURSOR POSITION FROM PARAMETER
              PTS(3,IPT) = ITEST(1)            ! IF USING 'POSITION'.
              PTS(4,IPT) = ITEST(2)
         END IF

      CALL DRAWGR(IPT,G1,1)
      IF(T1.NE.T2) GO TO 75
      IF(IPT.LT.2) GO TO 74
C        FOR ONE CURSOR, MOVE TO RIGHT SIDE USING LAST POINT AS PREDICT
      X=PTS(2,IPT-1)-PTS(4,IPT-1)+PTS(4,IPT)-SS2+1
      IX=X*ZOOM2 + NSDS + IOFF
      Y=PTS(1,IPT-1)-PTS(3,IPT-1)+PTS(3,IPT)-SL2+1
      IY=Y*ZOOM2
      IF(IX.LT.NSDS.OR.IX.GT.NSDS*2.OR.IY.LT.1.OR.IY.GT.NLDS) GO TO 74
      CALL WCURSE(IDEV,T2,IY,IX)
	STEP = 3     !1 PIXEL
74    CONTINUE

      CALL XVMESSAGE('Position cursor on right side and press <RETURN>',
     .               ' ')
      CALL XVINTRACT( 'READY', '  <RETURN' )

C        READ CURSOR POSITION  (RIGHT SIDE)
75    IF(NTB .EQ. 0) CALL KEY(T2,STEP)
	CALL RCURSE(IDEV,T2,ILINE,ISAMP)

      ILINE=(ILINE-1)/ZOOM2+1
      ISAMP=(ISAMP-NSDS-IOFF-1)/ZOOM2+1
      PTS(1,IPT)=SL2+ILINE-1
      PTS(2,IPT)=SS2+ISAMP-1

         CALL XVIPARM( 'POSITION', ITEST, ICOUNT, IDEF, 2 )
         IF ( IDEF .NE. 1 )  THEN          ! GET CURSOR POSITION FROM PARAMETER
              PTS(1,IPT) = ITEST(1)            ! IF USING 'POSITION'.
              PTS(2,IPT) = ITEST(2)
         END IF

C        FOR ONE CURSOR, MOVE BACK TO LEFT SIDE
      IF(T1.NE.T2) RETURN
      IX=(PTS(4,IPT)-SS1+1)*ZOOM1+.5
      IY=(PTS(3,IPT)-SL1+1)*ZOOM1+.5
      IF(IX.LT.1.OR.IX.GT.NSDS.OR.IY.LT.1.OR.IY.GT.NLDS) RETURN
      CALL WCURSE(IDEV,T1,IY,IX)
C
	RETURN
	END
c
c**************************************************************
	SUBROUTINE FORMT(COEF,A,B,RMAG,CL,CX,V,EX,E,Z1,Z2,Z3,R1,
     1                    PAR,OUTFILE)
	COMMON/CX/IPARM(90),PTS,jdum

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

	COMMON/C6/FIL5(3),NUM,idum(4)
	REAL PTS(4,2000),COEF(20),CX(2000,10),CL(2000)
	REAL V(2000),EX(10),R1(4224)
	DOUBLE PRECISION Z1(10,10),Z2(10,10),Z3(10,10)
	INTEGER PAR(8192)
	CHARACTER*256 OUTFILE
	CHARACTER*5 GPGM(3)
        CHARACTER*12 GPGMNAME
        CHARACTER*22 GMSG
        DATA GPGM/'GEOMA','LGEOM','MGEOM'/
        DATA GMSG/'TIEPOINTS FOR PROGRAM '/
C
700	CALL XVMESSAGE('SPECIFY GEOM PROGRAM YOU WILL BE USING',' ')
	CALL XVMESSAGE('ENTER GEOMA, LGEOM, OR MGEOM:',' ')
C
	CALL XVMESSAGE('AT PROMPT, ENTER `GEOMA, `LGEOM, OR `MGEOM',' ')
	CALL XVINTRACT('GEOM','GEOM PROGRAM?')

        CALL XVIPARM( 'PGM', GPGMNAME, ICOUNT, IDEF , 0)

	IF( GPGMNAME .EQ. 'GEOMA') IDGEOM = 1
	IF( GPGMNAME .EQ. 'LGEOM') IDGEOM = 2
	IF( GPGMNAME .EQ. 'MGEOM') IDGEOM = 3
C
C  FIND RANGE OF TIEPOINTS
705   RMINL=PTS(1,1)
      RMAXL=PTS(1,1)
      RMINS=PTS(2,1)
      RMAXS=PTS(2,1)
      DO 710 L=2,NPTS
      IF(RMINL.GT.PTS(1,L)) RMINL=PTS(1,L)
      IF(RMAXL.LT.PTS(1,L)) RMAXL=PTS(1,L)
      IF(RMINS.GT.PTS(2,L)) RMINS=PTS(2,L)
      IF(RMAXS.LT.PTS(2,L)) RMAXS=PTS(2,L)
  710 CONTINUE
      IF(MINL.EQ.0) MINL=RMINL-50
      IF(MINL.LT.1) MINL=1
      IF(MINS.EQ.0) MINS=RMINS-50
      IF(MINS.LT.1) MINS=1
      IF(MAXL.EQ.0) MAXL=RMAXL+50
      IF(MAXL.GT.NLI2) MAXL=NLI2
      IF(MAXS.EQ.0) MAXS=RMAXS+50
      IF(MAXS.GT.NSI2) MAXS=NSI2
C
C        REINTERPOLATE RANDOM TIEPOINTS
      IF(IFORM.LE.5) CALL RECOM(IFORM,MINL,MINS,MAXL,MAXS,COEF,
     &                          A,B,RMAG,NHOR,NVER,PAR)
      IF(IFORM.EQ.6.OR.IFORM.EQ.7) CALL RECO67(COEF,MINL,MINS,MAXL,
     &                                 MAXS,IFORM,NHOR,NVER,PAR)
      IF(IFORM.EQ.8) CALL SORTFT(NUSE,NPTS,MAXS,MINS,MAXL,MINL,NHOR,
     &                NVER,PTS,NUM,PAR,CL,CX,V,EX,COEF,E,Z1,Z2,Z3)
C
      IF(IFORM .EQ. 9) THEN           !TIECONM
              CALL MVE(7,4*NPTS,PTS,PAR(24), 1,1)

C------CREATE GEOM PARAMETER FILE FOR OUTPUT

              NPAR = 6
              NENTRY = NPTS*4
              MAX_PARM_SIZE = NENTRY*4
C
              CALL XVPOPEN(IST,NPAR,MAX_PARM_SIZE,OUTFILE,'SA',IPUNIT)
              CALL XLADD( IPUNIT, 'HISTORY', 'COMMENT', 
     &         GMSG//GPGM(IDGEOM),IST, 'FORMAT', 'STRING',' ')
              CALL XVSIGNAL( IPUNIT, IST, 1)

              CALL XVPOUT(IST,'MINL', FLOAT(MINL),'REAL',1)
              CALL XVPOUT(IST,'MINS',FLOAT( MINS),'REAL',1)
              CALL XVPOUT(IST,'MAXL', FLOAT(MAXL),'REAL',1)
              CALL XVPOUT(IST,'MAXS',FLOAT( MAXS),'REAL',1)
              CALL XVPOUT(IST,'MODE', GPGM(IDGEOM),'STRING',1)

              CALL XVPOUT(IST,'TIEPOINT',PAR(24),'REAL',NENTRY)
              CALL XVPCLOSE( IST)

      ELSE
C
       IF(IDGEOM .EQ. 1) THEN                   !GEOMA
  	CALL MVE(7,NHOR*NVER*4,PAR(10),R1,1,1)
        CALL TYTRI(NHOR-1,NVER-1,R1,PAR(10),NWORDS)  !MAKE TRIANGLES
        NHOR = NHOR*2 - 1           ! NUMBER OF HORIZONTAL REGIONS DOUBLED.
       END IF
C
C------CREATE GEOM PARAMETER FILE FOR OUTPUT
       NPAR = 3
       NENTRY = NHOR*NVER*4
       MAX_PARM_SIZE = NENTRY*4
C
       CALL XVPOPEN(IST,NPAR,MAX_PARM_SIZE,OUTFILE, 'SA', IPUNIT)
       CALL XLADD( IPUNIT, 'HISTORY', 'COMMENT', GMSG//GPGM(IDGEOM),
     & IST, 'FORMAT', 'STRING',' ')
       CALL XVSIGNAL( IPUNIT, IST, 1)
       CALL XVPOUT(IST,'NAH',NHOR-1,'INT',1)
       CALL XVPOUT(IST,'NAV',NVER-1,'INT',1)
       CALL XVPOUT(IST,'TIEPOINT',PAR(10),'REAL',NENTRY)
       CALL XVPCLOSE( IST)
      END IF
C
	RETURN
	END
c
c**************************************************************
	SUBROUTINE DISPX(V1,C,N,SCALE,LINE,SAMP,SCREEN)
	IMPLICIT INTEGER (A-Z)
        include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.


      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM


	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	INTEGER SS,NUMI2,V1,IDEV,D,LE,RI,TO,BO
	REAL SCALE,C(N,N)
	LOGICAL XST,XDILINEWRITE
	logical XDIAWLOCATION,XDIAWSET
	BYTE BUF(64)
C-----------------------------------------------------------------
	XST = XDIAWLOCATION(IDEV,V1,LE,TO,RI,BO)  ! GET AW TO SAVE
	SS = SCREEN/2 - 31
	NUMI2 = N
	XST = XDIAWSET(IDEV,V1,1,1,SCREEN,NUMI2) !SET AW
C
      DO 130 L=1,N
      L2 = L
      DO 129 I=1,N
      D  = C(I,L) * SCALE
      IF(D .LT. 0 ) D = 0
	BUF(I) = INT2BYTE(D)
  129 CONTINUE
      IF(L .EQ. LINE .AND. IPHAS .EQ. 0) BUF(SAMP) = 0     ! MARK CENTRAL PEAK
      XST = XDILINEWRITE(IDEV,V1,ss,L2,NUMI2,BUF)
  130 CONTINUE
C------------------------------------------------------------
	XST = XDIAWSET(IDEV,V1,LE,TO,RI,BO)   ! RESET AW AS BEFORE
	RETURN
	END
c
c**************************************************************
	SUBROUTINE USELUT(LUTDISP,LUT)
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	LOGICAL XST,XDLWRITE
	INTEGER IDEV,LUTDISP,LUT(256)
C
	if(lutdisp .eq. 0) then
		XST = XDLWRITE(IDEV,1,1,LUT)
		XST = XDLWRITE(IDEV,2,1,LUT)
		XST = XDLWRITE(IDEV,3,1,LUT)
	else	
		XST = XDLWRITE(IDEV,LUTDISP,1,LUT)
	endif
	RETURN
	END
c
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
c
c**************************************************************
	subroutine wcurse(idev,cur,iline,isamp)
	implicit integer (a-z)
	integer iline,isamp
	logical xst,xdcset
c
	x = isamp
	y = iline
	xst = xdcset(idev,cur,x,y)
	if( .not. xst) call XVMESSAGE('xdcset error',' ')
	return
	end
c
c**************************************************************
	SUBROUTINE REPLACE(G1,ISIDE)
C-------THIS ROUTINE WILL ERASE THE OLD GRAPHICS AND REPLACE
C-------THE MARKS IN THE APROPRIATE PLACES.
	IMPLICIT INTEGER (A-W), LOGICAL (X)
	COMMON/CD/IDEV,L(2),T(2),R(2),B(2),KDUM(10),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	INTEGER I

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

C-------SET THE ACCESS WINDOW TO THE APPROPRIATE SIDE.
	XST = XDIAWSET(IDEV,G1,L(ISIDE),T(ISIDE),R(ISIDE),B(ISIDE))
C-------ERASE ACCESS WINDOW
	XST = XDIFILL(IDEV,G1,0)
C
	DO 10 I=1,NPTS
	CALL DRAWGR(I,G1,ISIDE)
10	CONTINUE
C
C-------RESET THE ACCESS WINDOW TO THE ENTIRE SCREEN
	XST = XDIAWSET(IDEV,G1,L(1),T(1),R(2),B(1))
	RETURN
	END
c
c**************************************************************
      SUBROUTINE OPT67(NPTS,CL,PTS,CX,COEF,V,E,EX,RMAX,LMAX,
     &                 IFIT,E1,Z1,Z2,Z3,IPRINT)
C
      REAL CL(*),PTS(4,2000),CX(2000,10),COEF(*),V(*),EX(*)
      DOUBLE PRECISION Z1(10,10),Z2(10,10),Z3(10,10)
C
      NE=NPTS
      NU=10
      IF(IFIT.EQ.6) NU=6
C
      DO 110 L=1,NE
      CL(L)=PTS(3,L)
      CX(L,1)=1.0
      CX(L,2)=PTS(1,L)
      CX(L,3)=PTS(2,L)
      CX(L,4)=PTS(1,L)**2
      CX(L,5)=PTS(2,L)**2
      CX(L,6)=PTS(1,L)*PTS(2,L)
        IF(IFIT.EQ.6) GO TO 110
      CX(L,7)=PTS(1,L)*PTS(2,L)**2
      CX(L,8)=PTS(2,L)*PTS(1,L)**2
      CX(L,9)=PTS(1,L)**3
      CX(L,10)=PTS(2,L)**3
  110 CONTINUE
C
      CALL LSQP(NE,NU,CX,CL,COEF(1),V,E,EX,Z1,Z2,Z3)
      E1=E
C        FIND MAXIMUM DISCREPANCY
      RMAX=ABS(V(1))
      LMAX=1
      DO 120 L=1,NPTS
      IF(RMAX.GT.ABS(V(L))) GO TO 120
      RMAX=ABS(V(L))
      LMAX=L
  120 CONTINUE
C       PRINT FIT INFORMATION
      IF(IPRINT.EQ.0) GO TO 200
      IF(IFIT.EQ.6) CALL XVMESSAGE('OLD=NEW Y=J+IY+HX+GY**2+FX**2+EXY',
     .               ' ')
      IF(IFIT.EQ.7) CALL XVMESSAGE(
     &'OLD=NEW Y=J+IY+HX+GY**2+FX**2+EXY+DYX**2+CXY**2+BY**3+AX**3',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('Y EQUATION UNKNOWNS',' ')
      CALL PRNT(7,NU,COEF(1),'.')
      CALL PRNT(7,NU,EX,'MEAN ERROR OF THE UNKNOWNS.')
      CALL PRNT(7,1,E,'MEAN ERROR OF THE UNIT WEIGHT.')
      CALL PRNT(7,NE,V,'RESIDUALS OBSERVED-COMPUTED.')
C
C        FIT SURFACE TO SAMPLE VALUES
200   DO 210 L=1,NE
      CL(L)=PTS(4,L)
  210 CONTINUE
      CALL LSQP(NE,NU,CX,CL,COEF(11),V,E,EX,Z1,Z2,Z3)
C  SEARCH FOR MAX DISCREPANCY
      DO 321 L=1,NE
      IF(RMAX.GT.ABS(V(L))) GO TO 321
      RMAX=ABS(V(L))
      LMAX=L
  321 CONTINUE
C
      IF(IPRINT.EQ.0) GO TO 332
      IF(IFIT.EQ.6) CALL XVMESSAGE(
     &   'OLD=NEW X=J+IY+HX+GY**2+FX**2+EXY',' ')
      IF(IFIT.EQ.7) CALL XVMESSAGE(
     &'OLD=NEW X=J+IY+HX+GY**2+FX**2+EXY+DYX**2+CXY**2+BY**3+AX**3',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('X EQUATION UNKNOWNS',' ')
      CALL PRNT(7,NU,COEF(11),'.')
      CALL PRNT(7,NU,EX,'MEAN ERROR OF THE UNKNOWNS.')
      CALL PRNT(7,1,E,'MEAN ERROR OF THE UNIT WEIGHT.')
      CALL PRNT(7,NE,V,'RESIDUALS OBSERVED-COMPUTED.')
332   RETURN
      END
C
c
c**************************************************************
      SUBROUTINE RECO67(COEF,MINL,MINS,MAXL,MAXS,
     &                  IFIT,NHOR,NVER,RAR)
C
      REAL COEF(*),RAR(*)
C
      NL2INT=(MAXL-MINL)/(NVER-1)
      NS2INT=(MAXS-MINS)/(NHOR-1)
      LL=MINL-NL2INT
      IPTR=6
      DO 252 L=1,NVER
      JJ=MINS-NS2INT
      LL=LL+NL2INT
      DO 253 J=1,NHOR
      JJ=JJ+NS2INT
      IPTR=IPTR+4
      RAR(IPTR)=LL
      RAR(IPTR+1)=JJ
      RL=COEF(1)+COEF(2)*LL+COEF(3)*JJ+COEF(4)*LL**2+COEF(5)*JJ**2
     *     +COEF(6)*LL*JJ
      RS=COEF(11)+COEF(12)*LL+COEF(13)*JJ+COEF(14)*LL**2+COEF(15)*JJ**2
     *     +COEF(16)*LL*JJ
      IF(IFIT.EQ.6) GO TO 102
      RL=RL+COEF(7)*LL*FLOAT(JJ)**2+COEF(8)*JJ*FLOAT(LL)**2+
     *   COEF(9)*FLOAT(LL)**3+COEF(10)*FLOAT(JJ)**3
      RS=RS+COEF(17)*LL*FLOAT(JJ)**2+COEF(18)*JJ*FLOAT(LL)**2+
     *     COEF(19)*FLOAT(LL)**3+COEF(20)*FLOAT(JJ)**3
102   RAR(IPTR+2)=RL
      RAR(IPTR+3)=RS
253   CONTINUE
252   CONTINUE
      RETURN
      END
C
c
c**************************************************************
      SUBROUTINE SORTFT(NUSE,NPTS,MAXS,MINS,MAXL,MINL,NHOR,NVER,PTS,
     &                  NUM,RAR,CL,CX,V,EX,COEF,E,Z1,Z2,Z3)
C
      DOUBLE PRECISION Z1(10,10),Z2(10,10),Z3(10,10)
      REAL PTS(4,2000),R(10),RAR(*),CL(*),CX(2000,10),V(*)
      REAL EX(*),COEF(*)
      INTEGER LOC(10)
C
      NUM=NUSE
      IF(NUM.GT.NPTS) NUM=4
      DELX=(MAXS-MINS)/(NHOR-1.)
      DELY=(MAXL-MINL)/(NVER-1.)
      INCPAR=6
      Y=MINL-DELY
      NUMX=NUM+1
C
      DO 200 L=1,NVER
      Y=Y+DELY
      X=MINS-DELX
      DO 560 I=1,NHOR
      INCPAR=INCPAR+4
      X=X+DELX
C  COMPUTE ANY NUM VECTOR MAGNITUDES
      DO 300 J=1,NUM
      R(J)=(PTS(1,J)-Y)**2+(PTS(2,J)-X)**2
  300 LOC(J)=J
      RMAX=R(1)
      IPOS=1
C  GET MIN AND MAX MAGNITUDE OF NUM VECTORS
      DO 510 J=2,NUM
      IF(RMAX.GT.R(J)) GO TO 510
      IPOS=J
      RMAX=R(J)
510   CONTINUE
      IF(NUM.EQ.NPTS) GO TO 511
C  LOCATE NUM CLOSEST TIEPOINTS
      DO 400 K=NUMX,NPTS
      RR=(PTS(1,K)-Y)**2+(PTS(2,K)-X)**2
      IF(RR.GT.RMAX) GO TO 400
      R(IPOS)=RR
      LOC(IPOS)=K
      RMAX=R(1)
      IPOS=1
      DO 520 J=2,NUM
      IF(RMAX.GT.R(J)) GO TO 520
      IPOS=J
      RMAX=R(J)
520   CONTINUE
400   CONTINUE
C  LEAST SQUARES FIT
511   DO 522 K=1,NUM
      CL(K)=PTS(3,LOC(K))
      CX(K,1)=PTS(2,LOC(K))
      CX(K,2)=PTS(1,LOC(K))
522   CX(K,3)=1.0
C  Y EQUATION   Y=AX+BY+C
      CALL LSQP(NUM,3,CX,CL,COEF(1),V,E,EX,Z1,Z2,Z3)
      DO 521 K=1,NUM
521   CL(K)=PTS(4,LOC(K))
C  X EQUATION   X=AX+BY+C
      CALL LSQP(NUM,3,CX,CL,COEF(5),V,E,EX,Z1,Z2,Z3)
C  COMPUTE LEFT TIEPOINT FROM COEFFICIENTS
      RAR(INCPAR)=Y
      RAR(INCPAR+1)=X
      RAR(INCPAR+2)=COEF(1)*X+COEF(2)*Y+COEF(3)
      RAR(INCPAR+3)=COEF(5)*X+COEF(6)*Y+COEF(7)
560   CONTINUE
200   CONTINUE
      RETURN
      END
C
c
c**************************************************************
      SUBROUTINE RECOM(IFIT,MINL,MINS,MAXL,MAXS,COEF,
     &                 A,B,RMAG,NHOR,NVER,RAR)
C
      REAL COEF(*),RAR(*)
C
      NHOR=2
      NVER=2
      NL2INT=MAXL-MINL
      NS2INT=MAXS-MINS
      IPTR=6
      IF(IFIT.NE.3) GO TO 345
C
C        OPTION 3
      DO 252 L=MINL,MAXL,NL2INT
      DO 253 J=MINS,MAXS,NS2INT
      IPTR=IPTR+4
      RAR(IPTR)=L
      RAR(IPTR+1)=J
      RAR(IPTR+2)=L*COEF(1)+J*COEF(2)+COEF(3)
253   RAR(IPTR+3)=L*COEF(5)+J*COEF(6)+COEF(7)
252   CONTINUE
      RETURN
C
C        OPTIONS 1, 2, 4, 5
345   DO 346 L=MINL,MAXL,NL2INT
      DO 347 J=MINS,MAXS,NS2INT
      IPTR=IPTR+4
      RAR(IPTR)=L
      RAR(IPTR+1)=J
      RAR(IPTR+3)=(A*J-B*L)*RMAG+COEF(3)
347   RAR(IPTR+2)=(B*J+A*L)*RMAG+COEF(4)
346   CONTINUE
      RETURN
      END
C
c
c**************************************************************
      SUBROUTINE OPT3(NPTS,CL,PTS,CX,COEF,V,E,EX,C2,RMAX,LMAX,
     &            E1,Z1,Z2,Z3,IPRINT)
C
      DOUBLE PRECISION Z1(10,10),Z2(10,10),Z3(10,10)
      REAL CL(*),PTS(4,2000),C2(*),CX(2000,10),COEF(*),V(*),EX(*)
C
      NE=NPTS
      NU=3
C
C        FIT SURFACE TO OUTPUT LINE VALUE
      DO 110 L=1,NPTS
      CL(L)=PTS(3,L)
      CX(L,1)=PTS(1,L)
      CX(L,2)=PTS(2,L)
      CX(L,3)=1.0
  110 CONTINUE
      CALL LSQP(NE,NU,CX,CL,COEF(1),V,E,EX,Z1,Z2,Z3)
      E1=E
C        FIND MAXIMUM DISCREPANCY
      RMAX=ABS(V(1))
      LMAX=1
      DO 120 L=1,NPTS
      IF(RMAX.GT.ABS(V(L))) GO TO 120
      RMAX=ABS(V(L))
      LMAX=L
  120 CONTINUE
C        PRINT FIT INFORMATION
      IF(IPRINT.EQ.0) GO TO 200
      CALL XVMESSAGE('OLD=NEW  Y=AY+BX+D    ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('Y EQUATION UNKNOWNS',' ')
      CALL PRNT(7,3,COEF(1),'.')
      CALL PRNT(7,3,EX,'MEAN ERRORS OF THE UNKNOWNS.')
      CALL PRNT(7,1,E,'MEAN ERROR OF THE UNIT WEIGHT.')
      CALL PRNT(7,NPTS,V,'RESIDUALS OBSERVED - COMPUTED.')
C
C        FIT SURFACE TO OUTPUT SAMPLE VALUE
200   DO 210 L=1,NPTS
      CL(L)=PTS(4,L)
  210 CONTINUE
      CALL LSQP(NE,NU,CX,CL,COEF(5),V,E,EX,Z1,Z2,Z3)
C        FIND MAXIMUM DISCREPANCY OVERALL
      DO 220 L=1,NPTS
      IF(RMAX.GT.ABS(V(L))) GO TO 220
      RMAX=ABS(V(L))
      LMAX=L
  220 CONTINUE
C        PRINT FIT INFORMATION
      IF(IPRINT.EQ.0) RETURN
      CALL XVMESSAGE('OLD=NEW  X=EY+FX+H    ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('X EQUATION UNKNOWNS',' ')
      CALL PRNT(7,3,COEF(5),'.')
      CALL PRNT(7,3,EX,'MEAN ERRORS OF THE UNKNOWNS.')
      CALL PRNT(7,1,E,'MEAN ERROR OF THE UNIT WEIGHT.')
      CALL PRNT(7,NPTS,V,'RESIDUALS OBSERVED - COMPUTED.')
C
      RETURN
      END
C
c
c**************************************************************
      SUBROUTINE OPT12(NPTS,CL,PTS,CX,COEF,V,E,EX,B,A,RMAG,RMAX,
     &                 LMAX,IFIT,Z1,Z2,Z3,IPRINT,C2)
C
      DOUBLE PRECISION Z1(10,10),Z2(10,10),Z3(10,10)
      REAL CL(*),PTS(4,2000),CX(2000,10),COEF(*),V(*),EX(*),C2(*)
C
      NE=NPTS
      NU=3
C
      DO 10 L=1,NPTS
      CL(L)=PTS(3,L)-PTS(4,L)
      CX(L,1)=PTS(2,L)
      CX(L,2)=PTS(1,L)
      CX(L,3)=1.0
   10 CONTINUE
C
      CALL LSQP(NE,NU,CX,CL,COEF,V,E,EX,Z1,Z2,Z3)
      IF(ABS(COEF(2)-COEF(1)).LT.1.0E-10) GO TO 100
      A=((COEF(1)+COEF(2))/(COEF(2)-COEF(1)))**2+1.0
      A=1.0/SQRT(A)
      B=SQRT(1.0-A*A)
      GO TO 200
C
100   B=(((COEF(2)-COEF(1))/(COEF(1)+COEF(2)))**2+1.0)
      B=1./SQRT(B)
      A=SQRT(1.0-B*B)
C
C  GET SIGN OF A AND B
200   IF(COEF(2)+COEF(1).LT.0.0) B=-B
      IF(COEF(2)-COEF(1).LT.0.0) A=-A
      RMAG=(COEF(2)/(A+B))
C  SET CONSTRAINTS
      IF(IFIT.EQ.4.OR.IFIT.EQ.5) A=1.0
      IF(IFIT.EQ.4.OR.IFIT.EQ.5) B=0.0
      IF(IFIT.EQ.5) RMAG=1.0
      IF(IFIT.EQ.1) RMAG=1.0
C  EVALUATE OFFSET SEPARATELY
      SUMY=0.0
      SUMX=0.0
      DO 342 L=1,NPTS
      SUMX=PTS(4,L)-RMAG*(PTS(2,L)*A-PTS(1,L)*B) + SUMX
      SUMY=PTS(3,L)-RMAG*(PTS(2,L)*B+PTS(1,L)*A)  + SUMY
  342 CONTINUE
      COEF(3)=SUMX/NPTS
      COEF(4)=SUMY/NPTS
C  DETERMINE THE LARGEST ERROR
      RMAX=0.0
      E=0.0
      LMAX=1
      DO 343 L=1,NPTS
      SUMX=(A*PTS(2,L)-B*PTS(1,L))*RMAG+COEF(3)
      SUMY=(B*PTS(2,L)+A*PTS(1,L))*RMAG+COEF(4)
      E1=SQRT((SUMX-PTS(4,L))**2+(SUMY-PTS(3,L))**2)
      E=E+E1
      IF(E1.LT.RMAX) GO TO 343
      RMAX=E1
      LMAX=L
343   CONTINUE
      E=E/NPTS
C
      IF(IPRINT.EQ.0) GO TO 350
C        PRINT FIT INFORMATION
      CALL XVMESSAGE('OLD=NEW X=(AX-BY)MAG+CX',' ')
      CALL XVMESSAGE('OLD=NEW Y=(BX+AY)MAG+CY',' ')
      CALL PRNT(7,1,A,'A COEFFICIENT ( COSINE ).')
      CALL PRNT(7,1,B,'B COEFFICIENT ( SINE ).')
      CALL PRNT(7,1,RMAG,'MAG COEFFICIENT (MAGNIFICATION).')
      CALL PRNT(7,1,COEF(3),'CX COEFFICIENT (X OFFSET).')
      CALL PRNT(7,1,COEF(4),'CY COEFFICIENT (Y OFFSET).')
      CALL PRNT(7,1,E,'MEAN ERROR.')
350   RETURN
      END
C
c
c**************************************************************
      SUBROUTINE XCOR2(PTS,ILIN,J,IND,IN,P1,P2,NUM,NUM2,
     1                IPASS,ISAM,SCALE,IN1,IN2,INTER0)
C  85-7-30 ...LWK...  MODIFIED CENTROID-FINDING ALGORITHM FOR CASE OF
C                    NEGATIVE CROSS-CORR. MATRIX.
C  86-7-14 ...SP.... CENTROID METHOD WAS NOT WORKING SINCE OFFSET
C                    WAS BEING ADDED INSTEAD OF SUBTRACTED, AND
C                    SINCE ISAM WAS THE LINE OFFSET AND ILIN WAS THE SAMPLE
C                    OFFSET.  I COULD NOT STAND IT, AND WENT THROUGH THE CODE
C                    MAKING ISAM THE SAMPLE OFFSET AND ILIN THE LINE OFFSET.
C                    WE STILL NEED TO SUBTRACT INSTEAD OF ADD.

      COMMON/C2/ SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,
     .      ZOOM1,ZOOM2,IOFF,CFORM
      INTEGER  SL1,SL2,SS1,SS2,NLI1,NLI2,NSI1,NSI2,NLDS,NSDS,
     .      NPTS,IREDO,NLW,CONV,MODE,IPHAS,IHPF,INTER,NHOR,NVER,
     .      IZOOM1,IZOOM2,IFIT,IEXIT,ISTRE,ISHOW,
     .      IDISP1,IDISP2,IFORM,NUSE,MINL,MINS,MAXL,MAXS,IOFF
      REAL     ZOOM1,ZOOM2
      INTEGER  CFORM

      COMMON/FT/ NX
      COMMON/C6/ LCEN,ICEN,IPOW,NM,ISIGN,idum(3)
      DOUBLE PRECISION CENXN,CENYN
      REAL PTS(4,2000),P1(NUM,NUM2),P2(NUM,NUM2)
      INTEGER NX(28)
      BYTE   IN(*)
C
      NYQNUM=NUM/2+1
      ISIGN=-1
      LCEN=PTS(3,J) +0.5
      ICEN=PTS(4,J)  +0.5
      CALL FFT2R(1,IN1,IN,P1,NUM,NUM2,IND)
      IF(IND .EQ. 1) THEN
		NPTS=NPTS-1
		RETURN
      END IF

C-----ZERO OUT DC
      P1(1,1)= 0.0
      P1(1,2)= 0.0

      IF (IHPF.EQ.1) THEN                         !ZERO OUT AXES
	DO L=3,NYQNUM
	  P1(1,L) = 0.0				!ZERO Y-AXIS
	ENDDO
	CALL MVE(7,NUM-1,0.0,P1(2,1),0,1)     !ZERO OUT X-AXIS REALS
	CALL MVE(7,NUM-1,0.0,P1(2,2),0,1)     !ZERO OUT X-AXIS IMAGS
      END IF

      IF (IPHAS.EQ.1) THEN		!GET FT PHASE
	DO L=1,NYQNUM
	  LL = 2*L
	  DO I=1,NUM
	    XMAG = SQRT( P1(I,LL-1)**2 + P1(I,LL)**2 )
	    IF (XMAG.EQ.0.) THEN
	      P1(I,LL-1) = 0.
	      P1(I,LL) = 0.
	    ELSE
	      P1(I,LL-1) = P1(I,LL-1)/XMAG
	      P1(I,LL) = P1(I,LL)/XMAG
	    ENDIF
	  ENDDO
	ENDDO
      ENDIF

      LCEN=PTS(1,J) +0.5
      ICEN=PTS(2,J) +0.5

      CALL FFT2R(2,IN2,IN,P2,NUM,NUM2,IND)      !TRANSFORM RIGHT PIC
      IF(IND .EQ. 1) THEN
	NPTS=NPTS-1
      	RETURN
      END IF

      IF (IPHAS.EQ.1) THEN		!GET FT PHASE
	DO L=1,NYQNUM
	  LL = 2*L
	  DO I=1,NUM
	    XMAG = SQRT( P2(I,LL-1)**2 + P2(I,LL)**2 )
	    IF (XMAG.EQ.0.) THEN
	      P2(I,LL-1) = 0.
	      P2(I,LL) = 0.
	    ELSE
	      P2(I,LL-1) = P2(I,LL-1)/XMAG
	      P2(I,LL) = P2(I,LL)/XMAG
	    ENDIF
	  ENDDO
	ENDDO
      ENDIF

      CALL CMULX(NUM,NUM2,P1,P2)      !MULTIPLY TRANSFORMS INTO P2

      ISIGN=+1
      CALL FFT2R(1,IN1,IN,P2,NUM,NUM2,IND)
      IF(IND .EQ. 1) THEN
		NPTS=NPTS-1
		RETURN
      END IF
C
C  REARRANGE TRANSFORM
      CALL MVE(7,4096,P2,P1,1,1)
C  TOP LEFT QUADRANT
      N1=NUM/2
      N2=N1+1
      L1=N1
      DO 210 L=1,N1
      L1=L1+1
      I1=N1
      DO 211 I=1,N1
      I1=I1+1
211   P2(I1,L1)=P1(I,L)
210   CONTINUE
C  TOP RIGHT QUADRANT
      L1=0
      DO 212 L=N2,NUM
      L1=L1+1
      I1=N1
      DO 213 I=1,N1
      I1=I1+1
213   P2(I1,L1)=P1(I,L)
212   CONTINUE
C  LOWER LEFT QUADRANT
      L1=N1
      DO 214 L=1,N1
      L1=L1+1
      I1=0
      DO 215 I=N2,NUM
      I1=I1+1
215   P2(I1,L1)=P1(I,L)
214   CONTINUE
C  LOWER RIGHT QUADRANT
      L1=0
      DO 216 L=N2,NUM
      L1=L1+1
      I1=0
      DO 217 I=N2,NUM
      I1=I1+1
217   P2(I1,L1)=P1(I,L)
216   CONTINUE
C	DO 60 II=1,NUM
C60	CALL PRNT(7,NUM,P2(1,II),' P2(1,I).')
C  DETERMINE CENTER
      ILIN=N2
      ISAM=N2
      RMAX=P2(N2,N2)
      DO 218 L=1,NUM
      DO 219 I=1,NUM
      IF(P2(I,L).LT.RMAX) GO TO 219
      ILIN=L
      ISAM=I
      RMAX=P2(I,L)
219   CONTINUE
218   CONTINUE
      IF(RMAX.GT.1.0E-10) GO TO 630
      NPTS=NPTS-1
      CALL XVMESSAGE('NO CORRELATION',' ')
      IND=1
      RETURN
630   CONTINUE
      SCALE=255./RMAX
      IF(INTER0.EQ.0) GO TO 621
C
C  GET CENTROID OF CROSS CORRELATION MATRIX TO DO BETTER THAN 1 PIXEL
C
      CALL MVE(7,4096,P2,P1,1,1)	!FIRST SAVE P2
      RMARK = -1.E30			!"MARKER"
      P2(ISAM,ILIN)=RMARK
      CEND=0.
      CENXN=0.
      CENYN=0.
      CENXN=CENXN+RMAX*ISAM
      CEND=CEND+RMAX
      CENYN=CENYN+RMAX*ILIN
611   RMAX=-1.
C
C  FIND MAX VALUE
      DO 600 L=1,NUM
      DO 601 I=1,NUM
      IF(P2(I,L).LT.RMAX) GO TO 601
      ILIN=L
      ISAM=I
      RMAX=P2(I,L)
601   CONTINUE
600   CONTINUE
      IF (RMAX .LE. 1.0E-10)  GOTO 610      ! USE ONLY POSITIVE VALUES.
C
C  SEE IF AN ADJACENT POINT HAS BEEN MARKED
      IF(ILIN.EQ.1.OR.ILIN.EQ.NUM.OR.ISAM.EQ.1.OR.ISAM.EQ.NUM) GO TO 610
      IF(RMAX.LE.RMARK) GO TO 610

      DO 605 L=ILIN-1,ILIN+1
      DO 606 I=ISAM-1,ISAM+1
      IF(P2(I,L).LE.RMARK) GO TO 635
606   CONTINUE
605   CONTINUE
      GO TO 610
635   CONTINUE
      P2(ISAM,ILIN) = RMARK
C
C  UPDATE CENTROID MOMENTS AND SUMS
      CENXN=CENXN+RMAX*ISAM
      CEND=CEND+RMAX
      CENYN=CENYN+RMAX*ILIN
      GO TO 611
610   CONTINUE
C
C  COMPUTE CENTROID CENTER
      CENXN=CENXN/CEND
      CENYN=CENYN/CEND
      PTS(1,J)=LCEN   + N2-CENYN
      PTS(2,J)=ICEN   + N2-CENXN
C
      CALL MVE(7,4096,P1,P2,1,1)	!RESTORE P2
C
C  CENTER FOR DISPLAY ONLY
      ILIN=CENYN+0.5
      ISAM=CENXN+0.5
      RETURN
C
C  SET RIGHT CURSOR - NO INTERPOLATION
621   CONTINUE
C      PTS(1,J)=LCEN-N2+ILIN
C      PTS(2,J)=ICEN-N2+ISAM
C-----AS IT TURNS OUT, (ISAM-N2) IS THE SHIFT NECESSARY IN THE 
C-----MINUS SAMPLE DIRECTION, AND (ILIN-N2) IS THE SHIFT NECESSARY
C-----IN THE MINUS LINE DIRECTION
      PTS(1,J)=LCEN + N2 - ILIN
      PTS(2,J)=ICEN + N2 - ISAM
      RETURN
      END
C
c
c**************************************************************
      SUBROUTINE HISTCN(NLW,HISTH,HISTV,PTS,NS1,NS2,NL1,NL2,CONV,
     &                  IN1,IN2,J,IN)

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.
      INTEGER HISTH(*),HISTV(*),CONV
      REAL PTS(4,2000)
      BYTE IN(*)
C
      DO 35 L=1,NLW
35    HISTH(L)=0
      ISTART=PTS(4,J)-NLW/2
      LSTART=PTS(3,J)-NLW/2
      IEND=ISTART+NLW-1
      LEND=LSTART+NLW-1
      IF(ISTART.LT.1) ISTART=1
      IF(LSTART.LT.1) LSTART=1
      IF(IEND.GT.NS1) IEND=NS1
      IF(LEND.GT.NL1) LEND=NL1
      MHOR=IEND-ISTART+1
      MVER=LEND-LSTART+1
      LL=0
      DO 31 L=LSTART,LEND
      LL=LL+1
      CALL XVREAD( IN1, IN, IST, 'LINE', L, 'NSAMPS', NS1,' ')
      ISUM=0
      II=0
      DO 32 I=ISTART,IEND
      II=II+1
      ISUM=ISUM+BYTE2INT(IN(I))*CONV
32    HISTH(II)=HISTH(II)+ BYTE2INT(IN(I))*CONV
31    HISTV(LL)=ISUM

      call maxfcn(histv,mver,maxpt)
      pts(3,j)=maxpt+lstart-1
      call maxfcn(histh,mhor,maxpt)
      pts(4,j)=maxpt+istart-1

      DO 65 L=1,NLW
65    HISTH(L)=0
      ISTART=PTS(2,J)-NLW/2
      LSTART=PTS(1,J)-NLW/2
      IEND=ISTART+NLW-1
      LEND=LSTART+NLW-1
      IF(ISTART.LT.1) ISTART=1
      IF(LSTART.LT.1) LSTART=1
      IF(IEND.GT.NS2) IEND=NS2
      IF(LEND.GT.NL2) LEND=NL2
      MHOR=IEND-ISTART+1
      MVER=LEND-LSTART+1
      LL=0
      DO 61 L=LSTART,LEND
      LL=LL+1
      CALL XVREAD( IN2, IN, IST, 'LINE', L, 'NSAMPS', NS2,' ')
      ISUM=0
      II=0
      DO 62 I=ISTART,IEND
      II=II+1
      ISUM=ISUM+BYTE2INT(IN(I))*CONV
62    HISTH(II)=HISTH(II)+ BYTE2INT(IN(I))*CONV
61    HISTV(LL)=ISUM

      call maxfcn(histv,mver,maxpt)
      pts(1,j)=maxpt+lstart-1
      call maxfcn(histh,mhor,maxpt)
      pts(2,j)=maxpt+istart-1

      RETURN
      END

c**********************************************************************
      subroutine maxfcn(buf,n,maxpt)
      integer buf(*)
c returns in maxpt the location of the largest element in buf(1-n)
      maxpt=1
      m=buf(1)
      do i=2,n
        if(buf(i).gt.m)then
           maxpt=i
           m=buf(i)
        endif
      enddo
      return
      end

c**************************************************************
      SUBROUTINE LSQP(NE,NU,C,CL,X1,V,E,EX,A,R,Q)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C          V(I) = RESIDUALS  (I.E. OBSERVED MINUS COMPUTED)
C          E = MEAN ERROR OF THE UNIT WEIGHT
C          EX(J) = MEAN ERRORS OF THE UNKNOWNS
C
C4    THE DIMENSION STATEMENT BELOW PERMITS THE MAXIMUM VALUES NE=2000
C     AND NU=10;  IF THE DESIRED MAXIMA ARE DIFFERENT, ONLY THE
C     DIMENSION HAS TO BE CHANGED ACCORDINGLY.
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      DOUBLE PRECISION  A(10,10),AL(10),R(10,10),RL(10),Q(10,10),
     .                  X(10),SL,SQ,P,SUM
      REAL C(2000,10),CL(2000),X1(10),V(2000),EX(10)
C
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.0D0
      R(I,J)=0.0D0
57    Q(I,J)=0.0D0
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)
      DO 102 I=1,NU
      AL(I)=0.0D0
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      R(I,J)=A(I,J)/A(I,I)
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      RL(1)=AL(1)/A(1,1)
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
125   RL(I)=AL(I)/A(I,I)
       X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.0D0
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      Q(NU,NU)=1./A(NU,NU)
      DO 150 I=1,NUM
      NP=NUP-1
      DO 135 J=I,NUM
      NM=NU-J
      JP=NM+1
      P=0.0D0
      DO 135 K=JP,NU
      P=P-R(NM,K)*Q(NP,K)
      Q(NP,NM)=P
135   Q(NM,NP)=P
      NPM=NP-1
      SQ=0.0D0
      DO 145 L=NP,NU
145   SQ=SQ-R(NPM,L)*Q(L,NPM)
150   Q(NPM,NPM)=1./A(NPM,NPM)+SQ
      DO 151 I=1,NE
      V(I)=0.
      DO 151 J=1,NU
151   V(I)=V(I)+C(I,J)* X(J)
      SL=0.0D0
      DO 153 I=1,NE
      V(I)=CL(I)-V(I)
153   SL=SL+V(I)*V(I)
      FNE=NE
      FNU=NU
      E=DSQRT(SL/(FNE-FNU))
      DO 160 I=1,NU
        IF ( Q(I,I) .GE. 0.D0 ) THEN
          EX(I)=E*DSQRT(Q(I,I))
        ELSE
          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
        END IF
160   CONTINUE      
      RETURN
      END
C
c
c**************************************************************
      SUBROUTINE TYTRI(NAH,NAV,PTSIN,PTSOUT,N)
C
C  GENERAL PURPOSE GEOMA TIEPOINT PROGRAM
C  PURPOSE IS TO MAKE TRIANGLES OUT OF QUADRILATERALS
C  NAH        NO OF HORIZONTAL AREAS
C  NAV        NO OF VERTICAL AREAS
C  PTSIN      INPUT QUADRILATERAL BUFFER
C  PTSOUT     OUTPUT TRIANGLE TIEPOINTS BUFFER
C  N          NO OF FULL WORDS IN PTSOUT BUFFER
C
      REAL PTSIN(*),PTSOUT(*)
      INTEGER S
C
      NUMH=NAH+1
      NUMV=NAV+1
      N=0
      K=-3
C
      DO 10 L=1,NUMV
      IPOS=NUMH
      IF(L.EQ.(L/2)*2) IPOS=1
      NUM=2
      DO 20 S=1,NUMH
      IF(IPOS.EQ.S) NUM=1
      K=K+4
      DO 40 I=1,NUM
      DO 40 M=1,4
      N=N+1
40    PTSOUT(N)=PTSIN(K+M-1)
      NUM=2
20    CONTINUE
10    CONTINUE
      RETURN
      END
C
c
c**************************************************************
	SUBROUTINE DEVICE(V1,VL,VR,G1,T1,T2,LUT,LUTL,LUTR,NTB,FORM,*)
	IMPLICIT INTEGER (A-W,Y-Z),LOGICAL (X)
      include 'fortport'  ! DEFINES INT2BYTE 
	COMMON/CD/IDEV,FIL(8),SCREEN,MAX,KDUM(8),GDN
	INTEGER INFO(80),ALL(4),DEFSET(4)
  	INTEGER SCREEN,NTB,MAXTB,MAX
        INTEGER XDSVNL,XDSVNS, XDSNL, XDSNS
	REAL S
        DATA ALL/1,1,0,0/,DEFSET/0,0,0,0/
C==================================================================
	XST = XDEACTION( 2,2,3 )	!Define Error Action
	XST = XDDUNIT(IDEV)		!Get Unit Number
	XST = XDDOPEN(IDEV)		!OPEN DEVICE
	XST = XDDACTIVATE(IDEV,.TRUE.)	!ACTIVATE DEVICE
        XST = XDDCONFIGURE(IDEV,DEFSET)

	XST = XDDINFO(IDEV,1,80,INFO)

	MAXTB = MIN0(INFO(48),INFO(60))

        SCREEN = XDSVNL(IDEV)
        SCREEN = MIN0( SCREEN, XDSVNS(IDEV) )   
	CALL PRNT(4,1,SCREEN,'SCREEN SIZE =.')

        MAX = XDSNL(IDEV)
        MAX = MIN0( MAX, XDSNS(IDEV) )
	CALL PRNT(4,1,MAX,'IMAGE PLANE SIZE =.')

	VAL = 0
	XST = XDIFILL(IDEV,V1,VAL)	!ERASE DISPLAY PLANE

	SEC = 1
	XST = XDLCONNECT(IDEV,V1,LUT,SEC,.FALSE.)	!CONNECT PLANES
	XST = XDLCONNECT(IDEV,V1,LUTL,SEC,.FALSE.)
	XST = XDLCONNECT(IDEV,V1,LUTR,SEC,.FALSE.)

	XST = XDLRAMP(IDEV,LUT,SEC)	!ELIMINATE STRETCHES IN LUTS
	XST = XDLRAMP(IDEV,LUTL,SEC)
	XST = XDLRAMP(IDEV,LUTR,SEC)
        XST = XDGCONNECT(IDEV,G1,SEC,.FALSE.)	!PICK PLANE FOR GRAPHICS
	XST = XDIFILL(IDEV,G1,VAL)		!ERASE G1
        XST = XDGLINIT(IDEV,SEC)
        GDN = XDGCOLOR(IDEV,'red')              !save value for RED in COMMON
	XST = XDGON(IDEV)				!TURN G1 ON
	XST = XDTCOLOR(INT2BYTE(GDN),0)
	FONT = 1
	XST = XDTFONT(FONT)			!SET FONT TYPE
        IF (SCREEN .LE. 512) THEN
	   H = 12
        ELSE
           H = 18
        END IF
	S = 0.7
	XST = XDTSIZE(H,S)			!SET TEXT SIZE
	BLNK = 0
	IX = SCREEN/4
	IY = SCREEN/2
        NTB = 1                                 ! DEFAULT NUMBER OF TRACKBALLS.
	IF(MAXTB .EQ. 0) THEN
		NTB = 0
		T2 = T1
		CALL XVMESSAGE('ZERO TRACKBALL MODE',' ')
		XST = XDCON(IDEV,T1,FORM,BLNK)		!TURN CURSOR ON
		XST = XDCSET(IDEV,T1,IX,IY) 		!PLACE CURSOR
		RETURN
	END IF
C-----DETERMINE # OF TRACKBALLS

        IF ( XVPTST('NOTRACK') )   NTB=0
	IF(NTB .EQ. 0) THEN
		T2 = T1
		XST = XDCON(IDEV,T1,FORM,BLNK)		!TURN CURSOR ON
		XST = XDCSET(IDEV,T1,IX,IY) 		!PLACE CURSOR
	ELSE IF(NTB .EQ. 1) THEN
		XST = XDCON(IDEV,T1,FORM,BLNK)		!TURN CURSOR ON
		XST = XDCSET(IDEV,T1,IX,IY) 		!PLACE CURSOR
		XST = XDCAUTOTRACK(IDEV,T1,T1,.TRUE.)
		T2 = T1
	        CALL XVMESSAGE('ONE TRACKBALL, ALWAYS POSITION ' //
     1 'LEFT SIDE FIRST',' ')
	ELSE
		IX = 3 * SCREEN / 4
		XST = XDCON(IDEV,T2,FORM,BLNK)		!TURN CURSOR ON
		XST = XDCSET(IDEV,T2,IX,IY) 		!PLACE CURSOR
		XST = XDCAUTOTRACK(IDEV,T2,T2,.TRUE.)
	ENDIF
C
	RETURN
	END
c
c**************************************************************
	SUBROUTINE DISP(V1,IMP,SSD,SLD,ZOOM,NSI,NLI,OLDZ,*,*)
C-----THIS SUBROUTINE WILL DISPLAY AN IMAGE ON THE 
C-----SPECIFIED PORTION OF THE V1 IMP.  IF THE DESIRED
C-----AREA IS CONTAINED IN THE PIC'S IMP, A COPY WILL
C-----RESULT, IF NOT THE IMP must BE REFILLED FROM DISK.
C	V1    IS THE DISPLAY PLANE
C	IMP   IS THE PLANE WITH THE IMAGE TO BE REDISP'D
C	SSD   IS THE DESIRED DISPLAYED SS FOR THE IMAGE
C	SLD   IS THE DESIRED DISPLAYED SL FOR THE IMAGE
C	ZOOM  IS THE DESIRED ZOOM FACTOR FOR THE IMAGE
C	NSI   IS THE NUMBER OF SAMP OF THE ORIG. IMAGE
C	NLI   IS THE NUMBER OF LINES OF THE ORIG. IMAGE
C	OLDZ  IS THE EXISTING ZOOM FACTOR IN THE IMPS
C       RETURN 1 IS FOR SS OF DISP AREA IS NOT ODD
C       RETURN 2 IS FOR DISP AREA NOT ALL IN IMP(MUST REREAD)
C
	IMPLICIT INTEGER (A-W), LOGICAL (X)
        COMMON/CD/IDEV,LEFT,TOP,RIGHT,BOTTOM,SCREEN,MAX,
     .            SLI(2),SSI(2),NLIMP(2),NSIMP(2),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	REAL OLDZ(2)
	INTEGER LEFT(2),TOP(2),RIGHT(2),BOTTOM(2),ll,tt,bb,rr
	INTEGER IMP,IDEV,V1,L,T,R,B,ESD,ELD
C
	IF(ZOOM .NE. OLDZ(IMP-1) ) GO TO 200   !RET IF NEW ZOOM
	IF(MOD(SSD,2) .EQ. 0) GO TO 300		!RETURN IF EVEN
	L = LEFT(IMP-1)			!SET AW FOR CORRECT SIDE
	T = TOP(IMP-1)
	B = BOTTOM(IMP-1)
	R = RIGHT(IMP-1)
	scrs = r - l + 1                !SAMPLES IN AW
	scrl = b - t + 1                !LINES IN AW
	NSD = NSI - SSD + 1		!NS LEFT IN PIC
	NLD = NLI - SLD + 1		!NL LEFT IN PIC
	N = NSD*ZOOM
	N = (N/2)*2			!ENSURE IT'S EVEN
	NSD = MIN0(SCRS,N)/ ZOOM	!NS TO DISP REAL PIXELS
	N = NLD*ZOOM
	NLD = MIN0(SCRL,N)/ ZOOM	!NL TO DISP REAL PIXELS
	NSD = (NSD/2) * 2		!MAKE NSD EVEN
C-------CHECK IF REQUESTED AREA IS WITHIN THE IMP
	IF(SLD .LT. SLI(IMP-1) .OR. SSD .LT. SSI(IMP-1)) GO TO 100
	ELD = SLD + NLD - 1		!FIND END OF REQUEST AREA
	ESD = SSD + NSD - 1
	ELI = SLI(IMP-1) + NLIMP(IMP-1) - 1	!FIND END OF IMP
	ESI = SSI(IMP-1) + NSIMP(IMP-1) - 1     !IN REAL PIXELS
C-------CHECK IF REQUESTED AREA IS WITHIN THE IMP
	IF(ELD .GT. ELI .OR. ESD .GT. ESI) GO TO 100
C-------NOW WE START THINKING OF ZOOMED PIXELS
	ll = (ssd - ssi(imp-1) )*ZOOM + 1  !FIND WHERE IN IMP TO READ
	tt = (sld - sli(imp-1) )*ZOOM + 1
	rr = (SSD - SSI(IMP-1) + NSD ) * ZOOM
	bb = (SLD - SLI(IMP-1) + NLD ) * ZOOM
	NLZ = BB - TT + 1
	NSZ = RR - LL + 1
	NSZ = (NSZ/2)*2
	RR = LL + NSZ - 1			!MAKE COPY NS EVEN
	XST = XDIAWSET(IDEV,IMP,LL,TT,RR,BB)     !SET IMP FOR COPY
	r = l + nsZ - 1                    !set v1 size same as imp
	b = t + nlZ - 1                   
	XST = XDIAWSET(IDEV,V1,L,T,R,B)            !SET V1  FOR COPY
	XST = XDIICOPY(IDEV,IMP,V1)		!COPY
	RETURN
c
100	CALL XVMESSAGE('DISPLAY AREA NOT ALL IN IMP',' ')
	RETURN 2
200	OLDZ(IMP-1) = ZOOM
	RETURN 2
300	CALL  XVMESSAGE('SS OF DISP NOT ODD',' ')
	RETURN 1
	END
c
c**************************************************************
	SUBROUTINE FILLIMP(IMP,DSRN,SL,SS,NLI,NSI,IZ,ZZ)
C-------THIS ROUTINE WILL FILL THE INDICATED IMP WITH
C-------DATA FROM DISK BASED ON THE SL AND SS SPECIFIED.
C-------IMP   IS THE PLANE TO BE FILLED.
C       DSRN  IS THE FILE TO BE READ
C 	SL    IS THE STARTING LINE OF THE AREA TO BE READ
C	SS    IS THE STARTING SAMP OF THE AREA TO BE READ
C 	NLI   IS THE NUMBER OF LINES IN THE IMAGE
C	NSI   IS THE NUMBER OF SAMPS IN THE IMAGE
C	IZ    IS THE ZOOM FACTOR 1,2...,-1,-2,...ETC
C 	ZZ    IS THE REAL ZOOM FACTOR  1.,2.,.5,.1 ETC
C
	IMPLICIT INTEGER (A-Z)
	COMMON/C8/BUF
        COMMON/CD/IDEV,FIL(8),SCREEN,MAX,SLI(2),SSI(2),NLIMP(2),
     .           NSIMP(2),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	REAL ZZ
	INTEGER IDEV,IMP,MSL
	LOGICAL XST,XDILINEWRITE,XDIAWSET
	BYTE BUF(8192)
C
	HOLD = 1
	IF(IZ .GT. 1) HOLD = IZ
	INC = 1
	IF(IZ .LT. 0) INC = -IZ
	MSL = 1
	NLD = 0
	SLI(IMP-1) = SL              !SET CORNER OF IMP
	SSI(IMP-1) = SS
	NL = NLI - SL + 1            !PIXELS LEFT IN IMAGE
	NS = NSI - SS + 1
	Z = IFIX(NL*ZZ)          !ZOOMD EXTENT OF IMAGE
	NLZ = MIN0(MAX,Z)        !IMP SIZE VS MAX REQUESTED
	Z = IFIX(NS*ZZ)
	NSZ = MIN0(MAX,Z)
	NSZ = (NSZ/2) * 2              ! SO NS IS EVEN
	NRL = NLZ / ZZ	     !REAL PIXELS IN NLZ ZOOM'D PIXELS
	NRS = NSZ / ZZ
	NLIMP(IMP-1) = NRL   !REAL PIXEL EXTENT OF REQUEST
	NSIMP(IMP-1) = NRS
	EL = SL + NRL - 1          !LAST REAL LINE REQUESTED
C-------SET WINDOW TO AS MUCH IMP AS NEEDED
	XST = XDIAWSET(IDEV,IMP,1,1,NSZ,NLZ)  !EXTENT REQUESTED
C
	DO 200 L=SL,EL,INC
        CALL XVREAD( DSRN, BUF, IST, 'LINE', L, 'SAMP', SS, 'NSAMPS',
     &   NRS,' ')
	IF(IZ .GT. 1) CALL SHOLD(BUF,NSZ,IZ)    !EXPAND BUF TO NSZ
	IF(IZ .LT. 0) THEN
C>MVE REPLACED WITH BELOW. CALL MVE(1,NSZ,BUF,BUF,INC,1)  !REDUCE BUF TO NSZ

          LTO   = 1				! array index for destination of move
          LFROM = 1				! array index for source of move
          DO LMOVE = 1, NSZ			! move  values 
           BUF(LTO) = BUF(LFROM)
           LFROM = LFROM + INC
           LTO   = LTO   + 1
          END DO
        END IF
C
	DO 50 I=1,HOLD
	XST = XDILINEWRITE(IDEV,IMP,1,MSL,NSZ,BUF)  !WRITE TO IMP
	NLD = NLD + 1
	IF(NLD .GE. NLZ) GO TO 201
50	MSL = MSL + 1
C
200	CONTINUE
201	RETURN 
	END
c
c**************************************************************
	SUBROUTINE SHOLD(BUF,N,IZ)
	IMPLICIT INTEGER (A-Z)
	BYTE BUF(8192)
C
	D = N/IZ
	D2 = IZ * D
C>MVE REPLACED WITH NEXT LOOP.CALL MVE(1,D,BUF(D),BUF(D2),-1,-IZ)!EXPAND BUFFER

      LTO   = D2			! array index for destination of move
      LFROM = D 			! array index for source of move
      DO LMOVE = 1, D			! move  values 
         BUF(LTO) = BUF(LFROM)
         LFROM = LFROM - 1 
         LTO   = LTO   - IZ
      END DO

	M = IZ
	DO 10 I=1,N                           !FILLIN BUFFER
	IF(I .GT. D2) THEN
		BUF(I) = 0
	ELSE	
		IF(MOD(I,IZ) .NE. 0) THEN
			BUF(I) = BUF(M)
		ELSE
			M = M + IZ
		ENDIF
	ENDIF
10	CONTINUE
C
	RETURN
	END
c
c**************************************************************
	SUBROUTINE KEY(TB,J)
C-------THIS ROUTINE WILL MOVE CURSOR TB USING THE FOLLOWING
C-------KEYBOARD KEYS:
C   	Y...UP   N...DOWN  J...RIGHT   G...LEFT
C	H...CHANGES THE SIZE OF EACH STEP IN A ROTARY FASHION
C	STEP STARTS AS 100 PX. EACH ENTRY OF H CHANGES THE
C	STEP SIZE BY A FACTOR OF TEN.
C	ANY OTHER KEY ENTERED, EXITS THIS ROUTINE.
C	NOTE: THE KEYS ARE NOT FOLLOWED BY CR.
	IMPLICIT INTEGER (A-Z)
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
	INTEGER TB,IDEV
        CHARACTER*1 INPC
        INTEGER     WAITFORINPUT
	INTEGER C(4)
	LOGICAL I
        DATA C/100,10,1,10/
C
C-------INITIAL STEP SIZE IS SET AT CALL BY ARGUMENT J
	K = C(J)
	I = .TRUE.
	CALL RCURSE(IDEV,TB,L,S)
	CALL XVMESSAGE('MOVE CURSOR WITH Y,G,J,N KEYS',' ')
	CALL XVMESSAGE('H KEY INCREMENTS STEP SIZE',' ')
	CALL XVMESSAGE('ANY OTHER KEY TO EXIT',' ')
	CALL PRNT(4,1,K,'STEP SIZE.')
C
	DO WHILE (I)
        INPC = CHAR( WAITFORINPUT(1) ) !GET INPUT CHARACTER IN CHAR*1 VARIABLE.
        CALL UPRCASE(INPC)             !CONVERT TO UPPER CASE.
		IF(INPC.EQ. 'H') THEN
		J = J+1
		IF(J .EQ. 5) J = 1
		K = C(J)
		CALL PRNT(4,1,K,'STEP SIZE.')
		GO TO 11
	END IF
C
	IF(INPC .EQ.'Y') THEN
		L = L - K
	ELSE IF(INPC .EQ. 'N') THEN
		L = L + K
	ELSE IF(INPC .EQ. 'G') THEN
		S = S - K
	ELSE IF(INPC .EQ. 'J') THEN
		S = S + K
	ELSE
		GO TO 20
	END IF
C
10 	CALL WCURSE(IDEV,TB,L,S)
11	CONTINUE
	END DO
C
20 	IDUMMY= WAITFORINPUT(0)    ! TURN OFF CHARACTER MODE.
        CALL XVMESSAGE('EXITTING',' ')
	RETURN
	END
c
c**************************************************************
	subroutine crform(t1,iform,*)
	implicit integer (a-w), logical (x)
	COMMON/CD/IDEV,KDUM(18),GDN
        INTEGER GDN                  ! VALUE FOR RED.
c
	blnk = 0
	xst = xdcon(IDEV,t1,iform,blnk)
	if(.not. xst) then
		call XVMESSAGE('cursor not defined',' ')
		return 1
	end if
c
	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create picreg.imake
#define  PROGRAM   picreg

#define MODULE_LIST picreg.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_VRDI
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create picreg.pdf
process help=*
SUBCMD-DEFAULT MAIN
PARM 	INP	TYPE=STRING	COUNT=(2:3)
PARM	OUT	TYPE=STRING	COUNT=(1:2)
PARM	SIZE	TYPE=INTEGER	COUNT=4	DEFAULT=(1,1,0,0)
PARM	SL	TYPE=INTEGER		DEFAULT=1
PARM	SS	TYPE=INTEGER		DEFAULT=1
PARM	NL	TYPE=INTEGER		DEFAULT=0
PARM	NS	TYPE=INTEGER		DEFAULT=0
PARM	U	TYPE=INTEGER		DEFAULT=0
PARM	D	TYPE=INTEGER		DEFAULT=0
PARM	L	TYPE=INTEGER		DEFAULT=0
PARM	R	TYPE=INTEGER		DEFAULT=0
PARM 	U1	TYPE=INTEGER		DEFAULT=0
PARM	U2	TYPE=INTEGER		DEFAULT=0
PARM	D1	TYPE=INTEGER		DEFAULT=0
PARM	D2	TYPE=INTEGER		DEFAULT=0
PARM	L1	TYPE=INTEGER		DEFAULT=0
PARM	L2	TYPE=INTEGER		DEFAULT=0
PARM	R1	TYPE=INTEGER		DEFAULT=0
PARM	R2	TYPE=INTEGER		DEFAULT=0
PARM	NHOR    TYPE=INTEGER		DEFAULT=20
PARM	NVER	TYPE=INTEGER		DEFAULT=20
PARM	REDO	TYPE=INTEGER		DEFAULT=0
PARM	DELETE	TYPE=INTEGER			COUNT=2		DEFAULT=(0,0)
PARM	AREA	TYPE=INTEGER		DEFAULT=20
PARM	CONV	TYPE=KEYWORD	VALID=(MIN,MAX)	COUNT=(0:1)	DEFAULT=MIN
PARM	POWER	TYPE=INTEGER		DEFAULT=0
PARM	MODE	TYPE=KEYWORD	VALID=(CORR,POINT) COUNT=(0:1) DEFAULT=POINT
PARM	HIPASS	TYPE=KEYWORD	VALID=(HPF,NOHPF)   		DEFAULT=HPF
PARM	PHASE	KEYWORD		VALID=(PHASE,NOPHASE)		DEFAULT=NOPHASE
PARM	FIT	TYPE=INTEGER	VALID=1:7	DEFAULT=1
PARM	TPFORM	TYPE=INTEGER	VALID=1:9	DEFAULT=1
PARM	USE	TYPE=INTEGER	VALID=4:10 	DEFAULT=4
PARM	INTERP  TYPE=KEYWORD	VALID=(INTE,NOIN)   COUNT=(0:1) DEFAULT=NOIN
PARM	TEXTSIZE     INTEGER	VALID=1:99          COUNT=(0:1) DEFAULT=--
PARM	ZOOM	TYPE=INTEGER		DEFAULT=1
PARM	Z1	TYPE=INTEGER		DEFAULT=1
PARM	Z2	TYPE=INTEGER		DEFAULT=1
PARM	STRETCH	TYPE=INTEGER		COUNT=2			DEFAULT=(0,255)
PARM	LINEAR	TYPE=INTEGER		COUNT=2			DEFAULT=(0,255)
PARM	SL1	TYPE=INTEGER		DEFAULT=1
PARM	SS1	TYPE=INTEGER		DEFAULT=1
PARM	SL2	TYPE=INTEGER		DEFAULT=1
PARM	SS2	TYPE=INTEGER		DEFAULT=1
PARM	MINL	TYPE=INTEGER		DEFAULT=1
PARM	MINS	TYPE=INTEGER		DEFAULT=1
PARM	MAXL	TYPE=INTEGER		DEFAULT=1
PARM	MAXS	TYPE=INTEGER		DEFAULT=1
PARM 	HOME	TYPE=KEYWORD	VALID=HOME	COUNT=(0:1)	DEFAULT=--
PARM	PRINT	TYPE=KEYWORD  VALID=(PRINT,NOPRINT) COUNT=(0:1) DEFAULT=NOPRINT
PARM	DSTAT	TYPE=KEYWORD	VALID=DSTAT	COUNT=(0:1)	DEFAULT=--
PARM	LOCATE	TYPE=INTEGER		DEFAULT=1
PARM	SHOW	TYPE=KEYWORD	VALID=SHOW	COUNT=(0:1)	DEFAULT=--
PARM 	CURSOR	TYPE=INTEGER	VALID=(1:8)	DEFAULT=1
PARM 	NOTRACK	TYPE=KEYWORD	VALID=NOTRACK	COUNT=(0:1)	DEFAULT=--
END-SUBCMD
SUBCMD IPARAM
PARM	EXIT	TYPE=KEYWORD	VALID=EXIT	COUNT=(0:1)	DEFAULT=--
PARM	U	TYPE=INTEGER		DEFAULT=0
PARM	D	TYPE=INTEGER		DEFAULT=0
PARM	L	TYPE=INTEGER		DEFAULT=0
PARM	R	TYPE=INTEGER		DEFAULT=0
PARM 	U1	TYPE=INTEGER		DEFAULT=0
PARM	U2	TYPE=INTEGER		DEFAULT=0
PARM	D1	TYPE=INTEGER		DEFAULT=0
PARM	D2	TYPE=INTEGER		DEFAULT=0
PARM	L1	TYPE=INTEGER		DEFAULT=0
PARM	L2	TYPE=INTEGER		DEFAULT=0
PARM	R1	TYPE=INTEGER		DEFAULT=0
PARM	R2	TYPE=INTEGER		DEFAULT=0
PARM	NHOR    TYPE=INTEGER		DEFAULT=20
PARM	NVER	TYPE=INTEGER		DEFAULT=20
PARM	REDO	TYPE=INTEGER		DEFAULT=0
PARM	DELETE	TYPE=INTEGER			COUNT=2		DEFAULT=(0,0)
PARM	AREA	TYPE=INTEGER		DEFAULT=20
PARM	CONV	TYPE=KEYWORD	VALID=(MIN,MAX)	COUNT=(0:1)	DEFAULT=MIN
PARM	POWER	TYPE=INTEGER		DEFAULT=0
PARM	MODE	TYPE=KEYWORD	VALID=(CORR,POINT) COUNT=(0:1) DEFAULT=POINT
PARM	HIPASS	TYPE=KEYWORD	VALID=(HPF,NOHPF)   		DEFAULT=HPF
PARM	PHASE	KEYWORD		VALID=(PHASE,NOPHASE)		DEFAULT=NOPHASE
PARM	FIT	TYPE=INTEGER	VALID=1:7	DEFAULT=1
PARM	TPFORM	TYPE=INTEGER	VALID=1:9	DEFAULT=1
PARM	USE	TYPE=INTEGER	VALID=4:10 	DEFAULT=4
PARM	INTERP  TYPE=KEYWORD	VALID=(INTE,NOIN)   COUNT=(0:1) DEFAULT=NOIN
PARM	TEXTSIZE     INTEGER	VALID=1:99          COUNT=(0:1) DEFAULT=--
PARM	ZOOM	TYPE=INTEGER		DEFAULT=1
PARM	Z1	TYPE=INTEGER		DEFAULT=1
PARM	Z2	TYPE=INTEGER		DEFAULT=1
PARM	STRETCH	TYPE=INTEGER		COUNT=2			DEFAULT=(0,255)
PARM	LINEAR	TYPE=INTEGER		COUNT=2			DEFAULT=(0,255)
PARM	SL1	TYPE=INTEGER		DEFAULT=1
PARM	SS1	TYPE=INTEGER		DEFAULT=1
PARM	SL2	TYPE=INTEGER		DEFAULT=1
PARM	SS2	TYPE=INTEGER		DEFAULT=1
PARM	MINL	TYPE=INTEGER		DEFAULT=1
PARM	MINS	TYPE=INTEGER		DEFAULT=1
PARM	MAXL	TYPE=INTEGER		DEFAULT=1
PARM	MAXS	TYPE=INTEGER		DEFAULT=1
PARM 	HOME	TYPE=KEYWORD	VALID=HOME	COUNT=(0:1)	DEFAULT=--
PARM	PRINT	TYPE=KEYWORD  VALID=(PRINT,NOPRINT) COUNT=(0:1) DEFAULT=NOPRINT
PARM	DSTAT	TYPE=KEYWORD	VALID=DSTAT	COUNT=(0:1)	DEFAULT=--
PARM	LOCATE	TYPE=INTEGER		DEFAULT=1
PARM	SHOW	TYPE=KEYWORD	VALID=SHOW	COUNT=(0:1)	DEFAULT=--
PARM 	CURSOR	TYPE=INTEGER	VALID=(1:8)	DEFAULT=1
PARM POSITION	TYPE=INTEGER  COUNT=2       DEFAULT=(1,1)    VALID=(1:99999)
END-SUBCMD
SUBCMD READY ! TO ALLOW USER TO MOVE CURSOR AND HIT RETURN.
PARM POSITION	TYPE=INTEGER  COUNT=2       DEFAULT=(1,1)    VALID=(1:99999)
END-SUBCMD
SUBCMD GEOM
PARM	PGM    TYPE=KEYWORD VALID=(GEOMA,LGEOM,MGEOM) COUNT=1 DEFAULT=GEOMA
END-SUBCMD
END-PROC
.TITLE
 PICREG
.HELP
 Purpose:

 PICREG is an interactive program designed to allow the user to select
 tiepoints (matching points) in two images.  It displays both images in a
 split-screen fashion and collects tiepoints as they are cursored by the user.
 The tiepoints can then be formatted for use with TIECONM, GEOMA, LGEOM,
 or MGEOM to perform image registration (warping or mapping the first image to
 the geometry of the second image.)  (Note to testers: PICREG is used only in
 interactive mode.)
.page
TABLE OF CONTENTS / LIST OF SECTIONS IN HELP.
1.0  MOST RECENT CHANGES  
2.0  INVOCATION OF PICREG:
3.0  OVERVIEW OF PARAMETERS
4.0  GUIDELINES FOR SELECTING PARAMETERS
5.0  POSSIBLE APPLICATIONS OF PICREG  
6.0  RELATIONSHIP TO OTHER PROGRAMS
7.0  ADVANTAGES AND DISADVANTAGES OF PICREG VERSUS ALTERNATE METHODS
8.0  MODES OF SELECTING TIEPOINTS
9.0  GUIDELINES FOR COLLECTING TIEPOINTS
10.0  HOW TO USE CORRELATION
11.0  GUIDE FOR USING THE TPFORM PARAMETER
12.0  RESTRICTIONS
13.0  EXAMPLES
14.0  OPERATION
15.0  REFERENCES
16.0  REVISION HISTORY
17.0  PRECISION
.page

1.0 MOST RECENT CHANGES  (See REVISION HISTORY for details.)

a) The prompt for the number of trackballs has been removed.  The default is
   for there to be one trackball or mouse. 
b) The tiepoint numbering is now left justified and thus will be closer to
   the tiepoint.  Also the TEXTSIZE parameter has been added to allow the user
   to select a comfortable size for the device being used.
c) PICREG has been modified to work on the IVAS and on X Window System devices
   with four image memory planes.  X displays with 8 bits per pixel are not
   yet supported.
d) Example 3 has been added to show an end-to-end mosaicking example.
.page
2.0  INVOCATION OF PICREG:

PICREG allows tiepoints to be selected in one sitting (one execution of PICREG)
 OR to be accumulated using multiple executions of PICREG.

The following TAE command line formats show the most common usages:
      picreg INP=(a1,a2)       OUT=newtp          optional parameters
      picreg INP=(a1,a2)       OUT=(newtp,parout) optional parameters
      picreg INP=(a1,a2,oldtp) OUT=newtp          optional parameters
      picreg INP=(a1,a2,oldtp) OUT=(newtp,parout) optional parameters

       Here 'a1' represents the to-be-registered input image file name,
       'a2' represents the reference image file name, the file whose
            geometry is taken as the reference,
       'newtp' represents the output file containing the tiepoints selected
               by the user,  which may be used in a subsequent run of PICREG
               if the selection of more tiepoints is to be continued,
       'parout' represents the output file containing a set of tiepoints
                derived from the points in newtp according to the TPFORM
                parameter.  (See under GUIDE FOR USING THE TPFORM PARAMETER
                and the level 2 help for TPFORM.)
                This file is in a form to be used by TIECONM, LGEOM, MGEOM,
                or GEOMA.
       'oldtp'  represents an optional input file  of previously collected
                tiepoints from a prior run of PICREG.  (This may the file
                that was specified as the newtp in the prior run of PICREG,
                or it may be a similar file of tiepoints from another source.
                See under RELATIONSHIP TO OTHER PROGRAMS for possible other
                sources.)
.page
3.0  OVERVIEW OF PARAMETERS

 PICREG has numerous parameters.  Most of these are interactive parameters
 that can be entered at the interactive prompt which says 'Enter parameters:'.
 Most of the interactive parameters can be entered on the PICREG command line
 where applicable.

 The interactive parameters can be categorized under the following headings:
   Controlling the Display  
   Setting the Tiepoint-Gathering Mode
   Editing the Set of Tiepoints
   Obtaining Numeric Information from the Program
   Generating the Output Tiepoint File

3.1 Controlling the Display (first image = left side; second image = right side)
   U,D,L, or R      - to move Up, Down, Left, or Right in both images
   U1,D1,L1, or R1  - to move Up, Down, Left, or Right in the first (left) image
   U2,D2,L2, or R2  - to move Up, Down, Left, or Right in the second image
   TEXTSIZE         - to set the height of tiepoint numbers on the display.
   ZOOM             - to zoom (magnify or reduce) both images by a scale factor
   Z1 or Z2         - to zoom the first image or the second image, respectively
   SL1 or SL2       - to display starting at the specified line in the first 
                      image or the second image, respectively
   SS1 or SS2       - to display starting at the specified sample in the first 
                      image or the second image, respectively
   'HOME            - to display starting at line 1, sample 1 in both images
   LOCATE           - to center both sides about a previously-obtained tiepoint
   'SHOW            - to show tiepoint displacement vectors
   CURSOR           - to change the shape (style) of the cursor on the display

(For X devices, if the images do not appear at the beginning, try clicking
 the mouse in the display window.  There are some new features available
 in the VRDI for X.  See Bob Deen for details.)

.page
3.2  Setting the Tiepoint-Gathering Mode

   'CORR, 'POINT, AREA - to set one of three modes. 'CORR turns on the
                         assistance from the computer in finding the right-side
                         tiepoint location using FFT cross-correlation.
                         The AREA parameter turns on the assistance from the 
                         computer in finding the tiepoint locations
                         using the AREA correlation.  (The AREA mode has limited
                         application.)   'POINT turns off any assistance from 
                         the computer in finding the right-side tiepoint 
                         location.  This is the default mode, where both sides
                         are cursored manually.
   POWER               - The POWER parameter implies 'CORR and sets the size 
                         of the correlation window to 2**power.  
   'HPF, 'NOHPF        - 'HPF applies a high-pass filter to the image data
                         for the computation of the right-side tiepoint location
                         to prevent results being thrown off by low frequency
                         image information.  'HPF is the default.  'NOHPF keeps
                         the high pass filter from being applied.
   'PHASE, 'NOPHASE    - 'PHASE often prevents the FFT correlation from going 
                         astray in the computation of the right-side tiepoint 
                         location.  'NOPHASE is the default.  
   'INTE, 'NOIN        - With FFT correlation these parameters affect 
                         the computation of the right-side tiepoint 
                         location.  With 'NOIN, the location is determined to
                         the nearest pixel.  With 'INTE, the location is
                         determined to sub-pixel accuracy.  
   'MIN, 'MAX          - 'MIN causes PICREG to search for a local minimum
                         to use as a tiepoint location in the AREA mode.
                         'MAX searches for a local maximum.
.page
3.2  Editing the Set of Tiepoints
   (no parameter)      - If no parameters are entered at the interactive prompt,
                         PICREG begins the sequence to add a tiepoint pair to
                         the set.
   REDO                - REDO is used to change the location of the specified
                         tiepoint pair.
   DELETE              - DELETE is used to delete the specified range of
                         tiepoint pairs.
   EXIT or 'EXIT       - This concludes the edit session and exits the program.

3.3  Obtaining Numeric Information from the Program (displayed on terminal)
   'DSTAT              - to print the sl, ss, and zoom factor for each image.
   FIT                 - to print how well the current set of tiepoints fits
                         the specified transformation type.  (See HELP FIT.)
   'PRINT              - to print all raw tiepoint locations upon exit and to
                         print least squares fit information when the FIT is
   'NOPRINT              specified. 'NOPRINT turns off this printing.
.page
3.4  Generating the Output Tiepoint File (See also How to Use TPFORM Parameter.)
   TPFORM              - to generate the output tiepoint file from the raw
                         (user-selected) tiepoints according to the specified
                         transformation type.  (See HELP TPFORM.)  For most
                         transformation types, one or more of the other
                         parameters in this group are utilized.  These are
                         often defaulted; however non-default values may be 
                         specified at the same time or prior to when the TPFORM
                         value is entered.
   NHOR                - to specify the number of columns of tiepoints in the 
                         output grid, for the case of TPFORM = 6, 7, or 8.  
   NVER                - to specify the number of rows of tiepoints in the 
                         output grid, for the case of TPFORM = 6, 7, or 8.  
   MINL                - minimum line number of the output grid tiepoints. 
   MAXL                - maximum line number of the output grid tiepoints. 
   MINS                - minimum sample number of the output grid tiepoints. 
   MAXS                - maximum sample number of the output grid tiepoints. 
   USE                 - the number of tiepoints used to determine each output
                         tiepoint location, for the case of tpform=8 only.
.page
4.0  GUIDELINES FOR SELECTING PARAMETERS

The following are suggestions.

a) Try the STRETCH (or (LINEAR) parameter to contrast enhance the images 
   initially or to adjust the brightness occasionally to help you see the
   details in the image.
b) Line up the left and right images on the display using U1,U2,D1,D2,
   etc. so the same features appear on both the left and right side of the
   display.
c) Note the size of the input images and the screen size.  If the images
   do not fit entirely on the screen, plan a sequence for traversing the
   overlap area of the input images.  Try R= a value slightly less than half
   the window size.  Try D= a value slightly less than the window size.  You
   may like to use SL1,SS1, SL2, and SS2 especially if you want to display the
   bottom and right edges of the images so that the edges of the image line up
   with the edge of the screen.  (Otherwise there may be left-overs between the
   edge of the image and the edge of the screen.)
d) Use the DSTAT parameter to help keep track of where you are in the image.
   You can abbreviate - just type 'D or 'd for 'DSTAT.
e) Try starting with 'CORR 'INTE 'PHASE.  (You can build your own PDF that
   calls PICREG with your favorite starting parameters on the PICREG command
   line.   You could also build one to copy and rename tiepoint files to help
   keep the names of tiepoint files organized.)
f) Select a cursor form that works well for you.  I suggest CURSOR=3 for
   the DeAnza or X devices.  For the IVAS, CURSOR=7 gives a nice cursor
   where the upper left point of the bar is the cursor location.
g) Select a tiepoint pair.  Use TEXTSIZE if the default tiepoint numbering is
   not suitable.  You can set TEXTSIZE=1 when you want the numbering to go away
   for a while and then set it back to what it was.
h) Select first some tiepoints at prominent or distinctive features to give 
   PICREG some data that you have high confidence in.  If you are not planning
   to use TPFORM=9, this will make finding any erroneous tiepoints easier.
   Try to get an initial set of tiepoints that give small residuals for FIT.
i) As you collect tiepoints, use the FIT parameter to check that there are
   no erroneous tiepoints.  (See the GUIDE TO USING THE TPFORM PARAMETER for 
   details on FIT.  If FIT indicates an error, the tiepoint with the largest
   residual is not always the problem.  Enter 'PRINT and enter 'FIT again.
   This time FIT will give you the residuals for all of the tiepoint pairs
   both horizontally (x values) and vertically (y values).  Look for the 
   values listed following 'RESIDUALS OBSERVED - COMPUTED' for both x and y.
   They will be listed in the order of the tiepoint numbering.  Check all 
   tiepoints with a significant x or y residual.
   Another way to locate a bad tiepoint is with the SHOW parameter.  Look for 
   an offset vector that is much different from the neighboring offset vectors.
j) If you want to find a tiepoint that is not on the screen, use the LOCATE
   parameter.  If you want to eventually come back to what is on the display
   before you do the LOCATE,  then type 'D, note your position, and then use
   SL1, SS1, SL2, and SS2 when it is time to come back.
.page
5.0 POSSIBLE APPLICATIONS OF PICREG  

PICREG is a versatile program with many possible applications.  The main 
application is the gathering of tiepoints as a first step in image 
registration.  (Image registration is important for such tasks as the
generation of color pictures by registering the red, green, and blue images,
for generating mosaics by registering the component images to be used in the
mosaic and finding their relative offsets, for registering elevation data to
image data, and for production of time-lapse sequences or movies.)

In image registration one image is chosen to be the reference image (either
arbitrarily or because of prior processing), and the other is geometrically
corrected (transformed) to the geometry of the reference image.  PICREG is 
used to measure the first input image against the geometry (spatial 
characteristics) of the second input (reference) image.  The measurement
is done by finding the locations (coordinates) of user-selected features or
locations in both images.  PICREG uses these measurements to compute (via the
TPFORM parameter) the geometric transformation that will transform the first
image to the geometry of the second.  The image registration is commpleted by
using one of the Geom programs to apply the transformation to the first image.
The user may choose to use program TIECONM as a middle step in the image
registration.  (See under Guide for using the TPFORM parameter.)

Another use for PICREG is visual validation and editing of tiepoints produced
by another source, such as program R or PICMATCH, which generate tiepoints
automatically.  In such a case the tiepoints from the other source would be
specified as the third input file for PICREG.  The FIT parameter can be used to 
aid in finding incorrect tiepoints, or they might be found by visual inspection.
The REDO parameter can be used to correct a tiepoint pair.  The DELETE parameter
can be used to delete incorrect tiepoints.  The TPFORM parameter can be used
to generate a regular tiepoint grid from the edited tiepoints.

Another application of PICREG is the measurement of change between two pictures
of a scene taken at different times.  For example, someone might want to
measure the changes in the shape and position of clouds, the deformation of a
material caused by some process, changes in a contour plot, or changes in 
images produced in the scientific visualization of data.  For such cases, the 
value of TPFORM is critical.  A value of 3 for TPFORM will cause PICREG to
produce a first order approximation (or average) of the changes measured at the
tiepoints.  In many cases the user may want to study the exact changes at each
tiepoint. In these cases, a value of 9 should be specified for parameter TPFORM.
Then, after exiting PICREG, the user can display and study the tiepoints (in the
second output file) with program TIEPLOT or can process the tiepoints further
using IBIS progams such as MF or IBISSTAT. If a regular tiepoint grid is 
desired, the second output file from PICREG can be processed using program
TIECONM.

6.0  RELATIONSHIP TO OTHER PROGRAMS
In some cases it would be appropriate to do some preprocessing of one or both
input images prior to running PICREG.  If the input images are not BYTE data,
program CFORM may be used to convert them to BYTE.  If the images are quite
different in brightness, programs HISTGEN and STRETCH can be used on one of the
images so that its histogram matches the histogram of the other image.  
(Parameter LINEAR can be used in PICREG to stretch both images together, but
PICREG does not support stretching each side independently.)

If the two images have different rotation, procedure ROTATE or program FLOT
may be used to rotate one so that the input images have roughly the same
rotation.  This is important if the 'CORR parameter is to be used.

In some cases it would be appropriate to run program MAP3 on both of the input 
images before trying to register them.  For example, when trying to register
frames for a time-lapse sequence of an atmospheric feature on Jupiter, it would
be appropriate to perform a map projection to compensate for the fact that
Jupiter may rotate on its axis considerably during the course of the sequence.

When using PICREG as part of the image registration process, PICREG is followed 
by using one of the Geom programs to apply the transformation to the first 
image. (See the help file for program GEOMA for a discussion of the differences
between the different Geom programs.) The user may choose to use program
TIECONM as a middle step in the image registration.  (See under GUIDE FOR USING
THE TPFORM PARAMETER.) 

When using PICREG for visual validation and editing of tiepoints produced
by another source, such as program R or PICMATCH, the tiepoints from the other
source would be specified as the third input file for PICREG. The second output
file would reflect any changes made in PICREG.   (See under GUIDE FOR USING
THE TPFORM PARAMETER.) 
.page
7.0  ADVANTAGES AND DISADVANTAGES OF PICREG VERSUS ALTERNATE METHODS

The purpose of this section is to give the user some information for
deciding when to use PICREG and when to use some alternate methods.  
First I will describe some aspects of PICREG.  Then I will give an overview
of some similar programs.

PICREG is an interactive program (requires user interaction) for selecting
tiepoints.  Thus PICREG is not suitable for systematic or batch processing.
PICREG takes advantage of the user's ability to recognize similar features
in the two input images and thus can be used to find tiepoints even when
there is very little spatial overlap between the two input images.  PICREG 
gives the user full control over where and how many tiepoints are collected,
whether a given tiepoint pair is accepted or thrown out, and what post-
processing is performed on the raw tiepoints.  PICREG thus has an advantage
of being a What-You-See-Is-What-You-Get program.

PICREG offers both manual and semi-automatic (see Example 2) tiepoint
collection.  Although semi-automatic tiepoint collection cannot be used in
every case, it allows subpixel accuracy (see 'INTE parameter) and is much
faster to use than manual tiepoint collection.

AUTOMATCH - a program which automatically acquires tiepoints from
            sets of images for the purpose of producing mosaics.  AUTOMATCH
            uses spacecraft navigation and thus is for spacecraft imagery only.
            Automatch is designed to generate tiepoints from sets of images 
            which have been generated in a "burst" of frames where the scale 
            & rotation do not differ much between images. 
C130RECT  - removes panorama distortion for certain types of data, such
            as imagery obtained from aircraft.
LOCUS     - compares sets of input coordinates and displays the differences.
INTERLOC  - special purpose program for finding locations for a camera grid 
            target.
MANMATCH  - MANMATCH is a program which acquires manual tiepoints from
            sets of images for the purpose of producing mosaics.  MANMATCH
            is like AUTOMATCH excepty that tiepoint selection is manual.
PICMATCH  - an automated program for tiepoint gathering.  Requires three
            tiepoint pairs to give it a starting orientation, although these
            can be approximate.  PICMATCH can handle significant rotation
            or scale differences.  PICMATCH can get lost if there are changes 
            in shapes of features such as clouds or if the data is 'bumpy',
            as is some imagery obtained from aircraft.
R         - a fully automated program for tiepoint gathering.  Sometimes
            can get lost if there is not good overlap between the two input
            images or if there are changes in shapes of features such as
            clouds.  R will fail if there are significant rotation or scale
            differences.
RESLOC    - locates reseau marks in Voyager images. Can be used in geometric
            correction (i.e. removing geometric distortion caused by camera).
TRACKER   - an automatic program for finding a dense grid of tiepoints for
            two images that differ geometrically in minor ways.  TRACKER
            is more effective than PICREG for dealing with stereo pairs
            and for measuring atmospheric motion, such as on gaseous planets
            such as Jupiter.  TRACKER uses a correlation algorithm that
            appears to be more effective than the one used in PICREG.
TRACKER2  - similar to TRACKER but has an image size restriction.  TRACKER2
            can be used to follow features through a sequence of images.
8.0  MODES OF SELECTING TIEPOINTS

PICREG will display both images in a split-screen manner, the first
input on the left and the second input on the right.  The user positions
the cursor(s) over identical features in each image and enters these
tiepoints by hitting <cr>.  Tiepoint positions can be obtained directly
from the cursored location (unassisted mode) or can be optimized from this
initial position with two methods. 

First, the AREA parameter invokes a method  using the extreme positions of
histograms computed from the sums of rows  and columns of pixels.  The AREA
mode is not generally recommended, though  it may prove useful when dealing
with images of stars, where the brightest  point is in the center of the
feature. 

Second, a FFT cross-correlation technique can be invoked with the CORR or 
POWER parameters.  This is recommended because it is easier to use in many
cases and can produce more precise tiepoint locations if 'INTE is also used.
(See HOW TO USE CORRELATION.)
.page
9.0 GUIDELINES FOR COLLECTING TIEPOINTS

Whether the two input images overlap completely or just partially, the user
should span the overlap area with tiepoints.  The minimum number of tiepoint
pairs necessary to select depends on the nature of the data and the TPFORM
value to be used. (See GUIDE FOR USING THE TPFORM PARAMETER.)

Here is a tip from one of the users.  For first order fits try selecting
two tiepoints in each corner and one tiepoint in the middle of the overlap 
area.  If the geometry is not very consistent throughout the image, it is best 
to use more tiepoints, perhaps 15 or 20 (pairs).  If FIT=3 gives a large 
residual, then then worst tiepoint should be checked.  If it is inaccurate,
handle it with the REDO or DELETE parameter.  If it seems accurate, a higher
order fit is probably needed and additional tiepoints should be added until
the typical residual goes down.

For imagery taken from an airplane, 100 to 200 tiepoint pairs is common as is
the use of TPFORM=9.  For such data Ron Alley suggests having a tiepoint about
every 100 pixels and extra tiepoints where there are large elevation changes,
such as in Death Valley.

It is best if possible to select tiepoints at the corners or bends in the edges
of recognizable features.  This allows visual inspection for correctness as well
as effective use of correlation.  (See HOW TO USE CORRELATION.) Points along
a straight feature generally don't work well unless they are near some
irregularity.  Margie Power wrote that the following are good sites for
tiepoints, especially if correlation is used:  "tiny craters, small hummocks
(knolls), channel or stream intersections, albedo marks (if the two images
are contemporaneous), irregular markings similar in both images, even if quite
faint.  In general, any feature that is two-dimensional in the image plane
and similarly shaped in both images should work well.  Bad sites include:
anywhere on a linear feature, areas of nearly constant DN with only random
variations, and features distorted badly in one of the images due to a 
projection error."

10.0  HOW TO USE CORRELATION
Using correlation is using PICREG in an assisted mode where the program uses
cross-correlation to obtain the exact location of the right-side tiepoint.
It is the same thing that I have labelled 'semi-automatic' tiepoint gathering.
When correlation is used, the program uses a small window (square subset of
the image) centered around the cursor location on the left-hand side.  The 
program essentially searches around the expected location on the right side
for the window there that best matches the pixels in the window on the left-hand
side.  The size of the window is determined by the POWER parameter.  (See help
for the POWER parameter.)  Power=6 selects correlation mode using an area 
64 pixels square.  

This mode generally works well where the scale and the rotation of the two
images to be registered is the same. This mode makes PICREG easier to use,
since the user only needs to be a little accurate on selecting the very first
tiepoint and then can quickly select subsequent left-side tiepoints with
little accuracy and let the program determine the right-side tiepoint with
FFT cross correlation.

'INTE is recommended for precision.  Even if correlation does not work for
all prospective tiepoints, it generally can save you some time if you apply
it to the most prominent or distinct features first (for which correlation
is most likely to work well.)  Then if correlation goes astray for less distinct
features, you may have to turn it off for remaining tiepoints.  If correlation 
goes astray, i suggest you switch between 'NOPHASE and 'PHASE and try adjusting
the power parameter before giving up on correlation.  Frequently 'PHASE works
better then 'NOPHASE (which is the default).  The size of the correlation window
determines how far PICREG will search from the expected location on the right
hand side in looking for the best match to the left side correlation window.
(The expected location is determined by the offset between the previous left
and right tiepoints.) The size of the correlation window also determines how
many pixels of the image are used in comparing the left side and the right
side.   Generally users start with POWER=6, the default.  But having power
too large can increase the chances of the correlation going astray in looking
for the right side tiepoint.  If the feature you are using as a tiepoint is much
smaller than the size of the correlation window (default is 64 pixels square)
and correlation is going astray, try setting POWER so that the correlation
window is about the same size as the feature.  Otherwise the effect of the
background may outweigh the effect of the feature in the computation.  It may
be a good idea to keep POWER around 4 if there is a lot of rotational or scale
differences between the two images.

When correlation is used, PICREG displays the correlation matrix at the top
center of the display monitor screen (window).  The center of the correlation
matrix corresponds to the right tiepoint position before the correlation
optimization takes place.  The brightest spot in the matrix display corresponds
to the point chosen by the optimization.  If the matrix display does not
clearly show that one location is the brightest, the final right tiepoint
position should be checked.  If it does not look correct, it may be best to not
use that location for a tiepoint. 
.page
11.0  GUIDE FOR USING THE TPFORM PARAMETER
The TPFORM parameter is used to produce the second output file (the file
passed to TIECONM or one of the Geom programs.  The value of the TPFORM
parameter determines the post processing performed on the (raw) tiepoints
collected in PICREG.  The post processing is generally necessary because the
Geom programs require a grid of tiepoints (NVER rows of tiepoints with NHOR
tiepoints per row).  

For the case of TPFORM less than 9,
PICREG uses a least-squares polynomial fit to generate the grid from the raw
tiepoints.  The value of the TPFROM parameter specifies the type of polynomial
to use.  (See Help for the TPFORM parameter for a table that relates TPFORM
values to the type of geometric transformation which various polynomial types
will produce.)  The least squares fit generates a smooth transformation 
for the Geom program.  That way slight inaccuracies in some of the raw tiepoints
do not produce 'bumpy' or jagged distortions in the output from the Geom 
program.  This smoothing is generally desirable when trying to register 
spacecraft or satellite data.  Thus, except for the case of TPFORM=9, the raw
tiepoints selected in PICREG are not passed on to subsequent programs.

For appplications where the user wants the raw tiepoints to be passed on to
subsequent programs, TPFORM=9 should be specified.  Program TIECONM may then
be used to generate a fine grid of tiepoints that defines a transformation
which maps the raw tiepoints exactly the way they were specified in PICREG.
TPFORM=9 is recommended for registering imagery taken from airborne scanners.
(See Example 2 for a usage of TPFORM=9.)  TPFORM=9 was also found useful in
registering some spacecraft data with complex distortion, namely the IDA mosaic
images.

When users wish to produce a smooth transformation in registering the data,
the FIT parameter can be used to aid in selecting the TPFORM value.  (See
help for parameter FIT.)  The user may try various values of FIT and see
what (least-squares) residuals arise from trying to use various polynomial
types to fit the raw tiepoint data.  One goal in this process is to find a
value for FIT that gives a small residual.  This is a sign of a transformation
that fits the raw tiepoint data well.  (FIT reports the typical residual
and the tiepoint with the largest residual.  For your reference I will give
you Charlie Avis's answer to approximately what residual FIT=3 will
give (when there are no erroneous tiepoint pairs) as a signal that TPFORM=3
is a satisfactory transformation.  It is a typical residual of 0.5 pixels
and a largest residual of less than 2 or 3 pixels.  You may wish to adjust this
depending on the resolution and nature (source) of the data.)  Part of this
process is finding erroneous or dubious tiepoint pairs and either DELETE-ing
them or REDO-ing them.  The tiepoint pair with the largest residual is a good
one to re-examine.  If you are confident of the accuracy of this tiepoint
pair and want to give it even priority, look for other tiepoint pairs to
DELETE or REDO or try a different value of FIT.  

The SHOW keyword may be useful if the image geometry is uniform throughout
the image.  It keys off of tiepoint pair 1, so you may want to consider using
REDO to make a different tiepoint pair  be the first if the initial results
are not satisfactory.

Another goal is to use what you know about how the images were acquired to 
choose an appropriate FIT and TPFORM value.  For example, in a color 
registration application you may know that the two images were taken from
a spacecraft at almost the same time.  In this case you would expect the scale
to be the same and may expect that the geometric mapping between the images is
just an offset combined perhaps with a small rotation.  In this case as long as
FIT=1 gives a reasonably small residual, it makes sense to use TPFORM=1 even if
the residual from FIT=3 is smaller, figuring that any scale difference derived 
from the raw tiepoints is due to inaccuracies in the tiepoint selections.

In general, the residual will decrease as the constraints on the fit decrease.
For example, every transformation that is just a rotation plus an offset is a
particular type of first order fit.  Thus FIT=1 should nevver give a smaller
residual than FIT=3.  Also, every first order fit can be considered to be
a second order fit with the coefficients of the second order terms of the
polynomial being 0. Thus FIT=3 should never give a residual smaller than FIT=6.

Second and third order fits (FIT=6 and FIT=7) may give small residuals, but the
second and third order terms usually become larger near the edge of the images,
and the corresponding transformation might cause undesired distortions near the 
edge of the image.  Consequently, another goal in selecting the TPFORM value
is to use as low order a fit as will give a reasonable residual.

Here is at tip from Ron Alley.  Try FIT=3 and then FIT=6.  If they both say that
the same tiepoint pair is the worst and you are convinced that pair is OK, then
your set of tiepoints is probably fine.  If one of the edge pixels is the worst
for FIT=3 but is not the worst for FIT=6, then you probably need a second or
third order fit.

In typical usage, different values of FIT are tried and any tiepoint
DELETEing or REDOing is done until as FIT value is found that gives an 
acceptable residual.  Then this value is used as the TPFORM value.
The program then asks which Geom program you will use.  If you select GEOMA,
you are selecting a slower program, but GEOMA will give a geometric 
transformation that is continuous at the grid cell boundaries, which MGEOM
or LGEOM might not give in the case of complex distortions.  Currently
PICREG will do the same thing if you specify MGEOM or LGEOM.  PICREG will
generate the second output file and then produce the interactive prompt.
At this point the user typically exits from the program.


12.0  RESTRICTIONS
   1) Both input images must be in BYTE format.
   2) X displays with 8 bits per pixel are not yet supported.
.PAGE
13.0  EXAMPLES:

 1.     picreg inp=(in,ref) out=(newtp,par)
          .
        ( collect tiepoints interactively )
          .
        tpform=3
        'geoma
        exit
        geoma inp=in out=x parms=par

  This example shows 'in' being registered (geometricaly transformed) to the
  geometry of 'ref'.  'tpform=3' instructs PICREG to use a first-order-
  unconstrained least squares fit to generate (in file 'par') a rectangularly-
  lined-up grid of tiepoints from the tiepoints selected by the user.  This
  is done because the Geom programs, LGEOM, MGEOM, and GEOMA, require a grid
  of tiepoints.  This grid must contain multiple rows of tiepoints, with each
  row containing the same number of tiepoints.
.page
 2.     picreg inp=(in,ref) out=(newtp,par)
        cursor=3 power=6
        'inte  'phase
          .
        ( collect tiepoints interactively )
          .
        tpform=9
        'LGEOM
        exit
        tieconm out=parlgeom parms=par 'LGEOM NAH=30 NAV=30
        LGEOM INP=in out=y parms=parlgeom
   
  This example shows some precision tiepointing methods.  Cursor=3 facilitates
  both coarse and fine positioning by giving the cursor a square outline 
  with a dot in the center.  Power=6 selects correlation mode using
  an area 64 pixels square.  This mode generally works well where
  the scale and the rotation of the two images to be registered is the same.
  This mode makes PICREG easier to use, since the user only needs to be
  a little accurate on selecting the very first tiepoint and then can quickly 
  select subsequent left-side tiepoints with little accuracy and let the 
  program determine the right-side tiepoint with FFT cross correlation. (This
  could be called semi-automatic tiepoint gathering because the program does
  much of the work in obtaining the accurate results.) 'INTE causes the 
  program to do subpixel interpolation to an accuracy of about 1/10 of a 
  pixel. 'PHASE often prevents the FFT correlation from going astray.
  When tpform=9 is specified, the exact tiepoints selected are passed on
  for TIECONM.  (Other TPFORM values cause PICREG to do a least-squares fit
  on your tiepoints and to generate an evenly spaced grid based on
  the fit to the Geom program.)  
  The result is an output file 'y' from LGEOM which is registered to file 'ref'.
.page
 3.     gen b1 nl=256 ns=240
        gen b2 nl=256 ns=10 ival=239
        picreg (b2 b1) (t p)
        linear=(0,400)

    ...( Tell picreg that the left edge of b2 (left-hand side of screen) matches
        the right edge of b1 (right-hand side of screen) by selecting upper-left
        corner of b2 (left side) and upper-right corner of b1 (right side).
        Then select three more tiepoint pairs vertically beneath the first
        tiepoint pair on different lines.)...

        fit=5
        tpform=5
        'lgeom
        exit
        lgeom b2 b2mos size=(1,240,256,10) PARMS=p
        fastmos in=(b1 b2mos) out=bmos  size=(1,1,256,249)  OFF2=(1,240) 
        !
        gen b 256 249
        difpic (b bmos)
        write "Should get 0 differences."

This example shows a case used in the test procedure to demonstrate the
correct functionality of PICREG when used with LGEOM and FASTMOS to produce
a mosaic.  Although this is a fairly simple application that could be performed
in fewer steps, it is used to illustrate a general mosaicking method that can
be used on more difficult cases involving non-trivial image registration.
This example assumes that we want to mosaic images b1 and b2 in such a way that
b2 is appended to the right side of b1, with one pixel overlap in each line.
Because the IVAL of b2 is chosen appropriately, the final result should be
exactly the same as image b, which is GENed at the end.  In general mosaicking
has to proceed from left to right or top to bottom because the Geom programs
do not handle negative line and sample coordinates.  Thus in PICREG, image b1
is chosen as the reference since it is the image on the left in the mosaic.
The initial stretch prevents any part of the image from being too bright to
see the cursor.  Then tiepoints are chosen to tell PICREG that the left edge of
b2 (left-hand side of screen) matches the right edge of b1 (right-hand side of 
screen).  PICREG needs at least four tiepoints to work with. (They cannot
all have the same sample coordinate (they cannot all lie precisely on a straight
line or else PICREG has problems doing the least squares fit.)  In this case
we do not want to change the scale or rotation of b2 in the mosaic, so we 
specify TPFORM=5 to indicate that b2 is merely offset from b1.   In a case
where there was some nontrivial registration to do, TPFORM=3 would be a more
likely choice.  In LGEOM, the SIZE field is used to generate b2mos without
wasting disk space on zero-padding b2mos on the left.  The OFF2 parameter
sets the position for b2mos accordingly.  The SIZE field in FASTMOS needs to
be entered to tell FASTMOS the desired output size.
.PAGE
14.0  OPERATION

 PICREG will display both images in a split-screen manner, the first
 input on the left and the second input on the right.  The user positions
 the cursor(s) over identical features in each image and enters these
 tiepoints by hitting <cr>.  Tiepoint positions can be obtained directly
 from the cursored location or can be optimized from this initial 
 position with two methods.  First, the AREA parameter invokes a method
 using the extreme positions of histograms computed from the sums of rows
 and columns of pixels.  Second, a FFT cross-correlation technique can
 be invoked with the CORR or POWER parameters.  
.PAGE 
 The FFT expression is:
               -1         *
	cc = ff   [ ft(p1) x ft(p2) ]
 
 where cc is the correlation map, ft is Fourier Transform, p1 and p2 are
 the in and ref picture areas, and * is complex conjugate.  The location
 of the peak of cc determines the final tiepoint position.  
.PAGE
 When correlation is used, PICREG displays the correlation matrix at the top
 center of the display monitor screen.  The center of the correlation matrix
 corresponds to the right tiepoint position before the correlation optimization
 takes place.  The brightest spot in the matrix display corresponds to the
 point chosen by the optimization.  If the matrix display does not clearly
 show that one location is the brightest, the final right tiepoint position
 should be checked.  If it does not look correct, it may be best to not use
 that location for a tiepoint.
.PAGE
 PICREG writes the locations of these raw tiepoints to the first output
 file, updating the file for each new point.  PICREG will draw a cross 
 in the graphic plane) at each tiepoint location visible in the displayed 
 image and label the cross with the correspoinding tiepoint number.  The 
 raw tiepoints may be edited using the DELETE or REDO keywords.  
.PAGE
 The LINEAR keyword may be used to stretch (contrast enhance) the image data 
 displayed on the  screen.  Currently there is no provision for stretching the
 left and right  sides independently.  If the two input images require separate
 stretches,  the stretches must be performed prior to entering PICREG. 
 The FIT keyword allows the user to fit various surfaces to the raw 
 tiepoints.
 The TPFORM keyword will re-format the points and write them 
 to the second  output file.  
 PICREG will print the message "Enter parameters:" whenever it is ready to 
 accept new keywords or a new tiepoint location.
.PAGE
 PICREG is currently designed to operate on any display device that has
 four Image Memory Planes available.  This includes workstations running
 the X window system as well as display devices such as the DeAnza and IVAS.
 This device must be allocated to the
 user prior to the invocation of PICREG.  The nominal mode of cursoring
 locations is to have one trackball or nmouse at your user station.
.page
 However, a  zero-trackball mode may be selected by specifying 'NOTRACK
 on the PICREG command line.  Thereafter,
 <cr> will not pick a point (as in the nominal mode).  It will instead
 initiate a loop within which the user moves the cursor via the keyboard.
 Considering the H key as home (current location), the following keys are
 used:                    Y (UP)

                 G (LEFT)   H    J (RIGHT)

                         N (DOWN)

 If the H is keyed, the step size of the movement is changed by a factor of
 10.  The step size cycles from 100 to 10 to 1 to 10 and then back to 100 again.
.PAGE
15.0  REFERENCES

   1) Yagi, Gary M., "MIPL Image Registration and Design Study, Preliminary
      Report", 1987.
   2) LaVoie, Susan, et al.  "VICAR User's Guide", Version 2, June 1989.
      (Sections 6.2.1 and 7.1.1.5 have information on obtaining help using 
      <ESC><ESC> when the user is at the "Enter parameters:" prompt.  Note that
      Wyse terminals need to be set to VT100 mode in the terminal setup to
      generate <ESC>.  The user can find an overview of VICAR programs in
      Appendices 10.2 and 10.3)
   3) Power, M. A., "Stereophotogrammetry using STERGEN and STERMAP, JPL, 1976.
.page
16.0  REVISION HISTORY:
12/94 The prompt for the number of trackballs has been removed.  The default is
   for there to be one trackball or mouse.  If there is none, you can select
   the zero-trackball mode by entering 'NOTRACK on the PICREG command line.
   The tiepoint numbering is now left justified and thus will be closer to
   the tiepoint.  Also the TEXTSIZE parameter has been added to allow the user
   to select a comfortable size for the device being used.  By setting
   TEXTSIZE to 1, you can effectively suppress the numbering if it gets in the
   way for a while.

 PICREG Originally Written by:  J.J.Lorre, 28 Apr. 1978
 Current Cognizant Programmer:  Steve Pohorsky
 Ported to UNIX:                Steve Pohorsky  8-6-93

.PAGE
17.0 PRECISION: 
  When correlation is used with 'INTE, tiepoint locations are precise to 0.1
  pixels unless the correlation fails.  When 'PRINT is specified, FIT displays a
  number of values from the least-squares calculation.  The RESIDUALS displayed
  are precise to 0.1 pixels, since they are derived from the tiepoint locations.
  The values of MEAN ERROR OF THE UNIT WEIGHT that can be expected on different
  MIPS-supported machines shall not differ by more than 1 in five significant
  digits. 
.LEVEL1
.VARIABLE INP
 INPUT FILES - (IN,REF,OLDTP)
.VARIABLE OUT
 OUTPUT FILES - (NEWTP,PARMS)
.VARIABLE SIZE
 VICAR SIZE FIELD - IGNORED
.VARIABLE SL
 STARTING LINE - IGNORED
.VARIABLE SS
 STARTING SAMP - IGNORED
.VARIABLE NL
 NUMBER OF LINES - IGNORED
.VARIABLE NS
 NUMBER OF SAMPS - IGNORED
.VARIABLE EXIT
 KEYWORD-TERMINATES PROGRAM
.VARIABLE PGM
 GEOM PROGRAM TO BE USED
.VARIABLE NOTRACK
 SELECTS ZERO-TRACKBALL MODE
.VARIABLE U
 MOVE BOTH WINDOWS UP
.VARIABLE D
 MOVE BOTH WINDOWS DOWN
.VARIABLE L
 MOVE BOTH WINDOWS LEFT
.VARIABLE R
 MOVE BOTH WINDOWS RIGHT
.VARIABLE U1
 MOVE LEFT WINDOW UP
.VARIABLE U2
 MOVE RIGHT WINDOW UP
.VARIABLE D1
 MOVE LEFT WINDOW DOWN
.VARIABLE D2
 MOVE RIGHT WINDOW DOWN
.VARIABLE L1
 MOVE LEFT WINDOW LEFT
.VARIABLE L2
 MOVE RIGHT WINDOW LEFT
.VARIABLE R1
 MOVE LEFT WINDOW RIGHT
.VARIABLE R2
 MOVE RIGHT WINDOW RIGHT
.VARIABLE NHOR
 # COLUMNS IN OUTPUT GRID
.VARIABLE NVER
 # ROWS IN OUTPUT GRID
.VARIABLE REDO
 TIEPOINT # TO REDO
.VARIABLE DELETE
 RANGE OF TIEPOINTS TO DELETE
.VARIABLE AREA
 AREA MODE BOX SIZE
.VARIABLE CONV
 MIN OR MAX OF AREA SOUGHT,
 FOR 'AREA' MODE
.VARIABLE POWER
 CORRELATION SIZE = 2**POWER
.VARIABLE MODE
 KEYWORD: CORR OR POINT:
 CORRELATE OR ACCEPT POINT
.VARIABLE HIPASS
 USE HIGH PASS FILTER
 IN 'CORR' MODE?
.VARI PHASE
 USE PHASE CORRELATION?
.VARIABLE FIT
 SURFACE TO FIT TO TIEPOINTS
.VARIABLE TPFORM
 FIT AND FORMAT TIEPOINTS
.VARIABLE USE
 # TIEPOINTS FOR TPFORM=8
.VARIABLE INTERP
 INTERPOLATE IN 'CORR' MODE?
.VARIABLE TEXTSIZE
 HEIGHT OF TIEPOINT NUMBERS
.VARIABLE ZOOM
 ZOOM BOTH PIX.
.VARIABLE Z1
 ZOOM LEFT PIC
.VARIABLE Z2
 ZOOM RIGHT PIC
.VARIABLE STRETCH
 LINEAR CONTRAST STRETCH LIMITS
.VARI LINEAR
 SYNONYM FOR 'STRETCH'
.VARIABLE SL1
 SET START LINE OF DISP(LEFT)
.VARIABLE SS1
 SET START SAMP OF DISP(LEFT)
.VARIABLE SL2
 SET START LINE OF DISP(RIGHT)
.VARIABLE SS2
 SET START SAMP OF DISP(RIGHT)
.VARIABLE MINL
 MIN LINE OF OUTPUT GRID
.VARIABLE MINS
 MIN SAMP OF OUTPUT GRID
.VARIABLE MAXL
 MAX LINE OF OUTPUT GRID
.VARIABLE MAXS
 MAX SAMP OF OUTPUT GRID
.VARIABLE HOME
 DISP UPPER LEFT CORNERS
.VARIABLE PRINT
 PRINT LEAST SQUARES INFO
.VARIABLE DSTAT
 PRINT DISPLAY STATS
.VARIABLE LOCATE
 TIEPOINT # TO DISPLAY ABOUT
.VARIABLE SHOW
 DISPLAY TIEPOINT OFFSET VECTORS
.VARIABLE CURSOR
 SELECTS CURSOR SHAPE
.VARIABLE POSITION
POSITION=(ld,sd)
Use (ld,sd) as (image) line 
and sample cursor coordinates.
.LEVEL2
.VARIABLE INP
 Vicar labelled input files (IN,REF,OLDTP)
	IN - the image to be registered..byte format 
	REF - the image used as reference..byte format
 	OLDTP - optional file containing the raw tiepoints from a
 		previous unfinished run of PICREG.  See the
		format of NEWTP below.
.variable OUT
 Vicar labelled output files (NEWTP,PAR):

  NEWTP - file containing the raw tiepoint values which is
          updated for each tiepoint collected.  Up to 10
          records, each of 3200 bytes of real*4 data. The order of the
          tiepoints is  Lref,Sref,Lin,Sin.
          The purpose of this file is to save tiepoints for
          possible future runs of PICREG.  This file is NOT
          used as an interface to any other program!

          WARNING: immediately upon startup, PICREG initializes  
          this dataset to contain 0 tiepoints.  Therefore, the   
          user should be careful not to re-enter a previous NEWTP
          file in this position, or the accumulated tiepoints will
          be lost!

  PAR -   optional file to receive the formatted parameter file
          for the chosen Geom program.  It will be a Vicar2
          parameter file in XVPOUT format.

.variable SIZE
 Vicar SIZE field - ignored by PICREG
 default = (1,1,0,0)

.variable SL
 Starting line of  size field
 default = 1

.variable SS
 Starting samp of size field
 default = 1

.variable NL
 Number of lines of size field
 default = 0

.variable NS
 Number of samps of size field
 default = 0

.variable EXIT
 Interactive keyword which terminates the program.  Note that the apostrophe
 can be ommitted.  Both 'EXIT and EXIT are valid.

.variable PGM
 Interactive parameter which specifies, in response to a prompt,
 the Geom program which will be run on the output parameters.
 Valid = GEOMA, LGEOM, MGEOM.
 Default=GEOMA
 
.variable U
 Specifies that the display window for both images move up by the
 indicated number of pixels.
 Default=0.

.variable D
 Specifies that the display window for both images move down by the
 indicated number of pixels.
 Default=0.

.variable L
 Specifies that the display window for both images move left by the
 indicated number of pixels.
 Default=0.

.variable R
 Specifies that the display window for both images move right by the
 indicated number of pixels.
 Default=0.

.variable u1
 specifies that the display window for the left image move up by the
 indicated number of pixels. default=0.
.variable u2
 specifies that the display window for the right image move up by the
 indicated number of pixels. default=0.
.variable d1
 specifies that the display window for the left image move down by the
 indicated number of pixels. default=0.
.variable d2
 specifies that the display window for the right image move down by the
 indicated number of pixels. default=0.
.variable l1
 specifies that the display window for the left image move left by the
 indicated number of pixels. default=0.
.variable l2
 specifies that the display window for the right image move left by the
 indicated number of pixels. default=0.
.variable r1
 specifies that the display window for the left image move right by the
 indicated number of pixels. default=0.
.variable r2
 specifies that the display window for the right image move right by the
 indicated number of pixels. default=0.
.variable sl1
 specifies that the display window for the left image begin at the
 indicated pixel. default=1.
.variable sl2
 specifies that the display window for the right image begin at the
 indicated pixel. default=1.
.variable ss1
 specifies that the display window for the left image begin at the
 indicated pixel. default=1.
.variable ss2
 specifies that the display window for the right image begin at the
 indicated pixel. default=1.
.variable home
 specifies that both display windows are to begin at (1,1).
.variable locate
 will move both windows so that the tiepoint indicated is located in
 the center of each window.  This is true unless the tiepoint is located near 
 the left edge, or near the top edge of the image.  In this case, the image 
 will be displayed starting at sample 1, or line 1, and the tiepoint will
 be at the corresponding sample or line. default=1
.VARIABLE TEXTSIZE
 Height (in pixels) of tiepoint numbers on screen.  The program starts with
 a value of 18 if the screen size is bigger than 512 by 512 or a value of 12
 if the screen size does not exceed 512 by 512.  1<= textsize<=99.  The user
 can effectively turn off the  numbering by setting TEXTSIZE to 1 as long as
 desired. 
.variable zoom
 Indicates an integer which specifies the magnification or reduction 
 applied to both images.  If the value is >0, the pic is entlarged by that
 factor.  If it is <0, the pic is reduced by the factor 1/value.  
 Compressions can facilitate fewer window movements, give a better idea
 of where the user is in the file , and display the tiepoint in a more global
 sense.   zoom=0 is interpreted as no zoom, the same as zoom=1.
 The maximum value for zoom is 8.  There is no minimum value for zoom other
 than the limit of -2**31 for valid integers.  This means the compression
 may be as large as is useful.  The default for zoom is 1.
.variable z1
 the left image will be zoomed by this factor.  default=1
.variable z2  
 the right image will be zoomed by this factor.  default=1
.variable stretch
 stretch=(lo,hi)
 specifies that both images will have a linear contrast stretch applied.
 lo goes to 0 dn,  hi goes to 255 dn.  default=(0,255).
 (There is no restriction keeping hi from being greater than 255, but this is
 generally not done since it makes the image display darker.  It would not
 allow the image files to contain DNs outside the range 0 to 255 since the
 images must have byte format.)
 Currently there is no provision for stretching the left and right
 sides independently.  If the two input images require separate stretches,
 the stretches must be performed prior to entering PICREG.
.variable linear 
 linear=(lo,hi)
 specifies that both images will have a linear contrast stretch applied.
 lo goes to 0 dn,  hi goes to 255 dn.  default=(0,255).
 (There is no restriction keeping hi from being greater than 255, but this is
 generally not done since it makes the image display darker.  It would not
 allow the image files to contain DNs outside the range 0 to 255 since the
 images must have byte format.)
 Currently there is no provision for stretching the left and right
 sides independently.  If the two input images require separate stretches,
 the stretches must be performed prior to entering PICREG.
.variable dstat
 will cause the sl, ss, and zoom factor to be printed for each image.
.variable mode
 valid=(corr,point)
 determines whether PICREG will accept the user cursor locations as the
 final tiepoint values (point) or try to optimize the locations using a
 fft cross-correlation technique.  default=point.
 If MODE=CORR is selected, POWER defaults (is set) to 6.
.variable power
 Tells PICREG to use fft correlation.  The value indicates the 
 the size of the correlation window.  size= 2**power.  The default is 0,
 which means to not use fft correlation, unless MODE=CORR (or 'CORR) is 
 selected.  If MODE=CORR is selected, POWER defaults (is set) to 6.
 Thus, when fft correlation is selected, the correlation window size is
 64 by 64 (pixels) unless explicitly changed using the POWER parameter.
.vari HIPASS
 VALID=(HPF,NHPF)
 determines whether, in the fft correlation, the vertical and horizontal
 frequencies which lie on axes passing through the DC term are zeroed 
 out (hpf) or not (nohpf).  default=hpf.
.vari PHASE
 This keyword determines whether or not phase correlation is used when
 correlating tiepoints.  In phase correlation, the complex fourier
 transform coefficients are divided by their amplitudes before computing
 the correlation function.
.variable interp
 valid=(inte,noin)
 determines whether, in the fft correlation, the centroid of the cor-
 relation peak is determined to nearest-pixel (noin) or sub-pixel
 (inte) accuracy.  If 'inte is specified, PICREG tries to repeat
 the correlation until it converges to within .1 pixels.
 If 'noin is specified, PICREG tries to repeat
 the correlation until it converges to within 1.1 pixels.
 default=noin.  (See also Example 2 in the main HELP for PICREG.)
.variable area
 invokes the area mode of correlation and supplies the size of the box
 used. In this mode, PICREG will compute two buffers containing the sums
 of rows and of columns of dn's in an area about the tiepoint location.  The 
 final positions will be set to the minimum or maximum positions of
 each buffer.  default=20.
.variable conv
 valid=(min,max)
 indicates whether, in the area mode, to pick the maximum or the minimum
 positions of the buffers.  default=min.
.variable redo
 specifies which tiepoint number is to be deleted and redone.  default=0
.variable delete
 Specifies a range of tiepoint numbers to be deleted.  default=(0,0).
 Note that any following tiepoints will be renumbered. 
.variable show
 results in a display on the left image graphics plane of tiepoint displacements
 from their location in the left image relative to their location in the 
 right image.  This is done relative to the first tiepoint ( it will always show
 no displacement) and is only done for those tiepoint presently displayed.
.variable fit
 picks a type of least squares surface, and fits it to the current set of
 tiepoints.  The meanings of the values are:
	1. rotation + offset
 	2. rotation + offset + scale
	3. first order unconstrained
	4. scale + offset
	5. offset
	6. second order unconstrained
	7. third order unconstrained
 PICREG will print the mean error and the number and error of the worst
 fitting point. 
 This keyword is used to check for tiepoint location errors and to test how well
 a specifc surface fits prior to using that surface to generate an output
 grid of regularly spaced tiepoints for use with a Geom program.
 1<= fit <= 7.   default = 1
.variable tpform
 This keyword formats the second output file for later use with TIECONM,
 GEOMA, LGEOM, or MGEOM.  Value values and their meanings are:
	1-7. The corresponding surface as described under the fit parameter
	     (also below) is used to generate a nhor by nver grid of
 	     regularly spaced tiepoints in GEOMA, LGEOM, or MGEOM format.
	     These tiepoints, along with all necessary parameters are then
	     written to the second output file.
		 The meanings of the TPFORM values in the range 1 to 7 are:
			1. rotation + offset
		 	2. rotation + offset + scale
			3. first order unconstrained
			4. scale + offset
			5. offset
			6. second order unconstrained
			7. third order unconstrained
	8.   The region of the reference picture which is encompassed
	     by the output tiepoint network will be samlpled in a nhor by
	     nver grid.  At each grid intersection, a transformation will
	     be computed by least squares using the relations:
			y1 = ax + by + c
			x1 = dx + ey + f
	     between the reference pic (x,y) and the input picture (x1,y1)
	     using the n closest tiepoints to that grid location. (n is set
	     using the USE parameter.)  
	9.   This invokes the TIECONM option.  No re-sampling is done in
 	     this case.  Instead, the raw tiepoints and all necessary TIECONM
	     parameters are written to the second output.  This parameter
	     file can then be used with TIECONM which will in turn format
	     the tiepoints for use with GEOMA, LGEOM or MGEOM.  The TIECONM
	     triangulation method for surface fitting is useful in cases
	     where an image has areas of significant distortion.
 In any case, the user will be prompted for which Geom program he plans to
 use.  If GEOMA is specified, the output grid will be transformed into
 triangles.  Applicable keywords (nhor,nver,minl,mins,maxl,maxs,use) should
 be specified prior to or on the same line as the tpform keyword.
.variable nhor
Specifies the number of columns of tiepoints in the output grid, for the case
of TPFORM = 6, 7, or 8.  This is the number of tiepoints in a horizontal line
of the output tiepoint grid. 

If TPFORM <= 5 is chosen, then this parameter is ignored, and NHOR=2 is
used (since 4 points suffice to determine a first-order transformation).

Note that if GEOMA format is chosen for the tiepoints, then the value
of NHOR is almost doubled (NHOR = 2*NHOR-1) in order to make triangular
tiepoint areas.

Default=20.
.variable nver
 Specifies the number of rows of tiepoints in the output grid, for the case
 of TPFORM = 6, 7, or 8.  This is the number of tiepoints in a vertical line
 of the output tiepoint grid. 

 If TPFORM <= 5 is chosen, then this parameter is ignored, and 
NVER=2 is
 used (since 4 points suffice to determine a first-order transformation).

 Default=20.
.variable minl
 minimum line number of the output grid tiepoints. default is based on raw 
 tiepoints.
.variable mins
 minimum samp number of the output grid tiepoints. default is based on raw 
 tiepoints.
.variable maxl
 maximum line number of the output grid tiepoints. default is based on raw 
 tiepoints.
.variable maxs
 maximum samp number of the output grid tiepoints. default is based on raw
 tiepoints.
.variable print
 valid=(print,noprint)
 enables the printing of least squares fit information when the fit is
 specified and results in a printout of all raw tiepoint locations upon exit
 default=noprint.
.variable use
 the number of tiepoints used to determine each output tiepoint location. 
 Applies to tpform=8 only.  4<= use <= 10  . default=4
.variable cursor
 selects the shape of the cursor by specifying its number.  The number
 scheme is installation or display-type dependent, and may be changing.  
 At present, the following are available on the DeAnza:

	        .                |               -----
	1 =   . . .       2 = ---|---       3 = |  .  | 
                .                |              |_____|

                                                               ______
            ||                                \  /            |      |
    4 =   ==  ==     5 = complement       6 =  \/        7 =  |------|
            ||             of 4                /\             |______|    
                                              /  \                       
 The cursor shapes for values 1 to 6 are also valid under the X Window System.
.VARIABLE POSITION
POSITION values are used instead of the trackball cursor position.
POSITION values are in terms of image line and sample coordinates.
This parameter is useful in testing PICREG since it bypasses the trackball
and allows testing to be repeatable.
$ Return
$!#############################################################################
$Test_File:
$ create tstpicreg.pdf
procedure
!  You need to display on a user-area workstation or one with four 
!     image planes.
!  TO RUN ON UNIX or Alpha, 
!   FIRST TYPE
!    use xwa0   
!   OR select another VRDI device if desired.
!THEN TYPE   tstpicreg

refgbl $syschar
refgbl $echo
DEFGBL INPIC1   TYPE=STRING
DEFGBL INPIC2   TYPE=STRING
REFGBL INPIC1
REFGBL INPIC2
LOCAL  DIR      TYPE=STRING 
body
let _onfail="continue"
let $echo="yes"

if ($syschar(1) = "UNIX")
   LET DIR  ="/project/test_work/testdata/mipl/vgr/"
else 
   LET DIR  ="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
end-if

LET INPIC1 = "&DIR"//"picreg.j"
LET INPIC2 = "&DIR"//"picreg.j2"

use xwc0

write "See section 17.0 of the help file for information on precision"
write "   and the expected differences."
enable-script tstpicreg.scr
end-proc
$!-----------------------------------------------------------------------------
$ create tstpicreg.scr
!this is a script file to test the program PICREG
!must have allocated a display device which has a trackball or mouse
!tablet before running this.
! To make this test repeatable and verifiable, the POSITION parameter
! is used to override the position of the trackball position.
! The two images used are identical.  Thus the tiepoint coordinates
! should be the same from both images.

picreg (&INPIC1,&INPIC2) t
d=50 r=100
l=50 u=30
r1=100 r2=50 d1=50 d2=70
l1=80 l2=40 u1=40 u2=50
'home
sl1=50 ss1=50 sl2=100 ss2=100
'home
z1=3 z2=-2
zoom=1
linear=(50,110)
stretch=(40,120)
'dstat
'point
POSITION=( 185, 61)
POSITION=( 185, 61)
'hpf 'corr 'noin power=5
POSITION=( 100, 56)
POSITION=( 100, 56)
'nohpf 'inte
POSITION=( 156, 54)
POSITION=( 156, 54)
'min area=20
POSITION=( 80,40 )
POSITION=( 80, 40)
'max CURSOR=4
POSITION=( 81, 235)
POSITION=( 81, 235)
'point redo=2 CURSOR=1
POSITION=( 168, 117)
POSITION=( 168, 117)
delete=(1,3)
POSITION=( 140, 136)
POSITION=( 140, 136)
POSITION=( 57, 221)
POSITION=( 57, 221)
POSITION=( 91, 198)
POSITION=( 91, 198)
POSITION=( 181, 242)
POSITION=( 181, 242)
POSITION=( 168, 112)
POSITION=( 168, 112)
POSITION=( 168, 100)
POSITION=( 168, 100)
POSITION=( 168, 88)
POSITION=( 168, 88)
POSITION=( 168, 76)
POSITION=( 168, 76)
POSITION=( 156, 54)
POSITION=( 156, 54)
'show
locate=3
fit=1
fit=2
fit=3
fit=4
fit=5
fit=6
fit=7
'exit
!USAGE
!verify tiepoint file 
list t (1,1,1,96)
!enter PICREG with old tiepoint file and geom parameter file output
! Add a new tiepoint that is slightly wrong to make fit=3 info not infintessimal
picreg (&INPIC1,&INPIC2,t) (t2,g)
nhor=10 nver=10 minl=50 mins=50 maxl=350 maxs=300 use=4 TEXTSIZE=21
POSITION=( 185,161)
POSITION=( 186,162)
TEXTSIZE=1
TEXTSIZE=7
TEXTSIZE=12
'print fit=3
'noprint TPFORM=1
'mgeom
TPFORM=2
'lgeom
TPFORM=3
'geoma
'exit
!  a new test to demonstrate PICREG working together with LGEOM and FASTMOS.
!  See example 3.
gen b1 nl=256 ns=240
gen b2 nl=256 ns=10 ival=239
!
! ( Tell picreg that the left edge of b2 (left-hand side of screen) matches
!   the right edge of b1 (right-hand side of screen) by selecting upper-left
!   corner of b2 (left side) and upper-right corner of b1 (right side).
!   Then select three more tiepoint pairs vertically beneath the first
!   tiepoint pair on different lines.
!
picreg (b2 b1) (t p)
linear=(0,400)
POSITION=(   1,   1)
POSITION=(   1, 240)
POSITION=( 256,   1)
POSITION=( 256, 240)
POSITION=( 100,   1)
POSITION=( 100, 240)
POSITION=( 200,   2)
POSITION=( 200, 241)
fit=5
tpform=5
'lgeom
exit
lgeom b2 b2mos size=(1,240,256,10) PARMS=p
fastmos in=(b1 b2mos) out=bmos  size=(1,1,256,249)  OFF2=(1,240) 
!
gen b 256 249
difpic (b bmos)
write "Should get 0 differences."
!
gen b1 nl=256 ns=240
gen b2 nl=256 ns=10 ival=239
!
! ( Tell picreg that the left edge of b2 (left-hand side of screen) matches
!   the right edge of b1 (right-hand side of screen) by selecting upper-left
!   corner of b2 (left side) and upper-right corner of b1 (right side).
!   Then select three more tiepoint pairs vertically beneath the first
!   tiepoint pair on different lines.  Then some bogus tiepoints to
!   test FIT=6 and 7 with 'PRINT for FR 89205.
!
picreg (b2 b1) (t p)
linear=(0,400)
'print
POSITION=(   1,   1)
POSITION=(   1, 240)
POSITION=( 256,   1)
POSITION=( 256, 240)
POSITION=( 100,   1)
POSITION=( 100, 240)
POSITION=( 200,   2)
POSITION=( 200, 241)
POSITION=( 256,  11)
POSITION=( 256, 240)
POSITION=( 100,  11)
POSITION=( 100, 240)
POSITION=( 200,  12)
POSITION=( 200, 241)
POSITION=( 256,  11)
POSITION=( 256, 240)
POSITION=( 100,  21)
POSITION=( 100, 240)
POSITION=( 200,  22)
POSITION=( 200, 241)
POSITION=( 201,  12)
POSITION=( 200, 241)
fit=6
tpform=6
'lgeom
fit=7
tpform=7
'lgeom
tpform=9
'lgeom
exit

! lwk - 28apr2010
! removed these from last test as they caused pgm to crash:
!POSITION=( 257,  11)
!POSITION=( 256, 240)
!POSITION=( 101,  21)
!POSITION=( 100, 240)
!POSITION=( 201,  22)
!POSITION=( 200, 241)

!end
$ Return
$!#############################################################################
