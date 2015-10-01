$!****************************************************************************
$!
$! Build proc for MIPL module sargon
$! VPACK Version 1.8, Friday, April 12, 1996, 10:56:57
$!
$! Execute by entering:		$ @sargon
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
$ write sys$output "*** module sargon ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sargon.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
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
$   if F$SEARCH("sargon.imake") .nes. ""
$   then
$      vimake sargon
$      purge sargon.bld
$   else
$      if F$SEARCH("sargon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sargon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sargon.bld "STD"
$   else
$      @sargon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sargon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sargon.com -
	-s sargon.f vsort.f usort.f relevant_dsubs.f -
	-i sargon.imake -
	-p sargon.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sargon.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C**********************************************************************
C
      SUBROUTINE MAIN44

C-----PROGRAM SARGON

C  12-APR-96  ...OAM...  Modified sargon.imake to run on Unix.  Fix bugs 
C                        on variables HBL HBH MAXDN, and MINDN. FR 8902.
C  10-DEC-93  ...DDK...  Made application portable to UNIX.
C  13-JUL-92  ...FFM...  Modidifed routine OPRATE to handle DN outside of
C                        proper range 
C  14-APR-89  ...TCG...  Trap more bad inputs (no int params) which cause
C			 problems with all the rest of the commands.
C			 Add XVEACTION call and remove all XVCHECKs.
C  14-FEB-89  ...TCG...  Trap bad input (constants = 0.0) which cause 
C                        problems with divide but shouldn't be allowed for
C                        ADD, SUB, or MUL either.  Fixed bug in SORTX.
C                        Fixed bug in STRETCH when no params are input.
C  29-SEP-87  ...SP....  ADDED CHECK FOR VALID COMMAND AFTER HALFWORD DATA
C                        RANGE IS ENTERED.  ADDED READONLY PARAMETER IN
C                        OPEN STATEMENT IN SUBROUTINE IDIPARM TO KEEP
C                        UP TO DATE WITH VERSION OF IDIPARM IN IDX.  NOTE THAT
C                        THIS IS THE CLASSIC PROBLEM OF HOW IT IS DIFFICULT
C                        TO KEEP MORE THAN ONE COPY OF A PIECE OF CODE AND
C                        KEEP ALL VERSIONS UPDATED.
C  31-JAN-86  ...HBD...  FIXED BUGS, ADDED STATS/PCUR/RCUR/CIRC KEYWORDS
C  12-JUL-85  ...JHR...  CONVERTED TO VICAR2
C  11-MAY-84  ...CCA...  CONVERTED TO VAX
C   1-MAR-82  ...CCA...  NEW PROGRAM

C-----THIS PROGRAM IS AN INTERACTIVE AREA ALTERING PROGRAM
C-----IT WILL APPLY A SPECIFIED FUNCTION TO THE POLYGONAL
C-----AREA DEFINED BY CURSOR SELECTED VERTICIES.
C-------------------------------------------------------------------
C-----FUNC = 0       INTERPOLATE
C-----       1       MULTIPLY
C-----       2       ADD
C-----       3       SUBTRACT
C-----       4       DIVIDE
C-----       5       SET TO DN
C-----       6       ZERO OUT
C-----       7       CALC. MEAN AND STANDARD DEVIATION, MIN & MAX DN
C-----       8       COPY INPUT
C--------------------------------------------------------------------
      IMPLICIT INTEGER (A-Z)

      INCLUDE 'fortport'
      COMMON/C/IUNIT,OUNIT,ICOD,HBL,HBH,FUNC,RADI
      COMMON /D/ NL,NS,NLDS,NSDS
      COMMON /E/ PTBUF,SEGM,SORT
      COMMON /F/ LBUF,DBUG,NVERT,MINDN,MAXDN,PERC
      COMMON /G/ SL, SS
      COMMON /H/ DUNIT

      INTEGER OUNIT
      LOGICAL*1 LBUF(20000)
      CHARACTER*200 MSG
      INTEGER RAW(256),OFF(256),STR(256)
      REAL V(2,26)
      INTEGER SL,SS
      INTEGER*2 PTBUF(3,4000),SEGM(3,2000),SORT(2,4000)

      REAL    RARG(2),SLOPE,OFST,CONST
      INTEGER IARG(2),STAT,CMODE,SECTION
      INTEGER X(20001),Y(2001),L,S,XDSGRAPH,XDGCOLOR
      INTEGER XDIMFILL,XDCLOCATION,XDIPOLYLINE,XDLWRITE,XDGLINIT
      CHARACTER*8 FORMT
      BYTE BDN,BIDN,BMDN 

C      EQUIVALENCE (IPARM,RPARM)

      CALL XVMESSAGE(' SARGON version April 1996',' ')
C
C        SET DEFAULTS AND INITIALIZE
C
      MINDN = -9999
      MAXDN = 32768
      FUNC = 0
      DBUG = 0
      PERC = 100
      RADI = 1000000
      REDISP = 0
      HBL = 0
      HBH = 255
      CMODE = 0
      SL = 1
      SS = 1
      SLDS = 1
      SSDS = 1
      ATDL = 0
      ATDS = 0
      DO I=1,256
         RAW(I) = I - 1
         STR(I) = RAW(I)
         OFF(I) = 0
      END DO
C
	CALL XVEACTION( 'SA', ' ' )
C
C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,' ')
C
C        GET SIZE INFORMATION AND DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'NL',NLI,'NS',NSI,'FORMAT',FORMT,' ')
      IF(FORMT.NE.'BYTE'.AND.FORMT.NE.'HALF') THEN
         CALL XVMESSAGE('SARGON ACCEPTS BYTE AND HALFWORD DATA ONLY',
     .			' ')
         CALL ABEND
      END IF
      IF(NSI.GT.10000) THEN
         CALL XVMESSAGE(' INPUT SAMPLE SIZE EXCEEDS BUFFER SIZE',' ')
         CALL ABEND
      END IF
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NL',NLI,'U_NS',NSI,' ')
C
      NL=NLI
      NS=NSI
      ICOD=1
      IF(FORMT.EQ.'HALF') ICOD=2
      LIN1 = 1
      LINNL = NL
      SAM1 = 1
      SAMNS = NS
      CALL XVMESSAGE(' COPYING INPUT TO OUTPUT',' ')
C
C	QPRINT(..,24)
C
      DO I=1,NL
         CALL XVREAD(IUNIT,LBUF,STAT,'LINE',I,' ')
         CALL XVWRIT(OUNIT,LBUF,STAT,'LINE',I,' ')
      END DO
C
C        RE-OPEN OUTPUT FOR UPDATE
C
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE',' ')
C
C        OPEN AND INITIALIZE DISPLAY DEVICE
C
      CALL OPEN_DEVICE( DUNIT )
      CALL CONFIGURE_DEVICE( MAXL, MAXS, DUNIT )
      CALL BW_MODE(DUNIT)
      CALL AUTOTRACKING_MODE(.TRUE.,DUNIT)
      NSDS = 512
      NLDS = 512
      MAXSL = NL - NLDS + 1
      IF(NL .LT. NLDS) MAXSL = 1
      MAXSS = NS - NSDS + 1
      IF(NS .LT. NSDS) MAXSS = 1

C
C        INITIAL DISPLAY
C
5     IF(FORMT.EQ.'HALF') THEN
         CALL XVMESSAGE(' HALFWORD DATA, GIVE INTEGER DN RANGE',' ')
         CALL XVINTRACT('IPARAM','(USE KEYWORD RANGE)')
         CALL XVIPARM('RANGE',IARG,ICNT, IRDEF,2)
         CALL XVIPARM('EXIT',IARG,ICNT, IEDEF,0)
         IF    ( IRDEF.EQ.0 )  THEN 
            HBL = IARG(1)
            HBH = IARG(2)
            IF ((HBL.EQ.538976288) .OR. (HBH.EQ.538976288)) THEN
  	      CALL XVMESSAGE(' CHECK RANGE INPUTS',' ')
	      CALL XVMESSAGE(' TWO INTEGERS REQUIRED',' ')
	      GO TO 5
            END IF

         ELSE IF ( IEDEF.EQ.0 )  THEN
            GO TO 900

         ELSE
            CALL XVMESSAGE(' INVALID COMMAND:',' ')
	    CALL XVMESSAGE('	 TYPE "EXIT" TO EXIT SARGON',' ')
            CALL XVMESSAGE('     OR ENTER RANGE FOR',' ')
            GO TO 5
         END IF
      END IF
      PLANE = XDSGRAPH(DUNIT)
      CALL DSPLY(SL,SS)

C        PARAMETER PROCESSOR
24    CALL XVINTRACT('IPARAM','SARGON READY')
      VERFLG = 0
C
C        'RANGE'
C
10    CALL XVIPARM('RANGE',IARG,ICNT,IDEF,2)
      IF ( IDEF.EQ.0) THEN
        HBL = IARG(1)
        HBH = IARG(2)
        IF ((HBL.EQ.538976288) .OR. (HBH.EQ.538976288)) then
          CALL XVMESSAGE(' CHECK RANGE INPUTS',' ')
	  CALL XVMESSAGE(' TWO INTEGERS REQUIRED',' ')
        END IF
        REDISP = 1
      ENDIF
C
C        'FIT' 
C
      CALL XVIPARM('FIT',IARG,ICNT,IDEF,2)
      IF (IDEF.EQ.0) THEN
        HBL = IARG(1)
        HBH = IARG(2)
        IF ((HBL.EQ.538976288) .OR. (HBH.EQ.538976288)) then
          CALL XVMESSAGE(' CHECK RANGE INPUTS',' ')
	  CALL XVMESSAGE(' TWO INTEGERS REQUIRED',' ')
        END IF
        REDISP = 1
      ENDIF
C
C        'MIN'
C
      CALL XVIPARM('MIN',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        MINDN = IARG(1)
        IF (MINDN .eq. 538976288) THEN
	  CALL XVMESSAGE(' INTEGER MINIMUM DN VALUE REQUIRED ',' ')
	  MINDN = -9999
        END IF
      END IF
C
C        'MAX'
C
      CALL XVIPARM('MAX',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        MAXDN = IARG(1)
        IF (MAXDN .eq. 538976288) then
	  CALL XVMESSAGE(' INTEGER MAXIMUM DN VALUE REQUIRED ',' ')
	  MAXDN = 32768
        END IF 
      END IF
C
C        'HELP'
C
      CALL XVIPARM('HELP',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) CALL HELP
C
C        'PERC'
C
      CALL XVIPARM('PERC',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        PERC = IARG(1)
        IF ((PERC .LT. 0).or.(PERC .GT. 100)) THEN 
     	  CALL XVMESSAGE (' PERCENTAGE >0 and <= 100 REQUIRED',' ')
	  CALL XVMESSAGE (' PERCENTAGE SET TO 100',' ')
	  PERC = 100
        END IF
      END IF 
C
C        'RADIUS'
C
      CALL XVIPARM('RADIUS',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IR = IARG(1)
        IF (IR .EQ. 538976288) then
	  CALL XVMESSAGE(' INTEGER RADIUS VALUE REQUIRED',' ')
	  CALL XVMESSAGE(' RADIUS SET TO DEFAULT 1000000',' ')
          RADI = 1000000
        ELSE 
          RADI = IR * IR
        END IF
      END IF
C
C        'END'
C
      CALL XVIPARM('END',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) GO TO 900
C
C        'EXIT'
C
      CALL XVIPARM('EXIT',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) GO TO 900
C
C        'HOME'
C
      CALL XVIPARM('HOME',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        IF(SL .NE. 1 .OR. SS .NE. 1) THEN
          SL = 1
          SS = 1
          CALL DSPLY(SL,SS)
        END IF
      END IF
C
C        'MOVE WINDOW UP'
C
      CALL XVIPARM('U',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IF(SL .NE. 1) THEN 
          REDISP = 1
          NSL = SL - IARG(1)
          SL = MAX0(1,NSL)
        ENDIF
      END IF
C
C        'MOVE WINDOW UP'
C
      CALL XVIPARM('UP',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IF(SL .NE. 1) THEN 
          REDISP = 1
          NSL = SL - IARG(1)
          SL = MAX0(1,NSL)
        ENDIF
      END IF
C
C        'MOVE WINDOW DOWN'
C
      CALL XVIPARM('D',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IF(SL .NE. MAXSL) THEN
          REDISP = 1
          NSL = SL + IARG(1)
          SL = MIN0(MAXSL,NSL)
        END IF 
      END IF
C
C        'MOVE WINDOW DOWN'
C
      CALL XVIPARM('DOWN',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IF(SL .NE. MAXSL) THEN
          REDISP = 1
          NSL = SL + IARG(1)
          SL = MIN0(MAXSL,NSL)
        END IF 
      END IF
C
C        'MOVE WINDOW RIGHT'
C
      CALL XVIPARM('R',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IF(SS .NE. MAXSS) THEN
          REDISP = 1
          NSS = SS + IARG(1) 
          SS = MIN0(MAXSS,NSS)
        END IF
      END IF
C
C        'MOVE WINDOW RIGHT'
C
      CALL XVIPARM('RIGHT',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IF(SS .NE. MAXSS) THEN
          REDISP = 1
          NSS = SS + IARG(1) 
          SS = MIN0(MAXSS,NSS)
        END IF
      END IF
C
C        'MOVE WINDOW LEFT'
C
      CALL XVIPARM('L',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        IF(SS .NE. 1) THEN
          REDISP = 1
          NSS = SS - IARG(1)
          SS = MAX0(1,NSS)
        END IF
      END IF
C
C        'MOVE WINDOW LEFT'
C
      CALL XVIPARM('LEFT',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        IF(SS .NE. 1) THEN
          REDISP = 1
          NSS = SS - IARG(1)
          SS = MAX0(1,NSS)
        END IF
      END IF
C
C        'STRETCH'
C
      CALL XVIPARM('STRETCH',IARG,ICNT,IDEF,2)
      IF (IDEF.EQ.0) THEN 
        LO = IARG(1) 
        HI = IARG(2)
        IF((LO.EQ.538976288) .OR. (HI.EQ.538976288)) THEN
           CALL XVMESSAGE(' CHECK STRETCH INPUTS.',' ')
           CALL XVMESSAGE(' TWO INTEGERS REQUIRED.',' ')
        ELSE     
          SLOPE = 255. / (HI - LO)
          OFST = -LO * SLOPE
          DO I=1,256
             DN = I - 1
             II = IFIX(SLOPE*DN + OFST + 0.5)
             IF(II .LT. 0) II = 0
             IF(II .GT. 255) II = 255
             STR(I) = II
          END DO
          IERR = XDLWRITE(DUNIT,1,1,STR)
          IERR = XDLWRITE(DUNIT,2,1,STR)
          IERR = XDLWRITE(DUNIT,3,1,STR)
        END IF
      END IF
C
C        'LINEAR'
C
      CALL XVIPARM('LINEAR',IARG,ICNT,IDEF,2)
      IF (IDEF.EQ.0) THEN 
        LO = IARG(1) 
        HI = IARG(2)
        IF((LO.EQ.538976288) .OR. (HI.EQ.538976288)) THEN
           CALL XVMESSAGE(' CHECK STRETCH INPUTS.',' ')
           CALL XVMESSAGE(' TWO INTEGERS REQUIRED.',' ')
        ELSE     
          SLOPE = 255. / (HI - LO)
          OFST = -LO * SLOPE
          DO I=1,256
             DN = I - 1
             II = IFIX(SLOPE*DN + OFST + 0.5)
             IF(II .LT. 0) II = 0
             IF(II .GT. 255) II = 255
             STR(I) = II
          END DO
          IERR = XDLWRITE(DUNIT,1,1,STR)
          IERR = XDLWRITE(DUNIT,2,1,STR)
          IERR = XDLWRITE(DUNIT,3,1,STR)
        END IF
      END IF
C
C        'ON'
C
      CALL XVIPARM('ON',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        IERR = XDLWRITE(DUNIT,1,1,STR)
        IERR = XDLWRITE(DUNIT,2,1,STR)
        IERR = XDLWRITE(DUNIT,3,1,STR)
      END IF
C
C        'OFF'
C
      CALL XVIPARM('OFF',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN 
        IERR = XDLWRITE(DUNIT,1,1,OFF)
        IERR = XDLWRITE(DUNIT,2,1,OFF)
        IERR = XDLWRITE(DUNIT,3,1,OFF)
      END IF
C
C        'RAW'
C
      CALL XVIPARM('RAW',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        IERR = XDLWRITE(DUNIT,1,1,RAW)
        IERR = XDLWRITE(DUNIT,2,1,RAW)
        IERR = XDLWRITE(DUNIT,3,1,RAW)
      ENDIF
C
C        'MULTIPLY'
C
      CALL XVIPARM('MULT',RARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        FUNC = 1
        CONST = RARG(1)
        VERFLG = 1
      END IF
C
C        'ADD'
C
      CALL XVIPARM('ADD',RARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        FUNC = 2
        CONST = RARG(1)
        VERFLG = 1
      END IF
C
C        'SUBTRACT'
C
      CALL XVIPARM('SUBTRACT',RARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        FUNC = 3
        CONST = RARG(1)
        VERFLG = 1
      END IF 
C
C        'SUBTRACT'
C
      CALL XVIPARM('SUB',RARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        FUNC = 3
        CONST = RARG(1)
        VERFLG = 1
      END IF 
C
C        'DIVIDE'
C
      CALL XVIPARM('DIVIDE',RARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        FUNC = 4
        CONST = RARG(1)
        VERFLG = 1
      END IF
C
C        'DIVIDE'
C
      CALL XVIPARM('DIV',RARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        FUNC = 4
        CONST = RARG(1)
        VERFLG = 1
      END IF
C
C        'INTERPOLATE'
C
      CALL XVIPARM('INTERP',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
       FUNC = 0
       VERFLG=1
      END IF 
C
C        'SET TO DN'
C
      CALL XVIPARM('SETTO',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        FUNC = 5
        VERFLG = 1
        ICONST = IARG(1) 
        IF (ICONST .EQ. 538976288) THEN  
	  CALL XVMESSAGE(' SET TO ZERO ASSUMED',' ')
          ICONST = 0
        END IF
      END IF
C
C        'SET TO DN'
C
      CALL XVIPARM('SET',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN 
        FUNC = 5
        VERFLG = 1
        ICONST = IARG(1) 
        IF (ICONST .EQ. 538976288) THEN  
	  CALL XVMESSAGE(' SET TO ZERO ASSUMED',' ')
          ICONST = 0
        END IF
      END IF
C
C        'ZERO'
C
      CALL XVIPARM('ZERO',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN 
        FUNC = 6
        ICONST = 0
        VERFLG = 1
      END IF
C
C        'DBUG'
C
      CALL XVIPARM('DBUG',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) DBUG=1
C
C        'COPY INPUT'
C
      CALL XVIPARM('COPY',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        FUNC = 8
        VERFLG = 1
      END IF
C
C        'RESTORE'
C
      CALL XVIPARM('RESTORE',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        FUNC = 8
        VERFLG = 1
      END IF
C
C        'READ CURSOR'
C
      CALL XVIPARM('RCUR',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN 
        IERR = XDCLOCATION(DUNIT,1,IDS,IDL)
        IPL = IDL + SL - 1
        IPS = IDS + SS - 1

        WRITE(MSG,990) IDL,IDS
        CALL XVMESSAGE(MSG,' ')

        WRITE(MSG,1000) IPL,IPS
        CALL XVMESSAGE(MSG,' ')
      END IF
C
C        'POSITION CURSOR'
C
      CALL XVIPARM('PCUR',IARG,ICNT,IDEF,2)
      IF (IDEF.EQ.0)  THEN 
        L = IARG(1) - SL + 1
        S = IARG(2) - SS + 1
        IF ((L.EQ.538976288) .AND. (S.EQ.538976288)) THEN
  	  CALL XVMESSAGE(' LOCATION MUST BE SPECIFIED',' ')
        ELSE 
           IF (L.LT.1.OR.L.GT.MAXL.OR.S.LT.1.OR.S.GT.MAXS) THEN
             CALL XVMESSAGE(' LOCATION SPECIFIED IS CURRENTLY NOT',' ')
	     CALL XVMESSAGE(' DISPLAYED; REPOSITION IMAGE AND TRY AGAIN',
     .  			' ')
           ELSE
            IERR = XDCSET(DUNIT,1,S,L)
           END IF
        END IF
      END IF
C
C        'CIRCLE MODE'
C
      CALL XVIPARM('CIRC',IARG,ICNT,IDEF,1)
      IF (IDEF.EQ.0) THEN
        IRAD = IARG(1)
        IF( IRAD .EQ. 538976288 ) THEN
 	  CALL XVMESSAGE(' INTEGER RADIUS VALUE REQUIRED',' ')
        ELSE
          CALL XDDINFO(DUNIT, 35, 1, SECTION)
          IERR = XDGLINIT(DUNIT,SECTION)
          MDN = XDGCOLOR(DUNIT,'WHITE')
          IDN = XDGCOLOR(DUNIT,'BLACK')
          BMDN = INT2BYTE(MDN)
          BIDN = INT2BYTE(IDN)
          IERR = XDIMFILL(DUNIT,PLANE,BMDN,BIDN)
          CALL CIRCLE(IRAD,NSEG,NPTS,FUNC,*24)
          CMODE = 1
          GO TO 251
        END IF
      END IF
C
C        'CALCULATE AVG. SD AND MIN & MAX DN'
C
      CALL XVIPARM('STATS',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        FUNC=7
        VERFLG=1
      END IF
C
C--------------------------------------------------------------
C        REDISPLAY IF NECESSARY
200   ATDL = SL - SLDS
      ATDS = SS - SSDS
      IF (REDISP .EQ. 1) THEN
         REDISP = 0
         CALL DSPLY(SL,SS)
         LIN1 = 1 - ATDL
         LINNL = NL - ATDL
         SAM1 = 1 - ATDS
         SAMNS = NS - ATDS
      END IF

C        VERTEX ACQUISITION LOOP
      IF (VERFLG .EQ. 0) GO TO 24
      CALL XVMESSAGE('FOR EACH POINT, POSITION THE CURSOR AND',' ')
      CALL XVMESSAGE('ENTER NEXT AND PRESS RETURN.  ENTER NEXT',' ')
      CALL XVMESSAGE('AND PRESS RETURN A SECOND TIME WITHOUT',' ')
      CALL XVMESSAGE('MOVING THE CURSOR TO INDICATE THE LAST',' ')
      CALL XVMESSAGE('LAST VERTEX. DO NOT END WITH THE ORIGINAL',' ')
      CALL XVMESSAGE('VERTEX; THE PROGRAM WILL COMPLETE THE',' ')
      CALL XVMESSAGE('POLYGON FOR YOU.',' ')
215   NVERT = 0
      LL = -99
      LS = -99
C        ERASE GRAPHICS PLANE
      IERR = XDIMFILL(DUNIT,PLANE,MDN,IDN)
220   CALL XVMESSAGE(' READY FOR TRACKBALL',' ')
      CALL XVMESSAGE(' TO SPECIFY COORDINATES, POSITION TRACKBALL'
     +                                         ,' ')
      CALL XVMESSAGE(' AND ENTER NEXT.',' ')
      CALL XVMESSAGE(' TO EXIT, ENTER ANY OTHER COMMAND',' ')

221   CALL XVINTRACT('IPARAM','ENTER NEXT OR OTHER COMMAND')
      CALL XVIPARM('NEXT',IARG,ICNT,IDEF,0)

      IF (IDEF.NE.0) THEN
        VERFLG=0
        GO TO 10
      END IF

C        GET TRACKBALL LOCATION
      IERR = XDCLOCATION(DUNIT,1,S,L)
      IF(L.GT.LINNL.OR.L.LT.LIN1.OR.S.GT.SAMNS.OR.S.LT.SAM1) THEN
        CALL XVMESSAGE('REPOSITION, CURSOR IS OUTSIDE THE PICTURE',' ')
        GO TO 220		
      END IF
      IF(L .EQ. LL .AND. S .EQ. LS) GO TO 250
      LL = L
      LS = S
      NVERT = NVERT + 1
      IF (NVERT .EQ. 25) THEN
        CALL XVMESSAGE(' TOO MANY PTS GATHERED.  START OVER',' ')
        PLANE = XDSGRAPH(DUNIT)
        CALL XDDINFO(DUNIT, 35, 1, SECTION)
        IERR = XDGLINIT(DUNIT, SECTION)
        IDN = XDGCOLOR(DUNIT, 'BLACK')
        MDN = XDGCOLOR(DUNIT, 'WHITE')
        BMDN = INT2BYTE(MDN)
        BIDN = INT2BYTE(IDN) 
        IERR = XDIMFILL( DUNIT, PLANE, BMDN, BIDN)
        GO TO 215
      END IF
      V(1,NVERT) = S + (ATDS/1.0)
      V(2,NVERT) = L + (ATDL/1.0)
      X(2) = S
      Y(2) = L
 
      IF(NVERT .GT. 1) THEN
         CALL XDDINFO(DUNIT, 35, 1, SECTION)
         IERR = XDGLINIT(DUNIT,SECTION)
         DN = XDGCOLOR(DUNIT,'WHITE')
         BDN = INT2BYTE(DN)
         IERR = XDIPOLYLINE(DUNIT,PLANE,BDN,2,X,Y)
      END IF
      X(1)=X(2)
      Y(1)=Y(2)
      IF (NVERT .EQ. 23) THEN
	CALL XVMESSAGE(' YOU ARE ALLOWED ONE LAST POINT',' ')
      ENDIF	
      GO TO 221		

C        LAST POINT
250   X(2) = V(1,1) - (ATDS/1.0)
      Y(2) = V(2,1) - (ATDL/1.0)
      CALL XDDINFO(DUNIT, 35, 1, SECTION)
      IERR = XDGLINIT(DUNIT,SECTION)
      DN = XDGCOLOR(DUNIT,'WHITE')
      BDN = INT2BYTE(DN)
      IERR = XDIPOLYLINE(DUNIT,PLANE,BDN,2,X,Y)

C      CALL PRNT(4,1,NVERT,'NVERT=.')
      WRITE(MSG,252) NVERT
252   FORMAT('NVERT= ',I8)
      CALL XVMESSAGE(MSG,' ')

C        CHECK IF OK
C251   CALL XVMESSAGE(' OK ?   (Y OR N)',16)
251   CALL XVINTRACT('IPARAM',' OK ? (Y OR N) ')
      CALL XVIPARM('N',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        PLANE = XDSGRAPH(DUNIT)
        CALL XDDINFO(DUNIT, 35, 1, SECTION)
        IERR = XDGLINIT(DUNIT, SECTION)
        IDN = XDGCOLOR(DUNIT, 'BLACK')
        MDN = XDGCOLOR(DUNIT, 'WHITE')
        BMDN = INT2BYTE(MDN)
        BIDN = INT2BYTE(IDN) 
        IERR = XDIMFILL( DUNIT, PLANE, BMDN, BIDN)
        GO TO 215
      ENDIF
      CALL XVIPARM('NO',IARG,ICNT,IDEF,0)
      IF (IDEF.EQ.0) THEN
        PLANE = XDSGRAPH(DUNIT)
        CALL XDDINFO(DUNIT, 35, 1, SECTION)
        IERR = XDGLINIT(DUNIT, SECTION)
        IDN = XDGCOLOR(DUNIT, 'BLACK')
        MDN = XDGCOLOR(DUNIT, 'WHITE')
        BMDN = INT2BYTE(MDN)
        BIDN = INT2BYTE(IDN) 
        IERR = XDIMFILL( DUNIT, PLANE, BMDN, BIDN)
        GO TO 215
      ENDIF
C        START COLLECTING THE POINTS DEFINING THE POLYGON
      IF (CMODE .EQ. 1) GO TO 275
      CALL COLECT(NSEG,NPTS,V,*24)

C        NOW DO THE REQUESTED OPERATION ON THE LINE SEGMENTS OF POLYGON
275   CALL OPRATE(NSEG,ICONST,CONST,ATDL,ATDS,NPTS)
      CMODE = 0
      GO TO 215

C        CLOSE DATA SETS AND DISPLAY DEVICE
900   CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL CLOSE_DEVICE(DUNIT)

990   FORMAT('SCREEN  LINE= ',I3,'  SAMPLE= ',I3)
1000  FORMAT('PICTURE LINE= ',I3,'  SAMPLE= ',I3)

      RETURN
      END
C
C
C**********************************************************************
C
      SUBROUTINE DSPLY(SL,SS)

C        THIS ROUTINE WILL DISPLAY A PORTION OF A BYTE OR HALFWORD PIC.
      
      IMPLICIT INTEGER(A-Z)

      INCLUDE 'fortport'

      COMMON/C/IUNIT,OUNIT,ICOD,HBL,HBH,FUNC,RADI
      COMMON /D/ NL,NS,NLDS,NSDS
      COMMON /F/ LBUF,DBUG,NVERT,MINDN,MAXDN,PERC
      COMMON /H/ DUNIT

      INTEGER OUNIT

      LOGICAL*1 OBUF(20000)
      LOGICAL*1 LBUF(20000)
      REAL S1,S2
      INTEGER HISBUF(256),STAT,D1,D2,XDSGRAPH,XDGCOLOR
      INTEGER SECTION
      INTEGER XDIFILL,XDIMFILL,XDILINEWRITE,XDGLINIT
      BYTE BMDN, BIDN
C
      S1 = HBL
      S2 = HBH
      D1 = 0
      D2 = 0
C        ERASE GRAPHICS
      PLANE = XDSGRAPH(DUNIT)
      CALL XDDINFO(DUNIT, 35, 1, SECTION)
      IERR = XDGLINIT(DUNIT,SECTION)
      MDN = XDGCOLOR(DUNIT,'WHITE')
      IDN = XDGCOLOR(DUNIT,'BLACK')
      BMDN = INT2BYTE(MDN)
      BIDN = INT2BYTE(IDN)
      IERR = XDIMFILL(DUNIT,PLANE,BMDN,BIDN)
C        ERASE VIDEO
      IERR = XDIFILL(DUNIT,1,IDN)
      NLO = MIN0(NLDS,NL)
      EL = SL + NLO - 1
      NSO = MIN0(NSDS,NS)
      ILDS = 1
C
      IF(ICOD .EQ. 2) THEN
         ISW = 1
         CALL DFORM(LBUF,OBUF,NSO,HISBUF,ISW,1,2,D1,D2,S1,S2,0)
      END IF
C
      DO L=SL,EL
         CALL XVREAD(OUNIT,LBUF,STAT,'LINE',L,'SAMP',SS,
     .    	'NSAMPS',NSO,' ')
	 IF(ICOD .EQ. 2) THEN
            ISW = 0
            CALL DFORM(LBUF,OBUF,NSO,HISBUF,ISW,0,0,0,0,0,0,0)
            IERR = XDILINEWRITE(DUNIT,1,1,ILDS,NSO,OBUF)
         ELSE
            IERR = XDILINEWRITE(DUNIT,1,1,ILDS,NSO,LBUF)
         END IF
         ILDS = ILDS + 1
      END DO
C
      RETURN
      END
C
C
C**********************************************************************
C
      SUBROUTINE COLECT(NSEG,NPTS,V,*)

C        THIS ROUTINE WILL COLLECT THE NECESSARY POINTS ABOUT THE
C        PERIPHERY OF THE SPECIFIED POLYGON.

      IMPLICIT INTEGER (A-Z)

      COMMON/C/IUNIT,OUNIT,ICOD,HBL,HBH,FUNC,RADI
      COMMON /D/ NL,NS,NLDS,NSDS
      COMMON /E/ PTBUF,SEGM,SORT
      COMMON /F/ LBUF,DBUG,NVERT,MINDN,MAXDN,PERC
      COMMON /H/ DUNIT

      INTEGER OUNIT,NPTS,P,N,L
      REAL RANDOUT
      LOGICAL*1 LBUF(20000)
      INTEGER*2 PTBUF(3,4000),SEGM(3,2000),SORT(2,4000)
      
      REAL POINT(2),T,SLOPE,V(2,26)
      INTEGER*2 PT(3,1000)
      CHARACTER*80 PBUF
      CHARACTER*100 MSG
      LOGICAL*1 INSIDE

C     EQUIVALENCE (LBUF,PBUF,PT)
C
C        CHECK FOR DUPLICATED END VERTEX
C
      IF(V(1,NVERT).EQ.V(1,1) .AND. V(2,NVERT).EQ.V(2,1)) THEN
	NVERT = NVERT - 1
      ELSE
	V(1,NVERT+1) = V(1,1)
        V(2,NVERT+1) = V(2,1)
      ENDIF
C
C        CHECK FOR POLYGON COMPLETELY OUTSIDE OF IMAGE
C
      DO 1510 HH=1,NVERT
          IF (V(1,HH).LE.NS .AND. V(2,HH).LE.NL) GO TO 1520
1510  CONTINUE
      CALL XVMESSAGE(' POLYGON IS COMPLETELY OUTSIDE THE IMAGE',' ')
      RETURN 1
1520  CONTINUE
C
C---------------------------------------------------------------
C-----FOR EACH SIDE OF POLYGON,COLLECT POINTS OF INTERSECTION
C-----WITH LINES.
C-----N IS THE NUMBER OF INTERSECTION POINTS
C-----NPTS IS THE NUMBER OF EXTERIOR POINTS
C-----NSEG IS THE NUMBER OF LINE SEGMENTS TO OPERATE ON
      NPTS = 0
      NSEG = 0
      N = 0
      DO 2200 J=1,NVERT
      SL = V(2,J)
      SS = V(1,J)
      EL = V(2,J+1)
      ES = V(1,J+1)
C-------------------------------------------------------------
C-----NOW COLLECT A DUPLICATE POINT FOR VERTICIES WHICH ARE
C-----LOCAL MINIMA OR MAXIMA, SO WE WILL END UP WITH TWO
C-----INTERSECTION POINTS AT THESE VERTICIES
      IF(J .EQ. 1) GO TO 550
      PL = V(2,J-1)
      PS = V(1,J-1)
      GO TO  560
  550 PL = V(2,NVERT)
      PS = V(1,NVERT)
  560 IF(PL .EQ. SL .OR. EL .EQ. SL) GO TO 570
      IF (MIN0(PL,SL,EL) .NE. SL .AND. MAX0(PL,SL,EL) .NE. SL) THEN 
	GO TO 570
      ENDIF
      N = N + 1
      SORT(1,N) = SL
      SORT(2,N) = SS
  570 CONTINUE
C----------------------------------------------------------------
      IF(EL .GT. SL) INC = 1
      IF(EL .LT. SL) INC = -1
      IF(EL .NE. SL) GO TO 2100
      N = N + 1
      SORT(1,N) = SL
      SORT(2,N) = SS
      GO TO 2200
 2100 SLOPE = FLOAT (ES-SS) / FLOAT (EL-SL)
      L = SL
 2110 T = SLOPE * FLOAT (L-SL)
      S = IFIX(T+SS+0.5)
      N = N + 1
      SORT(1,N) = L
      SORT(2,N) = S
      L = L + INC
      IF(L .NE. EL) GO TO 2110
 2200 CONTINUE
C
C-----------------------------------------------------------------------
C        SORT INTERSECTION POINTS BY LINE AND SAMPLE SIMULTANEOUSLY
C
      CALL SORTX(SORT,N)
      IF(SORT(1,N-1).NE.SORT(1,N)) THEN
        SORT(1,N+1)=SORT(1,N)
        SORT(2,N+1)=SORT(2,N)
        N=N+1
      ENDIF
      IF (DBUG .EQ. 1) THEN
C     CALL PRNT(2,2*N,SORT,' AFT LINE.')
       DO J=1,N
        WRITE(MSG,2201) SORT(1,J),SORT(2,J)
 2201   FORMAT(' AFT LINE ', 2I8)
        CALL XVMESSAGE(MSG,' ')
       ENDDO
      END IF
C-----------------------------------------------------------------------
C        TRANSFORM INTERSECTION PTS INTO LINE SEGMENTS MAKING UP POLYGON
C
      MINL = SORT(1,1)
      MAXL = SORT(1,N)
      STL = 1
      IF (DBUG .EQ. 1) THEN 
C     CALL PRNT(4,1,N,' NO INTRSCTN PTS.')
      WRITE(MSG,2202) N
 2202 FORMAT('NO INTRSCTN PTS', I8)
      CALL XVMESSAGE(MSG,' ')
      END IF
      DO 2300 L=MINL,MAXL
      NONL = 0
C
C        FIND NUMBER OF POINTS ON LINE L
C
      DO 2250 P=STL,N
      IF(SORT(1,P) .NE. L)  GO TO 2260
 2250 NONL = NONL + 1 

C
C        CHECK FOR EVEN NUMBER OF POINTS ON THIS LINE
C
 2260 IF(MOD(NONL,2) .EQ. 0) GO TO 2265
CC      Call XLGET(UNIT,'HISTORY','ENTROPY',BUF(50),ISTAT,'HIST',
C     *		TASKS(JG),'FORMAT','REAL',' ')
C        FIXUP NECESSARY FOR THIS LINE.  IT HAS ODD # OF INTERSECTION
C        POINTS.  WE WILL THROW OUT HE POINTS WHICH IS BOTH A VERTEX
C        AND AN EVEN # OF POINTS FROM BEGINNING INTERSECTION.
C
      CALL ITLA(32,PBUF,80)
      WRITE (PBUF,190) NONL,L
  190 FORMAT(I2,' INTERSECTION POINTS FOR LINE ',I4)
      CALL XVMESSAGE(PBUF,' ')
      DO 200 LL=2,NONL,2
      PX = STL + LL - 1
      DO 200 MM=1,NVERT
      IF(SORT(1,PX).EQ.V(2,MM) .AND. SORT(2,PX).EQ.V(1,MM)) GO TO 250
  200 CONTINUE
      CALL XVMESSAGE(' NO EVEN POINTS WERE VERTICIES',' ')
      CALL XVMESSAGE(' PATHOLOGICAL FIGURE....TRY AGAIN',' ')
      RETURN 1
  250 MV = 2 * (N-PX)
      CALL MVE(2,MV,SORT(1,PX+1),SORT(1,PX),1,1)
      N = N - 1
      NONL = NONL - 1
      CALL XVMESSAGE(' FIXUP SUCCESSFUL',' ')
C     CALL PRNT(4,1,NONL,' PTS ON LINE.')
      WRITE(MSG,2264) NONL
 2264 FORMAT(' PTS ON LINE ', I8)
      CALL XVMESSAGE(MSG,' ')
 2265 CONTINUE
C
C---------------------------------------------------------------
C-----FILL LINE SEGMENT AND EXTERIOR POINTS BUFFERS
      P = 1
 2268 NSEG = NSEG + 1
      SEGM(1,NSEG) = L
      NX = STL+P-1
      SEGM(2,NSEG) = SORT(2,NX)
      NX = NX + 1
      IF(P .EQ. NONL-1) GO TO 2269
      IF(SORT(2,NX) .NE. SORT(2,NX+1)) GO TO 2269
      NX = NX + 2
      P = P + 2
 2269 SEGM(3,NSEG) = SORT(2,NX)
      IF(SEGM(2,NSEG) .LE. 1) GO TO 2270
      NPTS = NPTS + 1
      PTBUF(1,NPTS) = SEGM(2,NSEG) - 1
      PTBUF(2,NPTS) = L
 2270 CONTINUE
      IF(SEGM(3,NSEG) .GE. NS) GO TO 2290
      NPTS = NPTS + 1
      PTBUF(1,NPTS) = SEGM(3,NSEG) + 1
      PTBUF(2,NPTS) = L
 2290 P = P + 2
      IF(P .LE. NONL) GO TO 2268
 2300 STL = STL + NONL
C
      IF(DBUG .EQ. 0) GO TO 2301
C     CALL PRNT(4,1,NSEG,' NO LINE SEGMENTS.')
      WRITE(MSG,2291) NSEG
 2291 FORMAT(' NO LINE SEGMENTS ', I8)
      CALL XVMESSAGE(MSG,' ')
C     CALL PRNT(2,3*NSEG,SEGM,' SEGM BUF.')
      DO J=1,NSEG
        WRITE(MSG,2292) SEGM(1,J), SEGM(2,J), SEGM(3,J)
 2292   FORMAT(' SEGM BUF ', 3I8)
        CALL XVMESSAGE(MSG,' ')
      ENDDO
 2301 IF(FUNC .NE. 0) RETURN
C--------------------------------------------------------------
C-----COLLECT EXTERIOR POINTS ABOVE AND BELOW EACH SIDE BY
C-----MOVING IN SAMPLE DIRECTION (HENCE THE FUNNY SLOPE)
      DO 1400 J=1,NVERT
      SL = V(2,J)
      SS = V(1,J)
      EL = V(2,J+1)
      ES = V(1,J+1)
      S = SS
      IF(ES .GT. SS) INC = 1
      IF(ES .LT. SS) INC = -1
      IF(ES .EQ. SS) GO TO 1340
      SLOPE = FLOAT(EL-SL) / FLOAT(ES-SS)
 1310 T = SLOPE * FLOAT(S-SS) + SL
      IF(T .EQ. 1. .OR. T .EQ. 1.*NL) GO TO 1320
      L = IFIX(T - 0.001)
      POINT(1) = S
      POINT(2) = T - 1.
      IF(INSIDE(POINT,V,NVERT)) L = IFIX(T + 1. )
      CALL PTADD(L,S,NPTS,PTBUF)
 1320 S = S + INC
      IF(S .NE. ES) GO TO 1310
      GO TO 1400
 1340 MNL = MIN0(SL,EL)
      MXL = MAX0(SL,EL)
      IF(MNL .EQ. 1) GO TO 1350
      NPTS = NPTS + 1
      PTBUF(1,NPTS) = S
      PTBUF(2,NPTS) = MNL - 1
 1350 IF(MXL .EQ. NL) GO TO 1400
      NPTS = NPTS + 1
      PTBUF(1,NPTS) = S
      PTBUF(2,NPTS) = MXL + 1
 1400 CONTINUE
      IF(DBUG .EQ. 1) THEN
C     CALL PRNT(4,1,NPTS,' NO PTS TOTAL.')
      WRITE(MSG,1401) NPTS
 1401 FORMAT(' NO PTS TOTAL', I8)
      CALL XVMESSAGE(MSG,' ')
      END IF
C
 1500 CONTINUE
      IF(DBUG .EQ. 1) THEN
C     CALL PRNT(2,3*NPTS,PTBUF,' AFT SAMP.')
      DO J=1,NPTS
        WRITE(MSG,1501) PTBUF(1,J),PTBUF(2,J),PTBUF(3,J)
 1501   FORMAT(' AFT SAMP ', 3I8)
        CALL XVMESSAGE(MSG,' ')
      ENDDO
      END IF
C
C-----------------------------------------------------------------
C-----CUT BACK ON THE NUMBER OF POINTS BY SELECTING (WEED) POINTS
C-----AT RANDOM (I HOPE).  MAKE SURE N IS IN RANGE 1 - NPTS AND
C-----IS NOT REPEATED.
      IF(PERC .EQ. 100) GO TO 1100
      WEED = PERC * NPTS / 100
      SEED1 = 15289
C
      DO 1000 I=1,WEED
 1001 CALL GET_RAN(SEED1,RANDOUT)
      N=RANDOUT*NPTS
      IF(N .LT. 1 .OR. N .GT. NPTS) GO TO 1001
      IF (I.EQ.1) GO TO 1003
      DO 1002 L=1,I
      IF(N .EQ. PT(3,L)) GO TO 1001
 1002 CONTINUE
 1003 PT(1,I) = PTBUF(1,N)
      PT(3,I) = N
 1000 PT(2,I) = PTBUF(2,N)
C
      NPTS = WEED
      CALL MVE(2,3*NPTS,PT,PTBUF,1,1)
 1100 CONTINUE
      IF(DBUG .EQ. 1) THEN
C     CALL PRNT(4,1,NPTS,' NPTS AFT WEED.')
      WRITE(MSG,1101) NPTS
 1101 FORMAT(' NPTS AFT WEED ', I8)
      CALL XVMESSAGE(MSG,' ')
      END IF
C
C----------------------------------------------------------------
C-----SORT EXTERIOR POINTS BY LINE AND SAMPLE IN ORDER TO READ DNS
      CALL MVE(2,NPTS,PTBUF(2,1),SORT(1,1),3,2)
      CALL MVE(2,NPTS,PTBUF(1,1),SORT(2,1),3,2)
      CALL SORTX(SORT,NPTS)
C
C-------------------------------------------------------------------
C-----FILL PTBUF BY READING DNS OF SORTED POINTS
      CALL GETDN(NPTS)
C--------------------------------------------------------------------
C-----CHECK FOR THRESHHOLD CRITERIAN
      IF(MINDN .EQ. -9999 .AND. MAXDN .EQ. 32768) GO TO 650
      I = 0
  600 I = I + 1
      IF(I .GT. NPTS) GO TO 650
      IF(PTBUF(3,I) .GE. MINDN .AND. PTBUF(3,I) .LE. MAXDN) GO TO 600
      CALL MVE(2,3,PTBUF(1,NPTS),PTBUF(1,I),1,1)
      NPTS = NPTS - 1
      I = I - 1
      GO TO 600
  650 CONTINUE
C---------------------------------------------------------------------
      IF(NPTS .LT. 1) GO TO 997
      IF(DBUG .EQ. 0) RETURN
C     CALL PRNT(4,1,NPTS,' NPTS AFT THRESH.')
      WRITE(MSG,651) NPTS
  651 FORMAT(' NPTS AFT THRESH ', I8)
      CALL XVMESSAGE(MSG,' ')
C     CALL PRNT(2,NPTS*3,PTBUF,' PTBUF.')
      DO J=1,NPTS
        WRITE(MSG,652) PTBUF(1,J),PTBUF(2,J),PTBUF(3,J)
  652   FORMAT(' NO PTS TOTAL', 3I8)
        CALL XVMESSAGE(MSG,' ')
      ENDDO
C
C----------------------------------------------------------------
C
      RETURN

997   CALL XVMESSAGE(' NO PTS IN RANGE MIN TO MAX',' ')
      RETURN 1
      END
C
C
C**********************************************************************
C
      SUBROUTINE OPRATE(NSEG,ICONST,CONST,ATDL,ATDS,NPTS)

      IMPLICIT INTEGER (A-Z)

      INCLUDE 'fortport'

      COMMON/C/IUNIT,OUNIT,ICOD,HBL,HBH,FUNC,RADI
      COMMON /E/ PTBUF,SEGM,SORT
      COMMON /H/ DUNIT

      LOGICAL*1 OBUF(10000)

      CHARACTER*200 MSG
      INTEGER OUNIT
      INTEGER*2 HBUF(10000)
      INTEGER*2 PTBUF(3,4000),SEGM(3,2000),SORT(2,4000)

      REAL*8 DNSUM,DNSUM2
      REAL CONST,LO,HI,DNSD,SDEV
      INTEGER ULIM/255/,LLIM/0/,STAT,HISBUF(256)
      INTEGER X,Y,NNS,D1,D2,XDSGRAPH, XDGCOLOR,PLANE
      INTEGER XDILINEWRITE, XDIMFILL,XDGLINIT
      INTEGER*4 ICONST, IDN, MDN, SECTION
      INTEGER*4 IHBUF(10000)
      BYTE BIDN, BMDN

      CALL ZIA(IHBUF,10000)

C        LOOP THROUGH ALL SEGMENTS
      LO = HBL
      HI = HBH
      MARK = 0
      IF(ICOD .EQ. 2) THEN
         LLIM = -32768
         ULIM =  32767
      END IF
C
C        RE-OPEN OUTPUT AND INPUT DATA SETS WITH U_FORMAT = HALF
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE','U_FORMAT','HALF',' ')
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')

      CALL MVE(-6,10000,ICONST,HBUF,0,1)
      ISW=1
      IDNAVG = 0
      DNSUM = 0.
      DNSUM2 = 0.
      INUM = 0
      MINDN = 32767
      MAXDN = -32768
      IF (ICOD.EQ.2) THEN
	CALL DFORM(HBUF,OBUF,10000,HISBUF,ISW,1,2,D1,D2,LO,HI,0)
      ENDIF
C
      DO 1000 N = 1, NSEG
      L = SEGM(1,N)
      SS = SEGM(2,N)
      ES = SEGM(3,N)
      NS = ES - SS + 1
      DL = L - ATDL
      DS = SS - ATDS

      GO TO (810,810,810,810,810,810,810,800), FUNC

C        INTERPOLATE
      CALL XVREAD(OUNIT,HBUF,STAT,'LINE',L,' ')
      CALL EXTRAP(NPTS,L,SS,ES,PTBUF,HBUF(SS),RADI)
      GO TO 900

C        COPY INPUT
800   CALL XVREAD(OUNIT,HBUF,STAT,'LINE',L,' ')
      CALL XVREAD(IUNIT,HBUF(SS),STAT,'LINE',L,'SAMP',SS,
     .		'NSAMPS',NS,' ')
      GO TO 900

C        ADD SUBTRACT MULTIPLY DIVIDE SETTO ZERO STATS
810   CALL XVREAD(OUNIT,HBUF,STAT,'LINE',L,' ')

      GO TO (820,840,880,830,831,831,832), FUNC

      CALL XVMESSAGE(' ILLEGAL FUNC IN OPRATE',' ')
      CALL ABEND
C
C        MULTIPLY
C
820   IF (CONST .NE. 0.0) GO TO 1820
         IF (N .EQ. NSEG) THEN
             CALL XVMESSAGE(' MULTIPLY BY ZERO NOT ALLOWED.',' ')
             CALL XVMESSAGE(' FLOATING POINT VALUE EXPECTED.',' ')
         END IF
         GO TO 900
1820   DO I=SS,ES
         IHBUF(I) = HBUF(I) * CONST
         IF(IHBUF(I) .GT. ULIM) IHBUF(I) = ULIM
         IF(IHBUF(I) .LT. LLIM) IHBUF(I) = LLIM
         HBUF(I)=IHBUF(I)
      END DO
      GO TO 900
C
C        DIVIDE
C
830   IF (CONST .NE. 0.0) GO TO 1830
         IF (N .EQ. NSEG) THEN
             CALL XVMESSAGE(' DIVIDE BY ZERO NOT ALLOWED.',' ')
             CALL XVMESSAGE(' FLOATING POINT VALUE EXPECTED.',' ')
         END IF
         GO TO 900
1830  DO I=SS,ES
         IHBUF(I) = HBUF(I) / CONST
         IF(IHBUF(I) .GT. ULIM) IHBUF(I) = ULIM
         IF(IHBUF(I) .LT. LLIM) IHBUF(I) = LLIM
         HBUF(I)=IHBUF(I)
      END DO
      GO TO 900
C
C        SET TO CONST
C
831   DO I=SS,ES
         HBUF(I) = ICONST
      END DO
      GO TO 900
C
C        CALCULATE MEAN, SD, MINDN AND MAXDN
C
832   DO I=SS,ES
         DNSUM = DNSUM + HBUF(I)
         DNSUM2 = DNSUM2 + (DFLOAT(HBUF(I))*HBUF(I))
         INUM = INUM + 1
         IF (HBUF(I) .LT. MINDN) MINDN=HBUF(I)
         IF (HBUF(I) .GT. MAXDN) MAXDN=HBUF(I)
      END DO
      IF (N .EQ. NSEG) THEN
         IDNAVG = (DNSUM/INUM) + .5                ! CALC MEAN
C        CALL PRNT(4,1,IDNAVG,'MEAN DN=.')
         WRITE(MSG,833) IDNAVG
833      FORMAT(' MEAN DN = ', I8)
         CALL XVMESSAGE(MSG,' ')
         DNSD = DNSUM2*INUM - DNSUM*DNSUM
         IF (INUM .NE. 0 .AND. DNSD .GT. 0) THEN   ! CALC STANDARD DEVIATION
            SDEV = SQRT(DNSD)/INUM
         ELSE
	    SDEV = 0.0
         END IF
C        CALL PRNT(7,1,SDEV,'STANDARD DEVIATION=.')
         WRITE(MSG,834) SDEV
834      FORMAT(' STANDARD DEVIATION = ', E12.4)
         CALL XVMESSAGE(MSG,' ')
C        CALL PRNT(4,1,INUM,'NUM PTS USED TO CALC AVG=.')
         WRITE(MSG,835) INUM
835      FORMAT(' NUM PTS USED TO CALC AVG = ', I8)
         CALL XVMESSAGE(MSG,' ')
C        CALL PRNT(4,1,MINDN,'MINDN=.')
         WRITE(MSG,836) MINDN
836      FORMAT(' MINDN = ', I8)
         CALL XVMESSAGE(MSG,' ')
C        CALL PRNT(4,1,MAXDN,'MAXDN=.')
         WRITE(MSG,837) MAXDN
837      FORMAT(' MAXDN = ', I8)
         CALL XVMESSAGE(MSG,' ')
      END IF
      GO TO 1000
C
C        ADD
C
840   IF (CONST .NE. 0.0) GOTO 1840
         IF (N .EQ. NSEG) THEN
             CALL XVMESSAGE(' ADDING ZERO NOT ALLOWED.',' ')
             CALL XVMESSAGE(' FLOATING POINT VALUE EXPECTED.',' ')
         END IF
         GO TO 900
1840   DO I=SS,ES
         IHBUF(I) = HBUF(I) + CONST
         IF(IHBUF(I) .GT. ULIM) IHBUF(I) = ULIM
         IF(IHBUF(I) .LT. LLIM) IHBUF(I) = LLIM
         HBUF(I)=IHBUF(I)
      END DO
      GO TO 900
C
C        SUBTRACT
C
880   IF (CONST .NE. 0.0) GO TO 1880
         IF (N .EQ. NSEG) THEN
             CALL XVMESSAGE(' SUBTRACTING ZERO NOT ALLOWED.',' ')
             CALL XVMESSAGE(' FLOATING POINT VALUE EXPECTED.',' ')
         END IF
         GO TO 900
1880   DO I=SS,ES
         IHBUF(I) = HBUF(I) - CONST
         IF(IHBUF(I) .GT. ULIM) IHBUF(I) = ULIM
         IF(IHBUF(I) .LT. LLIM) IHBUF(I) = LLIM
         HBUF(I)=IHBUF(I)
      END DO
C
C	Change the output line
C
900   CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',L,' ')

C        WRITE THE NEW DNS TO THE DISPLAY
      IF(ICOD.EQ.2) THEN
        ISW = 0
      	CALL DFORM(HBUF(SS),OBUF,NS,HISBUF,ISW,0,0,0,0,0,0,0)
        
      ENDIF
      IF(ICOD.EQ.1) THEN
        CALL MVE(-3,NS,HBUF(SS),HBUF(SS),1,1)
      ENDIF
      X = DS
      Y = DL
      NNS = NS
      IF(ICOD.EQ.2) IERR = XDILINEWRITE(DUNIT,1,X,Y,NNS,OBUF)
      IF(ICOD.EQ.1) IERR = 
     & XDILINEWRITE(DUNIT,1,X,Y,NNS,HBUF(SS))
 1000 CONTINUE
C
C        RE-SET OUTPUT AND INPUT DATA SET U_FORMAT
      PLANE = XDSGRAPH(DUNIT)
      CALL XDDINFO(DUNIT, 35, 1, SECTION)
      IERR = XDGLINIT(DUNIT, SECTION)
      IDN = XDGCOLOR(DUNIT, 'BLACK')
      MDN = XDGCOLOR(DUNIT, 'WHITE')
      BMDN = INT2BYTE(MDN)
      BIDN = INT2BYTE(IDN) 
      IERR = XDIMFILL( DUNIT, PLANE, BMDN, BIDN)
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE',' ')
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,' ')

      RETURN
      END

C**********************************************************************

      FUNCTION IEVEN(I)
      IEVEN=(I+1)/2*2
      RETURN
      END
C
C
C**********************************************************************
C


      SUBROUTINE PTADD(L,S,NPTS,PTBUF)

C        THIS ROUTINE WILL ADD THE NEW POINTS TO PTBUF OR SKIP THEM
C        IF THE ARE ALREADY RECORDED THERE.

      IMPLICIT INTEGER (A-Z)
      INTEGER*2 PTBUF(3,4000)
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
C
C**********************************************************************
C
      SUBROUTINE GETDN(NPTS)

      IMPLICIT INTEGER (A-Z)
      COMMON/C/IUNIT,OUNIT,ICOD,HBL,HBH,FUNC,RADI
      COMMON /E/ PTBUF,SEGM,SORT
      COMMON /F/ LBUF,DBUG,NVERT,MINDN,MAXDN,PERC

      INTEGER OUNIT
      LOGICAL*1 LBUF(20000)
      INTEGER*2 PTBUF(3,4000),SEGM(3,2000),SORT(2,4000)

      INTEGER STAT
C
C        SORT HAS BEEN SORTED BY LINE THEN SAMP
      NP = 0
      SPT = 1
      MINL = SORT(1,1)
      MAXL = SORT(1,NPTS)
C
C        GET DNS ONE LINE AT A TIME
      DO 2000 L=MINL,MAXL
      NONL = 0
C
C        FIND NUMBER OF POINTS ON THIS LINE
      DO 1150 I=SPT,NPTS
      IF(SORT(1,I) .NE. L) GO TO 1200
 1150 NONL = NONL + 1

C        IFP & ILP ARE ELEMENTS OF SORT WHICH ARE FIRST & LAST PTS ON LINE
 1200 IF(NONL .EQ. 0) GO TO 2000
      IFP = SPT
      ILP = IFP + NONL - 1
C-----SFP & SLP ARE SAMP # IN PIC OF FIRST AND LAST POINT ON LINE L
      SFP = SORT(2,IFP)
      SLP = SORT(2,ILP)
      NSS=SLP-SFP+1
      CALL XVREAD(OUNIT,LBUF,STAT,'LINE',L,'SAMP',SFP,'NSAMPS',NSS,' ')
C-----FOR EACH POINTS ON THIS LINE FILL PTBUF
      DO 1300 J=1,NONL
C-----S IS # WITHIN SORT OF THE JTH PT ON LINE
      S = IFP - 1 + J
C-----SAMP IS SAMP # OF J TH PT
      SAMP = SORT(2,S)
C-----E IS ELEMENT # WITHIN LINE BUFFER
      E = SAMP - SFP + 1
      EX = 2 * E - 1
      NP = NP + 1
      PTBUF(1,NP) = SAMP
      PTBUF(2,NP) = L
      IF(ICOD .EQ. 1) CALL MVE(3,1,LBUF(E),PTBUF(3,NP),1,1)
      IF(ICOD .EQ. 2) CALL MVE(2,1,LBUF(EX),PTBUF(3,NP),1,1)
 1300 CONTINUE
      SPT = SPT+NONL
 2000 CONTINUE
      IF(NP .NE. NPTS) CALL XVMESSAGE(' NO DNS READ NE NPTS',' ')
      RETURN

      END
C
C**********************************************************************
C
      SUBROUTINE HELP

      CALL XVMESSAGE(' KEYWORDS.............FUNCTION',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ADD         *  ADD CONSTANT TO AREA',' ')
      CALL XVMESSAGE(' SUB  SUBT   *  SUBTRACT CONSTANT TO AREA',' ')
      CALL XVMESSAGE(' MUL  MULT   *  MULTPLY AREA BY CONSTANT',' ')
      CALL XVMESSAGE(' DIV  DIVI   *  DIVIDE AREA BY CONSTANT',' ')
      CALL XVMESSAGE(' ZERO        -  REPLACE AREA WITH ZEROES',' ')
      CALL XVMESSAGE(' SET  SETTO  +  REPLACE AREA WITH CONSTANT',' ')
      CALL XVMESSAGE(' STATS       -  CALC AVG, S.D., MIN/MAX DN ' //
     -                                            'OF AREA',' ')
      CALL XVMESSAGE(' COPY RESTOR -  RESTORE AREAS ORIGINAL CONDITION'
     -                                                 ,' ')
      CALL XVMESSAGE(' CIRC        +  COLLECT POINTS FOR RADIUS',' ')
      CALL XVMESSAGE(' INTERPOLATE -  FILL AREA WTH INTERPOLATED DNS',
     -                                                  ' ')
      CALL XVMESSAGE(' FIT  RANGE  ++ DN RANGE FOR DISPLAY OF HWD DATA'
     -                                                 ,' ')
      CALL XVMESSAGE(' RAW         -  NO STRETCH',' ')
      CALL XVMESSAGE(' ON          -  TURN VIDEO ON',' ')
      CALL XVMESSAGE(' OFF         -  TURN VIDEO OFF',' ')
      CALL XVMESSAGE(' LINE STRE   ++ DISP PICTURE WITH LINEAR STRETCH'
     -                                                 ,' ')
      CALL XVMESSAGE(' UP   U      +  MOVE DISPLAY WINDOW UP',' ')
      CALL XVMESSAGE(' DOWN D      +  MOVE DISPLAY WINDOW DOWN',' ')
      CALL XVMESSAGE(' LEFT L      +  MOVE DISPLAY WINDOW LEFT',' ')
      CALL XVMESSAGE(' RIGH R      +  MOVE DISPLAY WINDOW RIGHT',' ')
      CALL XVMESSAGE(' HOME        -  MOVE WINDOW TO UPPER LEFT CORNER'
     -                                                 ,' ')
      CALL XVMESSAGE(' MIN         +  MINIMUN DN USED IN INTERPOLATION'
     -                                                 ,' ')
      CALL XVMESSAGE(' MAX         +  MAXIMUM DN USED IN INTERPOLATION'
     -                                                 ,' ')
      CALL XVMESSAGE(' PERC        +  PRCNT BORDER PTS USED IN INTERP'
     -                                                 ,' ')
      CALL XVMESSAGE(' RADIUS      +  MAX DIST TO PTS USED IN INTERP'
     -                                                 ,' ')
      CALL XVMESSAGE(' RCUR        -  READ CURSOR POSITION',' ')
      CALL XVMESSAGE(' PCUR       ++  POSITION CURSOR TO PICTURE ' //
     -                                        'LOCATION',' ')
      CALL XVMESSAGE(' EXIT END    -  TERMINATE PROGRAM',' ')
      CALL XVMESSAGE(' DBUG        -  PRODUCE DBUG PRINT',' ')
      CALL XVMESSAGE(' @FILENAME   -  EXECUTE COMMANDS FOUND IN FILE'
     -                                                 ,' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' -     REQUIRES NO PARAMETERS',' ')
      CALL XVMESSAGE(' *     REQUIRES ONE REAL PARAMETER',' ')
      CALL XVMESSAGE(' +     REQUIRES ONE INTEGER PARAMETER',' ')
      CALL XVMESSAGE(' ++    REQUIRES TWO INTEGER PARAMETERS',' ')
      RETURN
      END
C
C
C**********************************************************************
C
      SUBROUTINE CIRCLE(RAD,NSEG,NPTS,FUNC,*)

      INCLUDE 'fortport'
      COMMON /D/ NL,NS,NLDS,NSDS
      COMMON /E/ PTBUF,SEGM,SORT
      COMMON /G/ SL, SS
      COMMON /H/ DUNIT

      CHARACTER*100 MSG
      INTEGER SL,SS
      INTEGER*2 PTBUF(3,4000),SEGM(3,2000),SORT(2,4000)
      INTEGER X(2000),Y(2000)
      INTEGER RAD,STL,EL,FL,DL,NSEG,LL,LS,LC,SC
      INTEGER XDCLOCATION,XDIPOLYLINE,XDSGRAPH,XDGCOLOR
      INTEGER SECTION,PLANE
      LOGICAL*1 BDN
      INTEGER XDGLINIT

C     CALL PRNT(4,1,RAD,'RADIUS =.')
      WRITE(MSG,201) RAD
201   FORMAT(' RADIUS = ', I8)
      CALL XVMESSAGE(MSG,' ')
      IERR = XDCLOCATION(DUNIT,1,SC,LC)
      STL = LC - RAD
      EL = LC + RAD
      NNL = EL - STL + 1
      FL = STL - 1
      DL = STL - LC - 1
      LL = MIN0((SL+512-1),NL)
      LS = MIN0((SS+512-1),NS)

      DO L = 1, NNL
        FL = FL + 1
        DL = DL + 1
        DSQ = RAD*RAD - DL*DL
        DS = SQRT(DSQ)
        SEGM(1,L) = FL + SL - 1
        SEGM(2,L) = INT(SC-DS+.5) + SS - 1
        SEGM(3,L) = INT(SC+DS+.5) + SS - 1
        IF (SEGM(1,L).LT.SL+1 .OR. SEGM(1,L).GT.LL-1 .OR. 
     +      SEGM(2,L).LT.SS+1 .OR. SEGM(3,L).GT.LS-1) THEN
           CALL XVMESSAGE(
     - ' CIRCLE IS TOO CLOSE TO EDGE OR OFF DISPLAY',' ')
           CALL XVMESSAGE(' TRY AGAIN',' ')
           RETURN1
        END IF
C        CALL PRNT(2,3,SEGM(1,L),' L,SS,ES =.')
      END DO
      NSEG = NNL
C         Print on graphics screen
      DO II = 1, NSEG
        X(II) = SEGM(2,II) - SS + 1
        Y(II) = SEGM(1,II) - SL + 1
        X(NSEG*2-(II-1)) = SEGM(3,II) - SS + 1
        Y(NSEG*2-(II-1)) = SEGM(1,II) - SL + 1
      END DO
      X(NSEG*2+1) = X(1)
      Y(NSEG*2+1) = Y(1)
      NN = NNL*2+1
      CALL XDDINFO(DUNIT, 35, 1, SECTION)
      IERR = XDGLINIT(DUNIT,SECTION)
      DN = XDGCOLOR(DUNIT,'WHITE')
      PLANE = XDSGRAPH(DUNIT)
      BDN = INT2BYTE(DN)
      IERR = XDIPOLYLINE(DUNIT,PLANE,BDN,NN,X,Y)
      NVERT = 2*NNL
C         IF INTERPOLATION, COLECT PTBUF
      IF (FUNC .EQ. 0) THEN
        NPTS = 1
        DO II = SEGM(2,1),SEGM(3,1)
          SORT(2,NPTS) = II
          SORT(1,NPTS) = SEGM(1,1) - 1
          NPTS = NPTS + 1
        END DO
        DO II = 1, NSEG
          SORT(2,NPTS) = SEGM(2,II) - 1
          SORT(1,NPTS) = SEGM(1,II)
          SORT(2,NPTS+1) = SEGM(2,II)
          SORT(1,NPTS+1) = SEGM(1,II)
          SORT(2,NPTS+2) = SEGM(3,II)
          SORT(1,NPTS+2) = SEGM(1,II)
          SORT(2,NPTS+3) = SEGM(3,II) + 1
          SORT(1,NPTS+3) = SEGM(1,II)
          NPTS = NPTS + 4
        END DO
        DO II = SEGM(2,NSEG),SEGM(3,NSEG)
          SORT(2,NPTS) = II
          SORT(1,NPTS) = SEGM(1,NSEG) + 1
          NPTS = NPTS + 1
        END DO
        NPTS = NPTS - 1
        CALL GETDN(NPTS)
      END IF
      RETURN
      END
C************************************************************************

 

           
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create vsort.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C     THIS SUBROUTINE WILL BE USED TO SORT THE LINES AND SAMPLES IN ASCENDING
C     ORDER USING LINE VALUES.

      SUBROUTINE SORTX(BUF,N)

      INTEGER*2 BUF(2,4000)
C
      DO 100 I=1,N
      J=BUF(1,I)
      BUF(1,I)=BUF(2,I)
      BUF(2,I)=J
100   CONTINUE

      CALL SORTIN(BUF,N)

      DO 200 I=1,N
      J=BUF(1,I)
      BUF(1,I)=BUF(2,I)
      BUF(2,I)=J
200   CONTINUE

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create usort.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C     THIS SUBROUTINE WILL SORT THE LINES AND SAMPLES IN ASCENDING ORDER
C     USING THE LINE VALUES.

      SUBROUTINE SORTX(BUF,N)

      INTEGER*2 BUF(2,4000)
C
      CALL SORTIN(BUF,N)

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create relevant_dsubs.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c---------------------------------------------------------
c
c
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
          SUBROUTINE OPEN_DEVICE( IUNIT )

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
          
          DATA C / 1 /, CSETUP / 0, 0, 0, 0/
          DATA H / 7 /, S / 1.0 /
          DATA IFORM / 0 /, CAUTO / .true. / 
          DATA IBLINK / 0 /, ICONOFF / 0 / 
          DATA SECTION /1/
          DATA W / 1 / 

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
          CALL XDDINFO( U, 3, 1, NLUTS )
          CALL XDDINFO( U, 4, 1, NIMPS )
          CALL XDDINFO( U, 5, 1, MAXLINES )
          CALL XDDINFO( U, 6, 1,  MAXSAMPS )
          CALL XDDINFO( U, 30, 1, IGRAPH )
          CALL XDDINFO( U, 34, 1, GPLANE )
          CALL XDDINFO( U, 48, 1, NCURS )
          CALL XDDINFO( U, 60, 1, NINTIO )
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
           CALL XDDINFO ( U, 35, 1, SECTION)
           IERR = XDGCONNECT (U, PLANE, SECTION, .FALSE. )
c
c          turn on the graphics overlay plane
c
           IERR = XDGON (U) 
          END IF
c
          IUNIT = U 
          RETURN
          END
c
c------------------------------------------------------------------
c
c	configure_device
c
c	configure the device given a number of 
c	lines and samples.  If the lines and samples are 0,
c	the the default configuration for the device is used
c	and the number of lines and samples are set to the
c	appropriate values.  Otherwise the device will be
c	configured to one of the following if it is available:
c		512x512 when lines=512 and samples=512
c		640x480 when lines=480 and samples=640
c		1024x1024 when lines=1024 and samples=1024
c	If the given lines and samples do not match any of
c	the above, the device's default is used but the values
c	of lines and samples are not changed.
c
      SUBROUTINE CONFIGURE_DEVICE( LINES, SAMPLES, IUNIT )

      CHARACTER*100 MSG
      CHARACTER*30 CTBL0(3)/ ' Video Output = 512x512       ',
     -                       ' Video Output = 1024x1024     ',
     -                       ' Video Output = 640x480       '/
      CHARACTER*30 CTBL1,CTBL2,CTBL3,CTBL4,CTBL5,CTBL6
      CHARACTER*30 CTBL7A/' GRAPHICS PLANE AVAILABLE     '/
      CHARACTER*30 CTBL7B/' GRAPHICS PLANE NOT AVAILABLE '/
      CHARACTER*30 CTBL7C/' GRAPHICS WILL BE PLACED ON   '/
      CHARACTER*30 CTBL7D/'       IMAGE PLANE 1          '/
      INTEGER  H,CSETUP(4), OUTMODES, CURRD3, SECTION,U
      INTEGER  GPLANE, NLUTS, NIMPS, MAXSAMPS  
      INTEGER  IGRAPH, NCURS, NINTIO, XDSGRAPH, PLANE
      INTEGER  RED(256), GREEN(256), BLUE(256)
      REAL     S
      INTEGER XDDCONFIGURE, XDGCONNECT 
      INTEGER XDTSIZE
      LOGICAL BTEST
c
      DATA CSETUP / 0, 0, 0, 0 /
      DATA RED   / 0,255*255 /
      DATA GREEN / 0,255*255 /
      DATA BLUE / 0,255*255 /
      DATA H / 7 /, S / 1.0 /

      U = IUNIT
      CALL XDDINFO( U, 35, 1, SECTION)
      CALL XDDINFO(U, 7, 1, OUTMODES )
      IF ((LINES.EQ.1024).AND.(SAMPLES.EQ.1024)) THEN
       IF ( BTEST(OUTMODES,9) ) THEN
         CSETUP(2) = 2
         CSETUP(3) = 2
       ELSE
         CALL XVMESSAGE(' 1024X1024 Output Not Available ',' ')
       END IF
      ELSE IF ((LINES.EQ.480).AND.(SAMPLES.EQ.640)) THEN
       IF ( BTEST(OUTMODES,10) ) THEN
         CSETUP(2) = 2
         CSETUP(3) = 3
       ELSE
         CALL XVMESSAGE(' 640x480 Output Mode Not Available ',' ')
       END IF
      ELSE IF ((LINES.EQ.512).AND.(SAMPLES.EQ.512)) THEN
       IF ( BTEST(OUTMODES,8) ) THEN
         CSETUP(2) = 1
         CSETUP(3) = 1
       ELSE
         CALL XVMESSAGE(' 512x512 Output Mode Not Available ',' ')
       END IF
      ELSE
       CSETUP(1) = 0
       CSETUP(2) = 0
       CSETUP(3) = 0
       CSETUP(4) = 0
       IF ( (LINES .NE. 0) .AND. (SAMPLES.NE.0) ) THEN 

	CALL XVMESSAGE(
     - ' Unrecognized Output Size, Display Default Used ', ' ')
       END IF  
      END IF 
c
c         now configure the display
c
          IERR = XDDCONFIGURE (U, CSETUP) 
c
c         find out what type of device we have
c
          CALL XDDINFO ( U, 3, 1, NLUTS )
          CALL XDDINFO ( U, 4, 1, NIMPS )
          CALL XDDINFO ( U, 5, 1, MAXLINES )
          CALL XDDINFO ( U, 6, 1,  MAXSAMPS )
          CALL XDDINFO ( U, 12, 1, CURRD3 )
          CALL XDDINFO ( U, 30, 1, IGRAPH )
          CALL XDDINFO ( U, 34, 1, GPLANE )
          CALL XDDINFO ( U, 48, 1, NCURS )
          CALL XDDINFO ( U, 60, 1, NINTIO )
c
c         print out what we have
c
          CALL XVMESSAGE(' ',' ')
          CALL XVMESSAGE(' Display Device Characteristics',' ')
          WRITE(MSG,50) CTBL0(CURRD3)
 50       FORMAT(A30)
          WRITE (CTBL1, 100) NLUTS
100       FORMAT(' No. of LUTs =     ',I2)
          WRITE (CTBL2, 200) NIMPS
200       FORMAT(' No. of IMPs =     ',I2)
          WRITE (CTBL3, 300) MAXLINES
300       FORMAT(' No. of LINES =    ',I4)
          WRITE (CTBL4, 400) MAXSAMPS
400       FORMAT(' No. of SAMPS =    ',I4)
          WRITE (CTBL5, 500) NCURS
500       FORMAT(' No. of CURSORS =  ',I2)
          WRITE (CTBL6, 600) NINTIO
600       FORMAT(' No. of IO DEVS =  ',I2)
          CALL XVMESSAGE(MSG,' ')
          CALL XVMESSAGE(CTBL1,' ')
          CALL XVMESSAGE(CTBL2,' ')
          CALL XVMESSAGE(CTBL3,' ')
          CALL XVMESSAGE(CTBL4,' ')
          CALL XVMESSAGE(CTBL5,' ')
          CALL XVMESSAGE(CTBL6,' ')

          IF (IGRAPH .EQ. 1) THEN 
           CALL XVMESSAGE(CTBL7A,' ')

          ELSE
           CALL XVMESSAGE(CTBL7B,' ')
           CALL XVMESSAGE(' ',' ')
           CALL XVMESSAGE(CTBL7C,' ')
           CALL XVMESSAGE(CTBL7D,' ')

           GPLANE = 1

          END IF 
          CALL XVMESSAGE(' ',' ')


	IF ( (LINES.EQ.0).AND.(SAMPLES.EQ.0) ) THEN 
         LINES = MAXLINES
         SAMPLES = MAXSAMPS
        END IF
c
c         connect the graphics plane to image plane 
c
          IF ( IGRAPH .GT. 0 ) THEN
           PLANE = XDSGRAPH(U)
           IERR = XDGCONNECT(U, PLANE, SECTION, .FALSE.)
          END IF
        LMAX = LINES
        SMAX = SAMPLES
        IF (LMAX.LE.512) THEN
           H = 7
        ELSE
           H = 14
        END IF 
        IERR = XDTSIZE (H,S) 
	RETURN 
        END
c
c-----------------------------------------------------------------
c
c
c        CLOSE_DEVICE
c
c         to deactivate the ability to modify the display unit u
c         and to deallocate it for the next user
c
c
c
c
	 SUBROUTINE CLOSE_DEVICE(IUNIT)
c
c        deactivate the device 
c
         INTEGER U
         INTEGER XDDCLOSE, XDDACTIVATE
	 LOGICAL FLAG

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
c----------------------------------------------------------------
c
c
c        bw_mode
c
c         this is the routine that connects image 1 to lut 1
c         2, and 3 and turns on the linear ramps.
c
          SUBROUTINE BW_MODE(IUNIT)
c
          INTEGER U, N1, SECTION, NLUTS
          INTEGER XDSSECTION,XDSGRAPH,PLANE
          INTEGER XDLCONNECT, XDLRAMP

          U = IUNIT
          PLANE = XDSGRAPH(U)
          CALL XDDINFO ( U, 3, 1, NLUTS )
         
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
c----------------------------------------------------------------
c
c
c        AUTOTRACKING_MODE
c	
c         this is the routine that turns autotracking on
c
          SUBROUTINE AUTOTRACKING_MODE( ON, IUNIT )
          LOGICAL ON
	  INTEGER XDCAUTOTRACK
          INTEGER U, NINTIO, C

          DATA  C /1/
          U = IUNIT
          CALL XDDINFO( U, 60, 1, NINTIO )
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sargon.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM sargon

   To Create the build file give the command:

		$ vimake sargon			(VMS)
   or
		% vimake sargon			(Unix)


************************************************************************/


#define PROGRAM	sargon
#define R2LIB

#if VMS_OS
#define MODULE_LIST sargon.f vsort.f relevant_dsubs.f
#define CLEAN_OTHER_LIST usort.f
#else
#define MODULE_LIST sargon.f usort.f relevant_dsubs.f
#define CLEAN_OTHER_LIST vsort.f
#endif


#define MAIN_LANG_FORTRAN
#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_VRDI
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create sargon.pdf
process help=*
SUBCMD-DEFAULT MAIN
   PARM INP TYPE=STRING 
   PARM OUT TYPE=STRING
 END-SUBCMD
SUBCMD IPARAM
   PARM EXIT    TYPE=KEYWORD VALID=EXIT COUNT=0:1 DEFAULT=--
   PARM FIT     TYPE=INTEGER COUNT=(0,2) DEFAULT=--
   PARM RANGE   TYPE=INTEGER COUNT=(0,2) DEFAULT=--
   PARM RIGHT   TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM R       TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM LEFT    TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM L       TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM UP      TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM U       TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM DOWN    TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM D       TYPE=INTEGER COUNT=(0,1) DEFAULT=--
   PARM HOME    TYPE=KEYWORD VALID=HOME COUNT=0:1 DEFAULT=--
   PARM LINEAR  TYPE=INTEGER COUNT=(0,2) DEFAULT=--
   PARM STRETCH TYPE=INTEGER COUNT=(0,2) DEFAULT=--
   PARM RAW     TYPE=KEYWORD VALID=RAW COUNT=0:1 DEFAULT=--
   PARM OFF     TYPE=KEYWORD VALID=OFF COUNT=0:1 DEFAULT=--
   PARM ON      TYPE=KEYWORD VALID=ON  COUNT=0:1 DEFAULT=--
   PARM MULT    TYPE=REAL               COUNT=(0,1)  DEFAULT=--
   PARM DIVIDE  TYPE=REAL               COUNT=(0,1)  DEFAULT=--
   PARM DIV     TYPE=REAL               COUNT=(0,1)  DEFAULT=--
   PARM ADD     TYPE=REAL               COUNT=(0,1)  DEFAULT=--
   PARM SUBTRACT TYPE=REAL              COUNT=(0,1) DEFAULT=--
   PARM SUB     TYPE=REAL               COUNT=(0,1)  DEFAULT=--
   PARM ZERO    TYPE=KEYWORD VALID=ZERO   COUNT=0:1 DEFAULT=--
   PARM SETTO   TYPE=INTEGER            COUNT=(0,1) DEFAULT=--
   PARM SET     TYPE=INTEGER            COUNT=(0,1) DEFAULT=--
   PARM COPY    TYPE=KEYWORD VALID=COPY    COUNT=0:1 DEFAULT=--
   PARM RESTORE TYPE=KEYWORD VALID=RESTORE COUNT=0:1 DEFAULT=--
   PARM INTERP  TYPE=KEYWORD VALID=INTERP  COUNT=0:1 DEFAULT=--
   PARM MIN     TYPE=INTEGER            COUNT=0:1 DEFAULT=-9999
   PARM MAX     TYPE=INTEGER            COUNT=0:1 DEFAULT=32768
   PARM RADIUS  TYPE=INTEGER            COUNT=0:1 DEFAULT=1000000
   PARM PERC    TYPE=INTEGER            COUNT=0:1 DEFAULT=100
   PARM DBUG    TYPE=KEYWORD VALID=DBUG COUNT=0:1 DEFAULT=--
   PARM HELP    TYPE=KEYWORD VALID=HELP COUNT=0:1 DEFAULT=--
   PARM END     TYPE=KEYWORD VALID=END  COUNT=0:1 DEFAULT=--
   PARM RCUR    TYPE=KEYWORD VALID=RCUR COUNT=0:1 DEFAULT=--
   PARM PCUR    TYPE=INTEGER            COUNT=0:2 DEFAULT=--
   PARM CIRC    TYPE=INTEGER            COUNT=0:1 DEFAULT=--
   PARM STATS   TYPE=KEYWORD VALID=STATS COUNT=0:1 DEFAULT=--
   PARM N       TYPE=KEYWORD VALID=N     COUNT=0:1 DEFAULT=--
   PARM Y       TYPE=KEYWORD VALID=Y     COUNT=0:1 DEFAULT=--
   PARM NO      TYPE=KEYWORD VALID=NO    COUNT=0:1 DEFAULT=--
   PARM YES     TYPE=KEYWORD VALID=YES   COUNT=0:1 DEFAULT=--
   PARM NEXT    TYPE=KEYWORD VALID=NEXT  COUNT=0:1 DEFAULT=--
END-SUBCMD
END-PROC
!# annot function=science/modifying
!# annot project=all keywords=modification
.TITLE
VICAR Program SARGON
.HELP
PURPOSE:
SARGON is an interactive program which performs operations on user-specified
areas (polygons) in an image.  SARGON operates on byte or halfword data.

EXECUTION:

Examples for this program are divided into two sections.  First is a
typical SARGON session.  The second section demonstrates the commands with
less attention given to the details of program operation and more to the
commands themselves.

.PAGE
Example #1  (Vertical spacing is used liberally in this example.  This
    spacing will not appear in a real SARGON session.)

TAE> SARGON INP=A OUT=B
COPYING INPUT TO OUTPUT
(There is a delay here, then the input image is displayed.)

.PAGE
SARGON READY
(The "SARGON READY" statement will come up whenever SARGON is 
requesting a new command from the user.  The other possible state is
flagged by the statement "READY FOR TRACKBALL" which indicates that 
SARGON is expecting the user to define a polygon using the trackball; the user
may, however, and often will, input a command in this latter state, too.)

.PAGE
'ZERO 

VERTEX ACQUISITION CYCLE....Hit carriage return TO STORE POINT
			    DOUBLE carriage return STORES LAST POINT
READY FOR TRACKBALL
(ENTER COMMAND OR PRESS RETURN TO DEFINE AREA)

(press RETURN)
(Move trackball and press <CR> for first point.  As you do this for
subsequent points, SARGON will connect the points you've selected with a
line.  DO NOT complete the polygon by selecting your starting point again
at the end; SARGON will complete the polygon for you.  When you're done, push
<CR> twice and SARGON will draw the completing line segment.)

.PAGE
OK ?   (Y OR N)

'Y
(Enter Y to use displayed polygon; N will allow you to respecify the area.)
(SARGON will zero out the specified region and come back with a prompt
for another area.)

READY FOR TRACKBALL
(ENTER COMMAND OR PRESS RETURN TO DEFINE AREA)

LINEAR=(10,100)
(Change lookup table.  We don't want to select any more areas in this
case, so we input a command.  SARGON will continue to accept areas for
the operation until EXIT or some other command is entered.)

SARGON READY
'EXIT
(This shows a return to the SARGON READY state, and we get out of
SARGON with EXIT.)

TAE>

.PAGE
Example #2

The following commands are more to show the functions of SARGON than
anything else; they don't represent a typical or necessarily reasonable
sequence of steps.  Moreover, SARGON output and trackball interaction
is omitted; the user should be able to deduce from the example above 
the actions that SARGON will take for each of these commands.)

GEN A 1000 1000		Generate a test image
SARGON A B		Call SARGON
   .
   .
   .
FIT=(0,32768)           For halfword data only, map 0 to 0 and 32768 to 255.
			This is only used for changing mapping mid-stream;
			SARGON automatically queries for these values at
			the start of the program when the input image is
			in halfword format.  Same as RANGE.
RIGHT=50		Move current window 50 pixels to the right.  
			This will not work - nor make sense - if
			the image already is filling the screen.
			This restriction also applies to the commands
			following.  Also, this and the next three commands
			may be abbreviated using the first letter of the
			command.
LEFT=50			Move current window 50 pixels to the left.
UP=200			Same as above, in up direction.
DOWN=75			Same as above, in down direction.
'HOME			Redisplay image such that pixel 1,1 is at the
			upper-left pixel of the monitor.

MULT=(0.5)		Multiply the DN of pixels within user-specified
			area by 0.5.
DIVIDE=5.3              Divide the DN of pixels within user-specified 
			area by 5.3 - may be abbreviated DIV.
ADD=180	         	Add 180 to the DN of all pixels within the
			specified area.  Values will top off at 255 as
			applicable.
SUBTRACT=200		Subtract 200 from the DN of all pixels within the
			specified area.  Value will bottom out at 0 as
			applicable.  This command may be abbreviated SUB.
'STATS			Stats will perform stats on the specified area.
			The Average DN, Standard Deviation, Min DN and Max DN
			are calculated.
'ZERO			Zero out the given area.  Same as SETTO=0 below.
SETTO=40		Set DN's within the indicated area to 40.  May
			be abbreviated SET.
'COPY			Copy input image to output file and screen.  Same as
			'RESTORE.  This is essentially the same as starting
			over.

'INTERP			Perform an EXTRAP interpolation over all interior
			points.  (See EXTRAP subroutine documentation.)
MIN=40			Set the minimum DN used in the interpolation
			formula to 40.  The default is -9999.
MAX=240			Set the maximum DN used in the interpolation
			formula to 240.  The default is 32768.
RADIUS=100000		Set the interpolation radius to 100000.  The
			default is 1 million.  (See OPERATION section.)
PERC=85			Specify that 85 percent of the points on the polygon
			border should be used for the interpolation.  The 
			default is 100%.

LINEAR=(150,200)    	Apply linear stretch to the display lookup table.
			Same as STRETCH.
CIRC=10			Command given at vertex acquisition level. It will
			gather points at a radius of 10 from the cursor
			location and perform specified function on interior
			points.
'RAW			Remove any applied stretch.
'OFF			Turn image display off.
'ON			Turn image display back on.

'DBUG			Produce debug print.
'HELP			Generate parameter summary.
'EXIT			Exit SARGON.  Equivalent to 'END.

'RCUR			Rcur will read and report the cursor position for the
			display and picture.
PCUR=(10,10)		Pcur will position the cursor according to picture
			coordinates given.
@FILE.EXT		Will execute commands found in file.ext where
			ext is any file extension.  Default ext is ".sar"
			so one could get away with just typing @file
			instead of @file.sar

RESTRICTIONS:
1) The maximum number of vertices is 24.
2) Polygon sides may not cross.
3) Maximum line length is 10000 bytes.
4) Maximum number of line segments to be interpolated for in any one polygon
   is 2000.
5) Maximum number of exterior points will be truncated at 4000.
6) Program is extremely picky about the data type passed to commands.  Be
   sure that you specify reals when it wants reals, and ints when it wants
   ints.

OPERATION:
SARGON first copies the input to the output so that all operations (with
the exception of COPY/RESTORE) will operate only on the output dataset.
It will always display the output dataset.

Whenever any of the keywords MULT, SUBT, ADD, DIVI, ZERO, COPY, SET, STATS, or
INTE (or their aliases) is specified, the Vertex Acquisition Cycle is
entered.

Each vertex of the polygon of interest is defined by trackball.  For each
push of the carriage return key within the Cycle, the current
location of the trackball is stored as a vertex.  A double carriage return
defines the last vertex.  The user should not repeat vertices; the program
will close the polygon automatically.  Or the circle mode may be used to
acquire vertices.  The command 'CIRC radius' would be used to invoke it
where 'radius' is an integer value.

The program will prompt the user to either accept or reject the polygon
drawn on the display.  If accepted, the output dataset is updated and the
appropriate lines are redisplayed.  The program is then ready for more
vertices and reports "READY FOR TRACKBALL."  Any parameter input will
exit the Cycle, winding up in "SARGON READY".

For interpolation, the points immediately exterior to the polygon are
collected.  Those exterior points with MIN <= DN <= MAX will be saved
for use.  If PERC is not 100, these exterior points will be randomly
weeded out until the proper percent remains.  These final points are then
fed to EXTRAP which interpolates.  For each point being interpolated for,
only exterior points within a radius of RADI will be used in the 
formula.  The use of RADI and PERC greatly determine the speed of the
algorithm for large areas.  (See EXTRAP document.)

MIN and MAX are important for painless interpolation.  For example, if
a large dark gore is to be removed, one can set MIN above the gore's
DN.  Then if one side of the polygon accidentally crosses into the gore,
the interpolation formula will ignore the border points collected from
within the gore.

One last thing to note.  Within the session one can execute a command
file similar to that found in IDX.  In order to do this, the "@" followed
by a file name must be given. (I.E. @commands - This will search for a
file by the name of "commands.sar" and execute it's contents.)  .SAR is
the default extension, otherwise the full file specification must be given.
.PAGE
HISTORY:

  WRITTEN BY:  Charles Avis, 1 March 1982
  REVISIONS:
    11 May 1984, Charlie Avis: converted to Vax
    12 July 1985, John Reimer: converted to Vicar2
    31 January 1986, Helen DeRueda: bug fixes, added parameters STATS, PCUR,
                                    RCUR, CIRC.
    20 February 1989, Tom Greer: Upgraded to R2LIB.  Wrote test plan, fixed
                                 bugs in OPRATE for ADD, SUB, MULT, DIV.  
                                 Fixed bug in SORTX.  Fixed bug in STRETCH.
    13 July 1992, Florance Moss: Added IHBUF in routine OPRATE to avoid access
                                 violation caused by DN out of range. (FR 66642)
                                 To test the FR, do the following commands :
                                 GEN A 512 512 LINC=256 SINC=256 'HALF
                                 SARGON A B
                                 RANGE -32768 32767
                                 ADD 20000.
                                 Use track ball to select an area in the bright 
                                 DN
    10 Dec. 1993, D.D. Knight:   Application made portable.
    12 Apr. 1996  O.A. Montoya : Bug fixes in OPRATE for ADD, SUB..., FR89204.
                                 Modified sargon.imake file to run on UNIX.
                                 

  COGNIZANT PROGRAMMER:  Tom Greer

.LEVEL1
.VARIABLE INP
 Input filename
.VARIABLE OUT
 Output filename
.VARIABLE FIT
Map to (0,255)
.VARIABLE RANGE
Same as FIT.
.VARIABLE RIGHT
Move window n pixels right.
.VARIABLE R
Same as RIGHT.
.VARIABLE LEFT
Move window n pixels left.
.VARIABLE L
Same as LEFT.
.VARIABLE UP
Move window n pixels up.
.VARIABLE U
Same as UP.
.VARIABLE DOWN
Move window n pixels down.
.VARIABLE D
 Same as DOWN.
.VARIABLE HOME
Put (1,1) at upper-left.
.VARIABLE LINEAR
Standard linear stretch.
.VARIABLE STRETCH
Same as LINEAR.
.VARIABLE RAW
Remove any applied stretch.
.VARIABLE OFF
Turn image display off.
.VARIABLE ON
Turn image display on.
.VARIABLE MULT
Multiply data by a constant.
.VARIABLE DIVIDE
Divide data by a constant.
.VARIABLE DIV
Same as DIVIDE.
.VARIABLE ADD
Add constant to data.
.VARIABLE SUBTRACT
Subtract constant from data.
.VARIABLE SUB
Same as SUBTRACT.
.VARIABLE MEAN
Calculate mean and standard
 deviation.
.VARIABLE ZERO
Set pixels to 0.
.VARIABLE SETTO
Set pixels to a constant.
.VARIABLE SET
Same as SETTO.
.VARIABLE COPY
Copy input file to output
 (start over).
.VARIABLE RESTORE
Same as COPY.
.VARIABLE INTERP
Perform interpolation.
.VARIABLE CIRC
Gather points within circle.
.VARIABLE MIN
Min DN value for interpolation.
.VARIABLE MAX
Max DN value for interpolation.
.VARIABLE RADIUS
Interpolation radius
.VARIABLE PERC
Percent of border points for
 interpolation.
.VARIABLE DBUG
Produce debug print.
.VARIABLE HELP
Produce printed summary.
.VARIABLE EXIT
Terminate program.
.VARIABLE END
Same as EXIT.
.VARIABLE RCUR
Read cursor position
.VARIABLE PCUR
Position cursor at specified
location.
.VARIABLE CIRC
Aquire vertices for specified
circle radius from current
cursor location.
.VARIABLE STATS
Calculate Average DN,
Standard Deviation, MINDN,
and MAXDN.
.LEVEL2
.VARIABLE INP
 File format must be byte or halfword.
.VARIABLE OUT
 A copy of the Input File where all operations are performed.
.VARIABLE FIT
INTEGER - For halfword data, two values to map to 0 and 255.
.VARIABLE RANGE
Same as FIT.
.VARIABLE RIGHT
INTEGER - Move display window n pixels right.
.VARIABLE R
Same as RIGHT.
.VARIABLE LEFT
INTEGER - Move display window n pixels left.
.VARIABLE L
Same as LEFT.
.VARIABLE UP
INTEGER - Move display window n pixels up.
.VARIABLE U
Same as UP.
.VARIABLE DOWN
INTEGER - Move display window n pixels down.
.VARIABLE D
Same as DOWN.
.VARIABLE HOME
KEYWORD - Put pixel 1,1 at upper-left corner of display.
.VARIABLE LINEAR
INTEGER - Two values for standard linear stretch of the display lookup table.
.VARIABLE STRETCH
Same as LINEAR.
.VARIABLE RAW
KEYWORD - Remove any applied stretch.
.VARIABLE OFF
KEYWORD - Turn image display off.
.VARIABLE ON
KEYWORD - Turn image display on.
.VARIABLE MULT
REAL - Multiply pixel data within specified area by a constant.
.VARIABLE DIVIDE
REAL - Divide pixel data within specified area by a constant.
.VARIABLE DIV
Same as DIVIDE.
.VARIABLE ADD
REAL - Add a constant value to pixel data within the specified area.
.VARIABLE SUBTRACT
REAL - Subtract a constant value from pixel data within the given area.
.VARIABLE SUB
Same as SUBTRACT.
.VARIABLE MEAN
KEYWORD - Calculate the mean and standard deviation for specified area.
.VARIABLE ZERO
KEYWORD - Set pixels within the specified area to 0.
.VARIABLE SETTO
INTEGER - Set pixels within the specified area to a constant value.
.VARIABLE SET
Same as SETTO.
.VARIABLE COPY
KEYWORD - Copy input file to output file (start over).
.VARIABLE RESTORE
Same as COPY.
.VARIABLE INTERP
KEYWORD - Perform an EXTRAP interpolation over interior points.
.VARIABLE CIRC
INTEGER - Will gather points from cursor location at a radius given.
.VARIABLE INTERP
'INTERP performs an EXTRAP interpolation over all interior points.
See the EXTRAP subroutine document for more information.
.VARIABLE MIN
Sets the minimum DN used in the interpolation formula.  The default is -9999.
.VARIABLE MAX
Sets the maximum DN used in the interpolation formula.  The default is 32768.
.VARIABLE RADIUS
Sets the interpolation radius to 1,000,000.  The default is 100,000.  See
the OPERATION section of this documentation for more information.
.VARIABLE PERC
Specifies the percent of all points on the polygon border which will be used
in the interpolation formula.  The default is 100.
.VARIABLE RCUR
Read cursor position and display on the screen the picture and display
coordinates.
.VARIABLE PCUR
Position cursor at specified picture location.
.VARIABLE CIRC
Aquire vertices for specified circle radius from current cursor location.
At SARGON READY prompt or VERTEX ACQUISITION level move cursor to position
wanted and then type CIRC ##, where ## is an integer number.
.VARIABLE STATS
Calculate Average DN, Standard Deviation, MINDN, and MAXDN for the specified
area.
.END
$ Return
$!#############################################################################
