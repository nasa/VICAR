$!****************************************************************************
$!
$! Build proc for MIPL module incon
$! VPACK Version 1.5, Monday, March 29, 1993, 16:25:07
$!
$! Execute by entering:		$ @incon
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
$ write sys$output "*** module incon ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("incon.imake") .nes. ""
$   then
$      vimake incon
$      purge incon.bld
$   else
$      if F$SEARCH("incon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake incon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @incon.bld "STD"
$   else
$      @incon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create incon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack incon.com -
	-s incon.f -
	-i incon.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create incon.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE INCON(NUMARGS,NPAR, IN, OUT, NBYT)


      IMPLICIT INTEGER (A-Z)

      CHARACTER*1 IN(*)
      INTEGER OUT(*)
C   buffer holding current token
      CHARACTER*1 TOK(576)  

      LOGICAL MORE, ENDTOK, EXPFLG

      CHARACTER*1 XBL, XCM, XEQ, XQU, XPE, XPL, XMN, XB, XX, XE, XD
      CHARACTER*1 XINT(10),XHEX(6)
      DATA  XBL/' '/, XCM/','/, XEQ/'='/, XQU/''''/,
     . XPE/'.'/, XPL/'+'/,
     . XMN/'-'/,
     . XB/'B'/, XX/'X'/, XE/'E'/, XD/'D'/, 
     . XINT/'0','1','2','3','4','5','6','7','8','9'/,
     . XHEX/'A','B','C','D','E','F'/

C INITIALIZE
      NB = 71
      IF (NUMARGS.EQ.4) NB = NBYT
C   current position (byte) in IN
      P = 0  
C   current position in current token (TOK)
      PT = 0  
      NPAR = 0
C  end of input buffer flag
      MORE = .TRUE. 
C  end of token flag
      ENDTOK = .FALSE. 
C  exponent flag for reals
      EXPFLG = .FALSE. 
      TYPE = 0
C type:  0 = no token, 1 = quoted string, 2 = integer, 3 = hex, 4 = binary, 
C        5 = real, 6 = DWAC

C*****************************************************************
C PROCESSING LOOP:

2     IF (.NOT.(MORE)) GOTO 3

        P = P+1
        IF (P.EQ.NB) MORE = .FALSE.

        IF (TYPE.EQ.0) THEN
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   NO TOKEN

          IF (IN(P).EQ.XX .OR. IN(P).EQ.XB) THEN

C   check for standalone B/X
            IF (.NOT.MORE) THEN  
              TYPE = 6
              PT = PT+1
              TOK(PT) = IN(P)
            ELSEIF (IN(P+1).EQ.XBL .OR. IN(P+1).EQ.XEQ .OR.
C   again
     .             IN(P+1).EQ.XCM) THEN  
              TYPE = 6
              PT = PT+1
              TOK(PT) = IN(P)
            ELSEIF (IN(P).EQ.XX) THEN
C   leading X may signal hex
              TYPE = 3  
            ELSE
C   leading B may signal binary
              TYPE = 4  
            ENDIF

          ELSEIF (IN(P).EQ.XPE) THEN

C    leading period signals real
            TYPE = 5  
            PT = PT+1
            TOK(PT) = IN(P)

          ELSEIF (IN(P).EQ.XQU) THEN

            IF (MORE) THEN

              COUNT = 1
C  check if multiple quotes
4             IF (.NOT.(IN(P+1).EQ.XQU.AND.MORE) ) GOTO 5
                P = P+1
                COUNT = COUNT+1
                IF (P.EQ.NB) MORE = .FALSE.
              GOTO 4
5             CONTINUE

C   number of extracted quotes
              CNT1 = COUNT/2  
C   if odd # of quotes
              IF (2*CNT1.NE.COUNT) THEN  
C   quoted string
                TYPE = 1  
              ELSE
C   DWAC
                TYPE = 6  
              ENDIF

              DO 6  I = 1, CNT1
                PT = PT+1
                TOK(PT) = XQU
6             CONTINUE

            ELSE

	      CALL XVMESSAGE(' Unmatched final quote ignored (INCON)',
     &            	' ')

            ENDIF

C   integer, DWAC, or nothing
          ELSE  

C   DWAC
            TYPE = 6  
            DO 7  I = 1, 10
C   integer
              IF (IN(P).EQ.XINT(I)) TYPE = 2  
7           CONTINUE
C  +/- is integer first
            IF (IN(P).EQ.XPL.OR.IN(P).EQ.XMN) TYPE = 2 
C nothing
            IF (IN(P).EQ.XBL.OR.IN(P).EQ.XCM.OR.IN(P).EQ.XEQ) TYPE = 0 

            IF (TYPE.NE.0) THEN
              PT = PT+1
              TOK(PT) = IN(P)
            ENDIF

          ENDIF

        ELSEIF (TYPE.EQ.1) THEN
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C    QUOTED STRING

          IF (.NOT.MORE) THEN

            IF (IN(P).NE.XQU)
     .              CALL QPRINT(' ** INCON: FINAL QUOTE ASSUMED **',33)
            ENDTOK = .TRUE.

C   if more
          ELSE  

            IF (IN(P).EQ.XQU) THEN

              IF (IN(P+1).EQ.XQU) THEN

C   skip one quote
                P = P+1  
                PT = PT+1
                TOK(PT) = IN(P)

C  single quote: terminate string
              ELSE  

                ENDTOK = .TRUE.

              ENDIF

C  if char. isn't quote
            ELSE  

              PT = PT+1
              TOK(PT) = IN(P)

            ENDIF
          ENDIF

C   a token other than quoted string
        ELSE  
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

C   LOOK FOR TERMINATORS
C   A QUOTE TERMINATES CURRENT TOKEN AND STARTS NEW STRING

          IF (IN(P).EQ.XBL.OR.IN(P).EQ.XEQ.OR.IN(P).EQ.XCM) THEN

            ENDTOK = .TRUE.

          ELSEIF (IN(P).EQ.XQU) THEN

            ENDTOK = .TRUE.
            P = P-1

          ELSEIF (TYPE.EQ.2) THEN
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C    INTEGER

            IF (IN(P).EQ.XPE) THEN

C   switch to real
              TYPE = 5  

C   not period
            ELSE  

C   switch to DWAC if invalid char.
              TYPE = 6  
C   check valid integers
              DO 8  I = 1, 10  
C   keep as integer
                IF (IN(P).EQ.XINT(I)) TYPE = 2  
8             CONTINUE

            ENDIF

            PT = PT+1
            TOK(PT) = IN(P)

          ELSEIF (TYPE.EQ.3) THEN
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C    HEX

C   switch to DWAC if invalid char.        
            TYPE = 6  
            DO 9  I =1, 10
C   keep as hex
              IF (IN(P).EQ.XINT(I)) TYPE = 3  
9           CONTINUE
            DO 10  I =1, 6
C   keep as hex
              IF (IN(P).EQ.XHEX(I)) TYPE = 3  
10          CONTINUE

C   if DWAC, insert 'X' into first pos.
            IF (TYPE.EQ.6) THEN  
              DO 11  J = 1, PT
                TOK(PT+2-J) = TOK(PT+1-J)
11            CONTINUE
              TOK(1) = XX
              PT = PT+1
            ENDIF

            PT = PT+1
            TOK(PT) = IN(P)

          ELSEIF (TYPE.EQ.4) THEN
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C    BINARY

C   switch to DWAC if invalid char.        
            TYPE = 6  
            DO 12  I =1, 2
C   keep as binary
              IF (IN(P).EQ.XINT(I)) TYPE = 4  
12          CONTINUE

C   if DWAC, insert 'B' into first pos.
            IF (TYPE.EQ.6) THEN  
              DO 13  J = 1, PT
                TOK(PT+2-J) = TOK(PT+1-J)
13            CONTINUE
              TOK(1) = XB
              PT = PT+1
            ENDIF

            PT = PT+1
            TOK(PT) = IN(P)

          ELSEIF (TYPE.EQ.5) THEN
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C    REAL  (note that a period must already have been found)

C   switch to DWAC if invalid char.
            TYPE = 6  
C   check valid integers
            DO 14  I = 1, 10  
C   keep as real
              IF (IN(P).EQ.XINT(I)) TYPE = 5  
14          CONTINUE
            IF (.NOT.EXPFLG.AND.(IN(P).EQ.XE.OR.IN(P).EQ.XD)) THEN
C  one D or E is valid for real
              TYPE = 5  
C  set exponent flag
              EXPFLG = .TRUE.  
            ENDIF

            PT = PT+1
            TOK(PT) = IN(P)

          ELSE
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C    DWAC (DOUBLE WORD ALPHAMERIC CONSTANT)

            PT = PT+1
            TOK(PT) = IN(P)

C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
          ENDIF

        ENDIF

        IF (ENDTOK.OR..NOT.MORE) THEN

C  convert token
          CALL CONTOK(TOK, PT, TYPE, OUT(NPAR+1),NP) 
          NPAR = NPAR+NP

C RESET INDICES & FLAGS
          PT = 0
          ENDTOK = .FALSE.
          EXPFLG = .FALSE.
          TYPE = 0

        ENDIF

      GOTO 2
3     CONTINUE

      RETURN
      END
C**************************************************************************
      SUBROUTINE CONTOK( TOK, NBYT, TYPE, OUT, NW)

C  83-8-4   ...LWK...

C This routine converts a token TOK, containing NBYT bytes, 
C to NW fullwords in OUT of type specified by TYPE.

C  TYPE = 1: TOK is copied byte-by-byte to (NB+3)/4 fullwords
C            in OUT. The last fullword is padded with blanks
C            if necessary.

C       = 2: decimal digits in TOK are converted to one
C            integer fullword in OUT.

C       = 3: hex digits in TOK are converted to one integer
C            fullword in OUT. If >8 digits, it is truncated
C            on the right.

C       = 4: binary digits in TOK are converted to one
C            integer fullword in OUT. If >32 digits, it
C            is truncated on the right.

C       = 5: decimal digits containing decimal point and
C            optionally an exponent field (E or D) in TOK
C            are converted to a real number stored in one
C             fullword in OUT.

C       = 6: the first 8 bytes of TOK are copied into two
C            fullwords of OUT, padded on the right with blanks
C            if necessary.

      IMPLICIT INTEGER (A-Z)

      INTEGER OUT(*)
      CHARACTER*1 TOK(*)

      INTEGER IBUF, ISIGN
      CHARACTER*1 BBUF(4)
      EQUIVALENCE (BBUF, IBUF)

      REAL*8 DBUF, DEC, DSIGN, DEXP
      REAL*4 RBUF
      INTEGER WBUF
      EQUIVALENCE (WBUF, RBUF)

      LOGICAL MORE
      CHARACTER*1 XBL, XPL, XMN, XE, XD
      INTEGER IMAX

        DATA  XBL/' '/, XPL/'+'/, XMN/'-'/, XE/'E'/, XD/'D'/
        DATA  IMAX/X'7FFFFFFF'/
      NW = 0
      IF (TYPE.LE.0.OR.TYPE.GT.6) RETURN

      IF (TYPE.EQ.1.OR.TYPE.EQ.6) THEN

        NB = NBYT
        IF (TYPE.EQ.6) NB = 8
        MORE = .TRUE.
        P = 0
        NW = 0
15      IF (.NOT.(MORE)) GOTO 16
          NW = NW+1
          DO 17  I = 1, 4
            P = P+1
            IF (P.EQ.NB) MORE = .FALSE.
            IF (P.LE.NBYT) THEN
              BBUF(I) = TOK(P)
            ELSE
              BBUF(I) = XBL
            ENDIF
17        CONTINUE
          OUT(NW) = IBUF
        GOTO 15
16      CONTINUE

      ELSEIF (TYPE.EQ.2) THEN

        ISIGN = 1
        N1 = 1
        IF (TOK(1).EQ.XPL) THEN
          N1 = 2
        ELSEIF (TOK(1).EQ.XMN) THEN
          N1 = 2
          ISIGN = -1
        ENDIF

        IF ((NBYT-N1).GT.8) THEN
C   TO PREVENT OVERFLOW
          IPAR = IMAX  
        ELSE
C   INITIALIZE
          IPAR = 0  
          DO 18  I=N1, NBYT
C   CONVERT ASCII TO DIGIT
C           DIGIT = TOK(I)-48  
            CALL MVE(5,1,TOK(I),DIGIT,1,1)
            DIGIT = DIGIT - 48
C   SHIFT DECIMAL PLACE UP
            IPAR = IPAR*10  
C   ADD DIGIT
            IPAR = IPAR+DIGIT  
18        CONTINUE
        ENDIF

        OUT(1) = ISIGN*IPAR
        NW = 1

      ELSEIF (TYPE.EQ.3) THEN

        ISIGN = 1
        IF (IVAL(TOK(1)).GT.56) ISIGN = -1
        NB = MIN(NBYT, 8)

C   INITIALIZE
        IPAR = 0  
        DO 19  I=1, NB
C   CONVERT ASCII TO DIGIT
C         DIGIT = TOK(I)-48  
          CALL MVE(5,1,TOK(I),DIGIT,1,1)
          DIGIT = DIGIT - 48
C   A-F
          IF (DIGIT.GT.9) DIGIT = DIGIT-7  
          IF (ISIGN.EQ.-1) DIGIT = 16-DIGIT
C   SHIFT HEX PLACE UP
          IPAR = IPAR*16  
C   ADD DIGIT
          IPAR = IPAR+DIGIT  
19      CONTINUE
        OUT(1) = ISIGN*IPAR
        NW = 1

      ELSEIF (TYPE.EQ.4) THEN

        ISIGN = 1
        IF (IVAL(TOK(1)).EQ.1) ISIGN = -1
        NB = MIN(NBYT, 32)

C   INITIALIZE
        IPAR = 0  
        DO 20  I=1, NBYT
C   CONVERT ASCII TO DIGIT
C         DIGIT = TOK(I)-48  
          CALL MVE(5,1,TOK(I),DIGIT,1,1)
          DIGIT = DIGIT - 48
          IF (ISIGN.EQ.-1) DIGIT = 1-DIGIT
C   SHIFT BINARY PLACE UP
          IPAR = IPAR*2  
C   ADD DIGIT
          IPAR = IPAR+DIGIT  
20      CONTINUE
        OUT(1) = ISIGN*IPAR
        NW = 1

      ELSEIF (TYPE.EQ.5) THEN

        DSIGN = 1.
        N1 = 1
        IF (TOK(1).EQ.XPL) THEN
          N1 = 2
        ELSEIF (TOK(1).EQ.XMN) THEN
          N1 = 2
          DSIGN = -1.
        ENDIF

        NB = NBYT
C   LOOK FOR EXPONENT
        DO 21  I = 1, NBYT  
          IF (TOK(I).EQ.XE.OR.TOK(I).EQ.XD) NB = I-1
21      CONTINUE

C   INITIALIZE
        DBUF = 0.  
        DEC = 0.
        DO 22  I=N1, NB
          IF (IVAL(TOK(I)).EQ.46) THEN
            DEC = 1.
          ELSE
C   CONVERT ASCII TO DIGIT
C           DIGIT = TOK(I)-48  
            CALL MVE(5,1,TOK(I),DIGIT,1,1)
            DIGIT = DIGIT - 48
            IF (DEC.EQ.0.) THEN
C   SHIFT DECIMAL PLACE UP
              DBUF = DBUF*10  
C   ADD DIGIT
              DBUF = DBUF+DIGIT  
            ELSE
C  SHIFT DECIMAL PLACE DOWN
              DEC = DEC/10.  
C  ADD DIGIT
              DBUF = DBUF+DIGIT*DEC  
            ENDIF
          ENDIF
22      CONTINUE

        DEXP = 1.
C   IF EXPONENT
        IF (NB.NE.NBYT) THEN  
          ISIGN = 1
          EXP = 0
          DO 23  I = NB+2, NBYT
            IF (TOK(I).EQ.XMN) THEN
              ISIGN = -1
            ELSEIF (TOK(I).NE.XPL) THEN
C             DIGIT = TOK(I)-48
              CALL MVE(5,1,TOK(I),DIGIT,1,1)
              DIGIT = DIGIT - 48
              EXP = EXP*10
              EXP = EXP+DIGIT
            ENDIF
23        CONTINUE
          EXP = ISIGN*EXP
          DEXP = 10.**EXP
        ENDIF

        DBUF = DSIGN*DBUF*DEXP
C *** should be rounded
        RBUF = DBUF                
        OUT(1) = WBUF
        NW = 1

      ENDIF

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create incon.imake
#define SUBROUTINE incon

#define MODULE_LIST incon.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
