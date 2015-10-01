$!****************************************************************************
$!
$! Build proc for MIPL module f2
$! VPACK Version 1.8, Friday, April 16, 1999, 19:43:04
$!
$! Execute by entering:		$ @f2
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
$ write sys$output "*** module f2 ***"
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
$ write sys$output "Invalid argument given to f2.com file -- ", primary
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
$   if F$SEARCH("f2.imake") .nes. ""
$   then
$      vimake f2
$      purge f2.bld
$   else
$      if F$SEARCH("f2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake f2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @f2.bld "STD"
$   else
$      @f2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create f2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack f2.com -
	-s f2.f f2.fin -
	-i f2.imake -
	-p f2.pdf -
	-t tstf2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create f2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
C
C   **** NOTE: When you Modify this file change the "Version" message!
C
C   REVSION HISTORY:
C
C     16 Apr 99   ...REA...   Fixed a bug that permitted a hash table index
C                             of zero.
C     04 Feb 94   ...NDR...   Fixed Improper handling of 'LINE','SAMP'
C                             arguments when SL != 1. Ref. FR#81717. 
C     24 Aug 93   ...NDR...   Fixed "Goto jumps into IF Block" warning.
C                             REF FR#83013.
C     19 Mar 93   ...SP....   Corrected initialization of hash table.
C     25 FEB 93   ...SP....   Merged in the capabilities to handle
C			      3D images from F2_3D. Corrected unlikely
C                             overflow error computing CODE for hash table.
C     28 AUG 92   ...SP....   ADDED VERSION NUMBER PRINT OUT.
C     13 JUL 92   ...SP....   ADDED SUBSET VARIABLE TO AVOID XVREAD OPTIONALS
C                             FOR PERFORMANCE.  CORRECTED DIMENSION OF
C                             INUNIT TO MAXIN.  CHANGED TO 1-D ARRAYS FOR
C                             SPEED IN BYTE TABLE LOOKUP LOOP.
C     07 JUL 92   ...NDR...   FIXED LOOKUP TABLE PROBLEM WITH SIGNED BYTES.
C     30 APR 92   ...NDR...   ADDED "DUMP" KEYWORD -- MAKES SYMBOLIC CODE
C                               DUMP OPTIONAL (OMITTED BY DEFAULT).
C     20 APR 92   ...NDR...   ELIMINATED LOGICAL*1 VARS, MODULARIZED CODE.
C     19 MAR 92   ...NDR...   FIXED "INTEGER OVERFLOW" ON "EXCLUDE" OPTION
C                             REF. FR#75716.
C     09 JAN 92   ...NDR...   CLEANED UP CODE; REMOVED "TEMPORARY" COMMENTS
C                  ABOUT R*4 AND I*4 CONVERSIONS IN XVOPEN -- REPLACEMENT
C                  CODE IN COMMENTS BECAME INVALID WHEN BYTE LOOKUP TABLE
C                  AND HALFWORD HASHING WAS INSTALLED.
C     07 AUG 91   ...NDR...   MODIFICATIONS FOR UNIX COMPATIBILITY AND
C                  HOST BYTE-ORDER INDEPENDENCE. USES NEW UNIX VERSION
C                  OF KNUTH, XKNUTH,XKN65_EXE. ELIMINATED CALLS TO PRNT,
C                  OUTCON AND BINBCD. CVSCI ROUTINE REMOVED, AS THE
C                  NEW KNUTH ROUTINES CORRECTLY HANDLE SCI. NOTATION.
C                  NEGATIVE NUMBERS NO LONGER NEED BE IN ()'S AND MANY
C                  NEW C-LANGUAGE OPS ARE NOW RECOGNIZED.
C     04 DEC 90   ...JFM...   WRITE COMMENTS ADDED TO TEST FILE; INVALID
C                  REPLC VALUES HANDLED DIFFERENTLY.
C     05 SEP 90   ...JFM...   FUNCTION STRING SPECIFIED BY USER EXPANDED TO 
C                  250 CHARACTERS.  EXCLUDE, LIMITS AND REPLC
C                  PARAMETERS ADDED TO ALLOW SPECIFIED DN VALUES
C                  FROM BEING INCLUDED IN ARITHMETIC.  NEGATIVE 
C                  NUMBERS MUST ALWAYS BE IN PARENTHESIS.
C
C     27 FEB 89   ...SP....   REMOVED LOCAL VERSION OF KNUTH, CORRECTED
C                             HANDLING OF SCI. NOTATION, ENLARGED BUFFER
C                             FOR FUNCTION STRING.  CORRECTED PROBLEM WITH
C                             XLADD BLOWING UP BECAUSE STRING NOT 
C                             NULL-TERMINATED IF NO INPUT FILES.
C     26 JUL 88   ...SP....   ADDED OPEN_ACT ON XVOPENS.
C     1  JAN 88   ...EJB...   FIXED A BUG IN SCI. NOTAT. ROUTINE THAT CONFUSED
C                             #.E#  WITH THE LOGICAL OPERATOR .EQ.
C     18 SEP 87   ...EJB...   CHANGED TO ACCEPT REAL NUMBERS/VARIABLES IN
C                             THE FUNCTION STRING, ESP SCIENTIFIC NOTAT.
C     30 AUG 85   ...FFM...   IF KEYWORD "FORMAT" IS NOT SPECIFIED,THEN THE
C                             OUTPUT FORMAT WILL DEFAULT TO THE PRIMARY 
C                             INPUT FORMAT
C     30 AUG 85   ...FFM...   CHANGE "ROUND" TO BE THE DEFAULT INSTEAD OF 
C                             "TRUNCATE"
C     16 JUL 84   ...DHF...   ALLOWS UP TO 18 INPUTS
C     16 JUL 84   ...DHF...   ALLOWS MIXED FORMAT TYPES (BYTE,HALF,FULL,REAL)
C     16 JUL 84   ...DHF...   CONVERT TO VICAR2 I/O ROUTINES
C     01 JUL 84   ...DHF...   COMPILE FUNCTION TO ASSEMBLR CODE FOR FASTER RUNS
C     01 MAR 84   ...ALZ...   CONVERT TO VAX FROM IBM
C     15 DEC 80   ...ALZ...   CORRECTION TO LABEL PROCESSING
C     10 AUG 80   ...ALZ...   CORRECTION TO SIZE FLD PARAM USE
C     15 FEB 80   ...ALZ...   DELETE CALL ABEND IN SUBROUTINE KNUTH
C     15 FEB 80   ...ALZ...   ALLOW USER LABELS, KEYWORD 'ROUND' ADDED
C     18 OCT 79   ...ALZ...   INITIAL RELEASE
C     01 AUG 75   ...ALZ...   PROGRAM WRITTEN BY A ZOBRIST


      INCLUDE 'f2.fin'
      DATA CASE, XKC,ROUND,KFLAGS,FMT /1,0,0.5,1,MAXIN*'    '/
C==================================================================
      call xvmessage('F2 version 4-14-99', ' ')

      CALL GET_FUNCTION

      CALL OPEN_FILES

      CALL GET_CONTROL_PARMS

      CALL INITIALIZE_TABLES

      CALL PROCESS_IMAGES

      CALL CLOSE_FILES

      RETURN
      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE GET_FUNCTION
      INCLUDE 'f2.fin'
      INTEGER COUNT,IER
      LOGICAL XVPTST

C ---    grab and parse the function
      CALL XVP ('FUNCTION', FCN, COUNT)
      CALL KNUTH (FCN,FBUF,IER)

      IF (IER.EQ.2) THEN
         CALL XVMESSAGE('BAD FUNCTION STRING',' ')
         CALL ABEND
         RETURN
      ELSEIF (IER.EQ.3) THEN
         CALL XVMESSAGE('EVALUATION ERROR',' ')
         CALL ABEND
         RETURN
      ENDIF

      IF (IER.EQ.1) THEN    ! LINE, SAMP, BAND are referenced
          CASE=3
          CALL KNUTH_VAR('LINE',INDEX_L) !Gets buffer index of LINE
          CALL KNUTH_VAR('SAMP',INDEX_S)
          CALL KNUTH_VAR('BAND',INDEX_B)
      ENDIF

C --- Display the symbolic code dump:
      IF (XVPTST('DUMP'))  CALL KNUTH_DUMP(FBUF)
      RETURN

      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE OPEN_FILES
      INCLUDE 'f2.fin'
      CHARACTER*4 FMT_CODE(4) 
      DATA FMT_CODE/'BYTE','HALF','FULL','REAL'/
      INTEGER  FMT_SIZE(4) 
      DATA FMT_SIZE/1, 2, 4, 4/
      INTEGER NL, NS, NL1, NS1, NB, NB1,DUMMY, MAXBAND, MAXLINE, MAXSAMP
      INTEGER COUNT,STATUS,I
      CHARACTER*36 MSG
      INTEGER  IN1C, IN2C, IN1S, IN2S, OUTS
      DATA IN1S,IN2S/0,0/
      DATA IN1C,IN2C/1,1/


C ---    Check to see if no input files
      CALL XVPCNT ('INP', NUM_INPUT)
      IF (NUM_INPUT .EQ. 0) THEN
          CASE = 3
          CALL XVUNIT (OUTUNIT, 'OUT', 1, STATUS,' ')
          CALL XVSIZE (SL, SS, NLINE, NSAMP,NL1,NS1)
          CALL XVBANDS(SB,NBAND,DUMMY)
          IF (NBAND .LT. 1) NBAND=1      ! IF NO BAND INFO ENTERED.
          IF (SB    .LT. 1) SB   =1
	  CALL XVP ('ORG', ORG, COUNT)
   	  IF (COUNT.EQ.0) ORG = 'BSQ'
          CALL XVP ('FORMAT', OUTFMT, COUNT)
          IF (COUNT .EQ. 0) OUTFMT='BYTE'
          DO I = 1,4
              IF (OUTFMT .EQ. FMT_CODE(I))  OUTC=I
          ENDDO
          IF (OUTFMT.NE.'BYTE') THEN
              CALL XVOPEN (OUTUNIT, STATUS, 'OP', 'WRITE',
     +            'O_FORMAT',OUTFMT, 'U_NL', NLINE, 'U_NS', NSAMP,
     +            'U_NB',NBAND,'U_ORG',ORG,
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
          ELSE
              CALL XVOPEN (OUTUNIT, STATUS, 'OP', 'WRITE','U_FORMAT',
     +          'HALF','O_FORMAT','BYTE','U_NL', NLINE, 'U_NS', NSAMP,
     +          'U_NB',NBAND,'U_ORG',ORG,
     +          'OPEN_ACT','SA','IO_ACT','SA',' ')
          ENDIF
          GOTO 10
      ENDIF

C ---    Get the unit number for I/O files
      DO I = 1, NUM_INPUT
          CALL  XVUNIT (INUNIT(I), 'INP', I, STATUS,' ')
          CALL  XVOPEN (INUNIT(I), STATUS,
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
      ENDDO
      CALL  XVUNIT (OUTUNIT, 'OUT', 1, STATUS,' ')

C ---    Get the size of the I/O files and the starting line and samp
      CALL  XVSIZE (SL, SS, NLINE, NSAMP, NL1, NS1)
      CALL  XVBANDS(SB,NBAND,NB1)

      MAXLINE = NLINE
      MAXSAMP = NSAMP
      MAXBAND = NBAND
      DO  I = 1, NUM_INPUT
          CALL XVGET (INUNIT(I), STATUS, 'NL', NL, 'NS', NS,'NB',NB,' ')
          NLINE = MIN0(NLINE, NL-SL+1)
          NSAMP = MIN0(NSAMP, NS-SS+1)
	  NBAND = MIN0(NBAND, NB-SB+1)
          MAXLINE = MAX0 (MAXLINE, NL)
          MAXSAMP = MAX0 (MAXSAMP, NS)
	  MAXBAND = MAX0 (MAXBAND, NB)
      ENDDO

C ---    Warning notices
      IF (MAXLINE .NE. NLINE)  CALL XVMESSAGE('LINES TRUNCATED',' ')
      IF (MAXSAMP .NE. NSAMP)  CALL XVMESSAGE('SAMPLES TRUNCATED',' ')
      IF (MAXBAND .NE. NBAND)  CALL XVMESSAGE('BANDS TRUNCATED',' ')
C ---    Get the format types
C    The types are 'BYTE', 'HALF', 'FULL', 'REAL'
C    The corresponding sizes (# of bytes) are 1,2,4,4
      DO I = 1, NUM_INPUT
          CALL XVGET (INUNIT(I), STATUS, 'FORMAT', FMT(I),' ')
	  IF (I.EQ.1) CALL XVGET (INUNIT(I), STATUS, 'ORG', ORG, ' ')
      ENDDO

      IF (ORG .EQ. 'BSQ' .AND. SS .EQ. 1 .AND. NSAMP .EQ. NS1) THEN
         SUBSET = .FALSE.   ! SEE IF WE CAN SKIP OPTIONALS IN XVREAD.    
      ELSE IF (ORG .EQ. 'BIL' .AND. SS .EQ. 1 .AND. NSAMP .EQ. NS1) THEN
         SUBSET = .FALSE.   ! SEE IF WE CAN SKIP OPTIONALS IN XVREAD.    
      ELSE IF (ORG .EQ. 'BIP' .AND. SB .EQ. 1 .AND. NBAND .EQ. NB1) THEN
         SUBSET = .FALSE.   ! SEE IF WE CAN SKIP OPTIONALS IN XVREAD.    
      ELSE 
         SUBSET = .TRUE. 
      END IF

        IF (FMT(1).EQ.'BYTE') THEN
               IN1C=1
        ELSE IF(FMT(1).EQ.'HALF')THEN
               IN1C=2
        ELSE IF(FMT(1).EQ.'WORD')THEN
               IN1C=2
        ELSE IF(FMT(1).EQ.'FULL')THEN
               IN1C=3
        ELSE IF(FMT(1).EQ.'REAL')THEN
               IN1C=4
        ELSE IF(FMT(1).EQ.'    ')THEN
               IN1C=1
        ELSE
              WRITE(MSG,'(A16,A4)') ' INVALID FORMAT:',FMT(1)
              CALL XVMESSAGE(MSG,' ')
              CALL ABEND
        ENDIF
        IF (FMT(2).EQ.'BYTE') THEN
               IN2C=1
        ELSE IF(FMT(2).EQ.'HALF')THEN
               IN2C=2
        ELSE IF(FMT(2).EQ.'WORD')THEN
               IN2C=2
        ELSE IF(FMT(2).EQ.'FULL')THEN
               IN2C=3
        ELSE IF(FMT(2).EQ.'REAL')THEN
               IN2C=4
        ELSE IF(FMT(2).EQ.'    ')THEN
               IN2C=1
        ELSE
              WRITE(MSG,'(A16,A4)') ' INVALID FORMAT:',FMT(2)
              CALL XVMESSAGE(MSG,' ')
              CALL ABEND
        ENDIF
        CALL  XVP ('FORMAT', OUTFMT, COUNT)
        IF (COUNT .EQ. 1) THEN
               DO  I = 1, 4
          IF (OUTFMT .EQ. FMT_CODE(I))  OUTC=I
            ENDDO
        ELSE
            OUTC=IN1C
            IF (FMT(1) .EQ. '    ') FMT(1)='BYTE'
            OUTFMT=FMT(1)
        END IF
C*************************************************************
      IN1S = FMT_SIZE(IN1C)
      IN2S = FMT_SIZE(IN2C)
      OUTS = FMT_SIZE(OUTC)

C --- close the files so that they can be reopened with correct format
      DO  I = 1, NUM_INPUT
          CALL  XVCLOSE(INUNIT(I), STATUS,' ')
      ENDDO

C ---    Determine which case to use in processing the image
C    The three cases for image processing are
C    CASE = 1:    Byte Table    (BYTE,BYTE) => BYTE
C    CASE = 2:    Hashing    HALFs    (HALF,HALF) => HALF
C            Small byte files
C    CASE = 3:    Functions using line and samp
C            I*4 or R*4 data

       IF (((NUM_INPUT.EQ.1.AND.IN1S+OUTS.EQ.2) .OR.
     +    (NUM_INPUT.EQ.2 .AND. IN1S+IN2S+OUTS.EQ.3 .AND.
     +    NLINE*NSAMP.GT.40000)) .AND. CASE .NE. 3)   THEN
          CASE = 1
          DO I = 1, NUM_INPUT
              CALL XVOPEN (INUNIT(I), STATUS,
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
          ENDDO
          CALL  XVOPEN (OUTUNIT, STATUS, 'OP', 'WRITE', 'U_NL', NLINE,
     +            'U_NS', NSAMP, 'U_NB',NBAND,'U_ORG',ORG,
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
      ELSE IF (((NUM_INPUT.EQ.1.AND.IN1S+OUTS.LE.4).OR.
     +    (NUM_INPUT.EQ.2 .AND. IN1S+IN2S.LE.4 .AND.OUTS.LE.2)
     +     ) .AND. CASE.NE.3) THEN
          CASE = 2
          DO I = 1, NUM_INPUT
              CALL  XVOPEN (INUNIT(I), STATUS, 'U_FORMAT', 'HALF',
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
          ENDDO
            CALL XVOPEN(OUTUNIT, STATUS,'OP','WRITE','O_FORMAT',OUTFMT,
     +            'U_FORMAT', 'HALF', 'U_NL', NLINE, 'U_NS', NSAMP,
     +            'U_NB',NBAND,'U_ORG',ORG,
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
      ELSE
          CASE = 3
          DO I = 1, NUM_INPUT

           IF (FMT(I).NE.'BYTE') THEN
              CALL  XVOPEN (INUNIT(I), STATUS,
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
           ELSE
              CALL  XVOPEN (INUNIT(I), STATUS, 'U_FORMAT', 'HALF',
     +            'OPEN_ACT','SA','IO_ACT','SA',' ')
           ENDIF

          ENDDO

          IF (OUTFMT.NE.'BYTE') THEN
             CALL  XVOPEN (OUTUNIT, STATUS, 'OP', 'WRITE', 'O_FORMAT',
     +        OUTFMT, 'U_FORMAT', OUTFMT, 'U_NL', NLINE, 'U_NS', NSAMP,
     +        'U_NB',NBAND,'U_ORG',ORG,
     +        'OPEN_ACT','SA','IO_ACT','SA',' ')
          ELSE
             CALL  XVOPEN (OUTUNIT, STATUS, 'OP', 'WRITE', 'O_FORMAT',
     +        OUTFMT, 'U_FORMAT', 'HALF', 'U_NL', NLINE, 'U_NS', NSAMP,
     +        'U_NB',NBAND,'U_ORG',ORG,
     +        'OPEN_ACT','SA','IO_ACT','SA',' ')
          ENDIF
      ENDIF

C ---    Add the function to the output history label
 10   CONTINUE
      CALL  XLADD (OUTUNIT,'HISTORY','FUNCTION',FCN,STATUS,'FORMAT',
     +        'STRING',' ')

      RETURN
      END
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE GET_CONTROL_PARMS
      INCLUDE 'f2.fin'
      INCLUDE 'fortport'
      INTEGER COUNT,I
      LOGICAL XVPTST

C ---     TYPE OF EXCLUSION
C    TYPEXC 1: NO EXCLUSION OR LIMITS SPECIFIED
C    TYPEXC 2: EXCLUDE PARAMETER SPECIFIED
C    TYPEXC 3: LIMITS PARAMETER SPECIFIED
      TYPEXC = 1
C ---     EXCLUDE parameter - check values
      CALL XVP( 'EXCLUDE', EXCLD, NUMEXC )
      IF( NUMEXC.GT.0 ) THEN
          TYPEXC = 2
          IF( FMT(1).EQ.'HALF' ) THEN
              DO I=1,NUMEXC
                 IF(EXCLD(I).LT.(-32768)) THEN
                        EXCLD(I) = -32768
                 ELSE
                   IF(EXCLD(I).GT.32767) EXCLD(I) = 32767
                 ENDIF
              ENDDO
          ELSE
              IF( FMT(1).EQ.'BYTE' ) THEN
                 DO I=1,NUMEXC
                  IF(EXCLD(I).LT.0) THEN
                    EXCLD(I) = 0
                  ELSE
                    IF(EXCLD(I).GT.255) EXCLD(I) = 255
                  ENDIF
                ENDDO
              ENDIF
          ENDIF
          AUTOREPLACE = .TRUE.
          CALL XVP('REPLACE',REPLC,COUNT)
          IF(COUNT.EQ.1) THEN
             IF( FMT(1).EQ.'HALF' ) THEN
                  IF(REPLC.LT.(-32768)) THEN
                     REPLC = -32768 
                  ELSE
                     IF(REPLC.GT.32767) REPLC = 32767
                  ENDIF
             ELSE
                IF( FMT(1).EQ.'BYTE' ) THEN
                  IF(REPLC.LT.0) THEN
                    REPLACE = 0
                  ELSE
                    IF(REPLC.GT.255) THEN
                      REPLACE = INT2BYTE(255)
                    ELSE
                      REPLACE = INT2BYTE(INT(REPLC))
                    ENDIF
                  ENDIF
                ENDIF
             ENDIF
          ELSE
             AUTOREPLACE = .FALSE.
          ENDIF
C ---     LIMITS parameter - check values
      ELSE
          CALL XVP( 'LIMITS', LIMITS, NUMLMTS )
          IF((NUMLMTS.EQ.2).AND.(LIMITS(1).LT.LIMITS(2))) THEN
              TYPEXC = 3
                 IF( FMT(1).EQ.'HALF' ) THEN
              IF(LIMITS(1).LT.(-32768)) LIMITS(1) = -32768
              IF(LIMITS(2).GT.32767) LIMITS(2) = 32767
                  ELSE
              IF( FMT(1).EQ.'BYTE' ) THEN
                  IF(LIMITS(1).LT.0) LIMITS(1) = 0
               IF(LIMITS(2).GT.255) LIMITS(2) = 255
              ENDIF
             ENDIF
          ENDIF
             CALL XVP('REPLACE',REPLC,COUNT)
              IF(COUNT.EQ.1) THEN
             IF( FMT(1).EQ.'HALF' ) THEN
                  IF(REPLC.LT.(-32768)) THEN
                     REPLC = -32768 
                 ELSE
                IF(REPLC.GT.32767) REPLC = 32767
              ENDIF
             ELSE
                   IF( FMT(1).EQ.'BYTE' ) THEN
                IF(REPLC.LT.0) THEN
                  REPLACE = 0
                ELSE
                  IF(REPLC.GT.255) THEN
                    REPLACE = INT2BYTE(255)
                  ELSE
                    REPLACE = INT2BYTE(INT(REPLC))
                  ENDIF
                ENDIF
                  ENDIF
             ENDIF
          ELSE
             IF( FMT(1).NE.'BYTE' ) THEN
                 REPLC = LIMITS(1)
             ELSE
                 REPLACE = INT2BYTE(INT(LIMITS(1)))
             ENDIF
          ENDIF
      ENDIF

C ---    Round or truncate data?
      IF (XVPTST('TRUNC')) THEN
         ROUND=0.0
           KFLAGS=0
      ENDIF

      IF (TYPEXC.EQ.1) KFLAGS=KFLAGS+2

      RETURN
      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE INITIALIZE_TABLES
      INCLUDE 'f2.fin'

      REAL     AMXV(4), AMNV(4), RESULT
      INTEGER  J,ICONV, K, CODE
      DATA AMXV /255., 32767.,  2147483647.,  1.7E38/
      DATA AMNV /0.,  -32768., -2147483648., -1.7E38/

C ---    Min and max values for the output file
      AMX = AMXV(OUTC)
      AMN = AMNV(OUTC)
      GO TO (15, 20, 25), CASE

 15   IF (NUM_INPUT.EQ.1) THEN
           IF (TYPEXC.EQ.1) THEN
             CALL KNUTH_LOOKUP (FBUF, BTABLE(-128,0), NUM_INPUT, KFLAGS)
           ELSE
             CALL KNUTH_LOOKUP (FBUF, IBTABLE, NUM_INPUT, KFLAGS)
           ENDIF
           XKC = XKC + 256
      ELSE
           CALL KNUTH_LOOKUP (FBUF, BTABLE, NUM_INPUT, KFLAGS)
           XKC = XKC + 256*256
      ENDIF
      CALL XVMESSAGE ('F2 using byte table lookup',' ')
      RETURN

C ---    Initialize the hashing table
 20   FBUF(1) = 0.
      FBUF(2) = 0.
      XKC = 1
      CALL XKNUTH(FBUF,  RESULT)
      J = ICONV(AMX,AMN,RESULT,ROUND)
      CALL ZIA (HKEY(1), PRIME)
      CODE = 32768                   ! CHANGED TO MATCH PROCESS_IMAGES.
      K = IABS(MOD(CODE,PRIME))+1
      HKEY(K) = CODE
      HVAL(K) = J

      CODE = 0          ! NEED TO SHOW THAT VALUE FOR CODE=0 NOT IN TABLE.
      K = IABS(MOD(CODE,PRIME))+1
      HKEY(K) = CODE+1  ! SOME VALUE OTHER THAN CODE.

      CALL XVMESSAGE ('F2 using hash table lookup',' ')
      RETURN

 25   CALL XVMESSAGE ('F2 calculating every pixel',' ')

      RETURN
      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE PROCESS_IMAGES
      INCLUDE 'f2.fin'
      INCLUDE 'fortport'

      REAL    RESULT, FLOAT 
      INTEGER I,J,K,STATUS,NPICS,ICONV
      INTEGER*2 IN1,IN2,IEXCL
      INTEGER*4 CODE,LIM1,LIM2
      INTEGER   SOUTER,SMIDDLE,SINNER,NOUTER,NMIDDLE,NINNER,
     .          IOUTER,IMIDDLE,IINNER,OUT_COUNT,MID_COUNT

      BYTE  BPIC(4*MAX_SAMP, MAXIN), B_OUTPIC(4*MAX_SAMP)
      BYTE  BPIC1(4*MAX_SAMP), BPIC2(4*MAX_SAMP)
      INTEGER*2  PIC(2*MAX_SAMP, MAXIN),  OUTPIC(2*MAX_SAMP)
      INTEGER*4    IPIC(MAX_SAMP, MAXIN),   I_OUTPIC(MAX_SAMP)
      REAL       FPIC(MAX_SAMP, MAXIN),   F_OUTPIC(MAX_SAMP)
      EQUIVALENCE (PIC, BPIC, IPIC, FPIC)
      EQUIVALENCE (BPIC1, BPIC(1,1)), (BPIC2, BPIC(1,2))
      EQUIVALENCE (B_OUTPIC, OUTPIC, I_OUTPIC, F_OUTPIC)
C==================================================================

C SET UP LOOP INDICES ACCORDING TO IMAGE ORGANIZATION.

      	IF (ORG.EQ.'BSQ') THEN
   		SOUTER = SB
   		SMIDDLE = SL
   		SINNER = SS
   		NOUTER = NBAND
   		NMIDDLE = NLINE
   		NINNER = NSAMP
   		IOUTER = INDEX_B
   		IMIDDLE = INDEX_L
   		IINNER = INDEX_S
   	ELSE IF (ORG.EQ.'BIL') THEN
   		SOUTER = SL
   		SMIDDLE = SB
   		SINNER = SS
   		NOUTER = NLINE
   		NMIDDLE = NBAND
   		NINNER = NSAMP
   		IOUTER = INDEX_L
   		IMIDDLE = INDEX_B
   		IINNER = INDEX_S
   	ELSE IF (ORG.EQ.'BIP') THEN
   		SOUTER = SL
   		SMIDDLE = SS
   		SINNER = SB
   		NOUTER = NLINE
   		NMIDDLE = NSAMP
   		NINNER = NBAND
   		IOUTER = INDEX_L
   		IMIDDLE = INDEX_S
   		IINNER = INDEX_B
   	END IF

C ---    Zero out pixel buffers
      NPICS = MIN0 (NINNER, MAX_SAMP)
      DO J = 1, MAX(NUM_INPUT,2)
          CALL ZIA (IPIC(1,J), NPICS)
      ENDDO
      CALL ZIA (I_OUTPIC(1), NPICS)

      DO 1001 OUT_COUNT = SOUTER, NOUTER+SOUTER-1
      DO 1000 MID_COUNT = SMIDDLE, NMIDDLE+SMIDDLE-1


C  Every time through the middle loop we need to read a record for each of
C  the input files (if any).  For the initial pass of the middle loop we
C  specify the exact record.  After that the middle loop can use sequential
C  access.

       IF (NUM_INPUT .GT. 0) THEN
       DO I = 1, NUM_INPUT
   	  IF (ORG.EQ.'BSQ') THEN
             IF (MID_COUNT.EQ. SMIDDLE) THEN
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,'BAND',OUT_COUNT,
     .            'LINE',MID_COUNT,'SAMP', SS, 'NSAMPS', NSAMP ,' ')
             ELSE IF (SUBSET) THEN
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,'SAMP',SS,
     .                 'NSAMPS',NSAMP,' ')
             ELSE                  ! SKIP XVREAD OPTIONALS IF NOT NEEDED.
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,' ')
             END IF

  	  ELSE IF (ORG.EQ.'BIL') THEN
             IF (MID_COUNT.EQ. SMIDDLE) THEN
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,'LINE',OUT_COUNT,
     .            'BAND',MID_COUNT,'SAMP', SS, 'NSAMPS', NSAMP,' ' )
             ELSE IF (SUBSET) THEN
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,'SAMP',SS,
     .                 'NSAMPS',NSAMP,' ')
             ELSE                  ! SKIP XVREAD OPTIONALS IF NOT NEEDED.
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,' ')
             END IF

  	  ELSE IF (ORG.EQ.'BIP') THEN
             IF (MID_COUNT.EQ. SMIDDLE) THEN
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,'LINE',OUT_COUNT,
     .            'SAMP',MID_COUNT,'BAND', SB,'NBANDS', NBAND,' ')
             ELSE IF (SUBSET) THEN
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,
     .                 'BAND', SB,'NBANDS', NBAND,' ')
             ELSE                  ! SKIP XVREAD OPTIONALS IF NOT NEEDED.
                CALL XVREAD(INUNIT(I),PIC(1,I),STATUS,' ')
             END IF
  	  ENDIF
      END DO  ! END LOOP OVER INPUT FILES
      END IF

C ---     DEPENDING ON WHAT TYPE OF DN EXCLUSION IS REQUESTED, DO
C    ARITHMETIC ON IMAGES, LINE BY LINE.  THE DUPLICATION OF 
C    CODE IS TO AVOID SPEED REDUCTION AS MUCH AS POSSIBLE.

      IF (TYPEXC .EQ. 1) THEN

C ---     NO LIMITS OR EXCLUDED DN VALUES


      IF (CASE .EQ. 1) THEN                       ! TYPE 1, CASE 1

C ---    Use the 256x256 byte table
      DO  J = 1, NINNER
          B_OUTPIC(J) = BTABLE(BPIC1(J),BPIC2(J))
      ENDDO

      ELSE IF (CASE .EQ. 2) THEN                  ! TYPE 1, CASE 2

C ---    Use the hash table for HALF word data
      DO  J = 1, NINNER
          IN1 = PIC(J,1)
          IN2 = PIC(J,2)
          CODE = (IN1+32768)+65536*IN2
          K = IABS(MOD(CODE,PRIME))+1
          IF (HKEY(K).EQ.CODE) GO TO 125
          FBUF(1) = FLOAT (PIC(J,1))
          FBUF(2) = FLOAT (PIC(J,2))
          CALL XKNUTH(FBUF,  RESULT)
          XKC = XKC+1
          HKEY(K) = CODE
          HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 125      OUTPIC(J) = HVAL(K)
      ENDDO

      ELSE IF (CASE .EQ. 3) THEN                  ! TYPE 1, CASE 3

C ---    Process data pixel by pixel
      FBUF(IOUTER) = FLOAT(OUT_COUNT)      ! LOAD LINE,SAMP,BAND
      FBUF(IMIDDLE) = FLOAT(MID_COUNT)
      DO   J= 1, NINNER
          FBUF(IINNER) = FLOAT(J + SINNER-1)
          IF (NUM_INPUT .NE. 0) THEN    
          DO K = 1, NUM_INPUT
          IF (FMT(K).EQ.'BYTE'  .OR. FMT(K).EQ.'HALF') THEN
              FBUF(K) = FLOAT (PIC(J,K))
          ELSE IF (FMT(K).EQ.'FULL') THEN
              FBUF(K) = FLOAT (IPIC(J,K))
          ELSE
              FBUF(K) = FPIC(J,K)
          ENDIF
           ENDDO
           ENDIF

          CALL XKNUTH(FBUF,RESULT)
          IF (OUTFMT.EQ.'BYTE'  .OR. OUTFMT.EQ.'HALF') THEN
              OUTPIC(J) = ICONV(AMX,AMN,RESULT,ROUND)
          ELSE IF (OUTFMT.EQ.'FULL') THEN
              I_OUTPIC(J) = ICONV(AMX,AMN,RESULT,ROUND)
          ELSE
              F_OUTPIC(J) = RESULT
          ENDIF
      ENDDO
      XKC = XKC+NINNER

      END IF !END CASE 3

      ELSE IF (TYPEXC .EQ. 2) THEN   

C ---     EXCLUDED DN VALUES FOR WHICH TO TEST
      IF (CASE .EQ. 1) THEN                       ! TYPE 2, CASE 1

C ---    Use the 256x256 byte table
      DO  J = 1, NINNER
          IN1 = BYTE2INT(BPIC(J,1))
          IN2 = BYTE2INT(BPIC(J,2))
          IF (NUM_INPUT.EQ.2) THEN
             DO I=1,NUMEXC
              IEXCL=INT(EXCLD(I))
              IF(IN1.EQ.IEXCL .OR. IN2.EQ.IEXCL) THEN
                 IF(AUTOREPLACE) THEN
                     B_OUTPIC(J) = REPLACE
                 ELSE    
                     B_OUTPIC(J) = INT2BYTE(IEXCL)
                 ENDIF
                 GO TO 215
               ENDIF
             ENDDO
          ELSE
             DO I=1,NUMEXC
              IEXCL=INT(EXCLD(I))
              IF(IN1.EQ.IEXCL) THEN
                 IF(AUTOREPLACE) THEN
                     B_OUTPIC(J) = REPLACE
                 ELSE    
                     B_OUTPIC(J) = INT2BYTE(IEXCL)
                 ENDIF
                 GO TO 215
              ENDIF
             ENDDO        
          ENDIF
          B_OUTPIC(J) = IBTABLE(IN1,IN2)
 215  ENDDO

      ELSE IF (CASE .EQ. 2) THEN                  ! TYPE 2, CASE 2

C ---    Use the hash table for HALF word data
      DO  J = 1, NINNER
          IN1 = PIC(J,1)
          IN2 = PIC(J,2)
          CODE = (IN1+32768)+65536*IN2
          IF(NUM_INPUT.EQ.2) THEN
            DO I=1,NUMEXC
             IEXCL=INT(EXCLD(I))
             IF(IN1.EQ.IEXCL .OR. IN2.EQ.IEXCL) THEN
                   IF(AUTOREPLACE) THEN
                       HVAL(K) = REPLC
                   ELSE
                       HVAL(K) = IEXCL
                   ENDIF
                   GO TO 225
             ENDIF    
            ENDDO        
          ELSE
            DO I=1,NUMEXC
             IEXCL=INT(EXCLD(I))
             IF(IN1.EQ.IEXCL) THEN
                   IF(AUTOREPLACE) THEN
                       HVAL(K) = REPLC
                   ELSE
                       HVAL(K) = IEXCL
                   ENDIF
                   GO TO 225
             ENDIF    
            ENDDO        
          ENDIF
          K = IABS(MOD(CODE,PRIME))+1
          IF (HKEY(K).EQ.CODE) GO TO 225
          FBUF(1) = FLOAT (PIC(J,1))
          FBUF(2) = FLOAT (PIC(J,2))
          CALL XKNUTH(FBUF,  RESULT)
          XKC = XKC+1
          HKEY(K) = CODE
          HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 225      OUTPIC(J) = HVAL(K)
      ENDDO

      ELSE IF (CASE .EQ. 3) THEN                  ! TYPE 2, CASE 3

C ---    Process data pixel by pixel
      FBUF(IOUTER) = FLOAT(OUT_COUNT)      ! LOAD LINE,SAMP,BAND
      FBUF(IMIDDLE) = FLOAT(MID_COUNT)
      DO   J= 1, NINNER
          FBUF(IINNER) = FLOAT(J + SINNER-1)
             IF (NUM_INPUT .NE. 0) THEN    
             DO K = 1, NUM_INPUT

              IF (FMT(K).EQ.'BYTE'.OR.FMT(K).EQ.'HALF') THEN
               FBUF(K) = FLOAT (PIC(J,K))
              ELSE IF (FMT(K).EQ.'FULL') THEN
               FBUF(K) = FLOAT (IPIC(J,K))
              ELSE
               FBUF(K) = FPIC(J,K)
              ENDIF

              DO I=1,NUMEXC
               IF(FBUF(K).EQ.EXCLD(I)) THEN
                  IF(FMT(K).EQ.'BYTE') THEN
                     IF(AUTOREPLACE) THEN
                         OUTPIC(J) = BYTE2INT(REPLACE)
                     ELSE
                         OUTPIC(J) = EXCLD(I)
                     ENDIF
                     GO TO 238
                  ELSE
                     IF(AUTOREPLACE) THEN
                        RESULT = REPLC
                     ELSE
                        RESULT = EXCLD(I)
                     ENDIF
                     GO TO 235
                  ENDIF            
               ENDIF    
              ENDDO        

             ENDDO
             ENDIF

          CALL XKNUTH(FBUF,RESULT)
 235      IF (OUTFMT.EQ.'BYTE'  .OR. OUTFMT.EQ.'HALF') THEN
              OUTPIC(J) = ICONV(AMX,AMN,RESULT,ROUND)
          ELSE IF (OUTFMT.EQ.'FULL') THEN
              I_OUTPIC(J) = ICONV(AMX,AMN,RESULT,ROUND)
          ELSE
             F_OUTPIC(J) = RESULT
          ENDIF
 238      CONTINUE
      ENDDO
      XKC = XKC+NINNER

      END IF !END CASE 3

      ELSE                

C ---     PIXELS TO BE CHECKED AGAINST LIMITS


      IF (CASE .EQ. 1) THEN                  ! TYPE 3, CASE 1

C ---    Use the 256x256 byte table
        DO  J = 1, NINNER
        IN1 = BYTE2INT(BPIC(J,1))
        IN2 = BYTE2INT(BPIC(J,2))
        LIM1=INT(LIMITS(1))
        LIM2=INT(LIMITS(2))
        IF(NUM_INPUT.EQ.2)    THEN
          IF(IN1.LT.LIM1 .OR. IN1.GT.LIM2 .OR.
     .    IN2.LT.LIM1.OR.IN2.GT.LIM2) THEN
              B_OUTPIC(J) = REPLACE 
          ELSE
              B_OUTPIC(J) = IBTABLE(IN1,IN2)
          ENDIF
        ELSE
          IF(IN1.LT.LIM1 .OR. IN1.GT.LIM2) THEN
              B_OUTPIC(J) = REPLACE
          ELSE
              B_OUTPIC(J) = IBTABLE(IN1,IN2)
          ENDIF
        ENDIF
      ENDDO

      ELSE IF (CASE .EQ. 2) THEN                  ! TYPE 3, CASE 2

C ---    Use the hash table for HALF word data
 320    DO  J = 1, NINNER
       IN1 = PIC(J,1)
       IN2 = PIC(J,2)
       LIM1=INT(LIMITS(1))
       LIM2=INT(LIMITS(2))
       CODE = (IN1+32768)+65536*IN2
        IF(NUM_INPUT.EQ.2) THEN
          IF(IN1.LT.LIM1 .OR. IN1.GT.LIM2 .OR.
     .    IN2.LT.LIM1.OR.IN2.GT.LIM2) THEN
                   OUTPIC(J) = REPLC
          ELSE
              K = IABS(MOD(CODE,PRIME))+1
              IF (HKEY(K).EQ.CODE) GO TO 325
              FBUF(1) = FLOAT (PIC(J,1))
              FBUF(2) = FLOAT (PIC(J,2))
              CALL XKNUTH(FBUF,  RESULT)
              XKC = XKC+1
              HKEY(K) = CODE
              HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 325            OUTPIC(J) = HVAL(K)
          ENDIF
        ELSE
          IF(IN1.LT.LIM1.OR.IN1.GT.LIM2) THEN
                   OUTPIC(J) = REPLC
          ELSE
              K = IABS(MOD(CODE,PRIME))+1
              IF (HKEY(K).EQ.CODE) GO TO 328
              FBUF(1) = FLOAT (PIC(J,1))
              FBUF(2) = FLOAT (PIC(J,2))
              CALL XKNUTH(FBUF, RESULT)
              XKC = XKC+1
              HKEY(K) = CODE
              HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 328            OUTPIC(J) = HVAL(K)
          ENDIF
        ENDIF
      ENDDO

      ELSE IF (CASE .EQ. 3) THEN                  ! TYPE 3, CASE 3

C ---    Process data pixel by pixel
      FBUF(IOUTER) = FLOAT(OUT_COUNT)      ! LOAD LINE,SAMP,BAND
      FBUF(IMIDDLE) = FLOAT(MID_COUNT)
      DO   J= 1, NINNER
          FBUF(IINNER) = FLOAT(J + SINNER-1)
           IF (NUM_INPUT .NE. 0) THEN    
           DO K = 1, NUM_INPUT
          IF (FMT(K).EQ.'BYTE'  .OR. FMT(K).EQ.'HALF') THEN
              FBUF(K) = FLOAT (PIC(J,K))
          ELSE IF (FMT(K).EQ.'FULL') THEN
              FBUF(K) = FLOAT (IPIC(J,K))
          ELSE
              FBUF(K) = FPIC(J,K)
          ENDIF
          IF(FBUF(K).LT.LIMITS(1).OR.FBUF(K).GT.LIMITS(2)) THEN
              IF(FMT(K).EQ.'BYTE') THEN
                OUTPIC(J) = BYTE2INT(REPLACE) 
                GO TO 338
              ELSE
                RESULT = REPLC
                GO TO 335
              ENDIF            
           ENDIF
           ENDDO
           ENDIF

          CALL XKNUTH(FBUF,RESULT)
 335        IF (OUTFMT.EQ.'BYTE'  .OR. OUTFMT.EQ.'HALF') THEN
              OUTPIC(J) = ICONV(AMX,AMN,RESULT,ROUND)
          ELSE IF (OUTFMT.EQ.'FULL') THEN
              I_OUTPIC(J) = ICONV(AMX,AMN,RESULT,ROUND)
          ELSE
              F_OUTPIC(J) = RESULT
            ENDIF
 338        CONTINUE
      ENDDO
      XKC = XKC+NINNER

      END IF !END CASE 3
      END IF !END TYPE 3


C ---	Write next line of data to output file
      IF (ORG.NE.'BIP') THEN
  	  CALL  XVWRIT (OUTUNIT, OUTPIC, STATUS,' ')
      ELSE
  	  CALL XVWRIT(OUTUNIT,OUTPIC,STATUS,'NBANDS',NINNER,' ')
      END IF
  
 1000 CONTINUE
 1001 CONTINUE


      RETURN
      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE CLOSE_FILES
      INCLUDE 'f2.fin'
      INTEGER NDIGITS,STATUS,I
      CHARACTER*36 MSG
      CHARACTER*12 MSGFMT

      NDIGITS = LOG10(FLOAT(MAX(1,XKC)))+1
      WRITE(MSGFMT,'(A6,I1,A4)') '(A19,I',NDIGITS,',A6)'
      WRITE(MSG,MSGFMT) 'FUNCTION EVALUATED ',XKC,' TIMES'
      CALL XVMESSAGE(MSG,' ')
      IF (NUM_INPUT .NE. 0) THEN
          DO I = 1, NUM_INPUT
          CALL XVCLOSE (INUNIT(I), STATUS,' ')
          ENDDO
      ENDIF
      CALL XVCLOSE (OUTUNIT, STATUS,' ')

      RETURN
      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------
C---------------------------------------------------------------------


      FUNCTION ICONV(AMX,AMN,RINPUT,ROUND)
      IF (RINPUT) 1,2,2
 1    R = RINPUT-ROUND
      GO TO 3
 2    R = RINPUT+ROUND
 3    ICONV = INT(AMIN1(AMX,AMAX1(AMN,R)))
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create f2.fin
$ DECK/DOLLARS="$ VOKAGLEVE"
C--- INCLUDE FILE FOR VICAR PROGRAM F2.F

      IMPLICIT NONE
C ---    buffer stuff
      INTEGER MAXIN,MAX_SAMP,MAXFBUF
      PARAMETER (MAXIN=18,MAX_SAMP=32000,MAXFBUF=300)

C ---- Table Stuff
      INTEGER PRIME
      PARAMETER (PRIME=10909)
      BYTE BTABLE(-128:127,-128:127)
      BYTE IBTABLE(0:255,0:255)
      INTEGER*4 HKEY(PRIME+1)
      INTEGER*2 HVAL(PRIME)
      EQUIVALENCE (HKEY,BTABLE,IBTABLE),(HVAL,HKEY(PRIME+1))
      COMMON/TABCOM/BTABLE

C --- Control Parameters

      INTEGER NUMEXC, NUMLMTS, TYPEXC
      REAL    EXCLD(20), LIMITS(2), REPLC
      BYTE REPLACE
      LOGICAL AUTOREPLACE, SUBSET
      CHARACTER*256 FCN
      REAL    AMX, AMN, ROUND
      REAL FBUF(MAXFBUF)
      INTEGER CASE,XKC, KFLAGS

      COMMON/F2COM/FCN,FBUF,AMX,AMN,ROUND,
     + CASE,XKC,NUMEXC, NUMLMTS, AUTOREPLACE,TYPEXC,EXCLD,
     + LIMITS,REPLC,KFLAGS,SUBSET,REPLACE

C --- FILE I/O Stuff

      CHARACTER*4 ORG
      CHARACTER*4 FMT(MAXIN)
      CHARACTER*4 OUTFMT
      INTEGER NUM_INPUT,OUTC,INDEX_L,INDEX_S,INDEX_B
      INTEGER INUNIT(MAXIN), OUTUNIT, SL, SS, SB, NLINE, NSAMP, NBAND
      COMMON/FILECOM/NUM_INPUT,INUNIT,OUTUNIT,SL,SS,SB,
     +  NLINE,NSAMP,NBAND,ORG,FMT,OUTFMT,OUTC,INDEX_L,INDEX_S,INDEX_B
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create f2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM f2

   To Create the build file give the command:

		$ vimake f2			(VMS)
   or
		% vimake f2			(Unix)


************************************************************************/


#define PROGRAM	f2
#define R2LIB

#define MODULE_LIST f2.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define INCLUDE_LIST f2.fin
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create f2.pdf
 ! pdf for PROGRAM F2
 process help=*
 PARM INP TYPE=STRING   DEFAULT=--  COUNT=0:18 
 PARM OUT TYPE=STRING               COUNT=1
 PARM SIZE TYPE=INTEGER DEFAULT=--  COUNT=0:4
 PARM BANDS TYPE=INTEGER DEFAULT=-- COUNT=0:2 
 PARM SL TYPE=INTEGER   DEFAULT=--  COUNT=0:1
 PARM SS TYPE=INTEGER   DEFAULT=--  COUNT=0:1
 PARM NL TYPE=INTEGER   DEFAULT=--  COUNT=0:1
 PARM NS TYPE=INTEGER   DEFAULT=--  COUNT=0:1
 PARM SB TYPE=INTEGER	DEFAULT=--  COUNT=0:1
 PARM NB TYPE=INTEGER	DEFAULT=--  COUNT=0:1
 PARM ORG TYPE=STRING   DEFAULT=--  COUNT=0:1 VALID=(BSQ,BIL,BIP)
 PARM FORMAT KEYWORD  VALID=(BYTE,HALF,FULL,REAL) DEFAULT=-- COUNT=0:1
 PARM FUNCTION TYPE=STRING       DEFAULT="IN1"
 PARM TRUNC    TYPE=KEYWORD      DEFAULT=-- COUNT=0:1 VALID=TRUNC
 PARM EXCLUDE  TYPE=REAL         DEFAULT=-- COUNT=0:20
 PARM LIMITS   TYPE=REAL         DEFAULT=-- COUNT=(0,2)
 PARM REPLACE  TYPE=REAL         DEFAULT=-- COUNT=0:1
 PARM DUMPCODE TYPE=KEYWORD VALID=(DUMP,NODUMP) DEFAULT=NODUMP
 END-PROC
!# ANNOT ICON = f2
!# parm inp(3-18) hints=default
.TITLE
 PROGRAM F2
.HELP
 PURPOSE:

 F2 allows general arithmetic operations to be performed on one to
 eighteen input images in either byte, halfword, fullword integer
 or fullword real formats (or any combination of these types).
 It now allows for arithmetic operations between three dimensional input files
 and can produce a three dimensional output.  All data organizations, BSQ, BIL,
 and BIP, are supported.  
 The arithmetic operation is specified by a FORTRAN or C-like expression
 which can include the following operators (synonyms connected by an =):
    
        + - * / ** unary-
	LOG   = ALOG    =  LN  (all these = natural (base e) log)
	LOG10 = ALOG10         (all these = common (base 10) log)
        SQRT   ABS   AINT = INT
        SIN    COS  TAN    ASIN  ACOS  ATAN  ATAN2
        AMAX1 = MAX  AMIN1 = MIN
        AMOD   MOD .AND.  .OR.  .XOR.  .NOT.
        .LT.  .LE. .EQ.   .NE.  .GT.  .GE.

 as well as the following C-operators:

  &    &&   |    ||    %   ^   !  
  ==   !=   >=   <=   >   <   >>   <<

 Operands to F2 can be :
	
	integers
	floating point numbers
	IN1	IN2	IN3	IN4  ...  IN18
	LINE 	SAMP    BAND

 The function string can include constants, including constants expressed
 in floating point notation. The string can also include TCL variable 
 references.  
	
 F2 can operate on zero inputs to generate an output using the
 operands LINE, SAMP, and BAND.
.page	
 The current version of F2 has the following improvements over the 
 original (1976) version:

	1.  Allows up to 18 input files
	2.  Handles byte, halfword, fullword, or real input data files,
	    and allows mixed format cases.
	3.  Significant runtime speedups due to byte lookup tables
	    and implementation of new Vicar2 I/O.
        4.  Function string parameter expanded to a maximum of 250 characters.
	5.  Exclude, limits, and replace parameter provided.
	6.  New KNUTH subroutines allow for C-Language constructs.
	7.  Unix-Compatible byte-order independent operation.
.page
 F2 performs arithmetic operations on image files.  The program
 uses two library routines, KNUTH and XKNUTH, to compile and
 execute FORTRAN or C-like expressions entered by the parameters in the
 FUNCTION expression such as:
 	
  ((IN1+IN2)*(AINT(ALOG10(IN1*IN2**2))+3.14+IN1/22)+4*(LINE.EQ.SAMP)
 or:
  ((in1 + in2) % (3*(line!=samp))) << 3 

 IN1 is an element from the first input and IN2 is the corresponding 
 element from the second input. KNUTH compiles the expression into 
 instructions executable by XKNUTH. The expression is applied to the
 pixels from  each input in XKNUTH to produce the output picture.  When
 performing  arithmetic, F2 converts all integer operands to real and
 executes  single precision floating point computations.  The exceptions
 are  the logical operators .AND.(&), .OR.(|), .XOR.(^), as well as the
 shift operators (>>) and (<<), all of which operate bitwise
 on  the 4-byte integer equivalent to the  truncated  floating point 
 number.  The final result from XKNUTH is converted back to integer by
 rounding,  which is  the default, or by truncation if 'TRUNC is specified.
 The logical values  'TRUE'  and  'FALSE' 
 when produced, are  interpreted as 1 and 0  respectively.  The logical NOT 
 operates only on the  values 0 and 1.

 Because of the type of calculations done on images, the routine XKNUTH
 is designed to never abort regardless of input values.  Illegal 
 operations result in the generation of a "reasonable" result as 
 follows: a) divide by zero causes divide by 1.E-6;  b) log of a
 negative number causes log its absolute value, etc.  The routine KNUTH 
 quits processing, prints an error message and returns if a syntax 
 error is detected in the expression string.  

 To conserve time, repetitive  executions of "F2" are  stored  as a 
 lookup table.  In the  cases  where both input and output are byte
 data,  the table is  construced  as a straight lookup initialized with
 every  possible  combination of  IN1  and IN2 from 1 to 256.  The table
 is  referenced by  indexing  the  array  using the concatenation of IN1
 and  IN2 to create  an index  number to  the vector table.  Table is 
 dimensioned 65536 to accommodate any possible combination of 256 x 256.
 For more information see KNUTH_LOOKUP.

 In cases which either the input or output is halfword data or  the
 inputs are small byte files, a hashing table is constructed.  The
 equation is then executed each time it encounters a unique
 combination of IN1 and IN2.  A key points to the storage location 
 of the results of XKNUTH.  The key is then determined by the 
 remainder of concatenating the two variables and dividing by 
 10909.  In case the key must be checked for a match of the 
 concatenation of IN1 and IN2 which was stored. If no key is  found,
 a solution is calculated and a concatenation of the result  of the
 XKNUTH calculation, IN1 and IN2 is stored. In cases which  the key
 points to an erroneous combination of IN1 and IN2, the  new values
 and the new key are stored. 

 If the operands LINE, SAMP, or BAND are used, no table is used and the
 formula is evaluated at every pixel. This option is also used on
 images involving fullword or real*4 length input or output, and 
 with multiple input files (3+). 

 Parameters SL, SS, SB, NL, NS, NB allow the user to specify a certain
 area or volume of the input files for which the function will be evaluated.
 Alternatively the SIZE and BANDS parameters may be used.
.page
	
 KNUTH compiles the function expression into instructions that XKNUTH
 can execute.  For example, the expression "IN1*10+IN2" will be
 decomposed as
			LOAD 1
			MULT 22
			ADD  2
			RETN 0

 The IN1 thru IN18 are stored in location 1 thru 18, LINE, SAMP, and BAND
 are stored in later locations. Space is also separately reserved
 in the executable buffer for static constants, temporary registers
 and executable code. For more information, see KNUTH and XKNUTH.
.page

 EXECUTION:
    The following is the execution statement for F2:
        f2 INP  OUT  PARAMS
 where INP, OUT, and PARAMS are parameters discussed in their respective 
 parameter section in tutor mode.

 EXAMPLES:

    gen A 10 10 'BYTE
    gen B 10 10 'BYTE
    gen H 10 10 'HALF
    gen F 10 10 'FULL
    gen R 10 10 'REAL4
	
    f2 INP=(A,B) OUT=C FUNCTION="IN2/4 + (ALOG10(IN1/IN2+3))**2"
 
 This example uses the default format specification for
 input and output.  The input files are A and B, the output is
 file C.
 
    f2 A C 'HALF FUNCTION="3.3*ALOG(IN1)"
	
 This example has only one input.   Output is halfword.
 
    f2 (R,H) C FUNCTION="MAX(IN1,IN2)"
 
 This example has mixed input real and halfword.  The output is in real
 format.
	
    f2 (A,B) C FUNCTION="IN1.OR.IN2"
	
 The bitwise logical OR of A and B becomes C. This operation may also
 be performed using the function "in1|in2", using C-Language for
 bitwise-or.
	
    f2 out=c size=(1,1,100,100) nb=100 'half function="line.eq.samp.eq.band"
	
 Produces a 100 x 100 x 100 halfword image with a diagonal in 3D of ones in a
 space of zeros.  In general, logical operators produce 0 for FALSE and 1 for 
 TRUE.
.page
  
 RESTRICTIONS:
	
The program is restricted to lines of length 64000 pixels (byte format)
32000 pixels (halfword format), or 16000 pixels (real or fullword format).

The expression must not contain more than 30 constants, 20 variables, 
or roughly 80 operations.  Embedded blanks are allowed.
The expression is case insensitive.

If any of the input images have a (3-D) file organization other than BSQ,
then all of the input images must have the same file organization.

OPERATIONS:

When performing  arithmetic, F2 converts all integer
operands to real and executes  single precision floating point
computations.  The exception is  .AND., .OR., .XOR.,>> and << which
operate on the binary equivalent of the  truncated floating
point number.  The final result from XKNUTH is converted back to integer by
rounding, which is the default, or by truncation if 'TRUNC is specified.
The logical values "TRUE'  and 'FALSE' 
when produced, are interpreted as 1 and 0 respectively.
The logical NOT operates only on the logical values 0 and 1. 
A subtle consequence of converting integers to floating point is
that exponentiation does not work as expected for negative values
raised to an integer exponent.  This is because they are interpreted
as the absolute value raised to a floating point exponent.
Users should use ABS before exponentiation for even powers.  
Odd powers can be handled using
multiplication and the next smaller even power.

All operations now operate as in standard FORTRAN and C, including
MIN and MAX. When using the C operations "&" and "&&" recall that
VICAR interprets "&name" as a dereference operator on the variable
"name", so for bitwise AND put a space between args, e.g. "in1 & in2".
For logical AND you must actually use "in1 &&&& in2", because VICAR
interprets "&&" as an escaped single ampersand. When in doubt, use
the command .AND. instead.
.PAGE

 PRECISION: 
  The precision of values in fullword output images is limited to six
significant digits because F2 converts all numeric values to real format
before performing computations.  The variation in fullword values that
can be expected on different MIPS-supported machines shall not differ by 
more than 1 in six significant digits.  
.PAGE

 TIMING: 
  The following CPU times for a few F2 cases were obtained on a 
VAX 8650 (MIPL3) in July 1992.  
                                                CPU Time
gen f2a 2000 2000 'real
gen f2b 2000 2000 'real
f2 (f2a,f2b) x func="(in1+ in2)/2"              187s
f2 (f2a,f2b) x func="in1+3.7e+05 * in2"         171s
!
gen f2a 2000 2000 'byte
gen f2b 2000 2000 'byte
f2 (f2a,f2b) x                                   13.7s

For the case of a single BYTE or HALFWORD image, using program STRETCH
with the FUNCTION parameter is at least twice as fast on the VAX as using 
the more general F2 program.


 WRITTEN BY: 	A. L. ZOBRIST		18 FEBRUARY 1976

 COGNIZANT PROGRAMMER: N. D. Ritter 	09 February 1992

 REVISION: 	25 February 1993  (SP)  Merged in the capabilities to handle
					3D images from F2_3D.
		29 April    1992  (NDR) Moved some routines to KNUTH module.
                09 February 1992  (NDR) Upgraded for Unix, C - constructs
        	04 December 1990  (JFM) LIMITS with invalid REPLACE parameter
			                handled differently.
		05 September 1990 (JFM)	EXCLUDE, LIMITS AND REPLACE parameters
				        added to enable the user to selectively
				        exclude certain input DN values from
					arithmetic operations.
.LEVEL1
.VARIABLE INP
 0:18 INPUT DATA SETS
.VARIABLE OUT
 THE OUTPUT DATA SET
.VARIABLE SIZE
 4 INTEGERS - OPTIONAL
 A STANDARD VICAR SIZE FIELD
.vari bands
(Optional) Window into input
in band dimension
.VARIABLE SL
 INTEGER - OPTIONAL
 STARTING LINE
.VARIABLE SS
 INTEGER - OPTIONAL
 - STARTING SAMPLE
.VARIABLE NL
 INTEGER - OPTIONAL
 - NUMBER OF LINES
.VARIABLE NS
 INTEGER - OPTIONAL
 - NUMBER OF SAMPLES
.VARIABLE SB
(OPTIONAL) STARTING BAND
.VARIABLE NB
(OPTIONAL) NUMBER OF BANDS
.VARIABLE ORG
(OPTIONAL) IMAGE ORGANIZATION
.VARIABLE FORMAT
 OUTPUT PIXEL FORMAT
 VALID: BYTE, HALF, FULL, REAL
.VARIABLE FUNCTION
 STRING - A FUNCTION TO BE
 APPLIED TO INPUT (<=250 CHARS)
.VARIABLE TRUNC
 THE RESULT WILL BE TRUNCATED
.VARIABLE EXCLUDE
 ARRAY OF VALUES TO BE 
 EXCLUDED FROM FUNCTION
 EVALUATION.	
.VARIABLE LIMITS
 RANGE OF VALUES TO BE
 INCLUDED IN FUNCTION
 EVALUATION.
.VARIABLE REPLACE
 VALUE TO BE PLACED IN
 OUTPUT FILE WHEN INPUT
 VALUE IS AN EXCLUDED 
 VALUE OR IS OUTSIDE OF
 LIMITS.
.VARIABLE DUMPCODE
 DO A SYMBOLIC DUMP OF THE
 COMPILED CODE ?
.LEVEL2
.VARIABLE INP
 INPut data sets in byte, I*2, I*4, or REAL*4 format.  The format of the
 input files is taken from its label.  There may be 0 to 18 input data sets.
.VARIABLE OUT
 OUTput data set in byte, I*2, I*4, or REAL*4 format. Primary input format
 is assumed if FORMAT is not specified
.VARIABLE SIZE
 (Starting Line, Starting Sample, Number of Lines, Number of Samples)
 The standard vicar size field. SIZE gives the area of operation on inputs
 and defines the size of the output.  If larger than any of the inputs,
 its values are reduced to the minimum line or sample of the inputs.
 The SIZE parameter takes precedence over SS, SL, NS, NL.  If it is not
 given (default case), then it gets the information from the first input
 file label.
.vari bands
The bands parameter determines the bands in the input
files to be processed. It is specified
as (SB,NB), where
	SB is the starting band
	NB is the number of bands to be copied
(Default is all bands in image.)
.VARILABLE SL
 Starting Line - Alternate method of specifying the first SIZE parameter value
.VARIABLE SS
 Starting Sample - Alternate method of specifying the first SIZE param value
.VARIABLE NL
 Number of Lines - Alternate method of specifying the first SIZE param value
.VARIABLE NS
 Number of Samples - Alternate method of specifying the first SIZE param value
.VARIABLE SB
Starting Band of window - Can not be defined in SIZE parameter. (Default is
first band in image.)
.VARIABLE NB
Number of Bands of window - Can not be defined in SIZE parameter.  (Default is
number of bands in image.)
.VARIABLE ORG
Image organization for output image when zero input images.  If omitted, output
image will be BSQ.  Has no effect if there ARE input images.
.VARIABLE FORMAT
 Valid values: BYTE, HALF, FULL, REAL.
 These values correspond to the following data formats:
 byte (BYTE), integer*2 (HALF), integer*4 (FULL), real*4 (REAL).
 This parameter specifies the data format of the output file(s).
 Default is primary input format.
.VARIABLE FUNCTION
 Specifies a function to be applied to the input data set.
 The function, in the form of a FORTRAN or C like expression, to be
 applied to the input data set(s) to produce the output data
 set.  The default expression is "IN1".
 The function string can include constants, including constants expressed
 in floating point notation.  The string can also include TCL variable
 references.
.VARIABLE TRUNC
 Specifies that the results of the calculation will be truncated.
 Default is that results will be rounded.  This keyword is
 inoperative if real*4 output is specified.
.VARIABLE EXCLUDE
 Array of values to be excluded from function evaluation.  Up to twenty (20)
 values can be specified.  If any one of the specified values are encountered
 in the inputs, that pixel is not evaluated by the function and the replace-
 ment value specified by the REPLACE parameter is placed in the output pixel.
 If the REPLACE parameter is not specified, the first excluded value among the 
 input pixels is used as the output pixel.
.VARIABLE LIMITS
 This is the user defined range of values to be included in function evaluation.
 If values fall outside of this range, the REPLACE parameter value is placed on 
 the output pixel.  If REPLACE is not specified, the lower limit of the range 
 is used.  If the REPLACE value is outside the valid data range, e.g. -54000 for
 halfword data, then the closest valid data value is used in the replacement.
 If the limits are beyond the valid range of the input data, the extremes of the
 input data values are used, e.g. LIMITS=(-5,240) for byte data yields true 
 limits of (0,240).
.VARIABLE REPLACE
 Value to be placed in output file when input value is an excluded value or is 
 outside the user specified limits.  REPLACE's value must be within the valid
 data range of the input file.  If the input file is byte, REPLACE can be any
 value between (0,255), inclusive.  If a REPLACE is outside this range, the
 closest valid data value is chosen, e.g. REPLACE=-40000 for halfword data
 yields a REPLACE=-32768.
.VARIABLE DUMPCODE
 Activating the keyword 'DUMP tells F2 to display the symbolic output of 
 the compiled code produced by the expression-compiler subroutine KNUTH.
 Its purpose is primarily diagnostic, and allows the examination of the
 way that KNUTH has interpreted the user's input function.

 For example, the command

        f2 INP=(F1,F2) OUT=X FUNCTION="IN1+SQRT(IN2)" 'DUMP

 will output something like:

	Beginning VICAR task F2
	   SQRT   2
	   ADD    1
	   RETN   0
	   ...

 Which reads: load the SQRT of register 2 (IN2) into the active 
 register (which is index 0), then ADD the value of register 1 (IN1)
 to it and RETurnN the value in the active register 0 -- which is
 indeed the requested operation.

 Formerly, F2 always displayed this code dump, but this was sometimes
 inconvienient with long, complex expressions whose dump might require
 80 or more output lines in a log file.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstf2.pdf
procedure
!   TESTERS:  THIS TAKES TOO LONG TO RUN IN THE VAX FAST QUEUE
refgbl $echo
refgbl $autousage
body
  local i integer
  local vals integer count=1:10
  local val integer
  local formats (string,20) count=1:10
  local format string
  local formats2 (string,20) count=1:10
  local format2 string
  local oformats (string,20) count=1:10
  local oformat string

let _onfail="continue"
let $autousage="none"
let $echo="yes"
! THIS IS A TEST SCRIPT FOR THE PROGRAM F2
GEN F1 NL=5 NS=20 'FULL
GEN F2 NL=5,NS=20 'FULL LINC=3 SINC=3
LIST F1 'FULL
LIST F2 'FULL
! INTEGER*4 TEST CASE
F2 INP=(F1,F2) OUT=X FUNCTION="IN1+SQRT(IN2)"
LIST X
GEN R NL=5 NS=20 'REAL4
GEN S NL=5,NS=20 'REAL4 LINC=1.5 SINC=1.5
LIST R
LIST S
! REAL*4 TEST CASE
F2 INP=(R,S) OUT=X FUNCTION="IN1+SQRT(IN2)"
LIST X
GEN B NL=10 NS=10
GEN G NL=10 NS=10 IVAL=125
GEN H NL=10 NS=20 'HALF
GEN L1 LINC=1. SINC=0. NL=10 NS=10
GEN L2 LINC=0. SINC=1. NL=10 NS=10
LIST B
LIST H
LIST L1
LIST L2
! BASIC TEST CASES
F2 INP=B OUT=X FUNCTION="IN1+1"
LIST X
F2 INP=B OUT=X FUNCTION="INT(IN1/10)"
LIST X 'ZERO
F2 INP=B OUT=X2 FUNCTION="IN1/10" 'TRUNC
DIFPIC (X,X2)
! BASIC TEST CASE WITH BYTE VALUES > 127
F2 INP=G OUT=X FUNCTION="IN1+1"
LIST X
! BASIC TEST CASE WITH WINDOW
F2 INP=B OUT=X FUNCTION="IN1+1" SIZE=(2,3,6,4)
LIST X
! NEXT TWO EXECUTIONS TEST THE TRUNC KEYWORD.  SEE VALUE 5 IN LOWER RIGHT
! CORNER CHANGE TO 6.
F2 B X FUNC="SQRT(IN1**1.1234+7)"
LIST X
F2 B X FUNC="SQRT(IN1**1.1234+7)"   'TRUNC
LIST X

! HALFWORD CASE
F2 INP=H OUT=X  FUNCTION="IN1+1" 
LIST X
! HALFWORD CASE WITH WINDOW
F2 INP=H OUT=X  FUNCTION="IN1+1"  SIZE=(2,5,6,8)
LIST X

! LOGICAL CASES
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1.AND.IN2"
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1.OR.IN2"
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1.XOR.IN2"
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1.LT.IN2"
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1.EQ.IN2"
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION=".NOT.(IN1.GE.IN2)"
LIST X

! C-Language Constructs -- Also checks 'DUMP keyword
F2 INP=L1 OUT=X  FUNCTION="!IN1" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 && IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 &&&& IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 ^ IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 | IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1<<IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 <= IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 % IN2" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="IN1 > IN2" 'DUMP
LIST X

! SPECIAL FUNCTIONS
COPY L2 L3
F2 INP=(L1,L2) OUT=X  FUNCTION="MOD(IN2,IN1)" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="MIN(IN1,IN2)" 'DUMP
LIST X
F2 INP=(L1,L2,L3) OUT=X  FUNCTION="MIN(IN1,IN2,IN3)" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  FUNCTION="MAX(IN1,IN2)" 'DUMP
LIST X
F2 INP=(L1,L2) OUT=X  +
 FUNCTION="100.*SIN(IN1/10.)+100.*COS(IN2/10)" 'DUMP
LIST X

! NO INPUT TEST CASE (IMAGE GENERATION)
F2 OUT=X  FUNCTION="LINE+SAMP"  SIZE=(1,1,10,10)
LIST X
!
! THIS IS A LARGE (90000 PIXEL) CASE WITH VARIOUS INPUTS.  THE TABLE LOOKUP
! METHOD IS USED.  THE TERMINAL MESSAGES
!       "FUNCTION EVALUATED 65536 TIMES"
!       "FUNCTION EVALUATED N TIMES"  (255<N<65536)
!       "FUNCTION EVALUATED 180000 TIMES"
! SHOULD APPEAR.  IN1 WAS MADE DIFFERENT THAN IN2 TO CHECK FOR POSSIBLE
! MIXUP OF INPUTS.
LET I=1
LET FORMATS=("BYTE","HALF","REAL4","END")
LOOP
    LET FORMAT=FORMATS(I)
    IF (FORMAT="END") BREAK

    GEN  A NL=300 NS=300 '&FORMAT
    GEN  B NL=300 NS=600 '&FORMAT
    F2 INP=(A,B) OUT=X FUNCTION="IN1+SQRT(IN2)"
    LIST X,NL=10,NS=10

    LET i=i+1
END-LOOP

! MIXED FORMATS FOR BOTH INPUT AND OUTPUT
LET I=1
LET FORMATS =("BYTE","HALF","BYTE","HALF", "FULL", "FULL","BYTE", "END")
LET FORMATS2=("HALF","BYTE","HALF","BYTE", "REAL4","HALF","REAL4","END")
LET OFORMATS=("BYTE","BYTE","FULL","REAL","BYTE", "BYTE","FULL", "END")
LOOP
    LET FORMAT=FORMATS(I)
    IF (FORMAT="END") BREAK

    LET FORMAT2=FORMATS2(I)
    LET OFORMAT=OFORMATS(I)
    GEN  A NL=10 NS=10 '&FORMAT
    GEN  B NL=10 NS=10 '&FORMAT2
    F2 INP=(A,B) OUT=X FUNCTION="IN1+IN2" '&OFORMAT
    LIST X

    LET i=i+1
END-LOOP


GEN A 10 10 'BYTE
GEN B 10 10 'HALF LINC=2 SINC=2
GEN C 10 10 'FULL LINC=3 SINC=3
GEN D 10 10 'REAL4 LINC=4 SINC=4
! MULTIPLE INPUT CASE
! THE ONE'S PLACE WILL INCREMENT BY 1'S, THE 100'S PLACE BY 2'S
! THE 10000'S PLACE BY 3'S, AND THE 1000000'S PLACE BY 4'S
F2 (A,B,C,D) X FUNC="IN1+100*IN2+10000*IN3+1000000*IN4" 'FULL
LIST X
! TEST OF FUNCTION STRING EXTENSION AND BAD FUNCTION STRINGS
GEN F1 10 10 IVAL=0 SINC=0 LINC=0 
GEN F2 10 10 IVAL=1 SINC=0 LINC=0 
F2 INP=(F1,F2) OUT=X FUNCTION=+
"(IN1+IN2)+(IN1+IN2)+(IN1+IN2)+(IN1+IN2)+(IN1+IN2)+(IN1+IN2)+(IN1+IN2)++
 (IN1+IN2)+(IN1+IN2)+(IN1+IN2)+(IN1+IN2)+(IN1+IN2)"
LIST X
! -- These next two tests are omitted, because the new versions of
!    knuth, xknuth can make sense of them (they DO make sense,after all).
!write "The following execution of F2 should result in a BAD FUNCTION"
!write "message and an F2 abend."
!F2 INP=(F1,F2) OUT=X FUNCTION="IN1.EQ.-IN2"
!F2 INP=(F1,F2) OUT=X FUNCTION="IN1.EQ.(-IN2)"
!LIST X
!


! THIS IS A TEST OF THE MAX NUMBER OF INPUTS
LET I=1
LET FORMATS =("BYTE", "HALF", "FULL", "REAL", "END")
LOOP
    LET FORMAT=FORMATS(I)
    IF (FORMAT="END") BREAK

    GEN B1 NL=10 NS=10 '&FORMAT
    COPY B1 B2
    COPY B1 B3
    COPY B1 B4
    COPY B1 B5
    COPY B1 B6
    COPY B1 B7
    COPY B1 B8
    COPY B1 B9
    COPY B1 B10
    COPY B1 B11
    COPY B1 B12
    COPY B1 B13
    COPY B1 B14
    COPY B1 B15
    COPY B1 B16
    COPY B1 B17
    COPY B1 B18
    F2 (B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,B17,B18) B '&FORMAT +
      FUNC="MIN(IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9,IN10,IN11,IN12,IN13,IN14,IN15,IN16,IN17,IN18)"
    WRITE "SHOULD GET 0 DIFFERENCES."
    DIFPIC (B,B1)
    F2 (B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,B17,B18) B '&FORMAT +
      FUNC="MAX(IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9,IN10,IN11,IN12,IN13,IN14,IN15,IN16,IN17,IN18)"
    WRITE "SHOULD GET 0 DIFFERENCES."
    DIFPIC (B,B1)

    LET i=i+1
END-LOOP

! THIS IS A TEST OF THE MAX LINE SIZE FOR F2
LET I=1
LET FORMATS =( "BYTE", "HALF", "FULL", "REAL", "END")
LET FORMATS2 =("BYTE", "HALF", "HALF", "HALF", "END")
LET VALS=(      64000,  32000,  16000,  16000,  0)
LOOP
    LET FORMAT=FORMATS(I)
    IF (FORMAT="END") BREAK

    LET FORMAT2=FORMATS2(I)
    LET VAL=VALS(I)
    GEN B0 NL=10 NS=&VAL '&FORMAT2
    GEN B1 NL=10 NS=&VAL '&FORMAT
    COPY B1 B2
    F2 (B1,B2) B '&FORMAT2  FUNC="MIN(IN1,IN2)"
    WRITE "SHOULD GET 0 DIFFERENCES."
    DIFPIC (B,B0)
    F2 (B1,B2) B '&FORMAT2 FUNC="MAX(IN1,IN2)"
    WRITE "SHOULD GET 0 DIFFERENCES."
    DIFPIC (B,B0)

    LET i=i+1
END-LOOP
!
gen a 10 10 'real4 ival=-9.0
list a
!
! test various mathematical and trigonometric identities
!
f2 a b func="sqrt(in1)"
list b
f2 b c func="in1*in1"
write "Should get c = abs(a)."
list c
!
f2 a b func="sin(in1)*sin(in1) + cos(in1)*cos(in1)"
write "Should get b = 1.0."
list b
!
f2 a b func="10**(in1)"
list b
f2 b c func="log10(in1)"
write "Should get c = max( -6, a)."
list c
!
f2 a b func="atan2(in1,1.0)"
write "Should get 0.0 and pi/4 = .78..."
list b (2,9,1,2)
f2 b c func="tan(in1)"
write "Should get c = a."
list c
!
f2 a b func="atan(in1)"
write "Should get 0.0 and pi/4 = .78..."
list b (2,9,1,2)
f2 b c func="tan(in1)"
write "Should get c = a."
list c
!
f2 a b func="tan(atan2(in1,in1))"
write "Should get b = 0 on diagonal from (10,1) to (1,10) and b=1 elsewhere."
list b
!!!!!!!!!!!
! TEST OF EXCLUDE, LIMITS AND REPLACE PARAMETERS
GEN A 10 10 IVAL=10 SINC=0 LINC=1
GEN B 10 10 'HALF IVAL=-32768 SINC=0 LINC=5000
GEN C 10 10 'FULL IVAL=432768 SINC=0 LINC=1
GEN D 10 10 'REAL IVAL=5432768 SINC=0 LINC=1
F2 INP=A OUT=X EXCLUDE=(13,14,18) FUNCTION="IN1+100"
LIST X
F2 INP=A OUT=X EXCLUDE=(13,14,18) FUNCTION="IN1+100" REPLACE=99
LIST X
F2 INP=A OUT=X EXCLUDE=(13,14,18) FUNCTION="IN1+100" REPLACE=280
LIST X
F2 INP=A OUT=X EXCLUDE=(13,14,18) FUNCTION="IN1+100" REPLACE=-280
LIST X
F2 INP=A OUT=X LIMITS=(5,18) FUNCTION="IN1+100" 
LIST X
F2 INP=A OUT=X LIMITS=(5,18) FUNCTION="IN1+100" REPLACE=99
LIST X
F2 INP=A OUT=X LIMITS=(5,18) FUNCTION="IN1+100" REPLACE=280
LIST X
F2 INP=A OUT=X LIMITS=(5,18) FUNCTION="IN1+100" REPLACE=-280
LIST X
F2 INP=B OUT=X EXCLUDE=-22768 FUNCTION="IN1+1000"
LIST X
F2 INP=B OUT=X EXCLUDE=-22768 REPLACE=99
LIST X
F2 INP=B OUT=X EXCLUDE=-27768 REPLACE=40000
LIST X
F2 INP=B OUT=X EXCLUDE=-27768 REPLACE=-40000
LIST X
F2 INP=B OUT=X LIMITS=(-50000, 0) 
LIST X
F2 INP=B OUT=X LIMITS=(-50000, 0) REPLACE=99
LIST X
F2 INP=B OUT=X LIMITS=(-30000, 0) REPLACE=40000
LIST X
F2 INP=B OUT=X LIMITS=(-30000, 0) REPLACE=-40000
LIST X
F2 INP=C OUT=X FUNCTION="IN1+1000" EXCLUDE=(432768,432775) 
LIST X
F2 INP=C OUT=X EXCLUDE=(432768,432775) REPLACE=99
LIST X
F2 INP=C OUT=X LIMITS=(432770,432775) 
LIST X
F2 INP=C OUT=X LIMITS=(432770,432775) REPLACE=99
LIST X
F2 INP=D OUT=X EXCLUDE=(5432770,5432775) FUNCTION="IN1+100000"
LIST X
F2 INP=D OUT=X EXCLUDE=(5432770,5432775) REPLACE=99
LIST X
F2 INP=D OUT=X LIMITS=(5432770,5432775) 
LIST X
F2 INP=D OUT=X LIMITS=(5432770,5432775) REPLACE=99
LIST X
F2 INP=(C,D) OUT=X EXCLUDE=(5432768,432768,432769) FUNCTION="IN1+IN2" +
 REPLACE=4040
LIST X
GEN A 10 10 IVAL=0 SINC=0 LINC=1
GEN B 10 10 IVAL=0 SINC=1 LINC=0
GEN C 10 10 IVAL=10 SINC=0 LINC=0
LIST A
LIST B
LIST C
F2 INP=(A,B,C) OUT=X EXCLUDE=(3,4,8) FUNCTION="IN1+IN2+IN3"
LIST X
F2 INP=(A,B,C) OUT=X EXCLUDE=(3,4,8) FUNCTION="IN1+IN2+IN3" REPLACE=99
LIST X
F2 INP=(A,B,C) OUT=X EXCLUDE=(3,4,8) FUNCTION="IN1+IN2+IN3" REPLACE=280
LIST X
F2 INP=(A,B,C) OUT=X EXCLUDE=(3,4,8) FUNCTION="IN1+IN2+IN3" REPLACE=-280
LIST X
F2 INP=(A,B,C) OUT=X LIMITS=(3,10) FUNCTION="IN1+IN2+IN3"
LIST X
F2 INP=(A,B,C) OUT=X LIMITS=(3,10) FUNCTION="IN1+IN2+IN3" REPLACE=99
LIST X
F2 INP=(A,B,C) OUT=X LIMITS=(3,10) FUNCTION="IN1+IN2+IN3" REPLACE=280
LIST X
F2 INP=(A,B,C) OUT=X LIMITS=(3,10) FUNCTION="IN1+IN2+IN3" REPLACE=-280
LIST X
! Test floating point error (FR#75716)
f2 out=a nl=5 ns=5 fun="-1.7e38*(line<3)+2" 'real
f2 out=b nl=5 ns=5 fun="-1.7e38*(line<3)+1" 'real
list a
list b
f2 (a,b) c fun="in1-in2" exclude=-1.7e38 replace=-1.7e38
list c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write "Test F2 on multiband images"
gen a 5 10 12 ORG="BSQ" 'BYTE   LINC=2 SINC=3 BINC=4 IVAL=5
f2 OUT=b nl=5 ns=10 nb=12 ORG="BSQ" 'BYTE   +
   FUNC="2*(LINE-1) + 3*(SAMP-1) + 4*(BINC-1) + 5"
write "Should get 0 differences."
difpic (a b)
!
gen a 5 10 12 ORG="BIL" 'HALF   LINC=2 SINC=3 BINC=4 IVAL=5
f2 OUT=b nl=5 ns=10 nb=12 ORG="BIL" 'HALF   +
   FUNC="2*(LINE-1) + 3*(SAMP-1) + 4*(BINC-1) + 5"
write "Should get 0 differences."
difpic (a b) 
!
gen a 5 10 12 ORG="BIP" 'REAL   LINC=2 SINC=3 BINC=4 IVAL=5
f2 OUT=b nl=5 ns=10 nb=12 ORG="BIP" 'REAL   +
   FUNC="2*(LINE-1) + 3*(SAMP-1) + 4*(BINC-1) + 5"
write "Should get 0 differences."
difpic (a b) 
!
gen a 5 10 12 ORG="BSQ" 'BYTE   
copy a b size=(2 3 4 5) bands=(5 4)
f2   a c size=(2 3 4 5) bands=(5 4) func="IN1"
write "Should get 0 differences."
difpic (b c)
!
gen a 5 10 12 ORG="BIL" 'half
copy a b size=(2 3 4 5) bands=(5 4)
f2   a c size=(2 3 4 5) bands=(5 4) func="IN1"
write "Should get 0 differences."
difpic (b c)
!
gen a 5 10 12 ORG="BIP" 'REAL4   
copy a b size=(2 3 4 5) bands=(5 4)
f2   a c size=(2 3 4 5) bands=(5 4) func="IN1"
write "Should get 0 differences."
difpic (b c)
!
GEN A 10 10 2 'BYTE BINC=0
GEN B 10 10 2 'HALF LINC=2 SINC=2 BINC=0
GEN C 10 10 2 'FULL LINC=3 SINC=3 BINC=0
GEN D 10 10 2 'REAL4 LINC=4 SINC=4 BINC=0
LIST A
LIST B
LIST C
LIST D
F2 (A,B,C,D) X FUNC="IN1+100*IN2+10000*IN3+1000000*IN4" 'FULL SB=1 NB=1
LIST X
F2 (A,B,C,D) X FUNC="IN1+100*IN2+10000*IN3+1000000*IN4" 'FULL +
	SIZE=(5,5,5,5) SB=1 NB=1
LIST X
GEN JFM 10 10 10 IVAL=0 SINC=0 LINC=0
GEN JSM 10 10 10 IVAL=10 SINC=0 LINC=0
F2 INP=(JSM,JFM) OUT=X FUNCTION="IN1+IN2"
LIST X
F2 INP=(JSM,JFM) OUT=X FUNCTION="IN1+IN2" SIZE=(5,5,5,5)
LIST X
F2 INP=(JSM,JFM) OUT=X FUNCTION="IN1+IN2" SB=1 NB=3
LIST X
F2 INP=(JSM,JFM) OUT=X FUNCTION="IN1+IN2" SB=1 NB=3 SIZE=(5,5,5,5)
LIST X
F2 OUT=X FUNCTION="LINE+SAMP+BAND" SIZE=(1,1,10,10) NB=10
LIST X
F2 OUT=X  FUNCTION="MOD(SAMP,LINE)"   'HALF  SIZE=(1,1,10,20)
LIST X
! Test Correct SL, SS handling FR#81717
! This should start out with first value 33, not 11.
GEN X nl=10 ns=10
F2 X Y (3,3,5,5) FUN="LINE*10+SAMP"
LIST Y
End-proc
$ Return
$!#############################################################################
