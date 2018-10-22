      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
C
C   **** NOTE: When you Modify this file change the "Version" message!
C
C   REVSION HISTORY:
C
C     03 Aug 15   ...WLB...   Added parms fu2, fu3, fu4 to extend the possible
C                             length of function strings.
C     22 Jul 11   ...LWK...   Hash table fails for both inputs = -32768, added
C                             special code.  Merged with RJB's changes.
C     10 Jun 11   ...RJB...   Fixes for linux gcc4.4 compiler, increased func
C                             Char variable to char*512.
C     11 Mar 08   ...LWK...   For cube files, check that all have same ORG
C     28 Nov 06   ...LWK...   Initialize INDEX_L/S/B in GET_FUNCTION
C     22 Sep 06   ...ALZ...   The third fix for this AR is going to be in
C                             subroutine XKNUTH      Ref. AR#112949. 
C     22 Sep 06   ...ALZ...   Fixed hash table case for two halfword inputs
C                             where the second input has negative values. see
C                             comments with initials "alz" below Ref. AR#112949. 
C     22 Sep 06   ...ALZ...   Fixed EXCLUDE case when floating output type
C                             hard to recreate case but see code comment
C                             with initials "alz" below Ref. AR#112949. 
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
      call xvmessage('F2 version 98-Aug-2015', ' ')

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
      INTEGER*4 COUNT,IER
      INTEGER*4 DEF,MAXLEN,ML2,ML3,ML4
      CHARACTER*7 TYPE
      CHARACTER*2000 BIGBUF
      LOGICAL*4 XVPTST

C ---    grab and parse the function
      CALL XVP ('FUNCTION', FCN, COUNT)
      CALL XVPSTAT ('FUNCTION', COUNT, DEF, MAXLEN, TYPE)

      CALL XVP ('FU2', FCN2, COUNT)
      CALL XVPSTAT ('FU2', COUNT, DEF, ML2, TYPE)

      CALL XVP ('FU3', FCN3, COUNT)
      CALL XVPSTAT ('FU3', COUNT, DEF, ML3, TYPE)

      CALL XVP ('FU4', FCN4, COUNT)
      CALL XVPSTAT ('FU4', COUNT, DEF, ML4, TYPE)

      BIGBUF(:) = " "
      BIGBUF(1:MAXLEN) = FCN(1:MAXLEN)
      BIGBUF(MAXLEN+1:MAXLEN+ML2) = FCN2(1:ML2)
      BIGBUF(MAXLEN+ML2+1:MAXLEN+ML2+ML3) = FCN3(1:ML3)
      BIGBUF(MAXLEN+ML2+ML3+1:MAXLEN+ML2+ML3+ML4) = FCN4(1:ML4)

      CALL KNUTH (BIGBUF,FBUF,IER)

      IF (IER.EQ.2) THEN
         CALL XVMESSAGE('BAD FUNCTION STRING',' ')
         CALL ABEND
         RETURN
      ELSEIF (IER.EQ.3) THEN
         CALL XVMESSAGE('EVALUATION ERROR',' ')
         CALL ABEND
         RETURN
      ENDIF

      ! initialize these in case CASE=3 but L/S/B *not* referenced:
      INDEX_L = 1
      INDEX_S = 1
      INDEX_B = 1

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
      INTEGER*4  FMT_SIZE(4) 
      DATA FMT_SIZE/1, 2, 4, 4/
      INTEGER*4 NL, NS, NL1, NS1, NB, NB1,DUMMY, MAXBAND, MAXLINE,
     1 MAXSAMP
      INTEGER*4 COUNT,STATUS,I
      CHARACTER*36 MSG
      INTEGER*4  IN1C, IN2C, IN1S, IN2S, OUTS
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
	IF (I.GT.1) THEN
	  CALL XVGET (INUNIT(I), STATUS, 'ORG', ORG1, ' ')
	  IF (ORG.NE.ORG1) CALL MABEND(' ALL INPUTS MUST HAVE SAME ORG!')
	ENDIF
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
      INTEGER*4 COUNT,I
      LOGICAL*4 XVPTST

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

      REAL*4     AMXV(4), AMNV(4), RESULT
      INTEGER*4  J,ICONV, K, CODE
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
C     K = IABS(MOD(CODE,PRIME)+1)   ! mod can go negative, k can be 0, alz
      K = MOD(IABS(CODE),PRIME)+1
      HKEY(K) = CODE
      HVAL(K) = J

      CODE = 0          ! NEED TO SHOW THAT VALUE FOR CODE=0 NOT IN TABLE.
C     K = IABS(MOD(CODE,PRIME)+1)   ! mod can go negative, k can be 0, alz
      K = MOD(IABS(CODE),PRIME)+1
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

      REAL*4    RESULT, FLOAT 
      INTEGER*4 I,J,K,STATUS,NPICS,ICONV
      INTEGER*2 IN1,IN2,IEXCL
      INTEGER*4 CODE,LIM1,LIM2
      INTEGER*4   SOUTER,SMIDDLE,SINNER,NOUTER,NMIDDLE,NINNER,
     .          IOUTER,IMIDDLE,IINNER,OUT_COUNT,MID_COUNT

      BYTE  BPIC(4*MAX_SAMP, MAXIN), B_OUTPIC(4*MAX_SAMP)
      BYTE  BPIC1(4*MAX_SAMP), BPIC2(4*MAX_SAMP)
      INTEGER*2  PIC(2*MAX_SAMP, MAXIN),  OUTPIC(2*MAX_SAMP)
      INTEGER*4    IPIC(MAX_SAMP, MAXIN),   I_OUTPIC(MAX_SAMP)
      REAL*4       FPIC(MAX_SAMP, MAXIN),   F_OUTPIC(MAX_SAMP)
      EQUIVALENCE (PIC, BPIC, IPIC, FPIC)
      EQUIVALENCE (BPIC1, BPIC(1,1)), (BPIC2, BPIC(1,2))
      EQUIVALENCE (B_OUTPIC, OUTPIC, I_OUTPIC, F_OUTPIC)
C==================================================================

C SET UP LOOP INDICES ACCORDING TO IMAGE ORGANIZATION.

	K = 0
   	SOUTER = 0
   	SMIDDLE = 0
   	SINNER = 0
   	NOUTER = 0
   	NMIDDLE = 0
   	NINNER = 0
   	IOUTER = 0
   	IMIDDLE = 0
   	IINNER = 0
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
          if (in1.eq.-32768 .and. in2.eq.-32768) then   ! lwk
            FBUF(1) = FLOAT (PIC(J,1))
            FBUF(2) = FLOAT (PIC(J,2))
            CALL XKNUTH(FBUF,  RESULT)
            go to 126
          endif
          CODE = (IN1+32768)+65536*IN2
C         K = IABS(MOD(CODE,PRIME)+1)   ! mod can go negative, k can be 0, alz
          K = MOD(IABS(CODE),PRIME)+1
          IF (HKEY(K).EQ.CODE) GO TO 125
          FBUF(1) = FLOAT (PIC(J,1))
          FBUF(2) = FLOAT (PIC(J,2))
          CALL XKNUTH(FBUF,  RESULT)
          XKC = XKC+1
          HKEY(K) = CODE
          HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 125      OUTPIC(J) = HVAL(K)
 126  ENDDO

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
          if (in1.eq.-32768 .and. in2.eq.-32768) then   ! lwk
            FBUF(1) = FLOAT (PIC(J,1))
            FBUF(2) = FLOAT (PIC(J,2))
            CALL XKNUTH(FBUF,  RESULT)
            go to 226
          endif
C         K = IABS(MOD(CODE,PRIME)+1)   ! mod can go negative, k can be 0, alz
          K = MOD(IABS(CODE),PRIME)+1
          IF (HKEY(K).EQ.CODE) GO TO 225
          FBUF(1) = FLOAT (PIC(J,1))
          FBUF(2) = FLOAT (PIC(J,2))
          CALL XKNUTH(FBUF,  RESULT)
          XKC = XKC+1
          HKEY(K) = CODE
          HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 225      OUTPIC(J) = HVAL(K)
 226  ENDDO

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
C ---    alz correction in these loops to set I_OUTPIC(J) and F_OUTPIC(J)
C ---    before the goto 238, which does not set them like section 235
              DO I=1,NUMEXC
               IF(FBUF(K).EQ.EXCLD(I)) THEN
                  IF(FMT(K).EQ.'BYTE') THEN
                     IF(AUTOREPLACE) THEN
                        IF (OUTFMT.EQ.'BYTE'.OR.OUTFMT.EQ.'HALF') THEN
                           OUTPIC(J) = BYTE2INT(REPLACE)
                        ELSE IF (OUTFMT.EQ.'FULL') THEN
                           I_OUTPIC(J) = BYTE2INT(REPLACE)
                        ELSE
                           F_OUTPIC(J) = BYTE2INT(REPLACE)
                        ENDIF
                     ELSE
                        IF (OUTFMT.EQ.'BYTE'.OR.OUTFMT.EQ.'HALF') THEN
                           OUTPIC(J) = EXCLD(I)
                        ELSE IF (OUTFMT.EQ.'FULL') THEN
                           I_OUTPIC(J) = EXCLD(I)
                        ELSE
                           F_OUTPIC(J) = EXCLD(I)
                        ENDIF
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
 320   DO  J = 1, NINNER
         IN1 = PIC(J,1)
         IN2 = PIC(J,2)
         LIM1=INT(LIMITS(1))
         LIM2=INT(LIMITS(2))
         CODE = (IN1+32768)+65536*IN2
         IF(NUM_INPUT.EQ.2) THEN
           IF(IN1.LT.LIM1 .OR. IN1.GT.LIM2 .OR.
     .        IN2.LT.LIM1.OR.IN2.GT.LIM2) THEN
                   OUTPIC(J) = REPLC
           ELSE
             if (in1.eq.-32768 .and. in2.eq.-32768) then   ! lwk
               FBUF(1) = FLOAT (PIC(J,1))
               FBUF(2) = FLOAT (PIC(J,2))
               CALL XKNUTH(FBUF,  RESULT)
               go to 326
             endif
C            K = IABS(MOD(CODE,PRIME)+1)   ! mod can go negative, k can be 0, alz
             K = MOD(IABS(CODE),PRIME)+1
             IF (HKEY(K).EQ.CODE) GO TO 325
             FBUF(1) = FLOAT (PIC(J,1))
             FBUF(2) = FLOAT (PIC(J,2))
             CALL XKNUTH(FBUF,  RESULT)
             XKC = XKC+1
             HKEY(K) = CODE
             HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 325         OUTPIC(J) = HVAL(K)
 326       ENDIF
         ELSE
           IF(IN1.LT.LIM1.OR.IN1.GT.LIM2) THEN
                   OUTPIC(J) = REPLC
           ELSE
             if (in1.eq.-32768 .and. in2.eq.-32768) then   ! lwk
               FBUF(1) = FLOAT (PIC(J,1))
               FBUF(2) = FLOAT (PIC(J,2))
               CALL XKNUTH(FBUF, RESULT)
               go to 329
             endif
C            K = IABS(MOD(CODE,PRIME)+1)   ! mod can go negative, k can be 0, alz
             K = MOD(IABS(CODE),PRIME)+1
             IF (HKEY(K).EQ.CODE) GO TO 328
             FBUF(1) = FLOAT (PIC(J,1))
             FBUF(2) = FLOAT (PIC(J,2))
             CALL XKNUTH(FBUF, RESULT)
             XKC = XKC+1
             HKEY(K) = CODE
             HVAL(K) = ICONV(AMX,AMN,RESULT,ROUND)
 328         OUTPIC(J) = HVAL(K)
 329       ENDIF
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
      INTEGER*4 NDIGITS,STATUS,I
      CHARACTER*36 MSG
      CHARACTER*12 MSGFMT

      NDIGITS = LOG10(FLOAT(MAX(1,XKC)))+1
      WRITE(MSGFMT,'(A6,I2,A4)') '(A19,I',NDIGITS,',A6)'
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


      INTEGER FUNCTION ICONV(AMX,AMN,RINPUT,ROUND)
      implicit none			! RJB
      real*4 amx,amn,rinput,round,r	! RJB
      IF (RINPUT) 1,2,2
 1    R = RINPUT-ROUND
      GO TO 3
 2    R = RINPUT+ROUND
 3    ICONV = INT(AMIN1(AMX,AMAX1(AMN,R)))
      RETURN
      END
