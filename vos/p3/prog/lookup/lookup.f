       PROGRAM  LOOKUP
C#######################################################################
C  NAME OF ROUTINE
C      LOOKUP   ( table LOOKUP )
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  FOR
C      RON ALLEY and JERRY SOLOMON
C  ORIGINAL LOOKUP PROGRAM BY
C      JOHN D. ADDINGTON  with modifications by RON ALLEY
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C      Sun4/370      SunOS/UNIX    conversion 4.30.91   Ron Alley
C     
C  REVISION HISTORY
C     7-83  SP   CONVERTED FROM IBM VICAR VERSION: MODIFIED TBLSAVE AND
C                LTABLE PARAMETER PROCESSING FOR TAE, CALL TO KEYGO
C                REPLACED WITH IN-LINE CODE, CALL TO INRACT REPLACED
C                WITH CALL TO BATCH, MISCELLANEOUS CLEANUP.
C     7-83  SP   CORRECTED CHECK FOR PRESENCE OF NEW TABLE FILE TO CORRECT
C                PROBLEM OF TABLE BEING USED AS AN IMAGE WHEN 0 < NO < NI.
C     7-83  SP   ADDED CALL TO WCHECK BEFOR WRITING OUT TABLE.
C     7-83  SP   CORRECTED PROBLEM OF TBLSAVE VALUE BEING IGNORED (IF > 1)
C                AND NEW TABLE ALWAYS BEING WRITTEN AT THE END OF THE FILE.
C     7-83  SP   ADDED CALL TO LABELC TO UPDATE LABEL OF TABLE FILE WHEN A
C                TABLE IS ADDED TO THE FILE.
C     7-83  SP   ADDED NEWTBLF PARAMETER TO CORRECT PROBLEM OF PROGRAM
C                ASSUMING TABLE FILE IS NEW IF TBLSAVE = 1.
C     7-83  SP   CHANGED IBUF,OBUF,TABLE,TABLE2 TO INTEGER*2 SO TABLE
C                LOOKUP WOULD WORK IN FORTRAN. READS AND WRITES CHANGED
C                ACCORDINGLY.  MVE TOO.
C     5-84  SP   ADDED READ BEFORE WRITE IN UPDATE MODE.
C     5-84  SP   ADDED CHECK FOR INDEXING PAST END OF TABLE ARRAY IN LIST CODE.
C     5-84  SP   CHANGED CALL ITLA(64...  TO CALL ITLA( ' '...
C     9-84  SP   CONVERTED TO USE VICAR2 CALLS (XVREAD...)
C    10-84  SP   CHANGED SO THAT AN OUTPUT TABLE FILE IS USED WHEN SAVING THE
C                TABLE.  INPUT TABLE FILES ARE NO LONGER MODIFIED.
C    10-84  SP   CORRECTED CALL TO ZIA TO ZERO 512 WORDS BECAUSE TABLE2 IS
C                INT*2.
C    10-84  SP   CHANGED IF STATEMENT TO GET 72 COLUMN TABLE LIST FOR INTERACT.
C    10-84  SP   STATCH CALLED IN LOOKUP LOOP ONLY FOR ERRORS.
C    10-84  SP   CHANGED SO XVREAD OF INPUT FILE HAS NO OPTIONALS MOST TIMES
C                (TO SPEED UP LOOP).
C    10-84  SP   TREAT PSEUDO COLOR PRODUCTION AS SPECIAL CASE TO SPEED UP.
C                LOOKUPLOOP3 SUBROUTINE ADDED.
C     4-86  REA  ADD 'PS' PARAMETER, CHANGE LOGIC FOR SL<>1.
C     8-89  REA  ADD PS 8-11 TABLES
C     4-91  REA  Conversion to UNIX/VICAR
C
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C      LOOKUP INP=(a...) OUT=(b...) SIZE=(sl,ss,nl,ns) optional parameters
C      LOOKUP INP=(a...) OUT=(b...) SL=sl SS=ss NL=nl NS=ns optional parameters
C      LOOKUP (a...) (b...) (sl,ss,nl,ns) optional parameters
C      LOOKUP (a...) (b...) optional parameters
C
C       Here (a...) represents a list of one to five file names which includes
C       from one to four input image file names optionally followed by the
C       name of a file containing the lookup table to be used by this program.
C       (b...) represents a list of one to four output image file names
C       optionally followed by an output lookup table file.
C
C      LOOKUP also accepts command lines without the INP and OUT fields.  This
C      allows the user to use LOOKUP just for working with tables.
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file names.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C               The same SIZE parameters apply to each of the input
C               image files.
C      LIST   - Listing flag.
C      TABNO  - Number (ordinal) of the lookup table if the table is in a file.
C      PS     - Number of the IDX pseudocolor lookup table to be used.
C      NEWTBLF- Flag to have lookup table saved in a file that is new.
C      SAVE   - Flag to have the lookup table saved in the next 
C               available location in the table file.
C      TBLSAVE- Table number in which to have the lookup table saved.
C      USE    - Specifies which channel of the table corresponds to which
C               output image. 
C      CHANGE - Changes to lookup table of the following type.
C               User enters  CHANGE=(n1,m1 n2,m2 ...)
C               The table entries for data number ni are, for each channel,
C               replaced with the table entries for data number mi,
C               for i=1,2,...
C      NCHAN  - The number of channels for which data is entered under 
C               the LTABLE parameter.  
C      LTABLE - Values for the lookup table.  
C  OUTPUT PARAMETERS
C      The output images produced are written to the output files. The
C      lookup table used is written to the table file if the SAVE, NEWTBLF, or
C      TBLSAVE parameters are specified.
C  PROGRAM LIMITATIONS
C      1. The input and output images must be byte data.
C      2. Maximum number of samples is 20000 per line.
C  SUBROUTINES CALLED
C      MAIN44 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C#######################################################################
C  NAME OF ROUTINE
C      MAIN44   (name of top level subroutine by VICAR convention)
C  PURPOSE
C      MAIN44 generates output images from input images using data number
C      mappings defined in a multi-channel lookup table.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  FOR
C      RON ALLEY and JERRY SOLOMON
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM LOOKUP.
C      
C  CALLED BY
C      LOOKUP
C  SUBROUTINES CALLED
C      LOOKUPLOOP, STATCH, plus the library routines  ABEND, BATCH, 
C      ITLA, IV, MVE, MVL, OUTCON, QPRINT, XVGET, XVPARM, XVPCNT, XVPTST,
C      XVOPEN, XVCLOSE, XVREAD, XVUNIT, XVWRIT, ZIA.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT INTEGER (A-Z)
      INTEGER PARM(2000),JANGE(512)
      INTEGER USE(4)/1,2,3,4/
      INTEGER*2 TABLE2(4,256)
      LOGICAL*1 IBUF(20000),OBUF(20000), OBUF2(20000), OBUF3(20000), 
     .          TABLE(256,4)
      CHARACTER*132 LBL
      INTEGER ODSN(5), IDSN(5), IARR(16)
      LOGICAL LIST, SAVE, NEWTBLF, TBLF
      LOGICAL XVPTST
C
C=================START OF EXECUTABLE CODE===============================     
C
C  DEFAULTS
      LIST=.FALSE.
      SAVE=.FALSE.
      TBLF=.FALSE.
      NEWTBLF = .FALSE.
      TABNO=0
      TABNO3=-1
      CALL ZIA(TABLE2,512)

      DO  J=1,256
         JANGE(J)=J-1
      END DO
C
C  GET THE PARAMETERS ENTERED BY THE USER.

      CALL XVPCNT( 'INP', NI )
      CALL XVPCNT( 'OUT', NO )

      NCHAN=NO
C
C
C  LOOK FOR A 'TABNO' PARAMETER

      CALL XVPARM( 'TABNO', TABNO, ICOUNT, IDEF, 1 )
      IF ( IDEF .EQ. 0 )  TBLF = .TRUE.
      IF ( IDEF .EQ. 0 .AND. TABNO .GT. 0 )  THEN

C  READ THE TABLE FROM THE INPUT DATA SET (IF PROVIDED)

         CALL XVUNIT( ITABLEFILE, 'INP', NI, IND, ' ')
         CALL XVOPEN( ITABLEFILE, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', 'U_FORMAT', 'HALF',' ' )
         CALL XVGET( ITABLEFILE, IND, 'NL', NL3, 'NS', NS3,' ' )

         IF(TABNO.GT.NL3) THEN
	    CALL XVMESSAGE(' ERROR: NON-EXISTENT TABLE REQUESTED $$$',
     +				' ')
	    CALL ABEND
	 ENDIF

         CALL XVREAD( ITABLEFILE, TABLE2, IND, 'LINE', TABNO, ' ' )
         CALL XVCLOSE(ITABLEFILE, IND, ' ')
      END IF
C
C  LOOK FOR A 'PS' PARAMETER

      CALL XVPARM( 'PS', PSNO, ICOUNT, IDEF, 1 )
      IF ( IDEF .EQ. 0 )  CALL PSTAB(PSNO,TABLE2)
C
C  GET USER PARAMETERS

      IF ( XVPTST('LIST') )         LIST=.TRUE.

      IF ( XVPTST('NEWTBLF') )   THEN
                    SAVE=.TRUE.
                    TABNO3 =  1
                    NEWTBLF = .TRUE.
                    NCHAN = NO - 1
      END IF

      IF ( XVPTST('SAVE') ) THEN
                    SAVE=.TRUE.
                    TABNO3 = -1
                    TBLF = .TRUE.
                    NCHAN = NO - 1
      END IF

      CALL XVPARM( 'TBLSAVE', PARM, ICOUNT, IDEF, 0 )
      IF (IDEF .EQ. 0)   THEN
                    SAVE=.TRUE.
                    TBLF = .TRUE.
                    TABNO3=PARM(1)
                    NCHAN = NO - 1
      END IF

      CALL XVPARM( 'USE', PARM, IVALS, IDEF, 0 )
      IF (IDEF .EQ. 0)   THEN
                    IVALS = MIN( IVALS, 4 )
                    DO   I = 1, IVALS
                       USE(I) = PARM(I)
                    END DO
      END IF

      CALL XVPARM( 'CHANGE', PARM, IVALS, IDEF, 0 )
      IF (IDEF .EQ. 0)   THEN        !! MAKE SURE USER ENTERED NUMBERS IN PAIRS.
          IF ( MOD(IVALS,2) .EQ. 1 ) THEN
	     CALL XVMESSAGE(
     +		' ERROR: ODD NUMBER OF CHANGE PARAMETERS ENTERED $$$',' ')
	     CALL ABEND
	  ENDIF
          DO   I = 1, IVALS, 2
              K=PARM(I)+1
              JANGE(K)=PARM(I+1)
          END DO
      END IF

      CALL XVPARM( 'NCHAN', PARM, ICOUNT, IDEF, 0 )
      IF (IDEF .EQ. 0)   NCHAN=PARM(1)

      CALL XVPARM( 'LTABLE', PARM, IVALS, IDEF, 0 )
      IF (IDEF .EQ. 0)   THEN
          IPAR=1
          IVLEFT = IVALS
                            !! NCHAN SHOULD BE ENTERED IF 0 OUTPUT FILES.
          IF ( NCHAN .LT. 1 ) THEN
	      CALL XVMESSAGE(
     +			' ERROR: MUST ENTER VALID NCHAN VALUE $$$',' ')
	      CALL ABEND
	  ENDIF

               ! LTABLE PARAMETERS ARE ENTERED IN GROUPS OF NCHAN+1 VALUES
               ! FOR A SINGLE DATA NUMBER OR NCHAN+2 VALUES FOR A RANGE OF
               ! DATA NUMBERS. FOR A SINGLE DATA NUMBER, THE GROUP CONSISTS
               ! OF THE DATA NUMBER FOLLOWED BY THE VALUES ASSIGNED TO THAT
               ! DATA NUMBER FOR EACH OF THE NCHAN CHANNELS.  FOR A RANGE OF
               ! DATA NUMBERS, THE GROUP CONSISTS OF THE DATA NUMBER FOR THE
               ! START OF THE RANGE FOLLOWED BY  -1 TIMES THE DATA NUMBER 
               ! FOR THE END OF THE RANGE FOLLOWED BY THE VALUES ASSIGNED TO
               ! ANY DATA NUMBER IN THAT RANGE FOR EACH OF THE NCHAN CHANNELS.

          DO WHILE  ( IVLEFT .GT. 0 )
                                                !CHECK FOR INCOMPLETE GROUP.
              IF  ( IVLEFT .LT. NCHAN+1 ) THEN
		 CALL XVMESSAGE(
     +		    ' ERROR: INVALID NUMBER OF LTABLE PARAMETER $$$',' ')
		 CALL ABEND
	      ENDIF

              J1 = PARM(IPAR) + 1
              IVLEFT = IVLEFT - 1
              IPAR   = IPAR   + 1
              J2 = J1
                                                ! CHECK FOR A RANGE.
              IF  ( PARM(IPAR) .LT. 0 )  THEN

                  J2 = - PARM(IPAR) + 1
                  IVLEFT = IVLEFT - 1
                  IPAR   = IPAR   + 1
                  				! CHECK FOR A VALID RANGE.
                  IF ( J2 .LT. J1 ) THEN
		     CALL XVMESSAGE(
     +			' ERROR: INVALID LTABLE RANGE SPECIFIED $$$',' ')
		     CALL ABEND
		  ENDIF
              END IF

              IF  ( IVLEFT .LT. NCHAN ) THEN
		 CALL XVMESSAGE(
     +		   ' ERROR: INVALID NUMBER OF LTABLE PARAMETER $$$',' ')
		 CALL ABEND
	      ENDIF
              DO J=J1,J2             		! STORE TABLE VALUES.
                  CALL MVE(-6,NCHAN,PARM(IPAR),TABLE2(1,J),1,1)
              END DO

              IPAR   = IPAR   + NCHAN
              IVLEFT = IVLEFT - NCHAN

          END DO   
      END IF

      IF ( NI .GE. 2  .OR. ( NI .EQ. 1 .AND. .NOT. TBLF) )  THEN
         CALL XVUNIT( IDSN(1), 'INP', 1, IND, ' ' )
         CALL XVOPEN( IDSN(1), IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', ' ' )
         CALL XVSIZE( SL, SS, NL, NS, NLIN, NSIN )   ! GET SIZE PARAMETER.
	 IF (SL.GT.1) CALL XVREAD(IDSN(1),IBUF,IND,'LINE',SL-1,' ')
      END IF

C
C  REFORMAT THE TABLE
      DO 205 I=1,256
        K=JANGE(I)+1
        DO 204 J=1,4
          N = TABLE2(J,K)
          CALL ITL(N,TABLE(I,J))
204     CONTINUE
205   CONTINUE
C
C  PRINT OUT THE LOOKUP TABLE
C
      IF (LIST) THEN
	CALL XVMESSAGE(' ',' ')
	WRITE (LBL,215) TABNO
215	FORMAT(' TABLE',I4)
	CALL XVMESSAGE(LBL,' ')
	DO I=1,64
	  DO J=1,4
	    M = 4*(J-1)
	    N = 64*(J-1) + I
	    IARR(M+1) = N - 1
	    IARR(M+2) = IV(TABLE(N,1))
	    IARR(M+3) = IV(TABLE(N,2))
	    IARR(M+4) = IV(TABLE(N,3))
          END DO
	  WRITE (LBL,230) (IARR(J),J=1,16)
230	  FORMAT(4I4,'    ',4I4,'    ',4I4,'    ',4I4)
	  CALL XVMESSAGE(LBL,' ')
	END DO
	CALL XVMESSAGE(' ',' ')
      END IF
C
C  SAVE THE TABLE
      IF(.NOT.SAVE)  GO TO 275
      DO 261 I=1,256
      DO 261 J=1,4
261   TABLE2(J,I)= IV( TABLE(I,J) )      ! CONVERT TO INTEGER*2.
      NS3=1024

      IF( NEWTBLF )  THEN

C       GENERATE A NEW TABLE FILE

         CALL XVUNIT( OTABLEFILE, 'OUT', NO, IND, ' ' )
         CALL XVOPEN( OTABLEFILE, IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', 'U_NL', 1, 'U_NS', NS3,
     .         'O_FORMAT', 'BYTE', 'U_FORMAT', 'HALF', ' ' )
         CALL XLADD( OTABLEFILE, 'HISTORY', 'COMMENT',
     .		    '*** LOOKUP TABLE DATASET ***', IND,
     .              'FORMAT', 'STRING', ' ' )
         CALL XVWRIT( OTABLEFILE, TABLE2, IND, ' ' )
         CALL XVCLOSE( OTABLEFILE, IND, ' ' )
         TABNO3 = 1

      ELSE

C       BUILD OUTPUT TABLE FILE FROM INPUT TABLE FILE AND CHANGES.

         CALL XVUNIT( ITABLEFILE, 'INP', NI, IND, ' ' )
         CALL XVOPEN( ITABLEFILE, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', 'U_FORMAT', 'HALF', ' ' )
         CALL XVGET( ITABLEFILE, IND, 'NL', TABLES, ' ' )

         IF ( TABNO3 .EQ. -1 )   TABNO3 = TABLES+1
         NLOT = MAX0( TABNO3, TABLES )                ! NUMBER OF LINES OUTPUT.
         CALL XVUNIT( OTABLEFILE, 'OUT', NO, IND, ' ' )
         CALL XVOPEN( OTABLEFILE, IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', 'U_NL', NLOT, 'U_NS', NS3,
     .         'O_FORMAT', 'BYTE', 'U_FORMAT', 'HALF', ' ' )

         DO  I = 1, NLOT
             IF (I.LE.TABLES) CALL XVREAD(ITABLEFILE,IBUF,IND,
     .					  'LINE',I,' ')
             IF ( I .EQ. TABNO3 )   THEN
                CALL XVWRIT(OTABLEFILE,TABLE2,IND,' ')  ! STORE NEW TABLE AT
              ELSE					! LINE TABNO3.
                CALL XVWRIT(OTABLEFILE,IBUF,IND,' ')    ! REST OF FILE IS SAME
             END IF					! AS INPUT TABLE FILE.
         END DO
         CALL XVCLOSE(ITABLEFILE, IND, ' ')
         CALL XVCLOSE(OTABLEFILE, IND, ' ')

      END IF

      WRITE(LBL,270) TABNO3
270   FORMAT(' SAVED AS TABLE',I4)
      CALL XVMESSAGE(LBL,' ')


275   CONTINUE
      IF (TBLF)   NI=NI-1
      IF (SAVE)   NO=NO-1

      IF(NI.LE.0 .OR. NO.LE.0)  GOTO 8000  ! DONE IF NO IMAGES TO PROCESS.
C
C
C  OPEN DATA SETS
      DO I=2,NI
         CALL XVUNIT( IDSN(I), 'INP', I, IND, ' ' )
         CALL XVOPEN( IDSN(I), IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', ' ' )
	 IF (SL.GT.1) CALL XVREAD(IDSN(I),IBUF,IND,'LINE',SL-1,' ')
      END DO

      DO I=1,NO
         CALL XVUNIT( ODSN(I), 'OUT', I, IND, ' ' )
         CALL XVOPEN( ODSN(I), IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .         'IO_ACT', 'SA', 'U_NL', NL, 'U_NS', NS, ' ' )
      END DO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP

      IF ( NI .EQ. 1  .AND.  NO .EQ. 3 )    THEN   ! IF PSEUDO-COLOR

          DO II=1,NL           				! LOOP OVER LINES.
              CALL XVREAD( IDSN(1), IBUF, IND, ' ' )
					                        ! DO LOOKUP.
              CALL LOOKUPLOOP3 (IBUF(SS),NS, OBUF,TABLE(1,USE(1)),
     .                    OBUF2,TABLE(1,USE(2)),OBUF3,TABLE(1,USE(3)))
              CALL XVWRIT( ODSN(1), OBUF, IND, ' ' )
              CALL XVWRIT( ODSN(2), OBUF2, IND, ' ' )
              CALL XVWRIT( ODSN(3), OBUF3, IND, ' ' )
          END DO

      ELSE

      DO 491 II=1,NL           ! LOOP OVER LINES.
         DO 471 J=1,NO             ! LOOP OVER OUTPUT IMAGES.
            IF(J.LE.NI) THEN
               CALL XVREAD( IDSN(J), IBUF, IND, ' ' )
            END IF
            CALL LOOKUPLOOP (IBUF(SS),OBUF,TABLE(1,USE(J)),NS)  ! DO LOOKUP.
            CALL XVWRIT( ODSN(J), OBUF, IND, ' ' )
471      CONTINUE
491   CONTINUE

      END IF

8000  RETURN          ! NORMAL END.
      END
C******************************************************************************
      SUBROUTINE LOOKUPLOOP (IBUF, OBUF, TABLE, NS)
C
C      LOOKUPLOOP loops through the samples of a line in an input image
C      file, reassigning the data numbers according to the lookup table.
C      
      LOGICAL*1     IBUF(NS),   OBUF(NS),  TABLE(256)
C
      DO  I = 1, NS
         J = IV( IBUF(I) ) + 1
         OBUF(I) = TABLE(J)
      END DO
      RETURN
      END
C******************************************************************************
      SUBROUTINE LOOKUPLOOP3 (IBUF, NS, OBUF, TABLE, OBUF2, TABLE2, 
     .                        OBUF3, TABLE3 )
C
C      LOOKUPLOOP loops through the samples of a line in an input image
C      file, reassigning the data numbers according to the lookup table.
C      
C      IBUF ARRAY    - DATA NUMBERS FOR THE LINE OF THE INPUT IMAGE.
C                      ( IBUF(I) FOR I = 1 TO NS. )
C      TABLE ARRAY   - LOOKUP TABLE. ( TABLE(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 1.
C      TABLE2 ARRAY  - LOOKUP TABLE. ( TABLE2(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 2.
C      TABLE3 ARRAY  - LOOKUP TABLE. ( TABLE3(N) FOR N = 1 TO 256.)
C                      THE TABLE MAPS M, A DATA NUMBER IN THE RANGE 0 TO
C                      255, INTO  TABLE(M+1).  FOR OUTPUT 3.
C      NS            - NUMBER OF SAMPLES (DATA NUMBERS) IN THE LINE.
C  OUTPUT PARAMETERS
C      OBUF ARRAY    - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 1.
C                      ( OBUF(I) FOR I = 1 TO NS. )
C      OBUF2 ARRAY   - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 2.
C                      ( OBUF2(I) FOR I = 1 TO NS. )
C      OBUF3 ARRAY   - LOOKED-UP DATA NUMBERS FOR THE OUTPUT IMAGE 3.
C                      ( OBUF3(I) FOR I = 1 TO NS. )
C
      LOGICAL*1  IBUF(NS), OBUF(NS), TABLE(256), OBUF2(NS), TABLE2(256),
     .           OBUF3(NS), TABLE3(256)
C
      DO  I = 1, NS
         J = IV( IBUF(I) ) + 1
         OBUF(I) =  TABLE(J)
         OBUF2(I) = TABLE2(J)
         OBUF3(I) = TABLE3(J)
      END DO
      RETURN
      END
C**************************************************************************
	SUBROUTINE PSTAB(NUM,TABLE)
C
C      PSTAB places into the array TABLE the IDISPLAY pseudocolor table
C      that corresponds to the number NUM.
C      
C  INPUT PARAMETER
C      NUM	     - THE NUMBER OF THE PSEUDOCOLOR TABLE DESIRED.
C  OUTPUT PARAMETER
C      TABLE ARRAY   - THE PSEUDOCOLOR TABLE ITSELF
C
	INTEGER*2 TABLE(4,256)
	INTEGER*2 PS(256,3,11)
	INTEGER*2 PS1(256,3)/128*0,32*84,96*255,			    !R
     +		     32*0,32*128,64*255,32*200,32*255,32*128,32*0,	    !G
     +		     96*255,160*0/					    !B
	INTEGER*2 PS2(256,3)/16*170,16*115,16*125,16*0,16*50,16*30,64*0,    !R
     +		     16*200,80*255,
     +		     32*0,16*70,16*50,16*110,16*150,16*200,16*185,	    !G
     +		     16*225,48*255,16*215,16*160,16*100,16*0,
     +		     16*255,16*210,16*240,16*255,16*224,32*200,16*120,	    !B
     +		     16*100,16*0,16*150,80*0/
	INTEGER*2 PS3(256,3)/104*0,8*90,8*130,16*220,48*255,8*200,8*220,    !R
     +		     8*255,8*230,8*245,32*255,
     +		     32*0,8*70,8*100,8*130,8*150,8*170,8*190,8*210,	    !G
     +		     8*220,8*200,8*180,16*170,8*200,8*220,8*240,8*255,
     +		     8*140,8*120,8*80,8*60,40*0,8*50,8*0,8*255,
     +		     8*0,8*160,8*200,8*255,8*230,8*200,8*170,8*150,	    !B
     +		     8*130,8*100,8*80,8*60,64*0,24*80,8*60,16*0,8*100,
     +		     8*120,8*150,8*180,16*255/
	INTEGER*2 PS4(256,3)/112*0,16*130,80*255,16*230,32*255,		    !R
     +		     32*0,16*100,16*130,16*170,16*210,16*200,16*170,	    !G
     +		     16*220,16*255,16*140,16*80,64*0,
     +		     16*160,16*255,16*200,16*170,16*130,16*80,64*0,	    !B
     +		     32*80,16*0,16*100,16*150,16*255/
	INTEGER*2 PS5(256,3)/128*0,128*255,				    !R
     +		     32*0,32*130,32*170,32*200,32*255,32*80,64*0,	    !G
     +		     32*255,32*170,32*130,64*0,32*80,32*0,32*150/	    !B
	INTEGER*2 PS6(256,3)/128*0,128*255,				    !R
     +		     42*0,43*170,43*200,43*255,85*0,			    !G
     +		     42*255,43*130,128*0,43*150/			    !B
	INTEGER*2 PS7(256,3)/128*0,128*255,				    !R
     +		     64*0,64*190,64*255,64*0,				    !G
     +		     64*255,64*100,128*0/				    !B
	INTEGER*2 PS8(256,3)/24*0,8*50,16*80,32*0,8*50,8*100,8*150,         !R
     +		     8*190,8*210,136*255,
     +		     24*0,8*50,8*80,8*120,8*150,8*200,8*220,24*255,	    !G
     +		     16*240,24*255,8*220,16*180,8*150,8*120,8*75,32*0,
     +		     8*80,8*120,8*160,8*200,8*255,
     +		     8*0,8*180,40*255,8*200,8*130,8*100,8*50,32*0,8*40,     !B
     +		     24*90,40*0,8*120,8*180,48*255/
	INTEGER*2 PS9(256,3)/16*0,16*80,32*0,16*50,16*150,160*255,	    !R
     +		     16*0,16*80,16*200,32*255,16*240,16*255,16*200,	    !G
     +		     16*180,16*150,16*120,16*75,32*0,16*80,16*160,
     +		     32*255,16*200,16*100,16*50,32*0,16*90,80*0,	    !B
     +		     16*180,32*255/
	INTEGER*2 PS10(256,3)/0,25*50,26*0,25*50,26*190,153*255,	    !R
     +		     0,25*50,26*200,25*255,26*240,25*255,25*180,26*120,	    !G
     +		     51*0,25*120,255,
     +		     0,25*255,26*200,25*50,26*0,25*90,76*0,52*255/	    !B
	INTEGER*2 PS11(256,3)/0,31*50,32*0,32*50,32*190,128*255,	    !R
     +		     0,31*50,32*200,32*255,32*240,32*255,32*150,32*0,	    !G
     +		     31*80,255,
     +		     0,31*255,32*200,32*50,32*0,32*90,64*0,32*255/	    !B
	EQUIVALENCE (PS(1,1,1),PS1),(PS(1,1,2),PS2),(PS(1,1,3),PS3),
     +		    (PS(1,1,4),PS4),(PS(1,1,5),PS5),(PS(1,1,6),PS6),
     +		    (PS(1,1,7),PS7),(PS(1,1,8),PS8),(PS(1,1,9),PS9),
     +		    (PS(1,1,10),PS10),(PS(1,1,11),PS11)
C
	IF (NUM.EQ.0) THEN
	    DO I=1,256
		TABLE(1,I) = I-1
		TABLE(2,I) = I-1
		TABLE(3,I) = I-1
		TABLE(4,I) = I-1
	    END DO
	ELSE
	    DO I=1,256
		TABLE(1,I) = PS(I,1,NUM)
		TABLE(2,I) = PS(I,2,NUM)
		TABLE(3,I) = PS(I,3,NUM)
		TABLE(4,I) = 0
	   END DO
	END IF
	RETURN
	END
