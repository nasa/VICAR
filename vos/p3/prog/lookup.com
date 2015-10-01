$!****************************************************************************
$!
$! Build proc for MIPL module lookup
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:38
$!
$! Execute by entering:		$ @lookup
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
$ write sys$output "*** module lookup ***"
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
$   if F$SEARCH("lookup.imake") .nes. ""
$   then
$      vimake lookup
$      purge lookup.bld
$   else
$      if F$SEARCH("lookup.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lookup
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lookup.bld "STD"
$   else
$      @lookup.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lookup.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lookup.com -
	-s lookup.f -
	-p lookup.pdf -
	-i lookup.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lookup.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create lookup.pdf
process help=*
!  FILE NAMES      INPUT AND OUTPUT FILE NAMES ARE OPTIONAL
!
PARM INP     TYPE=STRING   COUNT=(0:5)   DEFAULT=--
PARM OUT     TYPE=STRING   COUNT=(0:5)   DEFAULT=--
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM LIST    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=LIST
PARM TABNO   TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM PS	     TYPE=INTEGER  COUNT=1	 DEFAULT=0	    VALID=(0:11)
PARM NEWTBLF TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=NEWTBLF
PARM SAVE    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=SAVE
PARM TBLSAVE TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM USE     TYPE=INTEGER  COUNT=(1:4)   DEFAULT=(1,2,3,4)  VALID=(1:4)
!
PARM CHANGE  TYPE=INTEGER  COUNT=(2:300) DEFAULT=(0,0)      VALID=(0:255)
PARM NCHAN   TYPE=INTEGER  COUNT=1       DEFAULT=1          VALID=(1:4)
!
!   LTABLE HAS A THEORETICAL MAXIMUM COUNT OF 1280.  MAX FOR TAE IS 300?
PARM LTABLE  TYPE=INTEGER COUNT=(2:300)   DEFAULT=(0,0)     VALID=(-255:255)
!
END-PROC
.TITLE
LOOKUP
.HELP
 PURPOSE:

LOOKUP generates output images from input images using data number
mappings defined in a multi-channel lookup table.  The user can specify 
the table by giving its location (if it is contained in a file), by 
entering the contents of the table as program parameters, or by naming an 
IDX pseudocolor transformation. The user has the option of modifying the 
table via the CHANGE and LTABLE parameters. The modified table may then be 
saved by specifying the SAVE or TBLSAVE parameter.
.PAGE
 EXECUTION:

In the table format used by LOOKUP, a lookup table is made up of four
independent channels.  Each channel defines a mapping or discrete
transfer function to be applied to an input image. Some of these
channels may be undefined depending on the application.  The lookup
table is limited to four channels because LOOKUP allows at most
four output images.  LOOKUP allows at most four input images.

In the most straightforward use of LOOKUP, the number of input images
is equal to the number of output images and the default channel
assignments are used.  In this case, the first channel in the table
is used to map the first input image to the first output image, the
second channel in the table is used to map the second input image to 
the second output image, and so on.
.PAGE
The user may select to use different channels (than the default) with
the images by using the USE parameter.  The user may also specify
fewer input images than output images.  In this case, the last input
image is mapped through the remaining channels to create the appropriate
number of output images.  Thus, one input image could be mapped through
three independent channels (transfer functions) to three output images.
This feature allows simple production of Pseudo Color pictures.
.PAGE
When the user specifies a lookup table in a file, LOOKUP requires that
the file have a particular format.  The file must contain a VICAR label
followed by one or more tables, each table being stored as one 1024-byte
record.  A table record contains 1024 output data number values in the
following order:

 d(0,1), d(0,2), d(0,3), d(0,4),... d(255,1), d(255,2), d(255,3), d(255,4),

where d(i,j) is the output data number to which the data number i is mapped
by channel j.  The user specifies the location of the table by giving the
name of the file and the number (ordinal) of the table within the file.  
In general, such a file is built using another VICAR program or using
LOOKUP itself.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      LOOKUP INP=(a...) OUT=(b...) SIZE=(sl,ss,nl,ns) optional parameters
      LOOKUP INP=(a...) OUT=(b...) SL=sl SS=ss NL=nl NS=ns optional parameters
      LOOKUP (a...) (b...) (sl,ss,nl,ns) optional parameters
      LOOKUP (a...) (b...) optional parameters

       Here (a...) represents a list of one to five file names which includes
       from one to four input image file names optionally followed by the
       name of a file containing the lookup table to be used by this program.
       (b...) represents a list of one to four output image file names
       optionally followed by an output lookup table file.

      LOOKUP also accepts command lines without the INP or OUT fields.  
      This allows the user to use LOOKUP just for working with tables.
.PAGE
EXAMPLES

1.    LOOKUP  INP=(L1 T1) OUT=L2 'LIST TABNO=2 USE=3

      In this example, channel 3 of table 2 of file T1 is used to map
      image file L1 to image file L2.

2.    LOOKUP IN (RED,GREEN,BLUE) PS=3

      In this example, the IDX pseudocolor transformation PS 3 is used to
      map image file IN to files RED, GREEN, and BLUE. The color assignments
      must be in this order to get results corresponding to what is seen in
      IDX.
.PAGE
3.    LOOKUP INP=(L1) OUT=(L3,L4,L5,T2) 'NEWTBLF        +
             LTABLE=( 0       0  1  2                   +
                      1  -98  2  3  4                   +
                      99 -255 5  6  7                     )

      In this example, the three channels of the table defined through the
      LTABLE parameter are used to map the input file to the three output
      files.  T2 is the output table file, in which the table is saved.

RESTRICTIONS
1. The input and output images and the table file must be byte data.
2. Maximum number of samples is 20000 per line.

 WRITTEN BY:                 Steven Pohorsky           2 Aug 1983
 COGNIZANT PROGRAMMER:       Steven Pohorsky
 REVISION:                   3                        24 April 1986

.LEVEL1
.VARIABLE INP
Input file name(s);
 includes input images
 optionally followed by
 table file.
.VARIABLE OUT
Output file name(s)
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE LIST
Enter to get the lookup
table listed to your terminal.
.VARIABLE TABNO
Number (ordinal) of the lookup
table if the table is in a file.
This parameter is required
if the table is in a file.
.VARIABLE PS
Number of the IDX pseudocolor
table, if the table is a
standard IDX pseudocolor.
Valid: 1-11.
.VARIABLE NEWTBLF
Enter to have the
lookup table saved if the
table file is new.
.VARIABLE SAVE
Enter to have the
lookup table saved in the next 
available location in an
existing table file.
.VARIABLE TBLSAVE
To have the lookup table saved
in an exactly specified
location in an existing table 
file, enter the table number
(location).
.VARIABLE USE
Specifies which channel of the
table corresponds to which
output image. The nth value
entered is the channel for
the nth output image.
.VARIABLE CHANGE
One or more of the following
type of changes can be entered
using the following format:
CHANGE=(n1,m1 n2,m2 ...)
The table entries for data
number ni are, for each channel,
replaced with the table entries
for data number mi, for
i=1,2,...
.VARIABLE NCHAN 
The number of channels for which
data is entered under the LTABLE
parameter.  The default for
NCHAN is the number of output
images.
.VARIABLE LTABLE
Specifies values for the lookup
table.  If a table from a file
is not used, then the table
must be defined through LTABLE
values.
.LEVEL2
.VARIABLE INP
If an input table file is specified, it must be the last input file listed,
and the TABNO, TBLSAVE, or SAVE parameter must be specified.
.VARIABLE OUT
If an output table file is specified, it must be the last output file listed,
and the NEWTBLF, TBLSAVE, or SAVE parameter must be specified.
.VARIABLE LIST
The default LIST value is no listing of the lookup table.
.VARIABLE TABNO
The basis for the lookup table will be the table number (record number)
specified by this parameter. If this parameter is used, a lookup table
dataset must be supplied as the last input dataset.
.VARIABLE PS
The basis for the lookup table will be the IDX pseudocolor table of the same
number. IT may then be modified via the LTABLE parameter. Valid tables are:
      PS=0      for an identity transformation (all outputs like input)
         1      for the original 8 color table
         2      for the original 16 color table
         3      for the original 32 color table
         4      for the 16 color table, with colors selected for screen display
         5      for the 8 color table, with colors selected for screen display
         6      for the 6 color table
         7      for the 4 color table
         8      for the 32 color table, with colors selected for MDA playback
         9      for the 16 color table, with colors selected for MDA playback
        10      for the 10 color table, with colors selected for MDA playback
        11      for the 8 color table, with colors selected for MDA playback
PS3 and PS8 use black and white as two of the 32 colors; PS10 and PS11 use
black and white in addition to the stated number of colors to denote saturation.
They map only 0 and 255 input. All other tables do not use black or white.
.VARIABLE NEWTBLF
If you wish to save (write out) the lookup table and the 
table file is new, enter 'NEWTBLF. The lookup table will be saved as table 1.
 If NEWTBLF is specified, then neither SAVE nor TBLSAVE is specified.
.VARIABLE SAVE
If you wish to save (write out) the lookup table in an existing table
file, enter either 'SAVE to have the lookup table saved in the next 
available location in the table file,      or
enter a specific location (table number) for TBLSAVE.
The modified table file will be written to the output table file specified
in the OUT parameter.  The input table file is not modified.
.VARIABLE TBLSAVE
If you wish to save (write out) the lookup table in an existing table
file, enter either 'SAVE to have the lookup table saved in the next 
available location in the table file,      or
enter a specific location (table number) for TBLSAVE.
The modified table file will be written to the output table file specified
in the OUT parameter.  The input table file is not modified.
.VARIABLE USE
The default is USE=(1,2,3,4).
.VARIABLE CHANGE
Pairs of values are entered following the CHANGE keyword.  Each pair
of values represents a change to the lookup mapping.  Although the
program accomplishes the change by changing the table, the effect on
the lookup mapping can best be explained as changing input data numbers
of the first value (in the pair) to the second value (of the pair)
before the lookup table is applied.  Each change is based
on the table specified and is independent of other changes
specified with it.
The CHANGE parameters may be used with or without LTABLE parameters.
If both LTABLE and CHANGE parameters are used, the CHANGE parameters
are applied to the result produced by the LTABLE parameters.
.VARIABLE LTABLE
By specifying LTABLE parameters the user can apply modifications to
an existing table or can completely define a new table.  These
parameters specify the data numbers for each channel to which
given data numbers are to be mapped.

LTABLE parameters are entered in groups of NCHAN+1 values
for a single data number or NCHAN+2 values for a range of
data numbers. For a single data number, the group consists
of the data number followed by the values assigned to that
data number for each of the NCHAN channels.  For a range of
data numbers, the group consists of the data number for the
start of the range followed by  -1 times the data number 
for the end of the range followed by the values assigned to
any data number in that range for each of the NCHAN channels.

When modifying an existing table, the user enters groups of 
parameters only for the data numbers for which changes are
desired.  When defining a new table, the user enters the full
table so that a mapping is defined for all of the data numbers
from 0 to 255.

The following example illustrates the format and usage of the LTABLE
parameters.  In this example the mapping is completely defined for
two channels.  The first channel maps  0 to 1, 1 to 3, and 2 through
255 to 5.  The second channel maps  0 to 2, 1 to 4, and 2 through
255 to 6.
          LTABLE = ( 0         1  2           +
                     1         3  4           +
                     2  -255   5  6             )

The LTABLE parameters may be used with or without CHANGE parameters.
If both LTABLE and CHANGE parameters are used, the CHANGE parameters
are applied to the result produced by the LTABLE parameters.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create lookup.imake
#define  PROGRAM   lookup

#define MODULE_LIST lookup.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
