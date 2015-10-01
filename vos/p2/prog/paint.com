$!****************************************************************************
$!
$! Build proc for MIPL module paint
$! VPACK Version 1.9, Saturday, March 06, 2010, 11:11:22
$!
$! Execute by entering:		$ @paint
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
$ write sys$output "*** module paint ***"
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
$ write sys$output "Invalid argument given to paint.com file -- ", primary
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
$   if F$SEARCH("paint.imake") .nes. ""
$   then
$      vimake paint
$      purge paint.bld
$   else
$      if F$SEARCH("paint.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake paint
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @paint.bld "STD"
$   else
$      @paint.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create paint.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack paint.com -mixed -
	-s paint.f -
	-i paint.imake -
	-p paint.pdf -
	-t tstpaint.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create paint.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C    VICAR 2 PROGRAM PAINT
C
C
C    PROGRAM NAME: PAINT ( PAINT each region of an image a different color )
C
C    PURPOSE:
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C
C__________________________________________________________________________
C
C  ORIGINAL CONVERSION FROM IBM:
C  ----------------------------
C
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C
C  ORIGINAL PAINT PROGRAM BY
C      AL ZOBRIST
C
C  CONVERSION PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  FOR
C      RON ALLEY and JERRY SOLOMON
C
C     
C  REVISION HISTORY
C     7-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C     7-83  SP   INCREASED MAXIMUM LINE SIZE TO 64000 PIXELS.
C     7-83  SP   CORRECTED LABEL PROCESSING OF INPUT FILE BY USING LABELC.
C     7-83  SP   ALLOWED FOR STARTING SAMPLE AND STARTING LINE NOT EQUAL TO 1.
C     8-83  SP   IMPLEMENTED BORIS GOKHMAN'S CHANGES TO THE IBM PAINT WHICH
C                ALLOW PAINT TO HANDLE UP TO 65534 REGIONS.
C     8-83  SP   MADE RGNMX INDEPENDENT OF BORDER PARAMETER ENTERED BY USER.
C     8-83  SP   CORRECTED PROBLEM WHERE PRGNMX, AND RGNMX NOT INITIALIZED 
C                (AND KWD(6) INCORRECT) IF USER ENTERS NO OPTIONAL PARAMETERS.
C     8-83  SP   CHANGED HALF PARAMETER TO FORMAT=HALF/BYTE SO USER CAN
C                OVERRIDE LABEL WHEN LABEL SAYS HALF.
C     8-83  SP   CHANGED LABEL PROCESSING OF OUTPUT FILE TO INCLUDE CORRECT
C                DATA FORMAT IN LABEL. ALSO FOR WORK FILE.
C     9-83  SP   IMPLEMENTED BORIS GOKHMAN'S CHANGES TO THE IBM PAINT WHICH
C                ALLOW PAINT TO PRODUCE HALFWORD OUTPUT FROM BYTE INPUT.
C                ALLOWED FORMAT PARAMETER TO HAVE VALUE 'BH' (BYTE-TO-HALF).
C     9-84  SP   CORRECTED BUG THAT CAUSED ZEDGE OPTION TO FAIL IN SOME CASES.
C                RESET DN OF THE POINT TO THE RIGHT OF THE IMAGE LINE.
C
C__________________________________________________________________________
C
C  VICAR 2
C
C  ENVIRONMENT
C      VAX 11/780 - 8600    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      BARBARA MCGUFFIE              JPL     3/86
C     
C  REVISION HISTORY
C      A. VICAR 2 IO 
C      B. REPLACED "FORMAT" WITH 'BYTE 	OR 'HALF or FORMAT (RIDICULOUS)
C      C. INPUT FORMAT BYTE ONLY
C      D. MSTP S/W CONVERSION (VICAR PORTING) ...CRI... MARCH 6 1995
C
c  06Feb2010 -lwk- replaced inline fcn. using JZEXT (not supported by new
c		  compiler) by new external function
C__________________________________________________________________________
C
C
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      paint INP=a OUT=(b,w) SIZE=(sl,ss,nl,ns) optional parameters
C      paint INP=a OUT=(b,w) SL=sl SS=ss NL=nl NS=ns optional parameters
C      paint a (b,w) (sl,ss,nl,ns) optional parameters
C      paint a (b,w) optional parameters
C
C       Here 'a' represents the input image file name,
C       'b' represents the output image file name, and 
C       'w' represents the work file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file name.
C      OUT    - Output file name followed by work file name.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C      FORMAT - BYTE, HALF, or BH (byte input with halfword output). 
C      BORDER - Data number value used to define border pixels.
C      PBORDER- Paint-borders flag
C      THRESH - Threshold number. 
C      ZEDGE  - Specified if areas along the edge of the image are not to be
C               counted as regions and are to be assigned zero data numbers.
C      DEBUG  - Debug switch.
C
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The input and output images must be byte or halfword data.
C      2. Maximum number of pixels is 64000 per line.
C      3. Maximum number of regions is 254 for byte data and 65534 for
C         halfword data.
C      4. Maximum number of preliminary regions is 65534.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                 ORIGINAL HEADER COMMENTS.
C
C  IBIS ROUTINE PAINT
C
C  PAINT CONVERTS AN IMAGE FILE OF POLYGON BORDERS INTO A MULTICOLORED M
C  IN WHICH EACH COLOR (DN) CORRESPONDS TO AN INDIVIDUAL POLYGON.  THE
C  INPUT IMAGE MUST CONSIST OF NON-ZERO DN LINES AGAINST A ZERO-DN BACKG
C  OPTIONS ALLOW THE USER TO ERASE POLYGON BORDERS AND ZERO OUT AREAS
C  TOUCHING THE EDGE OF THE FRAME.
C
C  USER PARAMETERS:
C
C  HALF -    INDICATES THAT THE INPUT AND OUTPUT DATA SETS ARE HALFWORD.
C  BORDER,N - THE INTEGER N SPECIFIES THE DN VALUE USED TO DEFINE THE
C            BORDERS OF SCRIBED POLYGONS.  VALUES LARGER THAN N ARE ALSO
C            SIDERED TO BE BORDER VALUES.
C  PBORDER - INDICATES THAT POLYGON BORDERS ON THE OUTPUT FILE ARE TO BE
C            ERASED.
C  THRESH,N - THE INTEGER N SPECIFIES THAT ALL REGIONS WITH FEWER THAN N
C            ARE TO BE TURNED INTO BORDER PIXELS.  THEY ARE SUBSEQUENTLY
C            ERASED IF THE PBORDER OPTION IS SPECIFIED.
C  ZEDGE -   THE PARAMETER ZEDGE INDICATES THAT EDGE POLYGONS ARE TO BE
C            ZEROED OUT.  EDGE POLYGONS ARE DEFINED AS POLYGONS WHOSE
C            INTERIOR TOUCHES THE EDGE OF THE IMAGE AREA.
C
C                 END OF ORIGINAL HEADER COMMENTS.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC




      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 


      IMPLICIT INTEGER(A-Z)



      PARAMETER ( LSIZE_PAR = 64000 )      ! MAX NUMBER OF PIXELS PER LINE.
      PARAMETER ( LBSIZE_PAR= LSIZE_PAR+10) !LINE BUFFER SIZE. (10 EXTRA 
                                            !ELEMENTS FOR SPIRAL ALGORITHM).
      PARAMETER ( JSIZE_PAR = 65534 )      ! MAX NUMBER OF REGIONS ALLOWED.
      PARAMETER ( MAXINT_PAR= 32767 )      ! MAX INTEGER FOR I*2.
      PARAMETER ( BORDER_PAR= 65535 )      ! MAX UNSIGNED INTEGER FOR I*2. USED
                                           ! TO DESIGNATE A BORDER PIXEL.


      INTEGER*2 PREV,LINE,NEXT,JOIN,NWLINE,BJOIN,COIN
      CHARACTER*24 NRGM
      CHARACTER*28 NPRGM
      LOGICAL NEWCRY,ZEDGE,PBORD,DEBUG,XVPTST,OUT_HALF,OUT_BYTE


      DIMENSION NWLINE(LBSIZE_PAR)
      DIMENSION SEQ(64),ASEQ(64)

      ! ADDITIONAL DECLARATIONS TO ALLOW UP TO 65534 REGIONS.
      ! THESE DECLARATIONS ALLOW REPRESENTATION OF 16-BIT
      ! UNSIGNED VALUES IN I*2 ARRAYS.


      ! NOTE THAT ON VAX THE FIRST HALFWORD (NOT THE SECOND) CONTAINS THE
      ! LOW-ORDER 16 BITS.

       INTEGER  BORDX2,LIN2,LFT2,ABV2,NEW2,P2,KK2

       INTEGER*4  FULLWORD      ! FULLWORD IS A FUNCTION THAT MOVES A
       INTEGER*2  NHALF         ! 16-BIT UNSIGNED NUMBER INTO A FULL WORD.

        ! BORDVAL IS 65535 IN 16-BIT UNSIGNED FORMAT.
       INTEGER*2  BORDVAL2
       INTEGER SIZE(4),LOWHALF,TMPHALF

      COMMON /COM1/PREV(LBSIZE_PAR),LINE(LBSIZE_PAR),NEXT(LBSIZE_PAR),
     &             COIN(JSIZE_PAR),BJOIN, JOIN(JSIZE_PAR)

c       EQUIVALENCE ( BORDVAL4,BORDVAL2(1) )  , ( BORDVAL2(1), BORDVAL )

c       EQUIVALENCE ( BORDX, BORDX2(1) )
c       EQUIVALENCE ( LIN4, LIN2(1) )
c       EQUIVALENCE ( LFT, LFT2(1) )
c       EQUIVALENCE ( ABV, ABV2(1) )
c       EQUIVALENCE ( NEW, NEW2(1) )
c       EQUIVALENCE ( P, P2(1) )
c       EQUIVALENCE ( KK, KK2(1) )

       ! END OF ADDITIONAL DECLARATIONS

      DATA JSIZE / JSIZE_PAR/

       ! HORIZONTAL OFFSETS FOR SPIRAL SEARCH ALGORITHM (FOR PBORDER)

      DATA SEQ/  1, -1, 0,  0, 1, -1, -1, 1,
     &           2, -2, 2, -2, 2, -2,
     &           3, -3, 3, -3, 3, -3,
     &           4, -4, 4, -4, 4, -4,
     &           5, -5, 5, -5, 5, -5,
     &          -1,  1, 0,  0,-1,  1,  1, -1,
     &           2, -2, 2, -2, 2, -2,
     &           3, -3, 3, -3, 3, -3,
     &           4, -4, 4, -4, 4, -4,
     &           5, -5, 5, -5, 5, -5     /


        ! VERTICAL OFFSETS FOR SPIRAL SEARCH ALGORITHM

      DATA ASEQ/  0,  0, 1, -1,  1, -1,  1,  -1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  0,-1,  1, -1,  1, -1,   1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  1, 1,  0, -1, -1,
     &            0,  1, 1,  0, -1, -1   /



c     FULLWORD(NHALF) = JZEXT(NHALF)    ! FULLWORD IS A FUNCTION THAT MOVES A
                                     ! 16-BIT UNSIGNED NUMBER INTO A FULL WORD.
c  JZEXT not supported by new compiler (lwk / 06-Feb-2010)

C----------------------------------------------------------------------------
C
C                        INITIALIZE PARAMETERS
C
C----------------------------------------------------------------------------



      NPRGM='  XXXXXX PRELIMINARY REGIONS'
      NRGM= '  XXXXXX REGIONS PAINTED'

      DEBUG    = .FALSE.                ! initialize debug flag
      
      BORDER  = 255                     ! set border value
      thresh  = 0                       ! initialize threshold parameter
      myrand  = 5555
      BORDVAL2 = -1

C----------------------------------------------------------------------------
C
C                    OPEN INPUT FILE AND GET NL, NS
C
C----------------------------------------------------------------------------

      call ifmessage('PAINT version 6-MAR-10')

c   assume always byte data input


      call xvunit (infile, 'inp', 1, status,' ')  ! open input image
      if (status .ne. 1) then                     ! terminate on error
        call mabend ('Input unit error. Program terminated.')
      endif
      call xvopen(infile, status,'OPEN_ACT','SA',
     -            'I_FORMAT','BYTE','U_FORMAT','HALF',' ')


      call xvget(infile, status, 'nl', nl, 'ns', nbytes,' ')  ! fetch nl & ns
      if (status .ne. 1) call mabend('XVGET error. Program terminated.')

C----------------------------------------------------------------------------
C
C                        PARAMETER PROCESSOR
C
C----------------------------------------------------------------------------

      out_half = xvptst('HALF')              ! get flag for output type 
      out_byte = xvptst('BYTE')              ! default to byte
      if ( .not. out_byte .and. .not. out_half ) out_byte = .TRUE.
 


      call xvparm('border',border,count,def,1) ! get border value if entered

      PBORD = xvptst('pborder')              ! paint border flag

      ZEDGE = xvptst('zedge')                ! zero edge flag

      call xvparm('thresh',thresh,count,def,1) ! get threshold value if any



      call xvparm('SIZE',size,count,def,4)     ! user required image area
	if ( count .gt. 0 .and. def .eq. 0 ) then
	  sline = size(1)
	  ssamp = size(2)
	  nline = size(3)
	  nsamp = size(4)
	else
          call xvparm('sl', sline, count, def,1)
          call xvparm('ss', ssamp, count, def,1)
          call xvparm('nl', nline, count, def,1)
          call xvparm('ns', nsamp, count, def,1)
	endif
        if ( sline .eq. 0 ) sline = 1
        if ( ssamp .eq. 0 ) ssamp = 1
        if ( nline .eq. 0 ) nline = nl
        if ( nsamp .eq. 0 ) nsamp = nbytes

        tlines = nline - sline + 1         ! no .of lines in the output image
        tsamps = nsamp - ssamp + 1         ! no. of samples in the output image


      DEBUG = xvptst('debug')              ! debug flag





C----------------------------------------------------------------------------
C
C                        OPEN  OUTPUT  FILES
C
C----------------------------------------------------------------------------



      call xvunit (outfile, 'out', 1, status,' ') ! get unit for output data set
      if (status .ne. 1) call mabend ('outfile unit error')

      if ( out_half )                          ! halfword data set out
     &  call xvopen(outfile, status, 'OP', 'WRITE', 'O_FORMAT', 'HALF',
     &              'U_FORMAT','HALF','U_NL', tlines, 'U_NS', tsamps,
     &              'OPEN_ACT','SA',' ')

      if ( out_byte )                          ! byte data set out
     &  call xvopen(outfile, status, 'OP', 'WRITE', 'O_FORMAT', 'BYTE',
     &              'U_FORMAT','HALF','U_NL', tlines, 'U_NS', tsamps,
     &              'OPEN_ACT','SA',' ')



                                            ! open the intermediate work file
      call xvunit (wrkfile, 'out', 2, status,' ')
      if (status .ne. 1) call mabend('workfile unit error') ! terminate on error
      call xvopen(wrkfile, status, 'OP', 'WRITE', 'O_FORMAT', 'HALF',
     &            'U_FORMAT','HALF','OPEN_ACT','SA',' ')





C----------------------------------------------------------------------------
C
C                        START EXECUTABLE CODE
C
C----------------------------------------------------------------------------


      PRGNMX = JSIZE
      IF ( out_half )  THEN
          RGNMX = JSIZE
      ELSE
          RGNMX = 254            ! MAX OF 254 REGIONS FOR BYTE-DATA IMAGES.
      ENDIF


C
C  FIRST PASS.  REGIONS ARE PARTIALLY CONNECTED BY ADJACENCY TO LEFT AND
C  ABOVE READING TWO LINES AT A TIME.  THE MEETING OF TWO REGIONS IS REC
C  VECTOR JOIN VIA CIRCULAR LINKS.  SIZE OF SMALL REGIONS IS KEPT IN VEC
C  COIN.
C  THIS PASS CAN BE SPEEDED UP BY NOT COPYING AT #4 AND USING CORRECTED
C  IN REMAINDER OF #100 LOOP.
C
      BORDX2 = -1     ! establish border type
      IF (ZEDGE) BORDX2 = 1

      REC2 = SLINE - 1       ! initialize read start line
      REC3 = 0               ! initialize write start line

      NSU =  NSAMP

      DO I=1,NSU+10
         PREV(I) = BORDX2 ! THIS SURROUNDS THE IMAGE WITH BORDER POINTS
         LINE(I) = BORDX2 ! UNLESS ZEDGE IS USED. IF ZEDGE IS USED, THEN ALL
      END DO
      NEW = 0                ! EDGE REGIONS ARE CONNECTED THROUGH THESE POINTS
                             ! SURROUNDING THE IMAGE.

      IF (ZEDGE) NEW = 1
      BJOIN = 0
      TMPHALF = 256*256

      DO I=1,JSIZE
         COIN(I) = 0
         LOWHALF = MOD(I,TMPHALF)
         IF(LOWHALF.GE.TMPHALF/2) LOWHALF = - (TMPHALF - LOWHALF)
         JOIN(I) = LOWHALF
      END DO

      NLP1 = NLINE+1
      NSP1 = NSU+1


      DO 100 IX=1,NLP1

         DO I=1,NSU
             PREV(I+5) = LINE(I+5)
         END DO     


         REC2 = REC2+1                      ! get read line pointer

         ! RECORDS ARE STARTED IN POSITION 6 OF ARRAYS THROUGHOUT THE PROGRAM
         ! (BY CONVENTION) BECAUSE SPIRAL SEARCH ALGORITHM NEEDS TO BE ABLE
         ! TO LOOK FIVE POSITIONS TO THE LEFT AND TO THE RIGHT. THE ARRAYS
         ! HAVE SPACE FOR AT LEAST FIVE POSITONS FOLLOWING THE RECORD TOO.

         IF (IX.NE.NLP1) THEN
           call xvread(infile,line(6),status,
     -                 'LINE',rec2,'SAMP',ssamp,'NSAMPS',nsamp,' ')
           if (status.ne.1) call mabend
     -                      ('Read error. Program terminated.')
         ELSE
           DO I=1,NSU
              LINE(I+5) = BORDX2
           END DO
         END IF


         NEWCRY = .TRUE.
         CNTR = 0

         DO 10 I=1,NSP1

           ! LINE(I+5) IS IN SIGNED FORMAT UNLESS IX = NLP1 OR I = NSP1.

            IVAL = LINE(I+5)
            IF(IX.EQ.NLP1.OR.I.EQ.NSP1) IVAL = FULLWORD(LINE(I+5))
            IF ( IVAL .LT. BORDER) GO TO 71    ! IS LINE(I+5) A BORDER POINT?
            LINE(I+5) = BORDVAL2
            DCNTR = I-CNTR-1
            CNTR = I
            IF (DCNTR.LE.0) GO TO 10

C   COIN(LFT) IS A COUNTER OF THE NUMBER OF POINTS THAT HAVE BEEN
C   ASSIGNED THE PRELIMINARY REGION NUMBER LFT SO FAR. THIS IS 
C   INCREMENTED WHEN A BORDER POINT IS REACHED WITH AT LEAST ONE
C   NON-BORDER POINT TO THE LEFT OF IT.

            LFT = FULLWORD(LINE(I+4))
            LIN4 = MIN0(MAXINT_PAR,DCNTR+FULLWORD(COIN(LFT)))
            LIN2 = MOD(LIN4,256**2)
            IF(LIN2.GE.(256**2)/2) LIN2 = - (256**2 - LIN2)
            COIN(LFT) = LIN2                       ! ADD DCNTR TO COIN(LFT)
            GO TO 10

 71         LFT = FULLWORD(LINE(I+4))
            ABV = FULLWORD(PREV(I+5))
            IF (ABV.NE.LFT) GO TO 6
            IF (ABV.GE.BORDER_PAR) GO TO 5
            ABV2 = MOD(ABV,256**2)
            IF(ABV2.GE.(256**2)/2) ABV2 = - (256**2 - ABV2)
            LINE(I+5) = ABV2
            GO TO 10

 5          NEW = NEW+1
            IF (NEW.GT.PRGNMX) CALL MABEND('TOO MANY REGIONS') ! exit here
            NEWCRY = .FALSE.
            NEW2 = MOD(NEW,256**2)
            IF(NEW2.GE.(256**2)/2) NEW2 = - (256**2 - NEW2)
            LINE(I+5) = NEW2
            GO TO 10

 6          IF (ABV.LT.BORDER_PAR) GO TO 7
            LFT2 = MOD(LFT,256**2)
            IF(LFT2.GE.(256**2)/2) LFT2 = - (256**2 - LFT2)
            LINE(I+5) = LFT2
            GO TO 10

 7          IF (LFT.LT.BORDER_PAR) GO TO 8
            ABV2 = MOD(ABV,256**2)
            IF(ABV2.GE.(256**2)/2) ABV2 = - (256**2 - ABV2)
            LINE(I+5) = ABV2
            GO TO 10

 8          IF (LFT.NE.NEW) GO TO 11
            IF (NEWCRY) GO TO 11
            ERASE = I+5

 9          ABV2 = MOD(ABV,256**2)
            IF(ABV2.GE.(256**2)/2) ABV2 = - (256**2 - ABV2)
            LINE(ERASE) = ABV2
            ERASE = ERASE-1
            IF ( FULLWORD(LINE(ERASE)) .EQ.NEW) GO TO 9
            NEW = NEW-1
            NEWCRY = .TRUE.
            GO TO 10

 11         LFT2 = MOD(LFT,256**2)
            IF(LFT2.GE.(256**2)/2) LFT2 = - (256**2 - LFT2)
            LINE(I+5) = LFT2
            P = LFT

 12         P = FULLWORD(JOIN(P))
            IF (P.EQ.ABV) GO TO 10
            IF ( FULLWORD(JOIN(P)) .NE.LFT) GO TO 12
            JOIN(P) = JOIN(ABV)
            LFT2 = MOD(LFT,256**2)
            IF(LFT2.GE.(256**2)/2) LFT2 = - (256**2 - LFT2)
            LINE(I+5) = LFT2
            JOIN(ABV) = LFT2
 10      CONTINUE


         LINE(NSP1+5) = BORDX2     ! RESET DN OF THE POINT TO THE RIGHT 
                                      ! OF THE IMAGE LINE.


         REC3 = REC3+1                ! update output line pointer
         IF (IX.LT.NLP1) THEN         ! write line to output
             call xvwrit(wrkfile,line(6),status,
     -                   'LINE',rec3,'NSAMPS',nsamp,' ')
             if (status .ne. 1) then
                call mabend ('Write error. Program terminated.')
             end if
         end if

 100  CONTINUE


C
C  PROCESS THE JOIN LIST TO EQUIVALENCE EACH CONNECTED COMPONENT TO THE
C  SMALLEST AVAILABLE COMPONENT NUMBER.  SMALL REGIONS ARE DISCOUNTED HE
C

      IF  ( DEBUG ) CALL PRNT(2,800,JOIN,'JOIN.')

      NRG = 0
      IF (ZEDGE) NRG = -1

      DO 20 IX=1,NEW
          JOINF = FULLWORD( JOIN(IX) )
          IF (JOINF.LT.IX) GO TO 20
          IF (JOINF.EQ.BORDER_PAR) GO TO 20

          NRG = NRG+1
          P = IX
          PT = 0
          DCNTR = 0

 21       PT = PT+1
          P2 = MOD(P,256**2)
          IF(P2.GE.(256**2)/2) P2 = - (256**2 - P2)
          PREV(PT) = P2
          DCNTR = DCNTR+ FULLWORD(COIN(P))
          Q = FULLWORD(JOIN(P))
          LIN4 = NRG
          LIN2 = MOD(LIN4,256**2)
          IF(LIN2.GE.(256**2)/2) LIN2 = - (256**2 - LIN2)
          JOIN(P) = LIN2             ! MOVE NRG TO JOIN(P).
          P = Q
          IF (P.NE.IX) GO TO 21

          IF ( DEBUG )   CALL PRNT(2,PT,PREV,'PREV.')

          IF (DCNTR.GE.THRESH) GO TO 20

          NRG = NRG-1
          DO I=1,PT
              P = FULLWORD(PREV(I))
              JOIN(P) = BORDVAL2
          END DO
 20   CONTINUE


      IF (NRG.GT.RGNMX) THEN                ! exit here
          CALL MABEND('TOO MANY REGIONS')   ! ABNORMAL END.
      END IF

C
C     IF PBORDER IS NOT USED, BORDERS IN OUTPUT IMAGE WILL BE 255 FOR
C     BYTE IMAGES,  32767 FOR HALFWORD IMAGES WITH < 32767 REGIONS,
C     AND 65535 (= -1 IN SIGNED FORMAT) FOR HALFWORD IMAGES WITH 
C     AT LEAST 32767 REGIONS.
C     FOR BYTE IMAGES, ONLY THE LOW-ORDER BYTE OF BORDOUT IS SIGNIFICANT.

      BORDOUT = MAXINT_PAR
      IF ( NRG .GE. MAXINT_PAR ) BORDOUT = BORDVAL2

C
C     COPY THE WORKING DATA SET TO OUTPUT DATA SET USING JOIN AS A TRANSFOR
C     BORDER PIXELS MAY BE ERASED HERE BY ASSIGNING TO A NEIGHBOR REGION AT
C

      BORDER = BORDER_PAR


 
      call xvclose(wrkfile,status,' ')  ! close workfile
      if ( status.ne.1 ) then       ! teminate on error
         call mabend('Error in workfile close. Program terminated.')
      end if
      call xvopen(wrkfile,status,'OP','READ',   ! reopen for read
     -           'I_FORMAT','HALF','U_FORMAT','HALF',
     -           'OPEN_ACT','SA',' ')




      rec1 = 0         ! initialize start of output - line no.


      DO I=1,NSU+10
          PREV(I) = BORDVAL2
          LINE(I) = BORDVAL2
          NEXT(I) = BORDVAL2
      END DO


      REC3 = 1                                  ! read from work file
      call xvread(wrkfile,line(6),status,'LINE',rec3,
     -            'NSAMPS',nsamp,' ')
      if (status .ne. 1) then
         call mabend('Workfile read error. Program terminated.')
      end if


      DO IX=1,NLINE                         ! loop through no. of lines

          IF (IX.EQ.NLINE) THEN
             DO I=1,NSU
                NEXT(I+5) = BORDVAL2
             END DO
          ELSE
              REC3 = REC3+1
              call xvread(wrkfile,next(6),status,'LINE',rec3,
     -            'NSAMPS',nsamp,' ')
              if (status .ne. 1) then
                 call xvmessage('Workfile read error.',' ')
                 call mabend('Program terminated.')
              end if
          END IF

          DO 35 I=1,NSU
             K = FULLWORD(LINE(I+5))
             IF (K.GE.BORDER) GO TO 38
             KK = FULLWORD(JOIN(K))
             IF (KK.LT.BORDER) GO TO 34
 38          IF (PBORD) GO TO 37
             NWLINE(I+5) = BORDOUT
             GO TO 35
 37          MYRAND = MOD(MYRAND*1061+3251,100000)
             LL = (MYRAND/50000)*32+1
             LU = LL+31

             ! SEARCH FOR A NEIGHBORING NON-BORDER POINT.

             ! MYRAND/50000 = 0 OR 1 WITH EQUAL PROBABILITY.  MYRAND/50000 
             ! DETERMINES WHICH OF TWO POSSIBLE SPIRAL SEARCH PATHS ARE TAKEN.
             ! ONE SEARCH PATH STARTS AT L=1, THE OTHER AT L=33. (THE TWO SPIRALS
             ! APPARENTLY ARE THE SAME AFTER THE EIGHTH POINT, BUT THIS SEEMS TO
             ! HAVE WORKED OUT.)

             DO 39 L=LL,LU
                LX = SEQ(L)+I+5
                IF ( ASEQ(L) .EQ. -1 )  THEN  !GET APPROPRIATE LINE OF IMAGE
                     K = FULLWORD(PREV(LX))
                ELSE IF  ( ASEQ(L) .EQ.  0 )  THEN 
                     K = FULLWORD(LINE(LX))
                ELSE
                     K = FULLWORD(NEXT(LX))
                END IF

                IF ( K .GE.BORDER) GO TO 39
                KK = FULLWORD(JOIN(K))
                IF (KK.LT.BORDER) GO TO 34
 39          CONTINUE
             KK = 0        ! NO NEIGHBORING NON-BORDER POINT FOUND.
 34          KK2 = MOD(KK,256**2)
             IF(KK2.GE.(256**2)/2) KK2 = - (256**2 - KK2)
             NWLINE(I+5) = KK2
 35       CONTINUE

 
          REC1 = REC1+1
          call xvwrit(outfile,nwline(6),status,
     -               'LINE',rec1,'NSAMPS',nsamp,' ')
          if (status .ne. 1)  then
             call xvmessage('Output file write error.',' ')
             call mabend('Program terminated.')
          end if
  
          DO I=1,NSU
             PREV(I+5) = LINE(I+5)
             LINE(I+5) = NEXT(I+5)
          END DO

      END DO



C     and end normally



      CALL XVCLOSE ( INFILE,  STATUS,' ' )    ! close input
      CALL XVCLOSE ( OUTFILE, STATUS,' ' )    ! close output
      CALL XVCLOSE ( WRKFILE, STATUS,' ' )    ! and work file


      WRITE (NPRGM(1:8),'(I8)') NEW
      CALL XVMESSAGE(NPRGM,' ')
      WRITE (NRGM(1:8),'(I8)') NRG
      CALL XVMESSAGE(NRGM,' ')
      RETURN        
      END


c  function to replace old inline fcn. using JZEXT ...
      integer*4 function fullword(half)
      integer*4 long
      integer*2 half
      long = half
      if (long.lt.0) long = 65536+long
      fullword = long
      return
      end

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create paint.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM paint

   To Create the build file give the command:

		$ vimake paint			(VMS)
   or
		% vimake paint			(Unix)


************************************************************************/


#define PROGRAM	paint
#define R2LIB

#define MODULE_LIST paint.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create paint.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=2
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM OFORMAT TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=(BYTE,HALF)
PARM BORDER  TYPE=INTEGER  COUNT=1       DEFAULT=255
PARM PBORDER TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=PBORDER
PARM THRESH  TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM ZEDGE   TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=ZEDGE
PARM DEBUG   TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=DEBUG
!
END-PROC
.TITLE
VICAR Program "paint"
.HELP
PURPOSE:

Program "paint" takes an image that is divided into regions and
'paints' each region of the image a different color.  The regions
of the input image are defined in terms of border (or boundary)
pixels.  A border pixel is a pixel whose data number is greater
than or equal to the value specified for the BORDER parameter.
(See under BORDER parameter.)  A region is defined as a connected
set of non-border pixels that is bounded by border pixels.  In
the 'painted' image produced by "paint", the regions are numbered
consecutively beginning with 1, and all pixels in a given region
are assigned (as their data numbers) the number of that region.
.PAGE
 EXECUTION:

Commonly, the input image will be made up of sinuous lines of 
(border) pixels against a background of pixels with zero data numbers.
The lines divide the image into polygon-shaped regions.  The program,
however, does not impose requirements upon the arrangements and the
precise values of border and non-border pixels.  Non-border pixels
may have any value less than the BORDER parameter.  Although
border pixels usually form continuous curves, isolated border
pixels are allowed. Regions may have irregular shapes.  Depending on
the ZEDGE parameter, connected sets of non-border pixels that are
bounded in part by the edges of the image may or may not be considered
to be regions.  The input image may either be byte or halfword data.
.PAGE
In the output image the regions are numbered sequentially in the order
that the program encounters them as it reads (in order) the lines
of the file, scanning them from left to right. The maximum number of 
regions the program can handle is limited by the number of different 
data number values that can be represented in the data format of the input
image ie. 254 regions for byte data and 65534 regions for halfword data.)  
Byte-data images with more than 254 regions should be run with the parameter 
'HALF specified to produce a halfword output image.  Images in halfword 
format with more than 32767 regions will have their regions numbered 
sequentially from 1 to 32767 and then sequentially increasing from -32768. 
(This is sequentially in terms of unsigned 16-bit format.)

When the PBORDER option is used, border points are assigned the
data number of a neighboring region provided one exists. (See
under PBORDER.)  If the PBORDER option is not used, border points
will be assigned a data number of 255 for byte images, 32767 for
halfword images with less than 32767 regions, and 65535 (= -1 in
signed format) for halfword images with at least 32767 regions.

"paint" uses a halfword-format work file in its algorithm for identifying
and numbering the regions. In the work file the regions are numbered in a
preliminary way.  In certain cases portions of a region may be assigned
different numbers. (The program may find that two 'preliminary'
regions are actually connected several lines beneath where the regions
are first encountered by the program.)  Since the number of
preliminary regions is also limited to 65534, this can in effect
reduce the number of regions the program can handle for halfword
images.

TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      paint INP=a OUT=(b,w) SIZE=(sl,ss,nl,ns) optional parameters
      paint INP=a OUT=(b,w) SL=sl SS=ss NL=nl NS=ns optional parameters
      paint a (b,w) (sl,ss,nl,ns) optional parameters
      paint a (b,w) optional parameters

       Here 'a' represents the input image file name,
       'b' represents the output image file name, and 
       'w' represents the work file name.
.PAGE
EXAMPLES

 1.   paint INP=P1 OUT=(P2,WRK) SIZE=(1,1,100,100) BORDER=128

      In this example a byte-data image is painted.  Since WRK
      is halfword, it has twice the number of samples.


 2.   paint A (B,WK) 'HALF BORDER=200 'ZEDGE 'PBORDER

      In this example, file A is the input, file B is the output, and
      file WK is the work file.  The output and workfiles are halfword.  
      The border consists of pixels with a value greater than or equal
      to 200. Boundary polygons are zeroed by the keyword ZEDGE, and
      the keyword PBORDER assigns boundary pixels to the nearest 
      non-border polygon.

RESTRICTIONS
1. The input and output images must be byte data.
2. Maximum number of pixels is 64000 per line.  The TAE/VICAR executive
   may impose further size limitations.
3. Maximum number of regions is 254 for byte output data and 65534 for
   halfword data.
4. Maximum number of preliminary regions is 65534.
.PAGE
 OPERATION:

"paint" is an important step in the process of image polygon overlay.  The 
"paint" program assigns region numbers sequentially to non-border pixels,
line by line, using the connectivity algorithm to the pixel above and to
the left.  This algorithm has two possible cases which will cause pixels
to be temporarily mis-assigned.  Case 1 occurs when the first
point in a line for a particular region has a border point immediately
above it and the point is part of a previously encountered region.
In this case the algorithm at first assigns a new region number to the
pixel.  Case 2 occurs when a the intersection of a region with a line is 
a set of two or more strings of pixels.  In this case the algorithm at first
might assign different region numbers to each string. 

In the first case mis-assigned regions are corrected as soon as a non-border
pixel is encountered above the pixel being processed.  In this case the program
moves backwards correcting each pixel up to the border.  In the second case
strings are connected using the table JOIN to link sections of the region
together.  This is done in a second step which picks the final value for each
region while maintaining the reference to the original value through the
table so that the values of the differrent strings in the same region are
the same.
.PAGE
The following diagrams illustrate the two cases in which the program must
correct for mis-assignments. (Border points are denoted by the letter X.)


  XXXXXXX                              XXXXXXXXXXXX
  XXX2222                              X222XXXX333X
  X333333                              X2222222222X
 
   corrected to                 Table JOIN prior to correction:
                                             JOIN(1) = 1
  XXXXXXX                                    JOIN(2) = 3
  XXX2222                                    JOIN(3) = 2
  X222222                                    JOIN(4) = 4

 Example of Case 1                     Example of Case 2


In the final step of "paint", final values are assigned for both non-border
and border pixels.  If PBORDER is specified, border pixels are assigned
the value of the closest non-border pixel.  The search is made in a spiral
from the original pixel.  The direction of the spiral is randomly determined.

 ORIGINAL PROGRAMMER:   A. Zobrist		1975

 COGNIZANT PROGRAMMER:   Barbara McGuffie       4/1986

 DOCUMENTATION WRITTEN BY:             Steve Pohorsky               
 REVISION:               1      
 REVISION:               2      AMS (CRI) Mar. 6 1995   Made portable for UNIX

.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name followed
by work file name
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE OFORMAT
The output image data 
format: either 
BYTE or HALF.
.VARIABLE BORDER
Data number value used to
define border pixels.
.VARIABLE PBORDER
Specify to have border
pixels in the output image
be assigned the same value
as a neighboring region.
.VARIABLE THRESH
Threshold number. Specifies
that all regions containing
fewer pixels than the threshold
number will be changed into
border pixels.
.VARIABLE ZEDGE
Specify if areas along the
edge of the image are not to be
counted as regions and are to
be assigned zero data numbers.
.VARIABLE DEBUG
Debug switch.
.LEVEL2
.VARIABLE FORMAT

                    OUTPUT data format.

                    VALID = BYTE or HALF.

.VARIABLE BORDER
A border pixel is a pixel whose data number is greater
than or equal to the value specified for the BORDER parameter.
The default BORDER value is 255 for byte-data images and
32767 for halfword-data images.
.VARIABLE PBORDER
This option allows borders to be painted over.  For each border pixel
a neighboring region is selected at random, and the data number for
that region is assigned to the pixel.  (The random selection is used
so that this process increases the size of regions in a statistically
balanced way.)  If there are no non-border pixels within five pixels
of the border pixel in the same line, the line above, or the line
below, a zero data number is assigned to the border pixel.
.VARIABLE THRESH
If both the THRESH and PBORD options are used, regions smaller than
the threshold size are assigned the data number for a larger neighboring
region.
.VARIABLE ZEDGE
If ZEDGE is not specified, regions may have the edge of the image for a border.
If ZEDGE is specified, regions may not have the edge of the image for a border.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpaint.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!THIS JOB TESTS IBIS PROGRAM "PAINT" BY CREATING THREE POLYSCRB-LIKE
!INPUT FILES USING "F2": P1.IMG = BYTE; P2.IMG = HALF; P3.IMG =
!HALF WITH GREATER THAN 32767 REGIONS.
!
!CREATE TEST INPUTS WITH "F2"
!
f2       OUT=P1.IMG SIZE=(1,1,27,27) FORMAT=BYTE +
         FUNC=("IN1+((MOD(LINE,3).EQ.0).OR.(MOD(SAMP,3).EQ.0))*255")
list     P1.IMG SIZE=(18,18,10,10)
f2       OUT=P2.IMG SIZE=(1,1,54,54) FORMAT=HALF +
         FUNC=("IN1+((MOD(LINE,3).EQ.0).OR.(MOD(SAMP,3).EQ.0))*255")
list     P2.IMG SIZE=(45,45,10,10) FORMAT=HALF
f2       OUT=P3.IMG SIZE=(1,1,546,546) FORMAT=HALF +
         FUNC=("IN1+((MOD(LINE,3).EQ.0).OR.(MOD(SAMP,3).EQ.0))*255")
list     P3.IMG SIZE=(537,537,10,10) FORMAT=HALF
!
!TEST PAINT
!
paint    P1.IMG (A,D) 'BYTE 'PBORDER THRESH=2
list     A SIZE=(18,18,10,10) 
paint    P2.IMG (B,D) 'HALF 'PBORDER THRESH=2
list     B SIZE=(45,45,10,10) FORMAT=HALF
paint    P3.IMG (C,D) 'HALF 'PBORDER THRESH=2
list     C SIZE=(537,537,10,10) FORMAT=HALF
paint    P3.IMG (E,D) 'HALF 'PBORDER 'ZEDGE
list     E SIZE=(537,537,10,10) FORMAT=HALF
end-proc
$ Return
$!#############################################################################
