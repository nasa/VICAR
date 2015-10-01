$!****************************************************************************
$!
$! Build proc for MIPL module fastmos
$! VPACK Version 1.9, Tuesday, October 02, 2012, 15:50:54
$!
$! Execute by entering:		$ @fastmos
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
$ write sys$output "*** module fastmos ***"
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
$ write sys$output "Invalid argument given to fastmos.com file -- ", primary
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
$   if F$SEARCH("fastmos.imake") .nes. ""
$   then
$      vimake fastmos
$      purge fastmos.bld
$   else
$      if F$SEARCH("fastmos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fastmos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fastmos.bld "STD"
$   else
$      @fastmos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fastmos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fastmos.com -mixed -
	-s fastmos.f -
	-i fastmos.imake -
	-p fastmos.pdf -
	-t tstfastmos.pdf tstfastmos.log_solos tstfastmos_integer.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fastmos.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  FASTMOS
C#######################################################################
C      Program FASTMOS takes input images and mosaics them together to form
C      an output image.

c  31aug2012 -lwk- converted FASTMOS to work on real*4 data
c  22sep2012 -lwk- made THRESH parameters consistent with old FASTMOS;
c                  because the default threshold is now 0.0 (used to be 1),
c                  the tests are now for DN>THRESH instead of DN>=THRESH.

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 

      EXTERNAL MOSAICIT
 
      COMMON /C1/ PAR, THRESH, IB, MTYP, NSEQ, LNIB, RNIB,
     &            LTHR, RTHR, NIBINC, OFF, ISIZE
      real*4 thresh, lthr, rthr, rpar(5)
      LOGICAL ABORTFLAG
      INTEGER*4 ISB,NB
      INTEGER   PAR(100), LNIB, RNIB, OFF(4,48)

      CHARACTER*8 FMT
      LOGICAL XVPTST
      CHARACTER*5 OFFC(48) 
      CHARACTER*3  ORGIN
      DATA OFFC/ 
     .           'OFF1',  'OFF2', 'OFF3', 'OFF4', 'OFF5', 
     .           'OFF6',  'OFF7', 'OFF8', 'OFF9', 'OFF10', 
     .           'OFF11', 'OFF12', 'OFF13', 'OFF14', 'OFF15', 
     .           'OFF16', 'OFF17', 'OFF18', 'OFF19', 'OFF20', 
     .           'OFF21', 'OFF22', 'OFF23', 'OFF24', 'OFF25', 
     .           'OFF26', 'OFF27', 'OFF28', 'OFF29', 'OFF30', 
     .           'OFF31', 'OFF32', 'OFF33', 'OFF34', 'OFF35', 
     .           'OFF36', 'OFF37', 'OFF38', 'OFF39', 'OFF40', 
     .           'OFF41', 'OFF42', 'OFF43', 'OFF44', 'OFF45', 
     .           'OFF46', 'OFF47', 'OFF48'                   /

C   INITIALIZE VARIABLES

      CALL XVMESSAGE(' FASTMOS version 02-Oct-2012', ' ') 

      IB=0
      MTYP=0
      THRESH=0.0
      LTHR=0.0
      RTHR=0.0
      NSEQ=8
      LNIB=4
      RNIB=LNIB
      NIBINC=1
      CALL MVE(4,4*48,0,OFF,0,1)      ! USE MVE FOR ZEROING ARRAYS.

C  OPEN INPUT FILE

      CALL XVUNIT( INFILE, 'INP', 1, IND ,' ')
      CALL XVOPEN( INFILE, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INFILE,IND,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')     

c      ISL: start line, ISSAMP: start sample, NL: no. lines, NPIXEL: no. samples
      CALL XVSIZE( ISL, ISSAMP, NL, NPIXEL,idum1,idum2 )  ! SIZE PARAMETERS.
      CALL XVBANDS( ISB, NB, NBI)

      IF ( ISB .GT. NBI ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( ISB + NB - 1 .GT. NBI) THEN
         CALL XVMESSAGE('***Number of bands truncated', ' ') 
         NB = NBI + 1 - ISB
      ENDIF

      CALL XVGET( INFILE, IND, 'FORMAT', FMT, ' ') 

      CALL XVPCNT( 'INP', NI )
      CALL XVCLOSE( INFILE, IND ,' ')
 
C  NOW LOOK AT PARAMETERS ENTERED BY USER.

      IF ( XVPTST('EDGE') )   IB = 1

      CALL XVPARM( 'THRESH', THRESH, IVALS, IDEF,1 )

      CALL XVPARM( 'NSEQ', NSEQ, IVALS, IDEF ,1)

      CALL XVPARM( 'NIBBLE', PAR, IVALS, IDEF,1 )
          LNIB = PAR(1)
          RNIB = LNIB

      CALL XVPARM( 'LNIBBLE', PAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          LNIB = PAR(1)
      END IF

      CALL XVPARM( 'RNIBBLE', PAR, IVALS, IDEF ,1)
      IF (IVALS .GT. 0)   THEN
          RNIB = PAR(1)
      END IF

      CALL XVPARM( 'NTHRESH', rPAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          THR = rPAR(1)
      else
          thr = thresh
      END IF

      CALL XVPARM( 'LTHRESH', rPAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          LTHR = rPAR(1)
      else
          lthr = thr
      END IF

      CALL XVPARM( 'RTHRESH', rPAR, IVALS, IDEF,1 )
      IF (IVALS .GT. 0)   THEN
          RTHR = rPAR(1)
      else
          rthr = thr
      END IF

      IF ( XVPTST('OVERLAY') )     MTYP = 0
      IF ( XVPTST('AVERAGE') )     MTYP = 1
      IF ( XVPTST('MOD' ) )        MTYP = 2
      IF ( XVPTST('MAX' ) )        MTYP = 3
      IF ( XVPTST('MIN' ) )        MTYP = 4

      CALL XVPARM( 'NINCR', NIBINC, IVALS, IDEF,1 )

      DO IDSRN = 1, NI                  ! GET OFFSET PARAMETERS.
         CALL XVPARM( OFFC(IDSRN), PAR, IVALS, IDEF,4 )
             IF ( IVALS .NE. 2  .AND.  IVALS .NE. 4 )  GOTO 6100

             OFF(1,IDSRN) = 1 - PAR(1)
             OFF(2,IDSRN) = 1 - PAR(2)

             IF ( IVALS .EQ. 4 )  THEN
                 OFF(3,IDSRN) = PAR(3)
                 OFF(4,IDSRN) = PAR(4)
             END IF
      END DO

C  CALL SPECIAL LIBRARY SUBROUTINE STACKA TO ALLOCATE THE NECESSARY BUFFER
C  AND TO CALL SUBROUTINE MOSAICIT. (WE ALLOCATE NI BUFFERS TOGETHER IN A
C  TWO-DIMENSIONAL ARRAY.)

      ISIZE = 4*NPIXEL*NI 
      CALL STACKA( 9, MOSAICIT, 1, ISIZE, NL, NPIXEL, NB, NI,     
     &             ABORTFLAG, ITERMCODE)

                           ! NORMAL RETURN POINT FROM STACKA AFTER
                           ! EXECUTING SUBROUTINE MOSAICIT.

      IF (.NOT. ABORTFLAG)  GOTO 8000    ! IF MOSAICIT SUCCESSFUL, ALL DONE.


      IF (ITERMCODE .EQ. 1) 
     &    CALL XVMESSAGE('INSUFFICIENT MEMORY OBTAINED.',' ')
      GOTO 7000            ! CALL ABEND.
                           
6100  CALL XVMESSAGE('PARAMETER ERROR IN OFFSET PARAM',' ')

7000  CALL ABEND      ! ABNORMAL END. (NO RETURN FROM ABEND.)

8000  RETURN          ! NORMAL END.
      END


      SUBROUTINE MOSAICIT(INBUF,INL,NL,NPIXEL,NB,NI,
     &                    ABORTFLAG,ITERMCODE)
C#######################################################################
C  PURPOSE
C      MOSAICIT TAKES INPUT IMAGES AND MOSAICS THEM TOGETHER TO FORM AN OUTPUT
C      IMAGE.
C      
C      INBUF(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THE LINE BUFFERS ARE PACKED END TO 
C                          END IN THIS ARRAY.  THE INDEX OF THE START OF THE
C                          LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      INL               - NUMBER OF BYTES ALLOCATED BY STACKA FOR INBUF.
C      NPIXEL            - NUMBER OF PIXELS IN A LINE OF THE OUTPUT IMAGE.
C      NI                - NUMBER OF INPUT IMAGES.
C  OUTPUT PARAMETERS
C      ABORTFLAG - .TRUE. IF MOSAICIT FAILED. .FALSE. IF SUCCESSFUL. ABORTFLAG
C                  IS A LOGICAL   VARIABLE.
C      ITERMCODE - ERROR MESSAGE CODE IF MOSAICIT FAILS.
C  CALLED BY
C      STACKA
C  SUBROUTINES CALLED 
C      EDGE, MOS, FASTFILL 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      COMMON /C1/ PAR, THRESH, IB, MTYP, NSEQ, LNIB, RNIB,
     &            LTHR, RTHR, NIBINC, OFF, ISIZE
      LOGICAL ABORTFLAG
      INTEGER   PAR(100), LNIB, RNIB, OFF(4,48)
      INTEGER   OFFBACK(4,48)
      INTEGER*4 NL,NPIXEL,NB,BAND
      real*4 INBUF(ni*npixel), l0, thresh, lthr, rthr
      INTEGER   IPOS(48), IPIX(48), ISKIP(48), OUTFILE, INP(48), IS(48)
      LOGICAL XVPTST
      LOGICAL PROGRESS_MSG
      CHARACTER*40 PMESSAGE
      CHARACTER*3 ORGIN
      CHARACTER*8 FMT
      INTEGER*4 LINEOUT,BANDOUT
C
C=================START OF EXECUTABLE CODE===============================     

      ABORTFLAG = .FALSE.               ! INITIALIZE TO SUCCESSFUL-SO-FAR.

C....CHECK IF STACKA GOT ENOUGH MEMORY. 

      IF ( INL .LT. ISIZE )  THEN
         ABORTFLAG = .TRUE.             ! INDICATE SUBROUTINE FAILURE
         ITERMCODE  = 1                 ! AND REASON FOR FAILURE.
         GOTO 8000                      ! EXIT SUBROUTINE.
      END IF

C   OPEN INPUT FILES AND GET SIZE VALUES IF NOT ENTERED BY USER.

      DO  IDSRN = 1, NI

        CALL XVUNIT( INP(IDSRN), 'INP', IDSRN, IND ,' ')
        CALL XVOPEN( INP(IDSRN), IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .   'IO_ACT', 'SA', 'U_FORMAT', 'REAL', ' ')
c       Check organization of image, prohibit BIP
        CALL XVGET( INP(IDSRN), IND, 'ORG', ORGIN, 'FORMAT', FMT, ' ')
        IF (ORGIN.EQ.'BIP') CALL MABEND(
     +    'BIP files not supported, use program TRAN to convert to BSQ')

        IF ( OFF(3,IDSRN) .EQ. 0 )  THEN
            CALL XVGET(  INP(IDSRN), IND, 'NL', OFF(3,IDSRN),
     .                   'NS',  OFF(4,IDSRN) ,' ')
        END IF

      END DO

C  OPEN OUTPUT FILE.

      CALL XVUNIT( OUTFILE, 'OUT', 1, IND ,' ')
      CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'U_NL', NL,'U_NS', 
     .             NPIXEL,'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',' ')

C   SET UP FOR MOSAICING LOOP.

      ! SET UP LOGICAL ZERO VALUE THAT MAKES SENSE 
      if (fmt.eq.'REAL') then
        l0 = -1.0e10
      else
        ! (this code matches the previous integer-only FASTMOS)
        ithresh = thresh
        i = MIN ( 0, ITHRESH )
        i = MAX ( i, -32768)
        if  (ITHRESH .EQ. 0 .AND. fmt.eq.'BYTE')  i = 0
        l0 = i
      endif

      CALL FASTFILL(NPIXEL*NI,L0,INBUF) !ZERO ALL INPUT BUFFS
      DO IDSRN = 1, NI
         OFF(3,IDSRN) = OFF(3,IDSRN) - MAX( 0, OFF(1,IDSRN) )

         ISKIP(IDSRN)  = MAX( 0, OFF(2,IDSRN) )
         IPIX(IDSRN)   = MIN( OFF(4,IDSRN)-MAX( OFF(2,IDSRN), 0 ),
     &                            NPIXEL+MIN( OFF(2,IDSRN), 0 )    )
         IF ( IPIX(IDSRN) .LE. 0 )   IPIX(IDSRN) = 0
         IS(IDSRN) = 1 + (IDSRN-1)*NPIXEL   !START OF LINE BUFFER.
         IPOS(IDSRN) = IS(IDSRN) - MIN(0,OFF(2,IDSRN))

      END DO

          NLTENTH = NINT( NL/10.)      ! SET UP FOR PROGRESS PARAMETER
          II = 0
          PROGRESS_MSG = XVPTST('PROGRESS')

C  COPY OFF() to OFFBACK() for multiband reuse

      DO IDSRN = 1,NI
         OFFBACK(1,IDSRN) = OFF(1,IDSRN)
         OFFBACK(2,IDSRN) = OFF(2,IDSRN)
         OFFBACK(3,IDSRN) = OFF(3,IDSRN)
         OFFBACK(4,IDSRN) = OFF(4,IDSRN)
      END DO


C  MOSAICING LOOP:  MOSAIC LINE BY LINE.
      BANDOUT=0
      DO 3000 BAND=1,NB
         BANDOUT = BANDOUT + 1
         LINEOUT = 0

c      RESET OFF() from BACK UP OFFBACK()
       DO IDSRNX = 1,NI
         OFF(1,IDSRNX) = OFFBACK(1,IDSRNX)
         OFF(2,IDSRNX) = OFFBACK(2,IDSRNX)
         OFF(3,IDSRNX) = OFFBACK(3,IDSRNX)
         OFF(4,IDSRNX) = OFFBACK(4,IDSRNX)
       END DO

      DO 1000 I=1,NL
         LINEOUT = LINEOUT + 1
         DO IDSRN = 1, NI

           IF( OFF(3,IDSRN) .EQ. 0  .OR. IDSRN .EQ. 1 )  THEN
               CALL FASTFILL(NPIXEL, L0, INBUF(IS(IDSRN))) 
           END IF

           IF( OFF(1,IDSRN) .GE. 0 .AND. 
     &         OFF(3,IDSRN) .GT. 0 .AND. 
     &         IPIX(IDSRN) .GT. 0        )          THEN

               IREC   = 1 + OFF(1,IDSRN)
               ISSAMP = ISKIP(IDSRN) + 1
               INPIX  = IPIX(IDSRN)
               INPOS  = IPOS(IDSRN)
               CALL XVREAD( INP(IDSRN), INBUF(INPOS), IND,
     &                'LINE',IREC,'SAMP',ISSAMP,'NSAMPS',INPIX,
     &                'BAND', BAND, ' ')
           END IF

           IF ( OFF(1,IDSRN) .GE. 0 )  OFF(3,IDSRN)=OFF(3,IDSRN)-1
           OFF(1,IDSRN) = OFF(1,IDSRN)+1
         END DO

         IF(IB .NE. 0)  CALL EDGE(INBUF,NPIXEL,NI,IPIX,IPOS,
     &                            NSEQ,LNIB,RNIB,LTHR,RTHR,NIBINC,L0)

         CALL MOS( INBUF, NPIXEL, NI, THRESH, MTYP)
         CALL XVWRIT(OUTFILE, INBUF(1), IND,
     +        'LINE',LINEOUT,'BAND',BANDOUT, ' ')

            II = II + 1
            IF (PROGRESS_MSG .AND. II .GE. NLTENTH) THEN
               II = 0                     !PRINT PROGRESS IF DESIRED.
               NTENTHS = NINT(I * 10. / NL)
               IF (NTENTHS .GT. 0 .AND. NTENTHS .LE. 9) THEN
                  WRITE (PMESSAGE, 9100) NTENTHS
                  CALL XVMESSAGE( PMESSAGE, ' ')
               END IF
            END IF
1000  CONTINUE            ! END OF MOSAICING LOOP.
3000  CONTINUE
C  CLOSE ALL FILES AND RETURN TO MAIN44 VIA STACKA.

7000  CONTINUE
      DO IDSRN = 1, NI
         CALL XVCLOSE(INP(IDSRN),IND,' ')
      END DO
         CALL XVCLOSE(OUTFILE, IND,' ')
8000  RETURN

9100  FORMAT( '   fastmos ',I1,'0% done')
      END


      SUBROUTINE EDGE(INBUF, NPIXEL,NI,IPIX,IPOS,
     &                NSEQ,LNIB,RNIB,LTHR,RTHR,NIBINC, L0)
C#######################################################################
C  PURPOSE
C      EDGE removes the edges of lines from the input files.
C      
C  INPUT PARAMETERS  
C      INBUF(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THE LINE BUFFERS ARE PACKED END TO 
C                          END IN THIS ARRAY.  THE INDEX OF THE START OF THE
C                          LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      NPIXEL            - NUMBER OF PIXELS IN THE LINE.
C      NI                - NUMBER OF INPUT IMAGES.
C      IPIX              - ARRAY CONTAINING NUMBER OF PIXELS READ FOR LINE 
C                          FROM EACH INPUT FILE.
C      IPOS              - DATA READ FOR LINES STARTS AT 
C                          INBUF( IPOS(IDSRN) ) FOR EACH IDSRN.
C      NSEQ,LNIB,RNIB,LTHR,RTHR,NIBINC - PARAMETERS PASSED TO NIBLR FOR EDGING
C      L0                - LOGICAL ZERO VALUE TO ZERO BUFFERS WITH.
C  OUTPUT PARAMETERS
C      PIXELS REMOVED FROM LINES HAVE THEIR DATA NUMBERS ZEROED IN INBUF.
C  CALLED BY
C      MOSAICIT
C  SUBROUTINES CALLED 
C      NIBLR and FASTFILL.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      real*4 INBUF(*)
      INTEGER   IPOS(48), IPIX(48)
      INTEGER LNIB,RNIB
      real*4 LTHR,RTHR,L0
C
C=================START OF EXECUTABLE CODE===============================     

      DO  IDSRN = 1, NI                     ! FOR EACH FILE, EDGE THE LINE
        IF ( IPIX(IDSRN) .GT. 0 )  THEN       ! DATA IF PRESENT.

         CALL NIBLR(IPIX(IDSRN),INBUF(IPOS(IDSRN)), LTHR, NSEQ,
     &    0, IANSW, NIBINC )         ! EDGE ON LEFT.

C             IF EDGE FOUND, THEN NIBBLE OFF LNIB PIXELS PAST THE EDGE.
C             IF EDGE NOT FOUND, THIS MEANS NO DATA VALUES WERE FOUND THAT
C             WERE NOT BELOW THE THRESHOLD; SO ZERO THE WHOLE LINE.
C             SIMILARLY FOR NIBBLING FROM THE RIGHT. FASTFILL USED FOR ZEROING.
 
           IF ( IANSW .GT. 0 )  THEN                         
              NIBLGTH = MIN(IANSW+LNIB-1,IPIX(IDSRN))        ! NUMBER OF PIXELS
              IF ( NIBLGTH .GE. 1 )   CALL FASTFILL(          ! TO NIBBLE. 
     &          NIBLGTH,L0,INBUF(IPOS(IDSRN)))

              CALL NIBLR( IPIX(IDSRN), INBUF(IPOS(IDSRN)),
     &        RTHR, NSEQ, 1, IANSWR,NIBINC)

              IF ( IANSWR .GT. 0 )  THEN
               NIBLGTH = MIN( IPIX(IDSRN)-IANSWR+RNIB, IPIX(IDSRN) )
               NIBSTRT = IPOS(IDSRN) + IANSWR - RNIB
               IF ( NIBLGTH .GE. 1 )  CALL FASTFILL(NIBLGTH,
     &              L0, INBUF(NIBSTRT) )

              ELSE
               CALL FASTFILL(IPIX(IDSRN),L0,INBUF(IPOS(IDSRN)))
              END IF

           ELSE
             CALL FASTFILL(IPIX(IDSRN),L0,INBUF(IPOS(IDSRN)))

           END IF
        END IF
      END DO
      RETURN
      END


      SUBROUTINE MOS(INBUF,NPIXEL,NI, THRESH, MTYPE)
C#######################################################################
C  PURPOSE
C      MOS PRODUCES A LINE OF THE OUTPUT IMAGE FROM THE CORRESPONDING
C      LINES OF THE INPUT IMAGES.
C
C  CALLING SEQUENCE 
C     CALL MOS( INBUF, NPIXEL, NI, THRESH, MTYPE )
C  INPUT PARAMETERS ( all parameters are INTEGER   except as otherwise noted )
C      INBUF(K)         - LINE BUFFERS FOR EACH OF THE INPUT IMAGES, IDSRN =
C       array              1 TO NI.  THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          THIS SUBROUTINE ARGUMENT IS USED WHEN THE
C                          THE DATA FORMAT IS BYTE. THE LINE BUFFERS ARE PACKED
C                          END TO END IN THIS ARRAY.  THE INDEX OF THE START 
C                          OF THE LINE BUFFER FOR IMAGE IDSRN IS 
C                          1+ (IDSRN-1)*NPIXEL.
C                          IN CASES WHERE THE BOUNDARIES OF AN IMAGE DO NOT
C                          MATCH THE BOUNDARIES OF THE OUTPUT IMAGE, 
C                          MOSAICIT ELIMINATES PARTS OF THAT IMAGE THAT
C                          DO NOT LIE IN THE OUTPUT IMAGE BOUNDARIES AND PUTS
C                          ZERO DATA NUMBERS IN PARTS OF THE LINE BUFFER WHICH
C                          ARE OUTSIDE THE BOUNDARIES OF THAT INPUT IMAGE.
C      NPIXEL            - NUMBER OF PIXELS IN THE LINE.
C      NI                - NUMBER OF INPUT IMAGES.
C      THRESH           - DATA NUMBER THRESHOLD. DATA NUMBERS BELOW THE
C                          THRESHOLD ARE IGNORED FOR THE MOST PART.
C      MTYPE             - INDICATES THE THE MOSAICING METHOD BY WHICH THE 
C                          OUTPUT DATA NUMBERS ARE DETERMINED.
C                           = 0 FOR OVERLAY MODE
C                           = 1 FOR AVERAGE MODE
C                           = 2 FOR MOD MODE
C                           = 3 FOR MAXIMUM MODE
C                           = 4 FOR MINIMUM MODE
C  OUTPUT PARAMETERS
C      THE OUTPUT IMAGE LINE IS RETURNED IN  INBUF(I,1) FOR I = 1 TO NPIXEL.
C      IF MTYPE IS INVALID, THEN THIS IS THE SAME AS IT WAS IN INPUT.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

      real*4 INSAVE(48), INBUF(*)
      INTEGER ITEMP, IS(48)
      real*4 NCLOSE, NMIN, NMAX, NTOTAL

      IF (MTYPE .NE. 0)  THEN
        IS(1) = 1
        DO I = 2, NI
           IS(I) = IS(I-1) + NPIXEL
        END DO
      END IF

      IF (MTYPE .EQ. 0)  THEN            ! OVERLAY MODE.

         IS(1) = 0
         DO I = 2, NI
            IS(I) = IS(I-1) + NPIXEL
         END DO

         DO 1000 I = 1, NPIXEL

           IF ( INBUF(I) .GT. THRESH )  GOTO 1000
           DO IDSRN = 2, NI      ! FIND FIRST ELEMENT >= THRESH.

             IF ( INBUF(I+IS(IDSRN)) .GT. THRESH )  THEN
                INBUF(I) = INBUF(I+IS(IDSRN))
                GOTO 1000 
             END IF

           END DO
1000     CONTINUE


      ELSE IF (MTYPE .EQ. 1)  THEN            ! AVERAGE MODE.

         DO I = 1, NPIXEL
           NTOTAL = 0
           NCOUNT = 0

           DO IDSRN = 1, NI     ! AVERAGE ALL VALUES >= THRESH.

             IF ( INBUF(IS(IDSRN)) .GT. THRESH )  THEN
                NTOTAL = NTOTAL + INBUF(IS(IDSRN))
                NCOUNT = NCOUNT + 1
             END IF
             IS(IDSRN) = IS(IDSRN)+1
           END DO

           IF      (NCOUNT .EQ. 1)  THEN
                    INBUF(I) = NTOTAL

           ELSE IF (NCOUNT .GT. 1) THEN
                    INBUF(I) = NTOTAL/NCOUNT

           END IF

         END DO

      ELSE IF (MTYPE .EQ. 2)  THEN            ! MOD MODE.

         DO I = 1, NPIXEL
           NTOTAL = 0
           NCOUNT = 0

           DO IDSRN = 1, NI     ! AVERAGE ALL VALUES >= THRESH.

             IF ( INBUF(IS(IDSRN)) .GT. THRESH )  THEN
                NTOTAL = NTOTAL + INBUF(IS(IDSRN))
                NCOUNT = NCOUNT + 1
                INSAVE(NCOUNT) = INBUF(IS(IDSRN))
             END IF
             IS(IDSRN) = IS(IDSRN)+1

           END DO

           IF      (NCOUNT .EQ. 1)  THEN
                    INBUF(I) = NTOTAL

           ELSE IF (NCOUNT .EQ. 2) THEN
                    INBUF(I) = NTOTAL/2      ! USE AVERAGE IF 2 VALUES.

           ELSE IF (NCOUNT .GT. 2) THEN         ! IF >2 VALUES, USE CLOSEST
                    NAVE = NTOTAL/NCOUNT        ! VALUE TO AVERAGE.
                    NCLOSE = INSAVE(1)
                    MINDIFF  = ABS(NAVE-INSAVE(1))

                    DO J = 2, NCOUNT
                       JDIFF = ABS( NAVE-INSAVE(J) )
                       IF (JDIFF .LT. MINDIFF)  THEN
                           MINDIFF = JDIFF
                           NCLOSE  = INSAVE(J)
                       END IF
                    END DO

                    INBUF(I) = NCLOSE 

           END IF

         END DO

      ELSE IF (MTYPE .EQ. 3)  THEN            ! MAX MODE.

         DO I = 1, NPIXEL
           NCOUNT = 0

           DO IDSRN = 1, NI   ! FIND MAX VALUE >= THRESH.

             IF ( INBUF(IS(IDSRN)) .GT. THRESH )  THEN
                IF (NCOUNT .EQ. 0)  THEN
                  NMAX = INBUF(IS(IDSRN))
                  NCOUNT = 1
                ELSE 
                  NMAX = MAX(  NMAX, INBUF(IS(IDSRN))  )
                END IF
             END IF
             IS(IDSRN) = IS(IDSRN)+1

           END DO

           IF      (NCOUNT .EQ. 1)  INBUF(I) = NMAX

         END DO

      ELSE IF (MTYPE .EQ. 4)  THEN            ! MIN MODE.

         DO I = 1, NPIXEL
           NCOUNT = 0

           DO IDSRN = 1, NI   ! FIND MIN VALUE >= THRESH.

             IF ( INBUF(IS(IDSRN)) .GT. THRESH )  THEN
                IF (NCOUNT .EQ. 0)  THEN
                  NMIN = INBUF(IS(IDSRN))
                  NCOUNT = 1
                ELSE 
                  NMIN = MIN(  NMIN, INBUF(IS(IDSRN))  )
                END IF
             END IF

             IS(IDSRN) = IS(IDSRN)+1
           END DO

           IF      (NCOUNT .EQ. 1)  INBUF(I) = NMIN

         END DO

      END IF

      RETURN          
      END



      SUBROUTINE NIBLR(NS,BUF,THRESH,NSEQ,MODE,
     .                 IANSWER,INC)
C#######################################################################
C  PURPOSE
C      NIBLR SCANS THROUGH THE PIXELS OF A LINE TO FIND THE 'IMAGE EDGE'.
C      THE SCAN STARTS AT THE BEGINNING OR THE END OF THE LINE ACCORDING
C      TO THE MODE PARAMETER.  IF THE INC PARAMETER IS NOT 1, THEN THE SCAN
C      DOES NOT GO PIXEL BY PIXEL BUT CHECKS ONLY EVERY INCth PIXEL.  NIBLR
C      SCANS UNTIL IT FINDS A GROUP OF NSEQ CONSECUTIVE (IN TERMS OF INC)
C      PIXELS ALL OF WHICH HAVE A DATA NUMBER GREATER THAN OR EQUAL TO THE
C      THRESH PARAMETER VALUE.  THE EDGE IS DEFINED AS THE FIRST PIXEL 
C      (ACCORDING TO THE DIRECTION OF THE SCAN) OF THAT GROUP.  

C  CALLING SEQUENCE 
C     CALL NIBLR(NS,BUF,THRESH,NSEQ,MODE, IANSWER ,INC)
C             
C  INPUT PARAMETERS ( all parameters are integers.)
C      NS      - NUMBER OF PIXELS IN THE LINE.
C      BUF     - DATA NUMBERS FOR THE LINE. ( BUF(I) FOR I = 1 TO NS. )
C      THRESH - DATA NUMBER THRESHOLD FOR DEFINING EDGE.
C      NSEQ    - NUMBER OF CONSECUTIVE PIXELS REQUIRED TO BE AT OR ABOVE THE
C                THRESH DATA NUMBER FOR DETERMINING THE EDGE.
C      MODE    - 0 FOR SCAN FROM BEGINNING OF LINE ( NIBBLE FROM LEFT ),
C                1 FOR SCAN FROM END OF LINE ( NIBBLE FROM RIGHT ).
C      INC     - SCAN WILL CHECK EVERY INCth PIXEL. 
C  OUTPUT PARAMETERS
C      IANSWER - PIXEL NUMBER WHERE EDGE IS.  IF NO EDGE IS FOUND, IANSWER=0.
C      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

      real*4 BUF(NS), thresh
      LOGICAL   LTOR
C
C=================START OF EXECUTABLE CODE===============================     
C
      LTOR = .TRUE.                ! SCAN FROM LEFT TO RIGHT.
      INCPAR = INC
      IF (  MODE    .NE. 0 )       LTOR   = .FALSE. ! RIGHT TO LEFT.

      IF ( LTOR )  THEN            ! SET SCANNING ACCORDING TO DIRECTION.
           IBEG = 1
           IEND = NS
           ISTEP = INCPAR
      ELSE 
           IBEG = NS
           IEND = 1
           ISTEP = -INCPAR
      END IF

      ICNT = 0                               ! CONSECUTIVE PIXEL COUNTER.

      DO  I = IBEG, IEND, ISTEP

          IF ( BUF(I) .GT. THRESH ) THEN         ! IF THRESHOLD MET,
               IF ( ICNT .EQ. 0 )  ITEMP = I     ! SAVE LOCATION OF FIRST
               ICNT = ICNT + 1                    ! PIXEL, & CHECK FOR
               IF ( ICNT .GE. NSEQ )  GOTO 5000   ! NSEQ CONSECUTIVE
          ELSE                                    ! PIXELS.
               ICNT = 0                           ! RESET COUNTER IF 
          END IF                                  ! THRESHOLD NOT MET.

      END DO

      ITEMP = 0                                  ! NO EDGE FOUND.

5000  CONTINUE
      IANSWER = ITEMP

      RETURN          
      END



       SUBROUTINE FASTFILL( NPIXEL,L0,BUF)
C#######################################################################
C  NAME OF ROUTINE
C      FASTFILL( FASTmos FILL)
C
C  PURPOSE
C      Subroutine FASTFILL fills a buffer with the value L0.
C
C  CALLING SEQUENCE 
C     CALL FASTFILL( NPIXEL,L0,BUF)
C  INPUT PARAMETERS ( all parameters are INTEGER except for BUF.)
C      NPIXEL   - dimension of BUF array.
C      L0       - value to fill BUF with.
C      BUF      - buffer to fill.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'fortport'  ! DEFINES INT2BYTE AND BYTE2INT CONVERSIONS.

C   THIS ROUTINE FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      REAL*4 BUF(*), L0

      DO I = 1, NPIXEL
        BUF(I) = L0
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fastmos.imake
#define  PROGRAM   fastmos

#define MODULE_LIST fastmos.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create fastmos.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=(1:48)
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SB      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NB      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM MMODE   TYPE=KEYWORD  COUNT=1       DEFAULT=OVERLAY  +
               VALID=(OVERLAY,AVERAGE,MOD,MAX,MIN)
!
PARM THRESH  TYPE=REAL  COUNT=1       DEFAULT=0.0
PARM PROGRESS TYPE=KEYWORD COUNT=(0:1)   DEFAULT=--         VALID=PROGRESS
PARM EDGE    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=EDGE
PARM NTHRESH TYPE=REAL COUNT=(0:1)   DEFAULT=--
PARM LTHRESH TYPE=REAL COUNT=(0:1)   DEFAULT=--
PARM RTHRESH TYPE=REAL COUNT=(0:1)   DEFAULT=--
PARM NSEQ    TYPE=INTEGER  COUNT=1       DEFAULT=8          VALID=(1:32768)
PARM NIBBLE  TYPE=INTEGER  COUNT=1       DEFAULT=4          VALID=(0:32768)
PARM LNIBBLE TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--         VALID=(0:32768)
PARM RNIBBLE TYPE=INTEGER  COUNT=(0:1)   DEFAULT=--         VALID=(0:32768)
PARM NINCR   TYPE=INTEGER  COUNT=1       DEFAULT=1          VALID=(1:32768)
!
PARM OFF1    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF2    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF3    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF4    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF5    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF6    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF7    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF8    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF9    TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF10   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF11   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF12   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF13   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF14   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF15   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF16   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF17   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF18   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF19   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF20   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF21   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF22   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF23   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF24   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF25   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF26   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF27   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF28   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF29   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF30   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF31   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF32   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF33   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF34   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF35   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF36   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF37   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF38   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF39   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF40   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF41   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF42   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF43   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF44   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF45   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF46   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF47   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
PARM OFF48   TYPE=INTEGER  COUNT=(2:4)   DEFAULT=(1,1) 
!
!# parm inp(6-48) hints=default
!
END-PROC
.TITLE
FASTMOS
.HELP
 PURPOSE:

Program FASTMOS takes input images and mosaics them to form an output image.
FASTMOS performs mosaicking by reading all of the input image files and 
combining them to form an output image according to the parameters for
mosaicking mode, edging, and image placement.  In a typical application, the
output image is a composite picture made from two or more input images each
of which contains a portion of the overall picture.
.PAGE
 EXECUTION:

The size of the output image is determined by the number of lines and number 
of samples in the SIZE field if the SIZE field is entered by the user.  If the
SIZE field is not entered, the output file is the same size as the first
input file.

FASTMOS can accept from one to 48 input images.  The input images are not
required all to have the same size.  The data type of the input images may
either be byte or halfword data, but all of the input images must have the
same data type.  The data type is obtained from the VICAR label of the first 
input image.

For mosaicking, the program needs to know the locations in the output image 
of each of the input images.  This is done by giving the location in the output
image of each input image.  The locations are given in terms of line number and
pixel number within a line.  The offset parameters (OFF1, OFF2, OFF3,...) are
used to specify the locations for the respective input images.  The offset
parameter can be omitted for images at the default location of line 1, pixel 1.
An input image is not required to lie entirely within the boundaries of the
output image.  If the upper left hand corner of an input image is not within 
the boundaries of the output image, the location of the input image is given
by extending the numbering of lines and pixels beyond the boundaries of the
output image.  Thus negative numbers or zero would be used for the locations
of input images beginning to the left or above the boundaries of the output
image.  Input images are allowed to overlap.

The mode by which FASTMOS determines the output data number values from the
input data numbers is user selectable through the MODE parameter.  There
are currently five modes to choose from.  All of the modes make use of the
THRESH threshold parameter, which is usually greater than or equal to 0.
In determining the output data number for a given pixel, FASTMOS finds
out which of the input images containing that pixel have a data number for that
pixel which is greater than the THRESH value.  The data numbers
for that pixel from those input images form a set of values that is then used
in the selected mode to give the output data number.  For example, if 
MODE=AVERAGE is selected, then the output data number for a pixel is the
average of those values for the pixel that are greater than the
THRESH value.  The following rules apply for each of the modes.  If none
of the input images have a data number value for a given pixel that is
greater than the THRESH value, the output data number is the
data number from the first input image if the pixel is contained in the
first input image, and the output data number is L0 if the pixel is not
contained in the first input image, where L0 is 0 if THRESH is greater than 0,
L0 is 0 if THRESH=0 and the data format is byte, and L0 is equal to THRESH
otherwise for integer data.  For real*4 data, L0 is -1.0E10.
If exactly one of the input images has 
a data number value for a given pixel that is greater than the 
THRESH value, the output data number is the data number from the one input 
image.  If more than one of the input images have a data number value for a 
given pixel that is greater than the THRESH value, the output 
data number is determined by the mosaicking mode.  The five mosaicking modes 
are discussed under the MODE parameter.

FASTMOS has an option for edging the input images prior to applying the
mosaicking mode.  Edging effectively removes the 'edges' of the input images
so that pixels in the 'edges' are not considered to be contained in their
images as far as the mosaicking process is concerned.  Several parameters
determine the precise effect of edging in any situation, but basically if
edging is selected, then each line of each input image is edged first on the
left and then on the right.  Edging means scanning through a line from one
end or another to the point at which the data numbers are greater than
a threshold value and then removing the pixels from the end of
the line up to a certain number of pixels beyond the point.  This is 
referred to as finding the edge of the scene data and nibbling-in a 
certain number of pixels beyond the edge.  Because of the line by 
line processing performed by the program, edging is only available
in the horizontal direction.  (Edging the top and bottom of images would
normally require an intermediate data set.)  Edging is typically used
to remove distortion around the edges of pictures that was caused by
interpolation, filtering, or other things.

FASTMOS uses dynamic memory allocation (using subroutine STACKA) to avoid
imposing any restrictions on the size of the images. 

The output image has the same data format  (byte or halfword) as the input 
images.  
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      fastmos INP=(a...) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      fastmos INP=(a...) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      fastmos (a...) b (sl,ss,nl,ns) optional parameters
      fastmos (a...) b optional parameters

       Here '(a...)' represents a list of one to 48 input image file names,
       and 'b' represents the output image file name.
.PAGE
EXAMPLES

1.    fastmos INP=(A,B,C) OUT=D SIZE=(1,1,1000,1000)           +
              'AVERAGE  OFF1=(100,100) OFF2=(-100 -25 600 700) 

      In this example the output file D is a 1000 line by 1000 pixel 
      image produced by mosaicking three images using
      the averaging mode. The default THRESH threshold value of 0 is used.
      The position relative to the output image of each of the input 
      images is as follows:

      Image A,  the first input, is positioned with its upper-lefthand
                corner (ULC) at line 100, pixel 100.  Since only two values
                were specified for OFF1, the entire image is used.

      Image B,  is positioned with its ULC outside the boundaries of
                the output image at line -100, pixel -25.  The first 600
                lines and the first 700 pixels per line are used from image B.
                Of these, the first 101 lines and 26 samples are ignored
                because they do not lie within the boundaries of the output
                image.

      Image C,  is positioned with its ULC at the default position of
                line 1, pixel 1.  The entire image is used.
.PAGE
2.    fastmos INP=(A,B,C,D,E) OUT=F

      In this example the output file F has the same size as the input file
      A.  The data format of the files defaults to the format given in the
      label of file A.  The mosaicking is performed by using the default
      overlay mode. The default THRESH value of 0 is used.  The five input
      images have been pre-registered so that they begin in the same location.


3.    fastmos INP=(A,B,C,D,E) OUT=F THRESH=10   'EDGE NTHRESH=50

      In this example the output file F has the same size as the input file
      A.  The data format of the files defaults to the format given in the
      label of file A.  The mosaicking is performed by using the default
      overlay mode. The THRESH value of 10 is used.  Each of the input
      images gets edged horizontally using a threshold value of 50.  
      The edging nibbles away (the default) four pixels beyond the edge 
      of the scene data.
.PAGE

4.    fastmos INP=(A,B,C,D,E) OUT=F THRESH=10   'AVERAGE     +
              'EDGE LTHRESH=5 LNIBBLE=3 RTHRESH=10 RNIBBLE=1

      This is the same as example 3 except that the averaging mode is used
      and the edging parameters are different.  The edging on the left 
      uses a threshold of 5 and extends 3 pixels beyond the scene edge.
      The edging on the right uses a threshold of 10 and extends 1 pixel
      beyond the scene edge. 

5.    GEN A 10 10
      GEN B 10 10 IVAL=101
      fastmos INP=(A,B) OUT=F SIZE=(1 1 16 16) THRESH=0 'OVERLAY OFF1=(2,1) 

      This is a mostly academic example in which all of the files have
      byte data format.  In this example the output file F has the size 
      16 lines by 16 samples.  Program FASTMOS internally makes a copy of
      files A and B padded with zeroes to a size of 16 by 16 with the offsets
      specified for A and defaulted for B.  It then compares the copy of A
      to the THRESH value.  Since THRESH=0, everything in the copy of A
      goes into the file F, and nothing in file B is utilized.
      This illustrates that THRESH=0 is not normally used for mosaicking
      byte data.  (It might be used for averaging images of the same size and 
      offset.  fastmos was originally implemented with the requirement of 
      THRESH >= 1 since it uses 0 for any padding around byte input images.)

.PAGE
RESTRICTIONS
The input images may be any format of 4 or fewer bytes per pixel, and must all
be of the same format.  The output image will be the same format as the inputs.


 OPERATION:

FASTMOS performs mosaicking on a line by line basis.  The offsetting of
input images is done at READ time.
.PAGE

 TIMING: 

  The following CPU times for FASTMOS was obtained on a 
VAX 8650 (MIPL2) in April 1993
			                                                CPU Time
gen FASTA1 NL=1000 NS=1000 IVAL=0 
gen FASTA2 NL=1000 NS=1000 IVAL=200
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO8 SIZE=(1,1,2000,2000) +
!  				4-93  SP  CPU TIME ON MIPL2 (VAX8650)    17.66s

For more information, see the file timefastmos.pdf in fastmos.com.

 The following timing data were measured on a Solaris Sun4 with two 1000x1000
 byte input images, in seconds:

                          User   System   Wall-clock
 byte/halfword version:   0.17    0.17      0.73 
 real*4 version:          0.26    0.17      0.89
.page

 ORIGINAL PROGRAMMER:    W. Benton
 PORTED TO UNIX: Steve Pohorsky
 COGNIZANT PROGRAMMER:   Lucas Kamp

 REVISION HISTORY
  93-4-7    SP   Made portable for UNIX.  Added PROGRESS parameter to
                 print percentage DONE during execution.
  03-8-14   NTT  Enabled for 3D (multi-banded) images
2012-09-18  lwk  Enabled for real*4 data (converted all buffers to real*4).
2012-10-02  lwk  made THRESH parameters consistent with old (integer) FASTMOS;
                 because the default threshold is now 0.0 (used to be 1), the
                 tests are now for DN>THRESH instead of DN>=THRESH.

.page
 CHANGED BEHAVIOUR DUE TO CONVERSION TO REAL*4 (OCT.2012):
The output of FASTMOS is slightly different than it used to be, because of
the requirement that the behaviour with respect to the default threshold
value of zero be consistent between the two versions. The integer version
(old fastmos) did this by setting the default THRESH parameter to 1 and
keeping DN >= THRESH. However, for real*4 data, that would lead to the
undesirable result of throwing away DNs between 0 and 1, so the default
value of THRESH was changed to 0 and the check was changed to keep DN>THRESH.
The logfile of the old version is retained in the .com file under the name
tstfastmos_integer.log_solos, for comparison with the new one, named
tstfastmos.log_solos. It can be seen that, when the default THRESH is used,
the results are the same for the two versions, but when a non-zero value of
THRESH is specified, the results differ at the edges, typically by one pixel.

.LEVEL1
.VARIABLE INP
Input image file names
.VARIABLE OUT
Output image file name
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
(This field is ignored.)
.VARIABLE SS
Starting sample number
(This field is ignored.)
.VARIABLE SB
Starting band number
(This field is ignored.)
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE NB
Number of bands
.VARIABLE MMODE
Mosaicking mode -- OVERLAY,
AVERAGE, MOD, MAX, or MIN.
.VARIABLE THRESH
Threshold used for mosaicking.
.VARIABLE PROGRESS
Enter for progress reporting.
.VARIABLE EDGE
Enter for edging.
.VARIABLE NTHRESH
Threshold for edging on both
left and right.
.VARIABLE LTHRESH
Threshold for edging on left.
.VARIABLE RTHRESH
Threshold for edging on right.
.VARIABLE NSEQ
Number of sequential pixels
which must satisfying edging 
threshold criteria at edge of
scene data.
.VARIABLE NIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on both left and right.
.VARIABLE LNIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on left.
.VARIABLE RNIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on right.
.VARIABLE NINCR
If NINCR=n, then scanning for
edge of scene data will check
every nth pixel.
.VARIABLE OFF1
Offset values for first input
image.
.VARIABLE OFF2
Offset values for second input
image.
.VARIABLE OFF3
Offset values for third input
image.
.VARIABLE OFF4
Offset values for fourth input
image.
.VARIABLE OFF5
Offset values for fifth input
image.
.VARIABLE OFF6
Offset values for sixth input
image.
.VARIABLE OFF7
Offset values for seventh input
image.
.VARIABLE OFF8
Offset values for eighth input
image.
.VARIABLE OFF9
Offset values for ninth input
image.
.VARIABLE OFF10
Offset values for input image 10
.VARIABLE OFF11
Offset values for input image 11
.VARIABLE OFF12
Offset values for input image 12
.VARIABLE OFF13
Offset values for input image 13
.VARIABLE OFF14
Offset values for input image 14
.VARIABLE OFF15
Offset values for input image 15
.VARIABLE OFF16
Offset values for input image 16
.VARIABLE OFF17
Offset values for input image 17
.VARIABLE OFF18
Offset values for input image 18
.VARIABLE OFF19
Offset values for input image 19
.VARIABLE OFF20
Offset values for input image 20
.VARIABLE OFF21
Offset values for input image 21
.VARIABLE OFF22
Offset values for input image 22
.VARIABLE OFF23
Offset values for input image 23
.VARIABLE OFF24
Offset values for input image 24
.VARIABLE OFF25
Offset values for input image 25
.VARIABLE OFF26
Offset values for input image 26
.VARIABLE OFF27
Offset values for input image 27
.VARIABLE OFF28
Offset values for input image 28
.VARIABLE OFF29
Offset values for input image 29
.VARIABLE OFF30
Offset values for input image 30
.VARIABLE OFF31
Offset values for input image 31
.VARIABLE OFF32
Offset values for input image 32
.VARIABLE OFF33
Offset values for input image 33
.VARIABLE OFF34
Offset values for input image 34
.VARIABLE OFF35
Offset values for input image 35
.VARIABLE OFF36
Offset values for input image 36
.VARIABLE OFF37
Offset values for input image 37
.VARIABLE OFF38
Offset values for input image 38
.VARIABLE OFF39
Offset values for input image 39
.VARIABLE OFF40
Offset values for input image 40
.VARIABLE OFF41
Offset values for input image 41
.VARIABLE OFF42
Offset values for input image 42
.VARIABLE OFF43
Offset values for input image 43
.VARIABLE OFF44
Offset values for input image 44
.VARIABLE OFF45
Offset values for input image 45
.VARIABLE OFF46
Offset values for input image 46
.VARIABLE OFF47
Offset values for input image 47
.VARIABLE OFF48
Offset values for input image 48
.LEVEL2
.VARIABLE SIZE
If the SIZE field is not entered, the output image has the same size as the
first input image.  If the SIZE field is entered, the number of lines and
number of samples refer to the size of the output image.  The starting line
and starting sample are ignored.
.VARIABLE NB
Number of bands.
.VARIABLE MMODE
The mosaicking mode specifies how the output data number values are determined 
from the input data numbers.   The following rules apply for each of the 
modes.  If none of the input images have a data number value for a 
given pixel that is greater than the THRESH value, the 
output data number is the data number from the first input image if 
the pixel is contained in the first input image, and the output 
data number is L0 if the pixel is not contained in the first input image, where
L0 is 0 if THRESH is greater than 0, L0 is 0 if THRESH=0 and the data format 
is byte, and L0 is equal to THRESH otherwise, for integer data.  For real*4
data, L0 is -1.0E10.
If exactly one of the input images has a data number value for a given pixel that
is greater than the THRESH value, the output data number is the
data number from the one input image.  If more than one of the input images
have a data number value for a given pixel that is greater than the
THRESH value, the output data number is determined by the mosaicking mode. 

There are currently five modes to choose from.  They are listed by name below.
For each mode a description is given of how the output data number is 
determined when there is more than one input image having a data number value 
that is greater than the THRESH value for a given pixel.  The
default mode is OVERLAY.

OVERLAY  - The input images are checked against the THRESH value in the order
           in which they are entered by the user.  The first data number value
           found which meets the threshold criteria is used for the output
           image.  This means that the order in which the input files are 
           entered gives a priority to the data in the files.

AVERAGE  - The average of the values meeting the threshold criteria is used.
           The average is found by integer division with no rounding.

MOD      - When there are two values meeting the threshold criteria, the 
           average of the values is used.  When there are more than two 
           values meeting the threshold criteria, the value closest to the
           average is used.  This mode may be particularly useful when
           combining many images with high bit-error rates.

MAX      - The maximum of the values meeting the threshold criteria is used.

MIN      - The minimum of the values meeting the threshold criteria is used.
.VARIABLE THRESH
Only values greater than the THRESH threshold parameter are used
by the mosaicking mode in determining the data numbers for the output image.
The THRESH value is usually greater than 0 for mosaicking.  THRESH can be
set to 0 for cases such as averaging images.  The default value is 1.
(See Example 5 in the main help for details about the case of THRESH=0
for byte data.  Users may need to convert images to halfword to use THRESH=0
for mosaicking.  Other VICAR programs, such as INSECT may be an alternative.)

For halfword images for which negative DNs are considered valid, a negative
THRESH value may be used.  In this case, 0 is an inappropriate value for
representing the absence of image data.  When THRESH is less than 0, FASTMOS
uses an output DN of THRESH-1 to represent the absence of image data.
(If THRESH = -32768, -32768 is used to represent the absence of image data.)
This value is used as an output DN where the output pixel does not lie in one
of the input images.  (See the MMODE parameter.)
.VARIABLE PROGRESS
If the PROGRESS parameter is specified, FASTMOS prints the percentage done
(in generating the output image) every 10% from 10 to 90%.
The default is to not print the progress.
.VARIABLE EDGE
If the EDGE parameter is specified, all input images are edged prior to
applying the mosaicking mode. No edging is the default.  'EDGE M U S T
be specified to invoke the edging algorithm.

If edging is selected, then each line of each input image is edged first on the
left and then on the right.  Edging means scanning through a line from one
end or another to the point at which the data numbers are greater than
a threshold value and then removing the pixels from the end of
the line up to a certain number of pixels beyond the point.  This is 
referred to as finding the edge of the scene data and nibbling in a 
certain number of pixels beyond the edge.  Because of the line by 
line processing performed by the program, edging is only available
in the horizontal direction.  (Edging the top and bottom of images would
normally require an intermediate data set.)  Edging is typically used
to remove distortion around the edges of pictures that was caused by
interpolation, filtering, or other things.

Several parameters are used to control the way that edging is done.  The 
parameters NTHRESH, LTHRESH, RTHRESH, NSEQ, and NINCR determine the location
of the edge of the scene data for lines of the input images.  The parameters
NIBBLE, LNIBBLE, and RNIBBLE determine how many pixels beyond the edge of 
the scene data are removed.

The edge of the scene data for a line is determined as follows.  The program
scans through the pixels of a line comparing the data numbers against the
edging threshold.  (Separate edging thresholds can be specified for scanning
from the left and scanning from the right using the LTHRESH and RTHRESH
parameters.  The NTHRESH parameter can be used to specify the same threshold
for scanning from the left and scanning from the right.)  The scanning begins
at one end of the line, and it checks successive pixels unless the NINCR
parameter is entered.  If NINCR is entered, the scanning checks only every
NINCRth pixel.  The program scans until it finds a group of NSEQ consecutive
(in terms of NINCR) pixels all of which have a data number greater than
the edging threshold.  The edge of the scene data is defined as the
first pixel (according to the direction of the scan) of that group.

The nibbling number is the number of pixels, starting with the edge of the
scene data, which are to be removed along with any pixels from the end of
the line to the edge of the scene data.  (If the nibbling number is zero, 
then just the pixels from the end of the line to the edge of the scene data
are removed.)  Separate nibbling numbers can be specified for scanning from 
the left and scanning from the right using the LNIBBLE and RNIBBLE parameters.
The NIBBLE parameter can be used to specify the same nibbling number for 
scanning from the left and scanning from the right.  

If no edge of the scene data is found when scanning, the entire line is 
removed.
.VARIABLE NTHRESH
The default for NTHRESH is THRESH.  (See also under EDGE.)
.VARIABLE LTHRESH
The default for LTHRESH is NTHRESH.  (See also under EDGE.)
.VARIABLE RTHRESH
The default for RTHRESH is NTHRESH.  (See also under EDGE.)
.VARIABLE NSEQ
The default for NSEQ is 8.  (See also under EDGE.)
.VARIABLE NIBBLE
The default for NIBBLE is 4.  (See also under EDGE.)
.VARIABLE LNIBBLE
The default for LNIBBLE is 4.  (See also under EDGE.)
.VARIABLE RNIBBLE
The default for RNIBBLE is 4.  (See also under EDGE.)
.VARIABLE NINCR
The default for NINCR is 1.  (See also under EDGE.)
.VARIABLE OFF1
Either two or four values may be entered for OFF1.  The first two values
give the line and pixel numbers, respectively, of the upper left hand corner
of the first input image file with respect to the output image. 
If the upper left hand corner of the input image is not within the 
boundaries of the output image, the location of the input image is given
by extending the numbering of lines and pixels beyond the boundaries of the
output image.  Thus negative numbers or zero would be used for the locations
of input images beginning to the left or above the boundaries of the output
image.  The default for the first two values is line 1 and pixel 1.  (The
default assumes that the input image was pre-registered.)

If entered, the third and fourth values give, respectively, the number of lines
and number of pixels per line to be used from the image.  This is used when
not all of the image is to be used in the program.  The default is to use all
of the lines and samples as specified in the VICAR label.
.VARIABLE OFF2
(See under OFF1.)
.VARIABLE OFF3
(See under OFF1.)
.VARIABLE OFF4
(See under OFF1.)
.VARIABLE OFF5
(See under OFF1.)
.VARIABLE OFF6
(See under OFF1.)
.VARIABLE OFF7
(See under OFF1.)
.VARIABLE OFF8
(See under OFF1.)
.VARIABLE OFF9
(See under OFF1.)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfastmos.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM FASTMOS
!
!      first test with two small byte-data input images.
!
gen FASTA1 NL=5 NS=5 IVAL=0 
gen FASTA2 NL=5 NS=5 IVAL=200
list FASTA1
list FASTA2
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO1 MMODE=AVERAGE
list FASTAO1
!    
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO2 MMODE=MOD
list FASTAO2
!    
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO3 MMODE=OVERLAY THRESH=3
list FASTAO3
!    
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO4 MMODE=MAX     THRESH=202
list FASTAO4
!    
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO5 MMODE=MIN     THRESH=2
list FASTAO5
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO6 OFF1=(-1,0,4,3) OFF2=(2,1)
list FASTAO6
!
!    offset input images out of sight. should get a zero image with SIZE size.
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
list FASTAO7 'ZEROES
!
!    make some bigger images from FASTA1 & FASTA2.
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
list FASTAO8
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO9 SIZE=(1,1,10,10)               +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
list FASTAO9
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO10 SIZE=(1,1,10,10) OFF1=(1,6)       +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
list FASTAO10 'ZEROES
!
!    offset first image.  Because THRESH=0, should just get FASTA1 surrounded
!    on 3 sides with zeroes.
!
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO11 MMODE=OVERLAY THRESH=0 OFF1=(2,1) +
   SIZE=(1 1 10 10) 
list FASTAO11 'zeroes
!
!                  try a simple copy.
!
fastmos FASTA2 FASTA3
list FASTA3
!
!      test with two small halfword-data input images.
!
gen FASTB1 NL=5 NS=5 IVAL=0    'HALF
gen FASTB2 NL=5 NS=5 IVAL=20000  'HALF
list FASTB1
list FASTB2
!
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO1 MMODE=AVERAGE
list FASTBO1
!    
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO2 MMODE=MOD 
list FASTBO2
!    
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO3 MMODE=OVERLAY THRESH=3
list FASTBO3
!    
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO4 MMODE=MAX     THRESH=20002
list FASTBO4
!    
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO5 MMODE=MIN     THRESH=2
list FASTBO5
!
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO6 OFF1=(-1,0,4,3) OFF2=(2,1)
list FASTBO6
!
!    offset input images out of sight. should get a zero image with SIZE size.
!
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
list FASTBO7 'ZEROES
!
!    make some bigger images from FASTB1 & FASTB2.
!
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
list FASTBO8
!
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO9 SIZE=(1,1,10,10)               +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
list FASTBO9
!
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO10 SIZE=(1,1,10,10) OFF1=(1,6)       +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
list FASTBO10 'ZEROES
!
!  test case of negative THRESH values.
!
gen FASTB3 NL=5 NS=5 IVAL= -10 'HALF
fastmos FASTB3 FASTBO11 SIZE=(1,1,6,6) THRESH=-9
list FASTBO11
!
fastmos FASTB3 FASTBO12 SIZE=(1,1,6,6) THRESH=-32768 'EDGE NSEQ=1 NIBBLE=1
list FASTBO12
!
!                  try a simple copy.
!
fastmos FASTB2 FASTB3
list FASTB3
!
!      test with three small byte-data input images.
!
gen FASTC NL=10 NS=10 IVAL=0 LINC=0 SINC=0
sargonb FASTC FASTC1 ADD 100 (3,1,3,10,9,10,9,1)
sargonb FASTC FASTC2 ADD 200 (2,1,2,10,8,10,8,1)
sargonb FASTC FASTC3 ADD 120 (4,3,4,10,8,10,8,3)
list FASTC1 'ZEROES
list FASTC2 'ZEROES
list FASTC3 'ZEROES
!
fastmos INP=(FASTC1,FASTC2,FASTC3) OUT=FASTCO1 MMODE=AVERAGE OFF2=(2,1)
list FASTCO1 'ZEROES
!
fastmos INP=(FASTC1,FASTC2,FASTC3) OUT=FASTCO2 'MOD OFF2=(2,1) +
        'EDGE NIBBLE=2 NSEQ=4
list FASTCO2 'ZEROES
!    
!    mosaic 10 files together. 
!
gen FASTD  NL=10 NS=10 'HALF IVAL=0
sargonb FASTD FASTD1 SUBTRACT 20000 (3,3,3,6,6,6,6,3)
list FASTD1
gen FASTD2 NL=10 NS=10 'HALF IVAL=200
gen FASTD3 NL=10 NS=10 'HALF IVAL=400
gen FASTD4 NL=10 NS=10 'HALF IVAL=600
gen FASTD5 NL=10 NS=10 'HALF IVAL=800
gen FASTD6 NL=10 NS=10 'HALF IVAL=1000
gen FASTD7 NL=10 NS=10 'HALF IVAL=1200
gen FASTD8 NL=10 NS=10 'HALF IVAL=1400
gen FASTD9 NL=10 NS=10 'HALF IVAL=1600
gen FASTDA NL=10 NS=10 'HALF IVAL=1800
fastmos INP=(FASTD1 FASTD2 FASTD3 FASTD4 FASTD5 FASTD6   +
             FASTD7 FASTD8 FASTD9 FASTDA)                +
        OUT=FASTDO SIZE=(1 1 50 16) MMODE=OVER 'PROGRESS  +
        OFF1=(2,1)            OFF2=(1,5)                 +
        OFF3=(12,1)           OFF4=(5,5,10 10)           +
        OFF5=(22,1)           OFF6=(25,5)                +
        OFF7=(32,1)           OFF8=(30,5)                +
        OFF9=(42,1)           OFF10=(40,5) 
list FASTDO
!
!  Testing 3D images (added 2003-08-14)
!
gen FASTA13D NL=5 NS=5 NB=5 IVAL=0 
gen FASTA23D NL=5 NS=5 NB=5 IVAL=200
list FASTA13D
list FASTA23D
!
fastmos INP=(FASTA13D,FASTA23D) OUT=FASTAO13D MMODE=AVERAGE
list FASTAO13D 'zero
!
!      test with real*4 data (added 2012-10-01)
!
gen FASTE1 NL=5 NS=5 IVAL=0 'real
gen FASTE2 NL=5 NS=5 IVAL=200 'real
list FASTE1
list FASTE2
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO1 MMODE=AVERAGE
list FASTEO1
!    
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO2 MMODE=MOD
list FASTEO2
!    
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO3 MMODE=OVERLAY THRESH=3
list FASTEO3
!    
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO4 MMODE=MAX     THRESH=202
list FASTEO4
!    
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO5 MMODE=MIN     THRESH=2
list FASTEO5
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO6 OFF1=(-1,0,4,3) OFF2=(2,1)
list FASTEO6
!
!    offset input images out of sight. should get a zero image with SIZE size.
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
list FASTEO7 'ZEROES
!
!    make some bigger images from FASTE1 & FASTE2.
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
list FASTEO8
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO9 SIZE=(1,1,10,10)               +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
list FASTEO9
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO10 SIZE=(1,1,10,10) OFF1=(1,6)       +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
list FASTEO10 'ZEROES
!
!    offset first image.  Because THRESH=0, should just get FASTE1 surrounded
!    on 3 sides with zeroes.
!
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO11 MMODE=OVERLAY THRESH=0 OFF1=(2,1) +
   SIZE=(1 1 10 10) 
list FASTEO11 'zeroes

! clean up
ush rm -f FAST?
ush rm -f FAST??
ush rm -f FAST???
ush rm -f FAST????
ush rm -f FAST?????

end-proc
$!-----------------------------------------------------------------------------
$ create tstfastmos.log_solos
tstfastmos
gen FASTA1 NL=5 NS=5 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTA2 NL=5 NS=5 IVAL=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTA1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
list FASTA2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
     Samp     1       3       5
   Line
      1     200 201 202 203 204
      2     201 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO1 MMODE=AVERAGE
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:37 2012
     Samp     1       3       5
   Line
      1     200 101 102 103 104
      2     101 102 103 104 105
      3     102 103 104 105 106
      4     103 104 105 106 107
      5     104 105 106 107 108
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO2 MMODE=MOD
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:37 2012
     Samp     1       3       5
   Line
      1     200 101 102 103 104
      2     101 102 103 104 105
      3     102 103 104 105 106
      4     103 104 105 106 107
      5     104 105 106 107 108
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO3 MMODE=OVERLAY THRESH=3
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:38 2012
     Samp     1       3       5
   Line
      1     200 201 202 203   4
      2     201 202 203   4   5
      3     202 203   4   5   6
      4     203   4   5   6   7
      5       4   5   6   7   8
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO4 MMODE=MAX     THRESH=202
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO4
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:39 2012
     Samp     1       3       5
   Line
      1       0   1   2 203 204
      2       1   2 203 204 205
      3       2 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO5 MMODE=MIN     THRESH=2
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO5
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:39 2012
     Samp     1       3       5
   Line
      1     200 201 202   3   4
      2     201 202   3   4   5
      3     202   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO6 OFF1=(-1,0,4,3) OFF2=(2,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO6
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:40 2012
     Samp     1       3       5
   Line
      1       3   4   0   0   0
      2       4   5 202 203 204
      3     201 202 203 204 205
      4     202 203 204 205 206
      5     203 204 205 206 207
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO7 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:41 2012
     Samp     1       3
   Line
      1       0   0   0   0
      2       0   0   0   0
      3       0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO8
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:41 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   1   2   3   4
      2       0   0   0   0   0   1   2   3   4   5
      3       0   0   0   0   0   2   3   4   5   6
      4       0   0   0   0   0   3   4   5   6   7
      5       0   0   0   0   0   4   5   6   7   8
      6     200 201 202 203 204   0   0   0   0   0
      7     201 202 203 204 205   0   0   0   0   0
      8     202 203 204 205 206   0   0   0   0   0
      9     203 204 205 206 207   0   0   0   0   0
     10     204 205 206 207 208   0   0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO9 SIZE=(1,1,10,10)                +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO9
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:42 2012
     Samp     1       3       5       7       9
   Line

      2       0   0   0   0   0   0   0   0   4   0
      3       0   0   0   0   0   0   0   4   5   0
      4       0   0   0   0   0   0   4   5   6   0
      5       0   0   0   0   0   0   5   6   7   0
      6       0 201 202 203   0   0   0   0   0   0
      7       0 202 203 204   0   0   0   0   0   0
      8       0 203 204 205   0   0   0   0   0   0
      9       0 204 205 206   0   0   0   0   0   0
     10       0 205 206 207   0   0   0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO10 SIZE=(1,1,10,10) OFF1=(1,6)        +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO10 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:43 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   5   6   7   8
      6       0 201 202 203 204   0   0   0   0   0
      7       0 202 203 204 205   0   0   0   0   0
      8       0 203 204 205 206   0   0   0   0   0
      9       0 204 205 206 207   0   0   0   0   0
     10       0 205 206 207 208   0   0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO11 MMODE=OVERLAY THRESH=0 OFF1=(2,1)  +
   SIZE=(1 1 10 10)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO11 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:43 2012
     Samp     1       3       5       7       9
   Line
      1     200 201 202 203 204   0   0   0   0   0
      2     201   1   2   3   4   0   0   0   0   0
      3       1   2   3   4   5   0   0   0   0   0
      4       2   3   4   5   6   0   0   0   0   0
      5       3   4   5   6   7   0   0   0   0   0
      6       4   5   6   7   8   0   0   0   0   0
      7       0   0   0   0   0   0   0   0   0   0
      8       0   0   0   0   0   0   0   0   0   0
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
fastmos FASTA2 FASTA3
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTA3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:36 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
     Samp     1       3       5
   Line
      1     200 201 202 203 204
      2     201 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208
gen FASTB1 NL=5 NS=5 IVAL=0    'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTB2 NL=5 NS=5 IVAL=20000  'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTB1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
     Samp       1     2     3     4     5
   Line
      1         0     1     2     3     4
      2         1     2     3     4     5
      3         2     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
list FASTB2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002 20003 20004
      2     20001 20002 20003 20004 20005
      3     20002 20003 20004 20005 20006
      4     20003 20004 20005 20006 20007
      5     20004 20005 20006 20007 20008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO1 MMODE=AVERAGE
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:45 2012
     Samp       1     2     3     4     5
   Line
      1     20000 10001 10002 10003 10004
      2     10001 10002 10003 10004 10005
      3     10002 10003 10004 10005 10006
      4     10003 10004 10005 10006 10007
      5     10004 10005 10006 10007 10008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO2 MMODE=MOD
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:46 2012
     Samp       1     2     3     4     5
   Line
      1     20000 10001 10002 10003 10004
      2     10001 10002 10003 10004 10005
      3     10002 10003 10004 10005 10006
      4     10003 10004 10005 10006 10007
      5     10004 10005 10006 10007 10008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO3 MMODE=OVERLAY THRESH=3
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:46 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002 20003     4
      2     20001 20002 20003     4     5
      3     20002 20003     4     5     6
      4     20003     4     5     6     7
      5         4     5     6     7     8
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO4 MMODE=MAX     THRESH=20002
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO4
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:47 2012
     Samp       1     2     3     4     5
   Line
      1         0     1     2 20003 20004
      2         1     2 20003 20004 20005
      3         2 20003 20004 20005 20006
      4     20003 20004 20005 20006 20007
      5     20004 20005 20006 20007 20008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO5 MMODE=MIN     THRESH=2
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO5
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:48 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002     3     4
      2     20001 20002     3     4     5
      3     20002     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO6 OFF1=(-1,0,4,3) OFF2=(2,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO6
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:48 2012
     Samp       1     2     3     4     5
   Line
      1         3     4     0     0     0
      2         4     5 20002 20003 20004
      3     20001 20002 20003 20004 20005
      4     20002 20003 20004 20005 20006
      5     20003 20004 20005 20006 20007
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO7 'ZEROES
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:49 2012
     Samp       1     2     3     4
   Line
      1         0     0     0     0
      2         0     0     0     0
      3         0     0     0     0
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO8
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:50 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     1     2     3     4
      2         0     0     0     0     0     1     2     3     4     5
      3         0     0     0     0     0     2     3     4     5     6
      4         0     0     0     0     0     3     4     5     6     7
      5         0     0     0     0     0     4     5     6     7     8
      6     20000 20001 20002 20003 20004     0     0     0     0     0
      7     20001 20002 20003 20004 20005     0     0     0     0     0
      8     20002 20003 20004 20005 20006     0     0     0     0     0
      9     20003 20004 20005 20006 20007     0     0     0     0     0
     10     20004 20005 20006 20007 20008     0     0     0     0     0
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO9 SIZE=(1,1,10,10)                +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO9
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:50 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line

      2         0     0     0     0     0     0     0     0     4     0
      3         0     0     0     0     0     0     0     4     5     0
      4         0     0     0     0     0     0     4     5     6     0
      5         0     0     0     0     0     0     5     6     7     0
      6         0 20001 20002 20003     0     0     0     0     0     0
      7         0 20002 20003 20004     0     0     0     0     0     0
      8         0 20003 20004 20005     0     0     0     0     0     0
      9         0 20004 20005 20006     0     0     0     0     0     0
     10         0 20005 20006 20007     0     0     0     0     0     0
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO10 SIZE=(1,1,10,10) OFF1=(1,6)        +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO10 'ZEROES
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:51 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     0     0     0     0
      2         0     0     0     0     0     0     0     0     0     0
      3         0     0     0     0     0     0     0     0     0     0
      4         0     0     0     0     0     0     0     0     0     0
      5         0     0     0     0     0     0     5     6     7     8
      6         0 20001 20002 20003 20004     0     0     0     0     0
      7         0 20002 20003 20004 20005     0     0     0     0     0
      8         0 20003 20004 20005 20006     0     0     0     0     0
      9         0 20004 20005 20006 20007     0     0     0     0     0
     10         0 20005 20006 20007 20008     0     0     0     0     0
gen FASTB3 NL=5 NS=5 IVAL= -10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
fastmos FASTB3 FASTBO11 SIZE=(1,1,6,6) THRESH=-9
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO11
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:51 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:52 2012
     Samp       1     2     3     4     5     6
   Line
      1       -10    -9    -8    -7    -6    -9
      2        -9    -8    -7    -6    -5    -9
      3        -8    -7    -6    -5    -4    -9
      4        -7    -6    -5    -4    -3    -9
      5        -6    -5    -4    -3    -2    -9
      6        -9    -9    -9    -9    -9    -9
fastmos FASTB3 FASTBO12 SIZE=(1,1,6,6) THRESH=-32768 'EDGE NSEQ=1 NIBBLE=1
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTBO12
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:51 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:52 2012
     Samp       1     2     3     4     5     6
   Line
      1    -32768    -9    -8    -7-32768-32768
      2    -32768    -8    -7    -6-32768-32768
      3    -32768    -7    -6    -5-32768-32768
      4    -32768    -6    -5    -4-32768-32768
      5    -32768    -5    -4    -3-32768-32768
      6    -32768-32768-32768-32768-32768-32768
fastmos FASTB2 FASTB3
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTB3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:44 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:53 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002 20003 20004
      2     20001 20002 20003 20004 20005
      3     20002 20003 20004 20005 20006
      4     20003 20004 20005 20006 20007
      5     20004 20005 20006 20007 20008
gen FASTC NL=10 NS=10 IVAL=0 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb FASTC FASTC1 ADD 100 (3,1,3,10,9,10,9,1)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
sargonb FASTC FASTC2 ADD 200 (2,1,2,10,8,10,8,1)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
sargonb FASTC FASTC3 ADD 120 (4,3,4,10,8,10,8,3)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
list FASTC1 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:53 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Oct  2 14:52:54 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3     100 100 100 100 100 100 100 100 100 100
      4     100 100 100 100 100 100 100 100 100 100
      5     100 100 100 100 100 100 100 100 100 100
      6     100 100 100 100 100 100 100 100 100 100
      7     100 100 100 100 100 100 100 100 100 100
      8     100 100 100 100 100 100 100 100 100 100
      9     100 100 100 100 100 100 100 100 100 100
     10       0   0   0   0   0   0   0   0   0   0
list FASTC2 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:53 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Oct  2 14:52:54 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2     200 200 200 200 200 200 200 200 200 200
      3     200 200 200 200 200 200 200 200 200 200
      4     200 200 200 200 200 200 200 200 200 200
      5     200 200 200 200 200 200 200 200 200 200
      6     200 200 200 200 200 200 200 200 200 200
      7     200 200 200 200 200 200 200 200 200 200
      8     200 200 200 200 200 200 200 200 200 200
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
list FASTC3 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:53 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Oct  2 14:52:55 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0 120 120 120 120 120 120 120 120
      5       0   0 120 120 120 120 120 120 120 120
      6       0   0 120 120 120 120 120 120 120 120
      7       0   0 120 120 120 120 120 120 120 120
      8       0   0 120 120 120 120 120 120 120 120
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
fastmos INP=(FASTC1,FASTC2,FASTC3) OUT=FASTCO1 MMODE=AVERAGE OFF2=(2,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTCO1 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:53 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:56 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3     150 150 150 150 150 150 150 150 150 150
      4     150 150 140 140 140 140 140 140 140 140
      5     150 150 140 140 140 140 140 140 140 140
      6     150 150 140 140 140 140 140 140 140 140
      7     150 150 140 140 140 140 140 140 140 140
      8     150 150 140 140 140 140 140 140 140 140
      9     150 150 150 150 150 150 150 150 150 150
     10       0   0   0   0   0   0   0   0   0   0
fastmos INP=(FASTC1,FASTC2,FASTC3) OUT=FASTCO2 'MOD OFF2=(2,1)  +
        'EDGE NIBBLE=2 NSEQ=4
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTCO2 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:53 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:57 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0 150 150 150 150 150 150   0   0
      4       0   0 150 150 120 120 120 120   0   0
      5       0   0 150 150 120 120 120 120   0   0
      6       0   0 150 150 120 120 120 120   0   0
      7       0   0 150 150 120 120 120 120   0   0
      8       0   0 150 150 120 120 120 120   0   0
      9       0   0 150 150 150 150 150 150   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen FASTD  NL=10 NS=10 'HALF IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb FASTD FASTD1 SUBTRACT 20000 (3,3,3,6,6,6,6,3)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
list FASTD1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:57 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Oct  2 14:52:57 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3-19996-19995-19994-19993     8     9    10    11
      4         3     4-19995-19994-19993-19992     9    10    11    12
      5         4     5-19994-19993-19992-19991    10    11    12    13
      6         5     6-19993-19992-19991-19990    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
gen FASTD2 NL=10 NS=10 'HALF IVAL=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD3 NL=10 NS=10 'HALF IVAL=400
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD4 NL=10 NS=10 'HALF IVAL=600
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD5 NL=10 NS=10 'HALF IVAL=800
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD6 NL=10 NS=10 'HALF IVAL=1000
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD7 NL=10 NS=10 'HALF IVAL=1200
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD8 NL=10 NS=10 'HALF IVAL=1400
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD9 NL=10 NS=10 'HALF IVAL=1600
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTDA NL=10 NS=10 'HALF IVAL=1800
Beginning VICAR task gen
GEN Version 6
GEN task completed
fastmos INP=(FASTD1 FASTD2 FASTD3 FASTD4 FASTD5 FASTD6    +
             FASTD7 FASTD8 FASTD9 FASTDA)                 +
        OUT=FASTDO SIZE=(1 1 50 16) MMODE=OVER 'PROGRESS   +
        OFF1=(2,1)            OFF2=(1,5)                  +
        OFF3=(12,1)           OFF4=(5,5,10 10)            +
        OFF5=(22,1)           OFF6=(25,5)                 +
        OFF7=(32,1)           OFF8=(30,5)                 +
        OFF9=(42,1)           OFF10=(40,5)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
   fastmos 10% done
   fastmos 20% done
   fastmos 30% done
   fastmos 40% done
   fastmos 50% done
   fastmos 60% done
   fastmos 70% done
   fastmos 80% done
   fastmos 90% done
list FASTDO
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:57 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     0     0     0   200   201   202   203   204   205   206   207   208   209     0
      2         0     1     2     3     4     5     6     7     8     9   207   208   209   210     0
      3         1     2     3     4     5     6     7     8     9    10   208   209   210   211     0
      4         2     3-19996-19995   203   204     8     9    10    11   209   210   211   212     0
      5         3     4-19995-19994   204   205     9    10    11    12   210   211   212   213     0
      6         4     5-19994-19993   205   206    10    11    12    13   211   212   213   214     0
      7         5     6-19993-19992   206   207    11    12    13    14   212   213   214   215     0
      8         6     7     8     9    10    11    12    13    14    15   213   214   215   216     0
      9         7     8     9    10    11    12    13    14    15    16   214   215   216   217     0
     10         8     9    10    11    12    13    14    15    16    17   215   216   217   218     0
     11         9    10    11    12    13    14    15    16    17    18   612   613   614   615     0
     12       400   401   402   403   404   405   406   407   408   409   613   614   615   616     0
     13       401   402   403   404   405   406   407   408   409   410   614   615   616   617     0
     14       402   403   404   405   406   407   408   409   410   411   615   616   617   618     0
     15       403   404   405   406   407   408   409   410   411   412     0     0     0     0     0
     16       404   405   406   407   408   409   410   411   412   413     0     0     0     0     0
     17       405   406   407   408   409   410   411   412   413   414     0     0     0     0     0
     18       406   407   408   409   410   411   412   413   414   415     0     0     0     0     0
     19       407   408   409   410   411   412   413   414   415   416     0     0     0     0     0
     20       408   409   410   411   412   413   414   415   416   417     0     0     0     0     0
     21       409   410   411   412   413   414   415   416   417   418     0     0     0     0     0
     22       800   801   802   803   804   805   806   807   808   809     0     0     0     0     0
     23       801   802   803   804   805   806   807   808   809   810     0     0     0     0     0
     24       802   803   804   805   806   807   808   809   810   811     0     0     0     0     0
     25       803   804   805   806   807   808   809   810   811   812  1006  1007  1008  1009     0
     26       804   805   806   807   808   809   810   811   812   813  1007  1008  1009  1010     0
     27       805   806   807   808   809   810   811   812   813   814  1008  1009  1010  1011     0
     28       806   807   808   809   810   811   812   813   814   815  1009  1010  1011  1012     0
     29       807   808   809   810   811   812   813   814   815   816  1010  1011  1012  1013     0
     30       808   809   810   811   812   813   814   815   816   817  1011  1012  1013  1014     0
     31       809   810   811   812   813   814   815   816   817   818  1012  1013  1014  1015     0
     32      1200  1201  1202  1203  1007  1008  1009  1010  1011  1012  1013  1014  1015  1016     0
     33      1201  1202  1203  1204  1008  1009  1010  1011  1012  1013  1014  1015  1016  1017     0
     34      1202  1203  1204  1205  1009  1010  1011  1012  1013  1014  1015  1016  1017  1018     0
     35      1203  1204  1205  1206  1207  1208  1209  1210  1211  1212  1411  1412  1413  1414     0
     36      1204  1205  1206  1207  1208  1209  1210  1211  1212  1213  1412  1413  1414  1415     0
     37      1205  1206  1207  1208  1209  1210  1211  1212  1213  1214  1413  1414  1415  1416     0
     38      1206  1207  1208  1209  1210  1211  1212  1213  1214  1215  1414  1415  1416  1417     0
     39      1207  1208  1209  1210  1211  1212  1213  1214  1215  1216  1415  1416  1417  1418     0
     40      1208  1209  1210  1211  1212  1213  1214  1215  1216  1217  1806  1807  1808  1809     0
     41      1209  1210  1211  1212  1213  1214  1215  1216  1217  1218  1807  1808  1809  1810     0
     42      1600  1601  1602  1603  1604  1605  1606  1607  1608  1609  1808  1809  1810  1811     0
     43      1601  1602  1603  1604  1605  1606  1607  1608  1609  1610  1809  1810  1811  1812     0
     44      1602  1603  1604  1605  1606  1607  1608  1609  1610  1611  1810  1811  1812  1813     0
     45      1603  1604  1605  1606  1607  1608  1609  1610  1611  1612  1811  1812  1813  1814     0
     46      1604  1605  1606  1607  1608  1609  1610  1611  1612  1613  1812  1813  1814  1815     0
     47      1605  1606  1607  1608  1609  1610  1611  1612  1613  1614  1813  1814  1815  1816     0
     48      1606  1607  1608  1609  1610  1611  1612  1613  1614  1615  1814  1815  1816  1817     0


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:57 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
     49      1607  1608  1609  1610  1611  1612  1613  1614  1615  1616  1815  1816  1817  1818     0
     50      1608  1609  1610  1611  1612  1613  1614  1615  1616  1617     0     0     0     0     0
gen FASTA13D NL=5 NS=5 NB=5 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTA23D NL=5 NS=5 NB=5 IVAL=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTA13D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1       1   2   3   4   5
      2       2   3   4   5   6
      3       3   4   5   6   7
      4       4   5   6   7   8
      5       5   6   7   8   9


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1       2   3   4   5   6
      2       3   4   5   6   7
      3       4   5   6   7   8
      4       5   6   7   8   9
      5       6   7   8   9  10


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5
   Line
      1       3   4   5   6   7
      2       4   5   6   7   8
      3       5   6   7   8   9
      4       6   7   8   9  10
      5       7   8   9  10  11


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5
   Line
      1       4   5   6   7   8
      2       5   6   7   8   9
      3       6   7   8   9  10
      4       7   8   9  10  11
      5       8   9  10  11  12
list FASTA23D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1     200 201 202 203 204
      2     201 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1     201 202 203 204 205
      2     202 203 204 205 206
      3     203 204 205 206 207
      4     204 205 206 207 208
      5     205 206 207 208 209


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1     202 203 204 205 206
      2     203 204 205 206 207
      3     204 205 206 207 208
      4     205 206 207 208 209
      5     206 207 208 209 210


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5
   Line
      1     203 204 205 206 207
      2     204 205 206 207 208
      3     205 206 207 208 209
      4     206 207 208 209 210
      5     207 208 209 210 211


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5
   Line
      1     204 205 206 207 208
      2     205 206 207 208 209
      3     206 207 208 209 210
      4     207 208 209 210 211
      5     208 209 210 211 212
fastmos INP=(FASTA13D,FASTA23D) OUT=FASTAO13D MMODE=AVERAGE
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTAO13D 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1     200 101 102 103 104
      2     101 102 103 104 105
      3     102 103 104 105 106
      4     103 104 105 106 107
      5     104 105 106 107 108


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1     101 102 103 104 105
      2     102 103 104 105 106
      3     103 104 105 106 107
      4     104 105 106 107 108
      5     105 106 107 108 109


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1     102 103 104 105 106
      2     103 104 105 106 107
      3     104 105 106 107 108
      4     105 106 107 108 109
      5     106 107 108 109 110


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5
   Line
      1     103 104 105 106 107
      2     104 105 106 107 108
      3     105 106 107 108 109
      4     106 107 108 109 110
      5     107 108 109 110 111


 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:52:59 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5
   Line
      1     104 105 106 107 108
      2     105 106 107 108 109
      3     106 107 108 109 110
      4     107 108 109 110 111
      5     108 109 110 111 112
gen FASTE1 NL=5 NS=5 IVAL=0 'real
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTE2 NL=5 NS=5 IVAL=200 'real
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTE1
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
     Samp             1           2           3           4           5
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
list FASTE2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
     Samp             1           2           3           4           5
   Line
      1       2.000E+02   2.010E+02   2.020E+02   2.030E+02   2.040E+02
      2       2.010E+02   2.020E+02   2.030E+02   2.040E+02   2.050E+02
      3       2.020E+02   2.030E+02   2.040E+02   2.050E+02   2.060E+02
      4       2.030E+02   2.040E+02   2.050E+02   2.060E+02   2.070E+02
      5       2.040E+02   2.050E+02   2.060E+02   2.070E+02   2.080E+02
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO1 MMODE=AVERAGE
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO1
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:01 2012
     Samp             1           2           3           4           5
   Line
      1       2.000E+02   1.010E+02   1.020E+02   1.030E+02   1.040E+02
      2       1.010E+02   1.020E+02   1.030E+02   1.040E+02   1.050E+02
      3       1.020E+02   1.030E+02   1.040E+02   1.050E+02   1.060E+02
      4       1.030E+02   1.040E+02   1.050E+02   1.060E+02   1.070E+02
      5       1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO2 MMODE=MOD
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:01 2012
     Samp             1           2           3           4           5
   Line
      1       2.000E+02   1.010E+02   1.020E+02   1.030E+02   1.040E+02
      2       1.010E+02   1.020E+02   1.030E+02   1.040E+02   1.050E+02
      3       1.020E+02   1.030E+02   1.040E+02   1.050E+02   1.060E+02
      4       1.030E+02   1.040E+02   1.050E+02   1.060E+02   1.070E+02
      5       1.040E+02   1.050E+02   1.060E+02   1.070E+02   1.080E+02
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO3 MMODE=OVERLAY THRESH=3
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO3
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:02 2012
     Samp             1           2           3           4           5
   Line
      1       2.000E+02   2.010E+02   2.020E+02   2.030E+02   4.000E+00
      2       2.010E+02   2.020E+02   2.030E+02   4.000E+00   5.000E+00
      3       2.020E+02   2.030E+02   4.000E+00   5.000E+00   6.000E+00
      4       2.030E+02   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO4 MMODE=MAX     THRESH=202
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO4
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:03 2012
     Samp             1           2           3           4           5
   Line
      1       0.000E+00   1.000E+00   2.000E+00   2.030E+02   2.040E+02
      2       1.000E+00   2.000E+00   2.030E+02   2.040E+02   2.050E+02
      3       2.000E+00   2.030E+02   2.040E+02   2.050E+02   2.060E+02
      4       2.030E+02   2.040E+02   2.050E+02   2.060E+02   2.070E+02
      5       2.040E+02   2.050E+02   2.060E+02   2.070E+02   2.080E+02
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO5 MMODE=MIN     THRESH=2
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO5
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:03 2012
     Samp             1           2           3           4           5
   Line
      1       2.000E+02   2.010E+02   2.020E+02   3.000E+00   4.000E+00
      2       2.010E+02   2.020E+02   3.000E+00   4.000E+00   5.000E+00
      3       2.020E+02   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO6 OFF1=(-1,0,4,3) OFF2=(2,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO6
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:04 2012
     Samp             1           2           3           4           5
   Line
      1       3.000E+00   4.000E+00  -1.000E+10  -1.000E+10  -1.000E+10
      2       4.000E+00   5.000E+00   2.020E+02   2.030E+02   2.040E+02
      3       2.010E+02   2.020E+02   2.030E+02   2.040E+02   2.050E+02
      4       2.020E+02   2.030E+02   2.040E+02   2.050E+02   2.060E+02
      5       2.030E+02   2.040E+02   2.050E+02   2.060E+02   2.070E+02
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO7 'ZEROES
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:05 2012
     Samp             1           2           3           4
   Line
      1      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      2      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      3      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO8
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:05 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00
      2      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00
      3      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00
      4      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00
      5      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      6       2.000E+02   2.010E+02   2.020E+02   2.030E+02   2.040E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      7       2.010E+02   2.020E+02   2.030E+02   2.040E+02   2.050E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      8       2.020E+02   2.030E+02   2.040E+02   2.050E+02   2.060E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      9       2.030E+02   2.040E+02   2.050E+02   2.060E+02   2.070E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
     10       2.040E+02   2.050E+02   2.060E+02   2.070E+02   2.080E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO9 SIZE=(1,1,10,10)                +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO9
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:06 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      2      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   4.000E+00  -1.000E+10
      3      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   4.000E+00   5.000E+00  -1.000E+10
      4      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   4.000E+00   5.000E+00   6.000E+00  -1.000E+10
      5      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   5.000E+00   6.000E+00   7.000E+00  -1.000E+10
      6      -1.000E+10   2.010E+02   2.020E+02   2.030E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      7      -1.000E+10   2.020E+02   2.030E+02   2.040E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      8      -1.000E+10   2.030E+02   2.040E+02   2.050E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      9      -1.000E+10   2.040E+02   2.050E+02   2.060E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
     10      -1.000E+10   2.050E+02   2.060E+02   2.070E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO10 SIZE=(1,1,10,10) OFF1=(1,6)        +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO10 'ZEROES
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:07 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      2      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      3      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      4      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      5      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10   5.000E+00   6.000E+00   7.000E+00   8.000E+00
      6      -1.000E+10   2.010E+02   2.020E+02   2.030E+02   2.040E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      7      -1.000E+10   2.020E+02   2.030E+02   2.040E+02   2.050E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      8      -1.000E+10   2.030E+02   2.040E+02   2.050E+02   2.060E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      9      -1.000E+10   2.040E+02   2.050E+02   2.060E+02   2.070E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
     10      -1.000E+10   2.050E+02   2.060E+02   2.070E+02   2.080E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
fastmos INP=(FASTE1,FASTE2) OUT=FASTEO11 MMODE=OVERLAY THRESH=0 OFF1=(2,1)  +
   SIZE=(1 1 10 10)
Beginning VICAR task fastmos
 FASTMOS version 02-Oct-2012
list FASTEO11 'zeroes
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Oct  2 14:53:00 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Oct  2 14:53:07 2012
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       2.000E+02   2.010E+02   2.020E+02   2.030E+02   2.040E+02  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      2       2.010E+02   1.000E+00   2.000E+00   3.000E+00   4.000E+00  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      3       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      4       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      5       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      6       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      7      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      8      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
      9      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
     10      -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10  -1.000E+10
ush rm -f FAST?
ush rm -f FAST??
ush rm -f FAST???
ush rm -f FAST????
ush rm -f FAST?????
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$!-----------------------------------------------------------------------------
$ create tstfastmos_integer.log_solos
tstfastmos
gen FASTA1 NL=5 NS=5 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTA2 NL=5 NS=5 IVAL=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTA1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
list FASTA2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
     Samp     1       3       5
   Line
      1     200 201 202 203 204
      2     201 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO1 MMODE=AVERAGE
Beginning VICAR task fastmos
list FASTAO1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:44 2012
     Samp     1       3       5
   Line
      1     200 101 102 103 104
      2     101 102 103 104 105
      3     102 103 104 105 106
      4     103 104 105 106 107
      5     104 105 106 107 108
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO2 MMODE=MOD
Beginning VICAR task fastmos
list FASTAO2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:45 2012
     Samp     1       3       5
   Line
      1     200 101 102 103 104
      2     101 102 103 104 105
      3     102 103 104 105 106
      4     103 104 105 106 107
      5     104 105 106 107 108
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO3 MMODE=OVERLAY THRESH=3
Beginning VICAR task fastmos
list FASTAO3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:45 2012
     Samp     1       3       5
   Line
      1     200 201 202   3   4
      2     201 202   3   4   5
      3     202   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO4 MMODE=MAX     THRESH=202
Beginning VICAR task fastmos
list FASTAO4
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:46 2012
     Samp     1       3       5
   Line
      1       0   1 202 203 204
      2       1 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO5 MMODE=MIN     THRESH=2
Beginning VICAR task fastmos
list FASTAO5
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:47 2012
     Samp     1       3       5
   Line
      1     200 201   2   3   4
      2     201   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO6 OFF1=(-1,0,4,3) OFF2=(2,1)
Beginning VICAR task fastmos
list FASTAO6
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:47 2012
     Samp     1       3       5
   Line
      1       3   4   0   0   0
      2       4   5 202 203 204
      3     201 202 203 204 205
      4     202 203 204 205 206
      5     203 204 205 206 207
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
Beginning VICAR task fastmos
list FASTAO7 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:48 2012
     Samp     1       3
   Line
      1       0   0   0   0
      2       0   0   0   0
      3       0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
Beginning VICAR task fastmos
list FASTAO8
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:49 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   1   2   3   4
      2       0   0   0   0   0   1   2   3   4   5
      3       0   0   0   0   0   2   3   4   5   6
      4       0   0   0   0   0   3   4   5   6   7
      5       0   0   0   0   0   4   5   6   7   8
      6     200 201 202 203 204   0   0   0   0   0
      7     201 202 203 204 205   0   0   0   0   0
      8     202 203 204 205 206   0   0   0   0   0
      9     203 204 205 206 207   0   0   0   0   0
     10     204 205 206 207 208   0   0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO9 SIZE=(1,1,10,10)                +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
Beginning VICAR task fastmos
list FASTAO9
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:49 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   3   0
      2       0   0   0   0   0   0   0   3   4   0
      3       0   0   0   0   0   0   3   4   5   0
      4       0   0   0   0   0   0   4   5   6   0
      5       0   0   0   0   0   0   5   6   7   0
      6       0 201 202 203   0   0   0   0   0   0
      7       0 202 203 204   0   0   0   0   0   0
      8       0 203 204 205   0   0   0   0   0   0
      9       0 204 205 206   0   0   0   0   0   0
     10       0 205 206 207   0   0   0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO10 SIZE=(1,1,10,10) OFF1=(1,6)        +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
Beginning VICAR task fastmos
list FASTAO10 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:50 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   4   5   6   7
      5       0   0   0   0   0   0   5   6   7   8
      6       0 201 202 203 204   0   0   0   0   0
      7       0 202 203 204 205   0   0   0   0   0
      8       0 203 204 205 206   0   0   0   0   0
      9       0 204 205 206 207   0   0   0   0   0
     10       0 205 206 207 208   0   0   0   0   0
fastmos INP=(FASTA1,FASTA2) OUT=FASTAO11 MMODE=OVERLAY THRESH=0 OFF1=(2,1)  +
   SIZE=(1 1 10 10)
Beginning VICAR task fastmos
list FASTAO11 'zeroes
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:51 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   1   2   3   4   0   0   0   0   0
      3       1   2   3   4   5   0   0   0   0   0
      4       2   3   4   5   6   0   0   0   0   0
      5       3   4   5   6   7   0   0   0   0   0
      6       4   5   6   7   8   0   0   0   0   0
      7       0   0   0   0   0   0   0   0   0   0
      8       0   0   0   0   0   0   0   0   0   0
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
fastmos FASTA2 FASTA3
Beginning VICAR task fastmos
list FASTA3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:43 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:51 2012
     Samp     1       3       5
   Line
      1     200 201 202 203 204
      2     201 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208
gen FASTB1 NL=5 NS=5 IVAL=0    'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTB2 NL=5 NS=5 IVAL=20000  'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTB1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
     Samp       1     2     3     4     5
   Line
      1         0     1     2     3     4
      2         1     2     3     4     5
      3         2     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
list FASTB2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002 20003 20004
      2     20001 20002 20003 20004 20005
      3     20002 20003 20004 20005 20006
      4     20003 20004 20005 20006 20007
      5     20004 20005 20006 20007 20008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO1 MMODE=AVERAGE
Beginning VICAR task fastmos
list FASTBO1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
     Samp       1     2     3     4     5
   Line
      1     20000 10001 10002 10003 10004
      2     10001 10002 10003 10004 10005
      3     10002 10003 10004 10005 10006
      4     10003 10004 10005 10006 10007
      5     10004 10005 10006 10007 10008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO2 MMODE=MOD
Beginning VICAR task fastmos
list FASTBO2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:53 2012
     Samp       1     2     3     4     5
   Line
      1     20000 10001 10002 10003 10004
      2     10001 10002 10003 10004 10005
      3     10002 10003 10004 10005 10006
      4     10003 10004 10005 10006 10007
      5     10004 10005 10006 10007 10008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO3 MMODE=OVERLAY THRESH=3
Beginning VICAR task fastmos
list FASTBO3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:54 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002     3     4
      2     20001 20002     3     4     5
      3     20002     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO4 MMODE=MAX     THRESH=20002
Beginning VICAR task fastmos
list FASTBO4
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:54 2012
     Samp       1     2     3     4     5
   Line
      1         0     1 20002 20003 20004
      2         1 20002 20003 20004 20005
      3     20002 20003 20004 20005 20006
      4     20003 20004 20005 20006 20007
      5     20004 20005 20006 20007 20008
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO5 MMODE=MIN     THRESH=2
Beginning VICAR task fastmos
list FASTBO5
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:55 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001     2     3     4
      2     20001     2     3     4     5
      3         2     3     4     5     6
      4         3     4     5     6     7
      5         4     5     6     7     8
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO6 OFF1=(-1,0,4,3) OFF2=(2,1)
Beginning VICAR task fastmos
list FASTBO6
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:56 2012
     Samp       1     2     3     4     5
   Line
      1         3     4     0     0     0
      2         4     5 20002 20003 20004
      3     20001 20002 20003 20004 20005
      4     20002 20003 20004 20005 20006
      5     20003 20004 20005 20006 20007
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO7 SIZE=(1,1,3,4) OFF1=(-10,1) OFF2=(10,1)
Beginning VICAR task fastmos
list FASTBO7 'ZEROES
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:56 2012
     Samp       1     2     3     4
   Line
      1         0     0     0     0
      2         0     0     0     0
      3         0     0     0     0
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO8 SIZE=(1,1,10,10) OFF1=(1,6) OFF2=(6,1)
Beginning VICAR task fastmos
list FASTBO8
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:57 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     1     2     3     4
      2         0     0     0     0     0     1     2     3     4     5
      3         0     0     0     0     0     2     3     4     5     6
      4         0     0     0     0     0     3     4     5     6     7
      5         0     0     0     0     0     4     5     6     7     8
      6     20000 20001 20002 20003 20004     0     0     0     0     0
      7     20001 20002 20003 20004 20005     0     0     0     0     0
      8     20002 20003 20004 20005 20006     0     0     0     0     0
      9     20003 20004 20005 20006 20007     0     0     0     0     0
     10     20004 20005 20006 20007 20008     0     0     0     0     0
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO9 SIZE=(1,1,10,10)                +
        OFF1=(1,6) OFF2=(6,1) 'EDGE NSEQ=1 NTHRESH=2 NIBBLE=1
Beginning VICAR task fastmos
list FASTBO9
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:58 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     0     0     3     0
      2         0     0     0     0     0     0     0     3     4     0
      3         0     0     0     0     0     0     3     4     5     0
      4         0     0     0     0     0     0     4     5     6     0
      5         0     0     0     0     0     0     5     6     7     0
      6         0 20001 20002 20003     0     0     0     0     0     0
      7         0 20002 20003 20004     0     0     0     0     0     0
      8         0 20003 20004 20005     0     0     0     0     0     0
      9         0 20004 20005 20006     0     0     0     0     0     0
     10         0 20005 20006 20007     0     0     0     0     0     0
fastmos INP=(FASTB1,FASTB2) OUT=FASTBO10 SIZE=(1,1,10,10) OFF1=(1,6)        +
    OFF2=(6,1) 'EDGE NSEQ=2 LTHRESH=3 LNIBBLE=1 RTHRESH=2 RNIBBLE=0 NINCR=2
Beginning VICAR task fastmos
list FASTBO10 'ZEROES
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:58 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     0     0     0     0
      2         0     0     0     0     0     0     0     0     0     0
      3         0     0     0     0     0     0     0     0     0     0
      4         0     0     0     0     0     0     4     5     6     7
      5         0     0     0     0     0     0     5     6     7     8
      6         0 20001 20002 20003 20004     0     0     0     0     0
      7         0 20002 20003 20004 20005     0     0     0     0     0
      8         0 20003 20004 20005 20006     0     0     0     0     0
      9         0 20004 20005 20006 20007     0     0     0     0     0
     10         0 20005 20006 20007 20008     0     0     0     0     0
gen FASTB3 NL=5 NS=5 IVAL= -10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
fastmos FASTB3 FASTBO11 SIZE=(1,1,6,6) THRESH=-9
Beginning VICAR task fastmos
list FASTBO11
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:58 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:09:59 2012
     Samp       1     2     3     4     5     6
   Line
      1       -10    -9    -8    -7    -6   -10
      2        -9    -8    -7    -6    -5   -10
      3        -8    -7    -6    -5    -4   -10
      4        -7    -6    -5    -4    -3   -10
      5        -6    -5    -4    -3    -2   -10
      6       -10   -10   -10   -10   -10   -10
fastmos FASTB3 FASTBO12 SIZE=(1,1,6,6) THRESH=-32768 'EDGE NSEQ=1 NIBBLE=1
Beginning VICAR task fastmos
list FASTBO12
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:58 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:00 2012
     Samp       1     2     3     4     5     6
   Line
      1    -32768    -9    -8    -7-32768-32768
      2    -32768    -8    -7    -6-32768-32768
      3    -32768    -7    -6    -5-32768-32768
      4    -32768    -6    -5    -4-32768-32768
      5    -32768    -5    -4    -3-32768-32768
      6    -32768-32768-32768-32768-32768-32768
fastmos FASTB2 FASTB3
Beginning VICAR task fastmos
list FASTB3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:09:52 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:00 2012
     Samp       1     2     3     4     5
   Line
      1     20000 20001 20002 20003 20004
      2     20001 20002 20003 20004 20005
      3     20002 20003 20004 20005 20006
      4     20003 20004 20005 20006 20007
      5     20004 20005 20006 20007 20008
gen FASTC NL=10 NS=10 IVAL=0 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb FASTC FASTC1 ADD 100 (3,1,3,10,9,10,9,1)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
sargonb FASTC FASTC2 ADD 200 (2,1,2,10,8,10,8,1)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
sargonb FASTC FASTC3 ADD 120 (4,3,4,10,8,10,8,3)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
list FASTC1 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:01 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Sep 18 15:10:01 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3     100 100 100 100 100 100 100 100 100 100
      4     100 100 100 100 100 100 100 100 100 100
      5     100 100 100 100 100 100 100 100 100 100
      6     100 100 100 100 100 100 100 100 100 100
      7     100 100 100 100 100 100 100 100 100 100
      8     100 100 100 100 100 100 100 100 100 100
      9     100 100 100 100 100 100 100 100 100 100
     10       0   0   0   0   0   0   0   0   0   0
list FASTC2 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:01 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Sep 18 15:10:02 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2     200 200 200 200 200 200 200 200 200 200
      3     200 200 200 200 200 200 200 200 200 200
      4     200 200 200 200 200 200 200 200 200 200
      5     200 200 200 200 200 200 200 200 200 200
      6     200 200 200 200 200 200 200 200 200 200
      7     200 200 200 200 200 200 200 200 200 200
      8     200 200 200 200 200 200 200 200 200 200
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
list FASTC3 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:01 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Sep 18 15:10:02 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0 120 120 120 120 120 120 120 120
      5       0   0 120 120 120 120 120 120 120 120
      6       0   0 120 120 120 120 120 120 120 120
      7       0   0 120 120 120 120 120 120 120 120
      8       0   0 120 120 120 120 120 120 120 120
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
fastmos INP=(FASTC1,FASTC2,FASTC3) OUT=FASTCO1 MMODE=AVERAGE OFF2=(2,1)
Beginning VICAR task fastmos
list FASTCO1 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:01 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:03 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3     150 150 150 150 150 150 150 150 150 150
      4     150 150 140 140 140 140 140 140 140 140
      5     150 150 140 140 140 140 140 140 140 140
      6     150 150 140 140 140 140 140 140 140 140
      7     150 150 140 140 140 140 140 140 140 140
      8     150 150 140 140 140 140 140 140 140 140
      9     150 150 150 150 150 150 150 150 150 150
     10       0   0   0   0   0   0   0   0   0   0
fastmos INP=(FASTC1,FASTC2,FASTC3) OUT=FASTCO2 'MOD OFF2=(2,1)  +
        'EDGE NIBBLE=2 NSEQ=4
Beginning VICAR task fastmos
list FASTCO2 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:01 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:04 2012
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0 150 150 150 150 150 150   0   0
      4       0   0 150 150 120 120 120 120   0   0
      5       0   0 150 150 120 120 120 120   0   0
      6       0   0 150 150 120 120 120 120   0   0
      7       0   0 150 150 120 120 120 120   0   0
      8       0   0 150 150 120 120 120 120   0   0
      9       0   0 150 150 150 150 150 150   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen FASTD  NL=10 NS=10 'HALF IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
sargonb FASTD FASTD1 SUBTRACT 20000 (3,3,3,6,6,6,6,3)
Beginning VICAR task sargonb
SARGONB version 01-JUL-94
list FASTD1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:04 2012
 Task:SARGONB   User:lwk       Date_Time:Tue Sep 18 15:10:05 2012
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3-19996-19995-19994-19993     8     9    10    11
      4         3     4-19995-19994-19993-19992     9    10    11    12
      5         4     5-19994-19993-19992-19991    10    11    12    13
      6         5     6-19993-19992-19991-19990    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
gen FASTD2 NL=10 NS=10 'HALF IVAL=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD3 NL=10 NS=10 'HALF IVAL=400
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD4 NL=10 NS=10 'HALF IVAL=600
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD5 NL=10 NS=10 'HALF IVAL=800
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD6 NL=10 NS=10 'HALF IVAL=1000
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD7 NL=10 NS=10 'HALF IVAL=1200
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD8 NL=10 NS=10 'HALF IVAL=1400
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTD9 NL=10 NS=10 'HALF IVAL=1600
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTDA NL=10 NS=10 'HALF IVAL=1800
Beginning VICAR task gen
GEN Version 6
GEN task completed
fastmos INP=(FASTD1 FASTD2 FASTD3 FASTD4 FASTD5 FASTD6    +
             FASTD7 FASTD8 FASTD9 FASTDA)                 +
        OUT=FASTDO SIZE=(1 1 50 16) MMODE=OVER 'PROGRESS   +
        OFF1=(2,1)            OFF2=(1,5)                  +
        OFF3=(12,1)           OFF4=(5,5,10 10)            +
        OFF5=(22,1)           OFF6=(25,5)                 +
        OFF7=(32,1)           OFF8=(30,5)                 +
        OFF9=(42,1)           OFF10=(40,5)
Beginning VICAR task fastmos
   fastmos 10% done
   fastmos 20% done
   fastmos 30% done
   fastmos 40% done
   fastmos 50% done
   fastmos 60% done
   fastmos 70% done
   fastmos 80% done
   fastmos 90% done
list FASTDO
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:04 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     0     0     0   200   201   202   203   204   205   206   207   208   209     0
      2         0     1     2     3     4     5     6     7     8     9   207   208   209   210     0
      3         1     2     3     4     5     6     7     8     9    10   208   209   210   211     0
      4         2     3-19996-19995   203   204     8     9    10    11   209   210   211   212     0
      5         3     4-19995-19994   204   205     9    10    11    12   210   211   212   213     0
      6         4     5-19994-19993   205   206    10    11    12    13   211   212   213   214     0
      7         5     6-19993-19992   206   207    11    12    13    14   212   213   214   215     0
      8         6     7     8     9    10    11    12    13    14    15   213   214   215   216     0
      9         7     8     9    10    11    12    13    14    15    16   214   215   216   217     0
     10         8     9    10    11    12    13    14    15    16    17   215   216   217   218     0
     11         9    10    11    12    13    14    15    16    17    18   612   613   614   615     0
     12       400   401   402   403   404   405   406   407   408   409   613   614   615   616     0
     13       401   402   403   404   405   406   407   408   409   410   614   615   616   617     0
     14       402   403   404   405   406   407   408   409   410   411   615   616   617   618     0
     15       403   404   405   406   407   408   409   410   411   412     0     0     0     0     0
     16       404   405   406   407   408   409   410   411   412   413     0     0     0     0     0
     17       405   406   407   408   409   410   411   412   413   414     0     0     0     0     0
     18       406   407   408   409   410   411   412   413   414   415     0     0     0     0     0
     19       407   408   409   410   411   412   413   414   415   416     0     0     0     0     0
     20       408   409   410   411   412   413   414   415   416   417     0     0     0     0     0
     21       409   410   411   412   413   414   415   416   417   418     0     0     0     0     0
     22       800   801   802   803   804   805   806   807   808   809     0     0     0     0     0
     23       801   802   803   804   805   806   807   808   809   810     0     0     0     0     0
     24       802   803   804   805   806   807   808   809   810   811     0     0     0     0     0
     25       803   804   805   806   807   808   809   810   811   812  1006  1007  1008  1009     0
     26       804   805   806   807   808   809   810   811   812   813  1007  1008  1009  1010     0
     27       805   806   807   808   809   810   811   812   813   814  1008  1009  1010  1011     0
     28       806   807   808   809   810   811   812   813   814   815  1009  1010  1011  1012     0
     29       807   808   809   810   811   812   813   814   815   816  1010  1011  1012  1013     0
     30       808   809   810   811   812   813   814   815   816   817  1011  1012  1013  1014     0
     31       809   810   811   812   813   814   815   816   817   818  1012  1013  1014  1015     0
     32      1200  1201  1202  1203  1007  1008  1009  1010  1011  1012  1013  1014  1015  1016     0
     33      1201  1202  1203  1204  1008  1009  1010  1011  1012  1013  1014  1015  1016  1017     0
     34      1202  1203  1204  1205  1009  1010  1011  1012  1013  1014  1015  1016  1017  1018     0
     35      1203  1204  1205  1206  1207  1208  1209  1210  1211  1212  1411  1412  1413  1414     0
     36      1204  1205  1206  1207  1208  1209  1210  1211  1212  1213  1412  1413  1414  1415     0
     37      1205  1206  1207  1208  1209  1210  1211  1212  1213  1214  1413  1414  1415  1416     0
     38      1206  1207  1208  1209  1210  1211  1212  1213  1214  1215  1414  1415  1416  1417     0
     39      1207  1208  1209  1210  1211  1212  1213  1214  1215  1216  1415  1416  1417  1418     0
     40      1208  1209  1210  1211  1212  1213  1214  1215  1216  1217  1806  1807  1808  1809     0
     41      1209  1210  1211  1212  1213  1214  1215  1216  1217  1218  1807  1808  1809  1810     0
     42      1600  1601  1602  1603  1604  1605  1606  1607  1608  1609  1808  1809  1810  1811     0
     43      1601  1602  1603  1604  1605  1606  1607  1608  1609  1610  1809  1810  1811  1812     0
     44      1602  1603  1604  1605  1606  1607  1608  1609  1610  1611  1810  1811  1812  1813     0
     45      1603  1604  1605  1606  1607  1608  1609  1610  1611  1612  1811  1812  1813  1814     0
     46      1604  1605  1606  1607  1608  1609  1610  1611  1612  1613  1812  1813  1814  1815     0
     47      1605  1606  1607  1608  1609  1610  1611  1612  1613  1614  1813  1814  1815  1816     0
     48      1606  1607  1608  1609  1610  1611  1612  1613  1614  1615  1814  1815  1816  1817     0


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:04 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
     49      1607  1608  1609  1610  1611  1612  1613  1614  1615  1616  1815  1816  1817  1818     0
     50      1608  1609  1610  1611  1612  1613  1614  1615  1616  1617     0     0     0     0     0
gen FASTA13D NL=5 NS=5 NB=5 IVAL=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FASTA23D NL=5 NS=5 NB=5 IVAL=200
Beginning VICAR task gen
GEN Version 6
GEN task completed
list FASTA13D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1       0   1   2   3   4
      2       1   2   3   4   5
      3       2   3   4   5   6
      4       3   4   5   6   7
      5       4   5   6   7   8


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1       1   2   3   4   5
      2       2   3   4   5   6
      3       3   4   5   6   7
      4       4   5   6   7   8
      5       5   6   7   8   9


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1       2   3   4   5   6
      2       3   4   5   6   7
      3       4   5   6   7   8
      4       5   6   7   8   9
      5       6   7   8   9  10


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5
   Line
      1       3   4   5   6   7
      2       4   5   6   7   8
      3       5   6   7   8   9
      4       6   7   8   9  10
      5       7   8   9  10  11


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5
   Line
      1       4   5   6   7   8
      2       5   6   7   8   9
      3       6   7   8   9  10
      4       7   8   9  10  11
      5       8   9  10  11  12
list FASTA23D
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1     200 201 202 203 204
      2     201 202 203 204 205
      3     202 203 204 205 206
      4     203 204 205 206 207
      5     204 205 206 207 208


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1     201 202 203 204 205
      2     202 203 204 205 206
      3     203 204 205 206 207
      4     204 205 206 207 208
      5     205 206 207 208 209


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1     202 203 204 205 206
      2     203 204 205 206 207
      3     204 205 206 207 208
      4     205 206 207 208 209
      5     206 207 208 209 210


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5
   Line
      1     203 204 205 206 207
      2     204 205 206 207 208
      3     205 206 207 208 209
      4     206 207 208 209 210
      5     207 208 209 210 211


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5
   Line
      1     204 205 206 207 208
      2     205 206 207 208 209
      3     206 207 208 209 210
      4     207 208 209 210 211
      5     208 209 210 211 212
fastmos INP=(FASTA13D,FASTA23D) OUT=FASTAO13D MMODE=AVERAGE
Beginning VICAR task fastmos
list FASTAO13D 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:07 2012
 ***********
 Band =     1
 ***********
     Samp     1       3       5
   Line
      1     200 101 102 103 104
      2     101 102 103 104 105
      3     102 103 104 105 106
      4     103 104 105 106 107
      5     104 105 106 107 108


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:07 2012
 ***********
 Band =     2
 ***********
     Samp     1       3       5
   Line
      1     101 102 103 104 105
      2     102 103 104 105 106
      3     103 104 105 106 107
      4     104 105 106 107 108
      5     105 106 107 108 109


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:07 2012
 ***********
 Band =     3
 ***********
     Samp     1       3       5
   Line
      1     102 103 104 105 106
      2     103 104 105 106 107
      3     104 105 106 107 108
      4     105 106 107 108 109
      5     106 107 108 109 110


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:07 2012
 ***********
 Band =     4
 ***********
     Samp     1       3       5
   Line
      1     103 104 105 106 107
      2     104 105 106 107 108
      3     105 106 107 108 109
      4     106 107 108 109 110
      5     107 108 109 110 111


 Task:GEN       User:lwk       Date_Time:Tue Sep 18 15:10:06 2012
 Task:FASTMOS   User:lwk       Date_Time:Tue Sep 18 15:10:07 2012
 ***********
 Band =     5
 ***********
     Samp     1       3       5
   Line
      1     104 105 106 107 108
      2     105 106 107 108 109
      3     106 107 108 109 110
      4     107 108 109 110 111
      5     108 109 110 111 112
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
