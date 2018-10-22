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
