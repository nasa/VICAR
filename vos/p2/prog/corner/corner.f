       PROGRAM  CORNER
C#######################################################################
C  NAME OF ROUTINE
C      CORNER ( find CORNERs )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program CORNER is a VICAR applications program which is used to 
C      find good locations for tiepoints in an image.  CORNER looks mostly
C      for the corners of objects in the image.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    4-86
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     1-95   CRS (CRI)   PORTED FOR UNIX
C  
C     7-86   SP  UPDATED THE METHOD FOR CALCULATING 2ND DERIVATIVES TO USE
C                PIXELS ABOUT 3 PIXELS AWAY FROM CORNER LOCATION INSTEAD OF
C                MWIDE AWAY.  THIS RESULTS IN SELECTION OF MORE DISTINCT
C                FEATURES AND STILL AVOIDS NEEDING TO INTERPOLATE.
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      CORNER
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT INTEGER (A-Z)

      INTEGER*4  ICOUNT,    IDEF,     IDX,    IDY,   INSUMS, 
     .           IPSUMS,     IRETURN,  ISTAT,   IUNIT,  JSAMP, 
     .           LINE,      MWIDE,    MWIDE2,  NL,     NS, 
     .           NTHRESH,   NTHRESHM, OUNIT, OSTAT, WSTATUS
      INTEGER    IN_PAR, MW2_PAR, OUT_PAR, IBIS, RECORD, STATUS
      PARAMETER  (IN_PAR = 300000 )
      PARAMETER  (DIM_PAR = 1000 )
      PARAMETER  (OUT_PAR = 60000 )
      PARAMETER  (MW_PAR =  50 )
      PARAMETER  (MW2_PAR = 2* MW_PAR + 1 )
      INTEGER*2  IN( IN_PAR ), DY(OUT_PAR), DYA(OUT_PAR)
      INTEGER*4  BUFP(MW2_PAR), NEXTB(0:MW_PAR), PREVB(0:MW_PAR),
     .           PERP(2*MW_PAR), STEP(MW_PAR, 0:MW_PAR),
     .           SCALE(0:MW_PAR), CORNMAX(DIM_PAR),
     .           LINEMAX(DIM_PAR), SAMPMAX(DIM_PAR)
      INTEGER*4  IBISCOL(3)
      DATA       IBISCOL /  1,2,3  /
      CHARACTER*132  PBUF
      LOGICAL    FIRST, SAMESIGN, AUTO, PRINT, OPENFLAG, XVPTST
      REAL*4     IBISDATA(3),  DENOM, SINC, CURRP
C
C======================START OF EXECUTABLE CODE======================

      CALL IFMESSAGE('CORNER Version 2-JAN-95')
      FIRST = .TRUE.

C..OPEN INPUT FILE

      CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')
      IRETURN = 1
      CALL XVSIGNAL(IUNIT, ISTAT, IRETURN)
      CALL XVOPEN(IUNIT,ISTAT, 'OP', 'READ', 'U_FORMAT','HALF',
     .               'OPEN_ACT', 'SA',  'IO_ACT', 'SA',' ')

      CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)

C..READ THE PARAMETERS.

      CALL XVPARM('WIDTH',MWIDE,ICOUNT,IDEF,1)
      MDER    = MIN( MWIDE, 3 )             ! USE PIXELS ABOUT 3 AWAY FROM
                                            ! CORNER TO FIND 2ND DERIV.
      MWIDE2  = 2 * MWIDE
      MWIDE21 = MWIDE2 + 1
      IF ( NSI * MWIDE21 .GT. IN_PAR  )  THEN
           CALL XVMESSAGE ( 'WIDTH OR NSI TOO LARGE',' ' )
           CALL ABEND
      END IF

      CALL XVPARM('GTHRESH', NTHRESH,ICOUNT,IDEF,1)
      IF (ICOUNT .GT. 0)  THEN
         NTHRESHM = NTHRESH * MWIDE 
         AUTO     = .FALSE.
      ELSE
         AUTO     = .TRUE.
      END IF

      CALL XVPARM('NAH',NAH,ICOUNT, IDEF, 1)
      CALL XVPARM('NAV',NAV,ICOUNT, IDEF, 1)
      IF (NAH .GT. DIM_PAR)
     &   CALL MABEND('ERROR: NAH TOO BIG.')

      CALL XVPARM('THRESH', ITHRESH,ICOUNT,IDEF,1)

      CALL XVPARM('BORDER', IBORD,ICOUNT,IDEF,1)
      IBORD = MAX( IBORD-MWIDE, 0 )    ! ALGORITHM RESULTS IN A MWIDE BORDER.
      ISL = SL + IBORD
      ISS = SS + IBORD
      INL = NL - 2*IBORD
      INS = NS - 2*IBORD
      IF ( INL .LT. MWIDE21 .OR. INS .LT. MWIDE21-2 )  
     .    CALL MABEND( 'ERROR: BORDER TOO BIG.' )

      PRINT = .NOT. XVPTST( 'NOPRINT' )

C..COMPUTE OFFSETS IN VARIOUS DIRECTIONS FOR USE LATER.

      DO MRATIO = 0,  MWIDE
         DO K=1, MWIDE
            STEP(K, MRATIO) = NINT( FLOAT(K*MRATIO) / FLOAT(MWIDE) )
         END DO
      END DO

C..COMPUTE SCALE FACTOR INVERSELY PROPORTIONAL TO DISTANCE SQUARED FOR USE
C..IN COMPUTING SECOND DERIVATIVES.

      DO MRATIO = 0,  MWIDE
        SCALE(MRATIO) = (10*MWIDE**2) / (MDER**2 + STEP(MDER,MRATIO)**2)
      END DO
      DENOM = 1.0 / (10.0* MWIDE**2)

C..DIVIDE THE IN ARRAY INTO MWIDE21 LINE BUFFERS.

      BUFP(1) = 1
      DO K = 2, MWIDE21
         BUFP(K) = BUFP(K-1) + NSI
      END DO

C..NO CORNERS FOUND WITHIN MWIDE OF THE EDGE OF THE IMAGE.
C..THE MAIN COMPUTATION IN THIS PROGRAM REQUIRES MWIDE21 LINES IN MEMORY
C..AT ANY TIME: THE CURRENT LINE PLUS MWIDE PREVIOUS LINES AND MWIDE NEXT 
C..LINES.  PREPARE FOR MAIN LOOP BY READING MWIDE21-1 LINES.

      DO K = 1, MWIDE21-1
         IBUFP = BUFP(K)
         IF (K .EQ. 1)  THEN
           CALL XVREAD( IUNIT, IN(IBUFP), ISTAT,'LINE', ISL,' ' )
         ELSE
           CALL XVREAD( IUNIT, IN(IBUFP), ISTAT, ' ' )
         END IF
      END DO
      IBUF = MWIDE21-1

      ICNTA= 0
      NUM = MIN( 200, INS-2*MWIDE )
      NUM4 = MAX( NUM/4, 3 )
      IBISROW = 1
      NCORNS  = 0

      ELINE = ISL + INL - 1
      ESAMP = ISS + INS - 1

C..THE IMAGE WILL BE DIVIDED INTO NAH*NAV CELLS. COMPUTE THE SIZE OF THE CELLS.

      ILV = ( INL - 2*MWIDE ) / NAV
      IF ( ILV .LE. 0 )  CALL MABEND(' ERROR: NAV TOO BIG.')
      ISH = ( INS - 2*MWIDE - 2 ) / NAH
      IF ( ISH .LE. 0 )  CALL MABEND(' ERROR: NAH TOO BIG.')


C..OPEN THE OUTPUT FILE.

      CALL XVUNIT(OUNIT,'OUT',1,OSTAT,' ')
      IRETURN = 1
      CALL XVSIGNAL(OUNIT, OSTAT, IRETURN)
      CALL IBIS_FILE_OPEN(OUNIT,IBIS,'WRITE',3,NAH*NAV,
     +                    ' ','ROW',OPENFLAG)
      IF (.NOT.OPENFLAG) CALL IBIS_SIGNAL(IBIS,OPENFLAG,1)
      CALL IBIS_RECORD_OPEN(IBIS,RECORD,' ',IBISCOL,3,
     +                         'REAL',OPENFLAG)
      IF (.NOT.OPENFLAG) CALL IBIS_SIGNAL(IBIS,OPENFLAG,1)

C..THIS IS NOT QUITE AN OUTER LINE LOOP WITH AN INNER SAMPLE LOOP.
C..WE LOOP OVER THE ROWS OF GRID CELLS.  FOR EACH ROW WE SCAN THE
C..LINES OF THE CELLS OF THAT ROW.
C..THIS PROCESSES THE LINES OF THE IMAGE IN CONSECUTIVE ORDER.

      DO IAV = 1, NAV
         IF (IAV .EQ. 1)  THEN
            LBEG = MWIDE + ISL       ! BEGINNING AND ENDING LINES FOR THIS ROW.
         ELSE 
            LBEG = LEND+1
         END IF

         IF (IAV .EQ. NAV)  THEN
            LEND = ELINE - MWIDE    ! LAST ROW GOES TO END SINCE POINTS CLOSER
         ELSE                       ! TO EDGE ARE OFTEN USED IN MOSAICKING.
            LEND = LBEG + ILV - 1
         END IF

         ICNTA =  0                 ! IF GTHRESH DEFAULTED, COMPUTE DEFAULT 
                                    ! FOR EACH ROW.
         CALL ZIA( CORNMAX, NAH )
         CALL ZIA( LINEMAX, NAH )
         CALL ZIA( SAMPMAX, NAH )

C..NOW THE LINE LOOP FOR THIS ROW.

         DO LINE = LBEG,LEND
          IBUF = IBUF+1                  ! FIND NEXT LINE BUFFER FOR NEXT READ.
          IF (IBUF .GT. MWIDE21)  IBUF = 1
          IBUFP = BUFP(IBUF)

          CALL XVREAD( IUNIT, IN(IBUFP), ISTAT, ' ')

C..CURRENT LINE IS PRECEDED BY MWIDE 'PREVIOUS' LINES AND MWIDE 'NEXT' LINES.
C..SET UP POINTERS TO THE PREVIOUS AND NEXT LINES.

          INXT = IBUF
          DO K = MWIDE, 0, -1
             NEXTB(K) = BUFP(INXT) - 1
             INXT = INXT - 1
             IF (INXT .LT. 1)  INXT = MWIDE21
          END DO

          PREVB(0) = NEXTB(0)

          DO K = 1, MWIDE
             PREVB(K) = BUFP(INXT) - 1
             INXT = INXT - 1
             IF (INXT .LT. 1)  INXT = MWIDE21
          END DO

          ICURRP = PREVB(0) + MWIDE + ISS - 1
 
C..FIRST SAMPLE LOOP, FOR COMPUTING DN DELTAs IN SAMPLE DIRECTION

         DO JSAMP = MWIDE+ISS, ESAMP-MWIDE
            ICURRP = ICURRP + 1
            IF (JSAMP .EQ. MWIDE+ISS) THEN
               IPSUMS = 0
               INSUMS = 0
               DO K = 1, MWIDE
                  IPSUMS = IPSUMS + IN( ICURRP-K )
                  INSUMS = INSUMS + IN( ICURRP+K )
               END DO
               IDY = INSUMS - IPSUMS

            ELSE
               IPSUMS = IPSUMS + IN(ICURRP-1) - IN(ICURRP-MWIDE-1)
               INSUMS = INSUMS + IN(ICURRP+MWIDE) - IN(ICURRP)
               IDY = INSUMS - IPSUMS
     
            END IF

            IDYA = IABS(IDY) 
            DY(JSAMP) = IDY
            DYA(JSAMP) = IDYA

         END DO

C..COMPUTE NTHRESHM AT BEGINNING OF IMAGE IF NTHRESH WAS DEFAULTED.

         IF (AUTO .AND. ICNTA .LT. NUM4 )  THEN
             SINC = FLOAT(INS - 2*MWIDE-1) / NUM
             ICNTA = 0
             ITOT = 0
             ICURRP = ISS + MWIDE
             CURRP  = ICURRP
             DO I = 1, NUM    !..COMPUTE THE AVERAGE OF UP TO 200 NONZERO VALUES
                IF ( DYA( ICURRP) .GT. 0 )  THEN
                   ICNTA = ICNTA + 1
                   ITOT = ITOT + DYA( ICURRP )
                END IF
                CURRP =  CURRP + SINC
                ICURRP = CURRP
             END DO

             IF (ICNTA .LT. 5)  THEN
                NTHRESH = 50
                NTHRESHM = NTHRESH * MWIDE
             ELSE 
                NTHRESHM = 1 + ( 2*ITOT ) / ICNTA    ! USE TWICE THE AVERAGE.
                NTHRESHM = MAX( NTHRESHM, 5*MWIDE )  ! IF FIRST PART OF IMAGE 
                                                     ! IS FLAT, USE NTHRESH=5.
             END IF
         END IF

         ICURRP = PREVB(0) + MWIDE + ISS

C..LOOP THROUGH THE CELLS FOR THIS LINE.

           DO IAH = 1, NAH
             IF (IAH .EQ. 1)  THEN
                SBEG = ISS+MWIDE+1  ! BEGINNING AND ENDING SAMPS FOR THIS COLUMN.
             ELSE 
                SBEG = SEND+1
             END IF

             IF (IAH .EQ. NAH)  THEN
                SEND = ESAMP-MWIDE-1
             ELSE 
                SEND = SBEG+ISH-1
             END IF

C..LOOP THROUGH THE SAMPLES FOR EACH CELL.
C..SECOND SAMPLE LOOP, TO FIND CORNERS.

             DO 1000 JSAMP = SBEG, SEND
               ICURRP = ICURRP + 1
               IDYA = DYA(JSAMP)
               IF (IDYA .LT. NTHRESHM)  GOTO 1000
               IDYAM = IDYA + MWIDE
               IF (IDYAM .LT. DYA(JSAMP-1) .OR. 
     .             IDYAM .LT. DYA(JSAMP+1) )   GOTO 1000

C..CHECK FOR EDGE OF IMAGE DATA USING THRESH PARAMETER.

               IF ( ( IN(ICURRP-MWIDE) .LT. ITHRESH ) .OR.
     .              ( IN(ICURRP+MWIDE) .LT. ITHRESH ) )  GOTO 1000
               IF ( ( IN(JSAMP+PREVB(MWIDE)) .LT. ITHRESH ) .OR.
     .              ( IN(JSAMP+NEXTB(MWIDE)) .LT. ITHRESH ) )  GOTO 1000

               IDY  = DY(JSAMP)

C..COMPUTE DN DELTA IN LINE (VERTICAL) DIRECTION

               IPSUML = 0
               INSUML = 0
               DO K = 1, MWIDE
                  IPSUML = IPSUML + IN( PREVB(K)+JSAMP )
                  INSUML = INSUML + IN( NEXTB(K)+JSAMP )
               END DO

               IDX = INSUML - IPSUML
               IDXA = IABS(IDX)

               ICURR = IN(ICURRP)

               IF (IDXA .LT. NTHRESHM)  THEN
                  MCURR = MWIDE * ICURR
                  MN = IABS( MCURR - INSUML)
                  MP = IABS( MCURR - IPSUML)
                  IF ( MAX(MN,MP) .LT. NTHRESHM ) GOTO 1000
               END IF

C..SAMPLE THE IMAGE ALONG THE LINE PERPENDICULAR TO THE DN GRADIENT.

               IF ( (IDX .GE. 0 .AND. IDY .GE. 0) .OR.
     .              (IDX .LE. 0 .AND. IDY .LE. 0)  ) THEN
                   SAMESIGN = .TRUE.
               ELSE
                   SAMESIGN = .FALSE.
               END IF

               IF (IDYA .GE. IDXA)  THEN
                  MRATIO = (MWIDE*IDXA + IDYA/2) / IDYA

                  IF (SAMESIGN)  THEN
                   DO K = 1, MWIDE
                    PERP(K) = IN( PREVB(K)+JSAMP+STEP(K,MRATIO) )
                    PERP(K+MWIDE) = IN( NEXTB(K)+JSAMP-STEP(K,MRATIO) )
                   END DO

                  ELSE
                   DO K = 1, MWIDE
                    PERP(K) = IN( PREVB(K)+JSAMP-STEP(K,MRATIO) )
                    PERP(K+MWIDE) = IN( NEXTB(K)+JSAMP+STEP(K,MRATIO) )
                   END DO

                  END IF

               ELSE
                  MRATIO = (MWIDE*IDYA + IDXA/2) / IDXA

                  IF (SAMESIGN)  THEN
                   DO K = 1, MWIDE
                    PERP(K) = IN( PREVB(STEP(K,MRATIO)) +JSAMP+K )
                    PERP(K+MWIDE) = IN( NEXTB(STEP(K,MRATIO))+JSAMP-K)
                   END DO

                  ELSE
                   DO K = 1, MWIDE
                    PERP(K) = IN( PREVB(STEP(K,MRATIO)) +JSAMP-K )
                    PERP(K+MWIDE) = IN( NEXTB(STEP(K,MRATIO))+JSAMP+K)
                   END DO

                  END IF
               END IF

C..NOT A CORNER IF NOT A MIN AND NOT A MAX ON LINE PERP TO GRADIENT.

               IF (ICURR .EQ. PERP(1) ) THEN
                  GOTO 1000

               ELSE IF (ICURR .GT. PERP(1) ) THEN
                  DO K = 2, MWIDE2
                     IF (ICURR .LE. PERP(K) )  GOTO 1000
                  END DO

               ELSE 
                  DO K = 2, MWIDE2
                     IF (ICURR .GE. PERP(K) )  GOTO 1000
                  END DO

               END IF
 
               IDERIV2 = SCALE(MRATIO) * 
     .                   IABS( PERP(MDER)+PERP(MDER+MWIDE)-2*ICURR )

               IF (IDERIV2 .GT. CORNMAX(IAH))  THEN
                  CORNMAX(IAH) = IDERIV2
                  LINEMAX(IAH) = LINE
                  SAMPMAX(IAH) = JSAMP
               END IF

1000        CONTINUE

           END DO      ! END OF IAH LOOP
         END DO        ! END OF LINE LOOP

         IF (FIRST .AND. PRINT)  THEN
               CALL XVMESSAGE( ' ',' ')
               CALL XVMESSAGE( 'C O R N E R    L O C A T I O N S',' ' )
               CALL XVMESSAGE( ' ',' ')
               CALL XVMESSAGE( ' ROW  COL    LINE  SAMP  QUALITY',' ' )
               CALL XVMESSAGE( ' ',' ')
               FIRST = .FALSE.
         END IF

C..OUTPUT THE BEST CORNER LOCATION FOR EACH RECTANGLE IN THE ROW.

         DO IAH = 1, NAH

            IBISDATA(1) = LINEMAX(IAH)
            IBISDATA(2) = SAMPMAX(IAH)
            IBISDATA(3) = CORNMAX(IAH) * DENOM
            CALL IBIS_RECORD_SET(RECORD,'ROW',IBISROW, STATUS)
            CALL IBIS_RECORD_WRITE(RECORD,IBISDATA,0 ,WSTATUS)
            IF (WSTATUS.NE.1) CALL IBIS_SIGNAL(IBIS,WSTATUS,1)
            IBISROW = IBISROW + 1
            IF (LINEMAX(IAH) .GT. 0)  NCORNS = NCORNS + 1

            IF (PRINT) THEN
             PBUF='                                         '
	     WRITE(PBUF(1:4), '(I4)') IAV
	     WRITE(PBUF(6:9), '(I4)') IAH
	     WRITE(PBUF(12:17), '(I6)') LINEMAX(IAH)
	     WRITE(PBUF(18:23), '(I6)') SAMPMAX(IAH)
	     WRITE(PBUF(25:32), '(F8.1)') IBISDATA(3)
             CALL XVMESSAGE( PBUF,' ')
            END IF
         END DO
      END DO

      IF (PRINT)  THEN
         PBUF='OUTPUT FILE CONTAINS         CORNERS. '
         WRITE(PBUF(22:28), '(I7)' ) NCORNS
         CALL XVMESSAGE( PBUF, ' ' )
         PBUF='GTHRESH =XXXXX.0'
         IF (AUTO) THEN
           WRITE(PBUF(10:16),'(F7.1)') NTHRESHM/FLOAT(MWIDE) 
         ELSE
           WRITE(PBUF(10:14),'(I5)') NTHRESH 
         END IF
         CALL XVMESSAGE( PBUF,' ')
      END IF

      CALL XVCLOSE(IUNIT,ISTAT,' ')
      CALL IBIS_FILE_CLOSE(IBIS,'UDELETE',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
      RETURN
      END
