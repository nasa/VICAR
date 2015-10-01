$!****************************************************************************
$!
$! Build proc for MIPL module corner
$! VPACK Version 1.9, Monday, December 07, 2009, 16:04:26
$!
$! Execute by entering:		$ @corner
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
$ write sys$output "*** module corner ***"
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
$ write sys$output "Invalid argument given to corner.com file -- ", primary
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
$   if F$SEARCH("corner.imake") .nes. ""
$   then
$      vimake corner
$      purge corner.bld
$   else
$      if F$SEARCH("corner.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake corner
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @corner.bld "STD"
$   else
$      @corner.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create corner.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack corner.com -mixed -
	-s corner.f -
	-i corner.imake -
	-p corner.pdf -
	-t tstcorner.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create corner.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create corner.imake
#define  PROGRAM corner

#define MODULE_LIST corner.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create corner.pdf
process help=*
!  PDF FILE FOR CORNER
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM WIDTH   TYPE=INTEGER  COUNT=1       DEFAULT=4  VALID=(1:50)
PARM GTHRESH TYPE=INTEGER  COUNT=(0:1)   DEFAULT=-- VALID=(1:32767)
PARM NAH     TYPE=INTEGER  COUNT=1       DEFAULT=5  VALID=(1:1000)
PARM NAV     TYPE=INTEGER  COUNT=1       DEFAULT=5  VALID=(1:1000)
PARM THRESH  TYPE=INTEGER  COUNT=1       DEFAULT=1  VALID=(0:32767)
PARM PRINT   TYPE=KEYWORD  COUNT=1       DEFAULT=PRINT VALID=(PRINT,NOPRINT)
PARM BORDER  TYPE=INTEGER  COUNT=1       DEFAULT=16  VALID=(0:9999)
!
END-PROC
.TITLE
VICAR Program CORNER
.HELP
PURPOSE

Program CORNER is a VICAR/IBIS applications program which is used to 
find good locations for tiepoints in an image.  CORNER looks 
for the corners of objects or features in the image.

EXECUTION

Program CORNER looks for corners of objects or features in the input image.  
A corner is defined as a bend or curve in an edge.  An edge is defined as a 
contour line of brightness (DN) having a large brightness gradient relative
to neighboring contour lines.  

Program CORNER scans the image looking for edge pixels.  When an edge pixel is
found, CORNER checks if the edge is bent at that pixel.  This is done by 
sampling the image along the line perpendicular to the gradient and checking
for a local minimum or maximum at the pixel that was found.  If there is such
a local minimum or maximum, CORNER considers the pixel to be a corner.  The
sharpness or quality of the corner is measured by the absolute value of the
second derivative of brightness as a function of position along the line
perpendicular to the gradient.  (The first derivative is 0 at a local 
minimum or maximum.)  The scanning algorithm is designed more for speed than
thoroughness and consequently does not find all corners of features.

CORNER divides the input image into NAV rows (and NAH columns) of rectangular
cells.  CORNER uses the portion of the input image defined by the SIZE
parameter, minus any border specified by the BORDER  parameter.  For each
rectangular cell, CORNER finds the corner of greatest quality. 

The algorithm used for finding corners has two parameters that can be used to 
adjust the program's response to noise in the image and to features or objects
of differing sizes.  The GTHRESH parameter is used roughly as a gradient
magnitude threshold for edge detection.  If GTHRESH is defaulted, the program 
automatically computes a threshold based on the brightness variation in the
image.  The WIDTH parameter determines the size of the interval over which the
gradient and second derivative are computed.  The larger the WIDTH parameter,
the less the program pays attention to noise and fine detail.

The data type of the input image may either be byte or halfword data.  The data
type is obtained from the VICAR label of the input image. 

The output file is an IBIS interface file.  (See the DCL HELP on IBISFIL.)
This IBIS interface file has three columns, all with length NAH*NAV.  Each
column is stored in REAL*4 format.  The IBIS interface file has one row for
each rectangular cell.  The first two columns contain the line and sample
coordinates for the corner of greatest quality in the cell.  The third
columns contain the quality of the corners.  The quality is approximately the
absolute value of the second derivative of brightness.  This value is not 
normalized, but the larger the quality, the more distinct the corner.
If no corner is found in a given rectangular cell,  all three values are 0.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:

      corner INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters.
      corner INP=a OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters.
      corner INP=a OUT=b (sl,ss,nl,ns) optional parameters.
      corner INP=a OUT=b optional parameters.

       Here 'a' represents the input image file name,
       and 'b' represents the output IBIS interface file name.
.PAGE
EXAMPLES

1.    corner INP=A OUT=B WIDTH=10 BORDER=32

      In this example CORNER divides the image into 25 (the default)
      rectangular pieces and finds the best corner location in each piece.
      All locations found will be more than 32 pixels from the edge of the
      image so that these locations can be used for FFT correlation by another
      program.  The WIDTH parameter is set so that image noise and most 
      features much less than 10 pixels in diameter will be ignored.

.PAGE
 RESTRICTIONS
1. The input image must be byte or halfword data.
2. The maximum number of pixels per line is 60000.  The number of pixels 
   per line times (2*WIDTH + 1) must not exceed 300,000.
3. The maximum number for NAH or NAV is 1000.
4. The WIDTH parameter must be in the range 1 to 50.

 WRITTEN BY:             Steve Pohorsky              16 May 1986

 PORTED TO UNIX BY:	 Randy Schenk (CRI)           2 Jan 1995

 COGNIZANT PROGRAMMER:   Steve Pohorsky              16 May 1986

 REVISION:               1                           16 May 1986
.LEVEL1
.VARIABLE INP
Input image.
.VARIABLE OUT
Output IBIS interface file name.
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE WIDTH
Width of interval used for
corner finding.
.VARIABLE GTHRESH
Gradient threshold used for
corner finding.
.VARIABLE NAH
Number of columns of rectangles
to divide image into.
.VARIABLE NAV
Number of rows of rectangles
to divide image into.
.VARIABLE THRESH
Threshold for finding edge of
imge data.
.VARIABLE BORDER
Border width.
.VARIABLE PRINT
Enter 'NOPRINT for no printed
output.
.LEVEL2
.VARIABLE SIZE
CORNER uses the portion of the input image defined by the SIZE
parameter, minus any border specified by the BORDER  parameter.  
If the SIZE field is not entered, the image size is obtained from the
VICAR label.
.VARIABLE WIDTH
The WIDTH parameter determines the size of the interval over which the
gradient and second derivative are computed.  The larger the WIDTH parameter,
the less the program pays attention to noise and fine detail.  CORNER ignores
most features that have a diameter much less than the WIDTH parameter.
In the initial search for edge pixels, CORNER looks through a line for a pixel
for which the average brightness of the preceding M pixels differs from 
the average brightness of the succeeding M pixels by at least the GTHRESH
value, where M is the WIDTH value.  The default value for WIDTH is 4.
.VARIABLE GTHRESH
The GTHRESH parameter is used roughly as a gradient magnitude threshold for
edge detection.  If GTHRESH is defaulted, the program automatically computes a
threshold based on the brightness variation in the image. 
In the initial search for edge pixels, CORNER looks through a line for a pixel
for which the average brightness of the preceding M pixels differs from 
the average brightness of the succeeding M pixels by at least the GTHRESH
value, where M is the WIDTH value.  

This parameter can have a major effect on the number of corner locations
found.  For images with very low contrast, it may be more reliable to
explicitly specify a small value for GTHRESH, such as 5.
.VARIABLE NAH
CORNER divides the input image into NAV rows (and NAH columns) of rectangular
cells.  For each rectangular cell, CORNER finds the corner of greatest quality.
The default for NAH is 5.
.VARIABLE NAV
CORNER divides the input image into NAV rows (and NAH columns) of rectangular
cells.  For each rectangular cell, CORNER finds the corner of greatest quality.
The default for NAV is 5.
.VARIABLE THRESH
In some applications, only non-zero pixels are considered to be image data.
In such cases, the default value of 1 for THRESH can be used.  CORNER considers
any pixel with a DN less than THRESH to not be image data.  This parameter is
used so that the edge of image data will be ignored during the corner search.
Use THRESH=0 if 0 is to be included as image data.  (Negative halfword values
are always considered to not be image data by CORNER.)
.VARIABLE BORDER
CORNER uses the portion of the input image defined by the SIZE
parameter, minus any border specified by the BORDER  parameter.  
All corner locations found will be more than the BORDER value of pixels 
from the edge of the image so that these locations can be used for 
FFT correlation by another program.  
.VARIABLE PRINT
The default is for CORNER
 to print a list of corner locations found.
'NOPRINT supresses printed output.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstcorner.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
!
!  THIS IS A TEST OF PROGRAM corner
!
!      byte image - first test defaults.
!
gen corner1 NL=50 NS=50 IVAL=128 LINC=0 SINC=1
qsar corner1 cornera AREA=(  18,20,6,12, 255,  28,20,7,10,255  40,20,8,8,-255)
corner INP=cornera OUT=cornerao 
!LIST cornerao 'REAL4 'ZERO NS=30
!TEST NOTE: The original test pdf used LIST to list the contents of cornerao
!           LIST does not work for (new) IBIS files.
!           Use ibis-list as shown below:
ibis-list cornerao
!
!      try some parameters
!
corner cornera corner4 WIDTH=2 THRESH=0 BORDER=0 +
                       NAH=6 NAV=9
!
!      try some parameters
!
corner cornera corner3 SIZE=(25,5,25,30) WIDTH=1 GTHRESH=10 BORDER=0
!
!      halfword image - first test defaults.
!
gen corner1 NL=50 NS=50 IVAL=128 LINC=0 SINC=1 'half
qsar corner1 cornera AREA=(  18,20,6,12, 255,  28,20,7,10,255  40,20,8,8,-255)
corner INP=cornera OUT=cornerao 
!LIST cornerao 'REAL4 'ZERO NS=30
! REFERENCE  the TEST NOTE above for use of ibis-list instead of LIST
ibis-list cornerao
!
!      try some parameters
!
corner cornera corner4 WIDTH=2 THRESH=0 BORDER=0 +
                       NAH=6 NAV=9
!
!      try some parameters
!
corner cornera corner3 SIZE=(25,5,25,30) WIDTH=1 GTHRESH=10 BORDER=0
!    clean up
!
END-PROC
$ Return
$!#############################################################################
