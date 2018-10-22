       PROGRAM  FLOT
C#######################################################################
C  NAME OF ROUTINE
C      FLOT ( FLip or rOTate )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program FLOT is a VICAR applications program which is used to flip or 
C      rotate an image.  
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    AUGUST 1984
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C
C  ORIGINAL FLOT PROGRAM BY
C      T. C. RINDFLEISCH with modifications by GARY YAGI
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     8-84  SP   CONVERTED FROM IBM VICAR VERSION: CHANGED TO USE VICAR 2
C                (XVREAD...) ROUTINES. MISCELLANEOUS CLEANUP.
C     9-84  SP   IMPOSED LIMIT OF 200000 TO STACKA FOR ROTATE TO PREVENT
C                PAGING UNDER WORKING SET LIMIT OF 200000.
C     9-84  SP   ADDED ROT180 KEYWORD TO PROVIDE 180 DEGREE ROTATION MODE.
C     4-91  REA  CONVERTED TO UNIX/VICAR
C     2-98  REA  ADDED 3-D FILE CAPABILITY
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
C  PURPOSE
C      MAIN44 processes parameters entered by user and calls routines
C      that operate (flip, rotate,...) on the image.
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     AUGUST 1984
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C      SUN4 SunOS/UNIX  with TAE v4.1/VICAR
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      FLOT
C
C*   27 JAN 77   ... GMY ...  INITIAL RELEASE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT INTEGER(A-Z)

      LOGICAL XVPTST                       ! DECLARES XVPTST A LOG. FUNCTION.
      CHARACTER*3 ORG
      EXTERNAL FLIP,ROTATE

      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE, NBANDS

C
C=================START OF EXECUTABLE CODE===============================

C          DCODE=1 FOR BYTE DATA,       =2 FOR HALFWORD
C               =4 FOR FULL OR REAL*4,  =8 FOR REAL*8 OR COMPLEX
C          MODE=0 FOR 180 DEGREE ROTATION
C              =1 FOR VERTICAL FLIP
C              =2 FOR HORIZONTAL FLIP
C              =3 FOR CLOCKWISE ROTATION
C              =4 FOR COUNTER CLOCKWISE ROTATION
C              =5 FOR TRANSPOSE MATRIX

      MODE = 3              ! DEFAULT IS CLOCKWISE.

C  OPEN INPUT FILE

      CALL XVUNIT( INFILE, 'INP', 1, IND, ' ')
      CALL XVOPEN( INFILE, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA', ' ')
      CALL XVSIZE( ISL, ISSAMP, NL, NSAMP, NLIN, NSIN)       ! SIZE PARAMETERS.
      CALL XVGET(INFILE,IND,'PIX_SIZE',DCODE,'NB',NBANDS,'ORG',ORG,' ')
C
      IF (ORG .EQ. 'BIP') THEN
	 CALL XVMESSAGE('FLOT does not support BIP format',' ')
	 CALL ABEND
      ENDIF
C
      IF ( XVPTST('ROT180') )         MODE = 0
      IF ( XVPTST('VERT') )           MODE = 1
      IF ( XVPTST('HORIZ') )          MODE = 2
      IF ( XVPTST('CLOCK') )          MODE = 3
      IF ( XVPTST('COUNTER') )        MODE = 4
      IF ( XVPTST('TRANS') )          MODE = 5
C
      IF ( MODE .LT. 3 )  THEN
           NLO = NL                   ! OUTPUT IMAGE SIZE.
           NSO = NSAMP
      ELSE
           NLO = NSAMP
           NSO = NL
      END IF

C  OPEN OUTPUT FILE.

      CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ')
      CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'U_NL', NLO,'U_NS', NSO,
     .             'U_ORG','BSQ','OPEN_ACT', 'SA', 'IO_ACT', 'SA', ' ')

C  USE STACKA TO ALLOCATE BUFFER AND CALL APPROPRIATE ROUTINE.

      IF ( MODE .LT. 3 )   THEN
           NBYT = NSAMP * DCODE                     ! HORIZONTAL OR
           CALL STACKA(4, FLIP, 1, 2*NBYT, NBYT )   ! VERTICAL FLIP.
      ELSE
           NBYT = NSAMP * DCODE * ( NL+1 )          ! CLOCKWISE, COUNTERCLOCK.,
           NBYT = MIN0( NBYT, 200000 )                         ! (AVOID PAGING.)
           CALL STACKA(3, ROTATE, 1, NBYT )         ! OR TRANSPOSE.
      END IF

      RETURN
      END
C******************************************************************************
      SUBROUTINE ROTATE(BUF,NBUF)
C ROUTINE TO ROTATE A PICTURE 90 DEGREES CLOCKWISE OR COUNTER-CLOCKWISE
C OR TRANSPOSE

      IMPLICIT INTEGER(A-Z)
      LOGICAL*1 BUF(*)

      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE, NBANDS

C
C=================START OF EXECUTABLE CODE===============================
C
      DO IBAND = 1,NBANDS
C
C          OBI = OUTPUT BUFFER INDEX
C          LI = LINE INCREMENT
C          SI = SAMPLE INCREMENT
C          NLB = NUMBER OF LINES IN OUTPUT BUFFER
C          NPASS = NUMBER OF PASSES THROUGH INPUT PICTURE

          BUFL = NBUF / DCODE                ! BUFFER LENGTH IN PIXELS.
          NLB =  MIN0( BUFL/(NSO+1),  NLO )
          NPASS = ( NSAMP + NLB -1 ) / NLB

C   BUF BUFFER IS ORGANIZED AS FOLLOWS:
C
C             NLB*DCODE bytes as a XVREAD buffer
C                 |||||||
C
C                 |  |  |  |   NLB lines with NSO pixels per line.
C                 |  |  |  |   Each column in this rectangle is
C                 |  |  |  |   loaded from the XVREAD buffer by 
C                 |  |  |  |   one call to MVE.
C                 |  |  |  |
C                 |  |  |  |
C                 |  |  |  |
C

          IF ( MODE .EQ. 3 )  THEN       ! CLOCKWISE LOOP CONTROL VALUES **
               OBI = (NLB+NSO-1)*DCODE + 1
               LI = NSO
               SI = -DCODE
               ISS = ISSAMP
               ISSINC = NLB
          ELSE IF ( MODE .EQ. 4 ) THEN   !COUNTERCLOCKWISE LOOP CONTROL VALUES**
               OBI = (NLB+ (NLB-1)*NSO )*DCODE + 1
               LI = -NSO
               SI = DCODE
               ISS = ISSAMP + NSAMP - NLB
               ISSINC = -NLB
          ELSE                           ! **TRANSPOSE MATRIX**
               OBI = (NLB)*DCODE + 1
               LI = NSO
               SI = DCODE
               ISS = ISSAMP
               ISSINC = NLB
          END IF
C
          NREM = MOD( NSAMP, NLB )
          IF ( NREM .EQ. 0 )   NREM = NLB
          NLB1 = NLB*DCODE + 1
C
          DO  IPASS = 1, NPASS
C
              IPT = OBI
              JPT = NLB1
              IF (IPASS .EQ. NPASS) THEN  ! DO JUST REMAINING LINES ON LAST PASS
                  NLB = NREM
                  IF (MODE .EQ. 4)  THEN
                      ISS = ISSAMP
                      IPT = (NLB + (NLB-1)*NSO ) * DCODE  + 1
                      JPT = NLB * DCODE + 1
                  END IF
              END IF
C
              DO  L = 1, NL
                  LINE = ISL + L - 1
                  CALL XVREAD(INFILE,BUF,IND,'BAND',IBAND,'LINE',LINE,
     +                        'SAMP',ISS,'NSAMPS',NLB,' ')
                  CALL MVE(DCODE, NLB, BUF, BUF(IPT), 1, LI )
                  IPT = IPT + SI
              END DO
C
              DO  L = 1, NLB
                  CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ')
                  JPT = JPT + NSO*DCODE
              END DO
C
              ISS = ISS + ISSINC
          END DO
C
      END DO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FLIP(BUF,NBUF, NBYT)
C          ROUTINE TO FLIP A PICTURE ON VERTICAL OR HORIZONTAL AXIS
C          OR ROTATE BY 180 DEGREES.
      IMPLICIT INTEGER(A-Z)
      LOGICAL*1 BUF(*)
C
      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE, NBANDS

C
C=================START OF EXECUTABLE CODE===============================

      IF(NBUF .LT. 2*NBYT ) THEN
	 CALL XVMESSAGE(' **INSUFFICIENT SPACE--STACKA',' ')
	 CALL ABEND
      ENDIF
      IPT = 2*NBYT - DCODE + 1

C          IF HORIZONTAL, LINC = 1
C          IF VERTICAL, LINC = -1

      DO IBAND=1,NBANDS
      
          IF ( MODE .EQ. 0)  THEN              ! ROTATE 180 DEGREES
               JPT = 1 + NBYT
               LINC = -1
               REC  = ISL + NL - 1
          ELSE IF ( MODE .EQ. 1)  THEN         ! VERT
               JPT = 1
               LINC = -1
               REC  = ISL + NL - 1
          ELSE
               JPT = 1 + NBYT                  ! HORIZ
               LINC = 1
               REC  = ISL 
          END IF
C
          DO L=1,NL
              CALL XVREAD(INFILE,BUF,IND,'BAND',IBAND,'LINE',REC,
     +                    'SAMP',ISSAMP,'NSAMPS',NSAMP,' ')
              REC = REC + LINC
              IF(MODE.EQ.2 .OR. MODE .EQ. 0) 
     .                       CALL MVE(DCODE,NSAMP,BUF,BUF(IPT),1,-1)
              CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ')
          END DO
      END DO
C
      RETURN
      END
