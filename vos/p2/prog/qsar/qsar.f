	PROGRAM  QSAR
C#######################################################################
C  NAME OF ROUTINE
C      QSAR ( Quadrilateral Segment Averaging Routine )
C             Quadrilateral means rectangle here. SAR part is a misnomer.
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program QSAR is used to add or subtract values from the data numbers in
C      rectangular sections of an image. 
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    OCTOBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ORIGINAL QSAR PROGRAM BY
C      HOWARD FRIEDEN with modifications by JOEL MOSHER
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      QSAR INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      QSAR INP=a OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      QSAR a b (sl,ss,nl,ns) optional parameters
C      QSAR a b optional parameters
C
C       Here 'a' represents the input image file name,
C       'b' represents the output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file name.
C      OUT    - Output file name.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C      AREA   - Specifies the rectangles and the values to be added to data 
C               numbers.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The input and output images must be byte or halfword data.
C      2. The maximum number of pixels per line is 64000.
C      3. The maximum number of rectangles that can be specified is 120,
C         as of this writing.
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
C      MAIN44 is used to add or subtract values from the data numbers in
C      rectangular sections of an image. 
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     OCTOBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    09-03  NTT  ENABLED FOR 3D IMAGES
C    04-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C    07-85  JHR  CONVERTED TO VICAR2
C    10-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    10-83  SP   INCREASED MAXIMUM LINE SIZE TO 64000 PIXELS.
C    10-83  SP   CHANGED LABEL PROCESSING OF INPUT FILE BY USING LABELC
C                TO FIND THE DATA FORMAT AND USING FORMAT IN LABEL AS DEFAULT.
C    10-83  SP   CHANGED HALF PARAMETER TO FORMAT=HALF/BYTE SO USER CAN
C                OVERRIDE LABEL WHEN LABEL SAYS HALF.
C    10-83  SP   CHANGED LABEL PROCESSING OF OUTPUT FILE TO ALWAYS INCLUDE 
C                CORRECT DATA FORMAT IN LABEL. 
C    10-83  SP   MODIFIED PROGRAM SO THAT STARTING PIXEL VALUE IN THE  AREA
C                PARAMETER REFERS TO POSITION IN INPUT IMAGE EVEN WHEN THE
C                STARTING SAMPLE IN THE SIZE FIELD IS NOT 1. (THE STARTING
C                LINE VALUE IN THE  AREA PARAMETER ALREADY WAS SET TO REFER
C                TO THE POSITION IN INPUT IMAGE EVEN WHEN THE
C                STARTING LINE IN THE SIZE FIELD IS NOT 1. )
C    10-83  SP   CORRECTED PROGRAM TO NOT STORE VARIABLE INT INTO IBUF ARRAY
C                WHEN THE VALUE OF INT HAD NOT BEEN SET PROPERLY ( I.E. WHEN
C                LOOP INDEX K WAS NOT IN RANGE.)
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM QSAR.
C      
C  CALLED BY
C      QSAR
C  SUBROUTINES CALLED
C      The library routines  ABEND, XVOPEN, XVCLOSE, XVPARM,  
C                            XVREAD, XVWRIT, XVUNIT, XVGET
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C * 27 JUNE 1975...DAH...CHANGES FOR CONVERSION TO 360/OS
C *   25 JULY 79   ...JBS...   MAX LINE LENGTH TO 4000 BYTES
C *   5 JUNE 80   ...JAM...   ADD HALFWORD OPTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   THIS PROGRAM FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.
	IMPLICIT NONE
	INTEGER*4 LSIZE_PAR
      PARAMETER ( LSIZE_PAR = 64000 )      ! MAX NUMBER OF PIXELS PER LINE.
      INTEGER*4 OUNIT,STAT,SS,SLO,SSO,IPARM(1800),ES,ADJ,SBO,NBO,NBI,LBO
      INTEGER*4 BANDOUT,LINEOUT,BAND,LINE,IUNIT,I,NLO,NSO,NLI,NSI,ICOUNT
      INTEGER*4 IDEF,J,K,INT,LLO
      CHARACTER*48 ERR3
      INTEGER*2 IBUF(LSIZE_PAR)
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN
      DATA ERR3/' NUMBER OF PARAMETERS NOT A MULTIPLE OF FIVE'/

      CALL IFMESSAGE('QSAR version 14 Feb 2010 (64-bit) - RJB')
      CALL XVEACTION('SA',' ')

C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')		!import data as HALF

C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE('??E - QSAR ACCEPTS BYTE AND HALFWORD DATA ONLY',' ')
         CALL ABEND
      END IF

C     CHECK FORMAT
      CALL XVGET(IUNIT,I,'ORG',ORGIN, ' ')			!IUN to IUNIT - 14Feb2010
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  '??E - BIP files not supported, use program TRAN to convert to BSQ')

C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF(SLO+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE(
     & '??E - NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      IF(SSO+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE(
     & '??E - NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      CALL XVBANDS(SBO,NBO,NBI)

      IF ( SBO .GT. NBO ) CALL MABEND(
     +  '??E - SB is greater than the total number of bands')
                 
      IF ( SBO + NBO - 1 .GT. NBI) THEN
        CALL XVMESSAGE('??W - Number of bands truncated', ' ')
        NBO = NBI + 1 - SBO
      ENDIF

      

C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')

C        PROCESS PARAMETERS
      CALL XVPARM('AREA',IPARM,ICOUNT,IDEF,1800)
      IF(MOD(ICOUNT,5).NE.0) THEN
         CALL XVMESSAGE(ERR3,' ')			!CALL XVMESSAGE(ERR3,44,' ')
         CALL ABEND
      END IF

      LLO=SLO+NLO-1                 ! LAST LINE.
      LBO=SBO+NBO-1                 ! LAST BAND.

C  COPY THE INPUT IMAGE TO OUTPUT MAKING THE REQUESTED MODIFICATIONS.
      BANDOUT=0
      DO 105 BAND=SBO,LBO
        BANDOUT=BANDOUT+1
        LINEOUT=0
        DO 100 LINE=SLO,LLO
         LINEOUT=LINEOUT+1
         CALL XVREAD(IUNIT,IBUF,STAT,'LINE',LINE,'BAND',BAND,' ')

         IF(ICOUNT.EQ.0) GO TO 55      ! IF NO MODIFICATIONS, JUST COPY.

         DO 50 J = 1,ICOUNT,5
C              CHECK FOR MODS TO THIS LINE
            IF(LINE.LT.IPARM(J).OR.LINE.GE.IPARM(J)+IPARM(J+2)) GO TO 50
            SS=IPARM(J+1)                ! STARTING PIXEL.
            IF(SS.LT.1) THEN
               CALL XVMESSAGE('??E - INVALID SS PARAMETER SPECIFIED',' ')
               CALL ABEND
            END IF
            ES=IPARM(J+3)+SS-1           ! ENDING PIXEL.
            IF(ES.GT.NSI) THEN
               CALL XVMESSAGE('??E - INPUT SAMPLE SIZE EXCEEDED',' ')
               CALL ABEND
            END IF
            ADJ=IPARM(J+4)               ! ADJUSTMENT TO BE ADDED.
C 
            DO K=SS,ES
               INT=IBUF(K)+ADJ
               IF(FORMAT.EQ.'HALF')  THEN 
                  IF(INT.LT.-32768) INT=-32768
                  IF(INT.GT.32767)  INT= 32767
               ELSE 
                  IF (INT.LT.0) INT=0
                  IF (INT.GT.255) INT=255
               END IF 
               IBUF(K)=INT
            END DO

50       CONTINUE

55       CALL XVWRIT(OUNIT,IBUF(SSO),STAT,'NSAMPS',NSO,'BAND',BANDOUT,
     +        'LINE',LINEOUT,' ')
100     CONTINUE
105   CONTINUE

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN        
      END
