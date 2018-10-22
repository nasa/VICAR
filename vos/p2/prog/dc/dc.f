C    5 SEP 94    ...AS...    (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C         *******    SUBROUTINE DC   *******
C   10 OCT 88    ...SP...    CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 
C                            USES ONLY THE FORMAT IN LABEL.
C   11 MAY 84    ...HBD...   CONVERT FOR VAX and to vicar2 format
C   22 OCT 79    ...JAM...   INITIAL RELEASE
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
C
C          LOCAL VARIABLES
C  STATUS -- OPEN AND CLOSE STATUS INDICATOR
C  INUNIT -- CONTAINS UNIT NUMBERS FOR INPUT DATASETS
C  NOLINE -- CONTAINS THE NUMBER OF LINES FOR EACH INPUT DATASET
C  NOSAMP -- CONTAINS THE NUMBER OF SAMPLES FOR EACH INPUT DATASET
C  REFIN  -- STARTING BYTE OF EACH PIX IN INPUT BUFFER (INBUF in DCB)
C
      INTEGER NOLINE(10), NOSAMP(10), REFIN(10), INUNIT(10)
      INTEGER STATUS, NUMIPT, NUMOPT, I, INDEX
      CHARACTER*10 LBUF(10)
C
      CHARACTER*25 MSG1

      COMMON /C1/ NOSAMP,NOLINE,REFIN,NUMIPT,NUMOPT,INUNIT

      CALL IFMESSAGE('DC version 5-SEP-94')
      CALL XVEACTION('SA',' ')
C--- Zero out buffers
      MSG1(1:25)='NUMBER OF INPUTS NOT GT 1'
      DO I = 1, 10
         REFIN(I) = 0
         NOLINE(I) = 0
         NOSAMP(I) = 0
      END DO
      CALL XVP('INP',LBUF,NUMIPT)
      CALL XVP('OUT',LBUF,NUMOPT)
      IF (NUMIPT .LE. 1) THEN
         CALL XVMESSAGE(MSG1,' ')
         CALL ABEND
      ENDIF
      INDEX = 1
C
C--- Open input datasets
      DO I = 1,NUMIPT
         REFIN(I) = INDEX
         CALL XVUNIT(INUNIT(I),'INP',I,STATUS,' ')
	 CALL XVOPEN(INUNIT(I),STATUS,'U_FORMAT','HALF',' ')
	 CALL XVGET(INUNIT(I),STATUS,'NL',NOLINE(I),'NS',NOSAMP(I),' ')
         INDEX = NOSAMP(I)+INDEX
      END DO

      CALL DCB

      RETURN
      END


      SUBROUTINE DCB
C
      IMPLICIT NONE
C
      INTEGER*2 INBUF(10000)
      INTEGER*2 OBUF1(50000),OBUF2(50000),OBUF3(50000),OBUF4(50000)
      INTEGER NOLINE(10),NOSAMP(10),REFIN(10),NUMIPT,NUMOPT,INUNIT(10)
      INTEGER OUTUNIT(4),CNT,DEF, STATUS, I, J, LINE, IPIXSIZ
      INTEGER SLO,SSO,NLO,NSO,MAXDN,OSAMP,ISAMP,DN,NLI,NSI
      INTEGER INT,IREC,ISIGN,ISLOPE,XPNTST
      REAL*4 EXPO(10),ACONST,CONST,DELTA,DENOM,DENOM2,SLOPE,SCALE
      REAL*4 X,X2,XY,Y,RMAXDN,RINT,YPOINT,PDN,RDN,TDELTA, XPOINT
      CHARACTER*4 FORMAT(2)
      LOGICAL ISWTCH, NEGA
      LOGICAL XVPTST

      COMMON /C1/ NOSAMP,NOLINE,REFIN,NUMIPT,NUMOPT,INUNIT
      DATA FORMAT/'WORD','BYTE'/
C
C--- Get SIZE parameters
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IREC = SLO

C--- Test for halfword and negative DC picture
      CALL XVGET(INUNIT(1), STATUS, 'PIX_SIZE', IPIXSIZ,' ')  !BYTES PER PIXEL.
      IF ( IPIXSIZ .NE. 1 .AND. IPIXSIZ .NE. 2 )
     .     CALL MABEND('ERROR:INP. IMAGE IS NOT BYTE OR HALF FORMAT')

      ISWTCH = IPIXSIZ .EQ. 2
      ISIGN = 1
      NEGA = XVPTST('NEGATIVE')
      IF (NEGA) ISIGN = -1

C--- Determine MAXDN value
      CALL XVPARM('MAXDN',MAXDN,CNT,DEF,1)
      IF (ISWTCH .AND. DEF .NE. 0) MAXDN = 32767
      RMAXDN = MAXDN
      CONST = ISIGN / 2.0

C--- Get EXPOSURE times
      CALL XVP('EXPOSURE',EXPO,CNT)
      IF (CNT .EQ. 0) THEN
	 CALL XVMESSAGE('DC - Exposure values required.',' ')
	 CALL ABEND
      END IF
      DO I = 1, NUMIPT
	 X = X + EXPO(I)
	 X2 = X2 + EXPO(I) * EXPO(I)
      END DO
      DENOM = NUMIPT * X2 - X * X

C--- Get scaling factor
      CALL XVP('SCALE',SCALE,CNT)
      DENOM2 = DENOM / SCALE
      CALL XVP('X',XPOINT,XPNTST)
      CALL XVP('CONS',ACONST,CNT)

C--- Open output data sets
      DO I = 1, NUMOPT
	 CALL XVUNIT(OUTUNIT(I),'OUT',I,STATUS,' ')
	 CALL XVOPEN(OUTUNIT(I),STATUS,'OP','WRITE',' ')
      END DO
      DO 400 LINE = 1, NLO
         DO J = 1, NUMIPT
	    CALL XVREAD(INUNIT(J),INBUF(REFIN(J)),STATUS,'SAMP',SSO,
     &		       'NSAMPS',NSO,'LINE',IREC,' ')
         END DO
         IREC = IREC + 1
         OSAMP = 0

C--- This part does linear fit (DN = SLOPE * EXPO + INT)
         DO 200 ISAMP = 1, NSO
            OSAMP = OSAMP + 1
            Y = 0.0
            XY = 0.0
            DO I = 1, NUMIPT
               DN = INBUF(REFIN(I)+ISAMP-1)
               Y = Y + DN
               XY = XY + DN * EXPO(I)
            END DO
            RINT = FLOAT(ISIGN) * ((Y * X2 - X * XY) / DENOM)
            INT = RINT + CONST + ACONST
            IF (ISIGN .EQ. -1) INT = INT + 1
            IF (INT .LT. 1) INT = 0
            IF (INT .GT. MAXDN) INT=MAXDN
            OBUF1(OSAMP) = INT

C--- Calculate SLOPE picture
            IF (2 .LE. NUMOPT) THEN
               SLOPE = ((NUMIPT * XY - X * Y) / DENOM2)
               ISLOPE = SLOPE + 0.5
               IF (ISLOPE .LT. 1) ISLOPE = 0
               IF (ISLOPE .GT. MAXDN) ISLOPE = MAXDN
               OBUF2(OSAMP) = ISLOPE
            ELSE
               GOTO 200
            END IF

C--- Find predicted Y value
            IF (XPNTST .EQ. 0) GO TO 250
            IF (3 .LE. NUMOPT) THEN
               YPOINT = SLOPE * XPOINT + RINT
               IF (YPOINT .LT. 1.0) YPOINT=0.0
               IF (YPOINT .GT. RMAXDN) YPOINT = RMAXDN
               OBUF3(OSAMP) = YPOINT + .5
            ELSE
               GOTO 200
            END IF

C--- Find point which deviates the most from fitted line
  250       IF (4 .LE. NUMOPT) THEN
               DELTA = 0.0
               DO I = 1, NUMIPT                  ! Calculate predicted DN (PDN)
                  PDN = SLOPE * EXPO(I) + RINT
                  RDN = INBUF(REFIN(I)+ISAMP-1)  ! RDN is actual DN
                  TDELTA = ABS(PDN-RDN)
                  IF (TDELTA .GT. DELTA) DELTA = ABS(RDN-PDN)
               END DO
               IF (XPNTST .EQ. 0) THEN
		  OBUF3(OSAMP) = DELTA + ACONST
               ELSE 
		  OBUF4(OSAMP) = DELTA + ACONST
   	       END IF
            END IF
  200    CONTINUE

C--- Write out buffers to files
         CALL XVWRIT(OUTUNIT(1),OBUF1,STATUS,' ')
         IF (NUMOPT .EQ. 2) THEN
             CALL XVWRIT(OUTUNIT(2),OBUF2,STATUS,' ')
         ELSE IF (NUMOPT .EQ. 3) THEN
             CALL XVWRIT(OUTUNIT(2),OBUF2,STATUS,' ')
             CALL XVWRIT(OUTUNIT(3),OBUF3,STATUS,' ')
         ELSE IF (NUMOPT .EQ. 4) THEN
             CALL XVWRIT(OUTUNIT(2),OBUF2,STATUS,' ')
             CALL XVWRIT(OUTUNIT(3),OBUF3,STATUS,' ')
             CALL XVWRIT(OUTUNIT(4),OBUF4,STATUS,' ')
         END IF

C--- Close all files
  400 CONTINUE
      DO I = 1, NUMIPT
        CALL XVCLOSE(INUNIT(I),STATUS,' ')
      END DO
      DO I = 1, NUMOPT
         CALL XVCLOSE(OUTUNIT(I),STATUS,' ')
      END DO
      RETURN
      END
