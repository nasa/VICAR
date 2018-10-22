      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER NAH,NAH2, NAV,NAV2, NPTS,NPTS2, I,J,K, COLCOUNT, 
     .        STATUS, UNIT,UNIT2
      REAL LINE,SAMP, LINC, SINC, TIE1(100), TIE2(100)
      CHARACTER*132 PBUF
C==================================================================
      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ')
      CALL XVSIGNAL(UNIT, STATUS, 1)     ! ABORT IF ERROR

C  TEST1 FOR ITIEPOINT : GRIDDED SET OF TIEPOINTS
      NAH = 2
      NAV = 3
      COLCOUNT = 4
      LINC = 10.0
      SINC = 5.0
      K = 1

      DO I=1,NAV+1
         IF (I .EQ. 1) THEN
             LINE = 40.0
         ELSE
             LINE = LINE + LINC
         END IF

         DO J=1,NAH+1
            IF (J .EQ. 1) THEN
             SAMP = 10.0
            ELSE
             SAMP = SAMP + SINC
            END IF

            TIE1(K)   = LINE
            TIE1(K+1) = SAMP
            TIE1(K+2) = LINE+1.5
            TIE1(K+3) = SAMP+2.5
            K         = K+4

         END DO
      END DO

      CALL IWRITE_TIEPOINTS(UNIT,NAH,NAV,0,TIE1,COLCOUNT)  !WRITE TO A FILE.

C...NOW READ THE TIEPOINT DATA FROM THE FILE INTO A DIFFERENT ARRAY.

      CALL XVUNIT(UNIT,'OUT',1,STATUS,' ') ! DO NOT ASSUME UNIT IS STILL THERE.
      CALL XVSIGNAL(UNIT, STATUS, 1)     ! ABORT IF ERROR
      NAV2 = 0
      NAH2 = 0
      CALL IREAD_TIEPOINTS(UNIT,NAH2,NAV2,100,TIE2,COLCOUNT)  !READ FROM A FILE.

C...NOW CHECK THAT NO DATA WAS LOST IN THE MOVE.

      IF (NAH .NE. NAH2)  CALL MABEND('ERROR in NAH2 value')
      IF (NAV .NE. NAV2)  CALL MABEND('ERROR in NAV2 value')

      K = 0
      DO I = 1, (NAH+1)*(NAV+1)
         IF ( TIE2(I) .NE. TIE1(I) ) THEN
            K = K+1
            WRITE (PBUF,9000) I, TIE2(I), TIE1(I)
9000        FORMAT ('ERROR on tiepoint value',I3, 2F10.6)
            CALL XVMESSAGE(PBUF,' ')
         END IF
      END DO

      IF (K .EQ. 0) THEN
         CALL XVMESSAGE('SUCCESS ON TEST 1',' ')
      ELSE
         CALL XVMESSAGE('FAILURE ON TEST 1',' ')
      END IF

C test 1a:  set maxpts to 1.

      TIE2(1) = 0.0
      TIE2(5) = 0.0
      CALL IREAD_TIEPOINTS(UNIT,NAH2,NAV2,1,TIE2,COLCOUNT)  !READ FROM A FILE.
      IF ( TIE2(1) .eq. TIE1(1) .and. TIE2(5) .EQ. 0.0) THEN
         CALL XVMESSAGE('SUCCESS ON TEST 1a',' ')
      ELSE
         CALL XVMESSAGE('FAILURE ON TEST 1a',' ')
      END IF

C  TEST2 FOR ITIEPOINT : NON-GRIDDED SET OF TIEPOINTS

      CALL XVUNIT(UNIT2,'OUT',2,STATUS,' ')
      CALL XVSIGNAL(UNIT2, STATUS, 1)     ! ABORT IF ERROR


      NPTS = 5
      COLCOUNT = 4
      LINC = 10.0
      SINC = 5.0
      K = 1

      DO I=1,NPTS
         IF (I .EQ. 1) THEN
             LINE = 4.0
             SAMP = 1.0
         ELSE
             LINE = LINE + LINC
             SAMP = SAMP + SINC
         END IF

         TIE1(K)   = LINE
         TIE1(K+1) = SAMP
         TIE1(K+2) = LINE+1.5
         TIE1(K+3) = SAMP+2.5
         K         = K+4

      END DO

      CALL IWRITE_TIEPOINTS(UNIT2,0,0,NPTS, TIE1,COLCOUNT)  !WRITE TO A FILE.

C...NOW READ THE TIEPOINT DATA FROM THE FILE INTO A DIFFERENT ARRAY.

      CALL XVUNIT(UNIT2,'OUT',2,STATUS,' ') ! DO NOT ASSUME UNIT IS STILL THERE.
      CALL XVSIGNAL(UNIT2, STATUS, 1)     ! ABORT IF ERROR
      NPTS2 = 0
      CALL IREAD_TIEPOINTS(UNIT2,NAH2,NPTS2,100,TIE2,COLCOUNT) !READ FROM A FILE

C...NOW CHECK THAT NO DATA WAS LOST IN THE MOVE.

      IF (NAH2 .NE. 0)    CALL MABEND('ERROR in NAH2 value')
      IF (NPTS .NE. NPTS2)  CALL MABEND('ERROR in NPTS2 value')

      K = 0
      DO I = 1, NPTS
         IF ( TIE2(I) .NE. TIE1(I) ) THEN
            K = K+1
            WRITE (PBUF,9000) I, TIE2(I), TIE1(I)
            CALL XVMESSAGE(PBUF,' ')
         END IF
      END DO

      IF (K .EQ. 0) THEN
         CALL XVMESSAGE('SUCCESS ON TEST 2',' ')
      ELSE
         CALL XVMESSAGE('FAILURE ON TEST 2',' ')
      END IF


      CALL XVMESSAGE(
     . 'Repeat a test case in C to test C interface:', ' ')

      call tzitiepoint(TIE1,NPTS,TIE2 )

      return
      END

