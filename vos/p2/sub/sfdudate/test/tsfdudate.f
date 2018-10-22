       SUBROUTINE TSFDUDATE()
       INTEGER*4 SYEAR,HR,HOUR,I,YEAR
       INTEGER*4 FRAC_SEC,SFDUT1,SFDUT2,SS_1950,SECOND,MSEC
       INTEGER*2 SDR(3),SDR2(3)
       CHARACTER*100 MSG

       CALL XVMESSAGE('Gross check through entire century',' ')
       CALL XVMESSAGE(' ',' ')
       DO 25 I=0,2
        IF (I.EQ.0) HR=0
        IF (I.EQ.1) HR=1
        IF ((I.NE.0).AND.(I.NE.1)) HR=8759
        CALL XVMESSAGE(' ',' ')
        WRITE(MSG,1000) HR
        CALL XVMESSAGE(MSG,' ')
 1000      FORMAT('Hour of year = ', I)
        CALL XVMESSAGE(' ',' ')
        CALL XVMESSAGE('ss_sec,fsec,year,hour,sec,msec',' ')
        CALL XVMESSAGE(' ',' ')

        DO 50 YEAR=0,99
         SDR(1)=HR+24
         SDR(2)=3500
         SDR(3)=890
         CALL SFDUDATE(SFDUT1,SFDUT2,SDR,YEAR)
         CALL SDRDATE(SFDUT1,SFDUT2,SDR2,SYEAR)
         SS_1950 = SFDUT1
         FRAC_SEC = SFDUT2
         HOUR = SDR(1)
         SECOND = SDR(2)
         MSEC = SDR(3)
         WRITE(MSG,1500)SS_1950,FRAC_SEC,YEAR,HOUR,SECOND,MSEC
         CALL XVMESSAGE(MSG,' ')
 1500          FORMAT(6I12)
         IF (YEAR.NE.SYEAR.OR.HOUR.NE.SDR2(1).OR.
     c   SECOND.NE.SDR2(2).OR.MSEC.NE.SDR2(3)) THEN
          HOUR = SDR2(1)
          SECOND = SDR2(2)
          MSEC = SDR2(3)
          WRITE(MSG,1500)SS_1950,FRAC_SEC,SYEAR,HOUR,SECOND,MSEC
          CALL XVMESSAGE(MSG,' ')
         ENDIF
   50  CONTINUE
   25  CONTINUE

       CALL XVMESSAGE('Check through each day of VGR Uranus period',' ')
       CALL XVMESSAGE(' ',' ')
       DO 125 YEAR=85,88
       WRITE(MSG,2000)YEAR
       CALL XVMESSAGE(' ',' ')
       CALL XVMESSAGE(MSG,' ')    
       CALL XVMESSAGE(' ',' ')
 2000  FORMAT('Year = ', I)
       DO 150 HR=0,8783,24
       SDR(1)=HR+24
       SDR(2)=3500
       SDR(3)=890
       IF (YEAR.NE.88.AND.HR.EQ.8760) GO TO 125
       CALL SFDUDATE(SFDUT1,SFDUT2,SDR,YEAR)
       CALL SDRDATE(SFDUT1,SFDUT2,SDR2,SYEAR)
       SS_1950 = SFDUT1
       FRAC_SEC = SFDUT2
       HOUR = SDR(1)
       SECOND = SDR(2)
       MSEC = SDR(3)
       WRITE(MSG,1500)SS_1950,FRAC_SEC,YEAR,HOUR,SECOND,MSEC
       CALL XVMESSAGE(MSG,' ')
       IF (YEAR.NE.SYEAR.OR.HOUR.NE.SDR2(1).OR.
     c  SECOND.NE.SDR2(2).OR.MSEC.NE.SDR2(3)) THEN
         HOUR = SDR2(1)
         SECOND = SDR2(2)
         MSEC = SDR2(3)
         WRITE(MSG,1500)SS_1950,FRAC_SEC,SYEAR,HOUR,SECOND,MSEC
         CALL XVMESSAGE(MSG,' ')
       ENDIF
  150  CONTINUE
  125  CONTINUE
       END
