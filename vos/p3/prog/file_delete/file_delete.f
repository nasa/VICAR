      INCLUDE 'VICMAIN_FOR'
C
C  98-01-13   ...rea... Original Version
C
      SUBROUTINE MAIN44
C
      CHARACTER*80 FILENAME(30)
      CHARACTER*140 MSG
C
      CALL XVPARM('INP',FILENAME,NINP,IDEF,30)
      DO I=1,NINP
          OPEN (UNIT=66,FILE=FILENAME(I),IOSTAT=ISTAT,STATUS='OLD')
          IF (ISTAT .EQ. 0) THEN
              CLOSE (UNIT=66, STATUS='DELETE')
          ELSE
              WRITE (MSG,100) FILENAME(I)
  100         FORMAT('*** Unable to open/delete file: ',A80)
              CALL XVMESSAGE(MSG,' ')
          END IF
      END DO
      RETURN
      END
