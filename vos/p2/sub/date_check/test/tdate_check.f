      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      IMPLICIT NONE

      INTEGER*4 SCET_DATE(6), STD_DATE(7), COUNT, STATUS
      LOGICAL XVPTST

      CALL XVP ('SCET', SCET_DATE, COUNT)
      IF (COUNT .EQ. 6) THEN
         CALL CHK_SCET_DATE (SCET_DATE, STATUS)
         IF (STATUS .EQ. 1) THEN
            CALL XVMESSAGE ('CHK_SCET_DATE: Valid date', 0)
         ELSE
            CALL XVMESSAGE ('CHK_SCET_DATE: Invalid date',0)
         END IF

         IF (XVPTST('DEBUG')) THEN
            CALL XVMESSAGE ('--- Check All Fields ---', 0)

            CALL CHK_YEAR (SCET_DATE(1), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_YEAR: Valid year', 0)
            ELSE
               CALL XVMESSAGE ('CHK_YEAR: Invalid year', 0)
            END IF

            CALL CHK_JULIAN (SCET_DATE(1), SCET_DATE(2), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_JULIAN: Valid julian day', 0)
            ELSE
               CALL XVMESSAGE ('CHK_JULIAN: Invalid julian day', 0)
            END IF

            CALL CHK_HOUR (SCET_DATE(3), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_HOUR: Valid hour number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_HOUR: Invalid hour number', 0)
            END IF

            CALL CHK_MINUTE (SCET_DATE(4), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_MINUTE: Valid minute number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_MINUTE: Invalid minute number', 0)
            END IF

            CALL CHK_SECOND (SCET_DATE(5), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_SECOND: Valid second number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_SECOND: Invalid second number', 0)
            END IF

            CALL CHK_MSEC (SCET_DATE(6), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Valid millisecond number', 0)
            ELSE
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Invalid millisecond number', 0)
            END IF
         END IF

      END IF

      CALL XVP ('STD', STD_DATE, COUNT)
      IF (COUNT .EQ. 7) THEN
         CALL CHK_STD_DATE (STD_DATE, STATUS)
         IF (STATUS .EQ. 1) THEN
            CALL XVMESSAGE ('CHK_STD_DATE: Valid date', 0)
         ELSE
            CALL XVMESSAGE ('CHK_STD_DATE: Invalid date', 0)
         END IF

         IF (XVPTST('DEBUG')) THEN
            CALL XVMESSAGE ('--- Check All Fields ---', 0)
 
            CALL CHK_YEAR (STD_DATE(1), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_YEAR: Valid year', 0)
            ELSE
               CALL XVMESSAGE ('CHK_YEAR: Invalid year', 0)
            END IF
 
            CALL CHK_MONTH (STD_DATE(2), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_MONTH: Valid month', 0)
            ELSE
               CALL XVMESSAGE ('CHK_MONTH: Invalid month', 0)
            END IF

            CALL CHK_DAY (STD_DATE(1), STD_DATE(2), STD_DATE(3), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_DAY: Valid day', 0)
            ELSE
               CALL XVMESSAGE ('CHK_DAY: Invalid day', 0)
            END IF
 
            CALL CHK_HOUR (STD_DATE(4), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_HOUR: Valid hour number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_HOUR: Invalid hour number', 0)
            END IF
 
            CALL CHK_MINUTE (STD_DATE(5), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_MINUTE: Valid minute number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_MINUTE: Invalid minute number', 0)
            END IF
 
            CALL CHK_SECOND (STD_DATE(6), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE ('CHK_SECOND: Valid second number', 0)
            ELSE
               CALL XVMESSAGE ('CHK_SECOND: Invalid second number', 0)
            END IF
 
            CALL CHK_MSEC (STD_DATE(7), STATUS)
            IF (STATUS .EQ. 1) THEN
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Valid millisecond number', 0)
            ELSE
               CALL XVMESSAGE 
     &            ('CHK_MSEC: Invalid millisecond number', 0)
            END IF
         END IF

      END IF

      RETURN
      END

