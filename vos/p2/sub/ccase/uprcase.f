      SUBROUTINE UPRCASE(STR)
c   Short cut for converting whole argument string to upper case.

      CHARACTER*(*) STR

      CALL CCASE( STR, 1, -1)  ! -1 means convert the whole string
      RETURN
      END
