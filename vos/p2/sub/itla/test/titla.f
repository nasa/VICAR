C TEST SUBROUTINE itla
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE ITLA     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      BYTE     B(25)                !THE BYTE BUCKET

      DO I=1,16                     !LOOP OVER 16 FILL VALUES
         CALL ITLA(I*16, B, 25)        !FILL THE BYTE ARRAY WITH (I)
                                       ! last time tests error handling
                                       ! Should abend.
         CALL PRNT(1,25,B,' ARRAY B:.')
      ENDDO
      RETURN
      END
