      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE XORV                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL XVMESSAGE('Output values for this test should be 128',' ')
      CALL PARTA    !FOR DCODE > 0

      CALL XVMESSAGE(' ',' ')
      CALL PARTB    !FOR DCODE < 0

      RETURN
      END

      SUBROUTINE PARTA    !DCODE > 0

      include  'fortport'  ! DEFINES INT2BYTE

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B1(5), B2(5)        !BYTE ARRAYS
      INTEGER*2  H1(5), H2(5)        !HALFWORD ARRAYS
      INTEGER*4  F1(5), F2(5)        !FULLWORD ARRAYS
      INTEGER    DCODE(6) /1,2,3,4,5,6/
C--LABEL CONSTANTS
      CHARACTER  F*4  /'FULL'/
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 128+K1
         B1(I) = INT2BYTE(K1)
         B2(I) = INT2BYTE(K2)
         H1(I) = K1
         H2(I) = K2
         F1(I) = K1
         F2(I) = K2
      ENDDO

      CALL XORV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD,' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(3), 5, B1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(3), B, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(5), 5, B1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(5), B, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(6), 5, H1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(6), H, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO


      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
      END

      SUBROUTINE PARTB    !DCODE < 0

      include  'fortport'  ! DEFINES INT2BYTE

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B1(5), B2(5)        !BYTE ARRAYS
      INTEGER*2  H1(5), H2(5)        !HALFWORD ARRAYS
      INTEGER*4  F1(5), F2(5)        !FULLWORD ARRAYS
      REAL*4     R1(5), R2(5)        !REAL ARRAYS
      REAL*8     D1(5), D2(5)        !DOUBLE WORD ARRAYS
      INTEGER    DCODE(6)   /-1,-2,-3,-4,-5,-6/
C--LABEL CONSTANTS
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/
      CHARACTER  F*4  /'FULL'/

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 128+I
         B1(I) = INT2BYTE(K1)
         B2(I) = INT2BYTE(K2)
         H1(I) = K1
         H2(I) = K2
         F1(I) = K1
         F2(I) = K2
         R1(I) = K1
         R2(I) = K2
         D1(I) = R1(I)
         D2(I) = R2(I)
      ENDDO

      CALL XORV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(3), 5, H1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(3), H, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B1
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 128+I
         ENDDO

      CALL XORV(DCODE(5), 5, F1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(5), F, B, ( BYTE2INT(B2(I)), I=1,5)
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 128+I
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL XORV(DCODE(6), 5, F1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(6), F, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 128+I
         ENDDO


      CALL TZXORV      ! TEST C INTERFACE

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
      END
