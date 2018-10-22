      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS IS A TEST FOR MODULE MULV                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL XVMESSAGE(' ',' ')
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
      REAL*4     R1(5), R2(5)        !REAL ARRAYS
      REAL*8     D1(5), D2(5)        !DOUBLE WORD ARRAYS
      INTEGER    DCODE(9) /1,2,3,4,5,6,7,8,9/
C--LABEL CONSTANTS
      CHARACTER  R*4  /'REAL'/
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/
      CHARACTER  F*4  /'FULL'/
      CHARACTER  D*4  /'DOUB'/ 

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 2
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

      CALL MULV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL MULV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD,' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL MULV(DCODE(3), 5, B1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(3), B, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL MULV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL MULV(DCODE(5), 5, B1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(5), B, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL MULV(DCODE(6), 5, H1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(6), H, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL MULV(DCODE(7), 5, R1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(7), R, R, R2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES R2
            R2(I) = 2
         ENDDO

      CALL MULV(DCODE(8), 5, D1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(8), D, D, D2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES D2
            D2(I) = 2
         ENDDO

      CALL MULV(DCODE(9), 5, R1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(9), R, D, D2
      CALL XVMESSAGE(CARD, ' ')

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
 110  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5F5.1)
      END

      SUBROUTINE PARTB    !DCODE < 0

      include  'fortport'  ! DEFINES INT2BYTE

      CHARACTER  CARD*80             !PRINT BUFFER
      BYTE       B1(5), B2(5)        !BYTE ARRAYS
      INTEGER*2  H1(5), H2(5)        !HALFWORD ARRAYS
      INTEGER*4  F1(5), F2(5)        !FULLWORD ARRAYS
      REAL*4     R1(5), R2(5)        !REAL ARRAYS
      REAL*8     D1(5), D2(5)        !DOUBLE WORD ARRAYS
      INTEGER    DCODE(9)   /-1,-2,-3,-4,-5,-6,-7,-8,-9/
C--LABEL CONSTANTS
      CHARACTER  R*4  /'REAL'/
      CHARACTER  B*4  /'BYTE'/
      CHARACTER  H*4  /'HALF'/
      CHARACTER  F*4  /'FULL'/
      CHARACTER  D*4  /'DOUB'/ 

      DO I=1,5          !INITIALIZE THE ARRAYS
         K1    = I
         K2    = 2
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

      CALL MULV(DCODE(1), 5, B1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(1), B, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL MULV(DCODE(2), 5, H1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(2), H, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL MULV(DCODE(3), 5, H1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(3), H, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B1
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL MULV(DCODE(4), 5, F1, F2, 1, 1)
      WRITE( CARD, 100) DCODE(4), F, F, F2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES F2
            F2(I) = 2
         ENDDO

      CALL MULV(DCODE(5), 5, F1, B2, 1, 1)
      WRITE( CARD, 100) DCODE(5), F, B, B2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES B2
            K2    = 2
            B2(I) = INT2BYTE(K2)
         ENDDO

      CALL MULV(DCODE(6), 5, F1, H2, 1, 1)
      WRITE( CARD, 100) DCODE(6), F, H, H2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES H2
            H2(I) = 2
         ENDDO

      CALL MULV(DCODE(7), 5, R1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(7), R, R, R2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES R2
            R2(I) = 2
         ENDDO

      CALL MULV(DCODE(8), 5, D1, D2, 1, 1)
      WRITE( CARD, 110) DCODE(8), D, D, D2
      CALL XVMESSAGE(CARD, ' ')
         DO I=1,5      !RESTORE OLD VALUES D2
            D2(I) = 2
         ENDDO

      CALL MULV(DCODE(9), 5, D1, R2, 1, 1)
      WRITE( CARD, 110) DCODE(9), D, R, R2
      CALL XVMESSAGE(CARD, ' ')


      CALL TZMULV      ! TEST C INTERFACE

      RETURN
 100  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5I5)
 110  FORMAT( 'DCODE=', I5, 2X, A4, 2X, A4, 2X, 5F5.1)
      END
