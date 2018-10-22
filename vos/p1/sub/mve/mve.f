      SUBROUTINE MVE(DCODE,N,A,B,INCA,INCB)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                               MVE
C       ----------------                                               ---
C	General routine for transferring arrays.
C	Fortran format of call:
C
C	CALL MVE(DCODE, N, A, B, INCA, INCB)
C
C	Parameters:-
C
C	DCODE......Transfer mode
C	           1  = Move byte to byte
C                  2  = Move halfword to halfword
C                  3  = Move byte to halfword
C                  4  = Move fullword to fullword
C                  5  = Move byte to fullword
C                  6  = Move halfword to fullword
C                  7  = Move real (single) to real.
C                  8  = Move double to double.
C                  9  = Move real to double
C	           negative values -1 to -9 reverse of above.
C	N.........No of elements to transfer
C       A.........Source vector to be transferred
C       B.........Destination vector.
C       INCA......Source address increament 
C	INCB......Dest address increment 
C
C   REVISION HISTORY
C
C 92-03-30 ...SP.... Changed to use fortport.fin as improved portability
C                    method.  Added special case code for inca=1=incb.
C 89-11-22 ...SP.... Adapted FORTRAN version from J. Mosher to use ZEXT for
C                    speed and sysarch.fin (to handle byte-swap) for 
C                    portability.
C                    ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.
C 85-10-8  ...GMY... change back to MVE, numerous changes
C 84-7-21  ...LWK... fixed bug for negative numbers in DCODE=6
C 83-3-22  ... changed to umve, MVE fortran to handle quoted
C		string arguments --LWK
C 83-2-3: revised to allow 5 arguments - LWK
C UCL routine  --  used for VICAR1, 82-12.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       INTEGER*4 DCODE,INCA,INCB,A(*),B(*),N
       CHARACTER*80 CBUF
C==================================================================
      IF(DCODE.EQ.1.OR.DCODE.EQ.-1) CALL MVE1 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.2.OR.DCODE.EQ.-2) CALL MVE2 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.3)                CALL MVE3P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-3)               CALL MVE3N(N,A,B,INCA,INCB)
      IF(DCODE.EQ.4.OR.DCODE.EQ.-4) CALL MVE4 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.5)                CALL MVE5P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-5)               CALL MVE5N(N,A,B,INCA,INCB)
      IF(DCODE.EQ.6)                CALL MVE6P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-6)               CALL MVE6N(N,A,B,INCA,INCB)
      IF(DCODE.EQ.7.OR.DCODE.EQ.-7) CALL MVE7 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.8.OR.DCODE.EQ.-8) CALL MVE8 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.9)                CALL MVE9P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-9)               CALL MVE9N(N,A,B,INCA,INCB)
      IF(DCODE.GT.9.OR.DCODE.LT.-9 .OR. DCODE .EQ. 0)THEN
        WRITE( CBUF,90101) DCODE
90101   FORMAT(' MVE---DCODE OUT OF RANGE,DCODE=', I12)
        CALL XVMESSAGE( CBUF, ' ')
      ENDIF
      RETURN
      END
      SUBROUTINE MVE1(N,A,B,INCA,INCB)   !BYTE IN , BYTE OUT
      BYTE A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       
       II=1
       JJ=1
       DO k=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE2(N,A,B,INCA,INCB)!HALF IN , HALF OUT
      INTEGER*2 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       
       II=1
       JJ=1
       DO I=1,N
          B(JJ)=A(II)
          II=II+INCA
          JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE3P(N,A,B,INCA,INCB)! BYTE IN , HALF OUT
      include 'fortport'

      BYTE A(*)
      INTEGER*2 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=BYTE2INT( A(K) )
       ENDDO
      ELSE       
       II=1
       JJ=1
       DO I=1,N
         B(JJ)= BYTE2INT( A(II) )
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
       RETURN
      END IF
      END
      SUBROUTINE MVE3N(N,A,B,INCA,INCB) !HALF IN , BYTE OUT

      include  'fortport'  ! defines INT2BYTE.

      INTEGER*2 A(*)
      BYTE B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        IF ( A(K) .GT. 255 ) THEN
           B(K) = 255
        ELSE IF ( A(K) .LT. 0 ) THEN
           B(K) = 0
        ELSE
           B(K)= INT2BYTE( A(K) )
        END IF
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        IF ( A(II) .GT. 255 ) THEN
           B(JJ) = 255
        ELSE IF ( A(II) .LT. 0 ) THEN
           B(JJ) = 0
        ELSE
           B(JJ)= INT2BYTE( A(II) )
        END IF
        II=II+INCA
        JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE4(N,A,B,INCA,INCB) !FULLWORD IN , FULLWORD OUT
      INTEGER*4 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE5P(N,A,B,INCA,INCB) !BYTE IN ,FULLWORD OUT

      include  'fortport'  ! defines BYTE2INT.

      BYTE A(*)
      INTEGER*4 B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)= BYTE2INT( A(K) )
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        B(JJ)= BYTE2INT( A(II) )
        II=II+INCA
        JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE5N(N,A,B,INCA,INCB) !FULLWORD IN , BYTE OUT

      include  'fortport'  ! DEFINES INT2BYTE

      INTEGER*4 A(*)
      BYTE B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        IF ( A(K) .GT. 255 ) THEN
           B(K) = 255
        ELSE IF ( A(K) .LT. 0 ) THEN
           B(K) = 0
        ELSE
           B(K)= INT2BYTE( A(K) )
        END IF
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        IF ( A(II) .GT. 255 ) THEN
           B(JJ) = 255
        ELSE IF ( A(II) .LT. 0 ) THEN
           B(JJ) = 0
        ELSE
           B(JJ)= INT2BYTE( A(II) )
        END IF
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE6P(N,A,B,INCA,INCB) !HALFWORD IN , FULLWORD OUT
      INTEGER*2 A(*)
      INTEGER*4 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE6N(N,A,B,INCA,INCB) !FULLWORD IN , HALFWORD OUT

      INTEGER*4 A(*)
      INTEGER*2 B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        IF ( A(K) .GT. 32767 ) THEN
           B(K) = 32767
        ELSE IF ( A(K) .LT. -32768 ) THEN
           B(K) = -32768
        ELSE
           B(K)= A(K)
        END IF
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
        IF ( A(II) .GT. 32767 ) THEN
           B(JJ) = 32767
        ELSE IF ( A(II) .LT. -32768 ) THEN
           B(JJ) = -32768
        ELSE
           B(JJ)= A(II)
        END IF
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE7(N,A,B,INCA,INCB) !REAL*4 IN , REAL*4 OUT
      REAL*4 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE8(N,A,B,INCA,INCB) !REAL*8 IN , REAL*8 OUT
      REAL*8 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        B(JJ)=A(II)
        II=II+INCA
        JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE9P(N,A,B,INCA,INCB) !REAL*4 IN , REAL*8 OUT
      REAL*4 A(*)
      REAL*8 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
          B(JJ)=A(II)
          II=II+INCA
          JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE9N(N,A,B,INCA,INCB) !REAL*8 IN , REAL*4 OUT
      REAL*8 A(*)
      REAL*4 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
