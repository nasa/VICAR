      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      COMMON/C1/  LE,PE,S,A,BUF
      REAL*4      SL,SS,EL,ES
      INTEGER     NLO,NSO,NBO,NLI,NSI,NBI,ICOUNT,IDEF
      INTEGER     SLO,SSO,SBO,ADJ,OUNIT,STAT,IPARM(150)
      INTEGER     BANDOUT, LINEOUT, BAND
      INTEGER*2   PE(12000),S(12000),A(12000),BUF(10000),LE(10000)
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN

C        SET DEFAULTS AND INITIALIZE
      NPTS   = 0
      FORMAT = ' '
      SL    = 0.0
      SS    = 0.0
      EL    = 0.0
      ES    = 0.0
      SLO   = 0
      SSO   = 0
      ADJ   = 0
      NLO   = 0
      NSO   = 0
      NLI   = 0
      NSI   = 0 
      OUNIT = 0 
      IUNIT = 0
      STAT  = 0
      ICOUNT= 0
      IDEF  = 0 
      CALL  ZIA(IPARM,150)
      CALL  ZIA(LE,10000/2)
      CALL  ZIA(PE,12000/2)
      CALL  ZIA(S ,12000/2)
      CALL  ZIA(A ,12000/2)
      CALL  ZIA(BUF,10000/2)

      call ifmessage ('ADL version 16-Feb-06')
      call xveaction ('SA', ' ')
C
C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
C
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE') THEN
         CALL mabend('ADL HANDLES BYTE DATA ONLY')
      END IF

c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,STAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')      

C
C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF(NLI.GT.10000.OR.NSI.GT.10000) THEN
         CALL mabend('MAXIMUM IMAGE SIZE IS 10000 BY 10000')
      END IF
      IF(SLO+NLO-1 .GT. NLI) THEN
         CALL mabend('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      END IF
      IF(SSO+NSO-1 .GT. NSI) THEN
         CALL mabend('NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
      END IF

      CALL XVBANDS(SBO,NBO,NBI)

      IF ( SBO .GT. NBI ) CALL MABEND(
     +  'START BAND INDEX EXCEEDS INPUT SIZE')
      IF (SBO+NBO-1 .GT. NBI) THEN 
         CALL XVMESSAGE('***Number of bands truncated', ' ')
         NBO = NBI + 1 - SBO
      ENDIF
      

C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')
C
C        PROCESS PARAMETERS
      CALL XVPARM('ADD',IPARM,ICOUNT,IDEF,150)
      DO J=1,ICOUNT,5
         ADJ = IPARM(J)
         SL = IPARM(J+1)
         SS = IPARM(J+2)
         EL = IPARM(J+3)
         ES = IPARM(J+4)
C           MORE OR LESS VERTICAL LINES
         IF(ABS(EL-SL).GE.ABS(ES-SS)) THEN
               IF(SL.NE.EL) THEN
                       SLOPE=(ES-SS)/(EL-SL)
                   ELSE
                       SLOPE = 0.0
               END IF
               OFFSET = SS-SLOPE*SL
               ISTART = NINT(MIN(SL,EL))
               IEND = NINT(MAX(SL,EL))
               DO I=ISTART,IEND
                   IL = I
                   IS = NINT(SLOPE*IL+OFFSET)
                   IF(IS.GE.SSO.AND.IS.LE.SSO+NSO-1.AND.
     &                IL.GE.SLO.AND.IL.LE.SLO+NLO-1)
     &                CALL ENTER(IL,IS,ADJ,NPTS)
               END DO
C
C             MORE OR LESS HORIZONTAL LINES
           ELSE
               SLOPE = (EL-SL)/(ES-SS)
               OFFSET = SL-SLOPE*SS
               ISTART = NINT(MIN(SS,ES))
               IEND = NINT(MAX(SS,ES))
               DO I=ISTART,IEND
                   IS = I
                   IL = NINT(SLOPE*IS+OFFSET)
                   IF(IS.GE.SSO.AND.IS.LE.SSO+NSO-1.AND.
     &                IL.GE.SLO.AND.IL.LE.SLO+NLO-1)
     &                CALL ENTER(IL,IS,ADJ,NPTS)
               END DO
         END IF
      END DO
C
      BANDOUT = 0
      DO BAND = SBO,SBO+NBO-1
         LINEOUT = 0
         BANDOUT = BANDOUT + 1
C          READ EACH LINE
        DO  LINE = SLO,SLO+NLO-1
           LINEOUT = LINEOUT + 1
          CALL XVREAD(IUNIT,BUF,STAT,'LINE',LINE,'BAND',BAND,' ')
          N=LE(LINE)
C          CHANGE PIXELS WHERE NECESSARY
          DO WHILE (N.NE.0)
              INT = BUF(S(N))+A(N)
              IF(INT.LT.0) INT=0
              IF(INT.GT.255) INT=255
              BUF(S(N))=INT
              N=PE(N)
          END DO
C            WRITE OUTPUT LINE
          CALL XVWRIT(OUNIT,BUF(SSO),STAT,'NSAMPS',NSO,
     +         'BAND', BANDOUT, 'LINE', LINEOUT, ' ')
        ENDDO
      END DO
C
C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
C
C
C *********************************************************************
C
C
      SUBROUTINE ENTER(IL,IS,IADJ,N)
      COMMON/C1/  LE,PE,S,A,BUF
      INTEGER*2   PE(12000),S(12000),A(12000),BUF(10000),LE(10000)
C                            ENTER THE PIXEL INTO THE COMMON ARRAYS
C                                LE(I) IS THE INDEX TO THE LAST ENTRY
C                                      IN THE PE, S, AND A ARRAYS
C                                      THAT RELATES TO LINE 'I'
C                                PE(J) IS THE INDEX TO THE PREVIOUS
C                                      ENTRY IN THE PE, S AND A ARRAYS
C                                      THAT RELATES TO THE SAME INPUT
C                                      LINE
C                                S(K)  IS THE SAMPLE TO BE CHANGED
C                                A(K)  IS THE AMOUNT OF ADJUSTMENT
      N=N+1
      IF(N.GT.12000) THEN
          CALL mabend('ADL IS UNABLE TO CHANGE MORE THAN 12000 PIXELS')
      END IF

      PE(N) = LE(IL)
      LE(IL)=N
      S(N)=IS
      A(N)=IADJ
      RETURN
      END
