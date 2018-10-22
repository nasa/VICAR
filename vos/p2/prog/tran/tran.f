C  1 JULY 1994 .... CRI ....  MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
C
      IMPLICIT NONE
C
      INTEGER*4    CNT, DEF, I, ILOC, ILOC2, INUNIT, J, K, L, NBI
      INTEGER*4    NBO, NBYTE, NLI, NSI, NSO, NOUT, STATUS
      INTEGER*4    BAND(25), OUTUNIT(25)
      CHARACTER*1  INBUF(100)
      BYTE         BUF(900000), BUF2(900000)
      LOGICAL      BFLAG
      CHARACTER*3  INORG, OUTORG, ORG
      CHARACTER*4  FORMAT

      DATA BFLAG /.FALSE./
C
C  OPEN INPUT FILE
C
      CALL IFMESSAGE('TRAN version 06-04-98')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ')
C
      CALL XVGET(INUNIT,STATUS,'NL',NLI,'NS',NSI,'NB',NBI,'ORG',
     &           INORG,'FORMAT',FORMAT,' ')

C
      CALL XVP('OUT',INBUF,NOUT)
C
C  CHECK INPUT IMAGE SIZE IF IT'S MSS
C
      CALL XVPARM('NBANDS',NBO,CNT,DEF,1)
      IF (DEF.EQ.0) THEN
         IF (INORG.EQ.'BSQ' .AND. NBI.EQ.1) THEN
            INORG = 'MSS'
            NSO = INT(NSI/NBO)
            IF (NBO*NSO.NE.NSI) THEN
               CALL XVMESSAGE('PARAMETER ERROR',' ')
               CALL XVMESSAGE('NUMBER OF SAMPLES IN INPUT IS NOT ' //
     & 'DIVISIBLE BY NBANDS',' ')
               CALL ABEND
            ENDIF
         ELSE
            CALL XVMESSAGE
     &             ('NBANDS IGNORED.  ONLY USED FOR MSS DATA',' ')
         ENDIF
      ELSE
         NBO = NBI
         NSO = NSI
      ENDIF
C
C  IF OUTORG = MSS, THEN ORG = BSQ, NSO = NSI*NBI, NBO = 1, NLO = NLI
C
      CALL XVPARM('OUTORG',OUTORG,CNT,DEF,1)
      IF (OUTORG.EQ.'MSS') THEN
         ORG = 'BSQ'
         NBO = 1
         NSO = NSI * NBI
      ELSE
         ORG = OUTORG
      ENDIF
C
C  IF BAND IS SPECIFIED, THEN EACH OUTPUT IS 1 BAND, BSQ
C
      CALL XVPARM('BANDS',BAND,CNT,DEF,25)
      IF (DEF.EQ.0) THEN
         IF (OUTORG.NE.'BSQ') THEN
            CALL XVMESSAGE('PARAMETER ERROR',' ')
            CALL XVMESSAGE('BANDS IS ONLY SPECIFIED FOR CREATING ' //
     & 'BSQ FILES OF 1 BAND EACH',' ')
            CALL ABEND
         ENDIF
         IF (CNT.NE.NOUT) THEN
            CALL XVMESSAGE('NUMBER OF BANDS DOES NOT MATCH THE ' //
     & 'NUMBER OF OUTPUTS',' ')
            CALL ABEND
         ENDIF
         DO I = 1,NOUT
            IF (BAND(I).GT.NBO) THEN
               CALL XVMESSAGE('BAND NUMBER IS LARGER THAN THE ' //
     & 'NUMBER OF BANDS IN THE INPUT',' ')
               CALL ABEND
            ENDIF
         ENDDO
         BFLAG = .TRUE.
      ENDIF
C
C  CHECK THAT BUF IS LARGE ENOUGH
C
      IF (FORMAT.EQ.'BYTE') THEN
         NBYTE = 1
      ELSEIF (FORMAT.EQ.'HALF') THEN
         NBYTE = 2
      ELSEIF (FORMAT.EQ.'FULL' .OR. FORMAT.EQ.'REAL') THEN
         NBYTE = 4
      ELSE
         NBYTE = 8
      ENDIF
      IF (NSI*NBI*NBYTE.GT.900000) THEN
         CALL XVMESSAGE('BUFFER TOO SMALL.  CONSULT A PROGRAMMER',' ')
         CALL ABEND
      ENDIF
C
C  OPEN OUTPUT FILE
C
      IF (BFLAG) THEN
         DO I = 1,NOUT
            CALL XVUNIT(OUTUNIT(I),'OUT',I,STATUS,' ')
            CALL XVOPEN(OUTUNIT(I),STATUS,'OP','WRITE','U_NL',NLI,
     &           'U_NS',NSO,'U_NB',1,'U_ORG',ORG,' ')
            CALL XLADD(OUTUNIT(I),'HISTORY','BAND',BAND(I),STATUS,
     &           'FORMAT','INT',' ')
         ENDDO
      ELSE
         CALL XVUNIT(OUTUNIT(1),'OUT',1,STATUS,' ')
         CALL XVOPEN(OUTUNIT(1),STATUS,'OP','WRITE',
     &        'U_NL',NLI,'U_NS',NSO,'U_NB',NBO,'U_ORG',ORG,' ')
      ENDIF
C
C*******************
C
C  MSS TO BIL OR BIP
C
      IF (INORG.EQ.'MSS') THEN
         IF (OUTORG.EQ.'BIL' .OR. OUTORG.EQ.'BIP') THEN
            DO I = 1,NLI
               CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,'SAMP',1,
     &                     'NSAMPS',NSI,' ')
C
C     MSS TO BIL
C
               IF (OUTORG.EQ.'BIL') THEN
                  DO J = 1,NBO
                     ILOC = (NSO * (J-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(1),BUF(ILOC),STATUS,' ')
                  ENDDO
C
C     MSS TO BIP
C
               ELSE
                  DO J = 1,NSO
                     DO K = 1,NBO
                        ILOC = (NSO * (K-1) + (J-1))*NBYTE + 1
                        ILOC2 = (K-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
                  ENDDO
               ENDIF
            ENDDO
C
C  MSS TO BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ') THEN
            IF (BFLAG) THEN
               DO I = 1,NLI
                  CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,
     &                        'SAMP',1,'NSAMPS',NSI,' ')
                  DO J = 1,NOUT
                     ILOC = (NSO * (BAND(J)-1))*NBYTE  + 1
                     CALL XVWRIT(OUTUNIT(J),BUF(ILOC),STATUS,' ')
                  ENDDO
               ENDDO
            ELSE
               DO I = 1,NBO
                  DO J = 1,NLI
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',J,
     &                           'SAMP',1,'NSAMPS',NSI,' ')
                     ILOC = (NSO * (I-1))*NBYTE  + 1
                     CALL XVWRIT(OUTUNIT(1),BUF(ILOC),STATUS,' ')
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            CALL XVMESSAGE('OUTORG MUST BE BIL, BIP, OR BSQ',' ')
            CALL ABEND
         ENDIF
      ENDIF
C
C*******************
C
C  BIL TO MSS OR BIP
C
      IF (INORG.EQ.'BIL') THEN
         IF (OUTORG.EQ.'MSS' .OR. OUTORG.EQ.'BIP') THEN
            DO I = 1,NLI
               DO J = 1,NBI
                  ILOC = (NSI * (J-1))*NBYTE + 1
                  CALL XVREAD(INUNIT,BUF(ILOC),STATUS,'LINE',I,
     &                        'SAMP',1,'NSAMPS',NSI,'BAND',J,' ')
               ENDDO
C
C     BIL TO MSS
C
               IF (OUTORG.EQ.'MSS') THEN
                  CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
C
C     BIL TO BIP
C
               ELSE
                  DO J = 1,NSI
                     DO K = 1,NBI
                        ILOC = (NSI * (K-1) + (J-1))*NBYTE +1
                        ILOC2 = (K-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
                  ENDDO
               ENDIF
            ENDDO
C
C  BIL TO BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ') THEN
            IF (BFLAG) THEN
               DO I = 1,NLI
                  DO J = 1,NOUT
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,
     &                    'SAMP',1,'NSAMPS',NSI,'BAND',BAND(J),' ')
                     CALL XVWRIT(OUTUNIT(J),BUF,STATUS,' ')
                  ENDDO
               ENDDO
            ELSE
               DO I = 1,NBI
                  DO J = 1,NLI
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',J,
     &                    'SAMP',1,'NSAMPS',NSI,'BAND',I,' ')
                     CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            CALL XVMESSAGE('OUTORG MUST BE MSS, BIP, OR BSQ',' ')
            CALL ABEND
         ENDIF
      ENDIF
C
C*******************
C
C  BIP TO BIL OR MSS
C
      IF (INORG.EQ.'BIP') THEN
         IF (OUTORG.EQ.'BIL' .OR. OUTORG.EQ.'MSS') THEN
            DO I = 1,NLI
               DO J = 1,NSI
                  CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,'SAMP',
     &                        J,'BAND',1,'NBANDS',NBI,' ')
                  DO K = 1,NBI
                     ILOC2 = (NSI * (K-1) + (J-1))*NBYTE + 1
                     ILOC = (K-1)*NBYTE + 1
                     DO L = 1,NBYTE
                        BUF2(ILOC2) = BUF(ILOC)
                        ILOC = ILOC + 1
                        ILOC2 = ILOC2 + 1
                     ENDDO
                  ENDDO
               ENDDO
C
C     BIP TO BIL
C
               IF (OUTORG.EQ.'BIL') THEN
                  DO K = 1,NBI
                     ILOC = (NSI * (K-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(1),BUF2(ILOC),STATUS,' ')
                  ENDDO
C
C     BIP TO MSS
C
               ELSE
                  CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
               ENDIF
            ENDDO
C
C  BIP TO BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ') THEN
            IF (BFLAG) THEN
               DO I = 1,NLI
                  DO J = 1,NSI
                     CALL XVREAD(INUNIT,BUF,STATUS,'LINE',I,'SAMP',
     &                           J,'BAND',1,'NBANDS',NBI,' ')
                     DO K = 1,NOUT
                        ILOC2 = (NSI*(BAND(K)-1)+(J-1))*NBYTE + 1
                        ILOC = (BAND(K)-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                  ENDDO
                  DO K = 1,NOUT
                     ILOC = (NSI * (BAND(K)-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(K),BUF2(ILOC),STATUS,' ')
                  ENDDO
               ENDDO
            ELSE
               DO I = 1,NBI
                  DO J = 1,NLI
                     DO K = 1,NSI
                        ILOC = (K-1)*NBYTE + 1
                        CALL XVREAD(INUNIT,BUF(ILOC),STATUS,'LINE',
     &                       J,'SAMP',K,'BAND',I,'NBANDS',1,' ')
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
                  ENDDO
               ENDDO
            ENDIF
         ELSE
            CALL XVMESSAGE('OUTORG MUST BE MSS, BIL, OR BSQ',' ')
            CALL ABEND
         ENDIF
      ENDIF
C
C*******************
C
C  BSQ TO MSS, BIL, OR BIP
C
      IF (INORG.EQ.'BSQ') THEN
         IF (OUTORG.EQ.'MSS' .OR. OUTORG.EQ.'BIL' .OR.
     &       OUTORG.EQ.'BIP') THEN
            DO I = 1,NLI
               DO J = 1,NBI
                  ILOC = (NSI * (J-1))*NBYTE + 1
                  CALL XVREAD(INUNIT,BUF(ILOC),STATUS,'LINE',I,
     &                        'SAMP',1,'NSAMPS',NSI,'BAND',J,' ')
               ENDDO
C
C     BSQ TO MSS
C
               IF (OUTORG.EQ.'MSS') THEN
                  CALL XVWRIT(OUTUNIT(1),BUF,STATUS,' ')
C
C     BSQ TO BIL
C
               ELSEIF (OUTORG.EQ.'BIL') THEN
                  DO J = 1,NBO
                     ILOC = (NSO * (J-1))*NBYTE + 1
                     CALL XVWRIT(OUTUNIT(1),BUF(ILOC),STATUS,' ')
                  ENDDO
C
C     BSQ TO BIP
C
               ELSEIF (OUTORG.EQ.'BIP') THEN
                  DO J = 1,NSO
                     DO K = 1,NBO
                        ILOC = (NSO * (K-1) + (J-1))*NBYTE + 1
                        ILOC2 = (K-1)*NBYTE + 1
                        DO L = 1,NBYTE
                           BUF2(ILOC2) = BUF(ILOC)
                           ILOC = ILOC + 1
                           ILOC2 = ILOC2 + 1
                        ENDDO
                     ENDDO
                     CALL XVWRIT(OUTUNIT(1),BUF2,STATUS,' ')
                  ENDDO
               ENDIF
            ENDDO
C
C  BSQ TO MULTIPLE OUTPUTS OF 1 BAND OF BSQ
C
         ELSEIF (OUTORG.EQ.'BSQ' .AND.
     &           BFLAG.EQV..TRUE.) THEN
            DO I = 1,NOUT
               DO J = 1,NLI
                  CALL XVREAD(INUNIT,BUF,STATUS,'LINE',J,
     &                 'SAMP',1,'NSAMPS',NSI,'BAND',BAND(I),' ')
                  CALL XVWRIT(OUTUNIT(I),BUF,STATUS,' ')
               ENDDO
            ENDDO
         ELSE
            CALL XVMESSAGE('OUTORG MUST BE MSS, BIL, BIP, OR ' //
     & 'OUTPUT(S) OF 1 BAND OF BSQ',' ')
            CALL ABEND
         ENDIF
      ENDIF
C
C  CLOSE FILES
C
      CALL XVCLOSE(INUNIT,STATUS,' ')
      DO I = 1,NOUT
         CALL XVCLOSE(OUTUNIT(I),STATUS,' ')
      ENDDO
      RETURN
      END
