C
C    REVISION HISTORY
C       1-85  SP   EXPANDED BUFFER SIZES TO 60000 AND REMOVED MESSAGE ABOUT
C                  PROCESSING FIRST 10000 BYTES IF IMAGE LINE TOO BIG. NOW
C                  USES 2 BUFFERS INSTEAD OF 3.
C       1-85  SP   MOVED COUNTING OF DIFFERENCES BEFORE COMPARING AGAINST
C                  MINDN BECAUSE MINDN IS 0 FOR BYTE DATA.
C       1-85  SP   ADDED MOD PARAMETER FOR BYTE DATA.
C       1-85  SP   ADDED WCHECK CALL AFTER WRITES.
C       1-85  SP   ADDED CODE TO AVOID INTEGER OVERFLOW ON EXTREME HALFWORD
C                  VALUES.
C       1-85  SP   CONVERTED TO VICAR2 SUBROUTINE CALLS.  ( U_FORMAT and 
C                  optional parameters in XVREAD and XVWRIT avoided because
C                  of apparent speed problems.)
C       1-85  SP   CHANGED MESSAGE 'NUMBER OF NONZERO PIXELS' TO 'NUMBER OF
C                  DIFFERENT PIXELS'.
C       1-85  SP   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 USES
C                  ONLY THE FORMAT IN LABEL.
C       1-85  SP   MADE OUTPUT FILE OPTIONAL TO ALLOW GREATER SPEED.
C      12-91  SP   REPLACED PRNT CALLS WITH CALLS TO PRNINT AND PRNTREAL
C                  FOR SIMPLICITY.
C      12-91  SP   PORTED TO RUN ON BOTH UNIX AND VMS.
C       9-92  SP   Made buffer size 200000 bytes. Modified to handle 
C                  all data formats.  CHANGED AVE VALS TO DISPLAY AS FLOAT.
C                  CORRECTED "AVE DN OF PIX" TO "AVE VAL OF DIFFS"
C       3-93  SP   Modified to not use -2147483648 to work around Sun compiler.
C                  Added ability to handle 3d files if SIZE field defaulted
C                  and no output file specified.
C       7-94  SP   Allowed format="WORD" as alternative to HALF.
C       8-03  lwk  removed restrictions on NB, added SB parameter;  use of
C		   optionals in XVREAD/WRIT is no longer a speed issue.
C      12-03  lwk  added checks on size/format of input files
C       6-04  lwk  allow for deviant Format types WORD & COMPLEX; removed 
c		mabend at BIP check because SIT objected to it(!)  (I don't
c		agree, and have retained all the other mabend calls, but
c		there's no need to make an issue of it)
C      07-10  lwk  fixed checks on sizes of input files; replaced abends
c		with reasonable default sizes
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MAIN44_FTN(STATUS)

C============================================================================
C
      integer   MAXBYTESPAR, STATUS
      parameter (MAXBYTESPAR=200000)
      BYTE      BUF1(MAXBYTESPAR),BUF2(MAXBYTESPAR)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2,NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2,NO,OUTFILE,SL,SS,EL,ES,SB,EB

      CHARACTER*12 FORM1, FORM2
      CHARACTER*8 ORG1, ORG2

      INTEGER  COUNT,DEFAULTED,TSTATUS
      LOGICAL  DFPXFG, XVPTST

C============================================================================
C
C  OPEN DATA SETS

C      call xvmessage('DIFPIC version 17Sep11', ' ')
      STATUS=0
      TSTATUS=0
      CALL XVUNIT( INFILE1, 'INP', 1, IND, ' ' )
      CALL XVOPEN( INFILE1,IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     . 'IO_ACT', 'SA', ' ')
      CALL XVGET( INFILE1, IND, 'FORMAT', FORM1, 'PIX_SIZE', IPIXSIZE,
     . 'ORG', ORG1, 'NL', NL1, 'NS', NS1, 'NB', NB1, ' ')
      IF (NS1*IPIXSIZE .GT. MAXBYTESPAR) call mabend(
     . 'Record too big for buffer, notify cognizant programmer.',
     . ' ')
      if (nb1.gt.1 .and. org1.eq.'BIP') then
	call xvmessage(
     .  ' BIP files not supported, use program TRAN to convert to BSQ',
     .  ' ')
        CALL XVCLOSE(INFILE1,TSTATUS,' ')
	return
      endif
      if (form1.eq.'WORD') form1 = 'HALF'

      CALL XVUNIT( INFILE2, 'INP', 2, IND, ' ' )
      CALL XVOPEN( INFILE2, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     . 'IO_ACT', 'SA', ' ')
      CALL XVGET( INFILE2, IND, 'FORMAT', FORM2, 'ORG', ORG2,
     . 'NL', NL2, 'NS', NS2, 'NB', NB2, ' ')
      if (nb2.gt.1 .and. org2.eq.'BIP') then
	call xvmessage(
     .  ' BIP files not supported, use program TRAN to convert to BSQ',
     .  ' ')
        CALL XVCLOSE(INFILE1,TSTATUS,' ')
        CALL XVCLOSE(INFILE2,TSTATUS,' ')
	return
      endif
      if (form2.eq.'WORD') form2 = 'HALF'

      ! just look at 4 bytes to make COMP same as COMPLEX
      if (form1(1:4).ne.form2(1:4)) then
        CALL XVCLOSE(INFILE1,TSTATUS,' ')
        CALL XVCLOSE(INFILE2,TSTATUS,' ')
	call mabend('Files must have same formats')
      endif

      if (org1.eq.'BIL' .or. org2.eq.'BIL') call xvmessage(
     . ' Warning: BIL format may cause performance degradation',' ')

      CALL XVSIZE( SL, SS, NLO, NSO, NLI, NSI )   ! GET SIZE PARAMETER.
      CALL XVBANDS( SB, NBO, NBI)
      ! n?i is from 1st input label ... replace with smallest value
      ! n?o is from param N? or SIZE/BANDS, if specified;  else nli
      ! make n?o smallest value to fit both inputs
      nsi = min(ns1,ns2)
      if ((ss+nso-1).gt.nsi) then
        call xvmessage(' NS too large, reduced to fit input',' ')
        nso = nsi-ss+1
      endif
      nli = min(nl1,nl2)
      if ((sl+nlo-1).gt.nli) then
        call xvmessage(' NL too large, reduced to fit input',' ')
        nlo = nli-sl+1
      endif
      nbi = min(nb1,nb2)
      if ((sb+nbo-1).gt.nbi) then
        call xvmessage(' NB too large, reduced to fit input',' ')
        nbo = nbi-sb+1
      endif

c  these don't work!
c     if ( (nl1.ne.nl2.and.sl.eq.1.and.nlo.eq.nli) .or.
c    .     (ns1.ne.ns2.and.ss.eq.1.and.nso.eq.nsi) .or.
c    .     (nb1.ne.nb2.and.sb.eq.1.and.nbo.eq.nbi) ) call mabend(
c    .' Files have different dimensions, specify SIZE/BANDS parameter!')

      EL=SL+NLO-1
      ES=SS+NSO-1
      EB=SB+NBO-1

      CALL XVPCNT( 'OUT', NO )     ! NUMBER OF OUTPUT FILES.
      IF ( NO .NE. 0)  THEN
        CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
        CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .      'IO_ACT', 'SA', 'U_NL', NLO, 'U_NS', NSO, 'U_NB', NBO, ' ' )
      END IF


      IF (FORM1(1:4) .EQ. 'BYTE')  THEN
         CALL DIFPICB(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'HALF' .OR. FORM1(1:4) .EQ. 'WORD') THEN
         CALL DIFPICH(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'FULL') THEN
         CALL DIFPICF(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'REAL') THEN
         CALL DIFPICR(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'DOUB') THEN
         CALL DIFPICD(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'COMP') THEN
         CALL DIFPICC(BUF1, BUF2, STATUS)
      ELSE
         CALL XVMESSAGE('ERROR: INVALID DATA FORMAT FROM XVGET', ' ')
      END IF

      CALL XVCLOSE(INFILE1,TSTATUS,' ')
      CALL XVCLOSE(INFILE2,TSTATUS,' ')
      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICB(BUF1, BUF2, STATUS)

      include 'fortport'

      BYTE        BUF1(*),BUF2(*), STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     IPTOT, INTOT, NPIX, NPOS, NNEG, MAXDN, MINDN
      LOGICAL MODFLAG
      LOGICAL XVPTST

C==================================================================
      MODFLAG = XVPTST( 'MOD' )     ! MOD ONLY FOR BYTE DATA.

      STATUS=0
      IPTOT=0
      INTOT=0
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=255
      MINDN=0
      IREC=0
C      CALL XVMESSAGE('IN BYTE DIF PROGRAM',' ')
      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENT PIXELS =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
             K= BYTE2INT(BUF1(J)) - BYTE2INT(BUF2(J)) 
                           ! BYTE2INT CONVERTS UNSIGNED BYTE TO INTEGER.

             IF(K.NE.0)THEN
               IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE 
                                                        !DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+K
               ENDIF
               IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE 
                                                    !DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+K
               ENDIF	
             ENDIF

             IF (MODFLAG)    THEN
                 IF (K .LT. MINDN)  K = K + 256
             ELSE
               IF(K.GT.MAXDN) K=MAXDN
               IF(K.LT.MINDN) K=MINDN
             END IF

             BUF1(II) =INT2BYTE(K)               !LOW ORDER BYTE.
             II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1
      IF (NPOS.NE.0)
     .  CALL PRNTREAL(FLOAT(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF (NNEG.NE.0)
     .  CALL PRNTREAL(FLOAT(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(FLOAT(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICH(BUF1, BUF2, STATUS)

      INTEGER*2     BUF1(*),BUF2(*), STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG, MAXDN, MINDN
      REAL        IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=32767
      MINDN=-32768
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            K=BUF1(J)
            K = K - BUF2(J)       ! AVOID INTEGER OVERFLOW ON HALFWORD DATA.

            IF(K.NE.0)THEN
              IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+K
              ENDIF
              IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+K
              ENDIF	
            ENDIF

            IF(K.GT.MAXDN)THEN
              BUF1(II)=MAXDN
            ELSE IF(K.LT.MINDN)THEN
              BUF1(II)=MINDN
            ELSE
              BUF1(II)=K
            ENDIF
 
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICF(BUF1, BUF2, STATUS)

      INTEGER*4     BUF1(*),BUF2(*),STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG, MAXDN, MINDN
      REAL        IPTOT, INTOT, RMAX, RMIN
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=2147483647
      MINDN=-MAXDN-1
      RMAX = MAXDN
      RMIN = MINDN
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            K=BUF1(J)
            L=BUF2(J)
            IF (K .GE. 0 .AND. L .GE. 0) THEN   ! MOST COMMON CASE
              K = K - L
              R = FLOAT(K)
            ELSE 
              R = FLOAT(K) - FLOAT(L)    ! CHECK IF K-L IS OUT OF RANGE.
              IF ( R .LT. RMIN ) THEN
                K = MINDN
              ELSE IF ( R .GT. RMAX ) THEN
                K = MAXDN
              ELSE 
                K = K-L
              END IF
            END IF

            IF(K.NE.0)THEN
              IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF

            BUF1(II)=K
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICR(BUF1, BUF2, STATUS)

      REAL*4     BUF1(*),BUF2(*),STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      REAL        IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0) CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0) CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICD(BUF1, BUF2, STATUS)

      REAL*8     BUF1(*),BUF2(*), STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      REAL*8      R, IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(SNGL(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(SNGL(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(SNGL(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICC(BUF1, BUF2, STATUS)

      COMPLEX*8   BUF1(*),BUF2(*),STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      COMPLEX*8   R, IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=(0., 0.)
      INTOT=(0., 0.)
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(BUF1(J) .NE. BUF2(J) )THEN
              IF(REAL(R).GE.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(REAL(R).LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1
      IF(NPOS.NE.0) CALL PRNT(10,1,IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0) CALL PRNT(10,1,INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNT(10,1,(IPTOT+INTOT)/(NB*NL*NS),' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################

      SUBROUTINE PRNTINT( IVAL, TITLE )
C
C     PURPOSE: PRNTINT prints the INTEGER value IVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
      INTEGER*4     IVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER       L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

C  IF 4 DIGITS ARE ENOUGH USE 4 DIGITS; ELSE USE 11.

      IF (-999 .LE. IVAL .AND. IVAL .LE. 9999)  THEN
         WRITE( BUF(L+1:L+4), 9040) IVAL
         N = L+4
      ELSE
         WRITE( BUF(L+1:L+11), 9110) IVAL
         N = L+11
      END IF

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9040  FORMAT( I4 )
9110  FORMAT( I11)              ! MAXIMUM LENGTH FOR 32-BIT INTEGER
      END
      SUBROUTINE PRNTREAL( RVAL, TITLE )
C
C     PURPOSE: PRNTREAL prints the REAL value RVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
      REAL*4        RVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER       L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

      WRITE( BUF(L+1:L+13), 9130) RVAL
      N = L+13

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9130  FORMAT( G13.6)              ! 6 SIGNIFICANT DIGITS IN FIXED POINT
                                  ! FOR MAGNITUDES FROM .1 TO 999999.
                                  ! OTHERWISE IN EXPONENTIAL FORMAT.
      END
