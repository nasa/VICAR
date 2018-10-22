      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     May 7 2001   rea    Support double precision input
C
      REAL*8 RBUF(10000)
      INTEGER ICOL(1000),IBUF(10000)
      CHARACTER*50 PRINTFMT
      CHARACTER*40 OUTFILE
      CHARACTER*8 DATAFMT,UFMT
      CHARACTER*3 ORG
C
      CALL XVMESSAGE('TOASCII Version May 7, 2001',' ')
C                                                          open input, get size
      CALL XVUNIT(INPUT,'INP',1,ISTAT,' ')
      CALL XVOPEN(INPUT,ISTAT,'OPEN_ACT','SA',' ')
      CALL XVGET(INPUT,ISTAT,'ORG',ORG,'FORMAT',DATAFMT,' ')
      IF (DATAFMT.EQ.'REAL' .OR. DATAFMT.EQ.'DOUB') THEN
          UFMT = 'DOUB'
      ELSE
          UFMT = 'FULL'
      END IF
      CALL XVCLOSE(INPUT,ISTAT,' ')
      CALL XVOPEN(INPUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT',UFMT,' ')
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      CALL XVBANDS(ISB,NB,NBIN)
C                                                    get or set up COLUMNS array
      CALL XVPARM('COLUMNS',ICOL,NCOL,IDEF,0)
      IF (IDEF.EQ.1) THEN
          IF (ORG.NE.'BIP') THEN
              DO I=1,NS
                  ICOL(I) = ISS+I-1
              END DO
              NCOL = NS
          ELSE
              DO I=1,NB
                  ICOL(I) = ISB+I-1
              END DO
              NCOL = NB
          END IF
      END IF
C                                                     get or set up print format
      CALL XVPARM('FORMAT',PRINTFMT,ICNT,IDEF,0)
      IF (IDEF.EQ.1) THEN
          IF (DATAFMT.EQ.'BYTE') THEN
              WRITE (PRINTFMT,100) NCOL
  100         FORMAT( '(', I4, 'I4)' )
          ELSE IF (DATAFMT.EQ.'HALF') THEN
              WRITE (PRINTFMT,110) NCOL
  110         FORMAT( '(', I4, 'I7)' )
          ELSE IF (DATAFMT.EQ.'FULL') THEN
              WRITE (PRINTFMT,120) NCOL
  120         FORMAT( '(', I4, 'I13)' )
          ELSE
              WRITE (PRINTFMT,130) NCOL
  130         FORMAT( '(', I4, 'G14.6)' )
          END IF
      END IF
C                                                                    open output
      CALL XVPARM('OUT',OUTFILE,ICNT,IDEF,0)
      OPEN (1,FILE=OUTFILE,STATUS='UNKNOWN')
C                                                             single band output
      IF (NB .EQ. 1) THEN
          IF (UFMT.NE.'DOUB') THEN
              DO I=ISL,ISL+NL-1
                  CALL XVREAD(INPUT,IBUF,ISTAT,'BAND',ISB,'LINE',I,' ')
                  WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
              END DO
          ELSE
              DO I=ISL,ISL+NL-1
                  CALL XVREAD(INPUT,RBUF,ISTAT,'BAND',ISB,'LINE',I,' ')
                  WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
              END DO
          END IF
C                                                                        BSQ
      ELSE IF (ORG .EQ. 'BSQ') THEN
          IF (UFMT.NE.'DOUB') THEN
              DO J=ISB,ISB+NB-1
                  WRITE (1,200) J
  200             FORMAT(/,' Band',I3)
                  DO I=ISL,ISL+NL-1
                      CALL XVREAD(INPUT,IBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          ELSE
              DO J=ISB,ISB+NB-1
                  WRITE (1,200) J
                  DO I=ISL,ISL+NL-1
                      CALL XVREAD(INPUT,RBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          END IF
C                                                                        BIL
      ELSE IF (ORG .EQ. 'BIL') THEN
          IF (UFMT.NE.'DOUB') THEN
              DO I=ISL,ISL+NL-1
                  WRITE (1,300) I
  300             FORMAT(/,' Line',I5)
                  DO J=ISB,ISB+NB-1
                      CALL XVREAD(INPUT,IBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          ELSE
              DO I=ISL,ISL+NL-1
                  WRITE (1,300) I
                  DO J=ISB,ISB+NB-1
                      CALL XVREAD(INPUT,RBUF,ISTAT,'BAND',J,'LINE',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          END IF
C                                                                        BIP
      ELSE
          IF (UFMT.NE.'DOUB') THEN
              DO J=ISL,ISL+NL-1
                  WRITE (1,300) J
                  DO I=ISS,ISS+NS-1
                      CALL XVREAD(INPUT,IBUF,ISTAT,'LINE',J,'SAMP',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (IBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          ELSE
              DO J=ISL,ISL+NL-1
                  WRITE (1,300) J
                  DO I=ISS,ISS+NS-1
                      CALL XVREAD(INPUT,RBUF,ISTAT,'LINE',J,'SAMP',I,
     +				  ' ')
                      WRITE (1,PRINTFMT) (RBUF(ICOL(N)),N=1,NCOL)
                  END DO
              END DO
          END IF
      END IF
C
      CLOSE(1)
      CALL XVCLOSE(INPUT,ISTAT,' ')
      RETURN
      END
