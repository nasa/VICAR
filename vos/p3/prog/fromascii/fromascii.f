      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     APR 03 2002   ...rea... SLINE and NLINES parameters added
C
      REAL RBUF(150),ROUT(150)
      INTEGER ICOL(150),IBUF(150),IOUT(150)
      CHARACTER*50 PRINTFMT,PRT
      CHARACTER*40 INFILE
      CHARACTER*8 OFMT,UFMT
      LOGICAL QFORMAT
C                                                                    open input
      CALL XVPARM('ASCII',INFILE,ICNT,IDEF,0)
      OPEN (1,FILE=INFILE,STATUS='OLD')
C
      CALL XVPARM('DATA',OFMT,ICNT,IDEF,0)
      IF (OFMT .EQ. 'REAL') THEN
          UFMT = 'REAL'
      ELSE
          UFMT = 'FULL'
      END IF
      CALL XVPARM('NCOL',NCOL,ICNT,IDEF,0)
C                                                    get or set up COLUMNS array
      CALL XVPARM('COLUMNS',ICOL,NS,IDEF,0)
      IF (IDEF.EQ.1) THEN
          NS = NCOL
          DO I=1,NS
              ICOL(I) = I
          END DO
      END IF
C							 get START_LINE and skip
C								  unwanted lines
      CALL XVPARM('SLINE',ISL,ICNT,IDEF,0)
      IF (ISL .NE. 1) THEN
          DO I=1,ISL-1
              READ (1,*,END=500)
          END DO
      END IF
C								   get NUM_LINES
      CALL XVPARM('NLINES',NLOUT,ICNT,IDEF,0)
      IF (ICNT .EQ. 0) NLOUT = 99999999
C                                                               get print format
      CALL XVPARM('FORMAT',PRINTFMT,ICNT,IDEF,0)
      QFORMAT = (IDEF.EQ.0)
C                                           count out the number of output lines
      NL = 0
      IF (QFORMAT) THEN
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,PRINTFMT,END=500,ERR=100) (RBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  100             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,PRINTFMT,END=500,ERR=200) (IBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  200             CONTINUE
              END DO
          END IF
      ELSE
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,*,END=500,ERR=300) (RBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  300             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,*,END=500,ERR=400) (IBUF(J),J=1,NCOL)
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 500
  400             CONTINUE
              END DO
          END IF
      END IF
  500 CONTINUE
C
      IF (NL.LT.1) THEN
          CALL XVMESSAGE(' No records found that match the format',' ')
	  CALL ABEND
      ELSE
          WRITE (PRT,600) NL
  600     FORMAT(I10,' Records output')
          CALL XVMESSAGE(PRT,' ')
      END IF
      CLOSE(1)
      OPEN (1,FILE=INFILE,STATUS='OLD')
      IF (ISL .NE. 1) THEN
          DO I=1,ISL-1
              READ (1,*,END=500)
          END DO
      END IF
C                                                                  open output
      CALL XVUNIT(IOUTPUT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(IOUTPUT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT',UFMT,'O_FORMAT',OFMT,'OP','WRITE',
     +            'U_NL',NL,'U_NS',NS,'U_NB',1,'U_ORG','BSQ',' ')
C                                                                  copy output
      NLOUT = NL
      NL = 0
      IF (QFORMAT) THEN
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,PRINTFMT,END=1500,ERR=1100) (RBUF(J),J=1,NCOL)
                  DO J=1,NS
                      ROUT(J) = RBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,ROUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
                  
 1100             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,PRINTFMT,END=1500,ERR=1200) (IBUF(J),J=1,NCOL)
                  DO J=1,NS
                      IOUT(J) = IBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,IOUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
 1200             CONTINUE
              END DO
          END IF
      ELSE
          IF (UFMT.EQ.'REAL') THEN
              DO I=1,1000000
                  READ (1,*,END=1500,ERR=1300) (RBUF(J),J=1,NCOL)
                  DO J=1,NS
                      ROUT(J) = RBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,ROUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
 1300             CONTINUE
              END DO
          ELSE
              DO I=1,1000000
                  READ (1,*,END=1500,ERR=1400) (IBUF(J),J=1,NCOL)
                  DO J=1,NS
                      IOUT(J) = IBUF(ICOL(J))
                  END DO
                  CALL XVWRIT(IOUTPUT,IOUT,ISTAT,'NSAMPS',NS,' ')
                  NL = NL +1
                  IF (NL .EQ. NLOUT) GO TO 1500
 1400             CONTINUE
              END DO
          END IF
      END IF
 1500 CONTINUE
C
      CLOSE(1)
      CALL XVCLOSE(IOUTPUT,ISTAT,' ')
      RETURN
      END
