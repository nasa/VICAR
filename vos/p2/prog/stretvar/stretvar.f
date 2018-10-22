      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C VICAR PROGRAM--STRETVAR
C     PURPOSE: Perform a linear stretch such that the low and high
C	   stretch limits vary as a function of line number.
C          The stretch limits are specified by the user for a number
C          of discrete lines via the TABLE parameter.  The stretch
C          limits for each line between those specified are computed
C          by interpolation.  The linear stretch is performed line-
C          by-line by computing a look-up table and calling LUT.
C
C     USER PARAMETERS:
C          TABLE, L(1),INDN1(1),OUTDN1(1),INDN2(1),OUTDN2(1),
C                 L(2),INDN1(2),OUTDN1(2),INDN2(2),OUTDN2(2),
C                                  . . .
C                 L(N),INDN1(N),OUTDN1(N),INDN2(N),OUTDN2(N)
C           - All parameters are integers.
C             L(K) specifies the line number and
C             INDN1(K),OUTDN1(K),INDN2(K),OUTDN2(K) define
C             the low and high stretch limits for the input and
C             output, such that INDN1 maps to OUTDN1 and INDN2
C             maps to OUTDN2.
C
      INCLUDE 'fortport'
      COMMON/C1/BUF(65536),PAR(250),STBL(256)
      COMMON/C1/LINEST(50),INDN1(50),INDN2(50),OUTDN1(50),OUTDN2(50)
      INTEGER*4 PAR,OUTDN1,OUTDN2
      BYTE BUF,STBL
      CHARACTER*5 FORMAT


      CALL IFMESSAGE('STRETVAR version 02-MAY-94')
      CALL XVUNIT(IDSRN,'INP',1,IND,' ')
      CALL XVOPEN(IDSRN,IND,' ')
      CALL XVGET(IDSRN,IND,'FORMAT',FORMAT,' ')
      IF (FORMAT.NE.'BYTE') GOTO 993

      CALL XVUNIT(ODSRN,'OUT',1,IND,' ')
      CALL XVOPEN(ODSRN,IND,'OP','WRITE',' ')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)

      CALL XVP('TABLE',PAR,NVAL)
      NTAB = NVAL/5
      IF (5*NTAB.NE.NVAL) GOTO 998
      IF (NTAB.LT.2) GOTO 995

      DO I=1,NTAB
          J = (I-1)*5 + 1
          LINEST(I) = PAR(J)
          IF (I.GT.1.AND.LINEST(I).LE.LINEST(I-1)) GOTO 994
          INDN1(I) = PAR(J+1)
          INDN2(I) = PAR(J+3)
          OUTDN1(I) = PAR(J+2)
          OUTDN2(I) = PAR(J+4)
      ENDDO

      IF (LINEST(1).NE.1 .OR. LINEST(NTAB).NE.NL) GOTO 995
C
      NSTRIP = NTAB-1
C
C            Begin processing strip by strip.
      DO 500 ISTRIP=1,NSTRIP
          LINE1 = LINEST(ISTRIP)
          LINE2 = LINEST(ISTRIP+1)
          IF(ISTRIP.LT.NSTRIP) LINE2=LINE2-1
          IX1 = INDN1(ISTRIP)
          IX2 = INDN2(ISTRIP)
          IY1 = OUTDN1(ISTRIP)
          IY2 = OUTDN2(ISTRIP)
	  X1 = IX1
	  X2 = IX2
	  Y1 = IY1
	  Y2 = IY2
          X3 = INDN1(ISTRIP+1)
          X4 = INDN2(ISTRIP+1)
          Y3 = OUTDN1(ISTRIP+1)
          Y4 = OUTDN2(ISTRIP+1)
          GAP = LINE2 - LINE1
          IF(ISTRIP.LT.NSTRIP) GAP=GAP+1
          DELX1 = (X3-X1)/GAP
          DELX2 = (X4-X2)/GAP
          DELY1 = (Y3-Y1)/GAP
          DELY2 = (Y4-Y2)/GAP

          DO LINE=LINE1,LINE2
              IF(LINE/100*100.EQ.LINE) CALL PRNT(4,1,LINE,' LINE.')
C                   Compute stretch table for current line.
              CALL MVE(-5,IX1+1,IY1,STBL,0,1)
              CALL MVE(-5,256-IX2,IY2,STBL(IX2+1),0,1)
              J1 = IX1+2
              OUTDN = IY1 + 0.5
              DELTAB = (Y2-Y1)/(X2-X1)

              DO J=J1,IX2
                  OUTDN = OUTDN + DELTAB
                  IVAL = OUTDN
                  STBL(J) = INT2BYTE(IVAL)
              ENDDO
C
C		   Stretch the current line....
              CALL XVREAD(IDSRN,BUF,IND,' ')
              CALL LUT(NS,BUF,STBL,BUF)
              CALL XVWRIT(ODSRN,BUF,IND,' ')
              X1 = X1 + DELX1
              X2 = X2 + DELX2
              Y1 = Y1 + DELY1
              Y2 = Y2 + DELY2
              IX1 = X1 + .5
              IX2 = X2 + .5
              IY1 = Y1 + .5
              IY2 = Y2 + .5
          ENDDO
  500 CONTINUE

      CALL XVCLOSE(IDSRN,IND,' ')
      CALL XVCLOSE(ODSRN,IND,' ')
      CALL XVMESSAGE('STRETVAR task completed',' ')
      RETURN

  993 CALL XVMESSAGE('***Input image must be in byte format',' ')
      GOTO 999
  994 CALL XVMESSAGE('***TABLE line numbers must increase',' ')
      GOTO 999
  995 CALL XVMESSAGE('***TABLE must include lines 1 and NL',' ')
      GOTO 999
  998 CALL XVMESSAGE('***TABLE entries must be in multiples of 5',' ')
  999 CALL XVMESSAGE('***STRETVAR task cancelled',' ')
      CALL ABEND
      END
