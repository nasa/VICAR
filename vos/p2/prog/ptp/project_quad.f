CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Project quadrant from input PIC to output BUF (for byte data).
C
      SUBROUTINE PROJECT_QUAD(PIC,buf,INTERP,INCLUDE,INC,INCo,
     &		LBEG,SBEG,A,B,DL,DS,NLI,NSI,NLO,NSO,ROW)
      BYTE PIC(NSI,NLI),BUF(NSO,INCo)
      INTEGER*4 INTERP,INCLUDE,INC,INCo,LBEG,SBEG,NLI,NSI,NLO,NSO,ROW
      REAL*8 DL,DS

      COMMON/SCALING/SCALE_FACTOR
      REAL*8 SCALE_FACTOR

      REAL*8 X,Y,RLINE,RSAMP
      REAL*4 A(4),B(4)
      INTEGER*4 L,S,IL,IS,LEND,SEND,LL
      INTEGER*4 D1,D2,D3,D4,IVAL,IND

      INCLUDE 'fortport'

      LEND = LBEG + INC - 1
      SEND = SBEG + INC - 1
      IF (LEND.GT.NLO) LEND=NLO
      IF (SEND.GT.NSO) SEND=NSO
      IND = 1

      DO 30 L=LBEG,LEND
      LL = L - ROW + 1

      DO 30 S=SBEG,SEND
      IVAL=0				!Clear DN
      IF (INTERP.EQ.1) THEN
        CALL PSUB(INCLUDE,L,S,rline,rsamp,ind)
        IF (IND.EQ.0) THEN
           IF (INCLUDE.EQ.0) GOTO 30
           RSAMP = SCALE_FACTOR*(S + DS) !RRP FR89270
           RLINE = SCALE_FACTOR*(L + DL) !RRP FR89270
        ENDIF        
      ELSEIF (INTERP.EQ.2) THEN
        RSAMP = A(1) + A(2)*S + A(3)*L + A(4)*L*S
        RLINE = B(1) + B(2)*S + B(3)*L + B(4)*L*S
      ELSEIF (INTERP.EQ.3) THEN
        RSAMP = SCALE_FACTOR*(S + DS) !RRP FR89270
        RLINE = SCALE_FACTOR*(L + DL) !RRP FR89270
      ELSE 
        GOTO 30
      ENDIF

      IS = RSAMP
      IL = RLINE
      IF (IL.LT.1.OR.IS.LT.1) GOTO 30           !Point off image
      IF (IL.GE.NLI.OR.IS.GE.NSI) GOTO 30

C     ....Compute pixel using bilinear interpolation
      D1 = BYTE2INT(PIC(IS,IL))         !upper left pixel
      D2 = BYTE2INT(PIC(IS+1,IL))       !upper right pixel
      D3 = BYTE2INT(PIC(IS,IL+1))       !lower left pixel
      D4 = BYTE2INT(PIC(IS+1,IL+1))     !lower right pixel
      X = RSAMP - IS
      Y = RLINE - IL
      IVAL = D1 + (D2-D1)*X + (D3-D1)*Y + (D1-D2-D3+D4)*X*Y
CCC      IVAL = INTERP
CCC      IF (IND.EQ.0) IVAL=3
CCC      IF (INC.EQ.32) IVAL=32
CCC      IF (INC.EQ.16) IVAL=64
CCC      IF (INC.EQ.8) IVAL=128
CCC      IF (INC.EQ.4) IVAL=196
CCC      IF (INTERP.EQ.1) IVAL=255
   30 BUF(S,LL) = int2byte(IVAL)

      RETURN
      END
