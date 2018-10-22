CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Add post-stretch to look-up table
C
      SUBROUTINE POST_STRETCH(lut,nchar2,prt2)
      IMPLICIT NONE
      INTEGER*2 LUT(-32768:32767)
      INTEGER*4 NCHAR2
      CHARACTER*80 PRT2

      COMMON/C3/INMIN,INMAX,DNMIN,DNMAX
      INTEGER*4 INMIN,INMAX,DNMIN,DNMAX

      REAL*4 A,B,RTRUNC
      INTEGER*4 IDN,LVAL,HVAL,IPARM(2),ICOUNT,IDEF
  810 FORMAT ('Post-stretch:',I7,' to',I7,' and',I7,' to',I7)

      CALL XVPARM('POST',iparm,icount,idef,2)
      IF (ICOUNT.LT.2) RETURN
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('*** POST-STRETCH OPTION ***',' ')
      LVAL = IPARM(1)
      HVAL = IPARM(2)
      IF (LVAL.LT.DNMIN .OR. HVAL.LT.DNMIN) THEN
         CALL XVMESSAGE('Post-stretch parameter less than dnmin',' ')
         CALL XVMESSAGE('Parameter reset to dnmin',' ')
         IF (LVAL.LT.DNMIN) LVAL=DNMIN
         IF (HVAL.LT.DNMIN) HVAL=DNMIN
      ENDIF
      IF (LVAL.GT.DNMAX .OR. HVAL.GT.DNMAX) THEN
         CALL XVMESSAGE('Post-stretch parameter exceeds dnmax',' ')
         CALL XVMESSAGE('Parameter reset to dnmax',' ')
         IF (LVAL.GT.DNMAX) LVAL=DNMAX
         IF (HVAL.GT.DNMAX) HVAL=DNMAX
      ENDIF
      IF (LVAL.EQ.HVAL) THEN
         CALL XVMESSAGE('***Err - post values specified are equal',' ')
         CALL ABEND
      ENDIF

C     ....Compute composite lookup table
      A = FLOAT(HVAL-LVAL)/FLOAT(DNMAX-DNMIN)
      B = -A*DNMIN + LVAL
      DO IDN=INMIN,INMAX
         LUT(IDN) = RTRUNC(A*LUT(IDN) + B)
      ENDDO
      WRITE (PRT2,810) DNMIN,LVAL,DNMAX,HVAL
      NCHAR2 = 52
      CALL XVMESSAGE(PRT2,' ')
      RETURN
      END
