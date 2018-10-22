C******************************************************************************
      SUBROUTINE SORTIN(IBUF,NUM)
C
C	This routine sorts the NUM element integer*4 array, IBUF, in
C	ascending order.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      INTEGER*4 IBUF(NUM)
      LOGICAL MORE_TO_DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(IBUF(LOC1) .GT. IBUF(LOC2)) THEN
                      IHOLD = IBUF(LOC1)
                      IBUF(LOC1) = IBUF(LOC2)
                      IBUF(LOC2) = IHOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
C******************************************************************************
      SUBROUTINE SORTR(BUF,NUM)
C
C	This routine sorts the NUM element real*4 array, BUF, in
C	ascending order.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      REAL*4 BUF(NUM)
      LOGICAL MORE_TO_DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(BUF(LOC1) .GT. BUF(LOC2)) THEN
                      HOLD = BUF(LOC1)
                      BUF(LOC1) = BUF(LOC2)
                      BUF(LOC2) = HOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
C*************************************************************
      SUBROUTINE INDSRTF(IBUF,INDEX,NUM)
C
C	This routine provides an INDexed SoRT of Fullword data. That is, a
C	NUM element buffer, IBUF, is input.  A NUM element buffer, INDEX, is
C	output, where IBUF(INDEX(I)) is the I'th element of the sorted
C	sequence of IBUF.  The IBUF array itself remains unchanged.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      INTEGER*4 IBUF(NUM),INDEX(NUM)
      LOGICAL MORE_TO_DO
C						initialize the index array
      DO I=1,NUM
          INDEX(I) = I
      END DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(IBUF(INDEX(LOC1)) .GT. IBUF(INDEX(LOC2))) THEN
                      IHOLD = INDEX(LOC1)
                      INDEX(LOC1) = INDEX(LOC2)
                      INDEX(LOC2) = IHOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
C*************************************************************
      SUBROUTINE INDSRTR(BUF,INDEX,NUM)
C
C	This routine provides an INDexed SoRT of Real*4 data. That is, a
C	NUM element buffer, BUF, is input.  A NUM element buffer, INDEX, is
C	output, where BUF(INDEX(I)) is the I'th element of the sorted
C	sequence of BUF.  The BUF array itself remains unchanged.
C
C	The sort works by comparing each pair of array elements that are
C	ISPAN elements apart, and placing them in sort with each other.
C       Once this is done, ISPAN is halved, and the process repeated. When 
C	the process has been completed for ISPAN = 1, each element is
C	sorted with respect to the following element, hence the entire array
C	must be sorted, by induction.
C
      REAL*4 BUF(NUM)
      INTEGER*4 INDEX(NUM)
      LOGICAL MORE_TO_DO
C						initialize the index array
      DO I=1,NUM
          INDEX(I) = I
      END DO
C
      ISPAN = NUM/2
      DO WHILE (ISPAN .GT. 0)
          DO LOCPTR = 1,NUM-ISPAN
              LOC1 = LOCPTR
              MORE_TO_DO = .TRUE.
              DO WHILE (MORE_TO_DO)
                  LOC2 = LOC1 + ISPAN
                  IF(BUF(INDEX(LOC1)) .GT. BUF(INDEX(LOC2))) THEN
                      IHOLD = INDEX(LOC1)
                      INDEX(LOC1) = INDEX(LOC2)
                      INDEX(LOC2) = IHOLD
                      LOC1 = LOC1 - ISPAN
                      IF (LOC1 .LT. 1) MORE_TO_DO = .FALSE.
                  ELSE
                      MORE_TO_DO = .FALSE.
                  END IF
              END DO
          END DO
          ISPAN = ISPAN/2
      END DO
C
      RETURN
      END
