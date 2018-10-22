C	KEYINC -- SUBROUTINE TO INCREMENT CHARACTER STRING KEY
C
      SUBROUTINE KEYINC(KEY)
C
C	PASSED VARIABLES:
C
C KEY   -- CHARACTER STRING CONTAINING DIGITS
C
C	LOCAL VARIABLES:
C
C J,K -- ONE'S, AND TEN'S DIGITS
      character*1 J,K
      CHARACTER*5 KEY
      character*100 sccsKe
      data sccsKe /'@(#) keyinc.f 2.1 9/2/86 PDS Vicar2'/
C
      J = char(ICHAR(KEY(5:5)) + 1)
C
C--UPDATE KEY NAME... J INDICATES ONE'S DIGIT, K THE TEN'S DIGIT
      IF (J .GT. '9') THEN
          J = '0'
          K = char(ICHAR(KEY(4:4)) + 1)
          KEY(4:4) = K
      ENDIF
      KEY(5:5) = J
      RETURN
      END
