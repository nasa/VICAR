      SUBROUTINE HSUB(DCODE ,NS ,BUF ,HIST , ILOW , IHIGH)
C
C  92-9-30 ...SP.... Changed to use fortport.fin as improved portability
C                    method.  Went back from MACRO to FORTRAN CMS generation 1.
C                    Changed ILOW and IHIGH to required parameters.
C                    (Defaults were 0 & 255 for byte, 0 & 32767 for half.)
C
C  83-6-1  ...LWK... support byte data, make ILOW/HIGH optional
C  83-5-2  ...LWK... found in LIBSOR: (UCL routine?)
C
C     HSUB PRODUCES CUMULATIVE HISTOGRAM OF INPUT ARRAY( BUF)
C
C     DCODE      :- DATA FORMAT FOR INPUT ARRAY BUF
C        (1=byte, 2=halfwd)
C     NS         :- NUMBER OF SAMPLES IN ARRAY  BUF
C     BUF        :- INPUT ARRAY CONTAINING INPUT SAMPLES
C     HIST       :- ARRAY HOLDING SAMPLE FREQUENCIES
C     ILOW       :- LOWEST   DN IN HISTOGRAM RANGE (required)
C     IHIGH      :- HIGHEST  DN IN HISTOGRAM RANGE     "
C
      INTEGER   HIST(*) , ILOW , IHIGH , DCODE
      INTEGER*2 BUF(*) 
C==================================================================
      IF (DCODE.EQ.2) THEN
	ILO = ILOW
	IHI = IHIGH
	IF (IHI.LT.ILO) IHI = ILO+1
      ENDIF
      IF (DCODE.EQ.1) CALL HSUBB(NS,BUF,HIST)
      IF (DCODE.EQ.2) CALL HSUBH(NS,BUF,HIST,ILO,IHI)
      RETURN
      END
C
      SUBROUTINE HSUBH(NS,BUF,HIST,ILOW,IHIGH)
      IMPLICIT INTEGER (A-Z)
      DIMENSION HIST(*)
      INTEGER*2 BUF(*)
      DO 10 I = 1,NS
	K = BUF(I)
	IF(K .LT. ILOW ) K = ILOW
	IF(K .GT. IHIGH) K = IHIGH
	K = K - ILOW + 1
	HIST(K) = HIST(K) + 1
   10 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE HSUBB(NS,BUF,HIST)

      IMPLICIT INTEGER (A-Z)
      include  'fortport'  ! defines BYTE2INT.

      DIMENSION HIST(*)
      BYTE BUF(*)
C==================================================================

      DO 10 I = 1,NS
	K = BYTE2INT(BUF(I))+1
	HIST(K) = HIST(K) + 1
   10 CONTINUE
C
      RETURN
      END
