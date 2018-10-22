      FUNCTION CMPR(X,Y,N)
C
C  83-3-9  ...  LWK
C
C CMPR compares 2 byte arrays, X and Y, of length N.
C It returns .TRUE. if they are identical, .FALSE. otherwise.
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL CMPR
      BYTE X(*),Y(*)
C
      CMPR = .TRUE.
      DO I=1,N
	IF (X(I).NE.Y(I)) THEN
	  CMPR = .FALSE.
	  RETURN
	ENDIF
      ENDDO
      RETURN
      END
