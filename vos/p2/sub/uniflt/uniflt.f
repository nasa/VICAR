C***************************************************************************
	SUBROUTINE UNIFLT(DCODE, NPIX, IN, OUT, NSW)
C
C	This routine performs a smoothing filter of NSW elements with unity
C	weights over the array of values in IN.  The output is placed in the
C	array OUT.  If NSW is positive, the result is normalized (i.e.,
C	DN(out) = sum of the NSW nearest pixels / NSW ); if NSW is negative,
C	the normalization by NSW is suppressed.  Output elements near the edges
C	are computed by assuming that the first and last elements are repeated
C	at all points beyond the edge.
C
C	Arguments:
C	    DCODE   Input    Integer    Code for the data types of IN and OUT
C					 IN	OUT
C				 1 =>	byte	byte
C				 2 =>	half	half
C				 3 =>	byte	half
C				 4 =>	full	full
C				 5 =>	byte	full
C				 6 =>	half	full
C				 7 =>	real	real
C				 8 =>	real*8	real*8
C				 9 =>	real	real*8
C				-3 =>	half	byte
C				-5 =>	full	byte
C				-6 =>	full	half
C				-9 =>	real*8	real
C
C	    NPIX    Input    Integer    Number of elements to be filtered
C
C	    IN      Input    Array      Array in input values
C
C	    OUT     Output   Array      Array of output values
C
C	    NSW     Input    Integer    Number of filter weights; NSW must be
C					odd.  If negative, the summation is
C					output, rather than the average.
	INTEGER DCODE, NPIX, NSW, IN(*), OUT(*)
C
C					force a valid NSW
	IF (NSW .GT. 0) THEN
	    DIVISOR = NSW
	    NSWX = NSW
	ELSE
	    DIVISOR = 1.0
	    NSWX = -NSW
	END IF
	IF (MOD(NSWX,2) .EQ. 0) NSWX = 2*NSWX + 1
C						call appropriate subroutine
	IF (DCODE .EQ. 1) THEN
	    CALL UFLT1(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 2) THEN
	    CALL UFLT2(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 3) THEN
	    CALL UFLT3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 4) THEN
	    CALL UFLT4(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 5) THEN
	    CALL UFLT5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 6) THEN
	    CALL UFLT6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 7) THEN
	    CALL UFLT7(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 8) THEN
	    CALL UFLT8(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 9) THEN
	    CALL UFLT9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -3) THEN
	    CALL UFLTM3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -5) THEN
	    CALL UFLTM5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -6) THEN
	    CALL UFLTM6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -9) THEN
	    CALL UFLTM9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE 
	    CALL XVMESSAGE('Invalid DCODE passed to UNIFLT', ' ')
	    CALL ABEND
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT1(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to byte filter
C
	BYTE IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = BYTE2INT(IN(1))
	DO I=1,IHALF
	    SUM = SUM + BYTE2INT(IN(1)) + BYTE2INT(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(1))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(I-IOFF))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + BYTE2INT(IN(NPIX)) - BYTE2INT(IN(I-IOFF))
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT2(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Halfword to halfword filter
C
	INTEGER*2 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to halfword filter
C
	INTEGER*2 OUT(NPIX)
	BYTE IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = BYTE2INT(IN(1))
	DO I=1,IHALF
	    SUM = SUM + BYTE2INT(IN(1)) + BYTE2INT(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(1))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + BYTE2INT(IN(NPIX)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT4(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Fullword to Fullword filter
C
	INTEGER*4 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to fullword filter
C
	INTEGER*4 OUT(NPIX)
	BYTE IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = BYTE2INT(IN(1))
	DO I=1,IHALF
	    SUM = SUM + BYTE2INT(IN(1)) + BYTE2INT(IN(I))
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(1))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + BYTE2INT(IN(I+IHALF)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + BYTE2INT(IN(NPIX)) - BYTE2INT(IN(I-IOFF))
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT6(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Halfword to fullword filter
C
	INTEGER*4 OUT(NPIX)
	INTEGER*2 IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT7(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*4 filter
C
	REAL*4 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT8(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*8 filter
C
	REAL*8 IN(NPIX),OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLT9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*8 filter
C
	REAL*8 OUT(NPIX)
	REAL*4 IN(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Halfword to byte filter
C
	INTEGER*2 IN(NPIX)
	BYTE OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Fullword to byte filter
C
	INTEGER*4 IN(NPIX)
	BYTE OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) =  N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM6(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Fullword to halfword filter
C
	INTEGER*4 IN(NPIX)
	INTEGER*2 OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = NINT(SUM/DIVISOR)
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE UFLTM9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*4 filter
C
	REAL*8 IN(NPIX)
	REAL*4 OUT(NPIX)
C
	IHALF = NSW/2
	IOFF = IHALF + 1
C							set up SUM for Pixel 0
	SUM = IN(1)
	DO I=1,IHALF
	    SUM = SUM + IN(1) + IN(I)
	END DO
C								left edge
	DO I=1,IOFF
	    SUM = SUM + IN(I+IHALF) - IN(1)
	    OUT(I) = SUM/DIVISOR
	END DO
C								body
	DO I=IOFF+1,NPIX-IHALF
	    SUM = SUM + IN(I+IHALF) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C								right edge
	DO I=NPIX-IHALF+1,NPIX
	    SUM = SUM + IN(NPIX) - IN(I-IOFF)
	    OUT(I) = SUM/DIVISOR
	END DO
C
	RETURN
	END
