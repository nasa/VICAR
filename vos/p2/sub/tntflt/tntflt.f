C***************************************************************************
	SUBROUTINE TNTFLT(DCODE, NPIX, IN, OUT, NSW)
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
	    DIVISOR = (NSW/2+1)**2
	    NSWX = NSW
	ELSE
	    DIVISOR = 1.0
	    NSWX = -NSW
	END IF
	IF (MOD(NSWX,2) .EQ. 0) NSWX = 2*NSWX + 1
C						call appropriate subroutine
	IF (DCODE .EQ. 1) THEN
	    CALL FLT1(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 2) THEN
	    CALL FLT2(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 3) THEN
	    CALL FLT3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 4) THEN
	    CALL FLT4(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 5) THEN
	    CALL FLT5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 6) THEN
	    CALL FLT6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 7) THEN
	    CALL FLT7(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 8) THEN
	    CALL FLT8(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. 9) THEN
	    CALL FLT9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -3) THEN
	    CALL FLTM3(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -5) THEN
	    CALL FLTM5(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -6) THEN
	    CALL FLTM6(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE IF (DCODE .EQ. -9) THEN
	    CALL FLTM9(IN,OUT,NPIX,NSWX,DIVISOR)
	ELSE 
	    CALL XVMESSAGE('Invalid DCODE passed to TNTFLT', ' ')
	    CALL ABEND
	END IF
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT1(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to byte filter
C
	BYTE IN(NPIX),OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*BYTE2INT(IN(1)))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * BYTE2INT(IN(I))
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        IF (N.GT.127) N = N-256
        OUT(1) = N
C							all other Pixels
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-BYTE2INT(IN(INCR))
                 IF ((J-IOFF).GT.0) SUM=SUM+BYTE2INT(IN(INCR))
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT2(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Halfword to halfword filter
C
	INTEGER*2 IN(NPIX),OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C					    		all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to halfword filter
C
	INTEGER*2 OUT(NPIX)
	BYTE IN(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*BYTE2INT(IN(1)))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * BYTE2INT(IN(I))
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-BYTE2INT(IN(INCR))
                 IF ((J-IOFF).GT.0) SUM=SUM+BYTE2INT(IN(INCR))
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT4(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Fullword to Fullword filter
C
	INTEGER*4 IN(NPIX),OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C					    		all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Byte to fullword filter
C
	INTEGER*4 OUT(NPIX)
	BYTE IN(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*BYTE2INT(IN(1)))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * BYTE2INT(IN(I))
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-BYTE2INT(IN(INCR))
                 IF ((J-IOFF).GT.0) SUM=SUM+BYTE2INT(IN(INCR))
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT6(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Halfword to fullword filter
C
	INTEGER*4 OUT(NPIX)
	INTEGER*2 IN(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT7(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*4 filter
C
	REAL*4 IN(NPIX),OUT(NPIX),SUM,N
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT8(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*8 filter
C
	REAL*8 IN(NPIX),OUT(NPIX),SUM,N
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO            
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLT9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*4 to real*8 filter
C
	REAL*8 OUT(NPIX)
	REAL*4 IN(NPIX),SUM,N
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM3(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Halfword to byte filter
C
	INTEGER*2 IN(NPIX)
	BYTE OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        IF (N.GT.127) N = N-256
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM5(IN,OUT,NPIX,NSW,DIVISOR)
        INCLUDE 'fortport'
C
C	Fullword to byte filter
C
	INTEGER*4 IN(NPIX)
	BYTE OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        IF (N.GT.127) N = N-256
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    IF (N.GT.127) N=N-256
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM6(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Fullword to halfword filter
C
	INTEGER*4 IN(NPIX)
	INTEGER*2 OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = NINT(SUM/DIVISOR)
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = NINT(SUM/DIVISOR)
	    OUT(I) = N
	END DO
C
	RETURN
	END
C******************************************************************************
	SUBROUTINE FLTM9(IN,OUT,NPIX,NSW,DIVISOR)
C
C	Real*8 to real*4 filter
C
	REAL*8 IN(NPIX),SUM,N
	REAL*4 OUT(NPIX)
        INTEGER WEIGHT
C
	IHALF = NSW/2
	IOFF = IHALF + 1
        WEIGHT = IHALF
C							set up SUM for Pixel 0
	SUM = (IOFF*(IOFF+1)*IN(1))/2
	DO I=2,IOFF
	    SUM = SUM + (WEIGHT) * IN(I)
            WEIGHT = WEIGHT-1
	END DO
        N = SUM/DIVISOR
        OUT(1) = N
C							all other Pixels 
        INDI = IOFF+1
	DO I=2,NPIX
            INDI=INDI-1
            IF (INDI.GT.0) INIT=1
            IF (INDI.LE.0) INIT=INIT+1
            INCR=INIT
            COUNT=0
            DO J = 1,NPIX
                 IF ((J-IOFF).LE.0) SUM=SUM-IN(INCR)
                 IF ((J-IOFF).GT.0) SUM=SUM+IN(INCR)
                 COUNT = COUNT+1
                 IF (COUNT.GE.INDI) INCR=INCR+1
                 IF (INCR.GT.NPIX) INCR=NPIX
            ENDDO
	    N = SUM/DIVISOR
	    OUT(I) = N
	END DO
C
	RETURN
	END
