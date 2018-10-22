C
C     PROGRAM MEDIAN
C      4 DEC 91    ...REA...   REMOVE BYTE RESTICTION, ADDING ALGORITHMS
C                              MOVE REFLECTION AXIS TO TRUE EDGE OF IMAGE
C     24 APR 91    ...REA...   CONVERT TO UNIX/VICAR
C     30 NOV 83    ...HBD...   REWRITE ASSEMBLER TO FORTRAN
C     22 JULY 80   ...HJF...   MAKE USE OF FASTER ALGORITHM
C     18 DEC 78    ...WDB...    INITIAL RELEASE
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      EXTERNAL FILT,HFILT,VFILT,BFILT

      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH,XVPTST
      REAL XMINX(4)/  0.0,-32768.0,-2147483648.0,-1.6E38/
      REAL XMAXX(4)/255.0, 32767.0, 2147483647.0, 1.6E38/
      CHARACTER*10 FMT

C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','REAL',' ')
      CALL XVUNIT(IOUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(IOUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','REAL',' ')
C
      CALL XVSIZE(ISL,ISS,NL,NS,NLIN,NSIN)
      CALL XVGET(INUNIT,STATUS,'PIX_SIZE',NBPP,'FORMAT',FMT,' ')
      IF (NBPP.EQ.4 .AND. FMT.EQ.'FULL') NBPP=3
      XMIN = XMINX(NBPP)
      XMAX = XMAXX(NBPP)
C
C  GET NLW AND NSW VALUES
      CALL XVP('NLW',NLW,CNT)
      CALL XVP('NSW',NSW,CNT)
      NLW = (NLW/2)*2 + 1
      NSW = (NSW/2)*2 + 1
      IF (NLW .LE. 0) THEN
	 CALL XVMESSAGE(' ***ILLEGAL NLW VALUE',' ')
	 CALL ABEND
      ENDIF
      IF (NSW .LE. 0) THEN
	 CALL XVMESSAGE(' ***ILLEGAL NSW VALUE',' ')
	 CALL ABEND
      ENDIF
C
C  GET PERCENT AND DETERMINE THRESHOLD VALUE
      CALL XVP('PERCENT',PERC,CNT)
      PERC = PERC / 100.0
      IF (PERC .LE. 0.0 .AND. PERC .GT. 1.0) THEN
	 CALL XVMESSAGE(' ***ILLEGAL PERCENT VALUE',' ')
	 CALL ABEND
      ENDIF
      IVAL = NINT(PERC*NLW*NSW + 0.5)
C
C  DETERMINE IF HIGHPASS;  IF SO, GET DCTRAN AND DCLEVEL
      HIGH = XVPTST('HIGHPASS')
      IF (HIGH) THEN
         CALL XVP('DCTRAN',DCTRAN,CNT)
         CALL XVP('DCLEVEL',DCLEV,CNT)
      END IF
C
C  DETERMINE ARRAY SIZES AND CALL STACKA
      NLWX = NLW
      NSX = NS + NSW - 1
      NWIN = NLW*NSW
      N1 = 4*NSX*NLWX
      N2 = 4*NS
      N3 = N1
      N4 = 8*NWIN
      N5 = 4*NLW
      N6 = 4*NSW
      N7 = 1024
      IF (NBPP .EQ. 1) THEN
	 CALL STACKA(8,BFILT,3,N1,N2,N7,NLWX,NSX,NWIN)
      ELSE IF (NSW .EQ. 1) THEN
	 CALL STACKA(7,VFILT,3,N1,N2,N5,NLWX,NSX)
      ELSE IF (NLW .EQ. 1) THEN
	 CALL STACKA(5,HFILT,3,N1,N2,N6)
      ELSE
         CALL STACKA(9,FILT,4,N1,N2,N3,N4,NLWX,NSX,NWIN)
      END IF
C
C *****         CLOSE DATA SETS
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(IOUTUNIT,STATUS,' ')
      RETURN
      END
C**********************************************************
      SUBROUTINE BFILT(BUF,N1,OUT,N2,IHIST,N3,NLWX,NSX,NPIXELS)
C
C     BFILT is the median filtering routine used for all cases of byte
C     data input.  It searches a histogram of the window of surrounding
C     values for the median.  It is usually the fastest of the algorithms,
C     but works only on byte data.
C
      INTEGER BUF(*),OUT(*),IHIST(0:255)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      INSIZE = NLWX*NSX			! size of BUF array
      NLW2 = NLW/2			! half of the line weights
      NSW2 = NSW/2			! half of the sample weights
      IST = MAX(ISS-NSW2,1)		! first sample to be read in
      LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
      NLEFT = IST - ISS + NSW2		! number of pixels to pad on left
      NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
      NSREAD = LAST - IST + 1		! number of samples to be read
      LOC = INSIZE-NSX+1		! line position in input buffer
      JVAL = NPIXELS - IVAL + 1		! we are searching for the JVAL'th
C					  highest and IVAL'th lowest value in
C					  the window
C
C					Close and reopen datasets, so that we
C					can use integer buffers
      CALL XVCLOSE(INUNIT,ISTAT,' ')
      CALL XVCLOSE(IOUTUNIT,ISTAT,' ')
      CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','FULL',' ')
      CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		  'U_FORMAT','FULL','OP','WRITE',' ')
C
C						Set up input buffer
      DO LINE = ISL+NLW2, ISL-NLW2, -1
         IF (LINE .GE. 1) THEN
 	    CALL XVREAD(INUNIT,BUF(NLEFT+LOC),ISTAT,'LINE',LINE,
     +		        'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(LOC),NLEFT,NRIGHT,NS+2*NSW2)
	    LOCX = LOC
	 ELSE
	    CALL MVE(4,NSX,BUF(LOCX),BUF(LOC),1,1)
	    LOCX = LOCX + NSX
	 END IF
	 LOC = LOC - NSX
      END DO
C
      LOC = 1					!location for next line into BUF
      LINE = ISL + NLW2 +1			!line number of next line
C						looping thru all output lines
      DO L=1,NL
C						   initialize histogram
	 CALL ZIA(IHIST,256)
	 N = 0
	 DO I=1,NLW
	    DO J=1,NSW
	       IHIST(BUF(N+J)) = IHIST(BUF(N+J)) + 1
	    END DO
	    N = N + NSX
	 END DO
	 NLOW = 0
	 MED = -1
	 DO WHILE (NLOW .LT. IVAL)
	    MED = MED + 1
	    NLOW = NLOW + IHIST(MED)
	 END DO
	 OUT(1) = MED
	 NHIGH = NPIXELS - NLOW			! number of pixels > median
	 NLOW = NLOW - IHIST(MED)		! number of pixels < median
C						    compute value for each pixel
         NREM = 1
         NADD = NREM + NSW
	 DO ISAMP=2,NS
C							update histogram
	    N = 0
	    DO I=1,NLW
	       NUM = BUF(N+NREM)
	       IHIST(NUM) = IHIST(NUM) - 1
	       IF (NUM .GT. MED) THEN
		  NHIGH = NHIGH - 1
	       ELSE IF (NUM .LT. MED) THEN
		  NLOW = NLOW - 1
	       END IF
	       NUM = BUF(N+NADD)
	       IHIST(NUM) = IHIST(NUM) + 1
	       IF (NUM .GT. MED) THEN
		  NHIGH = NHIGH + 1
	       ELSE IF (NUM .LT. MED) THEN
		  NLOW = NLOW + 1
	       END IF
	       N = N + NSX
	    END DO
C							search for median
	    IF (NLOW .GE. IVAL) THEN
	       DO WHILE (NLOW .GE. IVAL)
		  MED = MED - 1
		  NLOW = NLOW - IHIST(MED)
	       END DO
	       NHIGH = NPIXELS - NLOW - IHIST(MED)
	    ELSE IF (NHIGH .GE. JVAL) THEN
	       DO WHILE (NHIGH .GE. JVAL)
		  MED = MED +1
		  NHIGH = NHIGH - IHIST(MED)
	       END DO
	       NLOW = NPIXELS - NHIGH - IHIST(MED)
	    END IF
	    OUT(ISAMP) = MED
	    NREM = NREM + 1
	    NADD = NADD + 1
	 END DO
C							for highpass filtering
	 IF (HIGH) THEN
	    INLINE = MOD(LOC+NLW2*NSX+NSW2,INSIZE)
	    CALL HPI(BUF(INLINE),OUT)
	 END IF
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	 IF (LINE .LE. NLIN) THEN
	    CALL XVREAD(INUNIT,BUF(NLEFT+LOC),ISTAT,'LINE',LINE,
     +			'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(LOC),NLEFT,NRIGHT,NSX)
	    LOCX = LOC
	 ELSE
	    CALL MVE(4,NSX,BUF(LOCX),BUF(LOC),1,1)
	    LOCX = LOCX - NSX
	    IF (LOCX .LT. 1) LOCX = LOCX + INSIZE
	 END IF
	 LOC = LOC + NSX
	 IF (LOC .GT. INSIZE) LOC = 1
	 LINE = LINE + 1
      END DO
      RETURN
      END
C**********************************************************
      SUBROUTINE FILT(BUF,N1,OUT,N2,COL,N3,WINDOW,N4,NLWX,NSX,NPIXELS)
C
C     This routine is used for non-byte data and a two dimensional (NLW>1,
C     NSW>1) window.  This algorithm is the slowest, but works for the
C     general case.
C
      REAL BUF(NSX,NLWX),OUT(*),COL(NLWX,NSX),WINDOW(NPIXELS,2)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      NLW2 = NLW/2			! half of the line weights
      NSW2 = NSW/2			! half of the sample weights
      IST = MAX(ISS-NSW2,1)		! first sample to be read in
      LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
      NLEFT = IST - ISS + NSW2		! number of pixels to pad on left
      NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
      NSREAD = LAST - IST + 1		! number of samples to be read
      LOC = NLW				! line position in input buffer
C
C						Set up input buffer
      DO LINE = ISL+NLW2, ISL-NLW2, -1
         IF (LINE .GE. 1) THEN
 	    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +		        'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
	 ELSE
	    CALL MVE(7,NS+NSW-1,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
	 END IF
	 LOC = LOC - 1
      END DO
C
      LOC = 1					!location for next line into BUF
      LINE = ISL + NLW2 +1			!line number of next line
C						looping thru all output lines
      DO L=1,NL
C						   initialize columns buffers
         DO I=1,NLW
            CALL MVE(7,NSX,BUF(1,I),COL(I,1),1,NLW)
         END DO
         DO I=1,NSX
            CALL SORTM(COL(1,I),NLW)
         END DO
C                                                  initialize window
         CALL MVE(7,NPIXELS,COL,WINDOW,1,1)
         CALL SORTM(WINDOW,NPIXELS)
C						    compute value for each pixel
	 NOW = 1
	 NEXT = 2
         OUT(1) = WINDOW(IVAL,NOW)
         NREM = 1
         NADD = NREM + NSW
	 DO ISAMP=2,NS
	    CALL MERGEM(WINDOW(1,NOW),COL(1,NREM),COL(1,NADD),
     +		        WINDOW(1,NEXT),NPIXELS,NLW)
	    OUT(ISAMP) = WINDOW(IVAL,NEXT)
	    N = NOW
	    NOW = NEXT
	    NEXT = N
	    NREM = NREM + 1
	    NADD = NADD + 1
	 END DO
C							for highpass filtering
	 IF (HIGH) THEN
	    INLINE = MOD(LOC+NLW2-1,NLW) + 1
	    CALL HPR(BUF(NSW2+1,INLINE),OUT)
	 END IF
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	 IF (LINE .LE. NLIN) THEN
	    CALL XVREAD(INUNIT,BUF(NLEFT+1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',IST,'NSAMPS',NSREAD,' ')
	    CALL REFLCT(BUF(1,LOC),NLEFT,NRIGHT,NS+2*NSW2)
	    LOCX = LOC
	 ELSE
	    CALL MVE(7,NS+NSW-1,BUF(1,LOCX),BUF(1,LOC),1,1)
	    LOCX = LOCX - 1
	    IF (LOCX .LT. 1) LOCX=NLW
	 END IF
	 LOC = LOC + 1
	 IF (LOC .GT. NLW) LOC = 1
	 LINE = LINE + 1
      END DO
      RETURN
      END
C**********************************************************
      SUBROUTINE VFILT(BUF,N1,OUT,N2,WINDOW,N3,NLWX,NSX)
C
C     This routine is used for non-byte data and a vertical window (NSW=1)
C
      REAL BUF(NSX,NLWX),OUT(*),WINDOW(NLWX)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      NLW2 = NLW/2			! half of the line weights
      LOC = NLW				! line position in input buffer
C
C						Set up input buffer
      DO LINE = ISL+NLW2, ISL-NLW2, -1
         IF (LINE .GE. 1) THEN
 	    CALL XVREAD(INUNIT,BUF(1,LOC),ISTAT,'LINE',LINE,
     +		        'SAMP',ISS,'NSAMPS',NS,' ')
	 ELSE
	    CALL MVE(7,NS,BUF(1,NLW-LOC),BUF(1,LOC),1,1)
	 END IF
	 LOC = LOC - 1
      END DO
C
      LOC = 1					!location for next line into BUF
      LINE = ISL + NLW2 +1			!line number of next line
C						looping thru all output lines
      DO L=1,NL
C						looping through each pixel
         DO I=1,NS
	    DO J=1,NLW
	       WINDOW(J) = BUF(I,J)
	    END DO
            CALL SORTM(WINDOW,NLW)
	    OUT(I) = WINDOW(IVAL)
         END DO
C							for highpass filtering
	 IF (HIGH) THEN
	    INLINE = MOD(LOC+NLW2-1,NLW) + 1
	    CALL HPR(BUF(1,INLINE),OUT)
	 END IF
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
C							read in next needed line
	 IF (LINE .LE. NLIN) THEN
	    CALL XVREAD(INUNIT,BUF(1,LOC),ISTAT,'LINE',LINE,
     +			'SAMP',ISS,'NSAMPS',NS,' ')
	    LOCX = LOC
	 ELSE
	    CALL MVE(7,NS,BUF(1,LOCX),BUF(1,LOC),1,1)
	    LOCX = LOCX - 1
	    IF (LOCX .LT. 1) LOCX=NLW
	 END IF
	 LOC = LOC + 1
	 IF (LOC .GT. NLW) LOC = 1
	 LINE = LINE + 1
      END DO
      RETURN
      END
C**********************************************************
      SUBROUTINE HFILT(BUF,N1,OUT,N2,WINDOW,N3)
C
C     This routine is used for non-byte data and a horizontal window
C     (NLW=1)
C
      REAL BUF(*),OUT(*),WINDOW(*)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
      LOGICAL HIGH
C
      NSW2 = NSW/2			! half of the sample weights
      IST = MAX(ISS-NSW2,1)		! first sample to be read in
      LAST = MIN(ISS+NS+NSW2-1,NSIN)	! last sample to be read in
      NLEFT = IST - ISS + NSW2		! number of pixels to pad on left
      NRIGHT = NS + NSW2 - LAST 	! number of pixels to pad on right
      NSREAD = LAST - IST + 1		! number of samples to be read
C
C							For each output line
      DO LINE = ISL,ISL+NL-1
C							Read input, reflect ends
	 CALL XVREAD(INUNIT,BUF(NLEFT+1),ISTAT,'LINE',LINE,'SAMP',IST,
     +		    'NSAMPS',NSREAD,' ')
	 CALL REFLCT(BUF,NLEFT,NRIGHT,NS+2*NSW2)
C	                                                initialize window
         CALL MVE(7,NSW,BUF,WINDOW,1,1)
         CALL SORTM(WINDOW,NSW)
C						    compute value for each pixel
         OUT(1) = WINDOW(IVAL)
	 NREM = 1
	 NADD = NREM + NSW
	 DO ISAMP=2,NS
C							add right pixel,
C						 remove left pixel from window
	    IF (BUF(NADD) .GT. BUF(NREM)) THEN
	       I = 1
	       DO WHILE (WINDOW(I) .NE. BUF(NREM))
		  I = I+1
	       END DO
	       DO WHILE (I.LT.NSW .AND. WINDOW(I+1).LT.BUF(NADD))
		  WINDOW(I) = WINDOW(I+1)
		  I = I+1
	       END DO
	       WINDOW(I) = BUF(NADD)
	    ELSE IF (BUF(NADD) .LT. BUF(NREM)) THEN
	       I = NSW
	       DO WHILE (WINDOW(I) .NE. BUF(NREM))
		  I = I-1
	       END DO
	       DO WHILE (I.GT.1 .AND. WINDOW(I-1).GT.BUF(NADD))
		  WINDOW(I) = WINDOW(I-1)
		  I = I-1
	       END DO
	       WINDOW(I) = BUF(NADD)
	    END IF
C
	    OUT(ISAMP) = WINDOW(IVAL)
	    NREM = NREM + 1
	    NADD = NADD + 1
	 END DO
C							for highpass filtering
	 IF (HIGH) CALL HPR(BUF(NSW2+1),OUT)
C								write output
	 CALL XVWRIT(IOUTUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
      END DO
      RETURN
      END
C******************************************************************************
	SUBROUTINE REFLCT(BUF,NLEFT,NRIGHT,LEN)
C
	INTEGER BUF(LEN)
C
	DO I=1,NLEFT
	    BUF(I) = BUF(2*NLEFT-I+1)
	END DO
	DO I=1,NRIGHT
	    BUF(LEN-I+1) = BUF(LEN-2*NRIGHT+I)
	END DO
	RETURN
	END
C******************************************************************************
	SUBROUTINE MERGEM(BUFOLD,BUFREM,BUFADD,BUFNEW,NUM,NLW)
C
	REAL BUFOLD(NUM),BUFREM(NLW),BUFADD(NLW),BUFNEW(NUM)
C
	LOCOLD = 1
	LOCADD = 1
	LOCREM = 1
	DO WHILE (BUFREM(LOCREM) .EQ. BUFOLD(LOCOLD) .AND.
     +		  LOCREM .LE. NLW)
	    LOCREM = LOCREM + 1
	    LOCOLD = LOCOLD + 1
	END DO
	DO I = 1,NUM
	    IF (BUFOLD(LOCOLD).LE.BUFADD(LOCADD) .OR. LOCADD.GT.NLW)THEN
		BUFNEW(I) = BUFOLD(LOCOLD)
	 	LOCOLD = LOCOLD + 1
		IF (LOCOLD .GT. NUM) THEN
		    DO J=0,NLW-LOCADD
			BUFNEW(I+J+1) = BUFADD(LOCADD+J)
		    END DO
		    RETURN
		END IF
		DO WHILE (BUFREM(LOCREM) .EQ. BUFOLD(LOCOLD) .AND.
     +			  LOCREM .LE. NLW)
		    LOCREM = LOCREM + 1
		    IF (LOCOLD .EQ. NUM) THEN
			BUFOLD(NUM) = BUFADD(NLW) + 1.0
		    ELSE
			LOCOLD = LOCOLD + 1
		    END IF
		END DO
	    ELSE
		BUFNEW(I) = BUFADD(LOCADD)
		LOCADD = LOCADD + 1
	    END IF
	END DO
	RETURN
	END
C*******************************************************************************
	SUBROUTINE SORTM(ARR,NUM)
C
C	This is an NlogN sort,  of NUM values in the real array ARR,
C	low to high
C
	REAL ARR(NUM)
C
	ISPAN = NUM/2
	DO WHILE (ISPAN .GT. 0)
	    LAST = NUM - ISPAN
	    LOC = 1
	    DO WHILE (LOC .LE. LAST)
		I1 = LOC
		DO WHILE (I1 .GE. 1)
		    I2 = I1 + ISPAN
		    IF (ARR(I1) .LE. ARR(I2)) GO TO 100
		    HOLD = ARR(I1)
		    ARR(I1) = ARR(I2)
		    ARR(I2) = HOLD
		    I1 = I1 - ISPAN
		END DO
  100		CONTINUE
		LOC = LOC + 1
	    END DO
	    ISPAN = ISPAN/2
	END DO
C
	RETURN
	END
C*******************************************************************************
      SUBROUTINE HPR(IN,OUT)
C
C ***** FUNCTION TO PERFORM HIGH PASS FILTERING ON LINE OF REAL VALUES
C
      REAL IN(*),OUT(*)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
C
      DO I = 1,NS
	X = IN(I) - OUT(I) + DCLEV + DCTRAN*OUT(I)
        OUT(I) = MIN(XMAX, MAX(XMIN, X))
      END DO
      RETURN
      END
C*******************************************************************************
      SUBROUTINE HPI(IN,OUT)
C
C ***** FUNCTION TO PERFORM HIGH PASS FILTERING ON LINE OF INTEGER VALUES
C
      INTEGER IN(*),OUT(*)
C
      COMMON /C1/ INUNIT,IOUTUNIT,ISL,ISS,NL,NS,NLIN,NSIN,NLW,NSW,IVAL,
     +            HIGH,DCLEV,DCTRAN,XMIN,XMAX
C
      DO I = 1,NS
	X = IN(I) - OUT(I) + DCLEV + DCTRAN*OUT(I)
        OUT(I) = MIN(XMAX, MAX(XMIN, X))
      END DO
      RETURN
      END
