	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL XL1(2000),XL2(2000),XL3(2000),XS1(2000),XS2(2000),XS3(2000)
	REAL BUF(12)
	INTEGER*2 IMAGE(20000)
C								statement func
	XSECT(XL,XLT,XLB,XST,XSB) = ((XLB-XL)*XST + (XL-XLT)*XSB) /
     +				    (XLB-XLT)
C								open input
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C						    		get size field
	CALL XVSIZE(ISL,ISS,NL,NS,NTRI,NSIN)
	IEL = ISL + NL - 1
C								open output
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_NL',NL,'U_NS',NS,'OP','WRITE','O_FORMAT','HALF',
     +		    'U_FORMAT','HALF',' ')
C								get triangles
	DO I=1,NTRI
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    XL1(I) = BUF(1)
	    XS1(I) = BUF(2)
	    XL2(I) = BUF(3)
	    XS2(I) = BUF(4)
	    XL3(I) = BUF(5)
	    XS3(I) = BUF(6)
	END DO
C							      build output image
	DO LINE=ISL,IEL
	    XLINE = LINE
	    CALL MVE(2,NS,0,IMAGE,0,1)			! zero out image line
	    DO I=1,NTRI
		IF (XL1(I) .GT. XLINE) GO TO 500
		IF (XL3(I) .GE. XLINE) THEN
		    A = XSECT(XLINE,XL1(I),XL3(I),XS1(I),XS3(I))
		    IF (XL2(I) .GT. XLINE .OR.
     +		       (XL2(I).EQ.XLINE .AND. XL3(I).EQ.XLINE) ) THEN
			B = XSECT(XLINE,XL1(I),XL2(I),XS1(I),XS2(I))
		    ELSE
			B = XSECT(XLINE,XL2(I),XL3(I),XS2(I),XS3(I))
		    END IF
C					A and B are the sample locations where
C					Line "LINE" intersects triangle "I"
C					
		    IST = MIN(A,B,FLOAT(NS)) + 1.0
		    LAST = MAX(A,B,1.0)
		    DO J=IST,LAST
			IMAGE(J) = I
		    END DO
		END IF
	    END DO
  500	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,IMAGE(ISS),ISTAT,' ')
	END DO
	RETURN
	END
