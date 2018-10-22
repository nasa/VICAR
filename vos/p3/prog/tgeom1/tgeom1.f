	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
	REAL XL1(2000),XL2(2000),XL3(2000),XS1(2000),XS2(2000),XS3(2000)
	REAL BUF(12),COEFFS(6,2000),BUFLINE(20000),BUFSAMP(20000)
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
     +		  'U_NL',NL,'U_NS',NS,'U_NB',2,'U_ORG','BIL',
     +		  'OP','WRITE','O_FORMAT','REAL','U_FORMAT','REAL', ' ')
C								get triangles
	DO I=1,NTRI
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    XL1(I) = BUF(1)
	    XS1(I) = BUF(2)
	    XL2(I) = BUF(3)
	    XS2(I) = BUF(4)
	    XL3(I) = BUF(5)
	    XS3(I) = BUF(6)
	    COEFFS(1,I) = BUF(7)
	    COEFFS(2,I) = BUF(8)
	    COEFFS(3,I) = BUF(9)
	    COEFFS(4,I) = BUF(10)
	    COEFFS(5,I) = BUF(11)
	    COEFFS(6,I) = BUF(12)
	END DO
C							      build output image
	DO LINE=ISL,IEL
	    XLINE = LINE
	    CALL MVE(7,NS,0.0,BUFLINE,0,1)		! zero out line buffer
	    CALL MVE(7,NS,0.0,BUFSAMP,0,1)		! zero out sample buffer
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
		    IST = MAX(MIN(A,B,FLOAT(NS))+1.0, 1.0)
		    LAST = MIN(MAX(A,B,1.0), FLOAT(NS))
		    BUFLINE(IST) = COEFFS(1,I)*XLINE +
     +					 COEFFS(2,I)*IST + COEFFS(3,I)
		    BUFSAMP(IST) = COEFFS(4,I)*XLINE +
     +					 COEFFS(5,I)*IST + COEFFS(6,I)
		    DO J=IST+1,LAST
			BUFLINE(J) = BUFLINE(J-1) + COEFFS(2,I)
			BUFSAMP(J) = BUFSAMP(J-1) + COEFFS(5,I)
		    END DO
		END IF
	    END DO
  500	    CONTINUE
	    CALL XVWRIT(IOUTUNIT,BUFLINE,ISTAT,' ')
	    CALL XVWRIT(IOUTUNIT,BUFSAMP,ISTAT,' ')
	END DO
	RETURN
	END
